"""
Tests statistiques sur un tableau de p variables et 3n lignes (groupes F1, F2, F3).
Format attendu : fichier CSV/TSV avec séparateur | (ou adapter SEP ci-dessous).

Usage :
    python stats_tests.py --file data.txt --sep "|"
    python stats_tests.py --file data.csv --sep ","

Si --file n'est pas fourni, un jeu de données de démonstration est généré.
"""

import argparse
from xml.parsers.expat import model
import numpy as np
import pandas as pd
from scipy import stats
from scipy.stats import anderson, bartlett, kruskal, levene, shapiro
import statsmodels.api as sm

from arch.bootstrap import IIDBootstrap

# ─────────────────────────────────────────────
# 1.  CHARGEMENT DES DONNÉES
# ─────────────────────────────────────────────

def load_data(filepath: str, sep: str = ",") -> pd.DataFrame:
    """Charge le fichier et renvoie un DataFrame."""
    df = pd.read_csv(filepath, sep=sep, engine="python")
    df.columns = df.columns.str.strip()
    return df


def split_groups(df: pd.DataFrame):
    """
    Suppose 3n lignes : n premières → F1, n suivantes → F2, dernières n → F3.
    Lève une erreur si le nombre de lignes n'est pas divisible par 3.
    """
    total = len(df)
    if total % 3 != 0:
        raise ValueError(
            f"Le tableau a {total} lignes, qui n'est pas divisible par 3. "
            "Vérifiez vos données."
        )
    n = total // 3
    return df.iloc[:n], df.iloc[n:2*n], df.iloc[2*n:]


# ─────────────────────────────────────────────
# 2.  CALCULS PAR VARIABLE
# ─────────────────────────────────────────────

def unbiased_skewness_over_ci(x: np.ndarray) -> float:
    """
    Asymétrie non biaisée (Fisher) divisée par son intervalle de confiance à 95 %.
    IC approché : ±1.96 * sqrt(6/n)   (formule standard de l'erreur-type du skewness)
    Renvoie NaN si n < 4.
    """
    x = x[~np.isnan(x)]
    n = len(x)
    if n < 4:
        return np.nan
    # Skewness non biaisé (formule de Fisher, identique à pandas/scipy avec bias=False)
    sk = stats.skew(x, bias=False)
    se = np.sqrt(6.0 / n)           # erreur-type standard du skewness
    ci_half = 1.96 * se             # demi-largeur de l'IC à 95 %
    return sk / ci_half if ci_half != 0 else np.nan


def skew_over_bca_ci0(x: np.ndarray, R: int = 1000, conf_level: float = 0.95,
                     seed: int = 42) -> float:

    x = np.asarray(x, dtype=float)
    x = x[~np.isnan(x)]

    skw_obs = stats.skew(x, bias=False)

    bs = IIDBootstrap(x, seed=seed)

    # arch passe un tableau 2D (n, 1) → il faut aplatir avec .ravel()
    ci = bs.conf_int(
        lambda v: np.array([stats.skew(v.ravel(), bias=False)]),
        reps=R,
        method="bca",
        size=conf_level
    )

    ci_lo, ci_hi = ci[0, 0], ci[1, 0]
    width = ci_hi - ci_lo
    return abs(skw_obs / width) if width != 0 else np.nan

def skew_over_bca_ci(x: np.ndarray, R: int = 1000, conf_level: float = 0.95,
                     seed: int = None) -> float:
    """
    Équivalent de :
        skw <- DescTools::Skew(x, method=3, conf.level=0.95, ci.type="bca", R=1000)
        abs(skw[[1]] / (skw[[3]] - skw[[2]]))

    method=3 → skewness non biaisé de Fisher (bias=False dans scipy)
    ci.type="bca" → intervalle BCa (bias-corrected and accelerated) par bootstrap
    """
    rng = np.random.default_rng(seed)
    x = np.asarray(x, dtype=float)
    x = x[~np.isnan(x)]
    n = len(x)

    # Skewness observé (method=3 = Fisher, non biaisé)
    skw_obs = stats.skew(x, bias=False)

    # ── Bootstrap ────────────────────────────────────────────────────────────
    boot_skews = np.array([
        stats.skew(rng.choice(x, size=n, replace=True), bias=False)
        for _ in range(R)
    ])

    # ── Correction de biais z0 ───────────────────────────────────────────────
    z0 = stats.norm.ppf(np.mean(boot_skews < skw_obs))

    # ── Accélération a (jackknife) ───────────────────────────────────────────
    jack = np.array([stats.skew(np.delete(x, i), bias=False) for i in range(n)])
    jack_mean = jack.mean()
    num   = np.sum((jack_mean - jack) ** 3)
    denom = 6.0 * (np.sum((jack_mean - jack) ** 2) ** 1.5)
    a = num / denom if denom != 0 else 0.0

    # ── Quantiles BCa ────────────────────────────────────────────────────────
    alpha = 1 - conf_level
    z_lo  = stats.norm.ppf(alpha / 2)
    z_hi  = stats.norm.ppf(1 - alpha / 2)

    def bca_quantile(z_val):
        p = stats.norm.cdf(z0 + (z0 + z_val) / (1 - a * (z0 + z_val)))
        p = np.clip(p, 0, 1)
        return np.quantile(boot_skews, p)

    ci_lo = bca_quantile(z_lo)
    ci_hi = bca_quantile(z_hi)

    # ── Résultat : |skw / largeur de l'IC| ──────────────────────────────────
    width = ci_hi - ci_lo
    return abs(skw_obs / width) if width != 0 else np.nan

def variance_ratio_max_over_min(g1: np.ndarray,
                                 g2: np.ndarray,
                                 g3: np.ndarray) -> float:
    """Rapport : variance maximale / variance minimale parmi les 3 groupes."""
    variances = [
        np.nanvar(g1, ddof=1),
        np.nanvar(g2, ddof=1),
        np.nanvar(g3, ddof=1),
    ]
    vmax, vmin = max(variances), min(variances)
    return vmax / vmin if vmin > 0 else np.nan


def run_anderson(x: np.ndarray):
    """
    Test d'Anderson-Darling pour la normalité.
    Renvoie la statistique et la p-valeur interpolée à partir des seuils fournis
    par scipy (car Anderson ne donne pas directement une p-valeur scalaire).
    Niveaux de signification disponibles : [15, 10, 5, 2.5, 1] %.
    """
    x = x[~np.isnan(x)]
    if len(x) < 8:
        return np.nan, np.nan
    res = anderson(x, dist="norm")
    stat = res.statistic
    # Interpolation log-linéaire entre les niveaux critiques fournis
    sig_levels = np.array([15, 10, 5, 2.5, 1]) / 100.0   # en proportion
    crit_vals  = res.critical_values
    # scipy ordonne par signif croissante → valeur critique croissante
    # On interpole : stat > crit[i]  ⟹  p < sig_levels[i]
    if stat <= crit_vals[0]:
        p_val = sig_levels[0]          # p ≥ 0.15  (borne haute)
    elif stat >= crit_vals[-1]:
        p_val = sig_levels[-1]         # p ≤ 0.01  (borne basse)
    else:
        # Interpolation log-linéaire sur les p-valeurs
        p_val = float(np.interp(stat, crit_vals, sig_levels))
    return stat, p_val


def compute_tests_for_variable(col: str,
                                F1: pd.DataFrame,
                                F2: pd.DataFrame,
                                F3: pd.DataFrame) -> dict:
    """Calcule tous les tests pour une variable (colonne) donnée."""
    g1 = F1[col].dropna().values.astype(float)
    g2 = F2[col].dropna().values.astype(float)
    g3 = F3[col].dropna().values.astype(float)
    all_vals = np.concatenate([g1, g2, g3])

    # Variable indicatrice de groupe (dummy encoding, F1 = référence)
    groups = np.array(["F1"] * len(g1) + ["F2"] * len(g2) + ["F3"] * len(g3))
    X = pd.get_dummies(pd.Series(groups), drop_first=True).astype(float).values
    X = sm.add_constant(X)  # ajoute l'intercept
    
    model = sm.OLS(all_vals, X).fit()
        
    result = {"variable": col}

    # ── Anderson (sur l'ensemble des observations) ──────────────────────────
    _, p_anderson = run_anderson(all_vals)
    result["anderson_pval"] = p_anderson

    # ── Shapiro-Wilk (sur l'ensemble) ───────────────────────────────────────
    if len(all_vals) >= 3:
        _, p_shapiro = shapiro(model.resid)
    else:
        p_shapiro = np.nan
    result["shapiro_pval"] = p_shapiro

    # ── Bartlett (homogénéité des variances, suppose normalité) ─────────────
    try:
        _, p_bartlett = bartlett(g1, g2, g3)
    except Exception:
        p_bartlett = np.nan
    result["bartlett_pval"] = p_bartlett

    # ── Levene (homogénéité des variances, robuste) ──────────────────────────
    try:
        _, p_levene = levene(g1, g2, g3)
    except Exception:
        p_levene = np.nan
    result["levene_pval"] = p_levene

    # ── Kruskal-Wallis ──────────────────────────────────────────────────────
    try:
        _, p_kruskal = kruskal(g1, g2, g3)
    except Exception:
        p_kruskal = np.nan
    result["kruskal_pval"] = p_kruskal

    # ── Skewness / IC ───────────────────────────────────────────────────────
    #result["skewness_over_ci"] = unbiased_skewness_over_ci(all_vals)
    result["skewness_over_ci"] = skew_over_bca_ci0(model.resid)
    
    # ── Rapport de variances max / min ──────────────────────────────────────
    result["var_ratio_max_min"] = variance_ratio_max_over_min(g1, g2, g3)

    return result


# ─────────────────────────────────────────────
# 3.  PIPELINE PRINCIPAL
# ─────────────────────────────────────────────

def run_pipeline(df: pd.DataFrame) -> pd.DataFrame:
    F1, F2, F3 = split_groups(df)
    rows = []
    for col in df.columns:
        rows.append(compute_tests_for_variable(col, F1, F2, F3))
    results = pd.DataFrame(rows).set_index("variable")
    return results


def format_results(results: pd.DataFrame) -> str:
    """Mise en forme lisible avec indications de signification."""
    col_labels = {
        "anderson_pval":    "Anderson p-val",
        "shapiro_pval":     "Shapiro p-val",
        "bartlett_pval":    "Bartlett p-val",
        "levene_pval":      "Levene p-val",
        "kruskal_pval":     "Kruskal p-val",
        "skewness_over_ci": "Skew / CI(95%)",
        "var_ratio_max_min":"VarMax / VarMin",
    }
    display = results.rename(columns=col_labels)

    # Formatage numérique
    for c in display.columns:
        display[c] = display[c].map(lambda v: f"{v:.4f}" if pd.notna(v) else "NaN")

    return display.to_string()


# ─────────────────────────────────────────────
# 4.  DONNÉES DE DÉMONSTRATION
# ─────────────────────────────────────────────

def make_demo_data(n: int = 30, p: int = 4, seed: int = 42) -> pd.DataFrame:
    """Génère 3n lignes de données synthétiques pour 3 groupes."""
    rng = np.random.default_rng(seed)
    cols = {f"var{i+1}": np.concatenate([
        rng.normal(loc=0,   scale=1,   size=n),   # F1
        rng.normal(loc=0.5, scale=1.5, size=n),   # F2
        rng.normal(loc=1.0, scale=2,   size=n),   # F3
    ]) for i in range(p)}
    return pd.DataFrame(cols)


# ─────────────────────────────────────────────
# 5.  POINT D'ENTRÉE
# ─────────────────────────────────────────────

def main():
    parser = argparse.ArgumentParser(
        description="Tests statistiques (Anderson, Bartlett, Kruskal, Levene, "
                    "Shapiro, Skewness/CI, VarRatio) sur 3 groupes."
    )
    parser.add_argument("--file", type=str, default=None,
                        help="Chemin vers le fichier de données.")
    parser.add_argument("--sep",  type=str, default="|",
                        help="Séparateur de colonnes (défaut : '|').")
    args = parser.parse_args()

    if args.file:
        print(f"Chargement de : {args.file}  (sep='{args.sep}')")
        df = load_data(args.file, sep=args.sep)
    else:
        print("Aucun fichier fourni → utilisation des données de démonstration "
              "(n=30 par groupe, 4 variables).\n")
        df = make_demo_data()

    print(f"Dimensions : {len(df)} lignes × {df.shape[1]} variables")
    print(f"Groupes    : F1={len(df)//3} lignes, F2={len(df)//3} lignes, "
          f"F3={len(df)//3} lignes\n")

    results = run_pipeline(df)

    print("═" * 80)
    print("RÉSULTATS DES TESTS STATISTIQUES")
    print("═" * 80)
    print(format_results(results))
    print()
    print("Légende :")
    print("  Anderson / Shapiro  → H0 : normalité (p < 0.05 ⟹ non-normal)")
    print("  Bartlett / Levene   → H0 : variances égales entre groupes")
    print("  Kruskal             → H0 : distributions identiques entre groupes")
    print("  Skew / CI(95%)      → |valeur| > 1 suggère une asymétrie significative")
    print("  VarMax / VarMin     → ratio > 3–4 souvent jugé problématique")
    print()

    # Export CSV
    from pathlib import Path
    import os
    scriptdir = Path(__file__).resolve().parent
    print(scriptdir)
    
    out_csv = os.path.join(scriptdir, "resultats_tests.csv")
    results.to_csv(out_csv)
    print(f"Résultats exportés dans : {out_csv}")


if __name__ == "__main__":
    main()