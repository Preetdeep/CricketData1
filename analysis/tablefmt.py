"""Minimal LaTeX regression-table writer for pyfixest models."""
from common import stars


def coef_rows(models, terms, labels, scale=1.0, digits=2):
    lines = []
    for t in terms:
        est, se = [], []
        for m in models:
            co = m.coef()
            if t in co.index:
                b = co[t] * scale
                s = m.se()[t] * scale
                p = m.pvalue()[t]
                est.append(f"{b:.{digits}f}{stars(p)}")
                se.append(f"({s:.{digits}f})")
            else:
                est.append("")
                se.append("")
        lines.append(f"{labels[t]} & " + " & ".join(est) + r" \\")
        lines.append(" & " + " & ".join(se) + r" \\")
    return lines


def regtable(models, terms, labels, footer_rows, header=None, scale=1.0, digits=2, colnames=None):
    k = len(models)
    out = [r"\begin{tabular}{l" + "c" * k + "}", r"\toprule"]
    if header:
        out += header
    names = colnames or [f"({i + 1})" for i in range(k)]
    out.append(" & " + " & ".join(names) + r" \\")
    out.append(r"\midrule")
    out += coef_rows(models, terms, labels, scale, digits)
    out.append(r"\midrule")
    for lab, vals in footer_rows:
        out.append(f"{lab} & " + " & ".join(vals) + r" \\")
    out += [r"\bottomrule", r"\end{tabular}"]
    return "\n".join(out) + "\n"


def yesno(flags):
    return [r"\checkmark" if f else "" for f in flags]
