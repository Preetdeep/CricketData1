"""Final pass over generated LaTeX tables: typeset negative numbers with a true minus sign."""
import re

from common import TABLES

NEG = re.compile(r"(?<![\w\-])-(?=\d)")

for path in sorted(TABLES.glob("*.tex")):
    txt = path.read_text()
    new = NEG.sub(r"$-$", txt)
    if new != txt:
        path.write_text(new)
        print(f"{path.name}: {len(NEG.findall(txt))} minus signs")
