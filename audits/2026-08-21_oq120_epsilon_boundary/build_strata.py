#!/usr/bin/env python3
"""Per-STORY model stratum, derived from each story's own story_provenance/8.

CLAUDE.md: "A leg's MODEL is not its directory name — verify from
story_provenance before attributing or pooling (OQ-78)." And `testsets` is not
one stratum at all: it is a MIXED leg (sonnet-5 + sonnet-4.5 + haiku-4.5 + ...),
so a per-LEG stratum map would pool three authoring idioms inside one leg and
silently violate OQ-78 ruling 5. Stratum is therefore keyed per STORY.

Stratum = (model, thinking-regime). The regime is part of the key because
OQ-347 measures thinking-on legs as a different authoring regime (ε moves
>=0.10 on 17-25% of redraws vs 4-9% thinking-off).
"""
import json, re, sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
PROLOG = ROOT / "prolog"

PROV = re.compile(
    r"story_provenance\(\s*([a-z0-9_]+)\s*,(?:[^)]*?)"
    r"'([^']*)'\s*,\s*'([^']*)'\s*\)\s*\.", re.S)


def regime(sampling: str) -> str:
    s = sampling.lower()
    if "thinking_budget=0" in s or "thinking=disabled" in s or "reasoning=disabled" in s:
        return "off"
    if "thinking_budget=" in s or "reasoning=model_default" in s or "thinking=enabled" in s:
        return "on"
    return "unknown"


def main():
    legs = [d.name for d in sorted(PROLOG.glob("testsets*")) if d.is_dir()
            and list(d.glob("*.pl"))]
    legs.append("archives/datasets/kernel_v1")
    out = {}
    for leg in legs:
        d = PROLOG / leg
        m = {}
        for f in d.glob("*.pl"):
            txt = f.read_text(encoding="utf-8", errors="replace")
            i = txt.find("story_provenance(")
            if i < 0:
                m[f.stem] = {"model": "unprovenanced", "regime": "unknown",
                             "stratum": "unprovenanced"}
                continue
            hit = PROV.search(txt, i)
            if not hit:
                m[f.stem] = {"model": "unparsed", "regime": "unknown",
                             "stratum": "unparsed"}
                continue
            model, sampling = hit.group(2), hit.group(3)
            r = regime(sampling)
            m[f.stem] = {"model": model, "regime": r, "stratum": f"{model}|{r}"}
        out[leg] = m
        from collections import Counter
        c = Counter(v["stratum"] for v in m.values())
        print(f"{leg:<38} {len(m):>5} stories  " +
              "; ".join(f"{k} {n}" for k, n in c.most_common(4)), file=sys.stderr)
    Path(sys.argv[1]).write_text(json.dumps(out), encoding="utf-8")
    print(f"wrote {sys.argv[1]}", file=sys.stderr)


if __name__ == "__main__":
    main()
