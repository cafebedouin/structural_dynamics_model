#!/usr/bin/env python3
"""Per-STORY stratum = (model, thinking-regime, prompt_hash, schema_hash).

v2 change: the key gained prompt_hash and schema_hash. Several legs were
BACKFILLED (operator, 2026-08-23) — the original pass left gaps, the schema was
fixed, and the models re-did the failed stories to improve cross-leg id
matching. That is a SECOND GENERATION EVENT INSIDE ONE LEG, at a different
prompt and often a different schema. Measured on disk 2026-08-23:

    testsets_haiku     505 @ 22843cdf/2e9dff2f 2026-06-13
                     + 455 @ e03e2210/685ed7cf 2026-08-22  (+stakeholder_backfill)
    testsets_flash     754 / 206   same two commit pairs
    testsets_nemotron  664+188 @ 685ed7cf  + 144 @ e03e2210 (+rescue1)
    testsets_stealth   968 @ 685ed7cf      +  36 @ e03e2210 (+rescue1)

testsets_haiku is 47% re-authored under a different prompt AND schema, 70 days
apart. OQ-78 ruling 5: ε-keyed denominators are per-Author stratum, NEVER
pooled, "and not across generation regimes within one model either... A model
swap is a re-baseline event on the ε axis." A prompt+schema change at 70 days'
distance is one too. A (model, regime) key would silently merge the two halves
of testsets_haiku into a single authored-ε range and call it a stratum.

`testsets` is additionally mixed-MODEL (sonnet-5 / sonnet-4.5 / haiku-4.5), which
is why the key is per STORY and never per leg.
"""
import json, re, sys
from collections import Counter
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
PROLOG = ROOT / "prolog"

# story_provenance(Id, PromptHash, SchemaHash, Date, RunTag, Example, Model, Sampling).
PROV = re.compile(
    r"story_provenance\(\s*([a-z0-9_]+)\s*,\s*'([^']*)'\s*,\s*'([^']*)'\s*,\s*'([^']*)'\s*,"
    r"\s*'([^']*)'\s*,\s*'([^']*)'\s*,\s*'([^']*)'\s*,\s*'([^']*)'\s*\)\s*\.", re.S)


def regime(sampling: str) -> str:
    s = sampling.lower()
    if "thinking_budget=0" in s or "thinking=disabled" in s or "reasoning=disabled" in s:
        return "off"
    if "thinking_budget=" in s or "reasoning=model_default" in s or "thinking=enabled" in s:
        return "on"
    return "unknown"


def main():
    legs = [d.name for d in sorted(PROLOG.glob("testsets*")) if d.is_dir() and list(d.glob("*.pl"))]
    legs.append("archives/datasets/kernel_v1")
    out = {}
    for leg in legs:
        m = {}
        for f in (PROLOG / leg).glob("*.pl"):
            txt = f.read_text(encoding="utf-8", errors="replace")
            hit = PROV.search(txt)
            if not hit:
                m[f.stem] = {"model": "unprovenanced", "regime": "unknown",
                             "prompt": "-", "schema": "-", "run_tag": "-",
                             "stratum": "unprovenanced"}
                continue
            _, prompt, schema, date, run_tag, _ex, model, sampling = hit.groups()
            r = regime(sampling)
            m[f.stem] = {"model": model, "regime": r, "prompt": prompt[:8],
                         "schema": schema[:8], "run_tag": run_tag, "date": date,
                         "stratum": f"{model}|{r}|{prompt[:8]}|{schema[:8]}"}
        out[leg] = m
        c = Counter(v["stratum"] for v in m.values())
        multi = "  << SPLIT LEG" if len(c) > 1 else ""
        print(f"{leg:<38} {len(m):>5}{multi}", file=sys.stderr)
        for k, nn in c.most_common():
            print(f"      {nn:>5}  {k}", file=sys.stderr)
    Path(sys.argv[1]).write_text(json.dumps(out), encoding="utf-8")
    print(f"wrote {sys.argv[1]}", file=sys.stderr)


if __name__ == "__main__":
    main()
