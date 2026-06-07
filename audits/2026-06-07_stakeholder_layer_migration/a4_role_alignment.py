#!/usr/bin/env python3
"""A4 role-alignment probe — stakeholder-layer migration audit (2026-06-07).

Unit (operator-pinned): each role-bearing agent mention from beneficiaries[]/
victims[]/commentary.key_agents across the live json/ corpus. "Aligned" = maps
to exactly one of {agenda_setter, beneficiary, payer, excluded, observer}
WITHOUT forcing. Assignment is keyed on STRUCTURAL POSITION (which array the
mention sits in), never the name string (OQ-64: value-name morphology lies).

Mechanical tiers:
  - beneficiaries[] -> beneficiary (mechanical by construction)
  - victims[]       -> payer       (mechanical by construction)
  - key_agents[] entries: extracted, then bucketed by the role keyword their
    own text carries (Primary target / Primary beneficiary / observer / ...);
    entries whose text carries no role keyword are NON-MECHANICAL residue for
    hand-judgment (sampled, marked INFERRED or bespoke in the report).

Verdict cuts (operator-ruled, declared-revisable): >=90% clean -> discovery
regime; 70-90% -> proceed + residue ledger; <70% -> escalate role set.
"""
import json
import re
import sys
from pathlib import Path
from collections import Counter

JSON_DIR = Path("/home/scott/bin/structural_dynamics_model/json")

# role-keyword -> proposed dial-set role, keyed on the agent's OWN descriptive
# text in key_agents (structural-position words, not names)
KEYWORD_RULES = [
    (re.compile(r"primary target|bears? (the )?(extraction|cost)|victim", re.I), "payer"),
    (re.compile(r"primary beneficiar|benefits? from", re.I), "beneficiary"),
    (re.compile(r"analytical observer|\bobserver\b|analyst", re.I), "observer"),
    (re.compile(r"enforce|administer|sets? the (agenda|rules)|gatekeep|author(s|ity) of", re.I), "agenda_setter"),
    (re.compile(r"excluded|not in the (room|conversation)|voiceless|unheard", re.I), "excluded"),
]

def main():
    files = sorted(JSON_DIR.glob("*.json"))
    mentions = []          # (file, name, source, role, tier)
    residue = []           # key_agents entries with no keyword match
    for f in files:
        try:
            d = json.loads(f.read_text(encoding="utf-8"))
        except Exception as e:
            print(f"PARSE_FAIL {f.name}: {e}", file=sys.stderr)
            continue
        bp = d.get("base_properties", {})
        for b in bp.get("beneficiaries", []) or []:
            mentions.append((f.name, b, "beneficiaries[]", "beneficiary", "mechanical"))
        for v in bp.get("victims", []) or []:
            mentions.append((f.name, v, "victims[]", "payer", "mechanical"))
        for ka in (d.get("commentary", {}) or {}).get("key_agents", []) or []:
            matched = None
            for rx, role in KEYWORD_RULES:
                if rx.search(ka):
                    matched = role
                    break
            if matched:
                mentions.append((f.name, ka[:80], "key_agents[]", matched, "keyword"))
            else:
                mentions.append((f.name, ka[:80], "key_agents[]", None, "residue"))
                residue.append((f.name, ka))

    n = len(mentions)
    tiers = Counter(t for *_x, t in mentions)
    roles = Counter(r for *_x, r, _t in mentions if r)
    print(f"FILES {len(files)}")
    print(f"MENTIONS {n}")
    print(f"TIER {dict(tiers)}")
    print(f"ROLE {dict(roles)}")
    clean = tiers["mechanical"] + tiers["keyword"]
    print(f"CLEAN {clean}/{n} = {clean/n:.1%}" if n else "CLEAN 0/0")
    print(f"RESIDUE {len(residue)}")
    print("---- residue entries (for hand judgment, full list) ----")
    for fn, ka in residue:
        print(f"RES {fn} :: {ka}")

if __name__ == "__main__":
    main()
