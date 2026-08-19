# audit_log — OQ-303(a) class-B conversion rollout (Unit B)

Chronological. Every result line below the PREREGISTRATION md5 line was produced after that
freeze. Nothing above it is a result.

---

## OPEN — 2026-08-18

**HEAD stamp (OPEN):** `161707bb1832d40f3a418b615bee89a827fd6da0`, working tree clean.
Branch `oq303-classb-rollout` cut from it — this unit is multi-file and semantics-changing by
construction, which is the branch case in `CLAUDE.md` *Git autonomy*.

**Input:** `audits/2026-08-18_bound_caller_rewitness/partition.md` — 55 `converts-clean`,
1 `live-output-path`, 2 `converts-clean-minus-dataflow`.

### R1 (pre-freeze reads) — the `commentary` reachability question, answered by reading

Taken BEFORE the prereg was frozen, and written into it as the claim the pair will confirm or
refute. Recorded here as reads, not as results.

```
$ grep -rn "signature_grade" prolog/ python/ agent/ --include=*.pl --include=*.py | grep -v testsets
prolog/diagnostic_summary.pl:728:    (   signature_detection:signature_grade(C, SigGrade)
prolog/json_report.pl:1509:        format(S, '        "signature_grade": ', []),
prolog/signature_detection.pl:1952:    signature_grade(C, correction).
python/tensions_ledger.py:173:    grade = vj.get("signature_grade") if vj else None
python/shared/schemas.py:547:                       "signature_grade"):
```

```
$ sed -n '726,732p' prolog/diagnostic_summary.pl
    (   signature_detection:signature_grade(C, SigGrade)
    ->  true
    ;   SigGrade = none
    ),
    Join = verdict_join(Joined, Base, Cap, Alerts, GridProv, MeasProv,
                        SigGrade).
```

**The answer, and it is a distinction rather than a yes/no:** `commentary` is reachable
downstream as a **VALUE** — `verdict_join/3` queries `signature_grade/2` **unbound** at
`:728`, the result is serialized as `verdict_join.signature_grade`
(`json_report.pl:1509`) and read live by `python/tensions_ledger.py:173`. It is queried
**nowhere as a BOUND SELECTOR**; the engine's only bound caller is
`signature_detection.pl:1951`, at `correction`.

That distinction is what decides Ω_E vs Ω_P, so it is pre-registered with its consequences
(PREREGISTRATION §2) rather than left to be read off the diff afterwards.

---

## PREREGISTRATION FROZEN — 2026-08-18, before any conversion or six-leg run

```
$ md5sum audits/2026-08-18_classb_conversion_rollout/PREREGISTRATION.md
000742e0039894e98855c1187935aea4  audits/2026-08-18_classb_conversion_rollout/PREREGISTRATION.md
```

**Everything below this line is a result.**

---
