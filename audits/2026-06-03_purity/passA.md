# Pass A — spec reconciliation (static)
Canonical spec: docs/logic_extensions.md §2.2-2.3 (Stages 7-9 spec, referenced by logic.md:16,1098)

## Band table comparison
| boundary | logic_extensions.md §2.3 (spec) | purity_zone/2 (logical_fingerprint.pl:607-611) | purity_scoring.pl header comment (22-27) |
|---|---|---|---|
| pristine | >=0.9 | >=0.9 | (absent; "1.0 perfectly pure") |
| sound | >=0.7 | >=0.7 | ">0.8 structurally sound" DIVERGENT |
| borderline | >=0.5 | >=0.5 | "0.5 borderline" ~consistent |
| contaminated | >=0.3 | >=0.3 | "<0.3 contaminated" DIVERGENT (names the wrong band) |
| degraded | <0.3 | <0.3 | (absent) |

VERDICT: purity_zone/2 CONSISTENT with spec. logic.md worked example (3023: 0.72=sound; 3028: 0.42=contaminated) CONSISTENT with spec.
purity_scoring.pl:22-27 header comment DIVERGENT from spec — stale doc comment (v5.1-era), not a code defect.
Candidate finding #2 RESOLVED BY EVIDENCE (not DESIGN-AMBIGUOUS): canonical = spec/purity_zone; fix = correct the header comment.

## Other spec deltas noted
- Spec implementation snippet (logic_extensions.md:750-760) vs purity_scoring.pl:41-50:
  - spec: sentinel clause FIRST (epistemic_access_check(C,false) -> -1.0); code: positive check + cut, sentinel as fallback clause.
  - CONSEQUENCE (code): subscore failure AFTER the cut = hard predicate failure (no score, no sentinel). Pass B witnesses.
  - spec snippet has NO clamp; code clamps min(1.0,max(0.0,Raw)). Code stricter; benign.
- Spec sentinel conditions (744-747): <3 indexed classifications; missing coordination_type; missing structural predicates. Verify against epistemic_access_check in Pass B.
- structural_purity/2 vs purity_score/2 complementarity is EXPLICIT in spec (logic_extensions.md:828): continuous health metric vs categorical diagnosis. Pass C question = do consumers respect it.

## Thresholds consumers must match (Pass D inputs)
- CI_Rope certification: purity >= 0.7 (logic.md:988)
- surgical reform gate: purity >= 0.30 (logic_extensions.md:786-788; logic.md:1767)
- reform candidate: purity >= 0.50 (logic.md:1765)
- naturalized contamination params (logic.md:2078)
- Type 10 purity drift (logic.md:2998-3034)
