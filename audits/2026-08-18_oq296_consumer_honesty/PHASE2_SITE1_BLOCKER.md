# Phase 2 site 1 — `container_typology_analysis.py`: the plan's premise is false here

Executed 2026-08-18. **Phase 2 site 1 is BLOCKED pending an operator ruling.** The edit was
written, run, diffed, and then REVERTED unshipped — its provenance text asserted something the
evidence contradicts.

## What the plan assumed

OQ-296 and the 2026-08-17 audit roster both record this site as a consumer *reading a constant
zero*: `natural_law_pct = sd.get("natural_law", 0) / n` → "prevalence surface silently reading 0."
The prescribed fix was to stop serializing a plausible measured `0.0`.

## What it actually serves

The before/after diff of `outputs/container_candidates.json` shows `natural_law_pct` is **not
zero** anywhere near uniformly:

```
CHANGED <domain>.natural_law_pct: 0.9808 -> None
CHANGED <domain>.natural_law_pct: 0.9636 -> None
CHANGED <domain>.natural_law_pct: 0.9444 -> None
CHANGED <domain>.natural_law_pct: 0.8462 -> None
CHANGED <domain>.natural_law_pct: 0.1358 -> None   (etc.)
```

## Why — two independent invalidations, neither of them "the detector is dark"

**(1) The input is a stale artifact.** `signature_distribution` is read from
`outputs/container_typology_recon_data.json`, **dated 2026-05-16**, whose own
`total_constraints` is **3369** — the chimera-era `original_v6` corpus (ID reuse across runs,
OQ-25), not the live post-reset corpus (279). 102 domains in it carry `natural_law > 0`
(mathematics 68/72, mathematical_logic 51/52, physics 22/26). Neither
`container_typology_recon.py` nor `container_typology_analysis.py` is wired into
`run_pipeline.py`, so the artifact never refreshes: Pattern 1's *consumed-once is not
kept-fresh*, frozen across the 2026-06-05 corpus reset.

**(2) Those firings are from the withdrawn pass-open regime.** `signature_detection.pl:249-255`
records it directly: `has_viable_alternatives/2`'s "default used to be `false`, which the empty
corpus-wide `intent_viable_alternative/3` table (GAP-08) satisfied by ABSENCE — and
natural_law_signature requires HasAlternatives == false, so the absence SUPPORTED every NL
certification (pass-open). Default is now `unknown`." The OQ-44 fail-close (2026-06-11) is what
darkened the detector. So the May-16 numbers are precisely the Pattern-5 fabricated
certifications that fix removed — not measurements that later went to zero.

## Consequence for the plan

- The site is **not** an instance of "consumer converts a constant zero into plausible output."
  It is a consumer serving **real numbers from a dead corpus under a withdrawn regime** — a
  larger defect wearing the smaller one's clothes.
- The staleness is **not scoped to the natural_law component**. `mountain_pct`,
  `mean_extractiveness`, `type_distribution`, orbit stats — every field in
  `container_candidates.json` comes from the same May-16 file. The whole artifact describes
  original_v6. A `formalization_provenance` flag naming only the dead NL component would
  certify the *rest* of the record as current, which is the more dangerous claim.
- The drafted edit's provenance block asserted `natural_law_pct` is "structurally zero on every
  corpus." For the values actually served that is **false**. Shipping it would have minted a
  confidently-wrong annotation at a site — the `MISSING_NL_PROFILE` failure mode, in the same
  session that spent Phase 1 removing one. Hence the revert.

## Verification that this finding is not itself an artifact

- Recon file date and `total_constraints` read directly from the file on disk.
- The 0.9444 value regenerates on a fresh `python3 python/container_typology_analysis.py`
  (exit 0) after the revert, so it is what HEAD serves today, not a leftover.
- The pass-open history is quoted from the engine source, not inferred.
