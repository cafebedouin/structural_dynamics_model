# Arm A — the pinned-arm tripwire (OQ-342 §9)

**Executed:** 2026-08-25. A tripwire, not a measurement: a pass tells us nearly nothing; a
failure would have told us something enormous — that "pinned" does not mean what we think.

**Result: PASS.** `backfill_diff.py --selftest` OK (`null excluded-and-counted (1), 0.0 retained
([0.0])`). Both legs run with the AFTER arm at its default — the preserved §9 copies, resolved
and pasted before the run:

```
haiku: AFTER = outputs/_arms_oq345_2026-08-25/pipeline_output.haiku.json.gz  exists=True 1345348 B
flash: AFTER = outputs/_arms_oq345_2026-08-25/pipeline_output.flash.json.gz  exists=True 1111733 B
```

The live `outputs/pipeline_output.<leg>.json` was **not** passed: it is the post-2026-08-25
coherent set, and passing it would compare the backfill against the coherent reclassify instead of
against the pre-backfill baseline — the wrong arm entirely.

**Byte comparison against the committed 2026-08-22 txt:**

| leg | measurement body md5 (fresh run) | committed txt md5 | verdict |
|---|---|---|---|
| haiku | `c4a669d5878c8fa31f43b4eb6f8f0284` | `c4a669d5878c8fa31f43b4eb6f8f0284` | identical |
| flash | `5a677f242b96084d706db5cc42a6677f` | `5a677f242b96084d706db5cc42a6677f` | identical |

**One line differs, and its provenance is git-witnessed.** The whole-file diff is not empty:

```
1d0
< AFTER arm: /…/outputs/_arms_oq345_2026-08-25/pipeline_output.haiku.json.gz
```

That header is emitted at `backfill_diff.py:61`, added in commit **`a3966e7c6`** — the freeze
commit that preserved the arms in the first place, and the same commit the coherent set is stamped
at. The committed txts were written 2026-08-22, before the `--after` option and its header
existed. So this is an **instrument change that ADDS a provenance line**, not a change in what the
instrument measures: every substantive line is byte-identical on both legs.

Reported as a pass with the deviation named, rather than as a silent pass, because "byte-identical
is the pass" was the pre-registered bar and the whole file is not byte-identical. Neither failure
shape the plan named is present: the arms are not *absent or unreadable* (the declared fresh-clone
gap in `preserved_arms.md` has not been realised — all four `.gz` arms are on disk), and the
numbers do not *differ*.

**What the tripwire would have caught, for the record.** The committed haiku txt reads:

```
leg=haiku before n=960 after n=960 shared=960 backfilled(tagged)=455
BACKFILLED stratum: n=455 | agreement h1_band=51% verdict=71% signature=87% purity_band=2% claimed_type=85% ε=46% | |Δε| median=0.01 ≥0.10=12% | h1_stakeholder null: before=100% after=4%
UNTOUCHED stratum (control): n=505 | agreement h1_band=100% verdict=97% signature=100% purity_band=100% claimed_type=100% ε=100% | |Δε| median=0.00 ≥0.10=0% | h1_stakeholder null: before=4% after=4%
```

The untouched stratum is the two-sided control and it still holds: 100% agreement on h1_band,
signature, purity_band, claimed_type and ε where the backfilled stratum moves to 51/87/2/85/46%.
The instrument declines on the stratum nothing happened to and fires on the stratum that was
regenerated — reproduced exactly, three days and one coherent reclassify later.
