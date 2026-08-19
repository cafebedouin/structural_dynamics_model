# OQ-303(a) — class-B conversion rollout: Phase 1 landed zero-diff on six legs; the clause-order census says the latent exposure is 165 of 218, not zero

**Executed:** 2026-08-18 → 2026-08-19 (directory dated to the open)
**OQ:** OQ-303 (arm (a))
**Verdict:** `signature_grade/2` — the one `live-output-path` row — is converted, and its
six-leg clean-vs-edited pair reads **0 changed constraints over 5,311**, so the pre-declared
**Ω_P escalation DECLINED**: the conversion is Ω_E and discharged. The census answering the
second carry found the latent exposure is **not** zero — **165 of 218 (predicate, atom) pairs
across 57 of 58 predicates** are hazardous-if-called-bound today, held back solely by the fact
that nobody calls them bound. **STATUS: IN PROGRESS** — Phases 3–4 (the 55 batch, then the 2
`converts-clean-minus-dataflow` rows) are OPEN; nothing is merged to `main`.
**Substrate:** six legs — `testsets` (n=279), `testsets_haiku` (960), `testsets_flash` (960),
`testsets_kimi` (1005), `testsets_sonnet` (1001), `archives/datasets/kernel_v1` (1106) =
5,311 constraints. Clean half at `code_commit` 2f459d3a, edited half at 6c1bfa44, corpus md5
identical per leg across halves. swipl 10.0.2.
**Fired:** live — the census's pre-registered control fired on the census itself (its first
version reported steal-risk 0 for both of `signature_grade/2`'s atoms, contradicting Unit A's
five-leg measurement), and the transformer's selftest fired three times on the transformer
before it touched a tracked file. The conversion itself flipped 29–33 over-permissive
`commentary` answers per leg into agreement with the engine, with zero consumer-visible change.
**Evidence map:**

| artifact | what it is | which claim it witnesses |
|---|---|---|
| `PREREGISTRATION.md` | frozen before any conversion or six-leg run; md5 `000742e0…` recorded in `audit_log.md` above the first result line | the ordering, the two carries, the Ω_E/Ω_P outcome table, the predicted zero-diff |
| `audit_log.md` | chronological log, R1–R6, with pasted output | every numeric claim below |
| `sixleg.py` | the six-leg pair harness: mtime-advance gate, corpus md5 fingerprint per half, per-leg refusals | §2 — that the pair actually ran on comparable substrates |
| `sixleg_clean_manifest.json`, `sixleg_edited_manifest.json` | per-leg provenance for both halves | §2 |
| `sixleg_diff.txt` | the diff output | §2 |
| `clause_order_census.pl`, `clause_order_census.py` | the steal-risk census + its fail-closed control | §3 |
| `clause_order_census.md`, `clause_order_census_raw.txt` | the census table and raw output | §3 |
| `convert.py` | the template transformer with adversarial parse fixtures | §4 |
| `../2026-08-18_bound_caller_rewitness/` | Unit A: the partition this unit executes | inputs |

---

## 1. Carry (i) — `commentary` reachability, answered before the run

Pre-registered from reads, not read off the diff afterwards (`audit_log.md` R1):
**`commentary` is reachable downstream as a VALUE but is queried NOWHERE as a BOUND SELECTOR.**
`diagnostic_summary:verdict_join/3:728` queries `signature_grade/2` **unbound**; the value is
serialized as `verdict_join.signature_grade` (`json_report.pl:1509`) and read live by
`python/tensions_ledger.py:173`. The engine's only bound caller is
`signature_detection.pl:1951`, at `correction`.

That distinction is what decides Ω_E from Ω_P, which is why it was fixed in advance along with
what each outcome would mean.

## 2. Phase 1 result — zero-diff, and the three ways a zero-diff can lie, closed

```
testsets  0 | testsets_haiku  0 | testsets_flash  0
testsets_kimi 0 | testsets_sonnet 0 | kernel_v1 0        (5,311 constraints)
```

- **Did it run?** Per leg, the output file's mtime was asserted to advance past a pre-run
  marker. A `run_pipeline`-family gate that aborts before writing leaves a stale file that
  diffs byte-identical against itself.
- **Are the halves comparable?** Corpus md5 fingerprinted per leg on both halves and compared
  — identical. Code state differs (2f459d3a vs 6c1bfa44), so the edited half did pick up the
  edit.
- **Can the differ report nonzero?** One field planted in one record of one leg: it names the
  constraint and the field, and declines when restored. Both directions, same session.

**So the Ω_P escalation declined.** That is the result, not a caveat: the escalation was live,
pre-declared, and did not fire. The conversion is Ω_E, discharged.

**The conversion is not a no-op — it is a no-op *at the consumer*.** Re-running Unit A's
agreement probe post-conversion, the bound form at `commentary` moved from 263→234 (testsets)
and 932→899 (haiku), i.e. into exact agreement with the engine, while `correction` was
untouched. Precisely 29 and 33 over-permissive answers retired, and zero consumer-visible
change, because nothing in the engine queries at `commentary`.

## 3. Carry (ii) — the census: the exposure is 165 of 218

`steal_risk(P, A)` = cut-bearing clauses of `P` whose head output arg is an atom ≠ `A`, before
the last clause that can yield `A`. Upper bound by design.

**57 of 58 `latent-B` predicates carry nonzero steal-risk at some atom; 165 of 218 (predicate,
atom) pairs.** Max 17 (`qualify_action/5`), then 14, 13, 11, 10. The single zero,
`resolve_with_perspectival_check/4`, is zero trivially — one atom, nothing to skip.

"May be zero" was the wrong prior, and for a structural reason rather than a surprising one:
these rows are in the registry *because* they carry the shape. What the census adds is
magnitude, granularity, and this: **the `latent-B` label holds all 165 back on one fact alone —
nobody calls them bound.** Unit A already showed that fact resting on an instrument with a
proven false negative.

**The control caught the census.** The first `steal_risk` treated a variable-headed cut-bearing
clause as an unconditional commit and reported 0 at *both* of `signature_grade/2`'s atoms — a
clean-looking table contradicting a five-leg measurement. A cut is not reached unless the body
reaches it. The driver fails closed on that check, so the corrected split is a precondition of
any zero in the table.

**And it hands the batch a witness stronger than a corpus diff.** Post-conversion the census
must read 0 of 218: a corpus diff says "no story exercised the difference", the census says the
difference is gone. `signature_grade/2` already reads `atoms=0 max_steal_risk=0`.

## 4. The transformer, and why it is a transformer

55 hand edits and one transformer make different *kinds* of mistake. A hand edit can drop a
guard in a way no structural check catches; a transformer makes the same mistake everywhere,
where the checks see it. The checks license it, not the code: clause count and cut-presence
compared before/after via the Prolog reader, no bare atom left in an output position, the file
still loads, then the batch-level six-leg pair and gate.

**Its fixtures fired three times before it touched a tracked file:** a `0'c` char-code
off-by-one that swallowed the rest of a file; comment text absorbed into the following clause's
span — which produced the *right* answer on `signature_detection.pl` by layout luck; and an
over-strong post-condition ("every output is a variable") that would have reverted any file
with a compound output argument. The second is the one worth keeping: a parser that is wrong
and right-by-accident on the file you tested it on is the failure mode this whole arc is about.

## 5. A tool that could not run the witness it exists for

`classify_corpus`'s `run_prolog` timeout is a hard-coded 300 s, sized on the live leg (~35 s).
Measured this session: haiku **288 s** (13 s of headroom), flash **530 s**, sonnet **724 s**,
kernel_v1 **577 s**. The first pair attempt burned three full-length attempts and refused on
flash. Fixed by a `timeout`/`soft_timeout` pass-through, default unchanged. Worth naming
because the failure was *loud and correct* — the harness refused rather than emitting a partial
— and still cost a full run to discover, since nothing about the default announced that it was
sized for one leg out of six.

## 6. Residue — what is OPEN

- **Phase 3: the 55 `converts-clean` rows.** Not started. Owed: `convert.py --all`, census → 0
  of 218, six-leg pair, gate, Prolog suites, registry entries retired in the same change.
- **Phase 4: the 2 `converts-clean-minus-dataflow` rows** (`json_report.pl`
  `boltzmann_label/2`, `live_index_label/3`), deliberately last — least evidence behind them.
- **Nothing merged to `main`.** Branch `oq303-classb-rollout`.
- **`caller_sweep.py` is not retired by any of this**, and OQ-303(c) is untouched.
