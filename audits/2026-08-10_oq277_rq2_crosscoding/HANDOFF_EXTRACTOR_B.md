# HANDOFF — extractor B's 13 units (direction (ii))

> ## ✅ CLOSED 2026-08-11 — B's half landed 13/13; extraction (direction (ii)) is COMPLETE
>
> | | |
> |---|---|
> | B's units | **13 of 13** — `packets/our_units/{14..26}_*.json` (numbered in `extraction_split.json` order) |
> | All direction-(ii) units | **26** (A 13 + B 13). Matrix cells = 22 (A 13 + B 9); B's four `overlap_source: true` extractions are floor-only |
> | Leak sweep | run after every unit; final `swept 26 units, direction (ii): 0 hits` |
> | `role` / `overlap_source` | `primary` on all 13; `overlap_source` written EXPLICITLY on all 13, true on exactly the four floor directories |
> | **NO-UNIT proposals from B** | **ZERO** — all 13 directories reported an incident. With A's zero, **k = 0** for the §H.2 row across the whole n=22 primary sample, subject to the operator's own read (the extractor may not self-certify either direction) |
> | `incident_location` (B's 13) | `subject` 7 / `self_audit_subsection` 4 / `incidental` 2. **Pooled over all 26** (A's 10/1/2 + B's): `subject` 17 / `self_audit_subsection` 5 / `incidental` 4 |
> | Blind-overlap rule | **HELD.** `our_units/01`, `06`, `12`, `13` were never opened, by any route; no `git log`/`git show` on the commits landing them; the leak sweep loads units programmatically and prints only hits. No comparison is VOID |
>
> **B's selection rule, stated so it is a rule and not a preference** (same as A's, adopted for
> continuity): fullest DESCRIBED mechanism — a unit can only carry the mechanism the prose states —
> tiebroken by realized consequence over caught-before-it-mattered.
>
> **Recording-threshold divergence, flagged rather than smoothed.** All **13 of 13** B units carry
> a non-empty `alternatives_not_extracted` (2–4 entries each, 38 total), against A's **6 of 13**.
> Do NOT read that as B's directories carrying more defects. B recorded every candidate it
> considered and rejected, *including* ones it judged not to be defects at all (a control working
> as designed, a declared scope limit, a stale comment) with the reason for rejection; A's
> convention may have been to record only genuine competing defects. The two halves' alternative
> counts are therefore **not comparable as a defect-density measure**, and no row should be built
> on them. The four floor comparisons are where extractor divergence is measured; this is a note
> about a field, not a finding.
>
> **Next step (unchanged, from §"After both halves land" below):** controls — anchors 3/direction,
> decoys 2, the PRE-DECLARED redaction pairs in `controls/redaction_pairs_predeclared.json` (do NOT
> choose them) — then the driver, then `PREREGISTRATION.md` with `verdict_grammar_amendment.md`
> incorporated verbatim and its md5 in `audit_log.md` ABOVE the first result line, then spend-go.
> **Before Phase 3, assert the driver's payload-capture count EQUALS the expected call count, then
> grep.** Count first.

**Written:** 2026-08-11 by extractor A2, on completing A's 13 of 13.
**Read first:** this file, then `frame/extraction_split.json`, then `HANDOFF.md` for the carried
rulings / pre-spend catches / writeup obligations. `HANDOFF_EXTRACTOR_A2.md` is A's file and is
now CLOSED — read its "Conventions established" section (they bind you) and nothing else from it.
**You are the EXTRACTOR. You never code.** Assigning a pattern to a unit breaks the experiment.

## STOP — the blind-overlap rule, which now has teeth it did not have before

When A2 was written, no unit existed for any overlap directory, so the rule was trivially safe.
**It is not any more.** Four files in `packets/our_units/` are A's extraction of directories YOU
must extract independently:

| do NOT open | it is A's version of | which is yours as |
|---|---|---|
| `our_units/01_recon_2.json` | `2025-05-15_recon_2` | overlap_from_A |
| `our_units/06_oq44_policy_close.json` | `2026-06-11_oq44_policy_close` | overlap_from_A |
| `our_units/12_oq124_oq149_committer_convention_control.json` | that directory | **primary** |
| `our_units/13_oq186_oq188_readsite.json` | that directory | **primary** |

Not once, not partially, not "just for the schema." The format is fully specified below and
`our_units/02_blocking_gate.json` is a clean template covering a directory you never touch.

**Two channels beyond the files themselves, both live:**
1. **`git log`.** A's commits name units by number; the commit landing 12/13 deliberately omits
   their content for this reason. Do not go looking for it in diffs either — `git show` on those
   commits prints the unit bodies.
2. **A glob or a sweep script.** The leak sweep in convention 7 loads every unit in the directory.
   That is fine — it reads fields programmatically and prints only hits — but do not print unit
   bodies to inspect them, and do not read the four files above into context by any route.

If you read one anyway, **say so in your handoff and mark that pair's floor comparison VOID**.
A voided comparison is recoverable; a silently contaminated one licenses the H5-gate extension
decision (`verdict_grammar_amendment.md` §E) on a fabricated basis, because two extractions that
are one extraction and its echo agree by construction and report INSIDE FLOOR whatever the truth.

## Your 13

`frame/extraction_split.json` → `extractor_B.primary` (11) + `overlap_from_A` (2). Measured md
volume, `find audits/<dir> -name '*.md' -printf '%s\n'` (measure, do not trust an estimate — a
prior handoff's was 4x low in the direction that made it look smaller):

```
python3 - <<'PY'
import json,subprocess
s=json.load(open('audits/2026-08-10_oq277_rq2_crosscoding/frame/extraction_split.json'))
for d in s['extractor_B']['primary']+s['extractor_B']['overlap_from_A']:
    out=subprocess.run(['find',f'audits/{d}','-name','*.md','-type','f','-printf','%s\n'],
                       capture_output=True,text=True).stdout.split()
    print(f"{sum(int(x) for x in out)/1024:8.1f} KB  {d}")
PY
```

Run from the repository root. **Largest first** — A stopped mid-half precisely because it left the
two largest for depleted attention. If you deplete, hand off at the boundary rather than thinning
units: thin units bias toward `other`, which is what control (c) exists to measure, so a degraded
extraction confounds the control meant to catch it.

## Conventions (from A2 §"Conventions established" — they bind, they are not preferences)

1. **One unit per directory**, own file, `packets/our_units/NN_slug.json`. Numbering continues at
   **14**; number in `extraction_split.json` order (primary 14–24, then overlap 25–26).
2. **Fields:** the four coder-facing ones (`symptom`, `mechanism_as_described`, `detection_path`,
   `consequence`) plus `source_dir`, `extractor` (`"B"`), `role`, `files_read`,
   `extraction_notes`, `metadata`.
3. **`files_read` is mandatory** — it makes the adjudicator's fidelity spot-check re-derivable
   instead of a trust exercise. List what you actually read; note targeted scans as scans.
4. **The unit is the INCIDENT the directory reports, never the directory's own subject.**
5. **More than one defect in a directory → record the ones you did not take** in
   `extraction_notes.alternatives_not_extracted`, each with why. **State your selection rule**, so
   it is visible as a rule rather than a preference. A's rules, for continuity, not obligation:
   fullest DESCRIBED mechanism (a unit can only carry the mechanism the prose states), tiebroken by
   realized consequence over caught-before-it-mattered. 6 of A's 13 needed this.
6. **`metadata.incident_location`** is mandatory: `subject` | `self_audit_subsection` |
   `incidental` (`verdict_grammar_amendment.md` §H.1). A's distribution: 10 / 1 / 2.
7. **Compose free of the P-lexicon, then sweep.** Never write freely and redact after — that
   produces mangled mechanisms, and mangled mechanisms bias toward `other`.
8. **Sweep after every unit** (do not write a second matcher):
   ```
   cd audits/2026-08-10_oq277_rq2_crosscoding/packets
   python3 - <<'PY'
   import json,glob,sys; sys.path.insert(0,'../../../python/audits')
   import oq277_lexicon as L
   n=hits=0
   for f in sorted(glob.glob('our_units/*.json')):
       u=json.load(open(f)); n+=1
       for fl in L.CODER_FACING_FIELDS:
           for h in L.scan(u.get(fl,''),'ii'): print('LEAK',f,fl,h); hits+=1
   print(f"swept {n} units, direction (ii): {hits} hits")
   PY
   ```
9. **Commit every 2–3 units.** In-flight work is what compaction destroys. **For the two overlap
   units, keep their content OUT of the commit message** — A's did, for the reason above.

## NO-UNIT — the rule is fixed; apply it, do not re-decide it

A directory yields a **UNIT if its prose REPORTS an incident anywhere in the document**, subject or
not; **NO-UNIT only if the prose DISCUSSES the concept without reporting an instance**
(`verdict_grammar_amendment.md` §H.1). **A NO-UNIT call is operator-confirmed, never
self-certified** — you proposing NO-UNIT and you having extracted thinly are competing explanations
for the same observation and you cannot adjudicate between them. Write the NO-UNIT file with your
reason and files read; the operator rules.

**A's half produced ZERO NO-UNIT proposals** (13 of 13 directories reported an incident). Under
§H.2 the row is `k` = operator-confirmed NO-UNIT dirs across the whole n=22 primary sample, so the
count is not closed until your half lands. Do not read A's zero as the answer.

## `role` and `overlap_source` — RULED 2026-08-11, apply it, do not re-decide it

`role` was encoding two independent facts and produced an inconsistency in A's half. The operator
split them (`verdict_grammar_amendment.md` §I, now part of the prereg):

- **`role: "primary"` on every unit — all thirteen of yours, as with all thirteen of A's.** Matrix
  membership is a *sampling* fact, fixed by the seeded draw. It is not what `role` decides.
- **`overlap_source: true|false`** — a *control* fact, fixed by the overlap design. Set it **true**
  on your four floor-participating directories: `2025-05-15_recon_2`,
  `2026-06-11_oq44_policy_close`, `2026-06-27_oq124_oq149_committer_convention_control`,
  `2026-07-11_oq186_oq188_readsite`. **False on the other nine. Write the field explicitly on all
  thirteen** — absent-means-false is the same two-tokens-one-slot collapse this fixed.
- **The driver quarantines on `overlap_source`, never on `role`.**

**Cell accounting (do not drift from it):** each of the 22 sampled directories contributes exactly
ONE unit to the matrices. A contributes 13; **you contribute 9**; your four overlap-directory
extractions are floor-only and never enter cells. 13 + 9 = 22. Your four are quarantined, not
discarded — they are half of every floor comparison, so extract them at full quality. Writing them
thinly because they "don't count" would shrink the measured floor toward zero, which reads as
"extraction is reliable" and would retire the control by flattering it.

**One consequence, pre-registered before the floor is measured** (§I.1): only the four overlap
directories will have a measured extraction error bar. Four measured units do not license a claim
about thirteen or twenty-two. Do not write, and do not let a later reader infer, that the overlap
units are representative of extraction quality generally.

## After both halves land

1. **The rest of the sequence:** controls (anchors 3/direction, decoys 2, the PRE-DECLARED redaction
   pairs in `controls/redaction_pairs_predeclared.json` — do NOT choose them), then the driver, then
   `PREREGISTRATION.md` with `verdict_grammar_amendment.md` incorporated verbatim and its md5 in
   `audit_log.md` ABOVE the first result line, then request spend-go. **Before Phase 3, assert the
   driver's payload-capture count EQUALS the expected call count, then grep** — a capture bug
   writing zero payloads yields a clean leak-grep and a green H2. Count first.
