# Part C results (mapping revealed after calls commit `7581cf98`)

Mapping `presence_mapping.json` md5 `e6a26bed1870e2364d570750b3344452` — matches the
pin in PRESENCE_CALL.md (committed `088cd57a`, before adjudication).

## Per-run record (all runs: input md5 `a365da8a…` verified before AND after; corpus
snapshot diff empty after each; serialized; `--dry-run --skip-search`)

| Run | Manifest | P1 (`is_contested_kernel`) | kernel_id (name only — never identity) | Readings | Axes |
|---|---|---|---|---|---|
| 1 | `policy_debate_framework_2026_20260806_182706` | **true** | `role_of_debate_kernel` | 3 | 3 |
| 2 | `policy_debate_topicality_framework_20260806_182916` | **true** | `topicality_boundary_kernel` | 6 | 6 |
| 3 | `framework_bcfpml_2026_20260806_183116` | **true** | `legitimate_ballot_grounding` | 5 | 5 |

Staged-rule path taken: run 1 P1-pass → run 2; runs 1+2 P1-pass → run 3. HALT never
reached. Ingest ~220,720 tok/run (×3 ≈ 662K total — at the upper edge of the ~620K
worst-case estimate; the Part A re-mint grew the file 597K→672K B, which the estimate
predated).

## Presence clause (blinded, planted-control)

- Instrument-validity: PASS — the planted different-subject control (AT Fiat kernel,
  ITEM-Q) was called DIFFERENT from all three T Framework items (3/3 pairwise).
- Presence: the three T Framework items (P=run2, R=run1, S=run3) form one SAME group
  under the pinned subject+stance rubric — **kernel presence 3/3**, adjudicated blind.
- Read of the churn: kernel *ids* churned 3/3 and reading-set sizes churned (3/6/5) —
  consistent with OQ-264 (names never identity) — while the contested commitment
  itself (what legitimates the debate round / the topicality-framework boundary) is
  the blinded-SAME subject across all three draws.

## Verdict (read off the frozen grammar, PREREGISTRATION.md)

**Kernel presence 3/3 (name-blind subject+stance, blinded, valid instrument) AND P1
unanimity 3/3 → "graduated second meta-layer file" — the grammar's conditions are
MET.** T Framework joins AT Fiat as a meta-layer file for the purposes of OQ-259 item
2's graduation decision.

Scope of the claim (pinned language): this is a k=3-unanimous PRESENCE result on one
file under emphasis-blind, dry-run, skip-search conditions. It is not a detection
claim, not a stability constant for any other file or observable class, and not an
ingestion: actually adding T Framework stories to the K-file corpus is a separate
operator spend decision the (f) verdict's meta-layer exception now permits but does
not compel. Per-reading identity across the three draws was NOT adjudicated (only
kernel-level presence); reading-level claims stay draw-level.

## P1 base-rate bound (operator review, 2026-08-06 — added post-verdict, changes the
## claim's scope, not the verdict)

P1 (`is_contested_kernel`) entered the grammar without a measured churn floor of its
own — it is the observable Cap K churned 1/2 on at byte-identical input. The bound is
supplied by a ZERO-SPEND measured comparator that already existed: the three OQ-264
AT Fiat k=3 manifests all carry `is_contested_kernel: true` — **the already-graduated
meta-layer file also measures P1 3/3** (its kernel ids churned 2-of-3:
`simulated_action_efficacy` ×2 + `fiat_value_kernel`; reading counts 7/7/6). So P1
3/3 is consistent with a meta-layer-file-general base rate and does NOT discriminate
T Framework specifically; the graduation rests on the conjunction (P1 3/3 AND blinded
same-kernel presence 3/3) as pinned, with this bound now part of the verdict's scope.
Comparator population to date: meta-layer files P1 3/3 (AT Fiat), 3/3 (T Framework);
arsenal Cap K 1/2 with one empty-CSR draw; arsenal Biopower 3/3 (origin + 2 redraws,
per their manifests). n is small everywhere; these are draws, not rates.
