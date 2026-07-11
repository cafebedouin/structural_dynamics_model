# PREREG — OQ-188 + OQ-186 read-site resolution (pre-registered 2026-07-11)

Written and committed BEFORE any probe or census run (Phase 0). Every criterion below is
fixed now; Phase-1 outputs decide only which pre-registered branch fires, never the criteria.

## Scope

- **OQ-188**: should a read site flag that an institutional seat's verdict is knife-edge on a
  discrete authored stakeholder-role choice (agenda_setter d=0.12 vs beneficiary d=0.25
  straddling the f(d) sign root)?
- **OQ-186**: should the contamination-network/convergence read carry a common-cause
  (node-independence) caveat so a clique of co-authored slices stops reading as mutual
  corroboration?
- Joint witnessed defect: `python/evaluative_convergence.py` `build_defensibility` (:346)
  rules "coordinated rather than independent operation" on uniform institutional=rope —
  the OQ-188 config artifact, not a cartel
  (`outputs/constraint_reports/dispositional_reading_report.md:396,428`).

Out of scope: OQ-01 re-adjudication (rope-gate χ≤0 bypass stays; everything here is
commentary-grade — annotates, never overrides classification); moving any config constant
(operator's declared seat); OQ-78 re-baselining watch; OQ-103/OQ-174 redesign; any LLM
generation spend or corpus rebuild.

## Block 1 — OQ-188 flip-under-role-change predicate (zero free parameters)

An institutional seat **flags** iff BOTH:

1. its serialized `perspective_chi.institutional.d` matches a `stakeholder_role_d_*` constant
   (tolerance 1e-6, compared against the SERIALIZED `config` section of the same pipeline
   output — never a hardcoded copy), AND
2. `sign(f(d_authored)) ≠ sign(f(d_nearest_alt))`, where `d_nearest_alt` minimizes
   |d_r − d_authored| over the other four role constants, and f is the sigmoid built from the
   serialized `sigmoid_*` params.

The f(d) sign root (from serialized params L=−0.20, U=1.50, d0=0.50, k=6.00):
d\* = d0 − ln((U−L)/(−L) − 1)/k = 0.50 − ln(7.5)/6 ≈ **0.16418**.

**All five role constants pasted** (the nearest-alt claim is contingent on every value;
verified against `prolog/config.pl:156–160` and the serialized `config` section, both read
2026-07-11 pre-run):

| role | d | f(d) sign | nearest-alt | alt sign | flags? |
|---|---|---|---|---|---|
| agenda_setter | 0.12 | − | beneficiary 0.25 | + | **fires** |
| beneficiary | 0.25 | + | agenda_setter 0.12 | − | **fires** |
| observer | 0.72 | + | payer 0.85 | + | silent |
| payer | 0.85 | + | excluded 0.90 | + | silent |
| excluded | 0.90 | + | payer 0.85 | + | silent |

Both straddle partners flagging is CORRECT (the fragility is symmetric), not noise.

Bucket rules (Pattern 6 — no silent unflagging):
- Canonical-fallback seats (d = 0.00, `canonical_d_institutional`) are OUT OF DOMAIN of the
  role predicate (no authored role selected d) — bucket `canonical`, counted and surfaced.
- A serialized d matching NO role constant (recon saw d=0.15 ×6) → bucket `unmatched`,
  counted and surfaced, never silently unflagged; it SHRINKS the gate's denominator, so the
  fire rate must not be read as "share of all institutional seats".
- null/absent d → EXCLUDED from the denominator, count reported (OQ-51 idiom: null ≠ 0).

## Block 2 — Fire-rate decision gate (branch pre-committed)

Census over the live `outputs/pipeline_output.json` (cite its manifest). Denominator =
institutional seats whose d MATCHES a role constant; matched/unmatched/null/canonical counts
all reported, for all four seats (institutional is the gated one).

- **≥50% of matched institutional seats flag** → **standing type-level form** (pre-specified
  NOW, not designed in Phase 2): ONE legend sentence per report (in the HOW TO READ THIS
  REPORT block, beside the OQ-187 RED note) PLUS a compact per-line glyph (`‡`) on affected
  institutional verdict lines — NEVER repeated caveat text per line (repeated text at ~78%
  fire rate is exactly the always-on disclaimer OQ-187 died on).
- **≤25%** → per-constraint conditional flag (caveat text rendered only on flagged
  constraints' reports).
- **25–50%** → escalate to operator (`blocked_on_human`); do NOT self-resolve.

Recon (2026-07-11, n=130 live: institutional d=0.12 ×101, 0.15 ×6, null ×9, ~15 non-role-constant)
expects the ≥50% branch — but this census, not the recon, decides.

## Block 3 — Caveat naming + exact texts (match the predicate, not root proximity)

Helper name: `_role_flip_caveat()` in `python/enhanced_report.py`. The text says the verdict
*flips under a single authored role change* — NOT "near the root": beneficiary at f=+0.110 is
continuously robust, and a proximity phrasing would read as a false alarm there.

**OQ-188 legend sentence (standing branch), verbatim:**

> ‡ on an institutional type = the verdict flips under a single authored stakeholder-role
> change: the seat's authored role d and its nearest alternative role constant sit on opposite
> sides of the f(d) sign root (agenda_setter 0.12 ↔ beneficiary 0.25 straddle d\*≈0.164), so
> the institutional rope/not-rope reading is role-authored, not situation-measured. Standing
> note — OQ-188.

**Per-constraint branch text (if ≤25% branch fires), verbatim:**

> ⓘ Institutional verdict is role-authored knife-edge (OQ-188): a single authored
> stakeholder-role change (agenda_setter ↔ beneficiary) crosses the f(d) sign root and flips
> this seat's verdict.

**Glyph:** `‡` appended to the institutional per-seat type at the structured render sites
(`Live Type:` / `Batch Type:` lines in `enhanced_report.py`; `per-position types:` line in
`tensions_ledger.py`, with one standing legend line in the ledger header).

## Block 4 — OQ-186 common-cause discriminator

`_edge_is_common_cause(subject_entry, neighbor_entry)` — fires iff ALL of:
- beneficiary-set overlap ≥ 1, AND
- victim-set overlap ≥ 1, AND
- |Δε| ≤ 0.02 (base_extractiveness of the two constraints).

**Domain:** corpus-derived agent edges only (`edge_type ∈ {shared_beneficiary,
shared_victim}`). Authored `explicit` edges and `inferred_coupling` edges are OUT OF DOMAIN
(rendered `n/a`, never silently false). A neighbor absent from the batch → `n/a (not in
batch)`, counted, never silently false.

**The 0.02 margin is AUTHORED** (owned as such): chosen below the smallest ε rail spacing
(0.04, the .x8/.x2 grid) so distinct rail values never read as near-duplicates; not derived.

**Subordinate gate (pre-registered):** census whether the ε clause discriminates. If
|Δε| ≤ 0.02 ALSO holds for a MAJORITY of non-both-sides agent-edge pairs (plausible given the
0.68 ε mode), DROP the ε clause and key on both-sides overlap alone.

Salience (OQ-103) and independence (this) stay orthogonal bits carried side by side; the
OQ-103 salience floor and the OQ-174 shared-input ruling are untouched.

**OQ-186 legend sentence, verbatim:**

> Independence: 'common-cause' = a corpus-derived edge whose two constraints share ≥1
> beneficiary AND ≥1 victim [at near-identical ε (|Δε| ≤ 0.02)] — consistent with co-authored
> slices of one underlying fact, so convergence across such edges is re-description, not
> independent corroboration (OQ-186).

(The bracketed ε clause is dropped verbatim if the subordinate gate kills it.)

**Defensibility downgrade text, verbatim** (replaces "indicating coordinated rather than
independent operation" when EITHER evidence boolean is true):

> consistent with a shared configuration mechanism (OQ-188: all members' institutional seats
> are role-authored knife-edge) and/or a common authored cause (OQ-186: members form a
> common-cause clique) — not by itself evidence of coordination.

Evidence booleans added to `detect_convergent_institutional` output:
`all_members_knife_edge` (every split member's institutional seat fires the Block-1
predicate) and `members_common_cause_clique` (every pair of split members is common-cause
per Block 4). The XCON elevation of `convergent_institutional`
(`enhanced_report.py:3219-3227`) is suppressed (not elevated to a primary claim) when either
boolean is true, with a one-line reason.

## Block 5 — A/B probe, pre-registered outcome meanings

Fixture (synthetic, asserted via `probe_harness:with_asserted/2`, one `constraint_metric`
each so `phantom_subject/1` passes; fixture idiom:
`tests/test_phantom_neighbor_filter.pl:61-69`):

- **Topic A** (3 slices of one fact): `oq186_a1/a2/a3` — same victim, same beneficiary,
  ε **0.68 / 0.69 / 0.68**. (0.69 keeps Δε=0.01 safely inside the 0.02 margin; 0.70 would sit
  float-exactly ON the boundary — 0.70−0.68 = 0.019999… — and pass only by rounding luck,
  one representation change from a spurious outcome (c).)
- **Topic B** (3 genuinely distinct): `oq186_b1/b2/b3` — pairwise-distinct victims AND
  beneficiaries, extractive ε.

Run `drl_purity_network:constraint_neighbors/3` on all six; paste neighbor lists.

- **A forms the shared-agent clique AND B forms no agent edges** → the machinery *does* form
  the edges and cannot distinguish independence; the defect is the corroboration READ →
  proceed with the read-site caveat (OQ-186 outcome (a)).
- **A forms no clique** → check the fixture first (positive control); if the fixture is
  sound, OQ-186 closes outcome (c) (machinery already separates; concern is essay-side only)
  and only the defensibility-text fix lands.

Note: outcome (b) (discriminator un-computable from authored fields) is already FORECLOSED
by recon — beneficiary/victim identity lists + ε verified serialized
(`json_report.pl:386-396`, live sample 2026-07-11).

## Phase plan (for the record)

0. This file, committed before any run.
1. A/B probe + OQ-188 fire-rate census + ε-discrimination census → evidence + README here.
2. OQ-188 read-site (output-changing commit) + separate behavior-preserving
   `prolog/config.pl:156-160` comment.
3. OQ-186 read-site (output-changing commit): enhanced_report.py Independence column +
   legend; evaluative_convergence.py evidence booleans + defensibility downgrade; XCON
   suppression; tensions_ledger edge marker.
4. Regression tests: `prolog/tests/test_oq186_common_cause_clique.pl` + Python-side
   two-sided OQ-188 flag test.
5. Bookkeeping: ISSUES.md (both OQs), omega index, KNOWN_STATE.md, gate GREEN.

No new Prolog reading predicate anywhere in this plan → `reading_registry.pl` obligation
discharged as N/A.
