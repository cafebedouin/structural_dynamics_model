# PREREGISTRATION — OQ-251 natural_law reachability post-OQ-70

**Frozen:** 2026-08-17, before any probe ran. md5 recorded in `audit_log.md` above the first
result line. Never retro-edited; the writeup quotes it.

**Plan of record:** `~/.claude/plans/put-together-a-plan-iterative-dusk.md` (Rev 4).

**Questions.** OQ-251 (P2, gates OQ-248's kill condition): (Q1, Ω_E) does ANY path remain by
which a paradigm natural-law story certifies `natural_law` after the OQ-70 fix (`72ec2cdd`,
2026-06-05)? (Q2, Ω_C) did OQ-70 *intend* the observed scope — is "no route to naturality
certification" a ruled outcome or an unchosen side effect?

**Hypotheses under test (planning-phase recon; the executor is the control on the recon —
these are to be independently re-witnessed, not inherited):**

- **H1** — OQ-70 (`72ec2cdd`) was never on the `natural_law` path; the change that stopped
  maxwell's certification is `8b5a34b8` (2026-06-11, OQ-43/OQ-44 fail-close,
  `has_viable_alternatives/2` default `false` → `unknown`).
- **H2** — `maxwell_demon_impossibility.pl:114` authors an explicit story-level
  `constraint_claim(maxwell_demon_impossibility, mountain)`, contradicting
  `GATE2_REWITNESS.md:70-71`.
- **H3** — exactly ONE conjunct blocks maxwell at HEAD: `HasAlternatives == false`.
- **H4** — the mis-attribution propagated into `prolog/narrative_ontology.pl:458-461`,
  `GATE2_REWITNESS.md:58-75`, and OQ-251's own ISSUES entry.

**Pass criterion throughout: the pasted output, never the prediction.**

---

## Phase 0 — apparatus positive control

Run `prolog/tests/test_oq113_dead_natural_law.pl` on its own header's load chain. Expected
3/3 (synthetic-profile fires / live corpus zero / never-false). **If not 3/3, HALT** — the
substrate has moved since planning; report, do not proceed on a red control.

---

## Phase 1 — H1/H2 code-level reads (outcome tables)

### P1 (era path) — `git show f600599b:prolog/signature_detection.pl`

| Outcome | Content | Consequence |
|---|---|---|
| A | era `natural_law` producer is the profile path; `claimed_natural/2` feeds FNL only | H1 mechanism holds |
| B | era `claimed_natural` feeds the natural_law path | H1 false as a mechanism claim. **Phase 1.5 runs regardless** — the bisect is behavioral and mechanism-independent; P1's outcome changes what the bisect result MEANS, never whether it runs. Interpret the bisect against OQ-251's original framing; escalation E2 if the pinned commit contradicts both framings. |

### P2 (the two candidate commits) — scoping only, decides nothing

Paste `git show 72ec2cdd -- prolog/signature_detection.pl` (does the diff touch only
`claimed_natural` source 2 / `appears_as_rope` sibling, or does it reach the natural_law
clause / `has_viable_alternatives`?) and `git show 8b5a34b8` (the `false`→`unknown` default
flip + its date).

### P3 (maxwell's claim) — `maxwell_demon_impossibility.pl:114` + `claimed_natural/2` clause 1

| Outcome | Content | Consequence |
|---|---|---|
| present | explicit story-level mountain claim exists | H2 witnessed |
| absent | no such claim | H2 fails; GATE2's statement stands; step-16 correction drops that limb |

---

## Phase 1.5 — P-BISECT: the attribution witness (three-point, behavioral)

Method: `git archive <commit> | tar -x` into a scratch tree under the session scratchpad; no
worktree, no checkout of the live repo. Corpus held at HEAD across all three points via an
**absolute** `corpus_path` overlay to the live repo's
`prolog/archives/datasets/kernel_v1`.

Query at each point:
`findall(S, signature_detection:constraint_signature(maxwell_demon_impossibility, S), Ss)`

- **Point 1, `f600599b`** (pre-both): expected `natural_law` present. This is a REPRODUCTION
  of GATE2 arm C, validating the scratch-tree method against a known result. **If it does not
  reproduce, HALT** — the probe apparatus is broken, nothing downstream counts.
- **Point 2, `8b5a34b8^`** (post-`72ec2cdd`, pre-fail-close): the discriminating point.
- **Point 3, `8b5a34b8`** (post-fail-close): expected `natural_law` absent.

| Outcome | Content | Consequence |
|---|---|---|
| A | fires at point 2, absent at point 3 | cause pinned to `8b5a34b8`; H1 attribution WITNESSED, not argued. All Phase 4 corrections licensed. |
| B | absent at point 2 | `72ec2cdd` (or something else in `f600599b..8b5a34b8^`) killed it — H1 false as stated; bisect further inside the window before writing any correction; if the binding commit is neither candidate, HALT (E2). |
| C | fires at point 3 | the fail-close did not kill it either; HEAD non-firing has a later cause; extend the bisect toward HEAD; no correction ships until the binding commit is in hand. |

---

## Phase 2 — H3 witnesses + the FNL exposure (live repo, kernel_v1 overlay)

### P4 (per-conjunct attribution at HEAD)

| Outcome | Content | Consequence |
|---|---|---|
| A | all conjuncts pass except `HasAlternatives == false` (slot = `unknown`) | H3: single blocker |
| B | another conjunct also fails | multi-blocker; the writeup's attribution table changes and the GATE2 correction must name all of them |

### P4b (FNL on maxwell — standalone)

| Outcome | Content | Consequence |
|---|---|---|
| A | FNL does not fire; the pasted compliance result shows why (compliant) | designed behavior: claim present, compliance gate holds |
| B | FNL FIRES on maxwell | false positive on the reference genuine law — headline altitude; mint its own OQ; do NOT fold into the OQ-251 close (E4) |
| C | the compliance read errors/aborts | report the substrate failure; do not coerce to either outcome |

### P5 (end-to-end flip control)

- **P5a — pre-injection:** expected no `natural_law`. If `natural_law` IS present pre-injection
  → **escalation E1** (a live path; OQ-248 kill condition trips).
- **P5b — injected** (`asserta` of `has_viable_alternatives(maxwell_demon_impossibility, false)`
  + cache clear):

| Outcome | Content | Consequence |
|---|---|---|
| A | `natural_law` appears | full path intact except the one conjunct; H3 confirmed end-to-end |
| B | still absent | a second blocker P4 missed; re-run P4 under the overlay and find it before writing anything |

- **P5c — restore witness:** after cleanup + cache clear, the P5a query must match
  byte-for-byte.

### P6 (range claim — clauses first, corpus second)

(a) `listing(signature_detection:has_viable_alternatives/2)` + clause-by-clause statement of
why no head or body can yield `false`. **If any clause binds the value from a CALLEE rather
than a literal, recurse until every binding site is a literal, or downgrade the claim from
"unsatisfiable by construction" to "unreachable over the authored field domain" and say
explicitly which is asserted.** (b) corpus enumeration as CORROBORATION (expected
`[true, unknown]` or `[unknown]`). A `false` in (b), or a clause in (a) that can emit `false`,
is an OQ-113 regression → HALT + report.

---

## Phase 3 — Q2 ruling-scope read + consumer sweep

### Step 12 — Q2 disposition (read-only)

Quote OQ-70's ruling text (ISSUES entry + `72ec2cdd` commit body) and confirm its scope
language is FNL/claims-side only; quote the OQ-43/OQ-44 fail-close ruling (`8b5a34b8` +
GAP-08) and OQ-113's fork-(b) ruling. If all three read as quoted, Q2's *scope* question
resolves without a new ruling. If the ruling texts do NOT support this read → **E3** (genuine
Ω_C, operator's seat). Either way Q2's disposition does NOT absorb the consumer surface.

### Step 13 — P7 consumer sweep + the consumer-surface OQ

Pinned `/usr/bin/grep -rn "natural_law"` over `prolog/` and `python/` (excluding `testsets*`,
`archives/`), with a planted-name positive control. Classify every consumer of the
`natural_law` signature ATOM as constant-zero-reader / exemption-never-granted /
dead-map-entry. **Pre-authorized (operator, plan review 2026-08-17): mint a new OQ for this
surface regardless of how H1–H4 resolve.**

### Step 14 — P8 (H4 witness + line-drift check)

Paste the CURRENT text at `prolog/narrative_ontology.pl:458-461` and
`GATE2_REWITNESS.md:58-75` BEFORE editing either. If the pasted text does NOT contain the
OQ-70 attribution, H4 is unwitnessed at that site — the corresponding correction step is
dropped, not retargeted.

---

## Phase 4 gate (declared in advance)

Steps 15–21 run only if H1–H4 all witnessed (H1 = P-BISECT outcome A; H2 = P3 present;
H3 = P4/P5 outcome A; H4 = P8 attribution-text present). If any H failed, the write pass
shrinks to (a) the audit dir + WRITEUP.md reporting exactly what was and wasn't witnessed,
(b) the step-13 consumer OQ (unconditional), (c) an OQ-251 status note recording the partial
result and the re-plan need. **P6's downgrade branch is NOT a gate failure** — it changes
wording in steps 15/19/20 only.

## Escalations (not self-resolved)

- **E1** P5a finds `natural_law` firing pre-injection → OQ-248 kill condition trips → HALT.
- **E2** P-BISECT outcome B/C landing outside both candidate commits → HALT.
- **E3** Phase 3 step 12's ruling texts do not support the chosen-not-side-effect read.
- **E4** P4b outcome B (FNL false-positive on the reference genuine law).
- **E5** gate-2 re-affirmation question — always surfaced, never decided by the executor.
