# B3 — Content-read rubric + strata registration (PRE-REGISTERED)

Written BEFORE any original_v6 404-member story is content-read, and before the
suspiciousness selection is run. mtime of this file vs the b3_* verdict files is the
ordering witness.

## Rubric (fixed; applied per story, text-only)

**Inputs the reader may use:** the NARRATIVE PROSE of the story file only — the SUMMARY block,
narrative-context comments, perspective/reading prose, omega/naturalness commentary. The reader
must IGNORE all machine fields: `constraint_metric` numbers, `constraint_beneficiary/victim`
facts, `domain_priors:*` scores, classifications. (Rationale: the engine screen already reads
those; the content read exists to catch what faithful-looking machine fields hide. In the
pre-flight the known cases DO carry beneficiary facts — reading them would trivialize the
control.)

**Question:** does the story text (a) assert or presuppose that the constraint is natural,
emergent, inevitable, or authorless — "just how things are" — while (b) describing, anywhere in
the same text, a party that systematically GAINS from the constraint's persistence (material,
positional, or authority gains — not merely "benefits exist diffusely")?

**Verdict per story (exactly one):**
- `hidden-winner` — both (a) and (b) hold, with a quoted passage for EACH. The naturalness
  claim conceals or launders the gain.
- `genuine-natural` — (a) holds and the text describes no systematically gaining party; or the
  constraint is a formal/mathematical/physical limit whose "beneficiaries" are only
  perspectival (e.g. "cryptographers rely on P≠NP" is use, not asymmetric extraction).
- `ambiguous` — a gaining party is arguable but the passage supports both readings; quote it
  and say what additional text would settle it.

**Output shape per story:** `{id, verdict, naturalness_quote, winner_quote_or_null, note}`.

## B3.0 pre-flight (positive control for THIS rubric)

Before any 404 read: apply the rubric to the story text of 3 known authored-extraction
false-mountains from kernel_v1 (OQ-52 anchors):
`quran_9_5_scope__abrogating_universal`, `article_9_war_renunciation__strict_pacifist_reading`,
`abrahamic_covenant__isaac_covenant_reading`. Each must come out `hidden-winner` from prose
alone. If any does not, the rubric is broken — fix and re-run the pre-flight before touching
the 404. Cross-corpus caveat: kernel_v1 ≠ original_v6; the control establishes the rubric CAN
catch a known case, not that the corpora are alike.

## RUBRIC v2 (amendment after pre-flight FAIL — v1 kept above for the record)

**Pre-flight outcome on v1 (2026-07-01): 0/3 anchors fired** — all three returned `ambiguous`
with the same structural reason: (b) was unambiguous in each (winner named in prose), but v1's
(a) required story-voice naturalness, and these stories present naturalness as an IN-FRAME
appearance ("appears immutable", "appears as inevitable rather than enforced", "divine mandate")
while the analytical narrator contests it. The control worked: v1 cannot fire on a known case.

**Amended (a):** the constraint is presented as natural / emergent / inevitable / authorless —
in story voice, OR as a live in-frame reading anywhere in the prose (a perspective, doctrine, or
seat that experiences/claims it as "just how things are"), EVEN IF another voice in the same
story contests or unmasks it. Rationale: OQ-45 hunts mis-authoring — a winner present in content
but absent from the fact tables. For NL-certified stories, naturalness is certified by the
engine; the discriminating condition is (b). A prose-open winner in an NL-certified story IS the
mis-authoring (the hiding happened at fact-authoring time, not in the prose).

(b), verdict set, and output shape unchanged. Pre-flight re-run required under v2: all 3 anchors
must return `hidden-winner`.

## Strata (adversarial-primary; registered before selection runs)

1. **Screen-flagged** (B2): any of the 404 that is false-mountain-shaped (strict or loose) or
   carries any authored beneficiary. Read ALL.
2. **Most-suspicious ~25** among the unflagged, by pre-registered criterion, in order:
   (a) rank the 404 by `h1_band` descending (from `pipeline_output_original_v6.json`),
       tie-break by `wasserstein_total_fracture` descending;
   (b) independently, grep each member's .pl prose for emergence-vocabulary
       (`natural|inevitab|emerg|spontaneous|no one designed|nobody controls|law of`)
       co-occurring in the same file with a named persistent-party token
       (`corporation|industry|state|government|institution|elite|incumbent|monopol|
       cartel|lobby|owner|shareholder|platform|church|clergy|regime|dynasty`);
       rank by co-occurrence count.
   Take the union of top-15 from (a) and top-15 from (b); if the union exceeds 25, trim from
   the (a)-list bottom. This recipe is FROZEN as of this file's mtime.
3. **Random calibration ~10**: seeded draw (`random.Random(45)` over the sorted member list)
   from the 404 minus strata 1–2. Calibration only — no rate claim (chimera-era corpus,
   prevalence forbidden per OQ-70/OQ-25).

## Verification hooks

- Spot-verify: every `hidden-winner` verdict re-read against the raw file before it enters the
  writeup.
- Reader agents return the structured verdicts; they are hypotheses until spot-verified.
