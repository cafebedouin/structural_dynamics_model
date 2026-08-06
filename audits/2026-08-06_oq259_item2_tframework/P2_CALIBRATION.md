# P2 two-sided calibration — RESULT: no token-mechanical form opens a gap in the required direction; P2 is unbuildable as a mechanical gate → ESCALATED to the C checkpoint

Executed 2026-08-06 (free; no API calls). Part C prereg work per the OQ-259 items-2–3
plan (rev 4): before Part C's promotion rule can pin P2, the meta-layer predicate must
be calibrated TWO-SIDED — ceiling on the graduated analog (AT Fiat, expected high) AND
floor on non-meta-layer analogs (Biopower NW, Cap K NW, expected low) — with the gap as
the discriminating quantity.

## Inputs (pinned)

- TAG inventories: mechanical extraction `grep -n '^#\{1,3\} ' <file>` (the OQ-264
  TAG_INVENTORY.txt method). Source md5s at run time: Biopower NW `722602a7…`, Cap K
  NW `18f726ab…` (both match the OQ-264 pins), AT Fiat `8d2224c863a44a466ea2b94571d8055e`.
- Manifests: AT Fiat k=3 triple (OQ-264 dir); Biopower NW origin + 2 Arm-0 redraws;
  Cap K NW origin + 2 Arm-0 redraws.
- Reading text: `commitment_system_recognition.readings[].commitment` ONLY (name-blind
  — reading_id excluded).

## Variants swept (script `p2_calibration.py` + inline v2/v3 probes; outputs
`p2_calibration_v{1,2,3}_output.txt`)

| Variant | Eligible TAG entries | Match rule | ATFIAT ceiling (3 draws) | BIOPOWER floor (3) | CAPK floor (2 scorable) |
|---|---|---|---|---|---|
| v1 | all with ≥1 content token | coverage ≥ 0.5 | 0.29, 0.14, 0.33 | 1.00, 1.00, 1.00 | 0.60, 1.00 |
| v2 | ≥2 content tokens | coverage ≥ 0.5 AND ≥2 shared | 0.14, 0.14, 0.17 | 0.83, 0.50, 0.50 | 0.00, 0.50 |
| v3 | ≥3 content tokens | coverage ≥ 0.67 AND ≥2 shared | 0.00, 0.00, 0.00 | 0.17, 0.00, 0.00 | 0.00, 0.00 |

At NO variant does the ceiling exceed the floor; v1–v2 are INVERTED (floor above
ceiling), v3 collapses both to ~0. The strengthening axis was swept, not point-sampled
— the ordering never flips anywhere on it.

## Structural diagnosis (why no token form can work — both legs fail independently)

1. **Ceiling is low at every strictness:** AT Fiat's manifest readings are articulated
   in disciplinary-tradition vocabulary ("empirical social-movement history", "Deweyan
   pragmatist political theory") while its section headers are colloquial position
   labels ("nobody thinks a policy is getting passed"). The OQ-264 6/6 TAG match that
   motivated "expected high" was a JUDGED call (blinded, name-blind subject+stance) —
   token overlap does not recover it.
2. **Floor is high at permissive strictness:** arsenal headers are topic-saturated
   (`L---Health`, `2NC---Biopower`), and every kernel reading's commitment shares that
   topic vocabulary, so single-/double-token headers match for free.
   Token machinery would have to simultaneously raise (1) and lower (2); the two
   failures have opposite fixes on the same dial.

## Verdict (per the plan's pinned escalation clause)

**P2 is unbuildable as a mechanical gate on current instruments.** The discriminating
quantity exists at the judged level (OQ-264's blinded adjudication) but not at the
token level. Per plan: "If no strengthening opens a gap, P2 is unbuildable as a
mechanical gate — escalate at the C checkpoint rather than pinning a fake one."
ESCALATED: the C-checkpoint options are (a) P2 as a blinded-adjudication step per run
(buildable, not mechanical — OQ-264 machinery reusable), (b) promotion rule reduced to
P1 + draw-level language, (c) another operator-specified form. No P2 threshold is
pinned; Part C run 1 must not start with a fake gate.

**RULED (operator, 2026-08-06): option (b) — P1-only promotion, draw-level language.**
The sweep did not fail to find a threshold; it found the construct is not
token-recoverable (ceiling below floor across all variants — an inverted
discriminator; opposite-direction errors on one dial are not fixable by dial-turning).
Judged-step P2 (option a) REJECTED: reintroducing a judgment gate mid-stage is the
post-hoc-licence shape the mechanical rule was written to prevent, and OQ-264's
machinery would need its own blinding and calibration to be trustworthy — a project,
not a gate. Standing note on this escalation: P2-style checks, if ever revived, apply
only to P1-passing draws (the Cap K r2 conditioning finding below).

**Two-sided calibration earned its keep in the falsifying direction:** a one-sided
(ceiling-only) calibration would have measured 0.14–0.33 on AT Fiat, pinned a low
threshold, and the gate would then FIRE ON ARSENALS (floor 0.50–1.00 at that
strictness) — a false meta-layer promotion channel, caught before any pin.

## Side finding

`capitalism_kritik_ndi2026_20260805_145128.manifest.json` carries an EMPTY
`commitment_system_recognition` (no readings) — a draw on which P1 (kernel minted)
fails before P2 is evaluated. P2's prereg form must state its conditioning: P2 is
evaluated only on P1-passing draws; a no-kernel draw is a P1 outcome, not a P2 zero.
