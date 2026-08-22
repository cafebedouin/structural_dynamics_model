% ============================================================================
% CONSTRAINT STORY: second_amendment_scope__collective_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [SUPERSEDED]
% ============================================================================

:- module(constraint_second_amendment_scope__collective_right_reading, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: second_amendment_scope__collective_right_reading
 *   human_readable: Second Amendment Collective-Right Reading (State Militia Protection)
 *   domain: constitutional_law/political_theory
 *
 * SUMMARY:
 *   From United States v. Miller (1939) until District of Columbia v. Heller
 *   (2008), federal courts read the Second Amendment as securing state
 *   authority to maintain militias — a federalism guarantee binding only the
 *   federal government's power over state military institutions — and
 *   conferring no individual entitlement to keep and bear arms. Under this
 *   reading the arrangement's operative content was twofold: protection of
 *   state militia institutions against federal abolition, and, increasingly
 *   as the protected object atrophied, denial of constitutional standing to
 *   individual ownership claims, which left firearms regulation to the
 *   unencumbered play of ordinary legislation. This story authors that
 *   standing arrangement as ONE reading of the contested
 *   second_amendment_scope kernel: epsilon is assessed for the arrangement
 *   this reading describes, by its own lights — not for the individual-right
 *   arrangement its rivals would install. The claim/metric gap is deliberate:
 *   the reading is CLAIMED as rope (a genuine, low-extraction federalism
 *   coordination mechanism) while the authored metrics trace its late-life
 *   drift — rising theater as the protected object vanished into the National
 *   Guard system, rising enforcement effort as individual-rights challenges
 *   mounted. The engine measures that divergence; this commentary does not
 *   reconcile it. KEY AGENTS (by structural relationship): - federal_courts:
 *   Agenda setter (institutional/constrained) — administers the reading,
 *   sustains it against challenge, holds unilateral revisiting power it
 *   declined for sixty-nine years - state_governments: Primary beneficiary
 *   (institutional/constrained) — collect constitutional immunity from
 *   federal dissolution of their military institutions -
 *   organized_state_militias: Protected object and beneficiary
 *   (organized/constrained) — the institutions the guarantee names;
 *   progressively federalized beneath the shield -
 *   firearms_regulatory_authorities: Practical beneficiary
 *   (institutional/mobile) — legislate and regulate without an
 *   enumerated-rights veto; collect the reading's operative fruit -
 *   federal_government: Paying party (institutional/trapped) — cedes one
 *   power as consensual federation price; dual-positioned, not harvested -
 *   individual_firearms_owners: Excluded claimants (organized/trapped) —
 *   assigned no standing by the reading; their organized challenge ultimately
 *   displaced it - constitutional_scholars: Analytical observers
 *   (analytical/analytical) — produced the revisionist scholarship that
 *   reframed the dispute
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_scope__collective_right_reading, 0.28).
domain_priors:suppression_score(second_amendment_scope__collective_right_reading, 0.58).
domain_priors:theater_ratio(second_amendment_scope__collective_right_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_scope__collective_right_reading, rope).
narrative_ontology:human_readable(second_amendment_scope__collective_right_reading, "Second Amendment Collective-Right Reading (State Militia Protection)").
narrative_ontology:topic_domain(second_amendment_scope__collective_right_reading, "constitutional_law/political_theory").

domain_priors:requires_active_enforcement(second_amendment_scope__collective_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_scope__collective_right_reading, '9ddb55d1-3d5c-492d-b5db-56e3bfea69d0').
narrative_ontology:cs_kernel_codification('9ddb55d1-3d5c-492d-b5db-56e3bfea69d0', fixed_text).
narrative_ontology:cs_authority_grounding('9ddb55d1-3d5c-492d-b5db-56e3bfea69d0', lineage).
narrative_ontology:cs_interpretation_layer_present('9ddb55d1-3d5c-492d-b5db-56e3bfea69d0').
narrative_ontology:cs_reading_relation('9ddb55d1-3d5c-492d-b5db-56e3bfea69d0', second_amendment_scope__individual_right_reading, forecloses).
narrative_ontology:cs_reading_relation('9ddb55d1-3d5c-492d-b5db-56e3bfea69d0', second_amendment_scope__civic_right_reading, forecloses).
narrative_ontology:cs_axiom('9ddb55d1-3d5c-492d-b5db-56e3bfea69d0', foundational, amendment_confers_no_individual_entitlement).
narrative_ontology:cs_axiom_status(amendment_confers_no_individual_entitlement, holdable).
narrative_ontology:cs_axiom_grounding('9ddb55d1-3d5c-492d-b5db-56e3bfea69d0', amendment_confers_no_individual_entitlement, conventional).
narrative_ontology:cs_axiom('9ddb55d1-3d5c-492d-b5db-56e3bfea69d0', foundational, prefatory_clause_frames_operative_scope).
narrative_ontology:cs_axiom_status(prefatory_clause_frames_operative_scope, holdable).
narrative_ontology:cs_axiom_grounding('9ddb55d1-3d5c-492d-b5db-56e3bfea69d0', prefatory_clause_frames_operative_scope, conventional).
narrative_ontology:cs_axiom('9ddb55d1-3d5c-492d-b5db-56e3bfea69d0', secondary, state_military_institutions_constitutionally_secured).
narrative_ontology:cs_axiom_status(state_military_institutions_constitutionally_secured, holdable).
narrative_ontology:cs_axiom_grounding('9ddb55d1-3d5c-492d-b5db-56e3bfea69d0', state_military_institutions_constitutionally_secured, conventional).
narrative_ontology:cs_reference_frame('9ddb55d1-3d5c-492d-b5db-56e3bfea69d0', state_militia_federalism_guarantee).
narrative_ontology:cs_drift_state('9ddb55d1-3d5c-492d-b5db-56e3bfea69d0', post_heller_jurisprudence, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('9ddb55d1-3d5c-492d-b5db-56e3bfea69d0', '2026-08-04T00:00:00Z').
narrative_ontology:cs_kernel_id(second_amendment_scope__collective_right_reading, second_amendment_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_scope__collective_right_reading, state_governments).
narrative_ontology:constraint_beneficiary(second_amendment_scope__collective_right_reading, organized_state_militias).
narrative_ontology:constraint_beneficiary(second_amendment_scope__collective_right_reading, firearms_regulatory_authorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(second_amendment_scope__collective_right_reading, federal_government).
narrative_ontology:constraint_victim(second_amendment_scope__collective_right_reading, federal_government).
narrative_ontology:constraint_vindicates(second_amendment_scope__collective_right_reading, federalism_dual_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(second_amendment_scope__collective_right_reading, prefatory_clause_primacy_method).
narrative_ontology:constraint_vindicates(second_amendment_scope__collective_right_reading, police_powers_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the amendment's meaning: from United States v. Miller (1939) forward, sustain the collective reading and treat individual-rights claims as non-cognizable. Hold unilateral power to revisit the reading at any time; declined for sixty-nine years while challenge volume stayed manageable, then exercised it in District of Columbia v. Heller (2008). Bear almost none of the arrangement's costs themselves.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, federal_courts, agenda_setter,
    institutional, generational, constrained, national).

% Fifty sovereign states party to the federal bargain; the reading guarantees their military institutions cannot be dissolved or disarmed by federal act. They collect that immunity without administering the arrangement — the courts do that. Exit from the constitutional order is unavailable; their leverage runs through Congress and the courts, not withdrawal.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, state_governments, beneficiary,
    institutional, generational, constrained, national).

% The state military institutions the amendment names — adjutant-general establishments, Guard formations, state defense forces. The reading shields their existence from federal abolition. In practice the Militia Acts of 1903-1916 and dual enlistment folded them into federal funding and command, so by mid-century the shield protected an institution already substantially integrated into federal control.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, organized_state_militias, beneficiary,
    organized, generational, constrained, national).

% State legislatures, municipal governments, and federal agencies writing firearms regulation. Because the reading confers no individual constitutional right, their statutes face no enumerated-rights veto; they collect the practical fruit of the reading every time a regulation survives challenge. Their discretion is theirs to expand or decline — the freest seat in the arrangement.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, firearms_regulatory_authorities, beneficiary,
    institutional, biographical, mobile, national).

% Cedes one power — dissolving or disarming state military institutions — as the consensual price of the union whose stability returns legitimacy to the same seat. Bears the arrangement's cost but is not harvested by it: the cession was ratified bargain terms, not a taking. No exit from the constitutional order exists; the seat's leverage is amendment, not withdrawal.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, federal_government, payer,
    institutional, generational, trapped, continental).
narrative_ontology:stakeholder_secondary_role(second_amendment_scope__collective_right_reading, federal_government, beneficiary).

% Tens of millions of Americans keeping handguns and long guns for defense, hunting, and sport. The reading assigns them no constitutional standing: their protection runs entirely through ordinary legislation they may lobby over but cannot invoke. They cannot exit the jurisdiction whose reading rules them, and their organized challenge — litigation, scholarship, electoral pressure — is the resistance that ultimately displaced the arrangement.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, individual_firearms_owners, excluded,
    organized, biographical, trapped, national).

% Debate the amendment's original meaning and the soundness of subordinating the operative clause to the militia preamble. Produced the revisionist scholarship that reframed the dispute and supplied the intellectual groundwork for the displacement campaign. Neither collect nor pay; their stake is interpretive.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, constitutional_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_scope__collective_right_reading, state_governments).
narrative_ontology:fixing_cost_class(second_amendment_scope__collective_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves dual sovereignty over organized force: guarantees that the constituent states retain military institutions the national government cannot dissolve, anchoring the federal bargain's internal balance. Stated without evaluation: whatever else the arrangement does, this is the coordination problem its text addresses.
% TRANSFER_FUNCTION: Moves constitutional immunity from federal interference to state military institutions; and, derivatively, moves regulatory discretion to legislatures and agencies by withholding constitutional standing from individual ownership claims. The first transfer runs from the federal government to the states; the second is a withholding rather than a taking — its incidence falls on would-be rights-claimants as forgone protection.
% ABSENT_VOICES: Individual firearms owners — tens of millions of them — would object that the amendment's text secures persons, not states; under this reading they had no courtroom in which that objection was cognizable. Also absent: the founding generation's militia tradition itself, whose bearers are two centuries gone and whose institutional form no longer exists to speak. Both absences are constitutive: unanimity in the case law held partly because the excluded claimants' position was ruled uncognizable rather than answered.
% DISAPPEARANCE_RATIONALE: It did rearrange, historically: when the reading fell in 2008, individual-rights claims became cognizable overnight, longstanding regulations drew constitutional challenge, and the interpretive center of gravity moved to the individual-right arrangement. Had it vanished without a successor, the same rearrangement would have run through ordinary jurisprudence — the arrangement's departure is precisely the event the interval's endpoint records.
% FOUNDING_PROBLEM: Ratification-era states feared a federal standing army would render their militias obsolete and leave them defenseless against federal consolidation; the amendment was written to guarantee that state military institutions could not be abolished or disarmed by national act.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: the federal judiciary itself (Perpich v. Department of Defense, 1990, confirming plenary federal control over the Guard), the statutory record (Militia Acts of 1903-1916), and constitutional historians across the interpretive spectrum — including scholars sympathetic to the collective reading — all attest that the institution the guarantee protected no longer exists in its founding form. No living party fears federal militia-abolition; the fear survives only as historical description.
narrative_ontology:disappearance_verdict(second_amendment_scope__collective_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_scope__collective_right_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_scope__collective_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(second_amendment_scope__collective_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_scope__collective_right_reading, 0.28, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_scope__collective_right_reading_tests).
:- end_tests(second_amendment_scope__collective_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is low and slow-moving (0.15 to 0.28 across the interval): the arrangement binds one actor — the federal government — in one narrow respect, as consensual bargain terms, and no seat is harvested. The gentle accumulation is rent-layering, not predation: each decade of firearms regulation built atop the reading's permission, and maintaining the doctrine against mounting claims cost the enforcing courts more each cycle. Suppression (0.58 at interval end) tracks enforcement intensity, not breadth: early consensus needed almost no enforcement (0.10 in 1939); by the 1990s the reading survived only through active judicial rejection of a maturing challenge apparatus. Theater rises from 0.15 to 0.48 as the protected object — the independent state militia — was absorbed into the federally controlled National Guard, leaving the reading's militia-protective content increasingly citational while its operative content shifted to regulatory permission. Accessibility collapse (0.65) reflects a judicially closed alternative that stayed alive in scholarship and politics; resistance (0.60) reflects the organized, ultimately successful displacement campaign. All three series share one six-point grid (1939, 1950, 1965, 1980, 1994, 2008) so no metric is sampled against another's end-state. The suppression_requirement series is authored deliberately: this story's dynamic IS enforcement intensification culminating in displacement.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical text. From the federal seat the arrangement is self-binding it consented to — trapped exit does not amplify extraction because the ceded power was the price of admission, not a taking. From the state and militia seats it is a shield. From the regulatory seat it is latitude. From the excluded individual seat the same words are a closed door: the amendment's protection is real, institutionally allocated, and unreachable. The excluded seat's experience is the sharpest divergence in the story — denial of standing is not extraction, but it is experienced as cost by those outside the coverage line, and their resistance is what the enforcement series registers. The engine computes per-seat classifications from the structural data; this commentary predicts the divergence without adjudicating it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: state_governments and organized_state_militias sit near the beneficiary pole (d roughly 0.05-0.15) — the arrangement subsidizes them with immunity. firearms_regulatory_authorities derive low-to-moderate d (roughly 0.15-0.25): they benefit from the arrangement's operation without being its object. The federal seat is dual-positioned (payer with beneficiary secondary): it bears the cession but receives the federation's stability, landing mid-scale (roughly 0.4) — and critically, its trapped exit should NOT be read as target-amplification, because the constraint's cost to it was consensual bargain terms. individual_firearms_owners contribute no chi: they are excluded from coverage, not governed by the arrangement — exclusion routes through the absent-voices and resistance channels, not the extraction arithmetic. National scope applies modest verification-difficulty amplification to the small base. Gain flow: the arrangement's transfer — federal non-interference — accrues demonstrably to the state seat, so gain_flow names state_governments; the regulatory seat's windfall is benefit-from-operation, not receipt of the transfer. Fixing cost is prohibitive: the seat that could fix the arrangement (the courts) bore almost none of its costs, so revision waited for an externally forced occasion — sixty-nine years.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — state militias menaced by a federal standing army — died mid-interval: the Militia Acts (1903-1916) and dual enlistment folded the militias into federal control, and Perpich v. DoD (1990) confirmed plenary federal authority over the Guard. The reading nonetheless persisted to 2008, its operative content migrated from militia protection to regulatory permission. Classification discipline keeps this from being mislabeled in either direction: calling it pure rope ignores the dead mandate and the rising theater (the zombie signature the R5 mismatch consumer flags — founding status dead crossed with a world_rearranges verdict); calling it a snare would require victims and extraction the structure lacks — no seat is harvested, individuals are excluded rather than bled. The honest reading is a rope whose mandate rotted under it, sustained by prohibitive fixing costs until a forced displacement. The theater series is the symptom trail; the cost-asymmetry (the courts could revise at tolerable cost to themselves but gained nothing by doing so) is why it lingered.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the second_amendment_scope kernel; what would the sibling readings (individual_right_reading, civic_right_reading) change structurally, and where exactly is the disagreement located?',
    'Comparative authoring of the sibling stories: the individual reading moves individuals into the beneficiary set and regulators into the constrained set with materially higher stakes; the civic reading conditions individual entitlement on militia participation, splitting the difference. The disagreement is located in whether the operative clause''s ''the people'' vest entitlement in private persons or in state institutions.',
    'Classification is reading-indexed: under the individual reading the same text computes as a regulator-constraining arrangement with different beneficiaries; under the civic reading as a conditional hybrid. No single epsilon spans the readings — each sibling file authors its own.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one kernel, three readings; disagreement located in the vesting of ''the people''s'' entitlement.').

omega_variable(
    protected_object_vestige_timing,
    'Did the independent state militia — the reading''s protected object — still meaningfully exist when the reading became governing doctrine (Miller, 1939), or had the Dick Act framework already absorbed it?',
    'Institutional history: Guard federalization milestones (1903, 1908, 1916, 1933 dual enlistment), the vitality of state defense forces, and Perpich v. DoD (1990).',
    'If the object was already vestigial at t0, the reading''s coordination function was partly ceremonial from the start, raising early theater and strengthening piton-drift readings of the trajectory; if alive, the theater rise dates from mid-century as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(protected_object_vestige_timing, empirical, 'Timing of the protected object''s atrophy relative to the reading''s dominance.').

omega_variable(
    displacement_mechanism,
    'Did the reading fall because its enforcement coalition eroded (courts losing the will to sustain it) or because an organized counter-coalition forced displacement (the post-1977 litigation strategy plus reframed scholarship)?',
    'Litigation-history analysis: challenge volume and quality 1977-2008, the strategic reorientation of gun-rights organizations after 1977, and the uptake of revisionist scholarship in judicial opinions.',
    'Coalition-forced displacement models as rope replacement; enforcement erosion would model as piton decay — the two carry different post-interval persistence predictions for the reading''s scholarly afterlife.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(displacement_mechanism, empirical, 'Whether the reading''s fall was attritional or forced.').

omega_variable(
    prefatory_clause_method_legitimacy,
    'Is subordinating the operative clause to the prefatory militia clause a sound interpretive convention grounded in the text''s structure, or a post-hoc rationalization that served regulatory interests?',
    'Drafting history and contemporaneous state-constitution analogues: whether preamble-framing was a live, general interpretive practice in 1789-1791 or ad hoc in this instance.',
    'If rationalization, the reading''s authority shifts from lineage toward interest-driven maintenance, raising effective suppression across its tenure and supporting an extraction-flavored reinterpretation of its enforcement series.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prefatory_clause_method_legitimacy, conceptual, 'Legitimacy of the interpretive method that distinguishes this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_scope__collective_right_reading, 1939, 2008).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1939, second_amendment_scope__collective_right_reading, theater_ratio, 1939, 0.15).
narrative_ontology:measurement_basis(seco_tr_t1939, observed).
narrative_ontology:measurement(seco_tr_t1950, second_amendment_scope__collective_right_reading, theater_ratio, 1950, 0.2).
narrative_ontology:measurement_basis(seco_tr_t1950, observed).
narrative_ontology:measurement(seco_tr_t1965, second_amendment_scope__collective_right_reading, theater_ratio, 1965, 0.26).
narrative_ontology:measurement_basis(seco_tr_t1965, observed).
narrative_ontology:measurement(seco_tr_t1980, second_amendment_scope__collective_right_reading, theater_ratio, 1980, 0.33).
narrative_ontology:measurement_basis(seco_tr_t1980, observed).
narrative_ontology:measurement(seco_tr_t1994, second_amendment_scope__collective_right_reading, theater_ratio, 1994, 0.41).
narrative_ontology:measurement_basis(seco_tr_t1994, observed).
narrative_ontology:measurement(seco_tr_t2008, second_amendment_scope__collective_right_reading, theater_ratio, 2008, 0.48).
narrative_ontology:measurement_basis(seco_tr_t2008, observed).

% Extraction over time
narrative_ontology:measurement(seco_be_t1939, second_amendment_scope__collective_right_reading, base_extractiveness, 1939, 0.15).
narrative_ontology:measurement_basis(seco_be_t1939, observed).
narrative_ontology:measurement(seco_be_t1950, second_amendment_scope__collective_right_reading, base_extractiveness, 1950, 0.17).
narrative_ontology:measurement_basis(seco_be_t1950, observed).
narrative_ontology:measurement(seco_be_t1965, second_amendment_scope__collective_right_reading, base_extractiveness, 1965, 0.2).
narrative_ontology:measurement_basis(seco_be_t1965, observed).
narrative_ontology:measurement(seco_be_t1980, second_amendment_scope__collective_right_reading, base_extractiveness, 1980, 0.23).
narrative_ontology:measurement_basis(seco_be_t1980, observed).
narrative_ontology:measurement(seco_be_t1994, second_amendment_scope__collective_right_reading, base_extractiveness, 1994, 0.26).
narrative_ontology:measurement_basis(seco_be_t1994, observed).
narrative_ontology:measurement(seco_be_t2008, second_amendment_scope__collective_right_reading, base_extractiveness, 2008, 0.28).
narrative_ontology:measurement_basis(seco_be_t2008, observed).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1939, second_amendment_scope__collective_right_reading, suppression_requirement, 1939, 0.1).
narrative_ontology:measurement_basis(seco_su_t1939, observed).
narrative_ontology:measurement(seco_su_t1950, second_amendment_scope__collective_right_reading, suppression_requirement, 1950, 0.14).
narrative_ontology:measurement_basis(seco_su_t1950, observed).
narrative_ontology:measurement(seco_su_t1965, second_amendment_scope__collective_right_reading, suppression_requirement, 1965, 0.22).
narrative_ontology:measurement_basis(seco_su_t1965, observed).
narrative_ontology:measurement(seco_su_t1980, second_amendment_scope__collective_right_reading, suppression_requirement, 1980, 0.34).
narrative_ontology:measurement_basis(seco_su_t1980, observed).
narrative_ontology:measurement(seco_su_t1994, second_amendment_scope__collective_right_reading, suppression_requirement, 1994, 0.47).
narrative_ontology:measurement_basis(seco_su_t1994, observed).
narrative_ontology:measurement(seco_su_t2008, second_amendment_scope__collective_right_reading, suppression_requirement, 2008, 0.58).
narrative_ontology:measurement_basis(seco_su_t2008, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_scope__collective_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_scope__collective_right_reading, second_amendment_scope__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_scope__collective_right_reading, second_amendment_scope__civic_right_reading).

% DUAL FORMULATION NOTE:
% Constraint family: one kernel (second_amendment_scope), three readings, three constraints. The colloquial label 'the Second Amendment' conflates structurally distinct arrangements: this collective reading (institutional beneficiaries, low epsilon, regulator latitude), the individual_right_reading (individual beneficiaries, regulator-constraining, materially higher stakes), and the civic_right_reading (entitlement conditioned on militia participation). Epsilon differs across the family because the arrangements differ, not because measurement varies: each story authors its own referent. Upstream/downstream: this reading's sixty-nine-year dominance structured the conditions under which the individual reading developed — as a challenger bearing the burden against settled doctrine — so this story influences its siblings' histories even though the readings are pairwise foreclosing within any single framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
