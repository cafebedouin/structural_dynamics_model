% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_preservation__mourning_practice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_preservation__mourning_practice_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: catastrophe_memory_preservation__mourning_practice_reading
 *   human_readable: Catastrophe Mourning Rite — Symbolic Continuity Reading
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   A coastal town lost a third of its population to a single storm-surge
 *   flood three generations ago. Every year on the anniversary it holds a
 *   rite: a procession to the sea wall, the reading of the memorial roll, a
 *   minute of silence at the hour of the breach, a shared meal.
 *   Quasi-liturgical in form — the roll is kept in the chapel, the litany has
 *   a fixed order, new names are added by the keeper elders — the rite is
 *   nonetheless voluntary: no authority compels attendance, and lapsed
 *   households lose nothing material. This story instantiates the
 *   mourning_practice_reading of the catastrophe_memory_preservation kernel:
 *   the rite preserves symbolic continuity and collective identity and
 *   transmits no operational survival competence — the litany contains names
 *   and dates, not evacuation routes or building standards. The claimed type
 *   (rope) and the metrics below are authored independently: the claim states
 *   what I believe structurally true of this reading; the metrics state what
 *   I believe descriptively true of the rite's operation. Sibling readings
 *   are separate constraint files linked through network.affects_constraints;
 *   the contest between readings is carried in the omegas, not inside this
 *   constraint.
 *
 * KEY AGENTS:
 *   - - commemorating_community_members: primary beneficiary (organized/mobile) — sustain the rite and receive identity cohesion and a grief-container
 *   - - memory_keeper_elders: beneficiary + agenda_setter (organized/identity_locked) — keep the memorial roll and litany; cannot leave the role without losing their place in the town's memory-work
 *   - - younger_generation_participants: secondary beneficiary (moderate/mobile) — inherit the rite as a membership marker; free to drift away
 *   - - grief_dissonant_survivors: excluded (moderate/constrained) — bereaved whose loss does not fit the communal script; outside the liturgy-planning circle
 *   - - local_historians_archivists: analytical observer (moderate/analytical) — maintain the documentary record; neither collect nor pay
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_preservation__mourning_practice_reading, 0.26).
domain_priors:suppression_score(catastrophe_memory_preservation__mourning_practice_reading, 0.15).
domain_priors:theater_ratio(catastrophe_memory_preservation__mourning_practice_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, extractiveness, 0.26).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, accessibility_collapse, 0.22).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, resistance, 0.18).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_preservation__mourning_practice_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_preservation__mourning_practice_reading, "Catastrophe Mourning Rite — Symbolic Continuity Reading").
narrative_ontology:topic_domain(catastrophe_memory_preservation__mourning_practice_reading, "religious_studies/collective_memory/ritual_practice").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_preservation__mourning_practice_reading, 'c486c636-e14d-4b65-9dd6-dc5ee7388e4e').
narrative_ontology:cs_kernel_codification('c486c636-e14d-4b65-9dd6-dc5ee7388e4e', distributed).
narrative_ontology:cs_authority_grounding('c486c636-e14d-4b65-9dd6-dc5ee7388e4e', practice).
narrative_ontology:cs_interpretation_layer_present('c486c636-e14d-4b65-9dd6-dc5ee7388e4e').
narrative_ontology:cs_reading_relation('c486c636-e14d-4b65-9dd6-dc5ee7388e4e', catastrophe_memory_preservation__survival_competence_reading, forecloses).
narrative_ontology:cs_reading_relation('c486c636-e14d-4b65-9dd6-dc5ee7388e4e', catastrophe_memory_preservation__hybrid_atrophy_reading, influences).
narrative_ontology:cs_axiom('c486c636-e14d-4b65-9dd6-dc5ee7388e4e', foundational, ritual_transfers_no_operational_capacity).
narrative_ontology:cs_axiom_status(ritual_transfers_no_operational_capacity, holdable).
narrative_ontology:cs_axiom_grounding('c486c636-e14d-4b65-9dd6-dc5ee7388e4e', ritual_transfers_no_operational_capacity, empirically_contingent).
narrative_ontology:cs_axiom('c486c636-e14d-4b65-9dd6-dc5ee7388e4e', foundational, symbolic_function_is_complete_not_residual).
narrative_ontology:cs_axiom_status(symbolic_function_is_complete_not_residual, holdable).
narrative_ontology:cs_axiom_grounding('c486c636-e14d-4b65-9dd6-dc5ee7388e4e', symbolic_function_is_complete_not_residual, conventional).
narrative_ontology:cs_reference_frame('c486c636-e14d-4b65-9dd6-dc5ee7388e4e', symbolic_commemorative_continuity).
narrative_ontology:cs_drift_state('c486c636-e14d-4b65-9dd6-dc5ee7388e4e', post_witness_generation_present, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('c486c636-e14d-4b65-9dd6-dc5ee7388e4e', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_preservation__mourning_practice_reading, catastrophe_memory_preservation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__mourning_practice_reading, commemorating_community_members).
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__mourning_practice_reading, memory_keeper_elders).
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__mourning_practice_reading, younger_generation_participants).
narrative_ontology:constraint_vindicates(catastrophe_memory_preservation__mourning_practice_reading, symbolic_memory_sufficiency_thesis).
narrative_ontology:constraint_vindicates(catastrophe_memory_preservation__mourning_practice_reading, ritual_identity_constitution_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Attend the annual procession to the sea wall, recite the names on the memorial roll, and keep the hour of silence at the time of the breach. The rite gives them a shared calendar, a bounded container for grief, and a visible mark of belonging; households that stay for the meal afterward are recognizably part of the town's continuity. Attendance is voluntary — a household that stops coming is noticed for a season and then simply counted among the absent. What the rite asks of them is an afternoon a year, modest dues for the roll's upkeep, and the discomfort of renewed grief.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, commemorating_community_members, beneficiary,
    organized, generational, mobile, regional).

% Keep the memorial roll, teach the litany to successors, decide which new names enter it, and adjust the order of the rite when the community asks. They serve in rotation and take no salary; the role confers standing and custodianship rather than income. Most have kept the roll for decades and describe the work as who they are; handing the roll on is the only way out of the role, and several delayed it until illness forced the question.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, memory_keeper_elders, beneficiary,
    organized, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_preservation__mourning_practice_reading, memory_keeper_elders, agenda_setter).

% Learn the litany in school and at the rite itself, and treat attendance as a marker of belonging rather than a duty. Some take the roll seriously and volunteer as readers; others come rarely and lose nothing material by staying away. What they inherit is a story and a date, not instructions for surviving the water.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, younger_generation_participants, beneficiary,
    moderate, biographical, mobile, regional).

% Lost kin in ways the communal script does not fit — some blame the town's leaders for the breached wall, some cannot bear the public recitation of names. They live inside the community and at its edges: present at the margins of the procession, absent from the committee that plans it. Their objection — that the rite curates grief into a tidy civic form — has been raised at town meetings and set aside each time.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, grief_dissonant_survivors, excluded,
    moderate, biographical, constrained, regional).

% Maintain the flood's documentary record — photographs, testimony, hydrology reports — outside the rite's frame. They attend as observers, sometimes advise the keepers on names and dates, and describe the rite as one memory-form among several rather than the town's memory itself. They collect nothing from the rite and bear none of its burdens.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, local_historians_archivists, observer,
    moderate, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_preservation__mourning_practice_reading, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_memory_preservation__mourning_practice_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of carrying a shared catastrophic past across the death of the witness generation: the rite synchronizes the community's grief on one date, marks membership in the continuity that survived the event, and keeps the catastrophe in common salience without requiring anyone to learn or teach survival procedure.
% TRANSFER_FUNCTION: Moves an afternoon of time, modest upkeep dues, and renewed emotional exposure from participating households into a shared symbolic repertoire — the roll, the litany, the procession, the meal. Recognition flows to the keeper elders for their custodial work; identity continuity flows from dying generations to children learning the names. No seat converts the flow into material gain.
% ABSENT_VOICES: Grief-dissonant survivors would object that the rite curates their loss into civic tidiness; they raise this at town meetings and are set aside. Lapsed younger members who found the form rote are never asked why they stopped. Local historians would weight the documentary record above liturgical memory but are consulted only on names and dates.
% DISAPPEARANCE_RATIONALE: Without the rite, the flood would pass unmarked within a generation or two: the memorial roll would stop being read aloud, the keeper role would dissolve, newcomer households would lose their one visible incorporation into the town's continuity, and memory of the event would retreat into the county archive and private families. Households would not flood anew and nothing operational would be lost — but the community's shared reference to its own founding wound would reorganize around documentary and familial memory.
% FOUNDING_PROBLEM: Founded in the years after the flood, when the dead were newly buried and the survivors needed a shared form for grief that private mourning could not carry, and a way to bind the diminished town into one continuity that the event had threatened to end.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: regional memory-studies researchers cite the town as a functioning case of post-disaster commemorative continuity; disaster-grief clinicians attest that dated communal mourning measurably contains bereavement in comparable towns; the county archive's accession logs show the documentary record thinning precisely where the rite is strongest. No attesting source sits inside the congregation's benefiting circle.
narrative_ontology:disappearance_verdict(catastrophe_memory_preservation__mourning_practice_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_preservation__mourning_practice_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_preservation__mourning_practice_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_preservation__mourning_practice_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_preservation__mourning_practice_reading, 0.26, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_preservation__mourning_practice_reading_tests).
:- end_tests(catastrophe_memory_preservation__mourning_practice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The scores describe the rite as it stands at interval end. Extractiveness 0.26: the arrangement takes an afternoon, modest dues, and renewed grief-exposure from participants and returns identity, a grief-container, and membership marking; the net is modestly favorable to participants, with the residue accounted as coordination overhead. Suppression 0.15 is authored as a raw structural property and is deliberately NOT scaled by anything — there is no enforcement machinery, only soft expectation, and only extractiveness gets scaled downstream. Theater_ratio 0.12 requires care: nearly all of the rite's activity is symbolic performance, but under this reading the performance IS the function — theater_ratio tracks proxy substitution (activity continued after its function died), and the rite's function is alive, so the ratio stays low despite the rite's overt theatricality. Accessibility_collapse 0.22: alternatives (private mourning, the county archive, grief counseling, secular memorial) remain fully workable; understanding the rite collapses none of them. Resistance 0.18: recurring but unorganized dissent from grief-dissonant survivors and rote-weary youth. Receipt surface: gain_flow is authored 'diffuse' affirmatively — I re-checked every seat; keeper elders accrue standing, not the flow's substance, and no seat converts participation into material gain. fixing_cost is 'cheap': ending the rite would cost the town nothing organizationally; it persists because it is valued, not because removal is expensive. All tracked series run on one shared grid (decades t=0..60) so no metric borrows another's end-state; suppression_requirement is deliberately not tracked because the enforcement picture is static-low and already captured by the scalar.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently. Keeper elders (identity_locked) sit nearest the beneficiary pole: the rite constitutes their standing, and leaving would cost them themselves, so the arrangement reads from their seat as pure continuity. Their fusion is relational-institutional — self-concept constituted through custodianship of the roll; if the frame broke (say the town voted to end the rite), their seat would flip from beneficiary toward a trapped bearer of meaning-loss, and the per-seat classification would move accordingly. Younger participants (mobile) hold a nearly free option: they can take the identity marker or leave it, so even the modest costs register as discretionary. Grief-dissonant survivors experience the same rite as a curated form that edits their grief — a mild imposition they cannot answer from inside the planning circle. The engine computes these divergences from role, power, and exit data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: commemorating members, keeper elders, and younger participants all sit near the beneficiary pole (low d), with the elders' identity lock deepening rather than reversing their position. No victim group is declared because none bears enforced cost — the excluded survivors bear dissonance, not extraction, and are left undeclared rather than forced into a victim role they do not occupy. Regional scope keeps verification cheap, so scope amplifies little. The observer seat is analytical and feeds no directionality. No directionality overrides are used: the derivation from declared beneficiaries plus exit options already places every seat correctly.
 *
 * MANDATROPHY ANALYSIS:
 *   The rope claim disciplines three misreadings. Against snare: with no victim set, no suppressed exits, and no enforcement machinery, the extraction-with-cover story finds no purchase — the soft pressure on non-attenders is real but is carried as an omega (concealed_conformity_costs) rather than asserted as fact. Against piton: the low theater_ratio is honest here precisely because symbolic performance is the live function; the piton risk is real but prospective, and it is routed through the elder-fusion omega rather than baked into current metrics. Against mountain: the rite is plainly constructed and maintained — emerges_naturally is false — and its persistence is explained by participant value, not naturality. Mandatrophy is not declared: the founding problem (carrying the catastrophe across the death of the witness generation) is still live, corroborated externally, and the founding-status/disappearance pairing (live/world_rearranges) raises no mismatch flag.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is the mourning_practice_reading of kernel catastrophe_memory_preservation; would instantiating survival_competence_reading or hybrid_atrophy_reading instead change the structural facts (epsilon, beneficiary and victim sets, type)?',
    'Comparative cognitive-ethnographic testing: measure whether ritual participants retain operational hazard knowledge (drill recall, threat recognition) versus matched non-participants; archive analysis of whether liturgical content ever encoded procedural survival information.',
    'If the survival reading holds, epsilon drops further (a hidden operational dividend) and the beneficiary set widens to future potential victims; if the hybrid reading holds, theater_ratio and the mandatrophy trajectory rise (residual performance around a lost function) and the type drifts toward piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, empirical, 'Committer structure: sibling readings of the catastrophe-memory kernel would re-author epsilon, victim sets, and type.').

omega_variable(
    concealed_conformity_costs,
    'Is participation genuinely opt-in, or do soft sanctions (marked non-attendance, kin disappointment) create a diffuse victim set among marginal members?',
    'Interview lapsed and reluctant members; measure status and network differentials between attending and non-attending households across a decade.',
    'A real sanction gradient would add a victim set, raise effective extraction for marginal members, and move the boundary toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(concealed_conformity_costs, empirical, 'Whether soft social sanction manufactures a hidden victim set behind the opt-in surface.').

omega_variable(
    embedded_procedural_fragments,
    'Does the liturgy embed procedural fragments (site references, kinship obligations, seasonal timing) that operate as survival-relevant knowledge without being framed as instruction?',
    'Content-analyze rite texts against the historical hazard response; test whether participants outperform non-participants on scenario recall tied to liturgical content.',
    'Partial embedded transfer would concede ground to the survival_competence sibling without changing this reading''s epsilon, but would alter the vindicated propositions and the network edge to the sibling.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(embedded_procedural_fragments, empirical, 'Whether symbolic content carries an incidental operational payload.').

omega_variable(
    elder_identity_fusion_persistence,
    'Does the practice persist because participants value it, or because identity-locked elders cannot exit and keep administering it by default?',
    'Succession studies: observe transitions when keeper elders die or step down; measure whether rites lapse, adapt, or continue under new stewards.',
    'Elder-driven persistence would mean theater_ratio understates inertial drift and the arrangement trends toward maintained-by-default rather than valued — raising long-run piton risk despite currently low theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elder_identity_fusion_persistence, empirical, 'Whether persistence rests on participant value or on elder identity lock.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_preservation__mourning_practice_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 0, 0.06).
narrative_ontology:measurement_basis(cata_tr_t0, observed).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 10, 0.07).
narrative_ontology:measurement_basis(cata_tr_t10, observed).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 20, 0.08).
narrative_ontology:measurement_basis(cata_tr_t20, observed).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 30, 0.09).
narrative_ontology:measurement_basis(cata_tr_t30, observed).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement_basis(cata_tr_t40, observed).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 50, 0.11).
narrative_ontology:measurement_basis(cata_tr_t50, observed).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 60, 0.12).
narrative_ontology:measurement_basis(cata_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(cata_be_t0, observed).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 10, 0.2).
narrative_ontology:measurement_basis(cata_be_t10, observed).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 20, 0.21).
narrative_ontology:measurement_basis(cata_be_t20, observed).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 30, 0.23).
narrative_ontology:measurement_basis(cata_be_t30, observed).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 40, 0.24).
narrative_ontology:measurement_basis(cata_be_t40, observed).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 50, 0.25).
narrative_ontology:measurement_basis(cata_be_t50, observed).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 60, 0.26).
narrative_ontology:measurement_basis(cata_be_t60, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(catastrophe_memory_preservation__mourning_practice_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_preservation__mourning_practice_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__mourning_practice_reading, catastrophe_memory_preservation__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__mourning_practice_reading, catastrophe_memory_preservation__hybrid_atrophy_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'ritual preserves catastrophe memory' covers three structurally distinct claims: that the rite transmits operational threat-recognition (survival_competence_reading), that it transmits only symbolic continuity and identity (this file), and that it once transmitted competence and has decayed into pure mourning (hybrid_atrophy_reading). The readings assign different epsilon to the same practices because the referent arrangement differs — a competence-transmission institution, a purely symbolic one, and a residual one respectively. This file authors the symbolic-only reading with its own stable epsilon; the family is linked through affects_constraints, with the survival claim upstream (where true, it grounds the other two) and the atrophy narrative downstream of this reading's dissolution of the loss-referent.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
