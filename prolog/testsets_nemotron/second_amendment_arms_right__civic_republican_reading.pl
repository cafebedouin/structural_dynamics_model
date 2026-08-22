% ============================================================================
% CONSTRAINT STORY: second_amendment_arms_right__civic_republican_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_arms_right__civic_republican_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: second_amendment_arms_right__civic_republican_reading
 *   human_readable: Second Amendment — Civic Republican Reading (Armed Citizenship as Republican Prerequisite)
 *   domain: constitutional_law/political_philosophy/legal_interpretation
 *
 * SUMMARY:
 *   The civic republican reading of the Second Amendment holds that the right
 *   protects armed citizenship as a prerequisite for republican
 *   self-governance — neither a purely individual liberty nor a
 *   state-centered militia power. The constraint coordinates citizen
 *   participation in organized militia structures (training, qualification,
 *   civic duty) while extracting compliance from those who would claim the
 *   right outside that civic frame. Beneficiaries are citizen-militia members
 *   (who gain both right and civic standing) and the republican polity (which
 *   gains a distributed defensive capacity rooted in civic virtue). Victims
 *   are unorganized nonparticipants who face regulatory burdens without the
 *   coordination payoff, and absolute individualist claimants whose
 *   libertarian frame is excluded by the civic participation norm. The
 *   constraint requires active enforcement (militia statutes, training
 *   requirements, qualification gates) and has seen rising extractiveness and
 *   theater as the founding militia function atrophied while the regulatory
 *   apparatus expanded.
 *
 * KEY AGENTS:
 *   - citizen_militia_members: Primary beneficiaries (dual right/duty holders) — organized, generational horizon, constrained exit (civic identity lock)
 *   - republican_polity: Institutional beneficiary — gains defensive capacity rooted in civic virtue, institutional power, civilizational horizon
 *   - local_governments: Secondary beneficiaries — administer militia organization, moderate power, generational horizon
 *   - unorganized_nonparticipants: Victims — bear regulatory costs without civic benefit, powerless to moderate, biographical horizon, constrained exit
 *   - absolute_individualist_claimants: Victims — their interpretive frame is excluded by the civic norm, moderate power, biographical horizon, mobile exit (can exit the civic frame)
 *   - federal_regulatory_authority: Agenda setter — enforces qualification/training gates, institutional power, generational horizon
 *   - state_militia_authorities: Secondary agenda setters — implement federal framework locally, institutional power, generational horizon
 *   - constitutional_observers: Observer seat — analytical, civilizational horizon, analytical exit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_arms_right__civic_republican_reading, 0.42).
domain_priors:suppression_score(second_amendment_arms_right__civic_republican_reading, 0.35).
domain_priors:theater_ratio(second_amendment_arms_right__civic_republican_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(second_amendment_arms_right__civic_republican_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_arms_right__civic_republican_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_arms_right__civic_republican_reading, "Second Amendment — Civic Republican Reading (Armed Citizenship as Republican Prerequisite)").
narrative_ontology:topic_domain(second_amendment_arms_right__civic_republican_reading, "constitutional_law/political_philosophy/legal_interpretation").

domain_priors:requires_active_enforcement(second_amendment_arms_right__civic_republican_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_arms_right__civic_republican_reading, 'b2737196-879d-4c32-b3f1-3625bd0caded').
narrative_ontology:cs_kernel_codification('b2737196-879d-4c32-b3f1-3625bd0caded', fixed_text).
narrative_ontology:cs_authority_grounding('b2737196-879d-4c32-b3f1-3625bd0caded', lineage).
narrative_ontology:cs_interpretation_layer_present('b2737196-879d-4c32-b3f1-3625bd0caded').
narrative_ontology:cs_reading_relation('b2737196-879d-4c32-b3f1-3625bd0caded', second_amendment_arms_right__individual_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('b2737196-879d-4c32-b3f1-3625bd0caded', second_amendment_arms_right__collective_right_reading, coexists_with).
narrative_ontology:cs_axiom('b2737196-879d-4c32-b3f1-3625bd0caded', foundational, armed_citizenship_as_republican_prerequisite).
narrative_ontology:cs_axiom_status(armed_citizenship_as_republican_prerequisite, holdable).
narrative_ontology:cs_axiom_grounding('b2737196-879d-4c32-b3f1-3625bd0caded', armed_citizenship_as_republican_prerequisite, conventional).
narrative_ontology:cs_axiom('b2737196-879d-4c32-b3f1-3625bd0caded', foundational, civic_participation_norm_constrains_regulatory_authority).
narrative_ontology:cs_axiom_status(civic_participation_norm_constrains_regulatory_authority, holdable).
narrative_ontology:cs_axiom_grounding('b2737196-879d-4c32-b3f1-3625bd0caded', civic_participation_norm_constrains_regulatory_authority, conventional).
narrative_ontology:cs_axiom('b2737196-879d-4c32-b3f1-3625bd0caded', secondary, militia_training_as_civic_duty_not_mere_regulation).
narrative_ontology:cs_axiom_status(militia_training_as_civic_duty_not_mere_regulation, holdable).
narrative_ontology:cs_axiom_grounding('b2737196-879d-4c32-b3f1-3625bd0caded', militia_training_as_civic_duty_not_mere_regulation, conventional).
narrative_ontology:cs_reference_frame('b2737196-879d-4c32-b3f1-3625bd0caded', founding_republican_armed_citizenship).
narrative_ontology:cs_drift_state('b2737196-879d-4c32-b3f1-3625bd0caded', post_heller_incorporation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b2737196-879d-4c32-b3f1-3625bd0caded', '2026-06-11T12:00:00Z').
narrative_ontology:cs_kernel_id(second_amendment_arms_right__civic_republican_reading, second_amendment_arms_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__civic_republican_reading, citizen_militia_members).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__civic_republican_reading, republican_polity).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__civic_republican_reading, local_governments).
narrative_ontology:constraint_victim(second_amendment_arms_right__civic_republican_reading, unorganized_nonparticipants).
narrative_ontology:constraint_victim(second_amendment_arms_right__civic_republican_reading, absolute_individualist_claimants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__civic_republican_reading, state_militia_authorities).
narrative_ontology:constraint_victim(second_amendment_arms_right__civic_republican_reading, citizen_militia_members).
narrative_ontology:constraint_vindicates(second_amendment_arms_right__civic_republican_reading, republican_self_governance_requires_armed_citizenry).
narrative_ontology:constraint_vindicates(second_amendment_arms_right__civic_republican_reading, civic_participation_norm_constrains_regulatory_authority).
narrative_ontology:constraint_vindicates(second_amendment_arms_right__civic_republican_reading, militia_training_as_civic_duty_not_mere_regulation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Organized participants in militia structures (National Guard, state defense forces, statutory militia, civic training programs). They hold the right to arms as a civic privilege tied to duty — training, qualification, and organized service. The civic identity fuses with the right: exit means surrendering the civic role, not just the gun. They gain coordination value (collective defense, civic standing) and bear the duty costs (time, discipline, subordination to civic authority).
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, citizen_militia_members, beneficiary,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_arms_right__civic_republican_reading, citizen_militia_members, payer).

% The constitutional order that depends on an armed citizenry for republican self-governance — as a check on standing armies, a cultivation of civic virtue, and a distributed defensive capacity. It benefits from the constraint's coordination function but does not directly administer it. Its 'exit' is analytical: the polity persists or transforms regardless of this specific constraint's fate.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, republican_polity, beneficiary,
    institutional, civilizational, analytical, national).

% Administer militia organization locally (training facilities, muster rolls, qualification programs). Gain institutional role and federal coordination resources. Constrained exit: they operate within the federal militia framework (Dick Act, Title 32) but retain some discretion in implementation.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, local_governments, beneficiary,
    moderate, generational, constrained, regional).

% Individuals who do not participate in organized militia structures but are subject to the regulatory regime (background checks, qualification gates, possession restrictions) justified by the civic participation norm. They bear extraction (compliance costs, denied access) without the coordination payoff (civic standing, collective defense role). Exit is constrained: they cannot easily escape the regulatory regime, but they also lack the identity lock of militia members.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, unorganized_nonparticipants, payer,
    powerless, biographical, constrained, national).

% Advocates and litigants who claim the Second Amendment as a purely individual liberty, pre-existing government, with no civic duty component. Their interpretive frame is excluded by the civic republican norm — they face regulatory burdens justified by a frame they reject. Exit is mobile: they can (and do) shift to the individual_right_reading constraint, which offers a different structural position with lower extraction for their seat.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, absolute_individualist_claimants, payer,
    moderate, biographical, mobile, national).

% Sets and enforces the qualification, training, and organizational gates (NFA, GCA, Brady Act, militia statutes). Constrained exit: the authority is bound by the civic republican frame it administers — it cannot arbitrarily expand extraction without undermining the coordination justification. It bears administrative costs and political risk.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, federal_regulatory_authority, agenda_setter,
    institutional, generational, constrained, national).

% Implement the federal militia framework at state level (National Guard, state defense forces, statutory unorganized militia). Dual position: they administer the constraint (agenda_setter) and gain institutional resources/standing (beneficiary). Constrained exit: embedded in the federal-state militia compact.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, state_militia_authorities, agenda_setter,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(second_amendment_arms_right__civic_republican_reading, state_militia_authorities, beneficiary).

% Scholars, courts, and analysts who evaluate the constraint's operation across seats. They neither collect nor pay; they observe the structural divergence between the civic republican reading and its siblings. Their exit is analytical: they can adopt any reading without material cost.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__civic_republican_reading, constitutional_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates armed citizenship as a republican practice: organizes citizens into militia structures (training, qualification, civic duty) that simultaneously provide distributed defensive capacity, cultivate civic virtue, and check standing army power — solving the republican problem of defense without militarism.
% TRANSFER_FUNCTION: Moves regulatory compliance costs (training time, qualification fees, organizational subordination) from citizen-militia members to the regulatory authority, while moving the right to arms and civic standing from the authority to the members. The unorganized nonparticipants bear compliance costs without receiving the civic standing transfer.
% ABSENT_VOICES: The unorganized nonparticipants (especially in urban areas with low militia participation) and absolute individualist claimants are structurally excluded from the civic republican frame — they would object to bearing regulatory costs justified by a civic duty they do not recognize or perform. They are present in the political system but absent from the civic republican reading's constituency.
% DISAPPEARANCE_RATIONALE: If the civic republican constraint vanished overnight, the regulatory apparatus justified by civic participation (militia-based qualification gates, training requirements, organized membership structures) would lose its legitimating logic. The National Guard and state defense forces would lose their constitutional anchor. The individual_right_reading and collective_right_reading would become the only live frames, reshaping Second Amendment doctrine and firearms regulation. The republican polity would lose its armed-citizenry anchor.
% FOUNDING_PROBLEM: The founding problem was the republican dilemma: how to maintain defense capacity without creating a standing army that threatens liberty, and how to cultivate civic virtue in a commercial republic. The armed citizen-militia was the solution — citizens who bear arms as a civic duty, not a private right, embodying the republican ideal of the citizen-soldier.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the founding era (Bailyn, Wood, Pocock) corroborate the republican armed-citizenship frame as the dominant founding understanding. Contemporary originalist scholars (e.g., Amar, Cornell) attest the frame is historically grounded but contested whether it survives incorporation. The individual_right_reading beneficiaries (gun rights organizations, libertarian legal scholars) contest that the founding problem is dead — they argue the individual right pre-exists and supersedes the civic frame. No single corroborating source outside the beneficiary set resolves the contestation.
narrative_ontology:disappearance_verdict(second_amendment_arms_right__civic_republican_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_arms_right__civic_republican_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_arms_right__civic_republican_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(second_amendment_arms_right__civic_republican_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_arms_right__civic_republican_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_arms_right__civic_republican_reading_tests).
:- end_tests(second_amendment_arms_right__civic_republican_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) is moderate: the civic participation requirement (training, qualification, organized membership) imposes real costs but delivers coordination value to participants. Suppression (0.35) is modest: alternatives (non-participation, individualist framing) are discouraged but not eliminated — the constraint operates through civic norms more than coercion. Theater ratio (0.28) reflects growing performativity: the militia function has atrophied since 1791 while regulatory apparatus expanded (Dick Act 1903, NFA 1934, GCA 1968, Heller/McDonald incorporation). The measurement series on a shared grid (1791, 1868, 1903, 1934, 1968, 2008, 2024) shows extraction accumulation and theater creep as the founding problem (standing army danger, republican virtue) transformed. Accessibility collapse (0.45) and resistance (0.55) reflect that the civic frame remains contestable — neither a natural law nor a closed snare.
 *
 * PERSPECTIVAL GAP:
 *   The citizen-militia member seat experiences the constraint as rope (coordination with reciprocal duty). The unorganized nonparticipant seat experiences it as snare (extraction without coordination payoff). The federal regulatory authority seat experiences it as scaffold (transitional structure for a militia function that has largely migrated to the National Guard). The engine computes this divergence from the structural data — the authored claim (tangled_rope) captures the hybrid nature across seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Citizen-militia members are structural beneficiaries (d ~ 0.2): they receive the right AND the civic standing, but bear the duty (training, organization). The republican polity is a beneficiary (d ~ 0.1): gains defensive capacity rooted in civic virtue. Local governments are mild beneficiaries (d ~ 0.3): administer the system but gain institutional role. Unorganized nonparticipants are targets (d ~ 0.75): bear regulatory costs (background checks, qualification gates) without the civic coordination payoff. Absolute individualist claimants are targets (d ~ 0.7): their interpretive frame is excluded by the civic norm. Federal regulatory authority is agenda_setter (d ~ 0.4): enforces the constraint but also bears institutional costs of administration. The directionality derives from beneficiary/victim declarations + exit options: civic identity lock for militia members dampens extraction; constrained exit for nonparticipants amplifies it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (republican self-governance requires armed citizenry as check on standing armies and cultivation of civic virtue) is contested: the standing army danger has transformed (professional military, nuclear deterrence), but civic virtue arguments persist. The constraint persists with rising extraction/theater because the regulatory apparatus (qualification gates, training requirements) expanded even as the militia function atrophied (National Guard federalized, unorganized militia statutory but inactive). This is NOT pure mandatrophy (the civic frame still coordinates some participants) but shows mandatrophy dynamics: the coordination function degraded while extraction persisted. The tangled_rope classification captures this hybrid — genuine coordination for participants, asymmetric extraction for nonparticipants.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the civic republican reading of the Second Amendment a distinct constraint with its own ε, or merely an interpretive gloss on the individual/collective rights binary?',
    'Trace whether the civic republican reading produces a stable beneficiary/victim structure and extractiveness profile that differs from both the individual_right_reading (which centers libertarian autonomy) and the collective_right_reading (which centers state militia authority). If the beneficiary structure is citizen-militia members as dual right/duty holders and regulatory authority is constrained by civic participation norms rather than individualist or statist logics, the reading instantiates a separate constraint.',
    'If this reading is structurally distinct, it must be authored as a separate constraint story with its own ε, stakeholders, and classification — linked to siblings via network.affects_constraints. If not, it collapses into one of the sibling readings and the kernel has only two constraints, not three.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the civic republican reading is a third structurally distinct constraint in the kernel family.').

omega_variable(
    civic_participation_vs_libertarian_exit,
    'Does the civic participation norm (training, qualification, organized membership) function as genuine coordination (rope-like) or as a suppressed exit mechanism that extracts from those who reject the civic frame?',
    'Compare the experience of citizen-militia members who accept the civic frame (low extraction, high coordination value) with unorganized nonparticipants who face regulatory burdens without the civic benefit. If the latter experience the requirement as pure extraction with no coordination payoff, the constraint has a snare-like edge for that seat.',
    'If the civic participation norm is experienced as extractive by nonparticipants, the constraint is tangled_rope with asymmetric extraction — coordination for participants, extraction for nonparticipants. If nonparticipants are few or the norm is genuinely optional, the constraint leans toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civic_participation_vs_libertarian_exit, empirical, 'Whether the civic participation requirement is coordination or extraction for those outside the civic frame.').

omega_variable(
    historical_militia_atrophy,
    'Has the organized militia function atrophied such that the constraint''s coordination justification (republican self-governance through armed citizenship) is now largely performative?',
    'Assess whether contemporary ''citizen-militia members'' (National Guard, organized militia statutes, civic training programs) actually perform the republican function the reading posits, or whether the constraint persists via institutional inertia while the founding problem (standing army danger, republican virtue) has transformed.',
    'If the militia function has atrophied, the constraint may be piton (theatrical maintenance of a degraded coordination function) rather than tangled_rope. The theater_ratio metric (0.28) and founding_problem_status (contested) capture this tension.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_militia_atrophy, empirical, 'Whether the constraint''s coordination function is live or atrophied.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_arms_right__civic_republican_reading, 1791, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(second_amendment_arms_right__civic_republican_reading_tr_t1791, second_amendment_arms_right__civic_republican_reading, theater_ratio, 1791, 0.05).
narrative_ontology:measurement(second_amendment_arms_right__civic_republican_reading_tr_t1868, second_amendment_arms_right__civic_republican_reading, theater_ratio, 1868, 0.12).
narrative_ontology:measurement(second_amendment_arms_right__civic_republican_reading_tr_t1903, second_amendment_arms_right__civic_republican_reading, theater_ratio, 1903, 0.18).
narrative_ontology:measurement(second_amendment_arms_right__civic_republican_reading_tr_t1934, second_amendment_arms_right__civic_republican_reading, theater_ratio, 1934, 0.22).
narrative_ontology:measurement(second_amendment_arms_right__civic_republican_reading_tr_t1968, second_amendment_arms_right__civic_republican_reading, theater_ratio, 1968, 0.25).
narrative_ontology:measurement(second_amendment_arms_right__civic_republican_reading_tr_t2008, second_amendment_arms_right__civic_republican_reading, theater_ratio, 2008, 0.27).
narrative_ontology:measurement(second_amendment_arms_right__civic_republican_reading_tr_t2024, second_amendment_arms_right__civic_republican_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(second_amendment_arms_right__civic_republican_reading_be_t1791, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 1791, 0.15).
narrative_ontology:measurement(second_amendment_arms_right__civic_republican_reading_be_t1868, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 1868, 0.22).
narrative_ontology:measurement(second_amendment_arms_right__civic_republican_reading_be_t1903, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 1903, 0.28).
narrative_ontology:measurement(second_amendment_arms_right__civic_republican_reading_be_t1934, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 1934, 0.35).
narrative_ontology:measurement(second_amendment_arms_right__civic_republican_reading_be_t1968, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 1968, 0.38).
narrative_ontology:measurement(second_amendment_arms_right__civic_republican_reading_be_t2008, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 2008, 0.4).
narrative_ontology:measurement(second_amendment_arms_right__civic_republican_reading_be_t2024, second_amendment_arms_right__civic_republican_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(second_amendment_arms_right__civic_republican_reading_su_t1791, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 1791, 0.1).
narrative_ontology:measurement(second_amendment_arms_right__civic_republican_reading_su_t1868, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 1868, 0.18).
narrative_ontology:measurement(second_amendment_arms_right__civic_republican_reading_su_t1903, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 1903, 0.25).
narrative_ontology:measurement(second_amendment_arms_right__civic_republican_reading_su_t1934, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 1934, 0.3).
narrative_ontology:measurement(second_amendment_arms_right__civic_republican_reading_su_t1968, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 1968, 0.32).
narrative_ontology:measurement(second_amendment_arms_right__civic_republican_reading_su_t2008, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 2008, 0.34).
narrative_ontology:measurement(second_amendment_arms_right__civic_republican_reading_su_t2024, second_amendment_arms_right__civic_republican_reading, suppression_requirement, 2024, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_arms_right__civic_republican_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(second_amendment_arms_right__civic_republican_reading, 0.08).
narrative_ontology:affects_constraint(second_amendment_arms_right__civic_republican_reading, second_amendment_arms_right__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_arms_right__civic_republican_reading, second_amendment_arms_right__collective_right_reading).

% DUAL FORMULATION NOTE:
% The second_amendment_arms_right kernel decomposes into three constraint stories: (1) civic_republican_reading — armed citizenship as republican prerequisite, tangled_rope, ε=0.42, citizen-militia members as dual beneficiaries; (2) individual_right_reading — individual liberty pre-existing government, likely rope or mountain depending on ε, individual owners as beneficiaries; (3) collective_right_reading — state militia authority, likely scaffold or piton, state entities as beneficiaries. The ε values differ structurally: civic republican reading has moderate ε on training/qualification; individual rights reading has low ε on possession but high ε on regulatory exclusion; collective rights reading has ε on state capacity. They are linked as a family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(second_amendment_arms_right__civic_republican_reading, organized, 0.25).
constraint_indexing:directionality_override(second_amendment_arms_right__civic_republican_reading, powerless, 0.8).
constraint_indexing:directionality_override(second_amendment_arms_right__civic_republican_reading, moderate, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
