% ============================================================================
% CONSTRAINT STORY: war_powers_allocation__congressional_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_powers_allocation__congressional_primacy_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: war_powers_allocation__congressional_primacy_reading
 *   human_readable: Congressional Authorization Required for Offensive War (Constitutional Primacy Reading)
 *   domain: constitutional/war_powers
 *
 * SUMMARY:
 *   The congressional primacy reading of the war powers allocation kernel
 *   holds that the Constitution's text, structure, and original understanding
 *   establish a Mountain constraint: offensive war requires explicit
 *   congressional authorization. This constraint is claimed as a fixed
 *   constitutional necessity (emerges_naturally: true). Descriptively, the
 *   constraint has faced persistent executive resistance, increasing
 *   performative compliance (theater), and periods of eroded enforcement, but
 *   its core claim remains that alternatives are collapsed by the
 *   constitutional design itself. The metrics reflect the constraint's actual
 *   historical operation: low extractiveness (the rule does not extract
 *   resources), high suppression of executive unilateral claims, moderate
 *   theater (performative consultations, AUMF drift), very high accessibility
 *   collapse (no legal alternative to congressional authorization for
 *   offensive war), and significant resistance (executive branch pushes
 *   inherent authority).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_powers_allocation__congressional_primacy_reading, 0.12).
domain_priors:suppression_score(war_powers_allocation__congressional_primacy_reading, 0.78).
domain_priors:theater_ratio(war_powers_allocation__congressional_primacy_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_powers_allocation__congressional_primacy_reading, mountain).
narrative_ontology:human_readable(war_powers_allocation__congressional_primacy_reading, "Congressional Authorization Required for Offensive War (Constitutional Primacy Reading)").
narrative_ontology:topic_domain(war_powers_allocation__congressional_primacy_reading, "constitutional/war_powers").

domain_priors:emerges_naturally(war_powers_allocation__congressional_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_powers_allocation__congressional_primacy_reading, 'b0b7f664-f1a7-4b77-9e25-4620c5ccdf77').
narrative_ontology:cs_kernel_codification('b0b7f664-f1a7-4b77-9e25-4620c5ccdf77', formalized).
narrative_ontology:cs_authority_grounding('b0b7f664-f1a7-4b77-9e25-4620c5ccdf77', lineage).
narrative_ontology:cs_interpretation_layer_present('b0b7f664-f1a7-4b77-9e25-4620c5ccdf77').
narrative_ontology:cs_reading_relation('b0b7f664-f1a7-4b77-9e25-4620c5ccdf77', war_powers_allocation__inherent_executive_reading, forecloses).
narrative_ontology:cs_reading_relation('b0b7f664-f1a7-4b77-9e25-4620c5ccdf77', war_powers_allocation__functional_accommodation_reading, coexists_with).
narrative_ontology:cs_axiom('b0b7f664-f1a7-4b77-9e25-4620c5ccdf77', foundational, congressional_authorization_required_for_offensive_war).
narrative_ontology:cs_axiom_status(congressional_authorization_required_for_offensive_war, holdable).
narrative_ontology:cs_axiom_grounding('b0b7f664-f1a7-4b77-9e25-4620c5ccdf77', congressional_authorization_required_for_offensive_war, conventional).
narrative_ontology:cs_axiom('b0b7f664-f1a7-4b77-9e25-4620c5ccdf77', secondary, executive_inherent_authority_limited_to_immediate_defense).
narrative_ontology:cs_axiom_status(executive_inherent_authority_limited_to_immediate_defense, holdable).
narrative_ontology:cs_axiom_grounding('b0b7f664-f1a7-4b77-9e25-4620c5ccdf77', executive_inherent_authority_limited_to_immediate_defense, conventional).
narrative_ontology:cs_reference_frame('b0b7f664-f1a7-4b77-9e25-4620c5ccdf77', original_constitutional_allocation).
narrative_ontology:cs_drift_state('b0b7f664-f1a7-4b77-9e25-4620c5ccdf77', contemporary_post_2001_aumf_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b0b7f664-f1a7-4b77-9e25-4620c5ccdf77', '').
narrative_ontology:cs_kernel_id(war_powers_allocation__congressional_primacy_reading, war_powers_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_powers_allocation__congressional_primacy_reading, congress).
narrative_ontology:constraint_beneficiary(war_powers_allocation__congressional_primacy_reading, the_public).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(war_powers_allocation__congressional_primacy_reading, president).
narrative_ontology:constraint_victim(war_powers_allocation__congressional_primacy_reading, military).
narrative_ontology:constraint_vindicates(war_powers_allocation__congressional_primacy_reading, constitutional_war_power_allocation).
narrative_ontology:constraint_vindicates(war_powers_allocation__congressional_primacy_reading, separation_of_powers_doctrine).
narrative_ontology:constraint_vindicates(war_powers_allocation__congressional_primacy_reading, legislative_control_of_offensive_force).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the constitutional power to declare war and authorize offensive military action. The constraint protects this institutional role from executive encroachment. Exit from this role would require constitutional amendment or systemic collapse.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, congress, agenda_setter,
    institutional, generational, constrained, national).

% Serves as commander-in-chief but is constitutionally barred from initiating offensive war without congressional authorization. Bears the political and operational costs of seeking authorization; unilateral action triggers constitutional crisis.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, president, payer,
    powerful, biographical, constrained, national).

% Benefits from the constitutional check that forces deliberative, representative decision-making before offensive war. The constraint channels war decisions through elected representatives rather than a single executive.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, the_public, beneficiary,
    organized, generational, mobile, national).

% Adjudicates war powers disputes when cases are justiciable. Provides interpretive rulings on the scope of congressional authorization and executive inherent authority. Does not initiate war or bear its costs directly.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, federal_courts, observer,
    institutional, generational, analytical, national).

% Executes authorized military operations. Bears the operational risks of war. The constraint shapes the legal basis for deployments but does not determine operational tactics.
narrative_ontology:constraint_stakeholder(war_powers_allocation__congressional_primacy_reading, military, payer,
    organized, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates the authority to initiate offensive war between the legislative and executive branches, preventing unilateral executive war-making and ensuring collective deliberation.
% TRANSFER_FUNCTION: Transfers the decision to initiate offensive war from the president (who would otherwise act unilaterally) to Congress, requiring explicit authorization as the price of legitimacy.
% ABSENT_VOICES: The executive branch's inherent-authority proponents (executive branch lawyers, unitary executive theorists) are structurally excluded from the constraint's design; they would argue for presidential discretion but are suppressed by the constitutional text and this reading's interpretation.
% DISAPPEARANCE_RATIONALE: If the constraint vanished overnight, the president would possess unchecked authority to initiate offensive war, fundamentally altering the constitutional balance, eliminating the legislative check, and enabling wars without public deliberation or representative consent.
% FOUNDING_PROBLEM: The founding problem was preventing the executive monopoly on war decisions that characterized the British monarchy, ensuring that the power to take the nation into offensive war rests with the people's representatives.
% FOUNDING_PROBLEM_CORROBORATION: The Constitutional Convention records, Federalist Papers (esp. Federalist 69), and early practice (Washington's limited unilateral actions) corroborate the founding problem. Executive-branch legal opinions (OLC memos) and modern practice since Korea constitute the contesting tradition; no neutral arbiter has settled the dispute.
narrative_ontology:disappearance_verdict(war_powers_allocation__congressional_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(war_powers_allocation__congressional_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_powers_allocation__congressional_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(war_powers_allocation__congressional_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(war_powers_allocation__congressional_primacy_reading, 0.12, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_powers_allocation__congressional_primacy_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, ExtMetricName, E),
    domain_priors:suppression_score(war_powers_allocation__congressional_primacy_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(war_powers_allocation__congressional_primacy_reading),
    narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(war_powers_allocation__congressional_primacy_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(war_powers_allocation__congressional_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.12) because the constraint is a limit on power, not a transfer of resources. Suppression is high (0.78) because the constraint actively suppresses inherent-executive-authority claims through constitutional text, judicial doctrine, and political norms. Theater ratio is moderate (0.32) because post-1950 practice includes performative congressional consultations and broad AUMFs that mimic authorization without genuine deliberation. Accessibility collapse is very high (0.88) because the constitutional text and structure leave no recognized legal alternative for offensive war. Resistance is high (0.68) because every modern president has asserted inherent authority to some degree, creating a persistent gap between the Mountain claim and operational reality.
 *
 * PERSPECTIVAL GAP:
 *   From Congress's seat (agenda_setter, institutional), the constraint is a Mountain: the constitutional text is fixed, alternatives are collapsed, and resistance is mere usurpation. From the president's seat (payer, powerful), the constraint operates as a Tangled Rope or Snare: it coordinates some military actions but extracts political autonomy and is actively resisted. The engine computes this seat divergence from the structural data; the authored claim (Mountain) reflects the congressional primacy reading's self-understanding, not a reconciliation.
 *
 * DIRECTIONALITY LOGIC:
 *   Congress is the primary beneficiary (d near 0.0): the constraint secures its constitutional role and prevents extraction of its war power. The public is a secondary beneficiary (d low): the constraint forces representative deliberation. The president is the primary payer (d near 1.0): bears the cost of seeking authorization and is constrained from unilateral action. The military is a payer (d high): executes wars whose legal basis depends on the constraint. Courts are observers (d=0.5): they interpret but do not initiate or bear costs. The directionality derivation from beneficiary/victim declarations plus exit options (president constrained, Congress institutional) yields these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing executive war monopoly) remains live — the constraint has not outlived its function. However, the constraint's mandate has been partially captured by executive practice: AUMFs and unilateral actions have layered extraction onto the original coordination function. The mandatrophy risk is not obsolescence but drift: the constraint's coordination core persists while its enforcement erodes, creating a gap the engine's T17 abductive trigger would flag if extraction accumulation were tracked. We declare mandatrophy_resolved false.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mountain_vs_constructed_ambiguity,
    'Is the congressional authorization requirement a genuine Mountain (fixed by constitutional nature) or a constructed constraint that benefits Congress and the public by suppressing executive power?',
    'Historical analysis of founding-era understanding vs. modern practice; judicial doctrine stability; whether the constraint would persist without active political enforcement.',
    'If Mountain, the constraint is immune to extraction metrics and FSM does not apply. If constructed, FSM triggers and the constraint reclassifies as Tangled Rope (coordination + asymmetric extraction from executive). The omega documents the irreducible ambiguity that makes FSM evaluation necessary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mountain_vs_constructed_ambiguity, conceptual, 'Natural-law vs. constructed status of the war powers allocation rule.').

omega_variable(
    suppression_mechanism_internalized,
    'Is the high suppression of inherent executive authority claims structural (legal barriers, judicial review) or internalized (executive branch self-restraint, norms), and what happens when internalized suppression erodes?',
    'Track executive branch compliance when political cost of non-compliance is low (e.g., limited strikes, covert actions). If suppression persists without structural enforcement, internalized component is significant.',
    'If suppression is largely internalized, the constraint''s effective suppression is higher than structural measures suggest, but vulnerable to norm erosion. A shift from internalized to purely structural suppression would increase measured resistance and decrease accessibility collapse.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized, empirical, 'Structural vs. internalized suppression of executive inherent authority claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_powers_allocation__congressional_primacy_reading, 1789, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t1789, war_powers_allocation__congressional_primacy_reading, theater_ratio, 1789, 0.05).
narrative_ontology:measurement(war__tr_t1850, war_powers_allocation__congressional_primacy_reading, theater_ratio, 1850, 0.1).
narrative_ontology:measurement(war__tr_t1917, war_powers_allocation__congressional_primacy_reading, theater_ratio, 1917, 0.15).
narrative_ontology:measurement(war__tr_t1950, war_powers_allocation__congressional_primacy_reading, theater_ratio, 1950, 0.25).
narrative_ontology:measurement(war__tr_t2001, war_powers_allocation__congressional_primacy_reading, theater_ratio, 2001, 0.35).
narrative_ontology:measurement(war__tr_t2024, war_powers_allocation__congressional_primacy_reading, theater_ratio, 2024, 0.32).

% Extraction over time
narrative_ontology:measurement(war__be_t1789, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 1789, 0.05).
narrative_ontology:measurement(war__be_t1850, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 1850, 0.07).
narrative_ontology:measurement(war__be_t1917, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 1917, 0.1).
narrative_ontology:measurement(war__be_t1950, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 1950, 0.15).
narrative_ontology:measurement(war__be_t2001, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 2001, 0.18).
narrative_ontology:measurement(war__be_t2024, war_powers_allocation__congressional_primacy_reading, base_extractiveness, 2024, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t1789, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 1789, 0.85).
narrative_ontology:measurement(war__su_t1850, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 1850, 0.8).
narrative_ontology:measurement(war__su_t1917, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 1917, 0.75).
narrative_ontology:measurement(war__su_t1950, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 1950, 0.7).
narrative_ontology:measurement(war__su_t2001, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 2001, 0.65).
narrative_ontology:measurement(war__su_t2024, war_powers_allocation__congressional_primacy_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_powers_allocation__congressional_primacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(war_powers_allocation__congressional_primacy_reading, 0.1).
narrative_ontology:affects_constraint(war_powers_allocation__congressional_primacy_reading, war_powers_allocation__inherent_executive_reading).
narrative_ontology:affects_constraint(war_powers_allocation__congressional_primacy_reading, war_powers_allocation__functional_accommodation_reading).

% DUAL FORMULATION NOTE:
% This constraint, the inherent_executive_reading, and the functional_accommodation_reading form a constraint family decomposing the 'war powers allocation' kernel. Each reading instantiates a different constraint with distinct ε, beneficiaries, and victims. The congressional primacy reading (this story) claims Mountain; inherent executive claims Snare (extracts from Congress); functional accommodation claims Tangled Rope (context-dependent coordination with extraction).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
