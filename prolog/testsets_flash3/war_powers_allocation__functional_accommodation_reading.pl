% ============================================================================
% CONSTRAINT STORY: war_powers_allocation__functional_accommodation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_powers_allocation__functional_accommodation_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: war_powers_allocation__functional_accommodation_reading
 *   human_readable: War Powers Allocation: Functional Accommodation Reading
 *   domain: constitutional_law/separation_of_powers/war_powers
 *
 * SUMMARY:
 *   This constraint represents the 'functional accommodation' reading of US
 *   war powers, where the allocation of authority between the executive and
 *   legislative branches is not rigidly fixed but adapts to operational
 *   context. Imminent threats are seen to permit unilateral executive action,
 *   while prolonged campaigns require congressional authorization. This
 *   reading acknowledges a gray area where both branches claim authority,
 *   leading to a dynamic tension rather than clear-cut rules. The
 *   constraint's extractiveness varies by context, and its persistence relies
 *   on suppressing categorical rules in favor of flexible interpretation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_powers_allocation__functional_accommodation_reading, 0.65).
domain_priors:suppression_score(war_powers_allocation__functional_accommodation_reading, 0.7).
domain_priors:theater_ratio(war_powers_allocation__functional_accommodation_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(war_powers_allocation__functional_accommodation_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_powers_allocation__functional_accommodation_reading, tangled_rope).
narrative_ontology:human_readable(war_powers_allocation__functional_accommodation_reading, "War Powers Allocation: Functional Accommodation Reading").
narrative_ontology:topic_domain(war_powers_allocation__functional_accommodation_reading, "constitutional_law/separation_of_powers/war_powers").

domain_priors:requires_active_enforcement(war_powers_allocation__functional_accommodation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_powers_allocation__functional_accommodation_reading, 'd1acccff-a124-4744-b2ec-3bc1af184fe1').
narrative_ontology:cs_kernel_codification('d1acccff-a124-4744-b2ec-3bc1af184fe1', fixed_text).
narrative_ontology:cs_authority_grounding('d1acccff-a124-4744-b2ec-3bc1af184fe1', lineage).
narrative_ontology:cs_interpretation_layer_present('d1acccff-a124-4744-b2ec-3bc1af184fe1').
narrative_ontology:cs_reading_relation('d1acccff-a124-4744-b2ec-3bc1af184fe1', war_powers_allocation__congressional_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('d1acccff-a124-4744-b2ec-3bc1af184fe1', war_powers_allocation__inherent_executive_reading, coexists_with).
narrative_ontology:cs_axiom('d1acccff-a124-4744-b2ec-3bc1af184fe1', foundational, executive_flexibility_in_threat_response).
narrative_ontology:cs_axiom_status(executive_flexibility_in_threat_response, holdable).
narrative_ontology:cs_axiom_grounding('d1acccff-a124-4744-b2ec-3bc1af184fe1', executive_flexibility_in_threat_response, instrumental).
narrative_ontology:cs_axiom('d1acccff-a124-4744-b2ec-3bc1af184fe1', foundational, congressional_oversight_for_prolonged_conflict).
narrative_ontology:cs_axiom_status(congressional_oversight_for_prolonged_conflict, holdable).
narrative_ontology:cs_axiom_grounding('d1acccff-a124-4744-b2ec-3bc1af184fe1', congressional_oversight_for_prolonged_conflict, conventional).
narrative_ontology:cs_reference_frame('d1acccff-a124-4744-b2ec-3bc1af184fe1', balanced_functional_powers).
narrative_ontology:cs_drift_state('d1acccff-a124-4744-b2ec-3bc1af184fe1', contemporary_era_of_global_threats, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d1acccff-a124-4744-b2ec-3bc1af184fe1', '').
narrative_ontology:cs_kernel_id(war_powers_allocation__functional_accommodation_reading, war_powers_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_powers_allocation__functional_accommodation_reading, executive_branch).
narrative_ontology:constraint_victim(war_powers_allocation__functional_accommodation_reading, congressional_branch).
narrative_ontology:constraint_victim(war_powers_allocation__functional_accommodation_reading, public_discourse).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(war_powers_allocation__functional_accommodation_reading, military_personnel).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Asserts inherent authority to deploy military force in response to imminent threats, interpreting 'imminent' broadly. Benefits from flexibility and speed in foreign policy, but faces political and legal challenges for prolonged engagements without congressional backing. Actively seeks to define operational contexts that permit unilateral action.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, executive_branch, agenda_setter,
    institutional, biographical, constrained, global).

% Bears the political cost of ceding war-making authority to the executive, often reacting to faits accomplis. Attempts to reassert its constitutional role through legislation (e.g., War Powers Resolution) but frequently finds its mechanisms circumvented or ignored. Its power is diluted by the executive's ability to act unilaterally in 'imminent' situations.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, congressional_branch, payer,
    institutional, generational, constrained, national).

% Generally avoids adjudicating war powers disputes, deeming them 'political questions.' Observes the ongoing contest between the executive and legislative branches, occasionally issuing rulings on specific aspects but rarely intervening directly in the allocation of war powers itself.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, judiciary, observer,
    institutional, civilizational, analytical, national).

% Suffers from a lack of clear accountability and transparency regarding military engagements. Public debate is often reactive, responding to executive actions rather than proactively shaping policy. The ambiguity of war powers allocation makes informed public consent difficult.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, public_discourse, payer,
    moderate, immediate, constrained, national).

% Execute orders from the Commander-in-Chief, regardless of the underlying constitutional authorization debate. Their lives and careers are directly impacted by deployments, but they have no direct voice in the war powers allocation. Their identity as service members binds them to the chain of command.
narrative_ontology:constraint_stakeholder(war_powers_allocation__functional_accommodation_reading, military_personnel, payer,
    powerless, biographical, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a flexible framework for national security decision-making, allowing for rapid executive response to immediate threats while theoretically requiring broader authorization for sustained conflicts. It attempts to balance efficiency with democratic accountability.
% TRANSFER_FUNCTION: Transfers decision-making authority for military action from the legislative to the executive branch in contexts deemed 'imminent threat,' and transfers the burden of justifying prolonged engagements back to Congress, often after the fact. It also transfers the cost of ambiguity to public discourse and military personnel.
% ABSENT_VOICES: A more robust and proactive congressional oversight mechanism, capable of asserting its constitutional prerogatives before, rather than after, executive military action. Also, a more direct and informed public voice in the decision to commit to military force, unmediated by executive framing.
% DISAPPEARANCE_RATIONALE: If this functional accommodation reading vanished, the constitutional framework for war powers would revert to a more rigid interpretation, likely leading to either a strict congressional primacy (requiring explicit authorization for almost all deployments) or an unchecked inherent executive power. Either outcome would fundamentally alter how the US engages in military action, with significant domestic and international repercussions.
% FOUNDING_PROBLEM: The framers sought to balance the need for a swift executive response to national emergencies with the democratic principle of legislative control over declarations of war and funding for military action.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars and legal historians widely corroborate the framers' intent to balance these powers. The ongoing debates and legislative efforts (e.g., War Powers Resolution) attest to the problem's continued live status, even if the functional accommodation reading attempts to manage the tension rather than resolve it definitively.
narrative_ontology:disappearance_verdict(war_powers_allocation__functional_accommodation_reading, world_rearranges).
narrative_ontology:founding_problem_status(war_powers_allocation__functional_accommodation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_powers_allocation__functional_accommodation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(war_powers_allocation__functional_accommodation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(war_powers_allocation__functional_accommodation_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_powers_allocation__functional_accommodation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_powers_allocation__functional_accommodation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_powers_allocation__functional_accommodation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) reflects the executive's ability to act without explicit prior authorization, effectively extracting decision-making power from Congress. Suppression (0.70) is high because this reading actively suppresses attempts to impose rigid, categorical rules on war powers, maintaining an ambiguous zone where executive action can proceed. The theater ratio (0.40) indicates that while some executive actions are genuinely responsive to threats, a significant portion of the 'imminence' justification serves to bypass congressional oversight. The temporal measurements show a slight increase in extractiveness and suppression over time, reflecting a trend towards greater executive unilateralism, before stabilizing as the political system adapts to this interpretation.
 *
 * PERSPECTIVAL GAP:
 *   The executive branch perceives this as a necessary and efficient coordination mechanism for national security. The legislative branch and public discourse often perceive it as an extractive mechanism that erodes checks and balances. The engine's per-seat classification will reflect these divergent experiences based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The executive branch is the primary beneficiary, gaining flexibility and speed in deploying force. The congressional branch and public discourse are victims, losing oversight and accountability. The judiciary acts as an observer, largely deferring to the political branches. Military personnel are also victims, bearing the direct costs of deployments decided under this ambiguous framework, with their identity as service members limiting their exit options.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    imminence_definition_ambiguity,
    'How is ''imminent threat'' defined, and who holds the authority to make that determination?',
    'A clear, judicially enforceable definition of ''imminent threat'' or a formal inter-branch agreement on the process for its determination.',
    'A narrow, objective definition would reduce executive unilateralism and lower extractiveness; a broad, executive-controlled definition would maintain or increase it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(imminence_definition_ambiguity, conceptual, 'Ambiguity in defining ''imminent threat'' allows executive discretion.').

omega_variable(
    congressional_will_vs_executive_action,
    'To what extent does congressional inaction or acquiescence constitute implicit authorization for executive military action?',
    'A Supreme Court ruling clarifying the legal weight of congressional silence or a legislative act explicitly defining the conditions under which inaction implies consent.',
    'If inaction is deemed implicit authorization, executive extractiveness is reinforced; if not, congressional power is strengthened, potentially reclassifying the constraint towards a more balanced rope or even a scaffold if temporary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(congressional_will_vs_executive_action, empirical, 'Whether congressional inaction implies consent for executive military action.').

omega_variable(
    mandatrophy_of_war_powers_resolution,
    'Has the War Powers Resolution (WPR) become a piton, where its original intent to curb executive power has atrophied, and it now primarily serves as a procedural hurdle rather than a substantive check?',
    'Empirical analysis of executive compliance with WPR reporting requirements versus actual changes in military deployments, and expert legal opinion on its effectiveness as a constraint.',
    'If the WPR is a piton, the functional accommodation reading''s extractiveness is higher than it appears, as a nominal check is inert. If it retains substantive force, extractiveness is lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_of_war_powers_resolution, empirical, 'Whether the War Powers Resolution has atrophied into a piton.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_powers_allocation__functional_accommodation_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t0, war_powers_allocation__functional_accommodation_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(war__tr_t10, war_powers_allocation__functional_accommodation_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(war__tr_t20, war_powers_allocation__functional_accommodation_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(war__tr_t30, war_powers_allocation__functional_accommodation_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement(war__tr_t40, war_powers_allocation__functional_accommodation_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement(war__tr_t50, war_powers_allocation__functional_accommodation_reading, theater_ratio, 50, 0.4).

% Extraction over time
narrative_ontology:measurement(war__be_t0, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(war__be_t10, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(war__be_t20, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(war__be_t30, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(war__be_t40, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 40, 0.67).
narrative_ontology:measurement(war__be_t50, war_powers_allocation__functional_accommodation_reading, base_extractiveness, 50, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t0, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(war__su_t10, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(war__su_t20, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(war__su_t30, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(war__su_t40, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement(war__su_t50, war_powers_allocation__functional_accommodation_reading, suppression_requirement, 50, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_powers_allocation__functional_accommodation_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'war_powers_allocation' kernel. It focuses on the dynamic, context-dependent allocation of war powers, contrasting with the 'congressional_primacy_reading' and 'inherent_executive_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
