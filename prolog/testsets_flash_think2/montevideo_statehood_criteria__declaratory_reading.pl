% ============================================================================
% CONSTRAINT STORY: montevideo_statehood_criteria__declaratory_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_montevideo_statehood_criteria__declaratory_reading, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: montevideo_statehood_criteria__declaratory_reading
 *   human_readable: Declaratory Theory of Statehood (Montevideo Convention)
 *   domain: international_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint represents the declaratory reading of the Montevideo
 *   Statehood Criteria, which posits that statehood is a legal fact
 *   established by meeting four objective criteria (defined territory,
 *   permanent population, government, capacity to enter into relations with
 *   other states), independent of recognition by other states. This reading
 *   contests the constitutive theory, which holds that recognition is
 *   necessary for statehood. The metrics reflect the ongoing tension: while
 *   the principle aims to reduce arbitrary extraction, its imperfect
 *   application in practice means entities meeting the criteria still face
 *   significant costs due to non-recognition, leading to a classification as
 *   a Tangled Rope.
 *
 * KEY AGENTS:
 *   - Newly emerging states: Primary beneficiaries (when the principle is upheld), but also targets of non-recognition.
 *   - De facto authorities meeting criteria: Primary victims, bearing the costs of non-recognition despite legal entitlement.
 *   - Existing states denying recognition: Agenda-setters, benefiting from maintaining political leverage.
 *   - International legal scholars: Observers and advocates for the principle.
 *   - International organizations: Agenda-setters, mediating between legal principle and political reality.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(montevideo_statehood_criteria__declaratory_reading, 0.65).
domain_priors:suppression_score(montevideo_statehood_criteria__declaratory_reading, 0.7).
domain_priors:theater_ratio(montevideo_statehood_criteria__declaratory_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(montevideo_statehood_criteria__declaratory_reading, tangled_rope).
narrative_ontology:human_readable(montevideo_statehood_criteria__declaratory_reading, "Declaratory Theory of Statehood (Montevideo Convention)").
narrative_ontology:topic_domain(montevideo_statehood_criteria__declaratory_reading, "international_law/political_philosophy").

domain_priors:requires_active_enforcement(montevideo_statehood_criteria__declaratory_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(montevideo_statehood_criteria__declaratory_reading, 'c80e9369-c952-4753-a0e1-e557c8e6bdef').
narrative_ontology:cs_kernel_codification('c80e9369-c952-4753-a0e1-e557c8e6bdef', formalized).
narrative_ontology:cs_authority_grounding('c80e9369-c952-4753-a0e1-e557c8e6bdef', lineage).
narrative_ontology:cs_interpretation_layer_present('c80e9369-c952-4753-a0e1-e557c8e6bdef').
narrative_ontology:cs_reading_relation('c80e9369-c952-4753-a0e1-e557c8e6bdef', montevideo_statehood_criteria__constitutive_reading, forecloses).
narrative_ontology:cs_reading_relation('c80e9369-c952-4753-a0e1-e557c8e6bdef', montevideo_statehood_criteria__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('c80e9369-c952-4753-a0e1-e557c8e6bdef', foundational, statehood_is_objective_fact).
narrative_ontology:cs_axiom_status(statehood_is_objective_fact, holdable).
narrative_ontology:cs_axiom_grounding('c80e9369-c952-4753-a0e1-e557c8e6bdef', statehood_is_objective_fact, conventional).
narrative_ontology:cs_axiom('c80e9369-c952-4753-a0e1-e557c8e6bdef', secondary, recognition_is_declaratory_only).
narrative_ontology:cs_axiom_status(recognition_is_declaratory_only, holdable).
narrative_ontology:cs_axiom_grounding('c80e9369-c952-4753-a0e1-e557c8e6bdef', recognition_is_declaratory_only, conventional).
narrative_ontology:cs_reference_frame('c80e9369-c952-4753-a0e1-e557c8e6bdef', post_montevideo_legal_order).
narrative_ontology:cs_drift_state('c80e9369-c952-4753-a0e1-e557c8e6bdef', contemporary_geopolitical_realities, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c80e9369-c952-4753-a0e1-e557c8e6bdef', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(montevideo_statehood_criteria__declaratory_reading, montevideo_statehood_criteria).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__declaratory_reading, newly_emerging_states).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__declaratory_reading, international_legal_scholars).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__declaratory_reading, de_facto_authorities_meeting_criteria).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These entities meet the objective criteria for statehood (defined territory, permanent population, government, capacity to enter into relations with other states) and, under this reading, are legally states. They benefit from the principle's assertion of their status, even if political recognition is withheld.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, newly_emerging_states, beneficiary,
    moderate, biographical, constrained, global).

% These are the actual governing bodies of entities that meet the Montevideo criteria but are denied recognition by existing states. They bear the costs of non-statehood (e.g., limited access to international institutions, trade barriers, security vulnerabilities) due to the gap between the declaratory principle and political practice.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, de_facto_authorities_meeting_criteria, payer,
    powerless, immediate, trapped, regional).

% These states, often powerful, deny recognition to entities that meet the Montevideo criteria, asserting a political prerogative over legal fact. They benefit from maintaining leverage over new state formation, despite the declaratory principle.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, existing_states_denying_recognition, agenda_setter,
    institutional, generational, mobile, global).

% They analyze and advocate for the consistent application of international law, including the declaratory principle. They benefit from a clear, objective framework for statehood, which simplifies legal analysis and reduces political maneuvering, but their influence is often limited by state practice.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, international_legal_scholars, observer,
    analytical, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(montevideo_statehood_criteria__declaratory_reading, international_legal_scholars, beneficiary).

% Organizations like the UN use statehood criteria for membership and engagement. While they often uphold the declaratory principle in theory, their actions can be constrained by the political will of member states, leading to inconsistent application.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, international_organizations, agenda_setter,
    institutional, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To provide an objective, universally applicable legal standard for determining statehood, aiming to reduce arbitrary political decisions and foster stability in international relations by grounding state existence in fact rather than recognition.
% TRANSFER_FUNCTION: Transfers the power to determine statehood from the subjective political will of existing states to objective legal criteria. However, in practice, this transfer is often incomplete, leading to costs borne by entities denied recognition.
% ABSENT_VOICES: De facto authorities that meet the Montevideo criteria but are denied recognition by powerful states; they would argue for the immediate and full application of the declaratory principle and their rightful place in the international community.
% DISAPPEARANCE_RATIONALE: If the declaratory principle vanished overnight, statehood would revert entirely to a political act of recognition, leading to greater instability, arbitrary power, prolonged conflicts over territorial claims, and a significant increase in the costs borne by entities seeking statehood without political backing.
% FOUNDING_PROBLEM: To establish a clear, objective, and depoliticized basis for statehood in international law, preventing powerful states from arbitrarily denying the existence of new states and ensuring a more stable and just international order.
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars, newly independent states, and some UN General Assembly resolutions corroborate the ongoing need for objective criteria to counter political opportunism and arbitrary denial of statehood. The persistent tension between legal fact and political recognition demonstrates the problem remains live.
narrative_ontology:disappearance_verdict(montevideo_statehood_criteria__declaratory_reading, world_rearranges).
narrative_ontology:founding_problem_status(montevideo_statehood_criteria__declaratory_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(montevideo_statehood_criteria__declaratory_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(montevideo_statehood_criteria__declaratory_reading, 'none', 1).
narrative_ontology:epsilon_provenance(montevideo_statehood_criteria__declaratory_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(montevideo_statehood_criteria__declaratory_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(montevideo_statehood_criteria__declaratory_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(montevideo_statehood_criteria__declaratory_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.65) and suppression (0.70) are substantial because, despite the legal principle, powerful existing states frequently deny recognition to entities that meet the criteria, imposing significant costs and actively suppressing their full participation in the international system. The theater ratio (0.20) is relatively low, as the criteria themselves are clear, but the performance lies in the selective application or disregard of the principle. The claimed type is Tangled Rope because it offers a genuine coordination function (objective criteria for statehood) but is marred by asymmetric extraction where entities meeting the criteria are still denied full statehood by powerful actors, requiring active enforcement (advocacy, legal challenges) to uphold.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of newly emerging states and de facto authorities, the declaratory principle is a crucial legal shield against arbitrary political power. From the perspective of existing states denying recognition, their actions are legitimate exercises of sovereignty, and the declaratory principle is either secondary or subject to political interpretation. The engine's computation of per-seat classifications will highlight this divergence, showing the principle as a benefit to some and a source of contested obligation for others.
 *
 * DIRECTIONALITY LOGIC:
 *   Newly emerging states are beneficiaries when the principle is applied, as it grants them legal status. De facto authorities meeting criteria are victims, as they bear the costs of non-recognition despite fulfilling the legal requirements. Existing states denying recognition act as agenda-setters, benefiting from the political leverage derived from withholding recognition. International legal scholars and organizations serve as observers and secondary beneficiaries, advocating for the principle's consistent application.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate of the declaratory principle — to depoliticize statehood — is still very much live, as evidenced by ongoing conflicts over recognition. However, its full realization is contested. The classification as a Tangled Rope prevents mislabeling it as a pure Rope (which would ignore the substantial extraction from entities denied recognition) or a Snare (which would ignore its genuine coordination function and legal grounding). The persistence of the 'founding problem' (arbitrary denial of statehood) indicates no mandatrophy, but rather an ongoing struggle for the constraint's full implementation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    declaratory_vs_constitutive_effectiveness,
    'Is the declaratory principle genuinely self-executing in international law, or does its effectiveness still rely on a degree of implicit or explicit recognition from existing states to be practically meaningful?',
    'Empirical analysis of the practical consequences for entities that meet the Montevideo criteria but lack widespread recognition (e.g., access to treaties, international organizations, diplomatic relations). If practical statehood is consistently denied, it suggests a de facto constitutive element persists.',
    'If effectiveness is recognition-dependent, the constraint''s actual extractiveness and suppression are higher than its ideal form suggests, reinforcing its Tangled Rope nature. If it is truly self-executing, its extractiveness would be lower, pushing it closer to a Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(declaratory_vs_constitutive_effectiveness, empirical, 'The practical versus theoretical efficacy of the declaratory principle.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of new states (denial of recognition despite meeting criteria) primarily structural (due to the power of existing states to block participation) or partly internalized (de facto authorities accepting their non-state status due to perceived futility of resistance)?',
    'Post-denial trajectory of de facto authorities: if they continue to operate as states and build parallel international relations despite non-recognition, the internalized component is low. If they cease to function or seek integration into existing states, internalization is higher.',
    'If internalized suppression is significant, the effective suppression is higher than the structural measure suggests, as the target carries the suppression with them. If purely structural, removing the external barriers would immediately alter the constraint''s impact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in statehood denial.').

omega_variable(
    kernel_context_ambiguity,
    'This constraint is the declaratory reading of the Montevideo Statehood Criteria kernel, asserting that statehood is a legal fact upon meeting objective criteria, independent of recognition. Sibling readings include the constitutive (recognition-dependent) and hybrid (objective + normative legitimacy) theories. What would change if a sibling reading were adopted?',
    'Conceptual analysis of legal frameworks and state practice under alternative readings. The structural delta for this reading is: de facto authorities enter victim set under recognition denial; parent states lose structural leverage to condition recognition; international law becomes self-executing rather than consensus-dependent.',
    'Adopting the constitutive reading would increase the power of existing states and the extraction from new entities. Adopting the hybrid reading would introduce additional normative criteria, potentially shifting the victim set to entities that meet objective criteria but fail normative ones.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_context_ambiguity, conceptual, 'Impact of adopting a sibling reading of the Montevideo Statehood Criteria.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(montevideo_statehood_criteria__declaratory_reading, 1933, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mont_tr_t1933, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 1933, 0.1).
narrative_ontology:measurement(mont_tr_t1953, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 1953, 0.12).
narrative_ontology:measurement(mont_tr_t1973, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 1973, 0.15).
narrative_ontology:measurement(mont_tr_t1993, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 1993, 0.18).
narrative_ontology:measurement(mont_tr_t2013, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 2013, 0.19).
narrative_ontology:measurement(mont_tr_t2023, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 2023, 0.2).

% Extraction over time
narrative_ontology:measurement(mont_be_t1933, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 1933, 0.5).
narrative_ontology:measurement(mont_be_t1953, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 1953, 0.55).
narrative_ontology:measurement(mont_be_t1973, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 1973, 0.6).
narrative_ontology:measurement(mont_be_t1993, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 1993, 0.63).
narrative_ontology:measurement(mont_be_t2013, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 2013, 0.64).
narrative_ontology:measurement(mont_be_t2023, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 2023, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(mont_su_t1933, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 1933, 0.55).
narrative_ontology:measurement(mont_su_t1953, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 1953, 0.6).
narrative_ontology:measurement(mont_su_t1973, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 1973, 0.65).
narrative_ontology:measurement(mont_su_t1993, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 1993, 0.68).
narrative_ontology:measurement(mont_su_t2013, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 2013, 0.69).
narrative_ontology:measurement(mont_su_t2023, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 2023, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(montevideo_statehood_criteria__declaratory_reading, information_standard).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__declaratory_reading, international_recognition_regime).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__declaratory_reading, self_determination_principle).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__declaratory_reading, constitutive_reading).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__declaratory_reading, hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the Montevideo Statehood Criteria kernel, which also includes the constitutive and hybrid theories. Each reading presents a distinct structural claim about the nature of statehood and its relationship to international law and political power.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
