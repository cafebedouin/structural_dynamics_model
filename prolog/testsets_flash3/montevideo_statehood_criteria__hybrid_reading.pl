% ============================================================================
% CONSTRAINT STORY: montevideo_statehood_criteria__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_montevideo_statehood_criteria__hybrid_reading, []).

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
 *   constraint_id: montevideo_statehood_criteria__hybrid_reading
 *   human_readable: Montevideo Statehood Criteria: Hybrid Reading (Objective + Normative Legitimacy)
 *   domain: international_law/political_philosophy/state_theory
 *
 * SUMMARY:
 *   This constraint represents the 'hybrid reading' of the Montevideo
 *   Convention's statehood criteria, which posits that in addition to
 *   objective criteria (territory, population, government, capacity to enter
 *   relations), normative legitimacy (democratic governance, human rights,
 *   non-aggression) is also required for statehood. This reading is a Tangled
 *   Rope because it genuinely coordinates the international community's
 *   response to new state claims while simultaneously extracting from
 *   entities that do not conform to liberal democratic norms. Its persistence
 *   relies on active enforcement by powerful states and international bodies.
 *   The structural delta from other readings is significant: non-liberal
 *   secessionist movements enter the victim set, while liberal democratic
 *   states gain normative justification for recognition denial, and
 *   humanitarian intervention or regime change gain legal cover.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(montevideo_statehood_criteria__hybrid_reading, 0.65).
domain_priors:suppression_score(montevideo_statehood_criteria__hybrid_reading, 0.75).
domain_priors:theater_ratio(montevideo_statehood_criteria__hybrid_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(montevideo_statehood_criteria__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(montevideo_statehood_criteria__hybrid_reading, "Montevideo Statehood Criteria: Hybrid Reading (Objective + Normative Legitimacy)").
narrative_ontology:topic_domain(montevideo_statehood_criteria__hybrid_reading, "international_law/political_philosophy/state_theory").

domain_priors:requires_active_enforcement(montevideo_statehood_criteria__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(montevideo_statehood_criteria__hybrid_reading, 'f7760085-398e-4dc0-a018-12bb09368b32').
narrative_ontology:cs_kernel_codification('f7760085-398e-4dc0-a018-12bb09368b32', formalized).
narrative_ontology:cs_authority_grounding('f7760085-398e-4dc0-a018-12bb09368b32', lineage).
narrative_ontology:cs_interpretation_layer_present('f7760085-398e-4dc0-a018-12bb09368b32').
narrative_ontology:cs_reading_relation('f7760085-398e-4dc0-a018-12bb09368b32', montevideo_statehood_criteria__declaratory_reading, influences).
narrative_ontology:cs_reading_relation('f7760085-398e-4dc0-a018-12bb09368b32', montevideo_statehood_criteria__constitutive_reading, coexists_with).
narrative_ontology:cs_axiom('f7760085-398e-4dc0-a018-12bb09368b32', foundational, statehood_requires_democratic_legitimacy).
narrative_ontology:cs_axiom_status(statehood_requires_democratic_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('f7760085-398e-4dc0-a018-12bb09368b32', statehood_requires_democratic_legitimacy, deontological).
narrative_ontology:cs_axiom('f7760085-398e-4dc0-a018-12bb09368b32', foundational, human_rights_are_precondition_for_sovereignty).
narrative_ontology:cs_axiom_status(human_rights_are_precondition_for_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('f7760085-398e-4dc0-a018-12bb09368b32', human_rights_are_precondition_for_sovereignty, deontological).
narrative_ontology:cs_reference_frame('f7760085-398e-4dc0-a018-12bb09368b32', post_cold_war_liberal_order).
narrative_ontology:cs_drift_state('f7760085-398e-4dc0-a018-12bb09368b32', contemporary_multipolar_challenge, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('f7760085-398e-4dc0-a018-12bb09368b32', '').
narrative_ontology:cs_kernel_id(montevideo_statehood_criteria__hybrid_reading, montevideo_statehood_criteria).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__hybrid_reading, liberal_democratic_states).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__hybrid_reading, international_human_rights_advocates).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__hybrid_reading, non_liberal_secessionist_movements).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__hybrid_reading, states_with_authoritarian_regimes).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states benefit from a framework that legitimizes their values and provides a basis for denying recognition to entities that do not conform. They actively shape and enforce the normative criteria, using them to justify foreign policy decisions, including intervention or non-recognition.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, liberal_democratic_states, agenda_setter,
    institutional, generational, arbitrage, global).

% These movements may meet objective criteria for statehood (territory, population, government, capacity to enter relations) but are denied recognition due to lacking democratic governance or human rights records. Their aspirations for statehood are suppressed by the normative overlay, making their path to international legitimacy extremely difficult.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, non_liberal_secessionist_movements, payer,
    powerless, generational, trapped, regional).

% Advocates for human rights and democratic principles benefit from this reading as it embeds their values into the very definition of statehood, providing a powerful tool for promoting these norms globally and challenging regimes that violate them.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, international_human_rights_advocates, beneficiary,
    organized, generational, mobile, global).

% Existing states that do not adhere to democratic or human rights norms find their legitimacy challenged by this reading. While they may resist external pressure, the normative criteria provide a basis for sanctions, diplomatic isolation, or even intervention, constraining their sovereignty.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, states_with_authoritarian_regimes, payer,
    powerful, biographical, constrained, national).

% Scholars and states adhering to the declaratory reading argue that statehood is an objective fact once Montevideo's four criteria are met, regardless of normative judgments. They are excluded from the dominant discourse that incorporates normative legitimacy, as their view would undermine the justification for conditional recognition.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, declaratory_reading_adherents, excluded,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the international community's approach to state recognition by providing a framework that balances objective facts with shared normative values, aiming to promote a more just and stable international order.
% TRANSFER_FUNCTION: Transfers legitimacy and international standing from entities that fail to meet normative criteria (e.g., non-democratic secessionist movements) to those that uphold them (e.g., liberal democratic states), and provides a basis for intervention or non-recognition.
% ABSENT_VOICES: Adherents of a purely declaratory theory of statehood, who would argue that normative criteria are political impositions rather than legal requirements, are marginalized. Non-liberal secessionist movements, whose claims to self-determination are dismissed on normative grounds, also lack a voice in shaping these criteria.
% DISAPPEARANCE_RATIONALE: If this hybrid reading vanished, the international system would revert to a more purely objective or purely constitutive model of statehood. Liberal democratic states would lose a key justification for their foreign policy, and non-liberal entities might find it easier to gain recognition, leading to a significant shift in global power dynamics and normative expectations.
% FOUNDING_PROBLEM: The problem of recognizing entities that meet objective criteria but pose threats to international peace and security, or violate fundamental human rights, creating a tension between state sovereignty and universal values.
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars, human rights organizations, and a significant bloc of liberal democratic states corroborate that this problem remains live, citing ongoing conflicts and human rights abuses by non-state actors and authoritarian regimes. States adhering to a purely declaratory view might contest the 'problem' itself, viewing it as an overreach of normative power.
narrative_ontology:disappearance_verdict(montevideo_statehood_criteria__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(montevideo_statehood_criteria__hybrid_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(montevideo_statehood_criteria__hybrid_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(montevideo_statehood_criteria__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(montevideo_statehood_criteria__hybrid_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(montevideo_statehood_criteria__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(montevideo_statehood_criteria__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(montevideo_statehood_criteria__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is substantial because the normative criteria impose significant costs on entities that do not align with liberal democratic values, effectively denying them statehood or subjecting them to intervention. Suppression (0.75) is high due to the active diplomatic, economic, and sometimes military enforcement by powerful states to uphold these norms. Theater ratio (0.20) is relatively low, as the normative criteria are genuinely applied, though sometimes selectively. The increasing trend in extractiveness and suppression reflects the growing assertiveness of normative criteria in international law over time.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of liberal democratic states, this is a legitimate framework for promoting universal values and stability (a Rope or even a Mountain of moral truth). From the perspective of non-liberal secessionist movements or authoritarian states, it is a Snare designed to perpetuate the dominance of certain political ideologies and suppress alternative forms of governance. The engine's classification as Tangled Rope captures this dual nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Liberal democratic states and human rights advocates are beneficiaries, as the constraint aligns with and promotes their values, providing tools for influence and intervention. Non-liberal secessionist movements and authoritarian states are victims, as they bear the costs of non-recognition, sanctions, or intervention due to their failure to meet the normative criteria. The constraint subsidizes the former while extracting from the latter.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    normative_criteria_universality,
    'Are the ''normative legitimacy'' criteria (democratic governance, human rights) genuinely universal, or are they culturally specific values imposed by powerful states?',
    'Cross-cultural consensus studies on political legitimacy, or a shift in international power dynamics leading to the emergence of alternative, widely accepted normative frameworks for statehood.',
    'If culturally specific, the constraint''s extractiveness would be reclassified as higher (more arbitrary imposition), and its claimed coordination function would be seen as cover for power projection. If truly universal, its legitimacy as a coordination mechanism would be strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(normative_criteria_universality, conceptual, 'Ambiguity regarding the universality vs. cultural specificity of normative statehood criteria.').

omega_variable(
    intervention_legitimacy_boundary,
    'At what point do violations of normative criteria justify external intervention, and is this threshold consistently applied?',
    'Development of clear, internationally agreed-upon legal doctrines and consistent application in practice, or empirical analysis of intervention patterns revealing selective enforcement.',
    'Inconsistent application or a low threshold for intervention would increase the constraint''s perceived extractiveness and suppression for targeted states, potentially reclassifying it closer to a Snare. Consistent, high-threshold application would reinforce its Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intervention_legitimacy_boundary, empirical, 'Ambiguity regarding the threshold and consistency of intervention based on normative statehood criteria.').

omega_variable(
    secessionist_self_determination_balance,
    'How does the normative legitimacy requirement balance against the principle of self-determination for groups seeking statehood, especially when those groups do not adhere to liberal democratic norms?',
    'International legal rulings or political consensus that clarifies the hierarchy or conditions under which one principle takes precedence over the other, or a shift in the international community''s approach to self-determination.',
    'If self-determination is consistently subordinated to normative legitimacy, the constraint''s suppression for non-liberal secessionist movements is higher. If a balance is struck, the suppression might be lower, reflecting a more nuanced coordination function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(secessionist_self_determination_balance, preference, 'The tension between normative legitimacy criteria and the right to self-determination for non-liberal groups.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(montevideo_statehood_criteria__hybrid_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mont_tr_t0, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(mont_tr_t8, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 8, 0.12).
narrative_ontology:measurement(mont_tr_t16, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 16, 0.15).
narrative_ontology:measurement(mont_tr_t24, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 24, 0.18).
narrative_ontology:measurement(mont_tr_t32, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 32, 0.19).
narrative_ontology:measurement(mont_tr_t40, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 40, 0.2).

% Extraction over time
narrative_ontology:measurement(mont_be_t0, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(mont_be_t8, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 8, 0.55).
narrative_ontology:measurement(mont_be_t16, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 16, 0.6).
narrative_ontology:measurement(mont_be_t24, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 24, 0.63).
narrative_ontology:measurement(mont_be_t32, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 32, 0.64).
narrative_ontology:measurement(mont_be_t40, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 40, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(mont_su_t0, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(mont_su_t8, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 8, 0.65).
narrative_ontology:measurement(mont_su_t16, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 16, 0.7).
narrative_ontology:measurement(mont_su_t24, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 24, 0.73).
narrative_ontology:measurement(mont_su_t32, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 32, 0.74).
narrative_ontology:measurement(mont_su_t40, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 40, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(montevideo_statehood_criteria__hybrid_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__hybrid_reading, international_humanitarian_law).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__hybrid_reading, un_security_council_veto_power).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'Montevideo Statehood Criteria' kernel. It emphasizes both objective and normative criteria, influencing the application of international law and the justification for intervention. It coexists with the 'declaratory reading' and 'constitutive reading', which offer different bases for statehood.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
