% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__diasporist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_self_determination__diasporist_reading, []).

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
 *   constraint_id: jewish_self_determination__diasporist_reading
 *   human_readable: Diasporist Reading of Jewish Self-Determination
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   This constraint represents the diasporist reading of Jewish
 *   self-determination, which posits that Jewish collective survival and
 *   flourishing are best secured through diaspora pluralism and minority
 *   rights, rather than territorial sovereignty. It views Zionism as a
 *   dangerous deviation that ties Jewish fate to a militarized state,
 *   endangering Jews globally. The constraint is classified as a Piton
 *   because the diasporist alternative, while historically significant, has
 *   atrophied under the hegemony of Zionist narratives, persisting more
 *   through inertial intellectual and cultural maintenance than active,
 *   widespread adoption as a primary political project. Its benefits are
 *   diffuse, and its costs are borne by those whose Jewish identity is
 *   constrained or endangered by the conflation with Zionism.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__diasporist_reading, 0.55).
domain_priors:suppression_score(jewish_self_determination__diasporist_reading, 0.65).
domain_priors:theater_ratio(jewish_self_determination__diasporist_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__diasporist_reading, piton).
narrative_ontology:human_readable(jewish_self_determination__diasporist_reading, "Diasporist Reading of Jewish Self-Determination").
narrative_ontology:topic_domain(jewish_self_determination__diasporist_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_self_determination__diasporist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__diasporist_reading, '57f67a84-b3a3-48c6-9e8e-f6634f937bc6').
narrative_ontology:cs_kernel_codification('57f67a84-b3a3-48c6-9e8e-f6634f937bc6', distributed).
narrative_ontology:cs_authority_grounding('57f67a84-b3a3-48c6-9e8e-f6634f937bc6', diffuse_epistemic).
narrative_ontology:cs_reading_relation('57f67a84-b3a3-48c6-9e8e-f6634f937bc6', jewish_self_determination__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('57f67a84-b3a3-48c6-9e8e-f6634f937bc6', jewish_self_determination__indigenous_return_reading, coexists_with).
narrative_ontology:cs_reading_relation('57f67a84-b3a3-48c6-9e8e-f6634f937bc6', jewish_self_determination__settler_colonial_reading, coexists_with).
narrative_ontology:cs_reading_relation('57f67a84-b3a3-48c6-9e8e-f6634f937bc6', jewish_self_determination__religious_covenant_reading, coexists_with).
narrative_ontology:cs_axiom('57f67a84-b3a3-48c6-9e8e-f6634f937bc6', foundational, diaspora_pluralism_is_optimal_survival_strategy).
narrative_ontology:cs_axiom_status(diaspora_pluralism_is_optimal_survival_strategy, holdable).
narrative_ontology:cs_axiom_grounding('57f67a84-b3a3-48c6-9e8e-f6634f937bc6', diaspora_pluralism_is_optimal_survival_strategy, instrumental).
narrative_ontology:cs_axiom('57f67a84-b3a3-48c6-9e8e-f6634f937bc6', foundational, zionism_endangers_jews_globally).
narrative_ontology:cs_axiom_status(zionism_endangers_jews_globally, holdable).
narrative_ontology:cs_axiom_grounding('57f67a84-b3a3-48c6-9e8e-f6634f937bc6', zionism_endangers_jews_globally, empirically_contingent).
narrative_ontology:cs_reference_frame('57f67a84-b3a3-48c6-9e8e-f6634f937bc6', historical_diaspora_autonomy).
narrative_ontology:cs_drift_state('57f67a84-b3a3-48c6-9e8e-f6634f937bc6', contemporary_zionist_hegemony, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('57f67a84-b3a3-48c6-9e8e-f6634f937bc6', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__diasporist_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__diasporist_reading, diaspora_jewish_communities).
narrative_ontology:constraint_victim(jewish_self_determination__diasporist_reading, jews_coerced_into_zionism).
narrative_ontology:constraint_victim(jewish_self_determination__diasporist_reading, jews_endangered_by_israeli_actions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These communities thrive through pluralism and minority rights in host nations, maintaining distinct identities. They benefit from a framework that prioritizes integration and cultural preservation over territorial nationalism, but face pressure to align with Zionist narratives.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, diaspora_jewish_communities, beneficiary,
    organized, generational, constrained, global).

% Individuals whose Jewish identity is increasingly conflated with support for Zionism, leading to internal conflict or external pressure. They bear the cost of a narrowed definition of Jewishness and suppressed alternative political expressions.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, jews_coerced_into_zionism, payer,
    powerless, biographical, identity_locked, global).

% Jews in various diasporas who face increased antisemitism or security threats due to their perceived association with the actions of the Israeli state, despite not supporting Zionism. Their safety is compromised by the conflation of Jewish identity with a militarized state.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, jews_endangered_by_israeli_actions, payer,
    powerless, immediate, trapped, global).

% Organizations and states that actively promote Zionism as the primary or sole legitimate expression of Jewish self-determination. They benefit from the narrative that ties Jewish fate to a territorial state, often suppressing alternative Jewish political thought.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, zionist_institutions, agenda_setter,
    institutional, generational, arbitrage, global).

% Governments and societies where diaspora Jewish communities reside. They navigate the complexities of minority rights, national identity, and international relations, often influenced by the perceived alignment of Jewish communities with the Israeli state.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, host_nations, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for Jewish communities to navigate their existence as minorities within diverse nation-states, fostering cultural and religious continuity without requiring territorial sovereignty.
% TRANSFER_FUNCTION: Transfers political and cultural capital from a singular, territorial-nationalist vision of Jewish identity to a pluralistic, diaspora-centric one, aiming to secure minority rights and foster integration.
% ABSENT_VOICES: The voices of those who advocate for a singular, territorial Jewish state are actively marginalized or dismissed within this framework, as their vision is seen as a dangerous deviation. Also, those who have been coerced into the Zionist framework and cannot articulate alternatives.
% DISAPPEARANCE_RATIONALE: If the diasporist reading vanished, the dominant Zionist narrative would face even less intellectual and political challenge, potentially leading to further conflation of Jewish identity with the Israeli state, and diminishing the space for alternative Jewish political thought and practice.
% FOUNDING_PROBLEM: The historical vulnerability of Jewish communities to persecution and the challenge of maintaining Jewish identity and culture in diverse diasporic contexts.
% FOUNDING_PROBLEM_CORROBORATION: Historians of Jewish diaspora, scholars of minority rights, and various anti-Zionist Jewish organizations corroborate that the problems of antisemitism and cultural assimilation remain live, and that tying Jewish fate to a single state exacerbates rather than solves these issues.
narrative_ontology:disappearance_verdict(jewish_self_determination__diasporist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_self_determination__diasporist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__diasporist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(jewish_self_determination__diasporist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__diasporist_reading, 0.55, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_self_determination__diasporist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_self_determination__diasporist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_self_determination__diasporist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.55) reflects the cost of maintaining a diasporist identity in the face of dominant Zionist narratives, which often demand allegiance to a territorial state. Suppression (0.65) is high due to the active marginalization of anti-Zionist Jewish voices and the pressure to conform. The theater ratio (0.4) indicates that while the intellectual and cultural arguments for diasporism are still articulated, the political project itself is largely performative in the face of institutionalized Zionism. The accessibility collapse (0.45) is moderate, as intellectual alternatives exist, but practical political avenues for diasporist self-determination are severely constrained. Resistance (0.5) is present but fragmented.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Zionist institutions, the diasporist reading is a threat to Jewish security and continuity, an 'anti-national' deviation. From the diasporist perspective, Zionism itself is the deviation, creating new forms of vulnerability. The engine's classification of this reading as a Piton highlights the atrophied political power of the diasporist alternative, despite its intellectual coherence.
 *
 * DIRECTIONALITY LOGIC:
 *   Diaspora Jewish communities are beneficiaries, as this reading offers a path to flourishing without the risks of territorial nationalism. However, they are constrained by the broader political landscape. Jews coerced into Zionism and those endangered by Israeli actions are victims, bearing the direct costs of the conflation of Jewish identity with a state. Zionist institutions act as agenda-setters, actively suppressing alternative narratives and benefiting from the current framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The diasporist reading argues that the original mandate for Jewish survival and flourishing is best served by pluralism, and that Zionism represents a mandatrophy of this original goal by substituting a territorial-nationalist solution that creates new dangers. The Piton classification reflects that the diasporist alternative, while still intellectually live, has largely lost its institutional and political force, becoming a 'theatrical' or inertial position in the face of a dominant, actively enforced Zionist framework.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    diaspora_political_efficacy,
    'To what extent can diaspora Jewish communities effectively pursue their self-determination through pluralism and minority rights, given the global political dominance of the Zionist framework?',
    'Empirical analysis of policy outcomes in host nations regarding Jewish minority rights, and the success of non-Zionist Jewish political advocacy groups.',
    'If efficacy is low, the diasporist reading''s claimed benefits are largely theoretical, pushing it closer to a Snare for those who identify with it but find no political outlet. If efficacy is high, it strengthens the Rope-like aspects of the claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diaspora_political_efficacy, empirical, 'The actual political power of diasporist approaches versus the hegemonic Zionist narrative.').

omega_variable(
    identity_coercion_vs_choice,
    'Is the conflation of Jewish identity with Zionism a result of active coercion by Zionist institutions, or a voluntary choice by a majority of Jews?',
    'Sociological studies of Jewish identity formation, surveys on political attitudes within Jewish communities, and analysis of funding flows to Jewish organizations.',
    'If coercion is dominant, the extractiveness and suppression metrics are accurate, and the constraint leans more towards a Snare. If voluntary choice is dominant, the constraint might be closer to a Tangled Rope, reflecting a coordination around a particular identity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_coercion_vs_choice, empirical, 'Distinguishing between structural coercion and individual agency in Jewish identity formation.').

omega_variable(
    mandatrophy_of_diasporism,
    'Has the diasporist political project truly atrophied to a Piton, or does it retain latent capacity for resurgence as a viable alternative?',
    'Analysis of historical periods of diasporist resurgence, growth in non-Zionist Jewish organizations, and shifts in global geopolitical dynamics that might favor pluralistic approaches.',
    'If latent capacity is significant, the Piton classification might understate its potential, suggesting it could transition to a Scaffold (temporary support for a transition) or even a Rope if conditions change. If atrophy is confirmed, the Piton classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mandatrophy_of_diasporism, conceptual, 'Assessing the true political vitality and potential for resurgence of the diasporist project.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__diasporist_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1948, jewish_self_determination__diasporist_reading, theater_ratio, 1948, 0.2).
narrative_ontology:measurement(jewi_tr_t1967, jewish_self_determination__diasporist_reading, theater_ratio, 1967, 0.25).
narrative_ontology:measurement(jewi_tr_t1987, jewish_self_determination__diasporist_reading, theater_ratio, 1987, 0.3).
narrative_ontology:measurement(jewi_tr_t2000, jewish_self_determination__diasporist_reading, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(jewi_tr_t2014, jewish_self_determination__diasporist_reading, theater_ratio, 2014, 0.38).
narrative_ontology:measurement(jewi_tr_t2024, jewish_self_determination__diasporist_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1948, jewish_self_determination__diasporist_reading, base_extractiveness, 1948, 0.4).
narrative_ontology:measurement(jewi_be_t1967, jewish_self_determination__diasporist_reading, base_extractiveness, 1967, 0.45).
narrative_ontology:measurement(jewi_be_t1987, jewish_self_determination__diasporist_reading, base_extractiveness, 1987, 0.5).
narrative_ontology:measurement(jewi_be_t2000, jewish_self_determination__diasporist_reading, base_extractiveness, 2000, 0.52).
narrative_ontology:measurement(jewi_be_t2014, jewish_self_determination__diasporist_reading, base_extractiveness, 2014, 0.54).
narrative_ontology:measurement(jewi_be_t2024, jewish_self_determination__diasporist_reading, base_extractiveness, 2024, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1948, jewish_self_determination__diasporist_reading, suppression_requirement, 1948, 0.5).
narrative_ontology:measurement(jewi_su_t1967, jewish_self_determination__diasporist_reading, suppression_requirement, 1967, 0.55).
narrative_ontology:measurement(jewi_su_t1987, jewish_self_determination__diasporist_reading, suppression_requirement, 1987, 0.6).
narrative_ontology:measurement(jewi_su_t2000, jewish_self_determination__diasporist_reading, suppression_requirement, 2000, 0.62).
narrative_ontology:measurement(jewi_su_t2014, jewish_self_determination__diasporist_reading, suppression_requirement, 2014, 0.64).
narrative_ontology:measurement(jewi_su_t2024, jewish_self_determination__diasporist_reading, suppression_requirement, 2024, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__diasporist_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_self_determination__diasporist_reading, jewish_self_determination__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__diasporist_reading, jewish_self_determination__indigenous_return_reading).
narrative_ontology:affects_constraint(jewish_self_determination__diasporist_reading, jewish_self_determination__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_self_determination__diasporist_reading, jewish_self_determination__religious_covenant_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of five readings of the 'jewish_self_determination' kernel. Each reading offers a distinct framework for Jewish collective identity and political action, with differing beneficiaries, victims, and structural properties. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
