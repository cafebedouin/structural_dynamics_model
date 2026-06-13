% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__diasporist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
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
    constraint_indexing:constraint_classification/3,
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
 *   This constraint story instantiates the 'diasporist_reading' of the
 *   'jewish_self_determination' kernel. It argues that Jewish collective
 *   survival and flourishing are best secured through diaspora pluralism and
 *   minority rights, not territorial sovereignty, and views Zionism as a
 *   dangerous deviation that ties Jewish fate to a militarized state. The
 *   constraint operates as a Piton because the alternative (robust diaspora
 *   institutions and political frameworks) has atrophied under the hegemony
 *   of Zionist discourse, which has effectively monopolized the definition of
 *   'Jewish interest' and 'Jewish self-determination.'
 *
 * KEY AGENTS:
 *   - diaspora_jewish_communities: Beneficiary (organized/constrained) — benefit from pluralism, but their institutions are weakened.
 *   - jews_coerced_into_zionism: Payer (powerless/identity_locked) — pressured to conform to Zionist narrative.
 *   - jews_endangered_by_israel_actions: Payer (powerless/trapped) — bear costs of association with Israeli state.
 *   - zionist_organizations: Agenda Setter (institutional/arbitrage) — actively promote Zionism and suppress alternatives.
 *   - host_nations: Observer (institutional/analytical) — navigate complex relations with Israel and domestic Jewish communities.
 *   - diaspora_institutions: Victim (organized/constrained) — struggle for legitimacy and resources against Zionist hegemony.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__diasporist_reading, 0.55).
domain_priors:suppression_score(jewish_self_determination__diasporist_reading, 0.65).
domain_priors:theater_ratio(jewish_self_determination__diasporist_reading, 0.7).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, theater_ratio, 0.7).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__diasporist_reading, piton).
narrative_ontology:human_readable(jewish_self_determination__diasporist_reading, "Diasporist Reading of Jewish Self-Determination").
narrative_ontology:topic_domain(jewish_self_determination__diasporist_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_self_determination__diasporist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__diasporist_reading, '4e40315f-2dee-44dc-9662-2036df91b566').
narrative_ontology:cs_kernel_codification('4e40315f-2dee-44dc-9662-2036df91b566', distributed).
narrative_ontology:cs_authority_grounding('4e40315f-2dee-44dc-9662-2036df91b566', extraction).
narrative_ontology:cs_interpretation_layer_present('4e40315f-2dee-44dc-9662-2036df91b566').
narrative_ontology:cs_reading_relation('4e40315f-2dee-44dc-9662-2036df91b566', jewish_self_determination__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('4e40315f-2dee-44dc-9662-2036df91b566', jewish_self_determination__indigenous_return_reading, coexists_with).
narrative_ontology:cs_reading_relation('4e40315f-2dee-44dc-9662-2036df91b566', jewish_self_determination__settler_colonial_reading, coexists_with).
narrative_ontology:cs_reading_relation('4e40315f-2dee-44dc-9662-2036df91b566', jewish_self_determination__religious_covenant_reading, coexists_with).
narrative_ontology:cs_axiom('4e40315f-2dee-44dc-9662-2036df91b566', foundational, diaspora_pluralism_is_primary_survival_strategy).
narrative_ontology:cs_axiom_status(diaspora_pluralism_is_primary_survival_strategy, holdable).
narrative_ontology:cs_axiom_grounding('4e40315f-2dee-44dc-9662-2036df91b566', diaspora_pluralism_is_primary_survival_strategy, empirically_contingent).
narrative_ontology:cs_axiom('4e40315f-2dee-44dc-9662-2036df91b566', foundational, territorial_sovereignty_is_dangerous_deviation).
narrative_ontology:cs_axiom_status(territorial_sovereignty_is_dangerous_deviation, holdable).
narrative_ontology:cs_axiom_grounding('4e40315f-2dee-44dc-9662-2036df91b566', territorial_sovereignty_is_dangerous_deviation, empirically_contingent).
narrative_ontology:cs_reference_frame('4e40315f-2dee-44dc-9662-2036df91b566', historical_diaspora_flourishing).
narrative_ontology:cs_drift_state('4e40315f-2dee-44dc-9662-2036df91b566', post_1948_zionist_hegemony, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4e40315f-2dee-44dc-9662-2036df91b566', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__diasporist_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__diasporist_reading, diaspora_jewish_communities).
narrative_ontology:constraint_victim(jewish_self_determination__diasporist_reading, jews_coerced_into_zionism).
narrative_ontology:constraint_victim(jewish_self_determination__diasporist_reading, jews_endangered_by_israel_actions).
narrative_ontology:constraint_victim(jewish_self_determination__diasporist_reading, diaspora_institutions).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__diasporist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(jewish_self_determination__diasporist_reading, 'none', 1).

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
 *   The extractiveness (0.55) reflects the cost to diaspora communities of having their identity and political agency co-opted or constrained by a territorial project they may not support. Suppression (0.65) is high due to the active efforts by Zionist organizations to marginalize alternative Jewish political expressions and to conflate Jewish identity with the Israeli state, making it difficult for individuals to dissent without being accused of disloyalty or antisemitism. The high theater ratio (0.7) indicates that much of the 'coordination' around Jewish self-determination is performative maintenance of a singular, state-centric narrative, while the actual function of securing diverse Jewish futures through pluralism has atrophied. The dip in extractiveness and suppression in 2024 reflects a recent, growing public challenge to Zionist hegemony within some Jewish communities, creating a slight opening for diasporist alternatives, though the overall structure remains a Piton.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Zionist organizations (agenda_setter), the constraint is a necessary Rope or even Mountain, ensuring Jewish survival through a strong state. From the perspective of diaspora Jewish communities and individuals (beneficiary/payer/victim), it is a Piton or Snare, where the original mandate of securing Jewish flourishing has been co-opted by a narrow, territorial nationalism that actively suppresses alternatives and endangers them.
 *
 * DIRECTIONALITY LOGIC:
 *   Zionist organizations are the primary beneficiaries (d=0.0-0.1) as they control the narrative and resources. Diaspora Jewish communities are beneficiaries of the pluralist ideal but victims of its suppression (d=0.4-0.6). Jews coerced into Zionism and those endangered by Israeli actions are clear targets (d=0.8-1.0). Host nations are observers (d=0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a Piton because the original mandate of securing diverse Jewish flourishing has atrophied. The 'coordination' function of a unified Jewish identity has been captured by a specific political project (Zionism), which now primarily serves to extract political and financial support while suppressing alternative forms of Jewish self-determination. The constraint persists not because it genuinely benefits all Jews, but due to institutional inertia, the power of Zionist organizations, and the difficulty of articulating and mobilizing around a robust diasporist alternative. The high theater ratio reflects the performative maintenance of a 'unified Jewish front' that masks internal dissent and the costs borne by many diaspora Jews.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    diaspora_institutional_strength,
    'To what extent have diaspora Jewish institutions genuinely atrophied, versus merely being overshadowed by Zionist organizations?',
    'Empirical study of funding flows, membership numbers, and political influence of non-Zionist diaspora organizations over time, compared to Zionist counterparts.',
    'If diaspora institutions are found to be robust and independently influential, the ''piton'' classification might be too strong, suggesting a more ''tangled rope'' dynamic where coordination and extraction are more actively contested. If they are truly weak, the piton classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diaspora_institutional_strength, empirical, 'Assessing the actual strength of non-Zionist diaspora institutions.').

omega_variable(
    identity_coercion_vs_choice,
    'Is the alignment of Jewish identity with Zionism a result of genuine collective choice, or is it primarily driven by social and political coercion?',
    'Sociological surveys and qualitative studies exploring the motivations and pressures on Jewish individuals regarding their relationship to Zionism, particularly among younger generations and those in diverse political contexts.',
    'If coercion is the dominant factor, the suppression metric is accurate, and the constraint leans more towards a Snare. If genuine choice is prevalent, the suppression is lower, and the constraint might be closer to a Rope (albeit one with significant external pressures).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_coercion_vs_choice, empirical, 'Distinguishing between coerced and chosen alignment with Zionist identity.').

omega_variable(
    kernel_reading_disagreement_location,
    'This constraint is the ''diasporist_reading'' of the ''jewish_self_determination'' kernel. Where is the core disagreement with sibling readings located structurally?',
    'Comparative analysis of the foundational axioms and reference frames of each reading.',
    'The diasporist reading fundamentally disagrees with the territorial and nationalist premises of the liberal_nationalist_reading and indigenous_return_reading, and rejects the theological grounding of the religious_covenant_reading. It also directly opposes the settler-colonial_reading''s framing of Zionism as inherently colonial, instead viewing it as a dangerous deviation from Jewish historical practice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Structural location of disagreement between diasporist and sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__diasporist_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1948, jewish_self_determination__diasporist_reading, theater_ratio, 1948, 0.3).
narrative_ontology:measurement(jewi_tr_t1967, jewish_self_determination__diasporist_reading, theater_ratio, 1967, 0.4).
narrative_ontology:measurement(jewi_tr_t1987, jewish_self_determination__diasporist_reading, theater_ratio, 1987, 0.5).
narrative_ontology:measurement(jewi_tr_t2000, jewish_self_determination__diasporist_reading, theater_ratio, 2000, 0.6).
narrative_ontology:measurement(jewi_tr_t2010, jewish_self_determination__diasporist_reading, theater_ratio, 2010, 0.7).
narrative_ontology:measurement(jewi_tr_t2024, jewish_self_determination__diasporist_reading, theater_ratio, 2024, 0.7).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1948, jewish_self_determination__diasporist_reading, base_extractiveness, 1948, 0.4).
narrative_ontology:measurement(jewi_be_t1967, jewish_self_determination__diasporist_reading, base_extractiveness, 1967, 0.5).
narrative_ontology:measurement(jewi_be_t1987, jewish_self_determination__diasporist_reading, base_extractiveness, 1987, 0.55).
narrative_ontology:measurement(jewi_be_t2000, jewish_self_determination__diasporist_reading, base_extractiveness, 2000, 0.6).
narrative_ontology:measurement(jewi_be_t2010, jewish_self_determination__diasporist_reading, base_extractiveness, 2010, 0.65).
narrative_ontology:measurement(jewi_be_t2024, jewish_self_determination__diasporist_reading, base_extractiveness, 2024, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1948, jewish_self_determination__diasporist_reading, suppression_requirement, 1948, 0.5).
narrative_ontology:measurement(jewi_su_t1967, jewish_self_determination__diasporist_reading, suppression_requirement, 1967, 0.6).
narrative_ontology:measurement(jewi_su_t1987, jewish_self_determination__diasporist_reading, suppression_requirement, 1987, 0.65).
narrative_ontology:measurement(jewi_su_t2000, jewish_self_determination__diasporist_reading, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(jewi_su_t2010, jewish_self_determination__diasporist_reading, suppression_requirement, 2010, 0.75).
narrative_ontology:measurement(jewi_su_t2024, jewish_self_determination__diasporist_reading, suppression_requirement, 2024, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
