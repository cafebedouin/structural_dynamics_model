% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__religious_covenant_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_self_determination__religious_covenant_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: jewish_self_determination__religious_covenant_reading
 *   human_readable: Jewish Self-Determination: Religious Covenant Reading
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   This constraint represents the reading of Jewish self-determination that
 *   grounds territorial claims in a divine covenant, making sovereignty a
 *   religious obligation. It is presented as a 'mountain' within its own
 *   theological framework (divine command is immutable), but its
 *   operationalization in a secular political context makes it function as a
 *   highly extractive and suppressive constraint. The high extractiveness and
 *   suppression reflect the costs imposed on those who do not share or are
 *   actively harmed by this theological claim, particularly the secular
 *   negotiation framework and Palestinian self-determination. The
 *   'emerges_naturally: true' reflects the internal theological claim of
 *   divine origin, which is then contested by the omegas.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__religious_covenant_reading, 0.85).
domain_priors:suppression_score(jewish_self_determination__religious_covenant_reading, 0.9).
domain_priors:theater_ratio(jewish_self_determination__religious_covenant_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__religious_covenant_reading, mountain).
narrative_ontology:human_readable(jewish_self_determination__religious_covenant_reading, "Jewish Self-Determination: Religious Covenant Reading").
narrative_ontology:topic_domain(jewish_self_determination__religious_covenant_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_self_determination__religious_covenant_reading).
domain_priors:emerges_naturally(jewish_self_determination__religious_covenant_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__religious_covenant_reading, 'fb43c1ed-3664-419d-bd1d-cdb0c7d8ac7b').
narrative_ontology:cs_kernel_codification('fb43c1ed-3664-419d-bd1d-cdb0c7d8ac7b', fixed_text).
narrative_ontology:cs_authority_grounding('fb43c1ed-3664-419d-bd1d-cdb0c7d8ac7b', lineage).
narrative_ontology:cs_interpretation_layer_present('fb43c1ed-3664-419d-bd1d-cdb0c7d8ac7b').
narrative_ontology:cs_reading_relation('fb43c1ed-3664-419d-bd1d-cdb0c7d8ac7b', jewish_self_determination__liberal_nationalist_reading, influences).
narrative_ontology:cs_reading_relation('fb43c1ed-3664-419d-bd1d-cdb0c7d8ac7b', jewish_self_determination__indigenous_return_reading, forecloses).
narrative_ontology:cs_reading_relation('fb43c1ed-3664-419d-bd1d-cdb0c7d8ac7b', jewish_self_determination__settler_colonial_reading, forecloses).
narrative_ontology:cs_reading_relation('fb43c1ed-3664-419d-bd1d-cdb0c7d8ac7b', jewish_self_determination__diasporist_reading, coexists_with).
narrative_ontology:cs_axiom('fb43c1ed-3664-419d-bd1d-cdb0c7d8ac7b', foundational, divine_covenant_grants_land).
narrative_ontology:cs_axiom_status(divine_covenant_grants_land, holdable).
narrative_ontology:cs_axiom_grounding('fb43c1ed-3664-419d-bd1d-cdb0c7d8ac7b', divine_covenant_grants_land, theological).
narrative_ontology:cs_axiom('fb43c1ed-3664-419d-bd1d-cdb0c7d8ac7b', foundational, territorial_sovereignty_is_religious_obligation).
narrative_ontology:cs_axiom_status(territorial_sovereignty_is_religious_obligation, holdable).
narrative_ontology:cs_axiom_grounding('fb43c1ed-3664-419d-bd1d-cdb0c7d8ac7b', territorial_sovereignty_is_religious_obligation, deontological).
narrative_ontology:cs_reference_frame('fb43c1ed-3664-419d-bd1d-cdb0c7d8ac7b', biblical_covenant_fulfillment).
narrative_ontology:cs_drift_state('fb43c1ed-3664-419d-bd1d-cdb0c7d8ac7b', contemporary_international_relations, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('fb43c1ed-3664-419d-bd1d-cdb0c7d8ac7b', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__religious_covenant_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__religious_covenant_reading, religious_zionist_movement).
narrative_ontology:constraint_beneficiary(jewish_self_determination__religious_covenant_reading, settlement_enterprise).
narrative_ontology:constraint_victim(jewish_self_determination__religious_covenant_reading, secular_territorial_negotiation_framework).
narrative_ontology:constraint_victim(jewish_self_determination__religious_covenant_reading, palestinian_self_determination).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__religious_covenant_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(jewish_self_determination__religious_covenant_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_self_determination__religious_covenant_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_self_determination__religious_covenant_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, ExtMetricName, E),
    domain_priors:suppression_score(jewish_self_determination__religious_covenant_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(jewish_self_determination__religious_covenant_reading),
    narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(jewish_self_determination__religious_covenant_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.85) is high because the absolute nature of the divine claim forecloses compromise and imposes a non-negotiable framework on others. Suppression (0.90) is severe as it actively delegitimizes and suppresses alternative secular or indigenous claims to the same territory. Theater ratio (0.20) is low because the religious conviction is genuine, but some political actions taken in its name may have performative elements. Accessibility collapse is very high (0.95) because, within this framework, no legitimate alternative to the divine mandate exists. Resistance is high (0.75) due to the ongoing conflict with those whose claims are suppressed.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the religious Zionist movement, this is a divinely ordained, immutable 'mountain' that coordinates a sacred mission. From the perspective of secular negotiation frameworks and Palestinian self-determination, it operates as a 'snare' or 'tangled rope' that extracts land and suppresses rights through an actively enforced, non-negotiable claim. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The religious Zionist movement and settlement enterprise are clear beneficiaries (d=0.0-0.1) as they gain legitimacy and material support from this reading. The secular territorial negotiation framework and Palestinian self-determination are clear victims (d=0.9-1.0) as their claims are directly undermined and suppressed. International law bodies are observers (d=0.5) attempting to apply a different framework, while diaspora Jewish communities are often excluded (d=0.7-0.8) if their views diverge from the dominant religious Zionist narrative.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (fulfilling the divine covenant) is considered 'live' within its own framework, preventing a mandatrophy resolution from within. However, the high extractiveness and suppression, coupled with the 'contested' status of the founding problem, indicate that from an external, secular perspective, the constraint functions as a highly extractive mechanism that has outlived its original coordination function (if any, for non-adherents) and now primarily serves to maintain a power asymmetry.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_vs_secular_legitimacy,
    'Is the divine covenant a legitimate basis for territorial sovereignty in a pluralistic international system, or is it a theological claim that cannot supersede secular legal frameworks?',
    'No empirical resolution; depends on a conceptual/normative choice regarding the hierarchy of legal and moral authority (theological vs. international law).',
    'If divine covenant is accepted as supreme, the constraint is a genuine mountain; if secular law is supreme, it functions as a snare or tangled rope, with the ''mountain'' claim serving as cover.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(divine_vs_secular_legitimacy, conceptual, 'The fundamental conflict between theological and secular claims to legitimacy.').

omega_variable(
    identity_lock_vs_coercion,
    'To what extent is the ''identity_locked'' exit option for the religious Zionist movement a genuine internal commitment, versus a product of external political and social coercion that makes alternative identities unthinkable?',
    'Sociological studies of identity formation in highly politicized religious communities, examining the role of state incentives, social pressure, and educational systems in reinforcing the ''identity_locked'' status.',
    'If primarily internal, the identity_lock is a feature of the agent''s genuine commitment. If substantially external, the ''identity_locked'' status is itself a form of suppression, increasing the effective extractiveness of the constraint on the agent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_coercion, empirical, 'Distinguishing genuine identity fusion from coerced identity.').

omega_variable(
    mountain_or_snare_of_policy,
    'Is this constraint a genuine ''mountain'' of divine command, or a ''snare'' of policy that leverages religious belief to achieve political ends?',
    'Analysis of the historical evolution of the ''divine covenant'' claim in relation to political and territorial expansion, examining whether the theological interpretation adapted to political opportunities or genuinely drove them.',
    'If primarily a mountain, its extractiveness is an inherent cost of a divine mandate. If primarily a snare, its extractiveness is a product of human agency and power, making it amenable to political resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mountain_or_snare_of_policy, empirical, 'Ambiguity between natural law and constructed policy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__religious_covenant_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1948, jewish_self_determination__religious_covenant_reading, theater_ratio, 1948, 0.1).
narrative_ontology:measurement(jewi_tr_t1967, jewish_self_determination__religious_covenant_reading, theater_ratio, 1967, 0.12).
narrative_ontology:measurement(jewi_tr_t1987, jewish_self_determination__religious_covenant_reading, theater_ratio, 1987, 0.15).
narrative_ontology:measurement(jewi_tr_t2000, jewish_self_determination__religious_covenant_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(jewi_tr_t2010, jewish_self_determination__religious_covenant_reading, theater_ratio, 2010, 0.19).
narrative_ontology:measurement(jewi_tr_t2024, jewish_self_determination__religious_covenant_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1948, jewish_self_determination__religious_covenant_reading, base_extractiveness, 1948, 0.6).
narrative_ontology:measurement(jewi_be_t1967, jewish_self_determination__religious_covenant_reading, base_extractiveness, 1967, 0.7).
narrative_ontology:measurement(jewi_be_t1987, jewish_self_determination__religious_covenant_reading, base_extractiveness, 1987, 0.78).
narrative_ontology:measurement(jewi_be_t2000, jewish_self_determination__religious_covenant_reading, base_extractiveness, 2000, 0.82).
narrative_ontology:measurement(jewi_be_t2010, jewish_self_determination__religious_covenant_reading, base_extractiveness, 2010, 0.84).
narrative_ontology:measurement(jewi_be_t2024, jewish_self_determination__religious_covenant_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1948, jewish_self_determination__religious_covenant_reading, suppression_requirement, 1948, 0.7).
narrative_ontology:measurement(jewi_su_t1967, jewish_self_determination__religious_covenant_reading, suppression_requirement, 1967, 0.78).
narrative_ontology:measurement(jewi_su_t1987, jewish_self_determination__religious_covenant_reading, suppression_requirement, 1987, 0.85).
narrative_ontology:measurement(jewi_su_t2000, jewish_self_determination__religious_covenant_reading, suppression_requirement, 2000, 0.88).
narrative_ontology:measurement(jewi_su_t2010, jewish_self_determination__religious_covenant_reading, suppression_requirement, 2010, 0.89).
narrative_ontology:measurement(jewi_su_t2024, jewish_self_determination__religious_covenant_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__religious_covenant_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, jewish_self_determination__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, jewish_self_determination__indigenous_return_reading).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, jewish_self_determination__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, jewish_self_determination__diasporist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'jewish_self_determination' kernel. Its high extractiveness and suppression stem from its absolute theological claims, which directly conflict with secular and alternative indigenous claims to the same territory. Other readings of this kernel offer different justifications and outcomes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
