% ============================================================================
% CONSTRAINT STORY: sex_gender_category__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sex_gender_category__hybrid_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: sex_gender_category__hybrid_reading
 *   human_readable: Sex/Gender Category Membership via Medical Transition (Hybrid Reading)
 *   domain: social/legal/medical
 *
 * SUMMARY:
 *   The hybrid reading instantiates sex/gender category membership as
 *   conditional on a combination of birth-assigned sex and completion of
 *   medical transition. Trans women who undergo hormone therapy and/or
 *   surgery gain legal and institutional recognition as female; those who do
 *   not transition remain categorized according to birth sex regardless of
 *   identity. This reading concentrates institutional authority in medical
 *   gatekeeping bodies (endocrinology, psychiatry, surgery) and legal
 *   institutions that recognize medical transition as proof of category
 *   membership. The constraint is presented as pragmatic (legible
 *   administrative criterion) but operates as extractive: it moves the burden
 *   of category entry to individual medical cost-bearing, excludes those
 *   without access to or willingness to pursue medical transition, and
 *   concentrates institutional power in medical institutions that control the
 *   transition pathway. The kernel contest is among three readings of 'what
 *   makes someone female' — biology, hybrid (medical transition), and
 *   identity — none of which logically forecloses the others, but which
 *   coexist as competing institutional and political frameworks.
 *
 * KEY AGENTS:
 *   - trans_women_post_transition: gain conditional access; bear transition costs
 *   - trans_women_without_transition: excluded; bear non-recognition costs; identity-locked
 *   - medical_gatekeeping_institutions: control entry criteria; collect transition revenue
 *   - cisgender_women: retain costless category membership; some segments support the boundary gatekeeping
 *   - non_transitioning_trans_advocates: excluded from agenda-setting; contest the reading
 *   - biology_reading_advocates: excluded from the hybrid framework; contest it actively
 *   - legal_institutions: implement the medical gatekeeping via legal procedures
 *   - observer_comparative_analysts: map the reading against its siblings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sex_gender_category__hybrid_reading, 0.68).
domain_priors:suppression_score(sex_gender_category__hybrid_reading, 0.71).
domain_priors:theater_ratio(sex_gender_category__hybrid_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sex_gender_category__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(sex_gender_category__hybrid_reading, "Sex/Gender Category Membership via Medical Transition (Hybrid Reading)").
narrative_ontology:topic_domain(sex_gender_category__hybrid_reading, "social/legal/medical").

domain_priors:requires_active_enforcement(sex_gender_category__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sex_gender_category__hybrid_reading, 'b6549ff3-dd0d-4d5b-8092-2819bf1326f3').
narrative_ontology:cs_kernel_codification('b6549ff3-dd0d-4d5b-8092-2819bf1326f3', formalized).
narrative_ontology:cs_authority_grounding('b6549ff3-dd0d-4d5b-8092-2819bf1326f3', extraction).
narrative_ontology:cs_interpretation_layer_present('b6549ff3-dd0d-4d5b-8092-2819bf1326f3').
narrative_ontology:cs_reading_relation('b6549ff3-dd0d-4d5b-8092-2819bf1326f3', sex_gender_category__biology_reading, coexists_with).
narrative_ontology:cs_reading_relation('b6549ff3-dd0d-4d5b-8092-2819bf1326f3', sex_gender_category__identity_reading, coexists_with).
narrative_ontology:cs_axiom('b6549ff3-dd0d-4d5b-8092-2819bf1326f3', foundational, medical_transition_required_for_category_change).
narrative_ontology:cs_axiom_status(medical_transition_required_for_category_change, holdable).
narrative_ontology:cs_axiom_grounding('b6549ff3-dd0d-4d5b-8092-2819bf1326f3', medical_transition_required_for_category_change, conventional).
narrative_ontology:cs_axiom('b6549ff3-dd0d-4d5b-8092-2819bf1326f3', foundational, birth_sex_baseline_female_identity_insufficient).
narrative_ontology:cs_axiom_status(birth_sex_baseline_female_identity_insufficient, holdable).
narrative_ontology:cs_axiom_grounding('b6549ff3-dd0d-4d5b-8092-2819bf1326f3', birth_sex_baseline_female_identity_insufficient, deontological).
narrative_ontology:cs_reference_frame('b6549ff3-dd0d-4d5b-8092-2819bf1326f3', medical_transition_as_proof).
narrative_ontology:cs_drift_state('b6549ff3-dd0d-4d5b-8092-2819bf1326f3', contemporary_trans_rights_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b6549ff3-dd0d-4d5b-8092-2819bf1326f3', '').
narrative_ontology:cs_kernel_id(sex_gender_category__hybrid_reading, sex_gender_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sex_gender_category__hybrid_reading, medical_gatekeeping_institutions).
narrative_ontology:constraint_beneficiary(sex_gender_category__hybrid_reading, cisgender_category_protectors).
narrative_ontology:constraint_victim(sex_gender_category__hybrid_reading, trans_women_without_transition).
narrative_ontology:constraint_victim(sex_gender_category__hybrid_reading, non_medical_trans_individuals).
narrative_ontology:constraint_victim(sex_gender_category__hybrid_reading, transition_cost_bearers).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sex_gender_category__hybrid_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(sex_gender_category__hybrid_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sex_gender_category__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sex_gender_category__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sex_gender_category__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) because the constraint produces asymmetric costs: trans women bear medical expenses and psychological burden to gain entry; medical institutions collect revenue and control the boundary; cisgender women retain costless membership. The medical gatekeeping is not incidental — it is the structural mechanism by which the reading operates. Suppression is also elevated (0.71) because alternatives (biology-alone, identity-alone) are institutionally suppressed — courts, medical boards, and legal regimes actively enforce the hybrid reading and discourage or penalize departures. Theater is moderate (0.42): the medical review function is real (genuine health screening occurs), but a growing share of enforcement activity is devoted to maintaining category boundaries against identity-based claims rather than purely health-based gatekeeping. Accessibility collapse at 0.62 reflects that once someone is locked into 'medical transition is required for category change,' their alternatives narrow to paying for transition or accepting non-recognition — exit is constrained. Resistance at 0.58 indicates substantial pushback from non-transitioning trans advocates and biology-reading advocates, but suppression prevents their successful institutional repositioning. The measurement series shows extractiveness and suppression rising over time as medical gatekeeping becomes more standardized and institutionalized; theater also rises as the performative dimension of medical review strengthens. The coercion grid shows that suppression and accessibility collapse are highest at the structural and organizational levels (medical and legal institutions maintain the boundary) and lower at the individual level (individual trans women experience both support and pressure, not pure suppression). Resistance is highest at the organizational level (advocacy groups contest actively) and lower at the structural level (the core boundary definition faces less active structural opposition). Stakes inflation is highest at the individual and class levels (transition costs and non-recognition affect trans individuals and the trans class acutely) and lower at structural/organizational levels (the institutions managing the boundary face less pressure from the constraint itself).
 *
 * PERSPECTIVAL GAP:
 *   The medical institutions and legal regimes experience this reading as legitimate coordination (solving the category-membership problem with an administratively clear criterion). Trans women who pursue transition experience it as a mixed exchange: gain legal recognition but bear substantial cost and submit to medical authority over their identity-claim. Trans women without transition experience pure suppression: their identity claim is deemed insufficient, and the costs of non-recognition are absorbed wholly by them. Cisgender women are internally divided: some see the medical gatekeeping as protecting category boundaries; others see it as unnecessarily coercive. The observation seat (comparative analysis) sees all three readings as structurally coherent but with different victim sets, gatekeeping costs, and authority distributions. The engine should compute tangled_rope from the payer seat (high χ due to high d from extraction + constraints), rope or mixed from the medical institution seat (low χ — they control the arrangement and benefit from it), and contested/multiple from the identity-locked trans women seat (d depends on directionality override: are they beneficiaries of conditional access or victims of gatekeeping?). The measurement series shows this sitting stable — not drifting toward mandatrophy, but not naturally stabilizing either.
 *
 * DIRECTIONALITY LOGIC:
 *   Trans women post-transition are conditionally included: they benefit from legal recognition (d moves toward beneficiary end) but bear the cost of accessing that recognition through required medical transition (d moves back toward payer end). Their net directionality depends on whether we weight the recognition benefit or the transition burden more heavily — directionality override may be needed. Trans women without transition have no such mixed experience: they are pure payers (excluded, bearing costs, d near 1.0). Medical institutions are beneficiaries (d near 0.0): they set the criteria, enforce them, and collect revenue. Legal institutions are similarly positioned but with slightly higher payer load (they enforce a rule that constrains their own institutional freedom). Cisgender women are slight beneficiaries (d near 0.3) if the boundary protection is valued; neutral if indifferent. Non-transitioning trans advocates and biology-reading advocates are payers (they lose the institutional debate and bear the costs of suppression — d near 0.8). The directionality override should apply to trans_women_post_transition: the derivation from beneficiary role + payer load (transition costs) produces an ambiguous d, around 0.5–0.6. The narrative logic suggests a directionality closer to 0.45 (modest beneficiary, modest payer) — they gain from the arrangement more than they lose, but the arrangement still extracts from them, unlike the pure beneficiary medical institutions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (no procedure for trans individuals to gain legal recognition) was live at the constraint's origin. The hybrid reading solved it by creating a medical-recognition pathway. The founding problem status is now contested: medical and legal authorities attest it is still live (transition pathways still needed); trans advocates attest it is partially solved but the solution created a secondary problem (coercive medicalization). The disappearance verdict is 'world_rearranges': if the hybrid reading vanished, legal category membership would reorganize — either toward biology-alone or identity-alone. The mismatch (founding_problem_status = contested, disappearance_verdict = world_rearranges) suggests mandatrophy in process: the founding mandate has outlived unanimity. However, this is not mandatrophy-resolved (the constraint still performs its founding function for some seats) but rather mandatrophy-contested — different seats experience it as functional or dysfunctional depending on their position. The theater ratio at 0.42 suggests moderate performative maintenance: medical institutions conduct real gatekeeping, but some of their activity is devoted to defending the boundary against identity-based claims. The extraction measurement rising (0.52 → 0.68) suggests the constraint is accumulating secondary benefits unrelated to the founding problem (medical institutions expanding transition services, legal institutions expanding documentation power), which feeds the contested status. This is not a false summit (not a mountain with beneficiaries) — the coordination function (administrative category clarity) is real. But it is a case where the founding problem's solution (medical recognition pathway) has become a mechanism for institutional expansion and extraction beyond the founding scope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    medical_gatekeeping_necessity,
    'Is medical transition structurally necessary to establish female category membership for legal and institutional purposes, or is medical gatekeeping a chosen mechanism that could be replaced by alternative criteria (identity documentation, lived-experience attestation, self-identification with administrative verification)?',
    'Comparative institutional analysis: jurisdictions that have adopted identity-based or alternative administrative criteria and measured outcomes (legal clarity, institutional implementation success, dispute rates) versus those using medical gatekeeping. Natural experiments where gatekeeping has been relaxed or tightened.',
    'If medical gatekeeping is necessary, the constraint''s extractiveness is partly the cost of solving a coordination problem; if it is a chosen mechanism, the extractiveness is mostly institutional rent-seeking. This changes whether the constraint is tangled_rope (genuine coordination + extraction) or snare (pure gatekeeping extraction covering coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medical_gatekeeping_necessity, conceptual, 'Whether medical authority is structurally required for the category boundary or is one possible implementation.').

omega_variable(
    internalized_identity_lock,
    'Is the suppression experienced by trans_women_without_transition purely structural (external barriers, institutional exclusion) or partially internalized (the individual has fused their identity-claim validity with medical proof, making them feel they must transition even when external barriers hypothetically disappear)?',
    'Longitudinal study of individuals who transition for legal/institutional recognition versus those who resist the pathway: post-removal of external barriers (hypothetically or through legal change), do individuals report persistent sense of invalidation or expectation of medical proof? Exit-trajectory analysis: do trans individuals who transition experience reduced suppression afterward, or persistent suppression indicating partial internalization?',
    'If suppression is mostly structural, removing barriers should reduce it significantly; if partially internalized, suppression persists post-barrier-removal. This affects the measured suppression value and the classification from the trans individual''s seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internalized_identity_lock, empirical, 'Degree of internalization of medical-gatekeeping logic in trans individuals'' self-concept.').

omega_variable(
    kernel_reading_contest_localization,
    'Which specific structural elements of the hybrid reading (as opposed to biology or identity readings) are genuinely contested, and which are institutionally settled?',
    'Jurisdictional analysis: some countries/regions enforce biology-reading, others hybrid, others identity-reading. Within hybrid-reading jurisdictions, what elements face legal or political challenge? Identify the margins of the contest.',
    'If the contest is localized to specific elements (e.g., whether surgery is required in addition to hormones), the constraint may be more stable than if the entire reading is contested. This affects mandatrophy assessment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_localization, empirical, 'Which dimensions of the hybrid reading are institutionally stable versus contested.').

omega_variable(
    medical_authority_beneficiary_concentration,
    'To what degree does the medical gatekeeping mechanism concentrate institutional authority and financial benefit in medical institutions, beyond what is necessary to deliver transition care?',
    'Cost analysis: actual medical cost of transition services versus medical revenue extracted; institutional analysis: do medical boards expand gatekeeping requirements beyond clinical necessity (increasing the role of medical authority)? Comparison with identity-based systems: do they require less institutional gatekeeping overhead?',
    'High concentration would support the interpretation that extractiveness (0.68) is partly institutional rent-seeking; low concentration would support the interpretation that extractiveness is partly the cost of medical transition itself. This affects whether the constraint is tangled_rope (legitimate coordination + extraction) or has snare-like elements (gatekeeping extraction disguised as necessary medical review).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medical_authority_beneficiary_concentration, empirical, 'Proportion of measured extractiveness that reflects necessary medical costs versus institutional gatekeeping expansion.').

omega_variable(
    three_readings_coexistence_stability,
    'Are the three readings (biology, hybrid, identity) genuinely coexisting as live institutional options, or is one reading becoming institutionally dominant and the others being suppressed from institutional viability?',
    'Jurisdictional trends over the interval: do hybrid-reading jurisdictions remain stable, or do they shift toward biology or identity? Do rejected readings persist as live political movements or fade to marginal status? Analysis of institutional investment: which readings receive legal support, medical infrastructure, and institutional enforcement resources?',
    'If the three readings are stable coexisting options, the hybrid reading is one choice among coherent alternatives. If one reading is becoming dominant, the reading-relations and axiom framings may need revision. This affects how the constraint family networks together.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(three_readings_coexistence_stability, empirical, 'Whether the three kernel readings remain institutionally viable or if one is suppressing the others.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sex_gender_category__hybrid_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sex__tr_t0, sex_gender_category__hybrid_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(sex__tr_t5, sex_gender_category__hybrid_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement(sex__tr_t10, sex_gender_category__hybrid_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement(sex__tr_t20, sex_gender_category__hybrid_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(sex__tr_t30, sex_gender_category__hybrid_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement(sex__tr_t40, sex_gender_category__hybrid_reading, theater_ratio, 40, 0.43).

% Extraction over time
narrative_ontology:measurement(sex__be_t0, sex_gender_category__hybrid_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(sex__be_t5, sex_gender_category__hybrid_reading, base_extractiveness, 5, 0.56).
narrative_ontology:measurement(sex__be_t10, sex_gender_category__hybrid_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(sex__be_t20, sex_gender_category__hybrid_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(sex__be_t30, sex_gender_category__hybrid_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(sex__be_t40, sex_gender_category__hybrid_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(sex__su_t0, sex_gender_category__hybrid_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(sex__su_t5, sex_gender_category__hybrid_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(sex__su_t10, sex_gender_category__hybrid_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(sex__su_t20, sex_gender_category__hybrid_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(sex__su_t30, sex_gender_category__hybrid_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(sex__su_t40, sex_gender_category__hybrid_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sex_gender_category__hybrid_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(sex_gender_category__hybrid_reading, 0.12).
narrative_ontology:affects_constraint(sex_gender_category__hybrid_reading, sex_gender_category__biology_reading).
narrative_ontology:affects_constraint(sex_gender_category__hybrid_reading, sex_gender_category__identity_reading).

% DUAL FORMULATION NOTE:
% The sex_gender_category kernel is instantiated by three structurally distinct readings, each with different ε values, gatekeeping costs, victim sets, and authority distributions. The biology_reading uses reproductive biology as the sole criterion (ε low, no gatekeeping, no victims). The identity_reading uses subjective identification alone (ε low-moderate, minimal gatekeeping, victims are boundary-defenders). The hybrid_reading (THIS constraint) uses biology + medical transition (ε high, substantial gatekeeping, victims are those without access to or willingness to pursue transition). All three readings coexist as live institutional positions across different jurisdictions and professional bodies. Each reading is a separate constraint story; network links document how they compete and influence each other.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sex_gender_category__hybrid_reading, moderate, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
