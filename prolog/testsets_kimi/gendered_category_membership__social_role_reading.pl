% ============================================================================
% CONSTRAINT STORY: gendered_category_membership__social_role_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gendered_category_membership__social_role_reading, []).

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
 *   constraint_id: gendered_category_membership__social_role_reading
 *   human_readable: Gendered Category Membership â Social Role Reading
 *   domain: social/political/bioethical
 *
 * SUMMARY:
 *   This constraint instantiates the social-role reading of gendered category
 *   membership: individuals are sorted into gender categories based on
 *   sustained social performance and recognition by others rather than
 *   biology or self-declaration alone. Trans women are conditionally included
 *   when their performance passes social thresholds; gender-non-conforming
 *   cis women face exclusion despite cisgender status. The gatekeeping is
 *   distributed across everyday social interactions rather than centralized.
 *   The constraint is claimed as tangled_rope because it carries a genuine
 *   coordination function â gender categories enable social fluency â
 *   while asymmetrically extracting performance labor from those whose
 *   membership is contested.
 *
 * KEY AGENTS:
 *   - Cisgender conformists (beneficiary): automatically recognized, bear no performance tax.
 *   - Trans individuals (payer): sustain costly performance for conditional inclusion; identity-locked exit.
 *   - Gender-non-conforming cis women (payer): face exclusion risk despite cisgender status; constrained exit.
 *   - Social ontologists (observer): analytical seat mapping the structure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gendered_category_membership__social_role_reading, 0.42).
domain_priors:suppression_score(gendered_category_membership__social_role_reading, 0.55).
domain_priors:theater_ratio(gendered_category_membership__social_role_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gendered_category_membership__social_role_reading, tangled_rope).
narrative_ontology:human_readable(gendered_category_membership__social_role_reading, "Gendered Category Membership â Social Role Reading").
narrative_ontology:topic_domain(gendered_category_membership__social_role_reading, "social/political/bioethical").

domain_priors:requires_active_enforcement(gendered_category_membership__social_role_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gendered_category_membership__social_role_reading, '63693ed5-0199-4ccb-8e78-ac864dacfc12').
narrative_ontology:cs_kernel_codification('63693ed5-0199-4ccb-8e78-ac864dacfc12', distributed).
narrative_ontology:cs_authority_grounding('63693ed5-0199-4ccb-8e78-ac864dacfc12', distributed).
narrative_ontology:cs_reading_relation('63693ed5-0199-4ccb-8e78-ac864dacfc12', gendered_category_membership__biological_sex_reading, coexists_with).
narrative_ontology:cs_reading_relation('63693ed5-0199-4ccb-8e78-ac864dacfc12', gendered_category_membership__gender_identity_reading, coexists_with).
narrative_ontology:cs_axiom('63693ed5-0199-4ccb-8e78-ac864dacfc12', foundational, membership_requires_social_recognition).
narrative_ontology:cs_axiom_status(membership_requires_social_recognition, holdable).
narrative_ontology:cs_axiom_grounding('63693ed5-0199-4ccb-8e78-ac864dacfc12', membership_requires_social_recognition, conventional).
narrative_ontology:cs_axiom('63693ed5-0199-4ccb-8e78-ac864dacfc12', foundational, gender_is_social_position_not_internal_essence).
narrative_ontology:cs_axiom_status(gender_is_social_position_not_internal_essence, holdable).
narrative_ontology:cs_axiom_grounding('63693ed5-0199-4ccb-8e78-ac864dacfc12', gender_is_social_position_not_internal_essence, conventional).
narrative_ontology:cs_reference_frame('63693ed5-0199-4ccb-8e78-ac864dacfc12', gender_as_social_position).
narrative_ontology:cs_drift_state('63693ed5-0199-4ccb-8e78-ac864dacfc12', contemporary_identity_politics_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('63693ed5-0199-4ccb-8e78-ac864dacfc12', '').
narrative_ontology:cs_kernel_id(gendered_category_membership__social_role_reading, gendered_category_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gendered_category_membership__social_role_reading, cisgender_conformists).
narrative_ontology:constraint_victim(gendered_category_membership__social_role_reading, trans_individuals).
narrative_ontology:constraint_victim(gendered_category_membership__social_role_reading, gender_non_conforming_cis_women).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Their gender performance is automatically recognized as legible and valid within the social framework. They receive unmarked membership in gendered spaces, language, and institutions without additional labor or scrutiny. The category coordinates their social life effortlessly.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, cisgender_conformists, beneficiary,
    powerful, biographical, mobile, national).

% Must sustain a continuous gender performance that meets socially distributed thresholds for recognition to be included in the category. Trans women in particular face conditional inclusion based on passing. Bear the costs of performance labor, misrecognition, and conditional access to gendered spaces. Exiting the identity is psychologically and socially prohibitive.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, trans_individuals, payer,
    moderate, biographical, identity_locked, national).

% Are cisgender women but face exclusion, policing, or misrecognition when their gender performance deviates from the expected social role â for example, butch women or women with masculine-coded appearance. Bear the risk of being denied access to women-only spaces or social membership despite their cisgender status.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, gender_non_conforming_cis_women, payer,
    moderate, biographical, constrained, national).

% Analyze the metaphysics and politics of gender category membership from a theoretical remove. Map the structural relationships between performance, recognition, and exclusion without directly bearing the costs or collecting the benefits of the constraint.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, social_ontologists, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates social interaction, linguistic reference, and institutional placement by assigning individuals to gender categories based on sustained, mutually intelligible performance rather than biology or self-declaration alone.
% TRANSFER_FUNCTION: Moves the burden of gender performance labor and the risk of misrecognition from trans individuals and gender-non-conforming cis women toward the unmarked social membership of conforming individuals, via distributed interpersonal gatekeeping.
% ABSENT_VOICES: Non-binary individuals who reject the binary architecture of the social role, trans individuals who do not pass and are therefore excluded from recognition, and gender-non-conforming cis women are often absent from policy frames that treat category membership as straightforward performance.
% DISAPPEARANCE_RATIONALE: If gendered category membership grounded in sustained social performance disappeared, everyday social interaction would lose a major coordinate; pronoun assignment, spatial segregation, and institutional placement would require extensive renegotiation, and the current distribution of recognition and exclusion would collapse.
% FOUNDING_PROBLEM: The need for stable, mutually intelligible gender categories to coordinate social life, reproduction, and institutional organization in the absence of purely biological or purely subjective markers.
% FOUNDING_PROBLEM_CORROBORATION: Feminist philosophers and sociologists attest that the problem is partially live â gender categories do coordinate social life â but that the social-role solution generates new exclusions. Trans activists and gender-critical feminists dispute whether the founding problem justifies the current gatekeeping structure. No neutral party outside the dispute corroborates the original problem as requiring this specific solution.
narrative_ontology:disappearance_verdict(gendered_category_membership__social_role_reading, world_rearranges).
narrative_ontology:founding_problem_status(gendered_category_membership__social_role_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gendered_category_membership__social_role_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gendered_category_membership__social_role_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gendered_category_membership__social_role_reading, 0.42, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gendered_category_membership__social_role_reading_tests).
:- end_tests(gendered_category_membership__social_role_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45 at interval end) because the constraint extracts real performance labor and subjects some agents to misrecognition, but the costs are distributed and not monetized. Suppression is moderate (0.60) because enforcement is interpersonal and habitual rather than state-coercive, though it intensifies as public contestation rises. Theater ratio is moderate-low (0.30): much gatekeeping remains functional, but public debates over bathrooms, sports, and pronouns have layered symbolic performance onto the constraint. Accessibility collapse is moderate (0.40) because self-declaration alternatives are visible and contested. Resistance is moderate (0.50) because both trans-rights advocates and gender-critical feminists actively contest the boundary.
 *
 * PERSPECTIVAL GAP:
 *   From the conforming beneficiary seat, the constraint appears as benign social coordination â categories work, interaction is fluent, and enforcement feels like natural response to legibility. From the payer seats, the same structure is experienced as a demanding, conditional, and insecure gate that must be continuously performed to remain open. The engine computes this divergence from the identical structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Cisgender conformists occupy the beneficiary seat: their directionality is near the subsidy end because the constraint grants them unmarked membership without performance costs. Trans individuals and gender-non-conforming cis women occupy target seats: their directionality is near the full-target end because the constraint extracts performance labor and threatens exclusion. The engine will compute divergent per-seat classifications from this structural asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying as tangled_rope prevents mislabeling the constraint as a pure snare (which would ignore the real coordination function of gender categories in social life) or as a pure rope (which would ignore the asymmetric performance burden and conditional exclusion). The victim structure is ambiguous because cis women â typically the reference class â also bear exclusion risk when non-conforming, which is diagnostic of a hybrid structure rather than a simple majority-exploits-minority dynamic.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is this constraint one reading of a three-way kernel (biological sex vs. gender identity vs. social role), and would reclassification change if the biological-sex or gender-identity reading were adopted as the dominant frame?',
    'Cross-reading comparison: evaluate whether the structural data (beneficiaries, victims, enforcement) shifts when the grounding of category membership changes from performance to biology or to self-declaration.',
    'If the biological-sex reading were adopted, victim structure would shift to exclude trans women entirely and center on intersex individuals; if the gender-identity reading were adopted, performance costs would drop and the extraction profile would flatten toward rope. This reading''s classification as tangled_rope depends on the performance-and-recognition mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Structural uncertainty arising from this being one reading of a contested kernel.').

omega_variable(
    performance_cost_ambiguity,
    'Are the performance costs borne by trans individuals and gender-non-conforming cis women a necessary friction of social coordination, or extractive overhead that could be reduced by alternative categorization schemes?',
    'Comparative analysis of social coordination costs in jurisdictions or subcultures with different gatekeeping intensities.',
    'If performance costs are necessary coordination friction, the constraint is closer to rope; if they are reducible overhead maintained by enforcement, it remains tangled_rope or shifts toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performance_cost_ambiguity, empirical, 'Whether performance labor is inherent coordination cost or extractive overhead.').

omega_variable(
    distributed_enforcement_ambiguity,
    'Is the enforcement of this constraint structural (institutional policies requiring performance) or internalized (self-policing of gender performance to avoid misrecognition)?',
    'Post-policy trajectory analysis: if institutional gatekeeping were removed, would performance pressure persist via interpersonal dynamics?',
    'If internalized, effective suppression exceeds structural measures; the constraint operates partly as identity-locked self-enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distributed_enforcement_ambiguity, empirical, 'Structural vs internalized suppression in distributed gender gatekeeping.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gendered_category_membership__social_role_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gend_tr_t0, gendered_category_membership__social_role_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(gend_tr_t4, gendered_category_membership__social_role_reading, theater_ratio, 4, 0.18).
narrative_ontology:measurement(gend_tr_t8, gendered_category_membership__social_role_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement(gend_tr_t12, gendered_category_membership__social_role_reading, theater_ratio, 12, 0.25).
narrative_ontology:measurement(gend_tr_t16, gendered_category_membership__social_role_reading, theater_ratio, 16, 0.28).
narrative_ontology:measurement(gend_tr_t20, gendered_category_membership__social_role_reading, theater_ratio, 20, 0.3).

% Extraction over time
narrative_ontology:measurement(gend_be_t0, gendered_category_membership__social_role_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(gend_be_t4, gendered_category_membership__social_role_reading, base_extractiveness, 4, 0.37).
narrative_ontology:measurement(gend_be_t8, gendered_category_membership__social_role_reading, base_extractiveness, 8, 0.39).
narrative_ontology:measurement(gend_be_t12, gendered_category_membership__social_role_reading, base_extractiveness, 12, 0.41).
narrative_ontology:measurement(gend_be_t16, gendered_category_membership__social_role_reading, base_extractiveness, 16, 0.43).
narrative_ontology:measurement(gend_be_t20, gendered_category_membership__social_role_reading, base_extractiveness, 20, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(gend_su_t0, gendered_category_membership__social_role_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(gend_su_t4, gendered_category_membership__social_role_reading, suppression_requirement, 4, 0.48).
narrative_ontology:measurement(gend_su_t8, gendered_category_membership__social_role_reading, suppression_requirement, 8, 0.52).
narrative_ontology:measurement(gend_su_t12, gendered_category_membership__social_role_reading, suppression_requirement, 12, 0.55).
narrative_ontology:measurement(gend_su_t16, gendered_category_membership__social_role_reading, suppression_requirement, 16, 0.58).
narrative_ontology:measurement(gend_su_t20, gendered_category_membership__social_role_reading, suppression_requirement, 20, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gendered_category_membership__social_role_reading, identity_coordination).
narrative_ontology:affects_constraint(gendered_category_membership__social_role_reading, biological_sex_reading).
narrative_ontology:affects_constraint(gendered_category_membership__social_role_reading, gender_identity_reading).

% DUAL FORMULATION NOTE:
% This constraint is the social_role_reading of the gendered_category_membership kernel. It decomposes from the colloquial label 'gender' into three structurally distinct constraints: biological_sex_reading (membership grounded in immutable biology), gender_identity_reading (membership grounded in self-declaration), and this reading (membership grounded in sustained social performance and recognition). Each has distinct epsilon, stakeholders, and victim structures. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
