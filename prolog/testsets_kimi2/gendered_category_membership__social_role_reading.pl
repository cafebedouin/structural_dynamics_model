% ============================================================================
% CONSTRAINT STORY: gendered_category_membership__social_role_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   human_readable: Gendered Category Membership (Social Role Reading)
 *   domain: social ontology / political philosophy / bioethics
 *
 * SUMMARY:
 *   This constraint instantiates the social_role_reading of the
 *   gendered_category_membership kernel: category membership (e.g., 'woman')
 *   is constituted by sustained social performance and recognition by others
 *   rather than by biological markers or subjective identity. Under this
 *   reading, trans women are conditionally included based on passing and
 *   recognition, and gatekeeping is distributed across everyday social
 *   interactions rather than centralized. The constraint extracts
 *   low-to-moderate performance costs unevenly: trans individuals must
 *   sustain labor-intensive recognition work, while gender-nonconforming cis
 *   women risk exclusion from spaces their biology would otherwise grant them
 *   access to. The coordination function is realâgender categories
 *   lubricate ordinary social interactionâbut the extraction is
 *   asymmetrically borne.
 *
 * KEY AGENTS:
 *   - trans_individuals: Primary target (powerless/identity_locked) â bear conditional inclusion and performance labor
 *   - gender_nonconforming_cis_women: Secondary target (moderate/constrained) â bear exclusion risk when femininity performance fails
 *   - conforming_cisgender_individuals: Primary beneficiary (moderate/mobile) â enjoy unmarked membership and coordination benefits
 *   - gender_scholars: Analytical observer (analytical/analytical) â document recognition conditions and gatekeeping
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gendered_category_membership__social_role_reading, 0.42).
domain_priors:suppression_score(gendered_category_membership__social_role_reading, 0.58).
domain_priors:theater_ratio(gendered_category_membership__social_role_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gendered_category_membership__social_role_reading, tangled_rope).
narrative_ontology:human_readable(gendered_category_membership__social_role_reading, "Gendered Category Membership (Social Role Reading)").
narrative_ontology:topic_domain(gendered_category_membership__social_role_reading, "social ontology / political philosophy / bioethics").

domain_priors:requires_active_enforcement(gendered_category_membership__social_role_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gendered_category_membership__social_role_reading, '5ec23153-1c7b-4545-9182-fb7bd22e37de').
narrative_ontology:cs_kernel_codification('5ec23153-1c7b-4545-9182-fb7bd22e37de', distributed).
narrative_ontology:cs_authority_grounding('5ec23153-1c7b-4545-9182-fb7bd22e37de', distributed).
narrative_ontology:cs_reading_relation('5ec23153-1c7b-4545-9182-fb7bd22e37de', gendered_category_membership__biological_sex_reading, forecloses).
narrative_ontology:cs_reading_relation('5ec23153-1c7b-4545-9182-fb7bd22e37de', gendered_category_membership__gender_identity_reading, influences).
narrative_ontology:cs_axiom('5ec23153-1c7b-4545-9182-fb7bd22e37de', foundational, membership_requires_recognition).
narrative_ontology:cs_axiom_status(membership_requires_recognition, holdable).
narrative_ontology:cs_axiom_grounding('5ec23153-1c7b-4545-9182-fb7bd22e37de', membership_requires_recognition, conventional).
narrative_ontology:cs_axiom('5ec23153-1c7b-4545-9182-fb7bd22e37de', foundational, performance_obligates_recognition).
narrative_ontology:cs_axiom_status(performance_obligates_recognition, holdable).
narrative_ontology:cs_axiom_grounding('5ec23153-1c7b-4545-9182-fb7bd22e37de', performance_obligates_recognition, deontological).
narrative_ontology:cs_reference_frame('5ec23153-1c7b-4545-9182-fb7bd22e37de', social_recognition_equilibrium).
narrative_ontology:cs_drift_state('5ec23153-1c7b-4545-9182-fb7bd22e37de', contemporary_trans_visibility_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5ec23153-1c7b-4545-9182-fb7bd22e37de', '').
narrative_ontology:cs_kernel_id(gendered_category_membership__social_role_reading, gendered_category_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gendered_category_membership__social_role_reading, conforming_cisgender_individuals).
narrative_ontology:constraint_victim(gendered_category_membership__social_role_reading, trans_individuals).
narrative_ontology:constraint_victim(gendered_category_membership__social_role_reading, gender_nonconforming_cis_women).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Must sustain continuous gender performance to secure recognition as category members from others; inclusion is conditional on passing and remains revocable if recognition falters; pays ongoing cognitive, emotional, and aesthetic labor to remain legible within the category.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, trans_individuals, payer,
    powerless, biographical, identity_locked, global).

% Cis women whose gender performance does not match normative expectations face exclusion from women's spaces, pronoun correctness, and social recognition despite presumptive biological membership; bear policing costs when their performance is read as insufficiently feminine.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, gender_nonconforming_cis_women, payer,
    moderate, biographical, constrained, global).

% Individuals whose gender performance aligns with dominant norms and who are readily recognized without effort; they benefit from smooth coordination of pronouns, bathroom access, social address, and role expectations without paying performance costs.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, conforming_cisgender_individuals, beneficiary,
    moderate, biographical, mobile, global).

% Document and analyze the distributed gatekeeping mechanisms through which recognition is granted or withheld; occupy an analytical seat that can describe the full constraint structure without being personally governed by its recognition demands.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, gender_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gendered_category_membership__social_role_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates complex social interaction by stabilizing gendered expectations: pronoun use, bathroom access, social address, and role allocation are settled by recognized category membership rather than negotiated anew in every encounter.
% TRANSFER_FUNCTION: Moves social legitimacy, safe access to gendered spaces, and unmarked everyday participation from those who fail to perform or pass to those who are readily recognized, via the distributed gatekeeping of ordinary social interaction.
% ABSENT_VOICES: Non-binary and genderqueer individuals who do not seek recognition within binary categories are structurally excluded from the recognition economy; their absence naturalizes the binary frame and removes pressure to expand the coordination function beyond two categories.
% DISAPPEARANCE_RATIONALE: If gendered category membership no longer required sustained social performance and recognition, everyday social coordination would lose a major heuristic; pronoun assignment, bathroom protocols, and gendered address would require explicit negotiation or alternative systems, rearranging ordinary interaction.
% FOUNDING_PROBLEM: How to coordinate complex social life among strangers without negotiating gendered interactional parameters anew in every encounter.
% FOUNDING_PROBLEM_CORROBORATION: Gender scholars and social theorists attest that gender categories historically solved coordination problems; feminist and trans activists outside the beneficiary set contest that the current arrangement still solves this problem or that its costs exceed its coordination benefit.
narrative_ontology:disappearance_verdict(gendered_category_membership__social_role_reading, world_rearranges).
narrative_ontology:founding_problem_status(gendered_category_membership__social_role_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gendered_category_membership__social_role_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is moderate (0.42) because the constraint imposes real but not extreme costs: performance labor, passing demands, and occasional exclusion. Suppression (0.58) reflects that alternatives (non-binary existence, refusing gender performance) are socially punished and partly collapsed. Accessibility collapse (0.65) captures that once the performance requirement is understood, exiting it is costly but not impossible. Resistance (0.48) registers ongoing feminist and trans activism challenging recognition conditions. Theater ratio (0.30) is moderate: some gender performance is functional coordination, some is theatrical maintenance of boundaries. The metrics and claim are independently authored: the structural claim is tangled_rope, and the metrics describe actual operation without tuning to match.
 *
 * PERSPECTIVAL GAP:
 *   The conforming beneficiary seat experiences the constraint as invisible infrastructureâgender 'just works'âwhile the trans and nonconforming payer seats experience it as active gatekeeping. The engine computes this divergence from identical structural data: the same interaction is coordination from one direction and extraction from the other.
 *
 * DIRECTIONALITY LOGIC:
 *   Conforming cisgender individuals occupy the beneficiary direction (d near 0.0): they are subsidized by the constraint's coordination function and pay negligible performance costs. Trans individuals occupy the target direction (d near 1.0): they pay performance costs and face recognition failure. Gender-nonconforming cis women sit between (d ~0.7): they have presumptive membership but risk its revocation, creating partial targethood. The distributed nature of gatekeeping means no single agent concentrates the gains; gain_flow is diffuse.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâcoordinating gendered social interactionâwas genuine and may remain partially live, preventing misclassification as pure snare. However, the persistence of distributed gatekeeping beyond its coordination necessity (e.g., policing bathroom access by appearance rather than by any functional need) suggests mandatrophy has not been resolved. The classification as tangled_rope captures both the live coordination function and the atrophied enforcement layers.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_boundary_ambiguity,
    'Does the social role reading of gendered category membership logically foreclose the gender identity reading, or can both grounds operate in a single framework?',
    'Comparative ethnography of communities employing both recognition and self-declaration criteria, plus conceptual analysis of whether category membership can have dual grounds without equivocation.',
    'If foreclosed, the social role reading functions as exclusive gatekeeping against identity-based claims; if co-possible, the constraint''s extraction profile lowers because multiple entry paths dilute the performance requirement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_boundary_ambiguity, conceptual, 'Whether social role and gender identity readings are mutually exclusive or co-possible').

omega_variable(
    cis_women_exclusion_scope,
    'Do cis women as a class bear material exclusion risk under the social role reading, or only gender-nonconforming subgroups?',
    'Empirical measurement of gatekeeping incidents such as bathroom exclusions, sports eligibility challenges, and workplace dress-code sanctions targeting cis women by perceived masculinity.',
    'If the risk is class-wide, victim structure is broader and the constraint more extractive; if subgroup-specific, extraction is narrowly targeted and the coordination function more benign.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cis_women_exclusion_scope, empirical, 'Scope of cis women''s exclusion risk under performance-based membership').

omega_variable(
    enforcement_naturalization,
    'Is distributed social recognition of gender a spontaneous coordination equilibrium, or an actively enforced normative structure?',
    'Historical comparison of gender recognition practices across societies with varying levels of institutional enforcement; measurement of sanction severity for non-performance.',
    'If spontaneous equilibrium, the constraint is closer to rope; if actively enforced through social sanction, it is tangled rope or snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_naturalization, conceptual, 'Whether distributed gender recognition is naturalized equilibrium or active enforcement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gendered_category_membership__social_role_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gend_tr_t0, gendered_category_membership__social_role_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(gend_tr_t10, gendered_category_membership__social_role_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(gend_tr_t20, gendered_category_membership__social_role_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(gend_tr_t30, gendered_category_membership__social_role_reading, theater_ratio, 30, 0.3).
narrative_ontology:measurement(gend_tr_t40, gendered_category_membership__social_role_reading, theater_ratio, 40, 0.3).

% Extraction over time
narrative_ontology:measurement(gend_be_t0, gendered_category_membership__social_role_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(gend_be_t10, gendered_category_membership__social_role_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(gend_be_t20, gendered_category_membership__social_role_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(gend_be_t30, gendered_category_membership__social_role_reading, base_extractiveness, 30, 0.41).
narrative_ontology:measurement(gend_be_t40, gendered_category_membership__social_role_reading, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(gend_su_t0, gendered_category_membership__social_role_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(gend_su_t10, gendered_category_membership__social_role_reading, suppression_requirement, 10, 0.54).
narrative_ontology:measurement(gend_su_t20, gendered_category_membership__social_role_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(gend_su_t30, gendered_category_membership__social_role_reading, suppression_requirement, 30, 0.56).
narrative_ontology:measurement(gend_su_t40, gendered_category_membership__social_role_reading, suppression_requirement, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gendered_category_membership__social_role_reading, identity_coordination).
narrative_ontology:affects_constraint(gendered_category_membership__social_role_reading, biological_sex_reading).
narrative_ontology:affects_constraint(gendered_category_membership__social_role_reading, gender_identity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the gendered_category_membership constraint family, decomposed per the epsilon-invariance principle because the biological, identity, and social-role grounds for category membership have structurally distinct epsilon values, beneficiary/victim structures, and enforcement mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
