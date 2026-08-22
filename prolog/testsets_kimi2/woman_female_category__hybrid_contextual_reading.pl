% ============================================================================
% CONSTRAINT STORY: woman_female_category__hybrid_contextual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_female_category__hybrid_contextual_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: woman_female_category__hybrid_contextual_reading
 *   human_readable: Hybrid Contextual Category Membership for Woman/Female
 *   domain: political_philosophy/bioethics/gender_studies/law
 *
 * SUMMARY:
 *   This constraint instantiates the hybrid_contextual_reading of the
 *   woman_female_category kernel. It holds that membership in the categories
 *   'woman' and 'female' varies by context: biological sex determines
 *   membership in medical, sports, and safety contexts, while gender identity
 *   determines membership in social and legal recognition contexts. The
 *   reading is advanced by administrative and legal institutions as a
 *   conflict-minimization framework, but extracts recognition costs from
 *   whichever group's preferred reading is subordinated in a given domain. It
 *   is structurally distinct from the universal sex_biology_reading and the
 *   universal gender_identity_reading, which it addresses by domain-splitting
 *   rather than universal adjudication.
 *
 * KEY AGENTS:
 *   - administrative_institutions (institutional/arbitrage) â adjudicate domain boundaries and benefit from reduced systemic conflict
 *   - gender_identity_recognition_seekers (powerless/identity_locked) â bear costs of sex-based categorization in medical, sports, and safety domains
 *   - sex_based_rights_claimants (moderate/identity_locked) â bear costs of gender-identity categorization in social and legal domains
 *   - bioethicist_observers (analytical) â analyze the framework's coherence and distributive effects
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_female_category__hybrid_contextual_reading, 0.55).
domain_priors:suppression_score(woman_female_category__hybrid_contextual_reading, 0.62).
domain_priors:theater_ratio(woman_female_category__hybrid_contextual_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_female_category__hybrid_contextual_reading, tangled_rope).
narrative_ontology:human_readable(woman_female_category__hybrid_contextual_reading, "Hybrid Contextual Category Membership for Woman/Female").
narrative_ontology:topic_domain(woman_female_category__hybrid_contextual_reading, "political_philosophy/bioethics/gender_studies/law").

domain_priors:requires_active_enforcement(woman_female_category__hybrid_contextual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_female_category__hybrid_contextual_reading, 'd5702701-f90a-46b9-9183-fd635538c6f2').
narrative_ontology:cs_kernel_codification('d5702701-f90a-46b9-9183-fd635538c6f2', formalized).
narrative_ontology:cs_authority_grounding('d5702701-f90a-46b9-9183-fd635538c6f2', expertise).
narrative_ontology:cs_interpretation_layer_present('d5702701-f90a-46b9-9183-fd635538c6f2').
narrative_ontology:cs_reading_relation('d5702701-f90a-46b9-9183-fd635538c6f2', woman_female_category__sex_biology_reading, influences).
narrative_ontology:cs_reading_relation('d5702701-f90a-46b9-9183-fd635538c6f2', woman_female_category__gender_identity_reading, influences).
narrative_ontology:cs_axiom('d5702701-f90a-46b9-9183-fd635538c6f2', foundational, contextual_membership_principle).
narrative_ontology:cs_axiom_status(contextual_membership_principle, holdable).
narrative_ontology:cs_axiom_grounding('d5702701-f90a-46b9-9183-fd635538c6f2', contextual_membership_principle, conventional).
narrative_ontology:cs_axiom('d5702701-f90a-46b9-9183-fd635538c6f2', foundational, domain_specific_sex_relevance).
narrative_ontology:cs_axiom_status(domain_specific_sex_relevance, holdable).
narrative_ontology:cs_axiom_grounding('d5702701-f90a-46b9-9183-fd635538c6f2', domain_specific_sex_relevance, empirically_contingent).
narrative_ontology:cs_reference_frame('d5702701-f90a-46b9-9183-fd635538c6f2', domain_partitioned_coherence).
narrative_ontology:cs_drift_state('d5702701-f90a-46b9-9183-fd635538c6f2', contemporary_policy_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d5702701-f90a-46b9-9183-fd635538c6f2', '').
narrative_ontology:cs_kernel_id(woman_female_category__hybrid_contextual_reading, woman_female_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_female_category__hybrid_contextual_reading, administrative_institutions).
narrative_ontology:constraint_victim(woman_female_category__hybrid_contextual_reading, gender_identity_recognition_seekers).
narrative_ontology:constraint_victim(woman_female_category__hybrid_contextual_reading, sex_based_rights_claimants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive competing claims from sex-based rights advocates and gender-identity advocates about who belongs in the categories woman and female. Adjudicate which contexts require biological sex classification and which permit gender identity classification, issuing guidelines and rulings that determine access to sports, medical protocols, and legal identity documents. Benefit from reduced political instability compared to universal adoption of either framework.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, administrative_institutions, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(woman_female_category__hybrid_contextual_reading, administrative_institutions, beneficiary).

% Live and identify as women in social and legal contexts where gender identity governs category membership, but are classified by biological sex in medical screenings, sports eligibility, and safety protocols. Experience this domain split as partial non-recognition of their gender identity in specific institutional settings. Have limited ability to exit the medical or sports categorization frameworks without foregoing care or competition.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, gender_identity_recognition_seekers, payer,
    powerless, biographical, identity_locked, national).

% Assert that biological sex is the basis of womanhood and female status. Experience the use of gender identity in social and legal recognition contextsâsuch as prisons, shelters, and statistical data collectionâas erasure of sex-based categories and protections. Organize around preserving sex-based exemptions and provisions. Have limited ability to exit the legal recognition framework without losing civic standing.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, sex_based_rights_claimants, payer,
    moderate, biographical, identity_locked, national).

% Study the hybrid framework's operation across medicine, law, and sports, publishing analyses of its consistency, distributive effects, and boundary cases. Do not stand to gain or lose personally from which categorical system prevails.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, bioethicist_observers, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(woman_female_category__hybrid_contextual_reading, administrative_institutions).
narrative_ontology:fixing_cost_class(woman_female_category__hybrid_contextual_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves competing categorical claims between biological sex and gender identity by partitioning social domains, preventing total victory of either framework and maintaining minimal social and legal coherence.
% TRANSFER_FUNCTION: Moves categorical authority from contested universal application to domain-specific institutional adjudication; transfers recognition and protection costs to whichever group is subordinated in a given context.
% ABSENT_VOICES: Radical eliminativists who reject both sex and gender categories entirely, and integrationists who demand a single unified category system across all contexts, are excluded from the hybrid compromise framework.
% DISAPPEARANCE_RATIONALE: If the hybrid contextual framework vanished, social and legal institutions would face unmediated conflict between sex-based and gender-identity-based categorical claims across all domains, forcing a choice of universal framework in each jurisdiction.
% FOUNDING_PROBLEM: Irreconcilable conflict between sex-based and gender-identity-based categorical claims in law, medicine, and sports, where applying either framework universally produces severe political resistance and legal instability.
% FOUNDING_PROBLEM_CORROBORATION: Administrative institutions and some legal scholars attest the conflict is live and requires domain-splitting; sex-based rights advocates and gender identity advocates each attest the founding problem is misdiagnosed and the hybrid framework merely defers necessary categorical resolution. Independent political theorists note the conflict is real but dispute whether domain-splitting is a stable solution.
narrative_ontology:disappearance_verdict(woman_female_category__hybrid_contextual_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_female_category__hybrid_contextual_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_female_category__hybrid_contextual_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(woman_female_category__hybrid_contextual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_female_category__hybrid_contextual_reading, 0.55, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_female_category__hybrid_contextual_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(woman_female_category__hybrid_contextual_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(woman_female_category__hybrid_contextual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.55) because the constraint systematically subordinates one reading in each domain, generating persistent misrecognition costs, but avoids the maximal extraction of a universal snare. Suppression (0.62) reflects active institutional enforcement of domain boundaries and the exclusion of universalist alternatives from their subordinated domains. Theater ratio (0.42) captures the increasing performative dimension of consultations and impact assessments that ratify predetermined domain splits. Accessibility collapse (0.50) is moderate: universalist alternatives remain conceptually available but are practically excluded in specific domains. Resistance (0.72) is high because both subordinated groups actively contest the framework in their respective domains. The measurement series share a single time grid to prevent temporal misalignment artifacts.
 *
 * PERSPECTIVAL GAP:
 *   The administrative_institutions seat experiences the constraint as coordination that preserves institutional stability and legitimacy, computing a low effective extraction. The gender_identity_recognition_seekers and sex_based_rights_claimants seats experience the same constraint as extraction in their subordinated domains, computing high effective extraction. The engine derives this divergence from the beneficiary/victim declarations combined with identity-locked exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Administrative institutions are the declared beneficiaries (low directionality, near-subsidy: the constraint reduces their political and legal costs). Gender identity recognition seekers and sex-based rights claimants are the declared victims (high directionality, amplified extraction: the constraint overrides their categorical claims in specific domains). The identity_locked exit option amplifies effective extraction for both payer groups because their self-concept is fused to the categorical outcome.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling as a pure snare because it possesses a genuine coordination function: without domain-splitting, jurisdictions face unresolvable conflict between sex-based and gender-identity-based claims across all domains. It prevents mislabeling as a pure rope because the domain split is not Pareto-improving; each party loses in some contexts. The tangled_rope classification captures the simultaneous presence of coordination (conflict partitioning) and asymmetric extraction (domain-specific subordination).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    domain_boundary_stability,
    'Are the boundaries between sex-relevant and gender-identity-relevant domains stable and principled, or do they shift based on political pressure?',
    'Track statutory and case-law boundary adjudication over time to detect drift or expansion of domains.',
    'If boundaries are unstable, the constraint may be a transient scaffold or a devolving snare; if stable, a durable tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_boundary_stability, conceptual, 'Stability of domain boundaries in hybrid contextual framework').

omega_variable(
    truce_vs_synthesis,
    'Does the hybrid reading represent a genuine synthesis of sex and gender identity claims, or merely a temporary truce that preserves incompatible universalist commitments?',
    'Analyze whether the framework generates novel normative principles or only modulates the conflict between pre-existing readings.',
    'If merely a truce, foreclosure of sibling readings may be temporary; if synthesis, the reading may harden into an independent position.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(truce_vs_synthesis, conceptual, 'Whether hybrid contextualism is a synthesis or a truce').

omega_variable(
    contextual_suppression_ambiguity,
    'Is the suppression of universalist claims in each domain achieved through institutional enforcement or through normative acceptance of the contextual split?',
    'Measure resistance and noncompliance rates in subordinated domains; high resistance indicates enforcement-heavy suppression.',
    'If enforcement-heavy, the constraint leans toward snare; if normatively accepted, it leans toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contextual_suppression_ambiguity, empirical, 'Structural versus normative suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_female_category__hybrid_contextual_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t0, woman_female_category__hybrid_contextual_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(woma_tr_t4, woman_female_category__hybrid_contextual_reading, theater_ratio, 4, 0.33).
narrative_ontology:measurement(woma_tr_t8, woman_female_category__hybrid_contextual_reading, theater_ratio, 8, 0.36).
narrative_ontology:measurement(woma_tr_t12, woman_female_category__hybrid_contextual_reading, theater_ratio, 12, 0.38).
narrative_ontology:measurement(woma_tr_t16, woman_female_category__hybrid_contextual_reading, theater_ratio, 16, 0.4).
narrative_ontology:measurement(woma_tr_t20, woman_female_category__hybrid_contextual_reading, theater_ratio, 20, 0.42).

% Extraction over time
narrative_ontology:measurement(woma_be_t0, woman_female_category__hybrid_contextual_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(woma_be_t4, woman_female_category__hybrid_contextual_reading, base_extractiveness, 4, 0.43).
narrative_ontology:measurement(woma_be_t8, woman_female_category__hybrid_contextual_reading, base_extractiveness, 8, 0.46).
narrative_ontology:measurement(woma_be_t12, woman_female_category__hybrid_contextual_reading, base_extractiveness, 12, 0.49).
narrative_ontology:measurement(woma_be_t16, woman_female_category__hybrid_contextual_reading, base_extractiveness, 16, 0.52).
narrative_ontology:measurement(woma_be_t20, woman_female_category__hybrid_contextual_reading, base_extractiveness, 20, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t0, woman_female_category__hybrid_contextual_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(woma_su_t4, woman_female_category__hybrid_contextual_reading, suppression_requirement, 4, 0.54).
narrative_ontology:measurement(woma_su_t8, woman_female_category__hybrid_contextual_reading, suppression_requirement, 8, 0.57).
narrative_ontology:measurement(woma_su_t12, woman_female_category__hybrid_contextual_reading, suppression_requirement, 12, 0.6).
narrative_ontology:measurement(woma_su_t16, woman_female_category__hybrid_contextual_reading, suppression_requirement, 16, 0.62).
narrative_ontology:measurement(woma_su_t20, woman_female_category__hybrid_contextual_reading, suppression_requirement, 20, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_female_category__hybrid_contextual_reading, identity_coordination).
narrative_ontology:affects_constraint(woman_female_category__hybrid_contextual_reading, sex_biology_reading).
narrative_ontology:affects_constraint(woman_female_category__hybrid_contextual_reading, gender_identity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the woman_female_category kernel, decomposed from the colloquial label due to epsilon-invariance violations between universalist and hybrid contextual approaches.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
