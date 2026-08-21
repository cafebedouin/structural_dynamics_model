% ============================================================================
% CONSTRAINT STORY: jurisprudential_method_kernel__maliki_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jurisprudential_method_kernel__maliki_reading, []).

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
 *   constraint_id: jurisprudential_method_kernel__maliki_reading
 *   human_readable: Maliki Jurisprudential Method: Medinan Practice as Source of Law
 *   domain: islamic_jurisprudence/legal_philosophy/institutional_history
 *
 * SUMMARY:
 *   This constraint describes the Maliki school's jurisprudential method,
 *   which prioritizes the 'amal ahl al-Madina (practice of the people of
 *   Medina) as a source of law, alongside the Qur'an and Hadith. This reading
 *   asserts that Medina, as the Prophet's city, preserved his practice most
 *   faithfully, granting its living tradition unique authority. This elevates
 *   the Medinan scholarly lineage and implicitly devalues non-Medinan
 *   interpretive claims, creating a structural asymmetry in jurisprudential
 *   authority. This is one reading of the broader
 *   'jurisprudential_method_kernel' contested across various Islamic legal
 *   schools.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jurisprudential_method_kernel__maliki_reading, 0.45).
domain_priors:suppression_score(jurisprudential_method_kernel__maliki_reading, 0.6).
domain_priors:theater_ratio(jurisprudential_method_kernel__maliki_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jurisprudential_method_kernel__maliki_reading, tangled_rope).
narrative_ontology:human_readable(jurisprudential_method_kernel__maliki_reading, "Maliki Jurisprudential Method: Medinan Practice as Source of Law").
narrative_ontology:topic_domain(jurisprudential_method_kernel__maliki_reading, "islamic_jurisprudence/legal_philosophy/institutional_history").

domain_priors:requires_active_enforcement(jurisprudential_method_kernel__maliki_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jurisprudential_method_kernel__maliki_reading, '1cadf9d7-c61b-4e26-918a-76941d3b970e').
narrative_ontology:cs_kernel_codification('1cadf9d7-c61b-4e26-918a-76941d3b970e', formalized).
narrative_ontology:cs_authority_grounding('1cadf9d7-c61b-4e26-918a-76941d3b970e', lineage).
narrative_ontology:cs_interpretation_layer_present('1cadf9d7-c61b-4e26-918a-76941d3b970e').
narrative_ontology:cs_reading_relation('1cadf9d7-c61b-4e26-918a-76941d3b970e', jurisprudential_method_kernel__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('1cadf9d7-c61b-4e26-918a-76941d3b970e', jurisprudential_method_kernel__shafii_reading, coexists_with).
narrative_ontology:cs_reading_relation('1cadf9d7-c61b-4e26-918a-76941d3b970e', jurisprudential_method_kernel__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('1cadf9d7-c61b-4e26-918a-76941d3b970e', foundational, amal_ahl_al_madina_as_prophetic_sunna).
narrative_ontology:cs_axiom_status(amal_ahl_al_madina_as_prophetic_sunna, holdable).
narrative_ontology:cs_axiom_grounding('1cadf9d7-c61b-4e26-918a-76941d3b970e', amal_ahl_al_madina_as_prophetic_sunna, theological).
narrative_ontology:cs_axiom('1cadf9d7-c61b-4e26-918a-76941d3b970e', foundational, medinan_practice_preserves_prophetic_authenticity).
narrative_ontology:cs_axiom_status(medinan_practice_preserves_prophetic_authenticity, holdable).
narrative_ontology:cs_axiom_grounding('1cadf9d7-c61b-4e26-918a-76941d3b970e', medinan_practice_preserves_prophetic_authenticity, empirically_contingent).
narrative_ontology:cs_reference_frame('1cadf9d7-c61b-4e26-918a-76941d3b970e', early_medinan_scholarly_consensus).
narrative_ontology:cs_drift_state('1cadf9d7-c61b-4e26-918a-76941d3b970e', contemporary_global_islamic_discourse, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('1cadf9d7-c61b-4e26-918a-76941d3b970e', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(jurisprudential_method_kernel__maliki_reading, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__maliki_reading, maliki_scholarly_lineage).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__maliki_reading, medinan_community_scholars).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__maliki_reading, non_medinan_interpretive_claims).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__maliki_reading, scholars_outside_maliki_school).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__maliki_reading, muslim_laity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The institutionalized body of scholars and jurists who transmit and interpret the Maliki school's methodology. They benefit from the authority derived from Medinan practice, which elevates their interpretive tradition and provides a stable framework for legal reasoning. Their identity is fused with this tradition.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, maliki_scholarly_lineage, agenda_setter,
    institutional, generational, identity_locked, regional).

% Scholars and jurists within Medina whose local traditions and interpretations are given elevated status by this methodology. They benefit from the presumption of authenticity for their local 'amal (practice).
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, medinan_community_scholars, beneficiary,
    organized, biographical, constrained, local).

% Interpretive claims and scholarly traditions originating outside Medina that are implicitly or explicitly devalued relative to Medinan practice. They bear the cost of having to justify their authenticity against a privileged Medinan standard.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, non_medinan_interpretive_claims, payer,
    moderate, biographical, constrained, regional).

% Scholars adhering to other schools of thought (Hanafi, Shafi'i, Hanbali) who contest the unique authority granted to Medinan practice. While they have their own schools, they face a rhetorical and jurisprudential challenge from the Maliki claim to a more authentic prophetic tradition.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, scholars_outside_maliki_school, payer,
    organized, biographical, mobile, global).

% The general Muslim population who receive a coherent and authoritative legal framework for their lives. They benefit from the stability and perceived authenticity of the Maliki school's rulings, but have no direct input into the jurisprudential method itself.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, muslim_laity, beneficiary,
    powerless, biographical, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a consistent and authoritative method for deriving Islamic law, ensuring legal coherence and stability within the Maliki school by prioritizing the living tradition of Medina as a source of prophetic practice.
% TRANSFER_FUNCTION: Transfers interpretive authority and legitimacy from diverse scholarly traditions to the Maliki school, specifically privileging the 'amal ahl al-Madina (practice of the people of Medina) as a primary source of law, thereby concentrating influence within the Medinan scholarly lineage.
% ABSENT_VOICES: Early jurists from other regions (e.g., Kufa, Syria) whose local practices and interpretive methods were not given the same weight would object. Their voices are absent from the Maliki framework's foundational claims, which implicitly devalues their contributions.
% DISAPPEARANCE_RATIONALE: If the Maliki jurisprudential method, particularly the authority of Medinan practice, vanished, the legal landscape in regions historically influenced by the Maliki school would undergo significant rearrangement. Other schools' methods would gain prominence, and the basis for legal rulings would shift, leading to a re-evaluation of countless legal precedents.
% FOUNDING_PROBLEM: The early Muslim community faced the challenge of establishing a consistent legal system after the Prophet's death, with diverse interpretations of the Qur'an and Hadith emerging across different regions. The Maliki school sought to ground law in the most authentic and direct transmission of prophetic practice.
% FOUNDING_PROBLEM_CORROBORATION: Maliki scholars attest that the problem of authentic legal derivation remains live, as new challenges require consistent application of foundational principles. Scholars from other schools, while disagreeing on the method, corroborate the historical problem of legal diversity and the need for methodological rigor in Islamic law.
narrative_ontology:disappearance_verdict(jurisprudential_method_kernel__maliki_reading, world_rearranges).
narrative_ontology:founding_problem_status(jurisprudential_method_kernel__maliki_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jurisprudential_method_kernel__maliki_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(jurisprudential_method_kernel__maliki_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jurisprudential_method_kernel__maliki_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jurisprudential_method_kernel__maliki_reading_tests).
:- end_tests(jurisprudential_method_kernel__maliki_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) arises from the implicit devaluation of alternative interpretive claims and the concentration of authority within the Maliki school, particularly its Medinan branch. Suppression (0.6) is present in the rhetorical and institutional pressure to conform to this methodological hierarchy, making it harder for non-Medinan practices to gain equal footing. Theater ratio is low (0.1) as the method is genuinely applied and forms the backbone of Maliki legal reasoning, not merely a performance. The historical measurements show a gradual increase in extractiveness and suppression as the Maliki school solidified its position and its methodology became more entrenched.
 *
 * PERSPECTIVAL GAP:
 *   From the Maliki perspective, this method is a Rope, a necessary coordination mechanism for authentic legal derivation. From the perspective of other schools, particularly those whose methods are implicitly devalued, it functions as a Tangled Rope or even a Snare, extracting authority and suppressing alternative, equally valid, interpretive paths. The engine's classification will reflect this divergence based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The Maliki scholarly lineage and Medinan community scholars are beneficiaries, as their interpretive authority is structurally enhanced. Non-Medinan interpretive claims and scholars outside the Maliki school are payers, as their methods are implicitly or explicitly subordinated. The Muslim laity are diffuse beneficiaries of legal stability but bear no direct costs or benefits from the methodological contest itself.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    authenticity_of_medinan_practice,
    'To what extent did the ''amal ahl al-Madina (practice of the people of Medina) genuinely preserve the Prophet''s practice more faithfully than other regional traditions?',
    'Historical-critical analysis of early Islamic legal texts, comparing chains of transmission and regional variations in practice, independent of later school affiliations.',
    'If Medinan practice is shown to be demonstrably more authentic, it would strengthen the Maliki claim to a Mountain-like foundation. If not, it would expose the claim as a constructed justification for a Tangled Rope or Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authenticity_of_medinan_practice, empirical, 'Empirical basis for the claim of Medinan authenticity.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of non-Medinan interpretive claims structural (institutionalized hierarchy) or internalized (scholars self-censor to align with dominant tradition)?',
    'Analysis of scholarly debates and fatwas: if non-Maliki methods are actively rejected by Maliki institutions, it''s structural. If scholars from other schools voluntarily adopt Maliki methods for legitimacy, it''s internalized.',
    'If internalized, the effective suppression is higher than the structural measure suggests, as it operates even without direct institutional enforcement. If purely structural, removing institutional barriers would resolve the suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for interpretive claims.').

omega_variable(
    maliki_reading_framing_underdetermination,
    'Is the Maliki school''s emphasis on Medinan practice a genuine methodological principle, or a post-hoc justification for a regional identity and power base?',
    'Comparative historical analysis of the school''s formation, examining the interplay between jurisprudential arguments and socio-political power dynamics in early Islamic history.',
    'If primarily a methodological principle, the constraint leans towards a Rope or Tangled Rope. If primarily a power justification, it leans towards a Snare, with the coordination story serving as cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maliki_reading_framing_underdetermination, conceptual, 'Conceptual ambiguity between methodological principle and power justification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jurisprudential_method_kernel__maliki_reading, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(juri_tr_t0, jurisprudential_method_kernel__maliki_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(juri_tr_t300, jurisprudential_method_kernel__maliki_reading, theater_ratio, 300, 0.08).
narrative_ontology:measurement(juri_tr_t600, jurisprudential_method_kernel__maliki_reading, theater_ratio, 600, 0.1).
narrative_ontology:measurement(juri_tr_t900, jurisprudential_method_kernel__maliki_reading, theater_ratio, 900, 0.1).
narrative_ontology:measurement(juri_tr_t1200, jurisprudential_method_kernel__maliki_reading, theater_ratio, 1200, 0.1).

% Extraction over time
narrative_ontology:measurement(juri_be_t0, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(juri_be_t300, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 300, 0.38).
narrative_ontology:measurement(juri_be_t600, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 600, 0.42).
narrative_ontology:measurement(juri_be_t900, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 900, 0.44).
narrative_ontology:measurement(juri_be_t1200, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 1200, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(juri_su_t0, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(juri_su_t300, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 300, 0.5).
narrative_ontology:measurement(juri_su_t600, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 600, 0.55).
narrative_ontology:measurement(juri_su_t900, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 900, 0.58).
narrative_ontology:measurement(juri_su_t1200, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 1200, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jurisprudential_method_kernel__maliki_reading, identity_coordination).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__maliki_reading, jurisprudential_method_kernel__hanafi_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__maliki_reading, jurisprudential_method_kernel__shafii_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__maliki_reading, jurisprudential_method_kernel__hanbali_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'jurisprudential_method_kernel'. Its claims about Medinan practice influence the perceived legitimacy and authority of other jurisprudential methods, creating a network of contestation among the schools.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
