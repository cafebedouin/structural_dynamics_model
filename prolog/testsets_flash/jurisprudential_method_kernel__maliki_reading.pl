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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: jurisprudential_method_kernel__maliki_reading
 *   human_readable: Maliki Jurisprudential Method: 'Amal Ahl al-Madina as Source of Law
 *   domain: islamic_jurisprudence/legal_philosophy/institutional_history
 *
 * SUMMARY:
 *   This constraint describes the Maliki school's jurisprudential method,
 *   which asserts the living tradition ('amal ahl al-Madina) as a valid
 *   source of law, alongside Qur'an and Hadith, due to Medina's perceived
 *   faithful preservation of the Prophet's practice. This is one reading of
 *   the broader 'jurisprudential_method_kernel' which encompasses the
 *   foundational disagreements between the major Sunni legal schools. The
 *   constraint is claimed as a Tangled Rope because it provides a
 *   coordination function (legal consistency) but also extracts by elevating
 *   one interpretive lineage and devaluing others.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jurisprudential_method_kernel__maliki_reading, 0.6).
domain_priors:suppression_score(jurisprudential_method_kernel__maliki_reading, 0.7).
domain_priors:theater_ratio(jurisprudential_method_kernel__maliki_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__maliki_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jurisprudential_method_kernel__maliki_reading, tangled_rope).
narrative_ontology:human_readable(jurisprudential_method_kernel__maliki_reading, "Maliki Jurisprudential Method: 'Amal Ahl al-Madina as Source of Law").
narrative_ontology:topic_domain(jurisprudential_method_kernel__maliki_reading, "islamic_jurisprudence/legal_philosophy/institutional_history").

domain_priors:requires_active_enforcement(jurisprudential_method_kernel__maliki_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jurisprudential_method_kernel__maliki_reading, '15b4876f-5849-44cf-a34e-a3a77f44f2ef').
narrative_ontology:cs_kernel_codification('15b4876f-5849-44cf-a34e-a3a77f44f2ef', formalized).
narrative_ontology:cs_authority_grounding('15b4876f-5849-44cf-a34e-a3a77f44f2ef', lineage).
narrative_ontology:cs_interpretation_layer_present('15b4876f-5849-44cf-a34e-a3a77f44f2ef').
narrative_ontology:cs_reading_relation('15b4876f-5849-44cf-a34e-a3a77f44f2ef', jurisprudential_method_kernel__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('15b4876f-5849-44cf-a34e-a3a77f44f2ef', jurisprudential_method_kernel__shafii_reading, coexists_with).
narrative_ontology:cs_reading_relation('15b4876f-5849-44cf-a34e-a3a77f44f2ef', jurisprudential_method_kernel__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('15b4876f-5849-44cf-a34e-a3a77f44f2ef', foundational, amal_ahl_al_madina_is_authoritative_source).
narrative_ontology:cs_axiom_status(amal_ahl_al_madina_is_authoritative_source, holdable).
narrative_ontology:cs_axiom_grounding('15b4876f-5849-44cf-a34e-a3a77f44f2ef', amal_ahl_al_madina_is_authoritative_source, conventional).
narrative_ontology:cs_axiom('15b4876f-5849-44cf-a34e-a3a77f44f2ef', secondary, medina_preserved_prophets_practice_most_faithfully).
narrative_ontology:cs_axiom_status(medina_preserved_prophets_practice_most_faithfully, holdable).
narrative_ontology:cs_axiom_grounding('15b4876f-5849-44cf-a34e-a3a77f44f2ef', medina_preserved_prophets_practice_most_faithfully, empirically_contingent).
narrative_ontology:cs_reference_frame('15b4876f-5849-44cf-a34e-a3a77f44f2ef', early_medinan_practice_as_normative).
narrative_ontology:cs_drift_state('15b4876f-5849-44cf-a34e-a3a77f44f2ef', contemporary_global_islamic_discourse, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('15b4876f-5849-44cf-a34e-a3a77f44f2ef', '').
narrative_ontology:cs_kernel_id(jurisprudential_method_kernel__maliki_reading, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__maliki_reading, maliki_scholarly_lineage).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__maliki_reading, medinan_community_leaders).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__maliki_reading, non_medinan_interpretive_claims).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__maliki_reading, scholars_prioritizing_individual_hadith).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__maliki_reading, muslim_laity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary interpreters and enforcers of the Maliki method, they benefit from the authority granted to Medinan practice, which elevates their interpretive tradition and gives them a privileged position in legal discourse. Their careers and institutional power are tied to the persistence of this methodology.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, maliki_scholarly_lineage, agenda_setter,
    institutional, generational, identity_locked, regional).

% Benefit from the elevation of Medinan practice, which reinforces their local authority and the perceived authenticity of their community's customs. This provides a stable framework for local governance and social cohesion, but also limits their ability to adopt external legal innovations.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, medinan_community_leaders, beneficiary,
    organized, biographical, constrained, local).

% Interpretive claims from other regions or schools that do not prioritize Medinan practice are implicitly devalued or challenged, forcing them to justify their methodologies against the Maliki standard. This creates a competitive disadvantage in establishing legal authority.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, non_medinan_interpretive_claims, payer,
    powerful, generational, constrained, global).

% Scholars who might prioritize the authenticity of individual Hadith transmissions over the collective practice of Medina find their arguments weakened within the Maliki framework. They bear the cost of having to reconcile their findings with the 'amal ahl al-Madina or be marginalized.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, scholars_prioritizing_individual_hadith, payer,
    moderate, biographical, constrained, global).

% Receive a consistent and historically grounded legal framework that provides clear guidance for daily life and religious practice. Their benefit is stability and perceived authenticity, but they have no direct influence over the jurisprudential method itself.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, muslim_laity, beneficiary,
    powerless, immediate, trapped, local).

% Jurists from the Hanafi school, who emphasize analogical reasoning and juristic preference, find their methodological claims challenged by the Maliki emphasis on Medinan practice. While not directly paying, their interpretive approach is implicitly excluded from the Maliki framework's highest authority.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__maliki_reading, hanafi_jurists, excluded,
    institutional, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a consistent and authoritative method for deriving Islamic law, ensuring legal stability and coherence within the Maliki school by prioritizing the preserved practice of the Prophet's community in Medina.
% TRANSFER_FUNCTION: Transfers interpretive authority and legitimacy from broader textual analysis or individual Hadith transmission to the collective practice ('amal) of the Medinan community, thereby elevating the Maliki scholarly lineage and Medinan leaders.
% ABSENT_VOICES: Scholars from other schools (Hanafi, Hanbali, Shafii) who prioritize different methodological principles (e.g., extensive qiyas, strict textualism, hierarchical sources) are implicitly sidelined; they would argue for the validity of their own methods as equally authentic derivations of divine intent.
% DISAPPEARANCE_RATIONALE: If the Maliki method's emphasis on 'amal ahl al-Madina vanished, the entire Maliki legal school would lose its foundational distinctive principle. Legal rulings would be re-evaluated, scholarly hierarchies would shift, and the authority of Medinan practice would be significantly diminished, leading to a major reorganization of Islamic jurisprudence.
% FOUNDING_PROBLEM: The early Islamic community faced the challenge of systematizing legal rulings after the Prophet's death, with diverse opinions and practices emerging across different regions. The Maliki method sought to anchor law in the most authentic and direct transmission of the Prophet's sunnah, as preserved in Medina.
% FOUNDING_PROBLEM_CORROBORATION: Maliki scholars and adherents universally attest that the problem of authentic legal derivation remains live, citing the need for a consistent methodology. While other schools dispute the Maliki solution, the underlying problem of legal authority and consistency is widely acknowledged across Islamic jurisprudence, corroborated by historical texts and ongoing scholarly debates.
narrative_ontology:disappearance_verdict(jurisprudential_method_kernel__maliki_reading, world_rearranges).
narrative_ontology:founding_problem_status(jurisprudential_method_kernel__maliki_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jurisprudential_method_kernel__maliki_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(jurisprudential_method_kernel__maliki_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jurisprudential_method_kernel__maliki_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jurisprudential_method_kernel__maliki_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jurisprudential_method_kernel__maliki_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is medium (0.6) because while the method provides a clear framework, it implicitly devalues alternative interpretive claims, creating a cost for those outside the Medinan tradition. Suppression is high (0.7) as the method requires active intellectual and institutional defense against rival methodologies, limiting the 'exit options' for scholars who might prefer other approaches. Theater ratio is low (0.2) as the method is genuinely applied and debated, not merely performed, though some aspects of its historical justification may be performative.
 *
 * PERSPECTIVAL GAP:
 *   From the Maliki perspective, this method is a pure Rope, providing essential coordination and authenticity. From the perspective of other schools, it is a Snare or Tangled Rope, extracting authority and suppressing alternative, equally valid, interpretive paths. The engine's classification will reflect this divergence based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   The Maliki scholarly lineage and Medinan community leaders are beneficiaries, as their authority is directly enhanced by this method. Non-Medinan interpretive claims and scholars prioritizing individual Hadith are victims, as their approaches are structurally disadvantaged. Muslim laity are diffuse beneficiaries of legal stability but have no direct agency in the method's perpetuation. Hanafi jurists are excluded, as their core methodology is implicitly challenged.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_authenticity_of_amal,
    'To what extent can the ''amal ahl al-Madina (practice of the people of Medina) be historically verified as a direct and uncorrupted transmission of the Prophet''s sunnah, distinct from later Medinan scholarly consensus?',
    'Further historical and archaeological research into early Medinan social and legal practices, and critical re-evaluation of early Maliki sources regarding the nature of ''amal.',
    'If ''amal is found to be more a product of later scholarly consensus than direct prophetic practice, its authority as a primary source would diminish, potentially reclassifying the constraint towards a Snare by revealing a weaker coordination function and stronger extraction of interpretive authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_authenticity_of_amal, empirical, 'The historical basis for the authority of Medinan practice.').

omega_variable(
    comparative_legitimacy_of_methodologies,
    'Is the Maliki emphasis on Medinan practice a genuinely superior method for deriving divine intent, or one among several equally legitimate, context-dependent approaches?',
    'A meta-jurisprudential analysis that evaluates the internal consistency, comprehensiveness, and ethical outcomes of various schools'' methodologies without a priori commitment to any single one.',
    'If found to be merely one among equals, the constraint''s claim to unique authenticity would weaken, increasing its effective extractiveness by exposing the arbitrary nature of its privileged position. If found genuinely superior, its Rope-like coordination function would be reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(comparative_legitimacy_of_methodologies, conceptual, 'The conceptual legitimacy of the Maliki method relative to other schools.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jurisprudential_method_kernel__maliki_reading, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(juri_tr_t0, jurisprudential_method_kernel__maliki_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(juri_tr_t300, jurisprudential_method_kernel__maliki_reading, theater_ratio, 300, 0.15).
narrative_ontology:measurement(juri_tr_t600, jurisprudential_method_kernel__maliki_reading, theater_ratio, 600, 0.18).
narrative_ontology:measurement(juri_tr_t900, jurisprudential_method_kernel__maliki_reading, theater_ratio, 900, 0.19).
narrative_ontology:measurement(juri_tr_t1200, jurisprudential_method_kernel__maliki_reading, theater_ratio, 1200, 0.2).

% Extraction over time
narrative_ontology:measurement(juri_be_t0, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(juri_be_t300, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 300, 0.5).
narrative_ontology:measurement(juri_be_t600, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 600, 0.55).
narrative_ontology:measurement(juri_be_t900, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 900, 0.58).
narrative_ontology:measurement(juri_be_t1200, jurisprudential_method_kernel__maliki_reading, base_extractiveness, 1200, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(juri_su_t0, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(juri_su_t300, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 300, 0.58).
narrative_ontology:measurement(juri_su_t600, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 600, 0.65).
narrative_ontology:measurement(juri_su_t900, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 900, 0.68).
narrative_ontology:measurement(juri_su_t1200, jurisprudential_method_kernel__maliki_reading, suppression_requirement, 1200, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jurisprudential_method_kernel__maliki_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__maliki_reading, jurisprudential_method_kernel__hanafi_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__maliki_reading, jurisprudential_method_kernel__shafii_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__maliki_reading, jurisprudential_method_kernel__hanbali_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'jurisprudential_method_kernel', which describes the foundational disagreements between the major Sunni legal schools. This Maliki reading emphasizes 'amal ahl al-Madina, distinguishing it from other schools' approaches to sources and methodology.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
