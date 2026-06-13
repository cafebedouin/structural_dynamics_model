% ============================================================================
% CONSTRAINT STORY: speech_harm_boundary__absolutist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_harm_boundary__absolutist_reading, []).

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
 *   constraint_id: speech_harm_boundary__absolutist_reading
 *   human_readable: Absolutist Reading of Speech Harm Boundary
 *   domain: constitutional_law/political_philosophy/communication_ethics
 *
 * SUMMARY:
 *   This constraint represents an 'absolutist' reading of free speech, where
 *   protection for expression is near-absolute and the threshold for
 *   overriding it due to harm is extremely high. This reading prioritizes
 *   speaker autonomy and robust public discourse, even at the cost of
 *   significant harm to individuals or groups. It is one reading of the
 *   broader 'speech_harm_boundary' kernel, which is contested by
 *   'harm_balancing_reading' and 'dignity_reading' siblings.
 *
 * KEY AGENTS:
 *   - speakers_with_controversial_views: Primary beneficiary (powerful/mobile) — benefits from broad protection
 *   - targets_of_harmful_speech: Primary victim (powerless/trapped) — bears the cost of high harm threshold
 *   - vulnerable_groups: Secondary victim (powerless/identity_locked) — disproportionately affected by harmful speech
 *   - judicial_authorities: Agenda setter (institutional/analytical) — interprets and enforces the harm boundary
 *   - public_discourse: Beneficiary (diffuse/civilizational) — benefits from robust, unconstrained speech
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_harm_boundary__absolutist_reading, 0.85).
domain_priors:suppression_score(speech_harm_boundary__absolutist_reading, 0.1).
domain_priors:theater_ratio(speech_harm_boundary__absolutist_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(speech_harm_boundary__absolutist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_harm_boundary__absolutist_reading, tangled_rope).
narrative_ontology:human_readable(speech_harm_boundary__absolutist_reading, "Absolutist Reading of Speech Harm Boundary").
narrative_ontology:topic_domain(speech_harm_boundary__absolutist_reading, "constitutional_law/political_philosophy/communication_ethics").

domain_priors:requires_active_enforcement(speech_harm_boundary__absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_harm_boundary__absolutist_reading, 'a6b09ae5-7689-40f2-b3a1-fdfb4e3b8f79').
narrative_ontology:cs_kernel_codification('a6b09ae5-7689-40f2-b3a1-fdfb4e3b8f79', fixed_text).
narrative_ontology:cs_authority_grounding('a6b09ae5-7689-40f2-b3a1-fdfb4e3b8f79', lineage).
narrative_ontology:cs_interpretation_layer_present('a6b09ae5-7689-40f2-b3a1-fdfb4e3b8f79').
narrative_ontology:cs_reading_relation('a6b09ae5-7689-40f2-b3a1-fdfb4e3b8f79', speech_harm_boundary__harm_balancing_reading, coexists_with).
narrative_ontology:cs_reading_relation('a6b09ae5-7689-40f2-b3a1-fdfb4e3b8f79', speech_harm_boundary__dignity_reading, coexists_with).
narrative_ontology:cs_axiom('a6b09ae5-7689-40f2-b3a1-fdfb4e3b8f79', foundational, speech_autonomy_maximization).
narrative_ontology:cs_axiom_status(speech_autonomy_maximization, holdable).
narrative_ontology:cs_axiom_grounding('a6b09ae5-7689-40f2-b3a1-fdfb4e3b8f79', speech_autonomy_maximization, deontological).
narrative_ontology:cs_axiom('a6b09ae5-7689-40f2-b3a1-fdfb4e3b8f79', foundational, harm_as_secondary_consideration).
narrative_ontology:cs_axiom_status(harm_as_secondary_consideration, holdable).
narrative_ontology:cs_axiom_grounding('a6b09ae5-7689-40f2-b3a1-fdfb4e3b8f79', harm_as_secondary_consideration, conventional).
narrative_ontology:cs_reference_frame('a6b09ae5-7689-40f2-b3a1-fdfb4e3b8f79', marketplace_of_ideas_ideal).
narrative_ontology:cs_drift_state('a6b09ae5-7689-40f2-b3a1-fdfb4e3b8f79', contemporary_digital_era, gap(stable, minor, false)).
narrative_ontology:cs_created_at('a6b09ae5-7689-40f2-b3a1-fdfb4e3b8f79', '').
narrative_ontology:cs_kernel_id(speech_harm_boundary__absolutist_reading, speech_harm_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_harm_boundary__absolutist_reading, speakers_with_controversial_views).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__absolutist_reading, public_discourse).
narrative_ontology:constraint_victim(speech_harm_boundary__absolutist_reading, targets_of_harmful_speech).
narrative_ontology:constraint_victim(speech_harm_boundary__absolutist_reading, vulnerable_groups).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_harm_boundary__absolutist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(speech_harm_boundary__absolutist_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_harm_boundary__absolutist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_harm_boundary__absolutist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_harm_boundary__absolutist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is high (0.85) because the constraint imposes significant costs (unmitigated harm) on targets for the benefit of speakers. Suppression is low (0.1) because the constraint's core function is to *prevent* suppression of speech, not to enforce it, except for the very narrow categories of unprotected speech. Theater ratio is low (0.05) as the constraint is actively and genuinely enforced according to its stated principles. Accessibility collapse is high (0.9) because, once the absolutist principle is accepted, alternatives for restricting speech are almost entirely foreclosed. Resistance is high (0.7) from those who bear the harms, leading to ongoing legal and social challenges.
 *
 * PERSPECTIVAL GAP:
 *   Speakers with controversial views experience this as a Rope or even a Mountain, as their speech is largely unconstrained. Targets of harmful speech, however, experience it as a Snare or Tangled Rope, as they bear significant costs with little recourse. Judicial authorities, acting as agenda setters, navigate this tension by upholding the high harm threshold, which benefits the abstract ideal of 'public discourse' but extracts from specific victims.
 *
 * DIRECTIONALITY LOGIC:
 *   Speakers with controversial views are full beneficiaries (d=0.0) as the constraint subsidizes their expression by externalizing harm costs. Targets of harmful speech are full targets (d=1.0) as they bear the direct and unmitigated costs. Vulnerable groups are also targets, often identity-locked, amplifying their effective extraction. Judicial authorities are agenda setters, balancing the abstract benefit of free speech with the concrete costs, but their enforcement of the high harm threshold structurally benefits speakers.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the traditional sense, as its 'mandate' (protecting speech) is still considered live. However, the *balance* of its mandate is contested: whether its original intent was to protect speech so absolutely that it imposes such high costs on targets, or if the high extractiveness is an unintended consequence of an absolutist interpretation. The 'founding_problem_status' being 'contested' reflects this ongoing debate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine absolutist reading of speech protection, or is it a rhetorical cover for other interests?',
    'Analysis of judicial decisions and legislative actions: if the unprotected categories expand or the harm threshold lowers in practice, it indicates a shift away from absolutism.',
    'If genuinely absolutist, it prioritizes speaker autonomy at high cost to targets. If rhetorical, it''s a snare for targets and a rope for powerful speakers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is the ''absolutist_reading'' of the ''speech_harm_boundary'' kernel.').

omega_variable(
    harm_threshold_objectivity,
    'Is the ''extremely high'' harm override threshold an objective standard, or is its application subject to interpretive bias favoring certain types of speech or speakers?',
    'Empirical study of how the threshold is applied across different contexts and against different speakers/targets; analysis of judicial reasoning for consistency.',
    'If biased, the effective extraction from vulnerable groups is higher than the stated threshold implies, making the constraint more Snare-like for them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_threshold_objectivity, empirical, 'Ambiguity in the application of the harm override threshold.').

omega_variable(
    sibling_reading_impact_dignity,
    'How would adopting the ''dignity_reading'' (speech protection subordinate to human dignity) structurally alter this constraint?',
    'Hypothetical legal analysis: if dignity were paramount, personhood-denying speech would be unprotected, significantly lowering the harm override threshold and expanding victim categories.',
    'The ''dignity_reading'' would transform this constraint from a Tangled Rope (high speaker autonomy, high harm cost) into a more balanced Rope or even a Mountain for dignity itself, by re-prioritizing the victim''s experience.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_impact_dignity, conceptual, 'Impact of the ''dignity_reading'' sibling on this constraint.').

omega_variable(
    sibling_reading_impact_harm_balancing,
    'How would adopting the ''harm_balancing_reading'' (speech protection yields to demonstrated harm) structurally alter this constraint?',
    'Comparative legal analysis: jurisdictions employing harm balancing typically have lower thresholds for intervention and a broader range of unprotected speech, based on proportionality.',
    'The ''harm_balancing_reading'' would reduce the extractiveness from targets by allowing more frequent overrides, shifting the constraint closer to a Rope or Scaffold, depending on the specific balancing test.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_impact_harm_balancing, conceptual, 'Impact of the ''harm_balancing_reading'' sibling on this constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_harm_boundary__absolutist_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_harm_boundary__absolutist_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(spee_be_t5, speech_harm_boundary__absolutist_reading, base_extractiveness, 5, 0.78).
narrative_ontology:measurement(spee_be_t10, speech_harm_boundary__absolutist_reading, base_extractiveness, 10, 0.81).
narrative_ontology:measurement(spee_be_t15, speech_harm_boundary__absolutist_reading, base_extractiveness, 15, 0.83).
narrative_ontology:measurement(spee_be_t20, speech_harm_boundary__absolutist_reading, base_extractiveness, 20, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_harm_boundary__absolutist_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(spee_su_t5, speech_harm_boundary__absolutist_reading, suppression_requirement, 5, 0.12).
narrative_ontology:measurement(spee_su_t10, speech_harm_boundary__absolutist_reading, suppression_requirement, 10, 0.11).
narrative_ontology:measurement(spee_su_t15, speech_harm_boundary__absolutist_reading, suppression_requirement, 15, 0.1).
narrative_ontology:measurement(spee_su_t20, speech_harm_boundary__absolutist_reading, suppression_requirement, 20, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_harm_boundary__absolutist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_harm_boundary__absolutist_reading, speech_harm_boundary__harm_balancing_reading).
narrative_ontology:affects_constraint(speech_harm_boundary__absolutist_reading, speech_harm_boundary__dignity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'speech_harm_boundary' kernel. Each reading has a different structural extractiveness and beneficiary/victim profile, necessitating separate constraint stories. This 'absolutist_reading' prioritizes speaker autonomy, while 'harm_balancing_reading' and 'dignity_reading' offer alternative frameworks for balancing speech with harm or dignity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
