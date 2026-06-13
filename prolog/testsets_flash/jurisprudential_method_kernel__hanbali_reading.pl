% ============================================================================
% CONSTRAINT STORY: jurisprudential_method_kernel__hanbali_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jurisprudential_method_kernel__hanbali_reading, []).

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
 *   constraint_id: jurisprudential_method_kernel__hanbali_reading
 *   human_readable: Hanbali Jurisprudential Method: Textual Literalism
 *   domain: islamic_jurisprudence/legal_philosophy/institutional_history
 *
 * SUMMARY:
 *   This constraint describes the Hanbali reading of Islamic jurisprudential
 *   method, which emphasizes strict adherence to the literal text of the
 *   Qur'an and Hadith, and the opinions of the Companions. It actively
 *   rejects analogical reasoning (qiyas) and juristic preference (istihsan)
 *   as illegitimate innovations (bid'ah). This reading functions as a snare,
 *   as it extracts interpretive authority from rationalist jurists and
 *   customary practices, suppressing alternative methodologies to benefit
 *   textualist scholars and conservative religious authorities. The high
 *   extractiveness and suppression reflect the active delegitimization and
 *   marginalization of competing interpretive schools.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jurisprudential_method_kernel__hanbali_reading, 0.85).
domain_priors:suppression_score(jurisprudential_method_kernel__hanbali_reading, 0.75).
domain_priors:theater_ratio(jurisprudential_method_kernel__hanbali_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jurisprudential_method_kernel__hanbali_reading, snare).
narrative_ontology:human_readable(jurisprudential_method_kernel__hanbali_reading, "Hanbali Jurisprudential Method: Textual Literalism").
narrative_ontology:topic_domain(jurisprudential_method_kernel__hanbali_reading, "islamic_jurisprudence/legal_philosophy/institutional_history").

domain_priors:requires_active_enforcement(jurisprudential_method_kernel__hanbali_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jurisprudential_method_kernel__hanbali_reading, 'a1813e49-0afa-46ca-90e3-3bac132de92c').
narrative_ontology:cs_kernel_codification('a1813e49-0afa-46ca-90e3-3bac132de92c', fixed_text).
narrative_ontology:cs_authority_grounding('a1813e49-0afa-46ca-90e3-3bac132de92c', lineage).
narrative_ontology:cs_interpretation_layer_present('a1813e49-0afa-46ca-90e3-3bac132de92c').
narrative_ontology:cs_reading_relation('a1813e49-0afa-46ca-90e3-3bac132de92c', jurisprudential_method_kernel__hanafi_reading, forecloses).
narrative_ontology:cs_reading_relation('a1813e49-0afa-46ca-90e3-3bac132de92c', jurisprudential_method_kernel__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('a1813e49-0afa-46ca-90e3-3bac132de92c', jurisprudential_method_kernel__shafii_reading, forecloses).
narrative_ontology:cs_axiom('a1813e49-0afa-46ca-90e3-3bac132de92c', foundational, literal_text_supremacy).
narrative_ontology:cs_axiom_status(literal_text_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('a1813e49-0afa-46ca-90e3-3bac132de92c', literal_text_supremacy, deontological).
narrative_ontology:cs_axiom('a1813e49-0afa-46ca-90e3-3bac132de92c', foundational, analogical_reasoning_is_bidah).
narrative_ontology:cs_axiom_status(analogical_reasoning_is_bidah, holdable).
narrative_ontology:cs_axiom_grounding('a1813e49-0afa-46ca-90e3-3bac132de92c', analogical_reasoning_is_bidah, theological).
narrative_ontology:cs_reference_frame('a1813e49-0afa-46ca-90e3-3bac132de92c', early_salafi_textualism).
narrative_ontology:cs_drift_state('a1813e49-0afa-46ca-90e3-3bac132de92c', contemporary_global_islam, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('a1813e49-0afa-46ca-90e3-3bac132de92c', '').
narrative_ontology:cs_kernel_id(jurisprudential_method_kernel__hanbali_reading, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanbali_reading, textualist_scholars).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanbali_reading, conservative_religious_authorities).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanbali_reading, rationalist_jurists).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanbali_reading, customary_practice_adherents).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanbali_reading, innovative_thinkers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These scholars uphold the Hanbali methodology, emphasizing strict adherence to the literal text of the Qur'an and Hadith, and the opinions of the Companions. They actively reject analogical reasoning and juristic preference as illegitimate innovations (bid'ah), thereby maintaining their authority as interpreters of the 'pure' tradition.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, textualist_scholars, agenda_setter,
    institutional, generational, identity_locked, global).

% Religious institutions and authorities in certain regions benefit from the Hanbali reading as it provides a clear, rigid framework that limits interpretive flexibility and reinforces their control over religious discourse and legal application. This reduces challenges to established norms.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, conservative_religious_authorities, beneficiary,
    institutional, generational, constrained, national).

% Jurists who advocate for analogical reasoning (qiyas) and juristic preference (istihsan) find their methodologies delegitimized and suppressed by the Hanbali reading. Their intellectual contributions are often dismissed as 'innovation,' limiting their influence and career progression within Hanbali-dominated institutions.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, rationalist_jurists, payer,
    powerful, biographical, constrained, global).

% Communities whose legal and social practices have evolved through local custom or non-textual reasoning find their traditions challenged and often declared invalid by the Hanbali emphasis on literal textual sources and unanimous consensus. This can lead to social disruption and legal uncertainty.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, customary_practice_adherents, payer,
    moderate, generational, identity_locked, local).

% Individuals seeking to apply Islamic principles to modern challenges through novel interpretive methods or contextual reasoning face severe intellectual and social penalties. Their work is often labeled as bid'ah, leading to marginalization, censorship, or even persecution in environments where the Hanbali reading is dominant.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, innovative_thinkers, payer,
    powerless, biographical, trapped, global).

% Jurists from the Hanafi school, which extensively uses analogical reasoning and juristic preference, are fundamentally at odds with the Hanbali methodology. While they operate in their own spheres, their interpretive tools are explicitly rejected by the Hanbali reading, making genuine dialogue or integration difficult.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, hanafi_jurists, excluded,
    institutional, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, unambiguous framework for deriving Islamic law, reducing interpretive variance and ensuring consistency across different legal rulings by limiting sources to literal texts and unanimous consensus.
% TRANSFER_FUNCTION: Transfers interpretive authority from individual juristic reasoning and evolving customary practices to a fixed body of literal texts and historical consensus, thereby consolidating power among textualist scholars and conservative authorities.
% ABSENT_VOICES: Rationalist jurists, proponents of juristic preference, and those who prioritize local customary practices are actively excluded from the Hanbali interpretive framework. They would argue for the necessity of reason and contextual adaptation in legal derivation, but their methods are deemed illegitimate innovations.
% DISAPPEARANCE_RATIONALE: If the Hanbali reading's strictures vanished, the landscape of Islamic jurisprudence would immediately diversify. Rationalist approaches would gain legitimacy, customary practices would be re-evaluated, and new interpretive methodologies would emerge, leading to a significant reorganization of legal and religious authority.
% FOUNDING_PROBLEM: The early Islamic community faced challenges in standardizing legal rulings across diverse regions and preventing arbitrary interpretations, leading to a need for a clear, authoritative methodology for deriving law from divine sources.
% FOUNDING_PROBLEM_CORROBORATION: The problem of interpretive fragmentation and the potential for 'innovation' (bid'ah) is still cited by textualist scholars and conservative authorities as a live concern, justifying the Hanbali approach. However, rationalist jurists and modern legal scholars, from outside the benefiting parties, argue that the Hanbali method itself creates rigidity that hinders addressing contemporary issues, suggesting the 'problem' has shifted or been exacerbated by the 'solution'.
narrative_ontology:disappearance_verdict(jurisprudential_method_kernel__hanbali_reading, world_rearranges).
narrative_ontology:founding_problem_status(jurisprudential_method_kernel__hanbali_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jurisprudential_method_kernel__hanbali_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(jurisprudential_method_kernel__hanbali_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jurisprudential_method_kernel__hanbali_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jurisprudential_method_kernel__hanbali_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jurisprudential_method_kernel__hanbali_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is high (0.85) because the Hanbali reading effectively monopolizes interpretive legitimacy, forcing adherence to its narrow methodology and invalidating alternative approaches. Suppression is also high (0.75) due to the active labeling of other methods as 'bid'ah,' which carries significant religious and social penalties, thereby suppressing intellectual dissent and innovation. The theater ratio is low (0.1) as the constraint is actively enforced and its function (limiting interpretation) is genuinely pursued, not merely performed.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of textualist scholars and conservative authorities (beneficiaries/agenda-setters), this method is a necessary 'rope' for preserving the purity of Islamic law. However, from the perspective of rationalist jurists and innovative thinkers (victims/payers), it operates as a 'snare' that stifles intellectual inquiry and imposes a rigid, often anachronistic, legal framework. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Textualist scholars and conservative religious authorities are clear beneficiaries (d near 0.0) as their interpretive authority is amplified and protected. Rationalist jurists, customary practice adherents, and innovative thinkers are targets (d near 1.0) as their methods are suppressed and their contributions delegitimized. Hanafi jurists are 'excluded' as their entire methodology is deemed outside the Hanbali framework, though they may operate in other contexts.
 *
 * MANDATROPHY ANALYSIS:
 *   The Hanbali reading, while initially addressing a genuine problem of interpretive fragmentation, has arguably outlived its original function in its most rigid form. Its continued insistence on a narrow textualism, despite the evolution of complex societal challenges, suggests a degree of mandatrophy. The classification as a snare prevents mislabeling this as pure coordination, highlighting the active extraction of interpretive freedom and suppression of alternative legal reasoning, even if it claims to uphold a 'pure' tradition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretive_legitimacy_source,
    'Is interpretive legitimacy derived solely from literal textual adherence and unanimous consensus, or does it also encompass rational inquiry and contextual adaptation?',
    'Analysis of historical and contemporary Islamic legal systems that successfully integrate rationalist methods without compromising core Islamic principles, or a re-evaluation of the historical context of ''bid''ah'' to distinguish between genuine innovation and harmful deviation.',
    'If rational inquiry is deemed legitimate, the Hanbali reading''s suppression of qiyas and istihsan would be reclassified as pure extraction, shifting its overall classification further towards a snare. If not, its current classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_legitimacy_source, conceptual, 'The fundamental source of legal interpretive authority in Islam.').

omega_variable(
    hanbali_reading_vs_kernel_intent,
    'Does the Hanbali reading accurately reflect the original intent of the jurisprudential method kernel, or is it a particular interpretation that has become dominant in certain contexts?',
    'Comparative historical analysis of early Islamic legal thought, examining the diversity of interpretive approaches among the Companions and early jurists before the formalization of the madhhabs.',
    'If it''s a particular interpretation, it would highlight the constructed nature of the constraint, potentially weakening its claim to ''natural'' or ''divine'' authority and exposing its extractive elements more clearly. If it''s the original intent, it would reinforce its claimed legitimacy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hanbali_reading_vs_kernel_intent, empirical, 'Whether the Hanbali reading is a faithful representation of the kernel''s original scope.').

omega_variable(
    bidah_definition_scope,
    'Is the Hanbali definition of ''bid''ah'' (innovation) overly broad, encompassing necessary intellectual development alongside genuinely harmful deviations?',
    'Scholarly consensus on a more nuanced taxonomy of innovation, distinguishing between ''good'' and ''bad'' bid''ah based on their alignment with broader Islamic objectives (maqasid al-shari''ah).',
    'A narrower definition of bid''ah would reduce the suppression metric by legitimizing more forms of reasoning, potentially shifting the constraint towards a tangled rope or even a rope if the coordination function becomes primary. A broad definition reinforces the snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bidah_definition_scope, conceptual, 'The scope and application of the concept of ''innovation'' (bid''ah).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jurisprudential_method_kernel__hanbali_reading, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(juri_tr_t0, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(juri_tr_t300, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 300, 0.1).
narrative_ontology:measurement(juri_tr_t600, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 600, 0.1).
narrative_ontology:measurement(juri_tr_t900, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 900, 0.1).
narrative_ontology:measurement(juri_tr_t1200, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 1200, 0.1).

% Extraction over time
narrative_ontology:measurement(juri_be_t0, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(juri_be_t300, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 300, 0.75).
narrative_ontology:measurement(juri_be_t600, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 600, 0.8).
narrative_ontology:measurement(juri_be_t900, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 900, 0.83).
narrative_ontology:measurement(juri_be_t1200, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 1200, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(juri_su_t0, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(juri_su_t300, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 300, 0.65).
narrative_ontology:measurement(juri_su_t600, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 600, 0.7).
narrative_ontology:measurement(juri_su_t900, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 900, 0.73).
narrative_ontology:measurement(juri_su_t1200, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 1200, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jurisprudential_method_kernel__hanbali_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanbali_reading, jurisprudential_method_kernel__hanafi_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanbali_reading, jurisprudential_method_kernel__maliki_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanbali_reading, jurisprudential_method_kernel__shafii_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the broader 'jurisprudential_method_kernel'. Its strict textualism and rejection of analogical reasoning stand in contrast to other schools, which are modeled as separate constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
