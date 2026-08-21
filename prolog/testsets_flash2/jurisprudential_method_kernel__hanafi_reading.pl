% ============================================================================
% CONSTRAINT STORY: jurisprudential_method_kernel__hanafi_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jurisprudential_method_kernel__hanafi_reading, []).

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
 *   constraint_id: jurisprudential_method_kernel__hanafi_reading
 *   human_readable: Hanafi Jurisprudential Method: Analogical Reasoning and Juristic Preference
 *   domain: islamic_jurisprudence/legal_philosophy/institutional_history
 *
 * SUMMARY:
 *   This constraint describes the Hanafi school's jurisprudential method,
 *   which emphasizes analogical reasoning (qiyas) and juristic preference
 *   (istihsan) as legitimate tools for extending divine intent beyond the
 *   literal texts of the Qur'an and Hadith. This reading is one of several
 *   competing methodologies within Islamic jurisprudence, each defining the
 *   sources and methods of law differently. The constraint is framed as a
 *   Tangled Rope because it genuinely coordinates the application of divine
 *   law to novel cases, but also extracts authority and interpretive power to
 *   a specific class of jurists, suppressing alternative, more textualist
 *   approaches.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jurisprudential_method_kernel__hanafi_reading, 0.65).
domain_priors:suppression_score(jurisprudential_method_kernel__hanafi_reading, 0.45).
domain_priors:theater_ratio(jurisprudential_method_kernel__hanafi_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jurisprudential_method_kernel__hanafi_reading, tangled_rope).
narrative_ontology:human_readable(jurisprudential_method_kernel__hanafi_reading, "Hanafi Jurisprudential Method: Analogical Reasoning and Juristic Preference").
narrative_ontology:topic_domain(jurisprudential_method_kernel__hanafi_reading, "islamic_jurisprudence/legal_philosophy/institutional_history").

domain_priors:requires_active_enforcement(jurisprudential_method_kernel__hanafi_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jurisprudential_method_kernel__hanafi_reading, '0d708b53-8ffa-420a-aa9e-6abb2ef10f6d').
narrative_ontology:cs_kernel_codification('0d708b53-8ffa-420a-aa9e-6abb2ef10f6d', formalized).
narrative_ontology:cs_authority_grounding('0d708b53-8ffa-420a-aa9e-6abb2ef10f6d', lineage).
narrative_ontology:cs_interpretation_layer_present('0d708b53-8ffa-420a-aa9e-6abb2ef10f6d').
narrative_ontology:cs_reading_relation('0d708b53-8ffa-420a-aa9e-6abb2ef10f6d', jurisprudential_method_kernel__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('0d708b53-8ffa-420a-aa9e-6abb2ef10f6d', jurisprudential_method_kernel__shafii_reading, coexists_with).
narrative_ontology:cs_reading_relation('0d708b53-8ffa-420a-aa9e-6abb2ef10f6d', jurisprudential_method_kernel__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('0d708b53-8ffa-420a-aa9e-6abb2ef10f6d', foundational, reason_extends_divine_intent).
narrative_ontology:cs_axiom_status(reason_extends_divine_intent, holdable).
narrative_ontology:cs_axiom_grounding('0d708b53-8ffa-420a-aa9e-6abb2ef10f6d', reason_extends_divine_intent, deontological).
narrative_ontology:cs_axiom('0d708b53-8ffa-420a-aa9e-6abb2ef10f6d', secondary, juristic_preference_serves_justice).
narrative_ontology:cs_axiom_status(juristic_preference_serves_justice, holdable).
narrative_ontology:cs_axiom_grounding('0d708b53-8ffa-420a-aa9e-6abb2ef10f6d', juristic_preference_serves_justice, instrumental).
narrative_ontology:cs_reference_frame('0d708b53-8ffa-420a-aa9e-6abb2ef10f6d', early_hanafi_rationalism).
narrative_ontology:cs_drift_state('0d708b53-8ffa-420a-aa9e-6abb2ef10f6d', contemporary_islamic_scholarship, gap(stable, minor, true)).
narrative_ontology:cs_created_at('0d708b53-8ffa-420a-aa9e-6abb2ef10f6d', '').
narrative_ontology:cs_kernel_id(jurisprudential_method_kernel__hanafi_reading, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanafi_reading, hanafi_jurists).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanafi_reading, rationalist_scholars).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanafi_reading, textualist_claimants).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanafi_reading, lay_community_seeking_simple_rulings).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These jurists interpret and extend divine law using qiyas (analogical reasoning) and istihsan (juristic preference), allowing for flexibility and adaptation to novel cases. Their authority is grounded in their rationalist training and mastery of these methods, which they administer and teach.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, hanafi_jurists, agenda_setter,
    institutional, generational, identity_locked, global).

% Benefit from a framework that legitimizes intellectual inquiry and the application of reason in legal derivation. Their careers and influence are tied to the continued acceptance and development of these rationalist tools within the Hanafi school.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, rationalist_scholars, beneficiary,
    organized, biographical, constrained, global).

% Bear the cost of having their literalist interpretations of Qur'an and Hadith superseded or reinterpreted by analogical reasoning and juristic preference. They often view these methods as innovations that dilute the purity of divine revelation.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, textualist_claimants, payer,
    moderate, biographical, constrained, regional).

% May find the complex, reasoned derivations of Hanafi jurisprudence opaque or difficult to reconcile with simpler, more direct textual interpretations. They rely on jurists for guidance but may feel disempowered by the interpretive layers.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, lay_community_seeking_simple_rulings, payer,
    powerless, immediate, trapped, local).

% While a distinct school, their emphasis on Medinan practice as a source of law differs fundamentally from Hanafi rationalism. They are excluded from the Hanafi interpretive framework but maintain their own parallel system.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, maliki_jurists, excluded,
    institutional, generational, identity_locked, global).

% Strongly textualist, they reject qiyas and istihsan as illegitimate innovations. Their exclusion from the Hanafi framework is mutual, representing a fundamental disagreement on methodological legitimacy.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, hanbali_jurists, excluded,
    institutional, generational, identity_locked, global).

% Their strict hierarchical methodology, while accepting qiyas, places it lower than ijma (consensus) and emphasizes rigorous hadith authentication, differing from the Hanafi school's more expansive use of rational tools.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, shafii_jurists, excluded,
    institutional, generational, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent and adaptable legal framework for Muslim communities, allowing divine law to address new circumstances and complex societal issues not explicitly covered in foundational texts, thereby maintaining the law's relevance and applicability across diverse contexts.
% TRANSFER_FUNCTION: Transfers interpretive authority and the power to derive new rulings from direct textual interpretation to a class of trained jurists skilled in analogical reasoning and juristic preference. This creates a demand for specialized legal scholarship.
% ABSENT_VOICES: Strict textualists and those advocating for a more direct, less mediated access to divine law are structurally marginalized. They would argue for the sufficiency of Qur'an and Hadith alone, or for a more constrained role for human reason, but their methodological premises are foreclosed by the Hanafi framework.
% DISAPPEARANCE_RATIONALE: If the Hanafi jurisprudential method vanished, the vast body of derived law would lose its methodological grounding. Hanafi-majority regions would face a legal vacuum, requiring a complete re-evaluation of how divine intent is applied to daily life, likely leading to fragmentation or adoption of other schools' methods.
% FOUNDING_PROBLEM: The early Muslim community faced novel legal questions not directly addressed in the Qur'an or Hadith, requiring a method to extend divine intent to new circumstances while maintaining fidelity to revelation.
% FOUNDING_PROBLEM_CORROBORATION: Hanafi scholars universally attest to the ongoing need for these methods to address contemporary issues like bioethics, digital finance, and international law. While other schools offer different solutions, the problem of extending divine law to novel cases remains live across Islamic jurisprudence, corroborated by the continuous production of fatwas (legal opinions) on new matters.
narrative_ontology:disappearance_verdict(jurisprudential_method_kernel__hanafi_reading, world_rearranges).
narrative_ontology:founding_problem_status(jurisprudential_method_kernel__hanafi_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jurisprudential_method_kernel__hanafi_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(jurisprudential_method_kernel__hanafi_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jurisprudential_method_kernel__hanafi_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jurisprudential_method_kernel__hanafi_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jurisprudential_method_kernel__hanafi_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jurisprudential_method_kernel__hanafi_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.65) because the method grants significant interpretive power to jurists, allowing for derivations that may not be immediately obvious from the foundational texts, thus creating a 'cost' for those who prefer direct textualism. Suppression (0.45) is present as the Hanafi school actively defends its methodology against rival schools that reject qiyas or istihsan, requiring active intellectual and institutional enforcement to maintain its legitimacy. Theater ratio is low (0.1) as the method is genuinely applied and functional, not merely performative. The historical measurements show a gradual increase in extractiveness and suppression as the school solidified its position and defended its methodology over centuries.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Hanafi jurists, this method is a necessary and divinely sanctioned means of ensuring the law's adaptability and justice. From the perspective of textualists, it represents an unwarranted human intrusion into divine prerogative, leading to 'extracted' interpretations. The engine's classification will reflect this divergence based on the declared structural relationships and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   Hanafi jurists and rationalist scholars are beneficiaries, as the method legitimizes their intellectual tools and grants them interpretive authority. Textualist claimants and the lay community seeking simple rulings are victims, as their preferred modes of legal derivation are either superseded or made inaccessible by the complexity of the Hanafi method. Other schools' jurists are excluded, as their methodologies are fundamentally incompatible or competing.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    qiyas_istihsan_legitimacy,
    'Is the use of qiyas and istihsan a legitimate extension of divine intent, or an innovation (bid''ah) that corrupts the kernel?',
    'Theological and jurisprudential consensus across all major schools, or a definitive textual discovery that explicitly sanctions or prohibits these methods.',
    'If deemed illegitimate, the Hanafi method would collapse as a valid source of law, reclassifying it as a Snare. If universally affirmed, its extractiveness would decrease as the ''cost'' of its interpretive authority would be seen as legitimate coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(qiyas_istihsan_legitimacy, conceptual, 'The fundamental theological and methodological legitimacy of Hanafi rationalist tools.').

omega_variable(
    interpretive_authority_concentration,
    'Does the Hanafi method''s reliance on qiyas and istihsan lead to an undue concentration of interpretive authority in the hands of a specialized elite, or is this a necessary function of legal expertise?',
    'Empirical study of access to legal education and interpretive roles within Hanafi-majority societies, compared to societies governed by more textualist schools.',
    'If it leads to undue concentration, the extractiveness metric would be further amplified for the lay community, pushing the classification closer to a Snare. If it''s a necessary function, the coordination aspect would be emphasized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_authority_concentration, empirical, 'The social and power implications of the Hanafi method''s interpretive complexity.').

omega_variable(
    kernel_reading_identity,
    'This constraint is a specific reading of the ''jurisprudential_method_kernel''. What would change structurally if a sibling reading (e.g., Hanbali textualism) were adopted as the dominant framework?',
    'Comparative analysis of legal systems and scholarly output under different dominant schools.',
    'Adopting a Hanbali reading would drastically reduce the legitimacy of rationalist tools, shifting beneficiaries and victims, and likely reclassifying the constraint to a Mountain (for textualists) or a Snare (for rationalists). The disagreement is located in the ''sources of law'' and ''methodology'' components of the kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Impact of adopting a sibling reading of the jurisprudential method kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jurisprudential_method_kernel__hanafi_reading, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(juri_tr_t0, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(juri_tr_t300, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 300, 0.07).
narrative_ontology:measurement(juri_tr_t600, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 600, 0.08).
narrative_ontology:measurement(juri_tr_t900, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 900, 0.09).
narrative_ontology:measurement(juri_tr_t1200, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 1200, 0.1).

% Extraction over time
narrative_ontology:measurement(juri_be_t0, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(juri_be_t300, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 300, 0.55).
narrative_ontology:measurement(juri_be_t600, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 600, 0.6).
narrative_ontology:measurement(juri_be_t900, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 900, 0.63).
narrative_ontology:measurement(juri_be_t1200, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 1200, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(juri_su_t0, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(juri_su_t300, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 300, 0.38).
narrative_ontology:measurement(juri_su_t600, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 600, 0.4).
narrative_ontology:measurement(juri_su_t900, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 900, 0.43).
narrative_ontology:measurement(juri_su_t1200, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 1200, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jurisprudential_method_kernel__hanafi_reading, identity_coordination).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanafi_reading, jurisprudential_method_kernel__maliki_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanafi_reading, jurisprudential_method_kernel__shafii_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanafi_reading, jurisprudential_method_kernel__hanbali_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the broader 'jurisprudential_method_kernel'. Its ε value reflects the specific Hanafi emphasis on rationalist tools, which differs significantly from other schools' approaches. Other readings are modeled as separate constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
