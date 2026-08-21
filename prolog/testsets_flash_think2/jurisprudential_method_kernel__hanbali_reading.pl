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
 *   constraint_id: jurisprudential_method_kernel__hanbali_reading
 *   human_readable: Hanbali Jurisprudential Method: Textual Literalism and Innovation Rejection
 *   domain: Islamic Jurisprudence / Legal Philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the Hanbali reading of the jurisprudential
 *   method kernel, emphasizing strict textual literalism in deriving Islamic
 *   law from the Qur'an, Hadith, and Companion opinions. It actively rejects
 *   analogical reasoning (qiyas) and juristic preference (istihsan) as
 *   'bid'ah' (innovation), which are seen as corrupting the kernel. This
 *   reading asserts that only unanimous consensus (ijma) is a valid secondary
 *   source, further limiting interpretive flexibility. The high
 *   extractiveness and suppression reflect the cost borne by alternative
 *   methodologies and their proponents.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jurisprudential_method_kernel__hanbali_reading, 0.85).
domain_priors:suppression_score(jurisprudential_method_kernel__hanbali_reading, 0.9).
domain_priors:theater_ratio(jurisprudential_method_kernel__hanbali_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jurisprudential_method_kernel__hanbali_reading, snare).
narrative_ontology:human_readable(jurisprudential_method_kernel__hanbali_reading, "Hanbali Jurisprudential Method: Textual Literalism and Innovation Rejection").
narrative_ontology:topic_domain(jurisprudential_method_kernel__hanbali_reading, "Islamic Jurisprudence / Legal Philosophy").

domain_priors:requires_active_enforcement(jurisprudential_method_kernel__hanbali_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jurisprudential_method_kernel__hanbali_reading, '56bdf04b-249c-491f-b31d-f3883fc40fcb').
narrative_ontology:cs_kernel_codification('56bdf04b-249c-491f-b31d-f3883fc40fcb', fixed_text).
narrative_ontology:cs_authority_grounding('56bdf04b-249c-491f-b31d-f3883fc40fcb', lineage).
narrative_ontology:cs_interpretation_layer_present('56bdf04b-249c-491f-b31d-f3883fc40fcb').
narrative_ontology:cs_reading_relation('56bdf04b-249c-491f-b31d-f3883fc40fcb', jurisprudential_method_kernel__hanafi_reading, forecloses).
narrative_ontology:cs_reading_relation('56bdf04b-249c-491f-b31d-f3883fc40fcb', jurisprudential_method_kernel__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('56bdf04b-249c-491f-b31d-f3883fc40fcb', jurisprudential_method_kernel__shafii_reading, forecloses).
narrative_ontology:cs_axiom('56bdf04b-249c-491f-b31d-f3883fc40fcb', foundational, textual_literalism_supremacy).
narrative_ontology:cs_axiom_status(textual_literalism_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('56bdf04b-249c-491f-b31d-f3883fc40fcb', textual_literalism_supremacy, deontological).
narrative_ontology:cs_axiom('56bdf04b-249c-491f-b31d-f3883fc40fcb', foundational, rejection_of_juristic_innovation).
narrative_ontology:cs_axiom_status(rejection_of_juristic_innovation, holdable).
narrative_ontology:cs_axiom_grounding('56bdf04b-249c-491f-b31d-f3883fc40fcb', rejection_of_juristic_innovation, deontological).
narrative_ontology:cs_reference_frame('56bdf04b-249c-491f-b31d-f3883fc40fcb', prophetic_and_companion_practice).
narrative_ontology:cs_drift_state('56bdf04b-249c-491f-b31d-f3883fc40fcb', contemporary_islamic_legal_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('56bdf04b-249c-491f-b31d-f3883fc40fcb', '').
narrative_ontology:cs_kernel_id(jurisprudential_method_kernel__hanbali_reading, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanbali_reading, hanbali_ulama).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanbali_reading, textualist_scholars).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanbali_reading, rationalist_jurists).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanbali_reading, customary_practice_adherents).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanbali_reading, other_madhhabs).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanbali_reading, lay_muslims).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The scholarly body that defines, interprets, and enforces the Hanbali methodology. They benefit from the clarity and perceived purity of the method, and their authority is grounded in its strict adherence to textual sources. They actively reject alternative methodologies as illegitimate innovations.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, hanbali_ulama, agenda_setter,
    institutional, generational, identity_locked, global).

% Scholars whose interpretive approach aligns with strict textual literalism. They benefit from the Hanbali method's emphasis on Qur'an, Hadith, and Companion opinions, which validates their preferred mode of scholarship and grants them authority within the framework.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, textualist_scholars, beneficiary,
    powerful, biographical, constrained, global).

% Jurists who advocate for analogical reasoning (qiyas), juristic preference (istihsan), or other forms of rationalist interpretation. They bear the cost of their methods being labeled as 'bid'ah' (innovation/heresy), leading to their marginalization or delegitimization within the Hanbali framework.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, rationalist_jurists, payer,
    powerful, biographical, constrained, global).

% Individuals or communities whose legal practices are rooted in local customs ('urf) that may not have direct textual support. They face pressure to conform to strictly textual derivations, and their traditions are delegitimized if they cannot be directly traced to Qur'an, Hadith, or Companion opinions.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, customary_practice_adherents, payer,
    moderate, biographical, constrained, local).

% The other major schools of Islamic jurisprudence (Hanafi, Maliki, Shafii) whose methodologies are implicitly or explicitly rejected by the Hanbali reading as corrupting the kernel. While they exist as distinct traditions, their methods are deemed illegitimate from the Hanbali perspective, effectively excluding them from the Hanbali framework's internal discourse on valid legal derivation.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, other_madhhabs, excluded,
    institutional, generational, identity_locked, global).

% The general Muslim populace who benefit from the perceived certainty, purity, and consistency of law derived through a strict textualist method. However, they may also experience inflexibility in applying the law to novel situations not directly addressed by the foundational texts.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, lay_muslims, beneficiary,
    powerless, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jurisprudential_method_kernel__hanbali_reading, hanbali_ulama).
narrative_ontology:fixing_cost_class(jurisprudential_method_kernel__hanbali_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, unambiguous, and highly textualist method for deriving Islamic law, aiming to ensure consistency, prevent arbitrary interpretations, and maintain the perceived purity of divine revelation.
% TRANSFER_FUNCTION: Transfers interpretive authority from individual juristic reasoning and local custom to strict textual adherence and established precedent, from rationalist jurists and customary practitioners to textualist scholars and the Hanbali ulama.
% ABSENT_VOICES: Jurists from other schools (Hanafi, Maliki, Shafii) who advocate for analogical reasoning, juristic preference, or local custom are structurally excluded from the Hanbali framework's internal legitimacy. They would argue for the necessity and validity of these methods in extending divine intent to novel cases.
% DISAPPEARANCE_RATIONALE: If the Hanbali jurisprudential method and its enforcement vanished overnight, the Hanbali school's legal system would collapse. This would lead to a profound re-evaluation of legal sources and methodologies within that tradition, likely resulting in a more diverse, less text-bound approach to law, and a significant shift in scholarly authority.
% FOUNDING_PROBLEM: Preventing the corruption of divine law through speculative reasoning and ensuring strict adherence to the earliest, purest forms of Islamic practice and revelation, particularly against perceived innovations (bid'ah).
% FOUNDING_PROBLEM_CORROBORATION: Hanbali scholars and their adherents attest to the ongoing need for strict textual adherence to prevent innovation and maintain the purity of Islamic law. Other schools and secular legal historians would contest the premise that analogical reasoning inherently corrupts the law, but acknowledge the historical concern for purity and the avoidance of arbitrary interpretation.
narrative_ontology:disappearance_verdict(jurisprudential_method_kernel__hanbali_reading, world_rearranges).
narrative_ontology:founding_problem_status(jurisprudential_method_kernel__hanbali_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jurisprudential_method_kernel__hanbali_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(jurisprudential_method_kernel__hanbali_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jurisprudential_method_kernel__hanbali_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.85) because the Hanbali method extracts interpretive freedom and methodological diversity from jurists and communities, forcing adherence to a narrow set of textual sources and interpretive tools. Suppression is very high (0.90) due to the active labeling of alternative methods as 'bid'ah', which carries significant theological and social weight, effectively suppressing intellectual and methodological dissent. Theater ratio is low (0.10) because the adherence to this method is generally strict and functional within its framework, with little performative maintenance of an atrophied function.
 *
 * PERSPECTIVAL GAP:
 *   From the Hanbali perspective, this method represents pure adherence to divine revelation and the avoidance of corrupting innovations, thus appearing as a 'mountain' of truth. From the perspective of rationalist jurists or other madhhabs, it is a 'snare' that stifles intellectual inquiry, imposes undue rigidity, and extracts methodological diversity through theological condemnation. The engine's classification as a snare reflects this structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   The Hanbali ulama and textualist scholars are the primary beneficiaries, as their authority and preferred methods are validated and reinforced. Rationalist jurists and adherents of customary practice are the primary targets, as their methodologies are delegitimized and suppressed. Other madhhabs are excluded, their very existence as alternative interpretive frameworks being implicitly challenged by the Hanbali claim to purity. Lay Muslims benefit from perceived certainty but may experience inflexibility.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_of_bidah_label,
    'Is analogical reasoning (qiyas) and juristic preference (istihsan) truly ''bid''ah'' (innovation/heresy) that corrupts the kernel, or are they necessary and legitimate tools for extending divine intent to novel cases?',
    'Conceptual analysis of early Islamic legal theory, historical impact studies of different madhhabs'' flexibility, and theological arguments regarding the scope of human reason in divine law.',
    'If qiyas/istihsan are deemed legitimate, the Hanbali reading''s suppression of these methods would be reclassified as pure extraction, increasing its effective extractiveness and solidifying its ''snare'' classification. If they are truly bid''ah, the suppression is a necessary coordination cost for purity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_bidah_label, conceptual, 'Whether the theological label of ''bid''ah'' for alternative legal methods is structurally justified or a tool of suppression.').

omega_variable(
    impact_on_legal_flexibility,
    'Does the Hanbali reading''s strict textual literalism lead to an inflexible legal system unable to adapt to contemporary challenges, or does it ensure purity and stability against societal drift?',
    'Empirical study of the Hanbali school''s historical and contemporary application in diverse contexts, comparing its adaptability to other madhhabs, and analyzing its capacity to address novel legal issues without resorting to ''bid''ah''.',
    'If shown to be inflexible, the constraint''s ''accessibility_collapse'' would be higher, and its ''resistance'' from those seeking practical solutions would increase. If it demonstrates robust adaptability within its framework, its coordination function would be more evident.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(impact_on_legal_flexibility, empirical, 'The practical consequences of strict textual literalism on legal adaptability and societal relevance.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of alternative jurisprudential methods primarily structural (doctrinal exclusion and theological condemnation) or internalized (scholars self-censor due to fear of being labeled ''bid''ah'')?',
    'Sociological studies of scholarly communities within and outside the Hanbali tradition, examining career paths, publication trends, and self-reported methodological choices. If suppression persists even when external doctrinal enforcement is weak, it suggests internalization.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as the target carries the suppression with them. This would amplify the ''snare'' characteristics, making exit from the methodological strictures more difficult even in less overtly coercive environments.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for jurisprudential methods.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jurisprudential_method_kernel__hanbali_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(juri_tr_t0, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(juri_tr_t20, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(juri_tr_t40, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(juri_tr_t60, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 60, 0.1).
narrative_ontology:measurement(juri_tr_t80, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 80, 0.1).
narrative_ontology:measurement(juri_tr_t100, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(juri_be_t0, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(juri_be_t20, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 20, 0.78).
narrative_ontology:measurement(juri_be_t40, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 40, 0.81).
narrative_ontology:measurement(juri_be_t60, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 60, 0.83).
narrative_ontology:measurement(juri_be_t80, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 80, 0.84).
narrative_ontology:measurement(juri_be_t100, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 100, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(juri_su_t0, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(juri_su_t20, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 20, 0.83).
narrative_ontology:measurement(juri_su_t40, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 40, 0.86).
narrative_ontology:measurement(juri_su_t60, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 60, 0.88).
narrative_ontology:measurement(juri_su_t80, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 80, 0.89).
narrative_ontology:measurement(juri_su_t100, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 100, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jurisprudential_method_kernel__hanbali_reading, identity_coordination).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanbali_reading, jurisprudential_method_kernel__hanafi_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanbali_reading, jurisprudential_method_kernel__maliki_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanbali_reading, jurisprudential_method_kernel__shafii_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'jurisprudential_method_kernel', which describes the foundational debate over legal sources and methods in Islamic jurisprudence. Each reading represents a distinct, structurally different constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
