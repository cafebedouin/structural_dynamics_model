% ============================================================================
% CONSTRAINT STORY: usul_al_fiqh_method__shafii_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_usul_al_fiqh_method__shafii_reading, []).

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
 *   constraint_id: usul_al_fiqh_method__shafii_reading
 *   human_readable: Shafi'i School's Usul al-Fiqh Methodology
 *   domain: islamic_jurisprudence/legal_theory/comparative_law
 *
 * SUMMARY:
 *   This constraint describes the Shafi'i school's specific methodology for
 *   deriving Islamic law, emphasizing the prerequisite of Hadith
 *   authentication, the subordination of analogical reasoning (qiyas) to
 *   authenticated Hadith, and the restriction of scholarly consensus (ijma)
 *   to the Companions of the Prophet. This systematized usul al-fiqh
 *   (principles of jurisprudence) acts as a meta-discipline governing the
 *   hierarchy and application of legal sources. This story is one reading of
 *   the broader 'usul_al_fiqh_method' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method__shafii_reading, 0.65).
domain_priors:suppression_score(usul_al_fiqh_method__shafii_reading, 0.75).
domain_priors:theater_ratio(usul_al_fiqh_method__shafii_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(usul_al_fiqh_method__shafii_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method__shafii_reading, tangled_rope).
narrative_ontology:human_readable(usul_al_fiqh_method__shafii_reading, "Shafi'i School's Usul al-Fiqh Methodology").
narrative_ontology:topic_domain(usul_al_fiqh_method__shafii_reading, "islamic_jurisprudence/legal_theory/comparative_law").

domain_priors:requires_active_enforcement(usul_al_fiqh_method__shafii_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method__shafii_reading, 'adb40b64-dbbd-4577-b96a-eca108c02a40').
narrative_ontology:cs_kernel_codification('adb40b64-dbbd-4577-b96a-eca108c02a40', formalized).
narrative_ontology:cs_authority_grounding('adb40b64-dbbd-4577-b96a-eca108c02a40', lineage).
narrative_ontology:cs_interpretation_layer_present('adb40b64-dbbd-4577-b96a-eca108c02a40').
narrative_ontology:cs_reading_relation('adb40b64-dbbd-4577-b96a-eca108c02a40', usul_al_fiqh_method__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('adb40b64-dbbd-4577-b96a-eca108c02a40', usul_al_fiqh_method__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('adb40b64-dbbd-4577-b96a-eca108c02a40', usul_al_fiqh_method__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('adb40b64-dbbd-4577-b96a-eca108c02a40', foundational, hadith_authenticity_precedes_derivation).
narrative_ontology:cs_axiom_status(hadith_authenticity_precedes_derivation, holdable).
narrative_ontology:cs_axiom_grounding('adb40b64-dbbd-4577-b96a-eca108c02a40', hadith_authenticity_precedes_derivation, empirically_contingent).
narrative_ontology:cs_axiom('adb40b64-dbbd-4577-b96a-eca108c02a40', foundational, ijma_restricted_to_companions).
narrative_ontology:cs_axiom_status(ijma_restricted_to_companions, holdable).
narrative_ontology:cs_axiom_grounding('adb40b64-dbbd-4577-b96a-eca108c02a40', ijma_restricted_to_companions, conventional).
narrative_ontology:cs_reference_frame('adb40b64-dbbd-4577-b96a-eca108c02a40', shafii_methodological_purity).
narrative_ontology:cs_drift_state('adb40b64-dbbd-4577-b96a-eca108c02a40', contemporary_islamic_jurisprudence, gap(stable, minor, true)).
narrative_ontology:cs_created_at('adb40b64-dbbd-4577-b96a-eca108c02a40', '').
narrative_ontology:cs_kernel_id(usul_al_fiqh_method__shafii_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__shafii_reading, hadith_transmission_specialists).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__shafii_reading, shafii_jurists).
narrative_ontology:constraint_victim(usul_al_fiqh_method__shafii_reading, rationalist_jurists).
narrative_ontology:constraint_victim(usul_al_fiqh_method__shafii_reading, jurists_of_other_schools_seeking_flexibility).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__shafii_reading, muslim_laity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain significant gatekeeping authority as their expertise in authenticating Hadith becomes a prerequisite for legal derivation. Their scholarly work is elevated to a foundational status within the Shafi'i framework.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, hadith_transmission_specialists, agenda_setter,
    institutional, generational, arbitrage, global).

% Benefit from a clear, systematized methodology that provides a robust framework for legal reasoning and a strong claim to authenticity. Their rulings are perceived as more authoritative due to adherence to this strict hierarchy of sources.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, shafii_jurists, beneficiary,
    powerful, biographical, constrained, global).

% Find their methods of independent reasoning (ra'y) and expansive analogical deduction (qiyas) subordinated to textual authentication. Their authority is diminished if they cannot ground their arguments in authenticated Hadith, leading to intellectual marginalization within this framework.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, rationalist_jurists, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method__shafii_reading, rationalist_jurists, excluded).

% Experience pressure to conform to the Shafi'i hierarchy of sources, particularly regarding the strict authentication of Hadith and the restriction of ijma. Their preferred methodologies (e.g., Maliki 'amal, Hanafi istihsan) are implicitly de-emphasized or challenged by the Shafi'i framework's claims to methodological purity.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, jurists_of_other_schools_seeking_flexibility, payer,
    powerful, biographical, constrained, global).

% Benefit from the perceived consistency, authenticity, and reliability of legal rulings derived from a systematized and textually-grounded methodology. This provides a sense of certainty and adherence to divine will in their religious practice.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, muslim_laity, beneficiary,
    powerless, biographical, constrained, global).

% Analyze the structural implications of the Shafi'i methodology within Islamic legal history and comparative legal theory, noting its influence on subsequent legal thought and its distinct approach to source hierarchy.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__shafii_reading, comparative_law_scholars, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, consistent, and textually-grounded methodology for deriving Islamic law, ensuring authenticity, minimizing arbitrary rulings, and establishing a common framework for juristic discourse.
% TRANSFER_FUNCTION: Transfers interpretive authority and methodological primacy from jurists relying on independent reasoning (ra'y, expansive qiyas) to those specializing in Hadith authentication and strict textual exegesis, thereby elevating the status of Hadith scholars.
% ABSENT_VOICES: Early rationalist theologians (Mu'tazila) whose methods were explicitly rejected, and jurists from other schools (Hanafi, Maliki, Hanbali) who prioritize different sources or methodologies (e.g., Medinan practice, juristic preference, unrestricted public interest) and would argue for greater methodological flexibility.
% DISAPPEARANCE_RATIONALE: If this systematized methodology vanished overnight, the derivation of Islamic law would become highly fragmented, leading to widespread disagreement on legal rulings, a crisis of legitimacy for judicial bodies, and a fundamental reordering of religious authority, as the established framework for authenticity and consistency would be lost.
% FOUNDING_PROBLEM: The proliferation of diverse and sometimes contradictory legal opinions in early Islam, leading to a need for a standardized, authoritative method to ensure consistency, authenticity, and a clear hierarchy of sources in legal derivation.
% FOUNDING_PROBLEM_CORROBORATION: Mainstream Islamic legal institutions and scholars across various schools acknowledge the historical problem of fragmentation and the ongoing need for authoritative methodology, even if they disagree on the specific Shafi'i solution or its contemporary application. Historical texts and independent scholarly analyses corroborate the problem's existence.
narrative_ontology:disappearance_verdict(usul_al_fiqh_method__shafii_reading, world_rearranges).
narrative_ontology:founding_problem_status(usul_al_fiqh_method__shafii_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(usul_al_fiqh_method__shafii_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(usul_al_fiqh_method__shafii_reading, 'none', 1).
narrative_ontology:epsilon_provenance(usul_al_fiqh_method__shafii_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(usul_al_fiqh_method__shafii_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(usul_al_fiqh_method__shafii_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(usul_al_fiqh_method__shafii_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it provides a genuine coordination function (a clear, consistent, and authentic methodology for legal derivation) but also involves significant asymmetric extraction. The high extractiveness (0.65) and suppression (0.75) reflect the gatekeeping power granted to Hadith transmission specialists and the intellectual subordination of alternative rationalist methodologies. The low theater ratio (0.1) indicates that the system is highly functional and genuinely applied, not merely performative. Accessibility collapse is high (0.7) for those operating within the Shafi'i framework, as alternative methods are significantly de-emphasized. Resistance (0.5) is moderate, reflecting ongoing intellectual debates with other schools of thought.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Shafi'i jurists and Hadith specialists, this methodology is a necessary Rope, ensuring authenticity and order in legal derivation. From the perspective of rationalist jurists or those from other schools, it functions as a Snare or Tangled Rope, imposing a restrictive hierarchy that limits intellectual freedom and privileges certain forms of authority over others. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Hadith transmission specialists and Shafi'i jurists are the primary beneficiaries, gaining authority and a clear framework for their work. Rationalist jurists and those from other schools seeking methodological flexibility are the victims, as their preferred approaches are subordinated or excluded. The Muslim laity are diffuse beneficiaries, gaining perceived authenticity and consistency in religious rulings.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents arbitrary legal rulings and ensures a strong textual basis, fulfilling its original mandate of bringing order to legal derivation. However, its strictness also creates a gatekeeping mechanism that can be seen as extractive, concentrating interpretive power. The classification as Tangled Rope acknowledges both the coordination function and the asymmetric extraction, preventing it from being mislabeled as pure coordination (Rope) or pure extraction (Snare) by ignoring one aspect.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    authenticity_vs_rationality_primacy,
    'Is the Shafi''i emphasis on Hadith authenticity as a prerequisite for legal derivation a necessary safeguard against arbitrary rulings, or an arbitrary privileging of textual transmission over rational inquiry?',
    'Comparative analysis of legal outcomes and societal impact in jurisdictions primarily following Shafi''i methodology versus those following more rationalist or flexible schools, assessing consistency, adaptability, and public welfare.',
    'If deemed an arbitrary privileging, the extractiveness and suppression metrics would be re-evaluated upwards, potentially shifting the classification closer to a Snare. If deemed a necessary safeguard, the coordination function would be emphasized, reinforcing the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authenticity_vs_rationality_primacy, conceptual, 'Ambiguity regarding the fundamental warrant for legal authority: textual authenticity versus rational coherence.').

omega_variable(
    ijma_restriction_historical_vs_methodological,
    'Is the restriction of ijma (consensus) to the Companions of the Prophet a historically necessary limitation based on unique historical circumstances, or a methodological choice that limits the scope of contemporary scholarly consensus?',
    'Historical-critical scholarship examining the socio-political context of early Islamic jurisprudence and its evolution, alongside contemporary juristic debates on the possibility and validity of later consensus.',
    'If primarily a historical necessity, the constraint''s ''naturalness'' (emerges_naturally) would be higher, reducing perceived extraction. If a methodological choice, it highlights the constructed nature of the constraint and its potential for ongoing suppression of broader scholarly participation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ijma_restriction_historical_vs_methodological, empirical, 'Whether the ijma restriction is a historical artifact or an active methodological choice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method__shafii_reading, 800, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usul_tr_t800, usul_al_fiqh_method__shafii_reading, theater_ratio, 800, 0.1).
narrative_ontology:measurement(usul_tr_t1000, usul_al_fiqh_method__shafii_reading, theater_ratio, 1000, 0.1).
narrative_ontology:measurement(usul_tr_t1200, usul_al_fiqh_method__shafii_reading, theater_ratio, 1200, 0.1).
narrative_ontology:measurement(usul_tr_t1400, usul_al_fiqh_method__shafii_reading, theater_ratio, 1400, 0.1).
narrative_ontology:measurement(usul_tr_t1600, usul_al_fiqh_method__shafii_reading, theater_ratio, 1600, 0.1).
narrative_ontology:measurement(usul_tr_t1800, usul_al_fiqh_method__shafii_reading, theater_ratio, 1800, 0.1).
narrative_ontology:measurement(usul_tr_t2024, usul_al_fiqh_method__shafii_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(usul_be_t800, usul_al_fiqh_method__shafii_reading, base_extractiveness, 800, 0.55).
narrative_ontology:measurement(usul_be_t1000, usul_al_fiqh_method__shafii_reading, base_extractiveness, 1000, 0.58).
narrative_ontology:measurement(usul_be_t1200, usul_al_fiqh_method__shafii_reading, base_extractiveness, 1200, 0.61).
narrative_ontology:measurement(usul_be_t1400, usul_al_fiqh_method__shafii_reading, base_extractiveness, 1400, 0.63).
narrative_ontology:measurement(usul_be_t1600, usul_al_fiqh_method__shafii_reading, base_extractiveness, 1600, 0.64).
narrative_ontology:measurement(usul_be_t1800, usul_al_fiqh_method__shafii_reading, base_extractiveness, 1800, 0.65).
narrative_ontology:measurement(usul_be_t2024, usul_al_fiqh_method__shafii_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(usul_su_t800, usul_al_fiqh_method__shafii_reading, suppression_requirement, 800, 0.65).
narrative_ontology:measurement(usul_su_t1000, usul_al_fiqh_method__shafii_reading, suppression_requirement, 1000, 0.68).
narrative_ontology:measurement(usul_su_t1200, usul_al_fiqh_method__shafii_reading, suppression_requirement, 1200, 0.71).
narrative_ontology:measurement(usul_su_t1400, usul_al_fiqh_method__shafii_reading, suppression_requirement, 1400, 0.73).
narrative_ontology:measurement(usul_su_t1600, usul_al_fiqh_method__shafii_reading, suppression_requirement, 1600, 0.74).
narrative_ontology:measurement(usul_su_t1800, usul_al_fiqh_method__shafii_reading, suppression_requirement, 1800, 0.75).
narrative_ontology:measurement(usul_su_t2024, usul_al_fiqh_method__shafii_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method__shafii_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(usul_al_fiqh_method__shafii_reading, usul_al_fiqh_method__hanafi_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__shafii_reading, usul_al_fiqh_method__maliki_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__shafii_reading, usul_al_fiqh_method__hanbali_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four distinct readings of the 'usul_al_fiqh_method' kernel, each representing a major school of Islamic jurisprudence. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
