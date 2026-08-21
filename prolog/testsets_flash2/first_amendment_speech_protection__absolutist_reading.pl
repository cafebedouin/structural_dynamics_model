% ============================================================================
% CONSTRAINT STORY: first_amendment_speech_protection__absolutist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_first_amendment_speech_protection__absolutist_reading, []).

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
 *   constraint_id: first_amendment_speech_protection__absolutist_reading
 *   human_readable: First Amendment Speech Protection (Absolutist Reading)
 *   domain: constitutional_law/political_philosophy/speech_regulation
 *
 * SUMMARY:
 *   This constraint represents an absolutist reading of the First Amendment's
 *   speech clause, where 'no law' means virtually no government restriction
 *   on speech, except for a few historically recognized, narrow categories
 *   (e.g., incitement, true threats). This reading prioritizes speaker
 *   liberty above all else, externalizing the costs of harmful speech onto
 *   targeted individuals and groups. It is a reading of the
 *   'first_amendment_speech_protection' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(first_amendment_speech_protection__absolutist_reading, 0.65).
domain_priors:suppression_score(first_amendment_speech_protection__absolutist_reading, 0.2).
domain_priors:theater_ratio(first_amendment_speech_protection__absolutist_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(first_amendment_speech_protection__absolutist_reading, tangled_rope).
narrative_ontology:human_readable(first_amendment_speech_protection__absolutist_reading, "First Amendment Speech Protection (Absolutist Reading)").
narrative_ontology:topic_domain(first_amendment_speech_protection__absolutist_reading, "constitutional_law/political_philosophy/speech_regulation").

domain_priors:requires_active_enforcement(first_amendment_speech_protection__absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(first_amendment_speech_protection__absolutist_reading, '3ef60a94-ca7b-4c88-9c43-d174ac764ac6').
narrative_ontology:cs_kernel_codification('3ef60a94-ca7b-4c88-9c43-d174ac764ac6', fixed_text).
narrative_ontology:cs_authority_grounding('3ef60a94-ca7b-4c88-9c43-d174ac764ac6', lineage).
narrative_ontology:cs_interpretation_layer_present('3ef60a94-ca7b-4c88-9c43-d174ac764ac6').
narrative_ontology:cs_reading_relation('3ef60a94-ca7b-4c88-9c43-d174ac764ac6', first_amendment_speech_protection__harm_limited_reading, coexists_with).
narrative_ontology:cs_reading_relation('3ef60a94-ca7b-4c88-9c43-d174ac764ac6', first_amendment_speech_protection__categorical_balancing_reading, coexists_with).
narrative_ontology:cs_axiom('3ef60a94-ca7b-4c88-9c43-d174ac764ac6', foundational, no_law_means_no_law).
narrative_ontology:cs_axiom_status(no_law_means_no_law, holdable).
narrative_ontology:cs_axiom_grounding('3ef60a94-ca7b-4c88-9c43-d174ac764ac6', no_law_means_no_law, deontological).
narrative_ontology:cs_axiom('3ef60a94-ca7b-4c88-9c43-d174ac764ac6', foundational, speech_is_presumptively_protected).
narrative_ontology:cs_axiom_status(speech_is_presumptively_protected, holdable).
narrative_ontology:cs_axiom_grounding('3ef60a94-ca7b-4c88-9c43-d174ac764ac6', speech_is_presumptively_protected, deontological).
narrative_ontology:cs_reference_frame('3ef60a94-ca7b-4c88-9c43-d174ac764ac6', original_textualist_interpretation).
narrative_ontology:cs_drift_state('3ef60a94-ca7b-4c88-9c43-d174ac764ac6', contemporary_digital_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('3ef60a94-ca7b-4c88-9c43-d174ac764ac6', '').
narrative_ontology:cs_kernel_id(first_amendment_speech_protection__absolutist_reading, first_amendment_speech_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__absolutist_reading, speakers_and_majority_groups).
narrative_ontology:constraint_victim(first_amendment_speech_protection__absolutist_reading, targeted_minority_groups).
narrative_ontology:constraint_victim(first_amendment_speech_protection__absolutist_reading, victims_of_hate_speech).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(first_amendment_speech_protection__absolutist_reading, legislators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from maximal protection for their speech, including controversial or offensive content, with minimal legal repercussions. Their ability to express themselves is largely unconstrained by potential harm to others.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, speakers_and_majority_groups, beneficiary,
    powerful, generational, mobile, national).

% Bear the costs of unconstrained speech, experiencing harassment, discrimination, and incitement to violence without adequate legal recourse. Their ability to participate equally in public life is diminished by the hostile environment created by protected speech.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, targeted_minority_groups, payer,
    powerless, biographical, trapped, local).

% Directly suffer the emotional, psychological, and sometimes physical harm resulting from speech that targets them based on identity. They have limited legal avenues for redress under this reading.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, victims_of_hate_speech, payer,
    powerless, immediate, trapped, local).

% Interpret and enforce the First Amendment according to this absolutist reading, often striking down laws that restrict speech beyond narrow historical exceptions. They are responsible for defining the boundaries of protected expression.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, courts_and_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Are constrained in their ability to pass laws regulating speech, even when such laws aim to protect vulnerable groups or public order. Their legislative efforts are frequently invalidated by courts adhering to this reading.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, legislators, payer,
    institutional, biographical, constrained, national).

% Analyze and debate the implications of this reading, advocating for or against its application based on their legal and philosophical perspectives. They influence judicial and public opinion.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, legal_scholars_and_advocates, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, high bar for speech regulation, aiming to prevent government censorship and promote a robust marketplace of ideas by minimizing state interference in expression.
% TRANSFER_FUNCTION: Transfers the burden of harm from potentially offensive or dangerous speech from speakers to those targeted by such speech, in exchange for maximal expressive liberty for all.
% ABSENT_VOICES: Those who advocate for greater protection against speech-induced harm, particularly minority groups and victims of hate speech, are often marginalized in the legal discourse that prioritizes speaker's rights.
% DISAPPEARANCE_RATIONALE: If this absolutist reading vanished, the legal landscape for speech would fundamentally shift. Legislatures would likely enact more expansive speech regulations, courts would adopt more balancing tests, and the perceived scope of protected expression would narrow significantly, leading to a reorganization of public discourse and legal challenges.
% FOUNDING_PROBLEM: The historical problem of government censorship and suppression of dissent, particularly in the context of political and religious expression.
% FOUNDING_PROBLEM_CORROBORATION: Historians and civil liberties advocates corroborate the founding problem of government overreach. However, critics (including targeted minority groups and some legal scholars) argue that while government censorship remains a concern, the absolutist reading has created new problems of private harm and systemic oppression that were not adequately addressed by the original framing.
narrative_ontology:disappearance_verdict(first_amendment_speech_protection__absolutist_reading, world_rearranges).
narrative_ontology:founding_problem_status(first_amendment_speech_protection__absolutist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(first_amendment_speech_protection__absolutist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(first_amendment_speech_protection__absolutist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(first_amendment_speech_protection__absolutist_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(first_amendment_speech_protection__absolutist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(first_amendment_speech_protection__absolutist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(first_amendment_speech_protection__absolutist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is high because the reading systematically transfers the cost of speech-related harm from speakers to victims. Suppression (0.20) is low because the constraint's primary function is to prevent suppression of speech, not to enforce it, though it actively suppresses attempts to regulate speech. Resistance (0.70) is high from those who bear the costs of this reading, particularly targeted minority groups advocating for greater protection against hate speech. Accessibility collapse (0.40) is moderate; while legal avenues for regulating speech are collapsed, social and political resistance remains active.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of speakers, this is a Rope, ensuring free expression. From the perspective of targeted minorities, it operates as a Snare, enabling harm with impunity. The engine's classification as Tangled Rope reflects the dual function: it coordinates expressive liberty for some while extracting costs from others through the same structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Speakers and majority groups are beneficiaries (d near 0.0) as their expressive freedom is maximized. Targeted minority groups and victims of hate speech are payers (d near 1.0) as they bear the direct and systemic costs of this expansive protection. Courts act as agenda-setters, enforcing this reading, while legislators are constrained in their ability to pass speech-regulating laws.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    harm_definition_ambiguity,
    'How is ''harm'' defined in the context of speech, and whose experience of harm is prioritized?',
    'Empirical studies on the psychological and social impact of different types of speech, combined with a re-evaluation of legal precedent to incorporate victim perspectives.',
    'A broader definition of harm, or one that prioritizes the experience of targeted groups, would significantly reduce the perceived benefits for speakers and increase the perceived costs for victims, potentially shifting the classification towards a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_definition_ambiguity, conceptual, 'Ambiguity in defining and prioritizing speech-related harm.').

omega_variable(
    historical_exclusions_scope,
    'What is the precise scope and justification of the ''narrow historical exclusions'' to speech protection, and are they truly fixed or subject to reinterpretation?',
    'Detailed historical and legal analysis of the original intent and evolving application of these exclusions, alongside a comparative analysis with other constitutional rights that have seen evolving interpretations.',
    'A more expansive or evolving understanding of historical exclusions could allow for greater regulation of harmful speech, reducing extractiveness. A rigid, minimal interpretation reinforces the current high extractiveness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_exclusions_scope, empirical, 'Uncertainty regarding the fixedness and scope of historical speech exclusions.').

omega_variable(
    marketplace_of_ideas_efficacy,
    'Does the ''marketplace of ideas'' metaphor accurately describe how truth and beneficial outcomes emerge from unconstrained speech in contemporary digital environments, or does it facilitate the spread of misinformation and hate?',
    'Empirical research on information ecosystems, social media dynamics, and the impact of algorithmic amplification on public discourse, comparing outcomes in regulated vs. unregulated speech environments.',
    'If the marketplace of ideas is found to be ineffective or harmful in practice, the coordination function of this reading would be undermined, strengthening the argument for its extractive nature and potentially reclassifying it as a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marketplace_of_ideas_efficacy, empirical, 'Efficacy of the ''marketplace of ideas'' as a coordination mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(first_amendment_speech_protection__absolutist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(firs_be_t0, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(firs_be_t10, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(firs_be_t20, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(firs_be_t30, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(firs_be_t40, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 40, 0.64).
narrative_ontology:measurement(firs_be_t50, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 50, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(firs_su_t0, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(firs_su_t10, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 10, 0.16).
narrative_ontology:measurement(firs_su_t20, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 20, 0.17).
narrative_ontology:measurement(firs_su_t30, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 30, 0.18).
narrative_ontology:measurement(firs_su_t40, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 40, 0.19).
narrative_ontology:measurement(firs_su_t50, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 50, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(first_amendment_speech_protection__absolutist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(first_amendment_speech_protection__absolutist_reading, first_amendment_speech_protection__categorical_balancing_reading).
narrative_ontology:affects_constraint(first_amendment_speech_protection__absolutist_reading, first_amendment_speech_protection__harm_limited_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
