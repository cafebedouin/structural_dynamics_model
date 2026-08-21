% ============================================================================
% CONSTRAINT STORY: first_amendment_speech_protection__categorical_balancing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_first_amendment_speech_protection__categorical_balancing_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: first_amendment_speech_protection__categorical_balancing_reading
 *   human_readable: First Amendment Speech Protection: Categorical Balancing Reading
 *   domain: constitutional_law/political_philosophy/speech_regulation
 *
 * SUMMARY:
 *   This constraint describes the 'categorical balancing' reading of the
 *   First Amendment, where the judiciary defines categories of speech (e.g.,
 *   obscenity, incitement, true threats) that receive less or no
 *   constitutional protection, often through a balancing test of speech value
 *   against potential harm. This reading is a specific interpretation of the
 *   First Amendment kernel, distinct from absolutist or purely harm-limited
 *   approaches. It benefits the institutional judiciary by granting it
 *   significant interpretive power and allows for the suppression of speech
 *   deemed undesirable by prevailing social norms, often at the expense of
 *   minority speakers and legal predictability.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(first_amendment_speech_protection__categorical_balancing_reading, 0.65).
domain_priors:suppression_score(first_amendment_speech_protection__categorical_balancing_reading, 0.7).
domain_priors:theater_ratio(first_amendment_speech_protection__categorical_balancing_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(first_amendment_speech_protection__categorical_balancing_reading, tangled_rope).
narrative_ontology:human_readable(first_amendment_speech_protection__categorical_balancing_reading, "First Amendment Speech Protection: Categorical Balancing Reading").
narrative_ontology:topic_domain(first_amendment_speech_protection__categorical_balancing_reading, "constitutional_law/political_philosophy/speech_regulation").

domain_priors:requires_active_enforcement(first_amendment_speech_protection__categorical_balancing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(first_amendment_speech_protection__categorical_balancing_reading, '172c14fc-8cde-4e11-a165-bb8d0631b851').
narrative_ontology:cs_kernel_codification('172c14fc-8cde-4e11-a165-bb8d0631b851', fixed_text).
narrative_ontology:cs_authority_grounding('172c14fc-8cde-4e11-a165-bb8d0631b851', lineage).
narrative_ontology:cs_interpretation_layer_present('172c14fc-8cde-4e11-a165-bb8d0631b851').
narrative_ontology:cs_reading_relation('172c14fc-8cde-4e11-a165-bb8d0631b851', first_amendment_speech_protection__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('172c14fc-8cde-4e11-a165-bb8d0631b851', first_amendment_speech_protection__harm_limited_reading, coexists_with).
narrative_ontology:cs_axiom('172c14fc-8cde-4e11-a165-bb8d0631b851', foundational, speech_is_not_absolute).
narrative_ontology:cs_axiom_status(speech_is_not_absolute, holdable).
narrative_ontology:cs_axiom_grounding('172c14fc-8cde-4e11-a165-bb8d0631b851', speech_is_not_absolute, conventional).
narrative_ontology:cs_axiom('172c14fc-8cde-4e11-a165-bb8d0631b851', foundational, judicial_balancing_is_necessary).
narrative_ontology:cs_axiom_status(judicial_balancing_is_necessary, holdable).
narrative_ontology:cs_axiom_grounding('172c14fc-8cde-4e11-a165-bb8d0631b851', judicial_balancing_is_necessary, instrumental).
narrative_ontology:cs_reference_frame('172c14fc-8cde-4e11-a165-bb8d0631b851', evolving_judicial_precedent).
narrative_ontology:cs_drift_state('172c14fc-8cde-4e11-a165-bb8d0631b851', contemporary_digital_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('172c14fc-8cde-4e11-a165-bb8d0631b851', '').
narrative_ontology:cs_kernel_id(first_amendment_speech_protection__categorical_balancing_reading, first_amendment_speech_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__categorical_balancing_reading, institutional_judiciary).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__categorical_balancing_reading, majority_public_opinion).
narrative_ontology:constraint_victim(first_amendment_speech_protection__categorical_balancing_reading, minority_speakers).
narrative_ontology:constraint_victim(first_amendment_speech_protection__categorical_balancing_reading, legal_predictability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary interpreter and enforcer of the First Amendment, defining categories of protected and unprotected speech through case-by-case balancing. Benefits from maintaining interpretive control and the flexibility to adapt speech law to evolving social norms and perceived harms.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, institutional_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Benefits from the judiciary's ability to restrict speech deemed harmful or offensive by prevailing social standards (e.g., obscenity, incitement). This reading allows for a perceived 'common sense' approach to speech regulation, aligning with majoritarian preferences for social order over absolute liberty.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, majority_public_opinion, beneficiary,
    organized, biographical, mobile, national).

% Often find their speech falling into categories deemed 'unprotected' or subject to balancing tests that disfavor their views. They bear the cost of censorship, prosecution, or social marginalization when their expression is deemed to lack 'value' or cause 'harm' by judicial standards. Their exit options are severely limited, often to silence or non-compliance with legal consequences.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, minority_speakers, payer,
    powerless, immediate, trapped, local).

% Suffers from the inherent uncertainty of a case-by-case balancing approach. The boundaries of protected speech are fluid, making it difficult for individuals and legal practitioners to anticipate judicial outcomes. This lack of clear rules imposes costs in terms of compliance, litigation, and chilling effects on potentially protected speech.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, legal_predictability, payer,
    analytical, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(first_amendment_speech_protection__categorical_balancing_reading, legal_predictability).

% Advocate for a more literal interpretation of the First Amendment, arguing that 'no law' means virtually no government restriction on speech. They are excluded from the dominant interpretive framework that prioritizes balancing and categorization, finding their arguments often dismissed as impractical or dangerous.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, absolutist_advocates, excluded,
    moderate, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for adjudicating conflicts between speech rights and other societal interests (e.g., public safety, privacy, reputation), aiming to balance competing values and maintain social order.
% TRANSFER_FUNCTION: Transfers interpretive authority and the power to define speech boundaries from a more absolute textual reading to the institutional judiciary. It transfers the burden of uncertainty and potential censorship to speakers whose expression falls into judicially disfavored categories.
% ABSENT_VOICES: Absolutist advocates and those who prioritize speech as a fundamental, nearly unqualified right are largely excluded from the dominant discourse, which frames speech regulation as a necessary and legitimate function of the state. Their arguments for broader protection are often marginalized in judicial and public debate.
% DISAPPEARANCE_RATIONALE: If this categorical balancing reading vanished, the legal landscape for speech would be thrown into chaos. Either an absolutist reading would take hold (leading to a dramatic increase in previously restricted speech and associated harms), or a purely harm-limited reading would emerge (leading to potentially broader restrictions based on perceived injury). The judiciary's role in speech regulation would be fundamentally altered, and the current equilibrium of speech and social order would collapse.
% FOUNDING_PROBLEM: The problem of how to reconcile the broad language of the First Amendment with the need to regulate certain types of speech (e.g., incitement to violence, defamation) that cause demonstrable harm or undermine social order.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, civil liberties organizations (though often critical of its application), and the judiciary itself attest that the problem of balancing speech against harm remains a live and complex issue, requiring ongoing judicial interpretation. This is corroborated by the continuous stream of new speech cases and evolving social contexts.
narrative_ontology:disappearance_verdict(first_amendment_speech_protection__categorical_balancing_reading, world_rearranges).
narrative_ontology:founding_problem_status(first_amendment_speech_protection__categorical_balancing_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(first_amendment_speech_protection__categorical_balancing_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(first_amendment_speech_protection__categorical_balancing_reading, 'none', 1).
narrative_ontology:epsilon_provenance(first_amendment_speech_protection__categorical_balancing_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(first_amendment_speech_protection__categorical_balancing_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(first_amendment_speech_protection__categorical_balancing_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(first_amendment_speech_protection__categorical_balancing_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is substantial because the judiciary's interpretive control allows for the suppression of speech that might otherwise be protected, effectively extracting the right to speak from certain groups. Suppression (0.70) is high due to the active enforcement of these categories through legal precedent and judicial rulings, which can lead to fines, imprisonment, or social marginalization for those whose speech falls outside protected bounds. The theater ratio (0.40) reflects that while there is genuine judicial deliberation, a significant portion of the activity involves maintaining the interpretive framework itself and defending its legitimacy against challenges, rather than purely applying clear, pre-existing rules. Accessibility collapse (0.45) is moderate, as alternatives (e.g., different legal arguments, political advocacy) exist but are constrained by the dominant judicial framework. Resistance (0.55) is also moderate, as there is ongoing legal and academic debate, but not widespread civil disobedience against the core framework.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the institutional judiciary, this reading is a necessary 'rope' for managing complex societal conflicts and adapting constitutional principles to new realities. From the perspective of minority speakers, it operates as a 'snare,' selectively extracting their speech rights based on subjective judicial balancing. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The institutional judiciary is a clear beneficiary and agenda-setter, as this reading grants it significant power and discretion. Majority public opinion also benefits by seeing speech it dislikes regulated. Minority speakers and legal predictability are victims, bearing the costs of censorship and uncertainty. Absolutist advocates are excluded, as their interpretive framework is not the one being applied.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification helps prevent mislabeling the categorical balancing approach as a pure 'rope' (simple coordination) or a 'mountain' (natural law). By identifying it as a 'tangled rope,' it acknowledges the genuine coordination function (balancing competing interests) while exposing the asymmetric extraction (judicial power, suppression of minority speech) and the active enforcement required to maintain it. This prevents the 'naturalness' claim from obscuring the underlying power dynamics and costs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_discretion_vs_rule_of_law,
    'Does the categorical balancing approach grant the judiciary excessive discretion, undermining the rule of law by making speech protection unpredictable?',
    'Empirical analysis of judicial outcomes over time, comparing consistency across different courts and cases, and assessing the predictability of rulings for novel speech acts. Legal scholarship on the coherence and stability of speech categories.',
    'If discretion is found to be excessive, it would strengthen the ''snare'' aspect of the constraint for speakers and legal predictability, potentially leading to calls for more rigid, rule-based approaches. If consistency is high, it would support the ''rope'' aspect, suggesting effective coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_discretion_vs_rule_of_law, empirical, 'Assesses the degree to which judicial discretion in speech categorization impacts legal predictability.').

omega_variable(
    speech_value_vs_harm_objectivity,
    'Are the judicial assessments of ''speech value'' and ''harm'' objective and consistent, or are they influenced by prevailing social norms and political pressures?',
    'Content analysis of judicial opinions, comparing reasoning across different eras and social contexts. Sociological studies of judicial decision-making and public opinion trends regarding controversial speech.',
    'If assessments are found to be highly subjective and responsive to transient norms, it would increase the perceived extractiveness and suppression for minority speakers, as their speech is judged by shifting standards. If objective criteria are consistently applied, it would bolster the legitimacy of the balancing act.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(speech_value_vs_harm_objectivity, empirical, 'Examines the objectivity of judicial balancing criteria for speech.').

omega_variable(
    kernel_reading_divergence,
    'Is this categorical balancing reading a legitimate interpretation of the First Amendment, or does it fundamentally distort the original intent or plain meaning of the text?',
    'Historical-textual analysis of the First Amendment''s drafting and ratification, comparison with originalist legal theories, and philosophical debate on constitutional interpretation. This is a conceptual and preference-based question, not purely empirical.',
    'If deemed a distortion, it would delegitimize the constraint for those who adhere to other readings (e.g., absolutist), potentially increasing resistance and calls for reform. If deemed legitimate, it reinforces the current framework''s authority.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'Addresses the fundamental interpretive dispute between this reading and others of the First Amendment kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(first_amendment_speech_protection__categorical_balancing_reading, 1940, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(firs_tr_t1940, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 1940, 0.3).
narrative_ontology:measurement(firs_tr_t1960, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 1960, 0.35).
narrative_ontology:measurement(firs_tr_t1980, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 1980, 0.38).
narrative_ontology:measurement(firs_tr_t2000, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 2000, 0.39).
narrative_ontology:measurement(firs_tr_t2024, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(firs_be_t1940, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 1940, 0.5).
narrative_ontology:measurement(firs_be_t1960, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 1960, 0.55).
narrative_ontology:measurement(firs_be_t1980, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 1980, 0.6).
narrative_ontology:measurement(firs_be_t2000, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 2000, 0.63).
narrative_ontology:measurement(firs_be_t2024, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(firs_su_t1940, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 1940, 0.55).
narrative_ontology:measurement(firs_su_t1960, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 1960, 0.6).
narrative_ontology:measurement(firs_su_t1980, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 1980, 0.65).
narrative_ontology:measurement(firs_su_t2000, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement(firs_su_t2024, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(first_amendment_speech_protection__categorical_balancing_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(first_amendment_speech_protection__categorical_balancing_reading, hate_speech_regulation).
narrative_ontology:affects_constraint(first_amendment_speech_protection__categorical_balancing_reading, obscenity_law).
narrative_ontology:affects_constraint(first_amendment_speech_protection__categorical_balancing_reading, incitement_doctrine).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'first_amendment_speech_protection' kernel. Its siblings are 'absolutist_reading' and 'harm_limited_reading', each representing a distinct structural constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
