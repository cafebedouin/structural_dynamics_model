% ============================================================================
% CONSTRAINT STORY: speech_harm_boundary__dignity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_harm_boundary__dignity_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: speech_harm_boundary__dignity_reading
 *   human_readable: Speech Protection Subordinate to Human Dignity (Dignity Reading)
 *   domain: constitutional_law/political_philosophy/communication_ethics
 *
 * SUMMARY:
 *   This constraint represents the 'dignity reading' of the speech-harm
 *   boundary kernel, where speech protection is explicitly subordinate to
 *   human dignity, and personhood-denying speech is categorically
 *   unprotected. This reading emerged strongly in the post-WWII human rights
 *   framework and is enshrined in various international and national legal
 *   systems. It stands in contrast to more absolutist or purely
 *   harm-balancing approaches to free speech.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_harm_boundary__dignity_reading, 0.85).
domain_priors:suppression_score(speech_harm_boundary__dignity_reading, 0.9).
domain_priors:theater_ratio(speech_harm_boundary__dignity_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_harm_boundary__dignity_reading, tangled_rope).
narrative_ontology:human_readable(speech_harm_boundary__dignity_reading, "Speech Protection Subordinate to Human Dignity (Dignity Reading)").
narrative_ontology:topic_domain(speech_harm_boundary__dignity_reading, "constitutional_law/political_philosophy/communication_ethics").

domain_priors:requires_active_enforcement(speech_harm_boundary__dignity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_harm_boundary__dignity_reading, '34de8a9b-fb63-44cd-9008-7fc90f946513').
narrative_ontology:cs_kernel_codification('34de8a9b-fb63-44cd-9008-7fc90f946513', formalized).
narrative_ontology:cs_authority_grounding('34de8a9b-fb63-44cd-9008-7fc90f946513', lineage).
narrative_ontology:cs_interpretation_layer_present('34de8a9b-fb63-44cd-9008-7fc90f946513').
narrative_ontology:cs_reading_relation('34de8a9b-fb63-44cd-9008-7fc90f946513', speech_harm_boundary__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('34de8a9b-fb63-44cd-9008-7fc90f946513', speech_harm_boundary__harm_balancing_reading, influences).
narrative_ontology:cs_axiom('34de8a9b-fb63-44cd-9008-7fc90f946513', foundational, human_dignity_is_foundational).
narrative_ontology:cs_axiom_status(human_dignity_is_foundational, holdable).
narrative_ontology:cs_axiom_grounding('34de8a9b-fb63-44cd-9008-7fc90f946513', human_dignity_is_foundational, deontological).
narrative_ontology:cs_axiom('34de8a9b-fb63-44cd-9008-7fc90f946513', foundational, speech_cannot_deny_personhood).
narrative_ontology:cs_axiom_status(speech_cannot_deny_personhood, holdable).
narrative_ontology:cs_axiom_grounding('34de8a9b-fb63-44cd-9008-7fc90f946513', speech_cannot_deny_personhood, deontological).
narrative_ontology:cs_reference_frame('34de8a9b-fb63-44cd-9008-7fc90f946513', post_wwii_human_rights_framework).
narrative_ontology:cs_drift_state('34de8a9b-fb63-44cd-9008-7fc90f946513', contemporary_digital_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('34de8a9b-fb63-44cd-9008-7fc90f946513', '').
narrative_ontology:cs_kernel_id(speech_harm_boundary__dignity_reading, speech_harm_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_harm_boundary__dignity_reading, vulnerable_groups).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__dignity_reading, dignity_advocates).
narrative_ontology:constraint_victim(speech_harm_boundary__dignity_reading, speakers_of_hate_speech).
narrative_ontology:constraint_victim(speech_harm_boundary__dignity_reading, absolutist_free_speech_advocates).
narrative_ontology:constraint_vindicates(speech_harm_boundary__dignity_reading, human_dignity_principle).
narrative_ontology:constraint_vindicates(speech_harm_boundary__dignity_reading, equality_principle).
narrative_ontology:constraint_vindicates(speech_harm_boundary__dignity_reading, non_discrimination_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively promote and defend the principle that human dignity is a foundational limit on speech. They lobby for legislation, support legal challenges, and shape public discourse to ensure personhood-denying speech is not protected.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, dignity_advocates, agenda_setter,
    institutional, generational, analytical, global).

% Are the primary beneficiaries of this constraint, as it aims to protect them from speech that denies their personhood, incites hatred, or promotes discrimination. They bear the direct harm when such speech is permitted.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, vulnerable_groups, beneficiary,
    powerless, biographical, trapped, local).

% Bear the direct costs of this constraint, as their speech is deemed categorically unprotected and subject to legal sanctions or platform removal. Their ability to express certain views is severely curtailed.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, speakers_of_hate_speech, payer,
    moderate, immediate, constrained, global).

% Oppose any categorical restrictions on speech, arguing that even offensive or hateful speech should be protected to ensure a robust marketplace of ideas. They bear the cost of a narrower definition of protected speech and actively challenge such constraints in courts and public forums.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, absolutist_free_speech_advocates, payer,
    organized, biographical, mobile, national).

% Are tasked with interpreting and enforcing the boundaries of protected speech, applying the dignity principle to specific cases. They adjudicate disputes and impose penalties, actively shaping the scope and impact of the constraint.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, courts_and_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Monitor and promote human rights standards globally, including the balance between freedom of expression and the protection of human dignity. They provide guidance and recommendations, influencing national legal frameworks.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, international_human_rights_bodies, observer,
    institutional, civilizational, analytical, global).

% Would argue for a more nuanced approach where speech is presumptively protected but yields to *demonstrated* harm through a proportionality balancing test, rather than categorical exclusions. Their approach is sidelined by the categorical nature of the dignity reading.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, harm_balancing_advocates, excluded,
    organized, biographical, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate society around the fundamental principle of human dignity, ensuring that public discourse does not undermine the personhood or equality of any group, thereby fostering a more inclusive and respectful social environment.
% TRANSFER_FUNCTION: Transfers the burden of tolerating personhood-denying speech from vulnerable groups (who would otherwise suffer its harms) to speakers whose expression falls into the categorically unprotected category, by restricting their speech.
% ABSENT_VOICES: Absolutist free speech advocates would argue that any categorical exclusion is an unacceptable infringement on fundamental rights. Harm-balancing advocates would propose a more flexible, context-dependent approach to speech regulation, rather than fixed categories.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, speech that denies human dignity or incites hatred would proliferate without legal consequence, leading to increased marginalization, discrimination, and potential violence against vulnerable groups. Society would be forced to re-establish new, likely more coercive, mechanisms to protect fundamental rights.
% FOUNDING_PROBLEM: The historical and ongoing problem of speech being used to dehumanize, incite violence against, or deny the personhood of vulnerable groups, leading to systemic discrimination, social fragmentation, and severe psychological and physical harm.
% FOUNDING_PROBLEM_CORROBORATION: International human rights law (e.g., ICCPR Article 20), historical records of genocide and discrimination, and ongoing reports from civil society organizations and vulnerable communities consistently corroborate the persistent nature and severe impact of personhood-denying speech.
narrative_ontology:disappearance_verdict(speech_harm_boundary__dignity_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_harm_boundary__dignity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_harm_boundary__dignity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(speech_harm_boundary__dignity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_harm_boundary__dignity_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_harm_boundary__dignity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_harm_boundary__dignity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_harm_boundary__dignity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.85) because this reading imposes significant costs on speakers whose expression is deemed to violate human dignity, effectively removing their speech from protection. Suppression is also very high (0.90) as it requires active enforcement (legal prohibitions, platform moderation) to prevent the proliferation of such speech. Theater ratio is low (0.10) because the enforcement is direct and functional, aimed at achieving the stated goal of protecting dignity, with little performative maintenance. Accessibility collapse is high (0.75) for the specific categories of speech deemed dignity-violating, as alternatives for expressing such content are severely curtailed. Resistance is moderate (0.60) from those who advocate for broader speech protections.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of vulnerable groups and dignity advocates, this constraint is a necessary safeguard for fundamental rights and social cohesion. From the perspective of those whose speech is restricted, or absolutist free speech advocates, it is an oppressive limitation on expression. The engine's per-seat classification will reflect this divergence, showing a beneficial 'rope' or 'scaffold' for beneficiaries and an extractive 'snare' or 'tangled_rope' for targets.
 *
 * DIRECTIONALITY LOGIC:
 *   Vulnerable groups and dignity advocates are clear beneficiaries, as the constraint aims to protect their fundamental rights and foster an inclusive public sphere. Speakers of hate speech and absolutist free speech advocates are the primary targets, bearing the costs of restricted expression. Courts and regulators act as agenda-setters, interpreting and enforcing the constraint. International human rights bodies serve as observers, influencing the global discourse on this balance.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is designed to prevent the mandatrophy of human dignity itself, by ensuring that the 'mandate' of free speech does not inadvertently undermine the 'function' of protecting fundamental human worth. The high extractiveness and suppression are seen as necessary to maintain the core mandate of dignity protection, rather than a sign of function atrophy. The 'live' status of the founding problem (persistent dehumanization) further indicates that the constraint's function is ongoing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_dignity_violation,
    'What specific types of speech fall under ''personhood-denying'' or ''dignity-violating'' and how consistently is this applied across jurisdictions and platforms?',
    'Comparative legal analysis of case law and platform content policies, coupled with empirical studies on the impact of different speech types on vulnerable groups.',
    'If the scope is inconsistently applied or overbroad, it could lead to arbitrary censorship, increasing the effective extraction on speakers beyond the intended target. If too narrow, it could fail to protect dignity, reducing the constraint''s effectiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_dignity_violation, empirical, 'Ambiguity in defining and applying ''dignity-violating'' speech.').

omega_variable(
    effectiveness_of_categorical_bans,
    'Do categorical bans on dignity-violating speech effectively reduce harm and promote dignity, or do they merely drive such speech underground and create chilling effects on legitimate expression?',
    'Longitudinal studies comparing social cohesion, hate crime rates, and public discourse quality in jurisdictions with and without categorical bans, alongside surveys of speakers regarding self-censorship.',
    'If bans are ineffective or counterproductive, the constraint''s high suppression and extractiveness might be misdirected, suggesting a need for alternative approaches (e.g., counter-speech, education) that could reclassify the constraint as a less extractive ''rope'' or ''scaffold''.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(effectiveness_of_categorical_bans, empirical, 'Debate over the efficacy and unintended consequences of categorical speech bans.').

omega_variable(
    dignity_vs_censorship_framing,
    'Is the constraint primarily a protection of human dignity, or is it a form of censorship that disproportionately targets dissenting or unpopular views?',
    'Analysis of the power dynamics of enforcement: if enforcement consistently targets marginalized voices while powerful actors evade scrutiny, it suggests a censorship dynamic. If it consistently protects the vulnerable, it supports the dignity framing.',
    'If framed as censorship, the constraint''s legitimacy would erode, and its classification might shift towards a ''snare'' due to perceived abuse of power. If consistently seen as dignity protection, its ''tangled_rope'' classification would be reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dignity_vs_censorship_framing, conceptual, 'Conceptual framing of the constraint as dignity protection versus censorship.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_harm_boundary__dignity_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t1948, speech_harm_boundary__dignity_reading, theater_ratio, 1948, 0.15).
narrative_ontology:measurement(spee_tr_t1968, speech_harm_boundary__dignity_reading, theater_ratio, 1968, 0.12).
narrative_ontology:measurement(spee_tr_t1988, speech_harm_boundary__dignity_reading, theater_ratio, 1988, 0.1).
narrative_ontology:measurement(spee_tr_t2008, speech_harm_boundary__dignity_reading, theater_ratio, 2008, 0.1).
narrative_ontology:measurement(spee_tr_t2024, speech_harm_boundary__dignity_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(spee_be_t1948, speech_harm_boundary__dignity_reading, base_extractiveness, 1948, 0.7).
narrative_ontology:measurement(spee_be_t1968, speech_harm_boundary__dignity_reading, base_extractiveness, 1968, 0.75).
narrative_ontology:measurement(spee_be_t1988, speech_harm_boundary__dignity_reading, base_extractiveness, 1988, 0.8).
narrative_ontology:measurement(spee_be_t2008, speech_harm_boundary__dignity_reading, base_extractiveness, 2008, 0.83).
narrative_ontology:measurement(spee_be_t2024, speech_harm_boundary__dignity_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t1948, speech_harm_boundary__dignity_reading, suppression_requirement, 1948, 0.75).
narrative_ontology:measurement(spee_su_t1968, speech_harm_boundary__dignity_reading, suppression_requirement, 1968, 0.8).
narrative_ontology:measurement(spee_su_t1988, speech_harm_boundary__dignity_reading, suppression_requirement, 1988, 0.85).
narrative_ontology:measurement(spee_su_t2008, speech_harm_boundary__dignity_reading, suppression_requirement, 2008, 0.88).
narrative_ontology:measurement(spee_su_t2024, speech_harm_boundary__dignity_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_harm_boundary__dignity_reading, identity_coordination).
narrative_ontology:affects_constraint(speech_harm_boundary__dignity_reading, speech_harm_boundary__absolutist_reading).
narrative_ontology:affects_constraint(speech_harm_boundary__dignity_reading, speech_harm_boundary__harm_balancing_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'speech_harm_boundary' kernel. This 'dignity_reading' emphasizes categorical protection of human dignity, contrasting with the 'absolutist_reading' (near-absolute protection) and the 'harm_balancing_reading' (proportionality test for harm).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
