% ============================================================================
% CONSTRAINT STORY: speech_harm_boundary__harm_balancing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_harm_boundary__harm_balancing_reading, []).

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
 *   constraint_id: speech_harm_boundary__harm_balancing_reading
 *   human_readable: Speech Harm Boundary (Proportionality Balancing Reading)
 *   domain: constitutional_law/political_philosophy/communication_ethics
 *
 * SUMMARY:
 *   This constraint represents the 'harm balancing' reading of the speech
 *   harm boundary, where speech protection is presumptive but yields to
 *   demonstrated harm, subject to a proportionality test. This reading
 *   acknowledges that speech can cause real-world injury and seeks to balance
 *   free expression against the need for protection. It results in broader
 *   categories of unprotected speech (e.g., hate speech, harassment) compared
 *   to absolutist views, but requires a high evidentiary bar for harm and
 *   proportionality in restrictions. Speakers bear the costs of restriction
 *   when harm is demonstrated.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_harm_boundary__harm_balancing_reading, 0.45).
domain_priors:suppression_score(speech_harm_boundary__harm_balancing_reading, 0.3).
domain_priors:theater_ratio(speech_harm_boundary__harm_balancing_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_harm_boundary__harm_balancing_reading, tangled_rope).
narrative_ontology:human_readable(speech_harm_boundary__harm_balancing_reading, "Speech Harm Boundary (Proportionality Balancing Reading)").
narrative_ontology:topic_domain(speech_harm_boundary__harm_balancing_reading, "constitutional_law/political_philosophy/communication_ethics").

domain_priors:requires_active_enforcement(speech_harm_boundary__harm_balancing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_harm_boundary__harm_balancing_reading, '49e5073d-4f5a-4cb6-b4bd-4e90fd7cb9b9').
narrative_ontology:cs_kernel_codification('49e5073d-4f5a-4cb6-b4bd-4e90fd7cb9b9', formalized).
narrative_ontology:cs_authority_grounding('49e5073d-4f5a-4cb6-b4bd-4e90fd7cb9b9', lineage).
narrative_ontology:cs_interpretation_layer_present('49e5073d-4f5a-4cb6-b4bd-4e90fd7cb9b9').
narrative_ontology:cs_reading_relation('49e5073d-4f5a-4cb6-b4bd-4e90fd7cb9b9', speech_harm_boundary__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('49e5073d-4f5a-4cb6-b4bd-4e90fd7cb9b9', speech_harm_boundary__dignity_reading, coexists_with).
narrative_ontology:cs_axiom('49e5073d-4f5a-4cb6-b4bd-4e90fd7cb9b9', foundational, speech_is_action_with_consequences).
narrative_ontology:cs_axiom_status(speech_is_action_with_consequences, holdable).
narrative_ontology:cs_axiom_grounding('49e5073d-4f5a-4cb6-b4bd-4e90fd7cb9b9', speech_is_action_with_consequences, empirically_contingent).
narrative_ontology:cs_axiom('49e5073d-4f5a-4cb6-b4bd-4e90fd7cb9b9', foundational, proportionality_is_just_restriction).
narrative_ontology:cs_axiom_status(proportionality_is_just_restriction, holdable).
narrative_ontology:cs_axiom_grounding('49e5073d-4f5a-4cb6-b4bd-4e90fd7cb9b9', proportionality_is_just_restriction, conventional).
narrative_ontology:cs_reference_frame('49e5073d-4f5a-4cb6-b4bd-4e90fd7cb9b9', liberal_democratic_balancing_tradition).
narrative_ontology:cs_drift_state('49e5073d-4f5a-4cb6-b4bd-4e90fd7cb9b9', contemporary_digital_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('49e5073d-4f5a-4cb6-b4bd-4e90fd7cb9b9', '').
narrative_ontology:cs_kernel_id(speech_harm_boundary__harm_balancing_reading, speech_harm_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_harm_boundary__harm_balancing_reading, society_at_large).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__harm_balancing_reading, vulnerable_groups).
narrative_ontology:constraint_victim(speech_harm_boundary__harm_balancing_reading, speakers_of_harmful_speech).
narrative_ontology:constraint_victim(speech_harm_boundary__harm_balancing_reading, speech_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from a framework that allows for the regulation of speech that causes demonstrable harm, fostering a more civil and safe public discourse. Bears the cost of some restrictions on expression.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, society_at_large, beneficiary,
    institutional, generational, constrained, national).

% Receives protection from speech that directly incites violence, harassment, or discrimination, which disproportionately affects them. Advocates for robust harm-balancing mechanisms.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, vulnerable_groups, beneficiary,
    organized, biographical, constrained, local).

% Bears the cost of having their speech restricted when it is deemed to cause demonstrable harm, even if they believe their expression is legitimate. Their options are to self-censor, face legal consequences, or challenge the restriction.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, speakers_of_harmful_speech, payer,
    moderate, immediate, constrained, local).

% Argues for broad speech protections and views harm-balancing as a potential slippery slope to censorship. Bears the cost of legal challenges and public discourse shifts that favor restrictions.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, speech_advocates, payer,
    powerful, generational, mobile, national).

% Responsible for applying the proportionality test, balancing speech interests against potential harms. They define the categories of unprotected speech and enforce restrictions, often facing public and legal scrutiny.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, courts_and_regulators, agenda_setter,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the exercise of free speech with the protection of individuals and groups from demonstrable harm, providing a framework for adjudicating conflicts between these values.
% TRANSFER_FUNCTION: Transfers the burden of demonstrating harm from the speaker to the party seeking restriction, but then transfers the cost of restriction from the harmed party to the speaker when harm is proven and proportional.
% ABSENT_VOICES: Those who suffer diffuse, cumulative harms from speech that does not meet the high bar for direct, demonstrable harm often lack a clear voice in the balancing process, as their injury is harder to quantify or attribute to specific acts of speech.
% DISAPPEARANCE_RATIONALE: If this framework vanished, society would face a stark choice between near-absolute speech freedom (leading to increased harm and social fragmentation) or broad, arbitrary censorship (leading to suppression of dissent). Legal systems would struggle to adjudicate speech conflicts, and public discourse would become more volatile.
% FOUNDING_PROBLEM: The problem of how to reconcile the fundamental right to free expression with the equally fundamental need to protect individuals and society from the tangible harms that speech can inflict, without resorting to arbitrary censorship.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, human rights organizations, and victims' advocacy groups consistently attest to the ongoing challenge of balancing speech and harm, citing new forms of online abuse and hate speech as evidence that the problem remains live and evolving.
narrative_ontology:disappearance_verdict(speech_harm_boundary__harm_balancing_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_harm_boundary__harm_balancing_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_harm_boundary__harm_balancing_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(speech_harm_boundary__harm_balancing_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_harm_boundary__harm_balancing_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_harm_boundary__harm_balancing_reading_tests).
:- end_tests(speech_harm_boundary__harm_balancing_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) because speakers bear the cost of restrictions when their speech is deemed harmful, but the framework also provides a clear, albeit contested, path for such restrictions. Suppression is moderate (0.30) as it requires active enforcement by courts and regulators to define and apply harm thresholds, but it does not aim for total suppression of all potentially offensive speech. The framework is a 'tangled rope' because it genuinely coordinates the conflicting interests of speakers and those harmed by speech, but it also involves asymmetric extraction from speakers whose expression is restricted.
 *
 * PERSPECTIVAL GAP:
 *   Speakers whose speech is restricted will experience this constraint as extractive and suppressive, viewing it as a limitation on fundamental rights. Those protected by the restrictions will see it as a necessary coordination mechanism for a civil society. The courts, as agenda-setters, aim for a balanced application, but their decisions are inherently contested.
 *
 * DIRECTIONALITY LOGIC:
 *   Society at large and vulnerable groups are beneficiaries, as they gain protection from harmful speech. Speakers of harmful speech and speech advocates are payers, as they bear the costs of restrictions and the ongoing contestation of speech boundaries. Courts and regulators act as agenda-setters, defining and enforcing the balance.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    demonstrable_harm_definition,
    'What constitutes ''demonstrable harm'' in the context of speech, and how is it measured consistently across diverse cases?',
    'Development of clearer legal precedents and empirical methodologies for assessing speech-related harm, potentially involving interdisciplinary expert consensus.',
    'A clearer definition would reduce the perceived arbitrariness of restrictions, potentially lowering extractiveness for speakers and increasing confidence for beneficiaries. Ambiguity allows for greater discretion and potential for abuse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demonstrable_harm_definition, conceptual, 'Ambiguity in defining and measuring ''demonstrable harm'' from speech.').

omega_variable(
    proportionality_test_application,
    'Is the proportionality test consistently and fairly applied, or does it disproportionately burden certain types of speech or speakers?',
    'Systematic review of judicial decisions and regulatory actions, including disaggregated data on speech types and speaker demographics affected by restrictions.',
    'If disproportionate, the constraint''s effective extractiveness and suppression would be higher for targeted groups, potentially reclassifying it closer to a snare for those seats. Fair application would reinforce its tangled rope nature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_test_application, empirical, 'Consistency and fairness of the proportionality test in practice.').

omega_variable(
    reading_legitimacy_contest,
    'Is the ''harm_balancing_reading'' a legitimate interpretation of free speech principles, or is it a deviation from foundational commitments?',
    'Ongoing philosophical and legal debate, potentially influenced by shifts in societal values and international human rights norms.',
    'If widely accepted as legitimate, the constraint''s stability increases. If seen as a deviation, it faces greater resistance and challenges from ''absolutist'' or ''dignity'' readings, increasing its suppression requirement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_legitimacy_contest, conceptual, 'Contestation over the legitimacy of the harm-balancing approach to free speech.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_harm_boundary__harm_balancing_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(spee_be_t10, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(spee_be_t20, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(spee_be_t30, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 30, 0.43).
narrative_ontology:measurement(spee_be_t40, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 40, 0.46).
narrative_ontology:measurement(spee_be_t50, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 50, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(spee_su_t10, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 10, 0.28).
narrative_ontology:measurement(spee_su_t20, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 20, 0.3).
narrative_ontology:measurement(spee_su_t30, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 30, 0.29).
narrative_ontology:measurement(spee_su_t40, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 40, 0.31).
narrative_ontology:measurement(spee_su_t50, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 50, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_harm_boundary__harm_balancing_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_harm_boundary__harm_balancing_reading, speech_harm_boundary__absolutist_reading).
narrative_ontology:affects_constraint(speech_harm_boundary__harm_balancing_reading, speech_harm_boundary__dignity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'speech_harm_boundary' kernel. Each reading instantiates a distinct constraint with different structural properties and classifications. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
