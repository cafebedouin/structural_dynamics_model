% ============================================================================
% CONSTRAINT STORY: speech_protection_boundary__harm_limited_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_boundary__harm_limited_reading, []).

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
 *   constraint_id: speech_protection_boundary__harm_limited_reading
 *   human_readable: Speech Protection Boundary (Harm-Limited Reading)
 *   domain: constitutional_law/political_philosophy/speech_regulation
 *
 * SUMMARY:
 *   This constraint represents the 'harm-limited' reading of speech
 *   protection, where the exercise of free speech is conditional on its not
 *   causing significant harm to dignity, equality, and freedom from
 *   harassment. This reading narrows the scope of protected speech compared
 *   to absolutist or balancing approaches, explicitly including categories
 *   like hate speech and harassment within the unprotected set. The state,
 *   acting as a gatekeeper, gains significant power to define and enforce
 *   these boundaries, which introduces risks of abuse and chilling effects on
 *   legitimate dissent. This is one reading of the
 *   'speech_protection_boundary' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_boundary__harm_limited_reading, 0.65).
domain_priors:suppression_score(speech_protection_boundary__harm_limited_reading, 0.7).
domain_priors:theater_ratio(speech_protection_boundary__harm_limited_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_boundary__harm_limited_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_boundary__harm_limited_reading, "Speech Protection Boundary (Harm-Limited Reading)").
narrative_ontology:topic_domain(speech_protection_boundary__harm_limited_reading, "constitutional_law/political_philosophy/speech_regulation").

domain_priors:requires_active_enforcement(speech_protection_boundary__harm_limited_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_boundary__harm_limited_reading, '1a85c4b1-c9fb-46c7-a392-7cb04c14a2ed').
narrative_ontology:cs_kernel_codification('1a85c4b1-c9fb-46c7-a392-7cb04c14a2ed', formalized).
narrative_ontology:cs_authority_grounding('1a85c4b1-c9fb-46c7-a392-7cb04c14a2ed', lineage).
narrative_ontology:cs_interpretation_layer_present('1a85c4b1-c9fb-46c7-a392-7cb04c14a2ed').
narrative_ontology:cs_reading_relation('1a85c4b1-c9fb-46c7-a392-7cb04c14a2ed', speech_protection_boundary__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('1a85c4b1-c9fb-46c7-a392-7cb04c14a2ed', speech_protection_boundary__balancing_reading, coexists_with).
narrative_ontology:cs_axiom('1a85c4b1-c9fb-46c7-a392-7cb04c14a2ed', foundational, speech_is_conditional_on_harm_absence).
narrative_ontology:cs_axiom_status(speech_is_conditional_on_harm_absence, holdable).
narrative_ontology:cs_axiom_grounding('1a85c4b1-c9fb-46c7-a392-7cb04c14a2ed', speech_is_conditional_on_harm_absence, deontological).
narrative_ontology:cs_axiom('1a85c4b1-c9fb-46c7-a392-7cb04c14a2ed', foundational, dignity_and_equality_are_preconditions_for_free_speech).
narrative_ontology:cs_axiom_status(dignity_and_equality_are_preconditions_for_free_speech, holdable).
narrative_ontology:cs_axiom_grounding('1a85c4b1-c9fb-46c7-a392-7cb04c14a2ed', dignity_and_equality_are_preconditions_for_free_speech, deontological).
narrative_ontology:cs_reference_frame('1a85c4b1-c9fb-46c7-a392-7cb04c14a2ed', post_rights_revolution_jurisprudence).
narrative_ontology:cs_drift_state('1a85c4b1-c9fb-46c7-a392-7cb04c14a2ed', contemporary_digital_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('1a85c4b1-c9fb-46c7-a392-7cb04c14a2ed', '').
narrative_ontology:cs_kernel_id(speech_protection_boundary__harm_limited_reading, speech_protection_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_boundary__harm_limited_reading, vulnerable_groups).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__harm_limited_reading, state_regulators).
narrative_ontology:constraint_victim(speech_protection_boundary__harm_limited_reading, speakers_of_controversial_views).
narrative_ontology:constraint_victim(speech_protection_boundary__harm_limited_reading, political_dissidents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from legal frameworks that protect their dignity, equality, and freedom from harassment by limiting certain forms of speech. They advocate for robust enforcement of these limits.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, vulnerable_groups, beneficiary,
    organized, generational, constrained, national).

% Are tasked with interpreting and enforcing speech regulations based on the harm-limited standard. They gain authority and legitimacy by protecting vulnerable groups, but also face pressure regarding potential overreach.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, state_regulators, agenda_setter,
    institutional, biographical, constrained, national).

% Bear the cost of having their speech restricted or chilled due to the broad interpretation of 'harm to dignity, equality, and freedom from harassment'. They may self-censor to avoid legal repercussions or social backlash.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, speakers_of_controversial_views, payer,
    moderate, immediate, constrained, local).

% Are particularly vulnerable to speech restrictions under this reading, as their challenges to the status quo may be reinterpreted as harmful or harassing, leading to suppression of their political expression.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, political_dissidents, payer,
    powerless, biographical, identity_locked, national).

% Argue for a near-absolute protection of speech, with very narrow exceptions, believing that any harm-based limitation opens the door to censorship and tyranny. Their views are often marginalized in this framework.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, absolutist_advocates, excluded,
    organized, generational, analytical, national).

% Seek a more nuanced, case-by-case approach to speech regulation, weighing competing constitutional values. They observe the implementation of the harm-limited reading with concern for its potential overbreadth.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, balancing_advocates, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate social interaction by establishing boundaries for acceptable public discourse, fostering an environment where all individuals can participate without fear of harassment or degradation, thereby promoting social cohesion and equality.
% TRANSFER_FUNCTION: Transfers the burden of potential offense or harm from vulnerable groups to speakers, who must self-regulate their expression. It also transfers interpretive power over speech boundaries to state regulators and courts.
% ABSENT_VOICES: Those who advocate for a more expansive view of free speech, particularly absolutist proponents, are often excluded from the framing of this constraint, as their core premise directly challenges the harm-limited approach. Their arguments about the 'slippery slope' of speech regulation are often dismissed.
% DISAPPEARANCE_RATIONALE: If this harm-limited reading of speech protection vanished, there would likely be an immediate increase in speech perceived as hateful or harassing, leading to social unrest, increased marginalization of vulnerable groups, and a demand for new, potentially more restrictive, forms of regulation. The legal and social landscape of public discourse would fundamentally shift.
% FOUNDING_PROBLEM: The problem of historical and ongoing harms inflicted upon marginalized groups through speech, including hate speech, harassment, and incitement, which undermine their dignity, equality, and participation in public life.
% FOUNDING_PROBLEM_CORROBORATION: Vulnerable groups and human rights organizations consistently attest that the problem of speech-related harm is live and pervasive. State regulators and international bodies also corroborate the need for such protections, citing empirical evidence of the impact of hate speech on social cohesion and individual well-being.
narrative_ontology:disappearance_verdict(speech_protection_boundary__harm_limited_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_boundary__harm_limited_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_boundary__harm_limited_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(speech_protection_boundary__harm_limited_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_boundary__harm_limited_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_boundary__harm_limited_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_boundary__harm_limited_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_protection_boundary__harm_limited_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) reflects the cost borne by speakers whose expression is curtailed, particularly those with controversial or dissenting views. Suppression (0.70) is high due to the active enforcement required to identify and sanction 'harmful' speech, and the chilling effect this creates. The theater ratio (0.20) is moderate; while genuine harm prevention occurs, a portion of enforcement activity may be performative or politically motivated, especially when 'harm' is broadly defined. The claimed type is 'tangled_rope' because it genuinely attempts to coordinate social interaction (protecting vulnerable groups) but does so through asymmetric extraction from speakers and by empowering state gatekeepers.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of vulnerable groups, this constraint is a necessary 'rope' for coordination and protection, ensuring their participation in public life. From the perspective of speakers of controversial views, it can feel like a 'snare', as it curtails their expression and empowers state censorship. The state regulators, while benefiting from increased authority, also face the challenge of balancing protection with avoiding overreach.
 *
 * DIRECTIONALITY LOGIC:
 *   Vulnerable groups and state regulators are beneficiaries, as the constraint aims to protect the former and empowers the latter. Speakers of controversial views and political dissidents are victims, as their speech is directly targeted for limitation. Absolutist advocates are excluded, as their fundamental premise is incompatible with this reading. Balancing advocates are observers, analyzing the practical effects of this approach.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_of_significant_harm,
    'How is ''significant harm to dignity, equality, and freedom from harassment'' precisely defined and consistently applied in practice?',
    'Analysis of judicial precedents and regulatory guidelines over time, assessing the consistency and specificity of harm definitions across diverse cases and contexts.',
    'If definitions are vague or inconsistently applied, the constraint''s suppression and extractiveness are higher than measured, as speakers face unpredictable boundaries. If definitions are clear and narrowly tailored, the constraint functions closer to a legitimate coordination mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definition_of_significant_harm, empirical, 'Ambiguity in the definition of ''harm'' leads to unpredictable application and potential for abuse.').

omega_variable(
    state_gatekeeper_abuse_risk,
    'To what extent does the state''s role as a gatekeeper of ''harmful'' speech lead to the suppression of legitimate dissent or political opposition?',
    'Empirical studies of speech prosecutions and regulatory actions, particularly those targeting political speech or minority viewpoints, and comparison with international human rights standards.',
    'If significant abuse is detected, the constraint''s classification shifts closer to a ''snare'' for political dissidents, as the coordination function becomes cover for political suppression. If abuse is minimal, the ''tangled_rope'' classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_gatekeeper_abuse_risk, empirical, 'The risk of state power being used to suppress dissent under the guise of harm prevention.').

omega_variable(
    framing_underdetermination_absolutist,
    'Is the ''absolutist_reading'' truly foreclosed by this harm-limited framework, or does it merely represent a competing, coexisting normative commitment?',
    'Conceptual analysis of the logical compatibility of the core axioms: if the harm-limited axiom (speech is conditional on harm absence) directly contradicts the absolutist axiom (speech is near-absolute), then foreclosure holds. Otherwise, they coexist as competing normative frameworks.',
    'If foreclosed, this reading represents a fundamental shift in constitutional understanding. If coexisting, the contest over speech boundaries remains a live, unresolved normative dispute between different societal factions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_underdetermination_absolutist, conceptual, 'Whether the harm-limited reading logically excludes the absolutist position or merely competes with it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_boundary__harm_limited_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_protection_boundary__harm_limited_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(spee_tr_t5, speech_protection_boundary__harm_limited_reading, theater_ratio, 5, 0.13).
narrative_ontology:measurement(spee_tr_t10, speech_protection_boundary__harm_limited_reading, theater_ratio, 10, 0.16).
narrative_ontology:measurement(spee_tr_t15, speech_protection_boundary__harm_limited_reading, theater_ratio, 15, 0.18).
narrative_ontology:measurement(spee_tr_t20, speech_protection_boundary__harm_limited_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_protection_boundary__harm_limited_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(spee_be_t5, speech_protection_boundary__harm_limited_reading, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(spee_be_t10, speech_protection_boundary__harm_limited_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(spee_be_t15, speech_protection_boundary__harm_limited_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(spee_be_t20, speech_protection_boundary__harm_limited_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_protection_boundary__harm_limited_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(spee_su_t5, speech_protection_boundary__harm_limited_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(spee_su_t10, speech_protection_boundary__harm_limited_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(spee_su_t15, speech_protection_boundary__harm_limited_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(spee_su_t20, speech_protection_boundary__harm_limited_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_boundary__harm_limited_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is the 'harm_limited_reading' of the 'speech_protection_boundary' kernel. It is structurally distinct from the 'absolutist_reading' and 'balancing_reading' due to its explicit conditioning of speech on the absence of harm to dignity, equality, and freedom from harassment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
