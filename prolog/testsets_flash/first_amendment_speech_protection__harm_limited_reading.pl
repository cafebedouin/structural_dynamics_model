% ============================================================================
% CONSTRAINT STORY: first_amendment_speech_protection__harm_limited_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_first_amendment_speech_protection__harm_limited_reading, []).

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
 *   constraint_id: first_amendment_speech_protection__harm_limited_reading
 *   human_readable: First Amendment Speech Protection (Harm-Limited Reading)
 *   domain: constitutional_law/political_philosophy/speech_regulation
 *
 * SUMMARY:
 *   This constraint represents the 'harm-limited' reading of First Amendment
 *   speech protection, where speech is protected unless it causes
 *   demonstrable, unconsented-to harm. This reading contrasts with absolutist
 *   views (which protect nearly all speech) and categorical balancing (which
 *   defines fixed categories of unprotected speech). It is a dynamic
 *   constraint, requiring active judicial interpretation and enforcement to
 *   define the harm boundary, and it shifts the burden of certain
 *   speech-related harms from victims to speakers.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(first_amendment_speech_protection__harm_limited_reading, 0.6).
domain_priors:suppression_score(first_amendment_speech_protection__harm_limited_reading, 0.4).
domain_priors:theater_ratio(first_amendment_speech_protection__harm_limited_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(first_amendment_speech_protection__harm_limited_reading, tangled_rope).
narrative_ontology:human_readable(first_amendment_speech_protection__harm_limited_reading, "First Amendment Speech Protection (Harm-Limited Reading)").
narrative_ontology:topic_domain(first_amendment_speech_protection__harm_limited_reading, "constitutional_law/political_philosophy/speech_regulation").

domain_priors:requires_active_enforcement(first_amendment_speech_protection__harm_limited_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(first_amendment_speech_protection__harm_limited_reading, '36f06e72-8963-4b59-88fd-b6e7c257053b').
narrative_ontology:cs_kernel_codification('36f06e72-8963-4b59-88fd-b6e7c257053b', fixed_text).
narrative_ontology:cs_authority_grounding('36f06e72-8963-4b59-88fd-b6e7c257053b', lineage).
narrative_ontology:cs_interpretation_layer_present('36f06e72-8963-4b59-88fd-b6e7c257053b').
narrative_ontology:cs_reading_relation('36f06e72-8963-4b59-88fd-b6e7c257053b', first_amendment_speech_protection__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('36f06e72-8963-4b59-88fd-b6e7c257053b', first_amendment_speech_protection__categorical_balancing_reading, coexists_with).
narrative_ontology:cs_axiom('36f06e72-8963-4b59-88fd-b6e7c257053b', foundational, speech_rights_yield_to_demonstrable_harm).
narrative_ontology:cs_axiom_status(speech_rights_yield_to_demonstrable_harm, holdable).
narrative_ontology:cs_axiom_grounding('36f06e72-8963-4b59-88fd-b6e7c257053b', speech_rights_yield_to_demonstrable_harm, empirically_contingent).
narrative_ontology:cs_axiom('36f06e72-8963-4b59-88fd-b6e7c257053b', secondary, unconsented_harm_is_a_legitimate_basis_for_regulation).
narrative_ontology:cs_axiom_status(unconsented_harm_is_a_legitimate_basis_for_regulation, holdable).
narrative_ontology:cs_axiom_grounding('36f06e72-8963-4b59-88fd-b6e7c257053b', unconsented_harm_is_a_legitimate_basis_for_regulation, deontological).
narrative_ontology:cs_reference_frame('36f06e72-8963-4b59-88fd-b6e7c257053b', post_brandenburg_v_ohio_framework).
narrative_ontology:cs_drift_state('36f06e72-8963-4b59-88fd-b6e7c257053b', contemporary_digital_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('36f06e72-8963-4b59-88fd-b6e7c257053b', '').
narrative_ontology:cs_kernel_id(first_amendment_speech_protection__harm_limited_reading, first_amendment_speech_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__harm_limited_reading, vulnerable_minorities).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__harm_limited_reading, public_safety_advocates).
narrative_ontology:constraint_victim(first_amendment_speech_protection__harm_limited_reading, speakers_of_harmful_speech).
narrative_ontology:constraint_victim(first_amendment_speech_protection__harm_limited_reading, free_speech_absolutists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__harm_limited_reading, general_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the ability to seek legal recourse or regulatory intervention against speech that causes them demonstrable, unconsented-to harm (e.g., incitement to violence, targeted harassment). Their safety and dignity are protected by this reading, but they must actively demonstrate harm.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, vulnerable_minorities, beneficiary,
    organized, generational, constrained, national).

% Bear the cost of having their speech restricted or penalized when it is found to cause demonstrable, unconsented-to harm. Their expressive freedom is curtailed at the boundary of harm, requiring them to self-censor or face legal consequences.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, speakers_of_harmful_speech, payer,
    moderate, immediate, constrained, national).

% Interprets and applies the First Amendment, determining when speech causes sufficient harm to lose protection. They balance expressive freedom against the prevention of harm, setting precedents that define the boundaries of protected speech under this reading.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Advocate for regulations that prevent speech from causing harm, seeing this reading as a necessary tool for maintaining social order and protecting vulnerable populations. They benefit from the legal framework that allows for such interventions.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, public_safety_advocates, beneficiary,
    organized, biographical, mobile, national).

% Oppose any limitation on speech based on its content or potential for harm, viewing such restrictions as an infringement on fundamental rights. They bear the cost of a legal framework that permits speech regulation, as it contradicts their core ideological commitment.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, free_speech_absolutists, payer,
    organized, generational, identity_locked, national).

% Benefits from a society where speech is generally free but also where egregious harms caused by speech can be addressed. They navigate a complex landscape where the line between protected expression and harmful speech is constantly debated and refined.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, general_public, beneficiary,
    powerless, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the exercise of free speech with the prevention of demonstrable, unconsented-to harm, aiming to balance individual expressive rights with collective safety and well-being.
% TRANSFER_FUNCTION: Transfers the burden of certain speech-related harms from vulnerable individuals and groups to speakers whose expression is deemed to cause such harms, by permitting regulation or penalty.
% ABSENT_VOICES: Those who suffer diffuse, unquantifiable, or unconsented-to harms from speech that does not meet the 'demonstrable' threshold for regulation. They would argue for a broader definition of harm or a lower bar for intervention, but their experiences often fall outside the current legal framework.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the legal landscape for speech would become either entirely absolutist (leading to unchecked harmful speech) or entirely categorical (leading to arbitrary restrictions). The current balance, however imperfect, structures a significant portion of public discourse and legal challenge.
% FOUNDING_PROBLEM: The inherent tension between the desire for robust public discourse and the need to protect individuals and society from direct, severe harms caused by speech.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, civil rights organizations, and public safety advocates outside the immediate beneficiary groups consistently attest to the ongoing challenge of balancing speech and harm, citing contemporary issues like online harassment, disinformation, and incitement. This corroboration supports the problem's live status.
narrative_ontology:disappearance_verdict(first_amendment_speech_protection__harm_limited_reading, world_rearranges).
narrative_ontology:founding_problem_status(first_amendment_speech_protection__harm_limited_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(first_amendment_speech_protection__harm_limited_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(first_amendment_speech_protection__harm_limited_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(first_amendment_speech_protection__harm_limited_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(first_amendment_speech_protection__harm_limited_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(first_amendment_speech_protection__harm_limited_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates expressive freedom with harm prevention (beneficiaries: vulnerable minorities, public safety advocates) but also involves asymmetric extraction from speakers whose speech is deemed harmful (victims: speakers of harmful speech, free speech absolutists). It requires active enforcement by the judiciary to define and police the harm boundary. Extractiveness is moderate-high (0.6) as it curtails expressive freedom for some, while suppression is moderate (0.4) as it actively restricts certain forms of speech. Theater ratio is low (0.1) as the harm-prevention function is generally genuine, though the definition of 'harm' is contested.
 *
 * PERSPECTIVAL GAP:
 *   The judiciary and beneficiaries perceive this as a legitimate and necessary balancing act, ensuring a functional society. The payers and victims, particularly free speech absolutists, perceive it as an illegitimate infringement on core constitutional principles, viewing the 'harm' justification as a cover for censorship. This divergence is central to the ongoing legal and philosophical debate.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary (agenda_setter) and vulnerable minorities/public safety advocates (beneficiaries) experience this as a necessary coordination mechanism. Speakers of harmful speech and free speech absolutists (payers/victims) experience it as an extractive limitation on fundamental rights. The general public benefits from a more orderly discourse but also bears the cost of navigating complex speech regulations.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    demonstrable_harm_definition,
    'What constitutes ''demonstrable, unconsented-to harm'' in practice, and how consistently is this standard applied across different types of speech and contexts?',
    'Empirical analysis of judicial rulings and regulatory enforcement actions, tracking the types of harms recognized and the evidentiary burden required for restriction.',
    'If the definition of harm is inconsistently applied or expands to include subjective offense, the constraint''s extractiveness and suppression would be higher than measured, potentially shifting its classification towards a Snare for speakers. If the definition remains narrow and objective, its coordination function is stronger.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(demonstrable_harm_definition, empirical, 'Ambiguity in the definition and application of ''demonstrable harm''.').

omega_variable(
    absolutist_vs_harm_limited_framing,
    'Is the ''harm-limited'' reading a legitimate interpretation of the First Amendment''s original intent, or a policy-driven departure from an ''absolutist'' constitutional command?',
    'Historical-legal scholarship on the drafting and early interpretation of the First Amendment, combined with philosophical analysis of constitutional interpretation methodologies.',
    'If found to be a departure, the constraint''s legitimacy would be undermined for those who adhere to originalist or absolutist interpretations, increasing perceived extraction and resistance. If found consistent, its perceived legitimacy would be strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(absolutist_vs_harm_limited_framing, conceptual, 'Conceptual dispute over the foundational interpretation of the First Amendment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(first_amendment_speech_protection__harm_limited_reading, 1969, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(firs_tr_t1969, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 1969, 0.05).
narrative_ontology:measurement(firs_tr_t1980, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 1980, 0.07).
narrative_ontology:measurement(firs_tr_t1990, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 1990, 0.08).
narrative_ontology:measurement(firs_tr_t2000, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 2000, 0.09).
narrative_ontology:measurement(firs_tr_t2010, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(firs_tr_t2024, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(firs_be_t1969, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 1969, 0.4).
narrative_ontology:measurement(firs_be_t1980, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 1980, 0.45).
narrative_ontology:measurement(firs_be_t1990, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 1990, 0.5).
narrative_ontology:measurement(firs_be_t2000, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(firs_be_t2010, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 2010, 0.58).
narrative_ontology:measurement(firs_be_t2024, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 2024, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(firs_su_t1969, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 1969, 0.25).
narrative_ontology:measurement(firs_su_t1980, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 1980, 0.3).
narrative_ontology:measurement(firs_su_t1990, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 1990, 0.35).
narrative_ontology:measurement(firs_su_t2000, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 2000, 0.38).
narrative_ontology:measurement(firs_su_t2010, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 2010, 0.39).
narrative_ontology:measurement(firs_su_t2024, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 2024, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(first_amendment_speech_protection__harm_limited_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(first_amendment_speech_protection__harm_limited_reading, first_amendment_speech_protection__absolutist_reading).
narrative_ontology:affects_constraint(first_amendment_speech_protection__harm_limited_reading, first_amendment_speech_protection__categorical_balancing_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'First Amendment speech protection' kernel. Its structural properties differ significantly from the absolutist and categorical balancing readings, particularly in its definition of protected speech and its beneficiaries/victims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
