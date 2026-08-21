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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: speech_harm_boundary__harm_balancing_reading
 *   human_readable: Speech Harm Balancing Principle
 *   domain: Constitutional Law/Political Philosophy/Communication Ethics
 *
 * SUMMARY:
 *   This constraint represents the 'harm_balancing_reading' of the broader
 *   'speech_harm_boundary' kernel. It posits that while speech is
 *   presumptively protected, this protection yields to demonstrated harm,
 *   requiring a proportionality balancing test. This reading acknowledges the
 *   social utility of free expression but prioritizes the prevention of
 *   direct and severe harms, leading to categories of unprotected speech such
 *   as incitement, harassment, and group libel. The constraint is actively
 *   enforced through judicial and regulatory adjudication.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_harm_boundary__harm_balancing_reading, 0.45).
domain_priors:suppression_score(speech_harm_boundary__harm_balancing_reading, 0.55).
domain_priors:theater_ratio(speech_harm_boundary__harm_balancing_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_harm_boundary__harm_balancing_reading, tangled_rope).
narrative_ontology:human_readable(speech_harm_boundary__harm_balancing_reading, "Speech Harm Balancing Principle").
narrative_ontology:topic_domain(speech_harm_boundary__harm_balancing_reading, "Constitutional Law/Political Philosophy/Communication Ethics").

domain_priors:requires_active_enforcement(speech_harm_boundary__harm_balancing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_harm_boundary__harm_balancing_reading, '7c9d282a-c73c-47cb-9910-9092de813545').
narrative_ontology:cs_kernel_codification('7c9d282a-c73c-47cb-9910-9092de813545', formalized).
narrative_ontology:cs_authority_grounding('7c9d282a-c73c-47cb-9910-9092de813545', lineage).
narrative_ontology:cs_interpretation_layer_present('7c9d282a-c73c-47cb-9910-9092de813545').
narrative_ontology:cs_reading_relation('7c9d282a-c73c-47cb-9910-9092de813545', speech_harm_boundary__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('7c9d282a-c73c-47cb-9910-9092de813545', speech_harm_boundary__dignity_reading, coexists_with).
narrative_ontology:cs_axiom('7c9d282a-c73c-47cb-9910-9092de813545', foundational, speech_is_presumptively_free).
narrative_ontology:cs_axiom_status(speech_is_presumptively_free, holdable).
narrative_ontology:cs_axiom_grounding('7c9d282a-c73c-47cb-9910-9092de813545', speech_is_presumptively_free, deontological).
narrative_ontology:cs_axiom('7c9d282a-c73c-47cb-9910-9092de813545', foundational, harm_principle_justifies_restriction).
narrative_ontology:cs_axiom_status(harm_principle_justifies_restriction, holdable).
narrative_ontology:cs_axiom_grounding('7c9d282a-c73c-47cb-9910-9092de813545', harm_principle_justifies_restriction, conventional).
narrative_ontology:cs_reference_frame('7c9d282a-c73c-47cb-9910-9092de813545', liberal_democratic_balancing_framework).
narrative_ontology:cs_drift_state('7c9d282a-c73c-47cb-9910-9092de813545', contemporary_digital_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('7c9d282a-c73c-47cb-9910-9092de813545', '').
narrative_ontology:cs_kernel_id(speech_harm_boundary__harm_balancing_reading, speech_harm_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_harm_boundary__harm_balancing_reading, society_at_large).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__harm_balancing_reading, vulnerable_groups).
narrative_ontology:constraint_victim(speech_harm_boundary__harm_balancing_reading, speakers_of_harmful_speech).
narrative_ontology:constraint_vindicates(speech_harm_boundary__harm_balancing_reading, harm_principle).
narrative_ontology:constraint_vindicates(speech_harm_boundary__harm_balancing_reading, proportionality_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from a public discourse environment where demonstrable harms (e.g., incitement, harassment) are mitigated, fostering social cohesion and safety. Bears diffuse costs of enforcement and potential over-restriction.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, society_at_large, beneficiary,
    moderate, generational, constrained, national).

% Are primary beneficiaries of restrictions on speech that causes demonstrable harm, as they are often disproportionately targeted by hate speech, harassment, and incitement. Their safety and dignity are directly protected.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, vulnerable_groups, beneficiary,
    powerless, biographical, trapped, local).

% Bear the costs of speech restriction when their expression is deemed to cause demonstrable harm. Their expressive freedom is curtailed, and they may face legal penalties. Their exit options are to self-censor or face legal consequences.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, speakers_of_harmful_speech, payer,
    moderate, immediate, constrained, local).

% Courts, regulatory agencies, and other bodies responsible for interpreting and applying the harm balancing test. They define what constitutes 'demonstrable harm' and ensure proportionality in restrictions. They enforce the constraint.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, adjudicating_bodies, agenda_setter,
    institutional, biographical, analytical, national).

% Argue for near-absolute speech protection, with an extremely high threshold for harm override. They are excluded from the core premise of this balancing reading, which accepts that speech can yield to demonstrated harm.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, absolutist_advocates, excluded,
    organized, generational, analytical, national).

% Argue that speech protection should be subordinate to human dignity, making personhood-denying speech categorically unprotected. While their concerns overlap with harm, their foundational premise for restriction differs from this reading's harm-centric balancing.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, dignity_advocates, excluded,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_harm_boundary__harm_balancing_reading, society_at_large).
narrative_ontology:fixing_cost_class(speech_harm_boundary__harm_balancing_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate the exercise of free expression with the prevention of demonstrable harm to individuals and groups, ensuring public discourse operates within bounds of mutual respect and safety.
% TRANSFER_FUNCTION: Transfers the cost of speech restriction (loss of expressive freedom) from potential victims of harm to speakers whose expression is adjudicated as causing demonstrable harm, based on a proportionality assessment.
% ABSENT_VOICES: Absolutist advocates would object to any balancing that allows for significant speech restriction, arguing it chills legitimate expression. Dignity advocates would argue for a more robust, categorical protection for vulnerable groups based on inherent worth, rather than a case-by-case harm balancing.
% DISAPPEARANCE_RATIONALE: If the principle of balancing speech with demonstrable harm vanished, society would face unchecked hate speech, harassment, incitement to violence, and defamation. This would lead to social fragmentation, increased inter-group conflict, and a breakdown of civil discourse, forcing a rapid reorganization of social and legal norms.
% FOUNDING_PROBLEM: Unfettered speech leading to direct and demonstrable harm to individuals and groups, undermining social cohesion, individual safety, and the ability of marginalized communities to participate equally in public life.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, social scientists, human rights organizations, and public health bodies (outside the direct beneficiaries of specific restrictions) corroborate the ongoing challenge of harmful speech and the necessity of mechanisms to balance it with free expression. This is evidenced by ongoing debates, legislative efforts, and empirical studies on the impact of online and offline speech harms.
narrative_ontology:disappearance_verdict(speech_harm_boundary__harm_balancing_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_harm_boundary__harm_balancing_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_harm_boundary__harm_balancing_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   Extractiveness is moderate (0.45) because speakers whose speech is restricted bear a real cost in terms of expressive freedom, but this is balanced against the harm prevented. Suppression is moderate (0.55) as the constraint requires active enforcement and adjudication to identify and restrict harmful speech. Theater ratio is low (0.15) because the balancing act is a genuine, ongoing function of legal systems, not merely performative. Accessibility collapse is moderate (0.40) as alternatives for harmful speech are constrained, but other forms of expression remain accessible. Resistance is moderate (0.50) due to ongoing legal and philosophical debates about the scope of 'harm' and the appropriate balance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of vulnerable groups, the constraint is a vital protection, enabling their participation in society. From the perspective of speakers whose speech is restricted, it can feel like an arbitrary curtailment of fundamental rights. Adjudicating bodies view it as a necessary and complex legal function. The engine's per-seat classification will reflect these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   Society at large and vulnerable groups are beneficiaries (low directionality) as they gain protection from harm. Speakers of harmful speech are targets (high directionality) as their expression is restricted. Adjudicating bodies are agenda-setters, responsible for maintaining the balance. Absolutist and dignity advocates are 'excluded' from this reading's core premise, meaning their foundational arguments are not fully integrated into this specific balancing framework.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint actively resists mandatrophy by continuously adapting its definition of 'demonstrable harm' and 'proportionality' through ongoing legal interpretation and societal evolution. Its persistence is tied to the live problem of speech-related harms, preventing it from becoming a mere inertial structure. The balancing act itself is the core function, not a cover for extraction, though extraction from harmful speech is an inherent part of its operation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately representing the ''harm_balancing_reading'' of the ''speech_harm_boundary'' kernel, distinct from its sibling readings?',
    'Comparative analysis with legal and philosophical texts defining each reading, ensuring the core premises and structural deltas are correctly captured.',
    'Misidentification would lead to incorrect classification and an inaccurate mapping of the kernel''s contested landscape, potentially conflating distinct normative frameworks.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Confirms this constraint as a specific reading of the speech harm kernel.').

omega_variable(
    demonstrated_harm_objectivity,
    'What constitutes ''demonstrated harm'' and how is it measured objectively and consistently across diverse contexts and evolving communication technologies?',
    'Empirical studies on the impact of speech, development of clear legal standards and evidentiary thresholds, and consistent judicial application across jurisdictions.',
    'If ''demonstrated harm'' is subjective or inconsistently applied, the constraint''s extractiveness could increase due to arbitrary restrictions, potentially shifting its classification towards a Snare for speakers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demonstrated_harm_objectivity, empirical, 'Ambiguity in defining and measuring ''demonstrated harm''.').

omega_variable(
    proportionality_application,
    'Is the proportionality balancing test consistently applied to ensure that speech restrictions are no more extensive than necessary to prevent the demonstrated harm?',
    'Systematic review of judicial decisions and regulatory actions, including appeals and challenges, to identify patterns of over-restriction or under-restriction.',
    'If proportionality is routinely violated, the constraint''s suppression and extractiveness would be higher than intended, indicating a drift towards a more extractive type for speakers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_application, empirical, 'Consistency and fairness of proportionality balancing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_harm_boundary__harm_balancing_reading, 1950, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t1950, speech_harm_boundary__harm_balancing_reading, theater_ratio, 1950, 0.15).
narrative_ontology:measurement(spee_tr_t1965, speech_harm_boundary__harm_balancing_reading, theater_ratio, 1965, 0.15).
narrative_ontology:measurement(spee_tr_t1980, speech_harm_boundary__harm_balancing_reading, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(spee_tr_t1995, speech_harm_boundary__harm_balancing_reading, theater_ratio, 1995, 0.15).
narrative_ontology:measurement(spee_tr_t2010, speech_harm_boundary__harm_balancing_reading, theater_ratio, 2010, 0.15).
narrative_ontology:measurement(spee_tr_t2025, speech_harm_boundary__harm_balancing_reading, theater_ratio, 2025, 0.15).

% Extraction over time
narrative_ontology:measurement(spee_be_t1950, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 1950, 0.35).
narrative_ontology:measurement(spee_be_t1965, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 1965, 0.38).
narrative_ontology:measurement(spee_be_t1980, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 1980, 0.4).
narrative_ontology:measurement(spee_be_t1995, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 1995, 0.42).
narrative_ontology:measurement(spee_be_t2010, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 2010, 0.44).
narrative_ontology:measurement(spee_be_t2025, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 2025, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t1950, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 1950, 0.45).
narrative_ontology:measurement(spee_su_t1965, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 1965, 0.48).
narrative_ontology:measurement(spee_su_t1980, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 1980, 0.5).
narrative_ontology:measurement(spee_su_t1995, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 1995, 0.52).
narrative_ontology:measurement(spee_su_t2010, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 2010, 0.54).
narrative_ontology:measurement(spee_su_t2025, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 2025, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_harm_boundary__harm_balancing_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
