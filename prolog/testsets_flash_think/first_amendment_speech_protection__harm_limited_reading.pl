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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: first_amendment_speech_protection__harm_limited_reading
 *   human_readable: First Amendment Speech Protection: Harm-Limited Reading
 *   domain: constitutional_law/political_philosophy/speech_regulation
 *
 * SUMMARY:
 *   This constraint represents the 'harm-limited' reading of First Amendment
 *   speech protection, which posits that constitutional protection for speech
 *   yields when that speech causes demonstrable, unconsented-to harm. This
 *   reading contrasts with absolutist views (where 'no law' means virtually
 *   no restriction) and categorical balancing (where speech is sorted into
 *   protected/unprotected categories based on content). The harm-limited
 *   reading emphasizes the consequences of speech, allowing regulation when a
 *   clear link to harm can be established. This story instantiates one
 *   specific interpretation of the First Amendment kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(first_amendment_speech_protection__harm_limited_reading, 0.68).
domain_priors:suppression_score(first_amendment_speech_protection__harm_limited_reading, 0.75).
domain_priors:theater_ratio(first_amendment_speech_protection__harm_limited_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(first_amendment_speech_protection__harm_limited_reading, tangled_rope).
narrative_ontology:human_readable(first_amendment_speech_protection__harm_limited_reading, "First Amendment Speech Protection: Harm-Limited Reading").
narrative_ontology:topic_domain(first_amendment_speech_protection__harm_limited_reading, "constitutional_law/political_philosophy/speech_regulation").

domain_priors:requires_active_enforcement(first_amendment_speech_protection__harm_limited_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(first_amendment_speech_protection__harm_limited_reading, 'e2868566-f160-4e27-b6a8-12b8cadffc9d').
narrative_ontology:cs_kernel_codification('e2868566-f160-4e27-b6a8-12b8cadffc9d', fixed_text).
narrative_ontology:cs_authority_grounding('e2868566-f160-4e27-b6a8-12b8cadffc9d', lineage).
narrative_ontology:cs_interpretation_layer_present('e2868566-f160-4e27-b6a8-12b8cadffc9d').
narrative_ontology:cs_reading_relation('e2868566-f160-4e27-b6a8-12b8cadffc9d', first_amendment_speech_protection__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('e2868566-f160-4e27-b6a8-12b8cadffc9d', first_amendment_speech_protection__categorical_balancing_reading, coexists_with).
narrative_ontology:cs_axiom('e2868566-f160-4e27-b6a8-12b8cadffc9d', foundational, speech_rights_are_not_absolute).
narrative_ontology:cs_axiom_status(speech_rights_are_not_absolute, holdable).
narrative_ontology:cs_axiom_grounding('e2868566-f160-4e27-b6a8-12b8cadffc9d', speech_rights_are_not_absolute, deontological).
narrative_ontology:cs_axiom('e2868566-f160-4e27-b6a8-12b8cadffc9d', foundational, demonstrable_harm_justifies_restriction).
narrative_ontology:cs_axiom_status(demonstrable_harm_justifies_restriction, holdable).
narrative_ontology:cs_axiom_grounding('e2868566-f160-4e27-b6a8-12b8cadffc9d', demonstrable_harm_justifies_restriction, empirically_contingent).
narrative_ontology:cs_reference_frame('e2868566-f160-4e27-b6a8-12b8cadffc9d', post_new_york_times_v_sullivan_era).
narrative_ontology:cs_drift_state('e2868566-f160-4e27-b6a8-12b8cadffc9d', contemporary_digital_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('e2868566-f160-4e27-b6a8-12b8cadffc9d', '').
narrative_ontology:cs_kernel_id(first_amendment_speech_protection__harm_limited_reading, first_amendment_speech_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__harm_limited_reading, vulnerable_minorities).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__harm_limited_reading, public_safety_advocates).
narrative_ontology:constraint_victim(first_amendment_speech_protection__harm_limited_reading, speakers_of_harmful_speech).
narrative_ontology:constraint_victim(first_amendment_speech_protection__harm_limited_reading, free_speech_absolutists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from legal protections against speech that causes them demonstrable, unconsented-to harm, such as incitement to violence, defamation, or harassment. Their ability to participate in public life is enhanced by these limits.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, vulnerable_minorities, beneficiary,
    powerless, immediate, constrained, national).

% Advocate for speech regulations that prioritize public safety and social cohesion over unfettered expression. They see the harm-limited reading as a necessary and just interpretation of constitutional rights.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, public_safety_advocates, beneficiary,
    organized, biographical, mobile, national).

% Bear the cost of having their speech restricted or penalized when it is deemed to cause demonstrable, unconsented-to harm. Their expressive freedom is curtailed by this interpretation, forcing them to self-censor or face legal consequences.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, speakers_of_harmful_speech, payer,
    moderate, immediate, constrained, local).

% Oppose any restriction on speech, viewing the harm-limited reading as an erosion of fundamental constitutional rights. They bear the cost of a legal framework that permits regulation of speech they believe should be absolutely protected.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, free_speech_absolutists, payer,
    organized, generational, constrained, national).

% Interpret and enforce the harm-limited reading, adjudicating specific cases where speech is alleged to cause harm. They define the boundaries of 'demonstrable harm' and 'unconsented-to' in practice, shaping the scope of protected speech.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, courts_and_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Academics and legal theorists who argue for a near-absolute interpretation of the First Amendment, often finding themselves outside the mainstream judicial discourse that incorporates harm limitations. Their arguments are often considered but rarely adopted as primary legal doctrine in this reading.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, absolutist_scholars, excluded,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate the exercise of free speech rights with the protection of individuals and groups from direct, demonstrable, and unconsented-to harms, ensuring a functional and safe public sphere.
% TRANSFER_FUNCTION: Transfers the burden of enduring certain types of speech from vulnerable individuals and groups to speakers whose expression causes demonstrable harm, by restricting or penalizing such speech.
% ABSENT_VOICES: Speakers and legal theorists who adhere to a strict absolutist interpretation of the First Amendment are often marginalized in the discourse surrounding the harm-limited reading, as their core premise is rejected by this framework.
% DISAPPEARANCE_RATIONALE: If the harm-limited reading vanished, the legal landscape would shift dramatically towards a more permissive speech environment, likely leading to increased instances of speech-induced harm, particularly for vulnerable groups. This would necessitate a societal reorganization to cope with the consequences, potentially leading to new, informal, or extra-legal forms of speech control.
% FOUNDING_PROBLEM: The challenge of reconciling the constitutional guarantee of free speech with the need to protect individuals and society from direct, unconsented-to harms caused by speech, especially as new forms of communication and potential harms emerge.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, civil rights organizations, and public health experts, often independent of the direct beneficiaries, consistently corroborate the ongoing challenge of balancing speech and harm, citing real-world consequences of unregulated harmful speech in areas like online harassment, disinformation, and incitement.
narrative_ontology:disappearance_verdict(first_amendment_speech_protection__harm_limited_reading, world_rearranges).
narrative_ontology:founding_problem_status(first_amendment_speech_protection__harm_limited_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(first_amendment_speech_protection__harm_limited_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(first_amendment_speech_protection__harm_limited_reading, 'none', 1).
narrative_ontology:epsilon_provenance(first_amendment_speech_protection__harm_limited_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.68) is moderately high because this reading actively curtails certain forms of speech, extracting the right to speak without consequence from those whose expression causes harm. Suppression (0.75) is also high, as it requires active judicial and regulatory enforcement to identify, adjudicate, and penalize harmful speech. Theater ratio (0.10) is low, as the process of identifying and litigating speech-related harm is generally functional and not performative. Accessibility collapse (0.60) reflects that while many forms of speech remain protected, the 'alternative' of unfettered, consequence-free harmful speech is significantly curtailed. Resistance (0.70) is high, as speakers whose speech is restricted, and those who advocate for broader speech rights, actively challenge this interpretation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of vulnerable minorities, this reading is a necessary safeguard for their safety and dignity, enabling their participation in society. From the perspective of free speech absolutists, it is an unacceptable infringement on fundamental rights, leading to a 'chilling effect' on legitimate expression. The engine's per-seat classification will reflect these divergent experiences based on the structural roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Vulnerable minorities and public safety advocates are clear beneficiaries, as the constraint aims to protect them from harm. Speakers of harmful speech and free speech absolutists are the primary targets/payers, as their expressive freedom is curtailed. Courts and regulators act as agenda-setters, defining and enforcing the boundaries of this reading. The directionality for beneficiaries is low (subsidized protection), and for victims/targets is high (extraction of speech rights).
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Tangled Rope prevents mislabeling this reading as a pure Rope (which would imply universal benefit with minimal extraction) or a pure Snare (which would imply no genuine coordination function). By acknowledging both the coordination function (protecting from harm) and the asymmetric extraction (from speakers of harmful speech), it accurately captures the complex trade-offs and active enforcement required.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    harm_definition_ambiguity,
    'What constitutes ''demonstrable unconsented-to harm'' in practice, and how is it consistently applied across diverse contexts and evolving forms of speech?',
    'Development of clear, judicially consistent, and empirically grounded definitions of harm, coupled with robust evidentiary standards for causation and impact.',
    'If harm definitions remain vague or inconsistently applied, the constraint''s extractiveness and suppression could become arbitrary, leading to over-regulation or under-protection. Clearer definitions would stabilize its operation and potentially reduce perceived extraction for non-harmful speech.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_definition_ambiguity, conceptual, 'Ambiguity in defining and applying the ''harm'' threshold for speech regulation.').

omega_variable(
    causation_standard_ambiguity,
    'What level of causation is required between speech and harm (e.g., direct, indirect, foreseeable, intended), and how does this standard adapt to complex digital environments?',
    'Judicial clarification of causation standards for speech-related harm, potentially informed by interdisciplinary research on online influence and social dynamics.',
    'A high causation standard would narrow the scope of regulated speech, reducing extraction from speakers but potentially leaving vulnerable groups exposed. A lower standard would expand regulation, increasing extraction but offering more protection. The classification''s effective extractiveness (χ) would shift accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causation_standard_ambiguity, empirical, 'Uncertainty regarding the causal link required between speech and harm.').

omega_variable(
    slippery_slope_risk,
    'Does the harm-limited reading inherently create a ''slippery slope'' towards over-regulation of speech, where legitimate expression is increasingly curtailed under the guise of preventing harm?',
    'Longitudinal empirical study of speech regulation trends in jurisdictions adopting this reading, assessing whether the scope of regulated speech expands beyond initial intent, and whether ''harm'' definitions broaden over time.',
    'If a slippery slope is demonstrated, the constraint''s long-term extractiveness and suppression would be higher than currently measured, potentially shifting its classification towards a Snare. If not, the current classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(slippery_slope_risk, empirical, 'Risk of over-regulation due to expanding definitions of harm.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(first_amendment_speech_protection__harm_limited_reading, 1969, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(firs_tr_t1969, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 1969, 0.05).
narrative_ontology:measurement(firs_tr_t1985, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 1985, 0.07).
narrative_ontology:measurement(firs_tr_t2000, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 2000, 0.08).
narrative_ontology:measurement(firs_tr_t2010, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 2010, 0.09).
narrative_ontology:measurement(firs_tr_t2020, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 2020, 0.1).
narrative_ontology:measurement(firs_tr_t2024, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(firs_be_t1969, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 1969, 0.5).
narrative_ontology:measurement(firs_be_t1985, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 1985, 0.58).
narrative_ontology:measurement(firs_be_t2000, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 2000, 0.63).
narrative_ontology:measurement(firs_be_t2010, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 2010, 0.65).
narrative_ontology:measurement(firs_be_t2020, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 2020, 0.67).
narrative_ontology:measurement(firs_be_t2024, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(firs_su_t1969, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 1969, 0.6).
narrative_ontology:measurement(firs_su_t1985, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 1985, 0.65).
narrative_ontology:measurement(firs_su_t2000, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(firs_su_t2010, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 2010, 0.72).
narrative_ontology:measurement(firs_su_t2020, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 2020, 0.74).
narrative_ontology:measurement(firs_su_t2024, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(first_amendment_speech_protection__harm_limited_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(first_amendment_speech_protection__harm_limited_reading, first_amendment_speech_protection__absolutist_reading).
narrative_ontology:affects_constraint(first_amendment_speech_protection__harm_limited_reading, first_amendment_speech_protection__categorical_balancing_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'first_amendment_speech_protection' kernel. Each reading represents a different structural claim about the scope and limits of free speech, with differing ε values and stakeholder impacts. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
