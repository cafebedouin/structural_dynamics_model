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
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint represents the 'absolutist' reading of the First
 *   Amendment's speech protection, where 'no law' is interpreted to mean
 *   categorical protection for speech, with only very narrow, historically
 *   recognized exceptions. This reading maximizes the scope of protected
 *   speech, often externalizing the costs of harmful expression onto targeted
 *   minorities and vulnerable groups. The claimed type is 'tangled_rope'
 *   because it provides a coordination function (clear rules for speakers)
 *   but with significant asymmetric extraction from those harmed by speech,
 *   requiring active enforcement by courts to maintain its boundaries.
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
narrative_ontology:topic_domain(first_amendment_speech_protection__absolutist_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(first_amendment_speech_protection__absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(first_amendment_speech_protection__absolutist_reading, '127e4b10-e665-4852-b8e7-175247be68c4').
narrative_ontology:cs_kernel_codification('127e4b10-e665-4852-b8e7-175247be68c4', fixed_text).
narrative_ontology:cs_authority_grounding('127e4b10-e665-4852-b8e7-175247be68c4', lineage).
narrative_ontology:cs_interpretation_layer_present('127e4b10-e665-4852-b8e7-175247be68c4').
narrative_ontology:cs_reading_relation('127e4b10-e665-4852-b8e7-175247be68c4', first_amendment_speech_protection__harm_limited_reading, coexists_with).
narrative_ontology:cs_reading_relation('127e4b10-e665-4852-b8e7-175247be68c4', first_amendment_speech_protection__categorical_balancing_reading, coexists_with).
narrative_ontology:cs_axiom('127e4b10-e665-4852-b8e7-175247be68c4', foundational, no_law_means_no_law).
narrative_ontology:cs_axiom_status(no_law_means_no_law, holdable).
narrative_ontology:cs_axiom_grounding('127e4b10-e665-4852-b8e7-175247be68c4', no_law_means_no_law, deontological).
narrative_ontology:cs_axiom('127e4b10-e665-4852-b8e7-175247be68c4', foundational, marketplace_of_ideas_maximization).
narrative_ontology:cs_axiom_status(marketplace_of_ideas_maximization, holdable).
narrative_ontology:cs_axiom_grounding('127e4b10-e665-4852-b8e7-175247be68c4', marketplace_of_ideas_maximization, instrumental).
narrative_ontology:cs_reference_frame('127e4b10-e665-4852-b8e7-175247be68c4', founding_era_categorical_protection).
narrative_ontology:cs_drift_state('127e4b10-e665-4852-b8e7-175247be68c4', contemporary_digital_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('127e4b10-e665-4852-b8e7-175247be68c4', '').
narrative_ontology:cs_kernel_id(first_amendment_speech_protection__absolutist_reading, first_amendment_speech_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__absolutist_reading, speakers).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__absolutist_reading, majority_groups).
narrative_ontology:constraint_victim(first_amendment_speech_protection__absolutist_reading, targeted_minorities).
narrative_ontology:constraint_victim(first_amendment_speech_protection__absolutist_reading, vulnerable_groups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(first_amendment_speech_protection__absolutist_reading, legislature).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enjoys broad protection for expression, even when it causes harm to others. The absolutist reading maximizes their freedom from state interference.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, speakers, beneficiary,
    powerful, biographical, mobile, national).

% Benefits from the expansive scope of protected speech, which often aligns with their prevailing views and allows for the free dissemination of their ideas without significant legal challenge.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, majority_groups, beneficiary,
    institutional, generational, arbitrage, national).

% Bears the costs of unprotected hate speech, incitement, and harassment, which are often deemed protected under this reading. Experiences systemic oppression and psychological harm with little legal recourse.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, targeted_minorities, payer,
    powerless, generational, trapped, national).

% Suffers direct and indirect harm from speech that targets them, including threats, intimidation, and the erosion of their social standing and safety. Their ability to participate in public life is constrained by the prevalence of such speech.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, vulnerable_groups, payer,
    powerless, biographical, trapped, local).

% Are tasked with interpreting and enforcing the First Amendment. Under this reading, they primarily focus on identifying narrow historical exceptions to an otherwise categorical protection, often leading to outcomes that prioritize speech over harm prevention.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, courts, agenda_setter,
    institutional, civilizational, constrained, national).

% Is constrained in its ability to pass laws regulating speech, even when such laws are intended to protect vulnerable populations or prevent social harms. The 'no law' interpretation severely limits legislative options.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, legislature, payer,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, categorical boundary for protected speech, minimizing state interference and promoting a robust marketplace of ideas by making most speech immune from regulation.
% TRANSFER_FUNCTION: Transfers the burden of harmful speech from the speaker and the state (which is prevented from regulating) to targeted minorities and vulnerable groups, who bear the social and psychological costs.
% ABSENT_VOICES: The voices of those harmed by speech are often marginalized or dismissed as a necessary cost of liberty. Their perspectives on the impact of speech are not adequately incorporated into the legal framework, leading to a one-sided understanding of 'free speech'.
% DISAPPEARANCE_RATIONALE: If this absolutist reading vanished, the legal landscape for speech would fundamentally shift. Courts would likely adopt more balancing tests, legislatures would gain more power to regulate harmful speech, and the social costs of speech would be reallocated, leading to a significant rearrangement of power dynamics and protections.
% FOUNDING_PROBLEM: To prevent government censorship and ensure a free exchange of ideas, drawing from historical experiences of state suppression of dissent.
% FOUNDING_PROBLEM_CORROBORATION: Historians and legal scholars widely corroborate the founding problem of preventing government censorship. However, the extent to which this problem remains the primary driver, versus the problem of managing speech-induced harm, is contested by civil rights advocates and some legal scholars.
narrative_ontology:disappearance_verdict(first_amendment_speech_protection__absolutist_reading, world_rearranges).
narrative_ontology:founding_problem_status(first_amendment_speech_protection__absolutist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(first_amendment_speech_protection__absolutist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.65) because the broad protection for speech allows for significant harm to be inflicted on vulnerable groups without legal recourse, effectively extracting their safety and well-being as a cost of others' liberty. Suppression is low (0.20) because the state's power to suppress speech is minimal under this reading, but it is 'active enforcement' by courts that maintains the boundary of what little can be suppressed. Theater ratio is low (0.10) as the courts genuinely apply the categorical rule, but the narrative of 'pure liberty' can mask the real-world harms. Resistance is high (0.70) from targeted groups and their advocates who continuously challenge the harms caused by this expansive protection.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of speakers and majority groups, this is a 'rope' or even a 'mountain' of liberty, ensuring a free marketplace of ideas. From the perspective of targeted minorities, it operates as a 'snare' or 'tangled_rope', extracting their safety and dignity under the guise of freedom. The engine's classification as 'tangled_rope' reflects this inherent asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Speakers and majority groups are clear beneficiaries, as their speech is maximally protected. Targeted minorities and vulnerable groups are victims, bearing the brunt of harms without legal protection. Courts act as agenda-setters, enforcing this reading, while the legislature is a payer, constrained in its ability to address social harms through speech regulation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    harm_externalization_measurement,
    'How can the ''extraction'' of safety and well-being from targeted minorities be quantitatively measured and attributed to this specific reading of the First Amendment?',
    'Longitudinal studies on the impact of unprotected hate speech on mental health, economic opportunity, and civic participation of vulnerable groups, correlated with legal precedents set by this reading.',
    'Clearer quantification of harm would strengthen arguments for re-evaluating the absolutist reading''s social costs, potentially shifting its classification towards a higher extractiveness or even a ''snare'' from the victims'' perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_externalization_measurement, empirical, 'Measuring the externalized costs of broad speech protection.').

omega_variable(
    historical_exceptions_scope,
    'What is the precise and historically defensible scope of ''narrow historical exclusions'' to speech protection, and how much has this scope been expanded or contracted by judicial interpretation?',
    'Detailed historical-legal analysis of founding-era and subsequent jurisprudence on speech categories (e.g., incitement, defamation, obscenity) to establish a baseline and track deviations.',
    'If the ''narrow historical exclusions'' have been judicially expanded beyond their original scope, it suggests a drift towards a more ''categorical_balancing_reading'' in practice, even if the absolutist rhetoric persists. If contracted, it reinforces the absolutist nature.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_exceptions_scope, empirical, 'Defining and tracking the boundaries of historical speech exclusions.').

omega_variable(
    absolutist_vs_balancing_framing,
    'Is the ''absolutist'' framing a genuine structural commitment to categorical protection, or a rhetorical cover for a de facto balancing test that consistently favors speakers over those harmed?',
    'Comparative analysis of judicial outcomes: if outcomes consistently favor speakers even in cases of severe harm, despite the ''absolutist'' rhetoric, it suggests a de facto balancing test is at play, with a strong bias.',
    'If it''s a rhetorical cover, the constraint''s ''theater_ratio'' would be higher, and its ''claimed_type'' might be more accurately ''tangled_rope'' or ''snare'' even from the agenda-setter''s perspective, as the coordination story is less genuine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(absolutist_vs_balancing_framing, conceptual, 'Distinguishing genuine absolutism from biased balancing under an absolutist guise.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(first_amendment_speech_protection__absolutist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(firs_tr_t0, first_amendment_speech_protection__absolutist_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(firs_tr_t10, first_amendment_speech_protection__absolutist_reading, theater_ratio, 10, 0.06).
narrative_ontology:measurement(firs_tr_t20, first_amendment_speech_protection__absolutist_reading, theater_ratio, 20, 0.07).
narrative_ontology:measurement(firs_tr_t30, first_amendment_speech_protection__absolutist_reading, theater_ratio, 30, 0.08).
narrative_ontology:measurement(firs_tr_t40, first_amendment_speech_protection__absolutist_reading, theater_ratio, 40, 0.09).
narrative_ontology:measurement(firs_tr_t50, first_amendment_speech_protection__absolutist_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(firs_be_t0, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(firs_be_t10, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(firs_be_t20, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(firs_be_t30, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 30, 0.63).
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
narrative_ontology:affects_constraint(first_amendment_speech_protection__absolutist_reading, hate_speech_regulation).
narrative_ontology:affects_constraint(first_amendment_speech_protection__absolutist_reading, online_content_moderation).

% DUAL FORMULATION NOTE:
% This is one of three readings of the 'first_amendment_speech_protection' kernel. This absolutist reading maximizes speaker protection, externalizing harm. Sibling readings (harm_limited_reading, categorical_balancing_reading) offer different balances.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
