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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   This constraint represents the 'categorical balancing' reading of the
 *   First Amendment, where judicial precedent establishes categories of
 *   speech (e.g., obscenity, incitement, true threats) that receive less or
 *   no protection, determined by balancing the speech's value against its
 *   potential harm. This approach grants significant interpretive power to
 *   the judiciary, leading to a dynamic and often unpredictable landscape for
 *   free speech. The constraint is claimed as a Rope by its proponents (the
 *   judiciary), but its metrics reflect a Tangled Rope due to its substantial
 *   extraction from legal predictability and marginalized speakers, coupled
 *   with active enforcement.
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
narrative_ontology:cs_story_uid(first_amendment_speech_protection__categorical_balancing_reading, '593986ad-39e6-463e-88a4-12cee5f61d7c').
narrative_ontology:cs_kernel_codification('593986ad-39e6-463e-88a4-12cee5f61d7c', fixed_text).
narrative_ontology:cs_authority_grounding('593986ad-39e6-463e-88a4-12cee5f61d7c', lineage).
narrative_ontology:cs_interpretation_layer_present('593986ad-39e6-463e-88a4-12cee5f61d7c').
narrative_ontology:cs_reading_relation('593986ad-39e6-463e-88a4-12cee5f61d7c', first_amendment_speech_protection__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('593986ad-39e6-463e-88a4-12cee5f61d7c', first_amendment_speech_protection__harm_limited_reading, coexists_with).
narrative_ontology:cs_axiom('593986ad-39e6-463e-88a4-12cee5f61d7c', foundational, speech_value_harm_balancing_is_judicial_function).
narrative_ontology:cs_axiom_status(speech_value_harm_balancing_is_judicial_function, holdable).
narrative_ontology:cs_axiom_grounding('593986ad-39e6-463e-88a4-12cee5f61d7c', speech_value_harm_balancing_is_judicial_function, conventional).
narrative_ontology:cs_axiom('593986ad-39e6-463e-88a4-12cee5f61d7c', foundational, some_speech_categories_lack_first_amendment_protection).
narrative_ontology:cs_axiom_status(some_speech_categories_lack_first_amendment_protection, holdable).
narrative_ontology:cs_axiom_grounding('593986ad-39e6-463e-88a4-12cee5f61d7c', some_speech_categories_lack_first_amendment_protection, conventional).
narrative_ontology:cs_reference_frame('593986ad-39e6-463e-88a4-12cee5f61d7c', judicial_categorical_balancing_tradition).
narrative_ontology:cs_drift_state('593986ad-39e6-463e-88a4-12cee5f61d7c', contemporary_digital_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('593986ad-39e6-463e-88a4-12cee5f61d7c', '').
narrative_ontology:cs_kernel_id(first_amendment_speech_protection__categorical_balancing_reading, first_amendment_speech_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__categorical_balancing_reading, institutional_judiciary).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__categorical_balancing_reading, dominant_social_groups).
narrative_ontology:constraint_victim(first_amendment_speech_protection__categorical_balancing_reading, legal_predictability).
narrative_ontology:constraint_victim(first_amendment_speech_protection__categorical_balancing_reading, minority_speakers).
narrative_ontology:constraint_victim(first_amendment_speech_protection__categorical_balancing_reading, marginalized_groups).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary interpreter and enforcer of First Amendment speech categories. Benefits from maintaining interpretive control and the flexibility of case-by-case balancing, which allows adaptation to new forms of speech and harm. Their professional identity is fused with this interpretive role.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, institutional_judiciary, agenda_setter,
    institutional, generational, identity_locked, national).

% Benefit from the judiciary's balancing acts, which often align with prevailing social norms, protecting their speech while allowing for the suppression of speech deemed harmful or offensive by the majority. Their influence shapes the categories.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, dominant_social_groups, beneficiary,
    organized, biographical, mobile, national).

% Suffers from the case-by-case nature of categorical balancing. The lack of clear, ex-ante rules makes it difficult for individuals and organizations to know what speech is protected, leading to self-censorship or costly litigation. It is an abstract good, not an agent.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, legal_predictability, payer,
    powerless, immediate, trapped, national).
narrative_ontology:stakeholder_non_agent(first_amendment_speech_protection__categorical_balancing_reading, legal_predictability).

% Often find their speech falling into 'unprotected' categories or facing greater scrutiny under balancing tests, especially when their views challenge dominant norms. They bear the cost of suppression and the chilling effect of unpredictable enforcement.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, minority_speakers, payer,
    powerless, biographical, constrained, local).

% Experience the harms of speech deemed 'protected' by the balancing test, such as hate speech or incitement that falls short of 'true threats.' They are often the targets of such speech and bear the social and psychological costs, with limited recourse due to the constraint's operation.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, marginalized_groups, payer,
    powerless, generational, identity_locked, local).

% Argue for a more expansive, less categorical protection of speech based on the literal text of the First Amendment. Their arguments are often marginalized in judicial discourse, as the categorical balancing approach is deeply entrenched.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, absolutist_advocates, excluded,
    moderate, generational, constrained, national).

% Seek to prioritize the prevention of demonstrable harm over speech protection, advocating for a broader scope of unprotected speech. Their proposals often conflict with the judiciary's established balancing categories and are resisted by those who benefit from the current framework.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, harm_reduction_advocates, excluded,
    moderate, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for adjudicating conflicts between speech and other societal values (e.g., public safety, privacy, reputation) by establishing categories of protected and unprotected expression, aiming for a stable legal environment for communication.
% TRANSFER_FUNCTION: Transfers interpretive authority and flexibility to the institutional judiciary, allowing them to define and refine speech categories. It transfers the burden of unpredictability and potential suppression to speakers, particularly those outside dominant norms, and transfers the costs of harmful speech to marginalized groups.
% ABSENT_VOICES: Absolutist advocates, who would argue for minimal state intervention in speech, and harm-reduction advocates, who would prioritize demonstrable harm over speech protection, are both structurally excluded from the core judicial balancing act. Their perspectives are considered at the margins, but the framework itself resists their fundamental challenges.
% DISAPPEARANCE_RATIONALE: If the categorical balancing framework vanished, the legal landscape for speech would become chaotic. Courts would lack a coherent method for resolving speech disputes, leading to inconsistent rulings, increased litigation, and a breakdown in public understanding of speech rights. A new framework would quickly emerge to fill the void.
% FOUNDING_PROBLEM: The problem of how to reconcile the constitutional guarantee of free speech with the need to protect individuals and society from certain harms (e.g., incitement to violence, defamation, obscenity).
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars across the ideological spectrum, civil liberties organizations, and public safety advocates all attest that the tension between free speech and harm remains a live and complex problem, requiring ongoing adjudication. The specific categories and their application are contested, but the underlying problem is not.
narrative_ontology:disappearance_verdict(first_amendment_speech_protection__categorical_balancing_reading, world_rearranges).
narrative_ontology:founding_problem_status(first_amendment_speech_protection__categorical_balancing_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(first_amendment_speech_protection__categorical_balancing_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.65) because the balancing test often results in the suppression of speech that challenges dominant norms, effectively extracting the right to speak from certain groups. Suppression is also high (0.70) due to the active judicial enforcement of these categories and the chilling effect of unpredictable rulings. Theater ratio is moderate (0.40) as the 'balancing' often serves to legitimize outcomes that align with institutional preferences rather than a neutral assessment of speech value. The metrics show a trend of increasing extractiveness and suppression over time, reflecting the expansion of judicial categories and the intensification of enforcement.
 *
 * PERSPECTIVAL GAP:
 *   From the judiciary's perspective, this is a necessary and legitimate coordination mechanism for a complex society. From the perspective of minority speakers and marginalized groups, it operates as an extractive mechanism that selectively suppresses their voices and fails to protect them from harm. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The institutional judiciary is a clear beneficiary and agenda-setter, as this reading grants them significant power to define and enforce speech boundaries. Dominant social groups also benefit, as the balancing often aligns with their interests. Legal predictability, minority speakers, and marginalized groups are victims/payers, bearing the costs of uncertainty, suppression, and unmitigated harm. The 'identity_locked' exit for the judiciary reflects their professional commitment to this interpretive framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_discretion_vs_predictability,
    'Is the flexibility afforded by categorical balancing a necessary feature for adapting to evolving societal norms and new forms of speech, or does it primarily serve to maintain judicial interpretive control at the expense of legal predictability?',
    'Comparative analysis of speech regimes: examine systems with more rigid, text-based protections versus those with extensive judicial balancing. Evaluate their adaptability, consistency, and impact on diverse speakers.',
    'If flexibility is primarily about control, the constraint''s extractiveness and suppression are higher than currently measured, as the ''coordination'' function is largely a cover for power maintenance. If truly necessary for adaptation, the current metrics are more accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_discretion_vs_predictability, conceptual, 'Ambiguity regarding the true function of judicial discretion in speech categorization.').

omega_variable(
    harm_definition_objectivity,
    'Are the ''harms'' used in the balancing test objectively defined and consistently applied, or are they subjectively interpreted in ways that disproportionately affect marginalized groups or align with dominant social preferences?',
    'Empirical study of judicial outcomes: analyze cases where speech is deemed unprotected due to ''harm'' to identify patterns of application, demographic impact, and consistency across different judicial panels and time periods.',
    'If harms are subjectively applied, the constraint''s suppression and extractiveness are higher for minority speakers, and the ''coordination'' function is further undermined by its discriminatory impact. This would push the classification closer to a Snare for those seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_definition_objectivity, empirical, 'Whether harm definitions are objective or biased in application.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(first_amendment_speech_protection__categorical_balancing_reading, 1940, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(firs_tr_t1940, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 1940, 0.2).
narrative_ontology:measurement(firs_tr_t1960, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 1960, 0.25).
narrative_ontology:measurement(firs_tr_t1980, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 1980, 0.3).
narrative_ontology:measurement(firs_tr_t2000, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(firs_tr_t2024, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(firs_be_t1940, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 1940, 0.4).
narrative_ontology:measurement(firs_be_t1960, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 1960, 0.48).
narrative_ontology:measurement(firs_be_t1980, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 1980, 0.55).
narrative_ontology:measurement(firs_be_t2000, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 2000, 0.6).
narrative_ontology:measurement(firs_be_t2024, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(firs_su_t1940, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 1940, 0.5).
narrative_ontology:measurement(firs_su_t1960, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 1960, 0.58).
narrative_ontology:measurement(firs_su_t1980, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 1980, 0.65).
narrative_ontology:measurement(firs_su_t2000, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement(firs_su_t2024, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(first_amendment_speech_protection__categorical_balancing_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(first_amendment_speech_protection__categorical_balancing_reading, 0.1).
narrative_ontology:affects_constraint(first_amendment_speech_protection__categorical_balancing_reading, first_amendment_speech_protection__absolutist_reading).
narrative_ontology:affects_constraint(first_amendment_speech_protection__categorical_balancing_reading, first_amendment_speech_protection__harm_limited_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the First Amendment speech protection kernel. Its categorical balancing approach directly influences the operational space and perceived legitimacy of both absolutist and harm-limited readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
