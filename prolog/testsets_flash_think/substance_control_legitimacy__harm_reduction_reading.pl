% ============================================================================
% CONSTRAINT STORY: substance_control_legitimacy__harm_reduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_legitimacy__harm_reduction_reading, []).

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
 *   constraint_id: substance_control_legitimacy__harm_reduction_reading
 *   human_readable: Substance Control Legitimacy (Harm Reduction Reading)
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This story models the 'harm reduction' reading of substance control
 *   legitimacy, where state authority is framed as a duty to minimize public
 *   health harms without resorting to criminalization. This reading shifts
 *   the focus from moral condemnation to medicalization and public health
 *   interventions, but often involves coercive treatment mandates and still
 *   contends with a persistent black market. The claimed type is 'Tangled
 *   Rope' because it genuinely coordinates public health efforts but also
 *   extracts autonomy and resources through mandates and ongoing enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_legitimacy__harm_reduction_reading, 0.6).
domain_priors:suppression_score(substance_control_legitimacy__harm_reduction_reading, 0.7).
domain_priors:theater_ratio(substance_control_legitimacy__harm_reduction_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_legitimacy__harm_reduction_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_legitimacy__harm_reduction_reading, "Substance Control Legitimacy (Harm Reduction Reading)").
narrative_ontology:topic_domain(substance_control_legitimacy__harm_reduction_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_legitimacy__harm_reduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_legitimacy__harm_reduction_reading, '5397cd05-a891-48c6-af46-9532b3b6252f').
narrative_ontology:cs_kernel_codification('5397cd05-a891-48c6-af46-9532b3b6252f', formalized).
narrative_ontology:cs_authority_grounding('5397cd05-a891-48c6-af46-9532b3b6252f', lineage).
narrative_ontology:cs_interpretation_layer_present('5397cd05-a891-48c6-af46-9532b3b6252f').
narrative_ontology:cs_reading_relation('5397cd05-a891-48c6-af46-9532b3b6252f', substance_control_legitimacy__prohibition_reading, influences).
narrative_ontology:cs_reading_relation('5397cd05-a891-48c6-af46-9532b3b6252f', substance_control_legitimacy__legalization_reading, coexists_with).
narrative_ontology:cs_axiom('5397cd05-a891-48c6-af46-9532b3b6252f', foundational, public_health_primary_goal).
narrative_ontology:cs_axiom_status(public_health_primary_goal, holdable).
narrative_ontology:cs_axiom_grounding('5397cd05-a891-48c6-af46-9532b3b6252f', public_health_primary_goal, instrumental).
narrative_ontology:cs_axiom('5397cd05-a891-48c6-af46-9532b3b6252f', foundational, non_criminalization_principle).
narrative_ontology:cs_axiom_status(non_criminalization_principle, holdable).
narrative_ontology:cs_axiom_grounding('5397cd05-a891-48c6-af46-9532b3b6252f', non_criminalization_principle, deontological).
narrative_ontology:cs_reference_frame('5397cd05-a891-48c6-af46-9532b3b6252f', public_health_first_principle).
narrative_ontology:cs_drift_state('5397cd05-a891-48c6-af46-9532b3b6252f', contemporary_policy_debate, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('5397cd05-a891-48c6-af46-9532b3b6252f', '').
narrative_ontology:cs_kernel_id(substance_control_legitimacy__harm_reduction_reading, substance_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__harm_reduction_reading, public_health_agencies).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__harm_reduction_reading, treatment_providers).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__harm_reduction_reading, some_substance_users).
narrative_ontology:constraint_victim(substance_control_legitimacy__harm_reduction_reading, substance_users_under_mandate).
narrative_ontology:constraint_victim(substance_control_legitimacy__harm_reduction_reading, black_market_participants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and implement harm reduction policies, manage treatment programs, and receive public funding. They frame substance use as a health issue requiring state intervention to minimize collective harm.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, public_health_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Deliver medical and therapeutic services to substance users, often receiving public funding or insurance payments. They operate within the policy framework set by public health agencies.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, treatment_providers, beneficiary,
    organized, biographical, constrained, local).

% Are diverted from criminalization but subjected to mandatory treatment, supervision, or other coercive public health interventions. They experience a loss of autonomy and bear the costs of compliance.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, substance_users_under_mandate, payer,
    powerless, immediate, constrained, local).

% Continue to operate due to unmet demand for unregulated substances, facing ongoing enforcement efforts and bearing the risks of illicit trade. They are a persistent byproduct of the harm reduction framework's limits.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, black_market_participants, payer,
    powerless, immediate, trapped, local).

% Argue for stricter criminalization and moral condemnation of substance use, viewing harm reduction as a compromise that normalizes harmful behavior. Their perspective is marginalized in this policy framework.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, prohibition_advocates, excluded,
    organized, generational, identity_locked, national).

% Argue for full adult autonomy over substance use and regulated legal markets, viewing harm reduction as an insufficient and paternalistic approach. Their perspective is also marginalized in this framework.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, legalization_advocates, excluded,
    organized, generational, mobile, national).

% Responsible for enacting and funding public health policies. They navigate the political landscape, balancing public health goals with public opinion and the demands of various advocacy groups.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, policymakers, agenda_setter,
    institutional, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_legitimacy__harm_reduction_reading, public_health_agencies).
narrative_ontology:fixing_cost_class(substance_control_legitimacy__harm_reduction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a public health response to substance use, aiming to reduce overdose deaths, disease transmission, and social harms by providing services and diverting users from the criminal justice system.
% TRANSFER_FUNCTION: Transfers resources (funding, personnel) to public health and treatment systems; transfers autonomy from substance users to the state via mandates; transfers risk from society to black market participants.
% ABSENT_VOICES: Full legalization advocates would argue for individual autonomy and regulated markets; strict prohibition advocates would argue for moral condemnation and criminal justice solutions. Both are structurally marginalized in the harm reduction framework.
% DISAPPEARANCE_RATIONALE: If the harm reduction framework vanished, the system would likely revert to either a more punitive prohibitionist approach (increasing criminalization and associated harms) or a more liberal legalization approach (requiring new regulatory structures), fundamentally altering the state's relationship to substance use.
% FOUNDING_PROBLEM: The failure of prohibition to eliminate substance use, leading to massive social harms, an overburdened criminal justice system, and preventable deaths.
% FOUNDING_PROBLEM_CORROBORATION: Public health data on overdose rates, disease transmission, and criminal justice system costs, as well as testimony from medical professionals and social workers, corroborate the ongoing nature of these problems.
narrative_ontology:disappearance_verdict(substance_control_legitimacy__harm_reduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_legitimacy__harm_reduction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_legitimacy__harm_reduction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(substance_control_legitimacy__harm_reduction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_legitimacy__harm_reduction_reading, 0.6, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_legitimacy__harm_reduction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_legitimacy__harm_reduction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substance_control_legitimacy__harm_reduction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The `extractiveness` is moderate because while criminal penalties are reduced, users are still subject to mandates, fines, and loss of autonomy, and the black market continues to impose costs. `suppression` is moderate as the state actively enforces treatment compliance and manages the illicit market. `theater_ratio` is low to moderate, reflecting genuine public health efforts alongside some performative aspects in justifying state control over individual choices. `accessibility_collapse` is moderate as alternatives to criminalization are offered, but full, unregulated access is not. `resistance` comes from both sides of the policy debate.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of public health agencies, this is a necessary and beneficial coordination mechanism. From the perspective of substance users under mandate, it can still feel highly coercive, even if preferable to criminalization. Legalization advocates see it as insufficient, while prohibition advocates see it as a dangerous compromise. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health agencies and treatment providers are beneficiaries, gaining resources and authority. Substance users under mandate and black market participants are targets, bearing costs and losing autonomy. Policymakers act as agenda-setters, navigating the complex landscape.
 *
 * MANDATROPHY ANALYSIS:
 *   The harm reduction framework directly addresses the failures of prohibition, so its mandate is live. The classification as Tangled Rope reflects that while it solves a genuine public health coordination problem, it also involves asymmetric extraction and requires active enforcement, preventing it from being mislabeled as a pure Rope or a Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    harm_reduction_vs_legalization_boundary,
    'Is the distinction between harm reduction and full legalization a matter of degree or a fundamental difference in principle (public health paternalism vs. individual autonomy)?',
    'Analysis of policy outcomes in jurisdictions with varying degrees of liberalization, focusing on the balance between public health metrics and individual liberty metrics.',
    'If a fundamental difference, the two readings represent distinct normative frameworks; if a matter of degree, they might converge under certain conditions, suggesting a shared underlying principle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_reduction_vs_legalization_boundary, conceptual, 'Conceptual boundary between harm reduction and legalization.').

omega_variable(
    black_market_persistence_drivers,
    'To what extent does the persistence of the black market reflect the limits of harm reduction policy (e.g., continued criminalization of certain substances, lack of full access to regulated alternatives) versus inherent market dynamics?',
    'Comparative studies of black market activity in jurisdictions with different harm reduction policies and varying levels of legal access to substances.',
    'If policy limits are the primary driver, further liberalization within a harm reduction framework could reduce extraction; if inherent market dynamics, the black market may persist regardless of policy shifts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(black_market_persistence_drivers, empirical, 'Drivers of black market persistence under harm reduction.').

omega_variable(
    mandate_coercion_threshold,
    'At what point do treatment mandates and other coercive public health interventions become functionally equivalent to criminalization in terms of individual liberty, even if framed differently?',
    'Qualitative research on the lived experience of individuals under such mandates, combined with legal analysis of the scope of state power and due process protections.',
    'If the functional equivalence is high, the ''non-criminalization'' aspect of harm reduction may be largely theatrical, increasing effective extraction for affected individuals.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mandate_coercion_threshold, conceptual, 'Threshold of coercion in public health mandates.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_legitimacy__harm_reduction_reading, 1980, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(subs_tr_t9, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 9, 0.15).
narrative_ontology:measurement(subs_tr_t18, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 18, 0.2).
narrative_ontology:measurement(subs_tr_t27, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 27, 0.23).
narrative_ontology:measurement(subs_tr_t45, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 45, 0.25).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(subs_be_t9, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 9, 0.5).
narrative_ontology:measurement(subs_be_t18, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 18, 0.55).
narrative_ontology:measurement(subs_be_t27, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 27, 0.58).
narrative_ontology:measurement(subs_be_t45, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 45, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(subs_su_t9, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 9, 0.58).
narrative_ontology:measurement(subs_su_t18, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 18, 0.65).
narrative_ontology:measurement(subs_su_t27, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 27, 0.68).
narrative_ontology:measurement(subs_su_t45, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 45, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_legitimacy__harm_reduction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(substance_control_legitimacy__harm_reduction_reading, substance_control_legitimacy__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_legitimacy__harm_reduction_reading, substance_control_legitimacy__legalization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'substance_control_legitimacy' kernel, alongside prohibition and legalization readings. Each represents a distinct structural claim about state authority over substance use.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
