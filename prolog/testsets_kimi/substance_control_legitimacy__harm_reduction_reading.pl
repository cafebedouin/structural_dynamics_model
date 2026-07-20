% ============================================================================
% CONSTRAINT STORY: substance_control_legitimacy__harm_reduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   human_readable: Harm Reduction Reading of Substance Control Legitimacy
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This is the harm_reduction_reading of the substance_control_legitimacy
 *   kernel. It asserts that substance use is a public health issue and that
 *   state authority flows from a duty to minimize harm without criminalizing
 *   users. In practice, this produces a medicalized regime: users are
 *   diverted to treatment, providers receive mandated referrals, and a
 *   persistent black market continues to supply substances outside the
 *   clinical framework. The constraint is structurally distinct from the
 *   prohibition_reading (which demands criminalization) and the
 *   legalization_reading (which demands autonomy and regulated markets).
 *
 * KEY AGENTS:
 *   - public_health_authority: agenda-setter (institutional/constrained) â administers treatment mandates and derives budget and authority from the harm reduction frame
 *   - substance_users: primary target (powerless/constrained) â bear the cost of mandated treatment and loss of autonomous choice
 *   - treatment_industry: beneficiary (organized/constrained) â captures revenue from publicly funded, compulsory referrals
 *   - medical_establishment: beneficiary (institutional/constrained) â gains jurisdictional expansion into formerly criminalized behavior
 *   - black_market_operators: excluded (moderate/trapped) â persist outside the medical framework and remain targeted by enforcement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_legitimacy__harm_reduction_reading, 0.58).
domain_priors:suppression_score(substance_control_legitimacy__harm_reduction_reading, 0.62).
domain_priors:theater_ratio(substance_control_legitimacy__harm_reduction_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_legitimacy__harm_reduction_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_legitimacy__harm_reduction_reading, "Harm Reduction Reading of Substance Control Legitimacy").
narrative_ontology:topic_domain(substance_control_legitimacy__harm_reduction_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_legitimacy__harm_reduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_legitimacy__harm_reduction_reading, '69958655-9677-44b4-9d47-b0f6db60f4b0').
narrative_ontology:cs_kernel_codification('69958655-9677-44b4-9d47-b0f6db60f4b0', formalized).
narrative_ontology:cs_authority_grounding('69958655-9677-44b4-9d47-b0f6db60f4b0', expertise).
narrative_ontology:cs_interpretation_layer_present('69958655-9677-44b4-9d47-b0f6db60f4b0').
narrative_ontology:cs_reading_relation('69958655-9677-44b4-9d47-b0f6db60f4b0', substance_control_legitimacy__prohibition_reading, forecloses).
narrative_ontology:cs_reading_relation('69958655-9677-44b4-9d47-b0f6db60f4b0', substance_control_legitimacy__legalization_reading, coexists_with).
narrative_ontology:cs_axiom('69958655-9677-44b4-9d47-b0f6db60f4b0', foundational, state_duty_minimize_harm_without_criminalization).
narrative_ontology:cs_axiom_status(state_duty_minimize_harm_without_criminalization, holdable).
narrative_ontology:cs_axiom_grounding('69958655-9677-44b4-9d47-b0f6db60f4b0', state_duty_minimize_harm_without_criminalization, deontological).
narrative_ontology:cs_axiom('69958655-9677-44b4-9d47-b0f6db60f4b0', foundational, medical_gatekeeping_over_substance_use).
narrative_ontology:cs_axiom_status(medical_gatekeeping_over_substance_use, holdable).
narrative_ontology:cs_axiom_grounding('69958655-9677-44b4-9d47-b0f6db60f4b0', medical_gatekeeping_over_substance_use, empirically_contingent).
narrative_ontology:cs_reference_frame('69958655-9677-44b4-9d47-b0f6db60f4b0', clinical_governance_authority).
narrative_ontology:cs_drift_state('69958655-9677-44b4-9d47-b0f6db60f4b0', contemporary_policy_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('69958655-9677-44b4-9d47-b0f6db60f4b0', '').
narrative_ontology:cs_kernel_id(substance_control_legitimacy__harm_reduction_reading, substance_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__harm_reduction_reading, public_health_authority).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__harm_reduction_reading, treatment_industry).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__harm_reduction_reading, medical_establishment).
narrative_ontology:constraint_victim(substance_control_legitimacy__harm_reduction_reading, substance_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers harm reduction policy, sets treatment mandate protocols, and channels public funds to licensed providers. Derives institutional authority from a duty-of-care framing that substitutes clinical management for criminal prosecution. Cannot easily abandon the framework without surrendering a major policy domain and budget line.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, public_health_authority, agenda_setter,
    institutional, generational, constrained, national).

% Diverted from criminal courts into mandated treatment and medical supervision. Shielded from criminal records but subjected to compulsory referrals, clinical monitoring, and loss of autonomous decision-making over substance use. The black market remains accessible but carries heightened risk and no quality control.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, substance_users, payer,
    powerless, immediate, constrained, local).

% Receives publicly funded referrals and a stream of mandated clients whose participation is legally or administratively compulsory. Revenue depends on the state maintaining medical gatekeeping over substance use and diverting users from criminal justice into clinical programs.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, treatment_industry, beneficiary,
    organized, biographical, constrained, national).

% Gains professional jurisdiction over a domain formerly governed by police and courts. Physicians and clinicians diagnose, classify, and manage substance use disorders, expanding the scope of medical authority into behavior previously subject to criminal sanction.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, medical_establishment, beneficiary,
    institutional, generational, constrained, national).

% Continue supplying substances outside the medical framework. Excluded from policy design and legitimacy; targeted by enforcement despite the public health framing of user behavior. Persistent because the medicalized supply does not meet demand or is inaccessible.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, black_market_operators, excluded,
    moderate, immediate, trapped, regional).

% Argue that substance use should remain criminalized. Sidelined by the harm reduction consensus but continue to contest the legitimacy of non-criminalization in legislative and media arenas.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, prohibition_advocates, excluded,
    organized, biographical, mobile, national).

% Argue for full adult autonomy over substance use and regulated commercial supply. Oppose mandatory treatment and medical gatekeeping as substituting one coercive frame for another.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, legalization_advocates, excluded,
    organized, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_legitimacy__harm_reduction_reading, treatment_industry).
narrative_ontology:fixing_cost_class(substance_control_legitimacy__harm_reduction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Medicalizing substance use to reduce infectious disease transmission, overdose mortality, and criminal justice entanglement by substituting clinical engagement for carceral processing of users.
% TRANSFER_FUNCTION: Moves substance users from criminal courts into mandated treatment programs; moves public funds to treatment providers and health agencies; moves autonomy and decision-making capacity from users to medical gatekeepers.
% ABSENT_VOICES: Black market operators remain criminalized and excluded from legitimacy; legalization advocates reject medical coercion; prohibition advocates reject non-criminalization. None are in the room when harm reduction policy is designed.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, users would revert to criminalization or move toward unregulated markets; treatment provider revenue models would collapse; public health agencies would lose a major policy domain and budget stream; the institutional balance between health and criminal justice would reorganize rapidly.
% FOUNDING_PROBLEM: Substance use was producing preventable deaths, disease outbreaks, and mass incarceration under a purely criminalizing regime; a health-based alternative was needed to break the cycle.
% FOUNDING_PROBLEM_CORROBORATION: Epidemiologists outside the treatment industry attest to reduced overdose mortality under medicalization; civil liberties organizations outside the beneficiary set attest that mass incarceration persists but the current arrangement introduces new harms through coerced treatment and medical social control.
narrative_ontology:disappearance_verdict(substance_control_legitimacy__harm_reduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_legitimacy__harm_reduction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_legitimacy__harm_reduction_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(substance_control_legitimacy__harm_reduction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_legitimacy__harm_reduction_reading, 0.58, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is moderate (0.58) because the constraint diverts public money to providers and extracts autonomy from users via treatment mandates, while still delivering genuine health benefits. Suppression is moderate-high (0.62) because the arrangement requires active enforcement to suppress the non-medical supply and to compel participation in mandated programs. Theater ratio (0.35) captures the growing gap between public health rhetoric and the coercive reality of clinical compliance regimes. Resistance (0.52) reflects active opposition from legalization advocates, prohibitionists, and users avoiding mandates.
 *
 * PERSPECTIVAL GAP:
 *   The public health authority and medical establishment experience the constraint as legitimate coordination saving lives; substance users experience it as state coercion routed through clinics rather than courts. The engine computes this divergence from the structural data: the same policy produces low directionality for the administering institutions and high directionality for the mandated population.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authority and treatment industry sit near the beneficiary end: they collect budget, authority, and revenue from the constraint. Substance_users sit near the target end: they bear the costs of mandated treatment, clinical surveillance, and exclusion from autonomous choice. Black market operators are excluded from the coordination story entirely and remain targets of enforcement, placing them at the high-d extreme.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as tangled rope prevents the mislabeling that would occur if it were read as pure rope (ignoring the coercion embedded in treatment mandates and the persistent black market) or pure snare (ignoring the genuine reduction in overdose and disease that medicalization achieves). The active enforcement requirement, the identifiable victims among mandated users, and the beneficiary capture by treatment providers together satisfy the tangled rope gate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mandate_vs_autonomy,
    'Is mandatory treatment a necessary coordination mechanism to engage hard-to-reach populations, or is it extractive coercion dressed in clinical language?',
    'Compare mortality, retention, and self-reported autonomy outcomes between voluntary harm reduction programs and mandate-based programs.',
    'If mandates show no added health benefit, the coordination story weakens and the constraint shifts toward snare-like extraction; if they show significant mortality reduction, the tangled rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_vs_autonomy, empirical, 'Whether treatment mandates are coordination or extraction').

omega_variable(
    black_market_persistence,
    'Does the persistent black market represent a failure of the harm reduction reading, or an unavoidable residual under any regime?',
    'Compare black market size and harms across jurisdictions with full legalization, harm reduction, and prohibition frameworks.',
    'If the black market persists primarily because medical gatekeeping limits legal supply, the constraint creates its own victims through artificial scarcity; if it persists regardless of regime, the extraction is less attributable to this specific constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(black_market_persistence, empirical, 'Whether black market persistence is endogenous to harm reduction policy').

omega_variable(
    kernel_reading_separation,
    'Is the medicalization of substance use a stable distinct constraint, or merely an intermediate stage toward full legalization?',
    'Examine whether medical gatekeeping (mandated treatment, prescription controls) is structurally separable from decriminalization; if the two functions cannot be disentangled in practice, the reading may collapse toward legalization.',
    'If medical gatekeeping and decriminalization are inseparable, this reading maintains a stable distinct epsilon; if separable, the constraint may dissolve into either legalization or prohibition depending on political trajectory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_separation, conceptual, 'Whether harm reduction is a stable distinct reading or transitional').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_legitimacy__harm_reduction_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(subs_tr_t8, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 8, 0.24).
narrative_ontology:measurement(subs_tr_t16, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 16, 0.28).
narrative_ontology:measurement(subs_tr_t24, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 24, 0.31).
narrative_ontology:measurement(subs_tr_t32, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 32, 0.33).
narrative_ontology:measurement(subs_tr_t40, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 40, 0.35).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(subs_be_t8, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 8, 0.4).
narrative_ontology:measurement(subs_be_t16, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 16, 0.46).
narrative_ontology:measurement(subs_be_t24, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 24, 0.51).
narrative_ontology:measurement(subs_be_t32, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 32, 0.55).
narrative_ontology:measurement(subs_be_t40, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(subs_su_t8, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 8, 0.5).
narrative_ontology:measurement(subs_su_t16, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 16, 0.55).
narrative_ontology:measurement(subs_su_t24, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 24, 0.58).
narrative_ontology:measurement(subs_su_t32, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 32, 0.6).
narrative_ontology:measurement(subs_su_t40, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_legitimacy__harm_reduction_reading, resource_allocation).
narrative_ontology:affects_constraint(substance_control_legitimacy__harm_reduction_reading, prohibition_reading).
narrative_ontology:affects_constraint(substance_control_legitimacy__harm_reduction_reading, legalization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the substance_control_legitimacy kernel. It is structurally distinct from prohibition_reading (which criminalizes users) and legalization_reading (which grants autonomy). Each reading carries a separate epsilon, stakeholder set, and classification, linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
