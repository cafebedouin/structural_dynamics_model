% ============================================================================
% CONSTRAINT STORY: substance_control_legitimacy__harm_reduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   domain: public health / criminal justice / political economy
 *
 * SUMMARY:
 *   This constraint models the harm-reduction reading of the contested
 *   substance-control-legitimacy kernel: the state's authority to intervene
 *   in substance use derives from a duty to minimize harm, discharged through
 *   medicalization (treatment mandates, diversion, supervised consumption,
 *   needle exchange) rather than criminal punishment. The reading retains the
 *   possession and distribution offenses as legal backstops but recasts the
 *   primary intervention as clinical rather than punitive. This produces a
 *   hybrid structure: genuine coordination (reduced overdose mortality,
 *   reduced disease transmission, reduced incarceration for pure possession)
 *   layered with a persistent extraction mechanism (coerced treatment
 *   participation under threat of prosecution, continued black-market risk
 *   absorption by users and low-level suppliers because legal supply is never
 *   authorized). The sibling readings — prohibition (state duty to
 *   criminalize inherent harm) and legalization (state authority limited to
 *   preventing third-party harm) — are separate constraints with their own ε
 *   values and their own beneficiary/victim structures; they are not blended
 *   into this one.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_legitimacy__harm_reduction_reading, 0.48).
domain_priors:suppression_score(substance_control_legitimacy__harm_reduction_reading, 0.42).
domain_priors:theater_ratio(substance_control_legitimacy__harm_reduction_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_legitimacy__harm_reduction_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_legitimacy__harm_reduction_reading, "Harm Reduction Reading of Substance Control Legitimacy").
narrative_ontology:topic_domain(substance_control_legitimacy__harm_reduction_reading, "public health / criminal justice / political economy").

domain_priors:requires_active_enforcement(substance_control_legitimacy__harm_reduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_legitimacy__harm_reduction_reading, '9a3a721c-86bc-4355-a18f-21d9a338f6de').
narrative_ontology:cs_kernel_codification('9a3a721c-86bc-4355-a18f-21d9a338f6de', distributed).
narrative_ontology:cs_authority_grounding('9a3a721c-86bc-4355-a18f-21d9a338f6de', expertise).
narrative_ontology:cs_interpretation_layer_present('9a3a721c-86bc-4355-a18f-21d9a338f6de').
narrative_ontology:cs_reading_relation('9a3a721c-86bc-4355-a18f-21d9a338f6de', substance_control_legitimacy__prohibition_reading, coexists_with).
narrative_ontology:cs_reading_relation('9a3a721c-86bc-4355-a18f-21d9a338f6de', substance_control_legitimacy__legalization_reading, influences).
narrative_ontology:cs_axiom('9a3a721c-86bc-4355-a18f-21d9a338f6de', foundational, state_duty_is_harm_minimization_not_moral_prevention).
narrative_ontology:cs_axiom_status(state_duty_is_harm_minimization_not_moral_prevention, holdable).
narrative_ontology:cs_axiom_grounding('9a3a721c-86bc-4355-a18f-21d9a338f6de', state_duty_is_harm_minimization_not_moral_prevention, instrumental).
narrative_ontology:cs_axiom('9a3a721c-86bc-4355-a18f-21d9a338f6de', foundational, use_itself_not_only_third_party_harm_is_states_proper_concern).
narrative_ontology:cs_axiom_status(use_itself_not_only_third_party_harm_is_states_proper_concern, holdable).
narrative_ontology:cs_axiom_grounding('9a3a721c-86bc-4355-a18f-21d9a338f6de', use_itself_not_only_third_party_harm_is_states_proper_concern, conventional).
narrative_ontology:cs_reference_frame('9a3a721c-86bc-4355-a18f-21d9a338f6de', public_health_harm_minimization_mandate).
narrative_ontology:cs_drift_state('9a3a721c-86bc-4355-a18f-21d9a338f6de', contemporary_overdose_crisis_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9a3a721c-86bc-4355-a18f-21d9a338f6de', '').
narrative_ontology:cs_kernel_id(substance_control_legitimacy__harm_reduction_reading, substance_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__harm_reduction_reading, public_health_agencies).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__harm_reduction_reading, treatment_provider_industry).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__harm_reduction_reading, municipal_governments).
narrative_ontology:constraint_victim(substance_control_legitimacy__harm_reduction_reading, mandated_treatment_participants).
narrative_ontology:constraint_victim(substance_control_legitimacy__harm_reduction_reading, unhoused_drug_users).
narrative_ontology:constraint_victim(substance_control_legitimacy__harm_reduction_reading, black_market_participants).
narrative_ontology:constraint_vindicates(substance_control_legitimacy__harm_reduction_reading, state_duty_to_minimize_harm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and administers the medicalized framework: needle exchanges, supervised consumption sites, diversion-to-treatment programs, and civil commitment triggers. Sets the clinical criteria that determine whether a user is routed to treatment, monitoring, or left alone, and controls funding allocation across these programs.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, public_health_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Receives referrals and public funding contingent on the diversion pipeline continuing to route users to treatment rather than courts. Revenue scales with enrollment and mandated program duration, creating an interest in maintaining referral volume regardless of clinical necessity.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, treatment_provider_industry, beneficiary,
    organized, biographical, mobile, national).

% Adopts harm-reduction framing to reduce jail and emergency-room costs and to defuse the political liability of visible drug use, while retaining civil commitment, loitering, and public-consumption enforcement as backstops. Captures cost savings and federal/state grant funding tied to the harm-reduction label.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, municipal_governments, beneficiary,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(substance_control_legitimacy__harm_reduction_reading, municipal_governments, agenda_setter).

% Ordered into treatment programs as an alternative to prosecution, often under threat of incarceration for noncompliance. Bears program costs, disclosure obligations, and monitoring even where the underlying use causes no third-party harm. Exit means either completing an indefinite program or facing the criminal system the arrangement claims to have replaced.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, mandated_treatment_participants, payer,
    powerless, biographical, trapped, local).

% Encounters the medicalized framework primarily through outreach contact that can trigger civil commitment or loitering enforcement, without the housing or income supports that would make treatment participation viable. Bears the disruption of forced relocation, seizure of belongings during sweeps framed as public-health interventions, and cyclical re-engagement with services that do not address material precarity.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, unhoused_drug_users, payer,
    powerless, immediate, trapped, local).

% Continues to supply an unregulated market because the medicalized framework does not authorize legal supply channels, only reduces criminal penalties for possession. Absorbs adulteration and violence risk that a regulated legalization framework would eliminate; the harm-reduction reading treats this risk as an unfortunate residual rather than a structural feature of leaving supply illegal.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, black_market_participants, payer,
    moderate, biographical, constrained, regional).

% Retains statutory power to prosecute possession and distribution but is asked to defer to the diversion framework in most cases. Has limited voice in setting clinical criteria yet remains the backstop enforcement mechanism when diversion programs deem a participant noncompliant, creating friction between medical and carceral logics that the reading does not resolve.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, prosecutors_and_police, excluded,
    institutional, immediate, constrained, regional).

% Argues the harm-reduction framing preserves state control over users' bodies and choices under a medical rather than criminal label, without addressing the root cause of black-market harm: the absence of a regulated legal supply. Present in public comment periods but with little formal role in program design.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, drug_policy_reform_advocates, excluded,
    organized, generational, constrained, national).

% Evaluates outcomes of diversion and treatment-mandate programs against both criminalization and legalization baselines, publishing comparative harm and recidivism data used by all sides of the kernel contest.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, independent_researchers, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_legitimacy__harm_reduction_reading, diffuse).
narrative_ontology:fixing_cost_class(substance_control_legitimacy__harm_reduction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Reduces overdose deaths, disease transmission, and criminal-justice system load by routing substance use through medical rather than punitive channels, and by funding harm-reduction infrastructure (needle exchange, naloxone distribution, supervised consumption) that lowers acute mortality regardless of whether use stops.
% TRANSFER_FUNCTION: Moves users from criminal-justice budgets and carceral institutions into public-health and treatment-industry budgets; moves discretion over a user's conduct from courts to clinicians and case managers; moves some enforcement cost from police to outreach and monitoring staff, without eliminating the underlying possession and distribution offenses.
% ABSENT_VOICES: Legalization advocates who would remove supply-side criminality entirely are structurally absent from program design — their position would dissolve the black market this reading treats as a residual harm rather than a designed consequence of maintaining illegality. Users themselves have limited input into what counts as compliance in mandated programs.
% DISAPPEARANCE_RATIONALE: Public health agencies and municipal governments would argue the world rearranges sharply: overdose deaths and disease transmission would rise without the harm-reduction infrastructure this framing funds. Legalization advocates would argue much of what remains harmful — black market violence, adulteration, mandated-treatment coercion — is itself an artifact of this reading's refusal to legalize supply, so removing the framework and replacing it with legalization would change relatively little about the underlying harm profile while removing the coercive treatment apparatus.
% FOUNDING_PROBLEM: Twentieth-century criminalization filled jails with users, did not reduce use or overdose rates, and consumed enforcement resources disproportionate to any measurable public safety benefit; the harm reduction reading was built to address the failure of pure criminalization to reduce mortality and disease.
% FOUNDING_PROBLEM_CORROBORATION: Public health researchers outside the treatment-provider industry corroborate that overdose and disease-transmission problems remain live and that harm-reduction interventions measurably reduce acute mortality. However, independent criminologists and drug-policy researchers attest that the coercive-referral component of the founding problem — reducing incarceration — has been only partially solved, since diversion programs still funnel noncompliant participants back into the criminal system, and note this corroboration comes from outside both the treatment-provider industry and municipal government, the primary beneficiaries.
narrative_ontology:disappearance_verdict(substance_control_legitimacy__harm_reduction_reading, contested).
narrative_ontology:founding_problem_status(substance_control_legitimacy__harm_reduction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_legitimacy__harm_reduction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(substance_control_legitimacy__harm_reduction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_legitimacy__harm_reduction_reading, 0.48, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.48, rising modestly from 0.30 over the interval) reflects the treatment-mandate apparatus scaling as more jurisdictions formalize diversion programs and treatment-industry referral volume grows — genuine coordination benefit coexists with rising compliance burden on mandated participants who face indefinite program requirements as an alternative to prosecution. Suppression (0.42) is moderate: lower than pure criminalization because incarceration is reduced for simple possession, but nontrivial because civil commitment, loitering enforcement, and threat-of-prosecution backstops remain active enforcement levers. Theater ratio (0.38, rising from 0.20) captures a growing share of program activity oriented toward demonstrating harm-reduction commitment (grant compliance reporting, referral-volume metrics) rather than directly reducing mortality or disease transmission. Accessibility collapse (0.50) and resistance (0.55) reflect that legal alternatives to the medicalized framework — full legalization with regulated supply — remain visible and actively advocated, unlike a genuine mountain where alternatives have vanished.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health agencies and municipal governments sit near the beneficiary end: they set clinical criteria, capture cost savings and grant funding, and bear no personal cost from the arrangement's operation. Treatment providers are direct financial beneficiaries whose revenue scales with referral volume. Mandated participants, unhoused users, and black-market participants sit near the target end: they bear compliance costs, monitoring, coercive threat-of-prosecution, or continued supply-side risk, with limited exit — trapped or constrained rather than mobile. Prosecutors/police and legalization advocates are excluded from the clinical framework's design despite holding structural stakes in its operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The harm-reduction reading's founding problem — reducing overdose deaths and disease transmission relative to pure criminalization — remains substantially live and is corroborated by public health researchers outside the treatment-provider industry, which argues against blanket mandatrophy. However, the coercive-referral layer (treatment mandates backed by prosecution threat) increasingly serves the treatment-provider industry's referral-volume interest and the municipal government's political cost-avoidance interest independent of clinical necessity, which is the tangled-rope signature: real coordination function (harm reduction infrastructure) coexisting with asymmetric extraction (coerced participants bearing costs the underlying use may not have justified) sustained by active enforcement (civil commitment, threat of prosecution).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    medicalization_vs_criminalization_boundary,
    'Is the coercive treatment-mandate apparatus (civil commitment, threat-of-prosecution diversion) structurally distinct from criminalization, or is it criminalization relabeled with clinical vocabulary?',
    'Compare outcomes and due-process protections for mandated-treatment participants against those for prosecuted defendants under the prohibition reading: if procedural protections, appeal rights, and proportionality constraints are substantially weaker under the medical framework, the relabeling hypothesis gains support.',
    'If the mandate apparatus is functionally criminalization without due-process protections, this reading''s classification should shift toward snare for the mandated-participant seat rather than remaining tangled_rope; if protections are genuinely stronger and clinically grounded, the coordination function is more robust than the current metrics suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medicalization_vs_criminalization_boundary, conceptual, 'Whether treatment mandates are substantively different from criminal punishment or a relabeled version of it.').

omega_variable(
    black_market_residual_vs_designed,
    'Is the persistence of a violent, unregulated black market a residual harm this reading has not yet solved, or a designed consequence of refusing to authorize legal supply channels?',
    'Compare adulteration rates, violence, and price volatility in jurisdictions with regulated legal supply (cannabis, some harm-reduction pilot programs with safe supply) against jurisdictions with decriminalized possession but illegal supply.',
    'If black-market harm falls sharply under regulated supply and only modestly under decriminalization-without-legal-supply, the harm-reduction reading''s claim to minimize harm without full legalization is substantially weakened, supporting reclassification of the black_market_participants seat toward higher effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(black_market_residual_vs_designed, empirical, 'Whether persistent black-market harm under this reading is an unaddressed residual or a structural product of leaving supply illegal.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the harm-reduction reading a stable, distinct commitment, or an unstable political compromise that drifts toward either the prohibition reading (when treatment mandates harden into de facto criminalization) or the legalization reading (when supply-side restrictions are progressively relaxed)?',
    'Track jurisdiction-level policy evolution over multi-decade horizons: does the harm-reduction framework stabilize, or does it consistently drift toward one sibling reading under political pressure?',
    'If harm-reduction frameworks systematically drift toward prohibition-style enforcement over time, this reading may function as a transitional scaffold toward re-criminalization rather than a stable independent commitment; if they drift toward legalization, it functions as a scaffold toward full autonomy-based regulation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether the harm-reduction reading is a stable equilibrium or a transitional state between its sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_legitimacy__harm_reduction_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(subs_tr_t4, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 4, 0.24).
narrative_ontology:measurement(subs_tr_t8, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(subs_tr_t12, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 12, 0.31).
narrative_ontology:measurement(subs_tr_t16, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 16, 0.34).
narrative_ontology:measurement(subs_tr_t20, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 20, 0.36).
narrative_ontology:measurement(subs_tr_t24, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 24, 0.38).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(subs_be_t4, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 4, 0.34).
narrative_ontology:measurement(subs_be_t8, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 8, 0.38).
narrative_ontology:measurement(subs_be_t12, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 12, 0.42).
narrative_ontology:measurement(subs_be_t16, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 16, 0.45).
narrative_ontology:measurement(subs_be_t20, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 20, 0.47).
narrative_ontology:measurement(subs_be_t24, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 24, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(subs_su_t4, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 4, 0.37).
narrative_ontology:measurement(subs_su_t8, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 8, 0.38).
narrative_ontology:measurement(subs_su_t12, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 12, 0.4).
narrative_ontology:measurement(subs_su_t16, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 16, 0.41).
narrative_ontology:measurement(subs_su_t20, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement(subs_su_t24, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 24, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_legitimacy__harm_reduction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(substance_control_legitimacy__harm_reduction_reading, 0.12).
narrative_ontology:affects_constraint(substance_control_legitimacy__harm_reduction_reading, substance_control_legitimacy__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_legitimacy__harm_reduction_reading, substance_control_legitimacy__legalization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the substance_control_legitimacy kernel. The prohibition_reading grounds state authority in a moral duty to criminalize inherently harmful conduct (higher ε, criminal-justice enforcement, prosecuted-user victim set). The legalization_reading grounds state authority narrowly in third-party harm prevention (lowest ε for users, regulated-market beneficiary set, no treatment-mandate coercion). This harm_reduction_reading occupies a structural middle position: medicalized rather than punitive, but retaining coercive civil intervention and a persistent black market absent from the legalization reading. Each reading has its own ε, beneficiary/victim structure, and classification; they are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
