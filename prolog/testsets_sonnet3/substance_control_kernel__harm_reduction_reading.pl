% ============================================================================
% CONSTRAINT STORY: substance_control_kernel__harm_reduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_kernel__harm_reduction_reading, []).

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
 *   constraint_id: substance_control_kernel__harm_reduction_reading
 *   human_readable: Harm Reduction Reading of Substance Control: Health Condition Requiring Pragmatic Intervention
 *   domain: public_health/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This story instantiates the harm reduction reading of the substance
 *   control kernel: substance use is a health condition requiring pragmatic
 *   intervention (needle exchanges, supervised consumption,
 *   medication-assisted treatment, naloxone distribution) to reduce overdose
 *   death and disease transmission, independent of whether use itself ceases.
 *   Under this reading, users exit the criminal victim set that the
 *   prohibition reading creates, but the reading does not decriminalize
 *   supply — the state pivots from punishing users to managing them as
 *   patients while continuing to criminalize the people who supply them. This
 *   produces a hybrid structure: genuine coordination (measurably reduced
 *   overdose mortality and infectious disease transmission) layered on
 *   continued extraction from a relocated victim class (supply-chain workers,
 *   unregulated-market participants) and continued paternalistic constraint
 *   on users themselves, who trade criminal liability for medicalized
 *   surveillance.
 *
 * KEY AGENTS:
 *   - people_who_use_drugs: primary beneficiary of reduced criminalization and improved survival services, but also payer of paternalistic health-system constraint and continued unregulated-supply risk
 *   - public_health_agencies: agenda_setter administering the reframe, institutionally invested in the health-condition framing persisting
 *   - harm_reduction_service_providers: beneficiary whose funding and legitimacy depend on this specific reading's dominance over both prohibition and legalization
 *   - supply_chain_workers: payer excluded from the health reframe entirely, bearing displaced criminalization
 *   - law_enforcement_agencies: agenda_setter/beneficiary that retains mandate by redirecting from use-enforcement to supply-enforcement
 *   - prohibition_advocates and legalization_advocates: excluded voices whose competing kernel readings are structurally sidelined by the professional health-policy consensus
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_kernel__harm_reduction_reading, 0.42).
domain_priors:suppression_score(substance_control_kernel__harm_reduction_reading, 0.38).
domain_priors:theater_ratio(substance_control_kernel__harm_reduction_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_kernel__harm_reduction_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_kernel__harm_reduction_reading, "Harm Reduction Reading of Substance Control: Health Condition Requiring Pragmatic Intervention").
narrative_ontology:topic_domain(substance_control_kernel__harm_reduction_reading, "public_health/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_kernel__harm_reduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_kernel__harm_reduction_reading, '7b26bc97-2a50-40e8-ae80-c8419a5c2ac9').
narrative_ontology:cs_kernel_codification('7b26bc97-2a50-40e8-ae80-c8419a5c2ac9', distributed).
narrative_ontology:cs_authority_grounding('7b26bc97-2a50-40e8-ae80-c8419a5c2ac9', expertise).
narrative_ontology:cs_interpretation_layer_present('7b26bc97-2a50-40e8-ae80-c8419a5c2ac9').
narrative_ontology:cs_reading_relation('7b26bc97-2a50-40e8-ae80-c8419a5c2ac9', substance_control_kernel__prohibition_reading, coexists_with).
narrative_ontology:cs_reading_relation('7b26bc97-2a50-40e8-ae80-c8419a5c2ac9', substance_control_kernel__legalization_reading, influences).
narrative_ontology:cs_axiom('7b26bc97-2a50-40e8-ae80-c8419a5c2ac9', foundational, substance_use_is_health_condition_not_moral_failure).
narrative_ontology:cs_axiom_status(substance_use_is_health_condition_not_moral_failure, holdable).
narrative_ontology:cs_axiom_grounding('7b26bc97-2a50-40e8-ae80-c8419a5c2ac9', substance_use_is_health_condition_not_moral_failure, empirically_contingent).
narrative_ontology:cs_axiom('7b26bc97-2a50-40e8-ae80-c8419a5c2ac9', foundational, harm_reduction_independent_of_cessation_goal).
narrative_ontology:cs_axiom_status(harm_reduction_independent_of_cessation_goal, holdable).
narrative_ontology:cs_axiom_grounding('7b26bc97-2a50-40e8-ae80-c8419a5c2ac9', harm_reduction_independent_of_cessation_goal, instrumental).
narrative_ontology:cs_reference_frame('7b26bc97-2a50-40e8-ae80-c8419a5c2ac9', public_health_medicalization_consensus).
narrative_ontology:cs_drift_state('7b26bc97-2a50-40e8-ae80-c8419a5c2ac9', post_overdose_epidemic_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('7b26bc97-2a50-40e8-ae80-c8419a5c2ac9', '').
narrative_ontology:cs_kernel_id(substance_control_kernel__harm_reduction_reading, substance_control_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_kernel__harm_reduction_reading, people_who_use_drugs).
narrative_ontology:constraint_beneficiary(substance_control_kernel__harm_reduction_reading, public_health_agencies).
narrative_ontology:constraint_beneficiary(substance_control_kernel__harm_reduction_reading, harm_reduction_service_providers).
narrative_ontology:constraint_beneficiary(substance_control_kernel__harm_reduction_reading, general_public_via_disease_containment).
narrative_ontology:constraint_victim(substance_control_kernel__harm_reduction_reading, supply_chain_workers).
narrative_ontology:constraint_victim(substance_control_kernel__harm_reduction_reading, unregulated_drug_market_participants).
narrative_ontology:constraint_victim(substance_control_kernel__harm_reduction_reading, people_who_use_drugs).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(substance_control_kernel__harm_reduction_reading, law_enforcement_agencies).
narrative_ontology:constraint_vindicates(substance_control_kernel__harm_reduction_reading, addiction_as_chronic_health_condition_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exit the criminal victim set that prohibition creates — no longer arrested for possession or use in covered jurisdictions — and gain access to needle exchanges, supervised consumption sites, naloxone, and drug-checking services that reduce overdose and disease transmission risk. In exchange, they remain subject to mandatory-adjacent case management, referral pressure toward treatment, surveillance by service intake systems, and a paternalistic framing that defines them as patients rather than autonomous agents. Their supply remains illegal, so they still buy from an unregulated, contaminated, violence-adjacent market even while the state helps them survive using it.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, people_who_use_drugs, beneficiary,
    powerless, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(substance_control_kernel__harm_reduction_reading, people_who_use_drugs, payer).

% Administer the harm reduction apparatus — funding exchanges, training outreach workers, setting clinical protocols for medication-assisted treatment, tracking overdose and infection data. They gain expanded mandate, budget, and institutional legitimacy from reframing substance use as a chronic health condition; their continued relevance depends on the problem remaining framed as ongoing management rather than resolved cure or decriminalized non-issue.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, public_health_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Nonprofits and clinics that deliver needle exchange, naloxone distribution, supervised consumption, and outreach. They receive grant funding and professional standing tied directly to this reading's dominance; a shift toward full legalization (which would reduce overdose risk via regulated supply) or toward strict prohibition (which would criminalize their clients again) both threaten their operating model and funding base.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, harm_reduction_service_providers, beneficiary,
    organized, biographical, mobile, regional).

% Low-level couriers, growers, and street-level sellers remain fully criminalized under this reading — the ε_mod explicitly excludes them from the health reframe. They face arrest, incarceration, and violence enforcing an illegal supply chain that harm reduction services depend on for continued relevance (someone must still supply the drugs users are helped to use more safely). Exit requires abandoning livelihood with few alternatives in the same economic stratum.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, supply_chain_workers, payer,
    powerless, immediate, trapped, regional).

% Both suppliers and consumers who interact with the unregulated market bear contamination risk (fentanyl adulteration, unknown potency), violence risk from black-market enforcement of debts and territory, and the absence of quality or dosage guarantees. Drug-checking services mitigate but do not eliminate this because the underlying supply stays criminalized rather than regulated.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, unregulated_drug_market_participants, payer,
    powerless, immediate, trapped, regional).

% Enforcement against use itself recedes under this reading, but supply-side enforcement (trafficking, distribution, cultivation) is preserved or even reallocated resources from use-enforcement. Agencies retain budget and mandate by pivoting toward supply interdiction rather than facing full defunding under a legalization reading that would eliminate most drug enforcement entirely.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, law_enforcement_agencies, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(substance_control_kernel__harm_reduction_reading, law_enforcement_agencies, beneficiary).

% Moral-order and social-conservative constituencies who view drug use itself as transgression requiring punishment are structurally sidelined by the health reframe — their voice is treated as regressive stigma rather than a legitimate competing account, even though the reading does not resolve their underlying claim about social order, only displaces it from official policy discourse.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, prohibition_advocates, excluded,
    organized, generational, mobile, national).

% Advocates for treating substance use as an individual liberty matter with regulated commercial supply argue that harm reduction, by leaving the supply chain criminalized, preserves the exact contamination and violence risks that regulated legalization would eliminate. Their critique is acknowledged in policy debate but rarely acted on because the health-provider institutional apparatus has no equivalent stake in full legalization.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, legalization_advocates, excluded,
    organized, generational, mobile, national).

% Benefits diffusely from reduced HIV/hepatitis transmission and lower overdose mortality rates achieved through needle exchange and naloxone access, and from reduced strain on emergency services and correctional systems compared to pure prohibition — without directly participating in the arrangement.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, general_public_via_disease_containment, beneficiary,
    moderate, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_kernel__harm_reduction_reading, public_health_agencies).
narrative_ontology:fixing_cost_class(substance_control_kernel__harm_reduction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a public health response to substance use that reduces overdose deaths and infectious disease transmission by meeting users where they are, without requiring cessation as a precondition for care — solving the genuine collective problem of preventable death and disease spread from unsafe use practices.
% TRANSFER_FUNCTION: Moves state resources (public health funding, service infrastructure) toward users in the form of clean supplies, supervised sites, and medical treatment; simultaneously keeps enforcement resources directed at supply-chain participants rather than users, transferring criminal-justice burden from the demand side to the supply side of the same market.
% ABSENT_VOICES: Legalization advocates, who argue the reading's continued criminalization of supply preserves avoidable contamination and violence risk, and prohibition advocates, who argue the reading abandons deterrence and moral accountability — both are treated as outside the professional health-policy consensus that authors and administers this reading.
% DISAPPEARANCE_RATIONALE: If this reading were abandoned overnight in favor of strict prohibition, users would re-enter the criminal victim set, overdose and disease-transmission rates would likely rise as service access collapsed, and public health agencies would lose mandate and funding. If abandoned in favor of full legalization, the supply chain would exit criminalization entirely, potentially reducing contamination risk further but eliminating much of the current service-provider institutional apparatus. Either direction substantially reorganizes who is criminalized, who is funded, and what gets measured as success.
% FOUNDING_PROBLEM: Rising overdose deaths and HIV/hepatitis transmission among people who inject drugs, occurring under strict prohibition regimes that treated any harm-reducing intervention (clean needles, safe consumption spaces) as facilitating illegal conduct rather than preventing death — the founding problem was mass preventable mortality and disease spread that pure criminalization was failing to reduce and arguably worsening.
% FOUNDING_PROBLEM_CORROBORATION: Independent epidemiological research (outside both public health agencies and harm reduction providers) continues to document overdose mortality and transmission rates as ongoing, unresolved public health crises, corroborating that the founding problem remains live. However, legalization advocates and some independent drug-policy researchers attest that the founding problem is only partially addressed by this reading because criminalized, contaminated supply remains the primary driver of overdose deaths — a driver this reading does not resolve, only manages.
narrative_ontology:disappearance_verdict(substance_control_kernel__harm_reduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_kernel__harm_reduction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_kernel__harm_reduction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(substance_control_kernel__harm_reduction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_kernel__harm_reduction_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_kernel__harm_reduction_reading_tests).
:- end_tests(substance_control_kernel__harm_reduction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts moderate-high (0.55) reflecting the early period when harm reduction programs still operate under significant residual criminalization friction, and declines to 0.42 as programs mature, decriminalization of use-related conduct expands, and services normalize — but it does not fall further because supply-side criminalization remains structurally intact throughout, keeping a floor of extraction on supply-chain participants and unregulated-market risk for users. Suppression declines correspondingly (0.62 to 0.38) as use-focused enforcement recedes, but does not approach zero because paternalistic case-management pressure and continued illegality of possession in many jurisdictions persist as softer suppression mechanisms. Theater ratio rises modestly (0.12 to 0.22) as programs mature and some service delivery becomes more about institutional demonstration (grant reporting, political cover) than pure function, without yet reaching piton-level performativity.
 *
 * PERSPECTIVAL GAP:
 *   From the public health agency and service-provider seats, this reading is coordination: a genuine, evidence-based reduction in preventable death and disease. From the supply-chain worker seat, the same kernel produces continued full-force criminal extraction with no offsetting benefit — the reading explicitly declines to extend its coordination gains to them. From the user seat, the experience is genuinely mixed: real relief from criminal liability and real access to survival services, combined with a persistent paternalistic framing that treats them as patients rather than rights-bearing agents and leaves them purchasing from a contaminated, unregulated supply the reading does nothing to fix.
 *
 * DIRECTIONALITY LOGIC:
 *   People who use drugs are coded as both beneficiary and payer (dual role) because the reading genuinely reduces their criminal exposure and mortality risk while imposing a different form of constraint (medicalized surveillance, unresolved supply risk) — this is not a beneficiary in the unambiguous sense of the service providers, whose funding and legitimacy is the clean beneficiary case. Supply-chain workers and unregulated-market participants are unambiguous victims: the reading's core structural move is to relocate criminalization onto them rather than eliminate it, which is exactly why the reading is tangled_rope rather than rope — the coordination function (harm reduction for users) is real, but it is purchased in part by continued extraction from a class the reading excludes from its own benefit logic.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventable overdose death and disease transmission under pure prohibition — remains live by independent epidemiological corroboration, which argues against pure mandatrophy. However, there is a partial mandatrophy signature in the institutional apparatus itself: public health agencies and service providers have organizational interests in the problem persisting in its current framing (chronic, manageable, not resolved by full legalization) that are separable from the founding problem's actual resolution. The classification as tangled_rope rather than snare or rope captures this precisely: real coordination benefit exists (mandatrophy is NOT fully resolved — the intervention still serves its founding function for users) even as an asymmetric extraction structure persists on the supply side that the reading's own logic does not address.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    supply_chain_exclusion_necessity,
    'Is continued criminalization of the supply chain a structural necessity for the harm reduction reading to maintain political legitimacy (avoiding the ''facilitating drug dealing'' charge), or is it a severable extraction that the reading could shed while retaining its coordination function?',
    'Comparative policy analysis of jurisdictions that have paired harm reduction services with partial supply decriminalization or regulated supply (e.g., Portugal''s decriminalization model, Swiss heroin-assisted treatment with regulated supply) against outcomes in jurisdictions that maintain harm reduction with full supply criminalization.',
    'If severable, the continued criminalization of supply chain workers is pure extraction riding on the health reframe''s political cover, strengthening the case for reclassifying toward snare on the supply-chain axis. If structurally necessary for legitimacy, the tangled_rope classification is the accurate steady-state description rather than a transitional failure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supply_chain_exclusion_necessity, empirical, 'Whether supply-chain criminalization is required for the reading''s legitimacy or is severable extraction.').

omega_variable(
    paternalism_vs_autonomy_framing,
    'Does the health-condition framing of substance use genuinely serve users'' interests better than a liberty framing would, or does it function primarily to preserve institutional and professional authority over users'' choices?',
    'User-reported outcome studies comparing satisfaction, autonomy, and health outcomes under mandatory-adjacent case-management models versus low-threshold, autonomy-preserving harm reduction models (e.g., unconditional cash transfer or housing-first approaches without treatment referral pressure).',
    'If the paternalistic elements produce no measurable benefit over autonomy-preserving alternatives, the paternalism component is closer to pure extraction (control for its own sake) rather than coordination; if it improves outcomes, it strengthens the coordination reading of the constraint on users.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(paternalism_vs_autonomy_framing, conceptual, 'Whether medicalized paternalism serves user interests or institutional control interests.').

omega_variable(
    kernel_reading_stability,
    'Is the harm reduction reading a stable equilibrium, or a way-station that either regresses toward prohibition (if overdose numbers fail to improve, triggering backlash) or progresses toward legalization (if supply-side harms become the dominant policy concern)?',
    'Longitudinal tracking of jurisdictions that adopted harm reduction frameworks 10+ years ago to observe whether policy has drifted toward either sibling reading, and what triggered the drift.',
    'If systematically unstable in one direction, this reading may be better modeled as a scaffold (transitional) rather than a steady-state tangled_rope — though no sunset clause is currently declared by any jurisdiction operating under this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_stability, conceptual, 'Whether this reading is a stable arrangement or a transitional state between prohibition and legalization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_kernel__harm_reduction_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_kernel__harm_reduction_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(subs_tr_t4, substance_control_kernel__harm_reduction_reading, theater_ratio, 4, 0.14).
narrative_ontology:measurement(subs_tr_t8, substance_control_kernel__harm_reduction_reading, theater_ratio, 8, 0.16).
narrative_ontology:measurement(subs_tr_t12, substance_control_kernel__harm_reduction_reading, theater_ratio, 12, 0.18).
narrative_ontology:measurement(subs_tr_t16, substance_control_kernel__harm_reduction_reading, theater_ratio, 16, 0.19).
narrative_ontology:measurement(subs_tr_t20, substance_control_kernel__harm_reduction_reading, theater_ratio, 20, 0.21).
narrative_ontology:measurement(subs_tr_t24, substance_control_kernel__harm_reduction_reading, theater_ratio, 24, 0.22).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_kernel__harm_reduction_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(subs_be_t4, substance_control_kernel__harm_reduction_reading, base_extractiveness, 4, 0.51).
narrative_ontology:measurement(subs_be_t8, substance_control_kernel__harm_reduction_reading, base_extractiveness, 8, 0.47).
narrative_ontology:measurement(subs_be_t12, substance_control_kernel__harm_reduction_reading, base_extractiveness, 12, 0.45).
narrative_ontology:measurement(subs_be_t16, substance_control_kernel__harm_reduction_reading, base_extractiveness, 16, 0.44).
narrative_ontology:measurement(subs_be_t20, substance_control_kernel__harm_reduction_reading, base_extractiveness, 20, 0.43).
narrative_ontology:measurement(subs_be_t24, substance_control_kernel__harm_reduction_reading, base_extractiveness, 24, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_kernel__harm_reduction_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(subs_su_t4, substance_control_kernel__harm_reduction_reading, suppression_requirement, 4, 0.55).
narrative_ontology:measurement(subs_su_t8, substance_control_kernel__harm_reduction_reading, suppression_requirement, 8, 0.49).
narrative_ontology:measurement(subs_su_t12, substance_control_kernel__harm_reduction_reading, suppression_requirement, 12, 0.44).
narrative_ontology:measurement(subs_su_t16, substance_control_kernel__harm_reduction_reading, suppression_requirement, 16, 0.41).
narrative_ontology:measurement(subs_su_t20, substance_control_kernel__harm_reduction_reading, suppression_requirement, 20, 0.39).
narrative_ontology:measurement(subs_su_t24, substance_control_kernel__harm_reduction_reading, suppression_requirement, 24, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_kernel__harm_reduction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(substance_control_kernel__harm_reduction_reading, 0.12).
narrative_ontology:affects_constraint(substance_control_kernel__harm_reduction_reading, substance_control_kernel__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_kernel__harm_reduction_reading, substance_control_kernel__legalization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the substance_control_kernel, decomposed per the ε-invariance principle because the three readings assign structurally different victim sets, different extractiveness values, and different coordination/extraction balances to what colloquial discourse treats as a single policy question ('drug policy'). The prohibition_reading treats users as the primary victim class under a moral-transgression framing (high ε, high suppression, snare-leaning). The legalization_reading treats state intervention as narrowly justified only by externality capture, with no supply-chain criminalization (lowest ε, rope-leaning). This harm_reduction_reading sits structurally between them: it removes users from the criminalized class but preserves supply-chain criminalization and layers medicalized paternalism onto users, producing a tangled_rope profile with moderate ε. Each reading is authored as its own constraint with its own stable ε; they are linked here rather than merged into one observable-dependent story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
