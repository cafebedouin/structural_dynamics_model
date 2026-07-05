% ============================================================================
% CONSTRAINT STORY: climate_response_imperative__adaptation_priority_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_imperative__adaptation_priority_reading, []).

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
 *   constraint_id: climate_response_imperative__adaptation_priority_reading
 *   human_readable: Adaptation-Priority Reading of the Climate Response Imperative
 *   domain: climate_policy/political_economy/intergenerational_justice
 *
 * SUMMARY:
 *   This story instantiates the adaptation-priority reading of the climate
 *   response imperative: the position that resilience-building and damage
 *   reduction in exposed regions is the primary, realistic climate response,
 *   with global mitigation treated as an aspirational but non-binding aim.
 *   This reading has become institutionally dominant in much multilateral
 *   finance architecture (Green Climate Fund adaptation windows, National
 *   Adaptation Plans) even as the scientific consensus treats adaptation as a
 *   necessary complement to, not substitute for, mitigation. The reading
 *   produces a distinctive structural delta: present-day developing nations
 *   enter the victim set through immediate, unmet capital requirements for
 *   resilience infrastructure they did not cause the need for, creating a
 *   vicious circle where the countries with the least historical
 *   responsibility for cumulative emissions bear the highest present and
 *   future adaptation costs, financed disproportionately through debt rather
 *   than transfer.
 *
 * KEY AGENTS:
 *   - high_emission_incumbent_industries: beneficiary of deferred mitigation, institutional power, arbitrage exit
 *   - donor_country_treasuries: agenda-setter over climate finance architecture, institutional power
 *   - adaptation_finance_intermediaries: capture fee income from adaptation buildout, organized power
 *   - low_lying_coastal_nations: primary payers, powerless, trapped by geography
 *   - smallholder_farmers_in_exposed_regions: payers, powerless, trapped by lack of capital access
 *   - future_generations_in_exposed_regions: excluded from present negotiations, will inherit compounding risk
 *   - climate_scientists_and_iam_modelers: analytical observers documenting the adaptation-mitigation gap
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_imperative__adaptation_priority_reading, 0.66).
domain_priors:suppression_score(climate_response_imperative__adaptation_priority_reading, 0.58).
domain_priors:theater_ratio(climate_response_imperative__adaptation_priority_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_imperative__adaptation_priority_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(climate_response_imperative__adaptation_priority_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(climate_response_imperative__adaptation_priority_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_imperative__adaptation_priority_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(climate_response_imperative__adaptation_priority_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_imperative__adaptation_priority_reading, tangled_rope).
narrative_ontology:human_readable(climate_response_imperative__adaptation_priority_reading, "Adaptation-Priority Reading of the Climate Response Imperative").
narrative_ontology:topic_domain(climate_response_imperative__adaptation_priority_reading, "climate_policy/political_economy/intergenerational_justice").

domain_priors:requires_active_enforcement(climate_response_imperative__adaptation_priority_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_imperative__adaptation_priority_reading, '6dfea5cc-2069-43d7-9f50-ba03e7dd9f12').
narrative_ontology:cs_kernel_codification('6dfea5cc-2069-43d7-9f50-ba03e7dd9f12', distributed).
narrative_ontology:cs_authority_grounding('6dfea5cc-2069-43d7-9f50-ba03e7dd9f12', distributed).
narrative_ontology:cs_reading_relation('6dfea5cc-2069-43d7-9f50-ba03e7dd9f12', climate_response_imperative__mitigation_priority_reading, coexists_with).
narrative_ontology:cs_reading_relation('6dfea5cc-2069-43d7-9f50-ba03e7dd9f12', climate_response_imperative__degrowth_reading, coexists_with).
narrative_ontology:cs_axiom('6dfea5cc-2069-43d7-9f50-ba03e7dd9f12', foundational, resilience_investment_is_the_tractable_near_term_obligation).
narrative_ontology:cs_axiom_status(resilience_investment_is_the_tractable_near_term_obligation, holdable).
narrative_ontology:cs_axiom_grounding('6dfea5cc-2069-43d7-9f50-ba03e7dd9f12', resilience_investment_is_the_tractable_near_term_obligation, instrumental).
narrative_ontology:cs_axiom('6dfea5cc-2069-43d7-9f50-ba03e7dd9f12', secondary, mitigation_targets_are_aspirational_rather_than_binding).
narrative_ontology:cs_axiom_status(mitigation_targets_are_aspirational_rather_than_binding, holdable).
narrative_ontology:cs_axiom_grounding('6dfea5cc-2069-43d7-9f50-ba03e7dd9f12', mitigation_targets_are_aspirational_rather_than_binding, conventional).
narrative_ontology:cs_reference_frame('6dfea5cc-2069-43d7-9f50-ba03e7dd9f12', unfccc_common_but_differentiated_responsibilities).
narrative_ontology:cs_drift_state('6dfea5cc-2069-43d7-9f50-ba03e7dd9f12', post_paris_agreement_implementation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('6dfea5cc-2069-43d7-9f50-ba03e7dd9f12', '').
narrative_ontology:cs_kernel_id(climate_response_imperative__adaptation_priority_reading, climate_response_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_imperative__adaptation_priority_reading, high_emission_incumbent_industries).
narrative_ontology:constraint_beneficiary(climate_response_imperative__adaptation_priority_reading, donor_country_treasuries).
narrative_ontology:constraint_beneficiary(climate_response_imperative__adaptation_priority_reading, adaptation_finance_intermediaries).
narrative_ontology:constraint_victim(climate_response_imperative__adaptation_priority_reading, low_lying_coastal_nations).
narrative_ontology:constraint_victim(climate_response_imperative__adaptation_priority_reading, smallholder_farmers_in_exposed_regions).
narrative_ontology:constraint_victim(climate_response_imperative__adaptation_priority_reading, future_generations_in_exposed_regions).
narrative_ontology:constraint_vindicates(climate_response_imperative__adaptation_priority_reading, adaptation_is_pragmatic_realism_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Fossil-fuel-dependent and heavy-industry firms in high-emission economies benefit directly when the dominant climate framing treats resilience and adaptation as the pragmatic near-term response, since this defers binding emissions-reduction schedules and preserves existing capital-intensive production. They fund research, lobbying, and multilateral positioning that frames adaptation as the responsible, achievable path while mitigation remains aspirational.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, high_emission_incumbent_industries, beneficiary,
    institutional, biographical, arbitrage, global).

% Wealthy-nation finance ministries set the terms of multilateral climate finance, favoring adaptation loans and resilience grants over binding domestic mitigation commitments or unconditional loss-and-damage transfers. They administer the funding instruments, attach conditionality, and can redirect climate finance architecture, but bear none of the direct physical exposure themselves.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, donor_country_treasuries, agenda_setter,
    institutional, biographical, arbitrage, global).

% Multilateral development banks, consulting firms, and resilience-infrastructure contractors capture fee income and project pipelines from the adaptation-finance buildout. Their institutional survival depends on adaptation remaining the dominant response paradigm rather than mitigation success shrinking the exposure base they service.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, adaptation_finance_intermediaries, beneficiary,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(climate_response_imperative__adaptation_priority_reading, adaptation_finance_intermediaries, agenda_setter).

% Small island states and low-lying coastal countries face existential physical exposure but must raise or borrow the capital for seawalls, relocation, and infrastructure hardening themselves, often through debt rather than grants. They did not generate the historical emissions driving the exposure and have no exit from geography; the adaptation-priority framing shifts the cost of a problem they did not cause onto their own constrained balance sheets.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, low_lying_coastal_nations, payer,
    powerless, civilizational, trapped, national).

% Rural agricultural populations in drought- and flood-exposed regions bear crop loss, displacement, and income collapse as climate impacts accelerate faster than local adaptation capacity can be financed. They have no capital markets access of their own and depend on aid or national government intermediation that is itself capital-constrained.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, smallholder_farmers_in_exposed_regions, payer,
    powerless, biographical, trapped, regional).

% People not yet born in currently exposed regions will inherit compounding physical risk and adaptation debt if mitigation stays aspirational while adaptation absorbs available finance and political attention. They cannot participate in present negotiations, sit outside every stakeholder table, and their interests are represented only derivatively through advocacy groups with no binding authority.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, future_generations_in_exposed_regions, excluded,
    powerless, civilizational, trapped, global).

% Researchers model emissions trajectories, adaptation limits, and loss-and-damage pathways, producing evidence used by all parties. They document the widening gap between adaptation-finance flows and assessed need, and increasingly warn that adaptation has hard physical limits (e.g., sea-level rise, wet-bulb temperature) beyond which no resilience investment substitutes for avoided emissions.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, climate_scientists_and_iam_modelers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_imperative__adaptation_priority_reading, diffuse).
narrative_ontology:fixing_cost_class(climate_response_imperative__adaptation_priority_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Adaptation finance genuinely coordinates real, urgent risk-reduction: seawalls, drought-resistant seed distribution, early-warning systems, and relocation planning save lives and reduce near-term damage regardless of what happens to global emissions. This coordination function is real and defensible on its own terms.
% TRANSFER_FUNCTION: Under the adaptation-priority framing, political attention, multilateral finance, and technical capacity concentrate on resilience infrastructure in exposed regions rather than on binding emissions-reduction obligations in high-emission economies. The costs of unmitigated warming are transferred forward in time (to future generations in exposed regions) and outward in space (onto the countries least responsible for cumulative emissions), while the economic benefit of deferred mitigation accrues to incumbent high-emission industries and to donor treasuries that avoid the fiscal and political cost of binding domestic decarbonization.
% ABSENT_VOICES: Future generations in currently exposed regions have no seat in present climate finance negotiations and cannot bargain over the intertemporal transfer being made in their name. Smallholder farmers and other localized populations within recipient nations are frequently absent from the design of the adaptation projects notionally built for them, with national elites and international intermediaries setting priorities instead.
% DISAPPEARANCE_RATIONALE: If the adaptation-priority framing disappeared overnight, donor treasuries and incumbent industries would lose a politically convenient deferral mechanism and would face renewed pressure for binding mitigation commitments — a real rearrangement of political economy. Exposed nations dispute whether this would help them: some argue immediate resilience needs are so urgent that any framing shift risking near-term adaptation finance would be actively harmful, while others argue the framing itself has suppressed the loss-and-damage and mitigation obligations that would address the root cause. The verdict is genuinely contested between and within the victim populations themselves.
% FOUNDING_PROBLEM: Physical climate impacts (sea-level rise, extreme heat, drought, storm intensification) are already occurring in exposed regions faster than mitigation alone can address them, creating an urgent, immediate need for resilience-building independent of whatever happens to future emissions trajectories.
% FOUNDING_PROBLEM_CORROBORATION: The physical urgency is corroborated by IPCC Working Group II assessments and by exposed-nation governments themselves, who are not primary beneficiaries of the adaptation-priority framing's finance architecture. However, the SPECIFIC prioritization of adaptation over binding mitigation is corroborated primarily by donor-country treasuries and incumbent-industry-funded research, not by independent scientific bodies — the IPCC and most loss-and-damage advocacy groups (outside the beneficiary set) explicitly warn against treating adaptation as a substitute for mitigation rather than a complement to it.
narrative_ontology:disappearance_verdict(climate_response_imperative__adaptation_priority_reading, contested).
narrative_ontology:founding_problem_status(climate_response_imperative__adaptation_priority_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_imperative__adaptation_priority_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_response_imperative__adaptation_priority_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_imperative__adaptation_priority_reading, 0.66, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_imperative__adaptation_priority_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_imperative__adaptation_priority_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_imperative__adaptation_priority_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.66 and rising over the interval because the adaptation-priority framing increasingly channels finite political and financial capital toward resilience infrastructure in exposed regions while binding mitigation commitments remain non-enforceable — a real and growing transfer from those least responsible for emissions to those who benefit from continued high-emission activity. Suppression (0.58) reflects the structural mechanism by which alternative framings (binding mitigation targets, loss-and-damage as unconditional transfer, degrowth) are marginalized in multilateral negotiating texts and finance instrument design, not overt coercion. Theater ratio (0.44, rising) captures the growing gap between adaptation-finance pledges and disbursed, need-matched funding — COP-cycle announcements substitute for tracked delivery. Accessibility collapse is moderate (0.5): exposed nations retain some access to alternative framings (loss-and-damage funds, litigation, South-South coalitions) but these remain structurally weaker than the dominant finance architecture. Resistance is substantial (0.62): AOSIS, the G77, and civil-society loss-and-damage advocacy actively contest the adaptation-priority framing at every COP.
 *
 * PERSPECTIVAL GAP:
 *   From the donor-treasury and incumbent-industry seats, adaptation-priority reads as pragmatic realism: mitigation targets have repeatedly been missed, so resourcing the response that saves lives now is the responsible course. From the exposed-nation seats, the same framing reads as a mechanism that lets historical emitters avoid binding obligations while transferring the compounding cost of their emissions onto nations with no meaningful role in causing the problem. The engine computes these as structurally different seat classifications from the same base data — the divergence is not a modeling artifact but the substance of the underlying dispute.
 *
 * DIRECTIONALITY LOGIC:
 *   High-emission incumbent industries and adaptation finance intermediaries sit near the beneficiary end: deferred mitigation preserves their capital base and revenue pipeline respectively. Donor treasuries benefit from avoided fiscal/political cost of binding decarbonization even while administering aid. Low-lying coastal nations, smallholder farmers, and future generations sit near the target end: trapped exit options (geography, poverty, non-existence-yet), civilizational time horizon for the least powerful group, and no meaningful capacity to renegotiate the framing that determines what finance they receive and on what terms.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — urgent, real physical exposure requiring resilience investment — remains genuinely live (status: live), which prevents this story from being mislabeled a pure snare with no coordination function; the resilience infrastructure funded under this framing does reduce real, measured harm. But the founding-problem corroboration reveals the specific PRIORITIZATION of adaptation over mitigation is attested mainly by the beneficiary parties, while independent scientific and exposed-nation voices treat adaptation as a necessary complement being weaponized as a substitute. This is the tangled-rope signature precisely: a genuine coordination function (resilience-building) coexisting with asymmetric extraction (deferred mitigation obligation) through the same institutional structure, requiring active enforcement (negotiating-text control, finance-instrument design) to hold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    adaptation_mitigation_substitutability,
    'Is adaptation capacity in principle substitutable for avoided emissions up to some threshold, or does physical reality (sea-level rise commitment, wet-bulb temperature limits) impose a hard ceiling beyond which no resilience investment can compensate for unmitigated warming?',
    'Physical climate science on committed sea-level rise, heat-stress thresholds for human habitability, and agricultural yield collapse points under various emissions scenarios; comparison of adaptation-finance trajectories against these physical limits.',
    'If a hard ceiling exists and current emissions trajectories approach it, the adaptation-priority reading is not merely extractive but eventually infeasible on its own terms — the coordination function itself collapses at high enough warming, which would push the classification further toward snare as the physical floor is approached.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptation_mitigation_substitutability, empirical, 'Whether adaptation has a physical substitutability ceiling relative to mitigation.').

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly do the adaptation-priority, mitigation-priority, and degrowth readings of the climate response imperative disagree — is it about causal efficacy (what actually reduces harm), about responsibility allocation (who should pay), or about feasibility (what is politically achievable given existing power structures)?',
    'Structural comparison of the three sibling constraint stories'' beneficiary/victim declarations and axiom sets; the degrowth_reading would allocate responsibility very differently than either adaptation-priority or mitigation-priority, while adaptation-priority and mitigation-priority disagree more on causal-efficacy sequencing than on responsibility allocation.',
    'If the disagreement is primarily about responsibility allocation rather than causal efficacy, the adaptation-priority reading''s persistence looks more like an extraction mechanism (avoiding responsibility) than a genuine empirical disagreement about what works; if primarily about feasibility, it looks more like a genuine political-economy constraint that any reading would have to contend with.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Locating the structural axis along which the three kernel readings actually diverge.').

omega_variable(
    finance_architecture_capture,
    'Is the adaptation-finance intermediary sector''s institutional interest in maintaining adaptation-priority framing (rather than mitigation success shrinking their project pipeline) a significant causal driver of the framing''s dominance in multilateral texts, or is the framing dominance better explained by donor-treasury preferences alone?',
    'Tracing intermediary-sector lobbying and technical-advisory participation in COP negotiating text drafting relative to their financial stake in adaptation project volume.',
    'If intermediary capture is a significant independent driver, the beneficiary set requires disaggregation and the extraction is more distributed and self-reinforcing than a simple donor-treasury/incumbent-industry account suggests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(finance_architecture_capture, empirical, 'Whether adaptation-finance intermediaries independently drive framing dominance beyond donor-treasury interests.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_imperative__adaptation_priority_reading, 1992, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t1992, climate_response_imperative__adaptation_priority_reading, theater_ratio, 1992, 0.2).
narrative_ontology:measurement(clim_tr_t1997, climate_response_imperative__adaptation_priority_reading, theater_ratio, 1997, 0.24).
narrative_ontology:measurement(clim_tr_t2005, climate_response_imperative__adaptation_priority_reading, theater_ratio, 2005, 0.28).
narrative_ontology:measurement(clim_tr_t2009, climate_response_imperative__adaptation_priority_reading, theater_ratio, 2009, 0.33).
narrative_ontology:measurement(clim_tr_t2015, climate_response_imperative__adaptation_priority_reading, theater_ratio, 2015, 0.36).
narrative_ontology:measurement(clim_tr_t2019, climate_response_imperative__adaptation_priority_reading, theater_ratio, 2019, 0.4).
narrative_ontology:measurement(clim_tr_t2024, climate_response_imperative__adaptation_priority_reading, theater_ratio, 2024, 0.44).

% Extraction over time
narrative_ontology:measurement(clim_be_t1992, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 1992, 0.32).
narrative_ontology:measurement(clim_be_t1997, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 1997, 0.38).
narrative_ontology:measurement(clim_be_t2005, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 2005, 0.45).
narrative_ontology:measurement(clim_be_t2009, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 2009, 0.52).
narrative_ontology:measurement(clim_be_t2015, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 2015, 0.58).
narrative_ontology:measurement(clim_be_t2019, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 2019, 0.62).
narrative_ontology:measurement(clim_be_t2024, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 2024, 0.66).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t1992, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 1992, 0.35).
narrative_ontology:measurement(clim_su_t1997, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 1997, 0.38).
narrative_ontology:measurement(clim_su_t2005, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 2005, 0.42).
narrative_ontology:measurement(clim_su_t2009, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 2009, 0.47).
narrative_ontology:measurement(clim_su_t2015, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 2015, 0.51).
narrative_ontology:measurement(clim_su_t2019, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 2019, 0.55).
narrative_ontology:measurement(clim_su_t2024, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 2024, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_imperative__adaptation_priority_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_imperative__adaptation_priority_reading, 0.12).
narrative_ontology:affects_constraint(climate_response_imperative__adaptation_priority_reading, climate_response_imperative__mitigation_priority_reading).
narrative_ontology:affects_constraint(climate_response_imperative__adaptation_priority_reading, climate_response_imperative__degrowth_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the climate_response_imperative kernel. adaptation_priority_reading (this story) treats resilience-building as primary and mitigation as aspirational; mitigation_priority_reading treats emissions reduction via technology/markets as primary and adaptation as residual; degrowth_reading treats structural economic transformation in the Global North as the necessary precondition for both. Each reading has a distinct beneficiary/victim structure and a distinct epsilon: this reading's victim set specifically includes present-day developing nations facing immediate, unmet capital requirements, a structural feature not present in the same form in the mitigation-priority reading (whose victims are more concentrated in future-affected populations globally) or the degrowth reading (whose victims are concentrated among Global North consumption-dependent constituencies).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
