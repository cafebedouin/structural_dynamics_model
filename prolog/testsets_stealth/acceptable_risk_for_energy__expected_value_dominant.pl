% ============================================================================
% CONSTRAINT STORY: acceptable_risk_for_energy__expected_value_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_for_energy__expected_value_dominant, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: acceptable_risk_for_energy__expected_value_dominant
 *   human_readable: Expected-Value-Dominant Energy Risk Acceptability Standard
 *   domain: economic/political/regulatory
 *
 * SUMMARY:
 *   A regulatory acceptability standard governs which energy infrastructure
 *   may be built and operated: a proposal is acceptable when its annualized
 *   expected costs, including rare accidents weighted by probability times
 *   consequence, are favorable against its expected benefits, chiefly climate
 *   and supply benefits. Probabilistic risk assessments, design-basis event
 *   frequencies, and dollar-per-unit-health-effect conversion factors are the
 *   working machinery; licensing boards, safety regulators, and reviewing
 *   courts apply the ledger. The arrangement solves a real problem —
 *   comparing heterogeneous risks on one auditable scale — while
 *   concentrating certain costs on parties who never agreed to them and whose
 *   losses enter the ledger at figures they would not voluntarily accept. KEY
 *   AGENTS (by structural relationship): - safety_regulators: Agenda-setter
 *   (institutional/identity_locked) — administers the standard, commissions
 *   and reviews the assessments, licenses on the ledger -
 *   nuclear_utilities_operators: Primary beneficiary (powerful/arbitrage) —
 *   receives approval pathways and revenue; commissions the underlying
 *   analyses - plant_host_communities: Primary local target
 *   (powerless/trapped) — bear concentrated accident risk and land-use
 *   burdens near facilities - downwind_downstream_residents: Excluded
 *   cost-bearer (powerless/trapped) — bear transboundary accident
 *   consequences with no seat in licensing - captive_regional_ratepayers: Net
 *   payer with consumption benefit (moderate/constrained) — fund construction
 *   overruns and decommissioning through tariffs -
 *   energy_intensive_industrial_users: Mobile beneficiary (organized/mobile)
 *   — enjoy cheap firm power and can relocate if prices rise -
 *   climate_policy_institutions: System-level beneficiary
 *   (institutional/constrained) — depend on the framework keeping firm
 *   low-carbon capacity licensable - independent_risk_assessment_community:
 *   Analytical observer (analytical/analytical) — audits methods, publishes
 *   critiques, supplies peer-review legitimacy
 *
 * KEY AGENTS:
 *   - safety_regulators: agenda-setter (institutional/identity_locked) — administers the acceptability standard; its authority rests on being the competent quantifier
 *   - nuclear_utilities_operators: primary beneficiary with agenda-setting reach (powerful/arbitrage) — collects approval and revenue; commissions the analyses that feed the ledger
 *   - plant_host_communities: primary local target (powerless/trapped) — concentrated involuntary exposure near facilities
 *   - downwind_downstream_residents: excluded cost-bearer (powerless/trapped) — transboundary exposure entered as population-dose statistics
 *   - captive_regional_ratepayers: net payer, secondary beneficiary (moderate/constrained) — absorb overruns and decommissioning costs through regulated tariffs
 *   - energy_intensive_industrial_users: mobile beneficiary (organized/mobile) — cheap firm power with jurisdictional exit
 *   - climate_policy_institutions: system-level beneficiary (institutional/constrained) — the framework keeps firm low-carbon capacity available for decarbonization targets
 *   - independent_risk_assessment_community: analytical observer (analytical/analytical) — method audit and peer-review legitimacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_for_energy__expected_value_dominant, 0.38).
domain_priors:suppression_score(acceptable_risk_for_energy__expected_value_dominant, 0.2).
domain_priors:theater_ratio(acceptable_risk_for_energy__expected_value_dominant, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, extractiveness, 0.38).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_for_energy__expected_value_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_for_energy__expected_value_dominant, "Expected-Value-Dominant Energy Risk Acceptability Standard").
narrative_ontology:topic_domain(acceptable_risk_for_energy__expected_value_dominant, "economic/political/regulatory").

domain_priors:requires_active_enforcement(acceptable_risk_for_energy__expected_value_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_for_energy__expected_value_dominant, 'c42f0815-3e37-4c27-acc1-a1401c5b3a87').
narrative_ontology:cs_kernel_codification('c42f0815-3e37-4c27-acc1-a1401c5b3a87', formalized).
narrative_ontology:cs_authority_grounding('c42f0815-3e37-4c27-acc1-a1401c5b3a87', expertise).
narrative_ontology:cs_interpretation_layer_present('c42f0815-3e37-4c27-acc1-a1401c5b3a87').
narrative_ontology:cs_reading_relation('c42f0815-3e37-4c27-acc1-a1401c5b3a87', acceptable_risk_for_energy__catastrophic_tail_dominant, coexists_with).
narrative_ontology:cs_reading_relation('c42f0815-3e37-4c27-acc1-a1401c5b3a87', acceptable_risk_for_energy__comparative_risk_dominant, influences).
narrative_ontology:cs_axiom('c42f0815-3e37-4c27-acc1-a1401c5b3a87', foundational, expected_value_suffices_for_acceptability).
narrative_ontology:cs_axiom_status(expected_value_suffices_for_acceptability, holdable).
narrative_ontology:cs_axiom_grounding('c42f0815-3e37-4c27-acc1-a1401c5b3a87', expected_value_suffices_for_acceptability, instrumental).
narrative_ontology:cs_axiom('c42f0815-3e37-4c27-acc1-a1401c5b3a87', foundational, rare_events_weighted_by_probability_times_consequence).
narrative_ontology:cs_axiom_status(rare_events_weighted_by_probability_times_consequence, holdable).
narrative_ontology:cs_axiom_grounding('c42f0815-3e37-4c27-acc1-a1401c5b3a87', rare_events_weighted_by_probability_times_consequence, empirically_contingent).
narrative_ontology:cs_axiom('c42f0815-3e37-4c27-acc1-a1401c5b3a87', secondary, climate_benefits_enter_acceptability_ledger).
narrative_ontology:cs_axiom_status(climate_benefits_enter_acceptability_ledger, holdable).
narrative_ontology:cs_axiom_grounding('c42f0815-3e37-4c27-acc1-a1401c5b3a87', climate_benefits_enter_acceptability_ledger, instrumental).
narrative_ontology:cs_reference_frame('c42f0815-3e37-4c27-acc1-a1401c5b3a87', annualized_expected_value_ledger).
narrative_ontology:cs_drift_state('c42f0815-3e37-4c27-acc1-a1401c5b3a87', post_fukushima_contemporary, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('c42f0815-3e37-4c27-acc1-a1401c5b3a87', '').
narrative_ontology:cs_kernel_id(acceptable_risk_for_energy__expected_value_dominant, acceptable_risk_for_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__expected_value_dominant, nuclear_utilities_operators).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__expected_value_dominant, energy_intensive_industrial_users).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__expected_value_dominant, climate_policy_institutions).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__expected_value_dominant, plant_host_communities).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__expected_value_dominant, downwind_downstream_residents).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__expected_value_dominant, captive_regional_ratepayers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__expected_value_dominant, captive_regional_ratepayers).
narrative_ontology:constraint_vindicates(acceptable_risk_for_energy__expected_value_dominant, expected_utility_decision_theory).
narrative_ontology:constraint_vindicates(acceptable_risk_for_energy__expected_value_dominant, actuarial_comparability_of_heterogeneous_risks).
narrative_ontology:constraint_vindicates(acceptable_risk_for_energy__expected_value_dominant, cost_benefit_analysis_as_neutral_arbiter).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the acceptability standard: sets design-basis event frequencies, reviews probabilistic risk assessments, converts health effects to monetary values, and grants or denies licenses on the resulting ledger. Staffed by professionals trained in the method; the agency's mandate, budget, and standing before reviewing courts all presuppose that quantitative risk assessment is the way safety is governed. Adopting a different acceptability logic would require statutory re-legitimation the agency cannot perform on its own.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, safety_regulators, agenda_setter,
    institutional, generational, identity_locked, national).

% Develops and operates generating stations whose approval depends on a favorable ledger entry. Commissions the underlying risk and cost studies, proposes the input assumptions, and lobbies for the methodology's continued primacy. Collects revenue from approved operations; when construction costs exceed the ex ante estimates, regulated recovery mechanisms pass much of the difference to customers rather than shareholders. Allocates capital across technologies and jurisdictions, and shifts out of any technology whose ledger turns unfavorable.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, nuclear_utilities_operators, beneficiary,
    powerful, biographical, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_for_energy__expected_value_dominant, nuclear_utilities_operators, agenda_setter).

% Live and work adjacent to licensed facilities, bearing concentrated accident risk, emergency-planning burdens, and land-use restrictions. Receive employment, tax base, and negotiated community payments. Siting decisions were made through aggregate national arithmetic; relocation away from the facility is costly and the facility cannot be relocated away from them. Participation is limited to hearing comment periods held after the fundamental siting choices are fixed.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, plant_host_communities, payer,
    powerless, biographical, trapped, local).

% Live outside the host jurisdiction but within range of accident plumes and waterborne releases. Their exposure enters the licensing record as population-dose statistics aggregated across large areas; they hold no party status in proceedings, receive no compensation stream, and would contest both the probability estimates and the per-health-effect valuations applied to them if given a seat.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, downwind_downstream_residents, excluded,
    powerless, biographical, trapped, regional).

% Purchase electricity from vertically integrated suppliers and pay tariffs that recover construction expenditures, cost overruns, and decommissioning accruals. Cannot choose alternative suppliers within the service territory. Also receive the firm low-carbon output the financed plants deliver, so the same tariff both funds and serves them.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, captive_regional_ratepayers, payer,
    moderate, immediate, constrained, regional).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_for_energy__expected_value_dominant, captive_regional_ratepayers, beneficiary).

% Operate smelters, data centers, and process industries that depend on abundant firm power at predictable prices. Benefit from the low-cost baseload the framework keeps licensable, and can relocate production across jurisdictions if regional prices rise, which disciplines how much of any cost increase they will tolerate.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, energy_intensive_industrial_users, beneficiary,
    organized, biographical, mobile, continental).

% Carry decarbonization commitments that require dispatchable low-carbon generation at scale. The acceptability framework determines whether that capacity remains buildable; without it, the institutions must pursue higher-cost or slower portfolios. They advocate for the framework's stability in legislative and international settings but do not run it.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, climate_policy_institutions, beneficiary,
    institutional, civilizational, constrained, global).

% Academic researchers, standing advisory committees, and international review panels who audit the method: recalibrate failure-rate databases, critique discount rates and tail treatments, and publish methodological standards. Supplies the peer-review legitimacy the framework trades on and is the venue where challenges to the weighting scheme are actually argued.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, independent_risk_assessment_community, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(acceptable_risk_for_energy__expected_value_dominant, nuclear_utilities_operators).
narrative_ontology:fixing_cost_class(acceptable_risk_for_energy__expected_value_dominant, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single commensurable, auditable metric — annualized expected cost including accident probabilities, netted against expected benefits — for comparing heterogeneous energy risks across technologies, proposals, and time, replacing case-by-case political adjudication with a repeatable procedure that investors, regulators, and reviewers can all price against.
% TRANSFER_FUNCTION: Moves decision authority from politically mobilized objectors to technical-analytic processes; moves approval probability, and behind it capital flows and operating revenue, toward projects with favorable ledgers; and moves cost-overrun and decommissioning liabilities from operator shareholders to captive regional ratepayers when ex ante estimates prove optimistic.
% ABSENT_VOICES: Downwind and downstream populations and prospective repository-host regions would object that their exposure was priced without their consent and at values they would not accept; they are absent because licensing aggregates their doses statistically, grants them no party status, and schedules comment periods only after siting fundamentals are fixed. Future cohorts bearing century-scale stewardship liabilities likewise have no seat, their interests entering only through a discount rate they had no hand in choosing.
% DISAPPEARANCE_RATIONALE: If the expected-value acceptability standard vanished overnight, licensing pipelines would stall pending a replacement decision logic, capital would flee long-horizon energy projects lacking any stable valuation basis, comparative technology assessment would collapse into ad hoc political combat, and the regulator's governing procedure would dissolve — the energy economy would reorganize around whatever acceptability logic replaced the ledger.
% FOUNDING_PROBLEM: Mid-twentieth-century expansion of civilian nuclear power and other large hazardous infrastructure outran case-by-case judgment: regulators faced recurring panic-driven demands for prohibition and promoter-driven assurances of safety, with no uniform, transparent, publicly defensible answer to 'how safe is safe enough.' The arrangement was built to make acceptability decisions consistent, auditable, and resistant to both extremes.
% FOUNDING_PROBLEM_CORROBORATION: Independent of the operating companies that profit from approvals, national academy committees, international safety-review bodies, and court opinions accepting quantitative risk assessment all attest that heterogeneous technological risk still requires a defensible adjudication procedure. Academic decision theorists corroborate the problem's persistence from outside the regulatory apparatus, and — significantly — the framework's sharpest critics also attest the founding problem is live while disputing whether this solution remains adequate to it.
narrative_ontology:disappearance_verdict(acceptable_risk_for_energy__expected_value_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_for_energy__expected_value_dominant, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_for_energy__expected_value_dominant, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(acceptable_risk_for_energy__expected_value_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_for_energy__expected_value_dominant, 0.38, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_for_energy__expected_value_dominant_tests).
:- end_tests(acceptable_risk_for_energy__expected_value_dominant_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38): the ledger genuinely prices most costs, but three channels keep extraction above coordination cost — involuntary exposure entered at actuarial value below exposed populations' willingness to accept, promoter-commissioned inputs with documented optimism, and socialization of construction overruns onto captive ratepayers after approval was granted on flattered ex ante figures. Suppression is low (0.20): counter-framings are legal, published, funded, and litigated; the framework wins by default procedure rather than by silencing critics, and the falling suppression_requirement series records the enforcement ratchet winding down as the method normalized from promoted innovation into administrative default. Theater is low-moderate (0.25) and rising slowly: the calculations are real, but sparse accident data invite precision theater — point estimates that imply more knowledge than exists, sensitivity analyses decorating conclusions reached elsewhere. Accessibility_collapse is low (0.30) because alternative acceptability logics remain fully available to any party that rejects the ledger; resistance is moderate (0.45), concentrated at siting and relicensing contests rather than against the framework itself. All three temporal series run on one shared six-point grid; the suppression_requirement series is authored deliberately because the story tracks enforcement-capacity change (active promotion era decaying into normalization), not merely extraction drift. Receipt surface: the gains demonstrably accrue to the operator seat (approvals convert directly into revenue streams and socialized overruns), so gain_flow names that seat; fixing is prohibitive because the licensing pipeline, administrative-law precedent, capital-market valuation models, and international harmonization all presuppose the ledger, so no seat that could fix it bears a fixable share of the cost.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the operator seat the ledger is fair coordination it helped design: every cost was counted, the number came out favorable, and the plant proceeded. From the host-community seat the same ledger is the machine that converted its involuntary exposure into a figure too small to win an argument it was never permitted to have on its own terms. From the regulator seat the ledger is not a policy at all but the boundary of its own competence — questioning the method reads, from inside, as questioning whether safety can be governed. The engine computes these divergent classifications from the structural data; the divergence between the operator's coordination experience and the hosts' extraction experience is the perspectival fact this story encodes.
 *
 * DIRECTIONALITY LOGIC:
 *   Operators sit near the beneficiary pole: they collect approvals and revenue and control the inputs (d near 0.05, derivable from the beneficiary declaration plus arbitrage exit). Industrial users sit nearest the subsidy end: they receive cheap firm power and hold mobility across jurisdictions, so the arrangement subsidizes them (very low d). Climate policy institutions draw mandate-legitimacy and portfolio feasibility from the framework (low d, overridden to 0.18 to capture the authority-subsidy channel the beneficiary declaration alone underweights). Host communities and downwind residents sit near the full-target pole: their exposure is the priced object, their exit is trapped, and incidental employment or compensation offsets are small (overridden to 0.80 because the derivation tends to over-credit those offsets). Captive ratepayers are genuinely dual-positioned — they consume the product and fund its overruns — so the override places them at net-payer 0.60 rather than letting the victim declaration alone push them to the pole. The regulator is overridden to 0.18: it collects no rents but the framework subsidizes its authority, a benefit channel invisible to a derivation keyed only to the beneficiary and victim arrays.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how to answer 'how safe is safe enough' uniformly, auditably, and defensibly for hazardous infrastructure — remains live, so this is not a mandate outliving its function and no sunset applies. The classification discipline cuts both ways here. Reading the arrangement as pure extraction erases the real comparability gains: without a common ledger, every technology approval collapses into ad hoc political combat, and the historical record shows the framework disciplining both panic-driven prohibition and promoter optimism. Reading it as pure coordination erases the identified cost-bearers: concentrated involuntary exposure priced below acceptance thresholds, captured inputs, and socialized overruns are not coordination overhead. The tangled_rope structure holds both truths — genuine coordination function, asymmetric extraction through the same structure, active enforcement maintaining the licensing pipeline — and prevents either cover story from closing the analysis.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint instantiates only the expected_value_dominant reading of kernel acceptable_risk_for_energy; what structurally changes under the sibling readings catastrophic_tail_dominant and comparative_risk_dominant?',
    'Separate constraint stories instantiate each sibling reading; cross-file comparison of victim sets, epsilon, and suppression locates the disagreement. The disagreement is located in the weighting function applied to low-probability high-consequence events and in whether absolute side-constraints exist alongside the ledger.',
    'Under catastrophic_tail_dominant, nuclear re-enters the victim set, suppression of tail-risk framing rises sharply, and waste disposal reappears as an intergenerational burden rather than a priced engineering line item. Under comparative_risk_dominant, absolute thresholds vanish and acceptability is indexed to competing energy risks such as coal emissions and climate damage.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: this story is one of three readings of the acceptable-risk-for-energy kernel.').

omega_variable(
    wta_vs_actuarial_gap,
    'Does the willingness-to-accept of host communities and downwind populations exceed the actuarial expected value at which their exposure enters the acceptability ledger?',
    'Contingent valuation and revealed-preference studies of siting compensation offers versus actual community acceptance behavior across multiple plant sitings.',
    'If the gap is wide, the ledger systematically underprices involuntary concentrated exposure and effective extraction from the local seats is far higher than the base measure suggests, shifting the computed classification toward the extractive end.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wta_vs_actuarial_gap, empirical, 'Whether expected-value pricing matches what exposed populations would actually accept.').

omega_variable(
    promoter_input_optimism,
    'Are the probability and cost inputs to the acceptability ledger systematically optimistic because the sponsoring operators commission the underlying analyses?',
    'Retrospective comparison of ex ante probabilistic risk assessments and capital cost estimates against ex post operating experience and construction histories across the reactor fleet.',
    'Documented systematic optimism means approvals rest on flattered ledgers and downstream overruns are socialized onto captive ratepayers, raising effective extraction; independent estimation would narrow the gap and support the framework''s fairness claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(promoter_input_optimism, empirical, 'Whether the ledger''s inputs are captured by the parties seeking approval.').

omega_variable(
    social_discount_rate_choice,
    'Which social discount rate legitimately governs long-horizon liabilities (decommissioning, spent-fuel stewardship), and does the chosen rate genuinely internalize those burdens or export them to future cohorts?',
    'Welfare-economic analysis of intergenerational discounting combined with audit of whether collected waste and decommissioning funds track realized liabilities.',
    'Low legitimate discounting implies the framework exports costs across generations and extraction is understated; high discounting supports the reading''s claim that long-horizon burdens are fully priced engineering items.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(social_discount_rate_choice, preference, 'Discount-rate discretion as the hinge of the internalization claim for century-scale liabilities.').

omega_variable(
    regulator_method_identity_lock,
    'Is the safety regulator''s attachment to the expected-value methodology institutional identity fusion (the agency''s authority is constituted by being the competent quantifier) or a revisable merit-based commitment?',
    'Natural experiment from jurisdictions that mandated alternative acceptability frameworks: observe whether the regulator''s legitimacy, staffing, and decision quality collapsed or adapted when the method changed.',
    'If identity-fused, method reform requires external re-legitimation and the framework''s persistence is partly inertial; if merit-based, incremental method pluralism is administratively feasible and the lock dissolves.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulator_method_identity_lock, empirical, 'Identity-lock versus merit commitment in the administering agency.').

omega_variable(
    authority_grounding_framing,
    'Is the framework''s authority best framed as expertise (accredited professional bodies adjudicate method validity) or alternatively as lineage (transmitted from the founding probabilistic risk assessment reports) or practice (the practitioner community''s self-grounding)?',
    'Trace which institutions actually resolve method disputes today: credentialing bodies and standing advisory committees support the expertise framing; citation of founding reports as controlling supports lineage; absence of any external adjudicator supports practice.',
    'Under a lineage framing the drift vector reads as codification_collapse rather than axiom_overriding; under practice framing no interpretive layer exists and drift is unbuffered. The constraint type is robust across framings but the drift diagnosis is not.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_grounding_framing, conceptual, 'Framing under-determination in the commitment-system classification of this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_for_energy__expected_value_dominant, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t0, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 0, 0.12).
narrative_ontology:measurement(acce_tr_t10, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 10, 0.15).
narrative_ontology:measurement(acce_tr_t20, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 20, 0.18).
narrative_ontology:measurement(acce_tr_t30, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 30, 0.21).
narrative_ontology:measurement(acce_tr_t40, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 40, 0.23).
narrative_ontology:measurement(acce_tr_t50, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 50, 0.25).

% Extraction over time
narrative_ontology:measurement(acce_be_t0, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 0, 0.26).
narrative_ontology:measurement(acce_be_t10, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 10, 0.29).
narrative_ontology:measurement(acce_be_t20, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 20, 0.31).
narrative_ontology:measurement(acce_be_t30, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 30, 0.33).
narrative_ontology:measurement(acce_be_t40, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 40, 0.36).
narrative_ontology:measurement(acce_be_t50, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 50, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t0, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(acce_su_t10, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 10, 0.3).
narrative_ontology:measurement(acce_su_t20, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 20, 0.27).
narrative_ontology:measurement(acce_su_t30, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 30, 0.24).
narrative_ontology:measurement(acce_su_t40, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 40, 0.22).
narrative_ontology:measurement(acce_su_t50, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 50, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_for_energy__expected_value_dominant, information_standard).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__expected_value_dominant, acceptable_risk_for_energy__catastrophic_tail_dominant).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__expected_value_dominant, acceptable_risk_for_energy__comparative_risk_dominant).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'acceptable nuclear/energy risk' decomposes into three structurally distinct constraints sharing one kernel. This member (expected_value_dominant) carries the commensuration apparatus — the annualized ledger and the probability-times-consequence weighting — which the comparative_risk_dominant sibling consumes as infrastructure (hence the influences edge: this reading shapes the sibling's operating environment without resolving their dispute). The catastrophic_tail_dominant sibling rejects the weighting premise outright; the two coexist as live positions held by different parties. Each file carries its own epsilon over the same standing arrangement: this reading assesses the ledger as mostly fair pricing with bounded capture channels; the tail sibling assesses the same arrangement as systematically underweighting precisely the events that matter. The epsilon difference is the decomposition's point, not an inconsistency.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(acceptable_risk_for_energy__expected_value_dominant, powerless, 0.8).
constraint_indexing:directionality_override(acceptable_risk_for_energy__expected_value_dominant, moderate, 0.6).
constraint_indexing:directionality_override(acceptable_risk_for_energy__expected_value_dominant, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
