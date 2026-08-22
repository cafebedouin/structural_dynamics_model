% ============================================================================
% CONSTRAINT STORY: climate_harm_prevention__adaptation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_harm_prevention__adaptation_priority, []).

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
 *   constraint_id: climate_harm_prevention__adaptation_priority
 *   human_readable: Adaptation-First Climate Response Doctrine
 *   domain: climate policy/political economy/intergenerational ethics
 *
 * SUMMARY:
 *   The arrangement under contest: legitimate climate response is defined as
 *   near-term resilience building, on the grounds that deep mitigation is
 *   politically and economically infeasible, and a higher warming trajectory
 *   is accepted as the price of present protection. Adaptation finance flows
 *   to visible protection for currently exposed populations; the emissions
 *   path that sets the hazard level is left substantially unchanged; the
 *   residual damages accumulate on parties with no seat in the allocation.
 *   This story instantiates ONE reading of the climate_harm_prevention
 *   kernel. The sibling readings (mitigation_priority, degrowth_reading) are
 *   separate constraint files with their own epsilon, beneficiary structures,
 *   and classifications; this file does not describe the contest or average
 *   across readings. KEY AGENTS (by structural relationship):
 *   national_governments: agenda setter (institutional/constrained) — sets
 *   the split, collects political credit; incumbent_high_emission_industries:
 *   primary structural beneficiary (institutional/arbitrage) — receives
 *   deferred-transition rents; present_vulnerable_populations: intended
 *   beneficiary (moderate/trapped) — receives resilience investment;
 *   adaptation_infrastructure_contractors: incidental beneficiary
 *   (institutional/mobile) — collects adaptation revenue; future_generations:
 *   primary target (powerless/trapped) — bears locked-in warming costs;
 *   low_adaptation_capacity_regions: secondary target (moderate/constrained)
 *   — bears uncompensated residual damages; youth_climate_justice_advocates:
 *   excluded voice (organized/identity_locked); climate_science_community:
 *   analytical observer (institutional/analytical).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_harm_prevention__adaptation_priority, 0.5).
domain_priors:suppression_score(climate_harm_prevention__adaptation_priority, 0.55).
domain_priors:theater_ratio(climate_harm_prevention__adaptation_priority, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, extractiveness, 0.5).
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_harm_prevention__adaptation_priority, tangled_rope).
narrative_ontology:human_readable(climate_harm_prevention__adaptation_priority, "Adaptation-First Climate Response Doctrine").
narrative_ontology:topic_domain(climate_harm_prevention__adaptation_priority, "climate policy/political economy/intergenerational ethics").

domain_priors:requires_active_enforcement(climate_harm_prevention__adaptation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_harm_prevention__adaptation_priority, 'c0a88f5b-81e7-4644-865a-fb171687e761').
narrative_ontology:cs_kernel_codification('c0a88f5b-81e7-4644-865a-fb171687e761', fixed_text).
narrative_ontology:cs_authority_grounding('c0a88f5b-81e7-4644-865a-fb171687e761', distributed).
narrative_ontology:cs_reading_relation('c0a88f5b-81e7-4644-865a-fb171687e761', climate_harm_prevention__mitigation_priority, influences).
narrative_ontology:cs_reading_relation('c0a88f5b-81e7-4644-865a-fb171687e761', climate_harm_prevention__degrowth_reading, coexists_with).
narrative_ontology:cs_axiom('c0a88f5b-81e7-4644-865a-fb171687e761', foundational, feasibility_bounds_obligation).
narrative_ontology:cs_axiom_status(feasibility_bounds_obligation, holdable).
narrative_ontology:cs_axiom_grounding('c0a88f5b-81e7-4644-865a-fb171687e761', feasibility_bounds_obligation, instrumental).
narrative_ontology:cs_axiom('c0a88f5b-81e7-4644-865a-fb171687e761', foundational, present_identifiable_suffering_outranks_future_statistical_harm).
narrative_ontology:cs_axiom_status(present_identifiable_suffering_outranks_future_statistical_harm, holdable).
narrative_ontology:cs_axiom_grounding('c0a88f5b-81e7-4644-865a-fb171687e761', present_identifiable_suffering_outranks_future_statistical_harm, deontological).
narrative_ontology:cs_reference_frame('c0a88f5b-81e7-4644-865a-fb171687e761', near_term_resilience_imperative).
narrative_ontology:cs_drift_state('c0a88f5b-81e7-4644-865a-fb171687e761', contemporary_net_zero_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('c0a88f5b-81e7-4644-865a-fb171687e761', '').
narrative_ontology:cs_kernel_id(climate_harm_prevention__adaptation_priority, climate_harm_prevention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_harm_prevention__adaptation_priority, present_vulnerable_populations).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__adaptation_priority, adaptation_infrastructure_contractors).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__adaptation_priority, incumbent_high_emission_industries).
narrative_ontology:constraint_victim(climate_harm_prevention__adaptation_priority, future_generations).
narrative_ontology:constraint_victim(climate_harm_prevention__adaptation_priority, low_adaptation_capacity_regions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__adaptation_priority, national_governments).
narrative_ontology:constraint_vindicates(climate_harm_prevention__adaptation_priority, mitigation_infeasibility_premise).
narrative_ontology:constraint_vindicates(climate_harm_prevention__adaptation_priority, resilience_substitution_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set national climate budgets and choose the split between resilience programs and emissions-reduction programs. Visible protection projects generate credit within electoral cycles, while avoided warming benefits arrive after office ends; the fiscal cost of adaptation lands on current budgets, and the cost of the warming trajectory the split implies lands elsewhere. Exit from the arrangement means electoral risk, since opponents campaign on visible protection.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, national_governments, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(climate_harm_prevention__adaptation_priority, national_governments, beneficiary).

% Channel international adaptation finance according to allocation criteria they administer. They publish needs assessments and gap reports, disburse to eligible projects, and report on delivery. Their leverage stops at the criteria: they cannot compel contributors to close the gap between pledged and delivered amounts, nor redirect the energy-policy choices that determine the trajectory being adapted to.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, multilateral_adaptation_finance_bodies, agenda_setter,
    institutional, generational, constrained, global).

% Live in places already committed to near-term climate exposure: coastlines, floodplains, drought belts, heat-stressed cities. They receive seawalls, early-warning systems, resilient crops, and cooling centers. Moving away from exposed land is often economically impossible, so protection arrives where they stand or not at all. Their say over how much protection they get, and over the emissions choices that set the hazard level, is mediated entirely by others.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, present_vulnerable_populations, beneficiary,
    moderate, biographical, trapped, global).

% Engineering, construction, water-management, and insurance firms that design and build resilience projects and price climate risk. Adaptation budgets are their revenue line, and demand grows with every increment of committed warming regardless of its source. They hold no position on the emissions path and can pursue contracts across jurisdictions.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, adaptation_infrastructure_contractors, beneficiary,
    institutional, biographical, mobile, global).

% Producers and heavy users of fossil energy whose assets depreciate over decades. Every year in which the policy center treats deep mitigation as off the table is a year of deferred stranding risk and uninterrupted cash flow; the feasibility premise, once established, retires the regulatory scenario their valuations fear most. They defend the premise indirectly, through lobbying and funded research aimed at the premise itself rather than at any particular protection project.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, incumbent_high_emission_industries, beneficiary,
    institutional, generational, arbitrage, global).

% Will inhabit whatever trajectory present allocation locks in. They hold no vote, no asset, and no seat in any budget process; their claim is carried by proxy advocates at best. Each year of adaptation-first allocation converts their option set, a stabilizable climate, into a fixed inheritance of higher seas, hotter extremes, and compounding damages, with no compensating asset transferred to them.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, future_generations, payer,
    powerless, civilizational, trapped, global).

% Governments and populations with high exposure and thin fiscal capacity. They receive a fraction of assessed adaptation needs through international flows, conditionally and late, while bearing the residual damages the accepted trajectory delivers. Exiting the arrangement is not available because the hazards arrive regardless, and their formal seats in climate negotiations carry little weight against contributor-state budget politics.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, low_adaptation_capacity_regions, payer,
    moderate, biographical, constrained, regional).

% Organized movements whose members will live deepest into the locked-in trajectory. They litigate, march, and contest elections, and their members' identities are fused with the cause of changing the allocation before it hardens. They are consulted in symbolic forums but are absent from the finance ministries and budget committees where the split is actually decided.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, youth_climate_justice_advocates, excluded,
    organized, generational, identity_locked, global).

% Produces the trajectory assessments, attribution studies, and impact projections the entire allocation debate consumes. Documents the widening gap between stated temperature goals and the path implied by the current policy mix. Holds no allocation authority; its findings enter the process as inputs that decision-makers may weigh or shelve.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, climate_science_community, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_harm_prevention__adaptation_priority, incumbent_high_emission_industries).
narrative_ontology:fixing_cost_class(climate_harm_prevention__adaptation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Pools public and international finance to build resilience for populations already committed to near-term climate exposure, solving the genuine collective-action problem of protecting present vulnerable people from harms that past emissions have made unavoidable on any political timescale.
% TRANSFER_FUNCTION: Moves present fiscal resources toward visible near-term protection, and, through the feasibility premise, relieves present emitters of transition costs; the offsetting movement is intertemporal and geographic, shifting the costs of the accepted warming trajectory onto future generations and onto regions lacking adaptation capital, neither of which participates in the allocation.
% ABSENT_VOICES: Future generations have no seat anywhere in the process; low-adaptation-capacity regions hold formal negotiating seats but almost no leverage over how feasibility is defined; youth and climate-justice advocates are consulted symbolically but excluded from the budget committees where the split is decided. Their standing objection, that infeasibility is a choice rather than a fact, never enters the accounting.
% DISAPPEARANCE_RATIONALE: If the adaptation-first arrangement vanished overnight, resilience finance flows would stop, the unprotected exposure of present vulnerable populations would become an explicit political emergency, incumbent emitters would lose the feasibility cover that defers stranding risk, and governments would have to choose openly between resumed mitigation and open abandonment; the intergenerational cost transfer would need defenders rather than running silently inside budget lines.
% FOUNDING_PROBLEM: After decades of stalled mitigation, policymakers confronted escalating near-term climate damages among populations that could not wait for emission cuts to take effect; the adaptation-first arrangement was built to deliver protection on political timescales once prevention had failed politically, with mitigation's infeasibility asserted as the premise licensing the priority.
% FOUNDING_PROBLEM_CORROBORATION: The harm side is corroborated from outside the beneficiary set: IPCC AR6 Working Group II and UNEP Adaptation Gap reporting attest that committed warming makes near-term adaptation need live and growing. The infeasibility premise is attested mainly by the arrangement's own architects and beneficiaries; independent energy-economics sources (declining renewable cost curves, Stern-lineage reviews, IEA deployment analyses) dispute it, arguing mitigation remains technically and economically available and that infeasibility is political and chosen. Corroboration therefore splits by component: live need corroborated, licensing premise contested.
narrative_ontology:disappearance_verdict(climate_harm_prevention__adaptation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_harm_prevention__adaptation_priority, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_harm_prevention__adaptation_priority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_harm_prevention__adaptation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_harm_prevention__adaptation_priority, 0.5, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_harm_prevention__adaptation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_harm_prevention__adaptation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_harm_prevention__adaptation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.50 at interval end: the arrangement genuinely subsidizes present vulnerable populations (damping extraction on that side) while transferring the costs of the accepted trajectory onto absent parties (amplifying it on the other); the reading's own lights acknowledge the transfer as the price of feasibility, so the self-assessed value sits mid-range rather than at snare levels. Suppression is 0.55 and is a raw structural property, unscaled by power or scope: it operates discursively and procedurally (agenda control via the feasibility framing, exclusion of alternatives from budget processes), not through physical coercion, and partly through internalized fatalism in younger cohorts (see omega). Theater ratio 0.30: most adaptation spending builds real assets, but a growing minority of activity is resilience rhetoric functioning as a substitute for prevention talk. Accessibility collapse 0.38: the alternative orderings (mitigation-first, degrowth) remain fully visible and argued; they are priced out politically rather than rendered unthinkable, so understanding the arrangement does not collapse the alternatives. Resistance 0.60: sustained movements, litigation, and small-island-state diplomacy actively contest the priority ordering. The measurement series run on one shared time grid (points 0-30 step 6) with every tracked metric authored at every point; base_extractiveness rises as accumulated delay compounds the transferred burden, theater_ratio rises as resilience rhetoric thickens, and suppression_requirement rises because each year of delay raises the political cost of reversal, requiring harder maintenance of the feasibility consensus. The dynamics are monotonic drift, not cyclical; no intermittent-reinforcement mechanism is claimed. The claimed type (tangled_rope) is authored from structure, the metrics from description, independently.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute very different arrangements from the same structure. From the agenda-setter seat this is responsible stewardship under hard constraints; from the contractor seat it is demand growth; from the incumbent seat it is regulatory calm; from the present-vulnerable seat it is partial protection arriving late; from the future-generations seat, computable only through proxies, it is pure cost imposition with no offsetting transfer received; from the scientific seat it is a widening gap between stated goals and the implied trajectory. The youth-advocate seat carries identity_lock: members' self-concept is constituted through the cause, so exit is unthinkable while the allocation stands; if the frame broke (for example, an abrupt cheap-abatement breakthrough dissolving the feasibility premise), the movement would convert from a fused identity into an ordinary constituency bargaining over implementation speed.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: present_vulnerable_populations are subsidized directly; contractors collect revenue with mobile exit, sitting nearest the beneficiary end among economic actors; incumbents benefit indirectly but massively through deferred stranding risk, so their derived d sits low despite participating in no adaptation spending. Victim declarations drive high directionality: future_generations are maximally trapped (no exit is even conceivable) and thus sit nearest the full-target end; low_adaptation_capacity_regions are constrained by finance dependence, and their partial compensation through adaptation flows damps their d somewhat below the future-generations seat (uncertainty carried in the compensation-adequacy omega). National governments are genuinely dual-positioned: administrators collecting political credit within electoral horizons while bearing fiscal cost, with the net tilt toward the beneficiary side set by the biographical time horizon; the derivation handles this through the dual role rather than an override. Multilateral finance bodies administer criteria without capturing surpluses; the science community observes analytically.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification blocks two symmetrical mislabels. Reading the arrangement as pure rope celebrates the real protection of present vulnerable populations while rendering the silent intergenerational and geographic transfer invisible; reading it as pure snare dismisses genuine resilience assets as cover and erases the populations they protect. On mandatrophy: the protective mandate is live, damages are growing, so the arrangement's core function has not atrophied and no piton reading is warranted; what is contested is the feasibility premise that licenses the transfer. If mitigation costs keep falling, the premise erodes and the arrangement drifts toward extraction-with-thinning-cover; if the premise holds, the arrangement remains a tragic but defensible hybrid. The R5 mismatch consumer reads founding_problem_status=contested against disappearance_verdict=world_rearranges: no dead-mandate flag fires, correctly, because the function is live even while its justification is disputed. Coalition dynamics: low_adaptation_capacity_regions retain coalition potential (vulnerable-states blocs), which is the principal countervailing power inside the arrangement; future_generations cannot coalition at all, having no agency, which is why their seat computes nearest full-target and their protection depends entirely on proxy advocacy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_distributive_underdetermination,
    'Is adaptation-priority a faithful instantiation of the shared climate_harm_prevention kernel, or a redistributive redefinition that relocates harm rather than preventing it?',
    'Comparative classification across the three sibling stories: if the readings disagree primarily about who bears residual harm rather than about how to prevent it, the kernel is under-determined at the distributive level and each reading constitutes a distinct constraint rather than a variant of one.',
    'If the readings are distributively distinct, this story''s victim structure is an artifact of the adaptation_priority reading and a sibling-authored assessment of the same arrangement would differ sharply; if the kernel is robust, the victim structure is intrinsic to any feasible climate response.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_distributive_underdetermination, conceptual, 'Whether the kernel fixes a distributive rule or leaves victim allocation to the reading.').

omega_variable(
    feasibility_premise_endogeneity,
    'Is mitigation''s political and economic infeasibility an exogenous fact binding this arrangement, or is it endogenously produced by the interests the arrangement shelters?',
    'Counterfactual policy analysis: jurisdictions that sustained aggressive mitigation (cost trajectories, political survival rates), renewable cost-decline data versus deployment gaps, and lobbying-expenditure studies correlating feasibility discourse with affected industries.',
    'If infeasibility is manufactured, the arrangement''s coordination cover thins and its classification shifts toward pure extraction with the protection function as cover; if exogenous, the residual-cost transfer is a forced tradeoff rather than captured rent, and the tangled_rope reading is stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(feasibility_premise_endogeneity, conceptual, 'Whether the feasibility premise is a binding constraint or a produced belief.').

omega_variable(
    implicit_discount_rate_status,
    'The arrangement prices future welfare implicitly through its allocation choices; is the operative discount rate a defensible ethical parameter or an extraction instrument?',
    'Explicit discount-rate deliberation in the Stern-Nordhaus lineage, plus sensitivity analysis of how the adaptation/mitigation allocation ranking changes across the defensible rate range.',
    'A steep implicit rate confirms the intergenerational transfer as extraction; a modest rate recasts the arrangement as tragic triage under genuine scarcity, damping victim-seat extraction estimates.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(implicit_discount_rate_status, preference, 'Status of the implicit discount rate embedded in the allocation.').

omega_variable(
    adaptation_finance_compensation_adequacy,
    'Do adaptation finance flows to low-adaptation-capacity regions materially offset the residual damages they bear under the accepted trajectory, or are they token relative to assessed losses?',
    'Audit of delivered adaptation finance against modeled residual damages (UNEP Adaptation Gap methodology), disaggregated by recipient region.',
    'Substantial compensation damps the victim-seat directionality for low-capacity regions; token flows confirm near-full-target positioning and sharpen the asymmetric-extraction reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptation_finance_compensation_adequacy, empirical, 'Whether the finance component offsets the residual burden it accompanies.').

omega_variable(
    internalized_feasibility_fatalism,
    'Does the feasibility doctrine operate partly through internalized fatalism among younger cohorts, such that suppression of the mitigation alternative persists independently of active enforcement?',
    'Longitudinal cohort attitude surveys around major policy shocks, particularly abrupt mitigation-cost breakthroughs: if deferral acceptance persists after the structural barrier falls, the internalized component is real.',
    'An internalized component raises effective suppression above the structural measure and would slow any post-reversal rearrangement, extending the arrangement''s persistence beyond its enforcement machinery.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internalized_feasibility_fatalism, empirical, 'Structural versus internalized share of the suppression maintaining the priority ordering.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_harm_prevention__adaptation_priority, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_harm_prevention__adaptation_priority, theater_ratio, 0, 0.22).
narrative_ontology:measurement(clim_tr_t6, climate_harm_prevention__adaptation_priority, theater_ratio, 6, 0.24).
narrative_ontology:measurement(clim_tr_t12, climate_harm_prevention__adaptation_priority, theater_ratio, 12, 0.25).
narrative_ontology:measurement(clim_tr_t18, climate_harm_prevention__adaptation_priority, theater_ratio, 18, 0.27).
narrative_ontology:measurement(clim_tr_t24, climate_harm_prevention__adaptation_priority, theater_ratio, 24, 0.29).
narrative_ontology:measurement(clim_tr_t30, climate_harm_prevention__adaptation_priority, theater_ratio, 30, 0.3).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_harm_prevention__adaptation_priority, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(clim_be_t6, climate_harm_prevention__adaptation_priority, base_extractiveness, 6, 0.43).
narrative_ontology:measurement(clim_be_t12, climate_harm_prevention__adaptation_priority, base_extractiveness, 12, 0.46).
narrative_ontology:measurement(clim_be_t18, climate_harm_prevention__adaptation_priority, base_extractiveness, 18, 0.48).
narrative_ontology:measurement(clim_be_t24, climate_harm_prevention__adaptation_priority, base_extractiveness, 24, 0.49).
narrative_ontology:measurement(clim_be_t30, climate_harm_prevention__adaptation_priority, base_extractiveness, 30, 0.5).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_harm_prevention__adaptation_priority, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(clim_su_t6, climate_harm_prevention__adaptation_priority, suppression_requirement, 6, 0.45).
narrative_ontology:measurement(clim_su_t12, climate_harm_prevention__adaptation_priority, suppression_requirement, 12, 0.47).
narrative_ontology:measurement(clim_su_t18, climate_harm_prevention__adaptation_priority, suppression_requirement, 18, 0.5).
narrative_ontology:measurement(clim_su_t24, climate_harm_prevention__adaptation_priority, suppression_requirement, 24, 0.53).
narrative_ontology:measurement(clim_su_t30, climate_harm_prevention__adaptation_priority, suppression_requirement, 30, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_harm_prevention__adaptation_priority, resource_allocation).
narrative_ontology:affects_constraint(climate_harm_prevention__adaptation_priority, climate_harm_prevention__mitigation_priority).
narrative_ontology:affects_constraint(climate_harm_prevention__adaptation_priority, climate_harm_prevention__degrowth_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'legitimate climate response' covers three structurally distinct claims and is authored as three linked stories sharing the climate_harm_prevention kernel. mitigation_priority is the historically upstream reading (its prevention logic anchors the treaty architecture); this adaptation_priority reading diverts finance and attention from it, creating the influences edge. degrowth_reading shares this reading's impossibility premise while rejecting its growth-framework response, hence coexistence without logical elimination. Each story authors its own epsilon over its own endorsed arrangement; no averaging across readings occurs in any file.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
