% ============================================================================
% CONSTRAINT STORY: acceptable_risk_for_energy__catastrophic_tail_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_for_energy__catastrophic_tail_dominant, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: acceptable_risk_for_energy__catastrophic_tail_dominant
 *   human_readable: Catastrophic Tail-Dominant Risk Acceptability Standard
 *   domain: risk governance/energy policy/public safety
 *
 * SUMMARY:
 *   This story authors ONE reading of the acceptable_risk_for_energy kernel:
 *   the catastrophic_tail_dominant reading, under which a low-probability,
 *   high-consequence event dominates the risk calculus, irreversibility and
 *   intergenerational burden operate as binding conditions rather than priced
 *   quantities, and aggregate expected-value arguments are inadmissible at
 *   the acceptability stage. Instantiated, this is the governance regime in
 *   which nuclear deployment is gated behind worst-case demonstrations,
 *   probabilistic trade-off framing is policed out of proceedings, and waste
 *   disposal functions as a standing veto rather than an engineering program.
 *   The claim and the metrics are independent authored facts: claimed_type is
 *   tangled_rope because I believe the structure genuinely coordinates
 *   (bounded siting conflict, protection of unconsenting parties) while
 *   genuinely extracting (asymmetric burden on the nuclear side and on
 *   ratepayers); the metrics record what I believe descriptively true of its
 *   operation. Epsilon's referent is the standing tail-dominant arrangement
 *   itself, assessed by this reading's own lights, not the arrangement any
 *   sibling reading would install. The sibling readings,
 *   expected_value_dominant and comparative_risk_dominant, are separate
 *   constraints in separate files, linked through
 *   network.affects_constraints; they are not averaged, hedged, or folded
 *   into this one.
 *
 * KEY AGENTS:
 *   - - nuclear_operators_and_developers: primary target (organized/trapped) — carries tail-sized compliance, decommissioning, and stewardship liabilities with ruinous exit
 *   - - electricity_ratepayers: diffuse target (powerless/trapped) — pays the tariff consequences with coalition potential only through intermediaries
 *   - - precautionary_regulatory_agencies: agenda setter and collector (institutional/identity_locked) — administers the gate and derives mandate, staffing, and legitimacy from it
 *   - - fossil_fuel_incumbents: indirect beneficiary (powerful/arbitrage) — preserved dispatch share and asset lives while nuclear expansion was gated
 *   - - renewable_energy_industries: market beneficiary (organized/arbitrage) — sells the substitute capacity the gate makes necessary
 *   - - anti_nuclear_advocacy_networks: movement beneficiary (organized/identity_locked) — collects legitimacy and agenda power; exit dissolves identity
 *   - - host_community_veto_holders: protected beneficiary with residual exposure (moderate/trapped) — holds blocking voice, lives with the facility
 *   - - probabilistic_risk_analysts: internal target (moderate/constrained) — employed for plant-level analysis while barred from applying their headline method
 *   - - climate_policy_institutions: dual-positioned party (institutional/constrained) — pays in decarbonization slippage while endorsing tail logic for climate itself
 *   - - expected_value_energy_economists: excluded voice (moderate/mobile) — holds a rival reading, ruled inadmissible in governed proceedings
 *   - - future_generations: claimed client, no seat (non-agent) — represented by proxies with divergent incentives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.64).
domain_priors:suppression_score(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.78).
domain_priors:theater_ratio(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, extractiveness, 0.64).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_for_energy__catastrophic_tail_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_for_energy__catastrophic_tail_dominant, "Catastrophic Tail-Dominant Risk Acceptability Standard").
narrative_ontology:topic_domain(acceptable_risk_for_energy__catastrophic_tail_dominant, "risk governance/energy policy/public safety").

domain_priors:requires_active_enforcement(acceptable_risk_for_energy__catastrophic_tail_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_for_energy__catastrophic_tail_dominant, 'fef88a19-2b69-4451-8eef-e8b026c477ca').
narrative_ontology:cs_kernel_codification('fef88a19-2b69-4451-8eef-e8b026c477ca', distributed).
narrative_ontology:cs_authority_grounding('fef88a19-2b69-4451-8eef-e8b026c477ca', lineage).
narrative_ontology:cs_interpretation_layer_present('fef88a19-2b69-4451-8eef-e8b026c477ca').
narrative_ontology:cs_reading_relation('fef88a19-2b69-4451-8eef-e8b026c477ca', acceptable_risk_for_energy__expected_value_dominant, forecloses).
narrative_ontology:cs_reading_relation('fef88a19-2b69-4451-8eef-e8b026c477ca', acceptable_risk_for_energy__comparative_risk_dominant, forecloses).
narrative_ontology:cs_axiom('fef88a19-2b69-4451-8eef-e8b026c477ca', foundational, catastrophic_tails_outweigh_expected_value).
narrative_ontology:cs_axiom_status(catastrophic_tails_outweigh_expected_value, holdable).
narrative_ontology:cs_axiom_grounding('fef88a19-2b69-4451-8eef-e8b026c477ca', catastrophic_tails_outweigh_expected_value, deontological).
narrative_ontology:cs_axiom('fef88a19-2b69-4451-8eef-e8b026c477ca', foundational, irreversibility_binds_independent_of_probability).
narrative_ontology:cs_axiom_status(irreversibility_binds_independent_of_probability, holdable).
narrative_ontology:cs_axiom_grounding('fef88a19-2b69-4451-8eef-e8b026c477ca', irreversibility_binds_independent_of_probability, deontological).
narrative_ontology:cs_axiom('fef88a19-2b69-4451-8eef-e8b026c477ca', secondary, probabilistic_tradeoff_framing_inadmissible).
narrative_ontology:cs_axiom_status(probabilistic_tradeoff_framing_inadmissible, holdable).
narrative_ontology:cs_axiom_grounding('fef88a19-2b69-4451-8eef-e8b026c477ca', probabilistic_tradeoff_framing_inadmissible, conventional).
narrative_ontology:cs_axiom('fef88a19-2b69-4451-8eef-e8b026c477ca', secondary, zero_risk_admissibility_standard).
narrative_ontology:cs_axiom_status(zero_risk_admissibility_standard, overridden).
narrative_ontology:cs_axiom_grounding('fef88a19-2b69-4451-8eef-e8b026c477ca', zero_risk_admissibility_standard, conventional).
narrative_ontology:cs_reference_frame('fef88a19-2b69-4451-8eef-e8b026c477ca', irreversibility_threshold_precedence).
narrative_ontology:cs_drift_state('fef88a19-2b69-4451-8eef-e8b026c477ca', contemporary_cost_benefit_encroachment, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('fef88a19-2b69-4451-8eef-e8b026c477ca', '').
narrative_ontology:cs_kernel_id(acceptable_risk_for_energy__catastrophic_tail_dominant, acceptable_risk_for_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__catastrophic_tail_dominant, anti_nuclear_advocacy_networks).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__catastrophic_tail_dominant, host_community_veto_holders).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__catastrophic_tail_dominant, renewable_energy_industries).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__catastrophic_tail_dominant, fossil_fuel_incumbents).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__catastrophic_tail_dominant, precautionary_regulatory_agencies).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__catastrophic_tail_dominant, nuclear_operators_and_developers).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__catastrophic_tail_dominant, electricity_ratepayers).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__catastrophic_tail_dominant, probabilistic_risk_analysts).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__catastrophic_tail_dominant, climate_policy_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__catastrophic_tail_dominant, probabilistic_risk_analysts).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__catastrophic_tail_dominant, climate_policy_institutions).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__catastrophic_tail_dominant, host_community_veto_holders).
narrative_ontology:constraint_vindicates(acceptable_risk_for_energy__catastrophic_tail_dominant, precautionary_principle_doctrine).
narrative_ontology:constraint_vindicates(acceptable_risk_for_energy__catastrophic_tail_dominant, intergenerational_equity_obligation).
narrative_ontology:constraint_vindicates(acceptable_risk_for_energy__catastrophic_tail_dominant, irreversibility_avoidance_norm).
narrative_ontology:constraint_vindicates(acceptable_risk_for_energy__catastrophic_tail_dominant, defense_in_depth_philosophy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Transnational campaign organizations and grassroots networks whose defining purpose is preventing nuclear deployment and catastrophic radioactive release. They receive legitimacy, membership, funding, and agenda-setting influence whenever the acceptability gate treats worst-case outcomes as decisive. Leaving the frame would mean dissolving the movement's core commitment, so their position in it is fused with their identity.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, anti_nuclear_advocacy_networks, beneficiary,
    organized, generational, identity_locked, global).

% Residents of candidate and existing host regions whose consent structures, hearings, referenda, and compensation negotiations give them blocking voice over siting and relicensing. They gain veto power and negotiated compensation, and they live with residual facility presence, transport routes, and land-value stigma. They cannot relocate away from the question because their homes are the site.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, host_community_veto_holders, beneficiary,
    moderate, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_for_energy__catastrophic_tail_dominant, host_community_veto_holders, payer).

% Wind, solar, storage, and grid-services firms whose market position improves when firm nuclear expansion is gated behind worst-case demonstrations they are never asked to pass. They sell the substitute capacity and benefit from preference flows calibrated to nuclear's constrained supply. Their positions are diversified enough to profit under several policy frames.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, renewable_energy_industries, beneficiary,
    organized, biographical, arbitrage, global).

% Coal and gas generator, mining, and fuel-delivery interests that faced displacement by mid-century nuclear buildout programs. Gating new nuclear behind catastrophic-outcome demonstrations slowed that displacement for decades, preserving dispatch share, fuel demand, and asset lives. The benefit is indirect and contingent: it accrues only while the frame dominates, and these actors would remain profitable under alternative acceptability rules as well.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, fossil_fuel_incumbents, beneficiary,
    powerful, biographical, arbitrage, global).

% Safety regulators that administer tail-dominant licensing: they set core-damage-frequency targets, impose defense-in-depth and severe-accident requirements, gate waste disposal on demonstrations of permanent isolation, and police which risk arguments are admissible in proceedings. Their mandate, staffing, budget, and public legitimacy derive from the frame they administer. Political principals supervise them, but the agencies' institutional purpose would dissolve if the frame were abandoned.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, precautionary_regulatory_agencies, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_for_energy__catastrophic_tail_dominant, precautionary_regulatory_agencies, beneficiary).

% Utilities, vendors, and investors carrying compliance, severe-accident, decommissioning, and waste-stewardship liabilities sized by tail assumptions rather than central estimates. Sunk reactor capital, license-specific designs, and stranded-cost recovery obligations make withdrawal ruinous; their realistic options are absorbing the burden, litigating for frame relief, or passing costs forward.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, nuclear_operators_and_developers, payer,
    organized, biographical, trapped, continental).

% Households and businesses paying tariffs shaped by foregone nuclear scale economies, stranded-cost recovery, premium-priced substitute generation, and long stewardship charges. No individual household has a meaningful voice; influence runs only through consumer-advocacy intermediaries, and no one can opt out of the grid.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, electricity_ratepayers, payer,
    powerless, biographical, trapped, national).

% Probabilistic safety assessment professionals employed inside the regime to compute core damage frequencies, source terms, and severe-accident sequences. Their tools are used extensively at the plant level, while their headline method, aggregating probability times consequence into an acceptability verdict, is ruled out of order at the acceptability stage. Careers, clearances, and publication venues depend on staying inside the frame that bars their central claim.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, probabilistic_risk_analysts, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_for_energy__catastrophic_tail_dominant, probabilistic_risk_analysts, beneficiary).

% Assessment bodies, energy agencies, and decarbonizing ministries pursuing emissions timetables. They bear firm-power shortfalls and schedule slippage when nuclear capacity is gated, while at the same time endorsing the same tail-precedence logic for climate tipping risks. They are philosophically invested in the standard that taxes their own instrument set, and their mandates do not permit open repudiation of either side.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, climate_policy_institutions, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_for_energy__catastrophic_tail_dominant, climate_policy_institutions, beneficiary).

% Energy economists whose acceptability analyses multiply probability by consequence and compare across hazards. In tail-governed proceedings their testimony is ruled inadmissible as a category error, their papers circulate past the venues that decide licensing, and their citations come mainly from the industry the frame constrains. They retain academic mobility and publish freely outside regulatory arenas.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, expected_value_energy_economists, excluded,
    moderate, biographical, mobile, global).

% The unborn cohorts in whose name the arrangement acts: waste-isolation obligations run for millennia and avoided catastrophic exposures are banked on their behalf. They hold no seat anywhere and are represented by proxy advocates whose institutional incentives diverge from the people they stand in for. Kept for narrative completeness; not an actor that collects anything.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, future_generations, beneficiary,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(acceptable_risk_for_energy__catastrophic_tail_dominant, future_generations).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(acceptable_risk_for_energy__catastrophic_tail_dominant, fossil_fuel_incumbents).
narrative_ontology:fixing_cost_class(acceptable_risk_for_energy__catastrophic_tail_dominant, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, enforceable answer to a genuine collective-action problem: no single community asked to host a catastrophic-capable facility can bargain alone against a developer, and no present generation can negotiate with future ones. A common acceptability standard converts unbounded siting warfare into structured proceedings and places a bound on the catastrophic risks any party may impose on unconsenting others.
% TRANSFER_FUNCTION: Moves decision authority over catastrophic-risk acceptance away from probabilistic aggregation and toward threshold-and-veto structures held by regulators and host communities; transfers economic cost in the form of compliance burdens, foregone generation capacity, stranded investments, and perpetual stewardship liabilities onto nuclear operators, ratepayers, and the pace of decarbonization, in exchange for bounded worst-case exposure.
% ABSENT_VOICES: Holders of the aggregate-expected-cost and compare-across-hazards readings are present in society but marginalized in formal proceedings, where their method is ruled inadmissible. Energy-poor households bearing tariff effects have no dedicated seat and surface only through diffuse consumer intermediaries. Future generations, the frame's claimed clients, are literally absent and represented by proxies with divergent incentives.
% DISAPPEARANCE_RATIONALE: If the tail-dominant acceptability gate vanished overnight, licensing regimes would reorganize around whichever sibling standard won, moratoria and post-accident closures would face immediate legal challenge, waste programs would reframe from permanence-demonstration vetoes to engineering-with-residual-risk projects, siting conflicts would convert into compensation bargaining, and the generation mix, tariff paths, and decarbonization schedules of every jurisdiction using the frame would shift within a decade.
% FOUNDING_PROBLEM: How should societies decide whether to deploy technologies whose failure modes are rare but regionally devastating and whose residues remain hazardous for timescales exceeding any institution's life, given that ordinary markets and probability-weighted averages systematically undervalue such tails and that the people bearing the tail, and the people bearing the residue centuries later, cannot consent?
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set by nuclear engineers and regulators themselves, whose defense-in-depth architecture exists precisely because they attest the tails are real; by actuaries and reinsurers, who price low-probability high-severity events as first-class objects; and by intergenerational-ethics scholarship unrelated to advocacy movements. What is NOT corroborated outside the beneficiary set is the dominance resolution itself, namely that tails must categorically outrank aggregation; holders of the rival readings attest the founding problem is real while disputing this file's answer to it.
narrative_ontology:disappearance_verdict(acceptable_risk_for_energy__catastrophic_tail_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_for_energy__catastrophic_tail_dominant, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_for_energy__catastrophic_tail_dominant, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(acceptable_risk_for_energy__catastrophic_tail_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.64, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_for_energy__catastrophic_tail_dominant_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(acceptable_risk_for_energy__catastrophic_tail_dominant, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(acceptable_risk_for_energy__catastrophic_tail_dominant_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness ends at 0.64 because the gate is deliberately expensive for the nuclear side and the costs are real even by this reading's own lights: the reading intends some of that burden, which caps how damning the number is, but the foregone decarbonization, stranded capital, and perpetual stewardship charges are not intended and register fully. Suppression is 0.78 and is authored as a RAW STRUCTURAL PROPERTY, unscaled by power or scope; only extractiveness gets scaled downstream. The suppression is mostly structural (proceedings rules, licensing gates, admissibility rulings) with a growing internalized component (professionals self-censor comparative arguments as career-imprudent), a split carried in the omega. Theater is 0.42: licensing and closure enforcement are functionally real, but a rising share of activity is process without convergence, predrafted consultations, and review rituals whose outcomes are known. The temporal series run on ONE shared grid, six points at ten-year spacing (roughly 1975 to 2025), every tracked metric authored at every point. The dynamics are cyclical ratchets, not monotone drift: each catastrophe (TMI near t=10, Chernobyl near t=20, Fukushima between t=30 and t=40) produces a step-up in enforcement intensity and extractive burden; between shocks the frame relaxes (visible at t=30 during the renaissance easing) but the relaxation never returns to the prior baseline. The oscillation is partly an extraction mechanism in the intermittent-reinforcement sense: each shock re-legitimates the frame and deepens it, so cycles compound rather than cancel. Base_properties values are the end-state readings at t=50, measured at a post-Fukushima plateau, not at the cycle trough.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently and the divergence is structural, not rhetorical. From the operator seat the gate is a liability machine sized by tail assumptions with no exit; from the host-community seat the same gate is the only reason their consent means anything; from the regulator seat it is the mandate that constitutes the agency; from the fossil seat it is quiet market protection they never had to ask for; from the PRA-analyst seat it is a workplace that employs their tools while outlawing their conclusion; from the advocacy seat it is a hard-won moral boundary that must not soften. Same structure, opposite lived types. The engine derives these per-seat classifications from the power, exit, and role data; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries derive toward the subsidized end: the regulator (agenda_setter plus beneficiary, institutional, identity-locked by mission fusion) sits low; advocacy networks (identity-locked, generational) lower still; host communities sit low despite their secondary exposure because the veto right dominates their ledger; renewables sit low with an arbitrage cushion. Targets derive toward the full-target end: operators (trapped, sunk capital) and ratepayers (trapped, no exit from the grid) sit high; PRA analysts sit moderately high, damped by the employment their inside position provides. One explicit override: fossil_fuel_incumbents carry power atom 'powerful', unique in this story, with d overridden to 0.35. The structural derivation from their beneficiary declaration would place them deep in subsidized territory near the advocacy networks, but their gain is indirect and contingent, they profit under rival frames too, and they never lobbied this gate into existence, so the honest d sits well above the derived value. The override is keyed to their unique power atom precisely because overrides resolve by power atom and a shared atom would leak the correction onto unintended seats. Climate_policy_institutions take no override: their dual payer/beneficiary position should already derive mid-range, which is the honest reading, and the commentary records the expectation so divergence stays visible.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live, so nothing here is mandatrophy-resolved and no zombie flag should fire: status=live combined with disappearance_verdict=world_rearranges is the consistent pairing. The classification discipline matters in both directions for this constraint. Read without the coordination half, the gate looks like a pure snare: an absolute veto on one technology, enforced by admissibility policing, with identifiable payers. Read without the extraction half, it looks like a pure rope: a shared standard protecting everyone from imposed catastrophe. Both misreads erase real structure. The tangled-rope claim keeps the genuine collective-action solution (bounded siting war, intergenerational obligation made enforceable) and the genuine asymmetric transfer (tail-sized liabilities, suppressed methodology, protected incumbents) on the same ledger, where the engine can price them together. The theater trajectory bears watching: if the disposal veto continues converting from standard into ritual while enforcement intensity keeps climbing, the structure drifts piton-ward, and the temporal series exists to catch exactly that turn.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This file instantiates only the catastrophic_tail_dominant reading of the acceptable_risk_for_energy kernel; which reading actually governs a jurisdiction''s acceptability determination is the primary contested variable, and the siblings (expected_value_dominant, comparative_risk_dominant) instantiate structurally different constraints with different victim sets and different epsilon. Where exactly is this reading operative rather than merely proclaimed?',
    'Jurisdictional decision-rule audits: examine licensing statutes, hearing admissibility rulings, and waste-program mandates to establish which decision-rule actually binds in each governing body, as opposed to which rule officials invoke rhetorically.',
    'If expected_value_dominant governs in a jurisdiction, nuclear exits the victim set and waste reframes as engineering with residual risk; if comparative_risk_dominant governs, the victim set narrows to absolute worst performers. This story''s classification holds only where the tail-dominant rule actually binds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one reading of a three-reading kernel; disagreement located in the acceptability decision-rule axiom.').

omega_variable(
    symmetric_tail_application_counterfactual,
    'If the tail-dominance rule were applied with full symmetry to all catastrophic-capable energy hazards, including fossil air-pollution mortality chains and climate tipping cascades, would the constraint''s net effect on future generations invert, turning fossil incumbents from incidental beneficiaries into primary targets?',
    'Counterfactual analysis applying this reading''s own thresholds (worst credible case, irreversibility, intergenerational duration) to the fossil externality chain and comparing resulting acceptability verdicts with the actual asymmetric application.',
    'Symmetric application would flip the extraction target from the nuclear side to the fossil side, reversing the directionality of the incumbent beneficiary and potentially converting the arrangement''s practical operation into something closer to its stated principle; the current asymmetric application is load-bearing for the beneficiary structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symmetric_tail_application_counterfactual, conceptual, 'Whether the reading''s victim set is an artifact of selective application rather than of the principle itself.').

omega_variable(
    intergenerational_client_verification,
    'Do the cohorts the frame claims as clients actually net-benefit, or does the arrangement impose on them the very intergenerational burden it cites as justification, through deferred and underfunded stewardship obligations, decommissioning debts rolled forward, and decarbonization delay that compounds climate damages across the same future generations?',
    'Longitudinal audit of waste-fund adequacy and stewardship-institution survival rates against counterfactual disposal pathways, paired with integrated assessment of decarbonization-delay damages attributable to constrained firm low-carbon supply.',
    'If the net intergenerational ledger is negative, the beneficiary declaration running in future generations'' name inverts in substance, the coordination half of the tangled-rope claim weakens, and the structure shifts toward extraction riding a protective cover story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_client_verification, empirical, 'Whether the claimed client of the constraint is served or burdened by it.').

omega_variable(
    frame_suppression_mechanism_split,
    'Is the suppression of probabilistic trade-off framing structural (admissibility rulings, proceeding rules, licensing gates) or internalized (analysts and journalists treating comparative arguments as morally disreputable and career-imprudent even where no rule bars them), and in what proportion?',
    'Speech-trajectory study in venues where formal frame-policing relaxed: if comparative-risk and expected-value arguments resurface promptly once rules permit, suppression was structural; if professionals continue avoiding them, an internalized component persists after barrier removal.',
    'To the extent suppression is internalized, effective suppression exceeds the structural measure and would survive deregulatory reform, meaning frame change requires normative turnover rather than rule change alone, with correspondingly slower classification dynamics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(frame_suppression_mechanism_split, empirical, 'Structural versus internalized mechanism behind the measured frame suppression.').

omega_variable(
    disposal_veto_vs_engineering_standard,
    'Is demonstrated-permanent-disposal a satisfiable engineering requirement that a competent program could meet, or a structurally unsatisfiable standard functioning purely as a veto, as the reading''s critics contend when they observe that no siting has succeeded regardless of technical merit?',
    'Operating-history evidence: licensed repositories in service for decades with maintained social license (Onkalo-class facilities) would show the standard satisfiable; repeated siting failures across technically strong candidates regardless of geology would confirm veto function.',
    'If the standard is satisfiable, the disposal component is genuine coordination and the tangled-rope claim stands; if it is unsatisfiable by construction, the disposal gate is a veto wearing an engineering costume, and the extraction component of this story is larger than the base measure indicates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disposal_veto_vs_engineering_standard, empirical, 'Whether the waste-disposal requirement is an achievable standard or an unsatisfiable veto.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_for_energy__catastrophic_tail_dominant, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t0, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(acce_tr_t0, observed).
narrative_ontology:measurement(acce_tr_t10, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 10, 0.28).
narrative_ontology:measurement_basis(acce_tr_t10, observed).
narrative_ontology:measurement(acce_tr_t20, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 20, 0.34).
narrative_ontology:measurement_basis(acce_tr_t20, observed).
narrative_ontology:measurement(acce_tr_t30, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 30, 0.31).
narrative_ontology:measurement_basis(acce_tr_t30, observed).
narrative_ontology:measurement(acce_tr_t40, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 40, 0.38).
narrative_ontology:measurement_basis(acce_tr_t40, observed).
narrative_ontology:measurement(acce_tr_t50, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 50, 0.42).
narrative_ontology:measurement_basis(acce_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(acce_be_t0, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 0, 0.44).
narrative_ontology:measurement_basis(acce_be_t0, observed).
narrative_ontology:measurement(acce_be_t10, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 10, 0.5).
narrative_ontology:measurement_basis(acce_be_t10, observed).
narrative_ontology:measurement(acce_be_t20, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 20, 0.57).
narrative_ontology:measurement_basis(acce_be_t20, observed).
narrative_ontology:measurement(acce_be_t30, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 30, 0.52).
narrative_ontology:measurement_basis(acce_be_t30, observed).
narrative_ontology:measurement(acce_be_t40, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 40, 0.6).
narrative_ontology:measurement_basis(acce_be_t40, observed).
narrative_ontology:measurement(acce_be_t50, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 50, 0.64).
narrative_ontology:measurement_basis(acce_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t0, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(acce_su_t0, observed).
narrative_ontology:measurement(acce_su_t10, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 10, 0.58).
narrative_ontology:measurement_basis(acce_su_t10, observed).
narrative_ontology:measurement(acce_su_t20, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(acce_su_t20, observed).
narrative_ontology:measurement(acce_su_t30, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 30, 0.6).
narrative_ontology:measurement_basis(acce_su_t30, observed).
narrative_ontology:measurement(acce_su_t40, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 40, 0.74).
narrative_ontology:measurement_basis(acce_su_t40, observed).
narrative_ontology:measurement(acce_su_t50, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 50, 0.78).
narrative_ontology:measurement_basis(acce_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_for_energy__catastrophic_tail_dominant, enforcement_mechanism).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__catastrophic_tail_dominant, acceptable_risk_for_energy__expected_value_dominant).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__catastrophic_tail_dominant, acceptable_risk_for_energy__comparative_risk_dominant).

% DUAL FORMULATION NOTE:
% The colloquial label 'acceptable risk for energy' conflates three structurally distinct constraints distinguished by their acceptability decision-rule: this file (catastrophic_tail_dominant, absolute tail-precedence, epsilon 0.64, nuclear-side victims), acceptable_risk_for_energy__expected_value_dominant (aggregation decides, epsilon tracks the probability-consequence product, no categorical victims), and acceptable_risk_for_energy__comparative_risk_dominant (relativist, victims only absolute worst performers). Each has its own epsilon, beneficiary/victim structure, and classification. The upstream-downstream pattern runs from this reading outward: tail-dominance is the historically entrenched frame whose operation supplies the evidence and legitimacy contests the siblings fight over, and its network edges record that influence without merging the constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(acceptable_risk_for_energy__catastrophic_tail_dominant, powerful, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
