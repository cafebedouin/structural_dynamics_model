% ============================================================================
% CONSTRAINT STORY: climate_response_action__degrowth_transformation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_action__degrowth_transformation, []).

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
 *   constraint_id: climate_response_action__degrowth_transformation
 *   human_readable: Growth-Primacy Climate Response Architecture (Degrowth Transformation Reading)
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This file instantiates the degrowth_transformation reading of the
 *   climate_response_action kernel. The standing arrangement under contest —
 *   the thing the story is about — is the prevailing growth-primacy climate
 *   response order: pledge-and-review treaty architecture, carbon markets and
 *   offset regimes, technology-substitution optimism, and fiscal, trade, and
 *   monetary rules that keep GDP expansion the operative objective of policy.
 *   Epsilon is authored for THAT arrangement, assessed through this reading's
 *   own lights (high: the arrangement transfers the costs of continued
 *   combustion onto parties who did not consent and cannot exit, while its
 *   market machinery generates fees and delay that protect incumbent asset
 *   values). The sibling readings author their own epsilon over the same
 *   referent: mitigation_priority reads the arrangement as substantially
 *   functional coordination (low epsilon), adaptation_priority as misdirected
 *   but good-faith effort (intermediate epsilon). The three files form a
 *   constraint family linked by network.affects_constraints; the disagreement
 *   between them is located in one structural element — whether maintaining
 *   GDP growth is compatible with the required mitigation scale — and is
 *   routed to omega variables rather than averaged into this file's metrics.
 *   KEY AGENTS (by structural relationship): - fossil_fuel_incumbents:
 *   primary beneficiary and agenda-shaper (institutional/arbitrage) —
 *   collects continued demand, shapes market design - major_northern_states:
 *   agenda-setter (institutional/arbitrage) — administers the fiscal and
 *   trade rules holding growth primacy - carbon_market_intermediaries and
 *   growth_dependent_financial_institutions: secondary beneficiaries
 *   (powerful-to-institutional, mobile-to-arbitrage) -
 *   northern_consumer_classes: dual-positioned beneficiary/payer
 *   (organized/constrained) - future_generations: primary payer
 *   (powerless/trapped) — bears the accumulated atmospheric stock, absent
 *   from every room - global_south_climate_exposed_populations: payer with
 *   coalition power (organized/constrained) - fence_line_communities:
 *   localized payer (powerless/trapped) - degrowth_climate_justice_movements:
 *   excluded voice (moderate/constrained) -
 *   integrated_assessment_modeling_community: analytical observer with
 *   identity-locked standing (institutional/identity_locked)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_action__degrowth_transformation, 0.8).
domain_priors:suppression_score(climate_response_action__degrowth_transformation, 0.6).
domain_priors:theater_ratio(climate_response_action__degrowth_transformation, 0.56).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_action__degrowth_transformation, extractiveness, 0.8).
narrative_ontology:constraint_metric(climate_response_action__degrowth_transformation, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(climate_response_action__degrowth_transformation, theater_ratio, 0.56).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_action__degrowth_transformation, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(climate_response_action__degrowth_transformation, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_action__degrowth_transformation, tangled_rope).
narrative_ontology:human_readable(climate_response_action__degrowth_transformation, "Growth-Primacy Climate Response Architecture (Degrowth Transformation Reading)").
narrative_ontology:topic_domain(climate_response_action__degrowth_transformation, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_action__degrowth_transformation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_action__degrowth_transformation, 'cce45757-929f-407a-96d4-1b0745e9d76c').
narrative_ontology:cs_kernel_codification('cce45757-929f-407a-96d4-1b0745e9d76c', fixed_text).
narrative_ontology:cs_authority_grounding('cce45757-929f-407a-96d4-1b0745e9d76c', lineage).
narrative_ontology:cs_interpretation_layer_present('cce45757-929f-407a-96d4-1b0745e9d76c').
narrative_ontology:cs_reading_relation('cce45757-929f-407a-96d4-1b0745e9d76c', climate_response_action__mitigation_priority, forecloses).
narrative_ontology:cs_reading_relation('cce45757-929f-407a-96d4-1b0745e9d76c', climate_response_action__adaptation_priority, coexists_with).
narrative_ontology:cs_axiom('cce45757-929f-407a-96d4-1b0745e9d76c', foundational, gdp_growth_mitigation_scale_incompatible).
narrative_ontology:cs_axiom_status(gdp_growth_mitigation_scale_incompatible, holdable).
narrative_ontology:cs_axiom_grounding('cce45757-929f-407a-96d4-1b0745e9d76c', gdp_growth_mitigation_scale_incompatible, empirically_contingent).
narrative_ontology:cs_axiom('cce45757-929f-407a-96d4-1b0745e9d76c', foundational, sufficiency_equity_lexical_priority).
narrative_ontology:cs_axiom_status(sufficiency_equity_lexical_priority, holdable).
narrative_ontology:cs_axiom_grounding('cce45757-929f-407a-96d4-1b0745e9d76c', sufficiency_equity_lexical_priority, deontological).
narrative_ontology:cs_reference_frame('cce45757-929f-407a-96d4-1b0745e9d76c', biophysical_sufficiency_primacy).
narrative_ontology:cs_drift_state('cce45757-929f-407a-96d4-1b0745e9d76c', contemporary_green_growth_consensus, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('cce45757-929f-407a-96d4-1b0745e9d76c', '').
narrative_ontology:cs_kernel_id(climate_response_action__degrowth_transformation, climate_response_action).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_action__degrowth_transformation, fossil_fuel_incumbents).
narrative_ontology:constraint_beneficiary(climate_response_action__degrowth_transformation, carbon_market_intermediaries).
narrative_ontology:constraint_beneficiary(climate_response_action__degrowth_transformation, northern_consumer_classes).
narrative_ontology:constraint_beneficiary(climate_response_action__degrowth_transformation, growth_dependent_financial_institutions).
narrative_ontology:constraint_victim(climate_response_action__degrowth_transformation, future_generations).
narrative_ontology:constraint_victim(climate_response_action__degrowth_transformation, global_south_climate_exposed_populations).
narrative_ontology:constraint_victim(climate_response_action__degrowth_transformation, fence_line_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_response_action__degrowth_transformation, major_northern_states).
narrative_ontology:constraint_victim(climate_response_action__degrowth_transformation, northern_consumer_classes).
narrative_ontology:constraint_vindicates(climate_response_action__degrowth_transformation, green_growth_absolute_decoupling_hypothesis).
narrative_ontology:constraint_vindicates(climate_response_action__degrowth_transformation, carbon_markets_cost_effectiveness_doctrine).
narrative_ontology:constraint_vindicates(climate_response_action__degrowth_transformation, technological_substitution_optimism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own and operate the reserves, pipelines, refineries, and combustion assets whose continued use the current response architecture accommodates. Revenue depends on demand sustained by growth-first macro policy; lobbying budgets shape the design of carbon markets, offset rules, and subsidy allocation. When rules tighten in one jurisdiction, capital, lobbying effort, and production shift to laxer ones.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, fossil_fuel_incumbents, beneficiary,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_response_action__degrowth_transformation, fossil_fuel_incumbents, agenda_setter).

% Set the fiscal rules, trade agreements, central bank mandates, and development-finance conditions that keep output expansion the operative objective of policy. Announce targets dated beyond electoral horizons, renegotiate or withdraw when compliance conflicts with domestic growth, and collect continued growth and geopolitical weight from the arrangement they administer.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, major_northern_states, agenda_setter,
    institutional, biographical, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(climate_response_action__degrowth_transformation, major_northern_states, beneficiary).

% Broker, verify, register, and exchange allowances and offsets, collecting fees on each transaction. Income scales with market volume regardless of whether traded credits correspond to additional abatement; verification failures in voluntary markets have repeatedly surfaced only after fees were collected.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, carbon_market_intermediaries, beneficiary,
    powerful, immediate, arbitrage, global).

% Hold balance sheets collateralized on continued expansion: real estate, sovereign debt serviced by growth revenues, corporate earnings projections. Climate stress tests notwithstanding, solvency models assume the growth trajectory continues, and lending flows reward projects consistent with it.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, growth_dependent_financial_institutions, beneficiary,
    institutional, biographical, arbitrage, global).

% Receive continued affluence, inexpensive goods, and employment from growth-first policy, and vote accordingly; carbon price increases have been reversed at the ballot box. They also carry rising insurance costs, heat and storm exposure, and the taxes funding adaptation, and cannot individually opt out of the growth economy their livelihoods run on.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, northern_consumer_classes, beneficiary,
    organized, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(climate_response_action__degrowth_transformation, northern_consumer_classes, payer).

% Will inhabit the atmosphere and oceans the present arrangement is filling. They appear in no negotiating room, hold no assets, cast no votes, and can neither consent to nor refuse the risks accumulating on their behalf; their interests enter only through proxy advocates and litigation brought in their name.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, future_generations, payer,
    powerless, civilizational, trapped, universal).

% Negotiate as the G77-plus-China bloc and secured a loss-and-damage fund after three decades of insistence, yet remain constrained: adaptation finance arrives largely as loans, development finance is conditioned on growth-compatible pathways, and their exports face carbon border adjustments designed elsewhere. Least responsible for cumulative emissions, most exposed to the resulting damage.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, global_south_climate_exposed_populations, payer,
    organized, generational, constrained, continental).

% Live adjacent to refineries, petrochemical corridors, and sacrifice zones, and in flood- and heat-exposed housing stock. Geographic immobility and local economic dependency bind them to facilities the current response schedule keeps running through mid-century transition timelines.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, fence_line_communities, payer,
    powerless, biographical, trapped, local).

% Organize for sufficiency, working-time reduction, and reparative climate finance. They publish, march, litigate, and stand for election, but hold no seats in the central banks, trade bodies, and fiscal councils where the growth objective is actually set; mainstream platforms and coverage treat their premise as outside the discussable range.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, degrowth_climate_justice_movements, excluded,
    moderate, generational, constrained, global).

% Produce the cost-benefit pathways, social cost of carbon figures, and decoupling scenarios through which governments calibrate ambition. Careers, journals, advisory chairs, and model codebases are built within the growth-compatible optimization frame; the discipline's standing rests on the frame's continued use, and dissenting members tend to publish outside its flagship venues.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, integrated_assessment_modeling_community, observer,
    institutional, generational, identity_locked, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_action__degrowth_transformation, fossil_fuel_incumbents).
narrative_ontology:fixing_cost_class(climate_response_action__degrowth_transformation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global climate action through price signals, market instruments, and voluntary pledge-and-review: channels investment toward lower-carbon technologies and builds common measurement and reporting infrastructure, while leaving the allocation of remaining carbon space unallocated and existing growth structures intact.
% TRANSFER_FUNCTION: Moves the costs of climate destabilization — accumulated atmospheric stock, adaptation burdens, stranded futures — from present Northern consumers and incumbent industries onto future generations and climate-exposed Southern populations, while moving subsidy flows, market fees, and preserved asset values toward incumbents and intermediaries.
% ABSENT_VOICES: Future generations have no seat anywhere in the architecture. Loss-and-damage claims from the Global South were structurally subordinated in agenda rules for three decades. Sufficiency and post-growth advocates are excluded from the central banks, trade bodies, and fiscal councils where the growth premise is actually set. Fence-line communities lack standing in the permitting processes that extend facility lifetimes.
% DISAPPEARANCE_RATIONALE: If the growth-primacy architecture vanished overnight, the fiscal mandates, trade rules, and market machinery referencing it would lose their object; asset valuations collateralized on continued expansion would reset; subsidy streams and carbon flows would reorganize around whatever objective replaced output growth. Concrete institutions depend on it, so the world rearranges.
% FOUNDING_PROBLEM: Assembled after the Kyoto binding-allocation failure and the Copenhagen breakdown to answer: how can emissions fall fast enough without halting growth or triggering distributional conflict — recasting decarbonization as a market-design problem rather than an allocation problem.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the benefiting parties: UNEP Emissions Gap reports document the persistent annual shortfall between the architecture and required rates; IPCC mitigation chapters document the gap between pledge trajectories and 1.5-2°C pathways; Global South negotiating statements attest the distributional failure. No source outside the benefiting parties attests that the founding problem — decarbonization without growth conflict — has been or can be solved by this architecture; the incumbency attests only that markets are scaling.
narrative_ontology:disappearance_verdict(climate_response_action__degrowth_transformation, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_action__degrowth_transformation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_action__degrowth_transformation, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_response_action__degrowth_transformation, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_action__degrowth_transformation, 0.8, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_action__degrowth_transformation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_action__degrowth_transformation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_action__degrowth_transformation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.80) because the arrangement's dominant material flow is uncompensated cost transfer: the atmospheric stock accumulates on parties who cannot decline it, while carbon-market fees and preserved incumbent asset values are collected in the present. It is not higher because the arrangement also delivers real mitigation value — renewable deployment and measurement infrastructure are genuine. Suppression (0.60) is discursive and institutional rather than coercive: sufficiency alternatives are marginalized through agenda exclusion, funding gates, and trade and fiscal lock-ins, not prohibited; this is a raw structural property and is deliberately left unscaled by power or scope. Theater ratio (0.56) reflects the pledge-announcement-offset cycle outrunning delivered mitigation — the UNEP emissions-gap record is the observable — while real capital expenditure continues underneath. Accessibility collapse is moderate (0.40): once the arrangement's structure is seen, alternatives do not fully collapse — post-growth politics remains articulable and increasingly organized — but within official policy channels the alternative space is largely foreclosed. Resistance (0.68) is substantial: climate justice movements, youth strikes, litigation, ballot-box reversals of carbon prices, and the Global South negotiating bloc. The measurement series run on one shared eight-point grid (1997-2025) so every tracked metric is authored at every examined time point; the rising suppression_requirement series tracks the maturation of enforcement machinery (monitoring regimes, border adjustments, disclosure rules, protest policing) and is authored because enforcement-capacity change is part of this story's dynamics.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary/agenda seats compute differently from the same structure. Future generations experience the arrangement as unconditional imposition — they cannot even decline participation, so no coordination surplus ever reaches them. Fence-line and Southern exposed populations experience negotiated-but-losing positions. Northern consumer classes experience a tolerable bargain: affluence now, diffuse costs later. Incumbents and financial institutions experience opportunity. The integrated-assessment community observes orderly optimization, because its professional standing is constituted inside the frame it observes. The engine computes these divergent per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation, so no directionality overrides are needed. Fossil incumbents, market intermediaries, and financial institutions are declared beneficiaries with arbitrage-grade exit — they sit nearest the beneficiary end, and effective burden on them dampens toward subsidy. Major Northern states are agenda-setters who also collect growth benefits — strongly beneficiary-side despite administering the arrangement. Future generations are declared victims with trapped exit — the canonical full-target seat; nothing moderates their position because they cannot move, consent, or bargain. Southern exposed populations are victims with organized coalition power: their directionality sits near the target end, but coalition capacity (the loss-and-damage victory at COP27) partially moderates realized burden — the coalition check for low-power victims is answered by their actual bloc behavior. Fence-line communities are victims with trapped exit at local scope. Northern consumer classes are dual-positioned (declared beneficiary with secondary payer role): genuine coordination benefit, real indirect cost-bearing — they land mid-range rather than at either pole. The modeling community is nominally an observer, but its identity-locked exit ties its standing to the arrangement's continuation, pulling it slightly beneficiary-side of symmetric.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim prevents two opposite errors. Accepting the arrangement's self-description as pure market coordination would erase the asymmetric burden structure — the transfer to future generations and the Global South is not a coordination cost, it is the arrangement's principal material flow. Condemning it as pure extraction would erase the real deployment function, whose loss would worsen physical outcomes; the correct remedy for a hybrid is repair of the asymmetry, not demolition of the coordination. On the genealogy interview: the founding problem (decarbonize without growth conflict) is contested rather than dead, so no mandatrophy resolution is declared; the mismatch consumer reads contested-status against a world_rearranges verdict and finds no zombie flag. The identity-lock dynamic is concentrated in the modeling community: professional identity fusion — careers, journals, and codebases constituted by the growth-compatible frame — means the frame's collapse would dissolve members' standing, which is why internal dissent migrates to peripheral venues. If that identity frame broke, the epistemic pillar of the arrangement's legitimacy would weaken faster than its fiscal pillars.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading (degrowth_transformation) of the climate_response_action kernel; what would the sibling readings change structurally, and where exactly is the disagreement located?',
    'Comparative analysis across the three family files: mitigation_priority keeps GDP growth as organizing principle and routes response through markets and innovation; adaptation_priority accepts temperature rise and redirects investment to resilience. The disagreement is located in the growth-economy premise — its compatibility with required mitigation scale — not in the reality of climate destabilization, which all three readings accept.',
    'Classification is reading-indexed: the same standing arrangement computes as functional coordination under mitigation_priority''s low epsilon and as substantially extractive under this reading''s high epsilon. Cross-reading comparison is valid only through the family links, never by averaging epsilons.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: one reading of a three-reading kernel; sibling deltas and disagreement location.').

omega_variable(
    absolute_decoupling_sufficiency,
    'Does absolute decoupling of GDP from emissions and resource throughput exist at the scale and rate the required mitigation pathways demand?',
    'Long-run national and sectoral accounting comparing consumption-based emissions and material footprint against GDP growth, tested against the carbon budgets implied by 1.5-2°C pathways; the decisive test is whether any major economy has sustained both at required rates for a full decade.',
    'If sufficient decoupling is demonstrated, this reading''s foundational empirical axiom weakens and mitigation_priority regains its footing; if not, the standing arrangement''s organizing premise collapses and this reading''s high extraction assessment is confirmed as structural rather than rhetorical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(absolute_decoupling_sufficiency, empirical, 'The load-bearing empirical question beneath the growth-compatibility dispute.').

omega_variable(
    feasibility_vs_agenda_exclusion,
    'How much of the degrowth reading''s political marginalization reflects genuine infeasibility versus incumbent-defended agenda exclusion?',
    'Natural experiments from jurisdictions experimenting with post-growth instruments (working-time reduction trials, universal basic services pilots, sufficiency-based planning) compared against jurisdictions where such instruments never reach the agenda; differential polling on policy content versus policy labeling.',
    'If marginalization tracks demonstrated infeasibility, the measured suppression is a rational filter; if comparable policies fail to advance where popular support exists, the suppression is capture-driven and the arrangement''s enforcement machinery is defending the growth premise itself, pushing the computed classification toward the snare boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(feasibility_vs_agenda_exclusion, empirical, 'Whether the suppression of sufficiency alternatives is necessity or defense.').

omega_variable(
    intra_north_burden_equity,
    'When this reading shifts mitigation burden from future generations to current wealthy populations, does the burden actually land on the wealthy, or does it diffuse onto precarious households within wealthy nations?',
    'Distributional incidence analysis of specific instruments (carbon rationing, working-time reduction, universal basic services funding) across income deciles within Northern economies; the design of universal basic services is the pivotal variable.',
    'If incidence is progressive, the reading''s equity claim holds and its asymmetry is corrective; if incidence is flat or regressive, the reading reproduces the distributional harm it criticizes in a new form, and its beneficiary/victim structure requires re-authoring before its classifications can be trusted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intra_north_burden_equity, preference, 'Whether the reading''s burden shift is a justice correction or a relocated imposition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_action__degrowth_transformation, 1997, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t1997, climate_response_action__degrowth_transformation, theater_ratio, 1997, 0.3).
narrative_ontology:measurement_basis(clim_tr_t1997, observed).
narrative_ontology:measurement(clim_tr_t2002, climate_response_action__degrowth_transformation, theater_ratio, 2002, 0.34).
narrative_ontology:measurement_basis(clim_tr_t2002, observed).
narrative_ontology:measurement(clim_tr_t2007, climate_response_action__degrowth_transformation, theater_ratio, 2007, 0.38).
narrative_ontology:measurement_basis(clim_tr_t2007, observed).
narrative_ontology:measurement(clim_tr_t2012, climate_response_action__degrowth_transformation, theater_ratio, 2012, 0.42).
narrative_ontology:measurement_basis(clim_tr_t2012, observed).
narrative_ontology:measurement(clim_tr_t2017, climate_response_action__degrowth_transformation, theater_ratio, 2017, 0.46).
narrative_ontology:measurement_basis(clim_tr_t2017, observed).
narrative_ontology:measurement(clim_tr_t2020, climate_response_action__degrowth_transformation, theater_ratio, 2020, 0.5).
narrative_ontology:measurement_basis(clim_tr_t2020, observed).
narrative_ontology:measurement(clim_tr_t2023, climate_response_action__degrowth_transformation, theater_ratio, 2023, 0.54).
narrative_ontology:measurement_basis(clim_tr_t2023, observed).
narrative_ontology:measurement(clim_tr_t2025, climate_response_action__degrowth_transformation, theater_ratio, 2025, 0.56).
narrative_ontology:measurement_basis(clim_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(clim_be_t1997, climate_response_action__degrowth_transformation, base_extractiveness, 1997, 0.6).
narrative_ontology:measurement_basis(clim_be_t1997, observed).
narrative_ontology:measurement(clim_be_t2002, climate_response_action__degrowth_transformation, base_extractiveness, 2002, 0.64).
narrative_ontology:measurement_basis(clim_be_t2002, observed).
narrative_ontology:measurement(clim_be_t2007, climate_response_action__degrowth_transformation, base_extractiveness, 2007, 0.68).
narrative_ontology:measurement_basis(clim_be_t2007, observed).
narrative_ontology:measurement(clim_be_t2012, climate_response_action__degrowth_transformation, base_extractiveness, 2012, 0.71).
narrative_ontology:measurement_basis(clim_be_t2012, observed).
narrative_ontology:measurement(clim_be_t2017, climate_response_action__degrowth_transformation, base_extractiveness, 2017, 0.74).
narrative_ontology:measurement_basis(clim_be_t2017, observed).
narrative_ontology:measurement(clim_be_t2020, climate_response_action__degrowth_transformation, base_extractiveness, 2020, 0.76).
narrative_ontology:measurement_basis(clim_be_t2020, observed).
narrative_ontology:measurement(clim_be_t2023, climate_response_action__degrowth_transformation, base_extractiveness, 2023, 0.78).
narrative_ontology:measurement_basis(clim_be_t2023, observed).
narrative_ontology:measurement(clim_be_t2025, climate_response_action__degrowth_transformation, base_extractiveness, 2025, 0.8).
narrative_ontology:measurement_basis(clim_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t1997, climate_response_action__degrowth_transformation, suppression_requirement, 1997, 0.3).
narrative_ontology:measurement_basis(clim_su_t1997, observed).
narrative_ontology:measurement(clim_su_t2002, climate_response_action__degrowth_transformation, suppression_requirement, 2002, 0.33).
narrative_ontology:measurement_basis(clim_su_t2002, observed).
narrative_ontology:measurement(clim_su_t2007, climate_response_action__degrowth_transformation, suppression_requirement, 2007, 0.38).
narrative_ontology:measurement_basis(clim_su_t2007, observed).
narrative_ontology:measurement(clim_su_t2012, climate_response_action__degrowth_transformation, suppression_requirement, 2012, 0.42).
narrative_ontology:measurement_basis(clim_su_t2012, observed).
narrative_ontology:measurement(clim_su_t2017, climate_response_action__degrowth_transformation, suppression_requirement, 2017, 0.48).
narrative_ontology:measurement_basis(clim_su_t2017, observed).
narrative_ontology:measurement(clim_su_t2020, climate_response_action__degrowth_transformation, suppression_requirement, 2020, 0.52).
narrative_ontology:measurement_basis(clim_su_t2020, observed).
narrative_ontology:measurement(clim_su_t2023, climate_response_action__degrowth_transformation, suppression_requirement, 2023, 0.57).
narrative_ontology:measurement_basis(clim_su_t2023, observed).
narrative_ontology:measurement(clim_su_t2025, climate_response_action__degrowth_transformation, suppression_requirement, 2025, 0.6).
narrative_ontology:measurement_basis(clim_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_action__degrowth_transformation, resource_allocation).
narrative_ontology:affects_constraint(climate_response_action__degrowth_transformation, climate_response_action__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_action__degrowth_transformation, climate_response_action__adaptation_priority).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'climate response' covers three structurally distinct governing claims that the climate_response_action kernel holds together. This file instantiates degrowth_transformation; climate_response_action__mitigation_priority and climate_response_action__adaptation_priority instantiate the siblings. All three author epsilon over the SAME referent — the standing growth-primacy climate response arrangement — with reading-indexed values: this reading authors high epsilon (the arrangement as a cost-transfer machine protecting growth), mitigation_priority authors low epsilon (the arrangement as functional market coordination), adaptation_priority authors intermediate epsilon (good-faith effort aimed at the wrong object). The upstream/downstream structure runs from mitigation_priority (highest empirical confidence, treaty-anchored) toward this reading (most contested, feasibility-barred); this reading's foundational axiom directly contradicts mitigation_priority's and is recorded as a foreclosure edge in cs_structure.reading_relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
