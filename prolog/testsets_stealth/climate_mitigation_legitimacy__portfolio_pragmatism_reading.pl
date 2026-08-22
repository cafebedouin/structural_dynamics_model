% ============================================================================
% CONSTRAINT STORY: climate_mitigation_legitimacy__portfolio_pragmatism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_legitimacy__portfolio_pragmatism_reading, []).

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
 *   constraint_id: climate_mitigation_legitimacy__portfolio_pragmatism_reading
 *   human_readable: Climate Mitigation Legitimacy - Portfolio Pragmatism Reading (Technology-Neutral Nuclear+Renewables Portfolio)
 *   domain: economic/political/technological
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the climate_mitigation_legitimacy
 *   kernel: the claim that optimal decarbonization requires a
 *   technology-neutral portfolio containing both nuclear and renewables. The
 *   constraint operates as a governing norm on climate-mitigation capital
 *   allocation, subsidy eligibility, planning regimes, and coalition
 *   structure. It solves a real coordination problem (technology-risk
 *   hedging, coalition maintenance under uncertainty) while running
 *   asymmetric extraction through the same structure: guaranteed inclusion
 *   converts into subsidized returns for nuclear-adjacent incumbents, the
 *   cost premium lands diffusely on ratepayers, and the frame's
 *   'reasonableness' disciplines rival readings out of agenda control. The
 *   claim and the metrics are authored independently: the reading presents
 *   itself as fair optimization, and the metrics below describe its actual
 *   operation, including the rent channels its own holders do not emphasize.
 *   KEY AGENTS (by structural relationship): -
 *   energy_ministries_and_grid_planners: Agenda-setter
 *   (institutional/arbitrage) — writes eligibility rules and procurement
 *   shares - nuclear_industry_coalition: Primary beneficiary
 *   (institutional/constrained) — collects subsidized returns on guaranteed
 *   inclusion - diversified_utility_holdcos, large_epc_contractors: Secondary
 *   beneficiaries (institutional/arbitrage) — asset-class protection and
 *   overrun-insulated fee streams - renewable_energy_sector: Dual-positioned
 *   (organized/mobile) — included beneficiary paying in agenda control and
 *   subsidy share - electricity_ratepayers, renewable_first_developers:
 *   Primary targets (powerless/trapped; moderate/constrained) — bear the cost
 *   premium and crowding-out - fossil_fuel_incumbents: Adversarial free-rider
 *   (powerful/arbitrage) — harvests neutrality rhetoric short-term, pays in
 *   displaced generation long-term - integrated_assessment_modelers:
 *   Identity-locked knowledge beneficiary (institutional) — operationalizes
 *   the frame they inhabit - degrowth_sufficiency_advocates: Excluded voice —
 *   objects to the question, not the answer - energy_systems_analysts:
 *   Analytical observer — sees the full structure
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.52).
domain_priors:suppression_score(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.46).
domain_priors:theater_ratio(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 0.46).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__portfolio_pragmatism_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_legitimacy__portfolio_pragmatism_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_legitimacy__portfolio_pragmatism_reading, "Climate Mitigation Legitimacy - Portfolio Pragmatism Reading (Technology-Neutral Nuclear+Renewables Portfolio)").
narrative_ontology:topic_domain(climate_mitigation_legitimacy__portfolio_pragmatism_reading, "economic/political/technological").

domain_priors:requires_active_enforcement(climate_mitigation_legitimacy__portfolio_pragmatism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_legitimacy__portfolio_pragmatism_reading, '3adae842-e12f-49b0-ac5c-a5de657c359d').
narrative_ontology:cs_kernel_codification('3adae842-e12f-49b0-ac5c-a5de657c359d', distributed).
narrative_ontology:cs_authority_grounding('3adae842-e12f-49b0-ac5c-a5de657c359d', distributed).
narrative_ontology:cs_reading_relation('3adae842-e12f-49b0-ac5c-a5de657c359d', climate_mitigation_legitimacy__baseload_necessity_reading, influences).
narrative_ontology:cs_reading_relation('3adae842-e12f-49b0-ac5c-a5de657c359d', climate_mitigation_legitimacy__renewable_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('3adae842-e12f-49b0-ac5c-a5de657c359d', climate_mitigation_legitimacy__degrowth_sufficiency_reading, coexists_with).
narrative_ontology:cs_axiom('3adae842-e12f-49b0-ac5c-a5de657c359d', foundational, no_technology_privileged_a_priori).
narrative_ontology:cs_axiom_status(no_technology_privileged_a_priori, holdable).
narrative_ontology:cs_axiom_grounding('3adae842-e12f-49b0-ac5c-a5de657c359d', no_technology_privileged_a_priori, instrumental).
narrative_ontology:cs_axiom('3adae842-e12f-49b0-ac5c-a5de657c359d', foundational, regional_optimal_mix_variation).
narrative_ontology:cs_axiom_status(regional_optimal_mix_variation, holdable).
narrative_ontology:cs_axiom_grounding('3adae842-e12f-49b0-ac5c-a5de657c359d', regional_optimal_mix_variation, empirically_contingent).
narrative_ontology:cs_reference_frame('3adae842-e12f-49b0-ac5c-a5de657c359d', technology_neutral_optimal_portfolio).
narrative_ontology:cs_drift_state('3adae842-e12f-49b0-ac5c-a5de657c359d', contemporary_post_cop28_nuclear_revival, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3adae842-e12f-49b0-ac5c-a5de657c359d', '').
narrative_ontology:cs_kernel_id(climate_mitigation_legitimacy__portfolio_pragmatism_reading, climate_mitigation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, nuclear_industry_coalition).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, diversified_utility_holdcos).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, large_epc_contractors).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__portfolio_pragmatism_reading, electricity_ratepayers).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__portfolio_pragmatism_reading, renewable_first_developers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, renewable_energy_sector).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, fossil_fuel_incumbents).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__portfolio_pragmatism_reading, integrated_assessment_modelers).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__portfolio_pragmatism_reading, renewable_energy_sector).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__portfolio_pragmatism_reading, renewable_primacy_advocates).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__portfolio_pragmatism_reading, fossil_fuel_incumbents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Write integrated resource plans, set subsidy eligibility categories, approve capacity mechanisms and plant siting, and draft the technology-inclusion language in climate framework laws. They decide what counts as an admissible low-carbon technology in procurement and what share of public capital each category may access. They can redefine eligibility categories in the next legislative cycle, though doing so breaks coalition commitments built over decades.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, energy_ministries_and_grid_planners, agenda_setter,
    institutional, generational, arbitrage, national).

% Vendor firms, operators, and trade bodies whose products and plants gain guaranteed inclusion in any portfolio framed as technology-neutral. They receive subsidized returns, cost-pass-through arrangements, and loan guarantees on projects with chronic overruns; revenue lands on their balance sheets regardless of whether their technology wins on open system cost. Their assets, workforce skills, and regulatory licenses are nuclear-specific, so leaving the field means writing off sunk capital and dismantling a professional community.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, nuclear_industry_coalition, beneficiary,
    institutional, generational, constrained, global).

% Large holding companies owning generation across every admissible category. A mandate that the portfolio must contain both nuclear and renewables guarantees their asset classes stay financeable and protects them from stranding in either direction. They hedge internally: whichever way the technology contest resolves, they hold winning assets, and they can rebalance holdings faster than the policy cycle moves.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, diversified_utility_holdcos, beneficiary,
    institutional, generational, arbitrage, continental).

% Engineering-procurement-construction firms that build large centralized plants under cost-plus or overrun-shared contracts. Portfolio mandates that require megaproject construction generate fee streams insulated from completion risk, because delay and overrun trigger renegotiation rather than penalty. They can redeploy crews and balance sheets across energy infrastructure categories.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, large_epc_contractors, beneficiary,
    institutional, biographical, arbitrage, global).

% Established wind, solar, and storage manufacturers and developers who are guaranteed a place in the portfolio and receive the largest absolute share of deployment capital. At the same time, the portfolio frame costs them: finite subsidy pools are split with firm-power categories, their political agenda is diluted into coalition-broadening compromise, and their central claim (that their technology plus storage suffices) is formally demoted to one option among several. They operate in global markets and can shift geographic focus when national frames turn hostile.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, renewable_energy_sector, beneficiary,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_legitimacy__portfolio_pragmatism_reading, renewable_energy_sector, payer).

% Smaller pure-play developers without nuclear-adjacent assets or diversified balance sheets. They compete for the same interconnection queues, land, and subsidy budgets that portfolio mandates reserve partly for firm-power categories, and they lack the lobbying footprint to shape eligibility rules. Their exit is limited: project pipelines, permits, and workforce are jurisdiction-specific, and selling into a market that down-weights their technology compresses valuations.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, renewable_first_developers, payer,
    moderate, biographical, constrained, regional).

% Households and businesses paying tariffs and taxes that fund the portfolio's above-market components: contract-for-difference top-ups, loan guarantee exposures, and capacity payments for firm power procured ahead of cheaper alternatives. In regulated monopoly service territories they cannot choose their supplier, and rooftop self-generation is capital-gated and unavailable to renters. They bear the cost difference between the mandated mix and the least-cost mix, diffusely and invisibly, embedded in bills.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, electricity_ratepayers, payer,
    powerless, biographical, trapped, national).

% NGOs, academics, and campaign networks whose organizing identity is the claim that renewables plus storage can decarbonize fully, faster, and cheaper. The portfolio frame costs them legitimacy: their position is recast as one input to a reasonable-center synthesis, their funding pitches must route through technology-inclusive language, and their strongest empirical arguments are absorbed as 'regional variation' caveats rather than accepted as refutations. Leaving the cause would dissolve the professional and moral identity built around it.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, renewable_primacy_advocates, payer,
    moderate, generational, identity_locked, global).

% Movements and researchers arguing that demand reduction makes large-scale generation expansion unnecessary, rejecting the growth-presupposing frame shared by every portfolio variant. They are present in public discourse but absent from the planning rooms where portfolio shares are set: integrated resource plans, auction design consultations, and scenario exercises all take demand growth as given. Their objection is not about the mix but about the question, and no seat exists for objections to the question itself.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, degrowth_sufficiency_advocates, excluded,
    powerless, civilizational, identity_locked, global).

% Oil, gas, and coal companies that did not author this reading and oppose its substance, but harvest its vocabulary: 'technology neutrality' language migrates easily into defenses of gas as a bridge fuel and CCS-linked fossil continuation. In the short run they gain rhetorical cover and delayed phase-out schedules. Over the constraint's operative horizon they pay: a portfolio that must contain both nuclear and renewables at scale displaces fossil generation from the mix they currently dominate. They hold mobile capital and can pivot into hydrogen, CCS services, or other energy categories.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, fossil_fuel_incumbents, beneficiary,
    powerful, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_legitimacy__portfolio_pragmatism_reading, fossil_fuel_incumbents, payer).

% Research groups producing the cost-optimization scenarios that operationalize 'optimal portfolio' as a computable object. Every portfolio-framing policy cycle generates demand for their models, advisory contracts, and scenario ensembles; their methodological choices (discount rates, technology learning curves, firm-capacity treatment) quietly allocate hundreds of billions in perceived feasibility. Their careers, methods, and institutional standing are built on the portfolio-optimization frame itself; abandoning it would mean abandoning the discipline's core object. They simultaneously analyze the frame they inhabit.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, integrated_assessment_modelers, beneficiary,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_legitimacy__portfolio_pragmatism_reading, integrated_assessment_modelers, observer).

% Independent researchers, auditors, and comparative-modeling teams who track the gap between model-optimal mixes and actually procured mixes, and between neutrality rhetoric and subsidy flow. They publish, testify, and referee disputes between the camps without holding procurement authority or asset exposure.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__portfolio_pragmatism_reading, energy_systems_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_mitigation_legitimacy__portfolio_pragmatism_reading, nuclear_industry_coalition).
narrative_ontology:fixing_cost_class(climate_mitigation_legitimacy__portfolio_pragmatism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a genuine collective-action problem under deep technology uncertainty: no single low-carbon technology is bankable across all regions and time horizons, so a mandated portfolio spreads technology risk, preserves supply-chain diversity, sustains firm-capacity options where renewable-plus-storage systems remain unproven at scale, and holds a broad climate coalition together that would fragment into warring single-technology camps otherwise.
% TRANSFER_FUNCTION: Moves public capital, subsidy eligibility, and political legitimacy toward a balanced technology basket: concretely, it shifts marginal dollars and regulatory attention away from least-cost renewable buildout toward firm/dispatchable low-carbon capacity (nuclear), guarantees megaproject construction fee streams to large contractors, and transfers the cost premium of the mandated mix to ratepayers and taxpayers embedded in bills.
% ABSENT_VOICES: Degrowth and sufficiency advocates would object that every portfolio variant presupposes demand growth they reject; they are outside the planning rooms entirely. Energy-poor households in the Global South would note that 'optimal portfolio' debates presuppose grid-centric electrification paths they did not choose. Future generations bear whatever delay the hedging frame licenses but hold no seat anywhere in the process.
% DISAPPEARANCE_RATIONALE: If the technology-neutral portfolio norm vanished overnight, capital allocation would re-sort around whichever rival reading captured the vacuum: renewable-primacy jurisdictions would cancel firm-power procurements and rewrite subsidy formulas, baseload-necessity jurisdictions would entrench larger firm-power entitlements, and the cross-technology climate coalition that depends on the frame for its internal peace would fracture into single-technology camps fighting over the same budgets. Integrated resource plans, auction designs, and scenario-modeling programs worldwide are built on the frame and would all require reconstruction.
% FOUNDING_PROBLEM: Early-2000s climate policy faced a repeated credibility failure: single-technology prescriptions kept collapsing (nuclear costs spiraling after market liberalization and Fukushima; renewables hitting intermittency and integration limits at scale), and climate coalitions fractured into mutually discrediting technological camps. Portfolio pragmatism was built to maintain a broad coalition under irreducible technology uncertainty: keep every low-carbon constituency inside one tent, hedge bet-the-planet risk across options, and deny any camp the veto that comes from a single-technology mandate.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: grid-operator reliability studies (NERC, ENTSO-E) attest firm-capacity value independent of nuclear advocacy; IPCC and IEA scenario literature documents persistent regional variation in optimal mixes; academic energy-systems work (e.g., Princeton Net-Zero America and successor studies) confirms that no single technology dominates across all regions. Note the corroboration covers the founding problem's liveness, not this reading's exclusivity: renewable-primacy holders accept the uncertainty but dispute that it entails mandatory nuclear inclusion.
narrative_ontology:disappearance_verdict(climate_mitigation_legitimacy__portfolio_pragmatism_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_legitimacy__portfolio_pragmatism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_legitimacy__portfolio_pragmatism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_legitimacy__portfolio_pragmatism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_mitigation_legitimacy__portfolio_pragmatism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_mitigation_legitimacy__portfolio_pragmatism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.52 reflects a real but bounded rent channel: guaranteed technology inclusion converts public capital into above-market returns (contract-for-difference top-ups, loan guarantees, cost-plus megaproject fees) wherever the mandated mix diverges from the least-cost mix. Suppression 0.46 is structural-discursive rather than coercive: the frame enforces itself through funding-gate design, planning criteria, and 'realism' policing that recasts rival readings as naive or extreme; nobody is jailed, but agenda access is rationed. Theater ratio 0.38 captures the growing share of neutrality performance — stakeholder processes, technology-inclusive consultation rounds, portfolio-optimization dashboards — that reliably produces incumbent-weighted outcomes regardless of input. Accessibility collapse 0.42 is deliberately moderate: unlike a natural law, this frame leaves all three sibling readings fully articulable and organizationally alive; what collapses is not the alternatives' existence but their access to procurement authority. Resistance 0.58 is high because the frame meets sustained, organized contestation from renewable-primacy campaigns, degrowth movements, and ratepayer litigation over specific plants. The measurement series run on one shared time grid (T0~1997 Kyoto-era, T12~2009 Copenhagen, T18~2015 Paris, T24~2021 IRA/REPowerEU, T30~2027 projected) with every tracked metric authored at every point; the T30 row is marked projected. Suppression_requirement rises modestly because enforcement machinery genuinely matured over the interval: loose 1990s rhetoric hardened into statutory technology-inclusion clauses, tech-neutral auction law, and capacity-market rules.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the agenda-setter seat (ministries, planners), the arrangement is prudent risk management they administer and can defend line-by-line. From the payer seats (ratepayers, renewable-first developers), the same structure is a transfer scheme they cannot exit: the cost premium is invisible in bills and the crowding-out is invisible in queues. From the nuclear-coalition seat, the frame is existential protection for a profession that has absorbed decades of cancellation risk. From the fossil seat, the frame is a hostile instrument whose vocabulary is nonetheless worth stealing. From the modeler seat, the frame is the discipline itself — questioning it questions their professional object. The engine derives these divergent classifications from the structural data; this story does not adjudicate which seat is right.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: nuclear_industry_coalition, diversified_utility_holdcos, and large_epc_contractors sit near the beneficiary end (low d, damped effective extraction); electricity_ratepayers and renewable_first_developers sit near the target end (high d, amplified — ratepayers especially, being trapped in monopoly tariff structures with no arbitrage-grade exit). Two overrides correct derivations the declarations alone would get wrong. First, renewable_energy_sector derives as a near-pure beneficiary from its inclusion in the portfolio, but its actual position is near-symmetric (override organized -> 0.45): it collects deployment capital yet pays in agenda control, subsidy-share dilution, and formal demotion of its core claim. Second, fossil_fuel_incumbents derive as beneficiaries from their rhetorical harvesting role, but their net structural position over the constraint's operative horizon is target-side (override powerful -> 0.60): a binding nuclear-plus-renewables portfolio displaces the generation fleet they dominate, and their short-run gains are linguistic cover, not receipts from this constraint's transfer function. The identity-locked seats (modelers, movement advocates) matter for persistence rather than directionality: their exit impossibility stabilizes the frame from both the benefiting and the contesting sides.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as tangled_rope rather than rope preserves the detection of the rent channel; classifying it as snare would erase the genuine coordination function that makes the frame durable and partially defensible. The founding problem (coalition maintenance under technology uncertainty) is live — corroborated by grid-operator reliability data and cross-regional modeling outside the benefiting parties — so no mandatrophy resolution is declared and no piton reading is warranted: the frame's function has not atrophied, and its maintenance is substantially functional rather than theatrical. The R5 mismatch consumer should note the alignment here: founding_problem_status=live with disappearance_verdict=world_rearranges is the coherent cell. The residual risk this classification guards against is drift, not decay: if neutrality rhetoric continues migrating toward fossil-continuation uses (the capture omega below) while the nuclear-inclusion component becomes politically untouchable, the frame's coordination share falls and the same structure slides toward snare — the temporal series is designed to catch that transition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_location,
    'This constraint is one reading of the climate_mitigation_legitimacy kernel; where exactly is the disagreement between readings located, and what would adopting a sibling change structurally?',
    'Comparative structural analysis across the four reading-stories: locate the disputed element (whether ''optimal'' means system-cost minimization, a reliability floor, speed-and-cost frontier dominance, or throughput reduction) and trace how each reading''s beneficiary/victim sets differ.',
    'Under renewable_primacy, the nuclear beneficiary set vanishes and this story''s extraction channel closes; under baseload_necessity, firm-power entitlements enlarge beyond regional variation; under degrowth_sufficiency, the entire generation-expansion frame dissolves along with its ratepayer-victim structure. Classification of the kernel''s politics depends on which reading governs.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Committer structure: one reading of a four-reading kernel; disagreement located in the definition of ''optimal'' and the resulting beneficiary sets.').

omega_variable(
    neutrality_rhetoric_capture,
    'Does ''technology neutrality'' in actual procurement operate neutrally, or does lobbying asymmetry convert formal neutrality into incumbent preference?',
    'Audit realized subsidy flows, contract terms, and procurement outcomes against model-optimal allocations across jurisdictions with differing lobbying environments; natural experiments exist where auction design changed eligibility rules.',
    'If neutrality systematically routes capital to the best-lobbied rather than the best-performing technology, the frame''s coordination share falls and its classification slides from tangled_rope toward snare; if allocations track modeled optima, the extraction measured here is bounded hedging cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(neutrality_rhetoric_capture, empirical, 'Whether the neutrality norm is operationally neutral or captured by incumbent lobbying asymmetry.').

omega_variable(
    fossil_seat_directionality,
    'What is the fossil incumbents'' true structural relationship to this constraint — rhetorical free-rider collecting legitimacy cover, or net payer as mandated portfolios displace their generation?',
    'Track fossil displacement trajectories in jurisdictions that hard-bind nuclear-plus-renewables portfolio requirements versus those with voluntary frames; measure whether fossil firms'' realized returns rise or fall under the binding frame.',
    'If the free-rider reading dominates, the frame''s extraction is broader than its declared beneficiary set suggests (unlisted collectors); if the payer reading dominates, the override to target-side d is confirmed and the frame functions as advertised against incumbents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fossil_seat_directionality, empirical, 'Resolves the dual-positioned fossil seat: short-run rhetorical beneficiary versus long-run displaced payer.').

omega_variable(
    mitigation_delay_attribution,
    'Does portfolio hedging cause net mitigation delay (making the atmosphere and future generations an unseated victim class), or does coalition durability accelerate cumulative decarbonization despite per-project inefficiency?',
    'Compare cumulative emissions trajectories across jurisdictions sorted by frame strictness, controlling for baseline ambition; decompose delay into coalition-collapse counterfactuals using historical episodes where single-technology mandates failed politically.',
    'If hedging causes net delay, the constraint''s true victim set extends beyond ratepayers to unrepresented future parties and effective extraction is understated by every seated metric; if coalition durability dominates, part of the measured extraction is the price of the political feasibility that makes any decarbonization happen at all.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mitigation_delay_attribution, empirical, 'Whether the frame''s deepest costs fall on an unseated class (future generations) via delay, or whether hedging buys net acceleration.').

omega_variable(
    pragmatist_center_identity_lock,
    'Is the reasonable-center position held on evidence, or is it identity-stabilizing for its holders — moderates constituting themselves against both ''utopian'' and ''reactionary'' technology camps — such that the frame persists beyond what its evidentiary support warrants?',
    'Track elite position stability against evidence shocks: if pragmatist institutions update portfolio shares when cost data shifts decisively (storage cost collapses, SMR delivery records), the position is evidentiary; if positions are invariant to decisive shocks, identity fusion dominates.',
    'If identity-fused, the frame''s persistence is partially decoupled from its function and its theater ratio understates inertial maintenance; exit-impossibility among the center explains why rival readings cannot win by evidence alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pragmatist_center_identity_lock, conceptual, 'Cognitive-capture check on the pragmatist center: evidentiary commitment versus identity fusion.').

omega_variable(
    regional_variation_falsifiability,
    'Is the regional-variation premise doing real analytical work, or does it function as an unfalsifiable fig leaf that absorbs any counterexample (''your region is just different'') and thereby immunizes the nuclear-inclusion requirement from refutation?',
    'Specify ex ante what regional evidence would falsify mandatory dual inclusion (e.g., a large-grid jurisdiction achieving reliability targets with zero firm-power procurement at lower system cost) and test whether pragmatist institutions accept such results or reclassify them as special cases.',
    'If the premise is fig-leaf, the reading''s axioms are empirically empty and its persistence rests entirely on beneficiary maintenance — strengthening the snare-drift hypothesis; if genuinely falsifiable, the reading retains epistemic legitimacy its rivals must engage.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regional_variation_falsifiability, conceptual, 'Falsifiability of the regional-variation axiom that distinguishes this reading from universalist siblings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_legitimacy__portfolio_pragmatism_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(clim_tr_t6, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 6, 0.26).
narrative_ontology:measurement(clim_tr_t12, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 12, 0.29).
narrative_ontology:measurement(clim_tr_t18, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 18, 0.33).
narrative_ontology:measurement(clim_tr_t24, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 24, 0.36).
narrative_ontology:measurement(clim_tr_t30, climate_mitigation_legitimacy__portfolio_pragmatism_reading, theater_ratio, 30, 0.38).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(clim_be_t6, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 6, 0.41).
narrative_ontology:measurement(clim_be_t12, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 12, 0.44).
narrative_ontology:measurement(clim_be_t18, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 18, 0.47).
narrative_ontology:measurement(clim_be_t24, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 24, 0.5).
narrative_ontology:measurement(clim_be_t30, climate_mitigation_legitimacy__portfolio_pragmatism_reading, base_extractiveness, 30, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 0, 0.34).
narrative_ontology:measurement(clim_su_t6, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 6, 0.37).
narrative_ontology:measurement(clim_su_t12, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 12, 0.4).
narrative_ontology:measurement(clim_su_t18, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 18, 0.43).
narrative_ontology:measurement(clim_su_t24, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 24, 0.45).
narrative_ontology:measurement(clim_su_t30, climate_mitigation_legitimacy__portfolio_pragmatism_reading, suppression_requirement, 30, 0.46).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_legitimacy__portfolio_pragmatism_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__portfolio_pragmatism_reading, baseload_necessity_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__portfolio_pragmatism_reading, renewable_primacy_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__portfolio_pragmatism_reading, degrowth_sufficiency_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'decarbonization strategy' decomposes into four structurally distinct readings of the climate_mitigation_legitimacy kernel, each with its own epsilon, beneficiary/victim structure, and classification. This story (portfolio_pragmatism) sits mid-family: it inherits firm-power legitimacy concerns from baseload_necessity (upstream, higher empirical confidence on reliability physics) and is contested downstream by renewable_primacy (whose cost-decline evidence pressures this reading's inclusion requirement) while degrowth_sufficiency rejects the expansion premise all three techno-readings share. The upstream story influences this one (reliability doctrine feeds its firm-capacity vindications); this story influences renewable_primacy by rationing its agenda access. Each member links to the others via network.affects_constraints; no member averages over the others' epsilon values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_mitigation_legitimacy__portfolio_pragmatism_reading, organized, 0.45).
constraint_indexing:directionality_override(climate_mitigation_legitimacy__portfolio_pragmatism_reading, powerful, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
