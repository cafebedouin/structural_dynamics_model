% ============================================================================
% CONSTRAINT STORY: climate_response_imperative__degrowth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_imperative__degrowth_reading, []).

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
 *   constraint_id: climate_response_imperative__degrowth_reading
 *   human_readable: Degrowth Requirement for Global North Climate Response
 *   domain: climate_policy/political_economy/intergenerational_justice
 *
 * SUMMARY:
 *   This story instantiates the degrowth_reading of the
 *   climate_response_imperative kernel: the claim that adequate climate
 *   response requires structural economic transformation of Global North
 *   economies — reduced material consumption, redistribution toward exposed
 *   populations, and post-growth provisioning institutions — as the enabling
 *   condition for both mitigation and adaptation. The arrangement under
 *   contest, and the referent of epsilon, is the degrowth requirement itself
 *   as an operative governing arrangement: this reading authors the real
 *   costs it imposes on present-day Global North households and fossil-asset
 *   holders at full magnitude rather than discounting them as justified,
 *   which is exactly the structural delta separating it from its siblings.
 *   CONSTRAINT FAMILY: the colloquial label 'climate response' decomposes,
 *   per the epsilon-invariance principle, into three structurally distinct
 *   constraints sharing one kernel. The mitigation_priority_reading keeps
 *   present-day Northerners largely outside the victim set by loading the
 *   response onto technological innovation, market mechanisms, and assumed
 *   carbon dioxide removal; the adaptation_priority_reading shifts burden
 *   toward exposed-region populations left under-protected while treating
 *   mitigation as aspirational. This reading moves present-day Global North
 *   populations INTO the victim set via reduced consumption and working-time,
 *   names future generations and Global South populations as beneficiaries,
 *   and eliminates reliance on unproven CDR. The three files are linked
 *   through network.affects_constraints; their epsilon values differ because
 *   their victim/beneficiary structures differ, not because any of them is
 *   measured inconsistently. KEY AGENTS (by structural relationship): -
 *   present_day_global_north_households: primary target
 *   (organized/constrained) — bear reduced consumption and restructured
 *   working time - fossil_capital_asset_holders: secondary target
 *   (institutional/arbitrage) — bear stranded-asset losses, partially
 *   redeployable - future_generations: primary beneficiary
 *   (powerless/trapped) — receive stabilized climate and preserved ecosystems
 *   - global_south_exposed_populations: primary beneficiary
 *   (powerless/trapped) — receive adaptation finance, loss-and-damage
 *   transfers, atmospheric headroom - global_north_welfare_states: agenda
 *   setter (institutional/constrained) — must legislate and administer the
 *   transformation while fiscally growth-dependent -
 *   post_growth_intellectual_movements: discursive agenda setter
 *   (organized/identity_locked) — authored the requirement, press it into
 *   policy - low_income_north_households: dual-positioned
 *   (moderate/constrained) — light formal payers, cushioned by
 *   redistribution, exposed to sequencing failure -
 *   ipcc_and_assessment_bodies: analytical observer
 *   (institutional/analytical) — certify the physical arithmetic, decline to
 *   allocate sacrifice
 *
 * KEY AGENTS:
 *   - present_day_global_north_households: primary target (organized/constrained) — bear reduced consumption and restructured working time
 *   - fossil_capital_asset_holders: secondary target (institutional/arbitrage) — bear stranded-asset losses, partially redeployable
 *   - future_generations: primary beneficiary (powerless/trapped) — receive stabilized climate and preserved ecosystems
 *   - global_south_exposed_populations: primary beneficiary (powerless/trapped) — receive adaptation finance, loss-and-damage transfers, atmospheric headroom
 *   - global_north_welfare_states: agenda setter (institutional/constrained) — administers the transformation while fiscally growth-dependent
 *   - post_growth_intellectual_movements: discursive agenda setter (organized/identity_locked) — authored the requirement
 *   - low_income_north_households: dual-positioned (moderate/constrained) — light formal payers, cushioned by redistribution
 *   - ipcc_and_assessment_bodies: analytical observer (institutional/analytical) — certifies the physical arithmetic
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_imperative__degrowth_reading, 0.72).
domain_priors:suppression_score(climate_response_imperative__degrowth_reading, 0.58).
domain_priors:theater_ratio(climate_response_imperative__degrowth_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_imperative__degrowth_reading, tangled_rope).
narrative_ontology:human_readable(climate_response_imperative__degrowth_reading, "Degrowth Requirement for Global North Climate Response").
narrative_ontology:topic_domain(climate_response_imperative__degrowth_reading, "climate_policy/political_economy/intergenerational_justice").

domain_priors:requires_active_enforcement(climate_response_imperative__degrowth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_imperative__degrowth_reading, 'c56f9217-07fa-445d-a46b-a6e01dff6891').
narrative_ontology:cs_kernel_codification('c56f9217-07fa-445d-a46b-a6e01dff6891', distributed).
narrative_ontology:cs_authority_grounding('c56f9217-07fa-445d-a46b-a6e01dff6891', distributed).
narrative_ontology:cs_reading_relation('c56f9217-07fa-445d-a46b-a6e01dff6891', climate_response_imperative__mitigation_priority_reading, forecloses).
narrative_ontology:cs_reading_relation('c56f9217-07fa-445d-a46b-a6e01dff6891', climate_response_imperative__adaptation_priority_reading, influences).
narrative_ontology:cs_axiom('c56f9217-07fa-445d-a46b-a6e01dff6891', foundational, absolute_demand_reduction_necessary).
narrative_ontology:cs_axiom_status(absolute_demand_reduction_necessary, holdable).
narrative_ontology:cs_axiom_grounding('c56f9217-07fa-445d-a46b-a6e01dff6891', absolute_demand_reduction_necessary, empirically_contingent).
narrative_ontology:cs_axiom('c56f9217-07fa-445d-a46b-a6e01dff6891', foundational, historical_beneficiaries_bear_transition_costs).
narrative_ontology:cs_axiom_status(historical_beneficiaries_bear_transition_costs, holdable).
narrative_ontology:cs_axiom_grounding('c56f9217-07fa-445d-a46b-a6e01dff6891', historical_beneficiaries_bear_transition_costs, deontological).
narrative_ontology:cs_reference_frame('c56f9217-07fa-445d-a46b-a6e01dff6891', entropy_limited_provisioning_order).
narrative_ontology:cs_drift_state('c56f9217-07fa-445d-a46b-a6e01dff6891', contemporary_post_ar6_policy_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('c56f9217-07fa-445d-a46b-a6e01dff6891', '').
narrative_ontology:cs_kernel_id(climate_response_imperative__degrowth_reading, climate_response_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_imperative__degrowth_reading, future_generations).
narrative_ontology:constraint_beneficiary(climate_response_imperative__degrowth_reading, global_south_exposed_populations).
narrative_ontology:constraint_victim(climate_response_imperative__degrowth_reading, present_day_global_north_households).
narrative_ontology:constraint_victim(climate_response_imperative__degrowth_reading, fossil_capital_asset_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_response_imperative__degrowth_reading, low_income_north_households).
narrative_ontology:constraint_victim(climate_response_imperative__degrowth_reading, low_income_north_households).
narrative_ontology:constraint_vindicates(climate_response_imperative__degrowth_reading, entropy_law_economics).
narrative_ontology:constraint_vindicates(climate_response_imperative__degrowth_reading, planetary_boundaries_framework).
narrative_ontology:constraint_vindicates(climate_response_imperative__degrowth_reading, insufficient_decoupling_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Live in the high-income economies whose consumption and working patterns the requirement addresses. Under the arrangement they would consume fewer energy-intensive goods and services, work and spend time under shortened or restructured hours, and watch carbon-intensive assets and lifestyles lose value or legality. Redistribution cushions the lowest-income among them, but the median household absorbs a real decline in material throughput available to it. They cannot leave the climate system, and moving between Northern countries does not escape the arrangement's reach; their consent, expressed through elections and protest, is the binding political limit on the whole program.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, present_day_global_north_households, payer,
    organized, biographical, constrained, continental).

% People not yet born who inherit whatever climate and ecosystems present decisions leave behind. They receive a stabilized climate, preserved ecological function, and avoided damages if the arrangement holds, and cascading harms if it does not. They hold no votes, seats, or market position in present decisions and appear only through advocacy, litigation proxies, and long-horizon institutions speaking on their behalf.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, future_generations, beneficiary,
    powerless, civilizational, trapped, universal).

% Populations in low-emitting, high-exposure regions — coastal, arid, tropical — who contributed least to cumulative emissions and face the sharpest damages. The arrangement directs adaptation finance, loss-and-damage transfers, and preserved atmospheric headroom toward them, and closes the growth-first development path they were told to follow, substituting a claim on Northern surplus instead. Their geographic exposure is fixed; migration is partial, costly, and politically closing.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, global_south_exposed_populations, beneficiary,
    powerless, generational, trapped, global).

% The governments and public administrations of high-income countries would legislate and administer the transformation: carbon rationing or firm pricing, working-time regulation, redistribution machinery, managed retirement of fossil infrastructure. Their fiscal systems — tax yield, pension funding, debt service — currently presuppose continuing growth, so they must rebuild solvency on a steady-state base while delivering the transition. They cannot decline the assignment: climate damages arrive regardless, and no other jurisdiction can perform the Northern share.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, global_north_welfare_states, agenda_setter,
    institutional, generational, constrained, continental).

% Owners of reserves, pipelines, combustion supply chains, and the financial claims built on continued throughput. Managed contraction strands a large share of these assets on a schedule announced in advance. Capital can partially redeploy into renewables, grids, and retrofit industries, and holders lobby heavily to slow or redirect the schedule, but the core claim — that fossil throughput retains its value — does not survive the arrangement.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, fossil_capital_asset_holders, payer,
    institutional, biographical, arbitrage, global).

% Ecological economists, degrowth researchers, and climate-justice organizers who developed the requirement and press it into policy: drafting post-growth institution designs, convening citizens' assemblies, publishing sufficiency scenarios. Their professional and activist identities are bound up with the project, and they accept its personal costs as demonstration. Their influence runs through ideas, evidence, and movement pressure rather than administration.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, post_growth_intellectual_movements, agenda_setter,
    organized, generational, identity_locked, global).

% Lower-income households inside Northern countries, whose consumption is already near subsistence in energy terms. Formal targets bind them lightly — they have little discretionary throughput to cut — and redistribution, efficiency retrofits, and shorter working hours can leave them materially better off. Their exposure is to design failure rather than magnitude: if levies land before cushions do, they pay first and are compensated last. They watch the sequence of implementation more than the size of the aggregate cut.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, low_income_north_households, payer,
    moderate, immediate, constrained, national).
narrative_ontology:stakeholder_secondary_role(climate_response_imperative__degrowth_reading, low_income_north_households, beneficiary).

% The scientific assessment machinery that compiles carbon budgets, scenario spaces, and the demand-side options literature. It certifies the physical arithmetic the requirement rests on while declining to endorse any particular allocation of sacrifice; its scenario databases now include low-demand futures alongside technology-heavy ones, which shifts what policymakers can cite as feasible.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, ipcc_and_assessment_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_imperative__degrowth_reading, global_south_exposed_populations).
narrative_ontology:fixing_cost_class(climate_response_imperative__degrowth_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligns Northern demand with physical carbon budgets and material throughput limits: reduces aggregate consumption in the highest-consuming populations to rates the biosphere can absorb, redirects surplus toward adaptation and loss-and-damage in exposed regions, and replaces growth-dependent provisioning with steady-state institutions so that mitigation and adaptation proceed together instead of competing for the same fiscal and political bandwidth.
% TRANSFER_FUNCTION: Moves consumption capacity, fiscal surplus, and atmospheric headroom from present-day high-consuming Northern households and fossil asset holders toward future generations (stabilized climate, preserved ecosystems) and Global South populations (adaptation finance, damage compensation, development space within remaining budgets); within the North, moves purchasing power from high-throughput households toward low-income households via redistribution.
% ABSENT_VOICES: Future generations are structurally absent — not yet alive to object or consent — and appear only through proxy advocates. Global South populations were absent from the arrangements that created the imbalance and remain under-weighted in agenda-setting despite growing negotiating presence. Within the North, low-income households are spoken for by policy elites richer than themselves. Nonhuman systems — the stable climate, surviving ecosystems — have no seat at all; their interests enter only as scientific boundary conditions.
% DISAPPEARANCE_RATIONALE: If the requirement vanished overnight, climate policy would reorganize entirely around the sibling pathways: technology-and-market mitigation with carbon removal doing the heavy lifting on paper, adaptation funded residually, Northern consumption trajectories uninterrupted. Carbon budgets would be allocated by price and bargaining power rather than by need and historical responsibility; the sacrifice the requirement schedules would not disappear but be transferred silently to future generations and exposed Southern populations as damages. The world rearranges because the constraint is a specific, contestable allocation of who gives up what, when.
% FOUNDING_PROBLEM: The arithmetic collision between exponential growth economies and a finite biosphere: early statements (Georgescu-Roegen's entropy critique, 1971; The Limits to Growth, 1972) argued that throughput growth on a finite planet must terminate, and the climate-specific form crystallized once carbon budgets made the termination date computable — efficiency and innovation alone cannot cut Northern emissions at the required rate, so absolute demand reduction in the highest-consuming populations is arithmetically unavoidable.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set — which matters doubly here, since the principal beneficiaries do not yet exist. The physical arithmetic is attested by the IPCC assessment cycle (remaining carbon budgets, overshoot trajectories) and by Earth-system science quantifying boundary transgression (Stockholm Resilience Centre planetary-boundary updates); the decoupling shortfall is attested by material-footprint accounting (UN International Resource Panel, European Environment Bureau analyses) showing GDP-emission separation failing at required rates. These sources attest the problem, not the remedy: the necessity of the degrowth remedy specifically is contested by the sibling readings and by growth-friendly economists, and this file does not claim otherwise.
narrative_ontology:disappearance_verdict(climate_response_imperative__degrowth_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_imperative__degrowth_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_imperative__degrowth_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_response_imperative__degrowth_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_imperative__degrowth_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_imperative__degrowth_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_imperative__degrowth_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_imperative__degrowth_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.72: the requirement, taken seriously, imposes large, deliberate, concentrated costs on identifiable present-day populations — consumption ceilings, working-time restructuring, stranded fossil wealth — and this reading declines to launder those costs as frictionless. It is not higher because the costs are bounded by sufficiency rather than immiseration, and the redistribution leg cushions the poorest Northern households. Suppression (0.58, raw and unscaled — only extractiveness is scaled by directionality and scope in the engine's computation) reflects the enforcement machinery the program genuinely requires: rationing or firm caps, mandated working-time change, retirement of operating infrastructure — coercion against dissent and against incumbent use, moderated by the deliberative, assembly-based methods the movement itself prescribes. Theater ratio is low (0.25): the program's currency is material measures, not pledges, though a performative strand (lifestyle sufficiency as substitute for structural change) persists. Accessibility collapse is moderate (0.45): adopting the constraint closes high-throughput options substantially while leaving wide room in how sufficiency is lived. Resistance is high (0.70): the constraint's defining political fact is that its heaviest costs fall on electorates able to veto it. CLAIM/METRIC INDEPENDENCE: claimed_type tangled_rope is stated from structure — a genuine coordination function (the only articulated path serving mitigation and adaptation jointly within physical budgets) fused with real asymmetric incidence (present Northern payers; absent-generation and Southern beneficiaries) held together by active enforcement — while the metrics above are authored independently as descriptive magnitudes; the engine computes per-seat types, and any divergence from the claim is the datum. MEASUREMENTS: all three tracked series share one eight-point grid (1972-2025). Base extractiveness rises as the requirement concretizes from academic thesis to named-policy demand with named payers. Suppression_requirement rises monotonically, tracing the buildout of enforcement capacity the program's specificity forces (persuasion-phase to rationing-phase design) — this enforcement-history dynamic is what the series is authored to track. Theater_ratio oscillates mildly with external attention cycles (1970s oil shocks, the 1992 Rio absorption of the radical edge into sustainable-development rhetoric, the post-2008 revival, post-2015 mainstreaming) — external drivers, not intermittent reinforcement as a mechanism. RECEIPT SURFACE: gain_flow names global_south_exposed_populations — the seat the extracted surplus demonstrably accrues to in the near term (adaptation finance, headroom); future_generations co-receive across a longer horizon, but their receipt is contingent on the arrangement holding for centuries, so the affirmative single-seat claim goes to the South. fixing_cost is authored prohibitive: for the seats able to abandon the requirement (Northern electorates and states), removal is politically cheap but substantively ruinous — it forfeits the only articulated pathway serving mitigation and adaptation jointly without betting on unproven CDR — so the cost of fixing relative to the benefit of retaining it is prohibitive.
 *
 * PERSPECTIVAL GAP:
 *   The payer and beneficiary seats compute opposite experiences from identical structural facts. From present-day Northern households the arrangement is a scheduled confiscation of accustomed life: less stuff, different work, devalued assets, enforced by their own governments. From the beneficiary seats it is the arrival of provision: finance, headroom, a survivable century. The agenda-setter seat carries an internal contradiction no other seat has: Northern welfare states must administer contraction while their solvency mechanics (tax yield, pension funding, debt service) presuppose growth, so the same institution is simultaneously the arrangement's author and its most plausible casualty. Same-level lateral divergence: high-consumption and low-income Northern households hold the same nominal class position and the same citizenship, but the arrangement touches them oppositely — the affluent household's consumption is the direct object of the cuts, while the low-income household's exposure runs through implementation sequence (levy before cushion) rather than magnitude. Identity lock: the post-growth movements' exit is identity_locked — professional and activist selves constituted by the project — so their seat reads costs they voluntarily absorb as vindication, a perception the engine should expect to diverge from conscripted payers absorbing the same costs involuntarily.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: future_generations and global_south_exposed_populations sit near the full-beneficiary end (d approaching 0) — the arrangement subsidizes them with finance, headroom, and a stabilized climate, and their trapped exit removes any arbitrage damping. present_day_global_north_households and fossil_capital_asset_holders sit near the full-target end (d approaching 1): they bear the transfer. Fossil capital's arbitrage exit damps its effective extraction somewhat — capital partially escapes by redeployment — while households' constrained exit leaves them near undamped. low_income_north_households derive near-symmetric directionality from their dual declaration (formal payers, cushioned beneficiaries). global_north_welfare_states mix agenda-setting authority with real exposure (fiscal growth-dependence), landing mid-range; post_growth_intellectual_movements land below midpoint as champions who absorb voluntary costs; the analytical seat carries no economic directionality. No directionality_overrides are authored: the beneficiary/victim declarations plus exit options already separate every seat, and the available override keys are power atoms shared by structurally opposed agents (e.g. 'organized' covers both consenting movements and resisting consumer blocs), so any override would misapply across the class it keyed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — the collision between finite planetary sinks and growth-dependent Northern economies — is live and worsening, so no mandatrophy resolution is declared and none should be inferred. The classification's work here is bidirectional mislabel prevention. Read from the payer seats alone, the arrangement presents as pure imposition — the snare reading its opponents author ('eco-austerity imposed on the many') — which erases the genuine coordination function grounded in carbon-budget arithmetic that no amount of institutional cleverness abolishes. Read from the movement's seat alone, it presents as pure coordination — the rope reading its proponents author ('shared sufficiency, everyone ultimately better off') — which erases the real, concentrated, unwilling incidence on present-day Northerners that the reading itself declares. Tangled_rope holds both: coordination function real, extraction real, enforcement load-bearing. The piton failure mode is distant: theater is low and the mandate is not outlived but unfulfilled — the risk on this constraint's horizon is not atrophied performance but abandoned enforcement, which would surface as falling suppression_requirement alongside rising damages rather than as theater drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is the degrowth_reading of the climate_response_imperative kernel. What structurally changes under the sibling readings — mitigation_priority_reading (innovation and market mechanisms primary, adaptation residual) and adaptation_priority_reading (resilience-building primary, mitigation aspirational) — and does this reading''s predicted delta (present-day Global North households entering the victim set; future generations and Global South as beneficiaries; no load-bearing CDR assumption) hold across the family?',
    'Comparative classification across the three sibling story-files sharing the kernel: victim/beneficiary sets, epsilon values, and computed per-seat types are read side by side; the delta is confirmed when the sibling files show present-day Northerners outside their victim sets and CDR or residual-adaptation assumptions carrying the response.',
    'If the mitigation_priority reading were adopted instead, present-day Northern households largely exit the victim set (costs deferred onto CDR promises and gradual substitution) and this constraint''s extractiveness drops sharply; if adaptation_priority were adopted, the victim set shifts toward under-protected exposed populations and the coordination function narrows to resilience investment. The spread among the three computed classifications is the measured content of the kernel contest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer structure: one of three readings of a shared kernel; sibling readings instantiate structurally different constraints.').

omega_variable(
    cdr_substitutability,
    'Can carbon dioxide removal deploy at the gigatonne scales and timelines the mitigation-priority pathway assumes, or does CDR remain unproven at load-bearing scale — making demand reduction arithmetically unavoidable?',
    'Tracked deployment versus model requirements: operating CDR tonnage, pipeline maturity, cost curves, and storage certification against the multi-gigatonne 2050 requirements embedded in net-zero scenarios.',
    'If CDR scales on schedule, this reading''s necessity axiom weakens, the victim set shrinks toward the mitigation-priority configuration, and extractiveness falls; if deployment stalls an order of magnitude short, the victim set stands as authored and the sibling pathway''s epsilon rises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cdr_substitutability, empirical, 'Whether unproven CDR can substitute for the demand reduction this requirement mandates.').

omega_variable(
    absolute_decoupling_feasibility,
    'Can Northern economies decouple wellbeing from emissions and material throughput absolutely and fast enough to make consumption reduction unnecessary — or does the decoupling record (outsourcing, rebound, pace) confirm the requirement''s premise?',
    'Consumption-based emission and material-footprint accounts against GDP for Northern economies, tested at the rates carbon budgets require rather than the rates achieved.',
    'Demonstrated decoupling at budget-compatible rates would falsify the necessity axiom and collapse this reading toward mitigation_priority; continued shortfall corroborates the victim set as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(absolute_decoupling_feasibility, empirical, 'Whether growth-compatible decoupling can obviate absolute demand reduction.').

omega_variable(
    democratic_consent_durability,
    'Can sustained consumption reduction hold under democratic consent in the North, or does enforcement against recalcitrant majorities push the arrangement''s coercive force beyond the authored 0.58 toward regimes its own movement rejects?',
    'Natural experiments and deliberative evidence: fuel-price backlash dynamics (the yellow-vest pattern), citizens''-assembly outputs and follow-through, referendum history on carbon pricing, and durability of working-time reforms where enacted.',
    'Durable consent keeps suppression as authored and the coordination-plus-incidence structure intact; consent failure forces either political abandonment (the constraint dies) or escalation (suppression rises, classification drifts toward enforced-extraction profiles).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_consent_durability, empirical, 'Whether the requirement''s enforcement can remain consent-based at scale.').

omega_variable(
    future_generations_standing,
    'Are future generations legitimately seatable as present beneficiaries — real parties whose claims ground the arrangement''s beneficiary side — or does their nonexistence reduce the beneficiary structure to advocacy fiction, leaving the arrangement as pure imposition on the living?',
    'Conceptual settlement in representation theory and law: standing doctrines (climate litigation on behalf of the young and unborn), ombudsman institutions, constitutional future-generations clauses, and the philosophical status of contingent persons.',
    'If standing is denied, the beneficiary side thins to living Southern populations only, the asymmetry sharpens, and the arrangement reads as present-Northern sacrifice for others rather than intergenerational exchange — raising effective extraction on the payer seats and pushing classification toward extraction-dominant profiles.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(future_generations_standing, conceptual, 'Whether absent future persons can ground the beneficiary structure.').

omega_variable(
    epsilon_referent_framing,
    'Is epsilon''s referent for this reading correctly the degrowth requirement itself as operative arrangement (authored here, with present-day Northerners in the victim set), or should the referent be the incumbent growth regime the requirement contests — which would raise epsilon further and move future generations and the Global South into the victim set instead?',
    'Framing arbitration at the kernel level: compare the three sibling files'' referent choices; if siblings author epsilon over the incumbent regime while this file authors over its own requirement, harmonize referents across the family and recompute comparability.',
    'Under the incumbent-regime referent, epsilon rises toward the high end (an existential-scale transfer to the voiceless), the victim set swaps to future generations and the Global South, and the story converges on an indictment of the status quo; the authored configuration instead records what this reading''s own program would cost its payers. Cross-kernel classification comparability depends on the choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epsilon_referent_framing, conceptual, 'Referent under-determination: the requirement itself versus the incumbent regime it contests.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_imperative__degrowth_reading, 1972, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t1972, climate_response_imperative__degrowth_reading, theater_ratio, 1972, 0.2).
narrative_ontology:measurement(clim_tr_t1980, climate_response_imperative__degrowth_reading, theater_ratio, 1980, 0.24).
narrative_ontology:measurement(clim_tr_t1990, climate_response_imperative__degrowth_reading, theater_ratio, 1990, 0.28).
narrative_ontology:measurement(clim_tr_t2000, climate_response_imperative__degrowth_reading, theater_ratio, 2000, 0.26).
narrative_ontology:measurement(clim_tr_t2008, climate_response_imperative__degrowth_reading, theater_ratio, 2008, 0.22).
narrative_ontology:measurement(clim_tr_t2015, climate_response_imperative__degrowth_reading, theater_ratio, 2015, 0.25).
narrative_ontology:measurement(clim_tr_t2020, climate_response_imperative__degrowth_reading, theater_ratio, 2020, 0.24).
narrative_ontology:measurement(clim_tr_t2025, climate_response_imperative__degrowth_reading, theater_ratio, 2025, 0.25).

% Extraction over time
narrative_ontology:measurement(clim_be_t1972, climate_response_imperative__degrowth_reading, base_extractiveness, 1972, 0.3).
narrative_ontology:measurement(clim_be_t1980, climate_response_imperative__degrowth_reading, base_extractiveness, 1980, 0.34).
narrative_ontology:measurement(clim_be_t1990, climate_response_imperative__degrowth_reading, base_extractiveness, 1990, 0.38).
narrative_ontology:measurement(clim_be_t2000, climate_response_imperative__degrowth_reading, base_extractiveness, 2000, 0.42).
narrative_ontology:measurement(clim_be_t2008, climate_response_imperative__degrowth_reading, base_extractiveness, 2008, 0.5).
narrative_ontology:measurement(clim_be_t2015, climate_response_imperative__degrowth_reading, base_extractiveness, 2015, 0.58).
narrative_ontology:measurement(clim_be_t2020, climate_response_imperative__degrowth_reading, base_extractiveness, 2020, 0.66).
narrative_ontology:measurement(clim_be_t2025, climate_response_imperative__degrowth_reading, base_extractiveness, 2025, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t1972, climate_response_imperative__degrowth_reading, suppression_requirement, 1972, 0.2).
narrative_ontology:measurement(clim_su_t1980, climate_response_imperative__degrowth_reading, suppression_requirement, 1980, 0.24).
narrative_ontology:measurement(clim_su_t1990, climate_response_imperative__degrowth_reading, suppression_requirement, 1990, 0.3).
narrative_ontology:measurement(clim_su_t2000, climate_response_imperative__degrowth_reading, suppression_requirement, 2000, 0.36).
narrative_ontology:measurement(clim_su_t2008, climate_response_imperative__degrowth_reading, suppression_requirement, 2008, 0.44).
narrative_ontology:measurement(clim_su_t2015, climate_response_imperative__degrowth_reading, suppression_requirement, 2015, 0.52).
narrative_ontology:measurement(clim_su_t2020, climate_response_imperative__degrowth_reading, suppression_requirement, 2020, 0.56).
narrative_ontology:measurement(clim_su_t2025, climate_response_imperative__degrowth_reading, suppression_requirement, 2025, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_imperative__degrowth_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_response_imperative__degrowth_reading, climate_response_imperative__mitigation_priority_reading).
narrative_ontology:affects_constraint(climate_response_imperative__degrowth_reading, climate_response_imperative__adaptation_priority_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial concept 'climate response' decomposes into three kernel readings with distinct epsilon values and victim/beneficiary structures, per the epsilon-invariance principle. This file (degrowth_reading) places present-day Global North households in the victim set and future generations plus Global South populations among beneficiaries, eliminating reliance on unproven CDR. The mitigation_priority_reading keeps present-day Northerners outside the victim set by deferring costs onto innovation, market mechanisms, and assumed CDR; the adaptation_priority_reading shifts burden toward exposed-region populations under a resilience-first frame. Upstream/downstream structure: the degrowth reading's redistribution component channels resources into the adaptation-priority agenda's domain (influences edge), while its necessity axiom logically excludes the mitigation-priority sufficiency claim (forecloses edge). All three files carry reciprocal links in network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
