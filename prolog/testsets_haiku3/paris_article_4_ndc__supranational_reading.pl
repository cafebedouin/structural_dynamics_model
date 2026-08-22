% ============================================================================
% CONSTRAINT STORY: paris_article_4_ndc__supranational_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_paris_article_4_ndc__supranational_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: paris_article_4_ndc__supranational_reading
 *   human_readable: Paris Article 4 NDC Binding Commitment (Supranational Reading)
 *   domain: international/environmental/political_economy
 *
 * SUMMARY:
 *   This constraint story instantiates the SUPRANATIONAL READING of the Paris
 *   Article 4 NDC kernel. The supranational reading interprets NDCs as
 *   binding, internationally enforceable commitments on a progressively
 *   tightening trajectory toward net-zero, with compliance mechanisms that
 *   override national sovereignty claims. This reading is contested by two
 *   sibling readings: the sovereigntist reading (NDCs are voluntary
 *   self-determined pledges preserving energy autonomy) and the equity
 *   reading (NDCs must preserve Common But Differentiated Responsibilities,
 *   distinguishing developed from developing-nation obligations). The
 *   supranational reading treats the binding structure and ratcheting
 *   mechanism as the core commitment's defining feature — and models the high
 *   extraction this produces: wealthy nations and renewable energy capital
 *   capture gains while carbon-intensive economies and petro-states bear
 *   stranded assets; developing nations face constrained energy access. The
 *   constraint is presented with high epsilon (0.78) because this reading
 *   instantiates a system of rules where non-compliance triggers coordinated
 *   reputational and financial sanctions, where alternative energy pathways
 *   are foreclosed by binding targets, and where wealth transfers flow upward
 *   from energy-intensive economies to renewable energy manufacturers and
 *   climate finance institutions. This reading's authority grounds itself in
 *   binding legal commitments and supranational enforcement — the Paris
 *   Agreement text, COP decisions, and the architecture of compliance
 *   reporting. The measurement trajectory shows extraction accumulation from
 *   2015 to 2030 as ratcheting mechanisms tighten and enforcement hardens.
 *
 * KEY AGENTS:
 *   - Global climate governance apparatus (UNFCCC, COP bodies, compliance committees) — sets binding targets and ratcheting schedules, enforces via reputational/financial sanctions
 *   - Wealthy developed nations — structural beneficiary; early movers in renewables capture rents and reduce climate risk
 *   - Carbon-intensive industries (fossil fuels, cement, steel) — structural victims; face regulatory extinction and stranded assets
 *   - Petro-states (Saudi Arabia, Russia, Venezuela, Nigeria) — structural victims; face fiscal collapse as export demand falls under global decarbonization pressure
 *   - Renewable energy manufacturers — structural beneficiary; mandated global demand from binding NDC targets
 *   - Energy-insecure developing nations — asymmetric payer and partial beneficiary; constrained energy access offsets climate risk reduction and inadequate climate finance
 *   - Indigenous land defenders — trapped victims; face displacement as NDC carbon-offset strategies appropriate ecosystems as carbon sinks
 *   - Northern climate finance institutions — structural beneficiary; expand scope and maintain policy influence through conditional lending
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(paris_article_4_ndc__supranational_reading, 0.78).
domain_priors:suppression_score(paris_article_4_ndc__supranational_reading, 0.71).
domain_priors:theater_ratio(paris_article_4_ndc__supranational_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(paris_article_4_ndc__supranational_reading, tangled_rope).
narrative_ontology:human_readable(paris_article_4_ndc__supranational_reading, "Paris Article 4 NDC Binding Commitment (Supranational Reading)").
narrative_ontology:topic_domain(paris_article_4_ndc__supranational_reading, "international/environmental/political_economy").

domain_priors:requires_active_enforcement(paris_article_4_ndc__supranational_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(paris_article_4_ndc__supranational_reading, '8b7bba06-1add-422b-86fd-4d3061c77588').
narrative_ontology:cs_kernel_codification('8b7bba06-1add-422b-86fd-4d3061c77588', formalized).
narrative_ontology:cs_authority_grounding('8b7bba06-1add-422b-86fd-4d3061c77588', lineage).
narrative_ontology:cs_interpretation_layer_present('8b7bba06-1add-422b-86fd-4d3061c77588').
narrative_ontology:cs_reading_relation('8b7bba06-1add-422b-86fd-4d3061c77588', paris_article_4_ndc__sovereigntist_reading, forecloses).
narrative_ontology:cs_reading_relation('8b7bba06-1add-422b-86fd-4d3061c77588', paris_article_4_ndc__equity_reading, coexists_with).
narrative_ontology:cs_axiom('8b7bba06-1add-422b-86fd-4d3061c77588', foundational, binding_supranational_enforcement).
narrative_ontology:cs_axiom_status(binding_supranational_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('8b7bba06-1add-422b-86fd-4d3061c77588', binding_supranational_enforcement, deontological).
narrative_ontology:cs_axiom('8b7bba06-1add-422b-86fd-4d3061c77588', foundational, ratcheting_mechanism_irreversible).
narrative_ontology:cs_axiom_status(ratcheting_mechanism_irreversible, holdable).
narrative_ontology:cs_axiom_grounding('8b7bba06-1add-422b-86fd-4d3061c77588', ratcheting_mechanism_irreversible, instrumental).
narrative_ontology:cs_reference_frame('8b7bba06-1add-422b-86fd-4d3061c77588', binding_international_commitment_supremacy).
narrative_ontology:cs_drift_state('8b7bba06-1add-422b-86fd-4d3061c77588', contemporary_enforcement_contestation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('8b7bba06-1add-422b-86fd-4d3061c77588', '').
narrative_ontology:cs_kernel_id(paris_article_4_ndc__supranational_reading, paris_article_4_ndc).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__supranational_reading, global_climate_governance_apparatus).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__supranational_reading, renewable_energy_manufacturers).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__supranational_reading, climate_finance_institutions).
narrative_ontology:constraint_victim(paris_article_4_ndc__supranational_reading, carbon_intensive_industries).
narrative_ontology:constraint_victim(paris_article_4_ndc__supranational_reading, petro_states).
narrative_ontology:constraint_victim(paris_article_4_ndc__supranational_reading, energy_insecure_developing_nations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__supranational_reading, developed_wealthy_nations).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__supranational_reading, energy_insecure_developing_nations).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__supranational_reading, northern_climate_finance_institutions).
narrative_ontology:constraint_victim(paris_article_4_ndc__supranational_reading, indigenous_land_defenders).
narrative_ontology:constraint_vindicates(paris_article_4_ndc__supranational_reading, climate_emergency_supranational_governance).
narrative_ontology:constraint_vindicates(paris_article_4_ndc__supranational_reading, binding_international_commitments_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The UNFCCC secretariat, COP bodies, and compliance committees set NDC review timelines, ratcheting mechanisms, and standardized reporting requirements. They enforce via reputational sanctions, loss of climate finance access, and exclusion from favorable trade terms. They administer the binding commitment framework itself and update it every five years under Article 4.3.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, global_climate_governance_apparatus, agenda_setter,
    institutional, generational, arbitrage, global).

% Set high NDC targets (net-zero by 2050) and benefit from a rules-based global decarbonization that protects their technology exports, renewable equipment markets, and future competitive position. Early movers in clean energy capture first-mover rents. They face moderate implementation costs offset by innovation payoffs and climate risk reduction.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, developed_wealthy_nations, beneficiary,
    institutional, generational, arbitrage, global).

% Oil, coal, natural gas, cement, and steel producers face regulatory extinction through NDC targets that mandate rapid decarbonization. Their assets are stranded; reinvestment is forced into clean alternatives or exit. They bear the direct cost of the transition and have constrained geographic exit (energy demand is global; hiding carbon-intensive production requires collusion).
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, carbon_intensive_industries, payer,
    powerful, biographical, constrained, global).

% Nations whose fiscal base depends on oil/gas export revenue face fiscal collapse as demand falls under global NDC pressure. Saudi Arabia, Russia, Nigeria, and Venezuela are structurally locked in by energy dependence. They can resist individually (exit via non-compliance threats) but face coordinated financial and trade sanctions if they do; their power is negated by the multilateral structure.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, petro_states, payer,
    powerful, biographical, constrained, national).

% Solar panel, wind turbine, battery, and electric vehicle manufacturers see mandated global demand from NDC targets. Market size expands dramatically; margins are protected by high capital barriers and scale economies. They benefit directly and indefinitely from the binding commitment.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, renewable_energy_manufacturers, beneficiary,
    powerful, biographical, mobile, global).

% Nations that must electrify and industrialize but face strict NDC limits on coal/gas. They pay by accepting restricted energy access, slower development, and dependence on imported renewable technology at high prices. They benefit from climate risk reduction and potential climate finance flows, but the finance is insufficient and conditional.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, energy_insecure_developing_nations, payer,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(paris_article_4_ndc__supranational_reading, energy_insecure_developing_nations, beneficiary).

% Communities in carbon-rich forests and wetlands face land confiscation, displacement, and restricted resource use as NDC carbon-offset strategies prioritize ecosystems as carbon sinks. They are excluded from NDC governance, paid minimally for land rights, and bear the cost of conservation-by-exclusion.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, indigenous_land_defenders, payer,
    powerless, biographical, trapped, local).

% World Bank, IMF, regional development banks, and bilateral aid agencies channel climate finance (promised $100B annually) with conditions attached (privatization, subsidy removal, carbon pricing adoption). They expand institutional scope and lending portfolio while maintaining influence over Southern energy policy.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, northern_climate_finance_institutions, beneficiary,
    institutional, generational, analytical, global).

% Cities, states, and corporations that lead decarbonization are not seated at NDC ratcheting negotiations; they implement binding commitments set by national governments but have no voice in their setting. They bear implementation costs while national negotiators capture legitimacy.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, subnational_actors, excluded,
    moderate, biographical, constrained, regional).

% Non-agent placeholder: abstract beneficiary (risk reduction from climate stabilization) with no voice, no exit, and no claim on present resource allocation.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, future_generations, beneficiary,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(paris_article_4_ndc__supranational_reading, future_generations).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(paris_article_4_ndc__supranational_reading, global_climate_governance_apparatus).
narrative_ontology:fixing_cost_class(paris_article_4_ndc__supranational_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global decarbonization by binding all states to progressively tightened emissions targets, with standardized reporting and mutual accountability, solving the tragedy-of-the-commons problem of greenhouse gas emissions where individual state incentives diverge from collective climate stability.
% TRANSFER_FUNCTION: Moves fiscal capacity from carbon-intensive economies and petro-states (through stranded assets, lost export revenue, and compliance costs) to renewable energy manufacturers, climate finance institutions, and wealthy nations capturing clean technology rents. Also moves sovereignty authority from national governments to supranational compliance mechanisms.
% ABSENT_VOICES: Subnational governments and private actors that implement the transition have no vote in NDC-ratcheting decisions; indigenous peoples and rural communities bearing land-use displacement have no seat at the table; carbon-intensive workers and communities dependent on fossil fuel industries are excluded from transition governance.
% DISAPPEARANCE_RATIONALE: If NDC binding commitments and their supranational enforcement vanished overnight, global carbon-intensive investment would resume, petro-state revenues would stabilize, renewable energy deployment would slow dramatically, and the climate stabilization trajectory would collapse. The energy economy would reorganize around uncoordinated national interest rather than binding collective targets.
% FOUNDING_PROBLEM: Uncoordinated national energy policies produce tragedy-of-commons greenhouse gas emissions; voluntary pledges (UNFCCC 1992, Kyoto 1997) failed because individual states defected when costs exceeded benefits; a binding, ratcheting, internationally accountable commitment structure was needed to overcome coordination failure.
% FOUNDING_PROBLEM_CORROBORATION: Climate scientists and energy modelers outside the governance apparatus attest the coordination problem is empirically live — unilateral decarbonization is insufficient and invites free-riding. Petro-states and carbon-intensive industries attest the binding enforcement is live by filing legal challenges and threatening exit. Wealthy nations and climate institutions attest the founding problem justifies the binding structure. No party credibly claims coordination failure is solved.
narrative_ontology:disappearance_verdict(paris_article_4_ndc__supranational_reading, world_rearranges).
narrative_ontology:founding_problem_status(paris_article_4_ndc__supranational_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(paris_article_4_ndc__supranational_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(paris_article_4_ndc__supranational_reading, 'none', 1).
narrative_ontology:epsilon_provenance(paris_article_4_ndc__supranational_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(paris_article_4_ndc__supranational_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(paris_article_4_ndc__supranational_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(paris_article_4_ndc__supranational_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This reading's high extractiveness (0.78) reflects that the binding commitment structure concentrates gains in renewable energy capital and wealthy nations while concentrating losses in carbon-intensive economies and petro-states. Suppression is high (0.71) because compliance is enforced through loss of climate finance access, exclusion from favorable trade terms, and coordinated diplomatic pressure — exit is suppressed by the multilateral structure (a petro-state cannot unilaterally exit without facing coordinated sanctions). Theater is low-to-moderate (0.28) because the coordination function (global emissions accountability) is real and substantial, but growing share of enforcement activity defends renewable-energy market protection rather than climate outcomes. The measurement series show extraction accumulating from 2015 (Paris Agreement adoption) through 2030 as ratcheting mechanisms tighten targets, national compliance machinery hardens, and carbon-intensive industries face extinction timelines. Suppression requirement rises sharply from 2015 to 2024 (the first two NDC cycles where high-ambition targets are binding) then stabilizes as the enforcement infrastructure matures. Theater ratio rises as the enforcement machinery becomes more elaborate relative to new decarbonization gains — diminishing returns on marginal emissions reductions trigger more theater (carbon accounting games, Scope 3 definitional disputes, offsetting schemes) to justify the continued apparatus.
 *
 * PERSPECTIVAL GAP:
 *   The supranational governance apparatus and wealthy beneficiary nations perceive the NDC binding structure as genuine coordination solving a tragedy-of-commons and as justified by climate emergency. From their seat the extraction is a legitimate transition cost borne by carbon-intensive economies for the greater good. From the petro-state and carbon-intensive industry seats, the same structure operates as enforced extraction backed by coordinated sanctions — they have no real exit option (energy is globally fungible; unilateral compliance would leave them bearing costs with no benefit). Energy-insecure developing nations sit asymmetrically: they see both coordination (climate risk reduction) and extraction (constrained energy access, technological dependence). The engine computes these divergent directionalities from the structural data: beneficiaries get low d (rewards dominate), victims get high d (costs dominate), and mixed seats get moderate d reflecting the asymmetry. The authored claim and metrics are independent; the perspectival divergence is the measurement the corpus exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Wealthy nations + renewable manufacturers: d near 0.15-0.25 (beneficiary end). They benefit directly, have arbitrage exit options (can shift to green investment), and hold power to shape the rules. Petro-states + carbon-intensive industries: d near 0.85-0.95 (target end). They face stranded assets, fiscal collapse, and constrained exit (energy demand is global; they cannot hide carbon-intensive production). Energy-insecure developing nations: d near 0.55-0.65 (asymmetric/mixed). They face constrained energy access and technological dependence (high cost to them) but also climate risk reduction and conditional finance (partial benefit). Indigenous land defenders: d near 0.92 (near-target). They have trapped exit (cannot leave their land) and bear displacement cost with minimal benefit. No directionality overrides are needed; the structural data (beneficiary/victim declarations + exit options + power) produces accurate derivation for every seat.
 *
 * MANDATROPHY ANALYSIS:
 *   This supranational reading instantiates a tangled_rope (genuine coordination function + asymmetric extraction). The coordination function is live: NDCs do solve the collective-action problem of greenhouse gas emissions by binding all states to progressively tightened targets with mutual accountability. However, the structure is also substantially extractive: the binding mechanism concentrates gains in renewable energy capital and wealthy nations while concentrating losses in carbon-intensive economies and petro-states. The key distinction from a snare is that the coordination function is NOT cover — it is the primary mechanism. But the asymmetric extraction is NOT incidental — it is institutionalized through ratcheting mechanisms that advantage wealthy early movers. The constraint is not dead/zombie (mandatrophy would require the founding problem to be solved but the constraint to persist); the coordination problem is live, but the distribution of gains/losses is increasingly contested. This classification prevents miscoding the structure as pure extraction (snare) which would be false — the coordination function is real. It also prevents miscoding as pure coordination (rope) which would miss the institutional extraction layered onto it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    binding_vs_voluntary_interpretation,
    'Does ''binding commitment'' in Article 4 of the Paris Agreement have legally binding force with enforcement mechanisms, or is it binding only in the moral sense without supranational sanctions?',
    'International Court of Justice interpretation, compliance dispute cases from states refusing penalties, or formal amendment/protocol clarifying binding status.',
    'If legally binding with enforcement, the constraint is tangled_rope with high suppression (as authoring assumes). If binding only morally without enforcement, suppression drops substantially and the constraint moves toward rope or piton. The whole classification depends on this resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(binding_vs_voluntary_interpretation, conceptual, 'Whether binding commitments carry supranational enforcement or are moral commitments without legal sanction.').

omega_variable(
    ratcheting_mechanism_enforcement,
    'Is the five-year ratcheting mechanism (Article 4.3) enforced such that states cannot set lower targets in successive NDCs, or do weaker NDCs face only reputational consequences?',
    'Empirical observation: if wealthy states cut targets and face no coordinated sanctions, enforcement is weak (reputational only); if coordinated financial/trade penalties follow, enforcement is hard.',
    'Hard enforcement supports high suppression (0.71) and tangled_rope classification. Weak enforcement raises theater_ratio and supports piton (atrophied enforcement) or rope (voluntary ratcheting).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ratcheting_mechanism_enforcement, empirical, 'Whether ratcheting targets are enforced through sanctions or only reputation.').

omega_variable(
    equity_modification_of_binding,
    'Does the equity reading (CBDR) modify the supranational reading''s binding structure such that developing nations have differentiated obligations, or are all obligations supranationally binding regardless of equity claims?',
    'COP decisions on CBDR implementation; compliance review of developing-nation NDCs; outcome of ongoing equity disputes in negotiation.',
    'If CBDR substantially modifies binding obligations, the supranational reading''s extraction falls (developing nations face lower targets, less extraction from energy constraint). If CBDR is rhetorical cover and binding applies equally, extraction remains high.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(equity_modification_of_binding, conceptual, 'Whether equity principles modify the binding structure or are secondary to universal binding commitments.').

omega_variable(
    stranded_asset_coordination_problem,
    'Is the coordinated stranding of carbon-intensive assets a genuine solution to tragedy-of-commons (preventing free-riding on unilateral decarbonization), or is it wealth extraction justified by climate emergency?',
    'Counterfactual analysis: would unilateral decarbonization by wealthy nations be undercut by free-riding petro-states, or would market forces strand assets anyway? Compare outcomes under coordinated vs. unilateral pathways.',
    'If genuine coordination solution, stranding is justified coordination cost (tangled_rope verified). If wealth extraction justified post-hoc by emergency, the constraint is closer to snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(stranded_asset_coordination_problem, preference, 'Whether coordinated asset stranding solves collective action or extracts wealth.').

omega_variable(
    northern_climate_finance_conditionality,
    'Do climate finance flows ($100B annually) represent genuine climate aid solving energy-poverty coordination, or do they represent conditional leverage exerted by wealthy nations to enforce NDC compliance and enforce market-friendly energy transitions (privatization, subsidy removal)?',
    'Empirical analysis of climate finance conditions; comparison with pre-Paris development aid patterns; recipient-nation testimony on policy sovereignty under finance conditions.',
    'If genuine aid, energy-insecure developing nations benefit more than authored (lower d, higher role as beneficiary). If conditional leverage, energy-insecure nations are victims with theater-ratio benefit (climate finance promised but constrained by conditions).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(northern_climate_finance_conditionality, empirical, 'Whether climate finance is genuine aid or conditional sovereignty extraction.').

omega_variable(
    supranational_vs_sovereigntist_foreclosure,
    'In practice, does the supranational reading''s enforcement (compliance reviews, sanctions, loss of finance access) foreclose the sovereigntist reading''s national self-determination, or do both readings coexist as different states choose different interpretations?',
    'Observation of state behavior under compliance pressure: if states refuse binding commitments and negotiate opt-outs, coexistence holds. If all states accept binding structure regardless of stated preference, supranational forecloses sovereigntist.',
    'If supranational forecloses sovereigntist, the classification is robust. If both coexist, the sovereigntist reading remains a live alternative and the kernel is genuinely underdetermined between readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supranational_vs_sovereigntist_foreclosure, empirical, 'Whether supranational enforcement logically or practically rules out sovereigntist interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(paris_article_4_ndc__supranational_reading, 2015, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pari_tr_t2015, paris_article_4_ndc__supranational_reading, theater_ratio, 2015, 0.08).
narrative_ontology:measurement_basis(pari_tr_t2015, observed).
narrative_ontology:measurement(pari_tr_t2018, paris_article_4_ndc__supranational_reading, theater_ratio, 2018, 0.12).
narrative_ontology:measurement_basis(pari_tr_t2018, observed).
narrative_ontology:measurement(pari_tr_t2021, paris_article_4_ndc__supranational_reading, theater_ratio, 2021, 0.21).
narrative_ontology:measurement_basis(pari_tr_t2021, observed).
narrative_ontology:measurement(pari_tr_t2024, paris_article_4_ndc__supranational_reading, theater_ratio, 2024, 0.26).
narrative_ontology:measurement_basis(pari_tr_t2024, observed).
narrative_ontology:measurement(pari_tr_t2027, paris_article_4_ndc__supranational_reading, theater_ratio, 2027, 0.28).
narrative_ontology:measurement_basis(pari_tr_t2027, projected).
narrative_ontology:measurement(pari_tr_t2030, paris_article_4_ndc__supranational_reading, theater_ratio, 2030, 0.28).
narrative_ontology:measurement_basis(pari_tr_t2030, projected).

% Extraction over time
narrative_ontology:measurement(pari_be_t2015, paris_article_4_ndc__supranational_reading, base_extractiveness, 2015, 0.42).
narrative_ontology:measurement_basis(pari_be_t2015, observed).
narrative_ontology:measurement(pari_be_t2018, paris_article_4_ndc__supranational_reading, base_extractiveness, 2018, 0.58).
narrative_ontology:measurement_basis(pari_be_t2018, observed).
narrative_ontology:measurement(pari_be_t2021, paris_article_4_ndc__supranational_reading, base_extractiveness, 2021, 0.68).
narrative_ontology:measurement_basis(pari_be_t2021, observed).
narrative_ontology:measurement(pari_be_t2024, paris_article_4_ndc__supranational_reading, base_extractiveness, 2024, 0.74).
narrative_ontology:measurement_basis(pari_be_t2024, observed).
narrative_ontology:measurement(pari_be_t2027, paris_article_4_ndc__supranational_reading, base_extractiveness, 2027, 0.77).
narrative_ontology:measurement_basis(pari_be_t2027, projected).
narrative_ontology:measurement(pari_be_t2030, paris_article_4_ndc__supranational_reading, base_extractiveness, 2030, 0.78).
narrative_ontology:measurement_basis(pari_be_t2030, projected).

% Suppression requirement over time
narrative_ontology:measurement(pari_su_t2015, paris_article_4_ndc__supranational_reading, suppression_requirement, 2015, 0.38).
narrative_ontology:measurement_basis(pari_su_t2015, observed).
narrative_ontology:measurement(pari_su_t2018, paris_article_4_ndc__supranational_reading, suppression_requirement, 2018, 0.52).
narrative_ontology:measurement_basis(pari_su_t2018, observed).
narrative_ontology:measurement(pari_su_t2021, paris_article_4_ndc__supranational_reading, suppression_requirement, 2021, 0.64).
narrative_ontology:measurement_basis(pari_su_t2021, observed).
narrative_ontology:measurement(pari_su_t2024, paris_article_4_ndc__supranational_reading, suppression_requirement, 2024, 0.69).
narrative_ontology:measurement_basis(pari_su_t2024, observed).
narrative_ontology:measurement(pari_su_t2027, paris_article_4_ndc__supranational_reading, suppression_requirement, 2027, 0.71).
narrative_ontology:measurement_basis(pari_su_t2027, projected).
narrative_ontology:measurement(pari_su_t2030, paris_article_4_ndc__supranational_reading, suppression_requirement, 2030, 0.71).
narrative_ontology:measurement_basis(pari_su_t2030, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(paris_article_4_ndc__supranational_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(paris_article_4_ndc__supranational_reading, 0.22).
narrative_ontology:affects_constraint(paris_article_4_ndc__supranational_reading, paris_article_4_ndc__sovereigntist_reading).
narrative_ontology:affects_constraint(paris_article_4_ndc__supranational_reading, paris_article_4_ndc__equity_reading).
narrative_ontology:affects_constraint(paris_article_4_ndc__supranational_reading, national_energy_sovereignty).
narrative_ontology:affects_constraint(paris_article_4_ndc__supranational_reading, climate_finance_conditionality).
narrative_ontology:affects_constraint(paris_article_4_ndc__supranational_reading, indigenous_land_rights_carbon_offsetting).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the paris_article_4_ndc kernel. The sovereigntist_reading interprets NDCs as voluntary self-determined pledges preserving national energy autonomy (same text, different authority grounding). The equity_reading interprets NDCs through Common But Differentiated Responsibilities requiring structural distinctions between developed and developing-state obligations (same text, different principle hierarchy). Each reading instantiates a different constraint with different epsilon, different beneficiary/victim structure, and different typology. They share the kernel text but diverge on binding force, enforcement, and equity-modification. Network links enable contamination-propagation analysis: if the supranational reading's enforcement is weakened by equity challenges, both siblings are affected.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(paris_article_4_ndc__supranational_reading, powerful, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
