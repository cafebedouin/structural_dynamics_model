% ============================================================================
% CONSTRAINT STORY: climate_response_obligation__adaptation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_obligation__adaptation_priority, []).

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
 *   constraint_id: climate_response_obligation__adaptation_priority
 *   human_readable: Adaptation-Priority Reading of Climate Response Obligation
 *   domain: climate policy/political economy/intergenerational ethics
 *
 * SUMMARY:
 *   This constraint captures the 'accept 2-3C, invest in resilience' policy
 *   reading of the broader climate response kernel. It is one of three
 *   structurally distinct readings of the same underlying commitment about
 *   what present societies owe in response to anthropogenic warming: this
 *   adaptation-priority reading, a mitigation-priority reading (rapid
 *   decarbonization to minimize warming), and a degrowth reading (reduce
 *   throughput to stay within planetary boundaries). Each reading names a
 *   different beneficiary/victim structure and is authored as its own
 *   constraint story with its own epsilon; this file is the
 *   adaptation-priority reading alone. Under this reading, current-generation
 *   wealthy-nation actors and fossil capital are structural beneficiaries;
 *   future generations and Global South frontline populations enter the
 *   victim set because prevention investment is foregone in favor of
 *   defensive infrastructure concentrated where capital already exists.
 *
 * KEY AGENTS:
 *   - incumbent_fossil_capital
 *   - wealthy_nation_current_generation
 *   - resilience_infrastructure_contractors
 *   - future_generations
 *   - global_south_frontline_populations
 *   - small_island_states
 *   - subsistence_agriculture_communities
 *   - climate_scientists_and_iam_researchers
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_obligation__adaptation_priority, 0.71).
domain_priors:suppression_score(climate_response_obligation__adaptation_priority, 0.58).
domain_priors:theater_ratio(climate_response_obligation__adaptation_priority, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, extractiveness, 0.71).
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(climate_response_obligation__adaptation_priority, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_obligation__adaptation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_obligation__adaptation_priority, "Adaptation-Priority Reading of Climate Response Obligation").
narrative_ontology:topic_domain(climate_response_obligation__adaptation_priority, "climate policy/political economy/intergenerational ethics").

domain_priors:requires_active_enforcement(climate_response_obligation__adaptation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_obligation__adaptation_priority, 'c18cba20-aede-444c-aae9-65f026d74835').
narrative_ontology:cs_kernel_codification('c18cba20-aede-444c-aae9-65f026d74835', distributed).
narrative_ontology:cs_authority_grounding('c18cba20-aede-444c-aae9-65f026d74835', distributed).
narrative_ontology:cs_reading_relation('c18cba20-aede-444c-aae9-65f026d74835', climate_response_obligation__mitigation_priority, influences).
narrative_ontology:cs_reading_relation('c18cba20-aede-444c-aae9-65f026d74835', climate_response_obligation__degrowth_reading, coexists_with).
narrative_ontology:cs_axiom('c18cba20-aede-444c-aae9-65f026d74835', foundational, warming_trajectory_substantially_locked_in).
narrative_ontology:cs_axiom_status(warming_trajectory_substantially_locked_in, holdable).
narrative_ontology:cs_axiom_grounding('c18cba20-aede-444c-aae9-65f026d74835', warming_trajectory_substantially_locked_in, empirically_contingent).
narrative_ontology:cs_axiom('c18cba20-aede-444c-aae9-65f026d74835', secondary, near_term_transition_cost_avoidance_justifies_resilience_priority).
narrative_ontology:cs_axiom_status(near_term_transition_cost_avoidance_justifies_resilience_priority, holdable).
narrative_ontology:cs_axiom_grounding('c18cba20-aede-444c-aae9-65f026d74835', near_term_transition_cost_avoidance_justifies_resilience_priority, instrumental).
narrative_ontology:cs_reference_frame('c18cba20-aede-444c-aae9-65f026d74835', post_paris_carbon_budget_consensus).
narrative_ontology:cs_drift_state('c18cba20-aede-444c-aae9-65f026d74835', post_2023_global_stocktake, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c18cba20-aede-444c-aae9-65f026d74835', '').
narrative_ontology:cs_kernel_id(climate_response_obligation__adaptation_priority, climate_response_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_obligation__adaptation_priority, incumbent_fossil_capital).
narrative_ontology:constraint_beneficiary(climate_response_obligation__adaptation_priority, wealthy_nation_current_generation).
narrative_ontology:constraint_beneficiary(climate_response_obligation__adaptation_priority, resilience_infrastructure_contractors).
narrative_ontology:constraint_victim(climate_response_obligation__adaptation_priority, future_generations).
narrative_ontology:constraint_victim(climate_response_obligation__adaptation_priority, global_south_frontline_populations).
narrative_ontology:constraint_victim(climate_response_obligation__adaptation_priority, small_island_states).
narrative_ontology:constraint_victim(climate_response_obligation__adaptation_priority, subsistence_agriculture_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Funds think tanks, lobbies legislatures, and shapes the policy discourse toward accepting 2-3C warming as a fixed baseline, framing rapid decarbonization as economically reckless. Continues extracting and monetizing reserves while the political horizon for mitigation is pushed back decade by decade. Faces essentially no binding constraint on continued extraction under this reading.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, incumbent_fossil_capital, agenda_setter,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_response_obligation__adaptation_priority, incumbent_fossil_capital, beneficiary).

% Avoids the near-term costs of rapid decarbonization — carbon taxes, stranded-asset write-downs, energy price shocks, industrial transition disruption. Can purchase resilience: sea walls, air conditioning, insured property, climate-controlled agriculture, migration options. Bears none of the worst physical impacts within its own lifetime under most warming trajectories in this band.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, wealthy_nation_current_generation, beneficiary,
    organized, biographical, mobile, national).

% Wins the contracts to build seawalls, desalination plants, climate-controlled buildings, and insurance products for wealthy jurisdictions. Has a direct commercial interest in adaptation being the chosen policy path over mitigation, since mitigation reduces the market for resilience infrastructure over time.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, resilience_infrastructure_contractors, beneficiary,
    powerful, biographical, arbitrage, national).

% Inherits a warmed climate baseline, locked-in feedback loops (ice sheet loss, permafrost methane release), and a shrinking window in which any residual mitigation is even physically possible. Has no vote, no seat, and no capacity to renegotiate a baseline set by decisions made before they existed.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, future_generations, payer,
    powerless, civilizational, trapped, global).

% Faces the crop failures, heat mortality, water stress, and displacement that 2-3C warming produces disproportionately in tropical and subtropical regions, without the fiscal capacity to build the resilience infrastructure the wealthy-nation reading assumes is the substitute for prevention. Migration routes to wealthier regions are simultaneously being tightened.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, global_south_frontline_populations, payer,
    powerless, biographical, trapped, regional).

% Faces sea-level rise that under a 2-3C trajectory threatens the physical existence of the territory itself, not merely its economic disruption. No adaptation investment renders permanent inundation survivable in place; the adaptation-priority reading has no answer for total territorial loss.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, small_island_states, payer,
    powerless, civilizational, trapped, national).

% Depends on climate stability for food security; a 2-3C world brings shifted growing seasons, pest range expansion, and water table collapse that adaptation aid rarely reaches at the scale or speed required. Has no mobility options comparable to wealthy-nation citizens and no capital to self-fund resilience.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, subsistence_agriculture_communities, payer,
    powerless, biographical, trapped, regional).

% Models both the physical trajectory under continued emissions and the cost curves for mitigation versus adaptation. Their integrated assessment models are cited by all sides of the kernel contest, often selectively, to support incompatible policy conclusions.
narrative_ontology:constraint_stakeholder(climate_response_obligation__adaptation_priority, climate_scientists_and_iam_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_obligation__adaptation_priority, diffuse).
narrative_ontology:fixing_cost_class(climate_response_obligation__adaptation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates current-generation economic actors around avoiding disruptive near-term transition costs by redirecting public and private investment toward defensive infrastructure (seawalls, cooling, crop resilience) rather than toward decarbonization that would strand existing capital and reorganize energy-dependent industries.
% TRANSFER_FUNCTION: Moves the cost of climate stability from the present generation and the historically high-emitting nations (who would bear mitigation costs) onto future generations and low-emitting, low-capacity regions (who bear the physical impacts and lack adaptation capital) — a temporal and geographic transfer executed through the choice of policy emphasis itself.
% ABSENT_VOICES: Future generations cannot testify in present policy debates by construction. Global South delegations at climate negotiations are present but structurally outvoted or under-resourced relative to wealthy-nation blocs and fossil-capital lobbying; small island states have raised existential objections in every major COP but lack the negotiating leverage to bind outcomes.
% DISAPPEARANCE_RATIONALE: If the adaptation-priority framing lost its political and institutional hold overnight, mitigation finance and stranded-asset write-downs would accelerate sharply, fossil capital valuations would reprice downward, and resilience-infrastructure firms would lose their primary policy tailwind — the entire allocation of current climate finance would reorganize toward decarbonization.
% FOUNDING_PROBLEM: Framed by its proponents as solving a real problem: rapid, economy-wide decarbonization within the remaining window is politically and technically difficult, and some warming is now physically locked in regardless of policy choice, so resources should go where they demonstrably reduce near-term harm.
% FOUNDING_PROBLEM_CORROBORATION: Fossil capital and adaptation-contractor interests attest the problem is live and that mitigation is no longer sufficient alone. Independent climate scientists and Global South negotiators attest that the 'inevitability' premise is itself partly a product of decades of the same actors lobbying against mitigation, making the founding problem self-manufactured rather than discovered — this corroboration comes from outside the beneficiary set and directly disputes the reading's own genealogy.
narrative_ontology:disappearance_verdict(climate_response_obligation__adaptation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_obligation__adaptation_priority, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_obligation__adaptation_priority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_response_obligation__adaptation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_obligation__adaptation_priority, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_obligation__adaptation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_obligation__adaptation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_obligation__adaptation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises across the interval (0.35 to 0.71) tracking the growing gap between locked-in physical trajectory and the political window in which mitigation could still change it — the later the adaptation-priority framing prevails, the more of the total future harm becomes structurally unavoidable, which is precisely the extraction this reading transfers onto non-consenting future and frontline parties. Theater ratio rises moderately (0.2 to 0.42) as 'resilience' rhetoric increasingly substitutes announced adaptation funding for the harder political work of decarbonization — commitments are made, disbursement lags, and the gap between pledged and delivered adaptation finance widens. Suppression is not physical coercion but institutional: fossil capital's lobbying capacity and the wealthy-nation voting weight in climate finance institutions actively narrow the menu of policy options considered viable, which is a real if diffuse suppressive force on the mitigation alternative.
 *
 * PERSPECTIVAL GAP:
 *   From the wealthy-nation current-generation seat, this reads as prudent risk management — warming is happening regardless of any single nation's mitigation effort, so directing resources to defensible, local resilience investment is the responsible choice given sunk physical trajectory. From the small island state or subsistence agriculture seat, the identical policy stance reads as a decision, made by parties who will not bear its worst consequences, to treat displaceable and asset-poor populations' territorial and food security as an acceptable cost of avoiding near-term transition disruption for wealthier actors. The engine computes these as different seat-level classifications from the same structural data; the divergence is not resolved by adjudicating which seat is 'right' about the physics.
 *
 * DIRECTIONALITY LOGIC:
 *   Fossil capital and wealthy-nation current generation are declared beneficiaries because the adaptation-priority stance directly relieves them of near-term transition costs while resilience investment flows disproportionately to jurisdictions with fiscal capacity to fund it — this maps to low d. Future generations, small island states, subsistence agriculture communities, and Global South frontline populations are declared victims because the same policy choice locks in physical impacts they cannot avoid, adapt to at scale, or vote against — this maps to high d, amplified by their trapped exit options and (for future generations) total absence from the decision process.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem this reading claims to solve — that rapid full decarbonization is now technically and politically infeasible within the remaining window — is genuinely partly live: some warming and some impacts are physically locked in regardless of policy choice from this point forward. But the founding-problem interview surfaces a genealogical complication: independent corroboration from outside the beneficiary set holds that the 'inevitability' premise was substantially manufactured by decades of lobbying from the same actors who now benefit from the adaptation framing, rather than discovered as an exogenous physical fact. This is the mandatrophy signature to watch: a reading whose founding problem is partly real and partly self-manufactured by its own beneficiaries is exactly the case the classification system needs to be able to hold without collapsing to either 'pure coordination around real constraint' or 'pure invented pretext' — it is a tangled rope, not a mountain or a pure snare, because the physical lock-in component is genuine even as the extractive component compounds on top of it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    physical_lock_in_boundary,
    'How much of the 2-3C trajectory is genuinely physically locked in regardless of any feasible near-term mitigation effort, versus how much remains preventable but is being treated as inevitable for political convenience?',
    'Updated carbon budget analysis against actually-achievable near-term emissions reduction rates, cross-checked against historical accuracy of prior ''inevitability'' claims made at earlier points in the interval (were 1990s or 2000s claims of inevitability falsified by subsequent mitigation that did occur elsewhere?).',
    'If most of the trajectory is genuinely locked in, the coordination component of this reading is larger than the metrics suggest and the classification should weight toward a more coordination-heavy tangled rope; if a large share remains preventable, the ''inevitability'' framing is closer to pure pretext and the reading is closer to snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(physical_lock_in_boundary, empirical, 'Whether the reading''s core premise of inevitability is physically grounded or partly self-fulfilling.').

omega_variable(
    kernel_reading_selection_pressure,
    'Why does the adaptation_priority reading gain institutional traction relative to the mitigation_priority and degrowth readings at particular historical moments — is reading selection driven by physical evidence updates, or by which reading serves incumbent capital''s near-term interest?',
    'Trace the funding and institutional sponsorship of adaptation-priority advocacy relative to mitigation-priority advocacy across the interval; compare timing of reading ascendance to fossil capital lobbying expenditure and to genuine physical-science updates (e.g. IPCC assessment report findings).',
    'If reading ascendance correlates more tightly with lobbying expenditure than with physical science updates, this corroborates the founding_problem_corroboration finding that the inevitability premise is partly manufactured, strengthening the tangled_rope classification over a more benign coordination reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_pressure, conceptual, 'Whether this reading''s institutional dominance tracks evidence or incumbent interest — the committer-axis location of disagreement among the three kernel readings.').

omega_variable(
    adaptation_finance_delivery_gap,
    'Is the theater_ratio rise driven by genuine measurement of a widening gap between pledged and delivered adaptation finance to the Global South, or by an authoring assumption not yet verified against disbursement data?',
    'Compare UNFCCC/OECD adaptation finance pledge tracking against verified disbursement data, disaggregated by recipient region, across the interval.',
    'A verified large gap would confirm the theater_ratio trajectory as descriptively accurate rather than assumed; a small or closing gap would require revising the theater_ratio series downward.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(adaptation_finance_delivery_gap, empirical, 'Whether the authored theater_ratio trajectory is empirically grounded or an unverified assumption about pledge-delivery gaps.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_obligation__adaptation_priority, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t1990, climate_response_obligation__adaptation_priority, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(clim_tr_t1997, climate_response_obligation__adaptation_priority, theater_ratio, 1997, 0.25).
narrative_ontology:measurement(clim_tr_t2005, climate_response_obligation__adaptation_priority, theater_ratio, 2005, 0.3).
narrative_ontology:measurement(clim_tr_t2012, climate_response_obligation__adaptation_priority, theater_ratio, 2012, 0.34).
narrative_ontology:measurement(clim_tr_t2018, climate_response_obligation__adaptation_priority, theater_ratio, 2018, 0.38).
narrative_ontology:measurement(clim_tr_t2024, climate_response_obligation__adaptation_priority, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(clim_be_t1990, climate_response_obligation__adaptation_priority, base_extractiveness, 1990, 0.35).
narrative_ontology:measurement(clim_be_t1997, climate_response_obligation__adaptation_priority, base_extractiveness, 1997, 0.42).
narrative_ontology:measurement(clim_be_t2005, climate_response_obligation__adaptation_priority, base_extractiveness, 2005, 0.5).
narrative_ontology:measurement(clim_be_t2012, climate_response_obligation__adaptation_priority, base_extractiveness, 2012, 0.58).
narrative_ontology:measurement(clim_be_t2018, climate_response_obligation__adaptation_priority, base_extractiveness, 2018, 0.65).
narrative_ontology:measurement(clim_be_t2024, climate_response_obligation__adaptation_priority, base_extractiveness, 2024, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t1990, climate_response_obligation__adaptation_priority, suppression_requirement, 1990, 0.3).
narrative_ontology:measurement(clim_su_t1997, climate_response_obligation__adaptation_priority, suppression_requirement, 1997, 0.36).
narrative_ontology:measurement(clim_su_t2005, climate_response_obligation__adaptation_priority, suppression_requirement, 2005, 0.43).
narrative_ontology:measurement(clim_su_t2012, climate_response_obligation__adaptation_priority, suppression_requirement, 2012, 0.49).
narrative_ontology:measurement(clim_su_t2018, climate_response_obligation__adaptation_priority, suppression_requirement, 2018, 0.54).
narrative_ontology:measurement(clim_su_t2024, climate_response_obligation__adaptation_priority, suppression_requirement, 2024, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_obligation__adaptation_priority, resource_allocation).
narrative_ontology:affects_constraint(climate_response_obligation__adaptation_priority, climate_response_obligation__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_obligation__adaptation_priority, climate_response_obligation__degrowth_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the climate_response_obligation kernel. All three name the same underlying question (what is owed in response to anthropogenic warming) but instantiate structurally distinct constraints with different beneficiary/victim sets and different epsilon values, per the ε-invariance principle: adaptation_priority (this file) treats warming as substantially locked in and redirects investment to resilience, concentrating benefit in current-generation wealthy actors and fossil capital; mitigation_priority treats warming as still substantially preventable and treats delay as the extraction mechanism; degrowth_reading treats growth-dependent throughput itself as the deeper generative mechanism behind both emissions and the adaptation/mitigation choice. Each is authored and network-linked separately rather than as one hedged or averaged story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
