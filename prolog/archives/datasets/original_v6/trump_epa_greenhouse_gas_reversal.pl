% ============================================================================
% CONSTRAINT STORY: trump_epa_greenhouse_gas_reversal
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_trump_epa_greenhouse_gas_reversal, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: trump_epa_greenhouse_gas_reversal
 *   human_readable: Trump EPA Reversal of Greenhouse Gas Finding
 *   domain: political/environmental/regulatory
 *
 * SUMMARY:
 *   The EPA's 2017 reversal of the 2009 Endangerment Finding represents a
 *   high-suppression extraction mechanism that shifts climate costs from
 *   regulated industries to vulnerable populations and future periods. The
 *   constraint operates through regulatory authority: the EPA possesses the
 *   statutory power to make scientific determinations about whether
 *   greenhouse gases endanger public health, and the reversal weaponizes that
 *   authority to suppress alternative regulatory pathways. The mechanism
 *   exhibits classic snare characteristics from the perspective of
 *   climate-vulnerable populations and the global atmospheric commons — they
 *   cannot exit the consequences of weakened regulation and bear the full
 *   cost of deferred climate action. From the perspective of fossil fuel
 *   producers, the reversal functions as coordinating rope — it clarifies
 *   regulatory expectations and provides arbitrage-enabled benefit. The
 *   constraint's theater ratio (0.55) reflects that the reversal maintains
 *   formal procedural legitimacy (Administrative Procedure Act compliance,
 *   cost-benefit analysis documentation) while substantively contradicting
 *   scientific consensus and prior regulatory findings. The reversal's
 *   extractiveness has increased from initial implementation (0.42) to
 *   current enforcement (0.58) as the time window for regulatory action has
 *   narrowed and climate damages have begun accruing.
 *
 * KEY AGENTS:
 *   - Fossil Fuel Producers: Primary beneficiary (institutional/arbitrage) — gain regulatory clarity and relief from Endangerment Finding implications; can invest in production maximization with reduced climate compliance risk
 *   - Climate Vulnerable Populations: Primary victim (powerless/trapped) — lack voice in regulatory reversal, cannot exit consequences of weakened greenhouse gas regulation, bear costs of deferred climate action
 *   - Atmospheric Commons: Primary victim (powerless/trapped) — global commons with no agent advocacy; physics-trapped into bearing accumulated carbon burden set by regulatory policy
 *   - States and Municipalities: Secondary victim/constrained actor (powerful/constrained) — limited federal regulatory authority but responsible for local adaptation; federal preemption restricts compensatory action
 *   - Environmental Organizations: Organized agents (organized/constrained) — see reversal as temporary Scaffold through litigation, state-level alternatives, and technological cost curves
 *   - The EPA: Institutional actor (institutional/arbitrage) — maintains performative regulatory process while substantively degrading environmental protection function (Piton perspective)
 *   - Analytical Observer: Civilizational context (analytical/analytical) — observes snare/rope perspectival gap and resolves mandatrophy by recognizing same mechanism coordinates for beneficiaries while extracting from victims
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trump_epa_greenhouse_gas_reversal, 0.58).
domain_priors:suppression_score(trump_epa_greenhouse_gas_reversal, 0.68).
domain_priors:theater_ratio(trump_epa_greenhouse_gas_reversal, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trump_epa_greenhouse_gas_reversal, extractiveness, 0.58).
narrative_ontology:constraint_metric(trump_epa_greenhouse_gas_reversal, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(trump_epa_greenhouse_gas_reversal, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trump_epa_greenhouse_gas_reversal, snare).
narrative_ontology:human_readable(trump_epa_greenhouse_gas_reversal, "Trump EPA Reversal of Greenhouse Gas Finding").
narrative_ontology:topic_domain(trump_epa_greenhouse_gas_reversal, "political/environmental/regulatory").

domain_priors:requires_active_enforcement(trump_epa_greenhouse_gas_reversal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trump_epa_greenhouse_gas_reversal, fossil_fuel_producers).
narrative_ontology:constraint_beneficiary(trump_epa_greenhouse_gas_reversal, high_emission_industries).
narrative_ontology:constraint_victim(trump_epa_greenhouse_gas_reversal, atmospheric_commons).
narrative_ontology:constraint_victim(trump_epa_greenhouse_gas_reversal, climate_vulnerable_populations).
narrative_ontology:constraint_victim(trump_epa_greenhouse_gas_reversal, future_generations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CLIMATE VULNERABLE POPULATIONS (SNARE) — Trapped by geography and economic circumstance into bearing the costs of weakened greenhouse gas regulation. Cannot exit the consequences of atmospheric carbon accumulation. Regulatory reversal directly extracts from their future security, health, and property. Maximum suppression: no political voice in reversal decision, no individual exit option, no compensation mechanism.
constraint_indexing:constraint_classification(trump_epa_greenhouse_gas_reversal, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE ATMOSPHERIC COMMONS (SNARE) — The global atmosphere is an abstract commons with no agent to advocate for it. Trapped by physics into accumulating greenhouse gases at rates set by regulatory policy. The EPA reversal directly extracts from the commons's absorption capacity, transferring climate burden to future periods. No exit, no voice, no self-correction mechanism.
constraint_indexing:constraint_classification(trump_epa_greenhouse_gas_reversal, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: FOSSIL FUEL PRODUCERS (ROPE) — Net beneficiaries of the regulatory reversal. Coordination mechanism: the reversal coordinates industry expectations around a deregulatory posture, reducing uncertainty and enabling production-maximizing strategies. High arbitrage: producers can diversify into renewables or relocate operations; they benefit from regulatory flexibility without bearing climate costs. Experienced extraction runs toward this agent.
constraint_indexing:constraint_classification(trump_epa_greenhouse_gas_reversal, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: STATES AND MUNICIPALITIES (TANGLED ROPE) — Constrained by federal regulatory authority but also responsible for local climate adaptation and infrastructure resilience. The EPA reversal imposes climate costs while restricting their regulatory capacity to offset those costs. Mixed: coordination benefit (regulatory clarity) coupled with asymmetric burden-bearing (climate damages). Some mobility through state-level climate action, but federal preemption limits alternatives.
constraint_indexing:constraint_classification(trump_epa_greenhouse_gas_reversal, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ENVIRONMENTAL AND CLIMATE ORGANIZATIONS (SCAFFOLD) — Organized agents (NGOs, climate networks, international coalitions) see the EPA reversal as a temporary setback with structural sunset conditions. Legal challenges, state-level regulatory capture reversal, investor pressure, and technological cost curves in renewables create alternative mechanisms that reduce the reversal's long-term extractive force. The reversal is high-suppression but low-duration — the constraint has a sunset as climate economics and political cycles shift. Theater ratio is moderate because the reversal is justified through formal administrative process (APA procedures, cost-benefit analysis) despite substantive climate science consensus.
constraint_indexing:constraint_classification(trump_epa_greenhouse_gas_reversal, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: THE EPA AS INSTITUTIONAL ACTOR (PITON) — The EPA sees its regulatory reversal as a policy reorientation with procedural legitimacy. However, the reversal maintains a substantial performative component: the agency continues to generate environmental impact assessments, air quality monitoring, and public comment processes that create the appearance of environmental consideration while reducing substantive constraints on emission pathways. The theater ratio reflects this — formal procedures persisting despite weakened functional environmental protection. The institution's own function (protecting public health) has degraded relative to its historical mandate, but the institutional machinery persists.
constraint_indexing:constraint_classification(trump_epa_greenhouse_gas_reversal, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — The reversal exhibits both coordination and extraction functions. Coordination: it clarifies regulatory expectations for industry, reducing uncertainty and enabling investment decisions. Extraction: it shifts climate costs from regulated entities to populations and future periods. The reversal is not a natural law but a reversible policy choice that maintains suppression of alternative regulatory pathways through doctrinal precedent and institutional inertia. The analytical perspective resolves mandatrophy by recognizing that the snare classification (from victim perspectives) and rope classification (from beneficiary perspectives) are both structurally accurate — the same mechanism coordinates for one party while extracting from another.
constraint_indexing:constraint_classification(trump_epa_greenhouse_gas_reversal, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trump_epa_greenhouse_gas_reversal_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(trump_epa_greenhouse_gas_reversal, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(trump_epa_greenhouse_gas_reversal, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(trump_epa_greenhouse_gas_reversal, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(trump_epa_greenhouse_gas_reversal, TR),
    TR >= 0.70.

:- end_tests(trump_epa_greenhouse_gas_reversal_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The EPA reversal directly reduces regulatory burden on fossil fuel producers, creating economic benefit (avoided compliance costs, extended production feasibility). However, the extraction is not total because state-level regulations and market forces partially offset the federal reversal. The measurement trajectory (0.42 → 0.58) reflects accumulating climate damages and narrowing time windows for regulatory action — as climate impacts strengthen, the reversal's opportunity cost to victims increases. Suppression (0.68): High. Multiple barriers prevent exit or alternative regulatory pathways: federal preemption overrides state authority (though this is contested), the Endangerment Finding reversal removes the primary legal basis for clean power rules, and the administrative process for reversing the reversal is lengthy. However, suppression is not absolute — states have legal pathways for climate regulation, litigation is ongoing, and technological alternatives (renewable cost curves) reduce the reversal's long-term binding force. Theater ratio (0.55): Moderate. The reversal maintains formal procedural legitimacy (APA notice-and-comment, cost-benefit analysis) that gives it the appearance of rational administrative action, but the substantive foundation (scientific finding of endangerment) is contradicted by consensus climate science. The theater is lower than pure institutional inertia because the reversal is actively justified and litigated, not merely performatively maintained.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence: the same regulatory mechanism is experienced as pure extraction (Snare) by trapped populations, as pure coordination benefit (Rope) by fossil fuel producers, as mixed burden-shifting (Tangled Rope) by states, as a temporary setback with structural sunset (Scaffold) by organized climate groups, and as degraded institutional performance (Piton) by the EPA itself. The divergence is not observational ambiguity but structural: the reversal genuinely coordinates regulatory expectations for industry while genuinely extracting from climate-vulnerable populations. The Snare perspective (powerless/trapped victims) and Rope perspective (institutional/arbitrage beneficiaries) are both factually accurate descriptions of different agents' structural positions. The perspectival gap is not resolved by averaging or splitting the difference — it is resolved by recognizing that the same constraint mechanism exhibits both functions depending on one's position within it.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) flow from structural position relative to the extraction mechanism. Fossil fuel producers occupy beneficiary positions (d ≈ 0.10, institutional power + arbitrage exit → low d → low/negative f(d) → experience as net coordination benefit). Climate-vulnerable populations occupy victim positions with trapped exits (d ≈ 0.95, powerless + trapped → high d → high f(d) ≈ 1.42 → maximum experienced extraction). States occupy constrained-victim positions (d ≈ 0.60, powerful + constrained → moderate d → moderate f(d) ≈ 0.85 → experienced as Tangled Rope). The engine's derivation chain produces these d values from declared beneficiary/victim status and exit options; no manual override is required for this story because the structural relationships are unambiguous.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED: The mandatrophy in this case is the apparent contradiction between calling the same regulatory mechanism both 'Rope' (from beneficiary perspective) and 'Snare' (from victim perspective). Standard coordinate systems would demand a single classification. Deferential Realism resolves this by indexing both classifications to the observer's structural position. The fossil fuel producers genuinely experience coordination benefit — the reversal enables their production strategies. The climate-vulnerable populations genuinely experience pure extraction — the reversal removes regulatory protections without offering alternatives. Both perspectives are structurally accurate because the constraint's extractiveness scales with directionality via f(d). The beneficiaries' d ≈ 0.10 produces f(d) < 0 (negative extractiveness = net benefit = Rope). The victims' d ≈ 0.95 produces f(d) ≈ 1.42 (maximum extractiveness = pure extraction = Snare). The analytical observer's Tangled Rope classification (extractiveness ≈ 0.58, suppression ≈ 0.68) represents the constraint's bifurcated function: real coordination for one party, real extraction from another, with no universal frame that collapses both into a single type.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    regulatory_precedent_durability,
    'How durable is the EPA reversal as regulatory precedent, or will subsequent administrations reverse it?',
    'Historical analysis of EPA reversals and re-reversals; assessment of Administrative Procedure Act litigation strength; comparison to similar deregulatory reversals',
    'If highly durable (persists 10+ years): extraction mechanism is locked in; classification remains Snare. If fragile (reversible within 5 years): sunset conditions strengthen; Scaffold perspective gains salience.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_precedent_durability, empirical, 'Durability of EPA regulatory reversal precedent').

omega_variable(
    cost_curve_transition_rate,
    'Will renewable energy cost curves and technology deployment outpace the time horizon of the EPA reversal''s regulatory benefit to fossil fuel producers?',
    'Comparison of LCOE (levelized cost of energy) trajectory for renewables vs fossil fuels; assessment of capital deployment rates; market share analysis for clean energy vs fossil fuels under both regulatory scenarios',
    'If renewables achieve cost parity within 5 years: reversal''s economic benefit to fossil producers declines rapidly; Scaffold sunset is real. If cost parity delayed 15+ years: reversal provides sustained extraction window; Snare classification strengthens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cost_curve_transition_rate, empirical, 'Pace of renewable energy cost transition relative to regulatory reversal duration').

omega_variable(
    state_regulatory_substitution,
    'Can state-level climate regulation and procurement policy fully substitute for federal EPA enforcement?',
    'Quantitative assessment of emissions reduction potential under state-level policies (California, Northeast Corridor, Clean Energy Standard programs); comparison to federal baseline under 2009 Endangerment Finding',
    'If state substitution is substantial (>60% of foregone federal reductions): victims have constrained exit; Tangled Rope classification gains empirical support. If minimal (<30%): federal reversal is effectively uncompensated; Snare classification strengthens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_regulatory_substitution, empirical, 'Capacity of state-level climate policy to substitute for federal EPA enforcement').

omega_variable(
    scientific_consensus_reversal_divergence,
    'Can the EPA''s legal reversal of the Endangerment Finding persist as federal policy while scientific consensus on greenhouse gas climate risks continues to strengthen?',
    'Monitoring of IPCC assessments, National Academy of Sciences climate science reviews, and peer-reviewed literature; assessment of political durability when scientific consensus divergence becomes extreme (>95% scientist consensus vs policy denial)',
    'If divergence persists indefinitely: institutions successfully suppress contrary evidence; extractive suppression mechanism succeeds. If divergence resolves through political reversal or litigation: suppression is temporary; Scaffold/Snare boundary shifts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scientific_consensus_reversal_divergence, conceptual, 'Divergence between policy and scientific consensus on greenhouse gas endangerment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trump_epa_greenhouse_gas_reversal, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epa_ghg_tr_t0, trump_epa_greenhouse_gas_reversal, theater_ratio, 0, 0.62).
narrative_ontology:measurement(epa_ghg_tr_t2, trump_epa_greenhouse_gas_reversal, theater_ratio, 2, 0.58).
narrative_ontology:measurement(epa_ghg_tr_t4, trump_epa_greenhouse_gas_reversal, theater_ratio, 4, 0.55).

% Extraction over time
narrative_ontology:measurement(epa_ghg_be_t0, trump_epa_greenhouse_gas_reversal, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(epa_ghg_be_t2, trump_epa_greenhouse_gas_reversal, base_extractiveness, 2, 0.53).
narrative_ontology:measurement(epa_ghg_be_t4, trump_epa_greenhouse_gas_reversal, base_extractiveness, 4, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trump_epa_greenhouse_gas_reversal, enforcement_mechanism).
narrative_ontology:affects_constraint(trump_epa_greenhouse_gas_reversal, clean_power_plan_reversal).
narrative_ontology:affects_constraint(trump_epa_greenhouse_gas_reversal, methane_emissions_deregulation).
narrative_ontology:affects_constraint(trump_epa_greenhouse_gas_reversal, climate_finance_withdrawal).

% DUAL FORMULATION NOTE:
% The EPA Endangerment Finding reversal is downstream of prior climate science consensus findings and regulatory precedents, but represents a distinct structural constraint focused on regulatory authority and extractive reallocation of climate costs. The upstream constraints (climate science findings, international climate agreements) have their own extractiveness values reflecting epistemic and diplomatic status; the regulatory reversal has extractiveness reflecting the institutional reallocation of compliance burden and climate risk.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
