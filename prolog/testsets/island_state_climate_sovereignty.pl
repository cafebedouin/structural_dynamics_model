% ============================================================================
% CONSTRAINT STORY: island_state_climate_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_island_state_climate_sovereignty, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: island_state_climate_sovereignty
 *   human_readable: Island State Climate Sovereignty and Development Extraction
 *   domain: geopolitical/environmental/development
 *
 * SUMMARY:
 *   Island state climate sovereignty represents a structurally complex
 *   constraint where a group of nations with minimal historical
 *   responsibility for climate change face existential threats from emissions
 *   produced by wealthy, high-capacity states. The constraint operates across
 *   multiple registers: physical (sea level rise, storm intensification),
 *   financial (adaptation costs, climate debt), institutional (development
 *   finance conditionality), and epistemic (climate science framing vs
 *   sovereignty claims). The constraint exhibits classic tangled rope
 *   structure: genuine coordination exists in climate science collaboration,
 *   renewable energy development, and disaster response networks, yet
 *   extraction occurs through forced adaptation spending, development finance
 *   dependency, and the asymmetric imposition of climate costs on those least
 *   responsible. Theater has increased steadily as climate governance
 *   institutions have proliferated (COPs, UNFCCC, bilateral climate deals)
 *   while actual emissions reductions lag and island vulnerability deepens.
 *   The interval spans 1990–2020, from early international climate
 *   negotiations through the Paris Agreement to the post-agreement period
 *   showing stalled progress.
 *
 * KEY AGENTS:
 *   - Island Nations: Primary victims (powerless/trapped) — face existential threats from external emissions; cannot exit or negotiate away climate physics
 *   - High-Emission Nations & Carbon Industries: Primary beneficiaries (institutional/arbitrage) — capture development benefits from carbon-intensive growth while externalizing costs
 *   - Development Finance Institutions (World Bank, IMF, bilateral donors): Secondary beneficiaries (institutional/arbitrage) — profit from adaptation financing and structural adjustment lending tied to climate programs
 *   - Alliance of Small Island States (AOSIS) & Organized Coalitions: Organized actors (organized/constrained) — build coalitions to negotiate Paris Agreement and adaptation finance; see constraint as temporary with sunset
 *   - UN Climate Framework Bureaucracy (UNFCCC, COP processes): Institutional infrastructure (institutional/arbitrage) — maintains climate negotiation theater; perpetuates itself through process while implementation stalls
 *   - Vulnerable Island Populations: Distributed victims (powerless/trapped) — face displacement, resource scarcity, and intergenerational harm; no exit options
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(island_state_climate_sovereignty, 0.58).
domain_priors:suppression_score(island_state_climate_sovereignty, 0.68).
domain_priors:theater_ratio(island_state_climate_sovereignty, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(island_state_climate_sovereignty, extractiveness, 0.58).
narrative_ontology:constraint_metric(island_state_climate_sovereignty, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(island_state_climate_sovereignty, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(island_state_climate_sovereignty, tangled_rope).
narrative_ontology:human_readable(island_state_climate_sovereignty, "Island State Climate Sovereignty and Development Extraction").
narrative_ontology:topic_domain(island_state_climate_sovereignty, "geopolitical/environmental/development").

domain_priors:requires_active_enforcement(island_state_climate_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(island_state_climate_sovereignty, high_emission_nations).
narrative_ontology:constraint_beneficiary(island_state_climate_sovereignty, carbon_intensive_industries).
narrative_ontology:constraint_beneficiary(island_state_climate_sovereignty, development_finance_institutions).
narrative_ontology:constraint_victim(island_state_climate_sovereignty, island_nations).
narrative_ontology:constraint_victim(island_state_climate_sovereignty, vulnerable_ecosystems).
narrative_ontology:constraint_victim(island_state_climate_sovereignty, future_island_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ISLAND NATIONS (SNARE) — Island states face imminent sea level rise, extreme weather, and ecosystem collapse caused by emissions they did not produce. Their exit options are minimal: they cannot relocate, cannot opt out of global climate systems, cannot force high-emitters to act. They are trapped within a constraint imposed by external actors. The constraint extracts their sovereignty (forcing climate adaptation spending), their territory (inundation), and their future (intergenerational harm). No coordination benefit accrues to them from the extraction mechanism itself.
constraint_indexing:constraint_classification(island_state_climate_sovereignty, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: HIGH-EMISSION NATIONS & CARBON INDUSTRIES (ROPE) — From the beneficiary's view, the constraint functions as a coordination mechanism: it allows high-emitting economies to benefit from carbon-intensive development while externalizing climate costs to vulnerable nations. The constraint solves the collective action problem for wealthy nations: 'How do we maintain growth while shifting disaster risk to those who cannot impose costs on us?' The coordination is highly asymmetric, but genuine from the beneficiary's structural position. Exit options for beneficiaries are arbitrage (they can shift energy infrastructure at will, or not, depending on profitability).
constraint_indexing:constraint_classification(island_state_climate_sovereignty, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: DEVELOPING ISLAND NATIONS AS DEVELOPMENT PARTNERS (TANGLED ROPE) — When viewed through development finance and capacity-building frameworks, the constraint exhibits hybrid structure: genuine coordination exists (islands benefit from renewable energy technology transfer, climate adaptation funding, scientific collaboration). Simultaneously, extraction occurs through debt-financed adaptation, conditional lending tied to structural reforms, and dependence on donor-designed programs that may not serve local priorities. Exit options are constrained by financial dependency and the high cost of rejecting development assistance (foregoing adaptation infrastructure). The constraint both enables development and extracts through asymmetric terms.
constraint_indexing:constraint_classification(island_state_climate_sovereignty, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: ALLIANCE OF SMALL ISLAND STATES / ORGANIZED COALITIONS (SCAFFOLD) — AOSIS and similar coalitions experience the constraint as a temporary coordination problem with an explicit exit pathway: the Paris Agreement's 1.5°C target represents a sunset clause embedded in international climate governance. If global emissions reductions proceed as targeted, the constraint's extraction mechanism (forced adaptation, involuntary climate migration) should sunset within one generation. Organized agents see suppression as high but declining — the constraint's function is explicitly designed to be temporary. However, this perspective depends on whether the sunset clause is real or performative (see omegas).
constraint_indexing:constraint_classification(island_state_climate_sovereignty, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: UN CLIMATE FRAMEWORK BUREAUCRACY (PITON) — The international climate governance apparatus (UNFCCC, COP processes, compliance mechanisms) is substantially performative: annual conferences produce non-binding commitments, emissions pledges consistently underperform actual reductions, loss-and-damage finance remains symbolic rather than transformative. The theater ratio (0.61) reflects that climate negotiations generate extensive reporting, monitoring, and accountability theater while actual extraction (climate harms to vulnerable populations) continues unchecked. The institutional machinery persists through organizational inertia and the difficulty of replacing it, not because it effectively solves the coordination problem. Theater has increased as the stakes have risen — COP processes have become more elaborate precisely as their effectiveness has declined.
constraint_indexing:constraint_classification(island_state_climate_sovereignty, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LIMIT VIEW (MOUNTAIN) — From a civilizational/universal perspective, the constraint could be viewed as a natural law: island vulnerability to sea level rise is a brute physical fact arising from geology and thermodynamics. Some extraction may appear inevitable given global economic structures. However, this perspective naturalizes what is structurally contingent: the extraction mechanism (forcing island states to bear adaptation costs while high-emitters profit) is not a law of physics but a governance choice. The engine's false summit detector should flag this as misclassification.
constraint_indexing:constraint_classification(island_state_climate_sovereignty, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(island_state_climate_sovereignty_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(island_state_climate_sovereignty, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(island_state_climate_sovereignty, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(island_state_climate_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(island_state_climate_sovereignty, TR),
    TR >= 0.70.

:- end_tests(island_state_climate_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Island states face forced adaptation spending, climate migration costs, ecosystem loss, and future economic harm from emissions they did not produce. This represents clear extraction — resources and territory flow away from island populations to beneficiaries in high-emitting nations. However, the extraction is not maximal (0.70+) because some genuine coordination exists: climate science collaboration is real, some renewable energy development provides mutual benefit, and disaster response networks have genuine reciprocal function. The value reflects the hybrid nature. Suppression (0.68): High. Island nations face formidable barriers to exit: they cannot relocate, cannot opt out of climate systems, cannot force high-emitters to reduce emissions, face resource constraints limiting adaptation options, and exist within unequal power structures in international negotiations. However, suppression is not total (0.90+) because some pathways exist: AOSIS coalitions have negotiated real commitments, some island states have achieved high renewable penetration, and technology transfer has occurred. Theater ratio (0.61): Moderate-high. Climate governance institutions generate substantial performative content — annual COPs with elaborate reporting, emissions pledges that underperform, loss-and-damage finance that remains symbolic, compliance monitoring systems that rarely enforce. Yet the constraint also has genuine coordination content — IPCC science is real, Paris Agreement framework is real, some adaptation projects genuinely help. The theater has increased over the interval (0.35 → 0.61) as the gap between governance performance and climate reality has widened. Extractiveness has similarly increased (0.42 → 0.58) as the cumulative costs of inaction have grown and island populations have borne more harm.
 *
 * PERSPECTIVAL GAP:
 *   The gap between the snare (island victim perspective) and the rope (high-emitter beneficiary perspective) is the defining diagnostic. Island states cannot escape the constraint's extraction because they cannot exit the climate system or negotiate away physics. High-emitting nations can exit anytime by switching energy infrastructure — they stay because extraction is profitable, making the constraint rope-like (coordination for mutual benefit at their analysis level). The tangled rope (development partner perspective) bridges these: genuine coordination exists alongside extraction. The scaffold (AOSIS perspective) depends on whether the sunset is real. The piton (UN bureaucracy) reflects institutional degradation. The false mountain (natural limit view) is a dangerous misframing that naturalizes governance choices as physics.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from each agent's structural position. Island nations as primary victims have d ≈ 0.92 (near-total targets of extraction). High-emitting nations as primary beneficiaries have d ≈ 0.08 (net beneficiaries). AOSIS as organized agents with some negotiating power have d ≈ 0.65 (moderate-high targets but with agency). Development finance institutions as institutional beneficiaries have d ≈ 0.10 (beneficiaries, though with some institutional constraint). The UN climate bureaucracy as institutional observers have d ≈ 0.50 (symmetric — neither pure beneficiary nor victim, but sustained by the constraint's continuation). The analytical observer has d ≈ 0.72 (observing from outside, seeing full structure). These directionality values feed into the sigmoid f(d) to produce effective extractiveness chi for each perspective, scaled by scope modifier σ(global ≈ 1.2). Island perspectives experience high chi despite moderate base extractiveness because their d values amplify the base through f(d). Beneficiary perspectives experience low or negative chi because their d values compress the base.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: The constraint genuinely exhibits both coordination and extraction, so tangled rope is structurally appropriate. The coordination component (climate science collaboration, renewable technology development, disaster response networks) is real but asymmetric — it benefits all parties but disproportionately benefits high-emission nations (who can adopt renewables incrementally) over island states (who must transform economies under threat). The extraction component is unmistakable: costs are imposed on those least responsible. The active enforcement gate is met: climate negotiations, bilateral climate agreements, and development finance conditionality actively maintain the constraint. The constraint would collapse if high-emitting nations were forced to internalize full climate costs or if island states could exit (e.g., by relocating populations, which they cannot). The beneficiaries (high-emitters, development finance institutions) are real. The victims (island nations, future populations) are real. The mandatrophy is resolved by recognizing that the constraint is neither pure coordination (which would be rope) nor pure extraction (which would be snare) — it is genuinely hybrid, with the coordination serving as the mechanism through which extraction occurs. International climate governance is the vehicle: cooperation in climate science and adaptation creates relationships of dependency and institutional capture that enable extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    paris_agreement_sunset_reality,
    'Is the Paris Agreement''s 1.5°C target a genuine sunset clause with binding enforcement, or a performative commitment unlikely to be met?',
    'Monitor global emissions trajectories against 1.5°C pathways; track actual policy implementation vs pledged commitments; assess whether enforcement mechanisms develop real teeth in post-2025 frameworks',
    'If genuine and binding: scaffold classification is correct — constraint is temporary and declining. If performative: constraint persists indefinitely and reclassifies as snare or tangled_rope with theater masking extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(paris_agreement_sunset_reality, empirical, 'Whether Paris Agreement target constitutes real sunset or performative commitment').

omega_variable(
    adaptation_finance_sufficiency,
    'Does climate finance provided to island states adequately cover adaptation costs, or is it tokenistic relative to actual climate damage?',
    'Compare pledged and delivered adaptation finance to independent assessments of actual adaptation requirements; track island state debt accumulation from climate-driven spending; measure outcomes of funded adaptation projects',
    'If sufficient: tangled_rope classification holds — genuine coordination with asymmetric extraction. If insufficient: reclassifies as snare — extraction without meaningful coordination benefit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(adaptation_finance_sufficiency, empirical, 'Whether adaptation finance matches actual climate adaptation needs').

omega_variable(
    emission_allocation_justice,
    'Should responsibility for emissions reductions be allocated by current-year emissions, cumulative historical emissions, or per-capita consumption?',
    'Historical carbon accounting and climate debt frameworks; comparison of outcome distributions across allocation methodologies; assessment of island state claims vs high-emitter resistance',
    'This is a preference and conceptual question: different allocation frames shift the constraint''s directionality and chi values across the board. No empirical resolution possible — outcome depends on which principle is negotiated into binding framework.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(emission_allocation_justice, preference, 'Which emissions allocation principle determines justice in climate sovereignty').

omega_variable(
    internal_island_extraction,
    'Within island states, does the constraint enable internal extraction by elites against vulnerable populations (e.g., displacement for adaptation infrastructure, climate debt servicing)?',
    'Track internal distribution of adaptation costs and benefits within island states; assess whether local elites capture climate finance; monitor intra-state displacement and inequality metrics',
    'If significant internal extraction: the constraint contains nested Snare structures within each island state. The apparent tangled_rope at the international level masks deeper snare dynamics within island governance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internal_island_extraction, empirical, 'Whether island state climate costs are distributed internally or captured by elites').

omega_variable(
    technology_transfer_sufficiency,
    'Does technology transfer for renewable energy and climate adaptation adequately enable island energy independence, or does it perpetuate technological dependency?',
    'Assess capacity building outcomes; measure island state ability to design, manufacture, and maintain renewable systems independently; track intellectual property barriers and licensing costs',
    'If adequate: tangled_rope with genuine coordination component. If insufficient: extraction disguised as coordination — islands remain dependent on external suppliers and expertise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_transfer_sufficiency, empirical, 'Whether technology transfer enables genuine energy independence or perpetuates dependency').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(island_state_climate_sovereignty, 1990, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(iscs_tr_t0, island_state_climate_sovereignty, theater_ratio, 0, 0.35).
narrative_ontology:measurement(iscs_tr_t15, island_state_climate_sovereignty, theater_ratio, 15, 0.5).
narrative_ontology:measurement(iscs_tr_t30, island_state_climate_sovereignty, theater_ratio, 30, 0.61).

% Extraction over time
narrative_ontology:measurement(iscs_be_t0, island_state_climate_sovereignty, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(iscs_be_t15, island_state_climate_sovereignty, base_extractiveness, 15, 0.52).
narrative_ontology:measurement(iscs_be_t30, island_state_climate_sovereignty, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(island_state_climate_sovereignty, global_infrastructure).
narrative_ontology:boltzmann_floor_override(island_state_climate_sovereignty, 0.22).
narrative_ontology:affects_constraint(island_state_climate_sovereignty, small_island_developing_state_debt).
narrative_ontology:affects_constraint(island_state_climate_sovereignty, climate_migration_sovereignty).
narrative_ontology:affects_constraint(island_state_climate_sovereignty, renewable_energy_technology_dependency).
narrative_ontology:affects_constraint(island_state_climate_sovereignty, ocean_acidification_ecosystem_collapse).

% DUAL FORMULATION NOTE:
% Island state climate sovereignty is upstream of multiple specific constraints (climate-driven migration, technology dependency, ecosystem collapse). Each downstream constraint has its own extractiveness reflecting specific mechanisms; this story models the general structural relationship through which extraction occurs. The constraint family shows how a single geopolitical dynamic (vulnerability to external emissions) generates multiple nested extraction mechanisms at different scales.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(island_state_climate_sovereignty, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
