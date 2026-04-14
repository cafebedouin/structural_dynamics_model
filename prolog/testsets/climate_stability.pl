% ============================================================================
% CONSTRAINT STORY: climate_stability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_stability, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: climate_stability
 *   human_readable: Climate Stability as Coordination and Extraction
 *   domain: environmental/geopolitical/economic
 *
 * SUMMARY:
 *   Climate stability represents a boundary constraint on human economic
 *   activity where the physics of greenhouse gas accumulation creates a
 *   global coordination problem with profoundly asymmetric costs. The
 *   constraint exhibits all structural signatures of a Tangled Rope: genuine
 *   coordination function (maintaining energy systems requires solving the
 *   transition problem; all agents benefit from climate stability in
 *   principle) combined with severe asymmetric extraction (high-emission
 *   industrialized economies capture decades of development benefits through
 *   fossil fuel energy while costs are deferred to low-lying states,
 *   subsistence communities, and future generations). The constraint's core
 *   structural tension is that mitigation requires coordinated global
 *   emissions reduction, yet the distribution of costs and benefits makes
 *   unilateral compliance rational for no agent. International climate
 *   governance institutions (UNFCCC, Paris Agreement) form a performative
 *   architecture that increased from 0.35 theater ratio in 1992 to 0.68 by
 *   2026: ceremonial commitments proliferate while actual emissions
 *   trajectories diverge from pledged reductions. Simultaneously,
 *   extractiveness has risen from 0.32 to 0.58 as the cost of climate
 *   stabilization compounds and adaptation becomes visibly necessary. The
 *   constraint is not immutable physical law (though it is bounded by
 *   physical chemistry); it is a contingent institutional arrangement
 *   sustained by economic incentives, power asymmetries, and the difficulty
 *   of coordinating planetary-scale commons.
 *
 * KEY AGENTS:
 *   - Low-lying island states: Primary victims (powerless/trapped) — face existential inundation; bear costs with zero agency
 *   - Subsistence agricultural communities: Primary victims (powerless/trapped) — structural dependency on climate-stable precipitation; zero adaptation options
 *   - Future generations: Primary victims (powerless/trapped) — inherit atmospheric CO2 as fixed constraint; zero decision-making power
 *   - High-emission developed economies: Primary beneficiaries (institutional/arbitrage) — captured decades of fossil fuel energy gains; now face transition costs but retain technological and capital advantages
 *   - Fossil fuel extractors and carbon-intensive industries: Secondary beneficiaries (institutional/arbitrage) — capture extractive rents during transition window; experience pressure to internalize costs through carbon pricing
 *   - Mid-income industrializing nations: Mixed agents (moderate/constrained) — face genuine coordination need (energy access) with extraction overhead (climate pressure); benefit from development, constrained by physics
 *   - Renewable energy coalition: Organized agents (organized/mobile) — perceive exit routes; see constraint as solvable coordination problem
 *   - International climate governance institutions: Institutional performers (institutional/constrained) — maintain ceremonial architecture despite degraded function; experience theater ratio drift
 *   - Climate justice and adaptation coalitions: Organized agents (organized/constrained) — advocate for loss-and-damage funds and adaptation finance; perceive sunset path
 *   - Analytical observer: Civilizational perspective (analytical/analytical) — risks naturalizing institutional choice as physical inevitability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_stability, 0.58).
domain_priors:suppression_score(climate_stability, 0.72).
domain_priors:theater_ratio(climate_stability, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_stability, extractiveness, 0.58).
narrative_ontology:constraint_metric(climate_stability, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(climate_stability, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_stability, tangled_rope).
narrative_ontology:human_readable(climate_stability, "Climate Stability as Coordination and Extraction").
narrative_ontology:topic_domain(climate_stability, "environmental/geopolitical/economic").

domain_priors:requires_active_enforcement(climate_stability).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_stability, high_emission_economies).
narrative_ontology:constraint_beneficiary(climate_stability, fossil_fuel_extractors).
narrative_ontology:constraint_beneficiary(climate_stability, carbon_intensive_industries).
narrative_ontology:constraint_victim(climate_stability, low_lying_island_states).
narrative_ontology:constraint_victim(climate_stability, subsistence_agricultural_communities).
narrative_ontology:constraint_victim(climate_stability, future_generations).
narrative_ontology:constraint_victim(climate_stability, global_ecosystem_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-LYING ISLAND STATES (SNARE) — Face existential inundation with no exit option. Cannot abandon territory; cannot unilaterally stabilize climate; cannot organize collective defense against rising seas. Bear full cost of others' emissions. Maximum suppression and extraction — geographic fate creates absolute dependency.
constraint_indexing:constraint_classification(climate_stability, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SUBSISTENCE AGRICULTURAL COMMUNITIES (SNARE) — Depend entirely on stable precipitation and temperature patterns. Climate destabilization directly destroys livelihood with zero agency. Cannot relocate, cannot industrialize out, cannot lobby effectively. Structural extraction from concentrated emitters.
constraint_indexing:constraint_classification(climate_stability, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: FUTURE GENERATIONS (SNARE) — Completely trapped: have no agency in current decisions, inherit atmospheric carbon concentration as fixed structural constraint. Pure extraction with zero recourse. The most severe powerlessness — the victim class is not yet born.
constraint_indexing:constraint_classification(climate_stability, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 4: MID-INCOME INDUSTRIALIZING NATIONS (TANGLED ROPE) — Face genuine coordination problem (energy access, grid stability) with extraction overhead. Benefit from fossil fuel development for poverty reduction but constrained by climate physics and international pressure. Some exit options (renewable energy transition) exist but at high cost. Mixed experience: real coordination needs + asymmetric extraction from path-dependent development.
constraint_indexing:constraint_classification(climate_stability, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: RENEWABLE ENERGY TRANSITION COALITION (ROPE) — Organized agents (green technology firms, climate-committed governments, renewable energy workers) see the constraint as a coordination problem with exit routes. Solar and wind technology creates genuine alternatives. The constraint is experienced as solvable coordination, not extraction — agents perceive agency and declining barriers. Theater ratio low within this perspective.
constraint_indexing:constraint_classification(climate_stability, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: CARBON-INTENSIVE INDUSTRIALISTS (ROPE) — Primary beneficiaries experience climate stability constraint as pure coordination: internalize carbon costs into pricing, maintain energy security through transition. For well-capitalized actors, the constraint is manageable (renewable investment, offsets, arbitrage in carbon markets). Experiences low extraction — the constraint redistributes to them, not from them.
constraint_indexing:constraint_classification(climate_stability, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: INTERNATIONAL CLIMATE GOVERNANCE (PITON) — Paris Agreement, UNFCCC, and national climate pledges form a performative architecture: ceremonial commitments with low enforcement, theater_ratio rising over time as gap between pledges and emissions grows. Institutions persist through inertia despite degraded function. Suppression rises (countries face pressure to sign agreements but no consequence for non-compliance) while functional climate action stalls.
constraint_indexing:constraint_classification(climate_stability, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: CLIMATE JUSTICE & ADAPTATION FUND COALITION (SCAFFOLD) — Organized agents (island state coalitions, climate finance advocates, adaptation-focused NGOs) perceive the constraint as temporary with a sunset: loss and damage funds, adaptation budgets, and climate reparations mechanisms are building parallel pathways for addressing climate impacts without requiring perfect mitigation first. Low suppression relative to powerless agents because coalition has leverage and exit vision. Scaffold logic applies if sunset timeline is credible (resolvable omega).
constraint_indexing:constraint_classification(climate_stability, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / PHYSICAL LIMITS (MOUNTAIN) — From a civilizational/universal perspective, climate stability reflects inescapable planetary physics: greenhouse gases trap heat; CO2 concentrations determine temperature; atmospheric residence time creates multi-century commitment. This perspective naturalizes climate change as a constraint of physical law, not institutional structure. ENGINE WARNING: This is a false summit — the structural data (beneficiaries, victims, active enforcement) contradicts the mountain classification. The constraint is contingent on economic/political choices, not physical inevitability.
constraint_indexing:constraint_classification(climate_stability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_stability_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(climate_stability, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(climate_stability, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_stability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(climate_stability, TR),
    TR >= 0.70.

:- end_tests(climate_stability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The original research estimated this at 0.72, but the revised assessment reflects that while development asymmetry is real and severe, it is not as extractive as pure Snare (ε ≥ 0.66) because beneficiary economies do face some real transition costs and constraints — they are not pure parasites living off trapped victims. However, the asymmetry is substantial: high-income economies captured ~70% of cumulative fossil fuel benefits (1950-2020) while bearing ~30% of climate costs so far; trajectory continues through 2050. Suppression (0.72): Very high. Multiple mechanisms: (1) structural — developing nations cannot unilaterally reduce emissions without sacrificing growth; (2) informational — carbon accounting obscures where emissions actually occur (outsourced manufacturing); (3) institutional — UNFCCC decisions require consensus, giving each nation veto power; (4) temporal — 20-40 year lag between emissions and climate impacts obscures causality. Theater ratio (0.68): High and rising. Paris Agreement pledges sum to ~2.7°C warming despite 1.5°C target. National commitments are ceremonial — countries issue pledges, hold climate conferences (theater), then proceed with business-as-usual emissions. The theater increased from 0.35 (1992, early optimism) to 0.52 (2015, Paris Agreement) to 0.68 (2026, reality of non-compliance evident). The performative component dominates: countries benefit from being seen as climate-committed without bearing costs of actual reduction.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows why single-perspective analysis fails catastrophically. From the developed-economy perspective (institutional/arbitrage), climate stabilization is a manageable coordination problem — transition costs are real but surmountable, technology is available, markets are working. From the island-state perspective (powerless/trapped), climate change is pure extraction — they did nothing to cause it, cannot escape it, and bear total cost. Both perspectives are observing the same CO2 concentration and temperature trajectory. But their experienced extractiveness differs by a factor of ~5. The framework captures this not by adding new axes but by recognizing that directionality (d) is different for each observer. The beneficiary sees Rope; the victim sees Snare. Neither is hallucinating — each is accurate about their structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (developed economies) get d ≈ 0.05-0.15: they benefit from fossil fuels; extractiveness runs toward them. Victims (island states) get d ≈ 0.90-0.95: they are the target; extractiveness runs from them. Mixed actors (mid-income nations) get d ≈ 0.55-0.70: they face both benefits (development) and costs (climate impacts); the high d reflects that their constrained exit options mean they cannot leverage the development gains to escape climate vulnerability. The temporal dimension is critical: a developed economy could have d ≈ 0.10 for biographical time (they still capture gains faster than costs materialize) but d ≈ 0.50 for civilizational time (by 2100, accumulated costs exceed accumulated gains). The framework captures this by allowing multiple perspectives at different time horizons.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exhibits classic mandatrophy: the classification depends critically on perspective. The beneficiary-institution perspective yields Rope (pure coordination, low extraction perceived). The victim-powerless perspective yields Snare (pure extraction, zero agency). The institutional-performer perspective yields Piton (degraded ritual). The organized-coalition perspective yields Scaffold (temporary problem with sunset through adaptation finance and technology transition). The analytical perspective risks Mountain (naturalizing institutional choice as physical law). No single type is 'correct' — the presheaf of perspectives IS the constraint. The mandatrophy resolves by accepting the perspectival structure as genuine: different observers truly experience different extractiveness values, and this difference is not error but structural fact. The constraint is Tangled Rope at the meta-level: it coordinates genuine energy/development needs while extracting asymmetrically from those without exit options.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tipping_point_irreversibility,
    'At what carbon concentration does climate change transition from reversible policy problem to irreversible physical constraint?',
    'Paleoclimate data (past CO2 thresholds and state transitions); current tipping point research (AMOC, ice sheet stability, Amazon dieback); empirical CO2 trajectory vs. model predictions',
    'If threshold already crossed: constraint is now mountain (physical irreversibility). If threshold far ahead: constraint remains institutional (policy choices). If threshold near and uncertain: classification unstable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tipping_point_irreversibility, empirical, 'Tipping point threshold for irreversible climate change').

omega_variable(
    enforcement_mechanism_coherence,
    'Do international climate agreements (Paris, national commitments, carbon markets) constitute genuine coordination enforcement or performative theater masking non-compliance?',
    'Historical compliance rates; actual emissions trajectory vs. pledged reductions; detection and sanction mechanisms; correlation between agreement intensity and emissions outcomes',
    'If genuine enforcement: tangled_rope classification confirmed (real coordination + real extraction). If performative: piton classification confirmed (degraded ritual). Determines whether institutional actors experience the constraint as binding or as optional signaling.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_mechanism_coherence, empirical, 'Whether climate agreements enforce actual emissions reductions').

omega_variable(
    adaptation_sufficiency_frontier,
    'Can adaptation (resilient agriculture, engineered infrastructure, migration) substitute for mitigation (emissions reduction) at scale?',
    'Agricultural yield stability under 2-3°C warming; infrastructure vulnerability assessments; historical migration capacity vs. projected climate migration; cost comparisons (mitigation vs. adaptation)',
    'If adaptation sufficient: moderate agents (mid-income nations) move from constrained to mobile exit options; classification shifts from Tangled Rope toward Rope. If adaptation insufficient: victims become more trapped; extraction increases. Determines whether the constraint is escapable through market mechanisms.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(adaptation_sufficiency_frontier, empirical, 'Whether adaptation can substitute for mitigation at climate impact scales').

omega_variable(
    beneficiary_lockout_persistence,
    'As renewable energy becomes cheaper than fossil fuels, do high-emission economies lose their beneficiary status through technological displacement or maintain extraction through path-dependent institutions?',
    'Cost trajectory of renewables vs. fossil fuels; renewable adoption rates across high-income economies; institutional barriers to grid transition; fossil fuel subsidy persistence despite cost disadvantage',
    'If technological displacement: beneficiary class erodes; extraction mechanism weakens; constraint moves toward rope. If institutional lockout persists: beneficiaries maintain extraction through regulatory capture despite cost disadvantage; extraction becomes increasingly parasitic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_lockout_persistence, empirical, 'Whether high-emission beneficiaries maintain extraction as technological advantage erodes').

omega_variable(
    generational_identity_lock_mechanism,
    'To what extent do future generations'' climate vulnerability derive from structural economic dependency on fossil fuel infrastructure versus identity-locked institutional commitment to carbon-intensive development models?',
    'Comparative policy analysis (countries with equivalent development levels but different climate commitments); institutional path dependence studies; narrative analysis of climate denialism and delay (cover story prevalence); economic modeling of transition costs vs. climate damages',
    'If structural dependency dominates: the constraint is Snare for future generations (structural immobility). If identity-lock dominates: the constraint is Rope from their perspective (perceptually changeable; actual change blocked by self-concept as ''developed industrialized nation''). Affects whether the constraint is seen as natural law (mountain) or institutional choice (rope/snare/tangled).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generational_identity_lock_mechanism, conceptual, 'Whether future generations are locked by structural dependency or institutional identity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_stability, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_stability, theater_ratio, 0, 0.35).
narrative_ontology:measurement(clim_tr_t20, climate_stability, theater_ratio, 20, 0.52).
narrative_ontology:measurement(clim_tr_t40, climate_stability, theater_ratio, 40, 0.68).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_stability, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(clim_be_t20, climate_stability, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(clim_be_t40, climate_stability, base_extractiveness, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_stability, global_infrastructure).
narrative_ontology:affects_constraint(climate_stability, fossil_fuel_subsidy_lock).
narrative_ontology:affects_constraint(climate_stability, carbon_intensive_infrastructure_path_dependency).
narrative_ontology:affects_constraint(climate_stability, climate_migration_border_controls).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_stability, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
