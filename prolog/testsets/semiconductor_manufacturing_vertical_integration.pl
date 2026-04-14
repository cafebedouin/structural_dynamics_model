% ============================================================================
% CONSTRAINT STORY: semiconductor_manufacturing_vertical_integration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_semiconductor_manufacturing_vertical_integration, []).

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
 *   constraint_id: semiconductor_manufacturing_vertical_integration
 *   human_readable: Semiconductor Manufacturing Vertical Integration
 *   domain: industrial_organization/technology_policy
 *
 * SUMMARY:
 *   Semiconductor manufacturing vertical integration — the consolidation of
 *   design, fabrication, process technology development, assembly, and
 *   testing within single corporate entities — creates a structurally
 *   asymmetric constraint on the semiconductor ecosystem. The constraint
 *   operates through two mechanisms: (1) technical lock-in, where
 *   process-specific design knowledge creates genuine switching costs, and
 *   (2) institutional lock-in, where exclusive relationships, proprietary
 *   secrecy, and withholding of leading-edge access maintain extraction
 *   beyond technical necessity. Over the 20-year measurement interval
 *   (2004-2024), extractiveness has increased from 0.35 to 0.58 as
 *   leading-edge nodes (7nm, 5nm, 3nm) have become increasingly concentrated
 *   in three foundries (TSMC, Samsung, Intel). Theater ratio has also
 *   increased from 0.32 to 0.48, reflecting that proprietary process control
 *   claims have become increasingly performative — the actual technical
 *   barriers are arguably lower than claimed, but the institutional barriers
 *   (exclusive foundry agreements, long qualification timelines, IP
 *   restrictions) enforce the constraint regardless. The constraint exhibits
 *   genuine coordination benefits (economies of scale, yield optimization,
 *   design-process co-optimization) alongside asymmetric extraction
 *   (restricted access to leading-edge nodes, forced bundling, information
 *   asymmetry). This hybrid structure classifies as Tangled Rope from the
 *   analytical perspective and as Snare from the perspectives of foundries
 *   and fabless designers.
 *
 * KEY AGENTS:
 *   - Integrated Device Manufacturers (IDMs): Primary beneficiaries (institutional/arbitrage) — TSMC, Samsung, Intel, SK Hynix capture value through supply chain control, proprietary process access, and exclusive customer relationships
 *   - Foundry Ecosystem: Primary victims (powerless/trapped) — Small and medium foundries locked into supply chains by capital requirements, process technology dependencies, and exclusive agreements
 *   - Fabless Designers: Secondary victims (moderate/constrained) — Design companies dependent on foundry access; face 18-24 month switching costs and restricted access to leading-edge process nodes
 *   - Open-Source Hardware Coalition: Organized victim-beneficiary (organized/constrained) — Benefits from standardized EDA and design methodologies; constrained by restricted access to advanced process nodes
 *   - Supply Chain Transparency: Victim (powerless/trapped) — Abstract collective good; asymmetric information maintained by proprietary secrecy prevents ecosystem-wide optimization
 *   - Analytical Observer: Civilizational analyst (analytical/analytical) — Can perceive the hybrid structure; identifies both genuine coordination and asymmetric extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(semiconductor_manufacturing_vertical_integration, 0.58).
domain_priors:suppression_score(semiconductor_manufacturing_vertical_integration, 0.52).
domain_priors:theater_ratio(semiconductor_manufacturing_vertical_integration, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(semiconductor_manufacturing_vertical_integration, extractiveness, 0.58).
narrative_ontology:constraint_metric(semiconductor_manufacturing_vertical_integration, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(semiconductor_manufacturing_vertical_integration, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(semiconductor_manufacturing_vertical_integration, tangled_rope).
narrative_ontology:human_readable(semiconductor_manufacturing_vertical_integration, "Semiconductor Manufacturing Vertical Integration").
narrative_ontology:topic_domain(semiconductor_manufacturing_vertical_integration, "industrial_organization/technology_policy").

domain_priors:requires_active_enforcement(semiconductor_manufacturing_vertical_integration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(semiconductor_manufacturing_vertical_integration, integrated_device_manufacturers).
narrative_ontology:constraint_beneficiary(semiconductor_manufacturing_vertical_integration, supply_chain_opacity_maintainers).
narrative_ontology:constraint_victim(semiconductor_manufacturing_vertical_integration, foundry_ecosystem).
narrative_ontology:constraint_victim(semiconductor_manufacturing_vertical_integration, fabless_designers).
narrative_ontology:constraint_victim(semiconductor_manufacturing_vertical_integration, supply_chain_transparency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FOUNDRY ECOSYSTEM (SNARE) — Small foundries and contract manufacturers locked into supply chains with no exit without massive capital reinvestment. Asymmetric information, proprietary process technology, and exclusive agreements create structural entrapment. The foundry bears costs of vertical integration by larger players while capturing minimal coordination benefit.
constraint_indexing:constraint_classification(semiconductor_manufacturing_vertical_integration, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: FABLESS DESIGNERS (SNARE) — Constrained by high switching costs between manufacturing partners, long qualification timelines, and locked-in process node dependencies. Fabless firms face significant barriers to exit; switching manufacturers costs 18-24 months and millions in re-engineering. The constraint extracts through forced bundling of design services with manufacturing and restricted access to leading-edge nodes.
constraint_indexing:constraint_classification(semiconductor_manufacturing_vertical_integration, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: OPEN-SOURCE HARDWARE COALITION (TANGLED ROPE) — Organized agents (EDA consortia, open-source chip projects, academic fab initiatives) benefit from vertical integration through lower design costs and standardized toolchains, but simultaneously face extraction through restricted access to leading-edge process technology. The coalition has some exit capacity (alternative fabs, cross-licensing) but remains constrained by technological asymmetry. Both genuine coordination and asymmetric extraction coexist.
constraint_indexing:constraint_classification(semiconductor_manufacturing_vertical_integration, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: INTEGRATED DEVICE MANUFACTURERS (ROPE) — Major IDMs (TSMC, Samsung, Intel, SK Hynix) experience the vertical integration constraint as pure coordination: capturing all stages of production (design, fabrication, assembly, testing) solves economies of scale and supply chain coordination. The IDM benefits from the constraint through preferential node access, proprietary process advantage, and supply chain control. Effective extraction flows toward this agent.
constraint_indexing:constraint_classification(semiconductor_manufacturing_vertical_integration, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: PROCESS TECHNOLOGY LOCK-IN (PITON) — Vertically integrated process control persists partly through technological necessity (precision, yield, customization) but increasingly through institutional inertia and exclusive relationships. The theater_ratio (0.48) reflects that much of the proprietary process control is performative — the actual technical barriers are lower than claimed, sustained by secrecy agreements and exclusive foundry relationships that have become ritual. Legacy fab partnerships persist despite superior open alternatives emerging.
constraint_indexing:constraint_classification(semiconductor_manufacturing_vertical_integration, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational vantage, vertical integration provides genuine coordination benefits (yield optimization, design-process co-optimization, supply assurance) but also extracts through information asymmetry, proprietary lock-in, and restricted access to leading-edge technology. The constraint is neither pure coordination nor pure extraction — it is an asymmetric hybrid where the coordination benefits are real but inequitably distributed.
constraint_indexing:constraint_classification(semiconductor_manufacturing_vertical_integration, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(semiconductor_manufacturing_vertical_integration_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(semiconductor_manufacturing_vertical_integration, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(semiconductor_manufacturing_vertical_integration, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(semiconductor_manufacturing_vertical_integration, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(semiconductor_manufacturing_vertical_integration, TR),
    TR >= 0.70.

:- end_tests(semiconductor_manufacturing_vertical_integration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, reflecting genuine coordination benefits (economies of scale, yield optimization) alongside asymmetric extraction (restricted node access, switching costs). The extraction is not total because the foundry ecosystem does provide genuine benefits to users — leading-edge fabrication, proven processes, supply assurance. However, the benefits are inequitably distributed. IDMs capture disproportionate value through control of process technology and exclusive customer relationships. The extractiveness trajectory (0.35→0.58 over 20 years) reflects concentration in leading-edge nodes and increasing institutional lock-in through exclusive agreements and proprietary IP claims. Suppression (0.52): Moderate. Barriers to exit include capital requirements for competing foundries ($10B+), process technology complexity, long qualification timelines (18-24 months), and institutional relationships that create switching costs. However, suppression is not total — open-source alternatives exist (MOSIS, efabless, IHP), and foundries can qualify with alternative partners at cost. Theater ratio (0.48): Moderate. The constraint has both genuine technical components (precision requirements, process yield, design-process co-optimization) and performative components (proprietary secrecy claims that exceed technical necessity, exclusive relationship rituals, qualification theater). The increasing theater ratio (0.32→0.48) suggests that institutional lock-in has grown faster than technical lock-in, and secrets are increasingly maintaining the constraint relative to genuine technical barriers.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural phenomenon classifies differently depending on the observer's power, exit options, and time horizon. An IDM at institutional/immediate level sees Rope (coordination). A foundry at powerless/biographical level sees Snare (extraction). An organized open-source coalition at organized/generational level sees Tangled Rope (mixed). The analytical observer at civilizational level sees Tangled Rope (hybrid with inequitable distribution). The piton perspective reveals that the constraint is increasingly maintained through institutional inertia — exclusive relationships and proprietary secrets persist even as open alternatives mature, suggesting the constraint is being maintained by enforcement rather than by technical necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from structural position: IDMs as beneficiaries with arbitrage options (low d, negative chi); foundries as victims with trapped/constrained options (high d, high chi); open-source hardware as organized agents with constrained exit (moderate d, moderate chi); analytical observer as neutral analyst (canonical d ≈ 0.73). The IDM's arbitrage exit option means they can freely reallocate capital between foundry operations and fabless designs, experiencing the constraint as coordination with negative effective extraction (they benefit). The foundry's trapped/constrained options mean they cannot easily exit without massive reinvestment, experiencing high effective extraction. The open-source coalition has moderate d because they have some exit capacity (alternative fabs, cross-licensing) but face technological barriers that prevent full escape. The extraction flow is directional: value flows from foundries and fabless designers toward IDMs, making d asymmetric across the ecosystem.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: The constraint qualifies as Tangled Rope under the canonical classifier: (1) beneficiaries exist (IDMs benefit from vertical integration through supply chain control and process access), (2) victims exist (foundries and fabless designers face extraction through lock-in and access restrictions), (3) active enforcement exists (exclusive agreements, IP restrictions, long qualification timelines enforce the constraint). The mandatrophy is resolved by recognizing that vertical integration provides genuine coordination benefits (economies of scale, yield optimization, design-process co-optimization) that justify some asymmetry, but the distribution of benefits is inequitable, and institutional lock-in maintains the constraint beyond technical necessity. The theater ratio (0.48) indicates that roughly half the constraint's persistence is performative (proprietary secrecy, exclusive relationship theater) rather than technically necessary. The extractiveness trajectory (0.35→0.58) reveals increasing asymmetry: concentration in leading-edge nodes has made exit more costly, and IDMs have captured disproportionate value. The constraint is neither pure coordination (Rope) because asymmetric extraction exists, nor pure extraction (Snare) because genuine coordination benefits exist and are widely distributed. Tangled Rope is the correct classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technical_necessity_vs_institutional_lock_in,
    'How much of vertical integration is technically necessary (precision, yield optimization, supply assurance) versus institutionally locked-in (exclusive relationships, proprietary secrets, withholding leading-edge access)?',
    'Comparative analysis of open-source foundry success rates (MOSIS, efabless, IHP) versus proprietary fabs; measurement of yield gaps, process control, and design flexibility; historical analysis of when exclusive relationships replaced technical necessity',
    'If 70%+ technical: reclassify as Rope (coordination primary). If 50%+ institutional lock-in: keep Tangled Rope (mixed). If 70%+ institutional: reclassify as Snare (extraction primary).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technical_necessity_vs_institutional_lock_in, empirical, 'Technical versus institutional components of vertical integration').

omega_variable(
    open_source_fab_scalability,
    'Can open-source foundries (MOSIS, efabless, IHP) scale to handle advanced nodes and high-volume production, or are they structurally limited to niche/legacy nodes?',
    'Tracking open-source fab capacity expansion, yield metrics, process maturity (28nm, 16nm, 7nm); comparison of design ecosystem migration rates; measurement of time-to-maturity for new open process nodes',
    'If open fabs mature to 5nm: sunset on Tangled Rope constraint — extractive vertical integration loses structural necessity. If locked at 28nm+: vertical integration remains structurally extractive for advanced-node designers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(open_source_fab_scalability, empirical, 'Scalability of open-source foundry alternatives').

omega_variable(
    supply_chain_transparency_reversal,
    'Would forced transparency (public process specs, cross-licensing, open EDA tools) reduce extraction faster than waiting for open alternatives to mature?',
    'Policy simulation; comparison of regulatory transparency regimes (EU Digital Services Act precedent); analysis of cross-licensing history in semiconductor industry; measurement of extraction levels before/after policy interventions in adjacent industries',
    'If transparency-first works: constraint reclassifies as Scaffold with policy sunset. If it fails: constraint remains Tangled Rope, and sunset depends on open-source technological maturation, not policy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(supply_chain_transparency_reversal, preference, 'Policy versus technological pathways to constraint resolution').

omega_variable(
    geographic_fragmentation_effect,
    'Does geographic redundancy (foundries in Taiwan, South Korea, US, Europe) reduce extraction by increasing competitive alternatives, or does coordination between IDMs preserve the constraint despite geographic distribution?',
    'Network analysis of foundry partnerships, exclusive agreements, and technology-sharing arrangements; measurement of inter-fab competition and price differentiation; historical analysis of foundry capacity allocation during supply-demand imbalances',
    'If geographic fragmentation breaks coordination: constraint drops to Snare (pure extraction). If IDMs maintain coordination despite geography: geographic redundancy is performative, and constraint remains Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geographic_fragmentation_effect, empirical, 'Whether geographic foundry diversity reduces extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(semiconductor_manufacturing_vertical_integration, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(semiv_tr_t0, semiconductor_manufacturing_vertical_integration, theater_ratio, 0, 0.32).
narrative_ontology:measurement(semiv_tr_t10, semiconductor_manufacturing_vertical_integration, theater_ratio, 10, 0.4).
narrative_ontology:measurement(semiv_tr_t20, semiconductor_manufacturing_vertical_integration, theater_ratio, 20, 0.48).
narrative_ontology:measurement(semiv_tr_t5, semiconductor_manufacturing_vertical_integration, theater_ratio, 5, 0.36).

% Extraction over time
narrative_ontology:measurement(semiv_be_t0, semiconductor_manufacturing_vertical_integration, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(semiv_be_t10, semiconductor_manufacturing_vertical_integration, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(semiv_be_t20, semiconductor_manufacturing_vertical_integration, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(semiv_be_t5, semiconductor_manufacturing_vertical_integration, base_extractiveness, 5, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(semiconductor_manufacturing_vertical_integration, resource_allocation).
narrative_ontology:boltzmann_floor_override(semiconductor_manufacturing_vertical_integration, 0.18).
narrative_ontology:affects_constraint(semiconductor_manufacturing_vertical_integration, semiconductor_supply_chain_concentration).
narrative_ontology:affects_constraint(semiconductor_manufacturing_vertical_integration, process_technology_lock_in).
narrative_ontology:affects_constraint(semiconductor_manufacturing_vertical_integration, fabless_foundry_dependency).
narrative_ontology:affects_constraint(semiconductor_manufacturing_vertical_integration, open_source_chip_design_barriers).

% DUAL FORMULATION NOTE:
% Semiconductor vertical integration is part of a constraint family covering supply chain consolidation (upstream: supply concentration, downstream: foundry dependency, process lock-in, open-source barriers). This story focuses on the institutional structure of vertical integration itself; sibling stories decompose supply chain transparency, geographic redundancy, and open-source alternative pathways. Link via affects_constraints to track ecosystem-wide impacts of concentration.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(semiconductor_manufacturing_vertical_integration, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
