% ============================================================================
% CONSTRAINT STORY: gpu_semiconductor_supply
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpu_semiconductor_supply, []).

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
 *   constraint_id: gpu_semiconductor_supply
 *   human_readable: GPU Semiconductor Supply Chain Constraint
 *   domain: economic/technological
 *
 * SUMMARY:
 *   The GPU semiconductor supply chain constraint represents a global
 *   bottleneck in computational capacity driven by explosive AI research
 *   demand colliding with constrained manufacturing capacity and concentrated
 *   control. Between 2020-2026, GPU demand for large-language model training
 *   and fine-tuning increased 500x while manufacturing capacity grew at
 *   15-20% annually, creating a persistent supply deficit. The constraint
 *   exhibits the full spectrum of DR classifications depending on agent
 *   structural position: trapped researchers in under-resourced institutions
 *   experience pure extraction (Snare); GPU manufacturers experience genuine
 *   coordination with beneficial asymmetry (Rope); organized research
 *   consortiums and national governments coordinate GPU access with mixed
 *   coordination-extraction (Tangled Rope); alternative compute initiatives
 *   persist through institutional inertia despite slow practical displacement
 *   (Piton); the civilizational analytical observer risks naturalizing a
 *   contingent institutional arrangement (false summit Mountain). The core
 *   tension is between the legitimate coordination problem (distributing
 *   scarce computational capacity requires allocation mechanisms) and the
 *   extractive mechanisms through which that coordination is implemented
 *   (CUDA ecosystem lock-in, priority allocation to established labs,
 *   geographic concentration of manufacturing, pricing power maintenance
 *   through supply restriction).
 *
 * KEY AGENTS:
 *   - GPU Manufacturers (NVIDIA, AMD, Intel): Primary beneficiary (institutional/arbitrage) — capture pricing premiums, ecosystem lock-in revenue, and buyer dependency during scarcity period
 *   - Resource-Constrained Researchers: Primary victim (powerless/trapped) — cannot access computational capacity at affordable cost; cannot exit constraint within biographical horizon; research agendas completely suppressed
 *   - Emerging AI Startups: Secondary victim (moderate/constrained) — face high capital barriers to GPU acquisition; can theoretically exit via acquisition, relocation, or alternative compute, but at prohibitive cost
 *   - Established AI Labs (FAIR, DeepMind, OpenAI, major university departments): Beneficiary (institutional/constrained or institutional/mobile) — benefit from priority allocation and ecosystem standardization; participate in consortiums that coordinate access
 *   - National Computing Authorities (DOE, EU, Chinese government): Organized beneficiary/victim (organized/constrained) — coordinate domestic GPU access for strategic AI development while facing geopolitical supply competition
 *   - Alternative Compute Movements (chiplet, neuromorphic, optical, ASIC): Organized actor (organized/constrained) — pursue decomposition of monolithic GPU supply via technological alternatives; persistence despite slow adoption suggests institutional inertia (piton classification)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements (CUDA lock-in, fab concentration) as immutable physical laws
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpu_semiconductor_supply, 0.58).
domain_priors:suppression_score(gpu_semiconductor_supply, 0.68).
domain_priors:theater_ratio(gpu_semiconductor_supply, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpu_semiconductor_supply, extractiveness, 0.58).
narrative_ontology:constraint_metric(gpu_semiconductor_supply, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(gpu_semiconductor_supply, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpu_semiconductor_supply, tangled_rope).
narrative_ontology:human_readable(gpu_semiconductor_supply, "GPU Semiconductor Supply Chain Constraint").
narrative_ontology:topic_domain(gpu_semiconductor_supply, "economic/technological").

domain_priors:requires_active_enforcement(gpu_semiconductor_supply).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpu_semiconductor_supply, gpu_manufacturers).
narrative_ontology:constraint_beneficiary(gpu_semiconductor_supply, established_ai_labs).
narrative_ontology:constraint_victim(gpu_semiconductor_supply, resource_constrained_researchers).
narrative_ontology:constraint_victim(gpu_semiconductor_supply, emerging_ai_startups).
narrative_ontology:constraint_victim(gpu_semiconductor_supply, global_computational_equity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RESOURCE-CONSTRAINED RESEARCHER (SNARE) — Trapped by absolute GPU scarcity. Cannot exit the constraint (computational research requires GPUs); cannot access scarce supply at affordable cost; faces complete suppression of research agenda unless institutional funding covers premium allocation. No alternatives exist for large-scale model training within the biographical horizon.
constraint_indexing:constraint_classification(gpu_semiconductor_supply, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EMERGING AI STARTUP (SNARE) — Constrained by high capital requirements (premium GPU acquisition, access to cutting-edge architectures via allocation queues, geographic concentration of manufacturing). Can potentially exit through acquisition, alternative compute, or geographic arbitrage, but at prohibitive cost. Biographical horizon too short to absorb multi-year waitlists. High suppression: startup survival depends on near-term computational capacity access.
constraint_indexing:constraint_classification(gpu_semiconductor_supply, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: GPU MANUFACTURER (ROPE) — Experiences the constraint as pure coordination: distributing scarce computational resources enables the global AI ecosystem development that drives long-term demand growth. Immediate extraction benefit (premium pricing, buyer lock-in via CUDA ecosystem), but genuine coordination function (enabling research that validates AI feasibility drives future manufacturing demand). Net beneficiary via arbitrage: can exit allocation decisions by shifting production, negotiating supply contracts, or developing proprietary architectures.
constraint_indexing:constraint_classification(gpu_semiconductor_supply, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ESTABLISHED AI RESEARCH CONSORTIUM (TANGLED ROPE) — Organized actors (major universities, government labs, large tech companies) coordinate resource pooling via consortiums (National Supercomputing Centers, industry partnerships). Coordination function: shared GPU clusters enable multiple research groups to operate within constrained supply. Asymmetric extraction: priority access concentrates benefits toward large consortium members; smaller members pay coordination overhead without proportional benefit. Constrained exit: generational commitment to consortium infrastructure creates path dependency, but members could theoretically exit via independent acquisition or geographic relocation.
constraint_indexing:constraint_classification(gpu_semiconductor_supply, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: NATIONAL COMPUTING AUTHORITY (TANGLED ROPE) — State-level actors (US Department of Energy, EU digital sovereignty initiatives, Chinese government computing mandates) coordinate domestic GPU access via strategic allocation. Coordination function: state-level GPU infrastructure enables national AI competitiveness. Asymmetric extraction: access is conditional on alignment with state priorities (research direction, technology export restrictions, location requirements). Constrained exit: geopolitical competition locks states into GPU procurement strategies; exit means ceding computational capacity to rival states.
constraint_indexing:constraint_classification(gpu_semiconductor_supply, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: CHIPLET/ALTERNATIVE COMPUTE MOVEMENT (PITON) — Organized efforts to decompose GPU monoliths into chiplet designs, develop specialized ASICs (tensor processors, inference accelerators), and promote open-source compute (TPUs via TensorFlow, custom silicon). These alternatives have been theoretically available for years (chiplets, ASICs, TPU development) but remain theater-heavy: adoption requires rewriting software stacks, abandoning CUDA ecosystem benefits, accepting performance trade-offs. The movement persists through institutional inertia and optimism despite slow practical displacement of monolithic GPUs. Low functional uptake despite high organizational effort suggests the constraint persists because institutional interests maintain the monolith (NVIDIA lock-in, developer familiarity, established supply chain).
constraint_indexing:constraint_classification(gpu_semiconductor_supply, piton,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal scale, GPU supply constraints appear as immutable physical laws: semiconductor manufacturing requires rare materials (tantalum, cobalt, rare earth elements), operates at nanometer scales approaching physical limits, and demands massive capital infrastructure ($20B+ fabrication plants). The bottleneck reflects fundamental manufacturing physics and resource scarcity, not contingent institutional arrangements. However, structural data reveals this as a false summit: the extraction mechanisms (NVIDIA's CUDA lock-in, allocation prioritization of established labs, geographic concentration of manufacturing, export restrictions) are socially contingent, not physical necessities. Alternative computational substrates (optical, neuromorphic, quantum) are theoretically feasible but suppressed by ecosystem lock-in.
constraint_indexing:constraint_classification(gpu_semiconductor_supply, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpu_semiconductor_supply_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(gpu_semiconductor_supply, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gpu_semiconductor_supply, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(gpu_semiconductor_supply, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(gpu_semiconductor_supply, TR),
    TR >= 0.70.

:- end_tests(gpu_semiconductor_supply_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, reflecting significant asymmetry in access and pricing but not total deprivation. The base value captures that manufacturers capture sustained pricing premiums during the scarcity period (estimated 2-4x markup over manufacturing cost) and that established labs receive priority allocation. However, extractiveness is not at snare-floor (0.66) because genuine coordination function exists: the allocation mechanisms enable distributed research that validates AI feasibility and drives long-term demand. The measurement trajectory shows extractiveness rising from 0.32 (early scarcity, perceived as temporary coordination problem) to 0.58 (sustained bottleneck, perceived as maintained extraction), indicating that agents have revised their assessment from temporary shortage to systematic constraint. Suppression (0.68): High. Significant barriers to exit include: (1) absolute scarcity (no substitute computational substrates at equivalent performance), (2) ecosystem lock-in (CUDA dominance, research tool chains standardized on NVIDIA), (3) capital barriers (GPU clusters require $1-100M capital investment), (4) geographic concentration (TSMC fab concentration creates chokepoint control), (5) institutional access restrictions (priority allocation by institutional prestige, not merit). Suppression is not 0.95 (complete) because some alternatives exist: geographic arbitrage (accessing distributed clusters), temporal arbitrage (queuing for access), capability arbitrage (reframing research for lower-compute methods). Theater ratio (0.45): Moderate-low. Unlike institutional review (which is largely performative), GPU allocation mechanisms have high functional content: manufacturers do genuinely face capacity constraints and must make allocation decisions; researchers do receive access proportional (roughly) to contributions; research outcomes do emerge from the allocated computing time. However, theater is not zero because: (1) alternative compute substrates are underexplored due to ecosystem inertia, (2) some allocation prioritizes prestige over research merit, (3) geographic concentration creates artificial scarcity (manufacturing capacity exists globally but is concentrated for historical/economic reasons, not physical necessity). Claimed type (Tangled Rope): The base properties satisfy tangled rope gates: (1) requires_active_enforcement: true (allocation decisions are actively made, priority queues are actively maintained, CUDA standardization is actively enforced via ecosystem investment), (2) beneficiaries declared (GPU manufacturers, established AI labs), (3) victims declared (resource-constrained researchers, emerging startups, global computational equity), (4) both coordination function (distributing scarce capacity enables research) and asymmetric extraction (power asymmetry in allocation and pricing).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates why single-perspective analysis fails. The GPU manufacturer sees Rope (pure coordination — enabling global AI development drives demand growth). The trapped researcher sees Snare (pure extraction — complete suppression of research agenda, no exit options, maximum experienced asymmetry). The established research consortium sees Tangled Rope (coordination via consortiums enabling pooled access, but asymmetric benefit concentration toward large members). The national computing authority sees Tangled Rope (state-level coordination of domestic AI capacity, but conditional on state alignment, geopolitical constraints on exit). The alternative compute movement sees Piton (initiatives to decompose the constraint exist but persist in low-adoption theater mode). The civilizational analyst risks Mountain (naturalizing the constraint as immutable physics of semiconductor manufacturing at nanoscale), but structural data reveals this as a false summit: CUDA ecosystem lock-in, geographic fab concentration, and allocation prioritization by prestige are socially contingent institutional arrangements, not laws of physics. The perspectival gap reveals the core analytical move: what appears to a beneficiary as legitimate coordination enabling global research (Rope) appears to a trapped agent as pure extraction via institutional power asymmetry (Snare). Both perceptions are structurally accurate from their respective positions — the gap is not disagreement about facts but about experienced asymmetry in the same institutional structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality d (agent's structural position on the extraction flow from 0=full beneficiary to 1=full target) is derived from three inputs: agent power, exit options, and beneficiary/victim status. GPU manufacturers (institutional/arbitrage/beneficiary): d ≈ 0.05 (strongly beneficiary, arbitrage escape options mean they experience scarcity as revenue driver, not constraint). Established AI labs (institutional/constrained/beneficiary): d ≈ 0.25 (beneficiary but constrained exit — consortium commitment creates path dependency; they capture priority allocation but cannot easily exit or shift suppliers). Resource-constrained researchers (powerless/trapped/victim): d ≈ 0.95 (strongly target, no exit options, complete suppression of research agenda). Emerging startups (moderate/constrained/victim): d ≈ 0.75 (target but some agency — theoretical exit via acquisition, relocation, or capital raising, but costs are prohibitive). National authorities (organized/constrained/mixed): d ≈ 0.50-0.65 depending on resource level — geopolitical constraints limit exit (cannot cede computational capacity to rivals), but some agency in allocation decisions and domestic procurement strategies. Alternative compute movements (organized/constrained/secondary victim): d ≈ 0.55 (mixed — benefit from NVIDIA ecosystem dominance limiting competition, but constrained by switching costs and slow technology maturation). These d values feed the sigmoid f(d) to compute experienced extractiveness chi, which varies across perspectives from nearly zero (manufacturers) to maximum (trapped researchers). The perspectival gap in chi values is the diagnostic signature of the constraint's asymmetric structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The GPU supply constraint resolves mandatrophy by revealing that the bottleneck contains genuine coordination function layered with extractive mechanisms. This is exactly what Tangled Rope captures: the constraint both enables (coordinates access to scarce capacity) and extracts (benefits accrue asymmetrically to manufacturers and established labs). The false summit risk lies in the mountain classification — if the analytical observer concludes the constraint is immutable law (physics of semiconductor manufacturing), policy responses focus on adaptation and equitable access within assumed scarcity. If the constraint is recognized as tangled rope with extractive components maintainable via institutional arrangements (CUDA lock-in, fab concentration, allocation control), policy responses can target the extractive mechanisms (ecosystem diversification, geographic fab distribution, alternative compute support). The mandatrophy is resolved by accepting that the classification is observer-relative: from the trapped researcher's position, the constraint is pure extraction (snare); from the manufacturer's position, it is coordination (rope); from the organized consortium's position, it is mixed (tangled rope). The presheaf over all observer positions — capturing powerless, moderate, powerful, organized, institutional, and analytical perspectives — is the complete characterization. No single type eliminates the others; instead, they form a configuration that reveals institutional structures only visible from the combination of views.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    manufacturing_capacity_constraint_mechanism,
    'Is the GPU supply bottleneck primarily a supply constraint (insufficient fabs/materials) or a demand constraint (manufacturers deliberately limiting production to maintain pricing power)?',
    'Historical analysis of manufacturing utilization rates, capital investment trends, and pricing elasticity; comparison with supply/demand for mature semiconductors (DRAM, logic) where scarcity dynamics differ',
    'If primarily supply: constraint is closer to mountain (physical limits on capacity scaling). If primarily demand: constraint is extraction mechanism (manufacturers maintaining scarcity premium), pushing classification toward snare. This determines whether the bottleneck is inherent or deliberately maintained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(manufacturing_capacity_constraint_mechanism, empirical, 'Whether GPU scarcity is supply-driven or demand-maintained').

omega_variable(
    cuda_ecosystem_lock_in_necessity,
    'How much of the GPU monolith persistence is driven by genuine technical superiority of CUDA-optimized architectures versus ecosystem lock-in and switching cost barriers?',
    'Performance benchmarking of equivalent workloads across CUDA, ROCm, custom TPU, and open-source alternatives; developer migration cost analysis; adoption rate trajectory of alternative accelerators in research contexts',
    'If lock-in is dominant: the constraint is extractive and maintainable via ecosystem control (snare from powerless perspective). If technical superiority is genuine: constraint reflects legitimate market preference, softening snare classification toward tangled rope (coordination benefit from standardization outweighs extraction cost).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cuda_ecosystem_lock_in_necessity, empirical, 'Whether CUDA persistence is technical merit or lock-in').

omega_variable(
    geographic_concentration_necessity,
    'Is the concentration of advanced GPU manufacturing in Taiwan/South Korea/US (TSMC, Samsung, Intel foundries) technologically necessary or a consequence of historical fab investment patterns and capital consolidation?',
    'Analysis of fab startup failure rates by region; capital requirements for entering advanced node manufacturing; technology transfer outcomes in geographic expansion attempts; correlation between geographic origin and technical capability',
    'If necessary: supply chain concentration is unavoidable (mountain-adjacent). If contingent: geographic diversification could increase supply resilience and reduce extraction mechanisms (tangled rope from organized actors softens as supply distribution broadens).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geographic_concentration_necessity, empirical, 'Whether GPU fab concentration is technologically necessary').

omega_variable(
    ai_research_computational_necessity,
    'What fraction of current AI research genuinely requires large-scale GPU training versus pursuing GPU-scale research because it is possible and prestigious within resource-rich institutions?',
    'Meta-analysis of published AI research by resource requirements; comparison of research novelty/impact against computational budget; archival analysis of research design rationales; surveys of researcher motivation for scale choices',
    'If most research requires GPUs: bottleneck is inherent to research frontier (constraint is legitimate coordination problem). If substantial proportion is demand-inflated by prestige effects: demand is partly contingent, and the bottleneck partially reflects extractive institutional arrangements (snare severity lower than measured).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ai_research_computational_necessity, empirical, 'How much GPU demand is research-necessary versus prestige-driven').

omega_variable(
    alternative_substrate_suppression_mechanism,
    'What fraction of slow adoption of neuromorphic, optical, and analog AI hardware reflects genuine technical immatureness versus ecosystem disincentives (CUDA dominance, established tool chains, funding concentrated in NVIDIA-compatible research)?',
    'Patent analysis of alternative substrate maturity; performance trajectory curves for neuromorphic/optical systems vs historical GPU improvements; funding allocation trends across substrate types; researcher surveys on technical obstacles vs institutional barriers',
    'If technical immaturity dominates: piton classification is correct (alternatives are genuinely inferior). If ecosystem suppression dominates: the bottleneck is maintained extraction (snare), and alternative substrates could relieve pressure if institutional barriers were removed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_substrate_suppression_mechanism, empirical, 'Whether alternative compute substrates are suppressed or immature').

omega_variable(
    false_summit_natural_law,
    'Is the GPU supply bottleneck a genuine natural law (immutable physics of semiconductor manufacturing at nanoscale) or a false summit naturalizing extractive institutional arrangements (ecosystem lock-in, geographic concentration, allocation control)?',
    'Comparative analysis: identify which aspects of the bottleneck are attributable to physical limits (lithography, materials physics) versus institutional choices (fab location, CUDA standardization, allocation policies). Counterfactual scenario: if ecosystem lock-in were removed and manufacturing were geographically distributed, would the bottleneck persist?',
    'If mountain (natural law): the constraint is inevitable and policy responses should focus on adaptation and equitable access within scarcity. If false summit (extractive): the constraint is maintainable via institutional control, and policy responses should target decomposition (ecosystem diversification, geographic fab distribution, open-source alternatives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_law, conceptual, 'Whether GPU bottleneck is natural law or false summit').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpu_semiconductor_supply, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpu_supply_tr_t0, gpu_semiconductor_supply, theater_ratio, 0, 0.35).
narrative_ontology:measurement(gpu_supply_tr_t2, gpu_semiconductor_supply, theater_ratio, 2, 0.38).
narrative_ontology:measurement(gpu_supply_tr_t4, gpu_semiconductor_supply, theater_ratio, 4, 0.42).
narrative_ontology:measurement(gpu_supply_tr_t6, gpu_semiconductor_supply, theater_ratio, 6, 0.45).

% Extraction over time
narrative_ontology:measurement(gpu_supply_be_t0, gpu_semiconductor_supply, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(gpu_supply_be_t2, gpu_semiconductor_supply, base_extractiveness, 2, 0.45).
narrative_ontology:measurement(gpu_supply_be_t4, gpu_semiconductor_supply, base_extractiveness, 4, 0.58).
narrative_ontology:measurement(gpu_supply_be_t6, gpu_semiconductor_supply, base_extractiveness, 6, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(gpu_supply_su_t0, gpu_semiconductor_supply, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(gpu_supply_su_t2, gpu_semiconductor_supply, suppression_requirement, 2, 0.62).
narrative_ontology:measurement(gpu_supply_su_t4, gpu_semiconductor_supply, suppression_requirement, 4, 0.68).
narrative_ontology:measurement(gpu_supply_su_t6, gpu_semiconductor_supply, suppression_requirement, 6, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpu_semiconductor_supply, resource_allocation).
narrative_ontology:boltzmann_floor_override(gpu_semiconductor_supply, 0.18).
narrative_ontology:affects_constraint(gpu_semiconductor_supply, ai_research_accessibility).
narrative_ontology:affects_constraint(gpu_semiconductor_supply, computational_equity_global).
narrative_ontology:affects_constraint(gpu_semiconductor_supply, nvidia_market_dominance).

% DUAL FORMULATION NOTE:
% The GPU supply constraint decomposes into multiple structurally distinct constraints: (1) gpu_manufacturing_capacity — physical/engineering constraint on fab throughput and capital requirements; (2) gpu_ecosystem_lock_in — institutional constraint maintaining CUDA standardization despite alternatives; (3) gpu_geographic_concentration — geopolitical constraint on fab distribution and supply chain resilience; (4) gpu_allocation_prioritization — institutional constraint favoring established labs over emerging actors. Each has different ε and structural properties. This story models the aggregate constraint spanning all decomposed components. See affected_constraints for component stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gpu_semiconductor_supply, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
