% ============================================================================
% CONSTRAINT STORY: degrowth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_degrowth_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: degrowth_reading
 *   human_readable: Climate Response via Degrowth: Structural Economic Transformation in Global North
 *   domain: climate_policy/political_economy/intergenerational_justice
 *
 * SUMMARY:
 *   The degrowth reading of climate response instantiates one of three
 *   structurally distinct interpretations of the same underlying commitment:
 *   that global climate stabilization is an unavoidable imperative. This
 *   reading asserts that the Global North must undergo absolute reductions in
 *   material throughput, consumption, and working hours—both to enable its
 *   own mitigation (reduce emissions immediately) and to enable Global South
 *   development and adaptation (by reducing claims on remaining carbon
 *   budget). Unlike the mitigation_priority_reading (which assumes decoupling
 *   and technology can decouple growth from emissions) or the
 *   adaptation_priority_reading (which emphasizes resilience and local
 *   adaptive capacity over global emission reduction), the degrowth reading
 *   forecloses reliance on unproven carbon removal technologies and asserts
 *   that justice requires the Global North to shrink first. This creates a
 *   fundamental redistribution: present Global North populations enter the
 *   victim set (through reduced consumption and working-time mandates);
 *   future generations and Global South populations become clear
 *   beneficiaries. The constraint is a tangled rope at the institutional
 *   level (requires active enforcement, mixes coordination and extraction)
 *   but perceived as a snare by trapped workers whose only exit is closure of
 *   high-emission sectors. The measurement trajectory shows extractiveness
 *   rising over time (0.35→0.58) as the transformation deepens, while theater
 *   declines (0.62→0.44) as material transformations replace rhetorical
 *   commitments.
 *
 * KEY AGENTS:
 *   - Present Global North Workers: Primary victim (powerless/trapped) — face reduced consumption, job elimination in fossil sectors, working-time reduction with uncertain income support
 *   - Future Generations: Primary beneficiary (implicit/mobile) — benefit from climate stabilization and inherited low-carbon infrastructure; do NOT bear immediate extraction costs
 *   - Global South Populations: Primary beneficiary (moderate/constrained) — gain increased per-capita emission rights and development headroom; also gain climate stability reducing adaptation burden
 *   - Organized Climate Coalition: Secondary actor (organized/constrained) — scientists, youth, progressive unions; perceive mixed coordination (solving genuine collective action problem) and extraction (advocating policies harmful to current constituencies)
 *   - Renewable Energy Sector: Institutional beneficiary (institutional/arbitrage) — profits from massive capital redirection, technology mandates, guaranteed markets; experiences pure coordination
 *   - Global Financial Sector: Powerful actor (powerful/mobile) — experiences both stranded assets (fossil infrastructure) and new investment opportunities (green bonds, renewable infrastructure); high agency in negotiating transition
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional choice (how to distribute contraction) as thermodynamic law (that contraction must occur)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(degrowth_reading, 0.58).
domain_priors:suppression_score(degrowth_reading, 0.68).
domain_priors:theater_ratio(degrowth_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(degrowth_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(degrowth_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(degrowth_reading, theater_ratio, 0.44).

% --- Constraint claim ---
narrative_ontology:constraint_claim(degrowth_reading, tangled_rope).
narrative_ontology:human_readable(degrowth_reading, "Climate Response via Degrowth: Structural Economic Transformation in Global North").
narrative_ontology:topic_domain(degrowth_reading, "climate_policy/political_economy/intergenerational_justice").

domain_priors:requires_active_enforcement(degrowth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(degrowth_reading, formalized).
narrative_ontology:cs_authority_grounding(degrowth_reading, lineage).
narrative_ontology:cs_interpretation_layer_present(degrowth_reading).
narrative_ontology:cs_kernel_id(degrowth_reading, climate_response_imperative).
narrative_ontology:cs_reading_relation(degrowth_reading, mitigation_priority_reading, forecloses).
narrative_ontology:cs_reading_relation(degrowth_reading, adaptation_priority_reading, coexists_with).
narrative_ontology:cs_axiom(degrowth_reading, foundational, global_north_consumption_reduction_mandatory).
narrative_ontology:cs_axiom_status(global_north_consumption_reduction_mandatory, holdable).
narrative_ontology:cs_axiom_grounding(degrowth_reading, global_north_consumption_reduction_mandatory, empirically_contingent).
narrative_ontology:cs_axiom(degrowth_reading, foundational, carbon_removal_technology_unreliable).
narrative_ontology:cs_axiom_status(carbon_removal_technology_unreliable, holdable).
narrative_ontology:cs_axiom_grounding(degrowth_reading, carbon_removal_technology_unreliable, empirically_contingent).
narrative_ontology:cs_axiom(degrowth_reading, secondary, distributional_justice_primary).
narrative_ontology:cs_axiom_status(distributional_justice_primary, holdable).
narrative_ontology:cs_axiom_grounding(degrowth_reading, distributional_justice_primary, deontological).
narrative_ontology:cs_reference_frame(degrowth_reading, climate_stabilization_imperative).
narrative_ontology:cs_drift_state(degrowth_reading, contemporary_post_2020_consensus, gap(axiom_overriding, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(degrowth_reading, future_generations).
narrative_ontology:constraint_beneficiary(degrowth_reading, global_south_populations).
narrative_ontology:constraint_beneficiary(degrowth_reading, climate_stabilization_regime).
narrative_ontology:constraint_victim(degrowth_reading, present_global_north_populations).
narrative_ontology:constraint_victim(degrowth_reading, high_consumption_sectors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOCKED-IN GLOBAL NORTH WORKER (SNARE) — High-income Global North workers face extraction through forced reduction in material consumption, working hours, and employment stability. The constraint offers no exit: carbon budgets are immutable, and the reduction must occur within their lifetime. Employment in fossil-fuel and consumption-dependent sectors becomes a liability. They experience suppression through carbon rationing, job elimination in high-emission sectors, and reduced purchasing power with no alternative income sources. Extraction is maximal — costs concentrate on this group; benefits accrue to future generations and Global South. No meaningful agency.
constraint_indexing:constraint_classification(degrowth_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ORGANIZED CLIMATE COALITION (TANGLED ROPE) — Climate scientists, youth movements, and progressive unions perceive both coordination and extraction. The constraint solves a genuine coordination problem: no individual actor can unilaterally reduce emissions; collective action is necessary. But the coalition also bears extraction costs — they must advocate for policies that harm their own nominal constituencies (workers in fossil sectors, middle-class consumption gains). The constraint requires active enforcement through carbon pricing, carbon budgets, and sectoral phase-outs. They have agency (can organize, negotiate terms) but face high costs (political opposition, coalition fragility). Mixed experience.
constraint_indexing:constraint_classification(degrowth_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: RENEWABLE ENERGY & GREEN TECH SECTOR (ROPE) — Benefits from degrowth transition through massive capital investment, industrial policy support, and technology mandates. Experiences the constraint as pure coordination: mobilizing capital and labor for renewable deployment, grid transformation, and manufacturing transition. Extraction flows toward this sector in the form of government subsidies, guaranteed markets, and regulatory certainty. No material suppression — exit options abundant (can arbitrage between markets, benefit from technology licensing). Net beneficiary perceiving coordination.
constraint_indexing:constraint_classification(degrowth_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: GLOBAL FINANCIAL & CORPORATE SECTOR (TANGLED ROPE) — Large corporations and financial institutions see mixed effects. Degrowth requires dismantling global supply chains, reducing consumption markets, and eliminating stranded assets (fossil fuel reserves, high-emission infrastructure). They lose extractive opportunities but gain new ones (green bonds, carbon markets, renewable infrastructure investment). They experience suppression through asset write-downs and regulatory constraints, but have high mobility — capital can flow to new sectors. The constraint is simultaneously a loss (stranded assets) and an opportunity (green growth). Significant agency and means to negotiate transition terms.
constraint_indexing:constraint_classification(degrowth_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: GLOBAL SOUTH POPULATION (ROPE) — Primary beneficiary. Degrowth in Global North reduces competition for remaining carbon budget, enables increased per-capita emissions in Global South for development and adaptation. Coordination function: enables just allocation of atmospheric carbon. Experience is primarily coordination (fair distribution mechanism) with minimal extraction. Constrained exit options reflect dependency on Global North technology transfer and climate finance, but benefits accrue directly. Net beneficiary perceiving coordination mechanism.
constraint_indexing:constraint_classification(degrowth_reading, rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: THERMODYNAMIC REALITY / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/thermodynamic perspective, degrowth is an immutable consequence of physical constraints: finite planetary carbon budget, biophysical limits on resource extraction, entropy laws governing material throughput. High-income economies have exceeded steady-state equilibrium; contraction is not optional but inevitable. The question is only whether contraction is planned or chaotic. This perspective risks false-summit naturalization: treating what is a contingent institutional choice (how to distribute contraction costs) as a law of physics (that contraction must occur). The engine should flag this as FSM candidate.
constraint_indexing:constraint_classification(degrowth_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: JUST TRANSITION FRAMEWORK (SCAFFOLD) — Regulatory frameworks for sectoral transformation, worker retraining, and income support create a temporary bridge from high-carbon to post-carbon economy. The constraint is scaffolding: it solves immediate coordination (rapid sectoral transition) with built-in sunset (as low-carbon infrastructure matures, support structures wind down). Theater is moderate — actual retraining and job creation occur alongside performative 'green jobs' announcements. Active enforcement required during transition window (15–25 years), then scaffolding can dissolve. Organized actors (unions, state actors) see agency and negotiable terms; suppression is high but time-bounded.
constraint_indexing:constraint_classification(degrowth_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(degrowth_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(degrowth_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(degrowth_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(degrowth_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(degrowth_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, rising over interval. The constraint extracts from present Global North through consumption reduction, job elimination, working-time cuts, and wealth reduction. But extraction is not maximal (0.66+) because: (a) some redistribution to workers via universal basic income and public services offsets consumption loss; (b) global renewable sector gains are concentrated, not universally distributed; (c) some Global North population segments (those in low-emission sectors, those with high savings) experience lower extraction. The rising trajectory (0.35→0.58) reflects that as targets tighten and transformation deepens, extraction increases—initially concentrated in fossil sectors, then broadening to all consumption-dependent sectors. Suppression (0.68): High. Carbon budgets are hard physical constraints; sectoral phase-outs eliminate entire employment categories; consumption rationing leaves few alternatives. But suppression is not maximal (0.85+) because: (a) just-transition frameworks (retraining, income support, reduced work weeks) provide partial alternatives; (b) some workers can transition to green sectors; (c) global capital retains arbitrage options. Theater (0.44): Low-moderate. This reading emphasizes material transformation over performative climate commitment—actual renewable deployment, actual working-time reduction, actual consumption contraction. Theater declines over the interval (0.62→0.44) as initial policy announcements give way to hard infrastructure deployment. This low theater distinguishes degrowth from 'net zero' rhetoric that promises decoupling without delivery.
 *
 * PERSPECTIVAL GAP:
 *   This reading produces stark perspectival divergence. The trapped Global North worker sees pure snare (forced contraction, no exit, extraction concentrated on them). The organized climate coalition sees tangled rope (genuine coordination problem solved, but at cost to their nominal constituencies). The renewable sector sees pure rope (coordination mechanism, benefits flow clearly). The global financial sector sees mixed mobile experience (stranded assets vs new opportunities). The Global South sees rope (just allocation mechanism, benefits accrue). The analytical observer risks seeing mountain (thermodynamic inevitability) but the structural data reveals contingency: the same carbon budget could be distributed differently via the mitigation_priority_reading (decouple through technology) or adaptation_priority_reading (build resilience, accept higher warming). The perspectival gap IS the kernel: different parties read the same climate imperative commitment as requiring different structural transformations.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's structural position relative to this specific reading's extraction flow. Present Global North workers are victims (d≈0.92, high f(d)): they bear costs, have no arbitrage options, are trapped by carbon budgets and sectoral closure. Global South populations are beneficiaries (d≈0.15, low f(d)): they gain emission rights and climate stability, have constrained but positive exit options (development pathways, adaptation investments). Future generations are beneficiaries (d≈0.08, negative f(d)): they inherit the constraint's benefits (stable climate, low-carbon infrastructure) and have highest mobility (they were not locked into high-carbon infrastructure). The renewable sector is a mixed beneficiary (d≈0.30): they gain from capital redirection but face some suppression from rapid deployment requirements. The analytical observer (d≈0.72) experiences the constraint as presenting an interpretive crisis: is this reading the only one structurally viable, or do alternatives exist? That ambiguity is recorded in the omegas.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in climate response is: how can one commitment (climate stabilization) instantiate three incompatible readings? The resolution is that each reading reflects a different empirical premise that is testable: (1) Is decoupling sufficient? (2) Can carbon removal scale? (3) Is adaptive capacity a viable substitute for mitigation? If all three empirical questions were resolved YES, mitigation_priority_reading would be vindicated and degrowth would be unnecessary. If all three were NO, degrowth would be the only viable reading. The current empirical status is: (1) likely NO (decoupling insufficient), (2) uncertain (CDR may partially work), (3) likely NO (adaptation cannot substitute for mitigation). The degrowth reading is currently the most conservative (lowest-risk) interpretation of the commitment, but it is not mandatory until the empirical questions are resolved. The mandatrophy is resolved through a presheaf of three distinct constraint stories, each with its own extraction metrics, beneficiary/victim declarations, and measurements. No single story is 'the' climate response—the presheaf over all three readings is the actual structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    growth_decoupling_feasibility,
    'Can Global North economies decouple material consumption from GDP growth sufficiently to meet climate targets without absolute degrowth, or is decoupling insufficient and absolute reduction necessary?',
    'Historical carbon intensity trends; comparison of required emission reductions vs. achievable decoupling rates (relative vs. absolute); technology cost curves and deployment timelines for low-carbon alternatives across all sectors (transport, heating, food, materials, cement, steel)',
    'If decoupling sufficient: degrowth reading becomes unnecessary; mitigation_priority_reading becomes viable alternative. If decoupling insufficient: degrowth reading is structurally required; suppression and extraction values increase. If decoupling partially sufficient but insufficient for targets: degrowth is reduced-intensity alternative (milder contraction required).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(growth_decoupling_feasibility, empirical, 'Whether technological decoupling can replace degrowth').

omega_variable(
    carbon_removal_technology_viability,
    'Will direct air capture (DAC) and negative-emission technologies achieve sufficient scale and cost reduction to offset residual Global North emissions, eliminating need for absolute reduction?',
    'Technology cost trajectories; deployment timelines; land/energy requirements for gigatonne-scale CDR; empirical performance data on existing DAC facilities; comparison of required CDR capacity vs. physically and economically viable capacity',
    'If viable at scale: unproven-CDR justification for mitigation_priority_reading becomes viable; degrowth becomes optional. If not viable: unproven-technology reliance becomes major risk; degrowth becomes mandatory backup. If partially viable: hybrid approach (some degrowth + some CDR) becomes optimal.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(carbon_removal_technology_viability, empirical, 'Whether carbon removal can replace consumption reduction').

omega_variable(
    redistribution_political_feasibility,
    'Can post-growth institutions (reduced working hours, basic income, universal services) achieve sufficient political coalition to enforce degrowth transformation against extraction-seeking capital, or does degrowth require authoritarian enforcement?',
    'Comparative analysis of political coalitions supporting degrowth vs. pro-growth parties; historical precedent for redistributive wage-labor restructuring (e.g. Scandinavia post-1945, post-war reconstruction); survey data on willingness to accept reduced consumption for climate; modeling of coalition stability across different income groups',
    'If politically feasible: degrowth can rely on democratic enforcement + voluntary institutional change; suppression values may decrease, extraction becomes less coercive. If not feasible: degrowth requires top-down enforcement; suppression increases; constraint becomes more snare-like. If partially feasible in some regions: constraint becomes regionally stratified (different types in different political contexts).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(redistribution_political_feasibility, preference, 'Political viability of degrowth transformation').

omega_variable(
    intergenerational_extraction_asymmetry,
    'Is present-day Global North contraction genuinely costless to future generations (they benefit from climate stability), or do they also bear extraction costs (inheriting degraded infrastructure, reduced wealth, institutional instability)?',
    'Modeling of post-contraction economic output, infrastructure stock, and institutional capacity under different transition pathways; comparison of future welfare under (a) planned degrowth, (b) chaotic collapse, (c) continued growth + climate damage; discount-rate analysis for intergenerational welfare',
    'If genuinely costless to future: degrowth is pure intergenerational redistribution; future generations are clear beneficiaries. If also costly: degrowth is mutual extraction (present suffers more, future suffers less, but both suffer); classification shifts toward snare even for future generations. If future costs dominate: degrowth framing becomes contestable; alternative pathways gain legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_extraction_asymmetry, empirical, 'Whether future generations bear net extraction costs from degrowth').

omega_variable(
    knowledge_kernel_interpretation,
    'Is the climate response kernel grounded in natural-law thermodynamic constraints (physical limit on carbon budget) or political-economy constraints (institutional choices about distribution)?',
    'Decomposition of the constraint into physical and institutional components; identification of what changes if only one component is resolved (e.g., if distribution is fairer but carbon budget unchanged, does that shift the kernel?); analysis of which component''s failure would dissolve the constraint entirely',
    'If natural-law dominant: mountain classification becomes more defensible; degrowth is inevitable transformation. If political-economy dominant: constraint is contingent on institutional choices; alternative readings become equally viable; all three sibling readings (degrowth, mitigation-priority, adaptation-priority) remain structurally possible. If both equally weighted: constraint is hybrid kernel with multiple valid readings; no single reading forecloses others.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(knowledge_kernel_interpretation, conceptual, 'Whether the kernel grounds in natural law or institutional choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(degrowth_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(degr_tr_t0, degrowth_reading, theater_ratio, 0, 0.62).
narrative_ontology:measurement(degr_tr_t5, degrowth_reading, theater_ratio, 5, 0.48).
narrative_ontology:measurement(degr_tr_t10, degrowth_reading, theater_ratio, 10, 0.44).

% Extraction over time
narrative_ontology:measurement(degr_be_t0, degrowth_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(degr_be_t5, degrowth_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(degr_be_t10, degrowth_reading, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(degrowth_reading, resource_allocation).
narrative_ontology:affects_constraint(degrowth_reading, mitigation_priority_reading).
narrative_ontology:affects_constraint(degrowth_reading, adaptation_priority_reading).
narrative_ontology:affects_constraint(degrowth_reading, carbon_removal_technology_viability).
narrative_ontology:affects_constraint(degrowth_reading, global_emissions_budget_partition).

% DUAL FORMULATION NOTE:
% The degrowth reading is part of a three-story constraint family rooted in the climate_response_imperative kernel. Each reading asserts a different structural transformation required by the same commitment. The degrowth reading asserts: consumption reduction in Global North is necessary (not optional). The mitigation_priority_reading asserts: technology decoupling is sufficient (degrowth not necessary). The adaptation_priority_reading asserts: adaptive capacity is an alternative to aggressive mitigation. All three stories are linked via network.affects_constraints. The family structure encodes the empirical dependencies: if carbon removal technology becomes viable (carbon_removal_technology_viability resolves YES), mitigation_priority_reading gains structural support. If decoupling proves insufficient, degrowth reading becomes the mandatory reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
