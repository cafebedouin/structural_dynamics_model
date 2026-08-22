% ============================================================================
% CONSTRAINT STORY: performance_legitimacy__qualitative_development_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_performance_legitimacy__qualitative_development_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: performance_legitimacy__qualitative_development_reading
 *   human_readable: Performance Legitimacy via Qualitative Development (Innovation/Efficiency Focus)
 *   domain: political_economy/development_planning/state_capitalism
 *
 * SUMMARY:
 *   A state-capitalist regime grounds its performance legitimacy on
 *   structural transformation toward 'high-quality development' — innovation,
 *   efficiency gains, and technological self-sufficiency — rather than raw
 *   GDP growth. This reading redefines successful development as industrial
 *   upgrading, venture capital ecosystem maturation, and efficiency
 *   improvements. The constraint operates as a tangled rope: it genuinely
 *   coordinates capital toward higher-value sectors and away from
 *   low-productivity overcapacity, solving a real collective-action problem.
 *   Simultaneously, it extracts from traditional manufacturing,
 *   property-dependent local governments, and rural regions by redefining
 *   their sector-preservation as 'blocking progress.' The claim/metric
 *   independence principle applies: this reading is CLAIMED as a coordination
 *   mechanism (it solves industrial structure problems); the authored metrics
 *   (high extractiveness, high suppression, rising theater) describe the
 *   extraction that riding on that coordination. The divergence is what the
 *   engine measures. This constraint is ONE READING of the contested
 *   performance_legitimacy kernel — the qualitative_development_reading.
 *   Sibling readings (quantitative_growth_reading,
 *   techno_nationalist_reading, livelihood_security_reading) ground
 *   legitimacy on different criteria and would produce different
 *   beneficiary/victim sets and different constraint types.
 *
 * KEY AGENTS:
 *   - state_backed_innovation_ecosystem — sets the qualitative development metrics and allocates capital accordingly; agenda-setter power at institutional level
 *   - high_tech_sectors — direct beneficiaries, receive preferential capital and policy treatment; organized power, arbitrage-level exit
 *   - traditional_manufacturing — structural payer; capacity constrained by capital disinvestment and narrative delegitimization; moderate power, constrained exit
 *   - property_dependent_local_governments — payers by proxy; lose tax base and resource-allocation priority as development model shifts from real estate to innovation; moderate power, constrained exit
 *   - redundant_industrial_workers — powerless payers; face plant closures and identity-locked exit (relocation or retraining away from industrial work); resistance is high but suppression is higher
 *   - political_leadership — agenda-setter; maintains constraint enforcement by justifying displacement as unavoidable modernization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_legitimacy__qualitative_development_reading, 0.68).
domain_priors:suppression_score(performance_legitimacy__qualitative_development_reading, 0.71).
domain_priors:theater_ratio(performance_legitimacy__qualitative_development_reading, 0.47).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, theater_ratio, 0.47).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_legitimacy__qualitative_development_reading, tangled_rope).
narrative_ontology:human_readable(performance_legitimacy__qualitative_development_reading, "Performance Legitimacy via Qualitative Development (Innovation/Efficiency Focus)").
narrative_ontology:topic_domain(performance_legitimacy__qualitative_development_reading, "political_economy/development_planning/state_capitalism").

domain_priors:requires_active_enforcement(performance_legitimacy__qualitative_development_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_legitimacy__qualitative_development_reading, '41f9607b-be11-4013-8a82-053fa9f3eaa2').
narrative_ontology:cs_kernel_codification('41f9607b-be11-4013-8a82-053fa9f3eaa2', distributed).
narrative_ontology:cs_authority_grounding('41f9607b-be11-4013-8a82-053fa9f3eaa2', extraction).
narrative_ontology:cs_interpretation_layer_present('41f9607b-be11-4013-8a82-053fa9f3eaa2').
narrative_ontology:cs_reading_relation('41f9607b-be11-4013-8a82-053fa9f3eaa2', performance_legitimacy__quantitative_growth_reading, coexists_with).
narrative_ontology:cs_reading_relation('41f9607b-be11-4013-8a82-053fa9f3eaa2', performance_legitimacy__techno_nationalist_reading, influences).
narrative_ontology:cs_reading_relation('41f9607b-be11-4013-8a82-053fa9f3eaa2', performance_legitimacy__livelihood_security_reading, forecloses).
narrative_ontology:cs_axiom('41f9607b-be11-4013-8a82-053fa9f3eaa2', foundational, innovation_capacity_supersedes_growth_quantity).
narrative_ontology:cs_axiom_status(innovation_capacity_supersedes_growth_quantity, holdable).
narrative_ontology:cs_axiom_grounding('41f9607b-be11-4013-8a82-053fa9f3eaa2', innovation_capacity_supersedes_growth_quantity, empirically_contingent).
narrative_ontology:cs_axiom('41f9607b-be11-4013-8a82-053fa9f3eaa2', foundational, structural_transformation_necessary_for_legitimacy).
narrative_ontology:cs_axiom_status(structural_transformation_necessary_for_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('41f9607b-be11-4013-8a82-053fa9f3eaa2', structural_transformation_necessary_for_legitimacy, conventional).
narrative_ontology:cs_reference_frame('41f9607b-be11-4013-8a82-053fa9f3eaa2', post_2008_development_model_shift).
narrative_ontology:cs_drift_state('41f9607b-be11-4013-8a82-053fa9f3eaa2', contemporary_tech_dominance_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('41f9607b-be11-4013-8a82-053fa9f3eaa2', '').
narrative_ontology:cs_kernel_id(performance_legitimacy__qualitative_development_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_legitimacy__qualitative_development_reading, high_tech_sectors).
narrative_ontology:constraint_beneficiary(performance_legitimacy__qualitative_development_reading, state_backed_innovation_ecosystem).
narrative_ontology:constraint_beneficiary(performance_legitimacy__qualitative_development_reading, venture_capital_infrastructure).
narrative_ontology:constraint_beneficiary(performance_legitimacy__qualitative_development_reading, urban_tech_hubs).
narrative_ontology:constraint_victim(performance_legitimacy__qualitative_development_reading, traditional_manufacturing).
narrative_ontology:constraint_victim(performance_legitimacy__qualitative_development_reading, property_dependent_local_governments).
narrative_ontology:constraint_victim(performance_legitimacy__qualitative_development_reading, rural_agricultural_regions).
narrative_ontology:constraint_victim(performance_legitimacy__qualitative_development_reading, redundant_industrial_workers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive directed capital, preferential regulatory treatment, tax incentives, and intellectual property protection. Their growth metrics (patent filings, startup valuations, R&D spending) become the primary performance indicators of the entire development model. They collect rents from state-directed venture capital and preferential access to talent through educational reorientation.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, high_tech_sectors, beneficiary,
    institutional, generational, arbitrage, national).

% Sets the innovation metrics, defines 'high-quality development,' allocates research funding, and administers the licensing and venture infrastructure. Justifies the constraint through efficiency gains, sustainability targets, and global competitiveness. Benefits from the institutional authority this framing grants and from control over which sectors receive innovation support.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, state_backed_innovation_ecosystem, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(performance_legitimacy__qualitative_development_reading, state_backed_innovation_ecosystem, beneficiary).

% Faces disinvestment, reduced access to development capital, and pressure to 'upgrade' or consolidate under the efficiency logic. Large workforce faces retraining mandates and relocation; plant closures are framed as necessary structural transformation. Exit means either accepting contract manufacturing at lower margins or migrating operations to other regions without state support.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, traditional_manufacturing, payer,
    moderate, biographical, constrained, regional).

% Lose tax revenue as property values stabilize (innovation focus deprioritizes real estate as development driver) and as manufacturing tax bases shrink. Must shift to service provision with reduced fiscal capacity while cities with tech clusters accumulate resources and political influence. Their traditional fiscal models become structurally misaligned with the new development priorities.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, property_dependent_local_governments, payer,
    moderate, biographical, constrained, regional).

% Are excluded from innovation investment and positioned as resource bases for urban tech clusters rather than as development beneficiaries. Rural development policy is reoriented toward serving tech hub supply chains. Population flight accelerates as economic opportunity concentrates in urban innovation centers; alternative exits require abandoning region.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, rural_agricultural_regions, payer,
    powerless, biographical, trapped, regional).

% Face plant closures and retraining mandates under the structural transformation logic. Their professional identity, community stability, and political power are tied to industrial employment. Exit means leaving region, retraining into low-wage service work, or early retirement. The constraint frames their job loss as unavoidable modernization, not as extraction of their labor-market rents.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, redundant_industrial_workers, payer,
    powerless, biographical, identity_locked, local).

% Receives institutional co-investment, regulatory favor, and intellectual property protection. Profit extraction through equity stakes in state-backed startups is legitimated as investment in national innovation. Institutional venture arms collect returns while bearing reduced downside risk through state backing and preferential deal flow.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, venture_capital_infrastructure, beneficiary,
    institutional, generational, arbitrage, national).

% Attract concentrated investment in education, infrastructure, and amenities. Become sites of talent concentration and institutional innovation capacity. Their economic success becomes the primary performance metric for the entire development model; their sustainability depends on continuous capital reallocation from lagging regions and sectors.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, urban_tech_hubs, beneficiary,
    powerful, generational, arbitrage, national).

% Frames performance legitimacy around innovation and efficiency metrics rather than raw growth, positioning the regime as forward-looking and pragmatic. Must manage the structural displacement this framing creates in traditional sectors while maintaining sufficient political support from displaced constituencies. Uses efficiency narrative to justify sectoral reallocation.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, political_leadership, agenda_setter,
    institutional, biographical, analytical, national).

% Endorses the innovation-led development frame through lending conditionalities, policy guidance, and technical assistance. Ties development finance to efficiency metrics, sustainability targets, and digital infrastructure spending. Their legitimation of the qualitative development reading shapes capital flows and policy priorities globally.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, international_development_institutions, observer,
    institutional, generational, analytical, global).

% Would demand maintenance of industrial employment, retention of property-tax-dependent fiscal models, and direct livelihood security investments. Their exclusion from the performance-legitimacy conversation is maintained through narratives of inevitability and modernization necessity. They lack representation in the innovation-metric-setting process.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, displaced_workers_coalitions, excluded,
    powerless, biographical, trapped, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(performance_legitimacy__qualitative_development_reading, state_backed_innovation_ecosystem).
narrative_ontology:fixing_cost_class(performance_legitimacy__qualitative_development_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Directs capital and policy toward structural economic transformation: reallocating resources from lower-productivity traditional sectors toward innovation, efficiency improvements, and technological capacity-building. Solves the collective-action problem of how to coordinate an entire economy toward higher-value production and away from capacity-building that generates political resistance in declining sectors.
% TRANSFER_FUNCTION: Moves capital investment (venture funding, research grants, infrastructure spending), educational resources (STEM emphasis, university-industry partnerships), and political priority from traditional manufacturing regions and rural areas toward high-tech sectors and urban innovation hubs. Extracts fiscal capacity from property-dependent local governments (reduced growth → reduced property tax base) and employment security from industrial workers through plant closures framed as necessary structural transformation.
% ABSENT_VOICES: Displaced workers, traditional manufacturers, property-dependent local governments, and rural regions would argue for diversified development strategies that maintain employment in existing sectors while enabling innovation. They are structurally excluded by the framing that defines their sector-preservation as 'blocking progress.' International development NGOs and labor economists focused on livelihood security are also absent from the performance-legitimacy conversation.
% DISAPPEARANCE_RATIONALE: If the qualitative development constraint (and its enforcement through capital allocation, educational policy, and regulatory priority) vanished overnight, capital would redistribute toward sectors with immediate employment and livelihood returns; property-dependent fiscal models would recover as real estate became viable development driver again; industrial policy would target sector preservation alongside innovation. The geographic and sectoral distribution of development gains would shift significantly; urban tech clusters would lose their structural advantage in resource competition.
% FOUNDING_PROBLEM: Post-2008 recognition that raw growth rates alone do not ensure regime stability or international competitiveness; growth that fails to produce innovation capacity, efficiency gains, or sustainability leaves the economy vulnerable to technological disruption and the regime vulnerable to legitimacy challenges from environmental pressure and inability to compete in advanced sectors.
% FOUNDING_PROBLEM_CORROBORATION: International development institutions and technology-sector economists attest the founding problem is live and the qualitative development frame is necessary for long-term stability and competitiveness. Traditional-sector advocates and labor economists attest the problem has been reframed to serve sectoral interests; they argue raw growth plus targeted livelihood investment would be more legitimacy-stabilizing than efficiency-driven displacement. Development NGOs and labor-rights organizations attest that manufacturing employment loss contradicts livelihood improvements. No independent source validates that efficiency gains outweigh employment loss as a legitimacy foundation or that innovation concentration outperforms diversified development.
narrative_ontology:disappearance_verdict(performance_legitimacy__qualitative_development_reading, world_rearranges).
narrative_ontology:founding_problem_status(performance_legitimacy__qualitative_development_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(performance_legitimacy__qualitative_development_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(performance_legitimacy__qualitative_development_reading, 'none', 1).
narrative_ontology:epsilon_provenance(performance_legitimacy__qualitative_development_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(performance_legitimacy__qualitative_development_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(performance_legitimacy__qualitative_development_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(performance_legitimacy__qualitative_development_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the constraint moves substantial resources from declining sectors to rising ones, and justifies this reallocation through a technical narrative (efficiency, innovation) that obscures the distributional consequences. Suppression (0.71) is correspondingly high because maintaining this constraint requires active enforcement of capital allocation priorities (venture investment preference, educational reorientation, regulatory favor for tech sectors) and narrative suppression (framing industrial decline as necessary progress rather than extraction). Theater ratio (0.47) reflects a genuine coordination function (industrial upgrading is a real problem) layered with increasing performative legitimation — as resistance from affected sectors grows, more enforcement effort goes to justifying the constraint (efficiency metrics, innovation narratives, sustainability framing) and less to solving the underlying structural problem. The coercion grid shows individual-level stakes inflation rising sharply (0.52→0.72), reflecting how plant closures and retraining mandates concentrate pressure on specific workers; organizational-level suppression also rises (0.60→0.73) as traditional manufacturers face regulatory and capital-access restrictions. Class-level resistance remains high (0.80→0.79) because displaced workers maintain collective mobilization despite suppression. Structural-level suppression is lower (0.48→0.62) because the state apparatus need not suppress system-level alternatives — the qualitative development frame is institutionalized in development institutions globally.
 *
 * PERSPECTIVAL GAP:
 *   The state_backed_innovation_ecosystem and political_leadership seats should compute as seeing genuine coordination (collective action problem solved); traditional_manufacturing and redundant_industrial_workers seats should compute as extractive (payment without compensation or exit). The engine derives this from: (1) beneficiary/victim declarations (innovation ecosystem and workers sit at opposite poles); (2) exit_options (innovation sectors have arbitrage-level exit; workers are identity_locked); (3) power asymmetry (institutional vs. powerless). The same constraint appears as coordination-with-side-effects from the agenda-setter seat and as enforced extraction from the payer seats. The authored metrics (high extraction, high suppression) describe the payer-seat experience; the beneficiary seats would author lower extractiveness and justify the suppression as correcting market failures.
 *
 * DIRECTIONALITY LOGIC:
 *   High-tech sectors and the state innovation ecosystem have directionality near 0.0 (full beneficiary): they collect rents from preferential capital, regulatory favor, and talent concentration without bearing suppression costs. Traditional manufacturing has directionality near 0.9 (near full target): they pay through capital withdrawal and narrative delegitimization while having constrained exit. Redundant industrial workers sit even higher in target position (d approaching 1.0) because identity_locked exit means they cannot arbitrage — plant closure forces relocation, retraining, or economic decline, all high-cost exits. Venture capital infrastructure (d ~ 0.1) collects returns while bearing minimal downside risk through state backing. The coercion grid directionally splits individual (0.69 suppression, 0.71 resistance at endpoint) vs. structural level (0.62 suppression, 0.78 resistance at endpoint): individual workers absorb more suppression relative to their resistance capacity because they lack organizational scale, while class-level resistance (0.79) approaches suppression capacity (0.76), indicating the constraint approaches a stability threshold.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (raw growth no longer stabilizes regime legitimacy; efficiency and innovation capacity matter) is LIVE and structural — it genuinely shifted in the post-2008 environment. However, the qualitative development constraint exhibits mandatrophy drift along a different dimension: it began as a legitimate response to overcapacity in traditional sectors and the need for structural upgrading, but has transformed into a rent-seeking framework protecting high-tech capital and urban hubs from competition. The theater ratio rising from 0.32 to 0.47 reflects exactly this drift — enforcement effort increasingly goes to narrative legitimation (innovation metrics, sustainability framing) rather than solving the underlying structural problem. If the original mandate was 'coordinate transition away from unsustainable overcapacity,' the mandate has shifted to 'defend high-tech sectoral preference.' The constraint should be classified as tangled_rope (genuine coordination function + asymmetric extraction + active enforcement) not snare, because the coordination problem is real — but the theater ratio rising toward 0.5 signals incipient mandatrophy if the innovation metrics become mere cover story rather than real outcome measures.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_rent_extraction_boundary,
    'Is the measured extractiveness (0.68) the price of genuine industrial upgrading coordination, or has the constraint become primarily a mechanism for protecting high-tech capital from market discipline?',
    'Counterfactual analysis: compare innovation and efficiency outcomes under this constraint vs. outcomes under alternative development models (e.g., diversified sector support) in other jurisdictions; track whether efficiency gains accrue to the broader economy or concentrate in high-tech sectors.',
    'If extractiveness is mostly coordination cost, the constraint is more legitimately tangled_rope; if extractiveness exceeds coordination cost, it is a snare disguised as coordination. This resolves whether theater_ratio rise (0.32→0.47) reflects genuine shift in mandate or performative masking of extractive intent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_rent_extraction_boundary, empirical, 'Whether measured extraction reflects coordination cost or captured rent.').

omega_variable(
    displaced_worker_exit_lock_mechanism,
    'Is the identity_locked exit assigned to redundant_industrial_workers structural (relocation/retraining economically unfeasible) or internalized (workers have internalized industrial identity as unchangeable self-concept)?',
    'Post-policy-change analysis: if regions with government-funded retraining and relocation support show workers accepting exit, identity lock is partly structural/economic and partly internalized; if workers reject supported exits, lock is primarily internalized.',
    'If internalized, suppression measurement (0.71) understates the actual control exerted — the worker carries suppression with them after exit. The constraint''s effective extraction is higher than direct capital measures suggest because it requires cognitive/identity restructuring, not just economic transfer.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(displaced_worker_exit_lock_mechanism, empirical, 'Internalized vs. structural basis of identity-locked exit in displaced workers.').

omega_variable(
    kernel_reading_foreclosure,
    'Does this reading (qualitative_development) logically foreclose the livelihood_security_reading, or do they merely occupy different political coalitions without logical contradiction?',
    'Axiomatic examination: if both readings can be held true within a single coherent framework (e.g., ''we upgrade sectors AND guarantee income floors for displaced workers''), no foreclosure; if holding both creates internal contradiction in claims about resource allocation or metrics, foreclosure is present.',
    'If foreclosure is genuine (rare), the engine reclassifies the relationship from coexists_with to forecloses. If no foreclosure, the readings remain in active contestation across different political factions. This determines whether the kernel permits multi-reading equilibria or forces eventual single-reading dominance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether qualitative_development reading logically rules out livelihood_security reading in a single framework.').

omega_variable(
    performance_metric_substitution_risk,
    'Are the innovation and efficiency metrics (patents, startup valuations, R&D spending) genuine measures of development quality, or have they become Goodhart-proxies that incentivize metric gaming rather than real structural improvement?',
    'Metric correlation analysis: track whether sectors with rising innovation metrics show corresponding productivity gains, cost reductions, or sustainability improvements; detect if innovation investment correlates with technology adoption in non-tech sectors or remains concentrated in tech-sector internal measures.',
    'If metrics are genuine, the theater_ratio plateau (0.45→0.47) reflects stable performance measurement; if metrics are Goodhart-corrupted, theater_ratio understates performative activity because the metrics themselves are theater. The constraint approaches piton status if metric-gaming exceeds real structural change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performance_metric_substitution_risk, empirical, 'Innovation and efficiency metrics as genuine performance measures vs. Goodhart proxies.').

omega_variable(
    reading_framingness_choice,
    'This reading interprets performance_legitimacy through innovation-and-efficiency framing. Is this framing the only structurally defensible one, or do the quantitative_growth and livelihood_security readings instantiate equally valid structural claims competing for institutional adoption?',
    'Comparative constraint-story analysis: if quantitative_growth and livelihood_security readings author structurally identical beneficiary/victim sets under different metrics, the readings are observational variants of one constraint; if they author genuinely different beneficiary/victim structures, they are distinct constraints under the same kernel label, and framing choice is path-dependent, not logically determined.',
    'If readings are observational variants, the choice of this reading (qualitative_development) is an author framing choice with no objective referent; if readings are structurally distinct, each is a valid constraint-story and the kernel houses multiple genuine constraint structures. This affects whether the kernel is under-determined or multiply realized.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_framingness_choice, conceptual, 'Whether framing choice determines or discovers the constraint''s structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_legitimacy__qualitative_development_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_tr_t0, performance_legitimacy__qualitative_development_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement_basis(perf_tr_t0, observed).
narrative_ontology:measurement(perf_tr_t5, performance_legitimacy__qualitative_development_reading, theater_ratio, 5, 0.38).
narrative_ontology:measurement_basis(perf_tr_t5, observed).
narrative_ontology:measurement(perf_tr_t10, performance_legitimacy__qualitative_development_reading, theater_ratio, 10, 0.42).
narrative_ontology:measurement_basis(perf_tr_t10, observed).
narrative_ontology:measurement(perf_tr_t15, performance_legitimacy__qualitative_development_reading, theater_ratio, 15, 0.45).
narrative_ontology:measurement_basis(perf_tr_t15, observed).
narrative_ontology:measurement(perf_tr_t20, performance_legitimacy__qualitative_development_reading, theater_ratio, 20, 0.46).
narrative_ontology:measurement_basis(perf_tr_t20, observed).
narrative_ontology:measurement(perf_tr_t25, performance_legitimacy__qualitative_development_reading, theater_ratio, 25, 0.47).
narrative_ontology:measurement_basis(perf_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(perf_be_t0, performance_legitimacy__qualitative_development_reading, base_extractiveness, 0, 0.51).
narrative_ontology:measurement_basis(perf_be_t0, observed).
narrative_ontology:measurement(perf_be_t5, performance_legitimacy__qualitative_development_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement_basis(perf_be_t5, observed).
narrative_ontology:measurement(perf_be_t10, performance_legitimacy__qualitative_development_reading, base_extractiveness, 10, 0.63).
narrative_ontology:measurement_basis(perf_be_t10, observed).
narrative_ontology:measurement(perf_be_t15, performance_legitimacy__qualitative_development_reading, base_extractiveness, 15, 0.66).
narrative_ontology:measurement_basis(perf_be_t15, observed).
narrative_ontology:measurement(perf_be_t20, performance_legitimacy__qualitative_development_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement_basis(perf_be_t20, observed).
narrative_ontology:measurement(perf_be_t25, performance_legitimacy__qualitative_development_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(perf_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(perf_su_t0, performance_legitimacy__qualitative_development_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(perf_su_t0, observed).
narrative_ontology:measurement(perf_su_t5, performance_legitimacy__qualitative_development_reading, suppression_requirement, 5, 0.63).
narrative_ontology:measurement_basis(perf_su_t5, observed).
narrative_ontology:measurement(perf_su_t10, performance_legitimacy__qualitative_development_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement_basis(perf_su_t10, observed).
narrative_ontology:measurement(perf_su_t15, performance_legitimacy__qualitative_development_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement_basis(perf_su_t15, observed).
narrative_ontology:measurement(perf_su_t20, performance_legitimacy__qualitative_development_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(perf_su_t20, observed).
narrative_ontology:measurement(perf_su_t25, performance_legitimacy__qualitative_development_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(perf_su_t25, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=25
narrative_ontology:measurement(perf_grid_01, performance_legitimacy__qualitative_development_reading, accessibility_collapse(class), 0, 0.7).
narrative_ontology:measurement(perf_grid_02, performance_legitimacy__qualitative_development_reading, accessibility_collapse(class), 25, 0.75).
narrative_ontology:measurement(perf_grid_03, performance_legitimacy__qualitative_development_reading, accessibility_collapse(individual), 0, 0.48).
narrative_ontology:measurement(perf_grid_04, performance_legitimacy__qualitative_development_reading, accessibility_collapse(individual), 25, 0.58).
narrative_ontology:measurement(perf_grid_05, performance_legitimacy__qualitative_development_reading, accessibility_collapse(organizational), 0, 0.62).
narrative_ontology:measurement(perf_grid_06, performance_legitimacy__qualitative_development_reading, accessibility_collapse(organizational), 25, 0.68).
narrative_ontology:measurement(perf_grid_07, performance_legitimacy__qualitative_development_reading, accessibility_collapse(structural), 0, 0.55).
narrative_ontology:measurement(perf_grid_08, performance_legitimacy__qualitative_development_reading, accessibility_collapse(structural), 25, 0.62).
narrative_ontology:measurement(perf_grid_09, performance_legitimacy__qualitative_development_reading, resistance(class), 0, 0.8).
narrative_ontology:measurement(perf_grid_10, performance_legitimacy__qualitative_development_reading, resistance(class), 25, 0.79).
narrative_ontology:measurement(perf_grid_11, performance_legitimacy__qualitative_development_reading, resistance(individual), 0, 0.68).
narrative_ontology:measurement(perf_grid_12, performance_legitimacy__qualitative_development_reading, resistance(individual), 25, 0.71).
narrative_ontology:measurement(perf_grid_13, performance_legitimacy__qualitative_development_reading, resistance(organizational), 0, 0.62).
narrative_ontology:measurement(perf_grid_14, performance_legitimacy__qualitative_development_reading, resistance(organizational), 25, 0.64).
narrative_ontology:measurement(perf_grid_15, performance_legitimacy__qualitative_development_reading, resistance(structural), 0, 0.72).
narrative_ontology:measurement(perf_grid_16, performance_legitimacy__qualitative_development_reading, resistance(structural), 25, 0.78).
narrative_ontology:measurement(perf_grid_17, performance_legitimacy__qualitative_development_reading, stakes_inflation(class), 0, 0.62).
narrative_ontology:measurement(perf_grid_18, performance_legitimacy__qualitative_development_reading, stakes_inflation(class), 25, 0.74).
narrative_ontology:measurement(perf_grid_19, performance_legitimacy__qualitative_development_reading, stakes_inflation(individual), 0, 0.52).
narrative_ontology:measurement(perf_grid_20, performance_legitimacy__qualitative_development_reading, stakes_inflation(individual), 25, 0.72).
narrative_ontology:measurement(perf_grid_21, performance_legitimacy__qualitative_development_reading, stakes_inflation(organizational), 0, 0.58).
narrative_ontology:measurement(perf_grid_22, performance_legitimacy__qualitative_development_reading, stakes_inflation(organizational), 25, 0.68).
narrative_ontology:measurement(perf_grid_23, performance_legitimacy__qualitative_development_reading, stakes_inflation(structural), 0, 0.51).
narrative_ontology:measurement(perf_grid_24, performance_legitimacy__qualitative_development_reading, stakes_inflation(structural), 25, 0.64).
narrative_ontology:measurement(perf_grid_25, performance_legitimacy__qualitative_development_reading, suppression(class), 0, 0.65).
narrative_ontology:measurement(perf_grid_26, performance_legitimacy__qualitative_development_reading, suppression(class), 25, 0.76).
narrative_ontology:measurement(perf_grid_27, performance_legitimacy__qualitative_development_reading, suppression(individual), 0, 0.55).
narrative_ontology:measurement(perf_grid_28, performance_legitimacy__qualitative_development_reading, suppression(individual), 25, 0.69).
narrative_ontology:measurement(perf_grid_29, performance_legitimacy__qualitative_development_reading, suppression(organizational), 0, 0.6).
narrative_ontology:measurement(perf_grid_30, performance_legitimacy__qualitative_development_reading, suppression(organizational), 25, 0.73).
narrative_ontology:measurement(perf_grid_31, performance_legitimacy__qualitative_development_reading, suppression(structural), 0, 0.48).
narrative_ontology:measurement(perf_grid_32, performance_legitimacy__qualitative_development_reading, suppression(structural), 25, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_legitimacy__qualitative_development_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(performance_legitimacy__qualitative_development_reading, 0.18).
narrative_ontology:affects_constraint(performance_legitimacy__qualitative_development_reading, performance_legitimacy__quantitative_growth_reading).
narrative_ontology:affects_constraint(performance_legitimacy__qualitative_development_reading, performance_legitimacy__techno_nationalist_reading).
narrative_ontology:affects_constraint(performance_legitimacy__qualitative_development_reading, performance_legitimacy__livelihood_security_reading).

% DUAL FORMULATION NOTE:
% This is one reading of the contested performance_legitimacy kernel. All four readings (qualitative_development_reading, quantitative_growth_reading, techno_nationalist_reading, livelihood_security_reading) ground performance legitimacy on different criteria and would produce different constraint types and extraction profiles. They coexist or foreclose in different configurations depending on institutional context. No single reading is objectively true; each is a valid constraint-story under a different committer frame. See cs_structure.reading_relations and cs_structure.axioms for the structural relationships between readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(performance_legitimacy__qualitative_development_reading, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
