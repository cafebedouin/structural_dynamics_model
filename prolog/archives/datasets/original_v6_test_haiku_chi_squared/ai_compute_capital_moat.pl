% ============================================================================
% CONSTRAINT STORY: ai_compute_capital_moat
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_compute_capital_moat, []).

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
 *   constraint_id: ai_compute_capital_moat
 *   human_readable: The AI Compute & Capital Moat
 *   domain: technological/economic
 *
 * SUMMARY:
 *   The AI Compute & Capital Moat represents a structural asymmetry in
 *   frontier artificial intelligence development that functions
 *   simultaneously as coordination mechanism (enabling long R&D runways and
 *   capital accumulation for ambitious research) and as extraction mechanism
 *   (excluding all but a handful of institutions and sovereign entities from
 *   frontier AI research). The constraint emerges from two nonreducible
 *   bottlenecks: (1) elite AI research talent is concentrated geographically
 *   and institutionally, creating a winner-take-most dynamic; (2) training
 *   frontier models requires GPU clusters at scales ($100M-$1B+) that only
 *   capital-rich institutions can access. This constraint exhibits the full
 *   spectrum of DR classifications depending on observer position. The
 *   incumbent lab experiences the moat as Rope (pure coordination enabling
 *   breakthrough research). The emerging research group experiences it as
 *   Snare (complete exclusion from the field). The open-source coalition
 *   experiences it as Tangled Rope (mixed coordination and extraction). The
 *   well-funded startup experiences it as Snare (constrained to dependency).
 *   The analytical observer risks naturalizing the moat as a Mountain
 *   (immutable law of AI development), but the structural data reveals this
 *   as a false summit: the moat is actively enforced through capital
 *   allocation decisions, compute access restrictions, and talent recruitment
 *   strategies. The theater ratio (0.35) reflects that while the genuine
 *   compute requirements are substantial, significant portions of the moat
 *   narrative are performative: claims that 'scale is necessary for safety'
 *   or 'frontier models require maximum compute' sometimes conflate
 *   architectural necessity with strategic choice.
 *
 * KEY AGENTS:
 *   - Incumbent AI Lab (OpenAI, DeepSeek, Anthropic, Google): Primary beneficiary (institutional/arbitrage) — captures long R&D runway, attracts elite talent, controls frontier model access and derivatives
 *   - Capital Providers (VCs, strategic investors, governments): Primary beneficiary (institutional/arbitrage) — protect return on investment, justify continued capital flows, access frontier model capabilities
 *   - Elite AI Researchers: Mixed position (powerful/mobile) — concentrated in incumbent labs but paradoxically constrained by them; mobile between labs but cannot access frontier compute outside incumbent ecosystem
 *   - Emerging Research Groups: Primary victim (powerless/trapped) — completely excluded from frontier AI development by capital and GPU access barriers
 *   - Well-Funded Startups: Secondary victim (moderate/constrained) — can access capital and some talent but face downstream dependency on incumbents
 *   - Open-Source AI Coalition (Meta, academic institutions, volunteer contributors): Organized agent (organized/constrained) — developing alternative pathways but constrained by resource asymmetry
 *   - Developing Nation Research Sector: Victim (moderate/constrained) — structurally excluded from frontier research by capital and GPU access barriers
 *   - GPU Supply Chain (NVIDIA, foundries, geopolitical actors): Structural controller (institutional/arbitrage) — gate-keeps access to compute through manufacturing concentration and export controls
 *   - Epistemic Diversity in AI: Abstract victim (powerless/trapped) — moat reduces diversity of research directions, architectural approaches, and safety perspectives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_compute_capital_moat, 0.58).
domain_priors:suppression_score(ai_compute_capital_moat, 0.68).
domain_priors:theater_ratio(ai_compute_capital_moat, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_compute_capital_moat, extractiveness, 0.58).
narrative_ontology:constraint_metric(ai_compute_capital_moat, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(ai_compute_capital_moat, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_compute_capital_moat, tangled_rope).
narrative_ontology:human_readable(ai_compute_capital_moat, "The AI Compute & Capital Moat").
narrative_ontology:topic_domain(ai_compute_capital_moat, "technological/economic").

domain_priors:requires_active_enforcement(ai_compute_capital_moat).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_compute_capital_moat, capital_holders).
narrative_ontology:constraint_beneficiary(ai_compute_capital_moat, incumbent_labs).
narrative_ontology:constraint_victim(ai_compute_capital_moat, emerging_research_groups).
narrative_ontology:constraint_victim(ai_compute_capital_moat, economically_constrained_developers).
narrative_ontology:constraint_victim(ai_compute_capital_moat, epistemic_diversity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EMERGING RESEARCH GROUP (SNARE) — Cannot afford GPUs or compete for elite talent without pre-existing capital. Blocked from frontier research entirely. d≈0.94, f(d)≈1.40, σ=1.2 → χ≈0.97.
constraint_indexing:constraint_classification(ai_compute_capital_moat, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: WELL-FUNDED STARTUP (SNARE) — Can access capital and some talent, but must accept downstream dependency on incumbents for model access, compute time, or acquisition. Constrained exit: acquisition or subservience. d≈0.82, f(d)≈1.25, σ=1.0 → χ≈0.73.
constraint_indexing:constraint_classification(ai_compute_capital_moat, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: OPEN-SOURCE AI COALITION (TANGLED ROPE) — Organized agent (Meta, academia, collective contributors) sees a coordination function: distributed compute, volunteer labor, open weights models reduce moat barriers. But also constrained by resource asymmetry and competition dynamics. d≈0.48, f(d)≈0.60, σ=1.2 → χ≈0.42.
constraint_indexing:constraint_classification(ai_compute_capital_moat, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: INCUMBENT AI LAB (ROPE) — Primary beneficiary (OpenAI, DeepSeek, Anthropic leadership at funding time). Experiences constraint as coordination of elite talent and capital deployment. Moat enables long R&D runways and attracts top researchers. d≈0.08, f(d)≈-0.09, σ=1.2 → χ≈-0.06. Net beneficiary.
constraint_indexing:constraint_classification(ai_compute_capital_moat, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CAPITAL PROVIDER (ROPE) — Venture, strategic, and government capital sees coordination function: capital concentration enables risk-taking on frontier research with long timelines. Moat protects return on investment and justifies continued capital flows. d≈0.10, f(d)≈-0.06, σ=1.2 → χ≈-0.04. Net beneficiary.
constraint_indexing:constraint_classification(ai_compute_capital_moat, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ELITE AI RESEARCHER (TANGLED ROPE) — Paradoxical position: moat concentrates resources they need for ambitious work, but also constrains their negotiating power and career mobility. Can move between labs but cannot access frontier compute outside incumbent ecosystem. d≈0.55, f(d)≈0.75, σ=1.2 → χ≈0.52.
constraint_indexing:constraint_classification(ai_compute_capital_moat, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: DEVELOPING NATION RESEARCH SECTOR (SNARE) — Structurally excluded from frontier AI development by capital and GPU access barriers. Cannot compete for global talent without funding. Constrained to downstream application work or dependent partnerships. d≈0.88, f(d)≈1.32, σ=1.2 → χ≈0.91.
constraint_indexing:constraint_classification(ai_compute_capital_moat, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: INSTITUTIONAL NARRATIVE (PITON) — The dominant framing 'frontier AI requires massive compute' is partly functional (true) and partly performative theater masking extraction. Theater ratio = 0.35 reflects that efficiency gains, algorithmic improvements, and inference-time scaling could reduce training compute requirements, but the narrative naturalizes capital concentration as inevitable. Institutional inertia maintains the moat as 'inherent to the field.'
constraint_indexing:constraint_classification(ai_compute_capital_moat, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational view, the moat might appear as a natural consequence of physics and economics: training frontier models genuinely requires massive GPU clusters. Scaling laws appear immutable. However, the structural data (ε=0.58, suppression=0.68, theater=0.35, requires_active_enforcement=true) contradicts the mountain classification. The engine will compute this as a false summit: the 'natural law' framing obscures contingent choices about capital allocation, compute efficiency, and knowledge access.
constraint_indexing:constraint_classification(ai_compute_capital_moat, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_compute_capital_moat_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ai_compute_capital_moat, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ai_compute_capital_moat, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_compute_capital_moat, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ai_compute_capital_moat, TR),
    TR >= 0.70.

:- end_tests(ai_compute_capital_moat_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The moat extracts substantial value from excluded researchers and developers through exclusion, but the extraction is not total (snare-level 0.66+) because: (1) open-source models provide some alternative access; (2) inference-time scaling and fine-tuning reduce the need for frontier training compute; (3) some valuable research happens outside the moat (interpretability, alignment, specialized domains). The value increased from ~0.35 (2020) to 0.58 (2026) as compute requirements scaled and capital concentration accelerated. Suppression (0.68): High. Structural barriers to exit include: capital requirements ($100M-$1B+ for frontier training); GPU scarcity and geopolitical controls; talent concentration in 3-5 institutions; first-mover advantages in model release and derivative access. These are not trivial costs to bear — they are fundamental barriers to entry. Theater ratio (0.35): Moderate-low. Unlike purely performative constraints, the moat has genuine functional content: frontier models do require substantial compute, and capital concentration does enable long-term R&D. However, 35% theater reflects that some narrative is performative: efficiency-improving claims are sometimes overstated; the necessity of maximum-scale training is sometimes conflated with contingent architectural choices; safety arguments are sometimes used to justify closed models.
 *
 * PERSPECTIVAL GAP:
 *   The moat demonstrates extreme perspectival divergence. The incumbent lab (Rope) experiences the moat as enabling their mission — concentrating capital and talent allows sustained breakthroughs. The emerging researcher (Snare) experiences complete structural exclusion. The well-funded startup (Snare) experiences constrained dependency. The open-source coalition (Tangled Rope) experiences both coordination benefits (open weights enable distributed development) and extraction costs (must compete with far-better-resourced incumbents). The elite researcher (Tangled Rope) is paradoxically privileged (access to frontier compute) and constrained (cannot negotiate mobility without organizational affiliation). The developing nation (Snare) experiences geopolitical exclusion. The GPU supplier benefits from the moat but is partially independent of any single AI lab. The institutional narrative (Piton) sees the moat as 'natural scaling laws' — partly true, partly theatrical. The analytical observer risks the false summit (Mountain) — naturalizing contingent capital allocation as an immutable law of physics. This perspectival range (four snare positions, two rope positions, two tangled rope positions, one piton, one false-summit mountain) reveals that the moat is not a single structural phenomenon but a presheaf of constraints indexed by observational position.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent AI Lab: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.09. Net beneficiary; no cost to participating in the moat. Capital Provider: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.06. Net beneficiary; moat protects returns. Elite Researcher: Mixed position — beneficiary (access) + victim (constraint). Derived d≈0.55, f(d)≈0.75 (constrained exit dominates mobile theoretically possible; research career locked to incumbent institution). Emerging Group: Victim + trapped → d≈0.94, f(d)≈1.40. Maximum extraction. Well-Funded Startup: Victim + constrained → d≈0.82, f(d)≈1.25. High extraction but not maximal (some exit paths via acquisition or partnership). Open-Source Coalition: Organized + constrained → d≈0.48, f(d)≈0.60. Low-moderate extraction; organized agents have agency. Developing Nation: Victim + constrained → d≈0.88, f(d)≈1.32. High extraction (geopolitically constrained). GPU Supplier: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.06. Net beneficiary (dual position as controller). Institutional Narrative: analytical (piton classification from theater gate, not from high chi).
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION STRATEGY: The moat resolves mandatrophy through perspectival decomposition. At the incumbent lab level, the moat appears as pure coordination (Rope) — concentrating resources for ambitious research. At the researcher level, it appears as mixed (Tangled Rope) — enabling and constraining simultaneously. At the excluded researcher level, it appears as pure extraction (Snare). The mandatrophy question ('is this coordination or extraction?') has no single answer — it depends on structural position. The constraint is NOT mislabeled as tangled_rope for the aggregate system; rather, the aggregate system exhibits tangled_rope behavior (mixed coordination and extraction from multiple perspectives) while individual sub-positions see purer types. The claimed_type (tangled_rope) correctly captures the system-level classification: beneficiaries (incumbent labs, capital) experience genuine coordination function; victims (emerging groups, developing nations) experience genuine asymmetric extraction; enforcement is active (capital allocation, GPU access controls, talent recruitment incentives). The false-summit perspective (Mountain from the analytical observer) is explicitly included to show that naturalizing the moat as 'inevitable scaling laws' misses the enforcement machinery. This exemplifies how mandatrophy is resolved by indexical precision, not by re-classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    compute_scaling_necessity,
    'How much of the compute requirement for frontier AI is fundamental physics vs. contingent engineering/architectural choice?',
    'Comparison of compute budgets across frontier labs; measurement of efficiency gains from algorithmic improvements; analysis of architectural efficiency (inference-time vs training-time scaling); empirical test of whether modest labs can match capabilities with alternative approaches',
    'If mostly fundamental: moat is closer to mountain, less extractive. If mostly contingent: moat is engineered extraction, snare classification strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compute_scaling_necessity, empirical, 'Fundamental vs contingent nature of compute requirements').

omega_variable(
    talent_mobility_barrier,
    'Is the elite AI talent concentration driven by moat-enforced exclusivity or by genuine epistemic advantage of incumbent labs?',
    'Career trajectory analysis of researchers who left incumbent labs; productivity metrics of distributed vs concentrated teams; emergence of strong new labs with fresh recruitment; comparison of innovations per capital invested',
    'If moat-enforced exclusivity: talent barrier is extracted rent. If epistemic advantage: talent follows capability, not capital. Classification shifts from snare toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(talent_mobility_barrier, empirical, 'Whether talent concentration is moat-driven or merit-driven').

omega_variable(
    gpu_supply_cartel,
    'Does NVIDIA/foundry concentration constitute a cartel enforcing moat, or does it reflect genuine manufacturing advantage?',
    'Price analysis of GPU margins vs manufacturing cost; timeline of competitive foundries entering market; assessment of whether supply restrictions are technical or commercial; geopolitical pressure on supply chains',
    'If cartel: moat has external structural enforcement. If manufacturing advantage: moat is contingent on ongoing technical lead. Affects who is the true beneficiary (GPU supplier vs AI lab).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(gpu_supply_cartel, empirical, 'Whether GPU concentration is cartel or manufacturing advantage').

omega_variable(
    open_weights_convergence,
    'At what capability threshold does open-weights models reduce the moat''s effectiveness? Can distributed open-source development match frontier labs at inference time?',
    'Capability parity tracking (benchmarks: reasoning, coding, multimodal); time lag between closed and open models; performance per compute invested; emergence of specialized open alternatives',
    'If convergence timeline < 12 months: moat is temporary (scaffold-like). If > 2 years: moat is durable (snare). If never converges: moat is structural (mountain-like from incumbent perspective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(open_weights_convergence, empirical, 'Timeline for open-weights models to achieve capability parity').

omega_variable(
    regulatory_intervention_likelihood,
    'Will antitrust, export controls, or compute allocation policies erode the moat, or does the moat itself prevent such intervention?',
    'Analysis of regulatory precedent in other tech monopolies (search, cloud); political economy of AI regulation lobbying; feasibility of compute-allocation mandates; geopolitical fragmentation of AI development',
    'If intervention likely: moat is politicized, vulnerable to policy shock. If unlikely: moat is self-reinforcing. Affects lifecycle predictions.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_intervention_likelihood, preference, 'Likelihood and impact of regulatory intervention').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_compute_capital_moat, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aiccm_theater_t0, ai_compute_capital_moat, theater_ratio, 0, 0.22).
narrative_ontology:measurement(aiccm_theater_t3, ai_compute_capital_moat, theater_ratio, 3, 0.28).
narrative_ontology:measurement(aiccm_theater_t6, ai_compute_capital_moat, theater_ratio, 6, 0.35).

% Extraction over time
narrative_ontology:measurement(aiccm_extract_t0, ai_compute_capital_moat, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(aiccm_extract_t3, ai_compute_capital_moat, base_extractiveness, 3, 0.5).
narrative_ontology:measurement(aiccm_extract_t6, ai_compute_capital_moat, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_compute_capital_moat, resource_allocation).
narrative_ontology:boltzmann_floor_override(ai_compute_capital_moat, 0.45).
narrative_ontology:affects_constraint(ai_compute_capital_moat, gpu_supply_chain_concentration).
narrative_ontology:affects_constraint(ai_compute_capital_moat, elite_ai_talent_scarcity).
narrative_ontology:affects_constraint(ai_compute_capital_moat, open_weights_model_development).
narrative_ontology:affects_constraint(ai_compute_capital_moat, geopolitical_ai_fragmentation).

% DUAL FORMULATION NOTE:
% The AI Compute & Capital Moat decomposes into three structurally distinct constraints: (1) GPU supply scarcity (affects compute access); (2) elite talent concentration (affects research capability); (3) capital concentration (affects funding capacity). Each has its own ε and network position. This story represents the aggregate moat constraint (tangled_rope, ε=0.58). The downstream constraints (open_weights development, geopolitical fragmentation) are developing alternative pathways that may eventually erode the moat's extractive component.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_compute_capital_moat, powerful, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
