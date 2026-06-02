% ============================================================================
% CONSTRAINT STORY: ai_superpowers_race_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_superpowers_race_2026, []).

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
 *   constraint_id: ai_superpowers_race_2026
 *   human_readable: The Sino-American AI Implementation Gap
 *   domain: technological/geopolitical
 *
 * SUMMARY:
 *   The Sino-American AI Implementation Gap represents a structural
 *   constraint on global technological development created by the divergence
 *   of two national systems pursuing incompatible optimization targets. The
 *   US focuses on AGI frontier research (moonshots with high uncertainty,
 *   high capital, long timelines), while China focuses on consumer AI
 *   deployment (rapid iteration, market-driven optimization, distributed
 *   testing). This constraint exhibits the tangled rope signature: genuine
 *   coordination benefits for the leading agents (frontier labs, engineering
 *   firms) combined with asymmetric extraction from dependent actors
 *   (developing economies, mid-tier commercial AI). The constraint is not
 *   pure coordination (mutual benefit) because the gap itself generates
 *   asymmetric power — whoever controls the AI stack controls downstream
 *   applications, data infrastructure, and technical standards. It is not
 *   pure extraction (dominance through coercion) because both sides genuinely
 *   compete through technical excellence rather than pure suppression. The
 *   theater ratio (0.48) reflects that much of the public discourse around
 *   the race is performative — geopolitical rhetoric masking technical
 *   uncertainty — but the underlying engineering effort is functional. The
 *   extractiveness has risen from 0.32 to 0.58 over the interval as the
 *   winner-take-most dynamics of AI markets have concentrated, increasing
 *   asymmetry and reducing the number of viable independent paths.
 *
 * KEY AGENTS:
 *   - Chinese Engineering Firms: Primary beneficiary (institutional/arbitrage) — capture consumer AI market dominance and data advantages from rapid iteration cycles
 *   - US Frontier Research Labs: Secondary beneficiary (institutional/arbitrage) — concentrate frontier AI capital and talent; dominate moonshot narrative
 *   - Developing Economies: Primary victim (powerless/trapped) — forced infrastructure lock-in; no independent verification; cannot exit dependency relationships
 *   - US Commercial AI Sector (Mid-Tier): Constrained victim (organized/constrained) — brain drain to frontier labs; caught between regulation and competition
 *   - Chinese Regulatory Apparatus: Constrained implementer (powerful/constrained) — must enable innovation while suppressing unauthorized capabilities; active enforcement required
 *   - Technical Talent (AI Researchers): Mobile agent (powerful/mobile) — high exit capacity but increasing geopolitical constraint; can arbitrage location but face visa/mobility restrictions
 *   - EU Regulatory Coalition: Organized challenger (organized/constrained) — attempting third pathway via AI Act; sunset-based approach
 *   - International Governance Consensus: Degraded institutional (institutional/analytical) — performative dialogue masking decoupled technical stacks
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_superpowers_race_2026, 0.58).
domain_priors:suppression_score(ai_superpowers_race_2026, 0.65).
domain_priors:theater_ratio(ai_superpowers_race_2026, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_superpowers_race_2026, extractiveness, 0.58).
narrative_ontology:constraint_metric(ai_superpowers_race_2026, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(ai_superpowers_race_2026, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_superpowers_race_2026, tangled_rope).
narrative_ontology:human_readable(ai_superpowers_race_2026, "The Sino-American AI Implementation Gap").
narrative_ontology:topic_domain(ai_superpowers_race_2026, "technological/geopolitical").

domain_priors:requires_active_enforcement(ai_superpowers_race_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_superpowers_race_2026, chinese_engineering_firms).
narrative_ontology:constraint_beneficiary(ai_superpowers_race_2026, us_frontier_research_labs).
narrative_ontology:constraint_victim(ai_superpowers_race_2026, developing_economies).
narrative_ontology:constraint_victim(ai_superpowers_race_2026, global_competitive_parity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEVELOPING ECONOMY (SNARE) — Trapped between two superpowers with incompatible AI stacks, standards, and training datasets. Cannot exit the dependency relationship. Forced to choose infrastructure lock-in (US cloud ecosystem vs Chinese platform dominance). No independent verification pathway for claims about superior AI capabilities. Maximal extraction.
constraint_indexing:constraint_classification(ai_superpowers_race_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CHINESE ENGINEERING FIRMS (ROPE) — Primary beneficiaries. Benefit from coordination via rapid iteration cycles, lower regulatory friction, large domestic market as testing ground, and winner-take-all consumer AI dominance. High exit capacity through multinational expansion and data arbitrage. Extraction runs toward this agent.
constraint_indexing:constraint_classification(ai_superpowers_race_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: US FRONTIER RESEARCH LABS (ROPE) — Secondary beneficiary. Benefit from AGI moonshot framing, frontier capital concentration, and asymmetric access to compute resources. High arbitrage exit via licensing and capability exports. The constraint coordinates research funding flows and talent concentration.
constraint_indexing:constraint_classification(ai_superpowers_race_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: US COMMERCIAL AI SECTOR (TANGLED_ROPE) — Organized but constrained by frontier lab brain drain and capital concentration in moonshots. Benefits from open-source ecosystems (coordination) but faces extraction via talent movement to frontier labs and regulatory compliance costs. Can coordinate through industry consortia but with limited exit capacity.
constraint_indexing:constraint_classification(ai_superpowers_race_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CHINESE REGULATORY APPARATUS (TANGLED_ROPE) — Constrained by need to maintain content control while enabling rapid iteration. Benefits from tech sector growth (coordination for state objectives) but bears costs of contradictory mandates: enable innovation while suppressing unauthorized capabilities. Active enforcement required to maintain the dual mandate.
constraint_indexing:constraint_classification(ai_superpowers_race_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: TECHNICAL TALENT / AI RESEARCHERS (TANGLED_ROPE) — Mobile agents with significant exit capacity. Experience extraction through visa restrictions, brain-drain incentives, and geopolitical talent competition. Also benefit from coordination (international conferences, published standards). Can arbitrage location and employer choice but face increasing geopolitical constraints on mobility.
constraint_indexing:constraint_classification(ai_superpowers_race_2026, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: INTERNATIONAL AI GOVERNANCE (PITON) — Degraded consensus on shared safety standards, measurement protocols, and capability transparency. The performative international dialogue (UN AI advisory groups, bilateral frameworks) masks the reality of decoupled competitive stacks and measurement incommensurability. Theater ratio high because rhetorical commitment to governance without structural enforcement. Function has atrophied as geopolitical competition outpaced norm-setting capacity.
constraint_indexing:constraint_classification(ai_superpowers_race_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 8: EU REGULATORY COALITION (SCAFFOLD) — Organized agent attempting to create a third pathway via the AI Act. Sees the Sino-American gap as a temporary competitive phase with a sunset: harmonized European standards + export controls are supposed to decouple from both superpowers' stacks within 5-7 years. High suppression (regulatory friction) is tolerated because it has a known decline trajectory and explicit sunset conditions.
constraint_indexing:constraint_classification(ai_superpowers_race_2026, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational/physics perspective, the implementation gap reflects irreducible tradeoffs in compute architecture and algorithmic scaling. The constraint may be framed as a competition between political systems, but the underlying limit is thermodynamic: energy-to-capability scaling curves follow physical laws independent of governance. However, this naturalizes contingent choices (chip design, cooling infrastructure, power consumption) as immutable.
constraint_indexing:constraint_classification(ai_superpowers_race_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_superpowers_race_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ai_superpowers_race_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ai_superpowers_race_2026, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_superpowers_race_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ai_superpowers_race_2026, TR),
    TR >= 0.70.

:- end_tests(ai_superpowers_race_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts from developing economies through forced infrastructure lock-in and from mid-tier US commercial AI through talent concentration and regulatory asymmetry. However, the extraction is not maximal (0.70+) because both superpowers genuinely compete on technical merit and the extraction is contingent on maintaining capability leadership — if either side loses technical dominance, the extraction mechanism weakens. The rising trajectory (0.32 → 0.58) reflects winner-take-most consolidation as capabilities diverged and market concentration increased. Suppression (0.65): Moderate-high. Significant barriers to independent AI development include compute cost barriers, talent scarcity, geopolitical restrictions on chip access and visa mobility, and data fragmentation. However, suppression is not maximal because open-source models and distributed research reduce some barriers — there are non-zero paths for developing economies to participate, they are just asymmetrically costly. Theater ratio (0.48): Moderate-low. Much of the public discourse is performative (geopolitical rhetoric, capability announcements), but the underlying technical effort is functional. The declining trajectory (0.55 → 0.48) reflects that as capabilities have become more concrete, the gap has become less theater and more measurable technical differentiation. Claimed type (tangled_rope) reflects that this constraint combines genuine coordination (technical standards, research collaboration) with asymmetric extraction (market dominance, infrastructure lock-in).
 *
 * PERSPECTIVAL GAP:
 *   The structural gap between beneficiary and victim perspectives is maximal. The beneficiaries (Chinese engineering firms, US frontier labs) experience the constraint as enabling their dominance — they see cooperation, standard-setting, and research coordination. The victims (developing economies) experience the constraint as coercive lock-in — they face binary choices between incompatible infrastructure stacks with no independent alternatives. The organized actors (EU, mid-tier US firms, technical talent) experience tension: they benefit from some aspects (international standards, frontier research access) while bearing extraction costs (regulatory asymmetry, talent drain, geopolitical constraints). This gap reflects that the constraint is genuinely hybrid: it coordinates at the superpower level while extracting from everyone else. The mountain perspective (physics-imposed) risks naturalizing what is actually a contingent infrastructure choice as inevitable — energy-to-capability tradeoffs are real but the specific allocation of compute to moonshots vs consumer iteration is policy-determined.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations map directly to directionality. Chinese engineering firms and US frontier labs are declared beneficiaries because they capture the gains from the constraint (market dominance, capital concentration, talent flow). Developing economies and global competitive parity are declared victims because they bear the costs (lock-in, asymmetric dependency, reduced alternatives). The Chinese regulatory apparatus faces a split directionality: it benefits from tech sector growth but bears costs of contradictory mandates (enable innovation vs suppress capabilities) — this justifies the tangled rope classification with active enforcement required. Technical talent has high mobility (arbitrage-capable) despite facing geopolitical constraints, reducing effective d despite victim-like aspects. The EU's constrained position reflects that it can coordinate (via AI Act) but cannot exit (it is embedded in global AI markets) — this produces tangled rope classification with sunset logic.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy between 'is this cooperation or extraction?' by showing it is BOTH simultaneously, from different perspectives. This is precisely what tangled rope captures: the same constraint that coordinates frontier research (genuine benefit) also extracts from developing economies (genuine cost). The false mountains are: (1) 'AGI race is inevitable physics' — actually a policy choice about R&D allocation; (2) 'International AI governance is solving this' — actually performative (piton) because it lacks enforcement mechanisms. The real structural fact is that the constraint persists because it benefits the dominant actors (Chinese firms, US labs) enough to maintain asymmetry despite rhetorical commitment to governance. The scaffold perspective is not aspirational — EU AI Act represents genuine institutional emergence that could decouple the system into three poles. The snare perspective is not exaggeration — developing economies genuinely have no exit options if they want AI-enabled infrastructure. The constraint exhibits class-stratified classification precisely because it is a tangled rope, not a universal phenomenon.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    agi_timeline_uncertainty,
    'What is the actual trajectory of AGI capability emergence in the next 5-10 years, and does frontier research or rapid iteration get there first?',
    'Longitudinal tracking of capability benchmarks (reasoning, long-context, multi-modal reasoning); comparison of frontier model releases vs consumer AI deployment quality metrics',
    'If frontier research dominates: US moonshot framing is structurally correct. If rapid iteration dominates: Chinese consumer AI dominance is structurally correct. If both proceed on separate tracks: constraint persists as genuine bifurcation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(agi_timeline_uncertainty, empirical, 'Trajectory of AGI capability emergence and which approach achieves it first').

omega_variable(
    measurement_incommensurability,
    'Are US and Chinese AI capability claims measurable against a shared benchmark, or are they fundamentally incommensurable (moonshots vs consumer optimization)?',
    'Independent benchmark testing (MMLU, ARC, real-world task performance) on both US frontier models and Chinese consumer models; identification of systematic bias in each evaluation regime',
    'If incommensurable: constraint is irresolvable (each side legitimately winning in its domain). If commensurable: one side''s claimed advantage is falsifiable, compression of the gap becomes possible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(measurement_incommensurability, empirical, 'Whether US and Chinese AI claims are measurable against shared benchmarks').

omega_variable(
    talent_decoupling_threshold,
    'At what level of geopolitical friction do AI talent movements (emigration/brain drain) drop below the threshold needed for either superpower to sustain capability leadership?',
    'Tracking of emigration rates of AI PhD students, researcher visa restrictions, industry hiring patterns in both countries; correlation with capability release cycles',
    'If talent flow persists: constraint maintains asymmetric power dynamics. If talent decouples: convergence toward capability parity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(talent_decoupling_threshold, empirical, 'Critical threshold for talent movement decoupling').

omega_variable(
    regulatory_fragmentation_path,
    'Will EU regulatory emergence (AI Act) create a genuine third stack, or will it effectively choose one superpower''s architecture despite regulatory neutrality claims?',
    'Implementation analysis of EU AI Act compliance across US and Chinese models; tracking of infrastructure procurement decisions; measurement of technical lock-in to either stack',
    'If genuine third path: constraint becomes three-pole system. If functional alignment: EU becomes de facto US or China ally, constraint tightens into bipolar.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_fragmentation_path, conceptual, 'Whether EU creates genuine third AI architecture path').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_superpowers_race_2026, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(airace_tr_t0, ai_superpowers_race_2026, theater_ratio, 0, 0.55).
narrative_ontology:measurement(airace_tr_t3, ai_superpowers_race_2026, theater_ratio, 3, 0.51).
narrative_ontology:measurement(airace_tr_t6, ai_superpowers_race_2026, theater_ratio, 6, 0.48).

% Extraction over time
narrative_ontology:measurement(airace_be_t0, ai_superpowers_race_2026, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(airace_be_t3, ai_superpowers_race_2026, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(airace_be_t6, ai_superpowers_race_2026, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_superpowers_race_2026, global_infrastructure).
narrative_ontology:affects_constraint(ai_superpowers_race_2026, semiconductor_supply_chain_bifurcation).
narrative_ontology:affects_constraint(ai_superpowers_race_2026, ai_talent_migration_restrictions).
narrative_ontology:affects_constraint(ai_superpowers_race_2026, data_localization_requirements).

% DUAL FORMULATION NOTE:
% The AI Implementation Gap decomposes into three structurally distinct claims: (1) Capability Frontier — US leads in raw capability ceiling (AGI moonshots); (2) Consumer Deployment — China leads in real-world consumer iteration and market dominance; (3) Infrastructure Stack — technical standards and hardware-software coupling are becoming geopolitically bifurcated. These are linked constraints with different ε values. The capability frontier has lower ε (more genuine coordination, less extraction) because it is measured by published benchmarks and reproducible research. The consumer deployment has higher ε (more extraction) because it involves winner-take-most market dynamics and data lock-in. The infrastructure bifurcation is the binding constraint that forces incompatibility.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_superpowers_race_2026, organized, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
