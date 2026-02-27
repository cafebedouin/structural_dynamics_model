% ============================================================================
% CONSTRAINT STORY: rare_earth_export_restrictions
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rare_earth_export_restrictions, []).

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
 *   constraint_id: rare_earth_export_restrictions
 *   human_readable: Rare Earth Export Restrictions and Supply Chain Dependency
 *   domain: economic/geopolitical
 *
 * SUMMARY:
 *   China's dominance of the rare earth elements supply chain — controlling
 *   approximately 60-80% of global production and 90%+ of refined capacity —
 *   combined with its use of export restrictions as a tool of economic and
 *   geopolitical leverage, creates a structural constraint on manufacturers
 *   globally dependent on REEs for critical technologies. The constraint
 *   exhibits pure snare characteristics from most perspectives: manufacturers
 *   are trapped without alternatives, exit costs are prohibitive, and
 *   suppression operates through both regulatory denial (export licenses) and
 *   structural scarcity. The analytical observer sees a stable snare
 *   maintained by geology and political control, not theatrical legitimacy.
 *   However, competing perspectives reveal complexity: the Chinese government
 *   experiences the constraint as legitimate resource nationalism (rope);
 *   organized technology consortiums see mixed coordination-extraction
 *   (tangled rope); the international trade regime appears as degraded piton
 *   infrastructure. The constraint's low theater ratio (0.35) distinguishes
 *   it from theatrical pitons — this is functional coercion, not
 *   performative, which increases persistence. Extractiveness has risen from
 *   0.38 to 0.52 over the interval as China has weaponized supply control in
 *   geopolitical disputes (particularly with Japan, Philippines, and
 *   US-allied nations).
 *
 * KEY AGENTS:
 *   - Dependent Manufacturers: Primary victims (powerless/trapped) — electronics, defense, renewable energy sectors with no substitutes; bear full extraction cost
 *   - Chinese Government: Primary beneficiary (institutional/arbitrage) — captures geopolitical leverage and revenue from export licensing; controls strategic asset
 *   - Chinese REE Producers: Secondary beneficiary (institutional/arbitrage) — benefit from monopoly pricing and reduced competition during export-restricted periods
 *   - Alternative Supply Initiatives: Secondary victim (moderate/constrained) — high capex, long timelines, political risk; face China's pricing discipline and cost barriers
 *   - Allied Technology Consortium: Organized victim (organized/constrained) — US, EU, Japan, allied manufacturers coordinating defensive strategies; can influence policy but cannot unilaterally exit
 *   - International Trade Regime: Institutional infrastructure (institutional/arbitrage) — WTO framework legitimizes restrictions while appearing neutral; degraded piton
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — measures structural stability and persistence of snare mechanisms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rare_earth_export_restrictions, 0.52).
domain_priors:suppression_score(rare_earth_export_restrictions, 0.68).
domain_priors:theater_ratio(rare_earth_export_restrictions, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rare_earth_export_restrictions, extractiveness, 0.52).
narrative_ontology:constraint_metric(rare_earth_export_restrictions, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(rare_earth_export_restrictions, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rare_earth_export_restrictions, snare).
narrative_ontology:human_readable(rare_earth_export_restrictions, "Rare Earth Export Restrictions and Supply Chain Dependency").
narrative_ontology:topic_domain(rare_earth_export_restrictions, "economic/geopolitical").

domain_priors:requires_active_enforcement(rare_earth_export_restrictions).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rare_earth_export_restrictions, chinese_government).
narrative_ontology:constraint_beneficiary(rare_earth_export_restrictions, chinese_ree_producers).
narrative_ontology:constraint_victim(rare_earth_export_restrictions, dependent_manufacturers).
narrative_ontology:constraint_victim(rare_earth_export_restrictions, global_supply_chain_reliability).
narrative_ontology:constraint_victim(rare_earth_export_restrictions, technology_sector_independence).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEPENDENT MANUFACTURER (SNARE) — Electronics manufacturers without REE sourcing alternatives are structurally trapped. REEs are critical for permanent magnets, phosphors, and catalysts with no adequate substitutes at scale. Exit requires multi-year supply chain restructuring with massive capital investment. d≈0.92, f(d)≈1.40, σ=1.2 → χ≈0.87. The constraint operates through scarcity enforcement and export license denial.
constraint_indexing:constraint_classification(rare_earth_export_restrictions, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ALTERNATIVE SUPPLY INITIATIVE (SNARE) — Competing rare earth producers (Vietnam, Myanmar, Australia) face high capex barriers and long project timelines (5-10 years to production). Political risk, environmental costs, and China's pricing discipline make scaling alternatives costly and slow. d≈0.78, f(d)≈1.12, σ=1.1 → χ≈0.65. Constrained exit: can build alternatives but the constraint persists during build-out period.
constraint_indexing:constraint_classification(rare_earth_export_restrictions, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: CHINESE GOVERNMENT (ROPE) — Experiences REE control as a coordination mechanism for resource nationalism and geopolitical leverage. The export restriction solves a collective action problem (preventing capital flight and resource depletion) while generating political benefit. d≈0.02, f(d)≈-0.14, σ=0.9 → χ≈-0.07. Net beneficiary; sees the constraint as a legitimate tool for managing strategic assets.
constraint_indexing:constraint_classification(rare_earth_export_restrictions, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ALLIED TECHNOLOGY CONSORTIUM (TANGLED ROPE) — US, EU, Japan, and allied manufacturers organized through trade groups and defense departments see the constraint as both a coordination problem (securing stable supply) and a tool of asymmetric extraction (geopolitical coercion via supply shock). d≈0.55, f(d)≈0.76, σ=1.2 → χ≈0.40. Organizations can coordinate sourcing alternatives and lobbying, reducing but not eliminating extraction.
constraint_indexing:constraint_classification(rare_earth_export_restrictions, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL TRADE REGIME (PITON) — The WTO framework permits export restrictions for non-renewable resources under GATT Article XX. The rule persists as neutral policy infrastructure, but in practice it functions as performative legitimation: it appears to constrain state behavior while permitting the exact behavior it ostensibly forbids (China's export restriction is technically WTO-compliant even as it extracts massive value). theater_ratio=0.35 suggests functional (not theatrical) enforcement, but the regime's neutrality is degraded — it no longer coordinates resource trade, only validates state control.
constraint_indexing:constraint_classification(rare_earth_export_restrictions, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — From a civilizational view, this constraint exhibits the structural properties of a snare with high persistence: extractiveness (0.52), suppression (0.68), and functional enforcement (theater=0.35) indicate a stable coercive mechanism. Unlike theatrical constraints that decay, this one operates through real scarcity and legitimate control over productive assets. The analytical frame sees a snare maintained by geology and political control, not by performative legitimacy.
constraint_indexing:constraint_classification(rare_earth_export_restrictions, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rare_earth_export_restrictions_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rare_earth_export_restrictions, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rare_earth_export_restrictions, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(rare_earth_export_restrictions, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(rare_earth_export_restrictions, TR),
    TR >= 0.70.

:- end_tests(rare_earth_export_restrictions_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Significant but not maximal. Chinese export restrictions generate substantial economic rent through dual-pricing (domestic vs export), licensing fees, and monopoly margins on refined materials. However, the extraction is constrained by technical limits (demand destruction reduces revenue) and political costs (trade retaliation, alternative supply investment). Theater ratio (0.35): Functional. China's export control operates through explicit regulatory mechanisms (export licenses, quotas, banned list) with clear technical enforcement. Unlike theatrical constraints, there is no pretense — the restrictions are openly stated and directly enforced. This functional character increases persistence: functional constraints survive scrutiny; theatrical ones decay when exposed. Suppression (0.68): High. Manufacturers face multiple suppressive factors: (1) technical barriers to substitution (REEs have unique nuclear/chemical properties with no adequate alternatives at scale), (2) capex/timeline barriers to alternative sourcing (5-10 years, billions in investment), (3) cartel pricing discipline (China coordinates pricing floors across producers), (4) regulatory uncertainty (export quotas change unpredictably, increasing risk premium). These barriers operate independently, creating high structural suppression.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap here is unusually acute. The dependent manufacturer sees a snare: trapped with no exit. The Chinese government sees rope: a legitimate coordination mechanism for resource sovereignty. The allied consortium sees tangled rope: mixed coordination (information about stable sourcing) and extraction (geopolitical coercion). The trade regime sees itself as rope (neutral rule enforcement) but functions as piton (performative legitimacy masking state control). The analytical observer sees snare (stable, functional coercion with geology + politics). The gap reflects competing ontologies: resource nationalism vs supply chain colonialism, legitimate sovereignty vs predatory monopoly, WTO-compliant policy vs geopolitical weapon. Each perspective captures real structural elements. The snare classification (from the analytical observer) is authoritative because it identifies the constraint's persistence mechanism — functional enforcement + structural scarcity — which resists the narratives that might dissolve it (trade regime legitimacy, market substitution, alternative supply).
 *
 * DIRECTIONALITY LOGIC:
 *   Dependent manufacturers: Victim + trapped → d≈0.92, f(d)≈1.40. Maximum extraction directionality. No exit option; bear all extraction cost. Chinese government: Beneficiary + arbitrage → d≈0.02, f(d)≈-0.14. Minimal extraction from government's perspective; net beneficiary. Alternative supply: Victim + constrained → d≈0.78, f(d)≈1.12. High extraction but not maximum; can build alternatives but timeline and capex create ongoing cost. Allied consortium: Organized + constrained → d≈0.55, f(d)≈0.76. Mixed directionality; organized agents have some agency but real structural constraints from supply dependence. International trade regime: Institutional + arbitrage → d≈0.02, f(d)≈-0.14. Piton classification arises not from high extraction but from theater gate: regime appears to regulate while legitimizing the exact behavior it should constrain. Analytical observer: analytical → d≈0.72, f(d)≈1.15. High extraction because observer sees structural inevitability and functional persistence.
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE CONFIRMED — COMPLEX VARIANT. The constraint resolves mandatrophy by distinguishing between false snares (theatrical coercion that decays) and true snares (functional coercion that persists). The theater ratio (0.35) is critical: this is NOT a theatrical constraint. Export restrictions operate through explicit regulatory mechanisms with technical enforcement, not performative legitimacy. The extraction mechanism is functional: Chinese government directly controls productive capacity + refined output bottleneck + geopolitical leverage of supply chokes. The dependent manufacturers experience no coordination benefit — the constraint exists solely to extract value and political concession. No beneficiary group (except China) sees rope or scaffold characteristics. This is a unidirectional snare maintained by geology (REE scarcity and Chinese deposits) + politics (export control) + economics (monopoly pricing). The snare is stable over civilizational timescales unless technical substitution or alternative supply reaches scale — both are decades away at best. The analytical observer's snare classification is the constraint's true type because it identifies why the snare persists: it is not held together by theater (which can be called out and abandoned), but by real structural asymmetries (supply geography + refining capacity + geopolitical will).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    substitution_technical_feasibility,
    'Can REE substitution or recycling reduce dependency to levels where export restrictions cease to function as coercive tools?',
    'Technical roadmap analysis of substitution candidates (silicon-based magnets, alternative phosphors); scaling of recycling infrastructure; cost parity timelines',
    'If substitution/recycling mature in 10 years: constraint shifts from snare toward scaffold (sunset visible). If technical barriers persist 20+ years: snare classification remains stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substitution_technical_feasibility, empirical, 'Technical feasibility of substitutes and recycling at scale').

omega_variable(
    alternative_supply_viability,
    'Will competing REE sources (Vietnam, Myanmar, Australia, USA) reach scale sufficient to break China''s monopoly pricing power?',
    'Capital deployment tracking; project completion timelines; cost benchmarking vs China; long-term price signal response',
    'If competing sources reach 30% of global supply: Chinese export leverage reduces, constraint shifts to tangled_rope. If China maintains 70%+ share: snare classification confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_supply_viability, empirical, 'Whether alternative sources can achieve competitive scale').

omega_variable(
    geopolitical_coercion_intent,
    'Is China''s export restriction primarily resource nationalism (coordination) or deliberate geopolitical coercion (extraction)?',
    'Policy statement analysis; pattern correlation between restriction timing and diplomatic disputes; comparative analysis with other commodity controls',
    'If primarily resource nationalism: constraint appears from China''s perspective as rope, not snare. If primarily coercion: constraint from analytical perspective is clearly snare with no coordination function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(geopolitical_coercion_intent, conceptual, 'Whether restrictions serve resource control or geopolitical coercion').

omega_variable(
    strategic_stockpile_effectiveness,
    'Can defensive strategic stockpiles reduce vulnerability to export shocks faster than supply-side alternatives can be built?',
    'Stockpile capacity analysis; consumption trajectory modeling; comparison of stockpile lifespan (months) vs alternative supply timelines (years)',
    'If stockpiles extend resilience 18-24 months: scaffold logic becomes viable (temporary storage buys time for alternatives). If stockpiles provide <6 months coverage: snare logic persists (storage cannot substitute for supply control).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(strategic_stockpile_effectiveness, empirical, 'Effectiveness of strategic reserves as coercion mitigation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rare_earth_export_restrictions, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ree_tr_t0, rare_earth_export_restrictions, theater_ratio, 0, 0.28).
narrative_ontology:measurement(ree_tr_t8, rare_earth_export_restrictions, theater_ratio, 8, 0.32).
narrative_ontology:measurement(ree_tr_t16, rare_earth_export_restrictions, theater_ratio, 16, 0.35).

% Extraction over time
narrative_ontology:measurement(ree_be_t0, rare_earth_export_restrictions, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(ree_be_t8, rare_earth_export_restrictions, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(ree_be_t16, rare_earth_export_restrictions, base_extractiveness, 16, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rare_earth_export_restrictions, resource_allocation).
narrative_ontology:affects_constraint(rare_earth_export_restrictions, semiconductor_supply_chain_vulnerability).
narrative_ontology:affects_constraint(rare_earth_export_restrictions, defense_technology_access_restriction).
narrative_ontology:affects_constraint(rare_earth_export_restrictions, green_energy_magnet_dependency).

% DUAL FORMULATION NOTE:
% Rare earth export restrictions operate as a single constraint but create cascading effects on technology supply chains. Upstream constraint: Chinese mining/refining capacity control. This story models the export restriction mechanism itself (ε=0.52, snare). Downstream constraints (semiconductor supply, defense access, green energy magnets) inherit dependency structure from this snare; their extractiveness derives from REE availability choke points.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rare_earth_export_restrictions, institutional, 0.02).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
