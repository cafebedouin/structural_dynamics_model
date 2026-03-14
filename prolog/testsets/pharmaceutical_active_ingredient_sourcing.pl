% ============================================================================
% CONSTRAINT STORY: pharmaceutical_active_ingredient_sourcing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_pharmaceutical_active_ingredient_sourcing, []).

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
 *   constraint_id: pharmaceutical_active_ingredient_sourcing
 *   human_readable: Pharmaceutical Active Ingredient Sourcing and Supply Chain Coordination
 *   domain: pharmaceutical_economics/supply_chain
 *
 * SUMMARY:
 *   Pharmaceutical active ingredient (API) sourcing represents a global
 *   coordination challenge masked as a natural economic necessity. The
 *   constraint involves coordinating quality standards, supply reliability,
 *   and manufacturing capacity across geographically dispersed actors with
 *   asymmetric power and exit options. Over the past two decades (interval
 *   0-20), extractiveness has risen from 0.35 to 0.62 as manufacturing has
 *   concentrated in Asia (primarily India and China) and as regulatory
 *   frameworks have become more complex. Theater has also risen from 0.35 to
 *   0.55, reflecting increasing performativity in regulatory oversight—GMP
 *   compliance has become more bureaucratic without corresponding quality
 *   improvements, serving primarily as a barrier to new entrants. The
 *   constraint is genuinely Tangled Rope: it solves the real coordination
 *   problem of global drug supply while simultaneously extracting through
 *   geographic concentration, IP barriers, regulatory gatekeeping, and
 *   capacity control. Patients in resource-poor regions and generic
 *   manufacturers in developing nations bear the costs while major
 *   pharmaceutical companies and Asian API manufacturers capture benefits.
 *
 * KEY AGENTS:
 *   - Patients in Developing Nations: Primary victims (powerless/trapped) — no exit options; dependent on supply chains; bear cost of price volatility and shortages
 *   - Generic Drug Manufacturers in Developing Nations: Primary victims (powerless/trapped) — trapped by API dependencies and regulatory barriers; cannot source alternatives at viable cost
 *   - Major Pharmaceutical Companies: Primary beneficiaries (institutional/arbitrage) — can integrate vertically, diversify suppliers, or relocate sourcing; experience constraint as coordination
 *   - Active Ingredient Manufacturers in Asia: Primary extractors (organized/mobile) — control capacity and pricing; coordinate supply networks while extracting through market power
 *   - Mid-Tier Pharmaceutical Manufacturers: Secondary actors (moderate/constrained) — experience mixed coordination and extraction; face barriers but have some exit options
 *   - Regulatory Bodies (FDA, EMA, national regulators): Institutional gatekeepers (institutional/arbitrage) — maintain approval frameworks that appear to serve quality but function as entry barriers
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees constraint as structurally Tangled Rope, not natural market outcome
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(pharmaceutical_active_ingredient_sourcing, 0.62).
domain_priors:suppression_score(pharmaceutical_active_ingredient_sourcing, 0.68).
domain_priors:theater_ratio(pharmaceutical_active_ingredient_sourcing, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(pharmaceutical_active_ingredient_sourcing, extractiveness, 0.62).
narrative_ontology:constraint_metric(pharmaceutical_active_ingredient_sourcing, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(pharmaceutical_active_ingredient_sourcing, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(pharmaceutical_active_ingredient_sourcing, tangled_rope).
narrative_ontology:human_readable(pharmaceutical_active_ingredient_sourcing, "Pharmaceutical Active Ingredient Sourcing and Supply Chain Coordination").
narrative_ontology:topic_domain(pharmaceutical_active_ingredient_sourcing, "pharmaceutical_economics/supply_chain").

domain_priors:requires_active_enforcement(pharmaceutical_active_ingredient_sourcing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(pharmaceutical_active_ingredient_sourcing, active_ingredient_manufacturers).
narrative_ontology:constraint_beneficiary(pharmaceutical_active_ingredient_sourcing, pharmaceutical_companies).
narrative_ontology:constraint_victim(pharmaceutical_active_ingredient_sourcing, developing_nations).
narrative_ontology:constraint_victim(pharmaceutical_active_ingredient_sourcing, generic_manufacturers).
narrative_ontology:constraint_victim(pharmaceutical_active_ingredient_sourcing, patient_access).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PATIENTS IN RESOURCE-CONSTRAINED REGIONS (SNARE) — No ability to exit or relocate; dependent on supply chains controlled by distant manufacturers. Bear full cost of sourcing disruptions, price volatility, and supply unavailability. No coordination benefit, maximum extraction.
constraint_indexing:constraint_classification(pharmaceutical_active_ingredient_sourcing, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: GENERIC DRUG MANUFACTURERS IN DEVELOPING NATIONS (SNARE) — Trapped by API dependencies, manufacturing capacity constraints, and regulatory barriers. Cannot source alternative inputs at viable cost. No exit option when API suppliers collude or restrict capacity. Experience maximum extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(pharmaceutical_active_ingredient_sourcing, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: MID-TIER PHARMACEUTICAL MANUFACTURERS (TANGLED ROPE) — Benefit from coordinated supply chain standards and quality assurance (coordination), but constrained by limited supplier alternatives and dependence on major API manufacturers. Can exit through vertical integration or alternative sourcing but at significant cost. Mixed extraction and coordination.
constraint_indexing:constraint_classification(pharmaceutical_active_ingredient_sourcing, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: MAJOR PHARMACEUTICAL COMPANIES (ROPE) — Primary beneficiaries with arbitrage options (can integrate vertically, diversify suppliers, or relocate sourcing). Experience the constraint as pure coordination: formalized API quality standards, supply chain transparency, and regulatory harmonization reduce transaction costs. Net beneficiaries.
constraint_indexing:constraint_classification(pharmaceutical_active_ingredient_sourcing, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ACTIVE INGREDIENT MANUFACTURERS IN ASIA (TANGLED ROPE) — Organized, mobile actors who coordinate global supply chains while extracting through capacity control, price-setting, and regulatory leverage. Can exit by shifting geographic sourcing but choose not to. Genuine coordination function (supply network efficiency) paired with asymmetric extraction power.
constraint_indexing:constraint_classification(pharmaceutical_active_ingredient_sourcing, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 6: REGULATORY OVERSIGHT BODIES (PITON) — FDA, EMA, and national regulators maintain approvals, quality standards, and manufacturing facility certifications that appear to serve a coordination function (ensuring drug quality). However, regulatory gatekeeping has become largely performative—complex GMP requirements and facility approval processes primarily entrench incumbent manufacturers and create barriers for new entrants. Theater ratio (0.55) reflects moderate performativity: genuine quality oversight exists but is layered with protectionist barriers maintained through inertia.
constraint_indexing:constraint_classification(pharmaceutical_active_ingredient_sourcing, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From the civilizational/universal scale, pharmaceutical API sourcing exhibits genuine coordination (global supply reliability, quality standards, regulatory harmonization) simultaneously with asymmetric extraction (geographic concentration in Asia, IP barriers, capacity control). The constraint persists because it generates real coordination value while systematically extracting from powerless populations and smaller competitors.
constraint_indexing:constraint_classification(pharmaceutical_active_ingredient_sourcing, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(pharmaceutical_active_ingredient_sourcing_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(pharmaceutical_active_ingredient_sourcing, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(pharmaceutical_active_ingredient_sourcing, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(pharmaceutical_active_ingredient_sourcing, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(pharmaceutical_active_ingredient_sourcing, TR),
    TR >= 0.70.

:- end_tests(pharmaceutical_active_ingredient_sourcing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): High and rising. The baseline measurement shows extraction growing from 0.35 to 0.62 over 20 years as API manufacturing consolidated geographically. This reflects real structural changes: increased dependency of developing-nation generic manufacturers on Asian suppliers, rising relative pricing of APIs vs finished drugs, and regulatory barriers that entrench incumbent suppliers. At 0.62, the constraint exceeds the tangled_rope lower bound (0.40) but remains below pure-snare territory (0.66+), reflecting that genuine coordination functions exist (supply reliability, quality standards) alongside extraction. Suppression (0.68): High and structural. Barriers to exit include regulatory gatekeeping (GMP facility approvals take 2-5 years), IP frameworks (process patents restrict alternative manufacturing routes), geographic constraints (API production requires specialized infrastructure), and political barriers (geopolitical tensions affect supply). These barriers are material and external — not just cognitive. Theater ratio (0.55): Moderate-high, reflecting that regulatory oversight has become increasingly performative. GMP compliance generates substantial administrative burden but provides diminishing marginal quality improvement; the primary effect is barrier maintenance. Theater has risen with regulatory complexity, suggesting that bureaucratic cover-story activity is increasing relative to functional verification. Claimed type (tangled_rope): Justified by the combination of genuine coordination function (global supply chain reliability, quality assurance, regulatory harmonization) AND asymmetric extraction (pricing power concentrated with API manufacturers, geographic bottleneck, regulatory barriers protecting incumbents). Mandatrophy resolved: At extractiveness 0.62, the constraint requires mandatrophy resolution. The resolution is: the constraint is legitimately both a coordination mechanism AND an extraction mechanism. The coordination is real (global supply networks are genuinely hard); the extraction is real (the power asymmetry is maintained by barriers that could theoretically be relaxed).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. Patient perspectives classify as Snare (χ ~ 0.85 at global scope); beneficiary perspectives classify as Rope (χ ~ -0.05 at global scope); observer perspective classifies as Tangled Rope (structural reality: both coordination and extraction coexist). The gap is not measurement error—it reflects that the constraint genuinely delivers coordination value to some agents while extracting from others. The analytical resolution: the constraint exists precisely because it solves a coordination problem that cannot be solved without creating extraction. Global supply chains ARE hard to coordinate. BUT the current coordination mechanism (geographic concentration, IP barriers, regulatory gatekeeping) is not the only possible one. Alternative mechanisms (distributed manufacturing, process patent exemptions, regulatory equivalence recognition) would reduce extraction while potentially degrading some coordination benefits. The constraint's persistence reveals path dependence: the current arrangement emerged from historical institutional choices, not from inevitability.
 *
 * DIRECTIONALITY LOGIC:
 *   Structural relationships determine d values independent of power level. Beneficiaries (major pharma, API manufacturers) have low d (~0.15-0.40) because the extraction flow runs toward them. Victims (patients, generic manufacturers) have high d (~0.85-0.95) because the extraction flow runs away from them. Exit options modulate d around these base values: arbitrage exits lower d further (beneficiaries become even more entrenched); trapped exits raise d further (victims become more completely victimized). The sigmoid converts d to f(d), which then scales base extractiveness ε. A powerless patient with trapped exit experiences χ = 0.62 × 1.42 × 1.2 ≈ 1.06 (effective extraction exceeds ε because their structural position amplifies it). A major pharmaceutical company with arbitrage exit experiences χ = 0.62 × (-0.12) × 1.2 ≈ -0.09 (negative effective extraction, i.e., they benefit). This directionality logic is not arbitrary—it flows from the structural data: who benefits, who bears costs, who can exit, and at what cost.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is resolved at ε = 0.62 by recognizing that the constraint is legitimately both coordination and extraction. The coordination function is real: global pharmaceutical supply chains ARE difficult to coordinate. Reliable, quality-assured API sourcing for 8 billion people requires standardized manufacturing, quality oversight, and supply reliability mechanisms. These are not trivial. The extraction mechanism is also real: the current coordination mechanism concentrates benefits (pricing power, capacity control, regulatory leverage) with specific actors (Asian manufacturers, major pharmaceutical companies) while concentrating costs (price volatility, supply uncertainty, regulatory barriers to entry) with others (patients, generic manufacturers). The constraint would not persist if it only extracted—it solves a genuine coordination problem. But it would not have this particular structure (geographic consolidation, IP protection, regulatory gatekeeping) if extraction were not also being generated and maintained. The mandatrophy resolution is: classify as Tangled Rope because both functions are present. The extraction is not incidental to coordination; it is structural to this particular coordination mechanism. Alternative mechanisms (e.g., decentralized API manufacturing with open-source process standards, regulatory equivalence recognition, compulsory licensing for generics) might solve coordination while reducing extraction, but they would have their own tradeoffs (potentially reduced quality oversight, reduced innovation incentives, geographic fragmentation risks).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    geographic_consolidation_inevitability,
    'Is API manufacturing consolidation in Asia an inevitable outcome of comparative advantage and economies of scale, or a contingent institutional arrangement maintained by regulatory and IP barriers?',
    'Counterfactual analysis: model what API manufacturing landscape would exist under alternative regulatory regimes (e.g., subsidized local manufacturing in developing nations, relaxed GMP equivalence standards, compulsory licensing for public health emergencies)',
    'If inevitable: constraint appears as natural economic law, reducing classification to Mountain. If contingent: constraint is revealed as Tangled Rope maintained by enforcement — regulatory barriers and IP frameworks are the extraction mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geographic_consolidation_inevitability, empirical, 'Whether geographic consolidation is inevitable or institutionally maintained').

omega_variable(
    regulatory_gatekeeping_function,
    'Do GMP standards and facility approvals primarily serve quality assurance or primarily serve as barriers to entry for new manufacturers?',
    'Analysis of rejection rates by manufacturer origin, approval timeline variation, and correlation between regulatory stringency and market consolidation. Comparison with alternative quality-assurance mechanisms (e.g., third-party testing, process validation without facility pre-approval).',
    'If primarily quality: theater_ratio should be lower (0.25-0.35), and regulatory constraint is genuine coordination. If primarily gatekeeping: theater_ratio is accurate (0.55+), and regulatory constraint is enforcement mechanism for market exclusion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_gatekeeping_function, empirical, 'Whether GMP regulation serves quality assurance or entry barriers').

omega_variable(
    patient_vs_manufacturer_exit_symmetry,
    'Can patients and generic manufacturers realistically exercise exit options (use alternative drugs, establish local manufacturing) equivalent to those available to major pharmaceutical companies?',
    'Mapping of real exit costs: therapy switching costs vs supplier switching costs, regulatory pathway timelines for new manufacturers vs approval delays, price differentials between local and imported APIs.',
    'If symmetric: constraint may reduce to Rope (coordination with symmetric participant agency). If asymmetric: asymmetry justifies snare and tangled_rope classifications for victims; validates suppression score ≥0.60.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(patient_vs_manufacturer_exit_symmetry, empirical, 'Symmetry of exit option costs across agent types').

omega_variable(
    supply_disruption_causation,
    'Are API supply disruptions (COVID shortages, geopolitical tensions, environmental regulations) caused by structural scarcity or by the concentration of manufacturing capacity in politically/environmentally fragile regions?',
    'Historical analysis of disruption events: distinguish supply shocks from capacity choice. Modeling of what disruption frequency would be under alternative geographic distribution of manufacturing.',
    'If structural scarcity: suppression is inherent to pharmaceuticals (landscape shift toward Mountain). If capacity choice: suppression is maintained extractively — the constraint exists precisely to keep manufacturing concentrated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supply_disruption_causation, empirical, 'Whether supply disruptions are structural or capacity-choice driven').

omega_variable(
    intellectual_property_necessity,
    'Are global API sourcing patents (process patents, formulation patents) necessary incentives for innovation, or do they primarily entrench incumbent manufacturers and restrict competitive entry?',
    'Comparative analysis of innovation rates pre/post-patent in specific API classes. Identification of patents that prevent process innovation vs those that protect truly novel chemistry.',
    'If necessary incentives: IP barriers are legitimate coordination cost (lower extraction perception). If entrenchment: IP frameworks are extraction mechanism via regulatory lock-in.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intellectual_property_necessity, preference, 'Whether IP frameworks serve innovation or entrenchment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(pharmaceutical_active_ingredient_sourcing, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pharma_api_tr_t0, pharmaceutical_active_ingredient_sourcing, theater_ratio, 0, 0.35).
narrative_ontology:measurement(pharma_api_tr_t10, pharmaceutical_active_ingredient_sourcing, theater_ratio, 10, 0.45).
narrative_ontology:measurement(pharma_api_tr_t20, pharmaceutical_active_ingredient_sourcing, theater_ratio, 20, 0.55).
narrative_ontology:measurement(pharma_api_tr_t5, pharmaceutical_active_ingredient_sourcing, theater_ratio, 5, 0.4).

% Extraction over time
narrative_ontology:measurement(pharma_api_be_t0, pharmaceutical_active_ingredient_sourcing, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(pharma_api_be_t10, pharmaceutical_active_ingredient_sourcing, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(pharma_api_be_t20, pharmaceutical_active_ingredient_sourcing, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(pharma_api_be_t5, pharmaceutical_active_ingredient_sourcing, base_extractiveness, 5, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(pharmaceutical_active_ingredient_sourcing, resource_allocation).
narrative_ontology:affects_constraint(pharmaceutical_active_ingredient_sourcing, generic_drug_affordability).
narrative_ontology:affects_constraint(pharmaceutical_active_ingredient_sourcing, antimicrobial_resistance_externalities).
narrative_ontology:affects_constraint(pharmaceutical_active_ingredient_sourcing, pharmaceutical_supply_chain_resilience).

% DUAL FORMULATION NOTE:
% Pharmaceutical API sourcing decomposes into three structurally distinct constraints with different ε values: (1) API manufacturing consolidation (ε≈0.62, Tangled Rope—this story), driven by economies of scale and geographic comparative advantage; (2) Generic drug affordability (downstream, ε≈0.70, Snare—extractive pricing by API suppliers), driven by capacity control; (3) Supply chain resilience (downstream, ε≈0.50, Tangled Rope—genuine coordination need paired with geopolitical fragility). The API sourcing story affects both downstream constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(pharmaceutical_active_ingredient_sourcing, institutional, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
