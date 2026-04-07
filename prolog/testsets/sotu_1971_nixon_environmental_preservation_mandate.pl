% ============================================================================
% CONSTRAINT STORY: sotu_1971_nixon_environmental_preservation_mandate
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1971_nixon_environmental_preservation_mandate, []).

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
 *   constraint_id: sotu_1971_nixon_environmental_preservation_mandate
 *   human_readable: Nixon 1971 Environmental Preservation Mandate (Federal Parks and Regulatory Framework)
 *   domain: infrastructure/environmental_policy
 *
 * SUMMARY:
 *   The Nixon 1971 environmental preservation mandate represents a watershed
 *   moment in federal regulatory scope: the systematic internalization of
 *   environmental externalities (air, water, noise pollution; resource
 *   preservation; recreational access) as a federal constraint on industrial
 *   and development activity. The constraint simultaneously coordinates
 *   legitimate inter-generational preservation interests and extracts costs
 *   from extractive industries and development capital through regulatory
 *   compliance, locked-out resource zones, and suppressed economic
 *   activities. Environmental constituencies and future generations benefit
 *   from locked-in access and pollution reduction. Extractive industries and
 *   development interests bear compliance costs and foregone resource
 *   extraction. Rural communities experience mixed effects: gaining
 *   recreation infrastructure and environmental health benefits while facing
 *   constraints on agricultural expansion and resource use. The constraint
 *   exhibits the full spectrum of DR classifications depending on observer
 *   position: it appears as coordination (rope) to environmental institutions
 *   that benefit from regulatory expansion; as mixed coordination and
 *   extraction (tangled rope) to rural communities gaining recreation
 *   infrastructure while facing use restrictions; as temporary scaffolding to
 *   future generations (scaffold) whose environmental norms will eventually
 *   make explicit regulation unnecessary; as degraded performative ritual
 *   (piton) to land management bureaucracies whose enforcement procedures
 *   accumulate while functional effectiveness declines against actual
 *   environmental pressures; as pure extraction (snare) to
 *   extractive-industry workers trapped in economically dependent communities
 *   with no biographical-scale exit; and as an immutable natural law
 *   (mountain) to the analytical observer at civilizational scope who sees
 *   environmental limits as non-negotiable — though this last perspective
 *   risks naturalizing the contingent regulatory architecture.
 *
 * KEY AGENTS:
 *   - Environmental Constituencies and Conservation Organizations: Primary beneficiary (institutional/arbitrage) — benefit from mandate expansion, budget growth, regulatory legitimacy, career pathways
 *   - Future Generations: Beneficiary (organized/mobile at civilizational scope) — locked-in preservation access; organized through environmental movement framing; mobile exit in long-term horizon through norm maturation (scaffold dynamic)
 *   - Extractive Industries and Development Capital: Primary victim (powerful/constrained) — bear regulatory compliance costs, locked-out resource zones; powerful enough to lobby but constrained by regulatory framework; mixed experience of extraction and coordination
 *   - Resource-Dependent Communities: Mixed (moderate/constrained at biographical scope) — gain recreation infrastructure and environmental health; constrained by agricultural expansion limits and water use restrictions; experience tangled rope dynamic
 *   - Extractive Industry Workers: Secondary victim (powerless/trapped) — trapped by foreclosed extraction pathways; no biographical-scale exit; full cost borne; structural suppression through economic dependency
 *   - Environmental Policy Establishment (EPA, state agencies, conservation NGOs): Beneficiary (institutional/arbitrage) — net beneficiary from regulatory expansion; experience constraint as coordination enabling long-term planning
 *   - Land Management Bureaucracy (NPS, BLM): Institutional maintainers (institutional/constrained) — maintain conservation mandate through routine enforcement; piton dynamic as theater increases relative to functional effectiveness
 *   - Public Health and Atmospheric Science Communities: Analytical beneficiary (analytical/analytical) — see constraint as legitimate coordination; clean air/water mandates internalize externalities enabling inter-generational efficiency
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1971_nixon_environmental_preservation_mandate, 0.48).
domain_priors:suppression_score(sotu_1971_nixon_environmental_preservation_mandate, 0.52).
domain_priors:theater_ratio(sotu_1971_nixon_environmental_preservation_mandate, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1971_nixon_environmental_preservation_mandate, extractiveness, 0.48).
narrative_ontology:constraint_metric(sotu_1971_nixon_environmental_preservation_mandate, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(sotu_1971_nixon_environmental_preservation_mandate, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1971_nixon_environmental_preservation_mandate, tangled_rope).
narrative_ontology:human_readable(sotu_1971_nixon_environmental_preservation_mandate, "Nixon 1971 Environmental Preservation Mandate (Federal Parks and Regulatory Framework)").
narrative_ontology:topic_domain(sotu_1971_nixon_environmental_preservation_mandate, "infrastructure/environmental_policy").

domain_priors:requires_active_enforcement(sotu_1971_nixon_environmental_preservation_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1971_nixon_environmental_preservation_mandate, environmental_constituencies).
narrative_ontology:constraint_beneficiary(sotu_1971_nixon_environmental_preservation_mandate, future_generations).
narrative_ontology:constraint_beneficiary(sotu_1971_nixon_environmental_preservation_mandate, rural_recreation_access).
narrative_ontology:constraint_beneficiary(sotu_1971_nixon_environmental_preservation_mandate, public_health_beneficiaries).
narrative_ontology:constraint_victim(sotu_1971_nixon_environmental_preservation_mandate, extractive_industries).
narrative_ontology:constraint_victim(sotu_1971_nixon_environmental_preservation_mandate, development_interests).
narrative_ontology:constraint_victim(sotu_1971_nixon_environmental_preservation_mandate, resource_dependent_communities).
narrative_ontology:constraint_victim(sotu_1971_nixon_environmental_preservation_mandate, compliance_burden_sectors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXTRACTIVE INDUSTRY WORKERS (SNARE) — Trapped by regulatory prohibition of prior extraction pathways. No exit from carbon-dependent economy exists within biographical horizon. Full cost borne; zero exit options. Suppression is structural: economic dependency on extractive employment with no local alternatives; regulatory prohibition forecloses historical income sources.
constraint_indexing:constraint_classification(sotu_1971_nixon_environmental_preservation_mandate, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INDUSTRIAL EXTRACTORS AND DEVELOPMENT CAPITAL (TANGLED ROPE) — Constrained by regulatory compliance costs and locked-out resource zones. Powerful agents can absorb costs and lobby for exemptions. Experience mixed extraction (forced compliance) and coordination (stable regulatory environment enables planning). High effective extraction chi due to powerful agent_power applied to constrained exit — can lobby but cannot escape the regulatory frame entirely.
constraint_indexing:constraint_classification(sotu_1971_nixon_environmental_preservation_mandate, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ENVIRONMENTAL POLICY ESTABLISHMENT (ROPE) — Net beneficiary with maximal arbitrage exit. Institutional actors (EPA, conservation NGOs, state environmental agencies) benefit from mandate expansion: budget growth, career pathways, political legitimacy. Experience the constraint as coordination: clearer environmental standards enable inter-jurisdictional alignment and long-term planning. Effective extraction chi is negative or near-zero (beneficiary + arbitrage).
constraint_indexing:constraint_classification(sotu_1971_nixon_environmental_preservation_mandate, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: RURAL RECREATION COMMUNITIES (TANGLED ROPE) — Moderate power, constrained exit. Benefit from expanded parks and recreation infrastructure; gain local jobs, tourism revenue, property values. Simultaneously constrained by environmental regulations that limit agricultural expansion, water use, and resource extraction. Mixed coordination (park access) and extraction (use restrictions). Moderate experienced extraction chi.
constraint_indexing:constraint_classification(sotu_1971_nixon_environmental_preservation_mandate, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: PUBLIC HEALTH AND ATMOSPHERIC SCIENCE (ROPE) — Analytical view at generational scope. Clean air and water mandates coordinate inter-generational benefit sharing. Suppression mechanisms (industrial compliance burden) are justified by health externality reduction. Constraint appears as legitimate coordination: internalizing externalities enables efficient long-term welfare gains. Effective extraction chi low — analytical perspective sees coordination function clearly.
constraint_indexing:constraint_classification(sotu_1971_nixon_environmental_preservation_mandate, rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 6: FUTURE GENERATIONS / CIVILIZATIONAL LEGACY (SCAFFOLD) — Organized through environmental movement and intergenerational stewardship frames. Benefit from locked-in preservation at civilizational scale. Temporary constraint structure: as environmental norms mature and circular-economy alternatives develop, the preservation mandate becomes less extractive (lower suppression, higher theater as enforcement becomes norm-based rather than coercive). Sunset implicit in the civilizational transition to sustainable systems — current coercion declines as future generations inherit norms as baseline.
constraint_indexing:constraint_classification(sotu_1971_nixon_environmental_preservation_mandate, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: LAND MANAGEMENT BUREAUCRACY (PITON) — Institutional actors (National Park Service, Bureau of Land Management) maintain conservation mandate through routine enforcement and maintenance operations. Over biographical time, theater_ratio increases as enforcement becomes ritualized: permit systems, environmental impact assessments, monitoring protocols persist as procedural requirements even as their functional capacity to prevent degradation declines relative to actual environmental pressures (climate change, invasive species, deferred maintenance). The constraint persists through institutional inertia — it is the default structure — but functional effectiveness degrades.
constraint_indexing:constraint_classification(sotu_1971_nixon_environmental_preservation_mandate, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From the civilizational/universal perspective, environmental preservation emerges as a natural law: biogeochemical cycles, ecosystem carrying capacity, and atmospheric composition constraints are not negotiable. The mandate appears as recognition of inherent biophysical limits rather than a constructed policy choice. However, this perspective risks a false summit: the specific regulatory architecture (park boundaries, compliance mechanisms, enforcement intensity) is contingent policy, while the underlying environmental necessity is genuine. The engine will flag this as naturalization.
constraint_indexing:constraint_classification(sotu_1971_nixon_environmental_preservation_mandate, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1971_nixon_environmental_preservation_mandate_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1971_nixon_environmental_preservation_mandate, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1971_nixon_environmental_preservation_mandate, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1971_nixon_environmental_preservation_mandate, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1971_nixon_environmental_preservation_mandate, TR),
    TR >= 0.70.

:- end_tests(sotu_1971_nixon_environmental_preservation_mandate_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The mandate extracts regulatory compliance costs and foregone extraction opportunities from powerful industrial actors and trapped extractive workers. However, the constraint is not maximal extraction — environmental constituencies also bear costs (monitoring, enforcement, adaptation), and the coordination function (inter-generational preservation, pollution reduction) is genuine. The measurement trajectory (0.35 → 0.48 over 10 years) reflects accumulating regulatory complexity as enforcement mechanisms elaborate and compliance costs compound. Suppression (0.52): Moderate-high. Structural barriers include regulatory prohibition (locked-out resource zones), economic dependency (extractive workers have no local alternatives), and compliance burden (industrial scale of environmental impact assessment and remediation). Suppression is not total — powerful industrial actors retain arbitrage options (geographic relocation, technological substitution, regulatory shopping); trapped workers face higher suppression. Theater ratio (0.58): Moderate. Environmental enforcement includes both functional elements (actual air/water quality measurement, monitoring) and performative elements (environmental impact statements, regulatory compliance documentation). Theater increases over the interval as procedures elaborate — permit systems, monitoring protocols, and assessment requirements become routinized. The constraint requires active enforcement (requires_active_enforcement: true) because the regulatory framework must continuously push against profit-maximizing behavior; without enforcement, extractive industries would revert to prior practices.
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces a classic perspectival wedge across the power and exit dimensions. Environmental institutions (institutional/arbitrage) experience pure coordination benefits — the mandate enables inter-jurisdictional alignment, career pathways, and budget growth. Powerful industrial actors (powerful/constrained) experience high extraction chi: they bear compliance costs and locked-out resources but have enough power to lobby for exemptions and enough capital to absorb costs, creating a mixed experience. Extractive workers (powerless/trapped) experience maximal extraction — they are structurally locked into economically dependent communities with no exit, forced to bear the full biographical cost of foreclosed extraction pathways. Rural communities (moderate/constrained) experience tangled rope: they gain recreation infrastructure and health benefits (coordination function) while facing constraints on resource use (extraction function). The scaffold perspective emerges at civilizational time horizons where environmental norms will mature and explicit coercion can decline. The piton perspective emerges at institutional scale where enforcement procedures accumulate while functional effectiveness degrades. The mountain perspective risks naturalizing the contingent regulatory architecture as an inherent environmental limit — the engine's false summit detection will flag this.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from beneficiary/victim declarations and exit options. Environmental institutions benefit from the mandate (low d, negative chi) and have arbitrage-level exit options, producing strong beneficiary positioning. Powerful industrial extractors are victims of regulatory constraint (high d) but have constrained rather than trapped exit, allowing partial arbitrage capacity — their chi is high but not maximal. Extractive workers are trapped victims (highest d) with no exit options — they experience maximum chi. Rural communities are both partial beneficiaries (recreation infrastructure) and partial victims (use constraints), producing moderate d and moderate chi. Future generations are beneficiaries locked in (low d) but experience the constraint as temporary (scaffold) because environmental norm maturation will reduce explicit enforcement — their effective extraction chi declines over civilizational time. The false summit in the mountain perspective derives from d = 0.72 (analytical perspective applied to universal scope) which produces high chi despite the mountain type claim — the engine's directionality derivation chain reveals that the analytical observer's natural-law framing is actually observing a structured constraint with identifiable beneficiaries and victims, contradicting the mountain claim.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy here resolves through the recognition that the constraint is genuinely both coordination and extraction simultaneously across different agent positions. The environmental mandate coordinates legitimate inter-generational preservation interests (air/water quality, species preservation, recreational access) while extracting from agents whose prior economic models depended on unpriced pollution and unrestricted resource access. Neither type (coordination-only rope nor pure extraction snare) is complete. The tangled rope classification is correct because the constraint has (1) a genuine coordination function (inter-generational environmental standards enable planning), (2) asymmetric extraction (compliance costs and locked-out resources flow from industries/workers to environmental constituencies), and (3) requires active enforcement (the regulatory framework must continuously constrain profit-maximizing behavior). The mandatrophy is resolved by recognizing that coordination and extraction are not mutually exclusive — legitimate coordination often requires extractive enforcement against agents who would otherwise externalize costs. The perspectival gap between snare (victim workers) and rope (beneficiary institutions) is not a contradiction but a structural feature: the same mechanism coordinates benefits for some agents by extracting from others.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    compliance_burden_distribution,
    'What proportion of aggregate regulatory compliance burden is distributed across major extractive sectors versus small operators? Is suppression symmetric or asymmetrically concentrated?',
    'Empirical analysis of compliance cost data by sector size and revenue scale; identification of regulatory burden distribution curves (Lorenz analysis for environmental compliance)',
    'If burden is symmetric (small operators bear proportional costs): snare classification for dependent communities is justified. If burden is concentrated on large capital (which can absorb it): tangled rope classification shifts toward rope for some extractive agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_burden_distribution, empirical, 'Distribution of environmental compliance burden across economic sectors').

omega_variable(
    enforcement_versus_norm_internalization,
    'To what extent does environmental preservation compliance derive from external enforcement (regulatory threat) versus internalized norms (genuine environmental commitment)?',
    'Comparative analysis of compliance rates in high-enforcement jurisdictions versus low-enforcement jurisdictions; measurement of voluntary vs mandated compliance; survey data on industry environmental values',
    'If primarily norm-driven: suppression values are overstated, and more perspectives classify as rope (coordination). If primarily enforcement-driven: snare and piton classifications confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_versus_norm_internalization, empirical, 'Whether compliance is externally enforced or norm-internalized').

omega_variable(
    resource_dependent_community_exit,
    'Did extractive-dependent communities face genuine ''no exit'' constraints (trapped) or high-cost exit (constrained) through the 1971–1990 interval?',
    'Historical analysis of migration patterns, economic diversification, retraining programs, employment trajectories in resource-dependent regions; identification of actual vs perceived exit barriers',
    'If trapped: snare classification is correct; suppression ≥ 0.60. If constrained: tangled rope is more accurate; suppression moderates toward 0.45–0.55.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(resource_dependent_community_exit, empirical, 'Whether resource-dependent communities faced trapped or constrained exit').

omega_variable(
    preservation_mandate_internalization,
    'Did the 1971 environmental mandate shift public values toward genuine environmental stewardship (identity_locked beneficiary perspective) or remain externally enforced regulation?',
    'Longitudinal tracking of public opinion data on environmental protection; measurement of voluntary versus mandated compliance; analysis of environmental movement growth and institutionalization',
    'If internalized: identity-locked perspectives appear; future generations see preservation as intrinsic value rather than imposed constraint. If imposed: snare and piton perspectives dominate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(preservation_mandate_internalization, conceptual, 'Whether preservation mandate became internalized environmental value').

omega_variable(
    industrial_arbitrage_capacity,
    'Could extractive industries arbitrage environmental regulations through geographic relocation, regulatory shopping, or technological substitution? How much constrainment is real versus apparent?',
    'Empirical tracking of industrial relocation patterns post-1971; measurement of regulatory arbitrage (moving extraction offshore); analysis of technological substitution capacity (e.g., alternative energy adoption rates)',
    'If high arbitrage capacity: constrained classification is correct; industries retained exit options. If low arbitrage capacity: snare classification for industries becomes more accurate; suppression rises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(industrial_arbitrage_capacity, empirical, 'Capacity for extractive industries to arbitrage environmental regulations').

omega_variable(
    park_access_equity,
    'Did expanded federal park infrastructure genuinely provide equitable access to environmental recreation across income and geographic strata, or did benefits concentrate on mobile/wealthy populations?',
    'Empirical analysis of park visitor demographics; measurement of access costs (transportation, equipment, time); analysis of park distribution relative to population density and income',
    'If equitable: beneficiary classification for rural communities confirmed. If concentrated: benefits flow primarily to mobile/wealthy agents; rural communities bear regulation costs without proportional recreation benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(park_access_equity, empirical, 'Whether park expansion provided equitable recreation access across populations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1971_nixon_environmental_preservation_mandate, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nixon_env_theater_t0, sotu_1971_nixon_environmental_preservation_mandate, theater_ratio, 0, 0.48).
narrative_ontology:measurement(nixon_env_theater_t5, sotu_1971_nixon_environmental_preservation_mandate, theater_ratio, 5, 0.54).
narrative_ontology:measurement(nixon_env_theater_t10, sotu_1971_nixon_environmental_preservation_mandate, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(nixon_env_extract_t0, sotu_1971_nixon_environmental_preservation_mandate, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(nixon_env_extract_t5, sotu_1971_nixon_environmental_preservation_mandate, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(nixon_env_extract_t10, sotu_1971_nixon_environmental_preservation_mandate, base_extractiveness, 10, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1971_nixon_environmental_preservation_mandate, resource_allocation).
narrative_ontology:boltzmann_floor_override(sotu_1971_nixon_environmental_preservation_mandate, 0.18).
narrative_ontology:affects_constraint(sotu_1971_nixon_environmental_preservation_mandate, interstate_pollution_commons_tragedy).
narrative_ontology:affects_constraint(sotu_1971_nixon_environmental_preservation_mandate, agricultural_water_rights_reallocation).
narrative_ontology:affects_constraint(sotu_1971_nixon_environmental_preservation_mandate, industrial_compliance_cost_burden).
narrative_ontology:affects_constraint(sotu_1971_nixon_environmental_preservation_mandate, rural_economic_transition_path_dependency).

% DUAL FORMULATION NOTE:
% The Nixon environmental mandate decomposes into four distinct constraints along observability lines: pollution regulation (affects interstate commons), water resource allocation (affects agricultural/industrial sectors), compliance burden distribution (affects different industrial scales asymmetrically), and rural economic transition (affects employment and community viability). Each constraint has its own epsilon value and structural beneficiary/victim profile. The mandate is the institutional umbrella linking them. See network.affects_constraints for constraint family members.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1971_nixon_environmental_preservation_mandate, powerful, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
