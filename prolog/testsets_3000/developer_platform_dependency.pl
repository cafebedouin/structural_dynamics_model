% ============================================================================
% CONSTRAINT STORY: developer_platform_dependency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_developer_platform_dependency, []).

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
 *   constraint_id: developer_platform_dependency
 *   human_readable: Developer Platform Dependency Lock-In
 *   domain: technology/platform_economics
 *
 * SUMMARY:
 *   Developer platform dependency represents the structural tension between
 *   the genuine coordination benefits of standardized APIs, shared
 *   toolchains, and network effects on one side, and the extraction mechanism
 *   of switching costs, API deprecation, pricing changes, and forced upgrades
 *   on the other. This constraint exhibits the defining signature of Tangled
 *   Rope: a real coordination function exists (developers genuinely benefit
 *   from SDK maturity, ecosystem size, and shared infrastructure) alongside
 *   real asymmetric extraction (platform operator captures value through
 *   lock-in, developers bear transition costs). The constraint's
 *   extractiveness (0.58) reflects that platform operators have captured
 *   increasing rents over the interval through API pricing models, forced
 *   version upgrades, and deprecation cycles. Theater ratio (0.48) indicates
 *   moderate performative content — much platform evolution is marketed as
 *   'developer empowerment' and 'ecosystem growth' while actually
 *   implementing stronger lock-in mechanisms. The constraint's suppression
 *   (0.65) reflects significant but not total barriers: developers face
 *   rewrite costs, client/user base switching costs, and career/reputation
 *   risks from platform transition, but exit is not impossible (open-source
 *   alternatives exist, cross-platform frameworks mature, standards
 *   stabilize). The constraint operates at global scope with differential
 *   impact: major platforms (iOS, Android, AWS, Salesforce) exhibit stronger
 *   lock-in; standards-based platforms (web, containers) exhibit weaker
 *   lock-in.
 *
 * KEY AGENTS:
 *   - Dependent Developers: Primary victims (powerless/trapped or moderate/constrained depending on exit options and business model) — face high rewrite costs, switching costs, and career risk from platform migration
 *   - Platform Operator: Primary beneficiary (institutional/arbitrage) — captures value through ecosystem network effects, API pricing, and switching costs. Enforces constraint through technical and contractual mechanisms.
 *   - Platform User Base: Secondary victim (powerless/trapped) — downstream effects of platform extraction: vendors increase prices, service quality decreases, vendor lock-in cascades to end users
 *   - Ecosystem Developers: Beneficiary subgroup (moderate/mobile or arbitrage) — early adopters and specialized developers benefit from first-mover advantage and ecosystem-specific expertise
 *   - Open Standards Coalition: Organized agents (organized/constrained) — open-source alternatives, cross-platform frameworks, standards bodies providing exit pathways. See constraint as temporary problem with sunset.
 *   - Legacy Developer Community: Institutional (institutional/arbitrage) — maintains platform-specific practices long after superior alternatives exist. Theater-driven through backward compatibility requirements.
 *   - Identity-Fused Specialists: Moderate agents with cognitive lock-in (moderate/identity_locked) — professional identity constituted through platform expertise; structural mobility exists but identity abandonment perceived as required for exit
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees mixed coordination-extraction hybrid
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(developer_platform_dependency, 0.58).
domain_priors:suppression_score(developer_platform_dependency, 0.65).
domain_priors:theater_ratio(developer_platform_dependency, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(developer_platform_dependency, extractiveness, 0.58).
narrative_ontology:constraint_metric(developer_platform_dependency, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(developer_platform_dependency, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(developer_platform_dependency, tangled_rope).
narrative_ontology:human_readable(developer_platform_dependency, "Developer Platform Dependency Lock-In").
narrative_ontology:topic_domain(developer_platform_dependency, "technology/platform_economics").

domain_priors:requires_active_enforcement(developer_platform_dependency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(developer_platform_dependency, platform_operator).
narrative_ontology:constraint_beneficiary(developer_platform_dependency, ecosystem_developers).
narrative_ontology:constraint_victim(developer_platform_dependency, dependent_developers).
narrative_ontology:constraint_victim(developer_platform_dependency, platform_user_base).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Developer with significant codebase built on proprietary platform APIs faces rewrite costs, client switching costs, and platform-enforced upgrade cycles. Cannot migrate without business interruption. Platform controls API evolution, pricing, and feature availability.
constraint_indexing:constraint_classification(developer_platform_dependency, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% Developer building on platform toolchain benefits from SDK maturity, documentation, and ecosystem network effects. Also constrained by platform policy changes, revenue share terms, and deprecation cycles. Mixed coordination (shared tools) and extraction (enforced dependency).
constraint_indexing:constraint_classification(developer_platform_dependency, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Platform operator coordinates developer ecosystem through standardized APIs, shared infrastructure, and network effects. Experiences constraint as coordination problem solved by their service. Benefits from lock-in through service lock and switching costs.
constraint_indexing:constraint_classification(developer_platform_dependency, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% Open-source alternatives, standards bodies, and multi-platform development frameworks (React Native, Flutter, cross-platform SDKs) provide exit pathways. These alternatives have sunset implications: as standards mature and developer tooling standardizes across platforms, dependency on any single platform decreases.
constraint_indexing:constraint_classification(developer_platform_dependency, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% Developers maintaining legacy applications on deprecated platforms continue platform-specific practices long after superior alternatives exist. Theater of backward compatibility and compatibility layers masks atrophied core functionality. Constraint persists through institutional inertia rather than genuine necessity.
constraint_indexing:constraint_classification(developer_platform_dependency, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Developer whose professional identity and career trajectory are constituted through deep platform expertise (iOS/Android, AWS, Salesforce, etc.). Has structural mobility (could learn alternative platforms) but cannot exercise it because identity abandonment would be required. Sees constraint as both coordination necessity and identity anchor.
constraint_indexing:constraint_classification(developer_platform_dependency, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(global))).

% The constraint exhibits genuine coordination (shared infrastructure, standardized APIs, ecosystem benefits) alongside genuine asymmetric extraction (lock-in through switching costs, API deprecation, pricing changes, forced upgrades). Engine derives tangled_rope from structural data: beneficiaries exist (platform operator, some developers), victims exist (locked-in developers, downstream users), and active enforcement exists (API enforcement, terms of service, technical lock-in mechanisms).
constraint_indexing:constraint_classification(developer_platform_dependency, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(developer_platform_dependency_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(developer_platform_dependency, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(developer_platform_dependency, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(developer_platform_dependency, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(developer_platform_dependency, TR),
    TR >= 0.70.

:- end_tests(developer_platform_dependency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and rising. Initial extractiveness (0.35) reflects coordination-heavy ecosystem with modest switching costs. Over 7 years, extractiveness increases to 0.58 as platforms implement stronger lock-in: API deprecation cycles force upgrades, new features concentrate on proprietary extensions (reducing open-source alternative viability), pricing models shift from platform-subsidized SDKs to premium API costs. The trajectory reflects cumulative rent-seeking layered onto coordination. Suppression (0.65): Significant. Developers face three suppression mechanisms: (1) Rewrite costs: rebuilding large codebase on alternative platform consumes 30-60% of development costs. (2) Switching costs: moving installed user base, maintaining dual platforms during migration, retraining teams. (3) Career/reputation costs: deep platform expertise becomes career liability on alternative platforms; developer brand becomes entangled with platform reputation. Suppression is not total because open-source alternatives and standards-based platforms do reduce barriers, but barriers remain substantial. Theater ratio (0.48): Moderate. Platform vendors market lock-in mechanisms as developer-empowering features. API deprecation is framed as 'modernization'; pricing increases are framed as 'sustainability'; forced version upgrades are framed as 'security maintenance'. The marketing narrative obscures extraction mechanism. Theater has increased slightly over interval as extraction has become more obvious and requires more rhetorical cover.
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap is between the platform operator's genuine coordination function and the dependent developer's extraction experience. Both are structurally correct: the platform does coordinate, and the platform does extract. The gap reveals that Tangled Rope is the accurate classification — the constraint is neither pure coordination (Rope) nor pure extraction (Snare), but hybrid. A secondary gap appears between identity-locked and other moderate-power agents: the identity-locked specialist perceives immobility despite structural mobility, while other moderates perceive constrained but real options. This gap indicates that identity fusion is the binding mechanism, not structural barriers. A tertiary gap appears between the scaffold (sunset logic) and piton (theatrical persistence) perspectives on legacy systems: the same technical tools are seen as transitional (open standards) or perpetual (backward compatibility theater). This gap reveals institutional inertia — the theater persists not because functionality requires it but because exit costs for large installed bases remain substantial even as alternatives improve.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform operators (institutional/arbitrage) derive d ≈ 0.12: clear beneficiary with flexibility to change terms, so f(d) ≈ -0.08, producing negative effective extraction (the constraint subsidizes the operator). Dependent developers (powerless/trapped) derive d ≈ 0.95: clear victims with no exit, so f(d) ≈ 1.42, producing high effective extraction. Moderate developers (moderate/constrained) derive d ≈ 0.58: mixed position with some benefits and some costs, so f(d) ≈ 0.85, producing moderate effective extraction. Organized alternatives (organized/constrained) derive d ≈ 0.45: credible exit pathway exists, so f(d) ≈ 0.45, producing dampened effective extraction. Identity-locked specialists (moderate/identity_locked) derive d structurally as moderate-agent d ≈ 0.58, but perceive extraction higher because they cannot exercise exit options despite theoretical availability. The gap between structural d (0.58) and perceived extraction (closer to d ≈ 0.75) instantiates the identity lock mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   The developer platform dependency resolves mandatrophy by showing that Tangled Rope is the structurally correct classification from the analytical perspective. The key evidence: (1) Beneficiary group exists (platform operator, ecosystem developers with first-mover advantage); (2) Victim group exists (dependent developers, downstream users); (3) Active enforcement exists (API deprecation cycles, forced upgrades, terms of service, technical lock-in); (4) Genuine coordination function exists (shared infrastructure, SDK maturity, ecosystem effects); (5) Genuine extraction exists (switching costs, rent concentration, reduced alternatives). All five gates for Tangled Rope are met. False summit detection: the platform operator's Rope perspective appears to naturalize lock-in as an inherent feature of coordination. The structural data contradicts this — lock-in is a choice (enforced via API policies, pricing, deprecation) not a necessity. If platforms competed purely on coordination value without lock-in mechanisms, developer retention would decline, confirming that extraction is the retained mechanism. The identity-locked perspective reveals cognitive capture at the individual developer level — the constraint has internalized itself through professional identity, making exit feel impossible despite structural mobility. This is a diagnostic signature of Tangled Rope: coordination mechanisms become identity-anchoring mechanisms become extraction mechanisms.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_lock_in_boundary,
    'At what point does shared infrastructure coordination become extractive lock-in? Is the same technical mechanism (standardized API, proprietary SDK) coordination or constraint depending on exit options?',
    'Compare developer switching costs across platforms with equivalent technical maturity. Measure: rewrite cost ratio (new platform / existing platform build time), time-to-productivity ratio, ecosystem size differential. If switching costs exceed 50% of ongoing development cost and exceed alternatives by 2x, classify as lock-in rather than coordination.',
    'If coordination dominates: constraints across platforms converge toward Rope. If lock-in dominates: platform dependency is Snare. If mixed: Tangled Rope confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_vs_lock_in_boundary, empirical, 'Boundary between coordination and extractive lock-in in platform dependency').

omega_variable(
    identity_lock_persistence,
    'Is identity-locked developer constraint cognitively persistent or structurally contingent? If platform alternatives offered equivalent career path and compensation, would professional identity remain fused to platform?',
    'Longitudinal study of developers transitioning between platforms during major ecosystem shifts (e.g., iOS to cross-platform, Windows to cloud-native). Measure identity self-description changes over 2-3 year transition period. Survey post-transition: degree to which professional identity remains platform-anchored.',
    'If identity-locked persists post-transition: binding mechanism is primarily cognitive (identity fusion confirmed). If identity shifts with platform adoption: binding mechanism is primarily structural (career incentives). Determines whether exit requires identity death or merely cost absorption.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_persistence, empirical, 'Whether identity lock in platform expertise persists across platform transitions').

omega_variable(
    ecosystem_network_effect_counterfactual,
    'How much of the platform''s value to developers derives from genuine coordination (toolchain maturity, ecosystem size, documentation) versus from lock-in (switching costs, API exclusivity)? If switching costs were zero, how many developers would remain?',
    'Experimental approach: measure developer satisfaction/retention across platform cohorts with varying switching costs (low-switching platforms like web frameworks vs high-switching platforms like mobile SDKs). Cross-platform comparison of developer churn rates when high-quality alternatives emerge.',
    'If retention >> ecosystem quality: lock-in is primary driver (Snare). If retention ~= ecosystem quality: coordination is primary driver (Rope). If mixed: Tangled Rope confirmed with specific extraction proportion measurable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ecosystem_network_effect_counterfactual, empirical, 'Platform value derived from coordination versus lock-in').

omega_variable(
    suppression_structural_vs_behavioral,
    'Is measured suppression (0.65) driven by structural barriers (rewrite costs, economic switching costs) or by behavioral capture (developer beliefs about platform necessity, internalized dependency)?',
    'Post-exit analysis: developers who successfully migrated to alternatives and their suppression trajectories. Do developers report decreased suppression post-migration? If suppression persists after structural barriers removed, it indicates internalization. If suppression drops immediately, it indicates structural origin.',
    'If primarily structural: escape is possible at acceptable cost for some agents (constrained exit appropriate). If primarily internalized: escape perceived as impossible despite structural mobility (identity_locked appropriate). Mixed origins would suggest decomposition into separate constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_behavioral, empirical, 'Whether suppression is structural or internalized/behavioral').

omega_variable(
    platform_operator_genuine_coordination,
    'Does the platform operator provide genuine coordination services that would be difficult to replicate, or is the coordination function secondary to the extraction mechanism? If the platform ceased enforcing lock-in, would coordination value remain?',
    'Comparative analysis: open-source alternatives and their developer retention/satisfaction. Measure: cost of equivalent functionality via open-source toolchain versus proprietary platform. Survey developers: which platform features are coordination (valued) versus enforcement (resented).',
    'If coordination is genuine and substantial: platform operator''s Rope perspective is correct, extractiveness overstated. If enforcement is primary: coordination is pretext, constraint should be reclassified as Snare from operator perspective. If balanced: Tangled Rope confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(platform_operator_genuine_coordination, empirical, 'Whether platform operator provides genuine coordination or primary extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(developer_platform_dependency, 0, 7).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(devplat_tr_t0, developer_platform_dependency, theater_ratio, 0, 0.32).
narrative_ontology:measurement(devplat_tr_t3, developer_platform_dependency, theater_ratio, 3, 0.4).
narrative_ontology:measurement(devplat_tr_t7, developer_platform_dependency, theater_ratio, 7, 0.48).

% Extraction over time
narrative_ontology:measurement(devplat_be_t0, developer_platform_dependency, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(devplat_be_t3, developer_platform_dependency, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(devplat_be_t7, developer_platform_dependency, base_extractiveness, 7, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(developer_platform_dependency, resource_allocation).
narrative_ontology:boltzmann_floor_override(developer_platform_dependency, 0.18).
narrative_ontology:affects_constraint(developer_platform_dependency, vendor_lock_in_general).
narrative_ontology:affects_constraint(developer_platform_dependency, api_ecosystem_switching_costs).

% DUAL FORMULATION NOTE:
% Developer platform dependency decomposes into separate constraints along observable boundaries. SDK switching costs (ε ≈ 0.58, Tangled Rope) is distinct from ecosystem lock-in at the application level (ε ≈ 0.72, Snare). Platform technical decisions (API deprecation, pricing) have different extraction mechanisms than developer career path lock-in (identity_locked). Consider decomposing into: (1) platform_api_lock_in (ε ≈ 0.58), (2) developer_identity_lock_in (ε ≈ 0.62), (3) ecosystem_ecosystem_switching (ε ≈ 0.65). Current story uses integrated view; family decomposition may reveal distinct constraint structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(developer_platform_dependency, organized, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
