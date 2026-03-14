% ============================================================================
% CONSTRAINT STORY: developer_migration_flows
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_developer_migration_flows, []).

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
 *   constraint_id: developer_migration_flows
 *   human_readable: Developer Migration Flows in Global Software Ecosystems
 *   domain: labor/technology/economic
 *
 * SUMMARY:
 *   Developer migration flows represent a global labor market constraint that
 *   exhibits structural extraction alongside genuine coordination benefits.
 *   The constraint emerges from the intersection of: (1) differential wage
 *   structures between developed and developing nations, (2) artificial
 *   scarcity created by visa and credential restrictions, (3) platform
 *   control over labor market access and intellectual property, and (4) the
 *   strategic benefits of cross-border talent distribution. From the emerging
 *   market developer's perspective, this is a snare — trapped in a lower-wage
 *   labor market while skills are extracted through remote work arrangements
 *   and IP flows to developed-nation platforms. From the developing nation's
 *   perspective, it is tangled rope — they experience both the coordination
 *   benefit of diaspora networks and knowledge transfer, and the asymmetric
 *   extraction of top talent. From the platform's perspective, it is pure
 *   coordination (rope) — the platform captures value from global labor
 *   distribution without experiencing constraints. The constraint's
 *   theater_ratio has declined (0.62 → 0.48) as remote work and digital nomad
 *   visas have reduced bureaucratic performance requirements, but
 *   extractiveness has increased as platform control over labor markets has
 *   concentrated.
 *
 * KEY AGENTS:
 *   - Emerging Market Developers: Primary victims (powerless/trapped) — face wage suppression, visa constraints, and credential gatekeeping; extract surplus flows to developed-nation platforms
 *   - Developing Nation Tech Sectors: Secondary victim/beneficiary (moderate/constrained) — lose talent through brain drain but gain through diaspora networks and return-of-talent initiatives; cannot easily exit the constraint
 *   - Technology Platforms (GAFAM, unicorns): Primary beneficiary (institutional/arbitrage) — access low-cost skilled labor in emerging markets, acquire top talent in developed nations, capture value through platform control
 *   - Developed Nation Tech Employers: Secondary beneficiary (institutional/arbitrage) — access immigrant developer talent and outsourced labor; maintain wage premiums through credential gatekeeping
 *   - Developer Collective Organizing: Emerging organized force (organized/constrained) — building unions, cooperatives, and cross-border solidarity to establish portable benefits and standardized contracting
 *   - Immigration/Visa Regulatory System: Institutional actor (institutional/arbitrage) — creates performative constraint through visa categories and credential requirements; enables extraction through artificial scarcity
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements (wage differentials, credential systems, platform monopolies) as immutable market forces
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(developer_migration_flows, 0.52).
domain_priors:suppression_score(developer_migration_flows, 0.58).
domain_priors:theater_ratio(developer_migration_flows, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(developer_migration_flows, extractiveness, 0.52).
narrative_ontology:constraint_metric(developer_migration_flows, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(developer_migration_flows, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(developer_migration_flows, tangled_rope).
narrative_ontology:human_readable(developer_migration_flows, "Developer Migration Flows in Global Software Ecosystems").
narrative_ontology:topic_domain(developer_migration_flows, "labor/technology/economic").

domain_priors:requires_active_enforcement(developer_migration_flows).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(developer_migration_flows, technology_platforms).
narrative_ontology:constraint_beneficiary(developer_migration_flows, developed_nation_tech_employers).
narrative_ontology:constraint_victim(developer_migration_flows, emerging_market_developers).
narrative_ontology:constraint_victim(developer_migration_flows, developing_nation_tech_labor_markets).
narrative_ontology:constraint_victim(developer_migration_flows, open_source_contributors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EMERGING MARKET DEVELOPER (SNARE) — Structurally trapped by visa constraints, credential requirements, and concentration of high-wage opportunities in developed nations. Faces maximum extraction: must remain in lower-wage labor market while their skills are extracted through remote work, outsourcing arrangements, and intellectual property flows. No viable exit from the constraint itself — the developer can migrate geographically but the structural asymmetry persists.
constraint_indexing:constraint_classification(developer_migration_flows, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DEVELOPING NATION TECH SECTOR (TANGLED ROPE) — Experiences genuine coordination benefit from developer migration flows: knowledge transfers, capital flows, diaspora networks, and return-of-talent pathways build local tech capacity. Simultaneously bears asymmetric extraction: loses top talent, faces brain drain, and cannot capture value from developer output that flows through global platforms. Constrained exit — the nation-state cannot easily block developer migration without damaging economic development strategy, yet accepts the extraction as cost of coordination.
constraint_indexing:constraint_classification(developer_migration_flows, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: TECHNOLOGY PLATFORM (ROPE) — Net beneficiary experiencing migration flows as pure coordination mechanism. Platforms extract surplus from developer talent distribution globally: accessing low-cost skilled labor in emerging markets, acquiring top talent from developed nations, and capturing value from both through platform control. Arbitrage exit — platforms have complete flexibility to shift hiring, outsourcing, and IP ownership strategies. The constraint serves their interests and they perceive it as legitimate labor market coordination.
constraint_indexing:constraint_classification(developer_migration_flows, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DEVELOPER COLLECTIVE ORGANIZING (SCAFFOLD) — Emerging organized labor movements (tech unions, developer cooperatives, cross-border solidarity networks) see developer migration as a temporary coordination failure with a sunset. Collective bargaining, standardized contracting, and portable benefits (health, pension) tied to developer identity rather than employment location are building alternative pathways that reduce extraction. Sunset logic: as developer power consolidates through unionization and market tightening (demographic changes, AI capability boundaries), the structural asymmetry weakens. Estimated sunset: 15-25 years as developer scarcity increases and collective power matures.
constraint_indexing:constraint_classification(developer_migration_flows, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: IMMIGRATION/VISA REGULATORY SYSTEM (PITON) — Formal visa categories (H1-B, skilled migration programs, digital nomad visas) are substantially performative: they create appearance of controlled developer flows while actually enabling extraction by creating artificial scarcity and restricting competition. Regulatory theater persists through institutional inertia despite mounting evidence that visa restrictions increase extraction rather than manage it. The system sees its own mechanism as degraded — revised repeatedly without addressing core asymmetry. Theater ratio high because visa procedure complexity creates performance without substance.
constraint_indexing:constraint_classification(developer_migration_flows, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, capital and labor flow toward higher returns in a competitive global market — differential wage structures and opportunity distribution are immutable properties of asymmetric development. This perspective sees developer migration as a natural equilibrium reflecting underlying resource constraints and human capital distribution. However, the structural data contradicts the mountain classification: the artificial scarcity (visa restrictions), credential gatekeeping, and platform monopolies are contingent institutional arrangements, not laws of nature. The engine will compute this as a false summit.
constraint_indexing:constraint_classification(developer_migration_flows, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(developer_migration_flows_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(developer_migration_flows, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(developer_migration_flows, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(developer_migration_flows, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(developer_migration_flows, TR),
    TR >= 0.70.

:- end_tests(developer_migration_flows_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): The constraint extracts substantial value through wage suppression, IP capture, and talent concentration, but this is not maximal extraction. Emerging market developers can earn significantly more than local alternatives through remote work, and platforms do distribute some value through scale effects. The extractiveness reflects the asymmetric power structure: platforms and developed-nation employers capture disproportionate surplus, but not through total coercion. Suppression (0.58): Significant but not total. Visa restrictions, credential gatekeeping, and platform control create real barriers, but developers retain agency — they can migrate, develop marketable skills, or organize collectively. Suppression is structural (external barriers) not internalized (cognitive capture). Theater ratio (0.48): Moderate. Visa categories and credential systems create performative bureaucracy, but remote work and digital nomad visas have reduced this theater. The constraint's core mechanism (wage asymmetry, platform control) functions without heavy performative overhead, suggesting the extraction is structural rather than maintained through theatrical justification.
 *
 * PERSPECTIVAL GAP:
 *   The snare perspective (emerging market developer) and the rope perspective (technology platform) experience the identical structural constraint with opposite sign in their experienced extraction. This gap reveals how asymmetric power transforms the same mechanism into extraction for one agent and coordination for another. The scaffold perspective (developer organizing) identifies a genuine sunset mechanism: as developer scarcity increases and collective power matures through unionization, the wage asymmetry that sustains extraction should decline. The piton perspective (visa system) observes that its own mechanisms have become substantially performative — visa procedures persist through institutional inertia despite evidence that they increase extraction rather than manage it. The mountain perspective risks naturalizing the entire structure as inevitable market equilibrium, but the other five perspectives reveal it as contingent on visa restrictions, credential gatekeeping, and platform monopsony — all modifiable institutional arrangements.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's experienced extractiveness (χ) is computed from their power level, exit options, beneficiary/victim status, and the structural d value. Emerging market developers face high d (~0.88) through trapped exit + victim status → high f(d) → high χ relative to base ε. Technology platforms face low d (~0.12) through arbitrage exit + beneficiary status → low/negative f(d) → negative χ (they experience subsidy). Developing nations face moderate d (~0.58) through constrained exit + mixed beneficiary/victim status → moderate f(d). The piton classification derives from the theater gate, not from high experienced extraction — the immigration system's apparatus is performative (theater_ratio = 0.62 at t=0), even though the underlying constraint is extractive. As remote work reduces procedural theater, the theater_ratio has declined while extractiveness has increased, revealing that the extraction mechanism is structural, not dependent on bureaucratic performance.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that tangled rope is the correct classification at the systems level (developer migration as a whole exhibits both coordination and asymmetric extraction), while snare emerges for individuals with maximum powerlessness and rope for platforms with maximum institutional power. The mandatrophy is not 'which type is correct?' but 'at what scale and for which agent?' The developing nation's tangled rope classification is crucial: it shows that the constraint has genuine coordination benefits (diaspora networks, knowledge transfer, capital flows) that would be destroyed by attempting to eliminate it entirely. The platform's rope classification reveals that the extraction is not maintained through coercion but through control over market structure (access, credentials, IP). The developer collective's scaffold perspective identifies the structural change mechanism: as developer scarcity increases and collective power consolidates, the wage asymmetry sustains less extraction and the constraint moves toward genuine coordination. The analytical observer's mountain is a false summit: it naturalizes what are contingent institutional arrangements and obscures the specific mechanisms (visa policy, credential gatekeeping, platform monopsony) that could be reformed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    visa_restriction_causality,
    'Do visa restrictions genuinely constrain developer mobility or do they primarily concentrate extraction through artificial scarcity?',
    'Comparative analysis of developer outcomes under high vs. low visa restriction regimes; tracking of wage premiums and IP capture under different regulatory regimes; longitudinal studies of developer career trajectories with/without visa barriers',
    'If restrictions constrain mobility: extractiveness drops as constraint loosens (χ decreases). If restrictions enable extraction through scarcity: extractiveness increases as restrictions tighten. Classification consequence: if extraction is primary mechanism, suppression score increases and mountain classification becomes untenable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(visa_restriction_causality, empirical, 'Whether visa restrictions constrain or concentrate extraction').

omega_variable(
    remote_work_extraction_equivalence,
    'Does remote work enable emerging market developers to escape wage extraction or does it simply relocate the extraction mechanism into virtual labor markets?',
    'Comparative wage analysis: emerging market developers in remote roles vs. in-person roles vs. local employment; measurement of exit options expansion under remote work; tracking of platform control mechanisms in remote arrangements vs. traditional employment',
    'If remote work expands exit options: developer classification shifts from trapped to constrained; extractiveness may decrease. If remote work enables new extraction mechanisms (platform arbitrage, time-zone-based wage suppression): trapped status persists despite geographic mobility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remote_work_extraction_equivalence, empirical, 'Whether remote work changes developer exit options or relocates extraction').

omega_variable(
    open_source_contribution_extraction,
    'Is unpaid open source contribution a coordination mechanism (developers voluntarily contributing to public goods) or an extraction mechanism (platforms capturing value from developer labor)?',
    'Developer motivation surveys; analysis of correlation between open source contribution and platform adoption; tracking of IP flows from open source to proprietary platforms; measurement of career advancement returns from open source contribution',
    'If coordination: open source is a genuine public good with low extraction. If extraction: open source is captured labor (theater_ratio increases, extractiveness increases, classification may shift toward snare from developer perspective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(open_source_contribution_extraction, empirical, 'Whether open source is coordination or captured labor').

omega_variable(
    developing_nation_brain_drain_threshold,
    'At what migration rate does developer emigration flip from beneficial (diaspora networks, knowledge transfer) to damaging (capacity collapse, market failure) for developing nation tech sectors?',
    'Cross-national analysis of migration rates vs. local tech sector vitality; measurement of knowledge transfer effectiveness at different brain drain thresholds; identification of critical transition points in country-level tech ecosystem data',
    'If threshold is high (>40% migration): tangled rope classification stable; nation-state can sustain coordination benefits despite asymmetric extraction. If threshold is low (<20%): tangled rope classification fails; snare emerges for nation-state as extraction overwhelms coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(developing_nation_brain_drain_threshold, empirical, 'Critical brain drain threshold for developing nation tech sectors').

omega_variable(
    platform_monopsony_binding,
    'Are major technology platforms operating as monopsonists in developer labor markets, and if so, does this constitute a separate constraint from migration flows?',
    'Market concentration analysis; measurement of developer bargaining power; wage compression studies; analysis of platform control over credential systems and career pathways',
    'If true monopsony: platform control is a structurally distinct constraint (separate story with high extractiveness). Developer migration constraint becomes secondary effect of monopsony, not primary extraction mechanism. If partial monopsony: migration constraint is tangled rope as stated; monopsony effects are internalized in χ computation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(platform_monopsony_binding, empirical, 'Whether platforms operate as labor market monopsonists').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(developer_migration_flows, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(devmig_tr_t0, developer_migration_flows, theater_ratio, 0, 0.62).
narrative_ontology:measurement(devmig_tr_t5, developer_migration_flows, theater_ratio, 5, 0.55).
narrative_ontology:measurement(devmig_tr_t10, developer_migration_flows, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(devmig_be_t0, developer_migration_flows, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(devmig_be_t5, developer_migration_flows, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(devmig_be_t10, developer_migration_flows, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(developer_migration_flows, resource_allocation).
narrative_ontology:affects_constraint(developer_migration_flows, technology_platform_monopsony).
narrative_ontology:affects_constraint(developer_migration_flows, visa_credential_gatekeeping).
narrative_ontology:affects_constraint(developer_migration_flows, remote_work_labor_markets).

% DUAL FORMULATION NOTE:
% Developer migration flows decompose into multiple structurally distinct constraints: (1) wage asymmetry across nations (this story, ε=0.52, tangled rope at systems level), (2) platform monopsony in labor markets (ε=0.68, snare), (3) visa/credential gatekeeping mechanisms (ε=0.58, tangled rope). Each story has its own beneficiaries and victims. This story links to the others through network.affects_constraints: migration flows are downstream of monopsony and gatekeeping mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(developer_migration_flows, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
