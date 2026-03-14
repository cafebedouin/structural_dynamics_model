% ============================================================================
% CONSTRAINT STORY: eu_free_movement_directive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eu_free_movement_directive, []).

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
 *   constraint_id: eu_free_movement_directive
 *   human_readable: EU Free Movement Directive: Labor Mobility and Welfare State Extraction
 *   domain: political_economy/immigration_policy
 *
 * SUMMARY:
 *   The EU Free Movement Directive (Directive 2004/38/EC and predecessors)
 *   formally guarantees EU citizens the right to reside, work, and access
 *   services across member states. It is a cornerstone of the EU's
 *   legitimating narrative: the freedom to move is presented as both a
 *   coordination mechanism (enabling efficient labor market matching) and a
 *   human right. However, the constraint exhibits a complex tangled
 *   structure: it genuinely coordinates high-skill professional mobility and
 *   multinational employer access to talent, but simultaneously extracts from
 *   low-skill native workers through wage compression, from origin-state
 *   labor markets through brain drain, and from destination-state welfare
 *   systems through fiscal burden. The directive's theater ratio has
 *   increased over its 20-year operational lifespan as the performative
 *   rhetoric ('freedom of movement') has increasingly diverged from the
 *   material reality: movement is formally free, but constrained by language
 *   barriers, credential non-recognition, housing market gatekeeping, and
 *   welfare access restrictions that the directive does not address. This gap
 *   between the formal right and the material barrier is the hallmark of a
 *   degraded constraint approaching piton status.
 *
 * KEY AGENTS:
 *   - Low-skill native workers: Primary victims (powerless/trapped) — wage compression, employment displacement, no exit options
 *   - High-skill professionals: Primary beneficiaries (institutional/arbitrage) — reduced labor market transaction costs, credential portability, arbitrage options
 *   - Multinational employers: Beneficiary (institutional/arbitrage) — labor arbitrage across wage differentials, talent access, exit options abundant
 *   - Destination-state welfare systems: Secondary victim (moderate/constrained) — fiscal burden of service provision without proportional tax contribution, constrained by treaty
 *   - Origin-state labor markets: Mixed victim-beneficiary (organized/constrained) — brain drain extraction offset by remittance coordination
 *   - EU supranational bureaucracy: Institutional maintainer (institutional/constrained) — enforcement through procedural persistence; constrained by political economy of deepening EU integration
 *   - Analytical observer: Risk of false summit (analytical/analytical) — naturalizing institutional barriers as structural immutability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_free_movement_directive, 0.38).
domain_priors:suppression_score(eu_free_movement_directive, 0.42).
domain_priors:theater_ratio(eu_free_movement_directive, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_free_movement_directive, extractiveness, 0.38).
narrative_ontology:constraint_metric(eu_free_movement_directive, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(eu_free_movement_directive, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_free_movement_directive, tangled_rope).
narrative_ontology:human_readable(eu_free_movement_directive, "EU Free Movement Directive: Labor Mobility and Welfare State Extraction").
narrative_ontology:topic_domain(eu_free_movement_directive, "political_economy/immigration_policy").

domain_priors:requires_active_enforcement(eu_free_movement_directive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_free_movement_directive, high_skill_workers).
narrative_ontology:constraint_beneficiary(eu_free_movement_directive, multinational_employers).
narrative_ontology:constraint_beneficiary(eu_free_movement_directive, service_sector_capitals).
narrative_ontology:constraint_beneficiary(eu_free_movement_directive, destination_state_economies).
narrative_ontology:constraint_victim(eu_free_movement_directive, low_skill_native_workers).
narrative_ontology:constraint_victim(eu_free_movement_directive, destination_state_welfare_systems).
narrative_ontology:constraint_victim(eu_free_movement_directive, origin_state_labor_markets).
narrative_ontology:constraint_victim(eu_free_movement_directive, local_housing_markets).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-SKILL NATIVE WORKER (SNARE) — Faces wage compression and employment displacement with no exit. Cannot relocate to higher-wage zones (immigration restrictions elsewhere), cannot acquire high-skill credentials (education access), cannot exit labor market entirely (welfare supports are below-subsistence). Experiences maximum extraction — suppression manifests as structural economic immobility despite formal labor market access.
constraint_indexing:constraint_classification(eu_free_movement_directive, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DESTINATION STATE WELFARE SYSTEM (TANGLED ROPE) — Coordinates labor market integration and social provision for diverse populations (genuine coordination function), but faces asymmetric costs when low-skill migration exceeds fiscal capacity. The system must service migrants' healthcare, education, and income support while receiving tax contributions below the per-capita cost. Constrained by treaty obligation to provide equal treatment; can exit only through political rupture (Brexit, Orban refusals). Mixed coordination and extraction.
constraint_indexing:constraint_classification(eu_free_movement_directive, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: HIGH-SKILL PROFESSIONAL (ROPE) — Experiences free movement as pure coordination: reduced transaction costs for finding employers across borders, standardized credential recognition, portable pension and benefits. Arbitrage options are abundant (can exit to higher-wage jurisdictions, can return home, can negotiate remote work). Net beneficiary — experiences constraint as enabling mechanism rather than extraction.
constraint_indexing:constraint_classification(eu_free_movement_directive, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: MULTINATIONAL EMPLOYER (ROPE) — Free movement solves coordination problem of accessing specialized talent across jurisdictions. Labor arbitrage (hiring low-cost workers from lower-wage member states) is the intended benefit, not hidden extraction. Can exit by relocating operations, by hiring from outside EU, by investing in automation. Net beneficiary experiencing constraint as coordination mechanism for talent distribution.
constraint_indexing:constraint_classification(eu_free_movement_directive, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 5: ORIGIN STATE LABOR MARKET (TANGLED ROPE) — Coordinates skilled worker mobility and remittance flows (genuine coordination function), but faces extraction through brain drain: highest-earning workers depart, reducing tax base and human capital stock. Constrained by supranational treaty; can exit only through EU withdrawal. Experiences both coordination (remittances, reverse migration) and extraction (permanent skill loss, younger cohorts departing).
constraint_indexing:constraint_classification(eu_free_movement_directive, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: SUPRANATIONAL EU BUREAUCRACY (PITON) — Free movement directive persists as a core EU identity marker and procedural requirement despite degraded function: the policy's original purpose (reducing transaction costs for skilled mobility in the 1990s) has been achieved; continued enforcement now maintains a performative commitment to 'freedom' while the real labor market coordination happens through multinational employer matching and housing market gatekeeping. Theater ratio reflects that 'free movement' legally exists but is heavily constrained by host-country welfare access rules, housing availability, and credential recognition friction. The directive survives through institutional inertia and symbolic attachment, not because it efficiently solves contemporary labor mobility problems.
constraint_indexing:constraint_classification(eu_free_movement_directive, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / STRUCTURAL IMMOBILITY VIEW (MOUNTAIN) — From a civilizational/universal perspective, free movement operates within immutable structural constraints: language barriers, credential non-recognition, housing markets with local network effects, and welfare systems designed around native citizenship. These constraints are features of how human societies organize social provision and belonging — not contingent policy choices. No directive can overcome them. However, this perspective risks naturalizing what is actually a contingent institutional arrangement: language barriers are permanent only if policy does not invest in multilingual education; credential recognition is blocked only if professional guilds maintain gatekeeping; housing markets constrain only if zoning and speculation are permitted. The mountain classification is diagnostically suspicious — likely a false summit.
constraint_indexing:constraint_classification(eu_free_movement_directive, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_free_movement_directive_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(eu_free_movement_directive, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(eu_free_movement_directive, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(eu_free_movement_directive, TR),
    TR >= 0.70.

:- end_tests(eu_free_movement_directive_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate, increasing over time. The directive's initial function (reducing transaction costs for skilled labor mobility) was achieved and contributed real coordination gains in the 1990s-2000s. However, as access has broadened and income migration has become more common, the constraint has accumulated extractive overlay. Low-skill migration creates wage competition effects not present when the directive targeted skilled professionals. The trajectory from 0.18 to 0.38 reflects this accumulation. Suppression (0.42): Moderate-high. Significant barriers to effective free movement exist: language requirements (undocumented but enforced through labor market), credential recognition gatekeeping by professional guilds, housing market exclusion through price and network effects, and welfare access restrictions (right to reside conditional on non-dependency). These barriers are not formally part of the directive but operate as suppression mechanisms in practice. Theater ratio (0.35): Moderate. The directive is substantively functional (not yet piton status) but increasingly performative: the formal right to move exceeds the material ability to move. The trajectory shows theater increasing as real barriers accumulate faster than the directive responds. Claimed type (Tangled Rope): Justified by the presence of genuine coordination (high-skill mobility, multinational hiring efficiency) alongside asymmetric extraction (wage effects on low-skill natives, welfare system burden, brain drain). The constraint requires active enforcement (supranational legal authority) and produces heterogeneous outcomes across agent positions.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates a perspectival chasm characteristic of asymmetric institutional arrangements. The beneficiary (high-skill professional, multinational employer) experiences Rope: their coordination problem is solved, their exit options abundant. The victim (low-skill native, welfare system) experiences Snare or Tangled Rope: mixing genuine access (the coordination achieved by the directive) with extraction (wage pressure, fiscal burden). The origin state experiences Tangled Rope: genuine benefit from remittance and return migration coordination, but extraction through permanent skill loss. The supranational bureaucracy experiences Piton: the directive persists through institutional attachment to the 'freedom of movement' identity, but the performative content (theater ratio) has increased as real barriers diverge from formal rights. The analytical observer risks seeing Mountain: language and cultural barriers appear immutable, making free movement structurally limited. But this naturalizes what is institutional: language barriers exist only if education policy does not require multilingualism; credential gatekeeping exists only if professional guilds maintain non-recognition; housing access is restricted only if zoning and speculation are permitted. The mountain perspective is a false summit — it mistakes contingent institutional barriers for structural immutability.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for each perspective is determined by the agent's structural position within the extraction flow. High-skill professionals derive d ≈ 0.15 (beneficiary + arbitrage exit → negative f(d) → effective extraction flows toward them). Low-skill natives derive d ≈ 0.85 (victim + trapped exit → high f(d) → they experience maximum χ). Destination-state welfare systems derive d ≈ 0.65 (victim + constrained exit → moderate-high f(d) → significant extraction). Multinational employers derive d ≈ 0.20 (beneficiary + arbitrage → low f(d)). Origin states derive d ≈ 0.60 (mixed victim-beneficiary through brain drain offset by remittance → moderate f(d)). The directionality decomposition reveals that the directive is not neutrally 'free' — it is structurally biased toward actors with arbitrage options (exit capacity) and against actors with trapped or constrained exits. The coordination function (high-skill labor matching) is real, but the asymmetric extraction is equally real for agents without exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by disaggregating the 'freedom of movement' label into its structural components. The label conflates six distinct claims: (1) low-skill labor mobility (empirically limited by wage effects, credential barriers, and welfare restrictions); (2) high-skill professional mobility (empirically enabled, coordination problem solved); (3) multinational employer labor arbitrage (empirically enabled, major benefit flow); (4) origin-state brain drain (empirically significant extraction); (5) destination-state welfare system fiscal burden (empirically significant, depending on skill composition); (6) supranational institutional legitimation (performative, increasing theater). No single type captures all six. The constraint IS tangled rope from the vantage of the destination-state welfare system: it coordinates labor market integration while extracting fiscal resources from native taxpayers. The constraint IS rope from the vantage of high-skill professionals: pure coordination. The constraint IS snare from the vantage of low-skill natives: extraction with suppressed exit. The constraint IS piton from the vantage of the EU bureaucracy: performative persistence despite degraded function. The mandatrophy is resolved by recognizing that 'free movement' is not a single constraint but a constraint family that the directive treats as unified. Decomposing into separate stories per agent perspective (high-skill labor mobility story ≈ Rope; low-skill labor story ≈ Snare; welfare system fiscal story ≈ Tangled Rope) would yield sharper analysis, but the lived institutional reality is a single undecomposed directive that agents at different positions experience as different types. This is the diagnostic signature of a constraint approaching degradation: the institution persists as one unit while the structural reality has fragmented into multiple incompatible effects.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    skill_selectivity_mechanism,
    'Does free movement produce wage depression through low-skill labor supply increase, or does skill selectivity dominate, making the constraint primarily high-skill-favoring coordination?',
    'Econometric decomposition of wage effects by skill quintile and migrant skill distribution; comparison of predicted vs observed wage impacts; cross-country variation in migrant skill composition relative to host-country skill demand',
    'If low-skill migration dominates: snare classification for low-skill natives justified, extractiveness rises to 0.52+. If high-skill selection dominates: rope classification correct, extractiveness falls to 0.22.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(skill_selectivity_mechanism, empirical, 'Whether migration selectivity is high-skill or low-skill').

omega_variable(
    welfare_magnet_causality,
    'Do migrants disproportionately target high-welfare member states because welfare policies attract them, or do they target any accessible labor market and subsequently discover welfare access constraints?',
    'Analysis of pre-migration destination selection patterns vs post-migration welfare receipt; comparison of migrant concentration in high-welfare vs high-wage regions; lagged policy variation to establish causality direction',
    'If welfare acts as attractor: suppression and extraction are intentional policy effects. If migrants are rationally responding to labor demand: welfare impact is incidental, and suppression is higher (migrants are trapped despite benefit access). Affects moral framing and classification stability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_magnet_causality, empirical, 'Welfare as attractor vs incidental impact mechanism').

omega_variable(
    language_credential_barrier_reducibility,
    'Are language and credential recognition barriers to mobility structural (inherent to how professional knowledge transfers across contexts) or institutional (contingent policy choices that could be reversed)?',
    'Comparative analysis of credential recognition frameworks; investment costs for multilingual service provision; pilot programs with streamlined credential recognition; labor market outcomes in jurisdictions with vs without reciprocal credential frameworks',
    'If structural: mountain perspective has merit; free movement is inherently limited. If institutional: barriers are contingent policy choices; mountain is false summit. Reshapes whether constraint is immutable or policy-contingent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(language_credential_barrier_reducibility, conceptual, 'Language/credential barriers as structural vs institutional').

omega_variable(
    remittance_extraction_direction,
    'Do remittance flows from destination to origin states compensate origin states for brain drain, or do they represent insufficient compensation (net extraction from origin)?',
    'Human capital accounting: compare remittance flows to estimated lifetime fiscal contribution of departed worker; analyze whether remaining cohorts experience wage increases (benefiting from departure) or labor market deterioration (suffering from skill loss); cross-sectional comparison of origin states with high vs low remittance-to-brain-drain ratios',
    'If remittances fully compensate: tangled rope classification correct (coordination + extraction mixed, net ambiguous). If remittances insufficient: origin states are snare victims, extractiveness rises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remittance_extraction_direction, empirical, 'Remittance compensation for brain drain adequacy').

omega_variable(
    housing_market_gatekeeping_attribution,
    'Does immigration pressure on housing markets materially reduce native access to affordable housing, or are housing costs driven by speculation, zoning restrictions, and investment flows independent of migration?',
    'Decompose housing price dynamics: isolate migration-driven demand from financial speculation, regulatory changes, and investment flows; cross-country variation in migration and housing inflation; temporal analysis of housing prices relative to migration inflows',
    'If migration is primary driver: suppression for low-skill natives includes housing barrier; extractiveness for welfare system includes housing externality. If speculation/zoning dominate: housing barrier is institutional and separable from free movement constraint. Affects whether free movement is root cause or convenient scapegoat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(housing_market_gatekeeping_attribution, empirical, 'Attribution of housing cost pressure to migration vs other factors').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_free_movement_directive, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eufmd_tr_t0, eu_free_movement_directive, theater_ratio, 0, 0.22).
narrative_ontology:measurement(eufmd_tr_t10, eu_free_movement_directive, theater_ratio, 10, 0.3).
narrative_ontology:measurement(eufmd_tr_t20, eu_free_movement_directive, theater_ratio, 20, 0.35).

% Extraction over time
narrative_ontology:measurement(eufmd_be_t0, eu_free_movement_directive, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(eufmd_be_t10, eu_free_movement_directive, base_extractiveness, 10, 0.28).
narrative_ontology:measurement(eufmd_be_t20, eu_free_movement_directive, base_extractiveness, 20, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_free_movement_directive, resource_allocation).
narrative_ontology:affects_constraint(eu_free_movement_directive, eu_welfare_state_fiscal_limits).
narrative_ontology:affects_constraint(eu_free_movement_directive, eu_credential_recognition_gatekeeping).
narrative_ontology:affects_constraint(eu_free_movement_directive, origin_state_brain_drain).
narrative_ontology:affects_constraint(eu_free_movement_directive, housing_market_exclusion_dynamics).

% DUAL FORMULATION NOTE:
% The Free Movement Directive decomposes into a family of structurally distinct constraints when examined through the ε-invariance principle. The high-skill labor mobility function (ε≈0.08, Rope) is empirically stable and well-established. The low-skill labor dynamics (ε≈0.52, Snare) represent the extractive mechanism on wage-earning natives. The welfare system fiscal impact (ε≈0.45, Tangled Rope) is contingent on destination-state welfare generosity and migrant skill composition. These three stories share the upstream EU formal law framework but have different observable mechanisms, different ε values, and different classification types. The directive unifies them institutionally; structural analysis decomposes them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(eu_free_movement_directive, moderate, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
