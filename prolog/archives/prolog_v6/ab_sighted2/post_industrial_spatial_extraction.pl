% ============================================================================
% CONSTRAINT STORY: post_industrial_spatial_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-21
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_post_industrial_spatial_extraction, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: post_industrial_spatial_extraction
 *   human_readable: Post-Industrial Spatial Extraction and Urban-Rural Divergence
 *   domain: political_economy/comparative_politics/democratic_theory
 *
 * SUMMARY:
 *   Post-industrial spatial extraction describes the structural divergence
 *   between urban knowledge-economy hubs and rural/deindustrialized regions
 *   in advanced economies. The constraint is presented as a natural law of
 *   agglomeration economics — knowledge-intensive industries require density
 *   for productivity, and no policy can reverse this without destroying the
 *   gains. However, the presence of identifiable beneficiaries (urban
 *   knowledge workers, metropolitan service sectors, tech clusters) and the
 *   role of policy choices (zoning restrictions, infrastructure investment
 *   concentration, tax incentives for urban development) in amplifying
 *   spatial concentration reveal this as a false summit. The constraint
 *   exhibits mountain characteristics (low extractiveness, low suppression,
 *   high accessibility collapse) but fails the natural law test because
 *   policy interventions could reduce divergence without eliminating
 *   agglomeration benefits. The analytical observer risks naturalizing what
 *   is partly a constructed outcome. The constraint's theater_ratio (0.15) is
 *   low — spatial divergence is not performative; the economic and
 *   demographic shifts are real. The modest increase over time reflects
 *   growing rhetorical emphasis on 'place-based policy' and 'left-behind
 *   regions' without corresponding structural change.
 *
 * KEY AGENTS:
 *   - Deindustrialized Worker: Primary victim (powerless/trapped) — bears subjective status decline and service accessibility loss; geographic mobility blocked by housing costs and skill mismatch
 *   - Urban Knowledge Workers: Primary beneficiary (institutional/arbitrage) — capture wage premiums, amenity access, and career mobility from spatial concentration
 *   - Metropolitan Development Authority: Institutional beneficiary (institutional/arbitrage) — benefits from tax base concentration and infrastructure economies of scale
 *   - Rural Service Provider: Secondary victim (moderate/constrained) — faces genuine coordination challenges (sparse populations) but also asymmetric extraction (funding formulas favor density)
 *   - Remote Work Coalition: Organized agents (organized/mobile) — building alternative pathways (remote work infrastructure, digital service delivery) with sunset logic
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing policy-amplified concentration as immutable agglomeration economics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(post_industrial_spatial_extraction, 0.18).
domain_priors:suppression_score(post_industrial_spatial_extraction, 0.03).
domain_priors:theater_ratio(post_industrial_spatial_extraction, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(post_industrial_spatial_extraction, extractiveness, 0.18).
narrative_ontology:constraint_metric(post_industrial_spatial_extraction, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(post_industrial_spatial_extraction, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(post_industrial_spatial_extraction, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(post_industrial_spatial_extraction, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(post_industrial_spatial_extraction, mountain).
narrative_ontology:human_readable(post_industrial_spatial_extraction, "Post-Industrial Spatial Extraction and Urban-Rural Divergence").
narrative_ontology:topic_domain(post_industrial_spatial_extraction, "political_economy/comparative_politics/democratic_theory").

domain_priors:emerges_naturally(post_industrial_spatial_extraction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(post_industrial_spatial_extraction, urban_knowledge_workers).
narrative_ontology:constraint_beneficiary(post_industrial_spatial_extraction, metropolitan_service_sectors).
narrative_ontology:constraint_beneficiary(post_industrial_spatial_extraction, tech_industry_clusters).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEINDUSTRIALIZED WORKER (MOUNTAIN) — Experiences spatial divergence as an immutable economic law. Geographic mobility is structurally blocked by housing costs in opportunity-rich metros, family ties, and skill mismatch. The concentration of knowledge-economy jobs in urban hubs appears as inevitable as gravity — a natural outcome of technological change and agglomeration economics. No perception of agency or alternative.
constraint_indexing:constraint_classification(post_industrial_spatial_extraction, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: METROPOLITAN DEVELOPMENT AUTHORITY (ROPE) — Experiences spatial concentration as efficient coordination. Urban density enables knowledge spillovers, labor market thickness, and infrastructure economies of scale. The authority benefits from tax base concentration and sees its role as managing growth, not redistributing it. Agglomeration is solving the coordination problem of matching specialized skills to specialized jobs.
constraint_indexing:constraint_classification(post_industrial_spatial_extraction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: RURAL SERVICE PROVIDER (TANGLED ROPE) — Healthcare, education, and infrastructure providers in declining regions face genuine coordination challenges (sparse populations make service provision costly) but also bear asymmetric extraction (funding formulas favor density, talent drains to metros, federal policy prioritizes urban innovation hubs). Mixed experience: real coordination problems exist, but the distribution of costs is not neutral.
constraint_indexing:constraint_classification(post_industrial_spatial_extraction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER / AGGLOMERATION ECONOMICS VIEW (MOUNTAIN) — From a civilizational/global perspective, spatial concentration of knowledge-intensive industries is a structural feature of post-industrial economies. Agglomeration externalities (knowledge spillovers, thick labor markets, specialized input suppliers) create increasing returns to density that no policy can reverse without destroying the productivity gains. This perspective sees urban-rural divergence as an immutable consequence of technological change. However, the structural data contradicts this — the presence of identifiable beneficiaries and the role of policy choices (zoning, infrastructure investment, tax policy) in amplifying agglomeration effects reveal this as a false summit.
constraint_indexing:constraint_classification(post_industrial_spatial_extraction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 5: REMOTE WORK COALITION (SCAFFOLD) — Organized actors (remote-first companies, digital nomad advocacy groups, rural broadband initiatives) see spatial extraction as a temporary coordination failure with a technological sunset. Remote work infrastructure, digital service delivery, and distributed collaboration tools are creating alternative pathways that bypass the agglomeration requirement. The constraint's extraction mechanism loses force as knowledge work decouples from physical proximity. Estimated sunset: 15-25 years for norms and infrastructure to mature.
constraint_indexing:constraint_classification(post_industrial_spatial_extraction, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(post_industrial_spatial_extraction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(post_industrial_spatial_extraction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(post_industrial_spatial_extraction, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(post_industrial_spatial_extraction, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(post_industrial_spatial_extraction, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(post_industrial_spatial_extraction, ExtMetricName, E),
    domain_priors:suppression_score(post_industrial_spatial_extraction, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(post_industrial_spatial_extraction),
    narrative_ontology:constraint_metric(post_industrial_spatial_extraction, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(post_industrial_spatial_extraction, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(post_industrial_spatial_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Very low. The constraint extracts subjective status and service accessibility from rural regions, but absolute material conditions have not declined — the extraction is relative, not absolute. Urban knowledge workers capture wage premiums and amenity access, but much of this reflects genuine productivity gains from agglomeration, not pure rent extraction. The low value reflects that the divergence is partly efficient coordination (agglomeration externalities are real) and partly constructed (policy choices amplify concentration). Suppression (0.03): Very low. Geographic mobility is not legally restricted, and rural residents are not coerced into staying. The barriers are economic (housing costs, skill mismatch) and social (family ties, community attachment), not suppressive. Accessibility collapse (0.92): Very high. From the perspective of a trapped rural worker, the constraint appears immutable — agglomeration economics and technological change seem like forces of nature. Resistance (0.08): Very low. The constraint is not actively resisted because it is perceived as inevitable. Theater ratio (0.15): Low. The divergence is real, not performative. The modest theater reflects rhetorical emphasis on place-based policy without structural change.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural phenomenon — urban-rural economic divergence — appears as an immutable natural law (mountain) from the perspective of trapped rural workers and analytical observers committed to agglomeration economics, as efficient coordination (rope) from the perspective of metropolitan development authorities, as a temporary problem with a technological sunset (scaffold) from the perspective of remote work advocates, and as mixed coordination-extraction (tangled rope) from the perspective of rural service providers. The perspectival gap is diagnostic: the trapped agent sees no alternative; the beneficiary sees no problem; the organized agent sees a solvable coordination failure; the analytical observer risks naturalizing policy choices as economic laws. The false summit detector resolves this by identifying beneficiary presence and policy amplification as evidence that the constraint is not a genuine natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation chain produces distinct d values for each agent based on their structural relationship to the constraint. Urban knowledge workers are beneficiaries with arbitrage exit options — they can move between metro areas to capture wage premiums. The engine derives low d (≈0.05) → negative f(d) → negative or very low chi, reflecting that extraction runs toward them. Deindustrialized workers are victims with trapped exit options — housing costs and skill mismatch block geographic mobility. The engine derives high d (≈0.95) → high f(d) → high chi, reflecting maximum experienced extraction. Rural service providers are victims with constrained exit options — they face genuine coordination challenges but also asymmetric extraction from funding formulas and talent drains. The engine derives moderate d (≈0.65) → moderate f(d) → moderate chi. The remote work coalition has organized power and mobile exit options — they see the constraint as temporary and are building alternatives. The engine derives moderate d (≈0.55) → moderate f(d) → moderate chi, but the scaffold classification reflects the sunset logic rather than the chi value. The analytical observer uses the canonical fallback d for analytical power (≈0.73), but the mountain classification is perspectival — the false summit detector identifies beneficiary presence as evidence of constructed constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED: This constraint resolves the mandatrophy by showing that the mountain classification is perspectival and fails the false summit test. The analytical observer's mountain is a naturalization of policy-amplified concentration — agglomeration externalities are real, but zoning restrictions, infrastructure investment concentration, and tax incentives amplify spatial divergence beyond what pure agglomeration economics would produce. The beneficiary's rope is their genuine experience — urban density solves real coordination problems. The scaffold is a real structural feature — remote work technology is creating alternative pathways. The tangled rope is the moderate agent's mixed experience — genuine coordination challenges exist alongside asymmetric extraction. The constraint is not 'purely' any single type — it is a presheaf over observation sites, and the false summit detector identifies the analytical mountain as a naturalized construction rather than a genuine natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    agglomeration_necessity_threshold,
    'What fraction of knowledge-economy productivity gains are due to irreducible agglomeration externalities vs. policy-amplified concentration?',
    'Natural experiments from remote work adoption (2020-2025); productivity comparisons between distributed and co-located teams; analysis of which knowledge sectors show persistent productivity penalties from geographic dispersion',
    'If >70% irreducible: mountain classification confirmed — spatial divergence is a natural law of post-industrial economies. If <30% irreducible: false summit — policy choices (zoning, infrastructure, tax incentives) are the binding constraint, not agglomeration economics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(agglomeration_necessity_threshold, empirical, 'Fraction of productivity gains due to irreducible agglomeration vs. policy amplification').

omega_variable(
    beneficiary_identification_ambiguity,
    'Are urban knowledge workers beneficiaries of a constructed constraint (policy-amplified concentration) or simply participants in an efficient coordination mechanism (genuine agglomeration)?',
    'Counterfactual analysis: would knowledge workers'' productivity and wages be lower in a policy regime that reduced spatial concentration (e.g., remote work subsidies, rural broadband investment, land use reform)? If productivity is invariant to dispersion, beneficiary status is confirmed. If productivity requires density, the mountain classification holds.',
    'If beneficiaries are real: false summit confirmed — the constraint extracts from rural regions to subsidize urban concentration. If beneficiaries are artifacts of measurement: mountain classification holds — everyone is better off under agglomeration, and the divergence is Pareto-improving.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_identification_ambiguity, conceptual, 'Whether urban knowledge workers are beneficiaries or participants in efficient coordination').

omega_variable(
    remote_work_sunset_credibility,
    'Does remote work technology genuinely dissolve agglomeration requirements, or does it create new forms of spatial sorting (remote workers cluster in amenity-rich secondary cities, preserving urban-rural divergence)?',
    'Longitudinal tracking of remote worker location choices; analysis of whether remote work disperses knowledge workers geographically or simply shifts them from primary to secondary metros; measurement of rural broadband adoption and remote work uptake in deindustrialized regions',
    'If remote work disperses to rural areas: scaffold perspective confirmed — technological sunset is real. If remote work preserves metro clustering: scaffold is aspirational, and the constraint persists under new forms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remote_work_sunset_credibility, empirical, 'Whether remote work dissolves agglomeration or creates new spatial sorting').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(post_industrial_spatial_extraction, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spatial_extract_tr_t0, post_industrial_spatial_extraction, theater_ratio, 0, 0.1).
narrative_ontology:measurement(spatial_extract_tr_t15, post_industrial_spatial_extraction, theater_ratio, 15, 0.12).
narrative_ontology:measurement(spatial_extract_tr_t30, post_industrial_spatial_extraction, theater_ratio, 30, 0.15).

% Extraction over time
narrative_ontology:measurement(spatial_extract_be_t0, post_industrial_spatial_extraction, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(spatial_extract_be_t15, post_industrial_spatial_extraction, base_extractiveness, 15, 0.15).
narrative_ontology:measurement(spatial_extract_be_t30, post_industrial_spatial_extraction, base_extractiveness, 30, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(post_industrial_spatial_extraction, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is a standalone structural analysis of post-industrial spatial divergence. It does not decompose into multiple stories because the extractiveness value is stable across observables (urban-rural GDP divergence, knowledge-sector employment concentration, and public service accessibility all yield similar ε estimates). Future constraints on specific policy mechanisms (zoning, infrastructure investment, tax policy) would link here via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
