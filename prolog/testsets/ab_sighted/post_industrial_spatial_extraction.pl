% ============================================================================
% CONSTRAINT STORY: post_industrial_spatial_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-15
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
 *   human_readable: Post-Industrial Spatial Extraction
 *   domain: political_economy/comparative_politics/democratic_theory
 *
 * SUMMARY:
 *   Post-industrial spatial extraction describes the concentration of wealth,
 *   opportunity, and knowledge-sector employment in metropolitan areas,
 *   leaving rural and deindustrialized regions with declining subjective
 *   status despite non-declining absolute material conditions. The constraint
 *   is framed as a mountain — an immutable feature of knowledge economies —
 *   but this classification is contested. The structural data shows
 *   identifiable beneficiaries (urban knowledge workers, metropolitan service
 *   sectors, tech clusters) who gain from agglomeration economies, which
 *   triggers the false summit detection mechanism. The constraint may be a
 *   genuine natural law (agglomeration is inherent to information-intensive
 *   production) or a policy-amplified extraction mechanism naturalized
 *   through economic geography discourse. The omega variables document this
 *   irreducible uncertainty.
 *
 * KEY AGENTS:
 *   - Deindustrialized Worker: Primary potential victim (powerless/trapped) — experiences spatial divergence as immutable; blocked from geographic mobility by housing costs and skill mismatch
 *   - Rural Professional: Secondary potential victim (moderate/constrained) — could relocate at high personal cost but perceives agglomeration as natural economic law
 *   - Urban Knowledge Workers: Primary beneficiary (institutional/arbitrage) — capture agglomeration wage premiums and network access; may also bear costs via housing inflation
 *   - Metropolitan Service Sectors: Secondary beneficiary (institutional/arbitrage) — benefit from dense consumer base and business services demand
 *   - Tech Industry Clusters: Tertiary beneficiary (institutional/arbitrage) — self-reinforcing concentration of talent, capital, and innovation infrastructure
 *   - Economic Geographer: Analytical observer (analytical/analytical) — sees agglomeration as universal feature of post-industrial economies
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(post_industrial_spatial_extraction, 0.08).
domain_priors:suppression_score(post_industrial_spatial_extraction, 0.03).
domain_priors:theater_ratio(post_industrial_spatial_extraction, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(post_industrial_spatial_extraction, extractiveness, 0.08).
narrative_ontology:constraint_metric(post_industrial_spatial_extraction, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(post_industrial_spatial_extraction, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(post_industrial_spatial_extraction, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(post_industrial_spatial_extraction, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(post_industrial_spatial_extraction, mountain).
narrative_ontology:human_readable(post_industrial_spatial_extraction, "Post-Industrial Spatial Extraction").
narrative_ontology:topic_domain(post_industrial_spatial_extraction, "political_economy/comparative_politics/democratic_theory").

domain_priors:emerges_naturally(post_industrial_spatial_extraction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(post_industrial_spatial_extraction, urban_knowledge_workers).
narrative_ontology:constraint_beneficiary(post_industrial_spatial_extraction, metropolitan_service_sectors).
narrative_ontology:constraint_beneficiary(post_industrial_spatial_extraction, tech_industry_clusters).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEINDUSTRIALIZED WORKER (MOUNTAIN) — Experiences spatial concentration as an immutable economic law. Geographic mobility is structurally blocked by housing costs in opportunity-rich metros, family ties, and skill mismatch. The divergence appears as natural as gravity — knowledge work concentrates where knowledge workers already are, creating self-reinforcing agglomeration that no individual can resist.
constraint_indexing:constraint_classification(post_industrial_spatial_extraction, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: RURAL PROFESSIONAL (MOUNTAIN) — Sees spatial divergence as inevitable economic geography. Could relocate to metros at significant personal cost (family disruption, cultural dislocation, housing expense) but perceives the underlying agglomeration dynamic as unchangeable. The constraint is the economic logic itself, not the specific policy choices that amplify it.
constraint_indexing:constraint_classification(post_industrial_spatial_extraction, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: METROPOLITAN DEVELOPMENT AUTHORITY (MOUNTAIN) — Benefits from agglomeration economies but perceives them as natural economic forces. Can arbitrage between metros (relocate investment, shift development priorities) but sees the concentration dynamic itself as an immutable feature of post-industrial economies. The authority administers the flow but does not create the gradient.
constraint_indexing:constraint_classification(post_industrial_spatial_extraction, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (MOUNTAIN) — Observes spatial concentration as a structural feature of knowledge economies globally. Agglomeration effects, network externalities, and human capital clustering create self-reinforcing dynamics that appear across all advanced economies regardless of policy regime. The pattern is as universal as comparative advantage — a feature of how information-intensive production organizes in space.
constraint_indexing:constraint_classification(post_industrial_spatial_extraction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(post_industrial_spatial_extraction_tests).

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
 *   Extractiveness (0.08): Very low, at the boundary of mountain classification. The constraint extracts primarily through relative status decline and political marginalization rather than absolute material deprivation. Rural regions have not experienced absolute GDP decline in most cases — the extraction is in the divergence, not in rural immiseration. This low extractiveness is what makes the mountain classification plausible as a natural law claim. Suppression (0.03): Very low. Geographic mobility is constrained by housing costs and skill mismatch, but these are not active enforcement mechanisms — they are structural barriers that emerge from the agglomeration dynamic itself. No institution actively prevents rural-to-urban migration; the barriers are economic, not coercive. Accessibility collapse (0.92): Very high. Alternative spatial arrangements (dispersed knowledge work, rural tech hubs, remote work at scale) are extremely difficult to access. The agglomeration dynamic creates strong path dependence — once concentration begins, network effects and human capital clustering make reversal nearly impossible. Resistance (0.08): Very low. Attempts to reverse spatial concentration through policy (rural development programs, tax incentives for dispersed employment, infrastructure investment in declining regions) have shown minimal success. The constraint resists intervention, which is consistent with natural law classification. Theater ratio (0.15): Low. Most spatial policy interventions are genuine attempts at redistribution, not performative gestures. The low theater ratio reflects that policymakers actually try to address spatial inequality — they fail because the constraint resists, not because the efforts are insincere.
 *
 * PERSPECTIVAL GAP:
 *   All four perspectives classify as mountain, which is unusual — most constraints show perspectival divergence. The uniformity reflects that spatial concentration appears immutable from every structural position: the trapped worker cannot escape it, the constrained professional sees it as inevitable, the beneficiary institution administers it but does not create it, and the analytical observer sees it as a universal feature of post-industrial economies. This uniform mountain classification is exactly what makes the false summit hypothesis compelling. If the constraint is genuinely a natural law, the uniform classification is correct. If it is a naturalized extraction mechanism, the uniform classification reveals how effectively the naturalization has worked — even the victims perceive it as immutable. The false summit detector does not resolve this ambiguity; it flags it for investigation.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations (urban knowledge workers, metropolitan service sectors, tech clusters) trigger the false summit detection mechanism despite the mountain classification. The engine will compute directionality values showing that these agents experience low or negative effective extraction (they benefit from the constraint), while rural/deindustrialized agents experience moderate extraction (relative status decline, political marginalization). The perspectival gap between the analytical mountain classification and the structural beneficiary/victim pattern is the diagnostic signal: if this is a genuine natural law, why are there identifiable winners? The omega variables document three pathways to resolution: (1) agglomeration is truly immutable and the beneficiaries are incidental (mountain confirmed), (2) policy choices amplify natural agglomeration and could be altered (reclassify to tangled_rope), or (3) the entire framing naturalizes what is actually constructed extraction (reclassify to snare with property owners as primary beneficiaries).
 *
 * MANDATROPHY ANALYSIS:
 *   MOUNTAIN WITH BENEFICIARIES (FALSE SUMMIT CANDIDATE): This constraint resolves the mandatrophy by explicitly modeling the natural-law-vs-constructed ambiguity through the false summit detection mechanism. The mountain classification is not wrong — it accurately captures how the constraint is experienced and perceived across all structural positions. But the presence of identifiable beneficiaries (urban knowledge workers, metro service sectors, tech clusters) creates a structural anomaly: genuine natural laws do not have winners and losers in this way. The three omega variables document the irreducible uncertainties that prevent definitive classification: Is agglomeration truly immutable or policy-amplified? Is status decline perceptual or institutionally real? Do knowledge workers genuinely benefit or are they also victims of housing inflation? The constraint's classification depends on which of these empirical questions resolves in which direction. The framework does not pre-adjudicate — it documents the uncertainty and provides the measurement criteria for future resolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    agglomeration_inevitability,
    'Are knowledge-economy agglomeration dynamics truly immutable, or are they amplified by contingent policy choices (zoning, infrastructure investment, tax policy) that could be altered?',
    'Cross-national comparison of spatial inequality trajectories under different policy regimes; historical analysis of periods when spatial divergence reversed or stabilized; identification of policy interventions that successfully dispersed knowledge-sector employment',
    'If truly immutable: mountain classification confirmed across all perspectives. If policy-contingent: reclassify to tangled_rope or snare, with identifiable beneficiaries (metro property owners, knowledge workers) and victims (rural communities, deindustrialized regions).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(agglomeration_inevitability, empirical, 'Whether agglomeration is natural law or policy-amplified extraction').

omega_variable(
    subjective_status_mechanism,
    'Is the subjective status decline in rural/deindustrialized regions a psychological response to relative position, or does it reflect real erosion of political voice, cultural representation, and institutional access?',
    'Longitudinal analysis of political representation, media coverage, and institutional responsiveness in declining regions; comparison of subjective well-being trajectories controlling for absolute economic conditions',
    'If purely psychological: the constraint is a perception problem, not structural extraction. If institutionally grounded: the constraint extracts real political and cultural capital, not just relative status.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(subjective_status_mechanism, empirical, 'Whether status decline is perceptual or institutionally real').

omega_variable(
    beneficiary_identification,
    'Do urban knowledge workers and metropolitan service sectors genuinely benefit from spatial concentration, or are they also victims of housing cost inflation, congestion, and quality-of-life degradation?',
    'Welfare analysis comparing real purchasing power, leisure time, and life satisfaction across spatial contexts; identification of who captures the agglomeration rent (property owners vs workers)',
    'If workers are net beneficiaries: beneficiary declaration is accurate. If property owners capture most gains: reclassify workers as victims and property owners as primary beneficiaries, potentially shifting from mountain to tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_identification, empirical, 'Who actually captures agglomeration benefits').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(post_industrial_spatial_extraction, 0, 0).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(post_industrial_spatial_extraction, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is a single structural claim about spatial concentration in post-industrial economies. It does not decompose into multiple observables with different epsilon values — the urban-rural divergence, knowledge-sector concentration, and public service decline are all measurements of the same underlying agglomeration dynamic.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
