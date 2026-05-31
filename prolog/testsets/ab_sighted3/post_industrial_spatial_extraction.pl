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
 *   The post-industrial spatial divergence between urban knowledge-economy
 *   hubs and rural/deindustrialized regions presents as an economic law:
 *   agglomeration economies, network effects, and human capital clustering
 *   appear to make geographic concentration of high-value economic activity
 *   inevitable. Workers in declining regions experience this as an immutable
 *   constraint — individual mobility is blocked by housing equity, family
 *   ties, and age; collective action cannot reverse structural trends; policy
 *   interventions face overwhelming economic headwinds. Urban knowledge
 *   workers experience the same structural forces as coordination benefits
 *   (access to talent, infrastructure, innovation spillovers) rather than as
 *   extraction. The constraint exhibits classic mountain signatures: high
 *   accessibility collapse (alternative spatial arrangements are difficult to
 *   imagine), low resistance (attempts to reverse concentration fail), and
 *   apparent natural emergence (no actor designed this outcome). However, the
 *   presence of identifiable beneficiaries (urban knowledge workers,
 *   metropolitan service sectors, tech clusters) triggers false summit
 *   evaluation. Is this a genuine economic law, or a naturalized
 *   institutional arrangement where intellectual property regimes, venture
 *   capital geography, university concentration, and zoning policy create the
 *   appearance of natural necessity? The omega variables document this
 *   irreducible uncertainty.
 *
 * KEY AGENTS:
 *   - Deindustrialized Worker: Primary victim (powerless/trapped) — bears full cost of spatial divergence with no exit options; experiences constraint as natural law
 *   - Rural Policy Advocate: Secondary victim (moderate/constrained) — has resources and organization but faces structural economic forces that appear immutable at policy-relevant timescales
 *   - Urban Knowledge Worker: Primary beneficiary (institutional/arbitrage) — captures agglomeration rents through higher wages, career opportunities, and innovation spillovers; experiences constraint as coordination rather than extraction
 *   - Metropolitan Service Sectors: Secondary beneficiary (institutional/arbitrage) — benefits from concentrated demand for urban services (housing, retail, professional services)
 *   - Tech Industry Clusters: Secondary beneficiary (institutional/arbitrage) — benefits from talent concentration, venture capital proximity, and network effects
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees structural economic forces but must distinguish genuine natural law from naturalized institutional arrangements
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

% PERSPECTIVE 1: DEINDUSTRIALIZED WORKER (MOUNTAIN) — Trapped in declining regions by family ties, housing equity lock-in, and age-related mobility barriers. Experiences spatial divergence as an immutable economic law: knowledge economy requires urban density, manufacturing jobs are gone forever, and individual action cannot reverse structural trends. The constraint appears as natural as gravity.
constraint_indexing:constraint_classification(post_industrial_spatial_extraction, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: RURAL POLICY ADVOCATE (MOUNTAIN) — Constrained by political economy realities: capital mobility, agglomeration economies, and network effects favor urban concentration. Sees spatial divergence as a structural feature of post-industrial capitalism that policy can only mitigate, not reverse. Even with resources and organization, the underlying economic forces appear immutable at generational timescales.
constraint_indexing:constraint_classification(post_industrial_spatial_extraction, mountain,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: URBAN KNOWLEDGE SECTOR (MOUNTAIN) — Benefits from agglomeration economies but experiences them as natural economic law, not as extraction. Network effects, talent clustering, and innovation spillovers appear as inevitable features of knowledge production. The beneficiary sees coordination (access to talent pools, infrastructure, markets) rather than extraction, but the structural position is that of a beneficiary of a constraint that appears immutable from all positions.
constraint_indexing:constraint_classification(post_industrial_spatial_extraction, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (MOUNTAIN) — From a universal civilizational perspective, spatial concentration of knowledge-intensive economic activity appears as a structural feature of post-industrial economies driven by agglomeration economies, network effects, and human capital clustering. The constraint exhibits mountain characteristics: high accessibility collapse (alternative spatial arrangements are difficult to imagine or implement), low resistance (attempts to reverse concentration face overwhelming economic headwinds), and apparent natural emergence (no single actor designed this outcome). However, the presence of identifiable beneficiaries triggers FSM evaluation — is this a genuine economic law or a naturalized institutional arrangement?
constraint_indexing:constraint_classification(post_industrial_spatial_extraction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

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
 *   Extractiveness (0.18): Very low. The constraint exhibits some extraction (urban-rural income divergence, declining rural public services, status asymmetry) but the base extraction is below the mountain threshold (0.25). The extraction that exists appears to emerge from structural economic forces (agglomeration economies, network effects) rather than from designed institutional arrangements. However, this appearance may itself be the false summit — institutional factors (IP regimes, VC geography, university concentration, zoning) may be constructing what appears as natural necessity. Suppression (0.03): Very low. No active enforcement mechanism prevents rural residents from relocating or prevents policy interventions. The barriers are economic (housing equity lock-in, moving costs, job search frictions) rather than coercive. The low suppression is consistent with mountain classification — the constraint binds through structural necessity, not through active suppression of alternatives. Theater ratio (0.15): Very low. Rural development policies exist but are not primarily performative — they represent genuine (if often ineffective) attempts to address spatial divergence. The low theater reflects that this is not a degraded institutional ritual but a structural economic phenomenon. Accessibility collapse (0.92): Very high. Alternative spatial arrangements (distributed knowledge production, rural innovation hubs, remote work at scale) are difficult to imagine or implement given current economic structures. Resistance (0.08): Very low. Attempts to reverse spatial concentration (rural development programs, tax incentives for relocation, infrastructure investment) face overwhelming economic headwinds and typically fail or produce only marginal effects.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates perspectival convergence rather than divergence — all four perspectives classify as mountain. The deindustrialized worker sees immutable economic law from a position of powerlessness. The rural policy advocate sees the same immutability despite having resources and organization. The urban knowledge worker sees natural coordination benefits rather than extraction but still experiences the underlying forces as immutable. The analytical observer sees structural economic forces that appear to operate as natural law. The convergence itself is diagnostic: when all perspectives agree on mountain classification despite different structural positions, the constraint is either a genuine natural law OR a successfully naturalized false summit. The FSM trigger (mountain with beneficiaries) routes this to investigation rather than accepting the convergence at face value. The omega variables document the empirical tests that would distinguish genuine law from naturalized construction.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality structure reveals the false summit dynamic. Urban knowledge workers are declared beneficiaries with arbitrage exit options — they can move between urban centers and capture agglomeration rents. The engine derives low d (≈0.05) → negative f(d) → negative effective extraction. They experience the constraint as coordination (access to opportunities) rather than extraction. Deindustrialized workers are victims with trapped exit options — housing equity lock-in, family ties, age barriers prevent mobility. The engine derives high d (≈0.95) → high f(d) → high effective extraction. They experience the constraint as pure extraction with no escape. Rural policy advocates are victims with constrained exit options — they have resources and organization but face structural barriers. The engine derives moderate-high d (≈0.65) → moderate f(d). The analytical observer uses canonical d for analytical power (≈0.73). All perspectives classify as mountain because the constraint's accessibility collapse and resistance metrics pass the mountain gates regardless of directionality. But the presence of beneficiaries triggers FSM evaluation — the engine will flag this as a potential false summit where genuine economic forces are conflated with contingent institutional arrangements.
 *
 * MANDATROPHY ANALYSIS:
 *   MOUNTAIN WITH BENEFICIARIES (FSM CANDIDATE): This constraint resolves the mandatrophy by explicitly modeling the false summit dynamic. The base metrics (ε=0.18, suppression=0.03, accessibility_collapse=0.92, resistance=0.08) pass all mountain gates. The constraint exhibits apparent natural emergence — no single actor designed urban-rural divergence. All perspectives classify as mountain. But the presence of identifiable beneficiaries (urban knowledge workers, tech clusters, metropolitan service sectors) triggers the false summit detector. The mandatrophy resolution is not 'this is definitely a mountain' or 'this is definitely extraction' but 'this exhibits mountain signatures AND has beneficiaries, which creates an irreducible ambiguity that must be routed to empirical investigation.' The omega variables specify the tests: Can agglomeration economies be decoupled from current institutional arrangements? Do urban workers actually benefit net of hidden costs? What policy intervention magnitude would reverse divergence? The framework's contribution is not resolving the ambiguity but making it explicit and specifying what evidence would resolve it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    agglomeration_necessity,
    'Are agglomeration economies for knowledge work a genuine economic law, or a contingent feature of current institutional arrangements (intellectual property regimes, venture capital geography, university concentration)?',
    'Historical analysis of distributed knowledge production (pre-industrial craft networks, Cold War dispersed research); contemporary experiments in remote work, distributed teams, and digital collaboration platforms; comparison of innovation productivity in concentrated vs distributed arrangements controlling for institutional factors',
    'If genuine law: mountain classification confirmed across all perspectives. If contingent: reclassification to tangled_rope (coordination function exists but asymmetric extraction is institutionally constructed, not naturally emergent).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(agglomeration_necessity, empirical, 'Whether agglomeration economies are natural law or institutional artifact').

omega_variable(
    beneficiary_identification_ambiguity,
    'Do urban knowledge workers genuinely benefit from spatial concentration, or do they bear hidden costs (housing prices, congestion, positional competition) that offset agglomeration gains?',
    'Comprehensive welfare analysis comparing urban knowledge workers'' real consumption, leisure time, housing quality, and subjective well-being to counterfactual distributed arrangements; analysis of whether agglomeration rents accrue to workers or to landowners and capital',
    'If workers are net beneficiaries: FSM trigger is valid (mountain with beneficiaries). If workers bear hidden costs: beneficiary set should be revised to landowners and capital, strengthening FSM case.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_identification_ambiguity, empirical, 'Whether declared beneficiaries actually benefit net of hidden costs').

omega_variable(
    policy_reversibility_threshold,
    'What magnitude of policy intervention would be required to reverse spatial divergence, and does that threshold distinguish natural law from constructed constraint?',
    'Comparative analysis of successful spatial redistribution policies (South Korea''s administrative capital relocation, China''s inland development zones, historical examples of planned industrial dispersion); estimation of fiscal and regulatory requirements; assessment of political feasibility',
    'If reversal requires only moderate policy intervention: mountain classification is false summit (constraint is mutable but politically difficult). If reversal requires intervention beyond any historical precedent: mountain classification confirmed (constraint is effectively immutable at policy-relevant timescales).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(policy_reversibility_threshold, empirical, 'Policy intervention threshold for reversing spatial concentration').

omega_variable(
    temporal_horizon_classification_gap,
    'Does the constraint''s apparent immutability at biographical timescales (workers cannot reverse trends within their lifetimes) constitute genuine mountain status, or does mutability at generational/civilizational timescales disqualify mountain classification?',
    'Framework clarification: does mountain classification require immutability at all time horizons, or only at the horizon relevant to the agent''s decision-making? Historical examples of constraints that appeared immutable at biographical scales but shifted at generational scales.',
    'If mountain requires universal immutability: this constraint may be rope or tangled_rope at longer horizons. If mountain is horizon-relative: classification stands but with explicit temporal qualification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(temporal_horizon_classification_gap, conceptual, 'Whether biographical immutability suffices for mountain classification').


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
% This constraint is a single structural phenomenon (post-industrial spatial divergence) rather than a decomposed family. The urban-rural divergence, knowledge-sector concentration, and public service decline are different observables of the same underlying constraint, not separate constraints with different epsilon values. All observables point to the same structural dynamic: agglomeration economies in knowledge production create geographic concentration of opportunity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
