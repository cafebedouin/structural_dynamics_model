% ============================================================================
% CONSTRAINT STORY: post_industrial_spatial_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-03
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
 *   in advanced economies since approximately 1980. The constraint is
 *   characterized by wealth and opportunity concentration in metropolitan
 *   areas with high-skill service sectors, leaving rural and formerly
 *   industrial regions with declining subjective status despite non-declining
 *   (or slowly declining) absolute material conditions. The pattern is
 *   observable across OECD countries but varies in magnitude: the US shows
 *   extreme coastal/interior divergence, while Germany's distributed
 *   manufacturing model shows less spatial polarization. The constraint is
 *   presented as a natural law of post-industrial economics (agglomeration
 *   economies, increasing returns to scale, network effects) but benefits
 *   identifiable groups (urban knowledge workers, tech platforms,
 *   metropolitan service sectors) while leaving others structurally
 *   disadvantaged. This triggers false summit evaluation: is spatial
 *   divergence an immutable feature of knowledge economies, or a contingent
 *   outcome of policy choices (infrastructure investment, education access,
 *   zoning, tax incentives) that could be altered? The 2020-2023 remote work
 *   natural experiment provides a potential test case.
 *
 * KEY AGENTS:
 *   - Deindustrialized Worker: Primary trapped agent (powerless/trapped) — locked in declining regions by housing equity, family ties, age/skill barriers; experiences divergence as immutable economic law
 *   - Rural Professional: Secondary constrained agent (moderate/constrained) — teachers, nurses, local government workers face declining budgets and brain drain but cannot easily exit; sees structural economic reality beyond local control
 *   - Tech Platform Company: Primary beneficiary (institutional/arbitrage) — captures agglomeration economies, network effects, talent concentration; global arbitrage exit option
 *   - Urban Knowledge Worker: Secondary beneficiary (powerful/mobile) — software engineers, consultants, researchers benefit from wage premiums and career mobility; mobile exit across cities
 *   - Regional Development Coalition: Organized agents (organized/constrained) — regional governments, economic development agencies working to attract investment; see coordination problem with policy solutions
 *   - Analytical Observer: Civilizational view (analytical/analytical) — agglomeration economics perspective risks naturalizing policy-contingent outcomes as economic laws
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(post_industrial_spatial_extraction, 0.12).
domain_priors:suppression_score(post_industrial_spatial_extraction, 0.18).
domain_priors:theater_ratio(post_industrial_spatial_extraction, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(post_industrial_spatial_extraction, extractiveness, 0.12).
narrative_ontology:constraint_metric(post_industrial_spatial_extraction, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(post_industrial_spatial_extraction, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(post_industrial_spatial_extraction, accessibility_collapse, 0.05).
narrative_ontology:constraint_metric(post_industrial_spatial_extraction, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(post_industrial_spatial_extraction, mountain).
narrative_ontology:human_readable(post_industrial_spatial_extraction, "Post-Industrial Spatial Extraction and Urban-Rural Divergence").
narrative_ontology:topic_domain(post_industrial_spatial_extraction, "political_economy/comparative_politics/democratic_theory").

domain_priors:emerges_naturally(post_industrial_spatial_extraction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(post_industrial_spatial_extraction, urban_knowledge_workers).
narrative_ontology:constraint_beneficiary(post_industrial_spatial_extraction, metropolitan_service_sectors).
narrative_ontology:constraint_beneficiary(post_industrial_spatial_extraction, tech_platform_companies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEINDUSTRIALIZED WORKER (MOUNTAIN) — Trapped in declining regions by family ties, housing equity locked in depreciated property, and age/skill barriers to retraining. Experiences spatial divergence as an immutable economic law — the knowledge economy 'naturally' concentrates in cities, and manufacturing jobs are 'naturally' gone. No perception of alternatives at biographical timescale.
constraint_indexing:constraint_classification(post_industrial_spatial_extraction, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: RURAL PROFESSIONAL (MOUNTAIN) — Teachers, nurses, local government workers in rural areas face declining public service budgets and brain drain but cannot easily relocate due to professional licensing, family obligations, or preference for rural life. Sees spatial divergence as structural economic reality beyond local control. Constrained exit (could move at high personal cost) but still perceives immutability at biographical horizon.
constraint_indexing:constraint_classification(post_industrial_spatial_extraction, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: TECH PLATFORM COMPANY (ROPE) — Benefits from agglomeration economies, network effects, and talent concentration in urban hubs. Experiences spatial clustering as efficient coordination: co-location reduces transaction costs, enables knowledge spillovers, and accelerates innovation. Arbitrage exit option (can relocate operations globally to optimize for talent/regulation/tax). Sees the constraint as pure coordination with minimal extraction.
constraint_indexing:constraint_classification(post_industrial_spatial_extraction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: URBAN KNOWLEDGE WORKER (ROPE) — Software engineers, consultants, researchers benefit from wage premiums, career mobility, and amenity access in metropolitan areas. Mobile exit (can switch cities or negotiate remote work). Experiences spatial concentration as coordination: clustering enables job matching, skill development, and professional networks. Low experienced extraction — the system works for them.
constraint_indexing:constraint_classification(post_industrial_spatial_extraction, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / AGGLOMERATION ECONOMICS (MOUNTAIN) — From a civilizational/global perspective, knowledge-economy clustering in high-density urban areas reflects increasing returns to scale, network externalities, and human capital complementarities that are structural features of post-industrial production. Spatial divergence appears as an economic law analogous to comparative advantage or economies of scale. This perspective naturalizes the pattern as inevitable. However, the beneficiary declarations trigger FSM evaluation — the constraint benefits identifiable groups (urban knowledge workers, platform companies) while leaving others behind, suggesting the 'natural law' framing may obscure policy choices about infrastructure investment, education access, and regional development that shape where agglomeration occurs.
constraint_indexing:constraint_classification(post_industrial_spatial_extraction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 6: REGIONAL DEVELOPMENT COALITION (ROPE) — Organized actors (regional governments, economic development agencies, rural advocacy groups) working to attract investment and retain population see spatial divergence as a coordination problem with policy solutions: broadband infrastructure, remote work incentives, university satellite campuses, tax credits for rural hiring. Constrained exit (cannot abandon their regions) but organized enough to pursue alternatives. Experiences the constraint as coordination challenge rather than extraction or immutable law.
constraint_indexing:constraint_classification(post_industrial_spatial_extraction, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
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
 *   Extractiveness (0.12): Very low. The constraint exhibits minimal direct extraction — no single agent is actively extracting resources from rural regions. Instead, the pattern reflects differential access to agglomeration benefits. Urban knowledge workers and tech platforms benefit from clustering, but they are not extracting from rural areas in the way a snare extracts from trapped agents. The low epsilon reflects that much of the divergence is coordination (efficient clustering) rather than extraction. However, the epsilon is non-zero because policy choices (infrastructure spending, education subsidies, zoning) have systematically favored urban agglomerations, creating path-dependent advantages that compound over time. Suppression (0.18): Very low. Exit barriers exist (housing equity, family ties, retraining costs) but are not insurmountable for most agents. Many rural residents could relocate if they chose to bear the costs. The suppression is primarily economic (moving costs, wage gaps) rather than coercive. Theater ratio (0.15): Very low. Regional development efforts (tax incentives, infrastructure projects, rural broadband initiatives) have some performative elements but are not primarily theatrical. Most policy interventions have genuine (if limited) effects on local economies. The theater ratio reflects that some rural development programs persist through political necessity rather than demonstrated efficacy, but this is a minor component. Accessibility collapse (0.05) and Resistance (0.08): Very low, consistent with mountain classification. No agent can collapse access to agglomeration economies (they emerge from decentralized coordination), and no agent can effectively resist the clustering pattern (even large-scale rural development programs have limited impact on core divergence trends).
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates a classic false summit pattern. Urban knowledge workers and tech platforms experience spatial clustering as efficient coordination (Rope) — agglomeration enables knowledge spillovers, reduces transaction costs, and accelerates innovation. Regional development coalitions see a coordination problem with policy solutions (Rope from organized perspective). But deindustrialized workers and rural professionals experience the divergence as an immutable economic law (Mountain) — the knowledge economy 'naturally' concentrates in cities, and their regions are 'naturally' left behind. The analytical observer, drawing on agglomeration economics, risks naturalizing this pattern as a structural feature of post-industrial production. The perspectival gap is not just about different experiences of the same constraint — it is about whether the constraint is a constraint at all (natural law) or a policy-contingent outcome that benefits some groups at others' expense (false summit). The 2020-2023 remote work experiment provides a natural test: if knowledge work can be distributed without major productivity losses, the agglomeration-as-natural-law framing is revealed as contingent. If distributed work proves unsustainable and urban concentration reasserts, the mountain classification is confirmed.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality structure is unusual: it has declared beneficiaries (urban knowledge workers, tech platforms) but no declared victims. This reflects the structural ambiguity at the heart of the false summit question. If spatial divergence is a natural law (genuine mountain), there are no victims — the pattern is an inevitable feature of post-industrial production, and rural decline is not extraction but differential access to coordination benefits. If spatial divergence is policy-contingent (false summit), then rural/deindustrialized regions are victims of policy choices that systematically favored urban agglomerations. The schema allows mountain constraints to declare beneficiaries (triggering FSM evaluation) without requiring victims. The engine will compute directionality for beneficiaries (low d, negative chi) and use canonical fallback values for non-beneficiary perspectives. The deindustrialized worker (powerless/trapped) gets canonical d=1.00 (maximum extraction) not because they are a declared victim but because their power/exit combination places them at the high end of the directionality scale. The perspectival gap emerges: beneficiaries see rope (coordination), trapped agents see mountain (immutable law), and the analytical observer risks naturalizing the pattern. The false summit detector evaluates whether the mountain classification + beneficiary presence indicates naturalization of contingent policy.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that very low extraction (epsilon=0.12) can coexist with significant subjective harm (status decline, political alienation) when the harm is primarily relative rather than absolute. The deindustrialized worker's material conditions may not be declining sharply (absolute income, access to goods), but their position in national status hierarchies is declining (media representation, political influence, cultural prestige). This is not extraction in the traditional sense (no agent is taking resources from them) but differential access to coordination benefits (agglomeration economies accrue to urban clusters). The mandatrophy question — 'Is this coordination or extraction?' — is resolved by recognizing that it is primarily coordination (low epsilon) with a distributional consequence (some regions benefit more than others). The false summit evaluation asks a different question: 'Is this distribution inevitable (natural law) or contingent (policy choice)?' If inevitable, the constraint is a genuine mountain and the distributional consequence is unavoidable. If contingent, the mountain classification naturalizes policy choices that favor urban agglomerations, and the constraint should be reclassified based on the policy mechanism (likely tangled_rope: genuine coordination function + asymmetric extraction via policy choices that concentrate infrastructure/education investment in already-advantaged regions).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    agglomeration_necessity,
    'Is knowledge-economy clustering in dense urban areas a necessary feature of post-industrial production, or a contingent outcome of policy choices (infrastructure investment, education access, zoning, tax policy) that could be altered?',
    'Comparative analysis of countries with different spatial distributions of knowledge work (e.g., Germany''s distributed manufacturing hubs vs. US coastal concentration); natural experiments from remote work adoption post-2020; historical analysis of spatial patterns before/after major infrastructure or education policy shifts',
    'If necessary: Mountain classification is correct — spatial divergence is structural. If contingent: Mountain classification is a false summit — the constraint naturalizes policy choices that benefit urban agglomerations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(agglomeration_necessity, empirical, 'Whether knowledge-economy clustering is structurally necessary or policy-contingent').

omega_variable(
    subjective_status_mechanism,
    'Is the subjective status decline in rural/deindustrialized regions driven by absolute economic conditions (income, employment, public services) or by relative position in national status hierarchies (media representation, political influence, cultural prestige)?',
    'Survey data correlating subjective well-being with absolute vs. relative economic indicators; analysis of political behavior (voting patterns, protest participation) controlling for income vs. status measures; media content analysis of rural representation',
    'If absolute: Extraction is material and measurable. If relative: Extraction is primarily status-based and may persist even if absolute conditions improve, suggesting identity_locked dynamics for some agents.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(subjective_status_mechanism, empirical, 'Whether subjective status decline is driven by absolute or relative conditions').

omega_variable(
    remote_work_counterfactual,
    'Does widespread remote work adoption (post-2020) represent a structural break in agglomeration economics, or a temporary deviation that will revert to urban concentration as coordination costs of distributed work become apparent?',
    'Longitudinal tracking of remote work rates, wage convergence between urban/rural remote workers, company location decisions, and urban-rural migration patterns 2020-2030; analysis of which knowledge-work sectors sustain distributed models vs. revert to co-location',
    'If structural break: The constraint''s suppression is declining (alternatives emerging), potentially shifting classification toward Scaffold for some perspectives. If temporary: Mountain classification confirmed — agglomeration forces reassert.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(remote_work_counterfactual, empirical, 'Whether remote work represents structural break or temporary deviation').

omega_variable(
    false_summit_beneficiary_structure,
    'Are the declared beneficiaries (urban knowledge workers, tech platforms) the result of natural agglomeration forces, or do they benefit from policy choices (infrastructure spending, education subsidies, zoning) that could have been allocated differently?',
    'Historical policy analysis: federal infrastructure investment patterns (interstate highways, broadband, transit); education funding distribution (R1 universities vs. community colleges); tax policy (state/local incentives for tech companies vs. manufacturing). Counterfactual: what spatial distribution would emerge under different policy regimes?',
    'If natural forces dominate: Mountain classification stands. If policy choices dominate: False summit confirmed — the constraint naturalizes extractive policy that concentrates resources in already-advantaged regions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_beneficiary_structure, conceptual, 'Whether beneficiary structure reflects natural forces or policy choices').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(post_industrial_spatial_extraction, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spatial_extract_theater_1980, post_industrial_spatial_extraction, theater_ratio, 0, 0.12).
narrative_ontology:measurement(spatial_extract_theater_2000, post_industrial_spatial_extraction, theater_ratio, 20, 0.14).
narrative_ontology:measurement(spatial_extract_theater_2020, post_industrial_spatial_extraction, theater_ratio, 40, 0.15).

% Extraction over time
narrative_ontology:measurement(spatial_extract_epsilon_1980, post_industrial_spatial_extraction, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(spatial_extract_epsilon_2000, post_industrial_spatial_extraction, base_extractiveness, 20, 0.1).
narrative_ontology:measurement(spatial_extract_epsilon_2020, post_industrial_spatial_extraction, base_extractiveness, 40, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(post_industrial_spatial_extraction, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is a single structural phenomenon (spatial divergence in post-industrial economies) with one stable epsilon value. It does not decompose into multiple constraints with different observables. However, it may be upstream of other constraints (political polarization, populist mobilization, rural healthcare access) that are affected by spatial divergence but have their own distinct epsilon values and should be modeled as separate stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
