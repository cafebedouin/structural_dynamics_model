% ============================================================================
% CONSTRAINT STORY: genie_ip_constraint
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_genie_ip_constraint, []).

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
 *   constraint_id: genie_ip_constraint
 *   human_readable: Google's Project Genie IP Concerns
 *   domain: technological/intellectual_property
 *
 * SUMMARY:
 *   Project Genie exemplifies a structural constraint arising from asymmetric
 *   access to training data and generative capability. Google's system is
 *   trained on decades of game design, art, mechanics, and narrative from
 *   indie developers, publishers, and user-generated content. The constraint
 *   arises from the gap between who contributes to the training corpus
 *   (dispersed global creators, many powerless to negotiate) and who controls
 *   the generative tool (Google's institutional position). The extractiveness
 *   is moderate (0.52) rather than maximal because legitimate coordination
 *   benefits exist: Genie enables game creation for users lacking skills,
 *   capital, or studio access. However, suppression is high (0.68) because
 *   alternatives are limited, licensing frameworks lack transparency, and
 *   individual creators have minimal negotiating power. The theater ratio
 *   (0.58) reflects Google's framing of Genie as a democratizing tool while
 *   the actual mechanism concentrates IP ownership—public messaging
 *   emphasizes accessibility; structural reality emphasizes control.
 *
 * KEY AGENTS:
 *   - Original Game Developers: Primary victims (powerless/trapped) — indie creators whose work trained models without licensing or compensation; cannot exit or negotiate
 *   - IP Rights Holders: Primary victims (powerful/constrained) — major publishers and studios with leverage but facing litigation costs and terms-of-service lock-in
 *   - Google Development Team: Primary beneficiary (institutional/arbitrage) — captures first-mover advantage; controls training data access and licensing terms
 *   - Game Users/Content Creators: Secondary victims (powerful/mobile) — gain tool access but lose IP ownership of generated content; can migrate to alternatives at switching cost
 *   - Open-Source Community: Organized agent (organized/mobile) — building alternative frameworks with transparent licensing; represents sunset pathway
 *   - Existing IP/Copyright Framework: Institutional actor (institutional/arbitrage) — legal infrastructure is degraded (piton); designed for publication scarcity, not neural network training
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing extraction as inherent to generative systems rather than contingent to training data sourcing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(genie_ip_constraint, 0.52).
domain_priors:suppression_score(genie_ip_constraint, 0.68).
domain_priors:theater_ratio(genie_ip_constraint, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(genie_ip_constraint, extractiveness, 0.52).
narrative_ontology:constraint_metric(genie_ip_constraint, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(genie_ip_constraint, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(genie_ip_constraint, tangled_rope).
narrative_ontology:human_readable(genie_ip_constraint, "Google's Project Genie IP Concerns").
narrative_ontology:topic_domain(genie_ip_constraint, "technological/intellectual_property").

domain_priors:requires_active_enforcement(genie_ip_constraint).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(genie_ip_constraint, google_development_team).
narrative_ontology:constraint_beneficiary(genie_ip_constraint, users_with_generative_tools).
narrative_ontology:constraint_victim(genie_ip_constraint, original_game_developers).
narrative_ontology:constraint_victim(genie_ip_constraint, ip_rights_holders).
narrative_ontology:constraint_victim(genie_ip_constraint, creative_industry_workers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ORIGINAL GAME DEVELOPERS (SNARE) — Small indie developers and IP rights holders cannot exit the constraint. Project Genie's generative models trained on existing games create derivative works without licensing or compensation mechanisms. Trapped with no alternatives; bear full extraction cost. d≈0.93, f(d)≈1.40, σ=1.2 → χ≈0.78.
constraint_indexing:constraint_classification(genie_ip_constraint, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: GAME PUBLISHING INDUSTRY (TANGLED ROPE) — Major publishers constrained by IP enforcement costs and litigation risk, but also benefit from potential Genie licensing partnerships and integration opportunities. Mixed extraction and coordination: enforcing IP rights is expensive; participating in Genie ecosystem offers market access but on Google's terms. d≈0.72, f(d)≈1.12, σ=1.0 → χ≈0.58.
constraint_indexing:constraint_classification(genie_ip_constraint, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: GOOGLE DEVELOPMENT TEAM (ROPE) — Benefits from access to vast training datasets and first-mover advantage in generative game creation. Experiences the constraint as coordination: training on existing games enables tool functionality; licensing frameworks are negotiated (albeit on Google's terms). d≈0.08, f(d)≈-0.11, σ=1.2 → χ≈-0.07. Net beneficiary through arbitrage position.
constraint_indexing:constraint_classification(genie_ip_constraint, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN-SOURCE GAME DEVELOPMENT COMMUNITY (SCAFFOLD) — Open-source frameworks (Godot, Unity Community) can build alternative generative tools with transparent licensing and community governance. See Genie as temporary extraction mechanism; distributed development and ethical licensing create sunset pathway. d≈0.35, f(d)≈0.32, σ=1.2 → χ≈0.20. Coalition has agency and migration path.
constraint_indexing:constraint_classification(genie_ip_constraint, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: EXISTING COPYRIGHT/IP LEGAL FRAMEWORK (PITON) — Copyright law governing derivative works and training data use is degraded: designed for 20th-century publication scarcity, not 21st-century neural network training. Theater ratio=0.58: regulatory rhetoric about 'fair use' and 'transformative works' masks that enforcement is theater—copyright holders lack practical remedies for large-scale model training. Persists through institutional inertia despite functional failure. d≈0.05, f(d)≈-0.12, σ=1.0 → χ≈-0.03.
constraint_indexing:constraint_classification(genie_ip_constraint, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: GAME USERS AND CONTENT CREATORS (TANGLED ROPE) — Users gain access to game creation tools (coordination benefit), but Google captures and owns derivative content generated through Genie. Terms of service create asymmetric extraction: users contribute generative labor; Google owns outputs. Mobile exit available (other tools, proprietary engines) but switching costs are real. d≈0.58, f(d)≈0.78, σ=1.2 → χ≈0.48.
constraint_indexing:constraint_classification(genie_ip_constraint, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / INFORMATION THEORY VIEW (MOUNTAIN) — From a universal perspective, the constraint appears as an inherent property of generative systems: information encoded in training data becomes inseparable from model function. Derivative content generation is mathematically unavoidable if models are trained on any corpus. However, this perspective naturalizes what is actually a contingent choice about training data sourcing and licensing—the engine's false summit detector will flag this as naturalization of policy.
constraint_indexing:constraint_classification(genie_ip_constraint, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(genie_ip_constraint_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(genie_ip_constraint, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(genie_ip_constraint, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(genie_ip_constraint, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(genie_ip_constraint, TR),
    TR >= 0.70.

:- end_tests(genie_ip_constraint_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Google captures asymmetric value: the tool provides genuine coordination benefit (enabling creation), but IP ownership flows to Google, not creators. Over the interval, extractiveness has increased (0.38→0.52) as the tool's capability improved and adoption grew—more powerful models extract more value from training data. Suppression (0.68): High. Individual creators lack practical remedies (litigation costs exceed typical game revenue), licensing frameworks are opaque, and alternatives are nascent. But suppression is not total—open-source alternatives exist and legal frameworks (copyright, fair use, emerging AI training regulations) provide some formal channels, albeit ineffective. Theater ratio (0.58): Moderate. Google's public positioning emphasizes democratization and accessibility (coordination narrative); the actual mechanism is IP consolidation and asymmetric extraction. The gap has widened over the interval (0.35→0.58) as Genie's real capability and actual terms of service became clearer to developers.
 *
 * PERSPECTIVAL GAP:
 *   Original developers see extraction (Snare): no compensation, no alternatives, no control over outputs. Publishers see mixed extraction and coordination (Tangled Rope): licensing partnerships offer upside but only on Google's terms; enforcing IP rights is expensive. Users see mixed extraction and coordination (Tangled Rope): tool access is real benefit; IP ownership loss is structural cost. Google sees coordination (Rope): training data enables the tool; licensing is negotiated. Open-source coalition sees temporary problem with sunset (Scaffold): alternative frameworks can be built with transparent licensing. IP/copyright framework sees its own degradation (Piton): copyright law's rhetoric about fair use masks that enforcement is theater; frameworks designed for copying, not model training. Analytical observer risks false summit (Mountain): naturalizes extraction as inherent to generative systems rather than contingent policy. The perspectival gap reveals that 'democratization' and 'extraction' are describing the same mechanism from different structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Original developers: Victim + trapped → d≈0.93, f(d)≈1.40. Maximum extraction. Publishers: Victim + constrained → d≈0.72, f(d)≈1.12. High extraction but not maximal; litigation options provide some constrained exit. Google: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Net beneficiary; controls training data access and licensing terms. Users: Victim + mobile → d≈0.58, f(d)≈0.78. Moderate extraction; can migrate to alternatives. Open-source coalition: Organized + mobile → d≈0.35, f(d)≈0.32. Low effective extraction; coalition has agency. IP framework: Institutional + arbitrage → d≈0.05, f(d)≈-0.12. Piton classification from theater gate. Analytical observer: analytical → d≈0.72, f(d)≈1.15. False summit risk—naturalizes extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED: The mandatrophy is resolved by recognizing that 'democratization' (Rope perspective) and 'extraction' (Snare perspective) are both accurate descriptions of the same mechanism, viewed from different structural positions. Google's framing emphasizes coordination (tool access), which is real. The structural reality for IP holders emphasizes extraction (asymmetric value capture), which is also real. The Tangled Rope classification captures this hybrid: Genie has a genuine coordination function (enables creation), active enforcement mechanism (terms of service, data licensing), and asymmetric extraction (IP flows to Google). The mandatrophy resolution is that both narratives are structurally valid—there is no single 'correct' type. The analytical observer risks false summit by naturalizing the extraction as inherent to generative systems. The real constraint is the contingent policy: training data sourcing without licensing, output IP allocation to Google, and terms-of-service lock-in. Alternative policies (transparent licensing, user IP ownership, open training data) would reclassify this as Rope or Scaffold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fair_use_generative_training,
    'Does training generative models on copyrighted game assets constitute fair use or infringing derivative creation?',
    'Judicial precedent from major IP litigation (Authors Guild v Google analog for generative AI); regulatory clarification from USPTO or EU on AI training rights; international treaty harmonization on training data licensing',
    'If fair use: Genie reclassifies from Snare/Tangled Rope to Rope for most perspectives. If infringement: Genie remains Snare for IP holders; legal liability and licensing frameworks become primary structural feature.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fair_use_generative_training, conceptual, 'Whether generative model training on copyrighted assets constitutes fair use').

omega_variable(
    training_data_detectability,
    'Can original game assets be identified or separated from model outputs by developers or forensic analysis?',
    'Technical analysis of model internals (activation pattern analysis, training data recovery); fingerprinting studies of synthetic outputs vs originals; reverse-engineering of Genie model weights',
    'If fully recoverable: enables proof of infringement and supports IP holder claims (strengthens Snare classification). If undetectable: suppression increases; extraction becomes nearly perfect and unverifiable (worse Snare/Tangled Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(training_data_detectability, empirical, 'Whether original game assets can be forensically detected in Genie outputs').

omega_variable(
    open_source_viability_timeline,
    'What is the realistic timeline for open-source game generation tools (Godot extensions, community forks) to match Genie capability and accessibility?',
    'Tracking development progress of open-source generative game tools; benchmark comparison (quality, speed, usability); adoption metrics for alternatives; funding availability for community projects',
    'If timeline < 3 years: Scaffold sunset is credible, extraction window narrows. If timeline > 10 years: Genie''s institutional lock-in deepens; coalition perspective is aspirational rather than structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(open_source_viability_timeline, empirical, 'Timeline for open-source game generation tools to reach parity with Genie').

omega_variable(
    licensing_partnership_uptake,
    'What percentage of indie developers and IP holders will accept licensing partnerships on Google''s standard terms vs demanding alternative models?',
    'Survey of developer communities; analysis of licensing agreements Genie negotiates; comparison with publisher acceptance rates for similar frameworks (Unity Asset Store, Unreal Marketplace)',
    'If partnership uptake > 60%: extraction moves from coercive (Snare) to negotiated (Rope/Tangled Rope). If < 20%: suppression evidence strengthens; licensing becomes performative theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(licensing_partnership_uptake, empirical, 'Percentage of developers accepting Google''s Genie licensing partnerships').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(genie_ip_constraint, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(genie_tr_t0, genie_ip_constraint, theater_ratio, 0, 0.35).
narrative_ontology:measurement(genie_tr_t3, genie_ip_constraint, theater_ratio, 3, 0.48).
narrative_ontology:measurement(genie_tr_t6, genie_ip_constraint, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(genie_be_t0, genie_ip_constraint, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(genie_be_t3, genie_ip_constraint, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(genie_be_t6, genie_ip_constraint, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(genie_ip_constraint, resource_allocation).
narrative_ontology:affects_constraint(genie_ip_constraint, ai_training_data_licensing).
narrative_ontology:affects_constraint(genie_ip_constraint, creative_worker_income_concentration).
narrative_ontology:affects_constraint(genie_ip_constraint, generative_tool_market_structure).

% DUAL FORMULATION NOTE:
% Project Genie's IP constraint is downstream of broader AI training data licensing frameworks and upstream of specific creative industry labor impacts. The structural constraint identified here (asymmetric IP capture) has ε=0.52; upstream constraints (training data sourcing regimes) may have different ε values; downstream constraints (individual creator income losses) have their own structural properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(genie_ip_constraint, powerful, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
