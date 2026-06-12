% ============================================================================
% CONSTRAINT STORY: technocratic_paradigm_vs_human_primacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-09
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technocratic_paradigm_vs_human_primacy, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: technocratic_paradigm_vs_human_primacy
 *   human_readable: Technocratic Paradigm vs Human Primacy in AI Systems
 *   domain: technology_ethics/political_theology/ai_governance
 *
 * SUMMARY:
 *   The technocratic paradigm in AI systems embeds a structural logic that
 *   privileges efficiency, control, and profit maximization over human
 *   dignity as the measure of value. This constraint operates through design
 *   choices: what AI systems measure (engagement, productivity, risk scores),
 *   what they optimize (platform revenue, operational efficiency, predictive
 *   accuracy), and what they structurally exclude (human context, appeal
 *   mechanisms, dignity-preserving alternatives). The encyclical Antiqua et
 *   Nova identifies this as a civilizational-scale challenge: AI is not
 *   neutral infrastructure but a paradigm that shapes how societies
 *   understand and value persons. The constraint exhibits tangled rope
 *   structure from the analytical perspective because it combines genuine
 *   coordination functions (AI enables scaled services, resource matching,
 *   global communication) with asymmetric extraction (vulnerable populations
 *   excluded by classification systems, workers subject to opaque algorithmic
 *   control, communities denied meaningful appeal). The measurements show
 *   accumulating extraction (0.35 to 0.58 over 12 years) and rising theater
 *   ratio (0.25 to 0.48) as human oversight mechanisms become increasingly
 *   performative. Suppression has intensified (0.45 to 0.62) as algorithmic
 *   systems become infrastructure with no viable exit: credit scoring,
 *   employment screening, and social service allocation increasingly route
 *   through AI systems that vulnerable populations cannot contest or escape.
 *
 * KEY AGENTS:
 *   - Vulnerable Populations Excluded by Algorithmic Classification: Primary victim (powerless/trapped) — denied services, employment, housing by automated systems with no appeal; dignity reduced to failing optimization criteria
 *   - Workers Subject to Algorithmic Management: Secondary victim (moderate/constrained) — benefit from platform coordination while bearing extraction through opaque performance metrics and unappealable control
 *   - Digital Power Holders / Platform Operators: Primary beneficiary (institutional/arbitrage) — capture value from coordination function while externalizing dignity costs onto users and workers
 *   - Regulatory Agencies: Institutional actor (institutional/constrained) — benefit from AI-enabled scaled enforcement while constrained by capture dynamics and technical complexity exceeding oversight capacity
 *   - Digital Rights Coalition: Organized agents (organized/mobile) — building alternative governance models and advocacy for human-centered AI; see regulatory sunset in emerging frameworks
 *   - Magisterial Observer (Catholic Social Teaching): Analytical perspective (analytical/analytical) — recognizes both coordination function and structural extraction; identifies technocratic paradigm as paradigm requiring contestation, not mere regulation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technocratic_paradigm_vs_human_primacy, 0.58).
domain_priors:suppression_score(technocratic_paradigm_vs_human_primacy, 0.62).
domain_priors:theater_ratio(technocratic_paradigm_vs_human_primacy, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technocratic_paradigm_vs_human_primacy, extractiveness, 0.58).
narrative_ontology:constraint_metric(technocratic_paradigm_vs_human_primacy, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(technocratic_paradigm_vs_human_primacy, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technocratic_paradigm_vs_human_primacy, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(technocratic_paradigm_vs_human_primacy, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technocratic_paradigm_vs_human_primacy, tangled_rope).
narrative_ontology:human_readable(technocratic_paradigm_vs_human_primacy, "Technocratic Paradigm vs Human Primacy in AI Systems").
narrative_ontology:topic_domain(technocratic_paradigm_vs_human_primacy, "technology_ethics/political_theology/ai_governance").

domain_priors:requires_active_enforcement(technocratic_paradigm_vs_human_primacy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technocratic_paradigm_vs_human_primacy, '094f17aa-3111-40ff-bc95-663b30059d68').
narrative_ontology:cs_kernel_codification('094f17aa-3111-40ff-bc95-663b30059d68', formalized).
narrative_ontology:cs_authority_grounding('094f17aa-3111-40ff-bc95-663b30059d68', lineage).
narrative_ontology:cs_interpretation_layer_present('094f17aa-3111-40ff-bc95-663b30059d68').
narrative_ontology:cs_created_at('094f17aa-3111-40ff-bc95-663b30059d68', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technocratic_paradigm_vs_human_primacy, digital_power_holders).
narrative_ontology:constraint_beneficiary(technocratic_paradigm_vs_human_primacy, platform_operators).
narrative_ontology:constraint_beneficiary(technocratic_paradigm_vs_human_primacy, surveillance_infrastructure_providers).
narrative_ontology:constraint_victim(technocratic_paradigm_vs_human_primacy, vulnerable_populations_excluded_by_algorithmic_classification).
narrative_ontology:constraint_victim(technocratic_paradigm_vs_human_primacy, workers_subject_to_algorithmic_management).
narrative_ontology:constraint_victim(technocratic_paradigm_vs_human_primacy, communities_without_appeal_mechanisms).
narrative_ontology:constraint_vindicates(technocratic_paradigm_vs_human_primacy, efficiency_as_primary_value).
narrative_ontology:constraint_vindicates(technocratic_paradigm_vs_human_primacy, quantification_completeness_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: AI systems coordinate resource allocation, service delivery, and information flows at scales that exceed human manual capacity. They match supply and demand across global platforms, enable real-time optimization of complex systems, and provide infrastructure for digital communication and commerce.
% TRANSFER_FUNCTION: The arrangement moves decision-making authority, economic value, and personal data from individuals and communities to platform operators and digital infrastructure providers. Workers transfer labor and attention to algorithmic management systems. Vulnerable populations transfer dignity and agency to classification systems that determine access to credit, housing, employment, and social services.
% ABSENT_VOICES: Vulnerable populations excluded by algorithmic classification are structurally absent from AI governance conversations: they lack technical literacy to contest system design, lack resources to participate in policy processes, and lack representation in the institutions (tech companies, regulatory agencies, standards bodies) that set AI governance frameworks. The encyclical explicitly names this absence as a justice issue requiring preferential option for those excluded.
% DISAPPEARANCE_RATIONALE: If AI systems embedding the technocratic paradigm disappeared overnight, digital platform economies would require fundamental reorganization. Gig workers would need alternative employment coordination mechanisms. Credit, housing, and employment allocation would revert to human decision-making with different (though not necessarily better) exclusion patterns. Surveillance infrastructure providers would lose revenue streams. The rearrangement would be substantial because AI has become infrastructure, but the world would not collapse — alternative coordination mechanisms exist and functioned historically.
% FOUNDING_PROBLEM: The founding problem was coordination at scale: how to match supply and demand across global platforms, optimize resource allocation in complex systems, and process information flows that exceed human manual capacity. AI systems were built to solve genuine coordination challenges in digital economies, global logistics, and information management.
% FOUNDING_PROBLEM_CORROBORATION: The coordination problem remains live and is corroborated by multiple sources outside the beneficiary set: labor economists document genuine efficiency gains from algorithmic matching in gig economies (though they also document extraction through wage suppression and control). Digital rights advocates acknowledge coordination benefits while contesting extraction mechanisms. The encyclical itself recognizes AI's potential to serve the common good (¶15-18), corroborating that the founding coordination problem is real. The problem is not that coordination is obsolete but that the current implementation embeds extraction alongside coordination.
narrative_ontology:disappearance_verdict(technocratic_paradigm_vs_human_primacy, world_rearranges).
narrative_ontology:founding_problem_status(technocratic_paradigm_vs_human_primacy, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ALGORITHMICALLY EXCLUDED (SNARE) — Vulnerable populations denied credit, housing, employment, or social services by algorithmic classification systems with no meaningful appeal. Trapped by lack of alternative systems and inability to contest automated decisions. Maximum extraction: dignity reduced to data points that fail optimization criteria.
constraint_indexing:constraint_classification(technocratic_paradigm_vs_human_primacy, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: GIG WORKER UNDER ALGORITHMIC MANAGEMENT (TANGLED ROPE) — Benefits from platform coordination (access to work, payment infrastructure) while bearing extraction through algorithmic control (opaque performance metrics, unappealable deactivation, wage suppression via optimization). Constrained exit: can switch platforms but cannot exit the algorithmic management paradigm itself.
constraint_indexing:constraint_classification(technocratic_paradigm_vs_human_primacy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PLATFORM OPERATOR (ROPE) — Experiences AI systems as pure coordination: matching supply and demand, optimizing resource allocation, scaling services globally. Net beneficiary of the constraint. Arbitrage exit: can choose governance frameworks, jurisdictions, and design paradigms that maximize extraction while maintaining coordination narrative.
constraint_indexing:constraint_classification(technocratic_paradigm_vs_human_primacy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DIGITAL RIGHTS COALITION (SCAFFOLD) — Organized advocacy groups (algorithmic accountability networks, worker organizing platforms, digital rights NGOs) see the technocratic paradigm as a temporary coordination failure being addressed through regulation (EU AI Act, right-to-explanation mandates, algorithmic impact assessments). Mobile exit: can shift advocacy strategies and build alternative governance models. Sees sunset in emerging regulatory frameworks requiring human oversight and appeal mechanisms.
constraint_indexing:constraint_classification(technocratic_paradigm_vs_human_primacy, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: REGULATORY AGENCY (TANGLED ROPE) — Benefits from AI systems enabling scaled enforcement and monitoring (coordination function) while constrained by regulatory capture dynamics and technical complexity that exceeds oversight capacity (extraction). Constrained exit: cannot abandon AI governance but lacks resources to verify compliance or contest industry technical claims.
constraint_indexing:constraint_classification(technocratic_paradigm_vs_human_primacy, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: MAGISTERIAL ANALYTICAL OBSERVER (TANGLED ROPE) — Catholic Social Teaching recognizes genuine coordination function (AI can serve common good, enhance human capabilities, address global challenges) while identifying structural extraction (technocratic paradigm reduces persons to data, concentrates power, excludes vulnerable populations). The encyclical's analytical stance: AI embeds a paradigm that must be contested, not merely regulated. Tangled rope reflects the document's own assessment: technology is not neutral, and the current trajectory mixes real benefits with structural violence against human dignity.
constraint_indexing:constraint_classification(technocratic_paradigm_vs_human_primacy, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technocratic_paradigm_vs_human_primacy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(technocratic_paradigm_vs_human_primacy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(technocratic_paradigm_vs_human_primacy, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(technocratic_paradigm_vs_human_primacy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(technocratic_paradigm_vs_human_primacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Substantial. AI systems designed under profit-maximizing and efficiency-first paradigms systematically displace human dignity as the measure of value. The extraction is not total because genuine coordination functions exist (platforms do match supply and demand, AI does enable scaled services), but the asymmetry is severe: vulnerable populations bear dignity costs (exclusion, loss of agency, reduction to data) while digital power holders capture coordination benefits. The value reflects that roughly 60% of the constraint's operation is extractive overhead beyond necessary coordination cost. Suppression (0.62): Moderate-high. Alternatives to algorithmic systems are increasingly foreclosed as AI becomes infrastructure for credit, employment, housing, and social services. Exit options exist in principle (cash economy, informal networks, geographic mobility) but are costly and incomplete. Appeal mechanisms exist formally but are often inaccessible or ineffective for vulnerable populations. The suppression is not total (resistance movements exist, some jurisdictions mandate human override) but is substantial and rising. Theater ratio (0.48): Moderate. Human-in-the-loop and explainability requirements are increasingly performative: humans lack capacity to meaningfully contest algorithmic outputs, explanations are post-hoc rationalizations rather than genuine transparency, and appeal processes route to the same algorithmic systems. But theater is not yet dominant — some oversight mechanisms retain function, and regulatory frameworks are still being built. The ratio has risen steadily as systems scale beyond human oversight capacity. Accessibility collapse (0.42): Moderate. Once algorithmic systems become infrastructure, alternatives partially collapse but do not disappear entirely. Informal economies, community mutual aid, and geographic arbitrage remain possible but costly. The collapse is less severe than for natural laws because the constraint is institutional rather than physical. Resistance (0.68): Substantial. The technocratic paradigm meets significant organized resistance from digital rights advocates, labor organizers, religious institutions, and affected communities. The encyclical itself is an act of resistance from magisterial authority. High resistance indicates the constraint is contested and contingent, not naturalized.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates the full indexical range from snare (powerless/trapped victims) through tangled rope (moderate/constrained workers and analytical observer) to rope (institutional/arbitrage beneficiaries) and scaffold (organized/mobile advocates). The platform operator sees pure coordination: AI solves matching problems, scales services, optimizes resource allocation. The algorithmically excluded see pure extraction: dignity reduced to data points, services denied without appeal, agency eliminated by automated systems. The gig worker sees both: platform coordination enables work while algorithmic management extracts through opaque control. The digital rights coalition sees a temporary problem with regulatory sunset: emerging frameworks (EU AI Act, right-to-explanation mandates) are building human-centered alternatives. The regulatory agency sees mixed coordination and capture: AI enables scaled enforcement while technical complexity and industry influence constrain oversight. The magisterial observer sees structural extraction embedded in a paradigm: the technocratic logic itself displaces human primacy, requiring civilizational-scale contestation rather than mere regulatory adjustment. The perspectival gap reveals that 'AI neutrality' is itself a beneficiary narrative: those who profit from the current paradigm experience it as pure coordination, while those who bear dignity costs experience it as extraction or snare.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from structural position relative to the constraint's extraction flow. Digital power holders (platform operators, surveillance infrastructure providers) are primary beneficiaries: they capture coordination value while externalizing dignity costs. The engine derives low d (beneficiary position) from their institutional power and arbitrage exit options, producing low or negative effective extraction — they experience the constraint as coordination. Vulnerable populations excluded by algorithmic classification are primary victims: they bear maximum dignity costs with no exit and no appeal. The engine derives high d (victim position) from their powerless status and trapped exit options, producing maximum effective extraction — they experience the constraint as pure snare. Workers under algorithmic management occupy a middle position: they benefit from platform coordination (access to work, payment infrastructure) while bearing extraction through algorithmic control. The engine derives moderate d from their moderate power and constrained exit, producing substantial but not maximal effective extraction — they experience tangled rope. Regulatory agencies are institutional actors with constrained exit: they benefit from AI-enabled enforcement capacity while constrained by capture dynamics and technical complexity. The engine derives moderate d from their institutional power dampened by constrained exit and partial victim status (captured regulators bear legitimacy costs). The magisterial analytical observer occupies the analytical context with no direct extraction flow — the engine derives d from the structural assessment of coordination vs extraction balance, producing the tangled rope classification that matches the encyclical's own analysis.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by distinguishing coordination function (AI enables scaled services, resource matching, global communication) from extraction mechanism (technocratic paradigm reduces persons to optimizable data, concentrates power, excludes vulnerable populations). The tangled rope classification at the analytical level reflects the encyclical's own structural assessment: AI is not neutral, and the current trajectory mixes genuine benefits with structural violence against human dignity. The mandate (serve human dignity and common good) has not outlived its function, but the current implementation embeds a paradigm that contradicts the mandate. This is not mandatrophy (obsolete function) but paradigm contestation (wrong implementation of live function). The scaffold perspective from organized advocates suggests a potential sunset through regulatory frameworks requiring human primacy, but the omega variables identify irreducible uncertainties: whether human override mechanisms will remain genuine or become theater, whether efficiency-dignity tradeoffs are technical necessities or institutional choices, and whether dignity can be operationalized as a primary optimization target without proxy drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    human_override_sufficiency,
    'Do mandated human-in-the-loop and appeal mechanisms constitute genuine structural protection of human primacy, or do they become performative compliance theater when humans lack capacity to contest algorithmic outputs?',
    'Empirical analysis of appeal mechanism outcomes: reversal rates, time-to-resolution, accessibility to vulnerable populations. Comparison of systems with formal human override vs. effective human authority.',
    'If genuine: regulatory scaffold perspective confirmed, sunset is real. If performative: human override becomes theater, and the tangled rope deepens into snare for those who cannot effectively exercise appeal rights.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(human_override_sufficiency, empirical, 'Whether human override mechanisms provide genuine protection or become compliance theater').

omega_variable(
    efficiency_dignity_tradeoff_necessity,
    'Is the efficiency-dignity tradeoff inherent to AI systems at scale, or is it a contingent design choice reflecting current power distributions and profit incentives?',
    'Comparative analysis of AI systems designed under different governance models (cooperative platforms, public infrastructure, profit-maximizing corporations). Identification of technical vs. institutional sources of dignity displacement.',
    'If inherent: the constraint is closer to mountain (immutable tradeoff). If contingent: the constraint is tangled rope or snare (constructed extraction), and alternative designs are structurally possible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(efficiency_dignity_tradeoff_necessity, conceptual, 'Whether efficiency-dignity tradeoff is technical necessity or institutional choice').

omega_variable(
    magisterial_authority_scope,
    'Does magisterial teaching authority extend to technical design specifications for AI systems, or only to normative principles that must be translated by technical experts?',
    'Doctrinal analysis of CST''s historical scope claims in technological domains. Examination of whether the encyclical''s specific prescriptions (e.g., ''algorithms must be verifiable, explainable, and controllable'') constitute binding technical requirements or illustrative applications of principles.',
    'If authority extends to specifications: CST becomes a technical governance framework with enforcement implications. If limited to principles: translation layer introduces interpretive discretion that may reintroduce technocratic paradigm through implementation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(magisterial_authority_scope, conceptual, 'Scope of magisterial authority over technical AI design choices').

omega_variable(
    alternative_optimization_targets,
    'Can AI systems be designed to optimize for human dignity and common good as primary objectives, or do these values resist quantification in ways that structurally privilege efficiency metrics?',
    'Technical research on value alignment and objective specification. Case studies of systems attempting dignity-centered design. Analysis of whether dignity metrics become proxy measures that drift toward efficiency.',
    'If dignity can be primary optimization target: technocratic paradigm is contingent, alternative designs are feasible. If dignity resists quantification: the constraint may be closer to mountain (technical limit) or require non-optimization governance paradigms.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_optimization_targets, empirical, 'Technical feasibility of dignity-centered optimization in AI systems').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technocratic_paradigm_vs_human_primacy, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_para_theater_2010, technocratic_paradigm_vs_human_primacy, theater_ratio, 0, 0.25).
narrative_ontology:measurement(tech_para_theater_2013, technocratic_paradigm_vs_human_primacy, theater_ratio, 3, 0.32).
narrative_ontology:measurement(tech_para_theater_2016, technocratic_paradigm_vs_human_primacy, theater_ratio, 6, 0.38).
narrative_ontology:measurement(tech_para_theater_2019, technocratic_paradigm_vs_human_primacy, theater_ratio, 9, 0.44).
narrative_ontology:measurement(tech_para_theater_2022, technocratic_paradigm_vs_human_primacy, theater_ratio, 12, 0.48).

% Extraction over time
narrative_ontology:measurement(tech_para_extract_2010, technocratic_paradigm_vs_human_primacy, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(tech_para_extract_2013, technocratic_paradigm_vs_human_primacy, base_extractiveness, 3, 0.42).
narrative_ontology:measurement(tech_para_extract_2016, technocratic_paradigm_vs_human_primacy, base_extractiveness, 6, 0.51).
narrative_ontology:measurement(tech_para_extract_2019, technocratic_paradigm_vs_human_primacy, base_extractiveness, 9, 0.55).
narrative_ontology:measurement(tech_para_extract_2022, technocratic_paradigm_vs_human_primacy, base_extractiveness, 12, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(tech_para_suppress_2010, technocratic_paradigm_vs_human_primacy, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(tech_para_suppress_2016, technocratic_paradigm_vs_human_primacy, suppression_requirement, 6, 0.55).
narrative_ontology:measurement(tech_para_suppress_2022, technocratic_paradigm_vs_human_primacy, suppression_requirement, 12, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technocratic_paradigm_vs_human_primacy, global_infrastructure).
narrative_ontology:affects_constraint(technocratic_paradigm_vs_human_primacy, algorithmic_management_labor_control).
narrative_ontology:affects_constraint(technocratic_paradigm_vs_human_primacy, credit_scoring_exclusion).
narrative_ontology:affects_constraint(technocratic_paradigm_vs_human_primacy, content_moderation_speech_governance).

% DUAL FORMULATION NOTE:
% The technocratic paradigm is a meta-constraint that structures multiple domain-specific AI governance constraints. Upstream of specific algorithmic systems (credit scoring, content moderation, labor management) but distinct from them: the paradigm is the embedded logic (efficiency/control/profit as primary values) that shapes design choices across domains. Each downstream constraint has its own extractiveness reflecting domain-specific power asymmetries; the paradigm constraint's extractiveness reflects the civilizational-scale displacement of human dignity as measure of value.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(technocratic_paradigm_vs_human_primacy, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
