% ============================================================================
% CONSTRAINT STORY: ai_talent_migration_restrictions
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_talent_migration_restrictions, []).

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
 *   constraint_id: ai_talent_migration_restrictions
 *   human_readable: AI Talent Migration Restrictions and Brain Drain Dynamics
 *   domain: geopolitical/labor/technology
 *
 * SUMMARY:
 *   AI talent migration restrictions represent a complex constraint operating
 *   at the intersection of national security, economic competition, and human
 *   capital distribution. The constraint exhibits hybrid characteristics:
 *   genuine coordination problems (security vetting, credential verification)
 *   coexist with asymmetric extraction (brain drain concentration in
 *   incumbent centers, blocking of emerging region talent). The constraint's
 *   extractiveness has increased over the measurement interval (0.42 → 0.58)
 *   as geopolitical competition has intensified and security screening has
 *   become more stringent. Theater_ratio remains moderate (0.45) because
 *   security screening, while partly performative, maintains some genuine
 *   verification function unlike pure theatrical constraints. The restriction
 *   mechanism operates through multiple channels: visa quota systems, work
 *   permit requirements, security clearance prerequisites, credential
 *   non-recognition, and active talent recruitment by incumbent centers.
 *   These mechanisms collectively suppress alternative exit routes and
 *   concentrate talent globally in a small number of dominant regions.
 *   Multiple perspectives are necessary because the constraint's
 *   classification depends fundamentally on the observer's structural
 *   position: incumbent centers perceive coordination and opportunity;
 *   emerging regions perceive snare and irreversible loss; elite mobile
 *   researchers experience manageable constraint; distributed networks
 *   perceive temporary problem with architectural solution.
 *
 * KEY AGENTS:
 *   - Emerging Region AI Researcher: Primary victim (powerless/trapped) — faces visa restrictions, credential barriers, and zero exit options
 *   - Incumbent AI Centers (Silicon Valley, Beijing, London): Primary beneficiary (institutional/arbitrage) — concentrate global talent through selective immigration and credential recognition
 *   - Local AI Ecosystem Developer: Secondary victim (moderate/constrained) — benefits from local coordination but suffers from talent loss
 *   - Elite Mobile Researcher: Mixed beneficiary/victim (powerful/mobile) — faces constraints but has capacity to overcome them through prestige and negotiating power
 *   - Emerging Nation State: Organized enforcer (organized/constrained) — coordinates domestic capability while extracting through brain drain prevention, also victim of incumbent center recruitment
 *   - National Security Apparatus: Institutional maintainer (institutional/arbitrage) — frames restrictions as security, maintains performative screening
 *   - Distributed AI Coalition: Organized alternative builder (organized/constrained) — developing remote-first and decentralized research pathways with sunset logic
 *   - Analytical Observer: Civilizational risk (analytical/analytical) — risks naturalizing contingent restrictions as sovereign necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_talent_migration_restrictions, 0.58).
domain_priors:suppression_score(ai_talent_migration_restrictions, 0.68).
domain_priors:theater_ratio(ai_talent_migration_restrictions, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_talent_migration_restrictions, extractiveness, 0.58).
narrative_ontology:constraint_metric(ai_talent_migration_restrictions, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(ai_talent_migration_restrictions, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_talent_migration_restrictions, tangled_rope).
narrative_ontology:human_readable(ai_talent_migration_restrictions, "AI Talent Migration Restrictions and Brain Drain Dynamics").
narrative_ontology:topic_domain(ai_talent_migration_restrictions, "geopolitical/labor/technology").

domain_priors:requires_active_enforcement(ai_talent_migration_restrictions).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_talent_migration_restrictions, incumbent_ai_centers).
narrative_ontology:constraint_beneficiary(ai_talent_migration_restrictions, large_incumbent_firms).
narrative_ontology:constraint_beneficiary(ai_talent_migration_restrictions, credentialing_gatekeepers).
narrative_ontology:constraint_victim(ai_talent_migration_restrictions, emerging_ai_regions).
narrative_ontology:constraint_victim(ai_talent_migration_restrictions, ai_researchers_from_restricted_regions).
narrative_ontology:constraint_victim(ai_talent_migration_restrictions, global_ai_capability_distribution).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EMERGING REGION AI RESEARCHER (SNARE) — Faces visa restrictions, credential non-recognition, and regulatory barriers to migration. High-skill workers in non-dominant AI regions are trapped by capital controls, visa quota systems, and bilateral restrictions. Zero meaningful exit options. Bears full extraction cost: brain drain to incumbent centers, loss of career mobility, suppression of local ecosystem development.
constraint_indexing:constraint_classification(ai_talent_migration_restrictions, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: LOCAL AI ECOSYSTEM DEVELOPER (TANGLED ROPE) — Within-country developer benefits from ecosystem coordination (shared talent pools, local networks, IP protection frameworks) while suffering extraction through brain drain (top talent leaves, reducing ecosystem critical mass). Constrained exit: can develop locally but faces talent loss and competitive disadvantage relative to incumbent centers.
constraint_indexing:constraint_classification(ai_talent_migration_restrictions, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INCUMBENT AI CENTER (ROPE) — Experiences migration restrictions as coordination of talent concentration. Visa systems, selective immigration policies, and credential recognition frameworks all funnel global talent toward established centers. Net beneficiary with high arbitrage capability: can pick top talent globally while excluding competitors' domestic talent pools. Sees restrictions as solving coordination problem of attracting and retaining the best researchers.
constraint_indexing:constraint_classification(ai_talent_migration_restrictions, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ELITE MOBILE RESEARCHER (TANGLED ROPE) — Top-tier researchers from any region face lower migration barriers (H1B exceptions, startup visa programs, direct recruitment). Still constrained by visa complexity and credential translation, but much lower than powerless cohort. Benefits from global talent market (multiple job offers, negotiating power) while constrained by administrative overhead. Mixed extraction: they enjoy benefits of mobility but still bear costs of complexity and restriction-induced scarcity premiums.
constraint_indexing:constraint_classification(ai_talent_migration_restrictions, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: NATIONAL SECURITY SCREENING APPARATUS (PITON) — Framed as coordination mechanism (vetting foreign talent, protecting national secrets, maintaining technological sovereignty). But the functional verification is degraded: security reviews are largely performative, theater_ratio high (checkbox compliance, blanket restrictions regardless of actual risk assessment). The apparatus persists through inertia and political theater rather than demonstrable security efficacy. Maintains restrictions because dismantling them is politically costly, not because the screening actually works.
constraint_indexing:constraint_classification(ai_talent_migration_restrictions, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: DISTRIBUTED AI DEVELOPMENT COALITION (SCAFFOLD) — Organized actors (remote-first companies, decentralized research networks, open-source communities) are building alternative coordination pathways that reduce migration dependence: distributed work enables talent to stay local while contributing to global research. Sunset dynamic: as remote collaboration, decentralized AI frameworks, and local funding improve, the extraction mechanism of geographic concentration weakens. Temporary constraint with visible exit pathway.
constraint_indexing:constraint_classification(ai_talent_migration_restrictions, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: EMERGING NATION STATE (TANGLED ROPE) — Coordinates domestic AI capability development (education, research funding, local hiring) while extracting through brain drain prevention policies (capital controls, work permit restrictions, emigration taxes). Also experiences extraction from incumbent centers that selectively attract its top talent. Complex hybrid: some genuine coordination of local AI capability, alongside asymmetric loss of human capital.
constraint_indexing:constraint_classification(ai_talent_migration_restrictions, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / FALSE SUMMIT VIEW (MOUNTAIN) — Risks naturalizing restrictions as inevitable feature of nation-state sovereignty and capital mobility constraints. Global talent flow is inherently limited by geographic friction, language barriers, family ties, and regulatory complexity — thus restrictions are 'natural'. However, structural evidence reveals contingent institutional choices (visa quota settings, credential recognition policies, security theater) rather than physical laws. False summit detection: this is social coordination framing temporary institutional arrangements as unchangeable.
constraint_indexing:constraint_classification(ai_talent_migration_restrictions, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_talent_migration_restrictions_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ai_talent_migration_restrictions, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ai_talent_migration_restrictions, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_talent_migration_restrictions, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ai_talent_migration_restrictions, TR),
    TR >= 0.70.

:- end_tests(ai_talent_migration_restrictions_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The restriction mechanism generates significant asymmetric benefit for incumbent centers (talent concentration) and cost for emerging regions (brain drain). The extractiveness is not as high as pure snare (0.70+) because some genuine coordination benefits exist (security vetting, credential harmonization reduce hiring risk) and because elite researchers have partial escape routes. The increasing trajectory (0.42 → 0.58) reflects geopolitical intensification and strengthening of screening apparatus. Suppression (0.68): High. Multiple compounding barriers: visa quotas create hard caps on migration, security screening creates indefinite delays and opacity, credential non-recognition creates professional penalties, and incumbent center recruitment directly extracts top talent from emerging regions. But suppression is not absolute (0.80+) because elite researchers can navigate barriers and some distributed alternatives are emerging. Theater_ratio (0.45): Moderate. Security screening component is substantially performative (checkbox compliance, blanket categories) but not dominant — actual visa allocation decisions reflect both security and economic/political preferences. The theater ratio has increased slightly (0.38 → 0.45) as security concerns have become more politicized, but remains lower than pure theatrical constraints (0.70+). Claimed_type (Tangled Rope) is correct: genuine coordination function (security vetting, credential standardization, local ecosystem development) coexists with asymmetric extraction (brain drain concentration, emerging region capability suppression).
 *
 * PERSPECTIVAL GAP:
 *   The gap between the incumbent center's rope perspective and the emerging researcher's snare perspective is the largest within the constraint. From the center's view (institutional/arbitrage), the restriction system coordinates talent attraction and retains local capability — a genuine coordination solution to the problem of attracting and keeping top researchers. The center sees themselves as solving a coordination game, not extracting. From the emerging researcher's view (powerless/trapped), the same system is pure extraction with no offsetting benefit — the researcher cannot access opportunity, pays opportunity cost, and receives no coordination benefit. This gap reveals a critical structural truth: the incumbent center's 'coordination' IS the emerging researcher's 'extraction.' There is no objective fact about whether restrictions coordinate or extract — it depends on whether you are the beneficiary (rope) or the target (snare). The analytical observer who naturalizes this as inevitable sovereignty (mountain) is choosing which gap to accept as permanent. The distributed coalition that builds alternatives (scaffold) is proposing to make the gap obsolete by decomposing geographic concentration's necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) derive from each agent's structural relationship to the extraction flow. Emerging region researchers (powerless/trapped) have d ≈ 0.95 (full target): they cannot exit, face maximum cost, and receive zero benefit. Incumbent centers (institutional/arbitrage) have d ≈ 0.10 (beneficiary with arbitrage): they extract value, have multiple exit options (could open immigration unilaterally if they chose), and benefit structurally. Local ecosystem developers (moderate/constrained) have d ≈ 0.60 (mixed): benefits from local coordination coordination (d down) but extraction from talent loss (d up). Elite researchers (powerful/mobile) have d ≈ 0.45 (near-symmetric): they experience costs (visa delay, credential friction) but have capacity to overcome them and benefit from scarcity premium on talent. These d values feed through f(d) sigmoid and scope modifier σ(S) to produce chi values. Incumbent centers with low d produce negative chi (perceived benefit exceeds cost), emerging region researchers with high d produce high chi (perceived cost is maximum). The directionality logic confirms tangled rope classification: asymmetric extraction (different d values for beneficiary vs victim) coexists with coordination function (security vetting, credential standardization produce genuine collective benefits).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in AI talent migration restrictions is between the 'security coordination' framing and the 'economic extraction' framing. National security advocates argue that restrictions coordinate important security and sovereignty goods: vetting hostile actors, protecting IP, maintaining technological independence. Economic advocates argue restrictions are protectionism dressed in security language: preventing brain drain, maintaining labor supply, protecting domestic talent markets. The engine must resolve this by recognizing that BOTH narratives are structurally present in the constraint. The security vetting (coordination) IS happening; the beneficiary concentration (extraction) IS happening. They are not mutually exclusive — this is exactly what makes the constraint tangled rope rather than pure rope (coordination) or pure snare (extraction). The mandatrophy is NOT resolved by choosing one narrative. It is resolved by recognizing that the constraint simultaneously coordinates (security vetting, credentialing) and extracts (talent concentration, capability suppression). The question becomes: what would need to change for the coordination function to persist while the extraction mechanism weakens? Answer: (1) security vetting becomes truly risk-based rather than blanket-categorical, (2) credential recognition improves so non-incumbent talent faces lower friction, (3) distributed alternatives mature so geographic concentration becomes optional rather than mandatory. These changes would shift classification from tangled rope toward rope (pure coordination) or scaffold (temporary coordination with sunset).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    security_vs_exclusion_ambiguity,
    'Are migration restrictions driven by genuine security risk assessment or by economic protectionism framed as security?',
    'Audit of security screening efficacy: correlation between screening outcomes and actual espionage/IP theft incidents; comparison of security risks across visa categories; analysis of screening stringency vs documented threat landscape',
    'If genuine security: suppression metric (0.68) reflects legitimate protective function; classification shifts toward rope (coordination). If protectionism: suppression reflects extraction mechanism masquerading as security; classification remains snare/tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_vs_exclusion_ambiguity, empirical, 'Whether security framing masks economic protectionism').

omega_variable(
    critical_mass_threshold_for_ecosystem_collapse,
    'At what brain drain rate do emerging AI ecosystems lose critical mass and enter irreversible decline?',
    'Time-series analysis of emerging region AI research output, funding levels, and talent concentration; modeling of feedback loops between talent loss and ecosystem vitality',
    'If threshold exceeded: constraint becomes catastrophic snare for emerging regions (ecosystem extinction). If threshold not reached: constraint is manageable tangled rope (coordinated capability development possible despite talent loss).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(critical_mass_threshold_for_ecosystem_collapse, empirical, 'Threshold for irreversible ecosystem collapse from brain drain').

omega_variable(
    remote_work_substitution_sufficiency,
    'Does distributed remote collaboration genuinely substitute for geographic co-location in cutting-edge AI research, or is cutting-edge work inherently dependent on physical proximity?',
    'Comparative analysis of remote-first AI research groups vs co-located groups: innovation speed, breakthrough frequency, training effectiveness, team retention; assessment of which research areas require physical proximity and which do not',
    'If substitution effective: scaffold sunset is real, restriction extraction declines over generational timescale. If substitution incomplete: remote alternative is aspirational, restrictions persist as binding constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remote_work_substitution_sufficiency, empirical, 'Whether remote collaboration substitutes for geographic co-location in AI research').

omega_variable(
    national_ai_sovereignty_necessity,
    'Is concentrated domestic AI capability development necessary for national technological sovereignty, or is it rationalization for rent-seeking?',
    'Historical case studies of AI capability distribution; analysis of whether nations with distributed talent pools retain sovereignty; comparison of outcomes under open vs closed migration policies',
    'If necessary: national restrictions coordinate genuine security/sovereignty goods (beneficiary logic valid). If rationalization: restrictions are pure extraction dressed in sovereignty language (snare classification confirmed).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(national_ai_sovereignty_necessity, conceptual, 'Whether AI sovereignty requires geographic concentration').

omega_variable(
    credential_portability_evolution,
    'Are international credential recognition frameworks improving fast enough to reduce suppression, or are they stagnating?',
    'Tracking of credential recognition agreements and their adoption; measurement of time-to-credentialing and cost differential across jurisdictions over time; comparative credential acceptance rates',
    'If improving: suppression (0.68) declines over interval; constraint lightens over time. If stagnating: suppression remains static; structural extraction persists.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(credential_portability_evolution, empirical, 'Trajectory of international credential recognition frameworks').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_talent_migration_restrictions, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aitm_tr_t0, ai_talent_migration_restrictions, theater_ratio, 0, 0.38).
narrative_ontology:measurement(aitm_tr_t5, ai_talent_migration_restrictions, theater_ratio, 5, 0.42).
narrative_ontology:measurement(aitm_tr_t10, ai_talent_migration_restrictions, theater_ratio, 10, 0.45).

% Extraction over time
narrative_ontology:measurement(aitm_be_t0, ai_talent_migration_restrictions, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(aitm_be_t5, ai_talent_migration_restrictions, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(aitm_be_t10, ai_talent_migration_restrictions, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_talent_migration_restrictions, enforcement_mechanism).
narrative_ontology:affects_constraint(ai_talent_migration_restrictions, ai_brain_drain_emerging_regions).
narrative_ontology:affects_constraint(ai_talent_migration_restrictions, geopolitical_ai_race_dynamics).
narrative_ontology:affects_constraint(ai_talent_migration_restrictions, talent_credential_recognition_barriers).

% DUAL FORMULATION NOTE:
% AI talent migration restrictions decompose into three structurally distinct constraint stories: (1) brain drain dynamics at emerging region level (ε ≈ 0.72, snare for local ecosystems), (2) migration restriction policy mechanism (ε ≈ 0.58, tangled rope at policy level), (3) credential portability barriers (ε ≈ 0.45, tangled rope at institutional recognition level). Each has distinct beneficiaries, victims, and measurement trajectories. This story addresses the policy mechanism; upstream stories address specific regional impacts and credential barriers.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_talent_migration_restrictions, organized, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
