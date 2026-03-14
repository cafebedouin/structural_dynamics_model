% ============================================================================
% CONSTRAINT STORY: global_south_knowledge_access
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_global_south_knowledge_access, []).

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
 *   constraint_id: global_south_knowledge_access
 *   human_readable: Global South Knowledge Access Restriction
 *   domain: knowledge_infrastructure/epistemic_justice
 *
 * SUMMARY:
 *   Global knowledge access through paywalled academic publishing represents
 *   a structural constraint that extracts wealth and epistemic resources from
 *   the Global South while claiming to coordinate scientific quality
 *   assurance. The constraint exhibits the core Tangled Rope signature:
 *   genuine coordination function (peer review, quality control, curation)
 *   layered with asymmetric extraction (subscription fees that
 *   developing-world institutions cannot afford, profit margins that exceed
 *   production costs, licensing restrictions on use and redistribution). The
 *   extractiveness has increased measurably over three decades as journal
 *   consolidation (six corporations control >50% of research publishing) has
 *   reduced competitive pressure and increased subscription costs annually at
 *   5-15% — far exceeding inflation or library budget growth. The theater
 *   ratio (0.58) reflects that much of the paywall's claimed necessity for
 *   quality control is performance: open-access and preprint platforms
 *   (arXiv, bioRxiv, medRxiv) demonstrate equivalent or superior quality
 *   signals through decentralized review, yet the paywalled system persists
 *   by leveraging institutional inertia (prestige, career advancement systems
 *   built around paywalled journals) and legal enforcement
 *   (copyright/licensing). The developing world researcher faces a snare:
 *   full participation in global science requires knowledge the paywalled
 *   system controls.
 *
 * KEY AGENTS:
 *   - Global South Researchers: Primary victims (powerless/trapped) — cannot afford subscription access; participation in knowledge production requires overcoming access barriers; no exit option
 *   - Developing World Research Institutions: Secondary victims (moderate/constrained) — face choice between subscription costs and research capacity; some negotiating power (consortial licensing) but insufficient to achieve fair terms
 *   - Paywalled Journal Systems: Primary beneficiary (institutional/arbitrage) — coordinate peer review and quality assurance; capture excess extraction through high margins and price discrimination; perceive open-access as existential threat
 *   - Global North Universities and Research Centers: Secondary beneficiary (institutional/arbitrage) — have fully subsidized access through institutional subscriptions; experience constraint as pure coordination; not directly exposed to extraction
 *   - Open Science Coalition: Organized agents (organized/constrained) — arXiv, Open Access Button, Plan S, funder mandates building alternative infrastructure with genuine sunset pathway; perceive paywall as temporary market failure being corrected
 *   - Copyright and IP Legal Framework: Institutional enforcement mechanism (institutional/arbitrage) — maintains paywall viability through legal enforcement of licensing restrictions; seen as enabling infrastructure by publishers but increasingly recognized as contingent policy choice
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing paywall as inevitable feature of knowledge production rather than contingent political-economic arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(global_south_knowledge_access, 0.58).
domain_priors:suppression_score(global_south_knowledge_access, 0.72).
domain_priors:theater_ratio(global_south_knowledge_access, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(global_south_knowledge_access, extractiveness, 0.58).
narrative_ontology:constraint_metric(global_south_knowledge_access, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(global_south_knowledge_access, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(global_south_knowledge_access, tangled_rope).
narrative_ontology:human_readable(global_south_knowledge_access, "Global South Knowledge Access Restriction").
narrative_ontology:topic_domain(global_south_knowledge_access, "knowledge_infrastructure/epistemic_justice").

domain_priors:requires_active_enforcement(global_south_knowledge_access).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(global_south_knowledge_access, global_north_publishers).
narrative_ontology:constraint_beneficiary(global_south_knowledge_access, paywalled_journal_systems).
narrative_ontology:constraint_beneficiary(global_south_knowledge_access, subscription_licensing_intermediaries).
narrative_ontology:constraint_victim(global_south_knowledge_access, global_south_researchers).
narrative_ontology:constraint_victim(global_south_knowledge_access, developing_world_institutions).
narrative_ontology:constraint_victim(global_south_knowledge_access, epistemic_justice).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEVELOPING WORLD RESEARCHER (SNARE) — Trapped by institutional poverty and currency barriers. Cannot afford subscription fees (often $30-40 per article or $5,000+ annual institutional subscriptions). No meaningful exit option: participation in global knowledge production requires access to published literature. Maximum extraction experienced. Cannot organize effectively across dispersed low-resource institutions.
constraint_indexing:constraint_classification(global_south_knowledge_access, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DEVELOPING NATION RESEARCH INSTITUTION (TANGLED ROPE) — Constrained by budget limitations and political priority competition. Genuinely benefits from journal coordination (access enables research participation, publication pipelines, citations). Also bears asymmetric extraction through escalating subscription costs (annually 5-15% increases) and licensing restrictions. Has some negotiating power (consortial licensing, government advocacy) but insufficient to achieve fair terms.
constraint_indexing:constraint_classification(global_south_knowledge_access, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PAYWALLED JOURNAL SYSTEM (ROPE) — Benefits from coordinating peer review, quality assurance, and distribution. Experiences the constraint as pure coordination problem: managing access controls enables sustainable operation (they claim). Net beneficiary — extraction flows toward this actor. Has exit option (open-access transition) but perceives it as economically threatening rather than viable.
constraint_indexing:constraint_classification(global_south_knowledge_access, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: GLOBAL NORTH RESEARCH CONSORTIUM (ROPE) — Primary beneficiary. Well-funded institutions negotiate favorable site licenses, provide researchers unlimited institutional access. Experiences the constraint as coordination mechanism (centralized publishing enables citation networks, quality control, career advancement through prestigious venue publication). Extraction does not touch this actor — they are subsidized by the system.
constraint_indexing:constraint_classification(global_south_knowledge_access, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: OPEN SCIENCE MOVEMENT (SCAFFOLD) — Organized agents (arXiv, Open Access Button, Plan S, institutional repositories) see this as a temporary structural failure being solved through alternative architectures. Has genuine sunset: preprints, open-access mandates, and funder requirements are building parallel knowledge infrastructure. Experiences suppression from established publishers but has agency and exit pathways (institutional repositories, funder mandates, open licenses). Effective extraction is moderate because the organizing coalition perceives viable alternatives.
constraint_indexing:constraint_classification(global_south_knowledge_access, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: COPYRIGHT/IP LEGAL FRAMEWORK (PITON) — The legal apparatus (copyright duration, patent protections, licensing restrictions) persists largely through institutional inertia. Its coordination function (incentivizing knowledge production through exclusivity) is partially atrophied — much research is publicly funded yet publicly inaccessible. Theater is high (copyright framed as necessary incentive, but empirical evidence shows open access increases citations and impact). The legal framework maintains access restrictions through enforcement of outdated IP norms rather than genuine functional necessity.
constraint_indexing:constraint_classification(global_south_knowledge_access, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a naturalization frame, knowledge access restriction appears inevitable: production of high-quality research requires funding, peer review infrastructure, and distribution systems; these systems require revenue streams; paywalling is a natural market mechanism. This perspective risks false summitry — it naturalizes a contingent political choice (centralized paywall model) as an immutable feature of knowledge production.
constraint_indexing:constraint_classification(global_south_knowledge_access, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(global_south_knowledge_access_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(global_south_knowledge_access, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(global_south_knowledge_access, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(global_south_knowledge_access, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(global_south_knowledge_access, TR),
    TR >= 0.70.

:- end_tests(global_south_knowledge_access_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and increasing. The paywall mechanism has genuine coordination value (peer review, editorial curation, distribution infrastructure cost ~$5-10k per article in traditional publishing), but captured rents far exceed coordination cost. Publisher profit margins of 30-40% vs. 10-15% norms indicate substantial excess extraction. The increase over time (0.35 → 0.58) reflects consolidation of publishing market (six corporations control >50%) reducing competitive pressure and enabling annual price increases of 5-15%. Suppression (0.72): High. Multiple structural barriers prevent exit: (a) Institutional barriers — prestige and career advancement systems built around paywalled venue publication; researchers cannot optimize for quality alone; (b) Economic barriers — institutional budgets cannot afford alternative systems; transitioning to open-access requires coordinated funding model change; (c) Legal barriers — copyright and IP frameworks enforce licensing restrictions; (d) Network barriers — researchers must follow where funding bodies and career systems lead; defection costs are high. Theater ratio (0.58): Moderate and increasing. The claimed necessity for paywall model (funding quality peer review, maintaining standards) is partly theatrical — open-access systems demonstrably provide equivalent quality signals. The theater increases as the open-access alternative becomes more viable and more clearly comparable in quality metrics. Claimed type (Tangled Rope): Required beneficiary (paywalled systems), victims (developing world researchers), and active enforcement (legal/licensing structures) all present.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximal perspectival divergence. The developing world researcher sees a snare — their exit is structurally blocked and the constraint extracts from them without offering proportional coordination benefit. The developing nation institution sees a tangled rope — both genuine coordination benefit (access enables participation, citations, prestige) and asymmetric extraction (unsustainable costs). The Global North university sees pure coordination (rope) — the constraint solves their knowledge distribution problem with no experienced extraction. The paywall system sees coordination (rope) — it genuinely solves the problem of funding peer review and distribution. The open science movement sees a temporary problem (scaffold) — arXiv, preprints, and open-access mandates are building alternative infrastructure with a real sunset timeline (15-25 years). The legal IP framework sees itself as enabling necessary coordination (rope), but the analytical observer sees the IP framework itself as performative (piton) — maintaining outdated restrictions through institutional inertia rather than functional necessity. The false summit (mountain) naturalizes paywall as inherent to knowledge production, ignoring the contingency of the current institutional arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation chain produces high d values (high extraction experienced) for developing-world researchers because they are victims with trapped exit options: they cannot afford access and have no alternative pathway to knowledge. The Global North institution derives low d (benefits without extraction cost) because they are beneficiaries with arbitrage options: they can negotiate institutional licenses or transition to open access without bearing structural costs. The paywalled system derives low d (benefits from coordination function) because they are institutional beneficiaries with arbitrage exit options. The developing nation institution derives moderate d (some extraction, some benefit) because they are both victims (bearing cost burden) and partial beneficiaries (gaining access and prestige). The open science coalition derives moderate d (constrained exit, but exits are viable) because they can build alternative infrastructure though facing resistance. The legal framework derives low d (enables extraction for others) as an institutional mechanism rather than direct actor. These derivations explain why the same constraint classifies as snare from one perspective, tangled rope from another, and rope from yet another — the structural extraction flow runs toward and away from different agents depending on their position in the system.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVES: The mandatrophy is resolved by recognizing that the constraint is genuinely hybrid. The paywalled system does coordinate (peer review, quality assurance, distribution infrastructure are real coordination functions). It also genuinely extracts (publisher profit margins, price discrimination, geographic access barriers are real extraction). The classification as Tangled Rope is correct precisely because both components are structural, not because one is naturalized or misnamed. The perspectival gap reveals that the mix is experienced differently: Global North sees pure coordination because they are subsidized; Global South sees pure extraction because they bear the cost. The false summit (analytical/mountain) is the error — it naturalizes the paywall as inevitable when the coordination function could be performed by alternative systems with lower extraction. The scaffold perspective (open science with sunset) and piton perspective (IP framework with theater) together provide diagnostic specificity: the paywall persists not because it is functionally necessary but because it is legally protected (copyright/licensing) and institutionally entrenched (career systems built around paywalled journals). The constraint is not an immutable feature of knowledge production — it is a contingent institutional arrangement being actively challenged by viable alternatives. Resolving the mandatrophy requires distinguishing the coordination function (real, worth maintaining) from the current institutional apparatus (contingent, replaceable).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    knowledge_production_necessity,
    'What fraction of the observed paywall extraction is necessary to fund legitimate research infrastructure vs. what fraction is rent-seeking profit accumulation?',
    'Comparative accounting: journal production costs vs. subscription revenue; open-access journal sustainability analysis; publisher profit margins (typically 30-40% vs. scientific publishing norms of 10-15%)',
    'If > 60% is rent-seeking: constraint reclassifies from Tangled Rope toward Snare (pure extraction with minimal coordination). If < 30% is rent-seeking: coordination function is genuine, classification holds as Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(knowledge_production_necessity, empirical, 'Proportion of paywall revenue that funds coordination vs. captures economic rent').

omega_variable(
    open_access_sustainability,
    'Can open-access models (preprints, government-funded repositories, author-fee models) actually sustain the quality-control and distribution infrastructure at global scale?',
    'Empirical tracking of open-access journal quality metrics (citation impact, retraction rates, peer review rigor); cost accounting for Plan S and similar mandates; long-term stability analysis of non-profit open platforms',
    'If sustainable: scaffold perspective is confirmed, sunset timeline is real (15-25 years for full transition). If not: open-access represents aspirational ideology without structural viability; scaffold reclassifies toward rope (coordination without real exit path).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(open_access_sustainability, empirical, 'Whether open-access models can sustainably replace paywalled publishing at scale').

omega_variable(
    epistemic_harm_quantification,
    'What is the epistemic damage (slower science, biased research agendas, missed discoveries) from Global South researchers lacking full knowledge access, and does it exceed the coordination benefits of centralized quality control?',
    'Bibliometric analysis of citation patterns and research diversity; comparative innovation rates by access level; survey of researcher populations on how access restrictions affect research direction and collaboration',
    'If epistemic harm significantly exceeds coordination benefit: classification moves toward Snare from developing world perspective. If harm is minor: Tangled Rope classification is accurate — extraction cost is balanced against genuine coordination value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epistemic_harm_quantification, empirical, 'Epistemic harm from access restrictions vs. coordination benefits of quality control').

omega_variable(
    alternative_peer_review_viability,
    'Can decentralized peer review systems (post-publication review, open commenting, reputation-based quality signals) provide equivalent quality assurance to traditional paywalled journal gatekeeping?',
    'Comparative analysis of retraction rates, fraud detection rates, and impact factor correlations across traditional journals vs. open-review platforms; longitudinal study of preprint quality evolution',
    'If alternative systems work: paywall''s coordination function is dispensable; reclassifies toward Snare. If traditional gatekeeping is necessary: coordination function is genuine; Tangled Rope holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_peer_review_viability, empirical, 'Whether alternative peer review models provide equivalent quality assurance').

omega_variable(
    global_north_subsidy_dependency,
    'To what extent does the paywall model depend on Global North institutional subscription revenue vs. how much could it function on open-access author fees alone?',
    'Financial modeling of journal economics; analysis of which journals have successfully transitioned to full open-access; comparison of subscription revenue vs. article processing charges (APCs) in hybrid/open journals',
    'If Global North subsidizes Global South access through high institutional fees: paywall is a North-South transfer mechanism wearing the mask of market coordination. If model is symmetric: less asymmetric extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(global_north_subsidy_dependency, empirical, 'Dependence of paywall model on Global North subscription revenue').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(global_south_knowledge_access, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gsouthka_tr_t0, global_south_knowledge_access, theater_ratio, 0, 0.42).
narrative_ontology:measurement(gsouthka_tr_t10, global_south_knowledge_access, theater_ratio, 10, 0.52).
narrative_ontology:measurement(gsouthka_tr_t20, global_south_knowledge_access, theater_ratio, 20, 0.58).
narrative_ontology:measurement(gsouthka_tr_t30, global_south_knowledge_access, theater_ratio, 30, 0.63).

% Extraction over time
narrative_ontology:measurement(gsouthka_be_t0, global_south_knowledge_access, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(gsouthka_be_t10, global_south_knowledge_access, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(gsouthka_be_t20, global_south_knowledge_access, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(gsouthka_be_t30, global_south_knowledge_access, base_extractiveness, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(global_south_knowledge_access, information_standard).
narrative_ontology:affects_constraint(global_south_knowledge_access, research_prestige_hierarchy).
narrative_ontology:affects_constraint(global_south_knowledge_access, global_innovation_capacity).
narrative_ontology:affects_constraint(global_south_knowledge_access, developing_world_institutional_capacity).

% DUAL FORMULATION NOTE:
% The global south knowledge access constraint represents extraction through institutional arrangements rather than direct physical barriers. It is upstream of specific research outcome constraints (innovation rates, institutional capacity building) whose extractiveness depends partly on this knowledge access bottleneck. The paywall acts as a gating constraint on broader epistemic justice dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(global_south_knowledge_access, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
