% ============================================================================
% CONSTRAINT STORY: open_source_ai_development_diffusion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_open_source_ai_development_diffusion, []).

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
 *   constraint_id: open_source_ai_development_diffusion
 *   human_readable: Open Source AI Development Diffusion
 *   domain: artificial_intelligence/governance
 *
 * SUMMARY:
 *   Open source AI development presents a constraint structure where the
 *   rhetoric of democratization masks asymmetric access to computational
 *   resources required for competitive model training. The constraint
 *   exhibits characteristics of a tangled rope: genuine coordination
 *   mechanisms (distributed development, rapid architectural innovation,
 *   broad model access) coexist with extraction mechanisms (resource barriers
 *   that concentrate training capability, free labor absorption into
 *   proprietary products, infrastructure rent concentration). The diffusion
 *   of open source models is constrained not by licensing or access controls
 *   but by the massive computational requirements for training competitive
 *   foundation models, which only well-resourced entities can afford. This
 *   creates a structural dependency where open source contribution appears
 *   voluntary but occurs within a framework that systematically advantages
 *   well-capitalized actors. The theater ratio is moderate (0.48) — the
 *   constraint involves genuine technical coordination but also performative
 *   elements where model releases create appearance of democratization while
 *   practical deployment remains concentrated.
 *
 * KEY AGENTS:
 *   - Large AI Companies: Primary beneficiaries (institutional/arbitrage) — capture architectural innovation from open source, use it to improve proprietary products, deploy at scale with infrastructure advantages
 *   - Cloud Infrastructure Providers: Secondary beneficiaries (institutional/arbitrage) — infrastructure rent extracted from model training and inference workloads driven by open source ecosystem
 *   - Independent Open Source Developers: Primary victims (powerless/trapped) — provide free labor maintaining compatibility layers and implementing community contributions with no institutional support
 *   - Developing Country AI Capacity: Primary victims (powerless/trapped) — locked out of competitive model training by resource requirements despite open source rhetoric of democratization
 *   - Academic Research Groups: Secondary victims/partial beneficiaries (moderate/constrained) — benefit from access to models but bear costs of dependency on proprietary infrastructure and limited ability to customize
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees both genuine coordination and structural extraction operating simultaneously
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(open_source_ai_development_diffusion, 0.58).
domain_priors:suppression_score(open_source_ai_development_diffusion, 0.62).
domain_priors:theater_ratio(open_source_ai_development_diffusion, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(open_source_ai_development_diffusion, extractiveness, 0.58).
narrative_ontology:constraint_metric(open_source_ai_development_diffusion, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(open_source_ai_development_diffusion, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(open_source_ai_development_diffusion, tangled_rope).
narrative_ontology:human_readable(open_source_ai_development_diffusion, "Open Source AI Development Diffusion").
narrative_ontology:topic_domain(open_source_ai_development_diffusion, "artificial_intelligence/governance").

domain_priors:requires_active_enforcement(open_source_ai_development_diffusion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(open_source_ai_development_diffusion, large_ai_companies).
narrative_ontology:constraint_beneficiary(open_source_ai_development_diffusion, academic_research_institutions).
narrative_ontology:constraint_beneficiary(open_source_ai_development_diffusion, cloud_infrastructure_providers).
narrative_ontology:constraint_victim(open_source_ai_development_diffusion, developing_country_ai_capacity).
narrative_ontology:constraint_victim(open_source_ai_development_diffusion, independent_researchers).
narrative_ontology:constraint_victim(open_source_ai_development_diffusion, open_source_maintainers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDEPENDENT OPEN SOURCE DEVELOPER (SNARE) — Trapped in a coordination structure ostensibly open but requiring massive computational resources (GPU clusters, training infrastructure) that only well-funded entities can afford. Bears full cost of maintaining compatibility with proprietary ecosystems while receiving no institutional support. Cannot exit: the AI development paradigm has become synonymous with resource-intensive training.
constraint_indexing:constraint_classification(open_source_ai_development_diffusion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DEVELOPING COUNTRY AI SOVEREIGNTY (SNARE) — Open source rhetoric promises AI democratization but the practical requirement for massive compute infrastructure locks developing nations into dependency on cloud providers and API-based models owned by wealthy countries. Training a competitive foundation model requires resources beyond most national budgets. The open source framing obscures this structural trap.
constraint_indexing:constraint_classification(open_source_ai_development_diffusion, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: ACADEMIC RESEARCH GROUP (TANGLED ROPE) — Benefits from open source models (reduced development overhead, ability to publish on top of shared foundations) while bearing extraction costs (dependency on proprietary infrastructure, limited ability to customize for non-commercial use cases, pressure to contribute improvements without compensation). Constrained by career incentives that reward published results more than systems maintenance.
constraint_indexing:constraint_classification(open_source_ai_development_diffusion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: LARGE AI COMPANY (ROPE) — Captures genuine coordination benefits: open source models provide training data annotation, architectural innovation, and competitive pressure that improves internal capabilities. Also captures asymmetric advantage: can fine-tune released models with proprietary datasets, deploy at scale with infrastructure advantages, and absorb open source innovation into closed products. Arbitrage exit: can shift between open and closed strategies based on market conditions.
constraint_indexing:constraint_classification(open_source_ai_development_diffusion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CLOUD INFRASTRUCTURE PROVIDER (ROPE) — Benefits from open source ecosystem requiring computational resources; entire market for fine-tuning, inference, and custom deployment depends on their infrastructure. Coordination function is genuine: open source models drive demand for compute services. Can arbitrage between supporting open source (cheap labor in model development) and proprietary solutions (margins on closed services).
constraint_indexing:constraint_classification(open_source_ai_development_diffusion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — Observes genuine coordination function (distributed development, rapid iteration, broad access to models) alongside asymmetric extraction (resource barriers prevent most agents from training competitive models, open source contribution provides free labor that benefits well-resourced entities, cloud platforms capture infrastructure rent). Neither pure coordination nor pure extraction: both mechanisms operate simultaneously with different beneficiaries.
constraint_indexing:constraint_classification(open_source_ai_development_diffusion, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(open_source_ai_development_diffusion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(open_source_ai_development_diffusion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(open_source_ai_development_diffusion, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(open_source_ai_development_diffusion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(open_source_ai_development_diffusion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): The constraint shows increasing extraction over time. Initially (0.32) open source was primarily coordination with modest resource requirements. Over 8 years, as foundation models have grown larger and more resource-intensive, extractiveness has increased as a growing gap emerges between access to code and access to computational resources for training. Current extractiveness reflects the systematic barrier created by scaling requirements. Suppression (0.62): High. Meaningful barriers prevent most independent developers and developing nations from training competitive models: cost of GPU infrastructure ($millions for competitive training), data acquisition and licensing complexity, tacit knowledge concentrated in large labs, and career incentives that reward proprietary work over open maintenance. Theater ratio (0.48): Moderate. The constraint involves genuine technical coordination (distributed development, architectural innovation) but includes performative elements: model releases that require massive compute to use, democratization rhetoric that masks resource concentration, licensing that appears to constrain but often does not prevent proprietary incorporation.
 *
 * PERSPECTIVAL GAP:
 *   Large AI companies perceive open source as enabling coordination that accelerates their internal development (Rope perspective). Independent developers perceive extraction combined with lack of viable exit options (Snare perspective). Developing nations perceive structural lock-in despite open source framing (Snare perspective). Academic researchers perceive mixed coordination benefits and infrastructure dependency (Tangled Rope perspective). Cloud providers perceive pure coordination that drives their infrastructure business (Rope perspective). The analytical observer recognizes both genuine coordination and systematic extraction operating on different axes — neither dominates completely, defining the tangled rope classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each actor's structural position relative to resource concentration. Large AI companies and cloud providers (beneficiaries with arbitrage exit) experience low or negative directionality — the constraint flows toward them, subsidizing their operations with free architectural innovation and development labor. Independent developers and developing nations (victims trapped by resource barriers) experience high directionality — they bear costs of contribution without corresponding benefit. Academic researchers (constrained by infrastructure dependency) experience moderate-to-high directionality reflecting partial extraction despite some benefits. The constraint's effectiveness depends on suppression of alternatives to centralized model training, which is maintained through both technical barriers (scaling efficiency of centralized infrastructure) and institutional factors (publication incentives, career structures, cloud platform pricing structures).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy tension between coordination and extraction by showing that both mechanisms are structurally real. The coordination function (distributed development, rapid iteration, broad model access) is genuine and beneficial. The extraction function (resource concentration, labor absorption, infrastructure rent) is also genuine and systematic. Neither can be dismissed as appearance or secondary effect. The constraint is tangled rope precisely because mislabeling would obscure critical dynamics: calling it pure rope would miss the structural lock-in that prevents developing nations and independent developers from training competitive models; calling it pure snare would miss the real coordination benefits that accelerated AI development for all participants. The mandatrophy resolves by accepting that both mechanisms coexist and that the perspectival gap reveals which agents experience which mechanism as primary. The constraint is extractive when viewed from powerless perspectives and coordinative when viewed from beneficiary perspectives — and both are true descriptions of the same structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    resource_democratization_counterfactual,
    'Would decentralized compute infrastructure (proof-of-work, federated learning, distributed training) materially reduce resource barriers to model training, or are centralized data centers inherently more efficient?',
    'Empirical comparison of decentralized vs centralized training efficiency; cost per FLOP for competitive models; adoption rates of distributed training frameworks',
    'If decentralized infrastructure is viable at comparable cost: snare classification weakens to tangled_rope (exit becomes available). If centralized dominates: snare classification strengthens and becomes structural rather than contingent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_democratization_counterfactual, empirical, 'Whether decentralized compute can compete with centralized infrastructure').

omega_variable(
    open_source_labor_substitution,
    'Do open source contributions substitute for proprietary R&D labor, or do they primarily supplement internal development (hence not extractive)?',
    'Comparison of development velocity for open models vs closed models; correlation between open source commits and corresponding closed-product improvements; wage differential analysis for similar work in open vs proprietary contexts',
    'If substitute: extraction of developer labor is real and suppression is enforced by job market dynamics (identification_locked for many developers). If supplement: open source creates genuine value addition without labor extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(open_source_labor_substitution, empirical, 'Whether open source contributions substitute for proprietary labor').

omega_variable(
    model_weight_accessibility_paradox,
    'Does releasing model weights without computational resources for inference actually democratize AI or merely create the appearance of democratization?',
    'Cost analysis: ratio of model weight release cost to inference infrastructure cost; usability metrics for users attempting to deploy released models; tracking of actual deployment distribution (who actually runs released models vs who only accesses via proprietary APIs)',
    'If appearance without substance: entire constraint is theater masking resource concentration (theater_ratio should be higher). If weights enable genuine decentralized deployment: suppression is lower than measured and exit becomes more available.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(model_weight_accessibility_paradox, empirical, 'Whether model weight release enables genuine accessibility or creates theatrical democratization').

omega_variable(
    licensing_enforcement_gap,
    'Can open source licenses (MIT, Apache, GPL) effectively prevent proprietary incorporation of open models, or do they function as requests rather than binding constraints?',
    'Analysis of license violation prevalence; cost of enforcement vs cost of violation; tracking of proprietary product improvement from licensed materials without attribution',
    'If enforcement is real: licensing creates meaningful constraints on extraction (suppression higher than measured). If enforcement is symbolic: licenses provide moral cover for appropriation, increasing effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(licensing_enforcement_gap, empirical, 'Enforceability of open source licensing constraints').

omega_variable(
    capability_overhang_distribution,
    'When a foundation model release outpaces most practitioners'' ability to productize it, does the overhang accumulate rent-extraction potential for well-resourced entities, or does distribution of that potential matter for classification?',
    'Timeline analysis: lag between model release and typical productive deployment by different classes of actors; correlation between resource availability and capability-to-use ratio',
    'If overhang is systematically exploited by well-resourced entities: extractiveness should be higher. If overhang distributes randomly: extraction is lower than measured.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capability_overhang_distribution, empirical, 'Distribution of capability overhang in AI model diffusion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(open_source_ai_development_diffusion, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(osaid_tr_t0, open_source_ai_development_diffusion, theater_ratio, 0, 0.35).
narrative_ontology:measurement(osaid_tr_t4, open_source_ai_development_diffusion, theater_ratio, 4, 0.42).
narrative_ontology:measurement(osaid_tr_t8, open_source_ai_development_diffusion, theater_ratio, 8, 0.48).

% Extraction over time
narrative_ontology:measurement(osaid_be_t0, open_source_ai_development_diffusion, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(osaid_be_t4, open_source_ai_development_diffusion, base_extractiveness, 4, 0.45).
narrative_ontology:measurement(osaid_be_t8, open_source_ai_development_diffusion, base_extractiveness, 8, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(open_source_ai_development_diffusion, global_infrastructure).
narrative_ontology:affects_constraint(open_source_ai_development_diffusion, ai_compute_concentration).
narrative_ontology:affects_constraint(open_source_ai_development_diffusion, developing_nation_technology_sovereignty).
narrative_ontology:affects_constraint(open_source_ai_development_diffusion, open_source_contribution_labor_extraction).

% DUAL FORMULATION NOTE:
% Open source AI development is downstream of broader compute infrastructure concentration (affects_constraints: ai_compute_concentration). The constraint can be partially decomposed: genuine architectural coordination (higher ε, lower suppression) operates separately from infrastructure barriers to deployment (lower ε, higher suppression). However, the ε-invariance principle suggests these are aspects of a single constraint where the observable is 'can a typical developer train a competitive model' — this observable determines both extractiveness and suppression values, so they should not be decomposed into separate stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(open_source_ai_development_diffusion, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
