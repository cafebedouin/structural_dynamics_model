% ============================================================================
% CONSTRAINT STORY: behavioral_epidemiology_funding
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_behavioral_epidemiology_funding, []).

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
 *   constraint_id: behavioral_epidemiology_funding
 *   human_readable: Behavioral Epidemiology Funding Constraint
 *   domain: public_health/epidemiology/behavioral_science
 *
 * SUMMARY:
 *   Behavioral epidemiology funding represents a structural constraint where
 *   research funding allocation, peer review standards, and evidence
 *   hierarchies systematically privilege individual behavior change
 *   interventions over population-level structural approaches to disease
 *   prevention. This constraint exhibits a tangled hybrid of genuine
 *   coordination function (establishing rigorous standards for behavioral
 *   trials) and asymmetric extraction (funding patterns that marginalize
 *   prevention and structural epidemiologists while concentrating resources
 *   toward interventions that benefit pharmaceutical industry and behavioral
 *   researchers). The constraint operates through active enforcement
 *   mechanisms: grant review criteria that reward RCT methodology,
 *   publication bias against null results for structural interventions, and
 *   career incentive structures that direct ambitious researchers toward
 *   behavioral work. Over the past decade (interval 0-10), the theater ratio
 *   has increased from 0.48 to 0.68 as intervention effectiveness has
 *   declined at the population level while funding has continued to flow
 *   toward behavioral approaches, suggesting performative maintenance of the
 *   funding allocation by institutional inertia rather than empirical
 *   support. The suppression mechanism operates asymmetrically: underserved
 *   populations are trapped in perpetual intervention pilots with no exit
 *   option, while pharmaceutical industry and behavioral researchers have
 *   arbitrage options (consulting, partnerships, alternative funding).
 *
 * KEY AGENTS:
 *   - Underserved Populations: Primary victim (powerless/trapped) — subject of behavioral interventions with no exit option; provides behavioral data extracted for research publication while structural conditions remain unchanged
 *   - Prevention-Focused Epidemiologists: Secondary victim (moderate/constrained) — career-constraining barriers to structural research; benefit from epidemiological infrastructure but experience extraction through funding disadvantage
 *   - Behavioral Intervention Researchers: Primary beneficiary (institutional/arbitrage) — funding flows toward their designs; they can exit to pharmaceutical partnerships; they define valid epidemiological evidence
 *   - Pharmaceutical Industry: Primary beneficiary (institutional/arbitrage) — behavioral epidemiology funding generates markets for behavioral interventions; high arbitrage options
 *   - Public Health Agencies: Mixed (organized/constrained) — benefit from federal behavioral intervention funding but trapped implementing programs with limited population impact; constrained exit to structural work
 *   - Public Health Advocacy Coalition: Organized actor (organized/constrained) — perceive scaffold dynamics; building alternative evidence pathways; constrained but with sunset visibility
 *   - Individual Behavior Change Paradigm: Institutional frame (institutional/arbitrage) — maintained by inertia despite declining empirical support; shows piton characteristics at civilizational scale
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(behavioral_epidemiology_funding, 0.58).
domain_priors:suppression_score(behavioral_epidemiology_funding, 0.65).
domain_priors:theater_ratio(behavioral_epidemiology_funding, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(behavioral_epidemiology_funding, extractiveness, 0.58).
narrative_ontology:constraint_metric(behavioral_epidemiology_funding, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(behavioral_epidemiology_funding, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(behavioral_epidemiology_funding, tangled_rope).
narrative_ontology:human_readable(behavioral_epidemiology_funding, "Behavioral Epidemiology Funding Constraint").
narrative_ontology:topic_domain(behavioral_epidemiology_funding, "public_health/epidemiology/behavioral_science").

domain_priors:requires_active_enforcement(behavioral_epidemiology_funding).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(behavioral_epidemiology_funding, pharmaceutical_industry).
narrative_ontology:constraint_beneficiary(behavioral_epidemiology_funding, behavioral_intervention_researchers).
narrative_ontology:constraint_beneficiary(behavioral_epidemiology_funding, public_health_institutions_promoting_interventions).
narrative_ontology:constraint_victim(behavioral_epidemiology_funding, population_level_epidemiological_evidence).
narrative_ontology:constraint_victim(behavioral_epidemiology_funding, low_income_populations).
narrative_ontology:constraint_victim(behavioral_epidemiology_funding, prevention_focused_researchers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNDERSERVED POPULATION (SNARE) — Trapped within systems designed to measure and modify behavior rather than address root causes. Population-level epidemiological evidence indicates structural determinants (poverty, housing, discrimination) drive health outcomes, but funding flows toward individual behavioral interventions. No exit mechanism exists for populations caught in perpetual intervention pilot programs; their behavioral data is extracted for research publication while structural conditions remain unchanged.
constraint_indexing:constraint_classification(behavioral_epidemiology_funding, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PREVENTION-FOCUSED EPIDEMIOLOGISTS (TANGLED ROPE) — Constrained by funding allocation that prioritizes intervention trials over structural analysis. These researchers benefit from the epidemiological infrastructure and peer network but experience extraction as their prevention-focused work struggles for resources. The constraint enforces a particular model of evidence (RCTs of behavioral interventions) while suppressing alternative models (population surveillance, structural analysis). Moderate experienced extraction due to career barriers but some agency through alternative publication venues.
constraint_indexing:constraint_classification(behavioral_epidemiology_funding, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: BEHAVIORAL INTERVENTION RESEARCHERS (ROPE) — Net beneficiaries with arbitrage options. Funding flows toward their research designs; their work defines what counts as valid epidemiological evidence; they can exit to pharmaceutical partnerships or consulting. They experience the constraint as pure coordination: establishing rigorous standards for behavioral interventions enables knowledge accumulation and funding multiplier effects. This perspective sees genuine coordination function without apparent asymmetric extraction.
constraint_indexing:constraint_classification(behavioral_epidemiology_funding, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PHARMACEUTICAL INDUSTRY (ROPE) — Primary beneficiary. Behavioral epidemiology funding generates markets for behavioral interventions (medication adherence apps, digital therapeutics, incentive-based compliance programs). Industry arbitrage options are high — they can shift between epidemiological evidence and marketing claims with minimal friction. Experience the constraint as coordination of market demand, not extraction. Funding allocation toward behavioral interventions expands their addressable market.
constraint_indexing:constraint_classification(behavioral_epidemiology_funding, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: PUBLIC HEALTH ADVOCACY COALITION (SCAFFOLD) — Organized actors (community health organizations, environmental health advocates, social determinants researchers) perceive this constraint as a temporary funding allocation problem solvable through institutional reform. They see the bottleneck as scaffolding: the current behavioral epidemiology emphasis is a transitional stage before structural epidemiology frameworks gain institutional recognition. Sunset logic applies if funding mechanisms shift toward root-cause analysis and health equity frameworks. Suppression is significant but sunset is visible — advocacy has built alternative evidence pathways (DHHS health equity initiatives, structural racism research funding).
constraint_indexing:constraint_classification(behavioral_epidemiology_funding, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: INDIVIDUAL BEHAVIOR CHANGE PARADIGM (PITON) — At the civilizational timescale, the individual behavior change model that dominates epidemiological funding has become substantially theatrical. Decades of evidence show behavior change interventions have minimal population-level impact on major health disparities without structural changes. Yet funding mechanisms, grant review criteria, and publication standards continue to enforce the paradigm through institutional inertia rather than empirical support. The constraint is maintained by the perception that 'we must at least try behavioral approaches' despite declining real-world impact.
constraint_indexing:constraint_classification(behavioral_epidemiology_funding, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: PUBLIC HEALTH AGENCIES (TANGLED ROPE) — Institutional beneficiaries and victims simultaneously. They benefit from federal funding directed toward behavioral intervention implementation and evaluation, which provides resources and legitimacy. Simultaneously, they are trapped implementing programs they know have limited population-level impact because alternative funding pathways (structural interventions, housing, economic development) require political capital they lack. Constrained exit options — agencies cannot easily shift to structural work without losing behavioral funding streams. Active enforcement through grant mechanisms and performance metrics.
constraint_indexing:constraint_classification(behavioral_epidemiology_funding, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / FALSE NATURAL LAW VIEW (MOUNTAIN) — Risk of classifying the constraint as a natural law of public health: 'behavior change is inherent to disease prevention.' This naturalizes what is actually a contingent funding allocation driven by historical (tobacco control success), pharmaceutical industry interests, and methodological preferences (RCTs favor interventions over structural analysis). The engine's false summit detector will identify this as naturalization of an institutional arrangement rather than a law of epidemiology.
constraint_indexing:constraint_classification(behavioral_epidemiology_funding, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(behavioral_epidemiology_funding_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(behavioral_epidemiology_funding, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(behavioral_epidemiology_funding, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(behavioral_epidemiology_funding, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(behavioral_epidemiology_funding, TR),
    TR >= 0.70.

:- end_tests(behavioral_epidemiology_funding_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts from underserved populations (no exit, provide behavioral data) and from prevention-focused researchers (funding barriers), while benefiting pharmaceutical industry and behavioral researchers. The extraction is not maximal because some coordination function is genuine (establishing rigorous standards for behavioral trials), but the asymmetry is pronounced. The extractiveness value increased over the interval (0.35 → 0.58) as evidence accumulated that behavior change interventions have limited population impact, yet funding continued to flow toward them. Suppression (0.65): High. Multiple suppression mechanisms: underserved populations lack alternatives to behavioral intervention participation; prevention researchers lack funding for structural work; alternative evidence models (population surveillance, structural analysis) are marginalized by peer review; publication bias against null results for behavioral interventions suppresses negative evidence. Theater ratio (0.68): High. Individual behavior change as a primary lever for disease prevention shows declining real-world effectiveness at the population level, yet funding mechanisms continue to enforce the paradigm through grant criteria, publication standards, and career incentives. The theater has increased over the decade as the gap between empirical support and funding allocation has widened. Claimed type (Tangled Rope): The constraint exhibits both genuine coordination (establishing evidence standards for behavioral trials) and extraction (funding asymmetry, marginalizing alternatives, trapped populations). Active enforcement through grant mechanisms and peer review standards.
 *
 * PERSPECTIVAL GAP:
 *   The deepest gap is between the beneficiary perspective (rope — pure coordination) and the trapped victim perspective (snare — pure extraction). Behavioral researchers see the constraint as establishing necessary standards for rigorous intervention science. Underserved populations see the same constraint as a mechanism for extracting behavioral data while structural barriers remain unchanged. These are not merely different evaluations of the same phenomenon; they reflect incompatible structural positions — one group's benefit flows are the other group's cost flows. The scaffold and piton perspectives reveal that even within the institutional order, there is doubt about whether the behavioral epidemiology paradigm is functional (piton suggests it's maintained by inertia) or temporary (scaffold suggests it's a transitional stage before structural epidemiology gains recognition). The analytical observer's mountain perspective risks naturalizing funding allocation as an immutable feature of epidemiological science when the base properties reveal it as a contingent institutional arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is determined by benefit/cost flow and exit options. Underserved populations: trapped exit + victim status → high d (1.0). Prevention researchers: constrained exit + victim status → high d (0.75-0.85). Behavioral researchers: arbitrage exit + beneficiary status → low d (0.10-0.20). Pharmaceutical industry: arbitrage exit + beneficiary status → low d (0.05-0.15). Public health agencies: constrained exit + mixed beneficiary/victim → moderate d (0.45-0.55). Public health advocacy: constrained exit + victim (population health) but organized → moderate d (0.35-0.45). The derived d values feed the sigmoid f(d) to compute effective extractiveness chi. Low d agents (beneficiaries) experience negative χ (extraction flows toward them); high d agents (victims) experience amplified χ through the sigmoid. The organizer coalition's moderate d reflects their organized power dampening the raw extraction signal they would experience if powerless.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The constraint resolves the mandatrophy by showing that apparent 'behavior change is good and necessary' framing (mountain/rope perspective) obscures structural asymmetry in benefit flow and exit options. The beneficiary perspective (institutional researchers and industry) experiences pure coordination and arbitrage. The victim perspectives (underserved populations, prevention researchers) experience snare and tangled rope. The scaffold perspective (advocacy coalition) sees a solvable institutional problem. The piton perspective (individual behavior paradigm) identifies the constraint as performatively maintained despite declining empirical support. The mandatrophy is resolved not by choosing which classification is 'correct' but by recognizing that the beneficiary class has defined what counts as valid epidemiological evidence in ways that privilege their own position. The extraction operates through epistemic enforcement (RCT methodology, behavior-change outcome measures) not through direct coercion. This is a classic case of how institutional power operates through defining frames and evidence standards rather than through explicit force.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    behavioral_vs_structural_causality,
    'To what extent are documented health disparities driven by individual behavioral choices versus structural determinants (housing, discrimination, economic opportunity)?',
    'Longitudinal comparison of health outcomes: populations with behavior change interventions vs. populations with structural interventions (housing-first programs, anti-discrimination enforcement, wage increases); cross-country analysis controlling for structural variables; decomposition analysis isolating behavioral vs. structural variance',
    'If structural ≥ 80% of variance: behavioral epidemiology funding is extractive mislabeling (reclassify toward snare). If behavioral ≥ 60% of variance: funding allocation is justified (reclassify toward rope). Most epidemiological literature suggests structural ≥ 70%, but funding allocation assumes behavioral ≥ 50%.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(behavioral_vs_structural_causality, empirical, 'Relative contribution of behavioral vs. structural determinants to health disparities').

omega_variable(
    intervention_effectiveness_decline,
    'Is the observed declining population-level impact of behavior change interventions over time evidence of natural limits or evidence of implementation failure masking structural problems?',
    'Mechanism analysis of failed interventions: qualitative research identifying implementation barriers (supply-side constraints, patient non-engagement due to untreated structural barriers); cost-effectiveness analysis comparing intervention costs to structural approaches; nested trials with and without structural supports',
    'If natural limits: funding allocation toward behavioral approaches is appropriate (theater_ratio lower). If implementation barriers: funding is treating the intervention as the problem rather than addressing structural constraints preventing effectiveness (theater_ratio higher, snare component stronger).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intervention_effectiveness_decline, empirical, 'Whether declining intervention effectiveness reflects natural limits or structural barriers').

omega_variable(
    funding_mechanism_alternative_pathways,
    'Are alternative research funding mechanisms (structural epidemiology, health equity research, environmental health) genuinely available to prevention-focused researchers or are they marginalized by peer review and grant evaluation?',
    'Funding database analysis: market share trends for structural vs. behavioral epidemiology research, 2010-2030; acceptance rates and review scores for structurally-focused grant applications; citation patterns and journal prestige comparison; career outcome analysis comparing structural researchers to behavioral researchers',
    'If alternatives are funded at comparable rates: constrained exit option is accurate, snare perspective overstated. If alternatives are systematically disadvantaged (lower acceptance rates, lower funding per application): snare and suppression assessment confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(funding_mechanism_alternative_pathways, empirical, 'Availability of alternative epidemiological research funding pathways').

omega_variable(
    pharmaceutical_industry_coupling,
    'How tightly coupled is behavioral epidemiology funding to pharmaceutical industry interests? Does funding flow preferentially toward interventions with commercial applications?',
    'Network analysis of funding sources: tracking NIH, NSF, and foundation grants to behavioral epidemiology with cross-reference to pharmaceutical-funded research and clinical trial capacity; analysis of industry presence in grant review panels; market size analysis correlating between funded research areas and commercial opportunity',
    'If coupling is tight (industry correlates with 60%+ of funded research): beneficiary analysis confirms pharmaceutical benefit (directionally supports snare/tangled rope perspectives). If coupling is weak: industry is incidental rather than driving beneficiary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pharmaceutical_industry_coupling, empirical, 'Coupling between behavioral epidemiology funding and pharmaceutical industry interests').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(behavioral_epidemiology_funding, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(behav_epi_tr_t0, behavioral_epidemiology_funding, theater_ratio, 0, 0.48).
narrative_ontology:measurement(behav_epi_tr_t5, behavioral_epidemiology_funding, theater_ratio, 5, 0.62).
narrative_ontology:measurement(behav_epi_tr_t10, behavioral_epidemiology_funding, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(behav_epi_be_t0, behavioral_epidemiology_funding, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(behav_epi_be_t5, behavioral_epidemiology_funding, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(behav_epi_be_t10, behavioral_epidemiology_funding, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(behavioral_epidemiology_funding, information_standard).
narrative_ontology:affects_constraint(behavioral_epidemiology_funding, structural_determinants_of_health_marginalization).
narrative_ontology:affects_constraint(behavioral_epidemiology_funding, pharmaceutical_industry_funding_capture).
narrative_ontology:affects_constraint(behavioral_epidemiology_funding, public_health_research_methodology_constraints).

% DUAL FORMULATION NOTE:
% Behavioral epidemiology funding is downstream of broader public health research methodology constraints and pharmaceutical industry influence. The constraint family includes structural health determinants marginalization (higher ε, more clearly snare-like) as the upstream force, and specific funding bottlenecks as downstream manifestations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(behavioral_epidemiology_funding, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
