% ============================================================================
% CONSTRAINT STORY: behavioral_genetics_reductionism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_behavioral_genetics_reductionism, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: behavioral_genetics_reductionism
 *   human_readable: Behavioral Genetics Reductionism Framework
 *   domain: behavioral_science/genetics/epistemology
 *
 * SUMMARY:
 *   Behavioral genetics reductionism operates as a constraint on how
 *   biological science explains human behavior, cognition, and psychology.
 *   The framework assumes that behavioral variation is primarily explained by
 *   genetic differences, with environment playing a supporting role. This
 *   constraint functions simultaneously as coordination (genetic research is
 *   standardized, organized, and scientifically productive), as asymmetric
 *   extraction (genetic determinism diverts resources from environmental
 *   social intervention), and as institutional inertia (the reductionist
 *   paradigm persists despite accumulating evidence for gene-environment
 *   interaction, epigenetic plasticity, and environmental causation). The
 *   constraint exhibits high theater: neuroscientific studies claiming to
 *   'find the brain basis' of behavior often correlate genetic markers or
 *   brain activity with phenotypes while treating correlation as mechanistic
 *   causation. The frame naturalizes as it constrains — genetic determinism
 *   is experienced as scientific fact by insiders, but appears to outsiders
 *   (marginalized populations, environmental scientists, social intervention
 *   advocates) as an explanatory monopoly maintained by gatekeeping. The
 *   constraint's evolution shows increasing theater (mechanistic reduction
 *   becoming more sophisticated in appearance) and increasing extractiveness
 *   (pharmaceutical industry capturing more behavioral healthcare through
 *   precision medicine framing while social intervention funding declines).
 *   The measurement trajectory reflects Goodhart drift: as alternative models
 *   (G×E interaction, epigenetics) gain evidence, the reductionist frame
 *   responds not by retreating but by incorporating them while preserving
 *   genetic primacy — theater rises to maintain extraction.
 *
 * KEY AGENTS:
 *   - Genetics Research Establishment: Institutional beneficiary (institutional/arbitrage) — captures funding priority, career advancement, and publication prestige through genetic reductionism
 *   - Marginalized Populations: Primary victim (powerless/trapped) — subjected to genetic determinism frameworks that block social intervention and naturalize their disadvantage
 *   - Environmental Social Scientists: Secondary victim (moderate/constrained) — face publication bias, funding barriers, and gatekeeping when proposing environmental primary causation
 *   - Pharmaceutical Industry: Institutional beneficiary (institutional/constrained) — benefits from precision medicine framing but also benefits from genetic reductionism's endorsement of pharmacological over social solutions
 *   - Social Welfare Systems: Organizational victim (powerless/identity_locked) — have internalized genetic determinism as explanation for behavioral poverty/crime/health disparities, blocking resource allocation to environmental remediation
 *   - Analytical Observer: Civilizational position (analytical/analytical) — risks naturalizing paradigm choice as scientific law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(behavioral_genetics_reductionism, 0.58).
domain_priors:suppression_score(behavioral_genetics_reductionism, 0.62).
domain_priors:theater_ratio(behavioral_genetics_reductionism, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(behavioral_genetics_reductionism, extractiveness, 0.58).
narrative_ontology:constraint_metric(behavioral_genetics_reductionism, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(behavioral_genetics_reductionism, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(behavioral_genetics_reductionism, tangled_rope).
narrative_ontology:human_readable(behavioral_genetics_reductionism, "Behavioral Genetics Reductionism Framework").
narrative_ontology:topic_domain(behavioral_genetics_reductionism, "behavioral_science/genetics/epistemology").

domain_priors:requires_active_enforcement(behavioral_genetics_reductionism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(behavioral_genetics_reductionism, genetics_research_establishment).
narrative_ontology:constraint_beneficiary(behavioral_genetics_reductionism, pharmaceutical_industry).
narrative_ontology:constraint_beneficiary(behavioral_genetics_reductionism, institutional_determinism_advocates).
narrative_ontology:constraint_victim(behavioral_genetics_reductionism, environmental_causal_science).
narrative_ontology:constraint_victim(behavioral_genetics_reductionism, marginalized_populations_in_genetic_studies).
narrative_ontology:constraint_victim(behavioral_genetics_reductionism, social_intervention_funding).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MARGINALIZED POPULATIONS (SNARE) — Trapped in explanatory frameworks that attribute their disadvantages to genetic causes, blocking policy-driven social intervention. No exit from the genetic label. Maximum extraction: resources flow away from environmental remediation toward pharmaceuticalization and genetic screening. The constraint naturalizes their condition.
constraint_indexing:constraint_classification(behavioral_genetics_reductionism, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ENVIRONMENTAL SOCIAL SCIENTISTS (TANGLED ROPE) — Constrained by funding structures, peer-review gatekeeping, and publication bias toward genetic findings. Also benefit from collaborative data access and methodology standardization within the constraint. Mixed extraction and coordination: the framework enables some multi-factor research while suppressing environmental-primary causality models. Significant career cost to exit.
constraint_indexing:constraint_classification(behavioral_genetics_reductionism, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: GENETICS RESEARCH ESTABLISHMENT (ROPE) — Net beneficiary (institutional/arbitrage). Experiences the reductionist framework as coordination: it organizes research priorities, funding allocation, and career pathways. Can exit the framework (reframe research questions) without structural barrier. Extraction runs toward this agent through publication priority, funding concentration, and institutional prestige.
constraint_indexing:constraint_classification(behavioral_genetics_reductionism, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PHARMACEUTICAL INDUSTRY (TANGLED ROPE) — Benefits from genetic reductionism as a framework for drug targeting and personalized medicine development. Also experiences genuine coordination benefit: the framework enables pharmacogenomics and precision medicine innovation. Constrained by regulatory pressure and scientific criticism that the framework oversimplifies. Active enforcement through patent protection and regulatory capture. Asymmetric extraction from social welfare perspective: shift from prevention to pharmacological management.
constraint_indexing:constraint_classification(behavioral_genetics_reductionism, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: NEUROSCIENTIFIC METHODOLOGY ESTABLISHMENT (PITON) — Maintains reductionist behavioral genetics framework through institutional inertia despite accumulating evidence for gene-environment interaction and epigenetic complexity. Theater ratio high: much neuroscientific work on behavior is presentation of genetic determinism through neurobiological mechanism (fMRI correlates framed as causation). Primary function (understanding behavior) has atrophied into performative reduction. Maintains constraint through methodological gatekeeping and training pipeline, not because the framework works best.
constraint_indexing:constraint_classification(behavioral_genetics_reductionism, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational scope, the genetic perspective appears as a fundamental truth about human nature: all behavior has genetic substrate, therefore genetic causation is primary. However, structural data contradicts this mountain classification. The constraint requires active enforcement through funding gatekeeping, journal bias, and institutional control. It is not naturally emergent; it is maintained. The engine will detect this as a false summit, revealing naturalization of a contingent paradigm.
constraint_indexing:constraint_classification(behavioral_genetics_reductionism, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(behavioral_genetics_reductionism_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(behavioral_genetics_reductionism, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(behavioral_genetics_reductionism, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(behavioral_genetics_reductionism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(behavioral_genetics_reductionism, TR),
    TR >= 0.70.

:- end_tests(behavioral_genetics_reductionism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The reductionist constraint extracts through resource redirection (genetics funding outpaces environmental social science 8:1 in behavioral domains), through explanation capture (genetic models dominate textbooks and research agendas), and through policy displacement (pharmaceutical intervention is incentivized over social intervention). But extraction is not total — environmental research still occurs, alternative paradigms are articulated, and genetic research produces some genuine insights. The value reflects that the constraint is sustained by institutional and financial structures, not by suppression of all alternatives. Suppression (0.62): Moderate-high. Significant barriers to environmental behavioral science include: publication bias (genetic findings are prioritized in high-impact journals), funding gatekeeping (NIH study sections weight genetic proposals more heavily), professional gatekeeping (genetics training is elevated in neuroscience/psychology PhD programs), and prestige gradients (genetics labs attract talent through career incentives). But these are not absolute — environmental scientists do publish, secure funding, and advance; the barriers are differential, not prohibitive. Theater ratio (0.68): High and rising. Neuroscientific mechanism papers claiming to explain behavior increasingly use fMRI, EEG, or pharmacological data to produce the appearance of mechanistic understanding while substrate (correlation) is conflated with causation. The more sophisticated the neuroscience, the more persuasive the theater. Behavioral genetics papers present heritable variance as causal genetic influence, eliding the gap between population-level heritability and individual-level etiology. The theater has increased because the alternative explanations (gene-environment interaction, epigenetic dynamism, social causation) are scientifically mature and credible — maintaining reductionism requires more sophistication in presentation.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how a single theoretical frame can be experienced as coordinate-and-productive (Rope), as extraction-and-barrier (Snare/Tangled Rope), as institutional degradation (Piton), or as natural law (Mountain-false-summit) depending on the observer's structural position. The genetics establishment genuinely sees the reductionist frame as enabling coordination — it organizes research priorities, standardizes methodology, and facilitates collaboration. Their experience is phenomenologically coherent. Marginalized populations see the same frame as a mechanism of extraction — it justifies genetic determinism, blocks social intervention, and naturalizes disadvantage. Their experience is also phenomenologically coherent. These are not disagreements about facts but about structural position. The perspectival gap is maximal — the beneficiaries and victims have fundamentally opposite classifications. This gap is the diagnostic signature that the constraint is extractive: when the beneficiary and victim see opposite types, extraction is operating. The mountain perspective (natural law view) appears when temporal scope is maximized — genetic causation seems inevitable and foundational. But the engine's false summit detector flags this because the constraint's properties (requires_active_enforcement, has victims, has beneficiaries) contradict mountain criteria. The false summit reveals naturalization: the paradigm choice masquerades as scientific law.
 *
 * DIRECTIONALITY LOGIC:
 *   The derivation chain for this constraint operates through institutional power and funding flow directionality. The genetics research establishment is institutional/arbitrage — they can reorient research questions (exit is low-cost) and benefits flow toward them (genetic research is funded, prestigious, and employable). Their d value is low, producing negative or minimal chi — they experience the constraint as beneficial coordination. Marginalized populations are powerless/trapped — they cannot exit the genetic determinism frame (it is socially and legally enforced) and bears costs flow away from them (intervention funding). Their d value is high (approaching 1.0), producing maximum chi — they experience maximum extraction. Environmental scientists are moderate/constrained — exit from the constraint is possible (they can change research programs) but at significant cost (career damage, funding loss, prestige penalty). Their d value is in the 0.55-0.70 range. The pharmaceutical industry is institutional/constrained — they benefit from genetic reductionism but face scientific and regulatory pressure to acknowledge complexity. Their d is moderate (0.35-0.50), producing moderate chi. The piton classification derives from theater_ratio (0.68) and the fact that the constraint's primary causal function (explaining behavior better than alternatives) has been overtaken by institutional maintenance function (preserving careers and funding structures).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    gene_environment_interaction_sufficiency,
    'Does acknowledging gene-environment interaction as a primary model require abandoning genetic reductionism or merely expanding it?',
    'Analysis of interaction research funding, publication trends, and institutional gatekeeping; comparison of G×E research that emphasizes genetic main effects vs those that treat interaction as primary causal pathway',
    'If interaction is treated as additive (G + E + G×E): reductionist frame persists, merely expanded. If interaction is treated as primary: paradigm shift requiring reclassification to rope/scaffold. Current constraint persistence depends on which interpretation dominates institutional science.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gene_environment_interaction_sufficiency, empirical, 'Whether gene-environment interaction models require or allow genetic reductionism').

omega_variable(
    epigenetic_mechanism_causality,
    'Do epigenetic mechanisms (DNA methylation, histone modification) driven by environmental factors constitute evidence that environmental causes are primary, or do they support genetic reductionism by showing how environment acts through genetic mechanisms?',
    'Historical analysis of epigenetic research framing; examination of whether epigenetic findings increase funding for social intervention or pharmaceutical epigenetic targeting',
    'If epigenetic mechanisms are framed as environmental causation: extraction mechanism is weakened, constraint approaches rope. If framed as genetic mechanistic substrate: extraction mechanism is strengthened, constraint approaches snare. Reductionist frame captures epigenetics either way — the question is whether it acknowledges environmental primacy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(epigenetic_mechanism_causality, conceptual, 'Whether epigenetic mechanisms support environmental or genetic primacy').

omega_variable(
    heritability_interpretation_ambiguity,
    'Does the widespread confusion between heritability (proportion of variance explained by genetic differences in a population) and inheritability (trait transmission across generations) reflect genuine scientific ambiguity or intentional reductionist framing?',
    'Textbook analysis and expert survey on heritability definition clarity; comparison of genetics vs psychology textbook presentations; study of whether research papers consistently distinguish heritability from genetic causation',
    'If genuine ambiguity: constraint approaches rope (coordination problem in terminology). If intentional or negligently persistent: constraint approaches snare (suppression of causal clarity). High-profile behavior genetics papers often equivocate on this distinction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(heritability_interpretation_ambiguity, empirical, 'Whether heritability/inheritability confusion is genuine or deliberate').

omega_variable(
    funding_feedback_loop_causality,
    'Is the reductionist constraint maintained by genuine scientific evidence favoring genetic models, or by funding structures and career incentives that make genetic reductionism the path of institutional advancement?',
    'Historical funding allocation analysis (NIH, NSF behavioral genetics vs environmental social science); career outcome data for researchers who challenge vs reinforce reductionist frame; comparison of citations and hiring patterns for genetic vs environmental behavioral studies with similar methodological rigor',
    'If evidence-driven: constraint approaches rope/mountain (reflects genuine understanding). If incentive-driven: constraint approaches snare (institutional extraction maintained by enforcement). Current data suggests mixed but with strong incentive component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(funding_feedback_loop_causality, empirical, 'Whether reductionist constraint is maintained by evidence or incentives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(behavioral_genetics_reductionism, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bgred_tr_t0, behavioral_genetics_reductionism, theater_ratio, 0, 0.42).
narrative_ontology:measurement(bgred_tr_t10, behavioral_genetics_reductionism, theater_ratio, 10, 0.55).
narrative_ontology:measurement(bgred_tr_t20, behavioral_genetics_reductionism, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(bgred_be_t0, behavioral_genetics_reductionism, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(bgred_be_t10, behavioral_genetics_reductionism, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(bgred_be_t20, behavioral_genetics_reductionism, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(behavioral_genetics_reductionism, information_standard).
narrative_ontology:boltzmann_floor_override(behavioral_genetics_reductionism, 0.12).
narrative_ontology:affects_constraint(behavioral_genetics_reductionism, pharmaceutical_precision_medicine_targeting).
narrative_ontology:affects_constraint(behavioral_genetics_reductionism, social_intervention_funding_displacement).
narrative_ontology:affects_constraint(behavioral_genetics_reductionism, behavioral_heritability_estimation).

% DUAL FORMULATION NOTE:
% Behavioral genetics reductionism is upstream of specific causal claims (whether intelligence is genetic, whether crime is heritable, whether mental illness is neurobiological). The constraint operates at the paradigm level — it determines which questions are asked, which evidence counts, and which explanations are publishable. Decomposition: the reductionist constraint (this story) affects the downstream precision medicine and funding stories. Each downstream story has its own epsilon reflecting its own empirical status and extraction mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(behavioral_genetics_reductionism, institutional, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
