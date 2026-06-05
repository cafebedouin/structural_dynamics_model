% ============================================================================
% CONSTRAINT STORY: burden_of_proof_scientific_empirical
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_burden_of_proof_scientific_empirical, []).

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
 *   constraint_id: burden_of_proof_scientific_empirical
 *   human_readable: Statistical Significance Threshold (p < 0.05)
 *   domain: technological/social
 *
 * SUMMARY:
 *   The p < 0.05 significance threshold represents a critical junction in
 *   empirical science where the burden of proof is codified through a single
 *   numerical gate. Adopted by Ronald Fisher in the 1920s as a pragmatic tool
 *   for agricultural experimentation, the threshold has become
 *   institutionalized globally across biology, medicine, psychology, and
 *   social sciences. This constraint exhibits multiple classification types
 *   depending on structural position: it functions as pure extraction (Snare)
 *   for researchers with marginal true effects or limited resources, as mixed
 *   coordination and extraction (Tangled Rope) for replication programs, as
 *   coordination (Rope) for established programs with large samples, and as
 *   degraded ritual (Piton) for the publishing system that enforces it
 *   despite knowing it is gamed. The theater_ratio (0.64) reflects that the
 *   significance threshold is substantially performative: journals enforce
 *   it, researchers optimize for it, but the actual false positive rate
 *   (estimated at 30-50% across many fields) indicates the gate fails to
 *   separate signal from noise effectively. The extractiveness (0.52)
 *   captures the asymmetry: those who clear the threshold gain publication,
 *   funding, and career advancement; those who fall just outside (p=0.06) are
 *   suppressed despite potentially discovering real effects. Open science
 *   reforms (preregistration, Bayesian methods, effect-size reporting)
 *   represent scaffolding with an explicit sunset: as alternative standards
 *   mature, the binary significance threshold loses gatekeeping force.
 *
 * KEY AGENTS:
 *   - Marginal Hypotheses: Primary victim (powerless/trapped) — true effects just outside the threshold (p=0.06 or effects true at smaller scope) are suppressed by the gate
 *   - Resource-Constrained Researchers: Primary victim (powerless/trapped) — underfunded research programs cannot afford large samples required to drive p-values below threshold; face pressure to p-hack or suppress findings
 *   - Replication Programs: Secondary victim (moderate/constrained) — required to replicate published claims but face higher burden of proof (Type S error asymmetry) to overturn them; also benefit from having clear standards
 *   - Established Research Programs: Primary beneficiary (institutional/arbitrage) — large sample sizes make crossing threshold easier; first-mover advantage under Bayesian updating asymmetry; can choose which findings to replicate
 *   - Methodology Gatekeepers: Primary beneficiary (institutional/arbitrage) — journals, statistical societies, funding agencies enforce threshold; benefit from gatekeeping role and clear standards; can shift thresholds without loss
 *   - Open Science Reform Coalition: Organized actors (organized/constrained) — arXiv, preregistration systems, meta-science communities building alternative verification pathways; see sunset as structural and achievable
 *   - Academic Publishing System: Institutional actor (institutional/arbitrage) — maintains threshold through institutional inertia despite knowing it is gamed; benefits from performative gatekeeping (piton perspective)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent threshold choice as mathematical necessity; false summit perspective
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(burden_of_proof_scientific_empirical, 0.52).
domain_priors:suppression_score(burden_of_proof_scientific_empirical, 0.68).
domain_priors:theater_ratio(burden_of_proof_scientific_empirical, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(burden_of_proof_scientific_empirical, extractiveness, 0.52).
narrative_ontology:constraint_metric(burden_of_proof_scientific_empirical, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(burden_of_proof_scientific_empirical, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(burden_of_proof_scientific_empirical, tangled_rope).
narrative_ontology:human_readable(burden_of_proof_scientific_empirical, "Statistical Significance Threshold (p < 0.05)").
narrative_ontology:topic_domain(burden_of_proof_scientific_empirical, "technological/social").

domain_priors:requires_active_enforcement(burden_of_proof_scientific_empirical).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(burden_of_proof_scientific_empirical, established_research_programs).
narrative_ontology:constraint_beneficiary(burden_of_proof_scientific_empirical, methodology_gatekeepers).
narrative_ontology:constraint_victim(burden_of_proof_scientific_empirical, marginal_hypotheses).
narrative_ontology:constraint_victim(burden_of_proof_scientific_empirical, resource_constrained_researchers).
narrative_ontology:constraint_victim(burden_of_proof_scientific_empirical, replication_studies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MARGINAL HYPOTHESIS (SNARE) — A genuinely true effect with true p-value of 0.06 or an effect true at local scope (regional/national) but global noise floor set at 0.05. Cannot exit the threshold; trapped by the gate. Bears full cost: suppression of true knowledge through arbitrary barrier. No advocacy mechanism for effects just outside the threshold boundary.
constraint_indexing:constraint_classification(burden_of_proof_scientific_empirical, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: RESOURCE-CONSTRAINED RESEARCHER (SNARE) — Cannot afford large sample sizes required to drive p-values below threshold. Underfunded research programs face pressure to either inflate p-values through methodological choices (p-hacking) or suppress null/marginal findings. Exit options are either data manipulation or silence. Trapped by statistical requirements that assume abundant resources.
constraint_indexing:constraint_classification(burden_of_proof_scientific_empirical, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: REPLICATION PROGRAM (TANGLED ROPE) — Benefits from having a clear target (threshold) and a coordination function (knowing what level of evidence is required). But also bears extraction: replication studies require higher statistical power to overturn published claims than to publish new ones (Type S error asymmetry). Constrained by pressure to generate novel positive findings for career advancement while also required to verify existing claims. Mixed: coordination mechanism (knowing the standard) plus asymmetric extraction (replication costs more than discovery).
constraint_indexing:constraint_classification(burden_of_proof_scientific_empirical, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ESTABLISHED RESEARCH PROGRAM (ROPE) — Benefits from the p < 0.05 threshold. Large sample sizes (from established funding and collaborations) make crossing the threshold easier. First-mover advantage: once a hypothesis crosses the threshold, subsequent replication studies face higher burden of proof to overturn the claim (Bayesian updating asymmetry). Experiences the constraint as a coordination mechanism: a shared standard enables comparison and cumulative knowledge. Net beneficiary.
constraint_indexing:constraint_classification(burden_of_proof_scientific_empirical, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: METHODOLOGY GATEKEEPER (ROPE) — Journals, statistical societies, funding agencies enforce the threshold. Benefits from its enforcement: it provides clear standards, reduces ambiguity about what counts as publishable, and creates a gatekeeping role. Can choose which threshold to apply and which methods to accept/reject. Arbitrage exit: can shift standards (adopt p < 0.005 for certain domains) without loss. Experiences the constraint as coordination (shared standards) with institutional beneficiary.
constraint_indexing:constraint_classification(burden_of_proof_scientific_empirical, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: OPEN SCIENCE REFORM COALITION (SCAFFOLD) — Organized agents (preregistration systems, p-curve analysis communities, meta-science researchers) see the p < 0.05 threshold as a temporary coordination problem with built-in sunset. Preregistration, Bayesian methods, effect-size reporting, and meta-science provide alternative verification pathways that bypass the binary significance gate. The coalition has agency and sees an explicit exit: as open-science norms mature, the binary threshold loses force. Suppression is tolerated because it is declining over the time horizon. Sunset estimated at 15-25 years as alternative standards mature.
constraint_indexing:constraint_classification(burden_of_proof_scientific_empirical, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ACADEMIC PUBLISHING SYSTEM (PITON) — The p < 0.05 threshold is largely performative in modern publishing. Editors and reviewers know that p-hacking, researcher degrees of freedom, and selective reporting are endemic (false positive rate ~30-50% across many fields). The threshold persists through institutional inertia: journals enforce it because it is the standard, not because it effectively separates signal from noise. Theater ratio is high (0.64) because the system maintains the ritual despite knowing it is degraded. The functional verification has atrophied (replaced by effect-size, pre-registration, replication pressure) but the gate remains.
constraint_indexing:constraint_classification(burden_of_proof_scientific_empirical, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, the p < 0.05 threshold appears to be an immutable feature of hypothesis testing: any threshold is arbitrary, and the choice of 0.05 is just a convention for managing Type I error rates. This perspective risks naturalizing a contingent institutional choice. However, the base properties (extractiveness 0.52, suppression 0.68) contradict the mountain classification — the engine will detect this as a false summit. The illusion of naturalness derives from the fact that the threshold is mathematically well-defined and universally applied, not from its structural necessity.
constraint_indexing:constraint_classification(burden_of_proof_scientific_empirical, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(burden_of_proof_scientific_empirical_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(burden_of_proof_scientific_empirical, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(burden_of_proof_scientific_empirical, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(burden_of_proof_scientific_empirical, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(burden_of_proof_scientific_empirical, TR),
    TR >= 0.70.

:- end_tests(burden_of_proof_scientific_empirical_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The p < 0.05 threshold creates clear winners and losers. Those who cross the threshold gain publication, citations, and career advancement; those just outside face suppression. The extraction is not total (some marginal findings do get published through alternative routes, some null results are now accepted), but the asymmetry is structural. The value increased from 0.28 to 0.52 over the interval as researcher degrees of freedom and p-hacking practices became endemic and well-documented — the constraint's extractive power grew as its ineffectiveness became visible. Suppression (0.68): High. Barriers include the binary gate itself (effects cannot be published at p=0.06 regardless of true magnitude), publication bias against null results, researcher pressure to p-hack or suppress findings, and career risk of replication studies that overturn published claims. The threshold actively prevents knowledge from being published. Theater_ratio (0.64): Moderate-high. The threshold is substantially performative: it is mathematically well-defined but empirically broken (false positive rate ~30-50% in many fields). Journals enforce it as a ritual; researchers optimize for it; the publishing system knows it is gamed. The theater increased over the interval as meta-science research exposed researcher degrees of freedom, p-hacking, and publication bias. The threshold's performative nature became visible.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural phenomenon (a numerical gate on publication) appears as pure extraction (Snare) from the perspective of those trapped outside it, as mixed coordination and extraction (Tangled Rope) from the perspective of replication programs that benefit from clear standards but suffer from asymmetric burden of proof, as coordination (Rope) from the perspective of well-resourced programs that benefit from the gate, as ritual degradation (Piton) from the publishing system's own perspective (which knows the threshold is gamed), and as temporary scaffolding (Scaffold) from the open science coalition that sees alternative pathways maturing. The perspectival gap is large because the constraint's effect depends entirely on structural position: where you stand relative to the threshold determines whether you experience it as law, coordination, support, or snare. The false summit perspective (mountain from analytical view) risks naturalizing the threshold as a necessary feature of hypothesis testing rather than as a contingent institutional choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by structural position relative to the gate. Marginal hypotheses (true effects with p=0.06) are trapped on the wrong side of the threshold with no exit → high d → high χ → snare. Resource-constrained researchers cannot afford samples large enough to cross threshold with reasonable power → trapped → high d → snare. Replication programs have constrained exit (required to publish replications but face higher burden to overturn) → constrained exit → moderate-high d → tangled_rope. Established programs with large samples can cross threshold easily and benefit from first-mover advantage → arbitrage exit, beneficiary status → low d → rope. Gatekeepers can shift thresholds and choose enforcement patterns → arbitrage exit, beneficiary status → low d → rope. The open science coalition sees an exit path (preregistration, alternative standards) → constrained exit, organized power → moderate d → scaffold with sunset. The publishing system maintains the threshold despite knowing it is degraded → arbitrage exit, institutional power → low d, but high theater → piton.
 *
 * MANDATROPHY ANALYSIS:
 *   EXEMPLAR OF MANDATROPHY RESOLUTION: This constraint resolves the mandatrophy by showing that the p < 0.05 threshold is NOT an irreducible tradeoff between coordination and extraction — it is an extractive gate that was chosen for historical contingency and persists through institutional inertia despite having alternatives. The mandatrophy would ask: 'Is the threshold a necessary burden-of-proof mechanism or an unjustified extraction?' The analysis shows it is an extraction mechanism masquerading as a necessary gate. Coordination alternatives exist (preregistration, Bayesian methods, open data) that provide clearer standards with lower extraction. The scaffold perspective shows the explicit exit path: as alternative standards mature, the binary threshold loses gatekeeping force. The Snare perspective for marginal hypotheses shows that effects just outside the threshold are suppressed despite being potentially true — the 'burden of proof' argument fails because the threshold is arbitrary and gamed. Therefore the constraint is classified as Tangled Rope (coordination function mixed with asymmetric extraction) for established programs but Snare (pure extraction) for those trapped outside the gate. The extractiveness increased over time as p-hacking became endemic and the threshold's ineffectiveness became visible — confirming that the gate is not a natural law but a degraded institutional arrangement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_arbitrariness_boundary,
    'Does the choice of 0.05 as a threshold reflect a mathematically necessary boundary or a historically contingent institutional choice?',
    'Historical analysis of Fisher''s original writings and subsequent adoption patterns; comparison of threshold choices across disciplines; cross-cultural science systems that use different thresholds (e.g., 0.01 in some fields, 0.10 in others)',
    'If mathematically necessary: the mountain perspective is correct, and the constraint is a natural law. If historically contingent: the constraint is institutional extraction disguised as mathematical necessity. This determines whether the classification stands as mountain or collapses to tangled_rope/snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(threshold_arbitrariness_boundary, conceptual, 'Whether p=0.05 is mathematically necessary or historically contingent').

omega_variable(
    effect_size_informativeness,
    'Do alternatives to p-value significance testing (Bayesian credible intervals, effect-size reporting, pre-registration) actually reduce false positives and improve replication rates, or do they simply displace the arbitrary threshold to a different metric?',
    'Meta-analysis of replication rates pre- and post-adoption of alternative standards in specific fields; tracking of p-hacking migration to effect-size gaming; monitoring of publication bias under different reporting standards',
    'If alternatives genuinely reduce false positives: the scaffold perspective is correct, and the sunset is structural. If alternatives merely create new games: the constraint persists unchanged, the scaffold is aspirational, and the extraction remains high.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effect_size_informativeness, empirical, 'Whether alternatives to p-value testing reduce false positives').

omega_variable(
    sample_size_equity_threshold,
    'What sample size threshold creates equity between well-funded and under-resourced research programs under the p < 0.05 gate?',
    'Analysis of sample size distributions across funding quintiles; correlation between research budget and p-value distribution; controlled experiments with equivalent true effects but different sample sizes',
    'If threshold can be achieved equitably: the snare classification for resource-constrained researchers is incorrect. If the gate inherently favors large-sample research: the extraction from underfunded programs is structural and the snare classification is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sample_size_equity_threshold, empirical, 'Whether p < 0.05 threshold can be achieved equitably across funding levels').

omega_variable(
    null_hypothesis_coherence,
    'Is the null hypothesis (effect size = 0) a coherent claim for all research domains, or is it a domain-contingent assumption?',
    'Examination of null hypothesis plausibility across disciplines (e.g., null hypothesis is absurd in some fields, reasonable in others); analysis of which fields report high rates of null rejections vs null acceptances; study of baseline effect-size distributions in mature fields',
    'If null hypothesis is incoherent in some domains: the p < 0.05 threshold is not a neutral gate but a domain-inappropriate extraction mechanism in those fields. This could split the constraint into multiple domain-specific stories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(null_hypothesis_coherence, conceptual, 'Whether null hypothesis is coherent across all research domains').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(burden_of_proof_scientific_empirical, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bops_tr_t0, burden_of_proof_scientific_empirical, theater_ratio, 0, 0.35).
narrative_ontology:measurement(bops_tr_t20, burden_of_proof_scientific_empirical, theater_ratio, 20, 0.5).
narrative_ontology:measurement(bops_tr_t40, burden_of_proof_scientific_empirical, theater_ratio, 40, 0.64).

% Extraction over time
narrative_ontology:measurement(bops_be_t0, burden_of_proof_scientific_empirical, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(bops_be_t20, burden_of_proof_scientific_empirical, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(bops_be_t40, burden_of_proof_scientific_empirical, base_extractiveness, 40, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(burden_of_proof_scientific_empirical, information_standard).
narrative_ontology:affects_constraint(burden_of_proof_scientific_empirical, publication_bias_psychology).
narrative_ontology:affects_constraint(burden_of_proof_scientific_empirical, replication_crisis_biomedicine).
narrative_ontology:affects_constraint(burden_of_proof_scientific_empirical, multiple_comparisons_problem).

% DUAL FORMULATION NOTE:
% The p < 0.05 threshold is downstream of the null hypothesis convention and upstream of publication bias. Separate constraint stories should decompose the threshold's role in each: (1) as a gate for publication decisions (this story), (2) as a Type I error control mechanism (measurement theory), and (3) as a proxy for effect size (behavioral economics). Each has different ε and structural relationships.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(burden_of_proof_scientific_empirical, moderate, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
