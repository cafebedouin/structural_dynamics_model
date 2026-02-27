% ============================================================================
% CONSTRAINT STORY: sorites_paradox
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sorites_paradox, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sorites_paradox
 *   human_readable: The Sorites Paradox (Application of Legal Cutoffs)
 *   domain: legal/social/epistemology
 *
 * SUMMARY:
 *   The Sorites Paradox — the logical problem that no sharp boundary can
 *   separate a continuous property into two discrete categories without
 *   contradiction — appears to be an immutable constraint on legal reasoning.
 *   Legal systems worldwide respond by imposing arbitrary sharp cutoffs (age
 *   18 for voting, residency duration for benefits, asset thresholds for
 *   taxation) to solve the boundary problem. This constraint exhibits the
 *   full range of DR classification depending on perspective. From the
 *   administrative authority's view, the sharp cutoff is pure coordination:
 *   it enables predictable, efficient decision-making and terminates the
 *   logical regress of 'is 17.9999... old enough?' From the marginalized
 *   boundary case's view, it is pure extraction: individuals just below the
 *   threshold bear all discontinuity cost with no recourse. From an
 *   analytical standpoint, the constraint risks naturalizing a contingent
 *   institutional choice as a law of logic. The paradox is real and deep, but
 *   the claim that legal systems must respond with sharp boundaries is
 *   questionable — graduated systems, contextual adjudication, and
 *   fuzzy-logic frameworks offer escape routes the classical view overlooks.
 *
 * KEY AGENTS:
 *   - Marginalized Boundary Cases: Primary victim (powerless/trapped) — individuals landing at or just below legal cutoffs; bear full discontinuity cost with no exit
 *   - Administrative Authority: Primary beneficiary (institutional/arbitrage) — government agencies, regulators, judges who benefit from clear rules that enable efficient decision-making
 *   - Administrative Applicants: Secondary victim/beneficiary (moderate/constrained) — individuals and businesses applying for benefits or licenses; experience both coordinative clarity and extractive discontinuity
 *   - Legal Reform Coalition: Organized agents (organized/constrained) — scholars, advocacy groups, courts exploring alternative systems (graduated benefits, contextual adjudication); building sunset pathway
 *   - Inherited Legal Doctrine: Institutional actor (institutional/arbitrage) — the doctrinal commitment to sharp legal boundaries; endures through inertia despite low remaining functional justification
 *   - Philosophical Impasse: Analytical observer (analytical/analytical) — the pure logical view of the Sorites; risks false summit (naturalizing pragmatic choice as logical necessity)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sorites_paradox, 0.38).
domain_priors:suppression_score(sorites_paradox, 0.48).
domain_priors:theater_ratio(sorites_paradox, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sorites_paradox, extractiveness, 0.38).
narrative_ontology:constraint_metric(sorites_paradox, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(sorites_paradox, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sorites_paradox, tangled_rope).
narrative_ontology:human_readable(sorites_paradox, "The Sorites Paradox (Application of Legal Cutoffs)").
narrative_ontology:topic_domain(sorites_paradox, "legal/social/epistemology").

domain_priors:requires_active_enforcement(sorites_paradox).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sorites_paradox, institutional_clarity).
narrative_ontology:constraint_beneficiary(sorites_paradox, administrative_efficiency).
narrative_ontology:constraint_victim(sorites_paradox, marginalized_boundary_cases).
narrative_ontology:constraint_victim(sorites_paradox, semantic_accuracy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MARGINALIZED BOUNDARY CASE (SNARE) — An individual or group landing precisely at or just below/above an arbitrary legal cutoff (e.g., age 18 for voting, residency requirement, asset threshold) experiences the constraint as pure extraction. No exit option: the law is fixed, the individual cannot escape their position relative to the boundary. Maximum suppression — the boundary is non-negotiable, and alternative adjudication is prohibited. The boundary case absorbs all discontinuity cost: one day before the threshold they have no access; one day after they have full access. The constraint extracts visibility/legitimacy from these agents by rendering their marginal status invisible in binary legal categories.
constraint_indexing:constraint_classification(sorites_paradox, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ADMINISTRATIVE APPLICANT (TANGLED ROPE) — A moderate-power actor (individual seeking benefits, business applying for licensing) experiences both coordination benefit and extraction. The sharp cutoff solves a real coordination problem: administrators need a clear rule to process applications efficiently without endless discretionary judgment. The applicant benefits from this clarity (knows exactly what to do to qualify). But they also bear extraction: if just below the threshold, they have no recourse; the system forces a discontinuous judgment that may diverge from actual circumstances. Constrained exit — they can pursue administrative appeals or wait for rule changes, but cannot escape the current boundary.
constraint_indexing:constraint_classification(sorites_paradox, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ADMINISTRATIVE AUTHORITY (ROPE) — The institution (government agency, regulator, judge applying the law) benefits from the sharp cutoff as a coordination mechanism. The rule solves the Sorites problem: without a sharp boundary, administrators face infinite regression (is 17 years 364 days old enough? what about 17 years 363 days?). The sharp cutoff terminates this regress and enables efficient, predictable decision-making. Arbitrage exit — the authority can lobby for rule changes or propose new thresholds, and is not trapped by the boundary itself. The constraint is genuinely coordinative from this perspective: it provides a stable focal point for billions of administrative decisions.
constraint_indexing:constraint_classification(sorites_paradox, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: LEGAL REFORM COALITION (SCAFFOLD) — Organized agents (legal scholars, advocacy groups, courts engaging in gradual rule refinement) see the sorites paradox as a solvable problem with a sunset clause. Alternative approaches exist: graduated benefit schedules (means-tested rather than hard cutoffs), sliding-scale penalties, equitable estoppel doctrines, and contextual adjudication that preserves discretion. These alternatives are slowly replacing blanket hard cutoffs in many jurisdictions. The constraint has high suppression (hard cutoffs are rigid) but temporary — as legal systems mature and computational capacity increases, softer boundaries (thresholds with sliding zones, interpolation) become more feasible. The coalition has constrained but meaningful exit: they can pilot alternative systems, litigate boundary cases, and advocate for statutory change.
constraint_indexing:constraint_classification(sorites_paradox, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: INHERITED LEGAL DOCTRINE (PITON) — The doctrine that the law must have sharp, clear boundaries is a degraded remnant of a past era when analog adjudication was the only option. The rule persists through institutional inertia: 'law must be clear and predictable, therefore sharp boundaries are necessary.' But the functional justification has attenuated. Digital systems, machine learning models, and graduated benefit schedules have proven that institutional efficiency does NOT require discontinuous thresholds. The boundary-sharpness doctrine is now largely performative: it persists because legal systems are conservative and institutional actors have sunk costs in boundary-based reasoning. Theater ratio is high (judicial rhetoric emphasizes predictability and clarity, but alternative systems deliver both). The doctrine endures despite low remaining functional force.
constraint_indexing:constraint_classification(sorites_paradox, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: PHILOSOPHICAL IMPASSE / NATURAL LAW VIEW (MOUNTAIN) — From a pure logic standpoint, the Sorites Paradox appears to be an immutable mathematical constraint: any continuous predicate (age, wealth, color) resists sharp binary partition without contradiction. The classical view sees sharp legal cutoffs as a necessary response to this logical inevitability — there is no escape from the paradox, only pragmatic solutions. This perspective risks naturalizing what is actually a contingent choice. The paradox is real at the philosophical level, but the claim that legal systems MUST use sharp boundaries to handle it is a false summit: graduated systems, fuzzy-logic adjudication, and contextual reasoning all escape the paradox without sharp boundaries.
constraint_indexing:constraint_classification(sorites_paradox, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sorites_paradox_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sorites_paradox, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sorites_paradox, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(sorites_paradox, TR),
    TR >= 0.70.

:- end_tests(sorites_paradox_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The sharp cutoff imposes real cost on boundary cases but also provides genuine administrative efficiency. The extraction is not maximal (as it would be in a pure Snare) because the coordinative benefit is real — administrators genuinely do need clear rules. However, it is substantial because alternative systems exist that could reduce discontinuity cost. The value reflects a mixed regime. Suppression (0.48): Moderate-high. Hard legal boundaries are non-negotiable by design; individuals cannot negotiate their way across a threshold. But suppression is not total because: (a) legal systems do allow appeals and exceptions in some jurisdictions, (b) elected officials can change boundaries, and (c) some jurisdictions have adopted softer alternatives. Theater ratio (0.65): Moderately high. Judicial rhetoric emphasizes that sharp boundaries are necessary for predictability and clarity, but empirical evidence suggests that graduated systems deliver predictability equally well. The boundary-sharpness doctrine persists partly through performative justification (the idea that 'law must be clear') rather than pure function. Theater has increased over the interval as alternatives have become more feasible without loss of clarity.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. The administrative authority sees pure coordination (Rope) — the sharp boundary is the elegant solution to an administrative problem. The marginalized boundary case sees pure extraction (Snare) — they bear all cost and have no voice. The administrative applicant sees mixed coordination and extraction (Tangled Rope) — the clarity helps them plan but can also harm them. The legal reform coalition sees a temporary problem with a sunset (Scaffold) — alternatives are emerging and will eventually replace hard boundaries. The inherited legal doctrine sees its own degradation (Piton) — the boundary-sharpness principle persists through inertia despite lower functional necessity. The philosophical observer risks seeing an immutable law of logic (Mountain) — the Sorites Paradox appears to make sharp boundaries inevitable — but this is a false summit: the paradox is real at the logical level, but the response of using sharp boundaries is a contingent institutional choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from its structural position. Beneficiaries of the clear-boundary regime (administrative authorities) have low d because the constraint benefits them directly and they have arbitrage exit options — they can change the rule if incentivized. Victims of discontinuity (boundary cases) have high d because they bear cost without reciprocal benefit and cannot exit. Agents with moderate power and constrained exit (applicants, reform coalitions) occupy middle d values. The engine's directionality derivation captures this structural asymmetry through the beneficiary/victim declarations and exit options: institutional beneficiaries with arbitrage → d ≈ 0.15 → f(d) ≈ -0.01 → negative chi (extractiveness scaled negatively). Powerless trapped victims → d ≈ 0.95 → f(d) ≈ 1.42 → amplified chi. This perspectival gap is not a measurement problem; it is the core insight of the constraint structure.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by revealing that the boundary-sharpness principle conflates two distinct claims: (1) the Sorites Paradox is real — continuous properties resist sharp partition without logical tension (TRUE, mathematical necessity), and (2) legal systems must respond with sharp boundaries — sharp boundaries are inevitable or optimal (CONTINGENT, empirically falsifiable). The classical view commits mandatrophy by taking (1)'s truth as evidence for (2)'s inevitability. But systems using graduated benefits, sliding scales, and contextual adjudication show that alternative responses to the paradox exist without loss of administrative clarity or public understanding. The constraint is real and deep, but the specific institutional response (hard cutoffs) is a choice, not a necessity. The tangled_rope classification captures this: the sharp boundary does provide genuine coordination (hence 'rope'), but only for institutional actors with arbitrage power; for marginalized cases it is pure extraction (hence 'tangled'). The mandatrophy is resolved by decomposing the boundary problem (true) from the boundary-solution choice (contingent).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    administrative_efficiency_necessity,
    'Do sharp legal cutoffs actually maximize administrative efficiency, or do they create costly litigation and exception-handling that graduated systems would avoid?',
    'Comparative cost analysis: litigation + appeals + exceptions under hard-cutoff regimes vs. administrative overhead under graduated systems; measurement of processing time and error rates across jurisdictions with different boundary strategies',
    'If hard cutoffs are more efficient: the coordinative function (Rope perspective) is justified, and suppression is a necessary trade-off. If graduated systems are more efficient: the hard cutoff is extractive theater masquerading as necessity, shifting classification toward Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(administrative_efficiency_necessity, empirical, 'Whether sharp cutoffs maximize administrative efficiency').

omega_variable(
    alternative_system_feasibility,
    'Can legal systems implement gradient-based benefit allocation, sliding-scale penalties, or contextual adjudication as operationally viable alternatives to sharp boundaries?',
    'Field studies of jurisdictions that have implemented graduated systems; measurement of compliance costs, judicial discretion variance, and public acceptance; technological feasibility assessment for automated graduated adjudication',
    'If feasible and cost-effective: the scaffold perspective is correct, and the sunset clause is real. If infeasible or socially rejected: sharp boundaries remain necessary despite their extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_system_feasibility, empirical, 'Feasibility of gradient-based legal systems').

omega_variable(
    boundary_discontinuity_cost,
    'What is the actual welfare cost imposed by boundary discontinuities? Is it justified by administrative savings, or is it a hidden extraction mechanism?',
    'Welfare analysis: measurement of aggregate harm from marginal cases rendered ineligible by sharp cutoffs vs. estimated administrative savings; comparison of life outcomes for just-below-cutoff vs. just-above-cutoff cohorts; meta-analysis of equity studies in social benefit and licensing regimes',
    'If discontinuity cost is substantial and unjustified: confirms Snare classification from powerless perspective and validates extraction mechanism. If small relative to savings: coordinative benefit is real.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_discontinuity_cost, empirical, 'Welfare cost of boundary discontinuities').

omega_variable(
    semantic_vs_pragmatic_solution,
    'Does the Sorites Paradox demand a pragmatic solution (sharp cutoff), or is it fundamentally a semantic problem requiring reconceptualization of legal predicates?',
    'Philosophical/linguistic analysis: comparison of legal systems using sharp boundaries vs. those using intrinsic concepts (e.g., ''mature judgment'' instead of ''18 years old''); evaluation of whether sharp boundaries actually resolve the paradox or merely suppress it; assessment of whether vague-predicate logic could be fully integrated into legal reasoning',
    'If pragmatic solution is correct: sharp boundaries are justified and inevitable. If semantic solution is viable: the constraint is a choice, not a necessity, and classification shifts from Mountain toward Tangled Rope or Scaffold.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(semantic_vs_pragmatic_solution, conceptual, 'Whether Sorites is a pragmatic or semantic problem').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sorites_paradox, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sorites_tr_t0, sorites_paradox, theater_ratio, 0, 0.5).
narrative_ontology:measurement(sorites_tr_t25, sorites_paradox, theater_ratio, 25, 0.62).
narrative_ontology:measurement(sorites_tr_t50, sorites_paradox, theater_ratio, 50, 0.65).

% Extraction over time
narrative_ontology:measurement(sorites_be_t0, sorites_paradox, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(sorites_be_t25, sorites_paradox, base_extractiveness, 25, 0.34).
narrative_ontology:measurement(sorites_be_t50, sorites_paradox, base_extractiveness, 50, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sorites_paradox, enforcement_mechanism).
narrative_ontology:affects_constraint(sorites_paradox, regulatory_arbitrary_thresholds).
narrative_ontology:affects_constraint(sorites_paradox, welfare_cliff_effects).

% DUAL FORMULATION NOTE:
% The Sorites Paradox (logical problem) is downstream of philosophical issues about vagueness and predication. The legal application (sharp cutoffs as a solution) is a distinct constraint with its own extractiveness value. The logical paradox has ε ≈ 0.05 (Mountain-level unavoidability); the institutional response has ε ≈ 0.38 (Tangled Rope, representing the contingent choice to use sharp boundaries). These should not be conflated — the paradox is real, but the response is not inevitable.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
