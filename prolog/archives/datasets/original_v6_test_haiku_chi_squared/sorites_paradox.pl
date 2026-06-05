% ============================================================================
% CONSTRAINT STORY: sorites_paradox
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
    constraint_indexing:directionality_override/3,
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
 *   The Sorites Paradox presents a foundational logical problem: vague
 *   predicates (tall, rich, adult, dangerous) cannot be resolved into precise
 *   boundaries without introducing discontinuity. Legal systems solve this
 *   problem through arbitrary sharp cutoffs (age 18/21 for majority, income
 *   thresholds for benefits, property value floors for court jurisdiction).
 *   This constraint models how institutional application of these cutoffs
 *   creates a hybrid coordination-extraction mechanism. The cutoff solves the
 *   coordination problem of vagueness — it enables consistent rule
 *   application and reduces adjudication costs. But it simultaneously creates
 *   extraction: individuals just below (or above) the threshold experience
 *   discontinuous consequences despite indistinguishable practical
 *   circumstances. The constraint's theater ratio (0.65) reflects that the
 *   cutoff is partly performative — societies maintain specific numbers (age
 *   21 for alcohol, $150,000 for small-claims court) through institutional
 *   inertia and path dependency rather than principled defense of why these
 *   particular values optimally resolve vagueness. The constraint's
 *   extractiveness (0.38) reflects moderate asymmetry: administrative
 *   beneficiaries gain clarity and efficiency; edge-case subjects bear sharp
 *   discontinuity costs; policy-affected populations experience mixed
 *   coordination and extraction. This is a diagnostic exemplar of how logical
 *   necessity (the sorites paradox is real) can mask institutional choice
 *   (sharp cutoffs are one solution among many).
 *
 * KEY AGENTS:
 *   - Edge-Case Subject: Individual at boundary (powerless/trapped) — bears full discontinuity cost; e.g., $1 below benefit threshold, one week below age cutoff
 *   - Legal Administration: Institutional actors (institutional/arbitrage) — benefit from efficiency and clarity; reduce adjudication and interpretation costs
 *   - Policy-Affected Population: Moderate-power groups (moderate/constrained) — near-threshold populations; experience both coordination benefit (clear rules) and extraction cost (discontinuous eligibility)
 *   - Legal Reform Coalition: Organized advocates (organized/constrained) — civil rights groups, administrative reformers; pushing toward graduated benefits and contextual judgment frameworks
 *   - Legislative Process: Institutional ritual (institutional/arbitrage) — maintains arbitrary numbers through path dependency; sees change as high coordination cost
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing institutional choice as logical necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sorites_paradox, 0.38).
domain_priors:suppression_score(sorites_paradox, 0.52).
domain_priors:theater_ratio(sorites_paradox, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sorites_paradox, extractiveness, 0.38).
narrative_ontology:constraint_metric(sorites_paradox, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(sorites_paradox, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sorites_paradox, tangled_rope).
narrative_ontology:human_readable(sorites_paradox, "The Sorites Paradox (Application of Legal Cutoffs)").
narrative_ontology:topic_domain(sorites_paradox, "legal/social/epistemology").

domain_priors:requires_active_enforcement(sorites_paradox).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sorites_paradox, legal_administrators).
narrative_ontology:constraint_beneficiary(sorites_paradox, decision_makers).
narrative_ontology:constraint_beneficiary(sorites_paradox, institutional_clarity).
narrative_ontology:constraint_victim(sorites_paradox, boundary_edge_cases).
narrative_ontology:constraint_victim(sorites_paradox, epistemic_precision).
narrative_ontology:constraint_victim(sorites_paradox, individual_fairness).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EDGE-CASE SUBJECT (SNARE) — An individual whose property, age, income, or status falls just below (or above) an arbitrary legal cutoff. Bears full cost of sharp boundary: legally distinct from those millimeters above/below threshold despite indistinguishable practical circumstance. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.52.
constraint_indexing:constraint_classification(sorites_paradox, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LEGAL ADMINISTRATION (ROPE) — Institutional actors benefit from sharp cutoffs as coordination mechanism: resolves vagueness through decision rule, enables consistent application, reduces adjudication costs. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.04. Negative effective extraction = net beneficiary via efficiency.
constraint_indexing:constraint_classification(sorites_paradox, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: POLICY-AFFECTED POPULATION (TANGLED ROPE) — Moderate-power groups (small business, working families near benefit thresholds) experience mixed coordination and extraction. Benefit from rule clarity (coordination function); harmed by discontinuous eligibility (asymmetric extraction at boundaries). d≈0.68, f(d)≈1.04, σ=1.0 → χ≈0.39.
constraint_indexing:constraint_classification(sorites_paradox, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: LEGAL REFORM COALITION (SCAFFOLD) — Organized advocates (civil rights groups, administrative reform movements) see sharp cutoffs as temporary solutions with sunset pathway: graduated benefit cliffs, contextual judgment frameworks, and risk-based thresholds are replacing brittle boundaries. d≈0.38, f(d)≈0.36, σ=1.0 → χ≈0.14. Low effective extraction because coalition has agency and path forward.
constraint_indexing:constraint_classification(sorites_paradox, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LEGISLATIVE RITUAL (PITON) — Sharp cutoffs persist through institutional inertia despite well-known perverse effects. Legislative process maintains arbitrary numbers (age 18/21, income thresholds $X) largely through path dependency and stakeholder coordination costs, not because they optimally solve vagueness. theater_ratio=0.65 indicates substantial performative maintenance. The ritual has degraded function — everyone acknowledges cutoffs are arbitrary, but replacing them requires legislative consensus that is harder than keeping them.
constraint_indexing:constraint_classification(sorites_paradox, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (FALSE SUMMIT ASSESSMENT) — From a civilizational perspective, vague predicates (tall, rich, adult, dangerous) create an inherent logical problem: the sorites paradox is a genuine mathematical/logical constraint. No rule-based system can perfectly resolve vagueness without discontinuity. However, the structural data (ε=0.38, suppression=0.52) contradicts mountain classification. The logical constraint is real, but its application in legal contexts is contingent — civilizations could use multi-valued logic, contextual judgment, or graduated transitions instead of sharp cutoffs. The engine will flag this as a false summit: naturalizing institutional choice as logical necessity.
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
    constraint_indexing:constraint_classification(sorites_paradox, TypeOther, context(agent_power(institutional), _, _, _)),
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
 *   Extractiveness (0.38): Moderate. The extraction is real but not maximal. Legal cutoffs do solve the genuine coordination problem of vagueness — without them, every boundary decision would require adjudication. But the extraction cost (discontinuous treatment of near-identical cases) is significant. The value reflects that institutional benefit (efficiency) is genuine, not pure rent-seeking. Suppression (0.52): Moderate-high. Edge-case subjects have minimal exit options — they cannot negotiate their way across the boundary through marginal adjustments. However, they can (theoretically) engage in litigation or advocacy; suppression is structural but not total. Theater ratio (0.65): Moderate-high. The performative element is substantial. Most legal systems retain specific cutoff numbers largely through path dependency — age 21 for alcohol use in the US, age 18 for majority in many jurisdictions, $X thresholds for small-claims court. These numbers are defended as 'natural' or 'optimal' but are empirically arbitrary: other democracies use different thresholds with equal success. The theater has increased over time as alternative approaches (graduated benefits, risk-based frameworks) have become technically feasible but remain institutionally sticky.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. The legal administrator sees a Rope — cutoffs solve the genuine coordination problem of vagueness. The edge-case subject sees a Snare — they bear full discontinuity cost with no escape. The organized coalition sees a Scaffold — graduated transitions and contextual judgment are technically and politically feasible alternatives; the sharp cutoff is temporary. The legislative process sees a Piton — the ritual of setting cutoffs persists through inertia, not principled defense. The analytical observer risks seeing a Mountain — vagueness is a logical problem, cutoffs are necessary. But structural data contradicts this: extractiveness and suppression are institutional features, not logical necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   Edge-case subject: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction — subject bears discontinuity cost and cannot negotiate across boundary. Legal administration: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary — gains efficiency and clarity. Policy-affected population: Victim + constrained → d≈0.68, f(d)≈1.04. Significant extraction — many near-threshold; some can appeal or requalify. Reform coalition: Organized + constrained → d≈0.38, f(d)≈0.36. Low effective extraction — coalition has agency and sees path forward through graduated systems. Legislative process: Institutional + arbitrage → d≈0.08, f(d)≈-0.10. Beneficiary via path dependency and change-resistance coordination. Analytical observer: analytical → d≈0.72, f(d)≈1.15. Mountain perspective is perspectival, not structural.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that the logical problem (sorites paradox is real) and the extraction mechanism (sharp cutoffs harm edge cases) are distinct. The sorites paradox is a genuine mathematical constraint: vague predicates cannot be resolved into precise boundaries without discontinuity somewhere. But the institutional application — using arbitrary sharp cutoffs rather than graduated transitions, contextual judgment, or multi-valued logic — is a choice. The mandatrophy would occur if the engine classified the constraint as a Mountain (logical necessity) without examining whether the institutional response (sharp cutoffs specifically) is optimal. The false summit detector catches this: the engine compares the logical claim (vagueness is irreducible) with the structural data (extractiveness=0.38, suppression=0.52, theater=0.65) and finds mismatch. The logical constraint is real; the extraction mechanism is institutional. The constraint should classify as Tangled Rope from most perspectives (genuine coordination function + significant extraction cost), not as a Mountain that naturalizes institutional choice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vagueness_reducibility,
    'Is vagueness in natural language predicates fundamentally irreducible, or merely epistemically unavoidable?',
    'Logical analysis of vague predicate semantics; comparison of multi-valued logic systems vs classical bivalent frameworks; empirical testing of whether contextual judgment scales to complex policy domains',
    'If irreducible: sharp cutoffs are necessary evil (constraint is closer to Mountain). If reducible: sharp cutoffs are institutional choice (constraint is Tangled Rope from all perspectives).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(vagueness_reducibility, conceptual, 'Whether vagueness in predicates is logically irreducible or institutionally chosen').

omega_variable(
    cost_of_graduated_boundaries,
    'What is the true institutional cost (in terms of litigation, interpretation disputes, administrative complexity) of replacing sharp cutoffs with graduated transitions or contextual judgment?',
    'Comparative case study of jurisdictions using graduated benefits (phase-out ranges, sliding scales) vs sharp cutoffs; measurement of appeal/dispute rates, administrative overhead, interpretation consistency',
    'If cost is low (<5% increase in administrative burden): scaffold perspective is validated, sunset is realistic. If cost is high (>25% increase): sharp cutoff persistence is explained by genuine coordination problem, not just institutional inertia.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_of_graduated_boundaries, empirical, 'Administrative cost differential between sharp cutoffs and graduated thresholds').

omega_variable(
    fairness_aggregation_threshold,
    'How many edge-case victims (those disadvantaged by sharp cutoff proximity) must exist for the constraint to shift from Rope (coordination) to Snare (extraction) classification?',
    'Demographic analysis of population distribution near policy thresholds; measurement of density clustering; identification of whether edge cases are statistical noise or systematic population cohort',
    'If edge cases <1% of affected population: Rope classification dominates (coordination benefit exceeds extraction cost). If >10%: Snare classification becomes unavoidable (systematic extraction). 1-10% is the perspectival gap zone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fairness_aggregation_threshold, empirical, 'Population density threshold distinguishing noise from systematic harm at boundaries').

omega_variable(
    judgment_reliability_tradeoff,
    'Can contextual judgment systems (case-by-case evaluation, judicial discretion, administrative review) reliably substitute for sharp cutoffs, or do they introduce new extraction mechanisms through inconsistency?',
    'Empirical comparison of variance in outcomes: sharp cutoff systems vs discretionary systems; measurement of appellate reversal rates, inter-judge consistency, and evidence of discriminatory application patterns',
    'If judgment systems are reliable: sharp cutoffs are unnecessary extraction mechanism (Snare). If judgment systems introduce new extraction (discriminatory application, wealth-dependent outcomes): both options are Snares; constraint is structural to legal systems, not specific to cutoffs.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(judgment_reliability_tradeoff, empirical, 'Whether contextual judgment reliably replaces sharp cutoffs without introducing new extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sorites_paradox, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sori_tr_t0, sorites_paradox, theater_ratio, 0, 0.4).
narrative_ontology:measurement(sori_tr_t3, sorites_paradox, theater_ratio, 3, 0.52).
narrative_ontology:measurement(sori_tr_t6, sorites_paradox, theater_ratio, 6, 0.65).

% Extraction over time
narrative_ontology:measurement(sori_be_t0, sorites_paradox, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(sori_be_t3, sorites_paradox, base_extractiveness, 3, 0.3).
narrative_ontology:measurement(sori_be_t6, sorites_paradox, base_extractiveness, 6, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sorites_paradox, enforcement_mechanism).
narrative_ontology:affects_constraint(sorites_paradox, benefit_cliff_extraction).
narrative_ontology:affects_constraint(sorites_paradox, regulatory_discontinuity_harm).
narrative_ontology:affects_constraint(sorites_paradox, age_cutoff_arbitrariness).

% DUAL FORMULATION NOTE:
% The Sorites Paradox constraint decomposes into two structurally distinct claims: (1) the logical/mathematical problem (vague predicates are irreducible), which is a genuine Mountain; (2) the institutional application (sharp cutoffs as solution), which is a Tangled Rope. The logical problem (ε≈0.05, Mountain) is downstream; the institutional application (ε=0.38, Tangled Rope) is the primary constraint modeled here. This story focuses on institutional application; logical irresolvability is a separate upstream constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sorites_paradox, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
