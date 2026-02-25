% ============================================================================
% CONSTRAINT STORY: academic_predatory_publishing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% Status: [ACTIVE, MANDATROPHY RESOLVED]
% ============================================================================

:- module(constraint_academic_predatory_publishing, []).

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
 *   constraint_id: academic_predatory_publishing
 *   human_readable: Academic Predatory Publishing Model
 *   domain: social/economic
 *
 * SUMMARY:
 *   The 'predatory publishing' model is a parasitic system that exploits the
 *   'publish or perish' culture in modern academia. These entities create
 *   journals, often with deceptive titles mimicking legitimate publications,
 *   and charge authors Article Processing Charges (APCs) for publication.
 *   However, they provide little to no genuine peer review or editorial
 *   services, effectively selling a worthless line on a CV. This primarily
 *   targets early-career researchers and academics in the Global South, who
 *   are under the most intense pressure to publish in international journals
 *   to secure tenure and career advancement.
 *
 * KEY AGENTS:
 *   - Early-Career Researchers / Global South Academics: Primary victims (powerless/trapped) who pay APCs under career duress.
 *   - Predatory Publishing Operators: Primary beneficiaries (organized/arbitrage) who collect APCs with minimal operating costs.
 *   - University Tenure Committees: Institutional enforcers (institutional/constrained) who perpetuate the 'publish or perish' metric.
 *   - The Scientific Record: Abstract victim, polluted with unvetted research.
 *   - Journal Indexing Services: Analytical observers attempting to create whitelists/blacklists.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(academic_predatory_publishing, 0.85).
domain_priors:suppression_score(academic_predatory_publishing, 0.8).
domain_priors:theater_ratio(academic_predatory_publishing, 0.9).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(academic_predatory_publishing, extractiveness, 0.85).
narrative_ontology:constraint_metric(academic_predatory_publishing, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(academic_predatory_publishing, theater_ratio, 0.9).

% --- Constraint claim ---
narrative_ontology:constraint_claim(academic_predatory_publishing, snare).
narrative_ontology:human_readable(academic_predatory_publishing, "Academic Predatory Publishing Model").
narrative_ontology:topic_domain(academic_predatory_publishing, "social/economic").

domain_priors:requires_active_enforcement(academic_predatory_publishing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(academic_predatory_publishing, predatory_publishing_operators).
narrative_ontology:constraint_victim(academic_predatory_publishing, early_career_researchers).
narrative_ontology:constraint_victim(academic_predatory_publishing, global_south_academics).
narrative_ontology:constraint_victim(academic_predatory_publishing, scientific_record).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE TARGET (SNARE) — An early-career researcher, often in a developing nation, faces immense 'publish or perish' pressure. Lacking funds, guidance, and time, they are susceptible to deceptive journals offering rapid publication for a fee (APC). The resulting publication is worthless or damaging, and the fee is lost. Their career progression is trapped by publication metrics. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈1.45. This is a pure extractive trap.
constraint_indexing:constraint_classification(academic_predatory_publishing, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE) — The operator of the predatory journal sees their business as a simple market transaction. They provide a service (a publication line-item) that researchers demand, for a price. From this perspective, it's a coordination mechanism between supply and demand with zero coercion. They can easily shut down journals and create new ones to evade blacklists. d≈0.15, f(d)≈-0.01, σ=1.2 → χ≈-0.01. The negative extraction signifies a pure subsidy to them.
constraint_indexing:constraint_classification(academic_predatory_publishing, rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ENFORCER (PITON) — University promotion and tenure committees enforce the 'publish or perish' norm, often relying on publication quantity as a proxy for research productivity. This inertial metric, detached from its original purpose of ensuring quality, creates the demand that predatory publishers exploit. Their function is largely theatrical compliance with an outdated standard. The high theater_ratio (0.90) secures the Piton classification.
constraint_indexing:constraint_classification(academic_predatory_publishing, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: THE OBSERVER (SNARE) — An analyst at a journal indexing service (like DOAJ or Cabell's) sees the full structure: high extraction of fees for no value, high suppression of alternatives due to career pressure, and active deception. The system is unambiguously a Snare that damages the integrity of the scientific record. This aligns with the system's claimed_type. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈1.17.
constraint_indexing:constraint_classification(academic_predatory_publishing, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(academic_predatory_publishing_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(academic_predatory_publishing, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(academic_predatory_publishing, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(academic_predatory_publishing, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(academic_predatory_publishing, TR),
    TR >= 0.70.

:- end_tests(academic_predatory_publishing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.85) is extremely high, as the APC is almost pure profit, with no value returned in the form of legitimate peer review, editing, or prestige. Suppression (0.80) is high because for many researchers, the alternative to publishing (even in a dubious journal) is career stagnation or termination. Theater Ratio (0.90) is also extremely high, as the entire enterprise is a performance of academic publishing—fake editorial boards, sham peer review—without any of the actual substance.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. The trapped researcher experiences a high-cost, high-coercion Snare. The publisher, operating with arbitrage-level freedom, views their activity as a simple, voluntary Rope that coordinates a researcher's need with their service. Meanwhile, the university committee that creates the demand for this 'service' perceives their role through the lens of a Piton—an inertial, performative adherence to a degraded institutional norm (publication volume as a proxy for quality).
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is driven by the extreme asymmetry in power and exit options. The victims ('early_career_researchers') are trapped, leading to a maximal directionality value (d≈0.95) and thus very high effective extraction (χ). The beneficiaries ('predatory_publishing_operators') have arbitrage exit, leading to a minimal directionality value (d≈0.15) and thus a negative effective extraction from their perspective, classifying it as a Rope.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a textbook resolution of mandatrophy. A surface-level analysis might see a 'market' where a 'service' is purchased. However, the framework's focus on structural properties—the extreme suppression (coercion) and near-total lack of value delivery (high ε)—correctly pierces this veil to classify the structure as a Snare. It demonstrates how a Rope-like appearance (a voluntary transaction) can mask a fundamentally extractive and coercive reality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    responsibility_locus,
    'Is the primary driver of this constraint the supply of predatory journals or the institutional demand for publication volume?',
    'Empirical studies analyzing researcher behavior in institutions that shift evaluation criteria from publication quantity to quality, assessing if this reduces submissions to predatory outlets.',
    'If demand-side incentives are the primary driver, the root constraint is ''academic_tenure_process'' (likely a Tangled Rope), making this Snare a downstream effect. If it''s purely supply-side opportunism, this is a standalone Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(responsibility_locus, empirical, 'Whether the root cause is supply-side predation or demand-side academic incentives').

omega_variable(
    open_access_culpability,
    'To what extent did the well-intentioned shift to author-pays ''Gold Open Access'' models create the economic niche for this form of predation?',
    'Comparative historical analysis of predatory publishing rates pre- and post- a wide adoption of the APC model, controlling for other factors like the growth of online publishing.',
    'If the APC model is a strong causal factor, then the ''gold_open_access'' constraint itself might be classifiable as a Tangled Rope, with this Snare being a predictable, extractive consequence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(open_access_culpability, conceptual, 'Causality between the Gold Open Access model and the rise of predatory publishing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(academic_predatory_publishing, 2005, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acad_tr_t2005, academic_predatory_publishing, theater_ratio, 2005, 0.7).
narrative_ontology:measurement(acad_tr_t2015, academic_predatory_publishing, theater_ratio, 2015, 0.82).
narrative_ontology:measurement(acad_tr_t2025, academic_predatory_publishing, theater_ratio, 2025, 0.9).

% Extraction over time
narrative_ontology:measurement(acad_be_t2005, academic_predatory_publishing, base_extractiveness, 2005, 0.6).
narrative_ontology:measurement(acad_be_t2015, academic_predatory_publishing, base_extractiveness, 2015, 0.75).
narrative_ontology:measurement(acad_be_t2025, academic_predatory_publishing, base_extractiveness, 2025, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(academic_predatory_publishing, information_standard).
narrative_ontology:affects_constraint(academic_predatory_publishing, academic_tenure_process).
narrative_ontology:affects_constraint(academic_predatory_publishing, global_research_inequality).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
