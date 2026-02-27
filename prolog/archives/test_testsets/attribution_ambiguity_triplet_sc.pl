% ============================================================================
% CONSTRAINT STORY: attribution_ambiguity_triplet_sc
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_attribution_ambiguity_triplet_sc, []).

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
 *   constraint_id: attribution_ambiguity_triplet_sc
 *   human_readable: Attribution Ambiguity in Triplet Superconductivity Claims
 *   domain: physics/condensed_matter
 *
 * SUMMARY:
 *   In condensed matter physics, the 'inverse spin-valve effect' is a key
 *   experimental signature used to claim the discovery of triplet
 *   superconductivity. However, this signature is not definitive; other
 *   physical phenomena, such as magnetic scattering or interface effects, can
 *   mimic the signal. This attribution ambiguity creates a structural
 *   constraint where research groups can claim a major discovery based on
 *   evidence that is difficult to falsify, capturing prestige and funding
 *   during the prolonged period of uncertainty.
 *
 * KEY AGENTS:
 *   - Claiming Research Groups: Primary beneficiary (institutional/arbitrage) - Gains first-mover advantage, citations, and funding from the ambiguous claim.
 *   - Field Epistemic Clarity: Primary victim (powerless/trapped) - The collective knowledge base is polluted by claims that are not definitively proven, leading to misallocated research efforts.
 *   - Competing Research Groups: Secondary victims (organized/constrained) - Must expend resources to replicate or challenge the claim, operating within the framework set by the initial, ambiguous result.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(attribution_ambiguity_triplet_sc, 0.52).
domain_priors:suppression_score(attribution_ambiguity_triplet_sc, 0.65).
domain_priors:theater_ratio(attribution_ambiguity_triplet_sc, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(attribution_ambiguity_triplet_sc, extractiveness, 0.52).
narrative_ontology:constraint_metric(attribution_ambiguity_triplet_sc, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(attribution_ambiguity_triplet_sc, theater_ratio, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(attribution_ambiguity_triplet_sc, tangled_rope).
narrative_ontology:human_readable(attribution_ambiguity_triplet_sc, "Attribution Ambiguity in Triplet Superconductivity Claims").
narrative_ontology:topic_domain(attribution_ambiguity_triplet_sc, "physics/condensed_matter").

domain_priors:requires_active_enforcement(attribution_ambiguity_triplet_sc).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(attribution_ambiguity_triplet_sc, claiming_research_groups).
narrative_ontology:constraint_victim(attribution_ambiguity_triplet_sc, field_epistemic_clarity).
narrative_ontology:constraint_victim(attribution_ambiguity_triplet_sc, competing_research_groups).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FIELD EPISTEMIC CLARITY (SNARE) — The collective knowledge base is trapped. It cannot reject ambiguous claims, which pollute the literature and misdirect research efforts. It bears the full cost of this ambiguity without recourse. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.88.
constraint_indexing:constraint_classification(attribution_ambiguity_triplet_sc, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CLAIMING RESEARCH GROUP (ROPE) — From the perspective of the group making the initial claim, the ambiguity is a feature of cutting-edge science. They see their publication as a pure coordination act: signaling a promising result for the community to investigate. They benefit from first-mover advantage and can exit to other research topics. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.08.
constraint_indexing:constraint_classification(attribution_ambiguity_triplet_sc, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: COMPETING RESEARCH GROUP (TANGLED ROPE) — Competitors are constrained; they must invest significant resources to verify or refute the ambiguous claim. However, they also benefit from the coordination function of the initial publication, which defines a new research direction. d≈0.60, f(d)≈0.85, σ=1.0 → χ≈0.44.
constraint_indexing:constraint_classification(attribution_ambiguity_triplet_sc, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (TANGLED ROPE) — The analyst sees both the coordination function (announcing a potential discovery) and the asymmetric extraction (career and funding benefits accruing to the claiming group due to the ambiguity). The classification matches the claimed_type. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.72.
constraint_indexing:constraint_classification(attribution_ambiguity_triplet_sc, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(attribution_ambiguity_triplet_sc_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(attribution_ambiguity_triplet_sc, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(attribution_ambiguity_triplet_sc, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(attribution_ambiguity_triplet_sc, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(attribution_ambiguity_triplet_sc_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.52): High. The career and funding advantages for being the 'first' to report a candidate triplet superconductor are substantial. The ambiguity allows this advantage to be maintained for years before a consensus is reached. Suppression (0.65): High. Refuting the claim requires building equally complex, expensive experiments and publishing a 'negative' result, which faces institutional hurdles. Theater Ratio (0.60): The peer review process for such claims contains significant theater. Reviewers can validate the experimental procedure but cannot resolve the core attribution ambiguity, making the verification ritual partially performative.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. The claiming group experiences the constraint as a pure coordination mechanism (Rope), where they are simply reporting their findings to the community. For the field's collective knowledge, however, the ambiguity is a Snare, trapping resources and attention on a potentially false path. Competing groups and analytical observers perceive the mixed reality of a Tangled Rope: the initial claim does coordinate research, but it does so in a way that asymmetrically benefits the claimants at the expense of others.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is derived from the structural roles. The 'claiming_research_groups' are beneficiaries with arbitrage exit, yielding a low 'd' and a Rope classification. The 'field_epistemic_clarity' is a victim with trapped exit, yielding a high 'd' and a Snare classification. 'Competing_research_groups' are victims but have agency and are constrained (not trapped), leading to an intermediate 'd' and a Tangled Rope classification. This demonstrates how fixed base properties can generate the full range of classifications based on the observer's position.
 *
 * MANDATROPHY ANALYSIS:
 *   This case resolves a potential mandatrophy by correctly identifying the structure as a Tangled Rope from the analytical perspective. To classify it as a pure Snare would be to ignore the genuine scientific coordination that occurs when a novel, albeit ambiguous, result is published. To classify it as a pure Rope would be to ignore the significant, asymmetric extraction of career capital that the ambiguity enables. The Tangled Rope classification acknowledges that the coordination and extraction functions are inseparable aspects of the same underlying structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    confounding_mechanisms,
    'Can the observed inverse spin-valve effect be definitively distinguished from confounding mechanisms like interface magnetism or other proximity effects?',
    'Development of new experimental probes or theoretical models that can uniquely identify signatures of triplet pairing versus alternative explanations.',
    'If distinguishable, the constraint dissolves into a standard verification problem (Rope). If fundamentally indistinguishable with current methods, the Snare/Tangled Rope structure persists.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(confounding_mechanisms, empirical, 'Whether the primary signature can be isolated from confounding physical effects.').

omega_variable(
    standard_of_evidence,
    'What is the appropriate community standard of evidence for an extraordinary claim based on an ambiguous signature?',
    'Community consensus established through workshops, review articles, and editorial policies requiring multiple independent lines of evidence.',
    'A higher standard would reduce the extractive potential (lowering ε), shifting the constraint towards a Rope. A lower standard maintains the high-extraction Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(standard_of_evidence, preference, 'The community''s accepted standard of evidence for triplet superconductivity claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(attribution_ambiguity_triplet_sc, 2005, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(attr_tr_t2005, attribution_ambiguity_triplet_sc, theater_ratio, 2005, 0.4).
narrative_ontology:measurement(attr_tr_t2015, attribution_ambiguity_triplet_sc, theater_ratio, 2015, 0.5).
narrative_ontology:measurement(attr_tr_t2025, attribution_ambiguity_triplet_sc, theater_ratio, 2025, 0.6).

% Extraction over time
narrative_ontology:measurement(attr_be_t2005, attribution_ambiguity_triplet_sc, base_extractiveness, 2005, 0.35).
narrative_ontology:measurement(attr_be_t2015, attribution_ambiguity_triplet_sc, base_extractiveness, 2015, 0.45).
narrative_ontology:measurement(attr_be_t2025, attribution_ambiguity_triplet_sc, base_extractiveness, 2025, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(attribution_ambiguity_triplet_sc, information_standard).
narrative_ontology:affects_constraint(attribution_ambiguity_triplet_sc, verification_bottleneck).

% DUAL FORMULATION NOTE:
% This constraint is a specific instance of the more general 'verification_bottleneck' in experimental science. Its unique ε value is determined by the specific physics of triplet superconductivity and its ambiguous signatures, distinguishing it from verification issues in other fields.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
