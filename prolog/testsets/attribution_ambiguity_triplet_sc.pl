% ============================================================================
% CONSTRAINT STORY: attribution_ambiguity_triplet_sc
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-16
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
 *   In condensed matter physics, a key signature for triplet
 *   superconductivity—the inverse spin-valve effect—is difficult to
 *   definitively attribute. The observed signal could arise from the novel
 *   physics claimed, but it could also be an artifact of complex interface
 *   effects, magnetic impurities, or other confounding phenomena. This
 *   ambiguity creates a constraint on scientific progress, forcing the
 *   community to expend significant resources to disentangle the true cause.
 *
 * KEY AGENTS:
 *   - Original Triplet Superconductivity Claimants: The research group(s) that first reported the effect (Victim; powerless/trapped).
 *   - Proponents of Alternative Explanations: Competing theorists and experimentalists who propose and test alternative, more conventional explanations (Beneficiary; organized/mobile).
 *   - Funding Agencies and Journals: Institutional actors who must adjudicate the debate through peer review and resource allocation (Institutional; constrained).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(attribution_ambiguity_triplet_sc, 0.42).
domain_priors:suppression_score(attribution_ambiguity_triplet_sc, 0.48).
domain_priors:theater_ratio(attribution_ambiguity_triplet_sc, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(attribution_ambiguity_triplet_sc, extractiveness, 0.42).
narrative_ontology:constraint_metric(attribution_ambiguity_triplet_sc, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(attribution_ambiguity_triplet_sc, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(attribution_ambiguity_triplet_sc, tangled_rope).
narrative_ontology:human_readable(attribution_ambiguity_triplet_sc, "Attribution Ambiguity in Triplet Superconductivity Claims").
narrative_ontology:topic_domain(attribution_ambiguity_triplet_sc, "physics/condensed_matter").

domain_priors:requires_active_enforcement(attribution_ambiguity_triplet_sc).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(attribution_ambiguity_triplet_sc, proponents_of_alternative_explanations).
narrative_ontology:constraint_victim(attribution_ambiguity_triplet_sc, original_triplet_superconductivity_claimants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: The original research group whose claim is under scrutiny. The ambiguity traps their reputation and research program, extracting significant resources to defend the claim against a global community of skeptics.
constraint_indexing:constraint_classification(attribution_ambiguity_triplet_sc, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: Competing research groups or theorists proposing alternative explanations. For them, the ambiguity is a pure coordination problem—a puzzle to be solved—that creates research opportunities and publications. They can easily exit to other problems if their alternative theory is disproven.
constraint_indexing:constraint_classification(attribution_ambiguity_triplet_sc, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: An institution like a funding agency or journal. They must coordinate the peer review process (Rope) but are constrained by the high-cost, high-risk nature of the debate, where funding either side could be a waste (Snare).
constraint_indexing:constraint_classification(attribution_ambiguity_triplet_sc, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: The analytical observer sees the full structure. The scientific method is coordinating a search for truth (Rope function), but this process imposes severe, asymmetric costs on the original claimants (extractive Snare function).
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
    constraint_indexing:constraint_classification(attribution_ambiguity_triplet_sc, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(attribution_ambiguity_triplet_sc_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (ε=0.42) represents the high cost (in research funding, time, and reputational capital) imposed on the original claimants to defend their discovery against plausible alternatives. The suppression score (0.48) reflects how the unresolved ambiguity chills further research in the area, as other groups are hesitant to build upon a potentially flawed foundation. Active enforcement is the scientific process itself: peer review, replication attempts, and the publication of competing theories.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For the original claimants, the ambiguity is a Snare that has trapped their research program. For competitors, it's a Rope—a fascinating scientific puzzle that coordinates the community's efforts and provides opportunities for discovery. The analytical view recognizes both the vital coordination function of scientific skepticism and its highly extractive, asymmetric cost structure, classifying it as a Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   The victims are the 'original_triplet_superconductivity_claimants', who bear the full cost of defending the claim. Their directionality is high (d≈1.0). The beneficiaries are the 'proponents_of_alternative_explanations', who gain from the research opportunity created by the ambiguity. Their directionality is low (d≈0.15). This asymmetry is the core of the Tangled Rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids mandatrophy by correctly identifying a situation with both a genuine coordination function (the scientific search for truth) and a severe, asymmetric extraction component. A simpler analysis might label it a Snare from the victim's view or a Rope from the beneficiary's, but both would be incomplete. The Tangled Rope classification captures the reality that a valuable social process (scientific debate) can simultaneously be highly predatory towards specific actors within that process.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intrinsic_vs_extrinsic_origin,
    'Is the observed inverse spin-valve effect an intrinsic property of the superconducting condensate, or an extrinsic artifact of specific material interfaces and device fabrication?',
    'Systematic experiments with a wide range of interface materials and geometries that show the effect is either universal (intrinsic) or interface-dependent (extrinsic).',
    'If resolved as intrinsic, the constraint could become a Mountain (a new physical principle). If resolved as extrinsic, it becomes a Piton (a historical artifact of early experimental setups).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intrinsic_vs_extrinsic_origin, empirical, 'The core ambiguity between an intrinsic physical law and an extrinsic experimental artifact.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(attribution_ambiguity_triplet_sc, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(attr_tr_t0, attribution_ambiguity_triplet_sc, theater_ratio, 0, 0.15).
narrative_ontology:measurement(attr_tr_t5, attribution_ambiguity_triplet_sc, theater_ratio, 5, 0.3).
narrative_ontology:measurement(attr_tr_t10, attribution_ambiguity_triplet_sc, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(attr_be_t0, attribution_ambiguity_triplet_sc, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(attr_be_t5, attribution_ambiguity_triplet_sc, base_extractiveness, 5, 0.33).
narrative_ontology:measurement(attr_be_t10, attribution_ambiguity_triplet_sc, base_extractiveness, 10, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(attribution_ambiguity_triplet_sc, information_standard).
narrative_ontology:affects_constraint(attribution_ambiguity_triplet_sc, topological_quantum_computing_platforms).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
