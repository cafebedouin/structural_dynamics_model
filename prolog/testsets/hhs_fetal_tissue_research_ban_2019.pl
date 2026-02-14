% ============================================================================
% CONSTRAINT STORY: hhs_fetal_tissue_research_ban_2019
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_hhs_fetal_tissue_research_ban_2019, []).

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
    domain_priors:emerges_naturally/1.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hhs_fetal_tissue_research_ban_2019
 *   human_readable: "2019 HHS Ban on Fetal Tissue Research Funding"
 *   domain: Political / Scientific
 *
 * SUMMARY:
 *   In June 2019, the U.S. Department of Health and Human Services (HHS)
 *   under the Trump administration halted all new internal (intramural)
 *   research at the National Institutes of Health (NIH) that used human fetal
 *   tissue. It also imposed a new, stringent ethics review process for all
 *   external (extramural) grant applications, effectively suppressing a
 *   critical line of biomedical research into diseases like HIV, Alzheimer's,
 *   and cancer. The policy was enacted following sustained pressure from
 *   anti-abortion advocacy groups.
 *
 * KEY AGENTS (by structural relationship):
 *   - Biomedical Research Community: Primary target (powerless/trapped) — bears the full cost of the constraint through loss of funding, materials, and research progress.
 *   - Anti-Abortion Advocacy Groups: Primary beneficiary (organized/arbitrage) — achieves a key ideological goal by aligning federal policy with their moral framework.
 *   - HHS Administration (2019): Institutional beneficiary (institutional/arbitrage) — implements the policy to satisfy its political constituency.
 *   - Analytical Observer: Analytical observer — sees the full structure of political extraction imposed on a scientific domain.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hhs_fetal_tissue_research_ban_2019, 0.75).
domain_priors:suppression_score(hhs_fetal_tissue_research_ban_2019, 0.80).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(hhs_fetal_tissue_research_ban_2019, 0.40).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hhs_fetal_tissue_research_ban_2019, extractiveness, 0.75).
narrative_ontology:constraint_metric(hhs_fetal_tissue_research_ban_2019, suppression_requirement, 0.80).
narrative_ontology:constraint_metric(hhs_fetal_tissue_research_ban_2019, theater_ratio, 0.40).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(hhs_fetal_tissue_research_ban_2019, snare).

% --- Binary flags ---
domain_priors:requires_active_enforcement(hhs_fetal_tissue_research_ban_2019).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(hhs_fetal_tissue_research_ban_2019, anti_abortion_advocacy_groups).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(hhs_fetal_tissue_research_ban_2019, biomedical_research_community).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (THE RESEARCHER)
% Agent whose work is directly halted. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
% This is a direct, coercive extraction of their ability to conduct research.
constraint_indexing:constraint_classification(hhs_fetal_tissue_research_ban_2019, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (THE ADVOCACY GROUP)
% Agent who lobbied for the policy. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ
% From their perspective, this is a successful coordination effort to align
% federal spending with their moral principles, a pure Rope.
constraint_indexing:constraint_classification(hhs_fetal_tissue_research_ban_2019, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Sees the coercive nature, the high suppression of alternatives, and the
% asymmetric cost/benefit structure.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(hhs_fetal_tissue_research_ban_2019, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hhs_fetal_tissue_research_ban_2019_tests).

test(perspectival_gap_is_snare_vs_rope, [nondet]) :-
    constraint_indexing:constraint_classification(hhs_fetal_tissue_research_ban_2019, snare,
        context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(hhs_fetal_tissue_research_ban_2019, rope,
        context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(analytical_view_is_snare, [nondet]) :-
    constraint_indexing:constraint_classification(hhs_fetal_tissue_research_ban_2019, snare,
        context(agent_power(analytical), _, _, _)).

test(threshold_validation_high_extraction, [nondet]) :-
    domain_priors:base_extractiveness(hhs_fetal_tissue_research_ban_2019, E),
    domain_priors:suppression_score(hhs_fetal_tissue_research_ban_2019, S),
    E >= 0.66,
    S >= 0.60.

:- end_tests(hhs_fetal_tissue_research_ban_2019_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.75): The policy directly extracts a critical
 *     resource (funding, research materials, career opportunities) from one
 *     group (scientists) to provide a non-material, ideological benefit to
 *     another (political advocates). The cost to science is concrete and high.
 *   - Suppression (0.80): The US government holds a near-monopoly on large-scale
 *     biomedical research funding. For affected researchers, there were few or no
 *     viable alternatives to continue their work, making this highly coercive.
 *   - Theater (0.40): While the policy has a strong functional component (it
 *     actually stops research), its announcement and justification are heavily
 *     performative, framed in moral terms for a political audience.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For the biomedical research community, the constraint is
 *   a Snare that traps them, halting progress and derailing careers for reasons
 *   they see as scientifically baseless. For the anti-abortion advocates, the
 *   constraint is a Rope—a successful coordination of public policy to reflect
 *   their moral values, with no perceived downside. The system correctly
 *   captures this by classifying the same object as Snare and Rope depending on
 *   the index.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `anti_abortion_advocacy_groups` explicitly lobbied for and
 *     celebrated this policy. They are the clear structural beneficiaries.
 *   - Victim: `biomedical_research_community` is the direct target. Their
 *     funding was cut, and their research materials were restricted. Their
 *     exit options are `trapped` because shifting a lifelong research program
 *     is not feasible in the short term.
 *   The engine uses these declarations to derive high directionality (d≈0.95)
 *   for researchers, amplifying extraction, and low directionality (d≈0.05)
 *   for advocates, making extraction appear negative (i.e., beneficial).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a classic example of what Deferential Realism is designed
 *   to disambiguate. A naive analysis might see it as "policy coordination"
 *   (a Rope). However, the high ε, high suppression, and clear victim/beneficiary
 *   asymmetry reveal its true nature as a Snare. The analytical perspective
 *   adopts the Snare classification, preventing the mislabeling of this
 *   extractive political act as a benign coordination mechanism. It correctly
 *   identifies that no genuine collective action problem is being solved for
 *   the group bearing the costs. [RESOLVED MANDATROPHY]
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_hhs_fetal_tissue_research_ban_2019,
    'Does the long-term societal cost from delayed or foregone medical cures due to this ban exceed the ideological/moral benefit perceived by its proponents?',
    'Longitudinal (multi-decade) public health and economic studies comparing progress in affected research fields vs. unaffected fields.',
    'If cost > benefit, the constraint is a net-negative Snare at a civilizational scale. If cost <= benefit, it could be argued as a justifiable, albeit extractive, values-based policy choice.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(hhs_fetal_tissue_research_ban_2019, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This policy was the culmination of years of increasing political pressure.
% The lifecycle data models this intensification, where rhetoric (theater)
% was gradually converted into functional extraction.
% Required because base_extractiveness (0.75) > 0.46.

% Theater ratio over time (starts as rhetoric, becomes functional):
narrative_ontology:measurement(hhs_ban_tr_t0, hhs_fetal_tissue_research_ban_2019, theater_ratio, 0, 0.60).
narrative_ontology:measurement(hhs_ban_tr_t5, hhs_fetal_tissue_research_ban_2019, theater_ratio, 5, 0.50).
narrative_ontology:measurement(hhs_ban_tr_t10, hhs_fetal_tissue_research_ban_2019, theater_ratio, 10, 0.40).

% Extraction over time (starts as political pressure, becomes a full ban):
narrative_ontology:measurement(hhs_ban_ex_t0, hhs_fetal_tissue_research_ban_2019, base_extractiveness, 0, 0.20).
narrative_ontology:measurement(hhs_ban_ex_t5, hhs_fetal_tissue_research_ban_2019, base_extractiveness, 5, 0.50).
narrative_ontology:measurement(hhs_ban_ex_t10, hhs_fetal_tissue_research_ban_2019, base_extractiveness, 10, 0.75).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% This constraint is not a coordination mechanism; it is a disruption of one.
% Therefore, coordination_type is not declared.

% This policy is part of a broader pattern of political interference in
% scientific funding and regulation.
narrative_ontology:affects_constraint(hhs_fetal_tissue_research_ban_2019, politicization_of_science_funding).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The structural derivation
% from beneficiary/victim declarations and exit options accurately models the
% directionality for the key agents involved. The standard derivation chain
% correctly assigns a high d-value to the trapped researchers and a low
% d-value to the institutional beneficiaries.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */