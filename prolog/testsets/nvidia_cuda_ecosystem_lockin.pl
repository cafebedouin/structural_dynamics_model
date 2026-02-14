% ============================================================================
% CONSTRAINT STORY: nvidia_cuda_ecosystem_lockin
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_nvidia_cuda_ecosystem_lockin, []).

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
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: nvidia_cuda_ecosystem_lockin
 *   human_readable: Nvidia CUDA Ecosystem Lock-in
 *   domain: technological
 *
 * SUMMARY:
 *   Nvidia's integrated hardware (GPUs) and proprietary software ecosystem (CUDA)
 *   create a high-performance standard for AI development. While this offers
 *   significant coordination benefits (a stable, powerful, well-documented
 *   platform), the extremely high costs of switching to alternative hardware
 *   and software stacks create a powerful lock-in effect. This enables Nvidia
 *   to extract significant value from the market and suppress competition.
 *
 * KEY AGENTS (by structural relationship):
 *   - AI Startups & Independent Developers: Primary target (powerless/trapped) — Must use the ecosystem to be competitive, but have little leverage.
 *   - Nvidia & its Shareholders: Primary beneficiary (institutional/arbitrage) — Captures enormous value from its dominant market position.
 *   - Large Cloud Providers (AWS, Azure, GCP): Inter-institutional actor (organized/constrained) — Are major customers and partners, but are also constrained by dependency on Nvidia's supply and roadmap, prompting investment in alternatives.
 *   - Systems Economists & Antitrust Regulators: Analytical observer — Sees the full structure of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Base extractiveness (ε) represents the total cost of being locked in:
% hardware price premiums, developer retraining, software migration, and the
% opportunity cost of not using a potentially cheaper/more open alternative.
domain_priors:base_extractiveness(nvidia_cuda_ecosystem_lockin, 0.65).

% Suppression score reflects how the ecosystem's network effects and proprietary
% nature actively hinder the viability of competing platforms (e.g., AMD's ROCm).
domain_priors:suppression_score(nvidia_cuda_ecosystem_lockin, 0.75).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(nvidia_cuda_ecosystem_lockin, 0.10).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nvidia_cuda_ecosystem_lockin, extractiveness, 0.65).
narrative_ontology:constraint_metric(nvidia_cuda_ecosystem_lockin, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(nvidia_cuda_ecosystem_lockin, theater_ratio, 0.10).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(nvidia_cuda_ecosystem_lockin, tangled_rope).

% --- Binary flags ---
% Required for Tangled Rope. Enforcement is not legal, but economic and
% technical: continuous R&D, developer relations, and strategic software
% choices that deepen the moat and reinforce the ecosystem's dominance.
domain_priors:requires_active_enforcement(nvidia_cuda_ecosystem_lockin).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(nvidia_cuda_ecosystem_lockin, nvidia_and_shareholders).
narrative_ontology:constraint_beneficiary(nvidia_cuda_ecosystem_lockin, ai_platform_engineers). % Benefit from a stable, documented standard.

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(nvidia_cuda_ecosystem_lockin, ai_startups_and_developers).
narrative_ontology:constraint_victim(nvidia_cuda_ecosystem_lockin, competing_hardware_vendors).
narrative_ontology:constraint_victim(nvidia_cuda_ecosystem_lockin, large_cloud_providers). % Also a victim due to dependency.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (AI STARTUP)
% For a startup, the ecosystem is a non-negotiable entry requirement. The high
% costs and lack of viable alternatives make it a Snare.
% Engine derives d from: victim + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42.
% With σ(global)=1.2, χ = 0.65 * 1.42 * 1.2 ≈ 1.11. This is a clear Snare.
constraint_indexing:constraint_classification(nvidia_cuda_ecosystem_lockin, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (NVIDIA)
% From Nvidia's perspective, this is a beneficial coordination mechanism
% that creates a stable market and platform.
% Engine derives d from: beneficiary + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12.
% χ = 0.65 * -0.12 * 1.2 ≈ -0.09. This is a clear Rope.
constraint_indexing:constraint_classification(nvidia_cuda_ecosystem_lockin, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% The analyst sees both the genuine coordination function (beneficiaries exist)
% and the severe asymmetric extraction (victims exist), along with the active
% enforcement that maintains the lock-in. This is the canonical Tangled Rope.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15.
% χ = 0.65 * 1.15 * 1.2 ≈ 0.90. Meets Tangled Rope thresholds.
constraint_indexing:constraint_classification(nvidia_cuda_ecosystem_lockin, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVE ---

% PERSPECTIVE 4: LARGE CLOUD PROVIDER (e.g., AWS, AZURE)
% They are both major beneficiaries (selling GPU access) and major victims
% (dependency, high costs). Their exit is 'constrained' as they can't switch
% overnight but are powerful enough to invest in alternatives (e.g., TPUs, Trainium).
% The engine derives a mid-range d, leading to a Tangled Rope classification that
% reflects their conflicted position.
constraint_indexing:constraint_classification(nvidia_cuda_ecosystem_lockin, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nvidia_cuda_ecosystem_lockin_tests).

test(perspectival_gap_is_snare_vs_rope) :-
    constraint_indexing:constraint_classification(nvidia_cuda_ecosystem_lockin, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nvidia_cuda_ecosystem_lockin, rope, context(agent_power(institutional), _, _, _)).

test(analytical_view_is_tangled_rope) :-
    constraint_indexing:constraint_classification(nvidia_cuda_ecosystem_lockin, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_requirements_met) :-
    narrative_ontology:constraint_beneficiary(nvidia_cuda_ecosystem_lockin, _),
    narrative_ontology:constraint_victim(nvidia_cuda_ecosystem_lockin, _),
    domain_priors:requires_active_enforcement(nvidia_cuda_ecosystem_lockin).

:- end_tests(nvidia_cuda_ecosystem_lockin_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.65): Set high to capture not just hardware price
 *     premiums but also the immense, implicit switching costs (developer
 *     retraining, software porting, performance risk) that constitute the
 *     lock-in. This is the value Nvidia extracts due to the lack of viable
 *     alternatives.
 *   - Suppression (0.75): The dominance of CUDA is not passive. Its vast library
 *     support, developer mindshare, and integration create strong network
 *     effects that actively suppress the growth and adoption of competing
 *     ecosystems like ROCm or oneAPI, making them perpetually lag behind.
 *
 * PERSPECTIVAL GAP:
 *   The gap is extreme. For a startup (powerless/trapped), the ecosystem is a
 *   Snare: a costly, unavoidable trap required for market participation. For
 *   Nvidia (institutional/arbitrage), it's a Rope: a beneficial standard they
 *   architected to coordinate the industry, creating a predictable and
 *   profitable market. The analytical view must reconcile these, hence
 *   Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: Nvidia and its shareholders capture direct financial returns.
 *     AI Platform Engineers also benefit from a stable, well-supported, and
 *     powerful standard that simplifies their work.
 *   - Victims: AI startups bear the high costs without leverage. Competing
 *     hardware vendors are effectively locked out of the high-end AI market.
 *     Large cloud providers are also victims, despite being customers, due to
 *     their deep strategic and financial dependency.
 *
 * MANDATROPHY ANALYSIS:
 *   This story is a classic case where the Tangled Rope classification is
 *   essential.
 *   - A pure 'Snare' classification would be inaccurate because it ignores the
 *     massive, genuine coordination function that CUDA provides. It solved a
 *     huge problem for the industry, which is why it was adopted.
 *   - A pure 'Rope' classification would be dangerously naive, ignoring the
 *     immense extractive power and anti-competitive effects of the lock-in.
 *   Tangled Rope correctly identifies that the constraint possesses BOTH a
 *   genuine coordination function AND severe asymmetric extraction, which are
 *   causally linked.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_nvidia_cuda_ecosystem_lockin,
    'Is the CUDA ecosystem''s dominance a result of persistent, superior innovation (a Mountain of engineering) or primarily due to anti-competitive network effect abuse (a Snare of market power)?',
    'A hypothetical scenario where a competitor releases a technically superior, open-source alternative with a seamless, automated migration path from CUDA. If adoption remains slow, it suggests the lock-in has deeper, more Mountain-like properties. If adoption is rapid, it confirms the Snare-like nature.',
    'Determines the appropriate regulatory response: intervention to foster competition (if Snare) versus allowing the market to reward the best innovator (if Mountain).',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nvidia_cuda_ecosystem_lockin, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This is a high-extraction constraint (ε=0.65), so temporal data is required.
% The timeline models the period from ~2012 (early AI adoption of CUDA) to
% the present day, showing how the lock-in intensified over time.
% The constraint started as a Rope (coordination tool) and evolved into a
% Tangled Rope as its dominance and extractive potential grew.

% Theater ratio over time (remains low; this is a functional system):
narrative_ontology:measurement(nvidia_tr_t0, nvidia_cuda_ecosystem_lockin, theater_ratio, 0, 0.05).
narrative_ontology:measurement(nvidia_tr_t5, nvidia_cuda_ecosystem_lockin, theater_ratio, 5, 0.08).
narrative_ontology:measurement(nvidia_tr_t10, nvidia_cuda_ecosystem_lockin, theater_ratio, 10, 0.10).

% Extraction over time (shows intensification of lock-in):
narrative_ontology:measurement(nvidia_ex_t0, nvidia_cuda_ecosystem_lockin, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(nvidia_ex_t5, nvidia_cuda_ecosystem_lockin, base_extractiveness, 5, 0.50).
narrative_ontology:measurement(nvidia_ex_t10, nvidia_cuda_ecosystem_lockin, base_extractiveness, 10, 0.65).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: CUDA functions as a de facto global infrastructure for
% a specific type of computation (parallel processing for AI).
narrative_ontology:coordination_type(nvidia_cuda_ecosystem_lockin, global_infrastructure).

% --- Network Decomposition (Constraint Families) ---
% The colloquial label "Nvidia's AI dominance" is decomposed into two
% distinct constraints per the ε-invariance principle. This story models the
% high-extraction lock-in. The other models the underlying performance.

% DUAL FORMULATION NOTE:
% This constraint is one of 2 stories decomposed from "Nvidia's AI Dominance".
% Decomposed because ε differs across observables (ε-invariance principle).
% Related stories:
%   - nvidia_gpu_performance (ε≈0.20, Rope/Mountain)
%   - nvidia_cuda_ecosystem_lockin (ε=0.65, Tangled Rope)

narrative_ontology:affects_constraint(nvidia_gpu_performance, nvidia_cuda_ecosystem_lockin).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The automatic derivation
% based on the declared beneficiary/victim groups and their respective exit
% options (trapped, arbitrage, constrained) accurately models the structural
% relationships and power dynamics of the ecosystem.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */