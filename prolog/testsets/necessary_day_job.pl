% ============================================================================
% CONSTRAINT STORY: necessary_day_job
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-22
% ============================================================================

:- module(constraint_necessary_day_job, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: necessary_day_job
 * human_readable: The Necessary Day Job for Creatives
 * domain: economic/social
 * * SUMMARY:
 * The "necessary day job" is the economic constraint where creative vocation is
 * decoupled from subsistence labor. Writers, artists, and scholars must perform
 * unrelated, often precarious or underpaid work (e.g., adjunct teaching, service jobs)
 * to fund their creative passions, effectively subsidizing the cultural sphere
 * with their time and non-creative labor.
 * * KEY AGENTS:
 * - The Artist/Adjunct: Subject (Powerless)
 * - The Employer/University: Beneficiary (Institutional)
 * - The Systems Auditor: Observer (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Base extractiveness (0.65): High. Represents the extraction of time, energy,
% and economic security from the creative worker. The value of their labor in
% the "day job" is captured by the employer, while the risk and cost of
% creative production remain with the individual.
domain_priors:base_extractiveness(necessary_day_job, 0.65).

% Suppression score (0.70): High. Economic alternatives (living solely off one's art,
% patronage, stable academic positions like tenure) are rare and have been
% systematically suppressed by market dynamics and institutional policies.
domain_priors:suppression_score(necessary_day_job, 0.70).

% Theater ratio (0.10): Low. While institutions may pay lip service to valuing
% creativity, the primary function of the day job is brutally economic, not
% performative.
domain_priors:theater_ratio(necessary_day_job, 0.10).

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(necessary_day_job, extractiveness, 0.65).
narrative_ontology:constraint_metric(necessary_day_job, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(necessary_day_job, theater_ratio, 0.10).

% Constraint self-claim: The system presents this as a constructed necessity,
% an unfortunate but unavoidable feature of a market economy.
narrative_ontology:constraint_claim(necessary_day_job, tangled_rope).
narrative_ontology:human_readable(necessary_day_job, "The Necessary Day Job for Creatives").

% Binary flags & Structural properties for Tangled Rope
domain_priors:requires_active_enforcement(necessary_day_job).
narrative_ontology:constraint_beneficiary(necessary_day_job, employers_and_institutions).
narrative_ontology:constraint_victim(necessary_day_job, creative_workers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE ARTIST / ADJUNCT (SNARE)
% For the individual creative, the day job is a trap. It consumes the finite
% resources (time, energy) needed for their real work, while offering just
% enough subsistence to perpetuate the cycle.
% χ = 0.65 * π(powerless:1.5) * σ(national:1.0) = 0.975
constraint_indexing:constraint_classification(necessary_day_job, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE EMPLOYER / UNIVERSITY (ROPE)
% From the institutional perspective, a large pool of self-subsidizing talent
% is a pure coordination benefit. It allows for flexible staffing, lower labor
% costs, and access to skilled individuals without long-term commitment.
% χ = 0.65 * π(institutional:-0.2) * σ(national:1.0) = -0.13
constraint_indexing:constraint_classification(necessary_day_job, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analyst sees both sides. The system provides a coordination function
% (staffing institutions, producing cultural goods) but does so via asymmetric
% extraction that is actively enforced by economic precarity. It is a functional
% system built on an exploitative foundation.
% χ = 0.65 * π(analytical:1.15) * σ(global:1.2) = 0.897
constraint_indexing:constraint_classification(necessary_day_job, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(necessary_day_job_tests).

test(perspectival_gap_artist_vs_institution, [nondet]) :-
    constraint_indexing:constraint_classification(necessary_day_job, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(necessary_day_job, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless == snare,
    TypeInstitutional == rope,
    TypePowerless \= TypeInstitutional.

test(analytical_observer_sees_tangled_rope, [nondet]) :-
    constraint_indexing:constraint_classification(necessary_day_job, TypeAnalytical, context(agent_power(analytical), _, _, _)),
    TypeAnalytical == tangled_rope.

test(threshold_validation_high_extraction) :-
    narrative_ontology:constraint_metric(necessary_day_job, extractiveness, E),
    E >= 0.46.

:- end_tests(necessary_day_job_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The scores reflect the modern "gig economy" reality for many creatives. The
 * base extractiveness (0.65) and suppression (0.70) are high because alternatives
 * like patronage or stable tenure have been eroded, forcing creatives into
 * precarious labor markets.
 *
 * The Perspectival Gap is stark:
 * - The powerless artist experiences this as a 'Snare', a time-theft machine that
 *   feels inescapable.
 * - The institutional beneficiary sees it as a 'Rope', an efficient mechanism to
 *   source flexible, low-cost, highly-skilled labor.
 * - The analytical observer, seeing both the coordination function and the
 *   coercive extraction, must classify it as a 'Tangled Rope'.
 *
 * * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY] The 'Tangled Rope' classification is critical here. A
 * simpler analysis might label the system a pure 'Snare'. However, that would
 * miss the fact that the system *does* successfully coordinate labor to produce
 * cultural and educational outputs. The Tangled Rope classification correctly
 * identifies that a genuine coordination function exists but is coupled with,
 * and dependent upon, high asymmetric extraction. This prevents mischaracterizing
 * a functional-but-exploitative system as pure, non-functional predation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% The core uncertainty is whether the struggle of a day job is a net positive
% or negative for the quality of creative output.
omega_variable(
    omega_necessary_day_job,
    'Is the "day job" a necessary crucible that forges better art by grounding it in reality, or is it a filter that primarily suppresses potential masterpieces from ever being created?',
    'Comparative analysis of creative output from systems with high vs. low day-job dependency (e.g., state-funded artist programs vs. market-driven systems).',
    'If crucible, the constraint has a hidden positive externality. If filter, its societal cost is far higher than the direct extraction from individuals.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(necessary_day_job, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% The data models the intensification of this constraint over the last few
% decades, reflecting the decline of tenure and the rise of the gig economy.
% Extraction has increased as stable alternatives have been suppressed.

% Theater ratio over time:
narrative_ontology:measurement(ndj_tr_t0, necessary_day_job, theater_ratio, 0, 0.05).
narrative_ontology:measurement(ndj_tr_t5, necessary_day_job, theater_ratio, 5, 0.08).
narrative_ontology:measurement(ndj_tr_t10, necessary_day_job, theater_ratio, 10, 0.10).

% Extraction over time:
narrative_ontology:measurement(ndj_ex_t0, necessary_day_job, base_extractiveness, 0, 0.60).
narrative_ontology:measurement(ndj_ex_t5, necessary_day_job, base_extractiveness, 5, 0.62).
narrative_ontology:measurement(ndj_ex_t10, necessary_day_job, base_extractiveness, 10, 0.65).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The constraint functions as a mechanism for allocating a resource (skilled
% labor) under conditions of scarcity (funding for the arts/humanities).
narrative_ontology:coordination_type(necessary_day_job, resource_allocation).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */