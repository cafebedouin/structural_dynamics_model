% ============================================================================
% CONSTRAINT STORY: necessary_day_job
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_necessary_day_job, []).

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
 *   constraint_id: necessary_day_job
 *   human_readable: The Necessary Day Job for Creatives
 *   domain: economic/social
 *
 * SUMMARY:
 *   The 'necessary day job' is the economic constraint where creative
 *   vocation is decoupled from subsistence labor. Many artists, writers,
 *   musicians, and other creatives must work in unrelated fields to support
 *   themselves, sacrificing time and energy that could be devoted to their
 *   creative pursuits. This arrangement benefits employers by providing a
 *   readily available workforce and consumers who enjoy creative works at
 *   prices lower than a full-time creative economy might allow. However, it
 *   extracts from individual creatives by limiting their potential output and
 *   overall well-being.
 *
 * KEY AGENTS:
 *   - Individual Creatives: Primary target (powerless/trapped) - Bears the cost of reduced creative output and potential burnout.
 *   - Employers: Primary beneficiary (institutional/arbitrage) - Benefits from readily available workforce.
 *   - Consumers of Creative Work: Secondary beneficiary (moderate/mobile) - Benefits from potentially lower prices and a wider variety of creative works.
 *   - Creatives with Side Hustles: Hybrid target/beneficiary (moderate/constrained) - Navigate the constraint through side hustles.
 *   - Analytical Observer: Analyzes long-term trends (analytical/analytical) - Examines the system from a broad perspective.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(necessary_day_job, 0.55).
domain_priors:suppression_score(necessary_day_job, 0.7).
domain_priors:theater_ratio(necessary_day_job, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(necessary_day_job, extractiveness, 0.55).
narrative_ontology:constraint_metric(necessary_day_job, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(necessary_day_job, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(necessary_day_job, tangled_rope).
narrative_ontology:human_readable(necessary_day_job, "The Necessary Day Job for Creatives").
narrative_ontology:topic_domain(necessary_day_job, "economic/social").

domain_priors:requires_active_enforcement(necessary_day_job).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(necessary_day_job, employers).
narrative_ontology:constraint_beneficiary(necessary_day_job, consumers_of_creative_work).
narrative_ontology:constraint_victim(necessary_day_job, individual_creatives).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For the individual creative, the necessity of a day job can feel like a snare, trapping them in unfulfilling labor that siphons time and energy away from their true vocation. Limited exit options due to financial needs.
constraint_indexing:constraint_classification(necessary_day_job, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% Employers who benefit from readily available labor see the arrangement as a coordination mechanism allowing them to fill positions and maintain productivity. They can arbitrage labor markets to minimize costs.
constraint_indexing:constraint_classification(necessary_day_job, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% From a broader perspective, the necessary day job functions as a tangled rope. It provides a baseline of economic stability, enabling some creative work to exist at all, but it also extracts potential creative output by diverting talent and time into other areas.
constraint_indexing:constraint_classification(necessary_day_job, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

% Many creatives supplement their income through freelance work or side hustles. This allows for some degree of creative fulfillment and financial independence, but they are still constrained by the demands of their day jobs. This creates a tangled rope situation where they benefit from both employment and creative pursuits, but are also extracted from by both.
constraint_indexing:constraint_classification(necessary_day_job, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(necessary_day_job_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(necessary_day_job, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(necessary_day_job, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(necessary_day_job, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(necessary_day_job_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate. A significant portion of potential creative output is extracted as creatives spend time and energy on non-creative labor. Suppression (0.70): High. The need for basic income heavily suppresses the ability of many creatives to pursue their vocations full-time. Theater Ratio (0.30): Low. There is not much 'theater' associated with this constraint. It is a fairly direct economic pressure.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap arises from the differing structural positions of the agents involved. Creatives experience the constraint as a snare, limiting their freedom and potential. Employers see it as a source of readily available labor, enabling economic activity. The analytical observer sees the complex interplay of forces, recognizing both the benefits and drawbacks of this arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality logic reflects the structural relationships between agents. Individual creatives, with limited exit options, bear the brunt of the extraction. Employers, with the ability to arbitrage labor markets, benefit from the arrangement. The analytical observer assesses the system as a whole, recognizing the tangled web of benefits and drawbacks.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled rope classification prevents mislabeling coordination as pure extraction by acknowledging the genuine economic benefits provided by day jobs, while simultaneously acknowledging the extraction of creative potential.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    automation_creative_labor,
    'To what extent will automation affect both ''day jobs'' and creative labor, potentially decoupling subsistence from vocation?',
    'Longitudinal study of automation''s impact on different job sectors, including creative industries.',
    'If automation displaces day jobs faster than creative labor, this constraint may weaken or transform. If automation eliminates creative labor faster, it may intensify the snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(automation_creative_labor, empirical, 'The impact of automation on both day jobs and creative labor.').

omega_variable(
    valuation_of_creative_work,
    'What are the long-term trends in the economic and social valuation of creative work, including art, music, writing, and design?',
    'Analysis of market trends, funding models, and cultural attitudes towards creative work.',
    'If the valuation of creative work increases, more creatives may be able to support themselves without day jobs. If the valuation decreases, the snare may intensify.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(valuation_of_creative_work, empirical, 'The economic and social valuation of creative work.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(necessary_day_job, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nece_tr_t0, necessary_day_job, theater_ratio, 0, 0.2).
narrative_ontology:measurement(nece_tr_t5, necessary_day_job, theater_ratio, 5, 0.25).
narrative_ontology:measurement(nece_tr_t10, necessary_day_job, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(nece_be_t0, necessary_day_job, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(nece_be_t5, necessary_day_job, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(nece_be_t10, necessary_day_job, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(necessary_day_job, resource_allocation).
narrative_ontology:affects_constraint(necessary_day_job, precarious_labor).
narrative_ontology:affects_constraint(necessary_day_job, intellectual_property_rights).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
