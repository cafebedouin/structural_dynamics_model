% ============================================================================
% CONSTRAINT STORY: dead_sea_effect
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dead_sea_effect, []).

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
 *   constraint_id: dead_sea_effect
 *   human_readable: The Dead Sea Effect (Talent Evaporation)
 *   domain: social/economic
 *
 * SUMMARY:
 *   The Dead Sea Effect occurs in organizations when highly talented and
 *   mobile individuals "evaporate" (leave) because they have the best exit
 *   options, while less talented or mobile individuals stay behind. This
 *   leads to a decline in overall organizational performance and creates a
 *   negative feedback loop that accelerates talent loss. The beneficiaries
 *   are competing organizations that attract this talent. The main victim is
 *   the organization itself, followed by the low-mobility employees that are
 *   trapped.
 *
 * KEY AGENTS:
 *   - Departing Talent: Mobile employee who leaves the organization (moderate/mobile)
 *   - Remaining Organization: The organization experiencing talent loss (institutional/constrained)
 *   - Low-Mobility Employee: Employee unable to easily leave the organization (powerless/trapped)
 *   - Competing Organization: Organization that attracts talent from the affected one (institutional/arbitrage)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dead_sea_effect, 0.55).
domain_priors:suppression_score(dead_sea_effect, 0.4).
domain_priors:theater_ratio(dead_sea_effect, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dead_sea_effect, extractiveness, 0.55).
narrative_ontology:constraint_metric(dead_sea_effect, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(dead_sea_effect, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dead_sea_effect, tangled_rope).
narrative_ontology:human_readable(dead_sea_effect, "The Dead Sea Effect (Talent Evaporation)").
narrative_ontology:topic_domain(dead_sea_effect, "social/economic").

domain_priors:requires_active_enforcement(dead_sea_effect).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dead_sea_effect, remaining_organization).
narrative_ontology:constraint_beneficiary(dead_sea_effect, competing_organizations).
narrative_ontology:constraint_victim(dead_sea_effect, departing_talent).
narrative_ontology:constraint_victim(dead_sea_effect, organization_productivity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Low-mobility employees experience a Snare because their limited exit options force them to remain in a deteriorating work environment. They are essentially trapped, bearing the full cost of the organization's decline.
constraint_indexing:constraint_classification(dead_sea_effect, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% Competing organizations that attract talent experience the effect as a Rope, benefiting from the influx of skilled workers and improved competitiveness. They actively arbitrage talent from other organizations.
constraint_indexing:constraint_classification(dead_sea_effect, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% High-talent employees with mobility experience a Tangled Rope. They benefit from their ability to exit a declining organization, but they also bear costs associated with job searching, relocation, and potential career disruption. There is both extraction (cost of change) and coordination (better opportunity).
constraint_indexing:constraint_classification(dead_sea_effect, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% An analytical observer examining the situation from a generational and global perspective sees a Tangled Rope, as the phenomenon represents a mixed bag of coordination and extraction. Talent is allocated across the market (coordination) but individual organizations face costs (extraction).
constraint_indexing:constraint_classification(dead_sea_effect, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dead_sea_effect_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(dead_sea_effect, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dead_sea_effect, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(dead_sea_effect, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dead_sea_effect_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.55) because the effect significantly degrades the affected organization over time. Suppression is moderate (0.40) because while highly talented employees generally have better exit options, various factors (e.g., non-compete agreements, personal circumstances) can limit their mobility. The theater ratio is low (0.20) because the effect is more about real talent loss than performative metrics.
 *
 * PERSPECTIVAL GAP:
 *   High-talent employees with mobility see a Tangled Rope (better opportunity elsewhere, but costs to leaving). Competing firms and the organization itself both gain and lose during the process (depending on whether they're the origin or destination of talent). The low mobility employee sees a snare, as they're essentially stuck within an increasingly unproductive organization.
 *
 * DIRECTIONALITY LOGIC:
 *   The departing talent bears the cost of job change but also secures better opportunities (mobile). The organization benefits initially from any work contributed by the employee prior to departure, then bears the cost of their loss (constrained). The trapped employee sees only the costs of a declining organization. The competing organization receives a disproportionate share of the benefits (arbitrage).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mobility_threshold,
    'What level of mobility is necessary to escape the ''Dead Sea'' and experience the upside of talent allocation?',
    'Correlation of job mobility with career satisfaction and salary growth.',
    'High threshold: More employees experience the effect as a Snare. Low threshold: More employees experience the effect as a Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mobility_threshold, empirical, 'Mobility threshold for escaping the Dead Sea').

omega_variable(
    organization_adaptation,
    'How effectively can organizations adapt to the loss of talent and prevent a downward spiral?',
    'Case studies of organizations that have successfully navigated the Dead Sea Effect.',
    'High adaptation: the constraint may resolve into a transient scaffold. Low adaptation: the extraction persists or worsens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(organization_adaptation, empirical, 'Organizational adaptation to talent loss.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dead_sea_effect, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dead_tr_t0, dead_sea_effect, theater_ratio, 0, 0.1).
narrative_ontology:measurement(dead_tr_t5, dead_sea_effect, theater_ratio, 5, 0.15).
narrative_ontology:measurement(dead_tr_t10, dead_sea_effect, theater_ratio, 10, 0.2).

% Extraction over time
narrative_ontology:measurement(dead_be_t0, dead_sea_effect, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(dead_be_t5, dead_sea_effect, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(dead_be_t10, dead_sea_effect, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dead_sea_effect, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
