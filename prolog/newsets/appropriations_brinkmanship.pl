% ============================================================================
% CONSTRAINT STORY: appropriations_brinkmanship
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_appropriations_brinkmanship, []).

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
 *   constraint_id: appropriations_brinkmanship
 *   human_readable: Government Shutdown Threat via Appropriations Process
 *   domain: political
 *
 * SUMMARY:
 *   This constraint models the use of the US government's appropriations
 *   process as a political weapon. By threatening to block the passage of
 *   essential funding bills before the fiscal deadline, a minority faction
 *   can trigger a government shutdown. The credible threat of inflicting
 *   widespread economic and social costs (furloughed workers, suspended
 *   services) creates leverage to extract policy concessions entirely
 *   unrelated to the budget itself. The constraint's structure relies on the
 *   Antideficiency Act, which legally mandates a shutdown in the absence of
 *   appropriations.
 *
 * KEY AGENTS:
 *   - Brinkmanship Faction: Primary beneficiary (organized/mobile) - Gains disproportionate political leverage.
 *   - Federal Employees & General Public: Primary victims (powerless/trapped) - Bear the direct costs of shutdowns through lost pay and services.
 *   - Legislative Majority / Executive Branch: Institutional victims (institutional/constrained) - Held responsible for governance and forced to negotiate under duress.
 *   - The Congressional Process: Institutional actor (institutional/arbitrage) - The set of rules and norms that enable the brinkmanship, which has become a degraded, performative ritual.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(appropriations_brinkmanship, 0.65).
domain_priors:suppression_score(appropriations_brinkmanship, 0.75).
domain_priors:theater_ratio(appropriations_brinkmanship, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(appropriations_brinkmanship, extractiveness, 0.65).
narrative_ontology:constraint_metric(appropriations_brinkmanship, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(appropriations_brinkmanship, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(appropriations_brinkmanship, tangled_rope).
narrative_ontology:human_readable(appropriations_brinkmanship, "Government Shutdown Threat via Appropriations Process").
narrative_ontology:topic_domain(appropriations_brinkmanship, "political").

domain_priors:requires_active_enforcement(appropriations_brinkmanship).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(appropriations_brinkmanship, brinkmanship_faction).
narrative_ontology:constraint_victim(appropriations_brinkmanship, federal_employees).
narrative_ontology:constraint_victim(appropriations_brinkmanship, general_public).
narrative_ontology:constraint_victim(appropriations_brinkmanship, government_agencies).
narrative_ontology:constraint_victim(appropriations_brinkmanship, legislative_majority).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FEDERAL EMPLOYEE (SNARE) — Furloughed or forced to work without pay, their livelihood is held hostage by a political process they cannot influence or exit. The constraint is pure, coercive extraction. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.92.
constraint_indexing:constraint_classification(appropriations_brinkmanship, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: BRINKMANSHIP FACTION (ROPE) — Views the shutdown threat as a legitimate, powerful tool to coordinate its members and force policy concessions that would be impossible through normal legislative means. For them, it is a feature, not a bug. d≈0.15, f(d)≈-0.01, σ=1.0 → χ≈-0.01. Negative extraction signifies a net subsidy of political power.
constraint_indexing:constraint_classification(appropriations_brinkmanship, rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: LEGISLATIVE MAJORITY (TANGLED ROPE) — Responsible for governance, they must engage with the legitimate coordination task of funding the government, but are simultaneously victims of an extractive process that holds their agenda hostage. d≈0.60, f(d)≈0.88, σ=1.0 → χ≈0.57.
constraint_indexing:constraint_classification(appropriations_brinkmanship, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: THE CONGRESSIONAL PROCESS (PITON) — The annual budget process has degraded from a functional resource allocation mechanism into a performative, ritualized crisis. The high theater of last-minute deals and public posturing masks the atrophied core function of good-faith negotiation. theater_ratio=0.75 satisfies the piton gate (≥0.70).
constraint_indexing:constraint_classification(appropriations_brinkmanship, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (TANGLED ROPE) — Sees the full structure: a necessary coordination function (funding the state) has been weaponized via a procedural choke point (the appropriations deadline) to enable asymmetric extraction of policy concessions. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.90.
constraint_indexing:constraint_classification(appropriations_brinkmanship, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(appropriations_brinkmanship_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(appropriations_brinkmanship, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(appropriations_brinkmanship, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(appropriations_brinkmanship, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(appropriations_brinkmanship, TR),
    TR >= 0.70.

:- end_tests(appropriations_brinkmanship_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65): High. The value extracted is not monetary but political: the ability to force major policy changes that lack majority support. Suppression (0.75): High. The all-or-nothing, time-sensitive nature of appropriations, backed by the Antideficiency Act, creates a powerful coercive environment with few alternatives. Theater Ratio (0.75): High. The process has evolved into a highly public, performative ritual of last-minute negotiations and manufactured crises, where the public debate about 'fiscal responsibility' often masks a raw power play.
 *
 * PERSPECTIVAL GAP:
 *   The gap is extreme. For the brinkmanship faction, the mechanism is a Rope—a tool for achieving legitimate policy ends. For a furloughed federal worker, it is a Snare—a coercive trap that threatens their livelihood. For the governing majority, it is a Tangled Rope—a necessary coordination task (funding the government) hijacked for extraction. For the process itself, viewed over decades, it is a Piton—a functional process that has degraded into a high-theater, low-function ritual.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiaries are the organized political faction who can deploy this tactic at will (mobile exit) to gain leverage, resulting in negative effective extraction (a power subsidy). The victims are the federal employees and public who are trapped within the system and bear the full costs, resulting in maximum effective extraction. The institutional actors responsible for governance are constrained and experience a mix of coordination and extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids mandatrophy by demonstrating how a single institutional process can be correctly classified in multiple ways depending on the observer's index. Labeling the system simply as 'broken' (a common critique) is analytically insufficient. The system is not equally broken for everyone; for the brinkmanship faction, it works exceptionally well. The DR classification reveals that it functions as a Rope for the powerful, a Snare for the powerless, and a Tangled Rope for the analytical observer, capturing the asymmetric distribution of costs and benefits.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bug_or_feature,
    'Is appropriations brinkmanship an unforeseen bug from legislative design, or an intended feature of a system with separation of powers?',
    'Constitutional and historical analysis of the framers'' intent regarding the Antideficiency Act and the power of the purse.',
    'If a bug, reforms like automatic continuing resolutions are corrective. If a feature, the constraint is a Mountain of constitutional design, and reform is nearly impossible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bug_or_feature, conceptual, 'Whether brinkmanship is a bug or intended feature of constitutional design.').

omega_variable(
    economic_vs_political_cost,
    'What is the quantifiable economic cost of a shutdown versus the perceived political cost to a faction of conceding on its policy demands?',
    'Economic modeling of shutdown impacts (CBO reports) correlated with polling data and voting records for the brinkmanship faction''s districts.',
    'If economic cost >> political cost, the tactic is highly irrational from a systemic view. If political cost >> economic cost (for the faction), the tactic is rational and will persist.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(economic_vs_political_cost, empirical, 'The calculus of economic damage versus political gain for the instigating faction.').

omega_variable(
    alternative_mechanism_viability,
    'Could an alternative mechanism, such as an automatic continuing resolution that triggers in the absence of a budget, be implemented to neutralize the threat?',
    'Analysis of political will, procedural hurdles to reform, and game-theoretic modeling of how such a change would alter legislative incentives.',
    'If viable, the constraint could be resolved into a pure Rope. If not politically viable, the Snare/Tangled Rope structure is deeply entrenched.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_mechanism_viability, preference, 'Political and procedural viability of implementing an automatic CR to prevent shutdowns.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(appropriations_brinkmanship, 1980, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(appr_tr_t1980, appropriations_brinkmanship, theater_ratio, 1980, 0.3).
narrative_ontology:measurement(appr_tr_t2000, appropriations_brinkmanship, theater_ratio, 2000, 0.55).
narrative_ontology:measurement(appr_tr_t2025, appropriations_brinkmanship, theater_ratio, 2025, 0.75).

% Extraction over time
narrative_ontology:measurement(appr_be_t1980, appropriations_brinkmanship, base_extractiveness, 1980, 0.2).
narrative_ontology:measurement(appr_be_t2000, appropriations_brinkmanship, base_extractiveness, 2000, 0.45).
narrative_ontology:measurement(appr_be_t2025, appropriations_brinkmanship, base_extractiveness, 2025, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(appropriations_brinkmanship, resource_allocation).
narrative_ontology:affects_constraint(appropriations_brinkmanship, debt_ceiling_brinkmanship).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
