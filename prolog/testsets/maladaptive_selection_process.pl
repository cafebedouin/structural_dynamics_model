% ============================================================================
% CONSTRAINT STORY: maladaptive_selection_process
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_maladaptive_selection_process, []).

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
 *   constraint_id: maladaptive_selection_process
 *   human_readable: The Evolutionary Dead-End: Maladaptive Selection Processes
 *   domain: organizational/technological
 *
 * SUMMARY:
 *   Maladaptive selection processes occur when the criteria used to evaluate
 *   organizational or technological fitness become decoupled from actual
 *   long-term survival or functional utility. A selection apparatus (metrics,
 *   KPIs, funding mechanisms, rankings) claims to be a Rope—a coordination
 *   mechanism solving the problem of comparing incomparable entities—but the
 *   metrics measure the wrong thing, creating systematic pressure toward
 *   local optimization at the expense of global function. Organizations
 *   optimizing for the measured fitness function discover they are evolving
 *   away from true fitness. The constraint exhibits all six DR types. The
 *   selection apparatus operators experience it as legitimate coordination
 *   (Rope). Trapped organizations experience it as pure extraction (Snare).
 *   Middle managers experience it as mixed coordination and extraction
 *   (Tangled Rope). Reform coalitions see it as a temporary problem with a
 *   sunset as better metrics emerge (Scaffold). The metrics themselves
 *   persist through inertia despite acknowledged malfunction (Piton). The
 *   analytical observer risks naturalizing it as inherent to any selection
 *   process (Mountain). The theater_ratio has risen from 0.42 to 0.68 over
 *   twenty years as the gap between measured and actual fitness widened,
 *   forcing organizations to devote increasing effort to performing well on
 *   the wrong metrics rather than addressing real functional problems.
 *
 * KEY AGENTS:
 *   - Selection Apparatus Operators: Primary beneficiary (institutional/arbitrage) — control metrics, resource distribution, institutional legitimacy; can modify criteria with minimal personal cost
 *   - System Long-term Viability: Primary victim (powerless/trapped) — abstract collective good; cannot exit or organize; bears full cost of maladaptation
 *   - Trapped Organizations: Secondary victim (powerless/trapped) — selected by maladaptive criteria; cannot exit without competitive disadvantage; cannot change metrics without appearing to reject their own success
 *   - Aware Middle Managers: Mixed victim/beneficiary (moderate/constrained) — benefit from gaming the metrics (promotion, bonuses); constrained by reputational risk of challenging them; bear long-term organizational risk
 *   - Reform Coalition: Organized agents (organized/mobile) — advocates for alternative metrics; have exit options; building alternative selection pathways with generational sunset logic
 *   - Legacy Measurement System: Institutional actor (institutional/arbitrage) — persists through inertia; acknowledged to measure the wrong thing; high theater ratio as primary function atrophies
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(maladaptive_selection_process, 0.58).
domain_priors:suppression_score(maladaptive_selection_process, 0.62).
domain_priors:theater_ratio(maladaptive_selection_process, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(maladaptive_selection_process, extractiveness, 0.58).
narrative_ontology:constraint_metric(maladaptive_selection_process, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(maladaptive_selection_process, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(maladaptive_selection_process, tangled_rope).
narrative_ontology:human_readable(maladaptive_selection_process, "The Evolutionary Dead-End: Maladaptive Selection Processes").
narrative_ontology:topic_domain(maladaptive_selection_process, "organizational/technological").

domain_priors:requires_active_enforcement(maladaptive_selection_process).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(maladaptive_selection_process, selection_apparatus_operators).
narrative_ontology:constraint_beneficiary(maladaptive_selection_process, short_term_optimizers).
narrative_ontology:constraint_victim(maladaptive_selection_process, system_long_term_viability).
narrative_ontology:constraint_victim(maladaptive_selection_process, functional_organizations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAPPED ORGANIZATION (SNARE) — Organizations selected by maladaptive criteria have no exit. They cannot change selection metrics without immediate competitive disadvantage. They cannot opt out of the selection process. They cannot advocate for better criteria without appearing to reject their own 'success.' Trapped in a fitness landscape that measures the wrong thing. Maximum extraction burden.
constraint_indexing:constraint_classification(maladaptive_selection_process, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: AWARE MIDDLE MANAGER (TANGLED ROPE) — Sees the maladaptive criteria. Benefits from compliance (promotion, bonus, job security through conformity). Constrained by reputational risk of challenging the metrics. Some agency to work around the system, some benefit from gaming it, but also bears long-term organizational risk. Asymmetric extraction: complying gains individual reward while distributing collective cost.
constraint_indexing:constraint_classification(maladaptive_selection_process, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: SELECTION APPARATUS OPERATORS (ROPE) — Designed and enforce the selection criteria (metrics, KPIs, rankings, funding mechanisms). Experience the constraint as coordination: the metrics are their solution to comparing incomparable things. Net beneficiary through institutional power, career advancement, control over resource distribution. High exit optionality — can modify criteria with minimal personal cost.
constraint_indexing:constraint_classification(maladaptive_selection_process, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REFORM COALITION (SCAFFOLD) — Advocates (academics, consultants, NGOs) promoting alternative metrics and selection criteria. See the maladaptive process as a temporary institutional failure with a sunset: better measurement tools, stakeholder governance, and long-term value accounting are building alternative selection pathways. Organized agents with exit options (can switch advocacy focus, move to other sectors). Low extractive burden because agency and sunset are real.
constraint_indexing:constraint_classification(maladaptive_selection_process, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY MEASUREMENT SYSTEM (PITON) — The metrics themselves (shareholder value, test scores, publication counts, quarterly earnings) are sustained by institutional inertia despite widespread acknowledgment that they measure the wrong thing. The theater_ratio is high because much organizational activity is devoted to performing well on the metrics rather than addressing actual fitness. The primary function (identifying genuinely adaptive organizations) has atrophied, but the measurement apparatus persists because alternatives have not fully displaced it.
constraint_indexing:constraint_classification(maladaptive_selection_process, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, selection pressure always creates a fitness function, and any fitness function can become maladapted relative to true environmental demands. This perspective risks naturalizing what is actually a contingent institutional choice as an inevitable constraint of selection itself. The engine will flag this as a false summit — what appears to be inherent to selection is actually the particular metrics chosen by humans.
constraint_indexing:constraint_classification(maladaptive_selection_process, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(maladaptive_selection_process_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(maladaptive_selection_process, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(maladaptive_selection_process, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(maladaptive_selection_process, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(maladaptive_selection_process, TR),
    TR >= 0.70.

:- end_tests(maladaptive_selection_process_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The maladaptive selection process extracts from trapped organizations in the form of forced optimization toward wrong objectives, while operators and short-term beneficiaries gain from controlled resource distribution and institutional power. The extraction is not as severe as a pure Snare (0.66+) because some organizations can partially game the metrics and some can exit through strategic repositioning. The growth from 0.35 to 0.58 reflects accumulating rent-seeking: as organizations become sophisticated at gaming the metrics, operators must continuously redefine them, layering new performance requirements on top of old ones. Suppression (0.62): Moderate-high. Significant barriers to challenging the metrics include: institutional legitimacy of the measurement apparatus, career risk of dissent, difficulty of coordinating reform across fragmented stakeholders, and path dependence (existing systems are deeply integrated). But suppression is not total — some organizations do exit, some reform coalitions have successfully changed metrics in specific sectors (ESG reporting, patient outcomes in healthcare, teaching load in universities). Theater ratio (0.68): High and rising. Measurement gaming is pervasive: organizations devote substantial effort to optimizing for metrics rather than addressing actual functional problems. The gap between measured and actual fitness has widened over the interval, forcing more theatrical activity to maintain apparent fitness while real function decays.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap in this constraint is between those who control the selection apparatus (who see legitimate coordination) and those selected by it (who see extraction). The operators experience a genuine coordination problem: they need some way to compare organizations with different structures, goals, and timescales. Their solution (metrics) is a real coordination mechanism. But the trapped organizations experience the selected metrics as extraction because the metrics were designed by operators with different incentives, tested on past environments, and are now misaligned with actual survival requirements. The gap is not between subjective perceptions but between control positions. Operators have the power to change the metrics; trapped organizations do not. The Scaffold perspective (reform coalition) bridges this gap by identifying real structural changes (better measurement tools, stakeholder governance, long-term accounting) that could align metrics with actual fitness without requiring trapped organizations to simply accept maladaptation.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are determined by each agent's structural relationship to the maladaptive selection process. Operators who designed and enforce the metrics experience low directionality (d ≈ 0.15) — they are beneficiaries with arbitrage options (can change the metrics). Trapped organizations experience high directionality (d ≈ 0.90) — they are victims with no exit. Aware middle managers experience medium directionality (d ≈ 0.55) — they benefit from gaming but are constrained by the system and bear long-term risk. Reform coalitions experience lower directionality (d ≈ 0.40) — they have agency and exit options despite being critical of the system. The legacy measurement system has institutional directionality (d ≈ 0.20) — it is a beneficiary of its own persistence through inertia. The analytical observer's naturalization (d ≈ 0.72) attempts to treat selection pressure itself as inevitable, but this is perspectival: selection pressure is real, but the choice of which metric to optimize for is contingent.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This constraint resolves the mandatrophy by showing that the claim 'selection processes are coordination mechanisms' (Rope) and the claim 'maladaptive metrics extract from trapped organizations' (Snare) are both structurally correct but from different observer positions. The selection apparatus IS a coordination mechanism—it solves a real problem of comparing incomparable entities. But the particular metrics chosen by operators ARE extractive toward trapped organizations—they transfer control over fitness definition to operators while imposing the costs of maladaptation on trapped agents. The mandatrophy is resolved by recognizing that Tangled Rope classification is structurally accurate: the constraint has a genuine coordination function (helping operators compare organizations) AND asymmetric extraction (transferring fitness-definition power to operators, imposing maladaptation costs on trapped agents). The operators experience it as Rope because they benefit from the coordination without bearing maladaptation costs. Trapped organizations experience it as Snare because they bear maladaptation costs without power over metric design. Both experiences are correct from their respective positions. The constraint genuinely combines coordination (operators' real problem) and extraction (trapped organizations' real burden) in the same mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    actual_vs_measured_fitness_divergence,
    'At what point does measured fitness diverge sufficiently from actual long-term survival fitness that the selection process becomes net-destructive?',
    'Historical analysis of organizations with high measured fitness but low long-term survival; comparison of metric-optimized vs metric-agnostic selection outcomes over 10+ year timescales',
    'If divergence is slow (< 0.05 per year): system appears to work for decades before collapse (Rope or Scaffold). If rapid (> 0.15 per year): system is visibly broken within 3-5 years (Snare). Timing determines whether stakeholders perceive extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(actual_vs_measured_fitness_divergence, empirical, 'Divergence threshold between measured and actual fitness').

omega_variable(
    metric_malleability_limit,
    'Can organizations reliably game the maladaptive metrics, and if so, how quickly do they discover and exploit the gaming loopholes?',
    'Speed of metric gaming discovery (weeks to years); stability of workarounds once discovered; rate of metric redefinition by operators in response to gaming',
    'If gaming is easy and rapid: the constraint becomes pure extraction theater (Snare, high theater_ratio). If gaming is hard or metric operators quickly adapt: the constraint is legitimate coordination with side effects (Rope or Tangled Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metric_malleability_limit, empirical, 'Whether and how quickly maladaptive metrics can be gamed').

omega_variable(
    stakeholder_coalition_formation_threshold,
    'What critical mass of awareness among trapped organizations triggers coordinated pressure to change the selection criteria?',
    'Analysis of organizational coalitions that have successfully pressured metric reform (ESG reporting, teaching load weighting in universities, patient outcomes vs test scores); identification of tipping points where individual exit attempts become collective action',
    'If threshold is low: scaffold perspective is correct (sunset is real, reform is imminent). If threshold is high or never reached: the constraint persists indefinitely (Piton or Snare). Coalition formation determines whether powerless agents can exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stakeholder_coalition_formation_threshold, empirical, 'Critical mass for stakeholder pressure to reform metrics').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(maladaptive_selection_process, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(maladapt_tr_t0, maladaptive_selection_process, theater_ratio, 0, 0.42).
narrative_ontology:measurement(maladapt_tr_t10, maladaptive_selection_process, theater_ratio, 10, 0.62).
narrative_ontology:measurement(maladapt_tr_t20, maladaptive_selection_process, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(maladapt_be_t0, maladaptive_selection_process, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(maladapt_be_t10, maladaptive_selection_process, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(maladapt_be_t20, maladaptive_selection_process, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(maladaptive_selection_process, resource_allocation).
narrative_ontology:affects_constraint(maladaptive_selection_process, goodhart_metric_substitution).
narrative_ontology:affects_constraint(maladaptive_selection_process, institutional_selection_bias).

% DUAL FORMULATION NOTE:
% Maladaptive selection processes are downstream of specific metric choices (shareholder primacy, test scores, publication counts) but represent a distinct structural constraint operating at the level of the selection apparatus itself. Each specific metric has its own extractiveness reflecting the degree of misalignment with actual fitness; this constraint models the meta-level process by which selection criteria become decoupled from function. The specific metrics are the observables; this constraint is their structural consequence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(maladaptive_selection_process, institutional, 0.18).
constraint_indexing:directionality_override(maladaptive_selection_process, moderate, 0.54).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
