% ============================================================================
% CONSTRAINT STORY: champions_bass_fishing_exclusion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_champions_bass_fishing_exclusion, []).

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
 *   constraint_id: champions_bass_fishing_exclusion
 *   human_readable: Champions Bass Fishing Tournament Exclusionary Practices
 *   domain: economic
 *
 * SUMMARY:
 *   The Champions Bass Fishing (CBF) tournament practices are exclusionary
 *   towards new, low-income, and minority anglers. High entry fees, limited
 *   qualifying spots, subjective judging, and reliance on established
 *   reputations create a significant barrier. While the tournaments provide
 *   coordination benefits for established anglers and organizers, they
 *   extract value from new entrants. This constraint creates a tangled rope
 *   effect: established anglers enjoy the exclusive environment and potential
 *   for high earnings, while new anglers struggle to break in.
 *
 * KEY AGENTS:
 *   - CBF Tournament Organizers: Primary beneficiary (institutional/arbitrage) — control the tournament, attract sponsors, and maintain exclusivity.
 *   - Established Anglers: Secondary beneficiary (moderate/constrained) — benefit from limited competition and maintain dominance.
 *   - New Anglers: Primary victim (powerless/trapped) — face high barriers to entry and limited opportunities to compete.
 *   - Low Income Anglers: Primary victim (powerless/trapped) - cannot afford entry fees or equipment requirements.
 *   - Minority Anglers: Primary victim (powerless/trapped) - face discrimination and may lack connections.
 *   - Angler Associations: Organized agents (organized/mobile) — can advocate for fairer practices but have limited influence.
 *   - Analytical Observer: (analytical/analytical) - observes and analyzes the entangled coordination and extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(champions_bass_fishing_exclusion, 0.6).
domain_priors:suppression_score(champions_bass_fishing_exclusion, 0.7).
domain_priors:theater_ratio(champions_bass_fishing_exclusion, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(champions_bass_fishing_exclusion, extractiveness, 0.6).
narrative_ontology:constraint_metric(champions_bass_fishing_exclusion, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(champions_bass_fishing_exclusion, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(champions_bass_fishing_exclusion, snare).
narrative_ontology:human_readable(champions_bass_fishing_exclusion, "Champions Bass Fishing Tournament Exclusionary Practices").
narrative_ontology:topic_domain(champions_bass_fishing_exclusion, "economic").

domain_priors:requires_active_enforcement(champions_bass_fishing_exclusion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(champions_bass_fishing_exclusion, cbf_tournament_organizers).
narrative_ontology:constraint_beneficiary(champions_bass_fishing_exclusion, established_anglers).
narrative_ontology:constraint_victim(champions_bass_fishing_exclusion, new_anglers).
narrative_ontology:constraint_victim(champions_bass_fishing_exclusion, low_income_anglers).
narrative_ontology:constraint_victim(champions_bass_fishing_exclusion, minority_anglers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% New anglers, lacking established reputations, sponsorships, and expensive equipment, find it difficult to enter and compete in CBF tournaments. High entry fees, limited qualifying spots, and subjective judging criteria create a significant barrier to entry.
constraint_indexing:constraint_classification(champions_bass_fishing_exclusion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% Established anglers benefit from the exclusionary practices as it limits competition and maintains their dominance in the tournaments. However, they are also constrained by the need to maintain their reputation and adhere to tournament rules.
constraint_indexing:constraint_classification(champions_bass_fishing_exclusion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% CBF tournament organizers benefit from the exclusionary practices as it allows them to control the tournament, attract high-paying sponsors, and maintain a perception of exclusivity. They can arbitrage different locations and angler bases to maximize profit.
constraint_indexing:constraint_classification(champions_bass_fishing_exclusion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% Angler Associations can organize and advocate for fairer tournament practices, but their influence on CBF is limited. They can choose to endorse or boycott tournaments, impacting participation and reputation.
constraint_indexing:constraint_classification(champions_bass_fishing_exclusion, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% An analytical observer sees the exclusionary practices as a tangled rope, where the benefits of maintaining exclusivity and attracting sponsors are intertwined with the suppression of new talent and the creation of barriers to entry.
constraint_indexing:constraint_classification(champions_bass_fishing_exclusion, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(champions_bass_fishing_exclusion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(champions_bass_fishing_exclusion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(champions_bass_fishing_exclusion, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(champions_bass_fishing_exclusion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(champions_bass_fishing_exclusion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.60): High. The exclusionary practices extract significant resources and opportunities from new anglers. Entry fees, equipment costs, and travel expenses create a substantial financial burden, while the limited number of qualifying spots restricts access.  Suppression (0.70): High. The high barriers to entry and subjective judging criteria suppress the potential of new anglers, limiting their ability to compete and advance. Theater ratio (0.30): Low. Tournaments are genuinely about fishing skill, but elements of branding and established relationships influence outcomes.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap arises from the differing experiences of beneficiaries and victims. Tournament organizers see a successful business model, established anglers enjoy the exclusive competition, and new anglers struggle to break in. The analytical observer sees the complex interplay of coordination and extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality values reflect the power and exit options of each agent. CBF tournament organizers have high power and arbitrage options, resulting in low directionality. New anglers have low power and are trapped, resulting in high directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The primary mandatrophy concern is whether this represents a genuine effort at defining a top-tier fishing competition or an extractive system designed to limit entry. The high extractiveness, suppression values, and differing perspectives from the victims confirm that this is a snare and not a legitimate high-skill competition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    subjective_judging_criteria,
    'To what extent are the judging criteria in CBF tournaments truly objective versus influenced by reputation and perceived skill of established anglers?',
    'Statistical analysis of judging scores correlated with angler reputation and demographic factors, blinded judging experiments.',
    'If criteria are primarily subjective, the exclusion mechanism is much stronger, and new anglers have little chance of success. If objective, the exclusionary effects are weaker.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subjective_judging_criteria, empirical, 'Objectivity of judging criteria in CBF tournaments.').

omega_variable(
    barrier_impact_alternative_circuits,
    'Does exclusion from CBF truly impede professional advancement for new anglers, or can they gain equivalent experience and exposure via other tournaments?',
    'Comparative analysis of career trajectories of anglers who primarily participate in CBF versus other circuits. Identification of crossover points and limitations.',
    'If CBF is a key bottleneck, the exclusionary practices significantly impact angler careers. If other circuits provide equivalent opportunities, the impact is less.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(barrier_impact_alternative_circuits, empirical, 'Alternative circuits provide professional experience for new anglers').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(champions_bass_fishing_exclusion, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cham_tr_t0, champions_bass_fishing_exclusion, theater_ratio, 0, 0.2).
narrative_ontology:measurement(cham_tr_t5, champions_bass_fishing_exclusion, theater_ratio, 5, 0.3).
narrative_ontology:measurement(cham_tr_t10, champions_bass_fishing_exclusion, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(cham_be_t0, champions_bass_fishing_exclusion, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(cham_be_t5, champions_bass_fishing_exclusion, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(cham_be_t10, champions_bass_fishing_exclusion, base_extractiveness, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(champions_bass_fishing_exclusion, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
