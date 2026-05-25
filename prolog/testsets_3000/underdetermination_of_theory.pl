% ============================================================================
% CONSTRAINT STORY: underdetermination_of_theory
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_underdetermination_of_theory, []).

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
 *   constraint_id: underdetermination_of_theory
 *   human_readable: Underdetermination of Theory by Evidence
 *   domain: epistemology/philosophy_of_science
 *
 * SUMMARY:
 *   Underdetermination of theory by evidence is a foundational
 *   epistemological claim: logically, any finite set of observations is
 *   consistent with infinitely many logically distinct theories. This creates
 *   a structural gap between what the evidence can support and which theory
 *   is selected. The constraint operates at two levels simultaneously — a
 *   logical level (Quine-Duhem thesis) and an institutional level (privileged
 *   theories suppressing alternatives through gatekeeping). The confusion
 *   between these levels is itself extractive: institutions cite logical
 *   underdetermination to justify their authority to select theories, while
 *   simultaneously denying that their selection is arbitrary. The constraint
 *   demonstrates how an epistemological truth (underdetermination exists) can
 *   be weaponized as an institutional tool (we get to decide which theory
 *   counts). The increasing theater ratio reflects the decoupling of
 *   epistemological doctrine from actual research practice — scientists write
 *   acknowledgments of underdetermination while acting as though their
 *   favored theory is uniquely correct. This performative gap widens as
 *   computational and open-access infrastructure make alternative theories
 *   more feasible but institutional incentives still privilege established
 *   frameworks.
 *
 * KEY AGENTS:
 *   - Alternative Theoretical Frameworks: Primary victims (powerless/trapped) — fitted equally well by available evidence but excluded from institutional acceptance through gatekeeping
 *   - Working Scientists: Secondary victim (moderate/constrained) — forced to commit to dominant theory for funding and publication despite genuine ambiguity, gaining coordination benefits but bearing extraction costs
 *   - Privileged Theory Institution: Primary beneficiary (institutional/arbitrage) — universities, journals, funding bodies built around dominant theory extract prestige and control authority through gate-keeping underdetermined theory choice
 *   - Pluralist Research Movement: Organized alternative (organized/mobile) — open-access journals, interdisciplinary institutes, computational platforms enabling many theories to coexist and be tested in parallel
 *   - Epistemological Doctrine: Institutional inertia (institutional/arbitrage) — the ritualized acknowledgment that underdetermination exists while suppressing alternatives in practice; theater persists while functional constraint weakens
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing institutional theory selection as a logical inevitability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(underdetermination_of_theory, 0.38).
domain_priors:suppression_score(underdetermination_of_theory, 0.42).
domain_priors:theater_ratio(underdetermination_of_theory, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(underdetermination_of_theory, extractiveness, 0.38).
narrative_ontology:constraint_metric(underdetermination_of_theory, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(underdetermination_of_theory, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(underdetermination_of_theory, tangled_rope).
narrative_ontology:human_readable(underdetermination_of_theory, "Underdetermination of Theory by Evidence").
narrative_ontology:topic_domain(underdetermination_of_theory, "epistemology/philosophy_of_science").

domain_priors:requires_active_enforcement(underdetermination_of_theory).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(underdetermination_of_theory, theory_privileged_institutions).
narrative_ontology:constraint_beneficiary(underdetermination_of_theory, dominant_research_paradigm).
narrative_ontology:constraint_victim(underdetermination_of_theory, alternative_theoretical_frameworks).
narrative_ontology:constraint_victim(underdetermination_of_theory, empirical_evidence_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE EMPIRICIST CHALLENGER (SNARE) — An alternative theoretical framework that fits the available evidence equally well but cannot gain institutional acceptance. Trapped by the underdetermination gap itself: even if the evidence is genuinely ambiguous between theories, the established theory controls publication, funding, and graduate training. Maximum extraction experienced — the challenger cannot exit the competitive arena, cannot mobilize evidence in its favor (the evidence underdetermines both), and bears the cost of permanent marginalization.
constraint_indexing:constraint_classification(underdetermination_of_theory, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE WORKING SCIENTIST (TANGLED ROPE) — The research community conducting experiments. Constrained by career pressures and institutional incentives to adopt the dominant theory, but also genuinely benefits from theoretical unification and predictive power that the dominant theory provides. The underdetermination creates mixed extraction: scientists must commit to one theory for grant applications and publication, but enjoy real coordination benefits (shared paradigm, joint progress). Significant extraction but not maximal — moderate agency and modest benefit.
constraint_indexing:constraint_classification(underdetermination_of_theory, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE PRIVILEGED THEORY INSTITUTION (ROPE) — Universities, journals, funding agencies built around the dominant theoretical framework. Benefits from the underdetermination through gate-keeping authority: the constraint that evidence does not uniquely determine theory means that institutional authority can select which theory counts as legitimate. Experiences the underdetermination as pure coordination: we must agree on a shared theoretical language to do science together. The institution gets to pick which language, extracting prestige and resource control. Net beneficiary — arbitrage options mean the institution could adopt alternatives if beneficial, but chooses not to.
constraint_indexing:constraint_classification(underdetermination_of_theory, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: THE PLURALIST RESEARCH MOVEMENT (SCAFFOLD) — Organized agents (open-access journals, independent research institutes, interdisciplinary programs) building parallel theoretical ecosystems. See underdetermination as a temporary problem with a sunset: multiple validated frameworks can coexist in a pluralist research ecology. Digital communication, computational reproducibility, and network science enable many theories to be tested against the same data without institutional gatekeeping. Low effective extraction because organized agents have agency and see an exit path through institutional diversification. Has sunset clause logic: as computational methods enable easier comparative testing of incompatible theories, institutional monopoly on theory selection loses force.
constraint_indexing:constraint_classification(underdetermination_of_theory, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: THE EPISTEMOLOGICAL NATURALIZATION (PITON) — The widespread claim that underdetermination is an inescapable feature of the scientific method itself ('we can never prove a theory true, only fail to disprove it'). This framing has become largely performative: the epistemological ritual of 'acknowledging underdetermination' persists in philosophy and methodology textbooks despite low functional verification. Most working scientists proceed as if their preferred theory is uniquely correct, showing that they don't actually believe underdetermination is binding. The ritual persists through academic inertia — maintained because alternatives in epistemology haven't fully replaced it, not because the doctrine constrains actual practice. Theater ratio high because the acknowledgment of underdetermination has decoupled from behavioral constraints.
constraint_indexing:constraint_classification(underdetermination_of_theory, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / LOGICAL VIEW (MOUNTAIN) — From a civilizational perspective, underdetermination appears as a logical inevitability: any finite set of observations is consistent with infinitely many logically distinct theories (Quine-Duhem thesis). No amount of empirical testing can rule out all but one. This perspective sees underdetermination as a structural limit of deduction itself, similar to Gödel incompleteness. However, the structural data contradicts this classification — the engine will identify it as a false summit, revealing that logical underdetermination (a genuinely immutable feature) is being conflated with institutional underdetermination (the contingent fact that privileged institutions can suppress alternatives).
constraint_indexing:constraint_classification(underdetermination_of_theory, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(underdetermination_of_theory_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(underdetermination_of_theory, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(underdetermination_of_theory, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(underdetermination_of_theory, TR),
    TR >= 0.70.

:- end_tests(underdetermination_of_theory_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. Underdetermination creates asymmetric access to institutional authority — established theories get funding, publication, and graduate student labor; alternatives are starved. But extractiveness is not extreme because (1) the underdetermination is real and not purely institutional propaganda — evidence genuinely does leave theory choice underconstrained — and (2) working scientists do derive real coordination benefits from shared theoretical framework. The extraction is the *institutional amplification* of a genuine epistemological gap, not pure fiction. Suppression (0.42): Moderate-high. Barriers to alternative theories include publication bias, funding concentration, graduate training curricula, and career risk. But suppression is incomplete — alternatives do find venues (specialized journals, conferences, online platforms) and do occasionally catalyze paradigm shifts. Theater ratio (0.58): Moderate-high. The academic ritual of 'acknowledging underdetermination' persists in methodology sections despite low functional constraint on theory adoption. Scientists perform the acknowledgment while acting as though their theory is uniquely justified. This gap has widened with digital infrastructure that makes alternative testing easier but institutional structures that still reward theory monopoly. The trajectory from 0.35 to 0.58 reflects increasing decoupling of doctrine from behavior.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates a full perspectival gap between beneficiary and victim. The privileged theory institution sees underdetermination as pure coordination (Rope) — we must agree on a shared theory to make progress. The alternative theory sees pure extraction (Snare) — I fit the data equally well but cannot gain hearing. The working scientist sees mixed coordination-extraction (Tangled Rope) — the shared paradigm enables joint progress but I'm locked into the dominant framework. The pluralist movement sees a solvable problem with institutional alternatives (Scaffold) — digital infrastructure and network methods enable many theories to compete. The epistemological doctrine sees degraded ritual (Piton) — the acknowledgment of underdetermination persists in writing but is bypassed in actual practice. The analytical observer risks seeing logical inevitability (Mountain) — underdetermination is a mathematical truth — but the structural data reveals this as conflation of two distinct constraints: logical underdetermination (genuinely immutable) and institutional theory suppression (contingent and removable).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from structural relationships to the underdetermination constraint. Privileged institutions have low d (high beneficiary status, arbitrage exit) — they experience the constraint as enabling their authority and can switch theories if beneficial. Alternative theories have high d (victim status, trapped exit) — the evidence supports them equally but institutional authority is not available. Working scientists occupy intermediate d (moderate victim status, constrained exit) — they benefit from theoretical unification but are locked into the privileged framework by career structures. The pluralist movement has moderate-low d because they have organized power and genuine alternatives available. The analytical observer has intermediate d — positioned outside the game but structurally unable to see that logical underdetermination differs fundamentally from institutional suppression.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by decomposing 'underdetermination' into two structurally distinct constraints: (1) LOGICAL UNDERDETERMINATION (ε ≈ 0.05, Mountain) — any finite data set is consistent with infinitely many theories, this is a Quine-Duhem theorem; (2) INSTITUTIONAL THEORY SELECTION (ε ≈ 0.38, Tangled Rope, current story) — privileged institutions suppress alternatives despite logical underdetermination. The false summit detection identifies when the analytical observer conflates (1) and (2) and claims the institutional phenomenon is immutable because the logical phenomenon is. Mandatrophy resolved by showing that logical underdetermination is correctly classified as Mountain, but the institutional constraint that leverages this logical fact is correctly classified as Tangled Rope — contingent, enforceable, and decomposable through pluralist institutional design. The two constraints are linked by network.affects_constraints: logical underdetermination makes institutional suppression possible, but does not require it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    logical_vs_institutional_underdetermination,
    'Is underdetermination a logical necessity (Quine-Duhem: infinite theories fit any finite data) or a contingent institutional arrangement (privileged theories suppress alternatives)?',
    'Comparative historical analysis: societies with pluralist theory selection institutions vs hierarchical ones; computational environments enabling many-theory testing vs traditional journal gatekeeping; measure institutional suppression of alternatives independent of logical underdetermination',
    'If logical necessity: mountain classification is correct, constraint is immutable. If institutional contingency: tangled_rope classification is correct, constraint is enforceable and removable through pluralist institutional reform.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(logical_vs_institutional_underdetermination, conceptual, 'Logical underdetermination vs institutional suppression of alternatives').

omega_variable(
    empirical_criteria_for_theory_selection,
    'Can empirical criteria (simplicity, explanatory power, predictive accuracy, coherence) uniquely determine theory choice when multiple theories fit the data, or are these criteria inherently underdetermined?',
    'Formal analysis of selection criteria; historical case studies of theory choice in paradigm shifts; measurement of inter-rater agreement on which criteria apply and their weighting',
    'If criteria are determinate: underdetermination is weaker than claimed, and institutional gatekeeping is less justified. If criteria are themselves underdetermined: underdetermination is structural and deeper than data-theory gap.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_criteria_for_theory_selection, conceptual, 'Whether empirical selection criteria can disambiguate theories').

omega_variable(
    pluralist_ecology_failure_modes,
    'In research ecosystems with genuinely pluralist theory selection (e.g., applied machine learning with multiple validated frameworks), does underdetermination still extract costs? Or does it become merely coordination overhead?',
    'Comparative study of monolithic vs pluralist research fields; measurement of resource allocation inefficiency in pluralist vs hierarchical systems; tracking of theory adoption timelines and diversity metrics',
    'If pluralist systems eliminate extraction: scaffold perspective is correct and underdetermination is solvable through institutional design. If pluralist systems show residual extraction: underdetermination extracts in all contexts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pluralist_ecology_failure_modes, empirical, 'Whether pluralist theory ecosystems eliminate underdetermination extraction').

omega_variable(
    intertheory_testing_feasibility,
    'Can two incompatible theoretical frameworks be tested against each other using a single data source, or do they inevitably define their own data domains (auxiliary hypothesis problem)?',
    'Formal logic analysis of translation between theories; empirical case studies where theories made discrepant predictions on identical experiments; measurement of success rate of crucial experiments in theory selection',
    'If testable: crucial experiments exist and underdetermination is weaker than Quine-Duhem suggests. If not testable: each theory can redefine the evidence to fit, making institutional selection truly arbitrary and suppression of alternatives truly extractive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intertheory_testing_feasibility, empirical, 'Whether incommensurable theories can be empirically compared').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(underdetermination_of_theory, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(undert_tr_t0, underdetermination_of_theory, theater_ratio, 0, 0.35).
narrative_ontology:measurement(undert_tr_t20, underdetermination_of_theory, theater_ratio, 20, 0.48).
narrative_ontology:measurement(undert_tr_t40, underdetermination_of_theory, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(undert_be_t0, underdetermination_of_theory, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(undert_be_t20, underdetermination_of_theory, base_extractiveness, 20, 0.33).
narrative_ontology:measurement(undert_be_t40, underdetermination_of_theory, base_extractiveness, 40, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(underdetermination_of_theory, information_standard).
narrative_ontology:affects_constraint(underdetermination_of_theory, logical_underdetermination_quine_duhem).
narrative_ontology:affects_constraint(underdetermination_of_theory, institutional_theory_gatekeeping).

% DUAL FORMULATION NOTE:
% The apparent single constraint 'underdetermination of theory' decomposes into logical underdetermination (Mountain: Quine-Duhem theorem) and institutional underdetermination (Tangled Rope: privileged theory suppression). The logical constraint is invariant across observables; the institutional constraint exhibits perspectival variation. Both stories must be included in corpus for false summit detection to function. See network.affects_constraints for upstream logical constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(underdetermination_of_theory, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
