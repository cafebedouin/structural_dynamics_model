% ============================================================================
% CONSTRAINT STORY: punishment_regress
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_punishment_regress, []).

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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: punishment_regress
 *   human_readable: Punishment Regress in Dirty Hands Ethics
 *   domain: political_philosophy/normative_ethics/applied_ethics
 *
 * SUMMARY:
 *   The punishment regress is a theoretical artifact of Michael Walzer's
 *   dirty hands framework: if political actors who commit dirty hands acts
 *   must be punished to preserve moral order, but punishment itself is a
 *   dirty hands act (it harms the punished, involves coercion, and claims a
 *   moral authority the punisher may not possess), then the punisher must
 *   also be punished, and so on infinitely. Walzer acknowledged this regress
 *   in 'Political Action: The Problem of Dirty Hands' (1973) but provided no
 *   stopping principle. Fifty years later, the regress persists in the
 *   philosophical literature as a puzzle and pedagogical example, but it has
 *   never been operationalized in any actual accountability structure.
 *   Political actors are held accountable through elections, investigations,
 *   trials, and institutional reforms without triggering further regress;
 *   citizens demand accountability without experiencing the regress as a
 *   binding obligation to punish themselves. The constraint exhibits high
 *   theater ratio (0.78) because it is maintained almost entirely through
 *   citation networks and classroom discussion rather than through any
 *   functional coordination or extraction mechanism. The regress vindicates
 *   two propositions central to the dirty hands framework: that moral
 *   remainders are irreducible (the regress shows that no amount of
 *   punishment can restore moral purity), and that dirty hands dilemmas
 *   cannot be resolved through procedural or institutional design (the
 *   regress infects even the accountability mechanisms meant to address dirty
 *   hands). But the regress itself has atrophied as a practical constraint —
 *   it is a piton, a degraded structure maintained through disciplinary
 *   inertia.
 *
 * KEY AGENTS:
 *   - Academic Theorists: Primary beneficiaries (institutional/arbitrage) — the regress provides a durable puzzle that sustains research programs and vindicates the dirty hands framework's claim to irreducibility
 *   - Political Actors: Non-victims (powerful/mobile) — experience the regress as a non-constraint; actual accountability mechanisms do not operationalize regress logic
 *   - Citizens Holding Leaders Accountable: Non-victims (moderate/constrained) — demand accountability without experiencing the regress as a binding obligation to punish themselves
 *   - Restorative Justice Movement: Organized agents (organized/constrained) — building alternative accountability frameworks that dissolve the regress by not claiming moral purity
 *   - Moral Philosophy Subdiscipline: Institutional beneficiary — the regress sustains a research niche and pedagogical tradition
 *   - Analytical Observer: Sees the regress as a degraded constraint maintained through citation and repetition rather than structural necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(punishment_regress, 0.18).
domain_priors:suppression_score(punishment_regress, 0.25).
domain_priors:theater_ratio(punishment_regress, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(punishment_regress, extractiveness, 0.18).
narrative_ontology:constraint_metric(punishment_regress, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(punishment_regress, theater_ratio, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(punishment_regress, piton).
narrative_ontology:human_readable(punishment_regress, "Punishment Regress in Dirty Hands Ethics").
narrative_ontology:topic_domain(punishment_regress, "political_philosophy/normative_ethics/applied_ethics").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(punishment_regress, moral_philosophy_subdiscipline).
narrative_ontology:constraint_beneficiary(punishment_regress, academic_theorists).
narrative_ontology:constraint_vindicates(punishment_regress, moral_remainder_doctrine).
narrative_ontology:constraint_vindicates(punishment_regress, dirty_hands_irreducibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ACADEMIC THEORIST (PITON) — The regress is maintained as a theoretical puzzle despite its practical irrelevance. Philosophers continue to cite and elaborate the regress structure not because it constrains actual political judgment but because it vindicates the dirty hands framework's claim to irreducibility. The constraint persists through disciplinary inertia and citation networks. Low extraction — the theorist benefits from the puzzle's existence but is not trapped by it.
constraint_indexing:constraint_classification(punishment_regress, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 2: POLITICAL ACTOR (ROPE) — Experiences the regress as a non-constraint. Political actors who commit dirty hands acts face punishment or accountability mechanisms, but the regress itself (that punishing them dirties our hands, requiring us to be punished, etc.) is a theoretical artifact with no operational force. The actor sees coordination: accountability mechanisms exist, they function, and the infinite regress is simply not a feature of the actual institutional landscape. Negligible extraction.
constraint_indexing:constraint_classification(punishment_regress, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: CITIZEN ACCOUNTABILITY AGENT (PITON) — Citizens who demand accountability for dirty hands acts (voting out leaders, supporting investigations, advocating for institutional reform) do not experience the regress as a binding constraint. The theoretical claim that demanding accountability dirties their own hands and requires them to pay a price is not operationalized in any institutional structure. The regress is maintained as a philosophical talking point but has atrophied as a practical constraint. Low extraction — the citizen is not trapped by the regress logic.
constraint_indexing:constraint_classification(punishment_regress, piton,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: LOGICAL PURIST (MOUNTAIN) — From a purely logical standpoint, if one accepts the premises (punishment is itself a dirty hands act; dirty hands acts require punishment; no stopping principle exists), the regress follows necessarily. This perspective treats the regress as an immutable logical structure — a reductio ad absurdum of the dirty hands framework's own premises. However, this mountain classification is a false summit: the regress is not a law of logic but a consequence of specific normative commitments (that punishment dirties hands, that all dirty hands acts must be punished) that are themselves contestable and historically contingent.
constraint_indexing:constraint_classification(punishment_regress, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 5: RESTORATIVE JUSTICE MOVEMENT (SCAFFOLD) — Organized agents building alternative accountability frameworks (restorative justice, truth and reconciliation commissions, transformative justice) see the regress as a temporary artifact of retributive punishment paradigms. As accountability mechanisms shift from punishment-as-retribution to repair-and-transformation, the regress dissolves: restorative processes do not dirty hands in the same way retributive punishment does, because they do not claim moral purity. The constraint has a sunset: the regress loses force as restorative norms mature. Moderate extraction — the movement faces institutional resistance but has agency and sees an exit path.
constraint_indexing:constraint_classification(punishment_regress, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (PITON) — The regress is a degraded constraint. Walzer acknowledged the regress structure in 1973 but provided no stopping principle and no institutional mechanism for operationalizing it. Fifty years later, the regress persists in the literature as a theoretical curiosity but has no force in actual political accountability structures. Political actors are punished or held accountable without triggering further regress; citizens demand accountability without experiencing the regress as a binding obligation. The constraint is maintained through citation and pedagogical repetition, not through structural necessity. High theater ratio — the regress is mostly performance, a philosophical puzzle kept alive by disciplinary convention rather than by any real coordination or extraction function.
constraint_indexing:constraint_classification(punishment_regress, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(punishment_regress_tests).

test(piton_threshold) :-
    domain_priors:theater_ratio(punishment_regress, TR),
    TR >= 0.70.

:- end_tests(punishment_regress_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. The regress extracts minimal value from anyone. Academic theorists benefit from the puzzle's existence (it sustains research and vindicates the dirty hands framework), but this is a modest disciplinary benefit, not substantial extraction. Political actors and citizens are not constrained by the regress in practice — accountability mechanisms function without operationalizing regress logic. The low extractiveness reflects that the regress is mostly inert. Suppression (0.25): Low. The regress does not suppress alternatives or coerce compliance. It is a theoretical claim, not an enforced rule. The modest suppression value reflects that the regress does exert some normative pressure within the dirty hands literature (it discourages attempts to resolve dirty hands dilemmas through institutional design by suggesting that all accountability mechanisms are themselves morally compromised), but this pressure is weak and easily ignored by practitioners. Theater ratio (0.78): High. The regress is maintained almost entirely through performative citation and pedagogical repetition. Walzer acknowledged the regress but provided no stopping principle and no institutional mechanism. Fifty years later, the regress appears in syllabi and literature reviews but has no operational force. The high theater ratio reflects that the constraint's primary function is symbolic — it demonstrates the dirty hands framework's commitment to moral irreducibility — rather than functional.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is between those who see the regress as a live theoretical constraint (the logical purist's mountain, the academic theorist's piton-as-resource) and those who see it as operationally irrelevant (the political actor's rope, the citizen's piton-as-inert-artifact). The analytical observer's piton classification synthesizes these: the regress is a real logical structure (the purist is not wrong about the formal regress), but it has atrophied as a practical constraint because no institutional designer has ever taken it seriously as a design requirement. The restorative justice movement's scaffold perspective introduces a temporal dimension: the regress may be dissolving as accountability paradigms shift away from retributive punishment. The gap reveals that the regress's force is entirely a function of whether one accepts the premises (punishment always dirties hands; all dirty hands acts must be punished) and whether one treats logical consistency as a binding constraint on institutional design. Political actors and citizens implicitly reject one or both premises; academic theorists accept both but treat the regress as a theoretical curiosity rather than a practical obligation.
 *
 * DIRECTIONALITY LOGIC:
 *   Academic theorists are beneficiaries: the regress sustains a research niche and vindicates core commitments of the dirty hands framework (moral remainder irreducibility, institutional design insufficiency). Their directionality is low (d ≈ 0.2), producing low or negative effective extraction — they experience the regress as a resource, not a cost. Political actors and citizens are neither beneficiaries nor victims in any structural sense: the regress does not constrain their behavior or extract value from them. Their directionality is near-neutral (d ≈ 0.5), and with mobile or arbitrage exit options, their effective extraction is negligible. The restorative justice movement is a secondary beneficiary in a paradoxical sense: the regress's incoherence strengthens the case for alternative accountability paradigms that do not rely on retributive punishment. Their directionality is low-moderate (d ≈ 0.3), and with organized power and constrained exit, they experience modest extraction from the retributive paradigm the regress is embedded in, but they see an exit path (the scaffold sunset). No agent is a clear victim of the regress itself — the constraint is too inert to extract substantially from anyone.
 *
 * MANDATROPHY ANALYSIS:
 *   The punishment regress is a piton because its primary function (operationalizing the infinite regress logic in actual accountability structures) has atrophied, but the constraint persists through disciplinary inertia and citation networks. The regress was never a robust coordination mechanism — Walzer acknowledged it but provided no stopping principle, and no institutional designer has ever attempted to implement regress logic. What remains is a theoretical puzzle that vindicates the dirty hands framework's core commitments (moral remainder irreducibility, institutional design insufficiency) without constraining actual political judgment or accountability practices. The high theater ratio (0.78) reflects that the regress is maintained almost entirely through performative citation rather than functional necessity. The low extractiveness (0.18) reflects that the regress extracts minimal value from anyone — it is a disciplinary resource for theorists but not a binding constraint on practitioners. The constraint is not a snare (no clear victims), not a tangled rope (no active enforcement or substantial extraction), not a scaffold (no sunset clause in the original formulation, though the restorative justice movement may be building one), and not a rope (no genuine coordination function). The mountain classification at the analytical/logical-purist perspective is a false summit: the regress is not a law of logic but a consequence of specific normative premises that are themselves contestable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    stopping_principle_existence,
    'Does a principled stopping point exist within the dirty hands framework, or is the regress genuinely infinite?',
    'Philosophical analysis of proposed stopping principles (democratic authorization, proportionality thresholds, restorative vs retributive distinction); empirical observation of whether actual accountability structures operationalize regress logic or implicitly adopt stopping rules',
    'If a stopping principle exists: the regress is a solvable coordination problem (Rope from more perspectives). If genuinely infinite: the regress is either a reductio of the framework (Mountain/false summit) or a permanently inert theoretical artifact (Piton).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(stopping_principle_existence, conceptual, 'Whether a principled stopping point exists for the punishment regress').

omega_variable(
    punishment_hand_dirtying_threshold,
    'Does punishment always dirty hands, or only punishment that exceeds proportionality or violates due process?',
    'Normative analysis of when punishment constitutes a dirty hands act; comparison of retributive vs restorative accountability mechanisms; examination of whether democratic authorization or procedural justice provides moral insulation',
    'If all punishment dirties hands: the regress is structural and unavoidable (Mountain or Piton depending on operationalization). If only disproportionate or procedurally unjust punishment dirties hands: the regress is avoidable through institutional design (Rope or Scaffold).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(punishment_hand_dirtying_threshold, conceptual, 'Whether all punishment dirties hands or only certain forms').

omega_variable(
    restorative_justice_escape,
    'Do restorative or transformative justice mechanisms avoid the regress by not claiming moral purity, or do they simply relocate the dirty hands problem?',
    'Empirical study of restorative justice outcomes; philosophical analysis of whether repair-focused accountability avoids the moral remainder that triggers regress; comparison of participant experiences in retributive vs restorative processes',
    'If restorative mechanisms escape the regress: Scaffold perspective confirmed, sunset is real. If they relocate the problem: the regress is more fundamental than accountability paradigm choice, and Piton or Mountain perspectives are more accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restorative_justice_escape, empirical, 'Whether restorative justice mechanisms escape the punishment regress').

omega_variable(
    operationalization_failure_cause,
    'Why has the regress never been operationalized in actual accountability structures — is it because the regress is logically incoherent, practically unworkable, or simply ignored by institutional designers?',
    'Historical analysis of accountability mechanism design; interviews with institutional architects; examination of whether the regress was considered and rejected or simply never taken seriously as a design constraint',
    'If logically incoherent: Mountain/false summit (the regress is a philosophical error). If practically unworkable: Piton (the regress is real but inert). If ignored: the regress may be a latent constraint that could be activated by future institutional reform.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(operationalization_failure_cause, empirical, 'Why the regress has never been institutionally operationalized').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(punishment_regress, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(punish_regress_theater_1973, punishment_regress, theater_ratio, 0, 0.55).
narrative_ontology:measurement(punish_regress_theater_1988, punishment_regress, theater_ratio, 15, 0.68).
narrative_ontology:measurement(punish_regress_theater_2003, punishment_regress, theater_ratio, 30, 0.75).
narrative_ontology:measurement(punish_regress_theater_2023, punishment_regress, theater_ratio, 50, 0.78).

% Extraction over time
narrative_ontology:measurement(punish_regress_extract_1973, punishment_regress, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(punish_regress_extract_1988, punishment_regress, base_extractiveness, 15, 0.22).
narrative_ontology:measurement(punish_regress_extract_2003, punishment_regress, base_extractiveness, 30, 0.2).
narrative_ontology:measurement(punish_regress_extract_2023, punishment_regress, base_extractiveness, 50, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(punishment_regress, information_standard).

% DUAL FORMULATION NOTE:
% The punishment regress is downstream of two upstream constraints: moral_remainder_requirement (tangled_rope) and citizen_complicity_structure (snare). The moral remainder requirement establishes that dirty hands acts leave an irreducible moral residue that must be acknowledged and paid for; the punishment regress extends this logic to the payment mechanism itself, showing that punishment also leaves a remainder. The citizen complicity structure establishes that citizens bear responsibility for their leaders' dirty hands acts; the punishment regress extends this to citizens' accountability-demanding acts, showing that demanding punishment also dirties hands. Both upstream constraints have higher extractiveness than the regress itself because they are operationalized in actual political discourse and institutional design, whereas the regress remains a theoretical artifact.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
