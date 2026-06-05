% ============================================================================
% CONSTRAINT STORY: procedural_legitimacy_decay
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_procedural_legitimacy_decay, []).

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
 *   constraint_id: procedural_legitimacy_decay
 *   human_readable: The Hollow Formality Trap
 *   domain: social/political
 *
 * SUMMARY:
 *   The hollow formality trap represents a bureaucratic procedure that has
 *   lost the justifying social consensus underlying its original design but
 *   maintains its legal force through institutional inertia and beneficiary
 *   gate-keeping. The procedure continues to extract compliance costs from
 *   subjects while delivering minimal functional benefit — the legitimacy on
 *   which it rested has decayed, yet the formal requirement persists. This
 *   creates a structural tension: the procedure simultaneously serves a
 *   residual coordination function (maintaining institutional cohesion,
 *   signaling procedural order) and an extraction mechanism (capturing
 *   compliance costs without reciprocal benefit). The theater_ratio rising
 *   from 0.52 to 0.78 reflects increasing recognition that the procedure's
 *   performance is divorced from its function. The extractiveness rising from
 *   0.35 to 0.52 indicates that as legitimacy decays, the constraint shifts
 *   from appearing as necessary coordination toward appearing as pure rent
 *   extraction. The constraint is a piton rather than a snare because the
 *   theatrical character of the procedure itself (the very emptiness that
 *   makes it extractive) creates structural vulnerability — the procedure
 *   persists through inertia, not through genuine function or beneficiary
 *   strength. Reform movements can target the obvious falseness of the
 *   claimed justification.
 *
 * KEY AGENTS:
 *   - Procedure Subjects: Primary victims (powerless/trapped) — mandatory compliance with procedure despite loss of legitimacy; no exit; bear full compliance costs
 *   - Bureaucratic Gatekeepers: Primary beneficiaries (institutional/arbitrage) — maintain authority through control of formal process; benefit from discretion and gate-keeping power; can selectively enforce
 *   - Advocacy Coalition: Organized reformers (organized/constrained) — attempt to eliminate or replace procedure; constrained by institutional resistance; visible opposition generates legitimacy costs for defenders
 *   - Reform Movement: Powerful agents (powerful/mobile) — build alternative pathways; see procedure as temporary problem vulnerable to replacement; possess capacity to create sunset mechanisms
 *   - Legitimacy-Preserving Institutions: Secondary beneficiaries (institutional/arbitrage) — benefit from procedure as low-cost coordination signal; use compliance to demonstrate institutional standing
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees dual structure of coordination and extraction; identifies piton as transitional state, not permanent equilibrium
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(procedural_legitimacy_decay, 0.52).
domain_priors:suppression_score(procedural_legitimacy_decay, 0.65).
domain_priors:theater_ratio(procedural_legitimacy_decay, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(procedural_legitimacy_decay, extractiveness, 0.52).
narrative_ontology:constraint_metric(procedural_legitimacy_decay, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(procedural_legitimacy_decay, theater_ratio, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(procedural_legitimacy_decay, piton).
narrative_ontology:human_readable(procedural_legitimacy_decay, "The Hollow Formality Trap").
narrative_ontology:topic_domain(procedural_legitimacy_decay, "social/political").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(procedural_legitimacy_decay, bureaucratic_gatekeepers).
narrative_ontology:constraint_victim(procedural_legitimacy_decay, procedure_subjects).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PROCEDURE SUBJECT (SNARE) — Trapped within mandatory compliance. No meaningful exit: formal legal requirement enforces participation regardless of perceived legitimacy. Bears full cost of procedural performance while benefiting minimally from stated function. Maximum experienced extraction without access to alternatives or appeal mechanisms.
constraint_indexing:constraint_classification(procedural_legitimacy_decay, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: BUREAUCRATIC GATEKEEPER (PITON) — Maintains the procedure through institutional inertia despite acknowledging its diminished legitimacy. Benefits from the gate-keeping power and discretionary authority the formal process provides. Theater is performative — the procedure persists because replacing it would require institutional reformation, not because it functionally achieves its stated goals. Arbitrage exit available: can selectively enforce or suspend procedure application.
constraint_indexing:constraint_classification(procedural_legitimacy_decay, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: ADVOCACY COALITION (TANGLED ROPE) — Organized actors attempting to reform or eliminate the procedure. Constrained by political obstacles and institutional resistance but possess some collective agency. Benefits from visibility and legitimacy their advocacy generates; bears costs of sustained opposition. Hybrid: contains coordination function (assembling consent for reform) alongside asymmetric extraction (the procedure prevents their preferred alternatives).
constraint_indexing:constraint_classification(procedural_legitimacy_decay, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: REFORM MOVEMENT (SCAFFOLD) — Sees the hollow procedure as temporary problem with built-in sunset: rising consensus that legitimacy has decayed creates structural opportunity for replacement. Mobile exit through building alternative institutional pathways (legislative reform, regulatory replacement). Low effective extraction because the constraint is perceived as transitional — the very hollowness that makes it extractive to trapped subjects makes it vulnerable to organized resistance. Sunset mechanism: growing public skepticism systematically erodes the procedure's enforceability.
constraint_indexing:constraint_classification(procedural_legitimacy_decay, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: LEGITIMACY-PRESERVING INSTITUTION (ROPE) — Smaller institution or subsidiary whose formal compliance with the procedure enables broader institutional coordination. Net beneficiary through reduced scrutiny and maintained organizational standing. Experiences the procedure as low-cost coordination: participation signals legitimacy without requiring internal reform. Arbitrage exit available but unused — cost of exit (loss of coordination benefits) exceeds cost of compliance.
constraint_indexing:constraint_classification(procedural_legitimacy_decay, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational view, the hollow procedure is a structural hybrid: still serves coordination function (maintaining institutional cohesion, signaling rule-of-law commitment) while simultaneously extracting from trapped subjects (costs of compliance without corresponding benefit). The procedure persists through dual equilibrium: subjects cannot exit; gatekeepers benefit from gate-keeping; institutions benefit from coordination; reformers lack sufficient power. Classification is tangled_rope because both functions (coordination and extraction) are active, not theatrical performance alone.
constraint_indexing:constraint_classification(procedural_legitimacy_decay, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(procedural_legitimacy_decay_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(procedural_legitimacy_decay, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(procedural_legitimacy_decay, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(procedural_legitimacy_decay, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(procedural_legitimacy_decay, TR),
    TR >= 0.70.

:- end_tests(procedural_legitimacy_decay_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The procedure extracts compliance costs without reciprocal benefit — subjects must participate in a process they no longer perceive as legitimate. However, extractiveness is not maximal because the procedure does retain a residual coordination function: institutional actors benefit from reduced scrutiny and maintained standing, and the procedure generates some efficiency (however minimal) in routing cases through established channels. The 0.52 value reflects that the constraint is both extraction and coordination, not pure rent capture. Suppression (0.65): Moderate-high. Significant suppression mechanisms include: formal legal requirement to participate, social stigma for non-compliance, institutional penalties for circumvention, and absence of viable alternative pathways (currently). However, suppression is not total — organized actors can challenge the procedure's legitimacy; rising public skepticism reduces enforcement effectiveness. Theater ratio (0.78): High and rising. The procedure is substantially performative: it continues because it must continue (institutional inertia), not because anyone believes it functionally achieves its stated purpose. The rising trajectory reflects increasing gap between procedural claim and perceived reality. The procedure's legitimacy decay makes its theatrical character more visible over time.
 *
 * PERSPECTIVAL GAP:
 *   The original beneficiary (gatekeeper) sees a low-extraction coordination mechanism that preserves institutional standing through formal compliance. The trap subject sees a snare with no exit and no benefit. The organized reformer sees an extractive procedure that could be replaced with better alternatives. The reform movement sees a temporary problem vulnerable to sunset through consensus shift. The legitimacy-preserving institution sees a low-cost coordination signal. The analytical observer sees a tangled hybrid: the procedure is both extractive (trapping subjects) and coordinative (maintaining institutional cohesion), held together by inertia and beneficiary gate-keeping. The perspectival gap reflects that actors with arbitrage options experience the procedure as coordination, while trapped actors experience it as pure extraction. The piton classification emerges because the procedure's theatrical character — its obvious emptiness and disconnection from function — creates its own structural vulnerability. Pitons are maintained by inertia, not strength; they become targets for reform because their falsehood is visible.
 *
 * DIRECTIONALITY LOGIC:
 *   The primary gatekeeper derives d from institutional power + arbitrage exit (can selectively enforce) + beneficiary status (captures gate-keeping authority). The engine computes low d → negative or minimal f(d) → low effective extraction for this actor. Procedure subjects derive d from powerless status + trapped exit (mandatory compliance regardless of perceived legitimacy) + victim status (bear compliance costs without reciprocal benefit). The engine computes high d → high f(d) → high experienced extraction for this actor. Organized reformers derive d from organized power + constrained exit (can advocate but face institutional obstacles) + mixed beneficiary/victim status (benefit from visibility; victimized by procedure's persistence). The engine computes moderate d → moderate f(d) → moderate experienced extraction. The piton classification derives from high theater_ratio (0.78) and institutional beneficiary (pitons are degraded snares or ropes maintained through theatrical persistence rather than genuine function). The gap between how gatekeepers experience the procedure (low extraction, some coordination benefit, arbitrage mobility) and how trapped subjects experience it (high extraction, no benefit, no exit) is the perspectival core of this constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves as a hybrid tangled_rope/piton that prevents misclassification as pure snare. The snare classification (from the procedure subject's perspective) is correct about the extraction they experience, but incomplete — it misses the residual coordination function that the procedure actually serves for institutional actors. The tangled_rope classification (from the analytical view) captures both the extraction and the coordination, showing why the procedure persists despite its hollowness: it provides real benefits to gatekeepers and institutional actors, even though it extracts from subjects. The piton classification (from the gatekeeper's view) is correct about how the procedure functions from within institutional logic — as an inertial maintenance of authority through formal compliance ritual. The mandatrophy is resolved by recognizing that all three classifications are structurally true from different positions: snare for trapped subjects, tangled_rope from analytical distance, piton from institutional self-perception. The constraint does not collapse into a single type because different actors genuinely experience different structural relationships to it. The framework's role is to show that the hollow procedure is simultaneously extractive (snare for subjects), coordinative (rope for institutions), degraded-but-persistent (piton for gatekeepers), and temporary (scaffold from reformers' view). No single classification is false — they are perspectival truths from different structural positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_decay_threshold,
    'At what level of public perception that a procedure is unjust does legal enforceability collapse into unenforceable theater?',
    'Historical analysis of procedural compliance rates, enforcement costs, and public resistance levels across procedural collapses; survey data on perceived legitimacy vs. compliance behavior',
    'If threshold is high (> 70% public rejection): procedure remains extractive and enforced long after functional hollowing. If threshold is low (< 30%): procedures become unenforceable theaters much earlier, reducing extraction window.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_decay_threshold, empirical, 'Public perception threshold for procedural legitimacy collapse').

omega_variable(
    gatekeeper_dependency_lock,
    'Do bureaucratic gatekeepers become dependent on hollow procedures as exclusive basis for their authority, preventing them from recognizing or enabling reform?',
    'Analysis of institutional resistance patterns; tracking whether gatekeepers actively obstruct reform proposals that would preserve coordination function while reducing extraction',
    'If locked: gatekeepers will maintain procedure indefinitely, converting it fully to snare. If mobile: gatekeepers may tolerate or enable reform that preserves their coordination benefits, shortening extraction window.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(gatekeeper_dependency_lock, conceptual, 'Whether gatekeeper authority dependency prevents procedural reform').

omega_variable(
    alternative_pathway_viability,
    'Do viable alternative mechanisms exist that could provide the coordination function without the extraction mechanism?',
    'Comparative institutional analysis of alternative procedures; cost-benefit analysis of replacement pathways; feasibility assessment of transitional implementation',
    'If viable alternatives exist: scaffold sunset is real and procedure is temporary (piton → scaffold). If no alternatives exist: gatekeepers can indefinitely maintain hollow procedure as least-bad option (piton → snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_pathway_viability, empirical, 'Whether replacement procedures can preserve coordination without extraction').

omega_variable(
    subject_coalition_capacity,
    'Can procedure subjects organize sufficiently to either exit collectively or demand reform faster than gatekeeper resistance can suppress?',
    'Historical analysis of procedural resistance movements; measurement of subject coordination capacity vs. institutional resistance capacity; identification of critical mass thresholds',
    'If subjects coordinate effectively: procedures collapse or reform rapidly (snare → rope or scaffold). If subjects remain fragmented: procedures persist indefinitely regardless of hollowness (snare stabilizes).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subject_coalition_capacity, empirical, 'Whether procedure subjects can organize effective resistance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(procedural_legitimacy_decay, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(procleg_tr_t0, procedural_legitimacy_decay, theater_ratio, 0, 0.52).
narrative_ontology:measurement(procleg_tr_t5, procedural_legitimacy_decay, theater_ratio, 5, 0.65).
narrative_ontology:measurement(procleg_tr_t10, procedural_legitimacy_decay, theater_ratio, 10, 0.78).

% Extraction over time
narrative_ontology:measurement(procleg_be_t0, procedural_legitimacy_decay, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(procleg_be_t5, procedural_legitimacy_decay, base_extractiveness, 5, 0.44).
narrative_ontology:measurement(procleg_be_t10, procedural_legitimacy_decay, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(procedural_legitimacy_decay, enforcement_mechanism).
narrative_ontology:affects_constraint(procedural_legitimacy_decay, institutional_legitimacy_erosion).
narrative_ontology:affects_constraint(procedural_legitimacy_decay, procedural_justice_deficit).

% DUAL FORMULATION NOTE:
% The hollow formality trap decomposes into two related constraints: (1) legitimacy_decay (the collapse of public consensus that justifies the procedure), and (2) procedural_persistence (the institutional inertia that maintains the procedure despite decay). The hollow_formality_trap story represents their intersection — the structural state where decay has occurred but persistence remains. Upstream: legitimacy_decay (ε ≈ 0.30, Rope) establishes the precondition. This story: procedural_legitimacy_decay (ε ≈ 0.52, Piton) describes the sustained extraction despite decay. Downstream: procedural_reform_blockade (ε ≈ 0.65, Snare) would describe gatekeepers actively preventing replacement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
