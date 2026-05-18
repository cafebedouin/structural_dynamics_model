% ============================================================================
% CONSTRAINT STORY: leuctra_competence_gap_surfacing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_leuctra_competence_gap_surfacing, []).

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
 *   constraint_id: leuctra_competence_gap_surfacing
 *   human_readable: Spartan Military Competence Gap Surfacing at Leuctra
 *   domain: ancient_politics/military_collapse
 *
 * SUMMARY:
 *   The Battle of Leuctra (371 BCE) represents the surfacing and catastrophic
 *   breakdown of an anchored-fixity constraint: Spartan military doctrine,
 *   training requirements, and tactical assumptions had accumulated a
 *   significant competence gap relative to the evolving battlefield
 *   environment, but the institutional suppression mechanisms (discipline
 *   enforcement, authority deference, doctrine conservation) prevented
 *   recognition or adaptation. The constraint operated invisibly for
 *   centuries while conditions remained stable — Sparta defeated every
 *   opponent using the shallow phalanx formation and rigid frontal doctrine.
 *   But as Theban innovations (oblique phalanx, deeper columns, elite Sacred
 *   Band concentration) matured and as Sparta's demographic collapse made
 *   battlefield losses irreplaceable, the gap became critical. The Theban
 *   victory revealed in a single afternoon that the Spartan operational
 *   substrate had eroded past the point where the formal kernel (doctrine and
 *   discipline) could sustain itself. This constraint exhibits extraction
 *   through enforced incompetence: the institutional apparatus extracts
 *   flexibility, responsiveness, and learning capacity from the entire
 *   military system while providing no return benefit — only the continuation
 *   of a coordination structure that was adaptive in the past but became
 *   maladaptive. The theater ratio increases dramatically as the constraint
 *   persists: by Leuctra, Spartan military ritual (reviews, exhibitions,
 *   training) becomes increasingly performative, maintaining the appearance
 *   of military dominance while actual capability erodes.
 *
 * KEY AGENTS:
 *   - Spartan Hoplite Formation: Powerless/trapped victim — locked into rigid doctrine with no cognitive permission to deviate; bears full cost of tactical inflexibility
 *   - Spartan Citizen-Soldier Reserve: Moderate/trapped victim — demographic collapse means losses are irreplaceable; cannot reduce obligations without losing hegemony, cannot increase without accelerating collapse
 *   - Spartan High Command: Institutional/constrained — benefits from formal doctrine structure but trapped by suppression of innovation; reformation risks dissolving cohesion
 *   - Theban Innovation Apparatus: Institutional/mobile — experiences Spartan dominance as constraint preventing their pathway; after breakdown, constraint is eliminated
 *   - Spartan Helot System: Institutional/arbitrage — maintains extractive suppression through inertia despite degraded functional purpose (piton classification)
 *   - Analytical Observer: Universal/analytical — risks naturalizing Spartan institutional rigidity as immutable law of military history
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(leuctra_competence_gap_surfacing, 0.68).
domain_priors:suppression_score(leuctra_competence_gap_surfacing, 0.72).
domain_priors:theater_ratio(leuctra_competence_gap_surfacing, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(leuctra_competence_gap_surfacing, extractiveness, 0.68).
narrative_ontology:constraint_metric(leuctra_competence_gap_surfacing, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(leuctra_competence_gap_surfacing, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(leuctra_competence_gap_surfacing, snare).
narrative_ontology:human_readable(leuctra_competence_gap_surfacing, "Spartan Military Competence Gap Surfacing at Leuctra").
narrative_ontology:topic_domain(leuctra_competence_gap_surfacing, "ancient_politics/military_collapse").

domain_priors:requires_active_enforcement(leuctra_competence_gap_surfacing).

% --- Structural relationships ---
narrative_ontology:constraint_victim(leuctra_competence_gap_surfacing, spartan_military_apparatus).
narrative_ontology:constraint_victim(leuctra_competence_gap_surfacing, spartan_citizen_reserves).
narrative_ontology:constraint_victim(leuctra_competence_gap_surfacing, spartan_hegemonic_control).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SPARTAN HOPLITE FORMATION (SNARE) — Locked into rigid tactical doctrine (the shallow phalanx, the assumption of frontal engagement) with no cognitive permission to deviate. Individual hoplites cannot exit the formation without death or dishonor. The formation itself cannot pivot to oblique echelon or deepen without violating the discipline mechanism that maintains cohesion. Maximum extraction: the hoplite bears the full cost of tactical inflexibility while the institutional framework that mandated the doctrine survives the collapse.
constraint_indexing:constraint_classification(leuctra_competence_gap_surfacing, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: SPARTAN CITIZEN-SOLDIER RESERVE (SNARE) — The demographic collapse means Sparta cannot absorb the 4,000 casualties Leuctra inflicted. Each loss represents an irreplaceable citizen-soldier. The reserve is trapped by the two-front pressure: cannot reduce military obligations without losing hegemony, cannot increase obligations without accelerating demographic collapse. The constraint extracts continuously through escalating unavoidable losses.
constraint_indexing:constraint_classification(leuctra_competence_gap_surfacing, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: SPARTAN HIGH COMMAND (TANGLED ROPE) — Benefits from the formal doctrine structure (which has defeated every opponent for 200 years) while simultaneously trapped by the suppression of tactical innovation. The command apparatus has institutional stability, but that stability is purchased through mandatory blindness to changing conditions. The constraint enforces coordination among units (the phalanx discipline) while extracting flexibility and responsiveness. Institutional power but constrained exit — reformation of doctrine risks dissolving the cohesion mechanism.
constraint_indexing:constraint_classification(leuctra_competence_gap_surfacing, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: THEBAN INNOVATION (ROPE) — The Theban system (oblique phalanx, deeper columns, elite Sacred Band concentration) is pure coordination without extraction overhead. Each innovation solves a specific coordination problem without requiring a victim class. The Thebans experience the constraint negatively — Spartan dominance suppresses their innovation pathway — but after the constraint breaks, their experience flips to pure enabling. From their perspective at Leuctra, the breakdown is liberation, not extraction.
constraint_indexing:constraint_classification(leuctra_competence_gap_surfacing, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 5: THE SPARTAN HELOT SYSTEM (PITON) — The constraint of helot suppression is theoretically critical to Spartan hegemony (freeing citizens for military service) but by 371 is largely degraded through theater. Constant low-intensity helot rebellion requires suppression apparatus that drains military capacity. The helot-control theater persists through inertia despite diminishing returns. Leuctra reveals that the helot constraint's extractive function had already eroded — maintaining helot suppression no longer subsidizes military dominance because the demographic base can no longer support both. The theater continues after the battle despite its function collapse.
constraint_indexing:constraint_classification(leuctra_competence_gap_surfacing, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a sufficiently removed analytical position, military dominance always exhibits a competence lag: the victor becomes optimized to yesterday's battlefield conditions. The constraint appears as an immutable structural property of military history — configurations that were adaptive become maladaptive as environment changes faster than institutional authority can acknowledge. However, the false summit detector will identify this as naturalization: Sparta's specific tactical rigidity and suppression of innovation are contingent institutional choices, not natural law.
constraint_indexing:constraint_classification(leuctra_competence_gap_surfacing, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(leuctra_competence_gap_surfacing_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(leuctra_competence_gap_surfacing, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(leuctra_competence_gap_surfacing, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(leuctra_competence_gap_surfacing, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(leuctra_competence_gap_surfacing, TR),
    TR >= 0.70.

:- end_tests(leuctra_competence_gap_surfacing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and increasing. The constraint extracts flexibility, innovation capacity, and learning ability from the entire Spartan military apparatus. The extraction is not classical rent-seeking but institutional incapacity — the system cannot adapt even when adaptation becomes necessary for survival. The baseline (0.35 at T=0) reflects that the constraint operated invisibly for centuries; it only becomes extractive when environmental conditions accelerate beyond the suppression mechanisms' capacity to contain them. The increase to 0.68 by Leuctra reflects the growing gap between organizational capability and battlefield reality. Suppression (0.72): Very high. The suppression mechanism operates through discipline enforcement, authority deference norms, and the ideological centering of Spartan martial superiority as a civilizational given. Helots suppress alternative labor organization. The citizen-soldier obligation suppresses non-military life paths. The doctrine itself suppresses tactical innovation through cultural prohibition. Theater ratio (0.81): Very high by Leuctra. Spartan military reviews, exhibitions, and training become increasingly ritualistic as the connection between formal performance and actual capability breaks. The constraint persists through theater — the appearance of dominance is maintained ceremonially while actual fighting capacity erodes. The high theater ratio indicates that the constraint is sustained not by genuine functional necessity but by institutional inertia and cultural mythology.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates catastrophic perspectival divergence: from the hoplite perspective, the structure is a pure snare with maximum suppression and no exit; from the high command perspective, it is a tangled rope where doctrine provides real coordination benefits at the cost of suppressed innovation; from the Theban perspective, it is a simple barrier to their own coordination (rope from outside); from the helot system, it is a piton where suppression theater persists through inertia. The magnitude of the gap — four different primary classifications from core perspectives — indicates a constraint approaching terminal instability. A stable constraint produces consistent classification across perspectives (all snare, all rope, etc.). A constraint with moderate perspectival gaps produces mixed classifications but with consistent underlying structure (hoplite sees snare, high command sees tangled rope, but both recognize the same suppression mechanism). This constraint's gap suggests the suppression mechanism is about to fail catastrophically because it depends on all perspectives maintaining cognitive alignment, and that alignment is breaking. Leuctra is what happens when the perspectival gap becomes unsustainable.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint exhibits no beneficiary class in the classical sense because extraction under Leuctra conditions does not benefit any surviving agent. Spartan high command might have received long-term institutional benefits from doctrine conservation (stable authority, preserved identity, coherent system), but those benefits evaporate completely in the battlefield collapse. The direction of extraction is unidirectional — toward institutional continuity and away from flexibility — until the battlefield reveals that the institutional continuity was purchased by sacrificing the actual military capability required to sustain it. This is a signature of a snare transitioning to terminal failure: the extraction mechanism (suppression of innovation) was always present, but it only becomes obvious as a pure snare when the extraction ceases to produce any return benefit. In the pre-Leuctra period, suppression might have been defensible as a coordination cost (tangled rope framing). By Leuctra, the return from that coordination has collapsed, and the constraint is revealed as pure extraction. Directionality shifts from d ≈ 0.60 (victim with some institutional benefit from system stability) to d ≈ 0.90 (victim bearing full cost with no offsetting benefit) over the constraint's lifecycle.
 *
 * MANDATROPHY ANALYSIS:
 *   TERMINAL SNARE WITH INSTITUTIONAL BIFURCATION: This constraint resolves the mandatrophy by demonstrating the distinction between a snare that extracts continuously vs. one that extracts from an institutional structure that is itself about to fail. The classical mandatrophy asks: 'Is this pure extraction (snare) or mixed coordination-extraction (tangled rope)?' The answer here is: it was tangled rope (doctrine provided real coordination) but became snare (when coordination benefits collapsed faster than suppression could adapt). The analytical challenge is that Sparta's high command genuinely experiences the constraint as coordination (the phalanx discipline works) until the moment it catastrophically fails. The constraint is therefore a snare from the perspective of the victims (hoplites, reserves) but appears as tangled rope from the perspective of those maintaining it (high command) until the moment of breakdown. Leuctra resolves the mandatrophy by empirical revelation: the constraint was always a snare for the troops but was hidden by institutional theater and ideology. The breakdown is mandatrophy resolution through falsification — the constraint's claimed benefit (military dominance through doctrine) is revealed to have evaporated, and the constraint stands naked as pure suppression of adaptive capacity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    demographic_collapse_causation,
    'Is the Spartan demographic collapse the cause of the competence gap or a symptom of it? Did rigid doctrine deplete population through unsustainable conquest requirements, or did prior population loss force reliance on increasingly rigid doctrine?',
    'Historical reconstruction of casualty rates, birth rate data, and temporal correlation of doctrine rigidity with population metrics across 50 years before Leuctra',
    'If doctrine caused collapse: the snare is self-catalyzing (institutional choices cascaded into population pressure). If collapse forced doctrine: the snare is a response to prior constraint (the demographic pressure was primary). Classification remains snare either way, but causality affects which omega dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demographic_collapse_causation, empirical, 'Whether demographic collapse caused doctrine rigidity or vice versa').

omega_variable(
    doctrine_innovation_suppressibility,
    'Was the suppression of tactical innovation a necessary feature of Spartan discipline, or could the phalanx formation have accommodated oblique and deeper variants without losing cohesion?',
    'Archaeological and textual analysis of non-Spartan phalanx variants; reconstruction of communication and coordination mechanisms in deeper formations; comparative stability analysis of rigid vs flexible formation discipline',
    'If suppressible: the constraint is extractive institutional choice (snare reclassifies). If necessary: the constraint is structural coordination requirement (shifts toward tangled_rope). Current classification assumes suppressible but acknowledges uncertainty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_innovation_suppressibility, empirical, 'Whether doctrine innovation suppression was necessary for discipline').

omega_variable(
    exit_window_recognizability,
    'Could Spartan leadership have recognized the competence gap and adapted doctrine before Leuctra, or was the gap objectively non-visible from within the Spartan institutional frame until the battlefield revealed it?',
    'Analysis of pre-Leuctra Theban innovations and Spartan tactical responses; reconstruction of information available to high command; comparison with post-Leuctra adaptive attempts',
    'If gap was recognizable: high command chose suppression (snare classification confirmed). If gap was invisible: constraint is a true anchored fixity breakdown (snare due to suppression but suppression was not chosen). Classification type remains snare; impact affects culpability attribution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(exit_window_recognizability, conceptual, 'Whether the competence gap was recognizable before Leuctra').

omega_variable(
    false_summit_naturalization,
    'Is the Analytical/Mountain perspective a genuine natural law about military dominance and competence lag, or is it naturalizing Spartan institutional choices as inevitable?',
    'Comparison with other hegemonic collapses (Athenian, Roman, etc.) to establish whether rigid doctrine + demographic collapse + battlefield defeat is a structural law or a recurring institutional pattern that could be avoided',
    'If law: mountain classification stands. If pattern: mountain is false summit — the constraint is contingent Spartan institutional arrangements. False summit detection will flag this via beneficiary analysis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_naturalization, empirical, 'Whether military competence lag is natural law or institutional choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(leuctra_competence_gap_surfacing, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(leuctra_tr_t0, leuctra_competence_gap_surfacing, theater_ratio, 0, 0.58).
narrative_ontology:measurement(leuctra_tr_t10, leuctra_competence_gap_surfacing, theater_ratio, 10, 0.71).
narrative_ontology:measurement(leuctra_tr_t20, leuctra_competence_gap_surfacing, theater_ratio, 20, 0.81).

% Extraction over time
narrative_ontology:measurement(leuctra_be_t0, leuctra_competence_gap_surfacing, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(leuctra_be_t10, leuctra_competence_gap_surfacing, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(leuctra_be_t20, leuctra_competence_gap_surfacing, base_extractiveness, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(leuctra_competence_gap_surfacing, enforcement_mechanism).
narrative_ontology:affects_constraint(leuctra_competence_gap_surfacing, spartan_hegemonic_stability).
narrative_ontology:affects_constraint(leuctra_competence_gap_surfacing, greek_city_state_military_innovation).
narrative_ontology:affects_constraint(leuctra_competence_gap_surfacing, helot_suppression_extractiveness).

% DUAL FORMULATION NOTE:
% This constraint is structurally downstream of the helot system (which freed Spartan citizens for military service) and the demographic collapse (which reduced casualty-absorption capacity), but represents a distinct structural problem: the inability of Spartan institutional authority to recognize and adapt to changing tactical environment. The upstream constraints establish the conditions for competence gap accumulation; this constraint models the surface and breakdown of that accumulated gap.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(leuctra_competence_gap_surfacing, institutional, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
