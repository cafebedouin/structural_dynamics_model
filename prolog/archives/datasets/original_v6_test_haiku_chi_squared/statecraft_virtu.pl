% ============================================================================
% CONSTRAINT STORY: statecraft_virtu
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_statecraft_virtu, []).

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
 *   constraint_id: statecraft_virtu
 *   human_readable: Machiavellian Virtù and State Maintenance
 *   domain: political/statecraft
 *
 * SUMMARY:
 *   Machiavelli's doctrine of virtù describes a structural constraint imposed
 *   by a ruler to maintain state power amidst the chaos of Fortune
 *   (unpredictable external events). Virtù is not moral virtue but strategic
 *   excellence: the prince's calculated use of fear, clemency, deception, and
 *   force to prevent rivals from challenging his authority and to extract
 *   resources (soldiers, taxes, obedience) necessary for state survival. This
 *   constraint exhibits the classical tangled-rope structure: it solves a
 *   genuine coordination problem (preventing civil war and state collapse)
 *   while imposing heavy asymmetric extraction on subject populations who
 *   have no exit option. The constraint's theater_ratio (0.68) reflects that
 *   Machiavelli himself observes the prince's virtue is performed, not
 *   authentic — clemency is calculated, severity is strategic, piety is
 *   instrumental. The ruler must appear virtuous to prevent subjects and
 *   rivals from organizing against him, yet the appearance of virtue is
 *   itself part of the extraction mechanism. Over the interval modeled (20
 *   years of a principality's stability), extractiveness has risen from 0.35
 *   (early consolidation requiring some legitimacy) to 0.58 (mature
 *   extraction as institutional power solidifies). Theater has risen
 *   correspondingly, indicating degradation: the prince no longer needs to
 *   win loyalty through genuine performance of virtue; institutional inertia
 *   carries the regime forward.
 *
 * KEY AGENTS:
 *   - Prince/Ruler: Primary beneficiary (institutional/arbitrage) — captures absolute power, legitimacy, and first claim on state resources; views virtù as coordination mechanism
 *   - Subject Population: Primary victim (powerless/trapped) — bears costs of conscription, taxation, surveillance, summary justice; cannot exit
 *   - Rival Faction Leaders: Secondary victim (powerful/mobile) — face suppression (assassination, exile risk); constrained exit (can relocate but lose power base)
 *   - Military-Administrative Class: Mixed (organized/constrained) — benefits from hierarchy and status but required to execute suppression; constrained exit
 *   - Court Legitimacy Theater: Institutional inertia (institutional/arbitrage) — maintains performative virtue as power consolidates; increasingly decoupled from function
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees both genuine coordination function and substantial extraction; classification as tangled rope reflects this hybrid
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statecraft_virtu, 0.58).
domain_priors:suppression_score(statecraft_virtu, 0.72).
domain_priors:theater_ratio(statecraft_virtu, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statecraft_virtu, extractiveness, 0.58).
narrative_ontology:constraint_metric(statecraft_virtu, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(statecraft_virtu, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statecraft_virtu, tangled_rope).
narrative_ontology:human_readable(statecraft_virtu, "Machiavellian Virtù and State Maintenance").
narrative_ontology:topic_domain(statecraft_virtu, "political/statecraft").

domain_priors:requires_active_enforcement(statecraft_virtu).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statecraft_virtu, prince_institutional_power).
narrative_ontology:constraint_beneficiary(statecraft_virtu, state_apparatus).
narrative_ontology:constraint_victim(statecraft_virtu, subject_population).
narrative_ontology:constraint_victim(statecraft_virtu, rival_factions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBJECT POPULATION (SNARE) — Citizens lack exit options and bear the costs of princely virtue: conscription, taxation, surveillance, and the moral hazard of living under a ruler who views their loyalty as contingent rather than reciprocal. d≈0.92, f(d)≈1.40, σ=1.0 → χ≈0.81.
constraint_indexing:constraint_classification(statecraft_virtu, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: RIVAL FACTION LEADERS (SNARE) — Competitors for power face high suppression (assassination, exile, imprisonment) with constrained exit (can relocate but lose base of power; can ally but lose independence). d≈0.88, f(d)≈1.32, σ=1.1 → χ≈0.85.
constraint_indexing:constraint_classification(statecraft_virtu, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 3: MILITARY-ADMINISTRATIVE CLASS (TANGLED ROPE) — Officers and administrators both enable the prince's power and are constrained by it. They benefit from institutional hierarchy, salary, and social mobility within the system, but cannot exit without losing status. The system requires their active participation and delivers genuine coordination (chain of command, order in chaos). d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.44.
constraint_indexing:constraint_classification(statecraft_virtu, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: THE PRINCE / STATECRAFT INSTITUTION (ROPE) — The ruler views virtù as pure coordination: discipline prevents civil war, fear prevents rebellion, calculated clemency prevents conspiracy. The prince experiences the constraint as a functional problem-solving mechanism. d≈0.08, f(d)≈-0.08, σ=1.1 → χ≈-0.05. Negative effective extraction = net beneficiary.
constraint_indexing:constraint_classification(statecraft_virtu, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 5: COURT LEGITIMACY THEATER (PITON) — Machiavelli himself documents that virtue is performed, not internalized. The prince's clemency, severity, and piety are calculus, not conviction. The theater of legitimate rule persists (ceremonial, legal fiction, divine right) long after its structural function (preventing challengers from claiming moral superiority) has degraded. theater_ratio=0.68 (high performative content relative to functional effect). The court system maintains itself through institutional inertia.
constraint_indexing:constraint_classification(statecraft_virtu, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / STRUCTURAL VIEW (TANGLED ROPE) — From a civilizational scale, Machiavellian virtù solves a real coordination problem (preventing state collapse into factional warfare) while extracting heavily from subject populations (conscription, taxation, loss of bodily autonomy). The constraint is neither pure extraction (it prevents worse — anarchy) nor pure coordination (it distributes costs asymmetrically). d≈0.70, f(d)≈1.12, σ=1.2 → χ≈0.73.
constraint_indexing:constraint_classification(statecraft_virtu, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statecraft_virtu_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(statecraft_virtu, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(statecraft_virtu, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(statecraft_virtu, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(statecraft_virtu, TR),
    TR >= 0.70.

:- end_tests(statecraft_virtu_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. The prince extracts heavily (soldiers, tax, obedience) from subject populations, but the extraction funds state maintenance that produces genuine coordination benefits (security, order, prevention of worse alternatives like factional warfare). The value reflects that the extraction is not purely parasitic — it produces public goods, though distributed very asymmetrically. This distinguishes it from a pure snare. Suppression (0.72): High. The prince maintains power through surveillance, imprisonment, execution of rivals, and the constant threat of force. But suppression is not absolute — alternatives exist (subjects can flee; rivals can build coalitions) but are costly. Theater ratio (0.68): Moderately high. The prince's virtue is substantially performative (calculated clemency, strategic piety, instrumental justice), but not entirely theatrical — the prince must genuinely maintain order and military discipline, which require some real enforcement. The rise over the interval reflects degradation: as power solidifies, the prince can rely more on inertia and fear; genuine performance of virtue becomes less necessary.
 *
 * PERSPECTIVAL GAP:
 *   This constraint generates stark perspectival disagreement. The prince sees rope (pure coordination: preventing civil war, organizing the state). The subject population sees snare (pure extraction: forced conscription, taxation without consent). The military-administrative class sees tangled rope (mixed coordination and extraction: institutional hierarchy enables their advancement, but they cannot exit). Rival faction leaders see snare with constrained exit (high suppression despite their power). The court legitimacy theater is increasingly performative (piton). The analytical observer sees tangled rope overall — genuine coordination function mixed with substantial extraction. The perspectival gap reveals that this constraint solves a coordination problem through extraction rather than through consent or reciprocal benefit.
 *
 * DIRECTIONALITY LOGIC:
 *   Subject population: Victim + trapped → d≈0.92, f(d)≈1.40. Maximal extraction (trapped has no exit). Rival factions: Victim + mobile → d≈0.88, f(d)≈1.32. High extraction despite mobility (costs of exit are severe). Military-administrative class: Mixed (beneficiary of hierarchy + victim of enforcement) + constrained → d≈0.55, f(d)≈0.75. Moderate extraction (they benefit from the institutional ladder but cannot exit without losing status). Prince: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Net beneficiary (experiences the constraint as coordination mechanism). Court theater: Institutional + arbitrage → d≈0.08, f(d)≈-0.08. Maintains itself through inertia. Analytical observer: Civilian analytical position → d≈0.70, f(d)≈1.12. Sees both coordination and extraction; neither pure type fits perfectly.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy through explicit acknowledgment of its tangled structure. Machiavelli himself documents the hybrid: virtù solves the genuine problem of state collapse (prevents anarchy, coordinates military defense, maintains borders) while extracting heavily through coercion. The constraint is NOT pure extraction (which would imply a snare) because the subject populations do receive order, security from invasion, and predictable law (however harsh). It is NOT pure coordination (which would imply a rope) because the benefits are distributed radically asymmetrically — the prince captures disproportionate gains while subjects bear conscription and taxation. The tangled rope classification reflects this: the constraint requires active enforcement (virtù is strategic, not internalized), possesses both a genuine coordination function and asymmetric extraction, and involves at least one beneficiary (prince/state) and victims (subjects). The increasing theater ratio over time indicates degradation of the tangled rope toward piton: as power consolidates, the prince can rely more on inertia (fear of the known) than on performance of virtue (clemency, calculated mercy). This is consistent with the constraint's natural lifecycle: tangled ropes age into pitons as the extraction mechanism becomes self-sustaining through habit and institutional inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    virtue_necessity_threshold,
    'Is Machiavellian virtù a structural necessity for maintaining a state against rival factions, or is it a contingent extraction mechanism that could be replaced by republican institutions or democratic consent?',
    'Historical-comparative analysis: states that transitioned from principalities to republics/democracies without collapse; examination of whether these transitions reduced extraction while maintaining order',
    'If necessary: Mountain (inherent to state maintenance). If contingent: Tangled Rope or Scaffold (institutional arrangement, potentially with sunset).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(virtue_necessity_threshold, conceptual, 'Whether virtù is inherent to statecraft or a contingent institutional form').

omega_variable(
    fortune_vs_virtue_causation,
    'How much of state collapse results from lack of virtù (inadequate suppression, poor strategic judgment) versus from Fortune (external invasion, plague, economic crisis) beyond the prince''s control?',
    'Counterfactual analysis of historical principalities; identification of states that failed despite high virtù and states that survived despite low virtù; estimation of the variance explained by virtù vs. exogenous shocks',
    'If virtù is primary: Snare/Tangled Rope classification confirmed (extraction has functional payoff). If Fortune is primary: virtù is largely performative (Piton classification confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fortune_vs_virtue_causation, empirical, 'Causal weight of virtù versus Fortune in state stability').

omega_variable(
    subject_coordination_capacity,
    'Could subject populations maintain order through horizontal coordination (guilds, communes, self-governance) without princely enforcement, reducing the extraction while preserving coordination benefits?',
    'Historical analysis of medieval communes, city-states, and guild-based governance; comparative institutional analysis with contemporary horizontally-governed systems; identification of failure modes unique to non-princely systems',
    'If coordination capacity is high: virtù is extractive rather than necessary (Snare). If capacity is low: virtù provides essential order (Scaffold or Tangled Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subject_coordination_capacity, empirical, 'Whether subjects could self-organize without princely authority').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statecraft_virtu, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(statecraft_tr_t0, statecraft_virtu, theater_ratio, 0, 0.52).
narrative_ontology:measurement(statecraft_tr_t10, statecraft_virtu, theater_ratio, 10, 0.6).
narrative_ontology:measurement(statecraft_tr_t20, statecraft_virtu, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(statecraft_be_t0, statecraft_virtu, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(statecraft_be_t10, statecraft_virtu, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(statecraft_be_t20, statecraft_virtu, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statecraft_virtu, enforcement_mechanism).
narrative_ontology:affects_constraint(statecraft_virtu, factional_warfare_prevention).
narrative_ontology:affects_constraint(statecraft_virtu, legitimacy_performance).
narrative_ontology:affects_constraint(statecraft_virtu, resource_extraction_infrastructure).

% DUAL FORMULATION NOTE:
% Machiavellian virtù is downstream of the fundamental problem of maintaining state power against rivals (factional_warfare_prevention). The constraint represents one solution to that problem — extraction through strategic performance. Alternative solutions (republicanism, institutional legitimacy, democratic consent) would constitute different constraints with different ε values. This story models the extractive enforcement pathway; constraint_id legitimacy_performance models the performance/theater aspect separately.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
