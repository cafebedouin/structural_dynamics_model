% ============================================================================
% CONSTRAINT STORY: operational_overextension_cascade
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_operational_overextension_cascade, []).

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
 *   constraint_id: operational_overextension_cascade
 *   human_readable: Operational Overextension Cascade in Multi-Axis Military Operations
 *   domain: military_operations/information_warfare/organizational_pathology
 *
 * SUMMARY:
 *   The operational overextension cascade emerges when military forces
 *   conduct simultaneous offensive operations across six or more axes without
 *   sufficient force density to achieve breakthroughs or consolidate gains in
 *   any single sector. This constraint is downstream of the beautiful reports
 *   feedback loop: information suppression prevents senior command from
 *   recognizing overextension, causing continued force dispersion, causing
 *   operational failures, causing more concealment. The cascade extracts
 *   combat power, unit cohesion, and logistics capacity from tactical units
 *   while providing political and career benefits to senior command and
 *   political leadership. The constraint exhibits high theater ratio (0.68)
 *   because multi-axis operations create the appearance of initiative and
 *   aggression regardless of territorial outcomes — the performance of
 *   offensive action substitutes for the achievement of operational
 *   objectives. Ukrainian forces experience the constraint as both
 *   opportunity (Russian overextension creates predictable vulnerabilities)
 *   and burden (must maintain defensive readiness across all axes). The
 *   constraint is sustained by hierarchical command structures that prevent
 *   tactical commanders from refusing operationally impossible orders and by
 *   political incentives that reward the appearance of offensive action over
 *   territorial consolidation.
 *
 * KEY AGENTS:
 *   - Russian Tactical Units: Primary victim (powerless/trapped) — ordered to attack in six directions simultaneously without sufficient combat power to succeed in any; cannot refuse orders or consolidate gains
 *   - Frontline Commanders: Secondary victim (moderate/constrained) — receive impossible orders but cannot refuse without career consequences; bear responsibility for unachievable objectives
 *   - Logistics Personnel: Mixed victim-beneficiary (organized/constrained) — coordinate complex multi-axis supply operations but bear blame for failures caused by overextension
 *   - Senior Command Structure: Primary beneficiary (institutional/arbitrage) — capture political credit for 'offensive operations on all fronts' regardless of tactical outcomes
 *   - Political Leadership: Primary beneficiary (institutional/arbitrage) — extract domestic propaganda value and international signaling benefit from multi-axis operations
 *   - Ukrainian Defense Forces: External actor (powerful/mobile) — experience both coordination benefit (Russian overextension creates targeting opportunities) and extraction cost (sustained defensive tempo)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — identifies the cascade as organizational pathology sustained by information suppression
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(operational_overextension_cascade, 0.58).
domain_priors:suppression_score(operational_overextension_cascade, 0.72).
domain_priors:theater_ratio(operational_overextension_cascade, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(operational_overextension_cascade, extractiveness, 0.58).
narrative_ontology:constraint_metric(operational_overextension_cascade, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(operational_overextension_cascade, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(operational_overextension_cascade, snare).
narrative_ontology:human_readable(operational_overextension_cascade, "Operational Overextension Cascade in Multi-Axis Military Operations").
narrative_ontology:topic_domain(operational_overextension_cascade, "military_operations/information_warfare/organizational_pathology").

domain_priors:requires_active_enforcement(operational_overextension_cascade).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(operational_overextension_cascade, senior_command_structure).
narrative_ontology:constraint_beneficiary(operational_overextension_cascade, political_leadership).
narrative_ontology:constraint_victim(operational_overextension_cascade, russian_tactical_units).
narrative_ontology:constraint_victim(operational_overextension_cascade, frontline_commanders).
narrative_ontology:constraint_victim(operational_overextension_cascade, logistics_personnel).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FRONTLINE TACTICAL UNIT (SNARE) — Trapped in simultaneous offensive operations without sufficient force density, artillery support, or logistics capacity to achieve any single objective. Cannot refuse orders, cannot consolidate gains, cannot withdraw without authorization. Experiences maximum extraction: ordered to attack in six directions simultaneously while lacking the combat power to succeed in any. The constraint extracts lives, equipment, and unit cohesion with no coordination benefit visible at this level.
constraint_indexing:constraint_classification(operational_overextension_cascade, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: FRONTLINE COMMANDER (SNARE) — Constrained by hierarchical command structure and career consequences of reporting failure. Receives operationally impossible orders (advance on six axes with insufficient forces) but cannot refuse or request consolidation without risking relief of command. Some agency to manage tactical execution within constraints, but the strategic overextension is imposed from above. High extraction: bears responsibility for unachievable objectives while lacking authority to refuse them.
constraint_indexing:constraint_classification(operational_overextension_cascade, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LOGISTICS NETWORK (TANGLED ROPE) — Organized logistics personnel experience both coordination function (multi-axis operations require complex supply coordination) and asymmetric extraction (impossible demands, blame for operational failures caused by overextension). The constraint coordinates supply distribution across multiple axes but extracts through impossible requirements: maintain six offensive operations simultaneously with supply lines designed for sequential operations. Mixed experience: genuine coordination problem layered with extractive blame assignment.
constraint_indexing:constraint_classification(operational_overextension_cascade, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: SENIOR COMMAND (ROPE) — Benefits from the constraint through political credit for 'offensive operations on all fronts' regardless of tactical outcomes. Experiences the multi-axis strategy as coordination: demonstrating initiative, maintaining pressure, preventing Ukrainian force concentration. Can exit or modify strategy but chooses not to because the political benefits (appearing aggressive, claiming initiative) outweigh tactical costs borne by subordinates. Low effective extraction: the constraint runs toward this agent.
constraint_indexing:constraint_classification(operational_overextension_cascade, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: POLITICAL LEADERSHIP (ROPE) — Primary beneficiary. Multi-axis offensive operations provide domestic propaganda value ('advancing on all fronts'), international signaling ('maintaining initiative'), and political cover for strategic failures. The constraint coordinates political messaging: simultaneous operations create the appearance of strength regardless of territorial outcomes. Maximum benefit, minimal cost: political leadership captures narrative advantage while tactical units bear operational costs.
constraint_indexing:constraint_classification(operational_overextension_cascade, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: UKRAINIAN DEFENSE (TANGLED ROPE) — External actor that experiences both coordination benefit (Russian overextension creates predictable vulnerability patterns, enables efficient counterattack targeting) and extraction cost (must maintain defensive readiness across six axes simultaneously, cannot concentrate forces for decisive counteroffensive). The constraint coordinates Ukrainian defensive strategy (where to position reserves, which axes to reinforce) but extracts through sustained operational tempo requirements. Mixed experience: Russian overextension is both opportunity and burden.
constraint_indexing:constraint_classification(operational_overextension_cascade, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — From a civilizational analytical perspective, the constraint is a snare: a self-reinforcing pathology where beautiful reports feedback (upstream constraint) prevents recognition of overextension, causing continued force dispersion, causing operational failures, causing more beautiful reports to conceal failures. The cascade is structurally extractive: it consumes combat power without achieving strategic objectives, sustained by information suppression rather than coordination logic. High extractiveness, high suppression, no genuine coordination function at the system level.
constraint_indexing:constraint_classification(operational_overextension_cascade, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(operational_overextension_cascade_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(operational_overextension_cascade, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(operational_overextension_cascade, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(operational_overextension_cascade, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(operational_overextension_cascade_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts combat power, lives, equipment, and unit cohesion from tactical units while providing political benefits to senior command. The extraction is substantial but not maximal — some tactical units achieve local gains, and the multi-axis strategy does prevent Ukrainian force concentration to some degree. However, the ratio of resources consumed to objectives achieved indicates significant extractive overhead. Suppression (0.72): High. Tactical commanders cannot refuse operationally impossible orders without career consequences. Information about overextension and failure is suppressed through beautiful reports feedback. Hierarchical command structure prevents bottom-up correction. Ukrainian counterattacks reveal the suppression: Russian forces are consistently surprised by attacks in sectors they reported as secure. Theater ratio (0.68): High. Multi-axis offensive operations create the appearance of initiative and strength regardless of territorial outcomes. The performance of offensive action (number of active axes, frequency of attacks, claims of advances) substitutes for the achievement of operational objectives (territorial consolidation, breakthrough, enemy force destruction). The theater has increased over time as the gap between claimed and achieved objectives has widened.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates extreme perspectival divergence. Frontline tactical units experience pure extraction (snare) — ordered to achieve impossible objectives with insufficient resources, no exit, no coordination benefit visible at their level. Senior command and political leadership experience pure coordination (rope) — the multi-axis strategy coordinates political messaging, demonstrates initiative, and provides career benefits regardless of tactical outcomes. Logistics personnel experience mixed coordination and extraction (tangled rope) — genuine supply coordination problems layered with extractive blame assignment. Ukrainian forces experience the constraint as both opportunity and burden (tangled rope) — Russian overextension creates targeting opportunities but requires sustained defensive readiness. The analytical observer identifies the cascade as organizational pathology (snare at system level) — a self-reinforcing cycle sustained by information suppression rather than coordination logic. The gap between the beneficiary's rope and the victim's snare is the diagnostic signature: what appears as coordination from above is experienced as pure extraction from below.
 *
 * DIRECTIONALITY LOGIC:
 *   Frontline tactical units are victims with trapped exit options — they experience maximum extraction because they bear the operational costs of overextension with no ability to refuse orders or consolidate gains. Frontline commanders are victims with constrained exit options — they have some tactical agency but cannot refuse strategic overextension without career consequences. Logistics personnel are organized victims with constrained exit — they coordinate complex operations but bear extractive blame for failures caused by impossible requirements. Senior command and political leadership are institutional beneficiaries with arbitrage exit — they capture political credit while bearing minimal operational costs and can modify strategy but choose not to because political benefits outweigh tactical costs borne by subordinates. Ukrainian forces are powerful external actors with mobile exit — they experience mixed coordination benefit (Russian overextension creates opportunities) and extraction cost (sustained defensive tempo). The analytical observer identifies the system-level pathology: a self-reinforcing cascade where information suppression prevents recognition of overextension.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by demonstrating that the same structural phenomenon — simultaneous operations across six axes without sufficient force density — is genuinely coordination from the political leadership's perspective (coordinates messaging, demonstrates initiative) and genuinely extraction from the tactical unit's perspective (consumes combat power without achieving objectives). The classification is not 'which type is correct?' but 'which structural position are you measuring from?' The snare classification at the analytical level is not a contradiction of the rope classification at the institutional level — it is the recognition that what coordinates political theater extracts operational capacity. The beautiful reports feedback loop (upstream constraint) sustains the cascade by preventing information about overextension from reaching decision-makers who could consolidate forces. The mandatrophy is resolved by recognizing that coordination at one level (political messaging) can be extraction at another level (tactical operations) when information suppression prevents feedback correction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    force_density_threshold,
    'What minimum force density (troops per kilometer of front) is required to achieve breakthrough vs. mere contact in modern mechanized warfare?',
    'Historical analysis of successful vs. failed offensive operations; correlation between force density, artillery support density, and territorial gains; comparison with Ukrainian force density in successful counteroffensives (Kharkiv, Kherson)',
    'If threshold is 5:1 attacker advantage: current Russian operations are tactically rational but under-resourced. If threshold is 3:1: operations are fundamentally irrational, indicating organizational pathology rather than resource constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(force_density_threshold, empirical, 'Minimum force density required for breakthrough operations').

omega_variable(
    command_information_accuracy,
    'What percentage of ground-truth tactical failures are accurately reported up the command chain vs. concealed or misrepresented in beautiful reports?',
    'Comparison of Russian military communications (intercepted) with official reports; analysis of claimed vs. confirmed territorial gains; correlation between reported success and subsequent force redeployment patterns',
    'If >70% accurate: overextension is strategic choice with full information. If <30% accurate: overextension is sustained by information suppression, confirming snare classification from analytical perspective.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(command_information_accuracy, empirical, 'Accuracy of tactical reporting up command chain').

omega_variable(
    political_vs_military_objective_primacy,
    'Are multi-axis operations driven by military logic (preventing Ukrainian force concentration) or political logic (maintaining appearance of initiative)?',
    'Analysis of operational timing relative to political events (domestic propaganda cycles, international summits, leadership speeches); comparison of force allocation with militarily rational concentration points; assessment of whether operations continue after achieving stated military objectives',
    'If military primacy: constraint is tangled rope (genuine coordination with extraction costs). If political primacy: constraint is snare (extraction sustained by political theater).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_vs_military_objective_primacy, conceptual, 'Whether operations serve military or political objectives').

omega_variable(
    cascade_reversibility,
    'Can the overextension cascade be reversed by operational pause and force consolidation, or has it created irreversible structural damage (unit cohesion loss, logistics collapse, command trust breakdown)?',
    'Historical analysis of military organizations recovering from overextension; assessment of Russian force reconstitution capacity; measurement of unit cohesion indicators (desertion rates, surrender rates, combat effectiveness) over time',
    'If reversible: constraint is temporary snare with exit path. If irreversible: constraint has caused permanent organizational degradation, confirming high long-term extractiveness.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cascade_reversibility, empirical, 'Whether overextension cascade can be operationally reversed').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(operational_overextension_cascade, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(opext_theater_initial, operational_overextension_cascade, theater_ratio, 0, 0.45).
narrative_ontology:measurement(opext_theater_early, operational_overextension_cascade, theater_ratio, 3, 0.55).
narrative_ontology:measurement(opext_theater_mid, operational_overextension_cascade, theater_ratio, 6, 0.62).
narrative_ontology:measurement(opext_theater_late, operational_overextension_cascade, theater_ratio, 9, 0.68).

% Extraction over time
narrative_ontology:measurement(opext_extract_initial, operational_overextension_cascade, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(opext_extract_early, operational_overextension_cascade, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(opext_extract_mid, operational_overextension_cascade, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(opext_extract_late, operational_overextension_cascade, base_extractiveness, 9, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(operational_overextension_cascade, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is downstream of beautiful_reports_feedback_loop. The upstream constraint (information suppression in hierarchical command structures) enables the downstream constraint (operational overextension) by preventing recognition of force dispersion and tactical failures. The two constraints have different extractiveness values reflecting different structural mechanisms: beautiful_reports extracts information accuracy and command trust; operational_overextension extracts combat power and unit cohesion. They form a cascade: information suppression → continued overextension → operational failures → more information suppression.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
