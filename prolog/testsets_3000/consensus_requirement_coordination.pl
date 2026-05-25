% ============================================================================
% CONSTRAINT STORY: consensus_requirement_coordination
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_consensus_requirement_coordination, []).

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
 *   constraint_id: consensus_requirement_coordination
 *   human_readable: Consensus Requirement Coordination
 *   domain: governance/decision_making
 *
 * SUMMARY:
 *   Consensus requirement coordination is a structural constraint that
 *   emerges in multi-stakeholder governance systems, deliberative bodies, and
 *   collective decision-making contexts where formal or informal rules
 *   require supermajority agreement or unanimous consent before decisions can
 *   be executed. The constraint solves a genuine coordination problem — how
 *   to prevent tyranny of majority and protect minority interests — but the
 *   mechanism also creates extraction opportunities: actors with blocking
 *   power can extract value by withholding consent to leverage favorable
 *   side-deals or prevent decisions that harm their interests. The constraint
 *   exhibits a persistent tension between legitimate minority protection
 *   (coordination function) and extractive veto power (asymmetric
 *   extraction). The theater ratio (0.58) reflects that consensus-building
 *   processes often become performative negotiation rituals where
 *   participants signal fixed positions rather than genuinely seeking common
 *   ground. The constraint's extractiveness has increased over time (0.22 to
 *   0.38) as coalitional interests have hardened and compromise has become
 *   politically costlier, suggesting the coordination function is degrading
 *   while the extraction function persists.
 *
 * KEY AGENTS:
 *   - Excluded Constituencies: Primary victim (powerless/trapped) — face indefinite blocking of preferred decisions; bear cost of inaction while minorities capture veto value
 *   - Veto-Holding Coalition: Primary beneficiary (organized/mobile) — organized actors with exit options who benefit from blocking power; perceive constraint as legitimate coordination
 *   - Pressured Majority: Secondary actor (moderate/constrained) — caught between commitment to inclusive deliberation and frustration with blocking; face costs of non-decision
 *   - Institutional Mediator: Governance structure (institutional/arbitrage) — manages consensus rules; perceives them as temporary tools with potential sunsetting as alternatives emerge
 *   - Ceremonial Deliberation: Ritualized process (institutional/arbitrage) — consensus discussions become signal-sending rather than problem-solving; theater persists through institutional inertia
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing consensus requirement as immutable law of group choice rather than contingent design
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(consensus_requirement_coordination, 0.38).
domain_priors:suppression_score(consensus_requirement_coordination, 0.42).
domain_priors:theater_ratio(consensus_requirement_coordination, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(consensus_requirement_coordination, extractiveness, 0.38).
narrative_ontology:constraint_metric(consensus_requirement_coordination, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(consensus_requirement_coordination, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(consensus_requirement_coordination, tangled_rope).
narrative_ontology:human_readable(consensus_requirement_coordination, "Consensus Requirement Coordination").
narrative_ontology:topic_domain(consensus_requirement_coordination, "governance/decision_making").

domain_priors:requires_active_enforcement(consensus_requirement_coordination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(consensus_requirement_coordination, blocking_minorities).
narrative_ontology:constraint_beneficiary(consensus_requirement_coordination, veto_holding_coalitions).
narrative_ontology:constraint_victim(consensus_requirement_coordination, excluded_constituencies).
narrative_ontology:constraint_victim(consensus_requirement_coordination, time_sensitive_decisions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED CONSTITUENCY (SNARE) — Trapped by consensus requirement with no exit mechanism. Majority preferences are suppressed by minority veto power; majority bears cost of inaction while minority captures extraction value of blocking unfavorable decisions. Maximal experienced extraction.
constraint_indexing:constraint_classification(consensus_requirement_coordination, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: VETO-HOLDING COALITION (ROPE) — Organized agents with exit options (can exit the consensus structure or form alternative coalitions) perceive consensus requirement as pure coordination mechanism: solves the collective action problem of ensuring minority voice in decisions. Benefits from veto power; experiences constraint as genuine collaboration with limited extraction.
constraint_indexing:constraint_classification(consensus_requirement_coordination, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: PRESSURED MAJORITY (TANGLED ROPE) — Constrained by exit costs (dissolution of coalition, loss of organizational structure) but not trapped. Experiences both genuine coordination function (minority protection prevents tyranny) and asymmetric extraction (blocked from executing majority preference). Mixed experience of both benefit and cost.
constraint_indexing:constraint_classification(consensus_requirement_coordination, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: INSTITUTIONAL MEDIATOR (SCAFFOLD) — Institutions (courts, arbitration bodies, supermajority thresholds) that manage consensus requirements see them as temporary coordination tools with built-in sunsetting mechanisms. Supermajority requirements can be modified; consensus rules can be replaced; mediators have exit options and perceive declining necessity as constituencies grow more ideologically aligned or as technological alternatives emerge.
constraint_indexing:constraint_classification(consensus_requirement_coordination, scaffold,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CEREMONIAL DELIBERATION (PITON) — Consensus-seeking discussions often become performative rituals where participants present fixed positions rather than genuinely negotiating. Theater ratio (0.58) reflects that much consensus-building discussion is signal-sending rather than problem-solving. Procedures persist through institutional inertia despite recognized dysfunction.
constraint_indexing:constraint_classification(consensus_requirement_coordination, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational perspective, consensus requirement appears as an immutable structural law: any collective decision system must either enforce majority rule (generating oppressed minorities) or require consensus (blocking decisions). This appears as an irreducible tradeoff built into logic of group choice itself. However, structural data contradicts mountain classification — the constraint's extractiveness (0.38) and suppression (0.42) are contingent institutional choices, not natural limits. Engine detects false summit: naturalizes a design choice as a law of logic.
constraint_indexing:constraint_classification(consensus_requirement_coordination, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(consensus_requirement_coordination_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(consensus_requirement_coordination, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(consensus_requirement_coordination, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(consensus_requirement_coordination, TR),
    TR >= 0.70.

:- end_tests(consensus_requirement_coordination_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The consensus requirement creates genuine blocking power for minorities. Over the measurement interval, extractiveness increased from 0.22 to 0.38 as coalitional interests hardened and compromise became costlier — suggesting the coordination function (preventing tyranny) is being increasingly displaced by extraction function (leveraging veto for side-benefits). The moderate value reflects that extraction is real but not total; alternatives exist and some coordination value persists. Suppression (0.42): Moderate. Blocking minorities are not eliminated but suppressed in the sense that majority preferences are suspended indefinitely. Exit costs are significant (dissolution of coalition, loss of organizational membership) but not insurmountable — some actors can walk away at the cost of exiting the system. Theater ratio (0.58): Elevated. Consensus-seeking processes show increasing performativity: participants present fixed positions, deliberation becomes signal-sending, genuine negotiation declines. Theater ratio increased from 0.35 to 0.58 over the interval, suggesting the deliberative process is increasingly about impression management rather than problem-solving.
 *
 * PERSPECTIVAL GAP:
 *   The original research group (veto-holder) genuinely experiences consensus requirement as coordination — they see the mechanism as preventing hasty decisions and ensuring minority voice is heard. Their snapshot of consensus-seeking includes legitimate collaborative problem-solving. The excluded constituency experiences the same mechanism as pure extraction — they see their preferences indefinitely suspended by blocking minorities who leverage veto power for side-benefits. The pressured majority sees both: they value minority protection in principle but experience blocking as increasingly extractive in practice. The institutional mediator sees the consensus mechanism as a temporary scaffold — supermajority thresholds can be calibrated, consensus rules can be replaced, and as constituencies grow more aligned or as technological alternatives (delegated voting, liquid democracy) mature, the necessity of consensus diminishes. The ceremonial deliberation process (piton) shows the constraint's degradation: as parties prepare fixed positions before entering consensus talks, the deliberative function atrophies while the veto mechanism persists.
 *
 * DIRECTIONALITY LOGIC:
 *   Veto-holding coalitions derive low directionality (d ≈ 0.25-0.35) because they are beneficiaries with exit options (organized/mobile). They can exit the consensus system or form alternative coalitions, giving them agency and reducing experienced extraction. The analytical formula derives d from beneficiary status (pushes d down toward 0.0) and mobile/arbitrage exit options (further reduces d). Excluded constituencies derive high directionality (d ≈ 0.85-0.95) because they are victims with no exit (trapped). The formula derives d from victim status (pushes d toward 1.0) and trapped exit options (keeps d high). Pressured majority derives moderate directionality (d ≈ 0.50-0.60) because they are partly victims (blocked from action) with constrained but not impossible exit (can exit at organizational cost but organizational value is real). This differentiation explains why the same constraint classifies as Rope from the veto-holder perspective, Snare from the excluded perspective, and Tangled Rope from the moderate perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The constraint's classification across perspectives reveals how consensus requirement coordination manages the mandatrophy — the tension between coordination and extraction — by admitting both legitimate and extractive functions simultaneously. The veto-holder's Rope classification is genuine: blocking power does solve the tyranny problem. The excluded constituency's Snare classification is equally genuine: their preferences are indefinitely suspended by minority veto. The tangled rope classification from the pressured majority is the most analytically informative: it captures the constraint as doing both things at once — genuine minority protection AND asymmetric extraction. The increasing theater ratio (0.35 to 0.58) and extractiveness (0.22 to 0.38) indicate the coordination function is degrading over time while the extraction function persists, suggesting the mechanism is drifting toward snare from its original tangled_rope state. This drift pattern is diagnostic: if a constraint starts as legitimate tangled rope but shows increasing theater and extractiveness while suppression stays constant, the coordination rationale is being progressively replaced by extraction justification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consensus_definition_ambiguity,
    'What constitutes ''consensus'' — unanimity, supermajority threshold, or authentic agreement?',
    'Historical analysis of consensus-rule decision systems; comparison of formal requirements vs actual practice; measurement of decision velocity under different threshold definitions',
    'If unanimity required: extractiveness rises sharply (blocking power maximized). If supermajority (66-75%): extractiveness moderate. If authentic agreement preferred but not required: extractiveness drops (flexibility increases).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consensus_definition_ambiguity, conceptual, 'Definition of consensus threshold').

omega_variable(
    minority_protection_necessity,
    'How much blocking power is structurally necessary to prevent tyranny of majority vs how much is supererogatory extraction?',
    'Comparative analysis of outcomes in simple majority vs consensus-rule systems; measurement of minority welfare in each; identification of threshold where additional blocking power creates no additional protection',
    'If low threshold necessary: most blocking power is extraction (Snare dominates). If high threshold necessary: blocking power is legitimate coordination cost (Rope/Tangled Rope dominates).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_protection_necessity, empirical, 'Threshold of minority protection necessity').

omega_variable(
    exit_cost_measurement,
    'Are exit costs from consensus systems structural (legal/organizational barriers) or internalized (identity/ideology commitment)?',
    'Study of actors who exit consensus-rule systems; measurement of defection costs; analysis of whether cost persists after agent removes themselves from system',
    'If structural: exit_options accurate (trapped vs constrained distinctions valid). If internalized: suppression measurement understated (agents carry constraints with them post-exit); identity_locked exit option may be more accurate for some agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_cost_measurement, empirical, 'Composition of exit costs (structural vs internalized)').

omega_variable(
    alternative_coordination_mechanisms,
    'Do ranked-choice voting, liquid democracy, or delegated decision-making reduce consensus requirement extraction without sacrificing minority protection?',
    'Comparison of outcomes in alternative voting systems vs consensus-rule systems; measurement of decision speed, minority satisfaction, and veto power concentration',
    'If alternatives effective: scaffold classification confirmed (consensus requirement has sunset path). If alternatives fail: consensus requirement is structurally necessary (shifts toward mountain/rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_coordination_mechanisms, empirical, 'Efficacy of alternative coordination mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(consensus_requirement_coordination, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, consensus_requirement_coordination, theater_ratio, 0, 0.35).
narrative_ontology:measurement(cons_tr_t3, consensus_requirement_coordination, theater_ratio, 3, 0.48).
narrative_ontology:measurement(cons_tr_t6, consensus_requirement_coordination, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, consensus_requirement_coordination, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(cons_be_t3, consensus_requirement_coordination, base_extractiveness, 3, 0.3).
narrative_ontology:measurement(cons_be_t6, consensus_requirement_coordination, base_extractiveness, 6, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(consensus_requirement_coordination, enforcement_mechanism).
narrative_ontology:affects_constraint(consensus_requirement_coordination, supermajority_threshold_lock).
narrative_ontology:affects_constraint(consensus_requirement_coordination, veto_player_dynamics).
narrative_ontology:affects_constraint(consensus_requirement_coordination, decision_speed_constraint).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(consensus_requirement_coordination, organized, 0.3).
constraint_indexing:directionality_override(consensus_requirement_coordination, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
