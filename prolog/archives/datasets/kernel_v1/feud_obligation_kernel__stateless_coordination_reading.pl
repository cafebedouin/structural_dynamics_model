% ============================================================================
% CONSTRAINT STORY: feud_obligation_kernel__stateless_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feud_obligation_kernel__stateless_coordination_reading, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: feud_obligation_kernel__stateless_coordination_reading
 *   human_readable: Blood-Feud Obligation as Stateless Coordination Mechanism
 *   domain: legal_anthropology/medieval_history/comparative_political_systems
 *
 * SUMMARY:
 *   Blood-feud obligation in stateless and early-state societies represents a
 *   self-enforcing coordination mechanism for justice and deterrence in
 *   contexts where centralized enforcement capacity does not exist or is not
 *   available to all groups. This constraint is ONE reading of the contested
 *   kernel 'feud_obligation_kernel' — specifically, the
 *   stateless_coordination_reading. The kernel itself remains ambiguous: does
 *   feud obligation primarily solve the coordination problem of
 *   justice-seeking (coordination reading), or does it primarily perpetuate
 *   cycles of extraction and violence (extraction-cycle reading)? Or is it
 *   primarily a pre-state practice that religions (particularly Christianity)
 *   undermined through pacification norms
 *   (christianized_pacification_reading)? This story instantiates the first
 *   reading only: feud obligation as functional coordination. The other
 *   readings are separate constraint stories with different base_properties
 *   and perspectival classifications. All three readings coexist in
 *   contemporary scholarly and cultural discourse as live analytical
 *   positions held by different disciplinary communities and cultural
 *   interpreters.
 *
 * KEY AGENTS:
 *   - Injured Kinship Group: Primary beneficiary (organized/constrained) — receives justice and deterrence; benefits from coordinated retaliation as enforcement mechanism
 *   - Kinship Obligor (individual member): Victim + beneficiary (moderate/identity_locked) — bears mandatory risk-bearing and participation costs but receives protection and justice; identity fused with kinship obligation
 *   - Feud-Cycle Perpetuation: Collective trapped agent (powerless/trapped) — the cycle itself is self-reinforcing and extracts from all parties through escalating violence and resource depletion
 *   - Stateless Authority (Chief/Elder): Beneficiary (institutional/arbitrage) — reduces direct enforcement costs; coordinates justice through feud norms; maintains social legitimacy
 *   - Wergild/Compensation System: Institutional alternative (institutional/arbitrage) — represents coordination upgrade; phasing out feud through alternative deterrence mechanism (monetary compensation)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — assesses whether feud obligation is genuine coordination (solving real justice problem) or extraction mechanism with collateral costs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feud_obligation_kernel__stateless_coordination_reading, 0.35).
domain_priors:suppression_score(feud_obligation_kernel__stateless_coordination_reading, 0.42).
domain_priors:theater_ratio(feud_obligation_kernel__stateless_coordination_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feud_obligation_kernel__stateless_coordination_reading, tangled_rope).
narrative_ontology:human_readable(feud_obligation_kernel__stateless_coordination_reading, "Blood-Feud Obligation as Stateless Coordination Mechanism").
narrative_ontology:topic_domain(feud_obligation_kernel__stateless_coordination_reading, "legal_anthropology/medieval_history/comparative_political_systems").

domain_priors:requires_active_enforcement(feud_obligation_kernel__stateless_coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feud_obligation_kernel__stateless_coordination_reading, 'c599e2cf-633b-426a-9be2-f8cbfc768d14').
narrative_ontology:cs_kernel_codification('c599e2cf-633b-426a-9be2-f8cbfc768d14', distributed).
narrative_ontology:cs_authority_grounding('c599e2cf-633b-426a-9be2-f8cbfc768d14', practice).
narrative_ontology:cs_interpretation_layer_present('c599e2cf-633b-426a-9be2-f8cbfc768d14').
narrative_ontology:cs_reading_relation('c599e2cf-633b-426a-9be2-f8cbfc768d14', feud_obligation_kernel__extraction_cycle_reading, coexists_with).
narrative_ontology:cs_reading_relation('c599e2cf-633b-426a-9be2-f8cbfc768d14', feud_obligation_kernel__christianized_pacification_reading, coexists_with).
narrative_ontology:cs_axiom('c599e2cf-633b-426a-9be2-f8cbfc768d14', foundational, justice_coordination_primary_function).
narrative_ontology:cs_axiom_status(justice_coordination_primary_function, holdable).
narrative_ontology:cs_axiom_grounding('c599e2cf-633b-426a-9be2-f8cbfc768d14', justice_coordination_primary_function, instrumental).
narrative_ontology:cs_axiom('c599e2cf-633b-426a-9be2-f8cbfc768d14', secondary, extractive_costs_secondary_to_coordination_benefits).
narrative_ontology:cs_axiom_status(extractive_costs_secondary_to_coordination_benefits, holdable).
narrative_ontology:cs_axiom_grounding('c599e2cf-633b-426a-9be2-f8cbfc768d14', extractive_costs_secondary_to_coordination_benefits, instrumental).
narrative_ontology:cs_reference_frame('c599e2cf-633b-426a-9be2-f8cbfc768d14', stateless_justice_vacuum).
narrative_ontology:cs_drift_state('c599e2cf-633b-426a-9be2-f8cbfc768d14', early_state_formation_and_centralization, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c599e2cf-633b-426a-9be2-f8cbfc768d14', '').
narrative_ontology:cs_kernel_id(feud_obligation_kernel__stateless_coordination_reading, feud_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__stateless_coordination_reading, injured_kinship_group).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__stateless_coordination_reading, would_be_targets_of_uncompensated_wrongs).
narrative_ontology:constraint_victim(feud_obligation_kernel__stateless_coordination_reading, feud_cycle_perpetuation).
narrative_ontology:constraint_victim(feud_obligation_kernel__stateless_coordination_reading, defectors_from_kinship_obligation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INJURED KINSHIP GROUP (ROPE) — Feud obligation solves the coordination problem of securing justice and deterrence when no centralized enforcement exists. The group organizes response; the threat of retaliation deters future wrongs. Low net extraction because the mechanism actually coordinates legitimate protection of group members. Suppression exists (kinship loyalty is binding) but reflects coordination costs, not pure coercion.
constraint_indexing:constraint_classification(feud_obligation_kernel__stateless_coordination_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 2: KINSHIP OBLIGOR (TANGLED ROPE) — Individual members experience feud obligation as both coordination (legitimate justice-seeking) and extraction (mandatory participation regardless of personal risk or cost). Exit is identity-locked: refusing kinship obligation means expulsion from the group, loss of protection, and loss of social identity. Structural mobility exists (one could flee) but the binding is cognitive/identity-based. Genuine coordination function (deterrence) coexists with extraction (compulsory risk-bearing).
constraint_indexing:constraint_classification(feud_obligation_kernel__stateless_coordination_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 3: FEUD-CYCLE PERPETUATION (SNARE) — The cycle itself is trapped. Retaliation provokes counter-retaliation; compensation demands trigger honor disputes; peacefully exiting the cycle means accepting injustice. No individual agent owns the cycle, but the cycle extracts continuously from all parties through escalating violence and resource depletion. Maximum suppression because the cycle is self-reinforcing through kinship obligation — breaking the obligation breaks one's identity.
constraint_indexing:constraint_classification(feud_obligation_kernel__stateless_coordination_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 4: WERGILD ALTERNATIVE (SCAFFOLD) — Wergild (monetary compensation) represents a coordination upgrade to feud: it provides restitution and deterrence without cycle escalation. From this perspective, the blood-feud obligation is a temporary coordination mechanism with a sunset: as wergild norms and compensation practices mature, the need for retaliation-based deterrence declines. Low effective extraction because the institutional actor (chief, community council) arbitrages between feud and wergild, phasing out feud as alternatives become available. The sunset is not formally decreed but structurally emergent — communities adopt wergild when it proves more efficient.
constraint_indexing:constraint_classification(feud_obligation_kernel__stateless_coordination_reading, scaffold,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: STATELESS AUTHORITY (ROPE) — Chiefs and elders benefit from feud obligation as a decentralized enforcement mechanism: it reduces their direct enforcement costs. They coordinate justice through feud norms while maintaining social legitimacy. The institutional position has arbitrage (can negotiate peace, set wergild rates, broker compensation). Feud obligation appears as functional coordination infrastructure — they are solving the genuine problem of justice in stateless contexts. Low net extraction because they are genuinely enabling coordination.
constraint_indexing:constraint_classification(feud_obligation_kernel__stateless_coordination_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational view, blood-feud obligation genuinely coordinates justice and deterrence in the absence of centralized capacity. It is not pure extraction or pure coordination but a hybrid: it solves a real coordination problem while generating secondary extraction (cycle perpetuation, cycle-constrained agents, defectors). The constraint's extractiveness reflects this hybridity. The reading instantiated here — stateless coordination — captures feud obligation as a functional justice mechanism, not as pathological violence.
constraint_indexing:constraint_classification(feud_obligation_kernel__stateless_coordination_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feud_obligation_kernel__stateless_coordination_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(feud_obligation_kernel__stateless_coordination_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(feud_obligation_kernel__stateless_coordination_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(feud_obligation_kernel__stateless_coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The reading frames feud obligation as a coordination mechanism that solves the genuine problem of justice and deterrence in stateless contexts. The extractiveness reflects secondary effects: kinship obligors bear mandatory participation costs (risk-bearing, resource expenditure), and the system perpetuates cycles that constrain all parties. However, the primary function is coordinated justice provision, not extraction. This is significantly lower than the extraction-cycle reading would estimate (which would place ε around 0.55–0.65). Suppression (0.42): Moderate. Kinship obligation is binding (individuals cannot freely choose whether to participate), but the binding is mixed: partly identity-locked (feud participation is constitutive of kinship identity) and partly structural (material costs to defection exist but are not as severe as trapped/total suppression). The suppression value reflects that alternatives exist (wergild, flight, community exile) but participation is normatively mandatory and identity-threatening. Theater ratio (0.38): Low-moderate. Feud obligation has substantial functional content — it actually deters wrongs and coordinates justice response. The theater component reflects ritual elements (honor demands, formal challenge structures, cycles of insult and response) that amplify the mechanism beyond pure deterrence function. The functional content is higher than in degraded institutional forms (piton), supporting the lower theater ratio.
 *
 * PERSPECTIVAL GAP:
 *   The key perspectival gap is between the beneficiary groups (injured kinship, stateless authority) who experience coordination and the trapped agents (feud-cycle perpetuation, cycle-constrained individuals) who experience extraction. The injured kinship group sees rope (genuine coordination of justice). The kinship obligor sees tangled rope (coordination function coexists with extraction). The stateless authority sees rope (coordination infrastructure reducing enforcement costs). The wergild alternative sees scaffold (temporary coordination mechanism being replaced by more efficient alternative). The feud-cycle perpetuation sees snare (self-reinforcing cycle with no exit). The analytical observer sees the constraint as genuinely hybrid — tangled rope — because both the coordination function and the extraction costs are structurally real. This reading differs from the extraction-cycle reading (which would lower the classification of beneficiaries toward snare, highlighting how they benefit from perpetuating cycles) and the pacification reading (which would classify feud obligation itself as a victim to be overcome).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's structural position relative to the constraint. Injured kinship groups (beneficiaries) and stateless authorities (beneficiaries/institutional) experience low or negative effective extraction — they benefit from the coordination function. Kinship obligors experience moderate extraction (mandatory participation, risk-bearing) but also benefit (protection, justice), creating the tangled_rope classification. The feud-cycle perpetuation (as an abstract entity) is trapped and powerless — it is self-reinforcing and extracts continuously. The analytical observer experiences the constraint structurally (can see the coordination function and the extraction costs) and assigns a moderate-ε tangled rope classification reflecting genuine hybridity. The readings differ in their directionality assignments: the extraction-cycle reading would assign higher d values to cycle perpetuation and kinship obligors (treating them as pure victims), while the christianized_pacification reading would assign feud obligation itself to the victim set (treating it as a harmful pre-state practice to be overcome).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exemplifies mandatrophy resolution through reading-specific classification. The question 'Is feud obligation pure coordination or pure extraction?' has no single answer independent of the reading adopted. Under the stateless-coordination reading, it is primarily tangled rope with genuine coordination function (ε=0.35). Under the extraction-cycle reading, it is snare-adjacent with dominant extraction logic (ε would be ~0.58). Under the pacification reading, it is a degraded institutional form (piton) whose function has been superseded. The mandatrophy resolves by recognizing that (a) all three readings are live analytical positions in contemporary discourse, (b) each reading instantiates a different constraint with different base_properties, and (c) the choice of reading is not empirically determined but depends on what one takes to be the primary function of the mechanism (justice provision, cycle perpetuation, or religious/moral opposition). The constraint story presented here commits to the coordination reading and assigns metrics accordingly.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    feud_cycle_exit_mechanism,
    'What structural mechanism breaks the feud cycle, and is it built into feud obligation itself or requires external institutional innovation (wergild, centralized justice)?',
    'Historical case analysis: Do feuds naturally exhaust and resolve within kinship groups, or do they require wergild/monetary compensation or state intervention to stop? Longitudinal tracking of feud durations across different institutional contexts.',
    'If cycles self-resolve: feud obligation is genuinely self-limiting (lower ε for extraction-cycle reading). If cycles require external intervention: feud obligation creates trapped perpetuation (higher ε for extraction-cycle reading). If wergild is sufficiently mature: scaffold sunset logic holds (this reading remains rope/tangled rope, but the system is transitioning).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(feud_cycle_exit_mechanism, empirical, 'Whether feud cycles have intrinsic termination conditions').

omega_variable(
    deterrence_effectiveness_vs_cycle_cost,
    'Does the deterrence function of feud obligation (preventing initial wrongs) outweigh the cycle perpetuation costs (escalating retaliation, resource depletion)?',
    'Comparative rates of violent injury in communities with active feud systems vs. communities without: do feud systems achieve lower pre-violence rates than the alternative (uncompensated wrongs + no deterrent)? Economic modeling of resource costs: total violence + cycle escalation vs. prevented first wrongs.',
    'If deterrence > cost: coordination reading confirmed (ε closer to 0.25, lower tangled_rope edge). If cost > deterrence: extraction cycle reading confirmed (ε closer to 0.55, higher snare/tangled rope edge). If approximately balanced: constraint is genuine coordination mechanism with real collateral costs (supports claimed 0.35).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_effectiveness_vs_cycle_cost, empirical, 'Net efficiency of feud deterrence relative to cycle costs').

omega_variable(
    reading_contingency_on_state_absence,
    'Is the stateless coordination reading only valid in contexts of state absence? Does centralized justice availability make feud obligation purely extractive/vestigial?',
    'Historical analysis of feud persistence in early-state and proto-state societies: do feuds continue when state justice becomes available, and if so, do they serve coordination function (filling state justice gaps) or extraction function (pre-state power structures resisting centralization)?',
    'If feuds persist as coordination in state-present contexts: reading is more general (ε holds across multiple political regimes). If feuds become purely extractive when states available: reading is specifically stateless (reading_relations imply transitions between readings as institutional context changes; this is a conceptual omega about reading identity and scope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contingency_on_state_absence, conceptual, 'Whether stateless coordination reading is regime-contingent or generalizable').

omega_variable(
    identity_lock_breakability,
    'Under what conditions does the identity_locked binding of kinship obligation break? Is the binding cognitive/identity-based (feud participants internalize obligation) or structural/coercive (genuine material barriers to exit)?',
    'Ethnographic observation: Can individuals who reject feud obligation remain in their community with reduced status but retained protection? Do defectors face material sanctions (expulsion, property loss, physical violence) or primarily status loss and social isolation? Comparative analysis across different kinship systems and historical periods.',
    'If binding is primarily identity-locked: the kinship obligor perspective''s exit should classify as identity_locked rather than constrained; this reading''s suppression reflects cognitive capture, not material coercion. If binding is primarily structural: exit should classify as trapped/constrained, and suppression reflects material barriers. If mixed: supports the claimed identity_locked classification with significant structural elements, and measurement should show suppression that persists after exit (internalized component).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_breakability, empirical, 'Binding mechanism of kinship obligation: identity-locked vs. structural coercion').

omega_variable(
    sibling_reading_prevalence_in_discourse,
    'Which reading — stateless coordination, extraction cycle, or christianized pacification — is the dominant scholarly and cultural narrative in contemporary and medieval discourse?',
    'Historiographic analysis: frequency and framing in primary sources (medieval chronicles, law codes), secondary sources (modern scholarship), and contemporary indigenous/traditional legal systems. Do actors describe feud obligation as coordination mechanism (justice, deterrence, honor) or extraction mechanism (violence, cycles, oppression)?',
    'If coordination reading dominates: the reading''s legitimacy is high; feud obligation is culturally validated as functional justice. If extraction reading dominates: coordination reading is marginalized/revisionist (requires asserting that participants were underestimating their own mechanism''s function). If readings coexist in different discourse communities: supports coexists_with relation to sibling readings (different parties hold different readings).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_prevalence_in_discourse, conceptual, 'Prevalence of stateless-coordination reading relative to extraction-cycle and pacification readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feud_obligation_kernel__stateless_coordination_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_coord_tr_t0, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(feud_coord_tr_t3, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 3, 0.37).
narrative_ontology:measurement(feud_coord_tr_t6, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 6, 0.38).

% Extraction over time
narrative_ontology:measurement(feud_coord_be_t0, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(feud_coord_be_t3, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 3, 0.32).
narrative_ontology:measurement(feud_coord_be_t6, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 6, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(feud_coord_su_t0, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(feud_coord_su_t3, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 3, 0.41).
narrative_ontology:measurement(feud_coord_su_t6, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 6, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feud_obligation_kernel__stateless_coordination_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(feud_obligation_kernel__stateless_coordination_reading, feud_obligation_kernel__extraction_cycle_reading).
narrative_ontology:affects_constraint(feud_obligation_kernel__stateless_coordination_reading, feud_obligation_kernel__christianized_pacification_reading).
narrative_ontology:affects_constraint(feud_obligation_kernel__stateless_coordination_reading, wergild_compensation_system).
narrative_ontology:affects_constraint(feud_obligation_kernel__stateless_coordination_reading, stateless_justice_absence_bottleneck).

% DUAL FORMULATION NOTE:
% Blood-feud obligation decomposes into three structurally distinct constraint stories depending on the reading adopted: (1) stateless_coordination_reading frames feud as functional justice mechanism (this story, ε=0.35, tangled rope); (2) extraction_cycle_reading frames feud as perpetuating violence and extracting resources (separate story, ε~0.58, snare-adjacent); (3) christianized_pacification_reading frames feud as pre-state practice overcome by religious norms (separate story, ε~0.40, piton). The ε values differ substantially because each reading focuses on different mechanisms: coordination function vs. cycle perpetuation vs. institutional degradation. All three readings share the same historical phenomenon but instantiate different constraints by emphasizing different structural elements. This is not observable-dependence (ε-invariance violation) but genuine reading-dependence: the constraint's identity is constituted through the reading adopted, which is a legitimate analytical choice within commitment-system frameworks.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
