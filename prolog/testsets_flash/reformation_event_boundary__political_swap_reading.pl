% ============================================================================
% CONSTRAINT STORY: reformation_event_boundary__political_swap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_event_boundary__political_swap_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: reformation_event_boundary__political_swap_reading
 *   human_readable: Reformation as Political Realignment (Political Swap Reading)
 *   domain: historical_epistemology/religious_history/commitment_system_analysis
 *
 * SUMMARY:
 *   This constraint story presents the 'political swap' reading of the
 *   Reformation, arguing that it was primarily a political realignment where
 *   secular rulers exploited theological disputes to break papal authority
 *   and seize church assets. Theology, in this reading, served as a post-hoc
 *   rationalization for power consolidation. The periodization extends to the
 *   Peace of Westphalia (1648), which solidified the political settlement.
 *   This is one reading of the 'reformation_event_boundary' kernel.
 *
 * KEY AGENTS:
 *   - secular_rulers: Primary beneficiaries and agenda-setters (institutional/arbitrage)
 *   - catholic_church: Primary victim of asset seizure and authority erosion (institutional/trapped)
 *   - papal_authority: Direct target of political challenge (institutional/trapped)
 *   - theologians_and_reformers: Instrumentalized beneficiaries (moderate/constrained)
 *   - common_people: Diffuse payers, caught in conflicts (powerless/trapped)
 *   - historians_of_political_economy: Analytical observers (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_event_boundary__political_swap_reading, 0.85).
domain_priors:suppression_score(reformation_event_boundary__political_swap_reading, 0.75).
domain_priors:theater_ratio(reformation_event_boundary__political_swap_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_event_boundary__political_swap_reading, snare).
narrative_ontology:human_readable(reformation_event_boundary__political_swap_reading, "Reformation as Political Realignment (Political Swap Reading)").
narrative_ontology:topic_domain(reformation_event_boundary__political_swap_reading, "historical_epistemology/religious_history/commitment_system_analysis").

domain_priors:requires_active_enforcement(reformation_event_boundary__political_swap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_event_boundary__political_swap_reading, 'b426677b-c8b1-4fb4-a546-6f3b0e9a934d').
narrative_ontology:cs_kernel_codification('b426677b-c8b1-4fb4-a546-6f3b0e9a934d', distributed).
narrative_ontology:cs_authority_grounding('b426677b-c8b1-4fb4-a546-6f3b0e9a934d', extraction).
narrative_ontology:cs_interpretation_layer_present('b426677b-c8b1-4fb4-a546-6f3b0e9a934d').
narrative_ontology:cs_reading_relation('b426677b-c8b1-4fb4-a546-6f3b0e9a934d', reformation_event_boundary__theological_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('b426677b-c8b1-4fb4-a546-6f3b0e9a934d', reformation_event_boundary__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('b426677b-c8b1-4fb4-a546-6f3b0e9a934d', foundational, theology_as_instrument_of_power).
narrative_ontology:cs_axiom_status(theology_as_instrument_of_power, holdable).
narrative_ontology:cs_axiom_grounding('b426677b-c8b1-4fb4-a546-6f3b0e9a934d', theology_as_instrument_of_power, empirically_contingent).
narrative_ontology:cs_axiom('b426677b-c8b1-4fb4-a546-6f3b0e9a934d', foundational, state_sovereignty_over_ecclesiastical_authority).
narrative_ontology:cs_axiom_status(state_sovereignty_over_ecclesiastical_authority, holdable).
narrative_ontology:cs_axiom_grounding('b426677b-c8b1-4fb4-a546-6f3b0e9a934d', state_sovereignty_over_ecclesiastical_authority, conventional).
narrative_ontology:cs_reference_frame('b426677b-c8b1-4fb4-a546-6f3b0e9a934d', secular_state_consolidation).
narrative_ontology:cs_drift_state('b426677b-c8b1-4fb4-a546-6f3b0e9a934d', contemporary_historical_scholarship, gap(stable, minor, true)).
narrative_ontology:cs_created_at('b426677b-c8b1-4fb4-a546-6f3b0e9a934d', '').
narrative_ontology:cs_kernel_id(reformation_event_boundary__political_swap_reading, reformation_event_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_event_boundary__political_swap_reading, secular_rulers).
narrative_ontology:constraint_victim(reformation_event_boundary__political_swap_reading, catholic_church).
narrative_ontology:constraint_victim(reformation_event_boundary__political_swap_reading, papal_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__political_swap_reading, theologians_and_reformers).
narrative_ontology:constraint_victim(reformation_event_boundary__political_swap_reading, common_people).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exploited theological disputes to break papal authority, seize church assets, and consolidate their own power. They were the primary beneficiaries of the political and economic transfers, using theological arguments as justification.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, secular_rulers, agenda_setter,
    institutional, generational, arbitrage, regional).

% Suffered significant loss of temporal power, land, and revenue as secular rulers asserted control. Its authority was directly challenged and diminished in many regions, leading to a fragmentation of its former universal jurisdiction.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, catholic_church, payer,
    institutional, civilizational, trapped, global).

% The direct target of the political realignment, experiencing a severe reduction in its temporal and spiritual jurisdiction over large parts of Europe. Its claims to universal sovereignty were repudiated by emerging nation-states.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, papal_authority, payer,
    institutional, civilizational, trapped, global).

% Provided the intellectual and doctrinal framework that secular rulers leveraged. While some genuinely sought theological reform, their ideas were instrumentalized, and their agency was often secondary to the political objectives of their patrons. They benefited from patronage but were constrained by political agendas.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, theologians_and_reformers, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(reformation_event_boundary__political_swap_reading, theologians_and_reformers, excluded).

% Were often caught in the conflicts, suffering from wars, religious persecution, and the imposition of new authorities. Their theological convictions were often secondary to the political and military outcomes that shaped their lives.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, common_people, payer,
    powerless, immediate, trapped, local).

% Analyze the Reformation through the lens of power dynamics, resource transfers, and state-building, often emphasizing the material and political drivers over purely theological ones. They seek to identify the underlying structural shifts.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, historians_of_political_economy, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enabled secular rulers to coordinate their efforts to assert sovereignty over ecclesiastical institutions and resources within their territories, thereby consolidating state power and reducing external (papal) interference.
% TRANSFER_FUNCTION: Transferred vast landholdings, tithes, and other forms of wealth from the Catholic Church to secular rulers, along with the authority to appoint clergy and define religious practice within their domains.
% ABSENT_VOICES: Theological purists who genuinely sought only spiritual reform, and those who believed in the indivisible spiritual and temporal authority of the Papacy, were marginalized or suppressed when their aims diverged from the political agenda. Their voices were drowned out by the clamor for political and economic gain.
% DISAPPEARANCE_RATIONALE: If the political realignment aspect of the Reformation vanished, the modern nation-state system would be fundamentally different, with a much stronger, unified, and trans-national ecclesiastical authority. The distribution of wealth and power in early modern Europe would be unrecognizable, and the trajectory of secularization would have been profoundly altered.
% FOUNDING_PROBLEM: The problem was the perceived dual sovereignty of secular rulers and the Papacy, leading to conflicts over jurisdiction, taxation, and appointments, which hindered the consolidation of centralized state power.
% FOUNDING_PROBLEM_CORROBORATION: Historians of political economy and state formation corroborate that the problem of dual sovereignty was a central driver for secular rulers. The problem is 'dead' because the political settlement of Westphalia (1648) largely resolved the issue in favor of state sovereignty, making the original problem obsolete, even if its theological justifications persist.
narrative_ontology:disappearance_verdict(reformation_event_boundary__political_swap_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_event_boundary__political_swap_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_event_boundary__political_swap_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(reformation_event_boundary__political_swap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_event_boundary__political_swap_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_event_boundary__political_swap_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reformation_event_boundary__political_swap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reformation_event_boundary__political_swap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the transfer of wealth and authority from the Church to secular rulers was substantial and asymmetric. Suppression (0.75) was necessary to overcome resistance from the Papacy and loyal Catholic factions, often involving military force and legal coercion. The theater ratio (0.6) reflects that while theological arguments were present, their primary function, in this reading, was to legitimize political and economic objectives, rather than being the sole or primary driver. The initial lower values for extractiveness and suppression reflect the nascent stages of the conflict, which intensified as secular rulers gained momentum and faced greater resistance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of secular rulers, the constraint was a necessary assertion of sovereignty, a 'rope' to coordinate state-building. From the perspective of the Catholic Church and Papal Authority, it was a 'snare' of immense extraction and suppression. The engine's classification will reflect this divergence based on the declared roles and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   Secular rulers are full beneficiaries (d=0.0) as they initiated and profited from the realignment. The Catholic Church and Papal Authority are full targets (d=1.0) as they bore the brunt of the asset seizure and authority erosion. Theologians and reformers are partial beneficiaries (d=0.3) as their ideas were promoted, but their agency was often co-opted. Common people are targets (d=0.8) as they suffered the consequences without significant agency.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading frames the theological justifications as a 'scaffold' for political consolidation, which became 'theatrical' (high theater_ratio) once the political objectives were largely achieved by Westphalia. The constraint's mandate (resolving dual sovereignty) was resolved, but the theological divisions persisted, becoming a 'piton' of inertial conflict in subsequent centuries, maintained by denominational identities rather than active political extraction. The high extractiveness and suppression, coupled with the 'dead' founding problem status, strongly suggest a snare-like operation, even if the initial coordination story (state-building) was present.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_agency_vs_instrumentalization,
    'To what extent were theological disputes genuine drivers of the Reformation, versus being instrumentalized by secular rulers for political gain?',
    'Detailed analysis of primary sources (e.g., private correspondence of rulers, theological treatises) to discern genuine conviction versus strategic rhetoric, and comparative studies of regions where theological reform occurred without significant political realignment.',
    'If theological agency was high, the ''theater_ratio'' would be lower, and the ''claimed_type'' might shift towards a ''tangled_rope'' or ''rope'' for the theological actors. If instrumentalization was dominant, the ''snare'' classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_agency_vs_instrumentalization, empirical, 'Ambiguity regarding the true causal weight of theology versus politics.').

omega_variable(
    reformation_periodization_boundary,
    'Is the Peace of Westphalia (1648) the appropriate endpoint for the ''political swap'' reading, or does the political realignment continue beyond this point, or conclude earlier?',
    'Historical analysis of state-church relations and international treaties post-1648 to identify continued significant transfers of authority or assets, or earlier stabilization points.',
    'A later endpoint would suggest a longer period of active extraction and suppression, potentially increasing the overall ''extractiveness'' and ''suppression'' metrics. An earlier endpoint would suggest a more rapid consolidation of power.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reformation_periodization_boundary, conceptual, 'Contestation over the temporal boundaries of the political realignment event.').

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine ''political swap'' reading of the Reformation kernel, or does it conflate elements of other readings?',
    'Cross-referencing with the ''theological_climb_reading'' and ''composite_overdetermination_reading'' to ensure distinct axioms and structural deltas are maintained. If significant overlap in core claims or structural outcomes is found, the reading may need further refinement or decomposition.',
    'If conflated, the distinctiveness of this reading is compromised, potentially leading to an inaccurate classification of the kernel''s overall dynamics. Maintaining distinct readings ensures the framework accurately models the contestation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Ensuring the integrity and distinctiveness of the ''political swap'' reading within the ''reformation_event_boundary'' kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_event_boundary__political_swap_reading, 1517, 1648).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t1517, reformation_event_boundary__political_swap_reading, theater_ratio, 1517, 0.3).
narrative_ontology:measurement(refo_tr_t1540, reformation_event_boundary__political_swap_reading, theater_ratio, 1540, 0.45).
narrative_ontology:measurement(refo_tr_t1570, reformation_event_boundary__political_swap_reading, theater_ratio, 1570, 0.55).
narrative_ontology:measurement(refo_tr_t1600, reformation_event_boundary__political_swap_reading, theater_ratio, 1600, 0.65).
narrative_ontology:measurement(refo_tr_t1620, reformation_event_boundary__political_swap_reading, theater_ratio, 1620, 0.62).
narrative_ontology:measurement(refo_tr_t1648, reformation_event_boundary__political_swap_reading, theater_ratio, 1648, 0.6).

% Extraction over time
narrative_ontology:measurement(refo_be_t1517, reformation_event_boundary__political_swap_reading, base_extractiveness, 1517, 0.6).
narrative_ontology:measurement(refo_be_t1540, reformation_event_boundary__political_swap_reading, base_extractiveness, 1540, 0.75).
narrative_ontology:measurement(refo_be_t1570, reformation_event_boundary__political_swap_reading, base_extractiveness, 1570, 0.82).
narrative_ontology:measurement(refo_be_t1600, reformation_event_boundary__political_swap_reading, base_extractiveness, 1600, 0.86).
narrative_ontology:measurement(refo_be_t1620, reformation_event_boundary__political_swap_reading, base_extractiveness, 1620, 0.88).
narrative_ontology:measurement(refo_be_t1648, reformation_event_boundary__political_swap_reading, base_extractiveness, 1648, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t1517, reformation_event_boundary__political_swap_reading, suppression_requirement, 1517, 0.5).
narrative_ontology:measurement(refo_su_t1540, reformation_event_boundary__political_swap_reading, suppression_requirement, 1540, 0.65).
narrative_ontology:measurement(refo_su_t1570, reformation_event_boundary__political_swap_reading, suppression_requirement, 1570, 0.78).
narrative_ontology:measurement(refo_su_t1600, reformation_event_boundary__political_swap_reading, suppression_requirement, 1600, 0.85).
narrative_ontology:measurement(refo_su_t1620, reformation_event_boundary__political_swap_reading, suppression_requirement, 1620, 0.8).
narrative_ontology:measurement(refo_su_t1648, reformation_event_boundary__political_swap_reading, suppression_requirement, 1648, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_event_boundary__political_swap_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(reformation_event_boundary__political_swap_reading, reformation_event_boundary__theological_climb_reading).
narrative_ontology:affects_constraint(reformation_event_boundary__political_swap_reading, reformation_event_boundary__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'reformation_event_boundary' kernel. This 'political_swap_reading' emphasizes the political and economic drivers, contrasting with the 'theological_climb_reading' (focus on doctrinal innovation) and the 'composite_overdetermination_reading' (irreducible complexity).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
