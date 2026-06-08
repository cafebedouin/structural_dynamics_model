% ============================================================================
% CONSTRAINT STORY: husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_husk_reading, []).

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
 *   constraint_id: husk_reading
 *   human_readable: Preparedness Husk: Drills and Inspections as Memorial Performance
 *   domain: disaster_preparedness/institutional_memory/commitment_systems
 *
 * SUMMARY:
 *   This constraint instantiates the 'husk reading' of the
 *   preparedness_persistence kernel: the claim that disaster preparedness
 *   drills and inspections persist as memorial performance while operational
 *   competence atrophies. The husk reading asserts that the original mandate
 *   for preparedness — institutional learning from past disasters — has
 *   become obsolete, but the ritual persists through institutional inertia
 *   and because it serves the institutional legitimacy apparatus. Drills and
 *   inspections create the appearance of readiness without delivering actual
 *   response capacity. The constraint exhibits high theater_ratio (0.81)
 *   because the performative content has grown over time: agencies conduct
 *   drills to satisfy regulatory requirements and public expectations, but
 *   the drills do not translate into maintained competence. Staff turnover,
 *   budget cuts, and the political need to appear prepared without investing
 *   in actual competence maintenance have created a situation where the
 *   ritual persists while the underlying capacity degrades. The husk reading
 *   contrasts with the competence reading (which asserts that preparedness
 *   remains a live mandate and that drills serve genuine coordination) and
 *   the hybrid reading (which asserts that both readings coexist in different
 *   institutional contexts). This story models only the husk reading:
 *   preparedness as atrophied performance.
 *
 * KEY AGENTS:
 *   - Population at Flood Risk: Primary victim (powerless/trapped) — depends on preparedness that exists only as performance; bears full cost of atrophied competence during actual disasters
 *   - Emergency Management Agency: Primary institutional actor (institutional/constrained) — maintains the drill and inspection ritual because the mandate requires it and because the ritual sustains legitimacy; knows the drills are largely performative
 *   - Municipal Government: Secondary beneficiary (institutional/arbitrage) — benefits from the appearance of preparedness without bearing the cost of actual competence maintenance; experiences the constraint as functional coordination
 *   - Disaster Preparedness Advocacy Coalition: Organized agents (organized/constrained) — see the constraint as both coordination and extraction; have agency to push for competence-based metrics but face institutional resistance
 *   - Institutional Legitimacy Apparatus: Beneficiary (non-agent) — the abstract institutional good that benefits from the appearance of preparedness; collects legitimacy rents from the drill ritual
 *   - Operational Readiness: Victim (non-agent) — the abstract institutional good that degrades as competence atrophies; bears the cost of theater-over-competence allocation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(husk_reading, 0.62).
domain_priors:suppression_score(husk_reading, 0.48).
domain_priors:theater_ratio(husk_reading, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(husk_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(husk_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(husk_reading, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(husk_reading, piton).
narrative_ontology:human_readable(husk_reading, "Preparedness Husk: Drills and Inspections as Memorial Performance").
narrative_ontology:topic_domain(husk_reading, "disaster_preparedness/institutional_memory/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(husk_reading, 'c77384f3-aa75-4adc-83ab-b9e19fe8af45').
narrative_ontology:cs_kernel_codification('c77384f3-aa75-4adc-83ab-b9e19fe8af45', formalized).
narrative_ontology:cs_authority_grounding('c77384f3-aa75-4adc-83ab-b9e19fe8af45', extraction).
narrative_ontology:cs_interpretation_layer_present('c77384f3-aa75-4adc-83ab-b9e19fe8af45').
narrative_ontology:cs_reading_relation('c77384f3-aa75-4adc-83ab-b9e19fe8af45', husk_reading__competence_reading, forecloses).
narrative_ontology:cs_reading_relation('c77384f3-aa75-4adc-83ab-b9e19fe8af45', husk_reading__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('c77384f3-aa75-4adc-83ab-b9e19fe8af45', foundational, preparedness_mandate_obsolete).
narrative_ontology:cs_axiom_status(preparedness_mandate_obsolete, holdable).
narrative_ontology:cs_axiom_grounding('c77384f3-aa75-4adc-83ab-b9e19fe8af45', preparedness_mandate_obsolete, empirically_contingent).
narrative_ontology:cs_axiom('c77384f3-aa75-4adc-83ab-b9e19fe8af45', foundational, institutional_legitimacy_primary_beneficiary).
narrative_ontology:cs_axiom_status(institutional_legitimacy_primary_beneficiary, holdable).
narrative_ontology:cs_axiom_grounding('c77384f3-aa75-4adc-83ab-b9e19fe8af45', institutional_legitimacy_primary_beneficiary, empirically_contingent).
narrative_ontology:cs_reference_frame('c77384f3-aa75-4adc-83ab-b9e19fe8af45', preparedness_as_competence).
narrative_ontology:cs_drift_state('c77384f3-aa75-4adc-83ab-b9e19fe8af45', contemporary, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c77384f3-aa75-4adc-83ab-b9e19fe8af45', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(husk_reading, preparedness_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(husk_reading, institutional_legitimacy_apparatus).
narrative_ontology:constraint_victim(husk_reading, population_at_flood_risk).
narrative_ontology:constraint_victim(husk_reading, operational_readiness).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FLOOD-EXPOSED RESIDENT (SNARE) — Trapped in geographic location; cannot exit the constraint. Depends on preparedness that exists only as performance. Drills and inspections create false confidence in protection that does not materialize during actual flood. Maximum extraction: the resident bears the cost of atrophied competence while institutional theater claims readiness.
constraint_indexing:constraint_classification(husk_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: EMERGENCY MANAGEMENT AGENCY (PITON) — Maintains the drill and inspection ritual because the mandate requires it and because the ritual sustains institutional legitimacy. The agency knows the drills are largely performative — actual competence has atrophied — but the theater persists through institutional inertia. Constrained by budget cuts, staff turnover, and the political need to appear prepared. The constraint is degraded function maintained as performance.
constraint_indexing:constraint_classification(husk_reading, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: MUNICIPAL GOVERNMENT (ROPE) — Benefits from the appearance of preparedness without bearing the cost of actual competence maintenance. Drills and inspections satisfy regulatory requirements and public expectations. The constraint solves a genuine coordination problem: how to signal readiness to constituents and state authorities. The municipality experiences the constraint as functional coordination, not extraction.
constraint_indexing:constraint_classification(husk_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: DISASTER PREPAREDNESS ADVOCACY COALITION (TANGLED ROPE) — Organized agents (flood survivors, emergency responders, public health advocates) see the constraint as both coordination and extraction. The drill-and-inspection regime coordinates public expectations and regulatory compliance (genuine function) while simultaneously extracting legitimacy from the appearance of readiness without delivering actual competence. The coalition has agency and can push for competence-based metrics, but faces institutional resistance.
constraint_indexing:constraint_classification(husk_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, institutional memory decay is an immutable feature of human organizations: competence atrophies when not continuously exercised, and the gap between ritual and reality is inherent to bureaucratic systems. This perspective risks naturalizing what is actually a contingent institutional choice: the decision to maintain theater rather than invest in actual competence. The engine will identify this as a false summit.
constraint_indexing:constraint_classification(husk_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(husk_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(husk_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(husk_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(husk_reading, TR),
    TR >= 0.70.

:- end_tests(husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): Moderate-high. The constraint extracts institutional legitimacy from the appearance of preparedness without delivering actual competence. The extraction is not total because some genuine coordination occurs (agencies do communicate, some competence is maintained), but the primary flow is toward institutional legitimacy and away from actual readiness. The measurement trajectory shows rising extractiveness over the interval (0.45 → 0.62) as budget pressures and staff turnover have increased the gap between theater and competence. Suppression (0.48): Moderate. The constraint suppresses alternatives through regulatory requirements (agencies must conduct drills), political expectations (elected officials cite drill participation), and the difficulty of measuring actual competence (it is easier to count drills than to assess response capacity). But suppression is not total — advocacy coalitions can push for competence-based metrics, and some agencies do invest in actual training. Theater ratio (0.81): High and rising. The constraint is substantially performative. Drills are conducted to satisfy regulatory requirements and public expectations, not to maintain competence. The theater has increased over the interval as the gap between ritual and reality has widened. Agencies conduct more drills with less actual competence maintenance, creating a situation where the ritual persists while the underlying capacity degrades.
 *
 * PERSPECTIVAL GAP:
 *   The husk reading produces a perspectival gap between the institutional actors (who see coordination and legitimacy) and the powerless agents (who see extraction and false confidence). The municipal government sees rope (functional coordination), while the flood-exposed resident sees snare (pure extraction). The emergency management agency sees piton (atrophied function maintained as performance), while the advocacy coalition sees tangled rope (mixed coordination and extraction). The analytical observer risks seeing mountain (institutional memory decay as natural law), but the structural data reveals this as a false summit: the decision to maintain theater rather than invest in competence is contingent, not inevitable.
 *
 * DIRECTIONALITY LOGIC:
 *   The husk reading's directionality derives from the beneficiary/victim structure and the institutional actors' exit options. The institutional legitimacy apparatus is the primary beneficiary (d ≈ 0.0 — full beneficiary), experiencing negative effective extraction (the constraint subsidizes legitimacy). The population at flood risk is the primary victim (d ≈ 1.0 — full target), experiencing maximum effective extraction (they bear the cost of atrophied competence). The emergency management agency is constrained (d ≈ 0.55) — they benefit from the appearance of preparedness but are also trapped by the mandate to conduct drills and the political need to appear prepared. The municipal government has arbitrage options (d ≈ 0.2) — they can exit by investing in actual competence, but the political cost is high. The advocacy coalition is organized (d ≈ 0.65) — they are partially victimized by the constraint but have agency to push for change.
 *
 * MANDATROPHY ANALYSIS:
 *   HUSK READING MANDATROPHY: The husk reading asserts that the original mandate for preparedness drills — institutional learning from past disasters — has become obsolete. The constraint persists because: (1) regulatory requirements mandate drills, (2) political actors cite drill participation as evidence of preparedness, (3) the ritual is easier to maintain than to replace, and (4) institutional legitimacy benefits from the appearance of preparedness. The mandatrophy is resolved by recognizing that the constraint is piton (atrophied function maintained as performance), not rope (pure coordination) or mountain (natural law). The husk reading's classification prevents mislabeling the constraint as coordination when it is actually extraction of institutional legitimacy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    husk_vs_competence_reading_distinction,
    'Is the constraint fundamentally about atrophied competence maintained as performance (husk reading), or about genuine coordination challenges in maintaining preparedness (competence reading)?',
    'Post-disaster analysis: do actual response failures correlate with drill performance metrics? Do agencies that invest in competence over theater show better outcomes? Longitudinal competence tracking vs. theater metrics.',
    'If husk reading is correct: piton classification confirmed, high theater_ratio, beneficiary is institutional legitimacy. If competence reading is correct: tangled_rope or rope classification, lower theater_ratio, beneficiary is actual preparedness. The readings foreclose each other at the level of mandate interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(husk_vs_competence_reading_distinction, empirical, 'Whether preparedness constraint is atrophied performance or genuine coordination challenge').

omega_variable(
    mandate_obsolescence_vs_persistence,
    'Has the original mandate for preparedness drills (post-disaster institutional learning) become obsolete, or does it remain live?',
    'Historical analysis of disaster response improvements correlated with drill participation; interviews with emergency responders about whether drills inform actual response; comparison of pre-drill and post-drill era disaster outcomes.',
    'If mandate is dead: mandatrophy is confirmed, piton classification is structural. If mandate is live: the constraint may be tangled_rope (mixed coordination and extraction) rather than piton (atrophied function). The husk reading asserts mandate death; the competence reading asserts mandate persistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_obsolescence_vs_persistence, empirical, 'Whether preparedness mandate has become obsolete').

omega_variable(
    institutional_legitimacy_as_beneficiary,
    'Is institutional legitimacy (the appearance of preparedness) a genuine beneficiary that collects rents from the constraint, or is it merely a side effect of the drill ritual?',
    'Political economy analysis: do agencies that maintain high-theater drills receive more funding, political support, or regulatory leniency than agencies that invest in competence? Do politicians cite drill participation in campaign messaging?',
    'If legitimacy is a genuine beneficiary: the husk reading''s extraction mechanism is confirmed. If legitimacy is incidental: the constraint may be better classified as rope (pure coordination) or piton (atrophied function without extraction). The husk reading asserts that institutional legitimacy actively benefits from theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_legitimacy_as_beneficiary, empirical, 'Whether institutional legitimacy actively benefits from preparedness theater').

omega_variable(
    reading_kernel_ambiguity,
    'What is the contested kernel that these readings diverge on? Is it the definition of ''preparedness'' (performance vs. competence), the mandate''s current status (live vs. dead), or the beneficiary structure (legitimacy vs. actual readiness)?',
    'Textual analysis of preparedness legislation, agency mission statements, and post-disaster reviews. Interviews with agency leadership about what preparedness means and whether drills achieve it.',
    'The husk reading asserts: preparedness = appearance of readiness; mandate = dead; beneficiary = institutional legitimacy. The competence reading asserts: preparedness = actual response capacity; mandate = live; beneficiary = population safety. The hybrid reading asserts both readings coexist in different institutional contexts. Clarifying the kernel resolves which reading is structurally accurate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_kernel_ambiguity, conceptual, 'Definition of preparedness kernel and its current status').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(husk_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(husk_tr_t0, husk_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(husk_tr_t5, husk_reading, theater_ratio, 5, 0.68).
narrative_ontology:measurement(husk_tr_t10, husk_reading, theater_ratio, 10, 0.81).

% Extraction over time
narrative_ontology:measurement(husk_be_t0, husk_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(husk_be_t5, husk_reading, base_extractiveness, 5, 0.54).
narrative_ontology:measurement(husk_be_t10, husk_reading, base_extractiveness, 10, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(husk_su_t0, husk_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(husk_su_t5, husk_reading, suppression_requirement, 5, 0.42).
narrative_ontology:measurement(husk_su_t10, husk_reading, suppression_requirement, 10, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(husk_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(husk_reading, competence_reading).
narrative_ontology:affects_constraint(husk_reading, hybrid_reading).

% DUAL FORMULATION NOTE:
% The preparedness_persistence kernel decomposes into three constraint stories with different ε values and classifications. The husk reading (this story) asserts that preparedness is atrophied performance (piton, ε=0.62). The competence reading asserts that preparedness is genuine coordination (tangled_rope or rope, lower ε). The hybrid reading asserts that both readings coexist in different institutional contexts. Each story has its own perspectives, beneficiary/victim structure, and measurements. The stories are linked via network.affects_constraints to show the kernel family relationship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
