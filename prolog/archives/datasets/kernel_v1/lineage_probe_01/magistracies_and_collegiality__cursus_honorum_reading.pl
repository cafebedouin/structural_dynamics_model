% ============================================================================
% CONSTRAINT STORY: magistracies_and_collegiality__cursus_honorum_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magistracies_cursus_honorum, []).

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
 *   constraint_id: magistracies_and_collegiality__cursus_honorum_reading
 *   human_readable: The Cursus Honorum: Magistracies as a Prescribed Ladder
 *   domain: legal/doctrinal
 *
 * SUMMARY:
 *   The cursus honorum reading frames the Roman magistracies as a ladder of
 *   prescribed offices (quaestor, aedile or tribune, praetor, consul)
 *   arranged by law in fixed sequence and minimum ages. This constraint
 *   embodies one reading of the magistracies kernel — specifically, the
 *   reading that emphasizes the sequential and hierarchical organization of
 *   ambition. The ladder served multiple structural functions simultaneously:
 *   it coordinated the advancement of aristocratic competitors into orderly
 *   succession, it distributed power-holding across time and persons, and it
 *   suppressed the consolidation of extraordinary power in extraordinary
 *   individuals (Scipio Africanus and Pompey were scandals precisely because
 *   they violated the sequence). The constraint exhibits tangled rope
 *   classification at the middle—an ambitious senator experiences both
 *   genuine coordination (predictable advancement, shared institutional
 *   identity, networks of mutual obligation) and extraction (power rationed
 *   by stage, pace dictated by calendar rather than competence, forced
 *   subordination to seniority). Extraordinary military talent, by contrast,
 *   experiences the constraint as snare: the ladder extracts the benefit of
 *   their abilities while denying them proportional authority. The senatorial
 *   order experiences the ladder as pure coordination: it socializes
 *   ambition, prevents any single faction from consolidating supreme power,
 *   and distributes offices according to predictable rules. The beneficiaries
 *   (the senatorial order and established patrician families) maintain the
 *   ladder through institutional inertia and the prestige of legal form, even
 *   as imperial expansion and civil wars increasingly require exceptional
 *   appointments that circumvent the sequence, raising the theater ratio and
 *   exposing the ladder as performative in the late republic and empire.
 *
 * KEY AGENTS:
 *   - The Extraordinary Commander (powerless/trapped): Scipio, Pompey, Caesar — military genius forced to wait for the calendar; bears the extraction of suppressed ascent
 *   - The Ambitious Quaestor (organized/constrained): Typical senatorial climber — experiences coordination (predictable trajectory) alongside extraction (power rationed by stage)
 *   - The Senatorial Order (institutional/arbitrage): Collective beneficiary — maintains the ladder as coordination mechanism; can arbitrage within it (accelerate favored candidates)
 *   - Established Patrician Families (institutional/arbitrage): Secondary beneficiary — the ladder reproduces their generational advantage under the guise of neutral procedure
 *   - The Calendar Ritual (institutional/arbitrage): The enforcement mechanism itself — persists through inertia and prestige even as actual power migrates to extraordinary appointments
 *   - The Analytical Observer (analytical/analytical): Civilizational view — risks naturalizing the ladder as inherent political necessity rather than contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magistracies_and_collegiality__cursus_honorum_reading, 0.38).
domain_priors:suppression_score(magistracies_and_collegiality__cursus_honorum_reading, 0.62).
domain_priors:theater_ratio(magistracies_and_collegiality__cursus_honorum_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magistracies_and_collegiality__cursus_honorum_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(magistracies_and_collegiality__cursus_honorum_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(magistracies_and_collegiality__cursus_honorum_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magistracies_and_collegiality__cursus_honorum_reading, tangled_rope).
narrative_ontology:human_readable(magistracies_and_collegiality__cursus_honorum_reading, "The Cursus Honorum: Magistracies as a Prescribed Ladder").
narrative_ontology:topic_domain(magistracies_and_collegiality__cursus_honorum_reading, "legal/doctrinal").

domain_priors:requires_active_enforcement(magistracies_and_collegiality__cursus_honorum_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magistracies_and_collegiality__cursus_honorum_reading, 'c487b8ab-11de-42a5-8c46-b034e8d876d1').
narrative_ontology:cs_kernel_codification('c487b8ab-11de-42a5-8c46-b034e8d876d1', formalized).
narrative_ontology:cs_authority_grounding('c487b8ab-11de-42a5-8c46-b034e8d876d1', lineage).
narrative_ontology:cs_interpretation_layer_present('c487b8ab-11de-42a5-8c46-b034e8d876d1').
narrative_ontology:cs_reading_relation('c487b8ab-11de-42a5-8c46-b034e8d876d1', magistracies_and_collegiality__collegial_veto_reading, coexists_with).
narrative_ontology:cs_reading_relation('c487b8ab-11de-42a5-8c46-b034e8d876d1', magistracies_and_collegiality__term_limit_reading, coexists_with).
narrative_ontology:cs_axiom('c487b8ab-11de-42a5-8c46-b034e8d876d1', foundational, ambition_must_be_sequenced_by_law).
narrative_ontology:cs_axiom_status(ambition_must_be_sequenced_by_law, holdable).
narrative_ontology:cs_axiom_grounding('c487b8ab-11de-42a5-8c46-b034e8d876d1', ambition_must_be_sequenced_by_law, instrumental).
narrative_ontology:cs_axiom('c487b8ab-11de-42a5-8c46-b034e8d876d1', foundational, power_distributed_by_calendar_stage_not_competence).
narrative_ontology:cs_axiom_status(power_distributed_by_calendar_stage_not_competence, holdable).
narrative_ontology:cs_axiom_grounding('c487b8ab-11de-42a5-8c46-b034e8d876d1', power_distributed_by_calendar_stage_not_competence, conventional).
narrative_ontology:cs_reference_frame('c487b8ab-11de-42a5-8c46-b034e8d876d1', senatorial_ladder_of_orderly_advancement).
narrative_ontology:cs_drift_state('c487b8ab-11de-42a5-8c46-b034e8d876d1', late_republic_imperial_transition, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c487b8ab-11de-42a5-8c46-b034e8d876d1', '').
narrative_ontology:cs_kernel_id(magistracies_and_collegiality__cursus_honorum_reading, magistracies_and_collegiality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magistracies_and_collegiality__cursus_honorum_reading, senatorial_order).
narrative_ontology:constraint_beneficiary(magistracies_and_collegiality__cursus_honorum_reading, established_patrician_families).
narrative_ontology:constraint_victim(magistracies_and_collegiality__cursus_honorum_reading, extraordinary_military_talent).
narrative_ontology:constraint_victim(magistracies_and_collegiality__cursus_honorum_reading, ambitious_outsiders).
narrative_ontology:constraint_victim(magistracies_and_collegiality__cursus_honorum_reading, rapid_consolidators_of_power).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE EXTRAORDINARY COMMANDER (SNARE) — Scipio Africanus or Pompey: exceptional military talent faces suppression of meteoric ascent. Cannot skip stages; cannot accelerate through demonstrated competence. The ladder's sequence extracts the benefit of their abilities (victory in the field) while denying them the power and prestige appropriate to their achievement. Trapped: refusing the ladder means loss of legitimacy; accepting it means subordination to the calendar. Maximum experienced extraction.
constraint_indexing:constraint_classification(magistracies_and_collegiality__cursus_honorum_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE AMBITIOUS QUAESTOR (TANGLED ROPE) — A capable senator climbing the ladder: the sequence both coordinates (provides clear roles, predictable advancement, shared standards) and extracts (constrains pace, rations access to power, forces patience through administrative stages). Constrained: can advance through the sequence but at cost of delayed authority. Experiences genuine coordination benefit (knows his trajectory, networks predictably) alongside significant extraction (power rationed by stage, not by demonstrated capacity).
constraint_indexing:constraint_classification(magistracies_and_collegiality__cursus_honorum_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE SENATORIAL ORDER (ROPE) — The collective body benefits from the ladder as a pure coordination mechanism: it socializes ambition into orderly sequence, prevents power concentration, distributes offices predictably, and creates shared expectations about advancement. The senatorial order can arbitrage within the system (accelerate a favored candidate through expedited posts, engineer special commands) without losing the coordination function. Net beneficiary; experiences minimal extraction.
constraint_indexing:constraint_classification(magistracies_and_collegiality__cursus_honorum_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ESTABLISHED PATRICIAN FAMILIES (ROPE) — The ladder institutionalizes existing advantage: family networks, age-based succession, and predictable advancement benefit families with generational capital and established connections. The sequence reproduces their dominance while cloaking it as neutral procedure. Can arbitrage within the ladder (ensure their sons advance expeditiously through networks and patronage) while maintaining the appearance of equal access. Net beneficiary; pure coordination from their perspective.
constraint_indexing:constraint_classification(magistracies_and_collegiality__cursus_honorum_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: THE CALENDAR RITUAL (PITON) — Over centuries, the calendar's enforcement of the ladder's sequence becomes increasingly performative. Late-republican and imperial extensions (special commands, extraordinary imperium, prorogation) hollow out the sequence while maintaining the ritual: the ladder persists through institutional inertia and the prestige of legal form, but the actual mechanisms of power have migrated to exception-making and extraordinary appointment. The ladder is maintained because it is familiar, not because it constraints.
constraint_indexing:constraint_classification(magistracies_and_collegiality__cursus_honorum_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some hierarchical sequencing of authority is inherent to governance: you cannot have an orderly state without stages of responsibility and age-based progression. The ladder appears as a natural law of political organization. However, the structural data contradicts this — identifiable beneficiaries (senatorial order, patrician families) and identifiable victims (extraordinary talent, rapid consolidators) reveal that the 'natural necessity' framing naturalizes what is actually a contingent institutional choice. False summit candidate.
constraint_indexing:constraint_classification(magistracies_and_collegiality__cursus_honorum_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magistracies_and_collegiality__cursus_honorum_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(magistracies_and_collegiality__cursus_honorum_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(magistracies_and_collegiality__cursus_honorum_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(magistracies_and_collegiality__cursus_honorum_reading, TR),
    TR >= 0.70.

:- end_tests(magistracies_and_collegiality__cursus_honorum_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The ladder extracts the benefit of exceptional military talent and suppresses meteoric consolidation, but it also provides genuine coordination benefits for typical ambitious senators who climb through orderly stages. The extractiveness reflects that the constraint combines real coordination (for the middle ranks) with real extraction (for the exceptional cases). The measurement trajectory shows a slight rise from 0.28 to 0.38 over the republican period, reflecting the increasing cost of suppressing exceptional talent as military competition intensifies. Suppression (0.62): Moderately high. The ladder actively suppresses extraordinary ascent through legal requirements (age minimums, required intervals between offices, prohibition of holding multiple magistracies simultaneously) and social scandal (Scipio and Pompey's accelerated careers generated legitimacy crises). Suppression is maintained through institutional enforcement (the Senate refuses to grant dispensations) and through the constraint of magistrate collegiality, which prevents any single officeholder from commanding unilateral power. Theater ratio (0.48): Moderate-low, rising. In the early-to-middle republic, the sequence functions genuinely — advancement truly is paced by the calendar and the hierarchy is enforced. However, by the late republic, the measurement shows rising theater: extraordinary commands (Pompey's special provincia, Caesar's imperium), prorogations, and other exceptions hollow out the sequence while maintaining the ritual. The ladder persists through inertia and prestige, not through actual constraint.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates a perspectival collapse between the senatorial order's experience (rope — pure coordination) and the extraordinary talent's experience (snare — pure extraction). Both perspectives measure the same structural phenomenon but reach opposite classifications. The gap reveals that the ladder functions as coordination for those it includes and benefits (typical senators, established families) while functioning as extraction for those it excludes and disadvantages (exceptional military talent, ambitious outsiders). The piton perspective (the calendar ritual at civilizational scope) shows how the constraint persists through institutional form even as its actual enforcement power decays over time. The mountain perspective risks naturalizing the sequence as inherent to governance, but the structural data (beneficiaries and victims) exposes this as false summit — the ladder is a contingent institutional choice benefiting identifiable agents, not a natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's experienced extraction (chi) is derived from base extractiveness (0.38), the agent's structural position (beneficiary vs victim, power level, exit options), and the scope modifier. Beneficiaries (senatorial order, patrician families) with institutional power and arbitrage exit options derive low directionality (d ≈ 0.15-0.20), producing negative or low chi — the constraint subsidizes them. Victims (extraordinary talent, ambitious outsiders) with powerless status and trapped exit options derive high directionality (d ≈ 0.90), producing high chi — the constraint extracts from them. The ambitious middle senator derives moderate directionality (d ≈ 0.55), producing moderate chi — mixed extraction and benefit. The calendar ritual at institutional scope derives institutional directionality (d ≈ 0.10), producing low chi — the ritual is maintained because it benefits the system.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_versus_seniority_trade,
    'Does suppression of meteoric ascent optimize for senatorial stability at the cost of military competence? Or does demonstrated battlefield success eventually override the sequence?',
    'Historical comparison of military outcomes under ladder-enforced commanders vs. exceptional-appointment commanders (Scipio, Pompey, Caesar); correlation between advancement speed and campaign success rates',
    'If competence wins: the snare perspective is contested — exceptional talent may break through the sequence when stakes are high, reclassifying to tangled_rope from the extraordinary commander view. If seniority wins: the snare classification holds; the ladder successfully extracts exceptional ability under the cover of procedural regularity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(competence_versus_seniority_trade, empirical, 'Whether exceptional military success overrides the sequence constraint').

omega_variable(
    patrician_network_amplification,
    'How much of the senatorial order''s actual advancement is determined by family networks vs. the ladder''s formal sequence? Is the ladder''s ''neutral procedure'' a cover for inherited advantage?',
    'Prosopographic analysis: correlation between family background (patrician/plebeian, connected families/outsiders) and advancement speed through identical ladder stages; variance in time spent at each level by family origin',
    'If networks dominate: the ladder is exposed as institutional cover for reproduction of patrician privilege. The rope classification (from senatorial perspective) becomes false summit — it is actually tangled_rope or snare for outsiders, with the senatorial order as beneficiary concealing extraction through procedure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(patrician_network_amplification, empirical, 'Degree to which family networks override the ladder''s formal sequence').

omega_variable(
    exceptional_command_frequency,
    'How often do late-republican and imperial regimes grant extraordinary appointments and prorogations that circumvent the ladder? Does this frequency reveal the ladder as performative inertia?',
    'Quantitative analysis of regular ladder appointments vs. extraordinary appointments over 150-year span; ratio of formal succession vs. exception-making',
    'If exceptions are rare: the ladder retains real constraining force. If exceptions are frequent: the piton perspective is confirmed — the ladder is maintained through ritual but power flows through exception-making, not through sequence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exceptional_command_frequency, empirical, 'Frequency of extraordinary appointments circumventing the ladder sequence').

omega_variable(
    kernel_reading_contest,
    'Is the cursus honorum''s core function the sequencing of ambition (this reading), the collegiate veto preventing any one magistrate from commanding unilateral power (collegial_veto_reading), or the calendar''s one-year expiration forcing rotation (term_limit_reading)?',
    'Analysis of which mechanism actually prevented power consolidation most effectively in cases of attempted coup or exceptional ambition (Catiline, Sulla, Caesar). Which constraint failed first when the republic broke down?',
    'If sequencing was the primary brake: this reading is confirmed as the dominant mechanism. If collegiality held when sequence failed: collegial_veto_reading is the better reading. If the calendar mattered most: term_limit_reading should be preferred. The answer determines which reading of the magistracies kernel best explains actual Roman political dynamics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, empirical, 'Which mechanism (sequence, collegiality, or calendar) most effectively constrained power consolidation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magistracies_and_collegiality__cursus_honorum_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magi_tr_t0, magistracies_and_collegiality__cursus_honorum_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(magi_tr_t40, magistracies_and_collegiality__cursus_honorum_reading, theater_ratio, 40, 0.38).
narrative_ontology:measurement(magi_tr_t80, magistracies_and_collegiality__cursus_honorum_reading, theater_ratio, 80, 0.48).

% Extraction over time
narrative_ontology:measurement(magi_be_t0, magistracies_and_collegiality__cursus_honorum_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(magi_be_t40, magistracies_and_collegiality__cursus_honorum_reading, base_extractiveness, 40, 0.35).
narrative_ontology:measurement(magi_be_t80, magistracies_and_collegiality__cursus_honorum_reading, base_extractiveness, 80, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(magi_su_t0, magistracies_and_collegiality__cursus_honorum_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(magi_su_t40, magistracies_and_collegiality__cursus_honorum_reading, suppression_requirement, 40, 0.61).
narrative_ontology:measurement(magi_su_t80, magistracies_and_collegiality__cursus_honorum_reading, suppression_requirement, 80, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magistracies_and_collegiality__cursus_honorum_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(magistracies_and_collegiality__cursus_honorum_reading, magistracies_and_collegiality__collegial_veto_reading).
narrative_ontology:affects_constraint(magistracies_and_collegiality__cursus_honorum_reading, magistracies_and_collegiality__term_limit_reading).

% DUAL FORMULATION NOTE:
% The magistracies kernel admits three structurally distinct constraint readings: sequencing (cursus_honorum_reading, this story), collegiality (collegial_veto_reading), and calendar expiration (term_limit_reading). Each reading has its own ε and beneficiary/victim structure. They coexist as live interpretations of the same kernel, not as competing descriptions of a single constraint. All three readings converge in empirical outcome (preventing power consolidation) but differ in the mechanism assigned as primary.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(magistracies_and_collegiality__cursus_honorum_reading, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
