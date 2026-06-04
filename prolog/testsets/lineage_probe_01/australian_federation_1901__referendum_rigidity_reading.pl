% ============================================================================
% CONSTRAINT STORY: australian_federation_1901__referendum_rigidity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_australian_federation_1901__referendum_rigidity_reading, []).

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
 *   constraint_id: australian_federation_1901__referendum_rigidity_reading
 *   human_readable: Australian Section 128 Referendum Rigidity: Elite Constitutional Change Blocked by Double-Majority Veto
 *   domain: legal/constitutional/political
 *
 * SUMMARY:
 *   Section 128 of the Australian Constitution enshrines amendment by
 *   double-majority referendum: passage requires a majority in the nation
 *   overall AND a majority of states. This reading instantiates Section 128
 *   as a constraint on constitutional change that systematically suppresses
 *   reform movements. The design privileges the status quo through structural
 *   veto: eight passed out of forty-five attempted amendments (18% success
 *   rate) reflects an almost-reliable popular veto on change. The constraint
 *   exhibits the tangled rope pattern: it performs a genuine coordination
 *   function (the people's voice is invoked in amendment decisions, creating
 *   legitimacy through popular sovereignty) while simultaneously functioning
 *   as an extraction mechanism (reform programs are suppressed; the status
 *   quo extracts protection; organized reform governments pay the cost of
 *   mounting referendum campaigns with predictably low success). From the
 *   powerless perspective (excluded constituencies seeking recognition),
 *   Section 128 is pure snare — the exit cost is infinite because
 *   constitutional amendment is the only path to rights recognition, and that
 *   path is structurally closed. From the beneficiary perspective
 *   (constitutional status quo), Section 128 is rope — coordination without
 *   felt extraction. The constraint has intensified over time: extractiveness
 *   rose from 0.45 (1901) to 0.62 (2026) as reform movements accumulated and
 *   electoral mandates for change were systematically blocked by the double
 *   majority. Theater ratio rose from 0.35 to 0.48 as referendum campaigns
 *   became more visible spectacles despite predictable failure.
 *
 * KEY AGENTS:
 *   - Reform Movements (Indigenous recognition, workers' rights, federal power expansion): Powerless/trapped — bear full cost of structural veto; cannot exit the amendment path because alternative constitutional change mechanisms do not exist
 *   - Elected Reform Governments (Whitlam, Rudd, recent ALP): Organized/constrained — win electoral mandates for change; benefit from legitimacy ritual of referendum campaigns; bear extraction cost when double majority blocks passage
 *   - Constitutional Status Quo (State governments, institutional defenders of 1901 federalism): Institutional/arbitrage — primary beneficiary; experience Section 128 as legitimate coordination mechanism protecting federal compact
 *   - Constitutional Law Scholarship: Institutional/arbitrage — maintains formalist narrative that Section 128 reflects appropriate super-majority consensus requirement; benefits from intellectual authority in defending existing amendment threshold
 *   - Analytical Observer: Analytical/analytical — views amendment rigidity as immutable law of federal structures or as contingent institutional choice depending on reference frame
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(australian_federation_1901__referendum_rigidity_reading, 0.62).
domain_priors:suppression_score(australian_federation_1901__referendum_rigidity_reading, 0.65).
domain_priors:theater_ratio(australian_federation_1901__referendum_rigidity_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(australian_federation_1901__referendum_rigidity_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(australian_federation_1901__referendum_rigidity_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(australian_federation_1901__referendum_rigidity_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(australian_federation_1901__referendum_rigidity_reading, tangled_rope).
narrative_ontology:human_readable(australian_federation_1901__referendum_rigidity_reading, "Australian Section 128 Referendum Rigidity: Elite Constitutional Change Blocked by Double-Majority Veto").
narrative_ontology:topic_domain(australian_federation_1901__referendum_rigidity_reading, "legal/constitutional/political").

domain_priors:requires_active_enforcement(australian_federation_1901__referendum_rigidity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(australian_federation_1901__referendum_rigidity_reading, '4177c76d-1428-4a1c-b39b-1703856a3d5d').
narrative_ontology:cs_kernel_codification('4177c76d-1428-4a1c-b39b-1703856a3d5d', formalized).
narrative_ontology:cs_authority_grounding('4177c76d-1428-4a1c-b39b-1703856a3d5d', lineage).
narrative_ontology:cs_interpretation_layer_present('4177c76d-1428-4a1c-b39b-1703856a3d5d').
narrative_ontology:cs_reading_relation('4177c76d-1428-4a1c-b39b-1703856a3d5d', australian_federation_1901__dismissal_1975_reading, coexists_with).
narrative_ontology:cs_reading_relation('4177c76d-1428-4a1c-b39b-1703856a3d5d', australian_federation_1901__washminster_hybrid_reading, influences).
narrative_ontology:cs_axiom('4177c76d-1428-4a1c-b39b-1703856a3d5d', foundational, constitutional_change_requires_federally_distributed_consensus).
narrative_ontology:cs_axiom_status(constitutional_change_requires_federally_distributed_consensus, holdable).
narrative_ontology:cs_axiom_grounding('4177c76d-1428-4a1c-b39b-1703856a3d5d', constitutional_change_requires_federally_distributed_consensus, instrumental).
narrative_ontology:cs_axiom('4177c76d-1428-4a1c-b39b-1703856a3d5d', foundational, popular_sovereignty_invocation_legitimates_constitutional_stability).
narrative_ontology:cs_axiom_status(popular_sovereignty_invocation_legitimates_constitutional_stability, holdable).
narrative_ontology:cs_axiom_grounding('4177c76d-1428-4a1c-b39b-1703856a3d5d', popular_sovereignty_invocation_legitimates_constitutional_stability, deontological).
narrative_ontology:cs_reference_frame('4177c76d-1428-4a1c-b39b-1703856a3d5d', constitutional_amendment_by_federal_consensus).
narrative_ontology:cs_drift_state('4177c76d-1428-4a1c-b39b-1703856a3d5d', contemporary_reform_pressure_accumulation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('4177c76d-1428-4a1c-b39b-1703856a3d5d', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(australian_federation_1901__referendum_rigidity_reading, australian_federation_1901).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(australian_federation_1901__referendum_rigidity_reading, constitutional_status_quo).
narrative_ontology:constraint_beneficiary(australian_federation_1901__referendum_rigidity_reading, existing_institutional_order).
narrative_ontology:constraint_victim(australian_federation_1901__referendum_rigidity_reading, reform_programs).
narrative_ontology:constraint_victim(australian_federation_1901__referendum_rigidity_reading, marginalized_constituencies).
narrative_ontology:constraint_victim(australian_federation_1901__referendum_rigidity_reading, indigenous_recognition_movements).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: REFORM MOVEMENTS (SNARE) — Indigenous recognition, workers' rights amendments, federal power expansion: all require Section 128 passage. The constraint extracts near-total suppression of reform pathways. Powerless agents (excluded constituencies) face an exit cost that is effectively infinite — they cannot amend the constitution that governs them without triggering the double majority, which has failed 37 of 45 times. The mechanism is pure extraction: reform capacity is systematically suppressed; the beneficiary is the status quo; the veto mechanism permits a permanent minority to block change indefinitely.
constraint_indexing:constraint_classification(australian_federation_1901__referendum_rigidity_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ELECTED REFORM GOVERNMENTS (TANGLED ROPE) — Governments winning elections on reform mandates (Whitlam on land rights, Rudd on indigenous apology, recent ALP platforms) face the double-majority constraint. They benefit from the legitimacy-conferring ritual of a referendum campaign (coordination: the people's voice is invoked) but bear the extraction cost when majorities in the lower house cannot overcome the federal-state veto structure. Constrained exit: they can govern, but cannot amend. Mixed: genuine coordination function (invoking popular sovereignty) alongside systematic suppression (the veto works almost always).
constraint_indexing:constraint_classification(australian_federation_1901__referendum_rigidity_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CONSTITUTIONAL STATUS QUO (ROPE) — State governments, entrenched institutional interests, and defenders of 1901 federalism see Section 128 as a coordination mechanism: it requires consensus before the Constitution changes. They benefit from structural stability; the double majority is their protection. From this perspective, the constraint is legitimate coordination: constitutional change should require more than bare legislative majority. No extraction is experienced because this agent is the beneficiary.
constraint_indexing:constraint_classification(australian_federation_1901__referendum_rigidity_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CONSTITUTIONAL FORMALISM (PITON) — The referendum ritual itself (campaigns, voting, result) is substantially performative. The outcomes are so predictable (failure rate 82%) that the referendum process functions as theater: a legitimacy-conferring ritual that permits the saying of 'the people decided' while outcomes are structurally predetermined by the federal veto design. The amendment process persists through institutional inertia and the legitimacy cover it provides, not because it functions as a genuine deliberative mechanism. Theater ratio ≥ 0.70 derives from the mechanism's ritualistic character and known failure rate.
constraint_indexing:constraint_classification(australian_federation_1901__referendum_rigidity_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: NATURAL LAW OF FEDERAL STRUCTURES (MOUNTAIN) — From a civilizational analytical view, federal systems structurally require super-majorities to change their foundations — the symmetry between states must be preserved, and unilateral change would destabilize the compact. This perspective sees Section 128's rigor as immutable: federal structures cannot be amended by simple majority in one jurisdiction; the requirement flows from the federalism principle itself. The constraint appears as a structural necessity. However, this reading risks naturalizing what is a contingent institutional choice — other federations (USA, Germany, Canada) have different thresholds and mechanisms.
constraint_indexing:constraint_classification(australian_federation_1901__referendum_rigidity_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(australian_federation_1901__referendum_rigidity_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(australian_federation_1901__referendum_rigidity_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(australian_federation_1901__referendum_rigidity_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(australian_federation_1901__referendum_rigidity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(australian_federation_1901__referendum_rigidity_reading, TR),
    TR >= 0.70.

:- end_tests(australian_federation_1901__referendum_rigidity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): High-moderate. Section 128 extracts protection for the status quo by systematically suppressing reform pathways. The extraction is not maximal because: (a) the mechanism operates through a transparent procedural rule (not hidden coercion), (b) eight amendments have passed (creating a nonzero exit cost for reform), and (c) the constraint's legitimacy rests partly on genuine coordination logic (invoking popular sovereignty). However, the 82% failure rate is sufficient evidence that extraction is substantial — reform programs are systematically blocked; the suppression mechanism is nearly reliable. Suppression (0.65): High. Barriers to amendment include: the need for simultaneous national majority AND state majority (federating veto), the campaign costs and public skepticism required to overcome 125 years of failed attempts, the structural irrelevance of lower-house supermajorities (they cannot unilaterally amend), and the perception that constitutional change is exceptionally difficult. These barriers reduce suppression below total (a truly trapped agent would have zero exit options; reform governments retain electoral mandate options even if constitutional amendment fails) but raise it above moderate. Theater ratio (0.48): Moderate. Referendums are not purely performative — outcomes genuinely depend on public voting (not predetermined). However, the predictable failure rate (82%) and the campaign's ceremonial invocation of 'the people's will' introduce substantial performative elements. The referendum ritual functions partly as legitimacy theater: even when the reform government wins lower-house supermajority and mounts a campaign, the federalized double majority ensures predictable failure, yet the ritual of consulting the people provides legitimacy cover for the outcome.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates a three-fold perspectival gap: (1) Beneficiary vs Victim: Constitutional status quo experiences Section 128 as rope (coordination protecting federal symmetry); reform movements experience it as snare (systematic suppression). (2) Immediate vs Civilizational: Elected reform governments see constrained exit (biographical horizon); analytical observers see natural law (civilizational horizon sees federalism principles as immutable). (3) Organized vs Powerless: Organized reform governments can mount campaigns and win electoral mandates (constrained exit); excluded constituencies have no alternative path to rights recognition (trapped exit). The piton perspective reveals the mechanism's degradation: the referendum ritual persists through inertia and legitimacy cover despite the known low functionality. The analytical mountain perspective risks naturalizing federalism principles as unchangeable law when other federal systems (USA, Germany, Canada) have lower thresholds or different mechanisms.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality (d) value encodes their structural relationship to Section 128. Beneficiaries of the status quo (institutional power, arbitrage exit) derive low d → low experienced extraction. Reform governments (organized power, constrained exit) derive medium-high d → medium-high extraction. Reform movements (powerless, trapped) derive maximum d → maximum extraction. The constraint's effective extractiveness (chi) scales with agent power and exit options per the formula. At the powerless/trapped pole, chi approaches the base extractiveness directly because f(d) amplifies extraction for trapped agents. At the institutional/arbitrage pole, chi can turn negative (the constraint subsidizes status quo defenders). This structure explains why the same constitutional rule appears as pure snare from the powerless perspective but pure rope from the beneficiary perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   Section 128 resolves the mandatrophy by recognizing that genuine coordination function (the need to protect federal symmetry and invoke popular sovereignty for constitutional change) coexists with systematic extraction (the suppression of reform). The tangled rope classification captures this hybrid. The snare classification from the powerless perspective is not contradictory — it accurately describes what the constraint does to agents with no exit options. The piton classification describes the constraint's degradation: the referendum ritual has become more theatrical as the failure rate has proven reliable, yet the ritual persists because it provides legitimacy cover for the real function (protecting the status quo). The mountain classification from the civilizational analytical perspective reveals the risk of naturalizing contingent design — federalism principles are real, but the specific double-majority threshold is a choice, not a law of nature. Mandatrophy is resolved by recognizing that the constraint is simultaneously a legitimate coordination mechanism AND an asymmetric extraction device, depending on structural position.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    amendment_frequency_sufficiency,
    'Is the 8-in-45 passage rate evidence that Section 128 suppresses legitimate reform or confirmation that only consensual reforms should pass?',
    'Comparative analysis: survey Australian constitutional law experts on which failed amendments represented major consensual reform vs. contested partisan change; compare to passage rates in other federations with similar or lower thresholds',
    'If the 82% failure rate suppresses genuine consensus reforms: Section 128 is extractive snare. If failed amendments were mostly partisan or low-consensus: Section 128 functions as intended coordination mechanism. Shifts classification from snare/tangled_rope toward rope for reform government perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_frequency_sufficiency, empirical, 'Whether amendment failure rate reflects suppression or appropriate consensus filtering').

omega_variable(
    state_veto_legitimacy,
    'Does the state-level veto component of Section 128 (requirement of majority in majority of states) represent genuine federal symmetry or an anachronistic power imbalance favoring small states?',
    'Historical analysis of state-population distribution shifts (NSW+VIC comprise ~65% of population; equal-state veto means 35% can block); comparison to original 1901 state-population parity; analysis of which reforms have been blocked by low-population state opposition',
    'If state veto represents genuine federal balance: the constraint has legitimate coordination function. If state veto is anachronistic minority rule: the constraint functions as extraction mechanism favoring small-state interests over national democratic majority.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_veto_legitimacy, empirical, 'Whether state-level veto component functions as federal balance or minority rule').

omega_variable(
    reading_contest_under_determination,
    'Within the Australian Federation kernel, does Section 128 rigidity represent the fundamental design feature (this reading) or merely one fault line exposed in the 1975 Dismissal crisis (sibling reading)?',
    'Constitutional law scholarship on whether amendment difficulty is central vs. peripheral to the 1901 compromise; historical argument structure of founding documents; analysis of which design feature (federation structure vs. amendment threshold vs. reserve powers) would require change to prevent institutional crises',
    'If Section 128 is fundamental design: constitutional reform would require addressing rigidity as the central constraint. If Dismissal is fundamental: reserve powers and executive discretion are the core problem. The two readings coexist across different diagnostic communities (federalism scholars vs. constitutional crisis analysts).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_under_determination, conceptual, 'Whether Section 128 rigidity is the fundamental or derivative feature of the Australian constitutional design').

omega_variable(
    kernel_reading_ambiguity,
    'Is the Australian Federation kernel best understood through the reading that emphasizes amendment rigidity (this constraint) or through readings emphasizing hybrid reserve powers (dismissal) or balanced institutional design (washminster)?',
    'Committer-frame analysis: which reading''s reference frame best explains constitutional evolution, institutional crises, and reform patterns? Does rigidity predict outcomes better than reserve powers or institutional design? Do constitutional law scholars gravitate toward one reading as explanatory?',
    'If rigidity reading is dominant: Section 128 is the core constraint shaping Australian constitutional politics. If reserve powers or hybrid design is dominant: rigidity is derivative. This omega documents the intra-kernel contest among siblings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Kernel-level ambiguity: which reading best characterizes the Australian Federation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(australian_federation_1901__referendum_rigidity_reading, 0, 125).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ausref_theater_1901, australian_federation_1901__referendum_rigidity_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ausref_theater_1951, australian_federation_1901__referendum_rigidity_reading, theater_ratio, 50, 0.42).
narrative_ontology:measurement(ausref_theater_2001, australian_federation_1901__referendum_rigidity_reading, theater_ratio, 100, 0.48).
narrative_ontology:measurement(ausref_theater_2026, australian_federation_1901__referendum_rigidity_reading, theater_ratio, 125, 0.48).

% Extraction over time
narrative_ontology:measurement(ausref_extract_1901, australian_federation_1901__referendum_rigidity_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(ausref_extract_1951, australian_federation_1901__referendum_rigidity_reading, base_extractiveness, 50, 0.58).
narrative_ontology:measurement(ausref_extract_2001, australian_federation_1901__referendum_rigidity_reading, base_extractiveness, 100, 0.62).
narrative_ontology:measurement(ausref_extract_2026, australian_federation_1901__referendum_rigidity_reading, base_extractiveness, 125, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(ausref_suppress_1901, australian_federation_1901__referendum_rigidity_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(ausref_suppress_1951, australian_federation_1901__referendum_rigidity_reading, suppression_requirement, 50, 0.62).
narrative_ontology:measurement(ausref_suppress_2001, australian_federation_1901__referendum_rigidity_reading, suppression_requirement, 100, 0.65).
narrative_ontology:measurement(ausref_suppress_2026, australian_federation_1901__referendum_rigidity_reading, suppression_requirement, 125, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(australian_federation_1901__referendum_rigidity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(australian_federation_1901__referendum_rigidity_reading, australian_federation_1901__dismissal_1975_reading).
narrative_ontology:affects_constraint(australian_federation_1901__referendum_rigidity_reading, australian_federation_1901__washminster_hybrid_reading).
narrative_ontology:affects_constraint(australian_federation_1901__referendum_rigidity_reading, indigenous_recognition_constitutional_bottleneck).
narrative_ontology:affects_constraint(australian_federation_1901__referendum_rigidity_reading, federal_state_power_allocation_rigidity).

% DUAL FORMULATION NOTE:
% Section 128 rigidity is one reading of the Australian Federation kernel. The sibling readings (dismissal_1975, washminster_hybrid) decompose the same foundational commitment into different structural fault lines. All three readings affect downstream constraints in Australian constitutional politics: indigenous recognition efforts, federal-state power allocation, and institutional reform proposals. Each reading provides a different diagnostic lens on why constitutional change is difficult in Australia.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(australian_federation_1901__referendum_rigidity_reading, organized, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
