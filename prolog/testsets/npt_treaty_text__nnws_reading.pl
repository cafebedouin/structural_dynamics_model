% ============================================================================
% CONSTRAINT STORY: npt_treaty_text__nnws_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_treaty_text__nnws_reading, []).

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
 *   constraint_id: npt_treaty_text__nnws_reading
 *   human_readable: NPT Article VI Disarmament Obligation (NNWS Reading): Binding NWS Commitment via Treaty Text and Review Conference Enforcement
 *   domain: international_law/arms_control/treaty_interpretation
 *
 * SUMMARY:
 *   The NPT Article VI disarmament obligation represents a foundational
 *   ambiguity in international law: whether NWS face a binding legal duty to
 *   pursue nuclear disarmament or merely an aspirational commitment to
 *   negotiation. This constraint story instantiates the NNWS reading — the
 *   interpretation that Article VI imposes binding obligation, that NWS
 *   compliance is conditional upon Review Conference pressure and competitive
 *   legitimacy threats (TPNW), and that NNWS retain verification and
 *   enforcement leverage through collective action. This reading frames
 *   disarmament as a genuine coordination problem with asymmetric extraction:
 *   NWS benefit from continued arsenal possession while nominally complying
 *   with disarmament rhetoric; NNWS bear the cost of reduced security options
 *   and perpetual enforcement failure. The NNWS reading differs structurally
 *   from the NWS reading (which treats Article VI as hortatory), which
 *   differs from the withdrawal-threshold reading (which treats NPT
 *   withdrawal as a response mechanism that restructures the binding force of
 *   Article VI). The NNWS reading is held primarily by non-nuclear states,
 *   some regional security alliances, TPNW signatories, and progressive
 *   arms-control advocacy coalitions. It coexists with the NWS reading in the
 *   treaty institution itself — no single framework resolves both
 *   simultaneously.
 *
 * KEY AGENTS:
 *   - Non-Nuclear Weapons States (NNWS collective): Primary victim and secondary beneficiary (powerless/trapped, and organized/constrained) — bear cost of enforcement failure and security exposure while benefiting from NPT non-proliferation regime that constrains peer acquisition. Demand binding disarmament and credible verification.
 *   - Nuclear Weapons States (NWS P5 + unofficial NWS): Primary beneficiary with constrained options (powerful/constrained) — extract benefit from continued arsenal possession, extended deterrence relationships, and strategic autonomy while nominally complying with Article VI commitments. Face reputational cost of overt non-compliance.
 *   - NPT Review Conference Apparatus: Institutional actor (institutional/arbitrage) — maintains review structure and consensus procedures; benefits from continued convening authority; theater increases as substantive outcomes decline.
 *   - TPNW Coalition: Organized NNWS (organized/mobile) — created alternative regime outside NWS veto; generates regime competition and reputational pressure on NPT; has implicit sunset (becomes redundant if NPT enforcement upgrades).
 *   - Analytical Observer: Civilization timescale (analytical/analytical) — risks naturalizing NWS nuclear autonomy as immutable feature of sovereignty, masking contingent institutional arrangements (export controls, enrichment monopolies, strategic partnerships) that maintain the distribution.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_text__nnws_reading, 0.38).
domain_priors:suppression_score(npt_treaty_text__nnws_reading, 0.48).
domain_priors:theater_ratio(npt_treaty_text__nnws_reading, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_text__nnws_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_text__nnws_reading, "NPT Article VI Disarmament Obligation (NNWS Reading): Binding NWS Commitment via Treaty Text and Review Conference Enforcement").
narrative_ontology:topic_domain(npt_treaty_text__nnws_reading, "international_law/arms_control/treaty_interpretation").

domain_priors:requires_active_enforcement(npt_treaty_text__nnws_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_text__nnws_reading, '5a73d7d2-3c57-4249-ba8d-4c199b5491f3').
narrative_ontology:cs_kernel_codification('5a73d7d2-3c57-4249-ba8d-4c199b5491f3', fixed_text).
narrative_ontology:cs_authority_grounding('5a73d7d2-3c57-4249-ba8d-4c199b5491f3', lineage).
narrative_ontology:cs_interpretation_layer_present('5a73d7d2-3c57-4249-ba8d-4c199b5491f3').
narrative_ontology:cs_reading_relation('5a73d7d2-3c57-4249-ba8d-4c199b5491f3', npt_treaty_text__nws_reading, coexists_with).
narrative_ontology:cs_reading_relation('5a73d7d2-3c57-4249-ba8d-4c199b5491f3', npt_treaty_text__withdrawal_threshold_reading, influences).
narrative_ontology:cs_axiom('5a73d7d2-3c57-4249-ba8d-4c199b5491f3', foundational, article_vi_imposes_binding_disarmament_obligation).
narrative_ontology:cs_axiom_status(article_vi_imposes_binding_disarmament_obligation, holdable).
narrative_ontology:cs_axiom_grounding('5a73d7d2-3c57-4249-ba8d-4c199b5491f3', article_vi_imposes_binding_disarmament_obligation, deontological).
narrative_ontology:cs_axiom('5a73d7d2-3c57-4249-ba8d-4c199b5491f3', foundational, review_conference_consensus_constitutes_enforcement_mechanism).
narrative_ontology:cs_axiom_status(review_conference_consensus_constitutes_enforcement_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('5a73d7d2-3c57-4249-ba8d-4c199b5491f3', review_conference_consensus_constitutes_enforcement_mechanism, conventional).
narrative_ontology:cs_reference_frame('5a73d7d2-3c57-4249-ba8d-4c199b5491f3', article_vi_binding_disarmament_commitment).
narrative_ontology:cs_drift_state('5a73d7d2-3c57-4249-ba8d-4c199b5491f3', contemporary_2026, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5a73d7d2-3c57-4249-ba8d-4c199b5491f3', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(npt_treaty_text__nnws_reading, npt_treaty_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_text__nnws_reading, non_nuclear_weapons_states).
narrative_ontology:constraint_beneficiary(npt_treaty_text__nnws_reading, npt_review_conference_consensus).
narrative_ontology:constraint_victim(npt_treaty_text__nnws_reading, nws_sovereignty_over_arsenal_policy).
narrative_ontology:constraint_victim(npt_treaty_text__nnws_reading, nws_strategic_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% NNWS PERSPECTIVE (SNARE): Cannot exit NPT without triggering regional security crisis; cannot force NWS compliance through treaty mechanisms; bear extraction cost of reduced security options and continued exposure to potential NWS use while disarmament stalls. Trapped by alliance dependencies and regional dynamics. Maximum experienced extraction — no exit options, no enforcement mechanism they control.
constraint_indexing:constraint_classification(npt_treaty_text__nnws_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% NNWS COALITION PERSPECTIVE (ROPE): Organized through Review Conference consensus-building, TPNW accession threshold logic, and coordinated voting blocs. Generate sufficient pressure on NWS for cosmetic compliance (committees, reports, diplomatic engagement statements) without forcing structural disarmament. Constrained by inability to sanction NWS directly but benefit from coordination among themselves and from demonstrating active treaty compliance. Experience the constraint as coordination mechanism with modest enforcement leverage.
constraint_indexing:constraint_classification(npt_treaty_text__nnws_reading, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% NWS PERSPECTIVE (TANGLED ROPE): Bound by Article VI commitment yet possess structural veto power over enforcement mechanisms (permanent Security Council seat, nuclear deterrent capability itself). Genuine coordination function exists (predictable rules prevent destabilizing arms races, confidence-building measures reduce miscalculation risk); simultaneously extract benefit from extended deterrence posture and arsenal modernization while nominally complying with 'disarmament in principle' language. Constrained by reputational cost of naked treaty violation, but not by enforcement mechanism — the constraint coordinates arms control norms while permitting de facto arsenal maintenance.
constraint_indexing:constraint_classification(npt_treaty_text__nnws_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% TPNW COALITION PERSPECTIVE (SCAFFOLD): Alternative verification pathway created by organized NNWS that bypassed NWS blockade of NPT enforcement. TPNW represents temporary scaffolding with implicit sunset: as threshold compliance increases and reputational cost of NPT violation rises, the structural pressure shifts. TPNW's existence creates regime competition and legitimacy pressure on NPT. Mobile exit for NNWS (can join TPNW); has sunset clause built into its logic — once NPT enforcement mechanisms are upgraded or NWS face sufficient reputational/strategic cost, TPNW becomes redundant. Current theater ratio (0.58): many signature ceremonies and compliance statements, modest concrete harm reduction.
constraint_indexing:constraint_classification(npt_treaty_text__nnws_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% REVIEW CONFERENCE PERSPECTIVE (PITON): The Review Conference apparatus itself has degraded into theatrical performance — every 5 years, NNWS and NWS gather to reaffirm commitment to disarmament while taking no structural steps to enforce it. The ritual persists through institutional inertia (the NPT apparatus has staff, established procedures, convening authority) despite minimal functional verification or enforcement capability. High theater ratio (0.61 baseline, rising): outcome documents use increasingly vague language ('reaffirms commitment,' 'expresses concern'), consensus requirement ensures lowest-common-denominator statements, working groups produce reports that sit on shelves. The apparatus is maintained because dismantling it would signal treaty collapse, not because it functions.
constraint_indexing:constraint_classification(npt_treaty_text__nnws_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% ANALYTICAL / NATURAL LAW PERSPECTIVE (MOUNTAIN — FALSE SUMMIT CANDIDATE): The most restrictive NWS reading naturalizes state sovereignty over strategic arsenal decisions as immutable law of international relations. 'States cannot credibly commit to disarm; nuclear deterrence is permanent; Article VI is aspirational only.' This perspective treats the distribution of nuclear weapons as a feature of the anarchic international system itself, not as a contingent historical outcome maintained by specific institutional arrangements (export control regimes, strategic partnerships, intellectual property barriers, enrichment monopolies). The engine flags this as a false summit: identifiable beneficiaries (NWS nuclear planners, extended deterrence states) and specific institutional mechanisms maintain this 'immutable' distribution.
constraint_indexing:constraint_classification(npt_treaty_text__nnws_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_treaty_text__nnws_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(npt_treaty_text__nnws_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(npt_treaty_text__nnws_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(npt_treaty_text__nnws_reading, TR),
    TR >= 0.70.

:- end_tests(npt_treaty_text__nnws_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The NNWS reading sees moderate extraction: NWS retain decision-making authority over arsenal modernization while NNWS bear opportunity costs (cannot acquire nuclear capabilities without treaty violation, cannot credibly threaten withdrawal without regional destabilization). The extraction is not maximal because genuine coordination exists — the NPT regime prevents destabilizing arms races and creates predictable rules for all parties. Some NNWS benefit from extended nuclear deterrence (NATO, Japan, South Korea), reducing their perceived extraction. Suppression (0.48): Moderate. Suppression mechanisms include: security dependencies (alliance relationships that make NPT exit costly); institutional lock-in (withdrawal triggers dangerous inspection gaps and alliance crises); technological barriers (NNWS cannot easily acquire disarmament-level expertise without infrastructure); information asymmetry (NWS control verification methods and data). But suppression is not total — TPNW provides alternative exit pathway, withdrawal is legally available, and organized coalitions can generate political pressure. Theater ratio (0.61): Moderate-high, rising over the interval (0.48 → 0.61 across 20 years). Rising theater reflects: increasing vagueness in Review Conference language ('reaffirms commitment' replaces specific timelines), growing gap between disarmament promises and actual NWS arsenal trajectories, expansion of procedural steps (working groups, committees, fact-finding missions) without substantive outcomes, performance of progress on verification while verification mechanisms remain underfunded and ineffective.
 *
 * PERSPECTIVAL GAP:
 *   This reading produces sharp perspectival divergence across power levels. The powerless NNWS perspective (Snare) sees a binding obligation being violated with no enforcement mechanism. The organized NNWS coalition perspective (Rope) sees coordination leverage through consensus-building and TPNW competition. The NWS perspective (Tangled Rope from this reading; would be Rope from the NWS reading) sees genuine coordination benefits alongside arsenals maintained as strategic necessity. The Review Conference apparatus perspective (Piton) sees institutional inertia and theatrical ritual. The natural-law analytical perspective (Mountain, false summit candidate) risks naturalizing state strategic autonomy as immutable, masking the specific institutional mechanisms (enrichment monopolies, supplier agreements, strategic partnerships) that maintain NWS capability advantage. The key diagnostic gap: whether Article VI is binding determines whether the NNWS reading (Tangled Rope with coordination function) or NWS reading (Rope or Piton with minimal enforcement) is more structurally accurate.
 *
 * DIRECTIONALITY LOGIC:
 *   NWS directionality (d): approximately 0.35–0.45 when they are treated as beneficiaries constrained by reputational cost but retaining strategic autonomy. They benefit from continued arsenal possession and extended deterrence provision (low d), but face reputational cost of treaty violation and organizational pressure from Review Conferences (moderate d upward). The NNWS reading treats them as constrained rather than arbitrage-level mobile. NNWS directionality (d): approximately 0.68–0.78 when they are treated as victims with constrained exits. They face reduced security options (high d) and institutional lock-in via alliance dependencies, but can theoretically exit via withdrawal or join TPNW (mobile exit exists but with high cost). Organized NNWS coalitions reduce d downward by generating coordinated pressure and alternative regimes. The derived d values feed into the sigmoid f(d), which translates to experienced chi: NNWS experience higher chi (higher extraction), NWS experience lower chi (lower extraction relative to their structural benefit). The Tangled Rope classification requires both a coordination function (genuine arms-control benefit for both sides) AND asymmetric extraction (benefits concentrated on NWS). Both conditions are met under this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids mandatrophy by clearly specifying that Article VI creates coordination (genuine arms-control stabilization) plus asymmetric extraction (NWS retain arsenal autonomy while NNWS bear costs). The Tangled Rope type captures both functions. The mandatrophy emerges in the comparison between this reading and the NWS reading — the same treaty text produces different constraint types from different perspectives. The resolution: this is not a single constraint viewed from two angles, but two readings of a contested kernel, each producing its own constraint with its own ε. The NNWS reading's ε (0.38) reflects moderate extraction with enforceable coordination. The NWS reading would have lower ε (0.20–0.25, Rope or Piton) because the NWS reading treats Article VI as aspirational with minimal extraction. The omega variables document the uncertainty that would resolve which reading is correct.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_binding_vs_aspirational,
    'Does Article VI impose a binding legal obligation on NWS to disarm, or is it aspirational language with no enforcement mechanism?',
    'ICJ advisory opinion on treaty interpretation; historical record of travaux préparatoires showing intent of drafters; enforcement actions (NNWS complaints to UN Security Council or Article VI enforcement proceedings); state practice (NWS willingness to accept inspection, timelines, verification mechanisms)',
    'If binding: NNWS reading sustained as Tangled Rope (genuine coordination function + asymmetric extraction). If aspirational: NWS reading (Rope or Piton) is more defensible — the constraint coordinates arms-control norms without imposing real disarmament.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(article_vi_binding_vs_aspirational, conceptual, 'Whether Article VI is binding or aspirational').

omega_variable(
    review_conference_enforcement_mechanism,
    'Does the NPT Review Conference consensus-building apparatus constitute a functional enforcement mechanism, or is it purely performative?',
    'Empirical: count substantive outcomes (enforced reductions, triggered inspections, blocked arms transfers, reputational sanctions). Comparative: assess Review Conference effectiveness vs IAEA verification mechanisms vs OPCW inspections. Longitudinal: measure NWS arsenal compliance with explicit disarmament schedules agreed at Review Conferences.',
    'If functional: NNWS organized coalition gains leverage, classification toward Rope. If performative: Review Conference is Piton, and NNWS experience closer to Snare (constraint theater without power).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(review_conference_enforcement_mechanism, empirical, 'Whether Review Conference provides functional enforcement').

omega_variable(
    tpnw_regime_competition_sunset_logic,
    'Does TPNW regime competition create structural pressure that will eventually force NPT reform, or does TPNW remain a parallel symbolic system while NPT enforcement stalls indefinitely?',
    'Track NWS response to TPNW accession thresholds; monitor whether TPNW adhesion affects NWS strategic calculations or triggers NPT protocol amendments; assess whether reputational cost of TPNW exclusion rises with membership growth; measure whether TPNW regime competition catalyzes NPT enforcement upgrades.',
    'If TPNW drives reform: Scaffold classification confirmed, sunset is real. If TPNW remains symbolic: Scaffold is aspirational, and the constraint remains Tangled Rope indefinitely with minimal enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tpnw_regime_competition_sunset_logic, empirical, 'Whether TPNW competition forces NPT reform').

omega_variable(
    kernel_interpretation_uncertainty,
    'This constraint is one reading of the NPT Article VI kernel. Do the NWS reading and NNWS reading represent genuinely incompatible interpretations of the treaty text, or are they compatible readings applied to different compliance phases?',
    'Textual analysis: can both readings find grammatical/logical support in Article VI wording? Historical analysis: did drafters intend one reading to foreclose the other? Institutional analysis: do NWS and NNWS act as if their readings are incompatible (treaty violation claims, reciprocal non-compliance) or compatible (tolerance of divergent timelines)?',
    'If incompatible: one reading forecloses the other (engine computes via cs_structure.reading_relations = forecloses). If compatible: readings coexist within different frameworks (coexists_with), and the constraint''s extractiveness is the cost of managing this coexistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_interpretation_uncertainty, conceptual, 'Whether Article VI readings are incompatible or compatible').

omega_variable(
    nws_compliance_ambiguity,
    'How much of NWS compliance with Article VI disarmament language is genuine intent vs. pure performance to sustain NPT legitimacy?',
    'Compare stated disarmament goals against actual arsenal trajectories; assess strategic rationale for modernization (are NWS modernizing because deterrence is ''permanent'' or because geopolitical windows permit it?); examine confidential strategic documents if declassified; track whether NWS support concrete verification mechanisms or only rhetorical commitments.',
    'If genuine: NNWS reading captures real constraint and coordination. If performative: NWS reading (Piton/Rope framing) is more accurate, and NNWS experience is closer to Snare than Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nws_compliance_ambiguity, empirical, 'Extent of NWS genuine compliance vs. performance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_text__nnws_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt_nnws_tr_t0, npt_treaty_text__nnws_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(npt_nnws_tr_t10, npt_treaty_text__nnws_reading, theater_ratio, 10, 0.58).
narrative_ontology:measurement(npt_nnws_tr_t20, npt_treaty_text__nnws_reading, theater_ratio, 20, 0.61).

% Extraction over time
narrative_ontology:measurement(npt_nnws_be_t0, npt_treaty_text__nnws_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(npt_nnws_be_t10, npt_treaty_text__nnws_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(npt_nnws_be_t20, npt_treaty_text__nnws_reading, base_extractiveness, 20, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(npt_nnws_su_t0, npt_treaty_text__nnws_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(npt_nnws_su_t10, npt_treaty_text__nnws_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(npt_nnws_su_t20, npt_treaty_text__nnws_reading, suppression_requirement, 20, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_text__nnws_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_treaty_text__nnws_reading, npt_treaty_text__nws_reading).
narrative_ontology:affects_constraint(npt_treaty_text__nnws_reading, npt_treaty_text__withdrawal_threshold_reading).
narrative_ontology:affects_constraint(npt_treaty_text__nnws_reading, tpnw_regime_legitimacy).
narrative_ontology:affects_constraint(npt_treaty_text__nnws_reading, nuclear_export_control_regime).

% DUAL FORMULATION NOTE:
% The NPT Article VI kernel generates three constraint stories with different ε values, reflecting different readings of treaty interpretation. The NNWS reading (this story, ε≈0.38) treats Article VI as binding with Review Conference enforcement. The NWS reading (separate story, ε≈0.20) treats Article VI as aspirational. The withdrawal-threshold reading (separate story) models Article VI's bindingness as conditional on non-cascade. All three are instantiations of the same kernel text; they differ in how they interpret the text's legal force and enforcement mechanisms. The ε values differ because the observable (measured enforcement, measured compliance, measured bindingness) changes across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(npt_treaty_text__nnws_reading, institutional, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
