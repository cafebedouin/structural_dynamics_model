% ============================================================================
% CONSTRAINT STORY: westphalia_sovereignty__conditional_responsibility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalia_sovereignty__conditional_responsibility, []).

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
 *   constraint_id: westphalia_sovereignty__conditional_responsibility
 *   human_readable: Conditional Sovereignty: State Legitimacy Forfeiture via Mass Atrocity
 *   domain: international_law/political_theory/state_systems
 *
 * SUMMARY:
 *   Conditional sovereignty—the doctrine that state territorial inviolability
 *   is forfeited when states systematically fail to protect populations from
 *   mass atrocities—represents one reading of a contested kernel in
 *   international law: what does the Westphalian state system's foundational
 *   commitment to sovereignty actually protect, and under what conditions can
 *   that commitment be overridden? This reading emerged forcefully in the
 *   1990s (Bosnia, Rwanda) and was formally codified as the Responsibility to
 *   Protect (R2P) doctrine in 2005. It asserts that while state sovereignty
 *   is a foundational principle, it is not absolute: a state's claim to
 *   non-interference depends on its meeting a responsibility to protect its
 *   own population from genocide, war crimes, crimes against humanity, and
 *   ethnic cleansing. This reading benefits humanitarian intervention
 *   coalitions and global governance institutions (UN, ICC, international
 *   legal authority) by creating a doctrine that legitimates their authority
 *   to adjudicate state legitimacy and authorize enforcement. It extracts
 *   from atrocity victims by promising protection while maintaining
 *   selectivity of enforcement—victims only receive international
 *   intervention if geopolitical interests align. It suppresses the
 *   alternative reading (absolute non-intervention) by claiming that the
 *   moral imperative to prevent atrocities overrides state sovereignty, while
 *   simultaneously coexisting with that alternative reading in the practice
 *   of states that reject R2P doctrine. The constraint's theater ratio (0.65)
 *   reflects the substantial performative content: extensive legal
 *   proceedings at the ICC, UN Security Council resolutions, International
 *   Court of Justice opinions, and treaty negotiations occur alongside
 *   selective enforcement that tracks great-power interests rather than
 *   atrocity severity.
 *
 * KEY AGENTS:
 *   - Atrocity victim populations: Primary victims (powerless/trapped) — jurisdictionally confined, experience conditional sovereignty as failed protection mechanism; zero agency in intervention decisions
 *   - Non-intervening states: Secondary victims and participants (moderate/constrained) — constrained by military and political capacity; bear reputational and legal pressure regardless of intervention choice
 *   - Humanitarian intervention coalitions (Western states, UN bodies, NGO networks): Primary beneficiaries (institutional/arbitrage) — derive legitimacy and authority from conditional sovereignty doctrine; benefit from selective enforcement
 *   - Global governance institutions (ICC, International Court of Justice, UN treaty organs): Primary beneficiaries (institutional/arbitrage) — expanded jurisdiction and legitimacy through conditional sovereignty framework; arbiter role in sovereignty forfeiture determination
 *   - Atrocity perpetrator states: Intermediate victims and strategic actors (powerful/mobile) — face enforcement risk and delegitimation but retain bargaining power through patron relationships; experience constraint as asymmetric (enforcement selectively applied)
 *   - R2P reformers and human rights coalitions: Organized beneficiaries (organized/mobile) — advocacy-driven actors who see conditional sovereignty as transitional bridge toward genuine protection capacity
 *   - UN system and international legal apparatus: Institutional maintainers (institutional/arbitrage) — theater-dominated actors maintaining enforcement machinery despite selective practice; see own system as degraded
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalia_sovereignty__conditional_responsibility, 0.58).
domain_priors:suppression_score(westphalia_sovereignty__conditional_responsibility, 0.68).
domain_priors:theater_ratio(westphalia_sovereignty__conditional_responsibility, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, extractiveness, 0.58).
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalia_sovereignty__conditional_responsibility, tangled_rope).
narrative_ontology:human_readable(westphalia_sovereignty__conditional_responsibility, "Conditional Sovereignty: State Legitimacy Forfeiture via Mass Atrocity").
narrative_ontology:topic_domain(westphalia_sovereignty__conditional_responsibility, "international_law/political_theory/state_systems").

domain_priors:requires_active_enforcement(westphalia_sovereignty__conditional_responsibility).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalia_sovereignty__conditional_responsibility, '264dacff-52ae-4311-88f5-e4b317d2174b').
narrative_ontology:cs_kernel_codification('264dacff-52ae-4311-88f5-e4b317d2174b', formalized).
narrative_ontology:cs_authority_grounding('264dacff-52ae-4311-88f5-e4b317d2174b', extraction).
narrative_ontology:cs_interpretation_layer_present('264dacff-52ae-4311-88f5-e4b317d2174b').
narrative_ontology:cs_reading_relation('264dacff-52ae-4311-88f5-e4b317d2174b', westphalia_sovereignty__absolute_non_intervention, forecloses).
narrative_ontology:cs_reading_relation('264dacff-52ae-4311-88f5-e4b317d2174b', westphalia_sovereignty__graded_sovereignty, coexists_with).
narrative_ontology:cs_axiom('264dacff-52ae-4311-88f5-e4b317d2174b', foundational, sovereignty_conditioned_on_protection).
narrative_ontology:cs_axiom_status(sovereignty_conditioned_on_protection, holdable).
narrative_ontology:cs_axiom_grounding('264dacff-52ae-4311-88f5-e4b317d2174b', sovereignty_conditioned_on_protection, deontological).
narrative_ontology:cs_axiom('264dacff-52ae-4311-88f5-e4b317d2174b', foundational, international_community_adjudicative_authority).
narrative_ontology:cs_axiom_status(international_community_adjudicative_authority, holdable).
narrative_ontology:cs_axiom_grounding('264dacff-52ae-4311-88f5-e4b317d2174b', international_community_adjudicative_authority, conventional).
narrative_ontology:cs_reference_frame('264dacff-52ae-4311-88f5-e4b317d2174b', post_cold_war_humanitarian_consensus).
narrative_ontology:cs_drift_state('264dacff-52ae-4311-88f5-e4b317d2174b', contemporary_great_power_realignment, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('264dacff-52ae-4311-88f5-e4b317d2174b', '').
narrative_ontology:cs_kernel_id(westphalia_sovereignty__conditional_responsibility, westphalia_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__conditional_responsibility, humanitarian_intervention_coalitions).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__conditional_responsibility, global_governance_institutions).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__conditional_responsibility, international_legal_authority).
narrative_ontology:constraint_victim(westphalia_sovereignty__conditional_responsibility, atrocity_victim_populations).
narrative_ontology:constraint_victim(westphalia_sovereignty__conditional_responsibility, non_intervening_states).
narrative_ontology:constraint_victim(westphalia_sovereignty__conditional_responsibility, state_sovereignty_principle).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ATROCITY VICTIM POPULATION (SNARE) — Trapped within territorial borders; cannot exit the jurisdiction where atrocities occur. International community's claim that sovereignty is conditional provides no material protection when enforcement is absent or selective. Maximum extraction: victim bears full cost of state failure with no recourse. Suppression is total — victimized population has zero agency in triggering intervention.
constraint_indexing:constraint_classification(westphalia_sovereignty__conditional_responsibility, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: NON-INTERVENING STATE (TANGLED ROPE) — Constrained by military capacity, political will, and costs of intervention. Yet also benefits from the coordination function of conditional sovereignty: the principle legitimates international legal frameworks that the state itself relies upon (trade agreements, investment treaties, territorial recognition). Asymmetric extraction: state bears intervention burden and reputational risk if it acts; bears reputational and legal pressure if it does not. Genuine coordination (collective security norms) layered with genuine extraction (selective enforcement).
constraint_indexing:constraint_classification(westphalia_sovereignty__conditional_responsibility, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: HUMANITARIAN INTERVENTION COALITION (ROPE) — Institutional actors (Western states, UN bodies, NGO networks) benefit from conditional sovereignty framing: it legitimates intervention authority while obscuring the causal asymmetry of enforcement (powerful states intervene selectively; weak states cannot intervene at all). Low extraction experienced because beneficiaries have exit options (diplomatic arbitrage, withdrawal from coalitions, selective enforcement). Net coordination function: enables collective legitimation of intervention.
constraint_indexing:constraint_classification(westphalia_sovereignty__conditional_responsibility, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: RESPONSIBILITY TO PROTECT (R2P) REFORMERS (SCAFFOLD) — Organized agents (human rights coalitions, progressive international legal scholars, ICC advocates) see conditional sovereignty as a transitional bridge toward genuine international protection mechanisms. R2P doctrine includes sunset logic: as regional peacekeeping capacity, early warning systems, and international enforcement architecture mature, the need for unilateral intervention by powerful states should decline. Mobile exit: reformers can shift advocacy toward capacity-building institutions. Low effective extraction because advocates have agency and perceive progress.
constraint_indexing:constraint_classification(westphalia_sovereignty__conditional_responsibility, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: UN SYSTEM / INTERNATIONAL LAW APPARATUS (PITON) — The institutional machinery of conditional sovereignty (UN Security Council, ICC, International Court of Justice, treaty organs) persists through inertia despite systematic performance failure. Theater ratio high (0.65): extensive legal proceedings, treaty negotiations, and doctrine development obscure the reality that enforcement is selective, deterrence is weak, and victim protection remains contingent on major-power interests. The institutional actors (judges, diplomats, legal scholars) see their own system as degraded — maintained because alternatives have not yet fully replaced it, not because it functions.
constraint_indexing:constraint_classification(westphalia_sovereignty__conditional_responsibility, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ATROCITY PERPETRATOR STATE (TANGLED ROPE) — The state committing atrocities experiences conditional sovereignty as genuine coordination (international norms that govern all states) coupled with asymmetric extraction (enforcement against itself but not against allies). The state has mobile exit options (strategic realignment, patron acquisition, forum shopping in weaker legal venues). Intermediate extraction — not maximum (state retains some autonomy and bargaining power) but substantial (faces delegitimation and intervention risk).
constraint_indexing:constraint_classification(westphalia_sovereignty__conditional_responsibility, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational vantage, conditional sovereignty appears as an immutable feature of anarchic international systems: any system that permits mass atrocities must produce mechanisms (humanitarian intervention, conditional recognition) to suppress such atrocities, or the system fails. This perspective naturalizes conditional sovereignty as an inevitable structural response. However, the presence of beneficiaries (humanitarian coalitions, global institutions) and the selective enforcement pattern indicate this is a false summit — what appears to be a law of state systems is actually a contingent institutional arrangement that benefits identifiable agents.
constraint_indexing:constraint_classification(westphalia_sovereignty__conditional_responsibility, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalia_sovereignty__conditional_responsibility_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(westphalia_sovereignty__conditional_responsibility, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(westphalia_sovereignty__conditional_responsibility, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalia_sovereignty__conditional_responsibility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(westphalia_sovereignty__conditional_responsibility, TR),
    TR >= 0.70.

:- end_tests(westphalia_sovereignty__conditional_responsibility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. The constraint creates a coordination function (all states accept that atrocities trigger legitimate international response) layered with significant extraction (enforcement benefits powerful intervening states while leaving weaker states vulnerable to selective intervention or non-intervention). The extractiveness has risen over the interval (0.38 → 0.58) as the doctrine has gained institutional embedment and as enforcement patterns have revealed that selectivity is systematic rather than anomalous. The rise reflects accumulating evidence that conditional sovereignty doctrine is weaponized (interventions follow great-power interests rather than atrocity severity). Suppression (0.68): Moderately high. Multiple barriers prevent victims from obtaining intervention: geopolitical disinterest, veto power of permanent Security Council members, military capacity constraints, and the framing device that sovereignty is conditional on protection (if you claim sovereignty, you accept intervention risk—a false choice for weak states that cannot prevent atrocities). Theater ratio (0.65): Moderately high. Substantial institutional apparatus (ICC indictments, International Court cases, treaty negotiations, doctrine development) operates alongside weak enforcement. The theater is partially acknowledged (states recognize ICC as performative, Security Council as gridlocked) but persists because it provides legitimacy cover for selective enforcement and because alternatives (regional peacekeeping, early warning systems) are underfunded.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between victim and beneficiary perspectives is maximal. Atrocity victims experience conditional sovereignty as pure extraction (Snare): the doctrine promises protection but provides selective enforcement, leaving victims hostage to geopolitical coincidence. Humanitarian intervention coalitions experience it as coordination (Rope): the doctrine legitimates collective action and enables them to frame intervention as principled rather than interest-driven. Non-intervening states experience the constraint as mixed (Tangled Rope): genuine coordination function (all states accept the principle) layered with asymmetric extraction (enforcement risks fall unequally). The R2P reformers see it as transitional (Scaffold): a bridge toward genuine protection institutions, with sunset logic as regional capacity matures. The perpetrator state experiences it as constrained extraction (Tangled Rope): the constraint legitimates intervention against itself but not against allies. The UN apparatus sees its own degradation (Piton): theater-dominated performance of enforcement while selectivity tracks interests. The civilizational analytical observer risks naturalizing the constraint as inherent to state systems (Mountain)—but the declared beneficiaries and asymmetric enforcement pattern reveal it as a false summit, naturalizing what is actually a contingent institutional arrangement that concentrates authority in powerful states and international bodies.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from each agent's structural position: are they beneficiary or victim, and what are their exit options? Atrocity victims are trapped (no exit), so d is near 1.0, producing maximum experienced extractiveness even though base extractiveness is moderate — the trapped exit amplifies the constraint's felt burden. Intervention coalitions have arbitrage options (choose when/where to intervene, withdraw from coalitions, shift to alternative doctrines like sovereignty as absolute), so d is near 0.0, producing negative or minimal effective extraction for beneficiaries. Non-intervening states have constrained exit (cannot avoid the legal/reputational burden, but can strategically align to reduce pressure), so d is intermediate (~0.55-0.65). Perpetrator states have mobile exit (patron acquisition, forum shopping, geopolitical realignment), so experienced extraction is intermediate despite high baseline suppression. The analytical observer's canonical d (~0.73) reflects the standard analytical position—observing the constraint from outside its mechanisms. The false summit detection fires because beneficiaries are declared (humanitarian coalitions, global institutions) on what the mountain perspective presents as a natural law—the moral imperative to prevent atrocities. The engine's FSM signature detects that this 'law of the system' benefits identifiable agents and produces an override toward tangled rope, revealing the constraint as constructed.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by distinguishing genuine coordination (all states accept that atrocities are a legitimate concern for the international community) from extractive asymmetry (enforcement benefits powerful states and international institutions while leaving victims dependent on geopolitical alignment). The constraint is genuinely Tangled Rope at the system level: it coordinates a collective commitment to prevent atrocities while simultaneously extracting authority and legitimacy for those doing the enforcing. The snare classification at the victim perspective reflects that victims cannot exit the constraint's jurisdiction and cannot trigger its protection function. The rope classification at the beneficiary perspective reflects that intervention coalitions experience the doctrine as enabling legitimate coordination. The mountain classification at the analytical perspective is a false summit diagnosis: what appears to be a law of state systems is a contingent arrangement. The mandatrophy is resolved by recognizing that the constraint operates across multiple levels simultaneously—a genuine coordination problem (atrocity prevention) that has been institutionalized in a way that concentrates authority asymmetrically.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    atrocity_threshold_definition,
    'What atrocity severity threshold triggers forfeiture of sovereignty protection? (e.g., death toll, population percentage, intent classification)',
    'Comparative case law analysis: identify which atrocities triggered intervention (Kosovo, Rwanda, Syria) and which did not (Myanmar, South Sudan, etc.). Quantify the threshold through systematic review of intervention decisions.',
    'If threshold is high (death toll > 100k): intervention rare, most atrocities unpunished, conditional sovereignty operates as snare. If threshold is low (organized civilian targeting): intervention frequent and selective, conditional sovereignty benefits powerful interveners (tangled rope for coalitions). If threshold is undefined: theater ratio dominates — legal process continues while enforcement remains contingent on geopolitical interest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(atrocity_threshold_definition, empirical, 'Operational atrocity threshold for triggering sovereignty forfeiture').

omega_variable(
    enforcement_selectivity_mechanism,
    'Is selective enforcement of conditional sovereignty a bug (institutional failure) or a feature (designed flexibility)?',
    'Historical analysis of enforcement patterns: correlate intervention decisions with intervening state interests (economic, geopolitical, military). Identify whether selection criteria are transparent and applied consistently or opaque and interest-driven. Compare stated doctrine (condition applies universally) with practice (enforcement concentrated on weaker states and ideological adversaries).',
    'If bug: conditional sovereignty is aspirational doctrine; actual mechanism is geopolitical snare for weak states. Reclassify victim and beneficiary relationships. If feature: conditional sovereignty is intentionally flexible institution; classification remains but theater ratio may be lower (acknowledged theater is not deception). If mixed: institutional reform is both possible (bug-fixing path) and blocked by benefits to powerful actors (feature-defending path).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_selectivity_mechanism, empirical, 'Whether selective enforcement of conditional sovereignty is unintended or designed').

omega_variable(
    alternative_sovereignty_models_foreclosure,
    'Does the conditional responsibility reading logically foreclose the absolute non-intervention reading, or do both remain live options for different state coalitions?',
    'Analyze the axioms: conditional responsibility asserts that sovereignty is not absolute — it can be forfeited via atrocity. Non-intervention asserts that state sovereignty is inviolable regardless of internal governance. These premises directly contradict each other within a single unified international legal framework. However, in practice, different states and coalitions hold different positions simultaneously. The question is whether one framework MUST rule out the other, or whether the international system permits both.',
    'If forecloses: conditional responsibility and absolute non-intervention cannot coexist in a coherent legal order. The manifest split in state practice indicates a legitimacy crisis or system transition. If coexists_with: the international system permits multiple sovereignties operating under different rule sets simultaneously — a distributed authority structure. This has profound implications for the reliability of conditional sovereignty as a binding constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_sovereignty_models_foreclosure, conceptual, 'Whether conditional responsibility reading forecloses absolute non-intervention or both remain live').

omega_variable(
    victim_set_composition_ambiguity,
    'Are atrocity victim populations direct victims of the conditional sovereignty constraint, or are they victims of the atrocity-perpetrator state with conditional sovereignty as an attempted remedy?',
    'Structural analysis: if conditional sovereignty enforcement reliably prevented atrocities, victims would benefit from the constraint (low d, beneficiary status). Since enforcement is selective and often fails, victims experience the constraint''s failure to protect as itself extractive. But the constraint''s existence creates the legal category ''victim'' that enables victim advocacy. Distinguish the constraint''s protective function (when it works) from its failure mode (when it doesn''t).',
    'If victims are primary: conditional sovereignty is a snare by design (extraction masquerading as protection). If victims are secondary (indirect beneficiaries of the constraint''s existence even when enforcement fails): classification becomes more nuanced — the constraint provides a voice and legitimacy to victim advocacy, even if material protection remains absent. This affects how we weight the victim group in chi calculations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_composition_ambiguity, conceptual, 'Whether atrocity victims are direct victims of the sovereignty constraint or of state failure with conditional sovereignty as remedy').

omega_variable(
    reading_contest_emergence,
    'Which structural conditions determine whether conditional responsibility, absolute non-intervention, or graded sovereignty becomes the dominant reading in interstate practice?',
    'Historical comparison: map periods when each reading dominated (e.g., conditional responsibility rose after Cold War''s end; absolute non-intervention reasserted during Russian/Chinese resurgence; graded sovereignty in EU/regional integration contexts). Identify causal factors: material capacity of powerful states, ideological alignment, victim mobilization, costs of enforcement, legitimacy crises in intervening institutions.',
    'If emergence is path-dependent: current dominance of conditional responsibility is contingent on post-Cold War power distribution and liberal institutional momentum. Shift in great-power alignment or costs of enforcement could flip the reading. If emergence tracks material interests: the reading that survives is the one that benefits dominant powers. R2P doctrine emerged when Western military dominance was unchallenged; it receded when enforcement costs rose (Iraq, Libya). This informs long-term stability of the current reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_emergence, empirical, 'Structural conditions determining dominance of conditional responsibility reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalia_sovereignty__conditional_responsibility, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wsov_tr_t0, westphalia_sovereignty__conditional_responsibility, theater_ratio, 0, 0.52).
narrative_ontology:measurement(wsov_tr_t5, westphalia_sovereignty__conditional_responsibility, theater_ratio, 5, 0.6).
narrative_ontology:measurement(wsov_tr_t10, westphalia_sovereignty__conditional_responsibility, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(wsov_be_t0, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(wsov_be_t5, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(wsov_be_t10, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalia_sovereignty__conditional_responsibility, enforcement_mechanism).
narrative_ontology:affects_constraint(westphalia_sovereignty__conditional_responsibility, westphalia_sovereignty__absolute_non_intervention).
narrative_ontology:affects_constraint(westphalia_sovereignty__conditional_responsibility, westphalia_sovereignty__graded_sovereignty).
narrative_ontology:affects_constraint(westphalia_sovereignty__conditional_responsibility, humanitarian_intervention_selectivity).
narrative_ontology:affects_constraint(westphalia_sovereignty__conditional_responsibility, international_court_enforcement_asymmetry).

% DUAL FORMULATION NOTE:
% Conditional_responsibility reading is part of the westphalia_sovereignty kernel constraint family. Sibling readings (absolute_non_intervention, graded_sovereignty) should be authored as separate constraint stories with their own ε values, beneficiary/victim structures, and measurement trajectories. Each reading has different empirical status and institutional embedding. Link all members via network.affects_constraints for contamination propagation analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(westphalia_sovereignty__conditional_responsibility, powerful, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
