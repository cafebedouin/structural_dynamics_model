% ============================================================================
% CONSTRAINT STORY: westphalian_sovereignty__conditional_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalian_sovereignty__conditional_sovereignty, []).

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
 *   constraint_id: westphalian_sovereignty__conditional_sovereignty
 *   human_readable: Conditional Sovereignty: Human Rights Threshold Triggering Legitimate Intervention
 *   domain: international_law/political_philosophy
 *
 * SUMMARY:
 *   The conditional sovereignty reading of Westphalian sovereignty asserts
 *   that state sovereignty is not absolute but conditional on respect for
 *   human rights; systematic violations trigger legitimate external
 *   intervention. This constraint exhibits a classic tangled_rope structure:
 *   genuine coordination problem (preventing atrocities requires collective
 *   enforcement) embedded within asymmetric extraction (intervening
 *   coalitions capture political advantage, non-intervening states are
 *   subordinated, persecuted populations remain dependent on great-power
 *   whim). The constraint generates perspectival disagreement across the full
 *   spectrum of DR types: intervening powers see coordination (rope),
 *   persecuted populations see pure extraction (snare), the UN framework sees
 *   ritual theater (piton), and realist observers risk naturalizing what may
 *   be contingent institutional arrangements (false-summit mountain). Theater
 *   ratio (0.48, rising to 0.55) reflects increasing invocation of the R2P
 *   doctrine in rhetoric without proportional consistency in application.
 *   Extractiveness (0.38, rising) reflects the growing use of humanitarian
 *   language to justify geopolitically motivated interventions. Suppression
 *   (0.52) reflects that non-intervening states, persecuted populations, and
 *   advocacy coalitions all face significant barriers to voice in threshold
 *   determination and enforcement decisions.
 *
 * KEY AGENTS:
 *   - Intervening State/Coalition: Primary beneficiary (powerful/constrained) — captures military advantage, geopolitical repositioning, humanitarian legitimacy; constrained by international law and domestic costs
 *   - Persecuted Population: Primary victim (powerless/trapped) — structurally locked within violating state's jurisdiction; no exit except through intervention; dependent on great-power calculations
 *   - Non-Intervening State: Secondary victim (institutional/constrained) — nominally bound by threshold but subordinated in enforcement decisions; constrained by veto power dynamics and economic dependency
 *   - International Humanitarian Regime: Institutional actor (institutional/arbitrage) — coordinates genuine atrocity-prevention function; benefits from legitimacy and institutional resources; arbitrage access to selective threshold invocation
 *   - Rights Advocacy Coalition: Organized actor (organized/mobile) — sees constraint as soluble through institutional development; treats intervention threshold as temporary problem with sunset as courts mature
 *   - Persecuted Population within Intervened State: Constrained agent (moderate/constrained) — benefits from intervention but treated as object not subject; no voice in decision-making; dependent on post-intervention support
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing strategic state behavior as immutable anarchy; may misclassify contingent institutions as natural law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalian_sovereignty__conditional_sovereignty, 0.38).
domain_priors:suppression_score(westphalian_sovereignty__conditional_sovereignty, 0.52).
domain_priors:theater_ratio(westphalian_sovereignty__conditional_sovereignty, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, extractiveness, 0.38).
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(westphalian_sovereignty__conditional_sovereignty, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalian_sovereignty__conditional_sovereignty, tangled_rope).
narrative_ontology:human_readable(westphalian_sovereignty__conditional_sovereignty, "Conditional Sovereignty: Human Rights Threshold Triggering Legitimate Intervention").
narrative_ontology:topic_domain(westphalian_sovereignty__conditional_sovereignty, "international_law/political_philosophy").

domain_priors:requires_active_enforcement(westphalian_sovereignty__conditional_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalian_sovereignty__conditional_sovereignty, 'ea51ca68-13a4-4417-84fc-82d6b9d24c7b').
narrative_ontology:cs_kernel_codification('ea51ca68-13a4-4417-84fc-82d6b9d24c7b', formalized).
narrative_ontology:cs_authority_grounding('ea51ca68-13a4-4417-84fc-82d6b9d24c7b', lineage).
narrative_ontology:cs_interpretation_layer_present('ea51ca68-13a4-4417-84fc-82d6b9d24c7b').
narrative_ontology:cs_reading_relation('ea51ca68-13a4-4417-84fc-82d6b9d24c7b', westphalian_sovereignty__absolute_sovereignty, coexists_with).
narrative_ontology:cs_reading_relation('ea51ca68-13a4-4417-84fc-82d6b9d24c7b', westphalian_sovereignty__graduated_sovereignty, influences).
narrative_ontology:cs_axiom('ea51ca68-13a4-4417-84fc-82d6b9d24c7b', foundational, human_rights_override_territorial_sovereignty).
narrative_ontology:cs_axiom_status(human_rights_override_territorial_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('ea51ca68-13a4-4417-84fc-82d6b9d24c7b', human_rights_override_territorial_sovereignty, deontological).
narrative_ontology:cs_axiom('ea51ca68-13a4-4417-84fc-82d6b9d24c7b', foundational, legitimate_external_enforcement_mechanism).
narrative_ontology:cs_axiom_status(legitimate_external_enforcement_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('ea51ca68-13a4-4417-84fc-82d6b9d24c7b', legitimate_external_enforcement_mechanism, conventional).
narrative_ontology:cs_reference_frame('ea51ca68-13a4-4417-84fc-82d6b9d24c7b', state_sovereignty_absolute_territorial_integrity).
narrative_ontology:cs_drift_state('ea51ca68-13a4-4417-84fc-82d6b9d24c7b', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ea51ca68-13a4-4417-84fc-82d6b9d24c7b', '').
narrative_ontology:cs_kernel_id(westphalian_sovereignty__conditional_sovereignty, westphalian_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__conditional_sovereignty, intervention_coalition).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__conditional_sovereignty, persecuted_populations).
narrative_ontology:constraint_victim(westphalian_sovereignty__conditional_sovereignty, state_autonomy).
narrative_ontology:constraint_victim(westphalian_sovereignty__conditional_sovereignty, non_intervening_states).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PERSECUTED POPULATION (SNARE) — Structurally trapped within the jurisdiction of the violating state. No exit available except through intervention; cannot organize effective external pressure. Bears the full cost of the sovereignty constraint while being framed as its beneficiary ('protected by intervention'). Intervention rhetoric extracts political benefit for interveners while the persecuted remain locked in structural dependency on great-power geopolitical calculations.
constraint_indexing:constraint_classification(westphalian_sovereignty__conditional_sovereignty, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INTERVENING STATE/COALITION (TANGLED ROPE) — Faces genuine coordination problem (preventing atrocities requires collective commitment to intervention threshold) but also captures asymmetric extraction (military advantage, geopolitical repositioning, humanitarian legitimacy). Constrained by international law, domestic politics, and resource costs; benefits from moral authority and strategic positioning. Mixed coordination-extraction structure.
constraint_indexing:constraint_classification(westphalian_sovereignty__conditional_sovereignty, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: INTERNATIONAL HUMANITARIAN REGIME (ROPE) — Coordinates genuine collective action problem: preventing systematic atrocities requires shared commitment to norms and enforcement mechanisms. UN human rights bodies, ICC, regional courts benefit from legitimacy and institutional resources. Arbitrage access: can selectively invoke the threshold, timing intervention for strategic advantage. Net coordination benefit — extraction remains within bounds of legitimate cost.
constraint_indexing:constraint_classification(westphalian_sovereignty__conditional_sovereignty, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: NON-INTERVENING STATE (TANGLED ROPE) — Nominally bound by the human rights threshold but structurally constrained by great-power veto (UN Security Council), regional power dynamics, and economic dependency. Experiences both coordination benefit (shared norms reduce arbitrary state violence) and asymmetric extraction (subordinated in intervention decisions, no voice in threshold enforcement). Constrained by institutional architecture, not by material barriers.
constraint_indexing:constraint_classification(westphalian_sovereignty__conditional_sovereignty, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: RIGHTS ADVOCACY COALITION (SCAFFOLD) — Organized civil society (Human Rights Watch, Amnesty, MSF) treats the sovereignty threshold as temporary coordination failure with a sunset: as international courts mature and norm-building continues, the need for unilateral intervention should decline. Theater is low (advocacy work is functional, not performative). Mobile exit: coalition can shift strategies as institutional capacity grows. Sees the constraint as soluble through institutional development.
constraint_indexing:constraint_classification(westphalian_sovereignty__conditional_sovereignty, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / REALIST NATURAL LAW (MOUNTAIN) — From a civilizational realist view, state sovereignty is immutable: intervention always masks self-interest, the human rights threshold is performative cover for power politics, and the constraint reflects the irreducible anarchy of the international system. No state can credibly commit to intervention without strategic payoff; the norms are theater masking competitive behavior. This perspective naturalizes what may be a contingent institutional arrangement.
constraint_indexing:constraint_classification(westphalian_sovereignty__conditional_sovereignty, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: UN FRAMEWORK (PITON) — The formal R2P (Responsibility to Protect) doctrine emerged post-2005 but has functioned increasingly as ritual: invoked selectively based on great-power interests, applied inconsistently across cases (Syria vs Libya vs Myanmar), and often subordinated to Security Council veto dynamics. Theater ratio is high (0.55+) because the invocation of the doctrine has become decoupled from actual intervention patterns. The framework persists through institutional inertia and legitimacy claims despite low predictive power.
constraint_indexing:constraint_classification(westphalian_sovereignty__conditional_sovereignty, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalian_sovereignty__conditional_sovereignty_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(westphalian_sovereignty__conditional_sovereignty, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(westphalian_sovereignty__conditional_sovereignty, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(westphalian_sovereignty__conditional_sovereignty, TR),
    TR >= 0.70.

:- end_tests(westphalian_sovereignty__conditional_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38, rising to 0.40 at t=10): Moderate. The conditional sovereignty reading does solve a genuine coordination problem (preventing systematic atrocities requires enforcement commitment). However, the reading also enables asymmetric extraction: intervening coalitions benefit from military repositioning, geopolitical advantage, and humanitarian legitimacy; non-intervening states are subordinated; persecuted populations remain dependent. The moderate value reflects that the coordination function is real (not pure snare at 0.66+) but the extraction is structural and not incidental (not pure rope at ≤0.35). Suppression (0.52, stable): Moderate-high. Non-intervening states face veto dynamics and institutional subordination. Persecuted populations face barriers to voice in planning and post-intervention support. The threshold itself is discretionary, enabling selective application. Theater ratio (0.48, rising to 0.55): Moderate, rising. The R2P doctrine (formally adopted 2005) has become increasingly invoked in rhetoric (theater rising) while actual intervention patterns remain driven by great-power interest rather than threshold consistency (functional content declining). Theater is moderate rather than high (0.70+) because the underlying coordination problem is genuine — some atrocity prevention is actually occurring — but the ritual elements are growing.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximal perspectival divergence. The persecuted population experiences pure extraction (snare) — trapped without voice. The intervening coalition experiences coordination with moderate extraction (tangled_rope) — solving real problems while capturing benefits. The international humanitarian regime experiences functional coordination with arbitrage opportunities (rope) — legitimate benefit-taking. Non-intervening states experience constrained participation (tangled_rope) — benefiting from shared norms but subordinated in enforcement. Rights advocates see a soluble problem with a sunset (scaffold) — institutional maturation will reduce intervention need. The UN framework itself appears as degraded ritual (piton) — high theater, declining functional fit. The realist observer risks a false summit (mountain) — naturalizing what may be contingent state behavior as immutable anarchy. The perspectival spread (snare to rope to piton to mountain) reveals that the 'conditional sovereignty' reading embeds fundamentally contested claims about state behavior, humanitarian obligation, and institutional evolution. No single perspective is obviously correct; the range indicates deep structural disagreement.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective flows from structural position relative to the extraction flow. The persecuted population is a pure victim with no exit (d ≈ 0.95, trapped) → high f(d) → high experienced extractiveness. The intervening coalition is a beneficiary with constrained mobility (d ≈ 0.40, powerful/constrained) → moderate f(d) → moderate extraction. The international regime is a beneficiary with arbitrage options (d ≈ 0.05, institutional/arbitrage) → negative f(d) → beneficiary extraction absorption. Non-intervening states are mixed (some coordination benefit, some subordination; d ≈ 0.60, institutional/constrained) → moderate f(d). The canonical derivation produces these d values from power+exit+beneficiary/victim declarations; no overrides needed. The perspectival gap arises because d varies substantially across positions while the same constraint base properties apply to all.
 *
 * MANDATROPHY ANALYSIS:
 *   The conditional sovereignty reading resolves mandatrophy by showing that all disagreement occurs within a single consistent structural description: a tangled_rope with extraction (0.38) and genuine coordination (beneficiaries explicitly declared). The tangled_rope gates are satisfied: requires_active_enforcement (true — UN institutions actively determine threshold and coordinate responses); beneficiaries present (intervention_coalition, persecuted_populations through atrocity prevention); victims present (state_autonomy, non_intervening_states through subordination). The perspectival range (snare to rope to piton) does not reflect different types; it reflects the same constraint experienced differently from different structural positions. The piton perspective (UN framework) is the constraint-implementation ritual degrading over time; the mountain perspective (realist natural law) is the risk of false summit — naturalizing contingent arrangements. The mandatrophy is resolved by the tangled_rope type itself: it is precisely the constraint that mixes coordination-with-extraction, generating disagreement across positions about whether the coordination or extraction is primary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    atrocity_threshold_definition,
    'What level and type of human rights violations constitute ''systematic'' violations that trigger legitimate intervention? Who determines the threshold operationally?',
    'Comparative case analysis of invoked vs. non-invoked interventions (Syria, Libya, Myanmar, Rohingya); cross-case consistency coding; measurement of threshold application variance by intervening coalition composition',
    'High variance = the threshold is discretionary (enables extraction via selective application). Low variance = the threshold is norm-governed (enables genuine coordination). The reading collapses into snare if threshold is purely discretionary; sustains as tangled_rope if norm-governed but still extractive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(atrocity_threshold_definition, empirical, 'Empirical definition and consistent application of atrocity threshold').

omega_variable(
    intervention_coalition_motives,
    'Is intervention motivated primarily by humanitarian concern or by strategic interest? Can these motives be empirically distinguished?',
    'Analysis of intervention timing relative to strategic context (oil reserves, geopolitical rivalry, military base access, regional power balance); comparison of humanitarian urgency vs. intervention likelihood across cases; institutional communication analysis (public rhetoric vs. decision-making processes)',
    'If primarily strategic: the constraint is snare disguised as rope (extraction masked as coordination). If genuinely mixed: tangled_rope classification holds. If primarily humanitarian: rope or scaffold classification is correct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intervention_coalition_motives, empirical, 'Whether intervention is driven by humanitarian or strategic motives').

omega_variable(
    sovereignty_vs_human_rights_kernel_ambiguity,
    'Is this reading one interpretation of a stable kernel (the Westphalian sovereignty principle admits multiple coherent readings), or does it represent a fundamental break with the kernel itself?',
    'Textual analysis of foundational documents (Westphalian treaties, UN Charter, Declaration of Human Rights); tracking of evolving doctrine and state practice; analysis of whether states treating this reading as authoritative maintain sovereignty commitments in other contexts',
    'If continuous reading: the constraint represents evolution within the sovereignty framework. If discontinuous break: the reading forecloses absolute sovereignty and instantiates a new kernel (conditional sovereignty). This affects whether sibling readings coexist or the reading forecloses them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_vs_human_rights_kernel_ambiguity, conceptual, 'Kernel continuity: is conditional sovereignty a reading of Westphalian sovereignty or a replacement kernel?').

omega_variable(
    non_intervening_state_victimhood,
    'Does the conditional sovereignty reading extract from non-intervening states by subordinating them in enforcement decisions, or do they benefit from shared norm-compliance?',
    'Analysis of Security Council patterns (veto usage by non-intervening powers, voice in threshold determination); post-intervention outcome tracking (burden-sharing in reconstruction, resource costs); comparison of autonomy costs vs. security benefits for non-intervening states',
    'If extraction dominates: snare from the non-intervening state''s perspective (forced compliance without voice). If benefits dominate: rope (shared norm-governance). This affects the tangled_rope classification''s internal structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_intervening_state_victimhood, empirical, 'Whether non-intervening states experience the constraint as extraction or coordination').

omega_variable(
    persecuted_population_agency,
    'Are persecuted populations treated as agents (with voice in intervention decisions) or as objects (on behalf of whom interventions occur without consultation)?',
    'Analysis of intervention planning: whether affected populations are consulted in decision-making; post-intervention surveys on perceived legitimacy; documentation of follow-up support vs. abandonment after military intervention; comparison with cases where populations were centrally involved',
    'If treated as objects: extraction from persecuted populations is severe (snare confirmed). If treated as agents: some agency exists, potentially upgrading the classification to constrained (moderate snare) or tangled_rope depending on voice mechanisms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(persecuted_population_agency, empirical, 'Whether persecuted populations have agency in intervention decisions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalian_sovereignty__conditional_sovereignty, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wps_cond_tr_t0, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 0, 0.35).
narrative_ontology:measurement(wps_cond_tr_t5, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 5, 0.42).
narrative_ontology:measurement(wps_cond_tr_t10, westphalian_sovereignty__conditional_sovereignty, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(wps_cond_be_t0, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(wps_cond_be_t5, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 5, 0.33).
narrative_ontology:measurement(wps_cond_be_t10, westphalian_sovereignty__conditional_sovereignty, base_extractiveness, 10, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(wps_cond_su_t0, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(wps_cond_su_t5, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 5, 0.5).
narrative_ontology:measurement(wps_cond_su_t10, westphalian_sovereignty__conditional_sovereignty, suppression_requirement, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalian_sovereignty__conditional_sovereignty, enforcement_mechanism).
narrative_ontology:affects_constraint(westphalian_sovereignty__conditional_sovereignty, westphalian_sovereignty__absolute_sovereignty).
narrative_ontology:affects_constraint(westphalian_sovereignty__conditional_sovereignty, westphalian_sovereignty__graduated_sovereignty).
narrative_ontology:affects_constraint(westphalian_sovereignty__conditional_sovereignty, humanitarian_intervention_selectivity).
narrative_ontology:affects_constraint(westphalian_sovereignty__conditional_sovereignty, international_law_sovereignty_asymmetry).

% DUAL FORMULATION NOTE:
% The conditional sovereignty reading is one member of the Westphalian sovereignty constraint family. All three readings (conditional, absolute, graduated) apply to the same kernel but produce different ε values and beneficiary/victim structures due to their different claims about intervention legitimacy and state obligation. Constraint family links enable analysis of how alternative readings to the same kernel affect downstream constraints in international governance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(westphalian_sovereignty__conditional_sovereignty, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
