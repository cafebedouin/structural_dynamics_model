% ============================================================================
% CONSTRAINT STORY: westphalian_sovereignty__absolute_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalian_sovereignty__absolute_sovereignty, []).

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
 *   constraint_id: westphalian_sovereignty__absolute_sovereignty
 *   human_readable: Westphalian Absolute Sovereignty: Non-Interference Shield
 *   domain: international_law/political_philosophy/global_governance
 *
 * SUMMARY:
 *   The principle of absolute state sovereignty — that states possess
 *   unconditional authority over domestic affairs and external interference
 *   is categorically illegitimate — is one coherent reading of the
 *   Westphalian kernel. This reading instantiates the absolute framing:
 *   sovereignty is indivisible, non-negotiable, and not subject to
 *   qualification based on how states use it. This is distinct from two
 *   sibling readings: conditional sovereignty (which permits intervention
 *   when domestic actions breach universal norms) and graduated sovereignty
 *   (which scales non-interference rights proportionally to regime quality or
 *   military capacity). The absolute reading represents the institutional
 *   orthodoxy in current international law and state practice, yet it creates
 *   a structural tension: it provides genuine coordination benefits (enabling
 *   multiple states to coexist without imperial hierarchy) while
 *   systematically benefiting states that use the shield to prevent
 *   accountability for repression. The constraint exhibits Tangled Rope
 *   structure: the coordination function (mutual recognition, predictable
 *   non-interference) is real and necessary; the extraction (authoritarian
 *   regimes gain impunity; trapped populations have no external recourse) is
 *   also structurally embedded. The theater ratio has risen from 0.42 to 0.58
 *   over the interval, reflecting increasing performative invocation of
 *   humanitarian justifications that do not functionally override the
 *   sovereignty shield.
 *
 * KEY AGENTS:
 *   - Authoritarian State Apparatus: Primary beneficiary (institutional/arbitrage) — extracts impunity from the non-interference shield
 *   - Domestic Population Under Repression: Primary victim (powerless/trapped) — bears full cost of suppression; prevented from seeking external help by sovereignty doctrine
 *   - International Human Rights Regime: Secondary victim (organized/constrained) — bound by the same doctrine that enables its legitimacy; cannot enforce norms it proclaims
 *   - Western Liberal Democracies: Tertiary actor (powerful/constrained) — benefit from sovereignty doctrine domestically but constrained by the principle they invoke when humanitarian crises arise
 *   - Westphalian State System: Structural beneficiary (institutional/arbitrage) — absolute sovereignty is the coordinating mechanism enabling decentralized coexistence
 *   - Conditional Sovereignty Advocates: Counter-reading agents (organized/constrained) — attempt to modify the reading toward conditional interpretation; constrained by the dominance of absolute framing in state practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalian_sovereignty__absolute_sovereignty, 0.52).
domain_priors:suppression_score(westphalian_sovereignty__absolute_sovereignty, 0.68).
domain_priors:theater_ratio(westphalian_sovereignty__absolute_sovereignty, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, extractiveness, 0.52).
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalian_sovereignty__absolute_sovereignty, tangled_rope).
narrative_ontology:human_readable(westphalian_sovereignty__absolute_sovereignty, "Westphalian Absolute Sovereignty: Non-Interference Shield").
narrative_ontology:topic_domain(westphalian_sovereignty__absolute_sovereignty, "international_law/political_philosophy/global_governance").

domain_priors:requires_active_enforcement(westphalian_sovereignty__absolute_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalian_sovereignty__absolute_sovereignty, 'dc931b0b-a801-47af-b2fb-1f8cb85405a4').
narrative_ontology:cs_kernel_codification('dc931b0b-a801-47af-b2fb-1f8cb85405a4', formalized).
narrative_ontology:cs_authority_grounding('dc931b0b-a801-47af-b2fb-1f8cb85405a4', lineage).
narrative_ontology:cs_interpretation_layer_present('dc931b0b-a801-47af-b2fb-1f8cb85405a4').
narrative_ontology:cs_reading_relation('dc931b0b-a801-47af-b2fb-1f8cb85405a4', westphalian_sovereignty__conditional_sovereignty, coexists_with).
narrative_ontology:cs_reading_relation('dc931b0b-a801-47af-b2fb-1f8cb85405a4', westphalian_sovereignty__graduated_sovereignty, coexists_with).
narrative_ontology:cs_axiom('dc931b0b-a801-47af-b2fb-1f8cb85405a4', foundational, sovereignty_unconditionally_protected).
narrative_ontology:cs_axiom_status(sovereignty_unconditionally_protected, holdable).
narrative_ontology:cs_axiom_grounding('dc931b0b-a801-47af-b2fb-1f8cb85405a4', sovereignty_unconditionally_protected, deontological).
narrative_ontology:cs_axiom('dc931b0b-a801-47af-b2fb-1f8cb85405a4', foundational, non_interference_categorically_legitimate).
narrative_ontology:cs_axiom_status(non_interference_categorically_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('dc931b0b-a801-47af-b2fb-1f8cb85405a4', non_interference_categorically_legitimate, conventional).
narrative_ontology:cs_reference_frame('dc931b0b-a801-47af-b2fb-1f8cb85405a4', unconditional_mutual_non_interference).
narrative_ontology:cs_drift_state('dc931b0b-a801-47af-b2fb-1f8cb85405a4', contemporary_post_humanitarian_intervention_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('dc931b0b-a801-47af-b2fb-1f8cb85405a4', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(westphalian_sovereignty__absolute_sovereignty, westphalian_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__absolute_sovereignty, authoritarian_state_apparatus).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__absolute_sovereignty, state_sovereigns_elite).
narrative_ontology:constraint_victim(westphalian_sovereignty__absolute_sovereignty, domestic_populations_under_repression).
narrative_ontology:constraint_victim(westphalian_sovereignty__absolute_sovereignty, international_human_rights_regime).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAPPED DOMESTIC POPULATION (SNARE) — No exit capacity. The sovereignty shield prevents international intervention, humanitarian relief, or asylum pathways. External actors that might help are categorically barred from action. Full extraction: the population bears the cost of repression while the suppression mechanism (sovereignty doctrine) prevents external remedies. The population is both victim and prisoner of the absolute sovereignty principle.
constraint_indexing:constraint_classification(westphalian_sovereignty__absolute_sovereignty, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INTERNATIONAL HUMAN RIGHTS REGIME (TANGLED ROPE) — Benefits from sovereignty doctrine (provides stable framework for treaty-making and global coordination) but constrained by its own rule against intervention. Bears significant costs: must watch documented atrocities without legal remedy; constrained by the same doctrine that enables it. Mixed experience: genuine coordination function (universal norms) layered with asymmetric extraction (regime states extract impunity; rights advocates extract legitimacy but no enforcement power).
constraint_indexing:constraint_classification(westphalian_sovereignty__absolute_sovereignty, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: AUTHORITARIAN STATE APPARATUS (ROPE) — Pure beneficiary. Sovereignty doctrine provides categorical shield against interference: no humanitarian interventions, no sanctions on human rights grounds alone, no exile prosecution, no refugee obligation. The state experiences this as coordination: it coordinates with other states via mutual non-interference pact, gaining predictable immunity in exchange for recognizing others' immunity. Net positive extraction — the constraint subsidizes this agent's authority and independence.
constraint_indexing:constraint_classification(westphalian_sovereignty__absolute_sovereignty, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: WESTERN LIBERAL DEMOCRACIES (TANGLED ROPE) — Benefits from sovereignty doctrine: protects their own domestic policies from international overrule, enables geopolitical autonomy, provides stable system for trade and alliance-building. But also constrained by the doctrine they invoke: cannot justify humanitarian intervention in allied states without undermining the principle they rely on; constrained by the rule they benefit from. Mixed experience of coordination and extraction running in opposite directions depending on context (beneficiary in material security, victim in humanitarian consistency).
constraint_indexing:constraint_classification(westphalian_sovereignty__absolute_sovereignty, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: WESTPHALIAN STATE SYSTEM (ROPE) — The absolute sovereignty principle is the coordinating mechanism that enables the state system to function. Without it, no state could plan domestically; without mutual recognition of non-interference, the system collapses into imperial competition. The system is a genuine coordination solution to a real problem: how to enable multiple power centers to coexist. The extraction (asymmetric costs borne by vulnerable populations) is layered on top of coordination, not replacing it.
constraint_indexing:constraint_classification(westphalian_sovereignty__absolute_sovereignty, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, absolute sovereignty might appear as an immutable natural law: the structural limit of any decentralized system of states. No authority above states can exist without creating a world state; therefore non-interference is inherent to anarchic international systems. However, the presence of identified beneficiaries (authoritarian regimes, states that extract impunity) and victims (repressed populations, the human rights regime itself) triggers false-summit detection. The principle naturalizes what is actually a contingent institutional choice — other readings (conditional, graduated sovereignty) remain coherent alternatives within the state system framework.
constraint_indexing:constraint_classification(westphalian_sovereignty__absolute_sovereignty, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalian_sovereignty__absolute_sovereignty_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(westphalian_sovereignty__absolute_sovereignty, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(westphalian_sovereignty__absolute_sovereignty, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalian_sovereignty__absolute_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalian_sovereignty__absolute_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The absolute sovereignty reading creates asymmetric benefits for states that prevent exit (authoritarian regimes) while imposing costs on trapped populations and the international human rights regime. The extractiveness is not maximal (ε < 0.66 would trigger Snare gates) because the principle also provides genuine coordination benefits even to repressive regimes — it enables them to plan domestically without external intervention uncertainty. But the concentration of benefits on regimes that use the shield for repression, combined with the documented victimhood of trapped populations, justifies the moderate-high estimate. The reading accrues extraction over time (0.38 → 0.52 across the interval) as authoritarian regimes have learned to invoke it defensively and as the gap between humanitarian rhetoric and enforcement has widened. Suppression (0.68): High. The doctrine prevents external actors from providing asylum, humanitarian relief, or accountability mechanisms. The trapped population faces severe barriers to exit: sovereignty shields border closure, criminalizes emigration, and prevents external prosecution of regime officials. Suppression is not maximum (< 1.0) because some exit pathways exist (refugee networks, private sponsorship) outside the formal system, and the doctrine's enforcement is only as strong as states' collective will to maintain it. Theater ratio (0.58): Moderate-high. Increasingly, states invoke humanitarian language and rights discourse while maintaining absolute sovereignty in practice. UN General Assembly and Security Council debates frequently invoke humanitarian principles, yet interventions remain rare and discretionary. The theater has risen as the doctrine's legitimacy has become contested — more performative justification is needed to sustain what was once seen as natural law. The rise from 0.42 to 0.58 reflects this increasing performance gap between rhetoric (humanitarian concern, universal norms) and practice (categorical non-interference).
 *
 * PERSPECTIVAL GAP:
 *   The absolute sovereignty reading produces a wide perspectival gap. The authoritarian regime (institutional/arbitrage) sees pure coordination — a mutual pact enabling predictable non-interference. The trapped population (powerless/trapped) sees pure extraction — a doctrine that prevents any external help. The international human rights regime (organized/constrained) sees tangled rope — genuine norms that it cannot enforce. Western democracies (powerful/constrained) see tangled rope at generational time horizons — the doctrine constrains their humanitarian impulses, even as it protects their autonomy. The state system (institutional/arbitrage) sees rope — the coordinating mechanism that makes multiple sovereigns possible. The analytical observer risks seeing mountain (natural law) but the structural data — identified beneficiaries, identifiable victims, contingent alternative readings — reveals this as a false summit. The perspectival gap is not simply disagreement about values; it reflects different structural relationships to the extraction mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is derived from the agent's beneficiary/victim status and exit options. Authoritarian state apparatus: beneficiary + arbitrage exit (can choose when to invoke sovereignty and when to cooperate) → low d → negative f(d) → extraction runs toward this agent. Trapped population: victim + trapped exit (no exit options) → high d → f(d) ≈ 1.42 → maximum experienced extraction. International human rights regime: victim + constrained exit (bound by the principle it proclaims) → d ≈ 0.70 → f(d) ≈ 1.05 → moderate experienced extraction. Western liberal democracies: both beneficiary (in material security) and constrained (by humanitarian consistency) → mixed d depending on context → f(d) varies by perspective time horizon. The state system itself: beneficiary + arbitrage (enforces its own logic) → low d. The analytical observer faces a potential false summit: if the absolute reading is naturalized as inherent to decentralized systems, the analysis risks misclassifying a contingent institutional choice as a natural law. The presence of identified beneficiaries (authoritarian regimes) and the fact that alternative readings (conditional, graduated) remain logically coherent within the state-system framework trigger false-summit detection.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_versus_reading_foreclusion,
    'Does the absolute sovereignty reading logically foreclose the conditional sovereignty reading, or can both coexist as live positions in practice?',
    'Examination of state practice: do states simultaneously invoke absolute sovereignty AND conditional humanitarian intervention grounds? If both are invoked by different states or even the same state in different contexts, they coexist rather than foreclose. If one reading has been formally abandoned by authority structures (e.g., humanitarian intervention doctrine is treated as superseded or illegitimate), it may be foreclosed.',
    'If foreclosed: absolute reading dominates institutional structure. If coexists_with: both readings are live, and the constraint family exhibits genuine perspectival plurality rather than hierarchy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_versus_reading_foreclusion, empirical, 'Whether absolute and conditional sovereignty readings foreclose each other or coexist').

omega_variable(
    domestic_tyranny_as_implicit_victim,
    'Is the doctrine of absolute sovereignty a genuine natural law of decentralized systems, or does it systematically extract in favor of regimes that prevent their populations from exercising exit options?',
    'Comparative analysis: do states with strong human rights protections domestically benefit equally from non-interference doctrine as repressive states? If the doctrine provides the same structural benefit to both, it is coordination. If the doctrine''s primary benefit flows to states using it as a shield against human rights accountability, it is extraction.',
    'If coordination only: ε drops below 0.40, constraint reclassifies toward Rope. If extraction is primary function: ε rises, Tangled Rope or Snare classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domestic_tyranny_as_implicit_victim, empirical, 'Whether absolute sovereignty primarily coordinates or extracts via enabling domestic repression').

omega_variable(
    humanitarian_intervention_as_legitimacy_pressure,
    'Is the rising discourse of humanitarian intervention (post-Rwanda, post-Syria) evidence that the absolute sovereignty reading is eroding, or merely evidence of performative rhetoric that does not functionally constrain the doctrine?',
    'Track: (a) frequency and proportion of humanitarian interventions approved by UN or regional bodies; (b) ratio of approved to proposed interventions; (c) correlation between documented atrocity scale and intervention likelihood; (d) legal status of unilateral humanitarian intervention (accepted or condemned). Rising intervention rates + legal acceptance = authentic drift. Performative invocation + rejection when invoked = stable theater.',
    'If authentic drift: the absolute reading''s reference frame (unconditional non-interference) is eroding; drift_state direction should be authority_erosion or practice_drift. If performative: reading remains stable despite rhetorical pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(humanitarian_intervention_as_legitimacy_pressure, empirical, 'Whether humanitarian intervention discourse represents authentic erosion of absolute sovereignty or performative rhetoric').

omega_variable(
    beneficiary_variance_across_regime_type,
    'Do democratic and non-democratic states derive equivalent benefits from the absolute sovereignty doctrine, or is the extraction mechanism particularly concentrated on benefiting authoritarian regimes?',
    'Analysis of actual state invocations: which states invoke the doctrine most frequently and in which contexts? Which states benefit materially (avoid sanctions, intervention, ICC prosecution) from the shield? Cross-tabulation with regime type (Polity, V-Dem coding). If invocations cluster around authoritarian regimes defending specific repressive practices, the doctrine serves extractive purposes for those regimes specifically.',
    'If concentrated on authoritarians: beneficiary group ''authoritarian_state_apparatus'' is accurate; victims group ''domestic_populations_under_repression'' is structurally tight. If distributed across regime types: both beneficiaries and victims are more diffuse; ε may be lower (more genuine coordination).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(beneficiary_variance_across_regime_type, empirical, 'Whether absolute sovereignty doctrine benefits concentrate on authoritarian regimes').

omega_variable(
    graduated_reading_empirical_viability,
    'Is the graduated sovereignty reading (conditional non-interference scaled by regime quality, military capacity, or other factors) a live institutional alternative, or a purely academic proposal with no structural support?',
    'Institutional audit: has any major state or international body formally adopted graduated sovereignty as doctrine? Has any security council resolution or treaty framework operationalized it? Or does it remain a proposal without enforcement machinery?',
    'If live alternative: the absolute reading''s dominance is contingent (could be displaced by graduated). If academic only: absolute reading''s institutional hegemony is explained by path dependence and collective action coordination, not logical necessity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(graduated_reading_empirical_viability, empirical, 'Whether graduated sovereignty reading is institutionally live or purely academic').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalian_sovereignty__absolute_sovereignty, 0, 375).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wps_abs_theater_t0, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 0, 0.42).
narrative_ontology:measurement(wps_abs_theater_t200, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 200, 0.52).
narrative_ontology:measurement(wps_abs_theater_t375, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 375, 0.58).

% Extraction over time
narrative_ontology:measurement(wps_abs_extract_t0, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(wps_abs_extract_t200, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 200, 0.48).
narrative_ontology:measurement(wps_abs_extract_t375, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 375, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(wps_abs_suppress_t0, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(wps_abs_suppress_t200, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 200, 0.65).
narrative_ontology:measurement(wps_abs_suppress_t375, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 375, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalian_sovereignty__absolute_sovereignty, enforcement_mechanism).
narrative_ontology:affects_constraint(westphalian_sovereignty__absolute_sovereignty, westphalian_sovereignty__conditional_sovereignty).
narrative_ontology:affects_constraint(westphalian_sovereignty__absolute_sovereignty, westphalian_sovereignty__graduated_sovereignty).
narrative_ontology:affects_constraint(westphalian_sovereignty__absolute_sovereignty, humanitarian_intervention_legitimacy).
narrative_ontology:affects_constraint(westphalian_sovereignty__absolute_sovereignty, state_system_anarchy_structure).

% DUAL FORMULATION NOTE:
% The Westphalian kernel decomposes into three reading-specific constraints: absolute, conditional, and graduated sovereignty. Each reading has a distinct ε value reflecting how it balances coordination and extraction. The absolute reading (this file, ε=0.52) emphasizes coordination but accrues extraction asymmetrically to authoritarian regimes. The conditional reading (downstream, ε varies) adds intervention rights, reducing extraction for trapped populations but potentially reducing coordination stability. The graduated reading (downstream, ε varies) scales non-interference by regime type, creating finer-grained extraction patterns. All three are linked via network.affects_constraints and instantiate the same kernel differently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
