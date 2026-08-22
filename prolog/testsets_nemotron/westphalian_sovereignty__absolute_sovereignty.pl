% ============================================================================
% CONSTRAINT STORY: westphalian_sovereignty__absolute_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: westphalian_sovereignty__absolute_sovereignty
 *   human_readable: Absolute Westphalian Sovereignty — Unconditional Non-Interference Shield
 *   domain: international_law/political_philosophy/global_governance
 *
 * SUMMARY:
 *   The absolute sovereignty reading of the Westphalian kernel holds that
 *   states possess unconditional authority over their domestic affairs and
 *   that external interference is categorically illegitimate. Originating in
 *   the 1648 Peace of Westphalia as a solution to religious wars, this
 *   reading has persisted as the default framing of international order
 *   despite the emergence of human rights norms, R2P doctrine, and
 *   supranational institutions. The constraint operates as a tangled rope: it
 *   provides genuine coordination (preventing systemic war through mutual
 *   non-interference recognition) while simultaneously extracting from
 *   domestic populations under repressive regimes who are shielded from
 *   external accountability. Authoritarian regimes and state elites are the
 *   primary beneficiaries; domestic populations bearing state violence are
 *   the primary victims. The constraint requires active enforcement through
 *   UN Security Council vetoes, diplomatic recognition practices, and the
 *   institutional architecture of state equality.
 *
 * KEY AGENTS:
 *   - authoritarian_regimes: Primary beneficiary (institutional/identity_locked) — uses non-interference shield to consolidate domestic control
 *   - state_elites_domestic_control: Primary beneficiary (institutional/constrained) — extracts rents and political survival from sovereignty shield
 *   - great_powers_protecting_sphere_of_influence: Secondary beneficiary (institutional/arbitrage) — invokes absolute sovereignty to protect regional clients while selectively violating it
 *   - domestic_populations_under_repression: Primary victim (powerless/trapped) — bears extraction (rights violations, violence) with no exit
 *   - minority_groups_targeted_by_state: Primary victim (powerless/identity_locked) — targeted by state policy, identity prevents exit
 *   - dissidents_and_opposition_movements: Victim (moderate/trapped) — organized resistance meets state suppression backed by sovereignty shield
 *   - refugees_fleeing_state_violence: Victim (powerless/constrained) — exit exists but is costly and incomplete
 *   - human_rights_advocacy_networks: Excluded (organized/constrained) — would challenge the shield but lacks enforcement power
 *   - r2p_proponents_international_lawyers: Observer (institutional/analytical) — reads the kernel as conditional_sovereignty
 *   - analytical_observer: Observer (analytical/analytical) — sees full structure across readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalian_sovereignty__absolute_sovereignty, 0.52).
domain_priors:suppression_score(westphalian_sovereignty__absolute_sovereignty, 0.78).
domain_priors:theater_ratio(westphalian_sovereignty__absolute_sovereignty, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, extractiveness, 0.52).
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalian_sovereignty__absolute_sovereignty, tangled_rope).
narrative_ontology:human_readable(westphalian_sovereignty__absolute_sovereignty, "Absolute Westphalian Sovereignty — Unconditional Non-Interference Shield").
narrative_ontology:topic_domain(westphalian_sovereignty__absolute_sovereignty, "international_law/political_philosophy/global_governance").

domain_priors:requires_active_enforcement(westphalian_sovereignty__absolute_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalian_sovereignty__absolute_sovereignty, '2568e25f-37ad-4cea-9211-3925ab5a32b7').
narrative_ontology:cs_kernel_codification('2568e25f-37ad-4cea-9211-3925ab5a32b7', formalized).
narrative_ontology:cs_authority_grounding('2568e25f-37ad-4cea-9211-3925ab5a32b7', lineage).
narrative_ontology:cs_interpretation_layer_present('2568e25f-37ad-4cea-9211-3925ab5a32b7').
narrative_ontology:cs_reading_relation('2568e25f-37ad-4cea-9211-3925ab5a32b7', westphalian_sovereignty__conditional_sovereignty, coexists_with).
narrative_ontology:cs_reading_relation('2568e25f-37ad-4cea-9211-3925ab5a32b7', westphalian_sovereignty__graduated_sovereignty, influences).
narrative_ontology:cs_axiom('2568e25f-37ad-4cea-9211-3925ab5a32b7', foundational, non_interference_categorical).
narrative_ontology:cs_axiom_status(non_interference_categorical, holdable).
narrative_ontology:cs_axiom_grounding('2568e25f-37ad-4cea-9211-3925ab5a32b7', non_interference_categorical, conventional).
narrative_ontology:cs_axiom('2568e25f-37ad-4cea-9211-3925ab5a32b7', foundational, state_equality_absolute).
narrative_ontology:cs_axiom_status(state_equality_absolute, holdable).
narrative_ontology:cs_axiom_grounding('2568e25f-37ad-4cea-9211-3925ab5a32b7', state_equality_absolute, conventional).
narrative_ontology:cs_axiom('2568e25f-37ad-4cea-9211-3925ab5a32b7', secondary, domestic_jurisdiction_exclusive).
narrative_ontology:cs_axiom_status(domestic_jurisdiction_exclusive, holdable).
narrative_ontology:cs_axiom_grounding('2568e25f-37ad-4cea-9211-3925ab5a32b7', domestic_jurisdiction_exclusive, conventional).
narrative_ontology:cs_reference_frame('2568e25f-37ad-4cea-9211-3925ab5a32b7', westphalian_1648_settlement).
narrative_ontology:cs_drift_state('2568e25f-37ad-4cea-9211-3925ab5a32b7', post_r2p_adoption_2005, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2568e25f-37ad-4cea-9211-3925ab5a32b7', '').
narrative_ontology:cs_kernel_id(westphalian_sovereignty__absolute_sovereignty, westphalian_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__absolute_sovereignty, authoritarian_regimes).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__absolute_sovereignty, state_elites_domestic_control).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__absolute_sovereignty, great_powers_protecting_sphere_of_influence).
narrative_ontology:constraint_victim(westphalian_sovereignty__absolute_sovereignty, domestic_populations_under_repression).
narrative_ontology:constraint_victim(westphalian_sovereignty__absolute_sovereignty, minority_groups_targeted_by_state).
narrative_ontology:constraint_victim(westphalian_sovereignty__absolute_sovereignty, dissidents_and_opposition_movements).
narrative_ontology:constraint_victim(westphalian_sovereignty__absolute_sovereignty, refugees_fleeing_state_violence).
narrative_ontology:constraint_vindicates(westphalian_sovereignty__absolute_sovereignty, state_equality_under_international_law).
narrative_ontology:constraint_vindicates(westphalian_sovereignty__absolute_sovereignty, territorial_integrity_principle).
narrative_ontology:constraint_vindicates(westphalian_sovereignty__absolute_sovereignty, non_intervention_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Uses the absolute non-interference norm to legitimize domestic repression, suppress dissent, and extract resources from population without external accountability. The regime's identity is fused with the sovereignty claim — abandoning absolute sovereignty would undermine its core legitimacy narrative. Exit from the constraint would mean accepting external monitoring, which the regime treats as existential threat.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, authoritarian_regimes, beneficiary,
    institutional, generational, identity_locked, national).

% Controls the state apparatus that invokes sovereignty at international forums. Benefits from the shield personally (impunity, asset protection, political survival). Can shape how the state presents its sovereignty claims. Exit is constrained: they could accept conditional sovereignty reforms but would lose the protective shield; their institutional position depends on maintaining the absolute reading.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, state_elites_domestic_control, beneficiary,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(westphalian_sovereignty__absolute_sovereignty, state_elites_domestic_control, agenda_setter).

% Permanent UNSC members (US, China, Russia, UK, France) who invoke absolute sovereignty to protect client states in their sphere of influence while reserving the right to violate it against adversaries (Iraq 2003, Libya 2011, Syria, Ukraine). They have arbitrage-grade exit: they can selectively apply or ignore the constraint based on strategic interest. Their benefit is structural — the norm creates a default of non-interference they can exploit.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, great_powers_protecting_sphere_of_influence, beneficiary,
    institutional, generational, arbitrage, global).

% Bears the full extraction of the constraint: rights violations, political violence, economic predation, and denial of accountability — all shielded by the absolute non-interference norm. No meaningful exit: emigration is blocked, dangerous, or incomplete; internal resistance meets state violence; international appeals are blocked by the sovereignty shield. The constraint's enforcement machinery (UNSC veto, non-recognition of intervention) directly prevents their rescue.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, domestic_populations_under_repression, payer,
    powerless, biographical, trapped, national).

% Targeted by state policies (discrimination, displacement, cultural erasure, violence) that the sovereignty shield protects from external scrutiny. Identity-locked: their collective identity is bound to the territory and community the state attacks; exit (assimilation, flight, abandonment of identity) is experienced as existential loss. The constraint extracts not just rights but existence-as-a-group.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, minority_groups_targeted_by_state, payer,
    powerless, generational, identity_locked, national).

% Organized political resistance to repressive regimes. The sovereignty shield denies them external support, recognition, or protection. They operate under surveillance, imprisonment, and violence with no structural exit — the international system treats their struggle as 'internal affairs.' Some achieve limited external solidarity, but the constraint's enforcement (non-recognition of rebel governance, arms embargoes on non-state actors) structurally disadvantages them.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, dissidents_and_opposition_movements, payer,
    moderate, biographical, trapped, national).

% Physical exit from the territory is possible but costly (dangerous journeys, family separation, asset loss, precarious legal status abroad). The constraint extracts by creating the conditions of flight and then denying international responsibility for the root causes. Host states invoke sovereignty to limit asylum obligations. The refugee regime (1951 Convention) is a partial mitigation but does not challenge the sovereignty shield that generates the outflow.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, refugees_fleeing_state_violence, payer,
    powerless, immediate, constrained, regional).

% Transnational NGOs, UN special procedures, treaty bodies, and activist networks that document violations and demand accountability. They are structurally excluded from the constraint's decision-making: they have no vote at the UNSC, no enforcement power, and their reports are formally acknowledged but structurally impotent against the sovereignty shield. They would object to the absolute reading if they had a seat; their exclusion is what allows the shield to persist.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, human_rights_advocacy_networks, excluded,
    organized, biographical, constrained, global).

% Scholars, diplomats, and officials who advance the conditional_sovereignty reading (R2P doctrine). They operate within the analytical seat: they see the full structure of the absolute reading and contest it with an alternative reading of the same kernel. Their 'exit' is analytical — they can shift frameworks but cannot unilaterally change the operative constraint. They provide the intellectual infrastructure for the sibling reading.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, r2p_proponents_international_lawyers, observer,
    institutional, generational, analytical, global).

% The indexical classification seat: sees all three readings of the westphalian_sovereignty kernel simultaneously, tracks their structural differences, and measures the constraint family's evolution. Has no stake in any reading's victory; the constraint's operation is the object of analysis, not a condition of their existence.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents systemic interstate war by establishing a mutual non-interference baseline: states agree not to intervene in each other's domestic affairs in exchange for the same guarantee. Solves the Hobbesian security dilemma at the international level by making domestic jurisdiction a closed domain.
% TRANSFER_FUNCTION: Moves impunity for domestic extraction (repression, resource predation, rights violations) from accountability mechanisms to state elites. The transfer is: domestic populations lose external protection; state elites gain a shield against intervention. Great powers gain a default non-interference rule they can selectively enforce or violate.
% ABSENT_VOICES: The populations most harmed by the constraint (repressed minorities, dissidents, refugees) are structurally absent from the international legal order that authors the sovereignty norm. They have no seat at the UN, no vote on treaties, no standing in the ICJ. Their exclusion is not accidental — the state-centric system defines them as objects of sovereignty, not subjects of international law. Human rights networks speak for them but lack structural power.
% DISAPPEARANCE_RATIONALE: If the absolute non-interference norm vanished overnight, the international system would not revert to chaos — it would reorganize around conditional sovereignty (R2P), graduated sovereignty, or regional accountability mechanisms. Intervention decisions would shift to case-by-case authorization (UNSC, regional bodies, ad hoc coalitions). Authoritarian regimes would lose their primary legitimacy shield. The coordination function (interstate order) would persist through alternative mechanisms (trade interdependence, nuclear deterrence, regional security architectures) but the extraction shield would collapse.
% FOUNDING_PROBLEM: The 1648 Peace of Westphalia ended the Thirty Years' War by establishing mutual non-interference in religious affairs as the basis for interstate peace. The founding problem was: how to stop sovereigns from waging war over the internal religious composition of other states. The solution was to make domestic jurisdiction a closed domain — cuius regio, eius religio generalized to cuius regio, eius jurisdictio.
% FOUNDING_PROBLEM_CORROBORATION: The absolute reading's proponents (majority of UN member states, Non-Aligned Movement, China/Russia diplomatic positions) attest the founding problem is LIVE: interstate intervention remains the primary threat to peace; the 1648 logic still applies. The conditional reading's proponents (R2P architects, human rights NGOs, Western liberal democracies, African Union) attest the founding problem is DEAD: the primary threat has shifted from interstate war to intrastate atrocity; the 1648 solution now enables the problem. The graduated reading's proponents (development economists, state-building practitioners) attest the founding problem is CONTESTED: sovereignty was never absolute in practice; capacity and legitimacy have always graded it. No single corroboration commands consensus — the kernel is genuinely contested.
narrative_ontology:disappearance_verdict(westphalian_sovereignty__absolute_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(westphalian_sovereignty__absolute_sovereignty, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalian_sovereignty__absolute_sovereignty, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(westphalian_sovereignty__absolute_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(westphalian_sovereignty__absolute_sovereignty, 0.52, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalian_sovereignty__absolute_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalian_sovereignty__absolute_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalian_sovereignty__absolute_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52) is substantial: the non-interference shield enables extraction from domestic populations that would be constrained by external accountability. The shield is not costless — it requires active enforcement through UNSC vetoes, diplomatic non-recognition of intervention, and the institutional reproduction of state equality. Suppression (0.78) is high: the constraint actively suppresses alternative accountability mechanisms (R2P, ICC, universal jurisdiction, humanitarian intervention) and the cost of resistance for victims is extreme (state violence, no exit). Theater ratio (0.31) reflects that the coordination function (interstate order) is real but increasingly performative — the shield is invoked to protect repression more than to prevent war. Accessibility collapse (0.65) is moderate-high: alternatives (conditional sovereignty, supranational accountability) exist but are structurally blocked by the veto-wielding beneficiaries. Resistance (0.48) is moderate: advocacy networks, R2P doctrine, and regional bodies (AU, EU) contest the absolute reading but have not displaced it.
 *
 * PERSPECTIVAL GAP:
 *   The engine will compute divergent types per seat: from the authoritarian regime seat, the constraint appears as rope (genuine coordination of interstate order that incidentally benefits them). From the domestic population seat, it appears as snare (pure extraction shielded by coordination cover). From the great power seat, it appears as tangled_rope (they benefit from the shield for clients but violate it for adversaries — asymmetric extraction). From the analytical observer seat, the full structure is visible: a tangled_rope with genuine coordination at the systemic level and concentrated extraction at the domestic level. This seat divergence IS the measurement — the constraint's classification depends on where you sit.
 *
 * DIRECTIONALITY LOGIC:
 *   Authoritarian regimes and state elites are structural beneficiaries (d ≈ 0.15): they collect the full value of the non-interference shield — impunity for domestic extraction. Great powers are partial beneficiaries (d ≈ 0.25): they benefit from the shield for their spheres of influence but also violate it selectively (arbitrage exit). Domestic populations under repression are full targets (d ≈ 0.95): they bear the extraction with no exit (trapped). Minority groups are identity-locked targets (d ≈ 0.9): their identity fuses with the victim position — exit would require abandoning community/territory. Dissidents are trapped targets (d ≈ 0.85): organized resistance meets the full force of the state backed by the shield. Refugees are constrained targets (d ≈ 0.7): physical exit exists but carries extreme cost and incomplete protection. Human rights networks are excluded observers (d ≈ 0.6): they contest the constraint but lack structural power to alter it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (1648: preventing religious wars through mutual non-interference) is contested in status. The coordination function (interstate order) remains live — great powers still need a baseline non-interference norm to avoid systemic conflict. But the extraction function has accumulated: the shield now primarily protects domestic repression rather than preventing interstate war. The constraint has not been formally sunset; no replacement architecture has achieved universal buy-in. Mandatrophy is unresolved: the coordination function persists but the extraction function has grown beyond its original justification. The conditional_sovereignty reading attempts to resolve this by conditioning the shield on responsibility; the graduated_sovereignty reading attempts to resolve it by making sovereignty contingent on capacity/legitimacy. Neither has displaced the absolute reading as the operative constraint for the majority of states.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_shield,
    'Is the absolute non-interference norm a structural feature of the international system (analogous to physical law) or a constructed constraint that benefits identifiable agents?',
    'Historical counterfactual: if the 1648 settlement had produced a conditional sovereignty norm, would the international system have collapsed? Comparative analysis of regional systems (e.g., EU supranationalism, African Union intervention mandates) that relax absolute non-interference without systemic failure.',
    'If constructed, the constraint is a false summit candidate (Mountain claim with beneficiaries) — FSM signature would reclassify to tangled_rope. If natural, the beneficiary declarations are epiphenomenal and the Mountain claim holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_shield, conceptual, 'Whether absolute sovereignty is a natural law of international order or a constructed shield for power').

omega_variable(
    conditional_sovereignty_emergence,
    'Does the R2P (Responsibility to Protect) doctrine represent a genuine structural evolution of the sovereignty constraint, or a rhetorical overlay that leaves the absolute shield intact for powerful states?',
    'Track invocation and enforcement of R2P across cases (Libya 2011, Syria, Myanmar, Xinjiang). Measure whether intervention authorization correlates with great power interest or solely with atrocity threshold.',
    'If R2P is structurally operative, the absolute_sovereignty reading is drifting toward conditional_sovereignty — the kernel''s authoritative reading is shifting. If R2P is selectively applied, absolute_sovereignty remains the operative constraint for powerful states while conditional_sovereignty applies only to the weak.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(conditional_sovereignty_emergence, empirical, 'Whether R2P has structurally modified the sovereignty constraint or remains rhetorical').

omega_variable(
    kernel_reading_identity,
    'This constraint is one reading (absolute_sovereignty) of the contested kernel westphalian_sovereignty. Sibling readings: conditional_sovereignty, graduated_sovereignty. What structural elements distinguish this reading from its siblings?',
    'Compare the three readings on: victim set (who is unprotected), beneficiary set (who collects from non-interference), enforcement machinery (what active enforcement sustains the shield), and founding problem status (whether the 1648 problem of religious war remains live).',
    'Clarifies whether the three readings are structurally distinct constraints (per ε-invariance principle) or measurement variants of one constraint. If distinct, each gets its own story; if variants, they must be merged.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Committer frame: structural identity of the absolute_sovereignty reading within the westphalian_sovereignty kernel').

omega_variable(
    great_power_exception_structure,
    'Do great powers operate under a de facto graduated_sovereignty reading while enforcing absolute_sovereignty for weaker states?',
    'Analyze intervention patterns: frequency, authorization source (UNSC vs. unilateral), and target state power ranking. Test whether intervention probability correlates inversely with target state''s power index.',
    'If true, the constraint operates as a two-tier system: absolute_sovereignty for the weak (extractive snare), graduated_sovereignty for the strong (coordination rope). This would require decomposing into two constraint stories linked by network.affects_constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(great_power_exception_structure, empirical, 'Whether sovereignty constraint application is power-graded rather than universal').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalian_sovereignty__absolute_sovereignty, 1648, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t1648, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 1648, 0.1).
narrative_ontology:measurement(west_tr_t1919, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 1919, 0.15).
narrative_ontology:measurement(west_tr_t1945, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 1945, 0.2).
narrative_ontology:measurement(west_tr_t1990, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 1990, 0.25).
narrative_ontology:measurement(west_tr_t2005, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 2005, 0.28).
narrative_ontology:measurement(west_tr_t2024, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 2024, 0.31).

% Extraction over time
narrative_ontology:measurement(west_be_t1648, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 1648, 0.25).
narrative_ontology:measurement(west_be_t1919, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 1919, 0.35).
narrative_ontology:measurement(west_be_t1945, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 1945, 0.42).
narrative_ontology:measurement(west_be_t1990, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 1990, 0.48).
narrative_ontology:measurement(west_be_t2005, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 2005, 0.5).
narrative_ontology:measurement(west_be_t2024, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 2024, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t1648, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 1648, 0.6).
narrative_ontology:measurement(west_su_t1919, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 1919, 0.65).
narrative_ontology:measurement(west_su_t1945, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 1945, 0.7).
narrative_ontology:measurement(west_su_t1990, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 1990, 0.73).
narrative_ontology:measurement(west_su_t2005, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 2005, 0.76).
narrative_ontology:measurement(west_su_t2024, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 2024, 0.78).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1648, tn=2024
narrative_ontology:measurement(west_grid_01, westphalian_sovereignty__absolute_sovereignty, accessibility_collapse(class), 1648, 0.6).
narrative_ontology:measurement(west_grid_02, westphalian_sovereignty__absolute_sovereignty, accessibility_collapse(class), 2024, 0.8).
narrative_ontology:measurement(west_grid_03, westphalian_sovereignty__absolute_sovereignty, accessibility_collapse(individual), 1648, 0.7).
narrative_ontology:measurement(west_grid_04, westphalian_sovereignty__absolute_sovereignty, accessibility_collapse(individual), 2024, 0.85).
narrative_ontology:measurement(west_grid_05, westphalian_sovereignty__absolute_sovereignty, accessibility_collapse(organizational), 1648, 0.45).
narrative_ontology:measurement(west_grid_06, westphalian_sovereignty__absolute_sovereignty, accessibility_collapse(organizational), 2024, 0.7).
narrative_ontology:measurement(west_grid_07, westphalian_sovereignty__absolute_sovereignty, accessibility_collapse(structural), 1648, 0.55).
narrative_ontology:measurement(west_grid_08, westphalian_sovereignty__absolute_sovereignty, accessibility_collapse(structural), 2024, 0.75).
narrative_ontology:measurement(west_grid_09, westphalian_sovereignty__absolute_sovereignty, resistance(class), 1648, 0.3).
narrative_ontology:measurement(west_grid_10, westphalian_sovereignty__absolute_sovereignty, resistance(class), 2024, 0.5).
narrative_ontology:measurement(west_grid_11, westphalian_sovereignty__absolute_sovereignty, resistance(individual), 1648, 0.15).
narrative_ontology:measurement(west_grid_12, westphalian_sovereignty__absolute_sovereignty, resistance(individual), 2024, 0.4).
narrative_ontology:measurement(west_grid_13, westphalian_sovereignty__absolute_sovereignty, resistance(organizational), 1648, 0.25).
narrative_ontology:measurement(west_grid_14, westphalian_sovereignty__absolute_sovereignty, resistance(organizational), 2024, 0.45).
narrative_ontology:measurement(west_grid_15, westphalian_sovereignty__absolute_sovereignty, resistance(structural), 1648, 0.2).
narrative_ontology:measurement(west_grid_16, westphalian_sovereignty__absolute_sovereignty, resistance(structural), 2024, 0.35).
narrative_ontology:measurement(west_grid_17, westphalian_sovereignty__absolute_sovereignty, stakes_inflation(class), 1648, 0.35).
narrative_ontology:measurement(west_grid_18, westphalian_sovereignty__absolute_sovereignty, stakes_inflation(class), 2024, 0.7).
narrative_ontology:measurement(west_grid_19, westphalian_sovereignty__absolute_sovereignty, stakes_inflation(individual), 1648, 0.4).
narrative_ontology:measurement(west_grid_20, westphalian_sovereignty__absolute_sovereignty, stakes_inflation(individual), 2024, 0.8).
narrative_ontology:measurement(west_grid_21, westphalian_sovereignty__absolute_sovereignty, stakes_inflation(organizational), 1648, 0.25).
narrative_ontology:measurement(west_grid_22, westphalian_sovereignty__absolute_sovereignty, stakes_inflation(organizational), 2024, 0.55).
narrative_ontology:measurement(west_grid_23, westphalian_sovereignty__absolute_sovereignty, stakes_inflation(structural), 1648, 0.3).
narrative_ontology:measurement(west_grid_24, westphalian_sovereignty__absolute_sovereignty, stakes_inflation(structural), 2024, 0.65).
narrative_ontology:measurement(west_grid_25, westphalian_sovereignty__absolute_sovereignty, suppression(class), 1648, 0.6).
narrative_ontology:measurement(west_grid_26, westphalian_sovereignty__absolute_sovereignty, suppression(class), 2024, 0.85).
narrative_ontology:measurement(west_grid_27, westphalian_sovereignty__absolute_sovereignty, suppression(individual), 1648, 0.65).
narrative_ontology:measurement(west_grid_28, westphalian_sovereignty__absolute_sovereignty, suppression(individual), 2024, 0.9).
narrative_ontology:measurement(west_grid_29, westphalian_sovereignty__absolute_sovereignty, suppression(organizational), 1648, 0.55).
narrative_ontology:measurement(west_grid_30, westphalian_sovereignty__absolute_sovereignty, suppression(organizational), 2024, 0.8).
narrative_ontology:measurement(west_grid_31, westphalian_sovereignty__absolute_sovereignty, suppression(structural), 1648, 0.5).
narrative_ontology:measurement(west_grid_32, westphalian_sovereignty__absolute_sovereignty, suppression(structural), 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalian_sovereignty__absolute_sovereignty, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(westphalian_sovereignty__absolute_sovereignty, 0.12).
narrative_ontology:affects_constraint(westphalian_sovereignty__absolute_sovereignty, westphalian_sovereignty__conditional_sovereignty).
narrative_ontology:affects_constraint(westphalian_sovereignty__absolute_sovereignty, westphalian_sovereignty__graduated_sovereignty).
narrative_ontology:affects_constraint(westphalian_sovereignty__absolute_sovereignty, responsibility_to_protect_doctrine).
narrative_ontology:affects_constraint(westphalian_sovereignty__absolute_sovereignty, international_criminal_court_jurisdiction).
narrative_ontology:affects_constraint(westphalian_sovereignty__absolute_sovereignty, universal_jurisdiction_norms).
narrative_ontology:affects_constraint(westphalian_sovereignty__absolute_sovereignty, humanitarian_intervention_norm).

% DUAL FORMULATION NOTE:
% This story is one member of the westphalian_sovereignty constraint family (3 stories). The absolute_sovereignty reading is the upstream anchor (highest institutional entrenchment, most states formally subscribe). The conditional_sovereignty reading (R2P) is downstream — it cites absolute_sovereignty as the baseline it modifies. The graduated_sovereignty reading is a lateral reformulation — it re-grounds sovereignty in capacity/legitimacy rather than territory/recognition. All three have distinct ε, distinct beneficiaries/victims, and distinct enforcement machinery. The family exists because the label 'Westphalian sovereignty' conflates them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(westphalian_sovereignty__absolute_sovereignty, institutional, 0.15).
constraint_indexing:directionality_override(westphalian_sovereignty__absolute_sovereignty, powerless, 0.95).
constraint_indexing:directionality_override(westphalian_sovereignty__absolute_sovereignty, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
