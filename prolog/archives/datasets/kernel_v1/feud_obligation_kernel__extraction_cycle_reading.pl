% ============================================================================
% CONSTRAINT STORY: feud_obligation_kernel__extraction_cycle_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feud_obligation_kernel__extraction_cycle_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: feud_obligation_kernel__extraction_cycle_reading
 *   human_readable: Blood-Feud Obligation Extraction Cycle (One Reading)
 *   domain: legal_anthropology/medieval_history/comparative_political_systems
 *
 * SUMMARY:
 *   This constraint story instantiates ONE READING of a contested medieval
 *   kernel: the blood-feud obligation system. The kernel is the formalized
 *   legal and cultural practice that kinship groups owe reciprocal vendetta
 *   for injuries to members — the obligation to pursue compensation or
 *   vengeance. Three structurally distinct readings of this kernel coexist in
 *   historical scholarship. THIS STORY represents the EXTRACTION-CYCLE
 *   READING: feud obligation functions as a destructive extraction mechanism
 *   that depletes productive capacity, concentrates mortality among
 *   territorial participants, and provides structural justification for royal
 *   consolidation of the violence monopoly. Under this reading, feud
 *   participants are victims of a system that extracts resources and labor
 *   toward unproductive (negative-sum) activities, while royal authority and
 *   initially regional overlords benefit from the depletion and instability
 *   feud creates. The constraint exhibits Tangled Rope classification at the
 *   primary level (both coordination benefits and asymmetric extraction
 *   present) but appears as Snare from the participant perspective (trapped
 *   by kinship obligation and honor semantics) and as Rope from the
 *   pre-consolidation overlord perspective (vendetta cycles maintain useful
 *   factional balance). The measurement trajectory shows extractiveness
 *   rising from 0.35 to 0.72 over a century, suppression intensifying from
 *   0.45 to 0.80 as royal authority consolidates, and theater ratio
 *   increasing from 0.35 to 0.55 as legal doctrine increasingly formalizes
 *   what was once a functional enforcement mechanism.
 *
 * KEY AGENTS:
 *   - Primary Feud Participants (kinship groups): Primary victims (powerless/trapped) — bear extraction costs of vendetta maintenance, mortality, resource depletion, territorial vulnerability
 *   - Regional Territorial Overlords: Secondary beneficiary (institutional/arbitrage, pre-consolidation phase) — profit from arbitrating disputes, extracting rents from feuding parties, maintaining balance of power
 *   - Royal Consolidation Authority: Primary beneficiary (institutional/constrained in the generational view) — extracts legitimacy and tax base from suppressing feud, justifies monopoly on violence, consolidates territorial control as feud-depleted regions become dependent on royal protection
 *   - Kinship Elite (during suppression): Ambiguous beneficiary (institutional/constrained) — initially benefit from honor and vendetta capital but become constrained by royal law; some transition to royal service
 *   - Non-Combatant Collateral Populations: Secondary victims (powerless/constrained) — experience economic disruption, resource diversion, mortality, territorial instability from feud cycles in their region
 *   - Post-Feudal Legal Tradition: Institutional observer (institutional/arbitrage) — preserves feud obligation in legal texts and historiography; maintains constraint through legal theater despite functional obsolescence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feud_obligation_kernel__extraction_cycle_reading, 0.58).
domain_priors:suppression_score(feud_obligation_kernel__extraction_cycle_reading, 0.72).
domain_priors:theater_ratio(feud_obligation_kernel__extraction_cycle_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feud_obligation_kernel__extraction_cycle_reading, tangled_rope).
narrative_ontology:human_readable(feud_obligation_kernel__extraction_cycle_reading, "Blood-Feud Obligation Extraction Cycle (One Reading)").
narrative_ontology:topic_domain(feud_obligation_kernel__extraction_cycle_reading, "legal_anthropology/medieval_history/comparative_political_systems").

domain_priors:requires_active_enforcement(feud_obligation_kernel__extraction_cycle_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feud_obligation_kernel__extraction_cycle_reading, '291f6611-cb25-43e2-a8ab-d4a7e8763202').
narrative_ontology:cs_kernel_codification('291f6611-cb25-43e2-a8ab-d4a7e8763202', formalized).
narrative_ontology:cs_authority_grounding('291f6611-cb25-43e2-a8ab-d4a7e8763202', extraction).
narrative_ontology:cs_interpretation_layer_present('291f6611-cb25-43e2-a8ab-d4a7e8763202').
narrative_ontology:cs_reading_relation('291f6611-cb25-43e2-a8ab-d4a7e8763202', feud_obligation_kernel__stateless_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('291f6611-cb25-43e2-a8ab-d4a7e8763202', feud_obligation_kernel__christianized_pacification_reading, influences).
narrative_ontology:cs_axiom('291f6611-cb25-43e2-a8ab-d4a7e8763202', foundational, feud_depletes_productive_capacity).
narrative_ontology:cs_axiom_status(feud_depletes_productive_capacity, holdable).
narrative_ontology:cs_axiom_grounding('291f6611-cb25-43e2-a8ab-d4a7e8763202', feud_depletes_productive_capacity, empirically_contingent).
narrative_ontology:cs_axiom('291f6611-cb25-43e2-a8ab-d4a7e8763202', foundational, royal_monopoly_benefits_from_feud_collapse).
narrative_ontology:cs_axiom_status(royal_monopoly_benefits_from_feud_collapse, holdable).
narrative_ontology:cs_axiom_grounding('291f6611-cb25-43e2-a8ab-d4a7e8763202', royal_monopoly_benefits_from_feud_collapse, empirically_contingent).
narrative_ontology:cs_reference_frame('291f6611-cb25-43e2-a8ab-d4a7e8763202', feudal_kinship_enforcement_regime).
narrative_ontology:cs_drift_state('291f6611-cb25-43e2-a8ab-d4a7e8763202', royal_consolidation_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('291f6611-cb25-43e2-a8ab-d4a7e8763202', '').
narrative_ontology:cs_kernel_id(feud_obligation_kernel__extraction_cycle_reading, feud_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__extraction_cycle_reading, royal_consolidation_authority).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__extraction_cycle_reading, kinship_elite_during_suppression_phase).
narrative_ontology:constraint_victim(feud_obligation_kernel__extraction_cycle_reading, primary_feud_participants).
narrative_ontology:constraint_victim(feud_obligation_kernel__extraction_cycle_reading, territorial_economic_productivity).
narrative_ontology:constraint_victim(feud_obligation_kernel__extraction_cycle_reading, non_combatant_collateral_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: BOUND KINSHIP GROUP (SNARE) — Trapped by honor obligation and kinship law. Exit is identity death (repudiation from lineage, loss of protection claims, economic exile). Bears full extraction cost: resources dedicated to vendetta maintenance, mortality among productive members, permanent state of mobilization. Suppression is total — kinship enforcement is internalized and externally reinforced.
constraint_indexing:constraint_classification(feud_obligation_kernel__extraction_cycle_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: LESSER NOBLE HOUSE (TANGLED ROPE) — Constrained by coordination benefits (feud alliances provide mutual defense in the absence of centralized protection) but also extracted from (resources diverted to vendetta, vulnerability to counter-vendetta cycles, inability to accumulate capital). Both extraction and coordination present; suppression via legal obligation codes is moderate-high but not total.
constraint_indexing:constraint_classification(feud_obligation_kernel__extraction_cycle_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: REGIONAL OVERLORD PRE-CONSOLIDATION (ROPE) — During the period before royal consolidation intensifies, the overlord experiences feud obligation as coordination: vendetta cycles maintain factional balance, prevent any single house from dominating, and allow the overlord to extract rents by arbitrating disputes. Benefits from the constraint without bearing primary costs. Sees the system as legitimate, stable structure.
constraint_indexing:constraint_classification(feud_obligation_kernel__extraction_cycle_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: ROYAL CONSOLIDATION AUTHORITY (TANGLED ROPE) — The emergent crown experiences feud obligation as both coordinating factor (centralizes violence under royal adjudication, justifies royal monopoly on legitimate force) and extraction mechanism (feud-depleted territories become easier to absorb, feud participants become dependent on royal protection, feud depletion justifies higher taxation to restore order). Active enforcement required: royal law must suppress private vendetta while maintaining the appearance of legitimate justice.
constraint_indexing:constraint_classification(feud_obligation_kernel__extraction_cycle_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: POST-FEUDAL LEGAL TRADITION (PITON) — Medieval legal texts and chronicles preserve feud obligation as a formal system (compensation codes, honor semantics, kinship law) long after the functional coordination purpose has been replaced by royal courts and centralized enforcement. The historical narrative maintains the constraint's legitimacy through legal theater — historians and jurists discuss feud as a necessary system in a world without central authority, even as the textual record itself demonstrates the transition to alternative enforcement. Theater ratio is moderate-high: the constraint persists in legal doctrine and historiographical interpretation despite diminished functional role.
constraint_indexing:constraint_classification(feud_obligation_kernel__extraction_cycle_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER NATURAL LAW (MOUNTAIN) — From a civilizational perspective, feud obligation could appear as an inevitable natural law: 'In the absence of centralized authority, kinship-based reciprocal justice is the only enforcement mechanism available — therefore feud obligation emerges necessarily.' However, the structural data contradicts this mountain classification. The constraint has identifiable beneficiaries (royal consolidation authority, regional overlords), victims, and suppression mechanisms — all hallmarks of constructed extraction masked as natural law. This perspective instantiates a false summit.
constraint_indexing:constraint_classification(feud_obligation_kernel__extraction_cycle_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feud_obligation_kernel__extraction_cycle_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(feud_obligation_kernel__extraction_cycle_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(feud_obligation_kernel__extraction_cycle_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(feud_obligation_kernel__extraction_cycle_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(feud_obligation_kernel__extraction_cycle_reading, TR),
    TR >= 0.70.

:- end_tests(feud_obligation_kernel__extraction_cycle_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   EXTRACTIVENESS (0.58): High-moderate. Feud obligation extracts from participants in three forms: (1) direct resource costs — maintenance of vendetta networks, mobilization capacity, fortifications; (2) opportunity costs — labor and capital diverted from productive activities (agriculture, trade, craft) toward violence and preparation; (3) mortality risk — members killed in vendetta cycles represent permanent loss of productive capacity. The extraction is not total (0.72 would be snare threshold) because some coordination benefit exists (vendetta networks provide mutual defense in absence of central authority) and participants perceive the system as legitimate (identity-fused rather than purely coercive). SUPPRESSION (0.72): High. Feud obligation is enforced through multiple mechanisms: (1) kinship law — explicit codes imposing vendetta duties; (2) honor semantics — cultural obligation and reputation damage for non-compliance; (3) royal legal codes — penalty for abandoning or compromising feud claims; (4) internalized identity — participants' sense of self is constituted through kinship obligation and honor. Early in the interval (t=0), suppression is moderate (0.45) because enforcement relies primarily on kinship/honor, with limited royal capacity. As royal consolidation intensifies (t=50-100), suppression rises to 0.72-0.80 through active legal prohibition and enforcement machinery. THEATER RATIO (0.55): Moderate-high. Early feud systems have lower theater (0.35) because vendetta cycles are functionally achieving enforcement through actual reciprocal violence — the constraint works through threat and execution. As royal consolidation proceeds, feud becomes increasingly ritualized in legal discourse: law codes elaborate compensation schedules that parallel actual vendetta practice, chronicles narrativize feud as necessary system, and surviving participants maintain vendetta identity through legal claim rather than active prosecution. By t=100, theater has risen to 0.55 — the constraint persists in legal text and historical narrative despite diminished functional enforcement role.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence. From the participant perspective (powerless/trapped), the system is a pure Snare — extraction with no perceived exit and no compensation. From the regional overlord perspective (institutional/arbitrage, pre-consolidation), the system is Rope — genuine coordination benefit that prevents dominance by any single house while allowing profitable arbitration. From the royal consolidation perspective (institutional/constrained, generational), the system is Tangled Rope — both coordination benefit (unifies the kingdom under royal justice) and extraction mechanism (depletes alternative centers of power, justifies fiscal extraction). From the post-feudal tradition perspective (institutional/arbitrage, civilizational), the system is Piton — preserved in legal text and historiography despite functional obsolescence. The analytical observer risks seeing a Mountain (inevitable natural law of stateless societies), but structural data contradicts this: the constraint has identifiable beneficiaries and suppression mechanisms inconsistent with natural law. The perspectival gap reveals that feud obligation's classification depends entirely on the observer's structural position relative to extraction flow.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality d is determined by the agent's relationship to the extraction flow. Feud participants are victims with trapped exit (d ≈ 0.95) — maximum experienced extractiveness through f(d). Royal consolidation authority is initially ambiguous (benefits from feud depletion but bears suppression costs) — approximately institutional/constrained with d ≈ 0.55 at generational timescale, moderating to d ≈ 0.35 at civilizational timescale as the system matures. Regional overlords are beneficiaries with arbitrage exit (d ≈ 0.05) — minimal experienced extraction, sometimes negative (they net gain from the constraint). Post-feudal legal tradition is institutional beneficiary (preserves its interpretive authority) with d ≈ 0.20. The canonical derivation produces these values automatically from beneficiary/victim declarations and exit options; no override needed.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by establishing that classification depends on reading choice within a contested kernel. The extraction-cycle reading produces Tangled Rope (primary classification) because it identifies both coordination function (vendetta networks provide mutual defense, royal consolidation achieves unified governance) and asymmetric extraction (participants bear resource costs, royal authority extracts fiscal benefit). No mandatrophy gap arises because the reading is internally consistent: feud obligation simultaneously coordinates (solves mutual defense problem) and extracts (depletes participants to benefit overlords and crown). The potential tension is resolved by observing that the coordination function and extraction mechanism are structurally orthogonal — the same vendetta system that achieves some coordination in the short term enables extraction in the long term, particularly as external (royal) consolidation pressure increases. The sibling readings resolve the mandatrophy differently: the stateless-coordination reading would classify as Rope or pure Tangled Rope by emphasizing symmetric benefits; the Christianized-pacification reading would classify as Piton by emphasizing theological theater replacing functional enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    feud_vs_compensation_regime,
    'Is the extraction cycle intrinsic to feud obligation, or is it a contingent property of how feud systems fail to transition to compensation-based settlement?',
    'Comparative historical analysis: societies that developed formalized compensation codes (wergeld, blood-price systems) vs those that devolved into perpetual vendetta. Measurement of cycle duration before transition or state collapse.',
    'If extraction is contingent on failed transition: the constraint is a Tangled Rope with a sunset, not a stable system. If extraction is structural to kinship enforcement: the constraint is a more fundamental Snare from the participant perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(feud_vs_compensation_regime, empirical, 'Whether feud extraction is intrinsic or contingent on failed transitions').

omega_variable(
    royal_consolidation_causality,
    'Does royal consolidation authority benefit from feud depletion, or does feud depletion merely enable consolidation that would have occurred through other mechanisms?',
    'Counterfactual analysis: timeline comparison between regions with intense feud cycles and regions with early centralization. Measurement of royal tax extraction increases correlating with feud suppression periods.',
    'If causal beneficiary: royal authority enters the beneficiary set and the constraint is Tangled Rope with asymmetric extraction. If merely enabled by: royal authority is a secondary beneficiary of a system that would have collapsed anyway, reducing the extraction reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(royal_consolidation_causality, empirical, 'Whether royal consolidation benefits from or merely exploits feud depletion').

omega_variable(
    kinship_enforcement_internalization,
    'To what extent is suppression of feud obligation structural (external legal codes, royal force) versus internalized (participants believe honor/kinship obligation is intrinsically binding)?',
    'Textual analysis of law codes vs heroic narratives: do legal texts claim to enforce pre-existing obligation or to create new obligation? Do participants flee to uncentralized regions or embrace centralization? Post-suppression generational analysis: do participants maintain vendetta identity after enforcement ceases?',
    'If internalized: suppression (0.72) may underestimate binding force — participants carry obligation identity even after external enforcement collapses, making exit functionally impossible. If structural: suppression measures effectiveness of royal enforcement, and alternative coordination structures could replace feud if enforcement ceased.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kinship_enforcement_internalization, empirical, 'Whether suppression is structural or internalized in kinship identity').

omega_variable(
    kernel_reading_contest,
    'Which reading of the feud obligation kernel is correct: extraction cycle, stateless coordination, or Christianized pacification?',
    'This omega documents the committer-frame ambiguity. The three readings coexist as live interpretations in historical scholarship. Extraction-cycle reading (this file) emphasizes resource depletion and royal benefit. Stateless-coordination reading emphasizes genuine equilibrium-maintaining function in absence of central authority. Christianized-pacification reading emphasizes theological delegitimization of honor culture. All three readings are supported by subsets of the historical evidence.',
    'If extraction-cycle reading is correct: feud is primarily extractive, royal consolidation is justified, and transition to centralized authority is welfare-improving. If stateless-coordination reading is correct: feud serves genuine function, and royal consolidation imposes costs through monopoly rent extraction. If Christianized-pacification reading is correct: feud is delegitimized by theological reframing, and transition is achieved through belief-change, not force. Classification differs substantially across readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Contest between extraction-cycle, stateless-coordination, and Christianized-pacification readings of the feud kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feud_obligation_kernel__extraction_cycle_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_extr_theater_t0, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(feud_extr_theater_t50, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 50, 0.48).
narrative_ontology:measurement(feud_extr_theater_t100, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 100, 0.55).

% Extraction over time
narrative_ontology:measurement(feud_extr_extract_t0, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(feud_extr_extract_t50, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 50, 0.58).
narrative_ontology:measurement(feud_extr_extract_t100, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 100, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(feud_extr_suppress_t0, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(feud_extr_suppress_t50, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 50, 0.72).
narrative_ontology:measurement(feud_extr_suppress_t100, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 100, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feud_obligation_kernel__extraction_cycle_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(feud_obligation_kernel__extraction_cycle_reading, 0.12).
narrative_ontology:affects_constraint(feud_obligation_kernel__extraction_cycle_reading, feud_obligation_kernel__stateless_coordination_reading).
narrative_ontology:affects_constraint(feud_obligation_kernel__extraction_cycle_reading, feud_obligation_kernel__christianized_pacification_reading).

% DUAL FORMULATION NOTE:
% The feud obligation kernel has three structurally distinct constraint readings. This story (extraction-cycle) emphasizes depletion and royal benefit. The stateless-coordination reading (sibling) emphasizes equilibrium maintenance and mutual defense. The Christianized-pacification reading (sibling) emphasizes theological delegitimization. Each reading has distinct epsilon, beneficiary/victim structure, and classification. They are linked via network.affects_constraints and should not be merged into a single story — the ε-invariance principle requires separate constraint files for readings with measurably different extraction structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
