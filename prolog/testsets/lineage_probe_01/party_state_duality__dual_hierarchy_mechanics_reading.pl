% ============================================================================
% CONSTRAINT STORY: party_state_duality__dual_hierarchy_mechanics_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_party_state_duality__dual_hierarchy_mechanics_reading, []).

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
 *   constraint_id: party_state_duality__dual_hierarchy_mechanics_reading
 *   human_readable: Party-State Duality: Dual Hierarchy Mechanics Reading
 *   domain: legal/doctrinal/constitutional
 *
 * SUMMARY:
 *   The party-state duality in the Soviet Union operated as a dual hierarchy
 *   where every state organ—from the all-union level down to factory
 *   committees—had a parallel party shadow structure controlling its
 *   function. Decisions were nominally made in the state committee (soviet)
 *   and formally ratified in the chamber, but the real authority flow ran
 *   through party channels: nomenklatura appointees determined who held state
 *   positions, party fractions within the soviet blocked decisions before
 *   they reached the floor, and the party committee's decision was the actual
 *   binding choice. The 1936 Stalin Constitution's Article 126 nominally
 *   guaranteed free elections and soviet autonomy, but the mechanics reading
 *   reveals this as performative: the constraint extracted the legitimacy and
 *   autonomy of the soviet system while leaving the formal apparatus intact.
 *   This is ONE reading of the contested 'party-state duality' kernel. Other
 *   readings focus on Article 126 as the constitutional keyhole
 *   (article_126_keyhole_reading) or treat the 1936 text as a descriptive
 *   anatomy rather than a constraint (description_not_constraint_reading).
 *   The mechanics reading identifies the dual hierarchy itself as the
 *   extractive structure.
 *
 * KEY AGENTS:
 *   - Central Party Apparatus (politburo, party secretariat, cadres administration): Primary beneficiary (institutional/arbitrage) — controls nomenklatura appointments and maintains centralized direction through party channels
 *   - Soviet Nominal Authority (soviet chairs, state administrators, elected deputies): Primary victim (powerless/trapped) — bear the suppression of autonomous function while maintaining the fiction of state power
 *   - Nomenklatura Controllers (party secretaries, regional cadres chiefs): Secondary beneficiary (institutional/arbitrage) — administer the dual-track appointments system
 *   - Soviet Intelligentsia (scientists, engineers, artists): Ambiguous (powerful/mobile) — benefit from state resource allocation but suppressed by ideological oversight; some exit options (emigration, foreign work)
 *   - Mid-Level Administrators (factory managers, collective farm chairs, regional deputies): Secondary victim (moderate/constrained) — structurally trapped but with surface agency within permitted bandwidth
 *   - Civilizational Analyst: Observes the system (analytical/analytical) — risks naturalizing the dual hierarchy as an inevitable feature of centralized planning rather than an extractive choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(party_state_duality__dual_hierarchy_mechanics_reading, 0.68).
domain_priors:suppression_score(party_state_duality__dual_hierarchy_mechanics_reading, 0.82).
domain_priors:theater_ratio(party_state_duality__dual_hierarchy_mechanics_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(party_state_duality__dual_hierarchy_mechanics_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(party_state_duality__dual_hierarchy_mechanics_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(party_state_duality__dual_hierarchy_mechanics_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(party_state_duality__dual_hierarchy_mechanics_reading, snare).
narrative_ontology:human_readable(party_state_duality__dual_hierarchy_mechanics_reading, "Party-State Duality: Dual Hierarchy Mechanics Reading").
narrative_ontology:topic_domain(party_state_duality__dual_hierarchy_mechanics_reading, "legal/doctrinal/constitutional").

domain_priors:requires_active_enforcement(party_state_duality__dual_hierarchy_mechanics_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(party_state_duality__dual_hierarchy_mechanics_reading, '60134f53-02b8-4420-a1e8-c7afba3a356e').
narrative_ontology:cs_kernel_codification('60134f53-02b8-4420-a1e8-c7afba3a356e', formalized).
narrative_ontology:cs_authority_grounding('60134f53-02b8-4420-a1e8-c7afba3a356e', extraction).
narrative_ontology:cs_interpretation_layer_present('60134f53-02b8-4420-a1e8-c7afba3a356e').
narrative_ontology:cs_reading_relation('60134f53-02b8-4420-a1e8-c7afba3a356e', party_state_duality__article_126_keyhole_reading, coexists_with).
narrative_ontology:cs_reading_relation('60134f53-02b8-4420-a1e8-c7afba3a356e', party_state_duality__description_not_constraint_reading, influences).
narrative_ontology:cs_axiom('60134f53-02b8-4420-a1e8-c7afba3a356e', foundational, dual_hierarchy_operationally_suppressive).
narrative_ontology:cs_axiom_status(dual_hierarchy_operationally_suppressive, holdable).
narrative_ontology:cs_axiom_grounding('60134f53-02b8-4420-a1e8-c7afba3a356e', dual_hierarchy_operationally_suppressive, empirically_contingent).
narrative_ontology:cs_axiom('60134f53-02b8-4420-a1e8-c7afba3a356e', foundational, soviet_autonomy_illusory_under_nomenklatura).
narrative_ontology:cs_axiom_status(soviet_autonomy_illusory_under_nomenklatura, holdable).
narrative_ontology:cs_axiom_grounding('60134f53-02b8-4420-a1e8-c7afba3a356e', soviet_autonomy_illusory_under_nomenklatura, empirically_contingent).
narrative_ontology:cs_reference_frame('60134f53-02b8-4420-a1e8-c7afba3a356e', soviet_democracy_nominal_framework).
narrative_ontology:cs_drift_state('60134f53-02b8-4420-a1e8-c7afba3a356e', late_soviet_period_1970s_1980s, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('60134f53-02b8-4420-a1e8-c7afba3a356e', '').
narrative_ontology:cs_kernel_id(party_state_duality__dual_hierarchy_mechanics_reading, party_state_duality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(party_state_duality__dual_hierarchy_mechanics_reading, central_party_apparatus).
narrative_ontology:constraint_beneficiary(party_state_duality__dual_hierarchy_mechanics_reading, nomenklatura_controllers).
narrative_ontology:constraint_victim(party_state_duality__dual_hierarchy_mechanics_reading, soviet_nominal_authority).
narrative_ontology:constraint_victim(party_state_duality__dual_hierarchy_mechanics_reading, state_executive_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SOVIET EXECUTIVE (SNARE) — A soviet chair or state administrator faces total suppression. Nominally in authority (Article 126 declares the soviet as the state power), but every decision flows through party channels: nomenklatura appointments precede formal elections, party groups within the soviet block decisions before chamber votes, and the party committee ratifies what the soviet formally enacts. The executive has no exit — removal from the dual-track position means elimination from power entirely. Maximum experienced extraction: the appearance of authority without its substance. The constraint is not extracting money or labor but extracting the legitimacy currency of the soviet system itself.
constraint_indexing:constraint_classification(party_state_duality__dual_hierarchy_mechanics_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MID-LEVEL ADMINISTRATOR (SNARE) — Factory managers, collective farm chairs, regional soviet deputies: structurally trapped but with slightly more surface agency than the top executive. Can improve conditions within permitted bandwidth but cannot deviate from the shadow hierarchy. The constraint is less absolutely suppressive (some real discretion exists) but still fundamentally extractive — the administrator's resources, time, and legitimacy are diverted toward maintaining the dual-track fiction rather than toward actual state function. Exit requires betraying the whole apparatus.
constraint_indexing:constraint_classification(party_state_duality__dual_hierarchy_mechanics_reading, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CENTRAL PARTY APPARATUS (ROPE) — The constraint apparatus itself (party secretariat, politburo, cadres administration) experiences the dual hierarchy as pure coordination: the nomenklatura system distributes authority without relinquishing control, party groups ensure coherence across the state apparatus, and the ratification process maintains hierarchy while preserving the fiction of soviet democracy. This is experienced as legitimate governance — the apparatus has arbitrage options (can deploy resources, shift personnel, reshape the nomenclature) and sees the constraint as a functional coordination mechanism. From this perspective: high efficiency, no coercion experienced, and all pieces moving as intended.
constraint_indexing:constraint_classification(party_state_duality__dual_hierarchy_mechanics_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: SOVIET INTELLIGENTSIA (TANGLED ROPE) — Scientists, engineers, artists, and intellectuals occupy an ambiguous position. They benefit from state resource allocation and the stability the dual hierarchy provides (predictable career ladders, research funding, access to publishing and exhibitions), but they are also suppressed: creative work must pass both functional review (does it work?) and party review (is it ideologically safe?). Exit is partly mobile (some can emigrate, some can work abroad) but costly. The constraint is mixed — genuine coordination of technical work alongside extraction of ideological compliance. Chi is moderate: some real functional benefit, substantial suppression, and clear asymmetry.
constraint_indexing:constraint_classification(party_state_duality__dual_hierarchy_mechanics_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: CIVILIZATIONAL ANALYST / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational view, the dual hierarchy might be seen as an immutable feature of large-scale socialist governance: any attempt to centrally plan an economy requires information control and cadre coordination that naturally bifurcates into party machinery and state administration. This perspective treats the constraint as emergent from the logic of centralized planning itself — a structural necessity, not an extractive choice. However, this reading naturalizes what the mechanics reading reveals as a contingent institutional arrangement. The engine will flag this as a false summit: the naturalizing observer is itself benefiting from or captured by the apparatus they claim to be describing as inevitable.
constraint_indexing:constraint_classification(party_state_duality__dual_hierarchy_mechanics_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: DEGRADED SOVIET STATE FORM (PITON) — By the 1980s, the dual-hierarchy constraint had become substantially performative. The fiction that the soviet was autonomous had eroded; the party groups still went through ratification motions, but the apparatus understood that this was ritual without substance. The nomenklatura system persisted through institutional inertia — removing it would have required restructuring the entire cadre system — even though its primary coordination function had degraded. The theater ratio rose as the actual suppression mechanism migrated toward security services and ideological enforcement rather than the elegant dual-track administrative structure. This perspective sees a constraint that has become mostly maintenance of its own form, not effective extraction.
constraint_indexing:constraint_classification(party_state_duality__dual_hierarchy_mechanics_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(party_state_duality__dual_hierarchy_mechanics_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(party_state_duality__dual_hierarchy_mechanics_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(party_state_duality__dual_hierarchy_mechanics_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(party_state_duality__dual_hierarchy_mechanics_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(party_state_duality__dual_hierarchy_mechanics_reading, TR),
    TR >= 0.70.

:- end_tests(party_state_duality__dual_hierarchy_mechanics_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts the legitimacy currency of soviet democracy itself. The nominal authority of the soviet is systematically suppressed through the shadow hierarchy, yet the soviet leadership must publicly defend the system as democratic. Over the 30-year measurement interval, extractiveness rose from 0.60 to 0.70 as the apparatus refined the dual-track mechanics and centralized control deepened. Suppression (0.82): Very high. Soviet administrators face multiple suppressive barriers: they cannot exit without elimination from power, cannot deviate from party direction without serious consequences, and cannot appeal to legal or constitutional authority because the dual hierarchy has effectively overwritten the constitution's promises. The nomenklatura system blocks alternative career paths, party groups block alternative decisions, and the ratification process enforces the hierarchy. Suppression increased slightly over the interval as the apparatus invested in security and enforcement infrastructure. Theater ratio (0.65): Moderately high. The dual hierarchy was partially theater: the formal soviet structure persisted and its rituals were performed, but the real authority was in the shadow party channels. However, this is not pure piton-level theater—the mechanics were genuinely operational, not merely decorative. The theater ratio rose over time as the apparatus became more confident in the fiction and reduced the functional visibility of party control. By year 30, the theater had become substantial but not overwhelming (piton threshold is 0.70).
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits a dramatic perspectival gap across power levels. The central apparatus sees coordination (Rope) and experiences the dual hierarchy as efficient governance. The soviet executive sees suppression (Snare) and experiences it as the systematic extraction of autonomy. The intelligentsia sees mixed coordination and extraction (Tangled Rope)—genuine resource allocation alongside ideological suppression. The analyst risks seeing inevitability (Mountain) when observing from a civilizational distance, but the mechanics reading reveals this as false summit: the dual hierarchy is a contingent institutional arrangement, not a law of nature. The gap is produced by the asymmetry of beneficiary vs. victim positions and the effectiveness of the shadow structure in hiding extraction from public discourse.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural position within the dual hierarchy. The central party apparatus benefits from nomenklatura appointments (d ≈ 0.05 — full beneficiary) and experiences arbitrage exit options, producing low chi. The soviet executive is victimized by suppression of autonomy (d ≈ 0.95 — full target) and trapped by the dual hierarchy's elimination of alternatives, producing high chi. The mid-level administrator is partially victimized (d ≈ 0.70) but has constrained options, producing moderate chi. The intelligentsia benefits from resource allocation but is suppressed by ideology (d ≈ 0.55 — symmetric), producing moderate chi. The apparatus itself claims to experience no extraction (d ≈ 0.05) because it genuinely perceives the dual hierarchy as coordination, not control. This divergence in experienced directionality is the signature of the snare: the beneficiary's low-chi perception depends on naturalizing what the victim experiences as systematic extraction.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dual_hierarchy_as_mechanism_vs_symptom,
    'Is the party-state duality the causal mechanism of suppression, or is it a symptom/expression of a prior commitment to central control?',
    'Historical counterfactual analysis: if the party apparatus had retained central control but merged party and state bureaucracies into a single hierarchy, would the suppression have been materially different in magnitude or mechanism?',
    'If mechanism: the constraint is correctly identified as the extractive apparatus (snare). If symptom: the true constraint is the commitment to central planning, and the party-state duality is one possible implementation of it. Changes how to model exit options and beneficiary leverage.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dual_hierarchy_as_mechanism_vs_symptom, conceptual, 'Whether dual hierarchy is the suppressive mechanism or a symptom of prior commitment to central control').

omega_variable(
    soviet_nominal_autonomy_perceived_reality,
    'Did soviet chair and state administrators genuinely believe they had autonomous authority, or did they understand from the start that the dual hierarchy was suppressive?',
    'Analysis of internal party communications, memoirs, and archival evidence of administrator understanding and resistance; comparison of public declarations vs private correspondence',
    'If genuinely believed: victims experienced mountain (unchangeable law of governance) rather than snare (extractive structure). If understood: suppression was maximized through misrepresentation, and snare classification holds. Changes the cognitive phenomenology of the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(soviet_nominal_autonomy_perceived_reality, empirical, 'Whether soviet administrators perceived autonomy or understood suppression').

omega_variable(
    nomenklatura_efficiency_vs_extraction,
    'Did the nomenklatura system produce better personnel selection (functional efficiency) than would have occurred with state-autonomous appointment? Or was efficiency merely the cover story for control extraction?',
    'Comparative analysis of administrative performance metrics: personnel tenure, turnover, error rates, and functional outcomes in nomenklatura-controlled positions vs. nominally autonomous state positions; cross-system comparison with other large bureaucracies',
    'If genuinely more efficient: coordination function is real, and classification should shift toward tangled rope. If purely extractive: snare classification confirmed. Determines whether beneficiary claim is functionally justified or purely ideological.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(nomenklatura_efficiency_vs_extraction, empirical, 'Whether nomenklatura system produced genuine efficiency or was pure control extraction').

omega_variable(
    kernel_reading_ambiguity,
    'This constraint is ONE reading of the contested kernel ''party-state duality''. The sibling reading ''article_126_keyhole_reading'' focuses on how Article 126 names the party as the ''leading core'' in a single clause. Are these readings describing the same constraint from different angles, or are they structurally different constraints that should be decomposed?',
    'If the article_126_keyhole reading produces a different epsilon (base extractiveness) when focused on the constitutional text rather than the administrative mechanics, they are different constraints. If epsilon is the same but perspectives differ, they are readings of the same constraint.',
    'If decomposed: each reading gets its own story with linked network edges. If unified: the ambiguity belongs in the omega variables. The ε-invariance principle applies: if measuring differently produces different epsilon, you have two constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether article_126 and dual_hierarchy_mechanics are readings of same constraint or separate constraints').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(party_state_duality__dual_hierarchy_mechanics_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(psd_tr_t0, party_state_duality__dual_hierarchy_mechanics_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(psd_tr_t15, party_state_duality__dual_hierarchy_mechanics_reading, theater_ratio, 15, 0.55).
narrative_ontology:measurement(psd_tr_t30, party_state_duality__dual_hierarchy_mechanics_reading, theater_ratio, 30, 0.65).

% Extraction over time
narrative_ontology:measurement(psd_be_t0, party_state_duality__dual_hierarchy_mechanics_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(psd_be_t15, party_state_duality__dual_hierarchy_mechanics_reading, base_extractiveness, 15, 0.68).
narrative_ontology:measurement(psd_be_t30, party_state_duality__dual_hierarchy_mechanics_reading, base_extractiveness, 30, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(psd_su_t0, party_state_duality__dual_hierarchy_mechanics_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(psd_su_t15, party_state_duality__dual_hierarchy_mechanics_reading, suppression_requirement, 15, 0.82).
narrative_ontology:measurement(psd_su_t30, party_state_duality__dual_hierarchy_mechanics_reading, suppression_requirement, 30, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(party_state_duality__dual_hierarchy_mechanics_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(party_state_duality__dual_hierarchy_mechanics_reading, article_126_constitutional_naming).
narrative_ontology:affects_constraint(party_state_duality__dual_hierarchy_mechanics_reading, nomenklatura_cadre_distribution).
narrative_ontology:affects_constraint(party_state_duality__dual_hierarchy_mechanics_reading, soviet_legitimacy_fiction).

% DUAL FORMULATION NOTE:
% The party-state duality is a constraint family with three decomposable stories: (1) dual_hierarchy_mechanics_reading (this story) — the operational suppression mechanics, epsilon ≈ 0.68, snare. (2) article_126_keyhole_reading — the constitutional text's role in legitimizing the hierarchy, potentially different epsilon focusing on textual rather than behavioral extractiveness. (3) description_not_constraint_reading — whether the 1936 Constitution describes or constrains, a meta-constraint on the kernel itself. The mechanics reading is downstream of the textual keyhole (the keyhole creates the permission structure for the mechanics) and upstream of the legitimacy fiction (the mechanics sustain the fiction).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(party_state_duality__dual_hierarchy_mechanics_reading, institutional, 0.02).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
