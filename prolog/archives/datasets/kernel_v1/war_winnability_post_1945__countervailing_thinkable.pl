% ============================================================================
% CONSTRAINT STORY: war_winnability_post_1945__countervailing_thinkable
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_winnability_post_1945__countervailing_thinkable, []).

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
 *   constraint_id: war_winnability_post_1945__countervailing_thinkable
 *   human_readable: War Winnability Constraint: Countervailing Strategy Reading
 *   domain: strategic_studies/nuclear_deterrence/international_relations
 *
 * SUMMARY:
 *   This constraint instantiates the 'countervailing_thinkable' reading of
 *   the kernel 'war winnability post-1945.' The reading holds that nuclear
 *   weapons raised the costs of total war but did not eliminate the logical
 *   or operational possibility of limited victory through precise
 *   counterforce targeting. Under this reading, strategic planning for
 *   nuclear war remains coherent: military forces continue to be optimized
 *   for damage limitation, force survivability, and escalation control. The
 *   constraint's structure is a hybrid: genuine coordination (military
 *   establishments solving the problem of credible deterrence), mixed with
 *   asymmetric extraction (military-industrial complex benefits from
 *   winnability doctrine persisting; arms control regimes are undermined;
 *   civilian populations are trapped within vulnerability assumptions). The
 *   constraint's theater component (0.55) reflects that winnability discourse
 *   relies on highly simplified modeling of adversary behavior, communication
 *   under stress, and damage assessment — models that are analytically
 *   degraded but strategically persistent. The suppression component (0.68)
 *   reflects that counterarguments to winnability (mutual assured
 *   destruction, strategic instability from first-strike incentives, arms
 *   race dynamics) are actively suppressed through classification,
 *   institutional gatekeeping of strategic studies, and the career risks of
 *   challenging the operational necessity of nuclear forces. The
 *   extractiveness trajectory shows slow accumulation: as military technology
 *   advances (precision targeting, AI-enabled kill chains), the technical
 *   case for winnability becomes subtly stronger, even as the political case
 *   for acknowledging winnability weakens (resulting in higher theater
 *   ratio). This reading coexists with the 'deterrence_unthinkable' reading
 *   (which holds that winnability became logically incoherent after 1945) and
 *   the 'rhetorical_contraction' reading (which holds that winnability
 *   remained operationally planned but became rhetorically taboo). The three
 *   readings are held by different institutional actors:
 *   countervailing_thinkable dominates in military strategic planning
 *   establishments; deterrence_unthinkable dominates in some diplomatic and
 *   disarmament communities; rhetorical_contraction describes the
 *   public-private gap in strategic doctrine.
 *
 * KEY AGENTS:
 *   - Military Strategic Planning Establishments: Primary beneficiary (institutional/arbitrage) — winnability doctrine enables continued strategic force justification and modernization planning
 *   - Military-Industrial Complex: Primary beneficiary (institutional/arbitrage) — benefits from decades-long procurement cycles justified by operational winnability requirements
 *   - Arms Control Advocates and Regimes: Primary victim (organized/constrained) — arms control aspirations are structurally undermined by parallel winnability planning that assumes agreements are not binding in conflict
 *   - Civilian Populations: Maximum victim (powerless/trapped) — trapped within vulnerability assumptions; bodies are the optimization surface for counterforce targeting calculations
 *   - Rival Nuclear Powers: Dual beneficiary-victim (powerful/constrained) — each benefits from shared winnability logic (mutual justification for deterrence), but coordinated extraction keeps both locked in reciprocal strategic planning
 *   - International Relations Academia: Institutional maintenance actor (institutional/arbitrage) — provides scholarly legitimation for winnability discourse through strategic studies programs and policy advising
 *   - Disarmament Movements: Organized counter-actor (organized/mobile) — see winnability doctrine as collapsing framework with identifiable sunset as proliferation dynamics and strategic instability increase
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_winnability_post_1945__countervailing_thinkable, 0.58).
domain_priors:suppression_score(war_winnability_post_1945__countervailing_thinkable, 0.68).
domain_priors:theater_ratio(war_winnability_post_1945__countervailing_thinkable, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, extractiveness, 0.58).
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_winnability_post_1945__countervailing_thinkable, tangled_rope).
narrative_ontology:human_readable(war_winnability_post_1945__countervailing_thinkable, "War Winnability Constraint: Countervailing Strategy Reading").
narrative_ontology:topic_domain(war_winnability_post_1945__countervailing_thinkable, "strategic_studies/nuclear_deterrence/international_relations").

domain_priors:requires_active_enforcement(war_winnability_post_1945__countervailing_thinkable).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_winnability_post_1945__countervailing_thinkable, '9da70a7b-dfb7-4b03-83e7-06351d43637c').
narrative_ontology:cs_kernel_codification('9da70a7b-dfb7-4b03-83e7-06351d43637c', formalized).
narrative_ontology:cs_authority_grounding('9da70a7b-dfb7-4b03-83e7-06351d43637c', extraction).
narrative_ontology:cs_interpretation_layer_present('9da70a7b-dfb7-4b03-83e7-06351d43637c').
narrative_ontology:cs_reading_relation('9da70a7b-dfb7-4b03-83e7-06351d43637c', war_winnability_post_1945__deterrence_unthinkable, coexists_with).
narrative_ontology:cs_reading_relation('9da70a7b-dfb7-4b03-83e7-06351d43637c', war_winnability_post_1945__rhetorical_contraction, influences).
narrative_ontology:cs_axiom('9da70a7b-dfb7-4b03-83e7-06351d43637c', foundational, counterforce_disarming_possible).
narrative_ontology:cs_axiom_status(counterforce_disarming_possible, holdable).
narrative_ontology:cs_axiom_grounding('9da70a7b-dfb7-4b03-83e7-06351d43637c', counterforce_disarming_possible, empirically_contingent).
narrative_ontology:cs_axiom('9da70a7b-dfb7-4b03-83e7-06351d43637c', foundational, escalation_control_maintainable).
narrative_ontology:cs_axiom_status(escalation_control_maintainable, holdable).
narrative_ontology:cs_axiom_grounding('9da70a7b-dfb7-4b03-83e7-06351d43637c', escalation_control_maintainable, empirically_contingent).
narrative_ontology:cs_reference_frame('9da70a7b-dfb7-4b03-83e7-06351d43637c', credible_deterrence_posture).
narrative_ontology:cs_drift_state('9da70a7b-dfb7-4b03-83e7-06351d43637c', contemporary_proliferation_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9da70a7b-dfb7-4b03-83e7-06351d43637c', '').
narrative_ontology:cs_kernel_id(war_winnability_post_1945__countervailing_thinkable, war_winnability_post_1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__countervailing_thinkable, military_industrial_complex).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__countervailing_thinkable, strategic_planning_establishments).
narrative_ontology:constraint_victim(war_winnability_post_1945__countervailing_thinkable, arms_control_regimes).
narrative_ontology:constraint_victim(war_winnability_post_1945__countervailing_thinkable, civilian_populations).
narrative_ontology:constraint_victim(war_winnability_post_1945__countervailing_thinkable, strategic_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CIVILIAN POPULATIONS (SNARE) — Trapped within nuclear deterrence logic that treats winnability as operational reality. No exit from geographic vulnerability; no agency in strategic planning. Bear maximum extraction: their bodies are the optimization surface for counterforce targeting calculations. Extraction is absolute and inescapable.
constraint_indexing:constraint_classification(war_winnability_post_1945__countervailing_thinkable, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ARMS CONTROL ADVOCATES (TANGLED ROPE) — Organized but constrained by the countervailing strategy framework. Benefit from coordination of international nonproliferation agreements, but these agreements are actively undermined by the winnability doctrine they are attempting to constrain. The constraint extracts from their institutional mission (rendering arms control aspirational rather than binding). High suppression from strategic planning continuity.
constraint_indexing:constraint_classification(war_winnability_post_1945__countervailing_thinkable, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: MILITARY STRATEGIC PLANNING ESTABLISHMENTS (ROPE) — Primary beneficiaries (institutional/arbitrage). Experience winnability doctrine as pure coordination: it enables continued deterrence planning, operational doctrine development, and strategic force justification. The doctrine solves their core problem: how to maintain nuclear credibility and deterrent posture when first-use appears irrational. Extraction flows toward this agent. Low experienced chi despite moderate base extractiveness.
constraint_indexing:constraint_classification(war_winnability_post_1945__countervailing_thinkable, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MILITARY-INDUSTRIAL COMPLEX (ROPE) — Beneficiary (institutional/arbitrage). Winnability doctrine maintains the operational necessity of strategic modernization programs, precision targeting infrastructure, damage assessment capabilities, and command-and-control systems. The constraint coordinates the production of these systems: winnability thinking enables the justification for decades-long procurement cycles.
constraint_indexing:constraint_classification(war_winnability_post_1945__countervailing_thinkable, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: RIVAL NUCLEAR POWERS (TANGLED ROPE) — Constrained by mutual adoption of countervailing strategy. Each power benefits from the coordination of shared strategic doctrine (both can claim deterrence logic is rational; both can justify modernization). But the coordination is purely negative — a shared framework for mutual extraction. Each is simultaneously beneficiary and victim of winnability thinking. High suppression from the lock-in to reciprocal strategic planning.
constraint_indexing:constraint_classification(war_winnability_post_1945__countervailing_thinkable, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: INTERNATIONAL RELATIONS ACADEMIA (PITON) — Maintains winnability discourse through scholarly legitimation, strategic studies programs, and policy advisory roles. The activity is substantially performative: strategic models of 'limited war' and 'counterforce victory' are intellectually degraded (assume away their own contradictions) but persist through institutional inertia and career path dependence. Theater ratio reflects the gap between the analytical sophistication demanded by the models and their actual explanatory or predictive power.
constraint_indexing:constraint_classification(war_winnability_post_1945__countervailing_thinkable, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: DISARMAMENT MOVEMENTS (SCAFFOLD) — Organized agents (peace movements, nuclear abolition coalitions) see winnability doctrine as a temporary, collapsing framework. The scaffold derives from a real temporal dynamic: winnability thinking is increasingly incoherent as both proliferation and technical instability increase. The movement's exit path is reframing — from deterrence logic to elimination logic. Theater is low because the coordination function (international mobilization against nuclear war) is genuine.
constraint_indexing:constraint_classification(war_winnability_post_1945__countervailing_thinkable, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (MOUNTAIN / FALSE SUMMIT CANDIDATE) — From a civilizational/universal view, winnability remains mathematically coherent at certain force levels and targeting strategies. The constraint (nuclear weapons) does not logically eliminate the possibility of limited victory under specific operational assumptions. This perspective risks naturalizing winnability as an immutable consequence of deterrence physics. However, the structural data reveals beneficiaries (military-industrial complex, strategic establishments) who benefit from the winnability framing remaining viable. Engine false-summit detection will flag this.
constraint_indexing:constraint_classification(war_winnability_post_1945__countervailing_thinkable, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_winnability_post_1945__countervailing_thinkable_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(war_winnability_post_1945__countervailing_thinkable, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(war_winnability_post_1945__countervailing_thinkable, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_winnability_post_1945__countervailing_thinkable, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(war_winnability_post_1945__countervailing_thinkable, TR),
    TR >= 0.70.

:- end_tests(war_winnability_post_1945__countervailing_thinkable_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The winnability doctrine extracts from arms control regimes (active undermining of nonproliferation agreements through parallel modernization), from strategic stability (creation of first-strike incentives and escalation dynamics), and from civilian populations (who are treated as optimization parameters in damage calculations). The extraction is not absolute (0.7+) because military strategic planning establishments also genuinely coordinate deterrence logic — there is a real coordination function alongside the extraction. Suppression (0.68): High. Suppression operates through multiple mechanisms: (1) Classification of strategic planning documents — winnability calculations remain largely secret; (2) Institutional gatekeeping — challenging the winnability framework within military or defense establishment is career-limiting; (3) Rhetorical taboo — public discourse treats winnability as unspeakable while strategy documents assume it (rhetorical contraction of the debate space); (4) Epistemological closure — strategic models are protected from falsification by treating them as games of incomplete information where adversary responses cannot be known in advance. Theater ratio (0.55): Moderate. The winnability discourse is partially theater (simplified strategic models, unrealistic assumptions about command survivability and decision-making under extreme stress) but also partially functional (does enable military planning and deterrent posture). The ratio has increased over time as weapons precision has increased (making winnability calculations marginally more plausible) but strategic doctrine has become more complex (requiring more sophisticated mathematical theater to sustain credibility).
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces a stark perspectival divergence. Military strategic establishments see winnability doctrine as pure coordination (Rope) — it solves the problem of maintaining credible deterrence and enables force planning. The military-industrial complex sees identical structure as pure benefit (Rope) — winnability keeps modernization programs justified. Arms control advocates see the same structure as pure extraction (Tangled Rope with extraction dominance) — winnability doctrine actively undermines their institutional mission. Civilian populations see snare (Snare) — they are trapped within vulnerability assumptions and have no exit or agency. Rival powers see mutual lock-in (Tangled Rope) — both benefit from shared winnability logic but are locked into reciprocal modernization. Disarmament movements see a temporary structure with a sunset (Scaffold) — winnability doctrine is incoherent under proliferation conditions and will eventually collapse. The analytical observer risks seeing winnability as a natural law of nuclear physics (Mountain / False Summit) — treating operational possibilities as inevitable institutional arrangements. The perspectival gap reveals that the constraint's structure depends entirely on which agent's interests are being optimized for: if you benefit from strategic planning continuity (military establishment), it appears to be pure coordination; if you bear the existential risk (civilian populations), it appears to be pure extraction; if you see the framework as temporally limited (disarmament movements), it appears to be a scaffold. No single type is 'correct' — the presheaf over the observation site is the actual structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is computed from beneficiary/victim declarations and exit options. Military strategic establishments (institutional/arbitrage) are beneficiaries with high exit capacity — they can shift doctrine or force structure if needed. Their d is low (~0.10), producing negative or near-zero f(d), so their experienced extractiveness chi is near zero despite the base extractiveness of 0.58. Arms control advocates (organized/constrained) are victims with limited exit — they can advocate for different regimes but cannot exit the constraint's domain. Their d is high (~0.75), producing f(d) ≈ 1.15, so their experienced chi is amplified above base extractiveness. Civilian populations (powerless/trapped) have maximum d (~0.95), producing f(d) ≈ 1.42, so their experienced chi is severely amplified: 0.58 × 1.42 × 1.2 (global scope) = 0.99 — near-total experienced extraction. Rival powers are simultaneously beneficiary (from shared deterrence logic) and victim (from mutual lock-in) — directionality derivation treats them as mixed, producing d ≈ 0.55, f(d) ≈ 0.75, moderate experienced chi. The gap between military establishment perspective (chi << base epsilon) and civilian population perspective (chi ≈ 0.99) reveals the core asymmetry: the same constraint is a minor coordination mechanism for planners and an existential trap for the vulnerable.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that winnability doctrine is simultaneously (1) a genuine coordination mechanism (military establishments need a framework for deterrence logic), (2) an extraction device (benefits military-industrial complex, harms arms control), (3) a degraded institutional activity (theater ratio shows strategic models are analytically simplified), and (4) temporally bounded (disarmament perspective shows winnability doctrine as collapsing under proliferation dynamics). The Tangled Rope classification (base extractiveness 0.58, suppression 0.68, requires_active_enforcement true, has both beneficiaries and victims) captures this precisely: it is neither pure extraction (Snare) nor pure coordination (Rope), but a hybrid where both functions are structurally present. The mandatrophy dissolves because the question 'Is winnability thinking coordination or extraction?' has no single answer — it is coordination for military planners (who benefit), extraction for arms control regimes (who are undermined), and theater for academic strategists (who maintain degraded models). The perspectival variability is diagnostic, not a flaw.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    winnability_operationality_vs_rhetoric,
    'Is winnability operationally possible (technical question) or rhetorically defensible (political question), and do these diverge?',
    'Examine strategic command planning documents, war gaming results, and damage-expectancy calculations; compare with public strategic doctrine statements and political rhetoric about nuclear deterrence stability.',
    'If operationally possible but rhetorically indefensible: the constraint is primarily rhetorical (Piton with degraded theater). If operationally questionable but rhetorically persistent: extraction is driven by institutional interests in maintaining winnability appearance (Snare disguised as Rope). If operationally and rhetorically aligned: Tangled Rope classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(winnability_operationality_vs_rhetoric, empirical, 'Divergence between operational winnability and rhetorical defensibility').

omega_variable(
    counterforce_targeting_sufficiency,
    'Can counterforce targeting eliminate an adversary''s retaliatory capacity before second strike, and at what confidence threshold does this transition from strategy to fiction?',
    'Analysis of ICBM silo hardness, submarine patrol patterns, command survivability protocols, alert rates, and launch-on-warning systems; comparison with attacker''s targeting precision and confidence in intelligence. Historical game-theory literature (Schelling, Jervis, Sagan).',
    'If operationally credible (>80% confidence in disarming strike): winnability remains in strategy space. If credible only under unrealistic assumptions (<50% confidence): winnability is theater masking the underlying logic of mutual assured destruction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterforce_targeting_sufficiency, empirical, 'Technical feasibility of disarming counterforce strike').

omega_variable(
    reading_forecast_collapse,
    'Will the countervailing_thinkable reading eventually foreclose the deterrence_unthinkable reading, or will both coexist indefinitely as different institutional commitments?',
    'Monitoring of strategic doctrine shifts, arms control treaty acceptance, proliferation dynamics, and strategic force modernization timelines. If winnability doctrine persists across weapons technology shifts (hypersonics, AI targeting), it indicates institutional lock-in favoring coexistence. If winnability doctrine degrades during arms reduction periods, it indicates reading pressure toward deterrence_unthinkable.',
    'If foreclosure occurs: the countervailing reading will have demonstrated logical dominance over deterrence_unthinkable within military-institutional frameworks. If coexistence persists: both readings remain live competing positions, and the strategic doctrine battlefield is the actual constraint (not winnability per se, but the contest over what winnability means).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_forecast_collapse, conceptual, 'Terminal attractor: will countervailing reading foreclose deterrence reading or persist in coexistence?').

omega_variable(
    arms_control_victim_status,
    'Are arms control regimes genuine victims of winnability doctrine, or are they minor institutional actors whose nominal status as ''victim'' obscures their structural irrelevance to the core military-strategic logic?',
    'Historical analysis of arms control treaty compliance and strategic force evolution post-treaty; examination of whether winnability-consistent modernization occurs before, during, or after treaty ratification; assessment of whether treaty constraints actually limit the weapons systems winnability doctrine requires.',
    'If genuine victims: winnability doctrine actively extracts from arms control legitimacy, and the Tangled Rope classification (mixed coordination and extraction) holds. If structurally irrelevant: arms control is decorative (Piton) and the core constraint is military-to-military coordination (Rope), with winnability doctrine purely benefiting strategic establishments.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(arms_control_victim_status, empirical, 'Whether arms control regimes are actual victims or institutional decoration').

omega_variable(
    reading_kernel_ambiguity,
    'Is the kernel ''war winnability post-1945'' referring to logical/mathematical winnability (can one compute a scenario where limited victory is possible?) or operational winnability (can one reliably execute such a victory?) or political winnability (can one credibly claim victory in public discourse)?',
    'Examination of foundational strategic theory texts (Wohlstetter, Schelling, Jervis, Betts); comparison of how winnability is framed in classified vs. unclassified doctrine; historical analysis of political claims about war outcomes vs. military assessments.',
    'If logical: winnability is nearly always possible (trivial mathematical result). If operational: winnability is contingent on force structure and adversary choices (empirical). If political: winnability is a rhetorical achievement independent of military reality (constructivist). Each reading carves the kernel differently.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_ambiguity, conceptual, 'Kernel ambiguity: logical vs. operational vs. political winnability').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_winnability_post_1945__countervailing_thinkable, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(winnability_tr_t0, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 0, 0.48).
narrative_ontology:measurement(winnability_tr_t10, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 10, 0.52).
narrative_ontology:measurement(winnability_tr_t20, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(winnability_be_t0, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(winnability_be_t10, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(winnability_be_t20, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(winnability_su_t0, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(winnability_su_t10, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(winnability_su_t20, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_winnability_post_1945__countervailing_thinkable, enforcement_mechanism).
narrative_ontology:affects_constraint(war_winnability_post_1945__countervailing_thinkable, deterrence_unthinkable).
narrative_ontology:affects_constraint(war_winnability_post_1945__countervailing_thinkable, rhetorical_contraction).
narrative_ontology:affects_constraint(war_winnability_post_1945__countervailing_thinkable, strategic_stability_dilemma).
narrative_ontology:affects_constraint(war_winnability_post_1945__countervailing_thinkable, arms_control_regime_legitimacy).

% DUAL FORMULATION NOTE:
% This is the countervailing_thinkable reading of the kernel war_winnability_post_1945. Sibling readings (deterrence_unthinkable and rhetorical_contraction) instantiate different structural constraints with different ε values and beneficiary/victim relationships. All three stories must be written separately per ε-invariance principle. The kernel is the contested claim itself; the readings are structural instantiations of different political commitments to what that kernel means. Links are: countervailing_thinkable affects deterrence_unthinkable (creates institutional pressure toward defensive posture logic) and affects rhetorical_contraction (winnability doctrine being operationally planned but rhetorically suppressed is a consequence of countervailing strategy dominance in military institutions while deterrence_unthinkable dominance in diplomatic institutions).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(war_winnability_post_1945__countervailing_thinkable, institutional, 0.08).
constraint_indexing:directionality_override(war_winnability_post_1945__countervailing_thinkable, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
