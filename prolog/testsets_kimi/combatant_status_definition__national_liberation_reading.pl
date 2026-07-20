% ============================================================================
% CONSTRAINT STORY: combatant_status_definition__national_liberation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_combatant_status_definition__national_liberation_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: combatant_status_definition__national_liberation_reading
 *   human_readable: AP I Article 1(4) National Liberation Combatant Status
 *   domain: international_law/political
 *
 * SUMMARY:
 *   This constraint instantiates the national_liberation_reading of the
 *   combatant_status_definition kernel under international humanitarian law.
 *   AP I Article 1(4) extends combatant status and POW protections to
 *   organized, command-controlled non-state armed groups fighting colonial,
 *   occupation, or racist regimes. The rule creates genuine coordination by
 *   incentivizing IHL compliance among insurgents, but simultaneously imposes
 *   asymmetric costs on occupying powers by obliging them to grant immunity
 *   to captured fighters. The claim/metric independence is maintained: the
 *   constraint is claimed as tangled_rope because it combines real
 *   coordination with asymmetric extraction, while the metrics independently
 *   reflect high extractiveness for occupying powers, substantial
 *   suppression, and rising theater as formal ratification outpaces actual
 *   POW grants.
 *
 * KEY AGENTS:
 *   - liberation_movements: Primary beneficiary (organized/constrained) â receive conditional combatant immunity
 *   - occupying_powers: Primary target/payer (institutional/constrained) â must grant POW status and forego domestic criminal prosecution
 *   - ap_i_state_parties: Agenda setter (institutional/constrained) â maintains treaty framework and interprets Article 1(4)
 *   - icrc: Analytical observer (institutional/analytical) â interprets and promotes IHL without direct benefit or cost
 *   - occupied_civilian_populations: Excluded voice (powerless/trapped) â affected by regulated warfare but absent from legal determinations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(combatant_status_definition__national_liberation_reading, 0.68).
domain_priors:suppression_score(combatant_status_definition__national_liberation_reading, 0.72).
domain_priors:theater_ratio(combatant_status_definition__national_liberation_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(combatant_status_definition__national_liberation_reading, tangled_rope).
narrative_ontology:human_readable(combatant_status_definition__national_liberation_reading, "AP I Article 1(4) National Liberation Combatant Status").
narrative_ontology:topic_domain(combatant_status_definition__national_liberation_reading, "international_law/political").

domain_priors:requires_active_enforcement(combatant_status_definition__national_liberation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(combatant_status_definition__national_liberation_reading, '97b59deb-d333-4e2e-83a9-cd798fa9382e').
narrative_ontology:cs_kernel_codification('97b59deb-d333-4e2e-83a9-cd798fa9382e', formalized).
narrative_ontology:cs_authority_grounding('97b59deb-d333-4e2e-83a9-cd798fa9382e', lineage).
narrative_ontology:cs_interpretation_layer_present('97b59deb-d333-4e2e-83a9-cd798fa9382e').
narrative_ontology:cs_reading_relation('97b59deb-d333-4e2e-83a9-cd798fa9382e', combatant_status_definition__state_centric_reading, forecloses).
narrative_ontology:cs_reading_relation('97b59deb-d333-4e2e-83a9-cd798fa9382e', combatant_status_definition__functional_protection_reading, coexists_with).
narrative_ontology:cs_axiom('97b59deb-d333-4e2e-83a9-cd798fa9382e', foundational, liberation_movement_combatant_parity).
narrative_ontology:cs_axiom_status(liberation_movement_combatant_parity, holdable).
narrative_ontology:cs_axiom_grounding('97b59deb-d333-4e2e-83a9-cd798fa9382e', liberation_movement_combatant_parity, conventional).
narrative_ontology:cs_axiom('97b59deb-d333-4e2e-83a9-cd798fa9382e', foundational, regime_character_triggers_status).
narrative_ontology:cs_axiom_status(regime_character_triggers_status, holdable).
narrative_ontology:cs_axiom_grounding('97b59deb-d333-4e2e-83a9-cd798fa9382e', regime_character_triggers_status, deontological).
narrative_ontology:cs_reference_frame('97b59deb-d333-4e2e-83a9-cd798fa9382e', anti_colonial_legal_equality).
narrative_ontology:cs_drift_state('97b59deb-d333-4e2e-83a9-cd798fa9382e', contemporary_state_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('97b59deb-d333-4e2e-83a9-cd798fa9382e', '').
narrative_ontology:cs_kernel_id(combatant_status_definition__national_liberation_reading, combatant_status_definition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(combatant_status_definition__national_liberation_reading, liberation_movements).
narrative_ontology:constraint_victim(combatant_status_definition__national_liberation_reading, occupying_powers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Non-state armed groups fighting colonial, occupation, or racist regimes. They gain conditional combatant status and POW protections under AP I Article 1(4) if they meet organization and command criteria. This incentivizes compliance with IHL norms such as distinction and humane treatment, but the benefit applies only where the opposing regime is internationally classified as colonial, occupation, or racist, and they remain materially weaker than the state forces they oppose.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, liberation_movements, beneficiary,
    organized, generational, constrained, global).

% States exercising occupation, colonial domination, or operating racist regimes against which liberation movements fight. They are obligated to treat captured insurgents who meet Article 1(4) criteria as lawful combatants entitled to POW status, rather than prosecuting them under domestic criminal law. This constrains their counter-insurgency legal framework and limits their ability to treat captured fighters as criminals.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, occupying_powers, payer,
    institutional, generational, constrained, global).

% States party to Additional Protocol I that collectively maintain the treaty framework, negotiate interpretations of Article 1(4), and exert diplomatic and legal pressure on occupying powers to comply. They administer the legal architecture that defines when liberation movements qualify for combatant status and police the boundary between lawful belligerency and criminal insurgency.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, ap_i_state_parties, agenda_setter,
    institutional, civilizational, constrained, global).

% Promotes and interprets international humanitarian law, including AP I Article 1(4). Documents compliance, engages with state and non-state actors, and publishes interpretive guidance on combatant status criteria without directly benefiting from or paying the constraint's costs.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, icrc, observer,
    institutional, civilizational, analytical, global).

% Civilian populations living under colonial or occupation rule who stand to benefit from regulated warfare and distinction but are not represented in the legal determinations of combatant status and have no direct voice in treaty interpretation, enforcement, or the classification of the regimes that control their territory.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, occupied_civilian_populations, excluded,
    powerless, biographical, trapped, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(combatant_status_definition__national_liberation_reading, liberation_movements).
narrative_ontology:fixing_cost_class(combatant_status_definition__national_liberation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the behavior of non-state armed groups in anti-colonial and occupation conflicts by offering combatant immunity and POW status in exchange for compliance with IHL criteria such as organization, command structure, and distinction, thereby reducing civilian harm and bringing asymmetric conflicts under legal regulation.
% TRANSFER_FUNCTION: Moves the legal obligation to treat captured insurgents as lawful combatants rather than criminals from occupying and colonial powers to the international legal accountability framework, with the practical cost borne by the detaining power and the legal benefit accruing to the insurgent group.
% ABSENT_VOICES: Civilian populations under occupation or colonial rule are structurally excluded from the legal conversations that determine when their liberators qualify for combatant status. States that reject AP I are formally outside the treaty framework but their opposition shapes the normative environment without being integrated into the Article 1(4) interpretive process.
% DISAPPEARANCE_RATIONALE: If AP I Article 1(4) vanished, liberation movements would lose a key legal incentive to organize under command structures and distinguish themselves from civilians; occupying powers would regain full domestic criminal jurisdiction over captured insurgents; the specific legal architecture regulating national liberation wars would collapse, likely increasing unregulated violence and reducing international oversight of asymmetric conflicts.
% FOUNDING_PROBLEM: Before 1977, anti-colonial and occupation fighters were treated as criminals under the domestic law of the controlling power, giving them no legal incentive to comply with humanitarian norms and exposing them to execution or imprisonment upon capture, while colonial powers faced no reciprocal legal obligations to treat them as lawful belligerents.
% FOUNDING_PROBLEM_CORROBORATION: Post-colonial states and the ICRC attest the problem was real and required the 1977 expansion. Occupying powers and non-party states argue the problem was addressed by existing Geneva Convention minima or that the liberation framing was politically motivated exceptionalism; independent legal historians corroborate the pre-1977 gap from outside the direct beneficiary coalition.
narrative_ontology:disappearance_verdict(combatant_status_definition__national_liberation_reading, world_rearranges).
narrative_ontology:founding_problem_status(combatant_status_definition__national_liberation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(combatant_status_definition__national_liberation_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(combatant_status_definition__national_liberation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(combatant_status_definition__national_liberation_reading, 0.68, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(combatant_status_definition__national_liberation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(combatant_status_definition__national_liberation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(combatant_status_definition__national_liberation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the constraint imposes a substantial legal obligation on occupying powers to treat non-state insurgents as lawful combatants, transferring legal and strategic advantage to liberation movements. Suppression (0.72) is high because the constraint's persistence depends on active diplomatic, judicial, and institutional enforcement against occupying powers that routinely resist granting such status. Theater ratio (0.55) is elevated: many AP I parties formally endorse Article 1(4) while operationally denying POW status to captured insurgents (e.g., through narrow conflict classification or evidentiary objections), making much of the enforcement activity performative. Resistance (0.75) reflects sustained opposition from non-party states and occupying powers. Accessibility collapse (0.50) is moderate because the state-centric reading remains a live alternative framework outside AP I.
 *
 * PERSPECTIVAL GAP:
 *   From the liberation movement seat, this constraint is legitimate legal equality that corrects colonial asymmetry and incentivizes humanitarian conduct. From the occupying power seat, it is coerced concession that legitimizes insurgents and constrains sovereign criminal jurisdiction. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Liberation movements are structural beneficiaries: they receive legal immunity and POW protections if they meet criteria, placing their directionality near the beneficiary pole. Occupying powers are structural targets: they bear the obligation to grant immunity and lose domestic criminal jurisdiction, placing their directionality near the full-target pole. AP I state parties are agenda setters who maintain the framework; they do not directly capture the extraction but administer it, sitting closer to symmetric than to either pole. The ICRC is an analytical observer with no extractive relationship to the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as tangled_rope prevents the mandatrophy error of labeling it pure rope (which would ignore the coercion exerted on occupying powers) or pure snare (which would ignore the genuine collective-action problem it solves: without combatant-status incentives, non-state actors in asymmetric conflicts have little reason to distinguish themselves from civilians or treat captives humanely). The coordination function is real but inseparable from the asymmetric transfer, and active enforcement is required to hold the arrangement against occupying power resistance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    regime_character_justiciability,
    'Is the determination that a regime is ''colonial,'' ''racist,'' or an ''occupation'' under Article 1(4) a justiciable legal standard or a political judgment masquerading as law?',
    'ICJ advisory opinions or consistent international jurisprudence establishing objective, reviewable criteria for regime classification under Article 1(4).',
    'If purely political, the constraint''s application is arbitrary and its effective extraction is unbounded; if justiciable, it operates as a rule-based coordination mechanism with predictable costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regime_character_justiciability, conceptual, 'Whether Article 1(4) regime classification is legal standard or political label').

omega_variable(
    enforcement_compliance_gap,
    'To what extent do AP I state parties actually comply with Article 1(4) in practice versus formally endorsing it while denying POW status to captured liberation fighters?',
    'Systematic empirical review of state practice in liberation conflicts, including treatment of detainees, domestic prosecution rates, and ICRC access records.',
    'Low compliance would indicate high theater ratio and a nominal rather than effective constraint; high compliance would validate the authored extraction measure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_compliance_gap, empirical, 'Gap between formal treaty adherence and actual POW grants').

omega_variable(
    state_centric_counter_norm_resilience,
    'Does the state-centric reading of combatant status structurally dominate actual state practice despite the national liberation reading''s treaty status, rendering this constraint operationally subordinate?',
    'Comparative analysis of domestic military manuals, judicial decisions in occupying powers, and official statements on combatant classification in asymmetric conflicts.',
    'If state-centric practice dominates, this reading functions more as a vindicated proposition than as an enforced constraint, and effective extraction is lower than the treaty text suggests.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_centric_counter_norm_resilience, empirical, 'Whether state-centric practice overrides the national liberation reading in operational reality').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(combatant_status_definition__national_liberation_reading, 0, 47).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(csd_nl_tr_t0, combatant_status_definition__national_liberation_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(csd_nl_tr_t8, combatant_status_definition__national_liberation_reading, theater_ratio, 8, 0.25).
narrative_ontology:measurement(csd_nl_tr_t15, combatant_status_definition__national_liberation_reading, theater_ratio, 15, 0.32).
narrative_ontology:measurement(csd_nl_tr_t23, combatant_status_definition__national_liberation_reading, theater_ratio, 23, 0.4).
narrative_ontology:measurement(csd_nl_tr_t31, combatant_status_definition__national_liberation_reading, theater_ratio, 31, 0.47).
narrative_ontology:measurement(csd_nl_tr_t39, combatant_status_definition__national_liberation_reading, theater_ratio, 39, 0.52).
narrative_ontology:measurement(csd_nl_tr_t47, combatant_status_definition__national_liberation_reading, theater_ratio, 47, 0.55).

% Extraction over time
narrative_ontology:measurement(csd_nl_be_t0, combatant_status_definition__national_liberation_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(csd_nl_be_t8, combatant_status_definition__national_liberation_reading, base_extractiveness, 8, 0.55).
narrative_ontology:measurement(csd_nl_be_t15, combatant_status_definition__national_liberation_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(csd_nl_be_t23, combatant_status_definition__national_liberation_reading, base_extractiveness, 23, 0.61).
narrative_ontology:measurement(csd_nl_be_t31, combatant_status_definition__national_liberation_reading, base_extractiveness, 31, 0.64).
narrative_ontology:measurement(csd_nl_be_t39, combatant_status_definition__national_liberation_reading, base_extractiveness, 39, 0.66).
narrative_ontology:measurement(csd_nl_be_t47, combatant_status_definition__national_liberation_reading, base_extractiveness, 47, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(csd_nl_su_t0, combatant_status_definition__national_liberation_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(csd_nl_su_t8, combatant_status_definition__national_liberation_reading, suppression_requirement, 8, 0.52).
narrative_ontology:measurement(csd_nl_su_t15, combatant_status_definition__national_liberation_reading, suppression_requirement, 15, 0.6).
narrative_ontology:measurement(csd_nl_su_t23, combatant_status_definition__national_liberation_reading, suppression_requirement, 23, 0.67).
narrative_ontology:measurement(csd_nl_su_t31, combatant_status_definition__national_liberation_reading, suppression_requirement, 31, 0.72).
narrative_ontology:measurement(csd_nl_su_t39, combatant_status_definition__national_liberation_reading, suppression_requirement, 39, 0.75).
narrative_ontology:measurement(csd_nl_su_t47, combatant_status_definition__national_liberation_reading, suppression_requirement, 47, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(combatant_status_definition__national_liberation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(combatant_status_definition__national_liberation_reading, combatant_status_definition__state_centric_reading).
narrative_ontology:affects_constraint(combatant_status_definition__national_liberation_reading, combatant_status_definition__functional_protection_reading).

% DUAL FORMULATION NOTE:
% This constraint is the national_liberation_reading of the combatant_status_definition kernel. It is structurally distinct from the state_centric_reading (which categorically excludes non-state actors from POW protections) and the functional_protection_reading (which renders status irrelevant to minimum protections). Its epsilon reflects asymmetric obligation on occupying powers to grant combatant immunity to insurgents.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
