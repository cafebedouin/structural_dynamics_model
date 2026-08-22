% ============================================================================
% CONSTRAINT STORY: war_winnability_post_1945__countervailing_thinkable
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: war_winnability_post_1945__countervailing_thinkable
 *   human_readable: Counterforce Nuclear Victory Doctrine (Countervailable Reading)
 *   domain: strategic/international_relations
 *
 * SUMMARY:
 *   This constraint instantiates the 'countervailing_thinkable' reading of
 *   the contested kernel 'war_winnability_post_1945': the doctrinal assertion
 *   that nuclear weapons constrain but do not eliminate the achievability of
 *   limited victory through counterforce targeting. The constraint is the
 *   standing strategic arrangement under which nuclear war planning continues
 *   as a coherent activity, benefiting the military-industrial complex
 *   through mission continuity while undermining arms control regimes and
 *   imposing existential risk on civilian populations. It is claimed as
 *   Tangled Rope because it carries a genuine coordination function
 *   (strategic planning coherence) alongside asymmetric extraction.
 *
 * KEY AGENTS:
 *   - Nuclear strategic command (agenda_setter): institutional power, constrained exit â sets targeting doctrine and force posture.
 *   - Military-industrial complex (beneficiary): powerful, mobile exit â collects funding and mission continuity from nuclear modernization.
 *   - Arms control treaty regimes (payer): institutional power, constrained exit â bear the erosion of their frameworks by warfighting doctrine.
 *   - Civilian populations (payer): powerless, trapped â bear existential risk with no exit.
 *   - Disarmament advocates (excluded): moderate power, constrained â marginalized from strategic discourse.
 *   - Security studies analysts (observer): analytical seat â external assessors of doctrinal consistency.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_winnability_post_1945__countervailing_thinkable, 0.72).
domain_priors:suppression_score(war_winnability_post_1945__countervailing_thinkable, 0.75).
domain_priors:theater_ratio(war_winnability_post_1945__countervailing_thinkable, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, extractiveness, 0.72).
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_winnability_post_1945__countervailing_thinkable, tangled_rope).
narrative_ontology:human_readable(war_winnability_post_1945__countervailing_thinkable, "Counterforce Nuclear Victory Doctrine (Countervailable Reading)").
narrative_ontology:topic_domain(war_winnability_post_1945__countervailing_thinkable, "strategic/international_relations").

domain_priors:requires_active_enforcement(war_winnability_post_1945__countervailing_thinkable).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_winnability_post_1945__countervailing_thinkable, '55371b22-35a4-4f66-8781-1cc568e0b6dd').
narrative_ontology:cs_kernel_codification('55371b22-35a4-4f66-8781-1cc568e0b6dd', formalized).
narrative_ontology:cs_authority_grounding('55371b22-35a4-4f66-8781-1cc568e0b6dd', extraction).
narrative_ontology:cs_interpretation_layer_present('55371b22-35a4-4f66-8781-1cc568e0b6dd').
narrative_ontology:cs_reading_relation('55371b22-35a4-4f66-8781-1cc568e0b6dd', war_winnability_post_1945__deterrence_unthinkable, coexists_with).
narrative_ontology:cs_reading_relation('55371b22-35a4-4f66-8781-1cc568e0b6dd', war_winnability_post_1945__rhetorical_contraction, influences).
narrative_ontology:cs_axiom('55371b22-35a4-4f66-8781-1cc568e0b6dd', foundational, limited_nuclear_victory_achievable).
narrative_ontology:cs_axiom_status(limited_nuclear_victory_achievable, holdable).
narrative_ontology:cs_axiom_grounding('55371b22-35a4-4f66-8781-1cc568e0b6dd', limited_nuclear_victory_achievable, empirically_contingent).
narrative_ontology:cs_axiom('55371b22-35a4-4f66-8781-1cc568e0b6dd', foundational, counterforce_planning_strategic_necessity).
narrative_ontology:cs_axiom_status(counterforce_planning_strategic_necessity, holdable).
narrative_ontology:cs_axiom_grounding('55371b22-35a4-4f66-8781-1cc568e0b6dd', counterforce_planning_strategic_necessity, instrumental).
narrative_ontology:cs_reference_frame('55371b22-35a4-4f66-8781-1cc568e0b6dd', nuclear_warfighting_posture).
narrative_ontology:cs_drift_state('55371b22-35a4-4f66-8781-1cc568e0b6dd', post_cold_war_deterrence_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('55371b22-35a4-4f66-8781-1cc568e0b6dd', '').
narrative_ontology:cs_kernel_id(war_winnability_post_1945__countervailing_thinkable, war_winnability_post_1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__countervailing_thinkable, military_industrial_complex).
narrative_ontology:constraint_victim(war_winnability_post_1945__countervailing_thinkable, arms_control_treaty_regimes).
narrative_ontology:constraint_victim(war_winnability_post_1945__countervailing_thinkable, civilian_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets nuclear targeting doctrine, force posture, and strategic planning assumptions. Their professional identity and institutional continuity depend on the continuing validity of nuclear war planning and the achievability of limited victory through counterforce.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, nuclear_strategic_command, agenda_setter,
    institutional, generational, constrained, global).

% Receives sustained funding for nuclear modernization, delivery systems, and command infrastructure. Mission continuity and revenue streams depend on the doctrine that nuclear forces are operationally usable beyond pure deterrence.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, military_industrial_complex, beneficiary,
    powerful, generational, mobile, global).

% Institutional frameworks for limitation and verification are systematically undermined when counterforce planning legitimizes nuclear use and arms racing. Their political support erodes as warfighting doctrines dominate strategic discourse.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, arms_control_treaty_regimes, payer,
    institutional, generational, constrained, global).

% Bear the existential risk of maintained high-alert nuclear arsenals and counterforce doctrines that lower the perceived threshold for nuclear use. No exit from the risk environment created by strategic postures.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, civilian_populations, payer,
    powerless, civilizational, trapped, global).

% Argue for the categorical unthinkability of nuclear victory and advocate for disarmament. Structurally excluded from defense planning circles, classified deliberations, and funding streams that set strategic posture.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, disarmament_advocates, excluded,
    moderate, generational, constrained, global).

% Analyze the doctrinal contest between warfighting and deterrence purists without institutional stake in maintaining any particular nuclear posture. Provide external assessment of strategic consistency and risk.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, security_studies_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(war_winnability_post_1945__countervailing_thinkable, military_industrial_complex).
narrative_ontology:fixing_cost_class(war_winnability_post_1945__countervailing_thinkable, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent planning framework for military institutions in the nuclear age, preventing strategic paralysis by defining how force can still be used rationally despite civilization-threatening weapons.
% TRANSFER_FUNCTION: Moves funding, institutional legitimacy, and mission priority from arms control and conventional postures to counterforce infrastructure and nuclear modernization, while transferring existential risk to civilian populations.
% ABSENT_VOICES: Disarmament advocates and categorical deterrence theorists are structurally excluded from defense planning and strategic discourse; they would argue that nuclear war is unwinnable and planning for victory is incoherent.
% DISAPPEARANCE_RATIONALE: If the doctrine of limited nuclear winnability vanished, strategic planning would shift toward pure deterrence or disarmament, the military-industrial complex would lose its central nuclear mission, arms control institutions would regain legitimacy, and the global security architecture would reorganize around stability rather than warfighting.
% FOUNDING_PROBLEM: How to maintain meaningful military strategy and great-power conflict management after the advent of weapons that threaten civilizational destruction, without surrendering the possibility of strategic advantage.
% FOUNDING_PROBLEM_CORROBORATION: Military historians attest the problem was genuine in 1945-1960. Arms control scholars and independent security analysts attest the problem is now managed by deterrence stability; the continued pursuit of winnability is a self-sustaining bureaucratic program rather than a response to a live operational need.
narrative_ontology:disappearance_verdict(war_winnability_post_1945__countervailing_thinkable, world_rearranges).
narrative_ontology:founding_problem_status(war_winnability_post_1945__countervailing_thinkable, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_winnability_post_1945__countervailing_thinkable, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(war_winnability_post_1945__countervailing_thinkable, 'none', 1).
narrative_ontology:epsilon_provenance(war_winnability_post_1945__countervailing_thinkable, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_winnability_post_1945__countervailing_thinkable_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_winnability_post_1945__countervailing_thinkable, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_winnability_post_1945__countervailing_thinkable_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the doctrine decouples strategic planning from the empirical reality of nuclear escalation, sustaining institutional rents. Suppression (0.75) is high because the constraint's persistence depends on actively excluding disarmament voices and classifying or marginalizing alternatives. Theater ratio (0.45) reflects moderate performative maintenance: genuine strategic analysis exists, but an increasing share of activity justifies budgets and postures rather than solving operational problems. Accessibility collapse (0.70) indicates that alternatives (minimum deterrence, disarmament) are institutionally visible but politically inaccessible. Resistance (0.60) captures sustained opposition from arms control communities and anti-nuclear movements. Temporal measurements show extraction rising from early Cold War institutionalization, peaking during the Second Cold War and arms race periods, with a post-Cold War dip followed by renewed intensification.
 *
 * PERSPECTIVAL GAP:
 *   From the nuclear command seat, the constraint is genuine coordination â without war planning, deterrence lacks credibility and strategy collapses into incoherence. From the arms control and civilian seats, the same structure is extractive theater that sustains an arms race. The engine computes this divergence from the structural data rather than adjudicating it.
 *
 * DIRECTIONALITY LOGIC:
 *   The nuclear strategic command and military-industrial complex sit near the beneficiary end: they collect institutional legitimacy, funding, and mission continuity from the doctrine. Arms control regimes and civilian populations sit near the target end: they bear the costs of undermined treaties and existential risk. Disarmament advocates are excluded from the conversation entirely. The divergence is structural: the same doctrine that coordinates force planning for the military community extracts from global stability.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â maintaining strategic coherence after 1945 â was genuine. However, the doctrine's persistence beyond the Cold War, despite empirical challenges to counterforce viability and the success of deterrence-based stability, suggests partial mandatrophy. The Tangled Rope classification prevents mislabeling this as pure coordination (Rope) by naming the victims, and prevents mislabeling it as pure extraction (Snare) by acknowledging the real coordination problem of strategic paralysis that the doctrine solves. The metrics are authored independently: high extraction and suppression with moderate theater capture the atrophied coordination function still performing genuine work.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_sibling_divergence,
    'How would classification change if the deterrence_unthinkable or rhetorical_contraction reading of the same kernel were adopted instead?',
    'Cross-reading decomposition: instantiate each reading as a separate constraint story and compare structural data. The deterrence_unthinkable reading would likely classify as Mountain or Rope (a structural feature of nuclear reality), while rhetorical_contraction would classify as Piton or Snare (discursive gap masking operational reality).',
    'The countervailing_thinkable reading''s classification as Tangled Rope depends on treating winnability as a constructed doctrine serving identifiable beneficiaries; alternative readings shift the constraint identity, beneficiary structure, and epsilon referent entirely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_sibling_divergence, conceptual, 'Structural divergence between sibling readings of the war winnability kernel').

omega_variable(
    counterforce_empirical_validity,
    'Is limited nuclear victory through counterforce targeting empirically achievable, or is it a doctrinal fiction maintained by institutional interests?',
    'War games, simulations, and historical crisis analysis; but full empirical test is impossible without actual nuclear war. Partial resolution through modeling of escalation dynamics and command-and-control fragility.',
    'If empirically unachievable, the coordination function is largely theater and the constraint approaches Snare; if genuinely achievable, the extraction may be the necessary price of strategic coherence under extreme uncertainty.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterforce_empirical_validity, empirical, 'Whether counterforce victory is operationally real or institutional myth').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of disarmament and unthinkability voices structural (funding exclusion, classification barriers, career penalties) or internalized (strategic community''s self-conception that nuclear war planning is simply rational)?',
    'Post-exit trajectory analysis: observe whether disarmament advocates who leave institutional roles continue to be suppressed, and whether strategic culture shifts when institutional incentives change.',
    'If suppression is primarily internalized, the constraint''s effective suppression exceeds structural measures and the constraint is more deeply embedded than surface institutions suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, conceptual, 'Structural versus internalized suppression of alternative nuclear framings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_winnability_post_1945__countervailing_thinkable, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t0, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 0, 0.1).
narrative_ontology:measurement(war__tr_t15, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 15, 0.2).
narrative_ontology:measurement(war__tr_t30, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 30, 0.35).
narrative_ontology:measurement(war__tr_t45, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 45, 0.5).
narrative_ontology:measurement(war__tr_t60, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 60, 0.42).
narrative_ontology:measurement(war__tr_t80, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 80, 0.45).

% Extraction over time
narrative_ontology:measurement(war__be_t0, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(war__be_t15, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(war__be_t30, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(war__be_t45, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 45, 0.7).
narrative_ontology:measurement(war__be_t60, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 60, 0.65).
narrative_ontology:measurement(war__be_t80, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 80, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t0, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(war__su_t15, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 15, 0.45).
narrative_ontology:measurement(war__su_t30, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 30, 0.6).
narrative_ontology:measurement(war__su_t45, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 45, 0.7).
narrative_ontology:measurement(war__su_t60, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 60, 0.58).
narrative_ontology:measurement(war__su_t80, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 80, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_winnability_post_1945__countervailing_thinkable, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
