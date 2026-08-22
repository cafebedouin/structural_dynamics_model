% ============================================================================
% CONSTRAINT STORY: war_winnability_post_1945__rhetorical_contraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_winnability_post_1945__rhetorical_contraction, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: war_winnability_post_1945__rhetorical_contraction
 *   human_readable: Rhetorical Contraction of Nuclear Winnability with Persistent Operational Planning
 *   domain: strategic/international_relations/nuclear_deterrence
 *
 * SUMMARY:
 *   This constraint is the rhetorical_contraction reading of the
 *   war_winnability_post_1945 kernel. After 1945, nuclear war winnability
 *   became unsayable in public discourse (the nuclear taboo) while
 *   operational planning continued to treat limited victory as
 *   constrained-but-reachable. This dual-layer structure coordinates public
 *   expectations around unthinkability while extracting planning autonomy
 *   from democratic accountability. The claim/metric gap is deliberate: the
 *   constraint carries coordination claims (deterrence stability) while the
 *   authored metrics describe substantial extraction through enforced
 *   opacity.
 *
 * KEY AGENTS:
 *   - Strategic planners: Primary agenda-setter and beneficiary (institutional/arbitrage) â administer classification and operational planning.
 *   - Democratic public: Primary target (powerless/trapped) â bears costs without transparency.
 *   - Legislative oversight bodies: Secondary target (institutional/constrained) â formal authority denied by information asymmetry.
 *   - Nuclear weapons complex: Concentrated beneficiary (institutional/constrained) â captures budget and mission autonomy.
 *   - Academic deterrence theorists: Discursive enforcer and beneficiary (moderate/constrained) â legitimizes the taboo.
 *   - Whistleblowers and reformists: Excluded observers (powerless/trapped) â empirically document the gap but are silenced.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_winnability_post_1945__rhetorical_contraction, 0.72).
domain_priors:suppression_score(war_winnability_post_1945__rhetorical_contraction, 0.78).
domain_priors:theater_ratio(war_winnability_post_1945__rhetorical_contraction, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, extractiveness, 0.72).
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_winnability_post_1945__rhetorical_contraction, tangled_rope).
narrative_ontology:human_readable(war_winnability_post_1945__rhetorical_contraction, "Rhetorical Contraction of Nuclear Winnability with Persistent Operational Planning").
narrative_ontology:topic_domain(war_winnability_post_1945__rhetorical_contraction, "strategic/international_relations/nuclear_deterrence").

domain_priors:requires_active_enforcement(war_winnability_post_1945__rhetorical_contraction).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_winnability_post_1945__rhetorical_contraction, 'e4e27223-3229-47a7-a87c-45cf3908b05a').
narrative_ontology:cs_kernel_codification('e4e27223-3229-47a7-a87c-45cf3908b05a', formalized).
narrative_ontology:cs_authority_grounding('e4e27223-3229-47a7-a87c-45cf3908b05a', extraction).
narrative_ontology:cs_interpretation_layer_present('e4e27223-3229-47a7-a87c-45cf3908b05a').
narrative_ontology:cs_reading_relation('e4e27223-3229-47a7-a87c-45cf3908b05a', war_winnability_post_1945__deterrence_unthinkable, coexists_with).
narrative_ontology:cs_reading_relation('e4e27223-3229-47a7-a87c-45cf3908b05a', war_winnability_post_1945__countervailing_thinkable, influences).
narrative_ontology:cs_axiom('e4e27223-3229-47a7-a87c-45cf3908b05a', foundational, discourse_operational_split_is_structural).
narrative_ontology:cs_axiom_status(discourse_operational_split_is_structural, holdable).
narrative_ontology:cs_axiom_grounding('e4e27223-3229-47a7-a87c-45cf3908b05a', discourse_operational_split_is_structural, empirically_contingent).
narrative_ontology:cs_axiom('e4e27223-3229-47a7-a87c-45cf3908b05a', foundational, opacity_undermines_democratic_legitimacy).
narrative_ontology:cs_axiom_status(opacity_undermines_democratic_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('e4e27223-3229-47a7-a87c-45cf3908b05a', opacity_undermines_democratic_legitimacy, deontological).
narrative_ontology:cs_reference_frame('e4e27223-3229-47a7-a87c-45cf3908b05a', post_war_nuclear_revolution_fear).
narrative_ontology:cs_drift_state('e4e27223-3229-47a7-a87c-45cf3908b05a', post_counterforce_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e4e27223-3229-47a7-a87c-45cf3908b05a', '').
narrative_ontology:cs_kernel_id(war_winnability_post_1945__rhetorical_contraction, war_winnability_post_1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__rhetorical_contraction, strategic_planners).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__rhetorical_contraction, nuclear_weapons_complex).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__rhetorical_contraction, academic_deterrence_theorists).
narrative_ontology:constraint_victim(war_winnability_post_1945__rhetorical_contraction, democratic_public).
narrative_ontology:constraint_victim(war_winnability_post_1945__rhetorical_contraction, legislative_oversight_bodies).
narrative_ontology:constraint_vindicates(war_winnability_post_1945__rhetorical_contraction, nuclear_use_unthinkable_doctrine).
narrative_ontology:constraint_vindicates(war_winnability_post_1945__rhetorical_contraction, deterrence_stability_through_ambiguity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop classified operational plans for nuclear warfighting under strict compartmentalization. They enforce the rhetorical taboo in public discourse while pursuing constrained-but-reachable winnability in secure planning cells, preserving freedom of action without requiring public justification for targeting doctrines or escalation logic.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, strategic_planners, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(war_winnability_post_1945__rhetorical_contraction, strategic_planners, beneficiary).

% Comprises weapons laboratories, production facilities, and military services whose budgets and missions depend on the continued operational treatment of nuclear war as a planable event. Benefits from the discursive taboo because it shields modernization and warfighting concepts from democratic budgetary and moral scrutiny.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, nuclear_weapons_complex, beneficiary,
    institutional, generational, constrained, national).

% Produce and validate the intellectual architecture that renders nuclear winnability unsayable in open discourse. They receive research funding, clearances, and institutional legitimacy in exchange for adhering to discursive boundaries; professional advancement depends on reproducing the taboo.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, academic_deterrence_theorists, beneficiary,
    moderate, biographical, constrained, national).

% Bears the existential and fiscal costs of nuclear arsenals without access to operational plans that determine risk exposure. The rhetorical taboo prevents public demand for transparency, leaving citizens unable to assess whether planning aligns with stated deterrent doctrine or with warfighting aims that could implicate their survival.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, democratic_public, payer,
    powerless, civilizational, trapped, national).

% Possess formal statutory authority to oversee nuclear policy but are constrained by classification, compartmentalization, and the rhetorical taboo against questioning winnability. They receive selectively curated briefings that reinforce the unthinkability frame while operational plans and targeting revisions remain opaque.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, legislative_oversight_bodies, payer,
    institutional, generational, constrained, national).

% Former planners, critical scholars, and transparency advocates who argue that operational planning contradicts public deterrence doctrine. They are structurally excluded from policy influence through loss of clearances, professional ostracism, or prosecution, and their empirical claims are dismissed as destabilizing.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, whistleblowers_and_reformists, excluded,
    powerless, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(war_winnability_post_1945__rhetorical_contraction, strategic_planners).
narrative_ontology:fixing_cost_class(war_winnability_post_1945__rhetorical_contraction, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains strategic stability by rendering nuclear war winnability unspeakable in public discourse, preventing normalization of nuclear use and reducing public pressure for preemptive escalation or arms racing.
% TRANSFER_FUNCTION: Moves operational flexibility and planning autonomy from democratic oversight institutions to classified strategic planning cells, shielding targeting doctrine and war plans from public accountability while extracting tax revenue and existential risk tolerance from the public.
% ABSENT_VOICES: Whistleblowers, reformist strategists, and critical scholars who would testify that operational planning assumes winnable scenarios are excluded from cleared discourse and mainstream academic venues by classification and professional sanction.
% DISAPPEARANCE_RATIONALE: If the dual-layer arrangement vanishedâif winnability were openly discussable and operational planning were transparentâlegislative oversight would demand alignment between declaratory policy and operational plans, public nuclear debate would intensify, and the current distribution of planning autonomy would collapse.
% FOUNDING_PROBLEM: Managing the revolutionary destructive capacity of nuclear weapons after 1945 to prevent great-power nuclear war while preserving state security and deterrent credibility.
% FOUNDING_PROBLEM_CORROBORATION: Strategic planners and the weapons complex attest the problem remains live, citing peer adversaries. Democratic oversight bodies and critical scholars attest the founding problem has mutated: the constraint now serves to hide operational contradictions from the public that funds and is threatened by the arsenal. Independent historians and non-aligned strategists corroborate the mutation reading.
narrative_ontology:disappearance_verdict(war_winnability_post_1945__rhetorical_contraction, world_rearranges).
narrative_ontology:founding_problem_status(war_winnability_post_1945__rhetorical_contraction, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_winnability_post_1945__rhetorical_contraction, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(war_winnability_post_1945__rhetorical_contraction, 'none', 1).
narrative_ontology:epsilon_provenance(war_winnability_post_1945__rhetorical_contraction, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_winnability_post_1945__rhetorical_contraction_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_winnability_post_1945__rhetorical_contraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_winnability_post_1945__rhetorical_contraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.72) is high because the operational planning layer extracts planning autonomy and evades accountability from democratic oversight. Suppression (0.78) is high because the taboo is actively enforced by classification regimes, professional career incentives, and social sanction. Theater ratio (0.55) reflects the substantial and growing gap between public rhetoric (unthinkable) and operational reality (planned for). Accessibility collapse (0.68) is high because alternativesâtransparent planning, open debate on winnabilityâare collapsed by classification and normative pressure. Resistance (0.45) is moderate because excluded critics exist but are systematically marginalized.
 *
 * PERSPECTIVAL GAP:
 *   Strategic planners experience the constraint as necessary operational realism and legitimate secrecy; their seat computes toward coordination or low extraction. Democratic oversight and the public experience the same arrangement as an information asymmetry that extracts their capacity for informed consent; their seats compute toward high extraction. Academic beneficiaries sit between, trading legitimacy for resources.
 *
 * DIRECTIONALITY LOGIC:
 *   Strategic planners and the weapons complex are declared beneficiaries with arbitrage-grade or constrained exit and institutional power, placing them near the full-beneficiary end (low d, negative or damped Ï). The democratic public is a declared victim with trapped exit and powerless status, placing them near the full-target end (high d, amplified Ï). Legislative oversight is also a victim but with constrained rather than trapped exit and institutional power, so its d is somewhat lower than the publicâs despite the victim declaration. Academics are beneficiaries with constrained exit, yielding low d but not as low as the planners.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâpreventing nuclear war through the nuclear revolutionâhas not disappeared, but its justification has drifted. The arrangement persists because it serves the concentrated benefit of planners and the weapons complex (operational flexibility, budgetary autonomy), not solely because it solves the original coordination problem. Without mandatrophy analysis, the taboo could be misread as a pure coordination rope; the persistence of secret warfighting planning demonstrates that extraction is structurally coupled to the coordination function, mandating tangled_rope classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rhetorical_vs_operational_gap,
    'Does the persistence of operational winnability planning undermine the authenticity of the rhetorical taboo, or is the taboo a necessary stabilizing myth that enables rational planning?',
    'Comprehensive declassification and comparative analysis of planning documents against public deterrence doctrine; or natural experiment from a state that abandoned the taboo.',
    'If operational planning actively pursues winnability while the taboo suppresses debate, the constraint is extractive toward democratic oversight. If the taboo is causally necessary for safe planning, the coordination function is genuine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rhetorical_vs_operational_gap, conceptual, 'Whether the taboo is authentic coordination or performative cover.').

omega_variable(
    classification_as_enforcement,
    'To what extent does classification enforce the rhetorical taboo by hiding operational winnability planning, as opposed to protecting legitimate security secrets?',
    'Systematic review of classification guides and declassification timelines for nuclear strategy documents; analysis of whether winnability concepts are classified to prevent adversary knowledge or domestic political reaction.',
    'If classification primarily serves to prevent domestic accountability, suppression is higher and the constraint is more extractive. If it primarily protects operational details from adversaries, the extraction metric overstates the case.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(classification_as_enforcement, empirical, 'Security function of classification versus domestic enforcement.').

omega_variable(
    taboo_persistence_mechanism,
    'Is the rhetorical taboo maintained by genuine normative internalization or by institutional incentives and career risk?',
    'Career trajectory analysis of deterrence theorists and strategists who broke the taboo; survey of professional incentives in national security graduate programs and think tanks.',
    'If internalized, the constraint has identity-locked exit characteristics and higher effective suppression. If incentive-driven, the constraint is more vulnerable to shifts in funding or political leadership.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(taboo_persistence_mechanism, empirical, 'Internalized norm versus incentive-driven enforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_winnability_post_1945__rhetorical_contraction, 0, 79).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t0, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 0, 0.1).
narrative_ontology:measurement(war__tr_t15, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 15, 0.25).
narrative_ontology:measurement(war__tr_t30, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 30, 0.4).
narrative_ontology:measurement(war__tr_t45, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 45, 0.45).
narrative_ontology:measurement(war__tr_t60, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 60, 0.5).
narrative_ontology:measurement(war__tr_t75, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 75, 0.53).
narrative_ontology:measurement(war__tr_t79, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 79, 0.55).

% Extraction over time
narrative_ontology:measurement(war__be_t0, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(war__be_t15, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 15, 0.5).
narrative_ontology:measurement(war__be_t30, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 30, 0.6).
narrative_ontology:measurement(war__be_t45, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 45, 0.58).
narrative_ontology:measurement(war__be_t60, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 60, 0.65).
narrative_ontology:measurement(war__be_t75, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 75, 0.7).
narrative_ontology:measurement(war__be_t79, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 79, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t0, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(war__su_t15, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 15, 0.5).
narrative_ontology:measurement(war__su_t30, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 30, 0.65).
narrative_ontology:measurement(war__su_t45, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 45, 0.7).
narrative_ontology:measurement(war__su_t60, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 60, 0.75).
narrative_ontology:measurement(war__su_t75, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 75, 0.77).
narrative_ontology:measurement(war__su_t79, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 79, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_winnability_post_1945__rhetorical_contraction, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
