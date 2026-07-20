% ============================================================================
% CONSTRAINT STORY: competence_exercise_requirement__catastrophe_as_necessary_anchor
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_exercise_requirement__catastrophe_as_necessary_anchor, []).

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
 *   constraint_id: competence_exercise_requirement__catastrophe_as_necessary_anchor
 *   human_readable: Catastrophe as Necessary Anchor for Competence Maintenance
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   This constraint story instantiates the reading
 *   'catastrophe_as_necessary_anchor' of the contested kernel
 *   'competence_exercise_requirement'. The reading claims that only real
 *   catastrophic events (or near-misses) provide the irreducible exercise
 *   that maintains competence in high-reliability organizations. It is
 *   presented as an empirical law of organizational learning but operates as
 *   a professional doctrine that coordinates learning around rare, dangerous
 *   events while extracting status and control for catastrophe survivors and
 *   the academic network that valorizes them. Sibling readings treat
 *   simulation as adequate (simulation_as_adequate_exercise) or as a
 *   necessary but insufficient hybrid (hybrid_dependency).
 *
 * KEY AGENTS:
 *   - veteran_practitioners: Primary beneficiary (organized/identity_locked) â catastrophe experience treated as irreplaceable capital.
 *   - hro_academic_network: Agenda-setter (institutional/mobile) â sets professional standards and curricula around crisis-derived expertise.
 *   - novice_operators: Primary target (powerless/constrained) â career advancement depends on uncontrolled, dangerous exposure.
 *   - simulation_technology_sector: Secondary target (moderate/constrained) â market suppressed by doctrinal ceiling on synthetic training.
 *   - safety_regulators: Enforcing agenda-setter (institutional/constrained) â codifies catastrophe-exposure requirements into licensing.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0.62).
domain_priors:suppression_score(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0.58).
domain_priors:theater_ratio(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, extractiveness, 0.62).
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(competence_exercise_requirement__catastrophe_as_necessary_anchor, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_requirement__catastrophe_as_necessary_anchor, tangled_rope).
narrative_ontology:human_readable(competence_exercise_requirement__catastrophe_as_necessary_anchor, "Catastrophe as Necessary Anchor for Competence Maintenance").
narrative_ontology:topic_domain(competence_exercise_requirement__catastrophe_as_necessary_anchor, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(competence_exercise_requirement__catastrophe_as_necessary_anchor).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_requirement__catastrophe_as_necessary_anchor, '52642624-3852-4f81-b6b5-7ecc921f0543').
narrative_ontology:cs_kernel_codification('52642624-3852-4f81-b6b5-7ecc921f0543', distributed).
narrative_ontology:cs_authority_grounding('52642624-3852-4f81-b6b5-7ecc921f0543', practice).
narrative_ontology:cs_interpretation_layer_present('52642624-3852-4f81-b6b5-7ecc921f0543').
narrative_ontology:cs_reading_relation('52642624-3852-4f81-b6b5-7ecc921f0543', competence_exercise_requirement__simulation_as_adequate_exercise, forecloses).
narrative_ontology:cs_reading_relation('52642624-3852-4f81-b6b5-7ecc921f0543', competence_exercise_requirement__hybrid_dependency, influences).
narrative_ontology:cs_axiom('52642624-3852-4f81-b6b5-7ecc921f0543', foundational, real_stress_irreplaceable_for_competence).
narrative_ontology:cs_axiom_status(real_stress_irreplaceable_for_competence, holdable).
narrative_ontology:cs_axiom_grounding('52642624-3852-4f81-b6b5-7ecc921f0543', real_stress_irreplaceable_for_competence, empirically_contingent).
narrative_ontology:cs_axiom('52642624-3852-4f81-b6b5-7ecc921f0543', foundational, simulation_produces_knowing_about_not_muscle_memory).
narrative_ontology:cs_axiom_status(simulation_produces_knowing_about_not_muscle_memory, holdable).
narrative_ontology:cs_axiom_grounding('52642624-3852-4f81-b6b5-7ecc921f0543', simulation_produces_knowing_about_not_muscle_memory, empirically_contingent).
narrative_ontology:cs_reference_frame('52642624-3852-4f81-b6b5-7ecc921f0543', catastrophe_anchored_expertise).
narrative_ontology:cs_drift_state('52642624-3852-4f81-b6b5-7ecc921f0543', contemporary_simulation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('52642624-3852-4f81-b6b5-7ecc921f0543', '').
narrative_ontology:cs_kernel_id(competence_exercise_requirement__catastrophe_as_necessary_anchor, competence_exercise_requirement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__catastrophe_as_necessary_anchor, veteran_practitioners).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__catastrophe_as_necessary_anchor, hro_academic_network).
narrative_ontology:constraint_victim(competence_exercise_requirement__catastrophe_as_necessary_anchor, novice_operators).
narrative_ontology:constraint_victim(competence_exercise_requirement__catastrophe_as_necessary_anchor, simulation_technology_sector).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Their professional status, authority, and career ceiling depend on having operated through real catastrophic events. This experience is treated as non-replicable by simulation, making them indispensable mentors and leaders. Professional identity is fused with catastrophe survival.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, veteran_practitioners, beneficiary,
    organized, biographical, identity_locked, global).

% Researches and publishes high-reliability organization theory, emphasizing mindfulness under fire and the irreplaceability of real crisis experience. Sets curricula, conference agendas, and citation networks that privilege catastrophe-derived case studies over simulation research.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, hro_academic_network, agenda_setter,
    institutional, generational, mobile, global).

% Must advance through career ladders where final competence certification requires catastrophe exposure they cannot control. Their skills are perpetually provisional until proven in fire, leaving them subordinate and exposed to danger.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, novice_operators, payer,
    powerless, biographical, constrained, national).

% Develops high-fidelity training systems and synthetic environments. Market legitimacy and adoption are persistently capped by the doctrinal ceiling that only real events provide irreducible exercise, forcing them into a supplementary vendor role.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, simulation_technology_sector, payer,
    moderate, biographical, constrained, global).

% Write training mandates and licensing requirements that incorporate live-event or real-crisis exposure as a prerequisite for full certification, treating simulation hours as secondary or preparatory credits rather than substitutes.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__catastrophe_as_necessary_anchor, safety_regulators, agenda_setter,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_exercise_requirement__catastrophe_as_necessary_anchor, veteran_practitioners).
narrative_ontology:fixing_cost_class(competence_exercise_requirement__catastrophe_as_necessary_anchor, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that competence in high-stakes socio-technical systems is validated against real-world complexity, stress, and uncertainty that are difficult to script artificially.
% TRANSFER_FUNCTION: Transfers authority, advancement, and legitimate expertise from operators without catastrophe exposure to those with it; transfers market legitimacy from simulation providers to experience-based credentialing institutions.
% ABSENT_VOICES: High-fidelity simulation researchers who have demonstrated physiological stress induction and skill transfer equivalent to live events; novice operators who would prefer safe, repeatable practice to dangerous on-the-job exposure; victims of the catastrophes treated as necessary organizational exercises.
% DISAPPEARANCE_RATIONALE: Training curricula would restructure around simulation and deliberate practice; promotion criteria would drop catastrophe-exposure requirements; the professional status of veterans would depreciate relative to simulation-certified operators; safety strategy would shift from reactive, event-dependent learning to proactive virtual rehearsal.
% FOUNDING_PROBLEM: Complex socio-technical systems fail in novel ways that exceed scripted procedures; operators must improvise under extreme stress, and early HRO research suggested that only live fire could produce this readiness.
% FOUNDING_PROBLEM_CORROBORATION: Accident investigators outside the benefiting parties attest that breakdowns during catastrophes often involve exactly the muscle-memory failures the doctrine warns of. However, human-factors researchers and simulation scientists outside the benefiting parties also attest that equivalent stress invariants can be reproduced synthetically, directly contesting the exclusivity claim. The founding problem is broadly corroborated; the catastrophe-only solution is self-asserted by the benefiting parties.
narrative_ontology:disappearance_verdict(competence_exercise_requirement__catastrophe_as_necessary_anchor, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_requirement__catastrophe_as_necessary_anchor, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_requirement__catastrophe_as_necessary_anchor, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(competence_exercise_requirement__catastrophe_as_necessary_anchor, 'none', 1).
narrative_ontology:epsilon_provenance(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_exercise_requirement__catastrophe_as_necessary_anchor_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_exercise_requirement__catastrophe_as_necessary_anchor, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_exercise_requirement__catastrophe_as_necessary_anchor_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is authored as moderately high because the constraint creates a persistent transfer of authority and economic opportunity from novices to veterans, and suppresses a viable alternative industry. Suppression (0.58) reflects active professional dismissal of simulation adequacy through credentialing, hiring, and regulatory standards. Theater ratio (0.30) is moderate-low: the underlying coordination function (real stress reveals gaps) is genuine, but an increasing share of the doctrine's performance is credentialist theater as simulation fidelity improves. Accessibility collapse (0.70) is high because once the doctrine is accepted, simulation alternatives become nearly unthinkable within the profession. Resistance (0.55) reflects growing challenge from simulation research and safety modernization advocates. The temporal series show a slow ratchet upward as the doctrine institutionalized and alternative technologies improved, requiring stronger enforcement to maintain exclusivity.
 *
 * PERSPECTIVAL GAP:
 *   The veteran practitioner seat experiences the constraint as a self-evident feature of high-stakes work: stress invariants cannot be faked, and their own survival proves the doctrine. The novice operator seat experiences the same structure as a barrier to advancement and an arbitrary risk exposure. The simulation technology seat experiences it as market suppression dressed in natural-law language. The engine computes these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (veteran_practitioners, hro_academic_network) sit near the full-beneficiary end: the constraint subsidizes their status and institutional role. Victims (novice_operators, simulation_technology_sector) sit near the full-target end: the constraint extracts career velocity and market legitimacy from them. Safety_regulators sit near symmetric: they enforce the constraint but are also bound by its professional consensus. No override is needed because the structural derivation captures the relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâpreparing operators for unscripted, high-stress failureâremains live, preventing a clean piton classification. However, the exclusivity claim ('only real catastrophes') may represent a mandatrophied expansion of a narrower truth ('real catastrophes reveal gaps simulation sometimes misses'). The R5 genealogy flags this: founding_problem_status is contested because the benefiting parties assert the problem is unchanged, while outsiders argue the solution set has expanded to include high-fidelity simulation. If the exclusivity claim is abandoned while the coordination function (stress-testing competence) is preserved via simulation, the constraint dissolves into the hybrid sibling reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    catastrophe_only_reading_contest,
    'Does this reading represent an irreducible feature of competence maintenance under stress, or a professional ideology that benefits catastrophe survivors and suppresses simulation alternatives?',
    'Comparative longitudinal performance studies and cost-benefit analysis of training regimes across industries that have shifted to simulation-heavy models, measuring operational outcomes under live stress.',
    'If simulation-trained operators perform equivalently, the constraint reclassifies toward snare or piton; if they underperform, the tangled-rope or mountain framing is supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(catastrophe_only_reading_contest, conceptual, 'Kernel reading contest: natural law vs constructed ideology').

omega_variable(
    simulation_fidelity_boundary,
    'Does high-fidelity simulation with physiological stress induction actually reproduce the cognitive load of catastrophe, or is there an irreducible gap that only live jeopardy can close?',
    'Controlled studies measuring cortisol response, decision latency, and error rates in synthetic versus live high-stakes environments across nuclear, aviation, and emergency medicine domains.',
    'If the gap is closed empirically, the constraint''s accessibility_collapse and suppression metrics were overstated by ideology; if the gap persists, the coordination function is stronger than the extraction framing suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_fidelity_boundary, empirical, 'Whether simulation can replicate catastrophe stress invariants').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (credentialing rules, licensing requirements, promotion criteria) or internalized (professional identity fusion among veterans who dismiss simulation as ''not real'')?',
    'Post-reform trajectory analysis: if suppression collapses quickly after credentialing rules change, it was structural; if dismissal of simulation persists despite rule changes, it was internalized.',
    'If internalized, effective suppression is higher than the structural measure suggests and the constraint is more deeply anchored than institutional reform alone can address.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_requirement__catastrophe_as_necessary_anchor, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cer_catastrophe_tr_t0, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 0, 0.18).
narrative_ontology:measurement(cer_catastrophe_tr_t8, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 8, 0.2).
narrative_ontology:measurement(cer_catastrophe_tr_t16, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 16, 0.24).
narrative_ontology:measurement(cer_catastrophe_tr_t24, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 24, 0.27).
narrative_ontology:measurement(cer_catastrophe_tr_t32, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 32, 0.29).
narrative_ontology:measurement(cer_catastrophe_tr_t40, competence_exercise_requirement__catastrophe_as_necessary_anchor, theater_ratio, 40, 0.3).

% Extraction over time
narrative_ontology:measurement(cer_catastrophe_be_t0, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(cer_catastrophe_be_t8, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(cer_catastrophe_be_t16, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 16, 0.56).
narrative_ontology:measurement(cer_catastrophe_be_t24, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 24, 0.59).
narrative_ontology:measurement(cer_catastrophe_be_t32, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 32, 0.61).
narrative_ontology:measurement(cer_catastrophe_be_t40, competence_exercise_requirement__catastrophe_as_necessary_anchor, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(cer_catastrophe_su_t0, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(cer_catastrophe_su_t8, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 8, 0.46).
narrative_ontology:measurement(cer_catastrophe_su_t16, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 16, 0.5).
narrative_ontology:measurement(cer_catastrophe_su_t24, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 24, 0.54).
narrative_ontology:measurement(cer_catastrophe_su_t32, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 32, 0.56).
narrative_ontology:measurement(cer_catastrophe_su_t40, competence_exercise_requirement__catastrophe_as_necessary_anchor, suppression_requirement, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_requirement__catastrophe_as_necessary_anchor, identity_coordination).
narrative_ontology:affects_constraint(competence_exercise_requirement__catastrophe_as_necessary_anchor, competence_exercise_requirement__simulation_as_adequate_exercise).
narrative_ontology:affects_constraint(competence_exercise_requirement__catastrophe_as_necessary_anchor, competence_exercise_requirement__hybrid_dependency).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the competence_exercise_requirement kernel. The label 'competence maintenance' conflates three structurally distinct claims: catastrophe-only, simulation-only, and hybrid. Each has different epsilon values, victim sets, and coordination functions. They are modeled as separate stories linked by affects_constraints, not as one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
