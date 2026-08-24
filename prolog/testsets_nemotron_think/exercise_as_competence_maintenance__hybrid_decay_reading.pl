% ============================================================================
% CONSTRAINT STORY: exercise_as_competence_maintenance__hybrid_decay_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_exercise_as_competence_maintenance__hybrid_decay_reading, []).

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
 *   constraint_id: exercise_as_competence_maintenance__hybrid_decay_reading
 *   human_readable: Simulation-Based Competence Maintenance (Hybrid Decay Reading)
 *   domain: safety_engineering/organizational_learning/crisis_preparedness
 *
 * SUMMARY:
 *   High-hazard industries mandate simulation-based training to maintain
 *   operator competence. The hybrid decay reading asserts this constraint has
 *   two structurally distinct components: procedural competence (checklists,
 *   muscle memory, standard responses) which IS genuinely exercised by
 *   high-fidelity simulation, and judgment-under-stakes (improvisation, moral
 *   reasoning under pressure, novel situation synthesis) which is NOT
 *   exercised by simulation because the stakes, irreversibility, and
 *   psychological weight are absent. The constraint extracts by creating
 *   false confidence that 'competence is maintained' when only half the
 *   kernel is exercised. Victims are those harmed when operators face real
 *   crises requiring the unexercised component.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(exercise_as_competence_maintenance__hybrid_decay_reading, 0.68).
domain_priors:suppression_score(exercise_as_competence_maintenance__hybrid_decay_reading, 0.55).
domain_priors:theater_ratio(exercise_as_competence_maintenance__hybrid_decay_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(exercise_as_competence_maintenance__hybrid_decay_reading, tangled_rope).
narrative_ontology:human_readable(exercise_as_competence_maintenance__hybrid_decay_reading, "Simulation-Based Competence Maintenance (Hybrid Decay Reading)").
narrative_ontology:topic_domain(exercise_as_competence_maintenance__hybrid_decay_reading, "safety_engineering/organizational_learning/crisis_preparedness").

domain_priors:requires_active_enforcement(exercise_as_competence_maintenance__hybrid_decay_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(exercise_as_competence_maintenance__hybrid_decay_reading, 'd8adcc7b-482e-4a99-9f73-c38575b40269').
narrative_ontology:cs_kernel_codification('d8adcc7b-482e-4a99-9f73-c38575b40269', formalized).
narrative_ontology:cs_authority_grounding('d8adcc7b-482e-4a99-9f73-c38575b40269', lineage).
narrative_ontology:cs_interpretation_layer_present('d8adcc7b-482e-4a99-9f73-c38575b40269').
narrative_ontology:cs_reading_relation('d8adcc7b-482e-4a99-9f73-c38575b40269', exercise_as_competence_maintenance__simulation_sufficiency_reading, forecloses).
narrative_ontology:cs_reading_relation('d8adcc7b-482e-4a99-9f73-c38575b40269', exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, coexists_with).
narrative_ontology:cs_axiom('d8adcc7b-482e-4a99-9f73-c38575b40269', foundational, competence_kernel_is_dual_component).
narrative_ontology:cs_axiom_status(competence_kernel_is_dual_component, holdable).
narrative_ontology:cs_axiom_grounding('d8adcc7b-482e-4a99-9f73-c38575b40269', competence_kernel_is_dual_component, empirically_contingent).
narrative_ontology:cs_axiom('d8adcc7b-482e-4a99-9f73-c38575b40269', foundational, simulation_exercises_procedural_only).
narrative_ontology:cs_axiom_status(simulation_exercises_procedural_only, holdable).
narrative_ontology:cs_axiom_grounding('d8adcc7b-482e-4a99-9f73-c38575b40269', simulation_exercises_procedural_only, empirically_contingent).
narrative_ontology:cs_axiom('d8adcc7b-482e-4a99-9f73-c38575b40269', secondary, judgment_requires_stakes).
narrative_ontology:cs_axiom_status(judgment_requires_stakes, holdable).
narrative_ontology:cs_axiom_grounding('d8adcc7b-482e-4a99-9f73-c38575b40269', judgment_requires_stakes, deontological).
narrative_ontology:cs_reference_frame('d8adcc7b-482e-4a99-9f73-c38575b40269', post_ww2_procedural_verification_mandate).
narrative_ontology:cs_drift_state('d8adcc7b-482e-4a99-9f73-c38575b40269', contemporary_judgment_gap_recognition, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d8adcc7b-482e-4a99-9f73-c38575b40269', '').
narrative_ontology:cs_kernel_id(exercise_as_competence_maintenance__hybrid_decay_reading, exercise_as_competence_maintenance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__hybrid_decay_reading, organizations_mandating_simulation).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__hybrid_decay_reading, training_vendors).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__hybrid_decay_reading, accreditation_bodies).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__hybrid_decay_reading, frontline_operators).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__hybrid_decay_reading, affected_public).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__hybrid_decay_reading, organizations_facing_judgment_failures).
narrative_ontology:constraint_vindicates(exercise_as_competence_maintenance__hybrid_decay_reading, procedural_standardization_reduces_variance).
narrative_ontology:constraint_vindicates(exercise_as_competence_maintenance__hybrid_decay_reading, muscle_memory_retention_via_repetition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Mandate simulation-based training for compliance and liability management. They set the curriculum, frequency, and fidelity standards. They benefit from predictable costs, measurable compliance metrics, and reduced insurance premiums. They can switch vendors or modify requirements but face institutional inertia and regulatory lock-in.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, organizations_mandating_simulation, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(exercise_as_competence_maintenance__hybrid_decay_reading, organizations_mandating_simulation, beneficiary).

% Design and deliver simulation platforms and scenarios. They profit from recurring contracts for simulator time, scenario development, and certification. Their products emphasize procedural fidelity and measurable outputs. They can pivot to other markets but have invested heavily in simulation-specific IP and infrastructure.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, training_vendors, beneficiary,
    organized, biographical, mobile, global).

% Set standards that define what counts as 'competence maintenance' for licensing and certification. They legitimize simulation hours as equivalent to experience. They collect fees and institutional authority. Changing standards requires multi-stakeholder consensus and risks their legitimacy.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, accreditation_bodies, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(exercise_as_competence_maintenance__hybrid_decay_reading, accreditation_bodies, agenda_setter).

% Undergo mandated simulation training. They gain procedural fluency but recognize the gap in judgment-under-stakes. They bear the cost of false confidence when real crises demand improvisation. Their exit options are limited by licensure requirements, career specialization, and organizational mandates.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, frontline_operators, payer,
    moderate, biographical, constrained, global).

% Patients, passengers, communities near industrial sites — anyone whose safety depends on operator judgment in novel crises. They have no voice in training design, no exit from the systems that protect them, and bear the full consequences when simulation-trained operators fail at improvisation.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, affected_public, payer,
    powerless, immediate, trapped, global).

% Organizations that suffer catastrophic failures traced to judgment gaps not covered by simulation. They pay in liability, reputation, and regulatory sanctions. They could invest in live exercises but face cost, risk, and cultural resistance. They are both agenda-setters (could change training) and payers (bear failure costs).
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, organizations_facing_judgment_failures, payer,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(exercise_as_competence_maintenance__hybrid_decay_reading, organizations_facing_judgment_failures, agenda_setter).

% Study transfer of training from simulation to real performance. They document the procedural/judgment gap, measure decay curves, and propose alternative training architectures. They neither collect rents nor bear operational costs.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, safety_researchers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardizes baseline procedural competence across distributed operators and organizations: ensures everyone can execute checklists, operate equipment, and follow protocols under known failure modes. Solves the variance problem in high-reliability systems.
% TRANSFER_FUNCTION: Moves resources (budget, time, career capital) from frontline operators and organizations toward training vendors and accreditation bodies, in exchange for procedural compliance certificates. Moves risk from organizations onto the public when judgment failures occur.
% ABSENT_VOICES: The affected public is structurally excluded — they cannot participate in training design or accreditation. Frontline operators who have experienced judgment failures in real crises are often excluded from curriculum committees by organizational hierarchy. Lived-catastrophe veterans who advocate for live exercises are marginalized as 'anecdotal' or 'high-risk'.
% DISAPPEARANCE_RATIONALE: If simulation mandates vanished overnight, organizations would initially face compliance gaps and liability exposure. But within 2-3 years, a mixed economy would emerge: some would invest in live exercises and mentored judgment training; others would revert to apprenticeship models; vendors would pivot to hybrid platforms. The procedural baseline would degrade initially but judgment capacity would likely improve where live substitutes emerge.
% FOUNDING_PROBLEM: Post-WWII expansion of high-hazard industries (nuclear, aviation, chemical) created a need to verify operator competence at scale without exposing trainees or public to real catastrophic risk. Simulation promised measurable, repeatable, safe verification.
% FOUNDING_PROBLEM_CORROBORATION: Aviation safety historians (e.g., NASA ASRS analyses, ICAO training reports) corroborate the founding problem was real and simulation solved the procedural-verification gap. Frontline operator associations (ALPA, IFATCA) and cognitive systems engineering researchers (Woods, Hollnagel, Dekker) attest the problem has shifted: procedural competence is now saturated, and the live gap is judgment under novelty — a problem simulation was not designed to solve and does not solve.
narrative_ontology:disappearance_verdict(exercise_as_competence_maintenance__hybrid_decay_reading, world_rearranges).
narrative_ontology:founding_problem_status(exercise_as_competence_maintenance__hybrid_decay_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(exercise_as_competence_maintenance__hybrid_decay_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(exercise_as_competence_maintenance__hybrid_decay_reading, 'none', 1).
narrative_ontology:epsilon_provenance(exercise_as_competence_maintenance__hybrid_decay_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(exercise_as_competence_maintenance__hybrid_decay_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(exercise_as_competence_maintenance__hybrid_decay_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(exercise_as_competence_maintenance__hybrid_decay_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects the widening gap between what simulation delivers (procedures) and what the mandate claims it delivers (full competence). The mandate creates a compliance floor that crowds out live exercises and mentored judgment development. Suppression (0.55) is moderate: live exercises exist but are expensive, risky, and not credited by accreditation. Theater ratio (0.45) is rising as simulation hours increase while judgment gaps persist — more hours yield diminishing returns on the procedural component while the judgment component remains unaddressed. Accessibility collapse (0.52) reflects that alternatives exist (live exercises, apprenticeship, red-teaming) but are structurally disadvantaged by cost and accreditation rules. Resistance (0.48) comes from operator groups and cognitive engineers but is fragmented across domains.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (organizations, accreditation), the constraint appears as genuine coordination: it solves the procedural verification problem at scale. From the payer seats (operators, public), the same structure operates as extraction: it certifies a competence that doesn't exist in the judgment domain, creating a liability shield for organizations and a risk transfer to the public. The engine computes this divergence from the declared structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Organizations mandating simulation and training vendors are structural beneficiaries (d ~ 0.15-0.25): they collect compliance revenue, predictable costs, and liability shields. Accreditation bodies are dual-positioned (agenda_setter/beneficiary, d ~ 0.2). Frontline operators are payers with constrained exit (d ~ 0.7): they invest time in training that partially doesn't transfer, and bear psychological burden of false confidence. Affected public are trapped payers (d ~ 0.95): no voice, no exit, full consequence. Organizations facing judgment failures are powerful payers with constrained exit (d ~ 0.6): they could change the system but face institutional inertia.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (procedural verification at scale without catastrophic risk) was live and solved. The mandate persists because the accreditation infrastructure, vendor ecosystem, and compliance metrics have become self-justifying. The mandate has not been updated to address the judgment gap because doing so would require admitting the original solution is incomplete — which threatens the legitimacy of the accreditation bodies and the business model of vendors. This is a classic mandatrophy pattern: the solution outlives the problem it solved and expands to cover a domain where it doesn't work.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judgment_decay_curve,
    'What is the quantitative decay curve for judgment-under-stakes capacity when only simulation is used, and how does it differ from procedural decay?',
    'Longitudinal studies tracking operators from simulation-only training through first real-crisis exposure, measuring decision quality degradation over time since last live exercise.',
    'If judgment decays significantly faster than procedures, the hybrid decay reading is empirically validated and the extractiveness of the mandate is higher than currently modeled. If decay curves are similar, the reading overstates the gap.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(judgment_decay_curve, empirical, 'Differential decay rates between procedural and judgment components of competence').

omega_variable(
    fidelity_threshold_for_judgment,
    'Is there a simulation fidelity threshold above which judgment-under-stakes IS exercised, or is the stake-absence fundamentally irreducible?',
    'Progressive fidelity experiments (VR, physiological stress induction, consequence-weighted scoring) measuring transfer to real-crisis judgment tasks.',
    'If a threshold exists, the constraint could be reformed by mandating higher fidelity rather than live exercises — the reading would shift toward simulation_sufficiency. If irreducible, the extraction is structural and the mandate is fundamentally incomplete.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fidelity_threshold_for_judgment, conceptual, 'Whether the judgment gap is a fidelity problem or a category error').

omega_variable(
    kernel_framing_ambiguity,
    'Does the kernel ''exercise as competence maintenance'' refer to a unitary competence or a dual-component structure (procedural + judgment)?',
    'Historical analysis of founding documents (ICAO Annex 1, IAEA safety guides, nuclear regulatory frameworks) to determine whether the original mandate conceptualized competence as unitary or dual.',
    'If the kernel was always dual-component, the simulation_sufficiency reading is a category error. If the kernel was unitary and the dual decomposition is this reading''s innovation, then the sibling readings are alternative framings of the same ambiguous kernel — the hybrid_decay reading is one resolution of the ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_ambiguity, conceptual, 'Whether the kernel''s internal structure is unitary or dual-component').

omega_variable(
    committer_structure_disagreement_location,
    'Where exactly do the three readings disagree structurally — on the kernel''s component structure, on the exercise efficacy of simulation per component, or on the victim set definition?',
    'Map each reading''s claims onto a shared matrix: (components x exercise efficacy x victim set). The disagreement location is the cell(s) where readings diverge.',
    'Locates the committer-axis disagreement for the engine''s kernel-reading machinery. If readings disagree on component structure, the kernel is genuinely ambiguous. If they agree on structure but disagree on efficacy, the kernel is stable but the readings are empirical rivals. If they agree on both but disagree on victim set, the disagreement is normative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_disagreement_location, conceptual, 'Structural location of disagreement among kernel readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(exercise_as_competence_maintenance__hybrid_decay_reading, 1960, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(exercise_hybrid_decay_tr_t1960, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 1960, 0.1).
narrative_ontology:measurement(exercise_hybrid_decay_tr_t1975, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 1975, 0.18).
narrative_ontology:measurement(exercise_hybrid_decay_tr_t1990, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 1990, 0.3).
narrative_ontology:measurement(exercise_hybrid_decay_tr_t2005, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 2005, 0.38).
narrative_ontology:measurement(exercise_hybrid_decay_tr_t2015, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 2015, 0.42).
narrative_ontology:measurement(exercise_hybrid_decay_tr_t2025, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 2025, 0.45).

% Extraction over time
narrative_ontology:measurement(exercise_hybrid_decay_be_t1960, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 1960, 0.15).
narrative_ontology:measurement(exercise_hybrid_decay_be_t1975, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 1975, 0.25).
narrative_ontology:measurement(exercise_hybrid_decay_be_t1990, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 1990, 0.42).
narrative_ontology:measurement(exercise_hybrid_decay_be_t2005, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 2005, 0.55).
narrative_ontology:measurement(exercise_hybrid_decay_be_t2015, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 2015, 0.62).
narrative_ontology:measurement(exercise_hybrid_decay_be_t2025, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(exercise_hybrid_decay_su_t1960, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 1960, 0.25).
narrative_ontology:measurement(exercise_hybrid_decay_su_t1975, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 1975, 0.35).
narrative_ontology:measurement(exercise_hybrid_decay_su_t1990, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 1990, 0.45).
narrative_ontology:measurement(exercise_hybrid_decay_su_t2005, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 2005, 0.5).
narrative_ontology:measurement(exercise_hybrid_decay_su_t2015, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 2015, 0.53).
narrative_ontology:measurement(exercise_hybrid_decay_su_t2025, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 2025, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(exercise_as_competence_maintenance__hybrid_decay_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(exercise_as_competence_maintenance__hybrid_decay_reading, 0.08).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__hybrid_decay_reading, live_exercise_mandate).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__hybrid_decay_reading, apprenticeship_transmission).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__hybrid_decay_reading, red_teaming_requirement).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__hybrid_decay_reading, incident_learning_system).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of kernel 'exercise_as_competence_maintenance'. The simulation_sufficiency_reading treats the kernel as unitary and simulation as fully efficacious. The lived_catastrophe_necessity_reading treats the kernel as requiring real stakes. This hybrid_decay_reading decomposes the kernel into procedural (simulation-efficacious) and judgment (simulation-inefficacious) components. The three readings form a constraint family linked by network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(exercise_as_competence_maintenance__hybrid_decay_reading, institutional, 0.2).
constraint_indexing:directionality_override(exercise_as_competence_maintenance__hybrid_decay_reading, organized, 0.15).
constraint_indexing:directionality_override(exercise_as_competence_maintenance__hybrid_decay_reading, moderate, 0.7).
constraint_indexing:directionality_override(exercise_as_competence_maintenance__hybrid_decay_reading, powerless, 0.95).
constraint_indexing:directionality_override(exercise_as_competence_maintenance__hybrid_decay_reading, powerful, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
