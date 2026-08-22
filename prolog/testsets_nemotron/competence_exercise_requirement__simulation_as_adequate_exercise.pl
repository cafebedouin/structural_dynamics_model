% ============================================================================
% CONSTRAINT STORY: competence_exercise_requirement__simulation_as_adequate_exercise
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_exercise_requirement__simulation_as_adequate_exercise, []).

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
 *   constraint_id: competence_exercise_requirement__simulation_as_adequate_exercise
 *   human_readable: Simulation with High Fidelity and Debriefing Constitutes Adequate Exercise of the Competence Kernel
 *   domain: safety_engineering/organizational_learning/high_reliability_organizations
 *
 * SUMMARY:
 *   In high-reliability organizations (aviation, nuclear, healthcare,
 *   maritime), a dominant reading holds that scheduled high-fidelity
 *   simulation with structured debriefing constitutes adequate exercise of
 *   the competence kernel — the claim that operators maintain readiness
 *   without requiring real catastrophic events or even routine operational
 *   jeopardy. This reading emerged from the 1980s-1990s simulation revolution
 *   (CRM, LOFT, full-flight simulators) and was codified in regulatory
 *   equivalence standards (FAA Part 121/142, EASA, IAEA). The constraint
 *   extracts by shifting the cost of competence maintenance from operational
 *   exposure (risk, scheduling disruption, real consequence) to simulated
 *   exposure — beneficiaries are simulation vendors, regulators who avoid
 *   hard equivalence questions, and organizations that externalize risk;
 *   victims are frontline operators who lose irreducible experience, the
 *   public who trusts a competence assurance that may be hollow, and junior
 *   personnel who never encounter the unscripted ambiguity that builds deep
 *   expertise. The theater ratio is high and rising: simulation programs
 *   increasingly perform compliance rather than build resilience, with
 *   debriefings following scripts, scenarios avoiding true novelty, and
 *   'pass' rates approaching 100%. The claimed type is tangled_rope because
 *   there IS a genuine coordination function (baseline procedural competence,
 *   crew resource management, standardized emergency response) but it is
 *   fused with asymmetric extraction (the irreducible experience gap is
 *   externalized to operators and the public).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_requirement__simulation_as_adequate_exercise, 0.32).
domain_priors:suppression_score(competence_exercise_requirement__simulation_as_adequate_exercise, 0.41).
domain_priors:theater_ratio(competence_exercise_requirement__simulation_as_adequate_exercise, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__simulation_as_adequate_exercise, extractiveness, 0.32).
narrative_ontology:constraint_metric(competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_requirement__simulation_as_adequate_exercise, accessibility_collapse, 0.47).
narrative_ontology:constraint_metric(competence_exercise_requirement__simulation_as_adequate_exercise, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_requirement__simulation_as_adequate_exercise, tangled_rope).
narrative_ontology:human_readable(competence_exercise_requirement__simulation_as_adequate_exercise, "Simulation with High Fidelity and Debriefing Constitutes Adequate Exercise of the Competence Kernel").
narrative_ontology:topic_domain(competence_exercise_requirement__simulation_as_adequate_exercise, "safety_engineering/organizational_learning/high_reliability_organizations").

domain_priors:requires_active_enforcement(competence_exercise_requirement__simulation_as_adequate_exercise).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_requirement__simulation_as_adequate_exercise, '331327fc-a016-48ce-b6cc-b03131daceeb').
narrative_ontology:cs_kernel_codification('331327fc-a016-48ce-b6cc-b03131daceeb', formalized).
narrative_ontology:cs_authority_grounding('331327fc-a016-48ce-b6cc-b03131daceeb', lineage).
narrative_ontology:cs_interpretation_layer_present('331327fc-a016-48ce-b6cc-b03131daceeb').
narrative_ontology:cs_reading_relation('331327fc-a016-48ce-b6cc-b03131daceeb', competence_exercise_requirement__catastrophe_as_necessary_anchor, forecloses).
narrative_ontology:cs_reading_relation('331327fc-a016-48ce-b6cc-b03131daceeb', competence_exercise_requirement__hybrid_dependency, coexists_with).
narrative_ontology:cs_axiom('331327fc-a016-48ce-b6cc-b03131daceeb', foundational, simulation_fidelity_debriefing_sufficiency).
narrative_ontology:cs_axiom_status(simulation_fidelity_debriefing_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('331327fc-a016-48ce-b6cc-b03131daceeb', simulation_fidelity_debriefing_sufficiency, empirically_contingent).
narrative_ontology:cs_axiom('331327fc-a016-48ce-b6cc-b03131daceeb', foundational, catastrophe_free_record_validates_adequacy).
narrative_ontology:cs_axiom_status(catastrophe_free_record_validates_adequacy, holdable).
narrative_ontology:cs_axiom_grounding('331327fc-a016-48ce-b6cc-b03131daceeb', catastrophe_free_record_validates_adequacy, empirically_contingent).
narrative_ontology:cs_axiom('331327fc-a016-48ce-b6cc-b03131daceeb', secondary, regulatory_equivalence_as_competence_certification).
narrative_ontology:cs_axiom_status(regulatory_equivalence_as_competence_certification, holdable).
narrative_ontology:cs_axiom_grounding('331327fc-a016-48ce-b6cc-b03131daceeb', regulatory_equivalence_as_competence_certification, conventional).
narrative_ontology:cs_reference_frame('331327fc-a016-48ce-b6cc-b03131daceeb', standardized_simulation_equivalence_framework).
narrative_ontology:cs_drift_state('331327fc-a016-48ce-b6cc-b03131daceeb', contemporary_post_empirical_challenge_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('331327fc-a016-48ce-b6cc-b03131daceeb', '').
narrative_ontology:cs_kernel_id(competence_exercise_requirement__simulation_as_adequate_exercise, competence_exercise_requirement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__simulation_as_adequate_exercise, simulation_training_vendors).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__simulation_as_adequate_exercise, regulatory_bodies_accepting_simulation_equivalence).
narrative_ontology:constraint_beneficiary(competence_exercise_requirement__simulation_as_adequate_exercise, organizations_avoiding_operational_risk_exposure).
narrative_ontology:constraint_victim(competence_exercise_requirement__simulation_as_adequate_exercise, frontline_operators_denied_real_world_anchoring).
narrative_ontology:constraint_victim(competence_exercise_requirement__simulation_as_adequate_exercise, public_trusting_in_competence_assurance).
narrative_ontology:constraint_victim(competence_exercise_requirement__simulation_as_adequate_exercise, junior_personnel_missing_irreducible_experience).
narrative_ontology:constraint_vindicates(competence_exercise_requirement__simulation_as_adequate_exercise, high_fidelity_simulation_equivalence_thesis).
narrative_ontology:constraint_vindicates(competence_exercise_requirement__simulation_as_adequate_exercise, debriefing_as_sufficient_learning_mechanism).
narrative_ontology:constraint_vindicates(competence_exercise_requirement__simulation_as_adequate_exercise, regulatory_compliance_as_competence_certification).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design, build, and sell high-fidelity simulators and training programs. Revenue scales with regulatory equivalence mandates and organizational substitution of simulation for operational experience. They shape fidelity standards through industry committees and lobbying. Exit is arbitrage-grade: they can pivot to adjacent markets (VR/AR training, gaming, defense) if aviation/nuclear regulation tightens.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, simulation_training_vendors, beneficiary,
    institutional, generational, arbitrage, global).

% Write and enforce regulations that accept simulation hours as equivalent to operational hours for licensing and recurrency. They benefit from administrative simplicity (clear standards, measurable outputs) and avoid the political risk of mandating operational exposure. Their exit is constrained: they are bound by legislative mandate, international standards (ICAO, IAEA), and institutional inertia; reversing equivalence would require massive rulemaking and industry opposition.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, regulatory_bodies_accepting_simulation_equivalence, agenda_setter,
    institutional, generational, constrained, national).

% Airlines, nuclear operators, hospitals, shipping companies that substitute simulation for line operations, operational rotations, and real-world jeopardy exposure. They save on scheduling disruption, insurance, fatigue management, and catastrophic risk. They set internal training policies that maximize simulation compliance. Exit is mobile: they could adopt hybrid models, but the cost structure and regulatory path of least resistance favor simulation-heavy programs.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, organizations_avoiding_operational_risk_exposure, beneficiary,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(competence_exercise_requirement__simulation_as_adequate_exercise, organizations_avoiding_operational_risk_exposure, agenda_setter).

% Pilots, reactor operators, surgeons, bridge officers who complete recurrency entirely in simulators. They lose the irreducible cognitive and psychological conditioning that only genuine jeopardy, unscripted ambiguity, and real consequence provide. Their exit is constrained: they can seek operational assignments (instructor pilot, line check airman, test pilot) but these are scarce and career-structure dependent; leaving the profession entirely is the only full exit.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, frontline_operators_denied_real_world_anchoring, payer,
    organized, biographical, constrained, global).

% Passengers, patients, communities near nuclear facilities who trust that licensed operators have demonstrated competence through means equivalent to real-world exercise. They bear the catastrophic risk if the assurance is hollow. Exit is trapped: they cannot choose operators based on training philosophy, cannot opt out of systemic risk, and have no visibility into the simulation-vs-operational balance.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, public_trusting_in_competence_assurance, payer,
    powerless, biographical, trapped, global).

% Early-career operators whose entire competence formation occurs in simulation-heavy regimes. They never build the deep pattern recognition, stress inoculation, and judgment calibration that comes from unscripted operational experience. Their professional identity fuses with the simulation regime — they know no other way. Exit is identity_locked: leaving means abandoning their professional self-concept; staying means accepting the competence gap as normal.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, junior_personnel_missing_irreducible_experience, payer,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(competence_exercise_requirement__simulation_as_adequate_exercise, junior_personnel_missing_irreducible_experience, excluded).

% Training experts, human factors researchers, some labor organizations arguing that simulation is necessary but insufficient — competence requires both simulation foundation AND periodic real-world anchoring (line operations, non-jeopardy audits, actual aircraft time). They are excluded from regulatory equivalence rulemaking and organizational training policy by the dominant simulation-as-adequate coalition. Their exit is constrained: they publish, testify, and advocate but the structural incentives favor the simulation-heavy status quo.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, hybrid_dependency_advocates, excluded,
    moderate, generational, constrained, global).

% A minority view (some safety theorists, accident investigators, veteran operators) holding that only real catastrophic events or genuine near-misses provide the irreducible exercise that maintains deep competence. They are structurally marginalized — their position is treated as anachronistic or dangerous. Exit is trapped: they cannot operationalize their view without violating the very safety norms the system upholds.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, catastrophe_anchor_proponents, excluded,
    powerless, generational, trapped, global).

% Academic researchers, safety science institutes, investigative bodies studying the long-term competence trajectories of simulation-only vs. hybrid vs. operational-anchoring regimes. They see the full structure but have no power to change it. Their exit is analytical: they can change their framing but not the constraint.
narrative_ontology:constraint_stakeholder(competence_exercise_requirement__simulation_as_adequate_exercise, independent_safety_analysts, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_exercise_requirement__simulation_as_adequate_exercise, simulation_training_vendors).
narrative_ontology:fixing_cost_class(competence_exercise_requirement__simulation_as_adequate_exercise, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a standardized, repeatable, safe, and measurable framework for maintaining baseline procedural competence, crew resource management, and emergency response proficiency across a distributed workforce without exposing operators or the public to catastrophic risk during training.
% TRANSFER_FUNCTION: Moves the cost and risk of competence maintenance from operational exposure (shared organizational risk, real consequence, scheduling disruption) to simulated environments — extracting revenue for vendors, regulatory simplicity for authorities, and risk externalization for organizations; the cost is borne by operators (irreducible experience gap), the public (hollow assurance risk), and junior personnel (truncated professional formation).
% ABSENT_VOICES: Frontline operators who would choose operational anchoring if not career-penalized; junior personnel who have never experienced the alternative and cannot articulate what they're missing; the public who has no voice in training philosophy; accident investigators who see the competence gaps post-hoc but are excluded from prospective policy.
% DISAPPEARANCE_RATIONALE: If simulation-equivalence regulations vanished overnight, organizations would immediately face the problem of how to maintain competence without simulation as a substitute for operational experience. Airlines would restructure line-check programs; nuclear plants would revive operational rotation requirements; medical boards would mandate supervised clinical hours. The simulation vendor market would collapse or pivot. The competence kernel would be re-anchored in operational reality — with higher immediate risk but deeper long-term resilience.
% FOUNDING_PROBLEM: Post-1970s/80s catastrophic accidents (Tenerife 1977, Three Mile Island 1979, Challenger 1986) revealed that ad-hoc, experience-based competence maintenance was insufficient — organizations needed standardized, verifiable, safe training that could be mandated and audited at scale.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (standardized safe competence maintenance) is attested as live by regulators and vendors citing ongoing safety threats. It is attested as substantially solved by major operators and independent safety researchers citing 40+ years of catastrophe-free operations in simulation-heavy regimes. It is attested as redefined by hybrid dependency advocates who argue the problem was 'safe competence maintenance' not 'competence maintenance via simulation equivalence' — the current arrangement solves a narrowed version of the problem while extracting the hardest competencies.
narrative_ontology:disappearance_verdict(competence_exercise_requirement__simulation_as_adequate_exercise, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_requirement__simulation_as_adequate_exercise, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_requirement__simulation_as_adequate_exercise, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(competence_exercise_requirement__simulation_as_adequate_exercise, 'none', 1).
narrative_ontology:epsilon_provenance(competence_exercise_requirement__simulation_as_adequate_exercise, 0.32, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_exercise_requirement__simulation_as_adequate_exercise_tests).
:- end_tests(competence_exercise_requirement__simulation_as_adequate_exercise_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.32) reflects the transfer of competence-maintenance burden from operational reality (shared risk, real consequence) to simulated environments (vendor revenue, organizational convenience, regulatory checkbox). The coordination function is real but bounded: simulation excellently coordinates procedural fluency and CRM — but it fails to coordinate the psychological and cognitive dimensions of genuine jeopardy. Suppression (0.41) is moderate: the constraint suppresses alternatives (operational anchoring, line checks, non-jeopardy audits) through regulatory equivalence rules and organizational inertia, but does not totally forbid them. Theater ratio (0.58) is the signature metric: more than half of simulation activity is performative compliance — scripted scenarios, checkbox debriefings, 'no-fail' cultures — because the constraint's persistence depends on maintaining the illusion of adequacy. Accessibility collapse (0.47) is moderate: alternatives exist (hybrid models, operational anchoring) but are structurally discouraged. Resistance (0.38) is moderate: frontline operators and some training experts resist, but institutional momentum and vendor/regulatory alignment dampen it.
 *
 * PERSPECTIVAL GAP:
 *   From the regulator/vendor/organization seat (beneficiary/agenda_setter), the constraint is a rope: it coordinates a complex training ecosystem, standardizes competence verification, and reduces operational risk. From the frontline operator/junior personnel/public seat (payer/victim), it is a snare: it extracts the irreducible experience that builds deep expertise, replaces it with performative compliance, and masks the extraction behind a coordination story. The engine computes this divergence from the structural data — the declared beneficiaries and victims, their power and exit options, the active enforcement maintaining simulation equivalence.
 *
 * DIRECTIONALITY LOGIC:
 *   Simulation vendors and regulators accepting equivalence are structural beneficiaries: they collect revenue and regulatory simplicity (d near 0.0). Organizations avoiding operational risk are beneficiaries: they externalize the cost of competence maintenance (d ~0.2). Frontline operators denied real-world anchoring are primary targets: they bear the competence gap with constrained exit (d ~0.8). The public trusting competence assurance is a diffuse target: they bear catastrophic risk if the assurance is hollow, with trapped exit (d ~0.9). Junior personnel missing irreducible experience are targets with identity_locked exit — their professional formation occurs within the simulation regime (d ~0.85). Rival approaches (hybrid dependency, catastrophe anchoring) are excluded: they are structurally marginalized by regulatory equivalence standards.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — 'how to maintain operator competence without catastrophic accidents' — was live and urgent post-Tenerife, Three Mile Island, Challenger. The simulation reading solved the immediate coordination problem (standardized, safe, repeatable training). But the mandate has accumulated extraction: simulation vendors expanded scope, regulators accepted equivalence without periodic re-validation, organizations substituted simulation for operational experience. The founding problem (safe competence maintenance) is now contested — some argue it's solved, others that it's been redefined to exclude the hardest competencies. The constraint persists not because it solves the current problem better than alternatives, but because the beneficiary coalition (vendors, regulators, risk-averse management) has the power to maintain it. This is mandatrophy: the original coordination mandate has atrophied into an extraction structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'Is this constraint one reading of the contested kernel ''competence_exercise_requirement'' with siblings catastrophe_as_necessary_anchor and hybrid_dependency?',
    'Committee frame analysis of the kernel''s structural decomposition; the reading instantiates a specific beneficiary/victim structure that differs from siblings.',
    'If this is a kernel reading, ε is assessed for this reading''s structural arrangement (simulation-as-adequate) not the kernel as a whole; sibling readings produce different constraints with different ε.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Commitment that this is a single reading of a contested kernel, not the kernel itself.').

omega_variable(
    simulation_fidelity_threshold_ambiguity,
    'What fidelity threshold separates ''adequate'' simulation from theatrical compliance? Is the threshold technically defined or politically negotiated?',
    'Empirical validation studies comparing simulation-trained vs. operationally-anchored operator performance on edge cases; regulatory audit of fidelity standards.',
    'If fidelity threshold is politically negotiated, the constraint''s extraction is higher — simulation vendors and regulators benefit from a loose standard; if technically defined, coordination function strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_fidelity_threshold_ambiguity, empirical, 'Whether the ''high fidelity'' qualifier is a genuine technical boundary or a cover story.').

omega_variable(
    catastrophe_free_decades_evidence_status,
    'Do catastrophe-free decades genuinely validate the reading, or do they reflect survivor bias and silent degradation of edge-case competence?',
    'Longitudinal analysis of near-miss rates, error propagation patterns, and latent failure accumulation in simulation-only regimes vs. hybrid regimes.',
    'If survivor bias, the constraint''s claimed coordination function is undermined — extraction is higher than measured; if genuine validation, the reading''s legitimacy strengthens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(catastrophe_free_decades_evidence_status, empirical, 'Whether the primary evidence for the reading (catastrophe-free record) is structurally sound or an artifact of silent degradation.').

omega_variable(
    regulatory_capture_via_equivalence_acceptance,
    'Does regulatory acceptance of simulation equivalence reflect genuine expert consensus or regulatory capture by simulation vendors and risk-averse operators?',
    'Analysis of regulatory rulemaking records, vendor lobbying expenditure, revolving-door employment, and dissenting expert testimony excluded from the record.',
    'If capture, the constraint''s beneficiary structure is more extractive than declared; the coordination story is cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_via_equivalence_acceptance, conceptual, 'Whether the regulatory equivalence standard is a genuine coordination achievement or a captured arrangement.').

omega_variable(
    irreducible_experience_gap,
    'Is there an irreducible component of competence that only real-world jeopardy, consequence, and ambiguity can exercise — a gap no simulation fidelity can close?',
    'Phenomenological studies of operator cognition under genuine jeopardy vs. simulated jeopardy; neuroscience of stress response and decision-making; analysis of accidents where simulation-trained operators failed on novel edge cases.',
    'If irreducible gap exists, the constraint''s coordination claim is structurally incomplete — it coordinates baseline competence but extracts resilience; classification shifts toward snare for frontline operators.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(irreducible_experience_gap, conceptual, 'Whether the competence kernel has a component that simulation structurally cannot reach.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_requirement__simulation_as_adequate_exercise, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 0, 0.25).
narrative_ontology:measurement(comp_tr_t8, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 8, 0.35).
narrative_ontology:measurement(comp_tr_t16, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 16, 0.45).
narrative_ontology:measurement(comp_tr_t24, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 24, 0.52).
narrative_ontology:measurement(comp_tr_t32, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 32, 0.56).
narrative_ontology:measurement(comp_tr_t40, competence_exercise_requirement__simulation_as_adequate_exercise, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(comp_be_t8, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 8, 0.22).
narrative_ontology:measurement(comp_be_t16, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 16, 0.27).
narrative_ontology:measurement(comp_be_t24, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 24, 0.3).
narrative_ontology:measurement(comp_be_t32, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 32, 0.31).
narrative_ontology:measurement(comp_be_t40, competence_exercise_requirement__simulation_as_adequate_exercise, base_extractiveness, 40, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(comp_su_t8, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 8, 0.28).
narrative_ontology:measurement(comp_su_t16, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 16, 0.34).
narrative_ontology:measurement(comp_su_t24, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 24, 0.38).
narrative_ontology:measurement(comp_su_t32, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 32, 0.4).
narrative_ontology:measurement(comp_su_t40, competence_exercise_requirement__simulation_as_adequate_exercise, suppression_requirement, 40, 0.41).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_requirement__simulation_as_adequate_exercise, identity_coordination).
narrative_ontology:boltzmann_floor_override(competence_exercise_requirement__simulation_as_adequate_exercise, 0.08).
narrative_ontology:affects_constraint(competence_exercise_requirement__simulation_as_adequate_exercise, competence_exercise_requirement__catastrophe_as_necessary_anchor).
narrative_ontology:affects_constraint(competence_exercise_requirement__simulation_as_adequate_exercise, competence_exercise_requirement__hybrid_dependency).
narrative_ontology:affects_constraint(competence_exercise_requirement__simulation_as_adequate_exercise, regulatory_equivalence_standards_part121).
narrative_ontology:affects_constraint(competence_exercise_requirement__simulation_as_adequate_exercise, simulation_vendor_market_structure).
narrative_ontology:affects_constraint(competence_exercise_requirement__simulation_as_adequate_exercise, operator_licensing_recurrency_requirements).

% DUAL FORMULATION NOTE:
% This constraint is one member of the competence_exercise_requirement kernel family (3 readings). The catastrophe reading forecloses this reading's core sufficiency claim; the hybrid reading coexists with both but structurally pressures this reading by demonstrating its incompleteness. All three share the same referent (what maintains the competence kernel) but instantiate different constraints with different ε, beneficiaries, victims, and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(competence_exercise_requirement__simulation_as_adequate_exercise, institutional, 0.15).
constraint_indexing:directionality_override(competence_exercise_requirement__simulation_as_adequate_exercise, organized, 0.75).
constraint_indexing:directionality_override(competence_exercise_requirement__simulation_as_adequate_exercise, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
