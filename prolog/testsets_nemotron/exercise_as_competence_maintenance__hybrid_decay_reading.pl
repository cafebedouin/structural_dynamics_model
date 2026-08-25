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
 *   constraint_id: exercise_as_competence_maintenance__hybrid_decay_reading
 *   human_readable: Simulation-Based Competence Maintenance in Safety-Critical Organizations (Hybrid Decay Reading)
 *   domain: safety_engineering/organizational_learning/crisis_preparedness
 *
 * SUMMARY:
 *   Safety-critical organizations (aviation, nuclear, healthcare, cyber
 *   defense) maintain competence through mandatory simulation exercises. The
 *   hybrid_decay_reading holds that simulation genuinely exercises and
 *   retains procedural competence (checklists, muscle memory, standard
 *   operating procedures) but fails to exercise judgment-under-stakes — the
 *   capacity to improvise, prioritize under novel failure cascades, and
 *   calibrate risk when procedures run out. The kernel has two components
 *   with different exercise requirements: procedure (simulable) and judgment
 *   (requires real or real-consequence stakes). Victims are frontline
 *   operators who face judgment demands without lived-stakes calibration, the
 *   affected public who bear the consequences of judgment failures, and
 *   junior personnel who never acquire judgment because the system only
 *   exercises procedure. Beneficiaries include regulators who get measurable
 *   compliance metrics, simulation vendors who sell procedural fidelity, and
 *   senior management who get liability shields from 'we trained them.' The
 *   arrangement is a Tangled Rope: it coordinates procedural standardization
 *   (genuine coordination function) while extracting judgment capacity from
 *   those who need it most (asymmetric extraction).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(exercise_as_competence_maintenance__hybrid_decay_reading, 0.38).
domain_priors:suppression_score(exercise_as_competence_maintenance__hybrid_decay_reading, 0.42).
domain_priors:theater_ratio(exercise_as_competence_maintenance__hybrid_decay_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(exercise_as_competence_maintenance__hybrid_decay_reading, tangled_rope).
narrative_ontology:human_readable(exercise_as_competence_maintenance__hybrid_decay_reading, "Simulation-Based Competence Maintenance in Safety-Critical Organizations (Hybrid Decay Reading)").
narrative_ontology:topic_domain(exercise_as_competence_maintenance__hybrid_decay_reading, "safety_engineering/organizational_learning/crisis_preparedness").

domain_priors:requires_active_enforcement(exercise_as_competence_maintenance__hybrid_decay_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(exercise_as_competence_maintenance__hybrid_decay_reading, 'f9829225-5d4c-4201-8149-c4f4c04b9758').
narrative_ontology:cs_kernel_codification('f9829225-5d4c-4201-8149-c4f4c04b9758', distributed).
narrative_ontology:cs_authority_grounding('f9829225-5d4c-4201-8149-c4f4c04b9758', practice).
narrative_ontology:cs_interpretation_layer_present('f9829225-5d4c-4201-8149-c4f4c04b9758').
narrative_ontology:cs_reading_relation('f9829225-5d4c-4201-8149-c4f4c04b9758', exercise_as_competence_maintenance__simulation_sufficiency_reading, influences).
narrative_ontology:cs_reading_relation('f9829225-5d4c-4201-8149-c4f4c04b9758', exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, coexists_with).
narrative_ontology:cs_axiom('f9829225-5d4c-4201-8149-c4f4c04b9758', foundational, competence_is_bifurcated_procedure_and_judgment).
narrative_ontology:cs_axiom_status(competence_is_bifurcated_procedure_and_judgment, holdable).
narrative_ontology:cs_axiom_grounding('f9829225-5d4c-4201-8149-c4f4c04b9758', competence_is_bifurcated_procedure_and_judgment, empirically_contingent).
narrative_ontology:cs_axiom('f9829225-5d4c-4201-8149-c4f4c04b9758', foundational, simulation_exercises_procedure_not_judgment).
narrative_ontology:cs_axiom_status(simulation_exercises_procedure_not_judgment, holdable).
narrative_ontology:cs_axiom_grounding('f9829225-5d4c-4201-8149-c4f4c04b9758', simulation_exercises_procedure_not_judgment, empirically_contingent).
narrative_ontology:cs_reference_frame('f9829225-5d4c-4201-8149-c4f4c04b9758', post_wwii_procedural_standardization_frame).
narrative_ontology:cs_drift_state('f9829225-5d4c-4201-8149-c4f4c04b9758', contemporary_fidelity_metrics_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f9829225-5d4c-4201-8149-c4f4c04b9758', '').
narrative_ontology:cs_kernel_id(exercise_as_competence_maintenance__hybrid_decay_reading, exercise_as_competence_maintenance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__hybrid_decay_reading, safety_regulation_bodies).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__hybrid_decay_reading, simulation_vendor_ecosystem).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__hybrid_decay_reading, senior_management_liability_shields).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__hybrid_decay_reading, frontline_operators).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__hybrid_decay_reading, affected_public).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__hybrid_decay_reading, junior_personnel_without_lived_experience).
narrative_ontology:constraint_vindicates(exercise_as_competence_maintenance__hybrid_decay_reading, procedural_compliance_as_safety_surrogate).
narrative_ontology:constraint_vindicates(exercise_as_competence_maintenance__hybrid_decay_reading, simulation_fidelity_as_competence_proxy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Mandate simulation hours and fidelity standards as the measurable proxy for competence. They collect compliance data, issue certifications, and defend the regime against challenges. The regime gives them auditable metrics and inter-organizational standardization — genuine coordination value. They do not bear the judgment gap in real events.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, safety_regulation_bodies, agenda_setter,
    institutional, generational, arbitrage, global).

% Sell simulation platforms, scenario libraries, fidelity upgrades, and compliance tracking. Their revenue grows with mandated hours and fidelity requirements. They advocate for simulation as the primary competence mechanism because it is their market. They do not operate in real events and do not bear judgment failures.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, simulation_vendor_ecosystem, beneficiary,
    organized, biographical, arbitrage, global).

% Use simulation compliance records as legal and regulatory defense: 'we met all training requirements.' The simulation regime converts an unmeasurable duty (judgment readiness) into a measurable checklist (simulation hours). They capture the benefit of legal protection; the cost of judgment failures falls on frontline operators and the public.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, senior_management_liability_shields, beneficiary,
    institutional, biographical, mobile, national).

% Complete mandated simulation hours, maintain procedural currency. In real events, they face novel cascades where procedures are silent or contradictory — judgment under stakes. They have constrained exit: licenses, certifications, and career investment tie them to the regime. They know the judgment gap exists but cannot demand what the system does not measure. When judgment fails, they bear personal, professional, and legal consequences.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, frontline_operators, payer,
    moderate, biographical, constrained, global).

% Bear the consequences of judgment failures in aviation, nuclear, medical, cyber events. They have zero exit from the systems (air travel, power grid, healthcare, digital infrastructure) and zero voice in competence maintenance design. They benefit from procedural safety (fewer baseline errors) but pay the full cost of judgment failures that simulation did not prevent.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, affected_public, payer,
    powerless, immediate, trapped, global).

% Enter the profession after the last real catastrophe; their entire formation is simulation-based. They have never experienced real stakes, so they cannot calibrate their judgment or recognize the gap. Their professional identity fuses with the simulation regime — 'this is how we train.' They are identity-locked: leaving the profession means abandoning their self-concept. They will be the operators facing the next novel cascade with only procedural preparation.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, junior_personnel_without_lived_experience, payer,
    moderate, biographical, identity_locked, global).

% Study the simulation-judgment gap across domains. They document incident trajectories where procedural compliance coexisted with judgment failure. They have no stake in the regime but produce the evidence that could challenge it. Their work is cited by regulators to justify more simulation, not less — the coordination function absorbs the critique.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, independent_safety_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(exercise_as_competence_maintenance__hybrid_decay_reading, senior_management_liability_shields).
narrative_ontology:fixing_cost_class(exercise_as_competence_maintenance__hybrid_decay_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardizes and audits baseline procedural competence across distributed, high-consequence organizations — ensures every operator can execute known procedures under known conditions, creating a measurable floor of operational reliability.
% TRANSFER_FUNCTION: Moves resources (budget, time, career capital) from frontline operational capacity to simulation infrastructure and compliance overhead. Moves judgment-building opportunities (apprenticeship, controlled real-stakes exposure, adversarial exercises) from operators to the compliance regime. Moves liability risk from senior management to frontline operators and the public.
% ABSENT_VOICES: Operators who have survived real catastrophe and would testify that simulation did not prepare them for the judgment demands — they are either retired, silenced by NDAs, or their testimony is treated as 'anecdotal' vs. the 'data' of simulation hours. Communities downstream of high-consequence facilities who would demand judgment readiness over procedural compliance — they are not in the regulatory room.
% DISAPPEARANCE_RATIONALE: If the simulation mandate vanished overnight, organizations would initially lose procedural standardization (baseline error rates would rise). But within 2-3 years, alternative competence models would emerge: apprenticeship revival, controlled real-stakes exposure programs, adversarial exercise frameworks. The judgment gap would become visible and addressable. The simulation vendor market would collapse. Senior management would lose liability shields. The world would rearrange around actual competence rather than its proxy.
% FOUNDING_PROBLEM: Post-WWII expansion of high-consequence industries (aviation, nuclear, later healthcare) created a crisis of baseline competence: no standard way to ensure every operator could execute critical procedures reliably. The founding problem was procedural drift — operators deviating from known-safe procedures over time. Simulation offered a measurable, scalable, auditable solution.
% FOUNDING_PROBLEM_CORROBORATION: Regulators and simulation vendors attest the founding problem (procedural drift) is live and growing with system complexity. Frontline operators, independent safety researchers, and incident investigators attest the founding problem is substantially solved for procedure but the arrangement now creates a new problem (judgment decay) that extracts from the vulnerable. The corroboration split is the signal: the arrangement's beneficiaries defend the founding problem; its victims identify the emergent problem.
narrative_ontology:disappearance_verdict(exercise_as_competence_maintenance__hybrid_decay_reading, world_rearranges).
narrative_ontology:founding_problem_status(exercise_as_competence_maintenance__hybrid_decay_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(exercise_as_competence_maintenance__hybrid_decay_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-25',
    'no_scope_rebuild_nemotron+seed_rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(exercise_as_competence_maintenance__hybrid_decay_reading, 'none', 1).
narrative_ontology:epsilon_provenance(exercise_as_competence_maintenance__hybrid_decay_reading, 0.38, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(exercise_as_competence_maintenance__hybrid_decay_reading_tests).
:- end_tests(exercise_as_competence_maintenance__hybrid_decay_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38) reflects the gap between procedural compliance and actual safety outcomes — organizations pay for simulation, get procedural retention, but lose judgment capacity that only real stakes build. Suppression (0.42) is moderate: regulatory mandates create structural pressure, but internalized belief in simulation sufficiency is the stronger suppressor — operators don't demand what they don't know they lack. Theater ratio (0.55) is high and rising: simulation hours and fidelity metrics increasingly substitute for judgment demonstration. Accessibility collapse (0.45) is partial — alternatives exist (apprenticeship, controlled real-stakes exposure, adversarial exercises) but are marginalized by the simulation compliance regime. Resistance (0.25) is low: few inside the system challenge it because procedural compliance is legally protective and career-safe.
 *
 * PERSPECTIVAL GAP:
 *   From the regulator/senior management seat, the constraint is a Rope: it standardizes preparation, produces auditable metrics, and coordinates across organizations. From the frontline operator seat, it is a Snare: the simulation hours are real cost, the judgment gap is real risk, and the alternative (lived catastrophe) is structurally suppressed. From the affected public seat, it is a Tangled Rope: they get procedural safety (coordination) but bear judgment failures (extraction). The engine computes this divergence from the structural data — the authored claim (tangled_rope) captures the hybrid reality that no single seat experiences as pure coordination or pure extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Senior management and regulators are structural beneficiaries (d ~ 0.15-0.25): they collect compliance assurance and liability protection at low personal cost. Simulation vendors are beneficiaries (d ~ 0.2): they capture budget for fidelity upgrades that don't address the judgment gap. Frontline operators are primary targets (d ~ 0.8): they bear the judgment demand in real events with only procedural preparation, constrained exit (certification requirements, license dependence). Affected public are targets (d ~ 0.9): they bear consequences with zero exit. Junior personnel are identity-locked targets (d ~ 0.85): their professional formation occurs entirely within the simulation regime; they cannot evaluate what they've never experienced. The hybrid structure means procedural competence is a genuine coordination benefit (lowering baseline error rates) while judgment decay is the extraction — the same constraint does both.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (standardizing baseline competence in high-consequence domains) is partially live — procedural drift is real. But the arrangement has accumulated extraction: judgment decay was not the founding problem, and the constraint now actively prevents the judgment-building alternatives (apprenticeship, controlled real-stakes exposure) that would solve the actual safety problem. Mandatrophy is unresolved: the constraint persists because it solves the measurable problem (procedural compliance) while the unmeasured problem (judgment capacity) extracts from the vulnerable. The theater ratio rise documents the substitution of proxy for purpose.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint one reading of the exercise_as_competence_maintenance kernel (hybrid_decay_reading), and what would the sibling readings change structurally?',
    'Compare victim sets, extraction profiles, and coordination claims across the three readings: hybrid_decay_reading (partial retention, judgment decay), simulation_sufficiency_reading (full retention via fidelity), lived_catastrophe_necessity_reading (no retention without real stakes). The structural delta is in which competence component is exercised and who bears the cost of the gap.',
    'If the kernel framing is correct, this reading''s ε=0.38 reflects a genuine hybrid structure; if the kernel is a false unity, each reading is a separate constraint and should be authored independently without committer-frame linkage.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Committee-frame identity: this constraint is the hybrid_decay_reading of kernel exercise_as_competence_maintenance; sibling readings are simulation_sufficiency_reading and lived_catastrophe_necessity_reading').

omega_variable(
    judgment_decay_mechanism_ambiguity,
    'Is judgment-under-stakes decay structural (simulation cannot replicate the neurobiological/affective conditions of real consequence) or institutional (organizations do not invest in judgment-rich simulation because procedural simulation is cheaper and legally sufficient)?',
    'Compare judgment retention in organizations that run high-fidelity consequential simulation (e.g., military RED FLAG, nuclear reactor full-scope simulators with career consequences) vs. those that run procedural drill-only programs. Track incident trajectories where judgment failure was causal.',
    'If structural, the hybrid decay is a Mountain-like limit on simulation — the constraint''s extraction is partly a law of learning. If institutional, the extraction is a choice and the constraint is a Snare or Tangled Rope with higher effective extraction than measured.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judgment_decay_mechanism_ambiguity, empirical, 'Whether judgment decay is a simulation ceiling or an investment choice').

omega_variable(
    procedural_judgment_boundary,
    'Where exactly does the boundary lie between ''procedural competence'' (retained via simulation) and ''judgment-under-stakes'' (decays without real catastrophe)? Is the boundary stable across domains (aviation, nuclear, medical, cyber) or domain-contingent?',
    'Domain-by-domain analysis of incident reports where operators followed procedures correctly but judgment failed (e.g., Air France 447, Three Mile Island, Elaine Bromiley case). Map the procedural/ judgment boundary in each.',
    'If the boundary is domain-stable, the two-component kernel structure is real and ε is coherent. If domain-contingent, the kernel may be an analytical convenience that conflates distinct constraints — each domain would need its own story.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(procedural_judgment_boundary, conceptual, 'Stability of the procedural/judgment competence boundary across safety-critical domains').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.42) structural (regulatory mandate for simulation hours, career penalties for non-compliance) or internalized (operators believe simulation is sufficient, have no lived catastrophe to disconfirm, and suppress their own doubt about judgment readiness)?',
    'Post-catastrophe suppression trajectory: if operators who survive real catastrophe report that suppression was internalized (they ''thought they were ready''), the internalized component is significant. If suppression vanishes when mandate is removed, it is primarily structural.',
    'If internalized, effective suppression is higher than measured — the target carries the suppression after exit from the simulation regime. This would increase effective extraction for frontline operators and affected public.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in simulation-based competence maintenance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(exercise_as_competence_maintenance__hybrid_decay_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(exer_tr_t0, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(exer_tr_t5, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 5, 0.35).
narrative_ontology:measurement(exer_tr_t10, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 10, 0.42).
narrative_ontology:measurement(exer_tr_t15, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 15, 0.48).
narrative_ontology:measurement(exer_tr_t20, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 20, 0.52).
narrative_ontology:measurement(exer_tr_t25, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 25, 0.55).

% Extraction over time
narrative_ontology:measurement(exer_be_t0, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(exer_be_t5, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 5, 0.28).
narrative_ontology:measurement(exer_be_t10, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 10, 0.32).
narrative_ontology:measurement(exer_be_t15, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 15, 0.35).
narrative_ontology:measurement(exer_be_t20, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 20, 0.37).
narrative_ontology:measurement(exer_be_t25, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 25, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(exer_su_t0, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(exer_su_t5, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 5, 0.35).
narrative_ontology:measurement(exer_su_t10, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 10, 0.38).
narrative_ontology:measurement(exer_su_t15, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 15, 0.4).
narrative_ontology:measurement(exer_su_t20, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 20, 0.41).
narrative_ontology:measurement(exer_su_t25, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 25, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(exercise_as_competence_maintenance__hybrid_decay_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(exercise_as_competence_maintenance__hybrid_decay_reading, 0.1).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__hybrid_decay_reading, exercise_as_competence_maintenance__simulation_sufficiency_reading).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__hybrid_decay_reading, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__hybrid_decay_reading, regulatory_capture_via_compliance_metrics).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__hybrid_decay_reading, simulation_vendor_market_consolidation).

% DUAL FORMULATION NOTE:
% Kernel exercise_as_competence_maintenance decomposes into three readings with distinct ε and victim structures. This reading (hybrid_decay) has ε=0.38, victims={frontline_operators, affected_public, junior_personnel}, coordination=procedural standardization. simulation_sufficiency_reading claims ε≈0.15, victims=minimal, coordination=full competence. lived_catastrophe_necessity_reading claims ε≈0.65, victims=all personnel without catastrophe exposure, coordination=none (simulation is decoy). The ε-invariance principle requires separate stories — the 'competence' label covers structurally distinct claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(exercise_as_competence_maintenance__hybrid_decay_reading, institutional, 0.15).
constraint_indexing:directionality_override(exercise_as_competence_maintenance__hybrid_decay_reading, organized, 0.2).
constraint_indexing:directionality_override(exercise_as_competence_maintenance__hybrid_decay_reading, moderate, 0.85).
constraint_indexing:directionality_override(exercise_as_competence_maintenance__hybrid_decay_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
