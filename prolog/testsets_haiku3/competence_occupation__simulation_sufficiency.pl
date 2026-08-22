% ============================================================================
% CONSTRAINT STORY: competence_occupation__simulation_sufficiency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_occupation__simulation_sufficiency, []).

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
 *   constraint_id: competence_occupation__simulation_sufficiency
 *   human_readable: Simulation-Sufficiency Competence Occupation Model
 *   domain: organizational/safety
 *
 * SUMMARY:
 *   Simulation-sufficiency doctrine holds that competence occupation and
 *   skill-decay prevention can be achieved through repeated simulator-based
 *   drills, provided frequency and fidelity are optimized. This reading
 *   treats the competence kernel as occupiable through measurable training
 *   compliance — drill completion, performance metrics, certification
 *   records. The simulation industry and regulatory compliance apparatus
 *   benefit by reducing competence assessment to documented training
 *   evidence. Operational crews and safety practitioners pay through
 *   constrained exit options (professional retaliation for questioning the
 *   doctrine) and identity-lock (practitioners' professional identity is
 *   constituted through authentic competence cultivation, making
 *   simulation-sufficiency doctrine incoherent with their commitments). The
 *   constraint is CLAIMED as tangled_rope (real coordination function:
 *   simulators do provide scalable, repeatable training venues; real
 *   asymmetric extraction: doctrine overreaches beyond founding problem to
 *   substitute for integrated competence maintenance). The authored metrics
 *   describe substantial extraction (0.68) and high theater (0.58),
 *   indicating the constraint's function has drifted from cost-reduction in
 *   live training toward validation of compliance apparatus and revenue
 *   capture for the simulation industry.
 *
 * KEY AGENTS:
 *   - simulation_training_industry: primary beneficiary, agenda-setter; controls curriculum design, certification criteria, and validation standards
 *   - regulatory_compliance_apparatus: beneficiary with agenda-setter secondary role; simplifies oversight by accepting training records as competence proxy
 *   - operational_crews: payers with constrained exit; bear temporal/cognitive cost of simulator cycles and professional risk of questioning sufficiency doctrine
 *   - safety_culture_practitioners: payers with identity_locked exit; professional identity depends on authentic competence cultivation incompatible with simulation-sufficiency framing
 *   - incident_survivors_and_families: excluded; would disconfirm sufficiency doctrine if seated
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_occupation__simulation_sufficiency, 0.68).
domain_priors:suppression_score(competence_occupation__simulation_sufficiency, 0.71).
domain_priors:theater_ratio(competence_occupation__simulation_sufficiency, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, extractiveness, 0.68).
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(competence_occupation__simulation_sufficiency, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_occupation__simulation_sufficiency, tangled_rope).
narrative_ontology:human_readable(competence_occupation__simulation_sufficiency, "Simulation-Sufficiency Competence Occupation Model").
narrative_ontology:topic_domain(competence_occupation__simulation_sufficiency, "organizational/safety").

domain_priors:requires_active_enforcement(competence_occupation__simulation_sufficiency).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_occupation__simulation_sufficiency, '5f123096-a667-40f1-9347-2f947ccea92e').
narrative_ontology:cs_kernel_codification('5f123096-a667-40f1-9347-2f947ccea92e', formalized).
narrative_ontology:cs_authority_grounding('5f123096-a667-40f1-9347-2f947ccea92e', extraction).
narrative_ontology:cs_interpretation_layer_present('5f123096-a667-40f1-9347-2f947ccea92e').
narrative_ontology:cs_reading_relation('5f123096-a667-40f1-9347-2f947ccea92e', competence_occupation__real_incident_necessity, coexists_with).
narrative_ontology:cs_reading_relation('5f123096-a667-40f1-9347-2f947ccea92e', competence_occupation__hybrid_occupation, influences).
narrative_ontology:cs_axiom('5f123096-a667-40f1-9347-2f947ccea92e', foundational, measurement_exhaustiveness_thesis).
narrative_ontology:cs_axiom_status(measurement_exhaustiveness_thesis, holdable).
narrative_ontology:cs_axiom_grounding('5f123096-a667-40f1-9347-2f947ccea92e', measurement_exhaustiveness_thesis, empirically_contingent).
narrative_ontology:cs_axiom('5f123096-a667-40f1-9347-2f947ccea92e', foundational, fidelity_substitution_sufficiency).
narrative_ontology:cs_axiom_status(fidelity_substitution_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('5f123096-a667-40f1-9347-2f947ccea92e', fidelity_substitution_sufficiency, empirically_contingent).
narrative_ontology:cs_reference_frame('5f123096-a667-40f1-9347-2f947ccea92e', simulator_cost_reduction_paradigm).
narrative_ontology:cs_drift_state('5f123096-a667-40f1-9347-2f947ccea92e', post_regulatory_expansion_era, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('5f123096-a667-40f1-9347-2f947ccea92e', '2026-06-12T00:00:00Z').
narrative_ontology:cs_kernel_id(competence_occupation__simulation_sufficiency, competence_occupation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_occupation__simulation_sufficiency, simulation_training_industry).
narrative_ontology:constraint_beneficiary(competence_occupation__simulation_sufficiency, regulatory_compliance_apparatus).
narrative_ontology:constraint_victim(competence_occupation__simulation_sufficiency, operational_crews).
narrative_ontology:constraint_victim(competence_occupation__simulation_sufficiency, safety_culture_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs, operates, and maintains simulation equipment and curricula. Collects revenue from training contracts and certifications. Frames competence occupation as measurable through simulator performance metrics and drill completion records. Defends simulation-sufficiency as the legitimate pathway to regulatory sign-off and operational readiness.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, simulation_training_industry, agenda_setter,
    organized, generational, arbitrage, global).

% Certifies operational readiness through documented simulator performance and training completion records. Simplifies compliance verification by treating training records as a proxy for actual competence. Reduces liability exposure by shifting accountability from the regulator's competence assessment to the operator's documented training compliance.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, regulatory_compliance_apparatus, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(competence_occupation__simulation_sufficiency, regulatory_compliance_apparatus, agenda_setter).

% Must pass simulator drills and maintain training records to retain certification. Bear the cognitive and temporal cost of repeated simulator cycles that may not map to actual operational conditions. Cannot exit the constraint without losing employment; cannot challenge simulation-sufficiency doctrine without professional retaliation.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, operational_crews, payer,
    moderate, biographical, constrained, national).

% Work within high-reliability organizations to cultivate competence through integrated practice (simulation, real-world feedback, procedural audits, team learning). Experience simulation-sufficiency doctrine as constraining the diversity of mechanisms they deploy. Identity-locked: their professional identity is constituted through commitment to authentic competence cultivation; accepting simulation-sufficiency as complete requires abandoning core professional commitments.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, safety_culture_practitioners, payer,
    moderate, biographical, identity_locked, national).

% Would testify, if present, that simulator drill records failed to predict or prevent operational failures. Their absence from training-design and competence-certification processes is structural; they have no seat at the table where simulation-sufficiency is defended.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, incident_survivors_and_families, excluded,
    powerless, immediate, trapped, local).

% Conduct empirical research into competence maintenance mechanisms. Observe that measured skill decay correlates weakly with simulator performance but correlates strongly with multi-mechanism exercise (simulation + line audit + procedural reinforcement). Their research is often deferred or funded by the simulation industry, creating epistemic conflicts of interest.
narrative_ontology:constraint_stakeholder(competence_occupation__simulation_sufficiency, hybrid_training_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_occupation__simulation_sufficiency, simulation_training_industry).
narrative_ontology:fixing_cost_class(competence_occupation__simulation_sufficiency, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a scalable, measurable, repeatable framework for operator certification and competence assessment: simulators offer controlled conditions for testing decision-making under stress, uniform curriculum coverage, and documented evidence trails for compliance auditing.
% TRANSFER_FUNCTION: Moves revenue from high-reliability organizations to simulation vendors; moves regulatory certification authority from regulator-conducted assessments to operator-generated training records; moves operational crews' cognitive and temporal resources into simulator cycles rather than integrated real-world feedback mechanisms.
% ABSENT_VOICES: Crews whose skill degraded despite simulator drill compliance; families of incident victims whose operators passed all simulator certifications; researchers whose findings contradict simulation-sufficiency but whose funding depends on simulation industry patronage.
% DISAPPEARANCE_RATIONALE: If simulation-sufficiency doctrine vanished, high-reliability organizations would rebalance competence maintenance toward integrated multi-mechanism exercise (line audits, procedural reinforcement, authentic crisis response scenarios, real-time feedback from operational incidents). Simulator vendors would face contract renegotiation. Regulatory frameworks would demand direct competence verification rather than training-record proxies. Operational crews would experience different certification pathways and evaluation criteria.
% FOUNDING_PROBLEM: In the 1970s-1990s, high-reliability organizations struggled with the cost and safety risk of conducting live-scenario training on critical infrastructure. Simulators offered a way to exercise decision-making under stress without live risk.
% FOUNDING_PROBLEM_CORROBORATION: The simulation industry attests the problem is live and simulators remain essential. Operational crews and safety practitioners report that simulators address the original problem (cost/safety of live training) but have been reframed as sufficient for occupying the competence kernel — a boundary shift the founding problem does not warrant. Empirical research from outside the simulation industry documents weak correlation between simulator performance and real-world competence decay; this corroboration is sparse and often defunded.
narrative_ontology:disappearance_verdict(competence_occupation__simulation_sufficiency, world_rearranges).
narrative_ontology:founding_problem_status(competence_occupation__simulation_sufficiency, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_occupation__simulation_sufficiency, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(competence_occupation__simulation_sufficiency, 'none', 1).
narrative_ontology:epsilon_provenance(competence_occupation__simulation_sufficiency, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_occupation__simulation_sufficiency_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_occupation__simulation_sufficiency, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_occupation__simulation_sufficiency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises over the interval (0.45→0.68) as the constraint's scope expands: initial justification as live-training-cost-reduction gives way to justification-by-optimization, shifting the constraint from coordinating a specific safety function toward validating an entire epistemic apparatus. Theater rises sharply (0.35→0.58), indicating a growing share of drill activity is devoted to generating compliance records rather than maintaining authentic competence. Suppression requirement rises (0.52→0.71) as industry, regulator, and institutional actors invest in defending simulation-sufficiency against empirical challenges and practitioner resistance. Accessibility_collapse is moderate (0.62) because alternative mechanisms (hybrid training, line audits) remain conceptually available but are practically suppressed through funding, credentialing, and professional norms. Resistance is moderate (0.55) because operational crews and practitioners understand the constraint but lack coordinated institutional power to challenge it. The shared time grid ensures every metric is authored at every time point; projected values (t=25-35) assume continued enforcement at the interval-end level as the doctrine ossifies.
 *
 * PERSPECTIVAL GAP:
 *   From the simulation_industry and regulator seats, simulation-sufficiency is genuine coordination: cost-reduction, scalability, reproducibility, audit trails. From the operational_crew and safety_practitioner seats, the same structure operates as enforced extraction disguised as optimization. The engine computes this divergence from the structural data: the beneficiary seats have low directionality (they collect rents), the payer seats have high directionality (they bear constrained costs). The claimed_type (tangled_rope) reflects the structural ambiguity: there IS a real coordination function (simulators do solve the foundational cost/safety problem from the 1970s), but the constraint has drifted toward asymmetric extraction (doctrine expansion beyond its founding warrant, suppression of alternative mechanisms, conversion of simulators from tools into validators of compliance). The metrics show both: extractiveness and theater rise over time, indicating extraction layering on coordination rather than pure coordination replacement.
 *
 * DIRECTIONALITY LOGIC:
 *   The simulation_training_industry (institutional power, arbitrage exit) benefits by capturing training contracts and validation authority — directionality near 0.0 (full beneficiary). The regulatory_compliance_apparatus (institutional power, arbitrage exit) benefits by reducing assessment burden and liability exposure — directionality near 0.15 (beneficiary with modest overhead). Operational_crews (moderate power, constrained exit, biographical time_horizon) pay the cost of repetitive simulator cycles and constrain their careers around certification pathways — directionality near 0.75 (substantial target). Safety_culture_practitioners (moderate power, identity_locked exit, biographical time_horizon) bear the identity-coherence cost of accepting simulation-sufficiency as complete — directionality near 0.80 (substantial target with internalized suppression component). The identity-lock on practitioners derives from the mismatch between their professional commitments (authentic competence cultivation across multi-mechanism integration) and the constraint's framing (competence as measurement-equivalent to compliance); exiting the constraint requires identity reconstruction, which the constraint's institutional embedding makes structurally difficult.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem (live-training cost and safety risk) was authentic and solved by simulators. The founding_problem_status is CONTESTED: the industry attests the problem is live (crews still need to exercise decision-making under stress without live risk), while practitioners attest the problem is substantially solved and the constraint has been repurposed as compliance validation. The disappearance_verdict is WORLD_REARRANGES: if simulation-sufficiency doctrine vanished, organizations would rebalance competence maintenance mechanisms, crews would experience different certification pathways, and vendors would face contract renegotiation. The mismatch (status=contested + verdict=world_rearranges) indicates mandatrophy: the founding problem no longer justifies the constraint's current scope and beneficiary alignment. The constraint persists because it serves the simulation industry and regulator, not because the founding problem demands it. Theater_ratio (0.58) corroborates: more than half of drill activity is devoted to generating compliance records rather than authentic competence cultivation, indicating performative maintenance of the doctrine.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Does the competence kernel require simulated exercise alone, or does authentic catastrophic risk constitute an irreducible element of competence occupation?',
    'Empirical comparison of skill-decay trajectories between crews trained solely via simulation and crews with authentic incident exposure (blinded to training method). Post-incident performance analysis revealing whether simulator-trained crews demonstrate competence or degradation.',
    'If authentic risk is necessary, this reading forecloses to real_incident_necessity and the simulation-sufficiency doctrine becomes a false natural law disguising extraction. If simulation is sufficient, this reading holds and the hybrid_occupation reading becomes over-constrained.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contestation, empirical, 'Whether the competence kernel''s occupancy conditions are indifferent to incident authenticity.').

omega_variable(
    measurement_coupling_ambiguity,
    'Are the training metrics (simulator performance, drill completion, certification records) actual measures of competence occupation, or are they substitutes that satisfy compliance apparatus requirements while masking underlying skill decay?',
    'Track operational crews through incident scenarios (real or high-fidelity simulated with authentic consequences). Measure correlation between metric pass/fail and actual incident response performance, skill retention post-incident, and team learning velocity.',
    'If correlation is strong, the metrics legitimately occupy the competence kernel. If weak or inverse, the metrics are theater and the constraint is pure extraction disguised as coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(measurement_coupling_ambiguity, empirical, 'Whether training metrics proxy for authentic competence or constitute performative substitution.').

omega_variable(
    suppression_mechanism_boundary,
    'Is suppression of alternative competence-maintenance mechanisms (hybrid training, line audits, procedural reinforcement) structural (the simulation paradigm genuinely works better) or internalized (crews and practitioners have absorbed the doctrine and abandoned experimentation)?',
    'Post-regulatory-change trajectory: if suppression persists after the simulation-sufficiency doctrine is formally abandoned, reclassify as partially internalized. If suppression dissipates, reclassify as structural.',
    'If internalized, crews and practitioners carry the suppression with them even after exiting; the constraint''s effective suppression is higher than the documented measure suggests. If structural, the suppression reflects the paradigm''s actual scope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_boundary, empirical, 'Whether measured suppression is structural or internalized in the competence-maintenance community.').

omega_variable(
    identity_lock_depth,
    'For safety practitioners identity-locked to the constraint, what specific mechanism binds them? Is it career-path dependence on simulator-certified credentials, relational identity (self-concept constituted through participation in simulation-based training), ideological identity (worldview treating authentic competence as measurement-equivalent to compliance), or institutional identity (the practitioner''s organization has become synonymous with simulation delivery)?',
    'Narrative interviews with practitioners who have left or attempted to leave simulation-sufficiency frameworks; tracking of career trajectories and identity shifts among exiters; documentation of explicit ideological commitments and how they change or persist post-exit.',
    'If lock is primarily career-path dependence, exit barriers are economic and removable. If relational, ideological, or institutional, exit requires identity reconstruction and is far more costly; the constraint''s effective suppression on this group is higher than the base measure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_depth, empirical, 'The specific identity-fusion mechanism binding safety practitioners to simulation-sufficiency doctrine.').

omega_variable(
    regulatory_capture_asymmetry,
    'To what extent does the regulatory compliance apparatus benefit from simulation-sufficiency independently of genuine competence outcomes, creating perverse incentive alignment with the simulation industry?',
    'Institutional analysis: do regulators face penalties for competence failures (incidents post-certification) proportional to their tolerance for simulation-only training? If penalties are low or indirect, capture is probable. If high and direct, capture is less likely despite aligned interests.',
    'If regulatory capture is strong, the apparatus''s framing of simulation-sufficiency as legitimating strategy amplifies extraction. If weak, the apparatus''s participation in the constraint is less extractive and more coordinative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_asymmetry, conceptual, 'Whether regulatory-apparatus alignment with simulation-sufficiency reflects genuine coordination interest or institutional capture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_occupation__simulation_sufficiency, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_occupation__simulation_sufficiency, theater_ratio, 0, 0.35).
narrative_ontology:measurement(comp_tr_t5, competence_occupation__simulation_sufficiency, theater_ratio, 5, 0.41).
narrative_ontology:measurement(comp_tr_t10, competence_occupation__simulation_sufficiency, theater_ratio, 10, 0.47).
narrative_ontology:measurement(comp_tr_t15, competence_occupation__simulation_sufficiency, theater_ratio, 15, 0.53).
narrative_ontology:measurement(comp_tr_t20, competence_occupation__simulation_sufficiency, theater_ratio, 20, 0.56).
narrative_ontology:measurement(comp_tr_t25, competence_occupation__simulation_sufficiency, theater_ratio, 25, 0.58).
narrative_ontology:measurement(comp_tr_t30, competence_occupation__simulation_sufficiency, theater_ratio, 30, 0.58).
narrative_ontology:measurement(comp_tr_t35, competence_occupation__simulation_sufficiency, theater_ratio, 35, 0.58).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_occupation__simulation_sufficiency, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(comp_be_t5, competence_occupation__simulation_sufficiency, base_extractiveness, 5, 0.51).
narrative_ontology:measurement(comp_be_t10, competence_occupation__simulation_sufficiency, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(comp_be_t15, competence_occupation__simulation_sufficiency, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(comp_be_t20, competence_occupation__simulation_sufficiency, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(comp_be_t25, competence_occupation__simulation_sufficiency, base_extractiveness, 25, 0.68).
narrative_ontology:measurement(comp_be_t30, competence_occupation__simulation_sufficiency, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(comp_be_t35, competence_occupation__simulation_sufficiency, base_extractiveness, 35, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_occupation__simulation_sufficiency, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(comp_su_t5, competence_occupation__simulation_sufficiency, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(comp_su_t10, competence_occupation__simulation_sufficiency, suppression_requirement, 10, 0.64).
narrative_ontology:measurement(comp_su_t15, competence_occupation__simulation_sufficiency, suppression_requirement, 15, 0.69).
narrative_ontology:measurement(comp_su_t20, competence_occupation__simulation_sufficiency, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(comp_su_t25, competence_occupation__simulation_sufficiency, suppression_requirement, 25, 0.71).
narrative_ontology:measurement(comp_su_t30, competence_occupation__simulation_sufficiency, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(comp_su_t35, competence_occupation__simulation_sufficiency, suppression_requirement, 35, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_occupation__simulation_sufficiency, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_occupation__simulation_sufficiency, 0.18).
narrative_ontology:affects_constraint(competence_occupation__simulation_sufficiency, competence_occupation__real_incident_necessity).
narrative_ontology:affects_constraint(competence_occupation__simulation_sufficiency, competence_occupation__hybrid_occupation).

% DUAL FORMULATION NOTE:
% The competence_occupation kernel decomposes into three structurally distinct constraints (readings) based on which conditions are SUFFICIENT to occupy the kernel. Each reading instantiates a different constraint with different ε values, beneficiary/victim structures, and time horizons. Simulation_sufficiency treats measurement-equivalence as sufficient; real_incident_necessity treats incident authenticity as necessary; hybrid_occupation treats integrated multi-mechanism exercise as necessary. Each reading is ε-invariant (OQ-26): measuring one via another reading's observables yields a different ε and different type classification. The three stories are linked via network.affects_constraints to enable contamination-path and family-level analysis; they are SEPARATE constraints not alternative framings of a single constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(competence_occupation__simulation_sufficiency, moderate, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
