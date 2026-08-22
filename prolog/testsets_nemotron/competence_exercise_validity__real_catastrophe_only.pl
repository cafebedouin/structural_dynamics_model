% ============================================================================
% CONSTRAINT STORY: competence_exercise_validity__real_catastrophe_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_exercise_validity__real_catastrophe_only, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: competence_exercise_validity__real_catastrophe_only
 *   human_readable: Real Catastrophe as Sole Valid Competence Exercise
 *   domain: safety_engineering/organizational_learning/competence_retention
 *
 * SUMMARY:
 *   The constraint asserts that only genuine catastrophic failure — not
 *   drills, simulations, or near-misses — can validate whether an
 *   organization's safety competence is real. This reading treats simulation
 *   as theater: it creates the appearance of preparedness while the actual
 *   competence required for real crisis response atrophies untested. The
 *   arrangement coordinates a safety bureaucracy (compliance officers,
 *   simulation vendors, cost-conscious management) around a proxy-validation
 *   regime that extracts legitimacy and budget from the claim of readiness,
 *   while the real burden of unvalidated competence falls on frontline
 *   operators and the public. The claim/metric gap is deliberate: the
 *   constraint is CLAIMED as tangled_rope (genuine coordination of safety
 *   practice + asymmetric extraction from those who bear the risk) while the
 *   metrics describe rising extractiveness, increasing theater, and hardening
 *   enforcement of the simulation regime — the engine measures that
 *   divergence; do not reconcile the claim to the metrics.
 *
 * KEY AGENTS:
 *   - frontline_operators: Primary target (powerless/identity_locked) — bears untested competence risk
 *   - public_at_risk: Primary victim (powerless/trapped) — bears consequence of competence decay
 *   - simulation_vendors: Primary beneficiary (organized/arbitrage) — sells proxy validation
 *   - compliance_officers: Beneficiary / agenda_setter (institutional/constrained) — administers the simulation regime
 *   - senior_management_cost_centers: Beneficiary (institutional/mobile) — avoids real exercise costs
 *   - junior_engineers: Victim (moderate/identity_locked) — trained in simulation, never tested
 *   - safety_investigators: Observer (analytical/analytical) — sees the gap post-catastrophe
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_validity__real_catastrophe_only, 0.68).
domain_priors:suppression_score(competence_exercise_validity__real_catastrophe_only, 0.72).
domain_priors:theater_ratio(competence_exercise_validity__real_catastrophe_only, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_validity__real_catastrophe_only, extractiveness, 0.68).
narrative_ontology:constraint_metric(competence_exercise_validity__real_catastrophe_only, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(competence_exercise_validity__real_catastrophe_only, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_validity__real_catastrophe_only, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(competence_exercise_validity__real_catastrophe_only, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_validity__real_catastrophe_only, tangled_rope).
narrative_ontology:human_readable(competence_exercise_validity__real_catastrophe_only, "Real Catastrophe as Sole Valid Competence Exercise").
narrative_ontology:topic_domain(competence_exercise_validity__real_catastrophe_only, "safety_engineering/organizational_learning/competence_retention").

domain_priors:requires_active_enforcement(competence_exercise_validity__real_catastrophe_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_validity__real_catastrophe_only, '775a150e-dbd2-478c-9f4b-f08975c4c7a6').
narrative_ontology:cs_kernel_codification('775a150e-dbd2-478c-9f4b-f08975c4c7a6', distributed).
narrative_ontology:cs_authority_grounding('775a150e-dbd2-478c-9f4b-f08975c4c7a6', practice).
narrative_ontology:cs_interpretation_layer_present('775a150e-dbd2-478c-9f4b-f08975c4c7a6').
narrative_ontology:cs_reading_relation('775a150e-dbd2-478c-9f4b-f08975c4c7a6', competence_exercise_validity__simulation_as_proxy, forecloses).
narrative_ontology:cs_reading_relation('775a150e-dbd2-478c-9f4b-f08975c4c7a6', competence_exercise_validity__continuous_refresh_hybrid, influences).
narrative_ontology:cs_axiom('775a150e-dbd2-478c-9f4b-f08975c4c7a6', foundational, simulation_cannot_validate_catastrophe_competence).
narrative_ontology:cs_axiom_status(simulation_cannot_validate_catastrophe_competence, holdable).
narrative_ontology:cs_axiom_grounding('775a150e-dbd2-478c-9f4b-f08975c4c7a6', simulation_cannot_validate_catastrophe_competence, empirically_contingent).
narrative_ontology:cs_axiom('775a150e-dbd2-478c-9f4b-f08975c4c7a6', foundational, competence_decay_is_masked_by_simulation_success).
narrative_ontology:cs_axiom_status(competence_decay_is_masked_by_simulation_success, holdable).
narrative_ontology:cs_axiom_grounding('775a150e-dbd2-478c-9f4b-f08975c4c7a6', competence_decay_is_masked_by_simulation_success, empirically_contingent).
narrative_ontology:cs_reference_frame('775a150e-dbd2-478c-9f4b-f08975c4c7a6', pre_simulation_regime_safety_practice).
narrative_ontology:cs_drift_state('775a150e-dbd2-478c-9f4b-f08975c4c7a6', post_three_mile_island_challenger_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('775a150e-dbd2-478c-9f4b-f08975c4c7a6', '').
narrative_ontology:cs_kernel_id(competence_exercise_validity__real_catastrophe_only, competence_exercise_validity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_validity__real_catastrophe_only, simulation_vendors).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__real_catastrophe_only, compliance_officers).
narrative_ontology:constraint_beneficiary(competence_exercise_validity__real_catastrophe_only, senior_management_cost_centers).
narrative_ontology:constraint_victim(competence_exercise_validity__real_catastrophe_only, frontline_operators).
narrative_ontology:constraint_victim(competence_exercise_validity__real_catastrophe_only, public_at_risk).
narrative_ontology:constraint_victim(competence_exercise_validity__real_catastrophe_only, junior_engineers).
narrative_ontology:constraint_vindicates(competence_exercise_validity__real_catastrophe_only, catastrophe_necessity_doctrine).
narrative_ontology:constraint_vindicates(competence_exercise_validity__real_catastrophe_only, simulation_inadequacy_claim).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate the actual systems under routine conditions. Their competence for crisis response is never validated because real catastrophes are rare and unethical to engineer. They train in simulations that this reading declares insufficient. Professional identity is fused to the simulation-trained role — leaving means abandoning their career framework. If catastrophe occurs, they bear the consequence of unvalidated competence.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, frontline_operators, payer,
    powerless, biographical, identity_locked, regional).

% Depend on safety-critical systems (nuclear, aviation, chemical, medical) without consent or knowledge of the validation regime. No exit from systems they depend on. Bear the full consequence if competence decay masked by simulation theater results in catastrophe. No voice in the validation standard.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, public_at_risk, payer,
    powerless, biographical, trapped, regional).

% Sell simulation platforms, scenario libraries, and certification services to regulated industries. Revenue scales with regulatory mandates for simulation hours and fidelity. The 'only real catastrophe' reading is their market ceiling — if regulators accepted it, the simulation market would collapse. They lobby for simulation-as-proxy standards and fund research supporting drill validity.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, simulation_vendors, beneficiary,
    organized, generational, arbitrage, global).

% Administer the simulation mandate: set hour requirements, approve scenarios, certify completion. Their authority derives from the regime's measurability — simulation hours are auditable; competence is not. Career advancement tracks exercise compliance, not crisis outcomes (which are rare). They genuinely believe the regime improves safety, but the regime also constitutes their institutional relevance.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, compliance_officers, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(competence_exercise_validity__real_catastrophe_only, compliance_officers, beneficiary).

% Approve budgets for safety validation. Real catastrophe exercises are impossibly expensive, ethically fraught, and operationally disruptive. Simulation is affordable, schedulable, and produces audit trails. They benefit from the regime's cost predictability and the appearance of due diligence. Their exit is mobile — they rotate through industries where the same regime applies.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, senior_management_cost_centers, beneficiary,
    institutional, biographical, mobile, global).

% Trained entirely within the simulation regime. Their professional formation teaches that drill performance equals competence. They have never experienced a real catastrophe and the regime teaches them they never need to. Identity-locked because their professional self-concept is built on simulation mastery. If they later face a real event, the gap between drill and reality is theirs to bridge alone.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, junior_engineers, payer,
    moderate, biographical, identity_locked, regional).

% Investigate catastrophes after they occur. They see the gap between simulation records and actual operator performance under crisis conditions. Their reports document the validity gap but their recommendations feed back into the same simulation regime (more hours, higher fidelity). They neither collect from nor pay into the constraint — they observe its structural failure post-hoc.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, safety_investigators, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardizes safety practice across a fragmented industry: creates common procedures, shared vocabulary, measurable training metrics, and regulatory auditability. Solves the coordination problem of 'how do we know everyone is trained?' without requiring the impossible coordination of real catastrophes.
% TRANSFER_FUNCTION: Moves budget, regulatory attention, and career capital from frontline operations (where real competence lives) to simulation infrastructure (vendors, compliance, management). Moves risk from the simulation regime (which cannot fail because it never faces a real test) to frontline operators and the public (who bear the consequence when unvalidated competence meets real crisis).
% ABSENT_VOICES: The catastrophes that never happen — and the operators who would have faced them — are structurally absent. Their silence is the regime's evidence of success. Families of future victims are not in the room when simulation standards are set. Near-miss survivors who could testify to the simulation-reality gap are filtered out by the regime's classification of near-misses as 'validation successes.'
% DISAPPEARANCE_RATIONALE: If the 'only real catastrophe validates' constraint vanished, the simulation regime would lose its philosophical cover. Regulators would face the validation gap directly: either accept that competence cannot be validated without catastrophe (forcing a different safety architecture — redundancy, autonomy, graceful degradation) or adopt the simulation_as_proxy or continuous_refresh_hybrid readings. The safety bureaucracy, vendor market, and training pipelines would reorganize around whichever reading fills the vacuum.
% FOUNDING_PROBLEM: How to validate that safety-critical organizations can respond competently to catastrophic events, given that engineering real catastrophes for validation is ethically unacceptable and practically impossible.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by every major accident investigation board (Columbia Accident Investigation Board, Deepwater Horizon Commission, Fukushima Nuclear Accident Independent Investigation Commission) — all external to the simulation vendor and compliance beneficiary set. These bodies consistently find that simulation-trained organizations failed in ways their drills did not anticipate. The simulation regime's own beneficiaries (vendors, compliance officers) attest the problem is solved by higher-fidelity simulation — a self-interested corroboration.
narrative_ontology:disappearance_verdict(competence_exercise_validity__real_catastrophe_only, world_rearranges).
narrative_ontology:founding_problem_status(competence_exercise_validity__real_catastrophe_only, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_validity__real_catastrophe_only, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(competence_exercise_validity__real_catastrophe_only, 'none', 1).
narrative_ontology:epsilon_provenance(competence_exercise_validity__real_catastrophe_only, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_exercise_validity__real_catastrophe_only_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_exercise_validity__real_catastrophe_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_exercise_validity__real_catastrophe_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is substantial (0.68) because the simulation regime consumes resources, careers, and regulatory attention while delivering unvalidated readiness. Suppression is higher (0.72) because the constraint actively prevents real exercise — you cannot ethically engineer catastrophes, so the 'only real catastrophe' standard structurally blocks validation. Theater rises from 0.28 to 0.45 as simulation fidelity increases without closing the validity gap. The coordination function is real: the simulation regime standardizes procedures, creates shared vocabulary, and enables regulatory auditing — but the extraction is asymmetric: vendors and compliance structures profit, frontline operators and the public bear the unvalidated risk. Active enforcement is required: regulations mandate simulation hours, certifications require drill completion, careers advance on exercise participation.
 *
 * PERSPECTIVAL GAP:
 *   From the compliance/simulation seat, the constraint is a rope: it coordinates safety practice across a fragmented industry. From the frontline operator seat, it is a snare: it substitutes theater for the only thing that validates competence. From the public seat, it is a mountain presented as a rope: the 'no catastrophe yet' record looks like a natural safety property but is actually luck wearing a simulation uniform. The engine computes this divergence from the structural data — the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Simulation vendors and compliance officers sit near the beneficiary end (d ~ 0.15-0.25): they collect revenue, authority, and career advancement from the regime. Senior management sits at low d (~0.2): they avoid the cost of real exercises. Frontline operators and junior engineers sit near the target end (d ~ 0.8-0.9): they inherit the untested competence burden, with identity-locked exit (professional identity fused to the simulation-trained role). The public is trapped (d ~ 0.95): no exit from systems they depend on. Safety investigators are analytical (d = 0.5): they observe the structure without collecting or paying.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — 'how to validate safety competence without catastrophes' — is live (catastrophes are still unacceptable). But the reading's answer — 'only catastrophe validates, so we simulate and call it validation' — is a mandatrophy trap: the mandate (validate competence) has been replaced by the proxy (run simulations). The constraint persists because the proxy is measurable, fundable, and career-advancing, while the real mandate remains unsolved. Classification as tangled_rope captures this: genuine coordination (standardized practice) + asymmetric extraction (risk transferred to those who cannot audit the proxy).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the constraint ''only real catastrophe exercises competence'' a reading of the contested kernel competence_exercise_validity, distinct from sibling readings simulation_as_proxy and continuous_refresh_hybrid?',
    'Structural comparison of beneficiary/victim sets, enforcement mechanisms, and metric profiles across the three readings. If each reading yields a stable, distinct ε and classification, the decomposition holds.',
    'If the readings collapse to one constraint, the kernel frame is spurious; if they remain distinct, the kernel structure is validated and the family linkage in network.affects_constraints is warranted.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the competence_exercise_validity kernel cleanly decomposes into three structurally distinct constraints.').

omega_variable(
    catastrophe_vs_simulation_boundary,
    'Where is the boundary between ''insufficient simulation'' and ''valid partial exercise'' — does any drill structure approach real catastrophe validity, or is the gap categorical?',
    'Empirical analysis of near-miss events and high-fidelity drill outcomes: if operators who experience near-misses show measurably better crisis response than simulation-only cohorts, the gap is gradient; if no drill ever closes the gap, it is categorical.',
    'A categorical gap supports the real_catastrophe_only reading as a mountain-like structural limit; a gradient gap reclassifies it as a tangled_rope with coordination function at the high-fidelity end.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(catastrophe_vs_simulation_boundary, empirical, 'Whether the simulation-catastrophe validity gap is categorical or gradient.').

omega_variable(
    extraction_masking_by_fortune,
    'Does the absence of catastrophe in a period function as a false validation signal that masks competence decay — i.e., does luck suppress the reading''s own falsification?',
    'Longitudinal tracking of organizations with clean safety records that then suffer catastrophic failures: if post-hoc analysis reveals competence decay that was invisible during the quiet period, the masking is real.',
    'If masking occurs, the constraint''s suppression metric understates its true suppressive force — the constraint actively prevents its own validation by making ''no catastrophe'' look like ''competence intact.''',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_masking_by_fortune, empirical, 'Whether safety-record quiescence masks competence decay, making the constraint self-concealing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_validity__real_catastrophe_only, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cev_real_cat_only_tr_t0, competence_exercise_validity__real_catastrophe_only, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(cev_real_cat_only_tr_t0, observed).
narrative_ontology:measurement(cev_real_cat_only_tr_t10, competence_exercise_validity__real_catastrophe_only, theater_ratio, 10, 0.33).
narrative_ontology:measurement_basis(cev_real_cat_only_tr_t10, observed).
narrative_ontology:measurement(cev_real_cat_only_tr_t20, competence_exercise_validity__real_catastrophe_only, theater_ratio, 20, 0.38).
narrative_ontology:measurement_basis(cev_real_cat_only_tr_t20, observed).
narrative_ontology:measurement(cev_real_cat_only_tr_t30, competence_exercise_validity__real_catastrophe_only, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(cev_real_cat_only_tr_t30, observed).
narrative_ontology:measurement(cev_real_cat_only_tr_t40, competence_exercise_validity__real_catastrophe_only, theater_ratio, 40, 0.45).
narrative_ontology:measurement_basis(cev_real_cat_only_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(cev_real_cat_only_be_t0, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(cev_real_cat_only_be_t0, observed).
narrative_ontology:measurement(cev_real_cat_only_be_t10, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 10, 0.58).
narrative_ontology:measurement_basis(cev_real_cat_only_be_t10, observed).
narrative_ontology:measurement(cev_real_cat_only_be_t20, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 20, 0.63).
narrative_ontology:measurement_basis(cev_real_cat_only_be_t20, observed).
narrative_ontology:measurement(cev_real_cat_only_be_t30, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 30, 0.66).
narrative_ontology:measurement_basis(cev_real_cat_only_be_t30, observed).
narrative_ontology:measurement(cev_real_cat_only_be_t40, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(cev_real_cat_only_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(cev_real_cat_only_su_t0, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(cev_real_cat_only_su_t0, observed).
narrative_ontology:measurement(cev_real_cat_only_su_t10, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 10, 0.61).
narrative_ontology:measurement_basis(cev_real_cat_only_su_t10, observed).
narrative_ontology:measurement(cev_real_cat_only_su_t20, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 20, 0.66).
narrative_ontology:measurement_basis(cev_real_cat_only_su_t20, observed).
narrative_ontology:measurement(cev_real_cat_only_su_t30, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 30, 0.7).
narrative_ontology:measurement_basis(cev_real_cat_only_su_t30, observed).
narrative_ontology:measurement(cev_real_cat_only_su_t40, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(cev_real_cat_only_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_validity__real_catastrophe_only, enforcement_mechanism).
narrative_ontology:affects_constraint(competence_exercise_validity__real_catastrophe_only, competence_exercise_validity__simulation_as_proxy).
narrative_ontology:affects_constraint(competence_exercise_validity__real_catastrophe_only, competence_exercise_validity__continuous_refresh_hybrid).

% DUAL FORMULATION NOTE:
% The competence_exercise_validity kernel decomposes into three readings with distinct beneficiary/victim structures and ε values. This reading (real_catastrophe_only) has the highest extraction (0.68) and suppression (0.72) because it structurally blocks its own validation. The simulation_as_proxy reading has lower extraction (~0.35) but higher theater (~0.6) as it legitimizes the proxy. The continuous_refresh_hybrid reading sits between, with a coordination function at the high-fidelity drill end. All three share the same referent (competence validation) but differ on what counts as exercise.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(competence_exercise_validity__real_catastrophe_only, organized, 0.2).
constraint_indexing:directionality_override(competence_exercise_validity__real_catastrophe_only, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
