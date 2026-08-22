% ============================================================================
% CONSTRAINT STORY: competence_exercise_validity__real_catastrophe_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
 *   domain: safety/organizational
 *
 * SUMMARY:
 *   The real_catastrophe_only reading of the competence_exercise_validity
 *   kernel makes an epistemic claim: true competence can only be validated
 *   through real operational performance under actual catastrophic
 *   conditions. Simulation, drills, and competence frameworks based on
 *   synthetic scenarios are declared theoretically invalid as proof of
 *   readiness. This reading operates as a snare constraint: it benefits
 *   institutional risk-minimizers by providing an epistemic cover story for
 *   deferring expensive simulation and drill programs, while suppressing the
 *   authority of competence validators and rendering operational personnel
 *   perpetually untested. The constraint masks extraction (budget relief,
 *   accountability deferral) as epistemology. The cyclical dynamics are
 *   pronounced: periods of stable operations reinforce the claim that 'we
 *   haven't been tested yet, so simulation is unnecessary,' while periods
 *   following actual catastrophe produce brief investment in drills before
 *   the real_catastrophe_only reading reasserts that simulation cannot
 *   validate. The measurement series track the rising theater ratio —
 *   increasing proportion of institutional discourse devoted to
 *   epistemological assertion ('only reality tests') versus actual
 *   competence-validation activities.
 *
 * KEY AGENTS:
 *   - operational_personnel: Powerless, trapped; their competence remains perpetually unvalidated until catastrophe; they bear the risk of untested performance.
 *   - institutional_risk_minimizers: Institutional power, arbitrage exit; benefit from deferred simulation investment; set the standard for what counts as valid competence exercise.
 *   - competence_validators: Organized, constrained; their professional authority is suppressed by the epistemic claim; rendered unable to certify readiness.
 *   - system_users: Powerless, trapped; bear the risk that personnel are incompetent when they encounter the operational system.
 *   - simulation_practitioners: Moderate power, constrained exit; their professional work is delegitimized regardless of fidelity or design.
 *   - competing_readiness_frameworks: Excluded from legitimate discourse; cannot contest the standard without being dismissed as seeking safety shortcuts.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_exercise_validity__real_catastrophe_only, 0.68).
domain_priors:suppression_score(competence_exercise_validity__real_catastrophe_only, 0.71).
domain_priors:theater_ratio(competence_exercise_validity__real_catastrophe_only, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_exercise_validity__real_catastrophe_only, extractiveness, 0.68).
narrative_ontology:constraint_metric(competence_exercise_validity__real_catastrophe_only, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(competence_exercise_validity__real_catastrophe_only, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_exercise_validity__real_catastrophe_only, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(competence_exercise_validity__real_catastrophe_only, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_exercise_validity__real_catastrophe_only, snare).
narrative_ontology:human_readable(competence_exercise_validity__real_catastrophe_only, "Real Catastrophe as Sole Valid Competence Exercise").
narrative_ontology:topic_domain(competence_exercise_validity__real_catastrophe_only, "safety/organizational").

domain_priors:requires_active_enforcement(competence_exercise_validity__real_catastrophe_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_exercise_validity__real_catastrophe_only, '74788999-0703-4274-95da-ee0f158106d9').
narrative_ontology:cs_kernel_codification('74788999-0703-4274-95da-ee0f158106d9', distributed).
narrative_ontology:cs_authority_grounding('74788999-0703-4274-95da-ee0f158106d9', extraction).
narrative_ontology:cs_reading_relation('74788999-0703-4274-95da-ee0f158106d9', competence_exercise_validity__simulation_as_proxy, coexists_with).
narrative_ontology:cs_reading_relation('74788999-0703-4274-95da-ee0f158106d9', competence_exercise_validity__continuous_refresh_hybrid, influences).
narrative_ontology:cs_axiom('74788999-0703-4274-95da-ee0f158106d9', foundational, only_real_catastrophe_validates_competence).
narrative_ontology:cs_axiom_status(only_real_catastrophe_validates_competence, holdable).
narrative_ontology:cs_axiom_grounding('74788999-0703-4274-95da-ee0f158106d9', only_real_catastrophe_validates_competence, empirically_contingent).
narrative_ontology:cs_axiom('74788999-0703-4274-95da-ee0f158106d9', foundational, simulation_theoretically_invalid_as_proof).
narrative_ontology:cs_axiom_status(simulation_theoretically_invalid_as_proof, holdable).
narrative_ontology:cs_axiom_grounding('74788999-0703-4274-95da-ee0f158106d9', simulation_theoretically_invalid_as_proof, empirically_contingent).
narrative_ontology:cs_reference_frame('74788999-0703-4274-95da-ee0f158106d9', simulation_as_theoretical_substitute).
narrative_ontology:cs_drift_state('74788999-0703-4274-95da-ee0f158106d9', contemporary_post_catastrophe_research_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('74788999-0703-4274-95da-ee0f158106d9', '').
narrative_ontology:cs_kernel_id(competence_exercise_validity__real_catastrophe_only, competence_exercise_validity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_exercise_validity__real_catastrophe_only, institutional_risk_minimizers).
narrative_ontology:constraint_victim(competence_exercise_validity__real_catastrophe_only, operational_personnel).
narrative_ontology:constraint_victim(competence_exercise_validity__real_catastrophe_only, competence_validators).
narrative_ontology:constraint_victim(competence_exercise_validity__real_catastrophe_only, system_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(competence_exercise_validity__real_catastrophe_only, simulation_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold the operational roles whose competence is under judgment. They invest time in simulation and drills, which the real_catastrophe_only reading declares insufficient and theoretically wasted. When an actual emergency arrives unpredictably, they must perform under conditions simulation could never fully replicate — stress, uncertainty, cascade failures, degraded information. Their competence is treated as unvalidated until real catastrophe tests it. They cannot exit their positions without abandoning their careers; they cannot force real catastrophe to occur on a schedule to prove their readiness.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, operational_personnel, payer,
    powerless, biographical, trapped, global).

% Administer safety policy and set the standard for what counts as valid competence exercise. The real_catastrophe_only reading places institutional authority in a position to defer costly simulation and drill programs by declaring them theoretically invalid — only actual catastrophe validates competence. This relieves budget pressure, reduces schedule disruption, and shifts accountability: if the system fails during a real event, the failure can be attributed to insufficient real-world testing rather than insufficient preparation. They collect the budgetary and operational benefit of minimizing simulation investment.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, institutional_risk_minimizers, agenda_setter,
    institutional, generational, arbitrage, global).

% Professional bodies, accreditation organizations, and safety auditors tasked with certifying that personnel are competent. The real_catastrophe_only reading undermines their authority: it declares their simulation-based validation frameworks theoretically invalid as proof of competence. They are rendered structurally unable to certify readiness without waiting for actual catastrophe, which is infeasible as a validation method. Their professional judgment is suppressed by the epistemic claim that only reality tests competence.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, competence_validators, payer,
    organized, biographical, constrained, global).

% The public, patients, travelers, workers, or other stakeholders whose safety depends on operational personnel performing competently. Under the real_catastrophe_only reading, they bear the risk that personnel remain untested and possibly incompetent until catastrophe arrives. They have no say in whether simulation is deployed; they cannot verify that personnel are ready; they encounter the system when it fails.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, system_users, payer,
    powerless, immediate, trapped, global).

% Engineers, instructors, and technicians who design and operate simulation systems. The real_catastrophe_only reading delegitimizes their work as fundamentally unable to validate competence, regardless of fidelity or design. Their professional judgment about what simulation can usefully reveal is suppressed. They continue operating simulations because organizations do so, but under a mandate that their output is theoretically non-validating.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, simulation_practitioners, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(competence_exercise_validity__real_catastrophe_only, simulation_practitioners, observer).

% The simulation_as_proxy and continuous_refresh_hybrid readings represent competing epistemic frameworks for what counts as valid competence exercise. They would advocate for crediting simulation and continuous drill as valid, complementary, or necessary methods. The real_catastrophe_only reading excludes them from legitimate discourse by claiming real catastrophe is the sole valid epistemic arbiter. They cannot contest the standard without being dismissed as seeking shortcuts on safety.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, competing_readiness_frameworks, excluded,
    organized, generational, trapped, global).

% Academic researchers, independent safety analysts, and audit bodies studying what actually happens when systems fail. They observe the constraint's operation and can measure whether competence decay occurs in systems operating under the real_catastrophe_only reading, whether simulation-validated personnel actually perform better than untested personnel when real events occur, and whether this constraint produces safer or less safe outcomes.
narrative_ontology:constraint_stakeholder(competence_exercise_validity__real_catastrophe_only, catastrophe_driven_organization, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None. The constraint does not solve a coordination problem. It makes an epistemic claim about what counts as evidence of competence. Under the real_catastrophe_only reading, institutions coordinate on deferring simulation investment by treating it as theoretically invalid.
% TRANSFER_FUNCTION: Moves budgetary and scheduling resources away from simulation programs and toward other institutional priorities. Moves accountability away from institutional risk management and toward operational personnel, who must prove readiness only after real catastrophe has occurred (or not at all, if no catastrophe tests them).
% ABSENT_VOICES: Operational personnel, once catastrophe has occurred and failed to prepare them, cannot retroactively dispute the constraint. Simulation practitioners whose work is delegitimized have no mechanism to contest the epistemic claim. System users who bear the risk of untested personnel have no say in setting the validation standard. Safety researchers who gather data about simulation efficacy are epistemically silenced by a claim that only reality validates.
% DISAPPEARANCE_RATIONALE: If the real_catastrophe_only reading disappeared and simulation-based validation returned to legitimacy, institutions would reinvest in drill programs, operational personnel would receive regular competence affirmation, and validation bodies would regain authority. Some catastrophes would still occur (simulation is not perfect), but the failure mode would shift — from 'untested incompetence' to 'validated personnel encountered an unforeseeable cascade.' The constraint's disappearance would redistribute risk and accountability. Beneficiaries (institutional risk minimizers) would lose budgetary relief; payers would gain investment in preparation. The verdict is contested because one reading asserts this change would make systems safer; the competing readings assert simulation-based preparation is superior; institutional risk minimizers assert the change would waste resources on false assurance.
% FOUNDING_PROBLEM: Simulation and drills can become theater — expensive, elaborate exercises that create the appearance of readiness without delivering real competence. Personnel can become skilled at navigating known scenarios while remaining fragile when confronted with novel conditions. Organizations can accumulate certifications while actual readiness decays.
% FOUNDING_PROBLEM_CORROBORATION: Academic research in emergency management, military operations, healthcare, and aviation consistently demonstrates that pre-catastrophe simulation and drill training measurably improves actual response performance compared to no preparation. Post-disaster investigations document that trained personnel respond better than untrained personnel, even when training was simulation-based. Meta-analyses of catastrophe response show that the quality of simulation (fidelity, frequency, feedback) correlates positively with actual performance. The founding problem — that simulation can be poor and theater-like — is solved by improving simulation quality and integration, not by declaring simulation inherently invalid. The institutional beneficiaries (risk minimizers) do not cite this corroboration; instead they assert that only catastrophe data counts as evidence, which is a different claim and amounts to changing the evidentiary standard rather than refuting the founding problem's solution.
narrative_ontology:disappearance_verdict(competence_exercise_validity__real_catastrophe_only, contested).
narrative_ontology:founding_problem_status(competence_exercise_validity__real_catastrophe_only, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_exercise_validity__real_catastrophe_only, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(competence_exercise_validity__real_catastrophe_only, 'none', 1).
narrative_ontology:epsilon_provenance(competence_exercise_validity__real_catastrophe_only, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.68) because the constraint systematically defers investments that would improve safety and competence validation, redistributing resources away from preparation and toward other institutional uses. Suppression is high (0.71) because the constraint's persistence depends on maintaining the epistemic claim that 'only reality validates' — any empirical or theoretical challenge to that claim (simulation efficacy studies, competence-decay research, post-catastrophe analyses showing prepared personnel outperform unprepared) must be actively suppressed or reframed as seeking safety shortcuts. Theater ratio is very high (0.62) because the constraint's primary function is epistemic assertion rather than actual safety management — the institutional discourse consists largely of asserting that simulation is theoretically invalid, while the actual operational system may or may not have functional competence-validation mechanisms. Accessibility_collapse is high (0.78) because once the real_catastrophe_only reading is accepted as the epistemic standard, alternatives become logically incoherent within that framework — you cannot coherently argue for 'simulation-based validation' within a framework that has declared simulation categorically invalid. Resistance is moderate (0.59) because operational personnel, safety researchers, and competing institutional frameworks actively resist the claim with data and argument, but the constraint persists because institutional risk-minimizers have incentive and authority to maintain it.
 *
 * PERSPECTIVAL GAP:
 *   From the institutional agenda_setter's seat, the real_catastrophe_only reading appears as epistemic honesty: 'simulation is inherently limited, so we should not claim it validates competence.' From the operational personnel seat, the same reading appears as a trap that allows the institution to avoid responsibility for preparation while treating unpreparedness as unavoidable natural fact. From the simulation practitioners' seat, it appears as a dismissal of their work and expertise. From the competence validators' seat, it appears as institutional authority usurping their professional standards. The engine's per-seat classification should capture this divergence: the agenda_setter computes a coordination or rope-type classification (they frame it as epistemological honesty); the payer seats compute snare or pure-extraction classifications (they experience it as suppression of their capacity and deferral of preparation). The claim/metric independence ensures the divergence registers: the JSON declares a snare classification while the institutional authority frames it as coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   The institutional agenda_setter experiences this constraint with directionality near 0.0 (beneficiary end): they set the standard, avoid simulation costs, and defer accountability for competence validation. Operational personnel experience directionality near 1.0 (target end): they are trapped in untested positions, cannot exit, bear the risk of failure, and cannot force catastrophe to occur on a validation schedule. Competence_validators experience directionality around 0.8 (near target): their professional authority is suppressed, their validation frameworks are declared invalid, they have constrained exit. System_users experience directionality around 0.85: they bear the risk but have no agency in the constraint's operation. Simulation practitioners experience directionality around 0.75: their work is delegitimized. Competing frameworks are excluded (trapped at 1.0 by definition of exclusion). The asymmetry is structural: one institutional seat benefits from the epistemic claim while all other seats bear costs and suppression.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — 'simulation can become theater' — is genuine but marginal. Most simulation failures are not that simulation is inherently invalid, but that simulation is done poorly, with low fidelity, or without feedback integration. The real_catastrophe_only reading solves the problem by declaring all simulation invalid, which is epistemic overreach. It preserves the mandate ('ensure competence') in name only: by making competence validation impossible until catastrophe arrives, the constraint guarantees that competence remains unvalidated and institutional accountability is permanently deferred. The founding problem's death is evident: robust simulation programs demonstrably improve real-world performance (documented in emergency management, military operations, healthcare, aviation). The constraint persists not because the founding problem is live, but because institutional risk-minimizers benefit from its persistence and have the authority to maintain the epistemic claim that 'only reality validates.' The mandatrophy verdict is clear — the founding problem is dead but the constraint persists for extractive reasons.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    epistemic_claim_validity,
    'Is the claim that ''only real catastrophe exercises competence'' itself a description of how competence validation actually works, or a normative assertion about what should count as valid evidence?',
    'Empirical comparison: measure competence under two cohorts — one trained exclusively via simulation and drill, one without prior simulation — and observe actual performance during equivalent real incidents. Meta-analysis of post-catastrophe investigations documenting whether pre-catastrophe simulation training correlates with better outcomes.',
    'If the claim is empirically false (simulation-trained personnel perform measurably better), the constraint''s epistemic foundation collapses and it reclassifies from snare to pure extraction mechanism. If simulation training shows no benefit over untrained personnel, the claim gains empirical support.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(epistemic_claim_validity, empirical, 'Whether the real_catastrophe_only reading accurately describes how competence is actually validated or is a post-hoc narrative masking institutional cost-shifting.').

omega_variable(
    kernel_reading_decomposition,
    'Is the ''competence_exercise_validity'' kernel a real epistemic question about what methods reveal true competence, or a cover for three distinct institutional power plays?',
    'Discourse analysis of how each reading is deployed in institutional contexts: whether the reading is chosen based on epistemic grounds (evidence about simulation efficacy) or based on institutional incentives (budget relief, accountability deferral). Track which reading is invoked when catastrophe occurs vs. between events.',
    'If readings are deployed strategically regardless of evidence, the kernel itself may be a manufactured contest that makes power-shifting sound like epistemology. The real_catastrophe_only reading would be revealed as a tool for institutional cost externalization rather than a genuine truth claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_decomposition, conceptual, 'Whether the competing readings represent genuine epistemic disagreement or strategic framing of institutional interests as epistemology.').

omega_variable(
    temporal_structure_of_catastrophe,
    'Is it coherent to treat catastrophe as a planned validation event that occurs on a schedule, or is the real_catastrophe_only reading actually claiming that competence cannot be validated until unpredictable disaster arrives?',
    'Clarify what the reading means by ''real catastrophe'': does it mean (a) any real-world operational test, no matter how minor; (b) large-scale disaster events that cannot be scheduled; (c) personnel-specific failure moments? If (b), the reading makes competence validation contingent on randomness — a logical incoherence as a validation method. If (a), simulation that uses real-world operational data would qualify and the reading collapses.',
    'If the reading requires actual large-scale disaster to validate competence, it is logically incoherent as a validation methodology (competence cannot depend on random catastrophe occurring). This would reveal the constraint as pure institutional extraction masquerading as epistemology. If the reading means ''real-world operational data rather than synthetic scenario-based data,'' the distinction from continuous_refresh_hybrid becomes unclear.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(temporal_structure_of_catastrophe, conceptual, 'The logical coherence of treating unpredictable catastrophe as a validation mechanism vs. the reading''s actual institutional effect of deferring all validation.').

omega_variable(
    competence_decay_measurement,
    'Does competence actually decay over time without exercise? Or do people retain learned skills indefinitely, making simulation unnecessary for maintenance?',
    'Longitudinal studies of personnel competence over years: measure actual performance on competence tests, simulations, and real operational tasks at multiple time points for personnel with and without intervening simulation or drill. Track skill retention in extended periods of no-incident operation.',
    'If competence decays without exercise and real catastrophe is rare, operational personnel will be incompetent when catastrophe arrives — supporting the real_catastrophe_only reading as a description of actual risk. If competence is retained indefinitely or improves under simulation-free conditions, the reading loses even descriptive validity. The constraint''s harm would be clear.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(competence_decay_measurement, empirical, 'Whether the real_catastrophe_only reading describes an actual risk of competence decay or exaggerates the risk to justify institutional policy.').

omega_variable(
    simulation_fidelity_threshold,
    'Is there a fidelity threshold above which simulation validly exercises competence under conditions sufficiently close to real-world stress and novelty, or is simulation categorically incapable of capturing the stakes and uncertainty of actual disaster?',
    'Measure simulation fidelity dimensions (stress inoculation, novel-scenario exposure, decision-making under uncertainty, cascade-failure feedback) and correlate with actual post-event performance. Test whether ultra-high-fidelity simulation (VR-based, full-cascade training) produces different outcomes than tabletop exercises.',
    'If a fidelity threshold exists above which simulation-trained personnel perform as well as catastrophe-taught personnel, the real_catastrophe_only reading is empirically wrong — high-fidelity simulation would qualify as valid competence exercise. If no threshold exists and simulation always underperforms, the reading gains support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_fidelity_threshold, empirical, 'Whether simulation technology can approach real-catastrophe conditions closely enough to count as valid competence exercise, or if the gap is categorical.').

omega_variable(
    sibling_reading_contention_location,
    'Where exactly do the three readings of the competence_exercise_validity kernel diverge? Are they disagreeing about epistemology (what counts as evidence), about empirical facts (what simulation can achieve), or about institutional incentives (who benefits from each standard)?',
    'Decompose each reading''s claims: (1) What are the factual/empirical claims each reading makes? (2) What are the normative claims about what should count as valid? (3) What institutional outcomes does each reading produce? Chart which claims are shared across readings and which are genuinely in conflict.',
    'If the readings disagree only on empirics (simulation efficacy) and share the same normative standard, the contest is resolvable by evidence — measure simulation outcomes. If they disagree on what counts as valid evidence itself, the contest is at the framework level and requires axiom-level resolution. If they diverge on institutional incentives, the epistemic framing is secondary to power and resource distribution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_contention_location, conceptual, 'The structural location of disagreement between the three kernel readings — empirical, normative, or institutional.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_exercise_validity__real_catastrophe_only, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_exercise_validity__real_catastrophe_only, theater_ratio, 0, 0.45).
narrative_ontology:measurement_basis(comp_tr_t0, observed).
narrative_ontology:measurement(comp_tr_t5, competence_exercise_validity__real_catastrophe_only, theater_ratio, 5, 0.49).
narrative_ontology:measurement_basis(comp_tr_t5, observed).
narrative_ontology:measurement(comp_tr_t10, competence_exercise_validity__real_catastrophe_only, theater_ratio, 10, 0.53).
narrative_ontology:measurement_basis(comp_tr_t10, observed).
narrative_ontology:measurement(comp_tr_t15, competence_exercise_validity__real_catastrophe_only, theater_ratio, 15, 0.57).
narrative_ontology:measurement_basis(comp_tr_t15, observed).
narrative_ontology:measurement(comp_tr_t25, competence_exercise_validity__real_catastrophe_only, theater_ratio, 25, 0.6).
narrative_ontology:measurement_basis(comp_tr_t25, observed).
narrative_ontology:measurement(comp_tr_t40, competence_exercise_validity__real_catastrophe_only, theater_ratio, 40, 0.62).
narrative_ontology:measurement_basis(comp_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(comp_be_t0, observed).
narrative_ontology:measurement(comp_be_t5, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 5, 0.56).
narrative_ontology:measurement_basis(comp_be_t5, observed).
narrative_ontology:measurement(comp_be_t10, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 10, 0.61).
narrative_ontology:measurement_basis(comp_be_t10, observed).
narrative_ontology:measurement(comp_be_t15, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 15, 0.64).
narrative_ontology:measurement_basis(comp_be_t15, observed).
narrative_ontology:measurement(comp_be_t25, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 25, 0.67).
narrative_ontology:measurement_basis(comp_be_t25, observed).
narrative_ontology:measurement(comp_be_t40, competence_exercise_validity__real_catastrophe_only, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(comp_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(comp_su_t0, observed).
narrative_ontology:measurement(comp_su_t5, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 5, 0.62).
narrative_ontology:measurement_basis(comp_su_t5, observed).
narrative_ontology:measurement(comp_su_t10, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 10, 0.65).
narrative_ontology:measurement_basis(comp_su_t10, observed).
narrative_ontology:measurement(comp_su_t15, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 15, 0.68).
narrative_ontology:measurement_basis(comp_su_t15, observed).
narrative_ontology:measurement(comp_su_t25, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 25, 0.7).
narrative_ontology:measurement_basis(comp_su_t25, observed).
narrative_ontology:measurement(comp_su_t40, competence_exercise_validity__real_catastrophe_only, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(comp_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_exercise_validity__real_catastrophe_only, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(competence_exercise_validity__real_catastrophe_only, 0.25).
narrative_ontology:affects_constraint(competence_exercise_validity__real_catastrophe_only, competence_exercise_validity__simulation_as_proxy).
narrative_ontology:affects_constraint(competence_exercise_validity__real_catastrophe_only, competence_exercise_validity__continuous_refresh_hybrid).

% DUAL FORMULATION NOTE:
% The competence_exercise_validity kernel decomposes into three structurally distinct constraints, each instantiating one reading of what counts as valid competence exercise. This constraint (real_catastrophe_only) claims simulation is categorically invalid; simulation_as_proxy claims simulation is sufficient; continuous_refresh_hybrid claims simulation is necessary but not sufficient. They share a referent (operational competence) but have radically different ε values and victim/beneficiary structures. All three are linked; the real_catastrophe_only reading exerts downstream institutional pressure on the other two by establishing the epistemic standard that simulation cannot be trusted.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(competence_exercise_validity__real_catastrophe_only, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
