% ============================================================================
% CONSTRAINT STORY: exercise_as_competence_maintenance__lived_catastrophe_necessity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: exercise_as_competence_maintenance__lived_catastrophe_necessity_reading
 *   human_readable: Lived Catastrophe Necessity for Competence Maintenance
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   This constraint instantiates the lived-catastrophe-necessity reading of
 *   the contested kernel: exercise_as_competence_maintenance. The reading
 *   asserts that only actual catastrophe exercises the competence kernel;
 *   simulation is rehearsal but not the thing itself; competence atrophies
 *   without real-stakes activation. Under this reading, safety leadership and
 *   exercise vendors benefit from the doctrine that simulation alone is
 *   insufficient—it justifies their authority, maintains demand for their
 *   services, and naturalizes the risk-bearing of exposed personnel and the
 *   public as inevitable. Exposed operators and the public are the payers:
 *   they carry uncertainty about readiness and the externalized risk of
 *   untested competence. The constraint is claimed as Tangled Rope because it
 *   coordinates expectations around catastrophe-necessity while extracting
 *   risk from those most exposed to its consequences. The measurement series
 *   shows extractiveness rising from 0.58 to 0.71 as the doctrine
 *   consolidates in institutional practice, and theater_ratio rising from
 *   0.38 to 0.52 as exercise regimes become increasingly performative
 *   relative to genuine competence validation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0.71).
domain_priors:suppression_score(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0.68).
domain_priors:theater_ratio(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, accessibility_collapse, 0.73).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, tangled_rope).
narrative_ontology:human_readable(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, "Lived Catastrophe Necessity for Competence Maintenance").
narrative_ontology:topic_domain(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, '44263b01-56af-4887-96fe-8132fb956b48').
narrative_ontology:cs_kernel_codification('44263b01-56af-4887-96fe-8132fb956b48', implicit).
narrative_ontology:cs_authority_grounding('44263b01-56af-4887-96fe-8132fb956b48', extraction).
narrative_ontology:cs_interpretation_layer_present('44263b01-56af-4887-96fe-8132fb956b48').
narrative_ontology:cs_reading_relation('44263b01-56af-4887-96fe-8132fb956b48', exercise_as_competence_maintenance__simulation_sufficiency_reading, forecloses).
narrative_ontology:cs_reading_relation('44263b01-56af-4887-96fe-8132fb956b48', exercise_as_competence_maintenance__hybrid_decay_reading, influences).
narrative_ontology:cs_axiom('44263b01-56af-4887-96fe-8132fb956b48', foundational, real_stakes_fundamental_incompleteness).
narrative_ontology:cs_axiom_status(real_stakes_fundamental_incompleteness, holdable).
narrative_ontology:cs_axiom_grounding('44263b01-56af-4887-96fe-8132fb956b48', real_stakes_fundamental_incompleteness, empirically_contingent).
narrative_ontology:cs_axiom('44263b01-56af-4887-96fe-8132fb956b48', foundational, catastrophe_necessity_doctrine).
narrative_ontology:cs_axiom_status(catastrophe_necessity_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('44263b01-56af-4887-96fe-8132fb956b48', catastrophe_necessity_doctrine, deontological).
narrative_ontology:cs_reference_frame('44263b01-56af-4887-96fe-8132fb956b48', catastrophe_as_competence_validator).
narrative_ontology:cs_drift_state('44263b01-56af-4887-96fe-8132fb956b48', contemporary_simulation_fidelity_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('44263b01-56af-4887-96fe-8132fb956b48', '').
narrative_ontology:cs_kernel_id(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, exercise_as_competence_maintenance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, safety_leadership_class).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, exercise_apparatus_vendors).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, exposed_personnel).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, public_at_risk).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets training standards, administers exercise regimes, certifies operator competence, and defines what counts as valid competence testing. Under this reading, they maintain authority by insisting that only real stakes test true competence, which justifies their judgment in crisis moments and naturalizes simulation as subsidiary. They resist system redesigns that would reduce catastrophe-dependence because such redesigns would transfer authority to measurement systems and undermine their epistemic gatekeeping role.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, safety_leadership_class, agenda_setter,
    institutional, generational, arbitrage, global).

% Supply simulation equipment, exercise infrastructure, and procedural training systems to safety organizations. Under this reading, they benefit from the persistent gap between simulation and real stakes: the doctrine suppresses demand for high-fidelity simulation that might reduce dependence on real-event validation, maintaining dual-market dynamics (they sell both simulation infrastructure and the narrative of its insufficiency). Organizations must maintain both simulation AND real-event response preparation because doctrine says simulation alone is not enough.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, exercise_apparatus_vendors, beneficiary,
    institutional, biographical, mobile, global).

% Flight crews, power plant operators, surgeons, emergency responders, and other operators whose decisions and actions directly affect public safety. They are told their competence is untested until real catastrophe occurs, which means they operate under a regime of uncertainty about their own readiness. Their identity is fused to their professional role—leaving the profession is the only exit—so they must accept this uncertainty. They bear the psychological and professional risk of being untested until failure might occur.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, exposed_personnel, payer,
    powerless, biographical, identity_locked, global).

% Citizens and persons operating in systems dependent on competent operators: passengers in aircraft, residents near power infrastructure, patients receiving medical care, people requiring emergency response. They are exposed to operators who are declared incompletely trained by doctrine until real catastrophe tests them. They cannot exit this exposure without abandoning the infrastructure modern life depends on—their identity as urban, mobile, medically-dependent persons fuses them to the risk.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, public_at_risk, payer,
    powerless, biographical, identity_locked, global).

% Conduct research on whether high-fidelity simulation can close the exercise gap between rehearsal and real competence. Under this reading, they are structurally excluded from the conversation that determines safety policy: their findings contradicting the necessity doctrine are dismissed as showing 'simulation effects' rather than genuine competence. Their research is published and cited but does not change the foundational doctrine because the doctrine's truth is pre-judged.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, simulation_effectiveness_researchers, excluded,
    moderate, generational, constrained, global).

% NTSB, accident investigation boards, public inquiry commissions analyze major failures and determine whether operators had been adequately trained and exercised. They observe post-hoc whether training had been sufficient. Their authority is constrained by the doctrine: they cannot recommend abandoning real-event testing as the ultimate validation without appearing to deny that catastrophes are how we learn truth about competence.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, post_catastrophe_investigators, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared epistemic and institutional framework in which real catastrophe is accepted as the definitive test of safety-critical competence. This solves the problem of how to certify competence when the consequences of failure are catastrophic, distributed across large populations, and cannot be ethically replicated in simulation. The coordination unifies expectations: safety leadership maintains legitimacy by anchoring certification in real-event outcomes; operators accept their training as preparatory rather than conclusive; organizations justify continued exercise regimes even when fidelity limits are acknowledged as necessary costs of the regime; the public accepts risk as the inevitable price of modern infrastructure. The constraint coordinates around the principle that only real stakes test true competence.
% TRANSFER_FUNCTION: Transfers risk, uncertainty, and the burden of proof from safety leadership and exercise vendors to exposed personnel and the public. Exposed operators must work in the epistemic state of being untested-under-real-stakes until a real event either validates their competence or fails catastrophically. The public bears the externalized risk of that untested state—they are exposed to operators who have not been proven competent. Safety leadership and vendors extract authority (they alone can judge true competence) and market position (simulation is necessary but insufficient, so both simulation and real-event response preparation are required).
% ABSENT_VOICES: Simulation effectiveness researchers are excluded—their evidence that simulation can be made sufficient is pre-judged as missing the point. Operators who would advocate for redesigns that reduce catastrophe-dependence have no voice because the doctrine makes their intuitions seem dangerously naive. Organizations and national systems that have successfully decomposed exercise into components testable without real stakes (e.g., some surgical training programs, some aviation-adjacent domains) are rarely cited in mainstream safety discourse because their success implicitly challenges the doctrine. Accident survivors and bereaved families who could testify that real events are NOT good teachers (learning from catastrophe is often too late, causality is obscured by chaos, institutional learning is poor) and that better preparation earlier would have prevented catastrophe—their testimony challenges the valorization of real-event learning.
% DISAPPEARANCE_RATIONALE: If this constraint disappeared overnight—if safety organizations abandoned the doctrine that only real catastrophe exercises competence and instead invested in high-fidelity simulation, systematic competence validation, and organizational learning outside catastrophe—the entire risk and authority structure would reorganize. Funding streams would shift from real-event response preparation to simulation and systems-engineering infrastructure. Authority over competence certification would move from real-event outcomes to validated measurement systems. The epistemic framework would shift from catastrophe-as-teacher to prevention-through-simulation. Operators would transition from uncertainty-bearing (untested until catastrophe) to measurable-readiness (validated through simulation). Exercise apparatus vendors would face demand for fundamentally different products. The public-risk externalization would convert to front-loaded investment in prevention and validation. Safety leadership would need to justify authority through transparency of measurement methods rather than through real-event outcomes. Institutional slack for catastrophe response would be reallocated to prevention.
% FOUNDING_PROBLEM: Early major failures in complex systems (aviation accidents in the 1950s–1970s, nuclear near-misses in the 1960s–1980s, medical errors across all eras) revealed that operators trained through procedures and simulation performed well in routine operation but failed catastrophically under the real time pressure, real uncertainty, real stakes, and real ambiguity of actual emergencies. Operators would freeze, misdiagnose, revert to incorrect memorized procedures, or fail to integrate knowledge under stress. The doctrine emerged: only exposure to real catastrophe stress conditions could exercise true competence, train the nervous system, and develop judgment under stakes.
% FOUNDING_PROBLEM_CORROBORATION: Early aviation accident investigators (NTSB, 1960s-1980s) and aeronautical human-factors researchers (Wiener, Helmreich, Reason, 1980s-2000s) documented and attested the founding problem: operator performance under real-flight-emergency stress diverged sharply from procedure-perfect performance in simulation. This established the doctrine's scientific credibility. However, simulation effectiveness researchers (outside the benefiting institutions) document that the founding problem has been substantially addressed by modern high-fidelity simulation with physiological stressors, crew resource management training, and systematic organizational learning. Organizations like modern surgical training programs and some aviation-adjacent domains validate competence through high-fidelity simulation and documented performance without waiting for real catastrophe. Post-catastrophe analyses (NTSB, accident investigators) continue to find operator errors, but increasingly attribute them to inadequate training fidelity and organizational factors, not to simulation's fundamental insufficiency. The doctrine persists despite this evidence not because the science supports it but because the institutional structures and risk-bearing arrangements it justifies have become embedded.
narrative_ontology:disappearance_verdict(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, world_rearranges).
narrative_ontology:founding_problem_status(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0.71, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.71) because the constraint transfers risk from leadership/vendors to personnel/public via the doctrine that only real stakes test competence. Suppression is substantial (0.68) because the doctrine actively suppresses simulation-effectiveness research, alternative exercise models, and organizational learning that would challenge the necessity claim. Theater is rising (0.38→0.52) because exercise regimes perform the appearance of competence testing while the doctrine insists that true testing only occurs in catastrophe—the performance masks the gap between what exercises claim to test and what the doctrine says only catastrophe can validate. Accessibility_collapse is high (0.73) because operators and the public have no viable alternative to accepting the risk under this regime—the doctrine naturalizes catastrophe as inevitable and exercise outside catastrophe as fundamentally insufficient. Resistance is moderate (0.58) because simulation researchers and forward-thinking organizations push back against the doctrine with evidence, but their challenges are pre-judged by the core claim that only real stakes count. The claim/metric gap is deliberate: this reading CLAIMS Tangled Rope (genuine safety coordination plus asymmetric risk transfer); the metrics show substantially extractive operation with high suppression of contradicting evidence. The engine will measure that divergence.
 *
 * PERSPECTIVAL GAP:
 *   From the safety leadership and vendor seats, this is genuine coordination: they are certifying competence according to the only scientifically valid method (real-event validation), and the risk-bearing by operators/public is the necessary cost of safety systems that cannot be improved beyond simulation. From the exposed personnel and public seats, the same structure is enforced extraction: they are told their competence is untested until catastrophe, their readiness is uncertain by doctrine, and risk is externalized to them to justify the authority and market position of those certifying competence. The agenda_setter seat (safety leadership) and beneficiary seats (leadership, vendors) compute near the coordination end; the payer seats (personnel, public) compute near the extraction end. The engine derives this divergence from the structural declarations—beneficiary/victim + exit_options + identity_lock.
 *
 * DIRECTIONALITY LOGIC:
 *   Safety leadership holds power (institutional), arbitrage exit (can shift to other institutional domains), so their directionality is toward beneficiary (d near 0.0-0.3). Exercise vendors hold institutional power, mobile exit (can serve other safety domains), also toward beneficiary (d ~0.2). Exposed personnel hold powerless or moderate power, trapped or identity_locked exit (professional identity fused to the role, career path dependent, cannot exit without leaving the profession), so directionality is toward target (d ~0.8-1.0). Public at risk holds powerless power, identity_locked exit (cannot abandon the infrastructure that depends on these operators), also toward target (d ~0.85-1.0). The beneficiary/victim declarations feed the directionality computation; the overrides would correct only if the derivation missed inter-institutional nuance (not needed here).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was genuine and urgent: operators trained in simulation were failing under real catastrophe stress. That problem has been substantially addressed by modern high-fidelity simulation, systematic procedures, and organizational learning from near-misses. However, the doctrine that 'only real catastrophe exercises competence' persists and even hardens, now decoupled from the problem it was built to solve. This is mandatrophy: the founding problem is dead (modern simulation and systems engineering can certify competence without catastrophe), the founding problem status should be 'dead', but the founding_problem_status is authored as 'contested' (safety leadership contests that the problem is solved, insisting that only real stakes count). The mismatch (dead problem + world_rearranges verdict) signals mandatrophy: an arrangement built to solve a real coordination problem has outlived its function and now persists as extractive performance. A constraint that solves no live problem but extracts real risk should be reclassified—it is a Snare wearing Rope's founding narrative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_vs_real_stakes_boundary,
    'Is the difference between simulated and real catastrophe exercises in competence a difference in degree (fidelity) or in kind (fundamentally unrecoverable by any simulation)?',
    'Comparative analysis of operator performance under high-fidelity simulation vs. real-event outcomes in the same domain; measurement of learning transfer and retention across both conditions; meta-analysis of simulation-effectiveness research across safety domains.',
    'If difference is in degree (fidelity), simulation sufficiency is possible and the lived-catastrophe doctrine is unnecessary—the constraint reclassifies as extractive with no coordination function. If difference is in kind, the doctrine captures a structural reality and the constraint''s extraction is coordination cost, not pure rent. This is the central contestation between readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_vs_real_stakes_boundary, empirical, 'Simulability of real-stakes competence exercise').

omega_variable(
    covert_decay_vs_measurable_degradation,
    'Does competence degrade covertly (undetectable until real event tests it), or can degradation be measured through systematic assessment in simulation?',
    'Time-series assessment of operator competence measured through high-fidelity simulation across operators who have and have not experienced real catastrophes; comparison of measured decay trajectories with actual failure rates in real events; organizational studies of institutions that validate competence through non-catastrophic methods.',
    'Covert decay supports the necessity doctrine (only real events reveal what is hidden); measurable degradation supports the sufficiency of well-designed simulation (decay is detectable and addressable without waiting for catastrophe). This omega addresses whether the risk-transfer is addressing a real information gap or preserving an artificial one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(covert_decay_vs_measurable_degradation, empirical, 'Detectability of competence decay outside real-event conditions').

omega_variable(
    doctrine_persistence_vs_functional_necessity,
    'Does the lived-catastrophe doctrine persist because it solves the genuine coordination problem of certifying competence-under-stakes, or because it justifies institutional structures and risk-bearing arrangements that benefit safety leadership and vendors?',
    'Institutional history analysis comparing acceptance of the doctrine across domains (aviation, nuclear, medicine, emergency response) and correlating with establishment of professional certification bodies, exercise-equipment markets, and leadership authority structures; comparative study of organizations that have rejected the doctrine and the mechanisms they use instead.',
    'If functional necessity: the doctrine''s persistence is justified and the constraint is Rope/Tangled Rope. If institutional capture: the doctrine''s persistence is extractive and the constraint is Snare. The measurement series showing rising theater_ratio suggests performance over function, indicating capture.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(doctrine_persistence_vs_functional_necessity, conceptual, 'Whether the doctrine''s persistence reflects genuine necessity or institutional embedding.').

omega_variable(
    identity_lock_mechanism_in_exposed_personnel,
    'Is the exit constraint for exposed personnel (flight crews, engineers, first responders) structural (economic/legal barriers to career change) or internalized (professional identity fused to the role, belief in the necessity doctrine)?',
    'Post-exit trajectory study: when operators leave the field, does the identity-lock persist (they continue to believe only catastrophe can validate competence) or dissolve (they adopt alternative frameworks for competence once outside the regime)? Comparative study of operators in organizations that reject the doctrine vs. those in doctrine-accepting organizations.',
    'If structural: exit is trapped and the constraint''s suppression is external; operators could leave if alternatives existed. If internalized: operators carry the doctrine''s assumptions with them and the suppression persists after exit—the constraint''s effective suppression is higher than the structural measure. If both: reclassify suppression as substantially internalized, not merely structural.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism_in_exposed_personnel, empirical, 'Structural vs. internalized suppression in operator identity-lock').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(exer_tr_t0, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(exer_tr_t5, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 5, 0.41).
narrative_ontology:measurement(exer_tr_t10, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 10, 0.44).
narrative_ontology:measurement(exer_tr_t20, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 20, 0.49).
narrative_ontology:measurement(exer_tr_t30, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 30, 0.51).
narrative_ontology:measurement(exer_tr_t40, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 40, 0.52).

% Extraction over time
narrative_ontology:measurement(exer_be_t0, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(exer_be_t5, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 5, 0.61).
narrative_ontology:measurement(exer_be_t10, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 10, 0.64).
narrative_ontology:measurement(exer_be_t20, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(exer_be_t30, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 30, 0.7).
narrative_ontology:measurement(exer_be_t40, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 40, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(exer_su_t0, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(exer_su_t5, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(exer_su_t10, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 10, 0.61).
narrative_ontology:measurement(exer_su_t20, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(exer_su_t30, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 30, 0.67).
narrative_ontology:measurement(exer_su_t40, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 40, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0.14).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, exercise_as_competence_maintenance__simulation_sufficiency_reading).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, exercise_as_competence_maintenance__hybrid_decay_reading).

% DUAL FORMULATION NOTE:
% This story is one reading of the contested kernel exercise_as_competence_maintenance. Sibling constraints model alternative readings: (1) simulation_sufficiency_reading asserts high-fidelity simulation can exercise the full competence kernel; (2) hybrid_decay_reading asserts the kernel decomposes into components with different exercise requirements. The three stories share a common referent (what counts as competence maintenance and exercise) but instantiate different constraints under different readings of what that maintenance requires. The ε values differ substantially: simulation_sufficiency modeling high potential for de-catastrophization (lower ε), lived_catastrophe_necessity modeling persistent extraction from risk-bearing personnel (higher ε, here 0.71), hybrid_decay modeling intermediate necessity for some real-stakes exposure (middle ε). All three share the network edge to signal kinship in the contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
