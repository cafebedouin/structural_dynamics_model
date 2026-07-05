% ============================================================================
% CONSTRAINT STORY: exercise_as_competence_maintenance__lived_catastrophe_necessity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: exercise_as_competence_maintenance__lived_catastrophe_necessity_reading
 *   human_readable: Lived-Catastrophe-Necessity Reading of Competence Maintenance
 *   domain: safety_engineering/organizational_learning/crisis_preparedness
 *
 * SUMMARY:
 *   This story instantiates the lived-catastrophe-necessity reading of the
 *   exercise_as_competence_maintenance kernel: only an actual catastrophe
 *   genuinely exercises the competence kernel that governs how operators
 *   respond under real stakes. Simulation, however elaborate, is rehearsal —
 *   a distinct activity that resembles the thing it prepares for but does not
 *   activate the same judgment mechanisms. Under this reading, competence
 *   quietly atrophies in the interval between real incidents regardless of
 *   how much simulation occurs, and the certification apparatus that treats
 *   simulation completion as equivalent to maintained competence is
 *   systematically over-certifying decayed capability. This is a
 *   decomposition sibling of two other readings of the same kernel
 *   (simulation_sufficiency_reading, hybrid_decay_reading) and is authored
 *   here as a single, clean, ε-invariant constraint per Rule 1 — the contest
 *   between readings is not described inside this constraint's own logic.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0.62).
domain_priors:suppression_score(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0.58).
domain_priors:theater_ratio(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0.71).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 0.71).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, tangled_rope).
narrative_ontology:human_readable(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, "Lived-Catastrophe-Necessity Reading of Competence Maintenance").
narrative_ontology:topic_domain(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, "safety_engineering/organizational_learning/crisis_preparedness").

domain_priors:requires_active_enforcement(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, '37b87b42-e349-4210-96c3-e60ce0f8b424').
narrative_ontology:cs_kernel_codification('37b87b42-e349-4210-96c3-e60ce0f8b424', distributed).
narrative_ontology:cs_authority_grounding('37b87b42-e349-4210-96c3-e60ce0f8b424', practice).
narrative_ontology:cs_interpretation_layer_present('37b87b42-e349-4210-96c3-e60ce0f8b424').
narrative_ontology:cs_reading_relation('37b87b42-e349-4210-96c3-e60ce0f8b424', exercise_as_competence_maintenance__simulation_sufficiency_reading, forecloses).
narrative_ontology:cs_reading_relation('37b87b42-e349-4210-96c3-e60ce0f8b424', exercise_as_competence_maintenance__hybrid_decay_reading, influences).
narrative_ontology:cs_axiom('37b87b42-e349-4210-96c3-e60ce0f8b424', foundational, only_lived_stakes_activate_judgment_kernel).
narrative_ontology:cs_axiom_status(only_lived_stakes_activate_judgment_kernel, holdable).
narrative_ontology:cs_axiom_grounding('37b87b42-e349-4210-96c3-e60ce0f8b424', only_lived_stakes_activate_judgment_kernel, empirically_contingent).
narrative_ontology:cs_axiom('37b87b42-e349-4210-96c3-e60ce0f8b424', secondary, simulation_produces_covert_not_absent_decay).
narrative_ontology:cs_axiom_status(simulation_produces_covert_not_absent_decay, holdable).
narrative_ontology:cs_axiom_grounding('37b87b42-e349-4210-96c3-e60ce0f8b424', simulation_produces_covert_not_absent_decay, empirically_contingent).
narrative_ontology:cs_reference_frame('37b87b42-e349-4210-96c3-e60ce0f8b424', apprenticeship_through_real_incident_exposure).
narrative_ontology:cs_drift_state('37b87b42-e349-4210-96c3-e60ce0f8b424', contemporary_simulation_centric_certification_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('37b87b42-e349-4210-96c3-e60ce0f8b424', '').
narrative_ontology:cs_kernel_id(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, exercise_as_competence_maintenance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, simulation_vendors).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, training_certification_bodies).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, incumbent_crisis_leadership).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, public_exposed_to_untested_operators).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, frontline_responders_facing_real_incidents).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, junior_operators_denied_real_stakes_rotation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sell tabletop exercises, drills, and simulation platforms as competence-maintenance products. Under this reading their offering is explicitly rehearsal, not the thing itself — they profit from being purchased anyway, since organizations must be seen doing something between catastrophes, and the necessity reading does not eliminate demand for simulation, it merely reframes it as insufficient theater that still gets funded.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, simulation_vendors, beneficiary,
    organized, biographical, arbitrage, national).

% Set and administer the certification standards that treat completed simulation cycles as evidence of maintained competence. They control the credentialing pipeline and collect fees, dues, and legitimacy from certifying people whose judgment-under-real-stakes has, on this reading, necessarily atrophied since their last actual catastrophe.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, training_certification_bodies, agenda_setter,
    institutional, generational, arbitrage, national).

% Senior officials whose authority rests on having led through a past real catastrophe. Under this reading their standing is doubly protected: they hold the rare credential of lived exercise, and the framework devalues everyone whose competence rests only on simulation, entrenching their position regardless of how long ago their real-stakes experience occurred or whether it has itself decayed.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, incumbent_crisis_leadership, beneficiary,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, incumbent_crisis_leadership, agenda_setter).

% Firefighters, ICU staff, control-room operators, and similar personnel who must act in the actual emergency. If the necessity reading is correct, most of them arrive at the real event with competence that has quietly decayed despite passing every simulated evaluation, and they discover the gap only at the moment it costs the most — they cannot exit the requirement to be the ones who face the real thing.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, frontline_responders_facing_real_incidents, payer,
    moderate, immediate, trapped, local).

% Newer staff who, by design or bureaucratic caution, are kept out of the rare real-catastrophe rotations that would exercise the kernel, on the theory that risk should be borne by seasoned staff. Under this reading they are structurally prevented from ever acquiring the only form of competence that counts, no matter how many simulations they complete.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, junior_operators_denied_real_stakes_rotation, payer,
    powerless, biographical, constrained, local).

% Residents, patients, passengers, and other bystanders who rely on operators certified through simulation but who, on this reading, have never had their competence genuinely exercised. They have no voice in certification design and no way to know, before an incident, whether the operator responsible for their safety carries real or merely simulated competence.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, public_exposed_to_untested_operators, excluded,
    powerless, immediate, trapped, regional).

% Study post-incident performance data to compare outcomes between operators with real-catastrophe exposure and those with simulation-only histories. They can produce the evidence that would settle or complicate the necessity claim but do not control certification policy or funding for exercises.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, organizational_learning_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, diffuse).
narrative_ontology:fixing_cost_class(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates organizational preparation for low-frequency, high-consequence events by providing a repeatable, low-cost substitute activity (simulation) that keeps skills notionally current between rare real incidents, and provides a credentialing mechanism so responsibility can be assigned.
% TRANSFER_FUNCTION: Moves budget, credentialing fees, and institutional legitimacy from organizations and the public to simulation vendors and certification bodies, while moving unacknowledged risk from the certifying apparatus onto frontline responders and the public who bear the cost when simulated competence proves insufficient at the real event.
% ABSENT_VOICES: The public exposed to a given operator's actual competence level has no seat in setting certification standards; junior staff excluded from real-stakes rotations have no channel to contest the exclusion; post-incident survivors rarely participate in redesigning the training regime that failed them.
% DISAPPEARANCE_RATIONALE: If the certification-via-simulation apparatus vanished overnight, incumbent leadership and certification bodies would lose their institutional basis and would insist the world had rearranged catastrophically; frontline responders and researchers who hold the necessity reading would argue little of substance changes, since the apparatus was already failing to exercise the kernel it claims to maintain — the disagreement is exactly the reading's central contest.
% FOUNDING_PROBLEM: Organizations needed some way to maintain and demonstrate readiness for catastrophic events that occur too rarely for any individual to accumulate real experience through direct exposure alone.
% FOUNDING_PROBLEM_CORROBORATION: Certification bodies and simulation vendors attest the founding problem is being solved by current practice. Post-incident review boards, several independent safety researchers, and survivors' inquiries in aviation and nuclear incident histories have repeatedly found that certified, simulation-trained personnel froze or misjudged under real conditions in ways their simulated record did not predict — corroboration from outside the certifying and vendor apparatus supports the reading that the founding problem remains live and largely unaddressed by simulation alone.
narrative_ontology:disappearance_verdict(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, contested).
narrative_ontology:founding_problem_status(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0.62, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness rises over the interval (0.42 to 0.62) reflecting a corpus of institutions that increasingly rely on simulation-based certification even as, on this reading, the actual competence it purports to maintain silently decays. Theater ratio is the dominant signal here (0.50 rising to 0.71): under the necessity reading, an increasing share of institutional 'preparedness activity' is properly classified as performance, since it produces certification and budget justification without producing the one thing (lived catastrophe exposure) that actually exercises the kernel. Suppression is moderate (0.58) rather than extreme because the constraint operates mostly through institutional habit and credentialing incentives rather than coercive enforcement against dissenters — though it does actively exclude junior staff from real-stakes rotation and marginalizes researchers who challenge the equivalence.
 *
 * PERSPECTIVAL GAP:
 *   From the certification body's seat, the system is functioning exactly as coordination should: standardized preparation, auditable process, defensible credentialing. From the frontline responder's seat under real incident conditions, the same system may have certified competence that was never actually tested — the gap is invisible until the catastrophe arrives, at which point it is too late to renegotiate the credential.
 *
 * DIRECTIONALITY LOGIC:
 *   Simulation vendors, certification bodies, and incumbent leadership sit near the beneficiary end: they collect fees, legitimacy, and entrenched authority from a system that treats simulation as sufficient evidence of readiness, even though this reading holds that evidence to be structurally inadequate. Frontline responders and junior operators sit near the target end: they are the ones whose (possibly decayed) competence is tested by the real event, and they bear the cost of a false positive in the certification system. The public is excluded entirely from the credentialing conversation yet bears the ultimate exposure — hence victim status despite having no formal role in the constraint's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preparing for rare catastrophic events without direct experience) remains genuinely live — rare-event preparation is a real coordination problem, which is why this is authored as tangled_rope rather than pure snare. But under the necessity reading, the mechanism that has grown up to solve it (simulation-based certification) is diverging further from actually solving it over time, as reflected in the rising theater ratio. The classification captures both: coordination function is real (hence not a pure snare), but the mechanism enforced to satisfy it is increasingly extractive of trust, budget, and false confidence — hence tangled rope, not scaffold, since no sunset clause exists and the arrangement shows no sign of being treated as transitional.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indeterminacy,
    'Is competence-under-catastrophe exercised only by lived catastrophe (this reading), sufficiently by high-fidelity simulation (simulation_sufficiency_reading), or split into separately-exercisable procedural and judgment components (hybrid_decay_reading)?',
    'Longitudinal comparison of post-incident performance between operators with real-catastrophe exposure, operators with high-fidelity-simulation-only histories, and operators under hybrid training regimes, controlling for time-since-last-real-event and simulation fidelity level.',
    'If the necessity reading is correct, current certification-by-simulation regimes systematically over-certify decayed competence and the tangled_rope classification with rising theater_ratio is the accurate structural account. If simulation_sufficiency is correct instead, the same regime is closer to a genuine rope with much lower true extraction than authored here. This story deliberately authors only the necessity reading per the ε-invariance principle; the sibling readings carry their own ε values in separate stories.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Which reading of the competence-exercise kernel is structurally correct is not resolved by this story alone.').

omega_variable(
    covert_decay_detectability,
    'If competence genuinely decays covertly between real catastrophes as this reading claims, is there any simulation-based or proxy measure capable of detecting that decay before a real incident exposes it?',
    'Design and validate stress-test protocols that attempt to simulate genuine stakes (e.g., unannounced high-fidelity drills with real consequences for failure) and compare their predictive validity against real-incident outcomes.',
    'If no proxy measure can detect the decay, the necessity reading implies the certification system is structurally incapable of self-correction, strengthening the tangled_rope/near-snare reading. If a valid proxy exists, the gap between this reading and hybrid_decay_reading narrows considerably.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(covert_decay_detectability, empirical, 'Whether covert competence decay under this reading is even measurable before catastrophe strikes.').

omega_variable(
    beneficiary_capture_of_evidence,
    'Given that certification bodies and simulation vendors both fund and interpret much of the evidence about simulation efficacy, is the corpus of evidence favoring simulation sufficiency itself compromised by beneficiary capture?',
    'Audit funding sources and methodological independence of published studies on simulation-based competence retention; compare findings from independently funded post-incident review boards against vendor-funded efficacy studies.',
    'If capture is substantial, the apparent empirical support for treating simulation as adequate is itself an artifact of the extraction this reading identifies, reinforcing the tangled_rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_capture_of_evidence, empirical, 'Whether the evidence base for simulation adequacy is compromised by the same beneficiaries this reading identifies.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(exer_tr_t0, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 0, 0.5).
narrative_ontology:measurement(exer_tr_t8, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 8, 0.56).
narrative_ontology:measurement(exer_tr_t16, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 16, 0.61).
narrative_ontology:measurement(exer_tr_t24, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 24, 0.65).
narrative_ontology:measurement(exer_tr_t32, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 32, 0.68).
narrative_ontology:measurement(exer_tr_t40, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 40, 0.71).

% Extraction over time
narrative_ontology:measurement(exer_be_t0, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(exer_be_t8, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(exer_be_t16, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 16, 0.53).
narrative_ontology:measurement(exer_be_t24, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 24, 0.57).
narrative_ontology:measurement(exer_be_t32, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 32, 0.6).
narrative_ontology:measurement(exer_be_t40, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(exer_su_t0, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(exer_su_t8, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 8, 0.45).
narrative_ontology:measurement(exer_su_t16, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 16, 0.49).
narrative_ontology:measurement(exer_su_t24, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 24, 0.53).
narrative_ontology:measurement(exer_su_t32, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 32, 0.56).
narrative_ontology:measurement(exer_su_t40, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0.12).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, exercise_as_competence_maintenance__simulation_sufficiency_reading).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, exercise_as_competence_maintenance__hybrid_decay_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language concept 'exercise as competence maintenance' per the ε-invariance principle. lived_catastrophe_necessity_reading claims simulation is insufficient and competence decays covertly (this file, tangled_rope, rising theater_ratio). simulation_sufficiency_reading claims high-fidelity simulation genuinely exercises the kernel (expected lower ε, closer to rope). hybrid_decay_reading splits the kernel into procedural and judgment components with different exercise requirements (expected mixed ε). Each carries its own beneficiary/victim structure and its own stable ε; they are linked via affects_constraints rather than merged into one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
