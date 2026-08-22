% ============================================================================
% CONSTRAINT STORY: preparedness_persistence__competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_persistence__competence_reading, []).

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
 *   constraint_id: preparedness_persistence__competence_reading
 *   human_readable: Drills and Inspections as Live Exercised Knowledge
 *   domain: institutional/disaster_preparedness
 *
 * SUMMARY:
 *   Under the competence reading, drills and inspections represent live
 *   exercised knowledge: practice that maintains operational readiness by
 *   preserving embodied procedural knowledge across responder teams. The
 *   constraint persists because it solves a genuine coordination and
 *   capability problem — emergency response under extreme stress requires
 *   muscle memory, team timing, and procedural automaticity that cannot be
 *   maintained through documentation alone. This reading asserts that the
 *   founding problem (the need for embodied readiness under stress) remains
 *   live and that drills continue to solve it. The alternative readings
 *   (husk_reading, hybrid_reading) contest this claim, arguing that drills
 *   have degraded into memorial performance or that competence is stratified
 *   across different preparedness components. The competence reading is a
 *   clean, ε-invariant constraint story that instantiates the frame in which
 *   drills ARE the practice, not its theatrical imitation.
 *
 * KEY AGENTS:
 *   - Emergency response personnel (beneficiaries, maintain readiness through practice)
 *   - Protected population (beneficiaries, depend on maintained responder competence)
 *   - Preparedness administrators (agenda-setters, allocate resources and schedule drills)
 *   - Oversight bodies (observers, enforce compliance and audit readiness)
 *   - Incident victims (excluded, their testimony appears only retrospectively)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_persistence__competence_reading, 0.12).
domain_priors:suppression_score(preparedness_persistence__competence_reading, 0.08).
domain_priors:theater_ratio(preparedness_persistence__competence_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(preparedness_persistence__competence_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_persistence__competence_reading, rope).
narrative_ontology:human_readable(preparedness_persistence__competence_reading, "Drills and Inspections as Live Exercised Knowledge").
narrative_ontology:topic_domain(preparedness_persistence__competence_reading, "institutional/disaster_preparedness").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_persistence__competence_reading, 'ccae7dfe-71cb-45a2-ae32-c06f84dc02de').
narrative_ontology:cs_kernel_codification('ccae7dfe-71cb-45a2-ae32-c06f84dc02de', distributed).
narrative_ontology:cs_authority_grounding('ccae7dfe-71cb-45a2-ae32-c06f84dc02de', practice).
narrative_ontology:cs_interpretation_layer_present('ccae7dfe-71cb-45a2-ae32-c06f84dc02de').
narrative_ontology:cs_reading_relation('ccae7dfe-71cb-45a2-ae32-c06f84dc02de', preparedness_persistence__husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('ccae7dfe-71cb-45a2-ae32-c06f84dc02de', preparedness_persistence__hybrid_reading, influences).
narrative_ontology:cs_axiom('ccae7dfe-71cb-45a2-ae32-c06f84dc02de', foundational, drills_maintain_embodied_competence).
narrative_ontology:cs_axiom_status(drills_maintain_embodied_competence, holdable).
narrative_ontology:cs_axiom_grounding('ccae7dfe-71cb-45a2-ae32-c06f84dc02de', drills_maintain_embodied_competence, empirically_contingent).
narrative_ontology:cs_axiom('ccae7dfe-71cb-45a2-ae32-c06f84dc02de', foundational, founding_problem_live_under_stress).
narrative_ontology:cs_axiom_status(founding_problem_live_under_stress, holdable).
narrative_ontology:cs_axiom_grounding('ccae7dfe-71cb-45a2-ae32-c06f84dc02de', founding_problem_live_under_stress, empirically_contingent).
narrative_ontology:cs_reference_frame('ccae7dfe-71cb-45a2-ae32-c06f84dc02de', practice_sustains_readiness).
narrative_ontology:cs_drift_state('ccae7dfe-71cb-45a2-ae32-c06f84dc02de', contemporary_professionalization_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('ccae7dfe-71cb-45a2-ae32-c06f84dc02de', '').
narrative_ontology:cs_kernel_id(preparedness_persistence__competence_reading, preparedness_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_persistence__competence_reading, emergency_response_personnel).
narrative_ontology:constraint_beneficiary(preparedness_persistence__competence_reading, protected_population).
narrative_ontology:constraint_vindicates(preparedness_persistence__competence_reading, operational_readiness_requires_continuous_practice).
narrative_ontology:constraint_vindicates(preparedness_persistence__competence_reading, muscle_memory_persists_under_stress).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Firefighters, paramedics, civil defense officials. Drills maintain their procedural competence, muscle memory, and team coordination under conditions approaching real incident stress. Without regular drills, they would lose the embodied knowledge required to perform under pressure. They participate in drills as part of their professional obligations and gain genuine operational readiness.
narrative_ontology:constraint_stakeholder(preparedness_persistence__competence_reading, emergency_response_personnel, beneficiary,
    moderate, biographical, constrained, local).

% Residents, workers, students in jurisdictions with active preparedness programs. They depend on responders' maintained competence for their survival in actual emergencies. They do not participate in drills but benefit from the coordination and readiness drills produce. Their exit option is geographic relocation, which is prohibitively expensive for most.
narrative_ontology:constraint_stakeholder(preparedness_persistence__competence_reading, protected_population, beneficiary,
    powerless, immediate, trapped, local).

% Fire chiefs, emergency management directors, civil protection agencies. They schedule and oversee drills, allocate resources to preparedness training, and certify readiness. They benefit from the constraint by maintaining institutional legitimacy and demonstrating competence to oversight bodies. They have substantial discretion over drill frequency, scope, and rigor.
narrative_ontology:constraint_stakeholder(preparedness_persistence__competence_reading, preparedness_administrators, agenda_setter,
    institutional, generational, mobile, regional).

% Elected officials, regulatory agencies, licensing boards. They audit preparedness programs, evaluate readiness after incidents, and enforce compliance with minimum standards. They observe the constraint's operation and can mandate increases in drill frequency or rigor based on incident investigation findings.
narrative_ontology:constraint_stakeholder(preparedness_persistence__competence_reading, oversight_bodies, observer,
    institutional, generational, analytical, regional).

% Those harmed in actual emergencies where responders lacked readiness. They are structurally excluded from the preparedness conversation — they appear only retrospectively after the constraint has failed. Their testimony drives post-incident review but does not shape ongoing drill policy in advance.
narrative_ontology:constraint_stakeholder(preparedness_persistence__competence_reading, incident_victims, excluded,
    powerless, immediate, trapped, local).

% The research and regulatory communities examining whether drills maintain operational readiness or have degraded into ritual. They assess this reading against alternatives and measure whether competence persists.
narrative_ontology:constraint_stakeholder(preparedness_persistence__competence_reading, analytical_observer, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a shared understanding of emergency procedures across distributed responder teams through repeated joint practice. Drills coordinate timing, communication protocols, and mutual role expectations that cannot be maintained through documentation alone — muscle memory and team timing must be practiced under conditions approaching real incident stress to persist.
% TRANSFER_FUNCTION: Moves time and attention from responders and administrators to preparedness training, at the cost of other activities. The constraint transfers opportunity cost (hours not spent on routine duties) into collective readiness. Under this reading, there is no extraction — the transfer is the product.
% ABSENT_VOICES: Victims of past incidents where preparedness failed are structurally excluded from the conversation that shapes current drill policy. They cannot advocate for increased rigor because they are not present until after a new incident. Their retrospective testimony drives review processes but does not govern ongoing decisions about drill frequency, scope, or standards.
% DISAPPEARANCE_RATIONALE: If drills and inspections disappeared, emergency response competence would degrade over months (procedural knowledge atrophies, team timing breaks down, new personnel lack training). Incident outcomes would shift sharply: response times would lengthen, coordination failures would increase, and fatality rates in major incidents would rise. The entire mutual-aid structure of emergency response depends on the practice maintaining readiness.
% FOUNDING_PROBLEM: Emergency response requires immediate coordinated action under extreme stress with little time for consultation. Procedures must be embodied (muscle memory) and team timing must be rehearsed so that coordination persists when communication is degraded or personnel are injured. Documentation and classroom instruction cannot maintain this readiness alone.
% FOUNDING_PROBLEM_CORROBORATION: Post-incident investigations consistently identify lapses in drilled procedure as a factor in casualty rates. Emergency medicine research documents the decay of procedural competence in medical responders who do not practice regularly. Neuroscience literature on motor learning under stress confirms that muscle memory requires repetition under conditions approximating real stress. Independent testimony from incident survivors, medical literature, and engineering failure analysis all support the founding problem.
narrative_ontology:disappearance_verdict(preparedness_persistence__competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_persistence__competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_persistence__competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(preparedness_persistence__competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_persistence__competence_reading, 0.12, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_persistence__competence_reading_tests).
:- end_tests(preparedness_persistence__competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.12) because the constraint does not concentrate gains or impose asymmetric costs. Responders benefit from maintained competence; protected populations benefit from improved survival chances in real incidents; administrators benefit from institutional legitimacy and reduced post-incident liability. The measured extraction reflects only the opportunity cost of time diverted to drills — a necessary cost of maintaining readiness, not an extraction in the economic sense. Suppression is negligible (0.08) because the constraint persists through alignment of interest, not coercion: responders have internalized the practice because it improves their performance; administrators enforce drills because they are required by law and insurance standards; oversight bodies mandate compliance because incidents drive public and legislative pressure. Theater is low but rising slightly through the interval (0.08 to 0.15) because the fraction of drill activity that is purely performative (for documentation and certification) increases as compliance bureaucracy grows, but the core practice remains functional. Accessibility alternatives are nearly completely collapsed (0.92) not because of suppression but because emergency response is a natural monopoly under crisis conditions — individuals cannot coordinate their own response, and exit means geographic relocation. Resistance is minimal (0.05) because the constraint faces no organized opposition: responders and administrators both benefit, and the population cannot voice objection to a preparedness program it depends on for survival.
 *
 * PERSPECTIVAL GAP:
 *   The competence reading produces uniform seat classification: all seats that exist should compute as Rope (genuine coordination with minimal extraction). Responders compute as beneficiaries (they gain readiness); protected populations compute as beneficiaries (they gain survival chances, albeit diffuse and uncertain); administrators compute as agenda-setters who benefit from maintaining institutional legitimacy; oversight bodies observe and certify. There is no target seat extracting value. The alternative readings (husk_reading, hybrid_reading) would change this structure significantly: they would identify administrators as targets (bearing the cost of performative drills without competence gain) or victims (maintaining an appearance of readiness they know is false). The competence reading forecloses the victim structure by asserting that the founding problem is live and that drills continue to solve it.
 *
 * DIRECTIONALITY LOGIC:
 *   Under this reading, directionality is symmetric across all beneficiary seats (d ≈ 0.5 or lower): responders benefit from readiness (d toward 0.0), protected populations benefit from improved survival (d toward 0.0), administrators benefit from institutional compliance and reduced post-incident liability (d toward 0.0). There are no victims — no seat bears costs without benefit. This contrasts sharply with the husk_reading (in which administrators would be targets forced to maintain performative compliance without gain) and the hybrid_reading (in which some components would show asymmetric extraction). The competence reading's claim that drills ARE functional practice, not memorial performance, determines the directionality structure entirely.
 *
 * MANDATROPHY ANALYSIS:
 *   The competence reading explicitly rejects mandatrophy: the founding problem (the need for embodied readiness) is asserted as live, and drills are asserted as solving it. This reading prevents mis-classification as Piton (atrophied coordination maintained only by theater). If empirical measurement over the interval showed theater rising above 0.5 while measured responder competence (post-incident response time, procedure adherence, team coordination) remained stable or improved, the competence reading would be falsified by the engine — theater > 0.5 signals that form persists while function atrophies, which is the Piton signature. The low theater ratio (0.15 at interval end) in this story is consistent with the reading's claim that drills remain functional.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    responder_competence_measurement,
    'How should responder competence be measured: by proxy (drill participation metrics, certification records) or by direct assessment (post-incident response quality, procedure adherence under stress)?',
    'Longitudinal study correlating drill participation and frequency against actual incident response performance (response time, procedure adherence, coordination quality). If proxy metrics rise while incident performance degrades, drills have become performative.',
    'If proxy and direct measures diverge, the competence reading fails and the husk_reading becomes plausible. If they converge, competence is maintained and the reading holds. This directly resolves whether the constraint is Rope (coordination maintained) or Piton (performance maintained).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(responder_competence_measurement, empirical, 'Whether drills maintain real competence or only maintain the appearance of competence.').

omega_variable(
    founding_problem_persistence,
    'Has the founding problem (the requirement for embodied procedural knowledge under stress) been solved by technology (real-time communication, decision-support systems, automated procedures) such that drills no longer solve the original problem?',
    'Comparative incident analysis: jurisdictions with advanced communication and automation systems versus those without. If advanced systems reduce the competence advantage of drill-trained personnel, the founding problem has shifted.',
    'If technology has substantially reduced the founding problem, drills may persist as institutional theater rather than functional practice. The constraint would transition from Rope (solving a live problem) to Piton (maintained by inertia). The reading would be falsified empirically but not logically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_persistence, empirical, 'Whether the founding problem motivating drills persists or has been partially solved by technological change.').

omega_variable(
    kernel_reading_identity,
    'Is the kernel ''emergency preparedness persists through drills and inspections'' a commitment to a PRACTICE (the reading is about how readiness is maintained) or a commitment to a FUNCTION (the reading is about what preparedness achieves)?',
    'Authority analysis: who maintains the kernel (practitioners, agencies, legislation)? What do they defend when challenged (the necessity of drill practice, or the effectiveness of preparedness overall)? The sibling readings (husk, hybrid) represent different answers to this question.',
    'If the kernel is practice-oriented, the competence reading is a coherent instantiation: drills ARE the practice. If the kernel is function-oriented, the same drills could instantiate the husk reading (practice persists but function atrophies). The kernel''s framing determines which reading is coherent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the preparedness kernel is framed as a commitment to practice or a commitment to function.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_persistence__competence_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_persistence__competence_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(prep_tr_t0, observed).
narrative_ontology:measurement(prep_tr_t5, preparedness_persistence__competence_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement_basis(prep_tr_t5, observed).
narrative_ontology:measurement(prep_tr_t10, preparedness_persistence__competence_reading, theater_ratio, 10, 0.14).
narrative_ontology:measurement_basis(prep_tr_t10, observed).
narrative_ontology:measurement(prep_tr_t15, preparedness_persistence__competence_reading, theater_ratio, 15, 0.17).
narrative_ontology:measurement_basis(prep_tr_t15, observed).
narrative_ontology:measurement(prep_tr_t20, preparedness_persistence__competence_reading, theater_ratio, 20, 0.16).
narrative_ontology:measurement_basis(prep_tr_t20, observed).
narrative_ontology:measurement(prep_tr_t25, preparedness_persistence__competence_reading, theater_ratio, 25, 0.15).
narrative_ontology:measurement_basis(prep_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_persistence__competence_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement_basis(prep_be_t0, observed).
narrative_ontology:measurement(prep_be_t5, preparedness_persistence__competence_reading, base_extractiveness, 5, 0.11).
narrative_ontology:measurement_basis(prep_be_t5, observed).
narrative_ontology:measurement(prep_be_t10, preparedness_persistence__competence_reading, base_extractiveness, 10, 0.12).
narrative_ontology:measurement_basis(prep_be_t10, observed).
narrative_ontology:measurement(prep_be_t15, preparedness_persistence__competence_reading, base_extractiveness, 15, 0.13).
narrative_ontology:measurement_basis(prep_be_t15, observed).
narrative_ontology:measurement(prep_be_t20, preparedness_persistence__competence_reading, base_extractiveness, 20, 0.12).
narrative_ontology:measurement_basis(prep_be_t20, observed).
narrative_ontology:measurement(prep_be_t25, preparedness_persistence__competence_reading, base_extractiveness, 25, 0.12).
narrative_ontology:measurement_basis(prep_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_persistence__competence_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement_basis(prep_su_t0, observed).
narrative_ontology:measurement(prep_su_t5, preparedness_persistence__competence_reading, suppression_requirement, 5, 0.06).
narrative_ontology:measurement_basis(prep_su_t5, observed).
narrative_ontology:measurement(prep_su_t10, preparedness_persistence__competence_reading, suppression_requirement, 10, 0.08).
narrative_ontology:measurement_basis(prep_su_t10, observed).
narrative_ontology:measurement(prep_su_t15, preparedness_persistence__competence_reading, suppression_requirement, 15, 0.09).
narrative_ontology:measurement_basis(prep_su_t15, observed).
narrative_ontology:measurement(prep_su_t20, preparedness_persistence__competence_reading, suppression_requirement, 20, 0.08).
narrative_ontology:measurement_basis(prep_su_t20, observed).
narrative_ontology:measurement(prep_su_t25, preparedness_persistence__competence_reading, suppression_requirement, 25, 0.08).
narrative_ontology:measurement_basis(prep_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_persistence__competence_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_persistence__competence_reading, 0.08).
narrative_ontology:affects_constraint(preparedness_persistence__competence_reading, preparedness_persistence__husk_reading).
narrative_ontology:affects_constraint(preparedness_persistence__competence_reading, preparedness_persistence__hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the preparedness_persistence kernel. All three stories share the same referent (the standing commitment to maintain preparedness through drills) but instantiate different readings of what that commitment entails. The competence_reading asserts that drills maintain live operational knowledge; the husk_reading asserts that drills have become memorial performance; the hybrid_reading asserts that competence is stratified. Each reading has its own ε (low for competence, high for husk), its own beneficiary/victim structure (no victims under competence, administrators as targets under husk), and its own classification (Rope vs. Piton). The engine computes which reading is structurally true from the measurements and stakeholder data.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
