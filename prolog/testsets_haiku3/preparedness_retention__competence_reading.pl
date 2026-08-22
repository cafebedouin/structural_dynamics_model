% ============================================================================
% CONSTRAINT STORY: preparedness_retention__competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_retention__competence_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: preparedness_retention__competence_reading
 *   human_readable: Live Preparedness Competence Retention (Competence Reading)
 *   domain: governance/institutional/safety
 *
 * SUMMARY:
 *   This constraint is the competence reading of the preparedness_retention
 *   kernel: the claim that drills and inspections are live, embodied
 *   competence-preserving practices that maintain operational capacity for
 *   genuine crisis response. The constraint is authored under the
 *   presupposition that preparedness works—that the exercises produce
 *   measurable, retained skill and that organizational muscle memory persists
 *   across staff turnover and inter-crisis periods. The alternative readings
 *   (husk_reading: drills are hollow ritual; hybrid_reading: competence is
 *   stratified across institutions, ceremony at the edge) contest whether the
 *   live knowledge claim holds uniformly across the emergency response
 *   system. This reading focuses on the institutional logic of competence
 *   retention itself, the coordination problem it solves, and the minimal
 *   extraction it entails.
 *
 * KEY AGENTS:
 *   - Emergency response personnel: design, execute, and maintain the drills; their careers and institutional legitimacy depend on demonstrated competence.
 *   - Institutional administrators: allocate budget and set drill frequency/rigor; they balance preparedness against fiscal constraint.
 *   - Population at risk: benefit from competent response but cannot exit the risk geography; have no voice in how preparedness is designed.
 *   - Competing fiscal priorities: excluded from the table but bear the opportunity cost of budget allocated to drills.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_retention__competence_reading, 0.18).
domain_priors:suppression_score(preparedness_retention__competence_reading, 0.12).
domain_priors:theater_ratio(preparedness_retention__competence_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, resistance, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_retention__competence_reading, rope).
narrative_ontology:human_readable(preparedness_retention__competence_reading, "Live Preparedness Competence Retention (Competence Reading)").
narrative_ontology:topic_domain(preparedness_retention__competence_reading, "governance/institutional/safety").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_retention__competence_reading, 'bf1b5526-4529-4316-9bfd-fc507c5c8fea').
narrative_ontology:cs_kernel_codification('bf1b5526-4529-4316-9bfd-fc507c5c8fea', distributed).
narrative_ontology:cs_authority_grounding('bf1b5526-4529-4316-9bfd-fc507c5c8fea', practice).
narrative_ontology:cs_interpretation_layer_present('bf1b5526-4529-4316-9bfd-fc507c5c8fea').
narrative_ontology:cs_reading_relation('bf1b5526-4529-4316-9bfd-fc507c5c8fea', preparedness_retention__husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('bf1b5526-4529-4316-9bfd-fc507c5c8fea', preparedness_retention__hybrid_reading, influences).
narrative_ontology:cs_axiom('bf1b5526-4529-4316-9bfd-fc507c5c8fea', foundational, live_embodied_competence_retention).
narrative_ontology:cs_axiom_status(live_embodied_competence_retention, holdable).
narrative_ontology:cs_axiom_grounding('bf1b5526-4529-4316-9bfd-fc507c5c8fea', live_embodied_competence_retention, empirically_contingent).
narrative_ontology:cs_axiom('bf1b5526-4529-4316-9bfd-fc507c5c8fea', secondary, drills_produce_measurable_skill_decay_prevention).
narrative_ontology:cs_axiom_status(drills_produce_measurable_skill_decay_prevention, holdable).
narrative_ontology:cs_axiom_grounding('bf1b5526-4529-4316-9bfd-fc507c5c8fea', drills_produce_measurable_skill_decay_prevention, empirically_contingent).
narrative_ontology:cs_reference_frame('bf1b5526-4529-4316-9bfd-fc507c5c8fea', functional_preparedness_baseline).
narrative_ontology:cs_drift_state('bf1b5526-4529-4316-9bfd-fc507c5c8fea', contemporary_post_major_disasters, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('bf1b5526-4529-4316-9bfd-fc507c5c8fea', '').
narrative_ontology:cs_kernel_id(preparedness_retention__competence_reading, preparedness_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_retention__competence_reading, population_safety).
narrative_ontology:constraint_beneficiary(preparedness_retention__competence_reading, emergency_response_capability).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_retention__competence_reading, emergency_response_personnel).
narrative_ontology:constraint_beneficiary(preparedness_retention__competence_reading, population_exposed_to_risk).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Professional emergency responders—fire, rescue, water management, public health—who depend on regular drills and competency certifications to maintain the muscle memory and procedural fluency required to function under crisis. They design and execute the drills, measure skill retention, and identify gaps. Their career progression and institutional legitimacy rest on demonstrated preparedness.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, emergency_response_personnel, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(preparedness_retention__competence_reading, emergency_response_personnel, agenda_setter).

% Communities at risk from disasters (floods, fires, chemical incidents, pandemics). They benefit from emergency response personnel who are competent and drilled. They cannot exit the risk geography. Their safety depends on whether the drills produce real competence or performative ritual.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, population_exposed_to_risk, beneficiary,
    powerless, immediate, trapped, national).

% Government agencies, regional water boards, and municipal authorities that allocate budget to drills, inspections, and training. They must balance preparation against fiscal constraint and competing priorities. They set the frequency and rigor of exercises, approve certification standards, and decide what constitutes 'competence verification.'
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, institutional_administrators, agenda_setter,
    institutional, generational, mobile, national).

% Health systems, infrastructure maintenance, education, and social services that also require funding. They are not at the table during preparedness planning but would argue that resources spent on drills reduce capacity for preventive health care, infrastructure upgrades, or other services.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, competing_fiscal_priorities, excluded,
    institutional, biographical, analytical, national).

% The institutional memory and procedural knowledge embedded in preparedness systems. This is not an actor but an outcome: the constraint's purpose is to keep this knowledge alive and transmissible across personnel turnover and generational shifts.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, knowledge_transfer_continuity, beneficiary,
    analytical, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(preparedness_retention__competence_reading, knowledge_transfer_continuity).

% Researchers, auditors, and external evaluators who assess whether drills produce competence or merely satisfy ceremonial requirements. They measure learning outcomes, retention curves, skill degradation between exercises, and compare against actual crisis performance data.
narrative_ontology:constraint_stakeholder(preparedness_retention__competence_reading, analytical_observer, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Keeps emergency response organizations functionally ready across generational staff turnover and extended inter-crisis periods by practicing and certifying the procedures, decisions, and muscle memory required to save lives under extreme time pressure. Drills solve the temporal coordination problem: how to maintain competence in low-frequency, high-stakes events.
% TRANSFER_FUNCTION: Moves personnel attention, institutional budget, and operational time from normal-state activities into rehearsal of crisis procedures. The transfer is from routine operations to competence maintenance. The beneficiary is future-crisis response capacity (population safety); the cost is opportunity cost of the time and resources spent drilling instead of other priorities.
% ABSENT_VOICES: Individuals and communities that bear opportunity costs (services underprovided because budget went to preparedness) are not typically represented in preparedness planning. Rival institutions competing for the same budget are structurally excluded from the table where drill frequency and rigor are decided.
% DISAPPEARANCE_RATIONALE: If the constraint—the live, enforced requirement to maintain competence through regular drills and inspections—vanished overnight, emergency response organizations would degrade competence on a predictable curve: within 12–18 months, procedural fluency would erode measurably; within 3–5 years, entire cohorts of trained personnel would retire without replacement training; crisis response would shift from coordinated, practiced protocols to improvisation and institutional confusion. Population safety outcomes would decline measurably.
% FOUNDING_PROBLEM: Historical crises revealed that emergency response systems that do not continuously drill become dysfunctional when called upon. Personnel trained years prior forget procedures; inter-organizational coordination breaks down; new staff lack the embodied knowledge to act under pressure. The founding problem is: how to prevent the organization from forgetting what it knows it must do.
% FOUNDING_PROBLEM_CORROBORATION: After-action reports from major disasters (1995 Kobe earthquake, 2005 Hurricane Katrina, 2010 Haiti earthquake, 2011 Japan tsunami) consistently identify competence gaps and coordination failures in organizations that had not drilled. Personnel who HAD participated in recent exercises performed measurably better. Independent disaster-research literature (Quarantelli, Dynes, Drabek) confirms that practiced, certified procedures outperform improvisation. Water management authorities in the Netherlands document that regular competence certification prevents the knowledge loss that would attend unmanaged staff turnover.
narrative_ontology:disappearance_verdict(preparedness_retention__competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_retention__competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_retention__competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(preparedness_retention__competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_retention__competence_reading, 0.18, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_retention__competence_reading_tests).
:- end_tests(preparedness_retention__competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18) because the constraint's primary function is coordination and competence maintenance, not wealth transfer. Personnel and administrators are net beneficiaries of clear standards and practiced procedures; populations at risk benefit from capable response. The opportunity cost exists but is modest relative to the coordination gain. Suppression is low (0.12) because the constraint does not coerce participation—personnel want to stay competent for professional reasons; administrators can choose drill frequency. Theater ratio is moderate (0.22) because some drills are exercises in tabletop planning (lower-fidelity theater), while others are full-scale field tests (high-fidelity competence practice); the reading's core claim is that the functional drills dominate. Accessibility collapse is low-to-moderate (0.35) because alternatives to drills exist (individual study, reading, simulation software) but are demonstrably inferior to embodied practice; once the constraint is understood, the necessity of drills is transparent. Resistance is low (0.28) because emergency responders and administrators endorse preparedness; the resistance that exists comes from fiscal pressure and competing priorities, not from the personnel expected to drill.
 *
 * PERSPECTIVAL GAP:
 *   From the emergency response personnel's seat, this is pure coordination: drills are how they maintain the competence they need to do their jobs safely and effectively. From the institutional administrator's seat, it is a resource allocation decision: they must justify the time and budget spent drilling against other claimed needs. From the population's seat, there is no divergence—they benefit if the constraint produces competence and suffer if it does not. The key structural difference is TIME HORIZON: personnel and administrators operate on biographical and generational timescales where competence decay is visible and salient; populations at risk are distributed across immediate exposures, not concentrated on the competence question. The engine should compute the same type (rope) across all seated perspectives because the coordination logic is symmetric—no party is systematically extracts from others through the drill requirement.
 *
 * DIRECTIONALITY LOGIC:
 *   Emergency response personnel (organized, biographical, constrained by professional licensing) are symmetric beneficiaries: they need competence certification for career legitimacy and the drills provide it. Institutional administrators (institutional, mobile) have low d toward the target end because they set the rules and can adjust frequency; they are closer to beneficiaries (they want capable response organizations). Populations at risk (powerless, immediate, trapped) are pure beneficiaries with zero directionality toward extraction: the constraint protects them and they have no input. Competing fiscal priorities are excluded entirely. No overrides are warranted; the structural data (beneficiary declarations, exit options by role) produce accurate d values without adjustment.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint has not suffered mandate obsolescence. The founding problem (organizations forget how to execute crisis procedures if they do not drill) is live and remains the central challenge in disaster preparedness. The constraint's purpose—keep competence alive—is still what the system is supposed to do. No omega variable flags mandate drift here because the founding problem is empirically validated by post-disaster reviews and the constraint's function remains aligned with its mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_vs_ceremony_empirical_boundary,
    'At what measurable threshold of drill fidelity, frequency, and post-exercise validation does a preparedness program shift from producing live competence to performing ceremony?',
    'Prospective correlational study linking drill design parameters (fidelity, frequency, staff rotation) to actual crisis performance metrics (response time, coordination success, mortality outcome) in comparable disaster events. Retrospective analysis of after-action reports from organizations with different drill regimes.',
    'If the boundary is sharp and measurable, preparedness systems can be certified as competence-producing or ceremonial. If the boundary is fuzzy or context-dependent, the constraint''s type is genuinely contested and no single reading captures all cases. The engine would then flag the kernel as irreducible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_vs_ceremony_empirical_boundary, empirical, 'Where the empirical line between functional competence drills and ceremonial exercises sits.').

omega_variable(
    stratification_across_institutional_scale,
    'Is competence retention uniformly distributed across emergency response institutions, or does it stratify—with specialized institutions (water boards, hazmat teams) maintaining live knowledge while generalist or peripheral organizations maintain only ceremony?',
    'Audit of competence outcomes in organizations at different scales and specialization levels. Post-disaster analysis comparing response quality across institutional types. Personnel interviews about actual versus practiced procedures.',
    'If stratification is systematic, the constraint is better modeled as the hybrid_reading (competence in specialized layers, ceremony at the edge) rather than this reading (competence uniformly live). This would indicate that the founding problem is partially solved for some institutions and unsolved for others.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stratification_across_institutional_scale, empirical, 'Whether preparedness competence is uniformly retained or stratified across institutional types.').

omega_variable(
    reading_foreclosure_test,
    'Does the competence_reading logically foreclose the husk_reading, or do both remain live positions that different parties could hold?',
    'Structural analysis: the competence_reading asserts that drills produce retained, embodied knowledge. The husk_reading asserts they do not—that the observed drills are performative and competence has atrophied. These are direct contradictions IF they claim the SAME observables. But they may be addressing different sub-populations or different time periods. The question is whether one reading''s core premise logically rules out the other within a SINGLE commitment framework.',
    'If foreclosure is real (one reading''s core premise directly contradicts the other''s), then the engine should compute forecloses in reading_relations. If both readings remain live for different institutional actors or different drill types, coexists_with is correct. The answer determines the logical shape of the kernel.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_foreclosure_test, conceptual, 'Whether the competence and husk readings logically foreclose each other or coexist.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_retention__competence_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_retention__competence_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(prep_tr_t5, preparedness_retention__competence_reading, theater_ratio, 5, 0.19).
narrative_ontology:measurement(prep_tr_t10, preparedness_retention__competence_reading, theater_ratio, 10, 0.21).
narrative_ontology:measurement(prep_tr_t15, preparedness_retention__competence_reading, theater_ratio, 15, 0.22).
narrative_ontology:measurement(prep_tr_t20, preparedness_retention__competence_reading, theater_ratio, 20, 0.23).
narrative_ontology:measurement(prep_tr_t25, preparedness_retention__competence_reading, theater_ratio, 25, 0.22).
narrative_ontology:measurement(prep_tr_t30, preparedness_retention__competence_reading, theater_ratio, 30, 0.22).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_retention__competence_reading, base_extractiveness, 0, 0.14).
narrative_ontology:measurement(prep_be_t5, preparedness_retention__competence_reading, base_extractiveness, 5, 0.16).
narrative_ontology:measurement(prep_be_t10, preparedness_retention__competence_reading, base_extractiveness, 10, 0.17).
narrative_ontology:measurement(prep_be_t15, preparedness_retention__competence_reading, base_extractiveness, 15, 0.18).
narrative_ontology:measurement(prep_be_t20, preparedness_retention__competence_reading, base_extractiveness, 20, 0.19).
narrative_ontology:measurement(prep_be_t25, preparedness_retention__competence_reading, base_extractiveness, 25, 0.18).
narrative_ontology:measurement(prep_be_t30, preparedness_retention__competence_reading, base_extractiveness, 30, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(preparedness_retention__competence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_retention__competence_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_retention__competence_reading, 0.12).
narrative_ontology:affects_constraint(preparedness_retention__competence_reading, preparedness_retention__husk_reading).
narrative_ontology:affects_constraint(preparedness_retention__competence_reading, preparedness_retention__hybrid_reading).

% DUAL FORMULATION NOTE:
% The preparedness_retention kernel decomposes into three constraint readings: (1) competence_reading—drills produce live competence; (2) husk_reading—drills are ceremonial and competence has atrophied; (3) hybrid_reading—competence stratifies across institutions. Each reading instantiates a different constraint with different ε, beneficiary/victim structures, and institutional implications. The three readings share the same kernel (preparedness institutions and their drills) but diverge on whether the drills functionally retain what they claim. Competence_reading influences both siblings by establishing the baseline claim that would be false if either alternative reading were correct.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
