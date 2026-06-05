% ============================================================================
% CONSTRAINT STORY: preparedness_commitment__competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_commitment__competence_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: preparedness_commitment__competence_reading
 *   human_readable: Preparedness as Live Exercised Knowledge (Competence Reading)
 *   domain: disaster_preparedness/institutional_memory/commitment_systems
 *
 * SUMMARY:
 *   Preparedness as live exercised knowledge is a distinct reading of the
 *   preparedness_commitment kernel, emphasizing that genuine institutional
 *   readiness depends on continuous, functionally-coupled training, real-time
 *   decision-making drills, and effective intergenerational transmission of
 *   tacit knowledge. This reading holds that preparedness is a rope-type
 *   coordination mechanism — a genuine solution to the problem of maintaining
 *   operational capacity despite inevitable personnel turnover and
 *   generational change. The constraint exhibits low base extractiveness
 *   (0.18) and low theater ratio (0.25) because the mechanism is
 *   self-reinforcing: training that improves capacity directly benefits the
 *   trained personnel and the organization, creating alignment between the
 *   constraint's function and the interests of those it governs. However,
 *   this reading is vulnerable to degradation. As institutional memory
 *   fragments, as drills become increasingly ritualized, or as theater ratio
 *   rises (through proceduralization, checklist compliance, loss of genuine
 *   surprise testing), the constraint transitions toward the husk reading —
 *   the same formal structure persists but actual competence erodes. The
 *   competence reading asserts that the distinction is real and
 *   diagnostically critical: organizations that maintain low theater (genuine
 *   decision-making challenges in drills) survive crises with contained
 *   failure; organizations that allow theater to rise (ritual drills
 *   disconnected from real decision-making) experience catastrophic failure
 *   (D5 break) when actual disaster occurs.
 *
 * KEY AGENTS:
 *   - Trained Response Organizations: Primary beneficiary (organized/mobile/generational) — organized actors that see preparedness as genuine coordination and reinvest in competence maintenance
 *   - Institutional Authority: Primary beneficiary and enforcer (institutional/arbitrage/national) — government or institutional actors mandating preparedness as a real solution to institutional continuity
 *   - Competent Practitioners: Secondary beneficiary (powerful/mobile/biographical) — individual responders or disaster managers who develop professional competence through live exercised knowledge
 *   - Obligated Community Members: Secondary victim (moderate/constrained/local) — civilians required to participate in drills or preparedness training who bear time costs without transparent benefit
 *   - Vestigial Bureaucracies: Institutional actor prone to degradation (institutional/arbitrage/civilizational) — authorities that maintain preparedness mandates but lose capacity to enforce competence-testing, allowing theater to rise
 *   - Analytical Observer: Structural position (analytical/analytical/global) — can identify the critical threshold between competence and husk readings by examining post-crisis failure analysis
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_commitment__competence_reading, 0.18).
domain_priors:suppression_score(preparedness_commitment__competence_reading, 0.12).
domain_priors:theater_ratio(preparedness_commitment__competence_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(preparedness_commitment__competence_reading, theater_ratio, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_commitment__competence_reading, rope).
narrative_ontology:human_readable(preparedness_commitment__competence_reading, "Preparedness as Live Exercised Knowledge (Competence Reading)").
narrative_ontology:topic_domain(preparedness_commitment__competence_reading, "disaster_preparedness/institutional_memory/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_commitment__competence_reading, '1a94e5a7-d473-4ac4-b830-3e88c8c0a353').
narrative_ontology:cs_kernel_codification('1a94e5a7-d473-4ac4-b830-3e88c8c0a353', formalized).
narrative_ontology:cs_authority_grounding('1a94e5a7-d473-4ac4-b830-3e88c8c0a353', practice).
narrative_ontology:cs_interpretation_layer_present('1a94e5a7-d473-4ac4-b830-3e88c8c0a353').
narrative_ontology:cs_reading_relation('1a94e5a7-d473-4ac4-b830-3e88c8c0a353', preparedness_commitment__husk_reading, forecloses).
narrative_ontology:cs_reading_relation('1a94e5a7-d473-4ac4-b830-3e88c8c0a353', preparedness_commitment__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('1a94e5a7-d473-4ac4-b830-3e88c8c0a353', foundational, drills_test_real_decision_making).
narrative_ontology:cs_axiom_status(drills_test_real_decision_making, holdable).
narrative_ontology:cs_axiom_grounding('1a94e5a7-d473-4ac4-b830-3e88c8c0a353', drills_test_real_decision_making, empirically_contingent).
narrative_ontology:cs_axiom('1a94e5a7-d473-4ac4-b830-3e88c8c0a353', foundational, tacit_knowledge_transmits_across_generations).
narrative_ontology:cs_axiom_status(tacit_knowledge_transmits_across_generations, holdable).
narrative_ontology:cs_axiom_grounding('1a94e5a7-d473-4ac4-b830-3e88c8c0a353', tacit_knowledge_transmits_across_generations, empirically_contingent).
narrative_ontology:cs_reference_frame('1a94e5a7-d473-4ac4-b830-3e88c8c0a353', live_competence_maintenance).
narrative_ontology:cs_drift_state('1a94e5a7-d473-4ac4-b830-3e88c8c0a353', contemporary_budget_constrained_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1a94e5a7-d473-4ac4-b830-3e88c8c0a353', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(preparedness_commitment__competence_reading, preparedness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_commitment__competence_reading, trained_personnel).
narrative_ontology:constraint_beneficiary(preparedness_commitment__competence_reading, organizations_maintaining_competence).
narrative_ontology:constraint_beneficiary(preparedness_commitment__competence_reading, future_generations_inheriting_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAINED RESPONSE ORGANIZATION (ROPE) — Sees preparedness as genuine coordination mechanism. Regular drills test real decision-making capacity, staff rotation is managed through apprenticeship and knowledge transfer, tabletop exercises expose gaps that training addresses. The constraint solves the collective action problem of maintaining readiness across personnel turnover. Benefits (capacity retention, institutional continuity) are distributed to the organization itself and reinvested in improved response capability. Low extraction because the constraint's function is its own reward.
constraint_indexing:constraint_classification(preparedness_commitment__competence_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 2: INSTITUTIONAL AUTHORITY (ROPE) — Government or institutional actors mandating preparedness see genuine coordination value: drills expose vulnerabilities and drive operational improvements; training ensures institutional memory survives personnel changes. The constraint is enforced through regulation and budget (requires active resource commitment) but the enforcement returns value in improved response capacity. The beneficiary (the institution itself, in its future capacity to respond) is the same as the agent managing the constraint. Pure coordination from this perspective.
constraint_indexing:constraint_classification(preparedness_commitment__competence_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: COMPETENT PRACTITIONER (ROPE) — Emergency responder or disaster management professional experiences preparedness as skill-building and career development. Drills sharpen decision-making; training provides professional identity and advancement. Exit options are available (can leave the profession) but the constraint itself coordinates professional growth and practical competence. The practitioner benefits directly from the knowledge-sharing and skill-testing that the constraint provides.
constraint_indexing:constraint_classification(preparedness_commitment__competence_reading, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 4: OBLIGATED COMMUNITY MEMBER (SNARE) — Civilian who participates in drills or receives mandatory preparedness training experiences significant constraints. Participation is legally required (constrained exit); time cost is borne by the individual; benefits accrue to institutional continuity but not equally to the participant. From this perspective, preparedness drills extract labor time without transparent benefit distribution. The constraint is real coordination for the organization; extraction for the obligated participant.
constraint_indexing:constraint_classification(preparedness_commitment__competence_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 5: VESTIGIAL BUREAUCRACY (PITON) — At civilizational scale, the constraint degrades. Annual compliance exercises become checklist rituals detached from real risk assessment. Competence-testing protocols persist through inertia but loss of institutional memory (due to budget cuts, organizational restructuring, leadership turnover) means the drills no longer test decision-making capacity — they test the ability to perform the ritual. Theater ratio rises substantially when competence pathway degrades; the constraint becomes maintenance of the appearance of preparedness rather than actual capacity.
constraint_indexing:constraint_classification(preparedness_commitment__competence_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (ROPE) — From a long-horizon, global perspective, live exercised knowledge is a genuine coordination mechanism. Organizations that maintain high theater ratio (≥0.70, pure ritual) experience D5 catastrophic failure when real crisis occurs (Pearl Harbor, 9/11 pre-event warnings ignored, New Orleans emergency command center). Organizations maintaining low theater ratio (≤0.30, live competence testing) show adaptive capacity and contained failure when crisis hits. The constraint is a real solution to the intergenerational knowledge transfer problem — but only when theater remains low and competence-testing remains functionally coupled to drills.
constraint_indexing:constraint_classification(preparedness_commitment__competence_reading, rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_commitment__competence_reading_tests).

test(piton_threshold) :-
    domain_priors:theater_ratio(preparedness_commitment__competence_reading, TR),
    TR >= 0.70.

:- end_tests(preparedness_commitment__competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low and stable. This reading asserts that when preparedness is genuinely live-exercised, benefits align with those bearing the constraint's cost. Trained personnel gain professional competence and career advancement; organizations gain improved response capacity; future cohorts inherit operational readiness. The constraint does not extract value away from its beneficiaries — it coordinates their interests. The modest extractiveness (not zero) reflects the labor cost borne by participants (time in drills, study, simulations) and the resource cost to institutional authorities (funding for realistic training, facility access). But this cost is transparent and reinvested in the shared good. Suppression (0.12): Low. In the competence reading, personnel are not suppressed — they have mobile exit options (can leave the profession or organization) and the constraint operates through genuine benefit generation, not coercion. Obligated community members experience higher suppression (constrained exit, time cost), but from the competence reading's perspective, this suppression is the cost of institutional coordination, not extractive overhead. Theater ratio (0.25): Low and slightly rising. The measurement trajectory shows theater remaining consistently low, indicating that drills continue to test real decision-making (surprise elements, novel scenarios, genuine risk of failure). Slight rise reflects normal proceduralization (some standardized testing elements) but does not cross into ritual-only performance. The competence reading remains stable under this trajectory.
 *
 * PERSPECTIVAL GAP:
 *   The critical perspectival gap in the competence reading is between the trained organization (rope) and the obligated community member (snare). Both perspectives acknowledge the same constraint structure — regular drills, mandatory training, institutional commitment to preparedness. But the trained organization sees genuine coordination and skill-building, while the community member sees mandatory time extraction. The gap widens when the community member cannot exit and cannot see how their participation benefits them directly. The competence reading resolves this gap by asserting that the gap itself is a measurement error: if true competence-testing is occurring, community member participation IS directly beneficial (they gain skills for self-protection and disaster response). If the gap persists despite competence-testing, the reading suspects the constraint has degraded toward the husk reading — the drills are ritual, and the community member's intuition about extraction is correct. The analytical observer sees this gap as the diagnostic signal: if competence-testing is real, gap should narrow as community members experience genuine skill development; if gap persists or widens, husk reading is emergent.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for the competence reading is derived from the genuine coordination function and aligned benefit structure. Beneficiary agents (trained personnel, institutional authorities, future response capacity) occupy low-d positions (low extractiveness experienced) because the constraint's function is their own interest. Moderately-powered agents with constrained exit (obligated community members) occupy higher-d positions because they bear costs without proportional voice in the coordination mechanism. Organizational-level agents with arbitrage options (institutional authorities) occupy the lowest-d positions because they both create and benefit from the constraint. The competence reading assumes that d remains relatively stable across the interval because the mechanism continues to align function with benefit. If theater ratio were to rise sharply (visible in measurements as theater_ratio → 0.65+), d values would shift upward for all agents except institutional authorities, indicating constraint degradation toward the husk reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_theater_decoupling,
    'At what point does rising theater ratio decouple from actual operational capacity? Is there a threshold beyond which drills become predictable performances that no longer test decision-making?',
    'Post-crisis analysis: compare theater ratio (measured from drill complexity, surprise elements, real-time decision requirements) at time t0 against actual failure modes observed in real disaster response at time t1. Correlation between theater ratio and response failure rate across multiple organizations and disaster types.',
    'If threshold exists below 0.40: competence reading remains valid only under strict theater discipline. If decoupling occurs smoothly: the husk reading (pure performance) is always latent and waiting to emerge. If decoupling is sharp (cliff): the constraint exhibits phase transition behavior, and managing theater ratio becomes the primary control variable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(competence_theater_decoupling, empirical, 'Threshold at which rising theater decouples from operational capacity').

omega_variable(
    generational_knowledge_transmission_mechanism,
    'What specific mechanisms ensure live knowledge (tacit decision-making heuristics, pattern recognition in novel situations, rapid judgment under uncertainty) actually transmit across cohort turnover? Does apprenticeship model work, or does knowledge evaporate at each generational boundary?',
    'Ethnographic study of knowledge transmission in high-reliability organizations (nuclear power, aviation, emergency response). Measure: retention of non-codified decision rules after 50%+ personnel turnover; comparison of disaster response quality before/after major generational transition; interviews identifying what training methods preserve tacit knowledge vs what is lost to replacement cohorts.',
    'If transmission mechanism is robust: competence reading is stable across generations. If transmission is fragile: each generation must re-learn capacity, and the constraint defaults toward husk reading (performative maintenance without competence). If transmission requires specific organizational structures (long-term employment, mentorship relationships): the constraint''s viability depends on labor market and organizational design choices outside the preparedness system itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generational_knowledge_transmission_mechanism, empirical, 'Whether tacit knowledge actually transmits across generational turnover').

omega_variable(
    reading_identity_recognition,
    'Does the institutional authority recognize the distinction between the competence reading (live exercised knowledge, low theater) and the husk reading (memorial performance, high theater)? Or does the authority treat both as valid preparedness?',
    'Policy analysis: examine disaster preparedness mandates and evaluation criteria. Do policies require demonstration of decision-making competence in drills (competence reading)? Or do they only require ritual completion and documentation (husk reading)? If both are acceptable, institutional authority has not recognized the distinction. Interview institutional leaders: can they articulate the difference? Do they value one over the other?',
    'If authority recognizes competence reading as distinct: institutional pressure maintains low theater and reinforces competence pathway. If authority conflates readings: husk reading becomes increasingly acceptable, theater ratio drifts upward, and the constraint gradually transitions from rope (genuine coordination) to piton (vestigial performance). The hybrid reading emerges when authorities attempt to mandate both simultaneously, creating permission for theater and competence to coexist.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_identity_recognition, conceptual, 'Whether institutional authority recognizes the competence/husk distinction').

omega_variable(
    crisis_exposure_window,
    'How long can an organization maintain the appearance of competence (high theater, degraded actual capacity) before a real crisis exposes the gap? Is there a ''crisis exposure window'' — a time period after which actual disaster risk has grown faster than theater can hide the incompetence?',
    'Historical analysis of organizations that experienced catastrophic failures (Challenger explosion, post-Katrina NOLA emergency response, COVID-19 supply chain collapse, bank stress tests that missed 2008 indicators). Measure: time lag between competence loss (identifiable in hindsight as degraded training, institutional memory loss, or ritual-only exercises) and crisis exposure. Identify whether early-warning signals were present in theater-only drills.',
    'If exposure window is short (<5 years): theater cannot sustain the appearance of competence for long. Organizations in husk reading will experience rapid D5 failure. If window is long (>10 years): theater can hide incompetence for a generation, allowing institutional inertia to fully degrade capacity. If window is variable by crisis type: some disasters (predictable, slow-onset) expose incompetence quickly; others (rare, complex) permit theater to persist longer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(crisis_exposure_window, empirical, 'Time lag between competence loss and crisis exposure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_commitment__competence_reading, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_competence_tr_t0, preparedness_commitment__competence_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(prep_competence_tr_t8, preparedness_commitment__competence_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement(prep_competence_tr_t16, preparedness_commitment__competence_reading, theater_ratio, 16, 0.25).

% Extraction over time
narrative_ontology:measurement(prep_competence_be_t0, preparedness_commitment__competence_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(prep_competence_be_t8, preparedness_commitment__competence_reading, base_extractiveness, 8, 0.17).
narrative_ontology:measurement(prep_competence_be_t16, preparedness_commitment__competence_reading, base_extractiveness, 16, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(prep_competence_su_t0, preparedness_commitment__competence_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(prep_competence_su_t8, preparedness_commitment__competence_reading, suppression_requirement, 8, 0.11).
narrative_ontology:measurement(prep_competence_su_t16, preparedness_commitment__competence_reading, suppression_requirement, 16, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_commitment__competence_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(preparedness_commitment__competence_reading, preparedness_commitment__husk_reading).
narrative_ontology:affects_constraint(preparedness_commitment__competence_reading, preparedness_commitment__hybrid_reading).
narrative_ontology:affects_constraint(preparedness_commitment__competence_reading, institutional_memory_fragmentation).
narrative_ontology:affects_constraint(preparedness_commitment__competence_reading, generational_knowledge_loss).

% DUAL FORMULATION NOTE:
% The preparedness_commitment kernel admits three structurally distinct readings: competence_reading (this file) models preparedness as live exercised knowledge with low theater and genuine coordination function; husk_reading models the same institutional structure but with risen theater and eroded competence; hybrid_reading models layered systems where memorial elements stabilize commitment while competence elements maintain function. The three readings share the same external institutional structure but differ fundamentally in whether drills test real decision-making capacity. Each reading has its own ε value reflecting whether the constraint coordinates or extracts. They are linked as related constraints within a single kernel contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
