% ============================================================================
% CONSTRAINT STORY: preparedness_transmission__competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_transmission__competence_reading, []).

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
 *   constraint_id: preparedness_transmission__competence_reading
 *   human_readable: Preparedness Transmission via Live Exercised Knowledge (Competence Reading)
 *   domain: disaster_risk_management/institutional_memory/civil_defense
 *
 * SUMMARY:
 *   The competence reading of preparedness transmission frames drills and
 *   inspections as live exercised knowledge — a coordination mechanism where
 *   capability is re-validated through repeated practice under controlled
 *   variation. This reading emphasizes that disaster response competence
 *   cannot be maintained through documentation alone; it requires continuous
 *   embodied practice, scenario engagement, and real-time decision-making
 *   feedback. From this perspective, drills are not bureaucratic compliance
 *   events but essential mechanisms for maintaining the distributed
 *   institutional knowledge that enables effective response when actual
 *   disasters occur. The constraint exhibits low extraction (ε=0.22) and low
 *   theater ratio (0.35) because the mechanism genuinely serves its stated
 *   coordination function: responders gain competence and adaptive capacity;
 *   organizations maintain response readiness; communities benefit from
 *   validated response capability. This reading presupposes that knowledge
 *   transfer occurs primarily through practice-based learning, that novel
 *   scenario elements are essential for exercising improvisation capacity,
 *   and that decentralized competence (knowledge distributed across trained
 *   personnel networks) is superior to centralized procedural documentation.
 *
 * KEY AGENTS:
 *   - Disaster Response Organizations: Primary beneficiary (organized/constrained) — maintain capability readiness and distributed competence through regular drills; coordination function is genuine and proportional to costs
 *   - Trained Practitioners (Firefighters, EMTs, Civil Defense Officers): Secondary beneficiary (powerful/mobile) — gain competence, decision-making skill, adaptive readiness; direct benefit from drill participation
 *   - Embedded Community Members: Mixed (moderate/constrained) — benefit from validated response capability but bear costs of disruption and resource allocation
 *   - Civil Defense System (Institutional): Primary beneficiary (institutional/arbitrage) — maintains continuity of preparedness capability across generations; no single point of failure
 *   - Inspectors and Training Officers: Moderate actor (organized/constrained) — validate competence, design scenario variations, identify knowledge gaps; benefit from having a functional assessment mechanism
 *   - Analytical Observer: Epistemological perspective (analytical/analytical) — represents the claim that institutional knowledge is maintained through live practice, not documentation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_transmission__competence_reading, 0.22).
domain_priors:suppression_score(preparedness_transmission__competence_reading, 0.18).
domain_priors:theater_ratio(preparedness_transmission__competence_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(preparedness_transmission__competence_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_transmission__competence_reading, rope).
narrative_ontology:human_readable(preparedness_transmission__competence_reading, "Preparedness Transmission via Live Exercised Knowledge (Competence Reading)").
narrative_ontology:topic_domain(preparedness_transmission__competence_reading, "disaster_risk_management/institutional_memory/civil_defense").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_transmission__competence_reading, 'd4ccd387-eff5-443e-83de-9d2466d3f74b').
narrative_ontology:cs_kernel_codification('d4ccd387-eff5-443e-83de-9d2466d3f74b', formalized).
narrative_ontology:cs_authority_grounding('d4ccd387-eff5-443e-83de-9d2466d3f74b', lineage).
narrative_ontology:cs_interpretation_layer_present('d4ccd387-eff5-443e-83de-9d2466d3f74b').
narrative_ontology:cs_reading_relation('d4ccd387-eff5-443e-83de-9d2466d3f74b', preparedness_transmission__husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('d4ccd387-eff5-443e-83de-9d2466d3f74b', preparedness_transmission__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('d4ccd387-eff5-443e-83de-9d2466d3f74b', foundational, competence_requires_embodied_practice).
narrative_ontology:cs_axiom_status(competence_requires_embodied_practice, holdable).
narrative_ontology:cs_axiom_grounding('d4ccd387-eff5-443e-83de-9d2466d3f74b', competence_requires_embodied_practice, empirically_contingent).
narrative_ontology:cs_axiom('d4ccd387-eff5-443e-83de-9d2466d3f74b', foundational, improvisation_under_novel_scenarios_is_essential).
narrative_ontology:cs_axiom_status(improvisation_under_novel_scenarios_is_essential, holdable).
narrative_ontology:cs_axiom_grounding('d4ccd387-eff5-443e-83de-9d2466d3f74b', improvisation_under_novel_scenarios_is_essential, empirically_contingent).
narrative_ontology:cs_reference_frame('d4ccd387-eff5-443e-83de-9d2466d3f74b', preparedness_as_distributed_embodied_competence).
narrative_ontology:cs_drift_state('d4ccd387-eff5-443e-83de-9d2466d3f74b', contemporary_institutional_compliance_pressure, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d4ccd387-eff5-443e-83de-9d2466d3f74b', '').
narrative_ontology:cs_kernel_id(preparedness_transmission__competence_reading, preparedness_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_transmission__competence_reading, disaster_response_organizations).
narrative_ontology:constraint_beneficiary(preparedness_transmission__competence_reading, civil_population).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISASTER RESPONSE ORGANIZATION (ROPE) — Organized institutional actors (fire departments, emergency management agencies, civil defense commands) experience drills and inspections as genuine coordination mechanisms. The constraint solves the collective action problem of maintaining capability readiness without a current crisis. The structure benefits the organization through distributed competence validation; costs are real but proportional to benefit (training time, equipment wear, personnel cycles). Low extraction — the mechanism serves its stated function.
constraint_indexing:constraint_classification(preparedness_transmission__competence_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 2: TRAINED PRACTITIONER (ROPE) — Individual responders see drills as skill maintenance and scenario learning. The constraint enables them to practice decision-making under conditions of controlled uncertainty, improvise adaptations, and recognize novel failure signatures they haven't encountered before. Exit option is mobile (can leave the profession, though career mobility has costs). Extraction is minimal — the practitioner gains competence, readiness confidence, and adaptive capacity directly from drill participation.
constraint_indexing:constraint_classification(preparedness_transmission__competence_reading, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: EMBEDDED COMMUNITY MEMBER (TANGLED ROPE) — Citizens who live in jurisdiction where drills occur experience mixed coordination and extraction. Benefit: the drills validate that responders possess the competence to protect them if an actual disaster occurs — genuine coordination function. Cost: drills disrupt routines, create anxiety about disaster risk, consume shared resources (street closures, traffic delays, alert system testing). Constrained exit — the community member cannot opt out of living in the jurisdiction without high relocation costs. The extraction is moderate because the coordination benefit is real and proportional to the cost burden.
constraint_indexing:constraint_classification(preparedness_transmission__competence_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 4: CIVIL DEFENSE SYSTEM (INSTITUTIONAL VIEW) (ROPE) — At the civilizational scale, the constraint codifies the principle that preparedness is maintained through continuous practice, not through stored procedures or documentation alone. The system benefits from distributed competence (no single point of failure, knowledge embedded in trained personnel networks). Extraction is minimal — the mechanism serves institutional continuity directly. This is the canonical institutional beneficiary perspective.
constraint_indexing:constraint_classification(preparedness_transmission__competence_reading, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: BUREAUCRATIC COMPLIANCE INTERPRETER (PITON) — A degraded institutional reading: when drills become audit checkboxes rather than competence validation, the constraint atrophies into pure theater. Inspectors follow compliance rubrics, participants perform prescribed motions, debriefs record learning that isn't acted upon. The ritual persists due to mandates and liability protection, not because it maintains capability. Theater ratio is high (0.80+); extraction is minimal because the bureaucratic actor benefits primarily from defensibility ('we held the drill') rather than actual capability gain. This perspective represents the constraint in its degraded form.
constraint_indexing:constraint_classification(preparedness_transmission__competence_reading, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (ROPE) — From the analytical/global perspective, the competence reading instantiates a claim about how institutional knowledge is transmitted: through live practice that allows participants to encounter novel scenarios, improvise responses, and internalize decision-making patterns that no documentation can fully capture. This is a genuine epistemic claim about knowledge transfer — that 'knowing how' requires repetition, variation, and real-time feedback. The constraint's low extraction and theater ratio (0.35) reflect that this mechanism actually works as stated: drills validly exercise and re-validate capability.
constraint_indexing:constraint_classification(preparedness_transmission__competence_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_transmission__competence_reading_tests).

test(piton_threshold) :-
    domain_priors:theater_ratio(preparedness_transmission__competence_reading, TR),
    TR >= 0.70.

:- end_tests(preparedness_transmission__competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.22): Low. The constraint solves a genuine collective action problem — maintaining disaster response competence without an active crisis. All parties benefit from the coordination function. Trained responders gain competence directly; organizations maintain readiness; communities benefit from validated response capability. The slight extractiveness reflects real costs: training time, personnel cycles, equipment wear, and community disruption. But these costs are proportional to benefits and transparently connected to the stated function. There is no hidden extraction or significant asymmetry. Suppression (0.18): Low. The constraint operates through voluntary institutional participation (disaster response organizations) and mandatory professional duty (trained responders). Suppression is minimal because the mechanism enables agency: responders practice decision-making under variation, improvise responses, and improve competence actively. Community members have constrained but not suppressed exit (relocation is high-cost but possible). Theater ratio (0.35): Low-moderate. Drills in organizations committed to actual competence maintenance show this ratio: some performative elements (official protocols, documentation, debriefs that record learning) alongside genuine adaptive content (novel scenarios, real-time problem-solving, genuine unknowns about system response). The ratio rises when compliance becomes the primary goal (piton reading), stays low when competence validation is primary (this reading). Measurements show slight rise over interval, reflecting institutional drift toward more formalized (more theatrical) processes, but the constraint remains functionally competence-focused rather than compliance-focused.
 *
 * PERSPECTIVAL GAP:
 *   The competence reading emphasizes adaptive capacity: inspectors recognize novel failure signatures; participants improvise effectively under scenario variation; knowledge transfer through practice enables real-time improvisation in actual emergencies. This perspective presupposes that the primary constraint function is working and that drills genuinely exercise competence. The piton perspective (Perspective 5, bureaucratic compliance interpreter) represents the constraint in its degraded form, where theater dominates and compliance becomes the primary goal. The analytical observer (Perspective 6) represents the epistemological claim about institutional knowledge — that 'knowing how' requires embodied practice. The gap between these perspectives is empirical: does the drill actually produce competence and adaptive capacity, or is it primarily performative? The competence reading says yes; the husk_reading (sibling, not included in perspectives) says the function has atrophied. The perspectival difference is measurable through theater_ratio and follow-up action metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is derived from structural position. Disaster response organizations are primary beneficiaries (arbitrage exit, low d ≈ 0.15) — they benefit from the coordination function directly and can exit if they choose (though response mandates constrain this). Trained practitioners are secondary beneficiaries (mobile exit, d ≈ 0.25) — they gain competence and readiness but can exit the profession. Embedded community members are mixed beneficiaries (constrained exit, d ≈ 0.45) — they benefit from validated capability but bear costs and cannot easily exit. The analytical observer (analytical exit, d ≈ 0.72) sees the full structure from outside and recognizes the coordination function. The institutional civil defense system (arbitrage exit, d ≈ 0.10) benefits directly from generational knowledge transfer. No single agent experiences this constraint as pure extraction because the coordination function is genuine. This differentiates the competence reading from sibling readings (husk_reading, which emphasizes degradation toward pure theater; hybrid_reading, which treats competence and compliance as equally weighted).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_vs_compliance_boundary,
    'What observable signals distinguish genuine competence re-validation (rope, low theater) from bureaucratic compliance performance (piton, high theater) in a single drill event?',
    'Post-drill analysis: (a) debriefs identify novel scenarios or unexpected failure modes discovered during drill; (b) follow-up actions are documented and implemented; (c) inspector findings reference adaptive responses, not just checklist completion; (d) participant interviews reveal learning and decision-making reflection, not rote performance',
    'If boundary is clear and measurable: drills can be classified individually by theater_ratio. If boundary is ambiguous: classification requires longitudinal tracking to distinguish true competence maintenance from sustained theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_vs_compliance_boundary, empirical, 'Observable signals distinguishing competence re-validation from compliance theater').

omega_variable(
    generational_knowledge_transfer_mechanism,
    'Does competence actually transfer from experienced responders to new personnel through shared drills, or is structured training and apprenticeship the primary knowledge mechanism?',
    'Comparative analysis: (a) competence test results for newly hired responders in organizations with frequent intergenerational drills vs organizations with formal training only; (b) incident response quality post-incident correlation with prior drill participation; (c) qualitative interviews with responders about knowledge acquisition pathways',
    'If drills are primary mechanism: competence reading is correct — live practice is the irreplaceable knowledge vector. If formal training is sufficient: drills become supplementary, reducing the constraint''s claimed coordination necessity; classification shifts toward piton (maintenance through inertia rather than essential function).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(generational_knowledge_transfer_mechanism, empirical, 'Whether drills are primary mechanism for generational knowledge transfer').

omega_variable(
    kernel_reading_contestation_locus,
    'Is the disagreement between competence_reading and husk_reading located in empirical claims about knowledge transfer, normative claims about disaster preparedness responsibility, or institutional claims about whose authority validates competence?',
    'Structured comparison of the three sibling readings across three dimensions: (a) empirical claims about what drills accomplish (knowledge transfer, muscle memory, team cohesion, risk awareness); (b) normative claims about who has the duty to maintain preparedness and at what cost to other social goods; (c) institutional claims about which actors (inspectors, participants, administrators, community) have authority to declare preparedness adequate',
    'If empirical disagreement dominates: the readings are falsifiable competitors; evidence about competence transfer settles the contest. If normative disagreement dominates: no amount of evidence resolves it — the readings reflect different value commitments. If institutional disagreement dominates: the readings reflect authority structure competition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation_locus, conceptual, 'The structural locus of disagreement between competence and sibling readings').

omega_variable(
    novel_scenario_improvisation_measurement,
    'How much of the drill''s adaptive value comes from participants encountering scenarios they have seen before (pattern recognition) vs. novel scenario elements requiring real-time improvisation?',
    'Drill design audit: measure novelty content in each drill across (a) scenario parameters (location variation, cascading failures, resource scarcity patterns); (b) post-drill analysis of which decision points were novel vs. replicated from prior drills; (c) inspector notes on improvisation instances vs. scripted response execution',
    'If novelty is high (>60% novel elements per drill): competence reading is strong — improvisation is exercised. If novelty is low (<30%): drills function as muscle-memory reinforcement, closer to pure performance repetition (supports husk/piton readings).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(novel_scenario_improvisation_measurement, empirical, 'Proportion of drill content that is novel vs. familiar scenario elements').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_transmission__competence_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_competence_tr_t0, preparedness_transmission__competence_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(prep_competence_tr_t10, preparedness_transmission__competence_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(prep_competence_tr_t20, preparedness_transmission__competence_reading, theater_ratio, 20, 0.35).

% Extraction over time
narrative_ontology:measurement(prep_competence_be_t0, preparedness_transmission__competence_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(prep_competence_be_t10, preparedness_transmission__competence_reading, base_extractiveness, 10, 0.21).
narrative_ontology:measurement(prep_competence_be_t20, preparedness_transmission__competence_reading, base_extractiveness, 20, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(prep_competence_su_t0, preparedness_transmission__competence_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(prep_competence_su_t10, preparedness_transmission__competence_reading, suppression_requirement, 10, 0.18).
narrative_ontology:measurement(prep_competence_su_t20, preparedness_transmission__competence_reading, suppression_requirement, 20, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_transmission__competence_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(preparedness_transmission__competence_reading, preparedness_transmission__husk_reading).
narrative_ontology:affects_constraint(preparedness_transmission__competence_reading, preparedness_transmission__hybrid_reading).

% DUAL FORMULATION NOTE:
% The preparedness_transmission kernel has three structurally distinct constraint stories corresponding to three readings of what preparedness means and how it is maintained. The competence_reading (this story) emphasizes live practice and adaptive capacity (ε=0.22, low extraction). The husk_reading (sibling) emphasizes institutional inertia and compliance theater (ε=0.60+, high extraction). The hybrid_reading (sibling) treats both competence and compliance as essential and mixed (ε=0.40, tangled rope). Each reading generates different ε values and classifications because the observable (what makes a drill 'successful') differs across readings. The competence reading asks 'did participants improvise and learn novel failure signatures?' The husk reading asks 'was the compliance checklist completed?' The hybrid reading asks 'was both competence and compliance achieved?' These are not the same constraint viewed from different angles — they are different constraints instantiated by different interpretations of the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
