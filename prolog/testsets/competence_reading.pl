% ============================================================================
% CONSTRAINT STORY: competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: competence_reading
 *   human_readable: Live Exercised Knowledge Maintained Through Active Practice and Knowledge Transfer
 *   domain: infrastructure_governance/disaster_preparedness/institutional_memory
 *
 * SUMMARY:
 *   This constraint models the institutional mechanism by which disaster
 *   preparedness organizations maintain operational competence through active
 *   practice, drills, inspections, and formal knowledge transfer programs.
 *   The constraint is ONE reading of a contested kernel
 *   (preparedness_retention) — the competence_reading assumes that
 *   institutional memory CAN be created and maintained through systematic
 *   mechanisms; the sibling husk_reading assumes that institutional memory is
 *   largely illusory and true competence is locked in individual personnel
 *   who cannot be fully replaced by transfer mechanisms. The
 *   competence_reading is the hypothesis that preparedness infrastructure
 *   evolves and improves when organizations invest in knowledge transfer (IHE
 *   Delft's 'second generation' model of disaster experience transmission,
 *   formal disaster simulation, codified response protocols, and systematic
 *   mentorship). The constraint exhibits a perspectival presheaf: frontline
 *   responders experience snare (knowledge maintenance burden with no exit);
 *   regional authorities experience tangled rope (genuine coordination
 *   benefit plus resource extraction burden); knowledge transfer institutions
 *   experience pure rope (coordination function); international standards
 *   bodies experience scaffold (systematizing knowledge to reduce dependence
 *   on continuous practice); the drill system itself appears as piton
 *   (performative ritual maintained by inertia); and from a civilizational
 *   view, the constraint risks appearing as mountain (natural law of
 *   expertise) — though structural data contradicts the natural-law framing.
 *
 * KEY AGENTS:
 *   - Frontline Responders (Disaster Response Teams): Trapped in continuous knowledge maintenance demands (drills, training, inspections) with career incentives tied to competence certification. Primary victim experience.
 *   - Regional Preparedness Authorities (Government Agencies, Emergency Management Offices): Moderate power actors bearing resource costs of knowledge maintenance while also benefiting from improved response capacity. Mixed extraction/coordination experience.
 *   - Knowledge Transfer Institutions (Universities, Training Centers, IHE Delft model programs): Institutional beneficiaries experiencing the constraint as pure coordination — transferring lived experience to new cohorts solves epistemic succession problem.
 *   - International Standards Coalition (ISO, UN agencies, certification bodies): Organized actors systematizing knowledge transfer through protocols and standards — creating scaffolding to reduce continuous practice dependency.
 *   - Disaster Response Organization Leadership: Institutional actors with arbitrage options balancing knowledge maintenance costs against response capacity requirements.
 *   - Knowledge itself (as abstract collective good): Cannot exit or organize; bears the cost of competence decay and knowledge loss through institutional turnover.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_reading, 0.28).
domain_priors:suppression_score(competence_reading, 0.35).
domain_priors:theater_ratio(competence_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(competence_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(competence_reading, theater_ratio, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_reading, tangled_rope).
narrative_ontology:human_readable(competence_reading, "Live Exercised Knowledge Maintained Through Active Practice and Knowledge Transfer").
narrative_ontology:topic_domain(competence_reading, "infrastructure_governance/disaster_preparedness/institutional_memory").

domain_priors:requires_active_enforcement(competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_reading, 'f823f29b-5035-4b75-bbc9-21556525ac69').
narrative_ontology:cs_created_at('f823f29b-5035-4b75-bbc9-21556525ac69', '').
narrative_ontology:cs_kernel_codification('f823f29b-5035-4b75-bbc9-21556525ac69', distributed).
narrative_ontology:cs_authority_grounding('f823f29b-5035-4b75-bbc9-21556525ac69', practice).
narrative_ontology:cs_interpretation_layer_present('f823f29b-5035-4b75-bbc9-21556525ac69').
narrative_ontology:cs_kernel_id(competence_reading, preparedness_retention).
narrative_ontology:cs_reading_relation('f823f29b-5035-4b75-bbc9-21556525ac69', husk_reading, coexists_with).
narrative_ontology:cs_axiom('f823f29b-5035-4b75-bbc9-21556525ac69', foundational, institutional_knowledge_transfer_succeeds).
narrative_ontology:cs_axiom_status(institutional_knowledge_transfer_succeeds, holdable).
narrative_ontology:cs_axiom_grounding('f823f29b-5035-4b75-bbc9-21556525ac69', institutional_knowledge_transfer_succeeds, empirically_contingent).
narrative_ontology:cs_axiom('f823f29b-5035-4b75-bbc9-21556525ac69', secondary, institutional_memory_is_structural).
narrative_ontology:cs_axiom_status(institutional_memory_is_structural, holdable).
narrative_ontology:cs_axiom_grounding('f823f29b-5035-4b75-bbc9-21556525ac69', institutional_memory_is_structural, instrumental).
narrative_ontology:cs_reference_frame('f823f29b-5035-4b75-bbc9-21556525ac69', systematized_knowledge_transfer_framework).
narrative_ontology:cs_drift_state('f823f29b-5035-4b75-bbc9-21556525ac69', contemporary_digital_era, gap(practice_drift, substantial, true)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_reading, responding_organizations).
narrative_ontology:constraint_beneficiary(competence_reading, affected_populations).
narrative_ontology:constraint_victim(competence_reading, knowledge_maintenance_labor).
narrative_ontology:constraint_victim(competence_reading, competing_resource_demands).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FRONTLINE RESPONDER (SNARE) — Trapped in continuous knowledge maintenance demands with no exit option. Drills and knowledge transfer consume career time; response failures due to knowledge decay create personal liability. Suppression is high (no alternative career paths in specialized disaster response roles). Extraction flows directly to the organization's operational continuity at responder cost.
constraint_indexing:constraint_classification(competence_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: REGIONAL PREPAREDNESS AUTHORITY (TANGLED ROPE) — Experiences coordination benefit (drills, inspections, knowledge transfer genuinely improve response capacity) AND extraction burden (continuous resource demand, competing priorities, institutional pressure to maintain competence despite budget constraints). Moderate power and constrained exit: can shift emphasis but cannot abandon knowledge maintenance without accepting catastrophic risk.
constraint_indexing:constraint_classification(competence_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: KNOWLEDGE TRANSFER INSTITUTION (ROPE) — Institution like IHE Delft 'second generation' program experiences this as pure coordination: transferring lived disaster experience to new cohorts solves a genuine collective action problem. No extraction experienced; institutional prestige and organizational continuity flow from effective knowledge transfer. Arbitrage exit: can shift to related domains if preparedness emphasis declines.
constraint_indexing:constraint_classification(competence_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INTERNATIONAL STANDARDS COALITION (SCAFFOLD) — Organized actors (ISO, national disaster management agencies, UNISDR) are systematizing knowledge transfer through codified protocols, training certification, and digital knowledge repositories. This scaffolding reduces dependency on individual responder expertise by institutionalizing best practices. Mobile exit: as standards mature, organizations can reduce dependence on continuous drilling and informal knowledge transfer. Sunset logic: when digital and formal systems fully replace lived-experience knowledge transfer, the constraint dissolves.
constraint_indexing:constraint_classification(competence_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: PERFORMATIVE DRILL SYSTEM (PITON) — Institutional ritual of scheduled drills persists largely through regulatory requirement and organizational inertia. Theater ratio is elevated (0.42): many drills are procedurally conducted but do not generate genuine knowledge updates or vulnerability identification. The constraint is maintained because alternatives (continuous simulation, live-fire exercises, digital scenario testing) are not yet fully standardized. Theater is declining as digital training and simulation systems mature.
constraint_indexing:constraint_classification(competence_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, competence in complex systems (disaster response, emergency medicine, aviation) requires active maintenance through practice: knowledge decays without use, novel scenarios emerge, environmental conditions change. This perspective views the constraint as an immutable property of how human expertise functions. However, the constraint's structural data reveals alternative readings: systematized knowledge transfer, codified protocols, and digital simulation may reduce but not eliminate the practice requirement.
constraint_indexing:constraint_classification(competence_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(competence_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(competence_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(competence_reading, TR),
    TR >= 0.70.

:- end_tests(competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low-to-moderate. The competence_reading frame assumes that knowledge maintenance yields genuine operational improvements, not pure extraction. Responders face real costs (time, career risk), but organizations benefit from genuine capability gains. The extraction is primarily on frontline labor (opportunity cost of drill time, regulatory compliance burden) not on populations. Suppression (0.35): Moderate. Barriers to exit from knowledge maintenance include: career path dependence (disaster response roles are specialized, hard to pivot out), regulatory requirements (mandated training/certification), organizational liability (failures to maintain competence create legal exposure). But suppression is not total — responders can transition to other fields, and organizations can choose different risk tolerance. Theater ratio (0.42): Moderate. Drills and inspections have performative components (compliance theater, regulatory box-checking) but also functional components (genuine vulnerability testing, capability validation, knowledge retention). The competence_reading assumes the functional component is real and significant; the husk_reading would estimate much higher theater. The measured theater ratio of 0.42 reflects current state where ~42% of visible activity is performative and ~58% is functionally testing/updating competence. The declining trajectory (0.55 → 0.42 over interval) reflects increasing systematization and simulation reducing the need for live-drill theater.
 *
 * PERSPECTIVAL GAP:
 *   The gap between competence_reading and husk_reading runs through all perspectives. Frontline responders experience snare in competence_reading (knowledge maintenance burden is real, but improves actual response), yet would experience snare-to-mountain in husk_reading (burden with no functional gain — pure institutional theater). Regional authorities experience tangled_rope in competence_reading (mixed coordination and burden), but would experience piton in husk_reading (all activity is theater maintained by regulatory requirement). Knowledge transfer institutions experience rope in competence_reading (genuine knowledge transmission), but would experience false_summit / snare in husk_reading (theater of transmission masking inability to transfer true competence). The perspectival gap is not within a single observation point but between two readings of the same institutional kernel. Competence_reading assumes the gap can be bridged through systematic mechanisms; husk_reading assumes the gap is irreducible.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are responding_organizations and affected_populations: responders benefit from improved institutional capacity; populations benefit from better disaster response. Victims are knowledge_maintenance_labor (time cost, regulatory burden) and competing_resource_demands (funding diverted from other priorities). The derivation chain for each perspective: (1) Frontline responders: powerless/trapped → d ≈ 0.95 → high chi, experienced as extraction despite actual coordination function (they see the time cost, not the institutional benefit). (2) Regional authorities: moderate/constrained, mixed beneficiary-victim → d ≈ 0.55 → moderate chi, experienced as Tangled Rope (genuine coordination benefit visible, but resource burden also acute). (3) Knowledge transfer institutions: institutional/arbitrage, pure beneficiary → d ≈ 0.15 → low chi, experienced as Rope (coordination function dominant). (4) Standards coalition: organized/mobile, architect of sunset → d ≈ 0.40 → chi scaled down by exit options and organized power. (5) Drill system: institutional/arbitrage, inertial → d ≈ 0.10 → low chi but theater-driven classification. (6) Analytical observer: analytical/analytical → d ≈ 0.72 → canonical analytical directionality, risks naturalizing institutional choice (mountain framing).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by explicitly instantiating the kernel contest: competence_reading assumes institutional memory is creatable (Tangled Rope with some Rope characteristics, Scaffold sunset path); husk_reading would classify most of the same activities as Piton or false-summit Mountain (apparent structures masking irreducible personnel dependence). The mandatrophy is NOT resolvable within a single reading — it is the contested claim that defines the two readings. The competence_reading's classification (tangled_rope) derives from assuming knowledge transfer mechanisms work; the husk_reading's classification would derive from assuming they are theater. The framework resolves this by separating the readings into different constraint stories and allowing both to coexist, with the kernel contest routed through omega variables and cs_structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    knowledge_decay_mechanism,
    'Is the observed competence decline due to memory decay (natural cognitive process), skill atrophy (lack of practice), or structural knowledge loss (institutional departure of experienced personnel)?',
    'Longitudinal cognitive testing of individual responders vs institutional turnover rates; correlation between personal experience tenure and actual response performance; comparison of decay curves for different knowledge domains',
    'If primarily memory decay: constraint is close to natural law (Mountain plausible). If primarily skill atrophy: practice requirement is genuine (Mountain justified). If primarily institutional turnover: the constraint is structural/institutional (Tangled Rope confirmed) — knowledge resides in people, not systems.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(knowledge_decay_mechanism, empirical, 'Mechanism of competence decline: cognitive, skill-based, or institutional').

omega_variable(
    knowledge_transfer_effectiveness,
    'Do systematic knowledge transfer programs (IHE Delft model, mentorship, apprenticeship) actually transmit operational competence, or do they transmit only formal knowledge while competence remains locked in lived experience?',
    'Comparison of response performance between responders trained via transfer programs vs those with direct disaster experience; measurement of error rates, decision speed, and adaptation capacity in novel scenarios; longitudinal tracking of transferred-knowledge cohorts through actual deployments',
    'If transfer is effective: competence_reading (this constraint) is accurate — knowledge maintenance is achievable through institutional mechanisms. If transfer is ineffective: the husk_reading''s view prevails — competence cannot be transferred, only experienced, creating irreducible personnel lock-in.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(knowledge_transfer_effectiveness, empirical, 'Whether knowledge transfer programs successfully transmit operational competence').

omega_variable(
    simulation_adequacy_for_novel_scenarios,
    'Do digital simulations and scenario-based training prepare responders for genuinely novel disaster types, or do they reinforce pattern-matching to previously encountered scenarios?',
    'Analysis of response failures in unprecedented disasters (COVID-19 pandemic, novel hazard combinations); comparison of performance on trained scenarios vs untrained novel scenarios; measurement of adaptive capacity vs procedural compliance',
    'If simulations are adequate: scaffold perspective is validated — codified knowledge can substitute for continuous live practice. If inadequate: live practice remains necessary — the Mountain perspective is partially vindicated, though the constraint is still Tangled Rope rather than natural law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(simulation_adequacy_for_novel_scenarios, empirical, 'Whether simulations prepare responders for genuinely novel scenarios').

omega_variable(
    institutional_kernel_reading_contest,
    'Is the constraint defined by the competence_reading (knowledge maintenance through active practice and transfer IS possible and structured) or the husk_reading (live experience cannot be transferred; institutional memory is an illusion; only personnel continuity matters)?',
    'Organizational ethnography and disaster response performance data across institutions with different knowledge transfer philosophies. Institutional change: track whether organizations moving from experience-based to systematized knowledge maintenance show performance degradation or improvement. Counterfactual: what would competence look like if the competing reading were true?',
    'Competence_reading: drills, inspections, and knowledge transfer mechanisms are structurally necessary and functionally valuable — institutional memory is real. Husk_reading: these mechanisms create appearance of continuity while actual competence depends entirely on personnel retention — institutional memory is theater. The readings coexist because different organizations instantiate different versions of institutional knowledge (some systematized, some embedded), but they cannot both be fully true in a single framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_kernel_reading_contest, conceptual, 'Kernel contest: Can institutional memory be created through knowledge transfer, or is competence locked in individuals?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(comp_tr_t5, competence_reading, theater_ratio, 5, 0.48).
narrative_ontology:measurement(comp_tr_t10, competence_reading, theater_ratio, 10, 0.42).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(comp_be_t5, competence_reading, base_extractiveness, 5, 0.3).
narrative_ontology:measurement(comp_be_t10, competence_reading, base_extractiveness, 10, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_reading, attachment_coordination).
narrative_ontology:affects_constraint(competence_reading, husk_reading).
narrative_ontology:affects_constraint(competence_reading, organizational_succession_gap).
narrative_ontology:affects_constraint(competence_reading, disaster_simulation_adequacy).

% DUAL FORMULATION NOTE:
% The preparedness_retention kernel decomposes into (at minimum) two structurally distinct constraints: competence_reading (this file) assumes institutional knowledge transfer mechanisms create real continuity; husk_reading assumes institutional memory is largely theater and true continuity depends on personnel retention. These are not alternative observables of the same constraint — they are different claims about whether institutional mechanisms can solve the succession problem. Each story has its own ε, its own perspectives, and its own measurement profile. They are linked by network.affects_constraints (husk_reading downstream of competence_reading) because competence_reading's institutional mechanisms would need to fail for husk_reading's predictions to hold.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
