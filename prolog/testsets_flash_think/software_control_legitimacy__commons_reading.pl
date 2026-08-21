% ============================================================================
% CONSTRAINT STORY: software_control_legitimacy__commons_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_control_legitimacy__commons_reading, []).

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
 *   constraint_id: software_control_legitimacy__commons_reading
 *   human_readable: Software as Digital Commons Governance
 *   domain: software_engineering/political_economy_of_technology/intellectual_property
 *
 * SUMMARY:
 *   This constraint represents the 'commons reading' of software control,
 *   asserting that software is a shared digital infrastructure requiring
 *   negotiated collective management, rather than being subject to absolute
 *   freedom or absolute property rights. It frames software control as a
 *   governance question, where stakeholder communities are beneficiaries of
 *   collective stewardship, while advocates of absolutist positions (pure
 *   freedom or pure property) are victims whose claims are denied by the
 *   collective framework. The constraint is claimed as a Tangled Rope because
 *   it provides a genuine coordination function (managing the commons) but
 *   also extracts from those who prefer alternative, absolutist models.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_control_legitimacy__commons_reading, 0.35).
domain_priors:suppression_score(software_control_legitimacy__commons_reading, 0.45).
domain_priors:theater_ratio(software_control_legitimacy__commons_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_control_legitimacy__commons_reading, tangled_rope).
narrative_ontology:human_readable(software_control_legitimacy__commons_reading, "Software as Digital Commons Governance").
narrative_ontology:topic_domain(software_control_legitimacy__commons_reading, "software_engineering/political_economy_of_technology/intellectual_property").

domain_priors:requires_active_enforcement(software_control_legitimacy__commons_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_control_legitimacy__commons_reading, '1040dda5-8ed7-4310-8058-4d42c61bb8ec').
narrative_ontology:cs_kernel_codification('1040dda5-8ed7-4310-8058-4d42c61bb8ec', formalized).
narrative_ontology:cs_authority_grounding('1040dda5-8ed7-4310-8058-4d42c61bb8ec', practice).
narrative_ontology:cs_interpretation_layer_present('1040dda5-8ed7-4310-8058-4d42c61bb8ec').
narrative_ontology:cs_reading_relation('1040dda5-8ed7-4310-8058-4d42c61bb8ec', software_control_legitimacy__freedom_imperative_reading, forecloses).
narrative_ontology:cs_reading_relation('1040dda5-8ed7-4310-8058-4d42c61bb8ec', software_control_legitimacy__pragmatic_openness_reading, influences).
narrative_ontology:cs_reading_relation('1040dda5-8ed7-4310-8058-4d42c61bb8ec', software_control_legitimacy__property_rights_reading, forecloses).
narrative_ontology:cs_axiom('1040dda5-8ed7-4310-8058-4d42c61bb8ec', foundational, software_as_shared_resource).
narrative_ontology:cs_axiom_status(software_as_shared_resource, holdable).
narrative_ontology:cs_axiom_grounding('1040dda5-8ed7-4310-8058-4d42c61bb8ec', software_as_shared_resource, conventional).
narrative_ontology:cs_axiom('1040dda5-8ed7-4310-8058-4d42c61bb8ec', foundational, collective_stewardship_imperative).
narrative_ontology:cs_axiom_status(collective_stewardship_imperative, holdable).
narrative_ontology:cs_axiom_grounding('1040dda5-8ed7-4310-8058-4d42c61bb8ec', collective_stewardship_imperative, deontological).
narrative_ontology:cs_reference_frame('1040dda5-8ed7-4310-8058-4d42c61bb8ec', collective_stewardship_ideal).
narrative_ontology:cs_drift_state('1040dda5-8ed7-4310-8058-4d42c61bb8ec', contemporary_digital_economy, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('1040dda5-8ed7-4310-8058-4d42c61bb8ec', '').
narrative_ontology:cs_kernel_id(software_control_legitimacy__commons_reading, software_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_control_legitimacy__commons_reading, stakeholder_communities).
narrative_ontology:constraint_victim(software_control_legitimacy__commons_reading, absolute_freedom_advocates).
narrative_ontology:constraint_victim(software_control_legitimacy__commons_reading, absolute_property_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the shared infrastructure and collective decision-making that prevents fragmentation and ensures long-term sustainability. They participate in governance but must adhere to collectively agreed-upon rules, which can constrain individual action.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, stakeholder_communities, beneficiary,
    organized, generational, constrained, global).

% Adhere to the belief that software should be absolutely free, denying any form of control or restriction. They bear the cost of having their preferred model of unfettered individual autonomy suppressed by the collective governance framework, which they perceive as illegitimate.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, absolute_freedom_advocates, payer,
    moderate, biographical, identity_locked, global).

% Believe software creators have absolute property rights, allowing them to restrict use, modification, and distribution. They bear the cost of having their claims to exclusive control challenged and limited by the commons governance model, which requires negotiation and sharing.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, absolute_property_advocates, payer,
    powerful, biographical, constrained, global).

% Are the individuals or organizations responsible for facilitating, maintaining, and enforcing the rules and norms of the software commons. They mediate disputes, ensure adherence to licenses, and work to sustain the shared infrastructure, often facing resistance from absolutist positions.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, commons_stewards, agenda_setter,
    institutional, generational, constrained, global).

% Academics, researchers, and policy analysts who study the theoretical and practical implications of software commons, evaluating their effectiveness, fairness, and sustainability without direct participation in their governance or direct benefit/cost from their operation.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, analytical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for collective management of shared digital infrastructure, preventing the 'tragedy of the commons' by defining rules for contribution, use, and modification, thereby ensuring long-term sustainability and equitable access.
% TRANSFER_FUNCTION: Transfers decision-making authority from individual developers or corporations to collective governance bodies; transfers resources (code, documentation, maintenance effort) into shared pools; and transfers the costs of fragmented, conflicting individualistic approaches into the overhead of collective stewardship.
% ABSENT_VOICES: Those who insist on absolute freedom or absolute property rights for software are conceptually and often practically excluded from the governance discussions of a software commons. They would object to any collective management as an infringement on their fundamental principles.
% DISAPPEARANCE_RATIONALE: If the concept and practice of software as a commons vanished, digital infrastructure would likely revert to either proprietary monopolies or fragmented, unsustainable 'free-for-all' projects, losing the benefits of collective stewardship, shared innovation, and equitable access. The digital economy would fundamentally reorganize.
% FOUNDING_PROBLEM: The historical tension between proprietary control (leading to monopolies and vendor lock-in) and absolute freedom (leading to fragmentation and unsustainable projects) created a need for a third way to manage shared digital resources, ensuring both innovation and public benefit.
% FOUNDING_PROBLEM_CORROBORATION: Academic research in commons theory, the ongoing success of many open-source projects with strong governance models, and persistent policy debates around digital public infrastructure corroborate the continued relevance of this problem and the need for collective management. This is attested by independent scholars and policy makers, not just the direct beneficiaries.
narrative_ontology:disappearance_verdict(software_control_legitimacy__commons_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_control_legitimacy__commons_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_control_legitimacy__commons_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(software_control_legitimacy__commons_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_control_legitimacy__commons_reading, 0.35, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_control_legitimacy__commons_reading_tests).
:- end_tests(software_control_legitimacy__commons_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.35) reflects the inherent cost of participation in a commons and the 'extraction' from those who would prefer unfettered individual action or absolute control. Suppression (0.45) is moderate, representing the active enforcement of commons rules against competing claims, rather than outright coercion. Resistance (0.65) is high due to the ongoing ideological contestation from absolutist positions. The low theater ratio (0.15) indicates that the governance function is largely genuine and not performative. The claimed type is Tangled Rope because it coordinates shared resources for collective benefit (beneficiaries: stakeholder_communities) but also imposes costs and limits on those who advocate for absolute freedom or property rights (victims: absolute_freedom_advocates, absolute_property_advocates), requiring active enforcement of its rules.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of stakeholder communities, the constraint is a beneficial coordination mechanism. From the perspective of absolutist advocates, it is an extractive imposition that denies fundamental rights or property claims. The engine will compute these divergent classifications based on the declared roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Stakeholder communities are beneficiaries (low d) as they gain from the stability and shared resources of the commons. Absolute freedom and property advocates are targets (high d) because the commons framework directly limits their preferred modes of action, imposing costs on their ideological positions. Commons stewards are agenda-setters, balancing coordination with enforcement, and thus have a more symmetric directionality (moderate d). Analytical observers are outside the direct flow of costs and benefits (analytical d).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    commons_rules_legitimacy,
    'Are the specific rules and enforcement mechanisms of a given software commons perceived as legitimate by all participants, or are they seen as arbitrary impositions?',
    'Surveys of participant satisfaction, analysis of governance disputes, and rates of compliance/defection within specific commons projects.',
    'If rules are widely seen as illegitimate, the constraint''s effective extractiveness and suppression are higher than measured, indicating a drift towards a Snare. If legitimate, it reinforces the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commons_rules_legitimacy, empirical, 'Perceived legitimacy of commons governance rules.').

omega_variable(
    boundary_of_the_commons,
    'What constitutes ''shared digital infrastructure'' versus purely private or individual software, and where should the boundary for commons governance be drawn?',
    'Ongoing legal and policy debates, and the evolution of community norms around new software technologies (e.g., AI models, decentralized protocols).',
    'A narrower definition of ''commons'' would reduce the scope of this constraint, potentially reclassifying some software under property rights or freedom imperatives. A broader definition would expand its scope and increase its perceived extractiveness for those outside the commons framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(boundary_of_the_commons, conceptual, 'Ambiguity in defining the scope of software as a commons.').

omega_variable(
    absolutist_exit_viability,
    'How viable are genuine exit options for advocates of absolute freedom or property rights to create alternative, non-commons-governed software ecosystems?',
    'Analysis of market share, user adoption, and developer participation in non-commons software projects over time.',
    'If viable alternatives exist and thrive, the ''trapped'' or ''constrained'' exit options for absolutist advocates are overstated, reducing their effective extraction. If alternatives consistently fail, it reinforces the current assessment of high extraction from these groups.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(absolutist_exit_viability, empirical, 'Viability of non-commons software ecosystems.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_control_legitimacy__commons_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_control_legitimacy__commons_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(soft_tr_t4, software_control_legitimacy__commons_reading, theater_ratio, 4, 0.14).
narrative_ontology:measurement(soft_tr_t8, software_control_legitimacy__commons_reading, theater_ratio, 8, 0.15).
narrative_ontology:measurement(soft_tr_t12, software_control_legitimacy__commons_reading, theater_ratio, 12, 0.16).
narrative_ontology:measurement(soft_tr_t16, software_control_legitimacy__commons_reading, theater_ratio, 16, 0.15).
narrative_ontology:measurement(soft_tr_t20, software_control_legitimacy__commons_reading, theater_ratio, 20, 0.15).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_control_legitimacy__commons_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(soft_be_t4, software_control_legitimacy__commons_reading, base_extractiveness, 4, 0.32).
narrative_ontology:measurement(soft_be_t8, software_control_legitimacy__commons_reading, base_extractiveness, 8, 0.34).
narrative_ontology:measurement(soft_be_t12, software_control_legitimacy__commons_reading, base_extractiveness, 12, 0.35).
narrative_ontology:measurement(soft_be_t16, software_control_legitimacy__commons_reading, base_extractiveness, 16, 0.36).
narrative_ontology:measurement(soft_be_t20, software_control_legitimacy__commons_reading, base_extractiveness, 20, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t0, software_control_legitimacy__commons_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(soft_su_t4, software_control_legitimacy__commons_reading, suppression_requirement, 4, 0.42).
narrative_ontology:measurement(soft_su_t8, software_control_legitimacy__commons_reading, suppression_requirement, 8, 0.44).
narrative_ontology:measurement(soft_su_t12, software_control_legitimacy__commons_reading, suppression_requirement, 12, 0.45).
narrative_ontology:measurement(soft_su_t16, software_control_legitimacy__commons_reading, suppression_requirement, 16, 0.46).
narrative_ontology:measurement(soft_su_t20, software_control_legitimacy__commons_reading, suppression_requirement, 20, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_control_legitimacy__commons_reading, resource_allocation).
narrative_ontology:affects_constraint(software_control_legitimacy__commons_reading, open_source_licensing_compliance).
narrative_ontology:affects_constraint(software_control_legitimacy__commons_reading, digital_rights_management_legitimacy).
narrative_ontology:affects_constraint(software_control_legitimacy__commons_reading, software_control_legitimacy__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__commons_reading, software_control_legitimacy__pragmatic_openness_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__commons_reading, software_control_legitimacy__property_rights_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'software_control_legitimacy' kernel, which decomposes into multiple structurally distinct claims about how software should be governed. This 'commons_reading' focuses on collective management, while sibling readings emphasize absolute freedom, pragmatic openness, or property rights. Each reading has a distinct ε value and stakeholder structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
