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
 *   human_readable: Software Control as Commons Governance (Commons Reading)
 *   domain: software_engineering/political_economy/intellectual_property
 *
 * SUMMARY:
 *   This constraint models the 'commons reading' of software control, where
 *   digital infrastructure is viewed as a shared resource requiring
 *   collective governance, rather than absolute freedom or absolute property.
 *   It seeks to establish a balance, making both extreme 'freedom' and
 *   'property' advocates bear the cost of compromise. The claimed type is
 *   'rope' because it aims for genuine coordination and mutual benefit
 *   through negotiated rules, with moderate extraction from those who resist
 *   collective management.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_control_legitimacy__commons_reading, 0.35).
domain_priors:suppression_score(software_control_legitimacy__commons_reading, 0.2).
domain_priors:theater_ratio(software_control_legitimacy__commons_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(software_control_legitimacy__commons_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_control_legitimacy__commons_reading, rope).
narrative_ontology:human_readable(software_control_legitimacy__commons_reading, "Software Control as Commons Governance (Commons Reading)").
narrative_ontology:topic_domain(software_control_legitimacy__commons_reading, "software_engineering/political_economy/intellectual_property").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_control_legitimacy__commons_reading, '3556eb14-4726-4d2a-a05a-cc4e3d003647').
narrative_ontology:cs_kernel_codification('3556eb14-4726-4d2a-a05a-cc4e3d003647', distributed).
narrative_ontology:cs_authority_grounding('3556eb14-4726-4d2a-a05a-cc4e3d003647', practice).
narrative_ontology:cs_interpretation_layer_present('3556eb14-4726-4d2a-a05a-cc4e3d003647').
narrative_ontology:cs_reading_relation('3556eb14-4726-4d2a-a05a-cc4e3d003647', software_control_legitimacy__freedom_imperative_reading, coexists_with).
narrative_ontology:cs_reading_relation('3556eb14-4726-4d2a-a05a-cc4e3d003647', software_control_legitimacy__pragmatic_openness_reading, coexists_with).
narrative_ontology:cs_reading_relation('3556eb14-4726-4d2a-a05a-cc4e3d003647', software_control_legitimacy__property_rights_reading, coexists_with).
narrative_ontology:cs_axiom('3556eb14-4726-4d2a-a05a-cc4e3d003647', foundational, digital_resources_are_shared_commons).
narrative_ontology:cs_axiom_status(digital_resources_are_shared_commons, holdable).
narrative_ontology:cs_axiom_grounding('3556eb14-4726-4d2a-a05a-cc4e3d003647', digital_resources_are_shared_commons, conventional).
narrative_ontology:cs_axiom('3556eb14-4726-4d2a-a05a-cc4e3d003647', foundational, collective_governance_ensures_sustainability).
narrative_ontology:cs_axiom_status(collective_governance_ensures_sustainability, holdable).
narrative_ontology:cs_axiom_grounding('3556eb14-4726-4d2a-a05a-cc4e3d003647', collective_governance_ensures_sustainability, instrumental).
narrative_ontology:cs_reference_frame('3556eb14-4726-4d2a-a05a-cc4e3d003647', sustainable_collective_management).
narrative_ontology:cs_drift_state('3556eb14-4726-4d2a-a05a-cc4e3d003647', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('3556eb14-4726-4d2a-a05a-cc4e3d003647', '').
narrative_ontology:cs_kernel_id(software_control_legitimacy__commons_reading, software_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_control_legitimacy__commons_reading, stakeholder_communities).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__commons_reading, digital_infrastructure_users).
narrative_ontology:constraint_victim(software_control_legitimacy__commons_reading, absolute_freedom_advocates).
narrative_ontology:constraint_victim(software_control_legitimacy__commons_reading, absolute_property_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participates in the collective management and governance of shared digital infrastructure, negotiating rules for access, modification, and distribution. Benefits from sustainable, collectively managed resources but bears the cost of ongoing governance.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, stakeholder_communities, agenda_setter,
    organized, generational, constrained, global).

% Benefits from stable, well-governed digital commons that provide reliable and accessible software. Contributes to the commons through use and feedback, but is not directly involved in governance decisions unless organized.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, digital_infrastructure_users, beneficiary,
    moderate, biographical, mobile, global).

% Views any restriction on software use or modification as an infringement on fundamental freedom. Bears the cost of accepting negotiated rules and collective management, which they perceive as a limitation on their autonomy.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, absolute_freedom_advocates, payer,
    moderate, generational, identity_locked, global).

% Asserts creators' absolute property rights over software, including the right to restrict use and distribution. Bears the cost of participating in a commons model that limits their exclusive control and requires negotiation.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, absolute_property_advocates, payer,
    powerful, generational, identity_locked, global).

% Views open source as a superior development methodology but acknowledges proprietary models as legitimate. Observes the debate from a position of practical efficacy, potentially aligning with commons governance if it yields better outcomes.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__commons_reading, pragmatic_openness_advocates, observer,
    organized, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for collective management of shared digital infrastructure, balancing individual freedoms with collective needs to ensure sustainability and equitable access, preventing 'tragedy of the commons' or 'enclosure'.
% TRANSFER_FUNCTION: Transfers decision-making authority from individual creators or users to a collective governance body, and transfers the benefits of shared, stable infrastructure to all participants, while imposing the cost of compromise on absolutist positions.
% ABSENT_VOICES: The voices of future generations of digital citizens, who will inherit the digital infrastructure, are implicitly represented by the long-term sustainability goals of commons governance, but are not directly present in current negotiations.
% DISAPPEARANCE_RATIONALE: If the commons governance framework disappeared, the debate would revert to absolutist positions, leading to either unchecked proprietary enclosure or chaotic 'free-for-all' scenarios, both of which would degrade the shared digital infrastructure and its utility.
% FOUNDING_PROBLEM: The problem of managing shared digital resources to prevent both monopolistic enclosure and chaotic degradation, ensuring long-term sustainability and equitable access for all users and contributors.
% FOUNDING_PROBLEM_CORROBORATION: Academic research in commons theory, historical examples of successful and failed commons management, and ongoing debates in digital rights and intellectual property law corroborate the persistent nature of this problem, independent of any single stakeholder's self-interest.
narrative_ontology:disappearance_verdict(software_control_legitimacy__commons_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_control_legitimacy__commons_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_control_legitimacy__commons_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   Extractiveness (0.35) is moderate, representing the cost of participation and compromise for those who prefer absolutist positions. Suppression (0.20) is low, as the constraint relies on negotiated agreement and shared understanding rather than coercion, though it does suppress extreme positions. Theater ratio (0.10) is low, indicating that the governance efforts are genuinely functional. Accessibility collapse (0.40) is moderate, as it limits the 'absolute' options but creates new avenues for participation. Resistance (0.30) is also moderate, reflecting ongoing ideological friction from absolutist camps.
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap is between the 'commons' view and the absolutist 'freedom' or 'property' views. From the commons perspective, the constraint is a necessary coordination mechanism. From the absolutist perspectives, it is an illegitimate imposition that extracts their 'rights' or 'control'. The engine's per-seat classification should reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Stakeholder communities and digital infrastructure users are beneficiaries, gaining from stable, managed commons. Absolute freedom and property advocates are 'victims' in the sense that their preferred maximalist positions are curtailed by the collective governance, making them bear the cost of compromise. Pragmatic openness advocates are observers, as their position is more about methodology than fundamental rights, and they may align with the commons if it proves effective.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is designed to prevent mandatrophy by actively adapting governance rules to evolving digital infrastructure needs. Its legitimacy is tied to its ongoing ability to manage the commons effectively, rather than a fixed, outdated mandate. The 'contested' status of the founding problem reflects the ongoing nature of this challenge.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    governance_effectiveness_ambiguity,
    'How effective is the collective governance mechanism in practice at preventing both enclosure and degradation of the digital commons?',
    'Empirical studies of specific digital commons projects, tracking metrics like participation rates, resource sustainability, and conflict resolution success.',
    'If governance is ineffective, the constraint may drift towards a ''snare'' (if captured by powerful interests) or ''piton'' (if it becomes performative). If highly effective, it reinforces the ''rope'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(governance_effectiveness_ambiguity, empirical, 'Uncertainty about the practical efficacy of commons governance in software.').

omega_variable(
    absolutist_resistance_threshold,
    'At what point does the ''cost of compromise'' for absolutist advocates become so high that they actively disengage or fork, rather than participate in commons governance?',
    'Observing participation rates and fork events in actual digital commons projects, correlated with the stringency of governance rules.',
    'If the threshold is low, the commons reading may struggle to maintain broad participation, potentially leading to fragmentation or a shift towards a more coercive ''tangled_rope'' to enforce participation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(absolutist_resistance_threshold, empirical, 'The point at which absolutist positions find commons governance intolerable.').

omega_variable(
    kernel_framing_choice,
    'Is the ''software_control_legitimacy'' kernel best framed as a commons governance problem, or is one of the sibling readings (freedom, property, pragmatism) a more accurate or productive framing?',
    'Conceptual analysis and philosophical debate, assessing which framing best accounts for the observed dynamics of software development, distribution, and use, and which leads to more just and sustainable outcomes.',
    'Adopting a different framing would lead to a different constraint classification and a different set of beneficiaries/victims, as the core problem definition shifts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_choice, conceptual, 'The fundamental conceptual choice of how to frame software control.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_control_legitimacy__commons_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_control_legitimacy__commons_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(soft_tr_t5, software_control_legitimacy__commons_reading, theater_ratio, 5, 0.09).
narrative_ontology:measurement(soft_tr_t10, software_control_legitimacy__commons_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(soft_tr_t15, software_control_legitimacy__commons_reading, theater_ratio, 15, 0.1).
narrative_ontology:measurement(soft_tr_t20, software_control_legitimacy__commons_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_control_legitimacy__commons_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(soft_be_t5, software_control_legitimacy__commons_reading, base_extractiveness, 5, 0.32).
narrative_ontology:measurement(soft_be_t10, software_control_legitimacy__commons_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(soft_be_t15, software_control_legitimacy__commons_reading, base_extractiveness, 15, 0.34).
narrative_ontology:measurement(soft_be_t20, software_control_legitimacy__commons_reading, base_extractiveness, 20, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t0, software_control_legitimacy__commons_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(soft_su_t5, software_control_legitimacy__commons_reading, suppression_requirement, 5, 0.19).
narrative_ontology:measurement(soft_su_t10, software_control_legitimacy__commons_reading, suppression_requirement, 10, 0.2).
narrative_ontology:measurement(soft_su_t15, software_control_legitimacy__commons_reading, suppression_requirement, 15, 0.2).
narrative_ontology:measurement(soft_su_t20, software_control_legitimacy__commons_reading, suppression_requirement, 20, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_control_legitimacy__commons_reading, resource_allocation).
narrative_ontology:affects_constraint(software_control_legitimacy__commons_reading, software_control_legitimacy__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__commons_reading, software_control_legitimacy__pragmatic_openness_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__commons_reading, software_control_legitimacy__property_rights_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the 'software_control_legitimacy' kernel. Each reading offers a distinct structural interpretation of software control, leading to different classifications and stakeholder dynamics. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
