% ============================================================================
% CONSTRAINT STORY: software_control_legitimacy__freedom_imperative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_control_legitimacy__freedom_imperative_reading, []).

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
 *   constraint_id: software_control_legitimacy__freedom_imperative_reading
 *   human_readable: Proprietary Software as Denial of User Freedom (Freedom Imperative Reading)
 *   domain: software_engineering/political_economy_of_technology/intellectual_property
 *
 * SUMMARY:
 *   This constraint represents the 'freedom imperative' reading of software
 *   control legitimacy. It asserts that proprietary software is ethically
 *   illegitimate because it denies users fundamental control over their
 *   computing. This reading frames proprietary software as a 'snare' that
 *   extracts user freedom and autonomy, requiring active resistance. The high
 *   extractiveness reflects the categorical rejection of closed-source models
 *   as inherently harmful to user rights. The high suppression reflects the
 *   pervasive nature of proprietary software and the difficulty of escaping
 *   its influence.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_control_legitimacy__freedom_imperative_reading, 0.9).
domain_priors:suppression_score(software_control_legitimacy__freedom_imperative_reading, 0.7).
domain_priors:theater_ratio(software_control_legitimacy__freedom_imperative_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, extractiveness, 0.9).
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_control_legitimacy__freedom_imperative_reading, snare).
narrative_ontology:human_readable(software_control_legitimacy__freedom_imperative_reading, "Proprietary Software as Denial of User Freedom (Freedom Imperative Reading)").
narrative_ontology:topic_domain(software_control_legitimacy__freedom_imperative_reading, "software_engineering/political_economy_of_technology/intellectual_property").

domain_priors:requires_active_enforcement(software_control_legitimacy__freedom_imperative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_control_legitimacy__freedom_imperative_reading, '140b4bbd-8581-4913-aac2-4af58bf51012').
narrative_ontology:cs_kernel_codification('140b4bbd-8581-4913-aac2-4af58bf51012', implicit).
narrative_ontology:cs_authority_grounding('140b4bbd-8581-4913-aac2-4af58bf51012', practice).
narrative_ontology:cs_interpretation_layer_present('140b4bbd-8581-4913-aac2-4af58bf51012').
narrative_ontology:cs_reading_relation('140b4bbd-8581-4913-aac2-4af58bf51012', software_control_legitimacy__pragmatic_openness_reading, influences).
narrative_ontology:cs_reading_relation('140b4bbd-8581-4913-aac2-4af58bf51012', software_control_legitimacy__property_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('140b4bbd-8581-4913-aac2-4af58bf51012', software_control_legitimacy__commons_reading, coexists_with).
narrative_ontology:cs_axiom('140b4bbd-8581-4913-aac2-4af58bf51012', foundational, user_freedom_is_absolute_in_computing).
narrative_ontology:cs_axiom_status(user_freedom_is_absolute_in_computing, holdable).
narrative_ontology:cs_axiom_grounding('140b4bbd-8581-4913-aac2-4af58bf51012', user_freedom_is_absolute_in_computing, deontological).
narrative_ontology:cs_axiom('140b4bbd-8581-4913-aac2-4af58bf51012', foundational, proprietary_software_is_inherently_unethical).
narrative_ontology:cs_axiom_status(proprietary_software_is_inherently_unethical, holdable).
narrative_ontology:cs_axiom_grounding('140b4bbd-8581-4913-aac2-4af58bf51012', proprietary_software_is_inherently_unethical, deontological).
narrative_ontology:cs_reference_frame('140b4bbd-8581-4913-aac2-4af58bf51012', fully_free_computing_environment).
narrative_ontology:cs_drift_state('140b4bbd-8581-4913-aac2-4af58bf51012', contemporary_digital_ecosystem, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('140b4bbd-8581-4913-aac2-4af58bf51012', '').
narrative_ontology:cs_kernel_id(software_control_legitimacy__freedom_imperative_reading, software_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_control_legitimacy__freedom_imperative_reading, users_as_rights_holders).
narrative_ontology:constraint_victim(software_control_legitimacy__freedom_imperative_reading, proprietary_software_users).
narrative_ontology:constraint_victim(software_control_legitimacy__freedom_imperative_reading, proprietary_software_developers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These users assert a fundamental right to control their computing, viewing proprietary software as a violation of this freedom. They benefit from the philosophical clarity and moral high ground of this position, even if practical alternatives are limited.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, users_as_rights_holders, beneficiary,
    organized, generational, identity_locked, global).

% Users of proprietary software are seen as victims of a system that denies them control over their own tools. They pay with their freedom and autonomy, often without realizing the full extent of the control they cede. Exiting means abandoning widely used and often essential software.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, proprietary_software_users, payer,
    powerless, biographical, constrained, global).

% Developers of proprietary software are seen as perpetuating an unethical system, even if they operate within legal frameworks. They bear the moral opprobrium from this reading and face pressure to adopt open-source models, but their business models are built on proprietary control.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, proprietary_software_developers, payer,
    powerful, biographical, constrained, global).

% These advocates actively promote the idea that software control is a fundamental freedom. They set the agenda for ethical computing and work to educate users and developers about the perceived harms of proprietary software. Their identity is deeply intertwined with this imperative.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, free_software_advocates, agenda_setter,
    organized, generational, identity_locked, global).

% These legal professionals operate within a framework that recognizes proprietary rights in software. Their arguments for property protection are largely dismissed or reframed as mechanisms of control by the freedom imperative reading, placing them outside the core ethical discourse.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, intellectual_property_lawyers, excluded,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This reading coordinates a community around a shared ethical stance on software, fostering collaboration on free software projects and collective resistance against proprietary models.
% TRANSFER_FUNCTION: It transfers moral legitimacy and user loyalty away from proprietary software and towards free software, effectively extracting social capital from closed-source developers and granting it to open-source advocates.
% ABSENT_VOICES: Proprietary software developers and intellectual property rights advocates are largely excluded from the core ethical discourse of this reading, their arguments for commercial viability or creator rights being reframed as justifications for user subjugation.
% DISAPPEARANCE_RATIONALE: If the 'freedom imperative' reading vanished, the ethical landscape of software would fundamentally shift. The moral pressure against proprietary software would dissipate, potentially leading to less resistance to closed-source models and a re-evaluation of user rights in computing.
% FOUNDING_PROBLEM: The problem this reading was built to solve is the perceived denial of user freedom and autonomy by proprietary software, which restricts users' ability to study, modify, distribute, and run software as they wish.
% FOUNDING_PROBLEM_CORROBORATION: Free software foundations and numerous academic papers corroborate the ongoing nature of this problem, citing continued restrictions on user rights by proprietary vendors. Proprietary developers dispute the framing of 'denial of freedom,' asserting legitimate property rights.
narrative_ontology:disappearance_verdict(software_control_legitimacy__freedom_imperative_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_control_legitimacy__freedom_imperative_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_control_legitimacy__freedom_imperative_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(software_control_legitimacy__freedom_imperative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_control_legitimacy__freedom_imperative_reading, 0.9, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_control_legitimacy__freedom_imperative_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(software_control_legitimacy__freedom_imperative_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(software_control_legitimacy__freedom_imperative_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is high because this reading views any restriction on user control as a fundamental ethical violation, making proprietary software inherently extractive of freedom. Suppression is high because proprietary software is ubiquitous and often necessary for daily life, making exit difficult for users. Resistance is also high, reflecting the active advocacy and development efforts within the free software movement. The theater ratio is low as the ethical claims are direct and not performative.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of free software advocates, this is a clear ethical imperative. From the perspective of proprietary software developers, it is an ideological attack on their legitimate business models. The engine's classification will highlight this divergence, showing a 'snare' from the perspective of those who believe in the freedom imperative, while other readings might classify proprietary software differently.
 *
 * DIRECTIONALITY LOGIC:
 *   Users who identify with the 'freedom imperative' are beneficiaries of this reading's moral framework, gaining a clear ethical stance and a community. Proprietary software users are victims, as they are seen as having their freedom extracted. Proprietary software developers are also victims, as their work is deemed ethically illegitimate. Free software advocates act as agenda-setters, actively shaping and enforcing this ethical view. Intellectual property lawyers are excluded, as their framework is fundamentally at odds with this reading's core premise.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_freedom_definition,
    'What constitutes ''control'' and ''freedom'' in the context of software? Does it extend to hardware, network services, or only the local execution of code?',
    'Philosophical consensus on the boundaries of digital autonomy, or legal precedent establishing the scope of user rights in computing.',
    'A narrower definition of freedom might reduce the perceived extractiveness of some proprietary software, while a broader definition would amplify it, potentially reclassifying more systems as snares.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scope_of_freedom_definition, conceptual, 'Ambiguity in the definition of ''user freedom'' in computing.').

omega_variable(
    practical_alternatives_viability,
    'To what extent do viable, truly free software alternatives exist for all essential computing tasks, making exit from proprietary systems genuinely feasible?',
    'Empirical assessment of the completeness, usability, and adoption rates of free software ecosystems across various domains (e.g., operating systems, productivity suites, specialized tools).',
    'If alternatives are widely viable, the ''suppression'' metric might be lower, as users are less ''trapped.'' If alternatives are scarce or impractical, suppression remains high, reinforcing the snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(practical_alternatives_viability, empirical, 'The actual availability and practicality of free software alternatives.').

omega_variable(
    internalized_suppression_of_proprietary_users,
    'Is the suppression experienced by proprietary software users primarily structural (lack of alternatives, network effects) or internalized (lack of awareness, belief in proprietary necessity)?',
    'Post-education user behavior: if users continue to choose proprietary software after being fully informed of free alternatives and the ''freedom imperative'' argument, it suggests internalized factors or a different preference ordering.',
    'If internalized suppression is significant, the constraint''s effective suppression is higher than the structural measure suggests, as users carry the suppression with them even when structural barriers are theoretically lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_suppression_of_proprietary_users, empirical, 'Structural vs. internalized suppression mechanism for proprietary software users.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_control_legitimacy__freedom_imperative_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(soft_tr_t10, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(soft_tr_t20, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(soft_tr_t30, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement(soft_tr_t40, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 40, 0.1).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 0, 0.85).
narrative_ontology:measurement(soft_be_t10, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 10, 0.87).
narrative_ontology:measurement(soft_be_t20, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 20, 0.88).
narrative_ontology:measurement(soft_be_t30, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 30, 0.89).
narrative_ontology:measurement(soft_be_t40, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 40, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t0, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(soft_su_t10, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 10, 0.67).
narrative_ontology:measurement(soft_su_t20, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(soft_su_t30, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 30, 0.69).
narrative_ontology:measurement(soft_su_t40, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 40, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_control_legitimacy__freedom_imperative_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
