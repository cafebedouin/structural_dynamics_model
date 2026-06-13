% ============================================================================
% CONSTRAINT STORY: software_source_status__freedom_imperative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_source_status__freedom_imperative_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: software_source_status__freedom_imperative_reading
 *   human_readable: Software Freedom Imperative (Proprietary Software as Injustice)
 *   domain: software_engineering/political_economy_of_technology/intellectual_property
 *
 * SUMMARY:
 *   This constraint represents the 'freedom imperative' reading of software
 *   source status, where proprietary software is fundamentally an injustice.
 *   It asserts that users have an inalienable right to access and modify
 *   software source code, and any licensing restrictions are illegitimate
 *   constraints. This reading positions all proprietary software as
 *   extractive, trapping users and developers in systems that deny
 *   fundamental freedoms. The high extractiveness and suppression reflect the
 *   perceived ethical violation and the active enforcement of proprietary
 *   licenses that restrict user rights.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_source_status__freedom_imperative_reading, 0.9).
domain_priors:suppression_score(software_source_status__freedom_imperative_reading, 0.7).
domain_priors:theater_ratio(software_source_status__freedom_imperative_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, extractiveness, 0.9).
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_source_status__freedom_imperative_reading, snare).
narrative_ontology:human_readable(software_source_status__freedom_imperative_reading, "Software Freedom Imperative (Proprietary Software as Injustice)").
narrative_ontology:topic_domain(software_source_status__freedom_imperative_reading, "software_engineering/political_economy_of_technology/intellectual_property").

domain_priors:requires_active_enforcement(software_source_status__freedom_imperative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_source_status__freedom_imperative_reading, 'e3dd1696-d834-4288-b048-ddb9b5833be1').
narrative_ontology:cs_kernel_codification('e3dd1696-d834-4288-b048-ddb9b5833be1', implicit).
narrative_ontology:cs_authority_grounding('e3dd1696-d834-4288-b048-ddb9b5833be1', distributed).
narrative_ontology:cs_reading_relation('e3dd1696-d834-4288-b048-ddb9b5833be1', software_source_status__pragmatic_development_reading, coexists_with).
narrative_ontology:cs_reading_relation('e3dd1696-d834-4288-b048-ddb9b5833be1', software_source_status__property_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('e3dd1696-d834-4288-b048-ddb9b5833be1', software_source_status__utilitarian_hybrid_reading, forecloses).
narrative_ontology:cs_axiom('e3dd1696-d834-4288-b048-ddb9b5833be1', foundational, software_freedom_is_a_moral_imperative).
narrative_ontology:cs_axiom_status(software_freedom_is_a_moral_imperative, holdable).
narrative_ontology:cs_axiom_grounding('e3dd1696-d834-4288-b048-ddb9b5833be1', software_freedom_is_a_moral_imperative, deontological).
narrative_ontology:cs_axiom('e3dd1696-d834-4288-b048-ddb9b5833be1', foundational, proprietary_software_is_an_injustice).
narrative_ontology:cs_axiom_status(proprietary_software_is_an_injustice, holdable).
narrative_ontology:cs_axiom_grounding('e3dd1696-d834-4288-b048-ddb9b5833be1', proprietary_software_is_an_injustice, deontological).
narrative_ontology:cs_reference_frame('e3dd1696-d834-4288-b048-ddb9b5833be1', universal_software_commons).
narrative_ontology:cs_drift_state('e3dd1696-d834-4288-b048-ddb9b5833be1', contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('e3dd1696-d834-4288-b048-ddb9b5833be1', '').
narrative_ontology:cs_kernel_id(software_source_status__freedom_imperative_reading, software_source_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_source_status__freedom_imperative_reading, free_software_advocates).
narrative_ontology:constraint_beneficiary(software_source_status__freedom_imperative_reading, users_of_free_software).
narrative_ontology:constraint_victim(software_source_status__freedom_imperative_reading, proprietary_software_developers).
narrative_ontology:constraint_victim(software_source_status__freedom_imperative_reading, proprietary_software_users).
narrative_ontology:constraint_vindicates(software_source_status__freedom_imperative_reading, user_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(software_source_status__freedom_imperative_reading, digital_commons_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Promote the ethical imperative of software freedom, viewing proprietary software as an injustice. They actively campaign for legal and social changes to enforce this view, defining the terms of 'freedom' and 'injustice' within this reading.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, free_software_advocates, agenda_setter,
    organized, generational, mobile, global).

% Benefit from the availability of software that adheres to the four freedoms (run, study, redistribute, improve). They are empowered by access to source code and the ability to control their computing, aligning with the ethical principles of this reading.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, users_of_free_software, beneficiary,
    moderate, biographical, mobile, global).

% Are seen as perpetuating an injustice by creating and distributing proprietary software. From the perspective of this reading, their business model is ethically illegitimate, and they bear the moral and potential future legal costs of non-compliance with the freedom imperative.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, proprietary_software_developers, payer,
    powerful, biographical, constrained, global).

% Are victims of the injustice of proprietary software, as their freedom to use, study, share, and modify the software is denied. They are often locked into ecosystems by network effects and lack of alternatives, making exit difficult despite the perceived ethical violation.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, proprietary_software_users, payer,
    powerless, immediate, identity_locked, global).

% Represent the legal framework that enables proprietary software. Their arguments for property rights are fundamentally rejected by this reading's ethical stance, making them an excluded voice in the core debate over software's moral status.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, intellectual_property_lawyers, excluded,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a global movement around a shared ethical vision for software, fostering collaboration on free software projects and advocating for policies that align with user freedom.
% TRANSFER_FUNCTION: Transfers control and knowledge from software creators to users, asserting that source code and modification rights are inalienable. It seeks to transfer economic value from proprietary vendors to a digital commons.
% ABSENT_VOICES: Proprietary software developers and intellectual property lawyers are absent from the core ethical framing, as their foundational premises (e.g., software as private property) are deemed illegitimate by this reading. They would argue for creator rights and economic incentives.
% DISAPPEARANCE_RATIONALE: If the imperative for software freedom vanished, the ethical foundation for the free software movement would collapse. The moral argument against proprietary software would disappear, potentially leading to a shift in public perception and policy towards a more permissive view of proprietary models, fundamentally altering the digital landscape.
% FOUNDING_PROBLEM: The problem of users losing control over their computing due to proprietary software, leading to a lack of transparency, security vulnerabilities, and restrictions on sharing and modification.
% FOUNDING_PROBLEM_CORROBORATION: The Free Software Foundation and various digital rights organizations continually attest to the ongoing nature of this problem, citing new instances of proprietary lock-in and user disempowerment. Independent technologists and ethicists also corroborate the persistence of these issues, highlighting the societal implications of restricted software access.
narrative_ontology:disappearance_verdict(software_source_status__freedom_imperative_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_source_status__freedom_imperative_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_source_status__freedom_imperative_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(software_source_status__freedom_imperative_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_source_status__freedom_imperative_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(software_source_status__freedom_imperative_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(software_source_status__freedom_imperative_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is high (0.9) because proprietary software is seen as extracting fundamental freedoms from users, denying them control over their digital lives. Suppression (0.7) is also high, as legal frameworks and technical measures actively enforce proprietary licenses, preventing users from exercising their 'rights' to modify and share. The theater ratio is low (0.1) because the constraint is not seen as performative; its proponents genuinely believe in the ethical necessity of its enforcement. Resistance is high (0.8) due to the ongoing free software movement actively challenging proprietary models.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of this reading, proprietary software is a snare, actively extracting freedom and suppressing alternatives. However, from the perspective of a property rights reading, proprietary software is a legitimate expression of creator rights. The engine's classification will highlight this divergence based on the declared metrics and stakeholder positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Free software advocates and users of free software are beneficiaries, as this reading vindicates their ethical stance and empowers them. Proprietary software developers and users are victims, as their existence within the proprietary model is deemed an injustice. Intellectual property lawyers are excluded, as their foundational arguments are rejected by this ethical framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ethical_vs_legal_status,
    'Is the ''injustice'' of proprietary software an ethical claim, a legal claim, or both? What would be required to translate the ethical imperative into legal enforceability?',
    'Analysis of successful legal challenges or legislative changes that codify software freedom as a right, or a shift in international intellectual property treaties.',
    'If primarily an ethical claim, its ''extractiveness'' is moral rather than material, and its ''suppression'' is ideological. If it becomes legally enforceable, the constraint''s effective extractiveness and suppression would become directly material and coercive, potentially reclassifying it from a conceptual snare to a legal snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ethical_vs_legal_status, conceptual, 'Distinction between ethical and legal enforceability of software freedom.').

omega_variable(
    identity_lock_proprietary_users,
    'To what extent are proprietary software users ''identity_locked'' versus merely ''constrained'' by network effects and switching costs?',
    'Empirical studies on user behavior when presented with viable free software alternatives, measuring the psychological and social barriers to switching versus purely technical/economic ones.',
    'If identity-locked, the suppression is deeper and more internalized, making exit harder even if structural barriers are lowered. If merely constrained, policy interventions focused on reducing switching costs would be more effective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_proprietary_users, empirical, 'Nature of lock-in for proprietary software users.').

omega_variable(
    natural_law_vs_constructed_right,
    'Is software freedom an inherent natural right, or a constructed ethical principle that requires active societal enforcement?',
    'Philosophical debate and consensus within digital ethics, or the emergence of a global legal framework that universally recognizes software freedom as a fundamental human right.',
    'If a natural right, the ''injustice'' of proprietary software is a violation of a pre-existing moral order. If a constructed right, its persistence depends entirely on the ongoing advocacy and enforcement by its proponents, making it more vulnerable to shifts in societal values.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_right, conceptual, 'Grounding of software freedom as a right.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_source_status__freedom_imperative_reading, 1985, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t1985, software_source_status__freedom_imperative_reading, theater_ratio, 1985, 0.1).
narrative_ontology:measurement(soft_tr_t1995, software_source_status__freedom_imperative_reading, theater_ratio, 1995, 0.1).
narrative_ontology:measurement(soft_tr_t2005, software_source_status__freedom_imperative_reading, theater_ratio, 2005, 0.1).
narrative_ontology:measurement(soft_tr_t2015, software_source_status__freedom_imperative_reading, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(soft_tr_t2024, software_source_status__freedom_imperative_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(soft_be_t1985, software_source_status__freedom_imperative_reading, base_extractiveness, 1985, 0.7).
narrative_ontology:measurement(soft_be_t1995, software_source_status__freedom_imperative_reading, base_extractiveness, 1995, 0.75).
narrative_ontology:measurement(soft_be_t2005, software_source_status__freedom_imperative_reading, base_extractiveness, 2005, 0.8).
narrative_ontology:measurement(soft_be_t2015, software_source_status__freedom_imperative_reading, base_extractiveness, 2015, 0.85).
narrative_ontology:measurement(soft_be_t2024, software_source_status__freedom_imperative_reading, base_extractiveness, 2024, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t1985, software_source_status__freedom_imperative_reading, suppression_requirement, 1985, 0.5).
narrative_ontology:measurement(soft_su_t1995, software_source_status__freedom_imperative_reading, suppression_requirement, 1995, 0.55).
narrative_ontology:measurement(soft_su_t2005, software_source_status__freedom_imperative_reading, suppression_requirement, 2005, 0.6).
narrative_ontology:measurement(soft_su_t2015, software_source_status__freedom_imperative_reading, suppression_requirement, 2015, 0.65).
narrative_ontology:measurement(soft_su_t2024, software_source_status__freedom_imperative_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_source_status__freedom_imperative_reading, identity_coordination).
narrative_ontology:affects_constraint(software_source_status__freedom_imperative_reading, software_source_status__pragmatic_development_reading).
narrative_ontology:affects_constraint(software_source_status__freedom_imperative_reading, software_source_status__property_rights_reading).
narrative_ontology:affects_constraint(software_source_status__freedom_imperative_reading, software_source_status__utilitarian_hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'software_source_status' kernel. It focuses on the ethical imperative of software freedom, viewing proprietary software as an injustice. Other readings (pragmatic development, property rights, utilitarian hybrid) offer alternative framings of software's status and licensing.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
