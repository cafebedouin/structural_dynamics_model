% ============================================================================
% CONSTRAINT STORY: software_source_status__property_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_source_status__property_rights_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: software_source_status__property_rights_reading
 *   human_readable: Software as Intellectual Property: Creator Rights Reading
 *   domain: software_engineering/political_economy_of_technology/intellectual_property
 *
 * SUMMARY:
 *   This constraint represents the 'property_rights_reading' of the
 *   'software_source_status' kernel, asserting that software is intellectual
 *   property and creators have a legitimate right to restrict access and
 *   modification. This reading views source code as a proprietary asset,
 *   licensing restrictions as legitimate exercises of ownership, and users as
 *   consumers with contractual rights only. It is a dominant legal and
 *   economic framework for software distribution.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_source_status__property_rights_reading, 0.65).
domain_priors:suppression_score(software_source_status__property_rights_reading, 0.75).
domain_priors:theater_ratio(software_source_status__property_rights_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_source_status__property_rights_reading, tangled_rope).
narrative_ontology:human_readable(software_source_status__property_rights_reading, "Software as Intellectual Property: Creator Rights Reading").
narrative_ontology:topic_domain(software_source_status__property_rights_reading, "software_engineering/political_economy_of_technology/intellectual_property").

domain_priors:requires_active_enforcement(software_source_status__property_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_source_status__property_rights_reading, 'bc1870a2-54e7-4671-8b15-e21bf38731f1').
narrative_ontology:cs_kernel_codification('bc1870a2-54e7-4671-8b15-e21bf38731f1', formalized).
narrative_ontology:cs_authority_grounding('bc1870a2-54e7-4671-8b15-e21bf38731f1', lineage).
narrative_ontology:cs_interpretation_layer_present('bc1870a2-54e7-4671-8b15-e21bf38731f1').
narrative_ontology:cs_reading_relation('bc1870a2-54e7-4671-8b15-e21bf38731f1', software_source_status__freedom_imperative_reading, forecloses).
narrative_ontology:cs_reading_relation('bc1870a2-54e7-4671-8b15-e21bf38731f1', software_source_status__pragmatic_development_reading, coexists_with).
narrative_ontology:cs_reading_relation('bc1870a2-54e7-4671-8b15-e21bf38731f1', software_source_status__utilitarian_hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('bc1870a2-54e7-4671-8b15-e21bf38731f1', foundational, creator_has_exclusive_rights).
narrative_ontology:cs_axiom_status(creator_has_exclusive_rights, holdable).
narrative_ontology:cs_axiom_grounding('bc1870a2-54e7-4671-8b15-e21bf38731f1', creator_has_exclusive_rights, deontological).
narrative_ontology:cs_axiom('bc1870a2-54e7-4671-8b15-e21bf38731f1', foundational, software_is_private_property).
narrative_ontology:cs_axiom_status(software_is_private_property, holdable).
narrative_ontology:cs_axiom_grounding('bc1870a2-54e7-4671-8b15-e21bf38731f1', software_is_private_property, conventional).
narrative_ontology:cs_reference_frame('bc1870a2-54e7-4671-8b15-e21bf38731f1', exclusive_ownership_paradigm).
narrative_ontology:cs_drift_state('bc1870a2-54e7-4671-8b15-e21bf38731f1', contemporary_open_source_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('bc1870a2-54e7-4671-8b15-e21bf38731f1', '').
narrative_ontology:cs_kernel_id(software_source_status__property_rights_reading, software_source_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_source_status__property_rights_reading, proprietary_software_companies).
narrative_ontology:constraint_beneficiary(software_source_status__property_rights_reading, individual_software_creators).
narrative_ontology:constraint_victim(software_source_status__property_rights_reading, software_users).
narrative_ontology:constraint_victim(software_source_status__property_rights_reading, independent_developers).
narrative_ontology:constraint_victim(software_source_status__property_rights_reading, academic_researchers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and distribute software under proprietary licenses, benefiting from exclusive control over their source code and the ability to charge licensing fees. They actively enforce their intellectual property rights through legal means.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, proprietary_software_companies, beneficiary,
    institutional, generational, arbitrage, global).

% Create software and rely on intellectual property rights to protect their work, enabling them to monetize their creations through licensing or sales. They benefit from the legal framework that grants them control over their software.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, individual_software_creators, beneficiary,
    moderate, biographical, mobile, global).

% Purchase or license proprietary software, accepting restrictions on access to source code, modification, and redistribution. They pay licensing fees and are subject to vendor lock-in, with limited recourse for customization or repair.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, software_users, payer,
    powerless, immediate, constrained, global).

% Are restricted from building upon or modifying proprietary software without explicit permission or licensing. They face legal barriers to interoperability and innovation, often having to work around existing proprietary systems.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, independent_developers, payer,
    moderate, biographical, constrained, global).

% Are often limited in their ability to study, verify, or extend proprietary software due to lack of source code access. This hinders scientific progress and reproducibility in fields reliant on software.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, academic_researchers, payer,
    organized, generational, constrained, global).

% Interpret and enforce intellectual property laws, adjudicating disputes over copyright, patents, and licensing agreements. They are the ultimate arbiters of the 'property rights' reading, shaping its application and boundaries.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, legal_systems_and_courts, agenda_setter,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_source_status__property_rights_reading, proprietary_software_companies).
narrative_ontology:fixing_cost_class(software_source_status__property_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legal framework that incentivizes software creation and investment by granting creators exclusive rights, thereby coordinating the efforts of developers and investors by protecting their returns.
% TRANSFER_FUNCTION: Transfers economic value from software users and other developers (through licensing fees, restricted access, and limited modification rights) to proprietary software companies and individual creators.
% ABSENT_VOICES: Advocates for software freedom and open-source principles are often marginalized in policy discussions dominated by intellectual property lobbies. They would argue for a more equitable distribution of software rights and benefits.
% DISAPPEARANCE_RATIONALE: If the legal framework for software as intellectual property vanished overnight, the software industry as currently structured would collapse. Companies relying on proprietary models would lose their core business, leading to a massive reorganization of software development, distribution, and monetization models, likely shifting towards open-source or service-based models.
% FOUNDING_PROBLEM: The problem of incentivizing investment and creative effort in software development, ensuring creators could reap rewards from their innovations, and preventing unauthorized copying and free-riding.
% FOUNDING_PROBLEM_CORROBORATION: Proprietary software companies and legal scholars attest that the problem of incentivizing creation and preventing free-riding remains live. Critics (e.g., open-source advocates, some economists) argue that while the problem was real, the current solution has overshot, creating monopolies and hindering innovation, making the 'live' status contested in its current form.
narrative_ontology:disappearance_verdict(software_source_status__property_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_source_status__property_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_source_status__property_rights_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(software_source_status__property_rights_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_source_status__property_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(software_source_status__property_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(software_source_status__property_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it provides a coordination function (incentivizing creation and investment by protecting intellectual property) but also involves significant asymmetric extraction (creators benefit from restricted access, while users and other developers pay through licensing fees and limited modification rights). Active enforcement through copyright law, DRM, and legal action is required to maintain these restrictions. The extractiveness (0.65) and suppression (0.75) reflect the substantial costs borne by users and the active measures taken to prevent unauthorized access or modification.
 *
 * PERSPECTIVAL GAP:
 *   Proprietary software companies and individual creators experience this as a legitimate framework that protects their work and incentivizes innovation. Software users and independent developers, however, experience it as a restrictive and extractive system that limits their ability to understand, modify, and share software, often leading to vendor lock-in and reduced innovation in the broader ecosystem.
 *
 * DIRECTIONALITY LOGIC:
 *   Proprietary software companies and individual creators are primary beneficiaries (d near 0.0) as they directly profit from restricting access and modification. Software users, independent developers, and academic researchers are targets (d near 1.0) as they bear the costs of licensing, lack of access to source code, and inability to freely modify or distribute software. Legal systems and enforcement bodies act as agenda-setters, maintaining the framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate to incentivize creation and protect investment remains live, but its application has expanded to create significant market power and extraction. The classification as Tangled Rope prevents mislabeling it as a pure Rope (ignoring extraction) or a pure Snare (ignoring the coordination function of IP protection for creators). The ongoing debate with other readings of the 'software_source_status' kernel highlights the contested nature of its 'mandate'.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is this constraint a fundamental property right, or a constructed legal framework that could be otherwise?',
    'Analysis of alternative legal frameworks (e.g., open-source licenses, public domain software) and their economic viability.',
    'If constructed, the extraction and suppression are policy choices, not inherent; if fundamental, they are unavoidable costs of creation. This constraint is the ''property_rights_reading'' of the ''software_source_status'' kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Ambiguity between fundamental right and legal construct for software IP.').

omega_variable(
    freedom_imperative_conflict,
    'How would the classification change if the ''freedom_imperative_reading'' of the software_source_status kernel were adopted?',
    'Re-evaluate the constraint from the perspective that software freedom is a fundamental ethical requirement, where proprietary software is an injustice.',
    'This reading would likely be reclassified as a Snare, as it would be seen as actively suppressing a fundamental right, with high extraction from users and developers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(freedom_imperative_conflict, conceptual, 'Conflict with the freedom imperative reading of software source status.').

omega_variable(
    pragmatic_development_conflict,
    'How would the classification change if the ''pragmatic_development_reading'' of the software_source_status kernel were adopted?',
    'Re-evaluate the constraint from the perspective that open source is a superior development methodology, where freedom is instrumental to quality.',
    'This reading would likely be reclassified as a Tangled Rope or Snare, as it would be seen as hindering development quality and innovation through artificial restrictions, with extraction from the broader software ecosystem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pragmatic_development_conflict, conceptual, 'Conflict with the pragmatic development reading of software source status.').

omega_variable(
    utilitarian_hybrid_conflict,
    'How would the classification change if the ''utilitarian_hybrid_reading'' of the software_source_status kernel were adopted?',
    'Re-evaluate the constraint from the perspective that software licensing should maximize aggregate welfare, with both proprietary and open models serving different contexts.',
    'This reading would likely be reclassified as a Tangled Rope or Rope, depending on the balance of welfare maximization. If the current proprietary model is found to not maximize welfare, its extractive elements would be highlighted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(utilitarian_hybrid_conflict, conceptual, 'Conflict with the utilitarian hybrid reading of software source status.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_source_status__property_rights_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_source_status__property_rights_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(soft_tr_t10, software_source_status__property_rights_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(soft_tr_t20, software_source_status__property_rights_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_source_status__property_rights_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(soft_be_t10, software_source_status__property_rights_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(soft_be_t20, software_source_status__property_rights_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t0, software_source_status__property_rights_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(soft_su_t10, software_source_status__property_rights_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(soft_su_t20, software_source_status__property_rights_reading, suppression_requirement, 20, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_source_status__property_rights_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'software_source_status' kernel. Other readings include 'freedom_imperative_reading', 'pragmatic_development_reading', and 'utilitarian_hybrid_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
