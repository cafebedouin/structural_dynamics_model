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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: software_source_status__freedom_imperative_reading
 *   human_readable: Software Freedom Imperative (Ethical Reading)
 *   domain: software_engineering/political_economy_of_technology/intellectual_property
 *
 * SUMMARY:
 *   This constraint story instantiates the 'freedom imperative' reading of
 *   the software source status kernel. It posits that software freedom is a
 *   fundamental ethical requirement, and proprietary software, by restricting
 *   user rights, constitutes an injustice. This reading actively targets
 *   proprietary software models, framing them as inherently extractive and
 *   suppressive of user autonomy. The classification as a Snare reflects the
 *   direct and declared negative impact on proprietary software, which is
 *   identified as the victim set.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_source_status__freedom_imperative_reading, 0.85).
domain_priors:suppression_score(software_source_status__freedom_imperative_reading, 0.65).
domain_priors:theater_ratio(software_source_status__freedom_imperative_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, resistance, 0.9).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_source_status__freedom_imperative_reading, snare).
narrative_ontology:human_readable(software_source_status__freedom_imperative_reading, "Software Freedom Imperative (Ethical Reading)").
narrative_ontology:topic_domain(software_source_status__freedom_imperative_reading, "software_engineering/political_economy_of_technology/intellectual_property").

domain_priors:requires_active_enforcement(software_source_status__freedom_imperative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_source_status__freedom_imperative_reading, '4152b80b-97e8-4827-91f1-384bb9711aa9').
narrative_ontology:cs_kernel_codification('4152b80b-97e8-4827-91f1-384bb9711aa9', formalized).
narrative_ontology:cs_authority_grounding('4152b80b-97e8-4827-91f1-384bb9711aa9', lineage).
narrative_ontology:cs_interpretation_layer_present('4152b80b-97e8-4827-91f1-384bb9711aa9').
narrative_ontology:cs_reading_relation('4152b80b-97e8-4827-91f1-384bb9711aa9', software_source_status__pragmatic_development_reading, coexists_with).
narrative_ontology:cs_reading_relation('4152b80b-97e8-4827-91f1-384bb9711aa9', software_source_status__property_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('4152b80b-97e8-4827-91f1-384bb9711aa9', software_source_status__utilitarian_hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('4152b80b-97e8-4827-91f1-384bb9711aa9', foundational, software_freedom_is_moral_imperative).
narrative_ontology:cs_axiom_status(software_freedom_is_moral_imperative, holdable).
narrative_ontology:cs_axiom_grounding('4152b80b-97e8-4827-91f1-384bb9711aa9', software_freedom_is_moral_imperative, deontological).
narrative_ontology:cs_axiom('4152b80b-97e8-4827-91f1-384bb9711aa9', foundational, proprietary_software_is_unjust).
narrative_ontology:cs_axiom_status(proprietary_software_is_unjust, holdable).
narrative_ontology:cs_axiom_grounding('4152b80b-97e8-4827-91f1-384bb9711aa9', proprietary_software_is_unjust, deontological).
narrative_ontology:cs_reference_frame('4152b80b-97e8-4827-91f1-384bb9711aa9', four_freedoms_definition).
narrative_ontology:cs_drift_state('4152b80b-97e8-4827-91f1-384bb9711aa9', contemporary_digital_economy, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('4152b80b-97e8-4827-91f1-384bb9711aa9', '').
narrative_ontology:cs_kernel_id(software_source_status__freedom_imperative_reading, software_source_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_source_status__freedom_imperative_reading, free_software_users).
narrative_ontology:constraint_beneficiary(software_source_status__freedom_imperative_reading, free_software_developers).
narrative_ontology:constraint_beneficiary(software_source_status__freedom_imperative_reading, free_software_foundations).
narrative_ontology:constraint_victim(software_source_status__freedom_imperative_reading, proprietary_software_companies).
narrative_ontology:constraint_victim(software_source_status__freedom_imperative_reading, proprietary_software_users).
narrative_ontology:constraint_vindicates(software_source_status__freedom_imperative_reading, user_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(software_source_status__freedom_imperative_reading, digital_commons_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define, promote, and legally defend the principles of software freedom, including the four essential freedoms. They actively advocate against proprietary software models and enforce copyleft licenses.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, free_software_foundations, agenda_setter,
    institutional, generational, analytical, global).

% Contribute to and benefit from the free software ecosystem, sharing code, knowledge, and reputation. They adhere to the ethical imperative, viewing proprietary development as a compromise or injustice.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, free_software_developers, beneficiary,
    organized, biographical, mobile, global).

% Benefit from the freedoms to use, study, modify, and share software. They are empowered by the control and transparency offered by free software, avoiding vendor lock-in and proprietary restrictions.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, free_software_users, beneficiary,
    organized, biographical, mobile, global).

% Bear the cost of ethical critique, market pressure, and legal challenges from the free software movement. Their business model, based on restricting access to source code, is deemed unjust by this imperative.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, proprietary_software_companies, payer,
    institutional, biographical, constrained, global).

% Are considered victims of injustice, as they are denied fundamental freedoms by proprietary software licenses. They pay with their control, privacy, and ability to share and modify software.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, proprietary_software_users, payer,
    moderate, biographical, constrained, global).

% Analyze the legal implications of software licensing, copyright, and patent law, often navigating the tension between proprietary rights and the principles of software freedom. They observe the conflict without directly participating in the ethical imperative's enforcement.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, intellectual_property_lawyers, observer,
    analytical, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_source_status__freedom_imperative_reading, diffuse).
narrative_ontology:fixing_cost_class(software_source_status__freedom_imperative_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the development, distribution, and use of software under principles of user freedom, ensuring that software remains a shared resource that users can control and adapt.
% TRANSFER_FUNCTION: Transfers control, knowledge, and modification rights from software creators to users and the broader community. It also seeks to transfer market share and legitimacy from proprietary software models to free software alternatives.
% ABSENT_VOICES: Proprietary software companies and users who prioritize convenience, specific features, or traditional intellectual property rights over the four freedoms are often dismissed or framed as misguided within this ethical framework. Their perspectives are actively excluded from the core discourse of software freedom.
% DISAPPEARANCE_RATIONALE: If the ethical imperative of software freedom vanished overnight, the entire free software movement, its vast ecosystems of code, communities, and legal structures (like copyleft licenses) would lose their foundational justification. The critique of proprietary software would disappear, fundamentally altering how software is created, distributed, and perceived, leading to a reorganization around purely commercial or pragmatic concerns.
% FOUNDING_PROBLEM: The perceived injustice of proprietary software restricting users' fundamental freedoms to use, study, modify, and share software, leading to a loss of control, transparency, and potential exploitation by vendors.
% FOUNDING_PROBLEM_CORROBORATION: Free software advocates, academic ethicists, and digital rights organizations consistently corroborate the ongoing nature of this problem, citing pervasive vendor lock-in, opaque software, and anti-competitive practices as evidence of continued injustice. This corroboration comes from outside the direct beneficiaries of proprietary software.
narrative_ontology:disappearance_verdict(software_source_status__freedom_imperative_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_source_status__freedom_imperative_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_source_status__freedom_imperative_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(software_source_status__freedom_imperative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_source_status__freedom_imperative_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness is high (0.85) because this reading views proprietary software as fundamentally extracting user control and rights. Suppression (0.65) is moderate, as the imperative actively seeks to suppress proprietary models through advocacy, legal frameworks (like copyleft), and market competition, but lacks direct legal enforcement over proprietary software's existence. Resistance is very high (0.9) due to the powerful economic interests of proprietary software companies. Theater ratio is low (0.1) as this is a genuine ethical and political stance, not a performative one. Accessibility collapse (0.6) is moderate; while proprietary software is ethically 'collapsed' for adherents, it remains widely available in the broader market.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of free software advocates, this imperative is a coordinating force (a Rope) that enables shared development and user empowerment. However, from the perspective of proprietary software entities, the same imperative functions as a Snare, actively delegitimizing and seeking to dismantle their business model, thus extracting their market share and perceived legitimacy.
 *
 * DIRECTIONALITY LOGIC:
 *   Free software foundations, developers, and users are the beneficiaries, gaining control, transparency, and the ability to share and modify software. Proprietary software companies are direct payers, bearing the brunt of the ethical critique and market pressure. Proprietary software users are also considered payers, as they are seen as losing their fundamental freedoms by using proprietary systems. The imperative's directionality is strongly towards extracting legitimacy and market share from proprietary models.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a Snare prevents mislabeling the ethical imperative as a simple Rope. While it coordinates the free software community, its core function, as articulated, is to actively target and delegitimize proprietary software, which it frames as an 'injustice.' This active targeting and declared victim set (proprietary software) are key to its Snare classification, distinguishing it from a purely cooperative arrangement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ethical_vs_economic_grounding,
    'Is the primary force driving this imperative ethical conviction, or is it also significantly influenced by economic benefits (e.g., shared development costs, network effects within the free software ecosystem)?',
    'Analysis of funding sources for free software initiatives, motivations stated by developers, and economic impact studies comparing free vs. proprietary models.',
    'If primarily economic, the ''injustice'' claim might be re-evaluated as a strategic framing for market competition, potentially shifting the classification towards a Tangled Rope or even a Rope for its adherents, with the extraction being a side-effect of competition rather than a primary ethical goal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ethical_vs_economic_grounding, conceptual, 'Ambiguity between ethical and economic drivers of the software freedom imperative.').

omega_variable(
    user_freedom_definition_scope,
    'How is ''user freedom'' precisely defined and measured in the context of modern software, especially for services (SaaS, cloud computing) where local source code access is less relevant?',
    'Development of new metrics and definitions for ''freedom'' in service-oriented architectures, or a re-evaluation of the imperative''s applicability to such models.',
    'If the definition of freedom is too narrow for modern contexts, the imperative''s effective scope and extractiveness might be lower than claimed, as it fails to address new forms of control. If expanded, its extractiveness could be higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(user_freedom_definition_scope, conceptual, 'Scope and definition of ''user freedom'' in evolving software paradigms.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of proprietary alternatives primarily structural (e.g., lack of funding for proprietary alternatives in public institutions) or internalized (e.g., ideological commitment among developers and users that makes proprietary software unthinkable)?',
    'Post-exit suppression trajectory: if developers/users continue to avoid proprietary software even when structural barriers are removed, it suggests internalized suppression. Surveys on developer/user motivations.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as the target carries the suppression with them. If purely structural, removing external barriers would lead to a more level playing field.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for proprietary software.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_source_status__freedom_imperative_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_source_status__freedom_imperative_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(soft_tr_t10, software_source_status__freedom_imperative_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(soft_tr_t20, software_source_status__freedom_imperative_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(soft_tr_t30, software_source_status__freedom_imperative_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement(soft_tr_t40, software_source_status__freedom_imperative_reading, theater_ratio, 40, 0.1).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_source_status__freedom_imperative_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(soft_be_t10, software_source_status__freedom_imperative_reading, base_extractiveness, 10, 0.78).
narrative_ontology:measurement(soft_be_t20, software_source_status__freedom_imperative_reading, base_extractiveness, 20, 0.81).
narrative_ontology:measurement(soft_be_t30, software_source_status__freedom_imperative_reading, base_extractiveness, 30, 0.83).
narrative_ontology:measurement(soft_be_t40, software_source_status__freedom_imperative_reading, base_extractiveness, 40, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t0, software_source_status__freedom_imperative_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(soft_su_t10, software_source_status__freedom_imperative_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(soft_su_t20, software_source_status__freedom_imperative_reading, suppression_requirement, 20, 0.61).
narrative_ontology:measurement(soft_su_t30, software_source_status__freedom_imperative_reading, suppression_requirement, 30, 0.63).
narrative_ontology:measurement(soft_su_t40, software_source_status__freedom_imperative_reading, suppression_requirement, 40, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_source_status__freedom_imperative_reading, identity_coordination).
narrative_ontology:affects_constraint(software_source_status__freedom_imperative_reading, software_licensing_regimes).
narrative_ontology:affects_constraint(software_source_status__freedom_imperative_reading, digital_rights_advocacy).
narrative_ontology:affects_constraint(software_source_status__freedom_imperative_reading, pragmatic_development_reading).
narrative_ontology:affects_constraint(software_source_status__freedom_imperative_reading, property_rights_reading).
narrative_ontology:affects_constraint(software_source_status__freedom_imperative_reading, utilitarian_hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'software_source_status' kernel, focusing on the ethical imperative of freedom. It is linked to sibling readings that offer alternative framings of software's status.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
