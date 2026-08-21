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
 *   human_readable: Software Freedom as Ethical Imperative
 *   domain: software_engineering/political_economy_of_technology/intellectual_property
 *
 * SUMMARY:
 *   This constraint represents the ethical imperative that software freedom
 *   is a fundamental requirement and proprietary software is an injustice. It
 *   is a normative framework that defines a specific moral stance on software
 *   licensing and distribution. From this reading's perspective, proprietary
 *   software inherently extracts freedom and control from users, and its
 *   licensing restrictions actively suppress user rights. The reading itself,
 *   as a structural claim, functions as a Snare by defining proprietary
 *   software and its users/developers as victims of an unjust system.
 *
 * KEY AGENTS:
 *   - free_software_advocates: Primary agenda_setter and beneficiary (institutional/identity_locked) — define and promote the imperative.
 *   - proprietary_software_developers: Primary target/victim (powerful/constrained) — their business model is deemed unjust.
 *   - proprietary_software_users: Secondary target/victim (moderate/constrained) — seen as deprived of freedom.
 *   - software_corporations: Institutional target/victim (institutional/constrained) — their existence is challenged.
 *   - intellectual_property_lawyers: Excluded (powerful/identity_locked) — their foundational premises are rejected.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_source_status__freedom_imperative_reading, 0.85).
domain_priors:suppression_score(software_source_status__freedom_imperative_reading, 0.9).
domain_priors:theater_ratio(software_source_status__freedom_imperative_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(software_source_status__freedom_imperative_reading, resistance, 0.9).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_source_status__freedom_imperative_reading, snare).
narrative_ontology:human_readable(software_source_status__freedom_imperative_reading, "Software Freedom as Ethical Imperative").
narrative_ontology:topic_domain(software_source_status__freedom_imperative_reading, "software_engineering/political_economy_of_technology/intellectual_property").

domain_priors:requires_active_enforcement(software_source_status__freedom_imperative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_source_status__freedom_imperative_reading, '2e30d8a8-accf-45ef-aa59-582587e8edac').
narrative_ontology:cs_kernel_codification('2e30d8a8-accf-45ef-aa59-582587e8edac', implicit).
narrative_ontology:cs_authority_grounding('2e30d8a8-accf-45ef-aa59-582587e8edac', lineage).
narrative_ontology:cs_interpretation_layer_present('2e30d8a8-accf-45ef-aa59-582587e8edac').
narrative_ontology:cs_reading_relation('2e30d8a8-accf-45ef-aa59-582587e8edac', software_source_status__pragmatic_development_reading, coexists_with).
narrative_ontology:cs_reading_relation('2e30d8a8-accf-45ef-aa59-582587e8edac', software_source_status__property_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('2e30d8a8-accf-45ef-aa59-582587e8edac', software_source_status__utilitarian_hybrid_reading, forecloses).
narrative_ontology:cs_axiom('2e30d8a8-accf-45ef-aa59-582587e8edac', foundational, software_freedom_is_a_moral_imperative).
narrative_ontology:cs_axiom_status(software_freedom_is_a_moral_imperative, holdable).
narrative_ontology:cs_axiom_grounding('2e30d8a8-accf-45ef-aa59-582587e8edac', software_freedom_is_a_moral_imperative, deontological).
narrative_ontology:cs_axiom('2e30d8a8-accf-45ef-aa59-582587e8edac', foundational, proprietary_software_is_inherently_unjust).
narrative_ontology:cs_axiom_status(proprietary_software_is_inherently_unjust, holdable).
narrative_ontology:cs_axiom_grounding('2e30d8a8-accf-45ef-aa59-582587e8edac', proprietary_software_is_inherently_unjust, deontological).
narrative_ontology:cs_reference_frame('2e30d8a8-accf-45ef-aa59-582587e8edac', universal_software_freedom).
narrative_ontology:cs_drift_state('2e30d8a8-accf-45ef-aa59-582587e8edac', contemporary_proprietary_dominance, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('2e30d8a8-accf-45ef-aa59-582587e8edac', '').
narrative_ontology:cs_kernel_id(software_source_status__freedom_imperative_reading, software_source_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_source_status__freedom_imperative_reading, free_software_advocates).
narrative_ontology:constraint_beneficiary(software_source_status__freedom_imperative_reading, users_of_free_software).
narrative_ontology:constraint_victim(software_source_status__freedom_imperative_reading, proprietary_software_developers).
narrative_ontology:constraint_victim(software_source_status__freedom_imperative_reading, proprietary_software_users).
narrative_ontology:constraint_victim(software_source_status__freedom_imperative_reading, software_corporations).
narrative_ontology:constraint_vindicates(software_source_status__freedom_imperative_reading, software_freedom_doctrine).
narrative_ontology:constraint_vindicates(software_source_status__freedom_imperative_reading, user_sovereignty_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define, promote, and defend the ethical imperative of software freedom. They see themselves as upholding fundamental user rights and actively campaign against proprietary software models. Their identity is deeply fused with this cause.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, free_software_advocates, agenda_setter,
    institutional, generational, identity_locked, global).

% Directly benefit from the freedom to use, study, modify, and distribute software. They align with the ethical imperative, often contributing to its advocacy and development, but can also choose proprietary alternatives if pragmatic needs arise.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, users_of_free_software, beneficiary,
    organized, biographical, mobile, global).

% Their business model, based on restricting access and modification, is deemed ethically illegitimate and an injustice by this reading. They bear the cost of moral condemnation and advocacy pressure, facing calls to abandon their practices.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, proprietary_software_developers, payer,
    powerful, biographical, constrained, global).

% Are seen as victims of proprietary software, deprived of fundamental freedoms, even if they do not perceive themselves as such. They bear the cost of lost autonomy and control, often without realizing it, due to licensing restrictions.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, proprietary_software_users, payer,
    moderate, biographical, constrained, global).

% Their entire existence and profit model, built on proprietary software, is fundamentally challenged as unjust. They face significant reputational and ethical pressure, and potential legal challenges in jurisdictions where software freedom principles gain traction.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, software_corporations, payer,
    institutional, generational, constrained, global).

% Their professional identity and legal framework are based on the legitimacy of intellectual property rights, which this reading categorically rejects for software. They are excluded from the ethical discourse of this reading, as their foundational premises are denied.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, intellectual_property_lawyers, excluded,
    powerful, biographical, identity_locked, national).

% May appreciate the technical or collaborative benefits of open source but do not necessarily subscribe to the ethical imperative. They observe the debate from a position of practical utility, rather than fundamental rights.
narrative_ontology:constraint_stakeholder(software_source_status__freedom_imperative_reading, pragmatic_developers, observer,
    moderate, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_source_status__freedom_imperative_reading, free_software_advocates).
narrative_ontology:fixing_cost_class(software_source_status__freedom_imperative_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the efforts of developers and users around shared, open codebases, ensuring collective control, modification rights, and transparency, thereby fostering a collaborative and ethical software ecosystem.
% TRANSFER_FUNCTION: Transfers control over software from proprietary owners to the user community; transfers the right to modify, distribute, and understand software from a single entity to all users, thereby redistributing power and autonomy.
% ABSENT_VOICES: Intellectual property lawyers, proprietary software executives, and users who prioritize convenience or specific features over source freedom are structurally excluded. They would argue for property rights, market efficiency, or user choice, but their premises are rejected by this reading.
% DISAPPEARANCE_RATIONALE: If this ethical imperative vanished overnight, the moral and philosophical foundation for the free software movement would collapse. The pressure to make software free would dissipate, potentially leading to a significant reduction in ethically-driven open source development and advocacy, and a further entrenchment of proprietary models.
% FOUNDING_PROBLEM: The perceived loss of user control and freedom due to proprietary software, which restricts modification, distribution, and understanding of the software, thereby creating an unjust power imbalance.
% FOUNDING_PROBLEM_CORROBORATION: Free software foundations (e.g., FSF), academic ethicists, and some privacy advocates corroborate this problem, arguing for digital rights and user autonomy. They point to ongoing examples of vendor lock-in, surveillance, and lack of transparency in proprietary systems.
narrative_ontology:disappearance_verdict(software_source_status__freedom_imperative_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_source_status__freedom_imperative_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_source_status__freedom_imperative_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.85) because this reading frames proprietary software as fundamentally extracting user freedom and control. Suppression is high (0.90) as the imperative seeks to suppress the legitimacy and prevalence of proprietary alternatives. Accessibility collapse is very high (0.95) because, from this ethical viewpoint, legitimate alternatives to free software are almost entirely collapsed. Resistance is high (0.90) due to the strong opposition from the proprietary software industry. Theater ratio is low (0.10) as the ethical claim is direct and not performative; the advocacy is genuine.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap lies between this reading, which views proprietary software as an injustice, and those who uphold intellectual property rights or prioritize pragmatic development. From the perspective of proprietary software developers, this imperative is an attack on their legitimate property and livelihood, while from the perspective of free software advocates, it is a necessary ethical stance.
 *
 * DIRECTIONALITY LOGIC:
 *   Free software advocates are beneficiaries (d near 0.0) as they gain moral authority and influence from upholding this imperative. Users of free software are also beneficiaries (d near 0.15) as they directly experience the freedom. Proprietary software developers, users, and corporations are victims/targets (d near 0.9-1.0) as their activities are deemed unjust and their freedoms restricted by the imperative's logic. Intellectual property lawyers are excluded, as their entire framework is denied legitimacy.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a Snare prevents mislabeling the ethical imperative as a benign coordination mechanism. While it aims for a coordinated, free software ecosystem, its method involves actively defining and targeting 'victims' (proprietary software) and suppressing 'alternatives' (proprietary models) through ethical condemnation and advocacy. The 'injustice' claim is the engine of its extraction and suppression, not a side effect of coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately representing the ''freedom_imperative_reading'' of the ''software_source_status'' kernel?',
    'Comparison with canonical texts and statements from leading free software advocates (e.g., Richard Stallman, Free Software Foundation).',
    'If misidentified, the analysis of inter-reading relations and axiom conflicts would be flawed, leading to incorrect classification of the kernel''s overall dynamics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Verifies the accurate instantiation of the specified kernel reading.').

omega_variable(
    ethical_vs_pragmatic_grounding,
    'Is the primary driver for open source adoption ethical imperative or pragmatic development benefits?',
    'Surveying open source contributors and users on their motivations, and analyzing the rhetoric of major open source projects and foundations.',
    'If pragmatic benefits are the dominant driver, the ''freedom_imperative_reading'' might have less real-world influence than claimed, potentially reducing its effective extractiveness and suppression in practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ethical_vs_pragmatic_grounding, empirical, 'Distinguishes between ethical and instrumental motivations for free software.').

omega_variable(
    property_rights_legitimacy,
    'Are intellectual property rights in software a legitimate basis for restriction, or are they inherently unjust?',
    'Philosophical debate and legal precedent regarding the nature of software as property versus a medium of expression or communication.',
    'If property rights are deemed legitimate, the ''injustice'' claim of this reading is undermined, reducing its perceived extractiveness and suppression. If deemed illegitimate, the Snare classification is strongly reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(property_rights_legitimacy, conceptual, 'Examines the fundamental legitimacy of software intellectual property.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_source_status__freedom_imperative_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_source_status__freedom_imperative_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(soft_tr_t6, software_source_status__freedom_imperative_reading, theater_ratio, 6, 0.1).
narrative_ontology:measurement(soft_tr_t12, software_source_status__freedom_imperative_reading, theater_ratio, 12, 0.1).
narrative_ontology:measurement(soft_tr_t18, software_source_status__freedom_imperative_reading, theater_ratio, 18, 0.1).
narrative_ontology:measurement(soft_tr_t24, software_source_status__freedom_imperative_reading, theater_ratio, 24, 0.1).
narrative_ontology:measurement(soft_tr_t30, software_source_status__freedom_imperative_reading, theater_ratio, 30, 0.1).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_source_status__freedom_imperative_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(soft_be_t6, software_source_status__freedom_imperative_reading, base_extractiveness, 6, 0.78).
narrative_ontology:measurement(soft_be_t12, software_source_status__freedom_imperative_reading, base_extractiveness, 12, 0.8).
narrative_ontology:measurement(soft_be_t18, software_source_status__freedom_imperative_reading, base_extractiveness, 18, 0.82).
narrative_ontology:measurement(soft_be_t24, software_source_status__freedom_imperative_reading, base_extractiveness, 24, 0.84).
narrative_ontology:measurement(soft_be_t30, software_source_status__freedom_imperative_reading, base_extractiveness, 30, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t0, software_source_status__freedom_imperative_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(soft_su_t6, software_source_status__freedom_imperative_reading, suppression_requirement, 6, 0.83).
narrative_ontology:measurement(soft_su_t12, software_source_status__freedom_imperative_reading, suppression_requirement, 12, 0.85).
narrative_ontology:measurement(soft_su_t18, software_source_status__freedom_imperative_reading, suppression_requirement, 18, 0.87).
narrative_ontology:measurement(soft_su_t24, software_source_status__freedom_imperative_reading, suppression_requirement, 24, 0.89).
narrative_ontology:measurement(soft_su_t30, software_source_status__freedom_imperative_reading, suppression_requirement, 30, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_source_status__freedom_imperative_reading, identity_coordination).
narrative_ontology:affects_constraint(software_source_status__freedom_imperative_reading, software_source_status__pragmatic_development_reading).
narrative_ontology:affects_constraint(software_source_status__freedom_imperative_reading, software_source_status__property_rights_reading).
narrative_ontology:affects_constraint(software_source_status__freedom_imperative_reading, software_source_status__utilitarian_hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'software_source_status' kernel, which describes the contested nature of software licensing and access. This reading focuses on the ethical imperative of freedom, while sibling readings explore pragmatic, property-rights, and utilitarian perspectives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
