% ============================================================================
% CONSTRAINT STORY: gpl_derivative_work_trigger__broad_copyleft_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_derivative_work_trigger__broad_copyleft_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: gpl_derivative_work_trigger__broad_copyleft_reading
 *   human_readable: GPL Derivative Work Trigger (Broad Copyleft Reading)
 *   domain: software_licensing/copyright_law/open_source_governance
 *
 * SUMMARY:
 *   This constraint represents the 'broad copyleft' interpretation of the GNU
 *   General Public License (GPL), asserting that any software linking to
 *   GPL-licensed code, even dynamically, creates a 'derivative work' and thus
 *   triggers the GPL's source code disclosure obligations. This reading aims
 *   to maximize the 'viral' effect of copyleft, pulling dependent code into
 *   the open-source commons. It is a contested interpretation within
 *   copyright law and the open-source community.
 *
 * KEY AGENTS:
 *   - open_source_community: Primary beneficiary (organized/analytical) — gains source access and ensures software freedom.
 *   - downstream_users: Secondary beneficiary (moderate/biographical) — gains access to source code and the right to modify.
 *   - proprietary_software_vendors: Primary victim (institutional/biographical) — faces compliance costs or avoidance strategies.
 *   - gpl_enforcers: Agenda setter (organized/institutional) — actively enforces the license terms through legal action or community pressure.
 *   - software_developers: Payer/Beneficiary (moderate/biographical) — may pay compliance costs or benefit from open ecosystem.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_derivative_work_trigger__broad_copyleft_reading, 0.3).
domain_priors:suppression_score(gpl_derivative_work_trigger__broad_copyleft_reading, 0.4).
domain_priors:theater_ratio(gpl_derivative_work_trigger__broad_copyleft_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_derivative_work_trigger__broad_copyleft_reading, rope).
narrative_ontology:human_readable(gpl_derivative_work_trigger__broad_copyleft_reading, "GPL Derivative Work Trigger (Broad Copyleft Reading)").
narrative_ontology:topic_domain(gpl_derivative_work_trigger__broad_copyleft_reading, "software_licensing/copyright_law/open_source_governance").

domain_priors:requires_active_enforcement(gpl_derivative_work_trigger__broad_copyleft_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_derivative_work_trigger__broad_copyleft_reading, '4d582e13-95c3-40ac-bb64-ffa43553eb23').
narrative_ontology:cs_kernel_codification('4d582e13-95c3-40ac-bb64-ffa43553eb23', fixed_text).
narrative_ontology:cs_authority_grounding('4d582e13-95c3-40ac-bb64-ffa43553eb23', lineage).
narrative_ontology:cs_interpretation_layer_present('4d582e13-95c3-40ac-bb64-ffa43553eb23').
narrative_ontology:cs_reading_relation('4d582e13-95c3-40ac-bb64-ffa43553eb23', gpl_derivative_work_trigger__narrow_linking_permissive_reading, coexists_with).
narrative_ontology:cs_reading_relation('4d582e13-95c3-40ac-bb64-ffa43553eb23', gpl_derivative_work_trigger__interface_boundary_reading, coexists_with).
narrative_ontology:cs_axiom('4d582e13-95c3-40ac-bb64-ffa43553eb23', foundational, linking_creates_derivative_work).
narrative_ontology:cs_axiom_status(linking_creates_derivative_work, holdable).
narrative_ontology:cs_axiom_grounding('4d582e13-95c3-40ac-bb64-ffa43553eb23', linking_creates_derivative_work, conventional).
narrative_ontology:cs_axiom('4d582e13-95c3-40ac-bb64-ffa43553eb23', foundational, software_freedom_requires_viral_copyleft).
narrative_ontology:cs_axiom_status(software_freedom_requires_viral_copyleft, holdable).
narrative_ontology:cs_axiom_grounding('4d582e13-95c3-40ac-bb64-ffa43553eb23', software_freedom_requires_viral_copyleft, deontological).
narrative_ontology:cs_reference_frame('4d582e13-95c3-40ac-bb64-ffa43553eb23', maximal_copyleft_interpretation).
narrative_ontology:cs_drift_state('4d582e13-95c3-40ac-bb64-ffa43553eb23', contemporary_legal_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('4d582e13-95c3-40ac-bb64-ffa43553eb23', '').
narrative_ontology:cs_kernel_id(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_derivative_work_trigger).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__broad_copyleft_reading, downstream_users).
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__broad_copyleft_reading, open_source_community).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__broad_copyleft_reading, proprietary_software_vendors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__broad_copyleft_reading, software_developers).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__broad_copyleft_reading, software_developers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the expansion of the open-source commons, gaining access to more source code and ensuring the 'freedom' of software. Actively advocates for this broad interpretation of copyleft.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, open_source_community, beneficiary,
    organized, generational, analytical, global).

% Benefits from the availability of source code, allowing them to inspect, modify, and redistribute software. They are often unaware of the legal intricacies but benefit from the outcomes of this interpretation.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, downstream_users, beneficiary,
    moderate, biographical, mobile, global).

% Faces significant compliance costs if they link to GPL code, as it would require them to release their proprietary source code. They often seek to avoid GPL-licensed components or use alternative, more permissive licenses.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, proprietary_software_vendors, payer,
    institutional, biographical, constrained, global).

% Organizations and individuals (e.g., Free Software Foundation, Software Freedom Conservancy) that actively monitor, educate, and enforce GPL compliance, including pursuing legal action against alleged infringers.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_enforcers, agenda_setter,
    institutional, generational, analytical, global).

% Depending on their project, they may be beneficiaries (if contributing to open source) or payers (if developing proprietary software that links to GPL code). They navigate the complexities of licensing to avoid legal issues.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, software_developers, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(gpl_derivative_work_trigger__broad_copyleft_reading, software_developers, beneficiary).

% Advise clients on GPL compliance, interpret case law, and litigate disputes. They are key interpreters of the 'derivative work' definition and its application to software linking.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, copyright_lawyers, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_derivative_work_trigger__broad_copyleft_reading, open_source_community).
narrative_ontology:fixing_cost_class(gpl_derivative_work_trigger__broad_copyleft_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the development and distribution of software under a shared legal framework that ensures all derivative works remain open source, fostering a collaborative commons.
% TRANSFER_FUNCTION: Transfers the right to use, modify, and distribute source code from proprietary developers to the public domain (or other GPL-licensed projects) when their code links to GPL-licensed components.
% ABSENT_VOICES: Proprietary software advocates who argue for a narrower interpretation of copyright law to protect their intellectual property, and developers who prefer more permissive licenses to avoid 'viral' copyleft effects. Their arguments are often heard in legal forums but are not part of the GPL's internal logic.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished, proprietary vendors would freely link to GPL code without disclosure, fragmenting the open-source commons and undermining the 'software freedom' mandate of the GPL. The open-source ecosystem would fundamentally change, with less code flowing into the public domain.
% FOUNDING_PROBLEM: The problem of proprietary software developers taking open-source code, modifying it, and then distributing the modified version as closed-source, thereby privatizing communal effort and hindering software freedom.
% FOUNDING_PROBLEM_CORROBORATION: The Free Software Foundation and other open-source advocacy groups consistently attest that the problem of 'privatizing' open-source contributions remains live. Independent legal scholars and software historians corroborate the historical context and ongoing relevance of this concern, noting that without strong copyleft, the open-source movement's goals would be significantly undermined.
narrative_ontology:disappearance_verdict(gpl_derivative_work_trigger__broad_copyleft_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_derivative_work_trigger__broad_copyleft_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_derivative_work_trigger__broad_copyleft_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(gpl_derivative_work_trigger__broad_copyleft_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_derivative_work_trigger__broad_copyleft_reading_tests).
:- end_tests(gpl_derivative_work_trigger__broad_copyleft_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Rope because its primary function is coordination (ensuring software freedom and shared development) with a clear benefit to the open-source community and users. However, it has a non-trivial extractiveness (0.3) and suppression (0.4) for proprietary vendors, who must either comply with source disclosure or re-architect their software to avoid linking. The 'derivative work' definition is the core of the contest. The theater ratio is low (0.1) as enforcement is genuine, not performative.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the open-source community and downstream users, this is a beneficial coordination mechanism that ensures software freedom. From the perspective of proprietary software vendors, it is a coercive mechanism that extracts their intellectual property or forces costly re-engineering. The engine will compute different classifications for these seats based on their declared roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   The open_source_community and downstream_users are clear beneficiaries (d near 0.0) as they gain access to source code. Proprietary_software_vendors are targets (d near 1.0) as they bear the cost of compliance or avoidance. Software_developers can be both, depending on their business model (open-source vs. proprietary development). GPL_enforcers are agenda setters, actively maintaining the constraint for the benefit of the open-source ecosystem.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint's mandate (ensuring software freedom) is still live and actively pursued by its beneficiaries. The classification as a Rope, despite its extractive elements, prevents mislabeling it as a pure Snare, acknowledging its genuine coordination function for the open-source commons. The contestation around 'derivative work' is central to its operation, not a sign of atrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    derivative_work_definition_ambiguity,
    'Is ''derivative work'' under copyright law broad enough to encompass dynamic linking without direct modification, as this reading asserts?',
    'Binding legal precedent from a high court specifically addressing dynamic linking and GPL compliance, or legislative clarification of copyright scope.',
    'If resolved narrowly, this constraint would be reclassified as a Snare (pure extraction) for proprietary vendors, as its legal basis would be weak. If resolved broadly, it would solidify as a Rope, with its coordination function legally affirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(derivative_work_definition_ambiguity, conceptual, 'Ambiguity in the legal definition of ''derivative work'' as applied to software linking.').

omega_variable(
    gpl_kernel_reading_divergence,
    'This constraint is the ''broad_copyleft_reading'' of the ''gpl_derivative_work_trigger'' kernel. How would its classification change under the ''narrow_linking_permissive_reading'' or ''interface_boundary_reading''?',
    'Analysis of the alternative readings'' structural properties (beneficiaries, victims, enforcement) and their respective legal interpretations.',
    'The ''narrow_linking_permissive_reading'' would likely classify as a Rope with lower extractiveness and suppression, as it would impose fewer obligations. The ''interface_boundary_reading'' would also be a Rope, but with a different boundary for derivative work, shifting the burden of compliance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(gpl_kernel_reading_divergence, conceptual, 'Impact of alternative readings of the GPL derivative work kernel on constraint classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_derivative_work_trigger__broad_copyleft_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(gpl__tr_t5, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement(gpl__tr_t10, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(gpl__tr_t15, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 15, 0.1).
narrative_ontology:measurement(gpl__tr_t20, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(gpl__be_t5, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 5, 0.27).
narrative_ontology:measurement(gpl__be_t10, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 10, 0.28).
narrative_ontology:measurement(gpl__be_t15, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 15, 0.29).
narrative_ontology:measurement(gpl__be_t20, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 20, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t0, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(gpl__su_t5, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 5, 0.37).
narrative_ontology:measurement(gpl__su_t10, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 10, 0.38).
narrative_ontology:measurement(gpl__su_t15, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 15, 0.39).
narrative_ontology:measurement(gpl__su_t20, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 20, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_derivative_work_trigger__broad_copyleft_reading, information_standard).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_derivative_work_trigger__narrow_linking_permissive_reading).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_derivative_work_trigger__interface_boundary_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'gpl_derivative_work_trigger' kernel. Other readings include 'narrow_linking_permissive_reading' and 'interface_boundary_reading', which interpret 'derivative work' differently, leading to different compliance obligations and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
