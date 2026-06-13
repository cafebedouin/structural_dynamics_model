% ============================================================================
% CONSTRAINT STORY: permissive_license_text__corporate_moat_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_permissive_license_text__corporate_moat_reading, []).

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
 *   constraint_id: permissive_license_text__corporate_moat_reading
 *   human_readable: Permissive License Text as Corporate Moat
 *   domain: software_licensing/intellectual_property/technology_governance
 *
 * SUMMARY:
 *   This constraint describes the operation of permissive open-source
 *   licenses (e.g., MIT, Apache 2.0) as a mechanism for enterprise
 *   corporations to build proprietary derivative products without
 *   contributing back to the upstream open-source projects. While presented
 *   as fostering innovation and collaboration (the 'commons coordination'
 *   reading), this 'corporate moat' reading highlights the uncompensated
 *   extraction from individual maintainers and the broader open-source
 *   community. The constraint's persistence relies on the legal
 *   enforceability of the permissive terms, which allow for proprietary
 *   re-licensing without reciprocity.
 *
 * KEY AGENTS:
 *   - enterprise_corporations: Primary beneficiary (institutional/arbitrage) — extracts value without obligation.
 *   - individual_maintainers: Primary victim (moderate/constrained) — provides uncompensated labor, loses control over derivatives.
 *   - open_source_community: Secondary victim (organized/constrained) — collective loss of commons, reduced reciprocity.
 *   - legal_departments: Agenda setter (institutional/analytical) — enforces license terms to protect corporate interests.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(permissive_license_text__corporate_moat_reading, 0.65).
domain_priors:suppression_score(permissive_license_text__corporate_moat_reading, 0.7).
domain_priors:theater_ratio(permissive_license_text__corporate_moat_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(permissive_license_text__corporate_moat_reading, snare).
narrative_ontology:human_readable(permissive_license_text__corporate_moat_reading, "Permissive License Text as Corporate Moat").
narrative_ontology:topic_domain(permissive_license_text__corporate_moat_reading, "software_licensing/intellectual_property/technology_governance").

domain_priors:requires_active_enforcement(permissive_license_text__corporate_moat_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(permissive_license_text__corporate_moat_reading, 'c8b2d658-9735-4e24-bb54-c6c235e9a9f0').
narrative_ontology:cs_kernel_codification('c8b2d658-9735-4e24-bb54-c6c235e9a9f0', fixed_text).
narrative_ontology:cs_authority_grounding('c8b2d658-9735-4e24-bb54-c6c235e9a9f0', extraction).
narrative_ontology:cs_interpretation_layer_present('c8b2d658-9735-4e24-bb54-c6c235e9a9f0').
narrative_ontology:cs_reading_relation('c8b2d658-9735-4e24-bb54-c6c235e9a9f0', permissive_license_text__commons_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('c8b2d658-9735-4e24-bb54-c6c235e9a9f0', permissive_license_text__copyleft_counterfactual_reading, influences).
narrative_ontology:cs_axiom('c8b2d658-9735-4e24-bb54-c6c235e9a9f0', foundational, uncompensated_extraction_is_legitimate).
narrative_ontology:cs_axiom_status(uncompensated_extraction_is_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('c8b2d658-9735-4e24-bb54-c6c235e9a9f0', uncompensated_extraction_is_legitimate, conventional).
narrative_ontology:cs_axiom('c8b2d658-9735-4e24-bb54-c6c235e9a9f0', secondary, proprietary_enclosure_maximizes_innovation).
narrative_ontology:cs_axiom_status(proprietary_enclosure_maximizes_innovation, holdable).
narrative_ontology:cs_axiom_grounding('c8b2d658-9735-4e24-bb54-c6c235e9a9f0', proprietary_enclosure_maximizes_innovation, instrumental).
narrative_ontology:cs_reference_frame('c8b2d658-9735-4e24-bb54-c6c235e9a9f0', unfettered_commercial_reuse).
narrative_ontology:cs_drift_state('c8b2d658-9735-4e24-bb54-c6c235e9a9f0', contemporary_open_source_economy, gap(stable, minor, false)).
narrative_ontology:cs_created_at('c8b2d658-9735-4e24-bb54-c6c235e9a9f0', '').
narrative_ontology:cs_kernel_id(permissive_license_text__corporate_moat_reading, permissive_license_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(permissive_license_text__corporate_moat_reading, enterprise_corporations).
narrative_ontology:constraint_victim(permissive_license_text__corporate_moat_reading, individual_maintainers).
narrative_ontology:constraint_victim(permissive_license_text__corporate_moat_reading, open_source_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Utilize open-source software under permissive licenses to build proprietary products, avoiding licensing fees and development costs. They benefit from the 'free rider' aspect, leveraging community-developed code without obligation to contribute back or share their derivative works.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, enterprise_corporations, beneficiary,
    institutional, generational, arbitrage, global).

% Develop and maintain open-source projects under permissive licenses, often without direct compensation. They bear the cost of uncompensated labor and the loss of control over how their work is used in proprietary contexts, seeing their contributions enclosed by corporations.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, individual_maintainers, payer,
    moderate, biographical, constrained, global).

% The collective body of developers and users who contribute to and rely on open-source software. They experience a dilution of the 'commons' ethos when permissive licenses enable one-way value extraction, leading to reduced reciprocity and potential burnout among maintainers.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, open_source_community, payer,
    organized, generational, constrained, global).

% Within enterprise corporations, these departments interpret and enforce permissive licenses to maximize corporate advantage, ensuring compliance with minimal obligations while protecting proprietary derivative works. They actively suppress attempts to impose reciprocal terms.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, legal_departments, agenda_setter,
    institutional, generational, analytical, global).

% Advocate for licenses that require derivative works to also be open source (e.g., GPL). They are structurally excluded from the 'permissive license' framework, as their core principle of reciprocity is directly undermined by the permissive approach, leading to a constant struggle for influence.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, copyleft_advocates, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(permissive_license_text__corporate_moat_reading, enterprise_corporations).
narrative_ontology:fixing_cost_class(permissive_license_text__corporate_moat_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Facilitates widespread adoption and integration of software components by minimizing legal friction and allowing diverse licensing of derivative works.
% TRANSFER_FUNCTION: Transfers the value of open-source development effort from individual maintainers and the open-source community to enterprise corporations, who then monetize proprietary derivative products without reciprocal obligations.
% ABSENT_VOICES: Copyleft advocates and those who prioritize a strong, reciprocal commons are often marginalized in discussions about permissive licensing, as their arguments for enforced sharing directly challenge the 'corporate moat' function. Their absence allows the narrative of 'unfettered innovation' to dominate.
% DISAPPEARANCE_RATIONALE: If permissive licenses vanished overnight, the landscape of software development would drastically change. Corporations would face significant new licensing costs or be forced to adopt copyleft models, fundamentally altering their business models and the flow of value in the software ecosystem.
% FOUNDING_PROBLEM: Early software development faced friction from restrictive copyright, hindering collaboration and reuse. Permissive licenses aimed to reduce this friction, enabling broader adoption and innovation by allowing maximum freedom for derivative works.
% FOUNDING_PROBLEM_CORROBORATION: Enterprise corporations and some developers argue the problem is still live, citing the need for frictionless integration. However, individual maintainers and copyleft advocates, supported by economic analyses of value capture, argue that while friction was reduced, the problem of uncompensated extraction has emerged, making the original 'solution' a new problem for the commons.
narrative_ontology:disappearance_verdict(permissive_license_text__corporate_moat_reading, world_rearranges).
narrative_ontology:founding_problem_status(permissive_license_text__corporate_moat_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(permissive_license_text__corporate_moat_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(permissive_license_text__corporate_moat_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(permissive_license_text__corporate_moat_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(permissive_license_text__corporate_moat_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(permissive_license_text__corporate_moat_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.65) because significant value is derived from the open-source code without equivalent compensation or contribution back. Suppression is high (0.70) because the legal framework of copyright and contract law actively prevents individual maintainers from demanding compensation or enforcing reciprocity, effectively suppressing their ability to capture value from their work. Theater ratio is low (0.20) as the licenses are genuinely functional in enabling software distribution, but the 'coordination' narrative often masks the extractive reality. Accessibility collapse is moderate (0.40) as alternatives (e.g., copyleft licenses) exist but are often less adopted due to corporate preference for permissive terms. Resistance is moderate (0.55) from parts of the open-source community advocating for stronger reciprocity.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of enterprise corporations, permissive licenses are a 'rope' that facilitates innovation and reduces legal friction, enabling them to build products efficiently. From the perspective of individual maintainers and the open-source community, the same licenses function as a 'snare,' enabling uncompensated extraction and the enclosure of what was once a commons. The engine's classification will reflect the structural asymmetry of benefits and costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Enterprise corporations are clear beneficiaries (d=0.0-0.1) as they gain access to high-quality, free-to-use software without reciprocal obligations. Individual maintainers and the open-source community are victims (d=0.9-1.0) as their labor is leveraged for proprietary gain without direct compensation or enforced reciprocity. Legal departments act as agenda setters, enforcing the permissive terms that enable this flow.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (to foster widespread adoption and innovation by reducing friction) is still 'live' in a narrow sense, but its function has drifted. It now primarily serves to enable corporate product development at the expense of the original creators, rather than solely fostering a shared commons. This is not a case of mandatrophy where the problem is gone, but rather where the solution has been co-opted for asymmetric gain, making it a snare rather than a rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is this constraint a genuine ''commons coordination'' mechanism, or does it primarily function as a ''corporate moat'' for uncompensated extraction?',
    'Empirical analysis of derivative product revenue streams vs. contributions back to upstream projects; legal analysis of license enforcement patterns.',
    'If primarily a corporate moat, the classification shifts from Rope to Snare, highlighting the extractive nature. If genuine commons coordination, extractiveness is lower.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Ambiguity between commons coordination and corporate moat readings of permissive licenses.').

omega_variable(
    copyleft_counterfactual_impact,
    'What would be the structural impact on uncompensated extraction if permissive licenses were replaced by copyleft licenses requiring reciprocity?',
    'Comparative case studies of ecosystems governed by permissive vs. copyleft licenses, analyzing economic flows and contribution patterns.',
    'If copyleft significantly reduces uncompensated extraction, it suggests the permissive license''s ''corporate moat'' function is a structural choice, not an inevitability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(copyleft_counterfactual_impact, empirical, 'Impact of copyleft counterfactual on uncompensated extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(permissive_license_text__corporate_moat_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perm_tr_t0, permissive_license_text__corporate_moat_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(perm_tr_t5, permissive_license_text__corporate_moat_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement(perm_tr_t10, permissive_license_text__corporate_moat_reading, theater_ratio, 10, 0.21).
narrative_ontology:measurement(perm_tr_t15, permissive_license_text__corporate_moat_reading, theater_ratio, 15, 0.2).

% Extraction over time
narrative_ontology:measurement(perm_be_t0, permissive_license_text__corporate_moat_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(perm_be_t5, permissive_license_text__corporate_moat_reading, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(perm_be_t10, permissive_license_text__corporate_moat_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(perm_be_t15, permissive_license_text__corporate_moat_reading, base_extractiveness, 15, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(perm_su_t0, permissive_license_text__corporate_moat_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(perm_su_t5, permissive_license_text__corporate_moat_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(perm_su_t10, permissive_license_text__corporate_moat_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(perm_su_t15, permissive_license_text__corporate_moat_reading, suppression_requirement, 15, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(permissive_license_text__corporate_moat_reading, information_standard).
narrative_ontology:affects_constraint(permissive_license_text__corporate_moat_reading, permissive_license_text__commons_coordination_reading).
narrative_ontology:affects_constraint(permissive_license_text__corporate_moat_reading, permissive_license_text__copyleft_counterfactual_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'permissive_license_text' kernel. It focuses on the extractive function for corporations, contrasting with the 'commons_coordination_reading' (which emphasizes shared benefit) and the 'copyleft_counterfactual_reading' (which highlights the need for reciprocity).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
