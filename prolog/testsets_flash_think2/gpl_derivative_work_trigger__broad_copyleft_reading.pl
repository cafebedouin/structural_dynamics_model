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
 *   constraint_id: gpl_derivative_work_trigger__broad_copyleft_reading
 *   human_readable: GPL Derivative Work Trigger (Broad Copyleft Reading)
 *   domain: software_licensing/copyright_law/open_source_governance
 *
 * SUMMARY:
 *   This constraint represents the 'broad copyleft reading' of the GNU
 *   General Public License (GPL), which asserts that linking to GPL-licensed
 *   code (even dynamically) creates a derivative work, thereby triggering the
 *   obligation to disclose the source code of the entire combined work. This
 *   interpretation is foundational to the Free Software Foundation's
 *   philosophy and is actively enforced through legal means. It aims to
 *   expand the 'commons' of free software by preventing proprietary enclosure
 *   of code that builds upon GPL components.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_derivative_work_trigger__broad_copyleft_reading, 0.7).
domain_priors:suppression_score(gpl_derivative_work_trigger__broad_copyleft_reading, 0.8).
domain_priors:theater_ratio(gpl_derivative_work_trigger__broad_copyleft_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_derivative_work_trigger__broad_copyleft_reading, tangled_rope).
narrative_ontology:human_readable(gpl_derivative_work_trigger__broad_copyleft_reading, "GPL Derivative Work Trigger (Broad Copyleft Reading)").
narrative_ontology:topic_domain(gpl_derivative_work_trigger__broad_copyleft_reading, "software_licensing/copyright_law/open_source_governance").

domain_priors:requires_active_enforcement(gpl_derivative_work_trigger__broad_copyleft_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_derivative_work_trigger__broad_copyleft_reading, '8b64962a-f911-4a27-981d-4a4b96fbedfb').
narrative_ontology:cs_kernel_codification('8b64962a-f911-4a27-981d-4a4b96fbedfb', fixed_text).
narrative_ontology:cs_authority_grounding('8b64962a-f911-4a27-981d-4a4b96fbedfb', lineage).
narrative_ontology:cs_interpretation_layer_present('8b64962a-f911-4a27-981d-4a4b96fbedfb').
narrative_ontology:cs_reading_relation('8b64962a-f911-4a27-981d-4a4b96fbedfb', gpl_derivative_work_trigger__narrow_linking_permissive_reading, forecloses).
narrative_ontology:cs_reading_relation('8b64962a-f911-4a27-981d-4a4b96fbedfb', gpl_derivative_work_trigger__interface_boundary_reading, forecloses).
narrative_ontology:cs_axiom('8b64962a-f911-4a27-981d-4a4b96fbedfb', foundational, linking_creates_derivative_work).
narrative_ontology:cs_axiom_status(linking_creates_derivative_work, holdable).
narrative_ontology:cs_axiom_grounding('8b64962a-f911-4a27-981d-4a4b96fbedfb', linking_creates_derivative_work, conventional).
narrative_ontology:cs_axiom('8b64962a-f911-4a27-981d-4a4b96fbedfb', foundational, software_freedom_requires_source_disclosure).
narrative_ontology:cs_axiom_status(software_freedom_requires_source_disclosure, holdable).
narrative_ontology:cs_axiom_grounding('8b64962a-f911-4a27-981d-4a4b96fbedfb', software_freedom_requires_source_disclosure, deontological).
narrative_ontology:cs_reference_frame('8b64962a-f911-4a27-981d-4a4b96fbedfb', fsf_copyleft_doctrine).
narrative_ontology:cs_drift_state('8b64962a-f911-4a27-981d-4a4b96fbedfb', contemporary_software_development, gap(stable, minor, true)).
narrative_ontology:cs_created_at('8b64962a-f911-4a27-981d-4a4b96fbedfb', '').
narrative_ontology:cs_kernel_id(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_derivative_work_trigger).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__broad_copyleft_reading, open_source_community).
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__broad_copyleft_reading, downstream_users).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__broad_copyleft_reading, proprietary_software_vendors).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__broad_copyleft_reading, commercial_integrators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Authors and maintainers of GPL-licensed software, and organizations like the Free Software Foundation, who define and actively enforce the license terms, including the broad interpretation of derivative works. They initiate legal action to ensure compliance.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_licensors, agenda_setter,
    institutional, generational, arbitrage, global).

% Benefits from the expansion of the software commons, as more code is brought under open-source licenses. They gain access to source code for modification, study, and redistribution, fostering collaborative development and innovation.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, open_source_community, beneficiary,
    organized, generational, mobile, global).

% Users of software that incorporates GPL-licensed components. They benefit from the right to receive, modify, and redistribute the source code of the entire derivative work, ensuring their freedom to control the software they use.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, downstream_users, beneficiary,
    moderate, biographical, mobile, global).

% Develop and distribute proprietary software. This interpretation forces them to either disclose the source code of their entire product if it links to GPL code, or to avoid GPL components, incurring significant development costs or market limitations. They actively resist this interpretation.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, proprietary_software_vendors, payer,
    powerful, biographical, constrained, global).

% Integrate various software components, including open-source libraries, into commercial products or services. They face the same compliance burden as proprietary vendors, needing to ensure their integration methods do not trigger the derivative work clause under this broad reading.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, commercial_integrators, payer,
    organized, biographical, constrained, global).

% Advise clients on software licensing compliance and litigate disputes. They analyze the evolving legal landscape and the practical implications of different interpretations of copyleft licenses.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, software_lawyers, observer,
    analytical, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_derivative_work_trigger__broad_copyleft_reading, diffuse).
narrative_ontology:fixing_cost_class(gpl_derivative_work_trigger__broad_copyleft_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that software built upon GPL-licensed code remains free and open, fostering a collaborative ecosystem where contributions are shared and not enclosed by proprietary interests.
% TRANSFER_FUNCTION: Transfers the obligation to disclose source code from the original GPL licensor to anyone who links to or distributes derivative works, effectively expanding the commons of freely available and modifiable software.
% ABSENT_VOICES: Proprietary developers and commercial integrators who wish to leverage GPL code without disclosing their own source are structurally excluded from the framework of this interpretation; they would argue for more permissive linking rules but are kept out by the same legal principles this reading upholds.
% DISAPPEARANCE_RATIONALE: If this broad interpretation of derivative works vanished overnight, proprietary vendors would freely link to GPL code without disclosure, fragmenting the open-source commons, reducing user freedom, and fundamentally altering the collaborative dynamics of the software ecosystem.
% FOUNDING_PROBLEM: The original problem was to prevent proprietary enclosure of software built on publicly shared code, ensuring that software freedom (the right to use, study, modify, and distribute software) is preserved and propagated.
% FOUNDING_PROBLEM_CORROBORATION: Open source foundations, legal scholars specializing in Free and Open Source Software (FOSS), and many individual developers corroborate the ongoing need to protect software freedom from enclosure, citing continuous attempts by proprietary interests to circumvent copyleft obligations. This corroboration comes from outside the direct benefiting parties (e.g., independent legal analysis, academic research).
narrative_ontology:disappearance_verdict(gpl_derivative_work_trigger__broad_copyleft_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_derivative_work_trigger__broad_copyleft_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_derivative_work_trigger__broad_copyleft_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(gpl_derivative_work_trigger__broad_copyleft_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_derivative_work_trigger__broad_copyleft_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_derivative_work_trigger__broad_copyleft_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gpl_derivative_work_trigger__broad_copyleft_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gpl_derivative_work_trigger__broad_copyleft_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The `extractiveness` is high (0.7) because this interpretation imposes a significant cost (source code disclosure) on proprietary entities wishing to leverage GPL code. `suppression` is also high (0.8) as it actively suppresses alternative linking strategies that would avoid disclosure, backed by copyright law and legal enforcement. `theater_ratio` is low (0.1) because the enforcement actions are direct and functional, aimed at achieving compliance rather than mere performance. `resistance` is high (0.75) due to ongoing legal challenges and lobbying efforts by proprietary software vendors against this interpretation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of GPL licensors and the open-source community, this is a necessary 'rope' to coordinate software freedom and prevent enclosure. From the perspective of proprietary vendors, it is a 'snare' that extracts their intellectual property or forces costly workarounds. The engine's computation of per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   GPL licensors and the broader open-source community are the primary beneficiaries, as this interpretation ensures the continued freedom and growth of the software commons. Downstream users also benefit from guaranteed access to source code. Proprietary software vendors and commercial integrators are the primary targets/payers, as they bear the cost of compliance (source disclosure) or avoidance (re-engineering, using alternative licenses).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine structural feature of software licensing, or one specific interpretation of a contested legal kernel?',
    'Analysis of legal precedents and legislative changes across jurisdictions; comparison with alternative readings of the GPL''s derivative work clause.',
    'If it is merely one interpretation, its classification is contingent on the prevailing legal and technical consensus, and its persistence is subject to ongoing contestation. If it were a universally accepted structural feature, its classification would be more stable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Acknowledges this constraint as one reading of the ''gpl_derivative_work_trigger'' kernel.').

omega_variable(
    derivative_work_definition_ambiguity,
    'What constitutes a ''derivative work'' when linking software components, particularly with dynamic linking, remains a point of legal and technical ambiguity?',
    'Further court rulings, legislative clarification, or industry-wide technical standards that explicitly define the boundary between aggregation and derivation in software linking.',
    'If the definition narrows, the extractiveness and suppression of this constraint would decrease, potentially shifting its classification towards a more permissive ''rope'' or even ''piton'' if enforcement atrophies. If it broadens further, it would intensify.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(derivative_work_definition_ambiguity, empirical, 'Ambiguity in the legal definition of ''derivative work'' in software linking.').


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
narrative_ontology:measurement(gpl__be_t0, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(gpl__be_t5, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(gpl__be_t10, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(gpl__be_t15, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 15, 0.69).
narrative_ontology:measurement(gpl__be_t20, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 20, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t0, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(gpl__su_t5, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 5, 0.75).
narrative_ontology:measurement(gpl__su_t10, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 10, 0.78).
narrative_ontology:measurement(gpl__su_t15, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 15, 0.79).
narrative_ontology:measurement(gpl__su_t20, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 20, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_derivative_work_trigger__broad_copyleft_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'gpl_derivative_work_trigger' kernel. The 'narrow_linking_permissive_reading' and 'interface_boundary_reading' are sibling constraints that offer alternative interpretations of what constitutes a derivative work under GPL.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
