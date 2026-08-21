% ============================================================================
% CONSTRAINT STORY: permissive_license_text__copyleft_counterfactual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_permissive_license_text__copyleft_counterfactual_reading, []).

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
 *   constraint_id: permissive_license_text__copyleft_counterfactual_reading
 *   human_readable: Permissive License Exploitation (Copyleft Counterfactual Reading)
 *   domain: software_licensing/intellectual_property/technology_governance
 *
 * SUMMARY:
 *   This constraint represents the 'copyleft counterfactual' reading of the
 *   'permissive_license_text' kernel. It argues that the widespread adoption
 *   of permissive open-source licenses (e.g., MIT, Apache) without a
 *   reciprocity requirement structurally enables the exploitation of the
 *   open-source commons. From this perspective, proprietary software
 *   companies benefit by incorporating permissively licensed code into
 *   closed-source products without contributing back, thereby privatizing
 *   value created by the commons. Viral reciprocity licenses (like the GPL)
 *   are seen as a necessary alternative to protect the commons. The
 *   constraint is claimed as a Rope by its proponents (maximizing reuse) but
 *   operates as a Tangled Rope from the perspective of copyleft advocates due
 *   to the asymmetric extraction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(permissive_license_text__copyleft_counterfactual_reading, 0.75).
domain_priors:suppression_score(permissive_license_text__copyleft_counterfactual_reading, 0.65).
domain_priors:theater_ratio(permissive_license_text__copyleft_counterfactual_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(permissive_license_text__copyleft_counterfactual_reading, tangled_rope).
narrative_ontology:human_readable(permissive_license_text__copyleft_counterfactual_reading, "Permissive License Exploitation (Copyleft Counterfactual Reading)").
narrative_ontology:topic_domain(permissive_license_text__copyleft_counterfactual_reading, "software_licensing/intellectual_property/technology_governance").

domain_priors:requires_active_enforcement(permissive_license_text__copyleft_counterfactual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(permissive_license_text__copyleft_counterfactual_reading, 'd287d472-10b0-442c-9601-41ecd3fbfd50').
narrative_ontology:cs_kernel_codification('d287d472-10b0-442c-9601-41ecd3fbfd50', fixed_text).
narrative_ontology:cs_authority_grounding('d287d472-10b0-442c-9601-41ecd3fbfd50', practice).
narrative_ontology:cs_interpretation_layer_present('d287d472-10b0-442c-9601-41ecd3fbfd50').
narrative_ontology:cs_reading_relation('d287d472-10b0-442c-9601-41ecd3fbfd50', permissive_license_text__commons_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('d287d472-10b0-442c-9601-41ecd3fbfd50', permissive_license_text__corporate_moat_reading, coexists_with).
narrative_ontology:cs_axiom('d287d472-10b0-442c-9601-41ecd3fbfd50', foundational, reciprocity_is_essential_for_commons_sustainability).
narrative_ontology:cs_axiom_status(reciprocity_is_essential_for_commons_sustainability, holdable).
narrative_ontology:cs_axiom_grounding('d287d472-10b0-442c-9601-41ecd3fbfd50', reciprocity_is_essential_for_commons_sustainability, instrumental).
narrative_ontology:cs_axiom('d287d472-10b0-442c-9601-41ecd3fbfd50', foundational, unrestricted_reuse_leads_to_exploitation).
narrative_ontology:cs_axiom_status(unrestricted_reuse_leads_to_exploitation, holdable).
narrative_ontology:cs_axiom_grounding('d287d472-10b0-442c-9601-41ecd3fbfd50', unrestricted_reuse_leads_to_exploitation, empirically_contingent).
narrative_ontology:cs_reference_frame('d287d472-10b0-442c-9601-41ecd3fbfd50', reciprocal_commons_protection).
narrative_ontology:cs_drift_state('d287d472-10b0-442c-9601-41ecd3fbfd50', contemporary_software_ecosystem, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d287d472-10b0-442c-9601-41ecd3fbfd50', '').
narrative_ontology:cs_kernel_id(permissive_license_text__copyleft_counterfactual_reading, permissive_license_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(permissive_license_text__copyleft_counterfactual_reading, proprietary_software_companies).
narrative_ontology:constraint_victim(permissive_license_text__copyleft_counterfactual_reading, open_source_commons_contributors).
narrative_ontology:constraint_victim(permissive_license_text__copyleft_counterfactual_reading, copyleft_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(permissive_license_text__copyleft_counterfactual_reading, users_of_proprietary_software).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These companies leverage software components released under permissive licenses (e.g., MIT, Apache) to build proprietary derivative products without being required to contribute their changes back to the commons. They benefit from reduced development costs and the ability to privatize value.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, proprietary_software_companies, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(permissive_license_text__copyleft_counterfactual_reading, proprietary_software_companies, beneficiary).

% Individuals and small teams who contribute code under permissive licenses, often with the intent of maximizing reuse. From this reading's perspective, they bear the cost of their contributions being taken and privatized without reciprocal benefit to the commons.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, open_source_commons_contributors, payer,
    moderate, biographical, constrained, global).

% Organizations and individuals (e.g., Free Software Foundation) who champion 'viral' reciprocity licenses (like GPL) to ensure that derivative works remain free and contribute back to the commons. They are 'excluded' from the dominant narrative that frames permissive licenses as universally beneficial and bear the cost of seeing the commons exploited.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, copyleft_advocates, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(permissive_license_text__copyleft_counterfactual_reading, copyleft_advocates, excluded).

% Consumers who use proprietary software that incorporates permissively licensed open-source components. They benefit from the availability of these products but often lack the freedoms (to inspect, modify, share) that copyleft licenses would ensure.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, users_of_proprietary_software, beneficiary,
    moderate, immediate, constrained, global).

% The teams and individuals responsible for the ongoing development and governance of open-source projects. They choose the license for their projects, often balancing broad adoption (permissive) with commons protection (copyleft). This reading highlights the pressure to choose permissive licenses for wider adoption, even if it enables exploitation.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, open_source_project_maintainers, agenda_setter,
    organized, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Facilitates widespread reuse and integration of software components across diverse projects and commercial products by minimizing legal friction and maximizing compatibility.
% TRANSFER_FUNCTION: Transfers the value of open-source contributions (under permissive licenses) from the open-source commons to proprietary software companies, who then capture and privatize that value in derivative works.
% ABSENT_VOICES: Stronger copyleft advocates and those who prioritize the long-term health and reciprocal growth of the open-source commons are often marginalized in discussions that emphasize the immediate benefits of unrestricted reuse for commercial adoption.
% DISAPPEARANCE_RATIONALE: If permissive licenses vanished overnight, the software ecosystem would undergo a profound reorganization. Proprietary companies would face significantly higher development costs or be forced to adopt copyleft principles, leading to a different balance of power, innovation models, and software availability.
% FOUNDING_PROBLEM: The desire to maximize software reuse, interoperability, and adoption by reducing legal barriers, and to foster a vibrant open-source ecosystem through minimal restrictions.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of permissive licenses (often corporate legal teams and some open-source foundations) argue that the founding problem of maximizing reuse and adoption is still live. Copyleft advocates (e.g., Free Software Foundation, independent researchers) argue that while reuse is achieved, the problem of ensuring a sustainable, reciprocally-growing commons is exacerbated, and the original intent has been co-opted for private gain; economic analyses of value capture support this shifted-function reading.
narrative_ontology:disappearance_verdict(permissive_license_text__copyleft_counterfactual_reading, world_rearranges).
narrative_ontology:founding_problem_status(permissive_license_text__copyleft_counterfactual_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(permissive_license_text__copyleft_counterfactual_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(permissive_license_text__copyleft_counterfactual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(permissive_license_text__copyleft_counterfactual_reading, 0.75, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(permissive_license_text__copyleft_counterfactual_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(permissive_license_text__copyleft_counterfactual_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(permissive_license_text__copyleft_counterfactual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.75) reflects the value transferred from the open-source commons to proprietary entities without reciprocal contribution. Suppression (0.65) is moderate-high because the 'permissive' norm is actively promoted by powerful commercial interests, making it harder for copyleft alternatives to gain traction and effectively 'suppressing' the reciprocal model. Theater ratio is low (0.15) because the exploitation is a direct, functional outcome of the license structure, not a performative one. The increasing extractiveness and suppression over time reflect the growing scale of open-source adoption and the entrenchment of permissive licensing as the default for many projects, leading to greater opportunities for one-way value extraction.
 *
 * PERSPECTIVAL GAP:
 *   The 'permissive_license_text' kernel is subject to significant perspectival divergence. Proponents of permissive licenses (e.g., many corporate legal teams, some open-source foundations) view it as a pure 'Rope' that maximizes coordination and innovation by minimizing friction. This 'copyleft counterfactual' reading, however, views the same structure as a 'Tangled Rope' or even a 'Snare' due to the inherent asymmetry that enables exploitation. The engine's computation of per-seat classifications will highlight this divergence, showing a 'Rope' for beneficiaries and a 'Tangled Rope' or 'Snare' for victims.
 *
 * DIRECTIONALITY LOGIC:
 *   Proprietary software companies are clear beneficiaries (d=0.0-0.1) as they gain free access to code without obligation. Open-source commons contributors and copyleft advocates are targets (d=0.8-1.0) as they see their contributions or the commons they champion exploited. Open-source project maintainers sit closer to symmetric (d=0.4-0.6) as they balance the benefits of broad adoption with the risks of exploitation, often making a constrained choice. Users of proprietary software are indirect beneficiaries (d=0.2-0.3) as they get products, but without the freedoms of open source.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    permissive_vs_copyleft_impact_on_innovation,
    'Does permissive licensing genuinely foster more overall innovation and adoption than copyleft licensing, or does it primarily shift value capture to proprietary entities at the expense of the commons?',
    'Longitudinal economic studies comparing innovation rates, project sustainability, and value distribution in ecosystems dominated by permissive vs. copyleft licenses, controlling for other factors.',
    'If permissive licenses are shown to primarily shift value without a net gain in innovation, the constraint''s extractiveness would be further validated. If they genuinely foster more innovation, the coordination function would be stronger, potentially reclassifying it closer to a Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(permissive_vs_copyleft_impact_on_innovation, empirical, 'The true impact of permissive vs. copyleft licenses on innovation and value distribution.').

omega_variable(
    coordination_vs_extraction_boundary,
    'Is the ''coordination'' function of permissive licenses (ease of reuse) structurally separable from the ''extraction'' function (privatization of commons value), or are they inherently intertwined?',
    'Analysis of hybrid licensing models or legal frameworks that attempt to achieve maximal reuse while mandating some form of reciprocal contribution. If such models prove viable and widely adopted, it suggests separability.',
    'If separable, the extraction component is clearly an overhead; if inseparable, a portion of the measured extraction might be considered an unavoidable cost of achieving broad coordination, potentially lowering effective extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary, conceptual, 'Whether the coordination and extraction functions of permissive licenses are structurally separable.').

omega_variable(
    kernel_reading_identity,
    'Is this constraint a valid ''copyleft counterfactual'' reading of the ''permissive_license_text'' kernel, or is it better understood as a distinct constraint about ''open_source_exploitation''?',
    'Consensus among legal scholars and open-source ethicists on whether the exploitation is an inherent feature of the permissive license text itself (kernel reading) or a separate phenomenon that merely uses permissive licenses as a tool (distinct constraint).',
    'If reclassified as a distinct constraint, it would lose its direct linkage to the ''permissive_license_text'' kernel, altering network effects and committer-frame analysis. If validated as a reading, it strengthens the kernel''s contested nature.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Confirms this constraint as a specific reading of the permissive license text kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(permissive_license_text__copyleft_counterfactual_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perm_tr_t0, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(perm_tr_t6, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 6, 0.12).
narrative_ontology:measurement(perm_tr_t12, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 12, 0.14).
narrative_ontology:measurement(perm_tr_t18, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 18, 0.15).
narrative_ontology:measurement(perm_tr_t24, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 24, 0.15).
narrative_ontology:measurement(perm_tr_t30, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 30, 0.15).

% Extraction over time
narrative_ontology:measurement(perm_be_t0, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(perm_be_t6, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 6, 0.62).
narrative_ontology:measurement(perm_be_t12, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 12, 0.68).
narrative_ontology:measurement(perm_be_t18, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 18, 0.72).
narrative_ontology:measurement(perm_be_t24, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 24, 0.74).
narrative_ontology:measurement(perm_be_t30, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 30, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(perm_su_t0, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(perm_su_t6, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 6, 0.5).
narrative_ontology:measurement(perm_su_t12, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 12, 0.56).
narrative_ontology:measurement(perm_su_t18, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 18, 0.6).
narrative_ontology:measurement(perm_su_t24, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 24, 0.63).
narrative_ontology:measurement(perm_su_t30, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 30, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(permissive_license_text__copyleft_counterfactual_reading, information_standard).
narrative_ontology:affects_constraint(permissive_license_text__copyleft_counterfactual_reading, open_source_project_sustainability).
narrative_ontology:affects_constraint(permissive_license_text__copyleft_counterfactual_reading, proprietary_software_market_dominance).
narrative_ontology:affects_constraint(permissive_license_text__copyleft_counterfactual_reading, commons_coordination_reading).
narrative_ontology:affects_constraint(permissive_license_text__copyleft_counterfactual_reading, corporate_moat_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'permissive_license_text' kernel, focusing on the exploitation enabled by the lack of reciprocity. It is linked to its sibling readings, 'commons_coordination_reading' and 'corporate_moat_reading', as part of a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
