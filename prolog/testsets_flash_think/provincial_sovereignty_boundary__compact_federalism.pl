% ============================================================================
% CONSTRAINT STORY: provincial_sovereignty_boundary__compact_federalism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_provincial_sovereignty_boundary__compact_federalism, []).

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
 *   constraint_id: provincial_sovereignty_boundary__compact_federalism
 *   human_readable: Provincial Sovereignty Boundary (Compact Federalism Reading)
 *   domain: political_economy/federalism/resource_governance
 *
 * SUMMARY:
 *   This constraint story instantiates the 'compact federalism' reading of
 *   the provincial sovereignty boundary kernel. This reading posits that
 *   Confederation was a compact among sovereign provinces, which retain
 *   residual sovereignty, and that exit from the federation is negotiable
 *   under duress. Federal authority is conditional on provincial consent for
 *   key matters, equalization is negotiable, and climate policy is subject to
 *   provincial override. The constraint is claimed as a 'rope' by this
 *   reading, emphasizing coordination among equals, but the authored metrics
 *   reflect the analytical reality of ongoing extraction and suppression
 *   within the federal framework.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(provincial_sovereignty_boundary__compact_federalism, 0.6).
domain_priors:suppression_score(provincial_sovereignty_boundary__compact_federalism, 0.5).
domain_priors:theater_ratio(provincial_sovereignty_boundary__compact_federalism, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, extractiveness, 0.6).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(provincial_sovereignty_boundary__compact_federalism, rope).
narrative_ontology:human_readable(provincial_sovereignty_boundary__compact_federalism, "Provincial Sovereignty Boundary (Compact Federalism Reading)").
narrative_ontology:topic_domain(provincial_sovereignty_boundary__compact_federalism, "political_economy/federalism/resource_governance").

domain_priors:requires_active_enforcement(provincial_sovereignty_boundary__compact_federalism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(provincial_sovereignty_boundary__compact_federalism, '106f6f8b-3790-44f0-9bd5-30a169f3709b').
narrative_ontology:cs_kernel_codification('106f6f8b-3790-44f0-9bd5-30a169f3709b', fixed_text).
narrative_ontology:cs_authority_grounding('106f6f8b-3790-44f0-9bd5-30a169f3709b', lineage).
narrative_ontology:cs_interpretation_layer_present('106f6f8b-3790-44f0-9bd5-30a169f3709b').
narrative_ontology:cs_reading_relation('106f6f8b-3790-44f0-9bd5-30a169f3709b', provincial_sovereignty_boundary__constitutional_subordination, forecloses).
narrative_ontology:cs_reading_relation('106f6f8b-3790-44f0-9bd5-30a169f3709b', provincial_sovereignty_boundary__resource_sovereignty_primacy, coexists_with).
narrative_ontology:cs_axiom('106f6f8b-3790-44f0-9bd5-30a169f3709b', foundational, provinces_retain_residual_sovereignty).
narrative_ontology:cs_axiom_status(provinces_retain_residual_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('106f6f8b-3790-44f0-9bd5-30a169f3709b', provinces_retain_residual_sovereignty, conventional).
narrative_ontology:cs_axiom('106f6f8b-3790-44f0-9bd5-30a169f3709b', foundational, federal_authority_requires_provincial_consent_on_key_matters).
narrative_ontology:cs_axiom_status(federal_authority_requires_provincial_consent_on_key_matters, holdable).
narrative_ontology:cs_axiom_grounding('106f6f8b-3790-44f0-9bd5-30a169f3709b', federal_authority_requires_provincial_consent_on_key_matters, conventional).
narrative_ontology:cs_reference_frame('106f6f8b-3790-44f0-9bd5-30a169f3709b', original_confederation_compact).
narrative_ontology:cs_drift_state('106f6f8b-3790-44f0-9bd5-30a169f3709b', contemporary_federal_relations, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('106f6f8b-3790-44f0-9bd5-30a169f3709b', '').
narrative_ontology:cs_kernel_id(provincial_sovereignty_boundary__compact_federalism, provincial_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__compact_federalism, federal_government).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__compact_federalism, resource_poor_provinces).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__compact_federalism, all_provinces).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__compact_federalism, provinces_seeking_autonomy).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__compact_federalism, resource_rich_provinces).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__compact_federalism, citizens_of_provinces).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__compact_federalism, citizens_of_provinces).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the federal system, collects national taxes, and provides shared services. Benefits from the stability of the federation and the ability to implement national policies, even if requiring provincial consent. Seeks to maintain a strong federal presence.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, federal_government, agenda_setter,
    institutional, generational, constrained, national).

% These provinces assert their residual sovereignty and resist federal encroachment on their jurisdiction. They bear the costs of federal policies they disagree with and the effort required to negotiate or challenge federal authority. Exit is a theoretical option, but highly constrained and under duress.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, provinces_seeking_autonomy, payer,
    organized, generational, constrained, national).

% Provinces with significant natural resources, they contribute disproportionately to federal equalization programs and often chafe under federal environmental or economic policies that impact their resource sector. They view federal authority as extracting from their wealth.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, resource_rich_provinces, payer,
    organized, generational, constrained, national).

% These provinces benefit significantly from federal equalization payments and shared services, which help maintain comparable public services across the federation. They generally support a strong federal role in maintaining national standards and transfers.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, resource_poor_provinces, beneficiary,
    organized, biographical, constrained, national).

% Interprets the Constitution and adjudicates disputes between federal and provincial governments, shaping the practical boundaries of sovereignty. Its rulings define the limits of both federal and provincial powers within the compact.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, supreme_court_of_canada, observer,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(provincial_sovereignty_boundary__compact_federalism, supreme_court_of_canada, agenda_setter).

% Benefit from the stability and shared services of the federation, but also bear the costs of intergovernmental disputes and the compromises made in policy areas like climate or resource development. Their daily lives are shaped by the balance of federal and provincial powers.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, citizens_of_provinces, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(provincial_sovereignty_boundary__compact_federalism, citizens_of_provinces, payer).

% Possess inherent sovereignty predating Confederation, which is often not fully recognized or accommodated by either federal or provincial claims. They are frequently excluded from federal-provincial negotiations that directly impact their lands and rights, despite being a distinct order of government.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, indigenous_nations, excluded,
    organized, generational, identity_locked, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for shared governance, economic union, and collective defense among distinct provincial entities, allowing for national policies while preserving provincial distinctiveness and autonomy.
% TRANSFER_FUNCTION: Involves the transfer of some provincial legislative and fiscal autonomy to the federal level for national purposes, alongside fiscal transfers (e.g., equalization payments) between provinces via the federal government.
% ABSENT_VOICES: Indigenous nations, whose inherent sovereignty predates the compact and is often marginalized in federal-provincial power-sharing discussions, would argue for their own distinct place in the constitutional order. Secessionist movements, advocating for full provincial independence, are also structurally excluded from the compact's internal negotiations.
% DISAPPEARANCE_RATIONALE: If the understanding of Confederation as a compact among sovereign provinces vanished, the Canadian federation would either centralize significantly (if a constitutional subordination reading prevailed) or fragment (if provinces asserted absolute, unconstrained sovereignty), leading to a complete reorganization of governance, resource allocation, and inter-provincial relations.
% FOUNDING_PROBLEM: To unite disparate British North American colonies into a single entity for defense, economic union, and shared governance, while preserving the distinct identities, legislative powers, and cultural specificities of the original provinces.
% FOUNDING_PROBLEM_CORROBORATION: Historians, constitutional scholars, and political scientists (outside of direct federal or provincial political actors) widely corroborate the original intent to form a compact among distinct entities. The ongoing tension over provincial powers and federal-provincial negotiations confirms the problem's continued relevance in contemporary governance.
narrative_ontology:disappearance_verdict(provincial_sovereignty_boundary__compact_federalism, world_rearranges).
narrative_ontology:founding_problem_status(provincial_sovereignty_boundary__compact_federalism, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(provincial_sovereignty_boundary__compact_federalism, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(provincial_sovereignty_boundary__compact_federalism, 'none', 1).
narrative_ontology:epsilon_provenance(provincial_sovereignty_boundary__compact_federalism, 0.6, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(provincial_sovereignty_boundary__compact_federalism_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(provincial_sovereignty_boundary__compact_federalism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(provincial_sovereignty_boundary__compact_federalism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.6) reflects the ongoing transfer of provincial autonomy and resources to the federal level, often through mechanisms like equalization or shared-cost programs, which are not always fully consensual. Suppression (0.5) arises from the federal government's ability to assert its jurisdiction and limit provincial actions, making full provincial autonomy difficult to achieve. Resistance (0.7) is high due to frequent intergovernmental disputes and provincial challenges to federal authority. The theater ratio (0.2) is relatively low, indicating that while there are performative aspects to sovereignty claims, the underlying negotiations and power struggles are real. The claimed type is 'rope' because this reading emphasizes the voluntary, cooperative nature of the compact, even as the metrics reveal a more extractive reality.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the federal government and beneficiary provinces, the compact functions as a necessary coordination mechanism. From the perspective of provinces seeking greater autonomy or those disproportionately contributing to federal programs, the same structure operates with significant extraction and suppression of their sovereign powers. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal government and resource-poor provinces are beneficiaries, gaining from the stability and transfers within the compact. Provinces seeking autonomy and resource-rich provinces are payers, bearing the costs of federal policies and contributing to equalization. The Supreme Court acts as an observer and occasional agenda-setter, interpreting the compact's boundaries. Indigenous nations are excluded, their inherent sovereignty often unacknowledged by the compact's terms.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    compact_vs_constitution_ambiguity,
    'Is the foundation of the Canadian federation primarily a ''compact'' among sovereign entities (as this reading asserts) or a ''constitution'' that is supreme law over all its parts (as the ''constitutional_subordination'' reading asserts)?',
    'A definitive Supreme Court ruling or constitutional amendment that explicitly clarifies the foundational nature of the federation, or a political consensus that resolves the ongoing debate.',
    'If resolved as a pure compact, provincial powers and exit options would be significantly strengthened. If resolved as a supreme constitution, federal authority would be enhanced, and provincial autonomy diminished.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(compact_vs_constitution_ambiguity, conceptual, 'Ambiguity regarding the foundational nature of the Canadian federation.').

omega_variable(
    duress_definition_ambiguity,
    'What constitutes ''duress'' in the context of provincial exit negotiations, and what are the legitimate conditions under which a province could negotiate its departure from the federation?',
    'Establishment of clear legal and political precedents for provincial secession or a constitutional mechanism for negotiated exit, defining the thresholds and processes for ''duress''.',
    'Clarifying ''duress'' would either legitimize provincial exit as a viable, albeit difficult, option (strengthening provincial bargaining power) or effectively close off the option, reinforcing federal supremacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(duress_definition_ambiguity, preference, 'Uncertainty regarding the conditions for provincial exit under duress.').

omega_variable(
    residual_sovereignty_scope,
    'What are the precise limits and scope of ''residual sovereignty'' retained by the provinces, particularly in areas of shared or evolving jurisdiction (e.g., climate change, social policy)?',
    'A series of definitive Supreme Court rulings on specific jurisdictional disputes, or intergovernmental agreements that clearly delineate federal and provincial powers in contested areas.',
    'A broader interpretation of residual sovereignty would empower provinces, while a narrower one would expand federal authority, impacting policy implementation and resource allocation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(residual_sovereignty_scope, empirical, 'Ambiguity regarding the extent of provincial residual sovereignty.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(provincial_sovereignty_boundary__compact_federalism, 1867, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prov_tr_t1867, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 1867, 0.1).
narrative_ontology:measurement(prov_tr_t1900, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 1900, 0.12).
narrative_ontology:measurement(prov_tr_t1950, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 1950, 0.15).
narrative_ontology:measurement(prov_tr_t1982, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 1982, 0.18).
narrative_ontology:measurement(prov_tr_t2000, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 2000, 0.19).
narrative_ontology:measurement(prov_tr_t2024, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(prov_be_t1867, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 1867, 0.4).
narrative_ontology:measurement(prov_be_t1900, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 1900, 0.45).
narrative_ontology:measurement(prov_be_t1950, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 1950, 0.5).
narrative_ontology:measurement(prov_be_t1982, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 1982, 0.55).
narrative_ontology:measurement(prov_be_t2000, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 2000, 0.58).
narrative_ontology:measurement(prov_be_t2024, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 2024, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(prov_su_t1867, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 1867, 0.3).
narrative_ontology:measurement(prov_su_t1900, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 1900, 0.35).
narrative_ontology:measurement(prov_su_t1950, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 1950, 0.4).
narrative_ontology:measurement(prov_su_t1982, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 1982, 0.45).
narrative_ontology:measurement(prov_su_t2000, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 2000, 0.48).
narrative_ontology:measurement(prov_su_t2024, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 2024, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(provincial_sovereignty_boundary__compact_federalism, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'provincial_sovereignty_boundary' kernel, alongside 'constitutional_subordination' and 'resource_sovereignty_primacy'. Each reading presents a distinct structural claim about federal-provincial relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
