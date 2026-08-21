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
    narrative_ontology:constraint_vindicates/2,
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
 *   This constraint represents the 'compact federalism' reading of the
 *   provincial sovereignty boundary, where Confederation is understood as a
 *   compact among sovereign provinces, retaining residual sovereignty, with
 *   exit negotiable under duress. This reading emphasizes provincial autonomy
 *   and conditional federal authority. It is one of several competing
 *   interpretations of the foundational nature of the federation, with
 *   significant implications for policy areas like resource governance,
 *   equalization, and climate policy. The metrics reflect the ongoing tension
 *   between coordination and extraction inherent in this interpretation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(provincial_sovereignty_boundary__compact_federalism, 0.65).
domain_priors:suppression_score(provincial_sovereignty_boundary__compact_federalism, 0.7).
domain_priors:theater_ratio(provincial_sovereignty_boundary__compact_federalism, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, extractiveness, 0.65).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(provincial_sovereignty_boundary__compact_federalism, tangled_rope).
narrative_ontology:human_readable(provincial_sovereignty_boundary__compact_federalism, "Provincial Sovereignty Boundary (Compact Federalism Reading)").
narrative_ontology:topic_domain(provincial_sovereignty_boundary__compact_federalism, "political_economy/federalism/resource_governance").

domain_priors:requires_active_enforcement(provincial_sovereignty_boundary__compact_federalism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(provincial_sovereignty_boundary__compact_federalism, '0657a914-2ef0-4065-a466-2e28a72fc34e').
narrative_ontology:cs_kernel_codification('0657a914-2ef0-4065-a466-2e28a72fc34e', fixed_text).
narrative_ontology:cs_authority_grounding('0657a914-2ef0-4065-a466-2e28a72fc34e', lineage).
narrative_ontology:cs_interpretation_layer_present('0657a914-2ef0-4065-a466-2e28a72fc34e').
narrative_ontology:cs_reading_relation('0657a914-2ef0-4065-a466-2e28a72fc34e', provincial_sovereignty_boundary__constitutional_subordination, forecloses).
narrative_ontology:cs_reading_relation('0657a914-2ef0-4065-a466-2e28a72fc34e', provincial_sovereignty_boundary__resource_sovereignty_primacy, influences).
narrative_ontology:cs_axiom('0657a914-2ef0-4065-a466-2e28a72fc34e', foundational, provincial_consent_is_foundational).
narrative_ontology:cs_axiom_status(provincial_consent_is_foundational, holdable).
narrative_ontology:cs_axiom_grounding('0657a914-2ef0-4065-a466-2e28a72fc34e', provincial_consent_is_foundational, conventional).
narrative_ontology:cs_axiom('0657a914-2ef0-4065-a466-2e28a72fc34e', foundational, negotiated_exit_is_a_right).
narrative_ontology:cs_axiom_status(negotiated_exit_is_a_right, holdable).
narrative_ontology:cs_axiom_grounding('0657a914-2ef0-4065-a466-2e28a72fc34e', negotiated_exit_is_a_right, conventional).
narrative_ontology:cs_reference_frame('0657a914-2ef0-4065-a466-2e28a72fc34e', confederation_compact_era).
narrative_ontology:cs_drift_state('0657a914-2ef0-4065-a466-2e28a72fc34e', contemporary_federal_relations, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0657a914-2ef0-4065-a466-2e28a72fc34e', '').
narrative_ontology:cs_kernel_id(provincial_sovereignty_boundary__compact_federalism, provincial_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__compact_federalism, provinces_asserting_sovereignty).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__compact_federalism, federal_government).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__compact_federalism, federal_government).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__compact_federalism, provinces_seeking_federal_support).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__compact_federalism, provinces_seeking_federal_support).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__compact_federalism, citizens_within_provinces).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__compact_federalism, citizens_within_provinces).
narrative_ontology:constraint_vindicates(provincial_sovereignty_boundary__compact_federalism, provincial_autonomy_doctrine).
narrative_ontology:constraint_vindicates(provincial_sovereignty_boundary__compact_federalism, subsidiarity_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These provinces interpret the federation as a voluntary compact, retaining significant residual sovereignty. They benefit from autonomy over their jurisdictions and negotiate federal policies, but face constraints from the overall federal framework. Exit is considered a negotiable, albeit difficult, option.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, provinces_asserting_sovereignty, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(provincial_sovereignty_boundary__compact_federalism, provinces_asserting_sovereignty, beneficiary).

% The federal government operates within the compact, requiring provincial consent for certain initiatives and engaging in complex negotiations (e.g., equalization, climate policy). It benefits from the stability of the compact but bears the costs of conditional authority and the need for constant negotiation. Its ability to unilaterally impose policy is constrained.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, federal_government, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(provincial_sovereignty_boundary__compact_federalism, federal_government, payer).

% These provinces often rely on federal transfers and shared programs. While they benefit from federal support, they are also constrained by the terms of the compact and federal policy, which may not always align with their specific needs. Their negotiating leverage is often less than that of larger, more assertive provinces.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, provinces_seeking_federal_support, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(provincial_sovereignty_boundary__compact_federalism, provinces_seeking_federal_support, beneficiary).

% Citizens benefit from local governance and services provided by their provincial governments, which are seen as closer and more responsive. However, they also bear the costs of federal-provincial disputes and the complexities of shared jurisdiction, often through taxation or policy inconsistencies. Their exit options are limited to moving between provinces or countries.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, citizens_within_provinces, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(provincial_sovereignty_boundary__compact_federalism, citizens_within_provinces, payer).

% These academics analyze the legal and historical basis of federalism, often debating the nature of the original compact and the evolution of provincial and federal powers. They provide critical commentary on the constraint's operation but do not directly participate in its enforcement or benefit from its extraction.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, constitutional_scholars, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates diverse provincial interests and populations into a single federal state, allowing for shared defense, economic union, and inter-provincial resource sharing while preserving distinct regional identities and governance structures.
% TRANSFER_FUNCTION: Moves political authority and fiscal resources between federal and provincial levels, often through equalization payments and shared-cost programs, subject to negotiation and provincial consent.
% ABSENT_VOICES: Indigenous nations, whose inherent sovereignty predates Confederation, are often excluded from the federal-provincial compact framework, despite their lands and rights being directly impacted. They would argue for nation-to-nation relationships rather than being treated as a 'third order' of government within the existing compact.
% DISAPPEARANCE_RATIONALE: If the compact federalism reading vanished, the entire structure of federal-provincial relations would collapse. Provinces would either assert full independence or be fully subordinated to a unitary federal authority, leading to a complete reorganization of governance, resource allocation, and national identity.
% FOUNDING_PROBLEM: The original problem was to unite disparate colonies with distinct identities and economies into a single political entity capable of self-governance and defense, while respecting existing colonial boundaries and powers.
% FOUNDING_PROBLEM_CORROBORATION: Historians and political scientists generally corroborate that the problem of balancing unity with diversity remains live, though its specific manifestations (e.g., climate policy, resource development) have evolved. Provincial governments consistently attest to the ongoing need to assert their distinct interests within the federation.
narrative_ontology:disappearance_verdict(provincial_sovereignty_boundary__compact_federalism, world_rearranges).
narrative_ontology:founding_problem_status(provincial_sovereignty_boundary__compact_federalism, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(provincial_sovereignty_boundary__compact_federalism, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(provincial_sovereignty_boundary__compact_federalism, 'none', 1).
narrative_ontology:epsilon_provenance(provincial_sovereignty_boundary__compact_federalism, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness (0.65) is moderate-high because while provinces retain sovereignty, the federal structure still extracts resources (e.g., through equalization payments) and limits provincial policy choices. Suppression (0.70) is also moderate-high, as exit is 'negotiable under duress,' implying significant barriers and federal enforcement to maintain the compact. Theater ratio (0.25) is low-moderate; while genuine coordination occurs, some federal-provincial negotiations can be performative, masking underlying power dynamics. Accessibility collapse (0.60) is moderate, as full independence is not completely foreclosed but is highly constrained. Resistance (0.60) is moderate-high, reflecting frequent provincial challenges to federal authority under this reading. The temporal measurements show a gradual increase in extractiveness and suppression, suggesting a slow federal creep or increasing pressure on provinces over time, even within this 'compact' framework.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of provinces asserting sovereignty, the constraint is a necessary compact that preserves their distinctiveness, even if it involves some give-and-take. From the federal government's perspective, it's a framework for national unity that requires managing diverse provincial demands. The engine's per-seat classification will highlight how these different structural positions lead to divergent experiences of the same constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Provinces asserting sovereignty are beneficiaries of retained autonomy but also payers of the compact's terms. The federal government is a beneficiary of the unified state but a payer in terms of conditional authority and negotiation costs. Provinces seeking federal support are beneficiaries of transfers but payers of federal policy alignment. Citizens are diffuse beneficiaries of governance but indirect payers of federal-provincial friction. This complex interplay leads to a 'tangled rope' classification, where coordination and extraction are intertwined.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    compact_vs_imposition,
    'Is the federal structure, under this reading, a genuinely voluntary compact or an imposed framework that provinces merely accommodate?',
    'Analysis of historical documents and legal precedents regarding provincial entry into Confederation, and the actual mechanisms and outcomes of provincial attempts to negotiate exit or assert greater autonomy.',
    'If primarily an imposition, the effective extractiveness and suppression would be higher for provinces, shifting the classification closer to a Snare for provincial seats. If genuinely a compact, the coordination function is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compact_vs_imposition, conceptual, 'Ambiguity regarding the voluntary nature of the federal compact.').

omega_variable(
    cost_of_provincial_exit,
    'What is the true economic, social, and political cost of exit for a province, and how does it compare to the benefits of remaining within the federation?',
    'Detailed economic modeling of secession scenarios, analysis of public opinion on independence, and historical case studies of sub-national entities attempting to leave larger federations.',
    'If the cost of exit is prohibitively high, the ''negotiable under duress'' clause becomes largely theoretical, increasing the effective suppression for provinces and pushing the classification towards a Snare. If costs are manageable, provincial leverage is greater.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cost_of_provincial_exit, empirical, 'Uncertainty about the real-world feasibility and cost of provincial exit.').

omega_variable(
    federal_constraint_authenticity,
    'Is the federal government genuinely constrained by provincial consent, or does it strategically accommodate provincial demands while maintaining ultimate authority?',
    'Longitudinal study of federal-provincial negotiations, examining instances where federal initiatives were genuinely blocked or significantly altered by provincial opposition versus instances of symbolic accommodation.',
    'If federal constraint is largely performative, the federal government''s directionality shifts closer to a full beneficiary, increasing its effective extraction from the system. If genuine, the system''s coordination function is more robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federal_constraint_authenticity, empirical, 'Whether federal authority is truly conditional or merely strategically flexible.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(provincial_sovereignty_boundary__compact_federalism, 1982, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prov_tr_t1982, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 1982, 0.15).
narrative_ontology:measurement(prov_tr_t1992, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 1992, 0.18).
narrative_ontology:measurement(prov_tr_t2002, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 2002, 0.21).
narrative_ontology:measurement(prov_tr_t2012, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 2012, 0.23).
narrative_ontology:measurement(prov_tr_t2024, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 2024, 0.25).

% Extraction over time
narrative_ontology:measurement(prov_be_t1982, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 1982, 0.55).
narrative_ontology:measurement(prov_be_t1992, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 1992, 0.58).
narrative_ontology:measurement(prov_be_t2002, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 2002, 0.61).
narrative_ontology:measurement(prov_be_t2012, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 2012, 0.63).
narrative_ontology:measurement(prov_be_t2024, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(prov_su_t1982, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 1982, 0.6).
narrative_ontology:measurement(prov_su_t1992, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 1992, 0.64).
narrative_ontology:measurement(prov_su_t2002, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 2002, 0.67).
narrative_ontology:measurement(prov_su_t2012, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 2012, 0.69).
narrative_ontology:measurement(prov_su_t2024, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(provincial_sovereignty_boundary__compact_federalism, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
