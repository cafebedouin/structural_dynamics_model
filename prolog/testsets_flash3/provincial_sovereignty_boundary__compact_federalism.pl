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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   This constraint represents the 'compact federalism' reading of provincial
 *   sovereignty, where provinces are seen as retaining significant residual
 *   sovereignty from the act of confederation, and federal authority is
 *   conditional on provincial consent. This reading emphasizes the negotiable
 *   nature of federal-provincial relations, particularly regarding resource
 *   governance, equalization, and climate policy. It is one of several
 *   competing interpretations of the constitutional division of powers.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(provincial_sovereignty_boundary__compact_federalism, 0.45).
domain_priors:suppression_score(provincial_sovereignty_boundary__compact_federalism, 0.3).
domain_priors:theater_ratio(provincial_sovereignty_boundary__compact_federalism, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, extractiveness, 0.45).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(provincial_sovereignty_boundary__compact_federalism, tangled_rope).
narrative_ontology:human_readable(provincial_sovereignty_boundary__compact_federalism, "Provincial Sovereignty Boundary (Compact Federalism Reading)").
narrative_ontology:topic_domain(provincial_sovereignty_boundary__compact_federalism, "political_economy/federalism/resource_governance").

domain_priors:requires_active_enforcement(provincial_sovereignty_boundary__compact_federalism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(provincial_sovereignty_boundary__compact_federalism, 'd74c4c95-f737-4090-934e-59a6c40ebd90').
narrative_ontology:cs_kernel_codification('d74c4c95-f737-4090-934e-59a6c40ebd90', fixed_text).
narrative_ontology:cs_authority_grounding('d74c4c95-f737-4090-934e-59a6c40ebd90', lineage).
narrative_ontology:cs_interpretation_layer_present('d74c4c95-f737-4090-934e-59a6c40ebd90').
narrative_ontology:cs_reading_relation('d74c4c95-f737-4090-934e-59a6c40ebd90', provincial_sovereignty_boundary__constitutional_subordination, forecloses).
narrative_ontology:cs_reading_relation('d74c4c95-f737-4090-934e-59a6c40ebd90', provincial_sovereignty_boundary__resource_sovereignty_primacy, coexists_with).
narrative_ontology:cs_axiom('d74c4c95-f737-4090-934e-59a6c40ebd90', foundational, confederation_as_provincial_compact).
narrative_ontology:cs_axiom_status(confederation_as_provincial_compact, holdable).
narrative_ontology:cs_axiom_grounding('d74c4c95-f737-4090-934e-59a6c40ebd90', confederation_as_provincial_compact, conventional).
narrative_ontology:cs_axiom('d74c4c95-f737-4090-934e-59a6c40ebd90', foundational, residual_provincial_sovereignty).
narrative_ontology:cs_axiom_status(residual_provincial_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('d74c4c95-f737-4090-934e-59a6c40ebd90', residual_provincial_sovereignty, conventional).
narrative_ontology:cs_reference_frame('d74c4c95-f737-4090-934e-59a6c40ebd90', original_confederation_compact).
narrative_ontology:cs_drift_state('d74c4c95-f737-4090-934e-59a6c40ebd90', contemporary_federal_centralization_pressure, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('d74c4c95-f737-4090-934e-59a6c40ebd90', '').
narrative_ontology:cs_kernel_id(provincial_sovereignty_boundary__compact_federalism, provincial_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__compact_federalism, provinces_asserting_autonomy).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__compact_federalism, federal_government).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__compact_federalism, national_equalization_recipients).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These provinces benefit from a reading of federalism that grants them significant residual sovereignty, allowing them to negotiate federal authority, control resource development, and assert conditional consent on national policies. Exit is seen as a negotiable, albeit high-cost, option.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, provinces_asserting_autonomy, beneficiary,
    institutional, generational, constrained, regional).

% The federal government bears the cost of this reading through reduced authority over national policy, increased negotiation overhead for inter-provincial agreements, and challenges to its equalization and climate policy mandates. Its ability to act unilaterally is constrained.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, federal_government, payer,
    institutional, generational, constrained, national).

% These provinces and their citizens rely on federal equalization payments. Under this reading, equalization becomes a negotiable federal transfer rather than a constitutional obligation, potentially reducing their funding and increasing their vulnerability to provincial political leverage.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, national_equalization_recipients, payer,
    organized, biographical, trapped, national).

% Advocate for strong, unified national climate policy. This reading of federalism allows provinces to override or significantly delay federal climate initiatives, fragmenting national efforts and undermining their goals. They are excluded from direct policy-making under this framework.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, national_climate_advocates, excluded,
    moderate, generational, constrained, national).

% Analyze the historical and legal basis of federal-provincial relations, interpreting constitutional texts and judicial precedents. They observe the practical implications of this reading on governance and inter-governmental disputes, without directly participating in the political contest.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, constitutional_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the distribution of powers and responsibilities between federal and provincial governments, allowing for regional autonomy and diverse policy approaches within a larger confederation.
% TRANSFER_FUNCTION: Transfers authority and policy control from the federal center to the provinces, particularly in areas like resource management, environmental policy, and social programs, making federal initiatives conditional on provincial consent.
% ABSENT_VOICES: National interest groups, particularly those advocating for unified climate action or stronger social safety nets, are marginalized when provincial consent becomes paramount. They would argue for a more centralized federal authority to address national challenges.
% DISAPPEARANCE_RATIONALE: If this reading of provincial sovereignty vanished, the federal government would assert greater authority, potentially leading to more unified national policies on climate and equalization. Provinces would lose significant leverage, and the balance of power within the confederation would fundamentally shift.
% FOUNDING_PROBLEM: The original confederation sought to unite diverse colonial entities while preserving their distinct identities and local governance structures, balancing national unity with regional autonomy.
% FOUNDING_PROBLEM_CORROBORATION: Historians and political scientists outside of provincial governments corroborate that balancing national unity with regional autonomy remains a live and ongoing challenge in federal systems, even as the specific interpretations of that balance evolve.
narrative_ontology:disappearance_verdict(provincial_sovereignty_boundary__compact_federalism, world_rearranges).
narrative_ontology:founding_problem_status(provincial_sovereignty_boundary__compact_federalism, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(provincial_sovereignty_boundary__compact_federalism, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(provincial_sovereignty_boundary__compact_federalism, 'none', 1).
narrative_ontology:epsilon_provenance(provincial_sovereignty_boundary__compact_federalism, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(provincial_sovereignty_boundary__compact_federalism_tests).
:- end_tests(provincial_sovereignty_boundary__compact_federalism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) reflects the federal government's and national interest groups' loss of policy control and resources to provinces, while suppression (0.30) indicates the active political and legal efforts required by provinces to maintain this interpretation against federal challenges. The theater ratio (0.20) suggests some performative aspects in provincial assertions of autonomy, but the underlying contest is real. The claimed type is Tangled Rope because it serves a genuine coordination function (balancing regional interests) but involves asymmetric extraction (from federal to provincial authority) and requires active enforcement (political and legal battles) to hold.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of provinces asserting autonomy, this constraint is a legitimate expression of federalism, ensuring regional self-determination. From the federal government's perspective, it is an impediment to national unity and effective governance. The engine's classification will reflect this divergence based on the structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Provinces asserting autonomy are beneficiaries (d near 0.0) as they gain policy control and leverage. The federal government and national equalization recipients are payers (d near 1.0) as they lose authority and resources. National climate advocates are excluded, as their policy goals are undermined by this reading. Constitutional scholars are observers.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling genuine coordination (balancing regional interests) as pure extraction, while still highlighting the extractive aspects of power transfer. The 'compact federalism' reading, while contested, still addresses a live founding problem of balancing unity and autonomy, preventing a full mandatrophy declaration, but the 'contested' status of the founding problem indicates ongoing tension.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    compact_vs_constitutional_origin,
    'Is the Canadian federation fundamentally a compact among sovereign provinces, or a constitutional creation where provinces derive authority from the federal constitution?',
    'A definitive Supreme Court ruling on the foundational nature of the federation, or a constitutional amendment explicitly clarifying provincial status.',
    'If ruled a constitutional creation, this reading''s claims of residual sovereignty would be significantly weakened, shifting power to the federal government. If affirmed as a compact, provincial leverage would increase.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(compact_vs_constitutional_origin, conceptual, 'The foundational legal and historical interpretation of the Canadian federation''s origin.').

omega_variable(
    exit_negotiability_scope,
    'What are the actual legal and political mechanisms for provincial exit, and how ''negotiable'' is it under duress?',
    'A province formally initiating exit proceedings, testing the legal and political boundaries of ''negotiable under duress''.',
    'If exit is found to be practically impossible or unilaterally blocked by the federal government, the ''constrained'' exit option for provinces would shift to ''trapped'', increasing their effective extraction. If genuinely negotiable, it reinforces their leverage.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_negotiability_scope, empirical, 'The practical feasibility and legal framework for provincial secession or significant autonomy.').

omega_variable(
    resource_sovereignty_absolute_or_shared,
    'Does provincial ownership of natural resources (s.92A) grant absolute sovereignty over resource development, or is it subject to federal environmental and economic regulation?',
    'Further Supreme Court rulings clarifying the scope of s.92A in relation to federal powers (e.g., climate regulation, interprovincial trade).',
    'If absolute, this reading gains significant structural power, further constraining federal authority. If shared, federal influence over resource-rich provinces increases, reducing their leverage.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_sovereignty_absolute_or_shared, empirical, 'The extent of provincial control over natural resources and its interaction with federal powers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(provincial_sovereignty_boundary__compact_federalism, 1982, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prov_tr_t1982, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 1982, 0.1).
narrative_ontology:measurement(prov_tr_t1995, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 1995, 0.15).
narrative_ontology:measurement(prov_tr_t2008, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 2008, 0.18).
narrative_ontology:measurement(prov_tr_t2024, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(prov_be_t1982, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 1982, 0.35).
narrative_ontology:measurement(prov_be_t1995, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 1995, 0.4).
narrative_ontology:measurement(prov_be_t2008, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 2008, 0.43).
narrative_ontology:measurement(prov_be_t2024, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(prov_su_t1982, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 1982, 0.25).
narrative_ontology:measurement(prov_su_t1995, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 1995, 0.28).
narrative_ontology:measurement(prov_su_t2008, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 2008, 0.29).
narrative_ontology:measurement(prov_su_t2024, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 2024, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
