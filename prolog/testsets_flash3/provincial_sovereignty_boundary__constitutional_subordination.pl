% ============================================================================
% CONSTRAINT STORY: provincial_sovereignty_boundary__constitutional_subordination
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_provincial_sovereignty_boundary__constitutional_subordination, []).

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
 *   constraint_id: provincial_sovereignty_boundary__constitutional_subordination
 *   human_readable: Provincial Sovereignty Boundary: Constitutional Subordination Reading
 *   domain: political_economy/federalism/resource_governance
 *
 * SUMMARY:
 *   This constraint represents the 'constitutional subordination' reading of
 *   provincial sovereignty within a federal system, where provinces are
 *   considered creations of the federal constitution with no inherent
 *   sovereignty. Exit from the federation requires federal consent, and
 *   national policies like equalization and climate action are legitimate
 *   federal authority. This reading views separatism as constitutionally
 *   null. The constraint is classified as a Tangled Rope due to its genuine
 *   coordination function (national unity, coherent policy) coupled with
 *   significant extraction from provinces seeking greater autonomy or
 *   resource control, maintained by active federal enforcement and judicial
 *   rulings.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(provincial_sovereignty_boundary__constitutional_subordination, 0.65).
domain_priors:suppression_score(provincial_sovereignty_boundary__constitutional_subordination, 0.75).
domain_priors:theater_ratio(provincial_sovereignty_boundary__constitutional_subordination, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, extractiveness, 0.65).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(provincial_sovereignty_boundary__constitutional_subordination, tangled_rope).
narrative_ontology:human_readable(provincial_sovereignty_boundary__constitutional_subordination, "Provincial Sovereignty Boundary: Constitutional Subordination Reading").
narrative_ontology:topic_domain(provincial_sovereignty_boundary__constitutional_subordination, "political_economy/federalism/resource_governance").

domain_priors:requires_active_enforcement(provincial_sovereignty_boundary__constitutional_subordination).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(provincial_sovereignty_boundary__constitutional_subordination, 'fae3d767-948d-4170-a9c8-6036088273f8').
narrative_ontology:cs_kernel_codification('fae3d767-948d-4170-a9c8-6036088273f8', fixed_text).
narrative_ontology:cs_authority_grounding('fae3d767-948d-4170-a9c8-6036088273f8', lineage).
narrative_ontology:cs_interpretation_layer_present('fae3d767-948d-4170-a9c8-6036088273f8').
narrative_ontology:cs_reading_relation('fae3d767-948d-4170-a9c8-6036088273f8', provincial_sovereignty_boundary__compact_federalism, forecloses).
narrative_ontology:cs_reading_relation('fae3d767-948d-4170-a9c8-6036088273f8', provincial_sovereignty_boundary__resource_sovereignty_primacy, forecloses).
narrative_ontology:cs_axiom('fae3d767-948d-4170-a9c8-6036088273f8', foundational, federal_supremacy_doctrine).
narrative_ontology:cs_axiom_status(federal_supremacy_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('fae3d767-948d-4170-a9c8-6036088273f8', federal_supremacy_doctrine, deontological).
narrative_ontology:cs_axiom('fae3d767-948d-4170-a9c8-6036088273f8', foundational, constitutional_unity_principle).
narrative_ontology:cs_axiom_status(constitutional_unity_principle, holdable).
narrative_ontology:cs_axiom_grounding('fae3d767-948d-4170-a9c8-6036088273f8', constitutional_unity_principle, deontological).
narrative_ontology:cs_reference_frame('fae3d767-948d-4170-a9c8-6036088273f8', strong_federal_state_post_confederation).
narrative_ontology:cs_drift_state('fae3d767-948d-4170-a9c8-6036088273f8', contemporary_era_of_provincial_assertiveness, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('fae3d767-948d-4170-a9c8-6036088273f8', '').
narrative_ontology:cs_kernel_id(provincial_sovereignty_boundary__constitutional_subordination, provincial_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__constitutional_subordination, federal_government).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__constitutional_subordination, national_unity_advocates).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__constitutional_subordination, provincial_separatist_movements).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__constitutional_subordination, resource_rich_provinces).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Asserts ultimate constitutional authority over provinces, including a veto on secession and the right to implement national policies like equalization and climate regulations. Benefits from a strong, unified federal state.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, federal_government, agenda_setter,
    institutional, generational, arbitrage, national).

% Seek to assert provincial independence or secession, but are legally constrained by the federal constitution. Their efforts are consistently deemed constitutionally null without federal consent, leading to political and legal battles.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, provincial_separatist_movements, payer,
    organized, generational, identity_locked, regional).

% Possess significant natural resources but face federal intervention on resource development, environmental policy, and revenue sharing (equalization payments). They are constrained by federal constitutional supremacy despite their economic power.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, resource_rich_provinces, payer,
    powerful, biographical, constrained, regional).

% Benefit from the constitutional framework that prioritizes federal authority and national unity, seeing it as essential for stability and shared identity. They actively support federal policies that reinforce this reading.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, national_unity_advocates, beneficiary,
    organized, generational, mobile, national).

% The ultimate arbiter of constitutional disputes, consistently upholding the principle of federal supremacy and the requirement of federal consent for provincial secession. Its rulings reinforce the constitutional subordination reading.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, supreme_court_of_canada, agenda_setter,
    institutional, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear hierarchy of constitutional authority, ensuring a unified national legal and political framework, preventing provincial unilateralism, and enabling national policy coherence (e.g., equalization, climate action).
% TRANSFER_FUNCTION: Transfers ultimate decision-making power and policy scope from provinces to the federal government, particularly regarding secession, inter-provincial resource disputes, and national standards. This also implies a transfer of potential resource wealth from provinces to the federal equalization scheme.
% ABSENT_VOICES: Indigenous nations, whose inherent sovereignty claims predate and often conflict with both federal and provincial constitutional frameworks, are largely absent from the formal constitutional debate as framed by this reading. They would argue for self-determination outside of federal or provincial subordination.
% DISAPPEARANCE_RATIONALE: If this reading of provincial constitutional subordination vanished, provinces would immediately assert greater autonomy, potentially leading to unilateral resource management, challenges to equalization, and renewed secessionist efforts. The federal state's power would diminish, and the national political landscape would fundamentally reorganize.
% FOUNDING_PROBLEM: The original confederation sought to create a strong central government to manage national defense, inter-provincial trade, and a unified legal system, avoiding the fragmentation seen in other federations.
% FOUNDING_PROBLEM_CORROBORATION: Historians and legal scholars outside the federal government corroborate the intent to create a strong federal center. Contemporary political scientists and constitutional lawyers attest that the problem of maintaining national unity and coherent national policy in a diverse federation remains live, especially in the face of regional economic disparities and climate change.
narrative_ontology:disappearance_verdict(provincial_sovereignty_boundary__constitutional_subordination, world_rearranges).
narrative_ontology:founding_problem_status(provincial_sovereignty_boundary__constitutional_subordination, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(provincial_sovereignty_boundary__constitutional_subordination, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(provincial_sovereignty_boundary__constitutional_subordination, 'none', 1).
narrative_ontology:epsilon_provenance(provincial_sovereignty_boundary__constitutional_subordination, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(provincial_sovereignty_boundary__constitutional_subordination_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(provincial_sovereignty_boundary__constitutional_subordination, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(provincial_sovereignty_boundary__constitutional_subordination_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) reflects the federal government's ability to impose policies and financial transfers on provinces, limiting their fiscal and policy autonomy. Suppression (0.75) is high due to the Supreme Court's consistent upholding of federal supremacy and the legal barriers to provincial secession. Theater ratio (0.20) is low, as the federal government actively enforces this constitutional interpretation, and the coordination function of national unity is genuinely pursued, even if it comes with extractive elements. The increasing trend in extractiveness and suppression reflects the federal government's growing assertiveness in areas like climate policy and the ongoing legal challenges from provinces.
 *
 * PERSPECTIVAL GAP:
 *   From the federal government's perspective, this is a necessary framework for national cohesion and effective governance (a Rope). From the perspective of separatist movements or resource-rich provinces, it is an extractive mechanism that denies their inherent rights and autonomy (a Snare). The engine's classification as Tangled Rope captures this hybrid nature, acknowledging both the coordination and extraction functions.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal government and national unity advocates are clear beneficiaries, as this reading entrenches their power and vision for the country. Provincial separatist movements and resource-rich provinces are targets, bearing the costs of limited autonomy and federal policy imposition. The Supreme Court acts as an agenda-setter, consistently reinforcing this reading through its judgments.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (national unity, coherent governance) is still live, preventing it from being a Piton. However, the 'contested' status of the founding problem indicates that while the problem of national cohesion persists, the means of addressing it (federal supremacy) are increasingly seen by some as extractive rather than purely coordinative. This prevents mislabeling it as a pure Rope, as the extraction is a significant and contested feature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutional_interpretation_ambiguity,
    'Is the constitutional text truly unambiguous regarding provincial subordination, or does it allow for alternative interpretations of provincial sovereignty?',
    'A constitutional amendment explicitly clarifying the nature of provincial sovereignty, or a shift in judicial philosophy that re-interprets existing texts to grant greater provincial autonomy.',
    'If the text is ambiguous and re-interpreted, the constraint''s suppression and extractiveness could decrease, potentially shifting its classification towards a more balanced Rope or even a Scaffold if a transitional framework for greater autonomy were adopted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_interpretation_ambiguity, conceptual, 'Ambiguity in constitutional interpretation regarding provincial sovereignty.').

omega_variable(
    federal_consent_legitimacy,
    'Is the federal government''s ''veto'' over provincial secession a legitimate exercise of constitutional authority, or an assertion of power that lacks democratic consent from the seceding entity?',
    'A national referendum on the principle of federal consent for secession, or a constitutional convention that re-negotiates the terms of federal-provincial relations.',
    'If the veto is deemed illegitimate, the suppression metric would be re-evaluated as purely coercive, increasing the constraint''s effective extraction and pushing it closer to a Snare for separatist movements. If affirmed, the current classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federal_consent_legitimacy, preference, 'Legitimacy of federal consent requirement for provincial secession.').

omega_variable(
    resource_sovereignty_vs_federal_power,
    'To what extent does provincial ownership of natural resources (s.92A Constitution Act 1982) grant inherent sovereignty that limits federal authority, as opposed to merely ownership within a federal framework?',
    'Further Supreme Court rulings specifically on the scope of s.92A in relation to federal powers (e.g., climate policy, equalization), or a constitutional amendment clarifying the balance of powers.',
    'If s.92A is interpreted to grant greater inherent sovereignty, the federal government''s ability to extract from resource-rich provinces would decrease, lowering the constraint''s extractiveness for those provinces. If interpreted narrowly, the current high extractiveness persists.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(resource_sovereignty_vs_federal_power, empirical, 'The scope of provincial resource sovereignty versus federal constitutional power.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(provincial_sovereignty_boundary__constitutional_subordination, 1982, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prov_tr_t1982, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 1982, 0.1).
narrative_ontology:measurement(prov_tr_t1995, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 1995, 0.15).
narrative_ontology:measurement(prov_tr_t2007, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 2007, 0.18).
narrative_ontology:measurement(prov_tr_t2024, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(prov_be_t1982, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 1982, 0.55).
narrative_ontology:measurement(prov_be_t1995, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 1995, 0.6).
narrative_ontology:measurement(prov_be_t2007, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 2007, 0.63).
narrative_ontology:measurement(prov_be_t2024, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(prov_su_t1982, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 1982, 0.65).
narrative_ontology:measurement(prov_su_t1995, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 1995, 0.7).
narrative_ontology:measurement(prov_su_t2007, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 2007, 0.73).
narrative_ontology:measurement(prov_su_t2024, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(provincial_sovereignty_boundary__constitutional_subordination, enforcement_mechanism).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__constitutional_subordination, provincial_sovereignty_boundary__compact_federalism).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__constitutional_subordination, provincial_sovereignty_boundary__resource_sovereignty_primacy).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__constitutional_subordination, federal_equalization_formula).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__constitutional_subordination, national_climate_policy_framework).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'provincial_sovereignty_boundary' kernel. This 'constitutional subordination' reading emphasizes federal supremacy and a federal veto over provincial exit. It contrasts with 'compact_federalism' (provinces as sovereign parties to a compact) and 'resource_sovereignty_primacy' (resource ownership as absolute sovereignty).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
