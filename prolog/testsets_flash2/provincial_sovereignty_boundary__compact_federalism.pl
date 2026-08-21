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
 *   sovereignty, where the federation is understood as a compact among
 *   sovereign provinces, granting them residual sovereignty and making
 *   federal authority conditional on provincial consent. This reading allows
 *   for negotiable exit under duress and provincial override on national
 *   policies like climate or equalization. It is one of several competing
 *   interpretations of the provincial sovereignty boundary within the
 *   federation.
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
narrative_ontology:cs_story_uid(provincial_sovereignty_boundary__compact_federalism, '982fd555-1966-4d34-83aa-7de572297f2b').
narrative_ontology:cs_kernel_codification('982fd555-1966-4d34-83aa-7de572297f2b', fixed_text).
narrative_ontology:cs_authority_grounding('982fd555-1966-4d34-83aa-7de572297f2b', lineage).
narrative_ontology:cs_interpretation_layer_present('982fd555-1966-4d34-83aa-7de572297f2b').
narrative_ontology:cs_reading_relation('982fd555-1966-4d34-83aa-7de572297f2b', provincial_sovereignty_boundary__constitutional_subordination, coexists_with).
narrative_ontology:cs_reading_relation('982fd555-1966-4d34-83aa-7de572297f2b', provincial_sovereignty_boundary__resource_sovereignty_primacy, coexists_with).
narrative_ontology:cs_axiom('982fd555-1966-4d34-83aa-7de572297f2b', foundational, confederation_as_voluntary_compact).
narrative_ontology:cs_axiom_status(confederation_as_voluntary_compact, holdable).
narrative_ontology:cs_axiom_grounding('982fd555-1966-4d34-83aa-7de572297f2b', confederation_as_voluntary_compact, conventional).
narrative_ontology:cs_axiom('982fd555-1966-4d34-83aa-7de572297f2b', foundational, provinces_retain_residual_sovereignty).
narrative_ontology:cs_axiom_status(provinces_retain_residual_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('982fd555-1966-4d34-83aa-7de572297f2b', provinces_retain_residual_sovereignty, conventional).
narrative_ontology:cs_reference_frame('982fd555-1966-4d34-83aa-7de572297f2b', original_compact_intent).
narrative_ontology:cs_drift_state('982fd555-1966-4d34-83aa-7de572297f2b', contemporary, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('982fd555-1966-4d34-83aa-7de572297f2b', '').
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

% These provinces benefit from a reading of federalism that grants them significant residual sovereignty, allowing them to assert conditional consent over federal initiatives, negotiate equalization payments, and potentially exit the federation under duress. They actively defend this interpretation.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, provinces_asserting_autonomy, beneficiary,
    institutional, generational, constrained, regional).

% The federal government, under this reading, faces limitations on its authority, requiring provincial consent for many policies and being subject to negotiation on fiscal transfers and national programs. Its ability to implement uniform national policies is constrained.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, federal_government, payer,
    institutional, generational, constrained, national).

% These provinces and their citizens rely on federal equalization payments. Under a compact federalism reading, these transfers become more negotiable and potentially less stable, as they are seen as a federal concession rather than a constitutional right, impacting their long-term planning and service provision.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, national_equalization_recipients, payer,
    organized, biographical, constrained, national).

% Academics and legal experts who interpret the historical and legal basis of the federation as a compact among founding provinces, providing intellectual justification for the residual sovereignty claims. They analyze the historical documents and legal precedents supporting this view.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, constitutional_scholars_compact_view, observer,
    analytical, civilizational, analytical, universal).

% Groups and individuals who prioritize a strong, unified federal state and view provincial claims of residual sovereignty as a threat to national cohesion. Their arguments for federal supremacy are often sidelined in debates dominated by provincial autonomy claims under this reading.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, national_unity_advocates, excluded,
    moderate, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for inter-provincial and federal-provincial relations, allowing for shared governance while acknowledging distinct provincial interests and historical origins of the federation.
% TRANSFER_FUNCTION: Shapes the flow of political authority and fiscal resources, allowing provinces to retain more control over their jurisdictions and negotiate federal transfers, potentially at the expense of federal uniformity or national programs.
% ABSENT_VOICES: Advocates for a strong, centralized federal government and those who view the constitution as a unitary document, not a compact, are often marginalized. Their arguments for federal supremacy and national standards are less influential when provincial consent is prioritized.
% DISAPPEARANCE_RATIONALE: If this reading of provincial sovereignty vanished, the balance of power in the federation would fundamentally shift. Federal authority would expand, provincial autonomy would diminish, and the terms of fiscal federalism and national policy implementation would be drastically altered, leading to a complete reorganization of governance.
% FOUNDING_PROBLEM: The original problem was to unite disparate colonies into a single nation while preserving their distinct identities, legal systems, and regional interests, balancing central authority with local autonomy.
% FOUNDING_PROBLEM_CORROBORATION: Historians and political scientists, including those outside the benefiting provinces, corroborate that the founding problem of balancing central and regional powers remains a live and ongoing challenge in the federation's evolution. Legal scholars also attest to the historical debates surrounding the nature of Confederation.
narrative_ontology:disappearance_verdict(provincial_sovereignty_boundary__compact_federalism, world_rearranges).
narrative_ontology:founding_problem_status(provincial_sovereignty_boundary__compact_federalism, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(provincial_sovereignty_boundary__compact_federalism, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.45) reflects the costs imposed on the federal government and national programs by the requirement for provincial consent and the negotiability of national standards. Suppression (0.30) is moderate, as this reading requires active political and legal defense by provinces but does not involve overt coercion. Theater ratio (0.20) is low, as the claims of provincial autonomy are genuinely asserted and acted upon, not merely performative. The claimed type is 'tangled_rope' because it genuinely coordinates federal-provincial relations while allowing for asymmetric extraction by provinces asserting their autonomy.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of provinces asserting autonomy, this reading is a legitimate defense of their historical rights and interests, ensuring fair coordination within the federation. From the federal government's perspective, it can be seen as an extractive mechanism that fragments national policy and imposes costs on the broader federal system. The engine's per-seat classification will reflect these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   Provinces asserting autonomy are beneficiaries (d=0.0-0.2) as they gain leverage and control. The federal government and national equalization recipients are payers (d=0.7-0.9) as they bear the costs of fragmented authority and negotiable transfers. Constitutional scholars (compact view) are observers (d=0.5) providing analytical support. National unity advocates are excluded (d=1.0) as their perspective is structurally marginalized by this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_compact_corroboration,
    'To what extent does historical evidence unequivocally support the ''compact'' theory of Confederation versus a unitary constitutional founding?',
    'Consensus among independent constitutional historians and legal scholars on the primary intent and legal nature of the founding documents and negotiations.',
    'Strong corroboration would bolster the legitimacy of provincial claims, potentially shifting the constraint towards a more ''rope-like'' coordination. Weak corroboration would expose the ''compact'' as a political construct, increasing its ''snare-like'' characteristics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_compact_corroboration, empirical, 'Empirical basis for the ''compact'' theory of federalism.').

omega_variable(
    exit_negotiability_threshold,
    'What constitutes ''duress'' sufficient to make provincial exit ''negotiable'' under this reading, and what are the practical mechanisms for such negotiation?',
    'Legal precedent or political convention establishing clear criteria for ''duress'' and a formal process for negotiating secession or significant constitutional re-alignment.',
    'Clearer thresholds would reduce the ambiguity and potential for political opportunism, making the constraint more predictable. Ambiguity allows for strategic leveraging of exit threats, increasing extractiveness.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(exit_negotiability_threshold, conceptual, 'Clarity on conditions and mechanisms for provincial exit.').

omega_variable(
    climate_policy_override_legitimacy,
    'Is the provincial right to override federal climate policy a legitimate exercise of residual sovereignty or an obstruction of national interest?',
    'A national referendum or constitutional amendment clarifying the division of powers regarding environmental policy, or a Supreme Court ruling on the scope of federal vs. provincial jurisdiction in this area.',
    'If deemed legitimate, it reinforces provincial autonomy. If deemed obstructionist, it highlights the extractive nature of this reading on national policy goals, potentially shifting the constraint towards a ''snare'' for the federal government.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(climate_policy_override_legitimacy, preference, 'Legitimacy of provincial override on national climate policy.').


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
