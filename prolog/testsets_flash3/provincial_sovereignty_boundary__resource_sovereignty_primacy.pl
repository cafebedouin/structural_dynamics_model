% ============================================================================
% CONSTRAINT STORY: provincial_sovereignty_boundary__resource_sovereignty_primacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_provincial_sovereignty_boundary__resource_sovereignty_primacy, []).

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
 *   constraint_id: provincial_sovereignty_boundary__resource_sovereignty_primacy
 *   human_readable: Provincial Resource Sovereignty Primacy
 *   domain: political_economy/federalism/resource_governance
 *
 * SUMMARY:
 *   This constraint represents the 'resource sovereignty primacy' reading of
 *   the provincial sovereignty boundary kernel in Canadian federalism. It
 *   asserts that s.92A of the Constitution Act 1982 grants provinces
 *   absolute, near-territorial sovereignty over their natural resources,
 *   allowing them to resist federal climate or fiscal policies as
 *   illegitimate extraction. This reading is a Tangled Rope: it coordinates
 *   provincial resource development but extracts from federal policy
 *   coherence and other provinces. The metrics reflect the increasing
 *   assertiveness of this reading over time, leading to higher extractiveness
 *   and suppression of federal alternatives.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0.65).
domain_priors:suppression_score(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0.75).
domain_priors:theater_ratio(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, extractiveness, 0.65).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(provincial_sovereignty_boundary__resource_sovereignty_primacy, tangled_rope).
narrative_ontology:human_readable(provincial_sovereignty_boundary__resource_sovereignty_primacy, "Provincial Resource Sovereignty Primacy").
narrative_ontology:topic_domain(provincial_sovereignty_boundary__resource_sovereignty_primacy, "political_economy/federalism/resource_governance").

domain_priors:requires_active_enforcement(provincial_sovereignty_boundary__resource_sovereignty_primacy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(provincial_sovereignty_boundary__resource_sovereignty_primacy, '250ba007-4624-4472-9e76-9f3af6711d49').
narrative_ontology:cs_kernel_codification('250ba007-4624-4472-9e76-9f3af6711d49', fixed_text).
narrative_ontology:cs_authority_grounding('250ba007-4624-4472-9e76-9f3af6711d49', lineage).
narrative_ontology:cs_interpretation_layer_present('250ba007-4624-4472-9e76-9f3af6711d49').
narrative_ontology:cs_reading_relation('250ba007-4624-4472-9e76-9f3af6711d49', provincial_sovereignty_boundary__constitutional_subordination, forecloses).
narrative_ontology:cs_reading_relation('250ba007-4624-4472-9e76-9f3af6711d49', provincial_sovereignty_boundary__compact_federalism, coexists_with).
narrative_ontology:cs_axiom('250ba007-4624-4472-9e76-9f3af6711d49', foundational, resource_control_equals_territorial_sovereignty).
narrative_ontology:cs_axiom_status(resource_control_equals_territorial_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('250ba007-4624-4472-9e76-9f3af6711d49', resource_control_equals_territorial_sovereignty, conventional).
narrative_ontology:cs_axiom('250ba007-4624-4472-9e76-9f3af6711d49', secondary, federal_climate_policy_is_illegitimate_extraction).
narrative_ontology:cs_axiom_status(federal_climate_policy_is_illegitimate_extraction, holdable).
narrative_ontology:cs_axiom_grounding('250ba007-4624-4472-9e76-9f3af6711d49', federal_climate_policy_is_illegitimate_extraction, instrumental).
narrative_ontology:cs_reference_frame('250ba007-4624-4472-9e76-9f3af6711d49', absolute_provincial_resource_autonomy).
narrative_ontology:cs_drift_state('250ba007-4624-4472-9e76-9f3af6711d49', contemporary_climate_crisis_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('250ba007-4624-4472-9e76-9f3af6711d49', '').
narrative_ontology:cs_kernel_id(provincial_sovereignty_boundary__resource_sovereignty_primacy, provincial_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__resource_sovereignty_primacy, resource_rich_provinces).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__resource_sovereignty_primacy, provincial_resource_industries).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__resource_sovereignty_primacy, federal_government).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__resource_sovereignty_primacy, national_environmental_groups).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__resource_sovereignty_primacy, other_provinces).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These provinces assert absolute control over their natural resources, interpreting s.92A as granting full territorial sovereignty. They benefit from direct resource revenues and the ability to set their own environmental and fiscal policies, often resisting federal oversight or revenue sharing.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, resource_rich_provinces, agenda_setter,
    institutional, generational, constrained, national).

% These industries benefit from provincial policies that prioritize resource extraction and minimize regulatory burdens, often aligning with provincial governments to resist federal environmental or carbon pricing initiatives. Their mobility allows them to leverage provincial autonomy.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, provincial_resource_industries, beneficiary,
    organized, biographical, mobile, regional).

% The federal government bears the cost of fragmented national policy, particularly in areas like climate change and inter-provincial trade. Its ability to implement national strategies is constrained by provincial assertions of resource sovereignty, leading to policy stalemates and legal challenges.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, federal_government, payer,
    institutional, generational, constrained, national).

% These groups advocate for stronger federal environmental regulations and national climate action, seeing provincial resource sovereignty as a barrier to addressing pan-Canadian environmental challenges. They bear the cost of environmental degradation and policy inaction.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, national_environmental_groups, payer,
    organized, generational, constrained, national).

% Provinces without significant natural resources or those with different economic priorities bear the costs of uneven resource development, inter-provincial trade barriers, and the inability to forge a cohesive national economic or environmental strategy. They are often caught between federal and resource-rich provincial interests.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, other_provinces, payer,
    institutional, generational, constrained, national).

% The ultimate arbiter of constitutional disputes, the Supreme Court interprets the division of powers, including s.92A. Its rulings shape the boundaries of provincial and federal authority, but it operates within the legal framework presented by litigants.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, supreme_court_of_canada, observer,
    institutional, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This reading coordinates provincial resource development and economic policy by granting clear, absolute authority to provinces, reducing inter-provincial disputes over resource control and allowing provinces to tailor policies to local conditions.
% TRANSFER_FUNCTION: Transfers absolute control over resource revenues and regulatory authority from the federal government (and implicitly, other provinces) to resource-rich provinces, enabling them to capture the full economic benefit of their resources.
% ABSENT_VOICES: Indigenous nations, whose inherent rights and title often overlap with provincial resource claims, are frequently marginalized in this framing of provincial sovereignty. They would argue for co-management or self-determination over resources, but their claims are often subordinated to provincial jurisdiction.
% DISAPPEARANCE_RATIONALE: If this interpretation of absolute provincial resource sovereignty vanished, federal environmental and fiscal policies would gain significant ground, leading to a more integrated national approach to climate change and resource revenue sharing. Provincial resource industries would face increased federal regulation, and the balance of power in Canadian federalism would fundamentally shift.
% FOUNDING_PROBLEM: The original intent of s.92A was to clarify provincial ownership and control over natural resources, particularly in the context of Western provinces seeking greater economic autonomy and control over their resource wealth.
% FOUNDING_PROBLEM_CORROBORATION: Resource-rich provinces and their industries attest that the problem of federal overreach and the need for provincial autonomy over resources remains live. Federal and other provincial governments, along with national environmental groups, contest this, arguing that the original problem has been superseded by national and global challenges requiring federal leadership.
narrative_ontology:disappearance_verdict(provincial_sovereignty_boundary__resource_sovereignty_primacy, world_rearranges).
narrative_ontology:founding_problem_status(provincial_sovereignty_boundary__resource_sovereignty_primacy, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(provincial_sovereignty_boundary__resource_sovereignty_primacy, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(provincial_sovereignty_boundary__resource_sovereignty_primacy, 'none', 1).
narrative_ontology:epsilon_provenance(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(provincial_sovereignty_boundary__resource_sovereignty_primacy_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(provincial_sovereignty_boundary__resource_sovereignty_primacy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(provincial_sovereignty_boundary__resource_sovereignty_primacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because this reading allows resource-rich provinces to unilaterally capture resource rents and externalize environmental costs onto the national commons, at the expense of federal policy objectives and other provinces. Suppression is high because it actively resists federal legislative and fiscal authority, often through legal challenges and political obstruction. Theater ratio is low as the assertion of sovereignty is a genuine, actively pursued political goal, not mere performance.
 *
 * PERSPECTIVAL GAP:
 *   Resource-rich provinces perceive this as a legitimate exercise of constitutional power, ensuring their economic prosperity. The federal government and other provinces perceive it as an extractive interpretation that undermines national unity and policy effectiveness. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Resource-rich provinces and their industries are clear beneficiaries, gaining control and revenue. The federal government, national environmental groups, and other provinces are victims, bearing the costs of fragmented policy and environmental impacts. The Supreme Court acts as an observer, interpreting the constitutional boundaries.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutional_interpretation_ambiguity,
    'Is s.92A an absolute grant of territorial sovereignty, or a specific grant of proprietary rights within a broader federal framework?',
    'A definitive Supreme Court of Canada ruling that explicitly clarifies the scope of s.92A relative to federal powers (e.g., ''peace, order, and good government'' or ''trade and commerce'').',
    'If interpreted as absolute sovereignty, this reading''s extractiveness and suppression of federal policy would be constitutionally validated. If interpreted as proprietary rights within a federal framework, its extractiveness would be reclassified as illegitimate overreach, and its suppression would be seen as unconstitutional.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutional_interpretation_ambiguity, conceptual, 'Ambiguity in the constitutional interpretation of provincial resource ownership.').

omega_variable(
    mandate_drift_from_original_intent,
    'Has the ''resource sovereignty primacy'' reading drifted from the original intent of s.92A, which was to clarify ownership, not to grant absolute territorial sovereignty?',
    'Historical legal scholarship and analysis of parliamentary debates surrounding the patriation of the Constitution and the inclusion of s.92A.',
    'If significant drift is confirmed, the constraint would be reclassified closer to a Snare, as its current function would be primarily extractive rather than fulfilling its original coordination mandate. If no drift, its current function is consistent with original intent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_drift_from_original_intent, empirical, 'Whether the current interpretation aligns with the historical legislative intent.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of federal policy structural (constitutional division of powers) or internalized (provinces'' political identity fused with resource autonomy)?',
    'Analysis of provincial political discourse and public opinion: if provincial identity is deeply tied to resource autonomy, suppression is partially internalized. If it''s purely a legal/political strategy, it''s structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as provincial actors carry the resistance with them even if legal barriers shift. If purely structural, legal changes would more directly alter suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in federal-provincial relations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(provincial_sovereignty_boundary__resource_sovereignty_primacy, 1982, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prov_tr_t1982, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 1982, 0.1).
narrative_ontology:measurement(prov_tr_t1995, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 1995, 0.12).
narrative_ontology:measurement(prov_tr_t2005, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 2005, 0.15).
narrative_ontology:measurement(prov_tr_t2015, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 2015, 0.18).
narrative_ontology:measurement(prov_tr_t2024, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(prov_be_t1982, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 1982, 0.4).
narrative_ontology:measurement(prov_be_t1995, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 1995, 0.5).
narrative_ontology:measurement(prov_be_t2005, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 2005, 0.58).
narrative_ontology:measurement(prov_be_t2015, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 2015, 0.62).
narrative_ontology:measurement(prov_be_t2024, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(prov_su_t1982, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 1982, 0.5).
narrative_ontology:measurement(prov_su_t1995, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 1995, 0.6).
narrative_ontology:measurement(prov_su_t2005, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 2005, 0.68).
narrative_ontology:measurement(prov_su_t2015, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 2015, 0.72).
narrative_ontology:measurement(prov_su_t2024, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(provincial_sovereignty_boundary__resource_sovereignty_primacy, enforcement_mechanism).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__resource_sovereignty_primacy, federal_climate_policy_framework).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__resource_sovereignty_primacy, interprovincial_trade_barriers).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'provincial sovereignty boundary' kernel. Its siblings are 'constitutional_subordination' and 'compact_federalism'. Each represents a distinct interpretation of provincial powers within the Canadian federation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
