% ============================================================================
% CONSTRAINT STORY: provincial_sovereignty_boundary__resource_sovereignty_primacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: provincial_sovereignty_boundary__resource_sovereignty_primacy
 *   human_readable: Provincial Resource Sovereignty as Constitutional Absolute
 *   domain: political/federalism/resource_governance
 *
 * SUMMARY:
 *   This constraint story captures the 'resource_sovereignty_primacy' reading
 *   of the Canadian provincial sovereignty boundary kernel. Under this
 *   reading, section 92A of the Constitution Act, 1982 â which affirms
 *   provincial ownership and management of natural resources â is
 *   interpreted not merely as a division-of-powers allocation but as a
 *   grounding of near-absolute provincial sovereignty. Resource jurisdiction
 *   becomes territorial sovereignty; federal climate, environmental, and
 *   fiscal policy in resource domains are framed as illegitimate extraction;
 *   and unilateral provincial resistance is cast as a constitutional right.
 *   This reading is advanced primarily by resource-rich provincial
 *   governments and the fossil fuel sector, resisted by the federal
 *   government and non-resource provinces, and exercised at the expense of
 *   Indigenous treaty rights and federal policy capacity. It is not a natural
 *   law but a constructed constitutional interpretation whose extraction has
 *   intensified as climate policy has expanded federal regulatory ambition.
 *
 * KEY AGENTS:
 *   - resource_rich_provincial_governments: Primary agenda-setter (institutional/constrained) â advances and enforces the constitutional reading through litigation and legislation
 *   - fossil_fuel_sector: Primary beneficiary (powerful/mobile) â captures regulatory avoidance and reduced carbon liability
 *   - federal_government: Primary target (institutional/constrained) â bears policy incapacity and constitutional deadlock costs
 *   - non_resource_provinces: Secondary target (institutional/constrained) â bears fiscal and climate costs of policy paralysis
 *   - indigenous_nations: Excluded payer (organized/trapped) â treaty rights overridden by provincial resource jurisdiction
 *   - constitutional_scholars: Analytical observer (analytical/analytical) â evaluates legal claims and scope of s.92A
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0.72).
domain_priors:suppression_score(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0.78).
domain_priors:theater_ratio(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, extractiveness, 0.72).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(provincial_sovereignty_boundary__resource_sovereignty_primacy, tangled_rope).
narrative_ontology:human_readable(provincial_sovereignty_boundary__resource_sovereignty_primacy, "Provincial Resource Sovereignty as Constitutional Absolute").
narrative_ontology:topic_domain(provincial_sovereignty_boundary__resource_sovereignty_primacy, "political/federalism/resource_governance").

domain_priors:requires_active_enforcement(provincial_sovereignty_boundary__resource_sovereignty_primacy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(provincial_sovereignty_boundary__resource_sovereignty_primacy, '3c19c130-1f7d-4982-a92e-ee5e51bb9acc').
narrative_ontology:cs_kernel_codification('3c19c130-1f7d-4982-a92e-ee5e51bb9acc', formalized).
narrative_ontology:cs_authority_grounding('3c19c130-1f7d-4982-a92e-ee5e51bb9acc', lineage).
narrative_ontology:cs_interpretation_layer_present('3c19c130-1f7d-4982-a92e-ee5e51bb9acc').
narrative_ontology:cs_reading_relation('3c19c130-1f7d-4982-a92e-ee5e51bb9acc', provincial_sovereignty_boundary__constitutional_subordination, forecloses).
narrative_ontology:cs_reading_relation('3c19c130-1f7d-4982-a92e-ee5e51bb9acc', provincial_sovereignty_boundary__compact_federalism, coexists_with).
narrative_ontology:cs_axiom('3c19c130-1f7d-4982-a92e-ee5e51bb9acc', foundational, resource_ownership_implies_sovereignty).
narrative_ontology:cs_axiom_status(resource_ownership_implies_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('3c19c130-1f7d-4982-a92e-ee5e51bb9acc', resource_ownership_implies_sovereignty, conventional).
narrative_ontology:cs_axiom('3c19c130-1f7d-4982-a92e-ee5e51bb9acc', secondary, provincial_unilateral_exit_right).
narrative_ontology:cs_axiom_status(provincial_unilateral_exit_right, holdable).
narrative_ontology:cs_axiom_grounding('3c19c130-1f7d-4982-a92e-ee5e51bb9acc', provincial_unilateral_exit_right, deontological).
narrative_ontology:cs_reference_frame('3c19c130-1f7d-4982-a92e-ee5e51bb9acc', provincial_resource_autonomy_1982).
narrative_ontology:cs_drift_state('3c19c130-1f7d-4982-a92e-ee5e51bb9acc', contemporary_climate_policy_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('3c19c130-1f7d-4982-a92e-ee5e51bb9acc', '').
narrative_ontology:cs_kernel_id(provincial_sovereignty_boundary__resource_sovereignty_primacy, provincial_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__resource_sovereignty_primacy, resource_rich_provincial_governments).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__resource_sovereignty_primacy, fossil_fuel_sector).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__resource_sovereignty_primacy, federal_government).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__resource_sovereignty_primacy, non_resource_provinces).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__resource_sovereignty_primacy, indigenous_nations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advance the constitutional reading that s.92A resource ownership constitutes absolute provincial sovereignty; use litigation, legislation, and political mobilization to block federal climate and fiscal policy; collect electoral support, resource revenue autonomy, and intergovernmental bargaining leverage.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, resource_rich_provincial_governments, agenda_setter,
    institutional, generational, constrained, national).

% Benefits from provincial constitutional vetoes that block federal environmental regulation and carbon pricing; supplies political and financial support to provincial governments advancing this reading; faces lower regulatory risk than under cooperative federalism but cannot itself set the constitutional agenda.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, fossil_fuel_sector, beneficiary,
    powerful, biographical, mobile, national).

% Bears the cost of incapacitated climate and fiscal policy; faces constitutional litigation and political deadlock when attempting national standards in resource sectors; must either accommodate provincial demands or risk prolonged court battles and constitutional crisis.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, federal_government, payer,
    institutional, generational, constrained, national).

% Bear diffuse costs of federal policy paralysis on climate and fiscal stabilization; subsidize resource province wealth through equalization while receiving no reciprocal regulatory cooperation; lack constitutional leverage to counterbalance resource-sovereignty claims.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, non_resource_provinces, payer,
    institutional, generational, constrained, national).

% Treaty rights and land claims are overridden or delayed by provincial resource sovereignty assertions; excluded from the constitutional framing that treats resource jurisdiction as exclusively provincial; bear environmental and cultural costs of unrestricted resource extraction with limited legal recourse.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, indigenous_nations, payer,
    organized, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(provincial_sovereignty_boundary__resource_sovereignty_primacy, indigenous_nations, excluded).

% Analyze and debate the scope of s.92A; provide legal opinions that either support or challenge the resource-sovereignty reading; serve as expert witnesses in constitutional references and advise governments on intergovernmental strategy.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, constitutional_scholars, observer,
    analytical, civilizational, analytical, national).

narrative_ontology:fixing_cost_class(provincial_sovereignty_boundary__resource_sovereignty_primacy, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates provincial governments and resource industries against federal regulatory and fiscal incursion; establishes a common constitutional front for resource-rich jurisdictions to resist climate and environmental policy.
% TRANSFER_FUNCTION: Moves regulatory autonomy and fiscal capacity from the federal government to provincial governments, and risk/liability (environmental, climate, fiscal) from present resource extraction to non-resource jurisdictions, Indigenous lands, and future publics.
% ABSENT_VOICES: Indigenous nations whose treaty rights and land claims are overridden by provincial resource jurisdiction; environmental scientists and climate policy advocates whose frameworks are excluded from the constitutional balance; non-resource provinces whose fiscal stability is undermined by unilateral resource decisions.
% DISAPPEARANCE_RATIONALE: If this constitutional reading vanished, federal climate and environmental policy would immediately apply to resource sectors, equalization and fiscal stabilization would shift, and provincial political coalitions organized around resource nationalism would lose their primary constitutional weapon â the federation would rearrange around cooperative rather than conflictual federalism.
% FOUNDING_PROBLEM: 1980s federal energy policy (National Energy Program) and constitutional patriation threatened provincial resource revenue control; provinces sought constitutional protection of resource ownership and management authority against federal intrusion.
% FOUNDING_PROBLEM_CORROBORATION: Resource-rich provincial governments attest the problem remains live, citing federal carbon pricing and environmental assessment as new intrusions. Federal government and constitutional scholars outside the beneficiary set attest the 1982 settlement resolved the revenue-control issue and current conflicts arise from extending provincial power to veto federal policy in shared jurisdictions; no independent corroboration from non-beneficiary parties supports the continued emergency framing.
narrative_ontology:disappearance_verdict(provincial_sovereignty_boundary__resource_sovereignty_primacy, world_rearranges).
narrative_ontology:founding_problem_status(provincial_sovereignty_boundary__resource_sovereignty_primacy, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(provincial_sovereignty_boundary__resource_sovereignty_primacy, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(provincial_sovereignty_boundary__resource_sovereignty_primacy, 'none', 1).
narrative_ontology:epsilon_provenance(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0.72, 'kimi-k2.6', 'none', direct).

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
 *   The high extractiveness (0.72) reflects the constraint's function in capturing federal policy capacity and blocking regulatory alternatives. Suppression (0.78) measures the active legal and political enforcement required to maintain this reading against federal climate legislation and cooperative federalism alternatives. Theater ratio (0.45) captures the performative constitutional rhetoric that outruns settled law. Accessibility collapse (0.65) indicates that within subscribing provinces, alternative federal-provincial cooperative frameworks become politically unavailable once this reading is adopted. Resistance (0.75) is high because the federal government, non-resource provinces, and Indigenous nations actively contest the reading in courts and political forums. The measurement series run on one shared time grid so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   From the resource-rich provincial seat, the constraint is experienced as defensive sovereignty protecting local control and revenue against federal overreach; from the federal and non-resource seats, it is experienced as an asymmetric veto that extracts national policy coherence and imposes diffuse costs. The engine computes this divergence from structural data â beneficiary/victim declarations and differentiated exit options â without requiring the claim to resolve the dispute.
 *
 * DIRECTIONALITY LOGIC:
 *   Resource-rich provincial governments and the fossil fuel sector are structural beneficiaries (low d), capturing regulatory autonomy and blocking costs. The federal government, non-resource provinces, and Indigenous nations are structural targets (high d), bearing the costs of policy paralysis and regulatory exclusion. The directionality spread is wide because the constraint is explicitly zero-sum in its sovereignty claim: provincial gain is federal loss. No override is needed because the structural derivation captures the relationship accurately.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading risks mandatrophy if the original 1982 problem (federal confiscation of resource revenues) is now dead, but the arrangement persists as a veto weapon. The founding problem status is contested: beneficiaries claim federal carbon pricing and environmental assessment represent a new National Energy Program; federal and scholarly corroboration outside the beneficiary set suggests the revenue issue was settled and current conflicts are about policy overlap, not survival. If the founding problem is dead but the constraint persists, it drifts toward piton or snare; the temporal measurements show rising theater and extraction consistent with this drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Is this constraint a true constitutional interpretation or a political deployment of a contested kernel reading?',
    'Comparative analysis across the three kernel readings to determine which legal precedents and political conditions activate each reading in practice.',
    'If primarily a political deployment, classification leans snare/tangled_rope; if a genuine legal consensus, classification could shift toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Contested kernel reading ambiguity: this constraint is one of three competing constitutional framings.').

omega_variable(
    section_92a_veto_scope,
    'Does section 92A of the Constitution Act, 1982 confer a constitutional veto over federal climate and environmental policy, or merely provincial management rights within federal parameters?',
    'Supreme Court constitutional reference or sustained jurisprudence clarifying the scope of 92A in the context of federal environmental criminal law and carbon pricing.',
    'If 92A is read narrowly, the extraction drops significantly (the constraint weakens to rhetoric); if read broadly, the extraction is structural and actively enforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(section_92a_veto_scope, empirical, 'Empirical ambiguity about the actual legal scope of s.92A.').

omega_variable(
    sovereignty_equivalence,
    'Does provincial ownership of natural resources logically entail full territorial sovereignty, or is this an equivocation between property rights and constitutional authority?',
    'Philosophical and legal analysis of sovereignty concepts; comparative federalism examining whether resource ownership in other federations carries similar constitutional weight.',
    'If ownership does not entail sovereignty, the foundational premise of this reading collapses and the constraint reverts to a standard division-of-powers dispute (lower extraction, different classification).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_equivalence, conceptual, 'Conceptual ambiguity in the sovereignty claim grounding this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(provincial_sovereignty_boundary_tr_t0, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 0, 0.15).
narrative_ontology:measurement(provincial_sovereignty_boundary_tr_t5, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 5, 0.18).
narrative_ontology:measurement(provincial_sovereignty_boundary_tr_t10, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 10, 0.22).
narrative_ontology:measurement(provincial_sovereignty_boundary_tr_t15, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 15, 0.28).
narrative_ontology:measurement(provincial_sovereignty_boundary_tr_t20, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 20, 0.32).
narrative_ontology:measurement(provincial_sovereignty_boundary_tr_t25, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 25, 0.38).
narrative_ontology:measurement(provincial_sovereignty_boundary_tr_t30, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 30, 0.42).
narrative_ontology:measurement(provincial_sovereignty_boundary_tr_t35, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 35, 0.44).
narrative_ontology:measurement(provincial_sovereignty_boundary_tr_t40, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 40, 0.45).

% Extraction over time
narrative_ontology:measurement(provincial_sovereignty_boundary_be_t0, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(provincial_sovereignty_boundary_be_t5, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(provincial_sovereignty_boundary_be_t10, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(provincial_sovereignty_boundary_be_t15, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(provincial_sovereignty_boundary_be_t20, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(provincial_sovereignty_boundary_be_t25, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 25, 0.6).
narrative_ontology:measurement(provincial_sovereignty_boundary_be_t30, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 30, 0.65).
narrative_ontology:measurement(provincial_sovereignty_boundary_be_t35, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 35, 0.7).
narrative_ontology:measurement(provincial_sovereignty_boundary_be_t40, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 40, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(provincial_sovereignty_boundary_su_t0, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(provincial_sovereignty_boundary_su_t5, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 5, 0.4).
narrative_ontology:measurement(provincial_sovereignty_boundary_su_t10, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(provincial_sovereignty_boundary_su_t15, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 15, 0.55).
narrative_ontology:measurement(provincial_sovereignty_boundary_su_t20, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(provincial_sovereignty_boundary_su_t25, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 25, 0.68).
narrative_ontology:measurement(provincial_sovereignty_boundary_su_t30, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(provincial_sovereignty_boundary_su_t35, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 35, 0.76).
narrative_ontology:measurement(provincial_sovereignty_boundary_su_t40, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 40, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(provincial_sovereignty_boundary__resource_sovereignty_primacy, constitutional_subordination).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__resource_sovereignty_primacy, compact_federalism).

% DUAL FORMULATION NOTE:
% This constraint is the resource_sovereignty_primacy reading of the provincial_sovereignty_boundary kernel. It decomposes from the colloquial label 'provincial sovereignty' into three structurally distinct claims: constitutional_subordination (provinces as creatures of the center), compact_federalism (confederation as a compact among sovereign provinces), and resource_sovereignty_primacy (s.92A resource ownership as absolute sovereignty). Each has distinct epsilon, beneficiary/victim structures, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
