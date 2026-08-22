% ============================================================================
% CONSTRAINT STORY: hoa_covenant_scope__coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hoa_covenant_scope__coordination_reading, []).

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
 *   constraint_id: hoa_covenant_scope__coordination_reading
 *   human_readable: HOA Covenant Scope — Coordination Reading
 *   domain: property_law/collective_governance/urban_planning
 *
 * SUMMARY:
 *   This constraint story captures the coordination reading of the HOA
 *   covenant kernel: the covenant as a genuine collective-action solution for
 *   shared infrastructure maintenance and objective externality resolution.
 *   The constraint is claimed as rope — a pure coordination mechanism with
 *   symmetric benefits, narrow enforcement scope limited to cost recovery and
 *   objectively measurable standards (decibel limits, runoff volumes,
 *   structural safety), and no aesthetic or behavioral conformity demands.
 *   Extraction is low (0.15) because assessments track documented
 *   infrastructure costs; suppression is low (0.25) because enforcement
 *   targets only free-riding and measurable nuisance, not taste. Theater is
 *   minimal (0.1) because the board's activity is functional maintenance, not
 *   performative control.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hoa_covenant_scope__coordination_reading, 0.15).
domain_priors:suppression_score(hoa_covenant_scope__coordination_reading, 0.25).
domain_priors:theater_ratio(hoa_covenant_scope__coordination_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hoa_covenant_scope__coordination_reading, rope).
narrative_ontology:human_readable(hoa_covenant_scope__coordination_reading, "HOA Covenant Scope — Coordination Reading").
narrative_ontology:topic_domain(hoa_covenant_scope__coordination_reading, "property_law/collective_governance/urban_planning").

domain_priors:requires_active_enforcement(hoa_covenant_scope__coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hoa_covenant_scope__coordination_reading, '5cae49b7-55ba-4b49-962c-eabacb069a7b').
narrative_ontology:cs_kernel_codification('5cae49b7-55ba-4b49-962c-eabacb069a7b', formalized).
narrative_ontology:cs_authority_grounding('5cae49b7-55ba-4b49-962c-eabacb069a7b', practice).
narrative_ontology:cs_interpretation_layer_present('5cae49b7-55ba-4b49-962c-eabacb069a7b').
narrative_ontology:cs_reading_relation('5cae49b7-55ba-4b49-962c-eabacb069a7b', hoa_covenant_scope__behavioral_control_reading, coexists_with).
narrative_ontology:cs_reading_relation('5cae49b7-55ba-4b49-962c-eabacb069a7b', hoa_covenant_scope__extraction_reading, coexists_with).
narrative_ontology:cs_axiom('5cae49b7-55ba-4b49-962c-eabacb069a7b', foundational, covenant_scope_limited_to_infrastructure_and_objective_nuisance).
narrative_ontology:cs_axiom_status(covenant_scope_limited_to_infrastructure_and_objective_nuisance, holdable).
narrative_ontology:cs_axiom_grounding('5cae49b7-55ba-4b49-962c-eabacb069a7b', covenant_scope_limited_to_infrastructure_and_objective_nuisance, conventional).
narrative_ontology:cs_axiom('5cae49b7-55ba-4b49-962c-eabacb069a7b', foundational, assessments_tied_to_documented_costs_only).
narrative_ontology:cs_axiom_status(assessments_tied_to_documented_costs_only, holdable).
narrative_ontology:cs_axiom_grounding('5cae49b7-55ba-4b49-962c-eabacb069a7b', assessments_tied_to_documented_costs_only, conventional).
narrative_ontology:cs_reference_frame('5cae49b7-55ba-4b49-962c-eabacb069a7b', planned_community_infrastructure_coordination).
narrative_ontology:cs_drift_state('5cae49b7-55ba-4b49-962c-eabacb069a7b', contemporary_hoa_landscape, gap(stable, minor, true)).
narrative_ontology:cs_created_at('5cae49b7-55ba-4b49-962c-eabacb069a7b', '').
narrative_ontology:cs_kernel_id(hoa_covenant_scope__coordination_reading, hoa_covenant_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__coordination_reading, all_homeowners).
narrative_ontology:constraint_victim(hoa_covenant_scope__coordination_reading, free_riders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__coordination_reading, hoa_board).
narrative_ontology:constraint_vindicates(hoa_covenant_scope__coordination_reading, shared_infrastructure_requires_collective_funding).
narrative_ontology:constraint_vindicates(hoa_covenant_scope__coordination_reading, objective_nuisance_standards_prevent_externalities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Every homeowner in the HOA community benefits from maintained shared infrastructure (roads, drainage, lighting, common areas) and from objective nuisance standards that prevent genuine externalities. They contribute through assessments proportional to infrastructure costs. Exit means selling the home, which is possible but costly and disruptive.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, all_homeowners, beneficiary,
    moderate, biographical, constrained, local).

% Homeowners who would avoid paying their share of infrastructure costs or would create objective nuisances (uncontrolled runoff, hazardous conditions, noise exceeding established decibel limits) if not constrained. The covenant's enforcement mechanism targets this behavior — they pay through compliance or fines.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, free_riders, payer,
    powerless, immediate, constrained, local).

% Elected volunteer board administers the covenant: sets assessment levels tied to documented infrastructure costs, enforces objective nuisance standards, maintains common elements. Board members are homeowners themselves and benefit symmetrically. Their authority is narrow — limited to cost recovery and objectively measurable standards.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, hoa_board, agenda_setter,
    organized, biographical, mobile, local).
narrative_ontology:stakeholder_secondary_role(hoa_covenant_scope__coordination_reading, hoa_board, beneficiary).

% Local government retains underlying regulatory authority (zoning, building codes, environmental regulations). The HOA covenant operates as a supplementary layer for hyper-local infrastructure and nuance standards the municipality does not provide. Municipality can intervene if covenant enforcement violates higher law.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, municipal_government, observer,
    institutional, generational, analytical, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective action problem of funding and maintaining shared infrastructure (roads, stormwater, lighting, common areas) that no single homeowner can efficiently provide alone, and resolves genuine externalities (runoff, noise, hazardous conditions) through objective, measurable standards rather than aesthetic preferences.
% TRANSFER_FUNCTION: Moves infrastructure maintenance costs from the collective pool (funded by proportional assessments) to service providers and materials. Transfers the cost of externality remediation from affected neighbors to the party creating the externality, via fines tied to objective standards.
% ABSENT_VOICES: Future homeowners who will inherit the covenant without having voted on it; renters in the community who bear assessment pass-through but have no vote; adjacent non-HOA property owners affected by the community's infrastructure decisions (e.g., stormwater flow).
% DISAPPEARANCE_RATIONALE: If the covenant vanished overnight, shared infrastructure would degrade rapidly (no funding mechanism), objective nuisance standards would disappear (reverting to municipal minimums which are weaker on hyper-local issues), and the community would face immediate collective action failures — free-riding on maintenance, uncontrolled externalities. The world rearranges because arrangements depend on this constraint.
% FOUNDING_PROBLEM: New planned communities in the 1960s-1980s needed a mechanism to maintain shared infrastructure that municipalities would not adopt (private roads, private stormwater, common areas) and to prevent the tragedy of the commons where individual owners underinvest in maintenance that benefits all.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by municipal planning departments (which confirm they do not maintain private infrastructure), civil engineering literature on infrastructure funding gaps in private communities, and homeowner surveys across multiple HOA communities showing majority support for assessment-funded maintenance. The founding problem is attested from outside the HOA board itself.
narrative_ontology:disappearance_verdict(hoa_covenant_scope__coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(hoa_covenant_scope__coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hoa_covenant_scope__coordination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(hoa_covenant_scope__coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hoa_covenant_scope__coordination_reading, 0.15, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hoa_covenant_scope__coordination_reading_tests).
:- end_tests(hoa_covenant_scope__coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness reflects assessments tied to actual infrastructure costs with transparent accounting. Low suppression reflects enforcement limited to objective standards (measurable noise, runoff, safety) — no architectural review committees, no paint-color rules, no behavioral mandates. The beneficiary set is symmetric (all homeowners) because infrastructure and externality protection benefit everyone equally. The victim set is narrow (free_riders) — only those who would defect from the collective action problem or create measurable externalities bear enforcement costs. This is the coordination function isolated from the behavioral control and extraction readings.
 *
 * PERSPECTIVAL GAP:
 *   From the homeowner seat, the constraint is experienced as a fair cost-sharing mechanism for necessary infrastructure. From the free_rider seat, it is experienced as coercive extraction (but the extraction is the cost of the externality they would impose). From the board seat, it is administrative burden with no personal gain beyond shared homeowner benefit. The engine computes these divergences from the structural data — the coordination reading claims rope and the metrics support it.
 *
 * DIRECTIONALITY LOGIC:
 *   All homeowners are structural beneficiaries (d near 0.0) — they pay assessments but receive equivalent value in maintained infrastructure and externality protection. The hoa_board sits near symmetric (d ~ 0.5) — they administer the constraint and benefit as homeowners, but their administrative role is unpaid volunteer work. Free_riders are the structural targets (d near 1.0) — they would extract the benefits without paying the costs, so the constraint's enforcement falls on them. Municipal government is analytical observer (d = 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (private infrastructure maintenance + hyper-local externality resolution) remains live — municipalities still do not adopt private roads/stormwater, and collective action failures still occur without a binding mechanism. The constraint has not atrophied into piton because the coordination function is actively used and the enforcement scope has not drifted toward aesthetic/behavioral control. Mandatrophy is not resolved — the constraint continues to serve its founding purpose.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_behavioral_boundary,
    'Where is the structural boundary between objective nuisance standards (coordination) and aesthetic/behavioral rules (behavioral control)?',
    'Codification test: can the standard be reduced to a measurable physical quantity (decibels, runoff volume, structural load) without reference to taste, preference, or ''community character''? If yes, it belongs to the coordination reading; if no, it belongs to behavioral_control_reading.',
    'If the boundary is porous, the coordination reading''s low ε claim is contaminated by behavioral control provisions that inevitably exist in the same covenant document. The ε-invariance principle would require decomposing the covenant into separate constraints per measurable standard.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_behavioral_boundary, conceptual, 'Whether objective nuisance standards can be cleanly separated from aesthetic rules in the same covenant.').

omega_variable(
    enforcement_drift_trajectory,
    'Does the coordination reading''s enforcement mechanism inevitably drift toward behavioral control or extraction over time, or can it remain stable?',
    'Longitudinal study of HOA covenants that started with coordination-only scope: track whether enforcement provisions expand to aesthetic/behavioral rules or fine structures proliferate beyond cost recovery.',
    'If drift is structurally inevitable, the coordination reading describes a transient state, not a stable constraint type. The constraint would be a scaffold (transitional) rather than a rope, or would inevitably mutate into the behavioral_control_reading or extraction_reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_drift_trajectory, empirical, 'Whether pure coordination covenants are stable or inevitably drift toward extraction/control.').

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine reading of the hoa_covenant_scope kernel, or does it artificially isolate one function from an inseparable whole?',
    'Counterfactual test: if a community adopted ONLY the coordination provisions (infrastructure funding + objective nuisance standards) and explicitly prohibited aesthetic/behavioral rules and fine proliferation, would the resulting constraint be recognizable as the same kernel? If yes, the reading is ε-invariant; if no, the kernel is inseparable.',
    'If the kernel is inseparable, the three readings are not distinct constraints but observer perspectives on one constraint — violating the ε-invariance principle. The decomposition would be authorial, not structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Whether the coordination reading is a structurally distinct constraint or an authorial decomposition of an inseparable kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hoa_covenant_scope__coordination_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hoa__tr_t0, hoa_covenant_scope__coordination_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(hoa__tr_t10, hoa_covenant_scope__coordination_reading, theater_ratio, 10, 0.07).
narrative_ontology:measurement(hoa__tr_t20, hoa_covenant_scope__coordination_reading, theater_ratio, 20, 0.08).
narrative_ontology:measurement(hoa__tr_t30, hoa_covenant_scope__coordination_reading, theater_ratio, 30, 0.09).
narrative_ontology:measurement(hoa__tr_t40, hoa_covenant_scope__coordination_reading, theater_ratio, 40, 0.1).

% Extraction over time
narrative_ontology:measurement(hoa__be_t0, hoa_covenant_scope__coordination_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(hoa__be_t10, hoa_covenant_scope__coordination_reading, base_extractiveness, 10, 0.12).
narrative_ontology:measurement(hoa__be_t20, hoa_covenant_scope__coordination_reading, base_extractiveness, 20, 0.14).
narrative_ontology:measurement(hoa__be_t30, hoa_covenant_scope__coordination_reading, base_extractiveness, 30, 0.15).
narrative_ontology:measurement(hoa__be_t40, hoa_covenant_scope__coordination_reading, base_extractiveness, 40, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(hoa__su_t0, hoa_covenant_scope__coordination_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(hoa__su_t10, hoa_covenant_scope__coordination_reading, suppression_requirement, 10, 0.22).
narrative_ontology:measurement(hoa__su_t20, hoa_covenant_scope__coordination_reading, suppression_requirement, 20, 0.24).
narrative_ontology:measurement(hoa__su_t30, hoa_covenant_scope__coordination_reading, suppression_requirement, 30, 0.25).
narrative_ontology:measurement(hoa__su_t40, hoa_covenant_scope__coordination_reading, suppression_requirement, 40, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hoa_covenant_scope__coordination_reading, resource_allocation).
narrative_ontology:affects_constraint(hoa_covenant_scope__coordination_reading, hoa_covenant_scope__behavioral_control_reading).
narrative_ontology:affects_constraint(hoa_covenant_scope__coordination_reading, hoa_covenant_scope__extraction_reading).

% DUAL FORMULATION NOTE:
% This constraint is the coordination_reading of the hoa_covenant_scope kernel. The behavioral_control_reading and extraction_reading are sibling constraints from the same kernel text. This reading claims rope with ε=0.15; the behavioral_control_reading claims tangled_rope with higher ε (aesthetic enforcement adds extraction); the extraction_reading claims snare with highest ε (fine proliferation as revenue). The ε values differ because the readings instantiate different constraints from the same kernel — the coordination reading restricts enforcement to cost recovery and objective standards, while the other readings expand enforcement scope.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
