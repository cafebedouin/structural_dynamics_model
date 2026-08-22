% ============================================================================
% CONSTRAINT STORY: hoa_covenant_scope__coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: hoa_covenant_scope__coordination_reading
 *   human_readable: HOA Covenant: Shared Infrastructure Coordination Reading
 *   domain: property_law/collective_governance/urban_planning
 *
 * SUMMARY:
 *   A homeowners association (HOA) covenant establishes rules for shared
 *   infrastructure maintenance and cost allocation in a multi-owner
 *   residential community. This is the coordination reading: the covenant
 *   exists to solve a genuine collective-action problem (shared
 *   infrastructure, interdependent maintenance, free-rider incentives) by
 *   establishing transparent cost recovery, joint decision-making, and
 *   objective enforcement limited to infrastructure-related purposes. The
 *   constraint is presented and operates as a Rope — genuine coordination
 *   with symmetric beneficiaries and minimal extractive overhead. This
 *   reading contests two sibling readings: the behavioral_control_reading
 *   frames the same covenant as a mechanism for aesthetic uniformity and
 *   conformity enforcement; the extraction_reading frames it as a revenue
 *   generation tool and board power consolidation device. This story
 *   instantiates the coordination reading and declares its structural
 *   relationships to those siblings.
 *
 * KEY AGENTS:
 *   - all_homeowners: collective beneficiary and agenda-setter; participate in covenant governance through voting and board service
 *   - board_administrators: operational agenda-setter; implement collectively-decided maintenance budgets and enforce cost obligations
 *   - free_riders: individual homeowners who attempt to avoid infrastructure cost contributions; narrow enforcement target
 *   - infrastructure_creditors: lenders and service providers; benefit from predictable assessment revenue that funds maintenance
 *   - new_buyers: incoming homeowners; inherit pre-maintained infrastructure and the covenant arrangement
 *   - non_resident_investors: external observers and cost discipline enforcers; hold properties for investment without occupying them
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hoa_covenant_scope__coordination_reading, 0.18).
domain_priors:suppression_score(hoa_covenant_scope__coordination_reading, 0.12).
domain_priors:theater_ratio(hoa_covenant_scope__coordination_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, resistance, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hoa_covenant_scope__coordination_reading, rope).
narrative_ontology:human_readable(hoa_covenant_scope__coordination_reading, "HOA Covenant: Shared Infrastructure Coordination Reading").
narrative_ontology:topic_domain(hoa_covenant_scope__coordination_reading, "property_law/collective_governance/urban_planning").

domain_priors:requires_active_enforcement(hoa_covenant_scope__coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hoa_covenant_scope__coordination_reading, '43a0d1c4-7df5-4655-bc33-5d43763150f7').
narrative_ontology:cs_kernel_codification('43a0d1c4-7df5-4655-bc33-5d43763150f7', formalized).
narrative_ontology:cs_authority_grounding('43a0d1c4-7df5-4655-bc33-5d43763150f7', lineage).
narrative_ontology:cs_interpretation_layer_present('43a0d1c4-7df5-4655-bc33-5d43763150f7').
narrative_ontology:cs_reading_relation('43a0d1c4-7df5-4655-bc33-5d43763150f7', hoa_covenant_scope__behavioral_control_reading, coexists_with).
narrative_ontology:cs_reading_relation('43a0d1c4-7df5-4655-bc33-5d43763150f7', hoa_covenant_scope__extraction_reading, influences).
narrative_ontology:cs_axiom('43a0d1c4-7df5-4655-bc33-5d43763150f7', foundational, shared_infrastructure_coordination_is_primary_function).
narrative_ontology:cs_axiom_status(shared_infrastructure_coordination_is_primary_function, holdable).
narrative_ontology:cs_axiom_grounding('43a0d1c4-7df5-4655-bc33-5d43763150f7', shared_infrastructure_coordination_is_primary_function, empirically_contingent).
narrative_ontology:cs_axiom('43a0d1c4-7df5-4655-bc33-5d43763150f7', foundational, enforcement_scope_limited_to_cost_recovery_and_objective_nuisance).
narrative_ontology:cs_axiom_status(enforcement_scope_limited_to_cost_recovery_and_objective_nuisance, holdable).
narrative_ontology:cs_axiom_grounding('43a0d1c4-7df5-4655-bc33-5d43763150f7', enforcement_scope_limited_to_cost_recovery_and_objective_nuisance, conventional).
narrative_ontology:cs_reference_frame('43a0d1c4-7df5-4655-bc33-5d43763150f7', infrastructure_coordination_framework).
narrative_ontology:cs_drift_state('43a0d1c4-7df5-4655-bc33-5d43763150f7', contemporary_post_2000s_hoa_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('43a0d1c4-7df5-4655-bc33-5d43763150f7', '').
narrative_ontology:cs_kernel_id(hoa_covenant_scope__coordination_reading, hoa_covenant_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__coordination_reading, all_homeowners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__coordination_reading, infrastructure_creditors).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__coordination_reading, new_buyers).
narrative_ontology:constraint_victim(hoa_covenant_scope__coordination_reading, free_riders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Collectively own and maintain shared infrastructure: roads, drainage, common areas, structural utilities. Each homeowner benefits from maintained infrastructure without bearing the full maintenance cost alone; collectively they set covenant terms through voting and board service. Individual exit is possible through sale but transfer of the covenant to new owners is structurally necessary for the arrangement to persist.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, all_homeowners, beneficiary,
    organized, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(hoa_covenant_scope__coordination_reading, all_homeowners, agenda_setter).

% Implement and administer the covenant on behalf of the homeowner collective. They set assessment levels, budget for maintenance, communicate enforcement actions, and resolve disputes over cost allocation. Their power is delegated by and revocable by the homeowner majority.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, board_administrators, agenda_setter,
    moderate, biographical, mobile, local).

% Individual homeowners who attempt to avoid contributing to shared infrastructure maintenance or who defer participation in mandatory upkeep. The covenant's enforcement against them is narrow and objective: cost recovery for specific shared infrastructure repairs and abatement of physical nuisance that impairs others' use of shared systems.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, free_riders, payer,
    powerless, immediate, trapped, local).

% Lenders and service providers (road contractors, drainage specialists, utility co-ops) who depend on predictable HOA assessment revenue to finance and maintain shared infrastructure. They benefit from the covenant's enforcement because it ensures stable funding for projects that serve all homeowners.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, infrastructure_creditors, beneficiary,
    organized, generational, analytical, local).

% Incoming homeowners inherit the covenant as part of property transfer. They benefit from pre-existing maintained infrastructure (roads, drainage, utilities) and the institutional arrangement that sustains it. Their acceptance of the covenant term is a disclosed condition of purchase.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, new_buyers, beneficiary,
    moderate, biographical, constrained, local).

% Property owners who do not occupy units but hold them as investments. They are subject to assessments but do not use common infrastructure directly. Their relationship to the covenant is as external enforcers of cost discipline: they pressure management to maintain efficiency and resist rent-seeking.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, non_resident_investors, observer,
    moderate, biographical, mobile, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hoa_covenant_scope__coordination_reading, all_homeowners).
narrative_ontology:fixing_cost_class(hoa_covenant_scope__coordination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of shared infrastructure maintenance and cost allocation in a multi-owner property system: roads, drainage, utilities, and common areas benefit all residents but would be under-maintained if each owner acted independently. The covenant establishes a mechanism for joint decision-making, compulsory contribution, and objective enforcement that makes infrastructure investment rational for individual homeowners.
% TRANSFER_FUNCTION: Moves maintenance costs from infrastructure creditors and service providers (who would otherwise face payment uncertainty) to the homeowner collective, distributed proportionally by assessed property value or usage. The transfer is transparent and limited to the marginal cost of shared infrastructure upkeep.
% ABSENT_VOICES: Renters and squatters occupy units but have no standing in covenant decisions; they bear infrastructure costs indirectly through rent but cannot contest assessments or participate in budget decisions. Neighboring non-covenant properties benefit from drainage and road infrastructure without formal contribution (positive externality).
% DISAPPEARANCE_RATIONALE: If the covenant vanished, homeowners would face collective-action paralysis on infrastructure maintenance: roads would deteriorate, drainage would fail during heavy rain causing property damage, utilities would become unreliable. Median property values would drop as deferred maintenance accumulated; existing homeowners would eventually re-contract the same mechanism or the properties would consolidate into a single owner-operator (apartment-complex model).
% FOUNDING_PROBLEM: Early suburban development created physical interdependencies (shared roads, joint drainage systems, common utilities) but no legal framework to compel joint maintenance. Individual homeowners had incentive to free-ride on others' upkeep investment; infrastructure deteriorated faster than any single owner could repair it unilaterally.
% FOUNDING_PROBLEM_CORROBORATION: Municipalities and state governments established statutory frameworks for HOAs beginning in the 1960s-1970s specifically to address the documented infrastructure deterioration problem (documented in planning commission archives and property assessor data). Lenders require covenant existence as a condition of mortgage funding, citing the infrastructure maintenance problem. Independent urban planning scholarship confirms that covenant-governed neighborhoods show better infrastructure persistence than non-covenanted multi-owner properties with similar age and construction.
narrative_ontology:disappearance_verdict(hoa_covenant_scope__coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(hoa_covenant_scope__coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hoa_covenant_scope__coordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hoa_covenant_scope__coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hoa_covenant_scope__coordination_reading, 0.18, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is low (0.18 at interval end) because the covenant's design constrains it to objective infrastructure costs, not subjective preferences. Suppression is low (0.12) because enforcement targets specific cost obligations, not behavioral compliance broadly; exit options for individual homeowners include sale or negotiated exemption from specific maintenance tasks. Theater ratio is minimal (0.08) because the covenant's operation is functionally straightforward: collect assessments, budget for maintenance, execute repairs, account for expenditures. The measurement series shows stable metrics over 40 time periods, indicating the constraint maintains its coordination function without drift toward extractive overhead or theatrical enforcement. The shared time grid ensures every metric is measured at every time point examined.
 *
 * PERSPECTIVAL GAP:
 *   A homeowner seat and the board seat should compute the same type (both benefit from infrastructure coordination symmetrically) but with different power atoms: homeowners are organized-collective while board members are moderate-individual delegates. A free-rider seat and a committed homeowner seat diverge sharply: the free-rider experiences suppression and constrained exit, while the committed homeowner experiences mobility and beneficiary status. The engine computes these divergences from the structural data without any authored classification override.
 *
 * DIRECTIONALITY LOGIC:
 *   All homeowners are beneficiaries symmetrically (the covenant coordinates shared infrastructure that benefits everyone equally); the collective agenda-setter status is distributed among homeowners through voting. Free-riders are targets of narrow, objective enforcement (cost recovery for infrastructure they used but didn't pay for). Board administrators are moderate-powered delegates whose authority is revocable. Directionality for all homeowners sits near d=0.5 (symmetric benefit and cost) rather than at the target end because exit options include sale (mobile) and the covenant benefits are transparent and compulsory. Infrastructure creditors sit at d=0.0 (beneficiary end) because they collect reliable revenue from the arrangement.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (shared infrastructure under-maintenance in multi-owner property systems) remains live and empirically established. The covenant persists because it continues to solve that problem demonstrably: neighborhoods with covenants show better infrastructure maintenance trajectories than non-covenanted multi-owner properties. The coordination function has not atrophied into theatrical performance; enforcement is narrow and objective. This reading prevents misclassification of the covenant as a snare (which would require identifying extraction victims and captured interests benefiting from regulation) or as a piton (which would require demonstrating that the founding problem is dead but the constraint persists inertially). The constraint is a genuine rope precisely because the coordination problem is live and the covenant's enforcement mechanisms remain proportional to that problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_boundary_infrastructure_vs_behavior,
    'Where is the line between legitimate infrastructure-focused enforcement and overreach into behavioral control? Specifically: does enforcing lawn maintenance fall under ''common area appearance impacting property value'' (infrastructure-adjacent) or is it pure behavioral conformity enforcement?',
    'Examine enforcement records: distinguish cases enforced for infrastructure cost recovery (e.g., roof maintenance affecting drainage, driveway cracks creating liability) from cases enforced for aesthetic conformity alone (e.g., lawn color, fence style). Measure the proportion of enforcement actions that have objective, infrastructure-related consequences.',
    'A high proportion of enforcement tied to objective infrastructure consequences supports the coordination reading. A high proportion tied purely to aesthetic conformity signals shift toward the behavioral_control_reading and higher ε.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_boundary_infrastructure_vs_behavior, empirical, 'Boundary between infrastructure coordination and behavioral control enforcement.').

omega_variable(
    board_capture_risk_assessment,
    'Is the board functioning as a transparent collective agent for homeowner infrastructure interests, or has it captured power to pursue extraction through selective enforcement and fee proliferation?',
    'Audit board meeting minutes for conflict-of-interest disclosure, assessment vote margins, fine distribution by property type/owner status, and rate of assessment increases relative to documented inflation and infrastructure inflation. Survey homeowner perception of board transparency and whether assessment votes reflect majority preference.',
    'Evidence of board capture, selective enforcement, or non-transparent fee increases would shift this reading toward the extraction_reading and raise ε substantially (0.18 → 0.60+). Evidence of transparent, majority-accountable governance supports the coordination reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(board_capture_risk_assessment, empirical, 'Board institutional capture and extraction risk.').

omega_variable(
    covenant_scope_drift_over_time,
    'Does the covenant''s enforcement scope remain bounded to infrastructure coordination and objective nuisance, or does it drift toward broader behavioral conformity and lifestyle regulation?',
    'Compare covenant interpretation and enforcement practice across 10-year intervals: measure the types and severity of violations enforced, the ratio of infrastructure-cost-recovery fines to aesthetic-conformity fines, and amendment activity that expands or narrows enforcement scope.',
    'Scope drift toward behavioral control would signal transition from coordination_reading toward behavioral_control_reading. Stable scope supports the coordination reading. Drift detection informs whether the constraint''s type changes over time.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(covenant_scope_drift_over_time, empirical, 'Temporal drift in covenant enforcement scope.').

omega_variable(
    free_rider_vs_victim_distinction,
    'Are homeowners who decline to participate in covenant-mandated infrastructure upkeep properly characterized as free-riders (rational actors trying to avoid costs they benefit from) or as victims of coercive cost allocation?',
    'Interview non-compliant homeowners: distinguish those attempting to evade costs they acknowledge benefiting from (free-riders) from those who dispute the infrastructure necessity, the cost allocation fairness, or their obligation to contribute (potential victims). Measure the proportion of each.',
    'High free-rider proportion (acknowledged benefit, attempted evasion) supports the coordination reading with victims=none. High victim proportion (contested legitimacy of the arrangement) signals need to reconceptualize under the extraction_reading or adopt a contested reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(free_rider_vs_victim_distinction, conceptual, 'Distinction between free-riders and victims in covenant non-compliance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hoa_covenant_scope__coordination_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hoa__tr_t0, hoa_covenant_scope__coordination_reading, theater_ratio, 0, 0.06).
narrative_ontology:measurement_basis(hoa__tr_t0, observed).
narrative_ontology:measurement(hoa__tr_t8, hoa_covenant_scope__coordination_reading, theater_ratio, 8, 0.07).
narrative_ontology:measurement_basis(hoa__tr_t8, observed).
narrative_ontology:measurement(hoa__tr_t16, hoa_covenant_scope__coordination_reading, theater_ratio, 16, 0.08).
narrative_ontology:measurement_basis(hoa__tr_t16, observed).
narrative_ontology:measurement(hoa__tr_t24, hoa_covenant_scope__coordination_reading, theater_ratio, 24, 0.08).
narrative_ontology:measurement_basis(hoa__tr_t24, observed).
narrative_ontology:measurement(hoa__tr_t32, hoa_covenant_scope__coordination_reading, theater_ratio, 32, 0.08).
narrative_ontology:measurement_basis(hoa__tr_t32, observed).
narrative_ontology:measurement(hoa__tr_t40, hoa_covenant_scope__coordination_reading, theater_ratio, 40, 0.08).
narrative_ontology:measurement_basis(hoa__tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(hoa__be_t0, hoa_covenant_scope__coordination_reading, base_extractiveness, 0, 0.16).
narrative_ontology:measurement_basis(hoa__be_t0, observed).
narrative_ontology:measurement(hoa__be_t8, hoa_covenant_scope__coordination_reading, base_extractiveness, 8, 0.17).
narrative_ontology:measurement_basis(hoa__be_t8, observed).
narrative_ontology:measurement(hoa__be_t16, hoa_covenant_scope__coordination_reading, base_extractiveness, 16, 0.18).
narrative_ontology:measurement_basis(hoa__be_t16, observed).
narrative_ontology:measurement(hoa__be_t24, hoa_covenant_scope__coordination_reading, base_extractiveness, 24, 0.19).
narrative_ontology:measurement_basis(hoa__be_t24, observed).
narrative_ontology:measurement(hoa__be_t32, hoa_covenant_scope__coordination_reading, base_extractiveness, 32, 0.18).
narrative_ontology:measurement_basis(hoa__be_t32, observed).
narrative_ontology:measurement(hoa__be_t40, hoa_covenant_scope__coordination_reading, base_extractiveness, 40, 0.18).
narrative_ontology:measurement_basis(hoa__be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(hoa__su_t0, hoa_covenant_scope__coordination_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement_basis(hoa__su_t0, observed).
narrative_ontology:measurement(hoa__su_t8, hoa_covenant_scope__coordination_reading, suppression_requirement, 8, 0.11).
narrative_ontology:measurement_basis(hoa__su_t8, observed).
narrative_ontology:measurement(hoa__su_t16, hoa_covenant_scope__coordination_reading, suppression_requirement, 16, 0.12).
narrative_ontology:measurement_basis(hoa__su_t16, observed).
narrative_ontology:measurement(hoa__su_t24, hoa_covenant_scope__coordination_reading, suppression_requirement, 24, 0.12).
narrative_ontology:measurement_basis(hoa__su_t24, observed).
narrative_ontology:measurement(hoa__su_t32, hoa_covenant_scope__coordination_reading, suppression_requirement, 32, 0.12).
narrative_ontology:measurement_basis(hoa__su_t32, observed).
narrative_ontology:measurement(hoa__su_t40, hoa_covenant_scope__coordination_reading, suppression_requirement, 40, 0.12).
narrative_ontology:measurement_basis(hoa__su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hoa_covenant_scope__coordination_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(hoa_covenant_scope__coordination_reading, 0.12).
narrative_ontology:affects_constraint(hoa_covenant_scope__coordination_reading, hoa_covenant_scope__behavioral_control_reading).
narrative_ontology:affects_constraint(hoa_covenant_scope__coordination_reading, hoa_covenant_scope__extraction_reading).

% DUAL FORMULATION NOTE:
% The HOA covenant kernel (a formalized legal document and institutional structure) admits three structurally distinct constraint readings: coordination_reading (this file) frames enforcement scope narrowly to infrastructure cost recovery with symmetric beneficiaries; behavioral_control_reading frames the same covenant as enforcing aesthetic uniformity and behavioral conformity with asymmetric beneficiaries (aesthetically-aligned homeowners vs. non-conformists); extraction_reading frames it as a revenue mechanism and board power consolidation tool with clear extraction victims. Each reading has a different ε, different beneficiary/victim structure, and different type. They are not variants of one constraint viewed from different angles; they are three genuinely distinct constraints instantiated in the same legal document. The sibling stories are linked via network.affects_constraints: the coordination reading establishes the legitimacy frame that the behavioral_control and extraction readings must overcome to claim authority. See the three stories' commentary.kernel_context fields for the full reading relationships.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
