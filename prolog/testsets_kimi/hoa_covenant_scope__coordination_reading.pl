% ============================================================================
% CONSTRAINT STORY: hoa_covenant_scope__coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:suppression_profile/2,
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
 *   human_readable: HOA Covenant â Coordination Reading (Infrastructure & Externalities)
 *   domain: property_law/collective_governance/urban_planning
 *
 * SUMMARY:
 *   This constraint instantiates the coordination reading of the
 *   homeowners-association covenant kernel. It treats the covenant as a
 *   resource-allocation mechanism designed to solve collective-action
 *   problems in shared infrastructure and genuine externalities among
 *   subdivided properties. The reading asserts symmetric cost-bearing and
 *   benefit-receipt across all homeowners, with enforcement narrowly limited
 *   to objective infrastructure maintenance and nuisance abatement. No party
 *   extracts disproportionate rent; the HOA board administers rather than
 *   profits.
 *
 * KEY AGENTS:
 *   - all_homeowners (beneficiary/payer): symmetrically bear assessment costs and receive infrastructure benefits; exit is property sale.
 *   - hoa_board (agenda_setter): administers assessments and maintenance contracts under transparent budget constraints; no surplus extraction.
 *   - prospective_free_riders (payer): would prefer to externalize maintenance costs; are constrained by covenant obligations.
 *   - municipal_government (observer): recognizes covenants as supplementary private governance; may mandate them for subdivision approval.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hoa_covenant_scope__coordination_reading, 0.18).
domain_priors:suppression_score(hoa_covenant_scope__coordination_reading, 0.15).
domain_priors:theater_ratio(hoa_covenant_scope__coordination_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hoa_covenant_scope__coordination_reading, rope).
narrative_ontology:human_readable(hoa_covenant_scope__coordination_reading, "HOA Covenant â Coordination Reading (Infrastructure & Externalities)").
narrative_ontology:topic_domain(hoa_covenant_scope__coordination_reading, "property_law/collective_governance/urban_planning").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hoa_covenant_scope__coordination_reading, '1e93a239-5adc-4800-abb3-fbc3baaa9c2b').
narrative_ontology:cs_kernel_codification('1e93a239-5adc-4800-abb3-fbc3baaa9c2b', formalized).
narrative_ontology:cs_authority_grounding('1e93a239-5adc-4800-abb3-fbc3baaa9c2b', lineage).
narrative_ontology:cs_interpretation_layer_present('1e93a239-5adc-4800-abb3-fbc3baaa9c2b').
narrative_ontology:cs_reading_relation('1e93a239-5adc-4800-abb3-fbc3baaa9c2b', hoa_covenant_scope__behavioral_control_reading, coexists_with).
narrative_ontology:cs_reading_relation('1e93a239-5adc-4800-abb3-fbc3baaa9c2b', hoa_covenant_scope__extraction_reading, coexists_with).
narrative_ontology:cs_axiom('1e93a239-5adc-4800-abb3-fbc3baaa9c2b', foundational, shared_infrastructure_obligation).
narrative_ontology:cs_axiom_status(shared_infrastructure_obligation, holdable).
narrative_ontology:cs_axiom_grounding('1e93a239-5adc-4800-abb3-fbc3baaa9c2b', shared_infrastructure_obligation, conventional).
narrative_ontology:cs_axiom('1e93a239-5adc-4800-abb3-fbc3baaa9c2b', foundational, objective_nuisance_scope_limit).
narrative_ontology:cs_axiom_status(objective_nuisance_scope_limit, holdable).
narrative_ontology:cs_axiom_grounding('1e93a239-5adc-4800-abb3-fbc3baaa9c2b', objective_nuisance_scope_limit, conventional).
narrative_ontology:cs_reference_frame('1e93a239-5adc-4800-abb3-fbc3baaa9c2b', infrastructure_coordination_ideal).
narrative_ontology:cs_drift_state('1e93a239-5adc-4800-abb3-fbc3baaa9c2b', contemporary_hoa_practice, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('1e93a239-5adc-4800-abb3-fbc3baaa9c2b', '').
narrative_ontology:cs_kernel_id(hoa_covenant_scope__coordination_reading, hoa_covenant_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__coordination_reading, all_homeowners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(hoa_covenant_scope__coordination_reading, all_homeowners).
narrative_ontology:constraint_victim(hoa_covenant_scope__coordination_reading, prospective_free_riders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own property within the covenant-bound subdivision. Pay regular assessments earmarked for road maintenance, drainage, common utilities, and objective nuisance abatement. Receive symmetric benefits from functioning shared infrastructure and avoided neighbor externalities. No homeowner captures disproportionate gain; costs and benefits are designed to be proportionate across the membership.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, all_homeowners, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(hoa_covenant_scope__coordination_reading, all_homeowners, payer).

% Administers the covenant, collects assessments, contracts for infrastructure upkeep, and enforces only objective nuisance and cost-recovery obligations. Operates under a budgetary transparency requirement with surplus recycling or assessment reduction. Authority derives from the recorded covenant instrument and delegated homeowner governance, not from extraction of rents for its own benefit.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, hoa_board, agenda_setter,
    organized, biographical, constrained, local).

% Homeowners who would otherwise prefer to enjoy maintained commons and infrastructure without contributing proportionally. The covenant constrains this option by binding all lots to assessment and maintenance obligations. Their cost is the foregone ability to externalize upkeep and nuisance onto neighboring properties.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, prospective_free_riders, payer,
    moderate, biographical, constrained, local).

% Recognizes the covenant as a private collective-governance instrument that supplements public infrastructure and zoning. May require covenants as a condition of subdivision approval where public service extension is deferred or declined.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, municipal_government, observer,
    institutional, generational, analytical, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinate maintenance of shared private infrastructure (roads, drainage, common utilities, greenways) and resolve genuine negative externalities among geographically adjacent properties where fragmented ownership makes individual bargaining costly and non-excludability invites free-riding.
% TRANSFER_FUNCTION: Moves regular monetary assessments from each homeowner to contracted maintenance providers and common-infrastructure reserves; moves behavioral constraints from free-riding tendency to collective obligation by binding all lots to objective maintenance and nuisance standards.
% ABSENT_VOICES: Renters in covenant-controlled communities often bear passed-through assessment costs without voting rights in covenant governance. Prospective purchasers may be unaware of the full burden at closing. Competitive contractors may be excluded if boards restrict vendor pools, though this is incidental rather than defining.
% DISAPPEARANCE_RATIONALE: If the covenant and its assessment mechanism vanished overnight, the shared private infrastructure would face rapid degradation from free-rider incentives. Property owners would revert to fragmented individual maintenance, ad hoc bilateral bargaining over nuisance, or costly litigation, reducing the functionality of the subdivision.
% FOUNDING_PROBLEM: Maintenance of common-pool infrastructure and abatement of neighborhood externalities among subdivided properties with fragmented ownership, in contexts where public infrastructure extension is deferred or declined and individual enforcement of nuisance is prohibitively expensive.
% FOUNDING_PROBLEM_CORROBORATION: Municipal planning departments and public-finance literature corroborate the free-rider problem in common-interest communities; many jurisdictions require covenants as a condition of subdivision approval precisely because the public sector declines to assume the infrastructure burden. This attestation comes from outside the set of benefiting homeowners.
narrative_ontology:disappearance_verdict(hoa_covenant_scope__coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(hoa_covenant_scope__coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hoa_covenant_scope__coordination_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hoa_covenant_scope__coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hoa_covenant_scope__coordination_reading, 0.18, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is low (0.18) because assessments are tied to actual maintenance expenditures and no agent captures a surplus. Suppression is low (0.15) because the constraint operates primarily through legal expectation and symmetric social pressure rather than coercion; enforcement is limited to objective standards. Theater ratio is very low (0.10) because the bulk of covenant activity is functional infrastructure maintenance rather than symbolic compliance performance. Accessibility collapse is moderate (0.40) because alternatives (public infrastructure, informal agreements) are visible but often legally or practically unavailable in an established subdivision. Resistance is low (0.20) because the symmetric benefit structure attenuates organized opposition.
 *
 * PERSPECTIVAL GAP:
 *   The all_homeowners seat experiences the constraint as coordination: they pay for and receive infrastructure services. The prospective_free_riders seat experiences it as a constraint on their preferred strategy of non-payment. The agenda-setter seat experiences it as administrative duty. The engine will compute divergent per-seat classifications from this structural data: the symmetric beneficiary seat should compute toward rope, while the constrained defector seat sits slightly higher on directionality.
 *
 * DIRECTIONALITY LOGIC:
 *   All_homeowners are symmetric beneficiaries and payers; their directionality is near the middle but slightly toward beneficiary because they net-gain from resolved externalities. Prospective_free_riders are pure payers in the structural sense (they bear the cost of foregone free-riding), giving them a modestly higher d. The hoa_board is an agenda-setter without rent capture, placing it near the symmetric middle. Municipal_government is an analytical observer with no stake in the transfer.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists mandatrophy mislabeling by maintaining a tight coupling between the founding problem (common-pool infrastructure under fragmented ownership) and current function. Assessments are budgeted to services; the board does not accumulate independent power or wealth. If assessments decoupled from maintenance costs, if enforcement expanded to aesthetic preferences, or if the board began extracting surplus, the constraint would drift toward the extraction or behavioral-control readings. The low theater ratio and live founding problem corroboration from municipal planners keep this within rope territory rather than piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    covenant_drift_to_behavioral_control,
    'Does the covenant''s enforcement apparatus remain confined to infrastructure and objective nuisance, or does it drift toward aesthetic and behavioral regulation?',
    'Longitudinal review of enforcement records, fine ledgers, and assessment expenditure ratios over a 10-year window; compare dollars and actions spent on infrastructure/abatement versus design-compliance or lifestyle penalties.',
    'Confirmed drift would reclassify this constraint away from the coordination reading toward behavioral_control_reading or extraction_reading, invalidating the low-extraction rope profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(covenant_drift_to_behavioral_control, empirical, 'Drift from infrastructure coordination toward behavioral control.').

omega_variable(
    externality_genuineness,
    'Are the externalities the covenant resolves (drainage, road wear, common lighting) genuine and non-excludable, or are they artificial burdens constructed to justify the covenant''s existence?',
    'Engineering assessment of infrastructure interdependence within the subdivision; comparison with municipally serviced subdivisions of similar density and vintage.',
    'If the externalities are manufactured or could be served by public infrastructure at comparable cost, the coordination justification weakens and the constraint approaches extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externality_genuineness, empirical, 'Whether resolved externalities are genuine or constructed.').

omega_variable(
    coordination_vs_control_separability,
    'Can infrastructure coordination be structurally separated from behavioral control in covenant design, or does the same enforcement mechanism inevitably enable both?',
    'Comparative institutional analysis of covenants with statutorily narrow versus broad scope; natural experiments from jurisdictions that cap covenant enforceability to infrastructure and recorded safety nuisances.',
    'If inseparable, the coordination reading is unstable and the kernel is inherently contested; if separable, this reading is a viable pure type.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_vs_control_separability, conceptual, 'Structural separability of coordination and control functions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hoa_covenant_scope__coordination_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hoa_coord_tr_t0, hoa_covenant_scope__coordination_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(hoa_coord_tr_t10, hoa_covenant_scope__coordination_reading, theater_ratio, 10, 0.06).
narrative_ontology:measurement(hoa_coord_tr_t20, hoa_covenant_scope__coordination_reading, theater_ratio, 20, 0.07).
narrative_ontology:measurement(hoa_coord_tr_t30, hoa_covenant_scope__coordination_reading, theater_ratio, 30, 0.08).
narrative_ontology:measurement(hoa_coord_tr_t40, hoa_covenant_scope__coordination_reading, theater_ratio, 40, 0.09).
narrative_ontology:measurement(hoa_coord_tr_t50, hoa_covenant_scope__coordination_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(hoa_coord_be_t0, hoa_covenant_scope__coordination_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(hoa_coord_be_t10, hoa_covenant_scope__coordination_reading, base_extractiveness, 10, 0.13).
narrative_ontology:measurement(hoa_coord_be_t20, hoa_covenant_scope__coordination_reading, base_extractiveness, 20, 0.15).
narrative_ontology:measurement(hoa_coord_be_t30, hoa_covenant_scope__coordination_reading, base_extractiveness, 30, 0.16).
narrative_ontology:measurement(hoa_coord_be_t40, hoa_covenant_scope__coordination_reading, base_extractiveness, 40, 0.17).
narrative_ontology:measurement(hoa_coord_be_t50, hoa_covenant_scope__coordination_reading, base_extractiveness, 50, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(hoa_covenant_scope__coordination_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hoa_covenant_scope__coordination_reading, resource_allocation).
narrative_ontology:affects_constraint(hoa_covenant_scope__coordination_reading, behavioral_control_reading).
narrative_ontology:affects_constraint(hoa_covenant_scope__coordination_reading, extraction_reading).

% DUAL FORMULATION NOTE:
% This constraint is the coordination reading of the hoa_covenant_scope kernel. It decomposes the colloquial label 'HOA covenant' into a structurally specific claim: the covenant as a resource-allocation mechanism for genuine common-pool infrastructure. Sibling readings (behavioral_control_reading, extraction_reading) instantiate different structural claims from the same legal kernel. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
