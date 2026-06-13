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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hoa_covenant_scope__coordination_reading
 *   human_readable: HOA Covenant: Shared Infrastructure Coordination (Coordination Reading)
 *   domain: property_law/collective_governance/urban_planning
 *
 * SUMMARY:
 *   A homeowners association covenant in a residential community with shared
 *   infrastructure (common roads, utilities, drainage) establishes rules for
 *   maintenance funding and enforcement against damage to shared property or
 *   objective nuisance. Under the COORDINATION READING, the covenant exists
 *   to solve a genuine collective-action problem: infrastructure cannot be
 *   maintained without coordinated funding, and individual homeowners cannot
 *   exit without selling. The covenant creates a binding commitment structure
 *   that benefits all residents symmetrically by preventing free-riding and
 *   ensuring reliable infrastructure. This reading treats enforcement scope
 *   narrowly — limited to infrastructure cost recovery and prevention of
 *   objective externalities (neglected property affecting drainage,
 *   structural violations affecting load-bearing shared walls, noise causing
 *   measurable property damage). It does not extend to aesthetic uniformity,
 *   behavioral conformity, or discretionary board authority. The claim is
 *   ROPE; the metrics reflect low extractiveness (0.10-0.16), low suppression
 *   (0.10-0.12), and minimal theater (0.06-0.08), consistent with a genuine
 *   coordination mechanism.
 *
 * KEY AGENTS:
 *   - all_homeowners: collective beneficiaries of shared infrastructure maintenance; bear symmetrical costs and benefits
 *   - governing_board: elected homeowner representatives; administer enforcement within infrastructure scope only
 *   - free_riders: homeowners who would exit funding if not for enforcement; suppressed by covenant cost requirement
 *   - municipality: external observer; can void covenants exceeding legitimate collective governance bounds
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hoa_covenant_scope__coordination_reading, 0.16).
domain_priors:suppression_score(hoa_covenant_scope__coordination_reading, 0.12).
domain_priors:theater_ratio(hoa_covenant_scope__coordination_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, extractiveness, 0.16).
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, resistance, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hoa_covenant_scope__coordination_reading, rope).
narrative_ontology:human_readable(hoa_covenant_scope__coordination_reading, "HOA Covenant: Shared Infrastructure Coordination (Coordination Reading)").
narrative_ontology:topic_domain(hoa_covenant_scope__coordination_reading, "property_law/collective_governance/urban_planning").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hoa_covenant_scope__coordination_reading, 'a18fa498-589c-4467-930d-f620d408a030').
narrative_ontology:cs_kernel_codification('a18fa498-589c-4467-930d-f620d408a030', formalized).
narrative_ontology:cs_authority_grounding('a18fa498-589c-4467-930d-f620d408a030', practice).
narrative_ontology:cs_interpretation_layer_present('a18fa498-589c-4467-930d-f620d408a030').
narrative_ontology:cs_reading_relation('a18fa498-589c-4467-930d-f620d408a030', hoa_covenant_scope__behavioral_control_reading, coexists_with).
narrative_ontology:cs_reading_relation('a18fa498-589c-4467-930d-f620d408a030', hoa_covenant_scope__extraction_reading, influences).
narrative_ontology:cs_axiom('a18fa498-589c-4467-930d-f620d408a030', foundational, infrastructure_coordination_solves_genuine_collective_action_problem).
narrative_ontology:cs_axiom_status(infrastructure_coordination_solves_genuine_collective_action_problem, holdable).
narrative_ontology:cs_axiom_grounding('a18fa498-589c-4467-930d-f620d408a030', infrastructure_coordination_solves_genuine_collective_action_problem, empirically_contingent).
narrative_ontology:cs_axiom('a18fa498-589c-4467-930d-f620d408a030', foundational, beneficiaries_and_payers_are_symmetrical_under_coordination_scope).
narrative_ontology:cs_axiom_status(beneficiaries_and_payers_are_symmetrical_under_coordination_scope, holdable).
narrative_ontology:cs_axiom_grounding('a18fa498-589c-4467-930d-f620d408a030', beneficiaries_and_payers_are_symmetrical_under_coordination_scope, empirically_contingent).
narrative_ontology:cs_reference_frame('a18fa498-589c-4467-930d-f620d408a030', infrastructure_coordination_necessity).
narrative_ontology:cs_drift_state('a18fa498-589c-4467-930d-f620d408a030', contemporary_enforcement_expansion, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('a18fa498-589c-4467-930d-f620d408a030', '').
narrative_ontology:cs_kernel_id(hoa_covenant_scope__coordination_reading, hoa_covenant_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__coordination_reading, all_homeowners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(hoa_covenant_scope__coordination_reading, all_homeowners).
narrative_ontology:constraint_victim(hoa_covenant_scope__coordination_reading, free_riders).
narrative_ontology:constraint_vindicates(hoa_covenant_scope__coordination_reading, shared_infrastructure_requires_coordination).
narrative_ontology:constraint_vindicates(hoa_covenant_scope__coordination_reading, externality_internalization_justifies_collective_rules).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own residential units within a community served by shared infrastructure (roads, water, sewers, common areas, stormwater management). Each homeowner benefits from maintained infrastructure and from rules that prevent free-riding on maintenance costs. Each also bears proportional maintenance assessments. They collectively set covenant terms through governance structures (HOA meetings, board elections). Exit consists of selling and moving — not leaving the community while retaining the home.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, all_homeowners, beneficiary,
    organized, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(hoa_covenant_scope__coordination_reading, all_homeowners, payer).

% Homeowners who would benefit from shared infrastructure without contributing proportionally to its maintenance if no enforcement mechanism existed. The covenant prevents this by requiring all residents to fund infrastructure at defined levels. Their 'cost' is the transfer from non-payment to mandated payment; exit is only through sale.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, free_riders, payer,
    powerless, immediate, trapped, local).

% Elected volunteers (homeowners themselves) who administer covenant enforcement strictly within the scope of infrastructure maintenance and externality prevention. They collect assessments, contract for repairs, and enforce rules against activities that damage shared property or create objective nuisances (neglected lawns affecting drainage, structural violations affecting load-bearing common walls, noise that damages neighboring property value). They have no independent economic interest in escalating enforcement beyond these functions.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, governing_board, agenda_setter,
    moderate, biographical, mobile, local).

% Provides baseline public infrastructure and zoning oversight. The covenant is a private supplement to public infrastructure, not a substitute for it. Municipal authorities can modify or void covenants that exceed legitimate collective governance bounds.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, municipality, observer,
    institutional, generational, analytical, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a genuine collective-action problem: shared infrastructure (roads, water, sewers, stormwater, common areas) cannot be maintained without coordinated funding and enforcement against free-riders. Individual homeowners cannot exit once in the community without selling, creating a commitment device for mutual resource pooling and preventing tragedy of the commons in infrastructure maintenance.
% TRANSFER_FUNCTION: Moves assessment contributions from each homeowner to a common fund used exclusively for infrastructure maintenance, repair, and replacement. The transfer is proportional (typically assessed per unit or square footage) and symmetrical across all homeowners — all benefit equally from the infrastructure, all pay equally to maintain it.
% ABSENT_VOICES: Future homeowners and non-resident stakeholders (renters, neighboring communities downstream of stormwater systems) have interests but limited voice in covenant design. From the coordination reading perspective, this reflects practical boundary-setting: the covenant governs residents with property rights in the community, not all affected parties. The absence is structural, not a manipulation.
% DISAPPEARANCE_RATIONALE: If the covenant and its enforcement vanished, homeowners would either collectively re-establish it (coordination problem unsolved without it) or fragment into non-maintained shared infrastructure, declining property values, and liability exposure. The coordination solution would reappear because the problem it solves is persistent.
% FOUNDING_PROBLEM: Residential developments with shared infrastructure (common roads, shared utilities, drainage systems serving multiple properties) face free-rider problems: individuals benefit from maintained infrastructure but have incentive to minimize personal contribution. Without coordination, infrastructure degrades. The covenant creates a binding commitment structure to solve this.
% FOUNDING_PROBLEM_CORROBORATION: Infrastructure engineers and municipal planners confirm that shared residential infrastructure requires ongoing coordinated maintenance or fails. Property economics studies document that neglected common infrastructure reduces all homeowner values equally. The coordination problem is real and persistent across jurisdictions and time periods — attested by non-benefiting sources (municipalities, academic research, property appraisers).
narrative_ontology:disappearance_verdict(hoa_covenant_scope__coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(hoa_covenant_scope__coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hoa_covenant_scope__coordination_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(hoa_covenant_scope__coordination_reading, 'none', 1).

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
 *   Extractiveness is low (0.16 at interval end) because beneficiaries and payers are the same set (all homeowners), and the transfer reflects actual infrastructure maintenance costs, not monopoly rent. Suppression is low (0.12) because the constraint requires minimal active coercion beyond cost accounting — homeowners rationally prefer maintained infrastructure to the alternative (degradation, liability, declining values). Theater is minimal (0.08) because enforcement activity tracks the actual coordination problem (preventing free-riding) and infrastructure needs; little performative maintenance is needed. Accessibility collapse is high (0.78) because exit from the community (the only way to escape the covenant) is costly and difficult — homeowners are locked into the territory by property ownership, making the constraint functionally unavoidable. Resistance is modest (0.22) because the constraint solves a real problem most homeowners recognize, but some resistance persists from free-riders and from those who dispute the scope boundary (is X enforcement a coordination necessity or overreach?). The measurement series is flat because this reading posits stable coordination function: extractiveness, suppression, and theater all remain low across the interval, reflecting a mature, functioning coordination mechanism. The flat trajectory supports the rope classification — not a snare accumulating extraction over time, not a piton where performative activity rises as function atrophies.
 *
 * PERSPECTIVAL GAP:
 *   The governing board and compliant homeowners should compute as beneficiaries with low directionality (d near 0.0); free_riders should compute as targets with high directionality (d near 1.0, since they are forced to pay). The engine derives d from beneficiary/victim declarations and exit options: all homeowners are declared beneficiaries (they all benefit from infrastructure); free_riders are a subset facing the highest exit cost (identity_locked to property, trapped in the community). This asymmetry in exit options produces divergent directionalities from the same base declarations — the structural feature the engine measures.
 *
 * DIRECTIONALITY LOGIC:
 *   Under the coordination reading, all homeowners are beneficiaries of the infrastructure maintenance function — they all receive the benefit of reliable roads, sewers, drainage, common areas. But free_riders are structurally distinguished by their exit options: they are trapped (cannot leave without selling, cannot stay without paying). This produces high directionality toward them (they bear the suppression of free-riding incentives). Non-free-riding homeowners have identical power (organized) and time horizon (generational) but constrained exit (cannot exit the covenant without selling, but selling is a real option). This produces moderate directionality — they pay, but they also benefit, and their exit is costly but available. The governing board sits at moderate power (volunteers, elected by peers) with mobile exit options (can step down), producing lower directionality — they administer the rule but are not its primary target. The symmetric beneficiary declaration reflects the coordination function: all homeowners benefit from the same infrastructure, all prefer it maintained to degraded. The asymmetry in directionality emerges from exit options and power, not from unequal benefit.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents misclassification of a genuine coordination constraint as extraction. Sibling readings (behavioral_control_reading and extraction_reading) claim the same covenant serves aesthetic conformity or revenue generation. If extractiveness were high (0.60+) and beneficiaries were a concentrated subset (board, long-term residents, property-flipping investors), the constraint would be snare or tangled_rope. But this reading's structural claim is that extractiveness is low because beneficiaries and payers align (all homeowners), enforcement scope is narrow (infrastructure + objective externality), and the alternative to coordination (degraded infrastructure, declining values, liability) is worse for all parties. The low measured extractiveness and high accessibility_collapse together support rope classification: high collapse reflects that homeowners are locked into the territory (cannot exit without selling), but low extractiveness reflects that they benefit from the constraint despite the lock-in. A snare would show high extractiveness (extraction exceeds coordination benefit); a mountain would show zero beneficiaries (no human agents benefit — it is natural law). This reading avoids both by declaring symmetric beneficiaries and low extractiveness.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_scope_boundary,
    'What constitutes a legitimate infrastructure maintenance or objective externality justifying covenant enforcement, versus overreach into behavioral control?',
    'Systematic analysis of enforcement actions across a sample of HOAs: what fraction targets infrastructure cost recovery vs. aesthetic/behavioral rules? Judicial review of covenant enforcement disputes and their outcomes. Homeowner surveys on perceived enforcement legitimacy by category.',
    'If enforcement consistently stays within infrastructure scope (80%+ of actions), the coordination reading holds. If enforcement spreads to aesthetic and behavioral rules (60%+ of enforcement actions), the constraint reclassifies toward tangled_rope or snare. The boundary is empirical, not stipulative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_scope_boundary, empirical, 'Whether HOA enforcement in practice remains within coordination scope or expands into behavioral control.').

omega_variable(
    alternative_coordination_mechanisms,
    'Is the covenant the only feasible mechanism to coordinate shared infrastructure maintenance, or could homeowners achieve equivalent coordination through property-law alternatives (mandatory servitudes, statutory liens, municipal utilities)?',
    'Comparative analysis of residential communities with and without covenants but with equivalent infrastructure; legal and economic analysis of alternative coordination mechanisms; historical evidence of covenant adoption in jurisdictions where alternatives existed.',
    'If covenants are the unique or overwhelmingly superior mechanism, the coordination reading is strengthened. If alternatives exist and achieve similar outcomes, the covenant''s necessity claim weakens, and the reading must acknowledge that other mechanisms (extraction mechanisms, control mechanisms) might achieve the same coordination more efficiently.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_coordination_mechanisms, conceptual, 'Whether covenants are structurally necessary for infrastructure coordination or contingent solutions.').

omega_variable(
    beneficiary_homogeneity,
    'Do all homeowners actually benefit equally from the covenant, or does benefit concentrate among long-term owners, early movers, or specific demographics?',
    'Empirical analysis of property value changes, assessment burden distribution, and enforcement patterns across homeowner cohorts (by entry year, property value, location within community). Survey of homeowner satisfaction with infrastructure maintenance and assessment fairness.',
    'If benefit is genuinely symmetrical across all homeowners, the rope classification holds. If benefit concentrates (e.g., long-term owners gain appreciation subsidized by transient renters, wealthy units use more common resources than assessed), the constraint reclassifies toward tangled_rope with non-uniform beneficiary distribution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_homogeneity, empirical, 'Whether declared beneficiaries (all homeowners) actually benefit symmetrically or whether benefit concentrates.').

omega_variable(
    kernel_reading_contest,
    'Does this constraint instantiate genuine infrastructure coordination, or does it function primarily to enforce behavioral conformity (behavioral_control_reading) or generate board revenue (extraction_reading)?',
    'The three readings have distinct ε-invariant structures: coordination_reading (low ε, symmetric beneficiaries), behavioral_control_reading (higher ε, concentrated beneficiaries of conformity, aesthetic rules), extraction_reading (highest ε, board/selective beneficiaries, fine proliferation). Empirical discrimination: audit enforcement actions by category (infrastructure vs. aesthetic vs. fine-generating), measure board authority expansion over time, compare ε by measurement basis (coordination metric vs. control metric vs. revenue metric). Sibling readings are separate constraint stories; this omega documents that the reading is contested.',
    'Classification depends critically on which reading is correct. Under coordination, rope. Under behavioral control, tangled_rope (asymmetric extraction of conformity labor). Under extraction, snare. The kernel contest cannot be resolved by stipulation; it requires empirical differentiation of the constraints.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'This constraint is one reading of a contested kernel; the sibling readings have different structural claims and classifications. The contest is not resolved here — it is named as an irreducible uncertainty.').


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
narrative_ontology:measurement(hoa__tr_t5, hoa_covenant_scope__coordination_reading, theater_ratio, 5, 0.06).
narrative_ontology:measurement_basis(hoa__tr_t5, observed).
narrative_ontology:measurement(hoa__tr_t10, hoa_covenant_scope__coordination_reading, theater_ratio, 10, 0.07).
narrative_ontology:measurement_basis(hoa__tr_t10, observed).
narrative_ontology:measurement(hoa__tr_t20, hoa_covenant_scope__coordination_reading, theater_ratio, 20, 0.08).
narrative_ontology:measurement_basis(hoa__tr_t20, observed).
narrative_ontology:measurement(hoa__tr_t30, hoa_covenant_scope__coordination_reading, theater_ratio, 30, 0.08).
narrative_ontology:measurement_basis(hoa__tr_t30, observed).
narrative_ontology:measurement(hoa__tr_t40, hoa_covenant_scope__coordination_reading, theater_ratio, 40, 0.08).
narrative_ontology:measurement_basis(hoa__tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(hoa__be_t0, hoa_covenant_scope__coordination_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement_basis(hoa__be_t0, observed).
narrative_ontology:measurement(hoa__be_t5, hoa_covenant_scope__coordination_reading, base_extractiveness, 5, 0.13).
narrative_ontology:measurement_basis(hoa__be_t5, observed).
narrative_ontology:measurement(hoa__be_t10, hoa_covenant_scope__coordination_reading, base_extractiveness, 10, 0.14).
narrative_ontology:measurement_basis(hoa__be_t10, observed).
narrative_ontology:measurement(hoa__be_t20, hoa_covenant_scope__coordination_reading, base_extractiveness, 20, 0.15).
narrative_ontology:measurement_basis(hoa__be_t20, observed).
narrative_ontology:measurement(hoa__be_t30, hoa_covenant_scope__coordination_reading, base_extractiveness, 30, 0.16).
narrative_ontology:measurement_basis(hoa__be_t30, observed).
narrative_ontology:measurement(hoa__be_t40, hoa_covenant_scope__coordination_reading, base_extractiveness, 40, 0.16).
narrative_ontology:measurement_basis(hoa__be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(hoa__su_t0, hoa_covenant_scope__coordination_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement_basis(hoa__su_t0, observed).
narrative_ontology:measurement(hoa__su_t5, hoa_covenant_scope__coordination_reading, suppression_requirement, 5, 0.1).
narrative_ontology:measurement_basis(hoa__su_t5, observed).
narrative_ontology:measurement(hoa__su_t10, hoa_covenant_scope__coordination_reading, suppression_requirement, 10, 0.11).
narrative_ontology:measurement_basis(hoa__su_t10, observed).
narrative_ontology:measurement(hoa__su_t20, hoa_covenant_scope__coordination_reading, suppression_requirement, 20, 0.12).
narrative_ontology:measurement_basis(hoa__su_t20, observed).
narrative_ontology:measurement(hoa__su_t30, hoa_covenant_scope__coordination_reading, suppression_requirement, 30, 0.12).
narrative_ontology:measurement_basis(hoa__su_t30, observed).
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
% The HOA covenant kernel is a contested commitment structure instantiated differently under three readings: coordination_reading (low ε, infrastructure maintenance), behavioral_control_reading (higher ε, aesthetic conformity enforcement), extraction_reading (highest ε, board revenue and power consolidation). Each reading is a separate constraint story with its own ε, beneficiary/victim structure, enforcement scope, and classification. The three stories form a constraint family linked by network.affects_constraints because they are readings of the same institutional practice (HOA covenants) but decomposed according to ε-invariance: the measurement basis (coordination function vs. control function vs. revenue generation) changes the observable constraint. The readings coexist in the real world — different homeowners and boards emphasize different readings, leading to institutional variance. This file instantiates the coordination reading only. Sibling readings appear in separate JSON files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
