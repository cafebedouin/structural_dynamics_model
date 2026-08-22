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
 *   human_readable: HOA Covenant as Shared Infrastructure Coordination
 *   domain: property_law/collective_governance
 *
 * SUMMARY:
 *   A homeowners association covenant exists in a residential development
 *   where shared infrastructure (roads, drainage, recreation areas) creates
 *   genuine externalities across individual property lines. This reading
 *   frames the covenant as a coordination mechanism that solves a real
 *   collective-action problem: properties cannot be maintained in isolation
 *   without cascading failures in shared systems. The covenant establishes
 *   symmetric cost-sharing and enforcement limited to infrastructure
 *   maintenance obligations. This is ONE reading of a contested kernel
 *   (hoa_covenant_scope); sibling readings frame the same covenant as
 *   behavioral control (aesthetic uniformity enforcement) or extraction
 *   (fines and board power consolidation). The engine computes per-seat
 *   classifications; this story authors the structural data for the
 *   coordination reading only.
 *
 * KEY AGENTS:
 *   - all_homeowners: symmetric beneficiaries and contributors (organized power, constrained exit)
 *   - hoa_board: agenda-setter that administers assessment collection and maintenance scheduling (institutional power, mobile exit)
 *   - free_riders: powerless homeowners who benefit from shared infrastructure but defer maintenance or assessment payment (trapped exit)
 *   - prospective_residents: excluded from HOA governance but can choose entry before purchase (moderate power, mobile exit)
 *   - infrastructure_analyst: observer seat measuring whether enforcement stays narrowly infrastructure-focused or drifts into behavioral control
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hoa_covenant_scope__coordination_reading, 0.12).
domain_priors:suppression_score(hoa_covenant_scope__coordination_reading, 0.15).
domain_priors:theater_ratio(hoa_covenant_scope__coordination_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, resistance, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hoa_covenant_scope__coordination_reading, rope).
narrative_ontology:human_readable(hoa_covenant_scope__coordination_reading, "HOA Covenant as Shared Infrastructure Coordination").
narrative_ontology:topic_domain(hoa_covenant_scope__coordination_reading, "property_law/collective_governance").

domain_priors:requires_active_enforcement(hoa_covenant_scope__coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hoa_covenant_scope__coordination_reading, '8c1438b6-49df-4935-846e-4daacd3aa047').
narrative_ontology:cs_kernel_codification('8c1438b6-49df-4935-846e-4daacd3aa047', formalized).
narrative_ontology:cs_authority_grounding('8c1438b6-49df-4935-846e-4daacd3aa047', practice).
narrative_ontology:cs_interpretation_layer_present('8c1438b6-49df-4935-846e-4daacd3aa047').
narrative_ontology:cs_reading_relation('8c1438b6-49df-4935-846e-4daacd3aa047', hoa_covenant_scope__behavioral_control_reading, coexists_with).
narrative_ontology:cs_reading_relation('8c1438b6-49df-4935-846e-4daacd3aa047', hoa_covenant_scope__extraction_reading, influences).
narrative_ontology:cs_axiom('8c1438b6-49df-4935-846e-4daacd3aa047', foundational, shared_externalities_justify_coordination).
narrative_ontology:cs_axiom_status(shared_externalities_justify_coordination, holdable).
narrative_ontology:cs_axiom_grounding('8c1438b6-49df-4935-846e-4daacd3aa047', shared_externalities_justify_coordination, empirically_contingent).
narrative_ontology:cs_axiom('8c1438b6-49df-4935-846e-4daacd3aa047', foundational, enforcement_limited_to_infrastructure_cost_recovery).
narrative_ontology:cs_axiom_status(enforcement_limited_to_infrastructure_cost_recovery, holdable).
narrative_ontology:cs_axiom_grounding('8c1438b6-49df-4935-846e-4daacd3aa047', enforcement_limited_to_infrastructure_cost_recovery, conventional).
narrative_ontology:cs_reference_frame('8c1438b6-49df-4935-846e-4daacd3aa047', shared_infrastructure_coordination).
narrative_ontology:cs_drift_state('8c1438b6-49df-4935-846e-4daacd3aa047', contemporary, gap(stable, minor, false)).
narrative_ontology:cs_created_at('8c1438b6-49df-4935-846e-4daacd3aa047', '').
narrative_ontology:cs_kernel_id(hoa_covenant_scope__coordination_reading, hoa_covenant_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__coordination_reading, all_homeowners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(hoa_covenant_scope__coordination_reading, free_riders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own individual parcels within a shared development where roads, drainage, recreation areas, and common utilities depend on coordinated maintenance funded and scheduled through the covenant. They benefit from the shared infrastructure being preserved and maintained without individual negotiation. They collectively set covenant policy through the HOA board and member voting, though individual homeowners cannot unilaterally exit the arrangement without selling their property.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, all_homeowners, beneficiary,
    organized, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(hoa_covenant_scope__coordination_reading, all_homeowners, agenda_setter).

% Administers the covenant rules, collects assessments, schedules maintenance, and enforces compliance with infrastructure-related obligations (e.g., maintaining roofs and drainage that affect shared systems, not repainting to match neighbors). Board members are elected homeowners; they have operational authority to implement infrastructure coordination but no independent revenue stream — all revenue comes from the homeowner assessment pool.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, hoa_board, agenda_setter,
    institutional, biographical, mobile, local).

% Homeowners who benefit from shared infrastructure maintenance (roads, drainage, recreational amenities) but neglect to fund their fair share through assessments or defer required maintenance on property elements that are structural externalities (e.g., a failing roof that cascades water onto adjacent properties, or deferred roof maintenance that affects common drainage). They are trapped: they cannot benefit-shift (the infrastructure's service crosses property lines) and cannot exit without selling, but noncompliance extracts a cost on other homeowners.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, free_riders, payer,
    powerless, biographical, trapped, local).

% Potential purchasers who choose whether to enter the covenant community. They can walk away before purchase if they reject the arrangement, but once they own property the exit becomes constrained. They are excluded from the current homeowner decision-making structure despite being subject to the terms if they buy.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, prospective_residents, excluded,
    moderate, immediate, mobile, local).

% External assessor of whether the covenant's rule set addresses genuine shared maintenance problems or has drifted into behavioral policing. Examines cost structures, complaint patterns, and outcome measures to distinguish coordination-focused enforcement from extraction-focused enforcement.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, infrastructure_analyst, observer,
    analytical, generational, analytical, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hoa_covenant_scope__coordination_reading, diffuse).
narrative_ontology:fixing_cost_class(hoa_covenant_scope__coordination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a genuine collective-action problem: properties in a shared development have externalities (shared roads, drainage systems, recreational facilities, storm water runoff patterns) that require coordinated maintenance funding and scheduling. Individual property owners cannot solve this through bilateral negotiation — the spatial interdependence is many-to-many. The covenant establishes a mechanism to fund and schedule this maintenance proportionally, ensuring no homeowner can free-ride on others' upkeep while avoiding their share of costs.
% TRANSFER_FUNCTION: Moves maintenance obligations and funding from individual choice to collective decision. Each homeowner contributes to a common assessment pool; the HOA allocates these funds to scheduled maintenance of infrastructure that benefits all parties (roads, drainage, common areas). Transfer is symmetric: every homeowner contributes and every homeowner benefits from the same infrastructure.
% ABSENT_VOICES: Prospective residents can examine the covenant before purchase but have no voice in its design or amendment. Once they own, their exit becomes constrained. Tenants renting within the community are structurally absent — they live under covenant rules but have no voting power in the HOA. Non-resident property investors (who own but do not occupy) have formal voting rights but often limited time-horizon commitment to the community.
% DISAPPEARANCE_RATIONALE: If the covenant enforcement vanished, the shared infrastructure would degrade without coordinated funding: roads would deteriorate without collective assessment, drainage systems would fail from deferred maintenance on individual properties, recreational amenities would go unmaintained. Individual homeowners would face increasing disputes over responsibility for cascading damages (roof failures, water intrusion). Some homeowners would seek alternative governance structures; others would lose property value as infrastructure eroded. The development's functional integrity depends on the arrangement.
% FOUNDING_PROBLEM: Properties were developed as a shared-infrastructure community where roads, drainage systems, and recreational facilities are legally shared or create unavoidable externalities across property lines. Early occupants faced the problem of funding and scheduling maintenance on infrastructure no single property owner could control alone. Bilateral negotiation proved unworkable — each owner could defer costly upkeep while expecting others to fund it, and infrastructure failure cascaded across properties.
% FOUNDING_PROBLEM_CORROBORATION: Structural engineers and drainage specialists attest that residential developments with shared infrastructure require coordinated maintenance or fail progressively. HOA case studies from well-maintained communities (low delinquency, transparent budgets) show the founding problem remains live. From outside the board beneficiary position: municipal engineers, insurance actuaries, and property law scholars confirm that covenant-free shared-infrastructure communities experience significantly higher maintenance failure rates and dispute costs.
narrative_ontology:disappearance_verdict(hoa_covenant_scope__coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(hoa_covenant_scope__coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hoa_covenant_scope__coordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hoa_covenant_scope__coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hoa_covenant_scope__coordination_reading, 0.12, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is low (0.12) because under this reading the covenant operates symmetrically: every homeowner pays roughly proportional to infrastructure benefit and receives the same benefit in return. No party collects unearned rent; the assessment pool is cycled back into shared maintenance, not accumulated by a beneficiary. Suppression is low (0.15) because enforcement is narrow and objective — it targets specific failure-to-maintain behaviors (deferred roof repairs affecting drainage, unpaid assessments) rather than aesthetic choices. Theater is very low (0.08) because enforcement activity maps directly to infrastructure cost recovery; there is minimal performative aspect. The measurement series remain flat because under this reading the constraint operates at steady state: the founding problem stays live, enforcement stays focused, and extraction stays minimal. The slight upward drift (0.10 to 0.12 over 40 years) reflects minor inflation in administrative overhead and dispute resolution costs as the community ages, not function drift.
 *
 * PERSPECTIVAL GAP:
 *   The payer-of-free-rider-enforcement and the beneficiary-of-shared-infrastructure seats compute the same constraint differently: a free rider experiences enforcement as suppression (cost extraction for service they believe they should receive freely); a compliant homeowner experiences enforcement as fair maintenance of a reciprocal agreement. The engine computes this per-seat; this story authors the structural data without prejudging the seats' perceptions. From the infrastructure analyst seat (observer), the question is whether enforcement scope stays narrow (infrastructure cost recovery) or drifts wider (behavioral policing, selective application to disfavored residents). This reading asserts the narrow scope; sibling readings dispute it.
 *
 * DIRECTIONALITY LOGIC:
 *   All homeowners are both contributors (they pay assessments) and beneficiaries (they receive infrastructure services). Their directionality d computes near-symmetric (d ≈ 0.5) under the coordination reading because costs and benefits are paired and proportional. Free riders have higher d (closer to 1.0) because they extract infrastructure service without full cost contribution — suppression toward them (enforcement of assessment payment) is legitimate cost-recovery, not extraction per se. The HOA board has power to set rates but no independent revenue stream; they are institutional but not beneficiaries (they collect nothing beyond reimbursement for administrative salary, if any). From the board's seat the constraint is enforced coordination; from a free-rider's seat it is mild suppression; from a symmetric homeowner's seat it is beneficial reciprocity.
 *
 * MANDATROPHY ANALYSIS:
 *   Under the coordination reading, mandatrophy does not apply: the founding problem (coordinating shared infrastructure maintenance) remains live, the covenant still solves it, and enforcement remains narrowly focused on that function. The constraint would be classified as mandatrophy-resolved only if the founding problem had become dead (e.g., if infrastructure were municipalized or fully privatized, eliminating shared externalities) while the covenant persisted for other purposes. This reading denies that condition holds.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_scope_drift,
    'Does HOA enforcement remain narrowly scoped to infrastructure cost recovery and objective nuisance (supporting the coordination reading), or has it drifted into aesthetic control and behavioral policing (supporting the behavioral_control or extraction readings)?',
    'Audit of enforcement actions over a decade: categorize each fine or enforcement action by target (infrastructure-related vs. aesthetic/behavioral). Compute the ratio of infrastructure-focused enforcement to total enforcement. Survey homeowners on whether enforcement feels proportional and infrastructure-focused or selective and intrusive.',
    'If infrastructure enforcement dominates (>75% of enforcement actions target maintenance obligations or cost recovery), the coordination reading is structurally supported and extraction remains low. If behavioral/aesthetic enforcement dominates, the constraint drifts toward the behavioral_control or extraction reading, and ε should be re-authored upward.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_scope_drift, empirical, 'Does enforcement scope track infrastructure maintenance (coordination) or has it broadened to behavioral control?').

omega_variable(
    assessment_proportionality,
    'Are homeowner assessments proportional to the infrastructure benefits they receive, or are they structured to concentrate gains on particular community segments (e.g., corner lots pay more for road frontage they don''t use; renters pay but receive no governance voice)?',
    'Comparative analysis of assessment structures in peer HOAs. Survey homeowners on whether assessment rates feel fair relative to their property''s contribution to and benefit from shared infrastructure. Audit budget allocation: does it track stated infrastructure maintenance priorities or does it fund selectively benefiting amenities (e.g., pool favoring affluent areas, decorative landscaping in entry lots).',
    'If assessments are proportional and budgets track infrastructure need, the constraint remains symmetric (all_homeowners as symmetric beneficiary). If assessments subsidize or concentrate benefits, the constraint edges toward extraction (some homeowners become covert victims), and ε rises to 0.25+.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(assessment_proportionality, empirical, 'Whether assessment structure maintains symmetric cost-sharing or creates hidden extraction.').

omega_variable(
    interpretive_frame_contestation,
    'Which reading (coordination, behavioral_control, extraction) legitimately captures the HOA covenant''s true function: addressing shared infrastructure needs, enforcing neighborhood aesthetics, or generating board revenue?',
    'This is not empirically resolvable in a way that settles the question for all stakeholders. Different seats (homeowners, board, prospective residents) will emphasize different functions. The resolution mechanism is intra-reading consistency: for the coordination reading to hold, enforcement must stay narrowly scoped and symmetric; for the behavioral_control reading to hold, aesthetic rules must be actively enforced; for the extraction reading to hold, fines and selective enforcement must generate measurable revenue or board control. Each reading has internal coherence conditions that can be audited. The question itself (which reading is ''true'') is a preference question, not an empirical one.',
    'If the intra-reading consistency tests show the coordination reading''s enforcement scope is maintained while the other readings would require drift the data does not support, the coordination reading remains the better-grounded account. If data shows broad enforcement scope and revenue generation, the extraction reading gains structural support despite board denials.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(interpretive_frame_contestation, conceptual, 'Which interpretive frame of the covenant kernel is the legitimate reading?').

omega_variable(
    exit_cost_asymmetry,
    'Are the real exit costs for homeowners from the covenant symmetric (all homeowners face equal constraints), or do they differ by property type, income, or tenure?',
    'Analyze property sale data: do homeowners in covenant communities experience different price premiums/discounts relative to non-covenant comparable properties by property type or location? Survey homeowners on their exit deliberation: what proportion cite covenant constraints as a reason they considered selling but did not? Measure switching costs (realtor fees, transaction costs) against property value changes correlated with covenant status.',
    'If exit costs are asymmetric (e.g., lower-income homeowners face larger proportional transaction costs relative to property value, or certain property types are unmortgageable due to HOA liability), their directionality d shifts higher (more target-like) and suppression is higher, even under the coordination reading. Symmetric exit costs support the symmetric beneficiary framing; asymmetric costs suggest hidden victimhood.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_cost_asymmetry, empirical, 'Are exit costs from the covenant symmetric across all homeowners, or concentrated on particular groups?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hoa_covenant_scope__coordination_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hoa__tr_t0, hoa_covenant_scope__coordination_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(hoa__tr_t0, observed).
narrative_ontology:measurement(hoa__tr_t5, hoa_covenant_scope__coordination_reading, theater_ratio, 5, 0.06).
narrative_ontology:measurement_basis(hoa__tr_t5, observed).
narrative_ontology:measurement(hoa__tr_t10, hoa_covenant_scope__coordination_reading, theater_ratio, 10, 0.07).
narrative_ontology:measurement_basis(hoa__tr_t10, observed).
narrative_ontology:measurement(hoa__tr_t15, hoa_covenant_scope__coordination_reading, theater_ratio, 15, 0.08).
narrative_ontology:measurement_basis(hoa__tr_t15, observed).
narrative_ontology:measurement(hoa__tr_t20, hoa_covenant_scope__coordination_reading, theater_ratio, 20, 0.08).
narrative_ontology:measurement_basis(hoa__tr_t20, observed).
narrative_ontology:measurement(hoa__tr_t40, hoa_covenant_scope__coordination_reading, theater_ratio, 40, 0.08).
narrative_ontology:measurement_basis(hoa__tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(hoa__be_t0, hoa_covenant_scope__coordination_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement_basis(hoa__be_t0, observed).
narrative_ontology:measurement(hoa__be_t5, hoa_covenant_scope__coordination_reading, base_extractiveness, 5, 0.1).
narrative_ontology:measurement_basis(hoa__be_t5, observed).
narrative_ontology:measurement(hoa__be_t10, hoa_covenant_scope__coordination_reading, base_extractiveness, 10, 0.11).
narrative_ontology:measurement_basis(hoa__be_t10, observed).
narrative_ontology:measurement(hoa__be_t15, hoa_covenant_scope__coordination_reading, base_extractiveness, 15, 0.12).
narrative_ontology:measurement_basis(hoa__be_t15, observed).
narrative_ontology:measurement(hoa__be_t20, hoa_covenant_scope__coordination_reading, base_extractiveness, 20, 0.12).
narrative_ontology:measurement_basis(hoa__be_t20, observed).
narrative_ontology:measurement(hoa__be_t40, hoa_covenant_scope__coordination_reading, base_extractiveness, 40, 0.12).
narrative_ontology:measurement_basis(hoa__be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(hoa__su_t0, hoa_covenant_scope__coordination_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement_basis(hoa__su_t0, observed).
narrative_ontology:measurement(hoa__su_t5, hoa_covenant_scope__coordination_reading, suppression_requirement, 5, 0.13).
narrative_ontology:measurement_basis(hoa__su_t5, observed).
narrative_ontology:measurement(hoa__su_t10, hoa_covenant_scope__coordination_reading, suppression_requirement, 10, 0.14).
narrative_ontology:measurement_basis(hoa__su_t10, observed).
narrative_ontology:measurement(hoa__su_t15, hoa_covenant_scope__coordination_reading, suppression_requirement, 15, 0.15).
narrative_ontology:measurement_basis(hoa__su_t15, observed).
narrative_ontology:measurement(hoa__su_t20, hoa_covenant_scope__coordination_reading, suppression_requirement, 20, 0.15).
narrative_ontology:measurement_basis(hoa__su_t20, observed).
narrative_ontology:measurement(hoa__su_t40, hoa_covenant_scope__coordination_reading, suppression_requirement, 40, 0.15).
narrative_ontology:measurement_basis(hoa__su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hoa_covenant_scope__coordination_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(hoa_covenant_scope__coordination_reading, 0.12).
narrative_ontology:affects_constraint(hoa_covenant_scope__coordination_reading, hoa_covenant_scope__behavioral_control_reading).
narrative_ontology:affects_constraint(hoa_covenant_scope__coordination_reading, hoa_covenant_scope__extraction_reading).

% DUAL FORMULATION NOTE:
% The hoa_covenant_scope kernel has three competing readings: coordination_reading (this file) interprets the covenant as solving genuine shared-infrastructure externalities with symmetric cost-sharing and narrow enforcement scope (low ε, Rope); behavioral_control_reading interprets the same kernel as enforcing aesthetic uniformity and property-value signaling (moderate ε, Tangled Rope); extraction_reading interprets the same kernel as enabling board revenue generation and power consolidation (high ε, Snare). All three readings appeal to the same founding legal text and institutional structure. The readings diverge in which enforcement patterns they normalize and which stakeholder relationships they emphasize. Each reading is one constraint story with its own ε, beneficiary structure, enforcement scope, and classification. Sibling stories share the network link bidirectionally.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
