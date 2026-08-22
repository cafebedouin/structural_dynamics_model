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
 *   human_readable: HOA Covenant as Shared Infrastructure Coordination (Coordination Reading)
 *   domain: property_law/collective_governance/urban_planning
 *
 * SUMMARY:
 *   This story instantiates the coordination reading of the
 *   hoa_covenant_scope kernel: the covenant is read as an infrastructure
 *   cost-sharing and externality-resolution mechanism, narrowly scoped to
 *   shared road maintenance, drainage, and objective nuisance abatement
 *   (structural hazards, unrepaired damage affecting neighbors). Under this
 *   reading the covenant solves a genuine collective-action problem — shared
 *   infrastructure that individual owners cannot economically maintain alone
 *   and would under-fund absent a binding mechanism. This is NOT the same
 *   constraint as the behavioral_control_reading (aesthetic/behavioral
 *   conformity enforcement) or the extraction_reading (revenue generation via
 *   fine proliferation) — those are sibling constraints authored separately
 *   with their own ε, beneficiaries, and victims, sharing only the kernel
 *   text (the covenant document) and the authority structure (the board) that
 *   different parties read differently.
 *
 * KEY AGENTS:
 *   - all_homeowners: symmetric beneficiary/payer (moderate/constrained) — funds and receives shared infrastructure upkeep
 *   - persistent_free_riders: narrow victim class (moderate/constrained) — the enforcement target under this reading, not a suppressed dissenting voice
 *   - hoa_board: agenda_setter (organized/mobile) — administers cost-recovery, itself subject to the same assessments
 *   - future_buyers: beneficiary (powerless/mobile) — inherits maintained infrastructure, enters with disclosure
 *   - municipal_government: observer (institutional/analytical) — externality this arrangement absorbs on the municipality's behalf
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hoa_covenant_scope__coordination_reading, 0.16).
domain_priors:suppression_score(hoa_covenant_scope__coordination_reading, 0.22).
domain_priors:theater_ratio(hoa_covenant_scope__coordination_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, extractiveness, 0.16).
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hoa_covenant_scope__coordination_reading, rope).
narrative_ontology:human_readable(hoa_covenant_scope__coordination_reading, "HOA Covenant as Shared Infrastructure Coordination (Coordination Reading)").
narrative_ontology:topic_domain(hoa_covenant_scope__coordination_reading, "property_law/collective_governance/urban_planning").

domain_priors:requires_active_enforcement(hoa_covenant_scope__coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hoa_covenant_scope__coordination_reading, 'a487ce9a-00bc-4da7-9eee-75268c0fc034').
narrative_ontology:cs_kernel_codification('a487ce9a-00bc-4da7-9eee-75268c0fc034', formalized).
narrative_ontology:cs_authority_grounding('a487ce9a-00bc-4da7-9eee-75268c0fc034', practice).
narrative_ontology:cs_interpretation_layer_present('a487ce9a-00bc-4da7-9eee-75268c0fc034').
narrative_ontology:cs_reading_relation('a487ce9a-00bc-4da7-9eee-75268c0fc034', hoa_covenant_scope__behavioral_control_reading, coexists_with).
narrative_ontology:cs_reading_relation('a487ce9a-00bc-4da7-9eee-75268c0fc034', hoa_covenant_scope__extraction_reading, influences).
narrative_ontology:cs_axiom('a487ce9a-00bc-4da7-9eee-75268c0fc034', foundational, enforcement_authority_bounded_by_objective_externality).
narrative_ontology:cs_axiom_status(enforcement_authority_bounded_by_objective_externality, holdable).
narrative_ontology:cs_axiom_grounding('a487ce9a-00bc-4da7-9eee-75268c0fc034', enforcement_authority_bounded_by_objective_externality, conventional).
narrative_ontology:cs_axiom('a487ce9a-00bc-4da7-9eee-75268c0fc034', secondary, assessment_obligation_tracks_actual_infrastructure_cost).
narrative_ontology:cs_axiom_status(assessment_obligation_tracks_actual_infrastructure_cost, holdable).
narrative_ontology:cs_axiom_grounding('a487ce9a-00bc-4da7-9eee-75268c0fc034', assessment_obligation_tracks_actual_infrastructure_cost, empirically_contingent).
narrative_ontology:cs_reference_frame('a487ce9a-00bc-4da7-9eee-75268c0fc034', narrow_cost_recovery_and_objective_nuisance_mandate).
narrative_ontology:cs_drift_state('a487ce9a-00bc-4da7-9eee-75268c0fc034', contemporary_hoa_governance, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('a487ce9a-00bc-4da7-9eee-75268c0fc034', '').
narrative_ontology:cs_kernel_id(hoa_covenant_scope__coordination_reading, hoa_covenant_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__coordination_reading, all_homeowners).
narrative_ontology:constraint_victim(hoa_covenant_scope__coordination_reading, persistent_free_riders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__coordination_reading, future_buyers).
narrative_ontology:constraint_victim(hoa_covenant_scope__coordination_reading, all_homeowners).
narrative_ontology:constraint_vindicates(hoa_covenant_scope__coordination_reading, collective_action_problem_requires_binding_mechanism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own property inside the association's boundary and pay assessments that fund shared road, drainage, and common-area upkeep no single owner could economically maintain alone. Each receives the benefit of maintained infrastructure and a functioning drainage system whether or not they personally value the aesthetic rules layered on top. Selling means disclosing the covenant to a buyer who accepts the same bargain.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, all_homeowners, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(hoa_covenant_scope__coordination_reading, all_homeowners, payer).

% A small subset of owners who decline to pay assessments while still using the shared road, storm drainage, and common areas funded by everyone else. The covenant's lien and assessment-collection machinery exists specifically to prevent their non-payment from degrading the shared infrastructure for the paying majority.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, persistent_free_riders, payer,
    moderate, biographical, constrained, local).

% Elected from and by the homeowner pool, administers the maintenance budget, collects assessments, and enforces the narrow set of rules tied to infrastructure cost-sharing and objective nuisance (unrepaired drainage damage, structural safety hazards). Board members are themselves homeowners subject to the same assessments; they hold no revenue stream beyond what funds the shared systems.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, hoa_board, agenda_setter,
    organized, biographical, mobile, local).

% Prospective purchasers who inherit the benefit of well-maintained shared infrastructure and a covenant that continues to prevent free-riding after they buy in. They choose to enter the arrangement with full disclosure at purchase and can decline the purchase if the terms are unacceptable.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, future_buyers, beneficiary,
    powerless, generational, mobile, local).

% Would otherwise bear responsibility for private road maintenance and drainage infrastructure that the covenant privately funds and manages instead, reducing the externality the municipality would otherwise have to internalize or ignore.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, municipal_government, observer,
    institutional, generational, analytical, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hoa_covenant_scope__coordination_reading, diffuse).
narrative_ontology:fixing_cost_class(hoa_covenant_scope__coordination_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a genuine collective-action problem: shared roads, storm drainage, and common-area infrastructure are jointly used but individually unmaintainable, and without a binding cost-sharing mechanism the incentive to under-invest or free-ride would degrade the infrastructure for everyone.
% TRANSFER_FUNCTION: Moves assessment payments from all homeowners into a shared maintenance fund, and moves the cost of infrastructure upkeep away from individual owners' idiosyncratic willingness to pay toward a fixed, predictable, universally-applied obligation.
% ABSENT_VOICES: Persistent free riders would object to being compelled to pay for infrastructure they'd prefer to use without funding, but their objection is precisely the collective-action failure the covenant exists to foreclose, not a legitimate alternative use case being suppressed.
% DISAPPEARANCE_RATIONALE: If the covenant's cost-recovery and enforcement mechanism vanished, shared roads and drainage would begin degrading within a few maintenance cycles as some owners under-contribute; either the municipality would need to absorb the infrastructure or a new collection mechanism would need to be built from scratch.
% FOUNDING_PROBLEM: Newly platted subdivisions with private roads, shared drainage, and common areas had no mechanism to fund upkeep once the developer withdrew, and voluntary contribution schemes reliably under-fund shared infrastructure due to free-rider incentives.
% FOUNDING_PROBLEM_CORROBORATION: Municipal planning departments and civil engineers external to the association attest that privately-maintained shared infrastructure (roads, stormwater systems) requires a binding cost-recovery mechanism or reliably degrades; this is corroborated independently of anything the HOA board or homeowners assert about their own arrangement.
narrative_ontology:disappearance_verdict(hoa_covenant_scope__coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(hoa_covenant_scope__coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hoa_covenant_scope__coordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hoa_covenant_scope__coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hoa_covenant_scope__coordination_reading, 0.16, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored low (0.16 at interval end) because, under this reading's own lights, assessments track actual infrastructure cost and enforcement targets objective nonpayment and objective nuisance, not subjective aesthetic judgment. Suppression is modest (0.22) — assessment liens are real coercive machinery, but they operate against a narrow, objectively verifiable failure (nonpayment, unrepaired hazard) rather than against a broad discretionary rule set. Theater ratio stays low (0.10) because almost all board activity under this reading is functional cost administration, not performative rule enforcement. The metrics reflect the coordination reading's own account of the covenant's operation; the sibling readings author their own metrics for the same kernel text.
 *
 * DIRECTIONALITY LOGIC:
 *   All_homeowners sit near-symmetric: they pay assessments and receive the maintained infrastructure in return, so directionality centers close to 0.5 with a slight beneficiary lean given the coordination function is real. Persistent_free_riders are the narrow target class — d rises toward the target end specifically because the enforcement machinery (liens, assessment collection) exists to correct their under-contribution, not because the covenant targets them arbitrarily. Future_buyers derive benefit with full ex-ante disclosure, keeping their d low. The board's exit option is mobile (they can decline re-election or move) rather than institutional-arbitrage, since under this reading the board captures no independent rent stream.
 *
 * MANDATROPHY ANALYSIS:
 *   The coordination reading resists mandatrophy precisely because its founding problem (private infrastructure funding) remains verifiably live — municipal engineers and planning departments external to the association corroborate that privately-held shared infrastructure degrades without a binding cost-recovery mechanism. This is what distinguishes the coordination reading from a piton or snare reading of the same kernel: if the founding problem were dead but enforcement persisted, that would indicate drift toward the extraction_reading's territory, which this story does not claim to describe.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_scope_boundary_ambiguity,
    'Where does the covenant''s genuine coordination function (infrastructure cost-sharing, objective nuisance) end and its discretionary behavioral/revenue function begin, within the same enforcement apparatus?',
    'Audit the association''s enforcement log: classify each fine/lien action as (a) unpaid assessment, (b) objective structural hazard, or (c) discretionary aesthetic/behavioral violation. The ratio of (a)+(b) to (c) over time indicates which reading better describes actual practice.',
    'If the enforcement log is dominated by category (c), the coordination reading is descriptively thin and the behavioral_control_reading or extraction_reading better describes the association''s actual operation, even though this story''s own ε remains authored against its own narrow referent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_scope_boundary_ambiguity, empirical, 'Whether real enforcement activity stays within the coordination reading''s claimed narrow scope.').

omega_variable(
    free_rider_victim_classification,
    'Are persistent_free_riders genuine victims of extraction, or is their designation as ''victim'' here simply the correct label for an enforcement target under a legitimate collective-action remedy?',
    'Compare assessment amounts against documented per-unit infrastructure cost; if assessments track cost closely, free-rider enforcement is coordination-correcting rather than extractive.',
    'If assessments substantially exceed documented infrastructure cost even for compliant owners, the ''free rider'' framing may be cover for the same extraction the extraction_reading names directly, undermining the coordination reading''s distinctiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(free_rider_victim_classification, conceptual, 'Whether the free-rider victim class is genuinely narrow or a rhetorical minimization of broader extraction.').

omega_variable(
    reading_selection_criterion,
    'What determines which of the three readings (coordination, behavioral_control, extraction) is the operative one for a given HOA at a given time, given they share the same kernel text and authority structure?',
    'Track board composition turnover, fine revenue as a share of total assessment revenue, and rule amendment history — associations drifting toward extraction typically show fine revenue growth disproportionate to assessment growth.',
    'This omega is the committer-structure question itself: it does not change this story''s own ε, but documents that the three readings are not merely interpretive choices but track observably different institutional trajectories of the same underlying document.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_selection_criterion, conceptual, 'What structural signal distinguishes which reading is empirically operative for a given association.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hoa_covenant_scope__coordination_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hoa__tr_t0, hoa_covenant_scope__coordination_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(hoa__tr_t4, hoa_covenant_scope__coordination_reading, theater_ratio, 4, 0.08).
narrative_ontology:measurement(hoa__tr_t8, hoa_covenant_scope__coordination_reading, theater_ratio, 8, 0.09).
narrative_ontology:measurement(hoa__tr_t12, hoa_covenant_scope__coordination_reading, theater_ratio, 12, 0.09).
narrative_ontology:measurement(hoa__tr_t16, hoa_covenant_scope__coordination_reading, theater_ratio, 16, 0.1).
narrative_ontology:measurement(hoa__tr_t20, hoa_covenant_scope__coordination_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(hoa__tr_t24, hoa_covenant_scope__coordination_reading, theater_ratio, 24, 0.1).

% Extraction over time
narrative_ontology:measurement(hoa__be_t0, hoa_covenant_scope__coordination_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(hoa__be_t4, hoa_covenant_scope__coordination_reading, base_extractiveness, 4, 0.13).
narrative_ontology:measurement(hoa__be_t8, hoa_covenant_scope__coordination_reading, base_extractiveness, 8, 0.14).
narrative_ontology:measurement(hoa__be_t12, hoa_covenant_scope__coordination_reading, base_extractiveness, 12, 0.14).
narrative_ontology:measurement(hoa__be_t16, hoa_covenant_scope__coordination_reading, base_extractiveness, 16, 0.15).
narrative_ontology:measurement(hoa__be_t20, hoa_covenant_scope__coordination_reading, base_extractiveness, 20, 0.15).
narrative_ontology:measurement(hoa__be_t24, hoa_covenant_scope__coordination_reading, base_extractiveness, 24, 0.16).

% Suppression requirement over time
narrative_ontology:measurement(hoa__su_t0, hoa_covenant_scope__coordination_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(hoa__su_t4, hoa_covenant_scope__coordination_reading, suppression_requirement, 4, 0.2).
narrative_ontology:measurement(hoa__su_t8, hoa_covenant_scope__coordination_reading, suppression_requirement, 8, 0.21).
narrative_ontology:measurement(hoa__su_t12, hoa_covenant_scope__coordination_reading, suppression_requirement, 12, 0.21).
narrative_ontology:measurement(hoa__su_t16, hoa_covenant_scope__coordination_reading, suppression_requirement, 16, 0.22).
narrative_ontology:measurement(hoa__su_t20, hoa_covenant_scope__coordination_reading, suppression_requirement, 20, 0.22).
narrative_ontology:measurement(hoa__su_t24, hoa_covenant_scope__coordination_reading, suppression_requirement, 24, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hoa_covenant_scope__coordination_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(hoa_covenant_scope__coordination_reading, 0.12).
narrative_ontology:affects_constraint(hoa_covenant_scope__coordination_reading, hoa_covenant_scope__behavioral_control_reading).
narrative_ontology:affects_constraint(hoa_covenant_scope__coordination_reading, hoa_covenant_scope__extraction_reading).

% DUAL FORMULATION NOTE:
% Three constraints share the hoa_covenant_scope kernel (the covenant document and board authority) but instantiate structurally distinct claims with different ε values: coordination_reading (this story, ε≈0.16, Rope, beneficiary=all_homeowners) is the low-extraction end; behavioral_control_reading claims a broader enforcement scope over aesthetic/behavioral conformity; extraction_reading claims concentrated board benefit via fine proliferation and selective enforcement (Snare/Tangled Rope territory, high ε). Per the ε-invariance principle, these are not one constraint measured three ways — they are three constraints that happen to be authored from the same underlying legal instrument. Link via affects_constraints, not merged into one story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
