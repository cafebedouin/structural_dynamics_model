% ============================================================================
% CONSTRAINT STORY: hoa_covenant_scope__coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: HOA Covenant as Shared Infrastructure Coordination Mechanism
 *   domain: property_law/collective_governance/urban_planning
 *
 * SUMMARY:
 *   This story instantiates ONLY the coordination reading of the HOA covenant
 *   kernel: the covenant as a mechanism to fund and administer maintenance of
 *   infrastructure genuinely shared among units (private roads, drainage,
 *   retention ponds, structural elements) and to resolve objective nuisances
 *   (blocked easements, structural neglect affecting neighbors). This reading
 *   deliberately excludes aesthetic/behavioral rule enforcement and excludes
 *   fine-driven revenue generation — those are separate constraints
 *   (behavioral_control_reading, extraction_reading) sharing the same
 *   covenant text but instantiating structurally distinct claims with
 *   different ε, different beneficiary/victim structures, and different
 *   classifications. Under this reading alone, the metrics are low:
 *   extraction is near the coordination floor, suppression is limited to the
 *   narrow lien mechanism against genuine free-riding, and theater is minimal
 *   because the enforcement scope is tightly bound to objective, measurable
 *   infrastructure conditions.
 *
 * KEY AGENTS:
 *   - all_homeowners: symmetric beneficiary/payer (organized/constrained) — funds and receives shared infrastructure upkeep
 *   - free_riders: payer under the formal ledger sense (moderate/constrained) — the covenant's lien mechanism prevents consumption of shared goods without proportional payment
 *   - hoa_board: agenda_setter (organized/mobile) — administers the narrow cost-recovery and nuisance-resolution mandate, itself composed of rotating homeowners
 *   - prospective_buyers: observer (moderate/mobile) — disciplines the arrangement via exit/entry pricing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hoa_covenant_scope__coordination_reading, 0.16).
domain_priors:suppression_score(hoa_covenant_scope__coordination_reading, 0.2).
domain_priors:theater_ratio(hoa_covenant_scope__coordination_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, extractiveness, 0.16).
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hoa_covenant_scope__coordination_reading, rope).
narrative_ontology:human_readable(hoa_covenant_scope__coordination_reading, "HOA Covenant as Shared Infrastructure Coordination Mechanism").
narrative_ontology:topic_domain(hoa_covenant_scope__coordination_reading, "property_law/collective_governance/urban_planning").

domain_priors:requires_active_enforcement(hoa_covenant_scope__coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hoa_covenant_scope__coordination_reading, '1eb0b10b-7484-4ad0-8dd5-5e60ff873933').
narrative_ontology:cs_kernel_codification('1eb0b10b-7484-4ad0-8dd5-5e60ff873933', fixed_text).
narrative_ontology:cs_authority_grounding('1eb0b10b-7484-4ad0-8dd5-5e60ff873933', lineage).
narrative_ontology:cs_interpretation_layer_present('1eb0b10b-7484-4ad0-8dd5-5e60ff873933').
narrative_ontology:cs_reading_relation('1eb0b10b-7484-4ad0-8dd5-5e60ff873933', hoa_covenant_scope__behavioral_control_reading, coexists_with).
narrative_ontology:cs_reading_relation('1eb0b10b-7484-4ad0-8dd5-5e60ff873933', hoa_covenant_scope__extraction_reading, influences).
narrative_ontology:cs_axiom('1eb0b10b-7484-4ad0-8dd5-5e60ff873933', foundational, enforcement_scope_bounded_by_objective_externality).
narrative_ontology:cs_axiom_status(enforcement_scope_bounded_by_objective_externality, holdable).
narrative_ontology:cs_axiom_grounding('1eb0b10b-7484-4ad0-8dd5-5e60ff873933', enforcement_scope_bounded_by_objective_externality, conventional).
narrative_ontology:cs_axiom('1eb0b10b-7484-4ad0-8dd5-5e60ff873933', secondary, dues_must_track_actual_infrastructure_cost).
narrative_ontology:cs_axiom_status(dues_must_track_actual_infrastructure_cost, holdable).
narrative_ontology:cs_axiom_grounding('1eb0b10b-7484-4ad0-8dd5-5e60ff873933', dues_must_track_actual_infrastructure_cost, instrumental).
narrative_ontology:cs_reference_frame('1eb0b10b-7484-4ad0-8dd5-5e60ff873933', narrow_infrastructure_cost_recovery_mandate).
narrative_ontology:cs_drift_state('1eb0b10b-7484-4ad0-8dd5-5e60ff873933', contemporary_hoa_practice, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('1eb0b10b-7484-4ad0-8dd5-5e60ff873933', '').
narrative_ontology:cs_kernel_id(hoa_covenant_scope__coordination_reading, hoa_covenant_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__coordination_reading, all_homeowners).
narrative_ontology:constraint_victim(hoa_covenant_scope__coordination_reading, free_riders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(hoa_covenant_scope__coordination_reading, all_homeowners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own units sharing private roads, drainage, retention ponds, and common structural elements. Pay assessed dues into a maintenance fund and, in exchange, get roads repaved, pipes fixed, and shared systems kept functional without needing to individually contract and monitor every repair. Exit means selling the property, which is costly but not impossible; while resident, they are bound by the same cost-sharing rule they benefit from.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, all_homeowners, beneficiary,
    organized, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(hoa_covenant_scope__coordination_reading, all_homeowners, payer).

% Homeowners who would prefer to defer or skip contributing to shared infrastructure costs while still using the shared road, drainage, or retention system. The covenant's dues-and-lien mechanism prevents them from consuming the shared good without paying their proportional share; in the coordination reading this is treated as the constraint doing its job, not as victimization, though the term 'victim' is used here only in the formal beneficiary/victim ledger sense demanded by the rope/tangled-rope gate logic, not to imply moral wrong.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, free_riders, payer,
    moderate, biographical, constrained, local).

% Elected from and by the homeowner pool, administers the maintenance schedule, collects dues, contracts repairs, and enforces the narrow set of rules tied to infrastructure cost-recovery and objectively defined nuisances (e.g., blocked drainage easements, unrepaired shared retaining walls). Board members are themselves homeowners bound by the same assessments; their exit option is the same as any homeowner's — sell and leave — though turnover in the board seat itself is easy (annual elections).
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, hoa_board, agenda_setter,
    organized, biographical, mobile, local).

% Evaluate the covenant's terms and the HOA's maintenance fund health before purchasing. In the coordination reading, a well-maintained shared infrastructure and transparent, cost-linked dues schedule is a legible signal of a functioning arrangement; they can walk away and buy elsewhere if the numbers do not check out, which disciplines the board toward the coordination function rather than mission creep.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, prospective_buyers, observer,
    moderate, immediate, mobile, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hoa_covenant_scope__coordination_reading, diffuse).
narrative_ontology:fixing_cost_class(hoa_covenant_scope__coordination_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a genuine collective-action problem: shared roads, drainage, retention ponds, and structural elements serving multiple units cannot be efficiently maintained by unilateral individual action, and their failure imposes costs on all units regardless of who caused the failure. The covenant pools funds and assigns maintenance authority so the shared asset does not degrade through free-riding or coordination failure.
% TRANSFER_FUNCTION: Moves proportional dues from each homeowner into a shared maintenance fund, and moves accumulated fund balances back out as contracted repair and upkeep work benefiting all units connected to the shared infrastructure. In this reading the net flow is symmetric across the homeowner population over time — everyone pays in and everyone draws maintenance benefit from the same shared systems.
% ABSENT_VOICES: Renters and long-term tenants who use the shared infrastructure but have no vote on assessment levels or maintenance priorities are not seated at the table; they would likely want a voice in maintenance sequencing (e.g., prioritizing a leaking pipe over a resurfaced entrance sign) but are structurally outside the covenant's membership, which runs to titled owners only.
% DISAPPEARANCE_RATIONALE: If the covenant and its narrow infrastructure-cost-recovery enforcement vanished overnight, the shared road, drainage, and structural systems would have no funding mechanism or maintenance authority; within a few seasons deferred maintenance would begin degrading shared assets, and homeowners would face either a costly emergency special assessment negotiated from scratch or unilateral individual repairs with no cost-sharing, both worse than the status quo.
% FOUNDING_PROBLEM: Multiple units sharing physical infrastructure (private roads, drainage, retention ponds, party walls) had no default legal mechanism to fund upkeep or resolve objective nuisances (e.g., a blocked easement) without either a court-ordered partition action or informal ad hoc collections that any owner could refuse.
% FOUNDING_PROBLEM_CORROBORATION: Independent civil engineering assessments of shared-infrastructure deterioration in developments lacking any cost-sharing mechanism (unincorporated subdivisions with private roads and no HOA) corroborate that the underlying maintenance-funding problem persists absent a coordinating structure; this is attested by municipal planning departments and civil engineers who are not HOA board members or dues-collecting parties.
narrative_ontology:disappearance_verdict(hoa_covenant_scope__coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(hoa_covenant_scope__coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hoa_covenant_scope__coordination_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extraction is low (0.16 at interval end) because dues under this reading track actual infrastructure cost, not discretionary fine revenue or behavioral compliance costs. Suppression is low-moderate (0.20) reflecting the real but narrow lien/foreclosure mechanism used against non-payment of infrastructure-linked assessments — a genuine coercive backstop, but scoped to cost recovery, not conduct policing. Theater is very low (0.10) because enforcement activity under this reading is concentrated on measurable infrastructure conditions (road surface integrity, drainage function, structural safety) rather than performative rule enforcement. Accessibility collapse is moderate (0.30): once inside the development, alternatives to the shared-infrastructure arrangement do collapse somewhat (you cannot opt out of the shared road while living there), but the buyer's ex ante choice not to purchase into the covenant remains a real, uncollapsed alternative — distinguishing this from a mountain-grade collapse.
 *
 * PERSPECTIVAL GAP:
 *   Under this reading there should be minimal seat divergence: homeowners, free riders (once they pay), and the board should all compute close to rope, because the structural data — narrow enforcement scope, symmetric beneficiary declaration, low suppression, low theater — is authored to reflect a genuinely functioning coordination mechanism. Divergence would only emerge if the engine's computation surfaces asymmetry the authoring did not intend, which would itself be a signal worth investigating rather than something to correct by adjusting the claim.
 *
 * DIRECTIONALITY LOGIC:
 *   All homeowners sit near symmetric directionality: they pay in and draw maintenance benefit from the same shared systems over the same time horizon, so d clusters near 0.5. Free riders are declared as the formal victim class required by the schema's gate logic, but their 'extraction' is the covenant successfully preventing them from consuming the shared good without paying — under this reading that is the coordination mechanism functioning as designed, not exploitation. The hoa_board, though administratively powerful, is itself drawn from and returns to the homeowner pool each election cycle, which keeps its directionality close to the homeowner baseline rather than approaching a captured-beneficiary profile.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (uncoordinated shared-infrastructure funding) remains live per independent civil-engineering corroboration outside the beneficiary set, so this reading shows no mandatrophy: the mandate has not outlived its function. This is precisely the discipline the kernel-reading structure is meant to enforce — by isolating the coordination function into its own clean constraint, the corpus avoids either (a) crediting behavioral control or fee extraction with the legitimacy of genuine infrastructure coordination, or (b) tarring genuine infrastructure coordination with the extraction dynamics that may be present elsewhere in the same covenant text.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_scope_creep_boundary,
    'Where exactly does the covenant''s enforcement scope stop being ''objective nuisance resolution tied to shared infrastructure'' and start being ''aesthetic/behavioral control'' or ''discretionary fine revenue''? The same clause (e.g., a nuisance provision) can be read narrowly (blocked drainage easement) or broadly (unapproved paint color) depending on board interpretation.',
    'Audit the actual enforcement log against a coding scheme that separates infrastructure-cost-linked actions (special assessments, lien filings for unpaid dues, contractor-verified repair) from discretionary or aesthetic actions (fines for paint color, lawn ornament removal, holiday decoration timing). A high ratio of the former corroborates the coordination reading as descriptively dominant in this specific HOA''s practice; a rising ratio of the latter over time would indicate drift toward the sibling readings.',
    'If audit shows scope creep into aesthetic/behavioral enforcement or fine-revenue patterns, THIS reading''s ε would need revision upward and the behavioral_control_reading or extraction_reading would better describe the HOA''s actual current operation, even though this file remains a valid description of the covenant''s coordination function in isolation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_scope_creep_boundary, empirical, 'Whether real-world enforcement stays within the coordination reading''s declared narrow scope or drifts toward the sibling readings.').

omega_variable(
    free_rider_victim_framing,
    'Is ''free_riders'' a genuine victim class in the extractive sense, or is it a formal ledger entry required by the schema''s beneficiary/victim gate that mischaracterizes what is actually happening (a homeowner being correctly required to pay their share)?',
    'Compare the lien/foreclosure enforcement rate and severity against the actual cost differential a free-riding homeowner would impose on neighbors; if enforcement is proportionate to the externality imposed, the ''victim'' framing is a formal artifact, not a substantive extraction claim.',
    'If enforcement against free riders is disproportionate to the externality they impose (e.g., punitive fees far exceeding the deferred maintenance cost), this reading would need to reclassify toward tangled_rope; if proportionate, the rope classification and low-ε reading hold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(free_rider_victim_framing, conceptual, 'Whether the free-rider victim declaration reflects genuine asymmetric extraction or is a formal artifact of the schema''s gate logic.').

omega_variable(
    kernel_reading_dominance,
    'Given that all three readings (coordination, behavioral_control, extraction) describe the same underlying covenant text and the same HOA board''s operation, which reading is descriptively dominant in practice for any given real HOA, and can a single HOA legitimately be scored under more than one reading simultaneously (i.e., is the covenant''s actual operation a blend rather than a pure instance of any single reading)?',
    'Longitudinal case studies scoring individual HOAs against all three reading rubrics using enforcement-log and financial-disclosure data; cluster analysis on whether real HOAs sort into distinct reading-dominant types or occupy a continuous blend.',
    'If real HOAs are typically blends rather than pure types, the ε-invariance decomposition into three separate constraint files remains structurally correct (each captures one component) but the network edges between them (affects_constraints) become the primary analytical tool for understanding any specific HOA, rather than any single file being ''the'' answer.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_dominance, conceptual, 'Whether real HOAs instantiate one dominant reading or a blend across all three, and what that implies for interpreting the decomposed constraint family.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hoa_covenant_scope__coordination_reading, 0, 20).

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

% Extraction over time
narrative_ontology:measurement(hoa__be_t0, hoa_covenant_scope__coordination_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(hoa__be_t4, hoa_covenant_scope__coordination_reading, base_extractiveness, 4, 0.13).
narrative_ontology:measurement(hoa__be_t8, hoa_covenant_scope__coordination_reading, base_extractiveness, 8, 0.14).
narrative_ontology:measurement(hoa__be_t12, hoa_covenant_scope__coordination_reading, base_extractiveness, 12, 0.15).
narrative_ontology:measurement(hoa__be_t16, hoa_covenant_scope__coordination_reading, base_extractiveness, 16, 0.16).
narrative_ontology:measurement(hoa__be_t20, hoa_covenant_scope__coordination_reading, base_extractiveness, 20, 0.16).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(hoa_covenant_scope__coordination_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hoa_covenant_scope__coordination_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(hoa_covenant_scope__coordination_reading, 0.12).
narrative_ontology:affects_constraint(hoa_covenant_scope__coordination_reading, hoa_covenant_scope__behavioral_control_reading).
narrative_ontology:affects_constraint(hoa_covenant_scope__coordination_reading, hoa_covenant_scope__extraction_reading).

% DUAL FORMULATION NOTE:
% This file is one of three linked stories decomposing the natural-language concept 'HOA covenant' per the ε-invariance principle. coordination_reading (this file, ε≈0.16, Rope) shares covenant text with behavioral_control_reading (expected higher ε, aesthetic/conformity enforcement, likely tangled_rope or snare depending on suppression) and extraction_reading (expected highest ε, fine-revenue/board-power consolidation, likely snare or tangled_rope). All three are linked bidirectionally in intent; each file independently declares affects_constraints to its siblings. The coordination reading is authored as upstream/foundational in the sense that it identifies the genuine function the other readings' cover stories invoke — behavioral control and extraction narratives typically justify themselves by pointing to the same infrastructure-coordination need this file isolates.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
