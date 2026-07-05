% ============================================================================
% CONSTRAINT STORY: hoa_covenant_scope__extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hoa_covenant_scope__extraction_reading, []).

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
 *   constraint_id: hoa_covenant_scope__extraction_reading
 *   human_readable: HOA Covenant as Fine-Proliferation and Board-Power Extraction Mechanism
 *   domain: property_law/collective_governance/urban_planning
 *
 * SUMMARY:
 *   This story instantiates the extraction reading of the HOA covenant
 *   kernel: the covenant's fine schedules, escalation procedures, and lien
 *   mechanics have drifted from deterrence of genuine externalities into a
 *   self-sustaining revenue pipeline benefiting the board, the contracted
 *   management firm, and retained counsel, at the direct expense of
 *   homeowners least able to absorb fines and pass-through costs. This is a
 *   distinct constraint from the coordination reading (shared infrastructure
 *   maintenance) and the behavioral_control reading (aesthetic/behavioral
 *   conformity) — same covenant text, three structurally different claims
 *   about what the covenant actually does and who it serves. ε here is high
 *   and stable-to-rising because the extraction dynamic compounds: fine
 *   schedules widen, management fee structures reward volume, and attorney
 *   fee add-ons make even small original fines balloon into lien-triggering
 *   debt.
 *
 * KEY AGENTS:
 *   - board_members: agenda_setter/beneficiary (organized/arbitrage) — sets fine schedule and enforcement priority, exits freely
 *   - property_management_firms: beneficiary (institutional/arbitrage) — earns fee revenue proportional to notice/fine volume
 *   - hoa_attorneys: beneficiary (institutional/arbitrage) — earns collection and lien-filing fees added to homeowner debt
 *   - financially_vulnerable_homeowners: payer (powerless/trapped) — bears fines, late fees, attorney costs, risk of lien/foreclosure
 *   - renters_via_pass_through: payer (powerless/constrained) — bears passed-through costs with no governance standing
 *   - financially_stable_homeowners: beneficiary/payer (moderate/mobile) — largely insulated, benefits from perceived stability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hoa_covenant_scope__extraction_reading, 0.63).
domain_priors:suppression_score(hoa_covenant_scope__extraction_reading, 0.71).
domain_priors:theater_ratio(hoa_covenant_scope__extraction_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, extractiveness, 0.63).
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hoa_covenant_scope__extraction_reading, tangled_rope).
narrative_ontology:human_readable(hoa_covenant_scope__extraction_reading, "HOA Covenant as Fine-Proliferation and Board-Power Extraction Mechanism").
narrative_ontology:topic_domain(hoa_covenant_scope__extraction_reading, "property_law/collective_governance/urban_planning").

domain_priors:requires_active_enforcement(hoa_covenant_scope__extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hoa_covenant_scope__extraction_reading, 'e68d9bc8-b4cf-4f53-ad54-7cb24f0d9ec3').
narrative_ontology:cs_kernel_codification('e68d9bc8-b4cf-4f53-ad54-7cb24f0d9ec3', formalized).
narrative_ontology:cs_authority_grounding('e68d9bc8-b4cf-4f53-ad54-7cb24f0d9ec3', extraction).
narrative_ontology:cs_interpretation_layer_present('e68d9bc8-b4cf-4f53-ad54-7cb24f0d9ec3').
narrative_ontology:cs_reading_relation('e68d9bc8-b4cf-4f53-ad54-7cb24f0d9ec3', hoa_covenant_scope__coordination_reading, influences).
narrative_ontology:cs_reading_relation('e68d9bc8-b4cf-4f53-ad54-7cb24f0d9ec3', hoa_covenant_scope__behavioral_control_reading, coexists_with).
narrative_ontology:cs_axiom('e68d9bc8-b4cf-4f53-ad54-7cb24f0d9ec3', foundational, fine_schedule_decoupled_from_externality_cost).
narrative_ontology:cs_axiom_status(fine_schedule_decoupled_from_externality_cost, holdable).
narrative_ontology:cs_axiom_grounding('e68d9bc8-b4cf-4f53-ad54-7cb24f0d9ec3', fine_schedule_decoupled_from_externality_cost, empirically_contingent).
narrative_ontology:cs_axiom('e68d9bc8-b4cf-4f53-ad54-7cb24f0d9ec3', foundational, board_discretion_functions_as_rent_extraction_lever).
narrative_ontology:cs_axiom_status(board_discretion_functions_as_rent_extraction_lever, holdable).
narrative_ontology:cs_axiom_grounding('e68d9bc8-b4cf-4f53-ad54-7cb24f0d9ec3', board_discretion_functions_as_rent_extraction_lever, empirically_contingent).
narrative_ontology:cs_reference_frame('e68d9bc8-b4cf-4f53-ad54-7cb24f0d9ec3', deed_restriction_infrastructure_coordination).
narrative_ontology:cs_drift_state('e68d9bc8-b4cf-4f53-ad54-7cb24f0d9ec3', contemporary_fine_lien_regime, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e68d9bc8-b4cf-4f53-ad54-7cb24f0d9ec3', '').
narrative_ontology:cs_kernel_id(hoa_covenant_scope__extraction_reading, hoa_covenant_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__extraction_reading, board_members).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__extraction_reading, property_management_firms).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__extraction_reading, hoa_attorneys).
narrative_ontology:constraint_victim(hoa_covenant_scope__extraction_reading, financially_vulnerable_homeowners).
narrative_ontology:constraint_victim(hoa_covenant_scope__extraction_reading, renters_via_pass_through).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__extraction_reading, financially_stable_homeowners).
narrative_ontology:constraint_victim(hoa_covenant_scope__extraction_reading, financially_stable_homeowners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and amends fine schedules, votes on selective enforcement priorities, and controls which violations get escalated to lien and legal action. Board seats confer social standing and influence over neighbors' property use; incumbents use enforcement discretion to reward allies and pressure critics. Can resign or sell and exit the community entirely, unlike most homeowners bound by the covenant.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, board_members, agenda_setter,
    organized, biographical, arbitrage, local).
narrative_ontology:stakeholder_secondary_role(hoa_covenant_scope__extraction_reading, board_members, beneficiary).

% Contracted by the board to administer inspections, issue violation notices, and process fines; often paid a percentage of fines and late fees collected, or flat administrative fees per notice issued. Has structural incentive to maximize notice volume rather than resolve underlying issues. Can drop or renegotiate a contract with one HOA and move to another client without loss.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, property_management_firms, beneficiary,
    institutional, generational, arbitrage, regional).

% Retained to send collection letters, file liens, and pursue foreclosure for unpaid fines and assessments. Fee structures typically add attorney costs directly onto the homeowner's debt, so litigation volume is a direct revenue stream regardless of case outcome. Operates across many HOA clients; loss of one relationship is immaterial.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, hoa_attorneys, beneficiary,
    institutional, biographical, arbitrage, regional).

% Subject to fines for covenant violations (paint color, parking, landscaping, trash bin timing) that compound with late fees and attorney costs into liens against the home. Selling to exit means absorbing the lien or losing equity; many cannot afford to move regardless. Fine schedules and escalation timelines are set unilaterally by the board and are difficult to contest without hiring counsel they cannot afford.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, financially_vulnerable_homeowners, payer,
    powerless, biographical, trapped, local).

% Do not sit on the board or vote on covenant terms but bear costs when landlords pass fines and increased HOA dues through as rent increases or fee assessments. Have no standing in HOA governance and no direct recourse; exit means finding new housing, which is costly and disruptive.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, renters_via_pass_through, payer,
    powerless, immediate, constrained, local).

% Pay occasional fines without financial strain and may benefit from perceived property-value stability the covenant claims to protect. Can absorb enforcement costs, contest violations through counsel if motivated, or sell and relocate without hardship. Largely insulated from the extraction dynamic that traps less liquid neighbors.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, financially_stable_homeowners, beneficiary,
    moderate, biographical, mobile, local).
narrative_ontology:stakeholder_secondary_role(hoa_covenant_scope__extraction_reading, financially_stable_homeowners, payer).

% Process lien filings and adjudicate foreclosure and collection disputes arising from unpaid fines. Generally treat properly filed liens as presumptively valid without auditing whether the underlying fine schedule or enforcement pattern was applied selectively or in bad faith.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, county_recorder_and_courts, observer,
    institutional, generational, analytical, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hoa_covenant_scope__extraction_reading, board_members).
narrative_ontology:fixing_cost_class(hoa_covenant_scope__extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In principle, a fine schedule deters covenant violations that would otherwise degrade shared amenities or provoke neighbor disputes, without requiring case-by-case litigation for every infraction.
% TRANSFER_FUNCTION: Moves money from fined homeowners (and, through rent pass-through, from renters) to the board's discretionary budget, to the property management firm's fee revenue, and to attorneys retained for collection and lien work — via fines, late fees, and attorney-cost add-ons layered onto the base assessment structure.
% ABSENT_VOICES: Renters have no vote and no seat in HOA governance despite bearing pass-through costs. Homeowners who cannot afford legal counsel to contest a fine or lien have no meaningful hearing process distinct from the board that issued the fine and benefits from its collection.
% DISAPPEARANCE_RATIONALE: If fine-based enforcement disappeared overnight, board discretionary revenue would collapse, property management contracts would shrink to core maintenance administration, attorney referral volume would drop sharply, and the threat of lien/foreclosure over minor violations would vanish — a substantial redistribution of both money and power within the community.
% FOUNDING_PROBLEM: Shared residential developments needed a mechanism to maintain common infrastructure and prevent free-riding on collectively funded amenities and aesthetic standards that affect resale value.
% FOUNDING_PROBLEM_CORROBORATION: Property management industry associations and board members attest the fine schedule is still needed to deter free-riding. Independent state legislative task force reports (multiple U.S. states have investigated HOA lien/foreclosure practices in the 2010s-2020s) and homeowner advocacy groups outside the beneficiary set attest that fine and lien practices have become detached from any genuine maintenance function and instead operate as a standalone revenue and control mechanism.
narrative_ontology:disappearance_verdict(hoa_covenant_scope__extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(hoa_covenant_scope__extraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hoa_covenant_scope__extraction_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hoa_covenant_scope__extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hoa_covenant_scope__extraction_reading, 0.63, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hoa_covenant_scope__extraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hoa_covenant_scope__extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hoa_covenant_scope__extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.63) reflects a fine/lien apparatus that has decoupled from the marginal cost of the violations it nominally deters — attorney fee add-ons and escalating late penalties generate revenue disproportionate to any coordination benefit. Suppression (0.71) is high because exit is structurally blocked: selling a lien-encumbered property destroys equity, and there is no independent forum to contest board-set fine schedules short of costly litigation. Theater ratio (0.42) captures those fines nominally justified as deterrence of externalities (unmowed lawns affecting neighbors) that in practice generate revenue disconnected from any measurable harm (paint-shade violations, holiday-decoration timing). All three temporal series share the 0-24 grid; suppression_requirement rises fastest, reflecting hardening enforcement infrastructure (standardized escalation letters, routinized lien filing) over the interval.
 *
 * PERSPECTIVAL GAP:
 *   From the board's seat, the fine schedule looks like responsible fiscal stewardship and legitimate deterrence — a rope. From the trapped homeowner's seat facing a $4,000 lien for a stack of unapproved patio furniture, the same structure is enforced extraction with an unreachable appeals process. The engine computing divergent per-seat types from identical structural data is the correct behavior here, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Board members and their contracted firms sit near the full-beneficiary end: they set the rules, collect or facilitate collection of the extraction, and can exit without cost. Financially vulnerable homeowners sit near the full-target end: trapped by equity loss on exit, without governance voice proportional to their exposure, and bearing compounding fee structures they did not design. Renters occupy an unusual position — victims of the extraction with zero formal standing in the arrangement, which is why they are declared excluded-adjacent through pass-through rather than as direct covenant parties.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (free-riding on shared infrastructure) is contested as still live — some maintenance coordination genuinely persists — but the fine/lien apparatus has outgrown and substantially decoupled from that founding function, evidenced by escalating attorney-fee-driven debt on cosmetic violations. Classifying as tangled_rope rather than pure snare preserves the genuine (if now minority) coordination residue while still naming the asymmetric extraction machinery riding on top of it — collapsing this into pure snare would deny that any coordination function ever existed; calling it a clean rope would launder the extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_residue_fraction,
    'What fraction of current fine and lien activity still tracks genuine externality deterrence (e.g., structural maintenance neglect affecting neighbors) versus fines with no plausible externality basis (cosmetic, procedural, timing violations)?',
    'Audit a sample of fine notices and lien filings across multiple HOAs, categorizing each violation type by whether it plausibly imposes a cost on neighbors versus purely aesthetic/procedural noncompliance; compare fine amounts to any measurable externality cost.',
    'A high coordination-residue fraction would push the classification toward the coordination_reading being the dominant real function with extraction as incidental; a low fraction confirms this reading''s premise that extraction has become the primary operative function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_residue_fraction, empirical, 'How much of current enforcement still serves genuine coordination versus pure extraction.').

omega_variable(
    selective_enforcement_bias_pattern,
    'Is enforcement selectivity correlated with board social alliances/conflicts (targeting critics, sparing allies) or is it a neutral byproduct of complaint-driven or randomized inspection?',
    'Cross-reference violation notice recipients against board voting/dissent records and social network data (e.g., HOA meeting attendance, public comment records) to test for statistically significant targeting patterns.',
    'Confirmed targeting would strengthen the board-power-consolidation half of this reading''s claim and support treating board_members'' directionality as even more concentrated toward the beneficiary end than currently modeled; absence of a pattern would weaken the power-consolidation claim while leaving the pure revenue-extraction claim intact.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(selective_enforcement_bias_pattern, empirical, 'Whether selective enforcement tracks political alliance rather than neutral rule violation.').

omega_variable(
    reading_boundary_stability,
    'Is the extraction_reading a stable, independently identifiable structural claim, or does it shade continuously into the coordination_reading such that the boundary between ''genuine deterrence fine'' and ''extractive fine'' is itself contested rather than empirically sharp?',
    'Longitudinal tracking of fine schedule amendments and enforcement volume against documented maintenance/infrastructure need; a genuinely bounded extraction reading should show fine growth outpacing any plausible growth in externality-generating behavior.',
    'If the boundary is fuzzy, the three sibling readings may not be cleanly separable stories but points on a continuum, which would argue for revisiting the ε-invariance decomposition itself rather than treating extraction_reading as a fully independent constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_boundary_stability, conceptual, 'Whether the extraction reading is a genuinely distinct structural claim or a matter of degree along the coordination reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hoa_covenant_scope__extraction_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hoa__tr_t0, hoa_covenant_scope__extraction_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(hoa__tr_t4, hoa_covenant_scope__extraction_reading, theater_ratio, 4, 0.24).
narrative_ontology:measurement(hoa__tr_t8, hoa_covenant_scope__extraction_reading, theater_ratio, 8, 0.29).
narrative_ontology:measurement(hoa__tr_t12, hoa_covenant_scope__extraction_reading, theater_ratio, 12, 0.33).
narrative_ontology:measurement(hoa__tr_t16, hoa_covenant_scope__extraction_reading, theater_ratio, 16, 0.37).
narrative_ontology:measurement(hoa__tr_t20, hoa_covenant_scope__extraction_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(hoa__tr_t24, hoa_covenant_scope__extraction_reading, theater_ratio, 24, 0.42).

% Extraction over time
narrative_ontology:measurement(hoa__be_t0, hoa_covenant_scope__extraction_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(hoa__be_t4, hoa_covenant_scope__extraction_reading, base_extractiveness, 4, 0.41).
narrative_ontology:measurement(hoa__be_t8, hoa_covenant_scope__extraction_reading, base_extractiveness, 8, 0.47).
narrative_ontology:measurement(hoa__be_t12, hoa_covenant_scope__extraction_reading, base_extractiveness, 12, 0.53).
narrative_ontology:measurement(hoa__be_t16, hoa_covenant_scope__extraction_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(hoa__be_t20, hoa_covenant_scope__extraction_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(hoa__be_t24, hoa_covenant_scope__extraction_reading, base_extractiveness, 24, 0.63).

% Suppression requirement over time
narrative_ontology:measurement(hoa__su_t0, hoa_covenant_scope__extraction_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(hoa__su_t4, hoa_covenant_scope__extraction_reading, suppression_requirement, 4, 0.52).
narrative_ontology:measurement(hoa__su_t8, hoa_covenant_scope__extraction_reading, suppression_requirement, 8, 0.58).
narrative_ontology:measurement(hoa__su_t12, hoa_covenant_scope__extraction_reading, suppression_requirement, 12, 0.63).
narrative_ontology:measurement(hoa__su_t16, hoa_covenant_scope__extraction_reading, suppression_requirement, 16, 0.67).
narrative_ontology:measurement(hoa__su_t20, hoa_covenant_scope__extraction_reading, suppression_requirement, 20, 0.69).
narrative_ontology:measurement(hoa__su_t24, hoa_covenant_scope__extraction_reading, suppression_requirement, 24, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hoa_covenant_scope__extraction_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(hoa_covenant_scope__extraction_reading, 0.1).
narrative_ontology:affects_constraint(hoa_covenant_scope__extraction_reading, hoa_covenant_scope__coordination_reading).
narrative_ontology:affects_constraint(hoa_covenant_scope__extraction_reading, hoa_covenant_scope__behavioral_control_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language label 'HOA covenant' per the ε-invariance principle. coordination_reading models the genuine shared-infrastructure function (lower ε, closer to rope/tangled_rope boundary from the maintenance-necessity angle). behavioral_control_reading models aesthetic/conformity enforcement as a value-maximization strategy (moderate ε, contested externality basis). This story (extraction_reading) isolates the fine-proliferation and board-power-consolidation dynamic as the highest-ε claim in the family, with a distinct beneficiary set (board_members, property_management_firms, hoa_attorneys) and victim set (financially_vulnerable_homeowners, renters_via_pass_through) not shared identically across the siblings. All three should be read as the same covenant text under three structurally distinct claims, not one constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
