% ============================================================================
% CONSTRAINT STORY: hoa_covenant_scope__extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   human_readable: HOA Covenant as Fine-Proliferation Revenue and Board Power Consolidation Mechanism
 *   domain: property_law/collective_governance/urban_planning
 *
 * SUMMARY:
 *   This story instantiates the extraction reading of the HOA covenant
 *   kernel: the covenant's operation is read as a mechanism whose primary
 *   structural function has become fine-schedule revenue generation and
 *   board/management/legal power consolidation, with the shared-maintenance
 *   coordination story serving as cover for expedited lien processes and
 *   attorney-fee capture. This is a distinct constraint from the
 *   coordination_reading (which reads the same covenant as solving genuine
 *   collective-action problems in shared infrastructure) and the
 *   behavioral_control_reading (which reads it as aesthetic/behavioral
 *   conformity enforcement) — the three readings share a kernel text but
 *   instantiate different ε values, different beneficiary/victim structures,
 *   and different classifications, per the ε-invariance principle. Do not
 *   average across readings; this file's ε (0.66) is this reading's own.
 *
 * KEY AGENTS:
 *   - board_members: agenda_setter (organized/arbitrage) — sets fine schedules and enforcement discretion, can exit via resignation or sale
 *   - property_management_firms: beneficiary (institutional/arbitrage) — earns per-violation fees, structurally incentivized toward high citation volume
 *   - collections_legal_counsel: beneficiary (institutional/arbitrage) — recovers statutory attorney fees added to liens, profits from escalation over early resolution
 *   - financially_vulnerable_homeowners: payer (powerless/trapped) — bears compounding fines and lien/foreclosure risk, cannot exit without losing the property
 *   - renters_via_pass_through: payer (powerless/constrained) — absorbs pass-through costs with no voice in enforcement decisions
 *   - state_hoa_oversight_bodies: excluded (institutional/analytical) — positioned to assess systemic abuse but rarely engaged before foreclosure is imminent
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hoa_covenant_scope__extraction_reading, 0.66).
domain_priors:suppression_score(hoa_covenant_scope__extraction_reading, 0.71).
domain_priors:theater_ratio(hoa_covenant_scope__extraction_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hoa_covenant_scope__extraction_reading, tangled_rope).
narrative_ontology:human_readable(hoa_covenant_scope__extraction_reading, "HOA Covenant as Fine-Proliferation Revenue and Board Power Consolidation Mechanism").
narrative_ontology:topic_domain(hoa_covenant_scope__extraction_reading, "property_law/collective_governance/urban_planning").

domain_priors:requires_active_enforcement(hoa_covenant_scope__extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hoa_covenant_scope__extraction_reading, 'e3f4d569-1b05-41db-9854-1c0b99d5d677').
narrative_ontology:cs_kernel_codification('e3f4d569-1b05-41db-9854-1c0b99d5d677', fixed_text).
narrative_ontology:cs_authority_grounding('e3f4d569-1b05-41db-9854-1c0b99d5d677', extraction).
narrative_ontology:cs_interpretation_layer_present('e3f4d569-1b05-41db-9854-1c0b99d5d677').
narrative_ontology:cs_reading_relation('e3f4d569-1b05-41db-9854-1c0b99d5d677', hoa_covenant_scope__coordination_reading, influences).
narrative_ontology:cs_reading_relation('e3f4d569-1b05-41db-9854-1c0b99d5d677', hoa_covenant_scope__behavioral_control_reading, coexists_with).
narrative_ontology:cs_axiom('e3f4d569-1b05-41db-9854-1c0b99d5d677', foundational, enforcement_discretion_is_rent_extraction_not_governance).
narrative_ontology:cs_axiom_status(enforcement_discretion_is_rent_extraction_not_governance, holdable).
narrative_ontology:cs_axiom_grounding('e3f4d569-1b05-41db-9854-1c0b99d5d677', enforcement_discretion_is_rent_extraction_not_governance, empirically_contingent).
narrative_ontology:cs_axiom('e3f4d569-1b05-41db-9854-1c0b99d5d677', secondary, fee_shifting_statutes_convert_dispute_resolution_into_profit_center).
narrative_ontology:cs_axiom_status(fee_shifting_statutes_convert_dispute_resolution_into_profit_center, holdable).
narrative_ontology:cs_axiom_grounding('e3f4d569-1b05-41db-9854-1c0b99d5d677', fee_shifting_statutes_convert_dispute_resolution_into_profit_center, empirically_contingent).
narrative_ontology:cs_reference_frame('e3f4d569-1b05-41db-9854-1c0b99d5d677', maintenance_funding_covenant).
narrative_ontology:cs_drift_state('e3f4d569-1b05-41db-9854-1c0b99d5d677', contemporary_fee_escalation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e3f4d569-1b05-41db-9854-1c0b99d5d677', '').
narrative_ontology:cs_kernel_id(hoa_covenant_scope__extraction_reading, hoa_covenant_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__extraction_reading, board_members).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__extraction_reading, property_management_firms).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__extraction_reading, collections_legal_counsel).
narrative_ontology:constraint_victim(hoa_covenant_scope__extraction_reading, financially_vulnerable_homeowners).
narrative_ontology:constraint_victim(hoa_covenant_scope__extraction_reading, renters_via_pass_through).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__extraction_reading, compliant_long_term_owners).
narrative_ontology:constraint_victim(hoa_covenant_scope__extraction_reading, compliant_long_term_owners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and amends the fine schedule, selects which violations get cited and which get waived, and retains discretion over lien referral timing. Board seats carry social standing and, in self-managed associations, direct control over vendor and legal-counsel contracts from which board members or their associates sometimes benefit. Board members can resign or sell and exit the association entirely, unlike homeowners who remain subject to covenant enforcement as long as they own the unit.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, board_members, agenda_setter,
    organized, biographical, arbitrage, local).
narrative_ontology:stakeholder_secondary_role(hoa_covenant_scope__extraction_reading, board_members, beneficiary).

% Contracted by the board to administer covenant enforcement; earns per-violation processing fees, inspection fees, and administrative surcharges layered onto every fine and lien action. Has no ownership stake in the community and can walk away from a management contract at will, while structuring fee schedules that reward high citation volume.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, property_management_firms, beneficiary,
    institutional, generational, arbitrage, regional).

% Retained to pursue liens and foreclosure actions against delinquent homeowners; recovers statutorily authorized attorney fees added directly to the homeowner's debt regardless of the underlying fine's size, making escalation to litigation more profitable than early resolution.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, collections_legal_counsel, beneficiary,
    institutional, biographical, arbitrage, regional).

% Faces compounding fines for minor covenant infractions (paint color, lawn height, parking) that escalate through late fees, administrative charges, and attorney fees into liens against the home. Cannot sell without settling the lien and cannot leave the association without leaving the property; a missed payment plan installment can trigger acceleration to the full balance plus fees.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, financially_vulnerable_homeowners, payer,
    powerless, biographical, trapped, local).

% Does not sit on the association or vote, but absorbs covenant-driven costs when landlords raise rent to cover HOA fines and special assessments levied to cover association legal costs and uncollected liens. Has no standing to contest enforcement decisions that nonetheless shape their housing costs.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, renters_via_pass_through, payer,
    powerless, immediate, constrained, local).

% Benefits from perceived neighborhood order and aesthetic consistency the covenant nominally protects, and may see modest property value support, but also pays regular dues that increasingly fund legal and management overhead generated by enforcement actions against other owners rather than shared infrastructure.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, compliant_long_term_owners, beneficiary,
    moderate, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(hoa_covenant_scope__extraction_reading, compliant_long_term_owners, payer).

% In most jurisdictions has limited statutory authority to review fine schedules or foreclosure practices and is chronically underfunded relative to the volume of associations; is rarely brought into individual enforcement disputes despite being positioned to assess systemic fine-proliferation patterns.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, state_hoa_oversight_bodies, excluded,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hoa_covenant_scope__extraction_reading, diffuse).
narrative_ontology:fixing_cost_class(hoa_covenant_scope__extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Nominally provides a mechanism to resolve shared-property externalities (shared roof and grounds maintenance, insurance pooling) and enforce agreed community standards.
% TRANSFER_FUNCTION: Moves money from cited homeowners and, indirectly, renters paying pass-through rent increases, to the management firm (processing and inspection fees), collections counsel (attorney fees added to liens), and to the association's operating and legal-defense accounts controlled by the board.
% ABSENT_VOICES: Renters bear pass-through costs but have no vote or standing in the association. State oversight bodies that could assess systemic fine-schedule abuse are rarely invited into individual disputes and typically only intervene after foreclosure is imminent, if at all.
% DISAPPEARANCE_RATIONALE: Board members, management firms, and collections counsel would say the community disintegrates into maintenance disputes and free-riding on shared assets. Financially vulnerable homeowners and tenant advocates would say the immediate effect is that fine-driven lien and foreclosure risk vanishes, rents stabilize, and shared maintenance could be re-organized under a leaner, lower-fee structure — the parties dispute which effect dominates.
% FOUNDING_PROBLEM: Original covenants were adopted to fund and coordinate maintenance of shared infrastructure (roofs, common grounds, drainage) and to prevent free-riding by owners who would otherwise let shared assets degrade.
% FOUNDING_PROBLEM_CORROBORATION: Board members and management firms attest the founding maintenance problem remains live and justifies the enforcement apparatus. Independent evidence — state ombudsman complaint data, foreclosure filings initiated over sub-$2,000 fine balances, and academic studies of HOA lien practices — corroborates, from outside the benefiting parties, that fine schedules and legal fee structures have expanded well beyond what shared-maintenance funding requires.
narrative_ontology:disappearance_verdict(hoa_covenant_scope__extraction_reading, contested).
narrative_ontology:founding_problem_status(hoa_covenant_scope__extraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hoa_covenant_scope__extraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hoa_covenant_scope__extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hoa_covenant_scope__extraction_reading, 0.66, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored at 0.66 for this reading: fine schedules under this reading are decoupled from actual maintenance cost or externality severity, and attorney-fee statutes create a structural incentive to litigate rather than resolve. Suppression (0.71) is high because exit for the primary payer class requires either paying the escalated debt or losing the home — there is no meaningful alternative dispute channel most homeowners can access before a lien attaches. Theater ratio (0.42) reflects that some genuine maintenance coordination persists alongside the extraction — the covenant is not pure theater, hence tangled_rope rather than snare — but a rising share of enforcement activity (reflected in the theater_ratio and suppression_requirement trajectories) serves fee generation and board authority rather than the shared-infrastructure function.
 *
 * DIRECTIONALITY LOGIC:
 *   Board members, management firms, and legal counsel are structural beneficiaries: they set or profit from enforcement discretion and carry mobile/arbitrage exit relative to the covenant itself (a board member can resign; a firm can lose the contract and move on). Financially vulnerable homeowners are structural targets: trapped exit (leaving requires losing home equity to a lien or selling under duress), high directionality toward extraction. Renters carry no formal relationship to the covenant at all yet absorb costs through pass-through rent increases — this is a diffuse-target case the derivation captures via victim declaration despite the absence of a direct contractual relationship.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope (not snare) matters because it preserves the coordination function this reading still concedes exists (genuine shared-infrastructure maintenance) while flagging that the same structure now carries asymmetric extraction requiring active enforcement (lien machinery, attorney-fee statutes, selective citation) to persist. Labeling this pure extraction (snare) would erase the real coordination problem the covenant also solves for compliant long-term owners; labeling it pure coordination (rope) would erase the documented fine-proliferation and fee-capture dynamics this reading is specifically about. The founding_problem/disappearance_verdict mismatch check applies here: founding_problem_status is authored 'contested' rather than 'dead' precisely because this reading does not claim the maintenance function has vanished — only that extraction has been layered onto it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indeterminacy_extraction_vs_coordination,
    'Is the covenant''s dominant structural function, at the level a court or regulator would assess, extraction (fine/fee capture) or coordination (shared-maintenance funding) — and is this indeterminacy resolvable, or does it vary association-by-association such that no single reading is the ''true'' one?',
    'Comparative audit of fine-schedule revenue as a share of total association budget versus documented shared-infrastructure capital expenditure, across a sample of associations, cross-referenced with foreclosure-initiation thresholds (dollar amount of underlying fine relative to litigation cost).',
    'If fine/fee revenue systematically exceeds documented maintenance need across most sampled associations, the extraction_reading generalizes beyond individual bad-actor boards to the instrument itself; if it is concentrated in a minority of associations, the extraction_reading is better modeled as a capture pathway available within the coordination_reading rather than a separate dominant reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy_extraction_vs_coordination, empirical, 'Whether extraction versus coordination is the covenant''s structurally dominant function or a variable capture pathway.').

omega_variable(
    board_capture_versus_structural_incentive,
    'Is the fine-proliferation and selective-enforcement pattern driven by individual board member self-dealing (a capture problem solvable by better board oversight) or by structural incentives built into management-firm fee models and attorney-fee statutes (a structural problem requiring legal reform)?',
    'Compare associations that rotate board membership frequently and have no management-firm relationship against those with entrenched boards and long-term management contracts; if fine-proliferation rates track structural features (management contract type, fee statute jurisdiction) more than board tenure, the structural hypothesis is favored.',
    'If structural, remedies must target management-firm fee models and attorney-fee statutes rather than individual board accountability, which changes what ''fixing'' this constraint would require.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(board_capture_versus_structural_incentive, conceptual, 'Whether extraction is driven by individual capture or by systemic fee-and-statute incentive structures.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hoa_covenant_scope__extraction_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hoa__tr_t0, hoa_covenant_scope__extraction_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(hoa__tr_t4, hoa_covenant_scope__extraction_reading, theater_ratio, 4, 0.25).
narrative_ontology:measurement(hoa__tr_t8, hoa_covenant_scope__extraction_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(hoa__tr_t12, hoa_covenant_scope__extraction_reading, theater_ratio, 12, 0.34).
narrative_ontology:measurement(hoa__tr_t16, hoa_covenant_scope__extraction_reading, theater_ratio, 16, 0.37).
narrative_ontology:measurement(hoa__tr_t20, hoa_covenant_scope__extraction_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(hoa__tr_t24, hoa_covenant_scope__extraction_reading, theater_ratio, 24, 0.42).

% Extraction over time
narrative_ontology:measurement(hoa__be_t0, hoa_covenant_scope__extraction_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(hoa__be_t4, hoa_covenant_scope__extraction_reading, base_extractiveness, 4, 0.41).
narrative_ontology:measurement(hoa__be_t8, hoa_covenant_scope__extraction_reading, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(hoa__be_t12, hoa_covenant_scope__extraction_reading, base_extractiveness, 12, 0.55).
narrative_ontology:measurement(hoa__be_t16, hoa_covenant_scope__extraction_reading, base_extractiveness, 16, 0.6).
narrative_ontology:measurement(hoa__be_t20, hoa_covenant_scope__extraction_reading, base_extractiveness, 20, 0.63).
narrative_ontology:measurement(hoa__be_t24, hoa_covenant_scope__extraction_reading, base_extractiveness, 24, 0.66).

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
narrative_ontology:boltzmann_floor_override(hoa_covenant_scope__extraction_reading, 0.12).
narrative_ontology:affects_constraint(hoa_covenant_scope__extraction_reading, hoa_covenant_scope__coordination_reading).
narrative_ontology:affects_constraint(hoa_covenant_scope__extraction_reading, hoa_covenant_scope__behavioral_control_reading).

% DUAL FORMULATION NOTE:
% Three constraints share the hoa_covenant_scope kernel: coordination_reading (genuine shared-infrastructure coordination, low-moderate ε), behavioral_control_reading (aesthetic/behavioral conformity enforcement, moderate ε), and this extraction_reading (revenue/power-consolidation via fine proliferation, high ε 0.66). Each carries its own beneficiary/victim structure and classification; they are linked here as siblings under one contested kernel, not merged into one constraint with a variable ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
