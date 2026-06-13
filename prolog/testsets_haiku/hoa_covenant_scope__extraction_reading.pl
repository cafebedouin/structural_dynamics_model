% ============================================================================
% CONSTRAINT STORY: hoa_covenant_scope__extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hoa_covenant_scope__extraction_reading
 *   human_readable: HOA Covenant Fine and Lien Extraction System
 *   domain: property_law/collective_governance
 *
 * SUMMARY:
 *   A homeowners association uses covenant enforcement as a revenue
 *   generation and board power consolidation mechanism. Covenants are written
 *   broadly and enforced selectively: board members and their allies see
 *   violations overlooked or resolved informally, while non-aligned
 *   homeowners face escalating fines, attorney fees, and lien processes.
 *   Property management firms and legal counsel benefit from enforcement
 *   volume and lien proceeds. Financially vulnerable homeowners and renters
 *   (who bear pass-through increases) face suppression from lien threats and
 *   foreclosure risk. The constraint is presented to homeowners as property
 *   value protection; this reading interprets it as rent extraction enabled
 *   by selective enforcement.
 *
 * KEY AGENTS:
 *   - board_members: agenda-setter and beneficiary; power is organized, exit is mobile — they can decline re-election or move to other communities, but the power position attracts repeat volunteers.
 *   - property_management_firms: beneficiary; power is powerful, exit is arbitrage — they operate across many HOAs and can reallocate to high-extraction communities.
 *   - legal_counsel_firms: beneficiary; power is powerful, exit is arbitrage — they bill hourly and retain contingent shares of lien proceeds.
 *   - financially_vulnerable_homeowners: victim; power is powerless, exit is trapped — they cannot exit without catastrophic loss; identity is locked to homeownership.
 *   - middle_income_homeowners: payer; power is moderate, exit is constrained — they can resist some enforcement but cannot change the board or covenant structure; exit via sale is costly.
 *   - renters_via_pass_through: victim; power is powerless, exit is constrained — they have no HOA seat and bear extraction invisible to them until rent increases.
 *   - state_legislators: observer; power is institutional, exit is analytical — they have authority to pass HOA reform laws but have not acted in most jurisdictions.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hoa_covenant_scope__extraction_reading, 0.68).
domain_priors:suppression_score(hoa_covenant_scope__extraction_reading, 0.81).
domain_priors:theater_ratio(hoa_covenant_scope__extraction_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(hoa_covenant_scope__extraction_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hoa_covenant_scope__extraction_reading, tangled_rope).
narrative_ontology:human_readable(hoa_covenant_scope__extraction_reading, "HOA Covenant Fine and Lien Extraction System").
narrative_ontology:topic_domain(hoa_covenant_scope__extraction_reading, "property_law/collective_governance").

domain_priors:requires_active_enforcement(hoa_covenant_scope__extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hoa_covenant_scope__extraction_reading, 'e7317e89-3675-4bd4-bd05-731803fd7fd1').
narrative_ontology:cs_kernel_codification('e7317e89-3675-4bd4-bd05-731803fd7fd1', formalized).
narrative_ontology:cs_authority_grounding('e7317e89-3675-4bd4-bd05-731803fd7fd1', extraction).
narrative_ontology:cs_interpretation_layer_present('e7317e89-3675-4bd4-bd05-731803fd7fd1').
narrative_ontology:cs_reading_relation('e7317e89-3675-4bd4-bd05-731803fd7fd1', hoa_covenant_scope__coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('e7317e89-3675-4bd4-bd05-731803fd7fd1', hoa_covenant_scope__behavioral_control_reading, coexists_with).
narrative_ontology:cs_axiom('e7317e89-3675-4bd4-bd05-731803fd7fd1', foundational, covenant_enforces_revenue_collection).
narrative_ontology:cs_axiom_status(covenant_enforces_revenue_collection, holdable).
narrative_ontology:cs_axiom_grounding('e7317e89-3675-4bd4-bd05-731803fd7fd1', covenant_enforces_revenue_collection, empirically_contingent).
narrative_ontology:cs_axiom('e7317e89-3675-4bd4-bd05-731803fd7fd1', foundational, selective_enforcement_maximizes_board_power).
narrative_ontology:cs_axiom_status(selective_enforcement_maximizes_board_power, holdable).
narrative_ontology:cs_axiom_grounding('e7317e89-3675-4bd4-bd05-731803fd7fd1', selective_enforcement_maximizes_board_power, empirically_contingent).
narrative_ontology:cs_reference_frame('e7317e89-3675-4bd4-bd05-731803fd7fd1', property_maintenance_coordination_framework).
narrative_ontology:cs_drift_state('e7317e89-3675-4bd4-bd05-731803fd7fd1', contemporary_fine_proliferation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e7317e89-3675-4bd4-bd05-731803fd7fd1', '').
narrative_ontology:cs_kernel_id(hoa_covenant_scope__extraction_reading, hoa_covenant_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__extraction_reading, board_members).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__extraction_reading, property_management_firms).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__extraction_reading, legal_counsel_firms).
narrative_ontology:constraint_victim(hoa_covenant_scope__extraction_reading, financially_vulnerable_homeowners).
narrative_ontology:constraint_victim(hoa_covenant_scope__extraction_reading, renters_via_pass_through).
narrative_ontology:constraint_victim(hoa_covenant_scope__extraction_reading, middle_income_homeowners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__extraction_reading, homeowners_board_allies).
narrative_ontology:constraint_victim(hoa_covenant_scope__extraction_reading, homeowners_board_allies).
narrative_ontology:constraint_vindicates(hoa_covenant_scope__extraction_reading, board_fiduciary_discretion).
narrative_ontology:constraint_vindicates(hoa_covenant_scope__extraction_reading, property_value_protection_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Elected or self-perpetuating volunteers who set covenant enforcement priorities, approve enforcement budgets, and vote on fine schedules. They benefit from selective enforcement that targets high-fine violations while overlooking violations by board allies. Many are property investors whose own properties benefit from board-directed enforcement patterns. Can decline re-election or move to other communities.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, board_members, agenda_setter,
    organized, biographical, mobile, local).
narrative_ontology:stakeholder_secondary_role(hoa_covenant_scope__extraction_reading, board_members, beneficiary).

% Hired to administer covenant enforcement. Collect management fees tied to collected fines, have discretion over which violations to report, and recommend fine schedules. Revenue grows with enforcement intensity. Operate across multiple HOAs and can reallocate resources to high-extraction communities.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, property_management_firms, beneficiary,
    powerful, generational, arbitrage, regional).

% Retained to draft covenant language, advise on enforcement procedures, and pursue lien and foreclosure actions. Bill hourly for enforcement work and receive contingent shares of lien proceeds. Benefit from aggressive enforcement generating litigation volume. Can shift to other practice areas or communities.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, legal_counsel_firms, beneficiary,
    powerful, generational, arbitrage, regional).

% Own property in the HOA but lack resources to dispute fines or hire legal counsel. Face accelerating fine schedules, attorney fee additions, and lien processes that threaten foreclosure. Cannot afford legal representation. Exit is foreclosure or sale at distressed prices. Identity is locked to homeownership and community stability.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, financially_vulnerable_homeowners, payer,
    powerless, biographical, identity_locked, local).

% Rent from HOA homeowners whose covenant violations or board assessments are passed through as rent increases. Bear extraction cost but have no governance seat. Exit is to rent elsewhere, but extraction is invisible to them until rent hike arrives.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, renters_via_pass_through, payer,
    powerless, immediate, constrained, local).

% Own property and can afford some legal counsel, but face the same fine structure and enforcement patterns as vulnerable neighbors. Have resources to resist some actions but lack power to change board or covenant. Exit via sale is costly because enforcement patterns depress property values.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, middle_income_homeowners, payer,
    moderate, biographical, constrained, local).

% Own property and have relationships with board members or political alignment sufficient to avoid selective enforcement. Their violations are overlooked or resolved informally while similar violations by non-allies trigger fines and liens. Benefit from selective exemption and targeted property value protection. Formally payers but factually beneficiaries.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, homeowners_board_allies, beneficiary,
    moderate, biographical, mobile, local).
narrative_ontology:stakeholder_secondary_role(hoa_covenant_scope__extraction_reading, homeowners_board_allies, payer).

% Non-profit organizations and legal reformers advocating for mandatory alternative dispute resolution, homeowner bill-of-rights protections, capped fine schedules, and enforcement transparency. Would redesign covenant enforcement to require neutral arbitration and mandatory homeowner voting on budgets. Excluded from HOA governance; influence is legislative and advocacy-based.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, homeowner_reform_advocates, excluded,
    analytical, generational, analytical, national).

% Observe covenant enforcement patterns through constituent complaints and reform advocates; have authority to pass laws requiring HOA transparency, capping fines, or mandating homeowner voting. Have not acted in most jurisdictions despite documented abuse. Structurally aligned with property rights framing and institutional deference.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__extraction_reading, state_legislators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hoa_covenant_scope__extraction_reading, board_members).
narrative_ontology:fixing_cost_class(hoa_covenant_scope__extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a shared interest: preventing severe deterioration of properties that would reduce collective property values and create externalities (deferred maintenance, hazardous conditions). Genuine coordination problem: without some mechanism to enforce upkeep, free-riding homeowners could degrade the community.
% TRANSFER_FUNCTION: Moves money from homeowners (via fines, liens, attorney fees, and increased assessments) to board members (via selective enforcement exemptions and property value protection), property management firms (via management fees tied to fine collection), and legal counsel (via litigation volume and contingent shares). Renters pay via rent increases passed through by landlord-members. The transfer is contingent on selective enforcement patterns that exempt board allies.
% ABSENT_VOICES: Renters subject to pass-through extraction are excluded from HOA governance. State legislatures and alternative dispute resolution advocates would argue for mandatory homeowner voting on enforcement budgets, capped fine schedules, and neutral arbitration of disputes. Financially vulnerable homeowners lack resources to organize collective challenges. Their absence is structural — renters have no legal seat; vulnerable owners are individually weak.
% DISAPPEARANCE_RATIONALE: If covenant enforcement and selective fine structures vanished, property management fees would collapse, legal counsel would lose enforcement retainers, board members would lose selective exemption benefits, and property values would initially decline. The community would reorganize around looser, transparent enforcement or mandatory homeowner voting on enforcement budgets. Renters would see stable rent. The extraction mechanism would disappear but the coordination problem (shared maintenance need) would remain to be solved by a different structure.
% FOUNDING_PROBLEM: Early HOAs were built to solve genuine coordination problems: shared amenities (pools, clubhouses), common infrastructure (roads, landscaping), and property value protection against neighborhood deterioration. Without some enforcement mechanism, free-riding was a real risk.
% FOUNDING_PROBLEM_CORROBORATION: Board members and property management firms attest the founding problem remains live — properties still deteriorate without enforcement, they argue. Homeowner advocates, alternative dispute resolution non-profits, and state regulatory analysts attest the problem is substantially solved in well-maintained communities and the apparatus now functions as pure extraction. Legal scholarship on HOA capture (outside the benefiting parties) documents the shift from maintenance coordination to fine proliferation.
narrative_ontology:disappearance_verdict(hoa_covenant_scope__extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(hoa_covenant_scope__extraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hoa_covenant_scope__extraction_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(hoa_covenant_scope__extraction_reading, 'none', 1).

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
 *   Extraction is high (0.68) because the constraint moves money from homeowners to board members (via selective exemption), management firms (via management fees tied to fine collection), and legal counsel (via enforcement retainers and contingent shares). Suppression is very high (0.81) because the threat of liens and foreclosure prevents homeowners from challenging enforcement — the legal process itself is the suppression mechanism. Theater ratio rises over the interval (0.28 to 0.52) as enforcement activity increasingly targets revenue generation (fine proliferation) rather than genuine maintenance coordination. Accessibility collapse is moderate (0.48) because homeowners retain the option to sell or contest fines in court, but at prohibitive cost — the alternatives do not disappear, they become inaccessible. The measurement series trace the trajectory: extractiveness and suppression rise as the constraint matures; theater ratio rises as maintenance coordination work is displaced by fine machinery. All metrics are measured on a single shared time grid (every metric at every time point) so the lifecycle is coherent.
 *
 * PERSPECTIVAL GAP:
 *   Board members and management firms perceive the constraint as necessary property value protection and legitimate community governance. They author fines as corrective, not extractive. Financially vulnerable homeowners perceive the constraint as a legal threat that preys on honest mistakes or minor infractions and uses foreclosure as extortion to extract payment. State legislators perceive the constraint as a private governance issue requiring deference to property rights, not intervention. The engine computes this divergence from the structural data: board members' d is near the beneficiary end (they control the rule, see exemptions); vulnerable homeowners' d is near the target end (they are powerless and trapped). The authored claim is tangled_rope (genuine coordination function + asymmetric extraction); the metrics describe substantially extractive operation. This divergence is the measurement the corpus exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Board members are beneficiaries and agenda-setters: they set enforcement priorities, approve fine schedules, and benefit from selective enforcement that targets non-allies. Their directionality d is near 0.1-0.2 (far beneficiary end) because they control the constraint, see personal exemptions, and have mobile exit. Property management firms and legal counsel have d near 0.15-0.25 (beneficiary end) because they collect fees and have arbitrage exit. Financially vulnerable homeowners have d near 0.85-0.95 (far target end) because they are powerless, trapped, and subject to escalating enforcement. Middle-income homeowners have d near 0.65-0.75 (target end, but less extreme) because they can afford some legal defense and have partial exit via sale (costly but possible). Homeowners with board connections have d near 0.25-0.35 (beneficiary side, via selective exemption) despite being formally payers. Renters have d near 0.80-0.90 (far target end) because they bear extraction invisible to them and have constrained exit to other rentals in the same HOA or different communities. No directionality override is needed — the structural derivation from beneficiary/victim declarations and exit options produces the right d for each seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (coordination of shared maintenance) is live, but the constraint's primary function has shifted to revenue generation. The measurement series show this: extractiveness rises from 0.38 to 0.68 while theater ratio rises from 0.28 to 0.52, indicating that enforcement work is increasingly devoted to revenue machinery (fine processing, lien filing) rather than property maintenance coordination. The constraint exhibits mandatrophy (mandate outlived function) without satisfying the piton definition — it is not mostly performance (theater at 0.52 suggests real extraction work, not theatrical maintenance). The distinction: a piton is abandoned by its builders and maintained by inertia; this constraint is actively rebuilt and expanded by beneficiaries who profit from extraction. It is tangled_rope (genuine coordination function + asymmetric extraction) that has evolved into extraction dominance, not a piton. The governance structure still claims coordination legitimacy ('property value protection') but deploys the machinery for extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    selective_enforcement_mechanism,
    'Is the selective enforcement of covenants a deliberate strategy by the board and management firms, or an artifact of inconsistent application?',
    'Audit of enforcement records across properties: statistical analysis of violation reporting, fine amounts, and lien filing patterns by property address, owner demographics, and board member proximity. Testimony from property management staff about enforcement decision-making.',
    'Deliberate strategy strengthens the extraction reading and supports mandatory transparency requirements; artifact-level inconsistency weakens the extraction reading and supports remediation training for management. The reading assumes deliberate strategy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(selective_enforcement_mechanism, empirical, 'Whether selective enforcement is intentional extraction or procedural inconsistency.').

omega_variable(
    coordination_extraction_boundary,
    'What share of covenant enforcement activity is directed at genuine maintenance coordination (preventing deterioration that affects collective property values) versus rent extraction (fines, liens, fees on violations unrelated to collective harm)?',
    'Classification of all active enforcement cases into genuine-coordination and rent-extraction categories by independent evaluators; analysis of fine schedules against estimated remediation costs; tracking of lien and foreclosure outcomes.',
    'High genuine-coordination share (>60%) would support a coordination reading of the constraint and suggest the extraction is incidental; low share (<30%) would consolidate the extraction reading. The reading assumes low share (10-25%).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, empirical, 'Proportion of enforcement targeting genuine shared externalities versus rent-seeking violations.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is the measured suppression (0.81) structural (legal processes, lien threats, foreclosure risk) or internalized (homeowners have internalized the board''s authority and fear social sanction more than legal process)?',
    'Post-exit analysis: when homeowners exit the HOA (via sale or relocation), does suppression persist (belief in board legitimacy remains) or evaporate (suppression was structural)? Qualitative interviews with former homeowners and displaced renters about their lived experience of enforcement.',
    'Structural suppression is more amenable to legal remedy (reform the lien process, cap fines); internalized suppression requires longer-term cultural change. If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the constraint travels with the person after exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Whether suppression is structural legal process or internalized belief in board authority.').

omega_variable(
    board_composition_capture,
    'Is the board genuinely representative of the homeowner population, or has it been captured by property investors and management-firm allies who use board seats to maximize extraction?',
    'Demographic and economic analysis of board members versus general homeowner population; tracking of board election patterns and incumbent re-election rates; analysis of board members'' property portfolios and relationships to management firms and legal counsel.',
    'Capture would consolidate the extraction reading and support mandatory homeowner voting on enforcement budgets and board composition requirements. Non-capture would suggest the board reflects community preferences (even if those preferences are extractive). The reading assumes capture.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(board_composition_capture, empirical, 'Whether the board is captured by investors and management-firm allies or representative of homeowners.').

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading of a contested kernel (hoa_covenant_scope). Three readings instantiate different interpretations of the same underlying covenant mechanism: coordination_reading (covenants solve genuine externalities), behavioral_control_reading (covenants enforce aesthetic conformity), and extraction_reading (this one — covenants are revenue generation). Which reading best explains the observed enforcement patterns?',
    'Empirical classification: does enforcement correlate with (a) maintenance impact on property values (coordination hypothesis), (b) visual/aesthetic conformity (behavioral control hypothesis), or (c) revenue/lien generation unrelated to property deterioration (extraction hypothesis)? Analysis of enforcement against these three independent variables.',
    'The reading claims extraction dominates; strong correlation with revenue patterns and weak correlation with either maintenance or aesthetic factors would consolidate the reading. Competing empirical support would indicate the other readings capture different aspects of the same constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, empirical, 'Whether covenant enforcement patterns match extraction, coordination, or behavioral-control hypotheses.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hoa_covenant_scope__extraction_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hoa__tr_t0, hoa_covenant_scope__extraction_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(hoa__tr_t0, observed).
narrative_ontology:measurement(hoa__tr_t5, hoa_covenant_scope__extraction_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement_basis(hoa__tr_t5, observed).
narrative_ontology:measurement(hoa__tr_t10, hoa_covenant_scope__extraction_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement_basis(hoa__tr_t10, observed).
narrative_ontology:measurement(hoa__tr_t15, hoa_covenant_scope__extraction_reading, theater_ratio, 15, 0.41).
narrative_ontology:measurement_basis(hoa__tr_t15, observed).
narrative_ontology:measurement(hoa__tr_t20, hoa_covenant_scope__extraction_reading, theater_ratio, 20, 0.46).
narrative_ontology:measurement_basis(hoa__tr_t20, observed).
narrative_ontology:measurement(hoa__tr_t25, hoa_covenant_scope__extraction_reading, theater_ratio, 25, 0.5).
narrative_ontology:measurement_basis(hoa__tr_t25, observed).
narrative_ontology:measurement(hoa__tr_t30, hoa_covenant_scope__extraction_reading, theater_ratio, 30, 0.51).
narrative_ontology:measurement_basis(hoa__tr_t30, observed).
narrative_ontology:measurement(hoa__tr_t35, hoa_covenant_scope__extraction_reading, theater_ratio, 35, 0.52).
narrative_ontology:measurement_basis(hoa__tr_t35, observed).

% Extraction over time
narrative_ontology:measurement(hoa__be_t0, hoa_covenant_scope__extraction_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(hoa__be_t0, observed).
narrative_ontology:measurement(hoa__be_t5, hoa_covenant_scope__extraction_reading, base_extractiveness, 5, 0.45).
narrative_ontology:measurement_basis(hoa__be_t5, observed).
narrative_ontology:measurement(hoa__be_t10, hoa_covenant_scope__extraction_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement_basis(hoa__be_t10, observed).
narrative_ontology:measurement(hoa__be_t15, hoa_covenant_scope__extraction_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement_basis(hoa__be_t15, observed).
narrative_ontology:measurement(hoa__be_t20, hoa_covenant_scope__extraction_reading, base_extractiveness, 20, 0.63).
narrative_ontology:measurement_basis(hoa__be_t20, observed).
narrative_ontology:measurement(hoa__be_t25, hoa_covenant_scope__extraction_reading, base_extractiveness, 25, 0.66).
narrative_ontology:measurement_basis(hoa__be_t25, observed).
narrative_ontology:measurement(hoa__be_t30, hoa_covenant_scope__extraction_reading, base_extractiveness, 30, 0.67).
narrative_ontology:measurement_basis(hoa__be_t30, observed).
narrative_ontology:measurement(hoa__be_t35, hoa_covenant_scope__extraction_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement_basis(hoa__be_t35, observed).

% Suppression requirement over time
narrative_ontology:measurement(hoa__su_t0, hoa_covenant_scope__extraction_reading, suppression_requirement, 0, 0.54).
narrative_ontology:measurement_basis(hoa__su_t0, observed).
narrative_ontology:measurement(hoa__su_t5, hoa_covenant_scope__extraction_reading, suppression_requirement, 5, 0.61).
narrative_ontology:measurement_basis(hoa__su_t5, observed).
narrative_ontology:measurement(hoa__su_t10, hoa_covenant_scope__extraction_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement_basis(hoa__su_t10, observed).
narrative_ontology:measurement(hoa__su_t15, hoa_covenant_scope__extraction_reading, suppression_requirement, 15, 0.73).
narrative_ontology:measurement_basis(hoa__su_t15, observed).
narrative_ontology:measurement(hoa__su_t20, hoa_covenant_scope__extraction_reading, suppression_requirement, 20, 0.77).
narrative_ontology:measurement_basis(hoa__su_t20, observed).
narrative_ontology:measurement(hoa__su_t25, hoa_covenant_scope__extraction_reading, suppression_requirement, 25, 0.79).
narrative_ontology:measurement_basis(hoa__su_t25, observed).
narrative_ontology:measurement(hoa__su_t30, hoa_covenant_scope__extraction_reading, suppression_requirement, 30, 0.8).
narrative_ontology:measurement_basis(hoa__su_t30, observed).
narrative_ontology:measurement(hoa__su_t35, hoa_covenant_scope__extraction_reading, suppression_requirement, 35, 0.81).
narrative_ontology:measurement_basis(hoa__su_t35, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=35
narrative_ontology:measurement(hoa__grid_01, hoa_covenant_scope__extraction_reading, accessibility_collapse(class), 0, 0.42).
narrative_ontology:measurement(hoa__grid_02, hoa_covenant_scope__extraction_reading, accessibility_collapse(class), 35, 0.58).
narrative_ontology:measurement(hoa__grid_03, hoa_covenant_scope__extraction_reading, accessibility_collapse(individual), 0, 0.35).
narrative_ontology:measurement(hoa__grid_04, hoa_covenant_scope__extraction_reading, accessibility_collapse(individual), 35, 0.52).
narrative_ontology:measurement(hoa__grid_05, hoa_covenant_scope__extraction_reading, accessibility_collapse(organizational), 0, 0.28).
narrative_ontology:measurement(hoa__grid_06, hoa_covenant_scope__extraction_reading, accessibility_collapse(organizational), 35, 0.41).
narrative_ontology:measurement(hoa__grid_07, hoa_covenant_scope__extraction_reading, accessibility_collapse(structural), 0, 0.55).
narrative_ontology:measurement(hoa__grid_08, hoa_covenant_scope__extraction_reading, accessibility_collapse(structural), 35, 0.68).
narrative_ontology:measurement(hoa__grid_09, hoa_covenant_scope__extraction_reading, resistance(class), 0, 0.61).
narrative_ontology:measurement(hoa__grid_10, hoa_covenant_scope__extraction_reading, resistance(class), 35, 0.74).
narrative_ontology:measurement(hoa__grid_11, hoa_covenant_scope__extraction_reading, resistance(individual), 0, 0.68).
narrative_ontology:measurement(hoa__grid_12, hoa_covenant_scope__extraction_reading, resistance(individual), 35, 0.71).
narrative_ontology:measurement(hoa__grid_13, hoa_covenant_scope__extraction_reading, resistance(organizational), 0, 0.42).
narrative_ontology:measurement(hoa__grid_14, hoa_covenant_scope__extraction_reading, resistance(organizational), 35, 0.48).
narrative_ontology:measurement(hoa__grid_15, hoa_covenant_scope__extraction_reading, resistance(structural), 0, 0.55).
narrative_ontology:measurement(hoa__grid_16, hoa_covenant_scope__extraction_reading, resistance(structural), 35, 0.68).
narrative_ontology:measurement(hoa__grid_17, hoa_covenant_scope__extraction_reading, stakes_inflation(class), 0, 0.51).
narrative_ontology:measurement(hoa__grid_18, hoa_covenant_scope__extraction_reading, stakes_inflation(class), 35, 0.71).
narrative_ontology:measurement(hoa__grid_19, hoa_covenant_scope__extraction_reading, stakes_inflation(individual), 0, 0.48).
narrative_ontology:measurement(hoa__grid_20, hoa_covenant_scope__extraction_reading, stakes_inflation(individual), 35, 0.74).
narrative_ontology:measurement(hoa__grid_21, hoa_covenant_scope__extraction_reading, stakes_inflation(organizational), 0, 0.32).
narrative_ontology:measurement(hoa__grid_22, hoa_covenant_scope__extraction_reading, stakes_inflation(organizational), 35, 0.51).
narrative_ontology:measurement(hoa__grid_23, hoa_covenant_scope__extraction_reading, stakes_inflation(structural), 0, 0.6).
narrative_ontology:measurement(hoa__grid_24, hoa_covenant_scope__extraction_reading, stakes_inflation(structural), 35, 0.78).
narrative_ontology:measurement(hoa__grid_25, hoa_covenant_scope__extraction_reading, suppression(class), 0, 0.56).
narrative_ontology:measurement(hoa__grid_26, hoa_covenant_scope__extraction_reading, suppression(class), 35, 0.81).
narrative_ontology:measurement(hoa__grid_27, hoa_covenant_scope__extraction_reading, suppression(individual), 0, 0.52).
narrative_ontology:measurement(hoa__grid_28, hoa_covenant_scope__extraction_reading, suppression(individual), 35, 0.79).
narrative_ontology:measurement(hoa__grid_29, hoa_covenant_scope__extraction_reading, suppression(organizational), 0, 0.38).
narrative_ontology:measurement(hoa__grid_30, hoa_covenant_scope__extraction_reading, suppression(organizational), 35, 0.62).
narrative_ontology:measurement(hoa__grid_31, hoa_covenant_scope__extraction_reading, suppression(structural), 0, 0.62).
narrative_ontology:measurement(hoa__grid_32, hoa_covenant_scope__extraction_reading, suppression(structural), 35, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hoa_covenant_scope__extraction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(hoa_covenant_scope__extraction_reading, 0.12).
narrative_ontology:affects_constraint(hoa_covenant_scope__extraction_reading, hoa_covenant_scope__coordination_reading).
narrative_ontology:affects_constraint(hoa_covenant_scope__extraction_reading, hoa_covenant_scope__behavioral_control_reading).

% DUAL FORMULATION NOTE:
% The hoa_covenant_scope kernel has three constraint stories corresponding to three readings of the same underlying covenant mechanism. The extraction_reading (this constraint) claims covenants function primarily as revenue generation via fine proliferation and selective enforcement, with extractiveness ~0.68. The coordination_reading claims covenants solve genuine externalities (shared maintenance, property deterioration prevention) with lower extractiveness and genuine beneficiary-to-all structure. The behavioral_control_reading claims covenants enforce aesthetic conformity as property value maximization with moderate extractiveness concentrated on aesthetic targets. Each reading has its own ε, beneficiary/victim structure, and classification. The three stories are linked via network.affects_constraints to signal family kinship. Decomposition is justified by ε-invariance: the observable used to measure the constraint changes the resulting ε (maintenance coordination vs. fine proliferation vs. aesthetic conformity are three structurally distinct observables that produce three different ε values). The extraction_reading measures extractiveness under the observable 'fine generation and lien filing patterns'; the coordination_reading measures it under 'maintenance coordination and prevention of deterioration'; the behavioral_control_reading measures it under 'aesthetic enforcement targeting'. No single ε can satisfy all three observables — therefore three constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hoa_covenant_scope__extraction_reading, powerless, 0.88).
constraint_indexing:directionality_override(hoa_covenant_scope__extraction_reading, moderate, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
