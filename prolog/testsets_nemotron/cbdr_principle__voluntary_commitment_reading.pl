% ============================================================================
% CONSTRAINT STORY: cbdr_principle__voluntary_commitment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cbdr_principle__voluntary_commitment_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: cbdr_principle__voluntary_commitment_reading
 *   human_readable: CBDR Voluntary Commitment Reading — Nationally Determined Contributions with Technology Transfer
 *   domain: international_climate_governance/treaty_law/development_economics
 *
 * SUMMARY:
 *   The CBDR principle has two structurally distinct readings. This
 *   constraint story captures the voluntary_commitment_reading: CBDR is
 *   interpreted as requiring only voluntary, nationally determined
 *   contributions (NDCs) with technology transfer as the primary
 *   developed-nation obligation — implemented through the Paris Agreement's
 *   pledge-and-review architecture. The historical_responsibility_reading (a
 *   separate constraint story) reads CBDR as requiring binding emissions
 *   reductions from developed nations proportional to cumulative historical
 *   emissions plus loss/damage financing. The voluntary reading emerged from
 *   the Kyoto Protocol's collapse and was cemented at Paris 2015. It presents
 *   itself as a coordination mechanism (universal participation, directional
 *   support) but operates as a Tangled Rope: genuine coordination function
 *   (universal NDC submission) coexists with asymmetric extraction (developed
 *   nations avoid binding constraints while developing nations bear
 *   adaptation costs without guaranteed compensation). The extraction has
 *   accumulated over the interval as NDC ambition gaps widened, technology
 *   transfer remained commercially mediated, and loss/damage was excluded
 *   from obligation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cbdr_principle__voluntary_commitment_reading, 0.62).
domain_priors:suppression_score(cbdr_principle__voluntary_commitment_reading, 0.58).
domain_priors:theater_ratio(cbdr_principle__voluntary_commitment_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cbdr_principle__voluntary_commitment_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(cbdr_principle__voluntary_commitment_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(cbdr_principle__voluntary_commitment_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cbdr_principle__voluntary_commitment_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(cbdr_principle__voluntary_commitment_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cbdr_principle__voluntary_commitment_reading, tangled_rope).
narrative_ontology:human_readable(cbdr_principle__voluntary_commitment_reading, "CBDR Voluntary Commitment Reading — Nationally Determined Contributions with Technology Transfer").
narrative_ontology:topic_domain(cbdr_principle__voluntary_commitment_reading, "international_climate_governance/treaty_law/development_economics").

domain_priors:requires_active_enforcement(cbdr_principle__voluntary_commitment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(cbdr_principle__voluntary_commitment_reading, 'a2ec938d-5651-4395-93ae-9e3f84a523b0').
narrative_ontology:cs_kernel_codification('a2ec938d-5651-4395-93ae-9e3f84a523b0', formalized).
narrative_ontology:cs_authority_grounding('a2ec938d-5651-4395-93ae-9e3f84a523b0', lineage).
narrative_ontology:cs_interpretation_layer_present('a2ec938d-5651-4395-93ae-9e3f84a523b0').
narrative_ontology:cs_reading_relation('a2ec938d-5651-4395-93ae-9e3f84a523b0', cbdr_principle__historical_responsibility_reading, coexists_with).
narrative_ontology:cs_axiom('a2ec938d-5651-4395-93ae-9e3f84a523b0', foundational, national_sovereignty_over_mitigation_ambition).
narrative_ontology:cs_axiom_status(national_sovereignty_over_mitigation_ambition, holdable).
narrative_ontology:cs_axiom_grounding('a2ec938d-5651-4395-93ae-9e3f84a523b0', national_sovereignty_over_mitigation_ambition, conventional).
narrative_ontology:cs_axiom('a2ec938d-5651-4395-93ae-9e3f84a523b0', foundational, technology_transfer_as_cooperative_not_compensatory).
narrative_ontology:cs_axiom_status(technology_transfer_as_cooperative_not_compensatory, holdable).
narrative_ontology:cs_axiom_grounding('a2ec938d-5651-4395-93ae-9e3f84a523b0', technology_transfer_as_cooperative_not_compensatory, conventional).
narrative_ontology:cs_axiom('a2ec938d-5651-4395-93ae-9e3f84a523b0', secondary, universal_participation_over_adequacy).
narrative_ontology:cs_axiom_status(universal_participation_over_adequacy, holdable).
narrative_ontology:cs_axiom_grounding('a2ec938d-5651-4395-93ae-9e3f84a523b0', universal_participation_over_adequacy, instrumental).
narrative_ontology:cs_reference_frame('a2ec938d-5651-4395-93ae-9e3f84a523b0', kyoto_protocol_binding_differentiation).
narrative_ontology:cs_drift_state('a2ec938d-5651-4395-93ae-9e3f84a523b0', paris_agreement_ndc_architecture, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a2ec938d-5651-4395-93ae-9e3f84a523b0', '').
narrative_ontology:cs_kernel_id(cbdr_principle__voluntary_commitment_reading, cbdr_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cbdr_principle__voluntary_commitment_reading, developed_nation_governments).
narrative_ontology:constraint_beneficiary(cbdr_principle__voluntary_commitment_reading, multinational_technology_firms).
narrative_ontology:constraint_beneficiary(cbdr_principle__voluntary_commitment_reading, international_finance_institutions).
narrative_ontology:constraint_victim(cbdr_principle__voluntary_commitment_reading, least_developed_countries).
narrative_ontology:constraint_victim(cbdr_principle__voluntary_commitment_reading, small_island_developing_states).
narrative_ontology:constraint_victim(cbdr_principle__voluntary_commitment_reading, climate_vulnerable_developing_nations).
narrative_ontology:constraint_vindicates(cbdr_principle__voluntary_commitment_reading, national_sovereignty_in_climate_action).
narrative_ontology:constraint_vindicates(cbdr_principle__voluntary_commitment_reading, technology_transfer_as_development_cooperation).
narrative_ontology:constraint_vindicates(cbdr_principle__voluntary_commitment_reading, common_but_differentiated_responsibilities_voluntary).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the terms of NDC submissions, define technology transfer frameworks, and control climate finance architecture. They benefit from avoiding binding emissions constraints while maintaining influence over global climate governance. Exit is near-arbitrage — they can withdraw from processes without domestic political cost and face no structural penalty for non-compliance with voluntary pledges.
narrative_ontology:constraint_stakeholder(cbdr_principle__voluntary_commitment_reading, developed_nation_governments, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(cbdr_principle__voluntary_commitment_reading, developed_nation_governments, beneficiary).

% Capture technology transfer finance streams (GEF, GCF, bilateral funds) through IP licensing, joint ventures, and consultancy contracts. The voluntary framework treats technology as a commercial good rather than a public obligation, ensuring revenue extraction. They can redirect capital across jurisdictions and sectors if climate finance terms become unfavorable.
narrative_ontology:constraint_stakeholder(cbdr_principle__voluntary_commitment_reading, multinational_technology_firms, beneficiary,
    powerful, biographical, mobile, global).

% Administer climate finance mechanisms (World Bank, regional development banks, GCF) that channel technology transfer funding. They collect management fees, set conditionalities, and expand institutional mandate without binding accountability for outcomes. Exit is costless — they control the architecture and face no penalty for failing to deliver adaptation finance.
narrative_ontology:constraint_stakeholder(cbdr_principle__voluntary_commitment_reading, international_finance_institutions, beneficiary,
    institutional, generational, arbitrage, global).

% Bear disproportionate adaptation costs with no compensation guarantee under the voluntary framework. They must submit NDCs despite minimal historical emissions, depend on technology transfer promises that are commercially mediated and unreliable, and have no exit from climate impacts. Their negotiating leverage is near-zero; they cannot credibly threaten withdrawal because they need the limited finance on offer.
narrative_ontology:constraint_stakeholder(cbdr_principle__voluntary_commitment_reading, least_developed_countries, payer,
    powerless, generational, trapped, global).

% Face existential adaptation costs (relocation, territorial loss) while the voluntary framework treats loss and damage as a dialog, not an obligation. They are structurally excluded from meaningful influence on NDC ambition mechanisms and technology transfer terms. Exit is physically impossible — territory loss is irreversible and they cannot relocate their sovereign status.
narrative_ontology:constraint_stakeholder(cbdr_principle__voluntary_commitment_reading, small_island_developing_states, payer,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_secondary_role(cbdr_principle__voluntary_commitment_reading, small_island_developing_states, excluded).

% Submit NDCs requiring conditional mitigation they cannot afford without technology transfer, then bear adaptation costs when transfers fail to materialize. They have some coalition power (G77, V20) but exit options are constrained by development finance dependence. They can threaten non-participation but face aid and trade retaliation.
narrative_ontology:constraint_stakeholder(cbdr_principle__voluntary_commitment_reading, climate_vulnerable_developing_nations, payer,
    moderate, biographical, constrained, global).

% Advocate for historical responsibility and loss/damage finance but are excluded from formal negotiation rooms where NDC rules and technology transfer terms are set. They can mobilize public pressure and litigation but have no formal seat at the table. Exit means shifting to outside-pressure tactics (litigation, divestment, direct action).
narrative_ontology:constraint_stakeholder(cbdr_principle__voluntary_commitment_reading, civil_society_climate_justice_networks, excluded,
    organized, biographical, mobile, global).

% Analyze the evolution of CBDR from binding differentiated obligations to voluntary NDCs, document the gap between technology transfer promises and delivery, and track the doctrinal shift in treaty interpretation. They have no stake in outcomes but shape the epistemic framework through which the constraint is understood.
narrative_ontology:constraint_stakeholder(cbdr_principle__voluntary_commitment_reading, academic_legal_scholars, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global climate action by establishing a universal participation framework (all nations submit NDCs) and a directional obligation for developed nations to support developing nations through technology transfer and finance — solving the collective action problem of universal engagement without binding enforcement.
% TRANSFER_FUNCTION: Moves the burden of mitigation implementation and adaptation costs from developed nations to developing nations, while moving technology transfer finance from public obligation to commercially mediated channels — effectively transferring climate risk to the most vulnerable and financial returns to technology holders and finance intermediaries.
% ABSENT_VOICES: Future generations (who bear the cumulative cost of insufficient mitigation), indigenous peoples in vulnerable territories (excluded from NDC design and technology transfer governance), and the historical responsibility reading's constituencies (who would demand binding developed-nation reductions and loss/damage finance) are structurally absent from the voluntary commitment architecture.
% DISAPPEARANCE_RATIONALE: If the voluntary NDC framework vanished overnight, the Paris Agreement's universal participation architecture would collapse, developed nations would face immediate pressure for binding commitments under the historical responsibility reading, climate finance flows would lose their institutional channels, and developing nations would lose even the limited technology transfer and adaptation finance currently pledged — the world would reorganize around either a binding top-down regime or fragmented bilateral arrangements.
% FOUNDING_PROBLEM: The Kyoto Protocol's binding developed-nation targets failed to achieve universal participation (US non-ratification, Canada withdrawal, developing nation exemption) and collapsed under its own rigidity. The voluntary NDC framework was built to solve the participation problem by allowing self-differentiation and avoiding the enforcement trap that killed Kyoto.
% FOUNDING_PROBLEM_CORROBORATION: Developed nations and UNFCCC secretariat attest the participation problem is live — universal NDC submission proves the framework works. Climate justice networks, LDC negotiators, and independent legal scholars (e.g., Rajamani, Mayer, Voigt) attest the participation problem was a false frame — the real problem was developed nation non-compliance, which the voluntary framework legitimized by converting obligations into aspirations. The IPCC AR6 WGIII assessment corroborates that voluntary NDCs are insufficient for 1.5°C/2°C pathways, confirming the founding problem's solution created a deeper adequacy crisis.
narrative_ontology:disappearance_verdict(cbdr_principle__voluntary_commitment_reading, world_rearranges).
narrative_ontology:founding_problem_status(cbdr_principle__voluntary_commitment_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(cbdr_principle__voluntary_commitment_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(cbdr_principle__voluntary_commitment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(cbdr_principle__voluntary_commitment_reading, 0.62, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cbdr_principle__voluntary_commitment_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(cbdr_principle__voluntary_commitment_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(cbdr_principle__voluntary_commitment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.62) reflects the gap between developed nations' voluntary pledges and their fair-share mitigation burden, plus the commercialization of technology transfer that extracts rent from adaptation finance. Suppression (0.58) captures the structural exclusion of loss/damage from obligation, the non-binding nature of NDCs that prevents accountability, and the commercial IP regime that gates technology access. Theater ratio (0.41) measures the growing performative gap: global stocktakes, transparency frameworks, and technology mechanisms create appearance of coordination while extraction accumulates. Accessibility collapse (0.38) is moderate — alternatives (binding top-down, climate clubs, carbon pricing) remain conceptually available but are politically suppressed by the voluntary architecture. Resistance (0.72) is high — from LDCs, SIDS, climate justice movements, and the historical responsibility reading's institutional champions — but is structurally constrained by the power asymmetry.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter/beneficiary seats (developed nations, tech firms, IFIs), the constraint appears as a functioning Rope: universal participation achieved, technology transfer flowing, coordination working. From the payer seats (LDCs, SIDS, vulnerable nations), it computes as a Snare: extraction without recourse, suppression of alternatives (loss/damage finance, binding commitments), no exit. The engine computes this seat divergence from the structural data — the claimed_type (tangled_rope) acknowledges both coordination and extraction are real, but the per-seat experience differs radically.
 *
 * DIRECTIONALITY LOGIC:
 *   Developed nations are structural beneficiaries (d ~0.15): they collect governance control, avoid binding constraints, and mediate technology transfer commercially. Multinational tech firms and IFIs are concentrated beneficiaries (d ~0.1-0.2): they capture finance streams. LDCs and SIDS are full targets (d ~0.9-0.95): trapped by climate impacts, excluded from rule-setting, bearing costs without compensation. Climate-vulnerable developing nations are constrained targets (d ~0.7): some coalition power but finance-dependent. Civil society is excluded (no d — outside the constraint). Observers are analytical (d=0.5). The directionality derivation from beneficiary/victim + exit options captures this gradient.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Kyoto's participation failure) was real but the solution (voluntary NDCs) displaced the adequacy problem onto the most vulnerable. The constraint now persists because developed nations benefit from the lack of binding enforcement, tech firms benefit from commercialized transfer, and IFIs benefit from administrative control — while the victims (LDCs, SIDS) are too powerless to force revision. The coordination function (universal participation) is real but has become a cover for the extraction function (risk transfer to vulnerable). This is classic mandatrophy: the mandate (universal participation) has outlived its function (effective mitigation) but persists because the beneficiaries of the extraction control the agenda.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    voluntary_vs_binding_adequacy_gap,
    'Can the voluntary NDC framework ever deliver sufficient aggregate mitigation to meet the Paris temperature goals, or is the adequacy gap structurally necessary to the voluntary architecture?',
    'Track the Emissions Gap Report trajectory vs. NDC implementation; assess whether the ratchet mechanism (global stocktake -> enhanced NDCs) can close the gap without binding enforcement.',
    'If the gap is structurally necessary, the voluntary reading is a Snare disguised as a Rope — coordination function is theater for extraction. If closable, it remains a Tangled Rope with genuine but insufficient coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(voluntary_vs_binding_adequacy_gap, empirical, 'Whether the voluntary architecture can self-correct to adequacy or is structurally inadequate by design.').

omega_variable(
    technology_transfer_commercialization_extent,
    'What fraction of technology transfer finance under the voluntary reading is captured as commercial rent (IP licensing, consultancy margins, equity returns) vs. deployed as concessionary public good?',
    'Audit GCF/GEF/bi-lateral technology project portfolios for financial terms, IP ownership, and concessionality; compare to developed nation ODA reporting.',
    'High commercial capture confirms the technology transfer obligation has been converted into a revenue stream for developed-nation firms — extraction disguised as coordination. Low capture would support the coordination framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_transfer_commercialization_extent, empirical, 'Whether technology transfer operates as public obligation or commercial extraction.').

omega_variable(
    loss_damage_exclusion_as_suppression,
    'Is the exclusion of loss/damage from binding obligation under the voluntary reading a structural suppression mechanism that protects developed nations from liability, or a legitimate deferral to future negotiation?',
    'Track the Warsaw International Mechanism evolution, the Santiago Network, and the COP27 loss/damage fund establishment — assess whether finance is new/additional and whether liability language is explicitly excluded.',
    'If exclusion is structural suppression, the voluntary reading''s coordination function actively suppresses the historical responsibility reading''s core claim (liability for cumulative harm). If deferral, the two readings may converge over time.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(loss_damage_exclusion_as_suppression, conceptual, 'Whether loss/damage exclusion is active suppression or pending negotiation.').

omega_variable(
    kernel_framing_underdetermination,
    'Does the CBDR kernel admit only these two readings, or is there a third framing (e.g., CBDR as dynamic differentiation based on evolving capacity/responsibility) that would change the structural classification?',
    'Survey UNFCCC negotiation text evolution, academic typologies of CBDR interpretations, and state submissions on CBDR operationalization to map the full reading space.',
    'If a third reading exists with different beneficiary/victim structure, the binary decomposition is incomplete and both current stories misrepresent the constraint family.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the declared reading pair exhausts the CBDR kernel''s structural possibilities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cbdr_principle__voluntary_commitment_reading, 1997, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cbdr_tr_t1997, cbdr_principle__voluntary_commitment_reading, theater_ratio, 1997, 0.08).
narrative_ontology:measurement(cbdr_tr_t2005, cbdr_principle__voluntary_commitment_reading, theater_ratio, 2005, 0.12).
narrative_ontology:measurement(cbdr_tr_t2009, cbdr_principle__voluntary_commitment_reading, theater_ratio, 2009, 0.21).
narrative_ontology:measurement(cbdr_tr_t2015, cbdr_principle__voluntary_commitment_reading, theater_ratio, 2015, 0.32).
narrative_ontology:measurement(cbdr_tr_t2018, cbdr_principle__voluntary_commitment_reading, theater_ratio, 2018, 0.37).
narrative_ontology:measurement(cbdr_tr_t2021, cbdr_principle__voluntary_commitment_reading, theater_ratio, 2021, 0.39).
narrative_ontology:measurement(cbdr_tr_t2025, cbdr_principle__voluntary_commitment_reading, theater_ratio, 2025, 0.41).

% Extraction over time
narrative_ontology:measurement(cbdr_be_t1997, cbdr_principle__voluntary_commitment_reading, base_extractiveness, 1997, 0.15).
narrative_ontology:measurement(cbdr_be_t2005, cbdr_principle__voluntary_commitment_reading, base_extractiveness, 2005, 0.22).
narrative_ontology:measurement(cbdr_be_t2009, cbdr_principle__voluntary_commitment_reading, base_extractiveness, 2009, 0.35).
narrative_ontology:measurement(cbdr_be_t2015, cbdr_principle__voluntary_commitment_reading, base_extractiveness, 2015, 0.48).
narrative_ontology:measurement(cbdr_be_t2018, cbdr_principle__voluntary_commitment_reading, base_extractiveness, 2018, 0.54).
narrative_ontology:measurement(cbdr_be_t2021, cbdr_principle__voluntary_commitment_reading, base_extractiveness, 2021, 0.58).
narrative_ontology:measurement(cbdr_be_t2025, cbdr_principle__voluntary_commitment_reading, base_extractiveness, 2025, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(cbdr_su_t1997, cbdr_principle__voluntary_commitment_reading, suppression_requirement, 1997, 0.25).
narrative_ontology:measurement(cbdr_su_t2005, cbdr_principle__voluntary_commitment_reading, suppression_requirement, 2005, 0.35).
narrative_ontology:measurement(cbdr_su_t2009, cbdr_principle__voluntary_commitment_reading, suppression_requirement, 2009, 0.42).
narrative_ontology:measurement(cbdr_su_t2015, cbdr_principle__voluntary_commitment_reading, suppression_requirement, 2015, 0.51).
narrative_ontology:measurement(cbdr_su_t2018, cbdr_principle__voluntary_commitment_reading, suppression_requirement, 2018, 0.54).
narrative_ontology:measurement(cbdr_su_t2021, cbdr_principle__voluntary_commitment_reading, suppression_requirement, 2021, 0.56).
narrative_ontology:measurement(cbdr_su_t2025, cbdr_principle__voluntary_commitment_reading, suppression_requirement, 2025, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cbdr_principle__voluntary_commitment_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(cbdr_principle__voluntary_commitment_reading, 0.18).
narrative_ontology:affects_constraint(cbdr_principle__voluntary_commitment_reading, cbdr_principle__historical_responsibility_reading).
narrative_ontology:affects_constraint(cbdr_principle__voluntary_commitment_reading, paris_agreement_architecture).
narrative_ontology:affects_constraint(cbdr_principle__voluntary_commitment_reading, global_climate_finance_architecture).
narrative_ontology:affects_constraint(cbdr_principle__voluntary_commitment_reading, technology_mechanism_unfccc).

% DUAL FORMULATION NOTE:
% This constraint and cbdr_principle__historical_responsibility_reading form a constraint family decomposing the CBDR kernel. The voluntary reading has higher extractiveness (0.62 vs ~0.35 projected for historical) because it converts binding developed-nation obligations into voluntary pledges and commercializes technology transfer. The historical reading would impose binding constraints on developed nations (making them victims of extraction) and create loss/damage finance obligations (making developing nations beneficiaries). They coexist as competing institutional interpretations; the voluntary reading currently dominates the operational architecture.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cbdr_principle__voluntary_commitment_reading, institutional, 0.15).
constraint_indexing:directionality_override(cbdr_principle__voluntary_commitment_reading, powerful, 0.12).
constraint_indexing:directionality_override(cbdr_principle__voluntary_commitment_reading, powerless, 0.93).
constraint_indexing:directionality_override(cbdr_principle__voluntary_commitment_reading, moderate, 0.7).
constraint_indexing:directionality_override(cbdr_principle__voluntary_commitment_reading, organized, 0.5).
constraint_indexing:directionality_override(cbdr_principle__voluntary_commitment_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
