% ============================================================================
% CONSTRAINT STORY: cbdr_principle__historical_responsibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cbdr_principle__historical_responsibility_reading, []).

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
 *   constraint_id: cbdr_principle__historical_responsibility_reading
 *   human_readable: CBDR Historical Responsibility Reading — Binding Emissions Reductions and Loss/Damage Finance from Developed Nations
 *   domain: international_climate_governance/treaty_law/development_economics
 *
 * SUMMARY:
 *   The CBDR (Common But Differentiated Responsibilities) principle, codified
 *   in UNFCCC Article 3.1 and carried into the Paris Agreement, is contested
 *   between two structural readings. This story instantiates the
 *   historical_responsibility_reading: CBDR requires binding, quantified
 *   emissions reductions from developed nations proportional to their
 *   cumulative historical emissions, plus mandatory loss/damage financing for
 *   developing nations. The reading treats historical emissions as generating
 *   a debt that must be repaid through both mitigation and finance. Developed
 *   nations (Annex I/OECD) are the structural payers — they bear the
 *   extraction. Developing nations (G77, LDCs, SIDS) are the structural
 *   beneficiaries — they receive the transfers and policy space. The
 *   constraint has a genuine coordination function (solving the global
 *   commons problem via equity) and asymmetric extraction (disproportionate
 *   burden on Annex I). Active enforcement exists through the Paris
 *   Agreement's compliance committee and global stocktake, but suppression is
 *   moderate because the treaty lacks hard sanctions. Theater ratio rises
 *   over time as NDCs consistently undershoot the reading's implied fair
 *   shares. The sibling voluntary_commitment_reading coexists as the live
 *   alternative framework.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cbdr_principle__historical_responsibility_reading, 0.72).
domain_priors:suppression_score(cbdr_principle__historical_responsibility_reading, 0.45).
domain_priors:theater_ratio(cbdr_principle__historical_responsibility_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cbdr_principle__historical_responsibility_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(cbdr_principle__historical_responsibility_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(cbdr_principle__historical_responsibility_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cbdr_principle__historical_responsibility_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(cbdr_principle__historical_responsibility_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cbdr_principle__historical_responsibility_reading, tangled_rope).
narrative_ontology:human_readable(cbdr_principle__historical_responsibility_reading, "CBDR Historical Responsibility Reading — Binding Emissions Reductions and Loss/Damage Finance from Developed Nations").
narrative_ontology:topic_domain(cbdr_principle__historical_responsibility_reading, "international_climate_governance/treaty_law/development_economics").

domain_priors:requires_active_enforcement(cbdr_principle__historical_responsibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(cbdr_principle__historical_responsibility_reading, 'ea3915f7-dcb1-46a4-8b33-26c6258db2c8').
narrative_ontology:cs_kernel_codification('ea3915f7-dcb1-46a4-8b33-26c6258db2c8', formalized).
narrative_ontology:cs_authority_grounding('ea3915f7-dcb1-46a4-8b33-26c6258db2c8', lineage).
narrative_ontology:cs_interpretation_layer_present('ea3915f7-dcb1-46a4-8b33-26c6258db2c8').
narrative_ontology:cs_reading_relation('ea3915f7-dcb1-46a4-8b33-26c6258db2c8', cbdr_principle__voluntary_commitment_reading, coexists_with).
narrative_ontology:cs_axiom('ea3915f7-dcb1-46a4-8b33-26c6258db2c8', foundational, historical_emissions_determine_obligation).
narrative_ontology:cs_axiom_status(historical_emissions_determine_obligation, holdable).
narrative_ontology:cs_axiom_grounding('ea3915f7-dcb1-46a4-8b33-26c6258db2c8', historical_emissions_determine_obligation, deontological).
narrative_ontology:cs_axiom('ea3915f7-dcb1-46a4-8b33-26c6258db2c8', foundational, loss_damage_financing_required).
narrative_ontology:cs_axiom_status(loss_damage_financing_required, holdable).
narrative_ontology:cs_axiom_grounding('ea3915f7-dcb1-46a4-8b33-26c6258db2c8', loss_damage_financing_required, empirically_contingent).
narrative_ontology:cs_reference_frame('ea3915f7-dcb1-46a4-8b33-26c6258db2c8', unfccc_annex_i_differentiation).
narrative_ontology:cs_drift_state('ea3915f7-dcb1-46a4-8b33-26c6258db2c8', paris_agreement_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ea3915f7-dcb1-46a4-8b33-26c6258db2c8', '').
narrative_ontology:cs_kernel_id(cbdr_principle__historical_responsibility_reading, cbdr_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cbdr_principle__historical_responsibility_reading, developing_nations_g77).
narrative_ontology:constraint_beneficiary(cbdr_principle__historical_responsibility_reading, least_developed_countries).
narrative_ontology:constraint_beneficiary(cbdr_principle__historical_responsibility_reading, small_island_developing_states).
narrative_ontology:constraint_victim(cbdr_principle__historical_responsibility_reading, annex_i_developed_nations).
narrative_ontology:constraint_victim(cbdr_principle__historical_responsibility_reading, oecd_countries).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(cbdr_principle__historical_responsibility_reading, fossil_fuel_industry_major_economies).
narrative_ontology:constraint_victim(cbdr_principle__historical_responsibility_reading, fossil_fuel_industry_major_economies).
narrative_ontology:constraint_vindicates(cbdr_principle__historical_responsibility_reading, historical_emissions_create_obligation).
narrative_ontology:constraint_vindicates(cbdr_principle__historical_responsibility_reading, climate_justice_requires_redistribution).
narrative_ontology:constraint_vindicates(cbdr_principle__historical_responsibility_reading, atmospheric_commons_equity_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear binding emissions reduction targets proportional to cumulative historical emissions and provide loss/damage financing. They negotiated the UNFCCC framework but resist the historical responsibility interpretation. Exit means withdrawing from Paris Agreement — politically costly and diplomatically isolating. They carry the financial and mitigation burden.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, annex_i_developed_nations, payer,
    institutional, generational, constrained, global).

% Subset of Annex I with greatest historical emissions and financial capacity. They set negotiation agendas through presidency roles and funding of UNFCCC processes. They pay into climate finance mechanisms (GCF, loss/damage fund) while shaping rule interpretation. Their dual position — agenda-setter and payer — creates internal tension between leadership claims and burden resistance.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, oecd_countries, payer,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(cbdr_principle__historical_responsibility_reading, oecd_countries, agenda_setter).

% Receive climate finance, technology transfer, and policy space for development. They champion the historical responsibility reading as a bloc. Their coordination depends on unity across diverse economies (China, India, Brazil, African Group, LDCs). Exit from the reading means accepting voluntary-only framework — they view this as abandoning equity.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, developing_nations_g77, beneficiary,
    organized, generational, constrained, global).

% Most climate-vulnerable, least historical emissions, least adaptive capacity. They depend entirely on loss/damage finance and adaptation support promised under this reading. No credible exit — they cannot mitigate their way out of vulnerability nor finance adaptation domestically. Their bargaining power is moral authority, not material leverage.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, least_developed_countries, beneficiary,
    powerless, biographical, trapped, global).

% Existential threat from sea-level rise; zero historical responsibility. They are the moral anchor of the historical responsibility reading. Loss/damage finance is not developmental but survival financing. They have no exit — territory loss is irreversible. Their role in negotiations is disproportionate to material power because they embody the justice claim.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, small_island_developing_states, beneficiary,
    powerless, immediate, trapped, global).

% Frontline communities (smallholder farmers, coastal dwellers, indigenous peoples) in both developed and developing nations. They bear climate impacts directly but have no seat at UNFCCC negotiations. Their interests are mediated through national governments and NGOs. If this reading fails, they lose first; if it succeeds, they may still lose if finance doesn't reach local level.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, climate_vulnerable_populations, excluded,
    powerless, immediate, trapped, local).

% Administers the treaty process, services COPs, manages compliance committee, tracks NDCs and finance flows. They do not set substantive obligations but structure the negotiation architecture that determines which reading prevails. Their institutional interest is process continuity and universal participation.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, unfccc_secretariat, agenda_setter,
    institutional, generational, analytical, global).

% Bear transition costs (stranded assets, regulation) under binding emissions reductions — payer. But benefit from delayed action, voluntary frameworks, and continued demand — beneficiary. They lobby within Annex I capitals to weaken historical responsibility interpretation. Their exit option is capital relocation and political capture, not available to nations.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, fossil_fuel_industry_major_economies, payer,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(cbdr_principle__historical_responsibility_reading, fossil_fuel_industry_major_economies, beneficiary).

% Green Climate Fund, Global Environment Facility, Loss and Damage Fund board. They operationalize the financial transfer function — deciding allocation, eligibility, and modalities. They shape what 'loss/damage financing' means in practice. Their institutional survival depends on the reading's financial commitments materializing.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, climate_finance_institutions, agenda_setter,
    institutional, generational, analytical, global).

% IPCC scientists, climate law scholars, justice theorists, civil society watchdogs. They evaluate whether emissions reductions match historical responsibility, whether finance is additional and accessible, whether the reading's claims hold empirically. They do not collect or pay but their assessments shift legitimacy across all seats.
narrative_ontology:constraint_stakeholder(cbdr_principle__historical_responsibility_reading, independent_expert_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates the global carbon budget and climate finance burden across nations based on historical emissions responsibility, enabling collective mitigation action that would otherwise stall on equity deadlock.
% TRANSFER_FUNCTION: Moves emissions reduction obligation (mitigation burden) and financial resources (loss/damage, adaptation, mitigation finance) from Annex I developed nations to developing nations, proportional to cumulative historical emissions since industrialization.
% ABSENT_VOICES: Climate-vulnerable populations within both developed and developing nations (subnational, indigenous, low-income communities) who bear impacts but lack negotiating standing. Future generations who inherit the atmospheric stock. Fossil-fuel-dependent workers and communities in developing nations who face transition costs without transition finance. Their exclusion lets the reading treat 'developing nations' as a monolithic beneficiary.
% DISAPPEARANCE_RATIONALE: If the historical responsibility reading vanished overnight, the Paris Agreement's equity architecture would collapse. Developed nations would face no binding differentiated obligation; developing nations would lose the legal basis for finance claims. Negotiations would revert to voluntary contributions only (the sibling reading), likely triggering withdrawal by vulnerable states and breakdown of universal participation. The global climate regime would reorganize around minimal common denominator pledges.
% FOUNDING_PROBLEM: How to allocate climate action burden fairly given that industrialized nations emitted the vast majority of cumulative GHGs since 1750 while developing nations need carbon space for development and face disproportionate impacts.
% FOUNDING_PROBLEM_CORROBORATION: IPCC AR6 WGIII (2022) documents historical emissions inequality: Annex I ~57% of cumulative CO2 1850-2019 with ~18% of population. UNFCCC historical records (1992) show Annex I/Non-Annex I differentiation was the negotiated compromise. Climate justice scholarship (Caney, Shue, Vanderheiden) corroborates the equity principle from outside the beneficiary bloc. Developed nation governments contest the status, arguing current emissions and capability should supersede historical responsibility.
narrative_ontology:disappearance_verdict(cbdr_principle__historical_responsibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(cbdr_principle__historical_responsibility_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(cbdr_principle__historical_responsibility_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(cbdr_principle__historical_responsibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(cbdr_principle__historical_responsibility_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cbdr_principle__historical_responsibility_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(cbdr_principle__historical_responsibility_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(cbdr_principle__historical_responsibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the reading demands both deep emissions cuts (costly structural transformation) and substantial financial transfers (loss/damage fund target $100B+/yr, actual needs estimated trillions). Suppression (0.45) is moderate — the constraint relies on treaty compliance mechanisms and reputational pressure, not coercive enforcement; parties can and do underperform NDCs with limited consequence. Theater ratio (0.48) is substantial and rising — the gap between pledged NDCs and historical-responsibility-aligned fair shares grows each stocktake cycle; compliance rituals (reporting, review) increasingly substitute for delivery. Accessibility collapse (0.55) is middling — the voluntary_commitment_reading provides a live alternative framework, and some developing nations (e.g., China, India) have leveraged the ambiguity to resist binding commitments themselves. Resistance (0.68) is high — Annex I nations consistently resist historical responsibility operationalization (Kyoto's limited participation, Paris's self-determined NDCs, loss/damage finance delays).
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (Annex I, OECD) and beneficiary seats (G77, LDCs, SIDS) compute to different types. From the payer seat, the constraint appears as a snare — high extraction, active enforcement, no reciprocity, alternatives (voluntary framework) suppressed by moral pressure. From the beneficiary seat, it appears as a rope — genuine coordination solving the equity deadlock, net benefit, alternatives (no finance, no carbon space) worse. From the trapped beneficiary seat (LDCs, SIDS), it appears as a scaffold — the only mechanism preventing catastrophic loss, but with a sunset clause implied by 1.5°C threshold breach. The engine computes this divergence from the structural data; the authored claim (tangled_rope) reflects the analyst's view that both coordination and extraction are real and neither reduces to the other.
 *
 * DIRECTIONALITY LOGIC:
 *   Annex I nations and OECD countries are structural payers (d near 1.0) — they bear the mitigation cost and finance obligation with constrained exit (treaty withdrawal is politically costly). G77 developing nations are structural beneficiaries (d near 0.0) — they receive finance and carbon space. LDCs and SIDS are trapped beneficiaries (d ≈ 0.0, exit: trapped) — they cannot exit the climate system and depend entirely on the reading's finance promises. Climate-vulnerable populations are excluded (no seat, exit: trapped) — they would object to both readings' inadequacy but have no voice. Fossil fuel industry in major economies is a dual-role payer/beneficiary with mobile exit — they capture rents from delay while externalizing transition costs. UNFCCC Secretariat and finance institutions are agenda_setters with analytical exit — they administer the constraint but don't bear its extraction. Independent observers are analytical seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (historical emissions inequity) is contested as live — IPCC data confirms the disparity persists, but developed nations argue the problem has mutated (current emissions now dominated by large developing economies). The reading prevents mislabeling coordination as pure extraction by demonstrating a real collective-action function: without differentiated responsibility, universal participation collapses (witnessed in Kyoto's failure). It prevents mislabeling extraction as pure coordination by documenting the rising theater ratio and the growing gap between fair-share allocations and actual NDCs — the extraction component is not incidental but structural, and the constraint's persistence depends on developed nations not exercising their exit option (withdrawal). Mandatrophy is unresolved: the original mandate (equity-based burden sharing) has been partially captured by the voluntary_commitment_reading's institutionalization in Paris, but the historical responsibility reading remains the legal anchor for loss/damage finance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_vs_current_emissions_metric,
    'Should differentiated responsibility be calibrated to cumulative historical emissions (1850-present) or to current/recent emissions and capability?',
    'Empirical resolution via IPCC carbon budget attribution studies; political resolution via COP decision on fair-share methodology for global stocktake.',
    'If current emissions/capability prevails, the reading''s extraction base shrinks (China, India become payers); if historical prevails, Annex I bears near-full burden. Classification shifts: historical metric → tangled_rope with high extraction on Annex I; current metric → more symmetric rope or shifted victim set.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_vs_current_emissions_metric, conceptual, 'Whether the reading''s core metric (historical emissions) remains the operative equity basis or is displaced by current-emissions metrics.').

omega_variable(
    loss_damage_finance_separability,
    'Is loss/damage financing structurally separable from adaptation and mitigation finance, or is it a rebranding of existing commitments?',
    'Track financial flows: new/additional funding vs. reallocated ODA; legal test in Loss and Damage Fund governance (independent board vs. GCF sub-window).',
    'If separable and additional, extraction on payers increases and reading''s coordination claim strengthens (new function). If rebranding, extraction is illusory and theater ratio is higher than measured — the constraint performs a new obligation while delivering old finance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(loss_damage_finance_separability, empirical, 'Whether the loss/damage finance obligation creates a genuinely new transfer function or repackages existing climate finance.').

omega_variable(
    enforcement_effectiveness_ambiguity,
    'Is the constraint''s suppression structural (binding treaty compliance mechanisms) or performative (naming-and-shaming with no material consequence)?',
    'Observe compliance committee outcomes: are there material consequences for NDC underachievement? Track withdrawal vs. non-compliance rates.',
    'If structural, suppression is underestimated and the constraint leans snare; if performative, suppression is overestimated and the constraint leans rope. The theater ratio trajectory (rising) suggests performative drift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_effectiveness_ambiguity, empirical, 'Whether the enforcement machinery has material teeth or operates through reputational pressure alone.').

omega_variable(
    developing_nation_internal_differentiation,
    'Does the reading''s beneficiary category (developing nations) mask internal extraction — do large emerging economies (China, India) extract from LDCs/SIDS within the G77 bloc?',
    'Analyze G77 negotiation positions: do major emerging economies block LDC/SIDS priorities (e.g., loss/damage finance granularity, 1.5°C target)? Track climate finance flows: do they reach most vulnerable?',
    'If internal extraction exists, the reading''s beneficiary set is not a unit — some ''beneficiaries'' are net payers within the bloc. This would split the victim/beneficiary structure and potentially reclassify sub-constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(developing_nation_internal_differentiation, conceptual, 'Whether the G77 beneficiary bloc conceals a secondary extraction layer between its members.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cbdr_principle__historical_responsibility_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cbdr_hist_resp_tr_t0, cbdr_principle__historical_responsibility_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(cbdr_hist_resp_tr_t6, cbdr_principle__historical_responsibility_reading, theater_ratio, 6, 0.22).
narrative_ontology:measurement(cbdr_hist_resp_tr_t12, cbdr_principle__historical_responsibility_reading, theater_ratio, 12, 0.31).
narrative_ontology:measurement(cbdr_hist_resp_tr_t18, cbdr_principle__historical_responsibility_reading, theater_ratio, 18, 0.42).
narrative_ontology:measurement(cbdr_hist_resp_tr_t24, cbdr_principle__historical_responsibility_reading, theater_ratio, 24, 0.45).
narrative_ontology:measurement(cbdr_hist_resp_tr_t30, cbdr_principle__historical_responsibility_reading, theater_ratio, 30, 0.48).

% Extraction over time
narrative_ontology:measurement(cbdr_hist_resp_be_t0, cbdr_principle__historical_responsibility_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cbdr_hist_resp_be_t6, cbdr_principle__historical_responsibility_reading, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(cbdr_hist_resp_be_t12, cbdr_principle__historical_responsibility_reading, base_extractiveness, 12, 0.55).
narrative_ontology:measurement(cbdr_hist_resp_be_t18, cbdr_principle__historical_responsibility_reading, base_extractiveness, 18, 0.62).
narrative_ontology:measurement(cbdr_hist_resp_be_t24, cbdr_principle__historical_responsibility_reading, base_extractiveness, 24, 0.68).
narrative_ontology:measurement(cbdr_hist_resp_be_t30, cbdr_principle__historical_responsibility_reading, base_extractiveness, 30, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(cbdr_hist_resp_su_t0, cbdr_principle__historical_responsibility_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(cbdr_hist_resp_su_t6, cbdr_principle__historical_responsibility_reading, suppression_requirement, 6, 0.32).
narrative_ontology:measurement(cbdr_hist_resp_su_t12, cbdr_principle__historical_responsibility_reading, suppression_requirement, 12, 0.38).
narrative_ontology:measurement(cbdr_hist_resp_su_t18, cbdr_principle__historical_responsibility_reading, suppression_requirement, 18, 0.41).
narrative_ontology:measurement(cbdr_hist_resp_su_t24, cbdr_principle__historical_responsibility_reading, suppression_requirement, 24, 0.43).
narrative_ontology:measurement(cbdr_hist_resp_su_t30, cbdr_principle__historical_responsibility_reading, suppression_requirement, 30, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cbdr_principle__historical_responsibility_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(cbdr_principle__historical_responsibility_reading, 0.18).
narrative_ontology:affects_constraint(cbdr_principle__historical_responsibility_reading, paris_agreement_ndc_framework).
narrative_ontology:affects_constraint(cbdr_principle__historical_responsibility_reading, loss_and_damage_fund_governance).
narrative_ontology:affects_constraint(cbdr_principle__historical_responsibility_reading, global_stocktake_equity_assessment).
narrative_ontology:affects_constraint(cbdr_principle__historical_responsibility_reading, cbdr_principle__voluntary_commitment_reading).

% DUAL FORMULATION NOTE:
% This reading and voluntary_commitment_reading form a constraint family decomposing the CBDR kernel. Historical responsibility reading has high extractiveness on Annex I (binding cuts + finance); voluntary commitment reading has low extractiveness (self-determined NDCs) but high theater (gap between pledges and 1.5°C). They coexist in the Paris text but pull the regime in opposite structural directions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cbdr_principle__historical_responsibility_reading, institutional, 0.85).
constraint_indexing:directionality_override(cbdr_principle__historical_responsibility_reading, powerless, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
