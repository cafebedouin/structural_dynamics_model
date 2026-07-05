% ============================================================================
% CONSTRAINT STORY: cbdr_principle__voluntary_commitment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: CBDR as Voluntary, Nationally Determined Contributions with Technology Transfer
 *   domain: international_governance/treaty_law/development_economics
 *
 * SUMMARY:
 *   This constraint is the voluntary-commitment reading of the Common But
 *   Differentiated Responsibilities (CBDR) principle as it has actually
 *   operated through the Paris Agreement's Nationally Determined
 *   Contributions (NDC) architecture since 2015, building on the 1992 UNFCCC
 *   founding text. Under this reading, developed nations' primary CBDR
 *   obligation is discharged through self-determined mitigation pledges,
 *   technology transfer (largely on commercial terms), and finance
 *   mobilization targets they set and revise themselves, rather than through
 *   binding emissions cuts indexed to cumulative historical responsibility or
 *   automatic loss-and-damage liability. This story is one of two readings of
 *   the same kernel: the sibling historical_responsibility_reading treats
 *   CBDR as requiring binding, cumulative-emissions-proportional cuts plus
 *   loss/damage financing as an entitlement. The two readings have
 *   structurally different victim sets — under this voluntary reading,
 *   developed nations exit the victim set for binding mitigation constraints
 *   (they bear no enforceable cut obligation), while developing and
 *   especially climate-vulnerable nations enter the victim set for adaptation
 *   costs they must bear without a compensation guarantee tied to
 *   responsibility. The ε values of the two readings are not interchangeable;
 *   each is authored as its own constraint per the ε-invariance principle.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cbdr_principle__voluntary_commitment_reading, 0.58).
domain_priors:suppression_score(cbdr_principle__voluntary_commitment_reading, 0.42).
domain_priors:theater_ratio(cbdr_principle__voluntary_commitment_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cbdr_principle__voluntary_commitment_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(cbdr_principle__voluntary_commitment_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(cbdr_principle__voluntary_commitment_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cbdr_principle__voluntary_commitment_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(cbdr_principle__voluntary_commitment_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cbdr_principle__voluntary_commitment_reading, tangled_rope).
narrative_ontology:human_readable(cbdr_principle__voluntary_commitment_reading, "CBDR as Voluntary, Nationally Determined Contributions with Technology Transfer").
narrative_ontology:topic_domain(cbdr_principle__voluntary_commitment_reading, "international_governance/treaty_law/development_economics").

domain_priors:requires_active_enforcement(cbdr_principle__voluntary_commitment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(cbdr_principle__voluntary_commitment_reading, '49a9051a-8b07-4f37-879c-8d0c9484a660').
narrative_ontology:cs_kernel_codification('49a9051a-8b07-4f37-879c-8d0c9484a660', fixed_text).
narrative_ontology:cs_authority_grounding('49a9051a-8b07-4f37-879c-8d0c9484a660', distributed).
narrative_ontology:cs_reading_relation('49a9051a-8b07-4f37-879c-8d0c9484a660', cbdr_principle__historical_responsibility_reading, coexists_with).
narrative_ontology:cs_axiom('49a9051a-8b07-4f37-879c-8d0c9484a660', foundational, national_sovereignty_over_mitigation_pace).
narrative_ontology:cs_axiom_status(national_sovereignty_over_mitigation_pace, holdable).
narrative_ontology:cs_axiom_grounding('49a9051a-8b07-4f37-879c-8d0c9484a660', national_sovereignty_over_mitigation_pace, conventional).
narrative_ontology:cs_axiom('49a9051a-8b07-4f37-879c-8d0c9484a660', secondary, universal_participation_outweighs_binding_precision).
narrative_ontology:cs_axiom_status(universal_participation_outweighs_binding_precision, holdable).
narrative_ontology:cs_axiom_grounding('49a9051a-8b07-4f37-879c-8d0c9484a660', universal_participation_outweighs_binding_precision, instrumental).
narrative_ontology:cs_reference_frame('49a9051a-8b07-4f37-879c-8d0c9484a660', unfccc_1992_differentiation_without_quantified_remedy).
narrative_ontology:cs_drift_state('49a9051a-8b07-4f37-879c-8d0c9484a660', post_paris_ndc_stocktake_2023, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('49a9051a-8b07-4f37-879c-8d0c9484a660', '').
narrative_ontology:cs_kernel_id(cbdr_principle__voluntary_commitment_reading, cbdr_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cbdr_principle__voluntary_commitment_reading, developed_nation_governments).
narrative_ontology:constraint_beneficiary(cbdr_principle__voluntary_commitment_reading, developed_nation_fossil_incumbents).
narrative_ontology:constraint_beneficiary(cbdr_principle__voluntary_commitment_reading, developed_nation_technology_exporters).
narrative_ontology:constraint_victim(cbdr_principle__voluntary_commitment_reading, small_island_developing_states).
narrative_ontology:constraint_victim(cbdr_principle__voluntary_commitment_reading, least_developed_countries).
narrative_ontology:constraint_victim(cbdr_principle__voluntary_commitment_reading, climate_vulnerable_coastal_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(cbdr_principle__voluntary_commitment_reading, large_emerging_economies).
narrative_ontology:constraint_victim(cbdr_principle__voluntary_commitment_reading, large_emerging_economies).
narrative_ontology:constraint_vindicates(cbdr_principle__voluntary_commitment_reading, national_sovereignty_over_climate_policy).
narrative_ontology:constraint_vindicates(cbdr_principle__voluntary_commitment_reading, differentiated_but_flexible_obligation_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Negotiate and ratify the Paris Agreement's NDC architecture, which converts CBDR's differentiation principle into self-set, non-binding pledges rather than formula-driven binding cuts tied to cumulative emissions. They control the diplomatic language that defines their own obligation as 'best efforts' plus technology transfer and finance mobilization, and they set the terms, timelines, and conditionality of that transfer. They face no binding penalty for missing self-set targets.
narrative_ontology:constraint_stakeholder(cbdr_principle__voluntary_commitment_reading, developed_nation_governments, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(cbdr_principle__voluntary_commitment_reading, developed_nation_governments, beneficiary).

% Benefit directly from the absence of binding, historically-proportional emissions cuts on their home governments; continue operating carbon-intensive assets on timelines their own governments set voluntarily rather than timelines derived from cumulative historical responsibility. Can relocate capital or shift to carbon markets if pressure increases.
narrative_ontology:constraint_stakeholder(cbdr_principle__voluntary_commitment_reading, developed_nation_fossil_incumbents, beneficiary,
    powerful, biographical, mobile, global).

% Sell clean-energy technology, licenses, and consulting services to developing nations under the 'technology transfer' obligation, frequently on commercial or near-commercial terms rather than as a grant-based transfer. The obligation being framed as technology transfer (rather than binding finance or reparative liability) creates a durable export market for them.
narrative_ontology:constraint_stakeholder(cbdr_principle__voluntary_commitment_reading, developed_nation_technology_exporters, beneficiary,
    organized, biographical, arbitrage, global).

% Retain policy space to grow emissions while industrializing, since their NDCs are also self-determined and non-binding; this is a genuine coordination benefit of the voluntary reading. But they receive little of the promised technology transfer on favorable terms and bear rising domestic adaptation costs as climate impacts accelerate, without a compensation mechanism tied to who caused the damage.
narrative_ontology:constraint_stakeholder(cbdr_principle__voluntary_commitment_reading, large_emerging_economies, beneficiary,
    powerful, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(cbdr_principle__voluntary_commitment_reading, large_emerging_economies, payer).

% Face existential sea-level and storm risk overwhelmingly caused by historical emissions from developed nations, but under the voluntary-commitment reading receive no binding entitlement to loss-and-damage financing proportional to responsibility — only aspirational, underfunded pledges (e.g. the Loss and Damage Fund's persistently low capitalization). They cannot relocate their territory or economy; exit does not exist for a sinking nation.
narrative_ontology:constraint_stakeholder(cbdr_principle__voluntary_commitment_reading, small_island_developing_states, payer,
    powerless, civilizational, trapped, global).

% Bear adaptation costs (drought, crop failure, displacement) that scale with global emissions they did not cause, while the technology and finance they were promised as the developed-nation 'obligation' arrives as loans, IP-protected licenses, or conditional aid rather than unconditional transfer. They lack the negotiating leverage to convert voluntary pledges into enforceable entitlements.
narrative_ontology:constraint_stakeholder(cbdr_principle__voluntary_commitment_reading, least_developed_countries, payer,
    powerless, generational, trapped, regional).

% Individuals and communities living the physical consequences of accumulated emissions decisions made by nations far away and decades prior. They have no seat at COP negotiations and no mechanism to convert historical responsibility into personal or communal compensation; their only recourse is domestic disaster response, itself under-resourced by the same finance gap.
narrative_ontology:constraint_stakeholder(cbdr_principle__voluntary_commitment_reading, climate_vulnerable_coastal_populations, payer,
    powerless, biographical, trapped, local).

% Administers the NDC reporting and stocktake process, tracks pledge fulfillment, and publishes gap reports showing aggregate NDCs falling far short of 1.5C-consistent pathways. Has no enforcement power to convert voluntary pledges into binding obligations.
narrative_ontology:constraint_stakeholder(cbdr_principle__voluntary_commitment_reading, un_climate_secretariat, observer,
    institutional, generational, analytical, global).

% Argue at the margins of COP negotiations and through litigation (e.g. ICJ advisory opinion proceedings) that CBDR's voluntary reading inverts the polluter-pays logic the principle was meant to encode. They are permitted observer status at negotiations but have no vote and no seat at the table where NDC architecture is set.
narrative_ontology:constraint_stakeholder(cbdr_principle__voluntary_commitment_reading, climate_justice_advocacy_coalitions, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(cbdr_principle__voluntary_commitment_reading, developed_nation_governments).
narrative_ontology:fixing_cost_class(cbdr_principle__voluntary_commitment_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the real problem that a rigid, formula-driven binding-cuts regime (Kyoto-style) proved unratifiable by major emitters (notably the US) and unworkable for fast-growing large emerging economies; the voluntary NDC architecture achieved near-universal participation (196 parties) where binding schedules achieved partial and fragile participation.
% TRANSFER_FUNCTION: Moves the practical burden of adaptation cost and residual climate damage from the nations whose cumulative historical emissions caused it onto the nations and populations most exposed to the physical impacts, while moving technology and finance from developed to developing nations only at the volume, timing, and commercial terms developed nations themselves choose to set.
% ABSENT_VOICES: Small island states and least developed countries speak in negotiations but do not set the architecture; climate justice coalitions and the ICJ advisory process represent the historical-responsibility counter-reading but operate outside the binding negotiating track. Populations bearing physical loss have no direct voice at all.
% DISAPPEARANCE_RATIONALE: If the voluntary-commitment reading were replaced by a binding, cumulative-responsibility-indexed obligation regime overnight, developed nations would face enforceable emissions schedules and quantified, mandatory loss-and-damage liabilities; technology transfer would shift from commercial licensing to obligated grant terms. Developed-nation fossil and technology-export interests would lose significant negotiating leverage; vulnerable states would gain an enforceable claim they currently lack.
% FOUNDING_PROBLEM: The 1992 UNFCCC founding problem was twofold: get near-universal state participation in a climate regime despite vastly unequal historical contributions and development levels, and encode that inequality (CBDR) so poorer, lower-emitting nations would not bear equal mitigation burdens to the nations that industrialized first.
% FOUNDING_PROBLEM_CORROBORATION: Developed nation negotiators attest the voluntary architecture is the only politically viable form of universal participation and cite Kyoto's ratification failure as proof binding schedules do not survive domestic politics. Independent corroboration from outside the benefiting parties exists: IPCC emissions-gap assessments, the ICJ's 2025 advisory opinion process, and UN Loss and Damage Fund capitalization data (a small fraction of assessed need) are produced by bodies with no direct stake in developed-nation obligation levels and consistently find the voluntary architecture's actual mitigation and finance delivery falling far short of what CBDR's differentiation principle was meant to guarantee.
narrative_ontology:disappearance_verdict(cbdr_principle__voluntary_commitment_reading, world_rearranges).
narrative_ontology:founding_problem_status(cbdr_principle__voluntary_commitment_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(cbdr_principle__voluntary_commitment_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(cbdr_principle__voluntary_commitment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(cbdr_principle__voluntary_commitment_reading, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored at a substantial-but-not-extreme 0.58: the voluntary architecture achieved genuine coordination value (near-universal treaty participation, which the binding-schedule Kyoto model could not sustain), so this is not a pure extraction story — the coordination function is real. But the extraction component is also real and accumulating: the transfer of physical adaptation burden onto low-emitting, high-exposure states without a proportional compensation mechanism has widened over the interval as emissions-gap reports and loss-and-damage data have accumulated (the theater_ratio rising from 0.30 to 0.55 tracks a growing gap between pledge rhetoric and delivered finance/technology). Suppression is moderate (0.42) — no party is coercively barred from raising the historical-responsibility reading, but the negotiating architecture (consensus-based COP process, developed-nation control of finance mechanisms) structurally disadvantages the powerless seats who would need it changed. Resistance (0.62) reflects sustained, organized pushback from climate justice coalitions, SIDS negotiating blocs (AOSIS), and the 2025 ICJ advisory opinion process — this is not an unresisted constraint.
 *
 * PERSPECTIVAL GAP:
 *   From the developed_nation_governments seat, this is a coordination achievement: it solved the ratification-failure problem that killed Kyoto and secured universal participation, discharging their CBDR obligation through technology and finance leadership rather than through mandates they judge domestically unenforceable. From the small_island_developing_states and least_developed_countries seats, the identical structure operates as an enforced extraction: they bear compounding physical and fiscal costs caused by others' historical emissions, with only voluntary, chronically underfunded compensation. The engine should compute divergent seat-level types from these structural facts; the claimed_type (tangled_rope) reflects my assessment that both a genuine coordination function AND asymmetric extraction requiring active diplomatic/procedural maintenance are simultaneously present — not that one seat's reading is simply correct.
 *
 * DIRECTIONALITY LOGIC:
 *   Developed nation governments and their fossil/technology-export constituencies sit near the beneficiary end: they set the terms of their own obligation and capture commercial value from the technology-transfer channel. Large emerging economies are dual-positioned — genuine beneficiaries of retained mitigation policy space, but increasingly payers of adaptation costs as their own vulnerability grows and technology transfer under-delivers. SIDS, LDCs, and vulnerable coastal populations sit at the full-target end: trapped exit (a sinking island cannot relocate its economy), civilizational-to-biographical time horizons foreshortened by escalating physical risk, and no leverage to convert pledge into entitlement.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (achieving universal participation despite unequal historical responsibility) is contested rather than simply dead: it remains partially live insofar as universal participation genuinely required flexibility, but the specific mechanism chosen (fully voluntary self-determination with no binding floor) has drifted from a differentiation device into a discretion device that lets the highest-responsibility parties set their own remedy. This is exactly the kind of divergence the framework is built to surface: treating the arrangement as pure coordination (rope) would erase the accumulating extraction on vulnerable states; treating it as pure extraction (snare) would erase the genuine participation gains over the binding-Kyoto counterfactual. Tangled rope names both simultaneously.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cbdr_kernel_text_underdetermination,
    'Does the UNFCCC/Paris Agreement text of CBDR itself mandate a particular remedy (binding cuts vs. voluntary contributions), or is the text genuinely silent/ambiguous, making both readings equally faithful to the founding kernel?',
    'Close textual and negotiating-history analysis of the 1992 UNFCCC Article 3/4 drafting record and the Paris Agreement Article 4 negotiating history, cross-referenced against subsequent state practice (which reading states themselves invoked when it suited them) and the 2025 ICJ advisory opinion''s treatment of CBDR''s legal content.',
    'If the kernel text is genuinely underdetermined, both readings are equally legitimate legal claims and the contest is purely political; if the drafting history shows a specific remedy was intended and later diluted, the voluntary reading is better characterized as drift/capture rather than a co-equal reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cbdr_kernel_text_underdetermination, conceptual, 'Whether the CBDR kernel text determines a remedy or is genuinely open between the two readings.').

omega_variable(
    technology_transfer_terms_effective_value,
    'What fraction of ''technology transfer'' delivered under this reading''s obligation structure is grant-equivalent versus commercial-value-equivalent (loans, IP-protected licensing, market-rate consulting)?',
    'Grant-equivalent analysis of climate finance flows reported to the UNFCCC finance mechanism, disaggregating concessional versus non-concessional and IP-encumbered versus open technology transfer.',
    'A low grant-equivalent fraction would support classifying the technology-transfer obligation as substantially extractive market-creation rather than genuine compensatory transfer, raising measured extractiveness further; a high fraction would narrow the gap between the two kernel readings'' practical effects.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_transfer_terms_effective_value, empirical, 'Whether technology transfer functions as compensation or as a subsidized export market.').

omega_variable(
    voluntary_reading_participation_counterfactual,
    'Would a binding, historical-responsibility-indexed regime have actually achieved lower aggregate emissions reductions than the voluntary regime, once accounting for non-ratification and non-compliance under a binding model (the Kyoto counterfactual)?',
    'Comparative analysis of Kyoto Protocol compliance and ratification patterns versus Paris Agreement NDC submission and partial-fulfillment patterns, modeling counterfactual aggregate emissions under each regime type.',
    'If the voluntary regime''s higher participation more than offsets its lower per-participant ambition, the coordination-function claim in this story is strengthened; if binding regimes with partial participation would still have delivered more aggregate mitigation, the coordination justification for the voluntary reading weakens substantially.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(voluntary_reading_participation_counterfactual, empirical, 'Whether universal-but-voluntary outperforms binding-but-partial on aggregate mitigation outcomes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cbdr_principle__voluntary_commitment_reading, 1992, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cbdr_tr_t1992, cbdr_principle__voluntary_commitment_reading, theater_ratio, 1992, 0.3).
narrative_ontology:measurement_basis(cbdr_tr_t1992, observed).
narrative_ontology:measurement(cbdr_tr_t1997, cbdr_principle__voluntary_commitment_reading, theater_ratio, 1997, 0.35).
narrative_ontology:measurement_basis(cbdr_tr_t1997, observed).
narrative_ontology:measurement(cbdr_tr_t2009, cbdr_principle__voluntary_commitment_reading, theater_ratio, 2009, 0.42).
narrative_ontology:measurement_basis(cbdr_tr_t2009, observed).
narrative_ontology:measurement(cbdr_tr_t2015, cbdr_principle__voluntary_commitment_reading, theater_ratio, 2015, 0.48).
narrative_ontology:measurement_basis(cbdr_tr_t2015, observed).
narrative_ontology:measurement(cbdr_tr_t2020, cbdr_principle__voluntary_commitment_reading, theater_ratio, 2020, 0.52).
narrative_ontology:measurement_basis(cbdr_tr_t2020, observed).
narrative_ontology:measurement(cbdr_tr_t2024, cbdr_principle__voluntary_commitment_reading, theater_ratio, 2024, 0.55).
narrative_ontology:measurement_basis(cbdr_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(cbdr_be_t1992, cbdr_principle__voluntary_commitment_reading, base_extractiveness, 1992, 0.28).
narrative_ontology:measurement_basis(cbdr_be_t1992, observed).
narrative_ontology:measurement(cbdr_be_t1997, cbdr_principle__voluntary_commitment_reading, base_extractiveness, 1997, 0.33).
narrative_ontology:measurement_basis(cbdr_be_t1997, observed).
narrative_ontology:measurement(cbdr_be_t2009, cbdr_principle__voluntary_commitment_reading, base_extractiveness, 2009, 0.4).
narrative_ontology:measurement_basis(cbdr_be_t2009, observed).
narrative_ontology:measurement(cbdr_be_t2015, cbdr_principle__voluntary_commitment_reading, base_extractiveness, 2015, 0.48).
narrative_ontology:measurement_basis(cbdr_be_t2015, observed).
narrative_ontology:measurement(cbdr_be_t2020, cbdr_principle__voluntary_commitment_reading, base_extractiveness, 2020, 0.54).
narrative_ontology:measurement_basis(cbdr_be_t2020, observed).
narrative_ontology:measurement(cbdr_be_t2024, cbdr_principle__voluntary_commitment_reading, base_extractiveness, 2024, 0.58).
narrative_ontology:measurement_basis(cbdr_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(cbdr_su_t1992, cbdr_principle__voluntary_commitment_reading, suppression_requirement, 1992, 0.2).
narrative_ontology:measurement_basis(cbdr_su_t1992, observed).
narrative_ontology:measurement(cbdr_su_t1997, cbdr_principle__voluntary_commitment_reading, suppression_requirement, 1997, 0.25).
narrative_ontology:measurement_basis(cbdr_su_t1997, observed).
narrative_ontology:measurement(cbdr_su_t2009, cbdr_principle__voluntary_commitment_reading, suppression_requirement, 2009, 0.3).
narrative_ontology:measurement_basis(cbdr_su_t2009, observed).
narrative_ontology:measurement(cbdr_su_t2015, cbdr_principle__voluntary_commitment_reading, suppression_requirement, 2015, 0.34).
narrative_ontology:measurement_basis(cbdr_su_t2015, observed).
narrative_ontology:measurement(cbdr_su_t2020, cbdr_principle__voluntary_commitment_reading, suppression_requirement, 2020, 0.38).
narrative_ontology:measurement_basis(cbdr_su_t2020, observed).
narrative_ontology:measurement(cbdr_su_t2024, cbdr_principle__voluntary_commitment_reading, suppression_requirement, 2024, 0.42).
narrative_ontology:measurement_basis(cbdr_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cbdr_principle__voluntary_commitment_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(cbdr_principle__voluntary_commitment_reading, 0.12).
narrative_ontology:affects_constraint(cbdr_principle__voluntary_commitment_reading, cbdr_principle__historical_responsibility_reading).
narrative_ontology:affects_constraint(cbdr_principle__voluntary_commitment_reading, loss_and_damage_fund_capitalization).
narrative_ontology:affects_constraint(cbdr_principle__voluntary_commitment_reading, paris_agreement_ndc_ratchet_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is the voluntary_commitment_reading half of the cbdr_principle kernel decomposition. The sibling story, cbdr_principle__historical_responsibility_reading, reads the identical UNFCCC/Paris text as mandating binding, cumulative-emissions-indexed cuts plus loss-and-damage entitlements. The two stories share a kernel but are not the same constraint: their ε values, beneficiary/victim sets, and claimed types differ (this story: tangled_rope, ε=0.58, developed nations largely exit the victim set; sibling: expected higher ε and a snare-leaning or tangled_rope profile with developed nations as primary payers). Both stories must link to each other via affects_constraints per the network decomposition rule.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
