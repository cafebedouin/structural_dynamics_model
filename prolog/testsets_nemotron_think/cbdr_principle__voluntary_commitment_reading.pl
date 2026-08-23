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
 *   human_readable: CBDR as Voluntary NDCs with Technology Transfer
 *   domain: international/climate/governance
 *
 * SUMMARY:
 *   This constraint story captures the 'voluntary commitment reading' of the
 *   CBDR principle — the interpretation that crystallized in the Paris
 *   Agreement. Under this reading, CBDR requires each nation to determine its
 *   own contribution (NDC) voluntarily, with developed nations' primary
 *   obligation being technology transfer and capacity building rather than
 *   binding emissions reductions. The structural delta from the kernel:
 *   developed nations exit the victim set for binding emissions constraints
 *   (they gain voluntary flexibility), while developing nations enter the
 *   victim set for adaptation costs without compensation guarantees (they
 *   lose the Kyoto-era annex protection). The constraint operates as a
 *   tangled rope: it genuinely coordinates universal participation
 *   (coordination function) but does so through asymmetric extraction where
 *   developing nations bear disproportionate physical and financial burdens
 *   without enforceable reciprocity.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cbdr_principle__voluntary_commitment_reading, 0.68).
domain_priors:suppression_score(cbdr_principle__voluntary_commitment_reading, 0.45).
domain_priors:theater_ratio(cbdr_principle__voluntary_commitment_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cbdr_principle__voluntary_commitment_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(cbdr_principle__voluntary_commitment_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(cbdr_principle__voluntary_commitment_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cbdr_principle__voluntary_commitment_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(cbdr_principle__voluntary_commitment_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cbdr_principle__voluntary_commitment_reading, tangled_rope).
narrative_ontology:human_readable(cbdr_principle__voluntary_commitment_reading, "CBDR as Voluntary NDCs with Technology Transfer").
narrative_ontology:topic_domain(cbdr_principle__voluntary_commitment_reading, "international/climate/governance").

domain_priors:requires_active_enforcement(cbdr_principle__voluntary_commitment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(cbdr_principle__voluntary_commitment_reading, '049e516f-0220-4977-9cc5-c0f8b2a67ccf').
narrative_ontology:cs_kernel_codification('049e516f-0220-4977-9cc5-c0f8b2a67ccf', formalized).
narrative_ontology:cs_authority_grounding('049e516f-0220-4977-9cc5-c0f8b2a67ccf', lineage).
narrative_ontology:cs_interpretation_layer_present('049e516f-0220-4977-9cc5-c0f8b2a67ccf').
narrative_ontology:cs_reading_relation('049e516f-0220-4977-9cc5-c0f8b2a67ccf', cbdr_principle__historical_responsibility_reading, coexists_with).
narrative_ontology:cs_axiom('049e516f-0220-4977-9cc5-c0f8b2a67ccf', foundational, voluntary_ndc_sovereignty).
narrative_ontology:cs_axiom_status(voluntary_ndc_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('049e516f-0220-4977-9cc5-c0f8b2a67ccf', voluntary_ndc_sovereignty, conventional).
narrative_ontology:cs_axiom('049e516f-0220-4977-9cc5-c0f8b2a67ccf', secondary, technology_transfer_primary_obligation).
narrative_ontology:cs_axiom_status(technology_transfer_primary_obligation, holdable).
narrative_ontology:cs_axiom_grounding('049e516f-0220-4977-9cc5-c0f8b2a67ccf', technology_transfer_primary_obligation, conventional).
narrative_ontology:cs_reference_frame('049e516f-0220-4977-9cc5-c0f8b2a67ccf', unfccc_1992_cbdrs_framework).
narrative_ontology:cs_drift_state('049e516f-0220-4977-9cc5-c0f8b2a67ccf', paris_agreement_2015, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('049e516f-0220-4977-9cc5-c0f8b2a67ccf', '').
narrative_ontology:cs_kernel_id(cbdr_principle__voluntary_commitment_reading, cbdr_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cbdr_principle__voluntary_commitment_reading, developed_nation_governments).
narrative_ontology:constraint_beneficiary(cbdr_principle__voluntary_commitment_reading, fossil_fuel_interests_developed).
narrative_ontology:constraint_victim(cbdr_principle__voluntary_commitment_reading, developing_nation_governments).
narrative_ontology:constraint_victim(cbdr_principle__voluntary_commitment_reading, climate_vulnerable_populations).
narrative_ontology:constraint_vindicates(cbdr_principle__voluntary_commitment_reading, national_sovereignty_in_climate_action).
narrative_ontology:constraint_vindicates(cbdr_principle__voluntary_commitment_reading, technology_transfer_as_primary_obligation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Negotiate and administer the UNFCCC/Paris framework. Avoid binding emissions constraints by championing voluntary NDCs. Provide technology transfer financing on voluntary terms. Control the financial and technical assistance channels that developing nations depend on.
narrative_ontology:constraint_stakeholder(cbdr_principle__voluntary_commitment_reading, developed_nation_governments, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(cbdr_principle__voluntary_commitment_reading, developed_nation_governments, beneficiary).

% Domestic fossil fuel industries in developed nations benefit from the absence of binding emissions caps. The voluntary framework allows continued extraction and combustion while technology transfer rhetoric provides political cover. They influence national NDC ambition levels through domestic lobbying.
narrative_ontology:constraint_stakeholder(cbdr_principle__voluntary_commitment_reading, fossil_fuel_interests_developed, beneficiary,
    organized, biographical, mobile, global).

% Submit NDCs under pressure to demonstrate participation. Bear disproportionate adaptation costs with no guaranteed compensation. Depend on voluntary technology transfer and climate finance that consistently falls short of pledged levels. Have limited leverage to enforce developed nation obligations.
narrative_ontology:constraint_stakeholder(cbdr_principle__voluntary_commitment_reading, developing_nation_governments, payer,
    moderate, biographical, constrained, global).

% Experience climate impacts directly (sea level rise, extreme heat, crop failure). Have no voice in NDC formulation or international negotiations. Bear adaptation costs personally while international finance mechanisms bypass local communities. No exit option from climate exposure.
narrative_ontology:constraint_stakeholder(cbdr_principle__voluntary_commitment_reading, climate_vulnerable_populations, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(cbdr_principle__voluntary_commitment_reading, climate_vulnerable_populations, excluded).

% Administer climate finance mechanisms (GCF, GEF, MDB climate windows). Shape technology transfer through lending conditions and project selection. Report on finance flows but cannot compel developed nation contributions. Their operational rules structure what counts as 'technology transfer'.
narrative_ontology:constraint_stakeholder(cbdr_principle__voluntary_commitment_reading, international_financial_institutions, observer,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(cbdr_principle__voluntary_commitment_reading, international_financial_institutions, agenda_setter).

% Advocate for historical responsibility reading, loss and damage finance, and binding equity. Provide independent assessment of NDC adequacy and finance delivery. Excluded from formal negotiation rooms; influence through public pressure, litigation, and side events.
narrative_ontology:constraint_stakeholder(cbdr_principle__voluntary_commitment_reading, civil_society_climate_justice, observer,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(cbdr_principle__voluntary_commitment_reading, civil_society_climate_justice, excluded).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global climate action without a central enforcement authority by letting each nation set its own contribution level, while creating a shared transparency framework and periodic global stocktake to ratchet ambition over time.
% TRANSFER_FUNCTION: Moves the burden of emissions reduction from a developed-nation-only obligation (Kyoto model) to a universal voluntary framework, while moving the promise (not guarantee) of technology transfer and finance from developed to developing nations. Adaptation costs remain with developing nations.
% ABSENT_VOICES: Climate vulnerable populations (especially indigenous communities, small island states, least developed countries' rural poor) are structurally excluded from NDC formulation and COP decision-making. Their objections to voluntary adequacy and finance shortfalls are recorded in side events but not in the decision text.
% DISAPPEARANCE_RATIONALE: If the voluntary NDC framework vanished, the Kyoto-era annex division would likely reassert (binding targets for Annex I only), or a new equity-based allocation would be negotiated. Developing nations would demand binding developed nation cuts + guaranteed finance. The Paris architecture's universal participation would collapse.
% FOUNDING_PROBLEM: The Kyoto Protocol's binding-targets-for-developed-nations-only model failed: the US never ratified, Canada withdrew, and emerging economy emissions grew uncovered. A new model was needed to bring all nations into a single framework.
% FOUNDING_PROBLEM_CORROBORATION: Developed nations and UNFCCC secretariat attest the universal participation problem is solved by Paris. Developing nations and climate justice NGOs attest the equity problem is worsened — universal participation came at the cost of dismantling the developed-nation obligation structure. Independent legal scholarship (e.g., Rajamani, Mayer) corroborates the shift from obligation to voluntariness.
narrative_ontology:disappearance_verdict(cbdr_principle__voluntary_commitment_reading, world_rearranges).
narrative_ontology:founding_problem_status(cbdr_principle__voluntary_commitment_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(cbdr_principle__voluntary_commitment_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(cbdr_principle__voluntary_commitment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(cbdr_principle__voluntary_commitment_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.68) is substantial because the voluntary framework shifts mitigation burden onto developing nations while developed nation finance/tech transfer pledges remain unmet. Suppression (0.45) is moderate — not overt coercion but structural: developing nations cannot exit the climate regime without catastrophic isolation, and the transparency framework creates naming/shaming pressure. Theater ratio (0.52) is high — the global stocktake and NDC cycles perform coordination while the emissions gap widens. Accessibility collapse (0.62) reflects the political impossibility of returning to binding annex differentiation after Paris. Resistance (0.58) captures developing nation pushback (Like-Minded Developing Countries, African Group, AOSIS) in negotiations and climate litigation.
 *
 * PERSPECTIVAL GAP:
 *   The developed nation seat computes as rope (genuine coordination of universal participation), while developing nation and vulnerable population seats compute as snare/tangled rope (extraction without enforceable reciprocity). The engine computes this divergence from the structural data: same constraint, different directionalities, different effective extraction. The voluntary_commitment_reading itself claims rope; the metrics describe tangled rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Developed nation governments are structural beneficiaries (d ~ 0.15) — they set the agenda, avoid binding targets, control finance channels. Fossil fuel interests in developed nations are beneficiaries (d ~ 0.1) — voluntary NDCs protect their asset base. Developing nation governments are payers (d ~ 0.75) — they submit NDCs under pressure, bear adaptation costs, depend on voluntary finance. Climate vulnerable populations are trapped payers (d ~ 0.95) — no voice, no exit, bear physical costs. International financial institutions sit near analytical (d ~ 0.5) — they administer but don't capture extraction. Civil society observers are analytical (d ~ 0.5) with excluded secondary role.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (universal participation after Kyoto's failure) is contested — solved for participation, unsolved for equity. The constraint persists because it serves developed nation interests (no binding caps) while maintaining the appearance of global cooperation. Mandatrophy is unresolved: the original mandate (equitable burden-sharing per CBDR) has been reinterpreted into a form that no longer serves its stated purpose for half the parties.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    voluntary_vs_binding_naturalness,
    'Is the voluntary NDC framework a genuine coordination innovation or a constructed retreat from developed nation obligations?',
    'Counterfactual analysis: if Kyoto''s annex structure had been enforced with US participation, would global emissions be lower? Compare emissions trajectories under binding vs voluntary regimes using integrated assessment models with political economy constraints.',
    'If constructed retreat, the constraint is a false summit (mountain claim masking extraction) — FSM signature would trigger. If genuine innovation, the coordination function justifies some extraction as transaction cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(voluntary_vs_binding_naturalness, conceptual, 'Whether voluntariness is a natural law of international cooperation or a political choice benefiting developed nations.').

omega_variable(
    technology_transfer_delivery_gap,
    'Is the technology transfer obligation under this reading structurally fulfillable, or is it a performative promise that masks extraction?',
    'Track actual technology transfer flows (patent licensing, joint R&D, capacity building) against NDC implementation needs identified by developing nations. Assess whether the UNFCCC Technology Mechanism delivers at scale.',
    'If unfulfillable, the technology transfer promise is theater — extraction ratio rises. If deliverable but underfunded, it''s a resource allocation problem (tangled rope with fixable coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_transfer_delivery_gap, empirical, 'Whether the primary developed nation obligation in this reading is real or rhetorical.').

omega_variable(
    adaptation_cost_allocation,
    'What fraction of developing nation adaptation costs are actually covered by international finance vs. domestically borne?',
    'OECD/UNFCCC biennial finance reports cross-referenced with developing nation adaptation cost estimates (UNEP Adaptation Gap Reports). Disaggregate grant vs loan, mitigation vs adaptation, and additionality.',
    'If coverage is <20% (current trajectory), developing nations are de facto payers for a crisis they didn''t cause — extraction is structural. If coverage approaches 50%+, the coordination function has material reciprocity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(adaptation_cost_allocation, empirical, 'The material balance of adaptation finance — the core test of whether this reading delivers on CBDR''s equity promise.').

omega_variable(
    kernel_framing_underdetermination,
    'Does the voluntary_commitment_reading foreclose the historical_responsibility_reading within a single legal framework, or do they coexist as competing interpretations of the same treaty text?',
    'Legal analysis of Paris Agreement Articles 2.2, 4, 9, and 13: can a single party simultaneously hold that (a) NDCs are nationally determined and (b) developed nations have binding proportional obligations? Court and tribunal jurisprudence on treaty interpretation (VCLT Art. 31).',
    'If forecloses, the kernel has a genuine logical split — the two readings cannot coexist in one framework, making the choice between them a structural commitment. If coexists_with, the ambiguity is exploited by developed nations to claim equity while practicing voluntariness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the two CBDR readings are logically incompatible or politically competing within the same treaty.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cbdr_principle__voluntary_commitment_reading, 1992, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cbdr_voluntary_tr_t1992, cbdr_principle__voluntary_commitment_reading, theater_ratio, 1992, 0.25).
narrative_ontology:measurement(cbdr_voluntary_tr_t1997, cbdr_principle__voluntary_commitment_reading, theater_ratio, 1997, 0.3).
narrative_ontology:measurement(cbdr_voluntary_tr_t2009, cbdr_principle__voluntary_commitment_reading, theater_ratio, 2009, 0.4).
narrative_ontology:measurement(cbdr_voluntary_tr_t2015, cbdr_principle__voluntary_commitment_reading, theater_ratio, 2015, 0.48).
narrative_ontology:measurement(cbdr_voluntary_tr_t2021, cbdr_principle__voluntary_commitment_reading, theater_ratio, 2021, 0.5).
narrative_ontology:measurement(cbdr_voluntary_tr_t2030, cbdr_principle__voluntary_commitment_reading, theater_ratio, 2030, 0.52).

% Extraction over time
narrative_ontology:measurement(cbdr_voluntary_be_t1992, cbdr_principle__voluntary_commitment_reading, base_extractiveness, 1992, 0.35).
narrative_ontology:measurement(cbdr_voluntary_be_t1997, cbdr_principle__voluntary_commitment_reading, base_extractiveness, 1997, 0.28).
narrative_ontology:measurement(cbdr_voluntary_be_t2009, cbdr_principle__voluntary_commitment_reading, base_extractiveness, 2009, 0.42).
narrative_ontology:measurement(cbdr_voluntary_be_t2015, cbdr_principle__voluntary_commitment_reading, base_extractiveness, 2015, 0.55).
narrative_ontology:measurement(cbdr_voluntary_be_t2021, cbdr_principle__voluntary_commitment_reading, base_extractiveness, 2021, 0.62).
narrative_ontology:measurement(cbdr_voluntary_be_t2030, cbdr_principle__voluntary_commitment_reading, base_extractiveness, 2030, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(cbdr_voluntary_su_t1992, cbdr_principle__voluntary_commitment_reading, suppression_requirement, 1992, 0.2).
narrative_ontology:measurement(cbdr_voluntary_su_t1997, cbdr_principle__voluntary_commitment_reading, suppression_requirement, 1997, 0.35).
narrative_ontology:measurement(cbdr_voluntary_su_t2009, cbdr_principle__voluntary_commitment_reading, suppression_requirement, 2009, 0.4).
narrative_ontology:measurement(cbdr_voluntary_su_t2015, cbdr_principle__voluntary_commitment_reading, suppression_requirement, 2015, 0.42).
narrative_ontology:measurement(cbdr_voluntary_su_t2021, cbdr_principle__voluntary_commitment_reading, suppression_requirement, 2021, 0.44).
narrative_ontology:measurement(cbdr_voluntary_su_t2030, cbdr_principle__voluntary_commitment_reading, suppression_requirement, 2030, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cbdr_principle__voluntary_commitment_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(cbdr_principle__voluntary_commitment_reading, 0.22).
narrative_ontology:affects_constraint(cbdr_principle__voluntary_commitment_reading, paris_agreement_ndc_framework).
narrative_ontology:affects_constraint(cbdr_principle__voluntary_commitment_reading, climate_finance_architecture).
narrative_ontology:affects_constraint(cbdr_principle__voluntary_commitment_reading, technology_mechanism_unfccc).
narrative_ontology:affects_constraint(cbdr_principle__voluntary_commitment_reading, loss_and_damage_fund).

% DUAL FORMULATION NOTE:
% This constraint (voluntary_commitment_reading) and historical_responsibility_reading form the cbdr_principle constraint family. They share the kernel (CBDR principle) but have different ε values: this reading ε≈0.68 (extraction via voluntariness), sibling reading ε≈0.35 (binding equity with coordination). The upstream UNFCCC 1992 text influences both; Paris 2015 crystallized this reading as the operational framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cbdr_principle__voluntary_commitment_reading, institutional, 0.15).
constraint_indexing:directionality_override(cbdr_principle__voluntary_commitment_reading, moderate, 0.75).
constraint_indexing:directionality_override(cbdr_principle__voluntary_commitment_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
