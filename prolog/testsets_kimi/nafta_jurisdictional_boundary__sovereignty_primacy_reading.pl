% ============================================================================
% CONSTRAINT STORY: nafta_jurisdictional_boundary__sovereignty_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nafta_jurisdictional_boundary__sovereignty_primacy_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: nafta_jurisdictional_boundary__sovereignty_primacy_reading
 *   human_readable: NAFTA Jurisdictional Boundary â Sovereignty Primacy Reading
 *   domain: international_trade_law/political_economy
 *
 * SUMMARY:
 *   This constraint instantiates the sovereignty-primacy reading of the NAFTA
 *   jurisdictional boundary kernel. Under this reading, the trade agreement
 *   text functions as a coordination mechanism that remains subordinate to
 *   sovereign domestic law; signatory states and their regulatory agencies
 *   retain full jurisdictional authority over labor, environmental, and
 *   health standards. The treaty enters the compliance-cost set of domestic
 *   regulators but does not override them. This reading stands in direct
 *   contest with the capital-supremacy reading (treaty as overriding domestic
 *   regulation) and the embedded-liberalism reading (balanced framework
 *   allowing non-discriminatory domestic policy space). As a kernel reading,
 *   it is authored as a clean Îµ-invariant constraint: low extractiveness,
 *   low suppression, and a coordination function that solves a genuine
 *   collective-action problem without coercive overhead.
 *
 * KEY AGENTS:
 *   - signatory_states: Primary agenda-setter (institutional/continental) â negotiate and maintain the treaty as subordinate to constitutional order
 *   - domestic_regulatory_agencies: Primary beneficiary (institutional/national) â retain full jurisdictional authority over standards
 *   - cross_border_firms: Dual-positioned beneficiary/payer (powerful/continental) â gain market access predictability, bear voluntary compliance costs
 *   - investor_state_arbitration_proponents: Excluded voice (organized/global) â seek treaty supremacy over domestic regulation, not authoritative under this reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0.18).
domain_priors:suppression_score(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0.22).
domain_priors:theater_ratio(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nafta_jurisdictional_boundary__sovereignty_primacy_reading, rope).
narrative_ontology:human_readable(nafta_jurisdictional_boundary__sovereignty_primacy_reading, "NAFTA Jurisdictional Boundary â Sovereignty Primacy Reading").
narrative_ontology:topic_domain(nafta_jurisdictional_boundary__sovereignty_primacy_reading, "international_trade_law/political_economy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nafta_jurisdictional_boundary__sovereignty_primacy_reading, '2d98dd3d-1784-4e01-8807-cb1dc6468169').
narrative_ontology:cs_kernel_codification('2d98dd3d-1784-4e01-8807-cb1dc6468169', formalized).
narrative_ontology:cs_authority_grounding('2d98dd3d-1784-4e01-8807-cb1dc6468169', distributed).
narrative_ontology:cs_reading_relation('2d98dd3d-1784-4e01-8807-cb1dc6468169', nafta_jurisdictional_boundary__capital_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('2d98dd3d-1784-4e01-8807-cb1dc6468169', nafta_jurisdictional_boundary__embedded_liberalism_reading, coexists_with).
narrative_ontology:cs_axiom('2d98dd3d-1784-4e01-8807-cb1dc6468169', foundational, domestic_regulatory_authority_is_non_derogable).
narrative_ontology:cs_axiom_status(domestic_regulatory_authority_is_non_derogable, holdable).
narrative_ontology:cs_axiom_grounding('2d98dd3d-1784-4e01-8807-cb1dc6468169', domestic_regulatory_authority_is_non_derogable, conventional).
narrative_ontology:cs_axiom('2d98dd3d-1784-4e01-8807-cb1dc6468169', foundational, treaty_obligations_are_compliance_costs_not_commands).
narrative_ontology:cs_axiom_status(treaty_obligations_are_compliance_costs_not_commands, holdable).
narrative_ontology:cs_axiom_grounding('2d98dd3d-1784-4e01-8807-cb1dc6468169', treaty_obligations_are_compliance_costs_not_commands, conventional).
narrative_ontology:cs_reference_frame('2d98dd3d-1784-4e01-8807-cb1dc6468169', westphalian_trade_coordination).
narrative_ontology:cs_drift_state('2d98dd3d-1784-4e01-8807-cb1dc6468169', contemporary_trade_dispute_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2d98dd3d-1784-4e01-8807-cb1dc6468169', '').
narrative_ontology:cs_kernel_id(nafta_jurisdictional_boundary__sovereignty_primacy_reading, nafta_jurisdictional_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__sovereignty_primacy_reading, signatory_states).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__sovereignty_primacy_reading, domestic_regulatory_agencies).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__sovereignty_primacy_reading, cross_border_firms).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__sovereignty_primacy_reading, cross_border_firms).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Negotiated the treaty and maintain the formal position that its obligations do not override domestic constitutional or statutory authority over labor, environmental, and health standards. They retain the legal capacity to withdraw from the treaty through prescribed procedures.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, signatory_states, agenda_setter,
    institutional, generational, mobile, continental).

% Administer and enforce national standards in labor, environment, and public health without recognizing treaty obligations as a superseding legal authority; they treat treaty compliance as one administrative cost input among many in regulatory analysis.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, domestic_regulatory_agencies, beneficiary,
    institutional, generational, constrained, national).

% Operate production and distribution networks across the treaty jurisdiction; they follow customs and documentation procedures to qualify for preferential market access while remaining subject to the full range of domestic regulation in each country where they operate.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, cross_border_firms, beneficiary,
    powerful, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(nafta_jurisdictional_boundary__sovereignty_primacy_reading, cross_border_firms, payer).

% Promote legal frameworks that allow private investors to challenge domestic regulations directly through international arbitration; they argue that treaty obligations should be enforceable against domestic law, a position that is not adopted as authoritative under this reading of the text.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, investor_state_arbitration_proponents, excluded,
    organized, biographical, mobile, global).

% Study whether the treaty's architecture actually reduces trade costs or merely layers procedural requirements on top of existing domestic regulatory systems; they publish comparative analyses of trade flows, regulatory autonomy, and compliance burdens.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, trade_economists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nafta_jurisdictional_boundary__sovereignty_primacy_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinate cross-border trade and investment by reducing transaction costs and creating predictable rules, while preserving the autonomy of domestic regulatory agencies over labor, environmental, and health standards.
% TRANSFER_FUNCTION: Moves market-access predictability and regulatory clarity from the treaty framework to signatory economies and firms; moves voluntary administrative compliance costs from firms to domestic regulatory agencies as an oversight burden.
% ABSENT_VOICES: Multinational capital interests and investor-state arbitration proponents who seek regulatory harmonization and the supremacy of capital mobility over domestic standards; they are structurally marginalized in this reading because the treaty text is interpreted as subordinate to sovereign law.
% DISAPPEARANCE_RATIONALE: If this sovereignty-primacy reading of the treaty text vanished, signatory states would lose the external coordination framework that legitimates their retained regulatory authority; firms would face less predictable trade rules; domestic agencies would revert to purely unilateral regulatory oversight without the compliance-cost reference point of the treaty.
% FOUNDING_PROBLEM: How to facilitate cross-border trade and investment among sovereign states without ceding domestic regulatory authority over labor standards, environmental protection, and public health.
% FOUNDING_PROBLEM_CORROBORATION: Public international law scholars and comparative constitutional courts outside the direct trade negotiation process attest that the problem of balancing trade liberalization with regulatory sovereignty remains live; however, trade economists and investor-rights advocates outside the benefiting state parties contest whether sovereignty primacy is the correct or sufficient response.
narrative_ontology:disappearance_verdict(nafta_jurisdictional_boundary__sovereignty_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(nafta_jurisdictional_boundary__sovereignty_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nafta_jurisdictional_boundary__sovereignty_primacy_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0.18, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nafta_jurisdictional_boundary__sovereignty_primacy_reading_tests).
:- end_tests(nafta_jurisdictional_boundary__sovereignty_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18) because treaty obligations are framed as entering the compliance-cost set rather than as overriding commands; costs are voluntary and borne symmetrically by firms seeking coordination benefits. Suppression is low (0.22) because the constraint's persistence does not depend on suppressing domestic regulatory alternatives â states retain the formal authority to regulate. Theater ratio is moderate (0.35) because the repeated textual assertion of sovereignty in treaty preambles and side letters increasingly outpaces the actual practice of dispute settlement, creating a performative gap. Accessibility collapse is low (0.30) because alternatives (pure domestic regulation, non-treaty trade) remain viable. Resistance is moderate (0.40) from capital-mobility advocates who contest the sovereignty-primacy framing. The measurement series tracks a modest drift: base extractiveness creeps upward slightly as compliance architectures thicken, while theater rises as the gap between sovereignty rhetoric and institutional practice widens.
 *
 * PERSPECTIVAL GAP:
 *   The signatory-state seat and the domestic-regulatory-agency seat both experience this constraint as protective of their authority. The cross-border-firm seat experiences it as a cost-benefit trade-off near symmetric. The excluded capital-supremacy seat would experience the same treaty text as an entirely different constraint with high extraction and high suppression â this divergence is not perspectival within one constraint but a kernel-level reading split, handled by the Îµ-invariance decomposition into separate stories.
 *
 * DIRECTIONALITY LOGIC:
 *   Signatory states and domestic regulatory agencies are structural beneficiaries of the sovereignty-primacy reading because the constraint vindicates their authority and does not extract from them. Cross-border firms sit near symmetric: they receive coordination value (predictable rules) and pay compliance costs. No stakeholder is positioned as a pure target under this reading; the absence of a victim group is consistent with the rope classification. The directionality derivation produces low d for state agencies and moderate d for firms, yielding low effective extraction across all seats.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists mandatrophy mislabeling by grounding the constraint in a live coordination problem (reducing trade transaction costs) with a clearly identified beneficiary set and no identified victims. If the treaty text were instead read as overriding domestic law (the capital-supremacy reading), it would instantiate a different constraint with different stakeholders and higher extraction â the kernel decomposition prevents conflating the two. The rope classification here is protected by the absence of coercion and the presence of viable alternatives; should dispute settlement practice erode sovereignty to the point that domestic alternatives collapse, the constraint would need to be re-authored as a tangled_rope or snare under a different reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_primacy_operational_reality,
    'Does the treaty text''s subordination to domestic law hold in practice, or do dispute settlement mechanisms and market-access conditionalities effectively override sovereign regulatory authority despite the textual claim?',
    'Comparative case study of domestic regulatory decisions that were challenged under treaty dispute mechanisms versus those that were not, measuring the rate of regulatory chill and panel override.',
    'If dispute panels systematically override domestic authority despite textual sovereignty reservations, the effective extraction is higher than the reading claims and the constraint may compute as tangled_rope or snare from the payer seat; if textual subordination holds, the rope classification is sustained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_primacy_operational_reality, empirical, 'Whether sovereignty primacy is operational or merely textual').

omega_variable(
    voluntary_compliance_cost_asymmetry,
    'Are compliance costs borne by cross-border firms truly voluntary, or do market structure and competitive pressure convert them into mandatory costs for market participation?',
    'Economic analysis of firm exit and entry rates in treaty-regulated markets versus non-treaty markets; surveys of firm compliance expenditure as share of revenue.',
    'If costs are structurally mandatory for competitive survival, firms are targets with high directionality rather than symmetric beneficiaries; if genuinely voluntary, the coordination function dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_compliance_cost_asymmetry, empirical, 'Whether compliance costs are voluntary or structurally coerced').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(naft_tr_t0, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(naft_tr_t10, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(naft_tr_t20, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 20, 0.34).
narrative_ontology:measurement(naft_tr_t30, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 30, 0.4).

% Extraction over time
narrative_ontology:measurement(naft_be_t0, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(naft_be_t10, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 10, 0.14).
narrative_ontology:measurement(naft_be_t20, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 20, 0.16).
narrative_ontology:measurement(naft_be_t30, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 30, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(nafta_jurisdictional_boundary__sovereignty_primacy_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__sovereignty_primacy_reading, capital_supremacy_reading).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__sovereignty_primacy_reading, embedded_liberalism_reading).

% DUAL FORMULATION NOTE:
% This constraint is the sovereignty_primacy_reading of the nafta_jurisdictional_boundary kernel. The kernel decomposes into three structurally distinct claims: capital_supremacy (high extraction, overriding constraints), embedded_liberalism (balanced coordination), and sovereignty_primacy (subordinate coordination, low extraction). Each reading instantiates a different constraint with different Îµ values and different stakeholder asymmetries.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
