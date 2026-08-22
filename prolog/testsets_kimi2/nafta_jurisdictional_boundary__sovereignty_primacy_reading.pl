% ============================================================================
% CONSTRAINT STORY: nafta_jurisdictional_boundary__sovereignty_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: nafta_jurisdictional_boundary__sovereignty_primacy_reading
 *   human_readable: NAFTA Jurisdictional Boundary â Sovereignty Primacy Reading
 *   domain: international_trade_law/political_economy/regulatory_federalism
 *
 * SUMMARY:
 *   This constraint story instantiates the sovereignty_primacy_reading of the
 *   nafta_jurisdictional_boundary kernel. The colloquial label 'NAFTA
 *   jurisdictional boundary' conflates three structurally distinct
 *   commitments: treaty-as-supreme-law (capital_supremacy_reading),
 *   treaty-as-balanced-framework (embedded_liberalism_reading), and
 *   treaty-as-subordinate-coordination (this reading). This reading treats
 *   the trade agreement as a coordination mechanism that preserves full
 *   domestic regulatory sovereignty, with extraction limited to voluntary
 *   compliance costs. The low Îµ reflects this reading's assessment that the
 *   standing arrangement does not override domestic law.
 *
 * KEY AGENTS:
 *   - Signatory states (agenda_setter/beneficiary): retain full regulatory sovereignty and administer the treaty as subordinate to domestic law.
 *   - Domestic regulatory agencies (agenda_setter): set and enforce standards without jurisdictional override.
 *   - Cross-border firms (beneficiary): voluntarily incur compliance costs for coordinated market access.
 *   - Domestic labor groups and public health advocates (excluded): structurally absent from interpretation forums despite sensitivity to standard erosion.
 *   - Competing trade blocs (observer): benchmark the architecture against alternative designs.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0.15).
domain_priors:suppression_score(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0.2).
domain_priors:theater_ratio(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nafta_jurisdictional_boundary__sovereignty_primacy_reading, rope).
narrative_ontology:human_readable(nafta_jurisdictional_boundary__sovereignty_primacy_reading, "NAFTA Jurisdictional Boundary â Sovereignty Primacy Reading").
narrative_ontology:topic_domain(nafta_jurisdictional_boundary__sovereignty_primacy_reading, "international_trade_law/political_economy/regulatory_federalism").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 'dbb0a065-df6d-48a0-a2b6-a8a811c2dc38').
narrative_ontology:cs_kernel_codification('dbb0a065-df6d-48a0-a2b6-a8a811c2dc38', formalized).
narrative_ontology:cs_authority_grounding('dbb0a065-df6d-48a0-a2b6-a8a811c2dc38', lineage).
narrative_ontology:cs_interpretation_layer_present('dbb0a065-df6d-48a0-a2b6-a8a811c2dc38').
narrative_ontology:cs_reading_relation('dbb0a065-df6d-48a0-a2b6-a8a811c2dc38', nafta_jurisdictional_boundary__capital_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('dbb0a065-df6d-48a0-a2b6-a8a811c2dc38', nafta_jurisdictional_boundary__embedded_liberalism_reading, influences).
narrative_ontology:cs_axiom('dbb0a065-df6d-48a0-a2b6-a8a811c2dc38', foundational, domestic_regulatory_sovereignty_supreme).
narrative_ontology:cs_axiom_status(domestic_regulatory_sovereignty_supreme, holdable).
narrative_ontology:cs_axiom_grounding('dbb0a065-df6d-48a0-a2b6-a8a811c2dc38', domestic_regulatory_sovereignty_supreme, conventional).
narrative_ontology:cs_axiom('dbb0a065-df6d-48a0-a2b6-a8a811c2dc38', secondary, market_access_coordination_without_jurisdictional_transfer).
narrative_ontology:cs_axiom_status(market_access_coordination_without_jurisdictional_transfer, holdable).
narrative_ontology:cs_axiom_grounding('dbb0a065-df6d-48a0-a2b6-a8a811c2dc38', market_access_coordination_without_jurisdictional_transfer, instrumental).
narrative_ontology:cs_reference_frame('dbb0a065-df6d-48a0-a2b6-a8a811c2dc38', sovereign_domestic_authority_as_baseline).
narrative_ontology:cs_drift_state('dbb0a065-df6d-48a0-a2b6-a8a811c2dc38', post_isds_expansion_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('dbb0a065-df6d-48a0-a2b6-a8a811c2dc38', '').
narrative_ontology:cs_kernel_id(nafta_jurisdictional_boundary__sovereignty_primacy_reading, nafta_jurisdictional_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__sovereignty_primacy_reading, signatory_states).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__sovereignty_primacy_reading, cross_border_firms).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Negotiate and administer the trade agreement, designating treaty obligations as explicitly subordinate to domestic legislative and regulatory authority. They retain full jurisdiction over labor, environmental, and health standards within their territories and may amend, renegotiate, or withdraw from the agreement through sovereign political processes.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, signatory_states, agenda_setter,
    institutional, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(nafta_jurisdictional_boundary__sovereignty_primacy_reading, signatory_states, beneficiary).

% Set and enforce labor, environmental, and health standards for goods and services traded within their jurisdiction. They treat international treaty obligations as one compliance-cost input among many, without ceding jurisdictional authority to supranational dispute bodies or harmonization mandates.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, domestic_regulatory_agencies, agenda_setter,
    institutional, generational, mobile, national).

% Access foreign markets under coordinated tariff schedules and procedural rules. They voluntarily incur compliance costs to meet distinct domestic standards in each jurisdiction, benefiting from predictability without facing treaty-backed extraction beyond administrative burdens.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, cross_border_firms, beneficiary,
    powerful, biographical, mobile, continental).

% Represent workers potentially affected by trade-driven regulatory competition, but are structurally underrepresented in trade-negotiation and treaty-interpretation forums. Under the sovereignty-primacy framing, their interests are protected by domestic democratic control over standards, yet they lack direct voice in the constraint's administration.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, domestic_labor_groups, excluded,
    organized, biographical, constrained, national).

% Advance health-protective regulations that may conflict with trade-facilitation goals. They are absent from dispute-settlement panels and treaty committees, relying instead on domestic regulatory agencies to maintain standards sovereignty.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, public_health_advocates, excluded,
    organized, biographical, constrained, national).

% Observe the agreement's sovereignty-preserving architecture as a benchmark for their own regional negotiations. They do not participate in the constraint but assess whether its jurisdictional boundaries offer a viable model for market access without sovereignty transfer.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, competing_trade_blocs, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinate cross-border trade procedures, tariff schedules, and customs facilitation among sovereign states while preserving distinct domestic regulatory regimes and avoiding jurisdictional override.
% TRANSFER_FUNCTION: Moves procedural certainty, tariff predictability, and market-access rules from the coordinating treaty framework to signatory states and cross-border firms; compliance and administrative costs move from firms to the domestic regulatory systems they navigate.
% ABSENT_VOICES: Domestic labor groups and public health advocates who fear regulatory chill or downward harmonization are structurally excluded from treaty interpretation and dispute-settlement forums; their absence from the interpretive layer means sovereignty-preserving claims go unchallenged by the seats most sensitive to standard erosion.
% DISAPPEARANCE_RATIONALE: If the constraint vanished overnight, cross-border supply chains would lose coordinated tariff and procedural rules, firms would face fragmented national customs regimes without reciprocal access guarantees, and signatory states would revert to pre-agreement bilateral bargaining â the North American trade architecture would reorganize around ad hoc arrangements.
% FOUNDING_PROBLEM: Post-war trade fragmentation and protectionist tariff escalation reducing economic welfare; the need for predictable market-access rules without ceding domestic regulatory sovereignty to supranational authority.
% FOUNDING_PROBLEM_CORROBORATION: Independent international legal scholars and heterodox political economists outside the direct state-beneficiary set attest that sovereignty-preserving trade coordination remains a live structural alternative to capital-supremacy frameworks; historical records of pre-NAFTA tariff disputes and bilateral friction corroborate the original fragmentation problem.
narrative_ontology:disappearance_verdict(nafta_jurisdictional_boundary__sovereignty_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(nafta_jurisdictional_boundary__sovereignty_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0.15, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is low (0.15) because the reading assesses the constraint as moving only voluntary compliance costs, not rents. Suppression is low (0.20) because states retain exit and renegotiation options; coercion is minimal. Accessibility_collapse is low (0.25) because domestic regulatory alternatives remain fully open under the sovereignty-preservation frame. Resistance is low (0.15) because the coordinated parties are net beneficiaries. Theater_ratio is low (0.10) because there is little performative maintenance â the coordination function is the steady-state activity. The measurement series are flat with slight initial ramp, reflecting stable operation over the interval.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (signatory states and domestic agencies) experience the constraint as a voluntary coordination tool they control. The beneficiary seat (cross-border firms) experiences it as a facilitation mechanism with predictable rules. Excluded seats (labor, health advocates) would experience it as threatening if they believed sovereignty is eroded in practice, but under this reading's frame their protection is structurally preserved. The engine computes seat divergence from these structural positions; there is no high-d target seat because the reading declares no victims.
 *
 * DIRECTIONALITY LOGIC:
 *   Signatory states and domestic regulatory agencies sit at low directionality (near beneficiary) because they control the constraint and retain jurisdictional authority. Cross-border firms sit near symmetric (moderate directionality) because they voluntarily incur costs for market access. No agent is structurally targeted for extraction under this reading; the absence of declared victims means the directionality derivation produces no full-target seat, consistent with a rope profile.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem â fragmented trade and protectionism â remains live, and the arrangement continues to coordinate market access without overriding domestic authority. There is no evidence that the mandate has outlived its function. The sovereignty-primacy reading guards against mandatrophy by insisting that treaty obligations do not accumulate into overriding constraints; the treaty's renegotiation into USMCA demonstrates ongoing sovereign revision capacity that prevents institutional atrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    regulatory_chill_under_sovereignty_frame,
    'Does the threat of dispute settlement or retaliation induce de facto regulatory chill even when treaty text preserves formal domestic sovereignty?',
    'Comparative regulatory-output analysis across signatory jurisdictions before and after treaty accession, controlling for non-treaty confounders; qualitative interviews with regulatory staff on perceived constraints.',
    'If regulatory chill is documented, the constraint''s effective extractiveness and suppression exceed the sovereignty-primacy reading''s frame, and the computed classification shifts toward tangled_rope or snare for affected regulatory seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_chill_under_sovereignty_frame, empirical, 'Empirical gap between formal sovereignty and actual regulatory behavior').

omega_variable(
    kernel_reading_stability,
    'Is the sovereignty-primacy reading a stable structural interpretation of the treaty text, or does textual ambiguity make it contingently sustained by political will rather than legal architecture?',
    'Doctrinal analysis of treaty dispute-settlement outcomes and subsequent protocol amendments; tracking whether sovereignty-preserving clauses are interpreted as default or as defeasible exceptions.',
    'If the reading is politically contingent, the constraint''s classification as rope depends on ongoing political maintenance rather than textually fixed coordination; erosion of political support could convert it to a piton or expose hidden extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_stability, conceptual, 'Structural stability of the sovereignty-primacy interpretation').

omega_variable(
    compliance_cost_extraction_boundary,
    'At what threshold do voluntary compliance costs become extractive overhead rather than coordination cost?',
    'Economic benchmarking of compliance costs against comparable non-treaty trade-facilitation expenses; assessment of whether costs exceed the Boltzmann floor for resource_allocation coordination.',
    'If costs exceed the coordination floor, the excess is extractive by the engine''s Boltzmann test, reclassifying the constraint even if all participation remains formally voluntary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(compliance_cost_extraction_boundary, conceptual, 'Boundary between coordination cost and extraction for voluntary compliance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nafta_sov_prim_tr_t0, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(nafta_sov_prim_tr_t6, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 6, 0.09).
narrative_ontology:measurement(nafta_sov_prim_tr_t12, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 12, 0.1).
narrative_ontology:measurement(nafta_sov_prim_tr_t18, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 18, 0.1).
narrative_ontology:measurement(nafta_sov_prim_tr_t24, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 24, 0.11).
narrative_ontology:measurement(nafta_sov_prim_tr_t30, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 30, 0.1).

% Extraction over time
narrative_ontology:measurement(nafta_sov_prim_be_t0, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(nafta_sov_prim_be_t6, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 6, 0.13).
narrative_ontology:measurement(nafta_sov_prim_be_t12, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 12, 0.14).
narrative_ontology:measurement(nafta_sov_prim_be_t18, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 18, 0.15).
narrative_ontology:measurement(nafta_sov_prim_be_t24, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 24, 0.15).
narrative_ontology:measurement(nafta_sov_prim_be_t30, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 30, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(nafta_sov_prim_su_t0, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(nafta_sov_prim_su_t6, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 6, 0.17).
narrative_ontology:measurement(nafta_sov_prim_su_t12, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 12, 0.18).
narrative_ontology:measurement(nafta_sov_prim_su_t18, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 18, 0.19).
narrative_ontology:measurement(nafta_sov_prim_su_t24, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 24, 0.2).
narrative_ontology:measurement(nafta_sov_prim_su_t30, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 30, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nafta_jurisdictional_boundary__sovereignty_primacy_reading, resource_allocation).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__sovereignty_primacy_reading, capital_supremacy_reading).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__sovereignty_primacy_reading, embedded_liberalism_reading).

% DUAL FORMULATION NOTE:
% The nafta_jurisdictional_boundary kernel decomposes into three readings because the treaty text's ambiguity about the relationship between international obligations and domestic law creates structurally distinct constraints with different Îµ values, beneficiary structures, and directionalities.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
