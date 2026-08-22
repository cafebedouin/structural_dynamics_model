% ============================================================================
% CONSTRAINT STORY: nafta_jurisdictional_boundary__sovereignty_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: NAFTA Jurisdictional Boundary — Sovereignty Primacy Reading
 *   domain: international_trade_law/political_economy/regulatory_federalism
 *
 * SUMMARY:
 *   This constraint story instantiates the sovereignty primacy reading of the
 *   NAFTA jurisdictional boundary kernel. Under this reading, the trade
 *   agreement functions as a coordination mechanism for market access while
 *   domestic law retains primacy over labor, environmental, and health
 *   standards. States do not cede regulatory authority to trade tribunals;
 *   treaty obligations enter the compliance-cost calculus but never override
 *   domestic police powers. The constraint is claimed as rope because it
 *   genuinely coordinates trade liberalization without extraction — the
 *   compliance costs borne by foreign investors are the price of market
 *   access, not rents captured by a supranational authority.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0.15).
domain_priors:suppression_score(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0.12).
domain_priors:theater_ratio(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nafta_jurisdictional_boundary__sovereignty_primacy_reading, rope).
narrative_ontology:human_readable(nafta_jurisdictional_boundary__sovereignty_primacy_reading, "NAFTA Jurisdictional Boundary — Sovereignty Primacy Reading").
narrative_ontology:topic_domain(nafta_jurisdictional_boundary__sovereignty_primacy_reading, "international_trade_law/political_economy/regulatory_federalism").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nafta_jurisdictional_boundary__sovereignty_primacy_reading, '814f7b98-4c5a-480f-a273-401173ba5c78').
narrative_ontology:cs_kernel_codification('814f7b98-4c5a-480f-a273-401173ba5c78', formalized).
narrative_ontology:cs_authority_grounding('814f7b98-4c5a-480f-a273-401173ba5c78', lineage).
narrative_ontology:cs_interpretation_layer_present('814f7b98-4c5a-480f-a273-401173ba5c78').
narrative_ontology:cs_reading_relation('814f7b98-4c5a-480f-a273-401173ba5c78', nafta_jurisdictional_boundary__capital_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('814f7b98-4c5a-480f-a273-401173ba5c78', nafta_jurisdictional_boundary__embedded_liberalism_reading, coexists_with).
narrative_ontology:cs_axiom('814f7b98-4c5a-480f-a273-401173ba5c78', foundational, domestic_law_primacy_over_treaty_obligations).
narrative_ontology:cs_axiom_status(domestic_law_primacy_over_treaty_obligations, holdable).
narrative_ontology:cs_axiom_grounding('814f7b98-4c5a-480f-a273-401173ba5c78', domestic_law_primacy_over_treaty_obligations, conventional).
narrative_ontology:cs_axiom('814f7b98-4c5a-480f-a273-401173ba5c78', foundational, regulatory_sovereignty_non_derogable).
narrative_ontology:cs_axiom_status(regulatory_sovereignty_non_derogable, holdable).
narrative_ontology:cs_axiom_grounding('814f7b98-4c5a-480f-a273-401173ba5c78', regulatory_sovereignty_non_derogable, deontological).
narrative_ontology:cs_reference_frame('814f7b98-4c5a-480f-a273-401173ba5c78', original_nafta_ratification_understanding).
narrative_ontology:cs_drift_state('814f7b98-4c5a-480f-a273-401173ba5c78', post_chapter11_jurisprudence_era, gap(authority_erosion, minor, false)).
narrative_ontology:cs_created_at('814f7b98-4c5a-480f-a273-401173ba5c78', '').
narrative_ontology:cs_kernel_id(nafta_jurisdictional_boundary__sovereignty_primacy_reading, nafta_jurisdictional_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__sovereignty_primacy_reading, domestic_regulatory_agencies).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__sovereignty_primacy_reading, state_legislatures).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__sovereignty_primacy_reading, labor_unions).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__sovereignty_primacy_reading, environmental_ngos).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__sovereignty_primacy_reading, foreign_investors).
narrative_ontology:constraint_vindicates(nafta_jurisdictional_boundary__sovereignty_primacy_reading, regulatory_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(nafta_jurisdictional_boundary__sovereignty_primacy_reading, police_powers_exception).
narrative_ontology:constraint_vindicates(nafta_jurisdictional_boundary__sovereignty_primacy_reading, non_derogation_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer and enforce domestic labor, environmental, and health standards within their jurisdiction. Treat trade agreement obligations as one input among many in regulatory decision-making, not as overriding constraints. Retain full authority to set standards above treaty minimums.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, domestic_regulatory_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Enact and amend domestic regulatory standards without treaty preemption concerns. Benefit from the coordination function of market access rules while preserving legislative autonomy over domestic policy space.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, state_legislatures, beneficiary,
    institutional, generational, analytical, national).

% Advocate for stronger domestic labor standards without facing trade agreement challenges that would treat higher standards as violations. Benefit from preserved policy space to organize and bargain collectively.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, labor_unions, beneficiary,
    organized, biographical, constrained, national).

% Push for stricter environmental and health regulations domestically. Benefit from the reading's preservation of regulatory sovereignty, which prevents trade tribunals from striking down domestic environmental measures as trade barriers.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, environmental_ngos, beneficiary,
    organized, biographical, constrained, national).

% Face compliance costs from varying domestic standards across NAFTA parties. Bear the cost of regulatory heterogeneity but retain exit options through capital mobility and investment arbitration where available.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, foreign_investors, payer,
    powerful, biographical, mobile, global).

% Interpret treaty obligations narrowly under this reading, deferring to domestic regulatory authority except where discrimination or disguised restriction on trade is demonstrated. Do not review the substantive level of domestic standards.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, trade_dispute_panels, observer,
    institutional, generational, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable framework for market access and investment flows across three sovereign states without requiring regulatory harmonization — coordinates trade liberalization while preserving each state's authority to set its own domestic standards.
% TRANSFER_FUNCTION: Moves compliance costs from domestic regulators to regulated entities (including foreign investors) who must navigate multiple regulatory regimes; does not transfer regulatory authority to supranational bodies.
% ABSENT_VOICES: Supranational governance advocates and harmonization proponents who would prefer stronger trade tribunal authority to review domestic regulatory levels are not represented in this reading's institutional structure; they operate through the sibling readings.
% DISAPPEARANCE_RATIONALE: If this reading vanished, trade tribunals would gain authority to review and potentially strike down domestic labor, environmental, and health standards as trade barriers — fundamentally rearranging the relationship between trade law and domestic regulatory sovereignty.
% FOUNDING_PROBLEM: How to achieve trade liberalization across sovereign states with vastly different domestic regulatory traditions without forcing regulatory harmonization or creating a supranational regulatory authority.
% FOUNDING_PROBLEM_CORROBORATION: The text of NAFTA Article 105 (relation to other agreements) and Article 712 (relation to environmental agreements) preserves domestic authority; the Side Agreements on Labor and Environment were negotiated precisely because parties insisted regulatory sovereignty remain intact; legal scholarship (e.g., Stewart, Charnovitz) confirms the founding design was coordination without supranational preemption.
narrative_ontology:disappearance_verdict(nafta_jurisdictional_boundary__sovereignty_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(nafta_jurisdictional_boundary__sovereignty_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nafta_jurisdictional_boundary__sovereignty_primacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0.15, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Low extractiveness (0.15) reflects that the only transfer is voluntary compliance costs — no actor captures rents from the constraint's operation. Low suppression (0.12) because the constraint does not coerce regulatory outcomes; states retain full exit from the coordination if they choose. Rising theater ratio (0.05→0.18) reflects increasing performative invocation of sovereignty in political discourse even as substantive regulatory autonomy remains intact. The measurement series shows stable extraction with modest performative drift over the NAFTA period.
 *
 * PERSPECTIVAL GAP:
 *   The capital supremacy reading would compute extraction as near-zero for investors (their preferred reading) and high for domestic regulators (constrained by tribunal review). This reading computes the inverse. The engine will show seat divergence: from the agenda-setter seat (domestic agencies) the constraint is rope; from the payer seat (investors) it may appear as tangled_rope if compliance costs are experienced as asymmetric extraction without reciprocity. The divergence is structural, not perspectival.
 *
 * DIRECTIONALITY LOGIC:
 *   Domestic regulators and legislatures are beneficiaries (d ≈ 0.1) — they gain coordination without losing authority. Labor unions and environmental NGOs are beneficiaries (d ≈ 0.2) — they retain policy space to advance standards. Foreign investors are payers (d ≈ 0.7) — they bear compliance costs across heterogeneous regimes but have mobile exit. Trade panels are analytical observers (d ≈ 0.5). The derivation chain from beneficiary/victim declarations produces these directionalities without overrides.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (trade liberalization without regulatory surrender) remains live — the USMCA renegotiation preserved the sovereignty primacy architecture while updating rules of origin and digital trade. No mandatrophy: the constraint's coordination function is active and its extraction remains minimal. The theater drift is political rhetoric, not functional decay.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tribunal_creep_ambiguity,
    'Do trade tribunal decisions under Chapter 11 (investment) and Chapter 20 (state-state) effectively review domestic regulatory levels despite the sovereignty primacy reading''s textual basis?',
    'Empirical coding of tribunal awards: count instances where domestic environmental/labor/health measures were found to violate NAFTA despite non-discriminatory application. Track dissenting opinions invoking regulatory sovereignty.',
    'If tribunal creep is documented, the sovereignty primacy reading describes an aspirational rather than operative constraint — effective extraction on domestic regulators would be higher than authored, potentially shifting classification toward tangled_rope for the agenda-setter seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tribunal_creep_ambiguity, empirical, 'Whether trade tribunals de facto override domestic regulatory authority despite textual primacy.').

omega_variable(
    coordination_vs_harmonization_boundary,
    'At what point does the coordination function of market access rules require sufficient regulatory harmonization that sovereignty primacy becomes a formal rather than substantive claim?',
    'Compare regulatory divergence metrics (e.g., chemical safety thresholds, labor standards indices) across NAFTA parties over time against trade flow integration. Identify threshold where coordination fails without convergence.',
    'If coordination functionally requires convergence, the sovereignty primacy reading masks de facto harmonization pressure — extraction on domestic regulators would include the cost of maintaining divergent standards under market integration pressure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_vs_harmonization_boundary, conceptual, 'Whether market integration structurally necessitates regulatory convergence regardless of treaty text.').

omega_variable(
    kernel_reading_relation_capital_supremacy,
    'Does the sovereignty primacy reading logically foreclose the capital supremacy reading within a single interpretive framework, or do they coexist as competing positions?',
    'Analyze whether a single legal framework (e.g., a domestic court system) could simultaneously hold that treaty obligations are subordinate to domestic law AND that they override domestic law — structural contradiction test.',
    'If forecloses: the two readings cannot be held by the same actor in the same proceeding. If coexists_with: different actors (courts, tribunals, legislatures) can hold different readings simultaneously. Determines reading_relations assignment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_relation_capital_supremacy, conceptual, 'Structural relationship between sovereignty primacy and capital supremacy readings of the NAFTA jurisdictional boundary kernel.').

omega_variable(
    kernel_reading_relation_embedded_liberalism,
    'Does the sovereignty primacy reading coexist with or influence the embedded liberalism reading?',
    'Examine whether embedded liberalism''s ''non-discriminatory policy space'' is a narrowing of sovereignty primacy''s ''full regulatory authority'' or a distinct equilibrium. Track institutional adoption: do the same actors invoke both?',
    'If coexists_with: both are live positions in the same discourse. If influences: sovereignty primacy creates pressure on embedded liberalism by expanding the policy space the latter must accommodate. Determines reading_relations assignment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_relation_embedded_liberalism, conceptual, 'Structural relationship between sovereignty primacy and embedded liberalism readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 1994, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(naft_tr_t1994, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 1994, 0.05).
narrative_ontology:measurement(naft_tr_t1998, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 1998, 0.08).
narrative_ontology:measurement(naft_tr_t2002, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 2002, 0.12).
narrative_ontology:measurement(naft_tr_t2006, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 2006, 0.15).
narrative_ontology:measurement(naft_tr_t2010, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 2010, 0.17).
narrative_ontology:measurement(naft_tr_t2014, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 2014, 0.18).
narrative_ontology:measurement(naft_tr_t2018, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 2018, 0.18).
narrative_ontology:measurement(naft_tr_t2020, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 2020, 0.18).

% Extraction over time
narrative_ontology:measurement(naft_be_t1994, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 1994, 0.08).
narrative_ontology:measurement(naft_be_t1998, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 1998, 0.1).
narrative_ontology:measurement(naft_be_t2002, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 2002, 0.12).
narrative_ontology:measurement(naft_be_t2006, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 2006, 0.13).
narrative_ontology:measurement(naft_be_t2010, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 2010, 0.14).
narrative_ontology:measurement(naft_be_t2014, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 2014, 0.15).
narrative_ontology:measurement(naft_be_t2018, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 2018, 0.15).
narrative_ontology:measurement(naft_be_t2020, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 2020, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(naft_su_t1994, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 1994, 0.05).
narrative_ontology:measurement(naft_su_t1998, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 1998, 0.08).
narrative_ontology:measurement(naft_su_t2002, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 2002, 0.1).
narrative_ontology:measurement(naft_su_t2006, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 2006, 0.11).
narrative_ontology:measurement(naft_su_t2010, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 2010, 0.12).
narrative_ontology:measurement(naft_su_t2014, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 2014, 0.12).
narrative_ontology:measurement(naft_su_t2018, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 2018, 0.12).
narrative_ontology:measurement(naft_su_t2020, nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 2020, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nafta_jurisdictional_boundary__sovereignty_primacy_reading, information_standard).
narrative_ontology:boltzmann_floor_override(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0.02).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__sovereignty_primacy_reading, nafta_jurisdictional_boundary__capital_supremacy_reading).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__sovereignty_primacy_reading, nafta_jurisdictional_boundary__embedded_liberalism_reading).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__sovereignty_primacy_reading, usmca_jurisdictional_boundary).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the nafta_jurisdictional_boundary kernel. The sovereignty primacy reading (this file) declares treaty obligations as subordinate coordination inputs. The capital_supremacy_reading declares them as overriding constraints. The embedded_liberalism_reading declares a balanced framework. All three share the same treaty text but instantiate different constraints with different ε, beneficiaries, and types.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
