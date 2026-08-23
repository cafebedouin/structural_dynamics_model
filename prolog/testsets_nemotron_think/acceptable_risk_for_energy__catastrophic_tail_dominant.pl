% ============================================================================
% CONSTRAINT STORY: acceptable_risk_for_energy__catastrophic_tail_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_for_energy__catastrophic_tail_dominant, []).

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
 *   constraint_id: acceptable_risk_for_energy__catastrophic_tail_dominant
 *   human_readable: Catastrophic Tail Dominance in Energy Risk Governance
 *   domain: risk_assessment/energy_policy/public_safety_governance
 *
 * SUMMARY:
 *   This constraint story captures the 'catastrophic_tail_dominant' reading
 *   of the contested kernel 'acceptable_risk_for_energy'. The reading asserts
 *   that energy risk governance must be dominated by low-probability
 *   high-consequence events — Chernobyl, Fukushima, geological repository
 *   failure — because their irreversibility and intergenerational burden
 *   structurally outweigh expected-value optimization. The constraint
 *   operates through the ALARA principle, dose limits set at the boundary of
 *   detectability, geological disposal requirements demanding million-year
 *   isolation, and the systematic exclusion of probabilistic trade-off
 *   framing from licensing. Nuclear industry enters the victim set via
 *   tail-risk weighting: it bears costs that scale with consequence
 *   magnitude, not probability. Waste disposal becomes a constraint
 *   (permanent isolation, retrievability forbidden) rather than an
 *   engineering problem (monitored retrievable storage). Probabilistic risk
 *   assessment is suppressed not because it is wrong but because it produces
 *   the 'wrong' answer for tail-risk governance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.78).
domain_priors:suppression_score(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.82).
domain_priors:theater_ratio(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, extractiveness, 0.78).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_for_energy__catastrophic_tail_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_for_energy__catastrophic_tail_dominant, "Catastrophic Tail Dominance in Energy Risk Governance").
narrative_ontology:topic_domain(acceptable_risk_for_energy__catastrophic_tail_dominant, "risk_assessment/energy_policy/public_safety_governance").

domain_priors:requires_active_enforcement(acceptable_risk_for_energy__catastrophic_tail_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_for_energy__catastrophic_tail_dominant, '7c0b7e17-8d99-4de5-a316-f8fd07128abf').
narrative_ontology:cs_kernel_codification('7c0b7e17-8d99-4de5-a316-f8fd07128abf', formalized).
narrative_ontology:cs_authority_grounding('7c0b7e17-8d99-4de5-a316-f8fd07128abf', lineage).
narrative_ontology:cs_interpretation_layer_present('7c0b7e17-8d99-4de5-a316-f8fd07128abf').
narrative_ontology:cs_reading_relation('7c0b7e17-8d99-4de5-a316-f8fd07128abf', acceptable_risk_for_energy__expected_value_dominant, coexists_with).
narrative_ontology:cs_reading_relation('7c0b7e17-8d99-4de5-a316-f8fd07128abf', acceptable_risk_for_energy__comparative_risk_dominant, influences).
narrative_ontology:cs_axiom('7c0b7e17-8d99-4de5-a316-f8fd07128abf', foundational, irreversibility_outranks_probability).
narrative_ontology:cs_axiom_status(irreversibility_outranks_probability, holdable).
narrative_ontology:cs_axiom_grounding('7c0b7e17-8d99-4de5-a316-f8fd07128abf', irreversibility_outranks_probability, deontological).
narrative_ontology:cs_axiom('7c0b7e17-8d99-4de5-a316-f8fd07128abf', foundational, intergenerational_burden_primacy).
narrative_ontology:cs_axiom_status(intergenerational_burden_primacy, holdable).
narrative_ontology:cs_axiom_grounding('7c0b7e17-8d99-4de5-a316-f8fd07128abf', intergenerational_burden_primacy, deontological).
narrative_ontology:cs_reference_frame('7c0b7e17-8d99-4de5-a316-f8fd07128abf', precautionary_governance_framework).
narrative_ontology:cs_drift_state('7c0b7e17-8d99-4de5-a316-f8fd07128abf', post_fukushima_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7c0b7e17-8d99-4de5-a316-f8fd07128abf', '').
narrative_ontology:cs_kernel_id(acceptable_risk_for_energy__catastrophic_tail_dominant, acceptable_risk_for_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__catastrophic_tail_dominant, precautionary_principle_advocates).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__catastrophic_tail_dominant, intergenerational_equity_proponents).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__catastrophic_tail_dominant, radiation_protection_regulators).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__catastrophic_tail_dominant, nuclear_industry).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__catastrophic_tail_dominant, future_generations_burdened_by_waste).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__catastrophic_tail_dominant, probabilistic_risk_assessors).
narrative_ontology:constraint_vindicates(acceptable_risk_for_energy__catastrophic_tail_dominant, precautionary_principle).
narrative_ontology:constraint_vindicates(acceptable_risk_for_energy__catastrophic_tail_dominant, intergenerational_equity).
narrative_ontology:constraint_vindicates(acceptable_risk_for_energy__catastrophic_tail_dominant, irreversibility_primacy).
narrative_ontology:constraint_vindicates(acceptable_risk_for_energy__catastrophic_tail_dominant, catastrophic_harm_avoidance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bears full compliance costs for waste disposal, decommissioning, and probabilistic safety analyses that must satisfy tail-risk standards. Cannot exit without abandoning sunk capital and regulatory licenses. Subject to moratoria and phase-outs driven by tail-risk framing. Gains no revenue from the constraint's coordination function.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, nuclear_industry, payer,
    powerful, biographical, constrained, global).

% Advocate for governance frameworks that prioritize irreversible harm prevention over expected-value calculations. Gain institutional recognition, funding, and policy influence when tail-risk framing dominates. Their exit is mobile — they can shift focus to other domains (GMOs, AI, climate) without losing structural position.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, precautionary_principle_advocates, beneficiary,
    organized, generational, mobile, global).

% Frame nuclear waste as a moral debt to future generations. Gain ethical authority and policy standing from the constraint's intergenerational logic. Identity-locked: their professional and moral self-concept is constituted through this framing; exit would dissolve their core advocacy identity.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, intergenerational_equity_proponents, beneficiary,
    organized, civilizational, identity_locked, global).

% Administer the ALARA (As Low As Reasonably Achievable) framework and dose limits that operationalize tail-risk dominance. Set standards, license facilities, and enforce compliance. Collect regulatory fees and institutional legitimacy. Can arbitrage across jurisdictions (IAEA, national frameworks) but are institutionally committed to the tail-risk paradigm.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, radiation_protection_regulators, agenda_setter,
    institutional, generational, arbitrage, national).

% Inherit the geological repository burden and surveillance obligations for high-level waste. Have no voice in current decisions, no exit from the biosphere, and no mechanism to renegotiate the burden. The constraint claims to protect them but also constitutes the waste burden they must bear.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, future_generations_burdened_by_waste, payer,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_for_energy__catastrophic_tail_dominant, future_generations_burdened_by_waste, excluded).

% Develop PRA/PSA methodologies that quantify risk as probability × consequence. Their framing is structurally suppressed when tail-risk dominance prevails — they are excluded from standard-setting or forced to adopt bounding analyses that discard probabilistic trade-offs. Can exit to other industries (aerospace, finance) but lose domain-specific capital.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, probabilistic_risk_assessors, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_for_energy__catastrophic_tail_dominant, probabilistic_risk_assessors, excluded).

% Need low-carbon baseload but face a constraint that makes nuclear expansion structurally difficult. They observe the tail-risk framing's effect on decarbonization pathways but do not set nuclear safety standards. Their analytical seat sees the full structure: tail-risk governance trades climate risk for nuclear risk avoidance.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, climate_policy_makers, observer,
    institutional, generational, analytical, global).

% Need energy density for development but face export control regimes and financing restrictions shaped by tail-risk governance. Would argue for probabilistic risk-informed licensing but are excluded from the standard-setting bodies. Their exit is constrained by technology denial regimes.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, developing_nation_energy_planners, excluded,
    moderate, biographical, constrained, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents irreversible catastrophic harm from low-probability high-consequence energy events by establishing governance that treats tail risks as dominant over expected-value calculations. Coordinates international waste disposal standards, emergency planning zones, and intergenerational stewardship obligations.
% TRANSFER_FUNCTION: Moves risk burden from probabilistic optimization to precautionary prevention. Nuclear industry bears compliance costs, opportunity costs from delayed/denied deployment, and waste stewardship liability. Future generations receive protection from radiation exposure but inherit the waste burden and surveillance obligations. Probabilistic risk assessors lose professional standing and domain authority.
% ABSENT_VOICES: Probabilistic risk assessors (PRA/PSA community), nuclear engineers advocating risk-informed regulation, cost-benefit economists, developing nations needing energy density for poverty alleviation, communities hosting waste repositories who were not consulted in the original tail-risk framing. These voices are structurally excluded from the IAEA safety standards committees and national regulatory rulemakings where the tail-risk paradigm is instantiated.
% DISAPPEARANCE_RATIONALE: If the tail-risk dominance constraint vanished overnight, nuclear licensing would shift to risk-informed, performance-based frameworks (as the NRC has partially attempted). Waste disposal would become an engineering optimization problem with retrievability and monitoring replacing permanent isolation as the design basis. Probabilistic trade-offs between nuclear risk and climate risk would be admitted in integrated resource planning. The nuclear industry would redeploy capital to SMRs and advanced reactors. The intergenerational equity frame would lose its primary policy instantiation.
% FOUNDING_PROBLEM: How to govern energy technologies where catastrophic failure modes exist (Chernobyl 1986, Fukushima 2011) and where waste remains hazardous for timescales exceeding all human institutions — the problem of binding present decisions to consequences that cannot be experienced, monitored, or reversed by the decision-makers.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the IAEA Safety Fundamentals (SF-1) and the Nuclear Energy Agency's 1995 'Collective Opinion on Radioactive Waste Management' — both outside the nuclear industry beneficiary set. However, the US NRC's 2011 Near-Term Task Force Review and the 2020 Risk-Informed Regulation Implementation Plan attest that modern designs and regulatory tools have substantially changed the problem structure. The Generation IV International Forum's safety methodology (2021) argues the founding problem is addressed by inherent safety features, not tail-risk dominance. No consensus exists across these seats.
narrative_ontology:disappearance_verdict(acceptable_risk_for_energy__catastrophic_tail_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_for_energy__catastrophic_tail_dominant, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_for_energy__catastrophic_tail_dominant, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(acceptable_risk_for_energy__catastrophic_tail_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_for_energy__catastrophic_tail_dominant_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(acceptable_risk_for_energy__catastrophic_tail_dominant, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(acceptable_risk_for_energy__catastrophic_tail_dominant_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) is high because the constraint transfers disproportionate cost to nuclear industry (compliance, waste liability, opportunity cost) while the coordination benefit (catastrophe avoidance) is diffuse and unverifiable. Suppression (0.82) is very high because the constraint's persistence depends on actively excluding probabilistic trade-off arguments from licensing proceedings, waste disposal standards, and international guidelines — the IAEA Safety Standards explicitly subordinate optimization to justification and optimization is bounded by dose constraints. Theater ratio (0.42) is moderate: the safety review function is real but a growing share of regulatory activity performs 'defense in depth' rituals that do not reduce tail risk (e.g., administrative controls on already-bounded events). Accessibility collapse (0.85) is high because once the tail-risk frame is accepted, alternatives (risk-informed licensing, comparative risk, adaptive management) appear morally illegitimate, not just technically inferior. Resistance (0.55) is moderate: nuclear industry resists through lobbying, litigation, and technical innovation (SMRs, accident-tolerant fuels) but cannot challenge the frame without appearing to dismiss catastrophe.
 *
 * PERSPECTIVAL GAP:
 *   From the regulator/beneficiary seats, the constraint is genuine coordination: it solves the collective action problem of binding present actors to intergenerational obligations that no single generation would voluntarily undertake. From the nuclear industry/probabilistic assessor seats, the same structure is enforced extraction: the coordination function (catastrophe avoidance) is real but the cost allocation is asymmetric and the suppression of alternative framings is active. The engine computes this divergence from the structural data — the claimed type 'tangled_rope' reflects the author's structural judgment that both functions are present and neither reduces to the other.
 *
 * DIRECTIONALITY LOGIC:
 *   Radiation protection regulators are agenda_setters (d ~ 0.15) — they administer the constraint and collect institutional legitimacy. Precautionary principle advocates and intergenerational equity proponents are beneficiaries (d ~ 0.2) — they gain moral authority and policy standing; their exit is mobile or identity-locked but not constrained by the constraint itself. Nuclear industry is the primary payer (d ~ 0.85) — bears extraction, constrained exit (sunk capital, license dependence), powerful but targeted. Future generations are payers (d ~ 0.95) — inherit waste burden, trapped exit, powerless. Probabilistic risk assessors are payers (d ~ 0.7) — lose professional standing, constrained exit to other domains. Climate policy makers are observers (d ~ 0.5) — analytical seat, see full structure. Developing nation planners are excluded (d ~ 0.6) — would benefit from probabilistic framing but are structurally kept out of standard-setting.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (governing irreversible catastrophe) remains live — Fukushima 2011 confirmed tail risks are not hypothetical. However, the mandate has accumulated extraction: waste disposal standards now exceed what the founding problem required (permanent isolation vs. monitored retrievability), and the suppression of probabilistic trade-offs now blocks climate-risk integration. The constraint shows mandatrophy indicators: theater ratio rising (0.25→0.42), extractiveness rising (0.55→0.78), but the founding problem status is 'contested' not 'dead' — so full mandatrophy resolution is not declared. The constraint is tangled_rope, not snare, because the coordination function (intergenerational catastrophe prevention) remains structurally necessary and not merely cover.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is ''acceptable_risk_for_energy'' a single kernel with three readings, or are these three distinct constraints that share vocabulary?',
    'Test ε-invariance: if measuring the constraint via ''tail-risk weight in licensing'' gives ε≈0.78 but measuring via ''expected-cost allocation'' gives ε≈0.25, they are distinct constraints. The BGS decomposition protocol applies: write separate stories, link via network.affects_constraints.',
    'If distinct constraints, the ''kernel'' framing is a category error — the readings don''t share a referent. If single kernel, the readings are genuine interpretive variants and the cs_structure fields correctly model their structural relations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the three declared readings share a stable referent (kernel) or are ε-distinct constraints').

omega_variable(
    tail_risk_weight_calibration,
    'What probability threshold defines ''low-probability high-consequence'' and how is the consequence magnitude calibrated across energy technologies?',
    'Compare IAEA Safety Guide SSG-3 (2010) frequency-consequence curves with NRC Regulatory Guide 1.174 (2011) risk-informed thresholds. If the curves diverge structurally (different mathematical form, not just parameters), the threshold is a policy choice, not a physical limit.',
    'If the threshold is a policy choice, the constraint''s extraction includes the arbitrariness of the boundary — nuclear is held to a standard coal/gas/solar are not. If physically derived, the extraction is the cost of a physical limit.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(tail_risk_weight_calibration, empirical, 'Whether the tail-risk dominance threshold is physically grounded or policy-constructed').

omega_variable(
    waste_disposal_as_constraint_vs_engineering,
    'Is permanent geological isolation (no retrievability, no monitoring dependence) a physical necessity or a tail-risk governance choice?',
    'Compare the Swedish KBS-3V repository design (passive safety, no retrievability) with the French CIGÉO design (retrievability for 100+ years, active monitoring). If both meet dose limits but only one satisfies the constraint, the constraint is governance choice.',
    'If governance choice, the extraction on nuclear industry includes the cost of foreclosing retrievability and the burden on future generations includes loss of adaptability. If physical necessity, the extraction is the cost of physics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(waste_disposal_as_constraint_vs_engineering, empirical, 'Whether permanent isolation is physically required or a tail-risk governance imposition').

omega_variable(
    suppression_mechanism_probabilistic_framing,
    'Is the suppression of probabilistic risk assessment structural (regulatory prohibition) or internalized (professional norm adoption)?',
    'Track PRA/PSA publication rates in nuclear vs. aerospace/chemical journals post-Fukushima. If nuclear PRA output declines while aerospace grows, suppression is structural. If nuclear PRA practitioners self-censor in licensing contexts but publish freely elsewhere, internalized component exists.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure — the target carries the suppression with them. This affects the omega''s impact on classification: internalized suppression resists reform even if regulatory text changes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_probabilistic_framing, empirical, 'Structural vs. internalized suppression of probabilistic trade-off framing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_for_energy__catastrophic_tail_dominant, 1986, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arce_ctd_tr_t1986, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 1986, 0.25).
narrative_ontology:measurement(arce_ctd_tr_t1996, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 1996, 0.3).
narrative_ontology:measurement(arce_ctd_tr_t2006, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 2006, 0.35).
narrative_ontology:measurement(arce_ctd_tr_t2011, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 2011, 0.4).
narrative_ontology:measurement(arce_ctd_tr_t2016, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 2016, 0.41).
narrative_ontology:measurement(arce_ctd_tr_t2021, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 2021, 0.42).
narrative_ontology:measurement(arce_ctd_tr_t2026, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 2026, 0.42).

% Extraction over time
narrative_ontology:measurement(arce_ctd_be_t1986, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 1986, 0.55).
narrative_ontology:measurement(arce_ctd_be_t1996, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 1996, 0.62).
narrative_ontology:measurement(arce_ctd_be_t2006, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 2006, 0.68).
narrative_ontology:measurement(arce_ctd_be_t2011, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 2011, 0.75).
narrative_ontology:measurement(arce_ctd_be_t2016, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 2016, 0.76).
narrative_ontology:measurement(arce_ctd_be_t2021, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 2021, 0.77).
narrative_ontology:measurement(arce_ctd_be_t2026, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 2026, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(arce_ctd_su_t1986, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 1986, 0.7).
narrative_ontology:measurement(arce_ctd_su_t1996, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 1996, 0.72).
narrative_ontology:measurement(arce_ctd_su_t2006, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 2006, 0.75).
narrative_ontology:measurement(arce_ctd_su_t2011, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 2011, 0.8).
narrative_ontology:measurement(arce_ctd_su_t2016, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 2016, 0.81).
narrative_ontology:measurement(arce_ctd_su_t2021, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 2021, 0.82).
narrative_ontology:measurement(arce_ctd_su_t2026, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 2026, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_for_energy__catastrophic_tail_dominant, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.12).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__catastrophic_tail_dominant, acceptable_risk_for_energy__expected_value_dominant).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__catastrophic_tail_dominant, acceptable_risk_for_energy__comparative_risk_dominant).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__catastrophic_tail_dominant, nuclear_waste_disposal__permanent_isolation).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__catastrophic_tail_dominant, climate_mitigation__nuclear_exclusion).

% DUAL FORMULATION NOTE:
% This constraint and its two sibling readings form the 'acceptable_risk_for_energy' constraint family. The catastrophic_tail_dominant reading has higher ε (0.78 vs ~0.25 for expected_value_dominant) because it treats tail risks as dominant rather than probabilistic. The readings are linked because the tail-risk framing is often cited as evidence against the expected-value framing in regulatory proceedings (e.g., NRC Commission decisions on filtered vents, 2012-2018).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(acceptable_risk_for_energy__catastrophic_tail_dominant, institutional, 0.15).
constraint_indexing:directionality_override(acceptable_risk_for_energy__catastrophic_tail_dominant, organized, 0.2).
constraint_indexing:directionality_override(acceptable_risk_for_energy__catastrophic_tail_dominant, powerful, 0.85).
constraint_indexing:directionality_override(acceptable_risk_for_energy__catastrophic_tail_dominant, powerless, 0.95).
constraint_indexing:directionality_override(acceptable_risk_for_energy__catastrophic_tail_dominant, moderate, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
