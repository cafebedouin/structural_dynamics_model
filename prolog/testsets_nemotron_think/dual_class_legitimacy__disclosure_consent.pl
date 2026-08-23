% ============================================================================
% CONSTRAINT STORY: dual_class_legitimacy__disclosure_consent
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dual_class_legitimacy__disclosure_consent, []).

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
 *   constraint_id: dual_class_legitimacy__disclosure_consent
 *   human_readable: Dual-Class Legitimacy via Securities Act Disclosure Consent
 *   domain: corporate_governance/securities_law/organizational_economics
 *
 * SUMMARY:
 *   This constraint story captures the 'disclosure consent' reading of
 *   dual-class share legitimacy: the arrangement is a voluntary contractual
 *   choice legitimated by Securities Act disclosure. Investors buy Class A
 *   shares with eyes open; S-1 filings disclose the governance disparity; the
 *   voting discount is priced into valuation. The constraint is claimed as a
 *   rope (coordination via contractual freedom) with minimal extraction and
 *   suppression. The kernel is 'dual_class_legitimacy'; sibling readings are
 *   'founder_stewardship' (control enables long-horizon value creation) and
 *   'minority_extraction' (minority shareholders are entitled to proportional
 *   governance).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dual_class_legitimacy__disclosure_consent, 0.08).
domain_priors:suppression_score(dual_class_legitimacy__disclosure_consent, 0.05).
domain_priors:theater_ratio(dual_class_legitimacy__disclosure_consent, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__disclosure_consent, extractiveness, 0.08).
narrative_ontology:constraint_metric(dual_class_legitimacy__disclosure_consent, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(dual_class_legitimacy__disclosure_consent, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__disclosure_consent, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(dual_class_legitimacy__disclosure_consent, resistance, 0.18).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dual_class_legitimacy__disclosure_consent, rope).
narrative_ontology:human_readable(dual_class_legitimacy__disclosure_consent, "Dual-Class Legitimacy via Securities Act Disclosure Consent").
narrative_ontology:topic_domain(dual_class_legitimacy__disclosure_consent, "corporate_governance/securities_law/organizational_economics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dual_class_legitimacy__disclosure_consent, 'da9d5aa1-ae3c-4d5a-a578-590ebb4fee40').
narrative_ontology:cs_kernel_codification('da9d5aa1-ae3c-4d5a-a578-590ebb4fee40', formalized).
narrative_ontology:cs_authority_grounding('da9d5aa1-ae3c-4d5a-a578-590ebb4fee40', expertise).
narrative_ontology:cs_interpretation_layer_present('da9d5aa1-ae3c-4d5a-a578-590ebb4fee40').
narrative_ontology:cs_reading_relation('da9d5aa1-ae3c-4d5a-a578-590ebb4fee40', dual_class_legitimacy__founder_stewardship, coexists_with).
narrative_ontology:cs_reading_relation('da9d5aa1-ae3c-4d5a-a578-590ebb4fee40', dual_class_legitimacy__minority_extraction, coexists_with).
narrative_ontology:cs_axiom('da9d5aa1-ae3c-4d5a-a578-590ebb4fee40', foundational, informed_consent_satisfies_legitimacy).
narrative_ontology:cs_axiom_status(informed_consent_satisfies_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('da9d5aa1-ae3c-4d5a-a578-590ebb4fee40', informed_consent_satisfies_legitimacy, empirically_contingent).
narrative_ontology:cs_axiom('da9d5aa1-ae3c-4d5a-a578-590ebb4fee40', foundational, governance_disparity_priced_into_valuation).
narrative_ontology:cs_axiom_status(governance_disparity_priced_into_valuation, holdable).
narrative_ontology:cs_axiom_grounding('da9d5aa1-ae3c-4d5a-a578-590ebb4fee40', governance_disparity_priced_into_valuation, empirically_contingent).
narrative_ontology:cs_axiom('da9d5aa1-ae3c-4d5a-a578-590ebb4fee40', secondary, securities_act_disclosure_sufficiency).
narrative_ontology:cs_axiom_status(securities_act_disclosure_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('da9d5aa1-ae3c-4d5a-a578-590ebb4fee40', securities_act_disclosure_sufficiency, conventional).
narrative_ontology:cs_reference_frame('da9d5aa1-ae3c-4d5a-a578-590ebb4fee40', securities_act_disclosure_regime).
narrative_ontology:cs_drift_state('da9d5aa1-ae3c-4d5a-a578-590ebb4fee40', contemporary_governance_pressure_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('da9d5aa1-ae3c-4d5a-a578-590ebb4fee40', '').
narrative_ontology:cs_kernel_id(dual_class_legitimacy__disclosure_consent, dual_class_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__disclosure_consent, founders_insiders_class_b).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__disclosure_consent, public_investors_class_a).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__disclosure_consent, index_providers).
narrative_ontology:constraint_victim(dual_class_legitimacy__disclosure_consent, public_investors_class_a).
narrative_ontology:constraint_victim(dual_class_legitimacy__disclosure_consent, institutional_investors).
narrative_ontology:constraint_vindicates(dual_class_legitimacy__disclosure_consent, informed_consent_legitimacy_doctrine).
narrative_ontology:constraint_vindicates(dual_class_legitimacy__disclosure_consent, securities_act_disclosure_sufficiency).
narrative_ontology:constraint_vindicates(dual_class_legitimacy__disclosure_consent, contractual_freedom_in_capital_formation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold Class B super-voting shares that concentrate control. Structure the IPO with dual-class shares and file S-1 disclosures describing the governance disparity. Retain decision authority without proportional capital risk. Can exit via secondary sales or take-private transactions at premium valuations.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, founders_insiders_class_b, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(dual_class_legitimacy__disclosure_consent, founders_insiders_class_b, beneficiary).

% Purchase Class A shares with full knowledge of limited voting rights via S-1 prospectus. Accept governance disparity in exchange for access to founder-led growth stories. Price the voting discount into valuation models. Can exit by selling shares in liquid public markets; alternative single-class investments are readily available.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, public_investors_class_a, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(dual_class_legitimacy__disclosure_consent, public_investors_class_a, payer).

% Manage large portfolios that include dual-class companies. Some vote against dual-class structures in proxy advisories but still allocate capital to them for index inclusion and return requirements. Face fiduciary pressure to hold index constituents; cannot easily exclude dual-class names without tracking error. Exit is constrained by benchmark mandates.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, institutional_investors, payer,
    powerful, biographical, constrained, national).

% Administer the Securities Act disclosure regime. Review S-1 filings for compliance with disclosure requirements but do not substantively regulate governance structures. The disclosure framework is treated as sufficient for investor protection; no merit review of dual-class terms occurs.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, sec_regulators, observer,
    institutional, generational, analytical, national).

% Argue that disclosure does not cure structural power imbalance; push for mandatory sunset provisions or one-share-one-vote rules. Their policy proposals are not adopted because the regulatory framework treats informed consent as the legitimacy threshold. They participate in comment letters and academic discourse but hold no formal authority.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, corporate_governance_advocates, excluded,
    moderate, generational, constrained, national).

% Set index inclusion criteria that historically admitted dual-class companies (e.g., S&P 500, Russell indices). Benefit from fee revenue on passive funds tracking broad indices. Modified criteria in 2017 to restrict new dual-class entrants but grandfathered existing constituents. Their decisions shape capital flows but they do not regulate governance.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, index_providers, beneficiary,
    institutional, generational, arbitrage, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables founder-led companies to access public capital markets while retaining long-horizon control, solving the coordination problem of aligning patient capital with entrepreneurial vision without forcing premature exit or control dilution.
% TRANSFER_FUNCTION: Moves voting control from public shareholders to founders/insiders at IPO and in perpetuity, in exchange for capital from public investors who accept limited voting rights as a priced term of the investment contract.
% ABSENT_VOICES: Future shareholders who inherit the dual-class structure without participating in the original IPO consent; retail investors who may not read or understand S-1 disclosures; employees and stakeholders affected by long-horizon decisions but holding no equity voice. These voices are structurally absent from the initial consent moment and from ongoing governance.
% DISAPPEARANCE_RATIONALE: If the disclosure-consent legitimacy framework vanished overnight, dual-class IPOs would face heightened regulatory scrutiny or be blocked; existing dual-class companies would face pressure to convert to single-class or adopt sunset provisions; capital allocation would shift toward governance-aligned structures; index inclusion criteria would tighten further.
% FOUNDING_PROBLEM: Early-stage companies needed to raise growth capital without surrendering control to short-horizon public markets that might force premature monetization or strategic pivots against founder vision. The Securities Act disclosure regime was adapted to permit dual-class structures as a contractual solution.
% FOUNDING_PROBLEM_CORROBORATION: Founders and venture capitalists attest the problem remains live, citing long-horizon innovation (e.g., Google, Meta, Berkshire) as evidence. Institutional investor coalitions (CII, ICGN) and academic governance scholars attest the problem is substantially solved or outweighed by accountability costs, citing empirical studies on dual-class valuation discounts and entrenchment. The SEC's 2017 index-provider consultation record documents both positions.
narrative_ontology:disappearance_verdict(dual_class_legitimacy__disclosure_consent, world_rearranges).
narrative_ontology:founding_problem_status(dual_class_legitimacy__disclosure_consent, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dual_class_legitimacy__disclosure_consent, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dual_class_legitimacy__disclosure_consent, 'none', 1).
narrative_ontology:epsilon_provenance(dual_class_legitimacy__disclosure_consent, 0.08, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dual_class_legitimacy__disclosure_consent_tests).
:- end_tests(dual_class_legitimacy__disclosure_consent_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.08) because the reading's own lights treat the governance disparity as a priced contract term, not extraction — Class A investors consent at purchase. Suppression is minimal (0.05) because no coercion operates; investors choose among thousands of single-class alternatives. Theater ratio is low (0.12) — the disclosure function is genuine, not performative. Accessibility collapse is moderate-high (0.72) because once the dual-class structure is understood, the alternative of 'governance parity within this company' collapses, but the alternative of 'invest elsewhere' remains fully open. Resistance is low (0.18) — the arrangement persists because it is chosen, not because it is enforced.
 *
 * PERSPECTIVAL GAP:
 *   From the founder/insider seat, the constraint is pure coordination — a contractual device that solves the capital-formation problem. From the institutional investor seat, the same structure operates as mild extraction — they are price-takers on governance terms due to index mandates. From the governance advocate seat, the disclosure regime is a snare's cover story — consent is manufactured by structural lack of alternatives. The engine computes these divergences from the declared power/exit/scope data.
 *
 * DIRECTIONALITY LOGIC:
 *   Founders/insiders are structural beneficiaries (d ~ 0.15): they retain control and capture control premium. Public Class A investors are near-symmetric (d ~ 0.5): they consent to the terms and receive priced returns. Institutional investors are mild payers (d ~ 0.6): constrained by index mandates, they bear governance costs without full exit freedom. Governance advocates are excluded (d not computed): they bear no direct cost but their policy preferences are suppressed. SEC regulators are analytical observers (d = 0.5). Index providers are beneficiaries (d ~ 0.2): they collect fees on broad indices that include dual-class names.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (access to patient capital without control dilution) is contested as live vs. solved. If dead, the arrangement persists as mandatrophy — a coordination device whose original justification has atrophied but which continues via institutional inertia (index inclusion, precedent). The disclosure-consent reading denies mandatrophy by asserting the problem remains live; the minority-extraction reading asserts mandatrophy is resolved in favor of extraction. This story authors the disclosure-consent reading's own lights: mandatrophy unresolved, founding problem contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    informed_consent_effectiveness,
    'Does S-1 disclosure actually achieve informed consent for Class A investors, or is consent nominal given complexity, retail inattention, and index-fund intermediation?',
    'Empirical studies of retail investor comprehension of dual-class terms in prospectuses; analysis of index-fund voting behavior and whether passive investors can express governance preferences.',
    'If consent is nominal, the rope classification collapses toward snare/tangled_rope — the coordination story becomes cover for extraction. If consent is genuine, rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(informed_consent_effectiveness, empirical, 'Whether the disclosure mechanism delivers the informed consent the reading''s legitimacy depends on.').

omega_variable(
    contractual_choice_vs_structural_power,
    'Does the ''contractual choice'' framing obscure a structural power asymmetry where founders set the terms of a take-it-or-leave-it offer in a concentrated IPO market?',
    'Analyze IPO market concentration, founder bargaining power vs. underwriter/investor power, and whether single-class alternatives are economically viable for comparable companies.',
    'If structural asymmetry is high, the reading''s core premise (voluntary contract between equals) is undermined; classification shifts toward tangled_rope or snare. If asymmetry is low, the reading stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(contractual_choice_vs_structural_power, conceptual, 'Whether the contractual framing masks structural coercion in the IPO bargaining process.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the contested kernel properly ''dual_class_legitimacy'' or should it be ''corporate_governance_legitimacy'' or ''shareholder_rights_scope'' — and does the kernel boundary choice determine which readings appear as siblings?',
    'Compare classification outcomes under alternative kernel boundaries: does a broader kernel absorb the disclosure_consent reading as one variant of governance legitimacy, or does a narrower kernel isolate the dual-class specific dispute?',
    'If kernel framing is under-determined, the reading_relations and axiom declarations become boundary-dependent — the same structural claim could be ''coexists_with'' under one kernel and ''forecloses'' under another.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the kernel identification itself is contested and affects the structural relations between readings.').

omega_variable(
    valuation_pricing_efficiency,
    'Is the governance disparity fully priced into Class A valuations, or does a persistent ''governance discount'' indicate market inefficiency or incomplete information?',
    'Longitudinal event studies of dual-class IPOs vs. single-class peers; analysis of conversion announcements and sunset provision adoptions for abnormal returns.',
    'If disparity is not fully priced, the ''priced into valuation'' claim fails and public investors bear uncompensated extraction — classification shifts toward snare. If fully priced, the rope claim holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(valuation_pricing_efficiency, empirical, 'Whether market pricing validates the contractual-choice premise.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dual_class_legitimacy__disclosure_consent, 1980, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dual_class_legitimacy__disclosure_consent_tr_t1980, dual_class_legitimacy__disclosure_consent, theater_ratio, 1980, 0.05).
narrative_ontology:measurement(dual_class_legitimacy__disclosure_consent_tr_t1995, dual_class_legitimacy__disclosure_consent, theater_ratio, 1995, 0.07).
narrative_ontology:measurement(dual_class_legitimacy__disclosure_consent_tr_t2004, dual_class_legitimacy__disclosure_consent, theater_ratio, 2004, 0.09).
narrative_ontology:measurement(dual_class_legitimacy__disclosure_consent_tr_t2012, dual_class_legitimacy__disclosure_consent, theater_ratio, 2012, 0.1).
narrative_ontology:measurement(dual_class_legitimacy__disclosure_consent_tr_t2017, dual_class_legitimacy__disclosure_consent, theater_ratio, 2017, 0.11).
narrative_ontology:measurement(dual_class_legitimacy__disclosure_consent_tr_t2025, dual_class_legitimacy__disclosure_consent, theater_ratio, 2025, 0.12).

% Extraction over time
narrative_ontology:measurement(dual_class_legitimacy__disclosure_consent_be_t1980, dual_class_legitimacy__disclosure_consent, base_extractiveness, 1980, 0.03).
narrative_ontology:measurement(dual_class_legitimacy__disclosure_consent_be_t1995, dual_class_legitimacy__disclosure_consent, base_extractiveness, 1995, 0.05).
narrative_ontology:measurement(dual_class_legitimacy__disclosure_consent_be_t2004, dual_class_legitimacy__disclosure_consent, base_extractiveness, 2004, 0.06).
narrative_ontology:measurement(dual_class_legitimacy__disclosure_consent_be_t2012, dual_class_legitimacy__disclosure_consent, base_extractiveness, 2012, 0.07).
narrative_ontology:measurement(dual_class_legitimacy__disclosure_consent_be_t2017, dual_class_legitimacy__disclosure_consent, base_extractiveness, 2017, 0.08).
narrative_ontology:measurement(dual_class_legitimacy__disclosure_consent_be_t2025, dual_class_legitimacy__disclosure_consent, base_extractiveness, 2025, 0.08).

% Suppression requirement over time
narrative_ontology:measurement(dual_class_legitimacy__disclosure_consent_su_t1980, dual_class_legitimacy__disclosure_consent, suppression_requirement, 1980, 0.02).
narrative_ontology:measurement(dual_class_legitimacy__disclosure_consent_su_t1995, dual_class_legitimacy__disclosure_consent, suppression_requirement, 1995, 0.03).
narrative_ontology:measurement(dual_class_legitimacy__disclosure_consent_su_t2004, dual_class_legitimacy__disclosure_consent, suppression_requirement, 2004, 0.04).
narrative_ontology:measurement(dual_class_legitimacy__disclosure_consent_su_t2012, dual_class_legitimacy__disclosure_consent, suppression_requirement, 2012, 0.05).
narrative_ontology:measurement(dual_class_legitimacy__disclosure_consent_su_t2017, dual_class_legitimacy__disclosure_consent, suppression_requirement, 2017, 0.05).
narrative_ontology:measurement(dual_class_legitimacy__disclosure_consent_su_t2025, dual_class_legitimacy__disclosure_consent, suppression_requirement, 2025, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dual_class_legitimacy__disclosure_consent, resource_allocation).
narrative_ontology:boltzmann_floor_override(dual_class_legitimacy__disclosure_consent, 0.1).
narrative_ontology:affects_constraint(dual_class_legitimacy__disclosure_consent, dual_class_legitimacy__founder_stewardship).
narrative_ontology:affects_constraint(dual_class_legitimacy__disclosure_consent, dual_class_legitimacy__minority_extraction).
narrative_ontology:affects_constraint(dual_class_legitimacy__disclosure_consent, securities_act_disclosure_regime).
narrative_ontology:affects_constraint(dual_class_legitimacy__disclosure_consent, index_inclusion_criteria).

% DUAL FORMULATION NOTE:
% This constraint (disclosure_consent) and its siblings (founder_stewardship, minority_extraction) form a constraint family decomposing the natural-language concept 'dual-class legitimacy.' Each reading authors a different ε: disclosure_consent (ε≈0.08, rope), founder_stewardship (ε≈0.15, rope/tangled_rope), minority_extraction (ε≈0.65, snare). The ε-invariance principle requires separate stories because the referent (the standing dual-class arrangement) is evaluated differently by each reading's epistemic premises.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dual_class_legitimacy__disclosure_consent, organized, 0.6).
constraint_indexing:directionality_override(dual_class_legitimacy__disclosure_consent, moderate, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
