% ============================================================================
% CONSTRAINT STORY: dual_class_legitimacy__disclosure_consent
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
 *   human_readable: Dual-Class Legitimacy via Disclosure Consent
 *   domain: corporate_governance/securities_law/organizational_economics
 *
 * SUMMARY:
 *   This constraint story captures the 'disclosure consent' reading of
 *   dual-class share legitimacy: the structure is a contractual choice
 *   validated by Securities Act disclosure. Investors buy Class A shares with
 *   full knowledge of voting disparity; the governance discount is priced in;
 *   no coercion occurs because exit (not buying, or selling) is always
 *   available. The constraint is neither coordination (rope) nor extraction
 *   (snare) in its pure form — it is a market-mediated bargain where
 *   legitimacy derives from informed consent, not control parity. This
 *   reading sits alongside two sibling readings of the same kernel:
 *   'founder_stewardship' (control serves all shareholders) and
 *   'minority_extraction' (minority shareholders are entitled to proportional
 *   governance).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dual_class_legitimacy__disclosure_consent, 0.12).
domain_priors:suppression_score(dual_class_legitimacy__disclosure_consent, 0.18).
domain_priors:theater_ratio(dual_class_legitimacy__disclosure_consent, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__disclosure_consent, extractiveness, 0.12).
narrative_ontology:constraint_metric(dual_class_legitimacy__disclosure_consent, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(dual_class_legitimacy__disclosure_consent, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__disclosure_consent, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(dual_class_legitimacy__disclosure_consent, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dual_class_legitimacy__disclosure_consent, rope).
narrative_ontology:human_readable(dual_class_legitimacy__disclosure_consent, "Dual-Class Legitimacy via Disclosure Consent").
narrative_ontology:topic_domain(dual_class_legitimacy__disclosure_consent, "corporate_governance/securities_law/organizational_economics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dual_class_legitimacy__disclosure_consent, '9c998bd2-7d0b-450e-a431-3661545e5ebb').
narrative_ontology:cs_kernel_codification('9c998bd2-7d0b-450e-a431-3661545e5ebb', formalized).
narrative_ontology:cs_authority_grounding('9c998bd2-7d0b-450e-a431-3661545e5ebb', lineage).
narrative_ontology:cs_interpretation_layer_present('9c998bd2-7d0b-450e-a431-3661545e5ebb').
narrative_ontology:cs_reading_relation('9c998bd2-7d0b-450e-a431-3661545e5ebb', dual_class_legitimacy__founder_stewardship, coexists_with).
narrative_ontology:cs_reading_relation('9c998bd2-7d0b-450e-a431-3661545e5ebb', dual_class_legitimacy__minority_extraction, coexists_with).
narrative_ontology:cs_axiom('9c998bd2-7d0b-450e-a431-3661545e5ebb', foundational, securities_act_disclosure_suffices_for_legitimacy).
narrative_ontology:cs_axiom_status(securities_act_disclosure_suffices_for_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('9c998bd2-7d0b-450e-a431-3661545e5ebb', securities_act_disclosure_suffices_for_legitimacy, conventional).
narrative_ontology:cs_axiom('9c998bd2-7d0b-450e-a431-3661545e5ebb', secondary, governance_discount_is_priced_and_voluntary).
narrative_ontology:cs_axiom_status(governance_discount_is_priced_and_voluntary, holdable).
narrative_ontology:cs_axiom_grounding('9c998bd2-7d0b-450e-a431-3661545e5ebb', governance_discount_is_priced_and_voluntary, empirically_contingent).
narrative_ontology:cs_reference_frame('9c998bd2-7d0b-450e-a431-3661545e5ebb', securities_act_disclosure_framework).
narrative_ontology:cs_drift_state('9c998bd2-7d0b-450e-a431-3661545e5ebb', contemporary_governance_reform_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9c998bd2-7d0b-450e-a431-3661545e5ebb', '2026-08-03T14:22:17Z').
narrative_ontology:cs_kernel_id(dual_class_legitimacy__disclosure_consent, dual_class_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__disclosure_consent, founding_controllers).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__disclosure_consent, institutional_investors).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__disclosure_consent, securities_regulators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(dual_class_legitimacy__disclosure_consent, institutional_investors).
narrative_ontology:constraint_victim(dual_class_legitimacy__disclosure_consent, retail_investors).
narrative_ontology:constraint_vindicates(dual_class_legitimacy__disclosure_consent, informed_consent_doctrine).
narrative_ontology:constraint_vindicates(dual_class_legitimacy__disclosure_consent, contractual_freedom_in_corporate_charter).
narrative_ontology:constraint_vindicates(dual_class_legitimacy__disclosure_consent, securities_act_disclosure_adequacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold Class B shares with superior voting rights (typically 10:1). Set corporate strategy, control board composition, and determine whether dual-class structure persists. Justify structure as enabling long-horizon value creation. Can convert to single-class or sell control at a premium.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, founding_controllers, agenda_setter,
    institutional, generational, arbitrage, national).

% Purchase Class A shares in IPOs and secondary markets with full knowledge of voting disparity. Price governance discount into valuation models. Benefit from founder-led execution when it outperforms; bear cost when governance discount widens. Can exit by selling shares or engaging via stewardship teams.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, institutional_investors, beneficiary,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(dual_class_legitimacy__disclosure_consent, institutional_investors, payer).

% Enforce Securities Act disclosure regime (S-1 registration, ongoing reporting). Validate that dual-class terms are fully disclosed and not materially misleading. Their regulatory mandate is satisfied by disclosure adequacy, not governance equality. No extraction from the structure itself.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, securities_regulators, beneficiary,
    institutional, generational, analytical, national).

% Buy Class A shares often without deep governance analysis. Rely on index inclusion and market pricing to reflect governance discount. Bear disproportionate cost if discount mispriced. Limited voice in charter amendments; exit is selling at market price.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, retail_investors, payer,
    powerless, biographical, constrained, national).

% Argue that disclosure consent is fictional — investors have no practical alternative to dual-class IPOs in tech-heavy indices. Push for sunset provisions, time-phased conversion, or exchange listing standards requiring one-share-one-vote. Their voice is heard in policy debates but not in corporate charters.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, governance_reform_advocates, excluded,
    organized, biographical, mobile, national).

% Study long-run performance differentials between dual-class and single-class firms. Analyze whether governance discount persists, narrows, or reverses over firm lifecycle. Provide empirical basis for policy debate without direct stake in any issuer.
narrative_ontology:constraint_stakeholder(dual_class_legitimacy__disclosure_consent, academic_corporate_governance_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables founder-led firms to access public capital markets without surrendering strategic control, solving the coordination problem between long-horizon mission execution and diversified capital formation.
% TRANSFER_FUNCTION: Moves voting control from public shareholders (Class A) to founding controllers (Class B) at IPO; the economic transfer is the governance discount priced into Class A shares, which founding controllers internalize as control premium.
% ABSENT_VOICES: Retail investors who lack the analytical capacity to price governance risk; future shareholders who inherit the structure without participating in the original IPO consent; employees and stakeholders whose interests are bound to the firm but hold no voting power.
% DISAPPEARANCE_RATIONALE: If the disclosure-consent framework vanished overnight, exchanges would likely impose one-share-one-vote listing standards, founders would face pressure to adopt sunset provisions or forgo IPOs, and the pipeline of founder-led public companies would restructure around new governance norms.
% FOUNDING_PROBLEM: How to allow founder-led firms to go public without forcing them to choose between capital access and the long-horizon control that (proponents argue) drives innovation — the pre-1980s norm where dual-class was rare and controlled firms stayed private.
% FOUNDING_PROBLEM_CORROBORATION: SEC historical releases and exchange rule filings from the 1980s corroborate the coordination rationale; institutional investor testimony (e.g., CII, CFA Institute) and academic studies (e.g., Bebchuk & Kastiel 2017) contest whether the problem persists or has been replaced by rent extraction.
narrative_ontology:disappearance_verdict(dual_class_legitimacy__disclosure_consent, world_rearranges).
narrative_ontology:founding_problem_status(dual_class_legitimacy__disclosure_consent, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dual_class_legitimacy__disclosure_consent, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(dual_class_legitimacy__disclosure_consent, 'none', 1).
narrative_ontology:epsilon_provenance(dual_class_legitimacy__disclosure_consent, 0.12, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is low (0.12) because the primary transfer — voting control for capital — is priced at issuance; ongoing extraction is limited to the gap between promised and delivered long-horizon performance. Suppression is low (0.18) because no party is legally barred from exiting or proposing charter amendments (though practical barriers exist). Theater ratio (0.25) reflects the growing gap between 'long-horizon mission' rhetoric and observable control entrenchment in mature dual-class firms. Accessibility collapse (0.35) is moderate: alternatives (single-class IPOs, private markets) exist but are constrained by index inclusion dynamics. Resistance (0.42) is rising: governance reform advocates, institutional stewardship, and exchange proposals (NYSE/NASDAQ 2020s) create active pressure.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (founding controllers) experiences this as genuine coordination — they built the firm, they set the terms, investors agreed. The payer seats (retail investors, future shareholders) experience it as structural extraction — no practical alternative to dual-class in major indices, consent is fictional. The engine will compute this divergence from the structural data; the claimed_type (rope) reflects the author's structural judgment that the coordination function is real and the extraction is priced, not coerced.
 *
 * DIRECTIONALITY LOGIC:
 *   Founding controllers sit at the beneficiary end (d ≈ 0.15): they receive control premium and set agenda. Institutional investors are near symmetric (d ≈ 0.5): they price the discount and can exit. Retail investors are modest targets (d ≈ 0.6): they bear mispricing risk with constrained exit. Securities regulators are analytical beneficiaries (d ≈ 0.2): their disclosure mandate is satisfied. Governance reform advocates are excluded (d ≈ 0.7): their structural position is opposition without voice in charters. The engine computes these from beneficiary declarations, power, and exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (capital access without control surrender) remains live for early-stage founder-led firms but is contested for mature firms where control persists long after the coordination rationale fades. This reading avoids mandatrophy by treating the constraint as a continuing bargain — if the bargain fails (performance lags, discount widens), market discipline operates via share price and activist pressure. The theater ratio rise since 2012 signals accumulating mandatrophy risk in mature issuers.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consent_reality_vs_form,
    'Is investor consent to dual-class terms genuinely informed and voluntary, or is it structurally coerced by index inclusion and lack of single-class alternatives in key sectors?',
    'Empirical study of retail investor comprehension of dual-class terms at IPO; analysis of single-class IPO availability in tech/growth sectors; measurement of governance discount persistence vs. dissipation.',
    'If consent is structurally coerced, the constraint reclassifies from rope toward tangled_rope or snare — extraction without genuine coordination. If consent is genuine, rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_reality_vs_form, empirical, 'Whether the disclosure-consent foundation reflects real choice or market-structure coercion.').

omega_variable(
    kernel_framing_underdetermination,
    'Does the ''dual_class_legitimacy'' kernel admit only these three readings, or is there a fourth framing — e.g., ''institutional_complementarity'' where dual-class solves a hold-up problem between founders and specific investors?',
    'Literature review of corporate governance theory for alternative legitimacy claims; structural analysis of whether additional readings produce distinct ε/victim/beneficiary profiles.',
    'If a fourth reading exists with distinct structural profile, the kernel decomposition is incomplete and this story''s classification may shift under the alternative framing.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the declared sibling readings exhaust the kernel''s structural space.').

omega_variable(
    theater_ratio_driver,
    'Is the rising theater_ratio (0.15→0.25) driven by performative ''mission'' rhetoric masking control entrenchment, or by genuine coordination costs of communicating long-horizon strategy to dispersed shareholders?',
    'Textual analysis of founder letters, proxy statements, and earnings calls over time; correlation of rhetoric shifts with performance inflection points and control transactions.',
    'If performative, the constraint accumulates mandatrophy and trends toward piton. If genuine coordination cost, theater is the price of the rope function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_ratio_driver, empirical, 'Whether theater accumulation signals degradation or coordination overhead.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dual_class_legitimacy__disclosure_consent, 1980, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dual_class_legitimacy__disclosure_consent_tr_t1980, dual_class_legitimacy__disclosure_consent, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(dual_class_legitimacy__disclosure_consent_tr_t1995, dual_class_legitimacy__disclosure_consent, theater_ratio, 1995, 0.18).
narrative_ontology:measurement(dual_class_legitimacy__disclosure_consent_tr_t2005, dual_class_legitimacy__disclosure_consent, theater_ratio, 2005, 0.2).
narrative_ontology:measurement(dual_class_legitimacy__disclosure_consent_tr_t2012, dual_class_legitimacy__disclosure_consent, theater_ratio, 2012, 0.22).
narrative_ontology:measurement(dual_class_legitimacy__disclosure_consent_tr_t2017, dual_class_legitimacy__disclosure_consent, theater_ratio, 2017, 0.24).
narrative_ontology:measurement(dual_class_legitimacy__disclosure_consent_tr_t2021, dual_class_legitimacy__disclosure_consent, theater_ratio, 2021, 0.25).
narrative_ontology:measurement(dual_class_legitimacy__disclosure_consent_tr_t2025, dual_class_legitimacy__disclosure_consent, theater_ratio, 2025, 0.25).

% Extraction over time
narrative_ontology:measurement(dual_class_legitimacy__disclosure_consent_be_t1980, dual_class_legitimacy__disclosure_consent, base_extractiveness, 1980, 0.08).
narrative_ontology:measurement(dual_class_legitimacy__disclosure_consent_be_t1995, dual_class_legitimacy__disclosure_consent, base_extractiveness, 1995, 0.09).
narrative_ontology:measurement(dual_class_legitimacy__disclosure_consent_be_t2005, dual_class_legitimacy__disclosure_consent, base_extractiveness, 2005, 0.1).
narrative_ontology:measurement(dual_class_legitimacy__disclosure_consent_be_t2012, dual_class_legitimacy__disclosure_consent, base_extractiveness, 2012, 0.11).
narrative_ontology:measurement(dual_class_legitimacy__disclosure_consent_be_t2017, dual_class_legitimacy__disclosure_consent, base_extractiveness, 2017, 0.11).
narrative_ontology:measurement(dual_class_legitimacy__disclosure_consent_be_t2021, dual_class_legitimacy__disclosure_consent, base_extractiveness, 2021, 0.12).
narrative_ontology:measurement(dual_class_legitimacy__disclosure_consent_be_t2025, dual_class_legitimacy__disclosure_consent, base_extractiveness, 2025, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(dual_class_legitimacy__disclosure_consent_su_t1980, dual_class_legitimacy__disclosure_consent, suppression_requirement, 1980, 0.1).
narrative_ontology:measurement(dual_class_legitimacy__disclosure_consent_su_t1995, dual_class_legitimacy__disclosure_consent, suppression_requirement, 1995, 0.12).
narrative_ontology:measurement(dual_class_legitimacy__disclosure_consent_su_t2005, dual_class_legitimacy__disclosure_consent, suppression_requirement, 2005, 0.14).
narrative_ontology:measurement(dual_class_legitimacy__disclosure_consent_su_t2012, dual_class_legitimacy__disclosure_consent, suppression_requirement, 2012, 0.16).
narrative_ontology:measurement(dual_class_legitimacy__disclosure_consent_su_t2017, dual_class_legitimacy__disclosure_consent, suppression_requirement, 2017, 0.17).
narrative_ontology:measurement(dual_class_legitimacy__disclosure_consent_su_t2021, dual_class_legitimacy__disclosure_consent, suppression_requirement, 2021, 0.18).
narrative_ontology:measurement(dual_class_legitimacy__disclosure_consent_su_t2025, dual_class_legitimacy__disclosure_consent, suppression_requirement, 2025, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dual_class_legitimacy__disclosure_consent, resource_allocation).
narrative_ontology:boltzmann_floor_override(dual_class_legitimacy__disclosure_consent, 0.15).
narrative_ontology:affects_constraint(dual_class_legitimacy__disclosure_consent, dual_class_legitimacy__founder_stewardship).
narrative_ontology:affects_constraint(dual_class_legitimacy__disclosure_consent, dual_class_legitimacy__minority_extraction).
narrative_ontology:affects_constraint(dual_class_legitimacy__disclosure_consent, exchange_listing_standards).
narrative_ontology:affects_constraint(dual_class_legitimacy__disclosure_consent, index_inclusion_governance).

% DUAL FORMULATION NOTE:
% This is the disclosure_consent reading of the dual_class_legitimacy kernel. The founder_stewardship reading claims control serves all shareholders; the minority_extraction reading claims proportional governance is a right. This reading claims legitimacy = informed consent under Securities Act disclosure. The three readings share the same referent (dual-class capital structures) but instantiate different constraints with different ε, beneficiaries, and types.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dual_class_legitimacy__disclosure_consent, organized, 0.45).
constraint_indexing:directionality_override(dual_class_legitimacy__disclosure_consent, powerless, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
