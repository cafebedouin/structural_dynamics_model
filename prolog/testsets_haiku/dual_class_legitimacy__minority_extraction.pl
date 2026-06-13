% ============================================================================
% CONSTRAINT STORY: dual_class_legitimacy__minority_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dual_class_legitimacy__minority_extraction, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: dual_class_legitimacy__minority_extraction
 *   human_readable: Dual-Class Share Structure: Minority Governance Extraction
 *   domain: corporate_governance/securities_law/organizational_economics
 *
 * SUMMARY:
 *   Dual-class share structures with super-voting founder shares create a
 *   constitutional asymmetry: minority public shareholders contribute capital
 *   and bear economic risk proportional to their investment, but hold zero
 *   governance control over how that capital is deployed and how strategic
 *   risk is managed. This constraint story instantiates the MINORITY
 *   EXTRACTION READING of the dual-class legitimacy kernel — the claim that
 *   governance voice should be proportional to capital and risk contribution,
 *   and that dual-class structures systematically violate this
 *   proportionality in order to extract governance rents to the founder. This
 *   reading competes with two siblings: the founder_stewardship reading
 *   (concentrated control serves all shareholders by enabling long-horizon
 *   execution) and the disclosure_consent reading (legitimacy rests on
 *   informed consent, not control parity). The minority_extraction reading
 *   asserts that the transfer of governance value is structurally extractive
 *   regardless of disclosure adequacy or founder intention.
 *
 * KEY AGENTS:
 *   - founder_control_group: institutional power, agenda-setter, collects governance rents and holds unilateral decision authority.
 *   - minority_public_shareholders: organized power, payers, bear full economic risk with zero governance voice.
 *   - institutional_investors: powerful, payers, structurally barred from effective oversight and locked into passive capital provision.
 *   - stock_exchange_operators: institutional, agenda-setters, benefit from listing fees and dual-class structures that attract founder-led IPOs.
 *   - securities_regulators: institutional, observers, maintain permissive disclosure-based legitimation framework.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dual_class_legitimacy__minority_extraction, 0.68).
domain_priors:suppression_score(dual_class_legitimacy__minority_extraction, 0.72).
domain_priors:theater_ratio(dual_class_legitimacy__minority_extraction, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, extractiveness, 0.68).
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(dual_class_legitimacy__minority_extraction, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dual_class_legitimacy__minority_extraction, tangled_rope).
narrative_ontology:human_readable(dual_class_legitimacy__minority_extraction, "Dual-Class Share Structure: Minority Governance Extraction").
narrative_ontology:topic_domain(dual_class_legitimacy__minority_extraction, "corporate_governance/securities_law/organizational_economics").

domain_priors:requires_active_enforcement(dual_class_legitimacy__minority_extraction).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dual_class_legitimacy__minority_extraction, 'e3a3f97b-bf60-49c3-81bb-dd00be663cf0').
narrative_ontology:cs_kernel_codification('e3a3f97b-bf60-49c3-81bb-dd00be663cf0', formalized).
narrative_ontology:cs_authority_grounding('e3a3f97b-bf60-49c3-81bb-dd00be663cf0', extraction).
narrative_ontology:cs_interpretation_layer_present('e3a3f97b-bf60-49c3-81bb-dd00be663cf0').
narrative_ontology:cs_reading_relation('e3a3f97b-bf60-49c3-81bb-dd00be663cf0', dual_class_legitimacy__founder_stewardship, coexists_with).
narrative_ontology:cs_reading_relation('e3a3f97b-bf60-49c3-81bb-dd00be663cf0', dual_class_legitimacy__disclosure_consent, coexists_with).
narrative_ontology:cs_axiom('e3a3f97b-bf60-49c3-81bb-dd00be663cf0', foundational, governance_proportionality_to_capital_and_risk).
narrative_ontology:cs_axiom_status(governance_proportionality_to_capital_and_risk, holdable).
narrative_ontology:cs_axiom_grounding('e3a3f97b-bf60-49c3-81bb-dd00be663cf0', governance_proportionality_to_capital_and_risk, deontological).
narrative_ontology:cs_axiom('e3a3f97b-bf60-49c3-81bb-dd00be663cf0', secondary, control_extraction_is_legitimate_cost_of_public_shareholding).
narrative_ontology:cs_axiom_status(control_extraction_is_legitimate_cost_of_public_shareholding, overridden).
narrative_ontology:cs_axiom_grounding('e3a3f97b-bf60-49c3-81bb-dd00be663cf0', control_extraction_is_legitimate_cost_of_public_shareholding, instrumental).
narrative_ontology:cs_reference_frame('e3a3f97b-bf60-49c3-81bb-dd00be663cf0', proportional_governance_parity).
narrative_ontology:cs_drift_state('e3a3f97b-bf60-49c3-81bb-dd00be663cf0', contemporary_dual_class_expansion_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e3a3f97b-bf60-49c3-81bb-dd00be663cf0', '').
narrative_ontology:cs_kernel_id(dual_class_legitimacy__minority_extraction, dual_class_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__minority_extraction, founder_control_group).
narrative_ontology:constraint_victim(dual_class_legitimacy__minority_extraction, minority_public_shareholders).
narrative_ontology:constraint_victim(dual_class_legitimacy__minority_extraction, institutional_investors).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dual_class_legitimacy__minority_extraction, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(dual_class_legitimacy__minority_extraction, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dual_class_legitimacy__minority_extraction_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dual_class_legitimacy__minority_extraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dual_class_legitimacy__minority_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is measured as governance-value transfer per unit capital at risk. At IPO (t=0), the dual-class structure transfers 48% of potential governance value from public shareholders to founder; this proportion grows as the company matures and strategic decisions accumulate (the founder exercises control on acquisitions, divestitures, compensation policy, related-party transactions). By t=25, extractiveness reaches 68% — the governance value transfer has deepened and solidified. Suppression is high (0.72) because the constraint's persistence depends on actively suppressing governance alternatives: minority shareholders cannot force conversion to one-share-one-vote, cannot remove the founder, cannot demand related-party transaction approval. They can only exit by selling their shares, which exits them from the constraint entirely (mobile, not trapped, because the exit is always available). Theater_ratio rises from 22% to 41% as independent board oversight mechanisms (audit committees, governance committees) are formalized but their actual constraining power over founder decisions remains minimal — the theater of governance grows while substantive minority voice shrinks. Accessibility_collapse (0.58) reflects that once the dual-class structure is disclosed and understood, minority shareholders' alternatives collapse: they can hold the shares, sell them, or exit the public market entirely, but they cannot access governance voice within the framework. Resistance (0.64) is moderate-high because institutional investors have begun filing shareholder proposals and governance activists periodically challenge dual-class structures, but the resistance is non-binding (advisory votes, regulatory pressure without mandatory rules) and founders routinely ignore it.
 *
 * PERSPECTIVAL GAP:
 *   The founder and the minority shareholders compute radically different constraint types from the same structural facts. From the founder's seat, dual-class is rope — genuine coordination on long-term strategy, disclosure-compliant, and value-creating for all shareholders (the founder's perception is that minority shareholders benefit from long-horizon execution). From the minority-shareholder seat, it is snare — governance extraction dressed as mission protection, with no exit except abandonment of the investment. The engine computes per-seat: from the founder's institutional power and arbitrage exit, directionality approaches full beneficiary (d near 0.0, negative extraction). From the minority-shareholder organized power with constrained exit (cannot exit without exiting the investment entirely), directionality approaches target (d near 0.8–0.9, high effective extraction). The single authored claim (tangled_rope) and metrics sit between: it is genuinely extractive (snare-adjacent), but it possesses a real coordination function (founder-stewardship is not a pure fiction), hence tangled.
 *
 * DIRECTIONALITY LOGIC:
 *   Founder control group: beneficiary seat, directionality d ≈ 0.1 (full beneficiary). They set the rules, collect the governance value, and face no downside for bad decisions (founder reputational damage is real but weaker than the governance upside). Power=institutional, exit=arbitrage, beneficiary status all drive d toward 0.0. Minority public shareholders: target seat, directionality d ≈ 0.82 (near-full target). They contribute capital, bear risk proportional to investment, exercise zero governance voice, and cannot opt out without exiting the investment entirely. Power=organized (collective action is possible but constrained by collective-action problems), exit=constrained (can sell but not to a better governance frame), victim status (governance value extracted) all drive d toward 1.0. Institutional investors: target-adjacent, directionality d ≈ 0.75. They are institutionally powerful but identity-locked to passive capital provision (fiduciary duty to their own beneficiaries constrains active governance intervention). They can exit more easily than retail shareholders but face portfolio-level pressure to hold winners even under dual-class. The identity_locked exit_options modulates d downward from pure target, but not by much because they are still structurally barred from effective voice. Stock exchange operators: beneficiary-adjacent, directionality d ≈ 0.25. They benefit from founder-led IPOs (trading volume, prestige) and face competitive pressure from permissive exchanges. They are not victims (they collect listing fees), but they are not full beneficiaries either (they do not control whether companies adopt dual-class, only whether they list; the founder controls the actual adoption). No overrides are needed — the derivation chain produces defensible per-seat d values from the structural data.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint appears to suffer from mandatrophy: the founding problem (protection from short-termist pressure during critical execution) was historically live (1990s–2010s when activist pressure on tech founders was rising) but is increasingly contested. The founding_problem_status is CONTESTED because: (1) empirical research does not show that dual-class structures systematically outperform one-share-one-vote structures long-term (Gompers et al., Tobin & others find mixed-to-negative effects on return on equity and ROA); (2) activist pressure on tech founders is not the primary failure mode observed (related-party transactions and founder entrenchment produce more losses than activist intervention prevents); (3) the problem statement itself ('founders need protection from short-termism') is no longer widely accepted outside founder and board circles — it is a proposition vindicated by founder assertion, not by corroboration from institutional investors or governance researchers. Yet the constraint persists with rising theater_ratio (independent governance structures, shareholder advisory votes, ESG criteria on dual-class) that maintain the appearance of governance without transferring voice. The persistence despite mandatrophy is sustained by: (1) extractiveness accumulation — as companies mature, strategic decisions compound (related-party transactions, strategic pivots) and founder extraction increases; (2) suppression intensification — rules restricting dual-class adoption (SEC proposals, NYSE/NASDAQ tightening on new listings) have been weakened or blocked, so the enforcement machinery to maintain dual-class has hardened; (3) theater expansion — governance theater (independent committees, advisory votes) creates the appearance of accountability without substance, masking the extraction beneath procedural legitimacy. The mismatch between founding problem (status=contested, disappearance_verdict=world_rearranges) signals mandatrophy: if the problem disappeared, the constraint should also disappear, but it is persisting and intensifying despite the problem's obsolescence. This is the classic Piton-adjacent pattern, except the constraint is classified as Tangled Rope because extraction is not yet fully theatricalized — there is still real founder stewardship and real coordination value, so it is not purely inertial. As the founding problem dies, the constraint may migrate toward Piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_empirical_status,
    'Do companies with dual-class structures systematically outperform one-share-one-vote peers long-term, controlling for founder quality and market conditions? Is activist pressure (the founding problem) the primary failure mode dual-class prevents, or is founder entrenchment the primary cost?',
    'Multi-cohort panel regression on long-term stock performance, acquisition success rates, and executive compensation ratios, comparing dual-class and one-vote-one-share matched pairs over 15+ year periods. Meta-analysis of existing empirical research (Gompers et al., Tobin, ISS studies).',
    'If dual-class significantly outperforms, the founding problem is still live and the tangled-rope classification holds. If dual-class underperforms or shows no significant difference, the founding problem is dead and the constraint approaches piton (performance no better than alternatives, persisted by theater and founder capture). If founder entrenchment costs exceed activist-pressure benefits, the constraint is snare-tilted rather than tangled.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_empirical_status, empirical, 'Whether the founding problem is empirically live or dead.').

omega_variable(
    governance_proportionality_foundational,
    'Is the axiom ''governance voice should be proportional to capital and risk contribution'' a coherent normative principle applicable across all shareholding structures, or is it a contestable principle that founders legitimately reject?',
    'Jurisprudence analysis of corporate law doctrine (does law treat proportionality as foundational or permissive?), shareholder theory literature (do governance theorists treat proportionality as first-order or derivative?), comparable commitment systems (do constitutions, cooperatives, or other stakeholder organizations treat proportionality similarly?).',
    'If proportionality is treated as foundational in competing commitment systems (constitutions, democratic governance, cooperatives), this reading''s axiom is universalizable and the constraint is snare-tilted (violating foundational commitments elsewhere). If proportionality is treated as contestable and founder discretion is permissible, the constraint remains tangled (genuine disagreement on legitimacy). If law explicitly permits dual-class as a valid exception to proportionality, the constraint is rope-tilted (founder stewardship framing dominates).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(governance_proportionality_foundational, conceptual, 'Whether proportionality is a foundational principle or a contestable design choice.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.72) structural (minority shareholders cannot force conversion or governance inclusion because of lock-in and legal barriers) or partly internalized (minority shareholders have internalized the founder-stewardship narrative and believe governance asymmetry is legitimate)?',
    'Post-conversion / post-liquidity trajectory: when dual-class constraints are removed (company goes private, converts to one-share-one-vote, or founders exit), does minority-shareholder governance participation or demand change? If participation increases sharply, suppression was partly internalized; if participation remains low, suppression is primarily structural.',
    'If partly internalized, the constraint''s effective suppression is higher than the structural measure (minority shareholders carry the suppression forward even after removal of the mechanism). If primarily structural, removal would immediately enable governance voice. Either way, the constraint persists through some mix of lock-in and belief.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression is primarily structural barriers or internalized belief in founder legitimacy.').

omega_variable(
    controlled_company_exemptions_extraction_channel,
    'Do controlled-company exemptions from mandatory governance protections (e.g., NYSE Rule 303A.00 exemptions that allow founder-controlled boards to skip independent committees) materially increase extraction, or are they a side effect of dual-class adoption with limited independent impact?',
    'Comparative analysis of controlled-company exemption usage (which companies claim which exemptions, at what cost) and outcomes (do exemption-heavy companies show higher related-party transaction volumes, worse M&A outcomes, higher executive compensation multiples?). Difference-in-differences analysis comparing controlled companies that claim exemptions vs. those that voluntarily comply.',
    'If exemptions materially enable extraction (related-party transactions increase, governance outcomes worsen), they are an independent extraction channel and the constraint''s extractiveness is partially attributable to exemption design. If exemptions show minimal independent effect, the constraint''s extractiveness is primarily from dual-class structure alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(controlled_company_exemptions_extraction_channel, empirical, 'Whether governance exemptions are a separate extraction mechanism or a side effect.').

omega_variable(
    sibling_reading_foreclusion_test,
    'Can all three readings of the dual-class legitimacy kernel (disclosure_consent, founder_stewardship, minority_extraction) coexist in a single institutional framework, or does the minority_extraction reading logically foreclose one or both siblings?',
    'Legal/institutional logic test: can a jurisdiction simultaneously (1) permit informed-consent-based dual-class, (2) treat founder stewardship as a legitimate governance value, AND (3) require governance proportionality to capital/risk? If yes, they coexist. If a jurisdiction has enacted law that explicitly rejects one reading in favor of another, the foreclosure relation holds.',
    'If all three coexist, the readings are properly modeled as coexists_with edges. If minority_extraction forecloses founder_stewardship (because proportionality requirement is incompatible with unilateral stewardship), the relation is forecloses. If disclosure_consent forecloses both others (by treating informed consent as sufficient regardless of proportionality), the architecture differs.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_foreclusion_test, conceptual, 'Logical relationship between the three contested readings of dual-class legitimacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dual_class_legitimacy__minority_extraction, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dual_tr_t0, dual_class_legitimacy__minority_extraction, theater_ratio, 0, 0.22).
narrative_ontology:measurement(dual_tr_t5, dual_class_legitimacy__minority_extraction, theater_ratio, 5, 0.27).
narrative_ontology:measurement(dual_tr_t10, dual_class_legitimacy__minority_extraction, theater_ratio, 10, 0.32).
narrative_ontology:measurement(dual_tr_t15, dual_class_legitimacy__minority_extraction, theater_ratio, 15, 0.38).
narrative_ontology:measurement(dual_tr_t20, dual_class_legitimacy__minority_extraction, theater_ratio, 20, 0.4).
narrative_ontology:measurement(dual_tr_t25, dual_class_legitimacy__minority_extraction, theater_ratio, 25, 0.41).

% Extraction over time
narrative_ontology:measurement(dual_be_t0, dual_class_legitimacy__minority_extraction, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(dual_be_t5, dual_class_legitimacy__minority_extraction, base_extractiveness, 5, 0.54).
narrative_ontology:measurement(dual_be_t10, dual_class_legitimacy__minority_extraction, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(dual_be_t15, dual_class_legitimacy__minority_extraction, base_extractiveness, 15, 0.65).
narrative_ontology:measurement(dual_be_t20, dual_class_legitimacy__minority_extraction, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(dual_be_t25, dual_class_legitimacy__minority_extraction, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(dual_su_t0, dual_class_legitimacy__minority_extraction, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(dual_su_t5, dual_class_legitimacy__minority_extraction, suppression_requirement, 5, 0.61).
narrative_ontology:measurement(dual_su_t10, dual_class_legitimacy__minority_extraction, suppression_requirement, 10, 0.67).
narrative_ontology:measurement(dual_su_t15, dual_class_legitimacy__minority_extraction, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(dual_su_t20, dual_class_legitimacy__minority_extraction, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(dual_su_t25, dual_class_legitimacy__minority_extraction, suppression_requirement, 25, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dual_class_legitimacy__minority_extraction, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(dual_class_legitimacy__minority_extraction, 0.12).
narrative_ontology:affects_constraint(dual_class_legitimacy__minority_extraction, dual_class_legitimacy__disclosure_consent).
narrative_ontology:affects_constraint(dual_class_legitimacy__minority_extraction, dual_class_legitimacy__founder_stewardship).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the dual-class legitimacy kernel. All three generate distinct ε-invariant constraints because they instantiate different measurement bases for legitimacy: disclosure_consent treats informed consent as adequate (lower ε), founder_stewardship treats founder long-horizon execution as value-creating for all (lower ε), minority_extraction treats governance asymmetry as extraction by design (higher ε). They are linked by network.affects_constraints because the empirical status of the founding problem (activist pressure harm vs. founder entrenchment harm) influences the plausibility of all three readings simultaneously.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dual_class_legitimacy__minority_extraction, organized, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
