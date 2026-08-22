% ============================================================================
% CONSTRAINT STORY: nafta_jurisdictional_boundary__capital_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nafta_jurisdictional_boundary__capital_supremacy_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: nafta_jurisdictional_boundary__capital_supremacy_reading
 *   human_readable: Trade Agreement Text as Supreme Law Overriding Domestic Regulatory Standards (Capital Supremacy Reading)
 *   domain: international_trade_law/political_economy/regulatory_federalism
 *
 * SUMMARY:
 *   This story instantiates the capital-supremacy reading of the NAFTA
 *   jurisdictional-boundary kernel: the treaty text (and its successor
 *   instruments) is read as supreme law that binds domestic regulatory
 *   agencies, with investor protection and capital mobility as the
 *   load-bearing, enforceable obligations while labor and environmental
 *   provisions remain comparatively unenforceable side commitments. This is a
 *   distinct constraint from the embedded_liberalism_reading (which treats
 *   the same text as balancing market access with legitimate domestic policy
 *   space) and the sovereignty_primacy_reading (which treats domestic law as
 *   retaining full authority). Each reading has its own ε, its own
 *   beneficiary/victim structure, and its own classification; they are
 *   linked, not merged, via network.affects_constraints. Under this reading,
 *   extraction runs upward from domestic regulatory capacity and immobile
 *   labor to capital-mobile investors and the arbitration apparatus, and this
 *   is a substantially extractive tangled rope: it does coordinate a real
 *   cross-border investment problem, but does so by asymmetrically enforcing
 *   investor protections while leaving labor and environmental obligations
 *   comparatively toothless.
 *
 * KEY AGENTS:
 *   - multinational_investors: Primary beneficiary (institutional/arbitrage) — captures surplus from regulatory harmonization and relocation option value
 *   - cross_border_manufacturers: Beneficiary/secondary payer (powerful/mobile) — restructures supply chains to exploit differential standards
 *   - domestic_environmental_regulators: Primary target (institutional/constrained) — loses effective jurisdiction under litigation-risk chilling
 *   - unionized_manufacturing_workers: Primary target (powerless/trapped) — bears relocation threat with no standing to contest it
 *   - national_trade_ministries: Agenda-setter (institutional/constrained) — administers and defends the text but faces high renegotiation cost
 *   - trade_law_scholars: Analytical observer — documents asymmetric enforceability across chapters
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nafta_jurisdictional_boundary__capital_supremacy_reading, 0.79).
domain_priors:suppression_score(nafta_jurisdictional_boundary__capital_supremacy_reading, 0.72).
domain_priors:theater_ratio(nafta_jurisdictional_boundary__capital_supremacy_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, extractiveness, 0.79).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nafta_jurisdictional_boundary__capital_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(nafta_jurisdictional_boundary__capital_supremacy_reading, "Trade Agreement Text as Supreme Law Overriding Domestic Regulatory Standards (Capital Supremacy Reading)").
narrative_ontology:topic_domain(nafta_jurisdictional_boundary__capital_supremacy_reading, "international_trade_law/political_economy/regulatory_federalism").

domain_priors:requires_active_enforcement(nafta_jurisdictional_boundary__capital_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nafta_jurisdictional_boundary__capital_supremacy_reading, '139f0f6e-6304-4100-bf1c-0e780b281c0b').
narrative_ontology:cs_kernel_codification('139f0f6e-6304-4100-bf1c-0e780b281c0b', fixed_text).
narrative_ontology:cs_authority_grounding('139f0f6e-6304-4100-bf1c-0e780b281c0b', extraction).
narrative_ontology:cs_interpretation_layer_present('139f0f6e-6304-4100-bf1c-0e780b281c0b').
narrative_ontology:cs_reading_relation('139f0f6e-6304-4100-bf1c-0e780b281c0b', nafta_jurisdictional_boundary__embedded_liberalism_reading, coexists_with).
narrative_ontology:cs_reading_relation('139f0f6e-6304-4100-bf1c-0e780b281c0b', nafta_jurisdictional_boundary__sovereignty_primacy_reading, forecloses).
narrative_ontology:cs_axiom('139f0f6e-6304-4100-bf1c-0e780b281c0b', foundational, investor_protection_is_hierarchically_supreme).
narrative_ontology:cs_axiom_status(investor_protection_is_hierarchically_supreme, holdable).
narrative_ontology:cs_axiom_grounding('139f0f6e-6304-4100-bf1c-0e780b281c0b', investor_protection_is_hierarchically_supreme, conventional).
narrative_ontology:cs_axiom('139f0f6e-6304-4100-bf1c-0e780b281c0b', secondary, capital_mobility_obligation_binds_regardless_of_domestic_democratic_outcome).
narrative_ontology:cs_axiom_status(capital_mobility_obligation_binds_regardless_of_domestic_democratic_outcome, holdable).
narrative_ontology:cs_axiom_grounding('139f0f6e-6304-4100-bf1c-0e780b281c0b', capital_mobility_obligation_binds_regardless_of_domestic_democratic_outcome, instrumental).
narrative_ontology:cs_reference_frame('139f0f6e-6304-4100-bf1c-0e780b281c0b', investor_protection_as_binding_supreme_obligation).
narrative_ontology:cs_drift_state('139f0f6e-6304-4100-bf1c-0e780b281c0b', post_usmca_renegotiation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('139f0f6e-6304-4100-bf1c-0e780b281c0b', '').
narrative_ontology:cs_kernel_id(nafta_jurisdictional_boundary__capital_supremacy_reading, nafta_jurisdictional_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__capital_supremacy_reading, multinational_investors).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__capital_supremacy_reading, cross_border_manufacturers).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__capital_supremacy_reading, investor_state_arbitration_bar).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, domestic_environmental_regulators).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, domestic_labor_standards_bodies).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, unionized_manufacturing_workers).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, municipal_zoning_and_health_authorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, cross_border_manufacturers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Relocate production and capital across the treaty zone to whichever jurisdiction offers the lowest regulatory friction and factor cost, then invoke investor-state dispute mechanisms when a remaining domestic regulation impairs expected returns. Captures the surplus created when regulatory harmonization compresses the range of standards a host state can impose without triggering a claim.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, multinational_investors, beneficiary,
    institutional, generational, arbitrage, continental).

% Restructure supply chains to exploit tariff elimination and regulatory alignment, moving production to the jurisdiction with the weakest enforceable labor or environmental standard within the bloc. Pays only in the sense of exposure to arbitration if it becomes a claim target rather than a claimant, but overwhelmingly occupies the capturing side.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, cross_border_manufacturers, beneficiary,
    powerful, biographical, mobile, continental).
narrative_ontology:stakeholder_secondary_role(nafta_jurisdictional_boundary__capital_supremacy_reading, cross_border_manufacturers, payer).

% Administers and profits from the dispute-resolution machinery that adjudicates claims against domestic regulatory measures. Fees and rents flow to this class regardless of outcome; the existence of a supremacy-graded treaty text is the entire basis of its business model.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, investor_state_arbitration_bar, beneficiary,
    organized, biographical, arbitrage, continental).

% Draft and attempt to enforce pollution, chemical-safety, and land-use standards that are now subject to challenge as indirect expropriation or discriminatory treatment if they diminish an investor's expected profit. Must weigh new regulation against the treaty-imposed litigation risk, which chills standard-setting even absent a formal challenge.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, domestic_environmental_regulators, payer,
    institutional, generational, constrained, national).

% Set minimum wage, safety, and organizing-rights rules that lose force when production can relocate to a lower-standard jurisdiction inside the same treaty zone without tariff penalty. Enforcement mechanisms for labor-side agreements are consultative rather than binding, unlike the investor-protection provisions.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, domestic_labor_standards_bodies, payer,
    moderate, generational, constrained, national).

% Bear plant closures and wage suppression as employers credibly threaten relocation under the treaty's capital-mobility guarantees. Cannot relocate themselves and have no standing to bring a claim under the investor-protection chapter that constrains the regulators who might otherwise protect them.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, unionized_manufacturing_workers, payer,
    powerless, biographical, trapped, national).

% Issue local permitting and health decisions (waste sites, water use, land conversion) that can trigger federal exposure to investor claims, creating pressure from national governments on municipalities to avoid decisions that generate treaty liability. Their local mandate is subordinated to a jurisdictional structure they had no role in negotiating.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, municipal_zoning_and_health_authorities, payer,
    powerless, immediate, trapped, local).

% Negotiated and continue to administer the treaty text, defend it before arbitration panels, and manage the political cost of domestic regulatory rollbacks attributable to treaty exposure. Retains formal authority to renegotiate but faces high diplomatic and economic cost in doing so, making the arrangement sticky even for the seat that could change it.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, national_trade_ministries, agenda_setter,
    institutional, generational, constrained, continental).

% Would argue that the investor-protection chapters were negotiated without meaningful input from labor or environmental constituencies and that the resulting asymmetry between binding investment obligations and non-binding side-agreements on labor/environment was foreseeable and structural. They participate in public comment processes but have no seat in arbitration or renegotiation.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, environmental_justice_and_labor_advocates, excluded,
    organized, generational, constrained, national).

% Study the asymmetric enforceability between investment chapters and labor/environmental side letters, publish comparative analyses of chilling effects on domestic regulation, and testify in legislative reviews of treaty renewal or renegotiation.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, trade_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nafta_jurisdictional_boundary__capital_supremacy_reading, multinational_investors).
narrative_ontology:fixing_cost_class(nafta_jurisdictional_boundary__capital_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Reduces transaction costs for cross-border investment and trade by harmonizing rules and providing a predictable, depoliticized forum (investor-state arbitration) for resolving disputes between foreign capital and host governments, in place of ad hoc expropriation risk.
% TRANSFER_FUNCTION: Moves regulatory authority and the associated rents from domestic democratic and administrative institutions to capital-mobile investors and the arbitration apparatus that adjudicates claims against domestic standards; moves wage and safety costs downward onto workers and localities that cannot relocate.
% ABSENT_VOICES: Unionized workers, municipal authorities, and environmental/labor advocacy groups were not parties to treaty negotiation and have no standing before investment tribunals; their objections surface only in post hoc legislative hearings and public comment, well after the binding text is settled.
% DISAPPEARANCE_RATIONALE: If the capital-supremacy reading's enforcement machinery (investor-state dispute settlement plus the supremacy-of-text doctrine) vanished, domestic regulators would recover discretion to set environmental and labor standards without litigation exposure, cross-border relocation calculus would shift, and the arbitration bar's revenue base would collapse — a genuine reallocation of authority and income, not a return to an unaffected baseline.
% FOUNDING_PROBLEM: Cross-border investors faced genuine risk of uncompensated expropriation or discriminatory treatment by host governments, and firms needed predictable market access to justify long-horizon capital commitments across the treaty zone.
% FOUNDING_PROBLEM_CORROBORATION: National trade ministries and multinational investors attest the expropriation-risk problem remains live and justifies continued binding investor protection. Independent trade law scholars, legislative review testimony from labor and environmental bodies, and comparative studies of chilling-effect litigation attest that the mechanism has substantially outgrown the original expropriation-risk problem and now functions primarily to constrain ordinary, non-discriminatory domestic regulation — corroboration from outside the capital-mobility beneficiary set exists and diverges from the ministries' framing.
narrative_ontology:disappearance_verdict(nafta_jurisdictional_boundary__capital_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(nafta_jurisdictional_boundary__capital_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nafta_jurisdictional_boundary__capital_supremacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(nafta_jurisdictional_boundary__capital_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nafta_jurisdictional_boundary__capital_supremacy_reading, 0.79, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nafta_jurisdictional_boundary__capital_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nafta_jurisdictional_boundary__capital_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nafta_jurisdictional_boundary__capital_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises across the interval (0.48 to 0.79) as accumulated arbitration precedent progressively narrows the space domestic regulators can occupy without triggering exposure — each successful investor claim functions as a ratchet, not a one-time cost. Suppression (0.72 at end) reflects that persistence of this reading's supremacy doctrine depends on active enforcement through binding investor-state arbitration, in contrast to the merely consultative labor/environmental side mechanisms; this is a structural asymmetry, not a scope effect. Theater ratio is comparatively low (0.28) because the coordination function (predictable market access, reduced expropriation risk) is genuinely operative for investors even as the doctrine also does extractive work — this is not pure performance. Accessibility collapse (0.62) and resistance (0.58) reflect that alternative regulatory postures remain legally imaginable (domestic legislatures can still act) but are substantially chilled by anticipated litigation cost, while resistance from advocacy coalitions and some legislatures is real but has not reversed the doctrine's operation.
 *
 * PERSPECTIVAL GAP:
 *   From the national trade ministry seat, the arrangement looks like a durable diplomatic achievement it must defend; from the domestic regulator or worker seat, the same text operates as an externally imposed jurisdictional ceiling. The engine computes these as different per-seat classifications from the shared structural data — the divergence is not resolved by either seat's self-description.
 *
 * DIRECTIONALITY LOGIC:
 *   Multinational investors and the arbitration bar sit at the low-d, beneficiary end: the treaty subsidizes their mobility and monetizes disputes in their favor. Domestic regulators, labor bodies, and municipal authorities sit at the high-d, target end: their jurisdiction is what gets constrained, and their exit options are structurally limited (regulators cannot relocate their mandate; workers cannot relocate their labor across the same mobility the treaty grants capital). Cross-border manufacturers carry a secondary payer role because they face arbitration exposure themselves in rare cases, but overwhelmingly occupy the capturing position given their exit-option asymmetry relative to labor.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (genuine expropriation risk to long-horizon foreign investment) is contested rather than uniformly dead: it retains some liveness for genuinely vulnerable capital in genuinely unstable jurisdictions, which prevents this reading from being classified as a pure snare with no coordination remainder. But the corroboration record — independent scholarship and legislative testimony converging on chilling-effect findings that exceed what expropriation-risk mitigation would require — supports classifying this as tangled rope rather than rope: a real coordination kernel wrapped in enforcement machinery that now does asymmetric extractive work beyond that kernel's original scope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    supremacy_doctrine_vs_balanced_framework,
    'Does the treaty text, properly interpreted, establish investor protection as hierarchically supreme over domestic regulatory authority, or does it establish a balanced framework in which non-discriminatory domestic standards are compatible with treaty obligations?',
    'Comparative analysis of arbitral tribunal decisions over time: a supremacy reading is supported if tribunals consistently strike down non-discriminatory, generally applicable domestic standards as indirect expropriation; a balanced-framework reading is supported if tribunals consistently defer to non-discriminatory domestic measures under a reasonable-regulation exception.',
    'If tribunal practice trends toward consistent deference to non-discriminatory regulation, this reading''s high extractiveness score would be overstated and the embedded_liberalism_reading would better describe the doctrine''s actual operation; if tribunal practice trends toward striking down non-discriminatory measures, this reading is vindicated and the sovereignty_primacy_reading is empirically falsified as a description of the arrangement''s actual operation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supremacy_doctrine_vs_balanced_framework, conceptual, 'Whether the treaty operates as hierarchical supremacy or balanced coexistence — the kernel''s central interpretive fork.').

omega_variable(
    chilling_effect_magnitude,
    'How much domestic regulatory activity is actually deterred by anticipated investor-state claims, versus how much regulatory restraint would have occurred anyway for independent political-economic reasons?',
    'Comparative case studies of regulatory proposals withdrawn or weakened following investor threat letters, cross-referenced against counterfactual jurisdictions without equivalent treaty exposure but facing similar political pressures.',
    'A large measured chilling effect specific to treaty exposure supports the high extractiveness and suppression scores authored here; a small effect would suggest this reading overstates the doctrine''s causal contribution relative to domestic political economy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(chilling_effect_magnitude, empirical, 'Whether chilling effects are treaty-caused or would occur independently.').

omega_variable(
    renegotiation_feasibility,
    'Given that national trade ministries retain formal authority to renegotiate the treaty, is the doctrine''s persistence better explained by genuine political infeasibility of renegotiation, or by ministries'' own capture by the constituencies that benefit from the current asymmetry?',
    'Analysis of renegotiation episodes (e.g., USMCA revisions) to see whether labor/environmental enforceability provisions were strengthened when renegotiation occurred, indicating genuine responsiveness, versus whether investor-protection provisions were preserved while labor gains remained largely symbolic.',
    'If renegotiation episodes show asymmetric responsiveness (investor protections preserved, labor gains symbolic), this supports classifying national_trade_ministries as partially captured rather than a neutral agenda-setter, which would push toward a directionality override for that seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(renegotiation_feasibility, empirical, 'Whether ministry persistence reflects infeasibility or capture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nafta_jurisdictional_boundary__capital_supremacy_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(naft_tr_t0, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(naft_tr_t5, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 5, 0.13).
narrative_ontology:measurement(naft_tr_t10, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 10, 0.16).
narrative_ontology:measurement(naft_tr_t15, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 15, 0.2).
narrative_ontology:measurement(naft_tr_t20, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 20, 0.23).
narrative_ontology:measurement(naft_tr_t25, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 25, 0.26).
narrative_ontology:measurement(naft_tr_t30, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 30, 0.28).

% Extraction over time
narrative_ontology:measurement(naft_be_t0, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(naft_be_t5, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(naft_be_t10, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(naft_be_t15, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 15, 0.68).
narrative_ontology:measurement(naft_be_t20, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 20, 0.73).
narrative_ontology:measurement(naft_be_t25, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 25, 0.77).
narrative_ontology:measurement(naft_be_t30, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 30, 0.79).

% Suppression requirement over time
narrative_ontology:measurement(naft_su_t0, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(naft_su_t5, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 5, 0.52).
narrative_ontology:measurement(naft_su_t10, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(naft_su_t15, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 15, 0.63).
narrative_ontology:measurement(naft_su_t20, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 20, 0.67).
narrative_ontology:measurement(naft_su_t25, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement(naft_su_t30, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 30, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nafta_jurisdictional_boundary__capital_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__capital_supremacy_reading, nafta_jurisdictional_boundary__embedded_liberalism_reading).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__capital_supremacy_reading, nafta_jurisdictional_boundary__sovereignty_primacy_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the nafta_jurisdictional_boundary kernel. capital_supremacy_reading (this file) authors high extractiveness (0.79) and classifies as tangled_rope. embedded_liberalism_reading authors substantially lower extractiveness reflecting genuine policy-space preservation and would classify closer to rope. sovereignty_primacy_reading authors near-mountain-low extractiveness reflecting the claim that domestic law retains full authority, and would likely classify as rope or show minimal victim structure. All three share the treaty text as referent but differ in ε, beneficiary/victim declarations, and computed type, per the ε-invariance principle — they are not the same constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
