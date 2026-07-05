% ============================================================================
% CONSTRAINT STORY: nafta_jurisdictional_boundary__capital_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: nafta_jurisdictional_boundary__capital_supremacy_reading
 *   human_readable: Trade Agreement Text as Supreme Law: Capital Mobility Supremacy Reading
 *   domain: international_trade_law/political_economy/regulatory_federalism
 *
 * SUMMARY:
 *   This story instantiates the capital-supremacy reading of the
 *   NAFTA/USMCA-style jurisdictional boundary kernel: trade agreement text is
 *   read as supreme law that binds domestic regulators, with mandatory
 *   capital mobility and regulatory harmonization obligations that override
 *   domestic environmental, labor, and health standards whenever they
 *   diminish expected investor returns. Under this reading, ISDS functions as
 *   a parallel supra-domestic legal order that domestic courts cannot review
 *   and domestic legislatures cannot unilaterally revise without incurring
 *   compensable liability. This is a distinct constraint from the
 *   embedded_liberalism_reading (which reads the same text as balancing
 *   market access against legitimate domestic policy space) and the
 *   sovereignty_primacy_reading (which reads domestic law as retaining full
 *   regulatory authority). The three readings are not three measurements of
 *   one constraint — they are three different constraints instantiated by
 *   different interpretive communities holding the same text, each with its
 *   own ε, beneficiary/victim structure, and persistence dynamics. Only this
 *   reading's structure is analyzed here.
 *
 * KEY AGENTS:
 *   - multinational_investors: primary beneficiary (institutional/arbitrage) — extracts compensation via ISDS when domestic regulation reduces returns
 *   - domestic_environmental_regulators and domestic_labor_standards_agencies: primary targets (institutional/constrained) — bear regulatory chill and treasury liability
 *   - investor_state_arbitration_bar: agenda-setting beneficiary (organized/arbitrage) — administers and profits from the interpretive machinery
 *   - affected_local_communities and unionized_manufacturing_workers: diffuse victims (powerless-moderate/trapped-constrained) — bear the downstream cost of regulatory forbearance without standing in the forum that causes it
 *   - trade_law_scholars: analytical observer — documents doctrinal drift toward capital protection
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nafta_jurisdictional_boundary__capital_supremacy_reading, 0.78).
domain_priors:suppression_score(nafta_jurisdictional_boundary__capital_supremacy_reading, 0.72).
domain_priors:theater_ratio(nafta_jurisdictional_boundary__capital_supremacy_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nafta_jurisdictional_boundary__capital_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(nafta_jurisdictional_boundary__capital_supremacy_reading, "Trade Agreement Text as Supreme Law: Capital Mobility Supremacy Reading").
narrative_ontology:topic_domain(nafta_jurisdictional_boundary__capital_supremacy_reading, "international_trade_law/political_economy/regulatory_federalism").

domain_priors:requires_active_enforcement(nafta_jurisdictional_boundary__capital_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nafta_jurisdictional_boundary__capital_supremacy_reading, 'bdc61193-64cd-4332-8678-9f6e670c2fdf').
narrative_ontology:cs_kernel_codification('bdc61193-64cd-4332-8678-9f6e670c2fdf', fixed_text).
narrative_ontology:cs_authority_grounding('bdc61193-64cd-4332-8678-9f6e670c2fdf', extraction).
narrative_ontology:cs_interpretation_layer_present('bdc61193-64cd-4332-8678-9f6e670c2fdf').
narrative_ontology:cs_reading_relation('bdc61193-64cd-4332-8678-9f6e670c2fdf', nafta_jurisdictional_boundary__embedded_liberalism_reading, influences).
narrative_ontology:cs_reading_relation('bdc61193-64cd-4332-8678-9f6e670c2fdf', nafta_jurisdictional_boundary__sovereignty_primacy_reading, forecloses).
narrative_ontology:cs_axiom('bdc61193-64cd-4332-8678-9f6e670c2fdf', foundational, treaty_text_self_executing_supreme_over_domestic_statute).
narrative_ontology:cs_axiom_status(treaty_text_self_executing_supreme_over_domestic_statute, holdable).
narrative_ontology:cs_axiom_grounding('bdc61193-64cd-4332-8678-9f6e670c2fdf', treaty_text_self_executing_supreme_over_domestic_statute, conventional).
narrative_ontology:cs_axiom('bdc61193-64cd-4332-8678-9f6e670c2fdf', foundational, capital_mobility_guarantee_overrides_precautionary_regulation).
narrative_ontology:cs_axiom_status(capital_mobility_guarantee_overrides_precautionary_regulation, holdable).
narrative_ontology:cs_axiom_grounding('bdc61193-64cd-4332-8678-9f6e670c2fdf', capital_mobility_guarantee_overrides_precautionary_regulation, instrumental).
narrative_ontology:cs_reference_frame('bdc61193-64cd-4332-8678-9f6e670c2fdf', gatt_era_negotiated_reciprocity_framework).
narrative_ontology:cs_drift_state('bdc61193-64cd-4332-8678-9f6e670c2fdf', post_isds_award_expansion_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('bdc61193-64cd-4332-8678-9f6e670c2fdf', '').
narrative_ontology:cs_kernel_id(nafta_jurisdictional_boundary__capital_supremacy_reading, nafta_jurisdictional_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__capital_supremacy_reading, multinational_investors).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__capital_supremacy_reading, cross_border_capital_holders).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__capital_supremacy_reading, investor_state_arbitration_bar).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, domestic_environmental_regulators).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, domestic_labor_standards_agencies).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, affected_local_communities).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, unionized_manufacturing_workers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Structure cross-border operations to invoke investor-state dispute settlement (ISDS) whenever a domestic regulation reduces expected profit. Treaty text is read as binding supreme law that domestic legislatures cannot unilaterally override without triggering compensable claims. Can relocate capital or threaten relocation to extract regulatory concessions from any single jurisdiction.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, multinational_investors, beneficiary,
    institutional, generational, arbitrage, continental).

% Benefit from mandatory capital mobility provisions that prevent any signatory from imposing capital controls, transfer restrictions, or performance requirements. Move liquid capital across borders to arbitrage differences in regulatory stringency, effectively voting with capital flows to punish jurisdictions that regulate more strictly.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, cross_border_capital_holders, beneficiary,
    organized, generational, arbitrage, continental).

% A small professional class of arbitrators, counsel, and expert witnesses who staff ISDS tribunals, draw fees from every dispute, and shape doctrine through repeated appointments across cases. Their income depends on the treaty text being read as supreme, self-executing law that displaces domestic judicial review; they administer the interpretive machinery that keeps that reading dominant.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, investor_state_arbitration_bar, beneficiary,
    organized, biographical, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(nafta_jurisdictional_boundary__capital_supremacy_reading, investor_state_arbitration_bar, agenda_setter).

% Draft and attempt to enforce pollution, land-use, and resource-extraction standards, but face the credible threat that any measure found to indirectly expropriate investor value can trigger an ISDS claim for damages payable from the public treasury. Regulatory agencies increasingly pre-clear rules against anticipated investor claims, chilling enforcement before it happens.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, domestic_environmental_regulators, payer,
    institutional, biographical, constrained, national).

% Set minimum wage, workplace safety, and organizing-rights rules, but capital mobility provisions let firms credibly threaten relocation to lower-standard jurisdictions within the trade bloc whenever enforcement tightens. Agencies routinely trade enforcement intensity against employment-flight risk, converting labor standards into negotiable variables rather than binding floors.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, domestic_labor_standards_agencies, payer,
    institutional, biographical, constrained, national).

% Live adjacent to facilities whose environmental or safety practices are shielded from stricter domestic regulation by the treaty's supremacy claim and the threat of investor claims. Have no standing before ISDS tribunals themselves and cannot exit the jurisdiction or the exposure; their only recourse runs through domestic political processes that the treaty text is read to override.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, affected_local_communities, payer,
    powerless, biographical, trapped, local).

% Bargain for wages and job security under continuous pressure from firms citing treaty-protected capital mobility as a relocation option. Organizing gains are structurally capped by the credible exit threat baked into the treaty's capital mobility guarantees; collective action cannot reach capital that has a treaty-secured right to leave.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, unionized_manufacturing_workers, payer,
    moderate, biographical, constrained, national).

% Negotiated and ratified the treaty text, and now administer compliance, defend the state in ISDS proceedings, and interpret the supremacy clause in disputes with domestic regulators. Their institutional incentive is to preserve treaty credibility (to keep capital inflows and avoid arbitration losses) even when that means constraining sister agencies' regulatory authority.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, national_trade_ministries, agenda_setter,
    institutional, generational, constrained, continental).

% Under the capital supremacy reading, domestic judicial review of regulatory takings claims is effectively bypassed by parallel investor-state arbitration that need not defer to, or even be reviewed by, national courts. Judges who might otherwise balance investor interests against domestic constitutional or statutory protections have no seat at the ISDS table.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, domestic_courts, excluded,
    institutional, generational, constrained, national).

% Study the doctrinal evolution of ISDS awards and the expansion of 'indirect expropriation' and 'fair and equitable treatment' standards. Document the pattern by which arbitral interpretation has steadily favored capital claims over regulatory autonomy, without themselves being party to any dispute.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, trade_law_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, predictable cross-border investment climate by binding all signatories to a common floor of investor protection, reducing the risk that any single government will expropriate or discriminate against foreign capital after it has sunk costs into a jurisdiction.
% TRANSFER_FUNCTION: Moves regulatory discretion and enforcement latitude away from domestic environmental, labor, and health agencies and toward capital holders and the arbitration apparatus that adjudicates their claims; moves compensation from public treasuries to investors whenever domestic regulation is found to have reduced expected returns.
% ABSENT_VOICES: Affected local communities, displaced workers, and the domestic courts that would otherwise review regulatory takings claims have no standing in investor-state arbitration; they would argue that democratic regulatory authority should not be conditioned on compensating private capital for the ordinary risk of policy change, but the forum that resolves these disputes was not built to hear them.
% DISAPPEARANCE_RATIONALE: If the capital-supremacy reading of the treaty text were abandoned overnight — if domestic regulatory authority were restored as the default and ISDS awards no longer bound public treasuries — environmental and labor agencies would resume enforcement they currently forbear from, capital would face genuine jurisdictional variance again, and the arbitration bar's caseload and fee base would collapse. The rearrangement would be immediate and structural, not marginal.
% FOUNDING_PROBLEM: Cross-border investors in the late 1980s and early 1990s faced real expropriation risk in trading-partner jurisdictions with weaker property-rights enforcement and volatile domestic politics; the treaty framework was built to give capital a credible, depoliticized forum for redress so that investment would flow across the new trade bloc.
% FOUNDING_PROBLEM_CORROBORATION: National trade ministries and the arbitration bar attest the expropriation-risk problem remains live and justifies the current scope of investor protection. Independent legal scholarship (particularly comparative studies of ISDS award expansion into 'regulatory chill' territory) and testimony from domestic regulatory agencies in legislative review hearings — sources outside the beneficiary set — attest that the original expropriation-risk problem has been substantially resolved by domestic legal modernization, and that the arbitration apparatus now principally functions to insure capital against ordinary democratic policy change rather than against genuine expropriation.
narrative_ontology:disappearance_verdict(nafta_jurisdictional_boundary__capital_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(nafta_jurisdictional_boundary__capital_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nafta_jurisdictional_boundary__capital_supremacy_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(nafta_jurisdictional_boundary__capital_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nafta_jurisdictional_boundary__capital_supremacy_reading, 0.78, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness rises from 0.42 at treaty entry-into-force to 0.78 by 2024 as arbitral doctrine on 'indirect expropriation' and 'fair and equitable treatment' expanded well beyond the founding text's plain language, capturing an increasing share of ordinary regulatory activity as compensable. Suppression tracks closely (0.38 to 0.72) because the mechanism's persistence depends on regulatory agencies' internalized anticipation of arbitral liability — agencies increasingly self-censor before any claim is filed, which is a suppression effect distinct from and larger than the raw claim count. Theater ratio is modest but rising (0.12 to 0.28): most of the machinery does real adjudicative work, but a growing share of tribunal activity performs legal neutrality while functionally extending investor protections beyond what domestic constitutional review would permit for comparable takings claims.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setting trade ministry seat, the treaty functions as credible-commitment coordination that keeps capital inflows stable. From the payer seats (regulatory agencies, communities, workers), the identical structure operates as an enforced upward transfer that forecloses policy options without their participation. The engine should compute these as structurally different experiences of the same constraint text, which is the seat divergence this tangled_rope classification is built to register.
 *
 * DIRECTIONALITY LOGIC:
 *   Multinational investors, capital holders, and the arbitration bar sit at the beneficiary end: low structural cost, high structural benefit, and arbitrage-grade exit (capital and counsel can relocate; regulatory targets cannot). Domestic regulatory agencies and affected communities sit at the target end: constrained-to-trapped exit, high realized cost, no standing in the forum that adjudicates the harm. National trade ministries occupy an intermediate agenda-setting position — they authored the framework and administer it, but are institutionally bound to defend treaty credibility even against their own sister agencies' regulatory interests, which is why they are coded as agenda_setter rather than simple beneficiary.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (genuine 1990s-era expropriation risk in weak-property-rights jurisdictions) is genuinely contested as live vs. dead: capital mobility protection was a real coordination good when signed, but the tangled_rope classification captures that the coordination function (protecting sunk investment from arbitrary state action) now coexists with, and is substantially outweighed by, an extraction function (insuring capital against ordinary democratic policy change). Classifying this as pure snare would erase the genuine investment-security coordination that justified the original bargain; classifying it as pure rope would erase the asymmetric, enforced cost now borne by regulatory agencies and communities with no standing in the forum. Tangled rope holds both facts simultaneously, which is the point of the classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_capital_supremacy,
    'Is the capital-supremacy reading of the treaty text (supreme law overriding domestic standards) the structurally correct reading, or is it one contested interpretation among the embedded_liberalism_reading and sovereignty_primacy_reading, each held by different institutional actors?',
    'This is not resolvable by further data within a single reading — it is the committer-axis question itself. Evidence: which reading''s practical consequences actually obtain (ISDS award patterns, domestic court deference patterns, legislative override attempts and their success rate) would show which reading has become operative in practice, but would not settle which reading is textually or normatively correct.',
    'If the sovereignty_primacy_reading is operative in a given jurisdiction (domestic courts successfully assert review authority, legislatures successfully override treaty-derived rulings), this constraint''s extraction and suppression values would not apply there — a different constraint (the sovereignty_primacy_reading story) would govern. The three readings are mutually exclusive AS OPERATIVE LAW in any single jurisdiction at any single time, even though all three exist as live argued positions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_capital_supremacy, conceptual, 'Which kernel reading is operative is itself contested and not resolvable by evidence internal to this reading.').

omega_variable(
    expropriation_risk_still_live,
    'Is the founding expropriation-risk problem (weak property rights enforcement in trading-partner jurisdictions circa 1990s) still live enough in any current signatory relationship to justify the current scope of investor protection, or has domestic legal modernization made the mechanism obsolete cover for ordinary regulatory insurance?',
    'Comparative empirical study of expropriation and discriminatory-treatment incident rates in signatory jurisdictions pre- and post-treaty, cross-referenced against ISDS claim subject matter (genuine expropriation vs. ordinary regulatory change) over the interval.',
    'If the founding problem is dead in practice, the founding_problem_status=dead + disappearance_verdict=world_rearranges combination signals a capture/zombie pattern: an arrangement whose stated justification no longer applies but whose institutional machinery (and beneficiary class) persists and has grown.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(expropriation_risk_still_live, empirical, 'Whether the founding expropriation-risk justification remains empirically live.').

omega_variable(
    regulatory_chill_measurement_uncertainty,
    'How much of the suppression effect (agencies declining to regulate) is directly caused by anticipated ISDS liability versus other factors (industry lobbying, budget constraints, genuine policy disagreement)?',
    'Process tracing of specific abandoned or weakened regulatory proposals, comparing agency internal deliberation records where available against counterfactual jurisdictions without equivalent treaty exposure.',
    'If regulatory chill is substantially attributable to non-treaty factors, the suppression metric authored here overstates the treaty mechanism''s causal contribution and should be revised downward; if chill is substantially treaty-attributable, the current suppression trajectory (0.38 to 0.72) is conservative.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_chill_measurement_uncertainty, empirical, 'Causal attribution uncertainty in the regulatory chill mechanism underlying the suppression metric.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nafta_jurisdictional_boundary__capital_supremacy_reading, 1994, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(naft_tr_t1994, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 1994, 0.12).
narrative_ontology:measurement(naft_tr_t2000, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(naft_tr_t2006, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 2006, 0.19).
narrative_ontology:measurement(naft_tr_t2012, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 2012, 0.22).
narrative_ontology:measurement(naft_tr_t2018, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 2018, 0.25).
narrative_ontology:measurement(naft_tr_t2024, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(naft_be_t1994, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 1994, 0.42).
narrative_ontology:measurement(naft_be_t2000, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 2000, 0.51).
narrative_ontology:measurement(naft_be_t2006, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 2006, 0.6).
narrative_ontology:measurement(naft_be_t2012, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 2012, 0.68).
narrative_ontology:measurement(naft_be_t2018, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 2018, 0.74).
narrative_ontology:measurement(naft_be_t2024, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(naft_su_t1994, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 1994, 0.38).
narrative_ontology:measurement(naft_su_t2000, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 2000, 0.48).
narrative_ontology:measurement(naft_su_t2006, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 2006, 0.56).
narrative_ontology:measurement(naft_su_t2012, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 2012, 0.63).
narrative_ontology:measurement(naft_su_t2018, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 2018, 0.69).
narrative_ontology:measurement(naft_su_t2024, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nafta_jurisdictional_boundary__capital_supremacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(nafta_jurisdictional_boundary__capital_supremacy_reading, 0.1).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__capital_supremacy_reading, nafta_jurisdictional_boundary__embedded_liberalism_reading).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__capital_supremacy_reading, nafta_jurisdictional_boundary__sovereignty_primacy_reading).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__capital_supremacy_reading, isds_arbitration_forum_selection).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the nafta_jurisdictional_boundary kernel, each authored as a separate constraint story per the ε-invariance principle: capital_supremacy_reading (this file, tangled_rope, high extraction), embedded_liberalism_reading (balanced market-access framing, expected lower extraction and rope-adjacent classification), and sovereignty_primacy_reading (domestic-law-retains-authority framing, expected minimal extraction, rope or mountain-adjacent). All three read the identical treaty text; they diverge on which institutional actor's interpretation of that text is treated as operative. Network edges connect all three siblings plus the downstream isds_arbitration_forum_selection constraint, which this reading's extraction substantially depends on structurally.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
