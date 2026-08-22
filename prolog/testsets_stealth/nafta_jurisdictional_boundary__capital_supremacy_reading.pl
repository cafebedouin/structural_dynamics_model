% ============================================================================
% CONSTRAINT STORY: nafta_jurisdictional_boundary__capital_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
 *   human_readable: NAFTA Jurisdictional Boundary — Capital Supremacy Reading
 *   domain: economic/political/legal_international
 *
 * SUMMARY:
 *   This story authors the capital-supremacy instantiation of the North
 *   American trade treaty's jurisdictional boundary: the treaty text operates
 *   as supreme law that domestic regulatory standards must yield to, and
 *   capital mobility plus regulatory harmonization stand as mandatory treaty
 *   obligations rather than bargained concessions. On this reading the
 *   arrangement's operative content is defined by investor-state dispute
 *   practice: when a domestic labor, environmental, or health measure raises
 *   the cost of a covered investment, the measure can be challenged as a
 *   compensable interference, and the tribunal's award — not the domestic
 *   statute — marks the effective limit of the regulator's authority.
 *   Extraction flows upward: compensation awards, secured market access, and
 *   a fee-generating dispute industry accrue to mobile capital and its
 *   professional infrastructure, while the costs — treasury payments, chilled
 *   rulemaking, narrowed standards — land on domestic publics and on the
 *   agencies that serve them. The epsilon referent is the standing
 *   treaty-as-supreme-law arrangement, assessed by this reading's own lights;
 *   the reading's endorsed alternative appears nowhere in this file. KEY
 *   AGENTS (by structural relationship): - multinational_investors: Primary
 *   beneficiary (powerful/arbitrage) — collects compensation and
 *   market-access rents; capital relocates freely -
 *   international_arbitration_bar: Secondary beneficiary (organized/mobile) —
 *   collects fees from every dispute regardless of outcome -
 *   investor_state_tribunals: Agenda-setter (institutional/constrained) —
 *   adjudicates and thereby defines the operative boundary -
 *   national_trade_ministries: Dual-positioned administrator
 *   (institutional/constrained) — administers and defends the arrangement
 *   while bearing its defense costs - environmental_regulatory_agencies:
 *   Primary target (institutional/trapped) — statutory authority intact,
 *   exercise of it priced - unionized_domestic_workforce: Primary target
 *   (organized/trapped) — standards overridable, no standing in disputes -
 *   subnational_governments: Target (moderate/trapped) — bound without
 *   consent, no amendment recourse - domestic_consumers: Incidental
 *   beneficiary (moderate/mobile) — diffuse price benefits, diffuse indirect
 *   costs - civil_society_trade_movements: Excluded voice
 *   (organized/constrained) — outside negotiation and dispute standing -
 *   trade_law_analysts: Analytical observer (analytical/analytical) — sees
 *   the full structure, holds no decision rights
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nafta_jurisdictional_boundary__capital_supremacy_reading, 0.72).
domain_priors:suppression_score(nafta_jurisdictional_boundary__capital_supremacy_reading, 0.78).
domain_priors:theater_ratio(nafta_jurisdictional_boundary__capital_supremacy_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__capital_supremacy_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nafta_jurisdictional_boundary__capital_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(nafta_jurisdictional_boundary__capital_supremacy_reading, "NAFTA Jurisdictional Boundary — Capital Supremacy Reading").
narrative_ontology:topic_domain(nafta_jurisdictional_boundary__capital_supremacy_reading, "economic/political/legal_international").

domain_priors:requires_active_enforcement(nafta_jurisdictional_boundary__capital_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nafta_jurisdictional_boundary__capital_supremacy_reading, '973e86f5-899a-478f-95a5-a1df404fd05c').
narrative_ontology:cs_kernel_codification('973e86f5-899a-478f-95a5-a1df404fd05c', fixed_text).
narrative_ontology:cs_authority_grounding('973e86f5-899a-478f-95a5-a1df404fd05c', extraction).
narrative_ontology:cs_interpretation_layer_present('973e86f5-899a-478f-95a5-a1df404fd05c').
narrative_ontology:cs_reading_relation('973e86f5-899a-478f-95a5-a1df404fd05c', nafta_jurisdictional_boundary__embedded_liberalism_reading, coexists_with).
narrative_ontology:cs_reading_relation('973e86f5-899a-478f-95a5-a1df404fd05c', nafta_jurisdictional_boundary__sovereignty_primacy_reading, forecloses).
narrative_ontology:cs_axiom('973e86f5-899a-478f-95a5-a1df404fd05c', foundational, treaty_supremacy_over_domestic_standards).
narrative_ontology:cs_axiom_status(treaty_supremacy_over_domestic_standards, holdable).
narrative_ontology:cs_axiom_grounding('973e86f5-899a-478f-95a5-a1df404fd05c', treaty_supremacy_over_domestic_standards, conventional).
narrative_ontology:cs_axiom('973e86f5-899a-478f-95a5-a1df404fd05c', foundational, capital_mobility_mandatory_treaty_obligation).
narrative_ontology:cs_axiom_status(capital_mobility_mandatory_treaty_obligation, holdable).
narrative_ontology:cs_axiom_grounding('973e86f5-899a-478f-95a5-a1df404fd05c', capital_mobility_mandatory_treaty_obligation, instrumental).
narrative_ontology:cs_axiom('973e86f5-899a-478f-95a5-a1df404fd05c', secondary, regulatory_diminution_compensable).
narrative_ontology:cs_axiom_status(regulatory_diminution_compensable, holdable).
narrative_ontology:cs_axiom_grounding('973e86f5-899a-478f-95a5-a1df404fd05c', regulatory_diminution_compensable, empirically_contingent).
narrative_ontology:cs_reference_frame('973e86f5-899a-478f-95a5-a1df404fd05c', treaty_text_supremacy_baseline).
narrative_ontology:cs_drift_state('973e86f5-899a-478f-95a5-a1df404fd05c', post_isds_backlash_contemporary, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('973e86f5-899a-478f-95a5-a1df404fd05c', '').
narrative_ontology:cs_kernel_id(nafta_jurisdictional_boundary__capital_supremacy_reading, nafta_jurisdictional_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__capital_supremacy_reading, multinational_investors).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__capital_supremacy_reading, international_arbitration_bar).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__capital_supremacy_reading, domestic_consumers).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, unionized_domestic_workforce).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, environmental_regulatory_agencies).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, subnational_governments).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__capital_supremacy_reading, national_trade_ministries).
narrative_ontology:constraint_vindicates(nafta_jurisdictional_boundary__capital_supremacy_reading, investor_state_arbitration_doctrine).
narrative_ontology:constraint_vindicates(nafta_jurisdictional_boundary__capital_supremacy_reading, indirect_expropriation_doctrine).
narrative_ontology:constraint_vindicates(nafta_jurisdictional_boundary__capital_supremacy_reading, fair_equitable_treatment_expansion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own or operate covered investments across the three countries. Can bring claims against host governments when new regulation reduces expected returns, collecting compensation from public treasuries when claims succeed. Capital crosses borders freely, so they can locate, threaten relocation, or expand wherever the post-regulation environment suits them best.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, multinational_investors, beneficiary,
    powerful, biographical, arbitrage, global).

% Law firms and arbitrators who staff and argue investor-state proceedings. Collect fees and appointment income from every dispute; their professional market expands with each novel theory of compensable harm the tribunals accept. Income depends on the continuation of the claims process, not on any particular outcome.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, international_arbitration_bar, beneficiary,
    organized, biographical, mobile, global).

% Buy imported goods at prices lowered by tariff elimination. Bear diffuse indirect costs when public funds pay tribunal awards or when regulation is narrowed. Individual stakes are small and dispersed; they neither organize around the treaty nor follow its disputes.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, domestic_consumers, beneficiary,
    moderate, immediate, mobile, national).

% Three-member ad hoc panels constituted per dispute under the treaty's investment chapter. Decide whether host-state measures breach investment obligations and set compensation levels. Their case law progressively defines what counts as a compensable interference, which is the operative meaning of the treaty's boundary line. They exist only while claims continue to be brought.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, investor_state_tribunals, agenda_setter,
    institutional, biographical, constrained, continental).

% Negotiated, signed, and administer the agreement; coordinate responses to disputes and decide whether to settle or litigate claims against domestic measures. When tribunals rule against domestic regulation they implement or resist the ruling, and they spend treasury funds defending claims. They cannot amend the treaty unilaterally — change requires all three parties' consent.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, national_trade_ministries, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(nafta_jurisdictional_boundary__capital_supremacy_reading, national_trade_ministries, payer).

% Draft and enforce environmental rules within their statutory mandates. When a measure draws an investor claim they must defend the rule's science and necessity before a tribunal, and agency practice shifts toward pre-clearing decisions against claim risk. Their formal authority remains intact while its exercise carries a price they did not set.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, environmental_regulatory_agencies, payer,
    institutional, generational, trapped, national).

% Work in sectors exposed to relocation and import competition. Labor standards — organizing rights, safety rules, wage floors — are domestic law that treaty obligations can override when they raise investor costs. Unions had no standing in investment disputes and no seat in negotiation; their leverage is political rather than juridical, and it aggregates only through electoral and renegotiation channels.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, unionized_domestic_workforce, payer,
    organized, biographical, trapped, national).

% States and provinces legislate land use, water, and local environmental rules. They are bound by the federal treaty obligation without having consented to it, and their measures have been the subject of investor claims. They cannot withdraw from or amend the treaty; their recourse is lobbying their own national government.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, subnational_governments, payer,
    moderate, generational, trapped, regional).

% Coalitions of labor, environmental, and consumer organizations that mobilized against the agreement's expansion and its dispute system. Excluded from the negotiation rooms and lacking standing in disputes, they operate through protest, electoral pressure, and model-text advocacy during renegotiations.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, civil_society_trade_movements, excluded,
    organized, biographical, constrained, continental).

% Academic and think-tank specialists in international economic law. Track dispute outcomes, interpretive trends, and comparative treaty design; publish assessments of how the investment chapter's operation compares with its stated aims. Hold no decision rights over the arrangement.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__capital_supremacy_reading, trade_law_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nafta_jurisdictional_boundary__capital_supremacy_reading, multinational_investors).
narrative_ontology:fixing_cost_class(nafta_jurisdictional_boundary__capital_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates market access across three sovereign jurisdictions: eliminates tariffs on a fixed schedule, harmonizes rules of origin and customs procedure, substitutes neutral dispute settlement for unilateral retaliation, and guarantees investment conditions against host-state expropriation.
% TRANSFER_FUNCTION: Moves regulatory authority from domestic legislatures and agencies to treaty obligations adjudicated by tribunals; moves compensation payments and secured market-access rents toward mobile capital holders; moves litigation and defense costs onto public treasuries; moves price reductions to consumers.
% ABSENT_VOICES: Labor unions, environmental organizations, and subnational governments were largely outside the negotiation and ratification rooms — closed drafting, expedited approval procedures, no dispute standing. They would contest the breadth of the supremacy reading and the exclusion of labor and environmental claims from the dispute system; their objections surface only post hoc, as protest and renegotiation pressure.
% DISAPPEARANCE_RATIONALE: Tariff schedules, rules-of-origin supply chains, and investment-protection expectations would unravel; tribunals would lose jurisdiction overnight and pending claims would collapse; domestic agencies would regain unencumbered regulatory authority; cross-border production networks built on the treaty's guarantees would reprice or relocate; the arbitration industry built on the claims process would lose its subject matter.
% FOUNDING_PROBLEM: Built to solve tariff escalation and retaliation cycles among the three economies, investor fear of host-state expropriation without recourse, and the transaction costs of three divergent regulatory regimes blocking integrated production.
% FOUNDING_PROBLEM_CORROBORATION: Empirical trade literature from outside the beneficiary set attests that tariff elimination succeeded and stands as the agreement's settled achievement. Labor economics and environmental law scholarship attests that the regulatory-supremacy extension was never required for tariff coordination and now constrains standards the founding problem did not mention. No party outside capital-side beneficiaries attests that mandatory capital-mobility obligations remain necessary to the original problem; the necessity claim is made only by the arrangement's operators and beneficiaries.
narrative_ontology:disappearance_verdict(nafta_jurisdictional_boundary__capital_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(nafta_jurisdictional_boundary__capital_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nafta_jurisdictional_boundary__capital_supremacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(nafta_jurisdictional_boundary__capital_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nafta_jurisdictional_boundary__capital_supremacy_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

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
 *   Epsilon is authored at 0.72 because the arrangement's dominant transfer is decoupled from service provision: compensation for lawful, non-discriminatory regulation, public defense costs, and rulemaking deterred before proposal are extraction in the direct sense, layered on top of a tariff-coordination core that still functions. Suppression is higher (0.78) because persistence depends on active machinery — tribunals that must be constituted, awards that must be enforced, rival forums (labor and environmental standing) that are structurally excluded — not on participant preference. Theater ratio (0.38) reflects the growing share of interpretive activity spent on balancing language ('proportionality,' 'legitimate regulatory objective') whose outcome distribution favors claimants: real work, increasingly performed alongside ritual justification. Accessibility collapse is moderate (0.48): the alternatives — regulate anyway and absorb the claim risk, or withdraw and absorb retaliation — remain visible and lawful, only priced. Resistance is substantial (0.62): mass protest, electoral backlash, legislative carve-out demands, and eventual renegotiation all met the arrangement and moved it. The temporal series run on one shared grid (t=0..26, years since entry into force, 1994–2020): extractiveness climbs as tribunal doctrine expands compensable interference (early environmental-measure claims, the indirect-expropriation and fair-equitable-treatment expansions, peak late-2010s caseload); suppression_requirement rises on the same grid because this story specifically tracks an enforcement ratchet — more claims, broader doctrines, deeper documented chill — not a static enforcement picture; theater rises as balancing language accumulates faster than it constrains outcomes. Claim/metric independence is preserved: the claimed type is authored from the structure (a genuine coordination core plus asymmetric extraction requiring enforcement), the metrics from the observed operation, and neither was tuned toward the other or toward a predicted engine output.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary/administrator seats should compute different types from identical structural data. From the investor and tribunal positions the arrangement is a rights-protecting order: predictable investment conditions, neutral adjudication, compensation for state interference — the coordination story is the lived experience. From the regulator, worker, and subnational-government positions the same text operates as a jurisdictional override: their mandates are intact on paper and priced in practice, their standards yield to awards they cannot appeal, and their only exits (withdraw, retaliate, amend) require unanimity they do not control. The coalition question matters for the weakest seats: the workforce's leverage is political rather than juridical — it materializes only when aggregated into electoral or renegotiation pressure, which is exactly the channel through which the observed resistance score was produced. The engine computes this divergence per seat; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to low directionality: multinational_investors sit nearest the subsidy end (compensation rights convert regulation into a payable event; arbitrage-grade exit lets them locate around adverse environments), the arbitration_bar collects from the machinery's operation itself, and domestic_consumers hold diffuse incidental benefit. Victim declarations map to high directionality: environmental_regulatory_agencies and the unionized_domestic_workforce are trapped (their mandate and livelihood are territorial; no exit exists from the jurisdiction whose rules are overridden), and subnational_governments are bound by an obligation they never consented to. national_trade_ministries are deliberately dual-positioned: they administer and defend the arrangement (agenda-setter) while bearing its defense costs and losing regulatory discretion (payer), placing them near symmetric rather than at either pole. No directionality overrides are authored: the beneficiary/victim declarations plus exit options already differentiate the seats correctly, and the available override granularity (per power atom) would smear across institutional seats that genuinely diverge — the tribunals and the environmental agencies share a power atom but sit at opposite ends of the directionality range, which is precisely what the structural derivation captures and a per-atom override would destroy.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — tariff escalation, retaliation cycles, investor fear of uncompensated expropriation — is substantially solved by the arrangement's own operation, and the corroboration for that comes from outside the beneficiary set. Yet the arrangement has extended past the solved problem into domains the founding problem never mentioned: labor standards, environmental regulation, public health measures. That mismatch (founding problem dead-or-contested, world still rearranges around the arrangement) is the capture/zombie signature the R5 interview exists to surface, and it is why the status is authored 'contested' rather than 'live': the tariff half is dead, the supremacy extension's necessity is the live dispute. The hybrid classification prevents mislabeling in both directions: calling the arrangement pure extraction erases the tariff coordination that demonstrably still works and that consumers and exporters still collect; calling it pure coordination hides the fact that its enforcement machinery now principally defends the extension, not the core. Keeping both visible is what makes the drift measurable — the theater and extractiveness series show the center of gravity migrating from the coordination core toward the extraction layer over the interval.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'Which reading of the nafta_jurisdictional_boundary kernel does the operative dispute-settlement practice actually realize — this capital-supremacy reading, or one of its sibling readings?',
    'Code the population of concluded investment-treaty awards and settlements for outcome distribution and interpretive doctrine. If outcomes converge on deference to non-discriminatory domestic measures, practice tracks the embedded-liberalism sibling; if domestic law is treated as controlling over the treaty text, practice tracks the sovereignty sibling.',
    'If practice realizes a sibling reading, this story''s victim set and epsilon misdescribe the operative constraint: embedded-liberalism practice shrinks the victim set and lowers epsilon toward moderate; sovereignty practice converts the arrangement into ordinary coordination with minimal extraction. This story''s classification is conditional on the capital-supremacy reading being the operative one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Kernel-level framing under-determination across the three declared readings of the treaty-boundary kernel.').

omega_variable(
    regulatory_chill_attribution,
    'How much observed narrowing or abandonment of domestic regulation is causally attributable to treaty exposure rather than to ordinary political economy?',
    'Difference-in-differences across regulated sectors matched on political salience, comparing jurisdictions inside and outside the treaty''s investment chapter; exploit natural experiments where claims failed on jurisdictional grounds and the underlying regulation survived.',
    'If chill attribution is small, the measured suppression overstates the arrangement''s coercive force and epsilon falls; if large, suppression is understated and the enforcement-ratchet trajectory steepens further.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_chill_attribution, empirical, 'Causal share of regulatory chill attributable to the treaty mechanism versus baseline politics.').

omega_variable(
    supremacy_coordination_separability,
    'Is the extraction-bearing supremacy machinery separable from the tariff-and-market-access coordination the treaty simultaneously performs?',
    'Compare trade and investment flows under agreement chapters with and without investor-state mechanisms across comparable treaties; observe the successor agreement''s removal of investor-state access between two of the three parties as a partial severance test.',
    'If separable, the supremacy layer is extraction riding on genuine coordination and the hybrid classification hardens toward its extractive pole; if inseparable, part of the measured epsilon is the unavoidable price of the coordination itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(supremacy_coordination_separability, empirical, 'Whether the coordination function and the extraction function are structurally separable components.').

omega_variable(
    jurisprudential_drift_reversibility,
    'Is the tribunal jurisprudence that defines the supremacy boundary reversible by party control (joint interpretive notes, negotiated carve-outs), or has accumulated precedent locked the boundary beyond party reach?',
    'Track whether the 2001 joint interpretive clarification and renegotiation-era carve-outs changed award outcomes in subsequent claims, or merely redistributed claim filing.',
    'If reversible, the arrangement''s trajectory remains politically contingent and the repudiation-pressure drift can bend back toward the reference frame; if locked, drift is irreversible and persistence no longer depends on any beneficiary actively maintaining it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(jurisprudential_drift_reversibility, empirical, 'Reversibility of the interpretive drift that constitutes the operative boundary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nafta_jurisdictional_boundary__capital_supremacy_reading, 0, 26).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(naft_tr_t0, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(naft_tr_t0, observed).
narrative_ontology:measurement(naft_tr_t4, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 4, 0.18).
narrative_ontology:measurement_basis(naft_tr_t4, observed).
narrative_ontology:measurement(naft_tr_t9, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 9, 0.24).
narrative_ontology:measurement_basis(naft_tr_t9, observed).
narrative_ontology:measurement(naft_tr_t13, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 13, 0.28).
narrative_ontology:measurement_basis(naft_tr_t13, observed).
narrative_ontology:measurement(naft_tr_t18, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 18, 0.32).
narrative_ontology:measurement_basis(naft_tr_t18, observed).
narrative_ontology:measurement(naft_tr_t22, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 22, 0.36).
narrative_ontology:measurement_basis(naft_tr_t22, observed).
narrative_ontology:measurement(naft_tr_t26, nafta_jurisdictional_boundary__capital_supremacy_reading, theater_ratio, 26, 0.38).
narrative_ontology:measurement_basis(naft_tr_t26, observed).

% Extraction over time
narrative_ontology:measurement(naft_be_t0, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(naft_be_t0, observed).
narrative_ontology:measurement(naft_be_t4, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 4, 0.5).
narrative_ontology:measurement_basis(naft_be_t4, observed).
narrative_ontology:measurement(naft_be_t9, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 9, 0.58).
narrative_ontology:measurement_basis(naft_be_t9, observed).
narrative_ontology:measurement(naft_be_t13, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 13, 0.63).
narrative_ontology:measurement_basis(naft_be_t13, observed).
narrative_ontology:measurement(naft_be_t18, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 18, 0.67).
narrative_ontology:measurement_basis(naft_be_t18, observed).
narrative_ontology:measurement(naft_be_t22, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 22, 0.7).
narrative_ontology:measurement_basis(naft_be_t22, observed).
narrative_ontology:measurement(naft_be_t26, nafta_jurisdictional_boundary__capital_supremacy_reading, base_extractiveness, 26, 0.72).
narrative_ontology:measurement_basis(naft_be_t26, observed).

% Suppression requirement over time
narrative_ontology:measurement(naft_su_t0, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement_basis(naft_su_t0, observed).
narrative_ontology:measurement(naft_su_t4, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 4, 0.57).
narrative_ontology:measurement_basis(naft_su_t4, observed).
narrative_ontology:measurement(naft_su_t9, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 9, 0.63).
narrative_ontology:measurement_basis(naft_su_t9, observed).
narrative_ontology:measurement(naft_su_t13, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 13, 0.68).
narrative_ontology:measurement_basis(naft_su_t13, observed).
narrative_ontology:measurement(naft_su_t18, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 18, 0.72).
narrative_ontology:measurement_basis(naft_su_t18, observed).
narrative_ontology:measurement(naft_su_t22, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 22, 0.76).
narrative_ontology:measurement_basis(naft_su_t22, observed).
narrative_ontology:measurement(naft_su_t26, nafta_jurisdictional_boundary__capital_supremacy_reading, suppression_requirement, 26, 0.78).
narrative_ontology:measurement_basis(naft_su_t26, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nafta_jurisdictional_boundary__capital_supremacy_reading, resource_allocation).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__capital_supremacy_reading, nafta_jurisdictional_boundary__embedded_liberalism_reading).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__capital_supremacy_reading, nafta_jurisdictional_boundary__sovereignty_primacy_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the trade treaty's constraint on domestic regulation' decomposes into three structurally distinct readings of one kernel (a fixed treaty text), per the epsilon-invariance principle: capital_supremacy_reading (this file; high epsilon, capital-beneficiary structure, standards-bearing populations and regulators as victims), embedded_liberalism_reading (moderate epsilon, balanced policy space, no categorical victim set), and sovereignty_primacy_reading (low epsilon, coordination subordinate to domestic law). Each is a separate story with a single stable epsilon; forcing one story to span all three would make epsilon observer-dependent, which the chi formula forbids. The family link runs through shared dispute-settlement practice: whichever reading the tribunals operationalize supplies the evidentiary record the other two readings argue against, so drift in this story's measurements propagates to both siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
