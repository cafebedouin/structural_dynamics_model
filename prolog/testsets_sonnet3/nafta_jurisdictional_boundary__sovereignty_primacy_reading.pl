% ============================================================================
% CONSTRAINT STORY: nafta_jurisdictional_boundary__sovereignty_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: NAFTA/USMCA Treaty Text as Coordination Instrument Subordinate to Domestic Regulatory Sovereignty
 *   domain: international_trade_law/political_economy/regulatory_federalism
 *
 * SUMMARY:
 *   This story instantiates the sovereignty-primacy reading of the
 *   nafta_jurisdictional_boundary kernel: trade agreement text (NAFTA, later
 *   USMCA) functions as a coordination instrument that reduces transaction
 *   costs for cross-border trade, but treaty obligations enter the domestic
 *   policy landscape as one input among many, never as an overriding legal
 *   constraint on a state's regulatory authority over labor, environmental,
 *   or health standards within its own territory. This is a distinct
 *   constraint from the capital_supremacy_reading (which claims treaty text
 *   is supreme law preempting domestic regulation) and the
 *   embedded_liberalism_reading (which claims a balanced,
 *   non-discrimination-conditioned compatibility). Each reading has its own
 *   ε: this reading's ε is low because, on its own terms, extraction is
 *   limited to voluntary compliance costs foreign firms bear when meeting
 *   domestic standards they did not design — there is no coercive override
 *   mechanism to point to. Do not average this ε against the sibling
 *   readings' higher extraction estimates; they describe different structural
 *   claims about the same kernel.
 *
 * KEY AGENTS:
 *   - domestic_regulatory_agencies: agenda_setter (institutional/analytical) — sets and enforces standards, treats treaty as non-binding input
 *   - national_legislatures: agenda_setter/beneficiary (institutional/arbitrage) — gatekeeper of implementing legislation, retains ratification and withdrawal power
 *   - foreign_exporting_firms: payer (powerful/constrained) — bears compliance cost with no preemption remedy under this reading
 *   - capital_mobility_advocates: excluded (powerful/trapped) — structurally loses under this reading's interpretive victory
 *   - trade_dispute_panels: excluded (institutional/analytical) — issues findings that function as pressure, not command
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0.22).
domain_priors:suppression_score(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0.18).
domain_priors:theater_ratio(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nafta_jurisdictional_boundary__sovereignty_primacy_reading, rope).
narrative_ontology:human_readable(nafta_jurisdictional_boundary__sovereignty_primacy_reading, "NAFTA/USMCA Treaty Text as Coordination Instrument Subordinate to Domestic Regulatory Sovereignty").
narrative_ontology:topic_domain(nafta_jurisdictional_boundary__sovereignty_primacy_reading, "international_trade_law/political_economy/regulatory_federalism").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 'e5dda753-6935-4520-a106-9d03eb204a24').
narrative_ontology:cs_kernel_codification('e5dda753-6935-4520-a106-9d03eb204a24', fixed_text).
narrative_ontology:cs_authority_grounding('e5dda753-6935-4520-a106-9d03eb204a24', distributed).
narrative_ontology:cs_reading_relation('e5dda753-6935-4520-a106-9d03eb204a24', nafta_jurisdictional_boundary__capital_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('e5dda753-6935-4520-a106-9d03eb204a24', nafta_jurisdictional_boundary__embedded_liberalism_reading, coexists_with).
narrative_ontology:cs_axiom('e5dda753-6935-4520-a106-9d03eb204a24', foundational, domestic_regulatory_authority_is_undiminished_by_treaty_text).
narrative_ontology:cs_axiom_status(domestic_regulatory_authority_is_undiminished_by_treaty_text, holdable).
narrative_ontology:cs_axiom_grounding('e5dda753-6935-4520-a106-9d03eb204a24', domestic_regulatory_authority_is_undiminished_by_treaty_text, conventional).
narrative_ontology:cs_axiom('e5dda753-6935-4520-a106-9d03eb204a24', foundational, treaty_obligations_require_domestic_implementing_legislation_to_bind).
narrative_ontology:cs_axiom_status(treaty_obligations_require_domestic_implementing_legislation_to_bind, holdable).
narrative_ontology:cs_axiom_grounding('e5dda753-6935-4520-a106-9d03eb204a24', treaty_obligations_require_domestic_implementing_legislation_to_bind, conventional).
narrative_ontology:cs_reference_frame('e5dda753-6935-4520-a106-9d03eb204a24', dualist_non_self_execution_framework).
narrative_ontology:cs_drift_state('e5dda753-6935-4520-a106-9d03eb204a24', post_usmca_dispute_settlement_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e5dda753-6935-4520-a106-9d03eb204a24', '').
narrative_ontology:cs_kernel_id(nafta_jurisdictional_boundary__sovereignty_primacy_reading, nafta_jurisdictional_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__sovereignty_primacy_reading, domestic_regulatory_agencies).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__sovereignty_primacy_reading, national_legislatures).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__sovereignty_primacy_reading, import_competing_domestic_industries).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__sovereignty_primacy_reading, labor_and_environmental_advocacy_groups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__sovereignty_primacy_reading, foreign_exporting_firms).
narrative_ontology:constraint_vindicates(nafta_jurisdictional_boundary__sovereignty_primacy_reading, state_sovereignty_over_domestic_police_power).
narrative_ontology:constraint_vindicates(nafta_jurisdictional_boundary__sovereignty_primacy_reading, treaty_non_self_execution_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces labor, environmental, and health standards within its territory using domestic statutory authority. Treats treaty obligations as one input weighed against domestic political and legal processes, not as a ceiling or floor that automatically overrides local rulemaking. Can raise standards unilaterally; a trade panel finding is advisory pressure, not a binding override of domestic law absent implementing legislation.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, domestic_regulatory_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Retains the constitutional authority to ratify, implement, modify, or withdraw from trade agreement provisions through ordinary domestic lawmaking. Treaty text requires implementing legislation to have domestic legal effect in most dualist systems; the legislature is the gatekeeper, not a passive recipient of treaty command.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, national_legislatures, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(nafta_jurisdictional_boundary__sovereignty_primacy_reading, national_legislatures, beneficiary).

% Benefits from the ability to lobby domestic regulators and legislators for protective standards (safety, labor, environmental) without those standards being preempted by treaty text, since sovereignty-primacy readings hold that domestic law is not subordinated to the agreement.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, import_competing_domestic_industries, beneficiary,
    organized, biographical, constrained, national).

% Uses domestic political and legal channels to push for higher labor and environmental standards, relying on the premise that treaty text cannot compel a race-to-the-bottom because domestic regulatory authority is retained in full. Their leverage depends entirely on this reading being the operative one in domestic courts and agencies.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, labor_and_environmental_advocacy_groups, beneficiary,
    moderate, generational, mobile, national).

% Bears the compliance cost of meeting whatever domestic standard the importing state sets, since under this reading the treaty offers no override mechanism to compel harmonization or preempt the local rule. Can pursue trade panel dispute settlement, but on this reading a panel finding is not self-executing and does not automatically displace domestic law.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, foreign_exporting_firms, payer,
    powerful, biographical, constrained, continental).

% Issues findings on treaty compliance, but under this reading its rulings function as diplomatic/political pressure and potential authorization for retaliatory tariffs rather than as directly enforceable domestic law. Its practical authority is bounded by whether the losing state's legislature chooses to conform domestic law — a choice, not a compulsion.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, trade_dispute_panels, excluded,
    institutional, biographical, analytical, continental).

% Would prefer the treaty text function as supreme law overriding domestic regulatory divergence to secure predictable, harmonized conditions for cross-border investment. Under the sovereignty-primacy reading this position is not vindicated: their preferred remedy (binding preemption) is structurally unavailable, and they are excluded from the interpretive victory this reading represents.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, capital_mobility_advocates, excluded,
    powerful, generational, trapped, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nafta_jurisdictional_boundary__sovereignty_primacy_reading, diffuse).
narrative_ontology:fixing_cost_class(nafta_jurisdictional_boundary__sovereignty_primacy_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared negotiated text that lowers transaction costs and predictability for cross-border trade and investment, while leaving each state's domestic regulatory apparatus as the final adjudicator of labor, environmental, and health standards within its own territory.
% TRANSFER_FUNCTION: Under this reading, the arrangement transfers little coercive authority anywhere: it moves information (a common negotiated baseline) and moves some compliance cost onto foreign firms who must meet whatever domestic standard applies, but does not move regulatory sovereignty from domestic legislatures to a supranational or foreign body.
% ABSENT_VOICES: Capital mobility advocates and firms seeking harmonized, litigation-proof regulatory certainty are structurally absent from this reading's victory — the reading exists precisely because their preferred preemption doctrine did not prevail domestically. Foreign firms bearing compliance costs are present as payers but have no forum in which this reading itself is contestable; they can only contest the underlying domestic standard.
% DISAPPEARANCE_RATIONALE: If this READING (rather than the treaty itself) disappeared overnight — i.e., if capital-supremacy or embedded-liberalism readings prevailed instead in domestic courts and agencies — advocacy groups and import-competing industries would lose significant leverage over domestic standard-setting, and foreign firms might gain preemption arguments they currently lack. Whether the 'world' meaningfully rearranges depends on which sibling reading fills the vacuum; parties dispute how much daylight exists between readings in practice.
% FOUNDING_PROBLEM: Early GATT/NAFTA-era negotiators needed language that could secure ratification across dualist legal systems (notably the US and Canada) where legislatures would not cede final regulatory authority to an international body, while still delivering tariff reduction and market access commitments.
% FOUNDING_PROBLEM_CORROBORATION: US constitutional scholars and congressional ratification debates (Trade Act implementing legislation requirements, non-self-execution doctrine in U.S. treaty jurisprudence) corroborate that sovereignty-primacy was a genuine legal condition of ratification, not merely advocacy rhetoric — this is attested in legislative history and case law outside the advocacy groups that now rely on the reading. Capital mobility advocates and some trade economists dispute that the founding problem is fully alive today, arguing dispute-settlement mechanisms have hardened into de facto override pressure that domestic sovereignty rhetoric obscures.
narrative_ontology:disappearance_verdict(nafta_jurisdictional_boundary__sovereignty_primacy_reading, contested).
narrative_ontology:founding_problem_status(nafta_jurisdictional_boundary__sovereignty_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0.22, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction is authored low (0.22) because, under this reading, the only cost imposed on any party is the ordinary compliance cost of meeting a domestic regulatory standard that was never subordinated to treaty text — there is no coercive transfer mechanism the reading recognizes as legitimate or operative. Suppression is low (0.18): no party is prevented from exiting or contesting the arrangement through ordinary domestic political and legal channels; a legislature can withdraw or amend implementing law at will. Theater ratio is moderate (0.30) and drifts slightly upward over the interval, reflecting a mild but real gap between the rhetorical invocation of 'sovereignty' in domestic political discourse and dispute-panel findings that, in practice, generate diplomatic and reputational pressure resembling soft compliance incentives — this is the seam where the sibling capital_supremacy_reading would measure the same historical record very differently.
 *
 * DIRECTIONALITY LOGIC:
 *   Domestic regulatory agencies and legislatures sit at the beneficiary end: the reading vindicates their retained authority and imposes no cost on them. Import-competing industries and advocacy groups are secondary beneficiaries, gaining leverage to shape domestic standards without treaty-based preemption risk. Foreign exporting firms are the nearest thing to a payer, bearing compliance costs, but under this reading those costs are ordinary regulatory costs of market access, not extraction by the treaty mechanism itself — hence no victims are declared. Capital mobility advocates and trade dispute panels are excluded rather than victimized: their preferred override mechanism simply does not operate under this reading, which is a structural loss for their position but not an extraction from them within this constraint's own terms.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — securing ratifiability in dualist legal systems whose legislatures would not cede final regulatory authority — remains partly live (contested) because current disputes over dispute-settlement panel authority and regulatory chill effects suggest the boundary this reading asserts is under active pressure, not settled. Because there are no beneficiaries collecting rent through coercion (extraction is low and diffuse) and no active enforcement is required to hold this reading in place domestically, this does not present as mandatrophy — the sovereignty-primacy reading persists because it continues to track real constitutional and statutory structure, not because a defunct mandate is administered by inertia. Its status could shift to mandatrophy-adjacent territory only if dispute-panel findings become de facto self-executing in practice while the sovereignty rhetoric persists unchanged — which is precisely the seam the corresponding omega documents.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dispute_panel_soft_override_ambiguity,
    'Do trade dispute panel findings under this treaty regime function purely as diplomatic/reputational pressure (consistent with sovereignty-primacy), or have they hardened into a de facto override mechanism that domestic ''sovereignty'' rhetoric obscures (which would support the capital_supremacy_reading instead)?',
    'Longitudinal tracking of domestic legislative and regulatory responses to adverse panel findings: if states routinely conform domestic law to panel findings without independent domestic political deliberation, the de facto pattern diverges from the sovereignty-primacy reading regardless of formal doctrine.',
    'If panel findings are shown to function as de facto binding constraints, this reading''s low extractiveness score would be understated and the constraint would more closely resemble the capital_supremacy_reading''s structural claims — a different constraint, not a recalibration of this one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dispute_panel_soft_override_ambiguity, empirical, 'Whether dispute panel findings are genuinely advisory or de facto binding in practice.').

omega_variable(
    regulatory_chill_effect_ambiguity,
    'Does the mere prospect of a trade dispute (even one this reading treats as non-binding) cause domestic regulators to self-censor or water down proposed labor/environmental/health standards before they are ever formally challenged?',
    'Comparative case studies of proposed domestic regulations that were withdrawn, weakened, or never introduced, cross-referenced against internal agency deliberation records citing trade-dispute exposure as a factor.',
    'If a significant chilling effect exists, the sovereignty-primacy reading''s claim that domestic regulatory authority is retained ''in full'' would be descriptively incomplete even where it remains formally accurate — suppression may be higher than the 0.18 authored here in practice, though the reading''s own doctrinal terms would still register it as voluntary restraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_chill_effect_ambiguity, empirical, 'Whether anticipated dispute exposure suppresses domestic regulatory ambition independent of formal treaty override.').

omega_variable(
    kernel_framing_which_reading_prevails_in_practice,
    'Which of the three sibling readings (sovereignty_primacy, capital_supremacy, embedded_liberalism) best describes the ACTUAL operative doctrine in a given dispute, court, or era — and does the answer vary by forum (domestic courts vs. trade panels vs. political rhetoric)?',
    'Systematic coding of judicial and panel decisions by which reading''s logic they implicitly apply, disaggregated by forum and time period.',
    'If different forums operate under different readings simultaneously, the kernel itself may be best modeled as jurisdiction-specific rather than as a single contested kernel with three competing readings — this would suggest further decomposition rather than resolution of the existing three.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_which_reading_prevails_in_practice, conceptual, 'Whether the three readings are genuinely competing interpretations of one kernel or are already forum-specific and hence separately operative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 1994, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(naft_tr_t1994, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 1994, 0.25).
narrative_ontology:measurement(naft_tr_t2000, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 2000, 0.26).
narrative_ontology:measurement(naft_tr_t2006, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 2006, 0.27).
narrative_ontology:measurement(naft_tr_t2012, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 2012, 0.28).
narrative_ontology:measurement(naft_tr_t2018, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 2018, 0.29).
narrative_ontology:measurement(naft_tr_t2024, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 2024, 0.3).

% Extraction over time
narrative_ontology:measurement(naft_be_t1994, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 1994, 0.15).
narrative_ontology:measurement(naft_be_t2000, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 2000, 0.17).
narrative_ontology:measurement(naft_be_t2006, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 2006, 0.19).
narrative_ontology:measurement(naft_be_t2012, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 2012, 0.2).
narrative_ontology:measurement(naft_be_t2018, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 2018, 0.21).
narrative_ontology:measurement(naft_be_t2024, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 2024, 0.22).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(nafta_jurisdictional_boundary__sovereignty_primacy_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nafta_jurisdictional_boundary__sovereignty_primacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0.08).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__sovereignty_primacy_reading, nafta_jurisdictional_boundary__capital_supremacy_reading).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__sovereignty_primacy_reading, nafta_jurisdictional_boundary__embedded_liberalism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the nafta_jurisdictional_boundary kernel. capital_supremacy_reading claims the treaty text functions as supreme law overriding domestic standards (high extraction, coercive harmonization). embedded_liberalism_reading claims a conditional compatibility (moderate extraction, non-discrimination-gated). This story (sovereignty_primacy_reading) claims the lowest extraction of the three, holding that domestic regulatory authority is fully retained and treaty obligations are non-overriding. All three share the same underlying treaty text and dispute-settlement history but diverge sharply in claimed ε because they are structurally distinct claims about what that text and history mean — per the ε-invariance principle, they are authored as three separate files linked here rather than as one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
