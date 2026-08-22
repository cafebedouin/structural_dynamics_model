% ============================================================================
% CONSTRAINT STORY: nafta_jurisdictional_boundary__sovereignty_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
    narrative_ontology:measurement_basis/2,
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
 *   human_readable: Trade Agreement as Sovereign-Subordinate Coordination Mechanism (Sovereignty Primacy Reading)
 *   domain: economic/legal/political
 *
 * SUMMARY:
 *   A continental trade agreement (in force 1994, succeeded by its
 *   renegotiated successor in 2020) functions on this reading as a
 *   coordination mechanism among sovereign states that remains subordinate to
 *   domestic law. The agreement binds tariff schedules, codifies rules of
 *   origin and customs procedures, and maintains dispute panels that price
 *   regulatory choices affecting trading partners — but no treaty organ can
 *   strike down or override a domestic labor, environmental, or health
 *   standard. States retain full regulatory authority within their territory:
 *   when a standard draws a partner challenge, the state chooses between
 *   paying the priced cost (compensation or retaliation exposure) and
 *   adjusting the rule, and the choice remains sovereign. Extraction under
 *   this reading is limited to the voluntary compliance costs the signatory
 *   states accepted in exchange for preferential market access; the
 *   arrangement persists through reciprocal benefit and a standing withdrawal
 *   clause, not through coercive maintenance.
 *
 * KEY AGENTS:
 *   - sovereign_member_states: Principal agenda-setter (institutional/mobile) — negotiated, signed, and administer the agreement through the Free Trade Commission; retain full regulatory authority; can withdraw on notice; bear voluntary compliance costs when regulation draws a priced challenge.
 *   - exporting_industries: Primary beneficiary (organized/mobile) — gain bound tariff schedules, predictable rules of origin, and preferential access to the continental market.
 *   - domestic_regulatory_agencies: Protected beneficiary (institutional/constrained) — set and enforce labor, environmental, and health standards that no treaty organ can overrule; the subordination structure shields their jurisdiction.
 *   - import_competing_producers: Cost-bearer (organized/constrained) — lost tariff protection by sovereign consent; recourse runs through domestic politics, which the treaty leaves fully open.
 *   - non_member_trading_partners: Excluded (powerful/mobile) — face preference erosion from outside the agreement; recourse through WTO committees and their own preferential networks.
 *   - dispute_settlement_practitioners: Secondary beneficiary (moderate/mobile) — panelists and trade counsel who sustain and are sustained by the dispute machinery.
 *   - trade_policy_analysts: Analytical observer (analytical/analytical) — track the boundary between treaty obligation and domestic regulatory authority across the agreement's operation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0.22).
domain_priors:suppression_score(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0.15).
domain_priors:theater_ratio(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(nafta_jurisdictional_boundary__sovereignty_primacy_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nafta_jurisdictional_boundary__sovereignty_primacy_reading, rope).
narrative_ontology:human_readable(nafta_jurisdictional_boundary__sovereignty_primacy_reading, "Trade Agreement as Sovereign-Subordinate Coordination Mechanism (Sovereignty Primacy Reading)").
narrative_ontology:topic_domain(nafta_jurisdictional_boundary__sovereignty_primacy_reading, "economic/legal/political").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nafta_jurisdictional_boundary__sovereignty_primacy_reading, '604eb0a3-2f4e-4513-bc5a-f66884a2e464').
narrative_ontology:cs_kernel_codification('604eb0a3-2f4e-4513-bc5a-f66884a2e464', formalized).
narrative_ontology:cs_authority_grounding('604eb0a3-2f4e-4513-bc5a-f66884a2e464', self_enforcing).
narrative_ontology:cs_reading_relation('604eb0a3-2f4e-4513-bc5a-f66884a2e464', nafta_jurisdictional_boundary__capital_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('604eb0a3-2f4e-4513-bc5a-f66884a2e464', nafta_jurisdictional_boundary__embedded_liberalism_reading, influences).
narrative_ontology:cs_axiom('604eb0a3-2f4e-4513-bc5a-f66884a2e464', foundational, domestic_regulatory_supremacy).
narrative_ontology:cs_axiom_status(domestic_regulatory_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('604eb0a3-2f4e-4513-bc5a-f66884a2e464', domestic_regulatory_supremacy, conventional).
narrative_ontology:cs_axiom('604eb0a3-2f4e-4513-bc5a-f66884a2e464', foundational, treaty_breach_priced_not_commanded).
narrative_ontology:cs_axiom_status(treaty_breach_priced_not_commanded, holdable).
narrative_ontology:cs_axiom_grounding('604eb0a3-2f4e-4513-bc5a-f66884a2e464', treaty_breach_priced_not_commanded, conventional).
narrative_ontology:cs_reference_frame('604eb0a3-2f4e-4513-bc5a-f66884a2e464', westphalian_consent_treaty_order).
narrative_ontology:cs_drift_state('604eb0a3-2f4e-4513-bc5a-f66884a2e464', usmca_implementation_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('604eb0a3-2f4e-4513-bc5a-f66884a2e464', '').
narrative_ontology:cs_kernel_id(nafta_jurisdictional_boundary__sovereignty_primacy_reading, nafta_jurisdictional_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__sovereignty_primacy_reading, sovereign_member_states).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__sovereignty_primacy_reading, exporting_industries).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__sovereignty_primacy_reading, domestic_regulatory_agencies).
narrative_ontology:constraint_beneficiary(nafta_jurisdictional_boundary__sovereignty_primacy_reading, dispute_settlement_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(nafta_jurisdictional_boundary__sovereignty_primacy_reading, import_competing_producers).
narrative_ontology:constraint_vindicates(nafta_jurisdictional_boundary__sovereignty_primacy_reading, sovereign_regulatory_autonomy_doctrine).
narrative_ontology:constraint_vindicates(nafta_jurisdictional_boundary__sovereignty_primacy_reading, treaty_non_self_execution_practice).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Negotiated and signed the agreement, administer it through the Free Trade Commission, and retain full authority to set and enforce labor, environmental, and health standards within their territory. When a regulatory choice draws a partner challenge, the state chooses between paying the priced cost — compensation or retaliation exposure — and adjusting the rule; the choice is sovereign. Withdrawal is available on six months' notice, as the 2017-2020 renegotiation demonstrated. The coordination surplus returns as preferential market access for their exporters.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, sovereign_member_states, agenda_setter,
    institutional, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(nafta_jurisdictional_boundary__sovereignty_primacy_reading, sovereign_member_states, beneficiary).

% Sell into the continental market under bound tariff schedules and codified rules of origin. They collect the agreement's principal coordination benefit — predictable, preferential access — and can redirect trade toward other markets if preferences erode, though sunk supply chains make redirection costly at the margin.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, exporting_industries, beneficiary,
    organized, biographical, mobile, continental).

% Set and enforce labor, environmental, and health standards within their national jurisdiction. No treaty organ can strike down or overrule their standards; when a standard draws a trade challenge, the state — not the agency — bears the priced cost. Their jurisdictional authority is what the subordination structure protects.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, domestic_regulatory_agencies, beneficiary,
    institutional, generational, constrained, national).

% Lost tariff protection by their state's consent and now face continental competition. Their recourse is domestic: antidumping and countervailing petitions (which the treaty channels into binational review rather than eliminating), adjustment demands, and electoral pressure. They cannot exit their industries cheaply, but the political channels that could restore protection remain fully open.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, import_competing_producers, payer,
    organized, biographical, constrained, national).

% Export into the bloc facing preferential margins against them and were never party to the negotiations. Their recourse is the WTO (where the agreement's free-trade-area status is defended) and building their own preferential networks, which most have done. They would negotiate market-access terms if admitted to the conversation.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, non_member_trading_partners, excluded,
    powerful, generational, mobile, global).

% Panelists, trade counsel, and arbitration specialists who operate the dispute machinery — binational panel reviews, state-to-state consultations, and in the treaty's earlier chapters investor claims. They collect fees for operating the pricing mechanism and would lose the work if the machinery wound down.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, dispute_settlement_practitioners, beneficiary,
    moderate, biographical, mobile, continental).

% Academic and institutional analysts who track where treaty authority ends and domestic regulatory authority begins, comparing panel reports, domestic court treatment of the agreement, and state behavior across episodes. They collect nothing and pay nothing; their seat is the measurement seat.
narrative_ontology:constraint_stakeholder(nafta_jurisdictional_boundary__sovereignty_primacy_reading, trade_policy_analysts, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a standing rule-set for continental market access: binds tariff schedules, codifies rules of origin and customs procedures, and gives the member states a reciprocal forum for pricing trade frictions — functions no single state can provide unilaterally without forgoing its neighbors' market.
% TRANSFER_FUNCTION: Moves tariff revenue and protection rents from import-competing producers toward exporters and consumers, by sovereign consent; and, when a regulatory choice burdens partners, moves a priced payment (compensation or retaliation exposure) from the regulating state toward the challenging state's affected industries — the regulation itself remains in force.
% ABSENT_VOICES: Non-member trading partners facing preference erosion were never in the negotiating room and object through WTO committees; import-competing producers and adjustment-burdened regions were represented only indirectly through domestic politics; future generations in concentrated adjustment regions have no seat at all. All of these channels remain open under this reading — the constraint does not close them.
% DISAPPEARANCE_RATIONALE: Tariff schedules and rules-of-origin regimes would lapse to WTO baselines; the three states would renegotiate bilateral coverage within a few years; regulatory authority would be unaffected, since it was never ceded — but trade flows, investment patterns, and the adjustment settlements built on preferential access would reorganize.
% FOUNDING_PROBLEM: Before the treaty, continental trade was taxed and unpredictable: tariff escalation, discretionary customs administration, and the threat of unilateral trade measures suppressed cross-border trade and investment in North America. The treaty was built to bind tariff schedules and replace unilateral threat with reciprocal rule.
% FOUNDING_PROBLEM_CORROBORATION: Pre-treaty tariff schedules, negotiating records, and the period's WTO trade policy reviews attest the founding problem from outside the beneficiary set, as does the academic trade literature; no party outside the signatory states disputes that the original tariff-uncertainty problem existed. The dispute is over status: the member states attest the machinery still serves live frictions (rules-of-origin drift, sanitary standards, energy, digital trade), while a substantial body of trade economists attests the core tariff problem was solved early and the residual machinery is maintenance.
narrative_ontology:disappearance_verdict(nafta_jurisdictional_boundary__sovereignty_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(nafta_jurisdictional_boundary__sovereignty_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nafta_jurisdictional_boundary__sovereignty_primacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0.22, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is low (0.22 at interval end) because the agreement's costs on states are priced choices, not compelled transfers: a state that loses a panel pays compensation or absorbs retaliation while keeping its regulation, and the withdrawal clause keeps participation itself a standing choice. Suppression is low (0.15) — and as a raw structural property it is authored unscaled; the engine owns any context scaling — because alternatives are not suppressed: WTO baseline rules, bilateral agreements, and plain domestic regulation remain available, and the dispute machinery cannot command, only price. Theater is low-moderate (0.18): the coordination function is real and load-bearing (tariffs stayed bound, rules of origin were administered, customs procedures harmonized), with a modest ritual share in panel practice and periodic review reporting. Accessibility_collapse is low (0.3): understanding the constraint does not close alternatives, because the constraint never claimed supremacy over them. Resistance is moderate (0.4): states negotiated extensive carve-outs and reservations, refused self-executing domestic effect, and import-competing producers mounted sustained antidumping pressure — resistance to the treaty's reach, not to its subordination core. requires_active_enforcement is authored false: the arrangement holds on reciprocal benefit with self-help pricing, not on coercive maintenance. Receipt surface: gain_flow is 'diffuse' as an affirmative check across all seven seats — coordination surplus accrues broadly to member exporters and consumers, compliance-cost receipts flow case-by-case to whichever partner prevails in a dispute, and the practitioner class collects service fees rather than extraction. fixing_cost is 'cheap': the agenda-setter states hold the revising pen, demonstrated at the successor renegotiation — removal is procedurally a notice clause, at ordinary political cost and with no structural barrier. The claim and the metrics are authored independently; where the engine's computed per-seat types diverge from the rope claim, that divergence is the measurement.
 *
 * PERSPECTIVAL GAP:
 *   The seats diverge sharply. From the sovereign states' seat the treaty is a self-imposed coordination device, revisable and dissolvable at will — the successor renegotiation demonstrated the pen. From the domestic regulatory agencies' seat the same structure is a shield: subordination is what keeps their standards law. From import-competing producers' seat the identical text dissolved their protection — a cost their sovereign consented to without theirs, experienced as imposed despite the consent structure. From non-member partners' seat the arrangement is a discriminatory preference wall they never joined. The engine computes per-seat classifications from these structural positions; this story's rope claim reflects the agenda-setter and beneficiary seats, where the consent structure is operative.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to low directionality: sovereign_member_states (collect the coordination surplus and control the rules), exporting_industries (collect market access), domestic_regulatory_agencies (collect jurisdictional protection), and dispute_settlement_practitioners (collect fees) all sit near the beneficiary end. import_competing_producers bear the agreement's principal costs — dissolved protection — but hold open domestic political recourse, placing them at moderate directionality rather than full target: they pay through consented politics, not through coercion, and are therefore authored as payers without a victims declaration under this reading. non_member_trading_partners sit at moderately high directionality — they bear discriminatory preference costs — without being extraction victims in this reading's sense: preference erosion is the byproduct of a consented liberalization among equals. No seat sits at the full-target end; the reading's structural claim is precisely that no one is compelled.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — tariff unpredictability and unilateral threat — is substantially solved: tariffs were bound early and the machinery never faced a compliance crisis on its core function. The constraint is not mandatrophy-resolved, because the coordination function migrated to live frictions (rules-of-origin drift, sanitary standards, energy, and in the successor era digital trade and labor enforcement), keeping the machinery load-bearing. The rope classification prevents two misreadings: reading the arrangement as pure extraction (which would require victims this reading cannot identify — the costs are priced and consented) and reading it as inertial theater (the coordination demonstrably works — preferential flows and bound schedules persisted through the 2018 tariff episode, where a member openly breached and paid the priced cost, confirming rather than refuting the subordination structure).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexical,
    'This constraint is the sovereignty_primacy_reading of the nafta_jurisdictional_boundary kernel: is the treaty''s jurisdictional boundary correctly located at the edge of domestic regulatory law (obligations price, never override), or inside it (capital_supremacy_reading: obligations override domestic standards), or conditional upon a protected policy space (embedded_liberalism_reading)?',
    'Cross-reading comparison of the three sibling stories over the same kernel, adjudicated against operative legal practice: how domestic courts treat the agreement''s domestic effect, whether any treaty organ ever overrode a domestic standard, and how states behaved when regulation and obligation collided.',
    'Adopting the capital-supremacy sibling moves treaty obligations into the overriding set, creates a victim set among regulated domestic actors, and raises extraction sharply above the voluntary-compliance range; adopting the embedded sibling splits this constraint into a market-access rope plus policy-space reservations with moderate extraction. This story''s epsilon and beneficiary structure are valid only under the sovereignty reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexical, conceptual, 'Reading-index of the jurisdictional kernel: which sibling reading the operative arrangement instantiates.').

omega_variable(
    subordination_in_operation,
    'Does the treaty remain subordinate in operation — do states in practice retain full regulatory authority, or does anticipated retaliation and compensation function as an overriding constraint on regulatory choices?',
    'Compare regulatory outcomes in treaty-covered versus uncovered sectors and pre- versus post-treaty standard-setting; count episodes where a state regulated and absorbed the priced cost (the 2018 tariff actions, retained SPS measures) against episodes where a state declined to regulate under anticipated challenge.',
    'If states systematically decline regulation under anticipated treaty cost, the effective constraint approaches the capital-supremacy sibling and extraction rises above the voluntary-compliance range; if regulation proceeds and prices are paid, this reading''s epsilon stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subordination_in_operation, empirical, 'Whether subordination holds in operation or retaliation pressure functions as override.').

omega_variable(
    compliance_voluntariness,
    'Are compliance costs genuinely voluntary — are the withdrawal clause and the regulate-and-pay option real alternatives — or does dependence on preferential market access make exit and non-compliance practically unavailable?',
    'Examine withdrawal and near-exit episodes (the 2017-2020 renegotiation threats, actual tariff actions by a member) and whether states that regulated against partner objections suffered durable market loss beyond the priced cost.',
    'If exit and regulate-and-pay are practically unavailable, suppression is understated by the scalar and the arrangement drifts from voluntary toward enforced coordination, moving the computed type toward the tangled-rope boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_voluntariness, empirical, 'Whether the voluntary-compliance premise holds against observed exit and defiance behavior.').

omega_variable(
    preference_erosion_victim_status,
    'Is the discriminatory preference structure — members'' preferential access against outsiders — a cost excluded partners voluntarily accept within WTO free-trade-area rules, or a transfer the constraint extracts from non-members?',
    'WTO committee records and outsiders'' own agreement-building responses; measure whether trade-diversion losses to non-members are offset by their subsequent preferential networks.',
    'If extraction from outsiders is substantial, the constraint carries a victim set this reading does not register and the rope classification is incomplete, with non_member_trading_partners as victims.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(preference_erosion_victim_status, empirical, 'Whether excluded trading partners constitute an unregistered victim set.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nafta_jurisdictional_boundary__sovereignty_primacy_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(naft_tr_t0, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(naft_tr_t0, observed).
narrative_ontology:measurement(naft_tr_t6, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 6, 0.11).
narrative_ontology:measurement_basis(naft_tr_t6, observed).
narrative_ontology:measurement(naft_tr_t12, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 12, 0.13).
narrative_ontology:measurement_basis(naft_tr_t12, observed).
narrative_ontology:measurement(naft_tr_t18, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 18, 0.15).
narrative_ontology:measurement_basis(naft_tr_t18, observed).
narrative_ontology:measurement(naft_tr_t24, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 24, 0.16).
narrative_ontology:measurement_basis(naft_tr_t24, observed).
narrative_ontology:measurement(naft_tr_t30, nafta_jurisdictional_boundary__sovereignty_primacy_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement_basis(naft_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(naft_be_t0, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 0, 0.14).
narrative_ontology:measurement_basis(naft_be_t0, observed).
narrative_ontology:measurement(naft_be_t6, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 6, 0.15).
narrative_ontology:measurement_basis(naft_be_t6, observed).
narrative_ontology:measurement(naft_be_t12, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 12, 0.17).
narrative_ontology:measurement_basis(naft_be_t12, observed).
narrative_ontology:measurement(naft_be_t18, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 18, 0.18).
narrative_ontology:measurement_basis(naft_be_t18, observed).
narrative_ontology:measurement(naft_be_t24, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 24, 0.2).
narrative_ontology:measurement_basis(naft_be_t24, observed).
narrative_ontology:measurement(naft_be_t30, nafta_jurisdictional_boundary__sovereignty_primacy_reading, base_extractiveness, 30, 0.22).
narrative_ontology:measurement_basis(naft_be_t30, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(nafta_jurisdictional_boundary__sovereignty_primacy_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nafta_jurisdictional_boundary__sovereignty_primacy_reading, resource_allocation).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__sovereignty_primacy_reading, nafta_jurisdictional_boundary__capital_supremacy_reading).
narrative_ontology:affects_constraint(nafta_jurisdictional_boundary__sovereignty_primacy_reading, nafta_jurisdictional_boundary__embedded_liberalism_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the nafta_jurisdictional_boundary kernel decomposes into three readings with distinct, stable epsilon values — capital_supremacy_reading (obligations override domestic law; high extraction on regulatory capacity), embedded_liberalism_reading (market access balanced against protected policy space; moderate extraction), and this sovereignty_primacy_reading (obligations price but never override; extraction limited to voluntary compliance costs). The colloquial label 'the treaty's effect on domestic regulation' conflates the three; each gets its own epsilon, beneficiary structure, and classification. This story links both siblings: the capital reading is the supremacy claim this reading's core premise denies, and the embedded reading is the intermediate settlement this reading's carve-out practice structurally pressures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
