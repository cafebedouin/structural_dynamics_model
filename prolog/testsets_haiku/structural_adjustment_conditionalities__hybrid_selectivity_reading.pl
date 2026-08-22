% ============================================================================
% CONSTRAINT STORY: structural_adjustment_conditionalities__hybrid_selectivity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_structural_adjustment_conditionalities__hybrid_selectivity_reading, []).

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
 *   constraint_id: structural_adjustment_conditionalities__hybrid_selectivity_reading
 *   human_readable: Conditional Structural Adjustment (Hybrid Selectivity Reading)
 *   domain: international_political_economy/development_finance/institutional_economics
 *
 * SUMMARY:
 *   This constraint instantiates one reading of the contested
 *   'structural_adjustment_conditionalities' kernel: the hybrid-selectivity
 *   reading. Conditionalities (loan conditions imposed by multilateral
 *   lenders) are ostensibly neutral coordination mechanisms ensuring fiscal
 *   discipline and market confidence. This reading contests that framing:
 *   conditionalities function as extractive discipline applied unequally
 *   depending on geopolitical position. Non-strategic debtor states face full
 *   enforcement—privatization mandates, austerity budgets, currency
 *   devaluation—that transfer policy control to creditors and impose massive
 *   welfare costs on debtor populations. Strategically important debtor
 *   states (Turkey, Egypt, Poland) receive conditionality waivers, policy
 *   flexibility, and rescue packages with minimal structural adjustment
 *   demands. The mechanism is Tangled Rope: real coordination problems exist
 *   (preventing default cascades, maintaining creditor confidence), but the
 *   solution has been captured by hegemon-aligned creditor states to extract
 *   geopolitical leverage and protect their claims while imposing costs
 *   asymmetrically based on strategic value rather than actual fiscal need.
 *   The selectivity is not random variation—it is structural: creditor voting
 *   power is concentrated in G7 hands, enforcement discretion is centralized
 *   in multilateral institutions dominated by US preferences, and the
 *   definition of 'strategic importance' is explicitly tied to geopolitical
 *   alignment with the hegemon. The constraint persists because non-strategic
 *   debtors cannot credibly exit (default is catastrophic), alternative
 *   creditors are delegitimized, and the coordination cover story remains
 *   plausible enough to maintain elite consensus.
 *
 * KEY AGENTS:
 *   - Hegemon-aligned creditor states (US, G7 allies): set the agenda, decide enforcement selectivity, benefit from geopolitical leverage and debt servicing.
 *   - Multilateral creditor institutions (IMF, World Bank, regional development banks): administer conditionalities under creditor-state direction, expand institutional scope, provide technical legitimation for politically selective enforcement.
 *   - Core private creditor banks: benefit from conditionalities ensuring debt-servicing priority and protecting their claims against welfare claims.
 *   - Non-strategic debtor states: bear full enforcement costs—policy control transferred, budgets slashed, development delayed by austerity mandates.
 *   - Populations of non-strategic debtor countries: powerless, trapped; bear welfare costs (healthcare/education cuts, unemployment, currency devaluation, loss of price controls) without voice in decisions that impose them.
 *   - Geopolitically strategic debtor states: receive waivers and flexibility in exchange for political alignment; access cheaper financing (risk premium waived); retain domestic policy autonomy on core issues.
 *   - Alternative creditors (China, regional development banks, Gulf funds): structurally excluded from governance; offer conditionality-free financing, but are delegitimized by the incumbent creditor coalition.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0.68).
domain_priors:suppression_score(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0.72).
domain_priors:theater_ratio(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(structural_adjustment_conditionalities__hybrid_selectivity_reading, tangled_rope).
narrative_ontology:human_readable(structural_adjustment_conditionalities__hybrid_selectivity_reading, "Conditional Structural Adjustment (Hybrid Selectivity Reading)").
narrative_ontology:topic_domain(structural_adjustment_conditionalities__hybrid_selectivity_reading, "international_political_economy/development_finance/institutional_economics").

domain_priors:requires_active_enforcement(structural_adjustment_conditionalities__hybrid_selectivity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(structural_adjustment_conditionalities__hybrid_selectivity_reading, 'e8908eed-5b31-48df-be15-bc9575a76649').
narrative_ontology:cs_kernel_codification('e8908eed-5b31-48df-be15-bc9575a76649', formalized).
narrative_ontology:cs_authority_grounding('e8908eed-5b31-48df-be15-bc9575a76649', extraction).
narrative_ontology:cs_interpretation_layer_present('e8908eed-5b31-48df-be15-bc9575a76649').
narrative_ontology:cs_reading_relation('e8908eed-5b31-48df-be15-bc9575a76649', structural_adjustment_conditionalities__creditor_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('e8908eed-5b31-48df-be15-bc9575a76649', structural_adjustment_conditionalities__debtor_extraction_reading, coexists_with).
narrative_ontology:cs_axiom('e8908eed-5b31-48df-be15-bc9575a76649', foundational, selectivity_is_geopolitical_not_technical).
narrative_ontology:cs_axiom_status(selectivity_is_geopolitical_not_technical, holdable).
narrative_ontology:cs_axiom_grounding('e8908eed-5b31-48df-be15-bc9575a76649', selectivity_is_geopolitical_not_technical, empirically_contingent).
narrative_ontology:cs_axiom('e8908eed-5b31-48df-be15-bc9575a76649', secondary, coordination_function_subordinate_to_extraction).
narrative_ontology:cs_axiom_status(coordination_function_subordinate_to_extraction, holdable).
narrative_ontology:cs_axiom_grounding('e8908eed-5b31-48df-be15-bc9575a76649', coordination_function_subordinate_to_extraction, empirically_contingent).
narrative_ontology:cs_reference_frame('e8908eed-5b31-48df-be15-bc9575a76649', multilateral_conditionality_discipline_framework).
narrative_ontology:cs_drift_state('e8908eed-5b31-48df-be15-bc9575a76649', contemporary_post_default_risk_decline, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e8908eed-5b31-48df-be15-bc9575a76649', '').
narrative_ontology:cs_kernel_id(structural_adjustment_conditionalities__hybrid_selectivity_reading, structural_adjustment_conditionalities).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__hybrid_selectivity_reading, hegemon_aligned_creditor_states).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__hybrid_selectivity_reading, core_multilateral_creditor_institutions).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__hybrid_selectivity_reading, core_private_creditor_banks).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__hybrid_selectivity_reading, non_strategic_debtor_states).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__hybrid_selectivity_reading, populations_of_policy_constrained_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__hybrid_selectivity_reading, geopolitically_strategic_debtor_states).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__hybrid_selectivity_reading, geopolitically_strategic_debtor_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Dominant geopolitical actors (typically the US and allied G7 countries) that shape lending rules through voting power in multilateral institutions and through bilateral loan covenants. They set the conditionality agenda, enforce it selectively (loosening conditions for strategically important debtors while tightening them for weaker states), and benefit from both debt servicing and geopolitical leverage. Their structural position allows them to decide which states are 'strategic' and thus exempt from full enforcement.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, hegemon_aligned_creditor_states, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(structural_adjustment_conditionalities__hybrid_selectivity_reading, hegemon_aligned_creditor_states, beneficiary).

% The IMF, World Bank, and regional development banks that administer conditionality frameworks. They justify conditionalities as technical requirements for fiscal health and market confidence. They collect administrative fees, expand their institutional scope, and gain leverage over debtor governments' domestic policy—yet their enforcement varies sharply: they tighten conditions on non-strategic debtors and accommodate deviations from strategic borrowers without penalty or public scrutiny.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, core_multilateral_creditor_institutions, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(structural_adjustment_conditionalities__hybrid_selectivity_reading, core_multilateral_creditor_institutions, beneficiary).

% Major multinational banks that hold sovereign debt and provide financing. They benefit from conditionalities because the enforcement apparatus ensures debt servicing priority and protects their claims ahead of social spending. Selective enforcement protects their exposure to strategically important debtors while conditionalities on non-strategic borrowers guarantee repayment even at the cost of welfare losses.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, core_private_creditor_banks, beneficiary,
    powerful, biographical, mobile, global).

% Developing and middle-income countries without geopolitical leverage (typically in sub-Saharan Africa, parts of Latin America, and South Asia) that face full enforcement of conditionalities: privatization mandates, austerity budgets, public sector reductions, currency devaluation, and removal of food/fuel subsidies. They must implement these policies to access new lending and debt relief, even when the policies deepen inequality and undermine public health. Exit options are severely limited: default risks catastrophic sanctions, capital flight, and exclusion from credit markets.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, non_strategic_debtor_states, payer,
    moderate, biographical, constrained, national).

% Ordinary citizens in non-strategic debtor countries who bear the welfare costs of enforced conditionalities: reduced healthcare and education budgets, higher user fees, job losses in public sectors, currency devaluation raising import costs, and elimination of price controls on food and fuel. They did not accumulate the debt (often inherited from prior regimes or incurred for prestige projects), cannot exit the country easily, and have no voice in the IMF/World Bank governance structures that imposed the policies.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, populations_of_policy_constrained_states, payer,
    powerless, biographical, trapped, national).

% Countries deemed strategically important to the hegemon (e.g., Turkey for NATO, Egypt for Suez, Poland for anti-Russian positioning) that receive conditionality waivers, policy flexibility, and rescue packages with minimal structural adjustment demands. They implement some surface-level reforms for optics but retain policy autonomy on the core issues—social spending, state-owned enterprises, exchange rates. They benefit from cheaper financing (risk premium is waived) and avoid the domestically costly adjustments imposed on non-strategic peers.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, geopolitically_strategic_debtor_states, beneficiary,
    powerful, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(structural_adjustment_conditionalities__hybrid_selectivity_reading, geopolitically_strategic_debtor_states, payer).

% Emerging creditors (China, regional development banks, Gulf sovereign wealth funds) that offer financing without conditionalities or with looser governance conditions. They are structurally excluded from setting the global conditionality regime because voting power in multilateral institutions is locked to G7/US control. If they gained influence, the selective enforcement mechanism would break down—so the Western creditor coalition works to delegitimize their lending as 'debt-trap diplomacy' (ironically, while conducting selective enforcement that is itself extractive).
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, alternative_creditor_sources, excluded,
    powerful, generational, trapped, global).

% NGOs, development economists, and civil society organizations that study and critique conditionality regimes. They document the unequal enforcement and call for policy change, but lack formal power over creditor institutions. Their analysis is sometimes incorporated into reform rhetoric but rarely changes enforcement practice.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, development_advocates, observer,
    organized, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(structural_adjustment_conditionalities__hybrid_selectivity_reading, hegemon_aligned_creditor_states).
narrative_ontology:fixing_cost_class(structural_adjustment_conditionalities__hybrid_selectivity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ostensibly, conditionalities coordinate a common problem: how to ensure that borrowing countries maintain fiscal discipline and market confidence so that they can access future lending and avoid default spirals. The technical framing presents conditionalities as neutral expertise—auditing government spending, privatizing inefficient public enterprises, removing price distortions—to align incentives between borrowers and lenders.
% TRANSFER_FUNCTION: Moves policy control from elected debtor governments to multilateral institutions and creditor states, and moves welfare costs (austerity, privatization revenue, user fees, currency devaluation rents) from creditors to debtor populations. The constraint also transfers geopolitical leverage: conditionality compliance becomes a mechanism for extracting foreign policy alignment and institutional access from debtor states. For strategic debtors, the transfer is dramatically reduced—they pay lip-service compliance while retaining policy autonomy.
% ABSENT_VOICES: Debtor-country labor unions, small farmers, healthcare workers, and school teachers are systematically excluded from loan negotiations and conditionality design. Their testimony would highlight the welfare costs of privatization and austerity. Alternative creditors (China, regional development banks) are excluded from the governance table and thus cannot propose competing conditionality frameworks. Non-strategic debtor governments themselves have minimal voice—they are presented with non-negotiable policy packages.
% DISAPPEARANCE_RATIONALE: If conditionalities vanished overnight, multilateral debt relief would become automatic; debtor governments would restore social spending without IMF approval; alternative creditors would become the default financing source; and the US/G7 leverage over non-strategic debtor policy would collapse. The geopolitical order would shift because conditionalities are the mechanism by which developed-country governments extract alignment from weaker states without explicit colonial administration.
% FOUNDING_PROBLEM: The founding problem (1980s–1990s) was real: Mexico, Brazil, and other major debtors faced acute illiquidity and sovereign default risk. The constraint was built to avoid cascading defaults that would damage creditor institutions and global financial stability. Conditionalities were justified as discipline to prevent fiscal profligacy and currency mismanagement.
% FOUNDING_PROBLEM_CORROBORATION: Creditor institutions and hegemon-aligned governments attest the founding problem is still live, citing ongoing fiscal risks and the need for market discipline. Debtor-country governments, development economists outside creditor institutions, and civil society organizations attest the founding problem is substantially solved (default risk has fallen, many debtors have positive credit ratings, and alternative financing sources exist) but the conditionality apparatus persists as a mechanism for extracting geopolitical compliance and protecting creditor claims. The 2008 financial crisis and subsequent development literature (IMF's own retrospectives, World Bank evaluations) acknowledge that conditionalities often *worsened* outcomes by deepening recessions and inequality—contradicting the stability rationale.
narrative_ontology:disappearance_verdict(structural_adjustment_conditionalities__hybrid_selectivity_reading, world_rearranges).
narrative_ontology:founding_problem_status(structural_adjustment_conditionalities__hybrid_selectivity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(structural_adjustment_conditionalities__hybrid_selectivity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(structural_adjustment_conditionalities__hybrid_selectivity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(structural_adjustment_conditionalities__hybrid_selectivity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(structural_adjustment_conditionalities__hybrid_selectivity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(structural_adjustment_conditionalities__hybrid_selectivity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is measured at 0.68 at interval end, reflecting the asymmetry between stated coordination function (prevent defaults, ensure macroeconomic stability) and actual distribution of costs/benefits (policy control extracted from debtors, welfare costs imposed on populations, geopolitical leverage captured by creditors). The extraction rises over the interval (0.45→0.68) because: (1) the initial 1980s–90s genuine default risk has faded, so the coordination rationale weakens and the extraction mechanism becomes more visible; (2) alternative creditor sources have grown, making the selectivity starker—strategic debtors can now point to China getting repaid without demanding privatization, exposing the choice to enforce on non-strategic debtors. Suppression is high (0.72) because persistence depends on active enforcement: (a) multilateral institutions rejecting disbursement if conditions are not met, (b) creditor states preventing conditionality waivers from being extended to non-strategic debtors, (c) delegitimization and exclusion of alternative creditors, (d) suppression of debtor-country resistance (protest is met with capital flight, credit-rating downgrades, new loan denial). Theater ratio is moderate-high (0.52) because substantial activity is genuinely technical (fiscal audits, spending reviews) but an increasing fraction is performative (the audits that approve waived conditions for strategic debtors are obviously not technical; the justification for privatization-vs.-waiver divergence is political, not economic; IMF and World Bank staff often acknowledge privately that the technical case for specific conditions on non-strategic debtors is weak but conditions are maintained for creditor-state political reasons). Accessibility collapse is moderate (0.61): non-strategic debtors genuinely have limited exits (default is severe, alternative financing from China requires political alignment with Beijing which conflicts with Western alignment, raising borrowing costs from alternative sources by rating-downgrade punishment), but the exits are not completely closed (some debtors have used alternative financing, some have restructured with creditors outside IMF mediation). Resistance is high (0.58): widespread protest in debtor countries, labor strikes, political opposition to privatization and austerity, academic and NGO critique of selectivity, and some debtor-government defiance (Argentina's repeated IMF conflicts, Malaysia's rejection of IMF conditions in 1997–98). The leveled coercion grid shows the multi-level structure: at the structural level (international credit regime), the constraint is most tightly enforced and least resisted (creditor states and multilateral institutions control the architecture; resistance is weak because it requires international coordination of debtors). At the organizational level (debtor government), enforcement is very tight (conditionality tied to disbursement) but resistance is higher (governments face domestic pressure and retain some negotiating leverage). At the class level (labor, farmers, public-sector workers who bear austerity), enforcement is diffuse (IMF does not police individual job losses) but resistance is highest (protests, strikes, electoral rejection). At the individual level, both enforcement and resistance are weaker—a single unemployed person cannot resist the constraint structure; organized collective action is required.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (creditor states and multilateral institutions) should compute this as coordination that they maintain through technical expertise and legitimate governance—they authorized the rules, they administer them professionally, and they have broad consensus among themselves and creditor elites. The payer seats (non-strategic debtor governments, affected populations) should compute this as enforced extraction: the rules are imposed without their consent, conditions are selective and politically motivated, enforcement is harsh while waivers flow to geopolitical allies, and the costs are catastrophic for populations. The beneficiary seats who also hold payer status (strategic debtors) compute this as modulated extraction: they pay some formal compliance costs but receive substantial waivers, cheaper access to credit, and geopolitical leverage that exceeds what they would have under equal enforcement. The analytical seat (development economists, human rights organizations) computes this as structural inequality: the mechanism is elegant (conditionalities appear technical, neutral, universally applicable) but the selectivity is systematic and geopolitical (not random noise)—it produces divergent outcomes for similar situations depending on each debtor's position in the geopolitical hierarchy.
 *
 * DIRECTIONALITY LOGIC:
 *   Hegemon-aligned creditor states have d near 0.0 (full beneficiary): they set the rules, decide who is exempt, extract geopolitical compliance from non-strategic debtors, and face no enforcement costs. Core multilateral institutions have d ~0.05–0.15 (beneficiary with some structural constraint): they gain institutional scope and fees but their governance is controlled by creditor states, so they have limited autonomy; they sometimes resist member-state pressure but cannot override it. Core private creditor banks have d ~0.1 (minor beneficiary cost): they benefit from enforcement but face some default risk on strategic debtors and some regulatory pressure from developed-country governments (US, EU) to provide relief to strategic allies. Non-strategic debtor states have d ~0.85–0.95 (near full target): they are forced to adopt policies they did not choose, lose policy control, bear welfare costs, and have minimal exit options. Populations of non-strategic debtors have d ~0.98 (extreme target): they did not incur the debt, have no voice in decisions, bear the full welfare costs, and are trapped (cannot migrate, cannot influence the constraint). Geopolitically strategic debtor states have d ~0.3–0.45 (moderate payer): they formally comply but receive waivers, retain policy autonomy, and can threaten to realign geopolitically if conditions tighten. Alternative creditors have d ~0.5 (symmetric/blocked): they want to participate in lending and benefit from repayment, but are excluded from governance and face delegitimization; they gain some business from non-strategic debtors but lose influence.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing default cascades in the 1980s debt crisis) was real and urgent. Conditionalities were justified as a discipline mechanism to prevent profligacy. However, the founding problem is now substantially resolved: (1) most debtor countries have positive sovereign credit ratings; (2) default risk has fallen dramatically; (3) alternative financing sources exist, reducing creditor monopoly; (4) many debtors have accumulated foreign reserves, reducing vulnerability. Despite this, conditionalities persist—and have actually expanded in scope. This is consistent with mandatrophy: the original function (preventing catastrophic cascading defaults) has been replaced by extraction and geopolitical leverage, but the institutional apparatus (IMF, World Bank conditionality procedures) persists and proliferates. The theater ratio (0.52) signals this atrophy: about half of conditionality activity is now performative (audits that rubberstamp geopolitically favored deviations, technical justifications for politically selective enforcement, institutional maintenance rituals that no longer serve the coordination function). The classification is Tangled Rope, not Snare, because genuine coordination problems remain (fiscal sustainability still matters, default contagion is still possible, some borrowers do face real fiscal constraints), and some debtors genuinely benefit from conditionalities (those with weak domestic institutions may gain credibility from external discipline). However, the selectivity—harsh enforcement on weak non-strategic debtors, waivers for strategic allies—means the constraint is not a pure rope solving a collective-action problem. It is tangled: the coordination cover story is real, but the extraction and leverage components now dominate, and the asymmetry is systematic and geopolitical, not random.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    selectivity_mechanism_irreducibility,
    'Is the observed selectivity of enforcement (harsh on non-strategic debtors, lenient on strategic allies) a result of (a) rational risk-adjusted lending by technically competent institutions, or (b) geopolitical power capture of the multilateral system by the US/G7, or (c) a hybrid where both mechanisms operate and are entangled?',
    'Comparative analysis of conditionality packages for debtors with similar fiscal indicators but different geopolitical positions (Egypt vs. Zambia, Poland vs. Peru, Turkey vs. Vietnam). If identical fiscal situations receive dramatically different treatment, selectivity is not technical. Interviews with IMF/World Bank staff about decision-making processes, pressure from shareholder governments, and internal debates about conditions. Access to internal documents on condition-setting would be decisive.',
    'If selectivity is purely technical, the constraint is closer to Tangled Rope (real coordination with distributional asymmetry due to capacity differences). If selectivity is purely geopolitical, the constraint is Snare (the coordination story is cover for extraction). If hybrid, the classification is Tangled Rope as authored, but the hybrid nature would require omegas documenting the entanglement—this omega is that documentation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selectivity_mechanism_irreducibility, empirical, 'Whether enforcement selectivity is technical risk-adjustment or geopolitical power capture (or both).').

omega_variable(
    coordination_function_persistence,
    'Does the coordination function (preventing default cascades, maintaining creditor confidence) remain genuinely necessary now that default risk has fallen and alternative creditors exist? Or has the constraint''s primary function shifted entirely to extraction and geopolitical leverage?',
    'Counterfactual analysis: if conditionalities were removed, would default cascades resume? Evidence: (1) do countries without IMF conditionalities face higher default risk than similar countries with IMF programs? (2) do creditor markets price in lower risk for countries under IMF programs, or is the premium already captured by geopolitical factors? (3) do episodes of successful debtor-creditor restructuring outside the IMF framework (Argentina 2005, Greece 2015 with Eurozone pressure but limited IMF enforcement) suggest the coordination function can be achieved differently?',
    'If coordination remains essential, Tangled Rope is correct. If the coordination function has been superseded by extraction and geopolitical leverage, the constraint should be reclassified as Snare with thin coordination cover. The answer likely varies by debtor (some low-capacity countries may still benefit from IMF discipline; most middle-income debtors can maintain fiscal discipline without it).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_persistence, empirical, 'Whether the coordination function remains essential or has been superseded by extraction.').

omega_variable(
    alternative_creditor_exclusion,
    'Is the exclusion of alternative creditors (China, regional development banks, Islamic development banks) from setting multilateral conditionality standards a technical governance choice based on development-finance expertise, or a strategic choice by the incumbent creditor coalition to protect its monopoly and its ability to conduct selective enforcement?',
    'Institutional analysis of voting power in multilateral banks (who votes, what voting thresholds are required to change standards, what happens when non-traditional creditors propose alternatives). Comparative analysis of conditionality frameworks offered by China, ADB, IsDB vs. IMF/World Bank (are they genuinely less demanding or is the criticism rhetorical?). Historical analysis of efforts by non-traditional creditors to gain voting power and G7 resistance to those efforts.',
    'If exclusion is technical (China and regional banks lack expertise in fiscal discipline), the constraint reflects legitimate governance hierarchy. If exclusion is strategic (keeping out competitors to preserve selectivity), the constraint is more extractive than the Tangled Rope classification suggests—it is an enforcement cartel protecting monopoly rents.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_creditor_exclusion, empirical, 'Whether alternative-creditor exclusion is technical or strategic.').

omega_variable(
    sibling_reading_reconciliation,
    'Can this hybrid-selectivity reading and the creditor-coordination reading coexist within a single analytical framework, or do their core premises about the SOURCE of selectivity logically foreclose each other?',
    'Formal comparison: creditor_coordination_reading claims selectivity is technical (risk-adjustment, debt capacity); hybrid_selectivity_reading claims selectivity is geopolitical. These are not merely different emphases—they attribute selectivity to different mechanisms. If selectivity is purely technical, geopolitical explanations are unnecessary. If selectivity is geopolitical, technical explanations are incomplete cover stories. The resolution depends on whether the observed selectivity can be fully explained by economic fundamentals (fiscal indicators, default risk, growth capacity) or if residual variance beyond economic factors correlates with geopolitical alignment.',
    'If they coexist (both mechanisms operate simultaneously), both readings remain holdable—this is a case of genuine pluralism. If one foreecloses the other (one set of mechanisms explains selectivity entirely, ruling out the alternative), the architecture of the constraint changes. If technical mechanisms fully explain selectivity, creditor_coordination_reading is more accurate and this reading should be reclassified as observing noise as signal. If geopolitical mechanisms fully explain selectivity, creditor_coordination_reading is foreclosed and this reading should be reclassified as the dominant reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_reconciliation, conceptual, 'Whether the two readings'' core premises logically coexist or foreclose each other.').

omega_variable(
    debtor_population_agency,
    'Populations of debtor countries are classified as powerless, trapped victims. But some debtor populations have mounted sustained resistance (strikes, protests, electoral rejection of IMF-aligned candidates). Does this resistance indicate the suppression metric (0.72) is overstated, or does it reflect the cost-asymmetry of resistance (resistance is possible but defeats are more probable than victories)?',
    'Case studies of sustained debtor-population resistance (Bolivia''s 2000 Water Wars, Ecuador''s IMF rejection in 2000–2002, Argentina''s 2001–2003 creditor default and restructuring outside IMF framework) vs. cases of ineffective resistance (most sub-Saharan African conditionality programs, Egypt, many Southeast Asian cases). What enables the former? What barriers prevent broader success?',
    'If resistance is genuinely effective in some cases, the constraint''s suppression may be lower than 0.72 (the distribution of coercive force is uneven, and some populations break free). If resistance is typically defeated or co-opted, the suppression metric is appropriate (populations can resist but at severe cost). This also affects the classification: if resistance is commonly successful, the constraint is less stable than Tangled Rope suggests, and may be better classified as Snare (extraction sustained by suppression rather than coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(debtor_population_agency, empirical, 'Whether measured suppression matches the actual distribution of resistance and its success rates.').

omega_variable(
    kernel_authority_grounding,
    'What grounds the authority of the conditionality kernel? Is it (a) the technical expertise of multilateral institutions (economists with credentials), (b) the lineage of post-WWII institutional frameworks (Bretton Woods authority passed through generations), (c) the extraction of geopolitical compliance by the hegemon (the US shapes rules to serve its interests), (d) the practice of creditor consensus (creditors collectively enforce the framework), or (e) distributed authority with no single grounding (different parties justify it differently)?',
    'Historical and institutional analysis: when did the creditor coalition form consensus around conditionality? Who proposed the framework? How has it changed? What happens when creditor consensus breaks (rare: China''s rise, the 2008 financial crisis, creditor divergence on Greece)? Do multilateral institutions resist creditor pressure, or defer to it? What legitimacy claims do they make publicly vs. what explains actual enforcement patterns?',
    'This determines the cs_structure.authority_grounding value and informs whether the constraint''s legitimacy is robust (expert/lineage) or contingent on power concentration (extraction). It affects classification: if authority is grounded in extraction and power concentration, the constraint is more fragile and more vulnerable to challenge as hegemons decline or alternative creditors rise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_authority_grounding, conceptual, 'What institutionally grounds the authority of the conditionality kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stru_tr_t0, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(stru_tr_t5, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 5, 0.39).
narrative_ontology:measurement(stru_tr_t10, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 10, 0.44).
narrative_ontology:measurement(stru_tr_t15, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 15, 0.48).
narrative_ontology:measurement(stru_tr_t20, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 20, 0.5).
narrative_ontology:measurement(stru_tr_t25, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 25, 0.51).
narrative_ontology:measurement(stru_tr_t30, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 30, 0.52).
narrative_ontology:measurement(stru_tr_t35, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 35, 0.52).
narrative_ontology:measurement(stru_tr_t40, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 40, 0.52).

% Extraction over time
narrative_ontology:measurement(stru_be_t0, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(stru_be_t5, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(stru_be_t10, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(stru_be_t15, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(stru_be_t20, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(stru_be_t25, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(stru_be_t30, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(stru_be_t35, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement(stru_be_t40, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(stru_su_t0, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(stru_su_t5, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(stru_su_t10, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(stru_su_t15, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 15, 0.69).
narrative_ontology:measurement(stru_su_t20, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(stru_su_t25, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement(stru_su_t30, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(stru_su_t35, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 35, 0.72).
narrative_ontology:measurement(stru_su_t40, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 40, 0.72).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=40
narrative_ontology:measurement(stru_grid_01, structural_adjustment_conditionalities__hybrid_selectivity_reading, accessibility_collapse(class), 0, 0.48).
narrative_ontology:measurement(stru_grid_02, structural_adjustment_conditionalities__hybrid_selectivity_reading, accessibility_collapse(class), 40, 0.52).
narrative_ontology:measurement(stru_grid_03, structural_adjustment_conditionalities__hybrid_selectivity_reading, accessibility_collapse(individual), 0, 0.35).
narrative_ontology:measurement(stru_grid_04, structural_adjustment_conditionalities__hybrid_selectivity_reading, accessibility_collapse(individual), 40, 0.38).
narrative_ontology:measurement(stru_grid_05, structural_adjustment_conditionalities__hybrid_selectivity_reading, accessibility_collapse(organizational), 0, 0.55).
narrative_ontology:measurement(stru_grid_06, structural_adjustment_conditionalities__hybrid_selectivity_reading, accessibility_collapse(organizational), 40, 0.68).
narrative_ontology:measurement(stru_grid_07, structural_adjustment_conditionalities__hybrid_selectivity_reading, accessibility_collapse(structural), 0, 0.72).
narrative_ontology:measurement(stru_grid_08, structural_adjustment_conditionalities__hybrid_selectivity_reading, accessibility_collapse(structural), 40, 0.78).
narrative_ontology:measurement(stru_grid_09, structural_adjustment_conditionalities__hybrid_selectivity_reading, resistance(class), 0, 0.72).
narrative_ontology:measurement(stru_grid_10, structural_adjustment_conditionalities__hybrid_selectivity_reading, resistance(class), 40, 0.68).
narrative_ontology:measurement(stru_grid_11, structural_adjustment_conditionalities__hybrid_selectivity_reading, resistance(individual), 0, 0.48).
narrative_ontology:measurement(stru_grid_12, structural_adjustment_conditionalities__hybrid_selectivity_reading, resistance(individual), 40, 0.44).
narrative_ontology:measurement(stru_grid_13, structural_adjustment_conditionalities__hybrid_selectivity_reading, resistance(organizational), 0, 0.58).
narrative_ontology:measurement(stru_grid_14, structural_adjustment_conditionalities__hybrid_selectivity_reading, resistance(organizational), 40, 0.56).
narrative_ontology:measurement(stru_grid_15, structural_adjustment_conditionalities__hybrid_selectivity_reading, resistance(structural), 0, 0.35).
narrative_ontology:measurement(stru_grid_16, structural_adjustment_conditionalities__hybrid_selectivity_reading, resistance(structural), 40, 0.32).
narrative_ontology:measurement(stru_grid_17, structural_adjustment_conditionalities__hybrid_selectivity_reading, stakes_inflation(class), 0, 0.58).
narrative_ontology:measurement(stru_grid_18, structural_adjustment_conditionalities__hybrid_selectivity_reading, stakes_inflation(class), 40, 0.62).
narrative_ontology:measurement(stru_grid_19, structural_adjustment_conditionalities__hybrid_selectivity_reading, stakes_inflation(individual), 0, 0.48).
narrative_ontology:measurement(stru_grid_20, structural_adjustment_conditionalities__hybrid_selectivity_reading, stakes_inflation(individual), 40, 0.52).
narrative_ontology:measurement(stru_grid_21, structural_adjustment_conditionalities__hybrid_selectivity_reading, stakes_inflation(organizational), 0, 0.62).
narrative_ontology:measurement(stru_grid_22, structural_adjustment_conditionalities__hybrid_selectivity_reading, stakes_inflation(organizational), 40, 0.73).
narrative_ontology:measurement(stru_grid_23, structural_adjustment_conditionalities__hybrid_selectivity_reading, stakes_inflation(structural), 0, 0.68).
narrative_ontology:measurement(stru_grid_24, structural_adjustment_conditionalities__hybrid_selectivity_reading, stakes_inflation(structural), 40, 0.71).
narrative_ontology:measurement(stru_grid_25, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression(class), 0, 0.58).
narrative_ontology:measurement(stru_grid_26, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression(class), 40, 0.62).
narrative_ontology:measurement(stru_grid_27, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression(individual), 0, 0.42).
narrative_ontology:measurement(stru_grid_28, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression(individual), 40, 0.46).
narrative_ontology:measurement(stru_grid_29, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression(organizational), 0, 0.72).
narrative_ontology:measurement(stru_grid_30, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression(organizational), 40, 0.78).
narrative_ontology:measurement(stru_grid_31, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression(structural), 0, 0.66).
narrative_ontology:measurement(stru_grid_32, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression(structural), 40, 0.74).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(structural_adjustment_conditionalities__hybrid_selectivity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0.12).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__hybrid_selectivity_reading, structural_adjustment_conditionalities__creditor_coordination_reading).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__hybrid_selectivity_reading, structural_adjustment_conditionalities__debtor_extraction_reading).

% DUAL FORMULATION NOTE:
% This story is one reading of the contested 'structural_adjustment_conditionalities' kernel. The creditor_coordination_reading frames conditionalities as neutral technical mechanisms for fiscal discipline. The debtor_extraction_reading frames them as neo-colonial extraction. This hybrid_selectivity_reading accepts elements of both: genuine coordination problems exist, but the solution is selectively enforced as geopolitical extraction. Each reading instantiates a different constraint with different ε values (creditor reading: low ε~0.25-0.35, coordination-focused; debtor reading: high ε~0.80-0.85, extraction-focused; hybrid reading: moderate-high ε~0.68, tangled). The three readings are linked via the network because they share a kernel and interpret it differently; each story's classification is computed from its own structural data and metrics independently of the siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(structural_adjustment_conditionalities__hybrid_selectivity_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
