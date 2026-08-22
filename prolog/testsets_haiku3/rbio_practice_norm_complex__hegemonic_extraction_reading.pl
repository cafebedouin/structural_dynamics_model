% ============================================================================
% CONSTRAINT STORY: rbio_practice_norm_complex__hegemonic_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rbio_practice_norm_complex__hegemonic_extraction_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: rbio_practice_norm_complex__hegemonic_extraction_reading
 *   human_readable: RBIO Norms as Hegemonic Extraction (Frozen Amending Architecture)
 *   domain: international_law/political_economy
 *
 * SUMMARY:
 *   The Bretton Woods institutional complex and UN Security Council veto
 *   structure constitute a post-WWII framework for managing global finance,
 *   trade, and security. This reading instantiates the RBIO norms as a
 *   hegemonic extraction mechanism: the norms are formally universal and
 *   nominally revisable, but the P5 veto architecture and weighted voting in
 *   the IMF/World Bank ensure that amendments benefiting Global South states
 *   cannot pass, while selective enforcement protects Northern interests and
 *   violations while penalizing Global South deviations. The claim/metric gap
 *   is intentional: the constraint is CLAIMED as tangled rope (real
 *   coordination function, real beneficiaries, active enforcement) while the
 *   authored metrics reflect rising extractiveness over the interval and a
 *   high theater ratio, indicating that the performance of neutral
 *   multilateralism has grown as the actual extraction has intensified. This
 *   gap is where this reading diverges most sharply from the liberal
 *   institutional reading, which would author lower extractiveness and lower
 *   theater (calling it genuine coordination with legitimate capacity
 *   constraints).
 *
 * KEY AGENTS:
 *   - us_european_governments: Hold P5 veto and majority voting; set and enforce rules selectively (institutional power/arbitrage exit)
 *   - us_european_capital_interests: Benefit from market openings and policy space preservation under structural adjustment conditionality (institutional power/arbitrage exit)
 *   - global_south_states: Bear extraction through conditionality clauses, debt service, and policy space loss; must accept nominal terms but cannot amend them (organized power/constrained exit)
 *   - structurally_adjusted_populations: Experience wage suppression, austerity, privatization as downstream effects; lack standing to contest legitimacy (powerless/trapped)
 *   - institutional_rbio_authority_seat: The IMF, World Bank, UN apparatus operationalize the norms; their governance is controlled by Northern interests through veto and vote-weighting (institutional/arbitrage)
 *   - emerging_powers and non_aligned_states: Present in institutions but hold no veto; build alternative institutions rather than amend existing ones (powerful/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rbio_practice_norm_complex__hegemonic_extraction_reading, 0.78).
domain_priors:suppression_score(rbio_practice_norm_complex__hegemonic_extraction_reading, 0.71).
domain_priors:theater_ratio(rbio_practice_norm_complex__hegemonic_extraction_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__hegemonic_extraction_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rbio_practice_norm_complex__hegemonic_extraction_reading, tangled_rope).
narrative_ontology:human_readable(rbio_practice_norm_complex__hegemonic_extraction_reading, "RBIO Norms as Hegemonic Extraction (Frozen Amending Architecture)").
narrative_ontology:topic_domain(rbio_practice_norm_complex__hegemonic_extraction_reading, "international_law/political_economy").

domain_priors:requires_active_enforcement(rbio_practice_norm_complex__hegemonic_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rbio_practice_norm_complex__hegemonic_extraction_reading, '99d13ce3-2383-4492-883a-a3074584ca2e').
narrative_ontology:cs_kernel_codification('99d13ce3-2383-4492-883a-a3074584ca2e', formalized).
narrative_ontology:cs_authority_grounding('99d13ce3-2383-4492-883a-a3074584ca2e', extraction).
narrative_ontology:cs_interpretation_layer_present('99d13ce3-2383-4492-883a-a3074584ca2e').
narrative_ontology:cs_reading_relation('99d13ce3-2383-4492-883a-a3074584ca2e', rbio_practice_norm_complex__liberal_institutional_reading, forecloses).
narrative_ontology:cs_reading_relation('99d13ce3-2383-4492-883a-a3074584ca2e', rbio_practice_norm_complex__sovereignty_maximalist_reading, coexists_with).
narrative_ontology:cs_axiom('99d13ce3-2383-4492-883a-a3074584ca2e', foundational, amendment_procedure_frozen_by_p5_veto).
narrative_ontology:cs_axiom_status(amendment_procedure_frozen_by_p5_veto, holdable).
narrative_ontology:cs_axiom_grounding('99d13ce3-2383-4492-883a-a3074584ca2e', amendment_procedure_frozen_by_p5_veto, empirically_contingent).
narrative_ontology:cs_axiom('99d13ce3-2383-4492-883a-a3074584ca2e', foundational, selective_enforcement_reveals_hegemonic_intent).
narrative_ontology:cs_axiom_status(selective_enforcement_reveals_hegemonic_intent, holdable).
narrative_ontology:cs_axiom_grounding('99d13ce3-2383-4492-883a-a3074584ca2e', selective_enforcement_reveals_hegemonic_intent, empirically_contingent).
narrative_ontology:cs_reference_frame('99d13ce3-2383-4492-883a-a3074584ca2e', bretton_woods_consensus_settlement).
narrative_ontology:cs_drift_state('99d13ce3-2383-4492-883a-a3074584ca2e', contemporary_post_2008_crisis_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('99d13ce3-2383-4492-883a-a3074584ca2e', '2026-06-12T14:32:18Z').
narrative_ontology:cs_kernel_id(rbio_practice_norm_complex__hegemonic_extraction_reading, rbio_practice_norm_complex).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__hegemonic_extraction_reading, us_european_capital_interests).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__hegemonic_extraction_reading, institutional_rbio_authority_seat).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__hegemonic_extraction_reading, global_south_states).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__hegemonic_extraction_reading, structurally_adjusted_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__hegemonic_extraction_reading, humanitarian_intervention_advocates).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__hegemonic_extraction_reading, us_european_governments).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__hegemonic_extraction_reading, non_aligned_states_and_emerging_powers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from structural adjustment conditionality that opens markets in Global South jurisdictions to Northern capital, restricts labor and environmental regulation in those states, and locks in debt obligations serviced through primary commodity export. The terms are denominated in dollars; capital repatriation is guaranteed; currency devaluation falls on the debtor state. U.S. and European firms gain market access and cost advantages; Northern governments maintain policy space through the veto seat while Global South states lose it through conditionality clauses.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, us_european_capital_interests, beneficiary,
    institutional, generational, arbitrage, global).

% The Bretton Woods institutions (IMF, World Bank, GATT/WTO successor architecture) and UN Security Council P5 structure operationalize the norms. The IMF and World Bank enforce structural adjustment through loan conditionality and debt certification; the P5 seat enforces through selective authorization of state action and humanitarian intervention. The authority seat itself is embedded in Northern control: P5 veto is held by the U.S., UK, France (Western alignment post-1989), and former Soviet states; voting weights in the Bank and Fund privilege Northern economies. Amending the institutional rules requires supermajorities that Northern votes control or P5 unanimity that Northern veto blocks.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, institutional_rbio_authority_seat, agenda_setter,
    institutional, generational, arbitrage, global).

% Must accept structural adjustment conditionality to access financing for development and crisis response. Formally, the conditions are negotiated; practically, they are non-negotiable: a state that rejects them loses access to multilateral credit, faces rating downgrades that raise borrowing costs elsewhere, and becomes an outlier in a system where most peers have already accepted. The amending procedure requires their nominal consent, but the veto architecture ensures their consent cannot reach the threshold for change. They bear extraction through currency devaluation, labor market deregulation, public asset sales at fire-sale prices, and the debt service burden itself.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, global_south_states, payer,
    organized, generational, constrained, global).

% Experience the downstream effects of structural adjustment: wage suppression (labor market deregulation), cut public services (austerity clauses), currency-driven inflation (devaluation), and privatization of public goods. They bear the costs but did not consent and cannot exit the jurisdiction. The legitimacy claim—that the norms are consent-based and benefit development—reaches them as a foreign imposition attached to debt their government contracted. Resistance is localized; national coalitions form around land grabs, wage theft, and austerity, but lack the institutional standing to contest the norm's legitimacy in the forums where amendments are decided.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, structurally_adjusted_populations, payer,
    powerless, biographical, trapped, national).

% States like China, India, and BRICS members experience the norm complex as a binding constraint on development paths but retain some arbitrage room through alternative financing (Belt and Road, development banks outside the Bretton Woods structure). They lack veto power to amend the formal rules; their resistance takes the form of building parallel institutions rather than amending existing ones. They can attest the extractive structure but cannot enforce amendment within the RBIO framework itself.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, non_aligned_states_and_emerging_powers, payer,
    powerful, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(rbio_practice_norm_complex__hegemonic_extraction_reading, non_aligned_states_and_emerging_powers, observer).

% International NGOs, human rights bodies, and development advocates benefit from the humanitarian exception as an outlet for concern and intervention channels, even as they document the harms. They are partial beneficiaries of the norm's coordination function (the humanitarian exception creates space for monitoring and advocacy) while being structurally excluded from the enforcement seat (they cannot amend the norm or control when the exception applies). Their advocacy simultaneously legitimates the system and contests specific applications.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, humanitarian_intervention_advocates, beneficiary,
    organized, biographical, mobile, global).

% Independent economic research demonstrates that structural adjustment conditionality often worsens debt sustainability (pro-cyclical austerity deepens downturns) and fails on its own stated growth criteria. They can document the harm but lack institutional standing to force amendment or even to compel the institutions to acknowledge the evidence. Their role is epistemic witness; the enforcement machinery is indifferent to their findings.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, debt_sustainability_analysts_and_development_economists, observer,
    organized, biographical, mobile, global).

% Hold the P5 veto and voting majorities in the institutions; they set the formal rules and enforce them selectively. They benefit directly through capital openings and policy space preservation. They also maintain the capacity to amend the rules if consensus forms within the Northern coalition; that they do not signals that the current extraction is optimal from their perspective and that formal entrenchment is preferable to renegotiation.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__hegemonic_extraction_reading, us_european_governments, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(rbio_practice_norm_complex__hegemonic_extraction_reading, us_european_governments, beneficiary).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rbio_practice_norm_complex__hegemonic_extraction_reading, us_european_capital_interests).
narrative_ontology:fixing_cost_class(rbio_practice_norm_complex__hegemonic_extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A post-WWII institutional architecture for managing global finance, trade, and security: centralized rule-setting avoids repeated bilateral negotiation; common standards for credit-worthiness, sovereignty recognition, and use-of-force authorization reduce transaction costs; centralized dispute resolution provides enforcement mechanisms. The formal coordination claim is consent-based universalism: all states agree to the norms and benefit from their predictability.
% TRANSFER_FUNCTION: Moves sovereignty and policy space from Global South states to Northern capital and institutional authority seats. The mechanism: debt conditionality (formal contractual transfer of fiscal and labor policy to creditor-defined benchmarks); selective humanitarian intervention authority (Northern states retain unilateral use-of-force options while denying them to others); selective rule enforcement (violations by Northern states and their allies go unpunished; violations by outliers trigger sanctions or intervention). The currency of transfer is economic openness (market access to Global South labor and resources), debt service (currency flows North), and political submission (accepting the legitimacy of externally-imposed policy).
% ABSENT_VOICES: Global South populations subjected to structural adjustment are excluded from the amendment process entirely—they have no seat in the institutions where rules are set and no veto capacity. Emerging powers (China, India) are present as states but hold no veto and cannot force amendment; their exclusion from decision-making is formal (no P5 seat, minority voting weight). Labor movements, indigenous communities, and environmental constituencies in both North and South are excluded from the authoritative forums; they can advocate but cannot bind the outcome. The absent voices would argue that the norms are illegitimate because they were not freely consented to, were imposed through asymmetric power, and persist through selective enforcement rather than universal application.
% DISAPPEARANCE_RATIONALE: If the RBIO norm complex and its enforcement machinery vanished overnight, states would need to renegotiate bilateral trade, finance, and security arrangements; Global South states would immediately gain back fiscal and labor policy space (removing conditionality); Northern capital would lose the automatic market access and policy guarantees; emerging powers would reshape regional institutions around alternative rules; debt obligations might be renegotiated or repudiated. The institutional vacuum would be contested, but the status quo distribution of sovereignty and policy space would not persist.
% FOUNDING_PROBLEM: Post-WWII great-power coordination: how to manage global finance and trade without repeated major-power war; how to allow weaker states enough autonomy to prevent destabilizing resentment while constraining their policy choices to prevent default cascades or communist contagion. The liberal institutional reading emphasizes the coordination function; the hegemonic extraction reading emphasizes that the formal solution was designed to lock in the post-war distribution of power.
% FOUNDING_PROBLEM_CORROBORATION: The liberal institutional community (IMF economists, World Bank staff, mainstream development scholars) attests the problem is still live and the norms solve it through consensus and consent. Historical scholarship outside the benefiting parties (dependency theorists, postcolonial scholars, critical political economy) attests the problem was always about great-power dominance, not coordination, and the 'consensus' was constructed through unequal power. Structural adjustment survivors in Latin America, Africa, and Asia attest the foundational problem was regime-change prevention and capital market opening, not development per se; the problem persists as long as Northern veto power blocks amendment.
narrative_ontology:disappearance_verdict(rbio_practice_norm_complex__hegemonic_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(rbio_practice_norm_complex__hegemonic_extraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rbio_practice_norm_complex__hegemonic_extraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(rbio_practice_norm_complex__hegemonic_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rbio_practice_norm_complex__hegemonic_extraction_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rbio_practice_norm_complex__hegemonic_extraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rbio_practice_norm_complex__hegemonic_extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rbio_practice_norm_complex__hegemonic_extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness starts at 0.52 (the founding era when the coordination function was still genuine and extractiveness was moderate) and accumulates to 0.78 (contemporary, when the coordination function has been largely achieved and the norms persist primarily as a transfer mechanism). The theater ratio rises from 0.38 to 0.62, indicating that an increasing share of institutional activity is devoted to maintaining the legitimacy story (consensus-building rituals, development rhetoric, humanitarian exceptions) rather than solving the foundational coordination problem. Suppression rises from 0.54 to 0.71, reflecting intensifying enforcement machinery against Global South deviations (escalating sanctions regimes, IMF conditionality tightening, humanitarian intervention selectivity) while enforcement against Northern violations remains theatrically absent. Accessibility collapse (0.58) is moderate because Global South states retain formal exit options (they can refuse IMF programs) but face prohibitive costs (capital flight, debt cascades, rating downgrades). Resistance (0.68) is substantial because independent economic scholarship, Global South populations, and emerging powers actively contest the norms' legitimacy, even as the veto architecture blocks amendment. The measurement series on a shared time grid: every metric authored at every time point (0, 8, 16, 24, 32 years into the contemporary interval), showing monotonic intensification of extraction and performance.
 *
 * PERSPECTIVAL GAP:
 *   From the institutional authority seat and Northern beneficiary seats, the constraint computes as weak tangled rope or even rope: genuine coordination problem was solved (post-war great-power management, capital market confidence), active enforcement maintains stable rules, beneficiaries and payers can identify themselves coherently. From the Global South state seat, especially the structurally-adjusted population seat, the constraint computes as snare: the coordination benefit is not felt locally (austerity deepens instead of alleviating crisis); the enforcement is asymmetric (conditionality imposed on them, violations by others ignored); exit is theoretically available but practically impossible; the beneficiaries are specifically Northern interests, not a universal set. From the emerging-power seat, the constraint computes as constrained tangled rope with no amendment path: coordination function exists but is asymmetrically distributed; they accept the rules to access the system but cannot change them and must build alternatives outside. The engine computes per-seat classification from power/exit data; the authored structural differences ensure the divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   US/European governments and their capital interests are full beneficiaries (d ≈ 0.1-0.2): the constraint preserves their policy space, protects their capital, and amplifies their votes through institutional control. Global South states are moderate targets (d ≈ 0.65-0.75): they bear conditionality, debt service, and policy constraints, but retain nominal voice (they sit in the institutions, can refuse programs, can build alternatives). Structurally-adjusted populations are full targets (d ≈ 0.9): they bear costs (wage suppression, austerity) with no voice or exit. Emerging powers are asymmetric (d ≈ 0.55): they benefit from institutional rules (trade, security) but lose policy space on their own terms; they are partial beneficiaries through arbitrage (they can build alternatives) but constrained targets on the amendment question. The high d values for Global South populations and states reflect the high suppression (0.71) and constrained exit (most Global South states have trapped or constrained exit from the RBIO system; emerging powers have constrained moving to arbitrage). The directionality logic feeds the effective extraction computation: beneficiaries with d near 0 see subsidized effective extraction (negative χ); targets with d near 1.0 see amplified effective extraction; institutional actors with arbitrage exit see modulated extraction across power and scope.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-war great-power coordination, preventing default cascades) was substantially solved by 1975 (the norms stabilized major-power relations, capital flows reached predictability, development finance became routinized). By 2000, the founding problem was dead as a driver of the norm complex: post-Cold War cooperation meant the coordination challenge was solved; capital markets matured; Global South states developed sufficient voice (through non-aligned movements, regional banks) to renegotiate. The constraint persists not to solve the founding problem but because its persistence benefits the institutional authority seat and Northern interests; no coalition has formed with sufficient power to amend it. The theater ratio (0.62) reflects the performance of consensus-building and development rhetoric that must accompany the constraint once its original function atrophies. Mandatrophy is not 'resolved' (the constraint is not in collapse) but is actively performed over: the institutions continuously narrate the founding problem as still live (development challenges, financial stability risks) to justify why amendment is not needed. This reading would declare mandatrophy_resolved = false (the constraint has not yet reached collapse or acknowledged dysfunction) but would author omegas flagging the founding-problem-is-dead scenario as a likely endpoint. If Global South states successfully build functioning alternatives (Belt and Road, regional development banks), the RBIO constraint will face true mandatrophy: its founding function is obsolete, amendment is blocked by veto, so it will either atrophy (reduced to ceremonial role) or collapse (bypassed by exit).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consensus_constructed_vs_authentic,
    'Is the apparent consent of Global South states to RBIO norms an authentic, voluntary commitment, or a coerced acceptance masked as consensus because exit options are systematically blocked?',
    'Counterfactual: states that withdrew from the RBIO system (Venezuela, Bolivia, pre-2014 Russia) did so at great economic cost and faced coordinated sanctions/exclusion; their exit trajectory shows that ''consensus'' persists only because the cost of exit is prohibitive. Survey data from policy elites in Global South states on the constraint experience (recorded in leaked cables, private testimony, development policy retrospectives) reveals coercion framing, not authentic agreement.',
    'If consent is constructed through coercion rather than authentic preference revelation, the constraint is snare-class rather than rope-class; the coordination function is cover for extraction; the ''revisability'' of the norms is meaningless because the amendment procedure is itself controlled by the extractive beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consensus_constructed_vs_authentic, empirical, 'Whether consensus is authentic or coercion-constructed.').

omega_variable(
    selective_enforcement_mechanism,
    'Is the selective enforcement of RBIO norms (sanctions on Global South violators, immunity for Northern violators) a capacity problem or a design feature of hegemonic extraction?',
    'Comparative enforcement analysis: document that the institutions COULD enforce against Northern states (legal authority exists) but choose not to; contrast this with aggressive enforcement against Global South states even when capacity to comply is impossible (e.g., austerity conditionality during pandemic). If the choice is systematic rather than case-by-case, it is a design feature.',
    'If selective enforcement is a design feature, it confirms that the norms function as hegemonic extraction rather than universal coordination; the norm complex itself is a snare, not a tangled rope; amending individual rules without amending the veto architecture will not change the fundamental extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(selective_enforcement_mechanism, empirical, 'Selectivity as design vs. capacity artifact.').

omega_variable(
    alternative_development_path_viability,
    'Could Global South states have achieved comparable development outcomes following non-RBIO paths (heterodox policy, capital controls, state-led industrialization) without the extraction costs they incurred under structural adjustment?',
    'Historical counterfactual analysis comparing post-1980 outcomes in states that pursued RBIO orthodoxy vs. those that deviated (South Korea pre-1997, Malaysia, Vietnam''s early transition, China''s managed integration). If deviation-path states achieved better outcomes, the RBIO norms are not necessary for development; the extraction is not a cost of coordination but a transfer to Northern interests.',
    'If alternative paths were viable, the claim that RBIO norms solve a development coordination problem collapses; they solve only a capital market coordination problem that benefits Northern finance. The constraint shifts from tangled rope (with a real coordination function for development) to snare (where coordination is a cover for extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_development_path_viability, empirical, 'Whether RBIO norms were necessary for development or merely optimal for Northern capital.').

omega_variable(
    reading_decomposition_kernel_contest,
    'This constraint is one reading of a contested kernel (rbio_practice_norm_complex). What distinguishes the hegemonic_extraction_reading from sibling readings (liberal_institutional_reading, sovereignty_maximalist_reading)?',
    'The sibling readings instantiate the constraint differently: liberal_institutional reads the norms as universal consent-based rules with a capacity-limited enforcement problem; sovereignty_maximalist reads them as intrusions on state autonomy regardless of coordination benefit. This reading reads them as formally universal but substantively reserved for Northern benefit through P5 veto architecture and selective enforcement. The distinguishing claims are: (1) the amendment procedure is practically unresponsive to Global South preference (frozen by veto); (2) enforcement selectivity reveals hegemonic intent, not capacity constraint; (3) the beneficiary set is specifically Northern capital and allied governments, not a universal set of states.',
    'If this reading is correct, the liberal reading''s empirical claims are false (norms are not genuinely revisable; enforcement selectivity is not a capacity problem); the sovereignty reading''s normative rejection is incomplete (it correctly identifies intrusion but misses the capital interest that drives it). Classification divergence: from the liberal seat, rope or weak tangled rope (coordination with legitimate procedures); from this seat, snare (extraction dressed as coordination with pseudo-revisable rules).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_decomposition_kernel_contest, conceptual, 'Kernel-reading structural divergence: how this reading''s core claims differentiate it from siblings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rbio_practice_norm_complex__hegemonic_extraction_reading, 0, 32).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rbio_tr_t0, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement_basis(rbio_tr_t0, observed).
narrative_ontology:measurement(rbio_tr_t8, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 8, 0.44).
narrative_ontology:measurement_basis(rbio_tr_t8, observed).
narrative_ontology:measurement(rbio_tr_t16, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 16, 0.51).
narrative_ontology:measurement_basis(rbio_tr_t16, observed).
narrative_ontology:measurement(rbio_tr_t24, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 24, 0.57).
narrative_ontology:measurement_basis(rbio_tr_t24, observed).
narrative_ontology:measurement(rbio_tr_t32, rbio_practice_norm_complex__hegemonic_extraction_reading, theater_ratio, 32, 0.62).
narrative_ontology:measurement_basis(rbio_tr_t32, observed).

% Extraction over time
narrative_ontology:measurement(rbio_be_t0, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(rbio_be_t0, observed).
narrative_ontology:measurement(rbio_be_t8, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 8, 0.58).
narrative_ontology:measurement_basis(rbio_be_t8, observed).
narrative_ontology:measurement(rbio_be_t16, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 16, 0.65).
narrative_ontology:measurement_basis(rbio_be_t16, observed).
narrative_ontology:measurement(rbio_be_t24, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 24, 0.72).
narrative_ontology:measurement_basis(rbio_be_t24, observed).
narrative_ontology:measurement(rbio_be_t32, rbio_practice_norm_complex__hegemonic_extraction_reading, base_extractiveness, 32, 0.78).
narrative_ontology:measurement_basis(rbio_be_t32, observed).

% Suppression requirement over time
narrative_ontology:measurement(rbio_su_t0, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 0, 0.54).
narrative_ontology:measurement_basis(rbio_su_t0, observed).
narrative_ontology:measurement(rbio_su_t8, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 8, 0.59).
narrative_ontology:measurement_basis(rbio_su_t8, observed).
narrative_ontology:measurement(rbio_su_t16, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 16, 0.64).
narrative_ontology:measurement_basis(rbio_su_t16, observed).
narrative_ontology:measurement(rbio_su_t24, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 24, 0.68).
narrative_ontology:measurement_basis(rbio_su_t24, observed).
narrative_ontology:measurement(rbio_su_t32, rbio_practice_norm_complex__hegemonic_extraction_reading, suppression_requirement, 32, 0.71).
narrative_ontology:measurement_basis(rbio_su_t32, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rbio_practice_norm_complex__hegemonic_extraction_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(rbio_practice_norm_complex__hegemonic_extraction_reading, 0.22).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__hegemonic_extraction_reading, global_debt_architecture).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__hegemonic_extraction_reading, humanitarian_intervention_selectivity).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__hegemonic_extraction_reading, development_policy_conditionality).

% DUAL FORMULATION NOTE:
% This constraint decomposes into three structural stories linked by ε-invariance principle: (1) resource_allocation coordination (global finance and trade rules) — separable from (2) extraction through selective enforcement — separable from (3) legitimacy claim through consensus framing. Each has different ε, different victim/beneficiary sets, different amendment pathways. This hegemonic_extraction_reading focuses on the extraction function and its selective enforcement; the liberal_institutional_reading focuses on coordination; sovereignty_maximalist focuses on the legitimacy claim itself. Linked via affects_constraints to distinguish which aspects of the RBIO architecture each reading emphasizes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rbio_practice_norm_complex__hegemonic_extraction_reading, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
