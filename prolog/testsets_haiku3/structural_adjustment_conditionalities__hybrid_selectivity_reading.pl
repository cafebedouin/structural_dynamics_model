% ============================================================================
% CONSTRAINT STORY: structural_adjustment_conditionalities__hybrid_selectivity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: structural_adjustment_conditionalities__hybrid_selectivity_reading
 *   human_readable: Selective Structural Adjustment Conditionalities
 *   domain: international_political_economy/development_finance
 *
 * SUMMARY:
 *   Structural adjustment conditionalities imposed by the IMF on debtor
 *   states are ostensibly coordination mechanisms ensuring fiscal discipline
 *   and market confidence across the international credit system. This
 *   reading—the hybrid_selectivity_reading—instantiates these
 *   conditionalities as selectively applied discipline: enforcement is harsh
 *   and comprehensive on geopolitically non-aligned or developmentally
 *   trapped states, while waivers and reduced enforcement apply
 *   systematically to strategic debtors whose geopolitical alignment with
 *   core powers makes their compliance less essential. The enforcement
 *   selectivity is asymmetrically hidden: non-aligned debtors receive public
 *   conditionality requirements; aligned debtors receive private or implicit
 *   waivers. The constraint operates as Tangled Rope: a genuine coordination
 *   function (preventing creditor chaos) is real, but its persistence and
 *   structure depend critically on asymmetric extraction from a subset of
 *   debtors determined by geopolitical position rather than fiscal condition.
 *
 * KEY AGENTS:
 *   - IMF creditor bloc: sets conditionality terms, determines enforcement selectivity based on geopolitical alignment, maintains discretion over waiver decisions
 *   - Core-aligned states: receive de facto enforcement waivers; experience coordination frame while escaping extraction
 *   - Non-aligned debtor states: bear full conditionality force; enforcement determines fiscal and social policy; exit options cut off by creditor pressure
 *   - Developmentally trapped states: locked into debt cycles reinforced by conditionality; institutional identity fused with IMF oversight role
 *   - Labor-dependent populations: bear distributed extraction costs through wage suppression and public-sector cuts; no seat at negotiation table
 *   - Multinational creditors: benefit from conditionality-enforced asset acquisition and debt prioritization
 *   - Heterodox economists and critics: excluded from IMF policy deliberations despite empirical critiques of conditionality effectiveness
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0.78).
domain_priors:suppression_score(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0.82).
domain_priors:theater_ratio(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__hybrid_selectivity_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(structural_adjustment_conditionalities__hybrid_selectivity_reading, tangled_rope).
narrative_ontology:human_readable(structural_adjustment_conditionalities__hybrid_selectivity_reading, "Selective Structural Adjustment Conditionalities").
narrative_ontology:topic_domain(structural_adjustment_conditionalities__hybrid_selectivity_reading, "international_political_economy/development_finance").

domain_priors:requires_active_enforcement(structural_adjustment_conditionalities__hybrid_selectivity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(structural_adjustment_conditionalities__hybrid_selectivity_reading, '38348adf-3509-4c14-84ed-a0588ea71466').
narrative_ontology:cs_kernel_codification('38348adf-3509-4c14-84ed-a0588ea71466', fixed_text).
narrative_ontology:cs_authority_grounding('38348adf-3509-4c14-84ed-a0588ea71466', extraction).
narrative_ontology:cs_interpretation_layer_present('38348adf-3509-4c14-84ed-a0588ea71466').
narrative_ontology:cs_reading_relation('38348adf-3509-4c14-84ed-a0588ea71466', structural_adjustment_conditionalities__creditor_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('38348adf-3509-4c14-84ed-a0588ea71466', structural_adjustment_conditionalities__debtor_extraction_reading, coexists_with).
narrative_ontology:cs_axiom('38348adf-3509-4c14-84ed-a0588ea71466', foundational, enforcement_selectivity_reveals_extraction).
narrative_ontology:cs_axiom_status(enforcement_selectivity_reveals_extraction, holdable).
narrative_ontology:cs_axiom_grounding('38348adf-3509-4c14-84ed-a0588ea71466', enforcement_selectivity_reveals_extraction, empirically_contingent).
narrative_ontology:cs_axiom('38348adf-3509-4c14-84ed-a0588ea71466', foundational, geopolitical_alignment_determines_conditionality_cost).
narrative_ontology:cs_axiom_status(geopolitical_alignment_determines_conditionality_cost, holdable).
narrative_ontology:cs_axiom_grounding('38348adf-3509-4c14-84ed-a0588ea71466', geopolitical_alignment_determines_conditionality_cost, empirically_contingent).
narrative_ontology:cs_reference_frame('38348adf-3509-4c14-84ed-a0588ea71466', imf_neutral_technical_authority).
narrative_ontology:cs_drift_state('38348adf-3509-4c14-84ed-a0588ea71466', contemporary_empirical_challenge_phase, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('38348adf-3509-4c14-84ed-a0588ea71466', '').
narrative_ontology:cs_kernel_id(structural_adjustment_conditionalities__hybrid_selectivity_reading, structural_adjustment_conditionalities).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__hybrid_selectivity_reading, imf_creditor_bloc).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__hybrid_selectivity_reading, core_aligned_states).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__hybrid_selectivity_reading, multinational_creditors).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__hybrid_selectivity_reading, non_aligned_debtor_states).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__hybrid_selectivity_reading, developmentally_trapped_states).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__hybrid_selectivity_reading, labor_dependent_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__hybrid_selectivity_reading, core_aligned_states).
narrative_ontology:constraint_vindicates(structural_adjustment_conditionalities__hybrid_selectivity_reading, fiscal_discipline_doctrine).
narrative_ontology:constraint_vindicates(structural_adjustment_conditionalities__hybrid_selectivity_reading, market_confidence_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets conditionality terms on development finance, enforces audit frameworks, determines which states qualify for debt relief. Justifies strictness as fiscal discipline necessary for market confidence and long-term sustainability. Maintains discretion to waive enforcement for geopolitically significant borrowers without formal transparency about waiver criteria. Collects compliance rent through extended repayment timelines and institutional dependency. Has ability to move enforcement selectivity and choose which debtors receive waivers based on alignment and strategic interest.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, imf_creditor_bloc, agenda_setter,
    institutional, generational, arbitrage, global).

% Receive de facto enforcement waivers: their conditionality requirements are selectively unenforced when they conflict with geopolitical strategy or when the state's alignment with core powers is confirmed. They experience the coordination frame (fiscal discipline language) while escaping most of the extraction (conditionality enforcement). Their domestic constituencies bear diffuse costs from selective enforcement against their geopolitical rivals. They have access to alternative financing sources when IMF conditionality is waived; their exit is mobile.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, core_aligned_states, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(structural_adjustment_conditionalities__hybrid_selectivity_reading, core_aligned_states, payer).

% Bear the full force of conditionality enforcement: structural adjustment requirements, austerity mandates, asset privatization, trade liberalization, labor-standard suppression. Their non-alignment or strategic irrelevance makes them enforcement priorities for the IMF bloc—their compliance demonstrates creditor commitment and their pain signals the cost of non-alignment. Exit options are extremely constrained: bilateral escape routes are cut off by creditor bloc pressure; unilateral default is treated as regime-delegitimizing and faces sanctions. Their political leaders face domestic pressure to resist conditionality while external pressure makes resistance costly.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, non_aligned_debtor_states, payer,
    moderate, generational, constrained, national).

% Are locked into debt trajectories by commodity dependence, colonial-era infrastructure, and prior conditionality rounds that weakened institutional capacity. Conditionality enforcement reinforces the trap: privatization of public assets to meet IMF benchmarks transfers productive capacity to foreign buyers at distressed valuations; labor suppression mandates disable the domestic wage-driven consumption that would diversify the economy. Institutional identity as 'reform state' under IMF oversight is fused with the state apparatus itself—political survival requires performing compliance even when conditionalities deepen the trap. The state's own development specialists are trained through IMF programs and internalize the constraint's logic.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, developmentally_trapped_states, payer,
    powerless, biographical, identity_locked, national).

% Bear the distributed costs of conditionality enforcement: wage suppression mandates, public-sector layoffs, healthcare and education cutbacks, removal of price controls on essentials. These populations have no seat at the negotiation table and no exit from the territory. Suppression of wage organizing and labor voice is often explicitly mandated as labor-market flexibility. The constraint operates as extraction mediated through the state apparatus, with populations absorbing the shock while creditors and multinational firms benefit. Geographic immobility and identity-fusion with the nation-state make exit impossible.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, labor_dependent_populations, payer,
    powerless, biographical, trapped, local).

% Private creditors embedded in the IMF's enforcement machinery through structural adjustment conditions that mandate debt repayment prioritization, competitive tendering favoring foreign contractors, and asset sales at fire-sale prices. They benefit from conditionality-enforced demand for imported goods and services, and from acquisition of state enterprises at depressed valuations post-adjustment. Their interests are formally distinct from the IMF bloc but structurally aligned through the conditionality machinery. They have arbitrage exit: can shift lending and investment to alternative markets if conditionality enforcement changes.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, multinational_creditors, beneficiary,
    institutional, generational, arbitrage, global).

% Are systematically excluded from IMF policy deliberations despite publishing empirical critiques of conditionality effectiveness. Their alternative frameworks (state-led development, infant-industry protection, capital controls as stabilizers) are treated as non-serious within creditor-bloc institutional discourse. Their exclusion is maintained by academic gatekeeping and the IMF's structural insulation from external accountability. They have constrained exit: can publish outside IMF circles but face career costs for challenging the dominant framework.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, heterodox_economists_and_critics, excluded,
    organized, generational, constrained, global).

% Appropriate aid budgets that flow through IMF conditionality frameworks without directly controlling the terms. They rely on IMF technical framing to justify aid effectiveness to domestic constituencies; this structural dependence on IMF legitimacy creates pressure toward non-interference with conditionality design. Their exit is constrained by the institutional separation between legislative appropriation and IMF technical authority.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__hybrid_selectivity_reading, donor_country_legislatures, observer,
    institutional, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(structural_adjustment_conditionalities__hybrid_selectivity_reading, imf_creditor_bloc).
narrative_ontology:fixing_cost_class(structural_adjustment_conditionalities__hybrid_selectivity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates fiscal discipline across a fragmented international credit system: sets common standards for debt repayment, currency reserves, budget deficits, and trade balances to prevent individual debtor default from cascading systemic contagion. Ostensibly solves the collective-action problem of creditor coordination and debtor moral hazard.
% TRANSFER_FUNCTION: Moves fiscal control (the authority to set domestic spending, wage, and price structures) from elected debtor governments to the IMF bloc, and transfers productive assets (state enterprises, natural resources) from debtor states to foreign capital at depressed valuations; moves labor income from workers to creditors via mandated wage suppression and labor-market liberalization; moves electoral sovereignty from local populations to creditor-set conditionality benchmarks.
% ABSENT_VOICES: Heterodox economists with alternative frameworks for development finance; labor organizations in debtor states; elected representatives of debtor populations (explicitly excluded from conditionality design); domestic constituencies in creditor states who bear diffuse costs of foreign-policy-driven conditionality waiving and the asset acquisition it enables; development-sector workers in debtor states whose salaries are cut under conditionality austerity mandates.
% DISAPPEARANCE_RATIONALE: If conditional structural adjustment enforcement vanished, debtor states would recover domestic fiscal authority and labor protections. Creditors would lose extracted rent from asset acquisition and extended repayment. Geopolitical alignment would cease to predict enforcement selectivity. The architecture of post-colonial economic dependence would require renegotiation. Multinational firms would lose conditionality-facilitated access to state enterprises. The disappearance would fundamentally alter who sets economic policy across the Global South and would shift leverage from creditors to elected debtor governments.
% FOUNDING_PROBLEM: Late-1970s debt crisis: poor debtor-state lending practices combined with commodity-price collapse created sovereign default risk; creditor banks faced insolvency if major borrowers could not service debt; the international financial system lacked mechanisms to coordinate debtor restructuring without cascading defaults that would trigger global financial contagion.
% FOUNDING_PROBLEM_CORROBORATION: IMF and core creditor states attest the founding problem remains live, citing ongoing moral hazard and fiscal indiscipline. Debtor state governments, academic development economists (outside IMF-aligned institutions), and international civil society organizations attest the founding problem was substantially solved by 1990s but conditionality persists as extractive architecture. They cite two decades of empirical data showing conditionality in non-aligned debtors correlated with SLOWER growth and persistent debt traps, versus faster recovery in states that escaped conditionality or received geopolitical waivers. Independent research outside the IMF institutional bloc (World Bank heterodox research, academic development economics, bilateral donor evaluations) confirms the divergence between conditionality's stated function and documented outcomes.
narrative_ontology:disappearance_verdict(structural_adjustment_conditionalities__hybrid_selectivity_reading, world_rearranges).
narrative_ontology:founding_problem_status(structural_adjustment_conditionalities__hybrid_selectivity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(structural_adjustment_conditionalities__hybrid_selectivity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(structural_adjustment_conditionalities__hybrid_selectivity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness measures at 0.78 at interval end because the constraint systematically transfers fiscal control, productive assets, and labor income from debtor states to creditors and aligned powers, with the transfer mechanism hidden behind coordination language. Suppression is high (0.82) because enforcement depends on cutting off exit options (bilateral alternatives, unilateral default) and suppressing domestic resistance to conditionality terms through labor-market mandates and institutional dependency. Theater_ratio climbs from 0.38 to 0.52 over the interval because the gap between stated coordination function and actual extraction mechanism widens as empirical evidence accumulates that conditionality in non-aligned debtors correlates with slower growth and persistent debt traps, forcing more rhetorical work to maintain the coordination frame. The selective enforcement architecture is the key: if conditionality were applied uniformly by fiscal condition, extractiveness would be substantially lower and the constraint would be more defensible as coordination. The fact that enforcement selectivity tracks geopolitical alignment rather than fiscal need is the structural marker that the arrangement extracts more from some debtors than necessary for coordination. Accessibility_collapse is high (0.68) for non-aligned debtors because conditionality requirements occupy the entire space of domestic policy options; private escape routes are foreclosed by creditor pressure on alternative lending sources. For core-aligned states, collapse is much lower (~0.35, coded in the class-level grid) because alternative financing sources remain available when alignment protects them. Resistance starts high (0.71) but declines to 0.62 over the interval as debtor states exhaust domestic political capacity to resist; labor movements are suppressed by conditionality mandates; heterodox policy coalitions are marginalized by IMF institutional dominance in economic discourse.
 *
 * PERSPECTIVAL GAP:
 *   The IMF creditor bloc and core-aligned states experience this constraint as genuine coordination mechanism—setting common standards for a fragmented credit system to prevent contagion. From their seats, the constraint is Rope with minor extraction. Non-aligned and trapped debtors experience the same constraint as enforced extraction—fiscal control transferred to external institutions, productive assets sold at fire-sale valuations, labor suppressed via mandates. From their seats, the constraint is Snare. The engine should compute this divergence directly from the structural data: IMF bloc has high power, arbitrage-grade exit options (can exit enforcement by choosing different lending terms for strategic debtors), beneficiary role → d near 0.0. Non-aligned debtors have moderate power, constrained exit options (creditor pressure forecloses bilateral alternatives), payer role → d near 1.0. This same divergence is also visible across institutional vs. individual levels: organizational resistance in debtor states (labor unions, development councils) is suppressed by conditionality mandates, leaving only individual-level resistance, which is then attributed to 'lack of ownership' rather than to the constraint's design.
 *
 * DIRECTIONALITY LOGIC:
 *   The IMF creditor bloc collects enforcement authority and discretion—they set terms for others and decide when to enforce them. Core-aligned states inherit that discretion indirectly by receiving waivers; their d shifts from what it would be under uniform enforcement toward the beneficiary end. Non-aligned debtors face the full asymmetry: they pay via fiscal control, asset transfer, and labor suppression, with no participation in waiver decisions. Their d is highest (near 1.0). Labor-dependent populations are trapped (no exit, highest suppression, identity-locked into state dependency they cannot renegotiate), so their d is maximal among payers. The IMF bloc's arbitrage exit (they can move enforcement selectivity if core alignment changes) keeps their d at the beneficiary floor. The constraint's persistence depends critically on this directionality structure: if conditionality were enforced uniformly by fiscal condition, non-aligned debtors would have more symmetric d, and either the coordination case would be clearer or the extraction would be more visible and harder to defend.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (1970s debt crisis, need for creditor coordination) was substantially solved by the 1990s; the debt crisis architecture has been restructured, multilateral lending has dispersed, and alternative financing sources exist. Yet conditionality persists and has expanded. The constraint's claimed function (prevent contagion through coordinated standards) now operates alongside a second function: maintain creditor leverage over debtor states for geopolitical purposes and to enforce asset privatization favorable to foreign capital. The finding_problem_status is 'contested' precisely because IMF bloc attests the founding problem is live (moral hazard, fiscal indiscipline) while debtor states and independent economists attest it is substantially solved but conditionality persists as extractive architecture. This is the classic mandatrophy signature: the arrangement outlived its founding problem and now persists for reasons (geopolitical leverage, asset extraction, institutional inertia) disconnected from the original coordination function. The theater_ratio climb (0.38→0.52) is symptomatic: more IMF discourse dedicates itself to defending conditionality's effectiveness and necessity against mounting empirical evidence of its failure in non-aligned contexts, forcing the coordination frame to do more rhetorical work.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    selective_enforcement_opacity,
    'Is the IMF''s selective waiving of conditionality requirements for aligned debtors a deliberate, strategically conscious decision, or an emergent consequence of institutional incentives and power asymmetries?',
    'Declassification of IMF Executive Board meeting minutes; internal memos revealing enforcement decision criteria; testimony from IMF staff on how alignment considerations factor into conditionality design decisions; comparison of treatment across debtors with identical fiscal conditions but different geopolitical positions.',
    'If deliberate and strategic, the constraint is intentionally structured extraction with coordination as cover—Snare. If emergent, the arrangement is Tangled Rope where coordination and extraction co-constitute the constraint''s operation. The distinction affects whether remediation requires constraint elimination or structural modification of decision-making processes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selective_enforcement_opacity, empirical, 'Whether selective enforcement is conscious strategy or institutional emergent property.').

omega_variable(
    coordination_necessity_boundary,
    'Would the international credit system experience meaningful contagion without IMF conditionality, or would alternative coordination mechanisms (peer creditor pressure, market mechanisms, alternative institutions) suffice?',
    'Historical comparison of debt crisis periods before IMF-led conditionality (1950s-1970s); examination of creditor outcomes in countries that escaped conditionality or experienced alternate financing; controlled comparison of default rates and contagion in conditioned vs. non-conditioned debtor cohorts adjusting for selection effects.',
    'If contagion would occur without conditionality, the coordination function is necessary and the constraint is genuine Tangled Rope. If contagion is prevented by markets or alternative mechanisms, conditionality''s persistence is indefensible as coordination and the constraint reclassifies toward Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_necessity_boundary, empirical, 'Whether IMF conditionality is structurally necessary for coordination or substitutable.').

omega_variable(
    extraction_vs_market_correction,
    'Do mandated asset privatizations under conditionality transfer productive capacity to more efficient foreign owners (market correction improving long-term growth), or do they transfer undervalued assets to foreign capital while impairing domestic capacity?',
    'Long-term growth trajectories post-privatization; sectoral productivity data; wage and employment outcomes; comparison of privatized vs. state-managed enterprises in equivalent contexts; international audit of fire-sale valuations against post-acquisition valuations for the same assets.',
    'If market correction, extractiveness should be re-measured as lower (part of the measured extraction is growth investment). If asset transfer to capital, extractiveness is confirmed; the constraint operates as pure extraction hidden as market discipline.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_market_correction, empirical, 'Whether conditionality-mandated asset sales represent market correction or value extraction.').

omega_variable(
    kernel_contest_framing_asymmetry,
    'Why is the creditor_coordination_reading treated as the default/legitimate reading within IMF institutional discourse, while the debtor_extraction_reading and this hybrid_selectivity_reading are marginalized as ''critical'' or ''unserious''?',
    'Institutional analysis of IMF publication practices, appointment processes, and funding flows; documentation of whose critiques are cited in IMF policy papers; comparison of treatment of coordination vs. extraction framings across IMF external research, academic partnerships, and staff training.',
    'The asymmetry is structural: the beneficiary reading is institutionalized as legitimate while victim readings are marginalized. This is not evidence that the coordination reading is true—it is evidence that the constraint''s persistence depends on epistemic closure, not on empirical superiority of the coordination frame. Recognizing this would shift the analysis from whether conditionality is good to whether the reading hierarchy itself is extractive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_contest_framing_asymmetry, conceptual, 'Institutional mechanisms maintaining creditor reading hegemony.').

omega_variable(
    conditionality_kernel_reading_divergence,
    'Is selective conditionality enforcement a deviation from a unified, coordinative kernel, or is the kernel itself internally structured to enable selective application?',
    'Analysis of IMF founding documents and bylaws: do they explicitly authorize discretion in conditionality application based on creditor preferences? If yes, the kernel is internally selective and the selective readings coexist as legitimate interpretations of the stabilized text. If no, selective enforcement is deviation and the sibling readings foreclose each other.',
    'If the kernel is internally selective (written to enable discretion), all three readings are equiprioritized and none forecloses; the contest is about which reading captures the actual operation. If the kernel is unitarily coordinative and selective enforcement is deviation, then the extraction reading and hybrid reading foreclose the pure coordination reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conditionality_kernel_reading_divergence, conceptual, 'Whether selectivity is reading-level or embedded in the kernel itself.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stru_tr_t0, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement_basis(stru_tr_t0, observed).
narrative_ontology:measurement(stru_tr_t5, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 5, 0.42).
narrative_ontology:measurement_basis(stru_tr_t5, observed).
narrative_ontology:measurement(stru_tr_t10, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 10, 0.45).
narrative_ontology:measurement_basis(stru_tr_t10, observed).
narrative_ontology:measurement(stru_tr_t15, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 15, 0.48).
narrative_ontology:measurement_basis(stru_tr_t15, observed).
narrative_ontology:measurement(stru_tr_t20, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 20, 0.5).
narrative_ontology:measurement_basis(stru_tr_t20, observed).
narrative_ontology:measurement(stru_tr_t25, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 25, 0.51).
narrative_ontology:measurement_basis(stru_tr_t25, observed).
narrative_ontology:measurement(stru_tr_t30, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 30, 0.52).
narrative_ontology:measurement_basis(stru_tr_t30, observed).
narrative_ontology:measurement(stru_tr_t40, structural_adjustment_conditionalities__hybrid_selectivity_reading, theater_ratio, 40, 0.52).
narrative_ontology:measurement_basis(stru_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(stru_be_t0, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement_basis(stru_be_t0, observed).
narrative_ontology:measurement(stru_be_t5, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 5, 0.67).
narrative_ontology:measurement_basis(stru_be_t5, observed).
narrative_ontology:measurement(stru_be_t10, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 10, 0.71).
narrative_ontology:measurement_basis(stru_be_t10, observed).
narrative_ontology:measurement(stru_be_t15, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 15, 0.74).
narrative_ontology:measurement_basis(stru_be_t15, observed).
narrative_ontology:measurement(stru_be_t20, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 20, 0.76).
narrative_ontology:measurement_basis(stru_be_t20, observed).
narrative_ontology:measurement(stru_be_t25, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 25, 0.77).
narrative_ontology:measurement_basis(stru_be_t25, observed).
narrative_ontology:measurement(stru_be_t30, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 30, 0.78).
narrative_ontology:measurement_basis(stru_be_t30, observed).
narrative_ontology:measurement(stru_be_t40, structural_adjustment_conditionalities__hybrid_selectivity_reading, base_extractiveness, 40, 0.78).
narrative_ontology:measurement_basis(stru_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(stru_su_t0, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 0, 0.71).
narrative_ontology:measurement_basis(stru_su_t0, observed).
narrative_ontology:measurement(stru_su_t5, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 5, 0.74).
narrative_ontology:measurement_basis(stru_su_t5, observed).
narrative_ontology:measurement(stru_su_t10, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 10, 0.76).
narrative_ontology:measurement_basis(stru_su_t10, observed).
narrative_ontology:measurement(stru_su_t15, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 15, 0.78).
narrative_ontology:measurement_basis(stru_su_t15, observed).
narrative_ontology:measurement(stru_su_t20, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 20, 0.8).
narrative_ontology:measurement_basis(stru_su_t20, observed).
narrative_ontology:measurement(stru_su_t25, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 25, 0.81).
narrative_ontology:measurement_basis(stru_su_t25, observed).
narrative_ontology:measurement(stru_su_t30, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 30, 0.82).
narrative_ontology:measurement_basis(stru_su_t30, observed).
narrative_ontology:measurement(stru_su_t40, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression_requirement, 40, 0.82).
narrative_ontology:measurement_basis(stru_su_t40, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=40
narrative_ontology:measurement(stru_grid_01, structural_adjustment_conditionalities__hybrid_selectivity_reading, accessibility_collapse(class), 0, 0.58).
narrative_ontology:measurement(stru_grid_02, structural_adjustment_conditionalities__hybrid_selectivity_reading, accessibility_collapse(class), 40, 0.62).
narrative_ontology:measurement(stru_grid_03, structural_adjustment_conditionalities__hybrid_selectivity_reading, accessibility_collapse(individual), 0, 0.82).
narrative_ontology:measurement(stru_grid_04, structural_adjustment_conditionalities__hybrid_selectivity_reading, accessibility_collapse(individual), 40, 0.85).
narrative_ontology:measurement(stru_grid_05, structural_adjustment_conditionalities__hybrid_selectivity_reading, accessibility_collapse(organizational), 0, 0.71).
narrative_ontology:measurement(stru_grid_06, structural_adjustment_conditionalities__hybrid_selectivity_reading, accessibility_collapse(organizational), 40, 0.74).
narrative_ontology:measurement(stru_grid_07, structural_adjustment_conditionalities__hybrid_selectivity_reading, accessibility_collapse(structural), 0, 0.45).
narrative_ontology:measurement(stru_grid_08, structural_adjustment_conditionalities__hybrid_selectivity_reading, accessibility_collapse(structural), 40, 0.48).
narrative_ontology:measurement(stru_grid_09, structural_adjustment_conditionalities__hybrid_selectivity_reading, resistance(class), 0, 0.68).
narrative_ontology:measurement(stru_grid_10, structural_adjustment_conditionalities__hybrid_selectivity_reading, resistance(class), 40, 0.62).
narrative_ontology:measurement(stru_grid_11, structural_adjustment_conditionalities__hybrid_selectivity_reading, resistance(individual), 0, 0.74).
narrative_ontology:measurement(stru_grid_12, structural_adjustment_conditionalities__hybrid_selectivity_reading, resistance(individual), 40, 0.68).
narrative_ontology:measurement(stru_grid_13, structural_adjustment_conditionalities__hybrid_selectivity_reading, resistance(organizational), 0, 0.72).
narrative_ontology:measurement(stru_grid_14, structural_adjustment_conditionalities__hybrid_selectivity_reading, resistance(organizational), 40, 0.69).
narrative_ontology:measurement(stru_grid_15, structural_adjustment_conditionalities__hybrid_selectivity_reading, resistance(structural), 0, 0.52).
narrative_ontology:measurement(stru_grid_16, structural_adjustment_conditionalities__hybrid_selectivity_reading, resistance(structural), 40, 0.48).
narrative_ontology:measurement(stru_grid_17, structural_adjustment_conditionalities__hybrid_selectivity_reading, stakes_inflation(class), 0, 0.64).
narrative_ontology:measurement(stru_grid_18, structural_adjustment_conditionalities__hybrid_selectivity_reading, stakes_inflation(class), 40, 0.68).
narrative_ontology:measurement(stru_grid_19, structural_adjustment_conditionalities__hybrid_selectivity_reading, stakes_inflation(individual), 0, 0.88).
narrative_ontology:measurement(stru_grid_20, structural_adjustment_conditionalities__hybrid_selectivity_reading, stakes_inflation(individual), 40, 0.91).
narrative_ontology:measurement(stru_grid_21, structural_adjustment_conditionalities__hybrid_selectivity_reading, stakes_inflation(organizational), 0, 0.76).
narrative_ontology:measurement(stru_grid_22, structural_adjustment_conditionalities__hybrid_selectivity_reading, stakes_inflation(organizational), 40, 0.79).
narrative_ontology:measurement(stru_grid_23, structural_adjustment_conditionalities__hybrid_selectivity_reading, stakes_inflation(structural), 0, 0.52).
narrative_ontology:measurement(stru_grid_24, structural_adjustment_conditionalities__hybrid_selectivity_reading, stakes_inflation(structural), 40, 0.55).
narrative_ontology:measurement(stru_grid_25, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression(class), 0, 0.61).
narrative_ontology:measurement(stru_grid_26, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression(class), 40, 0.64).
narrative_ontology:measurement(stru_grid_27, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression(individual), 0, 0.83).
narrative_ontology:measurement(stru_grid_28, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression(individual), 40, 0.86).
narrative_ontology:measurement(stru_grid_29, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression(organizational), 0, 0.68).
narrative_ontology:measurement(stru_grid_30, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression(organizational), 40, 0.71).
narrative_ontology:measurement(stru_grid_31, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression(structural), 0, 0.48).
narrative_ontology:measurement(stru_grid_32, structural_adjustment_conditionalities__hybrid_selectivity_reading, suppression(structural), 40, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(structural_adjustment_conditionalities__hybrid_selectivity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(structural_adjustment_conditionalities__hybrid_selectivity_reading, 0.18).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__hybrid_selectivity_reading, structural_adjustment_conditionalities__creditor_coordination_reading).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__hybrid_selectivity_reading, structural_adjustment_conditionalities__debtor_extraction_reading).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__hybrid_selectivity_reading, sovereign_debt_restructuring_framework).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__hybrid_selectivity_reading, imf_conditionality_enforcement_asymmetry).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'structural_adjustment_conditionalities'. The sibling readings instantiate different constraint types from the same kernel: creditor_coordination_reading (Rope, low extraction, genuine coordination) and debtor_extraction_reading (Snare, high extraction, pure coercion). This reading (hybrid_selectivity_reading) decomposes the coordination vs. extraction binary by recognizing that enforcement selectivity based on geopolitical alignment makes the constraint simultaneously coordinative for some seats (core-aligned debtors) and extractive for others (non-aligned debtors). The family structure is: creditor_coordination_reading ← (foundational, most consensual, pure coordination frame) | hybrid_selectivity_reading ← (intermediate, documenting selective enforcement mechanism) | debtor_extraction_reading ← (most contested, extraction-only frame). All three readings share the same referent (IMF conditionality framework) and the same ε measurement axis (asymmetry and selectivity), but author different ε values reflecting their reading's assessment of whether the observed selectivity is bug (deviation) or feature (enabling mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(structural_adjustment_conditionalities__hybrid_selectivity_reading, institutional, 0.15).
constraint_indexing:directionality_override(structural_adjustment_conditionalities__hybrid_selectivity_reading, moderate, 0.92).
constraint_indexing:directionality_override(structural_adjustment_conditionalities__hybrid_selectivity_reading, powerless, 0.98).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
