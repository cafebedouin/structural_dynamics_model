% ============================================================================
% CONSTRAINT STORY: market_as_natural_default__beneficiary_maintained_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_as_natural_default__beneficiary_maintained_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: market_as_natural_default__beneficiary_maintained_reading
 *   human_readable: Market Naturalization via Post-Hoc Beneficiary Defense
 *   domain: political_economy/ideology
 *
 * SUMMARY:
 *   This reading instantiates ONE constraint from the contested kernel
 *   'market_as_natural_default'. The constraint models market naturalism as
 *   the result of active post-hoc defense by identifiable
 *   beneficiaries—primarily finance and multinational capital—who defend
 *   'there is no alternative' through institutional gatekeeping, intellectual
 *   capture, and suppression of competing frames. The kernel contest
 *   acknowledges three structurally distinct readings: (1)
 *   beneficiary-maintained (this one): naturalism is engineered and actively
 *   enforced; (2) hybrid-amnesia: initial closure created conditions for
 *   beneficiary capture; (3) lapsed-alternative: alternatives were simply
 *   forgotten, not suppressed. Each reading has a different ε (this one:
 *   0.48, moderate-to-high extraction) and different victim/suppression
 *   structure. The CLAIM/METRIC independence is deliberate: this reading is
 *   CLAIMED as tangled_rope (real coordination function in price signals +
 *   asymmetric extraction of political authority) while the metrics describe
 *   actively enforced suppression and rising theater as enforcement machinery
 *   substitutes justification.
 *
 * KEY AGENTS:
 *   - Financial sector institutions — institutional beneficiary, agenda-setter; maintains market naturalism via think-tank funding, policy advisory capture, regulatory influence
 *   - Multinational corporations — institutional beneficiary; benefits from framing that justifies labor arbitrage, capital mobility, deregulation
 *   - Neoliberal policy intellectuals — organized beneficiary; career advancement via defending market naturalism in canonical institutions
 *   - Alternative economic framers — moderate-power payer; suppressed via journal gatekeeping, conference exclusion, defunding
 *   - Labor constituency — powerless payer; bears costs of deregulation, wage suppression, capital mobility justified by naturalism
 *   - Public goods allocators — moderate-power payer; budgets constrained, legitimacy delegitimized by market-naturalism austerity framing
 *   - Historical memory keepers — excluded powerless agents; knowledge of non-market systems cordoned off as historical curiosity
 *   - Competitive regulatory jurisdictions — excluded institutional agents; penalized for attempting alternatives via capital flight and sanctions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_as_natural_default__beneficiary_maintained_reading, 0.48).
domain_priors:suppression_score(market_as_natural_default__beneficiary_maintained_reading, 0.62).
domain_priors:theater_ratio(market_as_natural_default__beneficiary_maintained_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(market_as_natural_default__beneficiary_maintained_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_as_natural_default__beneficiary_maintained_reading, tangled_rope).
narrative_ontology:human_readable(market_as_natural_default__beneficiary_maintained_reading, "Market Naturalization via Post-Hoc Beneficiary Defense").
narrative_ontology:topic_domain(market_as_natural_default__beneficiary_maintained_reading, "political_economy/ideology").

domain_priors:requires_active_enforcement(market_as_natural_default__beneficiary_maintained_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_as_natural_default__beneficiary_maintained_reading, 'd3979186-18a1-4bdb-931f-453810aaf658').
narrative_ontology:cs_kernel_codification('d3979186-18a1-4bdb-931f-453810aaf658', fixed_text).
narrative_ontology:cs_authority_grounding('d3979186-18a1-4bdb-931f-453810aaf658', extraction).
narrative_ontology:cs_interpretation_layer_present('d3979186-18a1-4bdb-931f-453810aaf658').
narrative_ontology:cs_reading_relation('d3979186-18a1-4bdb-931f-453810aaf658', market_as_natural_default__lapsed_alternative_reading, coexists_with).
narrative_ontology:cs_reading_relation('d3979186-18a1-4bdb-931f-453810aaf658', market_as_natural_default__hybrid_amnesia_reading, influences).
narrative_ontology:cs_axiom('d3979186-18a1-4bdb-931f-453810aaf658', foundational, market_naturalism_is_engineered).
narrative_ontology:cs_axiom_status(market_naturalism_is_engineered, holdable).
narrative_ontology:cs_axiom_grounding('d3979186-18a1-4bdb-931f-453810aaf658', market_naturalism_is_engineered, empirically_contingent).
narrative_ontology:cs_axiom('d3979186-18a1-4bdb-931f-453810aaf658', foundational, alternatives_are_actively_suppressed_not_forgotten).
narrative_ontology:cs_axiom_status(alternatives_are_actively_suppressed_not_forgotten, holdable).
narrative_ontology:cs_axiom_grounding('d3979186-18a1-4bdb-931f-453810aaf658', alternatives_are_actively_suppressed_not_forgotten, empirically_contingent).
narrative_ontology:cs_reference_frame('d3979186-18a1-4bdb-931f-453810aaf658', market_inevitability_doctrine).
narrative_ontology:cs_drift_state('d3979186-18a1-4bdb-931f-453810aaf658', contemporary_post_2008_crisis, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d3979186-18a1-4bdb-931f-453810aaf658', '').
narrative_ontology:cs_kernel_id(market_as_natural_default__beneficiary_maintained_reading, market_as_natural_default).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_as_natural_default__beneficiary_maintained_reading, financial_sector_institutions).
narrative_ontology:constraint_beneficiary(market_as_natural_default__beneficiary_maintained_reading, multinational_corporations).
narrative_ontology:constraint_beneficiary(market_as_natural_default__beneficiary_maintained_reading, neoliberal_policy_intellectuals).
narrative_ontology:constraint_victim(market_as_natural_default__beneficiary_maintained_reading, alternative_economic_framers).
narrative_ontology:constraint_victim(market_as_natural_default__beneficiary_maintained_reading, labor_constituency).
narrative_ontology:constraint_victim(market_as_natural_default__beneficiary_maintained_reading, public_goods_allocators).
narrative_ontology:constraint_victim(market_as_natural_default__beneficiary_maintained_reading, subnational_governance_experimenters).
narrative_ontology:constraint_vindicates(market_as_natural_default__beneficiary_maintained_reading, market_efficiency_hypothesis).
narrative_ontology:constraint_vindicates(market_as_natural_default__beneficiary_maintained_reading, spontaneous_order_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set regulatory norms, fund policy research institutes, and sponsor intellectual defenses of market allocation. They collect concentrated extraction via financial deregulation, asset price inflation, and reduced taxation. They frame market naturalism as inevitable and scientific to foreclose state-directed alternatives that would constrain finance.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, financial_sector_institutions, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(market_as_natural_default__beneficiary_maintained_reading, financial_sector_institutions, beneficiary).

% Benefit from global capital mobility, labor arbitrage, and trade regimes justified by market naturalism. They sponsor and amplify the 'there is no alternative' framing and fund think tanks that defend market supremacy. Their exit options include regulatory forum-shopping, offshore asset relocation, and lobbying capture.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, multinational_corporations, beneficiary,
    institutional, generational, arbitrage, global).

% Gain career advancement, publication prestige, and institutional funding through defending market naturalism. They produce journals, conferences, and canonical texts that entrench the idea that markets are natural and alternatives are naïve. Their exit is constrained by institutional investment in a market-first epistemic frame.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, neoliberal_policy_intellectuals, beneficiary,
    organized, biographical, constrained, global).

% Propose non-market allocation mechanisms (public banking, cooperative ownership, state enterprise, commons governance). They face systematic defunding, journal rejection, conference gatekeeping, and rhetorical dismissal as 'unrealistic' or 'proven to fail.' The constraint makes their voice illegible even when empirically grounded.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, alternative_economic_framers, payer,
    moderate, biographical, constrained, national).

% Bears the cost of labor market deregulation, wage suppression via capital mobility, and reduced public investment in services. They have limited power to challenge market naturalism directly; their only exit is individual mobility or unionization, both constrained by the constraint's enforcement machinery.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, labor_constituency, payer,
    powerless, biographical, trapped, global).

% Manage education, healthcare, infrastructure, and welfare systems. Market naturalism constrains their budgets (via austerity framing) and delegitimizes public provision as inherently inefficient. They carry the cost of underfunded public goods while market mechanisms capture rents.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, public_goods_allocators, payer,
    moderate, generational, constrained, national).

% Attempt local alternatives (municipal enterprise, cooperative finance, gift economies, commons management). They face capital flight, credit-rating downgrades, loss of tax base, and institutional pressure to 'align with market discipline.' The constraint's enforcement via capital markets and bond ratings makes exit from market naturalism prohibitively costly.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, subnational_governance_experimenters, payer,
    moderate, biographical, constrained, regional).

% Scholars, archivists, and oral historians who document pre-market economic systems, cooperative movements, guild systems, and indigenous allocation mechanisms. They are systematically excluded from policy-setting conversations; their knowledge is cordoned off as 'historical curiosity' rather than available alternative. Their identity is fused with documentation work that is delegitimized by market naturalism.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, historical_memory_keepers, excluded,
    powerless, biographical, identity_locked, national).

% Countries and subnational regions attempting non-market or hybrid economic experiments are penalized via capital flight, sanctions, and credit access denial. They are excluded from the conversation about whether alternatives work because the constraint's enforcement machinery (capital markets, IMF conditionality, trade law) ensures they do not succeed long enough to generate proof.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, competitive_regulatory_jurisdictions, excluded,
    institutional, generational, trapped, national).

% Comparative economic historians, anthropologists, and institutional economists who examine multiple allocation systems. They document that markets are contingent institutional arrangements, not natural defaults, but their findings are filtered through gatekeeping mechanisms that privilege market naturalism.
narrative_ontology:constraint_stakeholder(market_as_natural_default__beneficiary_maintained_reading, market_economy_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(market_as_natural_default__beneficiary_maintained_reading, financial_sector_institutions).
narrative_ontology:fixing_cost_class(market_as_natural_default__beneficiary_maintained_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Markets coordinate decentralized information about supply, demand, and price through price signals. Compared to centrally planned allocation, markets do solve a genuine coordination problem: how to transmit dispersed information without a command hierarchy. This coordination function is real.
% TRANSFER_FUNCTION: Moves political and epistemic authority from democratic deliberation and public debate to capital-markets logic and neoliberal policy consensus. Transfers wealth from labor and public goods to finance and multinational capital. Transfers voice from alternative framers to market-naturalism defenders via institutional gatekeeping (journal, conference, foundation funding, policy advisory capture).
% ABSENT_VOICES: Excluded are those who documented and managed non-market allocation systems: indigenous economies, cooperative movements, guild systems, public utility operators, command-economy practitioners (even where they worked), and contemporary experimenters with commons, mutual aid, and gift economies. They would attest that markets are one option among many, not a natural default. Also excluded: workers and communities bearing the costs of market deregulation who would contest the naturalism frame directly.
% DISAPPEARANCE_RATIONALE: If market naturalism as an actively defended constraint disappeared—if the institutional defense of 'there is no alternative' collapsed—alternatives would emerge immediately: public banking proposals would be debated on merit, cooperative models would receive funding, labor power would be treated as non-commodifiable, and carbon/environmental constraints would reshape allocation mechanisms. The economy would reorganize around multiple coordination modes rather than market-default reasoning.
% FOUNDING_PROBLEM: Post-WWII capitalism faced a legitimacy crisis: competing economic systems (Soviet command economy, social democracy, national developmentalism) offered alternatives to pure market allocation. Market naturalism was constructed to foreclose this competition by redefining capitalism as inevitable rather than chosen, removing it from democratic deliberation.
% FOUNDING_PROBLEM_CORROBORATION: Neoliberal architects and beneficiary class attests the founding problem is permanent: capitalism always faces ideological competitors and must continuously defend itself. Scholars of economic history (Polanyi, Foucault, Mirowski, Varoufakis), heterodox economists, and labor historians attest the founding problem was CONTINGENT on Cold War threat perception and was actively engineered, not inevitable. The beneficiary maintenance of market naturalism is documented in archives of the Mont Pelerin Society, think-tank funding genealogies, and textbook gatekeeping studies.
narrative_ontology:disappearance_verdict(market_as_natural_default__beneficiary_maintained_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_as_natural_default__beneficiary_maintained_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_as_natural_default__beneficiary_maintained_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(market_as_natural_default__beneficiary_maintained_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_as_natural_default__beneficiary_maintained_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(market_as_natural_default__beneficiary_maintained_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(market_as_natural_default__beneficiary_maintained_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48) is moderate-to-high because market naturalism enables financial deregulation, labor market liberalization, and tax avoidance justified by inevitability rhetoric. The extraction is not transparent (rents are justified as 'efficiency', not as transfer), but measurable: finance sector's share of GDP, CEO-to-median-worker pay ratios, and declining labor's share of national income track with the constraint's rise post-1975. Suppression (0.62) reflects active institutional gatekeeping: alternatives face systematic journal rejection, foundation defunding, policy-advisory exclusion, and rhetorical dismissal as 'unrealistic'. Theater (0.41) moderately high because enforcement activity increasingly defends the naturalism frame itself (conferences on 'market discipline', think tanks, economic textbooks) rather than empirical coordination. The measurement series run on one shared grid (1975–2025 in 10-year intervals); every metric is authored at every examined time point. The series show extractiveness and suppression both rising from 1975–2015 then plateauing 2015–2025, suggesting enforcement has saturated institutional capacity. Rising theater suggests enforcement machinery is increasingly performative rather than functional—the constraint is being defended, not because coordination requires it, but because beneficiaries require defense.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (finance, corporations, neoliberal intellectuals) experience the constraint as rope: a coordination mechanism they champion and defend, which produces both coordination benefits (price signals work) and incidental benefits (their profits rise). The target seats (alternative framers, labor, public goods allocators, memory keepers) experience the same constraint as snare: suppression enforced to keep them out of the conversation about alternatives. The engine should compute this divergence: a powerful institutional beneficiary with arbitrage exit should compute d near 0.0 and type toward rope; a powerless labor constituency with trapped exit and direct costs should compute d near 1.0 and type toward snare. The gap between beneficiary-seat perception (rope) and target-seat perception (snare) is exactly what seat-level classification should reveal. The claim (tangled_rope) reflects that BOTH perceptions are structurally true: the constraint solves a coordination problem (matching buyers and sellers via price) AND extracts political authority from democratic deliberation to capital-markets logic.
 *
 * DIRECTIONALITY LOGIC:
 *   Financial institutions sit at d near 1.0 (full target of extraction by the constraint's own operation) — wait, reverse: they sit at d near 0.0 (full beneficiary) because the constraint extracts from payers and transfers to them. Their power is institutional, exit is arbitrage (can forum-shop, relocate), time horizon generational (institutional permanence). Alternative framers sit at d near 1.0 (full target): they bear suppression, carry the costs of epistemic exclusion, have constrained exit (cannot exit from being excluded), and have biographical time horizon. Labor sits at d = 1.0 (fully targeted): trapped exit, powerless power, biographical horizon, directly bears costs via wage suppression and deregulation. Public goods allocators sit d = 0.8–0.9 (highly targeted): their budgets are constrained by austerity framing justified by market naturalism; they have constrained exit (cannot exit from governance responsibility); moderate power lets them push back somewhat but institutional pressure is immense. Historical memory keepers sit d near 1.0 but with identity_locked exit (their knowledge production is delegitimized by the constraint itself; exit would require rejecting their disciplinary identity). This divergence in d across seats is the engine's seat-level classification: from beneficiary seats the constraint computes as rope or light tangled_rope; from target seats it computes as snare or heavy tangled_rope. The authored claim (tangled_rope) reflects the structural reality that both functions operate: genuine coordination in price signals AND asymmetric extraction of epistemic/political authority.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Cold War ideological competition; need to defend capitalism against socialist and social-democratic alternatives) has PARTIALLY evolved. The Soviet Union collapsed (founding problem status: partly dead), but competitive alternatives (China's state-directed capitalism, Nordic social democracy, development-state models) persist (founding problem status: contested, not fully dead). Market naturalism persists beyond its founding justification because beneficiaries have captured the institutional apparatus that could revise it (think tanks, universities, policy advisory, central banks). This is the classic mandatrophy signature: the constraint's original justification has eroded but the constraint persists because institutional beneficiaries maintain it. The measurement showing theater rising while suppression plateaus suggests the constraint is increasingly maintained through rhetorical theater (conferences, textbooks, policy authority assertions) rather than substantive enforcement—exactly mandatrophy's trajectory. A genuine rope would show declining theater (the coordination function alone maintains it); a piton would show theater dominating suppression (pure performance). This constraint sits between: suppression is real (active gatekeeping), but theater is rising (enforcement becoming performative), suggesting the founding problem is dead but enforcement machinery persists and is increasingly theatrical. The constraint has NOT yet decayed to pure piton because suppression remains high and beneficiary class is still actively defending (theater, not yet abandoned). But the trajectory is toward piton-hood: if suppression continues to plateau while theater rises, enforcement apparatus becomes increasingly decorative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_specification,
    'This constraint instantiates ONE reading of the contested kernel ''market_as_natural_default''. Is the active post-hoc defense by beneficiaries the primary mechanism of naturalization, or is naturalization itself inevitable and beneficiary defense merely incidental?',
    'Historical documentation of beneficiary intent and resource allocation: Did institutional actors (Mont Pelerin Society, Chicago School networks, neoliberal think tanks) deliberately construct naturalism arguments, or did naturalism emerge spontaneously and beneficiaries merely amplified it? Archive analysis, oral histories from early neoliberal architects, and funding genealogies of key intellectuals.',
    'If beneficiary maintenance is primary, the constraint is a snare/tangled_rope with identifiable extraction. If naturalization is inevitable and beneficiary defense merely rides it, the constraint approaches mountain-hood (natural ideology) with beneficiary capture as secondary. The reading distinction determines whether alternatives are suppressed (beneficiary-maintained) or forgotten (lapsed-amnesia reading).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_specification, empirical, 'Whether market naturalism is actively engineered or spontaneously emergent.').

omega_variable(
    suppression_vs_forgetting,
    'Are alternatives actively suppressed through institutional gatekeeping (this reading), or have they been forgotten through historical amnesia and structural path-dependency (amnesia reading)?',
    'Institutional analysis: Do gatekeeping mechanisms actively exclude alternatives (journal desk-rejects, foundation defunding, conference selection, textbook revision)? Or do alternatives simply not emerge because institutional memory has eroded? Test via: (1) historical recovery of suppressed alternatives and measurement of resistance they face when brought forward; (2) comparison of alternatives'' access to publication, funding, and policy influence across time periods and jurisdictions.',
    'If suppression is active, the constraint is a snare/tangled_rope where alternatives could emerge if enforcement relaxed. If forgetting is primary, the constraint is more piton-like (institutional inertia maintains what was once constructed). The suppression measurement (0.62) reflects MY reading that active gatekeeping is substantial; the amnesia reading would author lower suppression and higher theater_ratio.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_vs_forgetting, empirical, 'Active suppression vs. institutional amnesia as the primary mechanism of closure.').

omega_variable(
    sibling_reading_coexistence,
    'Do the three readings of market naturalism coexist as competing framings in different institutional and intellectual circles, or does one reading foreclose the others within a single framework?',
    'Institutional topology: map which institutions, journals, policy networks, and intellectual traditions advance which reading. If institutions cleanly separate (finance-dominated institutions advance beneficiary-maintained; development institutions advance lapsed-alternatives; post-crisis critical theorists advance hybrid-amnesia), readings coexist. If one institution or figure holds contradictory commitments across readings, foreclosure is weaker.',
    'Coexistence (my judgment) means all three readings remain live and contestable. Foreclosure would mean one reading''s adoption logically rules out the others within any coherent framework. Coexistence supports the network model of constraint families where siblings link via affects_constraints; foreclosure would suggest a bifurcation or hierarchy of readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_coexistence, conceptual, 'Logical and institutional coexistence of the three readings.').

omega_variable(
    beneficiary_class_internal_divergence,
    'Do financial institutions, multinational corporations, and neoliberal intellectuals form a unified beneficiary class maintaining market naturalism, or do they have divergent interests that would produce different readings?',
    'Institutional conflict analysis: Do these three groups co-fund and co-advocate, or do they have competing agendas? Example: financiers may want capital mobility (beneficiary_maintained reading), while corporations want protections against financial volatility (alternative-leaning reading). Search historical record for internal debates, public disagreements, differential funding of competing intellectual positions.',
    'If unified, the beneficiary class is a single coordinated agenda-setter as modeled. If divergent, the stakeholder surface and directionality should split the beneficiary class into separate seats with different d values and potentially different computed types. Unified beneficiary class supports tangled_rope framing; divergent class might distribute between rope and snare readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_class_internal_divergence, empirical, 'Unity vs. divergence within the financial-corporate-intellectual beneficiary complex.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_as_natural_default__beneficiary_maintained_reading, 1975, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t1975, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 1975, 0.22).
narrative_ontology:measurement_basis(mark_tr_t1975, observed).
narrative_ontology:measurement(mark_tr_t1985, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 1985, 0.28).
narrative_ontology:measurement_basis(mark_tr_t1985, observed).
narrative_ontology:measurement(mark_tr_t1995, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 1995, 0.35).
narrative_ontology:measurement_basis(mark_tr_t1995, observed).
narrative_ontology:measurement(mark_tr_t2005, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 2005, 0.39).
narrative_ontology:measurement_basis(mark_tr_t2005, observed).
narrative_ontology:measurement(mark_tr_t2015, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 2015, 0.41).
narrative_ontology:measurement_basis(mark_tr_t2015, observed).
narrative_ontology:measurement(mark_tr_t2025, market_as_natural_default__beneficiary_maintained_reading, theater_ratio, 2025, 0.41).
narrative_ontology:measurement_basis(mark_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(mark_be_t1975, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 1975, 0.28).
narrative_ontology:measurement_basis(mark_be_t1975, observed).
narrative_ontology:measurement(mark_be_t1985, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 1985, 0.35).
narrative_ontology:measurement_basis(mark_be_t1985, observed).
narrative_ontology:measurement(mark_be_t1995, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 1995, 0.42).
narrative_ontology:measurement_basis(mark_be_t1995, observed).
narrative_ontology:measurement(mark_be_t2005, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 2005, 0.46).
narrative_ontology:measurement_basis(mark_be_t2005, observed).
narrative_ontology:measurement(mark_be_t2015, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 2015, 0.48).
narrative_ontology:measurement_basis(mark_be_t2015, observed).
narrative_ontology:measurement(mark_be_t2025, market_as_natural_default__beneficiary_maintained_reading, base_extractiveness, 2025, 0.48).
narrative_ontology:measurement_basis(mark_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(mark_su_t1975, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 1975, 0.35).
narrative_ontology:measurement_basis(mark_su_t1975, observed).
narrative_ontology:measurement(mark_su_t1985, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 1985, 0.45).
narrative_ontology:measurement_basis(mark_su_t1985, observed).
narrative_ontology:measurement(mark_su_t1995, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 1995, 0.53).
narrative_ontology:measurement_basis(mark_su_t1995, observed).
narrative_ontology:measurement(mark_su_t2005, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 2005, 0.6).
narrative_ontology:measurement_basis(mark_su_t2005, observed).
narrative_ontology:measurement(mark_su_t2015, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 2015, 0.62).
narrative_ontology:measurement_basis(mark_su_t2015, observed).
narrative_ontology:measurement(mark_su_t2025, market_as_natural_default__beneficiary_maintained_reading, suppression_requirement, 2025, 0.62).
narrative_ontology:measurement_basis(mark_su_t2025, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1975, tn=2025
narrative_ontology:measurement(mark_grid_01, market_as_natural_default__beneficiary_maintained_reading, accessibility_collapse(class), 1975, 0.48).
narrative_ontology:measurement(mark_grid_02, market_as_natural_default__beneficiary_maintained_reading, accessibility_collapse(class), 2025, 0.65).
narrative_ontology:measurement(mark_grid_03, market_as_natural_default__beneficiary_maintained_reading, accessibility_collapse(individual), 1975, 0.38).
narrative_ontology:measurement(mark_grid_04, market_as_natural_default__beneficiary_maintained_reading, accessibility_collapse(individual), 2025, 0.62).
narrative_ontology:measurement(mark_grid_05, market_as_natural_default__beneficiary_maintained_reading, accessibility_collapse(organizational), 1975, 0.52).
narrative_ontology:measurement(mark_grid_06, market_as_natural_default__beneficiary_maintained_reading, accessibility_collapse(organizational), 2025, 0.78).
narrative_ontology:measurement(mark_grid_07, market_as_natural_default__beneficiary_maintained_reading, accessibility_collapse(structural), 1975, 0.45).
narrative_ontology:measurement(mark_grid_08, market_as_natural_default__beneficiary_maintained_reading, accessibility_collapse(structural), 2025, 0.72).
narrative_ontology:measurement(mark_grid_09, market_as_natural_default__beneficiary_maintained_reading, resistance(class), 1975, 0.65).
narrative_ontology:measurement(mark_grid_10, market_as_natural_default__beneficiary_maintained_reading, resistance(class), 2025, 0.48).
narrative_ontology:measurement(mark_grid_11, market_as_natural_default__beneficiary_maintained_reading, resistance(individual), 1975, 0.52).
narrative_ontology:measurement(mark_grid_12, market_as_natural_default__beneficiary_maintained_reading, resistance(individual), 2025, 0.35).
narrative_ontology:measurement(mark_grid_13, market_as_natural_default__beneficiary_maintained_reading, resistance(organizational), 1975, 0.58).
narrative_ontology:measurement(mark_grid_14, market_as_natural_default__beneficiary_maintained_reading, resistance(organizational), 2025, 0.38).
narrative_ontology:measurement(mark_grid_15, market_as_natural_default__beneficiary_maintained_reading, resistance(structural), 1975, 0.62).
narrative_ontology:measurement(mark_grid_16, market_as_natural_default__beneficiary_maintained_reading, resistance(structural), 2025, 0.45).
narrative_ontology:measurement(mark_grid_17, market_as_natural_default__beneficiary_maintained_reading, stakes_inflation(class), 1975, 0.35).
narrative_ontology:measurement(mark_grid_18, market_as_natural_default__beneficiary_maintained_reading, stakes_inflation(class), 2025, 0.58).
narrative_ontology:measurement(mark_grid_19, market_as_natural_default__beneficiary_maintained_reading, stakes_inflation(individual), 1975, 0.28).
narrative_ontology:measurement(mark_grid_20, market_as_natural_default__beneficiary_maintained_reading, stakes_inflation(individual), 2025, 0.52).
narrative_ontology:measurement(mark_grid_21, market_as_natural_default__beneficiary_maintained_reading, stakes_inflation(organizational), 1975, 0.38).
narrative_ontology:measurement(mark_grid_22, market_as_natural_default__beneficiary_maintained_reading, stakes_inflation(organizational), 2025, 0.72).
narrative_ontology:measurement(mark_grid_23, market_as_natural_default__beneficiary_maintained_reading, stakes_inflation(structural), 1975, 0.42).
narrative_ontology:measurement(mark_grid_24, market_as_natural_default__beneficiary_maintained_reading, stakes_inflation(structural), 2025, 0.68).
narrative_ontology:measurement(mark_grid_25, market_as_natural_default__beneficiary_maintained_reading, suppression(class), 1975, 0.35).
narrative_ontology:measurement(mark_grid_26, market_as_natural_default__beneficiary_maintained_reading, suppression(class), 2025, 0.62).
narrative_ontology:measurement(mark_grid_27, market_as_natural_default__beneficiary_maintained_reading, suppression(individual), 1975, 0.28).
narrative_ontology:measurement(mark_grid_28, market_as_natural_default__beneficiary_maintained_reading, suppression(individual), 2025, 0.55).
narrative_ontology:measurement(mark_grid_29, market_as_natural_default__beneficiary_maintained_reading, suppression(organizational), 1975, 0.38).
narrative_ontology:measurement(mark_grid_30, market_as_natural_default__beneficiary_maintained_reading, suppression(organizational), 2025, 0.68).
narrative_ontology:measurement(mark_grid_31, market_as_natural_default__beneficiary_maintained_reading, suppression(structural), 1975, 0.32).
narrative_ontology:measurement(mark_grid_32, market_as_natural_default__beneficiary_maintained_reading, suppression(structural), 2025, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_as_natural_default__beneficiary_maintained_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(market_as_natural_default__beneficiary_maintained_reading, 0.18).
narrative_ontology:affects_constraint(market_as_natural_default__beneficiary_maintained_reading, market_as_natural_default__lapsed_alternative_reading).
narrative_ontology:affects_constraint(market_as_natural_default__beneficiary_maintained_reading, market_as_natural_default__hybrid_amnesia_reading).

% DUAL FORMULATION NOTE:
% The kernel 'market_as_natural_default' decomposes into three structurally distinct constraints, each with different ε and different mechanisms of closure. The 'beneficiary_maintained_reading' (this story) models market naturalism as actively engineered and defended by identifiable institutional beneficiaries through suppression and gatekeeping (ε=0.48). The 'lapsed_alternative_reading' models naturalism as emerging from historical amnesia—alternatives were forgotten, not suppressed (ε≈0.28–0.35). The 'hybrid_amnesia_reading' models initial amnesia creating conditions for subsequent beneficiary capture (ε≈0.38–0.42). The three readings coexist in different institutions and intellectual traditions; none logically forecloses the others within the framework of a single institution. They are linked by the fact that each reading cites the others as either partial explanations (acknowledging elements of truth in rivals) or as cover stories for the mechanism the reading emphasizes. All three are live readings of the kernel; the ε-invariance principle requires separate stories to model the structural distinctness.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(market_as_natural_default__beneficiary_maintained_reading, powerful, 0.15).
constraint_indexing:directionality_override(market_as_natural_default__beneficiary_maintained_reading, institutional, 0.08).
constraint_indexing:directionality_override(market_as_natural_default__beneficiary_maintained_reading, organized, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
