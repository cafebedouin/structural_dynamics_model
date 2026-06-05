% ============================================================================
% CONSTRAINT STORY: bretton_woods_treaty_substrate__neoliberal_convertibility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bretton_woods_treaty_substrate__neoliberal_convertibility, []).

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
 *   constraint_id: bretton_woods_treaty_substrate__neoliberal_convertibility
 *   human_readable: Bretton Woods Treaty Substrate: Neoliberal Convertibility Reading
 *   domain: international_political_economy/monetary_institutions
 *
 * SUMMARY:
 *   The Bretton Woods institutional apparatus created a global monetary order
 *   ostensibly designed to enable reconstruction and trade after WWII. This
 *   constraint story represents ONE READING of that contested institutional
 *   kernel: the neoliberal convertibility interpretation, which holds that
 *   free capital movement and currency convertibility are the essential
 *   features that enable international commerce. Under this reading, national
 *   capital controls are violations of the regime's core purpose, and policy
 *   autonomy becomes a victim of the system's extractive logic. This is
 *   distinct from Keynes' original embedded liberalism reading (capital
 *   controls permitted to protect domestic policy space) and from the
 *   sovereignty defense reading (the regime is fundamentally illegitimate
 *   coercion). The neoliberal reading treats Bretton Woods as a technical
 *   coordination mechanism whose efficiency requires convertibility; the
 *   institutional machinery (IMF, World Bank) enforces this requirement
 *   through lending conditions on debtor nations. The constraint exhibits
 *   tangled coordination-extraction: international capital holders genuinely
 *   benefit from open markets (pure coordination gain), while debtor nations
 *   face real policy constraints masked by the language of international
 *   monetary order. Over the 30-year interval shown in measurements,
 *   extractiveness accumulates (0.32 → 0.58) as the original reconstruction
 *   function fades and the regime's pure extraction mechanism (capital
 *   control prohibition, austerity enforcement) becomes dominant.
 *
 * KEY AGENTS:
 *   - International Capital Holders: Primary beneficiary (institutional/arbitrage) — benefit from free capital mobility and policy constraints on debtor nations. Net extractive inflow.
 *   - Creditor Nations (US, UK, allies): Primary beneficiary (powerful/mobile) — deploy regime to protect financial dominance and creditor rights. Have exit option but prefer regime continuation.
 *   - Debtor Nations (developing economies): Primary victim (powerless/trapped) — cannot deploy capital controls, face austerity conditions, policy autonomy is constrained. Maximum extraction experienced.
 *   - National Policy Autonomy: Abstract victim (powerless/trapped) — reified in regime structure; cannot organize or resist. Bears extraction passively.
 *   - Bretton Woods Institutional Apparatus (IMF/World Bank): Institutional enforcer (institutional/arbitrage) — maintains the regime through loan conditionality and structural adjustment. Theater ratio moderating as original coordination function atrophies.
 *   - Postwar Reconstruction Coalition: Historical agent (powerful/constrained) — deployed regime as temporary mechanism; sunset never fired. Piton perspective.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.58).
domain_priors:suppression_score(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.72).
domain_priors:theater_ratio(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, extractiveness, 0.58).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bretton_woods_treaty_substrate__neoliberal_convertibility, tangled_rope).
narrative_ontology:human_readable(bretton_woods_treaty_substrate__neoliberal_convertibility, "Bretton Woods Treaty Substrate: Neoliberal Convertibility Reading").
narrative_ontology:topic_domain(bretton_woods_treaty_substrate__neoliberal_convertibility, "international_political_economy/monetary_institutions").

domain_priors:requires_active_enforcement(bretton_woods_treaty_substrate__neoliberal_convertibility).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bretton_woods_treaty_substrate__neoliberal_convertibility, 'bw-neoliberal-2026-02-26').
narrative_ontology:cs_kernel_codification('bw-neoliberal-2026-02-26', formalized).
narrative_ontology:cs_authority_grounding('bw-neoliberal-2026-02-26', extraction).
narrative_ontology:cs_interpretation_layer_present('bw-neoliberal-2026-02-26').
narrative_ontology:cs_reading_relation('bw-neoliberal-2026-02-26', bretton_woods_treaty_substrate__keynesian_embedded_liberalism, coexists_with).
narrative_ontology:cs_reading_relation('bw-neoliberal-2026-02-26', bretton_woods_treaty_substrate__sovereignty_defense, influences).
narrative_ontology:cs_axiom('bw-neoliberal-2026-02-26', foundational, capital_must_move_freely).
narrative_ontology:cs_axiom_status(capital_must_move_freely, holdable).
narrative_ontology:cs_axiom_grounding('bw-neoliberal-2026-02-26', capital_must_move_freely, empirically_contingent).
narrative_ontology:cs_axiom('bw-neoliberal-2026-02-26', foundational, convertibility_obligation_is_coordination_not_coercion).
narrative_ontology:cs_axiom_status(convertibility_obligation_is_coordination_not_coercion, holdable).
narrative_ontology:cs_axiom_grounding('bw-neoliberal-2026-02-26', convertibility_obligation_is_coordination_not_coercion, instrumental).
narrative_ontology:cs_reference_frame('bw-neoliberal-2026-02-26', free_capital_flows_institutional_baseline).
narrative_ontology:cs_drift_state('bw-neoliberal-2026-02-26', contemporary_post_2008_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('bw-neoliberal-2026-02-26', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(bretton_woods_treaty_substrate__neoliberal_convertibility, bretton_woods_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__neoliberal_convertibility, international_capital_holders).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__neoliberal_convertibility, creditor_nations).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__neoliberal_convertibility, multinational_firms).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__neoliberal_convertibility, national_policy_autonomy).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__neoliberal_convertibility, developing_economies).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__neoliberal_convertibility, capital_control_sovereignty).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEBTOR NATION (SNARE) — Trapped by currency peg and capital mobility requirements. Cannot deploy capital controls to protect domestic industry or manage capital flight. Convertibility obligation forces external discipline; national policy autonomy is victim. Maximum extraction: creditors extract through forced austerity, interest rate discipline, and policy surrender.
constraint_indexing:constraint_classification(bretton_woods_treaty_substrate__neoliberal_convertibility, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DOMESTIC PRODUCTIVE SECTOR (TANGLED ROPE) — Constrained by inability to use tariffs or capital controls but benefits from access to international credit and technology flows. Mixed extraction: some protection lost, but also gains from integration. High suppression: cannot exit the regime without sovereign defection (costly).
constraint_indexing:constraint_classification(bretton_woods_treaty_substrate__neoliberal_convertibility, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INTERNATIONAL CAPITAL HOLDERS (ROPE) — Primary beneficiaries. Experience the constraint as pure coordination: open capital markets enable profitable allocation without barriers. Arbitrage exit option (can reposition capital globally). Net beneficiary — extraction runs toward this agent.
constraint_indexing:constraint_classification(bretton_woods_treaty_substrate__neoliberal_convertibility, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: POSTWAR LIBERAL RECONSTRUCTION COALITION (SCAFFOLD) — Powerful actors (US, UK, allied governments) deployed convertibility as temporary coordination mechanism to rebuild international commerce after WWII. Saw regime as sunset: 'Until developing economies can self-service their currency needs.' The sunset never fired — instead, the regime persisted and was extended through IMF conditionality. Theater is moderate because the original coordination function (re-establishing trade) was genuine, even though the regime's extraction mechanism (policy control) became the lasting feature.
constraint_indexing:constraint_classification(bretton_woods_treaty_substrate__neoliberal_convertibility, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: BRETTON WOODS INSTITUTIONAL APPARATUS (PITON) — The IMF and World Bank maintain the convertibility regime through degraded functionality. The institutions' original coordination purpose (stabilizing exchange rates, enabling recovery lending) has atrophied; they now primarily enforce policy discipline on debtor nations. Theater ratio is moderate-to-high: Fund programs include ritualistic structural adjustment requirements that produce compliance theater rather than economic development. The apparatus persists through institutional inertia and because creditor-nation governments prefer the extraction mechanism to its dissolution.
constraint_indexing:constraint_classification(bretton_woods_treaty_substrate__neoliberal_convertibility, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: CREDITOR-NATION POLICY MAKERS (TANGLED ROPE) — Powerful actors with mobile exit options (can exit regime at cost of reputational damage). Experience genuine coordination benefits: the regime enables capital export and financial hegemony. But also bear suppression costs: must actively enforce convertibility obligations on debtors, defend the regime diplomatically, manage periodic crises. This is hybrid: pure extraction for debtors, tangled coordination-extraction for creditors.
constraint_indexing:constraint_classification(bretton_woods_treaty_substrate__neoliberal_convertibility, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / ECONOMIC LAW VIEW (MOUNTAIN) — From a civilizational perspective, fixed exchange rates and convertibility requirements appear as immutable laws of stable international commerce: 'Capital must move freely' and 'Currencies must be mutually convertible for trade to function' are presented as economic necessities, not policy choices. This perspective risks naturalizing what is actually a contingent institutional arrangement designed to benefit specific actors. The engine's false summit detector will classify this as naturalization of a political choice.
constraint_indexing:constraint_classification(bretton_woods_treaty_substrate__neoliberal_convertibility, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bretton_woods_treaty_substrate__neoliberal_convertibility_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(bretton_woods_treaty_substrate__neoliberal_convertibility, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bretton_woods_treaty_substrate__neoliberal_convertibility, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(bretton_woods_treaty_substrate__neoliberal_convertibility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(bretton_woods_treaty_substrate__neoliberal_convertibility, TR),
    TR >= 0.70.

:- end_tests(bretton_woods_treaty_substrate__neoliberal_convertibility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The regime generates real extraction from debtor nations through forced policy convergence (no capital controls, privatization, reduced social spending), but the extraction is not as severe as pure snares (0.70+) because international capital holders do experience genuine coordination benefits from open markets. The measurement trajectory (0.32 → 0.58) reflects institutional evolution: the regime began as reconstruction coordination (lower extraction) and matured into pure policy enforcement (higher extraction). Suppression (0.72): High. The regime creates substantial barriers to exit: IMF conditionality, diplomatic pressure, threat of credit cutoff, and ideological hegemony (the 'Washington Consensus' framing of convertibility as economic necessity). Debtor nations face material costs for defection. Theater ratio (0.48): Moderate-to-low. The regime is not maximally performative because the coordination function is real (capital does allocate more efficiently across open borders) and the extraction mechanism is relatively transparent (conditionality terms are written). However, theater increases over time (0.35 → 0.48) as the original reconstruction purpose fades and the regime becomes pure policy enforcement with rituals (IMF reviews, structural adjustment theater) that no longer serve genuine coordination.
 *
 * PERSPECTIVAL GAP:
 *   The neoliberal reading generates stark perspectival divergence. International capital holders see pure rope (coordination without extraction). Creditor nations see tangled rope (genuine coordination gains in capital allocation, but also enforcement costs). Debtor nations see snare (pure extraction with no coordination benefit). The domestic productive sector sees tangled rope (some integration benefits, but constrained by policy limits). The postwar reconstruction coalition sees scaffold with a failed sunset (the regime was meant to be temporary). The IMF apparatus sees piton (original coordination function atrophied, now performing compliance theater). The analytical observer risks seeing mountain (natural law of economics: 'capital must move freely'). The perspectival gap is the reading itself: different parties fundamentally disagree about whether convertibility is a coordination mechanism or an extraction device.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values are derived from beneficiary/victim status and exit options. International capital holders are beneficiaries with arbitrage options (d ≈ 0.10, low extraction experienced). Debtor nations are victims with trapped exit (d ≈ 0.95, maximum extraction experienced). Creditor nations are beneficiaries with mobile exit (d ≈ 0.45, moderate extraction experienced despite beneficiary status, because they bear enforcement costs). The piton perspective shows how institutional actors with arbitrage options experience low extraction (the regime works in their favor), while powerless agents experience high extraction. The mountain perspective's d value (0.72, analytical observer) reflects that the observer can see the extraction mechanism but presents it as natural law.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that this constraint is ONE READING of a contested kernel. The question 'is Bretton Woods rope or snare?' has no single answer — it depends on which reading of the treaty's core commitment you adopt. The neoliberal reading (capital must be free) makes convertibility requirements binding, converts policy autonomy to a victim, and produces snare from debtor perspectives. The keynesian reading (capital controls are tools) would classify the same institutional apparatus differently. The sovereignty defense reading (the regime is illegitimate) would classify as snare but for different reasons (violation of autonomy as principle, not just policy). The engine should not collapse these readings into a single classification. Instead, the presheaf over the reading space IS the answer: this constraint is tangled rope from the neoliberal reading perspective, rope from the keynesian reading perspective, and snare from the sovereignty reading perspective. The contested kernel structure explains the mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_kernel_choice,
    'Is the Bretton Woods charter''s requirement for ''convertibility'' an inherent feature of the treaty or a contested interpretation imposed by creditor nations?',
    'Historical analysis of treaty text, negotiation records (Keynes vs White papers), and post-1944 institutional evolution. Examine whether Keynes'' embedded liberalism reading (allowing capital controls) was deliberately suppressed in favor of the neoliberal reading.',
    'If treaty text is ambiguous: the readings coexist genuinely. If text supports capital controls: the neoliberal reading is an institutional override, not a reading. If text mandates convertibility: the neoliberal reading is the faithful interpretation and keynesian reading is revisionist.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_choice, empirical, 'Whether convertibility requirement is inherent or institutionally imposed').

omega_variable(
    capital_control_enforcement_cost,
    'What fraction of enforcement costs (suppressing alternative readings, defending regime against defections) falls to creditor nations vs. imposed on debtor nations?',
    'Analysis of IMF program conditions, structural adjustment conditionality, and military/diplomatic pressure applied to enforce capital account openness. Compare to voluntary participation rates if conditionality were removed.',
    'If costs are symmetrical: tangled rope classification confirmed (both creditors and debtors bear suppression burden). If costs fall primarily on debtors: snare classification from debtor perspective is confirmed as asymmetric extraction masked by institutional language.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_control_enforcement_cost, empirical, 'Distribution of enforcement costs between creditor and debtor nations').

omega_variable(
    policy_autonomy_victim_status,
    'Is ''national policy autonomy'' meaningfully a victim of the regime, or is this a reification of sovereignty that never existed in practice?',
    'Counterfactual analysis: what policies would debtor nations pursue absent convertibility requirements? Compare actual policy space under the regime to post-IMF-exit policies (Malaysia 1998, Argentina 2001). Measure policy divergence.',
    'If autonomy is real victim: extractiveness estimate (0.58) is accurate; regime genuinely constrains policy choice. If autonomy is retroactively claimed: extractiveness should be lower; regime offers coordination gains without real policy cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_autonomy_victim_status, conceptual, 'Whether national policy autonomy is a genuine victim or reified abstraction').

omega_variable(
    sibling_reading_foreclosure,
    'Does the neoliberal convertibility reading logically foreclose the keynesian embedded liberalism reading, or do they coexist as live policy options held by different parties?',
    'Examine whether accepting the neoliberal axiom (''capital must move freely'') requires rejecting the keynesian axiom (''capital controls are legitimate tools''). If both can be held without contradiction in different institutional frameworks, they coexist. If one logically entails the falsity of the other, they foreclose.',
    'If they foreclose: one reading will eventually dominate institutional structure; the contest is over which core premise gains authority. If they coexist: multiple institutional arrangements are simultaneously viable; the contest is over distribution of power to choose which regime operates where.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Logical relationship between neoliberal and keynesian readings of Bretton Woods').

omega_variable(
    convertibility_sunset_mechanism,
    'Was there ever a plausible sunset clause for convertibility requirements, or was the regime designed as permanent institutional lock-in?',
    'Examine IMF Articles of Agreement for explicit sunset language or capacity triggers. Compare to original reconstruction estimates: when did planners expect developing economies to achieve ''independence'' from IMF oversight? Did that date pass?',
    'If sunset was real (e.g., ''once countries reach X% of world GDP, convertibility obligations phase out''): the scaffold perspective is structurally accurate and the regime is degraded piton (sunset never fired). If no sunset exists: the regime was always designed as permanent institutional constraint, not temporary coordination scaffold.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(convertibility_sunset_mechanism, empirical, 'Whether Bretton Woods convertibility had a built-in sunset mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bretton_woods_treaty_substrate__neoliberal_convertibility, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bwconv_theater_t0, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 0, 0.35).
narrative_ontology:measurement(bwconv_theater_t15, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 15, 0.42).
narrative_ontology:measurement(bwconv_theater_t30, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 30, 0.48).

% Extraction over time
narrative_ontology:measurement(bwconv_extract_t0, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(bwconv_extract_t15, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(bwconv_extract_t30, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(bwconv_suppress_t0, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(bwconv_suppress_t15, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 15, 0.65).
narrative_ontology:measurement(bwconv_suppress_t30, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 30, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bretton_woods_treaty_substrate__neoliberal_convertibility, resource_allocation).
narrative_ontology:boltzmann_floor_override(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.18).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__neoliberal_convertibility, bretton_woods_treaty_substrate__keynesian_embedded_liberalism).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__neoliberal_convertibility, bretton_woods_treaty_substrate__sovereignty_defense).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__neoliberal_convertibility, imf_structural_adjustment_conditionality).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__neoliberal_convertibility, capital_account_liberalization_mandate).

% DUAL FORMULATION NOTE:
% Bretton Woods constraint family (3 readings of contested kernel): neoliberal_convertibility (this file) produces snare from debtor perspectives; keynesian_embedded_liberalism would allow capital controls and produce rope from most perspectives; sovereignty_defense treats regime as illegitimate coercion. Each reading has distinct metrics, victims, and classification topology. Linked via network edges to show they are readings of the same kernel, not independent constraints. The family's mandatrophy is resolved by recognizing the reading multiplicity — classification correctness is reading-relative.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bretton_woods_treaty_substrate__neoliberal_convertibility, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
