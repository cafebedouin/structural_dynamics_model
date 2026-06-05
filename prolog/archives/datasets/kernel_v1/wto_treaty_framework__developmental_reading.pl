% ============================================================================
% CONSTRAINT STORY: wto_treaty_framework__developmental_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_wto_treaty_framework__developmental_reading, []).

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
 *   constraint_id: wto_treaty_framework__developmental_reading
 *   human_readable: WTO Treaty Framework (Developmental Reading): Special and Differential Treatment and Technology Transfer Obligations
 *   domain: international_trade_law/development_economics/political_economy
 *
 * SUMMARY:
 *   The WTO treaty framework's developmental reading positions Special and
 *   Differential Treatment (S&D) provisions and technology transfer
 *   obligations as core commitments recognizing structural asymmetries
 *   between developed and developing states. This reading instantiates a
 *   specific interpretation of the treaty kernel: the WTO accommodates
 *   development by preserving policy space (tariff flexibility, subsidy
 *   exceptions, compulsory licensing authority) for states building
 *   industrial capacity from lower starting points. This reading is
 *   contested. The alternative market-access reading treats S&D as bounded
 *   exceptions to the primary commitment: trade liberalization through
 *   binding tariff reductions and IP protection. The developmental reading
 *   and market-access reading coexist as live positions held by different
 *   state coalitions in the WTO, with structural pressure from advanced
 *   economies pushing toward market-access primacy. The constraint exhibits
 *   six distinct classifications across perspectives, revealing how the same
 *   institutional framework operates as coordination mechanism, extraction
 *   apparatus, and theatrical ritual depending on the observer's structural
 *   position.
 *
 * KEY AGENTS:
 *   - Developing Country Coalition (G-77, ALBA): organized beneficiaries (organized/constrained) — S&D and transfer obligations enable policy space for industrial protection and technology acquisition. Genuine coordination function but subject to weak enforcement.
 *   - Least Developed Countries: trapped victims (powerless/trapped) — formally covered by S&D but face unenforceable extensions and no remedy mechanisms for non-compliance by advanced economies. Maximum experienced extraction at biographical time.
 *   - Multinational IP Holders: constrained beneficiaries of TRIPs, victims of transfer obligations (powerful/mobile) — gain from IP enforcement, lose from compulsory licensing and transfer requirements. Mixed extraction experience.
 *   - Advanced Economy States: institutional beneficiaries of market access and IP enforcement, constrained by S&D obligations (institutional/constrained) — benefit from trade liberalization and IP protection, must credibly enforce S&D to maintain framework stability.
 *   - WTO Dispute Settlement System: institutional arbitrageur (institutional/arbitrage) — benefits from both sides (TRIPs complaints and S&D complaints) while enforcement remains asymmetric; maintains theater of S&D consideration while structurally favoring IP enforcement.
 *   - Development-Oriented States: identity-locked organized actors (organized/identity_locked) — constitute governance legitimacy through 'development,' but development-as-WTO-accommodation differs from development-as-autonomous-industrial-policy.
 *   - Analytical Observer: risks naturalizing contingent institutional choices (analytical/analytical) — treats asymmetry as natural feature requiring accommodation rather than constructed feature of treaty design.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wto_treaty_framework__developmental_reading, 0.38).
domain_priors:suppression_score(wto_treaty_framework__developmental_reading, 0.42).
domain_priors:theater_ratio(wto_treaty_framework__developmental_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wto_treaty_framework__developmental_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(wto_treaty_framework__developmental_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(wto_treaty_framework__developmental_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wto_treaty_framework__developmental_reading, tangled_rope).
narrative_ontology:human_readable(wto_treaty_framework__developmental_reading, "WTO Treaty Framework (Developmental Reading): Special and Differential Treatment and Technology Transfer Obligations").
narrative_ontology:topic_domain(wto_treaty_framework__developmental_reading, "international_trade_law/development_economics/political_economy").

domain_priors:requires_active_enforcement(wto_treaty_framework__developmental_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(wto_treaty_framework__developmental_reading, '76910328-63dd-4956-8e1f-1653d2bf16d4').
narrative_ontology:cs_kernel_codification('76910328-63dd-4956-8e1f-1653d2bf16d4', fixed_text).
narrative_ontology:cs_authority_grounding('76910328-63dd-4956-8e1f-1653d2bf16d4', extraction).
narrative_ontology:cs_interpretation_layer_present('76910328-63dd-4956-8e1f-1653d2bf16d4').
narrative_ontology:cs_reading_relation('76910328-63dd-4956-8e1f-1653d2bf16d4', wto_treaty_framework__market_access_reading, coexists_with).
narrative_ontology:cs_axiom('76910328-63dd-4956-8e1f-1653d2bf16d4', foundational, structural_asymmetry_requires_policy_space).
narrative_ontology:cs_axiom_status(structural_asymmetry_requires_policy_space, holdable).
narrative_ontology:cs_axiom_grounding('76910328-63dd-4956-8e1f-1653d2bf16d4', structural_asymmetry_requires_policy_space, instrumental).
narrative_ontology:cs_axiom('76910328-63dd-4956-8e1f-1653d2bf16d4', foundational, technology_transfer_obligation_binds_ip_holders).
narrative_ontology:cs_axiom_status(technology_transfer_obligation_binds_ip_holders, holdable).
narrative_ontology:cs_axiom_grounding('76910328-63dd-4956-8e1f-1653d2bf16d4', technology_transfer_obligation_binds_ip_holders, deontological).
narrative_ontology:cs_reference_frame('76910328-63dd-4956-8e1f-1653d2bf16d4', development_accommodation_framework).
narrative_ontology:cs_drift_state('76910328-63dd-4956-8e1f-1653d2bf16d4', contemporary_post_2000s, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('76910328-63dd-4956-8e1f-1653d2bf16d4', '').
narrative_ontology:cs_kernel_id(wto_treaty_framework__developmental_reading, wto_treaty_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wto_treaty_framework__developmental_reading, developing_country_states).
narrative_ontology:constraint_beneficiary(wto_treaty_framework__developmental_reading, least_developed_country_states).
narrative_ontology:constraint_beneficiary(wto_treaty_framework__developmental_reading, infant_industries_in_global_south).
narrative_ontology:constraint_victim(wto_treaty_framework__developmental_reading, multinational_ip_holders).
narrative_ontology:constraint_victim(wto_treaty_framework__developmental_reading, advanced_economy_export_sectors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEVELOPING COUNTRY COALITION (ROPE) — Organized via G-77, ALBA, and WTO coalitions. The S&D framework and technology transfer obligations solve a genuine collective action problem: how to participate in global trade while building domestic capacity. Tariff flexibility enables infant industry protection; compulsory licensing authority enables medication access. The constraint coordinates participation without total subordination to multinational IP regimes. Organized agents with partial exit capacity (can opt for regional trade agreements, TRIPS flexibilities, Article 31 compulsory licensing). Low effective extraction from this perspective — the coordination benefit is real and the enforcement overhead is moderate.
constraint_indexing:constraint_classification(wto_treaty_framework__developmental_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 2: LEAST DEVELOPED COUNTRY STATES (SNARE) — Trapped by global supply chain integration and debt dependencies. S&D provisions exist as text but enforcement is weak — implementation deadlines are routinely extended without teeth. Technology transfer obligations have no enforcement mechanism (no WTO dispute settlement remedy for failure to transfer). LDCs cannot exit the system without economic isolation. Maximum extraction at biographical time horizon: participate or be excluded from global markets; participate and transfer obligations are hollow. No domestic organizing capacity independent of state apparatus.
constraint_indexing:constraint_classification(wto_treaty_framework__developmental_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: MULTINATIONAL IP HOLDERS (TANGLED ROPE) — Benefit from WTO enforcement of patent, trademark, and copyright standards (TRIPs enforcement is robust). Constrained by compulsory licensing authority and technology transfer obligations, which reduce licensing revenue streams. Can exit via supply-chain restructuring and market segmentation. Experience mixed extraction: gain from global IP enforcement, lose from licensing restrictions and transfer obligations. Mobile but not arbitrage-capacity — the constraint reduces but doesn't eliminate profitability.
constraint_indexing:constraint_classification(wto_treaty_framework__developmental_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: ADVANCED ECONOMY STATES (TANGLED ROPE) — Benefit from market access for high-value exports (machinery, pharmaceuticals, financial services); constrained by S&D obligations to extend tariff flexibility and compulsory licensing authority. The constraint coordinates market integration while protecting developing state capacity-building — without this coordination mechanism, alternative arrangements (regional protectionism, nationalization of multinational assets) would be more damaging to advanced economy interests. Generational time horizon: the framework stabilizes global supply chains across decades. Active enforcement required to maintain credibility of S&D commitments while advanced economies enjoy market-access benefits.
constraint_indexing:constraint_classification(wto_treaty_framework__developmental_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: WTO DISPUTE SETTLEMENT SYSTEM (PITON) — Ostensibly enforces S&D obligations and technology transfer commitments. Functionally: TRIPs enforcement is robust (strong DSB remedies); S&D enforcement is theatrical (non-binding implementation deadlines, no remedy mechanisms). Theater ratio high: the DSB issues reports acknowledging S&D non-compliance, states issue statements, and deadlines extend perpetually. The system maintains legitimacy by recognizing S&D formally while structurally favoring powerful states' enforcement (IP protection). Enforcement capacity has degraded (appeals crisis, consensus blocking) but theater persists (dispute filings continue). Institutional actors benefit from the arbitrage between formal S&D rhetoric and weak enforcement.
constraint_indexing:constraint_classification(wto_treaty_framework__developmental_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, development asymmetries are structural facts: countries with accumulated capital, industrial capacity, and technological expertise have inescapable advantages over those without. Any global trade framework must accommodate this asymmetry or generate collapse. S&D provisions and technology transfer obligations are treated as natural accommodations to structural inequality. However, the explicit beneficiary declarations contradict this reading: identifiable institutional actors (multinational IP holders, advanced economy export sectors) benefit from weak enforcement of S&D. This triggers false summit evaluation — the 'natural asymmetry' framing naturalizes what is actually a contingent regulatory choice.
constraint_indexing:constraint_classification(wto_treaty_framework__developmental_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: DEVELOPMENT-ORIENTED STATES (TANGLED ROPE, IDENTITY_LOCKED) — Some states have constituted their post-colonial identity and governance legitimacy through 'development' as a primary commitment. For these actors, exercising S&D flexibility (tariff protection, compulsory licensing) is not a contingent economic calculation but identity-constitutive: it is what it means to be a development state. Structural mobility exists (could abandon development goals and specialize in commodity extraction) but is unthinkable from within the identity frame. The constraint is simultaneously coordinating (develops trade relationships) and extractive (constrains industrial policy choices to WTO-compatible forms). Identity lock prevents recognizing that development-as-WTO-accommodation may be a different development model than development-as-autonomous-industrial-policy.
constraint_indexing:constraint_classification(wto_treaty_framework__developmental_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wto_treaty_framework__developmental_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(wto_treaty_framework__developmental_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(wto_treaty_framework__developmental_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(wto_treaty_framework__developmental_reading, TR),
    TR >= 0.70.

:- end_tests(wto_treaty_framework__developmental_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The developmental reading interprets the WTO as coordinating global trade while preserving capacity-building space for developing states. The extractiveness reflects asymmetric enforcement: S&D obligations are formally recognized but substantively weak, while TRIPs enforcement is robust. The metric represents the middle ground between coordination-only (ε ~0.15) and snare-level extraction (ε >0.60). Suppression (0.42): Moderate-high. Barriers include: weak enforcement mechanisms for S&D (no WTO-DSB remedy for non-compliance with transfer obligations), legal uncertainty in compulsory licensing exercise, retaliation risk from advanced economies (investment-treaty threats, aid conditionality), and supply-chain exclusion threats. These are substantial but not total — developing states can exercise S&D despite costs. Theater ratio (0.48): Moderate. The system exhibits significant performative elements: S&D implementation deadlines are routinely extended without enforcement; WTO-DSB issues reports acknowledging non-compliance; states issue statements; substantive change is minimal. However, some real functional activity exists: tariff flexibility is actually used, some technology transfer occurs, compulsory licensing authority is occasionally invoked. The constraint is not purely theatrical (piton level) but contains substantial theater.
 *
 * PERSPECTIVAL GAP:
 *   The developmental and market-access readings create a perspectival schism. From the developed-state and multinational-IP perspective, the constraint is tangled rope: market-access coordination with extraction of tariff flexibility/IP limitations. From the developing-state perspective, it is rope with weak enforcement (coordination with minimal extraction). From the LDC perspective, it is snare (formal protections with no enforcement teeth). The WTO-DSB sees piton (performative S&D recognition masking IP-enforcement priority). The analytical observer risks seeing mountain (natural development asymmetry requiring accommodation) but the beneficiary/victim structure reveals false summit: identifiable institutional actors benefit from weak S&D enforcement, not from natural law. The identity-locked development state perspective reveals how S&D flexibility becomes identity-constitutive, preventing recognition that alternative development models exist outside the WTO frame.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality flows from beneficiary/victim structure and exit capacity. Developing states benefit from S&D (low d), advanced states benefit from market access while bearing S&D constraint costs (moderate d), multinational IP holders benefit from TRIPs while losing to transfer obligations (moderate d ~0.55), LDCs are trapped victims (high d ~0.90). The identity-locked development states have moderate d (organized power) but experience it as constraining identity rather than external force — the binding is cognitive, not material. Advanced economy states experience low-moderate d from the TRIPs enforcement perspective but higher d from the S&D enforcement perspective — they are simultaneously beneficiaries and constrained actors depending on which obligation is foreground.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint manifestly exhibits mandatrophy — the developmental reading and market-access reading represent genuinely different interpretations of the same treaty kernel. The engine's mandatrophy resolution occurs through reading_relations: the developmental reading and market-access reading COEXIST rather than one foreclosing the other. They are held by different state coalitions, and neither logically eliminates the other within any single framework (a state can hold both: 'we support S&D as human concern AND competitive market access as efficiency concern'). The resolution is not reclassification but perspectival mapping: which reading is primary depends on which state's perspective is foreground. From developed-state institutional perspective, market-access reading is primary, developmental reading is bounded exception. From developing-state perspective, developmental reading is primary, market-access reading is constrained coordination. The mandatrophy is structural, not resolvable to single classification — it is the difference between readings of the same kernel.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sd_enforcement_mechanism_ambiguity,
    'Does S&D status confer genuine protective capacity or is it a rhetorical concession with no enforcement teeth?',
    'Comparative analysis of dispute settlement outcomes: S&D claims in DSB disputes vs TRIPs enforcement claims over 20+ years. Measurement of implementation rates for S&D extension decisions vs TRIPs compliance mandates.',
    'If enforcement is genuine: constraint is Rope with real coordination function. If enforcement is hollow: constraint is closer to Snare or Piton for developing countries — the text creates false expectations while the system enforces IP protections asymmetrically.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sd_enforcement_mechanism_ambiguity, empirical, 'Whether S&D provisions have enforceable mechanisms or are unenforceable rhetoric').

omega_variable(
    technology_transfer_obligation_scope,
    'Are technology transfer obligations binding commitments or aspirational frameworks with no remedy mechanism?',
    'Analysis of WTO-DSB jurisprudence on technology transfer disputes; survey of actual technology transfer flows post-TRIPs; examination of remedy mechanisms available to states claiming non-compliance with transfer obligations.',
    'If binding and enforced: constraint is tangled rope with real extraction on IP holders. If unenforceable: constraint is false commitment, theater increases, extractiveness of the system against developing countries increases.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(technology_transfer_obligation_scope, empirical, 'Whether technology transfer obligations are enforceable commitments').

omega_variable(
    infant_industry_protection_effectiveness,
    'Does S&D tariff flexibility enable genuine infant industry maturation or does it lock developing countries into intermediate stages of industrialization?',
    'Longitudinal analysis of developing countries using S&D tariff protection: comparison of industrial diversification trajectories for users vs non-users of tariff flexibility. Measurement of technological sophistication climb in protected vs unprotected sectors.',
    'If effective at enabling transition: S&D is functional coordination. If locking-in effect: S&D becomes a maintenance mechanism for dependency, extractiveness against developing countries increases, constraint reclassifies toward Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(infant_industry_protection_effectiveness, empirical, 'Whether tariff flexibility enables infant industry development or locks in dependency').

omega_variable(
    compulsory_licensing_exercise_barriers,
    'What non-legal barriers (retaliation risk, investment-treaty exposure, supply-chain exclusion) prevent developing countries from using Article 31 compulsory licensing authority despite formal textual right?',
    'Case studies of states attempting or considering compulsory licensing (India, Thailand, South Africa). Measurement of threats and countervailing pressures (investor-state dispute threats, market access threats, diplomatic pressure). Survey of legal uncertainty in compulsory licensing exercise.',
    'If non-legal barriers are substantial: formal right is theater, actual suppression is high, extractiveness against developing countries increases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compulsory_licensing_exercise_barriers, empirical, 'What barriers prevent exercise of compulsory licensing authority despite formal textual right').

omega_variable(
    developmental_reading_vs_market_access_reading_kernel_contest,
    'Which reading of the WTO treaty kernel is the actual binding commitment: development accommodation with flexibility and transfer obligations, or market access expansion with bounded exceptions?',
    'Textual analysis of TRIPs vs General Exceptions; DSB jurisprudence on whether S&D claims are treated as substantive obligations or procedural accommodations; state practice in dispute settlement and implementation.',
    'If developmental reading is binding: constraint is tangled rope with real coordination and real extraction asymmetry. If market-access reading is binding: S&D is subordinate to IP enforcement, constraint becomes Snare for developing countries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(developmental_reading_vs_market_access_reading_kernel_contest, conceptual, 'Which reading of the WTO treaty kernel (developmental accommodation vs market access expansion) is the actual binding commitment').

omega_variable(
    false_summit_natural_development_asymmetry,
    'Is the structural asymmetry between developed and developing states a natural feature of development economics (mountain) or a contingent result of WTO institutional design (constructed constraint)?',
    'Counterfactual analysis: what would alternative trade frameworks (autonomous industrial policy, South-South cooperation, technology commons) enable that WTO framework prevents? Historical analysis of pre-WTO development trajectories. Comparison with non-WTO development mechanisms.',
    'If natural: constraint is mountain, S&D is inherent accommodation. If constructed: constraint is false summit, S&D is naturalization of institutional choice, actual extractiveness is higher than apparent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(false_summit_natural_development_asymmetry, conceptual, 'Whether developmental asymmetry is natural or constructed by institutional design').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wto_treaty_framework__developmental_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wto_dev_theater_t0_1995, wto_treaty_framework__developmental_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(wto_dev_theater_t10_2005, wto_treaty_framework__developmental_reading, theater_ratio, 10, 0.42).
narrative_ontology:measurement(wto_dev_theater_t20_2015, wto_treaty_framework__developmental_reading, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(wto_dev_extractiveness_t0_1995, wto_treaty_framework__developmental_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(wto_dev_extractiveness_t10_2005, wto_treaty_framework__developmental_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(wto_dev_extractiveness_t20_2015, wto_treaty_framework__developmental_reading, base_extractiveness, 20, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(wto_dev_suppression_t0_1995, wto_treaty_framework__developmental_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(wto_dev_suppression_t10_2005, wto_treaty_framework__developmental_reading, suppression_requirement, 10, 0.38).
narrative_ontology:measurement(wto_dev_suppression_t20_2015, wto_treaty_framework__developmental_reading, suppression_requirement, 20, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wto_treaty_framework__developmental_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(wto_treaty_framework__developmental_reading, 0.18).
narrative_ontology:affects_constraint(wto_treaty_framework__developmental_reading, wto_treaty_framework__market_access_reading).
narrative_ontology:affects_constraint(wto_treaty_framework__developmental_reading, trips_enforcement_asymmetry).
narrative_ontology:affects_constraint(wto_treaty_framework__developmental_reading, compulsory_licensing_investment_treaty_conflict).

% DUAL FORMULATION NOTE:
% The WTO treaty framework decomposes into two structurally distinct constraint readings: developmental_reading (this constraint, ε=0.38, prioritizes S&D and technology transfer) and market_access_reading (ε to be determined, prioritizes tariff binding and IP enforcement). The readings have different ε values because they measure different obligations as primary. They are not observable-dependent measurements of one constraint but genuinely different commitments. Both readings affect downstream constraints (TRIPs enforcement asymmetry, compulsory licensing conflicts with investment treaties, technology transfer flows) but through different mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(wto_treaty_framework__developmental_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
