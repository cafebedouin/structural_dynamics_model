% ============================================================================
% CONSTRAINT STORY: paris_article_4_ndc__sovereigntist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_paris_article_4_ndc__sovereigntist_reading, []).

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
 *   constraint_id: paris_article_4_ndc__sovereigntist_reading
 *   human_readable: NDCs as Voluntary Sovereignty-Preserving Coordination (Sovereigntist Reading)
 *   domain: international_climate_governance/treaty_law/political_economy
 *
 * SUMMARY:
 *   The Paris Agreement's Article 4 permits states to submit Nationally
 *   Determined Contributions (NDCs) — voluntary climate pledges that preserve
 *   national energy sovereignty and unilateral revision rights. The
 *   sovereigntist reading interprets this design as a coordination mechanism
 *   that solves the collective action problem (preventing atmospheric
 *   stability collapse) while maintaining each state's right to exit or
 *   renegotiate. This reading emphasizes that the mechanism's voluntary
 *   character and revision-freedom are features, not bugs — they enable
 *   participation by fossil-dependent economies that would reject binding
 *   commitments. The reading frames NDCs as genuine rope: coordination
 *   without coercion. However, the same structural arrangement can be read as
 *   a snare (inadequate aggregate action binding weaker states to
 *   insufficient targets), a scaffold (temporary mechanism on the path to
 *   binding commitments), or even a mountain (sovereignty preservation as an
 *   immutable feature of international law). This constraint story
 *   instantiates the sovereigntist reading, examining how it models the NDC
 *   architecture, who benefits, and where the reading's frame produces false
 *   summits when confronted with implementation gaps.
 *
 * KEY AGENTS:
 *   - Fossil-Dependent Economies: Primary beneficiary (powerful/arbitrage) — preserve energy sovereignty and development pathways; retain unilateral revision freedom; capture climate finance without binding emission reductions
 *   - National Energy Planners: Institutional beneficiary (institutional/arbitrage) — gain planning legitimacy from international climate norms while preserving domestic control over energy policy
 *   - Low-Capacity Developing States: Mixed position (moderate/constrained) — benefit from technology transfer and capacity building but bear disproportionate reporting burdens and suffer path-dependent lock-in to public pledges
 *   - Climate Finance Coalition: Organized actors (organized/constrained) — mobilize climate finance through the NDC framework but recognize its sunset character; expect eventual graduation to binding mechanisms
 *   - UNFCCC Institutional Apparatus: Institutional actor (institutional/arbitrage) — extracts legitimacy from NDC administration despite degraded primary function (global action remains insufficient)
 *   - Analytical Observer: Civilizational position (analytical/analytical) — risks naturalizing the sovereigntist reading as immutable when it is actually a contested interpretation of treaty language
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(paris_article_4_ndc__sovereigntist_reading, 0.28).
domain_priors:suppression_score(paris_article_4_ndc__sovereigntist_reading, 0.35).
domain_priors:theater_ratio(paris_article_4_ndc__sovereigntist_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__sovereigntist_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(paris_article_4_ndc__sovereigntist_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(paris_article_4_ndc__sovereigntist_reading, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(paris_article_4_ndc__sovereigntist_reading, rope).
narrative_ontology:human_readable(paris_article_4_ndc__sovereigntist_reading, "NDCs as Voluntary Sovereignty-Preserving Coordination (Sovereigntist Reading)").
narrative_ontology:topic_domain(paris_article_4_ndc__sovereigntist_reading, "international_climate_governance/treaty_law/political_economy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(paris_article_4_ndc__sovereigntist_reading, 'paris-a4-sov-2026-02-26').
narrative_ontology:cs_kernel_codification('paris-a4-sov-2026-02-26', fixed_text).
narrative_ontology:cs_authority_grounding('paris-a4-sov-2026-02-26', lineage).
narrative_ontology:cs_interpretation_layer_present('paris-a4-sov-2026-02-26').
narrative_ontology:cs_reading_relation('paris-a4-sov-2026-02-26', paris_article_4_ndc_supranational_reading, coexists_with).
narrative_ontology:cs_reading_relation('paris-a4-sov-2026-02-26', paris_article_4_ndc_equity_reading, coexists_with).
narrative_ontology:cs_axiom('paris-a4-sov-2026-02-26', foundational, state_energy_sovereignty_preservable).
narrative_ontology:cs_axiom_status(state_energy_sovereignty_preservable, holdable).
narrative_ontology:cs_axiom_grounding('paris-a4-sov-2026-02-26', state_energy_sovereignty_preservable, deontological).
narrative_ontology:cs_axiom('paris-a4-sov-2026-02-26', foundational, voluntary_participation_enables_inclusion).
narrative_ontology:cs_axiom_status(voluntary_participation_enables_inclusion, holdable).
narrative_ontology:cs_axiom_grounding('paris-a4-sov-2026-02-26', voluntary_participation_enables_inclusion, instrumental).
narrative_ontology:cs_reference_frame('paris-a4-sov-2026-02-26', state_determined_pluralism).
narrative_ontology:cs_drift_state('paris-a4-sov-2026-02-26', contemporary_implementation_gap, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('paris-a4-sov-2026-02-26', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(paris_article_4_ndc__sovereigntist_reading, paris_article_4_ndc).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__sovereigntist_reading, fossil_dependent_economies).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__sovereigntist_reading, development_aspirant_states).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__sovereigntist_reading, national_energy_planners).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FOSSIL-DEPENDENT ECONOMY (ROPE) — This state experiences NDCs as a coordination mechanism that preserves exit options. The mechanism solves collective action (avoiding atmospheric stability degradation) while retaining national revision freedom. Exit option: revise pledges unilaterally at the next NDC submission cycle (every 5 years post-Paris). Extraction is minimal because the state can calibrate ambition to its economic constraints and energy security needs. Benefits from the coordination: access to climate finance, technology transfer, and reputational positioning without surrendering energy sovereignty. The sovereigntist reading frames this as genuine rope — coordination without coercion.
constraint_indexing:constraint_classification(paris_article_4_ndc__sovereigntist_reading, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 2: NATIONAL ENERGY PLANNER (ROPE) — The institutional actor tasked with balancing energy security, development goals, and climate targets sees NDCs as a coordination tool. The mechanism enables long-term planning by establishing global climate norms (no nation wants unilateral defection to become normalized) while preserving the planner's agency. Exit option: revise targets downward if energy circumstances change; no enforcement mechanism exists to penalize postponement or renegotiation. Theater present (0.52) reflects that achievement claims often track political cycles rather than implementation rigor, but the theater does not contradict rope classification — the coordination function is real even if partially performative.
constraint_indexing:constraint_classification(paris_article_4_ndc__sovereigntist_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: CLIMATE FINANCE COALITION (SCAFFOLD) — Organized actors (UNFCCC, MDBs, bilateral donors) see NDCs as a temporary coordination structure with implicit sunset logic. The NDC framework mobilizes funds (~$100B/year pledged) by tying transfers to state-submitted climate plans. However, the coalition recognizes the mechanism's weakness: absent binding enforcement, funds flow to politically favored states and high-capacity planners regardless of NDC ambition. The coalition's own institutional interest lies in eventual graduation to binding sectoral commitments or carbon pricing frameworks. Current NDC system is a temporary scaffolding — it coordinates the transfer while states retain exit options, but the coalition expects this to be superseded by harder mechanisms. Sunset rationale: as renewable costs fall and carbon pricing matures, fossil-dependent states will face economic (not legal) pressure to decarbonize. The NDC's voluntary character is intentionally temporary.
constraint_indexing:constraint_classification(paris_article_4_ndc__sovereigntist_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: LOW-CAPACITY DEVELOPING STATE (TANGLED ROPE) — A state with limited administrative capacity, data infrastructure, and technical expertise experiences NDCs as both coordination and subtle extraction. Coordination function: participation in the global climate regime legitimizes the state's energy planning and provides access to technology transfer and capacity-building support. Extraction element: the reporting and verification burdens are disproportionate for low-capacity states; the NDC submission requirements impose transaction costs (hiring consultants, building monitoring infrastructure) that wealthier states absorb more easily. Additionally, once a pledge is made public, domestic political pressure and international scrutiny constrain future revisions downward even if circumstances change — the state becomes locked into a publicly stated position. Exit options are constrained but not eliminated: the state can miss deadlines (low enforcement), revise targets (reputational cost but no penalty), or withdraw (legal right but diplomatic cost). Experienced extraction is moderate because constraints are real but circumventable.
constraint_indexing:constraint_classification(paris_article_4_ndc__sovereigntist_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: UNFCCC INSTITUTIONAL APPARATUS (PITON) — The formal institutional structure tasked with tracking and aggregating NDCs exhibits piton characteristics: the mechanism persists through institutional inertia despite degraded function. The primary function — aggregate global climate action — has largely atrophied because aggregated pledges remain insufficient to meet 1.5°C/2°C targets. The theater component (biennial reports, COP sessions, revision cycles) is high (0.52): states perform compliance through technical documentation and diplomatic attendance while underlying implementation lags. The apparatus extracts institutional legitimacy from the NDC process (existence of targets justifies the UNFCCC's budget and staff) but provides minimal enforcement or incentive alignment. Exit for states is easy (arbitrage — revise downward with low penalty); exit for the institution is blocked (path-dependent resource dependence). The piton classification reflects that the NDC framework preserves institutional jurisdiction through voluntary reporting rather than achieving climate outcomes. The institution has become the constraint rather than a tool for solving the constraint.
constraint_indexing:constraint_classification(paris_article_4_ndc__sovereigntist_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From the universal analytical stance, the sovereigntist reading naturalizes the NDC framework as an immutable structural feature of international climate governance: nation-states are the only legitimate actors in the inter-state system; states cannot surrender energy sovereignty without existential threat to domestic legitimacy; therefore any global climate mechanism must preserve state exit options and revision freedom as a fundamental constraint on the system's design. This reading frames NDCs as not merely a contingent institutional choice but as a necessary consequence of international law's foundational commitment to state sovereignty. However, this mountain classification is analytically suspect — it naturalizes what is a specific reading of a contested treaty text (Article 4 of the Paris Agreement). Alternative readings (supranational, equity) would produce different classifications. The engine's false summit detector will flag this as a natural-law reading that benefits identifiable agents (fossil-dependent states, national planners) and therefore risks confusing institutional design choices with immutable limits.
constraint_indexing:constraint_classification(paris_article_4_ndc__sovereigntist_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(paris_article_4_ndc__sovereigntist_reading_tests).

test(piton_threshold) :-
    domain_priors:theater_ratio(paris_article_4_ndc__sovereigntist_reading, TR),
    TR >= 0.70.

:- end_tests(paris_article_4_ndc__sovereigntist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low to moderate. The sovereigntist reading frames NDCs as primarily a coordination mechanism with minimal extraction. States benefit from participation (climate finance, technology access, reputational positioning) and retain full exit options (unilateral revision, no enforcement penalties). The moderate component reflects that states do face diplomatic costs for downward revisions and some institutional pressure toward convergence on UNFCCC norms. Suppression (0.35): Moderate. States face multiple barriers to exit: diplomatic costs of breaking publicly stated commitments, participation requirements for climate finance access, reputational penalties from climate-focused constituencies, and path-dependent lock-in to disclosed pledges. However, suppression is not high because legal enforcement is absent and states do revise downward when circumstances change. Theater ratio (0.52): Moderate. The mechanism exhibits substantial performative elements: biennial reporting and COP sessions provide the appearance of progress; states frequently miss implementation deadlines with minimal consequence; institutional effort often flows toward reporting infrastructure rather than emissions reductions. However, theater is not dominant (≥0.70) because genuine coordination functions exist: NDCs do coordinate expectations, do mobilize climate finance, and do establish norms against unilateral defection. The rising trajectory (0.38→0.52) reflects widening implementation gaps as the 2015-2020 period has shown that pledged reductions are insufficient for 1.5°C targets. Claimed type (Rope): The sovereigntist reading's base classification. Low extractiveness + presence of genuine coordination function (states solve collective action on atmospheric stability) + voluntary character with revision freedom → Rope from the beneficiary's perspective.
 *
 * PERSPECTIVAL GAP:
 *   The sovereigntist reading produces a rope classification from powerful and institutional perspectives (beneficiaries who retain exit options) but tangled_rope or piton from moderate and institutional perspectives that experience disproportionate burdens or institutional degradation. The most instructive gap is between the powerful fossil-dependent economy (Rope: genuine coordination with preserved sovereignty) and the low-capacity state (Tangled Rope: coordination mixed with subtle extraction via reporting burdens and pledge lock-in). Both states face the same treaty framework, but their structural capacity to exploit exit options differs. The piton perspective on the UNFCCC apparatus reveals institutional inertia: the organization maintains its mandate and budget by perpetuating the NDC process even as the mechanism's primary function (global emissions reduction) remains unmet. The mountain perspective from the analytical observer naturalizes the sovereigntist reading's core premise (state energy sovereignty is an immutable feature of international law) in ways that obscure the reading's contestation with alternative framings.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) in this constraint is derived from beneficiary/victim declarations and exit options. Fossil-dependent economies as primary beneficiaries with arbitrage exit options (can revise pledges, access finance from multiple sources, adjust energy policy unilaterally) derive low d → low f(d) → low χ. The mechanism benefits them more than it extracts from them. National planners as institutional beneficiaries with arbitrage options similarly derive low d. Low-capacity states as moderate-power actors with constrained exit (higher reporting burden relative to capacity, public pledge creates path-dependent expectations) derive higher d than beneficiaries but lower than trapped agents. The UNFCCC apparatus as an institutional actor extracts benefit (institutional legitimacy, budget justification) from NDC administration; its d is near zero despite arbitrage exit options because the institution's survival is path-dependent on the mechanism's perpetuation. The fossil-dependent economy's perception of low extraction relies on the assumption that exit options are genuinely costless — this assumption is contested by alternative readings that emphasize reputational costs, climate finance conditionality, and technological lock-in. The sovereigntist reading assigns low d by treating exit costs as acceptable policy adjustments rather than severe penalties.
 *
 * MANDATROPHY ANALYSIS:
 *   The sovereigntist reading resolves mandatrophy by explicitly acknowledging that the NDC mechanism trades coordination capacity for exit preservation. The 'mandate' of the Paris Agreement from this reading is to coordinate emissions reductions while preserving state energy sovereignty — not to achieve specific temperature targets. This allows the mechanism to claim success as a coordination tool (states do coordinate, do revise pledges, do participate) while being silent on whether the coordination achieves climate stability. Alternative readings would place the temperature target as the mandate — in which case the sovereigntist reading appears to have abandoned the primary mandate in favor of the procedural one. The measured theater ratio (0.52) supports the mandatrophy resolution: the mechanism functions as procedure and institution-building but exhibits moderate performative character as a climate action tool. Accepting rope classification requires accepting that coordination without enforcement is a legitimate reading of 'solving climate change' — which the sovereigntist reading does explicitly.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_vs_enforcement_trade_off,
    'Is the preservation of state energy sovereignty compatible with achieving global climate targets, or does it necessarily require accepting insufficient aggregate action?',
    'Comparative analysis: empirical assessment of whether any binding global climate mechanism has been ratified by major emitters without sovereignty-preserving exit clauses; modeling of whether 1.5°C-compatible pathways require coordinated sectoral targets or can be achieved through voluntary national planning',
    'If sovereignty-preserving mechanisms can achieve targets: the sovereigntist reading is structurally sound and rope classification is stable. If sovereignty-preserving necessarily implies insufficient action: the reading naturalizes a structural inadequacy (false summit risk — should reclassify to piton or snare depending on intentionality of the insufficiency).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_vs_enforcement_trade_off, empirical, 'Compatibility of sovereignty preservation with climate target achievement').

omega_variable(
    reading_premise_vs_supranational_foreclosure,
    'Does the sovereigntist reading''s foundational premise (states retain unilateral revision freedom) logically foreclose the supranational reading''s premise (binding enforcement of centrally-set targets), or do they merely represent different parties'' positions in an ongoing contestation?',
    'Legal/conceptual analysis: does Article 4''s language support both readings within the same treaty framework, or does accepting one reading require rejecting the other''s interpretation of the treaty text? Historical analysis: have states operated under both readings in different institutional contexts (e.g., accepting binding emission limits in the Montreal Protocol while preserving revision freedom in Paris)?',
    'If logically independent: coexists_with relation is correct. If mutually exclusive: forecloses relation is correct. The distinction determines whether these are competing frameworks or live options for different parties.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_premise_vs_supranational_foreclosure, conceptual, 'Whether sovereigntist premise logically forecloses supranational premise').

omega_variable(
    theater_ratio_tracking,
    'Is the measured theater ratio (0.52) tracking upward as the gap between pledged and delivered emissions widens, or stable because states have normalized the gap?',
    'Time-series analysis of: (a) ratio of NDC pledge strength to actual emissions trajectory; (b) ratio of narrative claims about implementation to verified implementation on the ground; (c) institutional effort devoted to NDC reporting vs implementation monitoring',
    'Rising theater suggests the constraint is degrading toward piton (the mechanism increasingly performative). Stable theater suggests the theater is built into the mechanism''s design (coordinate on reporting, not on outcomes). Declining theater would suggest implementation capacity is increasing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_ratio_tracking, empirical, 'Trajectory of theater ratio as implementation gap changes').

omega_variable(
    natural_law_vs_constructed_reading,
    'Is the sovereigntist reading''s foundation (states cannot surrender energy sovereignty in international law) a constraint of international law''s structure, or a specific reading of treaty language that other readings reject?',
    'Textual analysis of Article 4 Paris Agreement; comparison with language in alternative treaty regimes (Montreal Protocol, Kyoto Protocol, EU ETS) to determine whether sovereignty-preserving language is necessary or contingent; analysis of state ratification debates to determine whether sovereignty preservation was a disclosed essential requirement or a revealed preference',
    'If structural necessity: the reading''s mountain classification is defensible. If contingent reading: false summit detection fires, and the constraint should be reclassified based on actual functioning rather than naturalized limit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_reading, conceptual, 'Whether sovereignty preservation is a structural necessity or contingent reading choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(paris_article_4_ndc__sovereigntist_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ndc_sov_tr_t0, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(ndc_sov_tr_t3, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 3, 0.46).
narrative_ontology:measurement(ndc_sov_tr_t6, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 6, 0.52).

% Extraction over time
narrative_ontology:measurement(ndc_sov_be_t0, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(ndc_sov_be_t3, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 3, 0.22).
narrative_ontology:measurement(ndc_sov_be_t6, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 6, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(paris_article_4_ndc__sovereigntist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(paris_article_4_ndc__sovereigntist_reading, 0.15).
narrative_ontology:affects_constraint(paris_article_4_ndc__sovereigntist_reading, paris_article_4_ndc_supranational_reading).
narrative_ontology:affects_constraint(paris_article_4_ndc__sovereigntist_reading, paris_article_4_ndc_equity_reading).
narrative_ontology:affects_constraint(paris_article_4_ndc__sovereigntist_reading, climate_finance_access_bottleneck).
narrative_ontology:affects_constraint(paris_article_4_ndc__sovereigntist_reading, fossil_fuel_transition_commitment).

% DUAL FORMULATION NOTE:
% The sovereigntist reading of Article 4 is one point in a presheaf of constraint structures over the same treaty text. The supranational and equity readings instantiate different constraints with different epsilon values, different beneficiary/victim structures, and different classifications. The network edges indicate that all three readings operate on the same kernel and affect downstream constraints (climate finance conditionality, transition commitment dynamics). They should be authored as separate JSON files and linked via network.affects_constraints to preserve the reading contest explicitly in the compiled knowledge base.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
