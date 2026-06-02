% ============================================================================
% CONSTRAINT STORY: electronic_money_emergence__first_held_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_electronic_money_emergence__first_held_reading, []).

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
 *   constraint_id: electronic_money_emergence__first_held_reading
 *   human_readable: Electronic Money Emergence (First Institutional Holding Reading)
 *   domain: economic_history/monetary_theory/technology
 *
 * SUMMARY:
 *   Electronic money emergence, under the 'first held' reading, is marked by
 *   a discrete institutional event: the moment when a recognized authority
 *   (central bank or commercial banking institution) first held
 *   dematerialized currency in a form legally and operationally
 *   distinguishable from physical notes. This constraint models the
 *   institutional transition from bearer instruments (physical coin and note)
 *   to account-based electronic representation. The reading treats emergence
 *   as a bounded ontological shift, not as a gradual technological diffusion
 *   or intellectual precondition. The constraint exhibits the characteristic
 *   structure of an institutional extraction mechanism: genuine coordination
 *   benefits (faster settlement, reduced physical security costs, monetary
 *   policy transmission) coexist with asymmetric extraction (loss of bearer
 *   optionality, mandatory institutional intermediation, financial
 *   surveillance, account fee extraction). The tension between coordination
 *   and extraction drives classification as Tangled Rope from multiple
 *   perspectives. The measurement trajectory shows extractiveness and
 *   suppression requirement both rising over time as electronic architecture
 *   becomes mandatory and physical alternatives are regulated away,
 *   suggesting a constraint that began as mixed coordination (ε=0.12,
 *   suppression=0.15) and hardened into more extractive form (ε=0.38,
 *   suppression=0.42) as institutional dependence became total.
 *
 * KEY AGENTS:
 *   - Banking System: Primary beneficiary (institutional/arbitrage) — gains deposit collection, loan origination capacity, reduced physical vault constraints; experiences electronic substrate as enabling fundamental shift in monetary intermediation
 *   - Central Authority: Primary beneficiary (institutional/arbitrage) — gains monetary control, transaction visibility, inflation tax capacity; electronic substrate enables monetary policy mechanisms impossible with physical bearer money
 *   - Unbanked Populations: Primary victim (powerless/trapped) — forced into electronic systems without option to remain in physical bearer instruments; bears costs of account requirements, fees, digital surveillance, exclusion from informal cash economy
 *   - Transitional Merchants: Secondary victim (moderate/constrained) — experience mixed benefit/cost; gain settlement speed and theft reduction but lose cash-payment optionality and face mandatory digital infrastructure dependency
 *   - Physical Money System: Institutional actor (institutional/arbitrage) — degrades to piton status; continues through regulatory inertia and legitimacy claim despite electronic dominance
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional choice as technological inevitability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(electronic_money_emergence__first_held_reading, 0.38).
domain_priors:suppression_score(electronic_money_emergence__first_held_reading, 0.42).
domain_priors:theater_ratio(electronic_money_emergence__first_held_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(electronic_money_emergence__first_held_reading, tangled_rope).
narrative_ontology:human_readable(electronic_money_emergence__first_held_reading, "Electronic Money Emergence (First Institutional Holding Reading)").
narrative_ontology:topic_domain(electronic_money_emergence__first_held_reading, "economic_history/monetary_theory/technology").

domain_priors:requires_active_enforcement(electronic_money_emergence__first_held_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(electronic_money_emergence__first_held_reading, '0e54cbfb-0e11-40cb-a151-ae79581dbaab').
narrative_ontology:cs_kernel_codification('0e54cbfb-0e11-40cb-a151-ae79581dbaab', fixed_text).
narrative_ontology:cs_authority_grounding('0e54cbfb-0e11-40cb-a151-ae79581dbaab', extraction).
narrative_ontology:cs_interpretation_layer_present('0e54cbfb-0e11-40cb-a151-ae79581dbaab').
narrative_ontology:cs_reading_relation('0e54cbfb-0e11-40cb-a151-ae79581dbaab', electronic_money_emergence__became_thinkable_reading, influences).
narrative_ontology:cs_reading_relation('0e54cbfb-0e11-40cb-a151-ae79581dbaab', electronic_money_emergence__m4_m5_collapse_reading, influences).
narrative_ontology:cs_axiom('0e54cbfb-0e11-40cb-a151-ae79581dbaab', foundational, institutional_choice_model_of_emergence).
narrative_ontology:cs_axiom_status(institutional_choice_model_of_emergence, holdable).
narrative_ontology:cs_axiom_grounding('0e54cbfb-0e11-40cb-a151-ae79581dbaab', institutional_choice_model_of_emergence, empirically_contingent).
narrative_ontology:cs_axiom('0e54cbfb-0e11-40cb-a151-ae79581dbaab', foundational, beneficiary_extraction_via_bearer_instrument_elimination).
narrative_ontology:cs_axiom_status(beneficiary_extraction_via_bearer_instrument_elimination, holdable).
narrative_ontology:cs_axiom_grounding('0e54cbfb-0e11-40cb-a151-ae79581dbaab', beneficiary_extraction_via_bearer_instrument_elimination, empirically_contingent).
narrative_ontology:cs_reference_frame('0e54cbfb-0e11-40cb-a151-ae79581dbaab', institutional_monetary_intermediation).
narrative_ontology:cs_drift_state('0e54cbfb-0e11-40cb-a151-ae79581dbaab', contemporary_digital_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0e54cbfb-0e11-40cb-a151-ae79581dbaab', '').
narrative_ontology:cs_kernel_id(electronic_money_emergence__first_held_reading, electronic_money_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(electronic_money_emergence__first_held_reading, banking_institutions).
narrative_ontology:constraint_beneficiary(electronic_money_emergence__first_held_reading, central_authorities).
narrative_ontology:constraint_victim(electronic_money_emergence__first_held_reading, physical_money_users).
narrative_ontology:constraint_victim(electronic_money_emergence__first_held_reading, monetary_epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNBANKED POPULATIONS (SNARE) — Forced into electronic systems without option to remain in physical bearer instruments. Trapped by regulatory mandate and institutional infrastructure transition. No exit; full bearing of suppression costs (account requirements, fees, surveillance through digital records).
constraint_indexing:constraint_classification(electronic_money_emergence__first_held_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: TRANSITIONAL MERCHANTS (TANGLED ROPE) — Experience genuine coordination benefit (faster settlement, reduced theft risk from physical cash) alongside asymmetric extraction (loss of cash-payment optionality, mandatory digital infrastructure dependency, merchant transaction fees). Constrained by infrastructure requirements and regulatory pressure but also benefiting from electronic settlement speed.
constraint_indexing:constraint_classification(electronic_money_emergence__first_held_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: BANKING SYSTEM (ROPE) — Net beneficiary. Electronic money enables deposit collection, loan origination, and monetary control without physical vault constraints. Genuine coordination function (settlement efficiency, fractional reserve mediation) exists alongside asymmetric benefit capture. Exit from electronic infrastructure is possible but economically irrational.
constraint_indexing:constraint_classification(electronic_money_emergence__first_held_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CENTRAL AUTHORITY (ROPE) — Gains monetary control, transaction visibility, and inflation tax capacity. Electronic substrate enables monetary policy transmission mechanisms impossible with physical bearer instruments. Coordination function (price stability, liquidity management) is genuine; extraction (seigniorage, financial surveillance) is asymmetric benefit but not maximally coercive.
constraint_indexing:constraint_classification(electronic_money_emergence__first_held_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: PHYSICAL MONEY SYSTEM (PITON) — Degrades from functional constraint to theatrical ritual. Mint operations, currency design, anti-counterfeiting measures persist despite electronic dominance. Maintained through regulatory inertia and institutional legitimacy claims rather than active verification. Theater ratio reflects: central banks still issue coins and notes at declining volumes; physical money persists as symbolic store of value and emergency fallback.
constraint_indexing:constraint_classification(electronic_money_emergence__first_held_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational scope, electronic money emergence appears as inevitable technological determinism: dematerialization is inherent to efficiency, and institutional adoption is an unavoidable law of economic development. This perspective naturalizes what the structural data reveals as a contingent institutional choice with identifiable beneficiaries and victims. False summit candidate.
constraint_indexing:constraint_classification(electronic_money_emergence__first_held_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(electronic_money_emergence__first_held_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(electronic_money_emergence__first_held_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(electronic_money_emergence__first_held_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(electronic_money_emergence__first_held_reading, TR),
    TR >= 0.70.

:- end_tests(electronic_money_emergence__first_held_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The first institutional holding of electronic money created genuine coordination benefits — settlement speed, reduced physical security costs, monetary policy transmission. However, the institutional choice to *mandate* electronic money and regulate away physical bearer alternatives constitutes asymmetric extraction. The value reflects that initial extractiveness was lower (0.12 when electronic and physical coexisted as options) and has risen to 0.38 as physical alternatives have been regulated away, making the constraint more purely extractive over time. Suppression (0.42): Moderate-high. Significant barriers to maintaining physical bearer money include regulatory prohibition of unlicensed note issuance, infrastructure investment favoring electronic settlement, and network effects (merchants increasingly refuse cash). Suppression is not total — informal cash economies persist in many jurisdictions — but the trajectory is toward mandatory institutional intermediation. Theater ratio (0.55): Moderate. The first institutional holding of electronic money required performative elements: regulatory recognition ceremonies, new accounting standards, deposit insurance structures legitimizing electronic over physical. However, the core coordination function (settlement efficiency) is genuine, preventing theater from dominating the constraint. Theater has risen over time (0.25 → 0.55) as physical money persistence requires theatrical justification (central banks issuing coins at declining volumes, emergency fallback narratives) despite electronic dominance.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates perspectival divergence driven by institutional position. The banking system sees coordination (Rope) — electronic substrate solves real settlement problems. The central authority sees genuine monetary control (Rope) — electronic money enables transmission mechanisms impossible with physical bearer instruments. The unbanked see pure extraction (Snare) — they are forced into institutional systems with no option to remain in physical bearer money. The merchant class sees mixed (Tangled Rope) — genuine speed benefits alongside loss of cash optionality. Physical money persists as theatrical ritual (Piton) — central banks issue coins at declining volumes not for function but for institutional legitimacy. The civilizational analytical view risks natural law framing (Mountain) — dematerialization appears inevitable — but the structural data reveals institutional choice: the timing, pace, and regulatory forcing of emergence were decision-contingent, not technological inevitability.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'first held' reading anchors emergence to institutional adoption by a recognized authority. This framing allocates directionality sharply: banking institutions and central authorities are beneficiaries (d ≈ 0.15, institutional power + arbitrage exit → negative effective extraction from their perspective); unbanked populations and physical money users are victims (d ≈ 0.85, powerless/trapped position → high effective extraction). The reading's structure depends on institutional autonomy — if the holding decision was forced by technological inevitability, the beneficiary/victim framing collapses and the mountain perspective gains credibility. If the decision was institutional choice to extract benefits from electronic architecture, the beneficiary/victim structure is justified. The measurement trajectory (extractiveness rising, suppression rising) supports the choice interpretation: extractiveness would be constant if technological; the rise suggests institutional decisions hardening electronic dependence.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy through the reading's explicit anchoring to institutional choice rather than technological law. The beneficiary/victim structure is justified only if the first holding was a decision (to extract coordination benefits) rather than an imposition (technological inevitability). If the analytical observer's mountain framing is correct (emergence is inevitable), then beneficiaries are wrongly named (they did not choose; they merely adapted). The constraint's classification as Tangled Rope depends on maintaining the reading's assertion that institutional autonomy exists — that electronic money was chosen, not forced. This assertion is empirically contestable (Omega 3 and 4), but the reading's coherence requires it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    emergence_threshold_ambiguity,
    'What constitutes ''the first institutional holding'' of dematerialized currency — the first central bank deposit, the first commercial bank account, the first payment settlement on an electronic system, or the first regulatory recognition of electronic money as legal tender?',
    'Historical documentation of specific first-instance transactions and their institutional status; comparison across jurisdictions of recognition timeline; archival evidence of intent and regulatory interpretation at moment of first holding.',
    'Different thresholds produce different emergence dates separated by decades (1950s electronic banking vs 1970s payment systems vs 1990s regulatory frameworks). Each threshold embeds different causal narratives: technological capability vs institutional adoption vs legal recognition. Classification robustness depends on threshold stability.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(emergence_threshold_ambiguity, empirical, 'Definition of institutional holding event that marks emergence').

omega_variable(
    reading_foreclosure_ambiguity,
    'Does this ''first held'' reading foreclose the ''became thinkable'' reading, or do they coexist as complementary framings?',
    'Conceptual analysis: if ''first held'' entails a discrete institutional event (T0), then ''became thinkable'' (T<T0 intellectual precondition) necessarily precedes and enables it. Either the readings occupy different logical scopes (precondition vs event) or they compete for the same ontological slot (which reading gets to count as ''emergence''). The sibling relationship determines which.',
    'If coexist: both readings are live; electronic money has dual emergence (intellectual + institutional). If influences: ''first held'' presupposes ''became thinkable'' as prerequisite. If forecloses: declaring institutional event as emergence rules out intellectual precondition as alternative emergence marker.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_ambiguity, conceptual, 'Logical relationship between institutional and intellectual emergence framings').

omega_variable(
    m4_m5_measurement_class,
    'Is the M4/M5 collapse a consequence of electronic money emergence or a separate monetary aggregation problem driven by credit deregulation?',
    'Chronological analysis of regulatory deregulation timeline vs electronic money adoption; isolation of structural drivers: if electronic substrate enabled the M4 expansion, emergence causally constrains M4 behavior; if deregulation would have produced M4 expansion regardless of substrate, emergence and aggregation collapse are independent constraints.',
    'If causal dependency: electronic money emergence story includes M4 dynamics as downstream effect; m4_m5_collapse_reading should list electronic_money_emergence in its upstream network. If independent: m4 collapse has alternative causal story; network edges do not flow.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(m4_m5_measurement_class, empirical, 'Causal relationship between emergence and monetary aggregation instability').

omega_variable(
    institutional_autonomy_vs_inevitability,
    'Was electronic money emergence a product of institutional choice and policy design, or technological inevitability that institutions were forced to adopt?',
    'Comparative institutional analysis: jurisdictions that resisted electronic money adoption vs those that accelerated; policy debate archives; counterfactual analysis of alternative regulatory frameworks that could have retained physical money dominance.',
    'If choice-driven: beneficiary/victim declarations are justified (institutions chose to extract via electronic architecture). If inevitable: mountain perspective gains credibility (natural law framing becomes more defensible). Determines whether classification is constraint system (chosen) or natural law (imposed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_autonomy_vs_inevitability, conceptual, 'Whether emergence was institutional choice or technological inevitability').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(electronic_money_emergence__first_held_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(emoney_tr_t0, electronic_money_emergence__first_held_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(emoney_tr_t10, electronic_money_emergence__first_held_reading, theater_ratio, 10, 0.4).
narrative_ontology:measurement(emoney_tr_t20, electronic_money_emergence__first_held_reading, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(emoney_be_t0, electronic_money_emergence__first_held_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(emoney_be_t10, electronic_money_emergence__first_held_reading, base_extractiveness, 10, 0.25).
narrative_ontology:measurement(emoney_be_t20, electronic_money_emergence__first_held_reading, base_extractiveness, 20, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(emoney_su_t0, electronic_money_emergence__first_held_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(emoney_su_t10, electronic_money_emergence__first_held_reading, suppression_requirement, 10, 0.3).
narrative_ontology:measurement(emoney_su_t20, electronic_money_emergence__first_held_reading, suppression_requirement, 20, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(electronic_money_emergence__first_held_reading, global_infrastructure).
narrative_ontology:affects_constraint(electronic_money_emergence__first_held_reading, monetary_aggregation_instability).
narrative_ontology:affects_constraint(electronic_money_emergence__first_held_reading, digital_financial_surveillance).

% DUAL FORMULATION NOTE:
% Electronic money emergence under the 'first held' reading is the institutional adoption event that enables downstream constraints (monetary aggregation instability, financial surveillance). The sibling reading 'became_thinkable_reading' focuses on intellectual precondition (theory/design stage); the sibling 'm4_m5_collapse_reading' focuses on macroeconomic consequence. All three are part of the electronic_money_emergence kernel family. Each story has its own ε, beneficiary/victim structure, and temporal scope. The 'first held' reading is unique in anchoring to discrete institutional choice (regulatory recognition event) rather than intellectual precondition or macroeconomic consequence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(electronic_money_emergence__first_held_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
