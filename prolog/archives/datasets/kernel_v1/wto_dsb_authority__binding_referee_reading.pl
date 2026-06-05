% ============================================================================
% CONSTRAINT STORY: wto_dsb_authority__binding_referee_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_wto_dsb_authority__binding_referee_reading, []).

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
 *   constraint_id: wto_dsb_authority__binding_referee_reading
 *   human_readable: WTO Dispute Settlement Body Binding Authority (Binding Referee Reading)
 *   domain: international_law/trade_governance/institutional_legitimacy
 *
 * SUMMARY:
 *   The WTO Dispute Settlement Body (DSB) exercises binding authority over
 *   trade disputes between member states through the Understanding on Rules
 *   and Procedures Governing the Settlement of Disputes (DSU). This
 *   constraint, under the binding-referee reading, models the DSB as a
 *   binding arbiter whose rulings impose legal obligations on member states,
 *   with non-compliance triggering authorization for the winning state to
 *   impose retaliatory tariffs. Member states have surrendered policy
 *   discretion within WTO-covered domains (goods, services, intellectual
 *   property) in exchange for market access commitments and dispute
 *   resolution certainty. This reading generates a tangled-rope
 *   classification: the mechanism provides genuine coordination benefits
 *   (reduces bilateral power imbalances, establishes neutral dispute
 *   resolution) alongside significant extraction (constrains smaller states'
 *   policy autonomy, amplifies larger states' litigation capacity, authorizes
 *   retaliatory sanctions). The constraint exhibits measurable drift: base
 *   extractiveness has risen from 0.35 (early WTO period, 1995) to 0.58
 *   (contemporary period, 2015) as larger states have weaponized dispute
 *   settlement for strategic purposes, smaller states have accumulated
 *   adverse rulings, and the institutional authority has become increasingly
 *   asymmetric in application. Theater ratio has risen modestly (0.25 to
 *   0.42), reflecting the Appellate Body's dysfunction since 2017 —
 *   procedural theater now substitutes for genuine appeal review. Suppression
 *   has intensified (0.52 to 0.65) as retaliation threats have become more
 *   credible and smaller states have fewer outside options. This reading
 *   contrasts structurally with the advisory-coordination reading (which
 *   models DSB rulings as recommendations with reputational enforcement
 *   rather than binding authority) and the judicial-activism reading (which
 *   critiques the DSB for exceeding its mandate in interpreting treaty text).
 *   This is ONE reading of how the WTO institutional architecture
 *   instantiates authority — others remain live positions in international
 *   law discourse.
 *
 * KEY AGENTS:
 *   - Large Trading Powers (US, EU, China): Primary beneficiaries (institutional/arbitrage) — use dispute mechanism to enforce favorable interpretations of treaty obligations; can litigate multiple disputes simultaneously and credibly threaten retaliation
 *   - Export-Oriented Sectors within Trading Powers: Primary beneficiaries (powerful/mobile) — win DSB rulings that force market access for their products; benefit from binding authority that removes policy discretion from importing states
 *   - Smaller/Developing States: Primary victims (powerless/trapped) — subject to DSB rulings they cannot overturn; face retaliation threat if they do not comply; lack resources to litigate complex disputes; policy autonomy surrendered to WTO obligations
 *   - Domestic Policy Sectors in All States: Secondary victims (moderate/constrained) — non-export-competitive sectors (agriculture in developed countries, infant industries in developing countries) lose protection when DSB rulings constrain import barriers
 *   - WTO Secretariat and Panelists: Institutional actors (institutional/arbitrage) — maintain the binding-authority fiction through procedural theater; benefit from continued disputes (institutional funding, panel appointments, expert status); have incentive to keep the mechanism functioning
 *   - WTO Reform Coalition: Organized actors (organized/constrained) — developing countries, labor/environmental advocacy coalitions, states seeking appellate reform; pushing for sunset clauses on DSB authority, special carve-outs, or conversion to advisory model
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the binding-authority model as an inherent feature of trade governance rather than a contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wto_dsb_authority__binding_referee_reading, 0.58).
domain_priors:suppression_score(wto_dsb_authority__binding_referee_reading, 0.65).
domain_priors:theater_ratio(wto_dsb_authority__binding_referee_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, theater_ratio, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wto_dsb_authority__binding_referee_reading, tangled_rope).
narrative_ontology:human_readable(wto_dsb_authority__binding_referee_reading, "WTO Dispute Settlement Body Binding Authority (Binding Referee Reading)").
narrative_ontology:topic_domain(wto_dsb_authority__binding_referee_reading, "international_law/trade_governance/institutional_legitimacy").

domain_priors:requires_active_enforcement(wto_dsb_authority__binding_referee_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(wto_dsb_authority__binding_referee_reading, '1cfd4c81-b8b4-4fa2-8297-7e041638fc4d').
narrative_ontology:cs_kernel_codification('1cfd4c81-b8b4-4fa2-8297-7e041638fc4d', formalized).
narrative_ontology:cs_authority_grounding('1cfd4c81-b8b4-4fa2-8297-7e041638fc4d', extraction).
narrative_ontology:cs_interpretation_layer_present('1cfd4c81-b8b4-4fa2-8297-7e041638fc4d').
narrative_ontology:cs_reading_relation('1cfd4c81-b8b4-4fa2-8297-7e041638fc4d', wto_dsb_authority__advisory_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('1cfd4c81-b8b4-4fa2-8297-7e041638fc4d', wto_dsb_authority__judicial_activism_reading, influences).
narrative_ontology:cs_axiom('1cfd4c81-b8b4-4fa2-8297-7e041638fc4d', foundational, sovereignty_voluntarily_exchanged_for_market_access).
narrative_ontology:cs_axiom_status(sovereignty_voluntarily_exchanged_for_market_access, holdable).
narrative_ontology:cs_axiom_grounding('1cfd4c81-b8b4-4fa2-8297-7e041638fc4d', sovereignty_voluntarily_exchanged_for_market_access, conventional).
narrative_ontology:cs_axiom('1cfd4c81-b8b4-4fa2-8297-7e041638fc4d', foundational, binding_authority_enables_credible_compliance).
narrative_ontology:cs_axiom_status(binding_authority_enables_credible_compliance, holdable).
narrative_ontology:cs_axiom_grounding('1cfd4c81-b8b4-4fa2-8297-7e041638fc4d', binding_authority_enables_credible_compliance, instrumental).
narrative_ontology:cs_reference_frame('1cfd4c81-b8b4-4fa2-8297-7e041638fc4d', treaty_based_binding_authority).
narrative_ontology:cs_drift_state('1cfd4c81-b8b4-4fa2-8297-7e041638fc4d', contemporary_appellate_dysfunction_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('1cfd4c81-b8b4-4fa2-8297-7e041638fc4d', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(wto_dsb_authority__binding_referee_reading, wto_dsb_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wto_dsb_authority__binding_referee_reading, larger_trading_powers).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__binding_referee_reading, export_oriented_sectors).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__binding_referee_reading, dispute_winning_states).
narrative_ontology:constraint_victim(wto_dsb_authority__binding_referee_reading, smaller_trading_powers).
narrative_ontology:constraint_victim(wto_dsb_authority__binding_referee_reading, domestic_policy_autonomy).
narrative_ontology:constraint_victim(wto_dsb_authority__binding_referee_reading, non_export_competitive_sectors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALL DEVELOPING STATE (SNARE) — A weak state subject to a DSB ruling against its domestic policy faces retaliation authorization with no meaningful recourse. Trapped by treaty obligation and economic dependence on larger trading partners. Bears full extraction cost: policy capacity surrendered, retaliatory tariff threat, no negotiating leverage to challenge the ruling's interpretation. The binding authority mechanism ensures compliance through coercion rather than coordination.
constraint_indexing:constraint_classification(wto_dsb_authority__binding_referee_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MID-SIZED STATE (TANGLED ROPE) — Faces genuine coordination problem (bilateral disputes benefit from neutral arbiter) alongside extractive pressure (DSB ruling constrains domestic policy discretion in ways that may not serve national interest). Can litigate alternatives or negotiate exceptions but at significant cost. Benefits from predictable dispute resolution but loses policy autonomy. Constrained by both retaliation threat and existing legal obligations.
constraint_indexing:constraint_classification(wto_dsb_authority__binding_referee_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EXPORT-SECTOR COALITION / DISPUTE WINNER (ROPE) — Wins a DSB ruling that enforces market access. Experiences the binding authority as pure coordination: neutral arbiter eliminates the need for bilateral power negotiation; the binding ruling ensures compliance without requiring the winner to negotiate or reciprocate. Arbitrage available through forum-shopping (filing multiple complaints against the same state or coordinating with allies). Net beneficiary — the constraint coordinates dispute resolution in the winner's favor.
constraint_indexing:constraint_classification(wto_dsb_authority__binding_referee_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LARGE TRADING POWER (TANGLED ROPE) — A major economy experiences genuine coordination benefits (DSB prevents escalatory trade wars, enables predictable access) alongside the extractive dimension: can use dispute mechanism to enforce favorable interpretations of ambiguous treaty text. The binding authority amplifies large-state power because large states have capacity to litigate complex disputes and enforce retaliation credibly. Mobile exit (can withdraw; threatens to withdraw; can create alternative trade blocs) gives some leverage, but the constraint still extracts through legal authority.
constraint_indexing:constraint_classification(wto_dsb_authority__binding_referee_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: WTO REFORM COALITION (SCAFFOLD) — Organized actors (states, civil society) advocating for appellate reform, developing-country carve-outs, or renegotiation of dispute settlement rules see the current binding authority as a temporary institutional form with a sunset clause. Constrained by the need to maintain WTO consensus, but have structured alternatives (bilateral investment courts, regional trade agreements with modified dispute procedures). Theater is lower here — the debate about binding authority is explicit and ongoing; the mechanism is not maintaining itself through invisibility.
constraint_indexing:constraint_classification(wto_dsb_authority__binding_referee_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: WTO INSTITUTIONAL CONTINUITY (PITON) — The DSB itself, as an institutional authority, maintains the binding-referee fiction through procedural theater. The appellate mechanism is broken (Appellate Body non-functional as of 2020); large states have weaponized dispute settlement for strategic purposes; developing countries view the process as biased toward powerful states. Yet the institution persists because exit is costly (unilateral withdrawal from WTO triggers massive trade losses) and because the procedural formality creates theater: binding rulings are announced, states appear to comply, the ritual continues despite functional degradation. The WTO Secretariat maintains the binding authority classification through procedural inertia rather than through genuine neutral arbitration.
constraint_indexing:constraint_classification(wto_dsb_authority__binding_referee_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN — FALSE SUMMIT CANDIDATE) — From a civilizational/universal analytical lens, binding dispute settlement authority might appear as an irreducible feature of any international trade system: any system that coordinates trade across borders must have some mechanism to settle disputes, and that mechanism must have enforcement power to be credible. This perspective risks naturalizing the specific institutional form (WTO DSB with retaliation authorization) as an inevitable law of international trade. However, the structural data reveals this as a false summit: the binding authority is a contingent institutional choice that benefits specific actors (large exporters, dispute-winning states) and harms others (smaller states, domestic policy autonomy). The mountain reading papers over the beneficiary/victim structure that the tangled-rope reading reveals.
constraint_indexing:constraint_classification(wto_dsb_authority__binding_referee_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wto_dsb_authority__binding_referee_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(wto_dsb_authority__binding_referee_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(wto_dsb_authority__binding_referee_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(wto_dsb_authority__binding_referee_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(wto_dsb_authority__binding_referee_reading, TR),
    TR >= 0.70.

:- end_tests(wto_dsb_authority__binding_referee_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The binding authority extracts from states that lose disputes (constrains policy autonomy, imposes compliance cost, triggers retaliation threat) and benefits from states that win (enforces market access without negotiation cost). The extraction is not as severe as pure snare (0.66+) because coordination benefits exist: the DSB does reduce bilateral power imbalances and create predictable dispute resolution. But the extraction exceeds pure rope (0.35) because the binding authority asymmetrically amplifies larger states' power — they can litigate strategically and enforce compliance through retaliation. The rise from 0.35 to 0.58 over 20 years reflects the mechanism becoming increasingly weaponized: early WTO period had more genuine coordination; contemporary period shows more strategic use by large states. Suppression (0.65): High. Smaller states face multiple suppressive barriers: (1) economic dependence on trading partners makes retaliation threat credible, (2) lack of litigation resources compared to large states, (3) limited outside options (non-WTO trading arrangements offer less market access), (4) binding authority removes policy discretion even when state believes policy is justified. The rise from 0.52 to 0.65 reflects accumulating retaliation threats and declining appeal options (Appellate Body dysfunction). Theater ratio (0.42): Moderate. The DSB mechanism is not as theatrical as piton-level constraints (0.70+) because the binding authority is explicitly acknowledged and contested — the WTO reform debate is public. However, theater has risen modestly because the Appellate Body's collapse means that 'binding' decisions are now made by panels without appeal review, yet the procedural formality of binding classification persists. The mechanism maintains legitimacy through ritual (voting procedures, legal reasoning) even when functional neutral arbitration has degraded.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. The dispute-winning state (large power, arbitrage exit) experiences pure Rope — neutral arbiter solves coordination problem, enables market access enforcement, creates predictable dispute resolution with no felt extraction cost. The losing state (small power, trapped exit) experiences pure Snare — policy autonomy surrendered, retaliation threat real, no negotiating leverage, extraction guaranteed. A mid-sized state experiences Tangled Rope — mixed coordination and extraction. The WTO institutional apparatus experiences Piton — the binding-authority mechanism persists through procedural inertia despite the Appellate Body dysfunction degrading its neutral-arbiter function. The analytical observer at civilizational scope risks Mountain — seeing binding authority as an inherent feature of international trade rather than a contingent institutional choice. This perspectival gap is the diagnostic signal: if all observers agreed on Tangled Rope, the constraint might be transparent (everyone sees both coordination and extraction). But the gap between pure Rope (for winners) and pure Snare (for losers) reveals the asymmetric extraction that the binding-authority mechanism enables.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from each agent's structural position: power level, exit options, and relationship to the extraction flow. Smaller states subject to adverse DSB rulings have high d (0.90+) — they are full targets of the binding authority mechanism. Large trading powers with dispute-winning records have low d (0.15-0.25) — they are net beneficiaries whose interests are served by binding authority. Export-sector coalitions have near-zero d (arbitrage exit available; can litigate alternatives or coordinate with allies). The WTO institutional apparatus has low d (institutional power, arbitrage exit through continued dispute activity). The analytical observer has canonical d ≈ 0.72 (analytical power, analytical exit). This distributes extraction asymmetrically: the effective extraction chi (χ = ε × f(d) × σ(S)) is highest for powerless agents at high scope (smaller states at global scale) and lowest for institutional beneficiaries with arbitrage options. The scope modifier σ(S) = 1.2 at global scale amplifies the effective extraction — the WTO operates at maximum scope, which amplifies the extraction experienced by those at the bottom of the power distribution.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by acknowledging that the binding-referee reading is ONE reading of the WTO DSU, not the unique correct reading. The mandate problem arises because the same institutional structure (dispute settlement with retaliation authorization) is classified as Rope by beneficiaries (coordination benefits), Snare by victims (pure extraction), Tangled Rope by the structural analyst (mixed), and Mountain by those who naturalize it (inherent to trade). The binding-referee reading chooses Tangled Rope as the claimed type because it acknowledges both the genuine coordination function (reduces bilateral power imbalances, establishes neutral dispute resolution) and the extractive asymmetry (binding authority amplifies larger states' power, constrains smaller states' policy autonomy). The alternative readings (advisory-coordination reading, judicial-activism reading) would produce different extractiveness values and different claimed types. The mandate is resolved not by finding the 'correct' single classification but by specifying which reading is being instantiated and what structural features it emphasizes. Under the binding-referee reading, the constraint is Tangled Rope. Under the advisory-coordination reading, it would classify as Rope (coordination without binding enforcement). Under the judicial-activism reading, it might classify as Snare (institutional overreach in interpreting treaty text to expand jurisdiction). Each reading has different ε and different perspectives because each emphasizes different structural features of the same institutional mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mandatory_vs_permissive_interpretation,
    'Does the DSB''s binding authority derive from explicit treaty text mandating binding rulings, or from an interpretation of the Understanding on Rules and Procedures Governing the Settlement of Disputes (DSU) that reads ''binding'' into ambiguous procedural language?',
    'Textual analysis of DSU Article 16-17 on binding force; historical record of negotiation intent; comparative analysis with other international court mandates (ICJ, ITLOS, regional courts)',
    'If mandatory: the binding authority is a direct treaty commitment and exit requires formal amendment (very high cost). If permissive interpretation: the binding authority is contestable — a sibling reading (advisory coordination reading) becomes structurally viable, and the dispute settlement mechanism could be reformed without treaty amendment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandatory_vs_permissive_interpretation, conceptual, 'Whether binding authority is textually mandated or interpretively derived').

omega_variable(
    retaliation_authorization_legitimacy,
    'Does the authorization of retaliation by the losing state constitute a binding enforcement mechanism or a coercive sanction that violates principles of voluntary treaty compliance?',
    'Comparative international law analysis of enforcement mechanisms in other treaties; empirical study of retaliation effectiveness (do threatened states actually comply with DSB rulings, or do they pay the retaliation cost and maintain the policy?); normative analysis of whether treaty-authorized retaliation is consistent with good-faith compliance.',
    'If retaliation is viewed as legitimate enforcement: snare perspective is confirmed for losing states (trapped by enforcement mechanism). If retaliation is viewed as illegitimate coercion: the binding authority becomes a forced extraction mechanism and the constraint reclassifies as pure snare even from institutional perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retaliation_authorization_legitimacy, preference, 'Whether retaliation authorization constitutes legitimate enforcement or illegitimate coercion').

omega_variable(
    appellate_dysfunction_vs_design,
    'Is the Appellate Body''s non-functionality (blocked by US vetoes since 2017) a temporary institutional failure or a signal that the binding-authority model is structurally unworkable at scale?',
    'Longitudinal study of dispute outcomes before and after Appellate Body collapse; analysis of whether disputes are being resolved through alternative mechanisms (bilateral negotiation, regional courts, arbitration); assessment of whether blocking appellate review increases small-state perception of bias.',
    'If temporary failure: scaffold perspective is confirmed (reform coalition can rebuild the institution). If structural unworkability: the WTO DSB binding authority is degraded beyond repair (piton classification becomes permanent) and alternative dispute mechanisms will proliferate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(appellate_dysfunction_vs_design, empirical, 'Whether Appellate Body dysfunction is temporary or indicates structural collapse').

omega_variable(
    special_and_differential_treatment_adequacy,
    'Do existing special and differential treatment (SDT) provisions in DSU (longer implementation periods, carve-outs for least-developed countries) adequately mitigate the extraction from smaller states, or are they performative concessions that leave the underlying binding authority intact?',
    'Empirical analysis of compliance rates for LDC rulings vs. large-state rulings; assessment of whether SDT provisions are invoked and whether they alter enforcement pressure; case studies of SDT application in actual disputes.',
    'If adequate: the constraint reclassifies toward scaffold from small-state perspective (temporary with exits). If performative: small-state snare classification is confirmed (extraction persists despite concessions); the theater_ratio rises (SDT provisions are ritual theater, not functional mitigation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(special_and_differential_treatment_adequacy, empirical, 'Whether special and differential treatment provisions mitigate extraction').

omega_variable(
    binding_vs_advisory_coordinated_equilibrium,
    'Could the same dispute-settlement function be achieved through advisory rulings (non-binding recommendations) with high reputational cost for non-compliance, or does binding authority add material enforcement that coordination-alone cannot provide?',
    'Comparative case studies of dispute systems with advisory vs. binding authority (e.g., ITLOS advisory opinions vs. binding rulings; WTO dispute settlement vs. UNCLOS arbitration); game-theoretic model of compliance incentives under advisory vs. binding authority.',
    'If advisory is sufficient: the binding authority is an unnecessary extraction layer and the constraint could reclassify as pure rope (coordination only) under an advisory reading. If binding adds material enforcement: the binding authority is structurally necessary and the tangled-rope classification is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(binding_vs_advisory_coordinated_equilibrium, empirical, 'Whether binding authority is necessary or advisory rulings would suffice').

omega_variable(
    reading_contest_foreclosure,
    'Does this binding-referee reading logically foreclose the advisory-coordination reading within a single coherent institutional framework, or can both readings coexist as live positions held by different state coalitions?',
    'Analysis of whether a state party could simultaneously hold that DSB rulings are advisory (in disputes it loses) and binding (in disputes it wins) — i.e., whether the reading is systematically unstable or whether inconsistent application is empirically tolerated.',
    'If foreclosure: the reading_relations declaration should list ''forecloses'' for the advisory reading. If coexistence: the relation is ''coexists_with'' (different parties hold different readings in a live dispute).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_foreclosure, conceptual, 'Whether binding and advisory readings can coexist in the same institutional framework').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wto_dsb_authority__binding_referee_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wto_dsb_theater_1995, wto_dsb_authority__binding_referee_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(wto_dsb_theater_2005, wto_dsb_authority__binding_referee_reading, theater_ratio, 10, 0.38).
narrative_ontology:measurement(wto_dsb_theater_2015, wto_dsb_authority__binding_referee_reading, theater_ratio, 20, 0.42).

% Extraction over time
narrative_ontology:measurement(wto_dsb_extract_1995, wto_dsb_authority__binding_referee_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(wto_dsb_extract_2005, wto_dsb_authority__binding_referee_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(wto_dsb_extract_2015, wto_dsb_authority__binding_referee_reading, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(wto_dsb_suppress_1995, wto_dsb_authority__binding_referee_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(wto_dsb_suppress_2005, wto_dsb_authority__binding_referee_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(wto_dsb_suppress_2015, wto_dsb_authority__binding_referee_reading, suppression_requirement, 20, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wto_dsb_authority__binding_referee_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(wto_dsb_authority__binding_referee_reading, wto_dispute_initiation_access).
narrative_ontology:affects_constraint(wto_dsb_authority__binding_referee_reading, wto_appellate_body_functionality).
narrative_ontology:affects_constraint(wto_dsb_authority__binding_referee_reading, trade_retaliation_authorization).

% DUAL FORMULATION NOTE:
% The binding-referee reading is part of the wto_dsb_authority kernel family. The advisory-coordination reading (wto_dsb_authority__advisory_coordination_reading) would decompose the same institutional mechanism with a different ε value (lower, ~0.30–0.35, classified as Rope). The judicial-activism reading (wto_dsb_authority__judicial_activism_reading) would emphasize interpretive overreach with a higher ε value (~0.65–0.72, classified as Snare). All three readings are structurally distinct constraint stories that address the same institutional phenomenon (WTO dispute settlement) but emphasize different aspects of how authority is grounded and exercised. The ε difference reflects genuine structural disagreement about the mechanism's extractive force, not observational uncertainty about a single constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(wto_dsb_authority__binding_referee_reading, institutional, 0.18).
constraint_indexing:directionality_override(wto_dsb_authority__binding_referee_reading, powerful, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
