% ============================================================================
% CONSTRAINT STORY: gulf_arab_monarchies_us_alignment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gulf_arab_monarchies_us_alignment, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: gulf_arab_monarchies_us_alignment
 *   human_readable: Gulf Arab Monarchies US Strategic Alignment
 *   domain: geopolitical/military/economic
 *
 * SUMMARY:
 *   The US-Gulf Arab monarchy alignment emerged as a Cold War security
 *   arrangement and has persisted as the primary geopolitical structure
 *   stabilizing the Middle East region and securing global energy markets.
 *   The constraint binds the United States (seeking regional military primacy
 *   and energy market stability) and Gulf monarchy regimes (seeking regime
 *   security against internal dissent and Iranian power) into an asymmetric
 *   partnership characterized by military dependence, diplomatic
 *   coordination, and implicit endorsement of domestic suppression in
 *   exchange for strategic cooperation. The alignment has enabled significant
 *   extraction of regional self-determination (Palestinian statehood,
 *   independent foreign policies, Islamic governance experimentation) and
 *   externalized costs of regional conflicts (Yemen civil war, proxy
 *   conflicts) onto powerless populations. Over the past 50 years, the
 *   alignment's extractiveness has increased as theater-ratio has risen — the
 *   original coordination function (Cold War deterrence) has been superseded
 *   by performative gestures (democracy promotion rhetoric, defense
 *   technology partnerships) while the core extraction mechanism (military
 *   dependence, petrodollar recycling, suppression of independent regional
 *   governance) has intensified. The constraint now faces structural pressure
 *   from realigning actors (Iran-Saudi détente, Chinese strategic presence,
 *   energy transition) that have reduced the monopoly of US-Gulf alignment as
 *   the only viable security solution.
 *
 * KEY AGENTS:
 *   - United States Strategic Command: Primary beneficiary (institutional/arbitrage) — captures forward military bases, Fifth Fleet positioning, power projection capability; maintains energy market influence
 *   - Gulf Arab Monarchy Regimes (Saudi Arabia, UAE, Qatar, Kuwait, Bahrain, Oman): Primary beneficiary with constrained exit (institutional/constrained) — gain regime security and international legitimacy; constrained by military dependence and diplomatic subordination
 *   - Palestinian Stateless Population: Primary victim (powerless/trapped) — no state capacity, no exit from occupation enabled by alignment; maximum extraction via perpetual dispossession
 *   - Yemen Civilian Population: Primary victim (powerless/trapped) — trapped in conflict sustained by aligned powers; aerial bombardment, blockade, famine as extraction mechanisms
 *   - Regional Islamic and Nationalist Movements: Secondary victim (moderate/constrained) — suppressed through counterinsurgency; constrained exit due to superior military capacity; gain recruits through visible extraction
 *   - Global Energy Markets: Powerful actor (powerful/mobile) — benefits from price stability but experiences extraction via petrodollar hegemony and production constraints; mobile exit options through energy transition
 *   - Cold War Alliance Institution: Piton remnant (institutional/arbitrage) — persists through military-industrial investment and strategic doctrine; sees own degradation but maintains through inertia
 *   - Regional Realignment Coalition: Organized challenger (organized/constrained) — Iran, Houthis, Chinese overtures, Saudi-Iran détente building alternative structures; constrained by military inferiority but gaining leverage through multipolar dynamics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gulf_arab_monarchies_us_alignment, 0.58).
domain_priors:suppression_score(gulf_arab_monarchies_us_alignment, 0.65).
domain_priors:theater_ratio(gulf_arab_monarchies_us_alignment, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gulf_arab_monarchies_us_alignment, extractiveness, 0.58).
narrative_ontology:constraint_metric(gulf_arab_monarchies_us_alignment, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(gulf_arab_monarchies_us_alignment, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gulf_arab_monarchies_us_alignment, tangled_rope).
narrative_ontology:human_readable(gulf_arab_monarchies_us_alignment, "Gulf Arab Monarchies US Strategic Alignment").
narrative_ontology:topic_domain(gulf_arab_monarchies_us_alignment, "geopolitical/military/economic").

domain_priors:requires_active_enforcement(gulf_arab_monarchies_us_alignment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gulf_arab_monarchies_us_alignment, us_military_strategic_reach).
narrative_ontology:constraint_beneficiary(gulf_arab_monarchies_us_alignment, gulf_monarchy_regime_security).
narrative_ontology:constraint_beneficiary(gulf_arab_monarchies_us_alignment, us_energy_markets).
narrative_ontology:constraint_beneficiary(gulf_arab_monarchies_us_alignment, gulf_defense_industrial_complex).
narrative_ontology:constraint_victim(gulf_arab_monarchies_us_alignment, regional_self_determination).
narrative_ontology:constraint_victim(gulf_arab_monarchies_us_alignment, palestinian_state_viability).
narrative_ontology:constraint_victim(gulf_arab_monarchies_us_alignment, yemen_civilian_population).
narrative_ontology:constraint_victim(gulf_arab_monarchies_us_alignment, gulf_democratic_governance).
narrative_ontology:constraint_victim(gulf_arab_monarchies_us_alignment, regional_islamic_movements).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PALESTINIAN STATELESS POPULATION (SNARE) — Trapped in fragmented territories with no state capacity. The US-Gulf alignment enables indefinite Israeli occupation with no mediation or pressure for Palestinian self-determination. No exit options; maximum extraction via perpetual dispossession. The alignment suppresses alternative regional voices that might advocate for Palestinian rights.
constraint_indexing:constraint_classification(gulf_arab_monarchies_us_alignment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: YEMEN CIVILIAN POPULATION (SNARE) — Trapped in an active conflict sustained by Gulf Arab states with US military support. No exit from bombing campaigns, blockade, or famine. The constraint's suppression mechanism is violent: aerial strikes prevent organizational alternatives. Maximum extraction of civilian suffering with minimal coordination benefit to the victims.
constraint_indexing:constraint_classification(gulf_arab_monarchies_us_alignment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: REGIONAL ISLAMIC/NATIONALIST MOVEMENTS (TANGLED ROPE) — Constrained by military superiority of aligned powers and suppressed through direct counterinsurgency. However, the alignment also creates coordination problems it must solve — Hamas, Hezbollah, and anti-alignment coalitions gain recruits through the visible extraction (occupation, bombing, regime support). The constraint both suppresses and generates organized opposition. Moderate power allows some exit (relocate, reorganize) but at high cost.
constraint_indexing:constraint_classification(gulf_arab_monarchies_us_alignment, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: US STRATEGIC COMMAND (ROPE) — Beneficiary with full arbitrage capacity. The alignment provides forward bases (Qatar, Bahrain), naval facilities (Fifth Fleet), and air operations centers. These coordinate regional deterrence, power projection into Asia, and energy market stabilization. The US experiences the constraint as a pure coordination mechanism: maintaining security architecture across the Indo-Pacific requires stable Gulf partnerships. Can exit by redesigning force posture, but has strong incentives to maintain current arrangement.
constraint_indexing:constraint_classification(gulf_arab_monarchies_us_alignment, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: GULF ARAB MONARCHY REGIMES (TANGLED ROPE) — Beneficiaries with constrained exit. Genuine coordination function: US military umbrella enables regime security against Iran and internal dissent; US diplomatic backing protects from international pressure on human rights and governance. But also extraction: dependent on US military support for regime survival, constrained by US strategic priorities (Israel policy, oil price regulation), and suppressed from pursuing independent regional policies. High theater ratio reflects performative aspects of democracy-building rhetoric without genuine institutional reform.
constraint_indexing:constraint_classification(gulf_arab_monarchies_us_alignment, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: REGIONAL REALIGNMENT COALITION (SCAFFOLD) — Organized actors (Iran, Houthis, emerging intra-Gulf tensions, Chinese strategic overtures) are building alternative alignment structures. The constraint has a structural sunset: as Chinese naval capacity grows, as Gulf states pursue independent foreign policies (Saudi-Iran normalization), and as new energy dynamics reduce oil dependency, the extraction mechanism loses force. The coalition sees the US-Gulf alignment as temporary, being superseded by multipolarity. Extraction is moderate because coalition members have exit paths (new partnerships, resource leverage).
constraint_indexing:constraint_classification(gulf_arab_monarchies_us_alignment, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 7: COLD WAR ALLIANCE ARCHITECTURE (PITON) — Institutional remnant of bipolar geopolitics. The constraint persists through inertia: military bases, defense contracts, and strategic doctrine inherited from 1970s-1980s when the alignment genuinely solved acute coordination problems (Soviet encroachment, regional balance). The theater ratio is high — much of the contemporary alignment is maintained through performative gestures (joint exercises, mutual declarations) rather than functional necessity. The architecture sees its own degradation: multiple actors now treat it as outdated, but it persists through institutional investment. Suppression is maintained by path dependence, not by active force.
constraint_indexing:constraint_classification(gulf_arab_monarchies_us_alignment, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: GLOBAL ENERGY MARKETS (TANGLED ROPE) — Powerful actor with mobile exit options (renewable energy transition, OPEC+ leverage, diversified supply chains). Experiences genuine coordination through oil price stability and petrodollar recycling via the alignment. Also experiences extraction via US dollar hegemony, Saudi production constraints (enforced by US alliance), and price volatility driven by geopolitical crises. High effective extractiveness because power asymmetries concentrate costs on less-diversified actors (developing nations, oil-dependent economies).
constraint_indexing:constraint_classification(gulf_arab_monarchies_us_alignment, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / STRUCTURAL REALISM VIEW (MOUNTAIN) — From a civilizational perspective, great power alliances with regional security partners are treated as immutable features of international anarchy. The constraint appears as a necessary response to anarchic conditions: smaller states must align with great powers; great powers must maintain extended deterrence. However, the structural data (beneficiaries with full agency, alternative alliances emerging, new technologies disrupting deterrence architecture) contradicts the mountain classification. The engine will identify this as a false summit — naturalization of a contingent post-Cold War arrangement as inherent to international structure.
constraint_indexing:constraint_classification(gulf_arab_monarchies_us_alignment, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gulf_arab_monarchies_us_alignment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(gulf_arab_monarchies_us_alignment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gulf_arab_monarchies_us_alignment, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(gulf_arab_monarchies_us_alignment, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(gulf_arab_monarchies_us_alignment, TR),
    TR >= 0.70.

:- end_tests(gulf_arab_monarchies_us_alignment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): The alignment exhibits strong extractive features that have intensified over time. The original function (Cold War deterrence against Soviet influence in the 1970s-1980s) was genuine coordination with mutual benefit. But as that threat faded post-1991, the extraction mechanism persisted and expanded — US military primacy became an end in itself rather than a means to deterrence; Gulf monarchy regimes became locked into military dependence; Palestinian statehood was indefinitely suppressed; regional independent foreign policy became impossible. The measurement trajectory (0.42 → 0.58 over 50 years) shows extractiveness accumulating rather than declining. Suppression (0.65): Moderate-high. The alignment suppresses through multiple mechanisms: (1) military superiority creating asymmetric deterrence (no viable alternative to US protection for Gulf monarchies); (2) direct suppression of independent regional movements (Iranian influence, Palestinian organizing, Houthi resistance); (3) institutional suppression of alternative narratives (democracy-building rhetoric covering regime support, human rights exemptions for 'strategic partners'); (4) structural suppression via petrodollar dependence. Theater ratio (0.58): Moderate-high and rising. The alignment's contemporary form relies heavily on performative elements: Joint exercises, defense technology partnerships, and 'strategic dialogues' substitute for genuine problem-solving. The underlying coordination function (energy market stabilization) has weakened due to US shale production reducing imports, yet the institutional apparatus persists. The rise from 0.35 to 0.58 reflects increasing performativity as the original rational for the arrangement declines.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between the US (Rope, immediate coordination) and trapped Palestinian/Yemeni populations (Snare, indefinite extraction) reflects the constraint's core structural feature: alignment benefits are concentrated among militarily powerful actors with exit capacity, while costs are externalized onto populations without agency or exit. The gap between the beneficiary's rope and the victim's snare is the entire reason the constraint exists — it converts a generalized regional coordination problem into asymmetric extraction by making US interests dominant and regional populations' interests irrelevant. The scaffold perspective (realignment coalition) reveals that this gap is eroding: as multipolar alternatives become available, the constraint's claim to be the only viable coordination solution weakens, which forces the beneficiaries to become more extractive (increase suppression, expand theater) to maintain compliance. The rising theater ratio (0.35 → 0.58) is a diagnostic signal of this gap widening — when victims see through the coordination fiction and beneficiaries can no longer persuade through rhetoric, explicit force must increase.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by structural position and exit capacity. US Strategic Command (institutional/arbitrage) occupies d ≈ 0.05 — full beneficiary with escape routes (can redesign force posture, can pursue alternative partnerships). Gulf regimes (institutional/constrained) occupy d ≈ 0.35 — beneficiaries who are also dependent (cannot exit without regime collapse risk). Palestinian/Yemeni populations (powerless/trapped) occupy d ≈ 0.95 — pure targets with no exit. Energy markets (powerful/mobile) occupy d ≈ 0.60 — mixed, with significant exit capacity through energy transition but currently locked into petrodollar dependence. Regional realignment coalition (organized/constrained) occupies d ≈ 0.65 — targets of suppression but with growing alternative exit (Chinese partnerships, Iranian coordination, Saudi-Iran détente). These d values feed directly into the sigmoid f(d) to produce effective extractiveness χ for each perspective. The gap between US chi (negative, indicating net benefit) and Palestinian chi (maximal, indicating net cost) is the perspectival gap.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by showing that Tangled Rope (hybrid coordination-extraction) is the engine's computed type from the analytical perspective, while Rope (pure coordination from the US view) and Snare (pure extraction from the trapped victims' view) are valid perspectival readings. No single type is the 'true' classification — the presheaf over all observations IS the classification. The mandatrophy is resolved by recognizing that the US genuinely solves a coordination problem (deterrence, energy stability) AND that this coordination mechanism is asymmetrically captured to extract from other regional actors. Both are structurally true. The false summit (analytical observer seeing mountain) would claim that great power alliances are inevitable features of anarchy — but the data shows that the alliance persists partly through contingent institutional investment, partly through rising theater (decreasing functionality), and partly through the absence of credible alternatives (which are now emerging via realignment coalition). The constraint is contingent, not natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    saudi_iran_reconciliation_durability,
    'Will the 2023 Saudi-Iran détente persist as a structural shift in Gulf alignment, or is it a tactical maneuver preserving US centrality?',
    'Five-year monitoring of Saudi independent military operations, Chinese naval facility expansion in Gulf, and US military base accessibility; correlation between détente progress and US force posture changes',
    'If détente durable: regional realignment perspective confirmed — constraint has real sunset and moderate extraction. If tactical: US-Gulf alignment constrained exit is confirmed — Gulf states cannot sustain independent policies without reverting to US dependence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(saudi_iran_reconciliation_durability, empirical, 'Whether Saudi-Iran détente represents structural shift or tactical maneuver').

omega_variable(
    renewable_energy_petrodollar_decoupling,
    'Will renewable energy transition and energy diversification reduce global financial dependency on petrodollar recycling, weakening the alignment''s economic extraction mechanism?',
    'Monitoring of global oil demand trajectory, adoption of alternative reserve currencies, renewable energy capacity scaling in developed economies, and correlation with US pressure on Gulf allies',
    'If decoupling significant: energy markets perspective shifts from tangled_rope toward rope (more coordination benefit, less extraction). If minimal: petrodollar mechanism persists as structural extraction floor regardless of energy transition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(renewable_energy_petrodollar_decoupling, empirical, 'Whether energy transition decouples petrodollar financial extraction').

omega_variable(
    chinese_naval_capacity_regional_balance,
    'At what point does Chinese naval capacity in the Indian Ocean equal or exceed US capability to sustain unilateral power projection in the Gulf?',
    'Naval force composition modeling, Chinese facility expansion (Djibouti, Gwadar, future Gulf presence), US budget constraints, and correlation with Gulf state defense diversification',
    'If near-term (5-10 years): scaffold perspective becomes operative — structural sunset begins now, and extraction mechanism weakens. If distant (20+ years): constraint persists under current form with periodic destabilization threats as China approaches parity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(chinese_naval_capacity_regional_balance, empirical, 'Timeline for Chinese naval parity in Indian Ocean/Gulf region').

omega_variable(
    internal_gulf_state_legitimacy_crisis,
    'Can Gulf monarchy regimes sustain domestic legitimacy without significant governance reform as oil revenues decline and youth unemployment rises?',
    'Monitoring of internal security spending, political reform announcements, labor force demographics, and correlation with regime stability; comparison across Gulf states',
    'If crisis severe: monarchy regime perspective shifts from stable beneficiary to desperate dependent — forces either genuine reform (weakening extraction mechanism) or escalating suppression (making snare classification more accurate for internal populations). If manageable: regimes maintain current alignment calculus.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internal_gulf_state_legitimacy_crisis, empirical, 'Whether internal legitimacy pressures force Gulf monarchies to alter US alignment').

omega_variable(
    us_strategic_pivot_credibility,
    'Is the stated US strategic pivot to Asia genuine, or does enduring energy and regional balance interests keep the US committed to Gulf primacy?',
    'Monitoring of US military budget allocation (Pacific vs Middle East), naval base consolidations, weapons sales patterns, and diplomatic focus; correlation with stated pivot rhetoric',
    'If genuine: US exit option becomes more credible — constraint extraction weakens as US interest declines. If rhetorical: US-Gulf alignment persists with modified narrative, and suppression mechanism (implicit threat of withdrawal) remains operative.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(us_strategic_pivot_credibility, preference, 'Authenticity of US strategic pivot to Asia').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gulf_arab_monarchies_us_alignment, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gulf_tr_t0, gulf_arab_monarchies_us_alignment, theater_ratio, 0, 0.35).
narrative_ontology:measurement(gulf_tr_t15, gulf_arab_monarchies_us_alignment, theater_ratio, 15, 0.48).
narrative_ontology:measurement(gulf_tr_t30, gulf_arab_monarchies_us_alignment, theater_ratio, 30, 0.58).
narrative_ontology:measurement(gulf_tr_t45, gulf_arab_monarchies_us_alignment, theater_ratio, 45, 0.62).
narrative_ontology:measurement(gulf_tr_t50, gulf_arab_monarchies_us_alignment, theater_ratio, 50, 0.58).

% Extraction over time
narrative_ontology:measurement(gulf_be_t0, gulf_arab_monarchies_us_alignment, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(gulf_be_t15, gulf_arab_monarchies_us_alignment, base_extractiveness, 15, 0.52).
narrative_ontology:measurement(gulf_be_t30, gulf_arab_monarchies_us_alignment, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(gulf_be_t45, gulf_arab_monarchies_us_alignment, base_extractiveness, 45, 0.6).
narrative_ontology:measurement(gulf_be_t50, gulf_arab_monarchies_us_alignment, base_extractiveness, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gulf_arab_monarchies_us_alignment, enforcement_mechanism).
narrative_ontology:affects_constraint(gulf_arab_monarchies_us_alignment, israel_palestine_occupation).
narrative_ontology:affects_constraint(gulf_arab_monarchies_us_alignment, yemen_saudi_war).
narrative_ontology:affects_constraint(gulf_arab_monarchies_us_alignment, petrodollar_hegemony).
narrative_ontology:affects_constraint(gulf_arab_monarchies_us_alignment, iran_regional_containment).
narrative_ontology:affects_constraint(gulf_arab_monarchies_us_alignment, gulf_monarchy_legitimacy).

% DUAL FORMULATION NOTE:
% This constraint is upstream of multiple regional conflicts (Yemen war, Palestinian occupation, Iran containment). Each downstream constraint has its own extractiveness value reflecting the specific empirical dynamics of that conflict, but all share the alignment as an enabling structural condition. The upstream constraint (gulf_arab_monarchies_us_alignment, ε=0.58) is decomposed from the downstream constraints to preserve ε-invariance — attempting to measure the alignment's extractiveness via any single downstream conflict (Yemen, Palestine, Iran) would produce different ε values, revealing that the underlying constraint is an architectural mechanism rather than a single empirical phenomenon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gulf_arab_monarchies_us_alignment, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
