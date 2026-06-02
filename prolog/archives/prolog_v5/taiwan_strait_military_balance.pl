% ============================================================================
% CONSTRAINT STORY: taiwan_strait_military_balance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_taiwan_strait_military_balance, []).

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
 *   constraint_id: taiwan_strait_military_balance
 *   human_readable: Taiwan Strait Military Balance Constraint
 *   domain: geopolitical/military_security
 *
 * SUMMARY:
 *   The Taiwan Strait military balance represents a hybrid constraint
 *   combining genuine coordination problems (deterrence through capability
 *   demonstration, signaling intention without escalation) with significant
 *   asymmetric extraction (costs borne by Taiwan civilians and regional
 *   economies, benefits captured by military establishments and arms
 *   manufacturers). The constraint has persisted through strategic ambiguity
 *   maintained by all three primary parties — mainland China, Taiwan, and the
 *   United States — each benefiting from the current arrangement while
 *   bearing costs from the military escalation required to sustain it. Over
 *   the past two decades, the constraint has intensified: base extractiveness
 *   increased from ~0.35 to 0.58, theater_ratio rose from 0.55 to 0.65, and
 *   suppression increased as military capabilities advanced and stakes
 *   heightened. The constraint exhibits all six DR types from different
 *   structural positions, making it a diagnostic exemplar for
 *   inter-institutional geopolitical extraction. Economic interdependence
 *   (semiconductor supply chains, trade integration, ASEAN connectivity)
 *   creates scalar pressure toward deescalation, but political
 *   incompatibility and legitimacy claims by all parties prevent resolution.
 *   The structure is locked: Taiwan cannot exit (geographic and military
 *   vulnerability), PRC cannot exit (legitimacy depends on pursuing
 *   unification claims), US cannot fully exit (geopolitical interests require
 *   maintaining Indo-Pacific influence). This creates a tangled rope at the
 *   institutional level with snare characteristics for the civilian
 *   populations and piton characteristics for the international law
 *   frameworks.
 *
 * KEY AGENTS:
 *   - Taiwan Civilian Population: Primary victim (powerless/trapped) — bears security costs and existential risk with no exit options
 *   - Taiwan Government/Defense: Organized actor (organized/constrained) — manages deterrence coordination but extracts rents through military spending; cannot abandon commitment
 *   - People's Republic of China Military/State: Institutional actor (institutional/constrained) — maintains military capability while managing legitimacy constraints; cannot fully exit without sovereignty claim abandonment
 *   - United States Security Establishment: Institutional actor (institutional/arbitrage) — primary beneficiary; maintains geopolitical influence and arms sales revenue with option to reduce engagement
 *   - Defense Contractors and Arms Manufacturers: Powerful actors (powerful/mobile) — benefit from sustained competition; can reallocate if constraint resolves
 *   - Regional Economic Actors (ASEAN, Japan, Korea, multinational firms): Organized network (organized/constrained) — see economic interdependence as pressure toward deescalation but cannot unilaterally resolve political incompatibility
 *   - International Rules-Based Order Framework: Institutional narrative (institutional/arbitrage) — maintained performatively through legal and diplomatic theater with degraded functional role
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangements as immutable geographic facts
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(taiwan_strait_military_balance, 0.58).
domain_priors:suppression_score(taiwan_strait_military_balance, 0.72).
domain_priors:theater_ratio(taiwan_strait_military_balance, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(taiwan_strait_military_balance, extractiveness, 0.58).
narrative_ontology:constraint_metric(taiwan_strait_military_balance, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(taiwan_strait_military_balance, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(taiwan_strait_military_balance, tangled_rope).
narrative_ontology:human_readable(taiwan_strait_military_balance, "Taiwan Strait Military Balance Constraint").
narrative_ontology:topic_domain(taiwan_strait_military_balance, "geopolitical/military_security").

domain_priors:requires_active_enforcement(taiwan_strait_military_balance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(taiwan_strait_military_balance, people_republic_of_china_military).
narrative_ontology:constraint_beneficiary(taiwan_strait_military_balance, united_states_security_establishment).
narrative_ontology:constraint_beneficiary(taiwan_strait_military_balance, taiwan_defense_industry).
narrative_ontology:constraint_victim(taiwan_strait_military_balance, taiwan_civilian_population).
narrative_ontology:constraint_victim(taiwan_strait_military_balance, regional_economic_stability).
narrative_ontology:constraint_victim(taiwan_strait_military_balance, cross_strait_trade_interdependence).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TAIWAN CIVILIANS (SNARE) — Trapped by geographic proximity and lack of exit options. The military balance directly determines physical safety and existential risk. No meaningful choice to leave or negotiate individual safety. Experience maximum extraction in form of persistent security anxiety, economic uncertainty, and constrained development options. Cannot organize collective exit.
constraint_indexing:constraint_classification(taiwan_strait_military_balance, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: TAIWAN GOV/DEFENSE (TANGLED ROPE) — Organized agents with constrained exit. Face genuine coordination problem: must maintain credible deterrence through military modernization. But also benefit from arms sales relationships, defense industry development, and strategic partnership with US. Significant extraction through sustained military spending diverts resources from civilian needs, but extraction is not total — genuine security coordination function exists. Cannot abandon the military balance game without existential risk.
constraint_indexing:constraint_classification(taiwan_strait_military_balance, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: US SECURITY ESTABLISHMENT (ROPE) — Institutional actor with high arbitrage options. Benefits from Taiwan military balance through: geopolitical influence in Indo-Pacific, arms sales revenue, maintenance of strategic competition framework with China, and preservation of rules-based order narrative. Experiences constraint as coordination mechanism for maintaining regional influence. Can exit or reduce engagement with relatively low cost. Primary beneficiary from status quo maintenance.
constraint_indexing:constraint_classification(taiwan_strait_military_balance, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: PRC MILITARY/STATE (TANGLED ROPE) — Institutional actor with constrained exit options. Faces genuine coordination problem: maintaining military capability to enforce political claims requires sustained investment and strategic signaling. But also benefits from military-industrial complex development, geopolitical influence, and internal political consolidation through nationalist narrative. Cannot fully exit (abandoning claims would trigger internal legitimacy crisis). Extraction flows both ways: PRC bears costs of military buildup and regional tension, but extracts geopolitical concessions and domestic legitimacy from the constraint.
constraint_indexing:constraint_classification(taiwan_strait_military_balance, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: RULES-BASED ORDER FRAMEWORK (PITON) — The constraint is maintained through performative invocation of international law, UN conventions, and rules-based order narrative by multiple actors. The theater (formal statements, diplomatic protests, legal briefs) constitutes majority of observable activity. Actual functional role (preventing conflict through clarity) is much lower. Maintained through institutional inertia and because the alternative frameworks are not yet established. Theater ratio reflects that most military escalation is preceded by legal/diplomatic theater that performs constraint function but has limited predictive power.
constraint_indexing:constraint_classification(taiwan_strait_military_balance, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: DEFENSE CONTRACTORS (TANGLED ROPE) — Powerful actors with mobile options. Benefit significantly from sustained military competition (arms sales, technology development, strategic relationships). Also face genuine coordination problem: ensuring stable demand for military systems requires maintaining the threat perception without triggering actual war (which would destroy the sales environment). Can reallocate to other markets if Taiwan tensions resolve, but currently positioned to extract maximum rents from status quo ambiguity.
constraint_indexing:constraint_classification(taiwan_strait_military_balance, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ECONOMIC INTERDEPENDENCE (SCAFFOLD) — Organized actors (ASEAN, Japan, South Korea, multinational supply chains) see military balance as temporary problem with structural sunset. Deep economic interdependence, semiconductor supply chain integration, and trade networks create incentives for peaceful resolution. Theater_ratio declines as economic actors gain voice. Extraction is constrained by the coordination benefit of trade — actors have exit options through reorientation of supply chains and economic partnerships. Sunset clause: economic integration will eventually force political accommodation or lead to mutual economic disaster, creating pressure for deescalation.
constraint_indexing:constraint_classification(taiwan_strait_military_balance, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL / STRUCTURAL STABILITY (MOUNTAIN) — From civilizational perspective, military balance reflects fundamental geographic and demographic realities: Taiwan's location, mainland military superiority, US capability to project force, and incompatible political demands create an immutable constraint structure. No actor can fundamentally escape the underlying geography and power distribution. However, this natural law framing obscures the contingent institutional arrangements (strategic ambiguity, arms sales patterns, signaling conventions) that currently instantiate the constraint. The engine's false summit detector will identify this as naturalization.
constraint_indexing:constraint_classification(taiwan_strait_military_balance, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(taiwan_strait_military_balance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(taiwan_strait_military_balance, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(taiwan_strait_military_balance, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(taiwan_strait_military_balance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(taiwan_strait_military_balance, TR),
    TR >= 0.70.

:- end_tests(taiwan_strait_military_balance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.58): Elevated and increasing. The constraint's primary extractive mechanism is the forced military spending required to maintain deterrence credibility — Taiwan must spend 2-3% of GDP on defense, consuming resources that could fund civilian development. Mainland must sustain military modernization to make credible threat. But the constraint also provides genuine coordination benefits: clear military signaling prevents accidental escalation, capability demonstrations communicate resolve, strategic ambiguity allows all parties to maintain incompatible negotiating positions. The balance has shifted toward greater extractiveness over the interval as military capabilities advanced (PRC modernization accelerated, US commitment deepened through arms sales increases), making deterrence more expensive to maintain. The 0.35→0.58 trajectory reflects accumulating extraction as the military competition intensified. Suppression (0.72): High and structural. Taiwan civilians have no exit options (geographic, military, legal constraints). Taiwan government faces high costs of defection (existential risk). PRC faces internal legitimacy constraints preventing negotiated settlement. US faces geopolitical commitment lock-in. The suppression is a combination of structural barriers (military capabilities, geographic position) and institutional constraints (political commitments, domestic legitimacy requirements). Theater ratio (0.65): Moderate-high and increasing. Significant portion of constraint enforcement occurs through diplomatic theater (statements of resolve, legal briefs invoking international law, military exercises performed as signaling rather than preparation for conflict). Much activity is performative — intended to communicate intention without triggering unintended escalation. The theater has increased as military capabilities advanced, because actors must signal restraint even while demonstrating capability, creating need for choreographed displays. Theater serves coordination function (preventing miscalculation) but consumes resources and maintains ambiguity.
 *
 * PERSPECTIVAL GAP:
 *   The most diagnostic gap is between Taiwan civilians (snare) and US security establishment (rope). From US perspective: the constraint is coordination mechanism preventing war, maintaining rules-based order, enabling arms sales revenue. From Taiwan civilian perspective: the constraint is existential threat requiring permanent sacrifice of development resources and constant fear. This gap reveals the asymmetric extraction — US captures benefits with option to exit; Taiwan bears costs with no exit. The secondary gap is between institutional actors (Taiwan government and PRC military) who each see tangled rope with different balance of extraction/coordination. Taiwan government sees more extraction (military spending burden exceeds deterrence benefit for civilian population); PRC military sees more coordination (military modernization serves both deterrence and internal legitimacy simultaneously). The piton perspective on international law reveals that legal frameworks (UN conventions, rules-based order) are performatively invoked by all parties but have degraded predictive power for preventing escalation. The piton classification demonstrates how theater substitutes for functional constraint when underlying contradictions remain unresolved.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality computation reveals why US and Taiwan experience such different constraint types. US beneficiary status (arbitrage exit) produces low d ≈ 0.15, mapping to negative f(d) ≈ -0.01. Taiwan victim status (trapped exit) produces high d ≈ 0.95, mapping to positive f(d) ≈ 1.42. This structural asymmetry is encoded in the beneficiary/victim declarations: US captures security benefits (military influence, arms sales, geopolitical positioning); Taiwan bears security costs (defensive spending, existential risk, development constraints). The PRC military occupies a hybrid position: d ≈ 0.65 (mixed victim of military costs, beneficiary of geopolitical influence) maps to f(d) ≈ 1.00, producing moderate extracted χ. The defense contractors occupy d ≈ 0.40 (beneficiary from sustained competition, mobile exit options) producing f(d) ≈ 0.40. The regional economy network at d ≈ 0.50 (symmetric costs/benefits from trade integration) produces balanced f(d) ≈ 0.65. The directionality values are stable across time (they reflect structural position, not measurement noise), but the experienced extractiveness χ increases over the interval because base extractiveness ε grows as military capabilities advance and stakes increase.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates why mandatrophy resolution requires indexical analysis across multiple institutional positions. The temptation is to classify the Taiwan Strait constraint as pure SNARE (coercive, extractive, high suppression). But this misses the genuine coordination problems all parties face: PRC must demonstrate capability credibly without triggering US intervention; US must maintain deterrence without making unilateral commitment that constrains options; Taiwan must maintain defensive credibility without bankrupting civilian economy. The TANGLED ROPE classification (at institutional positions) captures this hybrid: genuine coordination function (deterrence signaling, conflict prevention) exists alongside asymmetric extraction (costs borne by civilians, rents captured by military establishments). The ROPE perspective (US security establishment) reveals that beneficiaries experience the constraint as pure coordination with negligible extraction — they are solving their geopolitical problem efficiently. The SNARE perspective (Taiwan civilians) reveals that victims experience pure extraction — the coordination benefits are invisible to them, the costs are absolute. The PITON perspective reveals institutional theater (legal frameworks, diplomatic statements) performing constraint function weakly. The SCAFFOLD perspective reveals that economic interdependence creates structural pressure for sunset — the constraint is maintained by political choices, not by immutable forces. The mountain perspective reveals the false summit risk: claiming geography and military capability gaps are natural laws that prevent resolution, when in fact they are contingent facts that constrain but do not determine outcomes. The mandatrophy is resolved by showing that ALL SIX types are valid perspectival readings: the classification depends entirely on where you stand in the structure. No universal type is 'correct' — the presheaf of perspectives across institutional positions IS the correct analysis.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    military_capability_threshold,
    'At what point does PRC military capability cross threshold where US deterrence becomes non-credible, transforming the constraint from maintained tension to resolved conflict?',
    'Analysis of US military capability relative to PRC projections; assessment of US domestic political willingness to sustain Taiwan defense commitment; wargaming scenarios across 5-year and 20-year horizons',
    'If threshold crossed: constraint transforms from tangled_rope to snare (Taiwan trapped without external guarantor). If threshold remains distant: constraint persists in current form.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(military_capability_threshold, empirical, 'Military capability threshold for deterrence credibility').

omega_variable(
    economic_interdependence_binding_strength,
    'Is semiconductor supply chain interdependence and broader trade integration sufficient to prevent military conflict despite political incompatibility, or is it merely delaying resolution?',
    'Historical analysis of cases where trade interdependence prevented or delayed conflict; simulation of supply chain disruption scenarios; assessment of whether economic actors can force political accommodation',
    'If binding: scaffold sunset mechanism is real and constraint will resolve toward peaceful coexistence. If insufficient: economic factors are subordinate to geopolitical competition and constraint will persist or escalate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(economic_interdependence_binding_strength, preference, 'Whether economic interdependence can prevent military conflict').

omega_variable(
    cross_strait_political_alignment_possibility,
    'Is political alignment between mainland and Taiwan governance sufficient to resolve the constraint, or are they permanently incompatible due to legitimacy and sovereignty claims?',
    'Analysis of political evolution on both sides; assessment of whether any governance arrangement (federation, autonomy, international status change) could satisfy both parties; tracking of public opinion shifts regarding political settlement',
    'If alignment possible: constraint can transform to rope (pure coordination around shared framework). If incompatible: constraint is locked into snare/tangled_rope indefinitely.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cross_strait_political_alignment_possibility, conceptual, 'Whether political alignment between mainland and Taiwan is possible').

omega_variable(
    us_commitment_contingency,
    'How much does US Taiwan security commitment depend on specific administrations, partisan alignment, or geopolitical US-China relationship, versus institutional lock-in through defense treaties and congressional authorization?',
    'Longitudinal analysis of US policy across administrations; assessment of legal/institutional barriers to US policy reversal; tracking of congressional vs executive branch constraints on Taiwan policy',
    'If administratively contingent: Taiwan constraint vulnerability increases; Taiwan must develop autonomous deterrence. If institutionally locked: US commitment is more stable and constraint persists in current form.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(us_commitment_contingency, empirical, 'Contingency of US Taiwan security commitment on administration vs institutional factors').

omega_variable(
    conflict_initiation_trigger_ambiguity,
    'What specific actions or thresholds would trigger transition from constrained tension to kinetic conflict, and how much ambiguity exists around those triggers?',
    'Analysis of red lines stated by all parties; assessment of how ambiguous or explicitly communicated trigger conditions are; examination of incidents that came close to transition and analysis of what prevented escalation',
    'If triggers are clear: actors can calibrate behavior to avoid transition. If triggers ambiguous: risk of accidental escalation through miscalculation increases; constraint becomes more fragile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conflict_initiation_trigger_ambiguity, empirical, 'Clarity of conflict initiation triggers and escalation threshold ambiguity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(taiwan_strait_military_balance, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(taiwan_tr_t0, taiwan_strait_military_balance, theater_ratio, 0, 0.55).
narrative_ontology:measurement(taiwan_tr_t10, taiwan_strait_military_balance, theater_ratio, 10, 0.62).
narrative_ontology:measurement(taiwan_tr_t20, taiwan_strait_military_balance, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(taiwan_be_t0, taiwan_strait_military_balance, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(taiwan_be_t10, taiwan_strait_military_balance, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(taiwan_be_t20, taiwan_strait_military_balance, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(taiwan_strait_military_balance, enforcement_mechanism).
narrative_ontology:affects_constraint(taiwan_strait_military_balance, semiconductor_supply_chain_concentration).
narrative_ontology:affects_constraint(taiwan_strait_military_balance, us_china_strategic_competition).
narrative_ontology:affects_constraint(taiwan_strait_military_balance, regional_economic_integration).

% DUAL FORMULATION NOTE:
% The Taiwan Strait military balance constraint operates at the intersection of three distinct structural problems: military deterrence (enforcement mechanism), economic integration (resource allocation mechanism), and political legitimacy (identity coordination mechanism). These could be decomposed into separate stories with different ε values, but they are tightly coupled through the institutional arrangements all parties maintain. The unified story captures the hybrid coordination-extraction structure at institutional level while the perspectives reveal how different actors experience the same constraint through their structural position.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(taiwan_strait_military_balance, institutional, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
