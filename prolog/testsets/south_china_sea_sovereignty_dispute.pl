% ============================================================================
% CONSTRAINT STORY: south_china_sea_sovereignty_dispute
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_south_china_sea_sovereignty_dispute, []).

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
 *   constraint_id: south_china_sea_sovereignty_dispute
 *   human_readable: South China Sea Sovereignty Dispute and Regional Constraint Structure
 *   domain: geopolitical/maritime/economic
 *
 * SUMMARY:
 *   The South China Sea sovereignty dispute represents a hybrid constraint
 *   structure combining territorial claim-staking, resource extraction
 *   monopoly, and great-power strategic competition. China's nine-dash-line
 *   claim overlaps with maritime EEZs of Vietnam, Philippines, Malaysia, and
 *   Brunei, creating contested jurisdiction over fishing grounds, oil/gas
 *   reserves, and critical shipping lanes. The constraint exhibits extraction
 *   mechanisms at multiple scales: China benefits from unilateral enforcement
 *   of maritime access; smaller claimant states are trapped by military
 *   asymmetry and sovereignty concerns; ASEAN mechanisms provide limited
 *   coordination function while embedding Chinese veto power; international
 *   law frameworks (UNCLOS, arbitration) offer performative legitimacy
 *   without enforcement; extra-regional powers (US, Japan, India) compete for
 *   strategic positioning. The theater ratio has increased as diplomatic
 *   mechanisms (Code of Conduct negotiations, maritime confidence-building)
 *   multiply without constraining enforcement reality — the gap between what
 *   negotiators agree to and what China implements widens. Base
 *   extractiveness has increased from 2009 (initial modernization of claim
 *   enforcement) to 2024 (routinized coast guard and fisher militia
 *   activities) as the constraint has matured from occasional incident to
 *   normalized extraction mechanism.
 *
 * KEY AGENTS:
 *   - China: Primary beneficiary (institutional/arbitrage) — captures resource access, geopolitical leverage, naval positioning advantage; experiences constraint as coordination of regional order
 *   - Vietnam, Philippines, Malaysia: Primary victims (powerless/trapped) — face territorial claim denial, disputed resource access, military coercion; cannot exit without sovereignty concession
 *   - ASEAN Non-Claimants (Indonesia, Thailand): Secondary actors (moderate/constrained) — benefit from stability mechanisms, constrained by Chinese leverage; participate in coordination negotiations while embedded in extraction dynamics
 *   - United States, Japan, India: Extra-regional powers (organized/constrained) — benefit from strategic positioning and containment partnerships, constrained by escalation risk; coordinating against Chinese hegemony while locked into great-power competition
 *   - International Maritime Law System: Institutional framework (institutional/arbitrage) — provides legitimacy to all claimants but enforces rulings only against non-great-powers; increasingly performative (piton)
 *   - ASEAN Diplomacy: Collective coordination mechanism (organized/constrained) — Code of Conduct negotiations, maritime dialogues; provide reduced-extraction pathways if enforcement mechanisms mature (scaffold perspective)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(south_china_sea_sovereignty_dispute, 0.58).
domain_priors:suppression_score(south_china_sea_sovereignty_dispute, 0.72).
domain_priors:theater_ratio(south_china_sea_sovereignty_dispute, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(south_china_sea_sovereignty_dispute, extractiveness, 0.58).
narrative_ontology:constraint_metric(south_china_sea_sovereignty_dispute, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(south_china_sea_sovereignty_dispute, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(south_china_sea_sovereignty_dispute, tangled_rope).
narrative_ontology:human_readable(south_china_sea_sovereignty_dispute, "South China Sea Sovereignty Dispute and Regional Constraint Structure").
narrative_ontology:topic_domain(south_china_sea_sovereignty_dispute, "geopolitical/maritime/economic").

domain_priors:requires_active_enforcement(south_china_sea_sovereignty_dispute).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(south_china_sea_sovereignty_dispute, china_regional_hegemony).
narrative_ontology:constraint_beneficiary(south_china_sea_sovereignty_dispute, resource_extraction_monopoly).
narrative_ontology:constraint_victim(south_china_sea_sovereignty_dispute, smaller_claimant_states).
narrative_ontology:constraint_victim(south_china_sea_sovereignty_dispute, international_maritime_commons).
narrative_ontology:constraint_victim(south_china_sea_sovereignty_dispute, sea_lane_navigation_freedom).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALLER CLAIMANT STATE (SNARE) — Vietnam, Philippines, Malaysia cannot credibly exit the dispute without surrendering territorial claims or accepting subordinate status. Military asymmetry creates structural entrapment despite legal claims. Maximum extraction experienced through contested access, resource denial, and forced accommodation of Chinese activities. No viable exit without existential cost to sovereignty narrative.
constraint_indexing:constraint_classification(south_china_sea_sovereignty_dispute, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: ASEAN COMMUNITY MEMBERS (TANGLED ROPE) — Indonesia, Thailand benefit from stability and resource access through ASEAN coordination mechanisms, but constrained by Chinese economic leverage and security coercion. The Code of Conduct negotiations provide genuine coordination function while embedding asymmetric extraction through dilution of enforcement mechanisms and Chinese veto power over implementation.
constraint_indexing:constraint_classification(south_china_sea_sovereignty_dispute, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CHINA (ROPE) — Experiences the constraint as coordination mechanism enabling regional order consolidation. Unilateral enforcement of nine-dash-line claim provides institutional benefits (resource access, geopolitical leverage, naval positioning). Benefits from first-mover advantage and military capacity. Net beneficiary with low experienced extraction — the constraint subsidizes this agent's strategic position.
constraint_indexing:constraint_classification(south_china_sea_sovereignty_dispute, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ASEAN COLLECTIVE DIPLOMACY (SCAFFOLD) — Code of Conduct negotiations, UNCLOS-based dispute resolution, and multilateral maritime dialogues represent temporary coordination mechanisms with implicit sunset clauses. These frameworks provide reduced-extraction pathways (arbitration, consultations, information sharing) that could mature into genuine rule-based order if China accepts limiting enforcement unilateralism. Sunset depends on whether great-power competition allows constraint relaxation.
constraint_indexing:constraint_classification(south_china_sea_sovereignty_dispute, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: UNCLOS/INTERNATIONAL LAW (PITON) — UNCLOS provides formal arbitration mechanisms (Philippines v. China tribunal 2016) and international legal standards for maritime claims. But enforcement is performative — the tribunal ruling is largely unenforced, and UNCLOS authority is theater without teeth. The legal framework persists through institutional inertia (provides legitimacy to all claimants) despite minimal enforcement capacity. Theater ratio high because UNCLOS activism is decoupled from actual constraint dynamics.
constraint_indexing:constraint_classification(south_china_sea_sovereignty_dispute, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: EXTRA-REGIONAL POWERS (TANGLED ROPE) — United States, Japan, India benefit from 'freedom of navigation' operations and strategic positioning in great-power competition, but constrained by risk of escalation and Chinese coercive responses. The dispute provides coordination function (basis for trilateral Quad, naval exercises, strategic partnerships) while extracting costs through military tensions, accident risks, and perpetual mobilization. These powers have agency and partial arbitrage options (can reduce involvement if costs exceed benefits) but remain structurally locked into regional great-power competition.
constraint_indexing:constraint_classification(south_china_sea_sovereignty_dispute, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / GEOPOLITICAL REALISM (MOUNTAIN) — From civilizational scope, some SCS tension is inherent to the structural geography: contested overlapping EEZs, critical global shipping lanes, energy resources, and power-balancing dynamics among great powers create permanent conflict potential independent of institutional will. The constraint appears natural and immutable — a feature of geography and relative power, not a contingent institutional arrangement. However, this risks naturalizing what is partially constructed through claim-staking, military doctrine, and institutional entrenchment.
constraint_indexing:constraint_classification(south_china_sea_sovereignty_dispute, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(south_china_sea_sovereignty_dispute_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(south_china_sea_sovereignty_dispute, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(south_china_sea_sovereignty_dispute, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(south_china_sea_sovereignty_dispute, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(south_china_sea_sovereignty_dispute, TR),
    TR >= 0.70.

:- end_tests(south_china_sea_sovereignty_dispute_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. China's unilateral enforcement of nine-dash-line claim extracts resource monopoly rents, geopolitical concessions from smaller states, and strategic positioning benefits. The extraction is not total (smaller states retain legal claims, limited unilateral activities, outside support) but substantive and growing over the measurement interval. Initial value (0.35 at T=0, 2004) reflects sporadic enforcement; mid-term value (0.48 at T=10, 2014) reflects increased modernization; current value (0.58 at T=20, 2024) reflects routinized coast guard/militia enforcement. Suppression (0.72): High. Multiple suppression mechanisms operate: military asymmetry prevents armed resistance; economic leverage (trade restrictions, tourism boycotts) sanctions unilateral actions by claimant states; diplomatic forum (ASEAN, UNCLOS) provides alternative pathways that are less coercive than military enforcement but still embedded in Chinese veto power; information asymmetry (Chinese activities in disputed areas are partly opaque). Theater ratio (0.68): High. UNCLOS arbitration (2016 ruling against nine-dash-line) is unenforced; Code of Conduct negotiations (ongoing since 2002) produce minimal binding commitments; maritime confidence-building measures (communication protocols) reduce accident risk but not strategic competition; official statements (both claimants and external powers) assert peaceful resolution while military activities intensify. The gap between diplomatic theater and enforcement reality has widened over the interval.
 *
 * PERSPECTIVAL GAP:
 *   China experiences Rope — legitimate regional coordination and order consolidation. Smaller claimants experience Snare — pure extraction with no viable exit. ASEAN non-claimants experience Tangled Rope — benefits from stability mechanisms but embedded in extraction. Extra-regional powers experience Tangled Rope — strategic coordination benefits but constrained by escalation risk. UNCLOS/COC framework experiences Piton — institutional maintenance of theater without functional enforcement. The analytical observer risks Mountain — seeing geopolitical constraint as natural/immutable feature of geography rather than constructed through claim-staking and institutional choices. The perspectival gap reveals that the constraint's classification depends critically on observer position; no single type captures the structure for all actors.
 *
 * DIRECTIONALITY LOGIC:
 *   China's d-value is low (~0.10-0.15) because it is the primary beneficiary with high exit optionality (could negotiate settlement, could reduce enforcement intensity). Smaller claimant d-values are high (~0.90-0.95) because they are trapped victims with no viable exit without sovereignty cost. ASEAN non-claimants have moderate d (~0.50-0.65) because they face constrained exit (regional economic integration limits unilateral disengagement but diplomatic coordination provides some agency). Extra-regional powers have moderate d (~0.50-0.60) because they are organized but strategically locked into competition (can modulate involvement but cannot fully exit without yielding to Chinese hegemony). These derivations feed the f(d) sigmoid, producing high effective extraction (χ) for trapped agents and low/negative χ for beneficiary institutional agents. The scope modifier at regional scale (σ=0.9) slightly dampens verification complexity compared to global constraints, but the geopolitical stakes ensure high behavioral extraction regardless of scope adjustment.
 *
 * MANDATROPHY ANALYSIS:
 *   The SCS constraint resolves mandatrophy by revealing the limit of single-perspective analysis. No single type ('this is just geopolitical realism' / 'this is purely institutional order') captures the structure. China's beneficiary institutional view (Rope) is not false — regional coordination genuinely occurs through their enforcement. Smaller claimants' victimized view (Snare) is not false — extraction is substantive and exit is trapped. The full structure requires all six perspectives: beneficiary coordination (Rope) + victim extraction (Snare) + moderate participation (Tangled Rope) + diplomatic scaffolding (Scaffold) + institutional theater (Piton) + analytical risk of naturalization (false Mountain). The mandatrophy is resolved by documenting that each perspective is structurally correct from its position — the constraint genuinely is coordination from one view, extraction from another, temporary solution from a third. This is not contradiction but presheaf structure over the observation site.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    nine_dash_line_legitimacy,
    'What is the true legal status of China''s nine-dash-line claim under UNCLOS: historical waters, continental shelf extension, or geopolitical assertion without legal basis?',
    'International tribunal precedent (Philippines arbitration 2016 concluded invalid); state party submissions to CLCS (Commission on the Limits of the Continental Shelf); comparative analysis of maritime claims by other states with similar geological/historical bases',
    'If legitimate (low probability): dispute is resource coordination problem (Rope-type from all perspectives). If invalid: dispute is hegemonic extraction (Snare-type from smaller claimant perspective), confirming tangled_rope classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(nine_dash_line_legitimacy, empirical, 'Legal validity of the nine-dash-line claim').

omega_variable(
    extraction_mechanism_clarity,
    'Is China''s enforcement mechanism primarily resource monopoly extraction, strategic coercion, or geopolitical signaling?',
    'Analysis of incident patterns: frequency of resource denial vs. military posturing; economic pressure on claimant states (trade restrictions, tourism boycotts); military activity correlation with diplomatic/economic cycles; cost-benefit analysis for China of maintaining unilateral enforcement vs. negotiated settlement',
    'If primarily monopoly: extractiveness ~ 0.40-0.50 (resource rent capture). If primarily coercion: extractiveness ~ 0.65-0.75 (security rent extraction). Mechanism type affects whether snare or tangled_rope is primary classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_mechanism_clarity, empirical, 'Dominant mechanism of extraction (resource monopoly vs. strategic coercion)').

omega_variable(
    code_of_conduct_implementation_threshold,
    'At what threshold of COC enforcement mechanisms would the constraint shift from tangled_rope to true rope (coordination without asymmetric extraction)?',
    'Comparison of COC draft text with final implementation; analysis of dispute resolution procedures (binding vs. consultative); assessment of Beijing''s veto power over enforcement; measurement of actual constraint on Chinese enforcement activities post-COC finalization',
    'If COC achieves binding arbitration and enforcement: constraint type could mature to rope-dominant (scaffold sunset realized). If COC remains consultative theater: constraint remains tangled_rope with high suppression. Determines whether smaller states have genuine exit pathway.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(code_of_conduct_implementation_threshold, empirical, 'Code of Conduct implementation sufficiency for constraint relaxation').

omega_variable(
    great_power_competition_lock_in,
    'Is the SCS constraint now locked into great-power competition (US-China strategic rivalry) in a way that makes ASEAN-only resolution mechanisms insufficient?',
    'Comparative analysis of dispute trajectory before/after 2017 (Trump doctrine change, US pivot return); correlation of SCS incident frequency with US-China trade/technology tensions; analysis of whether ASEAN states have actual negotiation space independent of great-power positioning',
    'If locked in: constraint is now structural to great-power competition, not amenable to regional diplomatic resolution (piton + snare from smaller state perspective). If not locked in: ASEAN-led mechanisms (scaffold) remain viable exit paths. Determines whether sunset clause is plausible or purely aspirational.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(great_power_competition_lock_in, empirical, 'Whether constraint is locked into great-power competition dynamics').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(south_china_sea_sovereignty_dispute, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scs_tr_t0, south_china_sea_sovereignty_dispute, theater_ratio, 0, 0.42).
narrative_ontology:measurement(scs_tr_t10, south_china_sea_sovereignty_dispute, theater_ratio, 10, 0.6).
narrative_ontology:measurement(scs_tr_t20, south_china_sea_sovereignty_dispute, theater_ratio, 20, 0.68).
narrative_ontology:measurement(scs_tr_t5, south_china_sea_sovereignty_dispute, theater_ratio, 5, 0.55).
narrative_ontology:measurement(scs_tr_t15, south_china_sea_sovereignty_dispute, theater_ratio, 15, 0.64).

% Extraction over time
narrative_ontology:measurement(scs_be_t0, south_china_sea_sovereignty_dispute, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(scs_be_t10, south_china_sea_sovereignty_dispute, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(scs_be_t20, south_china_sea_sovereignty_dispute, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(scs_be_t5, south_china_sea_sovereignty_dispute, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(scs_be_t15, south_china_sea_sovereignty_dispute, base_extractiveness, 15, 0.54).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(south_china_sea_sovereignty_dispute, enforcement_mechanism).
narrative_ontology:affects_constraint(south_china_sea_sovereignty_dispute, semiconductor_supply_competition).
narrative_ontology:affects_constraint(south_china_sea_sovereignty_dispute, taiwan_strait_militarization).
narrative_ontology:affects_constraint(south_china_sea_sovereignty_dispute, belt_and_road_constraint_infrastructure).
narrative_ontology:affects_constraint(south_china_sea_sovereignty_dispute, us_china_great_power_competition).

% DUAL FORMULATION NOTE:
% The SCS constraint sits at the intersection of maritime law (UNCLOS), resource extraction (fisheries/hydrocarbons), geopolitical positioning (great-power competition), and institutional coordination (ASEAN/COC). The constraint can be decomposed into distinct stories: resource extraction monopoly (ε~0.45), strategic coercion mechanism (ε~0.62), international law degradation (ε~0.35, piton-type), and great-power competition lock-in (ε~0.65). The unified story here treats the integrated constraint structure; decomposition would reveal how different observables (resource scarcity, military capability, legal claims, economic leverage) produce different ε values and classification paths.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(south_china_sea_sovereignty_dispute, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
