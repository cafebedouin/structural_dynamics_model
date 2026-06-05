% ============================================================================
% CONSTRAINT STORY: rotation_seven_black_soil
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rotation_seven_black_soil, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: rotation_seven_black_soil
 *   human_readable: R7 Black Soil Toxicity
 *   domain: biological/environmental
 *
 * SUMMARY:
 *   R7 Black Soil Toxicity represents a pure biological hazard that has
 *   become embedded in institutional extraction mechanisms. The toxin itself
 *   — a chemical compound that causes irreversible kidney failure in exposed
 *   humans — is biochemically immutable. However, the constraint it creates
 *   within the space station is not a natural law but a snare: a
 *   high-extraction mechanism maintained through suppression of information,
 *   inadequate decontamination protocols, and institutional pressure to
 *   maintain station operations despite the hazard. The extractiveness (0.92)
 *   and suppression (0.88) are extreme because exposed personnel face
 *   biological death with no exit option. The low theater_ratio (0.15)
 *   distinguishes this from a Piton: the hazard is real and the risk is
 *   genuine, not performatively maintained. The constraint's extractiveness
 *   has increased over time (0.85→0.92) as institutional adaptation has
 *   normalized the toxin's presence, shifting focus from evacuation or
 *   remediation to 'safe' habitation protocols that personnel increasingly
 *   recognize as inadequate. This is a case where the Snare classification
 *   prevents false naturalizing of a contingent institutional choice as an
 *   inevitable consequence of space station operations.
 *
 * KEY AGENTS:
 *   - Exposed Station Personnel: Primary victim (powerless/trapped) — face irreversible biological harm with no exit option; killed or permanently disabled by enforced proximity to toxin
 *   - Decontamination Teams: Secondary victim (moderate/constrained) — assigned to manage the toxin with high exposure risk and career pressure preventing refusal; constrained by occupational discipline
 *   - Station Management Authority: Primary beneficiary (institutional/mobile) — maintains operational fiction and continuous habitation by suppressing toxin information and constraining personnel alternatives; experiences coordination problem (how to seal the sector) alongside extraction benefit (avoiding costly evacuation or remediation)
 *   - Scientific Research Value: Secondary beneficiary (institutional/arbitrage) — research institutions benefit from study of the toxin's mechanism; can exit the constraint by transferring knowledge elsewhere
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees the biochemical mechanism as immutable but recognizes that the institutional extraction layer is contingent
 *   - Official Risk Assessment Protocol: Institutional actor (institutional/constrained) — safety procedures are largely performative, maintained through inertia rather than functional prevention
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rotation_seven_black_soil, 0.92).
domain_priors:suppression_score(rotation_seven_black_soil, 0.88).
domain_priors:theater_ratio(rotation_seven_black_soil, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rotation_seven_black_soil, extractiveness, 0.92).
narrative_ontology:constraint_metric(rotation_seven_black_soil, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(rotation_seven_black_soil, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rotation_seven_black_soil, snare).
narrative_ontology:human_readable(rotation_seven_black_soil, "R7 Black Soil Toxicity").
narrative_ontology:topic_domain(rotation_seven_black_soil, "biological/environmental").

domain_priors:requires_active_enforcement(rotation_seven_black_soil).
% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rotation_seven_black_soil, containment_protocols).
narrative_ontology:constraint_victim(rotation_seven_black_soil, exposed_station_personnel).
narrative_ontology:constraint_victim(rotation_seven_black_soil, decontamination_teams).
narrative_ontology:constraint_victim(rotation_seven_black_soil, long_term_station_habitation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXPOSED WORKER (SNARE) — Station personnel assigned to or accidentally exposed to R7 sector face irreversible kidney failure with no exit option. Trapped by station structure (cannot leave environment), by biological causality (exposure causes inevitable harm), and by institutional geography (sector is part of required operational infrastructure). Maximum extraction — death or permanent disability with zero escape.
constraint_indexing:constraint_classification(rotation_seven_black_soil, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: DECONTAMINATION TEAM (SNARE) — Tasked with managing the toxin, they face repeated exposure risk with constrained exit (cannot refuse assignment without severe career/disciplinary consequences). Suppression is extreme — the biological hazard itself enforces compliance. No meaningful alternatives exist for toxin management; the team bears extraction through occupational risk with minimal compensation or institutional recognition of danger.
constraint_indexing:constraint_classification(rotation_seven_black_soil, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: STATION MANAGEMENT AUTHORITY (TANGLED ROPE) — Must maintain operational continuity while managing toxic sector. Experiences the constraint as both coordination (sealing the sector, establishing protocols, training personnel) and extraction (suppression of information, pressure to maintain habitation in adjacent areas despite toxin proximity, budget constraints forcing understaffed decontamination teams). Has exit options (abandon sector, transfer operations) but chooses mobile constraints instead. Active enforcement required to maintain the fiction that the station remains fully operational.
constraint_indexing:constraint_classification(rotation_seven_black_soil, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: SCIENTIFIC RESEARCH VALUE (ROPE) — The toxin itself is a subject of study. Research institutions and medical teams benefit from detailed knowledge of the toxin's mechanism, enabling broader medical advances. This perspective experiences the constraint as pure coordination — the problem (managing and understanding a lethal biological agent) creates genuine research value and collaborative urgency. No extraction here; this is legitimate knowledge asymmetry resolved through institutional cooperation.
constraint_indexing:constraint_classification(rotation_seven_black_soil, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LIMIT VIEW (MOUNTAIN) — From a civilizational vantage, the toxin's lethality is a property of biochemistry: exposure causes irreversible kidney failure because the toxin binds to specific renal proteins and triggers cascading cellular failure. This mechanism is immutable — it is a natural law of that biological system's interaction with human physiology. No amount of social arrangement changes the toxin's chemical property. However, the structural data contradicts pure mountain classification: the constraint's institutional aspects (who gets exposed, suppression of information, inadequate decontamination protocols) are not biochemical laws but contingent management choices. The engine's false summit detector will flag this.
constraint_indexing:constraint_classification(rotation_seven_black_soil, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: OFFICIAL RISK ASSESSMENT PROTOCOL (PITON) — The formal safety procedures and risk matrices nominally govern the sector, but they are largely performative. The theater_ratio reflects the gap between documented protocols (extensive documentation, regular safety reviews, official hazard ratings) and actual prevention efficacy (exposures still occur, decontamination teams remain understaffed, information suppression persists). The protocol persists through institutional inertia — it is the official mechanism through which the station 'addresses' the toxin — but its functional prevention capacity has atrophied. It is maintained because abandoning it would require explicit acknowledgment of danger.
constraint_indexing:constraint_classification(rotation_seven_black_soil, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rotation_seven_black_soil_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rotation_seven_black_soil, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rotation_seven_black_soil, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(rotation_seven_black_soil, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(rotation_seven_black_soil, TR),
    TR >= 0.70.

:- end_tests(rotation_seven_black_soil_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.92): Extreme. This is near-maximum extraction because the targeted victims (exposed personnel) face biological death or permanent disability. The extraction mechanism is non-negotiable biochemistry: exposure→kidney failure→death. No gradated outcome exists; the constraint is binary and lethal. Suppression (0.88): Extreme. Multiple layers prevent escape: the toxin's physical location is sealed but adjacent to operational areas; personnel cannot leave the station without institutional approval; decontamination protocols are inadequate (theater_ratio 0.15 indicates low functional prevention); information about exposures is suppressed; career pressure prevents refusal. Theater ratio (0.15): Low. Unlike a Piton, the R7 hazard is real and the risk is genuine. The low theater reflects that decontamination procedures are straightforward (high functional content) even if inadequate. The theater_ratio is not zero because some procedural elements (daily safety briefings, official risk assessments) serve primarily to document institutional acknowledgment rather than prevent harm. The increasing theater over time (0.05→0.15) reflects institutional adaptation: as the toxin becomes 'normal,' more ritual accumulates around management, suggesting piton-level degradation of the safety system itself over longer timescales.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits a full perspectival gap between victim and beneficiary views. Exposed personnel see pure lethal extraction (Snare) with no coordination benefit — the toxin serves no purpose for them. Decontamination teams see mixed extraction and coordination (Tangled Rope) — they benefit from the research value and the problem-solving urgency, but at severe personal cost. Station Management sees coordination (how to seal the sector, how to maintain operations) alongside extraction benefit (avoiding expensive remediation). The analytical observer risks naturalizing the hazard as an immutable consequence of biochemistry (Mountain perspective) but the structural data reveals that the institutional choices — suppression of information, understaffing of decontamination, pressure to maintain adjacent habitation — are contingent human decisions, not laws of nature. The scientific research perspective sees pure coordination (understanding the toxin's mechanism). The piton perspective (official safety protocols) sees its own degradation: procedures exist but are increasingly theatrical, maintained through institutional inertia rather than functional prevention.
 *
 * DIRECTIONALITY LOGIC:
 *   Each victim's directionality (d) is determined by their relationship to the extraction flow and exit options. Exposed personnel have d≈0.95 (trapped, full target, powerless) — they bear maximum extraction with zero agency. Decontamination teams have d≈0.85 (constrained, mostly target, moderate) — they experience high extraction but retain some professional agency. Station Management has d≈0.10 (mobile, beneficiary, institutional) — the constraint generates extraction benefits they can arbitrage (maintain operations, avoid evacuation costs) while retaining exit options. The scientific research value has d≈0.05 (arbitrage, beneficiary, institutional) — genuine coordination benefit with full exit capability. The engine derives these d values from the beneficiary/victim declarations and exit options, then applies the sigmoid f(d) to produce effective extractiveness chi at each perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   [RESOLVED MANDATROPHY] Reviewed 2026-03-01. Override: false_natural_law.
 *   RESOLVED: The Snare classification is mandatory and unambiguous. This constraint cannot be misclassified as Rope (coordination) or Scaffold (temporary solution) because the biological mechanism is genuinely lethal and the extraction is genuinely maximum. The mandatrophy is resolved by showing that only two perspectives (Scientific Research as Rope, Station Management's coordination layer as Tangled Rope) produce non-Snare classifications, and both are secondary to the primary victim experience. The false summit (Mountain) is explicitly rejected by the structural data: the biochemistry is immutable, but the institutional extraction layer (suppression, inadequate protocols, pressure to remain) is contingent and human-caused. The piton element (degrading safety protocols over time) does not elevate the constraint to a pure Piton because the core hazard remains real and lethal, not merely theatrical. This is a pure Snare with institutional layering, not a degraded form of coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    toxin_origin_and_containment,
    'Did the toxin originate from the station''s experimental biology program, or is it a contaminant from external sources? What containment measures actually prevent secondary spread?',
    'Full forensic analysis of toxin source; containment integrity testing; environmental sampling of adjacent sectors',
    'If engineered: classification may shift toward Snare-with-institutional-culpability. If external: maintains Snare-as-accident framing. If containment is failing: extractiveness increases (0.92 → 0.97+) due to hidden risk.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(toxin_origin_and_containment, empirical, 'Origin and actual containment status of R7 toxin').

omega_variable(
    information_suppression_mechanism,
    'How systematically is toxin exposure information suppressed from station personnel and external oversight? Is suppression enforced through official channels or through institutional culture?',
    'Analysis of incident reporting vs actual exposures; interviews with personnel under conditions preventing retaliation; review of information quarantine directives',
    'If suppression is systematic and official: mandates explicit institutional extraction classification (Snare with enforced concealment). If cultural and informal: suggests Piton (degraded accountability rather than malicious extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(information_suppression_mechanism, conceptual, 'Degree and mechanism of toxin information suppression').

omega_variable(
    exit_option_realism,
    'Can station personnel genuinely exit the station, or is departure blocked by orbital mechanics, quarantine protocols, or institutional policy?',
    'Review of transfer request approvals; analysis of quarantine protocols'' actual application; cost/timeline analysis for emergency evacuation',
    'If exit is truly impossible: trapped→powerless classification confirmed, extractiveness remains at 0.92. If exit is theoretically possible but practically blocked: reveals institutional rather than natural constraint; may warrant Tangled Rope reclassification at personnel perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_option_realism, empirical, 'Actual feasibility of personnel exit from the station').

omega_variable(
    decontamination_protocol_effectiveness,
    'Do decontamination protocols actually reduce toxin exposure risk, or are they theatrical procedures that create a false sense of safety while exposure persists?',
    'Comparison of exposure rates before and after protocol implementation; biomarker analysis of supposedly decontaminated personnel; protocol design review for biochemical plausibility',
    'If protocols are effective: theater_ratio should decrease (0.15 → 0.05-0.10), suggesting Snare without the piton element. If protocols are theatrical: theater_ratio remains high (0.70+), supporting Piton classification for the safety system itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decontamination_protocol_effectiveness, empirical, 'Actual effectiveness vs theatrical appearance of decontamination protocols').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rotation_seven_black_soil, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(r7bs_tr_t0, rotation_seven_black_soil, theater_ratio, 0, 0.05).
narrative_ontology:measurement(r7bs_tr_t5, rotation_seven_black_soil, theater_ratio, 5, 0.1).
narrative_ontology:measurement(r7bs_tr_t10, rotation_seven_black_soil, theater_ratio, 10, 0.15).

% Extraction over time
narrative_ontology:measurement(r7bs_be_t0, rotation_seven_black_soil, base_extractiveness, 0, 0.85).
narrative_ontology:measurement(r7bs_be_t5, rotation_seven_black_soil, base_extractiveness, 5, 0.89).
narrative_ontology:measurement(r7bs_be_t10, rotation_seven_black_soil, base_extractiveness, 10, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rotation_seven_black_soil, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% R7 Black Soil Toxicity is a standalone constraint. Future decomposition may separate the biochemical immutability (Mountain) from the institutional extraction mechanisms (Snare), but currently the human-focused analysis treats them as unified.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
