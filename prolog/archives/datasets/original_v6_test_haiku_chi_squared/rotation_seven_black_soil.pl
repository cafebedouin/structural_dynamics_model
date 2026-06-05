% ============================================================================
% CONSTRAINT STORY: rotation_seven_black_soil
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
    narrative_ontology:affects_constraint/2,
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
 *   The R7 sector black soil toxin represents a structural extraction
 *   constraint in a closed microcosm (space station). A biological toxin of
 *   unknown origin and mechanism causes irreversible kidney failure in
 *   exposed humans. The constraint is not negotiable through policy or
 *   incentive — exposure leads to death — making it appear natural. However,
 *   the structural mechanism is institutional: crew are assigned to
 *   contaminated work without adequate protective equipment, decontamination
 *   protocols, or rotation schedules that would allow safety margins. The
 *   constraint extracts risk and health from powerless crew members (no exit
 *   option) and scientific value from the planetary biology mission
 *   (dependent on R7 samples). Station command retains options (containment,
 *   remediation, sector abandonment, supply return) and experiences the toxin
 *   as a temporary operational obstacle. The analytical observer risks
 *   treating biological toxicity as an immutable natural law; the structural
 *   data reveals it is a contingent arrangement of institutional neglect and
 *   resource prioritization.
 *
 * KEY AGENTS:
 *   - Exposed Crew Members: Primary victim (powerless/trapped) — assigned to R7 sector; no exit option; exposure causes irreversible harm
 *   - Planetary Biology Mission: Secondary victim (moderate/constrained) — depends on R7 samples; cannot exit without total scientific loss; forced into suboptimal choices
 *   - Station Command and Life Support: Primary beneficiary / institutional actor (institutional/arbitrage) — maintains sector access for operational/scientific goals; retains agency and remediation options
 *   - On-Station Medical and Decontamination Systems: Secondary institutional actor (institutional/constrained) — constrained by resource limits and protocol effectiveness; benefits from mission continuation but bears liability for crew exposure
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing an institutional failure as a law of biology
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rotation_seven_black_soil, 0.88).
domain_priors:suppression_score(rotation_seven_black_soil, 0.95).
domain_priors:theater_ratio(rotation_seven_black_soil, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rotation_seven_black_soil, extractiveness, 0.88).
narrative_ontology:constraint_metric(rotation_seven_black_soil, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(rotation_seven_black_soil, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rotation_seven_black_soil, snare).
narrative_ontology:human_readable(rotation_seven_black_soil, "R7 Black Soil Toxicity").
narrative_ontology:topic_domain(rotation_seven_black_soil, "biological/environmental").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rotation_seven_black_soil, station_structural_integrity_maintenance).
narrative_ontology:constraint_victim(rotation_seven_black_soil, exposed_crew_members).
narrative_ontology:constraint_victim(rotation_seven_black_soil, planetary_biology_mission).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXPOSED CREW MEMBER (SNARE) — Crew assigned to R7 sector have no meaningful exit option; exposure results in irreversible kidney failure and death. Zero degrees of freedom. d≈0.98, f(d)≈1.45, σ=0.8 → χ≈1.02. Pure extraction with maximum coercion.
constraint_indexing:constraint_classification(rotation_seven_black_soil, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: PLANETARY BIOLOGY MISSION (SNARE) — The mission depends on samples and data from R7; contamination would invalidate years of research. Mission cannot exit without total loss. Constrained by scientific investment and crew rotation dependencies. d≈0.92, f(d)≈1.38, σ=0.9 → χ≈1.10. High extraction; the constraint forces suboptimal choices (incomplete sampling, incomplete analysis, or accepting crew risk).
constraint_indexing:constraint_classification(rotation_seven_black_soil, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: STATION COMMAND / LIFE SUPPORT (SCAFFOLD) — Command has options: containment, decontamination protocols, sector abandonment, or return to Earth for resupply/repair. The toxin is a constraint on operational choices, not on existence itself. Command experiences the black soil as a temporary obstacle with known sunset — the underlying biological process can be studied, remediated, or avoided through engineering changes. d≈0.35, f(d)≈0.35, σ=1.0 → χ≈0.31. Low effective extraction; institutional actors retain agency.
constraint_indexing:constraint_classification(rotation_seven_black_soil, scaffold,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER / NATURAL TOXICITY VIEW (MOUNTAIN) — From a civilizational perspective, biological toxins in extraterrestrial soil are inherent to planetary contamination risk. No organism can 'exit' fundamental biochemistry. The toxin is a natural constraint on habitability, not a constructed one. However, the structural data (ε=0.88, suppression=0.95, no natural emergence) contradicts mountain classification. This is a false summit: the toxin itself is natural, but the constraint (crew assignment to contaminated sector without adequate protective equipment or decontamination) is institutional.
constraint_indexing:constraint_classification(rotation_seven_black_soil, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

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

:- end_tests(rotation_seven_black_soil_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.88): Very high. The constraint forces uncompensated risk acceptance (kidney failure, death) from crew in exchange for mission continuation and scientific data. The extraction is not negotiable — no payment, rotation, or risk premium can offset irreversible health damage. The upward trajectory from 0.72 to 0.88 over 6 intervals reflects that without intervention, the constraint becomes more severe as crew repeat exposures accumulate and remediation efforts (if any) fail. Suppression (0.95): Near-maximum. Crew have no meaningful alternative to sector assignment without career/mission consequences. No protective equipment is mentioned as reliable. No decontamination protocol is described. The only exit is mission abandonment or death. Theater ratio (0.15): Very low. The constraint is not performative — it kills. Crew awareness of toxicity is likely direct (medical reports, crew loss) rather than ritualized or symbolic. The low theater reflects that this is a structural reality, not an institutional performance.
 *
 * PERSPECTIVAL GAP:
 *   The exposed crew member sees only extraction and entrapment (Snare). The planetary biology mission sees high extraction with constrained exit (also Snare, but with slightly lower d due to mission agency). Station command sees a temporary operational obstacle with known remediation pathways (Scaffold) — the toxin limits but does not prevent action. The analytical observer risks seeing biological inevitability (Mountain) — toxins exist in nature — but the structural reality is institutional neglect of crew safety. The perspectival gap reveals that the constraint's severity depends entirely on institutional choices: if command prioritizes crew safety (protective equipment, rotation, quarantine), the constraint becomes manageable (Scaffold). If command accepts crew loss as a cost of mission continuation, the constraint remains a Snare.
 *
 * DIRECTIONALITY LOGIC:
 *   Exposed crew members: Victim + trapped → d≈0.98, f(d)≈1.45. Maximum extraction. Planetary biology mission: Victim + constrained → d≈0.92, f(d)≈1.38. High extraction; mission cannot exit without total loss. Station command: Beneficiary + arbitrage → d≈0.30, f(d)≈0.30. Low effective extraction; command retains agency and can choose remediation. Medical/decontamination systems: Institutional + constrained (caught between crew safety and mission demands) → d≈0.55, f(d)≈0.75. Mixed position; neither pure beneficiary nor pure victim. Analytical observer: analytical → d≈0.72, f(d)≈1.15. False summit risk: naturalizing institutional arrangement as biological law.
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE CLASSIFICATION CONFIRMED (ε=0.88, suppression=0.95, χ≥0.66). The constraint exhibits all snare signatures: high base extraction, high suppression (zero exits for crew), high effective extraction across relevant perspectives, no coordination benefit, and existence that depends on suppressing alternatives (abandoning the sector, accepting mission failure, or properly equipping/rotating crew). The false summit risk (analytical observer naturalizing toxicity) is mitigated by the low theater ratio (0.15) — this is not a performative constraint pretending to be natural; it is a structural reality with clear causal chains. Mandatrophy is resolved: the constraint is legitimately a Snare, not a misconstrued Rope or Scaffold. Crew extraction is non-negotiable because exit options are truly absent, not merely expensive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    toxin_production_mechanism,
    'Is the black soil toxin actively produced by a biological organism in the soil, or is it a degradation product of the original habitat material from the planet?',
    'Laboratory analysis of soil samples; culturing of microbial populations; toxin accumulation assays over time; geochemical analysis of toxin precursors',
    'If actively produced: the constraint is dynamic and may worsen over time (higher extraction trajectory). If degradation product: the constraint has finite duration (scaffold perspective strengthens). If geological: the constraint is immutable (mountain perspective).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(toxin_production_mechanism, empirical, 'Whether toxin is actively produced or residual').

omega_variable(
    protective_equipment_sufficiency,
    'Can existing or rapidly deployable protective equipment (isolation suits, air-recycling systems, enzymatic filters) prevent exposure with acceptable usability and reliability?',
    'Toxin degradation/isolation testing of current suit materials; field trials in contained R7 sector; failure mode analysis; crew acceptance and performance metrics with PPE',
    'If adequate protection exists: the constraint is operational (Scaffold) — remediation is a logistics problem. If protection is marginal or failure-prone: the constraint remains Snare — crew risk persists despite intervention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(protective_equipment_sufficiency, empirical, 'Whether protective equipment can prevent exposure reliably').

omega_variable(
    crew_rotation_economics,
    'What is the economic and logistical threshold (rotation schedule, medical screening, quarantine duration) that makes R7 sector assignment sustainable without accepting crew fatalities as inevitable?',
    'Cost-benefit analysis of: extended quarantine protocols, accelerated rotation cycles, specialized training, medical monitoring frequency; comparison to mission-critical objectives in R7',
    'If sustainable threshold exists below crew fatality rates: the constraint is a tradeoff problem (Tangled Rope). If no sustainable threshold: the constraint is non-negotiable extraction (Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(crew_rotation_economics, empirical, 'Sustainability threshold for crew rotation in contaminated sector').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rotation_seven_black_soil, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(r7bs_tr_t0, rotation_seven_black_soil, theater_ratio, 0, 0.1).
narrative_ontology:measurement(r7bs_tr_t3, rotation_seven_black_soil, theater_ratio, 3, 0.12).
narrative_ontology:measurement(r7bs_tr_t6, rotation_seven_black_soil, theater_ratio, 6, 0.15).

% Extraction over time
narrative_ontology:measurement(r7bs_be_t0, rotation_seven_black_soil, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(r7bs_be_t3, rotation_seven_black_soil, base_extractiveness, 3, 0.82).
narrative_ontology:measurement(r7bs_be_t6, rotation_seven_black_soil, base_extractiveness, 6, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rotation_seven_black_soil, enforcement_mechanism).
narrative_ontology:affects_constraint(rotation_seven_black_soil, space_station_crew_rotation_policy).
narrative_ontology:affects_constraint(rotation_seven_black_soil, planetary_sample_acquisition_safety).

% DUAL FORMULATION NOTE:
% R7 black soil toxicity is a distinct constraint from the underlying biological/geological causes of the toxin. The natural existence of the toxin is a property of the planet; the structural constraint is the institutional decision to expose crew to it without adequate protection or compensation. These could be decomposed into: (1) constraint_r7_toxin_existence (ε≈0.05, Mountain — inherent to planetary contamination), and (2) constraint_rotation_seven_black_soil (ε=0.88, Snare — institutional exposure). The current story focuses on the institutional constraint (the Snare) because that is where policy intervention is possible.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
