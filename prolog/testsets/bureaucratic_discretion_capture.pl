% ============================================================================
% CONSTRAINT STORY: bureaucratic_discretion_capture
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bureaucratic_discretion_capture, []).

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
 *   constraint_id: bureaucratic_discretion_capture
 *   human_readable: Bureaucratic Discretion Capture
 *   domain: political_economy/regulatory_capture
 *
 * SUMMARY:
 *   Bureaucratic discretion capture occurs when regulatory agencies develop
 *   stable, informal relationships with regulated industries that shape how
 *   enforcement discretion is applied. Unlike statutory capture (where
 *   legislation directly favors industry), discretion capture operates
 *   through the gap between written rules and enforcement practice. The
 *   constraint exhibits genuine coordination function — complex markets
 *   require expert judgment, adaptive rule application, and real-time market
 *   knowledge that only industry actors possess — but this coordination
 *   function becomes a vehicle for asymmetric extraction. The regulator
 *   benefits from industry information and career opportunities; the industry
 *   benefits from selective enforcement and barrier creation against
 *   competitors; the unrepresented public and competing firms bear the cost.
 *   The constraint is tangled because both elements are structurally real:
 *   the coordination function cannot be eliminated without destroying
 *   adaptive regulatory capacity, yet the extraction mechanism cannot be
 *   separated from the coordination function without eliminating the
 *   information dependence that creates the capture vulnerability in the
 *   first place.
 *
 * KEY AGENTS:
 *   - Regulated Industry (Large Incumbents): Primary beneficiary (institutional/arbitrage) — captures favorable discretionary enforcement, barrier creation against competitors, predictability advantage
 *   - Captured Regulators: Secondary beneficiary (institutional/arbitrage) — gains career advancement, industry consulting opportunities, post-government employment options, reduction in enforcement burden through cooperation
 *   - Unrepresented Public Interest: Primary victim (powerless/trapped) — bears cost of lax enforcement, weaker environmental or consumer protections, unable to organize or voice preferences
 *   - Competing Firms Outside Capture Circle: Secondary victim (moderate/constrained) — face higher enforcement burden than incumbents, barriers to entry from selective discretion, can exit market but at high cost
 *   - Reform-Oriented Oversight Bodies: Organized constraint on the constraint (organized/constrained) — Congressional committees, inspectors general, anti-corruption agencies see the mixed structure but face tradeoff between constraining capture and preserving adaptive capacity
 *   - Administrative Procedures Apparatus: Institutional actor maintaining theater (institutional/arbitrage) — notice-and-comment rulemaking, impact assessments, cost-benefit analysis continue as performative compliance while enforcement gap persists
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bureaucratic_discretion_capture, 0.58).
domain_priors:suppression_score(bureaucratic_discretion_capture, 0.62).
domain_priors:theater_ratio(bureaucratic_discretion_capture, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bureaucratic_discretion_capture, extractiveness, 0.58).
narrative_ontology:constraint_metric(bureaucratic_discretion_capture, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(bureaucratic_discretion_capture, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bureaucratic_discretion_capture, tangled_rope).
narrative_ontology:human_readable(bureaucratic_discretion_capture, "Bureaucratic Discretion Capture").
narrative_ontology:topic_domain(bureaucratic_discretion_capture, "political_economy/regulatory_capture").

domain_priors:requires_active_enforcement(bureaucratic_discretion_capture).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bureaucratic_discretion_capture, 'edc039a4-2d6e-40c5-963e-0380a525129c').
narrative_ontology:cs_kernel_codification('edc039a4-2d6e-40c5-963e-0380a525129c', formalized).
narrative_ontology:cs_authority_grounding('edc039a4-2d6e-40c5-963e-0380a525129c', extraction).
narrative_ontology:cs_interpretation_layer_present('edc039a4-2d6e-40c5-963e-0380a525129c').
narrative_ontology:cs_reference_frame('edc039a4-2d6e-40c5-963e-0380a525129c', rule_of_law_administrative_impartiality).
narrative_ontology:cs_drift_state('edc039a4-2d6e-40c5-963e-0380a525129c', contemporary_regulatory_state, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('edc039a4-2d6e-40c5-963e-0380a525129c', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bureaucratic_discretion_capture, regulated_industry).
narrative_ontology:constraint_beneficiary(bureaucratic_discretion_capture, captured_regulators).
narrative_ontology:constraint_victim(bureaucratic_discretion_capture, competing_firms).
narrative_ontology:constraint_victim(bureaucratic_discretion_capture, public_interest).
narrative_ontology:constraint_victim(bureaucratic_discretion_capture, unrepresented_consumers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNREPRESENTED PUBLIC INTEREST (SNARE) — Cannot exit or organize. Bears the cost of lax enforcement and selective rule application. No formal voice in rulemaking or discretion decisions. Extraction is maximal and suppression is complete.
constraint_indexing:constraint_classification(bureaucratic_discretion_capture, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: COMPETING FIRMS (SNARE) — Face higher enforcement burden than captured incumbents. Barriers to entry created by selective discretion application. Can exit market but at high cost (relocation, market abandonment). Trapped within the regulatory jurisdiction.
constraint_indexing:constraint_classification(bureaucratic_discretion_capture, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CAPTURED REGULATOR (ROPE) — Experiences the constraint as coordination. Complex markets require judgment calls and adaptive enforcement. Collaboration with industry provides technical expertise, market knowledge, and real-time information. Net beneficiary through career advancement, industry consulting, and post-government employment options. Discretion alignment with industry preferences is experienced as solving technical problems, not extraction.
constraint_indexing:constraint_classification(bureaucratic_discretion_capture, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REGULATED INDUSTRY (ROPE) — Achieves coordination: clear expectations, predictable enforcement, adaptive rules that reflect market realities. Benefits enormously from the stable relationship (lower compliance burden, selective enforcement, barrier creation against competitors). Experiences the constraint as essential cooperation between technical specialists.
constraint_indexing:constraint_classification(bureaucratic_discretion_capture, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: REFORM-ORIENTED OVERSIGHT (TANGLED ROPE) — Congressional committees, inspector generals, anti-corruption agencies see both genuine coordination function (complex rules need judgment) and asymmetric extraction (selective enforcement creates unfair advantage). Can constrain but cannot eliminate discretion capture without destroying adaptive regulatory capacity. Experiences the constraint as genuinely mixed.
constraint_indexing:constraint_classification(bureaucratic_discretion_capture, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ADMINISTRATIVE PROCEDURES APPARATUS (PITON) — Notice-and-comment rulemaking, impact assessments, cost-benefit analysis, administrative law review are meant to constrain discretion. But the apparatus has become largely performative — regulatory agencies conduct required procedures while maintaining capture relationships in the enforcement gap. Theater_ratio is high: formal procedures exist but discretionary application undermines them. The apparatus persists through institutional inertia and legal habit, not because it effectively prevents capture.
constraint_indexing:constraint_classification(bureaucratic_discretion_capture, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bureaucratic_discretion_capture_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(bureaucratic_discretion_capture, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bureaucratic_discretion_capture, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(bureaucratic_discretion_capture, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(bureaucratic_discretion_capture, TR),
    TR >= 0.70.

:- end_tests(bureaucratic_discretion_capture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, increasing over interval. Base measurement reflects that regulatory discretion creates real advantages for captured incumbents — enforcement is selective, rules are applied adaptively to favor established relationships, and barriers are created against new entrants. The increase from 0.35 to 0.58 over the measurement interval reflects the institutional accumulation of capture: as relationships mature and informal understanding deepens, explicit negotiation decreases but implicit alignment increases. The trajectory suggests that capture dynamics intensify once stable relationships form. Suppression (0.62): Moderate-high and stable. Barriers to exit and alternative enforcement include political costs of whistleblowing, career consequences for regulators who challenge industry relationships, absence of alternative regulatory pathways, and public confusion about whether discretion is legitimate or captured. Theater ratio (0.65): Moderate-high and increasing. Administrative procedures (notice-and-comment, impact assessments, inter-agency review) are conducted as required by law but the actual enforcement decisions reflect prior industry alignment. The gap between formal process and substantive outcome widens as capture deepens, producing higher theater. At t=0, capture is less institutionalized, so theater is lower.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates a structured perspectival gap between institutional actors with different structural positions. The regulated industry and captured regulators both see Rope — genuine coordination that solves technical problems and creates value. Their directionality is low (they are net beneficiaries); their experienced extractiveness is near zero or negative. The competing firms and public interest see Snare — selective enforcement that extracts from them without consent or benefit. Their directionality is high (they are targets); their experienced extractiveness is maximal. Oversight bodies see Tangled Rope — they perceive both the genuine coordination function and the asymmetric extraction, creating the perspectival bind. This gap is not observational disagreement; it is structural difference in position. The engine's directionality computation will derive this: beneficiary + arbitrage → low d → low chi (Rope); victim + trapped → high d → high chi (Snare); mixed + constrained → medium d → medium chi (Tangled Rope).
 *
 * DIRECTIONALITY LOGIC:
 *   Regulated incumbents (institutional/arbitrage) derive d ≈ 0.10-0.20: they are primary beneficiaries with multiple exit options (can relocate, shift industries, engage in regulatory arbitrage). Their experienced extraction chi is negative or near-zero; they experience the constraint as coordination benefit. Captured regulators (institutional/arbitrage) derive d ≈ 0.15-0.25: they are secondary beneficiaries (career advancement, post-government consulting) with high exit options (move to private industry, other agencies, academia). Their experienced chi is low; they experience coordination with industry as solving genuine technical problems. Unrepresented public (powerless/trapped) derives d ≈ 0.95: victim status with zero exit options within the regulatory system. Their experienced chi is very high (f(d) ≈ 1.42 × ε × σ(national) ≈ 1.42 × 0.58 × 1.0 ≈ 0.82). They experience maximum extraction. Competing firms (moderate/constrained) derive d ≈ 0.70: victim status with constrained exit (can leave market but at high cost). Their experienced chi is moderate-high. Oversight bodies (organized/constrained) derive d ≈ 0.55: mixed beneficiary/victim status (they benefit from agency effectiveness but bear cost of capture undermining mandate). Their experienced chi is moderate, matching the Tangled Rope classification. The directionality spread is large (0.10 to 0.95) — this is a high-inequality constraint where extraction concentrates on the powerless while beneficiaries have maximum exit freedom.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by the genuine presence of both coordination and extraction in the constraint structure. Tangled Rope is the correct classification because: (1) Coordination function: complex regulatory domains genuinely require adaptive judgment and industry technical knowledge. Eliminating discretion would either destroy regulatory responsiveness or require massive regulator investment in duplicate internal expertise. (2) Extraction asymmetry: the coordination function becomes a vehicle for selective enforcement that systematically advantages incumbents and creates barriers to competitors. The two mechanisms are not separable — the information dependence that enables coordination is the same dependence that enables capture. (3) Active enforcement: the constraint requires continuous relational work to maintain alignment. Formal rules exist, but enforcement practices are shaped by informal understanding. This requires active maintenance — if enforcement enforcement relationships were severed, formal rules would reemerge as the binding constraint. The mandatrophy dissolves the temptation to classify this as pure Rope (genuine coordination) by documenting the asymmetric extraction and the structural inequality. It also prevents misclassification as Snare by documenting the real coordination function and the necessity of discretion in complex markets. Tangled Rope is the only classification that holds both truths.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_extraction_boundary,
    'Where is the boundary between legitimate adaptive rule application (coordination) and selective enforcement favoring specific industry actors (extraction)?',
    'Comparative analysis of enforcement patterns: case selection bias detection, penalty variance across firms, temporal correlation between industry advocacy and enforcement decisions, post-government employment tracking for regulators',
    'If boundary is clear: constraint reclassifies toward Snare (extraction dominates). If boundary is fuzzy: Tangled Rope holds. If extraction mechanisms are structurally necessary for coordination: constraint reclassifies toward Rope (coordination legitimate).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary, empirical, 'Boundary between legitimate adaptive enforcement and selective extraction').

omega_variable(
    information_asymmetry_necessity,
    'Is the regulator''s dependence on industry for technical information structurally necessary, or has it been artificially cultivated to create capture vulnerability?',
    'Capability assessment: does regulator have in-house technical capacity? Comparative analysis with other jurisdictions: do agencies with higher internal capacity show different capture patterns? Personnel tracking: are positions deliberately deprioritized to create knowledge dependence?',
    'If necessary: information asymmetry is genuine coordination cost (Rope floor valid). If cultivated: the constraint''s extraction mechanism is structurally maintained (Snare or Tangled Rope with high extraction coefficient).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(information_asymmetry_necessity, empirical, 'Whether regulator dependence on industry information is necessary or cultivated').

omega_variable(
    revolving_door_causal_direction,
    'Does the revolving door (regulators moving to industry positions) cause capture, or does pre-existing capture cause the revolving door as a benefit to captured regulators?',
    'Temporal and directional analysis: do regulators adopt pro-industry positions before or after industry job offers? Do agencies with no revolving-door history show different capture patterns? Do regulators from captured agencies have different post-government employment trajectories than those from non-captured agencies?',
    'If revolving door causes capture: cutting the door weakens capture. If pre-existing capture causes the door: the door is a symptom, not a mechanism. True causal direction changes intervention priorities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revolving_door_causal_direction, empirical, 'Causal direction of revolving-door influence on capture').

omega_variable(
    democracy_constraint_interaction,
    'Is bureaucratic discretion capture a distinct constraint, or is it a manifestation of a deeper democratic accountability gap where agencies operate with minimal legislative or electoral oversight?',
    'Structural comparison: agencies with strong legislative oversight (frequent reauthorization, detailed statutory guidance, active committee oversight) vs weak oversight. Does capture intensity vary inversely with oversight intensity?',
    'If distinct: constraint can be addressed through transparency, internal controls, post-employment restrictions. If manifestation: the root constraint is democratic accountability, and discretion capture is one channel through which that constraint operates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democracy_constraint_interaction, conceptual, 'Whether discretion capture is a standalone constraint or manifestation of democratic accountability gap').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bureaucratic_discretion_capture, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bdc_tr_t0, bureaucratic_discretion_capture, theater_ratio, 0, 0.45).
narrative_ontology:measurement(bdc_tr_t10, bureaucratic_discretion_capture, theater_ratio, 10, 0.58).
narrative_ontology:measurement(bdc_tr_t20, bureaucratic_discretion_capture, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(bdc_be_t0, bureaucratic_discretion_capture, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(bdc_be_t10, bureaucratic_discretion_capture, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(bdc_be_t20, bureaucratic_discretion_capture, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(bdc_su_t0, bureaucratic_discretion_capture, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(bdc_su_t10, bureaucratic_discretion_capture, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(bdc_su_t20, bureaucratic_discretion_capture, suppression_requirement, 20, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bureaucratic_discretion_capture, resource_allocation).
narrative_ontology:affects_constraint(bureaucratic_discretion_capture, regulatory_forbearance).
narrative_ontology:affects_constraint(bureaucratic_discretion_capture, industry_technical_standard_setting).
narrative_ontology:affects_constraint(bureaucratic_discretion_capture, revolving_door_post_government_employment).

% DUAL FORMULATION NOTE:
% Bureaucratic discretion capture is one reading of a broader regulatory capture phenomenon. Statutory capture (direct legislative favoring of industry) and discretion capture (informal enforcement alignment) have distinct ε values and structural mechanisms. Statutory capture is higher-extraction (ε ≥ 0.70, Snare) because it lacks the genuine coordination function. Discretion capture has lower extraction (ε ≈ 0.58, Tangled Rope) because coordination is real. The two should be authored as separate stories and linked via affects_constraints to show the family relationship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bureaucratic_discretion_capture, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
