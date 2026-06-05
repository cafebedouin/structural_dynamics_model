% ============================================================================
% CONSTRAINT STORY: state_killing_authority__deterrence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_killing_authority__deterrence_reading, []).

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
 *   constraint_id: state_killing_authority__deterrence_reading
 *   human_readable: State Killing Authority (Deterrence Reading)
 *   domain: criminal_justice/legal_philosophy/political_theory
 *
 * SUMMARY:
 *   This constraint story represents ONE reading of the contested kernel:
 *   state killing authority. The deterrence reading grounds execution's
 *   legitimacy in instrumental crime prevention — the condemned person's
 *   death is justified because it deters potential criminals from committing
 *   capital offenses. This reading is distinct from retributive readings
 *   (which justify execution on grounds of desert, proportional punishment
 *   for moral guilt) and abolitionist readings (which deny the state's
 *   authority to kill regardless of function). The deterrence reading has
 *   been empirically and normatively contested since the 1970s. Meta-analytic
 *   criminology increasingly shows weak or null deterrent effects of
 *   execution beyond imprisonment. Yet the reading persists in many
 *   jurisdictions, increasingly as institutional theater rather than as
 *   empirically grounded policy. The constraint exhibits Tangled Rope
 *   structure from the beneficiary perspective (society gains
 *   crime-prevention coordination at the cost of asymmetric extraction from
 *   the condemned) and Snare structure from the condemned's perspective
 *   (complete immobility, no recognition as agent, death deployed
 *   instrumentally). The rising theater ratio (0.35 → 0.58 over 40 years)
 *   reflects degradation: as empirical support weakened, the constraint
 *   maintained itself through narrative authority assertion (the myth of
 *   deterrence) rather than through demonstrated causal effect. This is the
 *   diagnostic signature of a Piton — a constraint whose primary function has
 *   atrophied but which persists through institutional momentum.
 *
 * KEY AGENTS:
 *   - Condemned Person: Primary victim (powerless/trapped) — bears full extraction; provides the death-as-deterrent; no exit, no agency, no recognition as agent within the deterrence calculus
 *   - Society / Potential Crime Victims: Primary beneficiary (moderate/constrained) — benefits from crime-prevention coordination; distributed risk if deterrence is false; constrained because they cannot opt out of regime
 *   - State Authority: Institutional beneficiary (institutional/arbitrage) — derives authority from crime-prevention mandate; experiences constraint as pure coordination; full arbitrage capacity to shift away from execution if political pressure mounts
 *   - Retributive Legal Tradition: Competing institutional actor — sees execution as legitimate on grounds of desert, not deterrence; creates conceptual space for execution authority to persist even if deterrence fails
 *   - Abolitionist Movement: Organized agents (organized/constrained) — see execution as failed coordination mechanism; point to alternative regimes (life imprisonment, rehabilitation) that achieve crime prevention without state killing
 *   - Criminological Research Community: Analytical observer — empirical findings increasingly contradict deterrence hypothesis; role in reducing legitimacy of the reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_authority__deterrence_reading, 0.62).
domain_priors:suppression_score(state_killing_authority__deterrence_reading, 0.72).
domain_priors:theater_ratio(state_killing_authority__deterrence_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_authority__deterrence_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(state_killing_authority__deterrence_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(state_killing_authority__deterrence_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_authority__deterrence_reading, tangled_rope).
narrative_ontology:human_readable(state_killing_authority__deterrence_reading, "State Killing Authority (Deterrence Reading)").
narrative_ontology:topic_domain(state_killing_authority__deterrence_reading, "criminal_justice/legal_philosophy/political_theory").

domain_priors:requires_active_enforcement(state_killing_authority__deterrence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_authority__deterrence_reading, 'f2c00851-8982-45b1-a949-6a3c570091df').
narrative_ontology:cs_kernel_codification('f2c00851-8982-45b1-a949-6a3c570091df', formalized).
narrative_ontology:cs_authority_grounding('f2c00851-8982-45b1-a949-6a3c570091df', extraction).
narrative_ontology:cs_interpretation_layer_present('f2c00851-8982-45b1-a949-6a3c570091df').
narrative_ontology:cs_reading_relation('f2c00851-8982-45b1-a949-6a3c570091df', state_killing_authority__retributive_reading, coexists_with).
narrative_ontology:cs_reading_relation('f2c00851-8982-45b1-a949-6a3c570091df', state_killing_authority__abolition_reading, forecloses).
narrative_ontology:cs_axiom('f2c00851-8982-45b1-a949-6a3c570091df', foundational, execution_deters_capital_crime).
narrative_ontology:cs_axiom_status(execution_deters_capital_crime, holdable).
narrative_ontology:cs_axiom_grounding('f2c00851-8982-45b1-a949-6a3c570091df', execution_deters_capital_crime, empirically_contingent).
narrative_ontology:cs_axiom('f2c00851-8982-45b1-a949-6a3c570091df', foundational, instrumental_state_authority_legitimate).
narrative_ontology:cs_axiom_status(instrumental_state_authority_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('f2c00851-8982-45b1-a949-6a3c570091df', instrumental_state_authority_legitimate, deontological).
narrative_ontology:cs_reference_frame('f2c00851-8982-45b1-a949-6a3c570091df', utility_maximizing_crime_control).
narrative_ontology:cs_drift_state('f2c00851-8982-45b1-a949-6a3c570091df', contemporary_criminological_consensus, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f2c00851-8982-45b1-a949-6a3c570091df', '').
narrative_ontology:cs_kernel_id(state_killing_authority__deterrence_reading, state_killing_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_authority__deterrence_reading, potential_crime_victims).
narrative_ontology:constraint_beneficiary(state_killing_authority__deterrence_reading, society_aggregate_safety).
narrative_ontology:constraint_victim(state_killing_authority__deterrence_reading, condemned_person).
narrative_ontology:constraint_victim(state_killing_authority__deterrence_reading, condemned_person_family).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONDEMNED PERSON (SNARE) — Complete immobility. The condemned experiences execution as pure extraction — their life is the payment extracted to fund the deterrence hypothesis. No exit, no appeal to alternative reading, no say in whether the empirical claim (that their death deters others) is true. The state's instrumentalist reading of their death leaves them with zero agency. Maximum experienced extraction.
constraint_indexing:constraint_classification(state_killing_authority__deterrence_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: POTENTIAL CRIME VICTIMS (TANGLED ROPE) — Mixed beneficiary and victim status. They benefit from the coordination function (execution is framed as part of crime-control infrastructure) and from the deterrent effect if it exists. But they are also potentially harmed if the deterrence claim is false — their safety is purchased with an innocent person's execution if the causal mechanism fails. Constrained exit because they bear distributed risk and cannot opt out of the deterrence regime without forsaking the safety coordination structure. Genuine coordination function (group safety) paired with asymmetric extraction (condemned person's life).
constraint_indexing:constraint_classification(state_killing_authority__deterrence_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE AUTHORITY (ROPE) — Institutional beneficiary with full arbitrage capacity. The state derives authority from the instrumental mandate: execution is legitimate precisely because it is framed as crime prevention, not desert. The state sees the constraint as pure coordination — allocating the condemned's death efficiently toward the social good of deterrence. No suppression experienced; the framework is accepted as legitimate by institutional actors. Low theater because the state's own reasoning is internally consistent (even if empirically contestable).
constraint_indexing:constraint_classification(state_killing_authority__deterrence_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: HISTORICAL DETERRENCE DOCTRINE (PITON) — At civilizational scale, the deterrence reading of capital punishment has degraded into performance of authority rather than functional crime control. Empirical evidence does not support deterrent efficacy beyond (or even equal to) imprisonment. The doctrine persists through institutional inertia and theatrical authority assertion, not because it works. Theater ratio high (0.58) reflects that execution continues to be staged as a crime-deterring act despite weak evidence of deterrent effect. The constraint maintains itself through narrative and institutional momentum rather than empirical validation.
constraint_indexing:constraint_classification(state_killing_authority__deterrence_reading, piton,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, some societies may see punishment severity as an inherent necessity for deterrence, with execution as a natural limit imposed by the requirement to protect society. This perspective views the deterrence mechanism as an immutable property of rational crime control. However, this risks naturalizing what is actually a contestable empirical and normative claim about criminal psychology and deterrence. The engine's false-summit detector will flag this as a naturalization of a contingent institutional arrangement.
constraint_indexing:constraint_classification(state_killing_authority__deterrence_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: ABOLITIONIST COALITION (TANGLED ROPE / SCAFFOLD NEIGHBOR) — Organized agents see execution as a failed coordination mechanism that persists through enforcement rather than through genuine crime-reduction function. They perceive genuine coordination in alternative systems (life imprisonment, rehabilitation-focused regimes) that achieve crime prevention without extraction. This reading trades the deterrence benefit (assumed zero or negative) for a coordination mechanism that does not require the condemned's death. Constrained because the current regime is institutionally entrenched, but they have exit path and alternative regime to point to.
constraint_indexing:constraint_classification(state_killing_authority__deterrence_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_killing_authority__deterrence_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(state_killing_authority__deterrence_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(state_killing_authority__deterrence_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_killing_authority__deterrence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(state_killing_authority__deterrence_reading, TR),
    TR >= 0.70.

:- end_tests(state_killing_authority__deterrence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): Moderate-high. The condemned person's death is extracted and instrumentally deployed for social benefit (deterrence). The state claims this extraction is justified by its crime-prevention function. However, if deterrence is weak or absent, the extraction becomes unjustified — the condemned's death is taken for a benefit that does not materialize. The extractiveness value reflects the structural mechanism (state taking life for instrumental purpose) rather than its empirical validation. Rising trajectory (0.48 → 0.62) reflects increasing consciousness of the empirical gap — as deterrence effects weakened over 40 years, the constraint became more transparently extractive (the illusion of justified extraction fell away). Suppression (0.72): High. The condemned has zero exit options. Legal appeals are narrowly constrained. The condemned cannot refuse participation in the framework. No social recognition as an agent whose preferences matter. No opportunity to argue that their death is not producing the claimed deterrent effect. Public discourse is substantially suppressed — execution is typically carried out with limited media access, limited discussion of the condemned's perspective, limited acknowledgment of systemic errors or uncertainty. Theater ratio (0.58): Moderate-high. Executions are performed as justice, as crime prevention, as rational institutional action. The theatrical elements include formal judicial procedures that create appearance of careful deliberation, official narratives about crime prevention effects, and public messaging about safety. The theater has increased over time as the empirical basis weakened — more narrative effort is required to maintain legitimacy when evidence fails.
 *
 * PERSPECTIVAL GAP:
 *   The gap is maximal because the empirical claim (deterrence works) is systematically contested. If true, state and potential victims see Rope/Tangled Rope (justified coordination). If false, condemned and analysts see Snare (pure extraction). If empirically ambiguous (most current position), different stakeholders interpret the gap differently — state sees noise around a real effect (Rope remains), abolitionist sees null finding that disconfirms the reading (Snare is revealed). The gap is not a measurement problem but a constitutive feature of how the reading legitimizes itself. The reading stands or falls with its empirical claim.
 *
 * DIRECTIONALITY LOGIC:
 *   The condemned's directionality is d ≈ 1.0 (full victim, powerless, trapped exit) — they experience maximum extraction f(1.0) ≈ 1.42. The state's directionality is d ≈ 0.05 (full beneficiary, institutional power, arbitrage exit) — they experience negative extraction f(0.05) ≈ -0.12. Society's directionality is mixed: d ≈ 0.50 for distributed risk from potential crime victims who benefit from coordination but bear risk if deterrence fails. The abolitionist coalition's directionality is d ≈ 0.75 (victims of the failed coordination mechanism, organized power, constrained exit) — they experience f(0.75) ≈ 1.15 because they are organizing against a regime that constrains their exit path to alternative coordination mechanisms. The derivation chain runs from beneficiary/victim status (condemned is victim, state is beneficiary) through exit options (trapped vs. arbitrage) to power level (powerless vs. institutional), producing the directionality values that feed chi.
 *
 * MANDATROPHY ANALYSIS:
 *   The deterrence reading avoids mandatrophy by explicitly grounding authority in an instrumental mechanism (deterrence) rather than claiming coordination without extraction. The reading acknowledges that execution is extraction (the condemned person's life is taken) and claims this extraction is justified by its deterrent function. The mandatrophy would arise if the reading claimed execution was pure coordination (Rope) while simultaneously extracting the condemned's life. Instead, the reading claims Tangled Rope: genuine coordination function (crime prevention) paired with asymmetric extraction (condemned's death). The vulnerability is that if the empirical claim fails (deterrence is null), the extraction becomes unjustified, and the constraint reclassifies to Snare. The reading is empirically falsifiable in a way the retributive reading is not. This empirical dependency makes the deterrence reading structurally fragile compared to the retributive reading, which depends on normative commitments rather than empirical validation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_efficacy_empirical,
    'Does execution actually deter crime beyond the deterrent effect of long imprisonment?',
    'Systematic meta-analysis of criminological studies; cross-national comparison of execution vs. non-execution jurisdictions controlling for socioeconomic factors, policing intensity, and conviction certainty; time-series analysis of crime rates following execution moratoria',
    'If deterrence is zero or negative (crimes do not decrease with execution): the state killing authority loses its instrumental legitimacy in this reading. The condemned person''s death becomes pure extraction with no coordination benefit. Snare classification expands to society perspective. Abolitionist reading forecloses this reading. If deterrence is substantial (statistically significant crime reduction): the deterrence reading maintains Rope classification for state and Tangled Rope for potential victims. Abolitionist reading coexists but with weakened normative force.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(deterrence_efficacy_empirical, empirical, 'Whether execution deters crime beyond imprisonment').

omega_variable(
    innocent_execution_risk_integration,
    'How does the risk of executing innocent persons integrate into the expected utility calculation of the deterrence reading?',
    'Quantification of false-conviction rate; modeling of expected harm (executing innocent) vs. expected benefit (crimes prevented via deterrence); sensitivity analysis showing at what error rate the deterrence benefit disappears',
    'If innocent execution risk is systematically excluded from the deterrence calculus (current practice): the framework is empirically incomplete. The condemned person''s perspective as snare reflects this omission — their death counts toward deterrence benefit but potential innocence does not reduce it. If innocent risk is integrated: deterrence benefit must be large enough to offset the harm of wrongful execution. Small deterrence effects become insufficient to justify the constraint. The boundary between Tangled Rope and Snare shifts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(innocent_execution_risk_integration, empirical, 'Role of false-conviction risk in deterrence calculation').

omega_variable(
    kernel_deterrence_vs_retribution_foreclosure,
    'Do the deterrence reading and retributive reading logically foreclose each other, or do they coexist as distinct normative frameworks?',
    'Philosophical analysis of whether instrumentalist and retributive authority grounding can coexist in a single legal framework; empirical analysis of jurisdictions that cite both deterrence and desert in execution decisions',
    'If foreclosure holds: one reading rules the other out, and the kernel has a binary structure. If coexistence holds: both readings can be live within different legal traditions or even within a single jurisdiction''s mixed reasoning. This affects the cs_structure.reading_relations field and the narrative structure of the constraint family.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_deterrence_vs_retribution_foreclosure, conceptual, 'Whether deterrence and retribution logically foreclose each other').

omega_variable(
    reading_under_empirical_contestation,
    'As empirical evidence against deterrence accumulates (current state: meta-analyses show weak or zero effect), how does the deterrence reading maintain its legitimacy claim?',
    'Analysis of how legal institutions respond to null empirical findings: do they retrench, shift to alternative justifications (e.g., retribution, incapacitation), or discard the reading entirely? Tracking of jurisdiction-level abolition decisions following major null findings.',
    'If retrenchment: the deterrence reading becomes increasingly performative (theater ratio rises). The Piton perspective becomes dominant. If shift to retribution: the deterrence reading effectively forecloses, and the retributive reading takes over as legitimizing framework. If abolition: the reading is empirically and normatively rejected, and execution authority dissolves (or shifts to alternative frames).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_under_empirical_contestation, empirical, 'How deterrence reading sustains itself under empirical doubt').

omega_variable(
    instrumental_vs_intrinsic_value_boundary,
    'Where is the boundary between instrumental justification (execution is legitimate because it deters) and intrinsic justification (execution is legitimate because it satisfies retributive or incapacitative needs regardless of deterrence)?',
    'Analysis of legal precedent and statutory language; identification of jurisdictions using pure instrumental framing vs. mixed instrumental-intrinsic framing; historical tracking of shifts between justifications',
    'If boundary is blurred (most jurisdictions cite multiple justifications): the deterrence reading is empirically under-determined. Failure of deterrence might not eliminate execution authority if other justifications stand. If boundary is sharp: the deterrence reading stands or falls with its empirical claim. This affects whether the reading is Rope (pure coordination) or Tangled Rope (mixed with extraction justified on other grounds).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(instrumental_vs_intrinsic_value_boundary, conceptual, 'Boundary between instrumental and intrinsic authority justification').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_authority__deterrence_reading, 1972, 2015).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_1970s, state_killing_authority__deterrence_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(theater_1995, state_killing_authority__deterrence_reading, theater_ratio, 15, 0.5).
narrative_ontology:measurement(theater_2015, state_killing_authority__deterrence_reading, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(extract_1970s, state_killing_authority__deterrence_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(extract_1995, state_killing_authority__deterrence_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(extract_2015, state_killing_authority__deterrence_reading, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(suppress_1970s, state_killing_authority__deterrence_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(suppress_1995, state_killing_authority__deterrence_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(suppress_2015, state_killing_authority__deterrence_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_authority__deterrence_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_killing_authority__deterrence_reading, state_killing_authority__retributive_reading).
narrative_ontology:affects_constraint(state_killing_authority__deterrence_reading, state_killing_authority__abolition_reading).

% DUAL FORMULATION NOTE:
% The state_killing_authority kernel has three distinct readings, each with its own constraint story and distinct ε. The deterrence reading (this file, ε=0.62) depends on empirical claims about crime prevention. The retributive reading (sibling, ε varies) depends on normative claims about desert and proportionality. The abolition reading (sibling, ε varies) is a normativity-first denial of state authority. All three are linked via network.affects_constraints because they contend for legitimacy of the same institutional practice. The three readings are NOT observables of a single constraint — they are structurally distinct commitments with different authority groundings, different victim/beneficiary structures, and different empirical or normative vulnerabilities.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
