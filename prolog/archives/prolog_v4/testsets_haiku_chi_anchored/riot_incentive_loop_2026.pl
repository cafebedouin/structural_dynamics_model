% ============================================================================
% CONSTRAINT STORY: riot_incentive_loop_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_riot_incentive_loop_2026, []).

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
 *   constraint_id: riot_incentive_loop_2026
 *   human_readable: The Riot-Incentive Loop (State-Managed Chaos)
 *   domain: political/social/governance
 *
 * SUMMARY:
 *   The riot-incentive loop represents a structural extraction mechanism
 *   where state security apparatus deploys non-lethal chemical agents (tear
 *   gas) ostensibly for crowd dispersal, but with the systematic effect of
 *   inducing panic, disorientation, and aggressive behavior in exposed
 *   populations. This induced aggression then justifies further suppression,
 *   creating a feedback loop that generates its own legitimacy narrative
 *   while progressively constraining political participation. The constraint
 *   exhibits high extractiveness (0.68) and high suppression (0.75), placing
 *   it squarely in snare territory from the perspectives of affected citizens
 *   and observers. The theater ratio (0.65) reflects that while safety
 *   protocols exist on paper, their enforcement is performative — the same
 *   state apparatus deploying agents faces no accountability for health
 *   harms. The mechanism operates across three temporal scales: immediate
 *   (physical incapacitation during events), biographical (cumulative health
 *   burden and movement restriction for protest participants), and
 *   generational (normalization of chemical suppression as legitimate state
 *   power). The constraint's primary beneficiary is the state security
 *   apparatus, which captures institutional resources, expands surveillance
 *   authority, and consolidates monopoly control over lawful assembly through
 *   the justification narrative of disorder management.
 *
 * KEY AGENTS:
 *   - Protest Participants: Primary victims (powerless/trapped) — physically exposed to tear gas, suffer acute health effects, disorientation, behavioral induction, and cannot exit without severe bodily consequence
 *   - Surrounding Communities: Secondary victims (moderate/constrained) — non-participants exposed to drift, chemical effects, and police response escalation; constrained by residential proximity
 *   - State Security Apparatus: Primary beneficiary (institutional/arbitrage) — captures institutional power, budget expansion, surveillance justification, and monopoly control over political space
 *   - Civil Rights Coalition: Mixed observer (organized/constrained) — benefits from constraint existence (evidence for litigation) while harmed by suppression (restricted operations)
 *   - Public Health System: Degraded institutional actor (institutional/constrained) — formally mandated to manage harms but enforcement is performative under same state authority
 *   - Analytical Observer: Civilizational observer (analytical/analytical) — identifies the feedback loop mechanism and extraction architecture from global perspective
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(riot_incentive_loop_2026, 0.68).
domain_priors:suppression_score(riot_incentive_loop_2026, 0.75).
domain_priors:theater_ratio(riot_incentive_loop_2026, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(riot_incentive_loop_2026, extractiveness, 0.68).
narrative_ontology:constraint_metric(riot_incentive_loop_2026, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(riot_incentive_loop_2026, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(riot_incentive_loop_2026, snare).
narrative_ontology:human_readable(riot_incentive_loop_2026, "The Riot-Incentive Loop (State-Managed Chaos)").
narrative_ontology:topic_domain(riot_incentive_loop_2026, "political/social/governance").

domain_priors:requires_active_enforcement(riot_incentive_loop_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(riot_incentive_loop_2026, state_security_apparatus).
narrative_ontology:constraint_victim(riot_incentive_loop_2026, protest_populations).
narrative_ontology:constraint_victim(riot_incentive_loop_2026, civil_liberties_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PROTEST PARTICIPANT (SNARE) — Physically present at lawful assembly. Subject to tear gas deployment that induces panic, disorientation, and aggressive behavior. Cannot exit without severe physical consequence. Trapped by spatial geography and bodily vulnerability to chemical agents. d≈0.98, f(d)≈1.48, σ=1.0 → χ≈0.67. Pure extraction: constraint exists to suppress political voice and extract compliance through chemical coercion.
constraint_indexing:constraint_classification(riot_incentive_loop_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SURROUNDING COMMUNITY (SNARE) — Non-participants exposed to tear gas drift, secondary chemical effects, and escalating police response. Constrained by residential proximity and inability to flee effectively. d≈0.80, f(d)≈1.20, σ=1.0 → χ≈0.82. Bears extraction through health burden and restricted movement without beneficiary relationship.
constraint_indexing:constraint_classification(riot_incentive_loop_2026, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE SECURITY APPARATUS (ROPE) — Sees tear gas as a coordination mechanism: manages the 'disorder' narrative, justifies expanded funding and surveillance budgets, and consolidates institutional power through demonstrating necessity. From this perspective, the chemical agent solves the collective action problem of maintaining state authority during mass protests. d≈0.02, f(d)≈-0.16, σ=1.0 → χ≈-0.11. Net beneficiary through expanded institutional control and resource capture.
constraint_indexing:constraint_classification(riot_incentive_loop_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CIVIL RIGHTS COALITION (TANGLED ROPE) — Organized agents (legal advocates, human rights monitors, independent media) benefit from the constraint's existence as it provides evidence for litigation and advocacy campaigns, yet are harmed by the suppression of speech it enables. Experiences both coordination (documenting harms, building legal cases) and extraction (restricted access, police surveillance). d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.51. Mixed: constraint gives them material for work while restricting their operational space.
constraint_indexing:constraint_classification(riot_incentive_loop_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: PUBLIC HEALTH SYSTEM (PITON) — Formally mandated to manage tear gas health effects and document chemical exposure harms, but enforcement is constrained by same state apparatus deploying agents. Theater ratio=0.65: health protocols exist (chemical safety guidelines, emergency response procedures) but their enforcement is performative when the deploying authority faces no accountability. d≈0.60, f(d)≈0.82, σ=1.0 → χ≈0.53. Degraded function through institutional capture by security apparatus.
constraint_indexing:constraint_classification(riot_incentive_loop_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — Viewing the constraint from a civilizational/global analytical position reveals that riot-incentive dynamics are neither natural law nor coordination mechanism, but a deliberately engineered extraction system. Chemical agent deployment creates the very behavior (aggression, property damage) that justifies further suppression, creating a feedback loop that extracts political capacity from citizens. d≈0.75, f(d)≈1.10, σ=1.2 → χ≈0.90. The constraint's function is to generate its own legitimacy narrative.
constraint_indexing:constraint_classification(riot_incentive_loop_2026, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(riot_incentive_loop_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(riot_incentive_loop_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(riot_incentive_loop_2026, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(riot_incentive_loop_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(riot_incentive_loop_2026, TR),
    TR >= 0.70.

:- end_tests(riot_incentive_loop_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint systematically extracts political participation capacity from citizens through involuntary chemical exposure. The extraction is not temporary or reversible — cumulative health burden (respiratory damage, psychological trauma, movement restriction) accumulates across repeated exposures. The mechanism is designed to extract compliance through physical incapacitation and fear. Suppression (0.75): Very high. Barriers to exit are severe: (1) physical geography — participants cannot leave without passing through chemical agent zone, (2) bodily vulnerability — exposure effects (lacrimation, respiratory distress) impede coordinated exit, (3) legal jeopardy — police create arrest risk during dispersal, making orderly retreat impossible, (4) health burden — tear gas effects persist for hours, preventing immediate self-help. Theater ratio (0.65): Moderate-high, increasing over interval. While riot control procedures exist (safety guidelines, medical protocols), their enforcement is performative. State security apparatus faces minimal accountability for deployment decisions or health harms. The performative character has increased over the interval as deployments have become routine and legitimacy narratives have calcified ('crowd control is necessary'), decoupling from actual security outcomes (most tear-gassed assemblies were lawful, non-violent before exposure).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp perspectival divergence. Protest participants see pure extraction (Snare) — they are victims without beneficiary relationship. The civil rights coalition sees mixed extraction and benefit (Tangled Rope) — the constraint provides evidence for their work while restricting their operations. The state security apparatus sees coordination (Rope) — the constraint solves the institutional problem of maintaining monopoly control. The public health system sees degraded function (Piton) — formal mandate to manage harms exists but enforcement is compromised. The analytical observer sees the extraction architecture clearly (Snare at global scale) and identifies the feedback loop mechanism. The perspectival gap between the state apparatus's 'necessity' framing and the participant's 'weaponized chemistry' framing is the entire extraction mechanism — the constraint's power depends on suppressing the analytical perspective that reveals its true function.
 *
 * DIRECTIONALITY LOGIC:
 *   Protest Participants: Victim + trapped → d≈0.98, f(d)≈1.48. Maximum extraction. No exit pathway, involuntary exposure, health burden, behavioral induction. State Security Apparatus: Beneficiary + arbitrage → d≈0.02, f(d)≈-0.16. Net beneficiary with institutional arbitrage (can capture resources, define deployment, face no accountability). Civil Rights Coalition: Mixed + constrained → d≈0.55, f(d)≈0.75. Constrained by police surveillance yet benefit from evidence generation. Public Health System: Constrained + victim status → d≈0.60, f(d)≈0.82. Formally charged with harm management but subordinate to security apparatus. Surrounding Communities: Victim + constrained → d≈0.80, f(d)≈1.20. High extraction through involuntary exposure and movement restriction without direct political agency. Analytical Observer: Observational position + global → d≈0.75, f(d)≈1.10. Sees the extraction mechanism clearly but has no enforcement leverage against institutional apparatus.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This constraint resolves mandatrophy (ε > 0.70 threshold approaching with value at 0.68) by clearly establishing that the riot-incentive loop is pure extraction, not coordination. The mechanism satisfies no genuine collective action problem — crowd dispersal can be achieved through alternatives (water cannons, acoustic dispersal, dialogue) that do not induce aggression. The constraint's legitimacy narrative claims necessity ('riots must be managed'), but the structural data reveals deliberate design: (1) tear gas causally increases aggression rather than suppressing it, (2) alternatives exist but are not deployed, (3) accountability mechanisms are absent, (4) health harms are systematically under-reported. The mandatrophy check: Is this Snare or Rope? The presence of a coordination claim ('maintaining order') does not make it Rope, because the coordination function is false — the constraint does not solve any genuine collective action problem; it creates the very problem it claims to solve. The theater ratio (0.65) rising to 0.65 over the interval reflects that performative legitimacy narrative is the only thing sustaining the constraint. Unlike legitimate Tangled Rope (which provides real coordination alongside extraction), this constraint provides no coordination function — it is pure extraction wrapped in necessity theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causality_threshold_aggression,
    'Does tear gas exposure causally increase aggression and property damage, or does it suppress behavior that would otherwise occur?',
    'Controlled exposure studies (ethically limited); comparative analysis of protest escalation patterns in jurisdictions with/without tear gas use; plasma biomarker analysis of stress hormone elevation post-exposure',
    'If causally increases: constraint is pure extraction (snare confirmed). If suppresses existing aggression: constraint is coordination failure masquerading as security necessity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(causality_threshold_aggression, empirical, 'Causal mechanism: tear gas increasing vs suppressing aggression').

omega_variable(
    feedback_loop_intentionality,
    'Is the riot-incentive loop a deliberate operational design or an emergent property of available crowd-control options?',
    'Analysis of training protocols, deployment guidelines, and operational directives from internal state security documents; interviews with command-level officers; institutional design review of crowd-control procedures',
    'If deliberate: snare classification holds; institutional extraction mechanism. If emergent: constraint may be reclassified as failed coordination (tangled rope with high extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(feedback_loop_intentionality, empirical, 'Whether riot-incentive loop is intentional design or emergent behavior').

omega_variable(
    alternative_dispersal_availability,
    'What proportional alternative mechanisms exist (water cannons, acoustic dispersal, dialogue) that achieve crowd dispersal without inducing aggression?',
    'Comparative effectiveness review across jurisdictions; cost-benefit analysis of alternatives; civil unrest outcome measurement by dispersal method',
    'If true alternatives available: suppression gate (≥0.60) is partially artificial, and constraint reclassifies toward tangled_rope. If alternatives are genuinely ineffective: snare classification solidifies.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_dispersal_availability, empirical, 'Availability of non-aggressive crowd dispersal alternatives').

omega_variable(
    legitimacy_narratives_decoupling,
    'How many iterations of the feedback loop occur before legitimacy narratives decouple from objective security outcomes?',
    'Content analysis of state security communications before/after tear gas deployments; polling on public trust in security institutions; correlation between escalation severity and subsequent policy changes',
    'Short decoupling horizon: narrative sustainability is low, constraint becomes fragile (may degrade to piton). Long decoupling horizon: extraction mechanism is highly efficient, snare classification strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_narratives_decoupling, empirical, 'Timeline for legitimacy narratives to decouple from security outcomes').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(riot_incentive_loop_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(riot_tr_t0, riot_incentive_loop_2026, theater_ratio, 0, 0.4).
narrative_ontology:measurement(riot_tr_t5, riot_incentive_loop_2026, theater_ratio, 5, 0.52).
narrative_ontology:measurement(riot_tr_t10, riot_incentive_loop_2026, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(riot_be_t0, riot_incentive_loop_2026, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(riot_be_t5, riot_incentive_loop_2026, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(riot_be_t10, riot_incentive_loop_2026, base_extractiveness, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(riot_incentive_loop_2026, enforcement_mechanism).
narrative_ontology:affects_constraint(riot_incentive_loop_2026, mass_surveillance_consent_trading).
narrative_ontology:affects_constraint(riot_incentive_loop_2026, police_qualified_immunity_doctrine).
narrative_ontology:affects_constraint(riot_incentive_loop_2026, protest_permit_gatekeeping).

% DUAL FORMULATION NOTE:
% The riot-incentive loop is downstream of structural state monopoly claims but represents a distinct extraction architecture. The upstream constraint (state legitimacy and monopoly control justification) depends on the riot loop to generate evidence of 'disorder' requiring suppression. The downstream constraints (qualified immunity, permit gatekeeping) are institutional structures that sustain the loop by preventing accountability and restricting alternative assembly pathways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(riot_incentive_loop_2026, institutional, 0.02).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
