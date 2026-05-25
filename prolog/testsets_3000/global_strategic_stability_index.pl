% ============================================================================
% CONSTRAINT STORY: global_strategic_stability_index
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_global_strategic_stability_index, []).

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
 *   constraint_id: global_strategic_stability_index
 *   human_readable: Global Strategic Stability Index
 *   domain: geopolitics/security
 *
 * SUMMARY:
 *   The Global Strategic Stability Index represents the complex institutional
 *   apparatus through which hegemonic powers maintain predictability in
 *   military competition and deter escalation among nuclear-armed states.
 *   Ostensibly a coordination mechanism enabling all parties to understand
 *   red lines and signal intent clearly, the index simultaneously functions
 *   as an extraction mechanism: it constrains rising powers from certain
 *   military developments, presupposes the legitimacy of existing security
 *   arrangements, and embeds hegemonic preferences into the definition of
 *   'stability' itself. The constraint exhibits the full range of DR
 *   classifications depending on structural position. The hegemonic core
 *   experiences it as pure coordination (Rope) — their preferred equilibrium
 *   is enforced through rules they authored and can interpret. Rising powers
 *   experience it as mixed coordination and extraction (Tangled Rope) — they
 *   benefit from the predictability but are constrained from challenging the
 *   order. The global periphery experiences it as pure extraction (Snare) —
 *   rules-constrained from within a system designed for superpower
 *   competition, unable to exit. The constraint's theater ratio has risen
 *   from 0.35 to 0.65 over twenty years, reflecting degradation of actual
 *   verification capacity masked by increased rhetorical emphasis on norms.
 *   International institutions maintain legitimacy through declarations and
 *   forums while their actual capacity to verify compliance or deter
 *   violations has atrophied. Simultaneously, emerging multipolar frameworks
 *   (BRICS, regional security arrangements, strategic partnerships outside
 *   NATO) represent organized resistance with a natural sunset logic — as
 *   multipolarity crystallizes, the unipolar index loses binding force. The
 *   constraint's extractiveness has risen from 0.42 to 0.58, indicating that
 *   the gap between stated coordination function and actual extraction has
 *   widened as the index's functional capacity has declined.
 *
 * KEY AGENTS:
 *   - Hegemonic Power & Core Alliance (US/NATO bloc): Institutional/arbitrage — primary beneficiary, authors index definition, maintains structural advantage in verification asymmetries
 *   - Rising Powers (China, India, regional powers): Moderate/constrained — experience asymmetric extraction despite coordination benefits; constrained from certain military postures despite growing capability
 *   - Peer Competitors (Russia, China in specific domains): Powerful/mobile — near-peer military actors with exit options; experience Tangled Rope equilibrium with mutual gains from stability but mutual constraints from index design
 *   - Global South / Periphery States: Powerless/trapped — constrained by index rules designed for superpower competition; no voice in definition; bear stability costs without corresponding security benefits
 *   - International Institutions (UN, IAEA, treaty secretariats): Institutional/constrained — maintain organizational legitimacy through theater while verification function atrophies; increasingly performative role
 *   - Multipolar Coalition (BRICS, regional alliances, NGO networks): Organized/mobile — mobilizing alternative frameworks with sunset logic; exit pathways increasingly available as multipolarity solidifies
 *   - Analytical Observer: Analytical/analytical — risks naturalizing contingent hegemonic order as immutable realism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(global_strategic_stability_index, 0.58).
domain_priors:suppression_score(global_strategic_stability_index, 0.62).
domain_priors:theater_ratio(global_strategic_stability_index, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(global_strategic_stability_index, extractiveness, 0.58).
narrative_ontology:constraint_metric(global_strategic_stability_index, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(global_strategic_stability_index, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(global_strategic_stability_index, tangled_rope).
narrative_ontology:human_readable(global_strategic_stability_index, "Global Strategic Stability Index").
narrative_ontology:topic_domain(global_strategic_stability_index, "geopolitics/security").

domain_priors:requires_active_enforcement(global_strategic_stability_index).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(global_strategic_stability_index, hegemonic_power).
narrative_ontology:constraint_beneficiary(global_strategic_stability_index, status_quo_alliance).
narrative_ontology:constraint_victim(global_strategic_stability_index, rising_powers).
narrative_ontology:constraint_victim(global_strategic_stability_index, global_south_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DESTABILIZED PERIPHERY (SNARE) — Nations and regions outside the core alliance structure experience the index as pure extraction. Constrained by rules they did not write, unable to exit, bearing the full cost of stability definitions that exclude their interests. Maximum extraction, no coordination benefit perceived.
constraint_indexing:constraint_classification(global_strategic_stability_index, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: RISING POWER (TANGLED ROPE) — Experiences both coordination benefit (access to rules-based order, predictability in military signaling) and asymmetric extraction (constrained from challenging status quo, defensive military posture required despite growth). Coordination exists alongside extraction — genuine but asymmetric.
constraint_indexing:constraint_classification(global_strategic_stability_index, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: HEGEMONIC POWER & CORE ALLIANCE (ROPE) — Experiences the index as coordination mechanism for preserving their preferred equilibrium. Rules-based order benefits them directly. Exit options abundant (can withdraw, redefine terms). Net beneficiary — the constraint coordinates their interests.
constraint_indexing:constraint_classification(global_strategic_stability_index, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INTERNATIONAL INSTITUTIONS (PITON) — UN, NATO command structures, arms control regimes exist but their functional verification capability has atrophied. Institutions maintain legitimacy through theater (forums, declarations, monitoring reports) while actual strategic behavior increasingly escapes formal oversight. Theater ratio high; actual constraint on state behavior low and declining.
constraint_indexing:constraint_classification(global_strategic_stability_index, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: MULTIPOLAR TRANSITION COALITION (SCAFFOLD) — Organized non-state actors (scientific academies, NGOs, regional powers pursuing détente) see the index as a temporary coordination problem with a natural sunset. They perceive the hegemonic framework degrading (theater rising, effectiveness falling) and mobilize alternative polarity frameworks (BRICS, regional balances). Extraction suppressed by exit availability and organized resistance.
constraint_indexing:constraint_classification(global_strategic_stability_index, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: PEER COMPETITOR (TANGLED ROPE) — Near-peer military power (e.g., China, Russia relative to US/NATO) experiences the index as both coordination mechanism (mutual assured destruction prevents escalation) and extraction (constrained from certain military developments, cyber operations monitored). Power level high; exit options available but costly (arms race escalation). Moderate extraction with genuine coordination benefit.
constraint_indexing:constraint_classification(global_strategic_stability_index, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / REALIST VIEW (MOUNTAIN) — From the civilizational/universal frame, strategic stability is inherent to nuclear deterrence: the index is simply the mathematical consequence of mutual vulnerability. States cannot exit because nuclear weapons' existence makes the constraint immutable. However, this naturalizes what is contingent — the index is a human institutional construction, not a law of physics. Engine flags this as false summit.
constraint_indexing:constraint_classification(global_strategic_stability_index, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(global_strategic_stability_index_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(global_strategic_stability_index, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(global_strategic_stability_index, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(global_strategic_stability_index, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(global_strategic_stability_index, TR),
    TR >= 0.70.

:- end_tests(global_strategic_stability_index_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The index extracts from rising powers and the global south through constrained military development options, asymmetric verification burdens (inspected while inspectors are not), and definitional authority concentrated in hegemonic hands. But extraction is not maximal because genuine coordination benefits exist — mutual deterrence prevents escalation, signaling reduces miscalculation risk, and all parties value predictability. The metric reflects that coordination and extraction are genuinely intertwined. Suppression (0.62): High. Rising powers face significant barriers to exiting the index: the nuclear deterrent framework makes unilateral withdrawal impossible, economic integration creates dependencies, and alternative security frameworks are nascent and untested. Periphery states face total suppression — no realistic exit option. However, suppression is not absolutely total for all actors; organized powers can develop alternative frameworks (hence Scaffold and Multipolar Coalition perspectives exist). Theater ratio (0.65): High-moderate. Traditional verification mechanisms (arms control treaties, monitoring regimes, intelligence sharing) have declining functional capacity, but their legitimacy persists through increasing rhetorical emphasis on norms, declarations, and forums. The gap between what institutions claim to do and what they actually verify has widened over the measurement interval.
 *
 * PERSPECTIVAL GAP:
 *   The gap between hegemonic (Rope) and periphery (Snare) perspectives is maximum. The gap between hegemonic (Rope) and peer competitor (Tangled Rope) perspectives reflects asymmetric constraint architecture: structurally similar power levels but different extraction profiles because the index encodes hegemonic interests. The gap between institutional (Piton) and organized alternative (Scaffold) perspectives reflects the lifecycle of institutional order — the index is degrading (theater rising, function falling) while alternatives are forming (multipolarity rising, index binding force falling). These gaps are not measurement errors; they are the constraint's signature. When a structure produces maximum perspectival divergence across all classification types, it is either genuinely neutral (does not happen for constraints of this extractiveness level) or highly extractive with sophisticated coordination cover. The Global Strategic Stability Index is the latter.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values are derived from structural position: beneficiary status, power level, and exit options. The hegemonic core (institutional/arbitrage) has d ≈ 0.05 — they are net beneficiaries with exit options, experiencing negative or near-zero effective extraction. Rising powers (powerful/mobile or moderate/constrained) have d ≈ 0.55-0.70 depending on mobility — they experience moderate-to-high extraction despite their power level because they are targets of the extraction system. Periphery states (powerless/trapped) have d ≈ 0.95 — maximum extraction, no exit, no benefit. The peer competitor (powerful/mobile) has d ≈ 0.48-0.55 — powerful enough to threaten escalation but constrained by deterrent equilibrium, so extraction is moderate despite power. Organized alternatives (organized/mobile) have d ≈ 0.30-0.40 — they have agency and exit pathways, reducing experienced extraction. These directionality values map to the perspectival gap: low d produces Rope (coordination perceived), high d produces Snare (extraction perceived), moderate d with mixed coordination/extraction produces Tangled Rope.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that the classification depends critically on structural position. There is no single correct type. The mandatrophy arises from conflating 'What is the objective nature of this constraint?' (unanswerable — it is whatever it does to specific agents) with 'Which perspective is most legitimate?' (also unanswerable — legitimacy is not a metric the system measures). The resolution: all seven types are structurally correct from their respective positions. The hegemonic core genuinely experiences coordination (Rope). Rising powers genuinely experience mixed coordination and extraction (Tangled Rope). The periphery genuinely experiences pure extraction (Snare). The analytical realist genuinely risks false summit (Mountain) — this is not an error in the analysis but a structural feature of how realism naturalizes institutional arrangements. The mandatrophy is resolved by accepting that the index is a presheaf over multiple observational contexts, and no single type is 'the' answer. The system is working correctly when it produces multiple types — this is diagnostic evidence of extraction concealed within coordination language.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hegemonic_definition_boundary,
    'Who defines what counts as ''stability'' in the index, and is that definition biased toward hegemonic preferences?',
    'Comparative analysis of index metrics across decades; identification of metrics added/removed when they disadvantaged core alliance members vs when they disadvantaged others; structural decomposition of index weights by beneficiary preference',
    'If definition is neutral: constraint is pure coordination (Rope). If systematically biased: constraint is extractive (Snare or Tangled Rope). If bias visible and contested: Tangled Rope classification confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hegemonic_definition_boundary, conceptual, 'Whether stability definition favors hegemonic preferences').

omega_variable(
    verification_capacity_decay,
    'Has institutional capacity to verify compliance with the index''s stability rules actually declined, or is apparent decay merely theater amplifying perceived instability?',
    'Technical audit of verification infrastructure (satellite capability, signal intelligence, treaty monitoring); comparison of false positive rates in stability assessments across time periods; structural analysis of detection limits for violations',
    'If capacity declined: Piton classification confirmed (theater masking lost function). If capacity maintained: institutions are deliberately signaling weakness to preserve multipolar negotiation room. If capacity improved but theater increased: system is deliberately obscuring verification for political reasons.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_capacity_decay, empirical, 'Whether institutional verification capacity has actually declined').

omega_variable(
    multipolar_sustainability,
    'Can alternative multipolar frameworks (BRICS, regional balance clusters) sustain genuine coordination at the scale required to replace the hegemonic index?',
    'Empirical test: simulation of multipolar coordination mechanisms under crisis scenarios; historical case analysis of regional balance-of-power systems and their stability properties; measurement of coordination success rates in non-hegemonic frameworks',
    'If sustainable: Scaffold sunset is real, index extractiveness will decline. If not sustainable: Scaffold perspective is aspirational; rising powers face continued asymmetric extraction under alternatives. If partially sustainable: constraint will fragment into regional Tangled Ropes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(multipolar_sustainability, empirical, 'Whether multipolar alternatives can sustain coordination').

omega_variable(
    cyber_asymmetry_unmeasured,
    'Does the index''s focus on kinetic/nuclear metrics systematically exclude or underweight cyber and information warfare domains where non-hegemonic powers have asymmetric advantage?',
    'Audit of index metrics: list all measured domains; compare prevalence of kinetic/nuclear metrics vs cyber/information metrics; identify capability asymmetries in each domain; measure correlation between metric focus and national advantage',
    'If cyber is systematically underweighted: index extraction is higher than measured (hidden extraction in unmeasured domains). If evenly weighted: index faithfully represents constraint structure. If cyber is overweighted: non-hegemonic powers have hidden advantage not reflected in index design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cyber_asymmetry_unmeasured, empirical, 'Whether cyber domain is systematically underrepresented in index').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(global_strategic_stability_index, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gss_tr_t0, global_strategic_stability_index, theater_ratio, 0, 0.35).
narrative_ontology:measurement(gss_tr_t10, global_strategic_stability_index, theater_ratio, 10, 0.52).
narrative_ontology:measurement(gss_tr_t20, global_strategic_stability_index, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(gss_be_t0, global_strategic_stability_index, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(gss_be_t10, global_strategic_stability_index, base_extractiveness, 10, 0.51).
narrative_ontology:measurement(gss_be_t20, global_strategic_stability_index, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(global_strategic_stability_index, enforcement_mechanism).
narrative_ontology:affects_constraint(global_strategic_stability_index, nuclear_deterrence_mutual_vulnerability).
narrative_ontology:affects_constraint(global_strategic_stability_index, arms_control_treaty_compliance).
narrative_ontology:affects_constraint(global_strategic_stability_index, cyber_escalation_ladder).
narrative_ontology:affects_constraint(global_strategic_stability_index, great_power_competition_framework).

% DUAL FORMULATION NOTE:
% The Global Strategic Stability Index is downstream of specific deterrence mechanisms (nuclear vulnerability, arms control verification) but represents a distinct institutional constraint that aggregates and legitimizes hegemonic ordering of these mechanisms. The index itself is a story about how hegemonic definition authority constrains the global south and rising powers.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(global_strategic_stability_index, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
