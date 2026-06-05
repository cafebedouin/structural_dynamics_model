% ============================================================================
% CONSTRAINT STORY: other_peoples_troubles_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_other_peoples_troubles_2026, []).

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
 *   constraint_id: other_peoples_troubles_2026
 *   human_readable: The Asymmetry of Vicarious Resilience
 *   domain: social/psychological
 *
 * SUMMARY:
 *   The asymmetry of vicarious resilience names the psychological and
 *   structural distance that allows observers to 'bear' the suffering of
 *   others with ease — a ease that is systematically unavailable to those
 *   suffering or to those emotionally proximate to suffering. This constraint
 *   operates simultaneously as a coordination mechanism (allowing humans to
 *   care about multiple populations without affective overload) and as an
 *   extraction mechanism (enabling comfortable moral status without
 *   proportional action or resource commitment). The theatrical component—the
 *   elaborate performance of humanitarian concern, moral advocacy, and
 *   compassionate engagement—masks the underlying asymmetry: distant
 *   observers experience suffering as manageable narratives; proximate agents
 *   experience it as overwhelming presence. The constraint's extractiveness
 *   has increased over the measurement interval (0.42 → 0.58) as
 *   institutional mechanisms for processing others' suffering have
 *   professionalized, creating specialized 'compassion workers' who bear the
 *   emotional labor while distant observers consume carefully managed
 *   representations. The theater ratio (0.58) reflects that humanitarian
 *   infrastructure increasingly functions as ritual maintenance of moral
 *   identity rather than proportional suffering reduction.
 *
 * KEY AGENTS:
 *   - Suffering Population: Primary victim (powerless/trapped) — experiencing actual harm with no control over representational distance or observer engagement
 *   - Empathetic Intermediary: Secondary victim (moderate/constrained) — bearing authentic emotional labor of engagement while distant observers experience managed vicarious exposure
 *   - Distant Observer: Primary beneficiary (institutional/arbitrage) — experiencing psychological comfort through controlled exposure to others' suffering; gains moral status without proportional cost
 *   - Collective Suffering Movement: Organized victim group (organized/constrained) — attempting to close representational distance through visibility and testimony, constrained by dependence on distant observers' attention
 *   - Humanitarian Establishment: Institutional beneficiary (institutional/arbitrage) — maintaining elaborate apparatus for translating others' suffering into administrable categories; theater-to-function ratio increasingly skewed toward ritual
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — recognizes dual functionality (coordination + extraction) and identifies structural conditions enabling the asymmetry
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(other_peoples_troubles_2026, 0.58).
domain_priors:suppression_score(other_peoples_troubles_2026, 0.62).
domain_priors:theater_ratio(other_peoples_troubles_2026, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(other_peoples_troubles_2026, extractiveness, 0.58).
narrative_ontology:constraint_metric(other_peoples_troubles_2026, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(other_peoples_troubles_2026, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(other_peoples_troubles_2026, snare).
narrative_ontology:human_readable(other_peoples_troubles_2026, "The Asymmetry of Vicarious Resilience").
narrative_ontology:topic_domain(other_peoples_troubles_2026, "social/psychological").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(other_peoples_troubles_2026, distant_observers).
narrative_ontology:constraint_beneficiary(other_peoples_troubles_2026, moral_status_claimants).
narrative_ontology:constraint_victim(other_peoples_troubles_2026, suffering_population).
narrative_ontology:constraint_victim(other_peoples_troubles_2026, empathetic_intermediaries).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUFFERING POPULATION (SNARE) — Those experiencing actual harm have no exit from their condition and cannot control others' vicarious perception of it. Trapped both in suffering and in the representational asymmetry. d≈0.98, f(d)≈1.42, σ=0.8 → χ≈0.65.
constraint_indexing:constraint_classification(other_peoples_troubles_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: EMPATHETIC INTERMEDIARY (SNARE) — Advocates, caregivers, journalists bear emotional labor of authentic engagement while distant observers experience only managed, tolerable vicarious exposure. Constrained by responsibility to represent accurately; trapped by emotional burden. d≈0.80, f(d)≈1.15, σ=1.0 → χ≈0.67.
constraint_indexing:constraint_classification(other_peoples_troubles_2026, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DISTANT OBSERVER / COMFORTABLE POSITION (ROPE) — Experiences the constraint as pure coordination: can engage with others' suffering at psychological distance, building moral status through measured concern without bearing actual costs. d≈0.08, f(d)≈-0.11, σ=1.2 → χ≈-0.06. Net beneficiary via psychological distance.
constraint_indexing:constraint_classification(other_peoples_troubles_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: COLLECTIVE SUFFERING MOVEMENT (SNARE) — Organized victims attempt to close the vicarious distance through testimony, visibility campaigns, and demand for action, but the psychological distance persists as a structural feature. Constrained by dependence on distant observers' voluntary attention. d≈0.85, f(d)≈1.20, σ=1.2 → χ≈0.86.
constraint_indexing:constraint_classification(other_peoples_troubles_2026, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: HUMANITARIAN ESTABLISHMENT (PITON) — Formal organizations (NGOs, UN bodies, relief agencies) maintain elaborate apparatus for translating distant others' suffering into manageable, administrable categories. Ritual maintenance of compassion through institutional forms; actual function (reducing suffering) is secondary to theater (demonstrating concern). theater_ratio=0.58 approaches piton threshold. d≈0.15, f(d)≈-0.02, σ=1.2 → χ≈-0.01.
constraint_indexing:constraint_classification(other_peoples_troubles_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational view, psychological distance is both a coordination mechanism (allowing simultaneous care for multiple populations without affective collapse) and an extraction mechanism (enabling comfortable moral status without proportional action). The asymmetry is structural: both functions coexist. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.38.
constraint_indexing:constraint_classification(other_peoples_troubles_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(other_peoples_troubles_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(other_peoples_troubles_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(other_peoples_troubles_2026, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(other_peoples_troubles_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(other_peoples_troubles_2026, TR),
    TR >= 0.70.

:- end_tests(other_peoples_troubles_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts significant value from suffering populations and intermediaries. Suffering populations lose representational autonomy and see their harm processed through distant observers' psychological tolerance for narratives. Intermediaries are extracted from through emotional labor and dependence on distant observers' voluntary attention. Distant observers and humanitarian institutions benefit through moral status gain without proportional action cost. The value reflects that the extraction is substantial but not total — some resources do flow to suffering populations, and some genuine coordination benefits exist. Suppression (0.62): Moderate-high. Substantial barriers prevent suffering populations and intermediaries from challenging the asymmetry. Psychological distance itself is a suppression mechanism (easier to ignore distant suffering than proximate). Institutional structures (media gatekeeping, funding flows, representation standards) suppress alternative framings. Emotional exhaustion suppresses sustained mobilization by intermediaries. Theater ratio (0.58): Moderate. Humanitarian institutional practice increasingly focuses on performance of concern (media campaigns, awareness raising, symbolic gestures) rather than proportional suffering reduction. The theater has grown as professional humanitarian infrastructure has expanded, creating specialized roles that perform concern rather than directly ameliorate suffering.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits radical perspectival divergence. The suffering population and empathetic intermediaries classify it as Snare (extractive, high suppression). The distant observer and humanitarian establishment classify it as Rope or Piton (coordination, low personal extraction). The collective movement sees it as Snare with attempted closure mechanisms (organized resistance to the asymmetry). The analytical observer sees Tangled Rope (dual functionality): the constraint simultaneously enables and disables moral action. The gap is not empirical disagreement but structural position — agents occupy different places in the asymmetry and experience different classification outcomes from identical metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   Suffering population: Victim + trapped → d≈0.98, f(d)≈1.42. Maximum extraction. No exit from suffering or from being represented at distance. Empathetic intermediary: Victim + constrained → d≈0.80, f(d)≈1.15. High extraction. Constrained by responsibility to represent authentically while bearing emotional labor that distant observers avoid. Distant observer: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Net beneficiary through psychological distance. Can engage or disengage at will; moral status gain without cost. Collective suffering movement: Victim + organized + constrained → d≈0.85, f(d)≈1.20. High extraction despite organization; organization is constrained by dependence on distant observers' voluntary attention. Humanitarian establishment: Beneficiary + institutional + arbitrage → d≈0.15, f(d)≈-0.02. Near-zero effective extraction for institutional position; benefits through ritual maintenance. Analytical observer: d≈0.50, f(d)≈0.65. Symmetric position enabling recognition of dual functionality.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empathy_distance_curve,
    'Does psychological distance produce a smooth decay in empathetic activation or a binary collapse of care below detection threshold?',
    'Neuroscience: fMRI studies of empathetic response variation across physical/social distance gradients; behavioral economics: dictator game variations by victim distance and salience',
    'If smooth decay: vicarious resilience is coordinative (Rope). If binary collapse: vicarious resilience is extractive (Snare) with sharp phase transition marking where moral obligation is perceived to vanish.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empathy_distance_curve, empirical, 'Shape of empathetic response decay with distance').

omega_variable(
    representational_control_asymmetry,
    'Can suffering populations effectively control how their suffering is represented to distant observers, or does the representational mechanism itself constrain their agency?',
    'Analysis of narrative control: cases where victim populations'' own framings of their suffering differ from institutional/media framings; tracking power dynamics in testimony collection, humanitarian reporting standards, and victim-centered narrative development',
    'If representational control is possible: classification shifts toward Tangled Rope (mixed coordination and extraction). If representational mechanism is inherently extractive regardless of victim intent: Snare classification confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(representational_control_asymmetry, empirical, 'Whether suffering populations can control their representational narrative').

omega_variable(
    action_gap_causation,
    'Does the gap between vicarious concern and actual resource commitment arise from psychological distance per se, or from institutional structures that decouple empathy from obligation?',
    'Historical comparison: cases where institutional structures enforce proportional resource commitment to expressed concern (e.g., sliding-scale tithing, mandatory aid contributions) vs cases with purely voluntary coupling; measurement of action rates under different enforcement regimes',
    'If psychological distance is the primary cause: the constraint is inherent (near-Mountain classification for some observers). If institutional decoupling is primary: the constraint is socially contingent (Snare with potential for redesign).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(action_gap_causation, empirical, 'Whether psychological distance or institutional decoupling drives action gaps').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(other_peoples_troubles_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vicres_tr_t0, other_peoples_troubles_2026, theater_ratio, 0, 0.38).
narrative_ontology:measurement(vicres_tr_t5, other_peoples_troubles_2026, theater_ratio, 5, 0.48).
narrative_ontology:measurement(vicres_tr_t10, other_peoples_troubles_2026, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(vicres_be_t0, other_peoples_troubles_2026, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(vicres_be_t5, other_peoples_troubles_2026, base_extractiveness, 5, 0.51).
narrative_ontology:measurement(vicres_be_t10, other_peoples_troubles_2026, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(other_peoples_troubles_2026, information_standard).
narrative_ontology:affects_constraint(other_peoples_troubles_2026, attention_economy_scarcity).
narrative_ontology:affects_constraint(other_peoples_troubles_2026, moral_hazard_distant_action).
narrative_ontology:affects_constraint(other_peoples_troubles_2026, representational_autonomy).

% DUAL FORMULATION NOTE:
% The vicarious resilience constraint forms a family with three decomposed claims: (1) the psychological distance mechanism itself (neurobiological/coordination), (2) the institutional apparatus that exploits distance (social/extraction), and (3) the representational control asymmetry (power/agency). This story addresses the unified constraint; decomposition into separate stories may be required if empirical data reveals that distance-decay, institutional-extraction, and representational-control have substantially different ε values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
