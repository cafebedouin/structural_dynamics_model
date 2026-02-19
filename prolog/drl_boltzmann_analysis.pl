% ============================================================================
% DRL BOLTZMANN ANALYSIS — Stages 5-7: Reformability, Purity Reform, Action Algebra
% Split from drl_modal_logic.pl (v5.0+)
% ============================================================================

:- module(drl_boltzmann_analysis, [
    % Stage 5: Boltzmann-Aware Analysis (v5.0)
    reformability_score/3,
    reformability_score/2,
    boltzmann_invariant_check/2,
    coupling_aware_scaffold_need/3,

    % Stage 6: Purity-Aware Reform Recommendations (v5.1)
    purity_reform_target/2,
    purity_reform_recommendation/2,

    % Stage 7: Purity-Qualified Action Algebra (v5.1)
    purity_qualified_action/4,
    purity_qualified_action/3,
    purity_adjusted_energy/4,
    action_composition_gate/3,
    purity_scaffold_urgency/4,
    qualify_action/5
]).

:- use_module(drl_core).
:- use_module(constraint_indexing).
:- use_module(narrative_ontology).
:- use_module(config).
:- use_module(boltzmann_compliance, [cross_index_coupling/2, excess_extraction/2, boltzmann_invariant_mountain/2]).
:- use_module(purity_scoring, [purity_score/2, factorization_subscore/2, scope_invariance_subscore/2, coupling_cleanliness_subscore/2, excess_extraction_subscore/2]).
:- use_module(drl_counterfactual).

/* ================================================================
   STAGE 5: BOLTZMANN-AWARE ANALYSIS (v5.0)

   Based on Tamuz & Sandomirskiy (2025), "On the origin of the
   Boltzmann distribution."

   Provides:
   1. Coupling-aware reformability score
   2. Boltzmann-invariant mountain check
   3. Coupling-aware scaffold need assessment

   These predicates integrate the Boltzmann compliance engine
   (structural_signatures.pl) with the modal logic layer,
   enabling more precise intervention planning.
   ================================================================ */

/* ----------------------------------------------------------------
   COUPLING-AWARE REFORMABILITY SCORE
   ----------------------------------------------------------------
   Extends the existing Prob_Reform_Success formula with coupling
   topology awareness.

   Reformability(C) = f(Separability, CouplingTopology, ExcessExtraction)

   Where:
     Separability = can coordination be separated from extraction?
     CouplingTopology = how entangled are the index dimensions?
     ExcessExtraction = how much extraction is above the Boltzmann floor?

   Strong coupling → low reformability (can't separate components)
   Weak coupling → moderate reformability
   Independent dimensions → high reformability (clean separation possible)

   Score range: [0.0, 1.0] where 1.0 = fully reformable
   ---------------------------------------------------------------- */

%% reformability_score(+Constraint, +Context, -Score)
%  PRIMARY API: Computes coupling-aware reformability from context.
reformability_score(C, Context, Score) :-
    constraint_indexing:valid_context(Context),

    % Factor 1: Separability — does the constraint have both
    % coordination AND extraction that can be disentangled?
    separability_factor(C, SepFactor),

    % Factor 2: Coupling topology — how entangled are the dimensions?
    coupling_factor(C, CouplingFactor),

    % Factor 3: Excess extraction — how much is above Boltzmann floor?
    excess_extraction_factor(C, ExcessFactor),

    % Weighted combination (coupling topology most important for reform).
    % Sensitivity analysis (728 constraints, 66 weight triples each):
    %   258 constraints are weight-insensitive (scores > 0.75 or < 0.35,
    %   far from gate thresholds). 1198 constraints in the 0.35-0.80
    %   range can cross the 0.50 or 0.60 gates under alternate weights.
    %   However, no test outcomes change because the gates are consumed
    %   only at line 830 (scaffold need) and line 1195 (surgical reform),
    %   where other conditions dominate the decision. The 40% weight on
    %   coupling reflects that coupling topology is the hardest factor
    %   to change during reform (structural, not behavioral).
    Score is min(1.0, max(0.0,
        0.30 * SepFactor +
        0.40 * CouplingFactor +
        0.30 * ExcessFactor
    )).

%% reformability_score(+Constraint, -Score)
%  BACKWARD COMPAT: Uses analytical context.
reformability_score(C, Score) :-
    constraint_indexing:default_context(Ctx),
    reformability_score(C, Ctx, Score).

%% separability_factor(+C, -Factor)
%  Measures whether coordination and extraction can be separated.
%  High factor = separable (good for reform).
separability_factor(C, 0.9) :-
    narrative_ontology:has_coordination_function(C),
    narrative_ontology:has_asymmetric_extraction(C),
    % Has both coordination and extraction — but are they in
    % different structural components?
    narrative_ontology:constraint_beneficiary(C, _),
    narrative_ontology:constraint_victim(C, _), !.
separability_factor(C, 0.3) :-
    % Only extraction, no coordination — nothing to preserve
    \+ narrative_ontology:has_coordination_function(C),
    narrative_ontology:has_asymmetric_extraction(C), !.
separability_factor(C, 1.0) :-
    % Only coordination, no extraction — already reformed
    narrative_ontology:has_coordination_function(C),
    \+ narrative_ontology:has_asymmetric_extraction(C), !.
separability_factor(_, 0.5).  % Unknown structure

%% coupling_factor(+C, -Factor)
%  Converts coupling topology into reformability factor.
%  Independent = easy to reform (high factor).
%  Strongly coupled = hard to reform (low factor).
coupling_factor(C, Factor) :-
    (   boltzmann_compliance:cross_index_coupling(C, CouplingScore)
    ->  % Invert: low coupling → high reformability
        Factor is max(0.0, 1.0 - CouplingScore)
    ;   Factor = 0.5  % Unknown coupling → moderate assumption
    ).

%% excess_extraction_factor(+C, -Factor)
%  Higher excess extraction → lower reformability (more entrenched).
%  But also → higher MOTIVATION to reform.
%  Smooth Gaussian inverted-U: moderate excess = highest reformability.
%  Factor = Floor + (Peak - Floor) * exp(-((Excess - Center)^2) / (2 * Sigma^2))
%  Parameters from config: excess_factor_center (sweet spot), excess_factor_sigma
%  (width), excess_factor_peak (max at center), excess_factor_floor (min at extremes).
%  Calibration: approximates old step values within ~0.1 at all boundary points
%  (e.g. Excess=0.20→1.0, Excess=0.50→0.49 vs old 0.6, Excess=0.60→0.35 vs old 0.3).
excess_extraction_factor(C, Factor) :-
    (   boltzmann_compliance:excess_extraction(C, Excess)
    ->  config:param(excess_factor_center, Center),
        config:param(excess_factor_sigma, Sigma),
        config:param(excess_factor_peak, Peak),
        config:param(excess_factor_floor, Floor),
        Factor is Floor + (Peak - Floor) * exp(-((Excess - Center)**2) / (2 * Sigma**2))
    ;   Factor = 0.5  % No data
    ).

/* ----------------------------------------------------------------
   BOLTZMANN-INVARIANT MOUNTAIN CHECK
   ----------------------------------------------------------------
   Delegates to boltzmann_compliance:boltzmann_invariant_mountain/2
   but adds modal context: checks if the Mountain classification
   is also consistent across counterfactual worlds.
   ---------------------------------------------------------------- */

%% boltzmann_invariant_check(+Constraint, -Result)
%  Full Boltzmann invariance check for Mountains.
%  Combines structural signature test with modal consistency.
%
%  Result = boltzmann_check(InvarianceResult, ModalConsistency)
boltzmann_invariant_check(C, boltzmann_check(InvResult, ModalConsistency)) :-
    % Structural invariance from signatures module
    boltzmann_compliance:boltzmann_invariant_mountain(C, InvResult),

    % Modal consistency: is it a Mountain from ALL standard contexts?
    findall(
        context_type(Ctx, Type),
        (   drl_counterfactual:standard_context(Ctx),
            drl_core:dr_type(C, Ctx, Type)
        ),
        ContextTypes
    ),
    findall(Type, member(context_type(_, Type), ContextTypes), Types),
    sort(Types, UniqueTypes),
    (   UniqueTypes = [mountain]
    ->  ModalConsistency = consistent(all_mountain)
    ;   member(mountain, UniqueTypes)
    ->  ModalConsistency = partial(UniqueTypes)
    ;   ModalConsistency = inconsistent(UniqueTypes)
    ).

/* ----------------------------------------------------------------
   COUPLING-AWARE SCAFFOLD NEED ASSESSMENT
   ----------------------------------------------------------------
   Extends assess_scaffold_need/3 with coupling awareness.
   A strongly-coupled constraint is HARDER to scaffold because
   cutting any part risks cascading through coupled dimensions.
   ---------------------------------------------------------------- */

%% coupling_aware_scaffold_need(+Constraint, +Context, -Assessment)
%  Enhanced scaffold assessment that considers coupling topology.
%
%  Assessment is one of:
%    no_scaffold_needed
%    scaffold_required(Urgency)     — Urgency in {low, moderate, high}
%    scaffold_present
%    reform_preferred(Score)         — Reformability score suggests reform over cut

coupling_aware_scaffold_need(C, Context, Assessment) :-
    constraint_indexing:valid_context(Context),
    drl_core:dr_type(C, Context, Type),

    % Only relevant for cuttable/reformable types
    (   member(Type, [snare, tangled_rope, piton, rope])
    ->  % Get base scaffold assessment
        drl_counterfactual:assess_scaffold_need(C, Context, BaseAssessment),

        % Get coupling topology
        (   boltzmann_compliance:cross_index_coupling(C, CouplingScore)
        ->  true
        ;   CouplingScore = 0.0
        ),

        % Get reformability
        reformability_score(C, Context, ReformScore),

        % Decision logic
        (   Type = tangled_rope, ReformScore > 0.60
        ->  Assessment = reform_preferred(ReformScore)
        ;   BaseAssessment = scaffold_required,
            CouplingScore > 0.30
        ->  Assessment = scaffold_required(high)
        ;   BaseAssessment = scaffold_required
        ->  Assessment = scaffold_required(moderate)
        ;   BaseAssessment = scaffold_present
        ->  Assessment = scaffold_present
        ;   Assessment = no_scaffold_needed
        )
    ;   Assessment = no_scaffold_needed
    ).

/* ================================================================
   STAGE 6: PURITY-AWARE REFORM RECOMMENDATIONS (v5.1)

   Now that purity is a scalar, we can give the action layer
   quantitative reform targets: "Reform until purity ≥ 0.85."

   This extends the existing reformability_score and reform_pressure
   with:
   1. purity_reform_target/2 — minimum acceptable purity after reform
   2. purity_reform_recommendation/2 — structured actionable advice

   The target formula:
     Target = max(current_purity, 0.85)
   Meaning:
     - If purity is 0.40 → reform to at least 0.85 ("sound")
     - If purity is 0.92 → maintain at 0.92 (don't regress)
     - 0.85 is the "sound" zone threshold from the purity scale

   The recommendation combines target, gap, reformability, pressure,
   and specific purity deficits into a single structured term for
   the decision/action layer.
   ================================================================ */

%% purity_reform_target(+Constraint, -Target)
%  Returns the minimum acceptable purity score after reform.
%  Target is always at least 0.85 ("sound" zone), or higher if the
%  constraint's current purity already exceeds 0.85.
purity_reform_target(C, Target) :-
    purity_scoring:purity_score(C, Purity),
    Purity >= 0.0, !,
    Target is max(Purity, 0.85).
purity_reform_target(_, 0.85).  % Fallback if purity inconclusive

%% purity_reform_recommendation(+Constraint, -Recommendation)
%  Generates a structured reform recommendation combining purity
%  target, reformability, pressure, and specific deficits.
%
%  Recommendation = reform_recommendation(
%      CurrentPurity,      % Current purity score [0,1]
%      TargetPurity,       % Target purity score [0.85, 1.0]
%      PurityGap,          % How much improvement needed
%      Reformability,      % Can this be reformed? [0,1]
%      ReformPressure,     % How urgent is reform? [0, 99]
%      Deficits,           % List of specific purity deficits
%      Urgency             % none | low | moderate | high | critical
%  )

purity_reform_recommendation(C, reform_recommendation(
        CurrentPurity, TargetPurity, PurityGap,
        Reformability, Pressure, Deficits, Urgency)) :-
    % Get current purity
    purity_scoring:purity_score(C, CurrentPurity),
    CurrentPurity >= 0.0,

    % Compute target and gap
    purity_reform_target(C, TargetPurity),
    PurityGap is max(0.0, TargetPurity - CurrentPurity),

    % Get reformability score
    (   reformability_score(C, Reformability)
    ->  true
    ;   Reformability = 0.5
    ),

    % Get reform pressure from lifecycle module
    (   drl_lifecycle:reform_pressure(C, Pressure)
    ->  true
    ;   Pressure = 0.0
    ),

    % Identify specific purity deficits
    identify_purity_deficits(C, Deficits),

    % Compute urgency from gap, pressure, and reformability
    compute_reform_urgency(PurityGap, Pressure, Reformability, Urgency).

%% identify_purity_deficits(+C, -Deficits)
%  Returns list of specific purity components scoring below 0.85.
%  Each deficit identifies which structural aspect needs improvement.
identify_purity_deficits(C, Deficits) :-
    findall(Deficit, purity_deficit(C, Deficit), Deficits).

% Factorization deficit: coupling across index dimensions
purity_deficit(C, factorization_deficit(Score)) :-
    purity_scoring:factorization_subscore(C, Score),
    Score < 0.85.

% Scope invariance deficit: classification changes with scope
purity_deficit(C, scope_invariance_deficit(Score)) :-
    purity_scoring:scope_invariance_subscore(C, Score),
    Score < 0.85.

% Coupling cleanliness deficit: nonsensical coupling present
purity_deficit(C, coupling_cleanliness_deficit(Score)) :-
    purity_scoring:coupling_cleanliness_subscore(C, Score),
    Score < 0.85.

% Excess extraction deficit: extraction above Boltzmann floor
purity_deficit(C, excess_extraction_deficit(Score)) :-
    purity_scoring:excess_extraction_subscore(C, Score),
    Score < 0.85.

%% compute_reform_urgency(+Gap, +Pressure, +Reformability, -Urgency)
%  Maps quantitative inputs to categorical urgency level.
%  Two-phase: raw urgency from gap/pressure thresholds (config-driven),
%  then modulation by reformability — if Reformability =< floor,
%  urgency is downgraded one level (can't rush reform on entrenched
%  constraints). Urgency is informational only (packed into
%  reform_recommendation/7, not consumed for control flow).
compute_reform_urgency(Gap, Pressure, Reformability, Urgency) :-
    raw_urgency(Gap, Pressure, Reformability, RawUrgency),
    config:param(reform_urgency_reformability_floor, ReformFloor),
    (   Reformability =< ReformFloor,
        member(RawUrgency, [critical, high, moderate])
    ->  downgrade_urgency(RawUrgency, Urgency)
    ;   Urgency = RawUrgency
    ).

%% raw_urgency(+Gap, +Pressure, +Reformability, -Urgency)
%  Threshold-based urgency classification. Thresholds from config.pl.
raw_urgency(Gap, Pressure, _, critical) :-
    config:param(reform_urgency_gap_critical, GapCrit),
    config:param(reform_urgency_pressure_critical, PresCrit),
    Gap > GapCrit, Pressure > PresCrit, !.
raw_urgency(Gap, Pressure, _, high) :-
    config:param(reform_urgency_gap_high, GapHigh),
    config:param(reform_urgency_pressure_high, PresHigh),
    (Gap > GapHigh ; Pressure > PresHigh), !.
raw_urgency(Gap, _, Reformability, moderate) :-
    config:param(reform_urgency_gap_moderate, GapMod),
    Gap > GapMod, Reformability > 0.50, !.
raw_urgency(Gap, _, _, low) :-
    config:param(reform_urgency_gap_low, GapLow),
    Gap > GapLow, !.
raw_urgency(_, _, _, none).

%% downgrade_urgency(+Level, -Downgraded)
%  Reduces urgency one level for low-reformability constraints.
downgrade_urgency(critical, high).
downgrade_urgency(high, moderate).
downgrade_urgency(moderate, low).

/* ================================================================
   STAGE 7: PURITY-QUALIFIED ACTION ALGEBRA (v5.1)

   The action algebra (dr_action/3 in drl_core.pl) is a simple
   6-way type→action switch with no awareness of structural purity.
   This creates a gap: the system can detect that a Rope's
   coordination has been contaminated (purity=0.35) but still
   recommends 'maintain'.

   Stage 7 wraps dr_action/3 with a purity-qualified layer that
   upgrades/downgrades actions based on purity thresholds.

   SHADOW MODE: drl_core.pl is NOT modified. These predicates
   are additive — callers opt in by using purity_qualified_action
   instead of dr_action.

   Four predicates:
   A. purity_qualified_action/4  — Core action qualifier
   B. purity_adjusted_energy/4   — Energy cost with purity multiplier
   C. action_composition_gate/3  — Purity prerequisites for composites
   D. purity_scaffold_urgency/4  — Extends scaffold assessment
   ================================================================ */

/* ----------------------------------------------------------------
   A. PURITY-QUALIFIED ACTION — Core Action Qualifier
   ----------------------------------------------------------------
   Returns qualified_action(BaseAction, Qualifier, Priority) where:
     Qualifier ∈ {stable, monitor, escalate_reform, escalate_cut,
                  accelerate_sunset, degraded, inconclusive}
     Priority  ∈ {none, low, moderate, high, critical}

   When purity = -1.0 (insufficient data): returns
     qualified_action(BaseAction, inconclusive, none)
   — no qualification without evidence.
   ---------------------------------------------------------------- */

%% purity_qualified_action(+C, +Context, -QAction, -Rationale)
%  PRIMARY API: Qualifies the base action with purity awareness.
%  Rationale is a human-readable atom explaining the qualification.
purity_qualified_action(C, Context, QAction, Rationale) :-
    constraint_indexing:valid_context(Context),
    drl_core:dr_action(C, Context, BaseAction),
    purity_scoring:purity_score(C, Purity),
    qualify_action(BaseAction, Purity, C, QAction, Rationale).

%% purity_qualified_action(+C, -QAction, -Rationale)
%  BACKWARD COMPAT: Uses analytical context.
purity_qualified_action(C, QAction, Rationale) :-
    constraint_indexing:default_context(Ctx),
    purity_qualified_action(C, Ctx, QAction, Rationale).

%% qualify_action(+BaseAction, +Purity, +C, -QAction, -Rationale)
%  Core qualification logic. Dispatches by base action and purity zone.

% Inconclusive purity — no qualification without evidence
qualify_action(BaseAction, Purity, _C, qualified_action(BaseAction, inconclusive, none),
              insufficient_purity_data) :-
    Purity < 0.0, !.

% accept (Mountain) — natural laws need no purity gate
qualify_action(accept, _Purity, _C, qualified_action(accept, stable, none),
              natural_law_no_gate) :- !.

% maintain (Rope) — purity determines coordination health
qualify_action(maintain, Purity, _C, qualified_action(maintain, stable, low),
              sound_coordination) :-
    config:param(purity_action_sound_floor, SoundFloor),
    Purity >= SoundFloor, !.
qualify_action(maintain, Purity, _C, qualified_action(maintain, monitor, moderate),
              purity_declining) :-
    config:param(purity_action_escalation_floor, EscFloor),
    Purity >= EscFloor, !.
qualify_action(maintain, Purity, _C, qualified_action(maintain, escalate_reform, high),
              coordination_contaminated) :-
    config:param(purity_action_degraded_floor, DegFloor),
    Purity >= DegFloor, !.
qualify_action(maintain, _Purity, _C, qualified_action(maintain, escalate_cut, critical),
              degraded_beyond_coordination_value) :- !.

% reform (Tangled Rope) — purity determines reform feasibility
qualify_action(reform, Purity, _C, qualified_action(reform, stable, low),
              careful_reform_sufficient) :-
    config:param(purity_action_sound_floor, SoundFloor),
    Purity >= SoundFloor, !.
qualify_action(reform, Purity, _C, qualified_action(reform, stable, moderate),
              standard_surgical_reform) :-
    config:param(purity_action_escalation_floor, EscFloor),
    Purity >= EscFloor, !.
qualify_action(reform, Purity, _C, qualified_action(reform, stable, high),
              aggressive_reform_needed) :-
    config:param(purity_action_degraded_floor, DegFloor),
    Purity >= DegFloor, !.
qualify_action(reform, _Purity, _C, qualified_action(reform, escalate_cut, critical),
              unreformable_cut) :- !.

% cut (Snare) — purity informs energy only; cut unchanged
qualify_action(cut, Purity, _C, qualified_action(cut, stable, Priority),
              cut_purity_informs_energy) :-
    purity_to_cut_priority(Purity, Priority), !.

% monitor_sunset (Scaffold) — purity determines scaffold health
qualify_action(monitor_sunset, Purity, _C, qualified_action(monitor_sunset, stable, low),
              scaffold_healthy) :-
    config:param(purity_action_sound_floor, SoundFloor),
    Purity >= SoundFloor, !.
qualify_action(monitor_sunset, Purity, _C, qualified_action(monitor_sunset, monitor, moderate),
              scaffold_purity_declining) :-
    config:param(purity_scaffold_health_gate, HealthGate),
    Purity >= HealthGate, !.
qualify_action(monitor_sunset, _Purity, _C, qualified_action(monitor_sunset, accelerate_sunset, high),
              dissolve_sooner) :- !.

% bypass (Piton) — terminal state
qualify_action(bypass, _Purity, _C, qualified_action(bypass, degraded, none),
              terminal_state) :- !.

% investigate_opacity — passthrough (indexically opaque: consent dimension unresolvable)
qualify_action(investigate_opacity, _Purity, _C, qualified_action(investigate_opacity, stable, none),
              passthrough) :- !.

% investigate — passthrough
qualify_action(investigate, _Purity, _C, qualified_action(investigate, stable, none),
              passthrough) :- !.

% Fallback
qualify_action(BaseAction, _Purity, _C, qualified_action(BaseAction, stable, none),
              no_qualification_rule).

%% purity_to_cut_priority(+Purity, -Priority)
%  Maps purity to priority for cut actions (energy-only impact).
purity_to_cut_priority(Purity, low) :-
    config:param(purity_action_sound_floor, SoundFloor),
    Purity >= SoundFloor, !.
purity_to_cut_priority(Purity, moderate) :-
    config:param(purity_action_escalation_floor, EscFloor),
    Purity >= EscFloor, !.
purity_to_cut_priority(Purity, high) :-
    config:param(purity_action_degraded_floor, DegFloor),
    Purity >= DegFloor, !.
purity_to_cut_priority(_, critical).

/* ----------------------------------------------------------------
   B. PURITY-ADJUSTED ENERGY — Energy Cost with Purity Multiplier
   ----------------------------------------------------------------
   Returns energy_cost(BaseComplexity, Multiplier, AdjustedComplexity).

   Multiplier formulas per action:
     reform:        M = min(MaxMult, 1.0 + max(0, 0.70 - Purity) × 3.0)
     cut:           M = min(MaxMult, 1.0 + max(0, 0.50 - Purity) × 1.5)
     maintain:      M = max(0.8, 1.0 - max(0, Purity - 0.70) × 0.5)
     accept/bypass/monitor_sunset: M = 1.0

   Base complexities:
     accept=0, maintain=1, reform=4, cut=2, monitor_sunset=2, bypass=1
   ---------------------------------------------------------------- */

%% purity_adjusted_energy(+C, +Context, +BaseAction, -EnergyCost)
%  PRIMARY API: Computes purity-adjusted energy cost for an action.
purity_adjusted_energy(C, Context, BaseAction, energy_cost(BaseCost, Mult, Adjusted)) :-
    constraint_indexing:valid_context(Context),
    base_action_complexity(BaseAction, BaseCost),
    purity_scoring:purity_score(C, Purity),
    config:param(purity_energy_max_multiplier, MaxMult),
    energy_multiplier(BaseAction, Purity, MaxMult, Mult),
    Adjusted is BaseCost * Mult.

%% base_action_complexity(+Action, -Cost)
base_action_complexity(accept,         0).
base_action_complexity(maintain,       1).
base_action_complexity(reform,         4).
base_action_complexity(cut,            2).
base_action_complexity(monitor_sunset, 2).
base_action_complexity(bypass,         1).
base_action_complexity(investigate_opacity, 1).
base_action_complexity(investigate,    1).

%% energy_multiplier(+Action, +Purity, +MaxMult, -Mult)
%  Computes the energy multiplier based on action type and purity.

% Inconclusive purity — use baseline multiplier
energy_multiplier(_, Purity, _, 1.0) :-
    Purity < 0.0, !.

% Reform: harder at lower purity (more entangled)
energy_multiplier(reform, Purity, MaxMult, Mult) :-
    RawMult is 1.0 + max(0.0, 0.70 - Purity) * 3.0,
    Mult is min(MaxMult, RawMult), !.

% Cut: harder at lower purity (more collateral damage)
energy_multiplier(cut, Purity, MaxMult, Mult) :-
    RawMult is 1.0 + max(0.0, 0.50 - Purity) * 1.5,
    Mult is min(MaxMult, RawMult), !.

% Maintain: pristine coordination is cheaper to maintain
energy_multiplier(maintain, Purity, _, Mult) :-
    Mult is max(0.8, 1.0 - max(0.0, Purity - 0.70) * 0.5), !.

% All others: no purity effect
energy_multiplier(_, _, _, 1.0).

/* ----------------------------------------------------------------
   C. ACTION COMPOSITION GATE — Purity Prerequisites
   ----------------------------------------------------------------
   Returns gate(Pass, Reason) where Pass ∈ {pass, fail}.

   Gates:
     surgical_reform:        Purity ≥ 0.30 AND Reformability > 0.50
     safe_transition:        Scaffold purity ≥ 0.50
     efficient_coordination: Rope purity ≥ 0.50

   For unknown composite actions or purity = -1.0 →
     gate(pass, no_gate_defined)
   ---------------------------------------------------------------- */

%% action_composition_gate(+C, +CompositeAction, -GateResult)
%  Checks purity prerequisites for composite actions.

action_composition_gate(C, surgical_reform, gate(Pass, Reason)) :-
    purity_scoring:purity_score(C, Purity),
    (   Purity < 0.0
    ->  Pass = pass, Reason = no_gate_defined
    ;   config:param(purity_surgical_reform_gate, MinPurity),
        (   Purity >= MinPurity
        ->  (   reformability_score(C, ReformScore),
                ReformScore > 0.50
            ->  Pass = pass, Reason = purity_and_reformability_sufficient
            ;   Pass = fail, Reason = reformability_too_low
            )
        ;   Pass = fail, Reason = purity_below_surgical_threshold
        )
    ), !.

action_composition_gate(C, safe_transition, gate(Pass, Reason)) :-
    purity_scoring:purity_score(C, Purity),
    (   Purity < 0.0
    ->  Pass = pass, Reason = no_gate_defined
    ;   config:param(purity_scaffold_health_gate, MinPurity),
        (   Purity >= MinPurity
        ->  Pass = pass, Reason = scaffold_structurally_sound
        ;   Pass = fail, Reason = scaffold_purity_insufficient
        )
    ), !.

action_composition_gate(C, efficient_coordination, gate(Pass, Reason)) :-
    purity_scoring:purity_score(C, Purity),
    (   Purity < 0.0
    ->  Pass = pass, Reason = no_gate_defined
    ;   config:param(purity_action_escalation_floor, MinPurity),
        (   Purity >= MinPurity
        ->  Pass = pass, Reason = coordination_structurally_sound
        ;   Pass = fail, Reason = coordination_purity_insufficient
        )
    ), !.

% Unknown composite action or fallback
action_composition_gate(_, _, gate(pass, no_gate_defined)).

/* ----------------------------------------------------------------
   D. PURITY SCAFFOLD URGENCY — Extends Scaffold Assessment
   ----------------------------------------------------------------
   Returns urgency ∈ {none, low, moderate, high, critical}
   with explanatory factors.

   Combines coupling_aware_scaffold_need/3 output with purity:
     - Base scaffold_required(high) + purity < 0.30 → critical
     - Base scaffold_required(_) + purity drift → escalate one level
     - Base reform_preferred(Score) + purity < 0.50 →
       escalate to scaffold_required(high) (reform impractical)
   ---------------------------------------------------------------- */

%% purity_scaffold_urgency(+C, +Context, -Urgency, -Factors)
%  PRIMARY API: Purity-enhanced scaffold urgency assessment.
purity_scaffold_urgency(C, Context, Urgency, Factors) :-
    constraint_indexing:valid_context(Context),
    (   coupling_aware_scaffold_need(C, Context, BaseAssessment)
    ->  true
    ;   BaseAssessment = no_scaffold_needed
    ),
    purity_scoring:purity_score(C, Purity),
    has_purity_drift(C, DriftDetected),
    compute_scaffold_urgency(BaseAssessment, Purity, DriftDetected, Urgency, Factors).

%% has_purity_drift(+C, -Detected)
%  Checks if purity drift is detected for the constraint.
%  Fails gracefully if drl_lifecycle is not loaded.
has_purity_drift(C, true) :-
    catch(drl_lifecycle:detect_purity_drift(C), _, fail), !.
has_purity_drift(_, false).

%% compute_scaffold_urgency(+BaseAssessment, +Purity, +DriftDetected, -Urgency, -Factors)

% Inconclusive purity — pass through base assessment
compute_scaffold_urgency(BaseAssessment, Purity, _, BaseUrgency, [base(BaseAssessment), purity_inconclusive]) :-
    Purity < 0.0, !,
    base_assessment_urgency(BaseAssessment, BaseUrgency).

% scaffold_required(high) + purity < 0.30 → critical
compute_scaffold_urgency(scaffold_required(high), Purity, _, critical,
                         [base(scaffold_required(high)), purity(Purity), escalated(degraded_purity)]) :-
    config:param(purity_action_degraded_floor, DegFloor),
    Purity < DegFloor, !.

% scaffold_required(_) + drift → escalate one level
compute_scaffold_urgency(scaffold_required(BaseLevel), Purity, true, Escalated,
                         [base(scaffold_required(BaseLevel)), purity(Purity), escalated(purity_drift)]) :-
    escalate_urgency(BaseLevel, Escalated), !.

% reform_preferred(Score) + purity < 0.50 → scaffold_required(high)
compute_scaffold_urgency(reform_preferred(_Score), Purity, _, high,
                         [base(reform_preferred), purity(Purity), escalated(reform_impractical_at_purity)]) :-
    config:param(purity_action_escalation_floor, EscFloor),
    Purity < EscFloor, !.

% Default: derive urgency from base assessment without escalation
compute_scaffold_urgency(BaseAssessment, Purity, _, Urgency,
                         [base(BaseAssessment), purity(Purity)]) :-
    base_assessment_urgency(BaseAssessment, Urgency).

%% base_assessment_urgency(+Assessment, -Urgency)
%  Maps base coupling_aware_scaffold_need assessment to urgency.
base_assessment_urgency(scaffold_required(high), high).
base_assessment_urgency(scaffold_required(moderate), moderate).
base_assessment_urgency(scaffold_required(low), low).
base_assessment_urgency(scaffold_required(_), moderate).  % catch-all for scaffold_required
base_assessment_urgency(scaffold_present, low).
base_assessment_urgency(reform_preferred(_), moderate).
base_assessment_urgency(no_scaffold_needed, none).
base_assessment_urgency(_, none).

%% escalate_urgency(+Base, -Escalated)
%  Escalates urgency by one level.
escalate_urgency(low, moderate).
escalate_urgency(moderate, high).
escalate_urgency(high, critical).
escalate_urgency(critical, critical).  % Cannot escalate beyond critical
escalate_urgency(none, low).
