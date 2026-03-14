% ============================================================================
% CONSTRAINT STORY: venezuela_regime_survival
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_venezuela_regime_survival, []).

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
 *   constraint_id: venezuela_regime_survival
 *   human_readable: Venezuela Regime Survival Constraint
 *   domain: political_economy/state_capacity
 *
 * SUMMARY:
 *   The Venezuelan regime survival constraint operates as a pure extraction
 *   mechanism enforced through military coercion, state monopolies, and
 *   systematic suppression of alternative power centers. The Maduro regime
 *   (2013-present) maintains control through monopoly control of food
 *   distribution, currency arbitrage, gold exports, and military command
 *   loyalty. The constraint exhibits the structural characteristics of a
 *   Snare: high extractiveness (0.68), high suppression (0.75), and
 *   systematic elimination of exit options for civilian and opposition
 *   populations. The extractiveness has accumulated over time (rising from
 *   0.42 to 0.68 over the decade) as the regime has intensified resource
 *   concentration. The theater ratio (0.68) reflects that elections, courts,
 *   and legislative bodies persist as purely performative institutions — they
 *   provide international legitimacy theater while all real power resides in
 *   military command and inner circle coordination. The constraint is
 *   sustained through coercive capacity (military loyalty maintained by
 *   resource distribution) rather than coordination benefit — there is no
 *   genuine coordination function for victims, only coordinated suppression.
 *
 * KEY AGENTS:
 *   - Regime Inner Circle (institutional/constrained): Primary beneficiary — controls state resource flows, international arbitrage, and accumulates wealth through monopoly position. Benefits from constraint but constrained by international pressure and internal factional dynamics.
 *   - Military Command Structure (institutional/arbitrage): Primary beneficiary — receives privileged access to food, fuel, currency arbitrage, and command authority. Arbitrage position allows defection if compensation drops below threshold.
 *   - Civilian Population (powerless/trapped): Primary victim — faces hyperinflation (currency debasement extraction), price controls (goods unavailability extraction), and forced labor in state enterprises. No exit capacity — international relocation requires resources most lack.
 *   - Political Opposition (moderate/constrained): Secondary victim — imprisoned, exiled, assassinated, and excluded from electoral competition. Faces extraction through imprisonment costs and political representation denial.
 *   - Economic Institutions (institutional/constrained): Victim of systematic extraction through state seizure, currency controls, and forced resource allocation. Cannot exit national economy.
 *   - Regional Coalition (organized/constrained): Organized neighbors and diaspora face extraction through refugee flows, cross-border violence, and pressure to maintain sanctions. Constrained by geopolitical stability concerns.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(venezuela_regime_survival, 0.68).
domain_priors:suppression_score(venezuela_regime_survival, 0.75).
domain_priors:theater_ratio(venezuela_regime_survival, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(venezuela_regime_survival, extractiveness, 0.68).
narrative_ontology:constraint_metric(venezuela_regime_survival, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(venezuela_regime_survival, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(venezuela_regime_survival, snare).
narrative_ontology:human_readable(venezuela_regime_survival, "Venezuela Regime Survival Constraint").
narrative_ontology:topic_domain(venezuela_regime_survival, "political_economy/state_capacity").

domain_priors:requires_active_enforcement(venezuela_regime_survival).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(venezuela_regime_survival, regime_inner_circle).
narrative_ontology:constraint_beneficiary(venezuela_regime_survival, military_command_structure).
narrative_ontology:constraint_victim(venezuela_regime_survival, civilian_population).
narrative_ontology:constraint_victim(venezuela_regime_survival, political_opposition).
narrative_ontology:constraint_victim(venezuela_regime_survival, economic_institutions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CIVILIAN POPULATION (SNARE) — Trapped by hyperinflation, border restrictions, and economic collapse. Exit requires international relocation or defection which most lack resources for. Bears extraction through currency debasement, price controls that create shortages, and forced labor participation in state enterprises. No coordination benefit — extraction is pure.
constraint_indexing:constraint_classification(venezuela_regime_survival, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: POLITICAL OPPOSITION (SNARE) — Constrained by imprisonment, harassment, exile risk, and assassination. Can attempt mobilization but faces military suppression and electoral fraud mechanisms. Extraction is severe — opposition leaders imprisoned or exiled without trial. Minimal coordination function — the regime permits no genuine political process.
constraint_indexing:constraint_classification(venezuela_regime_survival, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MILITARY COMMAND STRUCTURE (ROPE) — Benefits from control of food distribution, fuel allocation, and currency arbitrage. Extraction runs toward this group. Experiences the regime as coordination mechanism: military hierarchy distributes resources, maintains order (for beneficiaries), and sustains institutional stability. Low experienced extraction — clear alignment with regime interests.
constraint_indexing:constraint_classification(venezuela_regime_survival, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REGIME INNER CIRCLE (TANGLED ROPE) — Coordinates state security apparatus and loyalty networks (genuine coordination function) while extracting through monopoly control of state enterprises, international sanctions circumvention, and gold/oil revenue capture. Experiences both coordination benefit (survival through unified command) and extraction vulnerability (cannot exit without losing power). Extraction is moderate relative to military — constrained by internal power competition and international pressure.
constraint_indexing:constraint_classification(venezuela_regime_survival, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ELECTORAL AND JUDICIAL THEATER (PITON) — Elections and courts exist but are purely performative. Electoral fraud, gerrymandering, and disqualification of opposition candidates are systematic. Courts impose sentences on political prisoners without legitimate judicial process. Theater ratio (0.68) reflects that these institutions are maintained through inertia — they provide legitimacy theater rather than functional governance. The regime could abandon them entirely but maintains the ritual for international and domestic psychological effect.
constraint_indexing:constraint_classification(venezuela_regime_survival, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: REGIONAL OPPOSITION COALITION (SNARE) — Organized neighbors (Colombia, Brazil) and diaspora provide coordination on sanctions, asylum, and information but face extraction through refugee flows, cross-border violence, and resource competition. Exit constrained by geopolitical stability concerns. Extraction significant but not total — coalition members retain independent action capacity.
constraint_indexing:constraint_classification(venezuela_regime_survival, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (PITON) — From civilizational scope, regime survival via coercion appears inevitable: autocracies always extract through military control and suppress opposition. This perspective naturalizes contingent institutional arrangements (specific policies, command structures, resource allocation mechanisms) as immutable features of authoritarian governance. Theater ratio high — much of the observation is projection of theoretical inevitability rather than structural analysis.
constraint_indexing:constraint_classification(venezuela_regime_survival, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(venezuela_regime_survival_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(venezuela_regime_survival, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(venezuela_regime_survival, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(venezuela_regime_survival, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(venezuela_regime_survival, TR),
    TR >= 0.70.

:- end_tests(venezuela_regime_survival_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and rising over the interval. The regime captures resources through multiple mechanisms: (1) currency debasement — printing bolivares to fund regime operations extracts purchasing power from civilians; (2) state enterprise monopolies — oil, gold, food distribution controlled by regime loyalists extract rents through monopoly pricing and diversion; (3) international sanctions circumvention — regime captures arbitrage spreads through black market currency exchange and contraband food imports. The measurement trajectory (0.42 → 0.55 → 0.68) reflects acceleration of extraction as economic collapse has forced tighter regime control. Suppression (0.75): High and stable. Barriers to exit include: (1) border restrictions — civilians cannot leave without regime permission and military checkpoints; (2) military enforcement — opposition mobilization met with imprisonment, torture, and assassination; (3) information control — censorship, internet restrictions, and state media monopoly prevent coordination; (4) economic trap — hyperinflation makes international relocation unaffordable for most. Theater ratio (0.68): High. Elections (gerrymandered, fraudulent), courts (regime-controlled), and parliament (opposition disqualified) are maintained as legitimacy rituals but exercise no real governance function. The theater has increased over time as economic collapse has made real governance capacity impossible — the regime substitutes theatrical institutions for functional ones.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits dramatic perspectival divergence. For the inner circle and military command, the regime appears as Rope or Tangled Rope — a coordination mechanism that aligns incentives and distributes benefits. For the civilian population, it appears as pure Snare — coercive extraction with no coordination benefit. For the regional opposition, it appears as constrained Snare — organized but unable to act decisively. For the analytical observer, there is strong temptation to see this as an inevitable feature of late-stage authoritarianism (Piton perspective naturalized as natural law). The perspectival gap reveals that classification depends entirely on structural position: beneficiaries see coordination, victims see extraction, observers risk naturalizing contingent arrangements.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality (d) for each perspective is derived from their structural relationship to extraction flow. Beneficiaries (military, inner circle) with exit options to arbitrage experience low d (extraction runs toward them). Victims (civilians, opposition) with exit options of trapped or constrained experience high d (extraction runs away from them). The regime's institutional power is constrained by international pressure and military loyalty thresholds, but within the national boundary, extracted resources concentrate toward military command and inner circle. The civilian population has no arbitrage exit — cannot reallocate resources or reorganize production. The opposition has constrained exit — imprisonment and assassination are permanent extraction forms that prevent exit. Directionality overrides are not necessary — the structural derivation captures the reality that the military command structure benefits and the civilian population bears costs.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating why Snare dominates from most perspectives. The snare gate requires: extractiveness ≥ 0.46 (✓ 0.68), suppression ≥ 0.60 (✓ 0.75), χ ≥ 0.66 (compute: ε=0.68 × f(d) for powerless/trapped agent ≈ 0.68 × 1.42 ≈ 0.96, scope σ(national)=1.0, so χ ≈ 0.96 ✓). The snare classification prevents mislabeling this as coordination (Rope) or mixed governance (Tangled Rope) from the civilian perspective — the suppression level (0.75) and trapped exit (no arbitrage, no mobility, no analytical distance) combine to produce maximum experienced extraction. The beneficiary perspectives (Rope for military, Tangled Rope for inner circle) show why snare extraction is possible — the beneficiaries genuinely experience coordination benefit and stable extraction flows. The false mountain perspective (analytical/natural law) is exposed by the theater gate: if this were an immutable feature of governance, the theater ratio would be low (performative activity approaches zero when something is truly necessary). High theater (0.68) indicates that elections, courts, and legislatures are maintained for appearance, not function — a diagnostic signal that the regime is contingent, not natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    military_defection_threshold,
    'At what point does military extraction become so constraining that defection becomes rational for mid-level commanders?',
    'Historical analysis of military loyalty in late-stage autocracies (Nicaragua 1989, Philippines 1986, Tunisia 2011); correlation between officer compensation compression and regime change.',
    'If threshold < current extraction level: regime vulnerable to military coup or mass defection. If threshold > current level: military loyalty sustainable despite economic deterioration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(military_defection_threshold, empirical, 'Military defection threshold under extraction pressure').

omega_variable(
    external_intervention_trigger,
    'Would sustained international military intervention (blockade, air support for opposition) shift the constraint from Snare to Tangled Rope by creating genuine negotiation space?',
    'Comparison with cases of external intervention: Syria (Russia intervention stabilized), Libya (NATO intervention destabilized), Nicaragua (Cold War pressure varied), Yemen (Saudi intervention prolonged conflict).',
    'If intervention degrades regime extraction: Snare weakens toward Tangled Rope with negotiation function. If intervention strengthens regime resistance: Snare deepens and extractiveness rises.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(external_intervention_trigger, preference, 'Whether external intervention enables regime negotiation').

omega_variable(
    economic_collapse_bifurcation,
    'Does economic collapse trigger regime rigidity (increased suppression, tighter coercion) or regime fragmentation (factional breakdown in inner circle)?',
    'Longitudinal tracking of suppression metrics vs fragmentation indicators (purges, military leadership turnover, coordination failures). Comparison with Soviet Union late stages (fragmentation) vs North Korea (rigidity).',
    'If rigidity: extractiveness increases, suppression approaches 1.0, regime becomes less sustainable. If fragmentation: inner circle becomes victim of extraction, Tangled Rope perspective becomes dominant, negotiation pathways emerge.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_collapse_bifurcation, empirical, 'Whether economic collapse produces regime rigidity or fragmentation').

omega_variable(
    diaspora_coordination_effectiveness,
    'Can diaspora networks and remittance control become alternative power structures that create competing extraction mechanisms?',
    'Analysis of diaspora communication channels, remittance flow restrictions, and underground economy scaling. Tracking of informal institutions created by diaspora (shadow banking, information networks).',
    'If effective: creates second-order constraint (regime vs diaspora) that fragments the primary Snare. If ineffective: regime maintains monopoly on extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diaspora_coordination_effectiveness, empirical, 'Whether diaspora networks can establish competing extraction structures').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(venezuela_regime_survival, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vrs_tr_t0, venezuela_regime_survival, theater_ratio, 0, 0.45).
narrative_ontology:measurement(vrs_tr_t5, venezuela_regime_survival, theater_ratio, 5, 0.58).
narrative_ontology:measurement(vrs_tr_t10, venezuela_regime_survival, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(vrs_be_t0, venezuela_regime_survival, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(vrs_be_t5, venezuela_regime_survival, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(vrs_be_t10, venezuela_regime_survival, base_extractiveness, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(venezuela_regime_survival, enforcement_mechanism).
narrative_ontology:affects_constraint(venezuela_regime_survival, latin_american_state_capacity).
narrative_ontology:affects_constraint(venezuela_regime_survival, oil_curse_resource_extraction).

% DUAL FORMULATION NOTE:
% The regime survival constraint is downstream of oil-revenue dependence (which creates resource-curse extraction) and upstream of Latin American regional stability (which propagates to migration, drug trafficking, and geopolitical competition). The three constraints form a family: oil_curse → venezuela_regime_survival → regional_destabilization. Each has distinct ε values reflecting empirical status: oil curse (ε=0.55, Tangled Rope), regime survival (ε=0.68, Snare), regional instability (ε=0.52, Tangled Rope).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
