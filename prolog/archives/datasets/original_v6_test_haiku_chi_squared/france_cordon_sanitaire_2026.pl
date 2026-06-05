% ============================================================================
% CONSTRAINT STORY: france_cordon_sanitaire_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_france_cordon_sanitaire_2026, []).

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
 *   constraint_id: france_cordon_sanitaire_2026
 *   human_readable: The 'Front Républicain' (Republican Front) Cordon Sanitaire
 *   domain: political/social
 *
 * SUMMARY:
 *   The 'Front Républicain' (Republican Front) cordon sanitaire was
 *   established as a coordination mechanism to prevent far-right electoral
 *   breakthrough through tactical voting by mainstream parties and civic
 *   mobilization. From 2002-2017, it functioned as genuine coordination:
 *   parties voluntarily withdrew in runoff ballots to prevent far-right
 *   candidates from advancing, and voters accepted tactical voting as a
 *   democratic defense. By 2026, the mechanism has transitioned into a Piton
 *   state—institutional inertia persists the constraint through performative
 *   enforcement (media consensus, party coordination rituals, elite
 *   statements) while its actual functional capacity to prevent far-right
 *   gains has degraded significantly. The cordon now extracts from political
 *   competition (suppressing alternative coalitions and policy diversity)
 *   without providing the coordination benefit it once offered. Theater ratio
 *   has risen from 0.35 (early 2000s: genuine coordination among authentic
 *   alternatives) to 0.78 (2026: performative ritual), while base
 *   extractiveness has fallen from 0.38 (when exclusion cost mattered to the
 *   far-right party) to 0.22 (when electoral growth is no longer suppressed
 *   by cordon mechanisms alone). The constraint exemplifies how coordination
 *   mechanisms degrade into theatrical maintenance when the structural
 *   conditions that justified their existence transform.
 *
 * KEY AGENTS:
 *   - Centrist Coalition Parties (PS, Renaissance, Republicans): Primary beneficiary (institutional/arbitrage) — maintains vote consolidation and agenda-setting power through cordon; benefits from moral framing as democratic defenders
 *   - Political Establishment & Media: Secondary beneficiary (institutional/arbitrage) — cordon provides narrative coherence and gate-keeping legitimacy
 *   - Marginalized Voters (supporters of cordon-blocked party): Primary victim (powerless/trapped) — face formal exclusion from coalition arithmetic despite electoral growth; theater ratio high (0.78) ensures exclusion is framed as civic duty rather than suppression
 *   - Regional Political Entrepreneurs: Secondary victim (moderate/constrained) — constrained by cordon framework preventing independent movement-building; career advancement requires accepting establishment coalition logic
 *   - Far-Right Party: Constrained beneficiary (powerful/mobile) — experiences extraction (vote-splitting via cordon coordination) but benefits from cordon's contradictory effect (mobilization against cordon increases engagement and frames establishment as anti-democratic)
 *   - Civic Opposition Movements: Organized agent (organized/constrained) — originally mobilized cordon as democratic defense; by 2026, enforcement role has atrophied and organization persists theatrically
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangement as irreducible feature of democracy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(france_cordon_sanitaire_2026, 0.22).
domain_priors:suppression_score(france_cordon_sanitaire_2026, 0.48).
domain_priors:theater_ratio(france_cordon_sanitaire_2026, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(france_cordon_sanitaire_2026, extractiveness, 0.22).
narrative_ontology:constraint_metric(france_cordon_sanitaire_2026, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(france_cordon_sanitaire_2026, theater_ratio, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(france_cordon_sanitaire_2026, piton).
narrative_ontology:human_readable(france_cordon_sanitaire_2026, "The 'Front Républicain' (Republican Front) Cordon Sanitaire").
narrative_ontology:topic_domain(france_cordon_sanitaire_2026, "political/social").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(france_cordon_sanitaire_2026, centrist_coalition_parties).
narrative_ontology:constraint_beneficiary(france_cordon_sanitaire_2026, political_establishment).
narrative_ontology:constraint_victim(france_cordon_sanitaire_2026, democratic_legitimacy).
narrative_ontology:constraint_victim(france_cordon_sanitaire_2026, electoral_competition).
narrative_ontology:constraint_victim(france_cordon_sanitaire_2026, policy_diversity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MARGINALIZED VOTER (PITON) — Voters supporting the cordon-blocked party face a performative democratic process. The cordon's theatrical enforcement (media framing, party coordination, elite consensus) persists despite its declining functional capacity to prevent electoral shifts. Theater ratio=0.78. Voters experience ritual exclusion; the mechanism persists through institutional inertia, not functional necessity. d≈0.88, f(d)≈1.25.
constraint_indexing:constraint_classification(france_cordon_sanitaire_2026, piton,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CENTRIST COALITION (ROPE) — For establishment parties, the cordon was originally coordination: preventing a far-right executive through tactical voting. But by 2026, it functions as pure institutional maintenance. The coalition still derives benefit (vote consolidation, agenda-setting power) with minimal coercion overhead. d≈0.10, f(d)≈-0.08, σ=1.0 → χ≈-0.02. Net beneficiary status sustains participation.
constraint_indexing:constraint_classification(france_cordon_sanitaire_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: REGIONAL POLITICAL ENTREPRENEUR (SNARE) — Locally-rooted politicians who might build independent movements are constrained by the cordon's suppression of alternatives. Career advancement within the establishment coalition requires accepting the cordon framework. Exit to independent candidacy risks electoral isolation (vote-splitting via cordon coordination). d≈0.70, f(d)≈1.05, σ=0.9 → χ≈0.33. Moderate extraction; constrained but not trapped.
constraint_indexing:constraint_classification(france_cordon_sanitaire_2026, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: DEMOCRATIC OPPOSITION MOVEMENT (PITON) — Civic groups and watchdogs initially mobilized the cordon as a democratic defense mechanism. By 2026, their enforcement role has atrophied — the cordon persists theatrically (rallies, media campaigns, 'civic duty' framings) despite declining effectiveness at preventing far-right gains. The opposition sees its own institutional structure as degraded: it mobilizes ritual consensus instead of building substantive policy alternatives. theater_ratio=0.78. d≈0.52, f(d)≈0.70, σ=1.0 → χ≈0.15. Low extraction reflects theater-dependent persistence.
constraint_indexing:constraint_classification(france_cordon_sanitaire_2026, piton,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: FAR-RIGHT PARTY (TANGLED ROPE) — The cordon extracts from this party (vote-splitting coordination prevents plurality conversion to executive power) while simultaneously enabling its organizational growth. Mobilization against the cordon increases supporter engagement and media visibility. The party benefits from framing the cordon as anti-democratic (coordination failure = narrative ammunition). Active enforcement of cordon gates at every election paradoxically legitimizes the party's claim that the system is rigged. d≈0.45, f(d)≈0.42, σ=1.0 → χ≈0.09. Low effective extraction despite high suppression because the party has agency and strategic benefits from the arrangement.
constraint_indexing:constraint_classification(france_cordon_sanitaire_2026, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FALSE NATURALIZATION (MOUNTAIN) — Risks treating the cordon as an irreducible feature of democratic governance: 'Democracies must prevent extremists from taking power.' But the structural data (ε=0.22, suppression=0.48, theater_ratio=0.78) contradicts the mountain gate (ε≤0.25, suppression≤0.05, emerges_naturally=true). The cordon is a contingent institutional arrangement, not a law of political physics. The false summit reveals that naturalizing the cordon obscures its actual composition: party coordination + media narrative + elite consensus-building. These are learned, maintained through effort, and subject to decay.
constraint_indexing:constraint_classification(france_cordon_sanitaire_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(france_cordon_sanitaire_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(france_cordon_sanitaire_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(france_cordon_sanitaire_2026, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(france_cordon_sanitaire_2026, TR),
    TR >= 0.70.

:- end_tests(france_cordon_sanitaire_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.22): Low and declining. Base extractiveness at time T=10 reflects the cordon's reduced functional capacity to suppress far-right electoral gains—the party has grown despite cordon enforcement, indicating suppression is no longer effective at preventing representation. The decline from 0.38 to 0.22 over the interval shows the constraint is becoming less extractive in structural terms; it suppresses electoral competition for the marginalized party, but this suppression no longer blocks the party's organizational growth or electoral advances. Suppression (0.48): Moderate. The cordon maintains suppression through party coordination (mainstream parties' withdrawal coordination in runoffs), media gatekeeping (consensus framing of the party as unsuitable), and elite consensus-building (civic leaders' mobilization for tactical voting). But this suppression is contingent and learned—it requires active institutional maintenance and shows signs of erosion. Theater ratio (0.78): High and rising. The dramatic increase from 0.35 (early 2000s: genuine coordination among authentic alternatives) to 0.78 (2026) signals the shift toward theatrical maintenance. Cordon enforcement has become ritualistic: media campaigns emphasizing civic duty, party leadership speeches about democratic values, and public mobilizations occur predictably each election cycle regardless of actual far-right electoral threat. The performative content has increased relative to functional necessity—the cordon persists through institutional inertia and narrative coherence, not because it prevents the far-right party from gaining seats or influence.
 *
 * PERSPECTIVAL GAP:
 *   This constraint generates a stark perspectival divide based on whether agents are inside or outside the cordon's 'acceptability zone.' The centrist coalition sees the cordon as coordination (Rope)—a legitimate mechanism for preventing democratic breakdown. Marginalized voters outside the acceptable zone see it as pure theater (Piton)—a ritual exclusion process that persists despite its declining functional capacity. The analytical observer risks seeing an immutable feature of democracies (Mountain)—'democracies must prevent extremists'—but the structural data reveals this as a false summit: the cordon is a contingent institutional arrangement with declining extractiveness and rising theater, characteristic of institutional decay rather than natural law. The far-right party's perspective (Tangled Rope) reveals a paradox: the cordon both extracts (suppresses coalition inclusion) and enables (mobilizes supporters by framing the system as rigged). Regional political entrepreneurs see a Snare—career advancement within establishment politics requires accepting cordon logic, constraining alternative political entrepreneurship. The civic opposition movement sees its own organization as Piton—the enforcement machinery (mobilizations, messaging, coordination) persists through inertia despite declining effectiveness.
 *
 * DIRECTIONALITY LOGIC:
 *   Centrist Coalition: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.08. Net beneficiary. Marginalized Voters: Victim + trapped → d≈0.88, f(d)≈1.25. High extraction; voters have no exit from the exclusion. Regional Entrepreneurs: Victim + constrained → d≈0.70, f(d)≈1.05. Significant extraction; career paths require accepting cordon constraints but exit to independent candidacy is costly. Far-Right Party: Constrained beneficiary + mobile → d≈0.45, f(d)≈0.42. Low effective extraction despite suppression because the party has agency and paradoxical benefit from cordon framing. Civic Opposition: Organized + constrained → d≈0.52, f(d)≈0.70. Low effective extraction; organization persists through inertia, theater-dependent.
 *
 * MANDATROPHY ANALYSIS:
 *   The cordon sanitaire resolves the mandatrophy by demonstrating how a coordination mechanism can degrade into a suppression mechanism while maintaining institutional legitimacy through performative renewal. The constraint is NOT a false snare (pure extraction falsely labeled as coordination) because the historical evidence shows genuine coordination function in 2002-2010. Instead, it is a REAL PITON: a former coordination mechanism (Rope) that has become increasingly theater-dependent as its functional necessity has declined. The extractiveness decline from 0.38 to 0.22 combined with theater ratio rise from 0.35 to 0.78 precisely tracks the piton transition: the mechanism's original function (vote-splitting coordination) is no longer structurally necessary because far-right electoral growth persists despite cordon enforcement, yet the constraint persists through institutional inertia (party coordination habits, media consensus, civic narratives). The classification as Piton (not degraded Snare or false Rope) is justified because: (1) suppression is moderate (0.48), not high (≥0.60 required for snare); (2) extractiveness is low (0.22), not high (≥0.46 for snare); (3) theater_ratio is very high (0.78, ≥0.70 required for piton), indicating performative maintenance. The cordon's persistence is not extraction-driven (high chi) but theater-driven (high performance-to-function ratio). This diagnosis enables policy intervention: replace cordon with substantive policy differentiation (shift from theater-dependent exclusion to genuine coalition competition based on programmatic differences).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    functional_threshold_collapse,
    'At what electoral threshold does the cordon mechanism cease to function as vote-splitting coordination and become pure theatrical enforcement?',
    'Historical analysis of cordon effectiveness: measure percentage of voters accepting tactical voting vs. voting sincerely despite cordon framing; correlation between cordon messaging intensity and actual vote concentration; comparison of actual vote outcomes vs. cordon-predicted outcomes across electoral cycles',
    'If threshold < 25% party support: cordon collapses into theater earlier than analysis suggests (piton classification confirmed). If threshold > 35%: cordon has functional reserve capacity, sustaining coordination function longer (shifts toward tangled_rope from some perspectives).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(functional_threshold_collapse, empirical, 'Electoral threshold at which cordon mechanism ceases coordination and becomes pure theater').

omega_variable(
    international_legitimacy_feedback,
    'Does EU/NATO legitimacy pressure on anti-democratic movements strengthen or weaken the cordon''s suppression function within France?',
    'Comparative analysis of European cordon mechanisms (Germany, Italy, Austria); interview data from French party elites on whether external pressure increases or decreases cordon coordination incentives; polling on voter perception of cordon as democratic necessity vs. elite gatekeeping',
    'If external pressure strengthens cordon: suppression remains structural (ε and suppression stable). If external pressure is irrelevant or counterproductive: suppression declines and ε shifts downward (constraint transitions toward rope or dissolves). If external pressure triggers anti-establishment backlash: suppression increases paradoxically (ε stable or rises, theater rises), accelerating piton degradation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_legitimacy_feedback, empirical, 'Whether international legitimacy pressure strengthens or weakens the cordon''s suppression function').

omega_variable(
    alternative_coalition_viability,
    'Would a political coalition including the far-right party (but excluding one current centrist party) be functionally stable, and what would trigger its formation?',
    'Scenario analysis of coalition game theory; identification of policy domains where far-right and current-excluded party have aligned interests; historical precedent analysis from Austrian, Italian, Polish coalitions; simulation of vote distribution shifts that would make such coalition arithmetic viable',
    'If viable: cordon''s suppression function is an active constraint preventing equilibrium, not theater (ε rises, piton classification weakened). If not viable: cordon''s suppression is redundant — the equilibrium excludes the far-right party anyway, and cordon persists as pure theater (ε falls, piton confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_coalition_viability, empirical, 'Whether far-right inclusion in alternative coalition would be functionally stable').

omega_variable(
    democratic_legitimacy_cost_accounting,
    'How much of the reported decline in French civic engagement and trust in democracy since 2017 is attributable to cordon enforcement suppressing political representation?',
    'Regression analysis: civic engagement trends vs. cordon messaging intensity; voter survey data on perceived democratic legitimacy, stratified by whether respondent''s preferred party is in cordon ''acceptability zone''; comparison with democracies lacking explicit cordons (Swiss multi-party coalition, Nordic consensus models)',
    'If cost is substantial (>30% of decline): cordon''s suppression function is genuine but costly to democratic health (supports piton classification — functional trade-off degrading). If cost is negligible: cordon''s suppression is redundant theater (piton confirmed). If cost is high but necessary: shifts toward tangled_rope (mixed coordination benefit and extraction cost).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(democratic_legitimacy_cost_accounting, conceptual, 'Portion of democratic legitimacy decline attributable to cordon enforcement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(france_cordon_sanitaire_2026, 2002, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cordon_tr_t0, france_cordon_sanitaire_2026, theater_ratio, 0, 0.35).
narrative_ontology:measurement(cordon_tr_t5, france_cordon_sanitaire_2026, theater_ratio, 5, 0.58).
narrative_ontology:measurement(cordon_tr_t10, france_cordon_sanitaire_2026, theater_ratio, 10, 0.78).

% Extraction over time
narrative_ontology:measurement(cordon_be_t0, france_cordon_sanitaire_2026, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(cordon_be_t5, france_cordon_sanitaire_2026, base_extractiveness, 5, 0.3).
narrative_ontology:measurement(cordon_be_t10, france_cordon_sanitaire_2026, base_extractiveness, 10, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(france_cordon_sanitaire_2026, enforcement_mechanism).
narrative_ontology:affects_constraint(france_cordon_sanitaire_2026, european_populist_movements).
narrative_ontology:affects_constraint(france_cordon_sanitaire_2026, french_civic_engagement_decline).
narrative_ontology:affects_constraint(france_cordon_sanitaire_2026, coalition_formation_constraints).

% DUAL FORMULATION NOTE:
% The cordon sanitaire constraint is downstream of the broader structural shift in European electoral competition (rise of anti-establishment parties) but represents a distinct contingent institutional response. The upstream structural constraint is the fragmentation of post-WWII consensus politics; the cordon is one nation's chosen mechanism for managing this fragmentation. This decomposition allows analysis of how different democracies (Austria, Italy, Germany, Netherlands) implement different responses to the same upstream structural pressure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(france_cordon_sanitaire_2026, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
