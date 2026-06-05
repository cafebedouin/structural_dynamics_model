% ============================================================================
% CONSTRAINT STORY: the_churn_systemic_upheaval
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_the_churn_systemic_upheaval, []).

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
 *   constraint_id: the_churn_systemic_upheaval
 *   human_readable: The Churn (Systemic Collapse and Rebirth)
 *   domain: political/social/economic
 *
 * SUMMARY:
 *   The Churn represents periods of systemic instability where established
 *   institutional rules dissolve and reconfigure. Historical examples include
 *   regime transitions (1789 France, 1917 Russia, 1989 Eastern Europe, 2011
 *   Arab Spring), economic system resets (1930s Great Depression, 2008
 *   financial collapse), and civilizational ruptures (collapse of empires,
 *   pandemic-driven institutional failure). The constraint is characterized
 *   by simultaneous collapse of enforcement mechanisms (suppression remains
 *   high but applied inconsistently), emergence of competing institutional
 *   forms (insurgent coalitions, shadow economies, informal governance), and
 *   profound extraction asymmetries distributed unevenly across populations
 *   based on adaptive capacity and institutional proximity. The Churn is NOT
 *   a natural law or inevitable feature of complex systems — it is a
 *   contingent outcome of specific institutional design failures
 *   (concentration of power, inequality, lack of exit options, fragile
 *   legitimacy) that interact with exogenous shocks (economic crisis,
 *   military defeat, pandemic, technological disruption). Once triggered, the
 *   Churn exhibits genuine features of both coordination (new institutions
 *   emerging, alternative rules being negotiated) and extraction (incumbent
 *   elites preserving power through coercion, international powers imposing
 *   terms, profiteers exploiting chaos). The constraint's theater_ratio
 *   increases over the interval as formal state institutions become
 *   increasingly performative while real authority migrates to informal
 *   networks and shadow power structures.
 *
 * KEY AGENTS:
 *   - Precarious Populations: Primary victims (powerless/trapped) — face maximum extraction as social safety nets collapse, employment vanishes, and bodily security becomes uncertain
 *   - Incumbent Elites: Secondary targets (organized/constrained) — must extract harder to preserve privilege but lack enforcement capacity; locked into old rules that no longer work
 *   - Insurgent Coalitions: Primary beneficiaries (institutional/arbitrage) — capture control over institutional innovation; can exit old system rules and impose new ones
 *   - International Institutions: Temporary stabilizers (moderate/mobile) — IMF, UN, regional powers provide scaffolding; can withdraw support if conditions deteriorate
 *   - Professional/Middle Classes: Secondary victims (moderate/constrained) — lose credentials and income as institutional value collapses; cannot arbitrage out
 *   - Vestigial State Apparatus: Institutional actor maintaining fictions (institutional/constrained) — preserves performative legitimacy (courts, bureaucracy, legal proceedings) while lacking enforcement capacity
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional failure as inevitable historical law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(the_churn_systemic_upheaval, 0.65).
domain_priors:suppression_score(the_churn_systemic_upheaval, 0.72).
domain_priors:theater_ratio(the_churn_systemic_upheaval, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(the_churn_systemic_upheaval, extractiveness, 0.65).
narrative_ontology:constraint_metric(the_churn_systemic_upheaval, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(the_churn_systemic_upheaval, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(the_churn_systemic_upheaval, tangled_rope).
narrative_ontology:human_readable(the_churn_systemic_upheaval, "The Churn (Systemic Collapse and Rebirth)").
narrative_ontology:topic_domain(the_churn_systemic_upheaval, "political/social/economic").

domain_priors:requires_active_enforcement(the_churn_systemic_upheaval).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(the_churn_systemic_upheaval, institutional_innovators).
narrative_ontology:constraint_beneficiary(the_churn_systemic_upheaval, insurgent_coalitions).
narrative_ontology:constraint_victim(the_churn_systemic_upheaval, incumbent_elites).
narrative_ontology:constraint_victim(the_churn_systemic_upheaval, institutional_stability).
narrative_ontology:constraint_victim(the_churn_systemic_upheaval, precarious_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRECARIOUS POPULATIONS (SNARE) — Trapped within collapsing institutions with no exit option. Bears maximum cost of systemic instability: loss of employment, savings, social safety nets, bodily security. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.92. Pure extraction masquerading as 'creative destruction'.
constraint_indexing:constraint_classification(the_churn_systemic_upheaval, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INCUMBENT ELITES (TANGLED ROPE) — Experience both coordination function (preservation of social order through law and custom) and asymmetric extraction (their privileged positions depend on system stability they cannot guarantee). Constrained by institutional lock-in; cannot exit without losing legitimacy. d≈0.60, f(d)≈0.82, σ=1.0 → χ≈0.53. Mixed: they benefit from the institutional coordination but bear extraction costs when that coordination fails.
constraint_indexing:constraint_classification(the_churn_systemic_upheaval, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INSURGENT COALITIONS (ROPE) — Benefit from coordinating around novel institutional forms and rules. Arbitrage exit: they can defect to alternative systems (new parties, alternative platforms, alternative governance structures). d≈0.10, f(d)≈0.05, σ=1.2 → χ≈0.04. Low effective extraction; genuine coordination function of offering new organizing principles. Pure beneficiaries.
constraint_indexing:constraint_classification(the_churn_systemic_upheaval, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INTERNATIONAL INSTITUTIONS (SCAFFOLD) — Temporary support structures (IMF restructuring, UN coordination, regional trade agreements, emergency governance frameworks) that manage churn's worst collective action failures. Mobile exit (can withdraw support if conditions deteriorate). d≈0.50, f(d)≈0.65, σ=1.1 → χ≈0.47. Coordination with explicit sunset: once local institutions stabilize, international scaffolding is no longer needed and may become extractive if prolonged.
constraint_indexing:constraint_classification(the_churn_systemic_upheaval, scaffold,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: VESTIGIAL STATE INSTITUTIONS (PITON) — Formal state apparatus persists (courts, bureaucracy, legislatures) but increasingly performative as real decision-making migrates to informal networks, warlords, shadow economies, or insurgent structures. theater_ratio=0.58 approaches piton threshold (0.70); the formal state maintains theater of legitimacy (elections, press releases, legal proceedings) while lacking functional enforcement capacity. d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.49. Maintained through inertia, risk of rapid collapse.
constraint_indexing:constraint_classification(the_churn_systemic_upheaval, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: PROFESSIONAL/MIDDLE CLASSES (SNARE) — Constrained by dependence on the collapsing institutions for credentials, income, and social position. Face extraction costs: depreciation of educational credentials, loss of professional identity, career disruption, skill obsolescence. Cannot arbitrage out (professional licenses not portable; cultural capital loses value). d≈0.80, f(d)≈1.25, σ=1.0 → χ≈0.81. Significant extraction with no clean exit.
constraint_indexing:constraint_classification(the_churn_systemic_upheaval, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / SYSTEMIC VIEW (FALSE SUMMIT) — Temptation to view the Churn as an immutable law of complex systems: all institutional orders eventually decay and are replaced; this is 'inevitable' or 'natural.' accessibility_collapse would be ≈0.75, resistance ≈0.30, emerges_naturally=false. This classification FAILS the mountain gates (needs emerges_naturally=true, accessibility_collapse≥0.85, resistance≤0.15). The false summit reveals that the Churn is NOT a natural law — it is a contingent outcome of specific institutional design failures, inequality dynamics, and coordination breakdowns. However, the constraint's structure (ε=0.65, suppression=0.72) confirms this is not a mountain.
constraint_indexing:constraint_classification(the_churn_systemic_upheaval, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(the_churn_systemic_upheaval_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(the_churn_systemic_upheaval, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(the_churn_systemic_upheaval, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(the_churn_systemic_upheaval, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(the_churn_systemic_upheaval, TR),
    TR >= 0.70.

:- end_tests(the_churn_systemic_upheaval_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65): High. The Churn is characterized by massive asymmetric extraction: precarious populations lose livelihoods and security with no compensation; incumbent elites extract harder to preserve privilege; insurgent coalitions extract institutional rents through control of new institutional forms. The constraint exhibits all three components of the chi formula operating at high intensity. The value increases over the interval (0.35→0.65) as initial chaos gives way to organized extraction by dominant groups. Suppression (0.72): High. Multiple overlapping suppression mechanisms: coercive capacity degradation (state enforcement breaks down but informal violence increases), institutional barriers to exit (professional credentials lose value, borders become uncontrollable, spatial arbitrage fails), and cognitive suppression (information environment becomes fragmented, epistemic commons collapses, competing narratives prevent coordination on escape routes). Theater_ratio (0.58): Moderate-high, increasing. As the Churn progresses, formal institutions lose functional capacity but increase performative content: elections held but power concentrated in shadow structures; legal proceedings continue but enforcement is arbitrary; bureaucratic forms persist but decisions are made in back channels. The ratio rises over the interval (0.30→0.58) as gap between formal and functional authority widens.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximum perspectival disagreement. Precarious populations see a pure Snare: extraction with no escape. Incumbent elites see both coordination (rule of law) and extraction (need to coerce harder to preserve order) — Tangled Rope. Insurgent coalitions see Rope: genuine coordination function of creating new institutional forms and offering escape routes. International institutions see Scaffold: temporary stabilization until local institutions recover and sunset the external coordination mechanism. Vestigial state institutions see their own performative degradation (Piton): maintaining theater of legitimacy with diminishing functional capacity. Professional classes see extraction (Snare) — their expertise becomes worthless and they cannot arbitrage out. The analytical observer risks seeing Mountain (inevitable historical cycles, natural law of civilization) but the structural data contradicts this: ε=0.65 is too high for a natural law; emergence_naturally=false; resistance and accessibility_collapse values would fail mountain gates. The perspectival gaps reveal that 'the Churn' is not a single constraint but an ecosystem of constraints with different extraction profiles depending on structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   Precarious populations: Victim + trapped → d≈0.95, f(d)≈1.42. Maximum extraction without escape. Incumbent elites: Both beneficiary (coordination, order) and victim (cannot preserve extraction, must coerce harder) + constrained → d≈0.60, f(d)≈0.82. Mixed extraction. Insurgent coalitions: Beneficiary (institutional innovation) + arbitrage → d≈0.10, f(d)≈0.05. Net beneficiaries with low effective extraction. International institutions: Moderate power + mobile + temporary support → d≈0.50, f(d)≈0.65. Balanced extraction costs and coordination benefits. Professional/middle classes: Victim (credentials lose value) + constrained (cannot exit) → d≈0.80, f(d)≈1.25. High extraction. Vestigial state: Institutional + constrained (maintains legitimacy but has no power) → d≈0.55, f(d)≈0.75. Moderate extraction, mixed with performative function.
 *
 * MANDATROPHY ANALYSIS:
 *   The Churn resolves mandatrophy by revealing that what appears to be 'inevitable systemic reset' is actually a structured extraction game disguised as institutional renewal. The beneficiary/victim analysis shows: (1) Incumbents extract through coercion to preserve privilege; (2) Insurgents extract rents through control of new institutional forms; (3) Precarious populations bear almost all costs with no coordination mechanism to escape. International institutions provide temporary coordination (Scaffold) but this can become extractive (international debt, conditionality). The risk of false naturalization is high: labeling the Churn as 'inevitable' or 'natural' is how insurgents justify extraction and how incumbents justify coercion. The mandatrophy is resolved by accepting that the Churn is a contingent outcome of institutional design failures, not a law of nature — and therefore fixable through institutional redesign (more exit options, flatter extraction profiles, stronger coordination for precarious populations). The Tangled Rope classification (coordination function of institutional renewal + asymmetric extraction costs) captures the true structure: the Churn offers genuine escape routes for some (Rope benefit) but only through massive extraction from others (Snare cost). Mandatrophy is resolved when this asymmetry is made explicit and addressed in post-churn institutional design.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    speed_of_institutional_collapse,
    'What triggers the transition from institutional degradation to open systemic collapse? Is there a tipping point or a continuum?',
    'Historical comparative analysis of regime change events; identification of measurable precursors (coercive capacity loss, elite consensus fragmentation, spontaneous coordination of alternative authority); network analysis of institutional interdependencies',
    'If sharp tipping point exists: early warning detection is feasible, intervention windows are narrow. If continuum: degradation can persist indefinitely, making systemic stability a matter of continual reproduction rather than threshold crossing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(speed_of_institutional_collapse, empirical, 'Whether institutional collapse has a sharp tipping point or gradual continuum').

omega_variable(
    beneficiary_identity_during_churn,
    'Who actually benefits from the Churn? Are insurgent coalitions genuine beneficiaries of new institutional innovation, or are they merely predators who extract during collapse?',
    'Longitudinal analysis of insurgent organizations: do they maintain low-extraction coordination structures after gaining power, or do they reconstitute extraction hierarchies? Comparison of post-churn institutional outcomes across cases.',
    'If insurgents maintain low-extraction: Rope classification is justified, churn is genuine institutional renewal with coordination benefits. If insurgents reconstitute extraction: insurgent benefit is predatory, and ''institutional innovation'' was merely a phase in the cycle of domination. Classification shifts from Rope to Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_identity_during_churn, empirical, 'Whether insurgent coalitions genuinely innovate or reconstitute extraction').

omega_variable(
    precarious_population_coalition_threshold,
    'Can precarious populations overcome coordination barriers to form self-organized alternatives, or does precarity itself prevent coalition formation?',
    'Analysis of historical churn events: identification of cases where precarious populations organized versus fragmented; network analysis of mutual aid and informal institutions; measurement of trust and social capital recovery during churn periods',
    'If coalition formation is possible: precarious populations shift from powerless/trapped to organized/constrained, classification changes from Snare to Tangled Rope, exit options improve. If precarity prevents coalition: Snare classification persists, extraction continues unabated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(precarious_population_coalition_threshold, empirical, 'Whether precarious populations can form coalitions during systemic collapse').

omega_variable(
    external_stabilization_dependency,
    'Does international scaffolding (IMF, UN, regional powers) enable faster institutional reconstruction or does it embed dependency and forestall genuine local renewal?',
    'Comparative outcomes analysis: post-churn institutional strength in cases with heavy vs light international stabilization; measurement of institutional debt, sovereignty constraints, and long-term stability; analysis of whether scaffolding becomes extractive lock-in',
    'If stabilization enables renewal: Scaffold classification is justified, sunset mechanism is real, temporary extraction is justified cost. If stabilization embeds dependency: external institutions become permanent extractive layer, classification shifts to Tangled Rope or Snare at international scale.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(external_stabilization_dependency, empirical, 'Whether international scaffolding enables or impedes institutional renewal').

omega_variable(
    churn_cyclicality,
    'Is the Churn a one-time event (regime transition), a cyclical recurrence (periodic systemic reset), or a symptom of fundamental institutional design failure requiring deeper change?',
    'Historical time-series analysis of institutional stability indicators across centuries; identification of cycle frequencies and amplitudes; analysis of whether ''stability periods'' are genuine equilibrium or just low-churn phases before next collapse',
    'If one-time: system can stabilize permanently post-churn. If cyclical: churn is inherent to the institutional type, and precarious populations face perpetual Snare conditions. If design failure: fundamental institutional change (property relations, governance structures, inequality) is necessary to prevent recurrence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(churn_cyclicality, conceptual, 'Whether the Churn is one-time, cyclical, or inherent to institutional design').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(the_churn_systemic_upheaval, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(churn_tr_t0, the_churn_systemic_upheaval, theater_ratio, 0, 0.3).
narrative_ontology:measurement(churn_tr_t5, the_churn_systemic_upheaval, theater_ratio, 5, 0.44).
narrative_ontology:measurement(churn_tr_t10, the_churn_systemic_upheaval, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(churn_be_t0, the_churn_systemic_upheaval, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(churn_be_t5, the_churn_systemic_upheaval, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(churn_be_t10, the_churn_systemic_upheaval, base_extractiveness, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(the_churn_systemic_upheaval, enforcement_mechanism).
narrative_ontology:affects_constraint(the_churn_systemic_upheaval, institutional_capture).
narrative_ontology:affects_constraint(the_churn_systemic_upheaval, legitimacy_collapse).
narrative_ontology:affects_constraint(the_churn_systemic_upheaval, coercive_capacity_fragmentation).
narrative_ontology:affects_constraint(the_churn_systemic_upheaval, inequality_amplification).

% DUAL FORMULATION NOTE:
% The Churn is an ecosystem of distinct structural constraints that become salient during systemic breakdown. Upstream constraints (institutional_capture, inequality_amplification) create fragility; the Churn is the phase transition when that fragility triggers cascade failures. Downstream constraints (legitimacy_collapse, coercive_capacity_fragmentation) are consequences of the Churn becoming manifest. The constraint family should be analyzed as a presheaf: each member has different ε, different beneficiary/victim profiles, and different temporal dynamics, but they are interdependent.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(the_churn_systemic_upheaval, organized, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
