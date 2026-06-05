% ============================================================================
% CONSTRAINT STORY: border_control_legitimacy__sovereignty_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_control_legitimacy__sovereignty_primary, []).

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
 *   constraint_id: border_control_legitimacy__sovereignty_primary
 *   human_readable: Border Control Legitimacy (Sovereignty-Primary Reading)
 *   domain: political_philosophy/international_law/migration_studies
 *
 * SUMMARY:
 *   The sovereignty-primary reading of border control legitimacy holds that
 *   state territorial sovereignty entails absolute discretion to exclude
 *   non-citizens and that border control is constitutive of statehood itself.
 *   Under this reading, the state's right to close its borders is not a
 *   contingent policy choice but a necessary condition of political
 *   existence. This constraint generates the full spectrum of DR
 *   classifications depending on observer position. From the perspective of
 *   excluded migrants, the constraint is a pure snare — they bear the
 *   extraction cost (territorial exclusion) with no exit option and no remedy
 *   within the system that justifies their exclusion. From the perspective of
 *   the territorial state apparatus, the constraint is rope — a pure
 *   coordination mechanism for maintaining the political community and its
 *   institutions. From the perspective of domestic labor forces, the
 *   constraint is tangled rope — it coordinates labor market stability and
 *   enforces wage floors while simultaneously extracting through suppression
 *   of geographic mobility. From international human rights frameworks, the
 *   constraint appears as degrading scaffold — institutional arrangements
 *   being dismantled by generational norm shift and practical pressures
 *   (climate migration, transnational labor markets). From traditional
 *   international law authority, the constraint appears as piton —
 *   Westphalian doctrine that persists through institutional inertia despite
 *   degraded functionality. The analytical observer risks a false summit,
 *   naturalizing a contested reading of a kernel into an immutable law of
 *   politics. This is a kernel reading: the same underlying phenomenon (state
 *   territorial authority) is read differently across the sibling readings
 *   (freedom_of_movement_primary, jurisdictional_sovereignty). This story
 *   instantiates the sovereignty-primary reading exclusively.
 *
 * KEY AGENTS:
 *   - Excluded Migrants: Primary victim (powerless/trapped) — bear extraction cost of sovereignty-based exclusion with no remedy; no exit options within the system
 *   - Territorial State Apparatus: Primary beneficiary (institutional/arbitrage) — treats border control as constitutive of statehood; experiences constraint as legitimate coordination
 *   - Domestic Labor Force: Secondary victim-beneficiary (moderate/constrained) — protected from migrant competition but restricted in geographic mobility; mixed extraction and coordination
 *   - Capital/Employers: Secondary victim-beneficiary (powerful/arbitrage) — restricted labor supply access (extraction) but benefit from stable labor markets and rule of law (coordination)
 *   - International Human Rights Coalition: Organized challenger (organized/mobile) — perceive sovereignty-primary as temporary institutional arrangement subject to sunset; building alternative norms
 *   - Traditional International Law Authority: Institutional incumbent (institutional/arbitrage) — maintains Westphalian sovereignty doctrine through institutional inertia despite degraded functionality
 *   - Analytical Observer: Sees potential false summit (analytical/analytical) — risks naturalizing contingent institutional arrangement into immutable political law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_control_legitimacy__sovereignty_primary, 0.68).
domain_priors:suppression_score(border_control_legitimacy__sovereignty_primary, 0.72).
domain_priors:theater_ratio(border_control_legitimacy__sovereignty_primary, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, extractiveness, 0.68).
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_control_legitimacy__sovereignty_primary, snare).
narrative_ontology:human_readable(border_control_legitimacy__sovereignty_primary, "Border Control Legitimacy (Sovereignty-Primary Reading)").
narrative_ontology:topic_domain(border_control_legitimacy__sovereignty_primary, "political_philosophy/international_law/migration_studies").

domain_priors:requires_active_enforcement(border_control_legitimacy__sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_control_legitimacy__sovereignty_primary, 'a373b737-7fd6-4d7c-a931-6d169743508f').
narrative_ontology:cs_kernel_codification('a373b737-7fd6-4d7c-a931-6d169743508f', formalized).
narrative_ontology:cs_authority_grounding('a373b737-7fd6-4d7c-a931-6d169743508f', extraction).
narrative_ontology:cs_interpretation_layer_present('a373b737-7fd6-4d7c-a931-6d169743508f').
narrative_ontology:cs_reading_relation('a373b737-7fd6-4d7c-a931-6d169743508f', border_control_legitimacy__freedom_of_movement_primary, forecloses).
narrative_ontology:cs_reading_relation('a373b737-7fd6-4d7c-a931-6d169743508f', border_control_legitimacy__jurisdictional_sovereignty, influences).
narrative_ontology:cs_axiom('a373b737-7fd6-4d7c-a931-6d169743508f', foundational, sovereignty_constitutive_of_absolute_exclusion).
narrative_ontology:cs_axiom_status(sovereignty_constitutive_of_absolute_exclusion, holdable).
narrative_ontology:cs_axiom_grounding('a373b737-7fd6-4d7c-a931-6d169743508f', sovereignty_constitutive_of_absolute_exclusion, deontological).
narrative_ontology:cs_axiom('a373b737-7fd6-4d7c-a931-6d169743508f', foundational, territorial_boundary_non_negotiable).
narrative_ontology:cs_axiom_status(territorial_boundary_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('a373b737-7fd6-4d7c-a931-6d169743508f', territorial_boundary_non_negotiable, deontological).
narrative_ontology:cs_reference_frame('a373b737-7fd6-4d7c-a931-6d169743508f', westphalian_sovereign_exclusivity).
narrative_ontology:cs_drift_state('a373b737-7fd6-4d7c-a931-6d169743508f', contemporary_migration_crisis_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a373b737-7fd6-4d7c-a931-6d169743508f', '').
narrative_ontology:cs_kernel_id(border_control_legitimacy__sovereignty_primary, border_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_control_legitimacy__sovereignty_primary, territorial_state_apparatus).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__sovereignty_primary, citizen_labor_protectionists).
narrative_ontology:constraint_victim(border_control_legitimacy__sovereignty_primary, excluded_migrants).
narrative_ontology:constraint_victim(border_control_legitimacy__sovereignty_primary, transnational_labor_mobility).
narrative_ontology:constraint_victim(border_control_legitimacy__sovereignty_primary, freedom_of_movement_claimants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED MIGRANT (SNARE) — No exit options; trapped by territorial exclusion justified as sovereign prerogative. Extraction is total: the migrant bears the cost of sovereignty-based exclusion with no remedy available within the system that justifies exclusion. The suppression mechanism is the border enforcement apparatus, treating entry denial as a sovereign right rather than a discretionary policy choice. Perceives the constraint as pure extraction with no coordination benefit.
constraint_indexing:constraint_classification(border_control_legitimacy__sovereignty_primary, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: TERRITORIAL STATE APPARATUS (ROPE) — Experiences the constraint as pure coordination of state capacity and territorial integrity. The sovereignty-primary reading treats border control as constitutive of statehood itself — not an optional policy choice but a requirement of state existence. From this perspective, exclusion authority enables the state to maintain the institutions (citizenship, law, public goods) that benefit both the state apparatus and its constituents. Experiences the constraint as legitimate coordination, not extraction.
constraint_indexing:constraint_classification(border_control_legitimacy__sovereignty_primary, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: DOMESTIC LABOR FORCE (TANGLED ROPE) — Constrained by labor market dynamics but benefits from state-managed labor supply via border control. The sovereignty-primary reading justifies border closure as protecting domestic wages and employment against migrant competition. This creates mixed coordination (labor market stability) and extraction (suppression of alternative labor sources, geographic mobility, wage optimization). The domestic worker is both beneficiary (protected labor market) and constrained agent (geographic mobility costs).
constraint_indexing:constraint_classification(border_control_legitimacy__sovereignty_primary, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CAPITAL/EMPLOYER (TANGLED ROPE) — Has exit options (relocate operations, lobby for exceptions) but constrained by labor market power. The sovereignty-primary reading creates extraction: employers cannot freely access global labor pools, which raises labor costs. But the reading also enables coordination by guaranteeing labor market stability, predictable wage floors, and enforceable labor law — which employers benefit from relative to pure labor arbitrage. Mixed position: extraction (restricted labor supply) and benefit (rule-of-law labor markets).
constraint_indexing:constraint_classification(border_control_legitimacy__sovereignty_primary, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL HUMAN RIGHTS COALITION (SCAFFOLD) — Organized agents (UN bodies, international NGOs, transnational advocacy networks) perceive the sovereignty-primary reading as a temporary institutional arrangement, not a natural law. The coalition sees border control as a policy choice subject to sunset via evolving human rights norms, regional open-movement agreements, and climate/refugee pressures that eventually force renegotiation. From this perspective, the sovereignty-primary framing is scaffolding being dismantled through generational norm shift. Theater ratio high because sovereignty language masks contingent enforcement choices.
constraint_indexing:constraint_classification(border_control_legitimacy__sovereignty_primary, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: TRADITIONAL INTERNATIONAL LAW AUTHORITY (PITON) — The sovereignty-primary reading rests on Westphalian legal doctrine (territorial exclusivity, border control as essential to statehood). This doctrine is increasingly performative — it persists through institutional inertia in law schools, treaties, and international organization founding texts while material conditions (climate migration, transnational labor markets, digital identity, pandemic response) have degraded its functionality. The international law establishment maintains the sovereignty-primary framing because institutional structures depend on it, not because it coherently addresses contemporary problems. Theater ratio high reflects the gap between the legal narrative and operational reality.
constraint_indexing:constraint_classification(border_control_legitimacy__sovereignty_primary, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, some form of territorial boundary-setting appears inherent to political community itself: any stable collective institution requires some distinction between members and non-members. From this view, border control is an immutable feature of human political organization, not a contingent institutional choice. This perspective risks a false summit — naturalizing what is actually a reading of a contested kernel into an immutable law of politics.
constraint_indexing:constraint_classification(border_control_legitimacy__sovereignty_primary, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_control_legitimacy__sovereignty_primary_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(border_control_legitimacy__sovereignty_primary, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(border_control_legitimacy__sovereignty_primary, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_control_legitimacy__sovereignty_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(border_control_legitimacy__sovereignty_primary, TR),
    TR >= 0.70.

:- end_tests(border_control_legitimacy__sovereignty_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The sovereignty-primary reading justifies border closure as a sovereign prerogative, meaning excluded migrants have no legitimate grounds for remedy or exit. The extraction mechanism is framed as legitimate authority rather than coercion, but the structural effect is that migrants bear 100% of the mobility cost while receiving no compensation or recognition of their agency. The extractiveness value reflects that the sovereignty-primary reading removes even the theoretical grounds for challenging exclusion (in jurisdictional_sovereignty reading, legitimacy requires balancing; in freedom_of_movement_primary reading, exclusion authority doesn't exist). Suppression (0.72): High. Multiple mechanisms suppress alternatives: border enforcement infrastructure (physical barriers, visa systems, deportation machinery); legal doctrine treating sovereignty as absolute (no rights-based limit); labor market protectionism (citizenship preference in employment); asylum law framed as discretionary grant rather than right. Suppression is rising over the measurement interval as border enforcement infrastructure intensifies. Theater ratio (0.58): Moderate-high. The sovereignty-primary framing contains substantial theater: sovereignty language frames a contingent policy choice (who to admit) as a constitutional necessity (territorial control essential to statehood). The performance is necessary because the empirical case for absolute exclusion is weak (open-border jurisdictions function fine; most state functions don't require border closure). As pressures for open movement increase (climate migration, labor market demand, family reunification), the gap between the sovereignty narrative and practical functionality widens, increasing theater. The reading is internally coherent but requires continuous performative assertion that border closure is non-negotiable rather than contingent.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap here is maximal. The excluded migrant perceives snare (pure extraction, no coordination, no escape). The state apparatus perceives rope (pure coordination enabling political community). The domestic worker perceives tangled rope (mixed benefit and extraction). The international human rights coalition perceives scaffold (temporary institutional arrangement with sunset). The international law authority perceives piton (performative maintenance of degraded doctrine). The analytical observer risks perceiving mountain (immutable law of politics). These divergent classifications from fixed base properties reveal the power of indexical framing: the same constraint is coordination, extraction, or natural law depending entirely on the observer's structural position within it. The sovereignty-primary reading is notable because it explicitly anchors the extreme snare classification for the victim (excluded migrant) by denying that any alternative framing is legitimate — sovereignty is absolute, therefore exclusion is justified, therefore the migrant has no claim. This is maximally asymmetric. The jurisdictional_sovereignty reading would soften this by introducing conditionality (legitimacy requires balancing). The freedom_of_movement_primary reading would foreclose the entire structure by denying that sovereignty includes exclusion authority.
 *
 * DIRECTIONALITY LOGIC:
 *   The sovereignty-primary reading determines directionality by assigning full sovereignty authority to the territorial state. This creates extreme directionality asymmetry: the state is the sole beneficiary (d ≈ 0.0 for institutional/arbitrage position) with power to define the rule, while excluded migrants are pure victims (d ≈ 1.0 for powerless/trapped position). The derivation chain works as follows: (1) beneficiary = territorial state apparatus with arbitrage exit (can relocate institutions, diplomatic options, economic leverage); (2) victim = excluded migrants with trapped exit (no territorial access, no legal remedy within state system); (3) engine derives d ≈ 0.0 for beneficiary and d ≈ 1.0 for victim; (4) f(d) sigmoid applies: institutional/arbitrage → negative chi (benefits from constraint), powerless/trapped → maximum chi (bears full extraction). The sovereignty-primary reading's theoretical claim is that this asymmetry is justified by the nature of statehood itself — the state's authority to define membership is not coercive but constitutive. The competing readings would derive different d values by introducing rights-based or balancing constraints that reduce the state's unilateral authority. Domestic labor force gets intermediate d (~0.55) by being both beneficiary (protected labor market) and constrained victim (geographic mobility suppressed); this produces tangled_rope. No directionality overrides are needed — the structural derivation captures the reading's claim accurately.
 *
 * MANDATROPHY ANALYSIS:
 *   The sovereignty-primary reading avoids simple mandatrophy (confusing coordination and extraction) by explicitly claiming that border control is pure coordination (enabling state function) rather than extraction. The snare classification from the migrant's perspective is not mandatrophy — it is a perspectival gap. The constraint genuinely enables state-provided public goods (law, defense, courts) for citizens, which is coordination. But it simultaneously extracts by denying mobility rights to non-citizens without their consent or compensatory mechanism. The reading resolves this by treating non-citizens as outside the moral community of the state — they are not stakeholders in the coordination problem the state solves. This is not mandatrophy; it is the logical consequence of the reading's core axiom (sovereignty_constitutive_of_absolute_exclusion). The mandatrophy would arise if the reading claimed border control was ONLY extraction (pure snare) while simultaneously being necessary for state function (which would be self-contradictory). Instead, the reading claims border control is legitimate coordination for citizens and legitimate exclusion authority over non-citizens — no contradiction from within the reading's logic. The external critique (freedom_of_movement_primary reading) claims this logic is incoherent because it denies that migrants have standing to be part of the balancing calculation. That is a reading-level disagreement, not a mandatrophy within a single reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_constitutive_vs_instrumental,
    'Is border control authority constitutive of statehood (intrinsic to what makes an entity a state) or an instrumental power (a state can exist and exercise authority while permitting migration)?',
    'Comparative analysis of state institutions that function without border closure (Schengen Area, EU member states, historical city-states with open movement); demonstration that statehood and rule-of-law persist with open borders; identification of whether border closure is required for any specific state function (law enforcement, public goods provision, taxation) or merely historically convenient',
    'If instrumental: the sovereignty-primary reading''s core axiom fails, and the constraint reclassifies toward rope or tangled_rope (border control is policy choice, not sovereign necessity). If constitutive: the axiom holds and the snare classification is defensible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_constitutive_vs_instrumental, conceptual, 'Whether border closure is intrinsic to statehood or an instrumental policy choice').

omega_variable(
    human_rights_constraint_status,
    'Are human rights constraints (freedom of movement, family reunification, asylum) external limits on legitimate state power, or are they constitutive of what makes state authority legitimate?',
    'Jurisprudential analysis: do human rights frameworks treat migration rights as negative constraints on sovereignty (sovereignty exists but is limited by rights) or as foundational to state legitimacy (a state that violates freedom of movement fundamentally lacks authority)? Empirical test: do states that violate freedom of movement retain international recognition and rule-of-law legitimacy?',
    'If external constraints: the sovereignty-primary reading holds (sovereignty exists first, rights limit it). If constitutive: the reading conflates authority with power and forecloses freedom-of-movement-primary reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(human_rights_constraint_status, conceptual, 'Whether human rights are limits on sovereignty or constitutive of it').

omega_variable(
    exclusion_mechanism_empirical_necessity,
    'What specific functions (public goods provisioning, law enforcement, citizenship institution, labor market regulation, welfare distribution) actually require border closure, and which are historically contingent adaptations?',
    'Functional decomposition: identify which state services can operate with open borders (law, courts, defense, infrastructure, education, healthcare) and which genuinely require membership boundaries. Comparative case analysis: open-border jurisdictions (EU, city-states, international organizations) and identification of what functions they sacrifice or restructure.',
    'If most functions are feasible with open borders: extractiveness should be lower (suppression is policy choice, not structural necessity). If some functions require closure: extractiveness reflects genuine coordination cost alongside extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exclusion_mechanism_empirical_necessity, empirical, 'Which state functions empirically require border closure vs. which are contingent adaptations').

omega_variable(
    reading_contest_location,
    'This constraint is one reading of the contested kernel ''border_control_legitimacy.'' Which sibling reading''s core premise does this reading''s axiom directly contradict?',
    'Structural analysis of three readings: (1) freedom_of_movement_primary claims freedom of movement is a fundamental right that sovereignty does not override; (2) jurisdictional_sovereignty separates regulatory authority from border closure authority and makes legitimacy conditional on balancing; (3) this reading claims sovereignty entails absolute border closure discretion. Freedom_of_movement_primary directly contradicts this reading''s axiom (sovereignty_constitutive_of_absolute_exclusion). Jurisdictional_sovereignty disputes the absoluteness claim but accepts some border authority.',
    'If freedom_of_movement_primary''s axiom (freedom_is_fundamental_right) is true, this reading''s axiom (sovereignty_constitutive_of_absolute_exclusion) is false — they foreclose each other. The jurisdictional_sovereignty reading influences both by introducing conditionality and balancing requirements.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_location, conceptual, 'Logical relationship between this reading''s axioms and sibling readings'' axioms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_control_legitimacy__sovereignty_primary, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t0, border_control_legitimacy__sovereignty_primary, theater_ratio, 0, 0.42).
narrative_ontology:measurement(bord_tr_t25, border_control_legitimacy__sovereignty_primary, theater_ratio, 25, 0.52).
narrative_ontology:measurement(bord_tr_t50, border_control_legitimacy__sovereignty_primary, theater_ratio, 50, 0.58).

% Extraction over time
narrative_ontology:measurement(bord_be_t0, border_control_legitimacy__sovereignty_primary, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(bord_be_t25, border_control_legitimacy__sovereignty_primary, base_extractiveness, 25, 0.62).
narrative_ontology:measurement(bord_be_t50, border_control_legitimacy__sovereignty_primary, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t0, border_control_legitimacy__sovereignty_primary, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(bord_su_t25, border_control_legitimacy__sovereignty_primary, suppression_requirement, 25, 0.68).
narrative_ontology:measurement(bord_su_t50, border_control_legitimacy__sovereignty_primary, suppression_requirement, 50, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_control_legitimacy__sovereignty_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(border_control_legitimacy__sovereignty_primary, border_control_legitimacy__freedom_of_movement_primary).
narrative_ontology:affects_constraint(border_control_legitimacy__sovereignty_primary, border_control_legitimacy__jurisdictional_sovereignty).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'border_control_legitimacy.' The sibling readings (freedom_of_movement_primary and jurisdictional_sovereignty) instantiate different interpretations of the same underlying phenomenon: state territorial authority and membership. The three readings have different epsilon values reflecting different structural claims about whether border closure is inherent (sovereignty_primary ε=0.68), contingent on human rights balance (jurisdictional_sovereignty), or illegitimate (freedom_of_movement_primary). Each reading is a separate constraint story with its own beneficiary/victim structure, directionality, and omega variables documenting unresolved disputes. The network links track the constitutional contest: sibling readings are distinct constraint instances derived from the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
