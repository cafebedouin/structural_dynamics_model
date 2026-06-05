% ============================================================================
% CONSTRAINT STORY: geneva_conventions_1949__security_maximization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_1949__security_maximization_reading, []).

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
 *   constraint_id: geneva_conventions_1949__security_maximization_reading
 *   human_readable: Geneva Conventions 1949 — Security Maximization Reading
 *   domain: international_humanitarian_law/armed_conflict/political_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the security-maximization reading of the
 *   1949 Geneva Conventions, treating humanitarian law obligations as
 *   peacetime aspirations that must yield to operational necessity in
 *   asymmetric conflict. This reading reorganizes the entire protective logic
 *   of international humanitarian law by: (1) expanding the 'unlawful
 *   combatant' category to deny POW status and habeas corpus protections to
 *   non-state actors; (2) normalizing coercive interrogation through
 *   redefinition of torture boundaries; (3) degrading civilian immunity
 *   through collateral damage acceptance and 'human shield' doctrines; (4)
 *   implementing indefinite detention without trial as a preventive security
 *   measure. The reading treats asymmetric conflict (state vs. non-state
 *   armed groups) as structurally requiring a different interpretive frame
 *   than symmetric warfare — where the Geneva Conventions' protective
 *   provisions were designed for conflicts between uniformed state forces,
 *   application to non-state actors is presented as requiring suspension or
 *   reinterpretation of those provisions to maximize state security. The
 *   constraint exhibits as a Snare from the perspective of detainees,
 *   civilians, and those denied combatant status; as Rope (coordination
 *   enabling security operations) from the state apparatus perspective; and
 *   as Piton (performatively functional but substantively hollowed) from the
 *   international humanitarian law regime perspective. The analytical
 *   observer's mountain perspective risks naturalizing what is actually a
 *   contingent institutional choice by the state security apparatus.
 *
 * KEY AGENTS:
 *   - State Security Apparatus: Primary beneficiary (institutional/arbitrage) — gains operational freedom, enhanced interrogation authority, indefinite detention power, expanded targeting latitude through collateral damage acceptance and human-shield framings.
 *   - Detainees Without Status: Primary victim (powerless/trapped) — denied POW protections, subjected to indefinite detention, coercive interrogation, and legal category denial; no habeas corpus or appeal mechanism.
 *   - Civilian Populations in Conflict Zones: Secondary victim (powerless/trapped) — trapped in geographic zones where civilian immunity is degraded through proportionality recalculation and human-shield doctrines.
 *   - International Humanitarian Law Regime: Institutional actor (institutional/arbitrage) — maintains performative structure but with degraded protective force; piton classification reflects hollowed functional constraint.
 *   - International Monitoring and Accountability Bodies: Organized actors (organized/constrained) — provide coordination (documenting violations, maintaining norms) while bearing cost of state cooperation requirements; extract from states through accountability pressure while also enabling states through selective enforcement.
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing security-maximization logic as structural inevitability rather than recognizing it as a reading that benefits the state apparatus.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_1949__security_maximization_reading, 0.68).
domain_priors:suppression_score(geneva_conventions_1949__security_maximization_reading, 0.75).
domain_priors:theater_ratio(geneva_conventions_1949__security_maximization_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_1949__security_maximization_reading, snare).
narrative_ontology:human_readable(geneva_conventions_1949__security_maximization_reading, "Geneva Conventions 1949 — Security Maximization Reading").
narrative_ontology:topic_domain(geneva_conventions_1949__security_maximization_reading, "international_humanitarian_law/armed_conflict/political_philosophy").

domain_priors:requires_active_enforcement(geneva_conventions_1949__security_maximization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_1949__security_maximization_reading, '70525136-7c9c-458b-bfa2-07f51c14b2d1').
narrative_ontology:cs_kernel_codification('70525136-7c9c-458b-bfa2-07f51c14b2d1', formalized).
narrative_ontology:cs_authority_grounding('70525136-7c9c-458b-bfa2-07f51c14b2d1', extraction).
narrative_ontology:cs_interpretation_layer_present('70525136-7c9c-458b-bfa2-07f51c14b2d1').
narrative_ontology:cs_reading_relation('70525136-7c9c-458b-bfa2-07f51c14b2d1', geneva_conventions_1949__humanitarian_ceiling_reading, forecloses).
narrative_ontology:cs_reading_relation('70525136-7c9c-458b-bfa2-07f51c14b2d1', geneva_conventions_1949__conditional_reciprocity_reading, influences).
narrative_ontology:cs_axiom('70525136-7c9c-458b-bfa2-07f51c14b2d1', foundational, operational_necessity_overrides_humanitarian_floors).
narrative_ontology:cs_axiom_status(operational_necessity_overrides_humanitarian_floors, holdable).
narrative_ontology:cs_axiom_grounding('70525136-7c9c-458b-bfa2-07f51c14b2d1', operational_necessity_overrides_humanitarian_floors, empirically_contingent).
narrative_ontology:cs_axiom('70525136-7c9c-458b-bfa2-07f51c14b2d1', foundational, asymmetric_conflict_requires_different_protective_logic).
narrative_ontology:cs_axiom_status(asymmetric_conflict_requires_different_protective_logic, holdable).
narrative_ontology:cs_axiom_grounding('70525136-7c9c-458b-bfa2-07f51c14b2d1', asymmetric_conflict_requires_different_protective_logic, empirically_contingent).
narrative_ontology:cs_reference_frame('70525136-7c9c-458b-bfa2-07f51c14b2d1', state_security_maximization_framework).
narrative_ontology:cs_drift_state('70525136-7c9c-458b-bfa2-07f51c14b2d1', contemporary_post_2001_counterterrorism_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('70525136-7c9c-458b-bfa2-07f51c14b2d1', '').
narrative_ontology:cs_kernel_id(geneva_conventions_1949__security_maximization_reading, geneva_conventions_1949).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__security_maximization_reading, state_security_apparatus).
narrative_ontology:constraint_victim(geneva_conventions_1949__security_maximization_reading, unlawful_combatants).
narrative_ontology:constraint_victim(geneva_conventions_1949__security_maximization_reading, detainees_without_status).
narrative_ontology:constraint_victim(geneva_conventions_1949__security_maximization_reading, civilian_populations_in_conflict_zones).
narrative_ontology:constraint_victim(geneva_conventions_1949__security_maximization_reading, international_humanitarian_law_regime).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DETAINEE WITHOUT STATUS (SNARE) — Cannot exit the detention system; lacks legal status (unlawful combatant category); zero habeas corpus; subject to indefinite detention and coercive interrogation; no appeal mechanism or protection floor. Maximum extraction — all suppression mechanisms (legal category denial, indefinite detention, isolation, interrogation coercion) operate without restraint.
constraint_indexing:constraint_classification(geneva_conventions_1949__security_maximization_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CIVILIAN POPULATIONS IN ASYMMETRIC CONFLICT (SNARE) — Trapped in geographic zones where state security logic justifies degraded civilian immunity; exposed to collateral damage acceptance, human-shield framings, and targeting logic that minimizes but permits civilian harm as cost of counterinsurgency. No exit option — civilians cannot relocate in conflict zones without additional risk. Suppression is structural: geographic entrapment + legal doctrine that treats their presence as force multiplier.
constraint_indexing:constraint_classification(geneva_conventions_1949__security_maximization_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: STATE SECURITY APPARATUS (ROPE) — Experiences the constraint as coordination mechanism for maximizing state security operational effectiveness. The security-maximization reading reformulates Geneva Conventions obligations as coordination problems solvable by: expanding unlawful combatant category, normalizing coercive interrogation as non-torture, accepting collateral damage within optimization logic, implementing indefinite detention as preventive security measure. Net beneficiary — apparatus gains operational freedom, enhanced interrogation capacity, and preventive detention authority. Low experienced extraction because the apparatus views the constraint as enabling rather than restricting.
constraint_indexing:constraint_classification(geneva_conventions_1949__security_maximization_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INTERNATIONAL HUMANITARIAN LAW REGIME (PITON) — The 1949 Geneva Conventions regime persists as institutional structure but with degraded functional constraint on state violence. The security-maximization reading has hollowed the regime's protective force: unlawful combatant category expands to deny POW status; interrogation coercion normalized as non-torture; civilian immunity degraded through collateral damage calculation. The regime remains performatively active (states ratify treaties, invoke provisions, claim compliance) but the protective mechanism is inert. Theater ratio reflects this: states stage compliance through legal categorizations and interrogation protocols that nominally respect humanitarian principles while systematically undermining their protective intent.
constraint_indexing:constraint_classification(geneva_conventions_1949__security_maximization_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL MONITORING AND ACCOUNTABILITY BODIES (TANGLED ROPE) — Organizations like the International Committee of the Red Cross, Human Rights Watch, and UN fact-finding missions perform a mixed function: they provide genuine coordination (documenting violations, maintaining humanitarian standards as reference frame, training in IHL compliance) while also extracting from states the burden of external accountability and legitimacy performance. Constrained by state cooperation requirements and lack of enforcement power. They benefit from the constraint's persistence (ongoing violations generate mandate renewal and funding) while also bearing costs of documenting extraction they cannot stop. Mixed extraction and coordination — they coordinate humanitarian norms while state security apparatus extracts legitimacy from their continued activism.
constraint_indexing:constraint_classification(geneva_conventions_1949__security_maximization_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / STRUCTURAL INEVITABILITY (MOUNTAIN) — From a civilizational perspective, security maximization in asymmetric conflict appears structurally inevitable: faced with non-state actors who do not wear uniforms, respect civilian areas, or comply with laws of war, states cannot credibly protect civilians without suspending constraints designed for symmetric warfare between uniformed forces. This reading naturalizes the constraint as an inherent response to structural threat, presenting the security-maximization logic as immutable law of armed conflict under asymmetric conditions. However, this perspective faces FSM scrutiny: the state security apparatus is an identifiable beneficiary whose extraction depends on maintaining the unlawful-combatant category and indefinite-detention authority. The 'inevitability' framing naturalizes what is actually a contingent institutional arrangement enforcing state security preferences.
constraint_indexing:constraint_classification(geneva_conventions_1949__security_maximization_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_1949__security_maximization_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(geneva_conventions_1949__security_maximization_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(geneva_conventions_1949__security_maximization_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_1949__security_maximization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(geneva_conventions_1949__security_maximization_reading, TR),
    TR >= 0.70.

:- end_tests(geneva_conventions_1949__security_maximization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The security-maximization reading extracts substantial compliance costs from powerless agents (detainees, civilians) while benefiting the state apparatus through operational freedom. The extraction has accumulated over time (0.48 → 0.62 → 0.68) as the reading's institutional embedding has deepened — unlawful combatant categories have expanded, interrogation protocols have been normalized, detention authority has been consolidated. The reading produces structural asymmetry where state actors gain freedom while non-state actors and civilians lose protections. Suppression (0.75): Very high. The constraint operates through multiple suppression mechanisms: (1) legal category denial (unlawful combatant status strips POW protections); (2) indefinite detention without habeas corpus or appeal; (3) interrogation authority normalization (coercive interrogation redefined as non-torture); (4) geographic entrapment (civilians cannot exit conflict zones); (5) definitional shifting (collateral damage acceptance reframes civilian harm as proportionate). All suppression mechanisms operate without independent oversight or meaningful constraint. Theater ratio (0.58): Moderate-high. The security-maximization reading performs legitimacy through legal categorization, interrogation protocol compliance, and proportionality documentation while systematically undermining the protective intent of those categories and protocols. The performative element has increased (0.42 → 0.52 → 0.58) as states have developed more sophisticated legal frameworks to justify practices that would previously have been characterized as violations. The increase reflects not functional improvement but theatrical legitimacy construction.
 *
 * PERSPECTIVAL GAP:
 *   The security-maximization reading produces maximum perspectival divergence. From the state security apparatus perspective (institutional/arbitrage), the reading appears as Rope — it coordinates effective counterinsurgency by reformulating humanitarian obligations as operational optimization problems. From detainee and civilian perspectives (powerless/trapped), the same constraint appears as pure Snare — all suppression, all extraction, no coordination benefit. The international humanitarian law regime sees itself as Piton — institutionally degraded, performing compliance through legal theater while the protective mechanism is inert. Monitoring and accountability bodies classify as Tangled Rope — they provide genuine coordination (maintaining humanitarian norms as reference frame) while inadvertently enabling state extraction through selective enforcement and legitimacy performance. The analytical observer's mountain perspective naturalizes what is actually a reading that benefits the state apparatus by presenting security-maximization logic as inevitable response to structural asymmetry. This false summit is revealed by the presence of identifiable beneficiaries (state apparatus, expanded interrogation authority) whose extraction depends on maintaining the reading's institutional embedding.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is computed from the structural relationship between each agent and the extraction flow. State security apparatus: beneficiary status + arbitrage exit options → low d (≈0.15) → negative or low χ. They experience the constraint as enabling rather than extractive. Detainees without status: victim status + trapped exit options → high d (≈0.92) → high f(d) ≈1.35 → high χ. They bear maximum extraction with no exit capacity. Civilian populations: victim status + trapped exit options → high d (≈0.92) → high χ. Trapped in conflict zones, they cannot exit the constraint. International humanitarian law regime: beneficiary (maintains institutional role) + victim (protective force degraded) → mixed directionality ≈0.50. Monitoring bodies: mixed (beneficiary through mandate renewal + victim through inability to stop violations) + constrained exit → moderate d ≈0.55 → moderate χ. The regime's piton classification derives from theater ratio (0.58) exceeding functional constraint — the constraint persists through institutional inertia and state compliance performance, not through effective protection.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED. The security-maximization reading resolves mandatrophy by showing that the constraint operates across two distinct institutional logics: (1) humanitarian logic (protections for all combatants and civilians regardless of status or reciprocity) and (2) security logic (protections conditional on operational necessity and state security assessment). The reading privileges security logic, subordinating humanitarian logic to it. This is not a failure of classification — it is a structural choice by the reading to reorganize priorities. The Snare classification from powerless perspectives and the Rope classification from state apparatus perspectives are both correct within their respective institutional logics. The mandatrophy resolves by acknowledging that the constraint does not collapse into inconsistency; rather, it operationalizes the power relationship: state security apparatus wins the definitional battle, subordinates humanitarian protections to operational necessity, and the constraint's function is to legitimize this subordination through legal theater. Mandatrophy resolution involves recognizing that the reading is not incoherent — it is coherent and successfully embedded — and therefore the classificatory divergence reflects real structural asymmetry, not analytical confusion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unlawful_combatant_category_definition,
    'What structural criteria actually distinguish ''unlawful combatants'' from POWs, and does the category function as neutral classification or as mechanism to deny protections?',
    'Comparative legal analysis of unlawful combatant definitions across states; correlation between expansions of category and actual differences in combatant conduct vs. deliberate denial of POW status; examination of whether the category has ever been applied to exclude state-aligned actors or only to exclude non-state actors from protections.',
    'If definition is neutral and applies symmetrically: category is a legitimate humanitarian classification. If definition expands selectively to deny non-state actors protections: the category functions as suppression mechanism masquerading as legal taxonomy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(unlawful_combatant_category_definition, empirical, 'Whether unlawful combatant category functions as neutral classification or suppression mechanism').

omega_variable(
    coercive_interrogation_torture_boundary,
    'What empirical and normative threshold distinguishes ''coercive interrogation'' from torture, and is the boundary maintained consistently across state practice?',
    'Comparative analysis of interrogation techniques authorized across states (water boarding, stress positions, sensory deprivation, isolation duration); temporal analysis of boundary shifts post-2001; correlation with ''ticking time bomb'' justifications; examination of whether the boundary has ever constrained state behavior or only shifted the definition to permit wider state behavior.',
    'If boundary is stable and constrains state practice: coercive interrogation is a limited exception to prohibition on torture. If boundary shifts to accommodate state practice: the category functions as definitional expansion enabling what would previously have been classified as torture.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coercive_interrogation_torture_boundary, empirical, 'Whether coercive interrogation boundary is stable or shifts to accommodate state practice').

omega_variable(
    collateral_damage_calculation_asymmetry,
    'Does collateral damage acceptance operate symmetrically (both state and non-state actors are held to equivalent proportionality standards), or does the standard apply asymmetrically (state permitted wider harm-to-civilian ratios than non-state actors)?',
    'Comparison of proportionality calculations applied to state military strikes vs. non-state actor attacks; examination of investigation thresholds and casualty estimates in each case; analysis of whether asymmetric harm ratios are justified by difference in actor capacity or by implicit different standards.',
    'If symmetric: collateral damage is neutral balancing of military advantage vs. civilian harm. If asymmetric: doctrine functions as extraction mechanism permitting greater state violence while constraining non-state actors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collateral_damage_calculation_asymmetry, empirical, 'Whether collateral damage proportionality standard is applied symmetrically').

omega_variable(
    humanitarian_regime_protective_force_empirical,
    'What is the actual protective force of Geneva Conventions obligations in asymmetric conflict — do states that ratify and invoke the conventions provide measurably greater protections than states that do not?',
    'Comparative analysis of civilian casualty rates, detention practices, interrogation methods, and unlawful-combatant prevalence across: states that ratify GCs and claim compliance; states that invoke security exceptions while ratifying; non-signatory states; correlation with conflict intensity and actor type (state vs. non-state).',
    'If ratification correlates with greater protections: regime has residual protective force despite security-maximization reading. If no correlation or inverse correlation: regime is performative, and security-maximization reading has successfully hollowed it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(humanitarian_regime_protective_force_empirical, empirical, 'Whether ratification of Geneva Conventions correlates with improved protection outcomes').

omega_variable(
    indefinite_detention_security_efficacy,
    'Do indefinite detention regimes (without trial, review, or release criteria) actually produce measurable security benefits that justify the suppression of habeas corpus and due process, or does the security gain primarily reflect prevention of political opposition and accountability?',
    'Comparative analysis of security outcomes (terrorist attacks, recidivism rates, destabilization reduction) in jurisdictions with indefinite detention vs. time-limited detention; examination of release criteria and outcomes when they exist; correlation between detention duration and actual threat assessment.',
    'If indefinite detention produces measurable security benefit: suppression is justified by security efficacy. If no measurable benefit or benefit is offset by destabilization costs: indefinite detention is pure extraction mechanism masquerading as security necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indefinite_detention_security_efficacy, empirical, 'Whether indefinite detention produces measurable security benefits or is primarily suppression mechanism').

omega_variable(
    humanitarian_ceiling_reading_foreclosure,
    'Does the security-maximization reading logically foreclose the humanitarian-ceiling reading (where Geneva Conventions set binding floors on state conduct that cannot be suspended regardless of military necessity), or do these readings coexist as competing interpretations?',
    'Examination of whether security-maximization core premise (operational necessity justifies suspending protections) directly contradicts humanitarian-ceiling core premise (protections are non-derogable minimums). Structural analysis of whether a single legal framework could hold both readings simultaneously.',
    'If forecloses: readings represent incompatible constitutional commitments; only one can be authoritative. If coexists: readings represent different institutional actors'' interpretations of the same text; both remain live.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(humanitarian_ceiling_reading_foreclosure, conceptual, 'Whether security-maximization reading forecloses humanitarian-ceiling reading').

omega_variable(
    conditional_reciprocity_reading_influence,
    'Does the security-maximization reading influence (create structural pressure on) the conditional-reciprocity reading (where humanitarian protections are conditional on adversary compliance), or do these readings operate independently?',
    'Examination of how security-maximization logic affects the conditions under which reciprocity is assessed; whether security-maximization justifications (operational necessity, asymmetric threat) shift the compliance baseline for evaluating reciprocity; whether expanded unlawful-combatant category affects which actors are held to reciprocity standards.',
    'If influences: conditional-reciprocity reading is downstream; security-maximization redefines when and to whom reciprocity applies. If independent: both readings can coexist without structural coupling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conditional_reciprocity_reading_influence, conceptual, 'Whether security-maximization reading creates structural pressure on conditional-reciprocity reading').

omega_variable(
    axiom_operational_necessity_overridden,
    'Has the foundational axiom of security-maximization reading (operational necessity overrides humanitarian floors) been formally overridden within its own tradition (e.g., by later protocol, convention, or state practice consensus), or does it remain holdable?',
    'Examination of post-1949 developments: Additional Protocols to Geneva Conventions (1977), Convention Against Torture (1984), case law from international courts, evolving state practice and ratification patterns, explicit repudiation of axiom by signatory states.',
    'If overridden: the reading''s foundational premise is no longer operative even within its own tradition. If holdable: the reading remains a live normative position.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(axiom_operational_necessity_overridden, empirical, 'Whether operational-necessity axiom has been formally overridden').

omega_variable(
    false_summit_candidate_analysis,
    'Is the security-maximization reading''s mountain perspective (analytical/civilizational view of structural inevitability) a genuine natural law of asymmetric conflict, or does it naturalize a contingent institutional arrangement that benefits the state security apparatus?',
    'Comparison with symmetric warfare case: do symmetric conflicts between uniformed state forces also show pressure to suspend humanitarian protections for operational necessity? Examination of whether asymmetry is the causal driver or whether security-maximization logic is applied post-hoc to justify state-preferred interpretations. Analysis of whether non-state actors show structural pressure toward opposite reading (humanitarian maximization under asymmetry).',
    'If natural law: security-maximization in asymmetric conflict is inevitable, and opposing readings are aspirational. If contingent: the ''inevitability'' framing is a false summit, and alternative readings (humanitarian ceiling, conditional reciprocity) are structurally viable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_candidate_analysis, conceptual, 'Whether security-maximization logic is structural inevitability or contingent institutional choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_1949__security_maximization_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gc1949_secmax_theater_2001, geneva_conventions_1949__security_maximization_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(gc1949_secmax_theater_2006, geneva_conventions_1949__security_maximization_reading, theater_ratio, 5, 0.52).
narrative_ontology:measurement(gc1949_secmax_theater_2011, geneva_conventions_1949__security_maximization_reading, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(gc1949_secmax_extract_2001, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(gc1949_secmax_extract_2006, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 5, 0.62).
narrative_ontology:measurement(gc1949_secmax_extract_2011, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 10, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(gc1949_secmax_supp_2001, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(gc1949_secmax_supp_2006, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 5, 0.7).
narrative_ontology:measurement(gc1949_secmax_supp_2011, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 10, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_1949__security_maximization_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(geneva_conventions_1949__security_maximization_reading, geneva_conventions_1949__humanitarian_ceiling_reading).
narrative_ontology:affects_constraint(geneva_conventions_1949__security_maximization_reading, geneva_conventions_1949__conditional_reciprocity_reading).
narrative_ontology:affects_constraint(geneva_conventions_1949__security_maximization_reading, indefinite_detention_authority).
narrative_ontology:affects_constraint(geneva_conventions_1949__security_maximization_reading, unlawful_combatant_category_expansion).
narrative_ontology:affects_constraint(geneva_conventions_1949__security_maximization_reading, interrogation_technique_redefinition).

% DUAL FORMULATION NOTE:
% The security-maximization reading is one decomposition of contested kernel geneva_conventions_1949. Its sibling readings (humanitarian_ceiling and conditional_reciprocity) are separate constraint stories with different extractiveness values, beneficiary/victim structures, and institutional embeddings. All three stories link via network.affects_constraints to show the kernel's decomposition. The upstream constraints (indefinite_detention_authority, unlawful_combatant_category_expansion, interrogation_technique_redefinition) are downstream implementations of this reading's core logic; they would not be institutionalized without the security-maximization interpretation framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(geneva_conventions_1949__security_maximization_reading, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
