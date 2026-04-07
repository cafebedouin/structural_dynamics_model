% ============================================================================
% CONSTRAINT STORY: martial_law_precedent
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_martial_law_precedent, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: martial_law_precedent
 *   human_readable: Martial Law Precedent as Extractive State Power
 *   domain: political/constitutional/governance
 *
 * SUMMARY:
 *   Martial law represents a structural mechanism by which states suspend
 *   normal constitutional constraints on executive power, ostensibly to
 *   respond to emergency threats. The constraint operates across multiple
 *   institutional levels: the executive branch gains unilateral authority,
 *   the civilian population loses civil liberties, the judiciary transitions
 *   from independent review to deference, and political opposition becomes
 *   criminalized. The extractiveness of martial law increases over its
 *   duration as emergency powers become institutionalized — initial
 *   suppression (checkpoints, curfews, arrests) is followed by deeper
 *   extraction (asset seizure, forced collaboration, normalization of
 *   surveillance). The theater_ratio rises as courts and legislatures
 *   maintain formal review mechanisms while substantively enabling executive
 *   actions through deference doctrines. The key structural ambiguity is
 *   whether martial law represents an immutable feature of state sovereignty
 *   (necessary response to existential threat, impossible to foreclose) or a
 *   contingent institutional choice (one governance option among
 *   alternatives, adopted for extractive political purposes). The empirical
 *   pattern suggests the latter: most states that declare martial law face
 *   non-existential crises; those that do face existential threats often do
 *   so without martial law; and martial law tends to be renewed and extended
 *   beyond the stated emergency period.
 *
 * KEY AGENTS:
 *   - Civilian Population: Primary victim (powerless/trapped) — territorial jurisdiction locks them in; martial law suspends civil liberties with no exit option.
 *   - Constitutional Legal Order: Secondary victim (powerless/identity_locked) — judges and legislators have fused their professional identity with executive emergency powers and state necessity narratives; could reassert constraint authority but have internalized emergency reasoning.
 *   - Opposition Political Actors: Secondary victim (moderate/constrained) — can exit via exile but at catastrophic cost; suppressed through harassment, arrest, detention.
 *   - Executive Branch and Security Apparatus: Primary beneficiary (institutional/arbitrage) — gains concentration of power, expanded resources, freedom from normal constraints; experiences martial law as pure coordination benefit.
 *   - Judicial System: Institutional actor (institutional/constrained) — maintains appearance of review authority (theater) while exercising deference to executive judgment; caught between constitutional role and institutional survival pressure.
 *   - International Observing States: Secondary institutional actor (organized/mobile) — possess leverage but face coordination costs; both condemn martial law (norm enforcement) and sometimes enable it (for geopolitical allies).
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangements as immutable laws of political necessity.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(martial_law_precedent, 0.58).
domain_priors:suppression_score(martial_law_precedent, 0.72).
domain_priors:theater_ratio(martial_law_precedent, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(martial_law_precedent, extractiveness, 0.58).
narrative_ontology:constraint_metric(martial_law_precedent, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(martial_law_precedent, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(martial_law_precedent, snare).
narrative_ontology:human_readable(martial_law_precedent, "Martial Law Precedent as Extractive State Power").
narrative_ontology:topic_domain(martial_law_precedent, "political/constitutional/governance").

domain_priors:requires_active_enforcement(martial_law_precedent).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(martial_law_precedent, executive_branch).
narrative_ontology:constraint_victim(martial_law_precedent, civilian_population).
narrative_ontology:constraint_victim(martial_law_precedent, constitutional_constraints).
narrative_ontology:constraint_victim(martial_law_precedent, judicial_independence).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CIVILIAN POPULATION (SNARE) — No exit from territorial jurisdiction; rights suspended with minimal judicial oversight. Suppression absolute: curfews, movement restrictions, armed enforcement. High extraction: property seizure, forced labor, surveillance normalized. The constraint persists through coercive apparatus alone — civilians cannot organize or appeal while constrained.
constraint_indexing:constraint_classification(martial_law_precedent, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CONSTITUTIONAL CONSTRAINTS (IDENTITY_LOCKED) — The legal order has internalized emergency powers as normal governance tools. Constitutional provisions that once bounded martial law are reinterpreted as enabling rather than limiting. Judges, legislators, and administrators have become identity-locked to the expedience of executive emergency powers — exit would require abandoning the institutional identity they have constructed within the emergency framework. Structurally mobile (judges could reassert independence) but perceptually trapped by their professional identity fusion with state necessity narratives.
constraint_indexing:constraint_classification(martial_law_precedent, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 3: OPPOSITION POLITICAL ACTORS (SNARE) — Can exit through exile or silence, but at severe cost: career destruction, family separation, personal safety. Suppression through arrest, detention, harassment of supporters. High extraction: ability to organize opposition is disabled. Not fully trapped (exit exists via departure) but constrained by impossible cost-benefit. Effective extraction is high — political voice is the extracted commodity.
constraint_indexing:constraint_classification(martial_law_precedent, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: INTERNATIONAL ACTORS (TANGLED ROPE) — Possess mobility (can apply sanctions, withdraw recognition, deny trade benefits) but face coordination costs. Genuine coordination function exists: establishing international norms against indefinite martial law, enforcing reciprocal accountability standards. But also asymmetric extraction: powerful states use martial law accusations to justify intervention; weaker states use international concern to build domestic legitimacy against local opposition. Mixed: both coordination and extraction present in the international system's relationship to the martial law constraint.
constraint_indexing:constraint_classification(martial_law_precedent, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: EXECUTIVE BRANCH (ROPE) — Experiences martial law as a coordination mechanism: centralizing decision-making, enabling rapid response to perceived threats, establishing clear command authority. Enormous beneficiary of the constraint — gains power and resources. Suppression is the mechanism through which they solve their coordination problem (unified state action). Exit is available (they can relinquish emergency powers) but costless to maintain. Net beneficiary perspective classifies as rope: for the executive, this is pure coordination benefit.
constraint_indexing:constraint_classification(martial_law_precedent, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: JUDICIAL SYSTEM (PITON) — Formally retains review authority over executive actions but exercises it rarely or deferentially ('clear and present danger' standard collapses into executive judgment). Theater_ratio high: courts issue rulings that appear to constrain martial law while substantively enabling it through deference doctrines. The judicial review mechanism persists as a constitutional ritual, maintaining legitimacy theater while failing functional constraint. Extraction has degraded from structural limitation (in early constitutional period) to performance (in long-emergency context). Piton: former rope that has atrophied into institutional theater.
constraint_indexing:constraint_classification(martial_law_precedent, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL/NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, martial law appears as an immutable feature of state sovereignty: in moments of existential threat, the state must centralize power and suspend normal constraints to survive. This is presented as a law of political nature — necessity overrides law, and no constitutional order can foreclose this. However, this perspective misses the empirical reality: most martial law events are not existential threats but tools of political suppression. The accessibility_collapse metric (how easy is it to avoid the constraint?) is actually low — most states never deploy martial law despite facing real emergencies. The mountain reading is a false summit that naturalizes a contingent institutional choice.
constraint_indexing:constraint_classification(martial_law_precedent, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(martial_law_precedent_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(martial_law_precedent, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(martial_law_precedent, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(martial_law_precedent, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(martial_law_precedent, TR),
    TR >= 0.70.

:- end_tests(martial_law_precedent_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Martial law extracts civilian liberties, property rights, and political voice through coercive apparatus. The metric reflects that extraction increases over time as emergency powers become institutionalized and renewal cycles normalize indefinite suspension. The initial value (0.35) reflects lower extraction in the early emergency phase; the progression to 0.58 reflects institutional capture of judicial and legislative constraints that would normally bound executive authority. Suppression (0.72): Very high. Martial law is defined by suspension of normal civil protections: curfews, movement restrictions, arrest without warrant, detention without trial, asset seizure. The suppression is enforced through military/security apparatus with monopoly on legitimate violence. No formal exit mechanism exists for civilians; opposition actors can exit only through exile at catastrophic cost. Theater ratio (0.65): Moderate-high. Courts and legislatures maintain formal appearances of oversight while deferring substantively to executive emergency judgments. Constitutional provisions that limit executive power are reinterpreted as enabling it. The theater increases over time (0.42 → 0.65) as institutional actors invest more effort in maintaining legitimacy theater while permitting power expansion.
 *
 * PERSPECTIVAL GAP:
 *   The executive branch perceives coordination (rope) — martial law solves the coordination problem of centralizing state action during perceived threat. The civilian population perceives pure extraction (snare) — their liberties are suspended with no corresponding benefit. Constitutional institutions perceive identity fusion with emergency necessity (identity_locked) — judges and legislators have internalized emergency reasoning as professional obligation, making exit from that framing impossible without abandoning their institutional role. The international observer perceives mixed coordination and extraction (tangled rope) — some legitimate norm-building against unlimited martial law, but also opportunistic use by both accusers and defenders. The judicial system perceives degraded constraint mechanisms (piton) — their review authority persists as theater while losing functional power. The civilizational analytical observer risks perceiving necessity (mountain) — that martial law is an immutable feature of state sovereignty — but the structural data contradicts this: accessibility_collapse is actually low (most states avoid martial law despite crises), resistance is not irreducible (constitutional alternatives exist), and it does not emerge naturally (it requires active political choice and institutional coordination).
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value derives from its structural position relative to extraction flows. The executive branch is a primary beneficiary (d ≈ 0.05, derived from institutional power + arbitrage exit + beneficiary status) — experiences minimal or negative effective extraction because they are the extraction mechanism itself. The civilian population is a primary victim with no exit (d ≈ 0.95, derived from powerless status + trapped exit + victim classification) — experiences maximum effective extraction, f(d) ≈ 1.42. Constitutional institutions have moderate-high directionality (d ≈ 0.70) despite institutional power, because they are victims of institutional capture — their identity has fused with emergency necessity narratives, reducing their perceived ability to exit. Opposition actors have high directionality (d ≈ 0.85) — political actors with theoretical exit options but facing extreme cost barriers. International actors have moderate directionality (d ≈ 0.55) due to organized power and mobile exit options, despite being structurally positioned as victims of authority violations.
 *
 * MANDATROPHY ANALYSIS:
 *   The martial law constraint resolves mandatrophy by identifying that the 'coordination' function from the executive perspective is actually unilateral centralization (not genuine coordination requiring distributed consent). The executive's rope classification is a beneficiary's view — from the civilian or constitutional perspective, the same mechanism is pure extraction. The piton classification correctly identifies that judicial review has become theatrical: courts maintain the appearance of constraint while enabling expansion. The identity_locked classification on constitutional institutions reveals the capture mechanism — it is not that judges lack the formal power to constrain executive action, but that they have fused their professional identity with emergency necessity narratives. The snare classification is correct from the victim perspective: extraction without coordination benefit, enforced through suppression alone. The mountain reading (natural law of state necessity) is false: martial law is neither immutable nor universal — it is a contingent institutional choice, adopted more frequently for political suppression than for genuine existential threats.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threat_severity_measurement,
    'What objective metric distinguishes an existential state threat that legitimates martial law from political suppression disguised as emergency?',
    'Comparative analysis of threat severity (casualty rates, territorial loss, economic collapse metrics) at declaration vs actual outcomes. Cross-national correlation between threat severity and duration/scope of martial law.',
    'If clear threshold exists: some martial law events legitimately belong in mountain (immutable response to necessity). If no clear threshold: all cases are contingent political choices, constraining mountain classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(threat_severity_measurement, empirical, 'Objective threshold for distinguishing existential threats from political suppression').

omega_variable(
    constitutional_recovery_possibility,
    'After martial law ends, can constitutional constraints be fully restored or do they remain permanently degraded?',
    'Post-martial law institutional analysis: tracking judges'' willingness to reassert independence, executive compliance with restored legal boundaries, reversal of identity-locked institutional norms.',
    'If recovery is possible: identity-lock is perspectival (judges could shift frames). If impossible: identity-lock is structural (institutional identity has permanently fused with emergency powers). Affects whether the constraint persists post-termination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_recovery_possibility, empirical, 'Whether constitutional constraints can recover after martial law ends').

omega_variable(
    temporal_asymmetry_extraction,
    'Does the extraction mechanism rely on the temporary nature of martial law (short duration credibly expected) or on indefinite/perpetual extension?',
    'Measurement of actual duration vs declared duration; analysis of renewal mechanisms; tracking whether suppression intensifies over time or plateaus.',
    'If extraction depends on apparent temporality: lifting martial law technically terminates it. If extraction has shifted to indefinite extension mechanisms: the constraint has evolved into a permanent form and the snare classification is more severe.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(temporal_asymmetry_extraction, empirical, 'Whether extraction depends on perceived temporality or indefinite perpetuation').

omega_variable(
    institutional_norm_sedimentation,
    'How much does executive power expansion during martial law become institutionally normalized even after formal termination?',
    'Tracking of laws enacted under martial law that persist after termination; executive discretionary power expansion; changes in judicial deference standards that survive the emergency.',
    'High sedimentation: martial law becomes a ratchet mechanism, permanently expanding executive power. Low sedimentation: truly temporary. Affects whether the constraint''s true extractiveness persists masked under formal restoration.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_norm_sedimentation, empirical, 'Institutional norm persistence after martial law formally ends').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(martial_law_precedent, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(martial_tr_t0, martial_law_precedent, theater_ratio, 0, 0.42).
narrative_ontology:measurement(martial_tr_t2, martial_law_precedent, theater_ratio, 2, 0.55).
narrative_ontology:measurement(martial_tr_t5, martial_law_precedent, theater_ratio, 5, 0.65).
narrative_ontology:measurement(martial_tr_t8, martial_law_precedent, theater_ratio, 8, 0.71).

% Extraction over time
narrative_ontology:measurement(martial_be_t0, martial_law_precedent, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(martial_be_t2, martial_law_precedent, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(martial_be_t5, martial_law_precedent, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(martial_be_t8, martial_law_precedent, base_extractiveness, 8, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(martial_law_precedent, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(martial_law_precedent, 0.12).
narrative_ontology:affects_constraint(martial_law_precedent, constitutional_emergency_provisions).
narrative_ontology:affects_constraint(martial_law_precedent, judicial_deference_doctrines).
narrative_ontology:affects_constraint(martial_law_precedent, surveillance_normalization).

% DUAL FORMULATION NOTE:
% Martial law precedent is downstream of specific constitutional emergency provisions (which permit declaration) and upstream of judicial deference doctrines and surveillance normalization (which enable persistence). The precedent constraint captures how historical uses of martial law establish institutional norms that reduce barriers to future declarations and expand scope of accepted suppression.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(martial_law_precedent, institutional, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
