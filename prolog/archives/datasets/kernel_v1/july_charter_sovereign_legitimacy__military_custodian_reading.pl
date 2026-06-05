% ============================================================================
% CONSTRAINT STORY: july_charter_sovereign_legitimacy__military_custodian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_july_charter_military_custodian, []).

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
 *   constraint_id: july_charter_sovereign_legitimacy__military_custodian_reading
 *   human_readable: July Charter: Military as Permanent Sovereign Guardian (Custodian Reading)
 *   domain: constitutional_law/political_transitions/post_revolutionary_state_building
 *
 * SUMMARY:
 *   The July Charter represents a post-revolutionary constitution that
 *   ratifies the military as a permanent institutional guardian ensuring
 *   stability. This constraint describes ONE READING of the contested kernel
 *   july_charter_sovereign_legitimacy—specifically, the reading that
 *   interprets the charter's operative clauses as institutionalizing military
 *   veto authority over civilian political contestation. The military
 *   custodian reading treats constitutional guardianship not as a temporary
 *   emergency measure but as a permanent structural feature: the military
 *   possesses guaranteed budget autonomy, constitutional authority to
 *   intervene if 'stability' is threatened, veto power over security and
 *   defense policy, and capacity to nullify electoral outcomes through
 *   constitutional mechanisms. Civilian parties retain electoral competition
 *   within boundaries defined by military-acceptable parameters; the student
 *   movement and broader political opposition face systematic suppression
 *   whenever mobilization threatens military-defined stability thresholds.
 *   The constraint exhibits dual extractive and coordinative functions
 *   (tangled rope at institutional perspectives) while appearing as pure
 *   snare (entrapment without coordination) to the powerless agents
 *   (opposition and students) and as false-summit natural law to the
 *   civilizational analytical observer. The measurement trajectory shows
 *   rising theater ratio (0.42 → 0.65) as the charter's legitimating
 *   ceremonies become increasingly performative and real contestation remains
 *   bounded—and rising suppression requirement (0.55 → 0.68) as enforcement
 *   intensity must increase to sustain the boundary between permitted and
 *   forbidden political activity.
 *
 * KEY AGENTS:
 *   - Military Institutional Apparatus: Primary beneficiary (institutional/arbitrage) — net recipient of extracted authority, budget autonomy, and policy veto. Has exit capacity but will not exercise it.
 *   - Security Technocracy: Secondary beneficiary (organized/constrained) — civilian bureaucrats gain technical authority over classified domains but subordinated to military direction.
 *   - Autonomous Political Parties: Primary victim (moderate/constrained) — experience mixed coordination (electoral procedures, party registration) and extraction (policy constraints, military veto thresholds). High cost to exit (dissolution, legal persecution).
 *   - Student Movement and Political Opposition: Primary victim (powerless/trapped) — face structural entrapment: mobilization triggers suppression, organizing outside approved channels results in arrest, contestation is permitted only within military guardrails.
 *   - Civilian Contestation Space (abstract): Victim collective (powerless/trapped) — the abstract good of autonomous political participation cannot organize or exit; bears full cost of boundary maintenance.
 *   - Charter Legitimating Ritual: Institutional performance (institutional/arbitrage) — the constitutional form performs stability through elections and parliamentary procedures while real authority remains concentrated in military hands. Theater ratio rises over time as the legitimacy frame absorbs challenge.
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the military guardianship as an immutable feature of post-revolutionary state formation (false-summit hazard).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__military_custodian_reading, 0.58).
domain_priors:suppression_score(july_charter_sovereign_legitimacy__military_custodian_reading, 0.68).
domain_priors:theater_ratio(july_charter_sovereign_legitimacy__military_custodian_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(july_charter_sovereign_legitimacy__military_custodian_reading, tangled_rope).
narrative_ontology:human_readable(july_charter_sovereign_legitimacy__military_custodian_reading, "July Charter: Military as Permanent Sovereign Guardian (Custodian Reading)").
narrative_ontology:topic_domain(july_charter_sovereign_legitimacy__military_custodian_reading, "constitutional_law/political_transitions/post_revolutionary_state_building").

domain_priors:requires_active_enforcement(july_charter_sovereign_legitimacy__military_custodian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(july_charter_sovereign_legitimacy__military_custodian_reading, '75451b2c-fe2b-4303-92f8-a45c79825bae').
narrative_ontology:cs_kernel_codification('75451b2c-fe2b-4303-92f8-a45c79825bae', formalized).
narrative_ontology:cs_authority_grounding('75451b2c-fe2b-4303-92f8-a45c79825bae', extraction).
narrative_ontology:cs_interpretation_layer_present('75451b2c-fe2b-4303-92f8-a45c79825bae').
narrative_ontology:cs_reading_relation('75451b2c-fe2b-4303-92f8-a45c79825bae', july_charter_sovereign_legitimacy__secular_democratic_reading, coexists_with).
narrative_ontology:cs_reading_relation('75451b2c-fe2b-4303-92f8-a45c79825bae', july_charter_sovereign_legitimacy__guided_nationalism_reading, influences).
narrative_ontology:cs_axiom('75451b2c-fe2b-4303-92f8-a45c79825bae', foundational, military_guardianship_permanent).
narrative_ontology:cs_axiom_status(military_guardianship_permanent, holdable).
narrative_ontology:cs_axiom_grounding('75451b2c-fe2b-4303-92f8-a45c79825bae', military_guardianship_permanent, conventional).
narrative_ontology:cs_axiom('75451b2c-fe2b-4303-92f8-a45c79825bae', foundational, civilian_subordination_to_security_veto).
narrative_ontology:cs_axiom_status(civilian_subordination_to_security_veto, holdable).
narrative_ontology:cs_axiom_grounding('75451b2c-fe2b-4303-92f8-a45c79825bae', civilian_subordination_to_security_veto, instrumental).
narrative_ontology:cs_reference_frame('75451b2c-fe2b-4303-92f8-a45c79825bae', military_permanent_guardianship).
narrative_ontology:cs_drift_state('75451b2c-fe2b-4303-92f8-a45c79825bae', contemporary_democratic_pressure, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('75451b2c-fe2b-4303-92f8-a45c79825bae', '').
narrative_ontology:cs_kernel_id(july_charter_sovereign_legitimacy__military_custodian_reading, july_charter_sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__military_custodian_reading, military_institutional_apparatus).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__military_custodian_reading, security_technocracy).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__military_custodian_reading, autonomous_political_parties).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__military_custodian_reading, student_movement).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__military_custodian_reading, civilian_contestation_space).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STUDENT MOVEMENT & OPPOSITION (SNARE) — Faces structural entrapment: mobilization triggers security apparatus response; electoral competition is bounded by military veto thresholds; organizing outside approved channels results in suppression. The constitutional framing (charter legitimacy) masks the extraction mechanism: civilian political contestation is permitted only within military-defined boundaries. Maximum experienced extraction for the powerless — no structural exit without regime change.
constraint_indexing:constraint_classification(july_charter_sovereign_legitimacy__military_custodian_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: AUTONOMOUS POLITICAL PARTIES (TANGLED ROPE) — Experience genuine coordination function (charter establishes electoral schedule, procedural rules enabling organized competition) alongside asymmetric extraction (military veto on security policy, defense budget autonomy, ability to nullify electoral outcomes via institutional intervention). Parties benefit from the constitutional framework's stability and predictability; simultaneously constrained by military guardrails. High cost to exit (dissolution, legal persecution) but some agency and some coordination benefit.
constraint_indexing:constraint_classification(july_charter_sovereign_legitimacy__military_custodian_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MILITARY INSTITUTIONAL APPARATUS (ROPE) — Net beneficiary. The charter ratifies permanent institutional guardianship: guaranteed budget autonomy, veto authority over security policy, constitutional right to intervene if 'stability' is threatened, control over defense appointments, and capacity to restrict civilian access to security information. The military frames this as coordination—ensuring stability against chaos—but experiences it as pure benefit extraction. The apparatus has exit capacity (could subordinate to civilian control) but will not exercise it because extraction is maximal.
constraint_indexing:constraint_classification(july_charter_sovereign_legitimacy__military_custodian_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: SECURITY TECHNOCRACY (TANGLED ROPE) — Civilian bureaucrats embedded in the security apparatus experience mixed extraction and coordination. The military-guardian framing legitimizes their technical authority over classified policy domains; it also subordinates civilian bureaucratic autonomy to military direction. Organized but constrained—they have some technical agency but reduced scope for independent policy judgment.
constraint_indexing:constraint_classification(july_charter_sovereign_legitimacy__military_custodian_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: THE CHARTER'S LEGITIMATING RITUAL (PITON) — The constitutional form persists through institutional inertia. The charter's preamble invokes democratic aspirations and rule of law, but the operative clauses institutionalize military veto. Over time, the theater ratio rises (annual compliance ceremonies, periodic elections that change no fundamental structure, parliamentary debates over constrained issues) while the actual coordination function degrades (parties learn their real authority is decorative). The constraint is maintained because the legitimacy frame—'constitutional guardian ensuring stability'—absorbs challenges that would otherwise target the military directly. If the frame broke, the underlying extraction would be undeniable.
constraint_indexing:constraint_classification(july_charter_sovereign_legitimacy__military_custodian_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / CIVILIZATIONAL STABILITY (MOUNTAIN) — From a civilizational vantage, military-led transitions from revolutionary chaos to stable constitutional order can appear as inevitable or naturally emergent. The logic: post-revolutionary fragmentation creates a vacuum; only the military apparatus has the organizational capacity and legitimacy to enforce order; therefore military guardianship is a natural law of state formation. This perspective risks false summit classification—treating contingent institutional choice as immutable structural necessity. The military's permanent guardianship looks like a civilizational constant but is actually the reading's own construction.
constraint_indexing:constraint_classification(july_charter_sovereign_legitimacy__military_custodian_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(july_charter_sovereign_legitimacy__military_custodian_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(july_charter_sovereign_legitimacy__military_custodian_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(july_charter_sovereign_legitimacy__military_custodian_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__military_custodian_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(july_charter_sovereign_legitimacy__military_custodian_reading, TR),
    TR >= 0.70.

:- end_tests(july_charter_sovereign_legitimacy__military_custodian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. The military apparatus captures substantial institutional benefit—guaranteed budget autonomy (typically 3-5% of GDP immune from civilian budget review), veto power over security policy and defense appointments, constitutional right to intervene if 'stability' is threatened, control over security information classification. However, the extraction is not maximal (snare range ≥0.66) because the constraint does provide genuine coordination goods to political parties: predictable electoral schedules, procedural rules for competition, institutional stability against revolutionary fragmentation. The tangled_rope classification reflects this hybrid: real coordination function coexists with asymmetric extraction. Suppression (0.68): High. The constraint maintains its extraction through high coercive overhead: security apparatus deployment against opposition mobilization, classification barriers to civilian oversight, legal restrictions on party platforms and organizing, credible threat of institutional intervention (coup capacity). The trajectory from 0.55 to 0.68 reflects that as democratic norms strengthen and contestation pressure increases, enforcement must intensify to sustain the boundary—this is a sign that the constraint is being actively maintained through coercion rather than natural evolution. Theater Ratio (0.65): High and rising. The charter establishes electoral ceremonies, parliamentary proceedings, and constitutional review rituals that perform democratic legitimacy while real authority remains concentrated in military hands. Over 16 years, as parties and opposition learn that their electoral mandates are subject to military override, the theater ratio rises: ceremonies become more elaborate precisely because they are less meaningful. The rising theater trajectory (0.42 → 0.65) is a piton-range indicator suggesting that the charter's legitimating function is degrading—the form persists but the substance is increasingly recognized as performative.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximal perspectival divergence. The military apparatus sees the charter as a coordination mechanism ensuring stability (rope classification)—from their position as beneficiary with arbitrage exit, the constraint genuinely coordinates state action and enables their preferred policy outcomes. Autonomous political parties occupy the middle ground, experiencing both coordination benefits (electoral procedures, constitutional framework providing predictability) and extraction costs (policy vetoes, military override capacity), classifying as tangled_rope. The student movement and political opposition experience the constraint as pure snare—no coordination benefits reach them, only systematic suppression of their political agency. The analytical observer risks the false-summit trap, seeing military guardianship as an immutable feature of post-revolutionary state formation rather than a contingent institutional choice that the military custodian reading instantiates. The perspectival gap reveals that the reading's coherence depends on which position you occupy: the military's narrative (guardianship ensures stability) is true from their vantage but masks the extraction experienced by others.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is derived from structural position: beneficiary status + exit capacity → low d → negative/low chi. Victim status + constrained/trapped exit → high d → high chi. The military institutional apparatus: beneficiary status + arbitrage exit → d ≈ 0.10 → f(d) ≈ -0.10 (negative chi, experiences low extraction). Political parties: victim status (they bear policy constraints) + constrained exit (party dissolution or persecution is costly but possible) + moderate power → d ≈ 0.55 → f(d) ≈ 0.75 (moderate-high chi). Opposition/students: victim status + trapped exit + powerless position → d ≈ 0.90 → f(d) ≈ 1.28 (maximum chi). The asymmetry in directionality is the mechanism that makes this reading's extraction stick.
 *
 * MANDATROPHY ANALYSIS:
 *   The military custodian reading resolves its mandatrophy by embracing the tangled_rope classification: the constraint does provide genuine coordination function (electoral procedures, constitutional stability, state capacity) alongside asymmetric extraction (military veto, budget autonomy, suppression capacity). The mandatrophy risk would be: Is this coordination masking pure extraction (snare), or is the coordination real? The omega variables directly address this—the stability_counterfactual omega tests whether military guardianship actually produces greater stability than alternative arrangements, and the victim_coalition_countervailing_power omega tests whether the extraction is robust or dependent on fragmentation. The rising theater_ratio trajectory (0.42 → 0.65) is the mandatrophy signal: if the coordination function were genuinely being performed, theater ratio should remain stable or decline (function becomes clearer); rising theater indicates the legitimacy frame is absorbing challenge while real authority remains concentrated, suggesting that the reading's own framing is becoming increasingly performative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    guardian_necessity_contingency,
    'Is permanent military guardianship a necessary structural feature of post-revolutionary state stabilization, or a contingent institutional choice that benefits the military apparatus?',
    'Comparative institutional analysis: examine post-revolutionary transitions (Portugal 1974, Spain 1978, South Korea 1987, Chile 1990) that achieved stable democratic orders without permanent military veto authority. If democratic stabilization is achievable without guardianship, the necessity claim is refuted.',
    'If contingent: the military custodian reading is false-summit classification—naturalizing choice as law. If necessary: the mountain perspective holds and the reading''s extraction ratios are understated (actual suppression may be lower if the constraint is genuinely protective rather than coercive).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(guardian_necessity_contingency, empirical, 'Whether permanent military guardianship is necessary or contingent').

omega_variable(
    stability_counterfactual,
    'Does the military-guardian framing actually produce greater stability than alternative institutional arrangements (civilian supreme court with constitutional review, parliamentary supermajority requirements, proportional representation), or does it merely claim stability while extracting from political contestation?',
    'Longitudinal institutional stability metrics: coup frequency, constitutional amendment success, electoral volatility, inter-branch conflict intensity, before/after the charter''s ratification. Compare against peer democracies without permanent military veto.',
    'If military guardianship correlates with higher stability: the tangled_rope classification is correct (genuine coordination function exists alongside extraction). If stability is similar to peer democracies: the constraint is pure extraction rationalized as protection (snare classification correct).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stability_counterfactual, empirical, 'Whether military guardianship produces greater institutional stability').

omega_variable(
    reading_foreclosure_structure,
    'Does the military custodian reading logically foreclose the secular democratic reading, or do both readings remain live political positions held by different institutional factions?',
    'Institutional game theory analysis: can a single coherent framework (the charter text) be interpreted as authorizing both military guardianship AND subordination of military to civilian control? If yes, the readings coexist (neither forecloses). If no, one reading''s core premise directly contradicts the other''s.',
    'If foreclosed: the engine classifies the democratic reading as structurally impossible within this constitutional framework. If coexists: the battlefield is over charter interpretation, not over logical possibility.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_structure, conceptual, 'Whether military custodian reading logically forecloses democratic reading').

omega_variable(
    charter_text_ambiguity_site,
    'What specific charter clauses anchor the military custodian reading, and would alternative phrasings enable the democratic reading to claim equal constitutional warrant?',
    'Close textual analysis: identify the key clauses (security council composition, emergency powers, budget autonomy, appointment authority) that enable military veto. Model counterfactual charter texts with alternative phrasings (e.g., ''military subordinate to elected civilian defense minister,'' ''security council advisory only''). If counterfactuals produce equally coherent readings, the charter''s ambiguity is structural, not accidental.',
    'If ambiguity is structural: the charter is a genuine kernel allowing multiple readings. If the text is asymmetrically favoring military guardianship: the democratic reading claims equal warrant only through radical reinterpretation, weakening its institutional hold.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(charter_text_ambiguity_site, conceptual, 'Location of charter text enabling military custodian reading').

omega_variable(
    victim_coalition_countervailing_power,
    'Could autonomous political parties and the student movement, if unified, generate sufficient countervailing power to shift the military-custodian equilibrium toward the democratic reading?',
    'Institutional capacity analysis: do the victims have organizational resources (mass mobilization capacity, international support, economic leverage) to credibly threaten the military''s institutional position? Track historical coalition attempts and their outcomes.',
    'If countervailing power exists: the extraction mechanism depends on victim organizational fragmentation (suppression is artificially high). If victims lack countervailing power: the extraction is robust to coalitional pressure (suppression reflects structural asymmetry).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_coalition_countervailing_power, empirical, 'Whether victim coalition could generate countervailing power').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(july_charter_sovereign_legitimacy__military_custodian_reading, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(july_mil_theater_t0, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(july_mil_theater_t8, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 8, 0.58).
narrative_ontology:measurement(july_mil_theater_t16, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 16, 0.65).

% Extraction over time
narrative_ontology:measurement(july_mil_extract_t0, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(july_mil_extract_t8, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 8, 0.54).
narrative_ontology:measurement(july_mil_extract_t16, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 16, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(july_mil_suppress_t0, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(july_mil_suppress_t8, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 8, 0.62).
narrative_ontology:measurement(july_mil_suppress_t16, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 16, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(july_charter_sovereign_legitimacy__military_custodian_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(july_charter_sovereign_legitimacy__military_custodian_reading, 0.12).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__military_custodian_reading, july_charter_sovereign_legitimacy__secular_democratic_reading).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__military_custodian_reading, july_charter_sovereign_legitimacy__guided_nationalism_reading).

% DUAL FORMULATION NOTE:
% The military custodian reading is one of three competing interpretations of the July Charter kernel (july_charter_sovereign_legitimacy). Each reading instantiates a distinct constraint with its own extractiveness value, victim set, and classification profile. The three readings are linked by network.affects_constraints because they share a common kernel text but produce different structural outcomes. The military custodian reading treats the charter's guardian provisions as permanent institutional design; the democratic reading treats them as emergency measures subject to civilian override; the guided_nationalism reading treats them as temporary constraints on democracy in service of national development. Each story has its own ε, perspectives, and measurements. The readings do not reduce to different observer positions on a single constraint—they are structurally distinct constraints grounded in alternative interpretations of the same contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
