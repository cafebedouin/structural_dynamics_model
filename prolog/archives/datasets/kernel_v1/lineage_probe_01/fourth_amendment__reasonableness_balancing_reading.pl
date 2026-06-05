% ============================================================================
% CONSTRAINT STORY: fourth_amendment__reasonableness_balancing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fourth_amendment__reasonableness_balancing_reading, []).

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
 *   constraint_id: fourth_amendment__reasonableness_balancing_reading
 *   human_readable: Fourth Amendment Reasonableness Balancing (Over Warrant Preference)
 *   domain: constitutional_law/fourth_amendment
 *
 * SUMMARY:
 *   The reasonableness balancing reading of the Fourth Amendment posits that
 *   the Amendment's ultimate touchstone is the reasonableness of a search,
 *   not the presence of a warrant. Under this reading, the Warrant Clause
 *   governs when warrants are issued and used, but reasonableness—a flexible
 *   balancing of government interests against individual privacy
 *   expectations—is the constitutional test. This reading enables law
 *   enforcement to conduct warrantless searches in 'special needs' contexts
 *   (school safety, drug testing, DUI checkpoints, administrative
 *   inspections) where the government's interest outweighs privacy
 *   expectations. The constraint operates as a doctrinal framework that
 *   extracts asymmetric operational authority from individual privacy
 *   protections while maintaining the appearance of constitutional guardrails
 *   through structured balancing. It coexists—precariously—with the
 *   warrant-preference reading, which holds that the two clauses are linked:
 *   searches are presumptively unreasonable without a warrant, and exceptions
 *   must be jealously confined. The extractiveness has accumulated over 40
 *   years (1985–2025) as the special-needs exception categories have expanded
 *   and lower courts have applied balancing doctrine with increasing
 *   flexibility. Theater ratio has risen from moderate to moderate-high as
 *   the warrant clause persists as textual law while its operative force has
 *   been displaced by balancing doctrine.
 *
 * KEY AGENTS:
 *   - Government Law Enforcement and Security Agencies: Primary beneficiary (institutional/arbitrage) — gain operational flexibility, warrantless search authority, special-needs exception deployment. Extract asymmetric access to searches that warrant doctrine would restrict.
 *   - Subjects of Warrantless Searches (especially in special-needs contexts): Primary victim (powerless/trapped) — face intrusions authorized by balancing doctrine that warrant preference would likely prohibit. Cannot exit jurisdiction or avoid search contexts.
 *   - Privacy-Maximalist Legal Formalists (academics, dissenting judges, ACLU): Secondary victim (moderate/constrained) — suffer professional and institutional penalties for articulating warrant-preference doctrine. Constrained from fully resisting doctrinal capture.
 *   - Civil Liberties Organizations: Organized agent (organized/mobile) — have litigation and political voice but benefit from balancing framework's existence as a contestable doctrine.
 *   - Supreme Court (as institutional authority): Institutional holder of doctrinal authority (institutional/arbitrage) — benefits from flexibility and discretion afforded by balancing over warrant-preference formalism. Can arbitrage between balancing and warrant preference across different doctrinal contexts.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fourth_amendment__reasonableness_balancing_reading, 0.58).
domain_priors:suppression_score(fourth_amendment__reasonableness_balancing_reading, 0.35).
domain_priors:theater_ratio(fourth_amendment__reasonableness_balancing_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fourth_amendment__reasonableness_balancing_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(fourth_amendment__reasonableness_balancing_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(fourth_amendment__reasonableness_balancing_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fourth_amendment__reasonableness_balancing_reading, tangled_rope).
narrative_ontology:human_readable(fourth_amendment__reasonableness_balancing_reading, "Fourth Amendment Reasonableness Balancing (Over Warrant Preference)").
narrative_ontology:topic_domain(fourth_amendment__reasonableness_balancing_reading, "constitutional_law/fourth_amendment").

domain_priors:requires_active_enforcement(fourth_amendment__reasonableness_balancing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fourth_amendment__reasonableness_balancing_reading, 'aa86da88-08ec-408a-802c-b318b0ed525f').
narrative_ontology:cs_kernel_codification('aa86da88-08ec-408a-802c-b318b0ed525f', formalized).
narrative_ontology:cs_authority_grounding('aa86da88-08ec-408a-802c-b318b0ed525f', lineage).
narrative_ontology:cs_interpretation_layer_present('aa86da88-08ec-408a-802c-b318b0ed525f').
narrative_ontology:cs_reading_relation('aa86da88-08ec-408a-802c-b318b0ed525f', fourth_amendment__warrant_preference_reading, coexists_with).
narrative_ontology:cs_axiom('aa86da88-08ec-408a-802c-b318b0ed525f', foundational, reasonableness_is_ultimate_touchstone).
narrative_ontology:cs_axiom_status(reasonableness_is_ultimate_touchstone, holdable).
narrative_ontology:cs_axiom_grounding('aa86da88-08ec-408a-802c-b318b0ed525f', reasonableness_is_ultimate_touchstone, deontological).
narrative_ontology:cs_axiom('aa86da88-08ec-408a-802c-b318b0ed525f', foundational, special_needs_context_exception_validity).
narrative_ontology:cs_axiom_status(special_needs_context_exception_validity, holdable).
narrative_ontology:cs_axiom_grounding('aa86da88-08ec-408a-802c-b318b0ed525f', special_needs_context_exception_validity, deontological).
narrative_ontology:cs_reference_frame('aa86da88-08ec-408a-802c-b318b0ed525f', flexible_reasonableness_standard).
narrative_ontology:cs_drift_state('aa86da88-08ec-408a-802c-b318b0ed525f', contemporary_doctrinal_state, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('aa86da88-08ec-408a-802c-b318b0ed525f', '').
narrative_ontology:cs_kernel_id(fourth_amendment__reasonableness_balancing_reading, fourth_amendment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fourth_amendment__reasonableness_balancing_reading, government_special_needs_regimes).
narrative_ontology:constraint_beneficiary(fourth_amendment__reasonableness_balancing_reading, administrative_search_authority).
narrative_ontology:constraint_beneficiary(fourth_amendment__reasonableness_balancing_reading, law_enforcement_flexibility).
narrative_ontology:constraint_victim(fourth_amendment__reasonableness_balancing_reading, warrant_preference_formalists).
narrative_ontology:constraint_victim(fourth_amendment__reasonableness_balancing_reading, privacy_maximalists).
narrative_ontology:constraint_victim(fourth_amendment__reasonableness_balancing_reading, subjects_of_warrantless_searches).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WARRANTLESS SEARCH SUBJECT (SNARE) — Cannot exit jurisdiction or refuse searches. No meaningful alternative to submission. Bears full extraction cost as the balancing doctrine permits intrusions that warrant doctrine would prohibit. The reasonableness balancing framework legitimizes warrantless searches in special-needs contexts, creating structural suppression of exit alternatives.
constraint_indexing:constraint_classification(fourth_amendment__reasonableness_balancing_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PRIVACY-MAXIMALIST FORMALISTS (SNARE) — Constrained by institutional capture of federal judiciary; cannot veto balancing doctrine despite principled objection. Face professional cost for dissent (academic marginalization, circuit court assignment patterns). Structurally victim to the reading's institutional dominance — the Supreme Court's embrace of reasonableness balancing has foreclosed warrant-preference as controlling doctrine in most circuits.
constraint_indexing:constraint_classification(fourth_amendment__reasonableness_balancing_reading, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CIVIL LIBERTIES ORGANIZATIONS (TANGLED ROPE) — Organized agents (ACLU, EFF) have mobility: can litigate, lobby, generate public attention, shift political coalitions. But also benefit from the balancing framework's existence as a doctrine to contest — the reasonableness test creates a visible adjudication site where interests are weighed. They experience extraction (many searches pass reasonableness despite warrant preference) AND coordination function (the balancing framework provides a structured vocabulary for rights claims). Not pure snare because they have organized exit capacity and can influence doctrine; not pure rope because they bear genuine asymmetric extraction.
constraint_indexing:constraint_classification(fourth_amendment__reasonableness_balancing_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: LAW ENFORCEMENT / GOVERNMENT AGENCIES (ROPE) — Primary beneficiary (institutional/arbitrage). The reasonableness balancing doctrine permits warrantless searches in special-needs contexts (school safety, drug testing, administrative inspections), enabling operational flexibility. They experience the constraint as coordination: balancing provides a framework for reconciling security needs with individual interests. Net extraction runs toward this agent — they capture efficiency gains and operational scope expansion. Can arbitrage between jurisdictions with different reasonableness thresholds.
constraint_indexing:constraint_classification(fourth_amendment__reasonableness_balancing_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: DOCTRINE DEVELOPMENT COALITION (SCAFFOLD) — Organized academic and judicial reform efforts aimed at constraining the reasonableness balancing doctrine back toward warrant preference. The scaffold reading sees the reasonableness balancing framework as a temporary doctrinal excursion from the 'true' Fourth Amendment (warrant preference), with ongoing pressure to restore limits. This perspective has a sunset: as generational judges retire and new appointees bring renewed focus on textual limits, the balancing framework could be constrained. Theater is moderate here because the doctrine is genuinely contested, not performatively maintained.
constraint_indexing:constraint_classification(fourth_amendment__reasonableness_balancing_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: WARRANT CLAUSE TEXT (PITON) — The warrant clause ('no Warrants shall issue, but upon probable cause...') is textually maintained as law but substantially degraded in function. It governs when warrants are used, but reasonableness balancing permits vast warrantless searches. The warrant clause persists as theater — recited, invoked, technically respected — while its primary protective function has been displaced by balancing doctrine. Piton classification follows from high theater ratio: the textual commitment to warrants survives as institutional artifact, but its operative force has been hollowed.
constraint_indexing:constraint_classification(fourth_amendment__reasonableness_balancing_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational/comparative view, the reasonableness balancing doctrine represents a genuine hybrid: it coordinates legitimate state interests (security, administration, special needs) with individual privacy claims through structured balancing. But it also extracts asymmetrically — the doctrine privileges state flexibility over individual protection, and the 'balancing' often presupposes state purposes are weightier than privacy. The doctrine functions as both a coordination mechanism and an extraction vehicle.
constraint_indexing:constraint_classification(fourth_amendment__reasonableness_balancing_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fourth_amendment__reasonableness_balancing_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fourth_amendment__reasonableness_balancing_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fourth_amendment__reasonableness_balancing_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(fourth_amendment__reasonableness_balancing_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(fourth_amendment__reasonableness_balancing_reading, TR),
    TR >= 0.70.

:- end_tests(fourth_amendment__reasonableness_balancing_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The reasonableness balancing doctrine permits warrantless searches in special-needs and administrative contexts, extracting operational authority from individual privacy protections. The extraction is not maximal (0.70+) because balancing doctrine ostensibly requires genuine interest weighing and is theoretically open to invalidating searches that fail the test. Empirically, however, courts applying balancing doctrine often presuppose government interests are weightier, making the ostensible neutrality of balancing a legitimation mechanism for extraction. Extractiveness has accumulated over 40 years as special-needs categories have expanded and lower courts have applied balancing with increasing laxness. Suppression (0.35): Moderate. The balancing doctrine itself does not directly suppress alternatives through coercion, but it suppresses warrant-preference doctrine institutionally—judges who favor warrant preference face reputational cost, academic warrant-preference scholars are marginalized, and the doctrinal framework presupposes balancing is the correct test. However, suppression is not severe because warrant-preference doctrine remains intellectually live in academic discourse and lower-court dissents, and the Supreme Court occasionally emphasizes warrant preference in particular contexts. Theater ratio (0.48): Moderate. The warrant clause persists as textual law ('no Warrants shall issue but upon probable cause') but reasonableness balancing permits vast warrantless searches. The warrant clause is invoked, maintained, technically respected—but its protective function has been displaced. Theater is not high (0.70+) because the balancing doctrine is genuinely contested and the theoretical arguments for warrant preference remain viable.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence across the indexical tuple. The powerless subject of a warrantless special-needs search sees Snare (trapped, no exit, pure extraction). Privacy-maximalist formalists see Snare (constrained by institutional capture, their doctrinal preference has been displaced). Civil liberties organizations see Tangled Rope (organized, mobile, but experiencing genuine extraction even as they benefit from contestation). Law enforcement sees Rope (coordination of security needs with rights protection, net beneficiary through expansion of special-needs exceptions). The institutional warrant clause sees Piton (textually maintained but functionally degraded). The analytical observer sees Tangled Rope (genuine hybrid of coordination and extraction). The perspectival gap reveals that the reasonableness balancing reading is not neutral doctrine—it privileges certain agent positions (law enforcement) while victimizing others (powerless search subjects), while maintaining the appearance of a balanced, interest-weighing framework.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality computation depends on each agent's structural position. Law enforcement beneficiaries with arbitrage options (can move between jurisdictions, expand special-needs categories) experience low or negative d, mapping to negative effective extraction (they are being subsidized by the doctrine). Warrant-preference formalists who are constrained from resisting the doctrine experience moderate d despite their intellectual position, because they face institutional barriers to exit. Powerless search subjects experience maximum d (trapped, bearing full extraction cost). Civil liberties organizations experience moderate d (organized but also somewhat captured by the doctrine's existence as a litigable framework). The analytical observer's d is derived from the assumption of symmetric interest in both readings, placing them near d=0.5 (symmetric position).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in the Fourth Amendment is rooted in the contest between warrant preference and reasonableness balancing as competing interpretations of what the Amendment fundamentally protects. The warrant-preference reading sees the Amendment as protecting from unreasonable seizure through the structure of warrants and probable cause. The reasonableness-balancing reading sees the Amendment as protecting through flexible balancing of interests, with warrants as one tool but not the fundamental test. The mandatrophy resolves by recognizing that BOTH readings are defensible from the text, but they have radically different distributional consequences. The balancing reading extracts from powerless subjects while enabling law enforcement flexibility. The warrant-preference reading would restore stronger protections but constrain government operations. The constraint's tangled-rope classification reflects that the balancing doctrine functions simultaneously as a coordination mechanism (reconciling security with privacy in theory) and an extraction mechanism (empirically favoring government interests). The mandatrophy is not 'which reading is right?' but 'which distribution of power do we authorize?' The doctrinal question masks a political question about the Fourth Amendment's ultimate purpose: protecting individual privacy against government power, or accommodating government power within a reasonableness constraint?
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reasonableness_threshold_indeterminacy,
    'What level of government interest justifies a warrantless search under the reasonableness balancing test? Where is the threshold between legitimate intrusion and rights violation?',
    'Systematic analysis of Supreme Court balancing decisions: what interest weights tip the balance? Are the standards predictable or discretionary? Do lower courts apply consistent thresholds or does balancing become ad hoc judicial preference?',
    'If threshold is clear and consistent: balancing provides legitimate coordination. If indeterminate: balancing becomes a legitimation mechanism for police discretion (extraction vehicle). Classification could shift from tangled_rope to snare if indeterminacy is severe.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reasonableness_threshold_indeterminacy, empirical, 'Whether reasonableness balancing produces predictable or indeterminate thresholds').

omega_variable(
    special_needs_doctrine_scope_creep,
    'Do ''special needs'' exception categories (school safety, drug testing, administrative searches, drunk driving checkpoints) represent genuinely exceptional contexts or has the category expanded to routinize warrantless searches?',
    'Historical analysis of special-needs exception expansion; comparison of original (1985) scope to contemporary application; identification of doctrinal category inflation',
    'If genuinely exceptional: special-needs balancing produces Rope for most subjects (exception to general warrant rule). If scope-creep severe: special-needs has become the general rule and warrant preference the exception, inverting the doctrinal hierarchy. Extractiveness would increase with scope-creep confirmation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(special_needs_doctrine_scope_creep, empirical, 'Whether special-needs exception has scope-crept toward routine warrantless searches').

omega_variable(
    warrant_preference_versus_balancing_logical_compatibility,
    'Are warrant preference and reasonableness balancing logically incompatible readings of the Fourth Amendment, or are they compatible doctrines operating in different domains?',
    'Formal analysis of the two readings'' foundational premises. Can a single interpretive framework coherently hold: (1) warrants are presumptively required (warrant preference) AND (2) searches are ultimately evaluated by reasonableness balancing? Or does adopting one necessarily foreclose the other?',
    'If forecloses: the two readings cannot coexist in a single legal framework — one reading logically rules out the other. The kernel itself is unstable and courts must eventually choose. If compatible: the readings can coexist as competing doctrines applied to different contexts. The classification of the reading_relation would shift from forecloses to coexists_with.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(warrant_preference_versus_balancing_logical_compatibility, conceptual, 'Whether warrant preference and balancing are logically incompatible or can coexist').

omega_variable(
    empirical_effects_of_balancing_doctrine_on_search_frequency,
    'Has adoption of reasonableness balancing doctrine measurably increased the frequency or intrusiveness of warrantless searches compared to jurisdictions or historical periods that prioritized warrant preference?',
    'Comparative empirical analysis: search frequency and type pre/post adoption of balancing doctrine; inter-jurisdictional comparison of warrant-preference vs balancing jurisdictions; police practice data',
    'If balancing increases searches: confirms extractiveness measurement; supports snare/tangled_rope classification over rope. If no significant difference: suggests balancing is doctrinal theater masking existing police practices. Supports piton hypothesis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_effects_of_balancing_doctrine_on_search_frequency, empirical, 'Empirical effects of balancing doctrine on search frequency and intrusiveness').

omega_variable(
    reading_asymmetry_in_doctrine_institutional_power,
    'Why has the reasonableness balancing reading achieved doctrinal dominance in the Supreme Court while warrant preference remains intellectually vibrant in academic commentary and lower-court dissents?',
    'Institutional analysis: What structural features of the Supreme Court favor balancing over textual warrant preference? What coalitional dynamics support adoption? How does the reading advantage government institutional interests over privacy formalism?',
    'This is a kernel commission question, not a classification question. But it reveals why the balancing reading is the operative doctrine despite ongoing theoretical contest. The answer likely involves: (1) institutional preferences for discretion and flexibility (government-aligned reading), (2) academic and dissent vitality (warrant-preference remains contestable), (3) doctrinal capture where the empowered reading becomes self-reinforcing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_asymmetry_in_doctrine_institutional_power, conceptual, 'Why reasonableness balancing achieved doctrinal dominance despite warrant-preference theoretical strength').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fourth_amendment__reasonableness_balancing_reading, 1985, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(four_tr_t0, fourth_amendment__reasonableness_balancing_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(four_tr_t15, fourth_amendment__reasonableness_balancing_reading, theater_ratio, 15, 0.42).
narrative_ontology:measurement(four_tr_t40, fourth_amendment__reasonableness_balancing_reading, theater_ratio, 40, 0.48).

% Extraction over time
narrative_ontology:measurement(four_be_t0, fourth_amendment__reasonableness_balancing_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(four_be_t15, fourth_amendment__reasonableness_balancing_reading, base_extractiveness, 15, 0.45).
narrative_ontology:measurement(four_be_t40, fourth_amendment__reasonableness_balancing_reading, base_extractiveness, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fourth_amendment__reasonableness_balancing_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fourth_amendment__reasonableness_balancing_reading, fourth_amendment__warrant_preference_reading).

% DUAL FORMULATION NOTE:
% The Fourth Amendment kernel decomposes into two structurally distinct constraint stories: reasonableness_balancing_reading (this story, ε=0.58, Tangled Rope) and warrant_preference_reading (sibling story, expected ε≈0.25-0.35, Rope or Mountain depending on analytical perspective). The ε values differ because the two readings define different constraints: balancing-reading measures the extraction enabled by flexible reasonableness doctrine; warrant-preference reading measures the protection enabled by textual warrant requirement. They are not the same constraint viewed from different angles—they are genuinely distinct doctrinal frameworks with different operative mechanisms. Both stories link via network.affects_constraints to indicate they are competing readings of the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fourth_amendment__reasonableness_balancing_reading, organized, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
