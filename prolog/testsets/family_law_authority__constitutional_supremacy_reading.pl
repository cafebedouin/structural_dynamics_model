% ============================================================================
% CONSTRAINT STORY: family_law_authority__constitutional_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_family_law_authority__constitutional_supremacy_reading, []).

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
 *   constraint_id: family_law_authority__constitutional_supremacy_reading
 *   human_readable: Constitutional Supremacy in Family Law Authority
 *   domain: constitutional_law/legal_pluralism/family_law
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the contested kernel
 *   'family_law_authority'—specifically, the constitutional supremacy
 *   reading. The reading asserts that family law authority derives from the
 *   constitutional state; religious practice is the private sphere, not a
 *   legal regime. This reading generates a tangled rope constraint: the state
 *   genuinely coordinates family law across diverse populations (coordination
 *   function), but does so by systematically extracting authority from
 *   religious communities and suppressing alternative legal regimes
 *   (asymmetric extraction). The state benefits from monopoly legal
 *   authority; religious minorities lose communal autonomy. The constraint
 *   relies on active state enforcement to override competing religious family
 *   law determinations. The reading is one live position in an ongoing
 *   constitutional contest with at least two structural siblings: the
 *   communal autonomy reading (family law derives from religious tradition;
 *   state recognizes but does not regulate) and the hybrid accommodation
 *   reading (state and communities share authority under constitutional
 *   floor). This story generates only the constitutional supremacy reading
 *   and routes the committer structure—the contest itself, the basis for
 *   rejecting siblings, and the reference frame this reading takes—through
 *   omega variables and cs_structure fields per Kernels and Readings rules.
 *
 * KEY AGENTS:
 *   - Constitutional State Apparatus: Primary beneficiary (institutional/arbitrage) — monopoly legal authority, extraction of deference, enforcement capacity. Experiences constraint as legitimate coordination.
 *   - Religious Minority Communities: Primary victim (powerless/trapped) — loss of communal autonomy, enforcement of uniform state rules overriding internal practice. Structurally trapped because state jurisdictional reach is inescapable.
 *   - Religious Community Leadership: Secondary actor (moderate/constrained) — can maintain internal practices within state law constraints but cannot enforce determinations against state. Experiences mixed coordination (internal practice) and extraction (authority override).
 *   - Traditional Religious Family Law Institutions: Institutional actor (institutional/arbitrage) — once primary authority, now theatrical vestige. Perform authority they no longer legally possess.
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing state supremacy as inherent to modernity rather than contingent institutional reading.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__constitutional_supremacy_reading, 0.32).
domain_priors:suppression_score(family_law_authority__constitutional_supremacy_reading, 0.48).
domain_priors:theater_ratio(family_law_authority__constitutional_supremacy_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__constitutional_supremacy_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(family_law_authority__constitutional_supremacy_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(family_law_authority__constitutional_supremacy_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__constitutional_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__constitutional_supremacy_reading, "Constitutional Supremacy in Family Law Authority").
narrative_ontology:topic_domain(family_law_authority__constitutional_supremacy_reading, "constitutional_law/legal_pluralism/family_law").

domain_priors:requires_active_enforcement(family_law_authority__constitutional_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__constitutional_supremacy_reading, '42c45dbc-7a36-4e19-948a-56986e052baa').
narrative_ontology:cs_kernel_codification('42c45dbc-7a36-4e19-948a-56986e052baa', formalized).
narrative_ontology:cs_authority_grounding('42c45dbc-7a36-4e19-948a-56986e052baa', extraction).
narrative_ontology:cs_interpretation_layer_present('42c45dbc-7a36-4e19-948a-56986e052baa').
narrative_ontology:cs_reading_relation('42c45dbc-7a36-4e19-948a-56986e052baa', family_law_authority__communal_autonomy_reading, forecloses).
narrative_ontology:cs_reading_relation('42c45dbc-7a36-4e19-948a-56986e052baa', family_law_authority__hybrid_accommodation_reading, coexists_with).
narrative_ontology:cs_axiom('42c45dbc-7a36-4e19-948a-56986e052baa', foundational, state_legal_monopoly_on_family_law).
narrative_ontology:cs_axiom_status(state_legal_monopoly_on_family_law, holdable).
narrative_ontology:cs_axiom_grounding('42c45dbc-7a36-4e19-948a-56986e052baa', state_legal_monopoly_on_family_law, deontological).
narrative_ontology:cs_axiom('42c45dbc-7a36-4e19-948a-56986e052baa', foundational, religious_practice_is_private_not_legal).
narrative_ontology:cs_axiom_status(religious_practice_is_private_not_legal, holdable).
narrative_ontology:cs_axiom_grounding('42c45dbc-7a36-4e19-948a-56986e052baa', religious_practice_is_private_not_legal, deontological).
narrative_ontology:cs_reference_frame('42c45dbc-7a36-4e19-948a-56986e052baa', constitutional_state_supremacy_framework).
narrative_ontology:cs_drift_state('42c45dbc-7a36-4e19-948a-56986e052baa', contemporary_pluralist_democracies, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('42c45dbc-7a36-4e19-948a-56986e052baa', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(family_law_authority__constitutional_supremacy_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__constitutional_supremacy_reading, constitutional_state_apparatus).
narrative_ontology:constraint_victim(family_law_authority__constitutional_supremacy_reading, religious_minority_communities).
narrative_ontology:constraint_victim(family_law_authority__constitutional_supremacy_reading, communal_autonomy_structures).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RELIGIOUS MINORITY COMMUNITY MEMBER (SNARE) — Structurally trapped. Cannot exit the state's jurisdictional reach; cannot practice family law according to religious tradition without state legal framework overriding communal authority. The member experiences uniform state rules as imposed law, not coordination. Zero degrees of freedom in practice.
constraint_indexing:constraint_classification(family_law_authority__constitutional_supremacy_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: RELIGIOUS COMMUNITY LEADERSHIP (TANGLED ROPE) — Constrained but not trapped. Leadership can maintain internal dispute resolution practices (counseling, mediation) but cannot enforce family law determinations against state law without state recognition. The reading provides genuine coordination of internal community practice—ritual solemnization, inheritance norms, internal marriage dissolution procedures—alongside systematic extraction: state overrides community authority on property, custody, and substantive rights. Moderate extraction because community retains functional coordination capacity within state constraints.
constraint_indexing:constraint_classification(family_law_authority__constitutional_supremacy_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CONSTITUTIONAL STATE APPARATUS (ROPE) — Net beneficiary. Experiences the constraint as coordination: uniform rules enable state capacity to manage family law across diverse populations. The state benefits from monopoly legal authority and extraction of deference; extraction is legitimate in this perspective because it solves a genuine coordination problem (conflicting laws, plural regimes, need for unified inheritance/custody framework). Low extraction experienced because the constraint solves the state's structural problem.
constraint_indexing:constraint_classification(family_law_authority__constitutional_supremacy_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: TRADITIONAL RELIGIOUS FAMILY LAW INSTITUTIONS (PITON) — Once primary authority, now largely performative. Religious courts and councils persist in some jurisdictions (Islamic qadi courts, Jewish beth din recognized in limited capacity) but lack enforcement power; state law enforcement backs state courts exclusively. The institutions continue through cultural inertia and private dispute resolution, but their legal authority is theatrically maintained—they function as alternative dispute resolution, not binding law. Theater ratio reflects that much of their activity is ritual performance of authority they no longer possess.
constraint_indexing:constraint_classification(family_law_authority__constitutional_supremacy_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL LAW FRAMING (MOUNTAIN) — From a civilizational view, this reading naturalizes state monopoly on law as inherent to modern statecraft: the territorial state's foundational structure requires unified legal system; plural family law regimes are incompatible with state capacity; the supremacy of constitutional law is not contingent extraction but structural necessity. However, the structural data contradicts this mountain classification—the beneficiary (state) and victims (religious communities) are identifiable, the extraction is measurable, and the constraint relies on suppression of alternatives (religious courts). The engine will classify this as a false summit.
constraint_indexing:constraint_classification(family_law_authority__constitutional_supremacy_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_law_authority__constitutional_supremacy_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(family_law_authority__constitutional_supremacy_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(family_law_authority__constitutional_supremacy_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(family_law_authority__constitutional_supremacy_reading, TR),
    TR >= 0.70.

:- end_tests(family_law_authority__constitutional_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.32): Moderate. The state's monopoly on family law authority is robust and enforced, but the extraction is not maximal because: (1) religious communities retain functional capacity for internal coordination, dispute resolution, and ritual authority within state law constraints; (2) state benefits from this constraint as a coordination solution to plural family law regimes, making some extraction legitimate; (3) enforcement is politically costly—sustained by constitutional doctrine rather than hegemonic consent, suggesting structural fragility. Over the 100-unit interval, extractiveness rises from 0.15 to 0.32 as state enforcement capacity hardens and community resistance becomes more explicitly organized. Suppression (0.48): Moderate-high. The constraint suppresses alternatives—religious courts cannot enforce determinations; state courts override community determinations on custody, property, inheritance. Suppression is not total because communities can internally mediate disputes and maintain ceremonial authority. Theater ratio (0.55): Moderate-high. Significant performative content exists: state law claims comprehensive family law authority, but actual implementation involves negotiation and partial recognition of community practice (informal mediation, customary marriage recognition in some jurisdictions). Over time, theater ratio increases as the gap between constitutional claim (state monopoly) and lived practice (hybrid de facto arrangements) widens. The measurement trajectory reflects accumulating state enforcement apparatus alongside persistent community practice outside formal law.
 *
 * PERSPECTIVAL GAP:
 *   The constitutional supremacy reading generates a sharp perspectival gap between beneficiary and victim. The state sees coordination (Rope)—unified legal rules solve conflicts between plural family law regimes, enabling state capacity. Religious minorities see extraction (Snare)—uniform rules override community practice without consent, trapping them within state law. Religious leadership sees mixed function (Tangled Rope)—they coordinate community practice internally while experiencing state override of their authority. Traditional religious institutions see degradation (Piton)—their authority persists performatively but lacks legal force. The analytical observer risks naturalizing the state's position (Mountain)—seeing constitutional supremacy as inherent to modernity—but the structural data reveals this as a false summit: the constraint benefits identifiable agents (state) and harms identifiable others (religious communities), and alternatives exist (hybrid readings).
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from structural position: power level, exit options, and relationship to extraction flow. The state (institutional/arbitrage) experiences low directionality (d ≈ 0.15)—net beneficiary with exit options, so extracted value flows toward them. Religious minorities (powerless/trapped) experience high directionality (d ≈ 0.95)—no exit options, full target, so extracted value flows away from them. Religious leadership (moderate/constrained) experiences medium directionality (d ≈ 0.55)—some exit capacity (maintaining internal practice) but structurally constrained, so they experience significant but not maximal extraction. The piton perspective at the institutional level reflects degradation of authority without corresponding exit—the religious institutions cannot leave the constraint's shadow; they perform authority they no longer possess.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by locating the ambiguity in reading choice rather than in classification. The constitutional supremacy reading generates a genuine tangled rope: the state coordinates family law (prevents conflicts, enables unified inheritance rules, protects vulnerable parties across regimes) while extracting authority from communities (overrides religious determinations, suppresses alternative legal institutions, imposes uniform rules). Both functions are real. The mandatrophy resolves because the question is not 'is this coordination or extraction?' but 'from which reading of constitutional family law authority does this constraint derive?' The constitutional supremacy reading answers: from the reading that the state's legal authority is supreme and religious authority is private practice, not law. A sibling reading (hybrid accommodation) would generate a different constraint with different ε and different beneficiary/victim structure. The mandatrophy does not dissolve into philosophical relativism—the readings differ structurally in what they entail about state/community relationships, and these structural differences produce measurable differences in extraction, suppression, and theater. The constraint story itself is ε-invariant within this reading; alternative readings are different constraints.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_committer_framing,
    'Is the constitutional supremacy of family law grounding a genuine structural requirement of state legitimacy, or a particular reading of how state and religious authority relate?',
    'Comparative analysis: jurisdictions with hybrid accommodation systems (parallel religious courts with state recognition) that maintain functional state capacity; empirical evidence on whether unified state law is necessary for state viability or merely one institutional choice.',
    'If structural requirement: this reading''s mountain perspective is justified (state authority constraint is immutable). If reading choice: false summit classification is correct, and the constraint is contingent institutional arrangement benefiting state actors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_committer_framing, conceptual, 'Whether constitutional supremacy is structural necessity or contingent reading choice').

omega_variable(
    authority_grounding_ambiguity,
    'Does family law authority ground itself in the constitutional state''s formal-legal supremacy, or in the state''s actual enforcement capacity and social recognition?',
    'Jurisdictions where constitutional recognition exists but enforcement is contested or inadequate (religious communities that ignore state law in practice, or where state lacks coercive capacity); cases where community law is recognized informally despite constitutional subordination.',
    'If formal supremacy only: the constraint exhibits high theater (authority claimed but not fully practiced). If enforcement capacity: the constraint is more purely extractive (enforcement is real coercion). Theater ratio and extraction values would shift based on actual vs. claimed authority.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(authority_grounding_ambiguity, empirical, 'Grounding of state family law authority: formal constitutional vs. actual enforcement').

omega_variable(
    sibling_reading_empirical_contestation,
    'What empirical conditions would make the communal_autonomy_reading or hybrid_accommodation_reading structurally viable alternatives to constitutional supremacy?',
    'Comparative institutional analysis of hybrid systems (Muslim-majority democracies with sharia courts, Jewish communities with beth din recognized in parallel, Indigenous customary law with state recognition); longitudinal data on whether plural family law regimes produce worse outcomes (conflict, enforcement failure, rights violations) than unified constitutional supremacy.',
    'If viable alternatives exist: this reading coexists with siblings rather than foreclosing them. If alternative regimes consistently fail: this reading forecloses competitors on empirical grounds (not just normative).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_empirical_contestation, empirical, 'Viability of alternative family law authority readings in comparative systems').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__constitutional_supremacy_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(flacs_tr_t0, family_law_authority__constitutional_supremacy_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(flacs_tr_t50, family_law_authority__constitutional_supremacy_reading, theater_ratio, 50, 0.5).
narrative_ontology:measurement(flacs_tr_t100, family_law_authority__constitutional_supremacy_reading, theater_ratio, 100, 0.55).

% Extraction over time
narrative_ontology:measurement(flacs_be_t0, family_law_authority__constitutional_supremacy_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(flacs_be_t50, family_law_authority__constitutional_supremacy_reading, base_extractiveness, 50, 0.28).
narrative_ontology:measurement(flacs_be_t100, family_law_authority__constitutional_supremacy_reading, base_extractiveness, 100, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__constitutional_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(family_law_authority__constitutional_supremacy_reading, family_law_authority__communal_autonomy_reading).
narrative_ontology:affects_constraint(family_law_authority__constitutional_supremacy_reading, family_law_authority__hybrid_accommodation_reading).

% DUAL FORMULATION NOTE:
% The family_law_authority kernel decomposes into three structurally distinct constraints corresponding to three readings. Each reading generates different ε values, different beneficiary/victim sets, and different theatrical content. The constitutional_supremacy_reading produces moderate extractiveness (0.32); communal_autonomy_reading is hypothesized to produce lower extractiveness (state as recognizer only); hybrid_accommodation_reading produces different victim structure (distributed benefits and costs). All three stories link via network.affects_constraints to show the constraint family structure and the logical relations between readings (forecloses, coexists_with, influences).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
