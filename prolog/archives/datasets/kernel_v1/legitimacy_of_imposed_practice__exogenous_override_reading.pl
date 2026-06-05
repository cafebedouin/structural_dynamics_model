% ============================================================================
% CONSTRAINT STORY: legitimacy_of_imposed_practice__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_imposed_practice__exogenous_override_reading, []).

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
 *   constraint_id: legitimacy_of_imposed_practice__exogenous_override_reading
 *   human_readable: State Decree Authority and Practice Displacement (Exogenous Override Reading)
 *   domain: political_history/state_formation/legal_authority
 *
 * SUMMARY:
 *   This constraint models the exogenous override reading of a contested
 *   kernel: the claim that state decree authority is SUFFICIENT to displace
 *   prior practice and that compliance follows from legal mandate REGARDLESS
 *   of internalization. This reading asserts that centralized state power can
 *   override distributed, embedded cultural practices through legal
 *   pronouncement, enforcement machinery, and administrative coercion,
 *   without requiring populations to adopt the new practice as meaningful or
 *   binding. The exogenous override reading is one of three competing
 *   readings of the legitimacy_of_imposed_practice kernel. It stands in
 *   productive tension with the endogenous_climb_reading (which holds that
 *   practice change requires bottom-up adoption) and the
 *   hybrid_scaffolding_reading (which claims decree plus ideological
 *   messaging creates quasi-endogenous pull). The structural data for this
 *   reading reflect cases of calendar reform (Gregorian calendar imposition),
 *   dress code enforcement, administrative procedure standardization, and
 *   language policy in centralizing states. The constraint exhibits high
 *   suppression (0.68) reflecting enforcement machinery, moderate
 *   extractiveness (0.58) reflecting the asymmetric distribution of authority
 *   benefits and adjustment costs, and moderate theater (0.55) indicating
 *   that formal legal compliance masks persistent informal non-compliance and
 *   workarounds.
 *
 * KEY AGENTS:
 *   - State Modernization Apparatus: Primary beneficiary (institutional/arbitrage) — captures authority premium and centralizing power; uses decree mechanism to extend state reach into peripheral territories and practices
 *   - Rural Populations: Primary victim (powerless/trapped) — face coercive enforcement of new practices without consultation or transition support; bear material and social adjustment costs; have no effective exit option
 *   - Local Administrative Officers: Secondary actor (moderate/constrained) — implement state mandate while absorbing community resistance; coordinate local practice under state enforcement; depend on state career structure
 *   - Reform Coalition Advocates: Organized agents (organized/constrained) — promote exogenous override theory as development strategy; see decree as sufficient and enforcement as temporary scaffold toward internalization
 *   - Institutional Legal System: Institutional actor (institutional/arbitrage) — maintains performative legal machinery; persists through inertia and state enforcement rather than functional effectiveness
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent enforcement apparatus as immutable feature of state power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_imposed_practice__exogenous_override_reading, 0.58).
domain_priors:suppression_score(legitimacy_of_imposed_practice__exogenous_override_reading, 0.68).
domain_priors:theater_ratio(legitimacy_of_imposed_practice__exogenous_override_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_imposed_practice__exogenous_override_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_imposed_practice__exogenous_override_reading, "State Decree Authority and Practice Displacement (Exogenous Override Reading)").
narrative_ontology:topic_domain(legitimacy_of_imposed_practice__exogenous_override_reading, "political_history/state_formation/legal_authority").

domain_priors:requires_active_enforcement(legitimacy_of_imposed_practice__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_imposed_practice__exogenous_override_reading, '9f5115e0-5269-44f1-954d-1e6e05b38c17').
narrative_ontology:cs_kernel_codification('9f5115e0-5269-44f1-954d-1e6e05b38c17', formalized).
narrative_ontology:cs_authority_grounding('9f5115e0-5269-44f1-954d-1e6e05b38c17', extraction).
narrative_ontology:cs_interpretation_layer_present('9f5115e0-5269-44f1-954d-1e6e05b38c17').
narrative_ontology:cs_reading_relation('9f5115e0-5269-44f1-954d-1e6e05b38c17', legitimacy_of_imposed_practice__endogenous_climb_reading, forecloses).
narrative_ontology:cs_reading_relation('9f5115e0-5269-44f1-954d-1e6e05b38c17', legitimacy_of_imposed_practice__hybrid_scaffolding_reading, influences).
narrative_ontology:cs_axiom('9f5115e0-5269-44f1-954d-1e6e05b38c17', foundational, decree_authority_is_sufficient).
narrative_ontology:cs_axiom_status(decree_authority_is_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('9f5115e0-5269-44f1-954d-1e6e05b38c17', decree_authority_is_sufficient, instrumental).
narrative_ontology:cs_axiom('9f5115e0-5269-44f1-954d-1e6e05b38c17', foundational, internalization_unnecessary_for_compliance).
narrative_ontology:cs_axiom_status(internalization_unnecessary_for_compliance, holdable).
narrative_ontology:cs_axiom_grounding('9f5115e0-5269-44f1-954d-1e6e05b38c17', internalization_unnecessary_for_compliance, instrumental).
narrative_ontology:cs_reference_frame('9f5115e0-5269-44f1-954d-1e6e05b38c17', sovereign_decree_sufficiency).
narrative_ontology:cs_drift_state('9f5115e0-5269-44f1-954d-1e6e05b38c17', contemporary_post_colonial_period, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9f5115e0-5269-44f1-954d-1e6e05b38c17', '').
narrative_ontology:cs_kernel_id(legitimacy_of_imposed_practice__exogenous_override_reading, legitimacy_of_imposed_practice).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__exogenous_override_reading, state_modernization_apparatus).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__exogenous_override_reading, rural_populations).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__exogenous_override_reading, prior_practice_continuity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Rural populations face coercive enforcement of new practices (calendar reform, dress codes, administrative procedures) with no consultation, no transition period, and material costs. Exit via migration is structurally available but economically prohibitive. The legal mandate creates suppression through enforcement (fines, social penalty, administrative exclusion) while offering no internalization pathway. Experienced as pure extraction.
constraint_indexing:constraint_classification(legitimacy_of_imposed_practice__exogenous_override_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% Local administrators occupy a hybrid position: they implement the state mandate (benefiting from institutional authority) but also absorb community resistance, costly enforcement actions, and the practical reality that decree does not equal compliance. They coordinate local practice while enforcing state requirement. Exit from enforcement is constrained by career dependence on state apparatus.
constraint_indexing:constraint_classification(legitimacy_of_imposed_practice__exogenous_override_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The state (or centralizing power) benefits from decree authority itself: the legal instrument amplifies institutional power over peripheral territories and populations. The coordination function is real—the decree establishes a unified administrative standard—but it is asymmetrically distributed: the state captures the authority premium while populations bear adjustment costs. The state perceives the constraint as coordination (unifying diverse regions under common law).
constraint_indexing:constraint_classification(legitimacy_of_imposed_practice__exogenous_override_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% The legal form of the decree (formal pronouncement, published text, ceremonial proclamation) is highly theatrical. In practice, compliance is selective, enforcement is spotty, and workarounds are endemic. The legal machinery persists—courts, administrative bodies, penalty structures—but primarily through institutional inertia and the state's continued insistence on the form, not because the decree mechanism is functionally displacing practice. Theater ratio reflects the gap between declared legal transformation and actual practice change.
constraint_indexing:constraint_classification(legitimacy_of_imposed_practice__exogenous_override_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Reformers and state architects viewing decree authority as sufficient see the constraint as temporary: legal mandate establishes the new standard; enforcement machinery ensures compliance; over time (one or two generations), populations born under the new regime internalize the practice as normal. The scaffold has an implicit sunset: once internalization occurs, coercive enforcement becomes unnecessary. From this perspective, theater and suppression are justified transitional costs.
constraint_indexing:constraint_classification(legitimacy_of_imposed_practice__exogenous_override_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Some analytical observers frame state decree authority as a natural law: sovereign authority IS sufficient to displace practice; compliance follows from legal mandate as a matter of political structure. This perspective treats the exogenous override as immutable—a structural feature of centralized state power itself. However, the structural data (suppression ≥0.68, theater ≥0.55, persistent rural non-compliance) contradicts the mountain classification. The engine will identify this as a false summit: treating a contingent enforcement apparatus as natural law.
constraint_indexing:constraint_classification(legitimacy_of_imposed_practice__exogenous_override_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_imposed_practice__exogenous_override_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(legitimacy_of_imposed_practice__exogenous_override_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(legitimacy_of_imposed_practice__exogenous_override_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_of_imposed_practice__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(legitimacy_of_imposed_practice__exogenous_override_reading, TR),
    TR >= 0.70.

:- end_tests(legitimacy_of_imposed_practice__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, increasing over time (0.35 → 0.50 → 0.58). Initial extraction is lower because early decree can achieve rapid compliance through novelty effect and state visibility; over time, as workarounds proliferate and enforcement must intensify, the extraction cost becomes more apparent. The state captures the modernization benefit (unified administrative standard, expanded territorial reach); populations bear adjustment costs without consultation. Suppression (0.68): High and rising (0.62 → 0.68 → 0.70). The decree mechanism requires sustained enforcement machinery—fines, social penalty, administrative exclusion, legal proceedings. Suppression is not simply material barriers (populations could technically adopt the new practice) but coercive enforcement making non-adoption costly. The rising trajectory reflects that enforcement intensity must increase as populations develop workarounds and resistance hardens. Theater ratio (0.55): Moderate, rising slightly (0.45 → 0.52 → 0.55). The formal legal pronouncement and administrative procedures are highly theatrical—ceremonial proclamation, published decrees, formal court proceedings—but actual practice change lags far behind declared legal status. The theater increases over time as the gap between formal legal compliance (which is enforced) and actual practice change (which is partial and uneven) widens. Populations comply formally (submit to enforcement, pay fines, perform public adherence) while maintaining prior practices informally (shadow calendars, informal dress in private, workarounds in administrative procedures).
 *
 * PERSPECTIVAL GAP:
 *   The exogenous override reading produces sharp perspectival divergence. The state apparatus sees coordination and modernization (Rope perspective): decree establishes unified standard, enabling administrative integration and territorial expansion. The rural subject sees pure extraction (Snare perspective): coercive enforcement of unfamiliar practice with no path to exit or influence. Local administrators see mixed coordination and extraction (Tangled Rope perspective): they enforce the decree and benefit from institutional authority, but also absorb community friction and enforcement costs. The reform coalition sees temporary scaffolding (Scaffold perspective): decree plus generational time produces internalization without the hybrid reading's ideological pull. The institutional legal system sees its own degradation (Piton perspective): legal machinery is theatrical, persisting through state enforcement rather than functional effectiveness. The civilizational analytical observer risks seeing immutable natural law (Mountain perspective): state decree authority as inherent to political structure, not contingent on enforcement. The perspectival gap is maximal between the state apparatus (Rope, net beneficiary) and rural populations (Snare, net victim). The gap reveals that 'decree authority' conceals the actual mechanism: coercive enforcement, not logical necessity or natural authority.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from the agent's structural position relative to the extraction flow. The state modernization apparatus is the beneficiary: it extracts authority value and centralizing power from the decree mechanism. Rural populations are the victims: they bear costs (fines, social penalty, adjustment burden) without benefit. Local administrators occupy a hybrid position: they gain institutional authority from enforcing state mandate but absorb the friction of enforcement and community resistance. The scaffold perspective sees the extraction as temporary—generational internalization will eventually make enforcement unnecessary. The piton perspective sees the legal form as largely performative—the institutional machinery persists not because it works but because the state mandates it. The analytical mountain perspective risks naturalization—treating contingent enforcement apparatus as inherent to state power. Directionality values for beneficiaries (state apparatus, institutional systems) are low (0.10-0.20), reflecting that extraction runs toward these agents. Directionality values for victims (rural populations) are high (0.85-0.95), reflecting maximum extraction experience. Directionality for moderate actors (local administrators, organized reformers) is mid-range (0.50-0.65), reflecting mixed costs and benefits.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    internalization_vs_compliance_distinction,
    'Can exogenous mandate produce durable compliance without internalization, or does non-internalized compliance require perpetual enforcement and gradually degrade?',
    'Longitudinal compliance trajectory analysis: Do enforcement costs remain constant, rise, or fall over 2-3 generational periods? Do workarounds increase or stabilize? Do second-generation populations internalize or merely obey?',
    'If exogenous mandate sustains compliance without internalization: the exogenous override reading is correct, and the constraint degrades to Piton once enforcement normalizes. If compliance requires internalization and exogenous mandate cannot produce it: the scaffold or hybrid reading is correct, and pure decree is inherently limited.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(internalization_vs_compliance_distinction, empirical, 'Whether exogenous mandate can sustain compliance without internalization').

omega_variable(
    enforcement_capacity_sustainability,
    'What proportion of the state''s enforcement capacity is consumed by ensuring compliance with a single imposed practice across a large population?',
    'Historical measurement of enforcement machinery deployment (officials, courts, penalties) relative to state total administrative capacity. Comparison across cases (calendar reform, dress codes, language policy, administrative procedures).',
    'If enforcement consumes >15% of state administrative capacity: decree is unsustainable without internalization. If <5%: decree may be sufficient. This threshold determines whether pure exogenous override is viable or collapse-prone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capacity_sustainability, empirical, 'Proportion of state enforcement capacity consumed by compliance with single imposed practice').

omega_variable(
    reading_foundational_premise,
    'Does this reading commit to the premise that state decree authority IS SUFFICIENT to displace practice, or only that it is NECESSARY?',
    'Textual analysis of reform rhetoric; historical case classification (pure decree with success vs. pure decree with persistent failure). Does reformist ideology claim decree sufficiency or merely decree necessity?',
    'If sufficiency claim: this reading forecloses the endogenous_climb_reading (which denies sufficiency) within a single framework. If only necessity: the readings coexist (decree is necessary but not sufficient; internalization is also required). The distinction determines the reading_relations entry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foundational_premise, conceptual, 'Whether exogenous override reading commits to decree sufficiency or only necessity').

omega_variable(
    rural_non_compliance_typology,
    'Is rural non-compliance best characterized as resistance (active rejection), evasion (workaround strategies), or slow internalization (gradual adoption)?',
    'Qualitative case analysis distinguishing active resistance movements, documented workarounds and shadow practices, generational compliance curves. Does non-compliance decrease over time (internalization) or persist (structural evasion)?',
    'If active resistance: exogenous override reading is falsified—populations retain agency and reject the mandate. If evasion: reading survives (compliance is enforced; workarounds are secondary). If internalization: the scaffold reading becomes more plausible (decree plus generational time creates compliance without the scaffold''s ideological pull).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rural_non_compliance_typology, empirical, 'Characterization of rural non-compliance as resistance, evasion, or slow internalization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_imposed_practice__exogenous_override_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(legi_tr_t10, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 10, 0.52).
narrative_ontology:measurement(legi_tr_t20, legitimacy_of_imposed_practice__exogenous_override_reading, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(legi_be_t10, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(legi_be_t20, legitimacy_of_imposed_practice__exogenous_override_reading, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(legi_su_t10, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(legi_su_t20, legitimacy_of_imposed_practice__exogenous_override_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_imposed_practice__exogenous_override_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__exogenous_override_reading, legitimacy_of_imposed_practice__endogenous_climb_reading).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__exogenous_override_reading, legitimacy_of_imposed_practice__hybrid_scaffolding_reading).

% DUAL FORMULATION NOTE:
% This story represents ONE reading of the legitimacy_of_imposed_practice kernel. The sibling readings (endogenous_climb and hybrid_scaffolding) are separate constraint stories with different epsilon values, different beneficiary/victim structures, and different classifications. The three stories are linked by network.affects_constraints to model the constraint family. Do NOT merge the readings into a single story or treat epsilon as observable-dependent. Each reading is a distinct constraint with its own structural properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
