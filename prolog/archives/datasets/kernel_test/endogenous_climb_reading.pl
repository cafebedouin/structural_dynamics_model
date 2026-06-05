% ============================================================================
% CONSTRAINT STORY: endogenous_climb_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_endogenous_climb_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: endogenous_climb_reading
 *   human_readable: Endogenous Practice Displacement and Internalization
 *   domain: political_history/state_formation/cultural_imposition
 *
 * SUMMARY:
 *   This constraint models the endogenous climb reading of imposed practice
 *   displacement: the claim that state-mandated calendar and dress reforms
 *   can succeed through bottom-up adoption pathways rather than top-down
 *   enforcement alone. The reading is instantiated as ONE interpretation of
 *   the contested kernel 'legitimacy_of_imposed_practice' — it is not a
 *   historical description of what happened, but a formal commitment to what
 *   constitutes legitimate state practice and under what conditions imposed
 *   practices can become self-sustaining. The historical observable —
 *   calendar conversion failures, dress code partial adoption, private
 *   retention of customary practice decades after official mandate — is
 *   filtered through this reading's normative frame: communities can
 *   endogenously climb toward new practices IF adoption pathways are built
 *   into the enforcement mechanism. The constraint exhibits declining
 *   extractiveness over the 40-year interval (0.68 → 0.52) as urban diffusion
 *   and syncretism create voluntary adoption pathways, offsetting the state's
 *   enforcement burden. However, the identity-locked response of traditional
 *   communities (lunar observance persists in private ritual) reveals the
 *   reading's limits: genuine internalization remains contested, and
 *   suppression remains necessary to maintain compliance in communities where
 *   identity-fusion to traditional practices is deep.
 *
 * KEY AGENTS:
 *   - Traditional Community: Primary victim (powerless/identity_locked) — identity-fused to lunar observance and customary dress; compliance would require abandoning identity frame; suppression operates internally as well as externally.
 *   - Syncretizing Urban Cohort: Secondary victim/partial beneficiary (moderate/constrained) — constrained by economic migration and education access; benefits from state integration opportunities (jobs, literacy) while bearing extraction cost of partial identity suppression; practices dual identity (public state conformity, private customary retention).
 *   - State Modernization Authority: Primary beneficiary (institutional/arbitrage) — extracts administrative coordination and symbolic integration; experiences constraint as solving collective action problem; perceives itself as enabling progress, not extracting.
 *   - Colonial or Imperial Oversight: Secondary beneficiary (organized/mobile) — invested in verifying that bottom-up adoption pathways are emerging to justify sunset of enforcement apparatus; motivated to interpret data optimistically.
 *   - Historical Institutional Analysis: Neutral observer (institutional/arbitrage) — notes that formal mandates persist inertially after enforcement necessity declines; interested in whether institutional persistence reflects actual internalization or path dependence.
 *   - Analytical Observer: Analytical position (analytical/analytical) — detects asymmetry in extraction flow: state benefits from coordination, communities bear identity-suppression cost; questions whether the reading's core mechanism (endogenous adoption) actually operates.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(endogenous_climb_reading, 0.52).
domain_priors:suppression_score(endogenous_climb_reading, 0.48).
domain_priors:theater_ratio(endogenous_climb_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(endogenous_climb_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(endogenous_climb_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(endogenous_climb_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(endogenous_climb_reading, tangled_rope).
narrative_ontology:human_readable(endogenous_climb_reading, "Endogenous Practice Displacement and Internalization").
narrative_ontology:topic_domain(endogenous_climb_reading, "political_history/state_formation/cultural_imposition").

domain_priors:requires_active_enforcement(endogenous_climb_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(endogenous_climb_reading, '33c86bab-a39d-4022-b8fd-9cd1eebe8482').
narrative_ontology:cs_created_at('33c86bab-a39d-4022-b8fd-9cd1eebe8482', '').
narrative_ontology:cs_kernel_codification('33c86bab-a39d-4022-b8fd-9cd1eebe8482', formalized).
narrative_ontology:cs_authority_grounding('33c86bab-a39d-4022-b8fd-9cd1eebe8482', lineage).
narrative_ontology:cs_interpretation_layer_present('33c86bab-a39d-4022-b8fd-9cd1eebe8482').
narrative_ontology:cs_kernel_id(endogenous_climb_reading, legitimacy_of_imposed_practice).
narrative_ontology:cs_reading_relation('33c86bab-a39d-4022-b8fd-9cd1eebe8482', exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('33c86bab-a39d-4022-b8fd-9cd1eebe8482', hybrid_scaffolding_reading, influences).
narrative_ontology:cs_axiom('33c86bab-a39d-4022-b8fd-9cd1eebe8482', foundational, endogenous_adoption_pathway_sufficient).
narrative_ontology:cs_axiom_status(endogenous_adoption_pathway_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('33c86bab-a39d-4022-b8fd-9cd1eebe8482', endogenous_adoption_pathway_sufficient, empirically_contingent).
narrative_ontology:cs_axiom('33c86bab-a39d-4022-b8fd-9cd1eebe8482', foundational, voluntary_adoption_legitimizes_coercion).
narrative_ontology:cs_axiom_status(voluntary_adoption_legitimizes_coercion, holdable).
narrative_ontology:cs_axiom_grounding('33c86bab-a39d-4022-b8fd-9cd1eebe8482', voluntary_adoption_legitimizes_coercion, deontological).
narrative_ontology:cs_reference_frame('33c86bab-a39d-4022-b8fd-9cd1eebe8482', lineage_legitimacy_requiring_internalization).
narrative_ontology:cs_drift_state('33c86bab-a39d-4022-b8fd-9cd1eebe8482', contemporary_postcolonial_context, gap(axiom_overriding, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(endogenous_climb_reading, communities_preserving_autonomy).
narrative_ontology:constraint_victim(endogenous_climb_reading, state_modernization_timeline).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRADITIONAL COMMUNITY (SNARE) — Identity-locked into lunar observance and customary dress as markers of community belonging and ancestral continuity. Structural mobility exists (can technically adopt new practices) but identity-fusion prevents exercise of exit. Suppression operates through internalized cultural identity and social ostracism risk, not external coercion. The state's imposition triggers maximum extraction: forced choice between identity and compliance.
constraint_indexing:constraint_classification(endogenous_climb_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: SYNCRETIZING URBAN COHORT (TANGLED ROPE) — Constrained by economic migration and education access, but also benefits from state integration opportunities (jobs, literacy, social mobility). Adopt new calendar for administrative purposes while privately retaining lunar observance; wear state-mandated dress publicly but retain customary dress for family ceremonies. Mixed extraction and coordination: the state enables mobility while extracting conformity.
constraint_indexing:constraint_classification(endogenous_climb_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: STATE MODERNIZATION AUTHORITY (ROPE) — Net beneficiary of calendar/dress standardization (administrative efficiency, symbolic integration, measured compliance). Experiences the constraint as pure coordination: mandating unified practices solves genuine calendrical and bureaucratic alignment problems. Low perceived extraction because the state sees itself as solving a collective action problem, not imposing costs.
constraint_indexing:constraint_classification(endogenous_climb_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: COLONIAL OR IMPERIAL OVERSIGHT (SCAFFOLD) — Views the imposition as a temporary coordination mechanism with a sunset clause: once the new calendar and dress are internalized across a generation, the enforcement apparatus can be withdrawn. The scaffold assumes that imposing practices from above creates bottom-up adoption pathways, allowing the constraint to become a self-sustaining norm. Extraction is tolerated only if enforcement suppression declines over the time horizon.
constraint_indexing:constraint_classification(endogenous_climb_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: HISTORICAL INSTITUTIONAL ANALYSIS (PITON) — Observes that the formal mandate (calendar conversion, dress codes) persists bureaucratically long after functional necessity has disappeared. The theater ratio is low (0.35) because the constraint still coordinates genuine administrative alignment, but the inertial persistence after internalization failure suggests the institution has lost its primary function and survives through path dependence. The mandate persists because it is easier to maintain than to revoke, not because it actively solves coordination problems.
constraint_indexing:constraint_classification(endogenous_climb_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, this constraint reveals a fundamental structural asymmetry: imposed practices can coordinate surface behavior but fail to generate bottom-up internalization without consensual adoption pathways. The state benefits from calendar/dress standardization (coordination function), but the communities bear the extraction cost of identity suppression (asymmetric cost distribution). The constraint is genuinely hybrid: it solves a real coordination problem while extracting from those whose identity frame makes compliance costly.
constraint_indexing:constraint_classification(endogenous_climb_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(endogenous_climb_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(endogenous_climb_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(endogenous_climb_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(endogenous_climb_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(endogenous_climb_reading, TR),
    TR >= 0.70.

:- end_tests(endogenous_climb_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, declining over time. The state extracts coordination benefits (unified calendar, standardized dress = administrative efficiency, symbolic integration). The communities bear extraction cost of identity suppression, particularly in the identity-locked traditional segments. The value of 0.52 reflects that partial adoption does occur (some communities genuinely internalize new practices), creating genuine coordination benefits rather than pure extraction. As syncretism emerges in urban cohorts, the extraction becomes more hybrid — some agents benefit from integration opportunities while others bear pure identity costs. Suppression (0.48): Moderate. The state's enforcement apparatus (legal penalties, social pressure, bureaucratic exclusion) creates barriers to practicing lunar observance or customary dress publicly. However, suppression is not total — private retention is possible, and enforcement is selectively applied. The 0.48 value reflects that suppression is real but unevenly distributed across geographic/economic lines. Theater ratio (0.35): Low. The calendar and dress mandate solve genuine administrative coordination problems (reducing ambiguity in bureaucratic timekeeping, standardizing formal presentation). The theater is not minimal (0.0) because some performative element exists (symbolic markers of state authority), but the primary function is genuinely coordinating rather than purely theatrical.
 *
 * PERSPECTIVAL GAP:
 *   The traditional community perceives immutability of the constraint (Snare at biographical horizon) because their identity-locked exit option prevents seeing how other cohorts might voluntarily adopt the new practices. The state perceives mutability (Rope at immediate horizon) because it sees the mandate as solving a coordination problem that agents would want to solve. The analytical observer at civilizational horizon sees the constraint as hybrid (Tangled Rope) — the state genuinely solves coordination problems, but the cost distribution is asymmetric and the internalization pathway is contested. The perspectival gap reveals the reading's empirical vulnerability: if internalization does NOT occur endogenously, the constraint collapses from Tangled Rope toward Snare across all perspectives, falsifying the reading's core axiom.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from beneficiary/victim declarations and power/exit combinations. The state (institutional/arbitrage) receives low d (~0.10 → negative χ) because it is a beneficiary with exit options (can abandon the mandate if it chooses). Communities (powerless/identity_locked) receive high d (~0.92 → high χ) because they are victims with no practical exit — they cannot abandon lunar observance without abandoning identity, and they cannot resist the state's enforcement apparatus. The syncretizing urban cohort receives moderate d (~0.50 → moderate χ) because they are mixed beneficiary-victim with constrained exit (can adopt new practices publicly, retain old ones privately, but cannot fully escape either path without cost). The Analytical observer receives neutral d (~0.72 → moderate χ) because the observer position itself is exposed to the constraint's structure — the framework itself is built from an institutional context that may be naturalizing imposed practice.
 *
 * MANDATROPHY ANALYSIS:
 *   The endogenous climb reading resolves mandatrophy by claiming that the constraint can transition from enforcement-dependent extraction (Snare) to voluntary adoption (Rope) through building bottom-up adoption pathways. The scaffold perspective (imperial oversight) embodies this transition: the constraint is justified as a temporary mechanism with a sunset — enforcement suppression should decline as internalization deepens. The measurement trajectory supports this logic: extractiveness declines from 0.68 to 0.52 as syncretism increases, suggesting that voluntary adoption is indeed reducing the state's enforcement burden. However, the identity-locked exit option for traditional communities reveals the reading's limit: genuine internalization may not occur for agents whose identity is constituted through the old practices. The measurement trajectory could reflect demographic replacement (old-identity cohorts dying, new-identity cohorts replacing them) rather than internalization (old-identity cohorts changing their identity). This ambiguity is the core irreducible uncertainty (omega variables) that the reading cannot resolve without empirical data on identity-frame shifts within cohorts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    internalization_threshold_ambiguity,
    'What distinguishes genuine internalization (the community comes to see the new practice as legitimate/natural) from performative adoption (compliance without belief)?',
    'Longitudinal ethnographic data: inter-generational persistence of private vs public practice; shift in language/framing when explaining why the practice is maintained; reduction in enforcement apparatus proportional to claimed internalization',
    'If internalization threshold is crossed: constraint may reclassify toward Rope or Scaffold (coordination dominates extraction). If threshold is not crossed after enforcement: constraint is sustained by suppression alone (reclassify toward Snare). This directly determines whether the reading''s core axiom (endogenous adoption is possible) is vindicated or falsified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalization_threshold_ambiguity, empirical, 'Distinguishing genuine internalization from performative adoption').

omega_variable(
    bottom_up_adoption_pathway_existence,
    'Does the imposed practice create its own demand-side incentive for adoption, or does continued compliance depend entirely on enforcement suppression?',
    'Analysis of voluntary adoption rates in regions with low enforcement apparatus; comparison of compliance in relational networks (family, craft guilds) vs administrative hierarchies; measurement of informal transmission of the practice among peers absent official mandate',
    'If bottom-up pathways emerge: the reading is vindicated — the constraint can transition from enforcement-dependent to self-sustaining, enabling the scaffold perspective''s sunset logic. If pathways remain dependent on enforcement: the reading''s core premise fails, and the constraint cannot escape Snare or high-suppression Tangled Rope states.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(bottom_up_adoption_pathway_existence, empirical, 'Whether imposed practice generates endogenous demand for adoption').

omega_variable(
    identity_lock_reversibility,
    'Is the identity-lock binding the traditional community to lunar observance and customary dress permanent, or can generational shift break the cognitive fusion between identity and practice?',
    'Longitudinal generational data on practice retention and stated identity frames; measurement of identity-fusion strength across age cohorts; analysis of post-enforcement-withdrawal practice persistence',
    'If identity-lock is reversible within 2-3 generations: the reading supports transition to lower-extraction states as new cohorts are socialized into the imposed practices. If identity-lock persists across generations despite enforcement withdrawal: the reading fails — imposed practices cannot overcome cognitive capture, and extraction remains high indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_reversibility, empirical, 'Whether identity-lock to traditional practices is generationally reversible').

omega_variable(
    kernel_reading_contention,
    'Is this reading''s core claim — that communities can voluntarily adopt imposed practices through endogenous pathways — an empirical claim about historical causation or a normative commitment about legitimate state practice?',
    'Distinction between descriptive mechanism (does bottom-up adoption occur?) and prescriptive framework (should states rely on enforcement or consensual adoption pathways?). The reading may describe actual historical dynamics while being grounded in a contested normative commitment about what constitutes legitimate imposition.',
    'If primarily empirical: the reading''s truth value depends on whether internalization actually occurs in the historical case. If primarily normative: the reading coexists with the sibling readings as different commitments about legitimate practice, not as competing empirical claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contention, conceptual, 'Whether the reading is empirically falsifiable or normatively committed').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(endogenous_climb_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(endo_tr_t0, endogenous_climb_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(endo_tr_t20, endogenous_climb_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(endo_tr_t40, endogenous_climb_reading, theater_ratio, 40, 0.35).

% Extraction over time
narrative_ontology:measurement(endo_be_t0, endogenous_climb_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(endo_be_t20, endogenous_climb_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(endo_be_t40, endogenous_climb_reading, base_extractiveness, 40, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(endogenous_climb_reading, identity_coordination).
narrative_ontology:affects_constraint(endogenous_climb_reading, calendar_conversion_failure).
narrative_ontology:affects_constraint(endogenous_climb_reading, dress_code_partial_adoption).

% DUAL FORMULATION NOTE:
% This constraint is part of a family of three readings of the legitimacy_of_imposed_practice kernel. Each reading has its own ε value and structural properties. The endogenous_climb_reading (ε=0.52, Tangled Rope) emphasizes the possibility of voluntary adoption pathways. The exogenous_override_reading (separate file, ε=0.68, Snare) emphasizes top-down enforcement necessity. The hybrid_scaffolding_reading (separate file, ε=0.35, Scaffold) emphasizes transition mechanisms and sunset clauses. All three readings apply to the same observable (calendar/dress mandate history) but filter it through different normative frameworks about legitimacy. The three stories are linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
