% ============================================================================
% CONSTRAINT STORY: husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_husk_reading, []).

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
 *   constraint_id: husk_reading
 *   human_readable: Preparedness as Memorial Performance (Husk Reading)
 *   domain: disaster_preparedness/institutional_governance
 *
 * SUMMARY:
 *   The preparedness-as-performance constraint captures a structural dynamic
 *   in disaster governance where compliance ceremony becomes decoupled from
 *   actual response competence. Organizations invest heavily in drills,
 *   inspections, certifications, and audit trails that produce the appearance
 *   of readiness without ensuring the tacit skills and live improvisation
 *   capacity needed in real disasters. This constraint is one reading of a
 *   contested kernel about how preparedness should be retained and measured.
 *   The husk_reading instantiates the position that theater has crowded out
 *   competence: the ceremonial apparatus (scheduled drills, standardized
 *   checklists, compliance documentation) consumes resources and attention
 *   that would be better spent on live-action exercises, continuous skill
 *   validation, and tacit knowledge transmission. The theater ratio has risen
 *   from 0.60 to 0.82 over the measurement interval, indicating increasing
 *   emphasis on visible performance relative to functional capacity.
 *   Frontline responders and actual response capacity bear the extraction
 *   cost; institutional compliance administration and legitimacy narratives
 *   benefit.
 *
 * KEY AGENTS:
 *   - Frontline Responders: Primary victims (powerless/trapped) — drills consume training time but do not build lived competence; trapped in compliance regime with no exit
 *   - Institutional Compliance Administration: Primary beneficiary (institutional/arbitrage) — benefits from standardized metrics, audit trails, and liability discharge; can arbitrage between jurisdictions
 *   - Community Preparedness Coordinators: Secondary victim (moderate/constrained) — must balance genuine response coordination with top-down inspection mandates; constrained by funding tied to compliance scores
 *   - Professional Emergency Response Community: Organized secondary actor (organized/constrained) — benefits from shared coordination protocols but extraction occurs as compliance overhead
 *   - Preparedness Institutional Framework: Institutional degradation carrier (institutional/arbitrage) — NIMS, standardized protocols, certification systems persist through inertia; knows self is degraded (piton)
 *   - Actual Response Capacity: Structural victim (powerless/trapped) — abstract collective good that decays when ceremony displaces skill retention; cannot organize or advocate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(husk_reading, 0.58).
domain_priors:suppression_score(husk_reading, 0.65).
domain_priors:theater_ratio(husk_reading, 0.82).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(husk_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(husk_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(husk_reading, theater_ratio, 0.82).

% --- Constraint claim ---
narrative_ontology:constraint_claim(husk_reading, tangled_rope).
narrative_ontology:human_readable(husk_reading, "Preparedness as Memorial Performance (Husk Reading)").
narrative_ontology:topic_domain(husk_reading, "disaster_preparedness/institutional_governance").

domain_priors:requires_active_enforcement(husk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(husk_reading, '9739f21d-4c02-427a-ac10-68d24cef8a19').
narrative_ontology:cs_created_at('9739f21d-4c02-427a-ac10-68d24cef8a19', '').
narrative_ontology:cs_kernel_codification('9739f21d-4c02-427a-ac10-68d24cef8a19', implicit).
narrative_ontology:cs_authority_grounding('9739f21d-4c02-427a-ac10-68d24cef8a19', practice).
narrative_ontology:cs_interpretation_layer_present('9739f21d-4c02-427a-ac10-68d24cef8a19').
narrative_ontology:cs_kernel_id(husk_reading, preparedness_retention).
narrative_ontology:cs_reading_relation('9739f21d-4c02-427a-ac10-68d24cef8a19', competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('9739f21d-4c02-427a-ac10-68d24cef8a19', hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('9739f21d-4c02-427a-ac10-68d24cef8a19', foundational, ceremony_displaces_competence_under_resource_constraint).
narrative_ontology:cs_axiom_status(ceremony_displaces_competence_under_resource_constraint, holdable).
narrative_ontology:cs_axiom_grounding('9739f21d-4c02-427a-ac10-68d24cef8a19', ceremony_displaces_competence_under_resource_constraint, empirically_contingent).
narrative_ontology:cs_axiom('9739f21d-4c02-427a-ac10-68d24cef8a19', foundational, live_exercise_capacity_requires_sustained_investment).
narrative_ontology:cs_axiom_status(live_exercise_capacity_requires_sustained_investment, holdable).
narrative_ontology:cs_axiom_grounding('9739f21d-4c02-427a-ac10-68d24cef8a19', live_exercise_capacity_requires_sustained_investment, empirically_contingent).
narrative_ontology:cs_reference_frame('9739f21d-4c02-427a-ac10-68d24cef8a19', continuous_skill_retention_through_live_practice).
narrative_ontology:cs_drift_state('9739f21d-4c02-427a-ac10-68d24cef8a19', contemporary_budget_constrained_governance, gap(practice_drift, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(husk_reading, institutional_legitimacy).
narrative_ontology:constraint_beneficiary(husk_reading, compliance_administrators).
narrative_ontology:constraint_victim(husk_reading, actual_response_capacity).
narrative_ontology:constraint_victim(husk_reading, frontline_responders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FRONTLINE RESPONDER (SNARE) — Trapped in ceremonial drills that consume training hours but do not build lived competence. Cannot exit the compliance regime. Bears the extraction cost during actual disaster when improvised skill replaces practiced response. Maximum experienced extraction from a powerless position.
constraint_indexing:constraint_classification(husk_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: COMMUNITY PREPAREDNESS COORDINATOR (TANGLED_ROPE) — Genuinely coordinates disaster response capacity (real coordination function) while being constrained by top-down inspection requirements that prioritize compliance theater over skill retention. Benefits from the institutional apparatus (funding, authority) but extraction occurs as mandate misalignment — forced to demonstrate readiness via checklist rather than competence.
constraint_indexing:constraint_classification(husk_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: COMPLIANCE ADMINISTRATION (ROPE) — Benefits from the drill-and-inspection regime as a coordination mechanism: standardized performance metrics, documented compliance, audit trails that discharge institutional liability. Experiences the constraint as beneficial coordination (we know who passed inspection) rather than extraction. Can arbitrage between jurisdictions or between compliance roles.
constraint_indexing:constraint_classification(husk_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PREPAREDNESS INSTITUTIONAL FRAMEWORK (PITON) — The larger apparatus (emergency management doctrine, NIMS training protocols, certification standards) persists through institutional inertia. Theater ratio is extremely high (0.82): drills are ritual performances that feel like retention but lack live-action competence measures. The framework knows it is degraded — alternatives exist (live exercises, scenario-based training, continuous competence validation) but are not adopted because they cost more to administer. The framework maintains itself through theatrical compliance.
constraint_indexing:constraint_classification(husk_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: PROFESSIONAL EMERGENCY RESPONSE COMMUNITY (TANGLED_ROPE) — Organized actors (fire chiefs associations, emergency management councils) see both coordination and extraction. The shared protocols enable inter-jurisdictional response coordination (genuine benefit). But the compliance regime extracts institutional overhead — time spent on compliance documentation that could be spent on skill development. Constrained by state/federal funding tied to compliance metrics.
constraint_indexing:constraint_classification(husk_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational timescale, the decay from competence to ceremony is an immutable feature of institutional memory: all large organizations face the atrophy of tacit knowledge as experienced operators retire. Drills are the only mechanism available to combat this decay, so the ceremony is inherent to the constraint, not contingent. However, the structural data contradicts this — the high theater ratio (0.82) and beneficiary concentration suggest false summit: institutional arrangements are naturalizing what is actually a governance choice.
constraint_indexing:constraint_classification(husk_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(husk_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(husk_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(husk_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(husk_reading, TR),
    TR >= 0.70.

:- end_tests(husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The regime extracts institutional legitimacy and compliance benefits while actual response capacity decays. The extraction is real but not maximal — some genuine coordination happens alongside the theater. Organizations that score high on compliance may still maintain reasonable real competence through informal practices. Suppression (0.65): Moderate-high. Responders face significant barriers to exit: legal requirements for certification, funding tied to compliance metrics, institutional pressure to document readiness. But suppression is not total — informal skill networks persist, and some organizations successfully balance ceremony and competence. Theater ratio (0.82): Very high, and rising. Drills are ritual performances (scheduled, scripted, observed, documented) rather than live-action stress tests. Certifications are checkbox completion rather than continuous competence validation. The increasing ratio over the measurement interval reflects institutional trend: as budget pressures rise, organizations shift toward cheaper visible compliance rather than expensive live exercises. This trend is diagnosed as theater creep — not growth of functional capacity but increasing performative content to maintain legitimacy with lower actual investment.
 *
 * PERSPECTIVAL GAP:
 *   Each perspective reveals a different structural relationship to the same constraint. The frontline responder sees extraction (snare) because they bear the cost of ceremony without competence gain. The compliance administrator sees coordination (rope) because the regime efficiently produces verifiable metrics. The community coordinator sees mixed coordination and extraction (tangled_rope) because they genuinely coordinate response while being forced to prioritize compliance over skill. The organized emergency response community sees mixed extraction and coordination (tangled_rope) because shared protocols enable coordination but compliance overhead is real. The institutional framework sees itself as degraded (piton) — it knows the theater-to-function ratio is too high but maintains itself anyway. The analytical observer risks naturalizing the ceremony as inherent to preparedness (mountain), but the structural data reveals this as false summit: the theater level is not immutable but reflects institutional choices about resource allocation and legitimacy claims.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural positions. Frontline responders (trapped, powerless, no arbitrage exit) experience maximum extraction (d ≈ 0.95). Compliance administrators (institutional, arbitrage exit, beneficiary of legitimacy flow) experience minimal extraction (d ≈ 0.10). Community coordinators (moderate power, constrained exit, mixed victim/beneficiary status) experience moderate extraction (d ≈ 0.65). Professional response community (organized power, constrained exit by funding ties, net victim but with some coordination benefit) experience moderate extraction (d ≈ 0.55). The piton perspective experiences extraction through inertia — the system extracts institutional legitimacy from continued compliance while knowing it is degraded.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates mandatrophy resolution through the reading distinction. The husk_reading rejects the proposition that 'ceremony is necessary for preparedness' by instantiating a coherent alternative: theater is extractive bloat, not essential retention mechanism. The competence_reading (sibling, not generated here) would argue ceremony serves genuine retention function. These readings coexist because they reflect different institutional commitments about resource allocation priorities. The husk_reading resolves mandatrophy by accepting that high theater + moderate extraction is a real, coherent constraint type (tangled_rope) rather than claiming the regime is either pure extraction (snare) or pure coordination (rope). The theater is real; the extraction is real; both coexist structurally.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_measurement_validity,
    'What valid measurement separates genuine disaster response competence from ceremonial performance in drills?',
    'Post-incident analysis: correlation between drill performance scores and actual incident response outcomes; identification of competencies demonstrated in drills vs. those that emerge only under real-event stress; longitudinal tracking of organizations with high drill scores vs. actual response metrics (fatalities, response time, coordination success)',
    'If drills correlate strongly with real competence: husk reading is overstated — ceremony provides real retention. If correlation is weak or negative: husk reading is confirmed — theater crowds out genuine skill development.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_measurement_validity, empirical, 'Validity of drill performance as a predictor of actual response competence').

omega_variable(
    alternative_retention_sufficiency,
    'Do live-action exercises, scenario-based training, or continuous competence validation (alternatives to ceremony-heavy drills) actually produce better-retained response capacity at sustainable administrative cost?',
    'Comparative study: jurisdictions using high-ceremony/low-live-exercise regimes vs. high-live-exercise regimes; measurement of response competence, administrative burden, equipment costs, personnel time; longitudinal tracking of knowledge retention curves post-training',
    'If alternatives are superior and sustainable: the ceremony regime is extractive rather than necessary — husk reading confirmed. If alternatives are insufficient or unsustainable: some theatrical compliance may be unavoidable cost of scaled preparedness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_retention_sufficiency, empirical, 'Whether alternative training modalities provide superior competence retention').

omega_variable(
    reading_distinction_empirical_status,
    'Is the distinction between husk_reading (ceremony-focused, high theater) and competence_reading (skill-focused, low theater) a matter of empirical fact about what actually retains competence, or a matter of institutional commitment and resource allocation choices?',
    'This is routed as a conceptual omega rather than empirical because the readings coexist in different institutional framings. Husk_reading assumes competence decays unless actively retained through live practice; competence_reading assumes standardized drills create sufficient institutional memory. Both are coherent theories with empirical implications. The question is whether the choice between them is determined by evidence or by values/commitments.',
    'If empirical: whichever reading''s predictions match post-incident outcomes is correct. If conceptual: the readings reflect different institutional commitments (compliance vs. effectiveness) that no amount of evidence can reconcile without value shift.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_distinction_empirical_status, conceptual, 'Whether reading distinction is empirically determined or reflects institutional value commitments').

omega_variable(
    institutional_legitimacy_beneficiary,
    'Does the preparedness regime derive its legitimacy from actual disaster response capacity, or from institutional compliance and audit trail demonstration?',
    'Institutional analysis: when preparedness regimes have faced genuine disasters, did compliance documentation matter to survival outcomes? Which institutions that scored high on compliance audits also performed well in real events? When institutions scored low on compliance but high on skill, did real-event outcomes reflect skill or compliance status?',
    'If legitimacy is tied to compliance documentation: beneficiary identification (institutional_legitimacy) is correct, and extraction mechanism is validated. If legitimacy is tied to actual response outcome: the beneficiary is misidentified — what appears as compliance is actually an instrumental tool for genuine response, not an extraction target.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_legitimacy_beneficiary, empirical, 'Whether preparedness regime legitimacy derives from compliance or actual response capacity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(husk_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(husk_tr_t0, husk_reading, theater_ratio, 0, 0.6).
narrative_ontology:measurement(husk_tr_t5, husk_reading, theater_ratio, 5, 0.72).
narrative_ontology:measurement(husk_tr_t10, husk_reading, theater_ratio, 10, 0.82).
narrative_ontology:measurement(husk_tr_t15, husk_reading, theater_ratio, 15, 0.85).

% Extraction over time
narrative_ontology:measurement(husk_be_t0, husk_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(husk_be_t5, husk_reading, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(husk_be_t10, husk_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(husk_be_t15, husk_reading, base_extractiveness, 15, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(husk_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(husk_reading, competence_reading).
narrative_ontology:affects_constraint(husk_reading, hybrid_reading).

% DUAL FORMULATION NOTE:
% The preparedness_retention kernel decomposes into three constraint stories: husk_reading (ceremony has displaced competence, theater is extractive), competence_reading (ceremony serves genuine institutional memory function), and hybrid_reading (both mechanisms operate; optimization is empirically determinable). Each reading has distinct extractiveness, distinct beneficiary/victim structure, and distinct institutional commitments. The three stories are linked by network.affects_constraints to indicate they are sibling readings of the same kernel, not separate constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
