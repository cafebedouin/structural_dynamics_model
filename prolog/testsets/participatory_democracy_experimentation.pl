% ============================================================================
% CONSTRAINT STORY: participatory_democracy_experimentation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_participatory_democracy_experimentation, []).

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
 *   constraint_id: participatory_democracy_experimentation
 *   human_readable: Participatory Democracy Experimentation Constraint
 *   domain: political_governance/democratic_innovation
 *
 * SUMMARY:
 *   Participatory democracy experimentation — citizen assemblies,
 *   participatory budgeting, deliberative forums, civic tech platforms —
 *   represents a structural tension between the aspiration for inclusive
 *   governance and the institutional machinery that sustains representative
 *   democracy. The constraint exhibits genuine coordination function (solving
 *   the legitimacy and information problems of representative systems)
 *   alongside systematic extraction (concentrating voice-bearing among
 *   already-engaged citizens, outsourcing decision costs to volunteers,
 *   enabling deniability extraction by elites). The measurement trajectory
 *   shows increasing theater_ratio and extractiveness over the interval: as
 *   experiments mature, they professionalize (higher theater), develop
 *   repeat-participant communities (higher extraction from non-participants),
 *   and become embedded in political strategy (higher deniability
 *   extraction). The constraint is tangled_rope at the core but exhibits all
 *   six types from different structural positions, making it diagnostic for
 *   understanding how democratic innovation can both enable voice and
 *   reproduce exclusion.
 *
 * KEY AGENTS:
 *   - Facilitating Institutions: Primary beneficiary (institutional/arbitrage) — gain legitimacy, distributed problem-solving, cost externalization
 *   - Professional Facilitators: Primary beneficiary (institutional/arbitrage) — market demand, consulting fees, career advancement
 *   - Engaged Participants: Secondary beneficiary and victim (moderate/constrained) — gain voice and agency but bear time and emotional labor costs
 *   - Non-Participating Citizens: Primary victim (powerless/trapped) — excluded by structural barriers; bear costs of experiments designed without their input
 *   - Marginalized Communities: Primary victim (powerless/identity_locked) — structurally mobile but identity-locked by distrust and internalized exclusion
 *   - Political Elites: Secondary beneficiary (powerful/mobile) — gain legitimacy and deniability while retaining actual power; can exit if politically inconvenient
 *   - Democratic Innovation Movement: Organized perspectives (organized/constrained) — see experiments as transitional scaffold with sunset as digital governance matures
 *   - Liberal Democratic Ritual: Civilizational view (institutional/constrained) — participatory forms maintain fiction of popular rule while actual power remains concentrated
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(participatory_democracy_experimentation, 0.52).
domain_priors:suppression_score(participatory_democracy_experimentation, 0.48).
domain_priors:theater_ratio(participatory_democracy_experimentation, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(participatory_democracy_experimentation, extractiveness, 0.52).
narrative_ontology:constraint_metric(participatory_democracy_experimentation, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(participatory_democracy_experimentation, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(participatory_democracy_experimentation, tangled_rope).
narrative_ontology:human_readable(participatory_democracy_experimentation, "Participatory Democracy Experimentation Constraint").
narrative_ontology:topic_domain(participatory_democracy_experimentation, "political_governance/democratic_innovation").

domain_priors:requires_active_enforcement(participatory_democracy_experimentation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(participatory_democracy_experimentation, facilitating_institutions).
narrative_ontology:constraint_beneficiary(participatory_democracy_experimentation, professional_facilitators).
narrative_ontology:constraint_beneficiary(participatory_democracy_experimentation, political_elites).
narrative_ontology:constraint_victim(participatory_democracy_experimentation, non_participating_citizens).
narrative_ontology:constraint_victim(participatory_democracy_experimentation, marginalized_communities).
narrative_ontology:constraint_victim(participatory_democracy_experimentation, representative_democracy_legitimacy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED NON-PARTICIPANT (SNARE) — Citizens without time, literacy, or cultural capital to engage in participatory forums bear costs of experimental governance without voice. Trapped by structural barriers (work schedule, childcare, language, civic confidence). Maximum extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(participatory_democracy_experimentation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: MARGINALIZED COMMUNITY MEMBER (SNARE) — Structurally mobile (could attend meetings, could organize) but identity-locked by prior exclusion, distrust of institutions, and internalized belief that 'people like us don't participate in politics.' Extraction mechanism relies on internalized exclusion, not material barriers.
constraint_indexing:constraint_classification(participatory_democracy_experimentation, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 3: ENGAGED PARTICIPANT (TANGLED ROPE) — Benefits from voice in decision-making (genuine coordination gain) but also experiences extraction: time burden, emotional labor, tokenism risk (consulted but decisions made elsewhere). Exit costly due to social commitment and civic identity.
constraint_indexing:constraint_classification(participatory_democracy_experimentation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: FACILITATING INSTITUTION (ROPE) — Municipal government, NGO, or civic tech platform benefits from participatory experiments (increased legitimacy, access to distributed intelligence, cost externalization to volunteers). Experiences constraint as pure coordination mechanism. Can exit by reverting to representative process if beneficial; high arbitrage optionality.
constraint_indexing:constraint_classification(participatory_democracy_experimentation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: PROFESSIONAL FACILITATOR (ROPE) — Consultants, community organizers, and facilitation experts benefit from market demand for participatory process design. Experiences constraint as opportunity (coordination problem to solve). Can arbitrage into other markets if participatory democracy loses funding.
constraint_indexing:constraint_classification(participatory_democracy_experimentation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: POLITICAL ELITE (TANGLED ROPE) — Benefits from participatory experiments when they generate legitimacy and distribute unpopular decisions to 'the people' (deniability extraction). Also constrained by need to maintain representative democracy fiction and implement decisions made in parallel representative channels. Mobile exit option (can abandon participatory experiment if politically convenient).
constraint_indexing:constraint_classification(participatory_democracy_experimentation, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: DEMOCRATIC INNOVATION MOVEMENT (SCAFFOLD) — Global network of civic technologists, political scientists, and activists see participatory experiments as transitional scaffolding toward more inclusive governance. Perceive the constraint as temporary: as digital literacy, civic infrastructure, and inclusive design improve, the need for managed participatory experiments will decline. Structured sunset as deliberative quality improves.
constraint_indexing:constraint_classification(participatory_democracy_experimentation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: LIBERAL DEMOCRATIC RITUAL (PITON) — From a civilizational view, participatory democracy experiments maintain the ritual of 'rule by the people' while actual power allocation remains concentrated. Theater_ratio high: the ceremonies of participation (forums, votes, deliberation) persist through institutional inertia and legitimacy needs, not because they fundamentally alter governance. The constraint degrades as real power hollows out the participatory form.
constraint_indexing:constraint_classification(participatory_democracy_experimentation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(participatory_democracy_experimentation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(participatory_democracy_experimentation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(participatory_democracy_experimentation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(participatory_democracy_experimentation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(participatory_democracy_experimentation, TR),
    TR >= 0.70.

:- end_tests(participatory_democracy_experimentation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint benefits facilitating institutions and professional facilitators substantially while imposing costs on non-participants (who bear governance decisions made without their input) and engaged participants (who bear time and emotional labor). The extraction is not total — genuine voice gains exist for engaged participants — but the participation ceiling and selection bias mean that the demos is not actually enlarged, only rearranged. Suppression (0.48): Moderate. Barriers to non-participation include time burden (structural), language/literacy (structural), childcare (structural), civic confidence (internalized), distrust of institutions (identity), and geographic/digital access (structural). Not all can be removed by design alone; some are identity-locked. Theater ratio (0.58): Moderate-high. Participatory forums perform deliberation and decision-making rituals; actual power allocation follows separate (representative/elite) processes. The constraint exhibits significant theater: forum ceremonies, voting procedures, and deliberation protocols persist partly for legitimacy rather than function.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap centers on the question: 'Does this constraint expand democratic voice or rationalize its concentration?' The engaged participant sees expansion (tangled_rope — mixed benefits and costs). The facilitating institution sees coordination (rope — solving legitimacy problem). The non-participant sees extraction (snare — decisions made without them). The elite sees opportunity (rope — deniability and legitimacy without power loss). The marginalized sees identity lock (snare via internalized exclusion). The democratic innovator sees transitional scaffolding (sunset as inclusive design matures). The civilizational observer sees degraded ritual (piton — performative participation masking concentrated power). The perspectival gap reveals that 'participatory democracy' is not a single constraint but a family of distinct extraction mechanisms (one per structural position) coordinated under a unifying label.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's experienced extractiveness (chi) is computed from their base extraction (0.52), their directionality value (d) derived from their structural position, and the scope modifier. Facilitators with arbitrage options derive low d → chi approaches zero (negative, coordinative experience). Non-participants with trapped exit derive high d → chi amplified (high experienced extraction). Engaged participants with constrained exit derive mid-range d → chi moderate. Elites are overridden upward (deniability extraction mechanism not captured by base derivation) → chi raised. Marginalized with identity_locked exit: if identity frame shifts, d drops from ~0.89 to ~0.60, and chi drops correspondingly — revealing that the extraction is partly cognitive rather than structural.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint avoids false conflation of coordination and extraction by decomposing perspectival experiences. It is NOT 'coordination that has extraction problems' (false rope). It is NOT 'pure extraction dressed as democracy' (false snare simplification). It IS a tangled_rope with genuine coordination function (solving representative democracy's legitimacy deficit) alongside genuine asymmetric extraction (concentrating voice among already-engaged, distributing costs to volunteers and non-participants, enabling deniability extraction by elites). The mandatrophy resolves by accepting the multiplicity: each agent's experience is accurate from their position. Facilitation institutions genuinely solve a coordination problem. Non-participants genuinely bear costs without voice. Elites genuinely extract deniability. All are simultaneously true. The error would be in claiming a single truth ('participatory democracy is good coordination' or 'participatory democracy is elite extraction theater'). The framework shows why both readings are structurally correct — from different positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    participation_selection_bias,
    'Are participatory forums generating genuinely representative deliberation or reproducing participation bias favoring already-engaged citizens?',
    'Demographic comparison of forum participants vs broader citizenry; tracking of repeat-participant bias; analysis of socioeconomic composition changes over time',
    'If selection bias is severe: constraint functions as snare for non-participants (extraction via false representation). If selection can be addressed: constraint trends toward rope (genuine coordination).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(participation_selection_bias, empirical, 'Whether participatory forums achieve representative participation or reproduce bias').

omega_variable(
    implementation_fidelity,
    'Are decisions made in participatory forums actually implemented, or does the constraint function as a legitimacy theater masking pre-determined outcomes?',
    'Audit of implementation rates; comparison of forum outputs vs official policy decisions; tracking of overruled or reinterpreted participatory outcomes',
    'If implementation is high: constraint is genuinely tangled_rope (coordination with asymmetry). If implementation is low: constraint is piton (performative degradation) or snare (extraction via false participation).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(implementation_fidelity, empirical, 'Whether participatory decisions are actually implemented').

omega_variable(
    identity_lock_mechanisms,
    'What specific internalized beliefs prevent marginalized citizens from engaging despite structural mobility (time off work available, translation services provided)?',
    'Qualitative interviews with non-participants; identity frames analysis; tracking of participation changes when internalized barriers are explicitly addressed vs structural barriers removed',
    'If identity lock is primary: exit from non-participation requires identity reframing, not just removing material barriers. Interventions must address internalized exclusion, not just logistics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanisms, conceptual, 'Identity lock mechanisms in participatory exclusion').

omega_variable(
    scalability_ceiling,
    'Does participatory deliberation quality degrade below a critical threshold as participation scale increases?',
    'Quality metrics across forum sizes; deliberation depth analysis; decision complexity handling at different scales; identification of quality degradation patterns',
    'If ceiling exists: scaffold perspective limited — participatory experiments may be transitional but cannot replace representative democracy. If no ceiling: scaffold represents genuine path to inclusive governance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scalability_ceiling, empirical, 'Scalability limits of participatory deliberation quality').

omega_variable(
    extraction_via_deniability,
    'Do political elites use participatory processes to shift accountability for unpopular decisions (''we consulted the people'') while retaining actual power?',
    'Tracking of decision reversals; analysis of unpopular vs popular decision implementation rates; interviews with both participants and elites about decision-making; accountability tracing',
    'If extraction via deniability is systematic: elite perspective classifies higher on victimhood scale — they extract from the constraint via legitimacy gain while bearing no real constraint on power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_via_deniability, empirical, 'Extraction through deniability in participatory decision-making').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(participatory_democracy_experimentation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pde_tr_t0, participatory_democracy_experimentation, theater_ratio, 0, 0.42).
narrative_ontology:measurement(pde_tr_t3, participatory_democracy_experimentation, theater_ratio, 3, 0.52).
narrative_ontology:measurement(pde_tr_t6, participatory_democracy_experimentation, theater_ratio, 6, 0.58).
narrative_ontology:measurement(pde_tr_t9, participatory_democracy_experimentation, theater_ratio, 9, 0.58).

% Extraction over time
narrative_ontology:measurement(pde_be_t0, participatory_democracy_experimentation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(pde_be_t3, participatory_democracy_experimentation, base_extractiveness, 3, 0.46).
narrative_ontology:measurement(pde_be_t6, participatory_democracy_experimentation, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(pde_be_t9, participatory_democracy_experimentation, base_extractiveness, 9, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(participatory_democracy_experimentation, identity_coordination).
narrative_ontology:affects_constraint(participatory_democracy_experimentation, representative_democracy_legitimacy_crisis).
narrative_ontology:affects_constraint(participatory_democracy_experimentation, civic_participation_inequality).
narrative_ontology:affects_constraint(participatory_democracy_experimentation, digital_governance_infrastructure).

% DUAL FORMULATION NOTE:
% Participatory democracy experimentation is downstream of representative democracy's legitimacy crisis but represents a structurally distinct constraint. It enables voice expansion for some while reproducing exclusion for others. Related constraints: civic_participation_inequality (extraction via access barriers), representative_democracy_legitimacy_crisis (coordination problem participatory experiments attempt to solve), digital_governance_infrastructure (enabling technology with separate extractiveness profile).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(participatory_democracy_experimentation, powerful, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
