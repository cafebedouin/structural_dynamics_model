% ============================================================================
% CONSTRAINT STORY: aneyoshi_land_use_prohibition__commemorative_husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_land_use_prohibition__commemorative_husk_reading, []).

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
 *   constraint_id: aneyoshi_land_use_prohibition__commemorative_husk_reading
 *   human_readable: Aneyoshi Land-Use Prohibition as Commemorative Husk (Operationally Inert Reading)
 *   domain: disaster_anthropology/institutional_commitment_decay
 *
 * SUMMARY:
 *   The Aneyoshi stone in Iwate Prefecture, Japan, bears an inscription
 *   warning against tsunami: a directive from ancestors to resist building in
 *   the zone and instead relocate settlements to higher ground. The
 *   commemorative_husk_reading interprets this stone as a memorial artifact
 *   whose literal directive force has decayed to symbolic acknowledgment. The
 *   stone is honored in ritual and cultural memory; community members and
 *   visitors reverently reference its warning; institutional authorities
 *   maintain it as a heritage site. Yet land-use decisions in Aneyoshi
 *   proceed independently of the prohibition. The neighboring village of
 *   Yoshihama, with an equivalent stone, built below the line and suffered
 *   catastrophic losses in the 2011 Tōhoku tsunami. This counterfactual—that
 *   communities with equivalent stones made independent decisions resulting
 *   in disaster—is the structural evidence that in this reading, the stone's
 *   authority has been severed from behavior. The extractiveness rises over
 *   the 60-year interval as theater increases (ritual grows more elaborate)
 *   and suppression decreases (fewer community members know the stone's
 *   original enforcement mechanism). The constraint operates as a snare:
 *   institutional authorities benefit from the memorial's cultural capital
 *   while future residents remain at risk, believing the stone's warning is
 *   being operationally followed when it has been reduced to commemoration.
 *
 * KEY AGENTS:
 *   - Future Residents of Aneyoshi: Primary victims (powerless/trapped) — structurally exposed to tsunami risk while the memorial's performance suggests protection
 *   - Institutional Authority (Heritage Administration): Primary beneficiaries (institutional/arbitrage) — curates the stone as cultural memory; garners legitimacy and identity continuity from maintaining the memorial
 *   - Land Developers and Municipal Planners: Secondary beneficiary-victims (moderate/constrained) — benefit from weakened constraint on development; constrained by need to maintain cultural respect for the memorial
 *   - Neighboring Villages (Yoshihama etc.): Structural evidence — built below the line despite having equivalent stones; suffered catastrophic losses — falsifies the behavioral_competence_reading, supports the commemorative_husk_reading
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — observes the theater-ratio trajectory and extractiveness rise, diagnosing institutional inertia masking as cultural continuity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_land_use_prohibition__commemorative_husk_reading, 0.68).
domain_priors:suppression_score(aneyoshi_land_use_prohibition__commemorative_husk_reading, 0.52).
domain_priors:theater_ratio(aneyoshi_land_use_prohibition__commemorative_husk_reading, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__commemorative_husk_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_land_use_prohibition__commemorative_husk_reading, snare).
narrative_ontology:human_readable(aneyoshi_land_use_prohibition__commemorative_husk_reading, "Aneyoshi Land-Use Prohibition as Commemorative Husk (Operationally Inert Reading)").
narrative_ontology:topic_domain(aneyoshi_land_use_prohibition__commemorative_husk_reading, "disaster_anthropology/institutional_commitment_decay").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_land_use_prohibition__commemorative_husk_reading, 'b8003f36-3156-476d-9934-5b26b13e2acf').
narrative_ontology:cs_kernel_codification('b8003f36-3156-476d-9934-5b26b13e2acf', fixed_text).
narrative_ontology:cs_authority_grounding('b8003f36-3156-476d-9934-5b26b13e2acf', extraction).
narrative_ontology:cs_interpretation_layer_present('b8003f36-3156-476d-9934-5b26b13e2acf').
narrative_ontology:cs_reading_relation('b8003f36-3156-476d-9934-5b26b13e2acf', aneyoshi_land_use_prohibition__behavioral_competence_reading, coexists_with).
narrative_ontology:cs_axiom('b8003f36-3156-476d-9934-5b26b13e2acf', foundational, memorial_function_decoupled_from_directive_force).
narrative_ontology:cs_axiom_status(memorial_function_decoupled_from_directive_force, holdable).
narrative_ontology:cs_axiom_grounding('b8003f36-3156-476d-9934-5b26b13e2acf', memorial_function_decoupled_from_directive_force, empirically_contingent).
narrative_ontology:cs_axiom('b8003f36-3156-476d-9934-5b26b13e2acf', secondary, institutional_extraction_via_cultural_legitimacy).
narrative_ontology:cs_axiom_status(institutional_extraction_via_cultural_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('b8003f36-3156-476d-9934-5b26b13e2acf', institutional_extraction_via_cultural_legitimacy, conventional).
narrative_ontology:cs_reference_frame('b8003f36-3156-476d-9934-5b26b13e2acf', lineage_authority_operationally_enforced).
narrative_ontology:cs_drift_state('b8003f36-3156-476d-9934-5b26b13e2acf', contemporary_heritage_curated, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b8003f36-3156-476d-9934-5b26b13e2acf', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(aneyoshi_land_use_prohibition__commemorative_husk_reading, aneyoshi_land_use_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_land_use_prohibition__commemorative_husk_reading, institutional_authority_preserving_ritual).
narrative_ontology:constraint_beneficiary(aneyoshi_land_use_prohibition__commemorative_husk_reading, commemorative_narrative_curators).
narrative_ontology:constraint_victim(aneyoshi_land_use_prohibition__commemorative_husk_reading, future_generations_at_risk).
narrative_ontology:constraint_victim(aneyoshi_land_use_prohibition__commemorative_husk_reading, community_land_use_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FUTURE RESIDENTS (SNARE) — Structurally trapped by the memorial's performative status. The stone exists; the prohibition is symbolically honored in commemoration; but land-use decisions proceed independently of the directive. Residents occupy the risk zone with no operative constraint. Maximum experienced extraction: the community celebrates the stone's warning while remaining exposed to the hazard it was meant to prevent. No exit from the geographic and temporal location; no operative constraint to guide behavior.
constraint_indexing:constraint_classification(aneyoshi_land_use_prohibition__commemorative_husk_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: LAND DEVELOPERS AND PLANNERS (SNARE) — Constrained but not trapped. They perceive the stone as a memorial artifact, not an operative land-use prohibition. The prohibition has decayed to ritual acknowledgment; developers can justify building in the zone by invoking local autonomy, economic development, and the memorial's historical rather than prescriptive status. Extraction: the stone's authority has been sufficiently delegitimized that development proceeds unimpeded, while the memorial absorbs community grief and institutional legitimacy. Constrained exit (not trapped) because planners could invoke the prohibition if political will existed, but the symbolic hollowing of the directive makes this invisible.
constraint_indexing:constraint_classification(aneyoshi_land_use_prohibition__commemorative_husk_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INSTITUTIONAL AUTHORITY / COMMEMORATION CURATORS (ROPE) — Benefits from the stone's status as memorial. The prohibition-as-cultural-memory serves institutional purposes: it preserves historical narrative, maintains community identity through shared loss, provides ritual practice, and garners tourism and cultural capital. The authority has arbitrage options — it can frame the stone as either living constraint or historical memorial depending on political context. Net beneficiary. The constraint operates as pure coordination for this perspective: sustaining the meaning of the memorial is genuine collective action. Institutional actors see rope because the stone's memorial function IS coordinated.
constraint_indexing:constraint_classification(aneyoshi_land_use_prohibition__commemorative_husk_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (PITON) — From the civilizational scale, the stone's prohibition persists through institutional inertia and theater. The directive has lost functional force (operationally inert) but persists because: (a) the memorial ritual sustains community identity and grief processing, (b) the local authority benefits from demonstrating continuity with historical wisdom, (c) dismantling the prohibition would require explicit rejection of the founding ancestors' warning and is politically infeasible. The piton classification derives from high theater (0.81): the stone's role is primarily performative (mourning, cultural continuity, legitimacy display) rather than functional (actual land-use governance). The constraint persists as institutional theater.
constraint_indexing:constraint_classification(aneyoshi_land_use_prohibition__commemorative_husk_reading, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_land_use_prohibition__commemorative_husk_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(aneyoshi_land_use_prohibition__commemorative_husk_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(aneyoshi_land_use_prohibition__commemorative_husk_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(aneyoshi_land_use_prohibition__commemorative_husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(aneyoshi_land_use_prohibition__commemorative_husk_reading, TR),
    TR >= 0.70.

:- end_tests(aneyoshi_land_use_prohibition__commemorative_husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High, rising over interval. In the commemorative_husk_reading, the stone's authority has decayed from operational constraint (behavioral_competence_reading, ε ≈ 0.08–0.15) to memorial artifact (ε ≈ 0.68). The extractiveness measures the gap between the stone's symbolic legitimacy (revered, honored, integrated into community identity) and its operative force (ignored in development decisions, absent from land-use governance). The beneficiaries are institutional authorities who extract cultural and legitimacy value from maintaining the memorial while planners operate independently. Suppression (0.52): Moderate, declining over interval. Initial suppression is high (0.75) because the stone's original authority rested on community practice and behavioral enforcement — to resist the directive required social friction and explicit rejection of ancestral wisdom. Over time, suppression declines (to 0.52) because the memorial's reframing as cultural artifact rather than binding constraint allows planners to build independently without explicit violation. The operative suppression (what prevents the original directive from governing behavior) is not a single enforcement mechanism but rather the institutional hollow-out of the directive's authority. Theater ratio (0.81): High, rising steeply. Initial theater is moderate (0.35) when the stone is a live constraint — behavior matches directive, ritual reflects actual practice. Over 60 years, theater rises as the ritual grows more elaborate (commemoration ceremonies, heritage site designation, scholarly attention) while the operative force declines. At t=60, the theater is maximal (0.81): the stone's meaning is almost entirely performative (mourning, cultural identity, institutional legitimacy) rather than functional (land-use governance). The measurements track the Piton-ward drift: a constraint that was operationally functional degraded to a constraint maintained through institutional inertia and performance.
 *
 * PERSPECTIVAL GAP:
 *   The gap between the commemorative_husk_reading and the behavioral_competence_reading is profound and structural. Under behavioral_competence (the sibling reading), the stone IS an operative land-use constraint; the community successfully resists building in the zone; extractiveness is low (0.08–0.15); classification is rope or mountain (coordination or natural law). The gap emerges because the two readings disagree about what the stone's persistent presence means. Husk reading: persistence reflects institutional inertia and memorial maintenance, not operative force. Competence reading: persistence reflects community practice successfully enforcing the ancestral directive. The empirical test is counterfactual-comparative: neighboring villages with equivalent stones built below the line and suffered catastrophic losses. This falsifies the competence reading and confirms the husk reading. But the husk reading's high extractiveness (0.68) derives from the institutional authorities' benefit — they maintain the memorial's symbolic authority while planners operate independently. This is not a perspectival gap in the classical sense (one observer sees snare, another sees rope from the same structural position). Rather, it is a kernel reading gap: two internally coherent framings of the stone's meaning that produce incommensurable constraint types.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from structural position relative to the extraction flow. Future residents (trapped/powerless) experience high d ≈ 0.95 (full targets) because they are exposed to the hazard with no operative constraint and no exit. Institutional authorities (institutional/arbitrage) experience low d ≈ 0.05 (full beneficiaries) because they extract cultural legitimacy while the memorial's operative force has decayed. Planners (moderate/constrained) experience intermediate d ≈ 0.60 (partial targets and beneficiaries) because they benefit from the loosened constraint but are constrained by cultural respect for the memorial. The sigmoid f(d) amplifies the target's experienced extractiveness and dampens the beneficiary's, which is appropriate: those at risk experience the full weight of the inert directive, while institutional curators experience the extraction as subtle and invisible (their benefit consists of image and legitimacy, not direct resource flow).
 *
 * MANDATROPHY ANALYSIS:
 *   The snare classification resolves potential mandatrophy by identifying who benefits from the stone's memorial status and who bears the cost of its operational inertia. Institutional authorities benefit (low d, arbitrage exit); future residents bear cost (high d, trapped exit). The extractiveness is not symmetric — it is concentrated on the powerless. Mandatrophy would arise if the reading tried to frame the memorial as purely coordinating (rope) — that would obscure the asymmetric risk distribution. The husk reading prevents mandatrophy by declaring victims (future generations at risk) explicitly and tracking the extractiveness rise over time as theater increases and suppression of alternative land-use practices declines.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    operational_decay_trajectory,
    'Did the stone''s operative force decay gradually after initial institutional abandonment, or was there a sharp pivot point where it was deliberately reframed from constraint to memorial?',
    'Archival analysis of administrative records, land-use decisions, and public statements across decade-scale intervals; oral history tracking when the prohibition stopped being invoked in development discussions; comparison with similar disaster-marker stones in other regions',
    'Gradual decay: extractiveness trajectory shows steady decline in suppression (constraint loosened over time). Sharp pivot: extractiveness shows a threshold event where the constraint category shifts from enforced prohibition to cultural memory. Classification remains snare in both cases, but the omega distinguishes passive decay from active reframing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operational_decay_trajectory, empirical, 'Mechanism of transition from operative constraint to memorial artifact').

omega_variable(
    neighboring_village_counterfactual,
    'Would Aneyoshi''s land-use pattern differ measurably if an equivalent stone were absent, given that the neighboring villages (with stones) built below the line and suffered catastrophic losses?',
    'Comparative analysis: settlement density, building footprint, economic activity above vs. below the line in Aneyoshi vs. villages with equivalent stones; disaster simulations assessing vulnerability difference; interviews with planners about role of stone in decisions',
    'If pattern would be identical: the stone''s presence has zero marginal effect on behavior — pure theater, highest snare reading. If pattern differs meaningfully: residual behavioral force persists despite institutional decay — tangled rope reading gains ground. If neighboring villages'' losses contradict Aneyoshi''s safety: the membrane between remembrance and competence is completely severed, snare confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(neighboring_village_counterfactual, empirical, 'Marginal causal effect of stone''s presence on actual land-use decisions').

omega_variable(
    kernel_reading_contest,
    'Is the stone a live operative constraint that the community successfully follows (behavioral_competence_reading), or a commemorative artifact whose directive force has decayed to symbolic acknowledgment (commemorative_husk_reading)?',
    'This is the omega routing the kernel contest to the apparatus per Rule 2. Evidence: (a) administrative decisions post-date stone placement but ignore the prohibition in land-use approvals — supports husk reading; (b) comparative case studies showing villages with stones building BELOW the line and suffering losses — falsifies live-constraint reading; (c) community members'' stated rationale for building decisions (memorial vs. operational) — epistemic; (d) actual building pattern relative to the stone''s line — structural.',
    'If behavioral_competence_reading holds: the stone IS operationally enforced through community practice; extractiveness drops to 0.15–0.25 (rope or mountain); snare classification dissolves. If commemorative_husk_reading holds: the stone''s ritual authority obscures its operational inertia; extractiveness remains high (0.65+); snare classification confirmed. The two readings coexist but produce structurally distinct constraint typologies.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, empirical, 'Core kernel contest: is stone a live constraint or commemorative husk?').

omega_variable(
    authority_grounding_shift,
    'Did the stone''s authority ground shift from lineage-plus-practice (ancestors'' warning embedded in community behavior) to distributed-plus-extraction (institutions preserve the memorial for cultural capital while planners operate independently)?',
    'Comparative analysis of authority invocation: early administrative records citing the stone as binding; later records omitting it from land-use justifications; oral histories of community enforcement mechanisms in early period vs. their absence later; analysis of who currently speaks for the stone''s meaning (institutional curators vs. community practitioners)',
    'If shift occurred: cs_structure authority_grounding was lineage/practice → now distributed/extraction. The kernel (the stone''s directive) persists but the authority structure changed. Explanation for extractiveness rise: institutional extraction of symbolic value while operational force decays. If no shift: authority was always distributed; the husk reading is original state, not decay.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_grounding_shift, empirical, 'Historical shift in authority grounding for the stone''s meaning').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_land_use_prohibition__commemorative_husk_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aneyoshi_husk_tr_t0, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(aneyoshi_husk_tr_t20, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 20, 0.58).
narrative_ontology:measurement(aneyoshi_husk_tr_t40, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 40, 0.75).
narrative_ontology:measurement(aneyoshi_husk_tr_t60, aneyoshi_land_use_prohibition__commemorative_husk_reading, theater_ratio, 60, 0.81).

% Extraction over time
narrative_ontology:measurement(aneyoshi_husk_be_t0, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(aneyoshi_husk_be_t20, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 20, 0.35).
narrative_ontology:measurement(aneyoshi_husk_be_t40, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 40, 0.55).
narrative_ontology:measurement(aneyoshi_husk_be_t60, aneyoshi_land_use_prohibition__commemorative_husk_reading, base_extractiveness, 60, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(aneyoshi_husk_su_t0, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(aneyoshi_husk_su_t20, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(aneyoshi_husk_su_t40, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 40, 0.58).
narrative_ontology:measurement(aneyoshi_husk_su_t60, aneyoshi_land_use_prohibition__commemorative_husk_reading, suppression_requirement, 60, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_land_use_prohibition__commemorative_husk_reading, identity_coordination).
narrative_ontology:affects_constraint(aneyoshi_land_use_prohibition__commemorative_husk_reading, aneyoshi_land_use_prohibition__behavioral_competence_reading).
narrative_ontology:affects_constraint(aneyoshi_land_use_prohibition__commemorative_husk_reading, tsunami_warning_architecture_institutional_decay).
narrative_ontology:affects_constraint(aneyoshi_land_use_prohibition__commemorative_husk_reading, memorial_to_operative_drift_disaster_governance).

% DUAL FORMULATION NOTE:
% The aneyoshi_land_use_prohibition kernel decomposes into two structurally distinct constraint readings. The behavioral_competence_reading models the stone as an operationally enforced directive (ε ≈ 0.08–0.15, rope/mountain). The commemorative_husk_reading models the stone as a memorial artifact whose directive force has decayed (ε = 0.68, snare). These are not the same constraint viewed from different angles — they rest on incompatible claims about whether the stone's directive governs behavior. The ε values differ by a factor of 4–8 because the underlying claim about the stone's operative status is structurally different. Both readings are linked via network.affects_constraints to mark their dependence on a common kernel and enable cross-reading consistency checks.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(aneyoshi_land_use_prohibition__commemorative_husk_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
