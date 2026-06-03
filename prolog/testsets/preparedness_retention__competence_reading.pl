% ============================================================================
% CONSTRAINT STORY: preparedness_retention__competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_retention__competence_reading, []).

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
 *   constraint_id: preparedness_retention__competence_reading
 *   human_readable: Preparedness as Live Exercised Knowledge (Competence Reading)
 *   domain: disaster_preparedness/institutional_memory/governance
 *
 * SUMMARY:
 *   Preparedness as live exercised knowledge is a reading of the contested
 *   kernel preparedness_retention that emphasizes functional competence over
 *   ceremonial performance. This reading holds that drills and inspections
 *   are not rituals that merely feel like retention but genuine exercises in
 *   which knowledge is actively maintained through practice. The constraint
 *   operates in disaster preparedness and institutional memory systems where
 *   competence across decades and generational turnover is required for rare,
 *   high-consequence events. Water management (Netherlands dike system),
 *   civil protection (earthquake response), and public health systems
 *   exemplify this reading. The structural signature is low
 *   theater-to-competence ratio: drills are functional rehearsals, not
 *   performances; resources optimize for skill retention and adaptive
 *   capacity; beneficiary is population safety and systemic resilience;
 *   victims are none (in the pure reading) or fiscal opportunity costs if
 *   over-invested. This reading competes with two sibling interpretations:
 *   the husk reading (preparedness is memorial performance lacking live
 *   competence) and the hybrid reading (technical competence retained in
 *   specialized institutions while broader societal memory becomes
 *   ceremonial). The three readings coexist in contemporary preparedness
 *   practice, each grounding legitimacy in different institutional traditions
 *   and empirical claims about what drills actually accomplish.
 *
 * KEY AGENTS:
 *   - Specialized Water Authority (Rijkswaterstaat model): Primary coordinator (institutional/arbitrage) — maintains technical competence through functional drills; benefits from low-theater rehearsal; embodies competence reading.
 *   - Water Boards / Regional Managers: Primary coordinator (organized/mobile) — organized practitioners who experience drills as genuine coordination solving personnel turnover and distributed knowledge maintenance.
 *   - Municipal Civil Protection Offices: Secondary coordinator (moderate/constrained) — statutory obligation for preparedness; experience drills as mixed coordination and constraint; constrained by resource and time costs.
 *   - Residents in High-Risk Areas: Partial victims (powerless/trapped) — benefit from competence-maintained infrastructure; experience extraction through mandatory participation, restricted land use, dependence on institutional capacity.
 *   - Population Safety (abstract beneficiary): Non-agent beneficiary — aggregate well-being enabled by maintained competence.
 *   - International Disaster Response Organizations: Temporary coordinator (organized/constrained) — experience preparedness as scaffold with explicit sunset; coordinate crisis response training and capacity building.
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees preparedness as fundamental coordination solving the problem of maintaining rare-event competence across generations.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_retention__competence_reading, 0.18).
domain_priors:suppression_score(preparedness_retention__competence_reading, 0.12).
domain_priors:theater_ratio(preparedness_retention__competence_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(preparedness_retention__competence_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_retention__competence_reading, rope).
narrative_ontology:human_readable(preparedness_retention__competence_reading, "Preparedness as Live Exercised Knowledge (Competence Reading)").
narrative_ontology:topic_domain(preparedness_retention__competence_reading, "disaster_preparedness/institutional_memory/governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_retention__competence_reading, 'e1219070-70da-4a37-8d60-ce7131a7cdd3').
narrative_ontology:cs_kernel_codification('e1219070-70da-4a37-8d60-ce7131a7cdd3', distributed).
narrative_ontology:cs_authority_grounding('e1219070-70da-4a37-8d60-ce7131a7cdd3', practice).
narrative_ontology:cs_interpretation_layer_present('e1219070-70da-4a37-8d60-ce7131a7cdd3').
narrative_ontology:cs_reading_relation('e1219070-70da-4a37-8d60-ce7131a7cdd3', preparedness_retention__husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('e1219070-70da-4a37-8d60-ce7131a7cdd3', preparedness_retention__hybrid_reading, influences).
narrative_ontology:cs_axiom('e1219070-70da-4a37-8d60-ce7131a7cdd3', foundational, drills_functionally_transfer_to_crises).
narrative_ontology:cs_axiom_status(drills_functionally_transfer_to_crises, holdable).
narrative_ontology:cs_axiom_grounding('e1219070-70da-4a37-8d60-ce7131a7cdd3', drills_functionally_transfer_to_crises, empirically_contingent).
narrative_ontology:cs_axiom('e1219070-70da-4a37-8d60-ce7131a7cdd3', foundational, competence_requires_active_maintenance).
narrative_ontology:cs_axiom_status(competence_requires_active_maintenance, holdable).
narrative_ontology:cs_axiom_grounding('e1219070-70da-4a37-8d60-ce7131a7cdd3', competence_requires_active_maintenance, empirically_contingent).
narrative_ontology:cs_reference_frame('e1219070-70da-4a37-8d60-ce7131a7cdd3', functional_preparedness).
narrative_ontology:cs_drift_state('e1219070-70da-4a37-8d60-ce7131a7cdd3', contemporary, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('e1219070-70da-4a37-8d60-ce7131a7cdd3', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(preparedness_retention__competence_reading, preparedness_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_retention__competence_reading, population_safety).
narrative_ontology:constraint_beneficiary(preparedness_retention__competence_reading, adaptive_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WATER BOARD PRACTITIONER (ROPE) — The organized technician (dike inspector, flood coordinator, water management engineer) experiences preparedness drills as genuine coordination mechanisms. Live exercised knowledge translates directly into operational capacity. Drills solve a collective action problem: maintaining competence across rotating personnel, distributed infrastructure, and slow-turnover threats. Low theater (drilling is functional rehearsal), low extraction (the practitioner benefits from the same system they maintain). Classification: Rope — pure coordination with minimal coercive overhead.
constraint_indexing:constraint_classification(preparedness_retention__competence_reading, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 2: WATER AUTHORITY / RIJKSWATERSTAAT (ROPE) — Institutional actors committed to dike maintenance and flood response experience preparedness as a coordination mechanism that directly serves their operational mandate. Drills test real infrastructure, train rotating personnel, and maintain institutional memory across decades. The authority benefits from low-theater preparedness (functional rehearsal) because it enables the actual function (flood control). No extraction — the constraint's existence is justified by genuine coordination need. Extraction value is negligible because the authority's interests align with competence preservation.
constraint_indexing:constraint_classification(preparedness_retention__competence_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: RURAL MUNICIPALITY (ROPE) — Municipal governments responsible for evacuation coordination and civil protection experience preparedness as coordination with mild constraints. Drills maintain local competence networks, test evacuation routes, and preserve institutional knowledge across electoral cycles. Extraction is low because the municipality's competence directly serves its statutory obligations. Some constraint (resource costs of drills, time commitment) but genuine coordination function — municipalities cannot exit without abandoning their core function.
constraint_indexing:constraint_classification(preparedness_retention__competence_reading, rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: RESIDENT IN HIGH-RISK AREA (TANGLED ROPE) — Individuals living in dike-protected zones experience preparedness as mixed coordination and extraction. Genuine coordination: drills and inspections maintain actual safety infrastructure that protects them. But also extraction: required participation in evacuation drills, restricted land use, constrained building codes, dependence on institutional capacity they cannot influence. Trapped by geography; constrained by competence requirements of living in flood-vulnerable area. Classification: Tangled Rope — real coordination function (dike safety) alongside asymmetric constraints on individual choice.
constraint_indexing:constraint_classification(preparedness_retention__competence_reading, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 5: INTERNATIONAL DISASTER RESPONSE NETWORK (SCAFFOLD) — Global organizations (UN OCHA, Red Cross, humanitarian NGOs) experience preparedness drills as temporary coordination scaffolds that build capacity for crisis response. International drills have explicit sunset logic: they are investments in rapid mobilization capacity for when actual disaster strikes. Once deployed in real crisis, the scaffold serves its function and dissolves back into specialized expertise. Low extraction (resources directed to actual preparedness), explicit temporal boundary, genuine coordination. Classification: Scaffold — temporary support with built-in sunset (when crisis occurs, scaffold is replaced by operational response).
constraint_indexing:constraint_classification(preparedness_retention__competence_reading, scaffold,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / CIVILIZATIONAL VIEW (ROPE) — From a long-term perspective, preparedness as live exercised knowledge is a pure coordination mechanism that solves the civilizational problem: maintaining competence to respond to rare, high-consequence threats across generations. No extraction — the constraint exists to preserve adaptive capacity in the face of entropy. Theater is low because the drills ARE the competence; there is no gap between ritual and function. The analytical observer sees this constraint as a rope — the foundational reading that all others are perturbations of.
constraint_indexing:constraint_classification(preparedness_retention__competence_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_retention__competence_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(preparedness_retention__competence_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(preparedness_retention__competence_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(preparedness_retention__competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. The competence reading minimizes extraction because the constraint's function (maintaining adaptive capacity) aligns with all major agents' interests. Water authorities benefit from functional drills. Practitioners benefit from preserved competence. Residents benefit from maintained safety infrastructure. The extractiveness value reflects mild friction costs (time spent on drills, resource allocation) rather than asymmetric capture. Measurement trajectory shows slight upward drift (0.12 → 0.18 over 20 years) reflecting gradual bureaucratization and administrative overhead accumulation, but remaining well below rope ceiling (0.45). Suppression (0.12): Very low. The competence reading presumes consensus on preparedness value. Suppression of alternatives (husk reading, skepticism about drills) exists but is mild — jurisdictions can and do question drill utility without facing severe barriers. The measurement trajectory is stable (0.10 → 0.12) indicating that suppression is not the constraint's mechanism. Theater ratio (0.35): Low-moderate. This reading characterizes drills as functional exercises rather than performances. Theater reflects the minimum ceremonial content required for legitimacy (formal reporting, documentation, participation credentials), not the core function. Measurement trajectory shows modest drift (0.28 → 0.35) reflecting institutional tendency toward increased documentation and formality as organizational size increases, but theater remains well below piton threshold (0.70). The low theater ratio is diagnostic: if theater rose above 0.50, the constraint would risk reclassification toward the husk reading.
 *
 * PERSPECTIVAL GAP:
 *   The six perspectives reveal how the same structural data (drills, inspections, resource allocation) are classified differently depending on the observer's power position and time horizon. Organized practitioners (water boards, specialists) classify the constraint as pure Rope — functional coordination with no extraction. Institutions (Rijkswaterstaat) also see Rope — their mandate aligns with competence preservation. Residents (powerless/trapped) see Tangled Rope — genuine safety coordination mixed with personal constraint. International organizations see Scaffold — temporary coordination with sunset logic. The municipal perspective (moderate/constrained) bridges Rope and Tangled Rope, experiencing both coordination and constraint. The analytical/civilizational observer sees Rope at the foundational level: preparedness is solving the civilizational problem of maintaining competence across generations. No perspective classifies as Snare or Mountain. The gap between organized/institutional and powerless perspectives (both Rope vs Tangled Rope) reveals the distributional structure: those organizing the constraint experience it as pure coordination; those participating in it (without organizing authority) experience mixed coordination and constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) in this constraint is determined by the agent's relationship to competence preservation — whether they benefit from maintained adaptive capacity and whether they bear costs of drill participation. Rijkswaterstaat (institutional/arbitrage) has d ≈ 0.05: strong beneficiary of competence maintenance, can arbitrage by integrating drills into operations. Water board practitioners (organized/mobile) have d ≈ 0.35: benefit from preserved competence, but mobile — could theoretically exit to non-preparedness roles (though most don't). Residents (powerless/trapped) have d ≈ 0.70: depend on maintained competence for safety but cannot exit high-risk geography; trapped position means they cannot arbitrage. All d values reflect the structure that the competence reading presumes: no agent is extracting from another; all agents benefit from maintained adaptive capacity. The gradations reflect differential power to influence the constraint's operation, not asymmetric extraction. This is why the constraint classifies as Rope from most perspectives — d values do not distribute in the high-extraction (d > 0.70) range that would produce Snare or Tangled Rope.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves potential mandatrophy by maintaining tight alignment between claimed function (maintaining competence) and observed mechanism (drills as functional rehearsal). The analytical observer might be tempted to see preparedness as a natural law (Mountain — rare catastrophes require maintained competence, inherently) or as performance without function (Piton — elaborate drill machinery with low real-world transfer). The competence reading rejects both. It is not natural law (competence is contingent on active maintenance, not inevitable). It is not piton (drills are functional, not performative). It is pure coordination (Rope) — a constraint that solves a collective action problem (maintaining competence despite generational turnover and infrastructure distribution) without extracting from anyone. The mandatrophy is avoided by insisting on empirical transfer fidelity: if drills do not translate to real-world competence, the reading itself fails, and the constraint reclassifies toward husk reading. The reading is self-correcting through the omega variables.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_measurement_ambiguity,
    'How do we empirically distinguish live exercised knowledge (functional competence) from ceremonial performance that mimics competence?',
    'Crisis event analysis: compare organizational behavior in actual floods or disasters to pre-crisis drill performance. Track whether drill procedures transfer to real-world execution without degradation.',
    'If drills demonstrate transfer fidelity (>80% procedure adoption in actual crisis): competence reading confirmed. If transfer fidelity is low (<50%): competence claim fails and constraint may reclassify toward husk reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(competence_measurement_ambiguity, empirical, 'Measurement of whether drills translate to actual competence').

omega_variable(
    kernel_interpretation_ambiguity,
    'Is this constraint one reading of a contested kernel (preparedness_retention) or a fundamental property of competence systems?',
    'Institutional analysis: compare competence-reading drills (Netherlands water boards, Singapore civil defense) with husk-reading drills (ceremonial disaster response in low-experience jurisdictions, memorial hurricane preparedness in areas without recent hurricanes). If both appear legitimate to different communities, the kernel contest is real.',
    'If kernel contest is real: this is one reading among coexisting alternatives, and the competence-reading axioms are holdable but not foreclosing. If one reading is empirically superior: that reading may foreclose or substantially influence the others.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_interpretation_ambiguity, conceptual, 'Whether preparedness_retention is a contested kernel').

omega_variable(
    hybrid_reading_coexistence,
    'Can the competence reading and hybrid reading (technical competence in specialized institutions, ceremonial in broader society) coexist as valid readings of the same kernel?',
    'Institutional stratification analysis: study multi-level preparedness systems (e.g., Netherlands: Rijkswaterstaat technical competence + municipal ceremonial performance + public awareness campaigns). Assess whether this is a single constraint read differently or two distinct constraints at different institutional levels.',
    'If coexistent: both competence and hybrid readings are live and the kernel contest involves influences relationships, not foreclosures. If one is foundational: the other is downstream perturbation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_reading_coexistence, conceptual, 'Whether hybrid and competence readings coexist').

omega_variable(
    atrophy_trajectory_risk,
    'In the absence of live exercises, does competence degrade toward husk-reading status even if the formal drill machinery persists?',
    'Historical analysis: trace competence trajectories in jurisdictions that reduced drill frequency (e.g., post-Cold War civil defense reductions in NATO countries). Track whether competence degradation precedes the formal sunset of preparedness programs.',
    'If atrophy trajectory is confirmed: the competence reading is vulnerable to drift toward husk reading through decay rather than reinterpretation. Maintenance cost implications: what frequency and fidelity of drills prevents atrophy?',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(atrophy_trajectory_risk, empirical, 'Risk of competence degradation without active drill maintenance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_retention__competence_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_comp_theater_t0, preparedness_retention__competence_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(prep_comp_theater_t10, preparedness_retention__competence_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(prep_comp_theater_t20, preparedness_retention__competence_reading, theater_ratio, 20, 0.35).

% Extraction over time
narrative_ontology:measurement(prep_comp_extract_t0, preparedness_retention__competence_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(prep_comp_extract_t10, preparedness_retention__competence_reading, base_extractiveness, 10, 0.15).
narrative_ontology:measurement(prep_comp_extract_t20, preparedness_retention__competence_reading, base_extractiveness, 20, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(prep_comp_suppress_t0, preparedness_retention__competence_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(prep_comp_suppress_t10, preparedness_retention__competence_reading, suppression_requirement, 10, 0.11).
narrative_ontology:measurement(prep_comp_suppress_t20, preparedness_retention__competence_reading, suppression_requirement, 20, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_retention__competence_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(preparedness_retention__competence_reading, preparedness_retention__husk_reading).
narrative_ontology:affects_constraint(preparedness_retention__competence_reading, preparedness_retention__hybrid_reading).

% DUAL FORMULATION NOTE:
% preparedness_retention is decomposed into three readings differing by authority grounding and axiom sets. The competence_reading emphasizes practice and expertise; the husk_reading emphasizes ritual critique; the hybrid_reading emphasizes institutional stratification. Each reading is a constraint story with its own ε and perspectives. They are linked via network.affects_constraints and documented in their respective kernel_context fields. The kernel contest is real — different agents genuinely adopt different readings, and no single agent framework contains all three.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(preparedness_retention__competence_reading, powerless, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
