% ============================================================================
% CONSTRAINT STORY: personhood_boundary__viability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_personhood_boundary__viability_reading, []).

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
 *   constraint_id: personhood_boundary__viability_reading
 *   human_readable: Personhood Boundary: Viability Reading (Moral Status at Independent Survival Capacity)
 *   domain: normative_ethics/bioethics/political_philosophy
 *
 * SUMMARY:
 *   The viability reading positions moral personhood at the point of fetal
 *   capacity for independent survival, approximately 24 weeks gestation. This
 *   reading instantiates one of three major competing positions on the
 *   personhood_boundary kernel — the foundational claim about which
 *   properties ground a being's moral status and thus protection from lethal
 *   harm. The viability reading attempts to establish a middle position
 *   between the conception_reading (personhood at fertilization) and the
 *   birth_reading (personhood at delivery), grounding moral status in a
 *   measurable biological capacity rather than either the earliest biological
 *   life or the moment of legal/social recognition. The reading creates a
 *   clear operational threshold for policy and law: pre-viability, the
 *   pregnant person's bodily autonomy takes priority; post-viability, the
 *   fetus's newly acquired moral status constrains reproductive options. This
 *   creates structural asymmetry — the constraint coordinates two irreducible
 *   moral claims (maternal autonomy and fetal status) while privileging the
 *   former in early pregnancy and the latter afterward. The constraint
 *   exhibits high suppression (0.62) because enforcing both the permission
 *   (pre-viability abortion access) and the prohibition (post-viability
 *   protection) requires continuous legal and medical authority. Theater
 *   ratio remains moderate (0.48) because the viability threshold, while
 *   socially constructed, operationalizes a genuine distinction in fetal
 *   development — unlike purely performative thresholds, the viability
 *   boundary tracks something materially real (fetal neural and respiratory
 *   maturation).
 *
 * KEY AGENTS:
 *   - Pregnant person (pre-viability): Primary beneficiary of bodily autonomy protection under this reading (but constrained by suppression mechanisms)
 *   - Pregnant person (post-viability): Primary victim subject to new post-viability constraints once fetal moral status is triggered
 *   - Viable fetus (post-viability): Secondary beneficiary granted moral status at viability threshold; experiences constraint as protective
 *   - Reproductive justice movement: Organized actor (institutional/mobile) supporting viability reading as coordination mechanism that protects early-term access
 *   - Fetal rights movement: Organized actor (institutional/mobile) contesting viability reading as underprotecting post-conception fetal status
 *   - Medical/legal establishment: Institutional actor (institutional/constrained) enforcing the dual mandate (pre-viability access + post-viability protection)
 *   - Analytical observer: Civilizational perspective (analytical/analytical) viewing the viability reading as a stable but contingent equilibrium between competing moral claims
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(personhood_boundary__viability_reading, 0.58).
domain_priors:suppression_score(personhood_boundary__viability_reading, 0.62).
domain_priors:theater_ratio(personhood_boundary__viability_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(personhood_boundary__viability_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(personhood_boundary__viability_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(personhood_boundary__viability_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(personhood_boundary__viability_reading, tangled_rope).
narrative_ontology:human_readable(personhood_boundary__viability_reading, "Personhood Boundary: Viability Reading (Moral Status at Independent Survival Capacity)").
narrative_ontology:topic_domain(personhood_boundary__viability_reading, "normative_ethics/bioethics/political_philosophy").

domain_priors:requires_active_enforcement(personhood_boundary__viability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(personhood_boundary__viability_reading, '5619f597-7826-4cd9-994d-77938224798b').
narrative_ontology:cs_kernel_codification('5619f597-7826-4cd9-994d-77938224798b', distributed).
narrative_ontology:cs_authority_grounding('5619f597-7826-4cd9-994d-77938224798b', distributed).
narrative_ontology:cs_reading_relation('5619f597-7826-4cd9-994d-77938224798b', personhood_boundary__conception_reading, forecloses).
narrative_ontology:cs_reading_relation('5619f597-7826-4cd9-994d-77938224798b', personhood_boundary__birth_reading, coexists_with).
narrative_ontology:cs_axiom('5619f597-7826-4cd9-994d-77938224798b', foundational, capacity_for_independent_survival_grounds_moral_status).
narrative_ontology:cs_axiom_status(capacity_for_independent_survival_grounds_moral_status, holdable).
narrative_ontology:cs_axiom_grounding('5619f597-7826-4cd9-994d-77938224798b', capacity_for_independent_survival_grounds_moral_status, deontological).
narrative_ontology:cs_axiom('5619f597-7826-4cd9-994d-77938224798b', foundational, maternal_bodily_autonomy_prior_to_viability).
narrative_ontology:cs_axiom_status(maternal_bodily_autonomy_prior_to_viability, holdable).
narrative_ontology:cs_axiom_grounding('5619f597-7826-4cd9-994d-77938224798b', maternal_bodily_autonomy_prior_to_viability, deontological).
narrative_ontology:cs_reference_frame('5619f597-7826-4cd9-994d-77938224798b', viability_threshold_equal_personhood).
narrative_ontology:cs_drift_state('5619f597-7826-4cd9-994d-77938224798b', contemporary_neuroscience_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('5619f597-7826-4cd9-994d-77938224798b', '').
narrative_ontology:cs_kernel_id(personhood_boundary__viability_reading, personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(personhood_boundary__viability_reading, maternal_bodily_autonomy).
narrative_ontology:constraint_beneficiary(personhood_boundary__viability_reading, reproductive_self_determination).
narrative_ontology:constraint_victim(personhood_boundary__viability_reading, viable_fetus_moral_status).
narrative_ontology:constraint_victim(personhood_boundary__viability_reading, post_viability_abortion_seekers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PREGNANT PERSON PRE-VIABILITY (SNARE) — Before viability, this reading grants reproductive autonomy. However, the constraint's suppression mechanisms (legal restrictions, medical paternalism, institutional barriers to access) remain in place. The agent experiences extraction even where the reading grants permission — procedural, financial, and social barriers to abortion persist. No real exit from pregnancy itself during this window; decision-making agency is constrained.
constraint_indexing:constraint_classification(personhood_boundary__viability_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PREGNANT PERSON POST-VIABILITY (MOUNTAIN) — Once viability is reached, this reading places the fetus in the victim set as a bearer of moral status. The pregnant person faces an irreducible constraint: continued pregnancy, birth, or lethal harm to the fetus. No genuine exit from the fetus's newly granted moral claims. This perspective experiences the boundary as fixed law — the capacity for independent survival is the immutable threshold, and crossing it creates unescapable obligation.
constraint_indexing:constraint_classification(personhood_boundary__viability_reading, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: REPRODUCTIVE JUSTICE MOVEMENT (ROPE) — Organized actors (abortion rights coalitions, feminist organizations) see the viability reading as a coordination mechanism: it protects early-term abortion access while acknowledging post-viability obligations. This perspective experiences the reading as solving a genuine collective action problem — how to honor both maternal autonomy and fetal status without requiring one to eliminate the other. Mobile exit options (advocacy, litigation, norm-building) allow this agent to reshape the constraint's enforcement.
constraint_indexing:constraint_classification(personhood_boundary__viability_reading, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: MEDICAL/LEGAL ESTABLISHMENT (TANGLED ROPE) — Hospitals, abortion providers, and courts operate under this reading as a dual mandate: enable pre-viability abortion access while restricting post-viability procedures. This is genuine coordination (medicine and law must align on a threshold to operate coherently) embedded with asymmetric extraction. The medical establishment benefits from clarity (the viability threshold is operationalizable) while bearing enforcement costs (ultrasound verification, legal liability management). Career constraints (malpractice risk, licensing boards) limit exit.
constraint_indexing:constraint_classification(personhood_boundary__viability_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: VIABLE FETUS POST-VIABILITY (SNARE) — This reading grants the viable fetus moral status and thus potential victim standing. The constraint places this agent in maximum extraction position: capacity for independent survival is treated as grounds for state intervention and legal protection against abortion, yet this same agent has no voice in the constraint's definition, interpretation, or enforcement. No exit from the moral status once viability is crossed. The fetus experiences the constraint as immutable coercion, though only retrospectively (post-birth) with capacity for retrospective judgment.
constraint_indexing:constraint_classification(personhood_boundary__viability_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 6: FETAL RIGHTS MOVEMENT (ORGANIZED) (TANGLED ROPE) — Organized actors advocating for fetal personhood (pro-life coalitions, religious institutions) see this reading as a coordination mechanism WITH extraction bias toward the pregnant person. From their perspective, the viability threshold is an arbitrary cutoff that reduces the viable fetus to a partial victim — the reading coordinates legal permission (pre-viability) with moral prohibition (post-viability), but does so in a way that privileges maternal autonomy over fetal status. Mobile exit options (advocacy, political mobilization, alternative institutional authority) allow this agent to contest the constraint's terms, though constrained by majoritarian politics.
constraint_indexing:constraint_classification(personhood_boundary__viability_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / EQUILIBRIUM VIEW (ROPE) — From a civilizational analytical perspective, the viability reading represents a stable coordination mechanism between two irreducible moral claims: (a) pregnant persons have moral agency and bodily autonomy, and (b) developed fetuses have morally significant interests in continued life. The viability threshold operationalizes a genuine compromise that enables both claims to coexist without requiring either party to deny the other's moral standing. No party achieves maximum benefit, but coordination is achieved. This perspective sees low extractiveness because the threshold solves a coordination problem rather than advancing one agent's interests over another's.
constraint_indexing:constraint_classification(personhood_boundary__viability_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(personhood_boundary__viability_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(personhood_boundary__viability_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(personhood_boundary__viability_reading, TypeOther, context(agent_power(powerless), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(personhood_boundary__viability_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(personhood_boundary__viability_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. This reading creates asymmetric costs and benefits relative to different agents and temporal phases. Pre-viability, the reading liberates maternal autonomy (low extraction) but embeds suppression mechanisms that constrain actual access (moderate extraction through procedural barriers). Post-viability, the reading grants fetal moral status but imposes obligation on the pregnant person (high extraction experienced by pregnant person; fetal perspective experiences protection, not extraction). The average is elevated by the post-viability phase and by institutional suppression mechanics that persist even where the reading nominally grants autonomy. Suppression (0.62): High. Enforcing both the permission (pre-viability abortion access) and the prohibition (post-viability constraint) requires sustained legal and medical authority — ultrasound verification of gestational age, legal frameworks criminalizing post-viability abortion, medical licensing boards that police provider conduct. The suppression is active and continuous, not passive. Theater ratio (0.48): Moderate-low. The viability threshold, while socially constructed, operationalizes a genuine distinction in fetal neural and respiratory development that is materially real and medically meaningful. Unlike purely symbolic thresholds, the viability boundary tracks biological processes that can be measured and verified. The theater is lower because the threshold has genuine referent in fetal development — it is not pure performance. However, viability itself depends on medical technology (NICU capability) and thus embeds some contingency; the theater rises somewhat from the recognition that the threshold drifts with technology.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits dramatic perspectival divergence. The pregnant person pre-viability sees coordination (permission to abort) but experiences suppression (barriers to access) — Snare classification from their perspective. The pregnant person post-viability sees immutable obligation (Mountain classification) because the fetus's newly granted moral status creates an irreducible constraint. The reproductive justice movement sees a coordination mechanism protecting early-term access (Rope classification) — genuine collective action problem solved. The fetal rights movement sees the reading as biased extraction against the post-conception fetus (Tangled Rope, experienced as Snare from their perspective). The medical establishment sees a dual mandate requiring constant enforcement (Tangled Rope — genuine coordination with embedded extraction). The viable fetus (post-viability) is granted moral status but has no voice in the constraint's terms — Snare classification (powerless victim in a constraint that purports to protect it). The analytical observer sees a stable equilibrium solving competing moral claims (Rope classification) — coordination achieved at a cost to both parties, but both claims preserved. This 7-way perspectival divergence reveals that the constraint's true nature depends entirely on which agent's position you adopt.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values vary dramatically across agents based on their structural position relative to the extraction flow. Pre-viability pregnant persons start with low d (beneficiary of autonomy protection, d ≈ 0.25) but experience high suppression despite the nominal permission (suppression_requirement ≈ 0.60), making experienced extraction (chi) moderate-high despite low d. Post-viability pregnant persons have d ≈ 0.75 (bearing the cost of fetal moral status imposition) and experience high suppression (enforcement of post-viability constraints), yielding high chi. Viable fetuses have d ≈ 0.90 (nominally protected but structurally voiceless) and experience suppression in the form of state intervention and legal framework imposing their interests without their participation. The reproductive justice movement has d ≈ 0.35 (organized beneficiary of viability reading as protection for early-term access) and experiences low chi because their exit options (advocacy, litigation, norm-building) are mobile. The fetal rights movement has d ≈ 0.55 (organized actor contesting the reading's boundaries) and experiences moderate chi because their position is contested but not foreclosed. The medical establishment has d ≈ 0.50 (dual beneficiary and burden-bearer, coordinating two directives) and experiences moderate chi due to constrained exit options (licensing, liability, professional norms). The analytical observer has d ≈ 0.72 (observational position showing full structure to all parties) and a canonical f(d) value. The wide range of d values across perspectives reflects that this constraint genuinely differentiates agents by their structural relationship to the extraction flow — not all perspectives experience equal impact.
 *
 * MANDATROPHY ANALYSIS:
 *   The viability reading's mandatrophy is resolved through explicit recognition that the constraint coordinates two irreducible moral claims (maternal autonomy and fetal status) while creating asymmetric costs and benefits at different phases of pregnancy. The reading is neither pure coordination (Rope) nor pure extraction (Snare); it is a genuine hybrid (Tangled Rope). The mandatrophy would arise if the reading attempted to claim that it simply protected maternal autonomy (ignoring post-viability fetal status constraints) or that it simply protected fetal life (ignoring pre-viability maternal autonomy). By explicitly embedding both claims and the phase-shift between them, the reading avoids false classification. The constraint's mandatrophy is also resolved by recognizing that different agents experience the same constraint as different types: the pregnant person post-viability experiences an immutable constraint (Mountain), not a hybrid, because for that agent the constraint lacks a coordination function — only extraction. The viable fetus experiences pure extraction (Snare) because its moral status is granted without its voice. The reproductive justice movement experiences coordination (Rope) because the reading solves their collective action problem. The Tangled Rope classification at the analytical/institutional level is the appropriate aggregation: the constraint exhibits both genuine coordination (solving the binary choice between protecting maternal autonomy and fetal status) and asymmetric extraction (differentiating costs and benefits across agents and phases). The mandatrophy resolution is that mandatrophy is perspectival — there is no single 'correct' type, but rather a presheaf of types indexed over observation positions. The claim that the constraint is Tangled Rope is the claim that from the civilizational analytical position, both coordination and extraction functions are structurally present.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    viability_threshold_biological_contingency,
    'Is the viability threshold (24 weeks) grounded in morally relevant biological properties of the fetus, or is it a convenient proxy that conflates development timing with moral status?',
    'Philosophical analysis of which capacities (lung development, consciousness, pain sensation, neural integration) are morally decisive. Empirical determination of when these capacities actually emerge. Assessment of whether viability itself (capacity for survival with medical support) is morally relevant or merely administratively convenient.',
    'If viability is morally arbitrary: the reading''s core axiom (capacity_for_independent_survival_grounds_moral_status) is undermined, and the viability threshold moves toward the conception_reading or toward a threshold based on consciousness/pain sensation. If viability is morally decisive: the reading holds, and the boundary remains stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(viability_threshold_biological_contingency, conceptual, 'Whether viability is a morally relevant threshold or administratively convenient proxy').

omega_variable(
    maternal_bodily_autonomy_hierarchy,
    'Does the viability reading grant maternal bodily autonomy absolute priority pre-viability, or does it already embed constraints on abortion (waiting periods, parental consent, state counseling)?',
    'Legal and institutional audit of actual abortion policies in jurisdictions claiming to follow viability reading. Measurement of which pre-viability restrictions are routine. Assessment of whether pre-viability abortion access is actually available or merely nominally permitted.',
    'If absolute autonomy pre-viability: reading operates as stated, and extraction pre-viability is suppression of access rather than moral claim on the fetus. If constrained: the reading embeds hidden post-viability-like restrictions even pre-viability, and extractiveness should be revised upward.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(maternal_bodily_autonomy_hierarchy, empirical, 'Actual pre-viability abortion access vs. nominal permission').

omega_variable(
    medical_authority_versus_moral_authority,
    'Is viability (operationally: capacity for survival with neonatal intensive care) a medical question, a moral question, or both?',
    'Historical analysis of how viability threshold has shifted with NICU technology. Determination of whether moral status should track medical capability (if NICU capacity improves, does viability move earlier?). Identification of whether the reading conflates medical viability with moral personhood or treats them as distinct.',
    'If coupled (moral status follows medical capability): the boundary drifts with technology, and the reading lacks stability. If decoupled (moral status fixed at current viability technology): the reading is more stable but depends on medical contingency at the point of threshold definition. If treated as fundamentally different: the reading may need a distinct moral capacity threshold decoupled from medical measures.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(medical_authority_versus_moral_authority, empirical, 'Relationship between medical viability and moral personhood').

omega_variable(
    consciousness_and_moral_status_connection,
    'Is the capacity for independent survival (viability) the morally relevant property, or should moral status track consciousness, pain sensation, or other neurological markers instead?',
    'Neuroscientific evidence on when sentience, pain sensation, and rudimentary consciousness emerge in fetal development (evidence suggests 24+ weeks for primitive sensation; 28+ weeks for more integrated experience). Philosophical argument for why consciousness or sentience should be morally decisive if the reading''s current justification relies on viability alone.',
    'If consciousness is morally decisive and emerges later than viability: the reading''s threshold is too early, and the viable_fetus_moral_status should extend to earlier fetuses. If consciousness is not morally decisive: the reading''s focus on viability is justified, and consciousness is a separate consideration. If consciousness is morally decisive but the reading treats viability as proxy: the reading''s axiom is misaligned with its justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consciousness_and_moral_status_connection, conceptual, 'Relationship between consciousness and viability as moral thresholds').

omega_variable(
    sibling_reading_foreclosure_conditional,
    'Does the viability reading logically foreclose the conception_reading (which places personhood at fertilization), or can both readings coexist as positions held by different moral frameworks?',
    'Logical analysis: does accepting capacity_for_independent_survival_grounds_moral_status require denying personhood_begins_at_conception? Or do these depend on different foundational premises (e.g., different definitions of ''person,'' different criteria for moral status) that can coexist? Empirical determination of whether any single coherent moral framework can hold both readings simultaneously.',
    'If foreclosed: the coexists_with relation to conception_reading is incorrect, should be forecloses. If coexistence is possible: both readings remain live options despite the competing axioms. This affects the engine''s downstream analysis of kernel stability and the possibility of simultaneous legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_conditional, conceptual, 'Logical foreclosure relationship to conception_reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(personhood_boundary__viability_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pbv_tr_t0, personhood_boundary__viability_reading, theater_ratio, 0, 0.52).
narrative_ontology:measurement(pbv_tr_t12, personhood_boundary__viability_reading, theater_ratio, 12, 0.5).
narrative_ontology:measurement(pbv_tr_t24, personhood_boundary__viability_reading, theater_ratio, 24, 0.48).

% Extraction over time
narrative_ontology:measurement(pbv_be_t0, personhood_boundary__viability_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(pbv_be_t12, personhood_boundary__viability_reading, base_extractiveness, 12, 0.5).
narrative_ontology:measurement(pbv_be_t24, personhood_boundary__viability_reading, base_extractiveness, 24, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(personhood_boundary__viability_reading, identity_coordination).
narrative_ontology:affects_constraint(personhood_boundary__viability_reading, personhood_boundary__conception_reading).
narrative_ontology:affects_constraint(personhood_boundary__viability_reading, personhood_boundary__birth_reading).
narrative_ontology:affects_constraint(personhood_boundary__viability_reading, abortion_access_suppression).

% DUAL FORMULATION NOTE:
% The personhood_boundary kernel decomposes into three distinct constraint stories, each representing a reading of the same foundational claim about what properties ground moral personhood. Extractiveness values differ: conception_reading (ε ≈ 0.72, Snare), viability_reading (ε ≈ 0.58, Tangled Rope), birth_reading (ε ≈ 0.42, Tangled Rope). These are not the same constraint viewed from three angles — they have genuinely different ε values reflecting different empirical consequences and moral/political structures. The viability_reading is downstream of the kernel dispute and influences abortion_access_suppression through its determination of which abortion-seekers face post-viability legal barriers.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(personhood_boundary__viability_reading, powerless, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
