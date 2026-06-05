% ============================================================================
% CONSTRAINT STORY: equal_protection_commitment__diversity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_commitment__diversity_reading, []).

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
 *   constraint_id: equal_protection_commitment__diversity_reading
 *   human_readable: Equal Protection Commitment: Diversity Reading (Race as One Factor Among Many)
 *   domain: constitutional_law/political_philosophy/social_policy
 *
 * SUMMARY:
 *   The diversity reading of the equal protection commitment interprets the
 *   14th Amendment's guarantee of equal protection to permit universities to
 *   consider race as one factor among many in admissions when such
 *   consideration serves the compelling state interest of achieving
 *   educational diversity. This reading emerged from Regents v. Bakke (1978)
 *   and was reaffirmed in Grutter v. Bollinger (2003), creating a procedural
 *   framework that nominally maintains colorblind equal protection doctrine
 *   while permitting categorical race consciousness justified by
 *   institutional mission. The constraint exhibits the full range of
 *   perspectival disagreement on what equal protection permits. To
 *   beneficiary institutions (universities pursuing diversity), the reading
 *   is coordination: a legal permission (diversity as compelling interest)
 *   aligned with institutional mission. To colorblind principle advocates,
 *   the reading is extraction: universities gain discretion to consider race
 *   while obscuring the substantive race-consciousness through procedural
 *   language. To individual applicants, the reading is snare: no justiciable
 *   standard clarifies how race figures in their individual rejection,
 *   trapping them outside transparent decision-making. The diversity reading
 *   is ONE reading of a contested kernel (equal_protection_commitment);
 *   sibling readings include the colorblind reading (race should be
 *   constitutionally irrelevant) and remedial reading (race consciousness is
 *   justified to remedy identified discrimination, not to pursue diversity as
 *   such). This kernel context section documents the core axioms and reading
 *   relations that distinguish this reading from its siblings.
 *
 * KEY AGENTS:
 *   - Universities with diversity mission (institutional/arbitrage): Primary beneficiary — gain discretion to pursue race-aware admissions while maintaining equal protection legitimacy; highest exit options (can appeal to peer institutions, can relocate to supportive jurisdictions, can reframe admissions criteria)
 *   - Underrepresented racial groups in higher education (moderate/constrained): Secondary beneficiary — benefit from universities' use of race as factor in admissions; constrained by dependence on institutional discretion rather than explicit constitutional guarantee
 *   - Individual applicants across all races (powerless/trapped): Primary victim — subject to opaque holistic review where race's weight is undefined; no justiciable standard permits individual claims; cannot exit from admissions system
 *   - Colorblind principle advocates (organized/constrained): Secondary victim and strategic actor — experience the constraint as corrupting equal protection doctrine; constrained by doctrinal standing and ceiling rules; benefit from the constraint's internal contradiction (permits legal challenge on grounds that equal protection should be colorblind)
 *   - Diversity advocacy organizations (organized/mobile): Supports beneficiary position; maintains the reading through litigation, legislative testimony, and institutional alliance; mobile exit (can shift to legislative strategy if doctrine shifts)
 *   - The Equal Protection Doctrine (institutional/analytical): Holds the reading through precedent and doctrinal interpretation; experiences the reading as both constraint (must justify diversity pursuit) and permission (can justify it); piton-like institutional inertia
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_commitment__diversity_reading, 0.28).
domain_priors:suppression_score(equal_protection_commitment__diversity_reading, 0.42).
domain_priors:theater_ratio(equal_protection_commitment__diversity_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(equal_protection_commitment__diversity_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_commitment__diversity_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_commitment__diversity_reading, "Equal Protection Commitment: Diversity Reading (Race as One Factor Among Many)").
narrative_ontology:topic_domain(equal_protection_commitment__diversity_reading, "constitutional_law/political_philosophy/social_policy").

domain_priors:requires_active_enforcement(equal_protection_commitment__diversity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_commitment__diversity_reading, 'a43a0fc5-5ac2-450e-9980-bfa255ce5bb4').
narrative_ontology:cs_kernel_codification('a43a0fc5-5ac2-450e-9980-bfa255ce5bb4', fixed_text).
narrative_ontology:cs_authority_grounding('a43a0fc5-5ac2-450e-9980-bfa255ce5bb4', lineage).
narrative_ontology:cs_interpretation_layer_present('a43a0fc5-5ac2-450e-9980-bfa255ce5bb4').
narrative_ontology:cs_reading_relation('a43a0fc5-5ac2-450e-9980-bfa255ce5bb4', equal_protection_commitment__colorblind_reading, coexists_with).
narrative_ontology:cs_reading_relation('a43a0fc5-5ac2-450e-9980-bfa255ce5bb4', equal_protection_commitment__remedial_reading, influences).
narrative_ontology:cs_axiom('a43a0fc5-5ac2-450e-9980-bfa255ce5bb4', foundational, compelling_state_interest_in_educational_diversity).
narrative_ontology:cs_axiom_status(compelling_state_interest_in_educational_diversity, holdable).
narrative_ontology:cs_axiom_grounding('a43a0fc5-5ac2-450e-9980-bfa255ce5bb4', compelling_state_interest_in_educational_diversity, instrumental).
narrative_ontology:cs_axiom('a43a0fc5-5ac2-450e-9980-bfa255ce5bb4', foundational, race_as_permissible_consideration_for_diversity).
narrative_ontology:cs_axiom_status(race_as_permissible_consideration_for_diversity, holdable).
narrative_ontology:cs_axiom_grounding('a43a0fc5-5ac2-450e-9980-bfa255ce5bb4', race_as_permissible_consideration_for_diversity, deontological).
narrative_ontology:cs_reference_frame('a43a0fc5-5ac2-450e-9980-bfa255ce5bb4', colorblind_equal_protection_mandate).
narrative_ontology:cs_drift_state('a43a0fc5-5ac2-450e-9980-bfa255ce5bb4', contemporary_post_grutter_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a43a0fc5-5ac2-450e-9980-bfa255ce5bb4', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(equal_protection_commitment__diversity_reading, equal_protection_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_commitment__diversity_reading, universities_with_diversity_mission).
narrative_ontology:constraint_beneficiary(equal_protection_commitment__diversity_reading, underrepresented_racial_groups_in_higher_education).
narrative_ontology:constraint_victim(equal_protection_commitment__diversity_reading, individual_applicants_all_races).
narrative_ontology:constraint_victim(equal_protection_commitment__diversity_reading, colorblind_principle_advocates).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL APPLICANT (SNARE) — Applicants lack transparency into how race figures in the admissions calculus. The 'one factor among many' formula provides no justiciable standard for individual claims. An applicant rejected while another with similar credentials is admitted cannot determine whether race was outcome-determinative. Exit is impossible (cannot choose alternative admission systems); suppression is high (institutional discretion is opaque); extraction is experienced as arbitrary.
constraint_indexing:constraint_classification(equal_protection_commitment__diversity_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: COLORBLIND ADVOCATES (TANGLED ROPE) — Organized opposition (libertarian, originalist, conservative institutions) benefits from the constraint's internal contradiction — the reading permits categorical awareness of race while requiring race-neutral justification (educational diversity as compelling state interest). This gap enables legal challenge. Exit is constrained by doctrine and standing requirements, but advocacy infrastructure and institutional alignment create coordination benefits. Experienced extraction is moderate: the constraint simultaneously constrains (doctrinal ceiling) and enables (provides attack vector).
constraint_indexing:constraint_classification(equal_protection_commitment__diversity_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: UNIVERSITIES (ROPE) — Primary beneficiary. Universities gain discretion to pursue diversity as a mission-aligned goal while maintaining the appearance of race-neutrality through procedural language ('one factor among many'). Institutional autonomy is enhanced; extraction toward universities is negligible. The constraint coordinates institutional mission (diversity commitment) with legal permission (equal protection clause reinterpreted to permit categorical awareness of race as long as race is not dispositive). Exit options are high (arbitrage into peer institutions with similar diversity missions or into openly race-conscious admissions in jurisdictions where legal risk is lower).
constraint_indexing:constraint_classification(equal_protection_commitment__diversity_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: DIVERSITY ADVOCATES (SCAFFOLD) — Organized movement (NAACP, civil rights law groups, progressive education associations) sees the constraint as a temporary procedural framework that prevents explicit race-consciousness while permitting its effects. The sunset logic is implicit: the 'one factor among many' formulation is theoretically justified only if underlying societal conditions (underrepresentation, discrimination residue) change such that race-aware admissions become unnecessary. This perspective acknowledges the constraint has a terminal condition — if the 'compelling state interest' (correcting educational underrepresentation) ceases to exist, the constraint should sunset. Mobile exit options (can pursue legislative change, can migrate to supportive jurisdictions, can frame admissions differently) keep the theater ratio moderate.
constraint_indexing:constraint_classification(equal_protection_commitment__diversity_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: EQUAL PROTECTION DOCTRINE (PITON) — Viewed from civilizational timescale, the diversity reading is a stretched interpretation of equal protection that has persisted through institutional inertia and the need to legitimize race-aware admissions within a formally colorblind constitutional frame. The doctrine has become largely performative: the 'strict scrutiny' gate nominally requires the state to show a compelling interest and narrow tailoring, but in practice, the diversity reading permits substantial discretion while maintaining the ritual of colorblind adjudication. Theater ratio is high (the doctrinal machinery is elaborate relative to the actual constraint it imposes); the functional verification — does the doctrine actually limit race-aware admissions? — is low. The piton persists because the alternative (explicit constitutional authorization for race-conscious remedies) would require institutional change, while the current reading preserves doctrinal continuity.
constraint_indexing:constraint_classification(equal_protection_commitment__diversity_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/analytical perspective, some tension between colorblind principle and race-aware remedy is inherent to anti-discrimination law: a framework dedicated to making race irrelevant must sometimes acknowledge race to correct for race-based harm. This perspective sees the diversity reading as instantiating an immutable logical property of equal protection jurisprudence. However, this classification is vulnerable to false-summit detection: the 'inherent tension' naturalizes what may be a contingent doctrinal choice (the colorblind frame itself) rather than a universal logical limit.
constraint_indexing:constraint_classification(equal_protection_commitment__diversity_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_commitment__diversity_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(equal_protection_commitment__diversity_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(equal_protection_commitment__diversity_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(equal_protection_commitment__diversity_reading, TR),
    TR >= 0.70.

:- end_tests(equal_protection_commitment__diversity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Moderate-low. The diversity reading is primarily a procedural permission rather than a substantive mandate. Universities are permitted to consider race, not required to do so. The extraction arises from the asymmetry between institutional discretion (beneficiaries) and applicant opacity (victims): applicants cannot know race's weight in their individual cases. However, the extraction is not severe because the constraint also benefits some applicants (those from underrepresented groups who benefit from race-conscious consideration). The moderate value reflects coordination (universities and underrepresented groups benefit from the permission) layered with moderate extraction (institutional discretion creates individual applicant opacity). Over the 30-year measurement interval (approximating the time from Bakke 1978 to post-Grutter doctrine maturation), extractiveness has risen slightly as admissions offices have become more confident in applying the 'one factor among many' discretion, creating more opacity rather than less. Suppression (0.42): Moderate. Institutional discretion and the opacity of holistic review create barriers to individual applicants' exit or challenge. However, suppression is not total: applicants have formal appeal procedures, legal aid is available for civil rights claims, and transparency advocacy has increased. The 'strict scrutiny' gate nominally constrains institutional discretion. Theater ratio (0.58): Moderate-high. The procedural complexity of the diversity reading — compelling state interest analysis, 'one factor among many' formulation, narrow tailoring requirements — is partially functional (it does provide a doctrine gate) but partially performative (institutions can meet the gate while maintaining substantial race-consciousness, and the gate has never actually rejected a diversity-justified admissions system). The theater ratio reflects that the procedural machinery is elaborate relative to its actual constraint on institutional behavior.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reveals the constraint's fundamental instability. From the university perspective, the diversity reading permits institutional mission alignment — it is coordination. From the applicant perspective, it permits institutional opacity — it is extraction. From the colorblind perspective, it permits categorical race consciousness under a false colorblind label — it is either extraction (corrupting equal protection) or snare (trapping colorblind advocates in a doctrine that nominally constrains but actually permits race consciousness). From the diversity advocacy perspective, it is a temporary procedural framework (scaffold) justified by the compelling state interest in remedying educational underrepresentation. The piton perspective views the elaborate doctrinal machinery as largely performative — the constraint persists through institutional inertia (need to maintain continuity with Bakke and Grutter precedent) rather than through functional verification that the 'strict scrutiny' gate actually constrains institutional behavior. The mountain perspective risks naturalizing this instability as an inherent logical feature of equal protection law.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from each agent's structural position: their power level, time horizon, exit options, and relationship to the extraction flow. Universities as beneficiaries with arbitrage options experience low d (around 0.15-0.20) — the constraint subsidizes their institutional discretion. Individual applicants as victims with no exit experience high d (around 0.90-0.95) — the constraint extracts opacity from them with no alternative. Colorblind advocates as organized agents with constrained exits experience moderate-high d (around 0.60-0.70) — they are targets of the reading's permissiveness while constrained by doctrinal rules. The engine's sigmoid function applies these d values to compute effective extractiveness χ per the formula χ = ε × f(d) × σ(S). For the institutional beneficiary perspective, low d yields negative f(d), making χ negative (the constraint provides benefit). For the powerless applicant perspective, high d yields high f(d), making χ substantial. The spatial scope is national (σ=1.0) because the constraint's force is bounded by US federal jurisdiction and state law variation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    diversity_vs_remediation_distinction,
    'Is the diversity reading structurally distinct from remedial race consciousness, or is it a rebranding of remediation under a softer justification?',
    'Comparative analysis of admissions policies justified as diversity-seeking vs those justified as remedial race correction. Track whether institutions claiming diversity justification apply race-consciousness differently than those claiming remedial justification. Historical doctrinal analysis of whether Bakke (diversity as compelling interest) and Gratz (remediation rejected) actually constrain different institutional behaviors.',
    'If structurally distinct: diversity reading is its own constraint with its own ε and beneficiary/victim structure, justifying separate constraint file. If rebranding: diversity reading is a performative relabeling of remedial race consciousness, suggesting higher theater ratio and different victim set.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(diversity_vs_remediation_distinction, conceptual, 'Whether diversity and remedial race consciousness are structurally distinct or rebranding').

omega_variable(
    one_factor_operationalization,
    'What does ''one factor among many'' operationally permit? Can race be outcome-determinative within a holistic review? How much weight can race carry before it becomes the ''primary'' factor rather than ''one of many''?',
    'Doctrinal analysis of post-Gratz case law and admissions guidance. Empirical study of admissions offices: what percentage of admitted applicants have race weighed as determinative vs merely considered? Can two applicants with identical credentials, differing only in race, be admitted/rejected respectively without violating ''one factor among many''?',
    'If ''one factor among many'' permits outcome-determinativeness: the constraint is nearly pure race consciousness, ε rises to 0.42+, tangled_rope classification is unstable. If strictly limits race weight: ε drops toward 0.15, constraint shifts toward Rope (coordination) rather than Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(one_factor_operationalization, empirical, 'Operational meaning of ''one factor among many'' in admissions practice').

omega_variable(
    colorblind_foreclosure_by_diversity_logic,
    'Does the diversity reading logically foreclose the colorblind reading, or can both coexist as live positions held by different parties?',
    'Doctrinal analysis: does accepting the diversity reading (race can be considered categorically to achieve educational diversity) require rejecting the colorblind premise (race should be constitutionally irrelevant)? Or can one party hold diversity-justification while another party holds colorblind-absolutism without logical contradiction within any single framework?',
    'If foreclosed: the sibling relationship is ''forecloses'' (rare, used only when one reading''s core premise directly contradicts the other''s). If coexistence is possible: the sibling relationship is ''coexists_with'' (different parties can maintain both readings as live positions). This determines cs_structure.reading_relations entries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(colorblind_foreclosure_by_diversity_logic, conceptual, 'Whether diversity reading forecloses colorblind reading or permits coexistence').

omega_variable(
    theater_ratio_transparency_gap,
    'How much of the constraint''s procedural complexity (strict scrutiny analysis, ''one factor among many'' formulation, compelling state interest documentation) is genuine limitation vs performative legitimation for institutional discretion?',
    'Comparative institutional analysis: do admissions offices bound by diversity reading apply less race consciousness than offices in jurisdictions with explicit race-conscious authority? Does the procedural complexity reduce racial consideration or merely obscure it? Post-admission surveys of rejected applicants: do those rejected in jurisdictions with diversity reading vs those in jurisdictions with explicit race-consciousness authority experience comparable opacity?',
    'If mostly performative (theater_ratio rises to 0.70+): the constraint shifts toward Piton classification. If substantial limitation (theater_ratio drops to 0.35): constraint shifts toward Rope (genuine coordination between equal protection principle and diversity mission).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_ratio_transparency_gap, empirical, 'How much of the constraint''s procedural complexity genuinely limits vs performatively legitimates institutional discretion').

omega_variable(
    kernel_reading_interpretation,
    'This constraint instantiates one reading of the equal_protection_commitment kernel. The diversity reading interprets equal protection to permit categorical race consciousness when justified by educational diversity as a compelling state interest. Does this reading remain a live position within constitutional law, or has subsequent doctrine (post-2023) foreclosed it?',
    'Doctrinal tracking: monitor Supreme Court decisions on affirmative action and equal protection. Does Students for Fair Admissions (2023) or successor rulings explicitly foreclose the diversity reading''s core axioms (compelling_state_interest_in_educational_diversity, race_as_permissible_consideration_for_diversity)? Or do subsequent decisions create space for alternative framings (e.g., recruitment of first-generation or socioeconomically disadvantaged applicants with statistical racial skew)?',
    'If foreclosed: the diversity reading transitions from ''holdable'' to ''overridden'' in cs_structure.axioms. The constraint''s claimed_type may shift from tangled_rope toward piton (degraded doctrine). If alternate framings emerge: the constraint family expands to include new readings (socioeconomic_proxy_reading, first_generation_reading).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_interpretation, conceptual, 'Whether the diversity reading remains live or has been foreclosed by subsequent doctrine').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_commitment__diversity_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epc_div_tr_t0, equal_protection_commitment__diversity_reading, theater_ratio, 0, 0.5).
narrative_ontology:measurement(epc_div_tr_t15, equal_protection_commitment__diversity_reading, theater_ratio, 15, 0.56).
narrative_ontology:measurement(epc_div_tr_t30, equal_protection_commitment__diversity_reading, theater_ratio, 30, 0.58).

% Extraction over time
narrative_ontology:measurement(epc_div_be_t0, equal_protection_commitment__diversity_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(epc_div_be_t15, equal_protection_commitment__diversity_reading, base_extractiveness, 15, 0.24).
narrative_ontology:measurement(epc_div_be_t30, equal_protection_commitment__diversity_reading, base_extractiveness, 30, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_commitment__diversity_reading, identity_coordination).
narrative_ontology:affects_constraint(equal_protection_commitment__diversity_reading, equal_protection_commitment__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_commitment__diversity_reading, equal_protection_commitment__remedial_reading).

% DUAL FORMULATION NOTE:
% The equal_protection_commitment kernel decomposes into three constraint stories corresponding to three live readings of what equal protection constitutionally permits. Each reading has distinct ε values, distinct beneficiary/victim structures, and distinct perspectival classifications. The diversity reading (this file) permits race-conscious admissions justified by educational diversity; ε≈0.28, Tangled Rope. The colorblind reading forbids categorical race consciousness entirely; ε≈0.15 (Mountain for some perspectives, Rope for others). The remedial reading permits race consciousness to remedy identified discrimination; ε≈0.35 (Tangled Rope). These are not observational variants of one constraint — they are readings of a contested kernel that different constitutional communities currently hold simultaneously. The ε-invariance principle applies: each reading's ε reflects the structural extraction implied by that reading's axioms and its current institutional realization. If measurement methodology (applicant surveys vs admissions office policy analysis vs litigation outcomes) changed how we computed ε for one reading, that would indicate two constraints within that reading, not one reading with measurement-dependent properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(equal_protection_commitment__diversity_reading, organized, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
