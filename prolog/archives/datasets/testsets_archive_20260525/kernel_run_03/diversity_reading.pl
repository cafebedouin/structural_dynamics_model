% ============================================================================
% CONSTRAINT STORY: diversity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_diversity_reading, []).

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
 *   constraint_id: diversity_reading
 *   human_readable: Diversity Rationale: Race as Limited Factor in Holistic Review
 *   domain: constitutional_law/civil_rights/educational_policy
 *
 * SUMMARY:
 *   The diversity reading of equal protection permits universities to
 *   consider race as one factor among many in holistic admissions review when
 *   diversity serves a compelling educational interest. This is ONE READING
 *   of the equal protection clause kernel — specifically, a reading that
 *   prioritizes institutional educational autonomy and the learning benefits
 *   of racial diversity against competing readings that prioritize
 *   colorblindness or remedial redress. The diversity reading was established
 *   in Regents v. Bakke (1978) and developed through Gratz v. Bollinger and
 *   Grutter v. Bollinger (2003), but has been substantially narrowed and
 *   constrained by Students for Fair Admissions v. Harvard (2023), which
 *   rejected the diversity rationale as unconstitutional. This constraint
 *   story documents the diversity reading as it was permissible prior to SFFA
 *   and examines its structural properties as a coordination-extraction
 *   hybrid. The reading instantiates a narrow window of institutional
 *   discretion: universities can consider race, but only if the consideration
 *   is individualized, limited to specific educational benefits, and not
 *   driven by numerical targets or racial balancing. This window has been
 *   closing — extractiveness has risen over the interval as judicial scrutiny
 *   has tightened and institutional discretion has narrowed.
 *
 * KEY AGENTS:
 *   - Universities (Institutional Autonomy): Primary beneficiary (institutional/arbitrage) — gain discretion to shape student body for educational mission; can exit by adopting colorblind policies
 *   - Underrepresented Minority Applicants: Secondary beneficiary (moderate/constrained) — benefit from diverse peer learning environment; also subject to race-conscious evaluation
 *   - Applicants Outside Diversity Frame: Primary victim (powerless/trapped) — may experience reduced admission probability without direct benefit; no escape from race-conscious admissions system
 *   - Educational Equity Coalition: Organized intermediary (organized/mobile) — advocates for diversity; sees alternative pathways (class-based, outreach) as potential substitutes
 *   - Federal Judiciary: Institutional authority (institutional/arbitrage) — enforces through strict scrutiny framework; uses legitimacy narratives (compelling interest, narrow tailoring) that increasingly diverge from actual practice
 *   - Analytical Observer: Vantage point (analytical/analytical) — risks naturalizing educational diversity as an immutable pedagogical law rather than a contestable institutional arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(diversity_reading, 0.28).
domain_priors:suppression_score(diversity_reading, 0.35).
domain_priors:theater_ratio(diversity_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(diversity_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(diversity_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(diversity_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(diversity_reading, tangled_rope).
narrative_ontology:human_readable(diversity_reading, "Diversity Rationale: Race as Limited Factor in Holistic Review").
narrative_ontology:topic_domain(diversity_reading, "constitutional_law/civil_rights/educational_policy").

domain_priors:requires_active_enforcement(diversity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(diversity_reading, fixed_text).
narrative_ontology:cs_authority_grounding(diversity_reading, lineage).
narrative_ontology:cs_interpretation_layer_present(diversity_reading).
narrative_ontology:cs_kernel_id(diversity_reading, equal_protection_clause).
narrative_ontology:cs_reading_relation(diversity_reading, colorblind_reading, forecloses).
narrative_ontology:cs_reading_relation(diversity_reading, remedial_reading, coexists_with).
narrative_ontology:cs_axiom(diversity_reading, foundational, race_consciousness_narrowly_permissible).
narrative_ontology:cs_axiom_status(race_consciousness_narrowly_permissible, holdable).
narrative_ontology:cs_axiom_grounding(diversity_reading, race_consciousness_narrowly_permissible, deontological).
narrative_ontology:cs_axiom(diversity_reading, secondary, institutional_educational_autonomy_paramount).
narrative_ontology:cs_axiom_status(institutional_educational_autonomy_paramount, overridden).
narrative_ontology:cs_axiom_grounding(diversity_reading, institutional_educational_autonomy_paramount, conventional).
narrative_ontology:cs_reference_frame(diversity_reading, narrow_tailoring_framework).
narrative_ontology:cs_drift_state(diversity_reading, post_sffa_era, gap(authority_erosion, substantial, true)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(diversity_reading, universities_institutional_autonomy).
narrative_ontology:constraint_beneficiary(diversity_reading, diverse_student_body).
narrative_ontology:constraint_victim(diversity_reading, applicants_outside_considered_diversity_frame).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNIVERSITY AUTONOMY (ROPE) — Institutional beneficiary with arbitrage exit (can adopt colorblind policies if diversity discretion is revoked). Experiences the diversity rationale as genuine coordination: considering race holistically enables mixed-identity cohorts that serve educational mission. Net benefit without coercion. The constraint enables rather than restricts institutional choice.
constraint_indexing:constraint_classification(diversity_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 2: UNDERREPRESENTED MINORITY APPLICANT (TANGLED ROPE) — Constrained exit (college application is high-stakes, not optional). Genuine coordination benefit (diverse peer learning environment benefits all students; applicant benefits from being part of diverse class). But also extraction component: race becomes admissions criterion, applicant cannot separate from racial identity in evaluation, and discretionary holistic review introduces opacity. Mixed experience — genuine educational benefit (coordination) alongside asymmetric treatment (extraction).
constraint_indexing:constraint_classification(diversity_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: APPLICANT OUTSIDE DIVERSITY FRAME (SNARE) — Trapped (college admissions is high-stakes with no genuine alternative for selective institutions). Zero agency in the race-consideration calculus. Experiences the constraint as pure extraction: race becomes a factor that reduces their admission probability without any compensating benefit. Cannot exit the system (college is economically essential); bears asymmetric cost. Maximum experienced extraction from this perspective.
constraint_indexing:constraint_classification(diversity_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 4: EDUCATIONAL EQUITY COALITION (SCAFFOLD) — Organized agents (civil rights groups, student organizations) see diversity as temporary institutional discretion with an exit path: alternative pathways to campus diversity (class-based affirmative action, outreach programs, elimination of legacy preferences) are being tested and may eventually supplant race-conscious consideration. The coalition has agency; sees diversity rationale as a transitional mechanism. Theater ratio relatively low — the coordination function (building diverse cohorts) is genuine, not performative.
constraint_indexing:constraint_classification(diversity_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: JUDICIAL LEGITIMACY (PITON) — The diversity rationale is structurally maintained through performative legitimacy work: strict scrutiny rhetoric that claims narrowly tailored interest, while the actual practice is broad institutional discretion. The doctrine persists through inertia — courts invoke precedent (Regents v. Bakke, Gratz v. Bollinger, Grutter v. Bollinger) while their application has been increasingly theatrical. The rationale requires continuous narrative maintenance (compelling interest, narrowly tailored, individual holistic review) despite structural drift toward pure institutional autonomy. Theater ratio high relative to functional constraint.
constraint_indexing:constraint_classification(diversity_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational vantage, educational excellence inherently requires exposure to diverse perspectives; this is treated as an irreducible feature of learning rather than a policy choice. The diversity rationale becomes naturalized as a law of pedagogy. However, the structural data contradicts this classification — the constraint has beneficiaries (universities), victims (applicants outside the diversity frame), and requires active enforcement (admissions processes). The engine will identify this as a false summit: the natural-law framing obscures that the diversity imperative is a contestable institutional arrangement, not an immutable law.
constraint_indexing:constraint_classification(diversity_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(diversity_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(diversity_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(diversity_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(diversity_reading, TR),
    TR >= 0.70.

:- end_tests(diversity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low-moderate. The constraint permits universities to extract modest advantage from applicants by using race as an admissions factor, but the extraction is limited by narrow tailoring doctrine and by the genuine coordination benefit (diverse learning environment serves all students). The value reflects that universities gain significant institutional autonomy while the individual cost to applicants outside the diversity frame is moderate rather than severe. Measurement trajectory (0.15→0.28→0.40) shows increasing extractiveness over the interval as strict scrutiny has tightened and universities have faced increasing pressure to defend and justify diversity consideration, raising the enforcement burden. Suppression (0.35): Moderate. Applicants cannot exit the college admissions system (economically essential), but the suppression is not total — applicants can apply to non-selective institutions, transfer, or challenge diversity policies through litigation. The constraint requires significant institutional enforcement (admissions offices must conduct holistic reviews). Theater ratio (0.48→0.65): Increasing performativity as universities develop more elaborate justificatory narratives (individual consideration, compelling interest, narrow tailoring) to defend diversity against judicial scrutiny. The theatrical increase reflects that the functional diversity mechanism (considering race) remains constant while the legitimacy work required to defend it has expanded. Claimed type is Tangled Rope: genuine coordination function (diverse cohorts benefit all students) exists alongside asymmetric extraction (some applicants bear uncompensated costs through race-conscious evaluation).
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits a dramatic perspectival gap between beneficiaries and victims. Universities see genuine coordination (Rope) — diversity rationale enables their educational mission with no coercion, just institutional discretion. Underrepresented minorities see tangled rope — mixed benefit (access, peer environment) and cost (identity becomes evaluation criterion). Applicants outside the diversity frame see pure extraction (Snare) — they bear race-conscious disadvantage with no compensating benefit. The organized equity coalition sees a temporary mechanism with sunset paths (Scaffold) — alternative pathways to diversity may eventually replace race-conscious consideration. The judicial system sees its own legitimacy maintenance work as performative (Piton) — strict scrutiny rhetoric persists while actual discretion is broad. The analytical observer risks a false summit (Mountain) — naturalizing diversity as a law of pedagogy rather than acknowledging it as a contestable institutional arrangement grounded in the beneficiary group's interests.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) reflects the agent's structural relationship to the race-consideration extraction flow. Universities (beneficiary + arbitrage exit) derive d≈0.08 (low); they experience the constraint as enabling rather than extracting. Underrepresented minorities (mixed beneficiary-victim + constrained exit) derive d≈0.45 (near-symmetric); they gain access and peer benefits but also bear identity-as-criterion cost. Applicants outside diversity frame (victim + trapped exit) derive d≈0.88 (high); they experience maximum extraction without escape. The organized coalition (organized + mobile exit) derives d≈0.52 (near-balanced); they see alternatives available. The judicial system (institutional authority + arbitrage) derives d≈0.15 (low); maintaining the doctrine benefits the court's institutional interests. The analytical observer (analytical context) derives canonical d≈0.73; the natural-law reading is a perspectival position, not a structural fact.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves mandatrophy by demonstrating that the diversity rationale is structurally a Tangled Rope — it contains both genuine coordination (diverse learning environment) and asymmetric extraction (race-conscious evaluation). The classification is robust across different extraction measurements: if measured by institutional discretion gained, the constraint is Rope. If measured by applicant experience, the constraint is Snare for those outside the diversity frame. The Tangled Rope classification splits the difference — it acknowledges that both the coordination and extraction components are real, neither subsumes the other, and the constraint's legitimacy depends on whether the coordination benefit is genuine (empirical question per omega_1) or whether it masks extraction (empirical question per omega_2). The mandatrophy is resolved by recognizing that the reading itself is contestable — the SFFA decision rejected the diversity rationale as insufficiently narrowly tailored, shifting the baseline from Tangled Rope (limited discretion permitted) to Piton (the doctrine persists through inertia but lacks legitimacy).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    diversity_coordination_vs_extraction,
    'Does the diversity mechanism coordinate genuine learning outcomes (all students benefit from mixed cohorts) or does it primarily extract benefit from applicants by instrumentalizing their identity?',
    'Longitudinal outcome studies comparing educational attainment, civic participation, and earnings for students in diverse vs. non-diverse cohorts; evidence on whether diversity benefit is symmetrical (all groups gain) or asymmetrical (some groups bear costs)',
    'If coordination: constraint is Rope from all perspectives, extraction is coordination overhead. If extraction: constraint is Snare for applicants outside diversity frame, victims list expands, epsilon rises to 0.50+. This is the core ambiguity in the diversity reading itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diversity_coordination_vs_extraction, empirical, 'Whether diversity mechanism primarily coordinates learning or extracts from applicants').

omega_variable(
    holistic_review_opacity,
    'Does holistic review''s discretionary structure enable genuine individualized consideration or does it function as cover for implicit preferences and racial stereotyping?',
    'Audit studies comparing admission outcomes across race with equivalent test scores and credentials; analysis of admissions decision documents where available; comparison of stated criteria vs. actual weighting across applicant groups',
    'If genuine individuation: suppression is lower (applicants have path to demonstrate value despite race). If cover for implicit bias: suppression is higher (structural opacity prevents appeals or redress). Epsilon may rise if discretion masks systematic disadvantage.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(holistic_review_opacity, empirical, 'Whether holistic review enables genuine individuation or masks implicit preferences').

omega_variable(
    reading_foreclosure_post_sffa,
    'Does the Supreme Court''s SFFA decision (2023) logically foreclose the diversity reading within originalist constitutional interpretation, or does the reading persist through doctrinal contestation?',
    'Constitutional law analysis of whether SFFA rejects the diversity rationale as constitutionally impermissible or merely narrows its application window. Observation of post-SFFA diversity programs and institutional responses (class-based proxies, narrative-based frames, alternative pathways).',
    'If foreclosed: this constraint becomes historical artifact (universities cannot legally adopt diversity rationale). If persisting: universities find alternative framings (educational benefit, institutional mission, peer diversity effects on learning) that instantiate similar mechanisms under different labels. Affects whether this reading remains ''holdable'' or becomes ''overridden'' in axiom status.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_post_sffa, conceptual, 'Whether SFFA logically forecloses diversity reading or permits doctrinal evasion').

omega_variable(
    colorblind_vs_diversity_irreconcilability,
    'Are the colorblind reading and diversity reading logically incompatible within a single equal protection framework, or can they coexist as different policy choices?',
    'Constitutional doctrine analysis: does the colorblind principle (race must be irrelevant) logically entail that diversity-conscious consideration is unconstitutional, or can both be valid under different justificatory theories? Historical observation of whether courts have recognized coexistence.',
    'If incompatible: diversity and colorblind readings foreclose each other (rare strong foreclosure relation). If coexistent: they represent competing doctrinal positions held by different institutional actors (normal coexistence). This determines the reading_relations entry in cs_structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(colorblind_vs_diversity_irreconcilability, conceptual, 'Logical compatibility of colorblind and diversity readings in equal protection doctrine').

omega_variable(
    remedial_vs_prospective_diversity_distinction,
    'Does the diversity reading (prospective: building diverse learning environment) logically differ from remedial reading (retrospective: redressing documented group discrimination), or do they collapse into a single rationale under scrutiny?',
    'Doctrinal analysis of how courts distinguish prospective diversity benefits from remedial redress claims. Empirical observation of whether universities frame their diversity rationale as remedial or as prospective. Analysis of whether the justificatory structure requires both elements.',
    'If distinct: three independent readings (colorblind, remedial, diversity). If overlapping: remedial and diversity readings may influence each other''s legitimacy conditions. Affects how network edges (affects_constraints) are structured.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remedial_vs_prospective_diversity_distinction, conceptual, 'Logical distinction between remedial and prospective diversity rationales').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(diversity_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dive_tr_t0, diversity_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(dive_tr_t15, diversity_reading, theater_ratio, 15, 0.48).
narrative_ontology:measurement(dive_tr_t30, diversity_reading, theater_ratio, 30, 0.65).

% Extraction over time
narrative_ontology:measurement(dive_be_t0, diversity_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(dive_be_t15, diversity_reading, base_extractiveness, 15, 0.28).
narrative_ontology:measurement(dive_be_t30, diversity_reading, base_extractiveness, 30, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(diversity_reading, resource_allocation).
narrative_ontology:affects_constraint(diversity_reading, colorblind_reading).
narrative_ontology:affects_constraint(diversity_reading, remedial_reading).
narrative_ontology:affects_constraint(diversity_reading, selective_admissions_legitimacy).

% DUAL FORMULATION NOTE:
% The diversity reading is one of three distinct constraint stories decomposing the equal protection clause's application to race-conscious admissions. Colorblind reading (epsilon≈0.05, Mountain/Rope) forbids all race consideration. Remedial reading (epsilon≈0.35, Tangled Rope) permits race-conscious action when redressing documented group discrimination. Diversity reading (epsilon≈0.28, Tangled Rope) permits race-conscious action when serving prospective educational benefits. Each reading has its own epsilon, its own beneficiary/victim structure, and its own institutional legitimacy claims. They are linked via network edges because changes in one reading's legitimacy affect the others' doctrinal standing.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
