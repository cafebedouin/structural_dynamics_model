% ============================================================================
% CONSTRAINT STORY: unconditional_university_offers_uk
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unconditional_university_offers_uk, []).

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
 *   constraint_id: unconditional_university_offers_uk
 *   human_readable: Unconditional Offers in UK University Admissions
 *   domain: economic/social
 *
 * SUMMARY:
 *   Following the 2015 removal of student number caps, UK universities
 *   entered a competitive recruitment market. Research-intensive institutions
 *   began issuing unconditional offers to secure enrollment pipelines early,
 *   targeting students from state schools, lower-income backgrounds, and
 *   geographic regions without strong university presence. The constraint
 *   exhibits high perspectival variation: universities experience it as
 *   coordination (solving legitimate enrollment matching), lower-income
 *   students experience it as extraction (early lock-in with information
 *   disadvantage), student advocacy organizations see it as a temporary
 *   market distortion with regulatory solutions, and from civilizational
 *   distance it appears as an immutable feature of competitive higher
 *   education. The measurement trajectory shows acceleration of both
 *   extractiveness and theater ratio over the post-cap period (2015-2024),
 *   with extractiveness nearly tripling from 0.18 to 0.58 as unconditional
 *   offers shifted from occasional practice to dominant enrollment strategy.
 *   Suppression (information asymmetry, geographic isolation, financial
 *   desperation) increased from 0.42 to 0.62 as competitive pressure
 *   intensified. Theater ratio (the performative conditional offer system
 *   persisting without enforcement) rose from 0.35 to 0.68, indicating the
 *   gap between formal admissions criteria and actual enrollment mechanics
 *   widened substantially.
 *
 * KEY AGENTS:
 *   - Research-Intensive University Management: Primary beneficiary (institutional/arbitrage) — secures enrollment early, targets specific student demographics, locks in tuition revenue
 *   - Lower-Income State School Students: Primary victim (powerless/trapped) — accepts unconditional offer due to information deficit and financial pressure; loses negotiating leverage; bears extraction cost of early lock-in
 *   - State School Guidance Counselors: Secondary victim (powerful/mobile) — manages genuine coordination function but operates under institutional incentives that favor enrollment over fit
 *   - Well-Resourced Background Students: Secondary beneficiary (moderate/constrained) — receives genuine coordination benefit (stress reduction) but also bears extraction cost of early commitment
 *   - Student Advocacy Organizations: Organized agents (organized/constrained) — sees regulatory solutions and sunset pathway; building alternative enrollment mechanisms
 *   - Office of Fair Admissions & Regulatory Bodies: Institutional actors (institutional/arbitrage) — gatekeeping admissions fairness but historically passive on unconditional offer proliferation
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing market-contingent institutional choices as inevitable features of competition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unconditional_university_offers_uk, 0.58).
domain_priors:suppression_score(unconditional_university_offers_uk, 0.62).
domain_priors:theater_ratio(unconditional_university_offers_uk, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unconditional_university_offers_uk, extractiveness, 0.58).
narrative_ontology:constraint_metric(unconditional_university_offers_uk, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(unconditional_university_offers_uk, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unconditional_university_offers_uk, tangled_rope).
narrative_ontology:human_readable(unconditional_university_offers_uk, "Unconditional Offers in UK University Admissions").
narrative_ontology:topic_domain(unconditional_university_offers_uk, "economic/social").

domain_priors:requires_active_enforcement(unconditional_university_offers_uk).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unconditional_university_offers_uk, research_intensive_institutions).
narrative_ontology:constraint_beneficiary(unconditional_university_offers_uk, university_management).
narrative_ontology:constraint_victim(unconditional_university_offers_uk, lower_income_students).
narrative_ontology:constraint_victim(unconditional_university_offers_uk, state_school_students).
narrative_ontology:constraint_victim(unconditional_university_offers_uk, admissions_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOWER-INCOME STATE SCHOOL STUDENT (SNARE) — Faces maximum extraction with minimal exit options. The unconditional offer appears as liberation (no achievement pressure) but functions as enrollment lock: student accepts early, loses leverage to negotiate, may enroll at institution misaligned with actual interests. Cannot defer, cannot shop around post-offer, cannot renegotiate. Suppression operates through limited information (guidance counselors unfamiliar with higher education market), geographic isolation from elite university alumni networks, and economic desperation (needs scholarship/accommodation certainty). Zero degrees of freedom once offer accepted.
constraint_indexing:constraint_classification(unconditional_university_offers_uk, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: WELL-RESOURCED STUDENT (TANGLED ROPE) — Has genuine coordination benefit (unconditional offer removes exam stress, enables certainty for family planning) alongside extraction. Can exit via conditional offer from competing institution, parental guidance, or explicit rejection of unconditional offer. But faces reputational cost of rejecting offer and social pressure from peer institutions. Moderate extraction with significant agency — extracted from but not trapped.
constraint_indexing:constraint_classification(unconditional_university_offers_uk, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: RESEARCH-INTENSIVE UNIVERSITY MANAGEMENT (ROPE) — Primary beneficiary. Secures enrollment pipeline early, reduces recruitment uncertainty, targets specific student demographics, locks in tuition revenue. Experiences unconditional offer as pure coordination: solving the legitimate problem of enrollment matching. Can exit via conditional offer strategy but benefits from unconditional approach in competitive market. Zero extraction experienced from their vantage — the mechanism exists to serve their interests.
constraint_indexing:constraint_classification(unconditional_university_offers_uk, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: STUDENT ADVOCACY ORGANIZATIONS (SCAFFOLD) — See unconditional offers as a temporary market distortion with a sunset clause: regulatory pressure (Office of Fair Admissions inquiry, university transparency mandates, accreditation bodies) is building alternative enrollment pathways (contextual admissions, foundation years, access programs) that bypass the unconditional offer mechanism. Low effective extraction because organized agents have policy leverage and see an exit timeline. Sunset estimated at 5-10 years as regulatory framework matures.
constraint_indexing:constraint_classification(unconditional_university_offers_uk, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: STATE SCHOOL LEADERSHIP (TANGLED ROPE) — Manages genuine coordination problem (matching students to suitable institutions) alongside extraction. Receives funding and partnership relationships from universities, increasing institutional prestige. But also bears pressure to advise students accurately, faces reputational damage from poor placement outcomes, and experiences information asymmetry about unconditional offer terms. Can exit by refusing to recommend unconditional offers but faces resource and partnership consequences. Mixed experience: genuine function (guidance) with embedded asymmetry (institutional incentives favor enrollment over optimal fit).
constraint_indexing:constraint_classification(unconditional_university_offers_uk, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: TRADITIONAL CONDITIONAL OFFER MODEL (PITON) — The prior system (conditional on exam results) persists in institutional memory and formal procedures, but its functional verification role has atrophied. Universities no longer use conditional offers as serious gates on enrollment — they rarely rescind for moderate grade shortfalls. The conditional offer is now largely performative: it maintains the fiction that entry requirements matter while institutions secure enrollment early via unconditional mechanisms. Theater ratio high because the conditional system still appears in regulations and communications but lacks enforcement.
constraint_indexing:constraint_classification(unconditional_university_offers_uk, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational distance, the unconditional offer appears as an immutable property of competitive higher education markets: when caps are removed and institutions compete for students, the pressure to lock in enrollment early is inherent and inescapable. This framing naturalizes what is actually a contingent institutional and regulatory choice. The engine will identify this as a false summit, revealing that 'market forces' is a convenient naturalization of policies (cap removal, financial dependence on enrollment numbers, lack of price regulation) that are themselves changeable.
constraint_indexing:constraint_classification(unconditional_university_offers_uk, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unconditional_university_offers_uk_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(unconditional_university_offers_uk, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(unconditional_university_offers_uk, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(unconditional_university_offers_uk, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(unconditional_university_offers_uk, TR),
    TR >= 0.70.

:- end_tests(unconditional_university_offers_uk_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Universities capture substantial value from early enrollment lock-in — reduced recruitment uncertainty, stronger negotiating position for student accommodation and course assignment, ability to plan cohorts and resource allocation. But extractiveness is not maximum because: (a) unconditional offers genuinely reduce application-stage stress for some students, (b) universities still must deliver educational value to maintain reputation, (c) students retain some exit options (declining offer, transferring post-enrollment, appealing if offer rescinded). The measurement trajectory from 0.18 to 0.58 reflects market transition: early adoption period (2015-2018) saw selective use; intensification period (2018-2024) saw widespread adoption and competitive escalation. Suppression (0.62): Moderate-high. Primary suppression mechanisms are (a) information asymmetry: lower-income students lack guidance on institution quality differences, peer effects, long-term career impact; (b) geographic isolation: students from areas without university presence lack alumni networks and local knowledge; (c) financial desperation: offering certainty about admission and accommodation appeals to students facing enrollment uncertainty; (d) limited exit options: once accepted, declining attracts reputational cost and forgoes certainty. But suppression is not total because: information is available online, advocacy organizations provide guidance, and students can technically decline offers. Theater ratio (0.68): High. The traditional conditional offer system persists in formal admissions procedures and university marketing (conditional on exam results) but lacks functional enforcement. Universities rarely rescind conditional offers for moderate grade shortfalls, effectively treating them as ceremonial. Unconditional offers bypass this theater entirely but replace it with different performative content (marketing language about 'flexibility' and 'access' that obscures early lock-in mechanics). Theater ratio increased from 0.35 to 0.68 as universities shifted from using conditional offers as actual gates (low theater) to using them as symbolic compliance artifacts (high theater) while employing unconditional offers for actual enrollment capture.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival divergence is extreme. Universities see rope (0.40 chi at institutional/arbitrage): a coordination mechanism for legitimate matching. Lower-income students see snare (1.22 chi at powerless/trapped): extraction with no exit. Well-resourced students see tangled_rope (0.50 chi at moderate/constrained): mixed benefits and costs. Student advocates see scaffold (0.18-0.30 chi at organized/constrained): a temporary market distortion with regulatory solutions visible. State school guidance counselors see tangled_rope from a different angle (0.75 chi at powerful/mobile): they have power to resist but face institutional pressure. The traditional conditional offer system sees piton (degraded ritual, maintained by inertia). The civilizational observer risks seeing mountain (immutable market competition) until the engine flags this as false-summit naturalization. The gap between university experience and student victim experience is the largest: a 2.4-fold difference in chi (1.22 vs 0.50). This gap reflects that universities have arbitrage options (can switch strategies) while trapped students do not.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from the agent's structural position within the constraint. Universities (institutional/arbitrage) are beneficiaries with low exit costs — they can switch to conditional offers if competitive pressure eases, making d ≈ 0.05-0.15 and f(d) ≈ -0.10 to 0.02. Lower-income students (powerless/trapped) are victims with no exit — they cannot exit the offer once accepted without substantial cost (loss of certainty, reputational damage, wasted application effort), making d ≈ 0.95 and f(d) ≈ 1.42. Well-resourced students (moderate/constrained) are mixed: they benefit from stress reduction but can exit via conditional offers from peer institutions, making d ≈ 0.50-0.60 and f(d) ≈ 0.65-0.85. State school counselors are secondary victims: they have nominal authority (can recommend declining offers) but face institutional pressure to maximize enrollment, making d ≈ 0.60-0.70 and f(d) ≈ 0.85-1.05. Student advocacy organizations are organized agents: they can organize collective action (accreditation lobbying, media campaigns), giving them effective exit and making d ≈ 0.40-0.50 and f(d) ≈ 0.40-0.65. The engine derives these values automatically from beneficiary/victim declarations and exit options; chi = ε × f(d) × σ(S) with S = national (σ = 1.0) produces different experienced extractiveness for each perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that coordination and extraction functions coexist but are distributed across perspectives. From university perspective, the constraint is genuine coordination: it solves the problem of matching students to suitable institutions and reduces recruitment uncertainty. From lower-income student perspective, it is genuine extraction: early lock-in removes negotiating leverage and exploits information disadvantage. The tangled_rope classification at the analytical level captures this hybrid: unconditional offers have real coordination function (reduce stress, provide certainty) and real extraction function (capture enrollment value, exploit desperation). The question 'is this coordination or extraction?' has no single answer — it is both, experienced differently depending on structural position. The mandatrophy resolves by recognizing that the constraint TYPE is determined by analytical position, but all six types (rope, snare, scaffold, tangled_rope, piton, mountain perspectives) describe real structural features that different agents genuinely perceive. The remedy is not to determine 'the' type, but to measure the perspectival gap and design policy to shift the distribution of experienced extractiveness (e.g., regulatory requirements for conditional offer transparency, mandatory waiting periods, information equity mandates).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    information_asymmetry_quantification,
    'What proportion of lower-income students making unconditional offer decisions have equivalent information access to well-resourced peers about institution quality, peer effects, and long-term career impact?',
    'Survey of student knowledge: comparative understanding of institution rankings, employment outcomes by institution, scholarship terms, residential costs. Correlation analysis between parental education/income and decision-relevant knowledge.',
    'If information asymmetry is severe (>70% knowledge gap): suppression gate rises, classification shifts toward snare. If asymmetry is moderate: tangled_rope classification holds. If asymmetry is minimal: classification shifts to rope (pure coordination).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(information_asymmetry_quantification, empirical, 'Degree of information asymmetry between low-income and well-resourced students in unconditional offer decisions').

omega_variable(
    student_outcome_divergence,
    'Do students enrolling via unconditional offers (lower-income, state school) experience systematically different educational and career outcomes than conditional-offer cohorts?',
    'Longitudinal tracking: degree completion rates, academic performance, employment outcomes, salary differential by offer type and student background. Decompose by institution quality and subject area.',
    'If outcomes are equivalent: unconditional offer functions as pure coordination (Rope). If outcomes are worse: extractive mechanism is confirmed (Snare/Tangled Rope). If outcomes are better: coordination function is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(student_outcome_divergence, empirical, 'Whether unconditional offer recipients experience systematically different outcomes').

omega_variable(
    alternative_enrollment_mechanisms,
    'Do contextual admissions, foundation years, and access program pathways provide genuine alternatives to unconditional offers, or do they serve different student demographics (not substitutes)?',
    'Comparative analysis: enrollment volumes and student demographics via contextual vs unconditional pathways. Institutional adoption rate of alternatives. Cost-benefit analysis for universities (revenue, reputation, operational complexity).',
    'If alternatives are genuine substitutes: scaffold sunset is real, and regulatory pressure will shift universities away from unconditional offers. If alternatives serve distinct populations: unconditional offers face no substitution pressure, and extraction mechanism persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_enrollment_mechanisms, empirical, 'Whether alternative enrollment mechanisms provide genuine substitutes for unconditional offers').

omega_variable(
    institutional_vs_student_benefit_attribution,
    'When universities claim unconditional offers benefit students (reduce exam stress, increase access), how much of that benefit is genuine coordination vs. rhetorical cover for enrollment capture?',
    'Decompose benefits into measurable components: stress reduction (survey student wellbeing pre/post-offer), access improvement (enrollment rates from previously underrepresented backgrounds), information quality (offer clarity and terms transparency). Compare university messaging against actual terms and outcomes.',
    'If benefits are substantial and genuine: tangled_rope classification confirmed with significant coordination function. If benefits are minimal: classification shifts to snare (extraction with minimal coordination cover).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_vs_student_benefit_attribution, conceptual, 'Genuine vs. rhetorical extent of student benefits from unconditional offers').

omega_variable(
    regulatory_capture_mechanism,
    'To what extent do university interests in enrollment certainty shape regulatory frameworks (OfA guidance, QAA standards, accreditation criteria) that govern admissions?',
    'Historical analysis of regulatory decisions: timing relative to unconditional offer growth, stakeholder input documentation, comparative analysis with international regulatory approaches (e.g., German numerus clausus, US regulatory constraints on binding admissions).',
    'If capture is substantial: the false summit (mountain view) is accurate — regulatory environment makes unconditional offers inevitable given current cap removal. If capture is minimal: regulatory alternatives exist, and classification shifts toward snare (institutions choose extraction mechanism despite regulatory space for alternatives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_mechanism, empirical, 'Degree of institutional capture in admissions regulation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unconditional_university_offers_uk, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncond_tr_t0, unconditional_university_offers_uk, theater_ratio, 0, 0.35).
narrative_ontology:measurement(uncond_tr_t3, unconditional_university_offers_uk, theater_ratio, 3, 0.52).
narrative_ontology:measurement(uncond_tr_t6, unconditional_university_offers_uk, theater_ratio, 6, 0.65).
narrative_ontology:measurement(uncond_tr_t9, unconditional_university_offers_uk, theater_ratio, 9, 0.68).

% Extraction over time
narrative_ontology:measurement(uncond_be_t0, unconditional_university_offers_uk, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(uncond_be_t3, unconditional_university_offers_uk, base_extractiveness, 3, 0.38).
narrative_ontology:measurement(uncond_be_t6, unconditional_university_offers_uk, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(uncond_be_t9, unconditional_university_offers_uk, base_extractiveness, 9, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(uncond_su_t0, unconditional_university_offers_uk, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(uncond_su_t5, unconditional_university_offers_uk, suppression_requirement, 5, 0.58).
narrative_ontology:measurement(uncond_su_t9, unconditional_university_offers_uk, suppression_requirement, 9, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unconditional_university_offers_uk, resource_allocation).
narrative_ontology:affects_constraint(unconditional_university_offers_uk, uk_university_funding_dependency).
narrative_ontology:affects_constraint(unconditional_university_offers_uk, state_school_widening_participation_programs).

% DUAL FORMULATION NOTE:
% Unconditional offers are downstream of the 2015 cap removal and universities' subsequent financial dependence on enrollment volume. The cap removal is itself a distinct constraint (regulatory choice, not market necessity). The offer mechanism has its own ε reflecting the extraction asymmetry; the cap removal has different ε reflecting the availability of alternative regulatory regimes. These are linked: cap removal created pressure for unconditional offers, but the offer mechanism is a contingent institutional response, not an inevitable consequence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unconditional_university_offers_uk, powerful, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
