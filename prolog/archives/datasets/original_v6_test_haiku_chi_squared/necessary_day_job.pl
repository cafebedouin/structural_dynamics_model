% ============================================================================
% CONSTRAINT STORY: necessary_day_job
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_necessary_day_job, []).

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
 *   constraint_id: necessary_day_job
 *   human_readable: The Necessary Day Job for Creatives
 *   domain: economic/social
 *
 * SUMMARY:
 *   The necessary day job for creatives represents a structural decoupling of
 *   creative vocation from subsistence income. This constraint has shaped
 *   artistic production across industries—music, visual art, literature,
 *   design—for centuries, but has intensified as cultural institutions have
 *   shifted cost from consumers to artists. A musician might earn $15/hour
 *   gigging but work 40 hours/week at a service job to cover rent. A visual
 *   artist might receive $0-$500 per gallery show but hold a day job that
 *   pays the bills. A writer might spend 10 hours/week writing and 40
 *   hours/week at an administrative position. The constraint extracts
 *   artistic labor time that would otherwise be creative production, but it
 *   also coordinates artistic output with economic subsistence—without this
 *   coupling mechanism, many artists could not practice their vocation at
 *   all. This creates a mixed extraction-and-coordination dynamic (Tangled
 *   Rope) from most perspectives, but appears as pure extraction (Snare) to
 *   those without resources, patronage, or alternative pathways. The
 *   constraint's theater ratio has risen over the past 60 years as cultural
 *   gatekeeping has increasingly emphasized the 'struggle' narrative and the
 *   romantic myth of the starving artist, even as the actual economic
 *   conditions have worsened. The emergence of grants, residencies, artist
 *   collectives, and patronage platforms (Patreon, Kickstarter, arts
 *   councils) suggests a scaffold structure with a genuine sunset:
 *   alternative funding pathways are maturing, which could eventually reduce
 *   the extractiveness of the day job requirement.
 *
 * KEY AGENTS:
 *   - Creative Practitioners: Primary victims (powerless/trapped, moderate/constrained) — bear the cost of decoupled vocation and subsistence; time diverted from creative work to wage labor
 *   - Employers of Day Labor: Primary beneficiaries (institutional/arbitrage) — capture educated, skilled labor force (many creatives have trained artistic skills); benefit from low wages sustained by creative aspirations
 *   - Cultural Gatekeepers (galleries, publishers, venues): Secondary beneficiaries (institutional/arbitrage) — benefit from unpaid or underpaid artistic output; can access competitive talent because artists will work for exposure/prestige
 *   - Arts Infrastructure Coalition (grants, residencies, platforms): Organized agents (organized/constrained) — building alternative pathways (Scaffold); working to reduce day-job dependency through direct funding
 *   - Academic Institutions: Institutional actors (institutional/arbitrage) — provide some patronage through teaching positions and residencies; but increasingly rely on adjunct labor and underpaid artistic output
 *   - Analytical Observer: Sees both coordination function (enabling artistic practice) and extraction mechanism (suppressing artistic output); identifies the tangled rope classification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(necessary_day_job, 0.55).
domain_priors:suppression_score(necessary_day_job, 0.65).
domain_priors:theater_ratio(necessary_day_job, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(necessary_day_job, extractiveness, 0.55).
narrative_ontology:constraint_metric(necessary_day_job, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(necessary_day_job, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(necessary_day_job, tangled_rope).
narrative_ontology:human_readable(necessary_day_job, "The Necessary Day Job for Creatives").
narrative_ontology:topic_domain(necessary_day_job, "economic/social").

domain_priors:requires_active_enforcement(necessary_day_job).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(necessary_day_job, employers_of_day_labor).
narrative_ontology:constraint_beneficiary(necessary_day_job, cultural_gatekeepers).
narrative_ontology:constraint_victim(necessary_day_job, creative_practitioners).
narrative_ontology:constraint_victim(necessary_day_job, artistic_output_diversity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STRUGGLING ARTIST (SNARE) — Trapped by cost of living and lack of alternative income paths. Must trade creative time for subsistence labor. Cannot exit without destitution. Suppression is severe: no childcare support for artists, no basic income, no patron systems for emerging work. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.94. Pure extraction from the artist's structural position.
constraint_indexing:constraint_classification(necessary_day_job, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EMPLOYER (ROPE) — Benefits from reliable, educated labor force (many creatives take day jobs requiring skills they developed artistically). Experiences the constraint as coordination: day jobs provide income stability that enables unpaid internships, networking, and creative work during off-hours. Employer captures value from the educational subsidy (artists developed skills through artistic practice). d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.07. Net beneficiary through arbitrage.
constraint_indexing:constraint_classification(necessary_day_job, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: ESTABLISHED MID-CAREER CREATIVE (TANGLED ROPE) — Constrained by age, family obligations, and career lock-in (switching to full-time creative work risks losing healthcare, retirement contributions). But also benefits from the constraint structure: day job provides stability that enables artistic risk-taking, tax deductions for supplies, professional network that influences artistic work. d≈0.65, f(d)≈0.95, σ=1.0 → χ≈0.52. Mixed extraction and coordination.
constraint_indexing:constraint_classification(necessary_day_job, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ARTS INFRASTRUCTURE COALITION (SCAFFOLD) — Arts councils, grants programs, artist residencies, and emerging patronage platforms (Patreon, arts funding platforms) see the day job as a temporary coordination failure with a sunset. These agents are building alternative pathways: grants reduce time burden, residencies replace day labor, collective funding enables lower-hour work. Theater is moderate (0.48) because the coalition's infrastructure is increasingly real and functional. d≈0.35, f(d)≈0.30, σ=1.0 → χ≈0.17. Low extraction; coalition agents see alternatives maturing.
constraint_indexing:constraint_classification(necessary_day_job, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ACADEMIC INSTITUTION (PITON) — Universities and art schools position themselves as the primary patron of artists, but increasingly rely on adjunct labor and unpaid artistic output (student artists produce work that subsidizes institutional prestige). The institutional self-image (patron/educator) persists despite structural degradation (adjuncts earn subsistence wages, full-time faculty positions decline). Theater ratio = 0.48 is below piton gate (0.70), but the institutional perspective emphasizes how the academy's patronage function has atrophied while the rhetorical claim persists.
constraint_indexing:constraint_classification(necessary_day_job, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational view, the day job represents a genuine hybrid: economies need creative output (design, culture, innovation) but have historically underfunded direct artistic production. The constraint redistributes cost from consumers (who underpay for art) to artists (who subsidize creative work via day labor). This is structurally entrenched: it coordinates artistic production with economic reality, but extracts disproportionately from artists. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.76. The analytical perspective confirms the tangled rope classification rather than seeing a natural law.
constraint_indexing:constraint_classification(necessary_day_job, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(necessary_day_job_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(necessary_day_job, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(necessary_day_job, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(necessary_day_job, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(necessary_day_job, TR),
    TR >= 0.70.

:- end_tests(necessary_day_job_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): High-moderate. The constraint extracts creative time from artists—time they cannot allocate to artistic production. This is not total extraction (which would be 0.85+) because some artists do maintain significant creative output alongside day jobs, and some cultural sectors (academic art, publicly-funded work) provide more direct support. But the average extractiveness is substantial: a typical artist might spend 60-70% of working hours on subsistence labor, reducing artistic output accordingly. The 60-year trajectory (0.35 → 0.55) reflects erosion of institutional patronage and cultural funding relative to cost of living. Suppression (0.65): High. Barriers to exit include: no childcare support for artists, healthcare tied to employment, lack of basic income, cultural devaluation of artistic work (payment expectations remain low despite rising skill requirements), credential-based hiring that prevents mid-career transitions. The suppression has structural depth—it's not just individual barrier but systemic: no alternative pathway exists for most artists except wealthy backgrounds or rare breakthrough success. Theater ratio (0.48): Moderate. There is substantial performative content in how the day job is discussed culturally ('paying your dues,' 'the struggling artist myth') and in how cultural institutions celebrate artistic production while underfunding direct support. But the constraint is not purely theatrical—day jobs do actually provide income. The theater ratio has risen as the gap between romantic narratives and economic reality has widened.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates stark perspectival divergence. The struggling artist sees Snare (pure extraction, no exit). The employer sees Rope (coordination mechanism that provides skilled labor). The mid-career creative sees Tangled Rope (both enabling and constraining). The arts infrastructure coalition sees Scaffold (temporary problem with a sunset). The academic institution sees Piton (degraded patronage role maintained through rhetoric). The analytical observer sees Tangled Rope (genuine hybrid of coordination and extraction at civilizational scale). The perspectival gap is driven by power asymmetry (who benefits from the coupling) and exit capacity (who can exit and who cannot). The gap widens when comparing powerless to institutional perspectives—the artist cannot exit, but the employer or gatekeeper can arbitrage their position.
 *
 * DIRECTIONALITY LOGIC:
 *   Creative practitioners: Victim + trapped (powerless) → d≈0.95, f(d)≈1.42. Maximum extraction from structural position. Victim + constrained (moderate) → d≈0.65, f(d)≈0.95. High extraction but not maximum; mid-career creatives have some agency (teaching, residencies, established platforms). Employers: Beneficiary + arbitrage (institutional) → d≈0.05, f(d)≈-0.12. Net beneficiary; the constraint provides them reliable labor. Cultural gatekeepers: Beneficiary + arbitrage (institutional) → d≈0.05, f(d)≈-0.12. Net beneficiary; artists work for exposure/prestige. Arts infrastructure: Organized + constrained → d≈0.40, f(d)≈0.40. Low extraction; these agents have agency and see alternatives. Analytical observer: analytical → d≈0.72, f(d)≈1.15. Observes full structure including extraction from most vulnerable.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY IDENTIFICATION: The constraint resolves mandatrophy by acknowledging that the day job serves BOTH coordination and extraction functions simultaneously, depending on perspective. From a powerless artist's view, it is pure extraction (Snare). From an employer's view, it is coordination (Rope). From the analytical view, it is a hybrid (Tangled Rope). The tangled rope classification at the analytical level reveals the structural truth: the day job does enable some artistic production (coordination function is real) but at the cost of suppressing overall output and redistributing wealth from artists to employers and gatekeepers (extraction function is real). The mandatrophy is resolved by recognizing that both readings are structurally true—the constraint simultaneously coordinates artistic output with subsistence and extracts labor time that would otherwise be creative. The classification does not collapse into one type; instead, the perspectival gap IS the resolution. The high-extractiveness measurement (0.55) and the presence of both beneficiaries and victims in the base properties confirm the tangled rope diagnosis: this is not a pure coordination mechanism masquerading as extraction, nor a pure extraction mechanism with a coordination justification. It is a genuine structural hybrid.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cultural_output_causality,
    'Does the day job actually subsidize artistic output, or does it primarily suppress it?',
    'Comparative analysis of artistic productivity and quality between supported artists (grants, residencies, teaching positions) vs. day-job artists; longitudinal tracking of career trajectories',
    'If subsidizing: constraint functions as partial coordination (scaffolding structure is real). If suppressing: constraint is pure extraction masquerading as necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_output_causality, empirical, 'Whether day jobs enable or suppress artistic output').

omega_variable(
    alternative_patronage_sufficiency,
    'Can emerging patronage systems (public funding, Patreon, artist collectives) scale to replace the day job within a generational timescale?',
    'Projection of alternative funding growth rates; analysis of funding capacity per artist in emerging systems; comparison with historical patronage systems',
    'If yes: scaffold sunset is structurally real, and this constraint is degrading (Piton). If no: the day job is a permanent feature (Snare or Tangled Rope from powerless perspective).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_patronage_sufficiency, empirical, 'Whether alternative patronage can scale sufficiently').

omega_variable(
    exploitation_threshold,
    'At what hours of day-job labor does the constraint tip from ''enabling creative work'' (coordination) to ''preventing creative work'' (extraction)?',
    'Time-use studies correlating day-job hours with artistic productivity; threshold analysis of creative output vs. wage income',
    'If threshold is low (20 hrs/week): most creatives are in extraction zone. If threshold is high (40+ hrs/week): day job is genuinely enabling for many.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exploitation_threshold, empirical, 'Hours threshold between enabling and suppressing artistic work').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(necessary_day_job, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dayj_tr_t0, necessary_day_job, theater_ratio, 0, 0.32).
narrative_ontology:measurement(dayj_tr_t30, necessary_day_job, theater_ratio, 30, 0.42).
narrative_ontology:measurement(dayj_tr_t60, necessary_day_job, theater_ratio, 60, 0.48).

% Extraction over time
narrative_ontology:measurement(dayj_be_t0, necessary_day_job, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(dayj_be_t30, necessary_day_job, base_extractiveness, 30, 0.5).
narrative_ontology:measurement(dayj_be_t60, necessary_day_job, base_extractiveness, 60, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(necessary_day_job, resource_allocation).
narrative_ontology:affects_constraint(necessary_day_job, cultural_gatekeeping).
narrative_ontology:affects_constraint(necessary_day_job, artist_network_access).
narrative_ontology:affects_constraint(necessary_day_job, intellectual_property_extraction).

% DUAL FORMULATION NOTE:
% The day job constraint is downstream of broader economic inequality and cultural devaluation of artistic work. It represents a specific manifestation of how economies distribute subsistence costs across creative practitioners. The upstream constraints (economic inequality, cultural gatekeeping) have their own ε values; the day job has ε=0.55 reflecting the specific structural coupling of creative vocation with wage labor.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(necessary_day_job, powerful, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
