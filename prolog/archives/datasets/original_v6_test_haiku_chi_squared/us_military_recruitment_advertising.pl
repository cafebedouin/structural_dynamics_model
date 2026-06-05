% ============================================================================
% CONSTRAINT STORY: us_military_recruitment_advertising
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_military_recruitment_advertising, []).

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
 *   constraint_id: us_military_recruitment_advertising
 *   human_readable: US Military Recruitment Advertising
 *   domain: political/defense/labor_relations
 *
 * SUMMARY:
 *   US military recruitment advertising presents a structural constraint on
 *   informed consent, particularly for economically disadvantaged youth. The
 *   constraint exhibits the properties of a snare: the Department of Defense
 *   benefits from a reliable supply of recruits, while prospective recruits
 *   from low-income backgrounds bear the costs of joining an institution with
 *   significant casualty and disability risk. The recruitment advertising
 *   apparatus suppresses information about combat trauma, sexual assault
 *   prevalence, and long-term disability while emphasizing educational
 *   benefits, career stability, and upward mobility. The constraint operates
 *   through multiple vectors: school-based recruiter access (JROTC programs,
 *   in-school recruitment days), targeted digital advertising (YouTube,
 *   TikTok in low-income ZIP codes), sports sponsorships, esports team
 *   partnerships, and influencer recruitment. The theater ratio (0.81)
 *   reflects that modern military recruitment relies heavily on aspirational
 *   and emotional messaging (cinematic video content, athlete testimonials,
 *   gaming/esports integration) rather than factual information about actual
 *   service conditions. The extractiveness has increased over the 1990–2026
 *   interval as recruitment techniques have become more sophisticated,
 *   targeting has become more granular, and casualty information has been
 *   more effectively suppressed from mainstream recruitment channels.
 *
 * KEY AGENTS:
 *   - Department of Defense (Institutional/Arbitrage) — Beneficiary; solves genuine recruitment problem but extracts through information asymmetry and targeting vulnerability
 *   - Economically Disadvantaged Youth (Powerless/Trapped) — Primary victim; faces limited employment alternatives and targeted recruitment in low-income schools; cannot exit advertising ecosystem
 *   - High School Students (Moderate/Constrained) — Secondary victims; constrained by school relationships with recruiters and limited information about alternatives
 *   - Military Families and Veterans (Organized/Constrained) — Dual interest; benefit from intergenerational participation but bear costs of casualty and trauma
 *   - School Districts (Institutional/Arbitrage) — Piton actors; maintain JROTC and recruiter access for educational framing but actual function is primarily recruitment pipeline; theater-heavy (uniforms, discipline narratives, performances)
 *   - Informed Consent Integrity (Abstract/Trapped) — Structural victim; collective epistemic good that cannot organize or exit; bearing cost of information suppression
 *   - Analytical Observer (Analytical/Analytical) — Risks naturalizing contingent institutional arrangements (message curation, targeting strategies) as immutable features of employment recruitment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_military_recruitment_advertising, 0.58).
domain_priors:suppression_score(us_military_recruitment_advertising, 0.68).
domain_priors:theater_ratio(us_military_recruitment_advertising, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_military_recruitment_advertising, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_military_recruitment_advertising, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(us_military_recruitment_advertising, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_military_recruitment_advertising, snare).
narrative_ontology:human_readable(us_military_recruitment_advertising, "US Military Recruitment Advertising").
narrative_ontology:topic_domain(us_military_recruitment_advertising, "political/defense/labor_relations").

domain_priors:requires_active_enforcement(us_military_recruitment_advertising).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_military_recruitment_advertising, department_of_defense).
narrative_ontology:constraint_beneficiary(us_military_recruitment_advertising, military_contractors).
narrative_ontology:constraint_victim(us_military_recruitment_advertising, prospective_recruits).
narrative_ontology:constraint_victim(us_military_recruitment_advertising, economically_disadvantaged_youth).
narrative_ontology:constraint_victim(us_military_recruitment_advertising, informed_consent_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ECONOMICALLY DISADVANTAGED RECRUIT (SNARE) — Faces limited employment alternatives; recruitment messaging targets low-income neighborhoods with curated imagery emphasizing stability, tuition assistance, and upward mobility while suppressing information about combat trauma, long-term disability, and mortality. Cannot exit the advertising ecosystem; targeted recruitment creates information asymmetry. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.80.
constraint_indexing:constraint_classification(us_military_recruitment_advertising, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: HIGH SCHOOL STUDENT (SNARE) — Constrained by parental authority, school relationships with recruiters, and limited information about alternatives. School-recruiter relationships create information monopoly. Theater-heavy advertising (sports sponsorships, esports teams, social media influencers) creates artificial social proof. d≈0.80, f(d)≈1.20, σ=0.9 → χ≈0.56.
constraint_indexing:constraint_classification(us_military_recruitment_advertising, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: DEPARTMENT OF DEFENSE (ROPE) — Institutional beneficiary with strategic arbitrage. Advertising solves a genuine coordination problem: communicating military opportunities to eligible youth. DoD sees recruitment messaging as coordination; the institutional frame normalizes information curation as legitimate messaging strategy. d≈0.08, f(d)≈-0.11, σ=1.0 → χ≈-0.06. Net beneficiary.
constraint_indexing:constraint_classification(us_military_recruitment_advertising, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: MILITARY FAMILIES AND VETERANS ORGS (TANGLED ROPE) — Organized actors with dual interest: families benefit from recruitment (intergenerational participation, community stability) but also bear costs of casualty, disability, and trauma. See recruitment advertising as partly coordination (connecting qualified recruits to opportunity) and partly extraction (targeting vulnerable populations, suppressing casualty information). d≈0.65, f(d)≈0.95, σ=1.0 → χ≈0.55.
constraint_indexing:constraint_classification(us_military_recruitment_advertising, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: SCHOOL DISTRICTS (PITON) — Institutional actors with degraded function. Schools maintain JROTC programs and recruiter access for stated educational purposes, but theater ratio is high: actual educational function is secondary to recruitment pipeline. Theater includes military aesthetics (uniforms, discipline narratives), JROTC performances, recruiter presence in hallways. theater_ratio≈0.81 satisfies piton gate. d≈0.25, f(d)≈0.05, σ=0.9 → χ≈0.04. Low extraction but high theater.
constraint_indexing:constraint_classification(us_military_recruitment_advertising, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FALSE MOUNTAIN (NATURAL LAW VIEW) — Observer may frame recruitment advertising as an immutable feature of military labor markets: all employment recruitment involves curation and persuasion; military recruitment is continuous with civilian recruiting. However, structural data (ε=0.58, suppression=0.68, theater=0.81) contradicts mountain classification. The engine identifies this as a false summit: military recruitment's information asymmetry, targeting of economically vulnerable populations, and suppression of casualty/disability information are contingent institutional arrangements, not natural laws of employment. accessibility_collapse would be <0.85; resistance >0.15.
constraint_indexing:constraint_classification(us_military_recruitment_advertising, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_military_recruitment_advertising_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_military_recruitment_advertising, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_military_recruitment_advertising, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_military_recruitment_advertising, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_military_recruitment_advertising, TR),
    TR >= 0.70.

:- end_tests(us_military_recruitment_advertising_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The DoD captures recruits from a population with limited alternatives; the extraction is not maximal because recruits do derive real benefits (education, employment, training). However, the 30-year trajectory shows increasing extractiveness as targeting has become more sophisticated. Theater ratio (0.81): High. Modern military recruitment relies heavily on aspirational and emotional content rather than factual service information. This has increased from 0.52 (1990s straightforward recruitment) to 0.81 (contemporary cinematic campaigns, esports sponsorships, influencer partnerships). The theater includes carefully curated imagery (physical fitness, camaraderie, technology), performance of discipline (JROTC uniforms and ceremonies), and narrative construction (stories of upward mobility, service meaning). Suppression (0.68): Moderate-high. Systematic absence of casualty statistics, disability prevalence, sexual assault rates, and mental health outcomes from mainstream recruitment messaging. Information is not unavailable (exists in Department of Veterans Affairs reports, academic studies), but is actively excluded from recruitment materials. The suppression is not absolute — some recruits find alternative information sources — but the default recruit pathway involves significant information asymmetry.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap separates the DoD's frame (recruitment is coordination solving a labor market information problem) from the economically disadvantaged youth's frame (recruitment is targeted extraction from a population with no exit alternatives). The DoD sees rope: messaging enables matching qualified recruits to legitimate opportunities. Economically disadvantaged youth see snare: limited employment alternatives, targeted recruitment in their schools, information suppression about casualty/disability, and contractual lock-in (8-year commitment). School districts occupy a middle position (piton): they maintain recruiter access for ostensible educational purposes (JROTC leadership training), but the actual function is primarily pipeline recruitment; the theatrical component (uniforms, drill performances) creates social proof rather than educational value. Military families see tangled rope: they participate in recruitment networks and benefit from intergenerational military identity, but they also bear the actual costs (casualty, disability, trauma) that recruitment advertising suppresses. The false mountain perspective (naturalized recruitment as immutable feature of labor markets) obscures the structural contingency: this particular form of recruitment — targeting economically vulnerable youth with emotionally manipulative content while suppressing casualty information — is not inherent to military employment but is a deliberate institutional arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Economically disadvantaged recruits: Victim + trapped → d≈0.92, f(d)≈1.38. Near-maximum extraction. Limited employment alternatives trap this population; information suppression prevents exit; career lock-in is contractual (8-year commitment). High school students: Victim + constrained → d≈0.80, f(d)≈1.20. High extraction. School-recruiter relationship creates access monopoly; parental authority and school authority constrain exit; information asymmetry favors recruiter. DoD: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Net beneficiary. Institutional actor with strategic flexibility; can adjust recruitment messaging, target populations, and information disclosure. Military families: Victim + constrained (but with some benefits) → d≈0.65, f(d)≈0.95. Mixed extraction and coordination. Families benefit from intergenerational military identity and community, but also bear casualty/disability costs that recruitment advertising suppresses. School districts: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.09. Piton classification comes from theater gate (0.81 ≥ 0.70), not from high directionality; schools benefit from recruiter relationships (financial support for JROTC, ease of student data access) and arbitrage (can withdraw recruiter access if pressured, though rarely do). Informed consent integrity: Victim + trapped → d≈0.95, f(d)≈1.42. Abstract collective with no organizational capacity; cannot exit constraint ecosystem.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy between 'military recruitment is legitimate labor market coordination' (DoD frame, rope classification) and 'military recruitment is extraction from vulnerable populations' (recruit frame, snare classification). The key distinction is not whether military recruitment exists (it does, and some coordination function is necessary), but whether the actual instantiation of this constraint exhibits the structural properties of snare (high extraction, high suppression, high theater, targeting of vulnerable populations, information asymmetry). The structural data (ε=0.58, suppression=0.68, theater=0.81) supports snare classification from the recruit perspective. The DoD's rope classification is correct from its institutional perspective — it genuinely solves a recruitment problem — but misses the asymmetric extraction and information suppression that characterize the constraint from the recruit's perspective. The mandatrophy is resolved by recognizing that both are correct perspectives on the same underlying structure: the constraint is snare-from-below (recruit view) and rope-from-above (DoD view), and the presheaf of perspectives reveals the true structure: a snare that maintains its appearance of rope-like coordination through theater, targeting, and suppression of alternative framings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    informed_consent_threshold,
    'What level of casualty/disability information disclosure constitutes adequate informed consent for military recruitment targeting of youth?',
    'Comparative analysis of recruitment messaging in volunteer militaries (US, UK) vs conscript systems (Israel, S. Korea); assessment of recruits'' actual baseline knowledge vs advertised knowledge; longitudinal tracking of recruit regret and documented unrealistic expectations',
    'If threshold requires explicit casualty statistics in all recruitment: current advertising violates informed consent, classification strengthens as snare. If threshold is satisfied by buried fine-print disclaimers: snare classification remains but suppression metric may need adjustment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(informed_consent_threshold, preference, 'Required level of casualty and disability information for informed consent').

omega_variable(
    school_recruiter_coercion_mechanism,
    'Does recruiter access to schools create coercive pressure on students, or is school-based recruitment merely convenient information provision?',
    'Comparison of recruitment rates in schools with vs without JROTC/recruiter presence; analysis of recruiter targeting data (zip codes, school demographics); student interviews on perceived pressure; analysis of school district policies regarding recruiter access',
    'If strong coercive mechanism: exit_options for high school student perspective should be ''trapped'' not ''constrained''; classification shifts toward pure snare across younger perspectives. If convenient information: exit_options justified as ''constrained,'' tangled rope classification becomes more defensible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(school_recruiter_coercion_mechanism, empirical, 'Whether school-based recruiting creates systematic coercive pressure').

omega_variable(
    advertising_suppression_vs_curation,
    'Does military recruitment advertising constitute legitimate message curation or illegitimate suppression of material information?',
    'Comparison of casualty/disability prevalence rates vs information presented in advertising; analysis of advertiser guidelines and editorial policies; recruit survey on whether they recalled casualty information before enlisting; longitudinal outcome data on mismatch between advertised vs actual experience',
    'If suppression is systematic and deliberate: supports high suppression metric (0.68) and snare classification. If curation is comparable to civilian recruitment: snare classification weakens toward tangled rope; suppression metric should decrease to ~0.40.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(advertising_suppression_vs_curation, empirical, 'Whether information absence is legitimate curation or illegitimate suppression').

omega_variable(
    theater_decline_trajectory,
    'Is military recruitment advertising theater ratio increasing (Goodhart drift toward pure persuasion theater) or stable (constant background theater in all recruitment)?',
    'Longitudinal analysis of recruitment campaign content (1980–2026); measurement of factual claim density vs emotional/aspirational content; analysis of budget allocation (special operations cinematography vs basic information materials); comparison to civilian recruitment advertising theater baseline',
    'If theater is increasing above civilian baseline: piton classification becomes stronger; degradation narrative supported. If theater is constant or comparable: piton classification may be premature; constraint may be a stable tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_decline_trajectory, empirical, 'Trajectory of theater ratio in recruitment messaging').

omega_variable(
    economic_vulnerability_targeting,
    'Is military recruitment deliberately targeted at economically disadvantaged youth, or is disparity in recruitment prevalence simply a consequence of geographic and information access differences?',
    'Analysis of recruitment spending by zip code and median household income; recruiter training materials and targeting guidelines; comparison of recruiter deployment density in high-poverty vs affluent school districts; analysis of advertising platform selection (YouTube, TikTok in low-income demographics vs LinkedIn, college-affiliated channels in affluent regions)',
    'If deliberate targeting: supports ''victims'' classification of disadvantaged youth; suppression metric and snare classification reinforced. If passive disparity: snare classification may weaken; suppression becomes more about information asymmetry than active targeting.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(economic_vulnerability_targeting, empirical, 'Whether recruitment targeting is deliberately concentrated on economically vulnerable populations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_military_recruitment_advertising, 1990, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usma_tr_t0, us_military_recruitment_advertising, theater_ratio, 0, 0.52).
narrative_ontology:measurement(usma_tr_t15, us_military_recruitment_advertising, theater_ratio, 15, 0.68).
narrative_ontology:measurement(usma_tr_t30, us_military_recruitment_advertising, theater_ratio, 30, 0.81).

% Extraction over time
narrative_ontology:measurement(usma_be_t0, us_military_recruitment_advertising, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(usma_be_t15, us_military_recruitment_advertising, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(usma_be_t30, us_military_recruitment_advertising, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_military_recruitment_advertising, resource_allocation).
narrative_ontology:affects_constraint(us_military_recruitment_advertising, socioeconomic_educational_access).
narrative_ontology:affects_constraint(us_military_recruitment_advertising, informed_labor_consent).
narrative_ontology:affects_constraint(us_military_recruitment_advertising, military_casualty_epistemic_suppression).

% DUAL FORMULATION NOTE:
% Military recruitment advertising is downstream of broader US military force structure constraints and economic inequality constraints. Causally independent are the specific recruitment messaging strategies, information suppression mechanisms, and targeting methodologies that constitute this constraint. The upstream constraint (economic inequality creating limited employment alternatives for low-income youth) enables this constraint but is structurally distinct.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_military_recruitment_advertising, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
