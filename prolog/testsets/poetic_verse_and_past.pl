% ============================================================================
% CONSTRAINT STORY: poetic_verse_and_past
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_poetic_verse_and_past, []).

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
 *   constraint_id: poetic_verse_and_past
 *   human_readable: The Inescapable Tale of Institutional Expectation
 *   domain: social/cultural/institutional
 *
 * SUMMARY:
 *   The preparatory school constraint depicted in 'Dead Poets Society' models
 *   the rigid institutional and familial expectations that govern student
 *   trajectories. This constraint exhibits the full range of DR
 *   classification types depending on observer position. From the student's
 *   perspective trapped within the institution, the constraint is a pure
 *   snare: narrow definitions of success, suppressed individuality,
 *   predetermined career paths, and no viable exit options create maximum
 *   extraction with minimal coordination benefit. From the institution's
 *   perspective, it is a coordination mechanism efficiently sorting and
 *   credentialing students. From the parent-stakeholder perspective, it is a
 *   tangled hybrid: they benefit from the prestige and network but bear costs
 *   in tuition and emotional estrangement. From organized alternative
 *   educators, it is a temporary scaffold with a sunset clause as online
 *   credentialing and competency-based hiring undermine traditional
 *   gatekeeping. From the institution's own self-examination, it is a piton:
 *   much of institutional effort (uniforms, hierarchy, chapel, codes) is
 *   performative maintenance of tradition rather than functional
 *   optimization. The analytical observer risks naturalizing the constraint
 *   as an immutable feature of social stratification, but the structural data
 *   reveals it as a contingent institutional arrangement. The constraint's
 *   extractiveness (0.58) and suppression (0.72) reflect significant costs
 *   borne by students, while theater ratio (0.68) indicates substantial
 *   performative content maintaining tradition.
 *
 * KEY AGENTS:
 *   - Individual Students: Primary victims (powerless/trapped) — bear extraction costs of suppressed creativity, predetermined futures, and conformity requirements with no viable exit options
 *   - Institutional Administrators: Primary beneficiaries (institutional/arbitrage) — capture prestige, tuition revenue, network control; maintain institutional legitimacy and selection power
 *   - Parents: Secondary stakeholders (moderate/constrained) — benefit from social prestige and credential signal but bear tuition costs and face dilemma of supporting or challenging institutional expectations
 *   - Legacy Families: Secondary beneficiaries (institutional/arbitrage) — benefit from inherited network access, tradition preservation, and social continuity; their children experience lower extraction pressure
 *   - Faculty Dissenters: Organized agents (organized/constrained) — perceive constraint as over-narrow definition of educational success; see exit path through alternative pedagogies and student liberation
 *   - Alternative Education Coalition: Organized agents (organized/constrained) — build parallel pathways (project-based learning, competency-based hiring, online credentials) creating sunset clause for traditional gatekeeping
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(poetic_verse_and_past, 0.58).
domain_priors:suppression_score(poetic_verse_and_past, 0.72).
domain_priors:theater_ratio(poetic_verse_and_past, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(poetic_verse_and_past, extractiveness, 0.58).
narrative_ontology:constraint_metric(poetic_verse_and_past, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(poetic_verse_and_past, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(poetic_verse_and_past, snare).
narrative_ontology:human_readable(poetic_verse_and_past, "The Inescapable Tale of Institutional Expectation").
narrative_ontology:topic_domain(poetic_verse_and_past, "social/cultural/institutional").

domain_priors:requires_active_enforcement(poetic_verse_and_past).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(poetic_verse_and_past, institutional_administrators).
narrative_ontology:constraint_beneficiary(poetic_verse_and_past, legacy_families).
narrative_ontology:constraint_victim(poetic_verse_and_past, individual_students).
narrative_ontology:constraint_victim(poetic_verse_and_past, creative_potential).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL STUDENT (SNARE) — Trapped within institutional mechanisms that define success narrowly: academic excellence, obedience, predetermined career paths. Exit options severely constrained by family financial dependency, social reputation, and limited outside alternatives. The student bears maximum extraction cost: suppression of individual voice, creative exploration, and authentic selfhood. Career trajectory is predetermined; deviation risks total social collapse.
constraint_indexing:constraint_classification(poetic_verse_and_past, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: PARENT AS STAKEHOLDER (TANGLED ROPE) — Parents benefit from the institutional constraint (prestige, social standing, predictable outcomes) but also bear costs (tuition burden, emotional estrangement from children, investment in a system whose outcomes may not serve their child's actual flourishing). They are both beneficiaries and victims: they have purchased access to the coordinating mechanism (elite network, standardized education) but are partly captured by it. Moderate exit options exist (transfer to public school, alternative education) but carry substantial social and financial costs.
constraint_indexing:constraint_classification(poetic_verse_and_past, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: WELTON ACADEMY INSTITUTION (ROPE) — The preparatory school benefits from the constraint as a pure coordination mechanism: it efficiently sorts, credentials, and networks students into social and economic positions. The institution has high exit options through arbitrage (can redefine mission, shift enrollment, adapt prestige metrics). From this perspective, the constraint functions as a coordination good — it solves the collective action problem of selecting and preparing students for institutional roles. Extraction flows toward the institution, but the institution experiences this as legitimate coordination function.
constraint_indexing:constraint_classification(poetic_verse_and_past, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: ALTERNATIVE EDUCATION COALITION (SCAFFOLD) — Organized actors (progressive educators, homeschooling networks, project-based learning advocates, online education platforms) perceive the traditional preparatory school constraint as a temporary institutional arrangement with a clear sunset clause. Alternative models (Montessori, Waldorf, democratic schools, competency-based learning) are building parallel pathways that bypass traditional credentialing entirely. The constraint's extraction mechanism weakens as employers increasingly hire based on demonstrated competence rather than diploma prestige. Estimated sunset: 15-25 years as credential alternatives mature and hiring practices decouple from institutional gatekeeping.
constraint_indexing:constraint_classification(poetic_verse_and_past, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: WELTON ACADEMY AS PITON (THEATER VIEW) — Despite its self-image as a premier educational institution, much of Welton's actual function is performative: the uniforms, the hierarchies, the chapel, the strict codes of conduct are theatrical maintenance of tradition rather than functional optimization of learning. The institution's real power (network access, credentialing signal) persists despite degraded educational function, maintained by institutional inertia and the difficulty of exit. Theater ratio (0.68) reflects that significant institutional effort goes to performative displays of tradition and order rather than pedagogical innovation. The constraint persists because stakeholders remain invested in the theatrical performance.
constraint_indexing:constraint_classification(poetic_verse_and_past, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURALIZATION VIEW (MOUNTAIN) — From a civilizational perspective, one might argue that institutional gatekeeping and selective credentialing are inherent to any complex society that must allocate scarce positions — this is a natural law of social organization. The constraint appears immutable because societies have always stratified. However, this classification risks naturalizing what is contingent: the specific form of the constraint (preparatory schools, legacy admissions, narrow success metrics) is institutional, not natural. The engine's false summit detector will flag this perspective as naturalization unless the accessibility_collapse and resistance metrics show genuine immutability.
constraint_indexing:constraint_classification(poetic_verse_and_past, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(poetic_verse_and_past_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(poetic_verse_and_past, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(poetic_verse_and_past, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(poetic_verse_and_past, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(poetic_verse_and_past, TR),
    TR >= 0.70.

:- end_tests(poetic_verse_and_past_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts significant costs from students (suppressed creativity, predetermined futures, identity conformity) while delivering coordination benefits primarily to institutions and legacy families. The value reflects that some students genuinely benefit from rigorous structure and network access, but the primary flow of extraction runs toward institutional and family interests. Suppression (0.72): High. Multiple mechanisms enforce conformity: financial dependency (tuition/family support), social reputation (expulsion, shame), limited information about alternatives, and psychological internalization of expectations. Career consequences of deviation are severe. Theater ratio (0.68): High-moderate. Institutional performance (uniforms, hierarchy, chapel, codes, tradition) comprises a substantial portion of institutional function. These elements serve boundary-marking and identity-reinforcement roles but carry diminishing pedagogical returns as modern learning is increasingly decoupled from institutional theater.
 *
 * PERSPECTIVAL GAP:
 *   The maximum perspectival gap occurs between student (snare) and institution (rope) perspectives. The student experiences suppression, extraction, and trapped exit; the institution experiences efficient coordination, legitimate network formation, and voluntary sorting. This is not a disagreement about facts but about structural position — the same institutional mechanism that coordinates and benefits the institution extracts from and constrains the student. The scaffold perspective reveals a temporal gap: the constraint's extraction mechanism weakens as employer hiring decouples from institutional prestige. The piton perspective reveals a functional gap: institutional theater persists despite reduced pedagogical necessity. The naturalization risk (mountain perspective) occurs when external observers attribute the constraint to inevitable social structure rather than contingent institutional design choices.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality is bifurcated. For students (powerless/trapped), the institutional apparatus creates high d → high χ: they experience maximum extraction. For institutional administrators (institutional/arbitrage), the constraint is a beneficiary position with d near 0 → negative χ: the institution experiences coordination benefit with no experienced extraction. For parents (moderate/constrained), directionality is mixed: they are partly beneficiaries (prestige, network) and partly victims (tuition, emotional cost), deriving moderate d ≈ 0.50. The scaffold perspective (alternative educators) has constrained exit but not trapped — they see the constraint as failing and buildable alternatives, so d reflects organized agency despite institutional pressure. The piton perspective observes degradation of functional capacity relative to performative maintenance, reflecting the attenuation of genuine coordination function over time.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by distinguishing the institutional coordination function (real but limited) from the extraction mechanism overlaid upon it. The institution genuinely does coordinate student sorting and network formation — these are real coordination benefits. But the constraint's primary extraction target is not the coordination benefit; it is the student's creative potential, authentic identity, and future autonomy. The constraint extracts future possibilities (deferred creative self-expression, narrowed life trajectory) in exchange for present institutional security. The mandatrophy is resolved by recognizing that tangled rope classification (mixed coordination + extraction) applies only to the parent/institution relationship; the student/institution relationship is snare (extraction with minimal coordination benefit to the student). The alternative educator scaffold perspective further clarifies: the institutional coordination function is being replicated by newer mechanisms (online credentials, portfolio assessment, project-based hiring) that accomplish sorting with lower extraction costs and theater. The constraint's extractiveness persists not because coordination requires it, but because institutional interests benefit from maintaining gatekeeping power.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    creative_potential_fungibility,
    'Is suppressed creative potential genuinely lost, or merely deferred until after institutional exit?',
    'Longitudinal tracking of creative output in post-institutional life; comparison of creativity metrics (artistic production, entrepreneurial innovation) across age cohorts (college, 30s, mid-career) for students from suppressive vs permissive environments',
    'If genuinely lost: extraction is permanent and severe (supports snare classification). If deferred: extraction is temporary (supports scaffold classification). If partially recovered: supports tangled rope (mixed benefits/costs).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(creative_potential_fungibility, empirical, 'Whether suppressed creativity is lost permanently or merely deferred').

omega_variable(
    institution_functional_necessity,
    'Is the institutional constraint functionally necessary for the coordination benefit (network, credentialing, social sorting) or merely historically contingent?',
    'Comparative analysis: outcomes for students from alternative educational pathways (progressive schools, homeschooled, online-credentialed); employer hiring patterns relative to educational pedigree over time; network formation mechanisms in credentialless environments',
    'If functionally necessary: constraint is mountain-like (immutable feature of social coordination). If contingent: constraint is institutional choice (snare or tangled rope). If being replaced: constraint is scaffold with sunset.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institution_functional_necessity, empirical, 'Whether institutional constraints are functionally necessary for coordination').

omega_variable(
    family_expectation_internalization,
    'To what degree do students internalize family expectations as authentic self-identity versus experiencing them as external coercion?',
    'Qualitative analysis of student narratives; psychological assessment of identity congruence; correlation between institutional conformity and post-institutional psychological integration',
    'If fully internalized: extraction appears voluntary (constraint narrows to coordination). If fully external: extraction is coercive (supports snare). If partial: supports tangled rope (mixed internalization).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(family_expectation_internalization, conceptual, 'Degree of internalization of institutional expectations as authentic self').

omega_variable(
    alternative_pathway_viability,
    'Do non-traditional credentialing pathways (direct employment, apprenticeship, portfolio-based hiring) provide genuine economic mobility or are they illusions masking institutional gatekeeping?',
    'Wage and career trajectory comparison: traditional credentials vs alternative pathways across 20-year career span; employer hiring surveys regarding credential requirements; barrier analysis for alternative pathway entrants',
    'If truly viable: scaffold sunset is real (alternative routes are materializing). If illusory: snare classification strengthens (institutional gatekeeping persists despite alternatives). If partially viable: tangled rope (mixed pathways with different extraction profiles).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_pathway_viability, empirical, 'Viability of non-traditional credentialing pathways').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(poetic_verse_and_past, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(poet_tr_t0, poetic_verse_and_past, theater_ratio, 0, 0.55).
narrative_ontology:measurement(poet_tr_t2, poetic_verse_and_past, theater_ratio, 2, 0.61).
narrative_ontology:measurement(poet_tr_t4, poetic_verse_and_past, theater_ratio, 4, 0.68).

% Extraction over time
narrative_ontology:measurement(poet_be_t0, poetic_verse_and_past, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(poet_be_t2, poetic_verse_and_past, base_extractiveness, 2, 0.5).
narrative_ontology:measurement(poet_be_t4, poetic_verse_and_past, base_extractiveness, 4, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(poetic_verse_and_past, resource_allocation).
narrative_ontology:affects_constraint(poetic_verse_and_past, legacy_admission_gatekeeping).
narrative_ontology:affects_constraint(poetic_verse_and_past, credentialism_wage_premium).
narrative_ontology:affects_constraint(poetic_verse_and_past, educational_prestige_signaling).

% DUAL FORMULATION NOTE:
% The institutional expectation constraint decomposes into three structural components: (1) legacy gatekeeping (institutional resource allocation mechanism), (2) credentialist wage premium (labor market enforcement), and (3) prestige signaling (status competition). Each has distinct ε values reflecting different extraction profiles. Legacy gatekeeping is pure snare (ε≈0.70). Credentialism is tangled rope (ε≈0.45, benefits some workers while trapping others). Prestige signaling is rope (ε≈0.25, coordination without primary extraction). The poetic_verse_and_past constraint operates at the local level where all three mechanisms converge within institutional setting.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(poetic_verse_and_past, moderate, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
