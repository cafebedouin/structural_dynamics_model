% ============================================================================
% CONSTRAINT STORY: compulsory_education_enforcement
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_compulsory_education_enforcement, []).

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
 *   constraint_id: compulsory_education_enforcement
 *   human_readable: Compulsory Education Enforcement
 *   domain: education/social_policy
 *
 * SUMMARY:
 *   Compulsory education enforcement creates a structural tension between the
 *   genuine coordination need for systematic knowledge transfer and the
 *   extractive use of state power to impose specific forms of education on
 *   populations with alternative values or needs. The constraint exhibits a
 *   full spectrum of DR classifications depending on the observer's
 *   structural position. From the state's perspective, compulsory education
 *   is a coordination mechanism solving the legitimate problem of ensuring
 *   human capital development and social cohesion. From the perspective of
 *   non-compliant families, it is pure extraction enforced through legal
 *   coercion. From the perspective of organized alternative education
 *   advocates, it is a transitional scaffold that will be replaced as
 *   alternative credential pathways mature. The analysis shows how the same
 *   constraint — mandatory school attendance for all children to a specified
 *   age — can be simultaneously a rope, a snare, a tangled rope, a scaffold,
 *   and a piton, depending on the observer's power, exit options, and
 *   temporal horizon.
 *
 * KEY AGENTS:
 *   - State Educational Authority: Primary beneficiary (institutional/arbitrage) — controls enforcement, defines compliance standards, benefits from standardized human capital pipeline
 *   - Non-Compliant Families: Primary victim (powerless/trapped) — face legal penalties, child welfare intervention, no legitimate exit
 *   - Identity-Locked Youth: Secondary victim (powerless/identity_locked) — structurally mobile but identity-fused with family/community rejecting compulsion; exit would dissolve primary identity
 *   - Credentialing Institutions: Beneficiary (institutional/arbitrage) — rely on standardized educational baseline for credential signaling
 *   - Alternative Education Advocates: Organized victim (organized/constrained) — constrained by legal barriers and social legitimacy costs; benefit from coordination standards
 *   - Affluent Private Schooling Users: Powerful partial beneficiary (powerful/mobile) — can arbitrage system but still constrained by regulation; benefit from credential framework
 *   - Educational Modernization Movement: Organized reformer (organized/constrained) — sees compulsion as transitional mechanism to be replaced by competency-based alternatives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(compulsory_education_enforcement, 0.55).
domain_priors:suppression_score(compulsory_education_enforcement, 0.65).
domain_priors:theater_ratio(compulsory_education_enforcement, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(compulsory_education_enforcement, extractiveness, 0.55).
narrative_ontology:constraint_metric(compulsory_education_enforcement, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(compulsory_education_enforcement, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(compulsory_education_enforcement, tangled_rope).
narrative_ontology:human_readable(compulsory_education_enforcement, "Compulsory Education Enforcement").
narrative_ontology:topic_domain(compulsory_education_enforcement, "education/social_policy").

domain_priors:requires_active_enforcement(compulsory_education_enforcement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(compulsory_education_enforcement, state_labor_market_interests).
narrative_ontology:constraint_beneficiary(compulsory_education_enforcement, credentialing_institutions).
narrative_ontology:constraint_beneficiary(compulsory_education_enforcement, professional_standardization).
narrative_ontology:constraint_victim(compulsory_education_enforcement, non_compliant_families).
narrative_ontology:constraint_victim(compulsory_education_enforcement, alternative_education_seekers).
narrative_ontology:constraint_victim(compulsory_education_enforcement, youth_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-COMPLIANT FAMILY (SNARE) — Faces legal coercion (truancy laws, fines, child welfare intervention) with no legitimate exit. Trapped between state mandate and family values/economic constraints. No arbitrage available; compulsion is absolute.
constraint_indexing:constraint_classification(compulsory_education_enforcement, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: IDENTITY-LOCKED YOUTH (SNARE) — Structurally mobile (could exit jurisdiction) but identity-fused with family/community that rejects compulsory schooling. Exit would require abandoning religious identity, cultural community membership, or family relationship. Trapped not by external barriers alone but by internalized framing that makes exit unthinkable.
constraint_indexing:constraint_classification(compulsory_education_enforcement, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 3: ALTERNATIVE EDUCATION ADVOCATES (TANGLED ROPE) — Constrained by legal barriers and social legitimacy costs, but benefits from coordination of education standards and certification recognition. Can exit through private schooling or homeschooling approval (constrained by cost/bureaucracy). Mixed experience: genuine coordination function (shared standards) alongside extraction (mandated curriculum, testing requirements).
constraint_indexing:constraint_classification(compulsory_education_enforcement, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: STATE EDUCATIONAL AUTHORITY (ROPE) — Primary beneficiary. Compulsion provides reliable enrollment and standardized cohorts for policy implementation. Low extraction cost from this position — they control enforcement and define compliance. The constraint solves the genuine coordination problem of ensuring widespread human capital development.
constraint_indexing:constraint_classification(compulsory_education_enforcement, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: CREDENTIALING INSTITUTIONS (ROPE) — Benefit from compulsory baseline education ensuring credential value and labor market standardization. Low extraction from this perspective — they benefit from the enforced coordination without bearing enforcement costs.
constraint_indexing:constraint_classification(compulsory_education_enforcement, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: AFFLUENT PRIVATE SCHOOLING USERS (TANGLED ROPE) — Mobile (can exit to private schools) but still subject to regulation and curriculum standards. Benefit from compulsory education enforcing educational norms and credential frameworks. Moderate experienced extraction: can exit the public system but not the compulsion mandate itself. Asymmetric: richer families can arbitrage the system, poor families cannot.
constraint_indexing:constraint_classification(compulsory_education_enforcement, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: EDUCATIONAL MODERNIZATION MOVEMENT (SCAFFOLD) — Organized agents (learning outcome advocates, competency-based education proponents, credentialing reform) see compulsory school attendance as a transitional mechanism. As alternative credential pathways (apprenticeships, online portfolios, competency verification) mature, the mandatory attendance gate becomes obsolete. Sunset clause: credibility as alternative pathways prove they can produce workforce-ready graduates. Theater ratio moderate because while the mechanism persists, its actual selection/certification function is degraded by grade inflation and diploma devaluation.
constraint_indexing:constraint_classification(compulsory_education_enforcement, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: SCHOOL ATTENDANCE AS PITON (PITON) — The daily attendance ritual itself has become largely performative: grade inflation, social promotion, and credential devaluation mean that formal attendance no longer reliably signals competence. Yet the ritual persists through institutional inertia. Theater ratio high because the enforcement apparatus maintains the form of the constraint while its functional signaling capacity has atrophied.
constraint_indexing:constraint_classification(compulsory_education_enforcement, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some form of systematic knowledge transfer is structurally necessary for industrial/post-industrial societies to function. Compulsory education appears as an immutable requirement of modern civilization. However, structural data contradicts this — the specific form (age-based classroom attendance) is contingent. This perspective's mountain classification is a false summit: it naturalizes a specific institutional arrangement as inherent to civilization itself.
constraint_indexing:constraint_classification(compulsory_education_enforcement, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(compulsory_education_enforcement_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(compulsory_education_enforcement, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(compulsory_education_enforcement, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(compulsory_education_enforcement, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(compulsory_education_enforcement, TR),
    TR >= 0.70.

:- end_tests(compulsory_education_enforcement_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate-high. Compulsory education enforcement extracts compliance from families with alternative educational values, labor constraints, or philosophical objections. However, extractiveness is not as severe as pure coercion constraints (0.72+) because the coordination function is genuine — ensuring widespread literacy and numeracy capability is a real public good that benefits even non-compliant populations. The extraction lies not in the existence of education standards but in the mandatory attendance mechanism that forecloses alternatives. Suppression (0.65): High. Powerful barriers to exit include legal penalties (fines, jail time), child welfare intervention, social stigma, geographic barriers (distance to alternative providers), and credential non-recognition for non-compliant paths. Suppression has increased over the interval as enforcement mechanisms (attendance tracking, truancy courts) have become more sophisticated. Theater ratio (0.58): Moderate. While the educational function is real (students do learn), the daily attendance ritual contains substantial performative elements: grade inflation, social promotion, and credential devaluation mean that formal attendance no longer reliably signals competence. The theater has increased as compulsion has intensified and educational demand has expanded — the ritual persists but its signaling function degrades.
 *
 * PERSPECTIVAL GAP:
 *   The gap between the state's rope classification and the non-compliant family's snare classification is maximal. The state sees a coordination mechanism solving legitimate public good provision; the family sees coercive extraction with no legitimate exit. This gap reveals the constraint's core tension: compulsory education is simultaneously genuine coordination (ensuring human capital) and extractive coercion (imposing specific educational form on non-consenting populations). The analytical observer must choose: either decompose into separate constraints (one for human capital coordination, one for form-imposition extraction) or recognize that tangled rope is the accurate classification — both coordination and extraction occur in the same mechanism, and the beneficiary's rope perception is partial, not wrong.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from structural position: Who bears costs? Who captures benefits? The state and credentialing institutions have arbitrage exit and capture benefits, producing low d and negative/neutral χ from their perspective (they experience the constraint as enabling, not extractive). Non-compliant families have trapped exit and bear costs, producing high d and high χ from their perspective (they experience pure extraction). Identity-locked youth have structural mobility (could physically leave jurisdiction) but identity-bound commitment to families/communities that reject compulsion, producing moderate-to-high d (0.75-0.89) and high χ. Alternative education advocates have constrained exit and mixed costs/benefits, producing moderate d. Affluent families have mobile exit and can partly arbitrage the system, producing moderate d and moderate χ. The analytical observer at civilizational scope risks adopting an institutional perspective that naturalizes the specific form as necessary.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: This constraint resolves mandatrophy by showing that compulsory education is genuinely both coordination AND extraction, depending on the observable. Measured as 'ensuring human capital development' (ε=0.35, Rope) vs measured as 'enforcing specific educational form on non-consenting populations' (ε=0.65, Snare) — these are different constraints. The ε-invariance principle applies: if the observable changes the classification, decompose. However, the natural-language concept 'compulsory education' bundles both observables. The analytical resolution is Tangled Rope: the mechanism simultaneously coordinates (provides education standards everyone benefits from) and extracts (imposes form on non-consenting populations). The state's rope perception is correct about the coordination function. The family's snare perception is correct about the extraction experienced. Both are describing the same constraint; the gap reveals its hybrid nature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    educational_function_vs_social_control,
    'Is compulsory education enforcement primarily a mechanism for developing human capital, or primarily a mechanism for social control and labor market standardization?',
    'Comparative analysis of educational outcomes vs. compliance outcomes; historical analysis of education expansion correlated with labor market demands vs. developmental theory; study of countries with high human capital development but low compulsion rates (homeschooling success, apprenticeship models)',
    'If primarily human capital: extractiveness drops to 0.25-0.35 (Rope). If primarily social control: extractiveness stays at 0.55+ (Tangled Rope/Snare). Determines whether beneficiaries list is legitimate coordination or post-hoc naturalization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(educational_function_vs_social_control, empirical, 'Whether compulsory education functions primarily as human capital development or social control').

omega_variable(
    alternative_pathway_viability,
    'Do alternative education pathways (apprenticeship, online learning, competency-based credentials, homeschooling) produce labor-market-ready graduates at rates comparable to compulsory schooling?',
    'Longitudinal employment outcomes comparison: alternative pathway graduates vs. traditional school graduates at 5, 10, 15-year marks; employer hiring preferences; skill certification reliability across pathways',
    'If viable: scaffold sunset clause is real, theater ratio will decrease as alternatives mature. If not viable: compulsion is justified by genuine coordination need, extractiveness may be reclassified downward. If partially viable for some populations: constraint decomposes into separate stories per demographic/pathway.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_pathway_viability, empirical, 'Whether alternative education pathways produce comparable labor-market outcomes').

omega_variable(
    identity_lock_mechanism_persistence,
    'For identity-locked youth and families, does the identity lock persist after reaching age of majority, or does it dissolve when external enforcement ends?',
    'Longitudinal tracking of post-compulsion behaviors: Do families that resisted continue alternative education with adult children? Do youth that were identity-locked choose compulsory schooling for their own children or continue the resistance? Post-exit identity reconstruction surveys.',
    'If identity lock persists: the binding mechanism is genuinely cognitive/internalized, not just external coercion. Constraint''s suppression is partially internalized. If identity lock dissolves: the binding was external pressure maintained through identity framing; once external pressure ends, the identity deconstructs and suppression was purely structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_persistence, empirical, 'Whether identity-lock binding mechanism persists after legal compulsion ends').

omega_variable(
    credential_inflation_mechanism,
    'Is the observed grade inflation and diploma devaluation a side effect of compulsory mass education, or an independent process driven by credentialing demand inflation?',
    'Historical analysis of grade distributions and employer credential requirements across compulsory-education countries with different enforcement intensities; correlation analysis of grade inflation timing vs. compulsion intensification vs. labor market demand shifts',
    'If caused by compulsion: piton perspective is correct — enforcement maintains form while function degrades. Theater ratio will continue rising. If independent: compulsion and inflation are separate constraints; theater ratio is stabilizing as institutions adapt.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(credential_inflation_mechanism, empirical, 'Whether credential inflation is caused by compulsory education expansion or independent process').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(compulsory_education_enforcement, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cee_tr_t0, compulsory_education_enforcement, theater_ratio, 0, 0.42).
narrative_ontology:measurement(cee_tr_t25, compulsory_education_enforcement, theater_ratio, 25, 0.52).
narrative_ontology:measurement(cee_tr_t50, compulsory_education_enforcement, theater_ratio, 50, 0.58).

% Extraction over time
narrative_ontology:measurement(cee_be_t0, compulsory_education_enforcement, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(cee_be_t25, compulsory_education_enforcement, base_extractiveness, 25, 0.48).
narrative_ontology:measurement(cee_be_t50, compulsory_education_enforcement, base_extractiveness, 50, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(compulsory_education_enforcement, resource_allocation).
narrative_ontology:affects_constraint(compulsory_education_enforcement, credential_inflation_signal_degradation).
narrative_ontology:affects_constraint(compulsory_education_enforcement, cultural_identity_state_conflict).
narrative_ontology:affects_constraint(compulsory_education_enforcement, alternative_education_pathway_recognition).

% DUAL FORMULATION NOTE:
% Compulsory education enforcement decomposes into three structurally distinct constraints: (1) human capital coordination (ensuring baseline literacy/numeracy — genuine public good, ε~0.25, Rope), (2) form imposition (mandating specific classroom-based attendance on non-consenting populations — ε~0.65, Snare), (3) credential standardization (enforcing educational credentials as labor market signals — ε~0.45, Tangled Rope). This story bundles all three through the natural-language concept 'compulsory education enforcement' and classifies as Tangled Rope (0.55). Constraint family decomposition preferred for granular policy analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(compulsory_education_enforcement, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
