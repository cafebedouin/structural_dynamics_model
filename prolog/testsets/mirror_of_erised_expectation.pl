% ============================================================================
% CONSTRAINT STORY: mirror_of_erised_expectation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mirror_of_erised_expectation, []).

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
 *   constraint_id: mirror_of_erised_expectation
 *   human_readable: The Erised Career/Stability Mirror
 *   domain: psychological/economic
 *
 * SUMMARY:
 *   The Erised expectation is a psychological-economic snare that captures
 *   Millennials (born 1981-1996) in a false promise of meritocratic
 *   stability. The constraint originates in 1990s cultural narratives —
 *   Disney films, educational policy, parental messaging — that portrayed a
 *   magical world where ordinary people defeat evil through virtue and merit,
 *   translating to economic narratives that 'education = stability, hard work
 *   = success, credentials = security.' This narrative remained plausible in
 *   the 1990s when college attendance could still deliver economic mobility.
 *   By the 2010s, the material reality had shifted (wage stagnation, housing
 *   costs, credential inflation, gig economy precarity) while the narrative
 *   persisted, creating a gap between expectation and outcome. Millennials
 *   internalized the promise early (ages 5-18), then encountered
 *   contradictory evidence (ages 20-35) with insufficient opportunity to exit
 *   without invalidating the years of sacrifice already invested. The
 *   constraint extracts psychological labor (managing cognitive dissonance),
 *   economic labor (credential stacking, unpaid internships), and temporal
 *   labor (degree completion delays labor market entry). The theater ratio
 *   has risen as institutional credentialists double down on credentials
 *   despite knowing the correlation with job performance has degraded — the
 *   meritocratic narrative now serves as theatrical justification for sorting
 *   decisions rather than as a functional mechanism for identifying talent.
 *
 * KEY AGENTS:
 *   - Millennial Precariat: Primary victims (powerless/trapped) — internalized the narrative, now trapped between expectation and material reality. Bearing full extraction cost through credential costs, opportunity costs, emotional labor.
 *   - Institutional Credentialists: Primary beneficiaries (institutional/arbitrage) — universities, employers, HR departments capture value from the narrative (tuition revenue, low-cost labor through internships, simple sorting mechanisms). Benefit from credential inflation without responsibility for outcomes.
 *   - Narrative Gatekeepers: Beneficiaries (institutional/arbitrage) — media, policy makers, educational leaders who constructed and perpetuate the meritocratic narrative. Benefit from its continued plausibility.
 *   - Credential Stackers: Moderate victims (moderate/constrained) — caught in arms race, pursuing credentials to signal competence while bearing the cost. Experience mixed extraction: some coordination (narrative alignment) and asymmetric cost-bearing.
 *   - Alternative Pathway Coalition: Organized agents (organized/mobile) — bootcamp operators, skills-based employers, portfolio-first hiring advocates building exit ramps. See opportunity to weaken the snare.
 *   - Early-Career Professionals: Mixed position (organized/mobile) — credentialed but precarious. Mobile enough to exit aspects of the constraint but constrained by need to recoup sunk credential costs.
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the contingent institutional arrangement (credential-based sorting) as inherent to market economics.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mirror_of_erised_expectation, 0.58).
domain_priors:suppression_score(mirror_of_erised_expectation, 0.68).
domain_priors:theater_ratio(mirror_of_erised_expectation, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mirror_of_erised_expectation, extractiveness, 0.58).
narrative_ontology:constraint_metric(mirror_of_erised_expectation, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(mirror_of_erised_expectation, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mirror_of_erised_expectation, snare).
narrative_ontology:human_readable(mirror_of_erised_expectation, "The Erised Career/Stability Mirror").
narrative_ontology:topic_domain(mirror_of_erised_expectation, "psychological/economic").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mirror_of_erised_expectation, institutional_credentialists).
narrative_ontology:constraint_beneficiary(mirror_of_erised_expectation, narrative_gatekeepers).
narrative_ontology:constraint_victim(mirror_of_erised_expectation, millennial_precariat).
narrative_ontology:constraint_victim(mirror_of_erised_expectation, aspiring_achievers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE TRAPPED MILLENNIAL (SNARE) — Internalized the 1990s narrative that hard work + merit = stability. Trapped between the promise and the structural reality: education costs exploded, wage stagnation deepened, housing became unaffordable, gig economy precarity accelerated. No exit without admitting the narrative was false. Maximum extraction: unpaid internships, credential stacking, emotional labor managing the cognitive dissonance between expectation and outcome.
constraint_indexing:constraint_classification(mirror_of_erised_expectation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE CREDENTIAL STACKER (TANGLED ROPE) — Constrained by the need to signal competence in an oversaturated labor market. Benefits from meritocratic narrative (justifies why they are striving) while extracting value from themselves: unpaid internships, graduate degrees, professional certifications. Mixed experience: some coordination (the narrative aligns their effort with institutional expectations) and asymmetric extraction (they bear the full cost of the game while institutions capture the value of their labor).
constraint_indexing:constraint_classification(mirror_of_erised_expectation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE UNIVERSITY CREDENTIALER (ROPE) — Experiences the constraint as pure coordination: the meritocratic narrative aligns student effort with institutional mission. Students internalize the promise, then pursue credentials that universities supply. Mutual benefit framing: universities gain enrollment and tuition revenue; students are told they gain economic mobility. Low coercion from this perspective — exit is available (don't enroll) and the transaction feels fair. This perspective generates the theater that sustains the snare.
constraint_indexing:constraint_classification(mirror_of_erised_expectation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: THE HUMAN RESOURCES GATEKEEPER (PITON) — Maintains credential inflation as a proxy for competence screening despite knowing the correlation has degraded. Uses the meritocratic narrative to justify sorting decisions, but the mechanism is largely performative: credentials serve as a signal of conformity rather than capability. Theater ratio high (0.81) — the credential review ritual persists through institutional inertia. Experiences this as constrained, not arbitrage, because the system generates its own requirements (credential arms race) that lock in the gatekeeper's behavior.
constraint_indexing:constraint_classification(mirror_of_erised_expectation, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ALTERNATIVE PATHWAY COALITION (SCAFFOLD) — Organized agents (bootcamp operators, portfolio-first employers, skill-based hiring advocates) are building exit ramps from the credential-based snare. Bootcamps, apprenticeships, and skill certification bypass the meritocratic narrative entirely. These pathways have sunset logic: as they mature and gain employer legitimacy, the traditional credential monopoly weakens. Lower experienced extraction because this coalition has agency and sees a real exit path.
constraint_indexing:constraint_classification(mirror_of_erised_expectation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: THE EARLY-CAREER PROFESSIONAL (TANGLED ROPE) — Credentialed but employed in precarious work. Benefits from the meritocratic narrative (justifies their educational investment) while experiencing it as false (degree did not deliver promised stability). Mobile enough to job-hop or change fields, but constrained by the need to recoup credential costs. Mixed experience: some coordination (narrative alignment with effort), asymmetric extraction (they bore credential costs, capture modest salary gains). Extraction is real but not total because they retain some agency.
constraint_indexing:constraint_classification(mirror_of_erised_expectation, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FALSE SUMMIT (MOUNTAIN) — From a civilizational view, might see the meritocratic narrative as an immutable feature of market economics: 'In any system with competition for jobs, credentialing is necessary.' However, this naturalizes what is structurally contingent: the *specific form* of credentialism (degree-based, institution-controlled, expensive) is not inherent — alternative sorting mechanisms (apprenticeships, portfolio assessment, skills testing) exist and work. Engine's false summit detector will flag this as naturalization of a contingent arrangement.
constraint_indexing:constraint_classification(mirror_of_erised_expectation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mirror_of_erised_expectation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(mirror_of_erised_expectation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(mirror_of_erised_expectation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(mirror_of_erised_expectation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(mirror_of_erised_expectation, TR),
    TR >= 0.70.

:- end_tests(mirror_of_erised_expectation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The snare extracts psychological labor (sustained hope despite falsifying evidence), economic labor (credential costs, internships, degree-completion delays), and temporal labor (years of preparation for promised outcomes that fail to materialize). The extraction is not complete (some Millennials do achieve stability, some alternative pathways exist) but affects a large, identifiable cohort. Suppression (0.68): High. Escape from the snare requires psychological acknowledgment that the foundational narrative is false — years of sacrifice were based on a falsehood. Suppression comes from within (sunk-cost commitment, identity investment in the striver identity) and without (social messaging that continues to reinforce meritocratic ideology). Alternative pathways exist but remain countercultural (seen as 'lesser' or inferior to the credential path). Theater ratio (0.81): Very high. The meritocratic narrative is now substantially theatrical. Institutions continue credential-based sorting despite knowing that degree-holder performance and job requirements have decoupled. The ritual persists because it serves organizational needs (simple, legally defensible sorting mechanism) independent of its efficacy. Hiring managers know that the job does not require the degree, but the institutional apparatus maintains the requirement. This is peak Piton behavior from the institutional perspective.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence. From the beneficiary's view (universities, HR departments), it is pure Rope — a coordination mechanism that aligns student effort with institutional supply of credentials. From the trapped Millennial's view, it is Snare — extraction with no exit. From the credential stacker's view, it is Tangled Rope — both coordination (the narrative motivates their effort) and asymmetric extraction (they bear all the cost). The alternative pathway coalition sees it as Scaffold — a temporary problem being solved by new hiring mechanisms. The institutional gatekeeper sees it as Piton — a degraded ritual they maintain through inertia despite knowing it fails. The analytical observer at civilizational scale risks seeing it as Mountain (meritocracy is inherent to markets), but this is a false summit — the specific *form* of credentialism is contingent. The perspectival gaps arise because different agents experience the constraint's directionality differently: beneficiaries have low d (derive value), trapped agents have high d (bear costs), constrained agents have mid-range d (mixed experience).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from the agent's structural position and exit capacity. The trapped Millennial has d ≈ 0.95 (maximum victim): internalized the narrative young, invested years in credentials, no escape path without admitting the investment was wasted. The institutional credentialist has d ≈ 0.05 (maximum beneficiary): captures tuition revenue, low-cost labor, simple sorting mechanism, no cost if students fail to achieve promised outcomes. The credential stacker has d ≈ 0.55 (symmetric): benefits from narrative alignment (motivated to do what the narrative says) while bearing the cost of credential acquisition. The alternative pathway organizer has d ≈ 0.35 (slight beneficiary): positioned to offer a solution, gains market share as the snare weakens, but also constrained by the credentialist incumbents' resistance. The early-career professional has d ≈ 0.62 (moderate victim): invested in credentials (cost already sunk), but mobile enough to partially exit (can change fields, build portfolio independently). The gatekeeper has d ≈ 0.10 (beneficiary): maintains the mechanism because it serves organizational needs, bears no cost if the mechanism fails.
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE CLASSIFICATION RESOLUTION: The constraint classifies as Snare across multiple perspectives (trapped Millennial, credential stacker, early-career professional) but as Rope/Scaffold/Piton from other angles. The mandatrophy is resolved by recognizing that this is not a contradiction — it is the natural consequence of an extractive constraint. Different structural positions produce different classifications. The Snare classification is confirmed by: (1) high extractiveness (0.58 > 0.46 threshold), (2) high suppression (0.68 > 0.60 threshold), (3) clear victim group (millennial_precariat), (4) no exit mechanism from the trapped agent's perspective, (5) chi computation shows high effective extraction when directionality is high (d ≈ 0.95 for trapped agents). The beneficiary's Rope classification is also accurate — from their perspective, it IS pure coordination with no extraction. The constraint is structurally Snare (asymmetric extraction) but theatrically maintained through the Rope framing (beneficiaries and gatekeepers genuinely believe the meritocratic narrative). This is a snare held together by theatrical coordination — a hybrid that manifests differently depending on position.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    meritocratic_narrative_origin,
    'To what extent is the 1990s meritocratic narrative genuinely false versus situationally accurate (was meritocracy more real in the 1990s than now)?',
    'Comparative analysis of wage/credentialing correlation 1990-2000 versus 2010-2026. Intergenerational mobility statistics. Returns to education metrics over time.',
    'If the narrative was once true: snare is a *degradation* of a formerly functional system, and the transition point matters for understanding blame and causality. If the narrative was always false: snare is structural/enduring, and the narrative''s power lies entirely in its psychological efficacy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(meritocratic_narrative_origin, empirical, 'Whether meritocratic outcomes were more achievable in the 1990s').

omega_variable(
    alternative_pathway_scalability,
    'Can skill-based hiring and portfolio assessment scale to replace credential-based sorting for the majority of labor market entrants, or is bootcamp-style training a niche pathway?',
    'Employer adoption rates of skills-based hiring. Career outcome comparison: bootcamp graduates vs traditional degree holders, controlling for selection effects. Labor market share of alternative pathways over 10 years.',
    'If scalable: scaffold sunset is real, and alternative pathways will drain the snare. If niche: alternative coalition remains marginal, and the snare persists as the dominant extraction mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_pathway_scalability, empirical, 'Whether alternative pathways can scale as primary hiring mechanism').

omega_variable(
    internalization_mechanism_breakdown,
    'What causes some Millennials to remain trapped in the meritocratic narrative despite encountering repeated falsifying evidence, while others exit psychologically?',
    'Narrative analysis of survivor accounts. Psychological research on cognitive dissonance management and sunk-cost commitment. Longitudinal tracking of belief persistence vs updating.',
    'If internalization is fragile: snare may weaken rapidly as cohort reaches peak dissonance (ages 35-45). If internalization is robust: snare persists through psychological commitment independent of material outcomes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalization_mechanism_breakdown, conceptual, 'Psychological mechanisms sustaining belief in meritocratic narrative despite falsification').

omega_variable(
    credential_arms_race_equilibrium,
    'Is there a stable equilibrium in credential inflation, or is the system in a runaway escalation cycle where degrees become necessary but not sufficient?',
    'Historical analysis of credential requirements for entry-level jobs. Economic modeling of signaling equilibria under credential proliferation. Data on when employers began preferring degrees for roles that did not require them.',
    'If runaway escalation: piton classification is correct — the system maintains its theater despite knowing it fails. If equilibrium exists: institutional response to credential inflation is rational, changing classification toward rope (coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credential_arms_race_equilibrium, empirical, 'Whether credential inflation follows runaway escalation or stable equilibrium').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mirror_of_erised_expectation, 1990, 2010).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(erised_tr_t0, mirror_of_erised_expectation, theater_ratio, 0, 0.35).
narrative_ontology:measurement(erised_tr_t10, mirror_of_erised_expectation, theater_ratio, 10, 0.58).
narrative_ontology:measurement(erised_tr_t20, mirror_of_erised_expectation, theater_ratio, 20, 0.81).

% Extraction over time
narrative_ontology:measurement(erised_be_t0, mirror_of_erised_expectation, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(erised_be_t10, mirror_of_erised_expectation, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(erised_be_t20, mirror_of_erised_expectation, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mirror_of_erised_expectation, information_standard).
narrative_ontology:affects_constraint(mirror_of_erised_expectation, credential_inflation_arms_race).
narrative_ontology:affects_constraint(mirror_of_erised_expectation, unpaid_internship_ecosystem).
narrative_ontology:affects_constraint(mirror_of_erised_expectation, college_debt_burden).

% DUAL FORMULATION NOTE:
% The Erised expectation is decomposed from the broader 'meritocratic fallacy' into a specific psychological-economic snare targeting Millennials via narrative internalization. Related constraints include credential inflation (the institutional response to meritocratic belief), unpaid internship systems (the labor mechanism of extraction), and college debt (the financial mechanism). The Erised constraint focuses on the narrative internalization and expectation gap; downstream constraints model the systemic mechanisms through which that expectation is operationalized and extracted.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(mirror_of_erised_expectation, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
