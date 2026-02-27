% ============================================================================
% CONSTRAINT STORY: medical_residency_match
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_medical_residency_match, []).

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
 *   constraint_id: medical_residency_match
 *   human_readable: The NRMP Medical Residency Match
 *   domain: economic/social
 *
 * SUMMARY:
 *   The NRMP Medical Residency Match is a centralized clearinghouse that
 *   pairs 42,000+ medical students annually with residency positions using a
 *   stable matching algorithm (Roth-Peranson). While presented as a neutral
 *   coordination mechanism that solved the chaos of pre-1990s bilateral
 *   negotiation, the Match has become an extraction mechanism that suppresses
 *   student information, enforces participation, and allows program directors
 *   to coordinate specialty distribution and geographic concentration. The
 *   constraint is a textbook hybrid: it solves the collective action problem
 *   of pairing thousands of students and programs (coordination function),
 *   but simultaneously extracts career control from students, suppresses
 *   information symmetries, and discriminates against underrepresented
 *   minorities and international medical graduates. The rising theater ratio
 *   (0.42→0.58 over 30 years) reflects increasing reliance on performative
 *   legitimacy ('algorithm is fair and neutral') as underlying extraction
 *   mechanisms become more visible. The constraint exhibits all six DR types
 *   from different perspectives: a Snare for students (trapped, no exit, no
 *   information), a Tangled Rope for programs (coordinated while extracting),
 *   a Rope for NRMP (pure coordination from its perspective), a Piton for the
 *   legacy ritual (maintained through inertia despite alternatives), and a
 *   risky false Mountain from the analytical observer who naturalizes
 *   matching difficulty.
 *
 * KEY AGENTS:
 *   - Medical Students: Primary victims (powerless/trapped) — mandatory participation, career depends on single algorithm run, no information symmetry, no exit option except abandoning residency pathway
 *   - Underrepresented Minority Students: Heightened victims (powerless/trapped) — systematic bias in interviews and program rankings, geographic clustering disadvantage, tacit mentorship gaps, implicit bias in preference signals
 *   - International Medical Graduates: Heightened victims (powerless/trapped) — explicit discrimination, non-standardized credential evaluation, visa sponsorship as selection mechanism, lower match rates despite equivalent qualifications
 *   - Program Directors: Organized beneficiaries (organized/constrained) — collective action coordinated through Match, extract labor-supply control, can suppress salary competition, can sort by geography
 *   - NRMP Organization: Institutional beneficiary (institutional/arbitrage) — monopoly on clearing mechanism, data lock and informational asymmetry, benefits from mandatory participation, maintains algorithm opacity
 *   - Academic Medicine Leadership: Meta-institutional actor (institutional/constrained) — regulatory capture by program directors, constrained to defend Match publicly while knowing limitations, incentivized to minimize equity scandals
 *   - Analytical Observer: Civilization perspective (analytical/analytical) — risks naturalizing contingent institutional arrangement (matching difficulty) as immutable feature of labor markets
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(medical_residency_match, 0.52).
domain_priors:suppression_score(medical_residency_match, 0.68).
domain_priors:theater_ratio(medical_residency_match, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(medical_residency_match, extractiveness, 0.52).
narrative_ontology:constraint_metric(medical_residency_match, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(medical_residency_match, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(medical_residency_match, tangled_rope).
narrative_ontology:human_readable(medical_residency_match, "The NRMP Medical Residency Match").
narrative_ontology:topic_domain(medical_residency_match, "economic/social").

domain_priors:requires_active_enforcement(medical_residency_match).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(medical_residency_match, program_directors).
narrative_ontology:constraint_beneficiary(medical_residency_match, nrmp_organization).
narrative_ontology:constraint_victim(medical_residency_match, medical_students).
narrative_ontology:constraint_victim(medical_residency_match, underrepresented_minority_students).
narrative_ontology:constraint_victim(medical_residency_match, international_medical_graduates).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MEDICAL STUDENT (SNARE) — Structurally trapped. Participation is mandatory (withdrawal from Match ends residency pathway). No information symmetry: program preferences opaque, ranking algorithm black-boxed, students cannot observe other students' preferences or outcomes. Career hinges on single algorithm run. d≈0.92, f(d)≈1.40, σ=1.0 → χ≈0.73. High extraction with suppression of exit and alternatives.
constraint_indexing:constraint_classification(medical_residency_match, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: UNDERREPRESENTED MINORITY STUDENT (SNARE) — More constrained than majority-background students. Implicit bias in interviews and ranking, geographic clustering disadvantages, limited mentorship networks, implicit bias in program selection processes. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.74. Maximum extraction with visibility suppression.
constraint_indexing:constraint_classification(medical_residency_match, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: INTERNATIONAL MEDICAL GRADUATE (SNARE) — Explicit structural discrimination: most programs screen out IMG applicants; credential evaluation non-standardized; visa sponsorship as selection mechanism; lower match rates despite equivalent qualifications. d≈0.96, f(d)≈1.42, σ=1.0 → χ≈0.74. Maximum extraction with explicit legal/administrative suppression of alternatives.
constraint_indexing:constraint_classification(medical_residency_match, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 4: PROGRAM DIRECTOR COALITION (TANGLED ROPE) — Organized agents with collective action coordination function (stable matching solves the problem of bilateral negotiation and strategic ranking). But programs extract significant labor-supply control: can set specialty distribution, geographic concentration, geographic location as sorting mechanism for candidate selection. Can use Match to suppress salary competition. d≈0.35, f(d)≈0.28, σ=1.0 → χ≈0.15. Low effective extraction because organized agents have agency within the constraint.
constraint_indexing:constraint_classification(medical_residency_match, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: NRMP ORGANIZATION (ROPE) — Benefits from monopoly on clearing mechanism; maintains data lock (non-transparency around algorithm, ranking outcomes, program preferences). But functions primarily as coordination mechanism: solves the massive bilateral matching problem that would otherwise require individual negotiation. Extractiveness comes from informational asymmetry and enforced participation, not from active extraction of value. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.05. Net beneficiary through institutional arbitrage.
constraint_indexing:constraint_classification(medical_residency_match, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: LEGACY COORDINATION RITUAL (PITON) — The Match persists through institutional inertia despite technical alternatives existing (blockchain-based matching, direct negotiation platforms, decentralized preference-collection). Theater_ratio=0.58 reflects that the 'secrecy' and 'algorithm' create performative legitimacy—the algorithm is stable matching (transparent, well-understood), but presented as complex/neutral/fair. Regulatory capture of NRMP by program directors maintains status quo despite known inefficiencies. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.38. Intermediate—the constraint persists but is increasingly theatrical.
constraint_indexing:constraint_classification(medical_residency_match, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (FALSE SUMMIT RISK) — From a civilizational perspective, some matching problem is inherent to labor markets with bilateral constraints. The Roth-Peranson algorithm is mathematically elegant and solves the stability/rationality problem. Risk: naturalizing NRMP's current monopoly and information asymmetries as inevitable features of matching problems. Engine false summit detector: ε=0.52 and suppression=0.68 contradict mountain classification. The constraint is contingent, not natural law.
constraint_indexing:constraint_classification(medical_residency_match, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(medical_residency_match_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(medical_residency_match, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(medical_residency_match, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(medical_residency_match, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(medical_residency_match, TR),
    TR >= 0.70.

:- end_tests(medical_residency_match_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The Match extracts significant value from students through information asymmetry, participation coercion, and outcome control. But the extraction is bounded: stable matching genuinely solves a coordination problem, so students do receive legitimate value (predictable outcomes, reduced bilateral negotiation costs). The 0.52 reflects the hybrid nature—not pure extraction (which would be ~0.70+), but more than pure coordination (0.05-0.20). Suppression (0.68): High. Multiple layers: (1) Algorithm opacity—the Roth-Peranson algorithm's computational details are intentionally withheld; (2) Preference confidentiality—students cannot observe program rankings, programs cannot observe student rankings, even retrospectively; (3) Outcome suppression—detailed matching data is restricted; (4) Structural discrimination—IMG exclusion is embedded in program screening, URM bias is embedded in interview and ranking processes, both difficult to audit; (5) Exit suppression—no alternative clearing mechanism exists, withdrawal from Match ends residency pathway. Theater ratio (0.58): Moderate-high. The 'algorithm' is presented as complex, neutral, and fair, creating performative legitimacy. The algorithm is actually transparent mathematics (stable matching theory), but the NRMP's presentation obscures this. Rising theater (0.42→0.58) reflects increasing performative effort as equity concerns have surfaced. The 'fairness' claim becomes more theatrical as evidence of systematic bias accumulates.
 *
 * PERSPECTIVAL GAP:
 *   The largest perspectival gap is between students and programs. For students, the Match is a Snare: they are trapped, have no information, and bear the full asymmetry. For programs, it is a Tangled Rope: they coordinate (solving the bilateral negotiation problem) while extracting (setting specialty distribution, sorting by geography, implicitly discriminating). For NRMP, it is a Rope: pure coordination problem solved, no meaningful victim group at NRMP's own level. The piton perspective reveals the constraint's inertial character—the Match persists not because it is optimal (alternatives exist), but because institutional stakeholders (program directors, medical school deans) benefit from the status quo and have captured regulatory mechanisms. The false mountain perspective warns against naturalizing the matching problem—bilateral allocation problems are real, but NRMP's specific implementation (opacity, suppression, discrimination) is contingent policy, not law of nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Medical students: Victims + trapped → d≈0.92, f(d)≈1.40. Maximum extraction trajectory. URM/IMG students: Victims + trapped + discrimination → d≈0.95, f(d)≈1.42. Maximum extraction with visibility amplification. Program directors: Beneficiaries + constrained (within Match while coordinated) → d≈0.35, f(d)≈0.28. Organized agents experience lower effective extraction because they have agency within the constraint's rules and coordinate collectively. NRMP: Institutional beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary—the constraint subsidizes NRMP's institutional position. Academic medicine leadership: Institutional + constrained (must defend Match publicly) → d≈0.50, f(d)≈0.65. Symmetric position—benefits from program coordination, constrained to not publicly critique system that produces their faculty.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION PATH: The mandatrophy is resolved by recognizing that Tangled Rope is the correct classification at the ensemble level. The constraint exhibits both coordination function (bilateral matching problem is real and solved) and asymmetric extraction (student suppression, discrimination, information opacity). The mandatrophy question 'Is this coordination or extraction?' has the answer: it is both simultaneously, and that duality is the structural definition of Tangled Rope. The false mountain perspective is correctly flagged as a false summit: the analytical observer's claim that 'matching problems are inherent to labor markets' is true, but NRMP's current implementation (opacity, suppression, discrimination, regulatory capture) is not inherent—it is contingent policy that could be reformed (transparency, portability, anti-discrimination mechanisms). The constraint does not degrade to pure Snare because programs do benefit from coordination, and students do benefit from stability relative to bilateral chaos. But the constraint is not a pure Rope because student suppression is real, discrimination is real, and NRMP extracts monopoly rents. Tangled Rope is the only classification that captures this duality without false naturalizing or false reduction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bias_measurement_basis,
    'How much of the specialty/geographic mismatch for URM and IMG students is due to Match algorithm constraints vs. upstream application/interview discrimination?',
    'Decompose outcomes: (1) Program director ranking patterns controlling for student qualifications; (2) student list-building patterns by demographic; (3) interview invitation rates by program type; (4) Match algorithm sensitivity analysis removing confidentiality constraints',
    'If upstream discrimination > 70%: Match is partially innocent, structural discrimination is earlier in pipeline. If Match mechanism > 50%: algorithm design or information asymmetry is primary lever. Classification may shift to pure snare (upstream) vs tangled rope (match-specific).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bias_measurement_basis, empirical, 'Attribution of student outcome disparities to Match algorithm vs upstream discrimination').

omega_variable(
    algorithm_transparency_counterfactual,
    'If Match algorithm, preference data, and ranking formulas were fully transparent, would observed market outcomes (specialty distribution, geographic clustering, IMG exclusion) persist or reverse?',
    'Controlled experiments with algorithm transparency in limited context; comparison with decentralized matching platforms (kidney exchange, school choice); student behavior modeling with full information',
    'If outcomes persist despite transparency: extraction is structural (program monopsony power), constraint is irreversible Snare. If outcomes reverse: extraction was primarily informational suppression, constraint is contingent Tangled Rope with sunset potential.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(algorithm_transparency_counterfactual, empirical, 'Whether transparency would alter Match outcomes').

omega_variable(
    alternative_clearing_mechanism_viability,
    'Could decentralized platforms (direct program-student negotiation, blockchain matching, auction-based allocation) achieve equivalent stability and efficiency without NRMP''s information suppression?',
    'Technical feasibility study of alternative mechanisms; pilot programs in non-binding early matching; comparison with international residency matching systems (Canada, Australia, UK)',
    'If viable alternatives exist: NRMP constraint is extractive monopoly (Snare from students, benefits NRMP/programs). If alternatives fail: NRMP monopoly provides genuine coordination service (Rope). Current sunset estimate (open-source algorithm, mandatory transparency) depends on this resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_clearing_mechanism_viability, empirical, 'Technical viability of alternative matching mechanisms').

omega_variable(
    program_preference_strategic_suppression,
    'To what extent do program directors suppress their true preferences in Match lists to extract negotiating power (via post-Match incentives, side deals, honorarium offers)?',
    'Survey program directors on preference discrepancy; analyze post-Match outcomes (unmatched positions, post-Match hiring, salary/incentive variation by match outcome); algorithmic audit of revealed vs stated preferences',
    'If strategic suppression > 30%: programs use Match as a theater mechanism while conducting actual allocation outside it (Piton classification strengthens). If <10%: programs use Match transparently (Rope classification strengthens).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(program_preference_strategic_suppression, empirical, 'Degree of strategic preference suppression by program directors').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(medical_residency_match, 1995, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nrmp_tr_t0, medical_residency_match, theater_ratio, 0, 0.42).
narrative_ontology:measurement(nrmp_tr_t15, medical_residency_match, theater_ratio, 15, 0.5).
narrative_ontology:measurement(nrmp_tr_t30, medical_residency_match, theater_ratio, 30, 0.58).

% Extraction over time
narrative_ontology:measurement(nrmp_be_t0, medical_residency_match, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(nrmp_be_t15, medical_residency_match, base_extractiveness, 15, 0.45).
narrative_ontology:measurement(nrmp_be_t30, medical_residency_match, base_extractiveness, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(medical_residency_match, resource_allocation).
narrative_ontology:affects_constraint(medical_residency_match, medical_education_debt_accumulation).
narrative_ontology:affects_constraint(medical_residency_match, specialty_distribution_imbalance).
narrative_ontology:affects_constraint(medical_residency_match, primary_care_workforce_shortage).

% DUAL FORMULATION NOTE:
% The NRMP Match is downstream of medical school admissions (which pre-sorts students by SES, undergraduate prestige, and demographic) and upstream of post-residency specialization decisions (which further extract from constrained graduates). The specialty distribution imbalance (too many proceduralists, too few generalists) is partially caused by Match-enabled program concentration. Primary care shortage is partially caused by Match-enabled specialty preference coordination by programs.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(medical_residency_match, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
