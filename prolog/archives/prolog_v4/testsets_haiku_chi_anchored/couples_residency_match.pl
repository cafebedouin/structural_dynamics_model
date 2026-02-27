% ============================================================================
% CONSTRAINT STORY: couples_residency_match
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_couples_residency_match, []).

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
 *   constraint_id: couples_residency_match
 *   human_readable: The Medical Residency Couples Match Algorithm
 *   domain: technological/economic
 *
 * SUMMARY:
 *   The National Resident Matching Program (NRMP) Couples Match is an
 *   algorithmic constraint that couples together two medical students' rank
 *   order lists, guaranteeing a match result where both members receive an
 *   acceptable position or both receive no match. Since its introduction in
 *   1987, the couples match has grown from a niche accommodation to a major
 *   constraint affecting approximately 15-20% of residency applicants
 *   annually. The constraint exhibits the full structural tension between
 *   coordination and extraction: it genuinely solves a real matching problem
 *   (how to place two physicians in the same geographic region), but it
 *   simultaneously extracts from coupled applicants by limiting their
 *   individual optimization space and from rural programs by reducing their
 *   applicant pools. The algorithm's extractiveness has grown over 15 years
 *   (from ε≈0.28 to ε≈0.38) as specialty concentration has increased, making
 *   geographic co-location increasingly costly for competitive specialties.
 *   The theater ratio (0.48) reflects that the algorithm's matching function
 *   is genuinely operational (not purely performative), but there is growing
 *   performative content in justifications ('we've always done it this way')
 *   rather than demonstrations of optimality relative to alternatives.
 *
 * KEY AGENTS:
 *   - Coupled Medical Applicants: Primary victims (powerless/trapped) — face algorithmic constraint on individual optimization; exit option (competing separately) causes geographic separation and career asymmetry
 *   - Program Directors (Competitive Urban Specialties): Primary beneficiaries (institutional/arbitrage) — benefit from predictable couples ranking patterns without extractive cost; experience constraint as coordination
 *   - Single Applicants: Secondary beneficiaries (institutional/arbitrage) — benefit from reduced match uncertainty and improved fill rates; net beneficiary status
 *   - Rural and Underserved Program Directors: Mixed actor (moderate/constrained) — benefit from algorithm's geographic clustering function but victimized by reduced rural applicant interest; witness reduced pipeline for rural workforce
 *   - NRMP and Medical Education System: Institutional enforcer (organized/constrained) — maintains couples match through active algorithm specification; experiences dual function (coordination + extraction enforcement)
 *   - Alternative Matching System Advocates: Organized agents (organized/constrained) — proposing constraint relaxation through flexible scheduling, geographic incentive support, and multi-stage protocols; see sunset pathway
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(couples_residency_match, 0.38).
domain_priors:suppression_score(couples_residency_match, 0.52).
domain_priors:theater_ratio(couples_residency_match, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(couples_residency_match, extractiveness, 0.38).
narrative_ontology:constraint_metric(couples_residency_match, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(couples_residency_match, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(couples_residency_match, tangled_rope).
narrative_ontology:human_readable(couples_residency_match, "The Medical Residency Couples Match Algorithm").
narrative_ontology:topic_domain(couples_residency_match, "technological/economic").

domain_priors:requires_active_enforcement(couples_residency_match).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(couples_residency_match, program_directors).
narrative_ontology:constraint_beneficiary(couples_residency_match, single_applicants).
narrative_ontology:constraint_beneficiary(couples_residency_match, specialty_concentrators).
narrative_ontology:constraint_victim(couples_residency_match, coupled_applicants).
narrative_ontology:constraint_victim(couples_residency_match, geographic_mobility).
narrative_ontology:constraint_victim(couples_residency_match, rural_program_recruitment).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COUPLED MEDICAL GRADUATES (SNARE) — Medical couples face an impossible algorithmic constraint: the couples match guarantees no match if the algorithm cannot find acceptable positions for both simultaneously, forcing acceptance of suboptimal outcomes or geographic separation. Exit options are fully trapped — declining the couples match means competing as individuals with high probability of geographic separation. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.52. The extraction is severe because it is dressed as 'choice' when the alternative (competing separately) is career-devastating.
constraint_indexing:constraint_classification(couples_residency_match, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: RURAL/UNDERSERVED PROGRAM DIRECTORS (TANGLED ROPE) — These directors benefit from the couples match as a tool to coordinate geographic clustering (retention of both residents in a region). But they are also victimized: coupled applicants are less likely to rank rural positions (citing family/relationship concerns), reducing the rural applicant pool. d≈0.58, f(d)≈0.72, σ=0.9 → χ≈0.30. Mixed coordination (couples algorithm helps match stability) and extraction (reduces rural recruitment effectiveness).
constraint_indexing:constraint_classification(couples_residency_match, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PROGRAM DIRECTORS IN COMPETITIVE SPECIALTIES (ROPE) — Urban programs in high-demand specialties (dermatology, orthopedic surgery, radiology) benefit structurally from the couples match without extractive cost. Coupled applicants tend to rank their specialty more favorably (accepting geographic constraint for career advancement). The algorithm functions as pure coordination: it predictably fills positions and reduces costly re-ranking cycles. d≈0.08, f(d)≈-0.11, σ=1.0 → χ≈-0.04. Net beneficiary; experiences the constraint as coordination.
constraint_indexing:constraint_classification(couples_residency_match, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: SINGLE APPLICANTS (ROPE) — The couples match improves the matching efficiency for single applicants by reducing the probability mass on unfilled positions and second-round scramble uncertainty. Single applicants see the couples algorithm as a coordinating mechanism that increases match certainty and reduces career risk. d≈0.15, f(d)≈0.02, σ=1.0 → χ≈0.008. Minimal extraction; primarily benefits from improved coordination.
constraint_indexing:constraint_classification(couples_residency_match, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: NRMP AND MEDICAL EDUCATION SYSTEM (TANGLED ROPE) — The couples match serves a genuine coordination function: stabilizing the match, reducing cycle complexity, and addressing the two-body problem in physician career placement. But it also extracts from the system by constraining geographic mobility, reducing rural workforce pipeline effectiveness, and creating decision asymmetries (couples optimize for their joint preference; system optimizes for efficiency). d≈0.45, f(d)≈0.48, σ=1.0 → χ≈0.18. Active enforcement (algorithm specification) and dual function (coordination + extraction) trigger tangled rope gate.
constraint_indexing:constraint_classification(couples_residency_match, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ALTERNATIVE MATCHING SYSTEMS (SCAFFOLD) — Proposed alternatives (two-stage matching, constraint relaxation protocols, geographic incentive structures) suggest a sunset clause: the current couples match is transitional. As flexible scheduling, remote work, and dual-career support expand, the rigid geographic constraint becomes less necessary. These alternatives reduce the algorithm's extractive force without eliminating its coordination benefit. d≈0.35, f(d)≈0.31, σ=1.0 → χ≈0.12. Theater ≤ 0.70 (functional matching remains core); sunset plausible within 15-20 years as institutional support for geographic flexibility increases.
constraint_indexing:constraint_classification(couples_residency_match, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (PITON/DEGRADED) — From a civilizational perspective, the couples match risks being characterized as an inherent feature of physician career matching ('the two-body problem is fundamental to medicine'). But the structural data reveals this as partially degraded: the constraint persists largely through institutional inertia in the NRMP design rather than genuine algorithmic necessity. Alternative matching protocols could reduce extraction while preserving coordination. Theater_ratio=0.48 (below piton gate), but the performative element is growing: the algorithm is increasingly justified by 'we've always done it this way' rather than by demonstrable superiority. The false summit risk is low here — the constraint classifies legitimately as tangled rope.
constraint_indexing:constraint_classification(couples_residency_match, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(couples_residency_match_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(couples_residency_match, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(couples_residency_match, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(couples_residency_match, TR),
    TR >= 0.70.

:- end_tests(couples_residency_match_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate and rising. The couples match imposes a genuine constraint on coupled applicants' preference optimization — they cannot independently rank all positions and must accept reduced choice space or geographic separation. However, the extraction is not extreme because the algorithm does solve a real problem: without coupling, many physicians would face the two-body problem without institutional support. The increasing trajectory (0.28→0.38) reflects specialty concentration: as dermatology, orthopedic surgery, and radiology have become increasingly popular and geographically concentrated in urban areas, the geographic constraint has become more costly for coupled applicants in these specialties. Suppression (0.52): Moderate-high. The primary suppression mechanism is informational asymmetry: coupled applicants face a complex algorithm with non-obvious constraints and rejection rules; most cannot optimize their couples' rank orders as well as they could if permitted to rank independently. Secondary suppression: the couple cannot collectively exit and re-enter later — once they match or fail, the decision is final for that cycle. Theater ratio (0.48): Moderate. The algorithm's core function (finding stable matches) is genuinely operational; the theater is lower than in many institutional constraints. However, the performative content is growing: justifications are increasingly historical ('we've always supported couples') rather than empirically comparative ('this design is superior to alternatives').
 *
 * PERSPECTIVAL GAP:
 *   The constraint generates sharp perspectival disagreement. Coupled applicants see a snare: an algorithm that limits their choices and forces suboptimal acceptance or separation. Program directors in competitive specialties see coordination: the couples match creates predictable preferences and reduces fill uncertainty. Single applicants see a benefit: the couples match's existence stabilizes the overall match by removing a category of strategic complications, leaving more stable slots for others. Rural directors see extraction: the couples match channels desirable applicants to competitive urban specialties, starving rural programs. The NRMP system sees tangled rope: the algorithm both solves the coupling problem and extracts from individual optimization. The analytical observer might see either a piton (historical inertia) or recognize that the algorithm legitimately solves a hard problem (tangled rope) without a clean alternative. The perspectival gap exists because the algorithm's benefits are concentrated (competitive programs, single applicants, efficient matching) while its costs are distributed (coupled applicants, rural recruitment, geographic immobility). Coupled applicants cannot organize effectively (they are dispersed, their identity is only salient in the match context, and they lack institutional power), which explains why the snare perspective is visible despite the rope perspectives' claims of coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   Coupled applicants: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction; no exit option except geographic separation or accepting suboptimal positions. Rural directors: Victim + constrained → d≈0.58, f(d)≈0.72. Significant extraction but not maximal; some rural programs successfully recruit coupled applicants through specialty demand or niche positioning. Program directors (competitive): Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Net beneficiary; low d reflects that they benefit from the algorithm without bearing its cost. Single applicants: Beneficiary + arbitrage → d≈0.15, f(d)≈0.02. Minor beneficiary; they experience the algorithm as reducing search space friction. NRMP system: Enforcer + constrained → d≈0.45, f(d)≈0.48. Mid-range directionality reflecting the dual coordination/extraction function; the institution must maintain enforcement (active constraint) while claiming benign coordination.
 *
 * MANDATROPHY ANALYSIS:
 *   The couples match resolves the mandatrophy by revealing that it is simultaneously genuine coordination (solves the two-body problem) and genuine extraction (limits coupled applicants' individual optimization). The tangled rope classification captures this: the constraint has 0.38 ≤ ε < 0.46 (below the snare threshold), active enforcement requirement, multiple beneficiaries (program directors, single applicants), and multiple victims (coupled applicants, rural recruitment). The constraint cannot be reclassified as pure rope because the extraction component is real and measurable (reduced specialty access for coupled applicants relative to their individual preference rankings). It cannot be reclassified as snare because the coordination function is also real (coupled applicants do receive matches that they accept; many appreciate the institutional support for the two-body problem). The mandatrophy is resolved by accepting that the algorithm successfully performs a hybrid function — but this does not make the extraction invisible or acceptable to its victims. The growth trajectory (ε: 0.28→0.38) reveals that the extraction component is not stable — as specialty concentration increases, the costs to coupled applicants increase while benefits to competitive programs remain constant. This suggests a policy-sensitive rather than law-like constraint, and points toward potential sunset if alternative support structures (flexible scheduling, dual-career programs, geographic incentives) can decouple the coordination benefit from the extraction cost.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    geographic_constraint_necessity,
    'Is the strict geographic co-location requirement (both members must match in the same city/region) a necessary feature of stable matching or a contingent institutional choice that could be relaxed through alternative support structures?',
    'Comparison with international residency systems (Canada, UK, Australia) that use different coupling mechanisms; analysis of whether loosened geographic constraints with enhanced cross-regional support (funded relocation assistance, flexible scheduling) would preserve match stability',
    'If necessary: couples match extraction is unavoidable; constraint should remain tangled rope. If contingent: alternative designs could reduce extraction to near-rope levels, revealing current design as suboptimal policy choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geographic_constraint_necessity, empirical, 'Whether strict geographic coupling is necessary or contingent').

omega_variable(
    rural_recruitment_causation,
    'Does the couples match algorithm directly cause rural program recruitment decline, or is rural decline driven by underlying specialty concentration trends independent of the matching mechanism?',
    'Longitudinal analysis of rural program ranking patterns pre- and post-couples match eligibility expansion; comparison of rural ranking rates between coupled and single applicants controlling for specialty preference; synthetic control analysis using programs with no couples-matched positions',
    'If direct causation confirmed: couples match is a significant contributing factor to healthcare workforce maldistribution; constraint warrants ''victim'' designation for rural programs. If independent: constraint is less culpable; rural decline must be addressed through specialty incentives separate from matching algorithm.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rural_recruitment_causation, empirical, 'Causal attribution of rural recruitment decline to couples match').

omega_variable(
    suboptimal_matching_frequency,
    'What fraction of coupled applicants accept positions that are individually suboptimal (lower specialty ranking, less desirable program) due to the couples match constraint, and how does this compare to the counterfactual outcome if they competed separately?',
    'Survey of matched couples asking about preference satisfaction and regret; analysis of de-ranked position acceptances in couples vs single cohorts; simulation of alternative matching outcomes under relaxed constraints',
    'If high suboptimality rate (>30%): extraction is severe and widespread; supports snare classification from couples'' perspective. If low (<10%): algorithm is genuinely solving the matching problem efficiently; supports rope classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suboptimal_matching_frequency, empirical, 'Frequency and magnitude of individually suboptimal matches due to couples constraint').

omega_variable(
    algorithmic_necessity_vs_design_choice,
    'Is the current couples match algorithm (with its specific rejection and rematching rules) the unique optimal solution to the two-body matching problem, or is it one of several viable designs with different extraction properties?',
    'Theoretical computer science analysis of alternative matching algorithms (constraint relaxation, weighted-preference aggregation, multi-stage protocols); comparison of stability, efficiency, and individual optimality metrics across designs',
    'If current algorithm is unique optimum: extraction properties are unavoidable; constraint is genuinely tangled rope (coordination necessity + extraction side effect). If alternatives exist: current algorithm choice reflects policy preference favoring certain groups; reveals institutional power in design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_necessity_vs_design_choice, conceptual, 'Whether current algorithm is uniquely optimal or one of several viable designs').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(couples_residency_match, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(crm_tr_t0, couples_residency_match, theater_ratio, 0, 0.35).
narrative_ontology:measurement(crm_tr_t7, couples_residency_match, theater_ratio, 7, 0.42).
narrative_ontology:measurement(crm_tr_t15, couples_residency_match, theater_ratio, 15, 0.48).

% Extraction over time
narrative_ontology:measurement(crm_be_t0, couples_residency_match, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(crm_be_t7, couples_residency_match, base_extractiveness, 7, 0.33).
narrative_ontology:measurement(crm_be_t15, couples_residency_match, base_extractiveness, 15, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(couples_residency_match, resource_allocation).
narrative_ontology:affects_constraint(couples_residency_match, healthcare_workforce_geographic_distribution).
narrative_ontology:affects_constraint(couples_residency_match, rural_physician_pipeline).
narrative_ontology:affects_constraint(couples_residency_match, specialty_concentration_inequality).

% DUAL FORMULATION NOTE:
% The couples match is downstream of the two-body problem in academic career placement generally but constitutes a distinct constraint because it represents an algorithmic choice rather than an inherent feature of dual-career family formation. The healthcare workforce distribution constraint (geographic maldistribution of physicians) is affected by the couples match because the algorithm concentrates coupled applicants in regions with competitive urban specialties, exacerbating rural workforce gaps.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(couples_residency_match, moderate, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
