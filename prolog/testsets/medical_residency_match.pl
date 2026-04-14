% ============================================================================
% CONSTRAINT STORY: medical_residency_match
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   The NRMP Medical Residency Match is a centralized clearinghouse
 *   established in 1952 to solve the decentralized matching problem: medical
 *   students and residency programs were engaging in chaotic bidding wars,
 *   with programs making offers earlier each year and students accepting
 *   prematurely. The Match introduced the Roth-Peranson algorithm (stable
 *   matching mechanism) to aggregate preferences and produce a stable
 *   allocation, eliminating bidding wars. For seven decades, this was
 *   presented as a pure coordination mechanism solving a collective action
 *   problem. However, the constraint exhibits structural extraction: the
 *   algorithm is optimized for student preferences (not program viability or
 *   specialty balance), cream-skimming concentrates applicants at
 *   high-prestige programs, and rural and primary care specialties face
 *   persistent applicant shortages. The Match's stabilizing function is real
 *   — decoupling would reintroduce bidding chaos — but the algorithm's
 *   preference weighting creates asymmetric extraction. Mid-tier students
 *   experience maximum extraction (trapped, no direct negotiation, wrong
 *   ranking strategy → unmatched). Rural programs experience consistent
 *   cream-skimming (cannot compete with prestige-ranked urban specialties).
 *   Top-tier academic medical centers experience pure coordination benefit.
 *   The theater ratio has risen from 0.35 to 0.58 as supplemental matching
 *   pathways (couples' matching, SOAP, informal agreements) have
 *   proliferated, indicating the centralized algorithm no longer fully
 *   governs the matching process. The Match persists through antitrust
 *   exemption and institutional inertia, not because it remains the optimal
 *   solution to the original coordination problem.
 *
 * KEY AGENTS:
 *   - Mid-Tier Medical Students: Primary victims (powerless/trapped) — cannot negotiate outside Match, ranking strategy determines outcomes, wrong strategy results in unmatched status
 *   - Rural and Primary Care Programs: Secondary victims (powerless/trapped) — suffer cream-skimming, cannot offer higher compensation or negotiate directly, structurally disadvantaged by algorithm design
 *   - Top-Tier Academic Medical Centers: Primary beneficiary (institutional/arbitrage) — algorithm delivers highest-ranked applicants without competitive bidding, maintains arbitrage option
 *   - Primary Care Advocacy Coalition: Organized victims (organized/constrained) — understand system extracts from specialty viability, pushing for rule changes but cannot exit the Match
 *   - NRMP Administrative Structure: Institutional enforcer (institutional/arbitrage) — maintains Match rules and antitrust exemption; maintains arbitrage option to modify or abandon system
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees both genuine coordination function and embedded asymmetric extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(medical_residency_match, 0.38).
domain_priors:suppression_score(medical_residency_match, 0.62).
domain_priors:theater_ratio(medical_residency_match, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(medical_residency_match, extractiveness, 0.38).
narrative_ontology:constraint_metric(medical_residency_match, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(medical_residency_match, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(medical_residency_match, tangled_rope).
narrative_ontology:human_readable(medical_residency_match, "The NRMP Medical Residency Match").
narrative_ontology:topic_domain(medical_residency_match, "economic/social").

domain_priors:requires_active_enforcement(medical_residency_match).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(medical_residency_match, top_tier_programs).
narrative_ontology:constraint_beneficiary(medical_residency_match, high_ranked_students).
narrative_ontology:constraint_victim(medical_residency_match, mid_tier_students).
narrative_ontology:constraint_victim(medical_residency_match, rural_programs).
narrative_ontology:constraint_victim(medical_residency_match, primary_care_specialties).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MID-TIER MEDICAL STUDENT (SNARE) — Trapped within the Match system with no exit option. Cannot negotiate directly with programs. Bears full cost of preference aggregation: ranking strategy becomes crucial, wrong strategy results in unmatched status (catastrophic outcome). High experienced extraction through forced participation and information asymmetry. Suppressed alternatives: decoupling, lateral negotiation, or independent matching would be illegal under NRMP rules.
constraint_indexing:constraint_classification(medical_residency_match, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: RURAL OR PRIMARY CARE PROGRAM (SNARE) — Structurally disadvantaged by algorithm: high-ranked students sort away into prestigious specialties regardless of program quality or mission fit. Cannot offer higher salaries or negotiate individually (Match rules prohibit). Trapped in a mechanism designed to optimize for student preferences, not program viability. Extraction: cream-skimming of applicants by prestige-ranked urban programs. Suppression: anti-negotiation rules prevent alternative matching.
constraint_indexing:constraint_classification(medical_residency_match, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: TOP-TIER ACADEMIC MEDICAL CENTER (ROPE) — Benefits from the Match: algorithm aggregates student preferences and delivers highest-ranked applicants without negotiation. Coordination benefit: Match solves the decentralized matching problem at scale. Can opt out if needed (maintains arbitrage option). Experiences constraint as a coordination mechanism rather than extraction — the Match delivers preferred stable matching outcomes.
constraint_indexing:constraint_classification(medical_residency_match, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PRIMARY CARE ADVOCACY COALITION (TANGLED ROPE) — Organized actors (AAFP, American College of Physicians, rural health organizations) pushing back on cream-skimming. Constrained: cannot exit the Match entirely (affects members' access to training slots) but can influence rule changes. Sees both coordination function (Match does solve the centralized allocation problem) and asymmetric extraction (algorithm optimized for student preferences, not specialty viability). Active enforcement required: coalition must lobby NRMP for rule changes, preferential ranking bonus, or weighted algorithm modifications.
constraint_indexing:constraint_classification(medical_residency_match, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: NRMP ADMINISTRATIVE STRUCTURE (PITON) — The Match persists as an institutional form despite degraded function. Original purpose (prevent bidding wars, reduce transaction costs for matching) is increasingly theater: informal agreements, couples' matching complexity, supplemental matching pathways all indicate the centralized algorithm is no longer adequate. Theater ratio high: extensive ritual around ranking, interview season, Match Day ceremony masks the reality that high-prestige programs get preferred applicants through other channels. Maintained through institutional inertia and antitrust exemption, not because the algorithm genuinely solves the allocation problem anymore.
constraint_indexing:constraint_classification(medical_residency_match, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From the analytical view, the Match is a hybrid mechanism: it provides genuine coordination benefit (solves the stable matching problem at scale, prevents chaotic bidding wars) while embedding asymmetric extraction (algorithm optimized for student preferences in prestige ranking, not for specialty workforce balance or geographic access). Active enforcement: Match rules are maintained through NRMP bylaws and antitrust exemption. Suppression: alternative matching mechanisms are legally blocked. Base extraction moderate (0.38) because the coordination function is real, but suppression is high (0.62) because alternatives are structurally blocked.
constraint_indexing:constraint_classification(medical_residency_match, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(medical_residency_match_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(medical_residency_match, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(medical_residency_match, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(medical_residency_match, TR),
    TR >= 0.70.

:- end_tests(medical_residency_match_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The Match does provide coordination benefit (prevents bidding wars, enables stable matching at scale), which reduces pure extraction. However, the algorithm's optimization for student preferences in prestige ranking creates systematic cream-skimming that extracts from mid-tier students (wrong ranking strategy → unmatched) and rural programs (consistent applicant shortages). The extraction is not maximal because the coordination function is real and beneficial. Suppression (0.62): High. Multiple suppression mechanisms: (1) antitrust exemption legally prevents decoupling, (2) NRMP bylaws prohibit direct negotiation between students and programs, (3) social convention stigmatizes outside-Match offers, (4) ranking strategy is opaque (no public feedback on standing), (5) couples' matching complexity increases entry barriers. Theater ratio (0.58): Moderate-high. The Match Day ceremony and ranking ritual are performative — actual allocation is increasingly determined by supplemental pathways (couples' matching, SOAP, informal pre-agreement). The growth of supplemental matching (time point 0 → 30) indicates the centralized algorithm's functional decline.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates how the same institutional mechanism can be perceived as pure coordination (by beneficiaries), pure extraction (by trapped agents), or hybrid (by organized actors and analytical observers). Top-tier programs see only coordination benefit. Mid-tier students see maximum extraction (wrong ranking → unmatched). Rural programs see chronic disadvantage. The primary care coalition sees both benefits (prevents bidding wars) and harms (cream-skimming). The NRMP sees the system as functional (despite supplemental pathways). The analytical observer sees a coordination mechanism with embedded extraction asymmetry. No single type captures all perspectives — the presheaf of classifications is the structural truth.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for each agent is derived from their structural position in the extraction flow. Top-tier programs are beneficiaries (d ≈ 0.05): receive preferred applicants without competitive cost. Mid-tier students are victims (d ≈ 0.85): trapped, high stakes, algorithm-determined outcomes, no negotiation capacity. Rural programs are victims (d ≈ 0.90): systematic disadvantage, cannot compete on prestige, no exit. The primary care coalition occupies middle ground (d ≈ 0.55): can lobby for rule changes (mobile exit) but cannot exit the Match entirely. The NRMP structure is an enforcer (d ≈ 0.10): maintains the system and benefits from its perpetuation through institutional authority. The analytical observer's directionality (d ≈ 0.72) reflects the structural complexity: the observer sees all perspectives and the full constraint structure, making them neither pure beneficiary nor pure victim.
 *
 * MANDATROPHY ANALYSIS:
 *   The Match resolves the mandatrophy by showing that the constraint is genuinely hybrid: it provides coordination benefit (solving the stable matching problem at scale, preventing bidding wars) while simultaneously embedding asymmetric extraction (algorithm optimization for student preferences, cream-skimming of mid-tier students and rural programs). The coordination function is not a cover story — it is real and valued by top-tier beneficiaries. The extraction is not purely coercive — it is mediated through algorithmic weighting, not brute force. Active enforcement (antitrust exemption, NRMP bylaws prohibiting negotiation) is required to maintain the system, confirming Tangled Rope classification. The rising theater ratio (0.35 → 0.58) indicates that supplemental matching pathways are proliferating, suggesting the centralized algorithm is becoming less functional and more theatrical over time. The system is not a Snare disguised as a Rope, nor a Rope capturing extraction rents — it is genuinely both, with the balance shifting toward increased theater and decreased centralized function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithm_optimization_target,
    'Is the Roth-Peranson algorithm optimized for student-proposer preferences or for system-wide welfare (e.g., specialty workforce balance, geographic access)?',
    'Algorithmic audit of optimization criteria; comparison of actual Match outcomes to Pareto-efficient allocations that would maximize primary care or rural placement rates',
    'If student-preference-optimized: extraction is embedded in the algorithm itself (Snare classification for mid-tier students justified). If welfare-optimized: extraction is pure suppression of alternatives (classification shifts toward Rope + regulatory constraint).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithm_optimization_target, empirical, 'Whether algorithm optimizes for student preferences or system welfare').

omega_variable(
    exit_alternative_legality,
    'Could decoupling from the Match system be legalized without antitrust liability, and what would be the stability consequences?',
    'Legal analysis of antitrust exemption scope; game-theoretic modeling of matching outcomes under alternative decoupled systems (Gale-Shapley vs current)',
    'If decoupling is legally feasible and stable: Match is pure suppression of alternatives (Snare from trapped agent perspective). If decoupling triggers race-to-the-bottom bidding wars: Match provides essential coordination (Rope from most perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_alternative_legality, conceptual, 'Whether decoupling from Match is legally and structurally viable').

omega_variable(
    cream_skimming_mechanism,
    'Does the Match mechanism cause cream-skimming by prestige-ranked programs, or does cream-skimming reflect pre-existing preference heterogeneity that any matching algorithm would produce?',
    'Counterfactual analysis: modeling Match outcomes under uniform student preferences; historical comparison to pre-Match era matching patterns; simulation of alternative weighting schemes',
    'If mechanism-induced: extraction is structural to the algorithm design (tangled rope with active enforcement required). If preference-driven: extraction is natural consequence of preference aggregation (classification shifts toward Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cream_skimming_mechanism, empirical, 'Whether cream-skimming is caused by Match mechanism or preference heterogeneity').

omega_variable(
    suppression_mechanism_enforcement,
    'What is the actual enforcement mechanism preventing students and programs from negotiating outside the Match, and how robust is it?',
    'Audit of NRMP enforcement actions; analysis of supplemental matching pathways (couples'' matching, SOAPing, independent agreement prevalence); interviews with compliance officers',
    'If enforcement is weak: suppression is low (constraint classifies as Rope). If enforcement is strong: suppression is high (Snare or Tangled Rope confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_enforcement, empirical, 'Robustness of NRMP enforcement against outside-Match negotiation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(medical_residency_match, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nrmp_tr_t0, medical_residency_match, theater_ratio, 0, 0.35).
narrative_ontology:measurement(nrmp_tr_t15, medical_residency_match, theater_ratio, 15, 0.48).
narrative_ontology:measurement(nrmp_tr_t30, medical_residency_match, theater_ratio, 30, 0.58).

% Extraction over time
narrative_ontology:measurement(nrmp_be_t0, medical_residency_match, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(nrmp_be_t15, medical_residency_match, base_extractiveness, 15, 0.3).
narrative_ontology:measurement(nrmp_be_t30, medical_residency_match, base_extractiveness, 30, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(medical_residency_match, resource_allocation).
narrative_ontology:affects_constraint(medical_residency_match, medical_specialty_workforce_distribution).
narrative_ontology:affects_constraint(medical_residency_match, geographic_healthcare_access_disparity).

% DUAL FORMULATION NOTE:
% The NRMP Match is upstream of specific specialty workforce distribution constraints. The specialty distribution outcomes (e.g., shortage of primary care doctors) are causally downstream of the Match mechanism's preference weighting. The Match itself has extractiveness 0.38 (hybrid coordination-extraction), while specialty distribution constraints have higher extractiveness reflecting the cascading effects of Match-driven cream-skimming.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(medical_residency_match, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
