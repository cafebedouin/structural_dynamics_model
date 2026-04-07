% ============================================================================
% CONSTRAINT STORY: interdisciplinary_synthesis_barriers
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_interdisciplinary_synthesis_barriers, []).

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
 *   constraint_id: interdisciplinary_synthesis_barriers
 *   human_readable: Interdisciplinary Synthesis Barriers in Knowledge Integration
 *   domain: epistemology/institutional/academic
 *
 * SUMMARY:
 *   Interdisciplinary synthesis barriers represent a structural constraint
 *   that has intensified over the past century as academic disciplines have
 *   become institutionally entrenched. The constraint operates through
 *   multiple mechanisms: career incentives reward specialization over
 *   integration, publishing infrastructure is calibrated to disciplinary
 *   depth, funding mechanisms require disciplinary framing, and credential
 *   systems gate entry into professional communities based on disciplinary
 *   identity. What originated as a rational coordination mechanism —
 *   organizing knowledge into manageable domains for deep study — has become
 *   an extractive mechanism that suppresses integrative capability and
 *   penalizes researchers attempting to work across boundaries. The
 *   constraint exhibits all six DR types depending on the observer's
 *   structural position. The barrier appears as pure extraction to
 *   integrative researchers (Snare), as legitimate coordination to
 *   disciplinary gatekeepers (Rope), as mixed extraction and coordination to
 *   cross-disciplinary centers (Tangled Rope), as degraded theater to
 *   credentialing systems (Piton), and as an emerging sunset structure to
 *   open science platforms building alternative pathways (Scaffold). The
 *   analytical observer risks naturalizing this as an immutable feature of
 *   knowledge itself (Mountain) rather than recognizing it as a contingent
 *   institutional choice that intensified during the 20th-century
 *   professionalization of academia.
 *
 * KEY AGENTS:
 *   - Integrative Researchers: Primary victim (powerless/trapped) — face career penalties, publishing rejection, and no professional home when working across disciplines
 *   - Field Epistemic Integrity: Primary victim (powerless/trapped) — abstract collective good of integrated knowledge; no mechanism for organization or exit
 *   - Disciplinary Gatekeepers: Primary beneficiary (institutional/arbitrage) — maintain gatekeeper status, career prestige, and institutional power through boundary maintenance
 *   - Disciplinary Publishing Infrastructure: Primary beneficiary (institutional/arbitrage) — specialized journals, citation indices, expert networks generate value through silo maintenance
 *   - Cross-Disciplinary Research Centers: Secondary actor (organized/constrained) — genuinely coordinate across disciplines but depend on institutional funding and legitimacy from parent disciplines
 *   - Open Science Platforms: Emergent alternative (organized/constrained) — build synthesis pathways (preprints, repositories, collaborative tools) that bypass traditional gatekeeping
 *   - Credential Systems: Institutional inertia (institutional/arbitrage) — PhD programs and professional certifications maintain boundaries through performative theater
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing institutional contingency as cognitive necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(interdisciplinary_synthesis_barriers, 0.52).
domain_priors:suppression_score(interdisciplinary_synthesis_barriers, 0.58).
domain_priors:theater_ratio(interdisciplinary_synthesis_barriers, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(interdisciplinary_synthesis_barriers, extractiveness, 0.52).
narrative_ontology:constraint_metric(interdisciplinary_synthesis_barriers, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(interdisciplinary_synthesis_barriers, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(interdisciplinary_synthesis_barriers, tangled_rope).
narrative_ontology:human_readable(interdisciplinary_synthesis_barriers, "Interdisciplinary Synthesis Barriers in Knowledge Integration").
narrative_ontology:topic_domain(interdisciplinary_synthesis_barriers, "epistemology/institutional/academic").

domain_priors:requires_active_enforcement(interdisciplinary_synthesis_barriers).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(interdisciplinary_synthesis_barriers, disciplinary_gatekeepers).
narrative_ontology:constraint_beneficiary(interdisciplinary_synthesis_barriers, specialized_publishing_infrastructure).
narrative_ontology:constraint_victim(interdisciplinary_synthesis_barriers, integrative_researchers).
narrative_ontology:constraint_victim(interdisciplinary_synthesis_barriers, field_epistemic_integrity).
narrative_ontology:constraint_victim(interdisciplinary_synthesis_barriers, knowledge_accessibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INTEGRATIVE RESEARCHER (SNARE) — Career trapped within disciplinary silos. Publishing venues, tenure standards, and funding mechanisms are all calibrated to disciplinary depth. An integrative scholar faces rejection from both parent disciplines, career penalty for 'lack of focus,' and no professional home. Suppression is maximal: the structural barriers (journal gatekeeping, citation metrics, tenure committees) are nearly insurmountable. No exit option without career abandonment.
constraint_indexing:constraint_classification(interdisciplinary_synthesis_barriers, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: FIELD EPISTEMIC INTEGRITY (SNARE) — The abstract collective good of integrated knowledge is systematically suppressed. Complex problems requiring synthesis (climate change, pandemic response, social-technological systems) receive fragmented, discipline-bounded analysis. The field-level cognitive capability for synthesis atrophies. No mechanism to organize or exit; full cost borne by epistemic commons.
constraint_indexing:constraint_classification(interdisciplinary_synthesis_barriers, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: CROSS-DISCIPLINARY RESEARCH CENTERS (TANGLED ROPE) — Genuinely solve coordination problems: researchers from different disciplines find common language, shared methods, collaborative infrastructure. But extraction occurs: centers must justify themselves through traditional disciplinary metrics to survive; they extract legitimacy from their member disciplines while remaining marginal to each. Constrained exit — depends on institutional funding and parent discipline acceptance.
constraint_indexing:constraint_classification(interdisciplinary_synthesis_barriers, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: DISCIPLINARY PUBLISHING INFRASTRUCTURE (ROPE) — Benefits from silo maintenance. Specialized journals, expert peer review systems, citation indices calibrated to disciplinary depth — all generate value for publishers and gatekeepers. Low extraction experienced; the system is perceived as legitimate coordination: organizing knowledge within disciplines. Net beneficiary with high arbitrage capacity (can pivot to new specialties, acquire related fields, etc.).
constraint_indexing:constraint_classification(interdisciplinary_synthesis_barriers, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: DISCIPLINARY GATEKEEPERS (ROPE) — Department chairs, journal editors, funding agencies calibrated to disciplinary identity experience the silo structure as legitimate coordination: it organizes expertise, ensures rigor within domains, and maintains professional identity. They benefit from gatekeeper status and have high arbitrage capacity — can enforce or relax disciplinary boundaries depending on institutional strategy.
constraint_indexing:constraint_classification(interdisciplinary_synthesis_barriers, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: DISCIPLINARY CREDENTIAL SYSTEMS (PITON) — PhD programs, degree structures, and professional certifications maintain disciplinary boundaries through institutional inertia. The theater ratio is high: credentials perform legitimacy but increasingly fail at predicting capability for complex problem-solving. The system persists because alternatives haven't fully replaced it and because institutions have invested heavily in the structure. Theatrical maintenance of boundaries that have lost functional necessity.
constraint_indexing:constraint_classification(interdisciplinary_synthesis_barriers, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: OPEN SCIENCE / DIGITAL INFRASTRUCTURE (SCAFFOLD) — Preprints, open repositories, computational tools, and collaborative platforms are creating alternative synthesis pathways that bypass traditional gatekeeping. ArXiv, GitHub, Jupyter notebooks, and federated databases enable cross-disciplinary discovery and collaboration without permission from disciplinary gatekeepers. This is a temporary support structure with sunset logic: as digital infrastructure matures and enables fluid knowledge exchange, the traditional disciplinary barriers lose force. Estimated sunset: 15-25 years for norms to mature.
constraint_indexing:constraint_classification(interdisciplinary_synthesis_barriers, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some degree of specialization is inherent to knowledge advancement: deep expertise requires focused study, and specialization is cognitively efficient. This perspective sees disciplinary boundaries as natural law — emergent from the structure of complex knowledge itself. However, this naturalizes what is actually a contingent institutional choice: boundaries could be permeable without sacrificing depth, and historical data shows synthesis was more common before 20th-century institutionalization of disciplines.
constraint_indexing:constraint_classification(interdisciplinary_synthesis_barriers, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(interdisciplinary_synthesis_barriers_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(interdisciplinary_synthesis_barriers, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(interdisciplinary_synthesis_barriers, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(interdisciplinary_synthesis_barriers, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(interdisciplinary_synthesis_barriers, TR),
    TR >= 0.70.

:- end_tests(interdisciplinary_synthesis_barriers_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts from integrative researchers through career penalties, but the extraction is not total — some cross-disciplinary work succeeds, and open-science alternatives are reducing barriers. The value reflects that the extraction is substantial but not inevitable; alternative pathways exist. The trend over 40 years shows extraction accelerating (from 0.25 to 0.52) as disciplinary institutionalization has deepened and performance metrics have become more granular. Suppression (0.58): Moderate-high. Barriers are significant but not absolute. Career risk, publishing gatekeeping, and funding requirements create substantial suppression; however, some researchers can work around them through institutional positioning, alternative venues, or individual prestige. Suppression is structural but has some permeability. Theater ratio (0.65): Moderate-high. Credential systems, peer review rituals, and departmental structures perform legitimacy and maintain boundaries, but increasingly fail to predict actual capability for complex problem-solving. The theater has increased as the functional necessity of boundaries has decreased but institutions have invested more heavily in performative maintenance. The constraint's theater ratio reflects the growing gap between what the credential system claims to do (identify deep expertise) and what it actually does (maintain disciplinary turf).
 *
 * PERSPECTIVAL GAP:
 *   The gap between gatekeeper and integrative researcher perspectives is maximal: one sees coordination (Rope), the other sees extraction (Snare). This reveals the constraint's mechanism: what the beneficiary experiences as legitimate organization (Rope) is exactly what the victim experiences as suppression (Snare). The perspectival gap is not a matter of disagreement about facts — both sides agree on the mechanisms (publishing gatekeeping, career incentives, credential requirements) — but about whether these mechanisms serve coordination or extraction. The gatekeeper believes they serve quality control and expertise organization; the integrative researcher experiences them as arbitrary suppression. This gap is diagnostic: when a single mechanism produces such opposite classifications from different perspectives, the constraint is likely a Tangled Rope or Snare depending on the strength of any genuine coordination function. The cross-disciplinary center perspective (Tangled Rope) bridges this gap by acknowledging both the real coordination function (enabling collaboration) and the real extraction (requiring legitimacy from parent disciplines).
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation chains from beneficiary/victim declarations plus exit options. Gatekeepers and publishers are declared beneficiaries with arbitrage exit — they derive d ≈ 0.10-0.15, producing negative f(d) values, so they experience chi approaching zero or negative (no extraction), consistent with their Rope classification. Integrative researchers are victims with trapped exit — d ≈ 0.95, producing f(d) ≈ 1.42, so they experience high chi, consistent with their Snare classification. Cross-disciplinary centers are mixed (coordinate across disciplines, but constrained by institutional dependencies) — they are both beneficiaries (via collaboration benefits) and victims (via boundary suppression), with constrained exit, producing d ≈ 0.50 and f(d) ≈ 0.65, yielding moderate chi and Tangled Rope classification. Field epistemic integrity is a victim with no exit — d ≈ 0.95, producing maximum f(d), consistent with Snare classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint does not resolve to a single type; instead, it demonstrates that the mandatrophy question ('is this coordination or extraction?') has a genuine answer: IT IS BOTH. The constraint coordinates expertise within disciplines (genuine function, experienced as Rope by beneficiaries) while simultaneously suppressing integrative synthesis (extraction function, experienced as Snare by victims). This is precisely what Tangled Rope is designed to capture: a constraint with both a genuine coordination function AND asymmetric extraction, with active enforcement required to maintain the extraction against natural drift toward pure coordination. The constraint's classification as Tangled Rope at the analytical level (with beneficiaries, victims, and required enforcement all declared) resolves the mandatrophy by showing that both readings are structurally correct — they are correct for different agents with different structural relationships. The false mountain classification at the civilizational/analytical perspective reveals that naturalizing the constraint as 'necessary specialization' is exactly the cover story that prevents seeing the asymmetric extraction. The detection of this false summit via accessibility_collapse and resistance metrics should flag the need for explicit consideration of whether the claimed natural law is actually a contingent institutional choice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    depth_vs_breadth_threshold,
    'What ratio of specialization to generalization maximizes cognitive capability for complex problem-solving?',
    'Empirical analysis of problem-solving success rates across cognitive profiles; comparison of specialized vs. integrative researcher contributions to major innovations',
    'If optimal ratio favors specialization: current barriers are efficient. If optimal ratio favors more integration: barriers represent significant cognitive loss.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(depth_vs_breadth_threshold, empirical, 'Optimal depth-to-breadth ratio for problem-solving capability').

omega_variable(
    gatekeeping_necessity,
    'Is disciplinary gatekeeping necessary for quality control and rigor, or does it primarily serve to maintain institutional power?',
    'Comparison of error rates, reproducibility, and impact across peer-reviewed disciplinary work vs. open-synthesis work; analysis of correlation between gatekeeper status and actual expertise vs. institutional position',
    'If necessary for rigor: barriers serve genuine coordination function (Rope). If primarily institutional: barriers are pure extraction (Snare/Piton).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(gatekeeping_necessity, empirical, 'Whether disciplinary gatekeeping provides necessary quality control').

omega_variable(
    alternative_credential_viability,
    'Can non-traditional pathways (open portfolios, demonstrated capability, collaborative credentials) effectively replace disciplinary degree requirements?',
    'Longitudinal tracking of non-traditionally-credentialed researchers in academia and industry; comparison of career outcomes, research impact, and problem-solving capability vs. traditionally-credentialed peers',
    'If viable: scaffold sunset is accelerating. If not viable: alternatives cannot yet replace traditional credentials.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_credential_viability, empirical, 'Viability of alternative credentialing systems').

omega_variable(
    cognitive_vs_institutional_barriers,
    'Are synthesis barriers primarily cognitive (limits of human knowledge integration) or institutional (career incentives, publishing structures, funding mechanisms)?',
    'Analysis of synthesis rates across different institutional contexts (open science platforms vs. traditional academia, crisis-driven research vs. peacetime research, pre-disciplinarization vs. post-disciplinarization historical comparison)',
    'If primarily cognitive: barriers are largely unchangeable (Mountain-like). If primarily institutional: barriers are contingent and removable (Snare/Tangled Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognitive_vs_institutional_barriers, conceptual, 'Cognitive vs. institutional sources of synthesis barriers').

omega_variable(
    synthesis_failure_cost_measurement,
    'What is the measurable cost of synthesis barriers in terms of delayed or missed insights on major societal problems?',
    'Case studies of major innovations requiring synthesis (immunology-computer science in vaccine design, geology-physics in climate modeling); counterfactual analysis of how much faster these could have progressed with fewer barriers',
    'If costs are high: extraction severity is underestimated. If costs are low: barriers may serve necessary function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(synthesis_failure_cost_measurement, empirical, 'Measurable cost of synthesis barriers to societal problem-solving').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(interdisciplinary_synthesis_barriers, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(idsb_tr_t0, interdisciplinary_synthesis_barriers, theater_ratio, 0, 0.45).
narrative_ontology:measurement(idsb_tr_t20, interdisciplinary_synthesis_barriers, theater_ratio, 20, 0.58).
narrative_ontology:measurement(idsb_tr_t40, interdisciplinary_synthesis_barriers, theater_ratio, 40, 0.65).

% Extraction over time
narrative_ontology:measurement(idsb_be_t0, interdisciplinary_synthesis_barriers, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(idsb_be_t20, interdisciplinary_synthesis_barriers, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(idsb_be_t40, interdisciplinary_synthesis_barriers, base_extractiveness, 40, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(interdisciplinary_synthesis_barriers, identity_coordination).
narrative_ontology:affects_constraint(interdisciplinary_synthesis_barriers, knowledge_fragmentation).
narrative_ontology:affects_constraint(interdisciplinary_synthesis_barriers, specialization_lock_in).

% DUAL FORMULATION NOTE:
% Interdisciplinary synthesis barriers are downstream of disciplinary institutionalization but represent a distinct structural constraint. Related upstream constraints include disciplinary credential systems, academic publishing economics, and career incentive structures; downstream constraints include innovation velocity reduction, complex problem-solving capability atrophy, and knowledge accessibility barriers. All members of this constraint family should be linked through network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(interdisciplinary_synthesis_barriers, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
