% ============================================================================
% CONSTRAINT STORY: occupational_licensing_barriers
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_occupational_licensing_barriers, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: occupational_licensing_barriers
 *   human_readable: Occupational Licensing Barriers
 *   domain: economic/labor/regulatory
 *
 * SUMMARY:
 *   Occupational licensing is a system of government-enforced credential
 *   requirements that restrict entry into skilled occupations (plumbing,
 *   electrician, nursing, law, cosmetology, etc.). Justified as consumer
 *   protection and quality assurance, licensing simultaneously functions as a
 *   supply-constraint mechanism that raises incumbent wages and reduces labor
 *   mobility. The constraint exhibits classic hybrid extraction-coordination
 *   dynamics: genuine coordination value exists (quality standards, consumer
 *   liability protection, ethics enforcement), but layered asymmetrically
 *   with extraction mechanisms (high entry barriers, reciprocity fees,
 *   continuing education mandates of questionable efficacy). The theater
 *   ratio has increased over the interval as the number of licensed
 *   occupations has expanded and requirements have accumulated without
 *   evidence-based justification for consumer protection. Prospective workers
 *   face traps: sunk costs in education and apprenticeships create commitment
 *   that makes exit costly; regulatory barriers suppress supply-side
 *   competition, keeping wages artificially high for incumbents while
 *   reducing occupational mobility for entrants.
 *
 * KEY AGENTS:
 *   - Prospective Workers: Primary victims (powerless/trapped) — face financial, temporal, and regulatory barriers to entry; committed to a profession after sunk-cost education; cannot exit without losing investment
 *   - Low-Income Consumers: Secondary victims (moderate/constrained) — benefit from quality assurance but pay inflated prices and face reduced service availability in underserved markets
 *   - Incumbent Licensed Professionals: Primary beneficiaries (institutional/arbitrage) — capture supply-constraint rents, price elevation, and job security; experience licensing as pure coordination (quality signaling, market stability)
 *   - Regulatory Licensing Boards: Secondary beneficiaries (institutional/arbitrage) — derive legitimacy, staffing, and budgets from license administration; perceive role as public service, not as extraction
 *   - Professional Associations: Structural power actors (organized/arbitrage) — lobby for stricter requirements, higher fees, reciprocity barriers; effectively control licensing board agendas
 *   - Occupational Mobility Coalition: Organized challengers (organized/constrained) — consumer advocates, gig platforms, right-to-work orgs; see licensing as extractive; push for deregulation but face resource disadvantage vs. professional associations
 *   - Analytical Observer: Civilizational view (analytical/analytical) — assesses whether licensing requirements correlate with actual consumer protection outcomes or primarily serve incumbent rent-seeking
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(occupational_licensing_barriers, 0.52).
domain_priors:suppression_score(occupational_licensing_barriers, 0.68).
domain_priors:theater_ratio(occupational_licensing_barriers, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(occupational_licensing_barriers, extractiveness, 0.52).
narrative_ontology:constraint_metric(occupational_licensing_barriers, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(occupational_licensing_barriers, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(occupational_licensing_barriers, tangled_rope).
narrative_ontology:human_readable(occupational_licensing_barriers, "Occupational Licensing Barriers").
narrative_ontology:topic_domain(occupational_licensing_barriers, "economic/labor/regulatory").

domain_priors:requires_active_enforcement(occupational_licensing_barriers).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(occupational_licensing_barriers, incumbent_licensed_professionals).
narrative_ontology:constraint_beneficiary(occupational_licensing_barriers, regulatory_licensing_boards).
narrative_ontology:constraint_victim(occupational_licensing_barriers, prospective_workers).
narrative_ontology:constraint_victim(occupational_licensing_barriers, low_income_consumers).
narrative_ontology:constraint_victim(occupational_licensing_barriers, occupational_mobility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PROSPECTIVE WORKER (SNARE) — Faces steep financial, temporal, and informational barriers to entry. Licensing requirements (education, apprenticeships, exams, fees) can cost $30,000–$100,000+ and take 2–7 years. No exit option once committed to a field; trapped between sunk costs and license walls. Maximum extraction experienced — bears full cost of gatekeeping with minimal coordination benefit.
constraint_indexing:constraint_classification(occupational_licensing_barriers, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LOW-INCOME CONSUMER (TANGLED ROPE) — Benefits from genuine quality assurance and liability protections that licensing provides (consumer protection is real coordination). But constrained by artificially elevated prices and reduced service availability in underserved markets. The licensing system coordinates quality standards while extracting through reduced competition and access scarcity. Constrained exit: could seek unlicensed services (legal risk, quality risk) but supply is suppressed.
constraint_indexing:constraint_classification(occupational_licensing_barriers, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INCUMBENT LICENSED PROFESSIONAL (ROPE) — Net beneficiary. Licensing provides two coordination functions: (1) quality signaling (consumers trust the credential), (2) supply constraint (fewer competitors drive up prices and reduce labor surplus). Experiences the constraint as pure coordination — professional quality maintenance and market stability. Maximum arbitrage: can relocate to states with different standards, can lobby for stricter requirements.
constraint_indexing:constraint_classification(occupational_licensing_barriers, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REGULATORY LICENSING BOARD (ROPE) — Institutional beneficiary. Derives legitimacy, budget, and staffing from license administration. Experiences the constraint as pure coordination: setting standards, maintaining professional ethics, protecting public. Does not perceive extraction — the board's framing is that barriers serve quality assurance. High arbitrage: can adjust requirements, engage in regulatory competition with other states.
constraint_indexing:constraint_classification(occupational_licensing_barriers, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: OCCUPATIONAL MOBILITY COALITION (TANGLED ROPE) — Organized agents (consumer advocates, gig economy platforms, right-to-work organizations) see licensing as hybrid coordination + extraction. Real coordination value (quality standards) exists but is layered with rent-extraction (unnecessary requirements, inflated fees, reciprocity barriers across states). Constrained exit: can lobby for deregulation, but incumbent professional associations have superior resources and capture regulatory bodies.
constraint_indexing:constraint_classification(occupational_licensing_barriers, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: LICENSING SYSTEM AS THEATER (PITON) — From a long-term view, much licensing theater has become disconnected from actual quality metrics. Continuing education requirements, renewal fees, reciprocity exam barriers persist through institutional inertia rather than evidence-based quality maintenance. Theater ratio (0.58) reflects that a significant portion of licensing administration is performative ritual: ceremonies of professional legitimacy maintained because the system exists, not because the requirements correlate with consumer protection outcomes. The system maintains itself through stakeholder investment and regulatory entrenchment.
constraint_indexing:constraint_classification(occupational_licensing_barriers, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(occupational_licensing_barriers_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(occupational_licensing_barriers, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(occupational_licensing_barriers, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(occupational_licensing_barriers, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(occupational_licensing_barriers, TR),
    TR >= 0.70.

:- end_tests(occupational_licensing_barriers_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Licensing creates persistent wage premiums for incumbents (10–25% above comparable unlicensed occupations in some cases) while erecting barriers that suppress entry and occupational mobility. However, extractiveness is not as severe as pure rent-seeking (0.70+) because genuine coordination functions exist: professional standards reduce quality variance, liability frameworks protect consumers, ethical enforcement deters fraud. The moderate-high value reflects that extraction is layered ON TOP of real coordination, not instead of it. Suppression (0.68): High. Barriers to entry are substantial: licensing fees ($100–$500 per exam/renewal), education costs ($10k–$100k+), apprenticeship duration (2–7 years), reciprocity exam barriers across states, and continuing education mandates. Career switching is costly; prospective workers cannot easily exit once committed. However, suppression is not absolute (1.0) because some unlicensed alternatives exist (gig economy, unlicensed services), though they carry legal and reputational risk. Theater ratio (0.58): Moderate-high. A significant proportion of licensing administration is performative: continuing education requirements that don't correlate with competency improvement, renewal ceremonies, reciprocity exams that duplicate information, and professional ethics codes maintained through tradition rather than enforcement data. However, theater is not dominant (< 0.70) because core licensing functions (initial competency testing, disciplinary mechanisms) retain functional content.
 *
 * PERSPECTIVAL GAP:
 *   The prospective worker perceives a snare: steep entry barriers, sunk costs, no exit option. The incumbent professional perceives rope: the system solves the coordination problem of maintaining professional quality and justifying market prices through credentialing. The licensing board perceives rope: public-interest quality assurance and consumer protection. The low-income consumer perceives tangled_rope: benefits from quality standards but pays inflated prices due to supply constraint. The mobility coalition perceives tangled_rope: genuine coordination value exists, but is wrapped in extractive rent-seeking (unnecessary reciprocity barriers, inflated education requirements, fee structures). The civilizational view perceives piton: the system maintains itself through institutional inertia and stakeholder investment, with theater ratio increasing as requirements accumulate without evidence-based justification. The engine's false summit detector will flag the piton perspective as risk: treating occupational licensing as an immutable institutional feature naturalizes what is actually a policy design choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from beneficiary/victim status and exit options. Prospective workers are victims with trapped exit (d ≈ 0.95) — they bear full cost of gatekeeping and cannot escape without sunk-cost abandonment. Low-income consumers are victims with constrained exit (d ≈ 0.70) — they benefit from some coordination but face inflated prices and access barriers. Incumbents are beneficiaries with arbitrage exit (d ≈ 0.10) — they capture rents and can adjust location/career dynamically. Licensing boards are beneficiaries with arbitrage exit (d ≈ 0.15) — they maintain institutional position and can modify requirements. The sigmoid function f(d) maps these to experienced extraction chi: prospective workers experience chi ≈ 1.15 (high effective extraction despite moderate base ε), incumbents experience chi ≈ -0.12 (negative, i.e., subsidized). The organized mobility coalition with constrained exit occupies the middle (d ≈ 0.55, f(d) ≈ 0.75, moderate chi). This directionality structure explains the perspectival gap: prospective workers see a snare (high experienced extraction), incumbents see rope (pure coordination and market stability), organized coalitions see tangled_rope (coordination with embedded extraction).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in occupational licensing is resolved by recognizing that all perspectives reflect real structural features. The prospective worker's snare is genuine: they do face irreversible commitment costs and suppressed exit. The incumbent's rope is genuine: licensing does coordinate professional quality and stability. The constraint is not miscategorized as one type when it should be another — it is legitimately tangled_rope from multiple institutional perspectives because it performs genuine coordination (quality assurance) while layering extraction (entry barriers, supply constraint) asymmetrically onto different agents. The mandatrophy resolution is that the classification is CORRECT: tangled_rope IS the right type because the constraint cannot be decomposed into pure coordination or pure extraction. It requires BOTH beneficiaries (incumbents, licensing boards) AND victims (prospective workers, consumers), AND active enforcement of barriers, AND genuine coordination functions. The analytical question is not 'is this rope or snare?' but 'what proportion of the constraint serves coordination vs. extraction?' The measurements show theater_ratio increasing (0.42 → 0.58), suggesting that extractiveness is drifting toward snare (theater increasing = ritual without function increasing). A 10-year trajectory that shows theater_ratio > 0.70 and extractiveness > 0.70 would indicate degradation toward piton (institutional inertia replacing functional coordination).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    quality_assurance_counterfactual,
    'How much of observed service quality in licensed professions is attributable to licensing requirements versus market reputation, professional ethics norms, and liability law?',
    'Cross-sectional comparison of licensed vs. unlicensed professions (interior design, event planning) and quasi-experimental studies (e.g., removal of reciprocity barriers); correlation analysis between licensing rigor and consumer complaint rates',
    'If licensing accounts for > 70% of quality variance: coordination value is high, licensing barriers are justified. If < 30%: coordination value is low, licensing is primarily extractive rent-seeking.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quality_assurance_counterfactual, empirical, 'Attribution of service quality to licensing vs. other mechanisms').

omega_variable(
    occupational_mobility_substitutes,
    'Are emerging digital credentials, portfolio-based hiring, and skill certifications (non-government-regulated) achieving equivalent consumer protection and labor matching without licensure barriers?',
    'Longitudinal tracking of gig economy platforms, portfolio-based hiring, and skills marketplaces; comparison of consumer satisfaction, incident rates, and labor market efficiency (time-to-hire, wage competitiveness) between licensed and alternative credentialing systems',
    'If substitutes are effective: licensing sunset is feasible within 10-15 years (scaffold perspective confirmed). If substitutes fail: licensing barriers persist as structural necessity (rope/mountain perspective validated).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(occupational_mobility_substitutes, empirical, 'Whether alternative credentialing systems substitute for occupational licensing').

omega_variable(
    regulatory_capture_asymmetry,
    'Do incumbent professional associations exercise structural power over licensing board composition and requirement-setting in ways that suppress entry-level competition beyond what consumer protection requires?',
    'Analysis of licensing board membership demographics (% from incumbent profession), voting patterns on requirement changes, correlation between board composition and requirement stringency across states; interviews with board members on decision-making rationale',
    'If capture is high: licensing is primarily extractive (snare). If capture is low: licensing represents genuine public-interest coordination (rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_capture_asymmetry, empirical, 'Degree of professional association capture over licensing boards').

omega_variable(
    interstate_reciprocity_efficiency,
    'Do reciprocity barriers between states serve consumer protection or primarily entrench incumbent professional power in each state market?',
    'Comparison of reciprocity regimes (full reciprocal recognition, exam-waiver agreements, portfolio review) with cross-state labor mobility rates and wage leveling; analysis of reciprocity requirement changes and their correlation with professional association lobbying efforts',
    'If reciprocity serves protection: national harmonization could improve mobility (scaffold perspective). If primarily entrenchment: reciprocity is a key extraction mechanism (snare/tangled_rope confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interstate_reciprocity_efficiency, empirical, 'Whether reciprocity barriers serve consumer protection or incumbent protection').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(occupational_licensing_barriers, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(occlic_tr_t0, occupational_licensing_barriers, theater_ratio, 0, 0.42).
narrative_ontology:measurement(occlic_tr_t10, occupational_licensing_barriers, theater_ratio, 10, 0.5).
narrative_ontology:measurement(occlic_tr_t20, occupational_licensing_barriers, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(occlic_be_t0, occupational_licensing_barriers, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(occlic_be_t10, occupational_licensing_barriers, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(occlic_be_t20, occupational_licensing_barriers, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(occupational_licensing_barriers, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(occupational_licensing_barriers, 0.15).
narrative_ontology:affects_constraint(occupational_licensing_barriers, occupational_wage_inequality).
narrative_ontology:affects_constraint(occupational_licensing_barriers, geographic_labor_mobility_restriction).
narrative_ontology:affects_constraint(occupational_licensing_barriers, professional_association_regulatory_capture).

% DUAL FORMULATION NOTE:
% Occupational licensing contains multiple structurally distinct coordination mechanisms: quality-assurance (information standard), supply-constraint (resource allocation), professional-boundary maintenance (identity coordination). This story treats the system as a unified enforcement mechanism. Decomposition into separate stories would require: (1) licensing as pure quality standard (rope, low ε), (2) licensing as supply constraint (snare/tangled_rope, high ε), (3) professional association gatekeeping (snare, high ε). The unified treatment is appropriate for policy analysis; decomposition would be appropriate for component-level research.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(occupational_licensing_barriers, organized, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
