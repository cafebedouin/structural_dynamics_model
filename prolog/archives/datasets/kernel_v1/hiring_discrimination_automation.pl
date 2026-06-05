% ============================================================================
% CONSTRAINT STORY: hiring_discrimination_automation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hiring_discrimination_automation, []).

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
 *   constraint_id: hiring_discrimination_automation
 *   human_readable: Hiring Discrimination Automation
 *   domain: labor_economics/algorithmic_governance
 *
 * SUMMARY:
 *   Hiring discrimination automation encodes historical bias into algorithmic
 *   decision-making systems while obscuring the discrimination mechanism
 *   behind technical complexity, statistical framing, and plausible
 *   deniability. Organizations deploy screening algorithms ostensibly to
 *   reduce hiring friction and standardize candidate evaluation, claiming
 *   efficiency and objectivity gains. The systems systematically exclude
 *   protected classes (women, minorities, people with non-traditional
 *   backgrounds) through mechanisms including biased training data, proxy
 *   discrimination (penalizing résumé gaps, non-traditional education paths,
 *   geographic markers, phonetic name inference), and optimization for
 *   homogeneity in protected-class historical hiring patterns. The constraint
 *   exhibits snare-class extraction: excluded candidates are powerless,
 *   trapped, and unable to identify or challenge the discrimination
 *   mechanism. The suppression is high — technical opacity creates epistemic
 *   barriers, burden of legal proof is on individuals rather than systems,
 *   and regulatory enforcement is degraded by algorithmic complexity. The
 *   theater ratio has risen over time as vendors deploy increasingly opaque
 *   machine learning techniques and organizations adopt compliance theater
 *   (bias audit reports, vendor certifications) without material
 *   discrimination reduction. This constraint demonstrates the false summit
 *   signature: the analytical observer risks naturalizing discrimination as
 *   an inevitable property of algorithmic systems ('algorithms inherit
 *   training data bias — this is inherent to machine learning') when the
 *   actual structure is institutional choice backed by beneficiary incentives
 *   (organizations capture efficiency gains, vendors capture market share,
 *   both externalize discrimination costs onto powerless candidates).
 *
 * KEY AGENTS:
 *   - Excluded Candidates: Primary victims (powerless/trapped) — protected class members systematically screened out with no visibility into mechanism or remedy
 *   - Protected Class Populations: Primary victims (powerless/trapped, generational perspective) — labor market accessibility degraded by accumulated bias across hiring cycles
 *   - Hiring Organizations: Primary beneficiaries (institutional/arbitrage, coordination frame) and constrained actors (institutional/constrained, regulatory frame) — benefit from efficiency and standardization while facing escalating legal liability
 *   - Algorithm Vendors: Secondary beneficiaries (powerful/arbitrage) — profit from sales while externalizing discrimination costs; benefit from technical opacity as insulation from liability
 *   - HR Professionals: Moderate-power constrained actors (moderate/constrained) — caught between employer directives to deploy algorithms and professional responsibility for fair hiring; often lack technical capacity to audit systems
 *   - Regulatory Agencies (EEOC, State DOL): Institutional actors (institutional/constrained, piton perspective) — enforcement authority degraded by algorithmic complexity; audit rituals are performative
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing constructed discrimination as technical inevitability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hiring_discrimination_automation, 0.68).
domain_priors:suppression_score(hiring_discrimination_automation, 0.72).
domain_priors:theater_ratio(hiring_discrimination_automation, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hiring_discrimination_automation, extractiveness, 0.68).
narrative_ontology:constraint_metric(hiring_discrimination_automation, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(hiring_discrimination_automation, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hiring_discrimination_automation, snare).
narrative_ontology:human_readable(hiring_discrimination_automation, "Hiring Discrimination Automation").
narrative_ontology:topic_domain(hiring_discrimination_automation, "labor_economics/algorithmic_governance").

domain_priors:requires_active_enforcement(hiring_discrimination_automation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hiring_discrimination_automation, hiring_organizations).
narrative_ontology:constraint_beneficiary(hiring_discrimination_automation, technical_vendors).
narrative_ontology:constraint_victim(hiring_discrimination_automation, protected_class_candidates).
narrative_ontology:constraint_victim(hiring_discrimination_automation, non_traditional_background_candidates).
narrative_ontology:constraint_victim(hiring_discrimination_automation, labor_market_accessibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED CANDIDATE (SNARE) — Powerless agent trapped within national labor market. Cannot exit the screening algorithm's jurisdiction or appeal opaque automated rejection. Experiences maximum extraction: career harm, income loss, and no avenue for remedy due to technical obscurity. The mechanism is invisible — candidate never knows the algorithm rejected them.
constraint_indexing:constraint_classification(hiring_discrimination_automation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LABOR MARKET ACCESSIBILITY (SNARE, GENERATIONAL) — Abstract collective good (equitable access to employment) is trapped within systematic exclusion architecture. No self-correction mechanism: the algorithms accumulate historical bias over hiring cycles, degrading labor market accessibility as a generational property. Powerless and trapped — cannot organize or exit the constraint.
constraint_indexing:constraint_classification(hiring_discrimination_automation, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: HIRING ORGANIZATION (ROPE, COORDINATION VIEW) — From the organization's internal narrative, the constraint solves a genuine coordination problem: screening thousands of résumés efficiently, reducing hiring manager variability, and standardizing evaluation. Net beneficiary with arbitrage exit (can switch algorithms, disable screening, or adopt alternative methods). Experiences the constraint as productive coordination rather than extraction.
constraint_indexing:constraint_classification(hiring_discrimination_automation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: HIRING ORGANIZATION (TANGLED ROPE, REGULATORY EXPOSURE) — Constrained by growing legal liability (EEOC scrutiny, disparate impact litigation, state algorithmic auditing requirements). Benefits from efficiency gains of automation but now faces compliance costs, reputational risk, and potential damages. Active enforcement (legal threat) escalating. Exit is costly (algorithm replacement, manual review resumption) but available.
constraint_indexing:constraint_classification(hiring_discrimination_automation, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ALGORITHM VENDOR (ROPE) — Powerful institutional actor with full arbitrage exit (sell algorithms to other industries, pivot to different compliance claims, or refactor products). Benefits from algorithmic sales without bearing discrimination costs — costs are externalized to organizations and candidates. Coordination function: vendors solve the technical problem of candidate screening at scale. No enforcement pressure on vendors themselves.
constraint_indexing:constraint_classification(hiring_discrimination_automation, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: HR PROFESSIONAL (SNARE, CONSTRAINED) — Moderate power, constrained by employer directives and professional norms ('use best tools available'). Experiences pressure to deploy discrimination automation for efficiency while bearing some responsibility for exclusionary outcomes. Exit is costly (job loss, career damage in same field) but theoretically possible. Often lacks technical literacy to audit algorithm bias or challenge vendor claims.
constraint_indexing:constraint_classification(hiring_discrimination_automation, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: REGULATORY APPARATUS (PITON) — EEOC guidance, state algorithmic auditing rules, and hiring discrimination law persist but their enforcement is degraded. Auditing algorithms for bias is technically difficult; burden of proof falls on individual candidates to demonstrate disparate impact; regulatory capacity does not match algorithmic opacity. The enforcement ritual (audit requirement, documentation obligation) is increasingly performative — the rules exist but their application is captured by technical complexity.
constraint_indexing:constraint_classification(hiring_discrimination_automation, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (MOUNTAIN, FALSE NATURAL LAW) — From a civilizational/global analytical view, the constraint risks being naturalized as 'algorithms inherit the biases of training data — this is a technical inevitability.' This framing occludes the structural choice: organizations select algorithms knowing they will exclude protected classes, vendors profit from plausible deniability, and the benefit-cost structure rewards discrimination automation. The mountain classification is a false summit revealing naturalization of institutional choice.
constraint_indexing:constraint_classification(hiring_discrimination_automation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hiring_discrimination_automation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hiring_discrimination_automation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hiring_discrimination_automation, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(hiring_discrimination_automation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(hiring_discrimination_automation, TR),
    TR >= 0.70.

:- end_tests(hiring_discrimination_automation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. Organizations and vendors extract substantial benefits (efficiency gains, market share, reduced hiring liability through algorithmic deniability) while excluded candidates bear the full cost of labor market exclusion (lost income, career delay, psychological harm). The extraction is not as severe as a pure debt trap (0.85+) because some candidates eventually find alternative pathways and the constraint operates at the hiring gate rather than through ongoing debt servitude. But the temporal accumulation is significant — a candidate excluded at age 22 may experience generational income loss. Suppression (0.72): High. Multiple layers of suppression: (1) Technical opacity — candidates cannot see the algorithm or understand why rejected; (2) Epistemic barrier — auditing algorithms for bias requires specialized expertise not available to individual candidates; (3) Burden of proof — legal remedy requires demonstrating disparate impact across populations, not individual cases; (4) Regulatory capture — vendors influence algorithm auditing standards and compliance metrics; (5) Deniability — organizations claim 'objective technical process' rather than discrimination. Theater ratio (0.64): Moderate-high. Organizations deploy bias audit reports, vendor certifications ('fairness-tested'), and compliance documentation (disparate impact analysis) that create appearance of systematic bias reduction without material changes to discrimination outcomes. The theater has increased as legal pressure escalates — organizations now perform compliance rather than achieve it. Vendors circulate white papers on 'responsible AI' while selling discrimination-adjacent products. Measurement trajectory (0.35→0.68) reflects extraction accumulation: early algorithmic hiring (2015-2018) had lower discrimination rates than contemporary systems because vendors competed on technical sophistication rather than bias reduction; as legal pressure escalated (2018-2023), vendors shifted to compliance theater while maintaining biased systems; current trajectory (2023+) shows suppression intensification (more opaque algorithms) even as extraction rises (greater scale of automated screening).
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits a dramatic perspectival gap driven by directionality. The hiring organization sees Rope (coordination efficiency) because they benefit from automation and have arbitrage exit (can change algorithms or abandon screening). The excluded candidate sees Snare (maximum extraction, no exit) because they are powerless, trapped, and invisible to the mechanism. The vendor sees Rope (market demand, technological challenge, value creation) and maintains full arbitrage. The HR professional sees Tangled Rope (genuine coordination need but forced complicity in discrimination). The regulatory agent sees Piton (enforcement rules exist but are performative). The algorithm-as-natural-law analytical observer risks missing that these different perspectives are not alternative readings of the same constraint — they are different agents experiencing the same discrimination infrastructure from different structural positions. The snare perspective (excluded candidate, generational labor market) is the structural reality; the rope and mountain perspectives are beneficiary framings that obscure it.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) determine how each agent's structural position maps to effective extraction (χ). Excluded candidates: d=0.95 (full target), trapped exit → f(d)≈1.42 → high χ. Hiring organizations (beneficiary view): d=0.10 (partial beneficiary), arbitrage exit → f(d)≈-0.08 → negative χ (constraint subsidizes them). Vendors: d=0.05 (full beneficiary), arbitrage exit → f(d)≈-0.12 → negative χ (maximum subsidy without extraction exposure). HR professionals: d=0.70 (mixed, closer to victim), constrained exit → f(d)≈1.08 → moderate χ. Regulatory agencies: d=0.65 (victim of regulatory capture), constrained exit → f(d)≈0.98 → high χ (must apply rules they cannot enforce). These d values are automatic derivations from beneficiary/victim declarations plus exit capacity; they show why the same constraint is snare from the victim perspective and rope from the beneficiary perspective. The directionality gap IS the constraint's structure.
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE CLASSIFICATION CONFIRMED. The constraint exhibits snare signature: (1) Beneficiaries (organizations, vendors) exist and are identified; (2) Victims (excluded candidates, labor market accessibility) exist and are identified; (3) Asymmetric extraction: beneficiaries capture efficiency/market gains while victims bear discrimination costs; (4) High suppression: technical opacity, epistemic barriers, deniability, regulatory capture all prevent victims from organizing or exiting; (5) χ=0.68 > 0.66 snare threshold. The tangled_rope alternative (claiming coordination + extraction hybrid) is a beneficiary framing: organizations do solve a genuine screening problem, but the constraint is NOT a mixed coordination-extraction mechanism — it is pure extraction dressed in coordination language. The distinction: a true tangled_rope would show both beneficiaries AND victims accessing benefits from the coordination function. Here, victims (excluded candidates) receive zero coordination benefit; the only beneficiaries are organizations and vendors. The false summit (analytical observer naturalizing as technical inevitability) is precisely the move the snare classification prevents — by identifying structural beneficiaries and victims, the constraint cannot hide behind 'just how algorithms work.' The extraction is institutional choice, not technical determinism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proxy_discrimination_detectability,
    'Are proxy discrimination mechanisms (résumé gaps, non-traditional education, location, name inference) intentionally engineered into algorithms or emergent from biased training data?',
    'Algorithm source code audits; vendor deposition testimony; comparison of algorithm versions before and after bias-detection pressure; analysis of proxy variable selection decisions',
    'If intentional: clear snare with maximal extraction and suppression. If emergent: shifts toward tangled_rope (coordination + unintended bias) with lower suppression. If vendors knew and suppressed this information: criminal liability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proxy_discrimination_detectability, empirical, 'Whether proxy discrimination is intentional or emergent').

omega_variable(
    algorithmic_audit_effectiveness,
    'Can bias audits (disparate impact analysis, fairness metrics) meaningfully detect or prevent discrimination in production hiring algorithms?',
    'Longitudinal audit data: comparison of pre-deployment bias predictions vs post-deployment hiring outcomes; analysis of whether audited algorithms show reduced discrimination in validation sets vs real hiring; study of whether vendors implement audit recommendations',
    'If effective: regulatory perspective (PITON) is incorrect — enforcement mechanisms work and piton classification is premature. If ineffective: suppression rises (candidates have no practical remedy) and snare classification is solid.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithmic_audit_effectiveness, empirical, 'Whether algorithmic bias audits prevent discrimination in practice').

omega_variable(
    vendor_knowledge_of_bias,
    'What did algorithm vendors know, when did they know it, and what did they disclose to customers regarding bias in their products?',
    'Internal vendor documentation; patent filings; customer communications; whistleblower testimony; academic literature cited in design phase; quality assurance reports',
    'If vendors knew and concealed: active deception, high intentionality, snare classification solidified with maximal suppression (fraud barrier). If vendors did not know: negligence rather than conspiracy, shifts toward tangled_rope. If vendors knew but disclosed clearly: transparency mitigates suppression somewhat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vendor_knowledge_of_bias, empirical, 'Vendor knowledge and disclosure of algorithmic bias').

omega_variable(
    candidate_awareness_and_remedy,
    'Can rejected candidates identify algorithmic screening as the cause, understand the mechanism, and access meaningful legal remedy?',
    'Candidate notification practices; transparency requirements compliance; EEOC complaint analysis; litigation outcomes for algorithmic discrimination cases; access to algorithm testing and audit results',
    'If candidates cannot identify or remedy: suppression remains high (powerless agents trapped). If transparency and remedy pathways materialize: suppression falls and classification edges toward tangled_rope with potential scaffold (legal remedy as sunset mechanism).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(candidate_awareness_and_remedy, empirical, 'Candidate awareness and access to remedy for algorithmic discrimination').

omega_variable(
    false_summit_risk,
    'Is the constraint a genuine emergent property of algorithmic systems or a naturalized cover story for intentional discrimination infrastructure?',
    'Historical analysis of pre-algorithmic hiring discrimination (1980s-2000s); comparison of discrimination rates before and after algorithm deployment; vendor design choices around proxy variables; cost-benefit analysis of bias reduction for vendors',
    'If genuine emergent property: mountain or piton classification justified. If intentional infrastructure: snare classification confirmed and false summit signature fires. If hybrid (some emergent, some intentional): snare remains primary, with mandatrophy noting the hybrid mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_risk, conceptual, 'Whether constraint is emergent technical property or intentional discrimination infrastructure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hiring_discrimination_automation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hda_tr_t0, hiring_discrimination_automation, theater_ratio, 0, 0.38).
narrative_ontology:measurement(hda_tr_t5, hiring_discrimination_automation, theater_ratio, 5, 0.52).
narrative_ontology:measurement(hda_tr_t10, hiring_discrimination_automation, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(hda_be_t0, hiring_discrimination_automation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(hda_be_t5, hiring_discrimination_automation, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(hda_be_t10, hiring_discrimination_automation, base_extractiveness, 10, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(hda_su_t0, hiring_discrimination_automation, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(hda_su_t5, hiring_discrimination_automation, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(hda_su_t10, hiring_discrimination_automation, suppression_requirement, 10, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hiring_discrimination_automation, resource_allocation).
narrative_ontology:affects_constraint(hiring_discrimination_automation, wage_gap_accumulation).
narrative_ontology:affects_constraint(hiring_discrimination_automation, occupational_segregation_automation).
narrative_ontology:affects_constraint(hiring_discrimination_automation, resume_gap_penalty).

% DUAL FORMULATION NOTE:
% Hiring discrimination automation is downstream of three distinct constraint families: (1) wage_gap_accumulation — the constraint creates income differential starting at hire that compounds over career; (2) occupational_segregation_automation — the constraint systematically channels protected classes into lower-wage occupational categories; (3) resume_gap_penalty — the constraint penalizes the specific life events (caregiving, education interruption, geographic mobility) that differentiate protected classes' careers. Each downstream constraint has its own ε and perspectives. This story models the hiring gate mechanism itself; downstream stories model the income and occupational consequences.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hiring_discrimination_automation, institutional, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
