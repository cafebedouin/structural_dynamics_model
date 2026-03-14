% ============================================================================
% CONSTRAINT STORY: employer_liability_aversion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_employer_liability_aversion, []).

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
 *   constraint_id: employer_liability_aversion
 *   human_readable: Employer Liability Aversion in Worker Protection
 *   domain: labor/employment/legal_risk
 *
 * SUMMARY:
 *   Employer liability aversion creates a structural constraint where the
 *   legal mechanism intended to align employer incentives with worker safety
 *   has evolved into an extraction system. The constraint operates through
 *   three coordinated mechanisms: (1) documented incident reporting that
 *   creates discoverable evidence in litigation, (2) insurance pooling that
 *   enables employers to transfer risk while minimizing individual hazard
 *   reduction incentives, and (3) asymmetric litigation barriers that make
 *   worker recovery difficult while protecting employer assets. The
 *   constraint exhibits Tangled Rope characteristics: genuine coordination
 *   exists (hazard management, safety standardization, risk quantification)
 *   alongside asymmetric extraction (workers bear injury costs; employers
 *   control documentation; insurance industry profits from risk
 *   quantification without bearing individual injury costs). The theater
 *   ratio has increased over the interval as litigation risk has grown
 *   relative to actual safety function — employers increasingly invest in
 *   defensive documentation and litigation preparation rather than hazard
 *   elimination. The constraint is not immutable law but a contingent
 *   institutional arrangement that could be replaced by alternative systems
 *   (no-fault insurance, outcome-based safety standards, direct regulatory
 *   accountability).
 *
 * KEY AGENTS:
 *   - Workers/Injured Employees: Primary victim (powerless/trapped) — bear full cost of injury including medical, income loss, and litigation barriers; face retaliation risk and employment dependency; no meaningful exit from employment-liability structure
 *   - Employers: Primary beneficiary (institutional/arbitrage) — benefit from liability protection, insurance pooling, and documentation leverage; can exit high-risk sectors or relocate; have arbitrage options for managing risk exposure
 *   - Liability Insurance Industry: Secondary beneficiary (powerful/arbitrage) — profits from systematic risk quantification and premium collection; has exit options (can adjust underwriting, exit sectors, or redefine coverage); sees constraint as pure coordination mechanism
 *   - Compliance/HR Officers: Moderate victim (moderate/constrained) — experience mixed coordination (genuine safety management) and extraction (litigation-driven documentation burden); face liability exposure asymmetrically distributed; have constrained exit (whistle-blowing risk, credential penalties)
 *   - Labor Unions and Safety Advocates: Organized agents (organized/constrained) — see genuine coordination function but blocked by suppression and litigation barriers; have constrained exit (can pressure for legislative change but face employer resistance); moderate extractiveness reflects both agency and structural blockage
 *   - Legal/Tort System: Institutional actor experiencing degradation (institutional/arbitrage) — original coordination mechanism (liability creates safety incentive) has atrophied; system now primarily functions as litigation theater; maintains itself through inertia rather than functional necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(employer_liability_aversion, 0.58).
domain_priors:suppression_score(employer_liability_aversion, 0.68).
domain_priors:theater_ratio(employer_liability_aversion, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(employer_liability_aversion, extractiveness, 0.58).
narrative_ontology:constraint_metric(employer_liability_aversion, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(employer_liability_aversion, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(employer_liability_aversion, tangled_rope).
narrative_ontology:human_readable(employer_liability_aversion, "Employer Liability Aversion in Worker Protection").
narrative_ontology:topic_domain(employer_liability_aversion, "labor/employment/legal_risk").

domain_priors:requires_active_enforcement(employer_liability_aversion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(employer_liability_aversion, employers).
narrative_ontology:constraint_beneficiary(employer_liability_aversion, liability_insurance_industry).
narrative_ontology:constraint_victim(employer_liability_aversion, workers).
narrative_ontology:constraint_victim(employer_liability_aversion, workplace_safety_culture).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INJURED WORKER (SNARE) — Worker bears full cost of injury while employer avoids liability through defensive documentation, arbitration clauses, and contingent-fee legal barriers. Worker has no exit from the employment-dependency structure and faces maximum suppression: medical costs, job loss risk, retaliation fear, and lengthy litigation. Experiences pure extraction with no coordination benefit.
constraint_indexing:constraint_classification(employer_liability_aversion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: COMPLIANCE OFFICER (TANGLED ROPE) — Experiences both genuine coordination (workplace safety genuinely requires incident reporting and hazard management) and asymmetric extraction (liability concerns drive documentation burden that protects employers more than workers; the compliance officer's own liability exposure is asymmetrically distributed). Has constrained exit — can find different employment but faces reputational and credential penalties for whistle-blowing.
constraint_indexing:constraint_classification(employer_liability_aversion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EMPLOYER (ROPE) — Experiences the constraint as coordination: liability management, incident documentation, and insurance protocols create shared standards that enable predictable risk assessment and contract formation. Employer has arbitrage options (can shift operations, self-insure, or exit high-risk sectors). Extraction runs toward this agent — they are the primary beneficiary of the constraint's structure.
constraint_indexing:constraint_classification(employer_liability_aversion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LIABILITY INSURANCE INDUSTRY (ROPE) — Experiences the constraint as pure coordination: systematic risk quantification, underwriting standards, and documented hazard disclosure reduce informational asymmetries and enable profitable risk pooling. The industry sees itself as solving a coordination problem (unknown risk), not extracting. Has high arbitrage options — can exit sectors, adjust premiums, or redefine coverage. Benefits from both the constraint structure and the employer's liability aversion.
constraint_indexing:constraint_classification(employer_liability_aversion, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: LABOR ORGANIZING / SAFETY ADVOCATES (TANGLED ROPE) — Organized agents (unions, OSHA, workplace safety NGOs) see both genuine coordination (hazard documentation, incident reporting, safety standards) and extraction (liability aversion suppresses transparent risk communication; employers use documentation as litigation defense rather than safety improvement). Have constrained exit — can pressure for legislative change but face employer resistance and political headwinds. Measured extraction reflects that advocacy has agency but is blocked by suppression.
constraint_indexing:constraint_classification(employer_liability_aversion, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: TORT LIABILITY SYSTEM (PITON) — The legal framework theoretically aligns employer incentives with worker protection (liability creates incentive to prevent injury), but this mechanism has atrophied. Theater ratio (0.64) reflects that the system now primarily functions as a barrier to worker recovery and a document production mill: lengthy discovery, expert gatekeeping, settlement leverage imbalances, and contingent-fee structures make the system increasingly performative. The original coordination function (liability creates safety incentive) persists rhetorically but is largely theatrical — employers invest in litigation defense and documentation rather than hazard elimination.
constraint_indexing:constraint_classification(employer_liability_aversion, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (FALSE MOUNTAIN) — From a civilizational view, employer liability aversion might appear immutable: employers inevitably face legal risk for workplace injury; risk aversion is inherent to rational economic behavior. However, the base properties reveal this as a false summit — the constraint is not inherent to employment, but contingent on specific legal structures, insurance mechanisms, and documentation requirements. The 'mountain' framing naturalizes what are actually mutable institutional arrangements (no-fault insurance systems, different liability regimes, alternative safety accountability mechanisms). Engine will correctly flag this as a false natural law.
constraint_indexing:constraint_classification(employer_liability_aversion, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(employer_liability_aversion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(employer_liability_aversion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(employer_liability_aversion, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(employer_liability_aversion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(employer_liability_aversion, TR),
    TR >= 0.70.

:- end_tests(employer_liability_aversion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, reflecting that the constraint genuinely extracts from workers and moderate agents (higher than pure coordination but not maximum extraction). The value reflects: (1) workers bear full injury costs while employers transfer risk via insurance, (2) litigation barriers suppress worker recovery, (3) documentation practices favor employer defense over hazard elimination. But extraction is not total — some safety coordination occurs; some worker recovery happens; some employers do invest in hazard reduction. Suppression (0.68): High, reflecting significant barriers to worker exit and information access: employment dependency, medical cost burden, litigation complexity, confidentiality clauses, retaliation risk, and job-market penalties for injured workers. Suppression is structural (external barriers) not internalized. Theater ratio (0.64): Moderate-high, reflecting that employer documentation and litigation preparation have become substantial compared to actual hazard elimination. The 35-year increase in theater ratio (0.35 → 0.64) reflects rising litigation intensity relative to safety function. However, theater is not total — genuine hazard documentation and safety practices still occur; the constraint serves dual functions rather than being purely performative.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates core perspectival disagreement despite identical base properties. The injured worker sees pure extraction (Snare) — liability aversion reduces their protection while increasing employer defense. The employer sees pure coordination (Rope) — liability management enables predictable risk assessment and contract formation. The insurance industry agrees with the employer (Rope) — systematic risk quantification is a coordination function they perform. The compliance officer sees mixed coordination and extraction (Tangled Rope) — genuine safety management alongside litigation-driven documentation burden. Unions and advocates see extraction with suppression (Tangled Rope) — the coordination function is real but blocked by litigation barriers and information suppression. The tort system itself sees degradation (Piton) — the original safety incentive mechanism has atrophied into documentation theater. The civilizational analyst risks seeing immutable risk aversion (Mountain) but engine will flag this as false natural law. The gap reveals that the constraint's classification depends entirely on whether you benefit from or bear costs from liability aversion.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality differs across agents based on their structural relationship. Injured workers (powerless/trapped) have d ≈ 0.95: they are full targets bearing all costs with no exit, producing maximum f(d) ≈ 1.42 and highest experienced extractiveness. Employers (institutional/arbitrage) have d ≈ 0.10: they are beneficiaries with exit options, producing low f(d) ≈ -0.05 and negative/zero experienced extractiveness. Insurance industry (powerful/arbitrage) has d ≈ 0.08: similar beneficiary status with escape options. Compliance officers (moderate/constrained) have d ≈ 0.58: mixed beneficiary/victim status with constrained exit, producing moderate f(d) ≈ 0.70 and moderate extractiveness. This divergence across agents with different power and exit parameters produces perspectival gap: the powerless worker experiences Snare (pure extraction), the moderate officer experiences Tangled Rope (mixed), the institutional beneficiary experiences Rope (coordination). The analytical observer risks seeing Mountain (naturalizing risk aversion as law of nature) but the structural data shows contingency.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: This constraint demonstrates how liability aversion prevents the very thing it claims to enable. The intended coordination mechanism is: employer liability → employer has incentive to prevent injury → safer workplace → worker protection. The mandatrophy emerges because this chain is broken: (1) liability creates incentive to suppress information and manage litigation rather than eliminate hazards, (2) insurance enables risk transfer without matching incentive to reduce risk, (3) workers bear injury costs regardless of employer liability, creating extraction rather than protection. The tangled_rope classification resolves the mandatrophy by showing that the constraint genuinely coordinates some activities (risk quantification, incident documentation, safety standardization) while asymmetrically extracting from workers (who pay in injury and litigation costs while employers pay in insurance and legal defense). The constraint is neither pure coordination (which would reduce injury through safety investment) nor pure extraction (which would have no safety function). It is hybrid: coordination function persists but is weaker than extraction function, and extraction is masked as coordination. Mandatrophy resolved by recognizing that liability aversion's dual nature — real coordination component alongside asymmetric extraction — is the accurate classification, not a failure to choose between Rope and Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    liability_prevention_hypothesis,
    'Does employer liability exposure actually create stronger safety incentives than alternative mechanisms (outcome-based safety standards, no-fault insurance with experience rating, direct regulatory inspection)?',
    'Comparative analysis of injury rates and safety investment under different liability regimes (US tort-based vs European no-fault vs strict outcome-based standards). Measurement of employer hazard elimination spending vs litigation defense spending.',
    'If liability creates stronger safety incentives: constraint is primarily coordinating incentives (Rope classification higher). If alternative mechanisms are equally or more effective: constraint''s safety function is weaker than its extraction function, supporting Snare/Tangled Rope classification. Changes classification for institutional perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liability_prevention_hypothesis, empirical, 'Whether liability exposure creates superior safety incentives').

omega_variable(
    documentation_gaming_threshold,
    'At what point does employer investment in defensive documentation (incident records, litigation files, expert reports) become a primary focus rather than an incidental byproduct of genuine safety management?',
    'Time allocation analysis: proportion of compliance officer and legal team time spent on hazard elimination vs document production and litigation response. Correlation between documentation intensity and actual injury reduction.',
    'If significant portion of defensive infrastructure is about litigation rather than prevention: extractiveness is higher than stated (0.58). If documentation genuinely drives hazard reduction: tangled_rope classification is accurate. Affects measurement trajectory and mandatrophy analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(documentation_gaming_threshold, empirical, 'Threshold at which documentation becomes gaming rather than safety management').

omega_variable(
    worker_information_suppression,
    'Does liability aversion create structural incentives for employers to suppress or obscure information about workplace hazards that might be discoverable in litigation?',
    'Comparison of hazard disclosure (MSDS, incident rates, exposure data) across high-litigation-risk vs low-litigation-risk industries. Analysis of confidentiality clauses and settlement terms that restrict worker communication about hazards.',
    'If suppression is significant: base suppression (0.68) is understated; theater_ratio higher; extractiveness increases toward snare range. If minimal: suppression reflects only normal information asymmetries and suppression value is appropriate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(worker_information_suppression, empirical, 'Whether liability aversion suppresses hazard information disclosure').

omega_variable(
    exit_cost_underestimation,
    'Is the ''constrained'' exit classification for workers understating the true barriers to exit (trapped vs constrained)?',
    'Analysis of job-switching costs for injured workers: wage penalties, credential recognition, insurance rating changes, retaliation prevalence. Measurement of actual exit rates post-injury across industry and injury severity.',
    'If exit is actually trapped-level: workers'' experienced extractiveness (chi) is higher; snare classification becomes stronger. If constrained is accurate: tangled_rope classification for moderate agents is appropriate. Changes directionality derivation for powerless and moderate perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_cost_underestimation, empirical, 'Whether worker exit barriers are trapped-level or constrained-level').

omega_variable(
    insurance_moral_hazard_gap,
    'Does liability insurance reduce moral hazard (employer safety investment) or simply spread risk, making hazard reduction less valuable to individual employers?',
    'Comparison of safety investment rates in fully-insured vs self-insured employers. Analysis of insurance premium structures: do experience-rated premiums create stronger safety incentives than flat-rate or market-average premiums?',
    'If insurance creates moral hazard: extraction is amplified (employers have less incentive to prevent injury if insured). If experience-rating maintains incentives: rope coordination mechanism is stronger than snare extraction. Changes insurance_industry perspective classification and beneficiary status.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(insurance_moral_hazard_gap, empirical, 'Whether liability insurance reduces or maintains safety incentives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(employer_liability_aversion, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(emplia_tr_t0, employer_liability_aversion, theater_ratio, 0, 0.35).
narrative_ontology:measurement(emplia_tr_t3, employer_liability_aversion, theater_ratio, 3, 0.48).
narrative_ontology:measurement(emplia_tr_t6, employer_liability_aversion, theater_ratio, 6, 0.58).
narrative_ontology:measurement(emplia_tr_t10, employer_liability_aversion, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(emplia_be_t0, employer_liability_aversion, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(emplia_be_t3, employer_liability_aversion, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(emplia_be_t6, employer_liability_aversion, base_extractiveness, 6, 0.54).
narrative_ontology:measurement(emplia_be_t10, employer_liability_aversion, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(employer_liability_aversion, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(employer_liability_aversion, 0.12).
narrative_ontology:affects_constraint(employer_liability_aversion, worker_contingent_status).
narrative_ontology:affects_constraint(employer_liability_aversion, occupational_safety_standards_compliance).
narrative_ontology:affects_constraint(employer_liability_aversion, litigation_bias_against_workers).

% DUAL FORMULATION NOTE:
% Employer liability aversion is the institutional mechanism linking worker status (contingent employment increases risk exposure) to safety standard compliance (documentation-heavy vs outcome-heavy). Downstream constraints inherit extractiveness via this network: contingent workers face higher injury rates under liability aversion; safety compliance becomes documentation performance rather than hazard elimination; litigation barriers concentrate on low-power workers.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(employer_liability_aversion, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
