% ============================================================================
% CONSTRAINT STORY: jordan_microfinance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jordan_microfinance, []).

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
 *   constraint_id: jordan_microfinance
 *   human_readable: Ideological Gating of Microfinance in Jordan
 *   domain: economic/international_development
 *
 * SUMMARY:
 *   The Jordan microfinance program operated by FINCA and backed by USAID
 *   represents a structural hybrid: a genuine coordination mechanism (access
 *   to working capital for marginalized populations) coupled with asymmetric
 *   extraction (conditioning access on mandatory ideological/behavioral
 *   compliance sessions). Impoverished loan applicants face a choice between
 *   financial autonomy (achieved through the loan) and behavioral autonomy
 *   (surrendered through mandatory attendance). The constraint exhibits all
 *   markers of a Tangled Rope from the primary target's perspective: the
 *   coordination function (microfinance access) is real and valuable, but the
 *   extraction mechanism (behavioral gating) is also real and coercive. The
 *   extractiveness has increased over the interval as the program has matured
 *   — from an initial value of 0.35 (when sessions were presented more as
 *   optional services) to 0.52 (when compliance became a documented program
 *   requirement with explicit funding accountability). The theater ratio has
 *   risen from 0.48 to 0.64, indicating that the mandatory sessions have
 *   increasingly taken on a performative character — compliance can be
 *   tracked and reported to USAID, but the educational outcomes for borrowers
 *   have not proportionally improved. This suggests institutional drift
 *   toward Goodhart's law: the program has optimized for measurable
 *   compliance metrics (session attendance, USAID reporting) rather than
 *   actual borrower financial capability.
 *
 * KEY AGENTS:
 *   - Impoverished Loan Applicants: Primary victim (powerless/trapped) — need capital urgently; no alternative access; forced choice between financial and behavioral autonomy
 *   - FINCA International: Primary beneficiary (institutional/arbitrage) — captures program management fees, borrower data, compliance metrics, and narrative alignment with donor priorities
 *   - USAID: Secondary beneficiary (institutional/arbitrage) — achieves programmatic narrative (development impact, behavioral alignment), justifies continued funding allocation
 *   - Local Microentrepreneur Community: Secondary victim (moderate/constrained) — access to credit but constrained by mandatory attendance and community visibility of compliance
 *   - Jordanian Government/Central Bank: Institutional regulator (institutional/constrained) — nominally oversees program but dependent on USAID funding; regulatory role degraded to performative endorsement
 *   - Development NGO Coalition: Organized monitors (organized/constrained) — recognize both coordination function and extraction; push for decoupling
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing conditionality as inherent to aid; structured analysis reveals contingent design choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jordan_microfinance, 0.52).
domain_priors:suppression_score(jordan_microfinance, 0.68).
domain_priors:theater_ratio(jordan_microfinance, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jordan_microfinance, extractiveness, 0.52).
narrative_ontology:constraint_metric(jordan_microfinance, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(jordan_microfinance, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jordan_microfinance, tangled_rope).
narrative_ontology:human_readable(jordan_microfinance, "Ideological Gating of Microfinance in Jordan").
narrative_ontology:topic_domain(jordan_microfinance, "economic/international_development").

domain_priors:requires_active_enforcement(jordan_microfinance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jordan_microfinance, finca_international).
narrative_ontology:constraint_beneficiary(jordan_microfinance, usaid_narrative_compliance).
narrative_ontology:constraint_beneficiary(jordan_microfinance, ideological_alignment_gatekeepers).
narrative_ontology:constraint_victim(jordan_microfinance, impoverished_loan_applicants).
narrative_ontology:constraint_victim(jordan_microfinance, financial_autonomy).
narrative_ontology:constraint_victim(jordan_microfinance, non_aligned_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: IMPOVERISHED LOAN APPLICANT (SNARE) — Needs capital urgently to start or expand microenterprise; has no alternative funding source; trapped: must attend mandatory ideological sessions to access credit. Cannot exit without losing access to the only available capital source in local economy. d≈0.93, f(d)≈1.38, σ=0.8 → χ≈0.58.
constraint_indexing:constraint_classification(jordan_microfinance, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: LOCAL MICROENTREPRENEUR COMMUNITY (TANGLED ROPE) — Constrained by limited alternative credit sources and social ties within communities that value ideological conformity; benefits from the microfinance mechanism itself (access to working capital) but also bears extraction through mandatory attendance and ideological compliance requirements. d≈0.72, f(d)≈1.08, σ=0.9 → χ≈0.50.
constraint_indexing:constraint_classification(jordan_microfinance, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: FINCA INTERNATIONAL / USAID (ROPE) — Primary beneficiary with arbitrage exit. Experiences the constraint as coordination mechanism: educational sessions align borrower populations with development narrative, ensuring programmatic compliance and reportable impact. Extraction framed as capacity-building. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.06. Net beneficiary; coordination function is real (borrower retention and compliance) but asymmetric.
constraint_indexing:constraint_classification(jordan_microfinance, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: JORDANIAN GOVERNMENT / CENTRAL BANK (PITON) — Nominally oversees microfinance regulation; constrained by dependence on USAID funding and development narrative compliance. Sees the mandatory education requirement as degraded theater: the sessions do not substantively improve loan outcomes or borrower financial literacy (theater_ratio=0.64), but persist because disabling them would contradict USAID narrative and risk funding flows. The regulatory role has atrophied into performative endorsement. d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.39.
constraint_indexing:constraint_classification(jordan_microfinance, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: DEVELOPMENT NGO COALITION (TANGLED ROPE) — Organized agents (human rights groups, financial inclusion advocates) see the constraint as both enabling and extractive. Coordination function: USAID funding and microfinance access do bring capital to marginalized populations — this is real and valuable. But the mandatory ideological sessions are extractive: they condition financial access on behavioral compliance and worldview alignment. d≈0.48, f(d)≈0.62, σ=1.1 → χ≈0.35. The coalition recognizes both functions and pushes for decoupling them (making education optional).
constraint_indexing:constraint_classification(jordan_microfinance, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (FALSE SUMMIT) — From a civilizational view, the perspective risks naturalizing the constraint as immutable: 'International development always involves value transmission; conditionality is inherent to aid.' This naturalizes what the structural data reveals as contingent: the mandatory ideological gating is a design choice (ε=0.52, suppression=0.68), not a law of nature. The theater ratio (0.64) suggests the education has degraded into performative compliance, not substantive capacity-building. Engine detects false summit: this is not a Mountain.
constraint_indexing:constraint_classification(jordan_microfinance, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jordan_microfinance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(jordan_microfinance, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(jordan_microfinance, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(jordan_microfinance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(jordan_microfinance, TR),
    TR >= 0.70.

:- end_tests(jordan_microfinance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The mandatory education requirement conditions access to the only available capital source in the local economy, creating behavioral extraction beyond the legitimate financial terms of the loan. The value has increased from 0.35 to 0.52 over the interval because the program has shifted from framing education as optional value-add to treating it as a documented compliance requirement tied to funding metrics. This is not the maximum extraction (Snare-tier) because the microfinance function itself is genuinely valuable and borrowers do gain capital access — but they pay a price beyond interest and fees: compliance with an ideological/behavioral gating mechanism. Suppression (0.68): High. Limited alternative credit sources in rural Jordan, social pressure within communities, implicit understanding that non-compliance means loan denial, and information asymmetry about the true nature of the sessions all create significant barriers to exit or refusal. Borrowers cannot 'just say no' without losing access to capital. Theater ratio (0.64): Moderately high. The mandatory education sessions appear to have become increasingly performative over time. The initial curriculum may have contained substantive financial literacy content, but as the program has matured and donor reporting has become more central, sessions have shifted toward compliance tracking and narrative demonstration rather than measurable borrower capability development. The theater ratio of 0.64 suggests that roughly 2/3 of the session value is performance for external audiences (USAID, program evaluators) rather than substantive education.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates a significant perspectival gap between the primary beneficiary (FINCA/USAID) and the primary target (loan applicants). From the beneficiary's view, the mandatory education is a coordination mechanism (Rope) — it solves the alignment problem of ensuring borrowers internalize development narratives and comply with program requirements. From the applicant's view, it is pure extraction (Snare) — it conditions financial access on behavioral compliance with no meaningful alternative. The civil society coalition occupies a middle position (Tangled Rope) — they recognize both the genuine coordination function (access to capital) and the genuine extraction mechanism (behavioral gating). The Jordanian government's piton perspective reveals institutional capture: the regulatory role has atrophied into performative endorsement because the government depends on USAID funding and challenging USAID's program design risks that funding flow. The analytical observer's risk is naturalizing this as inherent to international development rather than recognizing it as a contingent design choice that could be decoupled (making education optional while preserving access).
 *
 * DIRECTIONALITY LOGIC:
 *   Loan applicants: Victim + trapped → d≈0.93, f(d)≈1.38. Maximal extraction from the applicant perspective: they need the capital, have no alternatives, and face mandatory behavioral compliance. FINCA/USAID: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. They have multiple exit options (funding sources, program locations, donor relationships) and benefit from the constraint through program management, compliance metrics, and narrative alignment. Microentrepreneur community: Victim + constrained → d≈0.72, f(d)≈1.08. Constrained by limited alternatives and social ties; extraction is real but mixed with genuine coordination benefit from capital access. Jordanian regulator: Victim + constrained → d≈0.55, f(d)≈0.75. Nominally an overseer but actually constrained by funding dependence; sees own regulatory role as degraded (piton). Development NGO coalition: Organized + constrained → d≈0.48, f(d)≈0.62. Constrained by limited leverage over USAID-funded programs but organized enough to advocate for change and publicize the extraction mechanism. Analytical observer: analytical → d≈0.72, f(d)≈1.15. Risk of naturalizing contingent design choice; structural data prevents false summit classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy tension in this constraint is between the real coordination function (access to microfinance capital for populations excluded from traditional banking) and the real extraction mechanism (behavioral compliance gating). The claimed_type (Tangled Rope) resolves this by requiring ALL three gates: (1) beneficiaries present (FINCA, USAID, narrative compliance gatekeepers) ✓, (2) victims present (loan applicants, financial autonomy, non-aligned populations) ✓, (3) active enforcement (mandatory sessions documented as program requirement, tied to funding metrics) ✓. The constraint is not pure coordination (Rope) because the extraction is significant and asymmetric — borrowers cannot opt out of the education without losing capital access. It is not pure extraction (Snare) because the microfinance function itself is genuinely valuable and creates real coordination benefit. The Tangled Rope classification is robust: the program solves a real coordination problem (capital access) AND extracts beyond that solution through behavioral gating. The extraction is not minimizable without reducing coordination — this is the defining tension of a Tangled Rope. Mandatrophy is resolved by showing that both dimensions are structurally present and necessary to the classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    education_session_efficacy,
    'Do the mandatory educational sessions produce measurable improvements in loan repayment, financial literacy, or enterprise success for borrowers?',
    'Randomized controlled trial: cohorts with mandatory sessions vs optional sessions vs no sessions; longitudinal tracking of loan outcomes, business growth, and financial stress; borrower self-reported financial knowledge and confidence metrics',
    'If sessions show measurable benefit: constraint partially justifiable as coordination/capacity-building (Rope likelihood increases). If sessions show no benefit or negative effects: constraint is pure extraction theater (Snare from borrower perspective, Piton from institutional view).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(education_session_efficacy, empirical, 'Efficacy of mandatory educational sessions on loan outcomes').

omega_variable(
    ideological_content_proportion,
    'What proportion of the mandatory sessions contains explicit ideological content (values, worldview transmission) vs practical financial/business education?',
    'Content analysis of session curricula; borrower interviews on perceived session purpose; NGO observer documentation of session content and tone',
    'If <20% ideological: constraint is primarily a coordination mechanism (Rope). If >50% ideological: constraint is primarily extraction via behavioral compliance (Snare/Tangled Rope). Proportion directly affects theater_ratio calibration.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ideological_content_proportion, empirical, 'Proportion of ideological content in mandatory sessions').

omega_variable(
    alternative_credit_availability,
    'Are there functioning alternative microfinance or credit sources available to loan applicants in Jordan outside FINCA''s program?',
    'Mapping of microfinance providers in target regions; borrower survey on awareness of alternatives; loan application success rates and terms across providers',
    'If true alternatives exist: suppression should be lower (~0.40-0.50), and applicants have exit options (Tangled Rope dominates from borrower view, not Snare). If FINCA is de facto sole source: suppression remains high (~0.68), and Snare classification from borrower view is correct.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_credit_availability, empirical, 'Availability of alternative credit sources to borrowers').

omega_variable(
    usaid_narrative_conditionality,
    'Does USAID''s funding for the FINCA program explicitly condition continued funding on maintaining the mandatory education requirement?',
    'USAID grant agreements and performance metrics; correspondence between USAID and FINCA; testimony from FINCA program managers on funding contingencies',
    'If conditioning is explicit: narrative compliance is a real structural force (benefits USAID, extracts from borrowers). If conditioning is implicit/cultural: constraint reveals institutional capture (Piton from regulator view). Either way, the beneficiary classification and directionality are confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(usaid_narrative_conditionality, empirical, 'USAID conditionality on mandatory education requirement').

omega_variable(
    borrower_consent_and_awareness,
    'Are loan applicants genuinely informed that the education sessions are mandatory and conditioned on access to credit, or is the conditionality presented implicitly or obscured?',
    'Borrower interviews on decision-making process; review of loan application materials and disclosures; comparison of stated and actual mandatory status',
    'If applicants are uninformed or misled: suppression is near-maximum (~0.85), Snare classification is robust. If applicants are aware but accept the tradeoff: suppression is lower (~0.55-0.60), Tangled Rope classification is stronger from borrower view.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(borrower_consent_and_awareness, empirical, 'Borrower awareness and consent to mandatory education conditionality').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jordan_microfinance, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jmf_tr_t0, jordan_microfinance, theater_ratio, 0, 0.48).
narrative_ontology:measurement(jmf_tr_t5, jordan_microfinance, theater_ratio, 5, 0.56).
narrative_ontology:measurement(jmf_tr_t10, jordan_microfinance, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(jmf_be_t0, jordan_microfinance, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(jmf_be_t5, jordan_microfinance, base_extractiveness, 5, 0.44).
narrative_ontology:measurement(jmf_be_t10, jordan_microfinance, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jordan_microfinance, resource_allocation).
narrative_ontology:affects_constraint(jordan_microfinance, conditional_aid_behavioral_gating).
narrative_ontology:affects_constraint(jordan_microfinance, microfinance_debt_cycle_lock_in).

% DUAL FORMULATION NOTE:
% The Jordan microfinance constraint is downstream of broader structural constraints on international development funding (USAID narrative compliance, conditionality regimes) and upstream of borrower-level debt/autonomy tradeoffs. The network link reflects that this constraint's extractive force depends partly on upstream narrative gatekeeping and partly creates downstream borrower vulnerability.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jordan_microfinance, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
