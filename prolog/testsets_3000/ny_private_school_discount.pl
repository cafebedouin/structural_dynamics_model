% ============================================================================
% CONSTRAINT STORY: ny_private_school_discount
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ny_private_school_discount, []).

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
 *   constraint_id: ny_private_school_discount
 *   human_readable: Discount-for-Data Scheme in Private Schools
 *   domain: economic/education
 *
 * SUMMARY:
 *   A private school startup in New York offers families a 42% tuition
 *   discount in exchange for comprehensive data about their children's
 *   academic performance, learning patterns, behavioral metrics, and family
 *   socioeconomic information. The school monetizes this data by licensing it
 *   to educational technology vendors, curriculum providers, and behavioral
 *   analytics firms. The scheme creates a structural tension between
 *   legitimate coordination (data enables personalization and school
 *   efficiency) and extractive monetization (families subsidize operations
 *   through data transfer rather than tuition payment, while the school
 *   captures licensing revenue). The constraint exhibits tangled rope
 *   characteristics: it has genuine coordination functions (adaptive
 *   learning, personalization) and real extraction mechanisms (data
 *   monetization, information asymmetry, asymmetric exit costs). The
 *   beneficiaries (school operator, edtech partners) have arbitrage and
 *   immediate/national exit options. The victims (enrolled families, child
 *   privacy commons) have constrained or trapped exit options. The scheme is
 *   not pure extraction because the coordination function is real; it is not
 *   pure coordination because the extraction is material and suppressed
 *   behind enrollment consent.
 *
 * KEY AGENTS:
 *   - School Operator: Primary beneficiary (institutional/arbitrage) — captures data licensing revenue and operational subsidy through discount mechanism; can exit by raising tuition
 *   - Enrolled Cost-Constrained Families: Primary victim (powerless/trapped) — cannot afford alternative private schools; no meaningful exit; bear full extraction
 *   - Affluent Families: Secondary actor (moderate/constrained) — have exit options (other schools, gifted public programs) but switching has friction; bear partial extraction
 *   - EdTech Licensing Partners: Beneficiary (institutional/arbitrage) — license student data for product development, behavioral analytics, algorithmic training
 *   - Parent Advocacy Coalition: Organized actor (organized/constrained) — coordinating privacy concerns, pushing for transparency; members have mixed exit capacity
 *   - Regulatory Framework (FERPA/SHIELD): Institutional constraint (institutional/arbitrage) — appears to govern but enforcement is weak; maintained through theater (consent forms) rather than functional limitation
 *   - Child Privacy Commons: Victim (powerless/trapped) — abstract collective good; no exit mechanism; bears externality costs from normalized data monetization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ny_private_school_discount, 0.58).
domain_priors:suppression_score(ny_private_school_discount, 0.68).
domain_priors:theater_ratio(ny_private_school_discount, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ny_private_school_discount, extractiveness, 0.58).
narrative_ontology:constraint_metric(ny_private_school_discount, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(ny_private_school_discount, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ny_private_school_discount, tangled_rope).
narrative_ontology:human_readable(ny_private_school_discount, "Discount-for-Data Scheme in Private Schools").
narrative_ontology:topic_domain(ny_private_school_discount, "economic/education").

domain_priors:requires_active_enforcement(ny_private_school_discount).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ny_private_school_discount, school_operator).
narrative_ontology:constraint_beneficiary(ny_private_school_discount, edtech_licensing_partners).
narrative_ontology:constraint_victim(ny_private_school_discount, enrolled_families).
narrative_ontology:constraint_victim(ny_private_school_discount, child_privacy_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ENROLLED FAMILY / COST-CONSTRAINED (SNARE) — Family cannot afford full tuition at competitive private schools. The discount-for-data offer is presented as a choice, but the alternative (public school or unaffordable private school) is foreclosed by household budget constraints. No meaningful exit: withdrawal means loss of educational placement and sunk tuition discount. Data surrender is non-negotiable condition of enrollment.
constraint_indexing:constraint_classification(ny_private_school_discount, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: AFFLUENT FAMILY / MOBILE (TANGLED ROPE) — Has alternative schools available (other private schools, transfer to public with gifted programs). Can exit the scheme, but at some cost (switching friction, educational disruption). Benefits from coordination (access to specialized curriculum, smaller class sizes) and simultaneously bears extraction (data monetization). The threat of exit constrains but does not eliminate the extraction mechanism.
constraint_indexing:constraint_classification(ny_private_school_discount, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: SCHOOL OPERATOR (ROPE) — Experiences the constraint as pure coordination: data enables adaptive learning algorithms, personalization, and operational efficiency. Can arbitrage the data (license to edtech vendors, sell insights to curriculum firms). The operator experiences the mechanism as a legitimate value exchange that subsidizes operations and fuels product development. Exit option is frictionless — stop the scheme and raise tuition. Net beneficiary.
constraint_indexing:constraint_classification(ny_private_school_discount, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PARENT ADVOCACY COALITION / ORGANIZED (TANGLED ROPE) — Organized parents (through education nonprofits, privacy groups) have leverage: public pressure, regulatory attention, reputational risk to the school. Constrained exit because members include both cost-constrained and mobile families with different leverage. Benefits from coordination (school quality improvements funded by data monetization) while organizing against extraction (data practices). This is not a snare because collective action is possible and is happening.
constraint_indexing:constraint_classification(ny_private_school_discount, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: REGULATORY FRAMEWORK / FERPA & STATE PRIVACY LAW (PITON) — FERPA (Family Educational Rights and Privacy Act) technically applies to private schools but is often treated as advisory. State privacy laws (NY SHIELD Act, etc.) have ambiguous application to educational data. The regulatory regime persists as a performative constraint — it appears to govern the scheme but enforcement is weak and parental consent is interpreted as sufficient cover. Theater ratio is high because compliance is largely theatrical (opt-in consent forms, vague privacy notices) without substantive limitation on monetization. The regulation was designed for an era without automated data extraction and is maintained through inertia rather than functional governance.
constraint_indexing:constraint_classification(ny_private_school_discount, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / INFORMATION ASYMMETRY (FALSE SUMMIT) — From a civilizational perspective, the scheme might appear to reflect immutable information asymmetry: the operator has technical expertise in data infrastructure that families lack, and this gap is inherent to modern education. Parents cannot reasonably understand the value of their data or the implications of its monetization. This perspective risks naturalizing what is actually a contingent institutional arrangement — the information asymmetry is real but remediable through transparency, consent standards, and data minimization. It is not a law of nature but a regulatory choice. Engine's false summit detector should identify this as naturalization.
constraint_indexing:constraint_classification(ny_private_school_discount, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ny_private_school_discount_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ny_private_school_discount, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ny_private_school_discount, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ny_private_school_discount, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ny_private_school_discount, TR),
    TR >= 0.70.

:- end_tests(ny_private_school_discount_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The school captures material economic value through data monetization (licensing to edtech/behavioral analytics). The 42% discount is genuinely offered, so the extraction is not as severe as a pure snare (would be ≥0.66). However, the extraction is substantial because: (1) families may not understand the true value of their data, (2) cost-constrained families have no exit, (3) the monetization is ongoing and extends beyond the enrollment period (data can be relicensed). The increasing trajectory from 0.35 to 0.58 reflects that the school initially relies on the discount draw but over time increasingly monetizes data as scale grows. Suppression (0.68): High. Barriers to meaningful refusal include: financial necessity (families cannot afford full tuition), information asymmetry (parents do not understand data valuation or downstream uses), regulatory ambiguity (FERPA/SHIELD have weak application), and consent framing (opt-out is formally available but functionally costly). The suppression is not absolute (mobile families can exit, organized groups can pressure for change) but it is substantial. Theater ratio (0.45): Moderate. The regulatory framework (FERPA/SHIELD) and consent processes are partly theatrical — they appear to govern data practices but enforcement is minimal and consent is interpreted as sufficient cover. However, the school's actual operational use of data for personalization is functional (not purely performative), which lowers the theater ratio compared to a pure regulatory piton. The theater ratio grows from 0.25 to 0.45 as the regulatory regime remains performative while actual monetization scale increases.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same mechanism produces different classifications depending on structural position. The operator sees coordination (Rope) because they benefit and have exit options. The cost-constrained family sees extraction (Snare) because they bear costs and have no exit. The affluent family sees hybrid (Tangled Rope) because they have some exit but also some constraint. The organized coalition sees hybrid with leverage (Tangled Rope with constrained exit rather than trapped). The regulatory perspective sees degraded governance (Piton) because FERPA/SHIELD are performative without enforcement. The analytical perspective risks seeing immutable information asymmetry (Mountain) when it is actually a regulatory and institutional choice. No single perspective is 'correct' — the constraint IS the distribution of these perspectives over the observation site.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is computed from the agent's power level, exit options, and relationship to the extraction flow. Cost-constrained families: powerless + trapped → high d (0.95) → high f(d) (≈1.42) → they experience maximum extraction chi. Affluent families: moderate + constrained → mid-high d (0.55) → mid-high f(d) (≈0.75) → they experience partial extraction. School operator: institutional + arbitrage → low d (0.05) → negative f(d) (≈-0.12) → they experience negative extraction (subsidy). Organized coalition: organized + constrained → mid d (0.40) → moderate f(d) (≈0.40) → they experience moderate extraction but with leverage to reduce it. Regulatory framework: institutional + arbitrage → low d (0.05) → they do not bear extraction; they enforce it. The analytics observer: analytical + analytical → mid-high d (0.72) → they see the full mechanism but risk naturalizing it.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The scheme resolves the mandatrophy by distinguishing genuine coordination (adaptive learning, personalization, operational efficiency) from extractive rent-seeking (data licensing, behavioral analytics sales). The coordination is real — the data enables legitimate pedagogical benefits. But the extraction is also real — families subsidize the school through data transfer while the school captures additional revenue from licensing. This is precisely the structure of a Tangled Rope: both coordination and extraction are structurally present. The scheme is not a Snare because the coordination benefit is not illusory. It is not a Rope because the extraction is material and asymmetric (beneficiaries have arbitrage options; victims have trapped/constrained options). The mandatrophy is resolved by recognizing that both functions coexist in the same institutional mechanism, and the indexical classification (Tangled Rope) accurately captures this hybrid nature. The perspectival gap (cost-constrained families see Snare; operators see Rope; organized groups see constrained Tangled Rope) reflects the same underlying structure viewed from different structural positions — exactly what the system is designed to reveal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    data_monetization_scale,
    'What is the actual monetary value extracted from student data per family, and how does it compare to the 42% tuition discount offered?',
    'Audit of school licensing agreements; valuation of data through comparable edtech licensing rates; comparison of discount value to estimated data license revenue',
    'If extraction > discount value: the scheme is predatory (higher effective extraction than offered discount suggests). If extraction < discount value: scheme is subsidy with data monetization component (lower effective extraction than appears).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(data_monetization_scale, empirical, 'Comparison of data monetization value to tuition discount offered').

omega_variable(
    informed_consent_adequacy,
    'Do parents meaningfully understand the scope, duration, and downstream uses of data they are authorizing when they sign enrollment agreements?',
    'Cognitive accessibility testing of consent documents; qualitative interviews with enrolled parents about their understanding; audit of actual data use against disclosed uses',
    'If consent is not informed: extraction mechanism is hidden (suppression increases, classification shifts toward snare for mobile families). If consent is genuine: families have agency and extraction is partially mitigated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(informed_consent_adequacy, empirical, 'Whether parental consent meets informed consent standards').

omega_variable(
    exit_cost_fungibility,
    'For cost-constrained families, is the exit cost of switching schools actually materially different from the exit cost for affluent families, or is the cost-constrained exit exaggerated by loss aversion?',
    'Survey of actual switching costs (application fees, testing, curriculum adjustment time); comparison of actual exit rates between cost-constrained and mobile family cohorts; longitudinal tracking of family satisfaction and retention',
    'If exit costs are symmetric: powerless/trapped classification for cost-constrained families is accurate. If exit costs are actually moderable: families have more agency than snare model suggests, and classification should shift toward tangled rope across cohorts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_cost_fungibility, empirical, 'Whether exit costs for cost-constrained families are genuinely prohibitive').

omega_variable(
    collective_action_sufficiency,
    'Can organized parent coalitions actually compel data practice changes, or is their leverage insufficient given the school''s alternative revenue sources?',
    'Case study of advocacy outcomes in peer institutions; analysis of school''s financial dependence on data licensing revenue vs other sources; documentation of successful and failed parent pressure campaigns',
    'If coalitions have leverage: organized perspective and tangled rope classification are structurally accurate. If leverage is minimal: even organized families are constrained, classification shifts toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_action_sufficiency, empirical, 'Whether parent coalitions have effective leverage over data practices').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ny_private_school_discount, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nyps_tr_t0, ny_private_school_discount, theater_ratio, 0, 0.25).
narrative_ontology:measurement(nyps_tr_t2, ny_private_school_discount, theater_ratio, 2, 0.35).
narrative_ontology:measurement(nyps_tr_t4, ny_private_school_discount, theater_ratio, 4, 0.45).

% Extraction over time
narrative_ontology:measurement(nyps_be_t0, ny_private_school_discount, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(nyps_be_t2, ny_private_school_discount, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(nyps_be_t4, ny_private_school_discount, base_extractiveness, 4, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ny_private_school_discount, resource_allocation).
narrative_ontology:affects_constraint(ny_private_school_discount, education_data_monetization).
narrative_ontology:affects_constraint(ny_private_school_discount, student_behavioral_surveillance).
narrative_ontology:affects_constraint(ny_private_school_discount, information_asymmetry_edtech).

% DUAL FORMULATION NOTE:
% The discount-for-data scheme is downstream of broader education-sector data monetization practices but represents a distinct structural constraint focused on tuition pricing mechanisms. The scheme is upstream of broader surveillance capitalism normalization in education. This constraint story models the specific institutional arrangement (discount exchange) while upstream and downstream stories model the data exploitation ecosystem and surveillance internalization.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ny_private_school_discount, organized, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
