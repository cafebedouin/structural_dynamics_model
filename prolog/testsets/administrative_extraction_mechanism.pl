% ============================================================================
% CONSTRAINT STORY: administrative_extraction_mechanism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_administrative_extraction_mechanism, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: administrative_extraction_mechanism
 *   human_readable: Administrative Extraction Mechanism in Healthcare Delivery
 *   domain: healthcare_economics/organizational_sociology/gender_studies
 *
 * SUMMARY:
 *   The administrative extraction mechanism in healthcare delivery represents
 *   a structural transfer of organizational overhead from institutions to
 *   individual physicians through uncompensated documentation, inbox
 *   management, prior authorization processing, and compliance tasks. This
 *   constraint emerged from the digitization of medical records (EHR mandate
 *   circa 2009) and the expansion of utilization management by payers,
 *   creating a coordination infrastructure that genuinely enables
 *   interoperability and quality measurement while simultaneously extracting
 *   physician labor to subsidize institutional cost reduction. The constraint
 *   exhibits classic Tangled Rope characteristics: genuine coordination
 *   functions (care team communication, evidence-based decision support,
 *   fraud prevention) are inseparable from extractive mechanisms (duplicative
 *   documentation for billing optimization, performative compliance theater,
 *   cost externalization onto physicians). The theater ratio (0.58) reflects
 *   that a substantial portion of administrative tasks serve institutional
 *   liability management and revenue cycle optimization rather than direct
 *   patient care coordination. Measurements show both theater and
 *   extractiveness increasing over the 2014-2024 interval as EHR systems
 *   accumulated features and payer utilization management intensified.
 *
 * KEY AGENTS:
 *   - Practicing Physicians: Primary victims (powerless to moderate / trapped to constrained) — bear uncompensated time extraction; experience ranges from total lock-in (early-career, high debt) to constrained exit options (established physicians with alternative practice models available)
 *   - Patient Care Quality: Secondary victim (powerless/trapped) — abstract collective good that suffers when physician time is diverted from direct care to documentation
 *   - Physician Wellbeing: Secondary victim (powerless/trapped) — burnout and moral injury resulting from administrative burden and loss of professional autonomy
 *   - Healthcare Payers: Primary beneficiaries (institutional/arbitrage) — extract cost savings through utilization management that externalizes processing burden onto physicians
 *   - Hospital Administrators: Primary beneficiaries (institutional/arbitrage) — extract uncompensated physician labor for revenue cycle optimization and regulatory compliance
 *   - EHR Vendors: Secondary beneficiaries (institutional/arbitrage) — profit from feature accumulation and interoperability complexity that increases physician documentation burden
 *   - Compliance Departments: Secondary beneficiaries (institutional/arbitrage) — justify existence through expanding documentation requirements that physicians must fulfill
 *   - Medical Associations: Organized advocates (organized/constrained) — attempt collective action but constrained by fragmented physician employment and regulatory capture
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(administrative_extraction_mechanism, 0.52).
domain_priors:suppression_score(administrative_extraction_mechanism, 0.68).
domain_priors:theater_ratio(administrative_extraction_mechanism, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(administrative_extraction_mechanism, extractiveness, 0.52).
narrative_ontology:constraint_metric(administrative_extraction_mechanism, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(administrative_extraction_mechanism, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(administrative_extraction_mechanism, tangled_rope).
narrative_ontology:human_readable(administrative_extraction_mechanism, "Administrative Extraction Mechanism in Healthcare Delivery").
narrative_ontology:topic_domain(administrative_extraction_mechanism, "healthcare_economics/organizational_sociology/gender_studies").

domain_priors:requires_active_enforcement(administrative_extraction_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(administrative_extraction_mechanism, healthcare_payers).
narrative_ontology:constraint_beneficiary(administrative_extraction_mechanism, hospital_administrators).
narrative_ontology:constraint_beneficiary(administrative_extraction_mechanism, ehr_vendors).
narrative_ontology:constraint_beneficiary(administrative_extraction_mechanism, compliance_departments).
narrative_ontology:constraint_victim(administrative_extraction_mechanism, practicing_physicians).
narrative_ontology:constraint_victim(administrative_extraction_mechanism, patient_care_quality).
narrative_ontology:constraint_victim(administrative_extraction_mechanism, physician_wellbeing).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAPPED PHYSICIAN (SNARE) — Cannot exit without abandoning career investment; faces structural lock-in through licensing requirements, student debt, and specialized training. Administrative burden experienced as pure extraction with minimal coordination benefit. High suppression from professional identity fusion and economic dependency.
constraint_indexing:constraint_classification(administrative_extraction_mechanism, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ADAPTING PHYSICIAN (TANGLED ROPE) — Has some exit options (locum work, concierge practice, part-time arrangements) but faces significant costs. Recognizes genuine coordination functions (quality reporting, care coordination) embedded within extractive overhead. Mixed experience of necessary bureaucracy and rent-seeking.
constraint_indexing:constraint_classification(administrative_extraction_mechanism, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: HEALTHCARE PAYER (ROPE) — Experiences administrative requirements as coordination mechanism for cost control and quality assurance. Prior authorization prevents unnecessary utilization; documentation requirements enable audit and compliance. Net beneficiary of physician time extraction that reduces payer costs.
constraint_indexing:constraint_classification(administrative_extraction_mechanism, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: HOSPITAL ADMINISTRATOR (ROPE) — Administrative burden on physicians enables revenue cycle optimization, regulatory compliance, and liability management. Physician documentation labor is uncompensated organizational overhead that reduces administrative staffing costs. Coordination function genuine from institutional perspective.
constraint_indexing:constraint_classification(administrative_extraction_mechanism, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: MEDICAL ASSOCIATION (TANGLED ROPE) — Organized physician advocacy recognizes both genuine coordination needs (patient safety, quality measurement) and extractive overhead (duplicative documentation, performative compliance). Has some collective bargaining power but constrained by fragmented physician employment and regulatory capture of standard-setting bodies.
constraint_indexing:constraint_classification(administrative_extraction_mechanism, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — Identifies genuine coordination functions (interoperability standards, evidence-based protocols, fraud prevention) layered with extractive mechanisms (rent-seeking by intermediaries, regulatory theater, cost externalization onto physicians). The constraint coordinates necessary information flows while extracting uncompensated labor to subsidize institutional overhead.
constraint_indexing:constraint_classification(administrative_extraction_mechanism, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(administrative_extraction_mechanism_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(administrative_extraction_mechanism, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(administrative_extraction_mechanism, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(administrative_extraction_mechanism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(administrative_extraction_mechanism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Physicians spend 35-50% of work hours on administrative tasks rather than direct patient care, with much of this time uncompensated (salaried physicians) or poorly compensated (fee-for-service physicians whose documentation time is not billable). The extraction is substantial but not maximal — some administrative work genuinely coordinates care, and some physicians have adapted through scribes, part-time arrangements, or concierge models. The value reflects that roughly half of administrative burden is extractive overhead rather than necessary coordination. Suppression (0.68): High. Exit barriers include professional identity fusion (years of training investment), economic dependency (student debt averaging $200k+), licensing requirements that lock physicians into regulated practice settings, and limited alternative career paths that utilize medical training. Suppression is not total — some physicians exit to non-clinical roles, locum work, or early retirement — but the barriers are severe for most. Theater ratio (0.58): Moderate-high. Significant portion of administrative tasks serve institutional goals (billing optimization, liability management, regulatory performance theater) rather than direct patient care coordination. Examples include duplicative documentation across multiple systems, prior authorization for evidence-based treatments, and compliance checkboxes that do not influence clinical decisions. The theater has increased as EHR systems accumulated features and regulatory requirements expanded.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates the classic Tangled Rope perspectival structure. Trapped physicians experience pure extraction (Snare) — administrative burden is uncompensated labor that reduces patient care time and professional autonomy. Physicians with exit options experience mixed coordination and extraction (Tangled Rope) — they recognize genuine care coordination needs while resenting extractive overhead. Healthcare payers and hospital administrators experience coordination (Rope) — administrative requirements serve legitimate institutional functions of cost control, quality assurance, and regulatory compliance. The analytical observer identifies both functions as real: the constraint genuinely coordinates information flows across a fragmented healthcare system while extracting physician labor to subsidize institutional cost reduction. The perspectival gap is not a disagreement about facts but a structural difference in who bears costs and who captures benefits. Institutions externalize administrative costs onto physicians; physicians internalize those costs as uncompensated time. Both perspectives are correct from their structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Physicians are victims with exit options ranging from trapped (early-career, high debt, specialized training) to constrained (established physicians with some practice model flexibility). The engine derives high d values for physician perspectives, producing high effective extraction. Healthcare payers and hospital administrators are beneficiaries with arbitrage exit options — they can shift between organizational forms and regulatory strategies. The engine derives low d values for institutional beneficiaries, producing low or negative effective extraction (they experience the constraint as coordination). The perspectival gap is structural: institutions genuinely coordinate through administrative requirements while simultaneously extracting uncompensated physician labor to subsidize their own overhead costs. The analytical observer sees both functions operating simultaneously — this is the defining characteristic of Tangled Rope.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that coordination and extraction are not mutually exclusive categories but can coexist within the same structural mechanism. The administrative burden genuinely coordinates: EHR systems enable care team communication across settings; prior authorization prevents some unnecessary utilization; quality reporting drives evidence-based practice improvements. These coordination functions are real and measurable. Simultaneously, the administrative burden extracts: physicians perform uncompensated labor that subsidizes institutional overhead; duplicative documentation serves billing optimization rather than care coordination; performative compliance theater satisfies regulatory requirements without improving patient outcomes. The extraction is also real and measurable. The Tangled Rope classification captures this duality: the constraint has both a genuine coordination function (required for the classification) and asymmetric extraction (also required). The mandatrophy dissolves when we recognize that a single mechanism can serve multiple structural functions simultaneously, with different agents experiencing different aspects of that mechanism based on their position in the extraction flow.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_floor_ambiguity,
    'What proportion of administrative burden represents irreducible coordination cost vs extractive overhead that could be eliminated or redistributed?',
    'Comparative analysis of administrative burden across healthcare systems with different organizational structures; time-motion studies decomposing tasks by coordination necessity vs institutional rent-seeking',
    'If coordination floor is high (>40% of current burden): constraint is primarily Rope with modest extraction. If coordination floor is low (<20%): constraint is primarily Snare with thin coordination veneer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_floor_ambiguity, empirical, 'Proportion of administrative burden that is irreducible coordination cost').

omega_variable(
    gender_differential_impact,
    'Does administrative burden disproportionately extract from female physicians through gendered task allocation and communication norms?',
    'Gender-stratified analysis of EHR inbox volume, patient message response rates, and non-clinical task assignment; qualitative research on gendered expectations for emotional labor and care coordination',
    'If gender differential is significant: extractiveness is higher for female physicians, and the constraint has an additional identity-based suppression mechanism beyond professional lock-in.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(gender_differential_impact, empirical, 'Whether administrative extraction is gender-differentiated').

omega_variable(
    automation_substitution_feasibility,
    'What proportion of current administrative tasks could be automated or delegated to non-physician staff without compromising coordination functions?',
    'Pilot studies of AI-assisted documentation, team-based care models with expanded medical assistant roles, and natural language processing for inbox triage; measurement of quality and safety outcomes under alternative task allocation',
    'If high substitutability: constraint has a clear sunset path (Scaffold characteristics). If low substitutability: physician-specific expertise is genuinely required, and extraction is structural rather than contingent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(automation_substitution_feasibility, empirical, 'Feasibility of automating or delegating administrative tasks').

omega_variable(
    burnout_causality_direction,
    'Is administrative burden a primary cause of physician burnout, or is burnout a consequence of broader structural extraction that administrative burden merely indexes?',
    'Longitudinal studies tracking burnout trajectories relative to administrative burden changes; comparison of burnout rates across practice settings with varying administrative loads but similar patient complexity',
    'If administrative burden is causal: reducing it directly improves physician wellbeing. If administrative burden is an index: it reveals deeper extraction mechanisms (loss of autonomy, moral injury, productivity pressure) that persist even when documentation burden is reduced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(burnout_causality_direction, empirical, 'Causal relationship between administrative burden and physician burnout').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(administrative_extraction_mechanism, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(admin_extract_tr_t0, administrative_extraction_mechanism, theater_ratio, 0, 0.35).
narrative_ontology:measurement(admin_extract_tr_t5, administrative_extraction_mechanism, theater_ratio, 5, 0.48).
narrative_ontology:measurement(admin_extract_tr_t10, administrative_extraction_mechanism, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(admin_extract_be_t0, administrative_extraction_mechanism, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(admin_extract_be_t5, administrative_extraction_mechanism, base_extractiveness, 5, 0.43).
narrative_ontology:measurement(admin_extract_be_t10, administrative_extraction_mechanism, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(administrative_extraction_mechanism, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is a single structural mechanism with dual functions (coordination and extraction) rather than a decomposable family. The ε value (0.52) is stable across different observables (time-motion studies, EHR log analysis, physician surveys) because all measure the same underlying phenomenon: the proportion of physician work hours diverted from patient care to administrative tasks.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
