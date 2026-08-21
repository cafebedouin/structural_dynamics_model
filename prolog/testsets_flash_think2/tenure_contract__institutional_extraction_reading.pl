% ============================================================================
% CONSTRAINT STORY: tenure_contract__institutional_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tenure_contract__institutional_extraction_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: tenure_contract__institutional_extraction_reading
 *   human_readable: Tenure as Institutional Rent Extraction
 *   domain: higher_education_governance/labor_economics/institutional_theory
 *
 * SUMMARY:
 *   This constraint story analyzes academic tenure from an 'institutional
 *   extraction' reading, arguing that it functions as a mechanism for early
 *   winners (tenured faculty) to extract permanent rents, creating employment
 *   rigidity that prevents optimal resource reallocation and loads costs onto
 *   contingent labor and students. This reading contrasts sharply with the
 *   traditional 'academic freedom' justification for tenure, highlighting the
 *   divergence between claimed function and actual operation.
 *
 * KEY AGENTS:
 *   - Tenured Faculty: Primary beneficiaries and agenda-setters (institutional/constrained)
 *   - Contingent Faculty: Primary targets/victims (powerless/trapped)
 *   - Students: Cost bearers/victims (moderate/constrained)
 *   - University Administration: Bears rigidity costs, but also administers the system (institutional/constrained)
 *   - Taxpayers/Donors: Indirect cost bearers (organized/mobile)
 *   - Academic Disciplines: Excluded voices for optimal resource allocation (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tenure_contract__institutional_extraction_reading, 0.85).
domain_priors:suppression_score(tenure_contract__institutional_extraction_reading, 0.78).
domain_priors:theater_ratio(tenure_contract__institutional_extraction_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tenure_contract__institutional_extraction_reading, snare).
narrative_ontology:human_readable(tenure_contract__institutional_extraction_reading, "Tenure as Institutional Rent Extraction").
narrative_ontology:topic_domain(tenure_contract__institutional_extraction_reading, "higher_education_governance/labor_economics/institutional_theory").

domain_priors:requires_active_enforcement(tenure_contract__institutional_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tenure_contract__institutional_extraction_reading, 'c8d9e6ea-e426-4cdd-9e31-7e32784f5931').
narrative_ontology:cs_kernel_codification('c8d9e6ea-e426-4cdd-9e31-7e32784f5931', formalized).
narrative_ontology:cs_authority_grounding('c8d9e6ea-e426-4cdd-9e31-7e32784f5931', extraction).
narrative_ontology:cs_interpretation_layer_present('c8d9e6ea-e426-4cdd-9e31-7e32784f5931').
narrative_ontology:cs_reading_relation('c8d9e6ea-e426-4cdd-9e31-7e32784f5931', tenure_contract__academic_freedom_reading, coexists_with).
narrative_ontology:cs_reading_relation('c8d9e6ea-e426-4cdd-9e31-7e32784f5931', tenure_contract__demographic_reproduction_reading, coexists_with).
narrative_ontology:cs_axiom('c8d9e6ea-e426-4cdd-9e31-7e32784f5931', foundational, tenure_as_permanent_claim_on_resources).
narrative_ontology:cs_axiom_status(tenure_as_permanent_claim_on_resources, holdable).
narrative_ontology:cs_axiom_grounding('c8d9e6ea-e426-4cdd-9e31-7e32784f5931', tenure_as_permanent_claim_on_resources, conventional).
narrative_ontology:cs_axiom('c8d9e6ea-e426-4cdd-9e31-7e32784f5931', foundational, labor_market_rigidity_is_inefficient).
narrative_ontology:cs_axiom_status(labor_market_rigidity_is_inefficient, holdable).
narrative_ontology:cs_axiom_grounding('c8d9e6ea-e426-4cdd-9e31-7e32784f5931', labor_market_rigidity_is_inefficient, empirically_contingent).
narrative_ontology:cs_reference_frame('c8d9e6ea-e426-4cdd-9e31-7e32784f5931', post_1940_aaup_statement).
narrative_ontology:cs_drift_state('c8d9e6ea-e426-4cdd-9e31-7e32784f5931', contemporary_contingent_labor_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('c8d9e6ea-e426-4cdd-9e31-7e32784f5931', '').
narrative_ontology:cs_kernel_id(tenure_contract__institutional_extraction_reading, tenure_contract).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tenure_contract__institutional_extraction_reading, tenured_faculty).
narrative_ontology:constraint_victim(tenure_contract__institutional_extraction_reading, contingent_faculty).
narrative_ontology:constraint_victim(tenure_contract__institutional_extraction_reading, students).
narrative_ontology:constraint_victim(tenure_contract__institutional_extraction_reading, university_administration).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(tenure_contract__institutional_extraction_reading, taxpayers_donors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold permanent positions, high salaries, and significant influence over university governance and resource allocation. They benefit from the stability and prestige of tenure, which is largely insulated from market forces or performance reviews. Their exit options are limited by career path dependence, but their position is highly secure.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, tenured_faculty, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(tenure_contract__institutional_extraction_reading, tenured_faculty, beneficiary).

% Bear the costs of employment rigidity, working on short-term contracts with lower pay, fewer benefits, and no job security. They perform a significant portion of teaching and research but have minimal influence and limited career progression within the tenured system. Their exit options are severely limited by specialized skills and a saturated academic job market.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, contingent_faculty, payer,
    powerless, immediate, trapped, local).

% Pay high tuition fees that subsidize the tenured system, often receiving instruction from underpaid contingent faculty. They bear the cost of reduced institutional flexibility and resource reallocation, which can lead to fewer course offerings in emerging fields or less investment in innovative teaching methods. Their exit options are constrained by the sunk cost of education and limited alternatives.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, students, payer,
    moderate, biographical, constrained, local).

% Administers the tenure system and bears the costs of its rigidity, struggling to reallocate resources to new disciplines or respond to changing student demand due to fixed labor costs. While they set some policies, their ability to reform tenure is severely limited by tenured faculty power. Their exit options are constrained by institutional inertia and political resistance.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, university_administration, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(tenure_contract__institutional_extraction_reading, university_administration, agenda_setter).

% Indirectly subsidize the tenure system through public funding and charitable contributions. They bear the cost of an inefficient allocation of educational resources and a system that may not optimally serve public interest or evolving societal needs. Their exit options involve shifting funding priorities or advocating for policy changes.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, taxpayers_donors, payer,
    organized, generational, mobile, national).

% Represent the evolving intellectual landscape and optimal allocation of scholarly resources. They would advocate for flexibility to invest in new fields and divest from atrophied ones, but are excluded from the resource allocation decisions driven by tenure rigidity.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, academic_disciplines, excluded,
    analytical, generational, analytical, universal).
narrative_ontology:stakeholder_non_agent(tenure_contract__institutional_extraction_reading, academic_disciplines).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tenure_contract__institutional_extraction_reading, tenured_faculty).
narrative_ontology:fixing_cost_class(tenure_contract__institutional_extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Originally, to ensure academic freedom and attract top talent, fostering long-term research and teaching stability by protecting scholars from institutional or political interference.
% TRANSFER_FUNCTION: Transfers significant financial resources (salaries, benefits, research funds) and job security from university budgets (ultimately students, taxpayers, and donors) to tenured faculty, while transferring precarity and labor flexibility costs to contingent faculty.
% ABSENT_VOICES: Future generations of scholars, academic disciplines advocating for resource reallocation based on evolving intellectual needs, and a fully empowered university administration capable of strategic resource management are largely excluded from the conversation about tenure reform.
% DISAPPEARANCE_RATIONALE: If tenure vanished overnight, the entire academic labor market, university financial models, and governance structures would undergo a radical and immediate reorganization. Universities would gain immense flexibility in hiring and resource allocation, while the academic job market would become fully market-driven, likely leading to significant shifts in compensation and job security for all faculty.
% FOUNDING_PROBLEM: The founding problem was to protect academic freedom from political and institutional interference, ensuring scholars could pursue controversial research and express unpopular views without fear of reprisal, thereby attracting and retaining top intellectual talent.
% FOUNDING_PROBLEM_CORROBORATION: Critics of the tenure system (e.g., labor economists, institutional theorists, contingent faculty advocates) attest that the original problem is largely addressed, and the arrangement persists as rent collection; legislative-hearing testimony and independent economic analysis from outside the benefiting parties support the shifted-function reading.
narrative_ontology:disappearance_verdict(tenure_contract__institutional_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(tenure_contract__institutional_extraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tenure_contract__institutional_extraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(tenure_contract__institutional_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tenure_contract__institutional_extraction_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tenure_contract__institutional_extraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(tenure_contract__institutional_extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(tenure_contract__institutional_extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because tenured faculty secure permanent claims on resources (salaries, benefits, research funds) largely decoupled from contemporary market value or performance, at the expense of other stakeholders. Suppression is high (0.78) due to the structural barriers to entry for contingent faculty, the lack of alternatives in a highly specialized labor market, and the institutional power of tenured faculty to resist reform. Theater ratio is moderate (0.45) as the 'academic freedom' justification is still invoked, but a significant portion of institutional activity is dedicated to maintaining the existing power structure and defending against challenges to tenure's economic function. Accessibility collapse is high for contingent faculty, as the path to a secure academic career is largely closed off, while resistance is moderate from various groups advocating for reform.
 *
 * PERSPECTIVAL GAP:
 *   Tenured faculty experience this constraint as a protective 'rope' ensuring stability and intellectual freedom, while contingent faculty and students experience it as a 'snare' of extraction and precarity. University administration is caught between the two, bearing the costs of rigidity while being constrained in their ability to enact change. The engine's per-seat classification will highlight these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   Tenured faculty are clear beneficiaries (low d) as they directly collect rents and control the system. Contingent faculty and students are clear targets (high d) as they bear the costs of rigidity and extraction. University administration is a complex payer/agenda-setter, bearing costs but also perpetuating the system. Taxpayers/donors are indirect payers. Rival academic disciplines are excluded, their optimal resource allocation arguments suppressed by the existing structure.
 *
 * MANDATROPHY ANALYSIS:
 *   From this reading's perspective, the original mandate of tenure (protecting academic freedom) has largely atrophied or been superseded. The constraint persists not primarily due to its coordination function, but because it serves as a mechanism for institutionalized rent extraction by a powerful incumbent group. The classification as a 'snare' reflects this shift, preventing mislabeling it as a 'rope' or 'scaffold' based on its historical or claimed function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tenure_function_ambiguity,
    'Is the primary function of tenure to protect academic freedom, or has it become primarily a mechanism for institutionalized rent extraction and labor market rigidity?',
    'Comparative institutional analysis of universities with and without tenure, examining academic output, innovation, and labor market dynamics, alongside historical analysis of tenure''s evolution and its correlation with the rise of contingent faculty.',
    'If primarily rent extraction, the constraint is a Snare; if primarily academic freedom, it is closer to a Rope or Scaffold. The resolution would significantly alter the classification and policy recommendations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tenure_function_ambiguity, conceptual, 'Ambiguity regarding tenure''s core function: academic freedom vs. rent extraction.').

omega_variable(
    resource_reallocation_efficiency,
    'To what extent does tenure-induced employment rigidity prevent optimal resource reallocation within universities, hindering adaptation to evolving academic fields and student demand?',
    'Economic modeling comparing resource allocation efficiency in tenured vs. non-tenured academic systems, measuring responsiveness to changes in research priorities, student enrollment patterns, and emerging disciplines.',
    'Higher proven inefficiency strengthens the extraction reading and the Snare classification, indicating significant costs borne by the institution and students due to rigidity. Lower inefficiency would weaken this aspect of the extraction claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_reallocation_efficiency, empirical, 'Impact of tenure on university resource allocation efficiency.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression experienced by contingent faculty primarily structural (contractual terms, lack of institutional power) or internalized (identity-locked into academia, fear of blacklisting)?',
    'Post-exit career trajectory analysis for contingent faculty: if precarity and limited options persist after leaving the tenured system, it suggests internalized suppression. Surveys and qualitative interviews exploring career path dependence and psychological costs.',
    'If internalized suppression is significant, the effective suppression of the constraint is higher than structural measures suggest, as contingent faculty carry the suppression with them, making exit even more difficult.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for contingent faculty.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tenure_contract__institutional_extraction_reading, 1980, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tenu_tr_t1980, tenure_contract__institutional_extraction_reading, theater_ratio, 1980, 0.2).
narrative_ontology:measurement(tenu_tr_t1990, tenure_contract__institutional_extraction_reading, theater_ratio, 1990, 0.28).
narrative_ontology:measurement(tenu_tr_t2000, tenure_contract__institutional_extraction_reading, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(tenu_tr_t2010, tenure_contract__institutional_extraction_reading, theater_ratio, 2010, 0.4).
narrative_ontology:measurement(tenu_tr_t2020, tenure_contract__institutional_extraction_reading, theater_ratio, 2020, 0.44).
narrative_ontology:measurement(tenu_tr_t2023, tenure_contract__institutional_extraction_reading, theater_ratio, 2023, 0.45).

% Extraction over time
narrative_ontology:measurement(tenu_be_t1980, tenure_contract__institutional_extraction_reading, base_extractiveness, 1980, 0.6).
narrative_ontology:measurement(tenu_be_t1990, tenure_contract__institutional_extraction_reading, base_extractiveness, 1990, 0.7).
narrative_ontology:measurement(tenu_be_t2000, tenure_contract__institutional_extraction_reading, base_extractiveness, 2000, 0.78).
narrative_ontology:measurement(tenu_be_t2010, tenure_contract__institutional_extraction_reading, base_extractiveness, 2010, 0.82).
narrative_ontology:measurement(tenu_be_t2020, tenure_contract__institutional_extraction_reading, base_extractiveness, 2020, 0.84).
narrative_ontology:measurement(tenu_be_t2023, tenure_contract__institutional_extraction_reading, base_extractiveness, 2023, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(tenu_su_t1980, tenure_contract__institutional_extraction_reading, suppression_requirement, 1980, 0.55).
narrative_ontology:measurement(tenu_su_t1990, tenure_contract__institutional_extraction_reading, suppression_requirement, 1990, 0.65).
narrative_ontology:measurement(tenu_su_t2000, tenure_contract__institutional_extraction_reading, suppression_requirement, 2000, 0.72).
narrative_ontology:measurement(tenu_su_t2010, tenure_contract__institutional_extraction_reading, suppression_requirement, 2010, 0.75).
narrative_ontology:measurement(tenu_su_t2020, tenure_contract__institutional_extraction_reading, suppression_requirement, 2020, 0.77).
narrative_ontology:measurement(tenu_su_t2023, tenure_contract__institutional_extraction_reading, suppression_requirement, 2023, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tenure_contract__institutional_extraction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(tenure_contract__institutional_extraction_reading, university_budget_allocation).
narrative_ontology:affects_constraint(tenure_contract__institutional_extraction_reading, contingent_labor_precarity).
narrative_ontology:affects_constraint(tenure_contract__institutional_extraction_reading, academic_job_market_structure).

% DUAL FORMULATION NOTE:
% This is one of three readings of the 'tenure_contract' kernel. This 'institutional_extraction_reading' focuses on the economic and labor market consequences, while the 'academic_freedom_reading' emphasizes intellectual protection and the 'demographic_reproduction_reading' focuses on social reproduction through peer review. Each reading yields a distinct constraint with different epsilon values and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
