% ============================================================================
% CONSTRAINT STORY: tenure_contract__academic_freedom_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tenure_contract__academic_freedom_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: tenure_contract__academic_freedom_reading
 *   human_readable: Academic Freedom via Tenure Contract
 *   domain: higher_education_governance/labor_economics/institutional_theory
 *
 * SUMMARY:
 *   This constraint story models the tenure contract from the perspective of
 *   academic freedom, where it functions as a protective mechanism for
 *   truth-seeking against external pressures. It is one reading of the
 *   'tenure_contract' kernel. From this reading, tenure is a coordination
 *   mechanism that benefits faculty, students, and public discourse by
 *   enabling independent research and teaching. The costs are borne by
 *   external political actors and institutional administrators who face
 *   limitations on their ability to control academic output.
 *
 * KEY AGENTS:
 *   - tenured_faculty: Primary beneficiary (powerful/constrained) — protected from arbitrary dismissal.
 *   - students: Beneficiary (moderate/mobile) — access to diverse, independent knowledge.
 *   - public_discourse: Beneficiary (analytical/analytical) — benefits from independent knowledge generation.
 *   - external_political_actors: Primary target (institutional/constrained) — limited ability to suppress research.
 *   - institutional_administrators: Target (institutional/constrained) — reduced flexibility in personnel management.
 *   - contingent_faculty: Excluded (powerless/trapped) — lack tenure protections, excluded from policy discussions.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tenure_contract__academic_freedom_reading, 0.15).
domain_priors:suppression_score(tenure_contract__academic_freedom_reading, 0.1).
domain_priors:theater_ratio(tenure_contract__academic_freedom_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tenure_contract__academic_freedom_reading, rope).
narrative_ontology:human_readable(tenure_contract__academic_freedom_reading, "Academic Freedom via Tenure Contract").
narrative_ontology:topic_domain(tenure_contract__academic_freedom_reading, "higher_education_governance/labor_economics/institutional_theory").

domain_priors:requires_active_enforcement(tenure_contract__academic_freedom_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tenure_contract__academic_freedom_reading, 'af953fca-17ab-4f99-a831-b59220f3f5e2').
narrative_ontology:cs_kernel_codification('af953fca-17ab-4f99-a831-b59220f3f5e2', formalized).
narrative_ontology:cs_authority_grounding('af953fca-17ab-4f99-a831-b59220f3f5e2', lineage).
narrative_ontology:cs_interpretation_layer_present('af953fca-17ab-4f99-a831-b59220f3f5e2').
narrative_ontology:cs_reading_relation('af953fca-17ab-4f99-a831-b59220f3f5e2', tenure_contract__institutional_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('af953fca-17ab-4f99-a831-b59220f3f5e2', tenure_contract__demographic_reproduction_reading, coexists_with).
narrative_ontology:cs_axiom('af953fca-17ab-4f99-a831-b59220f3f5e2', foundational, academic_freedom_is_foundational_to_truth_seeking).
narrative_ontology:cs_axiom_status(academic_freedom_is_foundational_to_truth_seeking, holdable).
narrative_ontology:cs_axiom_grounding('af953fca-17ab-4f99-a831-b59220f3f5e2', academic_freedom_is_foundational_to_truth_seeking, deontological).
narrative_ontology:cs_axiom('af953fca-17ab-4f99-a831-b59220f3f5e2', foundational, tenure_is_the_primary_mechanism_for_academic_freedom).
narrative_ontology:cs_axiom_status(tenure_is_the_primary_mechanism_for_academic_freedom, holdable).
narrative_ontology:cs_axiom_grounding('af953fca-17ab-4f99-a831-b59220f3f5e2', tenure_is_the_primary_mechanism_for_academic_freedom, instrumental).
narrative_ontology:cs_reference_frame('af953fca-17ab-4f99-a831-b59220f3f5e2', post_1940_aa_up_statement_of_principles).
narrative_ontology:cs_drift_state('af953fca-17ab-4f99-a831-b59220f3f5e2', contemporary_higher_education_crisis, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('af953fca-17ab-4f99-a831-b59220f3f5e2', '').
narrative_ontology:cs_kernel_id(tenure_contract__academic_freedom_reading, tenure_contract).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tenure_contract__academic_freedom_reading, tenured_faculty).
narrative_ontology:constraint_beneficiary(tenure_contract__academic_freedom_reading, students).
narrative_ontology:constraint_beneficiary(tenure_contract__academic_freedom_reading, public_discourse).
narrative_ontology:constraint_victim(tenure_contract__academic_freedom_reading, external_political_actors).
narrative_ontology:constraint_victim(tenure_contract__academic_freedom_reading, institutional_administrators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Protected from arbitrary dismissal, enabling them to pursue controversial research and express unpopular opinions without fear of job loss. This stability allows for long-term, high-risk projects. While exit is possible, the benefits of tenure make it a high-cost decision.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, tenured_faculty, beneficiary,
    powerful, biographical, constrained, national).

% Benefit from access to diverse perspectives, cutting-edge research, and an environment where faculty are free to teach and inquire without undue external pressure. This enhances the quality and breadth of their education.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, students, beneficiary,
    moderate, immediate, mobile, local).

% Benefits from the independent generation and dissemination of knowledge, including critical analysis of societal issues, which might otherwise be suppressed by political or economic interests. This contributes to a more informed and robust public sphere.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, public_discourse, beneficiary,
    analytical, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(tenure_contract__academic_freedom_reading, public_discourse).

% Bear the 'cost' of being unable to easily suppress research or opinions that challenge their agendas. They face institutional and legal barriers to influencing academic content or personnel decisions, which limits their direct control over universities.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, external_political_actors, payer,
    institutional, immediate, constrained, national).

% Bear the cost of reduced flexibility in personnel management and resource allocation. They cannot easily dismiss tenured faculty for financial reasons or institutional restructuring, which can lead to rigidity in academic departments. They also manage the political backlash from external actors.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, institutional_administrators, payer,
    institutional, biographical, constrained, national).

% Are not covered by tenure protections and often perform similar teaching and research duties with significantly less job security and lower pay. They would advocate for broader protections or alternative career paths but are largely excluded from the governance structures that determine tenure policy.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, contingent_faculty, excluded,
    powerless, immediate, trapped, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the pursuit of knowledge by protecting researchers from short-term political and institutional pressures, ensuring that inquiry can follow truth wherever it leads, even into uncomfortable territory.
% TRANSFER_FUNCTION: Transfers job security and intellectual autonomy to tenured faculty, in exchange for their commitment to long-term, high-quality research and teaching, and the public good of independent knowledge.
% ABSENT_VOICES: Contingent faculty, who bear the precarity that tenure insulates tenured faculty from, are largely absent from the policy-making discussions around tenure. They would argue for a more equitable distribution of job security.
% DISAPPEARANCE_RATIONALE: If tenure vanished overnight, academic institutions would quickly become more susceptible to political and economic pressures. Research agendas would shift towards safer, more fundable topics, and faculty would self-censor, leading to a significant decline in independent truth-seeking and critical inquiry.
% FOUNDING_PROBLEM: Universities were historically vulnerable to external political and religious interference, leading to the suppression of inconvenient truths and the dismissal of scholars for unpopular views.
% FOUNDING_PROBLEM_CORROBORATION: Academic freedom organizations, faculty unions, and historical analyses of academic suppression attest that the problem of external interference remains live, citing ongoing political attacks on universities and specific faculty members. This corroboration comes from sources outside the direct beneficiaries of tenure.
narrative_ontology:disappearance_verdict(tenure_contract__academic_freedom_reading, world_rearranges).
narrative_ontology:founding_problem_status(tenure_contract__academic_freedom_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tenure_contract__academic_freedom_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(tenure_contract__academic_freedom_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tenure_contract__academic_freedom_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tenure_contract__academic_freedom_reading_tests).
:- end_tests(tenure_contract__academic_freedom_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because, from this reading, the primary function is protection and coordination, not rent extraction. The 'costs' borne by external actors are the price of academic independence, not a transfer of wealth. Suppression is also low (0.1) as the constraint's purpose is to *prevent* suppression of academic inquiry. Accessibility collapse is high (0.7) because the alternatives to tenure (e.g., short-term contracts without protections) are seen as fundamentally undermining academic freedom. Resistance is low (0.05) from within the academic freedom framework, as the constraint is largely accepted as necessary. The slight increase in extractiveness and suppression over time reflects growing external pressures on higher education and the increasing precarity of non-tenured faculty, even within this reading.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of tenured faculty, the constraint is a pure Rope, providing essential protection. From the perspective of external political actors, it is a Snare that prevents them from exercising legitimate oversight. The engine will compute these divergences based on the declared roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Tenured faculty are clear beneficiaries (d=0.0-0.1) as they gain job security and intellectual freedom. Students and public discourse are also beneficiaries (d=0.1-0.2) through access to independent knowledge. External political actors and institutional administrators are targets (d=0.8-0.9) as their ability to control or influence academic affairs is constrained. Contingent faculty are excluded, meaning their interests are not directly served by this specific reading of the tenure contract, and they bear the costs of its rigidity without its benefits.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading argues that the founding problem (vulnerability to external interference) is still live, preventing mandatrophy. The classification as a Rope, with low extractiveness and suppression, reflects the view that tenure continues to serve its original coordination function effectively, rather than having atrophied into a mere performance or extraction mechanism. The low theater ratio supports this, indicating that the protective function is genuine and not merely ceremonial.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tenure_as_property_right_vs_academic_freedom,
    'Is tenure primarily a property right for individual faculty members, or a mechanism to secure academic freedom for the institution and public good?',
    'Legal analysis of court rulings on tenure cases, particularly those involving dismissal for cause vs. financial exigency. Examination of institutional policies on faculty governance and intellectual property.',
    'If primarily a property right, the ''extraction'' from administrators (in terms of flexibility) might be reclassified as a cost of a labor contract, shifting the constraint towards a more neutral or even extractive classification from the institutional seat. If primarily for academic freedom, the Rope classification holds more strongly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tenure_as_property_right_vs_academic_freedom, conceptual, 'Ambiguity in the fundamental nature of tenure''s protection.').

omega_variable(
    effectiveness_of_academic_freedom_protection,
    'How effective is tenure in actually protecting academic freedom against contemporary forms of institutional displeasure or political backlash, especially in an era of declining public funding and increasing political polarization?',
    'Empirical studies tracking instances of attempted interference, faculty self-censorship rates, and the outcomes of academic freedom disputes across different institutional contexts. Comparative analysis with non-tenure systems.',
    'If tenure''s effectiveness is found to be significantly diminished, the ''accessibility_collapse'' for alternatives might be lower, and the ''resistance'' higher, potentially shifting the classification towards a Piton (if the function is mostly theatrical) or a Snare (if it primarily extracts from faculty by creating a false sense of security).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effectiveness_of_academic_freedom_protection, empirical, 'Empirical efficacy of tenure in its stated purpose.').

omega_variable(
    kernel_reading_divergence,
    'Given the ''tenure_contract'' kernel, how do the ''academic_freedom_reading'', ''institutional_extraction_reading'', and ''demographic_reproduction_reading'' structurally diverge, and what are the implications for policy interventions?',
    'Comparative analysis of the stakeholder maps, beneficiary/victim declarations, and metric profiles generated for each reading. Identification of common and divergent policy levers across readings.',
    'Understanding the structural divergence is crucial for designing effective policy interventions. If the academic freedom reading is dominant, policies might focus on strengthening tenure protections. If the extraction or reproduction readings are dominant, policies might focus on labor reform or diversity initiatives. The choice of reading dictates the problem definition and solution space.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'Structural differences between competing readings of the tenure contract kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tenure_contract__academic_freedom_reading, 1940, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tenu_tr_t1940, tenure_contract__academic_freedom_reading, theater_ratio, 1940, 0.05).
narrative_ontology:measurement(tenu_tr_t1960, tenure_contract__academic_freedom_reading, theater_ratio, 1960, 0.03).
narrative_ontology:measurement(tenu_tr_t1980, tenure_contract__academic_freedom_reading, theater_ratio, 1980, 0.02).
narrative_ontology:measurement(tenu_tr_t2000, tenure_contract__academic_freedom_reading, theater_ratio, 2000, 0.03).
narrative_ontology:measurement(tenu_tr_t2024, tenure_contract__academic_freedom_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(tenu_be_t1940, tenure_contract__academic_freedom_reading, base_extractiveness, 1940, 0.1).
narrative_ontology:measurement(tenu_be_t1960, tenure_contract__academic_freedom_reading, base_extractiveness, 1960, 0.08).
narrative_ontology:measurement(tenu_be_t1980, tenure_contract__academic_freedom_reading, base_extractiveness, 1980, 0.07).
narrative_ontology:measurement(tenu_be_t2000, tenure_contract__academic_freedom_reading, base_extractiveness, 2000, 0.1).
narrative_ontology:measurement(tenu_be_t2024, tenure_contract__academic_freedom_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(tenu_su_t1940, tenure_contract__academic_freedom_reading, suppression_requirement, 1940, 0.15).
narrative_ontology:measurement(tenu_su_t1960, tenure_contract__academic_freedom_reading, suppression_requirement, 1960, 0.1).
narrative_ontology:measurement(tenu_su_t1980, tenure_contract__academic_freedom_reading, suppression_requirement, 1980, 0.08).
narrative_ontology:measurement(tenu_su_t2000, tenure_contract__academic_freedom_reading, suppression_requirement, 2000, 0.09).
narrative_ontology:measurement(tenu_su_t2024, tenure_contract__academic_freedom_reading, suppression_requirement, 2024, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tenure_contract__academic_freedom_reading, identity_coordination).
narrative_ontology:affects_constraint(tenure_contract__academic_freedom_reading, university_governance_structures).
narrative_ontology:affects_constraint(tenure_contract__academic_freedom_reading, research_funding_allocation).
narrative_ontology:affects_constraint(tenure_contract__academic_freedom_reading, faculty_hiring_practices).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'tenure_contract' kernel. The other readings are 'institutional_extraction_reading' and 'demographic_reproduction_reading', each with distinct structural properties and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
