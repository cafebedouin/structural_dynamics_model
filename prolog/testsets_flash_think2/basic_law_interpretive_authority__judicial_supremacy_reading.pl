% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_authority__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_authority__judicial_supremacy_reading, []).

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
 *   constraint_id: basic_law_interpretive_authority__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy over Constitutional Interpretation
 *   domain: constitutional_law/political_theory/institutional_design
 *
 * SUMMARY:
 *   This constraint describes the 'judicial supremacy' reading of basic law
 *   interpretive authority, where courts hold final interpretive power over
 *   constitutional meaning, grounded in specialized legal expertise and
 *   independence from political pressure. This reading is one of several
 *   competing interpretations of how constitutional meaning is established
 *   and maintained. While presented as a mechanism for stable governance and
 *   rights protection, it also involves significant extraction of power from
 *   democratically elected branches.
 *
 * KEY AGENTS:
 *   - Judiciary: Primary agenda_setter, holds final interpretive authority.
 *   - Legislature: Primary payer, subject to judicial review and potential blocking of legislation.
 *   - Electoral Majorities: Payer, can have their democratic will frustrated by judicial rulings.
 *   - Legal Profession: Beneficiary, profits from the complexity and specialized nature of constitutional law.
 *   - Executive Branch: Payer, can have its policies and actions constrained by judicial review.
 *   - Popular Constitutionalism Advocates: Excluded, argue for broader public involvement.
 *   - Parliamentary Sovereignty Advocates: Excluded, argue for legislative supremacy.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_authority__judicial_supremacy_reading, 0.75).
domain_priors:suppression_score(basic_law_interpretive_authority__judicial_supremacy_reading, 0.8).
domain_priors:theater_ratio(basic_law_interpretive_authority__judicial_supremacy_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_authority__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_authority__judicial_supremacy_reading, "Judicial Supremacy over Constitutional Interpretation").
narrative_ontology:topic_domain(basic_law_interpretive_authority__judicial_supremacy_reading, "constitutional_law/political_theory/institutional_design").

domain_priors:requires_active_enforcement(basic_law_interpretive_authority__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_authority__judicial_supremacy_reading, '86e4b726-c7fb-41e4-b56d-e2b4fbecc4ed').
narrative_ontology:cs_kernel_codification('86e4b726-c7fb-41e4-b56d-e2b4fbecc4ed', fixed_text).
narrative_ontology:cs_authority_grounding('86e4b726-c7fb-41e4-b56d-e2b4fbecc4ed', lineage).
narrative_ontology:cs_interpretation_layer_present('86e4b726-c7fb-41e4-b56d-e2b4fbecc4ed').
narrative_ontology:cs_reading_relation('86e4b726-c7fb-41e4-b56d-e2b4fbecc4ed', basic_law_interpretive_authority__parliamentary_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('86e4b726-c7fb-41e4-b56d-e2b4fbecc4ed', basic_law_interpretive_authority__popular_constitutionalism_reading, coexists_with).
narrative_ontology:cs_axiom('86e4b726-c7fb-41e4-b56d-e2b4fbecc4ed', foundational, judicial_impartiality_axiom).
narrative_ontology:cs_axiom_status(judicial_impartiality_axiom, holdable).
narrative_ontology:cs_axiom_grounding('86e4b726-c7fb-41e4-b56d-e2b4fbecc4ed', judicial_impartiality_axiom, deontological).
narrative_ontology:cs_axiom('86e4b726-c7fb-41e4-b56d-e2b4fbecc4ed', foundational, constitutional_supremacy_axiom).
narrative_ontology:cs_axiom_status(constitutional_supremacy_axiom, holdable).
narrative_ontology:cs_axiom_grounding('86e4b726-c7fb-41e4-b56d-e2b4fbecc4ed', constitutional_supremacy_axiom, conventional).
narrative_ontology:cs_reference_frame('86e4b726-c7fb-41e4-b56d-e2b4fbecc4ed', marbury_v_madison_precedent).
narrative_ontology:cs_drift_state('86e4b726-c7fb-41e4-b56d-e2b4fbecc4ed', contemporary_political_polarization, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('86e4b726-c7fb-41e4-b56d-e2b4fbecc4ed', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_authority__judicial_supremacy_reading, basic_law_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__judicial_supremacy_reading, judiciary).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__judicial_supremacy_reading, legal_profession).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__judicial_supremacy_reading, legislature).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__judicial_supremacy_reading, electoral_majorities).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__judicial_supremacy_reading, executive_branch).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds final interpretive authority over constitutional meaning, issuing rulings that bind other branches. Benefits from institutional independence and the prestige of specialized expertise. Actively defends its role through precedent and legal reasoning.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Enacts laws that are subject to judicial review. Its legislative agenda and policy choices can be blocked or overturned by judicial rulings, leading to gridlock or the frustration of electoral mandates. Exit options are limited to constitutional amendment (difficult) or political pressure.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, legislature, payer,
    institutional, biographical, constrained, national).

% Expresses its will through elections, but its policy preferences can be thwarted by judicial decisions that invalidate democratically passed legislation. Experiences a sense of disempowerment when judicial rulings override popular mandates.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, electoral_majorities, payer,
    organized, biographical, constrained, national).

% Benefits from the complexity and specialized nature of constitutional law, which requires their expertise for interpretation, litigation, and advocacy. The judiciary's final authority reinforces the demand for legal professionals.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, legal_profession, beneficiary,
    organized, biographical, mobile, national).

% Responsible for implementing laws and policies, but its actions can be constrained or invalidated by judicial review. Must navigate judicial precedents and rulings, sometimes leading to policy paralysis or redirection.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, executive_branch, payer,
    institutional, biographical, constrained, national).

% Argue that constitutional meaning should primarily emerge from ongoing democratic contestation and popular engagement, rather than being settled by judicial elites. They are excluded from the formal adjudicative process and their arguments are often dismissed as undermining legal stability.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, popular_constitutionalism_advocates, excluded,
    moderate, generational, constrained, national).

% Believe that the elected legislature should retain final interpretive authority over constitutional meaning, reflecting democratic mandate and accountability. Their position is structurally foreclosed by judicial supremacy within this system, leaving them to advocate for systemic change.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, parliamentary_sovereignty_advocates, excluded,
    moderate, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, consistent, and expert interpretation of constitutional meaning, preventing legislative overreach, ensuring rights protection, and offering a final, impartial arbiter for disputes over the basic law.
% TRANSFER_FUNCTION: Transfers final interpretive power over constitutional meaning from elected legislative and executive branches to the judiciary. It also transfers the costs of gridlock, delayed policy implementation, and frustrated electoral mandates to the legislative process and electoral majorities.
% ABSENT_VOICES: Advocates of parliamentary sovereignty and popular constitutionalism are structurally excluded from the final interpretive authority. They would argue for democratic accountability or direct popular engagement in constitutional interpretation, challenging the judiciary's claim to exclusive expertise and impartiality.
% DISAPPEARANCE_RATIONALE: If judicial supremacy vanished overnight, there would be immediate and profound contestation over constitutional meaning. The legislative and executive branches would assert their own interpretive authority, potentially leading to constitutional crises, legislative supremacy, or direct popular referenda on constitutional issues, fundamentally altering the balance of power and the nature of governance.
% FOUNDING_PROBLEM: To establish a stable framework for governance, protect fundamental rights from majoritarian impulses, and provide a final, impartial arbiter of the basic law, preventing arbitrary rule and ensuring legal consistency.
% FOUNDING_PROBLEM_CORROBORATION: The judiciary and many legal scholars attest to the ongoing necessity of an independent arbiter for constitutional stability and rights protection. However, political scientists, some public interest groups, and advocates of alternative constitutional theories argue that the founding problem of majoritarian overreach is overstated or that judicial review has become a tool for partisan outcomes, challenging the impartiality and effectiveness claims. Legislative hearing testimony and academic critiques from outside the benefiting parties support the contested status.
narrative_ontology:disappearance_verdict(basic_law_interpretive_authority__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_authority__judicial_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_authority__judicial_supremacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(basic_law_interpretive_authority__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_authority__judicial_supremacy_reading, 0.75, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_authority__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(basic_law_interpretive_authority__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(basic_law_interpretive_authority__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.75) reflects the judiciary's power to invalidate legislation, effectively extracting policy choices from the legislative process and electoral majorities. Suppression (0.80) is high because the institutional and legal mechanisms (precedent, judicial review) actively suppress alternative interpretations or challenges to judicial authority. The accessibility collapse (0.85) is severe as avenues for overturning judicial constitutional interpretations (e.g., constitutional amendment) are extremely difficult. Resistance (0.50) is moderate, reflecting ongoing political and academic debate, but direct defiance of judicial rulings is rare. Theater ratio (0.40) indicates that while genuine legal reasoning occurs, the performance of judicial impartiality can sometimes mask politically charged outcomes, especially as the institution faces increasing scrutiny.
 *
 * PERSPECTIVAL GAP:
 *   From the judiciary's perspective, this constraint is a necessary 'rope' for stable governance and rights protection, ensuring a consistent and expert interpretation of the basic law. From the perspective of the legislature and electoral majorities, it often functions as a 'snare' or 'tangled rope,' extracting their democratic authority and imposing costs through blocked legislation or policy gridlock. The engine's computation of per-seat classifications will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary and legal profession are clear beneficiaries, gaining institutional authority, prestige, and professional demand. The legislature, electoral majorities, and executive branch are targets, bearing the costs of constrained policy-making and frustrated democratic mandates. The 'excluded' stakeholders are those whose alternative framings of constitutional authority are actively suppressed by this reading's dominance.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate to provide stable, impartial constitutional interpretation is contested. While the judiciary maintains it is fulfilling this role, critics argue that the institution has become politicized, and its decisions reflect ideological biases rather than pure legal expertise. This raises questions about whether the constraint's function has drifted from its original intent, accumulating extraction under the guise of coordination. The 'contested' status of the founding problem directly addresses this potential mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'This constraint is one reading of the ''basic_law_interpretive_authority'' kernel, specifically the ''judicial_supremacy_reading''. What would a sibling reading change structurally?',
    'Analysis of alternative constitutional systems (e.g., parliamentary sovereignty) or historical periods where different readings prevailed. The structural delta would be a shift in the primary agenda-setter for constitutional meaning.',
    'If a ''parliamentary_sovereignty_reading'' were adopted, the legislature would become the primary agenda-setter for constitutional meaning, shifting the judiciary to a ''payer'' or ''observer'' role and reducing extraction from the legislative process. If a ''popular_constitutionalism_reading'' were adopted, direct citizen engagement would gain interpretive authority, potentially diffusing extraction but increasing coordination costs.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Identifies this constraint as a specific reading of a contested kernel and outlines structural changes under sibling readings.').

omega_variable(
    judicial_impartiality_vs_ideology,
    'Is judicial independence truly apolitical and grounded in specialized legal expertise, or does it reflect ideological biases and policy preferences of judges?',
    'Empirical studies of judicial voting patterns, analysis of judicial appointments processes, and examination of the ideological alignment of judicial outcomes with political parties over time.',
    'If judicial decisions are found to be primarily driven by ideology, the ''expertise'' and ''impartiality'' claims underpinning the constraint''s legitimacy would erode, potentially reclassifying it closer to a ''snare'' due to the masking of political power as neutral legal interpretation. This would also amplify the perceived extraction from democratic processes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_impartiality_vs_ideology, empirical, 'Assesses the true nature of judicial decision-making and its impact on legitimacy.').

omega_variable(
    rights_protection_vs_status_quo,
    'Does judicial review genuinely protect minority rights and fundamental liberties, or does it primarily serve to entrench the status quo and established interests?',
    'Longitudinal analysis of judicial outcomes across various social and economic issues, examining whose rights are consistently protected and whose claims are consistently denied, particularly for marginalized groups.',
    'If judicial review is found to disproportionately protect established interests, the ''coordination function'' of rights protection would be undermined, increasing the perceived extraction from vulnerable populations and potentially shifting the constraint''s classification towards a ''snare'' for those groups.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rights_protection_vs_status_quo, empirical, 'Examines the actual beneficiaries of judicial review beyond stated intent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_authority__judicial_supremacy_reading, 1950, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t1950, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 1950, 0.2).
narrative_ontology:measurement(basi_tr_t1960, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 1960, 0.25).
narrative_ontology:measurement(basi_tr_t1970, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 1970, 0.3).
narrative_ontology:measurement(basi_tr_t1980, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 1980, 0.35).
narrative_ontology:measurement(basi_tr_t1990, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 1990, 0.38).
narrative_ontology:measurement(basi_tr_t2000, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 2000, 0.39).
narrative_ontology:measurement(basi_tr_t2010, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 2010, 0.4).
narrative_ontology:measurement(basi_tr_t2020, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 2020, 0.4).

% Extraction over time
narrative_ontology:measurement(basi_be_t1950, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 1950, 0.6).
narrative_ontology:measurement(basi_be_t1960, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 1960, 0.64).
narrative_ontology:measurement(basi_be_t1970, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 1970, 0.68).
narrative_ontology:measurement(basi_be_t1980, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 1980, 0.71).
narrative_ontology:measurement(basi_be_t1990, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 1990, 0.73).
narrative_ontology:measurement(basi_be_t2000, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 2000, 0.74).
narrative_ontology:measurement(basi_be_t2010, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 2010, 0.75).
narrative_ontology:measurement(basi_be_t2020, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 2020, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t1950, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 1950, 0.65).
narrative_ontology:measurement(basi_su_t1960, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 1960, 0.7).
narrative_ontology:measurement(basi_su_t1970, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 1970, 0.74).
narrative_ontology:measurement(basi_su_t1980, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 1980, 0.77).
narrative_ontology:measurement(basi_su_t1990, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 1990, 0.79).
narrative_ontology:measurement(basi_su_t2000, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 2000, 0.8).
narrative_ontology:measurement(basi_su_t2010, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 2010, 0.8).
narrative_ontology:measurement(basi_su_t2020, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 2020, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_authority__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__judicial_supremacy_reading, legislative_process_efficiency).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__judicial_supremacy_reading, executive_policy_implementation).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__judicial_supremacy_reading, constitutional_amendment_process).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'basic_law_interpretive_authority' kernel. The 'judicial_supremacy_reading' focuses on judicial finality, while 'parliamentary_sovereignty_reading' and 'popular_constitutionalism_reading' offer alternative loci of interpretive authority, each with different structural implications for power distribution and extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
