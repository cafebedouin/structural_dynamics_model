% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__hindu_codified_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority_kernel__hindu_codified_reading, []).

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
 *   constraint_id: marriage_authority_kernel__hindu_codified_reading
 *   human_readable: Hindu Marriage Law Authority (Codified Reading)
 *   domain: comparative_law/constitutional_pluralism/religious_governance
 *
 * SUMMARY:
 *   This constraint describes the authority of marriage and family law for
 *   Hindus in India, deriving from the Hindu Marriage Act 1955 as interpreted
 *   by civil courts. It is one reading of the broader
 *   'marriage_authority_kernel' which encompasses India's plural legal
 *   system. The Act aimed to codify and reform diverse Hindu customary laws,
 *   introducing some modernization and uniformity, but its application by
 *   civil courts continues to navigate tensions between traditional religious
 *   norms, evolving social expectations, and constitutional principles of
 *   equality.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__hindu_codified_reading, 0.62).
domain_priors:suppression_score(marriage_authority_kernel__hindu_codified_reading, 0.75).
domain_priors:theater_ratio(marriage_authority_kernel__hindu_codified_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__hindu_codified_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__hindu_codified_reading, "Hindu Marriage Law Authority (Codified Reading)").
narrative_ontology:topic_domain(marriage_authority_kernel__hindu_codified_reading, "comparative_law/constitutional_pluralism/religious_governance").

domain_priors:requires_active_enforcement(marriage_authority_kernel__hindu_codified_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__hindu_codified_reading, 'bff75725-8fb9-43aa-bbfc-9376d68416bf').
narrative_ontology:cs_kernel_codification('bff75725-8fb9-43aa-bbfc-9376d68416bf', formalized).
narrative_ontology:cs_authority_grounding('bff75725-8fb9-43aa-bbfc-9376d68416bf', lineage).
narrative_ontology:cs_interpretation_layer_present('bff75725-8fb9-43aa-bbfc-9376d68416bf').
narrative_ontology:cs_reading_relation('bff75725-8fb9-43aa-bbfc-9376d68416bf', marriage_authority_kernel__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('bff75725-8fb9-43aa-bbfc-9376d68416bf', marriage_authority_kernel__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('bff75725-8fb9-43aa-bbfc-9376d68416bf', marriage_authority_kernel__parsi_communal_reading, coexists_with).
narrative_ontology:cs_reading_relation('bff75725-8fb9-43aa-bbfc-9376d68416bf', marriage_authority_kernel__secular_civil_reading, coexists_with).
narrative_ontology:cs_axiom('bff75725-8fb9-43aa-bbfc-9376d68416bf', foundational, marriage_as_sacrament_within_hindu_dharma).
narrative_ontology:cs_axiom_status(marriage_as_sacrament_within_hindu_dharma, holdable).
narrative_ontology:cs_axiom_grounding('bff75725-8fb9-43aa-bbfc-9376d68416bf', marriage_as_sacrament_within_hindu_dharma, theological).
narrative_ontology:cs_axiom('bff75725-8fb9-43aa-bbfc-9376d68416bf', foundational, state_codification_for_uniformity_and_justice).
narrative_ontology:cs_axiom_status(state_codification_for_uniformity_and_justice, holdable).
narrative_ontology:cs_axiom_grounding('bff75725-8fb9-43aa-bbfc-9376d68416bf', state_codification_for_uniformity_and_justice, conventional).
narrative_ontology:cs_reference_frame('bff75725-8fb9-43aa-bbfc-9376d68416bf', post_independence_uniformity_and_reform).
narrative_ontology:cs_drift_state('bff75725-8fb9-43aa-bbfc-9376d68416bf', contemporary_gender_equity_and_uniform_civil_code_debates, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('bff75725-8fb9-43aa-bbfc-9376d68416bf', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__hindu_codified_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__hindu_codified_reading, hindu_community_leaders).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__hindu_codified_reading, hindu_men).
narrative_ontology:constraint_victim(marriage_authority_kernel__hindu_codified_reading, hindu_women).
narrative_ontology:constraint_victim(marriage_authority_kernel__hindu_codified_reading, individuals_seeking_secular_marriage).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the codified law providing a stable framework for community identity and social order, allowing them to maintain influence within the legal structure. They advocate for interpretations that align with traditional values while operating within the state's legal system.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, hindu_community_leaders, agenda_setter,
    institutional, generational, constrained, national).

% Generally benefit from the stability and traditional gender roles often reinforced by interpretations of the codified law, though they are also bound by its provisions. Their options for marriage are primarily within this framework or the Special Marriage Act.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, hindu_men, beneficiary,
    moderate, biographical, constrained, national).

% Bear the costs of gender inequities that persist within the codified law, despite reforms. While the law provides legal recognition and some protections, it often falls short of full gender equality compared to secular alternatives. Their ability to challenge or exit the system is limited by social and legal structures.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, hindu_women, payer,
    powerless, biographical, constrained, national).

% Are tasked with interpreting and enforcing the Hindu Marriage Act 1955. They balance statutory text, legal precedent, and evolving constitutional principles, acting as the primary adjudicators of disputes arising under this law.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, civil_courts, agenda_setter,
    institutional, generational, analytical, national).

% While the Special Marriage Act 1954 offers a secular alternative, individuals identifying as Hindu often face social pressure or legal complexities if they choose to marry outside the Hindu Marriage Act. They are effectively excluded from a fully secular framework if they wish to retain their community identity.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, individuals_seeking_secular_marriage, excluded,
    powerless, biographical, constrained, national).

% Analyze the interplay between personal laws, constitutional rights, and the push for a Uniform Civil Code. They observe the constraint's operation and its implications for legal pluralism and individual liberties.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, constitutional_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority_kernel__hindu_codified_reading, hindu_community_leaders).
narrative_ontology:fixing_cost_class(marriage_authority_kernel__hindu_codified_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a uniform, state-recognized legal framework for marriage, divorce, and family matters for individuals identifying as Hindu, replacing diverse customary laws and ensuring legal certainty within the community.
% TRANSFER_FUNCTION: Transfers legal authority over Hindu family matters from diverse customary practices to a codified state law interpreted by civil courts. It also implicitly transfers some traditional patriarchal authority to the state's legal system, while still allowing for some gender asymmetry in practice.
% ABSENT_VOICES: Advocates for a fully secular Uniform Civil Code are often marginalized in debates focused on personal laws, as are those seeking more radical gender equality reforms that challenge the foundational premises of religious personal laws.
% DISAPPEARANCE_RATIONALE: If the Hindu Marriage Act and its enforcement vanished overnight, it would create immense legal chaos for millions of Hindu families, invalidating marriages, divorces, and inheritance claims. The legal system would be forced to revert to diverse, often uncodified, customary laws or rapidly implement a new framework.
% FOUNDING_PROBLEM: The Hindu Marriage Act 1955 was enacted to codify and reform the diverse and often discriminatory customary laws governing marriage among Hindus, aiming for uniformity and modernization within the community post-independence.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and constitutional framers attest to the original problem of fragmentation and the goal of reform. While the goal of uniformity within the Hindu community is largely achieved, the 'reform' aspect, particularly regarding gender equity, remains a live and contested issue, corroborated by women's rights organizations and legal scholars.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__hindu_codified_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__hindu_codified_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__hindu_codified_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(marriage_authority_kernel__hindu_codified_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority_kernel__hindu_codified_reading, 0.62, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel__hindu_codified_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority_kernel__hindu_codified_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority_kernel__hindu_codified_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope. It provides a genuine coordination function by offering a uniform legal framework for Hindu marriages, preventing chaos from diverse customary laws (beneficiaries: Hindu community leaders, Hindu men). However, it also involves asymmetric extraction (victims: Hindu women, individuals seeking secular marriage) due to persistent gender inequities in its application and the implicit pressure to conform to community-specific law rather than secular alternatives. Active enforcement by civil courts is required to maintain this structure. Extractiveness is moderate-high, reflecting the ongoing debates around gender equity. Suppression is high due to the state's legal enforcement and social pressures. Theater ratio is low, as the system is functionally active.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Hindu community leaders, the Act is a successful coordination mechanism that preserves cultural identity while modernizing. From the perspective of Hindu women's rights advocates, it is an extractive mechanism that perpetuates gender inequality under the guise of religious freedom and community autonomy. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Hindu community leaders and Hindu men are structural beneficiaries, gaining from the stability, recognition, and traditional aspects of the codified law. Hindu women are primary targets, bearing the costs of gender disparities and limited autonomy within the framework. Civil courts act as agenda-setters, enforcing and interpreting the law. Individuals seeking secular marriage are excluded, as the system implicitly channels Hindus into this specific personal law framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (uniformity and reform of Hindu personal law) is still live, but its 'reform' aspect, particularly regarding gender equity, is contested. The classification as Tangled Rope prevents mislabeling it as a pure Rope (ignoring extraction) or a Snare (ignoring coordination), accurately reflecting its hybrid nature. The persistence of gender inequity, despite judicial efforts, indicates that the coordination function is intertwined with a degree of extraction that has not fully atrophied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    gender_equity_sufficiency,
    'To what extent has the Hindu Marriage Act, as interpreted by civil courts, achieved genuine gender equity, and how much extraction persists due to remaining disparities?',
    'Empirical studies on judicial outcomes, legal aid access for women, and comparative analysis with secular laws. Legislative reforms addressing specific discriminatory provisions.',
    'If gender equity is found to be substantially lacking, the extractiveness metric would be confirmed as high, reinforcing the Tangled Rope classification. If equity has significantly improved, extractiveness might be lower, pushing it closer to a Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(gender_equity_sufficiency, empirical, 'Assessing the actual impact of the law on gender equality.').

omega_variable(
    uniform_civil_code_feasibility,
    'Is the continued existence of separate personal laws, including the Hindu Marriage Act, a necessary feature of India''s constitutional pluralism, or is a Uniform Civil Code a feasible and desirable alternative?',
    'National political consensus, constitutional amendment, and judicial rulings on the enforceability of a Uniform Civil Code. Public discourse and social acceptance.',
    'If a Uniform Civil Code becomes feasible and widely accepted, the ''excluded'' status of individuals seeking secular marriage would diminish, and the overall suppression of alternatives would decrease. This could fundamentally alter the constraint''s classification towards a more Rope-like or Scaffold-like (transitional) form.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(uniform_civil_code_feasibility, preference, 'The political and social viability of replacing personal laws with a Uniform Civil Code.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__hindu_codified_reading, 1955, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1955, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 1955, 0.1).
narrative_ontology:measurement(marr_tr_t1965, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 1965, 0.1).
narrative_ontology:measurement(marr_tr_t1975, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 1975, 0.1).
narrative_ontology:measurement(marr_tr_t1985, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 1985, 0.1).
narrative_ontology:measurement(marr_tr_t1995, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 1995, 0.1).
narrative_ontology:measurement(marr_tr_t2005, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 2005, 0.1).
narrative_ontology:measurement(marr_tr_t2015, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(marr_tr_t2025, marriage_authority_kernel__hindu_codified_reading, theater_ratio, 2025, 0.1).

% Extraction over time
narrative_ontology:measurement(marr_be_t1955, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 1955, 0.68).
narrative_ontology:measurement(marr_be_t1965, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 1965, 0.65).
narrative_ontology:measurement(marr_be_t1975, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 1975, 0.63).
narrative_ontology:measurement(marr_be_t1985, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 1985, 0.62).
narrative_ontology:measurement(marr_be_t1995, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 1995, 0.61).
narrative_ontology:measurement(marr_be_t2005, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 2005, 0.6).
narrative_ontology:measurement(marr_be_t2015, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 2015, 0.61).
narrative_ontology:measurement(marr_be_t2025, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 2025, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1955, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 1955, 0.7).
narrative_ontology:measurement(marr_su_t1965, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 1965, 0.72).
narrative_ontology:measurement(marr_su_t1975, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 1975, 0.73).
narrative_ontology:measurement(marr_su_t1985, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 1985, 0.74).
narrative_ontology:measurement(marr_su_t1995, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 1995, 0.75).
narrative_ontology:measurement(marr_su_t2005, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 2005, 0.75).
narrative_ontology:measurement(marr_su_t2015, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 2015, 0.75).
narrative_ontology:measurement(marr_su_t2025, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 2025, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__hindu_codified_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, marriage_authority_kernel__muslim_shariat_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, marriage_authority_kernel__christian_canonical_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, marriage_authority_kernel__parsi_communal_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, marriage_authority_kernel__secular_civil_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of several readings of the 'marriage_authority_kernel', representing the codified Hindu law framework. Each reading describes a distinct legal and social reality within India's pluralistic legal system.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
