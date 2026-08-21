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
 *   human_readable: Hindu Marriage Act 1955 Authority (Civil Court Interpretation)
 *   domain: comparative_law/constitutional_pluralism/religious_governance
 *
 * SUMMARY:
 *   This constraint story analyzes the authority of marriage and family law
 *   for the Hindu community in India, specifically as derived from the Hindu
 *   Marriage Act 1955 and interpreted by civil courts. It is one reading of
 *   the broader 'marriage_authority_kernel' which encompasses various
 *   religious and secular legal frameworks in India. This reading emphasizes
 *   the codification of traditional law and its adjudication by the state,
 *   aiming for uniformity within the Hindu community while navigating
 *   constitutional principles of equality.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__hindu_codified_reading, 0.45).
domain_priors:suppression_score(marriage_authority_kernel__hindu_codified_reading, 0.6).
domain_priors:theater_ratio(marriage_authority_kernel__hindu_codified_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(marriage_authority_kernel__hindu_codified_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__hindu_codified_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__hindu_codified_reading, "Hindu Marriage Act 1955 Authority (Civil Court Interpretation)").
narrative_ontology:topic_domain(marriage_authority_kernel__hindu_codified_reading, "comparative_law/constitutional_pluralism/religious_governance").

domain_priors:requires_active_enforcement(marriage_authority_kernel__hindu_codified_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__hindu_codified_reading, '104030fc-a72b-4f43-be0f-280471119f2b').
narrative_ontology:cs_kernel_codification('104030fc-a72b-4f43-be0f-280471119f2b', formalized).
narrative_ontology:cs_authority_grounding('104030fc-a72b-4f43-be0f-280471119f2b', lineage).
narrative_ontology:cs_interpretation_layer_present('104030fc-a72b-4f43-be0f-280471119f2b').
narrative_ontology:cs_reading_relation('104030fc-a72b-4f43-be0f-280471119f2b', marriage_authority_kernel__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('104030fc-a72b-4f43-be0f-280471119f2b', marriage_authority_kernel__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('104030fc-a72b-4f43-be0f-280471119f2b', marriage_authority_kernel__parsi_communal_reading, coexists_with).
narrative_ontology:cs_reading_relation('104030fc-a72b-4f43-be0f-280471119f2b', marriage_authority_kernel__secular_civil_reading, influences).
narrative_ontology:cs_axiom('104030fc-a72b-4f43-be0f-280471119f2b', foundational, hindu_personal_law_autonomy).
narrative_ontology:cs_axiom_status(hindu_personal_law_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('104030fc-a72b-4f43-be0f-280471119f2b', hindu_personal_law_autonomy, conventional).
narrative_ontology:cs_axiom('104030fc-a72b-4f43-be0f-280471119f2b', foundational, state_adjudication_of_religious_law).
narrative_ontology:cs_axiom_status(state_adjudication_of_religious_law, holdable).
narrative_ontology:cs_axiom_grounding('104030fc-a72b-4f43-be0f-280471119f2b', state_adjudication_of_religious_law, conventional).
narrative_ontology:cs_reference_frame('104030fc-a72b-4f43-be0f-280471119f2b', codified_hindu_legal_tradition).
narrative_ontology:cs_drift_state('104030fc-a72b-4f43-be0f-280471119f2b', contemporary_constitutional_scrutiny, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('104030fc-a72b-4f43-be0f-280471119f2b', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__hindu_codified_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__hindu_codified_reading, hindu_community_members).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__hindu_codified_reading, civil_courts).
narrative_ontology:constraint_victim(marriage_authority_kernel__hindu_codified_reading, hindu_women_seeking_equal_rights).
narrative_ontology:constraint_victim(marriage_authority_kernel__hindu_codified_reading, interfaith_couples).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from a codified, relatively uniform set of marriage and family laws that largely reflect traditional Hindu social structures, providing legal clarity and social cohesion within the community. They are subject to its rules but also derive stability from them.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, hindu_community_members, beneficiary,
    organized, generational, constrained, national).

% Interpret and enforce the Hindu Marriage Act 1955, providing a formal legal framework for disputes and ensuring a degree of state oversight. They gain authority and legitimacy from this role, balancing traditional law with constitutional principles.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, civil_courts, agenda_setter,
    institutional, generational, analytical, national).

% Are subject to a legal framework that, while more progressive than uncodified personal laws, still contains provisions and interpretations that can disadvantage them compared to secular civil law, particularly regarding property rights, divorce, and maintenance. Their options for redress are limited to litigation within the existing framework.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, hindu_women_seeking_equal_rights, payer,
    powerless, biographical, constrained, local).

% Cannot marry under the Hindu Marriage Act if one partner is not Hindu, forcing them to use the Special Marriage Act 1954, which often entails social stigma and administrative hurdles. They are excluded from the community-specific legal framework.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, interfaith_couples, excluded,
    powerless, biographical, constrained, local).

% Advocate for a uniform civil code that would supersede all religion-specific personal laws, arguing that the Hindu Marriage Act, despite its codification, still perpetuates inequalities and undermines constitutional secularism. They analyze its operation and propose legislative changes.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__hindu_codified_reading, secular_legal_reformers, observer,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a standardized, state-enforced legal framework for marriage, divorce, and family matters for the Hindu community, reducing ambiguity and facilitating dispute resolution within that specific religious-cultural context.
% TRANSFER_FUNCTION: Transfers legal authority over Hindu family matters from purely religious or customary bodies to civil courts, while retaining a framework rooted in Hindu legal tradition. It also implicitly transfers some autonomy from individuals to community norms as interpreted by the state.
% ABSENT_VOICES: Interfaith couples and those advocating for a fully secular, gender-neutral uniform civil code are marginalized by the persistence of religion-specific personal laws. They would argue for individual autonomy over religious community identity in legal matters.
% DISAPPEARANCE_RATIONALE: If the Hindu Marriage Act 1955 vanished, the Hindu community would face legal chaos in family matters, forcing a rapid shift to either uncodified customary law (if permitted) or the Special Marriage Act 1954, fundamentally altering the legal landscape for a significant portion of the population.
% FOUNDING_PROBLEM: Prior to 1955, Hindu personal law was diverse, uncodified, and often inconsistent, leading to legal uncertainty and varying degrees of gender inequality across regions. The Act aimed to codify and reform these laws, introducing some uniformity and progressive changes.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars and civil society organizations acknowledge that the Act addressed significant problems of legal fragmentation and inequality, and that its framework continues to provide necessary structure. However, many also point to ongoing issues of gender inequality and the need for further reform, indicating the problem is still live but evolving.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__hindu_codified_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__hindu_codified_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__hindu_codified_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(marriage_authority_kernel__hindu_codified_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority_kernel__hindu_codified_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel__hindu_codified_reading_tests).
:- end_tests(marriage_authority_kernel__hindu_codified_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate: while it provides a structured legal system, it still contains elements that can disadvantage certain groups, particularly women, compared to a fully secular code. Suppression (0.6) is present as individuals are bound by this specific legal framework if they identify as Hindu, with limited options to opt out without social or legal friction. The 'claimed_type' is Tangled Rope because it genuinely coordinates legal processes for a large community while simultaneously extracting from those whose rights are not fully aligned with its traditional underpinnings. The temporal measurements show a slight decrease in extractiveness and suppression over time, reflecting ongoing legal reforms and evolving social norms, but a recent stabilization/slight increase as calls for a uniform civil code intensify, leading to a hardening of positions.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Hindu community members, the Act is a beneficial coordination mechanism that preserves cultural identity and provides legal clarity. From the perspective of Hindu women seeking full equality or interfaith couples, it represents a constraint that limits their choices and perpetuates inequalities. Civil courts, as agenda-setters, attempt to balance these perspectives within the constitutional framework.
 *
 * DIRECTIONALITY LOGIC:
 *   Hindu community members and civil courts are beneficiaries, as they gain legal clarity, social cohesion, and institutional authority, respectively. Hindu women seeking equal rights and interfaith couples are victims, as they bear the costs of its limitations and exclusions. The constraint subsidizes the former by providing a stable framework, while extracting from the latter by limiting their legal options or perpetuating traditional disadvantages.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (to codify and reform Hindu personal law) is still live, preventing it from being a Piton. However, the 'contested' status of the founding problem and the ongoing debates about gender equality and secularism suggest it is a Tangled Rope, where the coordination function is intertwined with asymmetric extraction. The classification prevents mislabeling it as a pure Rope (ignoring extraction) or a Snare (ignoring coordination).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    gender_equity_interpretation_drift,
    'To what extent do civil court interpretations of the Hindu Marriage Act 1955 align with or diverge from contemporary constitutional gender equality principles?',
    'Systematic analysis of court judgments over time, comparing outcomes in cases related to property, divorce, and maintenance with those under the Special Marriage Act or international human rights standards.',
    'If interpretations consistently fall short of constitutional principles, the effective extractiveness for Hindu women is higher than currently assessed, potentially reclassifying the constraint closer to a Snare. If interpretations show significant progressive drift, extractiveness may be lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gender_equity_interpretation_drift, empirical, 'Assesses the actual impact of judicial interpretation on gender equality within the Hindu Marriage Act framework.').

omega_variable(
    uniform_civil_code_feasibility,
    'Is a uniform civil code (UCC) a politically and socially feasible alternative that would genuinely resolve the coordination problems while eliminating extraction, or would it create new forms of social friction and resistance?',
    'Pilot implementation of UCC in specific regions or detailed sociological studies on community acceptance and potential for conflict, rather than purely legal analysis.',
    'If a UCC is feasible and widely accepted, the current constraint''s suppression and extractiveness are more clearly a choice rather than a necessity, strengthening arguments for its reform or abolition. If a UCC proves highly disruptive, the current constraint''s coordination function is more strongly justified, even with its extractive elements.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(uniform_civil_code_feasibility, preference, 'Examines the practical and social viability of a secular alternative to religion-specific personal laws.').

omega_variable(
    kernel_reading_identity,
    'This constraint is one reading of the ''marriage_authority_kernel''. This ''hindu_codified_reading'' emphasizes codified Hindu law interpreted by civil courts. How would the classification change if a ''secular_civil_reading'' (Special Marriage Act 1954) were adopted as the primary framework?',
    'Analyzing the ''secular_civil_reading'' constraint story and comparing its metrics and stakeholder impacts, particularly on ''hindu_women_seeking_equal_rights'' and ''interfaith_couples''.',
    'A shift to the ''secular_civil_reading'' would likely reduce extractiveness and suppression for ''hindu_women_seeking_equal_rights'' and ''interfaith_couples'' but might increase resistance from ''hindu_community_members'' who value their distinct legal identity. The core disagreement is located in the balance between individual rights and community identity in legal matters.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Examines the impact of adopting a different reading of the marriage authority kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__hindu_codified_reading, 1955, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(marr_be_t1955, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 1955, 0.5).
narrative_ontology:measurement(marr_be_t1975, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 1975, 0.48).
narrative_ontology:measurement(marr_be_t1995, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 1995, 0.45).
narrative_ontology:measurement(marr_be_t2010, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 2010, 0.43).
narrative_ontology:measurement(marr_be_t2024, marriage_authority_kernel__hindu_codified_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1955, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 1955, 0.7).
narrative_ontology:measurement(marr_su_t1975, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 1975, 0.65).
narrative_ontology:measurement(marr_su_t1995, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 1995, 0.6).
narrative_ontology:measurement(marr_su_t2010, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 2010, 0.58).
narrative_ontology:measurement(marr_su_t2024, marriage_authority_kernel__hindu_codified_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__hindu_codified_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, marriage_authority_kernel__muslim_shariat_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, marriage_authority_kernel__christian_canonical_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, marriage_authority_kernel__parsi_communal_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__hindu_codified_reading, marriage_authority_kernel__secular_civil_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
