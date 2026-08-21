% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__secular_civil_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority_kernel__secular_civil_reading, []).

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
 *   constraint_id: marriage_authority_kernel__secular_civil_reading
 *   human_readable: Marriage Authority: Secular Civil Code (Special Marriage Act 1954) Reading
 *   domain: comparative_law/constitutional_pluralism/religious_governance
 *
 * SUMMARY:
 *   This constraint story instantiates the 'secular_civil_reading' of the
 *   'marriage_authority_kernel' in India. It describes the legal authority
 *   derived from the Special Marriage Act of 1954, which provides a secular
 *   option for marriage, particularly for inter-religious couples, grounded
 *   in constitutional individual rights. This reading emphasizes civil
 *   courts' adjudication, higher gender equity, and the enablement of
 *   inter-religious unions, while acknowledging the social costs for
 *   individuals who opt out of community-specific personal laws. The
 *   constraint is claimed as a Rope, reflecting its genuine coordination
 *   function in providing a rights-based alternative, but with some
 *   extractiveness due to the social friction it generates for those who
 *   choose it.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__secular_civil_reading, 0.35).
domain_priors:suppression_score(marriage_authority_kernel__secular_civil_reading, 0.2).
domain_priors:theater_ratio(marriage_authority_kernel__secular_civil_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(marriage_authority_kernel__secular_civil_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__secular_civil_reading, rope).
narrative_ontology:human_readable(marriage_authority_kernel__secular_civil_reading, "Marriage Authority: Secular Civil Code (Special Marriage Act 1954) Reading").
narrative_ontology:topic_domain(marriage_authority_kernel__secular_civil_reading, "comparative_law/constitutional_pluralism/religious_governance").

domain_priors:requires_active_enforcement(marriage_authority_kernel__secular_civil_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__secular_civil_reading, 'bbcc4855-0531-4119-8833-5b5def33abe5').
narrative_ontology:cs_kernel_codification('bbcc4855-0531-4119-8833-5b5def33abe5', formalized).
narrative_ontology:cs_authority_grounding('bbcc4855-0531-4119-8833-5b5def33abe5', lineage).
narrative_ontology:cs_interpretation_layer_present('bbcc4855-0531-4119-8833-5b5def33abe5').
narrative_ontology:cs_reading_relation('bbcc4855-0531-4119-8833-5b5def33abe5', marriage_authority_kernel__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('bbcc4855-0531-4119-8833-5b5def33abe5', marriage_authority_kernel__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('bbcc4855-0531-4119-8833-5b5def33abe5', marriage_authority_kernel__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('bbcc4855-0531-4119-8833-5b5def33abe5', marriage_authority_kernel__parsi_communal_reading, coexists_with).
narrative_ontology:cs_axiom('bbcc4855-0531-4119-8833-5b5def33abe5', foundational, individual_rights_supremacy_in_marriage).
narrative_ontology:cs_axiom_status(individual_rights_supremacy_in_marriage, holdable).
narrative_ontology:cs_axiom_grounding('bbcc4855-0531-4119-8833-5b5def33abe5', individual_rights_supremacy_in_marriage, deontological).
narrative_ontology:cs_axiom('bbcc4855-0531-4119-8833-5b5def33abe5', foundational, state_neutrality_in_religious_matters).
narrative_ontology:cs_axiom_status(state_neutrality_in_religious_matters, holdable).
narrative_ontology:cs_axiom_grounding('bbcc4855-0531-4119-8833-5b5def33abe5', state_neutrality_in_religious_matters, conventional).
narrative_ontology:cs_reference_frame('bbcc4855-0531-4119-8833-5b5def33abe5', constitutional_secular_democracy).
narrative_ontology:cs_drift_state('bbcc4855-0531-4119-8833-5b5def33abe5', contemporary_political_discourse, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('bbcc4855-0531-4119-8833-5b5def33abe5', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__secular_civil_reading, inter_religious_couples).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__secular_civil_reading, women_seeking_equity).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__secular_civil_reading, secular_legal_scholars).
narrative_ontology:constraint_victim(marriage_authority_kernel__secular_civil_reading, religious_conservative_groups).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adjudicates marriage and family disputes under the Special Marriage Act, ensuring adherence to constitutional principles of equality and individual rights. This court system is the primary enforcer and interpreter of this reading.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, civil_courts, agenda_setter,
    institutional, generational, analytical, national).

% Benefit from a legal framework that explicitly permits and regulates marriage between individuals of different religious backgrounds, offering a legal alternative to personal laws that might not recognize such unions or impose conversion requirements. They face social costs from their communities for choosing this path.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, inter_religious_couples, beneficiary,
    moderate, biographical, mobile, national).

% Often find greater gender equity in property, divorce, and maintenance provisions under the secular civil code compared to some personal laws. This provides a legal avenue to assert individual rights over community norms, though social pressures may constrain their choice.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, women_seeking_equity, beneficiary,
    moderate, biographical, constrained, national).

% Bear the cost of a legal framework that bypasses their religious authority and norms, particularly regarding inter-religious marriage and gender roles. They actively resist its expansion and seek to reinforce the primacy of personal laws, viewing it as an erosion of religious identity and community control.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, religious_conservative_groups, payer,
    organized, generational, constrained, national).

% Benefit from the existence and expansion of a secular legal framework that aligns with universal human rights and constitutional principles, providing a robust case study for comparative law and legal reform. They advocate for its wider adoption.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, secular_legal_scholars, beneficiary,
    analytical, generational, analytical, global).

% Are excluded from adjudicating marriages registered under the Special Marriage Act, which diminishes their authority and influence over their community members who opt for this secular route. They would argue for the supremacy of their respective personal laws.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__secular_civil_reading, religious_personal_law_boards, excluded,
    institutional, generational, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a uniform, secular legal framework for marriage and family matters, ensuring individual rights and gender equity, particularly for inter-religious couples, where personal laws might conflict or be discriminatory.
% TRANSFER_FUNCTION: Transfers legal authority over marriage and family from religious personal laws to the secular civil code, shifting power from community religious institutions to state civil courts and granting greater individual autonomy.
% ABSENT_VOICES: Religious personal law boards and conservative community leaders are structurally excluded from the adjudication of marriages under the Special Marriage Act; they would object to the erosion of their traditional authority and the perceived secularization of family life.
% DISAPPEARANCE_RATIONALE: If the Special Marriage Act and its enforcement vanished, inter-religious couples would face significant legal hurdles or be forced into religious conversions, women seeking equity would lose a crucial legal recourse, and the legal landscape would revert to a more fragmented, religion-specific system, fundamentally altering individual rights and social dynamics.
% FOUNDING_PROBLEM: The problem of a fragmented legal system where personal laws, often based on religious texts, created inequalities, particularly for women, and made inter-religious marriages legally complex or impossible, undermining constitutional guarantees of equality and individual liberty.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, women's rights organizations, and human rights advocates attest that the founding problems of inequality and fragmentation persist, making the Special Marriage Act a vital, ongoing solution. Religious conservative groups, however, contest this, arguing that personal laws adequately address community needs.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__secular_civil_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__secular_civil_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__secular_civil_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(marriage_authority_kernel__secular_civil_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority_kernel__secular_civil_reading, 0.35, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel__secular_civil_reading_tests).
:- end_tests(marriage_authority_kernel__secular_civil_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.35) is moderate, reflecting the social and community friction experienced by individuals who choose this secular path, rather than direct financial extraction. Suppression (0.20) is low, as the state actively enforces this option, but it still requires individuals to actively opt-in against prevailing community norms. Theater ratio (0.10) is low, as the Act's function is largely direct and effective. Accessibility collapse is moderate (0.40) because while the option exists, social pressures and lack of awareness can limit its practical accessibility. Resistance (0.30) is moderate, primarily from religious conservative groups who oppose its principles.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of inter-religious couples and women seeking equity, this is a vital Rope, offering freedom and protection. From the perspective of religious conservative groups, it is an imposition that undermines their community's religious and social fabric. The engine's per-seat classification will reflect these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   Civil courts, inter-religious couples, women seeking equity, and secular legal scholars are beneficiaries, as the Act aligns with their interests or provides them with legal recourse. Religious conservative groups are payers, as the Act challenges their traditional authority. Religious personal law boards are excluded, as their jurisdiction is bypassed by this secular framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    social_cost_quantification,
    'How can the ''social costs'' borne by individuals opting for the Special Marriage Act (e.g., ostracization, family disapproval) be more precisely quantified and integrated into the extractiveness metric?',
    'Sociological studies and qualitative interviews with couples married under the SMA, comparing their experiences to those under personal laws, to develop a ''social friction'' index.',
    'A higher, more accurately measured social cost would increase the effective extractiveness for individuals, potentially shifting the classification for those seats towards a Tangled Rope or Snare, despite the legal protections.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(social_cost_quantification, empirical, 'Quantifying the non-legal, social extraction from choosing a secular marriage.').

omega_variable(
    uniform_civil_code_pressure,
    'To what extent does the existence and success of the Special Marriage Act create pressure for a broader Uniform Civil Code, and how would such a code alter the landscape of personal laws?',
    'Analysis of legislative debates, judicial pronouncements, and public discourse surrounding a Uniform Civil Code, and its potential impact on the autonomy of religious personal laws.',
    'If the SMA is seen as a precursor to a UCC, it could intensify resistance from religious groups, increasing suppression requirements. A full UCC would fundamentally alter the ''coexists_with'' relationship with other readings, potentially foreclosing them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(uniform_civil_code_pressure, conceptual, 'The SMA''s role as a potential catalyst for a Uniform Civil Code and its impact on legal pluralism.').

omega_variable(
    secular_vs_religious_framing,
    'Is the ''secular'' nature of the Special Marriage Act genuinely neutral, or does it implicitly privilege a particular (e.g., Western liberal) conception of marriage over diverse religious and cultural understandings?',
    'Comparative legal analysis of the SMA''s provisions against diverse cultural and religious marriage practices, examining whether its ''universal'' principles inadvertently marginalize specific community norms.',
    'If the SMA is found to implicitly privilege a specific cultural frame, its claimed neutrality and coordination function would be undermined, potentially increasing its perceived extractiveness for communities whose norms are marginalized.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(secular_vs_religious_framing, conceptual, 'Examining the cultural neutrality of the Special Marriage Act''s secular framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__secular_civil_reading, 1954, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1954, marriage_authority_kernel__secular_civil_reading, theater_ratio, 1954, 0.05).
narrative_ontology:measurement(marr_tr_t1974, marriage_authority_kernel__secular_civil_reading, theater_ratio, 1974, 0.07).
narrative_ontology:measurement(marr_tr_t1994, marriage_authority_kernel__secular_civil_reading, theater_ratio, 1994, 0.08).
narrative_ontology:measurement(marr_tr_t2014, marriage_authority_kernel__secular_civil_reading, theater_ratio, 2014, 0.09).
narrative_ontology:measurement(marr_tr_t2024, marriage_authority_kernel__secular_civil_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(marr_be_t1954, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 1954, 0.2).
narrative_ontology:measurement(marr_be_t1974, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 1974, 0.25).
narrative_ontology:measurement(marr_be_t1994, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 1994, 0.3).
narrative_ontology:measurement(marr_be_t2014, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 2014, 0.33).
narrative_ontology:measurement(marr_be_t2024, marriage_authority_kernel__secular_civil_reading, base_extractiveness, 2024, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1954, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 1954, 0.15).
narrative_ontology:measurement(marr_su_t1974, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 1974, 0.17).
narrative_ontology:measurement(marr_su_t1994, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 1994, 0.18).
narrative_ontology:measurement(marr_su_t2014, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 2014, 0.19).
narrative_ontology:measurement(marr_su_t2024, marriage_authority_kernel__secular_civil_reading, suppression_requirement, 2024, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__secular_civil_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel__hindu_codified_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel__muslim_shariat_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel__christian_canonical_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__secular_civil_reading, marriage_authority_kernel__parsi_communal_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'marriage_authority_kernel', which encompasses multiple, often competing, legal frameworks for marriage and family law in India. This secular civil reading influences and coexists with the various religious personal law readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
