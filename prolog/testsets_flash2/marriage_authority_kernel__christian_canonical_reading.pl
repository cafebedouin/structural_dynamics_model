% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__christian_canonical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority_kernel__christian_canonical_reading, []).

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
 *   constraint_id: marriage_authority_kernel__christian_canonical_reading
 *   human_readable: Indian Christian Marriage Law (Canonical Reading)
 *   domain: comparative_law/constitutional_pluralism/religious_governance
 *
 * SUMMARY:
 *   This constraint describes the operation of marriage and family law for
 *   Christians in India, where authority derives from Christian canonical law
 *   as codified in the Indian Christian Marriage Act 1872. This is one
 *   reading of the broader 'marriage_authority_kernel' in India, which is
 *   characterized by legal pluralism. This reading emphasizes restrictive
 *   divorce, church tribunals for annulment, and moderate gender equity
 *   compared to a purely secular code. The constraint is claimed as a Tangled
 *   Rope, reflecting its dual function of coordinating religious community
 *   life while extracting autonomy, particularly from women, through its
 *   restrictive provisions.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__christian_canonical_reading, 0.65).
domain_priors:suppression_score(marriage_authority_kernel__christian_canonical_reading, 0.7).
domain_priors:theater_ratio(marriage_authority_kernel__christian_canonical_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__christian_canonical_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__christian_canonical_reading, "Indian Christian Marriage Law (Canonical Reading)").
narrative_ontology:topic_domain(marriage_authority_kernel__christian_canonical_reading, "comparative_law/constitutional_pluralism/religious_governance").

domain_priors:requires_active_enforcement(marriage_authority_kernel__christian_canonical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__christian_canonical_reading, 'ddf443fb-2e11-4717-8d4e-7e14e480d2da').
narrative_ontology:cs_kernel_codification('ddf443fb-2e11-4717-8d4e-7e14e480d2da', formalized).
narrative_ontology:cs_authority_grounding('ddf443fb-2e11-4717-8d4e-7e14e480d2da', lineage).
narrative_ontology:cs_interpretation_layer_present('ddf443fb-2e11-4717-8d4e-7e14e480d2da').
narrative_ontology:cs_reading_relation('ddf443fb-2e11-4717-8d4e-7e14e480d2da', marriage_authority_kernel__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('ddf443fb-2e11-4717-8d4e-7e14e480d2da', marriage_authority_kernel__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('ddf443fb-2e11-4717-8d4e-7e14e480d2da', marriage_authority_kernel__parsi_communal_reading, coexists_with).
narrative_ontology:cs_reading_relation('ddf443fb-2e11-4717-8d4e-7e14e480d2da', marriage_authority_kernel__secular_civil_reading, coexists_with).
narrative_ontology:cs_axiom('ddf443fb-2e11-4717-8d4e-7e14e480d2da', foundational, marriage_as_sacrament_indissoluble).
narrative_ontology:cs_axiom_status(marriage_as_sacrament_indissoluble, holdable).
narrative_ontology:cs_axiom_grounding('ddf443fb-2e11-4717-8d4e-7e14e480d2da', marriage_as_sacrament_indissoluble, theological).
narrative_ontology:cs_axiom('ddf443fb-2e11-4717-8d4e-7e14e480d2da', foundational, canonical_law_as_supreme_authority).
narrative_ontology:cs_axiom_status(canonical_law_as_supreme_authority, holdable).
narrative_ontology:cs_axiom_grounding('ddf443fb-2e11-4717-8d4e-7e14e480d2da', canonical_law_as_supreme_authority, conventional).
narrative_ontology:cs_reference_frame('ddf443fb-2e11-4717-8d4e-7e14e480d2da', traditional_canonical_authority).
narrative_ontology:cs_drift_state('ddf443fb-2e11-4717-8d4e-7e14e480d2da', contemporary_indian_constitutional_context, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ddf443fb-2e11-4717-8d4e-7e14e480d2da', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__christian_canonical_reading, christian_church_authorities).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__christian_canonical_reading, conservative_christian_communities).
narrative_ontology:constraint_victim(marriage_authority_kernel__christian_canonical_reading, christian_women_seeking_divorce).
narrative_ontology:constraint_victim(marriage_authority_kernel__christian_canonical_reading, christian_individuals_seeking_annulment).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(marriage_authority_kernel__christian_canonical_reading, christian_men_seeking_divorce).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers marriage and divorce under the Indian Christian Marriage Act 1872, interpreting it through canonical law. Benefits from maintaining its authority over personal law for its community, which reinforces its institutional legitimacy and influence.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, christian_church_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Subject to restrictive, fault-based divorce provisions and often required to seek annulment through church tribunals, which can be lengthy, costly, and emotionally taxing. Their options are limited by religious identity and social pressure within their community.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, christian_women_seeking_divorce, payer,
    powerless, biographical, identity_locked, local).

% Also subject to restrictive divorce laws, but often face less social stigma and may have more resources to navigate the legal and ecclesiastical processes. Their situation is less severe than women's due to existing gender power dynamics.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, christian_men_seeking_divorce, payer,
    moderate, biographical, constrained, local).

% Benefit from the preservation of traditional marriage norms and the church's authority, which reinforces their social cohesion and moral framework. They exert social pressure to maintain the status quo.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, conservative_christian_communities, beneficiary,
    organized, generational, constrained, local).

% Interpret and apply the Indian Christian Marriage Act 1872, often balancing canonical interpretations with constitutional principles of equality. They are the ultimate arbiter but operate within the framework of existing personal laws.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, indian_civil_courts, observer,
    institutional, generational, analytical, national).

% Advocate for a uniform civil code and the abolition of religion-specific personal laws, arguing for individual rights and gender equality. They are outside the direct administration of the Christian personal law but influence the broader legal discourse.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, secular_legal_reformers, excluded,
    organized, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for marriage and family life for Christian communities in India, ensuring legal recognition and social order within that specific religious context.
% TRANSFER_FUNCTION: Transfers authority over marriage and divorce from the state's general civil code to Christian canonical law and its interpreters, impacting individual rights and autonomy, particularly for women.
% ABSENT_VOICES: Christian individuals, especially women, who seek more equitable and less restrictive divorce and annulment processes, are often marginalized within the church-led legal framework. Secular legal reformers are also excluded from direct influence over the canonical interpretation.
% DISAPPEARANCE_RATIONALE: If the Indian Christian Marriage Act 1872 and its canonical interpretation vanished, Christian marriages would default to the Special Marriage Act 1954 (secular civil code) or a new uniform civil code, fundamentally altering the legal and social landscape for Indian Christians, particularly regarding divorce and succession.
% FOUNDING_PROBLEM: To provide a specific legal framework for Christian marriages in British India, respecting religious customs while integrating them into the colonial legal system, distinct from Hindu or Muslim personal laws.
% FOUNDING_PROBLEM_CORROBORATION: Christian church authorities and conservative communities attest that the problem of maintaining distinct religious identity and order is still live. Secular legal reformers and many Christian individuals attest that the original problem is largely solved, and the current framework primarily serves to maintain institutional power and traditional gender roles, rather than genuine coordination.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__christian_canonical_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__christian_canonical_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__christian_canonical_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(marriage_authority_kernel__christian_canonical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority_kernel__christian_canonical_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel__christian_canonical_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority_kernel__christian_canonical_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority_kernel__christian_canonical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is substantial due to the restrictive nature of divorce and annulment processes, which impose significant costs on individuals seeking to exit marriages. Suppression (0.70) is high, driven by the institutional authority of the church and strong social pressures within conservative Christian communities, limiting alternatives. The theater ratio (0.20) is moderate; while there's genuine coordination, a portion of the enforcement is performative, maintaining traditional authority structures. Accessibility collapse (0.60) is moderate, as the secular Special Marriage Act offers an alternative, but identity-locked individuals find it difficult to access. Resistance (0.45) is present from women's rights groups and secular reformers, but not strong enough to fundamentally alter the constraint's operation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Christian church authorities, this constraint is a legitimate Rope, coordinating religious life and upholding sacred traditions. From the perspective of Christian women seeking divorce, it operates as a Snare, trapping them in difficult marriages due to high exit costs and social stigma. The engine's classification as Tangled Rope captures this hybrid nature, acknowledging both coordination and extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Christian church authorities and conservative Christian communities are beneficiaries, as the constraint reinforces their institutional power and traditional values. Christian women seeking divorce or annulment are primary victims, bearing the costs of restrictive laws and ecclesiastical processes. Christian men seeking divorce are also payers, but often with more social capital and fewer direct costs. Indian civil courts act as observers, interpreting the law. Secular legal reformers are excluded, advocating from outside the system.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate has drifted from its original purpose of providing a distinct legal framework for Christians to one that increasingly serves to maintain institutional authority and traditional gender roles, even as societal norms evolve. The 'contested' status of the founding problem and 'world_rearranges' disappearance verdict indicate a potential for mandatrophy, where the constraint persists due to inertia and concentrated benefits despite its original problem being largely solved or having evolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    gender_equity_vs_canonical_interpretation,
    'To what extent can Christian canonical law be reinterpreted to align with modern constitutional principles of gender equity without undermining its foundational theological premises?',
    'Judicial review by the Supreme Court of India on specific provisions of the Indian Christian Marriage Act, or internal theological reforms within Christian denominations.',
    'If reinterpretation is possible, the constraint could shift towards a Rope or Scaffold, reducing extractiveness. If not, the tension between canonical law and constitutional rights will persist, maintaining its Tangled Rope or Snare characteristics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gender_equity_vs_canonical_interpretation, conceptual, 'The conceptual flexibility of canonical interpretation regarding gender equity.').

omega_variable(
    identity_lock_strength,
    'How strong is the ''identity_locked'' exit option for Christian individuals, particularly women, in practice? What proportion would genuinely face severe social ostracism or loss of community by opting for secular marriage/divorce?',
    'Sociological studies on the lived experiences of Christian individuals who have utilized the Special Marriage Act or sought divorce outside canonical processes, measuring social and economic consequences.',
    'If identity lock is weaker than assumed, the effective suppression and extractiveness are lower, potentially shifting the constraint closer to a Rope. If stronger, it reinforces the Snare-like aspects for vulnerable groups.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_strength, empirical, 'Empirical strength of identity-based barriers to exit from canonical law.').

omega_variable(
    uniform_civil_code_impact,
    'What would be the full structural impact of a uniform civil code on the authority of Christian canonical law in India, and how would it affect the coordination function for Christian communities?',
    'Legal analysis of proposed uniform civil code drafts, comparative studies with countries that have transitioned from pluralistic to uniform family laws, and surveys of Christian community responses.',
    'A uniform civil code could effectively ''foreclose'' this reading, shifting all marriage authority to a secular civil reading. The impact on coordination for Christian communities could range from minimal disruption to significant social upheaval, depending on implementation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(uniform_civil_code_impact, preference, 'The potential for a uniform civil code to displace or integrate religious personal laws.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__christian_canonical_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(marr_tr_t10, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(marr_tr_t20, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(marr_tr_t30, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement(marr_tr_t40, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(marr_tr_t50, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 50, 0.2).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(marr_be_t10, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(marr_be_t20, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(marr_be_t30, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(marr_be_t40, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 40, 0.65).
narrative_ontology:measurement(marr_be_t50, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 50, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(marr_su_t10, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(marr_su_t20, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(marr_su_t30, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(marr_su_t40, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(marr_su_t50, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 50, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__christian_canonical_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_authority_kernel__christian_canonical_reading, 0.08).
narrative_ontology:affects_constraint(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel__hindu_codified_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel__muslim_shariat_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel__parsi_communal_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel__secular_civil_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'marriage_authority_kernel' in India, which encompasses multiple religious and secular legal frameworks. Each reading represents a distinct constraint with its own structural properties and stakeholders. This specific reading focuses on Christian canonical law as codified in the Indian Christian Marriage Act 1872.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
