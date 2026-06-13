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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: marriage_authority_kernel__christian_canonical_reading
 *   human_readable: Indian Christian Marriage Law (Canonical Reading)
 *   domain: comparative_law/religious_governance
 *
 * SUMMARY:
 *   This constraint describes the authority structure of marriage and family
 *   law for Christians in India, specifically as it derives from Christian
 *   canonical law and is codified in the Indian Christian Marriage Act 1872.
 *   This is one reading of the broader 'marriage_authority_kernel' in India,
 *   which is contested by Hindu, Muslim, Parsi, and secular civil law
 *   readings. This reading emphasizes restrictive divorce (fault-based), the
 *   role of church tribunals for annulment, and a moderate approach to gender
 *   equity within the canonical framework.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__christian_canonical_reading, 0.6).
domain_priors:suppression_score(marriage_authority_kernel__christian_canonical_reading, 0.7).
domain_priors:theater_ratio(marriage_authority_kernel__christian_canonical_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__christian_canonical_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__christian_canonical_reading, "Indian Christian Marriage Law (Canonical Reading)").
narrative_ontology:topic_domain(marriage_authority_kernel__christian_canonical_reading, "comparative_law/religious_governance").

domain_priors:requires_active_enforcement(marriage_authority_kernel__christian_canonical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__christian_canonical_reading, '2b0fa9f2-bf40-4276-8220-cb7042fc1762').
narrative_ontology:cs_kernel_codification('2b0fa9f2-bf40-4276-8220-cb7042fc1762', fixed_text).
narrative_ontology:cs_authority_grounding('2b0fa9f2-bf40-4276-8220-cb7042fc1762', lineage).
narrative_ontology:cs_interpretation_layer_present('2b0fa9f2-bf40-4276-8220-cb7042fc1762').
narrative_ontology:cs_reading_relation('2b0fa9f2-bf40-4276-8220-cb7042fc1762', marriage_authority_kernel__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('2b0fa9f2-bf40-4276-8220-cb7042fc1762', marriage_authority_kernel__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('2b0fa9f2-bf40-4276-8220-cb7042fc1762', marriage_authority_kernel__parsi_communal_reading, coexists_with).
narrative_ontology:cs_reading_relation('2b0fa9f2-bf40-4276-8220-cb7042fc1762', marriage_authority_kernel__secular_civil_reading, influences).
narrative_ontology:cs_axiom('2b0fa9f2-bf40-4276-8220-cb7042fc1762', foundational, marriage_as_sacrament_indissoluble).
narrative_ontology:cs_axiom_status(marriage_as_sacrament_indissoluble, holdable).
narrative_ontology:cs_axiom_grounding('2b0fa9f2-bf40-4276-8220-cb7042fc1762', marriage_as_sacrament_indissoluble, theological).
narrative_ontology:cs_axiom('2b0fa9f2-bf40-4276-8220-cb7042fc1762', foundational, canonical_law_as_primary_authority).
narrative_ontology:cs_axiom_status(canonical_law_as_primary_authority, holdable).
narrative_ontology:cs_axiom_grounding('2b0fa9f2-bf40-4276-8220-cb7042fc1762', canonical_law_as_primary_authority, conventional).
narrative_ontology:cs_reference_frame('2b0fa9f2-bf40-4276-8220-cb7042fc1762', traditional_canonical_authority).
narrative_ontology:cs_drift_state('2b0fa9f2-bf40-4276-8220-cb7042fc1762', contemporary_indian_constitutionalism, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2b0fa9f2-bf40-4276-8220-cb7042fc1762', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__christian_canonical_reading, christian_clergy).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__christian_canonical_reading, conservative_christian_community).
narrative_ontology:constraint_victim(marriage_authority_kernel__christian_canonical_reading, christian_women_seeking_divorce).
narrative_ontology:constraint_victim(marriage_authority_kernel__christian_canonical_reading, christian_individuals_seeking_annulment_outside_church_tribunals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(marriage_authority_kernel__christian_canonical_reading, christian_men_seeking_divorce).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers marriage and divorce proceedings according to canonical law, often through church tribunals. Benefits from maintaining the authority and interpretive power of the church over personal law matters for its community.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, christian_clergy, agenda_setter,
    institutional, generational, identity_locked, national).

% Face restrictive, fault-based divorce provisions and often must navigate church tribunals, which can be lengthy and emotionally taxing. Their options for exit from marriage are limited by the canonical interpretation of the law.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, christian_women_seeking_divorce, payer,
    powerless, biographical, constrained, local).

% Also subject to fault-based divorce, but may experience less social stigma or have greater access to legal resources than women. Their options are similarly constrained by canonical interpretations.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, christian_men_seeking_divorce, payer,
    moderate, biographical, constrained, local).

% Are legally bound to apply the Indian Christian Marriage Act 1872, which incorporates canonical principles. They interpret and enforce the law, but their interpretive scope is limited by the Act's framework, which defers to religious authority on certain matters.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, indian_civil_courts, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__christian_canonical_reading, indian_civil_courts, observer).

% Advocate for a uniform civil code and more equitable, secular marriage and divorce laws for all citizens, including Christians. They are excluded from the direct administration of Christian personal law but exert pressure through advocacy and litigation.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, secular_legal_reformers, excluded,
    organized, generational, mobile, national).

% Benefits from the preservation of traditional canonical interpretations of marriage and family, which reinforces community identity and moral norms. They see the Act as protecting their religious freedom and way of life.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, conservative_christian_community, beneficiary,
    organized, generational, identity_locked, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for Christian marriages and divorces in India, ensuring legal recognition and a degree of uniformity within the Christian community, while aligning with religious doctrine.
% TRANSFER_FUNCTION: Transfers authority over marital dissolution and annulment from individual choice or secular civil courts to religious institutions and their interpretations, particularly impacting those seeking divorce or annulment.
% ABSENT_VOICES: Christian individuals seeking more liberal divorce terms or gender-neutral provisions, and secular legal reformers advocating for a uniform civil code, are marginalized by the current framework. Their voices are present in public discourse but not in the direct administration of the law.
% DISAPPEARANCE_RATIONALE: If the Indian Christian Marriage Act 1872 and its canonical underpinnings vanished, Christian marriages and divorces would immediately fall under the Special Marriage Act 1954 (secular civil code) or face a legal vacuum, fundamentally altering the legal and social landscape for Christians in India.
% FOUNDING_PROBLEM: To provide a legal framework for Christian marriages in British India, recognizing Christian religious practices while integrating them into the colonial legal system, ensuring legal validity and preventing disputes.
% FOUNDING_PROBLEM_CORROBORATION: Christian clergy and conservative community members attest that the problem of legally recognizing Christian marriages and maintaining religious distinctiveness is still live. Secular legal reformers and some Christian individuals argue that while legal recognition is necessary, the specific, restrictive canonical interpretations within the Act are an outdated problem, not a solution, leading to a contested status.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__christian_canonical_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__christian_canonical_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__christian_canonical_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(marriage_authority_kernel__christian_canonical_reading, 'none', 1).

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
 *   The extractiveness (0.6) is substantial due to the restrictive divorce provisions and the power of church tribunals, which can impose significant costs on individuals seeking to exit marriages. Suppression (0.7) is high because alternatives to this legal framework are limited for Christians, and the community often exerts social pressure to conform. The theater ratio (0.2) is low, indicating that the Act's provisions are genuinely enforced and serve their intended function within the canonical framework, rather than being purely performative. The metrics show a gradual increase in extractiveness and suppression over time, reflecting the growing tension between traditional canonical interpretations and evolving societal norms.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Christian clergy and conservative community, the Act is a legitimate coordination mechanism that upholds religious tenets and community identity. From the perspective of individuals seeking divorce or annulment, it is an extractive and suppressive system that limits their autonomy. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Christian clergy and conservative Christian communities are beneficiaries, as the constraint preserves their authority and traditional values (d near 0.0-0.2). Christian individuals, particularly women seeking divorce or annulment, are targets, facing significant costs and limited options (d near 0.8-1.0). Indian civil courts act as agenda-setters, enforcing the Act but also mediating its application, placing them closer to symmetric (d near 0.5). Secular legal reformers are excluded, as their proposals challenge the very basis of this constraint.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    canonical_interpretation_flexibility,
    'To what extent can the canonical interpretations embedded in the Indian Christian Marriage Act 1872 be reinterpreted or reformed to align with contemporary gender equity and individual rights without fundamentally undermining Christian doctrine?',
    'Judicial rulings from higher courts that reinterpret the Act in light of constitutional rights, or internal theological reforms within Christian denominations that lead to more liberal interpretations of canonical law.',
    'If reinterpretation is possible, the constraint could shift towards a more equitable Rope or Scaffold, reducing extractiveness and suppression. If not, its Snare-like qualities for certain individuals would persist, requiring external legal reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(canonical_interpretation_flexibility, conceptual, 'The inherent flexibility of canonical interpretation within the legal framework.').

omega_variable(
    secular_law_supremacy_ambiguity,
    'Is the Indian Christian Marriage Act 1872 ultimately subordinate to the secular principles of the Indian Constitution, particularly regarding fundamental rights, or does it retain a distinct, parallel authority?',
    'A definitive Supreme Court ruling on the primacy of constitutional fundamental rights over personal law provisions that conflict with them, or legislative action to enact a uniform civil code.',
    'If constitutional supremacy is fully asserted, the constraint''s suppressive and extractive elements would be challenged, potentially forcing a reclassification towards a Scaffold (transitional to a uniform code) or even a Rope (if coordination remains without extraction). If parallel authority is maintained, the current Tangled Rope classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(secular_law_supremacy_ambiguity, empirical, 'The unresolved tension between religious personal law and secular constitutional principles.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__christian_canonical_reading, 1872, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1872, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 1872, 0.1).
narrative_ontology:measurement(marr_tr_t1920, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 1920, 0.12).
narrative_ontology:measurement(marr_tr_t1970, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(marr_tr_t2000, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(marr_tr_t2024, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(marr_be_t1872, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 1872, 0.4).
narrative_ontology:measurement(marr_be_t1920, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 1920, 0.45).
narrative_ontology:measurement(marr_be_t1970, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 1970, 0.55).
narrative_ontology:measurement(marr_be_t2000, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 2000, 0.58).
narrative_ontology:measurement(marr_be_t2024, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 2024, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1872, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 1872, 0.5).
narrative_ontology:measurement(marr_su_t1920, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 1920, 0.55).
narrative_ontology:measurement(marr_su_t1970, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 1970, 0.65).
narrative_ontology:measurement(marr_su_t2000, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement(marr_su_t2024, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__christian_canonical_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel__hindu_codified_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel__muslim_shariat_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel__parsi_communal_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel__secular_civil_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'marriage_authority_kernel' in India, which encompasses multiple religious and secular legal frameworks for marriage and family law. Each reading represents a distinct constraint with its own structural properties and impacts on stakeholders.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
