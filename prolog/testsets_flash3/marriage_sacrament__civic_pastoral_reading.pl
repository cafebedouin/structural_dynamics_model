% ============================================================================
% CONSTRAINT STORY: marriage_sacrament__civic_pastoral_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_sacrament__civic_pastoral_reading, []).

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
 *   constraint_id: marriage_sacrament__civic_pastoral_reading
 *   human_readable: Marriage as Pastoral Relationship (Civic-Pastoral Reading)
 *   domain: religious_doctrine/canon_law/political_sociology
 *
 * SUMMARY:
 *   This constraint describes the 'civic-pastoral' reading of the marriage
 *   sacrament within a religious institution, where indissolubility is
 *   treated as an ideal requiring compassionate discernment in individual
 *   cases, acknowledging human failure. This reading emphasizes pastoral care
 *   and flexibility, leading to a moderate level of extraction from those who
 *   prioritize strict doctrinal consistency. It is one reading of the broader
 *   'marriage_sacrament' kernel.
 *
 * KEY AGENTS:
 *   - pastoral_clergy: Agenda setter (institutional/constrained)
 *   - laity_seeking_discernment: Beneficiary (moderate/constrained)
 *   - traditional_catholics: Payer (organized/identity_locked)
 *   - doctrinal_conservatives: Payer (powerful/constrained)
 *   - institutional_hierarchy: Agenda setter (institutional/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_sacrament__civic_pastoral_reading, 0.45).
domain_priors:suppression_score(marriage_sacrament__civic_pastoral_reading, 0.3).
domain_priors:theater_ratio(marriage_sacrament__civic_pastoral_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_sacrament__civic_pastoral_reading, tangled_rope).
narrative_ontology:human_readable(marriage_sacrament__civic_pastoral_reading, "Marriage as Pastoral Relationship (Civic-Pastoral Reading)").
narrative_ontology:topic_domain(marriage_sacrament__civic_pastoral_reading, "religious_doctrine/canon_law/political_sociology").

domain_priors:requires_active_enforcement(marriage_sacrament__civic_pastoral_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_sacrament__civic_pastoral_reading, '96ffc5f9-96f4-47ee-842d-449b69e4869e').
narrative_ontology:cs_kernel_codification('96ffc5f9-96f4-47ee-842d-449b69e4869e', formalized).
narrative_ontology:cs_authority_grounding('96ffc5f9-96f4-47ee-842d-449b69e4869e', lineage).
narrative_ontology:cs_interpretation_layer_present('96ffc5f9-96f4-47ee-842d-449b69e4869e').
narrative_ontology:cs_reading_relation('96ffc5f9-96f4-47ee-842d-449b69e4869e', marriage_sacrament__hierarchical_indissolubility_reading, coexists_with).
narrative_ontology:cs_axiom('96ffc5f9-96f4-47ee-842d-449b69e4869e', foundational, indissolubility_as_ideal_for_discernment).
narrative_ontology:cs_axiom_status(indissolubility_as_ideal_for_discernment, holdable).
narrative_ontology:cs_axiom_grounding('96ffc5f9-96f4-47ee-842d-449b69e4869e', indissolubility_as_ideal_for_discernment, deontological).
narrative_ontology:cs_axiom('96ffc5f9-96f4-47ee-842d-449b69e4869e', foundational, pastoral_care_trumps_rigid_application).
narrative_ontology:cs_axiom_status(pastoral_care_trumps_rigid_application, holdable).
narrative_ontology:cs_axiom_grounding('96ffc5f9-96f4-47ee-842d-449b69e4869e', pastoral_care_trumps_rigid_application, deontological).
narrative_ontology:cs_reference_frame('96ffc5f9-96f4-47ee-842d-449b69e4869e', compassionate_pastoral_tradition).
narrative_ontology:cs_drift_state('96ffc5f9-96f4-47ee-842d-449b69e4869e', contemporary_theological_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('96ffc5f9-96f4-47ee-842d-449b69e4869e', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(marriage_sacrament__civic_pastoral_reading, marriage_sacrament).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_sacrament__civic_pastoral_reading, pastoral_clergy).
narrative_ontology:constraint_beneficiary(marriage_sacrament__civic_pastoral_reading, laity_seeking_discernment).
narrative_ontology:constraint_victim(marriage_sacrament__civic_pastoral_reading, traditional_catholics).
narrative_ontology:constraint_victim(marriage_sacrament__civic_pastoral_reading, doctrinal_conservatives).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Tasked with applying doctrinal ideals to complex individual lives, emphasizing compassion and discernment. They benefit from flexibility in pastoral care but face pressure from both traditionalists and those seeking greater liberalization.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, pastoral_clergy, agenda_setter,
    institutional, biographical, constrained, local).

% Seek compassionate guidance and potential pathways for reconciliation or annulment in difficult marital situations. They benefit from a less rigid application of doctrine but may experience uncertainty due to inconsistent interpretations.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, laity_seeking_discernment, beneficiary,
    moderate, immediate, constrained, local).

% Experience a perceived erosion of doctrinal clarity and normative stability regarding marriage. Their identity is deeply tied to the Church's consistent teaching, and they bear the cost of internal conflict and relativization of core beliefs.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, traditional_catholics, payer,
    organized, generational, identity_locked, global).

% Advocate for strict adherence to traditional interpretations of indissolubility. They bear the cost of perceived doctrinal drift and inconsistent enforcement, leading to internal dissent and challenges to hierarchical authority.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, doctrinal_conservatives, payer,
    powerful, generational, constrained, global).

% Navigates the tension between pastoral needs and doctrinal consistency. They set the overall direction for discernment processes, aiming to maintain unity while adapting to contemporary challenges. They benefit from perceived relevance but risk internal schism.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, institutional_hierarchy, agenda_setter,
    institutional, civilizational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the application of a sacred ideal (indissolubility) to the lived realities of individuals, providing a framework for pastoral care and discernment in complex marital situations.
% TRANSFER_FUNCTION: Transfers interpretive flexibility and pastoral discretion to local clergy, while transferring a sense of doctrinal relativization and normative uncertainty to traditionalist segments of the laity.
% ABSENT_VOICES: Those who have left the Church due to perceived rigidity or lack of pastoral care might advocate for even greater flexibility, while those who have left due to perceived doctrinal laxity might demand a return to strict adherence. Both groups are largely outside the current internal discourse.
% DISAPPEARANCE_RATIONALE: If this reading of marriage vanished, the Church's approach to marital failure would either revert to a more rigid, less compassionate stance (as in the hierarchical-indissolubility reading) or fragment into diverse, uncoordinated local practices. The pastoral landscape would fundamentally reorganize.
% FOUNDING_PROBLEM: The challenge of reconciling the ideal of lifelong, indissoluble marriage with the reality of human failure, suffering, and the need for pastoral care for individuals in broken relationships.
% FOUNDING_PROBLEM_CORROBORATION: Pastoral experience across dioceses, theological reflections on mercy, and sociological data on marital breakdown all corroborate the ongoing nature of this problem. This is attested by a broad range of clergy, theologians, and social scientists, not just those who benefit from the current reading.
narrative_ontology:disappearance_verdict(marriage_sacrament__civic_pastoral_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_sacrament__civic_pastoral_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_sacrament__civic_pastoral_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(marriage_sacrament__civic_pastoral_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_sacrament__civic_pastoral_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_sacrament__civic_pastoral_reading_tests).
:- end_tests(marriage_sacrament__civic_pastoral_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) arises from the perceived cost to traditionalists who experience doctrinal relativization and loss of normative clarity. Suppression (0.30) is present in the institutional pressure to conform to the pastoral approach, but it is not overtly coercive. The theater ratio (0.20) reflects some performative aspects of maintaining unity while navigating internal dissent. The claimed type is Tangled Rope because it genuinely coordinates pastoral care while extracting from those who value strict adherence.
 *
 * PERSPECTIVAL GAP:
 *   Pastoral clergy and laity seeking discernment experience this as a beneficial, compassionate framework. Traditional Catholics and doctrinal conservatives, however, experience it as an extractive force that undermines core tenets of their faith. The engine's per-seat classification will reflect this divergence based on their declared roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Pastoral clergy and laity seeking discernment are beneficiaries (low d) as they gain flexibility and support. Traditional Catholics and doctrinal conservatives are payers/victims (high d) as they bear the costs of doctrinal ambiguity and perceived erosion. The institutional hierarchy sits in a more symmetric position, balancing competing demands.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling genuine pastoral coordination as pure extraction. While there is extraction from certain groups, the constraint also serves a real function in providing care and guidance within the religious framework. The 'contested' status of the founding problem highlights the ongoing tension between the ideal and its application, preventing a premature declaration of mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    doctrinal_coherence_cost,
    'What is the long-term cost to doctrinal coherence and institutional unity of maintaining a flexible, discernment-based approach to indissolubility?',
    'Longitudinal studies of theological education, catechesis, and internal surveys of belief among different segments of the laity and clergy over several decades.',
    'If the cost is severe, the constraint''s effective extraction from traditionalists is higher than currently measured, potentially leading to schism or a reclassification towards Snare for those groups. If the cost is low, the pastoral approach is more sustainable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_coherence_cost, empirical, 'Assesses the impact of pastoral flexibility on doctrinal consistency.').

omega_variable(
    pastoral_care_efficacy,
    'Does the discernment-based approach genuinely improve pastoral care and spiritual well-being for individuals in difficult marital situations, or does it create new forms of uncertainty and anxiety?',
    'Qualitative and quantitative studies on the experiences of laity undergoing discernment processes, comparing outcomes with more rigid or more liberal approaches.',
    'If efficacy is low, the coordination function is weaker than claimed, increasing the effective extraction from all parties and potentially shifting the classification towards a more extractive type. If efficacy is high, the coordination function is robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pastoral_care_efficacy, empirical, 'Evaluates the effectiveness of the pastoral approach in practice.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.30) primarily structural (institutional pressure to conform) or internalized (traditionalists'' self-censorship to maintain identity within the Church)?',
    'Analysis of dissent channels, disciplinary actions against traditionalist clergy/theologians, and surveys of traditionalist laity regarding their perceived freedom to express dissent without fear of reprisal or marginalization.',
    'If internalized, the constraint''s effective suppression on traditionalists is higher than the structural measure suggests, as they carry the suppression with them. If structural, the institutional enforcement is more direct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for traditionalist dissent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_sacrament__civic_pastoral_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_sacrament__civic_pastoral_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(marr_tr_t5, marriage_sacrament__civic_pastoral_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement(marr_tr_t10, marriage_sacrament__civic_pastoral_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(marr_tr_t15, marriage_sacrament__civic_pastoral_reading, theater_ratio, 15, 0.25).
narrative_ontology:measurement(marr_tr_t20, marriage_sacrament__civic_pastoral_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(marr_be_t5, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(marr_be_t10, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(marr_be_t15, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(marr_be_t20, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 20, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(marr_su_t5, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 5, 0.28).
narrative_ontology:measurement(marr_su_t10, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 10, 0.3).
narrative_ontology:measurement(marr_su_t15, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 15, 0.32).
narrative_ontology:measurement(marr_su_t20, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 20, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_sacrament__civic_pastoral_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is the 'civic_pastoral_reading' of the 'marriage_sacrament' kernel, distinct from the 'hierarchical_indissolubility_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
