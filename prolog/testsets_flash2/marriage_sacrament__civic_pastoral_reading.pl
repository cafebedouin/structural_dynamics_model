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
 *   domain: religious/social/legal
 *
 * SUMMARY:
 *   This constraint represents a 'civic-pastoral' reading of the Catholic
 *   doctrine of marriage, emphasizing the pastoral relationship and
 *   compassionate discernment in individual cases, particularly regarding
 *   indissolubility and annulment. It acknowledges human failure and seeks to
 *   integrate individuals into the Church's life. This reading is one
 *   interpretation of the broader 'marriage_sacrament' kernel, contrasting
 *   with a more rigid 'hierarchical_indissolubility_reading'. The
 *   extractiveness is moderate, reflecting the cost borne by traditional
 *   Catholics who experience doctrinal relativization and loss of normative
 *   clarity, while pastoral clergy and laity seeking annulment benefit from
 *   increased flexibility.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_sacrament__civic_pastoral_reading, 0.58).
domain_priors:suppression_score(marriage_sacrament__civic_pastoral_reading, 0.45).
domain_priors:theater_ratio(marriage_sacrament__civic_pastoral_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_sacrament__civic_pastoral_reading, tangled_rope).
narrative_ontology:human_readable(marriage_sacrament__civic_pastoral_reading, "Marriage as Pastoral Relationship (Civic-Pastoral Reading)").
narrative_ontology:topic_domain(marriage_sacrament__civic_pastoral_reading, "religious/social/legal").

domain_priors:requires_active_enforcement(marriage_sacrament__civic_pastoral_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_sacrament__civic_pastoral_reading, '146efb9e-d5c4-42ae-b596-ff5b7bd2980b').
narrative_ontology:cs_kernel_codification('146efb9e-d5c4-42ae-b596-ff5b7bd2980b', formalized).
narrative_ontology:cs_authority_grounding('146efb9e-d5c4-42ae-b596-ff5b7bd2980b', lineage).
narrative_ontology:cs_interpretation_layer_present('146efb9e-d5c4-42ae-b596-ff5b7bd2980b').
narrative_ontology:cs_reading_relation('146efb9e-d5c4-42ae-b596-ff5b7bd2980b', marriage_sacrament__hierarchical_indissolubility_reading, coexists_with).
narrative_ontology:cs_axiom('146efb9e-d5c4-42ae-b596-ff5b7bd2980b', foundational, pastoral_care_primacy).
narrative_ontology:cs_axiom_status(pastoral_care_primacy, holdable).
narrative_ontology:cs_axiom_grounding('146efb9e-d5c4-42ae-b596-ff5b7bd2980b', pastoral_care_primacy, deontological).
narrative_ontology:cs_axiom('146efb9e-d5c4-42ae-b596-ff5b7bd2980b', foundational, human_failure_contextualizes_ideals).
narrative_ontology:cs_axiom_status(human_failure_contextualizes_ideals, holdable).
narrative_ontology:cs_axiom_grounding('146efb9e-d5c4-42ae-b596-ff5b7bd2980b', human_failure_contextualizes_ideals, conventional).
narrative_ontology:cs_reference_frame('146efb9e-d5c4-42ae-b596-ff5b7bd2980b', compassionate_pastoral_ministry).
narrative_ontology:cs_drift_state('146efb9e-d5c4-42ae-b596-ff5b7bd2980b', contemporary_church_practice, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('146efb9e-d5c4-42ae-b596-ff5b7bd2980b', '').
narrative_ontology:cs_kernel_id(marriage_sacrament__civic_pastoral_reading, marriage_sacrament).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_sacrament__civic_pastoral_reading, pastoral_clergy).
narrative_ontology:constraint_beneficiary(marriage_sacrament__civic_pastoral_reading, laity_seeking_annulment).
narrative_ontology:constraint_victim(marriage_sacrament__civic_pastoral_reading, traditional_catholics).
narrative_ontology:constraint_victim(marriage_sacrament__civic_pastoral_reading, doctrinal_conservatives).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Tasked with applying doctrine compassionately in individual cases, often navigating complex personal situations. This reading empowers them to offer discernment and pathways for those in difficult marital circumstances, sometimes leading to annulments that might be harder under a stricter interpretation. They benefit from increased pastoral flexibility but face pressure from doctrinal conservatives.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, pastoral_clergy, agenda_setter,
    institutional, biographical, constrained, local).

% Individuals in failed marriages who seek to regularize their status within the Church. This reading offers them a path to spiritual and sacramental peace through a more accessible annulment process, allowing for remarriage within the Church. Their identity is deeply tied to their Catholic faith, making exit unthinkable.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, laity_seeking_annulment, beneficiary,
    powerless, immediate, identity_locked, local).

% Laity who adhere to a strict interpretation of indissolubility as an ontological reality. They experience this reading as a relativization of doctrine, a loss of normative clarity, and an erosion of the sacrament's perceived sanctity. They bear the cost of doctrinal instability and feel their identity as faithful Catholics is challenged by perceived inconsistencies.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, traditional_catholics, payer,
    moderate, generational, constrained, global).

% Organized groups and theologians who advocate for a strict, unchanging interpretation of marriage doctrine. They perceive this reading as a departure from tradition and a threat to the Church's authority. They bear the cost of internal dissent and struggle to maintain doctrinal purity against what they see as pastoral pragmatism.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, doctrinal_conservatives, payer,
    organized, civilizational, constrained, global).

% The central Church authority that must balance pastoral needs with doctrinal consistency. They observe the effects of this reading on both the faithful and the institution's perceived integrity, weighing the benefits of compassion against the costs of doctrinal ambiguity and internal division.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, hierarchical_authority, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the Church's pastoral response to marital breakdown, providing a framework for clergy to offer compassionate care and discernment to individuals while upholding the ideal of indissolubility.
% TRANSFER_FUNCTION: Transfers pastoral flexibility and spiritual peace to individuals seeking annulment, while transferring a sense of doctrinal relativism and normative ambiguity to traditional Catholics and doctrinal conservatives.
% ABSENT_VOICES: Those who have left the Church due to perceived hypocrisy or rigidity in its marriage doctrine might argue that this reading is 'too little, too late' or that it still fails to address fundamental issues of human experience and autonomy.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the Church's pastoral approach to marriage would become significantly more rigid, leading to increased alienation for many laity in difficult marital situations. The annulment process would likely become much harder, and the internal theological debate would intensify, forcing a more direct confrontation between pastoral care and doctrinal absolutism.
% FOUNDING_PROBLEM: The Church faced the pastoral challenge of ministering to individuals whose marriages had failed, often through no fault of their own, while upholding the theological ideal of indissolubility. A rigid application led to many feeling excluded from the sacramental life.
% FOUNDING_PROBLEM_CORROBORATION: Pastoral clergy and many laity attest that the problem of reconciling doctrine with lived experience remains live. Doctrinal conservatives acknowledge the pastoral challenge but dispute the appropriateness of this reading's solution, arguing it creates new problems. Independent sociological studies of religious practice corroborate the ongoing tension between doctrine and lived reality for many Catholics.
narrative_ontology:disappearance_verdict(marriage_sacrament__civic_pastoral_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_sacrament__civic_pastoral_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_sacrament__civic_pastoral_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(marriage_sacrament__civic_pastoral_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_sacrament__civic_pastoral_reading, 0.58, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_sacrament__civic_pastoral_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_sacrament__civic_pastoral_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_sacrament__civic_pastoral_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58) because while some benefit from pastoral flexibility, others experience a significant cost in terms of doctrinal stability and identity. Suppression (0.45) is present as the institutional authority still enforces a particular interpretation, but it's less overt than in a purely hierarchical model, relying more on persuasion and internal pressure. Theater ratio (0.20) is low, as the pastoral function is genuine, but there's a degree of performative maintenance of the 'ideal' of indissolubility even as practical applications diverge. The metrics reflect a system in flux, where the balance between pastoral care and doctrinal rigor is actively negotiated.
 *
 * PERSPECTIVAL GAP:
 *   Pastoral clergy experience this as a necessary and compassionate adaptation, while traditional Catholics perceive it as a dangerous compromise. The engine's classification will reflect this divergence, showing a more beneficial outcome for those empowered by the flexibility and a more extractive one for those who feel their core beliefs are undermined.
 *
 * DIRECTIONALITY LOGIC:
 *   Pastoral clergy and laity seeking annulment are beneficiaries, as this reading empowers the former and provides pathways for the latter. Traditional Catholics and doctrinal conservatives are victims, as they bear the cost of doctrinal ambiguity and perceived erosion of tradition. Hierarchical authority acts as an observer, balancing competing demands. The 'identity_locked' exit for laity seeking annulment highlights their deep commitment to remaining within the Church, making alternative paths (leaving the Church) highly costly to their self-concept.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    doctrinal_coherence_vs_pastoral_flexibility,
    'Is the increased pastoral flexibility achieved by this reading sustainable without undermining the overall doctrinal coherence of the sacrament of marriage?',
    'Longitudinal study of theological discourse and lay adherence rates: if doctrinal coherence is maintained despite flexibility, the tension is resolved; if it leads to widespread theological relativism, it is not.',
    'If coherence is undermined, the extractiveness for traditional Catholics would increase further, potentially shifting the constraint towards a snare for them, as the ''coordination'' of pastoral care comes at the cost of fundamental belief. If sustainable, the constraint might move closer to a rope for all parties.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_coherence_vs_pastoral_flexibility, conceptual, 'The tension between doctrinal stability and pastoral adaptation.').

omega_variable(
    annulment_process_integrity,
    'Does the more accessible annulment process under this reading maintain the integrity of the Church''s teaching on the nullity of marriage, or does it become a de facto divorce mechanism?',
    'Analysis of annulment case outcomes and criteria over time, compared to historical precedents and theological definitions of nullity. If criteria for nullity remain distinct from civil divorce grounds, integrity is maintained.',
    'If it becomes a de facto divorce, the perceived theater ratio would increase significantly, and the extractiveness for traditional Catholics would rise, as the ''ideal'' of indissolubility becomes purely performative. If integrity is maintained, the pastoral benefits are more clearly justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(annulment_process_integrity, empirical, 'Whether annulment maintains its theological distinctiveness.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (institutional pressure to conform to the pastoral approach) or internalized (traditional Catholics'' self-censorship to avoid conflict within the Church)?',
    'Post-exit suppression trajectory: if traditional Catholics who leave the Church continue to experience internal conflict or self-censor on these issues, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression on traditional Catholics is higher than the structural measure suggests — they carry the suppression with them after exit, making their ''constrained'' exit options even more difficult.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for traditional Catholics.').


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
narrative_ontology:measurement(marr_tr_t10, marriage_sacrament__civic_pastoral_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(marr_tr_t15, marriage_sacrament__civic_pastoral_reading, theater_ratio, 15, 0.19).
narrative_ontology:measurement(marr_tr_t20, marriage_sacrament__civic_pastoral_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(marr_be_t5, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(marr_be_t10, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(marr_be_t15, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 15, 0.57).
narrative_ontology:measurement(marr_be_t20, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(marr_su_t5, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 5, 0.4).
narrative_ontology:measurement(marr_su_t10, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 10, 0.43).
narrative_ontology:measurement(marr_su_t15, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 15, 0.44).
narrative_ontology:measurement(marr_su_t20, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 20, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_sacrament__civic_pastoral_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is the 'civic_pastoral_reading' of the 'marriage_sacrament' kernel, which also includes the 'hierarchical_indissolubility_reading'. These readings represent competing interpretations of the same core doctrine.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
