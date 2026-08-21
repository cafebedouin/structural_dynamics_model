% ============================================================================
% CONSTRAINT STORY: marriage_sacrament__hierarchical_indissolubility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_sacrament__hierarchical_indissolubility_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: marriage_sacrament__hierarchical_indissolubility_reading
 *   human_readable: Marriage as Ontological Reality: Hierarchical Indissolubility Reading
 *   domain: religious_doctrine/canon_law/political_sociology
 *
 * SUMMARY:
 *   This constraint describes the Catholic Church's doctrine of marriage as
 *   an indissoluble sacrament, specifically from the 'hierarchical
 *   indissolubility' reading. This reading emphasizes marriage as an
 *   ontological reality requiring strict hierarchical adjudication, where
 *   indissolubility is constitutive rather than an aspirational ideal. The
 *   constraint's high extractiveness stems from the exclusion of divorced and
 *   remarried Catholics from full sacramental life and the burdensome
 *   annulment process. The claim of 'snare' reflects the view that the
 *   coordination story (preserving sacramental integrity) serves as cover for
 *   institutional control and extraction from those whose lives do not
 *   conform to the strict doctrinal interpretation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_sacrament__hierarchical_indissolubility_reading, 0.85).
domain_priors:suppression_score(marriage_sacrament__hierarchical_indissolubility_reading, 0.9).
domain_priors:theater_ratio(marriage_sacrament__hierarchical_indissolubility_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_sacrament__hierarchical_indissolubility_reading, snare).
narrative_ontology:human_readable(marriage_sacrament__hierarchical_indissolubility_reading, "Marriage as Ontological Reality: Hierarchical Indissolubility Reading").
narrative_ontology:topic_domain(marriage_sacrament__hierarchical_indissolubility_reading, "religious_doctrine/canon_law/political_sociology").

domain_priors:requires_active_enforcement(marriage_sacrament__hierarchical_indissolubility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_sacrament__hierarchical_indissolubility_reading, '131c5c14-9100-443a-9a21-e8744f550f54').
narrative_ontology:cs_kernel_codification('131c5c14-9100-443a-9a21-e8744f550f54', formalized).
narrative_ontology:cs_authority_grounding('131c5c14-9100-443a-9a21-e8744f550f54', lineage).
narrative_ontology:cs_interpretation_layer_present('131c5c14-9100-443a-9a21-e8744f550f54').
narrative_ontology:cs_reading_relation('131c5c14-9100-443a-9a21-e8744f550f54', marriage_sacrament__civic_pastoral_reading, coexists_with).
narrative_ontology:cs_axiom('131c5c14-9100-443a-9a21-e8744f550f54', foundational, marriage_is_ontologically_indissoluble).
narrative_ontology:cs_axiom_status(marriage_is_ontologically_indissoluble, holdable).
narrative_ontology:cs_axiom_grounding('131c5c14-9100-443a-9a21-e8744f550f54', marriage_is_ontologically_indissoluble, theological).
narrative_ontology:cs_axiom('131c5c14-9100-443a-9a21-e8744f550f54', foundational, magisterium_has_sole_adjudicatory_authority).
narrative_ontology:cs_axiom_status(magisterium_has_sole_adjudicatory_authority, holdable).
narrative_ontology:cs_axiom_grounding('131c5c14-9100-443a-9a21-e8744f550f54', magisterium_has_sole_adjudicatory_authority, conventional).
narrative_ontology:cs_reference_frame('131c5c14-9100-443a-9a21-e8744f550f54', tridentine_doctrinal_purity).
narrative_ontology:cs_drift_state('131c5c14-9100-443a-9a21-e8744f550f54', contemporary_secular_context, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('131c5c14-9100-443a-9a21-e8744f550f54', '').
narrative_ontology:cs_kernel_id(marriage_sacrament__hierarchical_indissolubility_reading, marriage_sacrament).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_sacrament__hierarchical_indissolubility_reading, magisterium).
narrative_ontology:constraint_beneficiary(marriage_sacrament__hierarchical_indissolubility_reading, canon_lawyers).
narrative_ontology:constraint_victim(marriage_sacrament__hierarchical_indissolubility_reading, divorced_remarried_catholics).
narrative_ontology:constraint_victim(marriage_sacrament__hierarchical_indissolubility_reading, catholic_laity_seeking_annulment).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(marriage_sacrament__hierarchical_indissolubility_reading, local_clergy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The teaching authority of the Catholic Church, which defines marriage as an indissoluble sacrament and adjudicates its validity. Benefits from maintaining doctrinal purity and institutional control over sacramental life.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, magisterium, agenda_setter,
    institutional, civilizational, identity_locked, universal).

% Are excluded from full sacramental participation, particularly the Eucharist, if they remarry without an annulment. They bear the spiritual and social cost of this exclusion, often feeling alienated from the Church while remaining committed to their Catholic identity.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, divorced_remarried_catholics, payer,
    powerless, biographical, identity_locked, local).

% Must navigate a complex, lengthy, and often costly annulment process through ecclesiastical tribunals to have a previous marriage declared null. They bear the procedural burden and emotional toll, with no guarantee of a favorable outcome.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, catholic_laity_seeking_annulment, payer,
    moderate, immediate, constrained, local).

% Professionals who specialize in canon law and assist individuals through the annulment process. They benefit from the complexity and necessity of the annulment system, providing a specialized service within the Church's legal framework.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, canon_lawyers, beneficiary,
    organized, biographical, mobile, national).

% Are tasked with enforcing the doctrinal position at the parish level, often leading to pastoral challenges and personal conflict when ministering to divorced and remarried parishioners. They bear the burden of upholding doctrine while facing pastoral needs.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, local_clergy, agenda_setter,
    institutional, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(marriage_sacrament__hierarchical_indissolubility_reading, local_clergy, payer).

% Analyze and interpret the theological foundations and ethical implications of the Church's teaching on marriage and indissolubility. They can influence doctrinal development but do not directly set or enforce policy.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, theologians_and_ethicists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a universal, unchanging definition of marriage as a sacrament, providing a stable theological and legal framework for all Catholics globally, ensuring consistency in doctrine and practice.
% TRANSFER_FUNCTION: Transfers spiritual and social capital (full sacramental participation, status within the community) from divorced and remarried Catholics to the institutional authority of the Magisterium, in exchange for upholding doctrinal purity and institutional control.
% ABSENT_VOICES: Many Catholics who have divorced and remarried, particularly those in cultures where civil divorce is common, feel their lived experience and pastoral needs are not adequately heard or addressed in the doctrinal formulation and enforcement. Their voices are often mediated through sympathetic clergy or theologians, but not directly integrated into the hierarchical decision-making process.
% DISAPPEARANCE_RATIONALE: If the hierarchical adjudication of indissolubility vanished overnight, the Catholic Church's sacramental theology and canon law regarding marriage would undergo a profound reorientation. Divorced and remarried Catholics would likely seek full sacramental participation, annulment tribunals would become obsolete, and the Magisterium's authority over marriage doctrine would be fundamentally challenged, leading to a significant reorganization of institutional power and pastoral practice.
% FOUNDING_PROBLEM: To define and protect the sacred, indissoluble nature of marriage as instituted by Christ, ensuring its theological integrity and preventing its dissolution by human will or civil law.
% FOUNDING_PROBLEM_CORROBORATION: The Magisterium and canon lawyers attest that the problem of upholding the sacred nature of marriage in a secularizing world is still live. Theologians and some laity, while acknowledging the ideal, contest the current enforcement mechanisms as disproportionate to the pastoral needs of the faithful, suggesting the problem's status is contested in its practical application rather than its theological ideal.
narrative_ontology:disappearance_verdict(marriage_sacrament__hierarchical_indissolubility_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_sacrament__hierarchical_indissolubility_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_sacrament__hierarchical_indissolubility_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(marriage_sacrament__hierarchical_indissolubility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_sacrament__hierarchical_indissolubility_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_sacrament__hierarchical_indissolubility_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_sacrament__hierarchical_indissolubility_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_sacrament__hierarchical_indissolubility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) due to the significant spiritual and social costs imposed on divorced and remarried Catholics, including exclusion from the Eucharist and the arduous annulment process. Suppression is also very high (0.9) because the Magisterium's authority is absolute within the Church's framework, leaving virtually no legitimate alternatives for those who wish to remain within the faith while remarrying. Theater ratio is low (0.1) as the enforcement of indissolubility is a core, actively maintained function, not a performative one. The metrics show a slight increase in extractiveness and suppression over time, reflecting a hardening of enforcement in response to increasing secularization and challenges to traditional doctrine.
 *
 * PERSPECTIVAL GAP:
 *   From the Magisterium's perspective, this is a 'mountain' or 'rope' – an unchangeable divine law or a necessary coordination mechanism for spiritual integrity. From the perspective of divorced and remarried Catholics, it operates as a 'snare' – a coercive mechanism that extracts spiritual participation and imposes significant burdens under the guise of divine law. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The Magisterium and canon lawyers are beneficiaries (d near 0.0): the former maintains doctrinal authority and institutional control, the latter benefit from the complex legal process. Divorced and remarried Catholics, along with those seeking annulment, are clear targets (d near 1.0): they bear the costs of exclusion and procedural burdens. Local clergy are agenda-setters but also payers, as they face the pastoral challenges of enforcing a strict doctrine on their parishioners.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    doctrinal_vs_pastoral_priority,
    'Is the primary purpose of the indissolubility doctrine to uphold an ontological truth about marriage, or to serve a pastoral function for the faithful?',
    'Analysis of papal encyclicals and synodal documents over time: shifts in emphasis from ontological claims to pastoral care would indicate a conceptual re-prioritization.',
    'If primarily pastoral, the high extractiveness and suppression would be re-evaluated against the goal of spiritual well-being, potentially leading to reclassification as a Tangled Rope or Scaffold. If primarily ontological, the current classification as Snare (from the victim''s seat) would remain robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_vs_pastoral_priority, conceptual, 'Ambiguity in the foundational priority of the doctrine.').

omega_variable(
    annulment_process_efficiency,
    'Is the annulment process genuinely designed for discerning marital nullity, or does its complexity and cost serve to deter applications and reinforce the indissolubility doctrine through procedural barriers?',
    'Empirical study of annulment tribunal processes, including average timeframes, costs, success rates, and comparison with civil divorce processes in various jurisdictions. Analysis of reforms aimed at streamlining the process and their actual impact.',
    'If the process is found to be intentionally burdensome, it would increase the measured suppression and extractiveness, solidifying the Snare classification. If it is genuinely efficient but complex, the classification might shift towards a Tangled Rope, acknowledging a legitimate (albeit costly) coordination function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(annulment_process_efficiency, empirical, 'Efficiency vs. deterrence in the annulment process.').

omega_variable(
    internalized_suppression_of_laity,
    'To what extent is the ''suppression'' experienced by divorced and remarried Catholics internalized (e.g., guilt, fear of spiritual consequences) versus purely structural (e.g., denial of sacraments)?',
    'Sociological and psychological studies of Catholic communities, examining the self-reported experiences of divorced and remarried individuals, their perceptions of sin and exclusion, and the impact of pastoral outreach efforts. If suppression persists after structural barriers are eased, it indicates internalized components.',
    'If internalized suppression is a significant factor, the effective suppression is higher than the structural measure suggests, as individuals carry the constraint with them even in the absence of direct enforcement. This would deepen the Snare classification by highlighting the identity-locked nature of the victim''s exit options.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_suppression_of_laity, empirical, 'Structural vs. internalized suppression mechanism for Catholic laity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_sacrament__hierarchical_indissolubility_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(marr_tr_t10, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 10, 0.13).
narrative_ontology:measurement(marr_tr_t20, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement(marr_tr_t30, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 30, 0.11).
narrative_ontology:measurement(marr_tr_t40, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(marr_tr_t50, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(marr_be_t10, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 10, 0.78).
narrative_ontology:measurement(marr_be_t20, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 20, 0.81).
narrative_ontology:measurement(marr_be_t30, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 30, 0.83).
narrative_ontology:measurement(marr_be_t40, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 40, 0.84).
narrative_ontology:measurement(marr_be_t50, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 50, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(marr_su_t10, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 10, 0.83).
narrative_ontology:measurement(marr_su_t20, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 20, 0.86).
narrative_ontology:measurement(marr_su_t30, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 30, 0.88).
narrative_ontology:measurement(marr_su_t40, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 40, 0.89).
narrative_ontology:measurement(marr_su_t50, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 50, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
