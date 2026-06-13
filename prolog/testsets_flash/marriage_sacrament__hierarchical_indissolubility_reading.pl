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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: marriage_sacrament__hierarchical_indissolubility_reading
 *   human_readable: Marriage as Ontological Reality (Hierarchical Indissolubility Reading)
 *   domain: religious_doctrine/canon_law/political_sociology
 *
 * SUMMARY:
 *   This constraint represents the 'hierarchical indissolubility' reading of
 *   the marriage sacrament within a specific religious tradition. It asserts
 *   marriage as an ontological reality, a divinely instituted bond that is
 *   inherently indissoluble, requiring strict hierarchical adjudication
 *   rather than pastoral discernment in cases of marital breakdown. This
 *   reading leads to the exclusion of divorced and civilly remarried
 *   individuals from full sacramental participation, and subjects those
 *   seeking to regularize their status to a formal, often burdensome,
 *   annulment process. The constraint is actively enforced by the
 *   ecclesiastical hierarchy.
 *
 * KEY AGENTS:
 *   - ecclesiastical_hierarchy: Agenda setter (institutional/arbitrage) — enforces canonical law, adjudicates annulments, maintains doctrinal purity.
 *   - divorced_remarried_catholics: Primary target (powerless/identity_locked) — excluded from sacraments, bear the social and spiritual cost of non-compliance.
 *   - catholics_seeking_annulment: Payer (moderate/constrained) — bear the costs (financial, emotional, temporal) of the annulment process.
 *   - traditionalist_catholics: Beneficiary (organized/mobile) — benefit from the perceived stability and doctrinal clarity of the strict interpretation.
 *   - pastoral_advocates: Excluded (organized/constrained) — advocate for more compassionate approaches but lack formal authority to alter the constraint.
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
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_sacrament__hierarchical_indissolubility_reading, snare).
narrative_ontology:human_readable(marriage_sacrament__hierarchical_indissolubility_reading, "Marriage as Ontological Reality (Hierarchical Indissolubility Reading)").
narrative_ontology:topic_domain(marriage_sacrament__hierarchical_indissolubility_reading, "religious_doctrine/canon_law/political_sociology").

domain_priors:requires_active_enforcement(marriage_sacrament__hierarchical_indissolubility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_sacrament__hierarchical_indissolubility_reading, 'b7046704-3b7d-4dd5-b6e2-6e51beeeed84').
narrative_ontology:cs_kernel_codification('b7046704-3b7d-4dd5-b6e2-6e51beeeed84', fixed_text).
narrative_ontology:cs_authority_grounding('b7046704-3b7d-4dd5-b6e2-6e51beeeed84', lineage).
narrative_ontology:cs_interpretation_layer_present('b7046704-3b7d-4dd5-b6e2-6e51beeeed84').
narrative_ontology:cs_reading_relation('b7046704-3b7d-4dd5-b6e2-6e51beeeed84', marriage_sacrament__civic_pastoral_reading, coexists_with).
narrative_ontology:cs_axiom('b7046704-3b7d-4dd5-b6e2-6e51beeeed84', foundational, marriage_is_ontological_indissoluble).
narrative_ontology:cs_axiom_status(marriage_is_ontological_indissoluble, holdable).
narrative_ontology:cs_axiom_grounding('b7046704-3b7d-4dd5-b6e2-6e51beeeed84', marriage_is_ontological_indissoluble, deontological).
narrative_ontology:cs_axiom('b7046704-3b7d-4dd5-b6e2-6e51beeeed84', foundational, hierarchical_adjudication_of_sacraments).
narrative_ontology:cs_axiom_status(hierarchical_adjudication_of_sacraments, holdable).
narrative_ontology:cs_axiom_grounding('b7046704-3b7d-4dd5-b6e2-6e51beeeed84', hierarchical_adjudication_of_sacraments, conventional).
narrative_ontology:cs_reference_frame('b7046704-3b7d-4dd5-b6e2-6e51beeeed84', tridentine_canonical_orthodoxy).
narrative_ontology:cs_drift_state('b7046704-3b7d-4dd5-b6e2-6e51beeeed84', contemporary_pastoral_challenges, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b7046704-3b7d-4dd5-b6e2-6e51beeeed84', '').
narrative_ontology:cs_kernel_id(marriage_sacrament__hierarchical_indissolubility_reading, marriage_sacrament).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_sacrament__hierarchical_indissolubility_reading, ecclesiastical_hierarchy).
narrative_ontology:constraint_beneficiary(marriage_sacrament__hierarchical_indissolubility_reading, traditionalist_catholics).
narrative_ontology:constraint_victim(marriage_sacrament__hierarchical_indissolubility_reading, divorced_remarried_catholics).
narrative_ontology:constraint_victim(marriage_sacrament__hierarchical_indissolubility_reading, catholics_seeking_annulment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines, interprets, and enforces canon law regarding marriage. Adjudicates annulment cases and determines sacramental eligibility. Benefits from maintaining doctrinal consistency and institutional authority.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, ecclesiastical_hierarchy, agenda_setter,
    institutional, generational, arbitrage, global).

% Are excluded from receiving the Eucharist and other sacraments due to their marital status, unless they obtain an annulment or live in continence. They face spiritual and social marginalization within their faith community, but often remain 'identity_locked' due to deep personal faith and community ties.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, divorced_remarried_catholics, payer,
    powerless, biographical, identity_locked, local).

% Undergo a lengthy, often emotionally taxing, and sometimes costly canonical process to determine if their prior marriage was sacramentally invalid. Their access to full sacramental life is contingent on the tribunal's decision, which can take years.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, catholics_seeking_annulment, payer,
    moderate, biographical, constrained, local).

% Strongly affirm the strict interpretation of marriage indissolubility and hierarchical authority. They benefit from the perceived clarity, stability, and 'purity' of doctrine, which reinforces their identity and worldview within the Church.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, traditionalist_catholics, beneficiary,
    organized, generational, mobile, global).

% Clergy and laypersons who advocate for more compassionate and inclusive pastoral approaches to divorced and remarried Catholics, often emphasizing mercy and individual conscience. While they operate within the Church, their views are often marginalized by the hierarchy, and they lack the authority to change canonical law.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, pastoral_advocates, excluded,
    organized, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_sacrament__hierarchical_indissolubility_reading, ecclesiastical_hierarchy).
narrative_ontology:fixing_cost_class(marriage_sacrament__hierarchical_indissolubility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the understanding and practice of marriage as a sacred, indissoluble bond, providing a consistent theological and legal framework for marital status within the religious tradition.
% TRANSFER_FUNCTION: Transfers spiritual and social standing (full sacramental participation, community recognition) from divorced/remarried Catholics to the ecclesiastical hierarchy, which controls access to these goods. It also transfers time, emotional labor, and sometimes financial resources from those seeking annulment to the canonical tribunals.
% ABSENT_VOICES: The voices of those who have left the Church due to these restrictions, or those who advocate for a more decentralized, conscience-based approach to marital status, are largely absent from the formal canonical discourse. They would argue for a re-evaluation of indissolubility in light of lived experience and pastoral needs.
% DISAPPEARANCE_RATIONALE: If this hierarchical indissolubility constraint vanished overnight, the religious tradition's understanding of marriage would fundamentally shift. Divorced and remarried Catholics would likely be welcomed back to full sacramental life, the annulment process would become obsolete or radically reformed, and the authority structure of the ecclesiastical hierarchy would be significantly challenged, leading to a major reorganization of religious practice and doctrine.
% FOUNDING_PROBLEM: The founding problem was to define and preserve the sanctity and permanence of marriage as a sacrament, reflecting divine will and providing a stable foundation for family and society within the religious tradition, particularly in response to historical challenges to marital stability.
% FOUNDING_PROBLEM_CORROBORATION: The ecclesiastical hierarchy attests that the problem of preserving sacramental marriage is still live and requires strict enforcement. However, pastoral advocates and many lay Catholics, supported by sociological studies of marital breakdown, contend that while the ideal of permanence remains, the current hierarchical adjudication and exclusion mechanisms are counterproductive to pastoral care and have become a source of alienation, suggesting the founding problem's status is contested in its application.
narrative_ontology:disappearance_verdict(marriage_sacrament__hierarchical_indissolubility_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_sacrament__hierarchical_indissolubility_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_sacrament__hierarchical_indissolubility_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(marriage_sacrament__hierarchical_indissolubility_reading, 'none', 1).

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
 *   The high extractiveness (0.85) stems from the significant spiritual and social costs imposed on divorced and remarried Catholics, including exclusion from the Eucharist. Suppression (0.90) is high due to the absolute authority of the ecclesiastical hierarchy in defining and enforcing sacramental validity, with no legitimate internal alternatives for those who wish to remain within the tradition. The low theater ratio (0.10) reflects that the enforcement is genuinely aimed at upholding the stated doctrine, not merely for show, though the process itself can be performative. Accessibility collapse is high (0.75) because for those committed to the tradition, there are few viable alternatives to the prescribed canonical process or exclusion.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the ecclesiastical hierarchy and traditionalist Catholics, this constraint is a 'mountain' or 'rope' – an unchangeable divine law or a necessary coordination for spiritual integrity. From the perspective of divorced/remarried Catholics and those seeking annulment, it operates as a 'snare' due to its high extraction, suppression, and limited exit options within their faith tradition. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The ecclesiastical hierarchy and traditionalist Catholics are beneficiaries (d near 0.0-0.1) as they uphold and benefit from the doctrinal clarity and institutional power derived from this reading. Divorced/remarried Catholics are clear targets (d near 1.0) as they bear the full cost of exclusion. Catholics seeking annulment are also targets (d near 0.8) as they navigate a costly and uncertain process. Pastoral advocates, while not directly paying, are structurally constrained in their ability to influence the constraint, placing them in a 'constrained' exit option.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate is to uphold the ontological reality and indissolubility of marriage. While the problem of marital breakdown is 'live', the 'founding_problem_status' is 'contested' because critics argue the current enforcement mechanism has outlived its pastoral utility and now primarily serves institutional power. The high extractiveness and suppression, coupled with the contested status of the founding problem, suggest a potential for mandatrophy, where the original spiritual purpose is overshadowed by institutional maintenance and control. The classification as 'snare' reflects this potential for the constraint to persist due to its extractive function rather than its original coordination purpose.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine ontological reality, or a constructed interpretation that benefits identifiable agents?',
    'Analysis of alternative theological interpretations and their practical consequences for agents, particularly the ''civic_pastoral_reading'' sibling.',
    'If a constructed interpretation, the constraint''s classification shifts from a perceived ''mountain'' (by its proponents) to a ''snare'' or ''tangled_rope'' for those it extracts from. This reading is one of two primary interpretations of the ''marriage_sacrament'' kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is the ''hierarchical_indissolubility_reading'' of the ''marriage_sacrament'' kernel. A sibling reading, ''civic_pastoral_reading'', would emphasize compassion and discernment over strict adjudication, leading to different victim sets and extractiveness.').

omega_variable(
    annulment_process_fairness,
    'Is the annulment process a genuine search for truth regarding sacramental validity, or an administrative hurdle designed to reinforce indissolubility and extract compliance?',
    'Empirical study of annulment outcomes, processing times, costs, and consistency across tribunals, compared to stated canonical principles.',
    'If primarily an administrative hurdle, the extractiveness and suppression metrics for ''catholics_seeking_annulment'' are higher than currently estimated, and the ''theater_ratio'' would increase, indicating performative rather than functional activity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(annulment_process_fairness, empirical, 'Assesses the functional integrity of the annulment process.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_sacrament__hierarchical_indissolubility_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(marr_tr_t10, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(marr_tr_t20, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(marr_be_t10, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 10, 0.8).
narrative_ontology:measurement(marr_be_t20, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 20, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(marr_su_t10, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 10, 0.85).
narrative_ontology:measurement(marr_su_t20, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 20, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_sacrament__hierarchical_indissolubility_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_sacrament__hierarchical_indissolubility_reading, marriage_sacrament__civic_pastoral_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'marriage_sacrament' kernel. It is linked to the 'civic_pastoral_reading' which offers an alternative interpretation of marriage indissolubility.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
