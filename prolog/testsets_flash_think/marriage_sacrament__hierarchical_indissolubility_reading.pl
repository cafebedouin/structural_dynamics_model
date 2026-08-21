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
 *   constraint_id: marriage_sacrament__hierarchical_indissolubility_reading
 *   human_readable: Hierarchical Indissolubility of Sacramental Marriage
 *   domain: religious/social/legal
 *
 * SUMMARY:
 *   This constraint describes the 'hierarchical_indissolubility_reading' of
 *   the 'marriage_sacrament' kernel within the Catholic Church. It posits
 *   marriage as an ontological reality, divinely instituted and inherently
 *   indissoluble, requiring strict hierarchical adjudication. This
 *   interpretation leads to significant extraction from divorced and civilly
 *   remarried Catholics, who are excluded from full sacramental participation
 *   unless an annulment is granted through a canonical process. The high
 *   extractiveness and suppression reflect the spiritual and social costs
 *   imposed by this doctrine and its enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_sacrament__hierarchical_indissolubility_reading, 0.8).
domain_priors:suppression_score(marriage_sacrament__hierarchical_indissolubility_reading, 0.85).
domain_priors:theater_ratio(marriage_sacrament__hierarchical_indissolubility_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_sacrament__hierarchical_indissolubility_reading, tangled_rope).
narrative_ontology:human_readable(marriage_sacrament__hierarchical_indissolubility_reading, "Hierarchical Indissolubility of Sacramental Marriage").
narrative_ontology:topic_domain(marriage_sacrament__hierarchical_indissolubility_reading, "religious/social/legal").

domain_priors:requires_active_enforcement(marriage_sacrament__hierarchical_indissolubility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_sacrament__hierarchical_indissolubility_reading, '923011bd-a839-47a9-b439-cbabaaff4356').
narrative_ontology:cs_kernel_codification('923011bd-a839-47a9-b439-cbabaaff4356', formalized).
narrative_ontology:cs_authority_grounding('923011bd-a839-47a9-b439-cbabaaff4356', lineage).
narrative_ontology:cs_interpretation_layer_present('923011bd-a839-47a9-b439-cbabaaff4356').
narrative_ontology:cs_reading_relation('923011bd-a839-47a9-b439-cbabaaff4356', marriage_sacrament__civic_pastoral_reading, coexists_with).
narrative_ontology:cs_axiom('923011bd-a839-47a9-b439-cbabaaff4356', foundational, marriage_ontologically_indissoluble).
narrative_ontology:cs_axiom_status(marriage_ontologically_indissoluble, holdable).
narrative_ontology:cs_axiom_grounding('923011bd-a839-47a9-b439-cbabaaff4356', marriage_ontologically_indissoluble, theological).
narrative_ontology:cs_axiom('923011bd-a839-47a9-b439-cbabaaff4356', secondary, ecclesiastical_hierarchy_sole_adjudicator).
narrative_ontology:cs_axiom_status(ecclesiastical_hierarchy_sole_adjudicator, holdable).
narrative_ontology:cs_axiom_grounding('923011bd-a839-47a9-b439-cbabaaff4356', ecclesiastical_hierarchy_sole_adjudicator, conventional).
narrative_ontology:cs_reference_frame('923011bd-a839-47a9-b439-cbabaaff4356', traditional_sacramental_theology).
narrative_ontology:cs_drift_state('923011bd-a839-47a9-b439-cbabaaff4356', contemporary_secular_context, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('923011bd-a839-47a9-b439-cbabaaff4356', '').
narrative_ontology:cs_kernel_id(marriage_sacrament__hierarchical_indissolubility_reading, marriage_sacrament).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_sacrament__hierarchical_indissolubility_reading, ecclesiastical_hierarchy).
narrative_ontology:constraint_beneficiary(marriage_sacrament__hierarchical_indissolubility_reading, faithful_adherents).
narrative_ontology:constraint_victim(marriage_sacrament__hierarchical_indissolubility_reading, divorced_remarried_catholics).
narrative_ontology:constraint_victim(marriage_sacrament__hierarchical_indissolubility_reading, catholics_seeking_annulment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines, interprets, and enforces canon law regarding marriage, maintaining doctrinal purity and institutional authority. Benefits from adherence to its interpretation and the stability it provides to the Church's moral framework.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, ecclesiastical_hierarchy, agenda_setter,
    institutional, generational, arbitrage, global).

% Excluded from full sacramental participation (e.g., Eucharist) unless an annulment is granted, which is a costly and lengthy process. Their identity as Catholics is deeply tied to the faith, making exit difficult despite the spiritual cost.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, divorced_remarried_catholics, payer,
    powerless, biographical, identity_locked, global).

% Must navigate a complex, often expensive, and emotionally taxing canonical process to have their previous marriage declared null, allowing them to remarry within the Church. The process itself is a significant burden.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, catholics_seeking_annulment, payer,
    moderate, biographical, constrained, global).

% Benefit from the clarity and stability of the Church's teaching on marriage, which provides a clear moral framework and sense of tradition. They are expected to uphold the doctrine and find spiritual security in its consistency.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, faithful_adherents, beneficiary,
    moderate, generational, constrained, global).

% Interpret and debate the theological and legal implications of marriage doctrine, sometimes advocating for reform or stricter adherence. Their work shapes the intellectual discourse around the constraint.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, theologians_canon_lawyers, observer,
    institutional, generational, analytical, global).

% While they recognize civil marriage, they have no authority over the sacramental definition or its canonical enforcement, which operates independently. They are structurally outside the Church's internal legal and theological framework.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, civic_authorities, excluded,
    institutional, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_sacrament__hierarchical_indissolubility_reading, ecclesiastical_hierarchy).
narrative_ontology:fixing_cost_class(marriage_sacrament__hierarchical_indissolubility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a consistent, universal definition and understanding of sacramental marriage for all adherents, providing a stable moral and theological framework for family life within the Church, and defining who is in full communion.
% TRANSFER_FUNCTION: Transfers spiritual and social capital (full sacramental participation, status within the community) from divorced/remarried Catholics to the ecclesiastical hierarchy, in exchange for doctrinal purity and institutional authority. It also transfers time, effort, and financial resources from those seeking annulment to the canonical tribunals.
% ABSENT_VOICES: Catholics who have left the Church due to its marriage doctrine, or those who advocate for a more inclusive, less rigid interpretation from outside the formal hierarchy. They would argue for a more compassionate approach that prioritizes pastoral care over strict canonical adherence.
% DISAPPEARANCE_RATIONALE: If the hierarchical adjudication and indissolubility doctrine vanished, the Catholic understanding of marriage, family life, and sacramental participation would fundamentally reorganize. Many previously excluded individuals would return to full participation, and the Church's authority structure would be significantly challenged, leading to a profound shift in its identity and practice.
% FOUNDING_PROBLEM: To define and protect the sanctity and permanence of marriage as a sacrament, reflecting divine law and ensuring the spiritual well-being of the faithful, in contrast to evolving secular understandings of marriage and to prevent arbitrary dissolution.
% FOUNDING_PROBLEM_CORROBORATION: The ecclesiastical hierarchy and many faithful adherents attest that the problem of maintaining the sanctity and permanence of marriage in a changing world is still live. Critics (e.g., some theologians, former Catholics) argue that the problem has shifted from protecting sanctity to maintaining institutional power, but within the Church, the problem is widely considered live.
narrative_ontology:disappearance_verdict(marriage_sacrament__hierarchical_indissolubility_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_sacrament__hierarchical_indissolubility_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_sacrament__hierarchical_indissolubility_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(marriage_sacrament__hierarchical_indissolubility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_sacrament__hierarchical_indissolubility_reading, 0.8, 'gemini-2.5-flash', 'none', direct).

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
 *   Extraction is high (0.8) due to the severe spiritual and social penalties (exclusion from Eucharist) and the significant burdens (time, cost, emotional toll) of the annulment process. Suppression is also very high (0.85) because the identity-locked nature of Catholic faith makes exit extremely difficult for many, and the institutional enforcement is robust. Theater ratio is low (0.1) as the enforcement mechanisms (canonical tribunals, denial of sacraments) are genuinely functional and central to maintaining doctrinal purity. The increasing extractiveness and suppression over the interval reflect the growing divergence between secular societal norms regarding divorce and the Church's unchanging doctrine, leading to more individuals falling under the constraint's extractive force.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the ecclesiastical hierarchy, this constraint is a necessary 'rope' for maintaining the integrity of a divine institution and guiding the faithful. From the perspective of divorced/remarried Catholics, it operates as a 'snare' or 'tangled_rope', imposing severe penalties and limiting spiritual participation based on a rigid interpretation of doctrine. The engine's computation of per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The ecclesiastical hierarchy is the primary beneficiary (d near 0.0) as it maintains its authority, doctrinal purity, and the integrity of the sacramental system. Faithful adherents also benefit from the clarity and stability of the doctrine. Divorced/remarried Catholics and those seeking annulment are the primary targets (d near 1.0) as they bear the direct costs of exclusion and the annulment process. Their identity-locked status amplifies their directionality towards the target end.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_identification,
    'Is this constraint accurately identified as the ''hierarchical_indissolubility_reading'' of the ''marriage_sacrament'' kernel?',
    'Analysis of canonical texts, magisterial documents, and theological interpretations to confirm the emphasis on ontological reality, constitutive indissolubility, and hierarchical adjudication.',
    'If misidentified, the analysis of this specific reading''s structural properties and its relation to sibling readings would be flawed, leading to incorrect classification and network effects.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_identification, conceptual, 'Confirmation of the specific kernel reading being analyzed.').

omega_variable(
    civic_pastoral_reading_impact,
    'What would be the structural impact on extractiveness and suppression if the ''civic_pastoral_reading'' were adopted as the dominant interpretation?',
    'Comparative analysis with religious traditions or internal Church movements that prioritize pastoral discernment and individual conscience over strict canonical enforcement in marriage cases.',
    'If the ''civic_pastoral_reading'' became dominant, extractiveness and suppression for divorced/remarried Catholics would likely decrease substantially, as the focus would shift from exclusion to inclusion and support, potentially reclassifying the constraint from a Tangled Rope towards a Rope or Scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civic_pastoral_reading_impact, empirical, 'Impact of alternative reading on constraint metrics.').

omega_variable(
    indissolubility_nature_locus_disagreement,
    'Is the core disagreement between readings primarily about the nature of indissolubility (constitutive vs. aspirational) or the locus of adjudication (hierarchy vs. pastoral discernment)?',
    'Detailed textual analysis of arguments from proponents of each reading, identifying the most fundamental points of contention and their theological/canonical grounding.',
    'Clarifying the locus of disagreement helps to precisely define the ''axioms'' and ''reading_relations'' in the commitment system structure, improving the accuracy of kernel-level analysis and potential for resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indissolubility_nature_locus_disagreement, conceptual, 'Pinpointing the fundamental point of divergence between kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_sacrament__hierarchical_indissolubility_reading, 1980, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1980, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(marr_tr_t1990, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(marr_tr_t2000, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(marr_tr_t2010, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(marr_tr_t2020, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 2020, 0.1).

% Extraction over time
narrative_ontology:measurement(marr_be_t1980, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 1980, 0.72).
narrative_ontology:measurement(marr_be_t1990, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 1990, 0.75).
narrative_ontology:measurement(marr_be_t2000, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 2000, 0.78).
narrative_ontology:measurement(marr_be_t2010, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 2010, 0.8).
narrative_ontology:measurement(marr_be_t2020, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 2020, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1980, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 1980, 0.8).
narrative_ontology:measurement(marr_su_t1990, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 1990, 0.82).
narrative_ontology:measurement(marr_su_t2000, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 2000, 0.83).
narrative_ontology:measurement(marr_su_t2010, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 2010, 0.84).
narrative_ontology:measurement(marr_su_t2020, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 2020, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_sacrament__hierarchical_indissolubility_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
