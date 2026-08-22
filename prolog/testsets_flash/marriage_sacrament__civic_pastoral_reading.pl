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
 *   constraint_id: marriage_sacrament__civic_pastoral_reading
 *   human_readable: Marriage as Pastoral Discernment (Civic-Pastoral Reading)
 *   domain: religious_doctrine/canon_law/political_sociology
 *
 * SUMMARY:
 *   This constraint describes the 'civic-pastoral' reading of the Catholic
 *   Church's doctrine of marriage, where indissolubility is an ideal to be
 *   compassionately discerned in individual cases, acknowledging human
 *   failure. This reading prioritizes pastoral care and inclusion, often
 *   leading to more accessible annulment processes or accommodations for
 *   those in irregular unions. While framed as a 'tangled_rope' due to its
 *   coordination function (pastoral care) and asymmetric extraction
 *   (doctrinal relativization for traditionalists), the claimed type is
 *   'rope' by the institutional hierarchy, reflecting their desired framing.
 *   The metrics, however, reflect the actual operation and its costs.
 *
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
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_sacrament__civic_pastoral_reading, tangled_rope).
narrative_ontology:human_readable(marriage_sacrament__civic_pastoral_reading, "Marriage as Pastoral Discernment (Civic-Pastoral Reading)").
narrative_ontology:topic_domain(marriage_sacrament__civic_pastoral_reading, "religious_doctrine/canon_law/political_sociology").

domain_priors:requires_active_enforcement(marriage_sacrament__civic_pastoral_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_sacrament__civic_pastoral_reading, 'dca6d5ba-98db-4fce-8706-788a6c88d185').
narrative_ontology:cs_kernel_codification('dca6d5ba-98db-4fce-8706-788a6c88d185', formalized).
narrative_ontology:cs_authority_grounding('dca6d5ba-98db-4fce-8706-788a6c88d185', lineage).
narrative_ontology:cs_interpretation_layer_present('dca6d5ba-98db-4fce-8706-788a6c88d185').
narrative_ontology:cs_reading_relation('dca6d5ba-98db-4fce-8706-788a6c88d185', marriage_sacrament__hierarchical_indissolubility_reading, coexists_with).
narrative_ontology:cs_axiom('dca6d5ba-98db-4fce-8706-788a6c88d185', foundational, indissolubility_as_pastoral_ideal).
narrative_ontology:cs_axiom_status(indissolubility_as_pastoral_ideal, holdable).
narrative_ontology:cs_axiom_grounding('dca6d5ba-98db-4fce-8706-788a6c88d185', indissolubility_as_pastoral_ideal, deontological).
narrative_ontology:cs_axiom('dca6d5ba-98db-4fce-8706-788a6c88d185', secondary, compassionate_discernment_in_individual_cases).
narrative_ontology:cs_axiom_status(compassionate_discernment_in_individual_cases, holdable).
narrative_ontology:cs_axiom_grounding('dca6d5ba-98db-4fce-8706-788a6c88d185', compassionate_discernment_in_individual_cases, instrumental).
narrative_ontology:cs_reference_frame('dca6d5ba-98db-4fce-8706-788a6c88d185', post_vatican_ii_pastoral_approach).
narrative_ontology:cs_drift_state('dca6d5ba-98db-4fce-8706-788a6c88d185', contemporary_synodal_process, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('dca6d5ba-98db-4fce-8706-788a6c88d185', '').
narrative_ontology:cs_kernel_id(marriage_sacrament__civic_pastoral_reading, marriage_sacrament).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_sacrament__civic_pastoral_reading, pastoral_clergy).
narrative_ontology:constraint_beneficiary(marriage_sacrament__civic_pastoral_reading, laity_seeking_annulment).
narrative_ontology:constraint_victim(marriage_sacrament__civic_pastoral_reading, traditional_laity).
narrative_ontology:constraint_victim(marriage_sacrament__civic_pastoral_reading, doctrinal_conservatives).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Tasked with applying doctrine compassionately in individual cases, often leading to annulments or accommodations for those in irregular unions. They benefit from increased flexibility and reduced pastoral burden, but face pressure from doctrinal conservatives.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, pastoral_clergy, agenda_setter,
    institutional, biographical, constrained, local).

% Directly benefit from a more lenient and pastoral approach to marriage nullity, allowing them to regularize their status within the Church after marital breakdown. Their options are limited by the availability and interpretation of canon law.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, laity_seeking_annulment, beneficiary,
    powerless, immediate, constrained, local).

% Experience a relativization of the doctrine of indissolubility, leading to a perceived loss of normative clarity and stability in their faith. Their identity is deeply tied to the Church's traditional teachings, making exit unthinkable despite their dissatisfaction.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, traditional_laity, payer,
    organized, generational, identity_locked, global).

% Bear the cost of what they perceive as doctrinal erosion and inconsistent application of canon law. They actively resist changes that dilute the traditional understanding of marriage, but their institutional power is often insufficient to halt the pastoral shift.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, doctrinal_conservatives, payer,
    powerful, generational, constrained, global).

% Sets the overall pastoral direction, balancing doctrinal fidelity with compassionate application. They benefit from maintaining institutional unity and relevance in a changing world, but face internal dissent and external scrutiny.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, institutional_hierarchy, agenda_setter,
    institutional, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the Church's pastoral response to marital breakdown, providing a framework for discernment and accommodation that aims to keep individuals within the sacramental life of the Church.
% TRANSFER_FUNCTION: Transfers pastoral flexibility and a sense of inclusion to individuals experiencing marital failure, while transferring a perceived loss of doctrinal certainty and institutional consistency to traditionalist factions.
% ABSENT_VOICES: Strict legalists who would argue for a rigid, unyielding application of canon law without pastoral discretion are marginalized in this reading, as are those who advocate for a complete secularization of marriage within the Church.
% DISAPPEARANCE_RATIONALE: If this pastoral reading vanished, the Church's approach to marital breakdown would revert to a more rigid, less accommodating stance, leading to increased alienation for many laity and a crisis of pastoral care. The institutional response to human failure would reorganize around strict legalism.
% FOUNDING_PROBLEM: The Church faced a growing disconnect between its strict doctrine of marriage indissolubility and the lived realities of many faithful experiencing marital breakdown, leading to alienation and exclusion from sacramental life.
% FOUNDING_PROBLEM_CORROBORATION: Pastoral surveys, anecdotal evidence from clergy, and theological discussions within the Church consistently attest to the ongoing challenge of reconciling doctrine with lived experience. This is corroborated by sociological studies on religious practice and marital trends.
narrative_ontology:disappearance_verdict(marriage_sacrament__civic_pastoral_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_sacrament__civic_pastoral_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_sacrament__civic_pastoral_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
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
 *   Extractiveness is moderate (0.45) as traditionalists experience a loss of doctrinal certainty and normative clarity, which is a significant cost to their identity and worldview. Suppression is low (0.30) because while traditionalist voices are often marginalized, they are not actively silenced or prevented from expressing dissent. Theater ratio is moderate (0.20) as the language of 'indissolubility as ideal' can sometimes mask a de facto shift in practice. Accessibility collapse is moderate (0.35) as alternatives for traditionalists (e.g., finding a more doctrinally strict community) are constrained by the global nature of the Church, but not entirely collapsed. Resistance is moderate (0.25) from traditionalist groups.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of pastoral clergy and those seeking annulment, this reading functions as a 'rope' or 'scaffold', providing necessary support and coordination for complex human situations. However, from the perspective of traditional laity and doctrinal conservatives, it operates as a 'snare' or 'tangled_rope', extracting their sense of doctrinal stability and identity. The engine's computation will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Pastoral clergy and laity seeking annulment are beneficiaries, experiencing greater flexibility and inclusion. Traditional laity and doctrinal conservatives are payers, bearing the cost of doctrinal relativization and perceived inconsistency. The institutional hierarchy acts as an agenda-setter, navigating the tension between doctrine and pastoral needs.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    doctrinal_relativization_impact,
    'To what extent does the pastoral discernment approach genuinely relativize the doctrine of indissolubility, versus merely applying it with greater nuance?',
    'Longitudinal study of annulment rates, canonical jurisprudence, and theological discourse over several decades, comparing stated doctrine with actual practice and its perceived impact on the faithful.',
    'If it''s a genuine relativization, the extraction from traditionalists is higher, pushing the classification closer to a Snare. If it''s merely nuanced application, extraction is lower, supporting a Rope or Scaffold classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_relativization_impact, empirical, 'Ambiguity in whether pastoral discernment is a doctrinal shift or a nuanced application.').

omega_variable(
    institutional_unity_cost,
    'Is the perceived erosion of institutional authority and doctrinal clarity a necessary cost for maintaining the broader unity and pastoral relevance of the Church in a modern context?',
    'Sociological and theological analysis of Church membership trends, internal dissent, and external perceptions of the Church''s coherence, weighed against the benefits of pastoral accommodation.',
    'If the cost is deemed necessary for unity, the constraint''s coordination function is stronger, supporting a Tangled Rope. If the cost is disproportionate, it suggests a more extractive dynamic.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_unity_cost, preference, 'Trade-off between doctrinal clarity and institutional unity/relevance.').

omega_variable(
    identity_lock_mechanism,
    'For traditional laity, is their ''identity_locked'' exit option primarily due to deep theological conviction, or is it reinforced by social and communal ties within traditionalist Catholic circles?',
    'Qualitative sociological research (interviews, ethnography) exploring the motivations and social networks of traditionalist Catholics, distinguishing between internal theological commitment and external social reinforcement.',
    'If primarily theological, the identity lock is more robust and less amenable to external intervention. If socially reinforced, interventions targeting social structures might weaken the lock, altering their effective directionality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Mechanism of identity lock for traditional laity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_sacrament__civic_pastoral_reading, 1965, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1965, marriage_sacrament__civic_pastoral_reading, theater_ratio, 1965, 0.05).
narrative_ontology:measurement(marr_tr_t1980, marriage_sacrament__civic_pastoral_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(marr_tr_t1995, marriage_sacrament__civic_pastoral_reading, theater_ratio, 1995, 0.15).
narrative_ontology:measurement(marr_tr_t2010, marriage_sacrament__civic_pastoral_reading, theater_ratio, 2010, 0.18).
narrative_ontology:measurement(marr_tr_t2024, marriage_sacrament__civic_pastoral_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(marr_be_t1965, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 1965, 0.2).
narrative_ontology:measurement(marr_be_t1980, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 1980, 0.3).
narrative_ontology:measurement(marr_be_t1995, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 1995, 0.38).
narrative_ontology:measurement(marr_be_t2010, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 2010, 0.42).
narrative_ontology:measurement(marr_be_t2024, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1965, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 1965, 0.15).
narrative_ontology:measurement(marr_su_t1980, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 1980, 0.2).
narrative_ontology:measurement(marr_su_t1995, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 1995, 0.25).
narrative_ontology:measurement(marr_su_t2010, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 2010, 0.28).
narrative_ontology:measurement(marr_su_t2024, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 2024, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_sacrament__civic_pastoral_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_sacrament__civic_pastoral_reading, 0.08).
narrative_ontology:affects_constraint(marriage_sacrament__civic_pastoral_reading, marriage_sacrament__hierarchical_indissolubility_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'marriage_sacrament' kernel. Its pastoral emphasis influences, and is influenced by, the more rigid 'hierarchical_indissolubility_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
