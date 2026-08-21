% ============================================================================
% CONSTRAINT STORY: second_amendment_text__originalist_civic_virtue_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_text__originalist_civic_virtue_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: second_amendment_text__originalist_civic_virtue_reading
 *   human_readable: Second Amendment: Originalist Civic Virtue Reading
 *   domain: constitutional_law/political_theory/firearms_policy
 *
 * SUMMARY:
 *   This constraint represents an originalist reading of the Second Amendment
 *   that understands the 'militia' as the universal armed citizenry and the
 *   'right to keep and bear arms' as protecting the capacity for
 *   citizen-soldier service, tied to civic republican virtue rather than
 *   solely individual self-defense or state-controlled security. It
 *   emphasizes the collective good of a well-regulated citizenry for the
 *   security of a free state.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_text__originalist_civic_virtue_reading, 0.25).
domain_priors:suppression_score(second_amendment_text__originalist_civic_virtue_reading, 0.3).
domain_priors:theater_ratio(second_amendment_text__originalist_civic_virtue_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_text__originalist_civic_virtue_reading, rope).
narrative_ontology:human_readable(second_amendment_text__originalist_civic_virtue_reading, "Second Amendment: Originalist Civic Virtue Reading").
narrative_ontology:topic_domain(second_amendment_text__originalist_civic_virtue_reading, "constitutional_law/political_theory/firearms_policy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_text__originalist_civic_virtue_reading, 'f3907f68-a450-4cb2-b1c5-bdb4ac7264d8').
narrative_ontology:cs_kernel_codification('f3907f68-a450-4cb2-b1c5-bdb4ac7264d8', fixed_text).
narrative_ontology:cs_authority_grounding('f3907f68-a450-4cb2-b1c5-bdb4ac7264d8', lineage).
narrative_ontology:cs_interpretation_layer_present('f3907f68-a450-4cb2-b1c5-bdb4ac7264d8').
narrative_ontology:cs_reading_relation('f3907f68-a450-4cb2-b1c5-bdb4ac7264d8', second_amendment_text__collective_security_reading, coexists_with).
narrative_ontology:cs_reading_relation('f3907f68-a450-4cb2-b1c5-bdb4ac7264d8', second_amendment_text__individual_right_reading, forecloses).
narrative_ontology:cs_axiom('f3907f68-a450-4cb2-b1c5-bdb4ac7264d8', foundational, arms_bearing_is_civic_duty).
narrative_ontology:cs_axiom_status(arms_bearing_is_civic_duty, holdable).
narrative_ontology:cs_axiom_grounding('f3907f68-a450-4cb2-b1c5-bdb4ac7264d8', arms_bearing_is_civic_duty, deontological).
narrative_ontology:cs_axiom('f3907f68-a450-4cb2-b1c5-bdb4ac7264d8', foundational, militia_is_universal_citizenry).
narrative_ontology:cs_axiom_status(militia_is_universal_citizenry, holdable).
narrative_ontology:cs_axiom_grounding('f3907f68-a450-4cb2-b1c5-bdb4ac7264d8', militia_is_universal_citizenry, conventional).
narrative_ontology:cs_reference_frame('f3907f68-a450-4cb2-b1c5-bdb4ac7264d8', founding_era_republicanism).
narrative_ontology:cs_drift_state('f3907f68-a450-4cb2-b1c5-bdb4ac7264d8', contemporary_legal_discourse, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('f3907f68-a450-4cb2-b1c5-bdb4ac7264d8', '').
narrative_ontology:cs_kernel_id(second_amendment_text__originalist_civic_virtue_reading, second_amendment_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_text__originalist_civic_virtue_reading, the_political_community).
narrative_ontology:constraint_beneficiary(second_amendment_text__originalist_civic_virtue_reading, well_regulated_militia).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(second_amendment_text__originalist_civic_virtue_reading, collective_security_advocates).
narrative_ontology:constraint_vindicates(second_amendment_text__originalist_civic_virtue_reading, civic_republicanism).
narrative_ontology:constraint_vindicates(second_amendment_text__originalist_civic_virtue_reading, citizen_soldier_ideal).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from a citizenry capable of collective defense, which this reading posits as the core purpose of the Second Amendment. It is the entity whose security and liberty are protected by the citizen-soldier capacity.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, the_political_community, beneficiary,
    institutional, generational, analytical, national).

% The organized body of the citizenry, whose existence and capacity are protected by this reading. It represents the collective defense mechanism envisioned by the founders.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, well_regulated_militia, beneficiary,
    organized, biographical, constrained, national).

% Proponents and interpreters of this reading, who advocate for its historical accuracy and contemporary relevance. They shape the understanding and application of this interpretation within legal and political discourse.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, originalist_scholars, agenda_setter,
    analytical, generational, analytical, universal).

% Advocates for an interpretation of the Second Amendment primarily focused on individual self-defense, independent of militia service. Their core premise is fundamentally at odds with the civic virtue reading's emphasis on collective capacity, leading to their exclusion from this reading's foundational logic.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, individual_rights_advocates, excluded,
    organized, biographical, constrained, national).

% While aligned with the collective aspect of the Second Amendment, this reading's emphasis on the universal armed citizenry differs from their focus on state control over arms for collective security. They benefit from the collective defense aspect but are excluded from the specific civic duty framing.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, collective_security_advocates, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_text__originalist_civic_virtue_reading, collective_security_advocates, excluded).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the citizenry's capacity for collective defense by framing the right to bear arms as a civic duty and communal resource, ensuring the security of a free state.
% TRANSFER_FUNCTION: Transfers the responsibility for collective defense to the armed citizenry, and transfers legitimacy from purely individual self-interest to civic virtue in the context of arms-bearing.
% ABSENT_VOICES: Individual rights advocates would object that this reading diminishes the personal self-defense aspect. Those who oppose mandatory militia service or the concept of a universal armed citizenry would object to the civic duty implications.
% DISAPPEARANCE_RATIONALE: If this understanding vanished, the legal and political discourse around the Second Amendment would lose a foundational historical interpretation, shifting the balance entirely towards individual rights or state control, fundamentally altering how the right is understood and applied in law and policy.
% FOUNDING_PROBLEM: To ensure the security of a free state by maintaining a well-regulated militia composed of the general citizenry, preventing both tyranny and foreign invasion, and preserving republican liberty.
% FOUNDING_PROBLEM_CORROBORATION: Historical texts (e.g., Federalist Papers, state ratification debates), early legal commentaries, and some contemporary historians and legal scholars corroborate this original intent. However, other scholars and legal interpretations contest its contemporary relevance or dominance, particularly in light of modern military structures and individual rights jurisprudence.
narrative_ontology:disappearance_verdict(second_amendment_text__originalist_civic_virtue_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_text__originalist_civic_virtue_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_text__originalist_civic_virtue_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(second_amendment_text__originalist_civic_virtue_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_text__originalist_civic_virtue_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_text__originalist_civic_virtue_reading_tests).
:- end_tests(second_amendment_text__originalist_civic_virtue_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low because this reading frames arms-bearing as a civic duty and a collective benefit, not a burden for private gain. Suppression is low because, while historically dominant, this reading now faces significant contestation from other interpretations, meaning alternatives are not actively suppressed. Resistance is high due to strong advocacy for competing interpretations. The claimed type is 'rope' because it describes a coordination mechanism for collective defense, with beneficiaries (the political community, the militia) and no specific victim set.
 *
 * PERSPECTIVAL GAP:
 *   Proponents of this reading view it as a foundational civic good, while individual rights advocates perceive it as an imposition of collective duty that diminishes individual liberty. The engine's classification will highlight how this 'rope' for the political community is experienced as an 'excluded' position by those holding alternative views.
 *
 * DIRECTIONALITY LOGIC:
 *   The political community and the well-regulated militia are direct beneficiaries, as their security and capacity are the core purpose of this reading. Originalist scholars act as agenda-setters, actively promoting and interpreting this view. Individual rights advocates are structurally excluded from this reading's core premise, as their focus on personal self-defense is not the primary function here.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_dominance_status,
    'Is this originalist civic virtue reading still a dominant interpretive framework in contemporary legal and political discourse, or has its influence been largely superseded by other readings?',
    'Analysis of Supreme Court jurisprudence, legislative debates, and public opinion polls regarding the Second Amendment''s primary purpose over time.',
    'If superseded, the constraint''s effective suppression of alternative readings would be lower, and its ''rope'' classification might shift towards ''piton'' if its functional relevance has atrophied despite scholarly maintenance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_dominance_status, empirical, 'Assesses the contemporary influence of the civic virtue reading.').

omega_variable(
    civic_duty_vs_individual_liberty_compatibility,
    'Is the civic duty aspect of this reading fundamentally compatible with modern understandings of individual liberty, or does it impose an undue burden on citizens?',
    'Conceptual analysis of constitutional theory and political philosophy regarding the balance between collective obligations and individual rights in a modern liberal democracy.',
    'If deemed incompatible, the ''rope'' classification might be challenged, as the ''benefit'' to the political community would come at a ''cost'' to individual liberty, potentially shifting it towards a ''tangled_rope'' or even ''snare'' from the individual''s perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civic_duty_vs_individual_liberty_compatibility, conceptual, 'Examines the philosophical tension between civic duty and individual liberty in this reading.').

omega_variable(
    militia_relevance_in_modern_era,
    'Is the ''well-regulated militia'' as understood in the founding era (universal armed citizenry) still functionally relevant for national security and preventing tyranny in the modern era, or has its practical basis atrophied?',
    'Empirical analysis of military doctrine, national defense strategies, and historical case studies of citizen militias in modern conflicts, alongside conceptual re-evaluation of ''tyranny'' in contemporary contexts.',
    'If deemed functionally irrelevant, the ''rope'' classification''s coordination function would be weakened, and the constraint might be reclassified as a ''piton'' if its persistence is primarily due to historical inertia rather than active function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(militia_relevance_in_modern_era, empirical, 'Assesses the contemporary functional relevance of the founding-era militia concept.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_text__originalist_civic_virtue_reading, 0, 229).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(seco_tr_t50, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 50, 0.07).
narrative_ontology:measurement(seco_tr_t100, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 100, 0.08).
narrative_ontology:measurement(seco_tr_t150, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 150, 0.09).
narrative_ontology:measurement(seco_tr_t200, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 200, 0.1).
narrative_ontology:measurement(seco_tr_t229, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 229, 0.1).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(seco_be_t50, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 50, 0.18).
narrative_ontology:measurement(seco_be_t100, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 100, 0.2).
narrative_ontology:measurement(seco_be_t150, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 150, 0.22).
narrative_ontology:measurement(seco_be_t200, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 200, 0.24).
narrative_ontology:measurement(seco_be_t229, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 229, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(seco_su_t50, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 50, 0.5).
narrative_ontology:measurement(seco_su_t100, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 100, 0.4).
narrative_ontology:measurement(seco_su_t150, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 150, 0.35).
narrative_ontology:measurement(seco_su_t200, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 200, 0.32).
narrative_ontology:measurement(seco_su_t229, second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 229, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_text__originalist_civic_virtue_reading, identity_coordination).
narrative_ontology:affects_constraint(second_amendment_text__originalist_civic_virtue_reading, second_amendment_text__collective_security_reading).
narrative_ontology:affects_constraint(second_amendment_text__originalist_civic_virtue_reading, second_amendment_text__individual_right_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the Second Amendment text, each with its own structural properties and classification. They form a constraint family where interpretations compete for dominance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
