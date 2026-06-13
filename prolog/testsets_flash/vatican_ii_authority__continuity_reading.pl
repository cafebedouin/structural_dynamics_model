% ============================================================================
% CONSTRAINT STORY: vatican_ii_authority__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_authority__continuity_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: vatican_ii_authority__continuity_reading
 *   human_readable: Vatican II as Organic Doctrinal Continuity
 *   domain: theology/ecclesiology/religious_authority
 *
 * SUMMARY:
 *   This constraint represents the 'continuity reading' of Vatican II, which
 *   asserts that the Council's reforms are legitimate expressions of an
 *   unchanging deposit of faith, representing organic doctrinal development.
 *   This reading is promulgated by the Magisterium to maintain doctrinal
 *   unity and the legitimacy of post-conciliar changes. It benefits
 *   progressive reformers by validating their efforts, while traditionalist
 *   critics bear the cost of marginalization for rejecting this
 *   interpretation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_authority__continuity_reading, 0.15).
domain_priors:suppression_score(vatican_ii_authority__continuity_reading, 0.25).
domain_priors:theater_ratio(vatican_ii_authority__continuity_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_authority__continuity_reading, rope).
narrative_ontology:human_readable(vatican_ii_authority__continuity_reading, "Vatican II as Organic Doctrinal Continuity").
narrative_ontology:topic_domain(vatican_ii_authority__continuity_reading, "theology/ecclesiology/religious_authority").

domain_priors:requires_active_enforcement(vatican_ii_authority__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_authority__continuity_reading, '8a4e14ac-6d83-4362-a394-806ad410488d').
narrative_ontology:cs_kernel_codification('8a4e14ac-6d83-4362-a394-806ad410488d', fixed_text).
narrative_ontology:cs_authority_grounding('8a4e14ac-6d83-4362-a394-806ad410488d', lineage).
narrative_ontology:cs_interpretation_layer_present('8a4e14ac-6d83-4362-a394-806ad410488d').
narrative_ontology:cs_reading_relation('8a4e14ac-6d83-4362-a394-806ad410488d', vatican_ii_authority__rupture_reading, coexists_with).
narrative_ontology:cs_reading_relation('8a4e14ac-6d83-4362-a394-806ad410488d', vatican_ii_authority__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('8a4e14ac-6d83-4362-a394-806ad410488d', foundational, doctrinal_development_is_organic).
narrative_ontology:cs_axiom_status(doctrinal_development_is_organic, holdable).
narrative_ontology:cs_axiom_grounding('8a4e14ac-6d83-4362-a394-806ad410488d', doctrinal_development_is_organic, deontological).
narrative_ontology:cs_axiom('8a4e14ac-6d83-4362-a394-806ad410488d', foundational, magisterial_authority_ensures_fidelity).
narrative_ontology:cs_axiom_status(magisterial_authority_ensures_fidelity, holdable).
narrative_ontology:cs_axiom_grounding('8a4e14ac-6d83-4362-a394-806ad410488d', magisterial_authority_ensures_fidelity, theological).
narrative_ontology:cs_reference_frame('8a4e14ac-6d83-4362-a394-806ad410488d', pre_vatican_ii_doctrinal_unity).
narrative_ontology:cs_drift_state('8a4e14ac-6d83-4362-a394-806ad410488d', contemporary_theological_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('8a4e14ac-6d83-4362-a394-806ad410488d', '').
narrative_ontology:cs_kernel_id(vatican_ii_authority__continuity_reading, vatican_ii_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_authority__continuity_reading, progressive_reformers).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__continuity_reading, magisterium).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__continuity_reading, lay_faithful).
narrative_ontology:constraint_victim(vatican_ii_authority__continuity_reading, traditionalist_critics).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The teaching authority of the Catholic Church, which promulgates and interprets the documents of Vatican II. It asserts the Council's continuity with tradition and guides its implementation, benefiting from the perceived legitimacy of both tradition and reform.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, magisterium, agenda_setter,
    institutional, civilizational, identity_locked, universal).

% Advocates for post-conciliar reforms, who find their theological and pastoral positions validated by the continuity reading. They benefit from the legitimacy this reading confers on their efforts to modernize the Church while remaining within its framework.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, progressive_reformers, beneficiary,
    organized, generational, constrained, global).

% Those who perceive a rupture with tradition in Vatican II and its reforms. They are forced to either accept the continuity reading (at cost to their theological convictions) or operate on the margins of the Church, bearing the cost of dissent and marginalization.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, traditionalist_critics, payer,
    moderate, generational, identity_locked, global).

% The general body of believers, who receive the teachings and reforms of Vatican II as presented by the Magisterium. They benefit from a unified, coherent narrative of Church development, but may experience confusion or alienation if the continuity narrative strains credulity.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, lay_faithful, beneficiary,
    powerless, biographical, constrained, local).

% Scholars who analyze the documents of Vatican II and their relationship to prior doctrine. They contribute to the interpretive discourse, either reinforcing or challenging the continuity reading, but are ultimately subject to the Magisterium's authoritative interpretation.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, theologians, observer,
    organized, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent framework for understanding the Second Vatican Council's teachings and subsequent reforms, integrating them into the existing doctrinal tradition and maintaining the unity of the Church's teaching authority.
% TRANSFER_FUNCTION: Transfers theological legitimacy and institutional support to post-conciliar reforms and their proponents, while requiring traditionalist dissenters to either conform or bear the costs of marginalization.
% ABSENT_VOICES: Those who left the Church due to perceived rupture or irreconcilable contradictions, or those who are silenced within it, would argue that the continuity reading is a forced interpretation that ignores genuine breaks with tradition.
% DISAPPEARANCE_RATIONALE: If the continuity reading vanished, the Catholic Church would face a profound crisis of authority and identity. The legitimacy of Vatican II and all subsequent reforms would be called into question, leading to widespread doctrinal confusion, schism, and a fundamental re-evaluation of the Magisterium's role.
% FOUNDING_PROBLEM: The need to reconcile the modernizing impulses of the Second Vatican Council with the unchanging nature of Catholic doctrine, ensuring that reforms were perceived as development rather than contradiction.
% FOUNDING_PROBLEM_CORROBORATION: The Magisterium consistently reiterates the continuity reading. While traditionalist critics dispute its validity, the official teaching body and a significant portion of the faithful continue to affirm it as the only legitimate interpretation, corroborated by ongoing theological efforts to demonstrate organic development.
narrative_ontology:disappearance_verdict(vatican_ii_authority__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_authority__continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_authority__continuity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(vatican_ii_authority__continuity_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_authority__continuity_reading_tests).
:- end_tests(vatican_ii_authority__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low (0.15) because the primary function is coordination and legitimation, not direct material extraction. Suppression (0.25) is moderate, reflecting the institutional pressure on dissenting views to conform to the official interpretation. Theater ratio (0.4) is also moderate, as significant intellectual and pastoral effort is genuinely invested in demonstrating continuity, though some of this effort serves to manage internal dissent rather than purely advance theological truth. The metrics reflect a constraint that primarily coordinates belief and institutional alignment.
 *
 * PERSPECTIVAL GAP:
 *   From the Magisterium's perspective, this is a necessary and true interpretation that ensures the Church's fidelity to its past while adapting to the present. From the traditionalist critics' perspective, it is a forced narrative that suppresses genuine rupture. The engine's per-seat classification will reflect this divergence based on their declared power, exit options, and roles.
 *
 * DIRECTIONALITY LOGIC:
 *   The Magisterium and progressive reformers are beneficiaries (d near 0.0-0.2) as this reading legitimizes their authority and agenda. Traditionalist critics are payers (d near 0.8-1.0) as they are forced to accept an interpretation they reject or face institutional marginalization. The lay faithful are diffuse beneficiaries, receiving a coherent narrative, while theologians act as observers, analyzing the claims.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    doctrinal_coherence_ambiguity,
    'Is the ''continuity reading'' a genuinely coherent theological interpretation, or does it require significant interpretive strain to reconcile perceived contradictions?',
    'Comprehensive, independent theological analysis of specific doctrinal points, assessing the degree of hermeneutical effort required to demonstrate continuity without reinterpreting prior magisterial statements beyond recognition.',
    'If significant strain is required, the ''theater_ratio'' might be higher, indicating more performative effort to maintain the narrative than genuine theological synthesis. This would shift the classification towards a ''tangled_rope'' for the Magisterium, as it actively enforces a strained interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_coherence_ambiguity, conceptual, 'Assessing the theological coherence of the continuity claim.').

omega_variable(
    legitimacy_of_dissent_ambiguity,
    'To what extent is traditionalist dissent genuinely suppressed by the ''continuity reading'', versus being a legitimate theological position that simply differs from the Magisterium''s authoritative interpretation?',
    'Analysis of institutional actions against traditionalist groups (e.g., excommunications, suppression of orders, restrictions on liturgy) versus purely theological debate. Also, surveys of traditionalist faithful regarding their perceived freedom to express dissent.',
    'If dissent is actively suppressed beyond normal theological disagreement, the ''suppression'' metric would be higher, and the ''continuity_reading'' would function more as a ''snare'' for traditionalists, enforcing conformity rather than coordinating belief.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_dissent_ambiguity, empirical, 'Distinguishing legitimate theological disagreement from active suppression of dissent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_authority__continuity_reading, 1965, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1965, vatican_ii_authority__continuity_reading, theater_ratio, 1965, 0.3).
narrative_ontology:measurement(vati_tr_t1980, vatican_ii_authority__continuity_reading, theater_ratio, 1980, 0.35).
narrative_ontology:measurement(vati_tr_t1995, vatican_ii_authority__continuity_reading, theater_ratio, 1995, 0.38).
narrative_ontology:measurement(vati_tr_t2010, vatican_ii_authority__continuity_reading, theater_ratio, 2010, 0.4).
narrative_ontology:measurement(vati_tr_t2024, vatican_ii_authority__continuity_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(vati_be_t1965, vatican_ii_authority__continuity_reading, base_extractiveness, 1965, 0.1).
narrative_ontology:measurement(vati_be_t1980, vatican_ii_authority__continuity_reading, base_extractiveness, 1980, 0.12).
narrative_ontology:measurement(vati_be_t1995, vatican_ii_authority__continuity_reading, base_extractiveness, 1995, 0.14).
narrative_ontology:measurement(vati_be_t2010, vatican_ii_authority__continuity_reading, base_extractiveness, 2010, 0.15).
narrative_ontology:measurement(vati_be_t2024, vatican_ii_authority__continuity_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1965, vatican_ii_authority__continuity_reading, suppression_requirement, 1965, 0.2).
narrative_ontology:measurement(vati_su_t1980, vatican_ii_authority__continuity_reading, suppression_requirement, 1980, 0.22).
narrative_ontology:measurement(vati_su_t1995, vatican_ii_authority__continuity_reading, suppression_requirement, 1995, 0.24).
narrative_ontology:measurement(vati_su_t2010, vatican_ii_authority__continuity_reading, suppression_requirement, 2010, 0.25).
narrative_ontology:measurement(vati_su_t2024, vatican_ii_authority__continuity_reading, suppression_requirement, 2024, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_authority__continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_authority__continuity_reading, vatican_ii_authority__rupture_reading).
narrative_ontology:affects_constraint(vatican_ii_authority__continuity_reading, vatican_ii_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'vatican_ii_authority' kernel, asserting continuity. It is linked to sibling readings that posit rupture or overdetermination, as these interpretations directly contest its claims and influence the broader theological landscape.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
