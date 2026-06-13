% ============================================================================
% CONSTRAINT STORY: reformation_composite__theological_fragmentation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_composite__theological_fragmentation_reading, []).

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
 *   constraint_id: reformation_composite__theological_fragmentation_reading
 *   human_readable: Reformation: Theological Fragmentation
 *   domain: historical_epistemology/religious_history/political_economy
 *
 * SUMMARY:
 *   This constraint models the Reformation as a process of theological
 *   fragmentation, where competing doctrinal commitments (soteriological and
 *   ecclesiological) led to the formation of distinct and often mutually
 *   exclusive Christian denominations. The constraint is 'tangled_rope'
 *   because it genuinely coordinates communities around shared beliefs (a
 *   coordination function) but also extracts costs from those seeking unity
 *   and from political rulers dealing with religious conflict (asymmetric
 *   extraction). The persistence of these divisions requires active
 *   enforcement of doctrinal boundaries by denominational leadership.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_composite__theological_fragmentation_reading, 0.6).
domain_priors:suppression_score(reformation_composite__theological_fragmentation_reading, 0.7).
domain_priors:theater_ratio(reformation_composite__theological_fragmentation_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_composite__theological_fragmentation_reading, tangled_rope).
narrative_ontology:human_readable(reformation_composite__theological_fragmentation_reading, "Reformation: Theological Fragmentation").
narrative_ontology:topic_domain(reformation_composite__theological_fragmentation_reading, "historical_epistemology/religious_history/political_economy").

domain_priors:requires_active_enforcement(reformation_composite__theological_fragmentation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_composite__theological_fragmentation_reading, '87948b98-da48-418d-ade8-1e10944aca63').
narrative_ontology:cs_kernel_codification('87948b98-da48-418d-ade8-1e10944aca63', formalized).
narrative_ontology:cs_authority_grounding('87948b98-da48-418d-ade8-1e10944aca63', lineage).
narrative_ontology:cs_interpretation_layer_present('87948b98-da48-418d-ade8-1e10944aca63').
narrative_ontology:cs_reading_relation('87948b98-da48-418d-ade8-1e10944aca63', reformation_composite__political_realignment_reading, influences).
narrative_ontology:cs_reading_relation('87948b98-da48-418d-ade8-1e10944aca63', reformation_composite__technological_mediation_reading, influences).
narrative_ontology:cs_axiom('87948b98-da48-418d-ade8-1e10944aca63', foundational, sola_scriptura_primacy).
narrative_ontology:cs_axiom_status(sola_scriptura_primacy, holdable).
narrative_ontology:cs_axiom_grounding('87948b98-da48-418d-ade8-1e10944aca63', sola_scriptura_primacy, deontological).
narrative_ontology:cs_axiom('87948b98-da48-418d-ade8-1e10944aca63', foundational, justification_by_faith_alone).
narrative_ontology:cs_axiom_status(justification_by_faith_alone, holdable).
narrative_ontology:cs_axiom_grounding('87948b98-da48-418d-ade8-1e10944aca63', justification_by_faith_alone, theological).
narrative_ontology:cs_reference_frame('87948b98-da48-418d-ade8-1e10944aca63', doctrinal_purity_and_scriptural_authority).
narrative_ontology:cs_drift_state('87948b98-da48-418d-ade8-1e10944aca63', post_peace_of_westphalia, gap(stable, minor, true)).
narrative_ontology:cs_created_at('87948b98-da48-418d-ade8-1e10944aca63', '').
narrative_ontology:cs_kernel_id(reformation_composite__theological_fragmentation_reading, reformation_composite).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_composite__theological_fragmentation_reading, denominational_leadership).
narrative_ontology:constraint_beneficiary(reformation_composite__theological_fragmentation_reading, theologians).
narrative_ontology:constraint_victim(reformation_composite__theological_fragmentation_reading, laity_seeking_unity).
narrative_ontology:constraint_victim(reformation_composite__theological_fragmentation_reading, political_rulers_seeking_stability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Leaders of various Protestant denominations and the Catholic hierarchy, who define and enforce doctrinal boundaries. They benefit from the distinctiveness of their theological positions, which justifies their authority and institutional existence. Exit means abandoning their theological commitments and institutional power.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, denominational_leadership, agenda_setter,
    institutional, generational, identity_locked, regional).

% Scholars and thinkers who develop, debate, and refine the specific soteriological and ecclesiological doctrines. Their careers and intellectual authority are built upon the distinct theological frameworks that emerge from the fragmentation. Exiting means losing their intellectual niche and influence.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, theologians, beneficiary,
    organized, biographical, constrained, continental).

% Individuals who desire a unified Christian church but are forced to choose between competing, often mutually exclusive, theological systems. They bear the social and spiritual costs of division, including religious wars and communal strife. Exit means abandoning their faith or accepting a fragmented religious landscape.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, laity_seeking_unity, payer,
    powerless, biographical, constrained, local).

% Monarchs and princes who face internal religious conflicts and wars of religion due to theological fragmentation. They bear the costs of maintaining civil order and often must choose a state religion, further entrenching division. Exit means risking political instability or ceding religious authority.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, political_rulers_seeking_stability, payer,
    powerful, generational, constrained, national).

% The dominant religious institution before the Reformation, which claimed universal spiritual authority. Its voice for a unified Christendom was increasingly marginalized by the emerging denominational structures, which actively rejected its theological claims.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, catholic_church_pre_reformation, excluded,
    institutional, civilizational, trapped, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates distinct communities of faith around specific interpretations of scripture and church practice, providing clear doctrinal boundaries and a sense of belonging for adherents within each denomination.
% TRANSFER_FUNCTION: Transfers spiritual authority, intellectual capital, and institutional loyalty from a singular, universal church to multiple, competing denominational structures. It also transfers resources (tithes, endowments) to these new institutions and their leadership.
% ABSENT_VOICES: The ideal of a unified, universal Christendom, as articulated by pre-Reformation Catholic thinkers and some later ecumenists, is absent. These voices would argue that theological fragmentation undermines the spiritual mission of the church and leads to unnecessary conflict.
% DISAPPEARANCE_RATIONALE: If the theological fragmentation vanished, the entire landscape of Western Christianity would fundamentally reorganize. Denominations would dissolve, their leadership structures would collapse, and the intellectual frameworks that define them would lose their meaning. A new, unified (or at least harmonized) religious order would emerge, with profound social and political consequences.
% FOUNDING_PROBLEM: The perceived corruption and doctrinal inconsistencies within the late medieval Catholic Church, coupled with a desire for a more direct and biblically grounded faith, led to calls for reform.
% FOUNDING_PROBLEM_CORROBORATION: Theologians and historians across various traditions corroborate the existence of genuine theological disputes and a desire for reform. While the specific 'problems' are interpreted differently, the underlying commitment to doctrinal purity and scriptural authority remains a live concern for many religious communities, as evidenced by ongoing theological debates and denominational distinctions.
narrative_ontology:disappearance_verdict(reformation_composite__theological_fragmentation_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_composite__theological_fragmentation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_composite__theological_fragmentation_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(reformation_composite__theological_fragmentation_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_composite__theological_fragmentation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reformation_composite__theological_fragmentation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reformation_composite__theological_fragmentation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.6) as the costs of fragmentation (religious wars, social division, intellectual labor to maintain distinctions) are significant, benefiting denominational leaders and theologians who gain authority and purpose from these distinctions. Suppression (0.7) is high because maintaining distinct theological identities requires active suppression of alternative interpretations and ecumenical movements. Theater ratio (0.2) is low, as the theological debates and institutional structures are largely functional in defining and maintaining denominational identity, with less performative maintenance. The metrics show a gradual increase in extractiveness and suppression as the fragmentation solidified over the period.
 *
 * PERSPECTIVAL GAP:
 *   Denominational leadership and theologians experience this as a necessary and beneficial process of clarifying truth and establishing authentic faith communities. Laity seeking unity and political rulers seeking stability experience it as a costly and divisive force. The engine will compute these divergent classifications from the declared roles and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   Denominational leadership and theologians are beneficiaries (d near 0.0) as their authority and intellectual work are directly tied to the distinct theological frameworks. Laity seeking unity and political rulers are targets (d near 1.0) as they bear the direct costs of religious division and conflict. The Catholic Church pre-Reformation is 'excluded' as its universalist claims are actively rejected by the emerging fragmented structure.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the theological fragmentation as pure extraction by acknowledging its genuine coordination function (providing distinct faith identities). However, by classifying it as a 'tangled_rope', it highlights the significant, often unacknowledged, costs borne by those outside the direct beneficiaries of the fragmentation, preventing it from being seen as a 'rope' or 'mountain' of inevitable, benign coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_vs_political_primacy,
    'Is the theological fragmentation the primary driver of the Reformation''s outcomes, or is it a consequence of deeper political and economic realignments?',
    'Comparative historical analysis across regions with varying political structures but similar theological disputes; counterfactual history exploring outcomes if theological disputes were resolved differently but political conditions remained.',
    'If political factors are primary, this constraint''s extractiveness might be lower, as the ''theological'' costs are merely symptoms. If theological factors are primary, the current classification holds, emphasizing the intrinsic costs of doctrinal incompatibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_vs_political_primacy, conceptual, 'Ambiguity regarding the causal primacy of theological vs. political factors in the Reformation.').

omega_variable(
    identity_lock_strength,
    'To what extent is denominational identity a freely chosen commitment versus an identity-locked condition for individuals and leaders?',
    'Sociological studies of conversion and deconversion rates across denominations, and analysis of the social and economic penalties for switching or abandoning denominational affiliation.',
    'If identity-lock is stronger than currently assessed, the effective suppression and extractiveness for individuals (laity) would be higher, pushing their seat classification closer to a snare. If it''s weaker, their exit options are more mobile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_strength, empirical, 'The degree to which denominational identity constitutes an identity-locked exit option.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_composite__theological_fragmentation_reading, 1517, 1648).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t1517, reformation_composite__theological_fragmentation_reading, theater_ratio, 1517, 0.1).
narrative_ontology:measurement(refo_tr_t1540, reformation_composite__theological_fragmentation_reading, theater_ratio, 1540, 0.15).
narrative_ontology:measurement(refo_tr_t1570, reformation_composite__theological_fragmentation_reading, theater_ratio, 1570, 0.18).
narrative_ontology:measurement(refo_tr_t1600, reformation_composite__theological_fragmentation_reading, theater_ratio, 1600, 0.19).
narrative_ontology:measurement(refo_tr_t1648, reformation_composite__theological_fragmentation_reading, theater_ratio, 1648, 0.2).

% Extraction over time
narrative_ontology:measurement(refo_be_t1517, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1517, 0.4).
narrative_ontology:measurement(refo_be_t1540, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1540, 0.5).
narrative_ontology:measurement(refo_be_t1570, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1570, 0.55).
narrative_ontology:measurement(refo_be_t1600, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1600, 0.58).
narrative_ontology:measurement(refo_be_t1648, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1648, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t1517, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1517, 0.5).
narrative_ontology:measurement(refo_su_t1540, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1540, 0.6).
narrative_ontology:measurement(refo_su_t1570, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1570, 0.65).
narrative_ontology:measurement(refo_su_t1600, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1600, 0.68).
narrative_ontology:measurement(refo_su_t1648, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1648, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_composite__theological_fragmentation_reading, identity_coordination).
narrative_ontology:affects_constraint(reformation_composite__theological_fragmentation_reading, reformation_composite__political_realignment_reading).
narrative_ontology:affects_constraint(reformation_composite__theological_fragmentation_reading, reformation_composite__technological_mediation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'reformation_composite' kernel, focusing on theological drivers. It influences and is influenced by the political and technological readings, as these aspects are deeply intertwined in the historical event.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
