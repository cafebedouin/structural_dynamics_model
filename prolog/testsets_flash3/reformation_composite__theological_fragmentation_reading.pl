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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: reformation_composite__theological_fragmentation_reading
 *   human_readable: Reformation: Theological Fragmentation (Reading)
 *   domain: historical_epistemology/religious_history/political_economy
 *
 * SUMMARY:
 *   This constraint models the Reformation as primarily a theological event,
 *   where competing doctrines (soteriology, ecclesiology) led to the
 *   fragmentation of Western Christianity into distinct, often mutually
 *   exclusive, denominations. This reading emphasizes the role of
 *   confessional documents and denominational leadership in solidifying and
 *   perpetuating these divisions. The constraint is claimed as a Tangled Rope
 *   because it provided genuine coordination for adherents within each
 *   confession but simultaneously extracted costs from those who did not
 *   conform, requiring active enforcement of doctrinal boundaries. This is
 *   one reading of the 'reformation_composite' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_composite__theological_fragmentation_reading, 0.65).
domain_priors:suppression_score(reformation_composite__theological_fragmentation_reading, 0.7).
domain_priors:theater_ratio(reformation_composite__theological_fragmentation_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_composite__theological_fragmentation_reading, tangled_rope).
narrative_ontology:human_readable(reformation_composite__theological_fragmentation_reading, "Reformation: Theological Fragmentation (Reading)").
narrative_ontology:topic_domain(reformation_composite__theological_fragmentation_reading, "historical_epistemology/religious_history/political_economy").

domain_priors:requires_active_enforcement(reformation_composite__theological_fragmentation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_composite__theological_fragmentation_reading, '63364982-0ade-4a84-aa30-82f447b81089').
narrative_ontology:cs_kernel_codification('63364982-0ade-4a84-aa30-82f447b81089', formalized).
narrative_ontology:cs_authority_grounding('63364982-0ade-4a84-aa30-82f447b81089', lineage).
narrative_ontology:cs_interpretation_layer_present('63364982-0ade-4a84-aa30-82f447b81089').
narrative_ontology:cs_reading_relation('63364982-0ade-4a84-aa30-82f447b81089', reformation_composite__political_realignment_reading, coexists_with).
narrative_ontology:cs_reading_relation('63364982-0ade-4a84-aa30-82f447b81089', reformation_composite__technological_mediation_reading, coexists_with).
narrative_ontology:cs_axiom('63364982-0ade-4a84-aa30-82f447b81089', foundational, sola_scriptura_ultimate_authority).
narrative_ontology:cs_axiom_status(sola_scriptura_ultimate_authority, holdable).
narrative_ontology:cs_axiom_grounding('63364982-0ade-4a84-aa30-82f447b81089', sola_scriptura_ultimate_authority, theological).
narrative_ontology:cs_axiom('63364982-0ade-4a84-aa30-82f447b81089', foundational, justification_by_faith_alone_essential).
narrative_ontology:cs_axiom_status(justification_by_faith_alone_essential, holdable).
narrative_ontology:cs_axiom_grounding('63364982-0ade-4a84-aa30-82f447b81089', justification_by_faith_alone_essential, theological).
narrative_ontology:cs_reference_frame('63364982-0ade-4a84-aa30-82f447b81089', confessional_purity_and_distinctiveness).
narrative_ontology:cs_drift_state('63364982-0ade-4a84-aa30-82f447b81089', peace_of_westphalia_1648, gap(stable, minor, true)).
narrative_ontology:cs_created_at('63364982-0ade-4a84-aa30-82f447b81089', '').
narrative_ontology:cs_kernel_id(reformation_composite__theological_fragmentation_reading, reformation_composite).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_composite__theological_fragmentation_reading, denominational_leadership).
narrative_ontology:constraint_beneficiary(reformation_composite__theological_fragmentation_reading, confessional_theologians).
narrative_ontology:constraint_victim(reformation_composite__theological_fragmentation_reading, religious_minorities).
narrative_ontology:constraint_victim(reformation_composite__theological_fragmentation_reading, ecumenical_movements).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Establishes and enforces doctrinal boundaries, benefiting from the distinct identity and institutional structures of their specific confession. Their authority is grounded in maintaining theological purity and distinctiveness, making exit from fragmentation a threat to their power base.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, denominational_leadership, agenda_setter,
    institutional, generational, identity_locked, regional).

% Their careers and intellectual projects are built upon articulating, defending, and elaborating specific denominational doctrines. They benefit from the ongoing need for theological justification of distinct confessional identities.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, confessional_theologians, beneficiary,
    organized, biographical, constrained, continental).

% Bear the costs of doctrinal fragmentation through persecution, social exclusion, and limited political rights in regions dominated by a different confession. Their existence is often seen as a threat to confessional unity, leading to active suppression.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, religious_minorities, payer,
    powerless, immediate, trapped, local).

% Actively work to overcome doctrinal divisions and promote Christian unity. They face significant resistance from denominational structures and leadership whose power is tied to maintaining distinct theological identities. Their efforts are often undermined by the very fragmentation they seek to resolve.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, ecumenical_movements, payer,
    moderate, generational, constrained, global).

% Observe and sometimes intervene in religious conflicts, often seeking to manage the political and social consequences of theological fragmentation without necessarily engaging with the theological claims themselves. Their interest is in civil order, not doctrinal truth.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, secular_authorities, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, internally consistent theological framework and community identity for adherents, allowing for coordinated worship, moral guidance, and social organization within distinct confessional groups.
% TRANSFER_FUNCTION: Transfers authority, resources, and social capital to denominational leadership and theologians by legitimizing their roles as guardians and interpreters of specific doctrines, at the cost of unity and tolerance for religious minorities and ecumenical efforts.
% ABSENT_VOICES: Those who prioritize universal Christian unity over confessional distinctiveness, and those who suffered directly from religious wars and persecution, would argue for a less fragmented religious landscape. Their voices are often marginalized by the institutional structures that benefit from fragmentation.
% DISAPPEARANCE_RATIONALE: If the theological commitments driving fragmentation vanished, the institutional structures of distinct denominations would lose their primary justification. Denominational leadership would face an existential crisis, and ecumenical movements would gain immense momentum, leading to a significant reorganization of religious and political power.
% FOUNDING_PROBLEM: The perceived corruption and theological errors of the medieval Catholic Church, leading to a crisis of salvation and spiritual authority for many individuals.
% FOUNDING_PROBLEM_CORROBORATION: Denominational leaders and theologians attest that the theological problems addressed by their specific confessions remain live. Ecumenical movements and historical scholars, from outside the benefiting parties, corroborate the initial crisis but contest the necessity of permanent fragmentation as its solution, arguing the problem has evolved.
narrative_ontology:disappearance_verdict(reformation_composite__theological_fragmentation_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_composite__theological_fragmentation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_composite__theological_fragmentation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(reformation_composite__theological_fragmentation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_composite__theological_fragmentation_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high because the benefits of clear doctrinal identity for some came at the cost of severe persecution and social exclusion for others, particularly religious minorities. Suppression is also high, reflecting the active enforcement of confessional conformity by both religious and secular authorities during the period. The theater ratio is moderate, as while genuine theological debate occurred, some efforts to maintain doctrinal purity became performative defenses of institutional power. The peak of extractiveness and suppression aligns with the height of the Wars of Religion, with a slight decline by 1648 (Peace of Westphalia) as a new, albeit fragmented, order began to stabilize.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of denominational leadership, the theological fragmentation was a necessary and beneficial clarification of truth, providing spiritual coordination. From the perspective of religious minorities, it was a source of immense suffering and extraction. The engine's classification will reflect this divergence based on the declared structural relationships and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   Denominational leadership and confessional theologians are beneficiaries, as their authority and intellectual work are directly tied to the maintenance of distinct theological identities. Religious minorities and ecumenical movements are victims, bearing the costs of fragmentation and active suppression. Secular authorities are observers, sometimes benefiting from political control over religious institutions, but not directly from the theological fragmentation itself.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_vs_political_primacy,
    'To what extent was theological fragmentation a primary driver, versus a rationalization for pre-existing political and economic realignments?',
    'Comparative historical analysis of regions where political fragmentation preceded or followed theological shifts, and analysis of the material interests of confessional leaders versus their stated theological motivations.',
    'If political factors were primary, the extractiveness attributed to theological commitments might be lower, and the constraint might reclassify towards a Snare (political extraction using theological cover). If theological factors were primary, the Tangled Rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_vs_political_primacy, conceptual, 'Ambiguity regarding the causal primacy of theological versus political factors in driving Reformation fragmentation.').

omega_variable(
    technological_mediation_impact,
    'How much did the printing press amplify and solidify theological differences, making fragmentation more durable than it would have been otherwise?',
    'Counterfactual historical analysis comparing the spread and persistence of theological dissent in pre-printing press eras versus the Reformation period, controlling for other factors.',
    'If technological mediation was a strong amplifier, the ''suppression'' metric might be higher for this reading, as the constraint''s persistence was aided by the difficulty of suppressing widely disseminated ideas. It could also suggest a stronger ''influences'' relationship from the technological reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(technological_mediation_impact, empirical, 'Uncertainty about the precise impact of printing technology on the durability and scope of theological fragmentation.').

omega_variable(
    identity_lock_durability,
    'Is the ''identity_locked'' exit option for denominational leadership a permanent structural feature, or could it be overcome by sufficiently strong ecumenical or secular pressures?',
    'Longitudinal study of denominational mergers and schisms in response to external pressures, and analysis of internal theological shifts that prioritize unity over distinctiveness.',
    'If identity lock is less durable, the ''suppression'' metric for ecumenical movements might be lower, and the ''resistance'' metric higher, suggesting more viable pathways to overcoming fragmentation. This would shift the constraint towards a more ''Rope-like'' character over time.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_durability, empirical, 'Durability of identity-locked exit for denominational leadership.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_composite__theological_fragmentation_reading, 1517, 1648).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t1517, reformation_composite__theological_fragmentation_reading, theater_ratio, 1517, 0.1).
narrative_ontology:measurement(refo_tr_t1530, reformation_composite__theological_fragmentation_reading, theater_ratio, 1530, 0.15).
narrative_ontology:measurement(refo_tr_t1555, reformation_composite__theological_fragmentation_reading, theater_ratio, 1555, 0.2).
narrative_ontology:measurement(refo_tr_t1580, reformation_composite__theological_fragmentation_reading, theater_ratio, 1580, 0.25).
narrative_ontology:measurement(refo_tr_t1610, reformation_composite__theological_fragmentation_reading, theater_ratio, 1610, 0.3).
narrative_ontology:measurement(refo_tr_t1648, reformation_composite__theological_fragmentation_reading, theater_ratio, 1648, 0.2).

% Extraction over time
narrative_ontology:measurement(refo_be_t1517, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1517, 0.4).
narrative_ontology:measurement(refo_be_t1530, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1530, 0.5).
narrative_ontology:measurement(refo_be_t1555, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1555, 0.6).
narrative_ontology:measurement(refo_be_t1580, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1580, 0.68).
narrative_ontology:measurement(refo_be_t1610, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1610, 0.72).
narrative_ontology:measurement(refo_be_t1648, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1648, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t1517, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1517, 0.5).
narrative_ontology:measurement(refo_su_t1530, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1530, 0.6).
narrative_ontology:measurement(refo_su_t1555, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1555, 0.75).
narrative_ontology:measurement(refo_su_t1580, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1580, 0.85).
narrative_ontology:measurement(refo_su_t1610, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1610, 0.9).
narrative_ontology:measurement(refo_su_t1648, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1648, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_composite__theological_fragmentation_reading, identity_coordination).
narrative_ontology:affects_constraint(reformation_composite__theological_fragmentation_reading, reformation_composite__political_realignment_reading).
narrative_ontology:affects_constraint(reformation_composite__theological_fragmentation_reading, reformation_composite__technological_mediation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'reformation_composite' kernel. This reading emphasizes theological fragmentation, while others focus on political realignment and technological mediation. All three are structurally linked as different facets of the same historical event.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
