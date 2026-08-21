% ============================================================================
% CONSTRAINT STORY: vatican_ii_doctrinal_authority__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_doctrinal_authority__composite_overdetermination_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: vatican_ii_doctrinal_authority__composite_overdetermination_reading
 *   human_readable: Vatican II Doctrinal Authority: Composite Overdetermination Reading
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   This constraint story analyzes Vatican II not as a single, monolithic
 *   event, but as the convergence of multiple distinct structural changes
 *   (liturgical, ecumenical, ecclesiological, political) that were
 *   subsequently packaged and presented as a unified reform. The constraint
 *   itself is this 'packaging' and the 'composite overdetermination' of the
 *   Council's outcomes, which rejects a single ε measurement for the Council
 *   as a whole. Instead, each component (liturgy, religious freedom,
 *   ecumenism) has independent extractiveness, and the continuity/rupture
 *   debate is viewed as a category error, as different components exhibit
 *   different degrees of change. Ambiguities are seen as a structural feature
 *   of this packaging, not a bug.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 0.68).
domain_priors:suppression_score(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 0.75).
domain_priors:theater_ratio(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__composite_overdetermination_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__composite_overdetermination_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__composite_overdetermination_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_doctrinal_authority__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_doctrinal_authority__composite_overdetermination_reading, "Vatican II Doctrinal Authority: Composite Overdetermination Reading").
narrative_ontology:topic_domain(vatican_ii_doctrinal_authority__composite_overdetermination_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_doctrinal_authority__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_doctrinal_authority__composite_overdetermination_reading, '0536198e-b412-4353-a3c4-280203843a3e').
narrative_ontology:cs_kernel_codification('0536198e-b412-4353-a3c4-280203843a3e', fixed_text).
narrative_ontology:cs_authority_grounding('0536198e-b412-4353-a3c4-280203843a3e', lineage).
narrative_ontology:cs_interpretation_layer_present('0536198e-b412-4353-a3c4-280203843a3e').
narrative_ontology:cs_reading_relation('0536198e-b412-4353-a3c4-280203843a3e', vatican_ii_doctrinal_authority__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('0536198e-b412-4353-a3c4-280203843a3e', vatican_ii_doctrinal_authority__rupture_progressive_reading, coexists_with).
narrative_ontology:cs_reading_relation('0536198e-b412-4353-a3c4-280203843a3e', vatican_ii_doctrinal_authority__rupture_traditionalist_reading, coexists_with).
narrative_ontology:cs_axiom('0536198e-b412-4353-a3c4-280203843a3e', foundational, doctrinal_change_is_multivariate).
narrative_ontology:cs_axiom_status(doctrinal_change_is_multivariate, holdable).
narrative_ontology:cs_axiom_grounding('0536198e-b412-4353-a3c4-280203843a3e', doctrinal_change_is_multivariate, empirically_contingent).
narrative_ontology:cs_axiom('0536198e-b412-4353-a3c4-280203843a3e', foundational, conciliar_unity_is_constructed).
narrative_ontology:cs_axiom_status(conciliar_unity_is_constructed, holdable).
narrative_ontology:cs_axiom_grounding('0536198e-b412-4353-a3c4-280203843a3e', conciliar_unity_is_constructed, conventional).
narrative_ontology:cs_reference_frame('0536198e-b412-4353-a3c4-280203843a3e', doctrinal_unity_through_managed_pluralism).
narrative_ontology:cs_drift_state('0536198e-b412-4353-a3c4-280203843a3e', post_conciliar_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0536198e-b412-4353-a3c4-280203843a3e', '').
narrative_ontology:cs_kernel_id(vatican_ii_doctrinal_authority__composite_overdetermination_reading, vatican_ii_doctrinal_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__composite_overdetermination_reading, magisterium).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__composite_overdetermination_reading, theological_experts_aligned).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__composite_overdetermination_reading, traditionalist_factions).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__composite_overdetermination_reading, theological_experts_dissenting).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__composite_overdetermination_reading, laity_confused).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__composite_overdetermination_reading, progressive_factions).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__composite_overdetermination_reading, ecumenical_partners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The teaching authority of the Church, responsible for interpreting and enforcing the Council's decrees. Benefits from maintaining a unified, authoritative narrative of Vatican II, even if it means managing inherent ambiguities and disparate changes. Bears the cost of internal dissent and external criticism.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, magisterium, agenda_setter,
    institutional, generational, constrained, global).

% Theologians and scholars whose interpretations align with the official 'unified reform' narrative. They benefit from institutional support, publication opportunities, and influence within the Church's intellectual life. Their careers are often tied to the accepted hermeneutic.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, theological_experts_aligned, beneficiary,
    organized, biographical, constrained, global).

% Groups who perceive Vatican II as a rupture with tradition and resist its implementation. They bear the cost of being marginalized, disciplined, or excommunicated for rejecting the 'unified reform' narrative. Their identity is often deeply tied to pre-conciliar forms.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, traditionalist_factions, payer,
    powerless, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_doctrinal_authority__composite_overdetermination_reading, traditionalist_factions, excluded).

% Theologians who challenge the official 'unified reform' narrative, either by emphasizing specific ruptures or by highlighting the unresolved tensions within the Council's documents. They face professional marginalization, censorship, or loss of institutional standing.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, theological_experts_dissenting, payer,
    moderate, biographical, constrained, global).

% Ordinary faithful who experience the practical effects of the Council's changes (e.g., liturgical shifts) but struggle to reconcile the official narrative of unity with perceived inconsistencies or ongoing debates. They bear the cost of confusion, alienation, or loss of a clear sense of identity.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, laity_confused, payer,
    powerless, immediate, constrained, local).

% Groups who embrace the changes of Vatican II and advocate for further reforms. They benefit from the Council's opening to the modern world but may also feel constrained by the official narrative's limits on further development, leading to internal tensions.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, progressive_factions, beneficiary,
    organized, biographical, mobile, global).

% Other Christian denominations and religious traditions who engage in dialogue with the Catholic Church post-Vatican II. They benefit from the Council's ecumenical opening but are also affected by the internal Catholic debates over its interpretation and implementation.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__composite_overdetermination_reading, ecumenical_partners, beneficiary,
    organized, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_doctrinal_authority__composite_overdetermination_reading, magisterium).
narrative_ontology:fixing_cost_class(vatican_ii_doctrinal_authority__composite_overdetermination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To package multiple, distinct structural changes (liturgical, ecumenical, ecclesiological, political) into a unified reform, providing a coherent, authoritative framework for the Catholic Church's engagement with the modern world.
% TRANSFER_FUNCTION: Transfers the burden of reconciling disparate changes and inherent ambiguities from the Magisterium to the intellectual and spiritual life of the Church, while consolidating the Magisterium's authority over the interpretation of the Council.
% ABSENT_VOICES: Those who reject the very premise of a 'unified reform' as a category error, insisting on analyzing each component change independently, are often marginalized or dismissed as failing to grasp the 'spirit' or 'letter' of the Council. Their analytical framework is excluded from the dominant discourse.
% DISAPPEARANCE_RATIONALE: If the constraint of packaging Vatican II as a 'unified reform' vanished, the various components (liturgical, ecumenical, ecclesiological, political changes) would likely be re-evaluated on their own merits and trajectories, leading to a more fragmented but potentially more honest assessment of their impact and ongoing relevance. The Church's internal and external relations would reorganize around these distinct elements.
% FOUNDING_PROBLEM: The Catholic Church faced a crisis of relevance and engagement with the modern world, requiring a comprehensive aggiornamento (updating) across multiple domains, while maintaining doctrinal continuity.
% FOUNDING_PROBLEM_CORROBORATION: The Magisterium and aligned theologians assert the problem is live, citing ongoing challenges to faith and the need for a unified response. Dissenting theologians and historians, however, argue that the 'unified reform' narrative has obscured more than it has clarified, and that the original problems have evolved into new, distinct challenges, supported by historical analysis of post-conciliar developments from outside the benefiting parties.
narrative_ontology:disappearance_verdict(vatican_ii_doctrinal_authority__composite_overdetermination_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_doctrinal_authority__composite_overdetermination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_doctrinal_authority__composite_overdetermination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_doctrinal_authority__composite_overdetermination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__composite_overdetermination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_doctrinal_authority__composite_overdetermination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.68) stems from the cost of forcing disparate changes into a unified narrative, which often involves downplaying tensions or suppressing alternative interpretations of the Council's *process* and *outcome*. Suppression (0.75) is high due to the active enforcement required to maintain this unified narrative against internal dissent and external critiques. The theater ratio (0.45) reflects the performative aspect of continually asserting unity and continuity, even as the underlying components exhibit divergent trajectories. The slight dip in extractiveness and suppression towards the end of the interval reflects a period of more open discussion under recent pontificates, though the core constraint of 'unified packaging' remains.
 *
 * PERSPECTIVAL GAP:
 *   From the Magisterium's perspective, the 'unified reform' is a necessary act of coordination to maintain the Church's coherence. From the perspective of traditionalists or dissenting theologians, it is an extractive act that suppresses legitimate concerns or alternative historical analyses. The engine's computation of per-seat classifications will highlight this divergence, showing how the same structural constraint is experienced as coordination by some and extraction by others.
 *
 * DIRECTIONALITY LOGIC:
 *   The Magisterium and aligned theological experts are beneficiaries, as the 'unified reform' narrative reinforces their authority and intellectual framework. Traditionalist factions, dissenting theologians, and confused laity are victims, bearing the cost of marginalization, intellectual suppression, or spiritual disorientation. Progressive factions and ecumenical partners are beneficiaries of the Council's openings, but also experience constraints from the limits of the 'unified reform' packaging.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unified_reform_vs_disparate_changes,
    'To what extent is the ''unified reform'' narrative of Vatican II a genuine synthesis, versus a post-hoc packaging of disparate, independently evolving structural changes?',
    'Comprehensive historical-theological analysis of each conciliar document''s drafting, reception, and subsequent implementation, tracing the independent trajectories of liturgical, ecumenical, and ecclesiological reforms.',
    'If primarily a post-hoc packaging, the constraint''s extractiveness and theater_ratio would be higher, reflecting the cost of maintaining an artificial unity. If a genuine synthesis, the coordination function would be stronger, reducing perceived extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unified_reform_vs_disparate_changes, empirical, 'Assessing the coherence of Vatican II''s ''unified reform'' narrative.').

omega_variable(
    continuity_rupture_category_error,
    'Is the debate over ''continuity'' versus ''rupture'' in Vatican II a valid theological/historical question, or a category error that obscures the multivariate nature of doctrinal and institutional change?',
    'Meta-analysis of hermeneutical approaches to Vatican II, evaluating whether frameworks beyond the continuity/rupture binary offer more explanatory power for the Council''s diverse outcomes.',
    'If a category error, the constraint''s suppression of alternative analytical frameworks would be more evident, and the ''unified reform'' narrative would be seen as a tool for managing intellectual dissent rather than resolving it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(continuity_rupture_category_error, conceptual, 'Reframing the core debate around Vatican II''s nature.').

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately identified as the ''composite_overdetermination_reading'' of the ''vatican_ii_doctrinal_authority'' kernel?',
    'Comparison with other generated readings of the same kernel, ensuring distinct ε values, stakeholder structures, and axiomatic foundations, as per the ε-invariance principle.',
    'If not distinct, this reading would be merged with a sibling, indicating a failure to decompose the kernel effectively. If distinct, it validates the decomposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Verifies the unique identity of this kernel reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_doctrinal_authority__composite_overdetermination_reading, 1962, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1962, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 1962, 0.2).
narrative_ontology:measurement(vati_tr_t1975, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 1975, 0.3).
narrative_ontology:measurement(vati_tr_t1988, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 1988, 0.4).
narrative_ontology:measurement(vati_tr_t2001, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 2001, 0.48).
narrative_ontology:measurement(vati_tr_t2012, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 2012, 0.5).
narrative_ontology:measurement(vati_tr_t2024, vatican_ii_doctrinal_authority__composite_overdetermination_reading, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(vati_be_t1962, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 1962, 0.55).
narrative_ontology:measurement(vati_be_t1975, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 1975, 0.6).
narrative_ontology:measurement(vati_be_t1988, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 1988, 0.65).
narrative_ontology:measurement(vati_be_t2001, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 2001, 0.67).
narrative_ontology:measurement(vati_be_t2012, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 2012, 0.69).
narrative_ontology:measurement(vati_be_t2024, vatican_ii_doctrinal_authority__composite_overdetermination_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1962, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 1962, 0.6).
narrative_ontology:measurement(vati_su_t1975, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 1975, 0.68).
narrative_ontology:measurement(vati_su_t1988, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 1988, 0.72).
narrative_ontology:measurement(vati_su_t2001, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 2001, 0.76).
narrative_ontology:measurement(vati_su_t2012, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 2012, 0.78).
narrative_ontology:measurement(vati_su_t2024, vatican_ii_doctrinal_authority__composite_overdetermination_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_doctrinal_authority__composite_overdetermination_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__composite_overdetermination_reading, vatican_ii_doctrinal_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__composite_overdetermination_reading, vatican_ii_doctrinal_authority__rupture_progressive_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__composite_overdetermination_reading, vatican_ii_doctrinal_authority__rupture_traditionalist_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__composite_overdetermination_reading, liturgical_reform_implementation).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__composite_overdetermination_reading, ecumenical_dialogue_framework).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the 'vatican_ii_doctrinal_authority' kernel. This 'composite overdetermination' reading focuses on the packaging of distinct changes as a unified reform, differing from readings that emphasize continuity or rupture.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
