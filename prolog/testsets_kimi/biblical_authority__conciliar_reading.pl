% ============================================================================
% CONSTRAINT STORY: biblical_authority__conciliar_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_authority__conciliar_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: biblical_authority__conciliar_reading
 *   human_readable: Conciliar-Patristic Scriptural Authority
 *   domain: theological/religious
 *
 * SUMMARY:
 *   This constraint instantiates the conciliar reading of the
 *   biblical_authority kernel: Scripture is interpreted through ecumenical
 *   councils and patristic consensus, with tradition understood as living
 *   continuity rather than magisterial decree. The kernel 'biblical
 *   authority' conflates three structurally distinct readingsâsola
 *   scriptura, tradition-magisterium, and conciliar-patristicâeach with
 *   different beneficiary structures and epsilon values. This reading
 *   coordinates the global Orthodox and parts of the Anglican communion,
 *   enforcing doctrinal boundaries through collective episcopal authority
 *   rather than papal monarchy.
 *
 * KEY AGENTS:
 *   - Conciliar episcopate (agenda_setter/beneficiary): Institutional power, global scope, constrained exit. Administers councils and patristic gatekeeping.
 *   - Autocephalous jurisdictions (beneficiary): Institutional power, national scope, constrained exit. Gain legitimacy from the conciliar framework without papal submission.
 *   - Doctrinal reformers (payer): Moderate power, national scope, constrained exit. Bear the cost of slowed adaptation and precedent-based exclusion.
 *   - Ecumenical theologians (observer): Analytical power, global scope, analytical exit. Document the gap between conciliar ideals and autocephalous practice.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_authority__conciliar_reading, 0.5).
domain_priors:suppression_score(biblical_authority__conciliar_reading, 0.48).
domain_priors:theater_ratio(biblical_authority__conciliar_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_authority__conciliar_reading, extractiveness, 0.5).
narrative_ontology:constraint_metric(biblical_authority__conciliar_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(biblical_authority__conciliar_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_authority__conciliar_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(biblical_authority__conciliar_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_authority__conciliar_reading, tangled_rope).
narrative_ontology:human_readable(biblical_authority__conciliar_reading, "Conciliar-Patristic Scriptural Authority").
narrative_ontology:topic_domain(biblical_authority__conciliar_reading, "theological/religious").

domain_priors:requires_active_enforcement(biblical_authority__conciliar_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_authority__conciliar_reading, '787643bf-1b56-4481-8168-68272db6280c').
narrative_ontology:cs_kernel_codification('787643bf-1b56-4481-8168-68272db6280c', fixed_text).
narrative_ontology:cs_authority_grounding('787643bf-1b56-4481-8168-68272db6280c', lineage).
narrative_ontology:cs_interpretation_layer_present('787643bf-1b56-4481-8168-68272db6280c').
narrative_ontology:cs_reading_relation('787643bf-1b56-4481-8168-68272db6280c', biblical_authority__sola_scriptura_reading, forecloses).
narrative_ontology:cs_reading_relation('787643bf-1b56-4481-8168-68272db6280c', biblical_authority__tradition_scripture_reading, coexists_with).
narrative_ontology:cs_axiom('787643bf-1b56-4481-8168-68272db6280c', foundational, ecumenical_council_scriptural_authority).
narrative_ontology:cs_axiom_status(ecumenical_council_scriptural_authority, holdable).
narrative_ontology:cs_axiom_grounding('787643bf-1b56-4481-8168-68272db6280c', ecumenical_council_scriptural_authority, theological).
narrative_ontology:cs_axiom('787643bf-1b56-4481-8168-68272db6280c', foundational, tradition_as_living_continuity_not_decree).
narrative_ontology:cs_axiom_status(tradition_as_living_continuity_not_decree, holdable).
narrative_ontology:cs_axiom_grounding('787643bf-1b56-4481-8168-68272db6280c', tradition_as_living_continuity_not_decree, theological).
narrative_ontology:cs_reference_frame('787643bf-1b56-4481-8168-68272db6280c', patristic_continuity_framework).
narrative_ontology:cs_drift_state('787643bf-1b56-4481-8168-68272db6280c', contemporary_autocephalous_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('787643bf-1b56-4481-8168-68272db6280c', '').
narrative_ontology:cs_kernel_id(biblical_authority__conciliar_reading, biblical_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_authority__conciliar_reading, conciliar_episcopate).
narrative_ontology:constraint_beneficiary(biblical_authority__conciliar_reading, autocephalous_jurisdictions).
narrative_ontology:constraint_victim(biblical_authority__conciliar_reading, doctrinal_reformers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Governs scriptural interpretation and doctrinal boundaries through participation in ecumenical councils and appeal to patristic consensus. Maintains liturgical and sacramental continuity across jurisdictions. Their authority and office depend on recognition within this framework; exiting means surrendering jurisdictional claims and the episcopal identity constituted by apostolic succession.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, conciliar_episcopate, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(biblical_authority__conciliar_reading, conciliar_episcopate, beneficiary).

% Self-headed national churches that derive legitimacy and stability from the conciliar-patristic framework while exercising local autonomy. They benefit from a global consensus that validates their independence without requiring submission to a centralized magisterium, though they absorb the costs of fragmentation and inter-jurisdictional rivalry.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, autocephalous_jurisdictions, beneficiary,
    institutional, generational, constrained, national).

% Theologians, clergy, and movements seeking rapid doctrinal or ethical adaptation to contemporary contexts. Their proposals are measured against conciliar and patristic precedent; innovative positions risk non-recognition, exclusion from teaching office, or institutional marginalization. They bear the cost of slowed adaptation and the gatekeeping required to maintain continuity.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, doctrinal_reformers, payer,
    moderate, biographical, constrained, national).

% Study the conciliar framework comparatively across Orthodox, Anglican, and broader ecumenical contexts. They document how authority flows through councils and tradition, assess the gap between conciliar ideals and autocephalous practice, and analyze the framework's persistence without magisterial centralization.
narrative_ontology:constraint_stakeholder(biblical_authority__conciliar_reading, ecumenical_theologians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(biblical_authority__conciliar_reading, conciliar_episcopate).
narrative_ontology:fixing_cost_class(biblical_authority__conciliar_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves doctrinal and sacramental continuity across generations and geographies by grounding scriptural interpretation in shared ecumenical councils and the consensus of the Church Fathers, avoiding both individualistic reading and centralized magisterial monopoly.
% TRANSFER_FUNCTION: Moves authority to define and bound legitimate interpretation from individual readers and local innovators to the collective episcopal body and the received patristic tradition.
% ABSENT_VOICES: Communities operating under sola scriptura, Roman Catholic magisterial theologians affirming papal supremacy over councils, and radical reformers seeking non-patristic doctrinal development are structurally absent from the consensus-building process.
% DISAPPEARANCE_RATIONALE: If the conciliar-patristic constraint vanished, autocephalous churches would lose their shared authority framework, doctrinal boundaries would fragment into local or individual interpretations, and the sacramental-liturgical continuity claimed as the Church's life would face radical renegotiation.
% FOUNDING_PROBLEM: How to maintain unity and orthodoxy in interpretation of Scripture across diverse churches without either a single supreme earthly authority or endless doctrinal fragmentation.
% FOUNDING_PROBLEM_CORROBORATION: Church historians and ecumenical theologians outside the immediate beneficiary hierarchy attest that the conciliar framework solved specific heresies and schisms in the first millennium; sociologists of religion note that the problem of fragmentation persists and has shifted to autocephalous nationalism, while the beneficiary churches assert the problem remains live through modern challenges.
narrative_ontology:disappearance_verdict(biblical_authority__conciliar_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_authority__conciliar_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_authority__conciliar_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-19',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(biblical_authority__conciliar_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_authority__conciliar_reading, 0.5, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_authority__conciliar_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(biblical_authority__conciliar_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(biblical_authority__conciliar_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.50) because the constraint genuinely coordinates sacramental and doctrinal continuity while simultaneously concentrating interpretive authority in the episcopal college. Suppression is moderate (0.48): enforcement operates through canonical exclusion and liturgical non-recognition rather than violent coercion. Theater_ratio rises from 0.25 to 0.40 over the interval, reflecting increasing performative maintenance of conciliar unity as autocephalous fragmentation deepens. Accessibility_collapse is substantial (0.62) because once inside the framework, alternatives such as sola scriptura or papal magisterium collapse in legitimacy. Resistance is moderate (0.42) from reformers and modernist movements.
 *
 * PERSPECTIVAL GAP:
 *   The conciliar episcopate experiences the constraint as a necessary coordination mechanism preserving apostolic faith across nations and centuries. Doctrinal reformers experience the same structure as an extractive gate that transfers authority to define orthodoxy away from contemporary discernment and toward a closed patristic archive. The engine computes this divergence from the structural data: identical power levels would yield symmetric directionality, but the beneficiary/victim declarations plus exit modulation produce asymmetric effective extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The conciliar episcopate and autocephalous jurisdictions are structural beneficiaries: the constraint subsidizes their authority and institutional identity (low d, extraction damped). Doctrinal reformers are structural targets: they bear the cost of continuity enforcement through marginalization and slowed adaptation (high d, extraction amplified). Ecumenical theologians occupy the analytical seat with neutral directionality. The asymmetry is not in raw power but in the mapping of exit options and the identity-fusion of episcopal office with the conciliar framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâmaintaining unity without papal supremacy or fragmentationâremains contested. The arrangement has not atrophied into a pure piton because the coordination function (sacramental continuity, shared liturgical life) remains operationally real and valued by participants. However, the rising theater_ratio indicates that a growing share of conciliar activity is performative maintenance of unity that autocephalous practice has already fragmented. It is not a snare because the victim set is narrowly those seeking rapid doctrinal adaptation, not the entire laity or all alternative interpreters. The classification as tangled_rope captures the inseparability of genuine coordination and episcopal extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conciliar_vs_magisterial_authority,
    'Does the conciliar reading''s rejection of magisterial decree foreclose the Catholic tradition-scripture reading, or can both coexist as legitimate traditionalisms?',
    'Comparative canonical analysis of authority claims in Orthodox and Catholic communions; ecumenical dialogue outcomes assessing whether papal supremacy and conciliarity are logically commensurable.',
    'If foreclosed, the conciliar reading is structurally exclusive in a way that increases its suppressive potential toward Catholic reunion efforts; if coexisting, the extraction is bounded by plural traditionalism and the framework competes rather than excludes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conciliar_vs_magisterial_authority, conceptual, 'Structural relationship between conciliar and magisterial readings').

omega_variable(
    clerical_extraction_naturalness,
    'Is the episcopal benefit from this constraint a necessary cost of maintaining doctrinal continuity, or is it a rent collected by gatekeeping access to legitimate interpretation?',
    'Historical analysis of doctrinal innovation rates and suppression mechanisms before and after major conciliar eras; comparative measurement of lay interpretive authority across communions with different clerical structures.',
    'If the coordination function is inseparable from episcopal authority, the constraint remains tangled_rope; if separable without loss of continuity, the constraint trends toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(clerical_extraction_naturalness, empirical, 'Whether episcopal extraction is coordination cost or rent').

omega_variable(
    kernel_reading_identity,
    'This constraint is the conciliar reading of the biblical_authority kernel. Would reclassifying it as a sola scriptura reading invert the victim and beneficiary sets, making the conciliar hierarchy the target of disintermediation?',
    'Comparative seat analysis across the sibling reading constraints; mapping directionality and exit options under the sola scriptura frame.',
    'A sola scriptura reading would structurally invert directionality: individual interpreters become beneficiaries and the conciliar hierarchy becomes the victim of authority displacement, confirming that kernel readings are not observer-relative shifts but distinct constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Reading identity and structural inversion across kernel siblings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_authority__conciliar_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bacc_tr_t0, biblical_authority__conciliar_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(bacc_tr_t5, biblical_authority__conciliar_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement(bacc_tr_t10, biblical_authority__conciliar_reading, theater_ratio, 10, 0.31).
narrative_ontology:measurement(bacc_tr_t15, biblical_authority__conciliar_reading, theater_ratio, 15, 0.34).
narrative_ontology:measurement(bacc_tr_t20, biblical_authority__conciliar_reading, theater_ratio, 20, 0.36).
narrative_ontology:measurement(bacc_tr_t25, biblical_authority__conciliar_reading, theater_ratio, 25, 0.38).
narrative_ontology:measurement(bacc_tr_t30, biblical_authority__conciliar_reading, theater_ratio, 30, 0.4).

% Extraction over time
narrative_ontology:measurement(bacc_be_t0, biblical_authority__conciliar_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(bacc_be_t5, biblical_authority__conciliar_reading, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(bacc_be_t10, biblical_authority__conciliar_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(bacc_be_t15, biblical_authority__conciliar_reading, base_extractiveness, 15, 0.45).
narrative_ontology:measurement(bacc_be_t20, biblical_authority__conciliar_reading, base_extractiveness, 20, 0.47).
narrative_ontology:measurement(bacc_be_t25, biblical_authority__conciliar_reading, base_extractiveness, 25, 0.49).
narrative_ontology:measurement(bacc_be_t30, biblical_authority__conciliar_reading, base_extractiveness, 30, 0.5).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(biblical_authority__conciliar_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_authority__conciliar_reading, identity_coordination).
narrative_ontology:affects_constraint(biblical_authority__conciliar_reading, biblical_authority__sola_scriptura_reading).
narrative_ontology:affects_constraint(biblical_authority__conciliar_reading, biblical_authority__tradition_scripture_reading).

% DUAL FORMULATION NOTE:
% This constraint is the conciliar reading of the biblical_authority kernel, decomposed from the colloquial label 'biblical authority' which conflates sola scriptura, magisterial tradition, and conciliar-patristic authority into a single ambiguous term. Each reading instantiates a structurally distinct constraint with different beneficiary/victim profiles and epsilon values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
