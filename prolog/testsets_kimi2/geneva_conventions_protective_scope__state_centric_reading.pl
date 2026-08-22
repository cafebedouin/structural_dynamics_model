% ============================================================================
% CONSTRAINT STORY: geneva_conventions_protective_scope__state_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_protective_scope__state_centric_reading, []).

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
 *   constraint_id: geneva_conventions_protective_scope__state_centric_reading
 *   human_readable: State-Centric Geneva Protective Scope (Article 4 Combatant Status)
 *   domain: legal/international_humanitarian_law
 *
 * SUMMARY:
 *   This constraint story captures the state_centric_reading of the kernel
 *   geneva_conventions_protective_scope: the interpretation that Geneva
 *   Convention III protections, including prisoner-of-war status and
 *   combatant immunity, apply exclusively to uniformed combatants operating
 *   under responsible command and meeting the four cumulative Article 4
 *   criteria. Under this reading, unprivileged belligerents â non-state
 *   actors who fail to meet these formal criteria â fall outside the
 *   treatyâs protective scope and may be targeted without combatant
 *   immunity and prosecuted for mere participation in hostilities. The
 *   reading is contested by a universal_rights_reading (which extends a
 *   protective floor to all persons via Common Article 3 and human rights
 *   law) and a hybrid_proportionality_reading (which scales protections by
 *   conflict type). It is advanced by state parties and conventional state
 *   militaries, particularly in asymmetric conflicts where it lowers legal
 *   constraints on counterinsurgency and counterterrorism operations.
 *
 * KEY AGENTS:
 *   - state_parties_geneva: Primary agenda-setter (institutional/global) â negotiates, interprets, and enforces Article 4 criteria through military manuals and diplomatic practice
 *   - conventional_state_militaries: Primary beneficiary (institutional/global) â receives combatant immunity and POW protections; benefits from legal clarity that permits targeting of unprivileged belligerents
 *   - unprivileged_belligerents: Primary target (powerless/global) â denied protections, exposed to targeting and prosecution, structurally unable to meet Article 4 criteria
 *   - international_judicial_bodies: Analytical observer (institutional/global) â interprets Geneva obligations, occasionally challenges state-centric readings from the margins
 *   - human_rights_advocates: Excluded voice (organized/global) â argues for universal protective scope but lacks a seat in treaty interpretation forums
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_protective_scope__state_centric_reading, 0.58).
domain_priors:suppression_score(geneva_conventions_protective_scope__state_centric_reading, 0.65).
domain_priors:theater_ratio(geneva_conventions_protective_scope__state_centric_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__state_centric_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_protective_scope__state_centric_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_protective_scope__state_centric_reading, "State-Centric Geneva Protective Scope (Article 4 Combatant Status)").
narrative_ontology:topic_domain(geneva_conventions_protective_scope__state_centric_reading, "legal/international_humanitarian_law").

domain_priors:requires_active_enforcement(geneva_conventions_protective_scope__state_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_protective_scope__state_centric_reading, '68f0b25d-2d9e-44c0-be93-d0851807c121').
narrative_ontology:cs_kernel_codification('68f0b25d-2d9e-44c0-be93-d0851807c121', fixed_text).
narrative_ontology:cs_authority_grounding('68f0b25d-2d9e-44c0-be93-d0851807c121', lineage).
narrative_ontology:cs_interpretation_layer_present('68f0b25d-2d9e-44c0-be93-d0851807c121').
narrative_ontology:cs_reading_relation('68f0b25d-2d9e-44c0-be93-d0851807c121', geneva_conventions_protective_scope__universal_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('68f0b25d-2d9e-44c0-be93-d0851807c121', geneva_conventions_protective_scope__hybrid_proportionality_reading, influences).
narrative_ontology:cs_axiom('68f0b25d-2d9e-44c0-be93-d0851807c121', foundational, article_four_exclusivity).
narrative_ontology:cs_axiom_status(article_four_exclusivity, holdable).
narrative_ontology:cs_axiom_grounding('68f0b25d-2d9e-44c0-be93-d0851807c121', article_four_exclusivity, conventional).
narrative_ontology:cs_axiom('68f0b25d-2d9e-44c0-be93-d0851807c121', foundational, state_reciprocity_as_legitimacy_base).
narrative_ontology:cs_axiom_status(state_reciprocity_as_legitimacy_base, holdable).
narrative_ontology:cs_axiom_grounding('68f0b25d-2d9e-44c0-be93-d0851807c121', state_reciprocity_as_legitimacy_base, conventional).
narrative_ontology:cs_reference_frame('68f0b25d-2d9e-44c0-be93-d0851807c121', classical_interstate_reciprocity).
narrative_ontology:cs_drift_state('68f0b25d-2d9e-44c0-be93-d0851807c121', asymmetric_conflict_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('68f0b25d-2d9e-44c0-be93-d0851807c121', '').
narrative_ontology:cs_kernel_id(geneva_conventions_protective_scope__state_centric_reading, geneva_conventions_protective_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__state_centric_reading, conventional_state_militaries).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__state_centric_reading, unprivileged_belligerents).
narrative_ontology:constraint_vindicates(geneva_conventions_protective_scope__state_centric_reading, state_sovereignty_in_ihl).
narrative_ontology:constraint_vindicates(geneva_conventions_protective_scope__state_centric_reading, reciprocity_principle_combatant_status).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Negotiated the 1949 Geneva Conventions and retain interpretive authority over Article 4 criteria through state practice, military manuals, and diplomatic statements. They enforce the distinction between privileged and unprivileged belligerents through domestic military law, targeting policies, and detention regimes.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, state_parties_geneva, agenda_setter,
    institutional, generational, constrained, global).

% Receive combatant immunity and POW protections under the state-centric reading. Their operations against non-state actors are legally shielded from war crimes liability for targeting unprivileged belligerents, provided they meet Article 4 criteria. Professional military identity is constituted by adherence to lawful combatant status.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, conventional_state_militaries, beneficiary,
    institutional, biographical, identity_locked, global).

% Non-state actors who fail to meet Article 4 criteria (responsible command, fixed distinctive sign, open carriage of arms, compliance with laws of war). They are denied combatant immunity, may be prosecuted for mere participation in hostilities, and are excluded from POW status upon capture. They cannot opt into the protected category without fundamentally altering their operational structure and usually their political character.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, unprivileged_belligerents, payer,
    powerless, immediate, trapped, global).

% Interpret Geneva obligations in prosecutions and advisory opinions. Some chambers have pushed for broader protective scope, but their authority is contingent on state consent and enforcement. They observe and occasionally challenge the state-centric reading from the margins.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, international_judicial_bodies, observer,
    institutional, generational, analytical, global).

% Argue that humanitarian protections should not depend on combatant status and that Common Article 3 plus human rights law creates a universal floor. They are structurally excluded from the treaty interpretation process dominated by state military and diplomatic actors, though they file amicus briefs and publish interpretive guidance that states often ignore.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__state_centric_reading, human_rights_advocates, excluded,
    organized, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(geneva_conventions_protective_scope__state_centric_reading, conventional_state_militaries).
narrative_ontology:fixing_cost_class(geneva_conventions_protective_scope__state_centric_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of reciprocal protection for lawful combatants in interstate armed conflict by creating a clear, enforceable status that shields uniformed soldiers from criminal prosecution for lawful acts of war and guarantees humane treatment upon capture, contingent on meeting formal criteria.
% TRANSFER_FUNCTION: Moves legal immunity, detention protections, and operational freedom from the unprotected category (unprivileged belligerents) to the protected category (state militaries meeting Article 4), while transferring the burden of exposure to targeting and criminal liability onto non-state actors.
% ABSENT_VOICES: Unprivileged belligerents themselves are rarely heard in treaty interpretation forums; human rights advocates arguing for universal protective scope are systematically outnumbered by state delegations; affected civilian populations in asymmetric conflict zones have no seat in the legal classification process.
% DISAPPEARANCE_RATIONALE: If the state-centric reading disappeared overnight, state militaries would lose combatant immunity for their personnel in asymmetric conflicts, detention regimes would have to be restructured around criminal or human rights law rather than POW status, and the legal basis for targeting non-state actors without trial would collapse â the architecture of modern asymmetric warfare would rearrange.
% FOUNDING_PROBLEM: The lack of reciprocal protection for lawful combatants in interstate wars, leading to summary execution of captured soldiers and unlimited violence; the 1949 Conventions sought to civilize warfare by granting protections to those who distinguish themselves from civilians and follow the rules.
% FOUNDING_PROBLEM_CORROBORATION: Academic historians of IHL attest the post-1949 founding context was interstate warfare. The International Committee of the Red Cross has acknowledged that the combatant/civilian dichotomy is under strain in modern asymmetric conflicts, corroborating that the original problem context has shifted. Human rights monitors outside the state beneficiary set attest that the current application to asymmetric conflicts exceeds the founding problem.
narrative_ontology:disappearance_verdict(geneva_conventions_protective_scope__state_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_protective_scope__state_centric_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_protective_scope__state_centric_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(geneva_conventions_protective_scope__state_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_protective_scope__state_centric_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_protective_scope__state_centric_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_protective_scope__state_centric_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_protective_scope__state_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects that the state-centric reading genuinely coordinated reciprocal protection in interstate wars but now functions as asymmetric extraction in asymmetric conflicts, where non-state actors cannot meet the formal criteria. Suppression (0.65) captures active state resistance to broader interpretations through military manuals, diplomatic lobbying, and refusal to ratify protocols that would expand protected status. Theater ratio (0.45) indicates significant performative maintenance: states invoke formalistic Article 4 criteria to demonstrate compliance with IHL while systematically excluding irregular adversaries. Accessibility collapse (0.75) is high because once the state-centric frame is accepted, alternative universal or hybrid readings appear legally invalid in state courts and military legal education. Resistance (0.55) reflects sustained pushback from human rights advocates, some international judicial opinions, and academic critics. The claim/metric independence is maintained: the reading is claimed as tangled_rope because a genuine coordination function persists for interstate conflicts, while the metrics describe its substantially extractive application in asymmetric warfare.
 *
 * PERSPECTIVAL GAP:
 *   Conventional state militaries experience the constraint as legal clarity and protective order that enables lawful warfare; unprivileged belligerents experience it as a legalized elimination of their protective status, exposing them to targeting and prosecution without the safeguards afforded to state soldiers. The divergence is structural: the same legal rule that immunizes one party criminalizes the otherâs mere participation.
 *
 * DIRECTIONALITY LOGIC:
 *   State parties and conventional state militaries sit near the beneficiary end (low d): the constraint subsidizes their operational freedom, legal security, and professional identity. Unprivileged belligerents sit near the full-target end (high d): the constraint directly extracts their protective status and exposes them to violence and detention. International judicial bodies, as analytical observers with analytical exit, sit near symmetric. Human rights advocates, though excluded, retain mobile exit options and sit at moderate d. No overrides are needed because the structural derivation accurately captures these relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â reciprocal protection in interstate warfare â has shifted dramatically: the majority of contemporary armed conflicts are asymmetric or non-international, where the reciprocity logic is structurally absent. This creates mandatrophy risk, as the coordination rationale (reciprocal compliance) is weakened when one side cannot reciprocate in kind. However, the constraint is not a piton because the coordination function remains live in interstate conflicts and states actively defend and enforce the reading. The moderate theater ratio (0.45) signals some performative maintenance but not dominance; the constraint still performs real legal work, even as its extraction component has grown.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_scope_contest,
    'Does the state-centric reading reflect the only legally defensible interpretation of the Geneva text, or is it one contested reading among several that privileges state military interests?',
    'Systematic review of travaux prÃ©paratoires, subsequent state practice, and international jurisprudence to determine whether Article 4 was intended as an exclusive gateway or a default category.',
    'If exclusive and textually mandated, the constraint''s coordination function dominates and it remains a tangled rope; if one contested reading among many, the extraction component is larger and the constraint approaches a snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_scope_contest, conceptual, 'Whether the state-centric reading is the sole valid interpretation or a contested partisan reading.').

omega_variable(
    asymmetric_conflict_drift,
    'Has the dominance of asymmetric armed conflict transformed the state-centric reading from a reciprocal coordination device into an extractive tool for state counterinsurgency?',
    'Quantitative analysis of conflict-type distribution over time correlated with legal classification of detainees and targeting authorizations.',
    'If drift is confirmed, the temporal measurements support a rising extraction trajectory and potential reclassification toward snare or piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(asymmetric_conflict_drift, empirical, 'Whether conflict-type drift has increased the extractiveness of the state-centric reading.').

omega_variable(
    reciprocity_legitimacy_ambiguity,
    'Does the legitimacy of the state-centric reading depend on a reciprocity that is structurally absent in asymmetric conflicts, making its application to non-international conflicts a category error?',
    'Comparative doctrinal analysis of state military manuals and judicial opinions in interstate versus asymmetric conflicts to assess whether reciprocity is invoked as a justification.',
    'If reciprocity is the claimed legitimacy base but is absent in practice, the reading functions as a false summit or snare in asymmetric contexts; if an alternative legitimacy base is offered, the coordination function may be partially preserved.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_legitimacy_ambiguity, conceptual, 'Whether the reading''s legitimacy depends on a reciprocity that asymmetric conflicts cannot sustain.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_protective_scope__state_centric_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t0, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(gene_tr_t15, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 15, 0.2).
narrative_ontology:measurement(gene_tr_t30, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement(gene_tr_t45, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 45, 0.35).
narrative_ontology:measurement(gene_tr_t60, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 60, 0.4).
narrative_ontology:measurement(gene_tr_t75, geneva_conventions_protective_scope__state_centric_reading, theater_ratio, 75, 0.45).

% Extraction over time
narrative_ontology:measurement(gene_be_t0, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(gene_be_t15, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 15, 0.35).
narrative_ontology:measurement(gene_be_t30, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 30, 0.42).
narrative_ontology:measurement(gene_be_t45, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 45, 0.5).
narrative_ontology:measurement(gene_be_t60, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 60, 0.55).
narrative_ontology:measurement(gene_be_t75, geneva_conventions_protective_scope__state_centric_reading, base_extractiveness, 75, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t0, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(gene_su_t15, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 15, 0.4).
narrative_ontology:measurement(gene_su_t30, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 30, 0.48).
narrative_ontology:measurement(gene_su_t45, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 45, 0.55).
narrative_ontology:measurement(gene_su_t60, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 60, 0.62).
narrative_ontology:measurement(gene_su_t75, geneva_conventions_protective_scope__state_centric_reading, suppression_requirement, 75, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_protective_scope__state_centric_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__state_centric_reading, universal_rights_reading).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__state_centric_reading, hybrid_proportionality_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the geneva_conventions_protective_scope kernel. The state_centric_reading, universal_rights_reading, and hybrid_proportionality_reading are structurally distinct claims with different epsilon values, beneficiary/victim structures, and coordination functions. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
