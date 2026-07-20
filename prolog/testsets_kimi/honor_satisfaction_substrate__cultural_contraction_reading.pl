% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_substrate__cultural_contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_honor_satisfaction_substrate__cultural_contraction_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: honor_satisfaction_substrate__cultural_contraction_reading
 *   human_readable: Honor Satisfaction Substrate â Cultural Contraction Reading
 *   domain: historical_sociology/cultural_anthropology/legal_history
 *
 * SUMMARY:
 *   This constraint story instantiates the cultural_contraction_reading of
 *   the honor_satisfaction_substrate kernel. Under this reading, the
 *   obligation to satisfy affronts through dueling was not merely suppressed
 *   by exogenous legal enforcement, but became unthinkable because the honor
 *   code itself underwent foundational transformation. The 'culture of honor'
 *   collapsed as an interpretive substrate, giving way to a 'culture of
 *   dignity' in which personal violence was delegitimized. The constraint is
 *   modeled as a mountain (a naturalized social law within its historical
 *   context) that experienced endogenous erosion rather than external
 *   demolition. The story deliberately claims mountain while authoring
 *   metrics that describe a once-highly-extractive, highly-suppressive
 *   system, inviting false-summit detection: the honor code presented itself
 *   as an unchangeable feature of gentlemanly nature, yet identifiable
 *   beneficiaries (the gentleman class) captured status rents from its
 *   operation.
 *
 * KEY AGENTS:
 *   - gentleman_class_elite (beneficiary/payer, powerful, identity_locked) â captures status and distinction but bears lethal risk
 *   - commoners_and_excluded (excluded, powerless, trapped) â outside the moral community of honor
 *   - state_legal_authorities (observer, institutional, analytical) â background legal framework ineffective against the substrate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_substrate__cultural_contraction_reading, 0.3).
domain_priors:suppression_score(honor_satisfaction_substrate__cultural_contraction_reading, 0.35).
domain_priors:theater_ratio(honor_satisfaction_substrate__cultural_contraction_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_substrate__cultural_contraction_reading, mountain).
narrative_ontology:human_readable(honor_satisfaction_substrate__cultural_contraction_reading, "Honor Satisfaction Substrate â Cultural Contraction Reading").
narrative_ontology:topic_domain(honor_satisfaction_substrate__cultural_contraction_reading, "historical_sociology/cultural_anthropology/legal_history").

domain_priors:emerges_naturally(honor_satisfaction_substrate__cultural_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_substrate__cultural_contraction_reading, '515c835a-15aa-46b3-b1f5-aa1a1a317264').
narrative_ontology:cs_kernel_codification('515c835a-15aa-46b3-b1f5-aa1a1a317264', implicit).
narrative_ontology:cs_authority_grounding('515c835a-15aa-46b3-b1f5-aa1a1a317264', practice).
narrative_ontology:cs_interpretation_layer_present('515c835a-15aa-46b3-b1f5-aa1a1a317264').
narrative_ontology:cs_reading_relation('515c835a-15aa-46b3-b1f5-aa1a1a317264', honor_satisfaction_substrate__practice_decline_reading, forecloses).
narrative_ontology:cs_reading_relation('515c835a-15aa-46b3-b1f5-aa1a1a317264', honor_satisfaction_substrate__composite_overdetermined_reading, coexists_with).
narrative_ontology:cs_axiom('515c835a-15aa-46b3-b1f5-aa1a1a317264', foundational, honor_code_substrate_integrity).
narrative_ontology:cs_axiom_status(honor_code_substrate_integrity, holdable).
narrative_ontology:cs_axiom_grounding('515c835a-15aa-46b3-b1f5-aa1a1a317264', honor_code_substrate_integrity, conventional).
narrative_ontology:cs_reference_frame('515c835a-15aa-46b3-b1f5-aa1a1a317264', culture_of_honor_moral_order).
narrative_ontology:cs_drift_state('515c835a-15aa-46b3-b1f5-aa1a1a317264', culture_of_dignity_emergence, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('515c835a-15aa-46b3-b1f5-aa1a1a317264', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_substrate__cultural_contraction_reading, honor_satisfaction_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__cultural_contraction_reading, gentleman_class_elite).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__cultural_contraction_reading, gentleman_class_elite).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Their social identity and class distinction are constituted through the honor code; they benefit from status boundary maintenance and the monopoly on legitimate violence, yet also bear the physical risks, death, and economic costs of dueling. Exiting the code means forfeiting gentleman status and becoming socially unmoored.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, gentleman_class_elite, beneficiary,
    powerful, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_substrate__cultural_contraction_reading, gentleman_class_elite, payer).

% Excluded from the honor code's moral community; their inability to offer or receive satisfaction marks them as socially inferior, yet they are spared the lethal risks of dueling. They have no voice in the code's operation and no pathway to claim its protections.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, commoners_and_excluded, excluded,
    powerless, generational, trapped, national).

% Maintain legal prohibitions against dueling that were historically ineffective against the honor substrate; under this reading they observe the endogenous cultural contraction rather than drive it, serving as a backdrop to the normative transformation.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, state_legal_authorities, observer,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_satisfaction_substrate__cultural_contraction_reading, gentleman_class_elite).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the internal hierarchy of a warrior or aristocratic class by providing a ritualized, predictable mechanism for resolving status disputes and asserting masculine identity without collapsing into factional warfare or total social disorder.
% TRANSFER_FUNCTION: Moves status, masculine honor, and class boundary maintenance to the gentleman elite; moves physical risk, death, injury, and economic depletion to the same gentleman class in their capacity as duelists.
% ABSENT_VOICES: Women, commoners, and religious pacifists who bore witness to the violence but were excluded from the moral community of honor; they would contest the necessity of bloodshed but had no standing to challenge a gentleman's obligation.
% DISAPPEARANCE_RATIONALE: Without the honor substrate, the gentleman class loses its distinctive mechanism for status assertion; social organization reorients around centralized legal arbitration, commercial wealth, and bureaucratic dignity rather than personal violence and insult.
% FOUNDING_PROBLEM: How to maintain a cohesive, self-regulating elite warrior class and its hierarchical boundaries in the absence of a monopoly on violence by a centralized state.
% FOUNDING_PROBLEM_CORROBORATION: Historical sociologists such as Elias and contemporary legal historians attest the transition; state archives from the dignity-culture era corroborate that centralized courts replaced personal satisfaction, from a seat outside the benefiting gentleman class.
narrative_ontology:disappearance_verdict(honor_satisfaction_substrate__cultural_contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_satisfaction_substrate__cultural_contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_substrate__cultural_contraction_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(honor_satisfaction_substrate__cultural_contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_substrate__cultural_contraction_reading, 0.3, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_substrate__cultural_contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(honor_satisfaction_substrate__cultural_contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(honor_satisfaction_substrate__cultural_contraction_reading),
    narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(honor_satisfaction_substrate__cultural_contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.30) and suppression (0.35) are authored low to represent the end-state of the interval, by which point the cultural substrate had substantially collapsed and dueling had become unthinkable. Theater_ratio (0.50) captures the late-period performative remnants of honor rhetoric that outlived the functional dueling core. Accessibility_collapse (0.35) reflects that alternatives to honor (legal recourse, apology, ignoring affronts) had become thinkable by interval end. Resistance (0.25) acknowledges residual traditionalist opposition to the dignity transition. The measurement series shows monotonic decline in extraction and suppression from near-total levels, consistent with mountain erosion rather than enforcement atrophy.
 *
 * PERSPECTIVAL GAP:
 *   The gentleman class experienced the constraint simultaneously as the seat of benefit (status distinction) and cost (physical risk), creating a dual-position asymmetry. From within the honor culture, the code appeared as an unchangeable mountain; from the dignity-culture outside, the same code appears as a constructed snare or tangled rope benefiting a narrow elite. The engine will compute different per-seat classifications depending on whether the agent is coded as beneficiary or payer.
 *
 * DIRECTIONALITY LOGIC:
 *   The gentleman_class_elite is declared as beneficiary because the honor code's primary structural effect was to concentrate legitimate violence and status distinction in this class. They are also secondary_role payer because they bore the extraction (death, injury) directly. Commoners are excluded â their structural position is defined by absence from the code. State authorities are observers under this reading because the cultural contraction reading backgrounds their causal role.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â maintaining aristocratic hierarchy without centralized state violence â is dead. The constraint persists today only as historical memory and occasional performative rhetoric. The mismatch between founding_problem_status=dead and disappearance_verdict=world_rearranges flags that the arrangement's disappearance would (and did) cause social rearrangement, preventing mislabeling as a mere piton. However, the claim of mountain erosion prevents mislabeling the collapse as the result of mere exogenous suppression (the snare frame); instead, the substrate itself dissolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_benefit,
    'Is the honor code a genuine natural outgrowth of aristocratic social organization, or a constructed system concentrating status and lethal risk in the gentleman class?',
    'Cross-cultural comparison: if honor dueling appears only under specific class structures and disappears when those structures change, the constraint is constructed; if it is a human universal under anarchy, it is closer to natural law.',
    'If constructed, the mountain claim is a false summit and the constraint reclassifies as a tangled rope or snare benefiting the gentleman elite; if natural, the cultural contraction reading describes genuine mountain erosion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_benefit, conceptual, 'Whether the honor code is natural or constructed').

omega_variable(
    suppression_mechanism_ambiguity,
    'Was the unthinkability of refusing a duel sustained by internalized identity fusion or by external social ostracism?',
    'Biographical and literary evidence examining whether gentlemen who abstained from dueling suffered internal shame independent of social detection, or solely external sanction.',
    'If internalized, effective suppression is higher than structural measures suggest and the constraint operates as identity-locked extraction; if external, suppression scales with community density.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Internalized versus structural suppression of dueling refusal').

omega_variable(
    substrate_erosion_vs_atrophy,
    'Did the honor code collapse because its foundational substrate eroded (mountain erosion), or because its functional purpose atrophied while theatrical remnants persisted (piton degradation)?',
    'Examine whether late-period honor rhetoric retained any enforcement power or was purely nostalgic; if enforcement power persisted, the constraint was piton-like; if it dissolved entirely, erosion is the better model.',
    'If piton-like, the constraint should carry higher theater_ratio and lower base_extractiveness; if eroded mountain, the dissolution is cleaner and more total.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(substrate_erosion_vs_atrophy, conceptual, 'Whether the collapse was substrate erosion or piton atrophy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_substrate__cultural_contraction_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t0, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(hono_tr_t5, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 5, 0.2).
narrative_ontology:measurement(hono_tr_t10, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(hono_tr_t15, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 15, 0.42).
narrative_ontology:measurement(hono_tr_t20, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 20, 0.5).

% Extraction over time
narrative_ontology:measurement(hono_be_t0, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 0, 0.85).
narrative_ontology:measurement(hono_be_t5, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 5, 0.7).
narrative_ontology:measurement(hono_be_t10, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(hono_be_t15, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 15, 0.42).
narrative_ontology:measurement(hono_be_t20, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 20, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t0, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 0, 0.9).
narrative_ontology:measurement(hono_su_t5, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 5, 0.75).
narrative_ontology:measurement(hono_su_t10, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(hono_su_t15, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 15, 0.48).
narrative_ontology:measurement(hono_su_t20, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 20, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_substrate__cultural_contraction_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__cultural_contraction_reading, practice_decline_reading).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__cultural_contraction_reading, composite_overdetermined_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the honor_satisfaction_substrate kernel. The cultural_contraction_reading instantiates the substrate as an endogenously eroding mountain; sibling readings treat the same kernel as exogenously suppressed practice or overdetermined composite.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
