% ============================================================================
% CONSTRAINT STORY: honor_settlement_legitimacy__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_settlement_legitimacy__contraction_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: honor_settlement_legitimacy__contraction_reading
 *   human_readable: Dueling Unthinkability via Cultural Framework Transformation
 *   domain: historical_sociology/cultural_anthropology
 *
 * SUMMARY:
 *   This constraint models the contraction_reading of the
 *   honor_settlement_legitimacy kernel: the claim that dueling became not
 *   merely prohibited but cognitively unthinkable through a transformation of
 *   the cultural framework itself. Honor culture — the entire semantic and
 *   normative field that made dueling intelligible as legitimate action —
 *   exits the possibility space. The constraint is claimed as a mountain
 *   because the unthinkability presents as a natural cognitive limit: modern
 *   persons literally cannot conceive of killing over an insult as honorable.
 *   But identifiable beneficiaries exist (state legal monopoly, professional
 *   classes, bourgeois citizens), triggering FSM evaluation. The reading's
 *   axiom — that honor violence is categorically excluded from legitimate
 *   action — forecloses the drop_reading's claim that residual honor cultures
 *   persist, while the composite_reading treats both as simultaneously
 *   operative.
 *
 * KEY AGENTS:
 *   - state_legal_monopoly: Primary beneficiary (institutional/arbitrage) — gains legitimacy from honor violence unthinkability
 *   - modern_professional_classes: Primary beneficiary (organized/mobile) — professional honor replaces violent honor
 *   - bourgeois_citizens: Primary beneficiary (moderate/mobile) — citizenship rights replace honor status
 *   - aristocratic_officer_corps: Excluded (powerful/trapped) — residual honor culture practitioners
 *   - dueling_participants: Victim (historical, powerless/trapped) — historical targets of the transition
 *   - historical_sociologist: Observer (analytical/analytical) — sees full structural transformation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_settlement_legitimacy__contraction_reading, 0.05).
domain_priors:suppression_score(honor_settlement_legitimacy__contraction_reading, 0.15).
domain_priors:theater_ratio(honor_settlement_legitimacy__contraction_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_settlement_legitimacy__contraction_reading, mountain).
narrative_ontology:human_readable(honor_settlement_legitimacy__contraction_reading, "Dueling Unthinkability via Cultural Framework Transformation").
narrative_ontology:topic_domain(honor_settlement_legitimacy__contraction_reading, "historical_sociology/cultural_anthropology").

domain_priors:emerges_naturally(honor_settlement_legitimacy__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_settlement_legitimacy__contraction_reading, '0188bac4-9488-4f1c-9ff0-23a1e7383c5f').
narrative_ontology:cs_kernel_codification('0188bac4-9488-4f1c-9ff0-23a1e7383c5f', distributed).
narrative_ontology:cs_authority_grounding('0188bac4-9488-4f1c-9ff0-23a1e7383c5f', diffuse_epistemic).
narrative_ontology:cs_reading_relation('0188bac4-9488-4f1c-9ff0-23a1e7383c5f', honor_settlement_legitimacy__drop_reading, forecloses).
narrative_ontology:cs_reading_relation('0188bac4-9488-4f1c-9ff0-23a1e7383c5f', honor_settlement_legitimacy__composite_reading, influences).
narrative_ontology:cs_axiom('0188bac4-9488-4f1c-9ff0-23a1e7383c5f', foundational, honor_violence_categorically_excluded).
narrative_ontology:cs_axiom_status(honor_violence_categorically_excluded, holdable).
narrative_ontology:cs_axiom_grounding('0188bac4-9488-4f1c-9ff0-23a1e7383c5f', honor_violence_categorically_excluded, deontological).
narrative_ontology:cs_axiom('0188bac4-9488-4f1c-9ff0-23a1e7383c5f', secondary, cognitive_framework_determines_legitimacy).
narrative_ontology:cs_axiom_status(cognitive_framework_determines_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('0188bac4-9488-4f1c-9ff0-23a1e7383c5f', cognitive_framework_determines_legitimacy, conventional).
narrative_ontology:cs_reference_frame('0188bac4-9488-4f1c-9ff0-23a1e7383c5f', aristocratic_honor_framework).
narrative_ontology:cs_drift_state('0188bac4-9488-4f1c-9ff0-23a1e7383c5f', modern_citizenship_framework, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('0188bac4-9488-4f1c-9ff0-23a1e7383c5f', '').
narrative_ontology:cs_kernel_id(honor_settlement_legitimacy__contraction_reading, honor_settlement_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__contraction_reading, state_legal_monopoly).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__contraction_reading, modern_professional_classes).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__contraction_reading, bourgeois_citizens).
narrative_ontology:constraint_vindicates(honor_settlement_legitimacy__contraction_reading, cognitive_unthinkability_of_honor_violence).
narrative_ontology:constraint_vindicates(honor_settlement_legitimacy__contraction_reading, state_monopoly_on_legitimate_violence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gains unchallenged legitimacy monopoly on violence once honor violence becomes unthinkable. No longer needs to actively suppress dueling; the cultural framework does the work. Exit from this benefit is meaningless — the state is the framework.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, state_legal_monopoly, beneficiary,
    institutional, civilizational, arbitrage, global).

% Professional honor (competence, ethics, peer recognition) replaces aristocratic honor (violence, lineage, personal courage). The unthinkability of dueling clears the field for professional status systems. Can exit by leaving the professional system, but the cognitive framework travels with them.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, modern_professional_classes, beneficiary,
    organized, generational, mobile, global).

% Rights-bearing citizenship replaces honor-status. Legal predictability and impersonal contract replace personal violence. The constraint enables the bourgeois social order. Exit is emigration, but the cognitive framework is global.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, bourgeois_citizens, beneficiary,
    moderate, biographical, mobile, global).

% Historical carriers of honor culture. As the framework shifts, their honor practices become incomprehensible to the wider society, then illegal, then unthinkable. They cannot exit the constraint without abandoning their identity — hence trapped. Residual dueling persists in this subculture (drop_reading's empirical claim).
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, aristocratic_officer_corps, excluded,
    powerful, biographical, trapped, national).

% Historical agents who paid with their lives during the transition period. Not current stakeholders — the constraint's current operation does not extract from them. Listed for historical completeness and kernel genealogy.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, dueling_participants, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_non_agent(honor_settlement_legitimacy__contraction_reading, dueling_participants).

% Analyzes the constraint from outside its operation. Sees the full structure: the cognitive mountain, the beneficiaries, the excluded residual honor cultures, the historical victims of the transition. Their analysis is the engine's input.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, historical_sociologist, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_settlement_legitimacy__contraction_reading, diffuse).
narrative_ontology:fixing_cost_class(honor_settlement_legitimacy__contraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared cognitive framework in which violence over honor is not an option — enabling impersonal law, professional trust, and rights-based citizenship to function without the constant threat of honor violence.
% TRANSFER_FUNCTION: Transfers legitimacy from personal honor (backed by violence) to institutional honor (backed by law and professional norms). The transfer is not a flow of resources but a restructuring of the status economy.
% ABSENT_VOICES: Residual honor culture practitioners (aristocratic officer corps, student corps, certain military subcultures) who would object to the claim that honor violence is unthinkable — they are structurally excluded from the mainstream cognitive framework but persist in subcultural niches.
% DISAPPEARANCE_RATIONALE: If the unthinkability constraint vanished overnight, honor violence would not immediately return — but the cognitive barrier preventing its legitimacy would be gone. The state would need active suppression; professional honor systems would face competition from revived honor cultures; the legitimacy monopoly would become fragile. The world rearranges because the constraint is the cognitive infrastructure of modern order.
% FOUNDING_PROBLEM: Honor violence as a mechanism of social order and status allocation in aristocratic societies — unpredictable, personally costly, and incompatible with impersonal law and market exchange.
% FOUNDING_PROBLEM_CORROBORATION: Standard historical sociology consensus (Weber, Elias, Nye, Kingston) attests the founding problem is dead: honor violence no longer functions as a social order mechanism in modern societies. The contraction_reading's beneficiaries (state, professions, bourgeoisie) all corroborate this from their institutional positions. No living party claims honor violence is still a viable social order mechanism — the drop_reading claims residual practice persists, not that the founding problem lives.
narrative_ontology:disappearance_verdict(honor_settlement_legitimacy__contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_settlement_legitimacy__contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_settlement_legitimacy__contraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(honor_settlement_legitimacy__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_settlement_legitimacy__contraction_reading, 0.05, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_settlement_legitimacy__contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(honor_settlement_legitimacy__contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(honor_settlement_legitimacy__contraction_reading),
    narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(honor_settlement_legitimacy__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness near zero (0.05): the constraint does not extract resources from those it governs; it constitutes the cognitive boundary within which modern persons operate. Suppression low (0.15) and declining: early legal prohibitions required enforcement, but the cultural transformation makes enforcement increasingly unnecessary — the constraint becomes self-sustaining. Theater ratio negligible (0.05): no performative maintenance needed once the framework shifts. Accessibility collapse very high (0.92): alternatives (honor violence) are not merely suppressed but cognitively inaccessible. Resistance near zero (0.02): no organized resistance to the unthinkability itself; resistance would be to the cultural transformation that produced it, not to the resulting constraint.
 *
 * PERSPECTIVAL GAP:
 *   From the analytical observer seat, this is a mountain — a cognitive boundary that structures modern subjectivity. From the aristocratic_officer_corps seat (historically), the constraint was experienced as a snare/tangled_rope during transition: active suppression of their honor practices, extraction of status and legitimacy. From the bourgeois_citizens seat, experienced as a rope: coordination gain (legal predictability, professional status) with minimal coercion. The engine computes this divergence from the structural data — the claimed mountain type reflects the analytical seat's view, which is the reading's own frame.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries declared: state_legal_monopoly gains legitimacy monopoly (d → 0.0); modern_professional_classes gain professional honor system (d → 0.1); bourgeois_citizens gain rights-based status (d → 0.2). No victims declared for the current constraint state — historical dueling_participants were victims of the transition process, not of the current mountain. The unthinkability itself extracts nothing; it constitutes the field. The omega on natural_law_vs_constructed_unthinkability captures the FSM ambiguity: if the cultural transformation was driven by beneficiary interests, the mountain claim is a false summit.
 *
 * MANDATROPHY ANALYSIS:
 *   Not applicable — the constraint has no mandate that could atrophy. It is a cognitive boundary, not an institutional arrangement. The founding problem (honor violence as social order mechanism) is dead; the arrangement (unthinkability) persists because it is the cognitive condition of modernity, not because it serves a function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_unthinkability,
    'Is the unthinkability of dueling a genuine cognitive mountain (natural-law-like invariance) or a constructed cultural achievement that benefits identifiable agents?',
    'Cross-cultural and historical comparison: if societies without state legal monopolies or bourgeois classes also develop unthinkability of honor violence, the cognitive mountain claim is strengthened; if unthinkability tracks state formation and professional class emergence, the false summit hypothesis is supported.',
    'If false summit, reclassification to tangled_rope with state_legal_monopoly and modern_professional_classes as beneficiaries extracting legitimacy rents from the constraint''s mountain presentation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_unthinkability, conceptual, 'Whether cognitive unthinkability is a natural constraint or a constructed one with beneficiaries').

omega_variable(
    kernel_reading_framing,
    'Does this contraction_reading accurately capture the kernel''s structural dynamics, or does it foreclose the drop_reading''s empirical claim that residual honor violence persists?',
    'Historical ethnography of 19th-20th century dueling persistence in military, aristocratic, and student corps subcultures; quantitative analysis of dueling incidence after legal prohibition.',
    'If drop_reading''s persistence claim is empirically robust, contraction_reading''s ''exit from normative possibility space'' is overstated — the constraint would be better modeled as composite_reading''s overdetermined decline.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_framing, empirical, 'Commitment-system framing: whether this reading''s axiom forecloses sibling readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_settlement_legitimacy__contraction_reading, 1750, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1750, honor_settlement_legitimacy__contraction_reading, theater_ratio, 1750, 0.02).
narrative_ontology:measurement(hono_tr_t1800, honor_settlement_legitimacy__contraction_reading, theater_ratio, 1800, 0.03).
narrative_ontology:measurement(hono_tr_t1850, honor_settlement_legitimacy__contraction_reading, theater_ratio, 1850, 0.04).
narrative_ontology:measurement(hono_tr_t1900, honor_settlement_legitimacy__contraction_reading, theater_ratio, 1900, 0.05).
narrative_ontology:measurement(hono_tr_t1950, honor_settlement_legitimacy__contraction_reading, theater_ratio, 1950, 0.05).
narrative_ontology:measurement(hono_tr_t2000, honor_settlement_legitimacy__contraction_reading, theater_ratio, 2000, 0.05).

% Extraction over time
narrative_ontology:measurement(hono_be_t1750, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1750, 0.02).
narrative_ontology:measurement(hono_be_t1800, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1800, 0.03).
narrative_ontology:measurement(hono_be_t1850, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1850, 0.04).
narrative_ontology:measurement(hono_be_t1900, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1900, 0.05).
narrative_ontology:measurement(hono_be_t1950, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1950, 0.05).
narrative_ontology:measurement(hono_be_t2000, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 2000, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1750, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 1750, 0.6).
narrative_ontology:measurement(hono_su_t1800, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 1800, 0.4).
narrative_ontology:measurement(hono_su_t1850, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 1850, 0.2).
narrative_ontology:measurement(hono_su_t1900, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 1900, 0.1).
narrative_ontology:measurement(hono_su_t1950, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 1950, 0.05).
narrative_ontology:measurement(hono_su_t2000, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 2000, 0.02).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_settlement_legitimacy__contraction_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__contraction_reading, honor_settlement_legitimacy__drop_reading).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__contraction_reading, honor_settlement_legitimacy__composite_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the honor_settlement_legitimacy kernel into three readings with distinct ε values and structural claims. contraction_reading: ε≈0.05 (mountain, cognitive unthinkability). drop_reading: ε≈0.35 (snare/tangled_rope, active suppression of residual practice). composite_reading: ε≈0.20 (tangled_rope, overdetermined decline). The readings are linked via affects_constraints; each has distinct beneficiary/victim structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
