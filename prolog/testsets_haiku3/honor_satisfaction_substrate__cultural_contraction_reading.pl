% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_substrate__cultural_contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Honor Code Substrate Erosion (Cultural Contraction Reading)
 *   domain: social/cultural/legal
 *
 * SUMMARY:
 *   The cultural contraction reading frames the decline of dueling as a
 *   transformation in the substrate supporting honor satisfaction itself.
 *   Honor codes did not persist with legal suppression layered on top;
 *   instead, the very cultural logic that made dueling a thinkable,
 *   obligatory response to certain slights underwent foundational erosion.
 *   Cultures of honor, organized around reputation sanctioned by peer
 *   judgment and blood vindication, contracted and gave way to cultures of
 *   dignity, organized around legal standing, character reputation, and civil
 *   discourse. This reading claims dueling became unthinkable not because the
 *   state successfully coerced abandonment, but because the interpretive
 *   substrate that validated dueling had dissolved. Aristocratic gentry found
 *   themselves identity-locked not by enforced suppression but by the
 *   incoherence of a frame they had internalized — the honor code's logic no
 *   longer functioned as a validation system within the wider cultural order.
 *
 * KEY AGENTS:
 *   - aristocratic_gentry (identity-locked payers; bear the cost of substrate collapse)
 *   - dignity_culture_carriers (beneficiaries; articulate and propagate the alternative frame)
 *   - legal_enforcement_apparatus (agenda-setter; administers suppression, but faces declining burden)
 *   - dueling_victims (excluded; structurally absent from the conversation)
 *   - observer_anthropological (analytical; takes the transformation as the object of study)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_substrate__cultural_contraction_reading, 0.38).
domain_priors:suppression_score(honor_satisfaction_substrate__cultural_contraction_reading, 0.22).
domain_priors:theater_ratio(honor_satisfaction_substrate__cultural_contraction_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, accessibility_collapse, 0.91).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_substrate__cultural_contraction_reading, mountain).
narrative_ontology:human_readable(honor_satisfaction_substrate__cultural_contraction_reading, "Honor Code Substrate Erosion (Cultural Contraction Reading)").
narrative_ontology:topic_domain(honor_satisfaction_substrate__cultural_contraction_reading, "social/cultural/legal").

domain_priors:emerges_naturally(honor_satisfaction_substrate__cultural_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_substrate__cultural_contraction_reading, 'aaca3e80-addf-4747-bac8-7aecefaff40c').
narrative_ontology:cs_kernel_codification('aaca3e80-addf-4747-bac8-7aecefaff40c', distributed).
narrative_ontology:cs_authority_grounding('aaca3e80-addf-4747-bac8-7aecefaff40c', practice).
narrative_ontology:cs_interpretation_layer_present('aaca3e80-addf-4747-bac8-7aecefaff40c').
narrative_ontology:cs_reading_relation('aaca3e80-addf-4747-bac8-7aecefaff40c', honor_satisfaction_substrate__practice_decline_reading, forecloses).
narrative_ontology:cs_reading_relation('aaca3e80-addf-4747-bac8-7aecefaff40c', honor_satisfaction_substrate__composite_overdetermined_reading, influences).
narrative_ontology:cs_axiom('aaca3e80-addf-4747-bac8-7aecefaff40c', foundational, honor_code_substrate_irreversibly_eroded).
narrative_ontology:cs_axiom_status(honor_code_substrate_irreversibly_eroded, holdable).
narrative_ontology:cs_axiom_grounding('aaca3e80-addf-4747-bac8-7aecefaff40c', honor_code_substrate_irreversibly_eroded, empirically_contingent).
narrative_ontology:cs_axiom('aaca3e80-addf-4747-bac8-7aecefaff40c', foundational, dignity_frame_logically_incompatible_with_dueling_justification).
narrative_ontology:cs_axiom_status(dignity_frame_logically_incompatible_with_dueling_justification, holdable).
narrative_ontology:cs_axiom_grounding('aaca3e80-addf-4747-bac8-7aecefaff40c', dignity_frame_logically_incompatible_with_dueling_justification, deontological).
narrative_ontology:cs_reference_frame('aaca3e80-addf-4747-bac8-7aecefaff40c', honor_code_as_primary_status_substrate).
narrative_ontology:cs_drift_state('aaca3e80-addf-4747-bac8-7aecefaff40c', contemporary_dignity_culture_dominance, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('aaca3e80-addf-4747-bac8-7aecefaff40c', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_substrate__cultural_contraction_reading, honor_satisfaction_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__cultural_contraction_reading, dignity_culture_carriers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__cultural_contraction_reading, aristocratic_gentry).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The historical class for whom dueling was a primary honor-satisfaction mechanism and identity anchor. As the cultural substrate shifted from honor to dignity, their exit from dueling became unthinkable not because of external legal barriers but because the very logic that made dueling meaningful — the honor code's validation system — had dissolved. They bore the identity cost of this substrate collapse.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, aristocratic_gentry, payer,
    powerful, generational, identity_locked, national).

% Administered prohibitions against dueling through statute and prosecution. But this reading claims legal suppression was secondary to cultural substrate erosion: the apparatus faced declining enforcement burden because dueling was already becoming unthinkable within the reframed cultural logic, not because the apparatus successfully coerced abandonment.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, legal_enforcement_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Intellectuals, clergy, bourgeois reformers, and cultural authorities who reframed honor satisfaction away from dueling and toward dignity — personal character, legal standing, reputation in civil discourse. They articulated an alternative substrate for self-respect that made dueling structurally incoherent. The constraint's dissolution benefited them by establishing their reading as the legitimate frame.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, dignity_culture_carriers, beneficiary,
    organized, generational, mobile, national).

% Those killed or maimed in duels. They are excluded from the constraint's conversation about its own legitimacy — dueling culture never incorporated their voices or counted their deaths as evidence against the practice. Their exclusion was structural to the honor code's logic.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, dueling_victims, excluded,
    powerless, biographical, trapped, national).

% Analytical seat taking the constraint story as an object of study: the transformation from honor-satisfaction-through-dueling to dignity-satisfaction-through-reputation-in-law. This reading's frame itself.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, observer_anthropological, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The honor code coordinated status-dispute resolution and reputation validation in aristocratic societies where legal institutions did not adjudicate personal insult. Dueling was one legitimized mechanism. As dignity culture emerged, status disputes began to coordinate around legal standing and character reputation — a different mechanism solving the same coordination problem.
% TRANSFER_FUNCTION: Early: honor code transferred validation authority to peer judgment and blood vindication; individuals risked death to gain/maintain status. Late: dignity code transfers validation authority to legal standing and civil reputation; individuals invest in legal defense and reputation in law. The constraint's disappearance is the shift in what mechanism solves the status-validation problem.
% ABSENT_VOICES: Dueling victims (the dead and maimed), lower classes, women, colonized peoples, and pacifist religious communities never had standing in the honor code's conversation about what counts as legitimate reputation repair. Their alternative values were never seated; their objections were treated as outside the frame entirely. The dignity culture's emergence did incorporate some previously-excluded voices (though not all), but only as the substrate itself shifted.
% DISAPPEARANCE_RATIONALE: The constraint is not the practice of dueling per se, but the cultural logic that made dueling a thinkable, obligatory response to insult. If that logic had not eroded, dueling would have persisted or adapted. The world rearranged because the substrate supporting honor-satisfaction-through-dueling dissolved; dignity culture became the dominant frame for status validation, and dueling stopped making sense within it.
% FOUNDING_PROBLEM: In the absence of legal institutional capacity to adjudicate reputation and resolve status disputes, how can elites maintain honor claims and satisfy personal insults within a coherent social order?
% FOUNDING_PROBLEM_CORROBORATION: Legal historians document the expansion of defamation, slander, and reputation law that made legal standing a viable alternative to violence for reputation repair. Cultural anthropologists document the articulation and propagation of dignity frames by reformers, intellectuals, and clergy. Comparative historians show jurisdictions adopting dignity language and losing dueling practice in correlated patterns. This testimony comes from outside the honor-culture beneficiary set; it is not the self-justifying narrative of the gentry defending their practice.
narrative_ontology:disappearance_verdict(honor_satisfaction_substrate__cultural_contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_satisfaction_substrate__cultural_contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_substrate__cultural_contraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(honor_satisfaction_substrate__cultural_contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_substrate__cultural_contraction_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   The metrics show a declining trajectory: base_extractiveness falls from 0.42 to 0.15, suppression_requirement from 0.28 to 0.08, theater_ratio from 0.12 to 0.02. This profile is consistent with mountain erosion rather than enforced suppression. The coercion grid shows stakes_inflation collapsing across all levels (from 0.78–0.85 to 0.08–0.22), while accessibility_collapse remains high and even rises slightly, indicating the alternative (non-dueling, dignity-based) action-set becomes dominant and dueling exits the thinkable space entirely. Suppression requirement falls because enforcement burden lightens as the cultural substrate supporting dueling dissolves — there is less resistance to overcome because the logic supporting resistance has eroded. Resistance itself is minimal (0.08–0.12 range, never high) because those embedded in the honor code are identity-locked in an incoherent frame, not actively resisting suppression. The beneficiary declaration (dignity_culture_carriers) triggers FSM candidate status; the omegas address whether this is genuinely a natural shift in cultural substrate or a constructed constraint benefiting a particular class.
 *
 * PERSPECTIVAL GAP:
 *   From the aristocratic gentry's seat, the constraint appears as cultural collapse — the loss of a validation system they depended on for identity. From the dignity culture carriers' seat, it is the emergence of a superior frame that makes violence obsolete. From the legal apparatus's seat, it is a successful suppression project. From the analytical seat, it is substrate transformation. The engine will compute different type-classifications per seat from the same structural data: the gentry may experience it as snare (coerced loss of identity options), the reformers as rope (coordination on a new dignity-based system), the apparatus as piton (administration of a declining practice). This reading's own framing privileges the substrate-erosion interpretation.
 *
 * DIRECTIONALITY LOGIC:
 *   Aristocratic gentry are coded as payers with identity_locked exit because their participation in honor-satisfaction-through-dueling was constitutive of their social identity. As the cultural substrate shifted, their exit became unthinkable not due to legal coercion but because the logic that had made dueling meaningful dissolved. They paid the identity cost of this substrate collapse — they could not maintain honor-code identity within the dignity-culture frame. Dignity culture carriers (bourgeois reformers, intellectuals, clergy) are coded as beneficiaries because the shift to dignity-culture legitimated their values and gave them cultural authority. They articulated the alternative frame and benefited from its ascendance. Legal enforcement apparatus faces declining suppression burden because cultural substrate erosion did the work; they administrate the transition but do not drive it. Dueling victims are excluded because the honor code never granted them standing in its conversation — their deaths were not evidence against the practice within the frame that sustained it.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids mandatrophy by anchoring in an endogenous cultural shift, not an exogenous legal regime. The founding problem (how to settle honor disputes in the absence of legal adjudication) is genuinely dead — legal systems expanded to adjudicate reputation, and dignity frameworks made character-in-law a sufficient source of self-respect. The mandate (honor code as the primary substrate for status validation) became obsolete when the cultural logic shifted. Dueling did not become mandatrophy-laden because the constraint was not a persistent extraction mechanism defended by ritual maintenance; it was a cultural logic that eroded. The alternative readings (practice_decline_reading, composite_overdetermined_reading) carry higher mandatrophy risk: if the honor code persisted and only legal suppression declined, or if both mechanisms operated simultaneously, then the constraint could become a zombie (suppression authority no longer invested, but cultural logic re-emerges). This reading forecloses that scenario by asserting the substrate itself dissolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    substrate_vs_suppression_boundary,
    'Was dueling''s decline driven primarily by the cultural substrate''s collapse (honor → dignity), or by exogenous legal/institutional suppression, or both non-independently?',
    'Historical analysis of the sequence: Do legal prohibitions precede cultural reframing, follow it, or overlap? Did dueling persist where dignity culture had not yet been articulated? Did enforcement intensity drive cultural change, or did cultural change make enforcement unnecessary? Comparison across jurisdictions with different legal prohibition timelines and different cultural adoption rates of dignity frames.',
    'This reading (cultural_contraction_reading) claims substrate erosion as primary; the practice_decline_reading claims persistent honor code + exogenous suppression; the composite_overdetermined_reading claims non-independent simultaneous causation. The classification depends on the directionality of causation and the explanatory weight of each factor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substrate_vs_suppression_boundary, empirical, 'Whether dueling declined because the honor code eroded, or the honor code persisted while external pressure suppressed the practice.').

omega_variable(
    mountain_vs_constructed_ambiguity,
    'Is honor-code-substrate-erosion a genuine ''mountain'' — an irreversible shift in the thinkability of dueling — or a contingent cultural construction that could theoretically be reconstructed?',
    'Counterfactual analysis: Could honor culture be revived if dignity culture were discredited? What would it take? If revival requires a complete re-embedding in social institutions and identity formation (not just intellectual argument), and if modern dignity-based institutions have rendered honor-satisfaction-through-dueling socially incoherent at the structural level, then the mountain claim holds. If dueling persists in pockets or could easily re-emerge, the claim weakens.',
    'A true mountain claim asserts the substrate shift is irreversible for any party within the modern framework. If the substrate is fragile or contingent, the constraint is better classified as a snare with cultural enforcement (dignity ideology) rather than a mountain. The beneficiary declaration (dignity_culture_carriers) requires an omega documenting this ambiguity per FSM rules.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mountain_vs_constructed_ambiguity, conceptual, 'Whether honor-code erosion is an irreversible structural shift (mountain) or a reversible cultural choice (constructed constraint).').

omega_variable(
    identity_lock_mechanism_aristocracy,
    'For aristocratic gentry coded as identity_locked: Is their exit from dueling suppressed by internalized honor-code identity, by structural legal barriers, or by the substrate''s collapse having made the identity frame itself incoherent?',
    'Post-exit trajectories: Did aristocrats who abandoned dueling maintain honor-code identity in other domains (seeking vindication through law, reputation, social status) or did they shift to dignity-based identity? If they maintained honor but in transformed expressions, the substrate has shifted but not fully dissolved for them. If they fully adopted dignity frames, the substrate collapse is more complete.',
    'If identity-lock is sourced in an incoherent substrate (the honor code no longer functions as a validation system), the exit is not suppressed in the traditional sense — it is made unthinkable because the logic supporting it dissolved. This is a mountain-like mechanism (alternatives collapse because the old frame is no longer actionable), not suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(identity_lock_mechanism_aristocracy, empirical, 'Whether aristocratic identity-lock stems from persistent honor-code internalization or from substrate dissolution.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_substrate__cultural_contraction_reading, 1650, 1850).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1650, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1650, 0.12).
narrative_ontology:measurement(hono_tr_t1700, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1700, 0.11).
narrative_ontology:measurement(hono_tr_t1750, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1750, 0.1).
narrative_ontology:measurement(hono_tr_t1800, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1800, 0.07).
narrative_ontology:measurement(hono_tr_t1850, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1850, 0.02).

% Extraction over time
narrative_ontology:measurement(hono_be_t1650, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1650, 0.42).
narrative_ontology:measurement(hono_be_t1700, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1700, 0.4).
narrative_ontology:measurement(hono_be_t1750, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1750, 0.38).
narrative_ontology:measurement(hono_be_t1800, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1800, 0.32).
narrative_ontology:measurement(hono_be_t1850, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1850, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1650, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 1650, 0.28).
narrative_ontology:measurement(hono_su_t1700, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 1700, 0.26).
narrative_ontology:measurement(hono_su_t1750, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 1750, 0.24).
narrative_ontology:measurement(hono_su_t1800, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 1800, 0.18).
narrative_ontology:measurement(hono_su_t1850, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 1850, 0.08).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1650, tn=1850
narrative_ontology:measurement(hono_grid_01, honor_satisfaction_substrate__cultural_contraction_reading, accessibility_collapse(class), 1650, 0.92).
narrative_ontology:measurement(hono_grid_02, honor_satisfaction_substrate__cultural_contraction_reading, accessibility_collapse(class), 1850, 0.97).
narrative_ontology:measurement(hono_grid_03, honor_satisfaction_substrate__cultural_contraction_reading, accessibility_collapse(individual), 1650, 0.89).
narrative_ontology:measurement(hono_grid_04, honor_satisfaction_substrate__cultural_contraction_reading, accessibility_collapse(individual), 1850, 0.95).
narrative_ontology:measurement(hono_grid_05, honor_satisfaction_substrate__cultural_contraction_reading, accessibility_collapse(organizational), 1650, 0.85).
narrative_ontology:measurement(hono_grid_06, honor_satisfaction_substrate__cultural_contraction_reading, accessibility_collapse(organizational), 1850, 0.92).
narrative_ontology:measurement(hono_grid_07, honor_satisfaction_substrate__cultural_contraction_reading, accessibility_collapse(structural), 1650, 0.88).
narrative_ontology:measurement(hono_grid_08, honor_satisfaction_substrate__cultural_contraction_reading, accessibility_collapse(structural), 1850, 0.94).
narrative_ontology:measurement(hono_grid_09, honor_satisfaction_substrate__cultural_contraction_reading, resistance(class), 1650, 0.14).
narrative_ontology:measurement(hono_grid_10, honor_satisfaction_substrate__cultural_contraction_reading, resistance(class), 1850, 0.18).
narrative_ontology:measurement(hono_grid_11, honor_satisfaction_substrate__cultural_contraction_reading, resistance(individual), 1650, 0.12).
narrative_ontology:measurement(hono_grid_12, honor_satisfaction_substrate__cultural_contraction_reading, resistance(individual), 1850, 0.16).
narrative_ontology:measurement(hono_grid_13, honor_satisfaction_substrate__cultural_contraction_reading, resistance(organizational), 1650, 0.1).
narrative_ontology:measurement(hono_grid_14, honor_satisfaction_substrate__cultural_contraction_reading, resistance(organizational), 1850, 0.03).
narrative_ontology:measurement(hono_grid_15, honor_satisfaction_substrate__cultural_contraction_reading, resistance(structural), 1650, 0.08).
narrative_ontology:measurement(hono_grid_16, honor_satisfaction_substrate__cultural_contraction_reading, resistance(structural), 1850, 0.02).
narrative_ontology:measurement(hono_grid_17, honor_satisfaction_substrate__cultural_contraction_reading, stakes_inflation(class), 1650, 0.85).
narrative_ontology:measurement(hono_grid_18, honor_satisfaction_substrate__cultural_contraction_reading, stakes_inflation(class), 1850, 0.12).
narrative_ontology:measurement(hono_grid_19, honor_satisfaction_substrate__cultural_contraction_reading, stakes_inflation(individual), 1650, 0.8).
narrative_ontology:measurement(hono_grid_20, honor_satisfaction_substrate__cultural_contraction_reading, stakes_inflation(individual), 1850, 0.08).
narrative_ontology:measurement(hono_grid_21, honor_satisfaction_substrate__cultural_contraction_reading, stakes_inflation(organizational), 1650, 0.78).
narrative_ontology:measurement(hono_grid_22, honor_satisfaction_substrate__cultural_contraction_reading, stakes_inflation(organizational), 1850, 0.22).
narrative_ontology:measurement(hono_grid_23, honor_satisfaction_substrate__cultural_contraction_reading, stakes_inflation(structural), 1650, 0.82).
narrative_ontology:measurement(hono_grid_24, honor_satisfaction_substrate__cultural_contraction_reading, stakes_inflation(structural), 1850, 0.18).
narrative_ontology:measurement(hono_grid_25, honor_satisfaction_substrate__cultural_contraction_reading, suppression(class), 1650, 0.26).
narrative_ontology:measurement(hono_grid_26, honor_satisfaction_substrate__cultural_contraction_reading, suppression(class), 1850, 0.08).
narrative_ontology:measurement(hono_grid_27, honor_satisfaction_substrate__cultural_contraction_reading, suppression(individual), 1650, 0.3).
narrative_ontology:measurement(hono_grid_28, honor_satisfaction_substrate__cultural_contraction_reading, suppression(individual), 1850, 0.12).
narrative_ontology:measurement(hono_grid_29, honor_satisfaction_substrate__cultural_contraction_reading, suppression(organizational), 1650, 0.28).
narrative_ontology:measurement(hono_grid_30, honor_satisfaction_substrate__cultural_contraction_reading, suppression(organizational), 1850, 0.04).
narrative_ontology:measurement(hono_grid_31, honor_satisfaction_substrate__cultural_contraction_reading, suppression(structural), 1650, 0.32).
narrative_ontology:measurement(hono_grid_32, honor_satisfaction_substrate__cultural_contraction_reading, suppression(structural), 1850, 0.06).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_substrate__cultural_contraction_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__cultural_contraction_reading, honor_satisfaction_substrate__practice_decline_reading).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__cultural_contraction_reading, honor_satisfaction_substrate__composite_overdetermined_reading).

% DUAL FORMULATION NOTE:
% The honor_satisfaction_substrate kernel decomposes into three distinct constraint stories per the ε-invariance principle. Each reading instantiates a different constraint with different ε values, different beneficiary/victim structures, and different classification outcomes. (1) cultural_contraction_reading claims honor substrate eroded; dueling became unthinkable within dignity frame — mountain-type natural shift in cultural logic. (2) practice_decline_reading claims honor code persisted; dueling declined due to exogenous suppression (legal, institutional, opportunity cost) — snare-type or tangled_rope-type extraction. (3) composite_overdetermined_reading claims both mechanisms operated simultaneously with non-independent causal pathways. The readings are linked by network.affects_constraints; they share a kernel but constitute separate empirical and analytical claims. No single observation can falsify all three; the family exists to enable comparative analysis of competing causal narratives about the same historical phenomenon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
