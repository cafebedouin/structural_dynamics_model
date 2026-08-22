% ============================================================================
% CONSTRAINT STORY: war_winnability_post_1945__deterrence_unthinkable
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_winnability_post_1945__deterrence_unthinkable, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: war_winnability_post_1945__deterrence_unthinkable
 *   human_readable: Nuclear-Age Categorical Unwinnability of Great-Power War
 *   domain: strategic_studies/nuclear_deterrence_theory/international_relations
 *
 * SUMMARY:
 *   This story authors the 'deterrence_unthinkable' reading of the contested
 *   war-winnability kernel: the claim that the advent of assured mutual
 *   destruction removed great-power total war from the space of rationally
 *   plannable outcomes altogether, not merely constraining how it could be
 *   fought. Under this reading, strategic planning's coherent object shifts
 *   entirely from victory to prevention. The reading has a genuine
 *   coordination function (crisis stability, arms control) but also produces
 *   real institutional victims (war-fighting military establishments whose
 *   founding mission becomes incoherent) and requires active enforcement
 *   (doctrine gatekeeping, arms control verification regimes, public taboo
 *   maintenance) to hold against residual counterforce planning — this is why
 *   the story is authored as tangled_rope rather than mountain or rope. This
 *   is NOT the 'countervailing_thinkable' reading (which holds limited
 *   victory remains achievable) nor the 'rhetorical_contraction' reading
 *   (which holds the unwinnability claim is discursive cover over continued
 *   war-fighting planning) — those are separate constraint files linked via
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - civilian_populations: diffuse global beneficiary, no formal voice, bears existential tail risk either way
 *   - military_establishments: institutional payer, mission coherence undermined by the reading's success
 *   - arms_control_diplomats: institutional beneficiary and agenda-setter, career and treaty architecture depend on the reading holding
 *   - deterrence_theorists: analytical beneficiary and agenda-setter, the reading is their intellectual load-bearing premise
 *   - counterforce_strategists: excluded voice, represent the sibling countervailing_thinkable position
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_winnability_post_1945__deterrence_unthinkable, 0.42).
domain_priors:suppression_score(war_winnability_post_1945__deterrence_unthinkable, 0.55).
domain_priors:theater_ratio(war_winnability_post_1945__deterrence_unthinkable, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, extractiveness, 0.42).
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_winnability_post_1945__deterrence_unthinkable, tangled_rope).
narrative_ontology:human_readable(war_winnability_post_1945__deterrence_unthinkable, "Nuclear-Age Categorical Unwinnability of Great-Power War").
narrative_ontology:topic_domain(war_winnability_post_1945__deterrence_unthinkable, "strategic_studies/nuclear_deterrence_theory/international_relations").

domain_priors:requires_active_enforcement(war_winnability_post_1945__deterrence_unthinkable).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_winnability_post_1945__deterrence_unthinkable, 'dd3660c0-fab7-44be-bb78-1d96518e191a').
narrative_ontology:cs_kernel_codification('dd3660c0-fab7-44be-bb78-1d96518e191a', distributed).
narrative_ontology:cs_authority_grounding('dd3660c0-fab7-44be-bb78-1d96518e191a', expertise).
narrative_ontology:cs_interpretation_layer_present('dd3660c0-fab7-44be-bb78-1d96518e191a').
narrative_ontology:cs_reading_relation('dd3660c0-fab7-44be-bb78-1d96518e191a', war_winnability_post_1945__countervailing_thinkable, coexists_with).
narrative_ontology:cs_reading_relation('dd3660c0-fab7-44be-bb78-1d96518e191a', war_winnability_post_1945__rhetorical_contraction, influences).
narrative_ontology:cs_axiom('dd3660c0-fab7-44be-bb78-1d96518e191a', foundational, total_war_categorically_excludes_victory_planning).
narrative_ontology:cs_axiom_status(total_war_categorically_excludes_victory_planning, holdable).
narrative_ontology:cs_axiom_grounding('dd3660c0-fab7-44be-bb78-1d96518e191a', total_war_categorically_excludes_victory_planning, empirically_contingent).
narrative_ontology:cs_axiom('dd3660c0-fab7-44be-bb78-1d96518e191a', secondary, strategic_planning_object_is_prevention_not_victory).
narrative_ontology:cs_axiom_status(strategic_planning_object_is_prevention_not_victory, holdable).
narrative_ontology:cs_axiom_grounding('dd3660c0-fab7-44be-bb78-1d96518e191a', strategic_planning_object_is_prevention_not_victory, instrumental).
narrative_ontology:cs_reference_frame('dd3660c0-fab7-44be-bb78-1d96518e191a', assured_destruction_makes_general_war_irrational).
narrative_ontology:cs_drift_state('dd3660c0-fab7-44be-bb78-1d96518e191a', post_cold_war_multipolar_nuclear_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('dd3660c0-fab7-44be-bb78-1d96518e191a', '').
narrative_ontology:cs_kernel_id(war_winnability_post_1945__deterrence_unthinkable, war_winnability_post_1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__deterrence_unthinkable, civilian_populations).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__deterrence_unthinkable, arms_control_diplomats).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__deterrence_unthinkable, deterrence_theorists).
narrative_ontology:constraint_victim(war_winnability_post_1945__deterrence_unthinkable, military_establishments).
narrative_ontology:constraint_victim(war_winnability_post_1945__deterrence_unthinkable, war_fighting_doctrine_communities).
narrative_ontology:constraint_victim(war_winnability_post_1945__deterrence_unthinkable, conventional_force_planners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear no formal role in strategic planning but are the referent of the entire arrangement: their survival is the thing the unwinnability claim protects. They cannot exit the nuclear order — there is no jurisdiction outside its shadow — but they benefit categorically from any doctrine that forecloses total war as a rational instrument.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, civilian_populations, beneficiary,
    powerless, civilizational, trapped, global).

% Exist institutionally to plan for and win wars; the categorical-unwinnability reading strips the great-power war-fighting mission of coherence, forcing a pivot to deterrence maintenance, arms control verification, and crisis management — functions that do not fit the martial identity the institution was built around. Cannot simply exit their founding mission without dissolving their reason for being.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, military_establishments, payer,
    institutional, generational, identity_locked, national).

% Career strategists, war college faculty, and doctrine writers whose professional output — victory conditions, war termination criteria, escalation-to-advantage theories — becomes structurally incoherent once total war between nuclear powers is defined as categorically unwinnable. Their career capital is sunk in a framework this reading declares obsolete.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, war_fighting_doctrine_communities, payer,
    organized, biographical, constrained, national).

% Plan force structure and procurement premised on scenarios of decisive conventional-to-nuclear escalation dynamics; the unwinnability reading undercuts the rationale for large standing forces oriented toward winning a general war, pressuring budgets and force posture toward minimal deterrence instead.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, conventional_force_planners, payer,
    organized, biographical, constrained, national).

% Negotiate treaties (SALT, START, INF-era instruments) whose entire legitimacy rests on the premise that total war is unwinnable and therefore arsenal reduction is rational rather than self-defeating. Their institutional relevance and career trajectories are built on this reading holding; they actively promote and enforce it in international fora.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, arms_control_diplomats, beneficiary,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(war_winnability_post_1945__deterrence_unthinkable, arms_control_diplomats, agenda_setter).

% Academic and think-tank strategists (in the tradition of Schelling, Jervis, Brodie) whose intellectual framework depends on total war being categorically unwinnable — this is the load-bearing premise of mutual assured destruction as stabilizing rather than merely constraining. They set the conceptual agenda that filters into doctrine and public discourse.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, deterrence_theorists, beneficiary,
    analytical, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(war_winnability_post_1945__deterrence_unthinkable, deterrence_theorists, agenda_setter).

% Hold that limited nuclear options and counterforce targeting preserve a meaningful winnability space; this reading treats their position as the sibling 'countervailing_thinkable' claim and excludes it from this constraint's own operative logic — they are heard in policy debate but not admitted into this reading's premises.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, counterforce_strategists, excluded,
    organized, biographical, constrained, national).

% Examine declassified war plans (SIOP, targeting doctrine) and public rhetoric to assess whether the categorical-unwinnability claim was ever operationally true or was instead a discursive taboo layered over continued war-fighting planning — this is precisely the empirical fault line between this reading and 'rhetorical_contraction.'
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__deterrence_unthinkable, historians_of_strategy, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(war_winnability_post_1945__deterrence_unthinkable, diffuse).
narrative_ontology:fixing_cost_class(war_winnability_post_1945__deterrence_unthinkable, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared, mutually legible ceiling on what great-power conflict can rationally aim to achieve, allowing adversaries to coordinate on crisis stability, arms limitation, and escalation avoidance without either side needing to trust the other's intentions — only the physics and logic of assured destruction.
% TRANSFER_FUNCTION: Moves institutional relevance, budget authority, and professional legitimacy away from war-fighting and victory-planning military communities toward deterrence-maintenance, verification, and crisis-management communities; moves existential risk exposure away from being a function of strategic choice and toward being a structural constant borne diffusely by all civilian populations.
% ABSENT_VOICES: Counterforce and limited-nuclear-option strategists are marginalized within this reading's own logic — they are treated as holding the sibling 'countervailing_thinkable' position rather than as internal dissent to be reconciled. Populations in non-nuclear states bear the systemic tail risk of great-power miscalculation but have no seat in either doctrine formation or arms control negotiation.
% DISAPPEARANCE_RATIONALE: If the categorical-unwinnability premise were abandoned and great-power total war were reconstituted as a coherently winnable enterprise, war colleges would resume victory-condition planning, force structures would re-orient toward decisive war-fighting capability, arms control architecture would lose its foundational rationale and likely collapse, and crisis stability doctrines built on mutual vulnerability would be replaced by first-strike or damage-limitation postures — a substantial reorganization of military institutions, budgets, and international law.
% FOUNDING_PROBLEM: The advent of thermonuclear weapons and assured second-strike capability made it physically true that no plausible general war between nuclear-armed great powers could produce a coherent 'winning' outcome for either side, given the certainty of civilizational-scale destruction on both sides regardless of who struck first or 'won' the exchange.
% FOUNDING_PROBLEM_CORROBORATION: Physicists and systems analysts of the early nuclear era (e.g., the RAND Corporation's own targeting studies) independently corroborated the destructive-capacity logic underlying this reading from outside the arms-control-advocacy community. However, declassified war-planning documents (SIOP revisions, presidential nuclear guidance through the Cold War and after) show military planners continuing to develop damage-limitation and counterforce options consistent with a residual winnability logic — corroboration from outside the deterrence-theory community is mixed, which is part of why the kernel itself remains contested across the three sibling readings.
narrative_ontology:disappearance_verdict(war_winnability_post_1945__deterrence_unthinkable, world_rearranges).
narrative_ontology:founding_problem_status(war_winnability_post_1945__deterrence_unthinkable, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_winnability_post_1945__deterrence_unthinkable, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(war_winnability_post_1945__deterrence_unthinkable, 'none', 1).
narrative_ontology:epsilon_provenance(war_winnability_post_1945__deterrence_unthinkable, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_winnability_post_1945__deterrence_unthinkable_tests).
:- end_tests(war_winnability_post_1945__deterrence_unthinkable_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) rather than low, reflecting that this reading is not neutral natural law — it reallocates institutional legitimacy and resources away from war-fighting communities toward deterrence-maintenance communities, and that reallocation has real costs for the losing side even though it also plausibly reduces existential risk for everyone. Suppression is moderate-high (0.55): the reading is actively defended against counterforce/war-fighting doctrine through career gatekeeping in war colleges, treaty verification regimes, and normative taboo enforcement (e.g., no-first-use debates, negative security assurances). Theater ratio (0.4) reflects that a meaningful share of deterrence posturing (parity debates, modernization programs justified in war-fighting language) may be performative relative to the underlying prevention function. Accessibility collapse is high (0.8) because once the physics of assured destruction is understood, the alternative framing (total war as winnable) becomes very difficult to hold credibly at the level of first principles — though resistance remains substantial (0.6) from military and doctrinal communities whose institutional survival depends on contesting it.
 *
 * PERSPECTIVAL GAP:
 *   From the deterrence theorist and arms control diplomat seats, this reading is close to genuine coordination: a hard-won, physically grounded insight that stabilizes an otherwise catastrophic rivalry. From the military establishment and war-fighting doctrine seats, the same reading operates as an imposed constraint that hollows out their core mission and forces adaptation to a role (deterrence administration) that was not what the institution was built for. The engine should compute these as structurally different experiences of one constraint, not as a disagreement to be averaged.
 *
 * DIRECTIONALITY LOGIC:
 *   Civilian populations are coded as the primary beneficiary despite having no formal power — the constraint's entire justification is protecting them from a total-war outcome, even though they cannot exercise agency over whether the reading holds. Arms control diplomats and deterrence theorists are institutional/analytical beneficiaries with real agenda-setting power: they actively construct and defend the reading. Military establishments, doctrine communities, and conventional force planners are victims in the specific sense that their founding professional purpose is rendered incoherent by the reading's success, even though none of them face literal extraction of resources to a rival party — the 'payment' is mission coherence and institutional identity, not money.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (the physical fact of assured mutual destruction making general nuclear war strategically incoherent) plausibly remains live — nuclear arsenals capable of civilization-ending exchange still exist. This differentiates the constraint from a pure mandatrophy case where the founding problem has vanished but the arrangement persists. However, the founding_problem_status is marked 'contested' precisely because declassified planning documents complicate a clean 'problem still fully live, arrangement fully justified' story — some of what looks like continued deterrence-maintenance may in fact be atrophied war-fighting apparatus persisting on inertia (pointing toward the piton-flavored 'rhetorical_contraction' sibling), which is why this reading is authored as tangled_rope with real ongoing enforcement rather than assumed settled.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_deterrence_unthinkable,
    'Is ''war winnability post-1945'' best read as (a) categorically unwinnable and operationally removed from the planning space (this reading), (b) constrained but still achievable through counterforce/limited-war doctrine (countervailing_thinkable), or (c) rhetorically taboo while remaining operationally planned underneath the public doctrine (rhetorical_contraction)?',
    'Systematic comparison of declassified war plans (SIOP and successors) against public doctrinal statements across decades: if operational planning consistently abandoned victory-condition targeting in favor of pure retaliation/deterrence postures, this reading is corroborated; if damage-limitation and counterforce planning persisted substantially unchanged beneath declaratory taboo, the rhetorical_contraction reading is corroborated instead.',
    'If the rhetorical_contraction reading is correct, this constraint''s claimed_type should be understood as closer to a piton (a form of words maintained theatrically over unchanged war-fighting substance) rather than a tangled_rope with genuine operational contraction. The beneficiary/victim structure would also shift: military establishments would not actually be victims of mission incoherence, since the mission would have persisted covertly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_deterrence_unthinkable, conceptual, 'Which of the three kernel readings best matches the actual historical operational record, as opposed to declaratory doctrine.').

omega_variable(
    military_establishment_genuine_victimhood,
    'Do military establishments genuinely bear a cost from this reading (mission incoherence, institutional identity disruption), or do they substantially adapt and capture new institutional roles (deterrence administration, arms control verification, missile defense) that preserve their resource base and relevance?',
    'Track military budget shares, personnel allocation, and institutional prestige metrics across the interval to see whether ''victim'' communities actually lost resources/relevance or merely changed their justificatory language while resource flows continued.',
    'If military establishments substantially captured new roles without net institutional loss, the victim declaration should be weakened or reclassified as a lateral role shift rather than genuine extraction — this would push the constraint''s classification toward rope rather than tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(military_establishment_genuine_victimhood, empirical, 'Whether the claimed institutional victim group experienced real costs or successfully adapted without net loss.').

omega_variable(
    cs_framing_underdetermination_kernel_vs_doctrine,
    'Should the commitment-system kernel here be framed as the physical fact of mutual assured destruction itself (a near-mountain, minimal interpretive layer) or as the doctrinal tradition of deterrence theory that interprets that fact (a fixed_text/lineage-style commitment system with substantial interpretive latitude, which is the framing adopted in this story''s cs_structure)? The two framings could produce different cs_pattern classifications — the physics framing tends toward Mountain-like immunity, while the doctrinal framing (adopted here) exposes the reading to contest, drift, and axiom_overriding.',
    'Examine whether disputes among the three sibling readings turn on disagreement about the underlying physics (they do not — all three readings accept mutual assured destruction as physically real) or about how to interpret its strategic implications (they do — this is exactly what distinguishes the three readings). This favors the doctrinal/interpretive framing.',
    'Adopting the physics framing would make this constraint closer to a Mountain with contested beneficiaries (FSM candidate); adopting the doctrinal framing (as this story does) makes it a tangled_rope kernel reading with genuine axiom contest among the three siblings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cs_framing_underdetermination_kernel_vs_doctrine, conceptual, 'Alternative framing of the kernel as raw physics versus as doctrinal interpretation, and why the doctrinal framing was selected for cs_structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_winnability_post_1945__deterrence_unthinkable, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t1945, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 1945, 0.15).
narrative_ontology:measurement(war__tr_t1962, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 1962, 0.25).
narrative_ontology:measurement(war__tr_t1979, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 1979, 0.35).
narrative_ontology:measurement(war__tr_t1991, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 1991, 0.5).
narrative_ontology:measurement(war__tr_t2008, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 2008, 0.42).
narrative_ontology:measurement(war__tr_t2025, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(war__be_t1945, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 1945, 0.2).
narrative_ontology:measurement(war__be_t1962, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 1962, 0.3).
narrative_ontology:measurement(war__be_t1979, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 1979, 0.38).
narrative_ontology:measurement(war__be_t1991, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 1991, 0.45).
narrative_ontology:measurement(war__be_t2008, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 2008, 0.4).
narrative_ontology:measurement(war__be_t2025, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t1945, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 1945, 0.3).
narrative_ontology:measurement(war__su_t1962, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 1962, 0.45).
narrative_ontology:measurement(war__su_t1979, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 1979, 0.55).
narrative_ontology:measurement(war__su_t1991, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 1991, 0.5).
narrative_ontology:measurement(war__su_t2008, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 2008, 0.48).
narrative_ontology:measurement(war__su_t2025, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 2025, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_winnability_post_1945__deterrence_unthinkable, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(war_winnability_post_1945__deterrence_unthinkable, 0.12).
narrative_ontology:affects_constraint(war_winnability_post_1945__deterrence_unthinkable, war_winnability_post_1945__countervailing_thinkable).
narrative_ontology:affects_constraint(war_winnability_post_1945__deterrence_unthinkable, war_winnability_post_1945__rhetorical_contraction).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the 'war_winnability_post_1945' kernel, each authored as a separate constraint story per the ε-invariance principle. This reading (deterrence_unthinkable) holds that great-power total war exited the reachable strategic-planning space entirely after 1945, with civilian populations as beneficiary and military establishments as victim of mission incoherence. The 'countervailing_thinkable' sibling holds limited victory remains achievable through counterforce targeting — a materially different beneficiary/victim structure (counterforce strategists benefit, arms control advocates are marginalized) and a lower suppression profile since it does not need to enforce a taboo against war-fighting planning. The 'rhetorical_contraction' sibling holds the unwinnability claim is discursive cover over continued operational war-fighting planning — a piton-flavored reading where the theater_ratio would be authored much higher and the 'genuine contraction' claimed here would be treated as false. All three share the same underlying kernel (does great-power total war remain winnable post-1945) but diverge in what they claim actually happened structurally, producing different ε values and different classifications; they must not be merged into one story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(war_winnability_post_1945__deterrence_unthinkable, institutional, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
