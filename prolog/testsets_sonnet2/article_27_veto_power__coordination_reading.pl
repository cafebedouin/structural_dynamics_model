% ============================================================================
% CONSTRAINT STORY: article_27_veto_power__coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_27_veto_power__coordination_reading, []).

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
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: article_27_veto_power__coordination_reading
 *   human_readable: UN Charter Article 27(3) P5 Veto — Great-Power War Avoidance Reading
 *   domain: international_relations/institutional_design
 *
 * SUMMARY:
 *   This story instantiates the coordination reading of the Article 27(3)
 *   veto kernel: the veto is understood as the structural device that
 *   prevents the Security Council from ever legally authorizing force that
 *   would compel a nuclear-armed permanent member into a war it has not
 *   chosen. On this reading, the coordination problem is genuine and severe —
 *   the alternative institutional design (simple or qualified majority rule
 *   extending to enforcement against great powers) is not a viable
 *   alternative but a recipe for the kind of great-power confrontation the UN
 *   was built to prevent. Extraction is authored low because, under this
 *   reading's own lights, no party captures rents from the arrangement that
 *   others are structurally denied; the benefit (avoided catastrophic war)
 *   accrues to the entire membership, including non-P5 states, even though
 *   only P5 states hold the instrument.
 *
 * KEY AGENTS:
 *   - p5_states: agenda-setters and beneficiaries who administer and rely on the veto
 *   - non_p5_member_states: beneficiaries of the war-avoidance function without possessing the instrument
 *   - international_system_stability: the non-agent collective good the reading names as primary beneficiary
 *   - un_secretariat: administrative observer with no power over the kernel
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_27_veto_power__coordination_reading, 0.18).
domain_priors:suppression_score(article_27_veto_power__coordination_reading, 0.35).
domain_priors:theater_ratio(article_27_veto_power__coordination_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_27_veto_power__coordination_reading, rope).
narrative_ontology:human_readable(article_27_veto_power__coordination_reading, "UN Charter Article 27(3) P5 Veto — Great-Power War Avoidance Reading").
narrative_ontology:topic_domain(article_27_veto_power__coordination_reading, "international_relations/institutional_design").

domain_priors:requires_active_enforcement(article_27_veto_power__coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_27_veto_power__coordination_reading, 'ad956921-0551-46fe-bdd2-51e56b98c3fa').
narrative_ontology:cs_kernel_codification('ad956921-0551-46fe-bdd2-51e56b98c3fa', fixed_text).
narrative_ontology:cs_authority_grounding('ad956921-0551-46fe-bdd2-51e56b98c3fa', lineage).
narrative_ontology:cs_interpretation_layer_present('ad956921-0551-46fe-bdd2-51e56b98c3fa').
narrative_ontology:cs_reading_relation('ad956921-0551-46fe-bdd2-51e56b98c3fa', article_27_veto_power__oligopoly_reading, coexists_with).
narrative_ontology:cs_reading_relation('ad956921-0551-46fe-bdd2-51e56b98c3fa', article_27_veto_power__sovereignty_reading, influences).
narrative_ontology:cs_axiom('ad956921-0551-46fe-bdd2-51e56b98c3fa', foundational, unanimity_among_nuclear_powers_prevents_forced_confrontation).
narrative_ontology:cs_axiom_status(unanimity_among_nuclear_powers_prevents_forced_confrontation, holdable).
narrative_ontology:cs_axiom_grounding('ad956921-0551-46fe-bdd2-51e56b98c3fa', unanimity_among_nuclear_powers_prevents_forced_confrontation, empirically_contingent).
narrative_ontology:cs_axiom('ad956921-0551-46fe-bdd2-51e56b98c3fa', secondary, avoided_catastrophic_war_is_a_universal_benefit_not_a_captured_rent).
narrative_ontology:cs_axiom_status(avoided_catastrophic_war_is_a_universal_benefit_not_a_captured_rent, holdable).
narrative_ontology:cs_axiom_grounding('ad956921-0551-46fe-bdd2-51e56b98c3fa', avoided_catastrophic_war_is_a_universal_benefit_not_a_captured_rent, instrumental).
narrative_ontology:cs_reference_frame('ad956921-0551-46fe-bdd2-51e56b98c3fa', postwar_great_power_unanimity_settlement).
narrative_ontology:cs_drift_state('ad956921-0551-46fe-bdd2-51e56b98c3fa', post_cold_war_multipolarity, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ad956921-0551-46fe-bdd2-51e56b98c3fa', '').
narrative_ontology:cs_kernel_id(article_27_veto_power__coordination_reading, article_27_veto_power).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_27_veto_power__coordination_reading, international_system_stability).
narrative_ontology:constraint_beneficiary(article_27_veto_power__coordination_reading, non_p5_member_states).
narrative_ontology:constraint_beneficiary(article_27_veto_power__coordination_reading, p5_states).
narrative_ontology:constraint_vindicates(article_27_veto_power__coordination_reading, great_power_unanimity_prevents_bloc_war).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Each of the five permanent members can block any Security Council resolution it judges would compel it into military confrontation it rejects. They administer the veto and also rely on its mutual restraint function: each accepts the others' veto power as the price of retaining its own, since none can be forced into war by majority vote of the Council.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, p5_states, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(article_27_veto_power__coordination_reading, p5_states, beneficiary).

% Non-permanent and non-member states cannot themselves wield the veto, but benefit from the mechanism's core function: the Council cannot authorize collective action that would trigger a direct war between nuclear-armed great powers, which would be catastrophic for all states regardless of their formal standing at the table. Their exit from the arrangement is limited to voice within the General Assembly and diplomatic pressure, not exit from the risk the veto manages.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, non_p5_member_states, beneficiary,
    moderate, generational, constrained, global).

% Not an actor but the collective good the mechanism is read as producing: the absence of a structural pathway by which a Council majority could legally authorize force against a nuclear great power, which under this reading is the single most important war-avoidance feature of the postwar order.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, international_system_stability, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(article_27_veto_power__coordination_reading, international_system_stability).

% Administers Council procedure and documents veto usage. Has no power to alter Article 27 and no stake in specific vetoes, but observes the pattern of use and non-use over decades.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, un_secretariat, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_27_veto_power__coordination_reading, diffuse).
narrative_ontology:fixing_cost_class(article_27_veto_power__coordination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of authorizing multilateral force: without a unanimity requirement among the states capable of waging and surviving great-power war, a Council majority could vote to authorize action against a nuclear power, which that power would either resist militarily or be forced to accept under coercion — either outcome risking catastrophic escalation. The veto removes that pathway entirely.
% TRANSFER_FUNCTION: Moves nothing between parties in the ordinary sense; it withholds a capability (Council-authorized coercive force against a P5 state) that would otherwise exist, converting a majority-rule institution into a unanimity-among-great-powers institution for the highest-stakes decisions.
% ABSENT_VOICES: States that have been on the receiving end of Council paralysis — parties to conflicts where a P5 veto blocked intervention or condemnation — would object that the mechanism they experience is not war-avoidance but abandonment. Under this reading, however, those objections are treated as evidence of the mechanism functioning as designed (no forced great-power confrontation), not as evidence against the coordination account; the sibling readings carry that contest.
% DISAPPEARANCE_RATIONALE: Under this reading, removing the veto would restore majority-rule authorization power to the Council, meaning a coalition could in principle vote to authorize force against a nuclear P5 state — a scenario this reading holds would sharply raise the risk of great-power war being triggered by Council action rather than avoided by it. Whether the world 'rearranges' catastrophically or simply reverts to a more contestable but survivable order is exactly what separates this reading from the oligopoly reading, hence 'contested' rather than a clean verdict.
% FOUNDING_PROBLEM: The League of Nations collapsed partly because it lacked any mechanism preventing majority coalitions from taking positions that great powers would refuse to accept, contributing to great-power withdrawal and the collapse of collective security before WWII. The UN Charter's drafters built the veto specifically so no great power could be structurally cornered into choosing between submission and war.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the San Francisco Conference and the League's collapse (an academic literature substantially outside any P5 government) corroborate that the founding intent was war-avoidance among the wartime allies, not merely privilege preservation. Whether that problem remains live in a multipolar, non-bipolar security environment is disputed by scholars and by non-P5 states themselves, who form no part of the veto's beneficiary set under the oligopoly reading but are named as beneficiaries here.
narrative_ontology:disappearance_verdict(article_27_veto_power__coordination_reading, contested).
narrative_ontology:founding_problem_status(article_27_veto_power__coordination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_27_veto_power__coordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article_27_veto_power__coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_27_veto_power__coordination_reading, 0.18, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_27_veto_power__coordination_reading_tests).
:- end_tests(article_27_veto_power__coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.18) and roughly flat across the interval because, under this reading, the veto's function — blocking coercive authorization against nuclear great powers — has not degraded; its use pattern (mostly by the same five states, mostly on matters touching their core security interests) is read as consistent operation of the original coordination function rather than accumulating rent-seeking. Suppression is moderate (0.35): the mechanism does suppress the alternative of majority-rule enforcement against P5 states, but this reading holds that suppression is the coordination mechanism itself, not a symptom of capture. Theater ratio stays low and only slightly rising, reflecting that veto use remains functionally tied to genuine security disputes rather than becoming primarily performative, under this reading's own accounting.
 *
 * PERSPECTIVAL GAP:
 *   A state that has watched a Council resolution vetoed on a matter it experiences as urgent will not see coordination; it will see paralysis. This reading does not deny that experience — it reinterprets it as the visible cost of a mechanism whose benefit (no forced great-power war) is mostly invisible because it consists of wars that did not happen. The engine will compute per-seat types from the structural data; this reading's stakeholder set is authored so that even the constrained non-P5 seat carries beneficiary role, which is a substantive and contestable claim, not a neutral default.
 *
 * DIRECTIONALITY LOGIC:
 *   P5 states are declared beneficiaries because they hold and rely on the instrument, but their d is not pushed to the full-beneficiary extreme because the mechanism binds them symmetrically to each other's restraint — a P5 state that wants freedom of action elsewhere pays for that by accepting others' vetoes too. Non-P5 states are declared beneficiaries under this reading despite having no access to the instrument, because the reading's core claim is that the avoided-war benefit is universal and does not require possession of the veto to receive.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (League-era collapse from lack of a great-power-restraint mechanism) is authored as contested rather than dead, because under this reading multipolar nuclear proliferation since 1945 has, if anything, generalized rather than resolved the underlying risk the veto addresses. This blocks a premature mandatrophy verdict: a mechanism whose founding problem (avoiding a structural path to great-power war) remains live cannot be waved off as pure inertia, even though its distributional pattern looks static across 80 years.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_entrenchment_boundary,
    'Is the veto''s stabilizing function separable from its entrenchment function, or does the same mechanism necessarily produce both — meaning the coordination reading and the oligopoly reading describe the same structure at different levels of the same fact rather than genuinely competing accounts?',
    'Comparative institutional analysis: examine whether alternative unanimity-among-nuclear-powers designs (e.g., rotating or criteria-based rather than fixed P5 membership) could preserve war-avoidance while reducing entrenchment — if such designs are feasible and were never seriously considered at San Francisco or since, that evidence favors the oligopoly reading''s claim that entrenchment, not war-avoidance, is doing the explanatory work.',
    'If the two functions are inseparable, the coordination reading''s low ε is defensible as the honest measure of a necessary cost. If separable, the coordination reading understates ε by attributing to war-avoidance what is actually static privilege-protection.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_vs_entrenchment_boundary, conceptual, 'Whether the veto''s coordination and entrenchment functions are structurally the same mechanism or separable ones.').

omega_variable(
    counterfactual_war_avoidance_evidence,
    'How much confidence can be placed in the claim that the veto has actually prevented specific great-power wars, versus other factors (nuclear deterrence itself, economic interdependence, bipolar/multipolar balance) doing the actual preventive work with the veto as an epiphenomenal formality?',
    'Historical case analysis of Cold War and post-Cold War crises where Council action was blocked by veto and a plausible alternative-history pathway to great-power war can be reconstructed and compared against cases where the veto was absent or bypassed (e.g., Korea 1950, Kosovo 1999) without resulting great-power war.',
    'If Korea and Kosovo show that Council paralysis or bypass does not in fact trigger great-power war, the coordination reading''s central causal claim weakens substantially, pushing ε upward toward the oligopoly reading''s account.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_war_avoidance_evidence, empirical, 'Whether the veto is doing genuine causal work in war-avoidance or is a formality alongside deterrence and other stabilizers.').

omega_variable(
    reading_selection_under_determination,
    'Is the coordination framing (war-avoidance as the primary lens) the natural default reading of Article 27, or was it selected here because it produces the cleanest low-ε, no-victim story, while the oligopoly and sovereignty framings are equally textually and historically defensible?',
    'Compare the drafting record''s stated justifications (San Francisco Conference debates, US/UK/USSR negotiating positions) against each reading''s core premise; note which justification was foregrounded by the P5 themselves versus by smaller states at the time.',
    'If the drafting record shows the P5''s own contemporaneous justification leaned more heavily on retaining great-power prerogative than on collective war-avoidance, that would suggest this reading, while coherent, is not privileged over the oligopoly reading and the two should be read as equally live rather than one being more ''basic.''',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_selection_under_determination, conceptual, 'Whether the coordination framing is the natural reading of the veto or one of several equally defensible framings selected for this story.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_27_veto_power__coordination_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t1945, article_27_veto_power__coordination_reading, theater_ratio, 1945, 0.05).
narrative_ontology:measurement(arti_tr_t1961, article_27_veto_power__coordination_reading, theater_ratio, 1961, 0.08).
narrative_ontology:measurement(arti_tr_t1977, article_27_veto_power__coordination_reading, theater_ratio, 1977, 0.1).
narrative_ontology:measurement(arti_tr_t1993, article_27_veto_power__coordination_reading, theater_ratio, 1993, 0.1).
narrative_ontology:measurement(arti_tr_t2009, article_27_veto_power__coordination_reading, theater_ratio, 2009, 0.13).
narrative_ontology:measurement(arti_tr_t2025, article_27_veto_power__coordination_reading, theater_ratio, 2025, 0.15).

% Extraction over time
narrative_ontology:measurement(arti_be_t1945, article_27_veto_power__coordination_reading, base_extractiveness, 1945, 0.1).
narrative_ontology:measurement(arti_be_t1961, article_27_veto_power__coordination_reading, base_extractiveness, 1961, 0.14).
narrative_ontology:measurement(arti_be_t1977, article_27_veto_power__coordination_reading, base_extractiveness, 1977, 0.15).
narrative_ontology:measurement(arti_be_t1993, article_27_veto_power__coordination_reading, base_extractiveness, 1993, 0.13).
narrative_ontology:measurement(arti_be_t2009, article_27_veto_power__coordination_reading, base_extractiveness, 2009, 0.16).
narrative_ontology:measurement(arti_be_t2025, article_27_veto_power__coordination_reading, base_extractiveness, 2025, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(article_27_veto_power__coordination_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(article_27_veto_power__coordination_reading, article_27_veto_power__oligopoly_reading).
narrative_ontology:affects_constraint(article_27_veto_power__coordination_reading, article_27_veto_power__sovereignty_reading).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the article_27_veto_power kernel. The coordination_reading (this file) authors ε low (0.18) and names international_system_stability and non_p5_member_states as beneficiaries with no victim class. The oligopoly_reading authors the same standing arrangement as high-ε structural rent extraction with non-P5 states and would-be reformist coalitions as victims. The sovereignty_reading authors the veto as a consent-based Westphalian entitlement, a distinct normative ground from either war-avoidance or rent-extraction. All three share the same kernel text (Article 27(3)) and the same historical practice but diverge sharply on ε, beneficiary/victim structure, and claimed type — per the ε-invariance principle these are three constraints, not one constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
