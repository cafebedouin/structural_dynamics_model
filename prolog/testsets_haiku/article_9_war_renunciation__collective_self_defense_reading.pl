% ============================================================================
% CONSTRAINT STORY: article_9_war_renunciation__collective_self_defense_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_9_war_renunciation__collective_self_defense_reading, []).

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
 *   constraint_id: article_9_war_renunciation__collective_self_defense_reading
 *   human_readable: Article 9 Collective Self-Defense Interpretation (Survival-Threatened Trigger)
 *   domain: constitutional_law/security_policy/institutional_legitimacy
 *
 * SUMMARY:
 *   This constraint is the COLLECTIVE SELF-DEFENSE READING of Article 9,
 *   Japan's constitutional war renunciation. The reading reinterprets
 *   'inherent right to self-defense' to include collective defense of allies
 *   when Japan's 'survival' is threatened—a doctrine expanded in practice to
 *   encompass overseas deployments and joint operations without direct attack
 *   on Japan. This reading coexists with two sibling readings: the STRICT
 *   PACIFIST READING (which treats 'never be maintained' as absolute
 *   prohibition on any armed forces) and the NARROWER INHERENT-RIGHT READING
 *   (which preserves minimum direct-defense capacity but rejects collective
 *   action). The constraint story presented here is ONE READING ONLY—a clean,
 *   ε-invariant account of the collective self-defense interpretation's
 *   structure, not a synthesis or hedge across readings. The
 *   'survival-threatened' trigger is the interpretive hinge: it transforms a
 *   textual constraint into an elastic doctrine that absorbs operational
 *   expansion without formal amendment. The victim set includes both the
 *   strict pacifist reading (displaced from the interpretation conversation)
 *   and the narrower inherent-right school (whose language is absorbed and
 *   weaponized to justify what they oppose). Okinawan base-host communities
 *   bear concrete costs—expanded military presence, environmental
 *   impact—without a seat at the interpretation table.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_9_war_renunciation__collective_self_defense_reading, 0.68).
domain_priors:suppression_score(article_9_war_renunciation__collective_self_defense_reading, 0.72).
domain_priors:theater_ratio(article_9_war_renunciation__collective_self_defense_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(article_9_war_renunciation__collective_self_defense_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_9_war_renunciation__collective_self_defense_reading, tangled_rope).
narrative_ontology:human_readable(article_9_war_renunciation__collective_self_defense_reading, "Article 9 Collective Self-Defense Interpretation (Survival-Threatened Trigger)").
narrative_ontology:topic_domain(article_9_war_renunciation__collective_self_defense_reading, "constitutional_law/security_policy/institutional_legitimacy").

domain_priors:requires_active_enforcement(article_9_war_renunciation__collective_self_defense_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_9_war_renunciation__collective_self_defense_reading, '94b020fa-76f6-45cc-b136-cee9882e5a38').
narrative_ontology:cs_kernel_codification('94b020fa-76f6-45cc-b136-cee9882e5a38', fixed_text).
narrative_ontology:cs_authority_grounding('94b020fa-76f6-45cc-b136-cee9882e5a38', extraction).
narrative_ontology:cs_interpretation_layer_present('94b020fa-76f6-45cc-b136-cee9882e5a38').
narrative_ontology:cs_reading_relation('94b020fa-76f6-45cc-b136-cee9882e5a38', article_9_war_renunciation__strict_pacifist_reading, forecloses).
narrative_ontology:cs_reading_relation('94b020fa-76f6-45cc-b136-cee9882e5a38', article_9_war_renunciation__inherent_right_reading, influences).
narrative_ontology:cs_axiom('94b020fa-76f6-45cc-b136-cee9882e5a38', foundational, collective_self_defense_constitutionally_permissible).
narrative_ontology:cs_axiom_status(collective_self_defense_constitutionally_permissible, holdable).
narrative_ontology:cs_axiom_grounding('94b020fa-76f6-45cc-b136-cee9882e5a38', collective_self_defense_constitutionally_permissible, deontological).
narrative_ontology:cs_axiom('94b020fa-76f6-45cc-b136-cee9882e5a38', foundational, survival_threat_overrides_textual_constraint).
narrative_ontology:cs_axiom_status(survival_threat_overrides_textual_constraint, holdable).
narrative_ontology:cs_axiom_grounding('94b020fa-76f6-45cc-b136-cee9882e5a38', survival_threat_overrides_textual_constraint, instrumental).
narrative_ontology:cs_reference_frame('94b020fa-76f6-45cc-b136-cee9882e5a38', article_9_textual_renunciation).
narrative_ontology:cs_drift_state('94b020fa-76f6-45cc-b136-cee9882e5a38', contemporary_2024, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('94b020fa-76f6-45cc-b136-cee9882e5a38', '').
narrative_ontology:cs_kernel_id(article_9_war_renunciation__collective_self_defense_reading, article_9_war_renunciation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__collective_self_defense_reading, us_security_alliance_framework).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__collective_self_defense_reading, japanese_military_institutional_capacity).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__collective_self_defense_reading, regional_deterrence_structure).
narrative_ontology:constraint_victim(article_9_war_renunciation__collective_self_defense_reading, strict_pacifist_constitutional_reading).
narrative_ontology:constraint_victim(article_9_war_renunciation__collective_self_defense_reading, narrower_inherent_right_reading_stability).
narrative_ontology:constraint_victim(article_9_war_renunciation__collective_self_defense_reading, okinawan_base_host_communities).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_9_war_renunciation__collective_self_defense_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(article_9_war_renunciation__collective_self_defense_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_9_war_renunciation__collective_self_defense_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_9_war_renunciation__collective_self_defense_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_9_war_renunciation__collective_self_defense_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68 at 2024) reflects the constraint's asymmetric transfer of authority: the cabinet unilaterally reinterprets a foundational constitutional text without explicit legislative or popular mandate. The trend rises from 0.38 (1972) to 0.68 (2024), tracking the operational expansion of SDF overseas deployments and joint operations under increasingly elastic 'survival-threatened' doctrine—extractiveness accumulates as the interpretation is tested and broadened. Suppression (0.72) is high because the constraint persists despite organized opposition from strict-pacifist movements, opposition political parties, and peace constituencies; the cabinet's interpretation function is insulated from legislative check and judicial review. Theater (0.41, rising from 0.12) reflects increasing gap between the stated security justification (deterrence, alliance credibility) and the actual operational trajectory (permanent overseas bases, expanded military budget, closer integration with U.S. military command)—the functional purpose has shifted from 'minimum self-defense' to 'credible great-power military,' and the rhetoric has not kept pace, creating performative maintenance of the 'defensive' framing. Accessibility collapse (0.62) reflects partial closure: the strict-pacifist reading is functionally removed from the interpretation conversation, and the narrower inherent-right reading's boundaries have eroded, but exit routes for institutional opposition remain: legislative supermajority could pass new amendment, courts could reverse deference doctrine, Diet could refuse funding. Resistance (0.58) is substantial because the strict-pacifist movement, opposition parties, constitutional scholars, and Okinawan communities mount continuous objection; the constraint persists not because opposition is absent but because the opposition has no institutional lever on cabinet interpretation. Time grid is shared: every metric is authored at every examined point (1972, 1992, 2005, 2015, 2020, 2024), allowing temporal analysis of drift.
 *
 * PERSPECTIVAL GAP:
 *   From the cabinet's perspective, this is genuine coordination: solving the institutional problem of credible alliance participation without formal amendment, maintaining both constitutional form and security substance. From the strict-pacifist perspective, this is pure extraction: the cabinet seized interpretive authority it does not constitutionally possess and used it to override a textual commitment the public ratified. From the narrower inherent-right perspective, this is a betrayal: their own interpretive framework (inherent right to self-defense) is weaponized to justify collective action they explicitly rejected. From the Okinawan perspective, this is coercion: their communities bear the operational costs of a military expansion they did not consent to and cannot block. The engine computes each seat's divergent classification from the structural data; my commentary does not adjudicate between perspectives, only maps them.
 *
 * DIRECTIONALITY LOGIC:
 *   The Cabinet Security Apparatus is the structural beneficiary (agenda_setter, controls interpretation, collects authority). The U.S. alliance framework and regional deterrence coalition benefit from expanded Japanese participation without formally revising the Constitution. The strict-pacifist reading and narrower inherent-right school are victims (their constitutional interpretations are displaced from the operative conversation). Okinawan base-host communities are payers (bear concrete costs—expanded presence, environmental impact—without representation in the interpretation process). The narrower inherent-right school's identity-lock is the most precise structural marker: they are professional constitutional scholars, judges, and policy figures whose institutional identity is built on constitutional interpretation; exit from this reading means leaving their profession or accepting professional humiliation. The cabinet's institutional power allows it to claim 'survival-threatened' without external validation; the boundary remains within the cabinet's discretion. Directionality: agenda_setter near full beneficiary (d~0.1), alliance partners near full beneficiary (d~0.15), strict-pacifist movement near full target (d~0.88), narrower inherent-right school near target but with identity lock (d~0.75), Okinawan communities fully trapped (d~0.95), opposition parties constrained (d~0.65).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint shows partial mandatrophy: the founding problem (alliance credibility in the Cold War context) is contested as live vs. solved. The cabinet asserts the problem remains live and 'survival-threatened' scenarios require readiness. Opposition constituencies and narrower-inherent-right scholars assert the problem is solved—the alliance is stable, regional deterrence exists, and further expansion is institutional rent-seeking rather than necessity-driven. The disappearance verdict (world_rearranges) confirms arrangements depend on it, but the founding problem status being 'contested' signals the constraint persists partly through inertia and partly through institutional interest-alignment (cabinet's authority-expansion interest aligns with U.S. alliance interest). Mandatrophy is present but not complete: the constraint is not purely theatrical—it does enable real operations—but the functional justification has shifted from 'solving a genuine security problem' to 'maintaining military institutional capacity within the existing alliance framework.' The rising theater_ratio (0.12 to 0.41) reflects this drift: defensive framing persists while offensive or power-projection capability expands. The constraint is neither pure coordination nor pure mandatrophy, but a tangled_rope that becomes increasingly extractive as the founding-problem justification erodes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    survival_threatened_elasticity,
    'What scenarios count as ''survival-threatened'' under this reading, and who determines the boundary?',
    'Documentary evidence of cabinet policy guidance on survival-threat triggers; legislative or court oversight that would establish public criteria; comparison of stated vs. actual deployment triggers over time.',
    'If ''survival-threatened'' is elastic and determined unilaterally by the cabinet, the constraint enables expansive military commitment without amendment. If the boundary is fixed by legislative agreement or court precedent, the extraction is bounded. This omega directly bears on whether the reading is coordination (fixed scope) or a mechanism for extractive authority-expansion (elastic scope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(survival_threatened_elasticity, empirical, 'Whether ''survival-threatened'' is a fixed criterion or an elastic doctrine manipulated by the cabinet.').

omega_variable(
    narrower_inherent_right_displacement,
    'Is the narrower inherent-right reading genuinely foreclosed (logically incompatible with this reading), or merely marginalized (politically displaced but logically coherent)?',
    'Constitutional textual analysis: can both readings hold within a single coherent framework, or do they logically contradict? Legislative debate if constitutional amendment were pursued: would amendment clarify boundaries between direct self-defense and collective action, or does current text permit both readings simultaneously?',
    'If foreclosed, the narrower reading is unavailable as a live position, and the constraint legitimately overrides it. If marginalized, the narrower reading remains a live constitutional option that the cabinet suppresses through interpretive practice, making the victims'' objection a constitutional claim, not merely preference-based opposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(narrower_inherent_right_displacement, conceptual, 'Whether the narrower inherent-right reading is logically foreclosed or politically marginalized.').

omega_variable(
    collective_self_defense_vs_offensive_capacity,
    'Is Japan''s expanded military capacity structurally limited to collective self-defense (defensive in purpose), or does the reading enable independent offensive projection?',
    'Military capability analysis: SDF doctrine, weapons procurement, deployment posture, and operational planning. Assessment of whether Japan could sustain independent operations without U.S. alliance support, and whether operational planning assumes joint command or independent action.',
    'If limited to collective defense, the constraint is genuine coordination within the alliance. If enabling independent offensive capacity, the reading masks power-projection expansion and the extraction is directed toward Japanese institutional military growth, not alliance coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_self_defense_vs_offensive_capacity, empirical, 'Whether the reading''s operational scope is genuinely limited to collective defense or extends to independent projection.').

omega_variable(
    okinawan_suppression_mechanism,
    'Is Okinawan opposition suppressed structurally (legal barriers to exit, economic dependency) or internalized (communities have accepted the military presence as inevitable)?',
    'Post-SDF-reduction scenarios: if military presence were substantially withdrawn, would suppression persist? Community surveys and testimony about internalization vs. structural constraint. Comparative analysis of host communities in other countries with similar military presence.',
    'If structural, the measured suppression (0.72) underestimates the constraint''s coercive force on Okinawans, because they carry the suppression with them even if deployment scales back. If internalized, the constraint''s suppression is effective at reducing active resistance despite its coercive character.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(okinawan_suppression_mechanism, empirical, 'Whether Okinawan suppression is structural or internalized.').

omega_variable(
    kernel_reading_coexistence,
    'Can the strict_pacifist_reading and this collective_self_defense_reading coexist as live interpretive options in a single constitutional framework, or does accepting one logically foreclose the other?',
    'Constitutional hermeneutics: textual analysis of whether ''never be maintained'' can simultaneously permit both any armed forces (pacifist reading) and collective military action (this reading). Jurisprudential analysis of whether a court could accept both without self-contradiction.',
    'If coexistent, both readings are live options and the cabinet''s choice of this reading is a political decision, not a logical necessity. If one forecloses the other, the constitutional text itself determines which reading is correct. This shapes the status of the victim constituencies'' objections: are they losing a legitimate interpretive option, or objecting to a determined constitutional conclusion?',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_coexistence, conceptual, 'Whether Article 9''s text permits both strict-pacifist and collective-self-defense readings simultaneously.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_9_war_renunciation__collective_self_defense_reading, 1972, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t1972, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 1972, 0.12).
narrative_ontology:measurement(arti_tr_t1992, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 1992, 0.18).
narrative_ontology:measurement(arti_tr_t2005, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 2005, 0.28).
narrative_ontology:measurement(arti_tr_t2015, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 2015, 0.37).
narrative_ontology:measurement(arti_tr_t2020, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 2020, 0.39).
narrative_ontology:measurement(arti_tr_t2024, article_9_war_renunciation__collective_self_defense_reading, theater_ratio, 2024, 0.41).

% Extraction over time
narrative_ontology:measurement(arti_be_t1972, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 1972, 0.38).
narrative_ontology:measurement(arti_be_t1992, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 1992, 0.51).
narrative_ontology:measurement(arti_be_t2005, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 2005, 0.62).
narrative_ontology:measurement(arti_be_t2015, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 2015, 0.66).
narrative_ontology:measurement(arti_be_t2020, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 2020, 0.67).
narrative_ontology:measurement(arti_be_t2024, article_9_war_renunciation__collective_self_defense_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t1972, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 1972, 0.44).
narrative_ontology:measurement(arti_su_t1992, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 1992, 0.52).
narrative_ontology:measurement(arti_su_t2005, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 2005, 0.63).
narrative_ontology:measurement(arti_su_t2015, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 2015, 0.69).
narrative_ontology:measurement(arti_su_t2020, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 2020, 0.7).
narrative_ontology:measurement(arti_su_t2024, article_9_war_renunciation__collective_self_defense_reading, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_9_war_renunciation__collective_self_defense_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_9_war_renunciation__collective_self_defense_reading, 0.14).
narrative_ontology:affects_constraint(article_9_war_renunciation__collective_self_defense_reading, article_9_war_renunciation__strict_pacifist_reading).
narrative_ontology:affects_constraint(article_9_war_renunciation__collective_self_defense_reading, article_9_war_renunciation__inherent_right_reading).
narrative_ontology:affects_constraint(article_9_war_renunciation__collective_self_defense_reading, us_japan_security_alliance_treaty_base).
narrative_ontology:affects_constraint(article_9_war_renunciation__collective_self_defense_reading, okinawa_military_base_governance).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a three-way interpretive contest over Article 9. The strict_pacifist_reading treats Article 9 as categorical prohibition on any armed forces. The inherent_right_reading preserves minimum direct-defense capacity but rejects collective action outside direct attack. This collective_self_defense_reading absorbs the inherent-right language but expands scope to collective action under 'survival-threatened' trigger. All three are constraints on the SAME kernel text; they have different ε values, different victim sets, different enforcement mechanisms. The three constraint stories are linked via network.affects_constraints to show the interpretation family and the causal/political dependencies between readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article_9_war_renunciation__collective_self_defense_reading, institutional, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
