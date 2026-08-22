% ============================================================================
% CONSTRAINT STORY: article_9_war_renunciation__strict_pacifist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_9_war_renunciation__strict_pacifist_reading, []).

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
 *   constraint_id: article_9_war_renunciation__strict_pacifist_reading
 *   human_readable: Article 9 War Renunciation — Strict Pacifist (Categorical Prohibition) Reading
 *   domain: constitutional_law/security_policy
 *
 * SUMMARY:
 *   This story instantiates the strict-pacifist reading of the Article 9
 *   kernel: the textual clause 'land, sea, and air forces, as well as other
 *   war potential, will never be maintained' is read as a categorical,
 *   absolute prohibition on any organized armed force, defensive or
 *   otherwise. Under this reading Japan's only lawful paths to security are
 *   non-military means (diplomacy, economic statecraft, civil defense) or
 *   dependence on allied military guarantees. This reading treats the
 *   Self-Defense Forces' six-decade existence as a standing constitutional
 *   violation tolerated by political convenience rather than legalized by any
 *   amendment. The rising theater_ratio reflects an increasingly elaborate
 *   interpretive apparatus (Cabinet Legal Bureau opinions, doctrine of
 *   'exclusively defense-oriented policy,' semantic distinctions between 'war
 *   potential' and 'minimum necessary capability') that performs
 *   constitutional compliance with the categorical text while the state's
 *   actual military capacity has grown substantially — the theater is the
 *   widening gap between the literal prohibition and administered practice,
 *   papered over rather than resolved.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_9_war_renunciation__strict_pacifist_reading, 0.42).
domain_priors:suppression_score(article_9_war_renunciation__strict_pacifist_reading, 0.55).
domain_priors:theater_ratio(article_9_war_renunciation__strict_pacifist_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(article_9_war_renunciation__strict_pacifist_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_9_war_renunciation__strict_pacifist_reading, tangled_rope).
narrative_ontology:human_readable(article_9_war_renunciation__strict_pacifist_reading, "Article 9 War Renunciation — Strict Pacifist (Categorical Prohibition) Reading").
narrative_ontology:topic_domain(article_9_war_renunciation__strict_pacifist_reading, "constitutional_law/security_policy").

domain_priors:requires_active_enforcement(article_9_war_renunciation__strict_pacifist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_9_war_renunciation__strict_pacifist_reading, 'eb811890-d0c0-4f8c-be23-5e0424e5798d').
narrative_ontology:cs_kernel_codification('eb811890-d0c0-4f8c-be23-5e0424e5798d', fixed_text).
narrative_ontology:cs_authority_grounding('eb811890-d0c0-4f8c-be23-5e0424e5798d', practice).
narrative_ontology:cs_interpretation_layer_present('eb811890-d0c0-4f8c-be23-5e0424e5798d').
narrative_ontology:cs_reading_relation('eb811890-d0c0-4f8c-be23-5e0424e5798d', article_9_war_renunciation__inherent_right_reading, forecloses).
narrative_ontology:cs_reading_relation('eb811890-d0c0-4f8c-be23-5e0424e5798d', article_9_war_renunciation__collective_self_defense_reading, forecloses).
narrative_ontology:cs_axiom('eb811890-d0c0-4f8c-be23-5e0424e5798d', foundational, textual_literalism_binds_absolutely).
narrative_ontology:cs_axiom_status(textual_literalism_binds_absolutely, holdable).
narrative_ontology:cs_axiom_grounding('eb811890-d0c0-4f8c-be23-5e0424e5798d', textual_literalism_binds_absolutely, conventional).
narrative_ontology:cs_axiom('eb811890-d0c0-4f8c-be23-5e0424e5798d', foundational, no_inherent_sovereign_exception_survives_explicit_renunciation).
narrative_ontology:cs_axiom_status(no_inherent_sovereign_exception_survives_explicit_renunciation, holdable).
narrative_ontology:cs_axiom_grounding('eb811890-d0c0-4f8c-be23-5e0424e5798d', no_inherent_sovereign_exception_survives_explicit_renunciation, deontological).
narrative_ontology:cs_reference_frame('eb811890-d0c0-4f8c-be23-5e0424e5798d', id_1947_occupation_era_categorical_prohibition).
narrative_ontology:cs_drift_state('eb811890-d0c0-4f8c-be23-5e0424e5798d', contemporary_post_2015_security_legislation_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('eb811890-d0c0-4f8c-be23-5e0424e5798d', '').
narrative_ontology:cs_kernel_id(article_9_war_renunciation__strict_pacifist_reading, article_9_war_renunciation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__strict_pacifist_reading, pacifist_civil_society_movements).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__strict_pacifist_reading, regional_states_wary_of_remilitarization).
narrative_ontology:constraint_beneficiary(article_9_war_renunciation__strict_pacifist_reading, united_states_forward_basing_strategy).
narrative_ontology:constraint_victim(article_9_war_renunciation__strict_pacifist_reading, japanese_state_security_autonomy).
narrative_ontology:constraint_victim(article_9_war_renunciation__strict_pacifist_reading, self_defense_forces_personnel).
narrative_ontology:constraint_victim(article_9_war_renunciation__strict_pacifist_reading, coastal_and_frontier_prefectures).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Litigate, organize, and vote to hold the government to the literal text. They read 'never be maintained' as settling the question and treat any move toward standing forces as constitutional violation. They gain moral and political standing from the categorical reading and lose it if the reading is abandoned.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, pacifist_civil_society_movements, beneficiary,
    organized, generational, constrained, national).

% Benefits from a Japan that is textually barred from independent military capacity and therefore structurally dependent on the US-Japan security treaty for its defense. The categorical reading, whatever its domestic pacifist origins, locks in a client-state security posture the US drafted the clause partly to produce and later found useful to preserve.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, united_states_forward_basing_strategy, beneficiary,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(article_9_war_renunciation__strict_pacifist_reading, united_states_forward_basing_strategy, agenda_setter).

% Neighboring states with historical grievance from Japanese wartime aggression treat the categorical prohibition as a durable guarantee against renewed Japanese military autonomy. They have no formal role in enforcing the reading but invoke it diplomatically whenever Japan's security posture shifts.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, regional_states_wary_of_remilitarization, beneficiary,
    institutional, generational, analytical, continental).

% The Japanese state, under this reading, cannot lawfully maintain any organized armed force — defensive or otherwise — without a constitutional amendment that has never passed. It is left to either violate the text (via the Self-Defense Forces' contested existence) or accept structural dependence on the US alliance for any credible deterrence, forfeiting an independent security policy either way.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, japanese_state_security_autonomy, payer,
    institutional, generational, trapped, national).

% Serve in an organization whose basic legal legitimacy is perpetually contested under the reading they operate within. Career, pension, and professional identity are built on an institution that a straightforward textual reading of the constitution says may not exist, producing chronic legitimacy anxiety and constrained doctrine (no offensive capability, ambiguous rules of engagement).
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, self_defense_forces_personnel, payer,
    moderate, biographical, constrained, national).

% Populations nearest contested waters and airspace bear the practical security consequences of a constitutionally hobbled defense posture — reliance on slow-to-mobilize coast guard assets and foreign basing rather than an unambiguously legitimate national defense force. They have no voice in the interpretive dispute and live with its operational consequences.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, coastal_and_frontier_prefectures, payer,
    powerless, immediate, trapped, regional).

% The government's own legal apparatus has, since the 1950s, adopted the inherent-right reading in practice while never repudiating the strict-pacifist reading's textual authority — administering a permanent gap between declared constitutional meaning and operative policy, reinterpreting rather than amending.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, diet_and_cabinet_legal_bureau, agenda_setter,
    institutional, generational, constrained, national).

% The formal mechanism that could resolve the kernel dispute (Article 96 supermajority amendment) has never been successfully invoked for Article 9. Its absence from the actual resolution path is itself part of the story: the categorical reading persists not because it was affirmed through the amendment process but because no reading has cleared that bar.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, constitutional_amendment_process, excluded,
    institutional, civilizational, trapped, national).

% Japanese courts have largely treated Article 9's application to the Self-Defense Forces as a political question, declining to rule definitively. Scholars remain split, with the strict-pacifist reading retaining significant academic and public support despite decades of contrary state practice.
narrative_ontology:constraint_stakeholder(article_9_war_renunciation__strict_pacifist_reading, constitutional_scholars_and_courts, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_9_war_renunciation__strict_pacifist_reading, diffuse).
narrative_ontology:fixing_cost_class(article_9_war_renunciation__strict_pacifist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The categorical reading coordinates a durable postwar settlement: it gives pacifist domestic constituencies, wary regional neighbors, and the US alliance framework a shared, legible commitment that Japan will not reconstitute independent offensive military capacity — reducing everyone's need to monitor and react to Japanese remilitarization signals.
% TRANSFER_FUNCTION: Moves security autonomy and the capacity for independent defense policy away from the Japanese state and its frontier populations, and toward the US alliance structure (which captures basing rights and strategic leverage) and toward pacifist civil society (which captures durable moral and political authority over defense debates).
% ABSENT_VOICES: Self-Defense Forces personnel and frontier prefecture residents bear the operational costs of the ambiguity but have no interpretive standing — courts decline to rule, and the amendment process that would let the public actually decide has never been invoked. Regional security experts warning of the gap between textual prohibition and operational necessity are heard in policy circles but rarely shape the constitutional debate itself.
% DISAPPEARANCE_RATIONALE: If the strict-pacifist reading were to prevail and be enforced as written, the Self-Defense Forces would face immediate dissolution or radical restructuring, Japan's defense would become fully alliance-dependent, and six decades of accumulated defense doctrine, procurement, and personnel structure would have to be dismantled or reconceived from zero.
% FOUNDING_PROBLEM: Post-surrender Japan needed a credible, internationally legible guarantee that it would never again wage aggressive war, both to satisfy Allied occupation demands and to reassure a region devastated by Japanese militarism.
% FOUNDING_PROBLEM_CORROBORATION: Pacifist civil society and many constitutional scholars attest the founding problem — restraining Japanese military aggression — remains live and requires the categorical reading to stay meaningful. The Cabinet Legal Bureau's own six-decade practice of permitting the Self-Defense Forces under a contrary operative interpretation, plus statements from US and allied strategic planners treating Japan as a normal security partner, corroborate from outside the pacifist movement that the original threat-restraint problem is treated as substantially resolved by state practitioners, even though the textual reading has never been formally revised.
narrative_ontology:disappearance_verdict(article_9_war_renunciation__strict_pacifist_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_9_war_renunciation__strict_pacifist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_9_war_renunciation__strict_pacifist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article_9_war_renunciation__strict_pacifist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_9_war_renunciation__strict_pacifist_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_9_war_renunciation__strict_pacifist_reading_tests).
:- end_tests(article_9_war_renunciation__strict_pacifist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (ε=0.42) is moderate: this reading, taken on its own terms, is not primarily an extraction mechanism against Japan — it is a genuine, if severe, coordination commitment that gives the region and the alliance predictability. But real costs are imposed on Japanese state security autonomy and on frontier populations who live with an internally contradictory defense posture, which is why the reading carries both a coordination function and a victim set — the structural signature of a tangled rope rather than a clean rope or a mountain. Suppression (0.55) is substantial because maintaining the categorical reading against six decades of contrary state practice requires active interpretive and political labor — courts declining jurisdiction, the Cabinet Legal Bureau's continuous doctrinal maintenance, and civil society's continuous political mobilization to prevent formal abandonment of the text. Resistance (0.72) is high because the reading is continuously contested by the government's own operative practice, by revisionist political coalitions, and by security analysts — this is not a settled natural fact but an actively fought-over textual claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Pacifist civil society and the US alliance structure sit near the beneficiary end: both gain from a Japan textually and politically constrained from independent military buildup, though for very different reasons (moral/political authority versus strategic dependency capture). The Japanese state itself, its Self-Defense Forces personnel, and frontier prefecture populations sit near the target end: they bear the operational and legitimacy costs of a security apparatus that must exist in practice while remaining constitutionally suspect in the reading's own terms. The Cabinet Legal Bureau is best modeled as an agenda-setter that manages the gap rather than resolving it — administering ambiguity rather than committing to either pole.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — restraining Japanese militarism to reassure the region and satisfy occupation demands — is genuinely contested as live versus dead: pacifist and regional beneficiaries treat it as live and requiring the categorical reading's continued force; the state's own six-decade practice of maintaining a substantial defense force under contrary operative doctrine suggests the practical constraint has been substituted by something else (alliance-embedded conventional deterrence) while the categorical textual claim is preserved mostly for its coordination and legitimacy value. Classifying this as tangled_rope rather than snare or mountain prevents two mislabelings: treating the reading as pure extraction (it does perform a real, valued coordination function for multiple beneficiary groups) and treating it as settled natural law (the amendment process that would resolve it decisively has never been invoked, and courts treat it as a live political question, not a closed constitutional fact).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indeterminacy,
    'Is the strict-pacifist reading the constitutionally correct reading of Article 9''s text, or has six decades of state practice under the inherent-right reading effectively superseded it as a matter of living constitutional law, regardless of the text''s literal wording?',
    'A definitive Japanese Supreme Court ruling on the merits of SDF constitutionality (rather than continued political-question avoidance), or a successful Article 96 amendment either codifying or abandoning the categorical prohibition.',
    'If the categorical reading is judicially vindicated, the Self-Defense Forces'' existence becomes an acknowledged constitutional crisis requiring resolution; if the inherent-right reading is judicially vindicated, this story''s classification of Japanese security autonomy as a ''victim'' of the categorical text would need to be reassessed as a victim of unresolved constitutional ambiguity rather than of an operative prohibition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether the strict-pacifist textual reading or six decades of contrary practice constitutes the operative constitutional rule.').

omega_variable(
    sibling_reading_structural_delta,
    'What would the beneficiary and victim sets look like under the collective_self_defense_reading, given that it removes the very constraint (categorical prohibition on organized forces) whose costs this story documents?',
    'Author the collective_self_defense_reading as a separate constraint story with its own ε, beneficiaries, and victims — expected to show near-zero extraction against Japanese security autonomy but new extraction/risk transferred to regional neighbors and to Japanese taxpayers funding expanded alliance military commitments.',
    'Confirms per the ε-invariance principle that the three Article 9 readings are three distinct constraints, not one constraint measured three ways; this story''s ε must not be reconciled with or averaged against the siblings'' ε values.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Documents that sibling kernel readings are separately authored constraints per DP-001.').

omega_variable(
    textualism_vs_pragmatic_drift,
    'Is the ongoing gap between the categorical text and the SDF''s operative existence better understood as a stable, tolerated legal fiction (a scaffold-like arrangement everyone quietly accepts) or as an active, unresolved contradiction that could rupture under sufficient political or security pressure (e.g., a regional crisis forcing a definitive test)?',
    'Track whether future security crises (e.g., Taiwan Strait contingency, North Korean escalation) produce renewed formal amendment attempts or judicial review petitions, versus continued indefinite toleration of the ambiguity.',
    'If stable, the tangled_rope classification with high theater_ratio is durable; if rupture-prone, the constraint may be better modeled as approaching a snare (categorical text used to delegitimize a de facto necessary institution) or trending toward eventual scaffold resolution via amendment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(textualism_vs_pragmatic_drift, empirical, 'Whether the text-practice gap is a stable equilibrium or an accumulating pressure toward rupture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_9_war_renunciation__strict_pacifist_reading, 1947, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t1947, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 1947, 0.15).
narrative_ontology:measurement(arti_tr_t1960, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 1960, 0.25).
narrative_ontology:measurement(arti_tr_t1976, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 1976, 0.35).
narrative_ontology:measurement(arti_tr_t1992, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 1992, 0.42).
narrative_ontology:measurement(arti_tr_t2015, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 2015, 0.47).
narrative_ontology:measurement(arti_tr_t2024, article_9_war_renunciation__strict_pacifist_reading, theater_ratio, 2024, 0.5).

% Extraction over time
narrative_ontology:measurement(arti_be_t1947, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 1947, 0.2).
narrative_ontology:measurement(arti_be_t1960, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 1960, 0.28).
narrative_ontology:measurement(arti_be_t1976, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 1976, 0.33).
narrative_ontology:measurement(arti_be_t1992, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 1992, 0.36).
narrative_ontology:measurement(arti_be_t2015, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 2015, 0.4).
narrative_ontology:measurement(arti_be_t2024, article_9_war_renunciation__strict_pacifist_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t1947, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 1947, 0.35).
narrative_ontology:measurement(arti_su_t1960, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 1960, 0.4).
narrative_ontology:measurement(arti_su_t1976, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 1976, 0.45).
narrative_ontology:measurement(arti_su_t1992, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 1992, 0.48).
narrative_ontology:measurement(arti_su_t2015, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 2015, 0.52).
narrative_ontology:measurement(arti_su_t2024, article_9_war_renunciation__strict_pacifist_reading, suppression_requirement, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_9_war_renunciation__strict_pacifist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_9_war_renunciation__strict_pacifist_reading, 0.12).
narrative_ontology:affects_constraint(article_9_war_renunciation__strict_pacifist_reading, article_9_inherent_right_reading).
narrative_ontology:affects_constraint(article_9_war_renunciation__strict_pacifist_reading, article_9_collective_self_defense_reading).
narrative_ontology:affects_constraint(article_9_war_renunciation__strict_pacifist_reading, us_japan_security_treaty_basing_regime).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints decomposing the colloquial label 'Article 9 pacifism' per the ε-invariance principle: strict_pacifist_reading (this story, tangled_rope, ε=0.42), inherent_right_reading (expected mountain-adjacent or rope, much lower ε — treats minimum defense as settled sovereign right, not extraction), and collective_self_defense_reading (expected tangled_rope or snare with a different victim set — regional neighbors and expanded fiscal/personnel burden on Japanese taxpayers rather than domestic security autonomy). Each reading is linked via affects_constraints; none averages or reconciles ε with the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
