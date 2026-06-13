% ============================================================================
% CONSTRAINT STORY: jihad_quranic_corpus__defensive_spiritual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jihad_quranic_corpus__defensive_spiritual_reading, []).

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
 *   constraint_id: jihad_quranic_corpus__defensive_spiritual_reading
 *   human_readable: Jihad as Defensive Spiritual Struggle with Proportional Armed Response
 *   domain: religious/political/legal
 *
 * SUMMARY:
 *   This constraint story instantiates ONE reading of the contested Quranic
 *   corpus on jihad (kernel_id: jihad_quranic_corpus). The
 *   defensive-spiritual reading prioritizes internal spiritual struggle
 *   (jihad al-nafs) as the superior form of jihad, permits armed defense
 *   against aggression under state authority and proportionality constraints,
 *   excludes non-Muslims from the victim set except when they are aggressors,
 *   and establishes non-combatant immunity as inviolable. This reading is
 *   held by major Islamic jurisprudential schools (Maliki, Hanafi, Shafi'i)
 *   and contemporary mainstream Islamic scholarship. It coexists with two
 *   sibling readings: an expansionist-legalist reading permitting offensive
 *   campaigns to establish Islamic governance (with jurisprudential
 *   conditions), and a revolutionary-vanguard reading treating jihad as
 *   immediate individual obligation against apostate rulers. The three
 *   readings are NOT accounts of objective reality — they are readings of the
 *   same contested scriptural kernel, each internally coherent but
 *   structurally incompatible at the level of obligation and legitimacy.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jihad_quranic_corpus__defensive_spiritual_reading, 0.32).
domain_priors:suppression_score(jihad_quranic_corpus__defensive_spiritual_reading, 0.18).
domain_priors:theater_ratio(jihad_quranic_corpus__defensive_spiritual_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jihad_quranic_corpus__defensive_spiritual_reading, rope).
narrative_ontology:human_readable(jihad_quranic_corpus__defensive_spiritual_reading, "Jihad as Defensive Spiritual Struggle with Proportional Armed Response").
narrative_ontology:topic_domain(jihad_quranic_corpus__defensive_spiritual_reading, "religious/political/legal").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jihad_quranic_corpus__defensive_spiritual_reading, 'ca91858e-72ff-4c3f-ac58-4ad6758574b8').
narrative_ontology:cs_kernel_codification('ca91858e-72ff-4c3f-ac58-4ad6758574b8', fixed_text).
narrative_ontology:cs_authority_grounding('ca91858e-72ff-4c3f-ac58-4ad6758574b8', lineage).
narrative_ontology:cs_interpretation_layer_present('ca91858e-72ff-4c3f-ac58-4ad6758574b8').
narrative_ontology:cs_reading_relation('ca91858e-72ff-4c3f-ac58-4ad6758574b8', jihad_quranic_corpus__expansionist_legalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('ca91858e-72ff-4c3f-ac58-4ad6758574b8', jihad_quranic_corpus__revolutionary_vanguard_reading, coexists_with).
narrative_ontology:cs_axiom('ca91858e-72ff-4c3f-ac58-4ad6758574b8', foundational, spiritual_struggle_primacy).
narrative_ontology:cs_axiom_status(spiritual_struggle_primacy, holdable).
narrative_ontology:cs_axiom_grounding('ca91858e-72ff-4c3f-ac58-4ad6758574b8', spiritual_struggle_primacy, deontological).
narrative_ontology:cs_axiom('ca91858e-72ff-4c3f-ac58-4ad6758574b8', foundational, state_authority_requirement).
narrative_ontology:cs_axiom_status(state_authority_requirement, holdable).
narrative_ontology:cs_axiom_grounding('ca91858e-72ff-4c3f-ac58-4ad6758574b8', state_authority_requirement, conventional).
narrative_ontology:cs_axiom('ca91858e-72ff-4c3f-ac58-4ad6758574b8', secondary, proportionality_non_combatant_immunity).
narrative_ontology:cs_axiom_status(proportionality_non_combatant_immunity, holdable).
narrative_ontology:cs_axiom_grounding('ca91858e-72ff-4c3f-ac58-4ad6758574b8', proportionality_non_combatant_immunity, deontological).
narrative_ontology:cs_reference_frame('ca91858e-72ff-4c3f-ac58-4ad6758574b8', quranic_directive_toward_spiritual_internal_struggle_with_defensive_military_constraint).
narrative_ontology:cs_drift_state('ca91858e-72ff-4c3f-ac58-4ad6758574b8', contemporary_post_colonial_pluralist_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ca91858e-72ff-4c3f-ac58-4ad6758574b8', '').
narrative_ontology:cs_kernel_id(jihad_quranic_corpus__defensive_spiritual_reading, jihad_quranic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__defensive_spiritual_reading, muslim_scholars_interpreting_tradition).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__defensive_spiritual_reading, muslim_communities_seeking_coexistence).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__defensive_spiritual_reading, states_claiming_legitimate_war_authority).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jihad_quranic_corpus__defensive_spiritual_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(jihad_quranic_corpus__defensive_spiritual_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jihad_quranic_corpus__defensive_spiritual_reading_tests).
:- end_tests(jihad_quranic_corpus__defensive_spiritual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is LOW (0.32) because this reading does not claim extraction from non-Muslims or non-believers as a class; it establishes mutual coexistence and non-combatant immunity as inviolable principles. The constraint operates primarily through interpretive authority and scholarly consensus, not through suppression or enforcement machinery. Theater ratio is VERY LOW (0.12) because the reading's function (establishing a framework for legitimate warfare and coexistence) is largely its stated function — there is minimal gap between what the constraint appears to do and what it actually does. Suppression is LOW (0.18) because the reading does not require active enforcement against resistance; those who hold it do so by interpretive commitment to textual tradition, and those who reject it do so by alternative scriptural readings, not by coercion. The reading is STABLE over the measurement interval: scholarly consensus on this framework has persisted across 1200+ years of Islamic jurisprudence, and the core principles show no drift. The claim/metric gap is intentional: the constraint is CLAIMED as rope (genuine coordination of Islamic jurisprudence with non-aggression and coexistence norms) while the authored metrics show very low extraction and very low suppression — consistent with a high-fidelity coordination mechanism.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of Muslim scholars committed to this reading, jihad is properly understood as integrated with mercy, proportionality, and spiritual priority. From the seat of revolutionary movements, the same Quranic verses support individual obligation to resist occupation regardless of state authority. From the seat of expansionist legalists, the same corpus permits establishing Islamic governance where absent. The engine computes how each seat would experience this constraint DIFFERENTLY — not because the reading changes, but because each seat's relationship to the constraint's restrictions differs. A revolutionary actor experiences the requirement for state authority as a binding constraint (high d); a state actor experiences it as empowering (low d). The directionality derivation captures this asymmetry from the structural data (power, exit, beneficiary/victim position).
 *
 * DIRECTIONALITY LOGIC:
 *   Muslim scholars interpreting tradition hold d near beneficiary (they build and maintain the consensus framework; the framework vindicates their exegetical work). Muslim communities seeking coexistence and non-Muslim populations hold d near beneficiary (the constraint enables their participation in the world without abandoning principle or facing perpetual threat). States claiming legitimate war authority sit near symmetric: they gain the monopoly on force declaration but lose the freedom to wage unlimited war — genuine coordination benefit with clear cost. Revolutionary movements and expansionist scholars hold d near target: the constraint restricts their claims to obligation and legitimacy. The reading itself does not suppress these alternative readings by force; it suppresses them through scholarly authority and textual argument — suppression_requirement is low because the suppression is interpretive, not institutional.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids mandatrophy on its own terms: the founding problem (reconciling scriptural imperatives for defense with imperatives for mercy and proportionality) remains live, and the reading's function (establishing a coherent jurisprudential framework) remains necessary. The reading does NOT claim that the problem of aggression has been solved or that the Quranic prescription for jihad has become obsolete — it claims that the proper understanding of jihad prioritizes spiritual struggle and constrains armed response to specific conditions. Mandatrophy would arise only if the founding problem ceased to be live (if Islamic communities no longer needed to reconcile these scriptural imperatives) or if the reading's function was entirely replaced by a different interpretive framework. Neither condition holds: the reading remains the dominant framework in mainstream Islamic jurisprudence and addresses a persistent theological problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    quranic_verse_interpretation_ambiguity,
    'Which Quranic verses establish the PRIMARY framework for jihad: those emphasizing spiritual struggle and mercy (e.g., 22:78, 49:15), or those addressing armed defense and warfare (e.g., 2:190-193, 8:39)? Does the corpus establish a clear hierarchy, or do competing verses leave room for multiple coherent readings?',
    'Comparative textual analysis across Islamic jurisprudential schools and contemporary Quranic exegesis (tafsir) to establish whether the corpus itself privileges one set of verses or whether the hierarchy is imposed by interpretive tradition.',
    'If the corpus itself establishes a clear hierarchy (spiritual struggle as primary, armed response as secondary), this reading''s claim to textual grounding is strengthened and the sibling readings face a charge of selective reading. If the hierarchy is imposed by interpretive tradition, the contest between readings becomes one of scholarly authority and consensus-building, not textual evidence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quranic_verse_interpretation_ambiguity, empirical, 'Whether the Quranic corpus establishes a clear hierarchy of jihad forms or leaves them structurally ambiguous.').

omega_variable(
    state_authority_legitimacy_ambiguity,
    'In contexts where the state is itself illegitimate, aggressive, or apostate, does the requirement for state authorization remain binding? Is state authority a necessary condition for jihad in all contexts, or only when legitimate state authority exists?',
    'Historical analysis of how Islamic jurisprudence has addressed jihad under illegitimate rulers (e.g., during Crusades, Ottoman decline, colonial occupation) and contemporary Islamic legal scholarship on when state authority can be overridden.',
    'If state authority is non-negotiable, the reading forecloses revolutionary readings and establishes state monopoly on force. If state authority can be overridden when the state itself is illegitimate or apostate, the boundary between this reading and the revolutionary-vanguard reading becomes permeable, and the reading''s claim to a rigid structural distinction weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_authority_legitimacy_ambiguity, conceptual, 'Whether state authority requirement is absolute or can be overridden by state illegitimacy.').

omega_variable(
    kernel_vs_reading_alternative_framing,
    'Is the contest between defensive-spiritual, expansionist-legalist, and revolutionary-vanguard readings a matter of DIFFERENT KERNELS (three genuinely distinct scriptural commitments) or THREE READINGS OF ONE KERNEL (the same corpus producing three coherent but incompatible interpretations)?',
    'Systematic comparison of textual bases: if all three readings cite overlapping Quranic verses and justify themselves through exegetical method, they are readings of one kernel. If they cite non-overlapping or contradictory verse sets without common exegetical apparatus, they are distinct kernels.',
    'If one kernel with three readings: the readings coexist as competing interpretations of shared scripture, and truth/falsity is not a meaningful discriminant (the kernel permits multiple readings). If three kernels: the readings are incompatible at the level of foundational commitment, and the contest is genuinely zero-sum — one must be chosen over the others. The framing chosen affects how the engine treats kernel dynamics and signature patterns.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_vs_reading_alternative_framing, conceptual, 'Whether the three jihad readings constitute one contested kernel or three distinct scriptural kernels.').

omega_variable(
    suppression_internalization_mechanism,
    'When Muslim believers hold this reading and reject the revolutionary-vanguard reading, is the rejection purely interpretive (they understand the Quranic corpus differently), or does it involve internalized suppression (they accept state authority as binding even when they disagree with it)?',
    'Post-exit trajectory analysis: if Muslims who leave state authority structures or break with mainstream interpretive consensus continue to accept the state-authority requirement, suppression is internalized. If they immediately reframe the requirement as illegitimate, suppression is structural/interpretive only.',
    'If internalized: the reading''s effective suppression of alternatives is higher than the authored 0.18 suggests, because believers carry the suppression of individual obligation even after escaping institutional contexts. If purely interpretive: suppression remains low because it operates only through argument and scholarly consensus, dissolving when the believer encounters an alternative interpretive community.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_internalization_mechanism, empirical, 'Whether the suppression of revolutionary readings is structural or internalized in believers.').

omega_variable(
    coexistence_feasibility_ambiguity,
    'Does this reading''s commitment to coexistence with non-Muslims require them to accept subordinate status under Islamic authority (dhimmi framework), or does it permit full equality in pluralist governance structures?',
    'Historical analysis of how Islamic jurisprudence has applied the coexistence framework under different state structures (caliphates, Ottoman millet system, modern nation-states) and contemporary Islamic legal scholarship on the requirements for coexistence.',
    'If subordinate status is required: the reading''s claim to coexistence is weakened (it permits non-Muslims to exist but not as equals), and extraction from non-Muslims as a class becomes implicit. If equality is permitted: coexistence is genuine, and the reading''s low extractiveness claim holds. The ambiguity is real in the jurisprudential literature.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coexistence_feasibility_ambiguity, conceptual, 'Whether coexistence requires non-Muslim subordinacy or permits full equality.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jihad_quranic_corpus__defensive_spiritual_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jiha_tr_t0, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(jiha_tr_t10, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 10, 0.11).
narrative_ontology:measurement(jiha_tr_t20, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 20, 0.11).
narrative_ontology:measurement(jiha_tr_t30, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 30, 0.12).
narrative_ontology:measurement(jiha_tr_t40, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 40, 0.12).
narrative_ontology:measurement(jiha_tr_t50, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 50, 0.12).

% Extraction over time
narrative_ontology:measurement(jiha_be_t0, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(jiha_be_t10, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 10, 0.3).
narrative_ontology:measurement(jiha_be_t20, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 20, 0.31).
narrative_ontology:measurement(jiha_be_t30, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 30, 0.32).
narrative_ontology:measurement(jiha_be_t40, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 40, 0.32).
narrative_ontology:measurement(jiha_be_t50, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 50, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(jiha_su_t0, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 0, 0.16).
narrative_ontology:measurement(jiha_su_t10, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 10, 0.17).
narrative_ontology:measurement(jiha_su_t20, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 20, 0.17).
narrative_ontology:measurement(jiha_su_t30, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 30, 0.18).
narrative_ontology:measurement(jiha_su_t40, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 40, 0.18).
narrative_ontology:measurement(jiha_su_t50, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 50, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jihad_quranic_corpus__defensive_spiritual_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jihad_quranic_corpus__defensive_spiritual_reading, 0.08).
narrative_ontology:affects_constraint(jihad_quranic_corpus__defensive_spiritual_reading, jihad_quranic_corpus__expansionist_legalist_reading).
narrative_ontology:affects_constraint(jihad_quranic_corpus__defensive_spiritual_reading, jihad_quranic_corpus__revolutionary_vanguard_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the contested Quranic corpus on jihad (kernel_id: jihad_quranic_corpus). The defensive-spiritual reading prioritizes internal spiritual struggle and defensive armed response under state authority with proportionality constraints. The expansionist-legalist reading permits offensive campaigns to establish Islamic governance. The revolutionary-vanguard reading treats jihad as immediate individual obligation against apostates, bypassing state authority. Each reading instantiates a different ε and produces different beneficiary/victim structures. The three are linked via network.affects_constraints as members of the same constraint family, reflecting their contest for interpretive authority over a single scriptural kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jihad_quranic_corpus__defensive_spiritual_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
