% ============================================================================
% CONSTRAINT STORY: jihad_quranic_corpus__defensive_spiritual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: jihad_quranic_corpus__defensive_spiritual_reading
 *   human_readable: Defensive Spiritual Reading of Quranic Jihad
 *   domain: religious/political_theology/islamic_jurisprudence
 *
 * SUMMARY:
 *   This constraint story models the defensive_spiritual_reading of the
 *   jihad_quranic_corpus kernel: the doctrinal position that jihad is
 *   primarily internal spiritual struggle (jihad al-nafs) and that armed
 *   jihad is legitimate only as defensive response to aggression, declared by
 *   state authority, constrained by proportionality and non-combatant
 *   immunity. Non-Muslims are structurally outside the victim set unless they
 *   are aggressors. The reading is presented as a coordination mechanism that
 *   regulates violence and privileges coexistence. It is claimed as rope. The
 *   metrics are authored independently: low extractiveness, moderate
 *   suppression of unauthorized militancy, low theater, and moderate
 *   resistance from rival readings.
 *
 * KEY AGENTS:
 *   - muslim_adherents: Primary participants (organized/global/identity_locked) â net beneficiaries of regulated spiritual and military ethics
 *   - non_muslim_communities: Protected beneficiaries (organized/global/constrained) â outside victim set via non-combatant immunity
 *   - legitimate_state_authority: Agenda setter (institutional/global/constrained) â monopolizes declaration of armed jihad
 *   - jurist_class_ulama: Agenda setter and interpretive authority (organized/global/identity_locked) â maintains the doctrinal boundary
 *   - unauthorized_militant_groups: Excluded actors (moderate/regional/constrained) â expansionist and revolutionary actors suppressed by the state-authority requirement
 *   - external_observer_academics: Observer (analytical/civilizational/analytical) â tracks the contest between readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jihad_quranic_corpus__defensive_spiritual_reading, 0.22).
domain_priors:suppression_score(jihad_quranic_corpus__defensive_spiritual_reading, 0.35).
domain_priors:theater_ratio(jihad_quranic_corpus__defensive_spiritual_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jihad_quranic_corpus__defensive_spiritual_reading, rope).
narrative_ontology:human_readable(jihad_quranic_corpus__defensive_spiritual_reading, "Defensive Spiritual Reading of Quranic Jihad").
narrative_ontology:topic_domain(jihad_quranic_corpus__defensive_spiritual_reading, "religious/political_theology/islamic_jurisprudence").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jihad_quranic_corpus__defensive_spiritual_reading, '6d2d78d3-21fe-43cf-a6bb-0e342665216b').
narrative_ontology:cs_kernel_codification('6d2d78d3-21fe-43cf-a6bb-0e342665216b', fixed_text).
narrative_ontology:cs_authority_grounding('6d2d78d3-21fe-43cf-a6bb-0e342665216b', lineage).
narrative_ontology:cs_interpretation_layer_present('6d2d78d3-21fe-43cf-a6bb-0e342665216b').
narrative_ontology:cs_reading_relation('6d2d78d3-21fe-43cf-a6bb-0e342665216b', jihad_quranic_corpus__expansionist_legalist_reading, forecloses).
narrative_ontology:cs_reading_relation('6d2d78d3-21fe-43cf-a6bb-0e342665216b', jihad_quranic_corpus__revolutionary_vanguard_reading, forecloses).
narrative_ontology:cs_axiom('6d2d78d3-21fe-43cf-a6bb-0e342665216b', foundational, armed_jihad_defensive_state_only).
narrative_ontology:cs_axiom_status(armed_jihad_defensive_state_only, holdable).
narrative_ontology:cs_axiom_grounding('6d2d78d3-21fe-43cf-a6bb-0e342665216b', armed_jihad_defensive_state_only, theological).
narrative_ontology:cs_axiom('6d2d78d3-21fe-43cf-a6bb-0e342665216b', foundational, non_combatant_immunity_unconditional).
narrative_ontology:cs_axiom_status(non_combatant_immunity_unconditional, holdable).
narrative_ontology:cs_axiom_grounding('6d2d78d3-21fe-43cf-a6bb-0e342665216b', non_combatant_immunity_unconditional, theological).
narrative_ontology:cs_reference_frame('6d2d78d3-21fe-43cf-a6bb-0e342665216b', classical_defensive_jihad_framework).
narrative_ontology:cs_drift_state('6d2d78d3-21fe-43cf-a6bb-0e342665216b', contemporary_post_colonial, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6d2d78d3-21fe-43cf-a6bb-0e342665216b', '').
narrative_ontology:cs_kernel_id(jihad_quranic_corpus__defensive_spiritual_reading, jihad_quranic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__defensive_spiritual_reading, muslim_adherents).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__defensive_spiritual_reading, non_muslim_communities).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__defensive_spiritual_reading, legitimate_state_authority).
narrative_ontology:constraint_vindicates(jihad_quranic_corpus__defensive_spiritual_reading, quranic_hermeneutic_priority_of_makkah_verses).
narrative_ontology:constraint_vindicates(jihad_quranic_corpus__defensive_spiritual_reading, just_war_proportionality_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Live under a jurisprudential framework that prioritizes internal spiritual struggle and permits armed action only under state authority in defense against aggression. Their religious identity binds them to the scholarly interpretation; departure from this reading risks social ostracism or theological censure, though other Islamic readings exist.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, muslim_adherents, beneficiary,
    organized, generational, identity_locked, global).

% Benefit from doctrinal rules of non-combatant immunity and proportionality that remove them from the victim set of legitimate jihad unless they participate in aggression. Their security depends on whether Muslim political authorities adhere to this reading.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, non_muslim_communities, beneficiary,
    organized, generational, constrained, global).

% Retains a juridical monopoly on declaring legitimate armed jihad under this reading. Must justify any military action as defensive and proportional. Derives legitimacy from adherence to the constraint, but is itself constrained by the same scholarly criteria.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, legitimate_state_authority, agenda_setter,
    institutional, generational, constrained, global).

% Seek to pursue offensive or individual armed jihad without state authorization. Are structurally excluded from the legitimating conversation; their preferred course of action is ruled illegitimate by the doctrinal constraint, exposing them to suppression by both state authorities and the scholarly establishment.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, unauthorized_militant_groups, excluded,
    moderate, biographical, constrained, regional).

% Interpret the sources to maintain that armed jihad is defensive and state-authorized. Their authority depends on preserving the coherence of this reading against rival expansionist and revolutionary interpretations.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, jurist_class_ulama, agenda_setter,
    organized, generational, identity_locked, global).

% Comparative religious-law scholars and political theologians who analyze the contest between jihad readings without being bound to any.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, external_observer_academics, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the Muslim community's relationship to armed force by restricting legitimate military action to defensive contexts under state authority, while channeling religious striving into internal spiritual discipline; simultaneously coordinates inter-communal relations by establishing non-combatant immunity and proportionality rules that protect non-Muslim communities.
% TRANSFER_FUNCTION: Moves the authority to declare and conduct legitimate armed jihad from individual believers and non-state actors to state institutions and recognized jurists; moves the primary religious obligation from external conquest to internal moral struggle.
% ABSENT_VOICES: Expansionist jurists and revolutionary vanguard militants who hold that offensive or individual armed jihad is licit are structurally excluded from the legitimating conversation within this reading; their objections are pre-empted by the requirement of state authority and defensive cause.
% DISAPPEARANCE_RATIONALE: If this doctrinal reading vanished, the constraint on state authority and defensive cause would vanish with it. Individual and non-state actors could claim legitimacy for offensive campaigns; non-Muslim communities would lose the structural protection of non-combatant immunity; the scholarly-juristic order would lose a major source of its authority. The world of Islamic jurisprudence and political theology would rearrange around the remaining readings.
% FOUNDING_PROBLEM: How to regulate the use of force within a religious community spanning diverse polities, and how to prevent the dissolution of moral boundaries between combatants and non-combatants in warfare.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by historians of Islamic law and independent political theologians outside the beneficiary set, who attest that the classical jurisprudential effort to restrain irregular warfare and protect non-combatants responded to genuine coordination failures in early and medieval Islamic political orders. Contemporary human rights scholars also attest the continued relevance of these restraints.
narrative_ontology:disappearance_verdict(jihad_quranic_corpus__defensive_spiritual_reading, world_rearranges).
narrative_ontology:founding_problem_status(jihad_quranic_corpus__defensive_spiritual_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jihad_quranic_corpus__defensive_spiritual_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jihad_quranic_corpus__defensive_spiritual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jihad_quranic_corpus__defensive_spiritual_reading, 0.22, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is authored low (0.22) because the reading's core function is to limit and regulate violence rather than extract resources or status asymmetrically. Suppression (0.35) reflects the active social and political enforcement needed to maintain the state-authority monopoly against individual and non-state militant challengers. Theater ratio (0.12) is low because the spiritual and jurisprudential functions are substantively performed. Resistance (0.42) captures the live contest from expansionist and revolutionary sibling readings. Accessibility collapse (0.45) is moderate: for adherents, alternative readings are epistemically available but normatively collapsed; for non-adherents, the reading is one option among several. The measurement series share a single time grid so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (state authority, ulama) experience the constraint as a source of legitimacy and juridical order; the excluded militant seat experiences it as suppression of their preferred course of action. The engine will compute different per-seat types from these structural relationships. Muslim adherents as identity-locked beneficiaries will compute toward rope; unauthorized militants as constrained excluded actors would experience high directionality, but because they are not declared victims and the constraint does not extract from them, their exclusion registers as suppression rather than extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Muslim adherents and non-Muslim communities are declared beneficiaries, deriving low directionality and damped effective extraction. State authority and jurist class are agenda-setters with constrained or identity-locked exit, deriving moderate directionality because they are symmetrically bound by the rules they administer and interpret. Unauthorized militant groups are excluded with constrained exit, deriving high directionality toward target, but they are not in the victim array because the constraint suppresses rather than extracts from them. The structural derivation should place beneficiaries near d=0.0 and the agenda-setters near d=0.4, while excluded suppressees sit near d=0.9 without triggering victim-derived extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â regulating force and protecting non-combatants â remains live (founding_problem_status: live), so mandatrophy is not declared. If the state authority requirement were to atrophy into a mere justification for regime security while the spiritual dimension vanished, the constraint would degrade toward piton or snare. Currently, both dimensions remain substantively performed, and the metrics reflect genuine coordination rather than theatrical maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    state_monopoly_extraction_risk,
    'Does the state-authority requirement function as coordination (preventing vigilantism) or as extraction (consolidating power in regimes that claim Islamic legitimacy)?',
    'Compare jurisdictions where state authority is democratic or accountable versus authoritarian; measure whether the constraint restrains state aggression or enables it.',
    'If the state-authority gate is captured by authoritarian regimes for regime security, the constraint reclassifies as tangled_rope with legitimate_state_authority as concentrated beneficiary and muslim_adherents as diffuse payer. If it restrains state aggression, the rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_monopoly_extraction_risk, empirical, 'Whether state authority functions as coordination or extraction.').

omega_variable(
    hermeneutic_stability,
    'Is the defensive/spiritual reading a stable feature of the Quranic corpus, or does it depend on suppressing or contextualizing verses that expansionist readings foreground?',
    'Historical-textual analysis of tafsir evolution and verse-abrogation debates across classical and modern periods.',
    'If the reading requires systematic suppression of textual evidence, its accessibility_collapse is higher and its rope-status is weakened toward tangled_rope or snare. If it is independently coherent, the low-extraction profile is corroborated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hermeneutic_stability, conceptual, 'Whether the reading is textually stable or selectively suppressive.').

omega_variable(
    kernel_reading_foreclosure,
    'This reading declares foreclosure against both sibling readings, but classical fiqh historically compartmentalized defensive and offensive obligations. Is the foreclosure relation overstated?',
    'Examine whether classical madhabs permitted compartmentalization of defensive and offensive jihad obligations within a single jurist''s framework.',
    'If compartmentalization is possible, the relation should be coexists_with rather than forecloses; the constraint''s purity as rope would be unaffected but its boundary would blur, affecting network contamination analysis.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether foreclosure against sibling readings is logically airtight.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jihad_quranic_corpus__defensive_spiritual_reading, 0, 125).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jiha_tr_t0, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(jiha_tr_t25, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 25, 0.11).
narrative_ontology:measurement(jiha_tr_t50, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 50, 0.1).
narrative_ontology:measurement(jiha_tr_t75, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 75, 0.12).
narrative_ontology:measurement(jiha_tr_t100, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 100, 0.11).
narrative_ontology:measurement(jiha_tr_t125, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 125, 0.12).

% Extraction over time
narrative_ontology:measurement(jiha_be_t0, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(jiha_be_t25, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 25, 0.16).
narrative_ontology:measurement(jiha_be_t50, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 50, 0.18).
narrative_ontology:measurement(jiha_be_t75, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 75, 0.19).
narrative_ontology:measurement(jiha_be_t100, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 100, 0.21).
narrative_ontology:measurement(jiha_be_t125, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 125, 0.22).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(jihad_quranic_corpus__defensive_spiritual_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jihad_quranic_corpus__defensive_spiritual_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jihad_quranic_corpus__defensive_spiritual_reading, jihad_quranic_corpus__expansionist_legalist_reading).
narrative_ontology:affects_constraint(jihad_quranic_corpus__defensive_spiritual_reading, jihad_quranic_corpus__revolutionary_vanguard_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the jihad_quranic_corpus kernel, which decomposes into at least three structurally distinct constraints. Each reading has a different epsilon, beneficiary/victim structure, and classification. This reading claims rope; the expansionist reading likely computes as tangled_rope or snare; the revolutionary reading likely computes as snare.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
