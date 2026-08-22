% ============================================================================
% CONSTRAINT STORY: jihad_quranic_corpus__defensive_spiritual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: Quranic Jihad as Spiritual Struggle and Defensive Armed Response (Proportionality-Bound)
 *   domain: religious_law/political_theology
 *
 * SUMMARY:
 *   This constraint story instantiates the defensive-spiritual reading of the
 *   Quranic jihad corpus: jihad is primarily the internal spiritual struggle
 *   (jihad al-nafs), with armed jihad permitted only as a collective
 *   obligation (fard kifaya) under legitimate state authority, strictly
 *   defensive, and bound by proportionality and non-combatant immunity.
 *   Non-Muslims are outside the victim set unless they are aggressors;
 *   coexistence (ta'ayush) is the privileged framework. This reading competes
 *   with the expansionist legalist reading (offensive jihad to establish
 *   Islamic governance) and the revolutionary vanguard reading (immediate
 *   individual obligation against apostate rulers/occupiers via takfir).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jihad_quranic_corpus__defensive_spiritual_reading, 0.18).
domain_priors:suppression_score(jihad_quranic_corpus__defensive_spiritual_reading, 0.12).
domain_priors:theater_ratio(jihad_quranic_corpus__defensive_spiritual_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jihad_quranic_corpus__defensive_spiritual_reading, rope).
narrative_ontology:human_readable(jihad_quranic_corpus__defensive_spiritual_reading, "Quranic Jihad as Spiritual Struggle and Defensive Armed Response (Proportionality-Bound)").
narrative_ontology:topic_domain(jihad_quranic_corpus__defensive_spiritual_reading, "religious_law/political_theology").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jihad_quranic_corpus__defensive_spiritual_reading, '9ce5f7f1-e3ae-4dff-a161-1d7e86dc3010').
narrative_ontology:cs_kernel_codification('9ce5f7f1-e3ae-4dff-a161-1d7e86dc3010', fixed_text).
narrative_ontology:cs_authority_grounding('9ce5f7f1-e3ae-4dff-a161-1d7e86dc3010', lineage).
narrative_ontology:cs_interpretation_layer_present('9ce5f7f1-e3ae-4dff-a161-1d7e86dc3010').
narrative_ontology:cs_reading_relation('9ce5f7f1-e3ae-4dff-a161-1d7e86dc3010', jihad_quranic_corpus__expansionist_legalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('9ce5f7f1-e3ae-4dff-a161-1d7e86dc3010', jihad_quranic_corpus__revolutionary_vanguard_reading, forecloses).
narrative_ontology:cs_axiom('9ce5f7f1-e3ae-4dff-a161-1d7e86dc3010', foundational, armed_jihad_requires_legitimate_state_authority).
narrative_ontology:cs_axiom_status(armed_jihad_requires_legitimate_state_authority, holdable).
narrative_ontology:cs_axiom_grounding('9ce5f7f1-e3ae-4dff-a161-1d7e86dc3010', armed_jihad_requires_legitimate_state_authority, conventional).
narrative_ontology:cs_axiom('9ce5f7f1-e3ae-4dff-a161-1d7e86dc3010', foundational, non_combatant_immunity_is_absolute_in_islamic_law).
narrative_ontology:cs_axiom_status(non_combatant_immunity_is_absolute_in_islamic_law, holdable).
narrative_ontology:cs_axiom_grounding('9ce5f7f1-e3ae-4dff-a161-1d7e86dc3010', non_combatant_immunity_is_absolute_in_islamic_law, deontological).
narrative_ontology:cs_axiom('9ce5f7f1-e3ae-4dff-a161-1d7e86dc3010', foundational, jihad_al_nafs_is_primary_continuous_obligation).
narrative_ontology:cs_axiom_status(jihad_al_nafs_is_primary_continuous_obligation, holdable).
narrative_ontology:cs_axiom_grounding('9ce5f7f1-e3ae-4dff-a161-1d7e86dc3010', jihad_al_nafs_is_primary_continuous_obligation, deontological).
narrative_ontology:cs_reference_frame('9ce5f7f1-e3ae-4dff-a161-1d7e86dc3010', prophetic_defensive_model).
narrative_ontology:cs_drift_state('9ce5f7f1-e3ae-4dff-a161-1d7e86dc3010', contemporary_international_law_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9ce5f7f1-e3ae-4dff-a161-1d7e86dc3010', '').
narrative_ontology:cs_kernel_id(jihad_quranic_corpus__defensive_spiritual_reading, jihad_quranic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__defensive_spiritual_reading, muslim_communities_under_threat).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__defensive_spiritual_reading, non_muslim_civilians_under_islamic_rule).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__defensive_spiritual_reading, islamic_legal_tradition).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(jihad_quranic_corpus__defensive_spiritual_reading, state_authorities_muslim_majority).
narrative_ontology:constraint_vindicates(jihad_quranic_corpus__defensive_spiritual_reading, proportionality_in_armed_conflict).
narrative_ontology:constraint_vindicates(jihad_quranic_corpus__defensive_spiritual_reading, non_combatant_immunity).
narrative_ontology:constraint_vindicates(jihad_quranic_corpus__defensive_spiritual_reading, spiritual_primacy_over_violent_action).
narrative_ontology:constraint_vindicates(jihad_quranic_corpus__defensive_spiritual_reading, state_monopoly_on_legitimate_force).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret the Quranic corpus through the four Sunni madhhabs and Shi'i usul al-fiqh, establishing that armed jihad is collective obligation (fard kifaya) only under legitimate state authority, strictly defensive, and constrained by proportionality and non-combatant immunity. Their interpretive authority derives from chains of transmission (isnad) and methodological coherence.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, classical_jurists_traditionalist, agenda_setter,
    institutional, civilizational, analytical, universal).

% Communities facing aggression receive a juristically grounded framework for legitimate self-defense that limits escalation, protects civilians, and requires state authorization — preventing unauthorized violence that would invite disproportionate retaliation.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, muslim_communities_under_threat, beneficiary,
    moderate, generational, constrained, regional).

% Protected by the non-combatant immunity rule (siyar) and dhimma provisions that this reading treats as binding Islamic law, not revocable policy. Their protection is structurally built into the constraint's operational logic.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, non_muslim_civilians_under_islamic_rule, beneficiary,
    powerless, biographical, constrained, local).

% The tradition itself benefits from a reading that resolves the tension between Quranic verses on fighting and the broader ethical trajectory toward coexistence, preserving the corpus's coherence against both quietist and expansionist fragmentations.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, islamic_legal_tradition, beneficiary,
    organized, civilizational, analytical, universal).

% Hold the monopoly on legitimate declaration of armed jihad, bearing the political and military costs of defensive campaigns. Their authority is both empowered and constrained by the reading: they alone can authorize, but only under strict conditions they cannot unilaterally relax.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, state_authorities_muslim_majority, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(jihad_quranic_corpus__defensive_spiritual_reading, state_authorities_muslim_majority, payer).

% Maintain a jurisprudential framework permitting offensive jihad to establish Islamic governance, grounded in classical fiqh categories (dar al-islam/dar al-harb, invitation to Islam, imam authority). They are not in the conversation of this reading — their framework treats defensive-only as an abrogated or contextual position.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, expansionist_jurists_legalist, excluded,
    institutional, civilizational, identity_locked, universal).

% Declare individual offensive jihad against rulers deemed apostate and foreign occupiers, bypassing state authority through takfir and emergency jurisprudence. They treat the defensive-spiritual reading as quietist deviation. Their exclusion is structural: this reading's state-authority requirement directly forecloses their legitimacy.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, revolutionary_vanguard_actors, excluded,
    organized, immediate, identity_locked, global).

% Analyze the constraint from within the tradition using historical-critical methods, emphasizing the Meccan/Medinan chronology, the Prophet's defensive wars, and the ethical trajectory toward religious freedom (la ikraha fi al-din). They neither set the agenda nor pay the costs.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, contemporary_reformist_scholars, observer,
    moderate, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the Muslim community's response to aggression by channeling legitimate force exclusively through state authority, under strict proportionality and civilian immunity rules, while centering the internal spiritual struggle (jihad al-nafs) as the primary, continuous obligation — solving the collective-action problem of when and how force may be used without fracturing the community into competing warlords or vigilante actors.
% TRANSFER_FUNCTION: Transfers the burden of defensive violence from the community onto legitimate state structures (which bear the political/military cost), while transferring protection to non-combatants (Muslim and non-Muslim alike) through binding non-combatant immunity rules. The spiritual struggle absorbs the community's martial energy into self-purification rather than external conquest.
% ABSENT_VOICES: The expansionist legalist jurists (classical fiqh of offensive jihad) and revolutionary vanguard ideologues (individual obligation, takfir-based) are structurally excluded — their frameworks are logically incompatible with the state-authority and defensive-only premises of this reading. Also absent: non-Muslim populations outside historical Islamic rule who would be affected by the expansionist reading's offensive campaigns but are protected by this reading's non-combatant immunity and coexistence framework.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the constraint's coordinating function would collapse: state monopoly on legitimate force would erode, non-combatant immunity would lose its strongest juristic grounding in the tradition, and the community would fracture between quietist withdrawal and competing armed actors claiming individual obligation. The spiritual primacy of jihad al-nafs would be displaced by militarized frameworks.
% FOUNDING_PROBLEM: The early Muslim community faced existential military threats (Meccan persecution, Medinan battles) while receiving Quranic verses permitting fighting (qital) under strict conditions. The founding problem was: how to authorize necessary defensive violence without legitimizing unrestrained conquest, vigilante action, or the displacement of spiritual struggle by martial virtue — preserving the Quranic ethical trajectory toward justice and coexistence.
% FOUNDING_PROBLEM_CORROBORATION: Classical jurists across madhhabs (Hanafi, Maliki, Shafi'i, Hanbali, Ja'fari) attest the defensive-only, state-authorized framework as the dominant historical position, documented in siyar literature and usul al-fiqh. Contemporary international lawyers of Islamic law (e.g., Khaled Abou El Fadl, Abdullahi An-Na'im) corroborate from outside the traditionalist beneficiary set. The expansionist and revolutionary readings themselves implicitly corroborate by defining themselves against this framework.
narrative_ontology:disappearance_verdict(jihad_quranic_corpus__defensive_spiritual_reading, world_rearranges).
narrative_ontology:founding_problem_status(jihad_quranic_corpus__defensive_spiritual_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jihad_quranic_corpus__defensive_spiritual_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(jihad_quranic_corpus__defensive_spiritual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jihad_quranic_corpus__defensive_spiritual_reading, 0.18, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Low extractiveness (0.18) reflects that the constraint primarily channels and limits violence rather than extracting resources. Low suppression (0.12) because enforcement is through juristic consensus and state monopoly, not coercion of dissenters. Very low theater (0.08) because the spiritual and defensive functions are genuine, not performative. Moderate accessibility_collapse (0.35) because alternative readings persist vigorously. Moderate resistance (0.42) because the reading faces active contestation from expansionist and revolutionary frameworks. The claimed_type 'rope' reflects genuine coordination: solving the collective-action problem of legitimate force without extracting from the coordinated parties.
 *
 * PERSPECTIVAL GAP:
 *   The state authority seat and the vulnerable civilian seat experience the constraint differently: the state sees a burdensome obligation with strict conditions; the civilian sees protective immunity. The classical jurist sees methodological coherence; the revolutionary vanguard sees quietist betrayal. The engine computes these divergences from power/exit/spatial_scope — this reading's structural data makes the state a constrained agenda-setter, not an extractive one.
 *
 * DIRECTIONALITY LOGIC:
 *   State authorities are dual-positioned: agenda_setter (monopoly on declaration) and payer (bear costs of defense) — d near symmetric. Muslim communities under threat and non-Muslim civilians are beneficiaries (d near 0.0). Classical jurists and the legal tradition are analytical/beneficiary (d low). Expansionist jurists and revolutionary vanguards are excluded — their exit is identity_locked because their frameworks structurally require rejecting this reading's core premises. The directionality derivation captures this: beneficiaries collect protection/coordination; payers bear costs; excluded agents are structurally foreclosed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (existential defense with ethical limits) remains live — aggression against Muslim communities persists, and the need to channel defensive force through legitimate authority with civilian protection remains unresolved. No mandatrophy: the constraint's function has not atrophied. The expansionist and revolutionary readings are live contestations, not evidence of obsolescence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_foreclosure_vs_coexistence,
    'Does this reading''s state-authority requirement logically foreclose the revolutionary vanguard reading within a single framework, or do they coexist as competing positions held by different parties?',
    'Analyze whether a single juristic framework could simultaneously hold: (a) only the state may declare armed jihad, and (b) individuals may declare jihad against rulers deemed apostate. If the framework''s logic treats (b) as necessarily invalidating (a), foreclosure holds; if it treats them as distinct positions in an unresolved dispute, coexistence holds.',
    'If forecloses, the engine registers a structural contradiction between readings; if coexists_with, the kernel carries live pluralism. This affects whether the constraint family shows contamination or stable competition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_vs_coexistence, conceptual, 'Whether state-authority premise forecloses individual-obligation premise in one framework').

omega_variable(
    expansionist_historical_dominance,
    'Was the expansionist legalist reading historically dominant in classical fiqh (pre-modern), making this reading a modern revision — or was the defensive-only position the mainstream with expansionist doctrines as specialized contexts?',
    'Historiography of siyar literature across madhhabs: survey of whether offensive jihad was treated as standing obligation or contextual suspension of defensive norm. Corroboration from non-traditionalist scholars.',
    'If expansionist was dominant, this reading''s claimed_type ''rope'' (coordination) may be a modern reconstruction; if defensive was dominant, the expansionist reading is the deviation. Affects mandatrophy and historical authenticity claims.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(expansionist_historical_dominance, empirical, 'Historical dominance of expansionist vs. defensive reading in classical jurisprudence').

omega_variable(
    spiritual_primacy_operationalization,
    'Does the primacy of jihad al-nafs (spiritual struggle) have operational force in this reading — does it structurally limit armed jihad — or is it a rhetorical framing with no constraining effect on the defensive warfare rules?',
    'Test whether jurists citing jihad al-nafs primacy produce different fatwas on defensive warfare thresholds than those who don''t. Check if spiritual primacy correlates with stricter proportionality/civilian immunity rulings.',
    'If operational, the low extractiveness is structurally grounded; if rhetorical, the low extractiveness may be fragile — a thin veneer over expansionist potential. Affects whether the rope classification is stable or a false summit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(spiritual_primacy_operationalization, empirical, 'Whether spiritual primacy functionally constrains armed jihad rules').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jihad_quranic_corpus__defensive_spiritual_reading, 610, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jiha_tr_t610, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 610, 0.15).
narrative_ontology:measurement(jiha_tr_t661, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 661, 0.08).
narrative_ontology:measurement(jiha_tr_t750, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 750, 0.05).
narrative_ontology:measurement(jiha_tr_t1100, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 1100, 0.04).
narrative_ontology:measurement(jiha_tr_t1500, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 1500, 0.06).
narrative_ontology:measurement(jiha_tr_t1800, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 1800, 0.09).
narrative_ontology:measurement(jiha_tr_t1924, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 1924, 0.18).
narrative_ontology:measurement(jiha_tr_t1979, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 1979, 0.22).
narrative_ontology:measurement(jiha_tr_t2001, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 2001, 0.2).
narrative_ontology:measurement(jiha_tr_t2024, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 2024, 0.08).

% Extraction over time
narrative_ontology:measurement(jiha_be_t610, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 610, 0.25).
narrative_ontology:measurement(jiha_be_t661, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 661, 0.15).
narrative_ontology:measurement(jiha_be_t750, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 750, 0.12).
narrative_ontology:measurement(jiha_be_t1100, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 1100, 0.1).
narrative_ontology:measurement(jiha_be_t1500, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 1500, 0.08).
narrative_ontology:measurement(jiha_be_t1800, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 1800, 0.12).
narrative_ontology:measurement(jiha_be_t1924, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 1924, 0.22).
narrative_ontology:measurement(jiha_be_t1979, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 1979, 0.28).
narrative_ontology:measurement(jiha_be_t2001, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 2001, 0.25).
narrative_ontology:measurement(jiha_be_t2024, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 2024, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(jiha_su_t610, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 610, 0.3).
narrative_ontology:measurement(jiha_su_t661, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 661, 0.12).
narrative_ontology:measurement(jiha_su_t750, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 750, 0.08).
narrative_ontology:measurement(jiha_su_t1100, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 1100, 0.06).
narrative_ontology:measurement(jiha_su_t1500, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 1500, 0.05).
narrative_ontology:measurement(jiha_su_t1800, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 1800, 0.08).
narrative_ontology:measurement(jiha_su_t1924, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 1924, 0.18).
narrative_ontology:measurement(jiha_su_t1979, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 1979, 0.25).
narrative_ontology:measurement(jiha_su_t2001, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 2001, 0.22).
narrative_ontology:measurement(jiha_su_t2024, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 2024, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jihad_quranic_corpus__defensive_spiritual_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jihad_quranic_corpus__defensive_spiritual_reading, 0.06).
narrative_ontology:affects_constraint(jihad_quranic_corpus__defensive_spiritual_reading, jihad_quranic_corpus__expansionist_legalist_reading).
narrative_ontology:affects_constraint(jihad_quranic_corpus__defensive_spiritual_reading, jihad_quranic_corpus__revolutionary_vanguard_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the jihad_quranic_corpus kernel. This reading (defensive_spiritual) has ε=0.18, claimed_type=rope. The expansionist_legalist_reading has higher extractiveness (offensive campaigns transfer resources/territory) and would likely classify as tangled_rope or snare. The revolutionary_vanguard_reading has highest extractiveness (individual obligation bypassing state, takfir enabling unbounded targeting) and would likely classify as snare. All three share the same textual corpus but instantiate different constraints with different beneficiary/victim structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jihad_quranic_corpus__defensive_spiritual_reading, institutional, 0.45).
constraint_indexing:directionality_override(jihad_quranic_corpus__defensive_spiritual_reading, moderate, 0.15).
constraint_indexing:directionality_override(jihad_quranic_corpus__defensive_spiritual_reading, powerless, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
