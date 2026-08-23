% ============================================================================
% CONSTRAINT STORY: jihad_quranic_corpus__defensive_spiritual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-09
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
 *   constraint_id: jihad_quranic_corpus__defensive_spiritual_reading
 *   human_readable: Defensive Spiritual Jihad Reading (Quranic Corpus)
 *   domain: religious/political_theology
 *
 * SUMMARY:
 *   This constraint instantiates the defensive_spiritual_reading of the
 *   contested kernel jihad_quranic_corpus. It treats jihad as primarily
 *   internal spiritual discipline (jihad al-nafs) and secondarily as
 *   defensive armed response to aggression, constrained by proportionality,
 *   non-combatant immunity, and legitimate state authority. The reading is
 *   one of three live positions in the kernel; sibling readings
 *   (expansionist_legalist_reading and revolutionary_vanguard_reading)
 *   produce structurally different constraints from the same textual corpus.
 *   This story isolates the defensive reading as a clean epsilon-invariant
 *   constraint: its referent is the standing doctrinal arrangement as this
 *   reading constructs it, not the reading's endorsed alternative.
 *
 * KEY AGENTS:
 *   - Islamic jurists (institutional/agenda_setter) â maintain interpretive tradition and enforce state-authority requirement
 *   - Legitimate state authorities (institutional/agenda_setter+beneficiary) â monopolize declaration of armed jihad and administer proportionality constraints
 *   - Civilian populations (powerless/beneficiary) â receive non-combatant immunity and proportionality protection
 *   - Muslim communities (organized/beneficiary) â coordinated by ethical framework privileging spiritual struggle
 *   - Non-state militant groups (moderate/payer) â bear cost of delegitimization and loss of autonomous military authority
 *   - Expansionist political factions (organized/payer) â constrained from offensive campaigns to establish Islamic governance
 *   - Revolutionary vanguard theologians (moderate/excluded) â excluded from authoritative interpretive conversation
 *   - Comparative religious scholars (analytical/observer) â analytical seat tracing structural divergence across readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jihad_quranic_corpus__defensive_spiritual_reading, 0.58).
domain_priors:suppression_score(jihad_quranic_corpus__defensive_spiritual_reading, 0.65).
domain_priors:theater_ratio(jihad_quranic_corpus__defensive_spiritual_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jihad_quranic_corpus__defensive_spiritual_reading, tangled_rope).
narrative_ontology:human_readable(jihad_quranic_corpus__defensive_spiritual_reading, "Defensive Spiritual Jihad Reading (Quranic Corpus)").
narrative_ontology:topic_domain(jihad_quranic_corpus__defensive_spiritual_reading, "religious/political_theology").

domain_priors:requires_active_enforcement(jihad_quranic_corpus__defensive_spiritual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jihad_quranic_corpus__defensive_spiritual_reading, '323e4376-587e-463a-903e-5c050fcf4057').
narrative_ontology:cs_kernel_codification('323e4376-587e-463a-903e-5c050fcf4057', fixed_text).
narrative_ontology:cs_authority_grounding('323e4376-587e-463a-903e-5c050fcf4057', lineage).
narrative_ontology:cs_interpretation_layer_present('323e4376-587e-463a-903e-5c050fcf4057').
narrative_ontology:cs_reading_relation('323e4376-587e-463a-903e-5c050fcf4057', jihad_quranic_corpus__expansionist_legalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('323e4376-587e-463a-903e-5c050fcf4057', jihad_quranic_corpus__revolutionary_vanguard_reading, forecloses).
narrative_ontology:cs_axiom('323e4376-587e-463a-903e-5c050fcf4057', foundational, jihad_al_nafs_primacy).
narrative_ontology:cs_axiom_status(jihad_al_nafs_primacy, holdable).
narrative_ontology:cs_axiom_grounding('323e4376-587e-463a-903e-5c050fcf4057', jihad_al_nafs_primacy, theological).
narrative_ontology:cs_axiom('323e4376-587e-463a-903e-5c050fcf4057', foundational, state_authority_monopoly_on_armed_jihad).
narrative_ontology:cs_axiom_status(state_authority_monopoly_on_armed_jihad, holdable).
narrative_ontology:cs_axiom_grounding('323e4376-587e-463a-903e-5c050fcf4057', state_authority_monopoly_on_armed_jihad, theological).
narrative_ontology:cs_reference_frame('323e4376-587e-463a-903e-5c050fcf4057', classical_defensive_jihad_consensus).
narrative_ontology:cs_drift_state('323e4376-587e-463a-903e-5c050fcf4057', contemporary_post_colonial_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('323e4376-587e-463a-903e-5c050fcf4057', '').
narrative_ontology:cs_kernel_id(jihad_quranic_corpus__defensive_spiritual_reading, jihad_quranic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__defensive_spiritual_reading, civilian_populations).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__defensive_spiritual_reading, legitimate_state_authorities).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__defensive_spiritual_reading, muslim_communities).
narrative_ontology:constraint_victim(jihad_quranic_corpus__defensive_spiritual_reading, non_state_militant_groups).
narrative_ontology:constraint_victim(jihad_quranic_corpus__defensive_spiritual_reading, expansionist_political_factions).
narrative_ontology:constraint_vindicates(jihad_quranic_corpus__defensive_spiritual_reading, just_war_proportionality).
narrative_ontology:constraint_vindicates(jihad_quranic_corpus__defensive_spiritual_reading, non_combatant_immunity_islamic_law).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain and transmit the interpretive tradition that distinguishes spiritual from armed jihad, set the conditions for legitimate defensive war, and enforce the state-authority requirement through fatwa, madhhab consensus, and canonical jurisprudential methodology. Their authority depends on continuity with classical sources and the interpretive community.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, islamic_jurists, agenda_setter,
    institutional, generational, constrained, global).

% Claim and administer the monopoly on declaring armed jihad, enforce non-combatant immunity through military law and command structures, and justify campaigns through scholarly consultation. Departing from the doctrinal framework undermines domestic and international legitimacy. Benefit from the delegitimization of non-state violence.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, legitimate_state_authorities, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(jihad_quranic_corpus__defensive_spiritual_reading, legitimate_state_authorities, beneficiary).

% Receive protection from non-combatant immunity and proportionality constraints when this reading is operative; benefit from the high threshold that limits declared wars. Cannot opt out of the doctrinal framework governing the territory or conflict zone they inhabit.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, civilian_populations, beneficiary,
    powerless, immediate, trapped, local).

% Coordinated by a clear ethical framework that privileges spiritual struggle and regulates communal violence. Protected from the chaos of unregulated militant violence. Religious identity is partly constituted by adherence to this normative structure, making exit from the doctrinal frame costly.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, muslim_communities, beneficiary,
    organized, biographical, identity_locked, global).

% Denied religious legitimacy for autonomous armed campaigns by the state-authority requirement. Must either submit to state command or be classified as illegitimate rebels or bandits. Bear the cost of lost autonomy, delegitimization, and potential military suppression.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, non_state_militant_groups, payer,
    moderate, immediate, constrained, regional).

% Constrained from offensive campaigns to establish Islamic governance by the defensive-only framework and state-authority requirement. Their political theology is ruled illegitimate within this reading, forcing them into silence, exile, or clandestine organization.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, expansionist_political_factions, payer,
    organized, biographical, constrained, national).

% Reject the state-authority requirement and claim individual obligation to armed jihad via takfir. Are excluded from authoritative interpretive conversation and scholarly consensus mechanisms. Their identity is constituted by opposition to this constraint.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, revolutionary_vanguard_theologians, excluded,
    moderate, biographical, identity_locked, regional).

% Analytical seat tracing how the same textual kernel emits structurally different constraints across readings. Observe the divergence between the defensive, expansionist, and vanguard framings without being governed by any.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, comparative_religious_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jihad_quranic_corpus__defensive_spiritual_reading, legitimate_state_authorities).
narrative_ontology:fixing_cost_class(jihad_quranic_corpus__defensive_spiritual_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Regulates the use of force within Islamic normative framework by prioritizing spiritual self-discipline (jihad al-nafs), restricting legitimate armed conflict to defensive responses to aggression, and imposing proportionality and non-combatant immunity on any authorized campaign.
% TRANSFER_FUNCTION: Moves authority to declare and conduct armed jihad from non-state actors and individuals to state authorities and scholarly consensus; moves risk of arbitrary violence away from civilian populations toward combatant aggressors; transfers spiritual priority from external military action to internal ethical formation.
% ABSENT_VOICES: Revolutionary vanguard theologians who reject state authority and claim individual obligation to armed jihad; expansionist political theologians who advocate offensive campaigns to establish Islamic governance globally. Both are structurally excluded from the authoritative interpretive conversation that produces this reading.
% DISAPPEARANCE_RATIONALE: If this doctrinal constraint vanished overnight, the restriction on offensive war, the requirement of state authority, and the non-combatant immunity framework would collapse. Non-state actors would claim unilateral jihad legitimacy, civilian protections would erode, the legitimizing framework for state monopoly on violence would dissolve, and the prioritization of spiritual struggle would yield to militarized readings.
% FOUNDING_PROBLEM: How to regulate the early Muslim community's use of force in contexts of persecution, migration, and state-building while maintaining ethical boundaries, community cohesion, and distinguishing legitimate resistance from brigandage.
% FOUNDING_PROBLEM_CORROBORATION: Islamic jurists attest the founding problem remains live in contexts of occupation and aggression. Political scientists, historians, and human rights observers outside the benefiting parties note that the classical formulation addressed a specific 7th-8th century context and that modern geopolitical conditions have substantially altered the problem-space; some corroborate the continuity, others dispute it.
narrative_ontology:disappearance_verdict(jihad_quranic_corpus__defensive_spiritual_reading, world_rearranges).
narrative_ontology:founding_problem_status(jihad_quranic_corpus__defensive_spiritual_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jihad_quranic_corpus__defensive_spiritual_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jihad_quranic_corpus__defensive_spiritual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jihad_quranic_corpus__defensive_spiritual_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jihad_quranic_corpus__defensive_spiritual_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jihad_quranic_corpus__defensive_spiritual_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jihad_quranic_corpus__defensive_spiritual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) is moderate-to-substantial because the state-authority requirement asymmetrically transfers military legitimacy from non-state actors to state institutions; the genuine coordination function (civilian protection, ethical clarity, spiritual prioritization) coexists with this extraction. Suppression (0.65) reflects active scholarly and state enforcement against competing vanguard and expansionist readings. Theater ratio (0.38) captures the growing performative dimension in modern interstate warfare, where states cite defensive jihad and proportionality while operating outside classical constraints. Accessibility collapse (0.52) is moderate: the vanguard reading is knowable but socially and legally dangerous to articulate in communities adhering to this reading. Resistance (0.55) reflects persistent challenge from non-state militant movements and post-colonial revisionist theologians. Measurements share a single time grid spanning the formative-to-contemporary interval.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (state authorities and jurists) experiences the constraint as legitimate coordination they maintain; the payer seat (non-state militants and expansionist factions) experiences the same structure as active delegitimization. Civilian beneficiaries experience protection without agency to alter the doctrinal framework. The engine computes these divergences from the structural data: beneficiary status, trapped/identity-locked exit, and power asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   State authorities and jurists derive low d (near-beneficiary) because they are declared beneficiaries/agenda-setters with generational time horizons. Non-state militants derive high d (near-target) because they are declared victims with constrained exit. Civilian populations are declared beneficiaries but powerless and trapped; the beneficiary declaration should dominate, placing them nearer the subsidy end than their structural vulnerability alone would suggest. Muslim communities are beneficiaries with identity-locked exit, placing them in a fused position where the constraint subsidizes their identity structure.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint prevents mislabeling in two directions. Without the coordination-function acknowledgment, the state monopoly on jihad-declaration could be read as pure snare (state extracting military autonomy from society). Without the extraction acknowledgment, the non-combatant immunity and proportionality rules could be read as pure rope or even mountain (natural law of just war). The tangled_rope classification holds because the same structure that protects civilians and spiritualizes struggle also concentrates military legitimacy in state hands, and this concentration requires active scholarly and coercive enforcement to maintain against competing readings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Does the defensive/spiritual reading exhaust the quranic jihad corpus, or does the same textual kernel sustain the expansionist and vanguard sibling readings with equal hermeneutic validity?',
    'Comparative hermeneutic analysis and historical sociology of which communities can stably maintain which reading under what political conditions.',
    'If the kernel sustains all three readings equally, none is structurally inevitable and classification varies by community; if this reading is textually privileged, it approaches Mountain-like stability in its adherents'' framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether the kernel deterministically selects this reading or underdetermines the contest.').

omega_variable(
    state_authority_naturalness,
    'Is the state monopoly on armed jihad a natural feature of classical Islamic jurisprudence or a constructed consolidation benefiting modern territorial states?',
    'Historical jurisprudential archaeology of the imam/caliph requirement across madhhabs and pre-modern political fragmentation.',
    'If constructed/modern, the state-authority requirement is a Tangled Rope extraction benefiting state actors; if classical/natural, it is a Mountain-like feature of the tradition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_authority_naturalness, empirical, 'Natural vs constructed status of the state authority requirement.').

omega_variable(
    non_combatant_immunity_scope,
    'Does non-combatant immunity in this reading protect all civilians absolutely, or does it collapse under asymmetric warfare conditions where combatant/civilian distinction blurs?',
    'Empirical track record of this reading''s application in modern conflicts and fatwa corpora regarding collateral damage.',
    'If immunity collapses operationally, the coordination function is weaker and extraction (theater) higher than claimed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_combatant_immunity_scope, empirical, 'Operational stability of non-combatant immunity under modern warfare.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jihad_quranic_corpus__defensive_spiritual_reading, 0, 1400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jihad_ds_tr_t0, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(jihad_ds_tr_t280, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 280, 0.2).
narrative_ontology:measurement(jihad_ds_tr_t560, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 560, 0.25).
narrative_ontology:measurement(jihad_ds_tr_t840, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 840, 0.3).
narrative_ontology:measurement(jihad_ds_tr_t1120, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 1120, 0.35).
narrative_ontology:measurement(jihad_ds_tr_t1400, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 1400, 0.38).

% Extraction over time
narrative_ontology:measurement(jihad_ds_be_t0, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(jihad_ds_be_t280, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 280, 0.45).
narrative_ontology:measurement(jihad_ds_be_t560, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 560, 0.5).
narrative_ontology:measurement(jihad_ds_be_t840, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 840, 0.48).
narrative_ontology:measurement(jihad_ds_be_t1120, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 1120, 0.55).
narrative_ontology:measurement(jihad_ds_be_t1400, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 1400, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(jihad_ds_su_t0, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(jihad_ds_su_t280, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 280, 0.5).
narrative_ontology:measurement(jihad_ds_su_t560, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 560, 0.45).
narrative_ontology:measurement(jihad_ds_su_t840, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 840, 0.5).
narrative_ontology:measurement(jihad_ds_su_t1120, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 1120, 0.6).
narrative_ontology:measurement(jihad_ds_su_t1400, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 1400, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jihad_quranic_corpus__defensive_spiritual_reading, identity_coordination).
narrative_ontology:affects_constraint(jihad_quranic_corpus__defensive_spiritual_reading, jihad_quranic_corpus__expansionist_legalist_reading).
narrative_ontology:affects_constraint(jihad_quranic_corpus__defensive_spiritual_reading, jihad_quranic_corpus__revolutionary_vanguard_reading).

% DUAL FORMULATION NOTE:
% The jihad_quranic_corpus kernel decomposes into at least three structurally distinct constraints because the same textual label conflates claims with different epsilon profiles, beneficiary/victim structures, and coordination/extraction balances. This defensive reading has negligible overlap in victim/beneficiary structure with the expansionist reading, and logical incompatibility with the vanguard reading on the locus of authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
