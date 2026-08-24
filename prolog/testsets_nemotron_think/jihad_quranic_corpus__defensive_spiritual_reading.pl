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
 *   human_readable: Jihad as Internal Spiritual Struggle and Defensive Armed Response (Quranic Corpus Reading)
 *   domain: religious_law/political_theology
 *
 * SUMMARY:
 *   This constraint story instantiates the defensive_spiritual_reading of the
 *   jihad_quranic_corpus kernel. It reads the Quranic jihad corpus as
 *   establishing two primary modalities: (1) jihad al-nafs — the internal
 *   spiritual struggle against base desires, incumbent on every Muslim as a
 *   lifelong practice of self-discipline; and (2) armed jihad — strictly
 *   defensive, requiring legitimate state authority (imam/caliph/recognized
 *   government), proportionality in force, and absolute immunity for
 *   non-combatants (women, children, elderly, clergy, non-fighting men).
 *   Offensive warfare to expand Islamic governance is excluded; the Quranic
 *   'sword verses' (9:5, 9:29) are read contextually as responding to
 *   specific treaty violations by named tribes, not as general license. The
 *   coexistence framework (dhimma historically, constitutional citizenship in
 *   modern formulations) is privileged as the normative relation with
 *   non-Muslim polities. This reading is held by mainstream Sunni
 *   institutions (Al-Azhar, Zaytuna, traditional madrasa networks), major
 *   Shi'a maraji', and modern reformist scholars (Abduh, Fazlur Rahman,
 *   Abdullahi An-Na'im). It competes with two sibling readings that share the
 *   same kernel but instantiate different constraints.
 *
 * KEY AGENTS:
 *   - muslim_community: Primary beneficiary (spiritual framework, defensive coordination) — organized/biographical/constrained
 *   - islamic_state_authority: Agenda setter (declares/authorizes armed jihad) — institutional/generational/arbitrage
 *   - islamic_scholars: Agenda setter (interpret, set thresholds, transmit tradition) — institutional/generational/analytical
 *   - non_muslim_populations: Beneficiary (protected by immunity rules and coexistence framework) — organized/biographical/constrained
 *   - potential_aggressors: Payer (deterred by defensive posture, bear costs of restraint) — powerful/immediate/mobile
 *   - expansionist_legalist_adherents: Excluded (would object to offensive jihad exclusion) — organized/biographical/trapped
 *   - revolutionary_vanguard_adherents: Excluded (would object to state authority requirement) — organized/immediate/trapped
 *   - comparative_legal_scholars: Observer (analyze across readings) — analytical/civilizational/analytical
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jihad_quranic_corpus__defensive_spiritual_reading, 0.15).
domain_priors:suppression_score(jihad_quranic_corpus__defensive_spiritual_reading, 0.1).
domain_priors:theater_ratio(jihad_quranic_corpus__defensive_spiritual_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jihad_quranic_corpus__defensive_spiritual_reading, rope).
narrative_ontology:human_readable(jihad_quranic_corpus__defensive_spiritual_reading, "Jihad as Internal Spiritual Struggle and Defensive Armed Response (Quranic Corpus Reading)").
narrative_ontology:topic_domain(jihad_quranic_corpus__defensive_spiritual_reading, "religious_law/political_theology").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jihad_quranic_corpus__defensive_spiritual_reading, '01845605-f8cb-4d67-a1cc-9a4027917cfb').
narrative_ontology:cs_kernel_codification('01845605-f8cb-4d67-a1cc-9a4027917cfb', fixed_text).
narrative_ontology:cs_authority_grounding('01845605-f8cb-4d67-a1cc-9a4027917cfb', lineage).
narrative_ontology:cs_interpretation_layer_present('01845605-f8cb-4d67-a1cc-9a4027917cfb').
narrative_ontology:cs_reading_relation('01845605-f8cb-4d67-a1cc-9a4027917cfb', jihad_quranic_corpus__expansionist_legalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('01845605-f8cb-4d67-a1cc-9a4027917cfb', jihad_quranic_corpus__revolutionary_vanguard_reading, forecloses).
narrative_ontology:cs_axiom('01845605-f8cb-4d67-a1cc-9a4027917cfb', foundational, defensive_only_armed_jihad).
narrative_ontology:cs_axiom_status(defensive_only_armed_jihad, holdable).
narrative_ontology:cs_axiom_grounding('01845605-f8cb-4d67-a1cc-9a4027917cfb', defensive_only_armed_jihad, deontological).
narrative_ontology:cs_axiom('01845605-f8cb-4d67-a1cc-9a4027917cfb', foundational, state_authority_required_for_armed_jihad).
narrative_ontology:cs_axiom_status(state_authority_required_for_armed_jihad, holdable).
narrative_ontology:cs_axiom_grounding('01845605-f8cb-4d67-a1cc-9a4027917cfb', state_authority_required_for_armed_jihad, conventional).
narrative_ontology:cs_axiom('01845605-f8cb-4d67-a1cc-9a4027917cfb', foundational, jihad_al_nafs_primary_over_armed_jihad).
narrative_ontology:cs_axiom_status(jihad_al_nafs_primary_over_armed_jihad, holdable).
narrative_ontology:cs_axiom_grounding('01845605-f8cb-4d67-a1cc-9a4027917cfb', jihad_al_nafs_primary_over_armed_jihad, deontological).
narrative_ontology:cs_axiom('01845605-f8cb-4d67-a1cc-9a4027917cfb', foundational, non_combatant_immunity_absolute).
narrative_ontology:cs_axiom_status(non_combatant_immunity_absolute, holdable).
narrative_ontology:cs_axiom_grounding('01845605-f8cb-4d67-a1cc-9a4027917cfb', non_combatant_immunity_absolute, deontological).
narrative_ontology:cs_axiom('01845605-f8cb-4d67-a1cc-9a4027917cfb', secondary, coexistence_framework_normative).
narrative_ontology:cs_axiom_status(coexistence_framework_normative, holdable).
narrative_ontology:cs_axiom_grounding('01845605-f8cb-4d67-a1cc-9a4027917cfb', coexistence_framework_normative, conventional).
narrative_ontology:cs_reference_frame('01845605-f8cb-4d67-a1cc-9a4027917cfb', classical_defensive_fiqh).
narrative_ontology:cs_drift_state('01845605-f8cb-4d67-a1cc-9a4027917cfb', contemporary_reformist_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('01845605-f8cb-4d67-a1cc-9a4027917cfb', '').
narrative_ontology:cs_kernel_id(jihad_quranic_corpus__defensive_spiritual_reading, jihad_quranic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__defensive_spiritual_reading, muslim_community).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__defensive_spiritual_reading, non_muslim_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__defensive_spiritual_reading, islamic_scholars).
narrative_ontology:constraint_victim(jihad_quranic_corpus__defensive_spiritual_reading, potential_aggressors).
narrative_ontology:constraint_vindicates(jihad_quranic_corpus__defensive_spiritual_reading, quranic_proportionality_principle).
narrative_ontology:constraint_vindicates(jihad_quranic_corpus__defensive_spiritual_reading, non_combatant_immunity_rule).
narrative_ontology:constraint_vindicates(jihad_quranic_corpus__defensive_spiritual_reading, state_monopoly_on_armed_jihad_declaration).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receives the spiritual framework of jihad al-nafs as a practice of self-discipline and the defensive coordination of collective security under clear rules. Exit from the religious identity is constrained by communal bonds, family, and in some polities legal consequences; exit from the defensive framework means accepting vulnerability or adopting a competing reading.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, muslim_community, beneficiary,
    organized, biographical, constrained, global).

% Holds the monopoly on declaring armed jihad, setting rules of engagement, and authorizing military action. Bears the political cost of restraint (cannot launch offensive wars for expansion) but controls the authorization gate. Can shift to a different reading (arbitrage) but faces legitimacy costs from scholarly establishment and population.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, islamic_state_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Interpret the corpus, transmit the tradition, issue fatwas on defensive thresholds, and legitimate state declarations. Benefit from interpretive authority and institutional position. Can engage other readings analytically but face professional pressure to maintain consensus.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, islamic_scholars, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(jihad_quranic_corpus__defensive_spiritual_reading, islamic_scholars, beneficiary).

% Protected by non-combatant immunity rules and coexistence frameworks (dhimma historically, constitutional citizenship in modern practice). Gain security from the reading's restraint on offensive violence. Exit options constrained by geography, citizenship, and in some cases legal status; the reading's protections are a structural benefit they cannot individually negotiate.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, non_muslim_populations, beneficiary,
    organized, biographical, constrained, global).

% States or non-state actors considering aggression against Muslim populations. Bear the deterrence cost: the reading's defensive coordination raises the cost of attack. They are not targeted unless they initiate aggression; their 'payment' is foregone opportunity. Mobile — they can choose not to aggress.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, potential_aggressors, payer,
    powerful, immediate, mobile, global).

% Scholars and movements (classical madhhab positions on offensive jihad, some modern Islamist parties) who read the corpus as permitting offensive campaigns under juristic conditions. They are excluded from this reading's framework because their core premise (offensive jihad as obligation/permission) is foreclosed. They would object to the defensive-only restriction but have no voice in this reading's internal deliberation.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, expansionist_legalist_adherents, excluded,
    organized, biographical, trapped, global).

% Activist groups (historical Kharijites, modern takfiri movements) who read armed jihad as immediate individual obligation (fard 'ayn) against rulers deemed apostate or occupiers, bypassing state authority. This reading's state-authority axiom forecloses their core premise. They are structurally excluded and would violently reject the constraint; their exit is trapped because they define themselves against it.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, revolutionary_vanguard_adherents, excluded,
    organized, immediate, trapped, global).

% Academic and juridical analysts who study the kernel across its readings. They neither collect nor pay; they map the structural differences between readings. Their analytical exit is complete — they can adopt any reading as an object of study.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, comparative_legal_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates Muslim spiritual self-discipline (jihad al-nafs) as a lifelong practice of ego-restraint and God-orientation, and coordinates collective defensive security through a high-threshold, state-authorized, proportional armed response that protects non-combatants and privileges coexistence with non-Muslim polities.
% TRANSFER_FUNCTION: Transfers the burden of offensive violence from the Muslim community to potential aggressors (deterrence): the community restrains from expansion, aggressors bear the cost of being deterred. Transfers spiritual effort from individuals to communal coherence: each person's internal struggle reinforces the collective identity that makes defensive coordination possible.
% ABSENT_VOICES: Expansionist legalist adherents (classical offensive-jihad jurists, some modern Islamist parties) and revolutionary vanguard adherents (Kharijite-descendant movements, takfiri groups) are structurally excluded. They would argue that the Quran mandates offensive expansion or immediate individual obligation respectively, and that the defensive reading abandons divine command. They are absent because this reading's axioms foreclose (vanguard) or marginalize (expansionist) their premises.
% DISAPPEARANCE_RATIONALE: If this reading vanished overnight, the kernel would be instantiated solely by the expansionist and revolutionary readings. Muslim polities would lose the juristic basis for defensive-only postures and coexistence frameworks; non-Muslim populations would lose the textual protection of non-combatant immunity; the spiritual discipline of jihad al-nafs would lose its primary textual anchor. The Muslim world would reorganize around offensive and/or revolutionary modalities.
% FOUNDING_PROBLEM: How to read the Quranic jihad corpus — which contains verses commanding fighting, verses restricting fighting to defense, verses on spiritual struggle, and verses on coexistence — as a coherent whole without authorizing unrestricted violence against non-Muslims or internal dissenters.
% FOUNDING_PROBLEM_CORROBORATION: Classical corroboration: Hanafi/Maliki/Shafi'i/Hanbali consensus on defensive war as default (offensive requires Imam), Ghazali's Ihya on jihad al-nafs as greater jihad, Ibn Taymiyyah's distinction between defensive (fard 'ayn) and offensive (fard kifaya). Modern corroboration: Muhammad Abduh's rationalist tafsir, Fazlur Rahman's thematic hermeneutic, Abdullahi An-Na'im's constitutional mediation. No single authority outside the beneficiary set (scholarly establishment) definitively corroborates; the contest is internal to the tradition.
narrative_ontology:disappearance_verdict(jihad_quranic_corpus__defensive_spiritual_reading, world_rearranges).
narrative_ontology:founding_problem_status(jihad_quranic_corpus__defensive_spiritual_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jihad_quranic_corpus__defensive_spiritual_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jihad_quranic_corpus__defensive_spiritual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jihad_quranic_corpus__defensive_spiritual_reading, 0.15, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is low (0.15) because the constraint primarily coordinates spiritual practice and defensive deterrence rather than extracting resources. The spiritual struggle is self-imposed; the defensive rules protect all populations. Suppression is low (0.1) because the reading explicitly rejects compulsion in religion (2:256) and limits armed force to state-authorized defense — alternatives (other readings, secular frameworks) are not suppressed by this reading's internal logic. Theater ratio is low (0.1) because the spiritual practice and defensive coordination are genuine, not performative. Accessibility collapse is moderate (0.4) because the reading offers a coherent hermeneutic that resolves textual tensions, but competing readings remain live. Resistance is low (0.2) because the reading's main contestation comes from other readings of the same kernel, not from external rejection.
 *
 * PERSPECTIVAL GAP:
 *   The muslim_community and islamic_state_authority seats experience this as genuine coordination (rope) — spiritual coherence and collective security. The potential_aggressors seat experiences deterrence cost but not extraction. The expansionist_legalist_adherents and revolutionary_vanguard_adherents seats (excluded) experience this reading as a constraint on their preferred modality — they would classify the kernel differently. The engine computes per-seat types from the structural data; the divergence between included and excluded seats is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: muslim_community (spiritual framework, defensive coordination, d ~ 0.1), non_muslim_populations (protection from targeting, coexistence rights, d ~ 0.05). Agenda setters: islamic_state_authority (holds declaration monopoly, d ~ 0.2 — bears responsibility but controls authorization), islamic_scholars (interpretive authority, d ~ 0.15). Payers: potential_aggressors (deterred, bear opportunity cost of foregone aggression, d ~ 0.8). Excluded: expansionist_legalist_adherents and revolutionary_vanguard_adherents (their preferred readings are not accommodated; they would experience high extraction if forced into this framework). The directionality derivation follows from beneficiary declarations + exit options: community members have constrained exit (religious identity), state authority has arbitrage (can shift reading), scholars have analytical exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how to read the Quranic jihad corpus without authorizing unrestricted violence — remains live (contested). The reading has not resolved into mandatrophy because the kernel itself is contested; the defensive-spiritual reading is actively maintained by scholarly institutions as the correct reading, not as a vestigial form. If the kernel were settled, the reading might become piton (theatrical maintenance of a settled interpretation), but the live contest with sibling readings keeps it functionally alive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_legitimacy,
    'Does this defensive-spiritual reading represent the Quran''s original intent, or is it a historically contingent interpretive construct that coexists with expansionist and revolutionary readings?',
    'Comparative tafsir analysis across classical schools (Hanafi, Maliki, Shafi''i, Hanbali, Ja''fari) and modern reformist scholarship; historical analysis of early conquest narratives vs. defensive verses.',
    'If this reading is the dominant classical consensus, its low extraction profile is structural; if it is a modern reformist construction, the low extraction may reflect selective emphasis rather than structural reality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_legitimacy, conceptual, 'Whether the defensive-spiritual reading''s coherence reflects original textual intent or interpretive selection.').

omega_variable(
    defensive_threshold_stability,
    'Is the high threshold for declaring armed jihad (state authority, proportionality, non-combatant immunity) structurally stable under pressure, or does it collapse into offensive authorization when Muslim polities face existential threat?',
    'Case study analysis of historical Muslim states under invasion (Mongol, Crusader, colonial) — did defensive jurisprudence hold or expand? Contemporary analysis of states claiming defensive jihad (e.g., Iran-Iraq war, Afghan resistance).',
    'If the threshold collapses under pressure, the constraint''s effective extraction rises (civilians become targets, proportionality erodes) and it reclassifies toward tangled_rope or snare in practice.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(defensive_threshold_stability, empirical, 'Stress-test of the defensive threshold under existential threat conditions.').

omega_variable(
    coexistence_framework_enforcement,
    'Does the coexistence framework (dhimma, aman, modern citizenship models) genuinely protect non-Muslims, or does it function as a managed inequality that extracts loyalty and revenue (jizya) while denying full political equality?',
    'Historical analysis of dhimma practice vs. theory; modern constitutional analysis of Muslim-majority states'' treatment of minorities; comparison with minority protections in non-Muslim polities.',
    'If coexistence extracts subordinated status, the reading''s beneficiary claim for non_muslim_populations is partial — they are protected but not equal, creating a structural extraction the reading denies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coexistence_framework_enforcement, empirical, 'Whether non-combatant immunity and coexistence entail full equality or managed subordination.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jihad_quranic_corpus__defensive_spiritual_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jihad_def_spiritual_tr_t0, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(jihad_def_spiritual_tr_t6, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 6, 0.08).
narrative_ontology:measurement(jihad_def_spiritual_tr_t12, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 12, 0.1).
narrative_ontology:measurement(jihad_def_spiritual_tr_t18, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 18, 0.1).
narrative_ontology:measurement(jihad_def_spiritual_tr_t24, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 24, 0.1).
narrative_ontology:measurement(jihad_def_spiritual_tr_t30, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 30, 0.1).

% Extraction over time
narrative_ontology:measurement(jihad_def_spiritual_be_t0, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(jihad_def_spiritual_be_t6, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 6, 0.12).
narrative_ontology:measurement(jihad_def_spiritual_be_t12, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 12, 0.14).
narrative_ontology:measurement(jihad_def_spiritual_be_t18, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 18, 0.15).
narrative_ontology:measurement(jihad_def_spiritual_be_t24, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 24, 0.15).
narrative_ontology:measurement(jihad_def_spiritual_be_t30, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 30, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(jihad_def_spiritual_su_t0, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(jihad_def_spiritual_su_t6, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 6, 0.08).
narrative_ontology:measurement(jihad_def_spiritual_su_t12, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 12, 0.1).
narrative_ontology:measurement(jihad_def_spiritual_su_t18, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 18, 0.1).
narrative_ontology:measurement(jihad_def_spiritual_su_t24, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 24, 0.1).
narrative_ontology:measurement(jihad_def_spiritual_su_t30, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 30, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jihad_quranic_corpus__defensive_spiritual_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jihad_quranic_corpus__defensive_spiritual_reading, 0.08).
narrative_ontology:affects_constraint(jihad_quranic_corpus__defensive_spiritual_reading, islamic_international_law).
narrative_ontology:affects_constraint(jihad_quranic_corpus__defensive_spiritual_reading, muslim_minority_fiqh).
narrative_ontology:affects_constraint(jihad_quranic_corpus__defensive_spiritual_reading, jihad_quranic_corpus__expansionist_legalist_reading).
narrative_ontology:affects_constraint(jihad_quranic_corpus__defensive_spiritual_reading, jihad_quranic_corpus__revolutionary_vanguard_reading).

% DUAL FORMULATION NOTE:
% This reading and expansionist_legalist_reading share the fixed_text kernel_codification and lineage authority_grounding but diverge on the foreclosing axiom (defensive_only vs. offensive_permitted). The revolutionary_vanguard_reading shifts kernel_codification to distributed and authority_grounding to practice/extraction, foreclosing both lineage-grounded readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jihad_quranic_corpus__defensive_spiritual_reading, institutional, 0.2).
constraint_indexing:directionality_override(jihad_quranic_corpus__defensive_spiritual_reading, organized, 0.1).
constraint_indexing:directionality_override(jihad_quranic_corpus__defensive_spiritual_reading, powerful, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
