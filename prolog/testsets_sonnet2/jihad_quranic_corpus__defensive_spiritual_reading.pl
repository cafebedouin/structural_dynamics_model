% ============================================================================
% CONSTRAINT STORY: jihad_quranic_corpus__defensive_spiritual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-15
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
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: Jihad as Internal Struggle and Constrained Defensive Response (Defensive-Spiritual Reading)
 *   domain: Islamic Jurisprudence / Comparative Religious Law / Political Theology
 *
 * SUMMARY:
 *   This constraint captures the defensive-spiritual reading of jihad within
 *   the contested Quranic corpus kernel: jihad al-nafs (internal moral
 *   struggle against one's own vices and weaknesses) as the primary meaning,
 *   with armed jihad permitted only as a constrained, state-authorized,
 *   defensive response to actual aggression, bound by proportionality and
 *   non-combatant immunity. This reading is not a description of 'what jihad
 *   really is' independent of contest — it is one structurally coherent
 *   reading among several live readings of the same kernel texts and
 *   traditions, distinguished by who falls within its scope of legitimate
 *   force (aggressors only, not non-Muslims generally), who may authorize
 *   armed jihad (established state/institutional religious authority, not
 *   individual claimants), and how high the threshold for declaration sits
 *   (very high, requiring recognized aggression). The sibling readings —
 *   expansionist-legalist (permitting offensive campaigns to establish
 *   Islamic governance) and revolutionary-vanguard (individual obligation
 *   bypassing state authority via takfir) — are separate constraints with
 *   their own ε values, victim sets, and authority structures; they are not
 *   folded into this one.
 *
 * KEY AGENTS:
 *   - muslim_communities_seeking_coexistence: beneficiary/organized — relies on this reading for peaceable religious self-understanding
 *   - recognized_aggressors_under_defensive_criteria: payer/moderate — bears the constraint's defensive force under its own criteria
 *   - state_authorities_administering_just_war_criteria: agenda_setter/institutional — gatekeeps legitimate declaration
 *   - non_combatant_populations_under_this_reading: beneficiary/powerless — protected by proportionality and immunity norms if honored
 *   - revolutionary_vanguard_claimants: excluded — delegitimized by this reading's authority structure
 *   - interfaith_diplomatic_actors: beneficiary/moderate — cites this reading in coexistence-building work
 *   - comparative_religious_law_scholars: observer/analytical — studies the reading without institutional stake
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jihad_quranic_corpus__defensive_spiritual_reading, 0.18).
domain_priors:suppression_score(jihad_quranic_corpus__defensive_spiritual_reading, 0.22).
domain_priors:theater_ratio(jihad_quranic_corpus__defensive_spiritual_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jihad_quranic_corpus__defensive_spiritual_reading, rope).
narrative_ontology:human_readable(jihad_quranic_corpus__defensive_spiritual_reading, "Jihad as Internal Struggle and Constrained Defensive Response (Defensive-Spiritual Reading)").
narrative_ontology:topic_domain(jihad_quranic_corpus__defensive_spiritual_reading, "Islamic Jurisprudence / Comparative Religious Law / Political Theology").

domain_priors:requires_active_enforcement(jihad_quranic_corpus__defensive_spiritual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jihad_quranic_corpus__defensive_spiritual_reading, '2dbda593-3ee4-442a-b017-c54d7685ef29').
narrative_ontology:cs_kernel_codification('2dbda593-3ee4-442a-b017-c54d7685ef29', fixed_text).
narrative_ontology:cs_authority_grounding('2dbda593-3ee4-442a-b017-c54d7685ef29', lineage).
narrative_ontology:cs_interpretation_layer_present('2dbda593-3ee4-442a-b017-c54d7685ef29').
narrative_ontology:cs_reading_relation('2dbda593-3ee4-442a-b017-c54d7685ef29', jihad_quranic_corpus__expansionist_legalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('2dbda593-3ee4-442a-b017-c54d7685ef29', jihad_quranic_corpus__revolutionary_vanguard_reading, forecloses).
narrative_ontology:cs_axiom('2dbda593-3ee4-442a-b017-c54d7685ef29', foundational, greater_jihad_primacy_over_armed_struggle).
narrative_ontology:cs_axiom_status(greater_jihad_primacy_over_armed_struggle, holdable).
narrative_ontology:cs_axiom_grounding('2dbda593-3ee4-442a-b017-c54d7685ef29', greater_jihad_primacy_over_armed_struggle, conventional).
narrative_ontology:cs_axiom('2dbda593-3ee4-442a-b017-c54d7685ef29', foundational, state_authority_exclusive_gate_on_armed_jihad).
narrative_ontology:cs_axiom_status(state_authority_exclusive_gate_on_armed_jihad, holdable).
narrative_ontology:cs_axiom_grounding('2dbda593-3ee4-442a-b017-c54d7685ef29', state_authority_exclusive_gate_on_armed_jihad, conventional).
narrative_ontology:cs_axiom('2dbda593-3ee4-442a-b017-c54d7685ef29', secondary, non_muslims_outside_scope_absent_aggression).
narrative_ontology:cs_axiom_status(non_muslims_outside_scope_absent_aggression, holdable).
narrative_ontology:cs_axiom_grounding('2dbda593-3ee4-442a-b017-c54d7685ef29', non_muslims_outside_scope_absent_aggression, deontological).
narrative_ontology:cs_reference_frame('2dbda593-3ee4-442a-b017-c54d7685ef29', classical_defensive_just_war_synthesis).
narrative_ontology:cs_drift_state('2dbda593-3ee4-442a-b017-c54d7685ef29', post_9_11_securitization_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('2dbda593-3ee4-442a-b017-c54d7685ef29', '').
narrative_ontology:cs_kernel_id(jihad_quranic_corpus__defensive_spiritual_reading, jihad_quranic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__defensive_spiritual_reading, muslim_communities_seeking_coexistence).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__defensive_spiritual_reading, non_combatant_populations_under_this_reading).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__defensive_spiritual_reading, state_authorities_administering_just_war_criteria).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__defensive_spiritual_reading, interfaith_diplomatic_actors).
narrative_ontology:constraint_victim(jihad_quranic_corpus__defensive_spiritual_reading, recognized_aggressors_under_defensive_criteria).
narrative_ontology:constraint_vindicates(jihad_quranic_corpus__defensive_spiritual_reading, primacy_of_greater_jihad_doctrine).
narrative_ontology:constraint_vindicates(jihad_quranic_corpus__defensive_spiritual_reading, proportionality_and_discrimination_norms).
narrative_ontology:constraint_vindicates(jihad_quranic_corpus__defensive_spiritual_reading, state_monopoly_on_legitimate_force_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adherents who understand jihad primarily as internal moral and spiritual effort (jihad al-nafs) and who rely on the defensive-and-proportionate framing to justify coexistence with non-Muslim neighbors and states. This reading provides religious legitimacy for peaceful integration and reduces internal and external suspicion of Muslim political loyalty.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, muslim_communities_seeking_coexistence, beneficiary,
    organized, generational, constrained, global).

% Parties who, under this reading's own criteria, have initiated aggression against Muslim populations or violated treaty obligations, and against whom defensive armed jihad becomes religiously licensed once state authority declares it. Their treatment depends on proportionality and non-combatant immunity norms that this reading insists bind even against them.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, recognized_aggressors_under_defensive_criteria, payer,
    moderate, immediate, constrained, regional).

% Governments and recognized religious-legal authorities (imams, fuqaha attached to states or established institutions) who hold the exclusive competence to declare defensive jihad, assess aggression, and authorize armed response. They administer the high threshold for declaration and can withhold authorization, which concentrates legitimate-force determination in institutional hands.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, state_authorities_administering_just_war_criteria, agenda_setter,
    institutional, generational, arbitrage, national).

% Civilians on any side of a conflict who benefit from the reading's insistence on non-combatant immunity and proportionality as binding constraints on how defensive jihad may be conducted, even when the underlying conflict is not of their making. They cannot exit the conflict zone but the doctrine, if honored, limits what may be done to them.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, non_combatant_populations_under_this_reading, beneficiary,
    powerless, immediate, trapped, regional).

% Groups who reject state-authority gatekeeping and declare jihad as an individual obligation bypassing established religious-legal authorities. They are excluded from legitimacy under this reading's framework, which treats their unilateral declarations as juristically invalid regardless of their own grievance narrative.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, revolutionary_vanguard_claimants, excluded,
    organized, immediate, trapped, regional).

% Religious leaders, diplomats, and scholars engaged in interfaith dialogue who rely on this reading to build coexistence frameworks and counter-narratives against expansionist or vanguardist interpretations. They benefit from having a textually grounded, institutionally endorsed peaceable reading to cite.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, interfaith_diplomatic_actors, beneficiary,
    moderate, biographical, mobile, global).

% Scholars who study the textual, historical, and doctrinal basis of competing jihad readings without institutional stake in any single reading's political success, examining how each reading emerged and what evidentiary and hermeneutic commitments underlie it.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, comparative_religious_law_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jihad_quranic_corpus__defensive_spiritual_reading, diffuse).
narrative_ontology:fixing_cost_class(jihad_quranic_corpus__defensive_spiritual_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, textually-grounded standard distinguishing legitimate defensive force from unauthorized violence, coordinating expectations among Muslim polities, non-Muslim states, and ordinary believers about when armed jihad is licensed, by whom, and under what limits (proportionality, non-combatant immunity, state authorization).
% TRANSFER_FUNCTION: Moves interpretive and moral authority toward established state and institutional religious-legal authorities and away from individual claimants or non-state actors; moves practical security benefit toward non-combatant populations (if the constraint holds) and toward states seeking to distinguish their conduct from terrorism; imposes constraint-compliance costs on any party wishing to wage armed jihad outside state channels.
% ABSENT_VOICES: Revolutionary vanguard claimants and expansionist-legalist jurists are structurally excluded from this reading's legitimacy determination — they would object that state-authority gatekeeping serves incumbent power and abandons obligations they read as textually mandated, but their objections are treated as juristically foreclosed rather than engaged on the merits within this reading's own framework.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the interpretive space it occupies would not go empty — sibling readings (expansionist-legalist, revolutionary-vanguard) would expand to fill it, altering which non-Muslims fall within scope of legitimate force and removing the state-authorization gate. Coexistence-oriented Muslim communities and interfaith actors would lose a citable doctrinal anchor, and non-combatant immunity would lose one of its textual defenders, though other juridical traditions (international humanitarian law analogues within Islamic law) might partially substitute.
% FOUNDING_PROBLEM: Early Muslim community faced literal armed aggression and needed doctrine distinguishing permissible self-defense from unconstrained warfare, while also needing to elevate moral/spiritual struggle over literal combat as the primary meaning of jihad to prevent the term from collapsing into pure militarism.
% FOUNDING_PROBLEM_CORROBORATION: Mainstream Sunni and Shia juridical traditions, along with contemporary state religious authorities (e.g., Al-Azhar, various national fatwa councils) and many independent scholars of Islamic law outside any single state's benefit, attest this reading reflects classical majority jurisprudence on just-war constraints. Critics from both expansionist-legalist and revolutionary-vanguard traditions, and some secular scholars of religious violence, dispute that the founding problem was ever primarily defensive, arguing this reading reflects post-colonial apologetic reinterpretation rather than continuous classical consensus — corroboration for the reading's textual priority is therefore genuinely contested rather than settled.
narrative_ontology:disappearance_verdict(jihad_quranic_corpus__defensive_spiritual_reading, contested).
narrative_ontology:founding_problem_status(jihad_quranic_corpus__defensive_spiritual_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jihad_quranic_corpus__defensive_spiritual_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jihad_quranic_corpus__defensive_spiritual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jihad_quranic_corpus__defensive_spiritual_reading, 0.18, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored low (0.18) because this reading's own structure minimizes coercive extraction: legitimate force is narrowly scoped to recognized aggressors, non-Muslims outside that category are explicitly not targets, and the coordination function (shared standard for distinguishing licit from illicit violence) benefits nearly all named parties including bystanders. Suppression is low-moderate (0.22) reflecting the real but limited coercive apparatus needed to enforce state-authorization gatekeeping against rival claimants (the revolutionary-vanguard reading in particular) — this is not zero because maintaining institutional monopoly on jihad declaration does require actively delegitimizing competing claims. Accessibility collapse is moderate (0.35): once understood, the reading does not eliminate all interpretive alternatives — sibling readings persist as live, contested options, which is why collapse is far below mountain-level. Resistance is moderate (0.4): the reading meets genuine contestation both from within Islamic jurisprudence (scholars favoring more expansive or more urgent readings) and from external critics who doubt its historical primacy. The extraction accumulation over the measured interval is mild and driven mainly by the gradual institutionalization of state religious authority as the exclusive interpretive gatekeeper, which slightly raises both suppression_requirement and theater_ratio as formal endorsement processes (fatwa councils, state religious ministries) become more elaborate without necessarily strengthening the underlying coordination function.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of coexistence-oriented Muslim communities and non-combatant populations, this reading functions close to a rope: a genuine coordination standard that protects them and gives them religious grounding for peaceful life among non-Muslims. From the seat of recognized aggressors under the reading's own criteria, the same structure delivers licensed defensive force against them, which they may experience as extraction even though the reading claims proportionality bounds it. From the seat of revolutionary-vanguard claimants, the reading operates as a suppression mechanism denying their preferred jurisprudential path entirely — they are excluded, not merely disagreed with.
 *
 * DIRECTIONALITY LOGIC:
 *   Muslim communities seeking coexistence and non-combatant populations are beneficiaries with low directionality (d) because the reading's coordination function subsidizes their security and religious legitimacy without extracting from them. State authorities sit as agenda-setters with arbitrage-level exit (they can shift interpretive alignment as political circumstances change) and institutional power, giving them centrality without being targets. Recognized aggressors under the reading's defensive criteria sit at higher d because the reading licenses force against them specifically, though this is bounded by the proportionality and immunity constraints the reading itself insists on — this is a self-limiting target relationship, structurally different from an unconstrained snare. Revolutionary-vanguard claimants are excluded rather than victimized in the payer sense; they are not extracted from by this constraint, they are delegitimized by it, which is why they appear as an excluded stakeholder rather than in the victims array.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (distinguishing licit defensive force from unconstrained violence, and elevating spiritual struggle over literal combat) remains genuinely live in this reading's own account — it is not treated as resolved-and-persisting-anyway. The mandatrophy risk here runs the other direction from the typical pattern: rather than an obsolete mandate propping up extraction, the risk is that a genuinely coordinating doctrine gets mislabeled as pure apologetics or pure militarism by observers who collapse it into one of its siblings. Classifying this as rope (not mountain, not tangled_rope) prevents two errors: treating it as natural/inevitable (it is one contested reading among several, requiring active interpretive maintenance and institutional backing) and treating it as pure extraction/cover-story (it has a genuine, non-trivial coordination function — distinguishing licit from illicit force — that benefits a broad set of parties including those outside the faith community).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_primacy_of_defensive_reading,
    'Does the defensive-spiritual reading reflect the historically dominant classical juridical consensus, or is it a modern apologetic reconstruction responding to post-colonial and post-9/11 pressures to distance mainstream Islam from militant interpretations?',
    'Comparative textual-historical analysis of classical tafsir and fiqh across schools (Hanafi, Shafi''i, Maliki, Hanbali, Ja''fari) and centuries, tracing whether proportionality/non-combatant-immunity/state-authorization constraints were majority doctrine prior to the 20th century or represent a later reformist synthesis.',
    'If primarily a modern reconstruction, this reading''s claim to textual/historical authority weakens relative to the expansionist-legalist reading''s claim to classical continuity, which would not change this story''s own ε (authored from this reading''s internal lights per the ε-referent rule) but would affect its comparative standing in the kernel contest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_primacy_of_defensive_reading, empirical, 'Whether the defensive-spiritual reading is classically dominant or a modern reconstruction.').

omega_variable(
    state_authorization_gate_as_capture_risk,
    'Does requiring state authorization for legitimate jihad declaration function as a genuine safeguard against unconstrained violence, or does it primarily serve incumbent state power by making religious legitimacy contingent on political loyalty?',
    'Case analysis of instances where state religious authorities withheld or granted jihad authorization and cross-reference with whether the state''s own political interests aligned with the authorization outcome, versus independent assessment of whether aggression criteria were genuinely met.',
    'If the gate is substantially captured by state political interest, the coordination function this reading claims is partly cover for state monopolization of religious legitimacy, which would push the type assessment toward tangled_rope; if the gate functions independently of state interest, the rope classification holds more cleanly.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_authorization_gate_as_capture_risk, conceptual, 'Whether state-authorization gatekeeping is a genuine safeguard or a capture mechanism.').

omega_variable(
    coexists_or_forecloses_relative_to_siblings,
    'Is this reading''s core premise (non-Muslims outside legitimate target scope absent aggression; state authority required) logically compatible with the expansionist-legalist reading''s premise (offensive campaigns permitted under jurisprudential conditions) within a single interpretive framework, or does adopting one foreclose the other?',
    'Formal analysis of whether the two readings'' core textual warrants (verses cited, abrogation theory applied, hadith selected as authoritative) can be jointly held without contradiction by a single jurist, versus requiring an exclusive choice of interpretive method.',
    'Determines whether the reading_relations edge to expansionist_legalist_reading should be coexists_with (both remain live options across different scholarly communities) or something stronger; this story treats it as coexists_with based on documented historical coexistence of restrictive and expansive jihad doctrines across different schools and eras without one formally overriding the other.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coexists_or_forecloses_relative_to_siblings, conceptual, 'Whether this reading and the expansionist-legalist reading are logically compatible or mutually exclusive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jihad_quranic_corpus__defensive_spiritual_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jiha_tr_t0, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(jiha_tr_t20, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 20, 0.11).
narrative_ontology:measurement(jiha_tr_t40, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 40, 0.12).
narrative_ontology:measurement(jiha_tr_t60, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 60, 0.13).
narrative_ontology:measurement(jiha_tr_t80, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 80, 0.14).
narrative_ontology:measurement(jiha_tr_t100, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(jiha_be_t0, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(jiha_be_t20, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 20, 0.14).
narrative_ontology:measurement(jiha_be_t40, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 40, 0.15).
narrative_ontology:measurement(jiha_be_t60, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 60, 0.16).
narrative_ontology:measurement(jiha_be_t80, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 80, 0.17).
narrative_ontology:measurement(jiha_be_t100, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 100, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(jiha_su_t0, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(jiha_su_t20, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 20, 0.17).
narrative_ontology:measurement(jiha_su_t40, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 40, 0.18).
narrative_ontology:measurement(jiha_su_t60, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 60, 0.19).
narrative_ontology:measurement(jiha_su_t80, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 80, 0.21).
narrative_ontology:measurement(jiha_su_t100, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 100, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jihad_quranic_corpus__defensive_spiritual_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jihad_quranic_corpus__defensive_spiritual_reading, 0.1).
narrative_ontology:affects_constraint(jihad_quranic_corpus__defensive_spiritual_reading, jihad_quranic_corpus__expansionist_legalist_reading).
narrative_ontology:affects_constraint(jihad_quranic_corpus__defensive_spiritual_reading, jihad_quranic_corpus__revolutionary_vanguard_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the jihad_quranic_corpus kernel. The expansionist_legalist_reading and revolutionary_vanguard_reading are separate constraint files with independently authored ε, victim sets, and authority structures. The defensive-spiritual reading's ε (0.18) is far lower than what the revolutionary-vanguard reading would author for the same underlying textual corpus, because the readings diverge on who counts as a legitimate target, who may authorize force, and what threshold triggers obligation — per the ε-invariance principle these are structurally distinct constraints sharing a kernel, not one constraint measured two ways. The forecloses relation to revolutionary_vanguard_reading reflects that this reading's state-authorization gate is logically incompatible with the vanguard reading's core premise of bypassing state authority via emergency jurisprudence — a single jurist cannot coherently hold both that state authorization is required and that it may be bypassed. The coexists_with relation to expansionist_legalist_reading reflects that both restrictive and expansive jihad doctrines have coexisted across different schools and historical periods without one formally displacing the other.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
