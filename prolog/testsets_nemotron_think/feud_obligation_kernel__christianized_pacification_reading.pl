% ============================================================================
% CONSTRAINT STORY: feud_obligation_kernel__christianized_pacification_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feud_obligation_kernel__christianized_pacification_reading, []).

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
 *   constraint_id: feud_obligation_kernel__christianized_pacification_reading
 *   human_readable: Church Prohibition of Blood-Feud as Divine Law
 *   domain: legal_anthropology/medieval_history/comparative_political_systems
 *
 * SUMMARY:
 *   The christianized_pacification_reading instantiates the
 *   feud_obligation_kernel as a divine-law prohibition: blood-feud
 *   obligations violate God's ban on private vengeance; legitimate violence
 *   resides solely with God, delegated to Church and anointed kings. This
 *   reading presents the constraint as mountain (divine law), but structural
 *   analysis reveals a snare: ecclesiastical institutions gain interpretive
 *   monopoly and penitential revenue; royal institutions gain delegated
 *   violence monopoly; all feud participants (individuals and kindreds) enter
 *   the victim set via spiritual peril and enforced penitential discipline.
 *   Complete suppression is sought through canonical courts, penitential
 *   tariffs, and the Peace of God/Truce of God movements. The claim/metric
 *   divergence is deliberate: the reading claims mountain; the metrics
 *   describe extractive suppression.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feud_obligation_kernel__christianized_pacification_reading, 0.82).
domain_priors:suppression_score(feud_obligation_kernel__christianized_pacification_reading, 0.88).
domain_priors:theater_ratio(feud_obligation_kernel__christianized_pacification_reading, 0.73).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 0.73).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, accessibility_collapse, 0.84).
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feud_obligation_kernel__christianized_pacification_reading, snare).
narrative_ontology:human_readable(feud_obligation_kernel__christianized_pacification_reading, "Church Prohibition of Blood-Feud as Divine Law").
narrative_ontology:topic_domain(feud_obligation_kernel__christianized_pacification_reading, "legal_anthropology/medieval_history/comparative_political_systems").

domain_priors:requires_active_enforcement(feud_obligation_kernel__christianized_pacification_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feud_obligation_kernel__christianized_pacification_reading, '5b40dff5-6bfc-4e06-ac8a-5004504e9ee5').
narrative_ontology:cs_kernel_codification('5b40dff5-6bfc-4e06-ac8a-5004504e9ee5', formalized).
narrative_ontology:cs_authority_grounding('5b40dff5-6bfc-4e06-ac8a-5004504e9ee5', extraction).
narrative_ontology:cs_interpretation_layer_present('5b40dff5-6bfc-4e06-ac8a-5004504e9ee5').
narrative_ontology:cs_reading_relation('5b40dff5-6bfc-4e06-ac8a-5004504e9ee5', feud_obligation_kernel__stateless_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('5b40dff5-6bfc-4e06-ac8a-5004504e9ee5', feud_obligation_kernel__extraction_cycle_reading, coexists_with).
narrative_ontology:cs_axiom('5b40dff5-6bfc-4e06-ac8a-5004504e9ee5', foundational, blood_feud_violates_divine_law).
narrative_ontology:cs_axiom_status(blood_feud_violates_divine_law, holdable).
narrative_ontology:cs_axiom_grounding('5b40dff5-6bfc-4e06-ac8a-5004504e9ee5', blood_feud_violates_divine_law, theological).
narrative_ontology:cs_axiom('5b40dff5-6bfc-4e06-ac8a-5004504e9ee5', foundational, legitimate_violence_monopoly_resides_in_church_and_crown).
narrative_ontology:cs_axiom_status(legitimate_violence_monopoly_resides_in_church_and_crown, holdable).
narrative_ontology:cs_axiom_grounding('5b40dff5-6bfc-4e06-ac8a-5004504e9ee5', legitimate_violence_monopoly_resides_in_church_and_crown, theological).
narrative_ontology:cs_reference_frame('5b40dff5-6bfc-4e06-ac8a-5004504e9ee5', divine_law_prohibition_of_private_vengeance).
narrative_ontology:cs_drift_state('5b40dff5-6bfc-4e06-ac8a-5004504e9ee5', high_medieval_penitential_regime, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5b40dff5-6bfc-4e06-ac8a-5004504e9ee5', '').
narrative_ontology:cs_kernel_id(feud_obligation_kernel__christianized_pacification_reading, feud_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__christianized_pacification_reading, ecclesiastical_institutions).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__christianized_pacification_reading, royal_institutions).
narrative_ontology:constraint_victim(feud_obligation_kernel__christianized_pacification_reading, feud_participants).
narrative_ontology:constraint_victim(feud_obligation_kernel__christianized_pacification_reading, feud_kindred_groups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__christianized_pacification_reading, secular_lords).
narrative_ontology:constraint_victim(feud_obligation_kernel__christianized_pacification_reading, secular_lords).
narrative_ontology:constraint_vindicates(feud_obligation_kernel__christianized_pacification_reading, divine_law_prohibition_of_vengeance).
narrative_ontology:constraint_vindicates(feud_obligation_kernel__christianized_pacification_reading, ecclesiastical_monopoly_on_legitimate_violence).
narrative_ontology:constraint_vindicates(feud_obligation_kernel__christianized_pacification_reading, royal_delegation_of_coercive_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers penitential discipline, defines canonical prohibitions on private vengeance, operates ecclesiastical courts that adjudicate feud-related sins, and claims interpretive monopoly over divine law on violence. Collects penitential revenue, expands jurisdictional reach into lay violence, and legitimates royal delegation. Can shift doctrine but is bound by theological coherence requirements.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, ecclesiastical_institutions, agenda_setter,
    institutional, generational, arbitrage, continental).

% Receives delegated legitimate violence authority from ecclesiastical blessing, enabling territorial consolidation and monopoly on justice. Gains taxation base from pacified populations and judicial fees from royal courts. Depends on Church legitimation for sacred kingship; cannot easily exit the symbiotic arrangement without losing sacred authority.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, royal_institutions, beneficiary,
    institutional, generational, constrained, national).

% Bound by kinship honor to pursue vengeance, but face spiritual peril (excommunication, denied burial) and penitential costs (fines, pilgrimages, public penance) for violating canonical prohibition. Their identity as 'avengers of kin' is fused with the feud obligation; exit requires abandoning kin identity and risking eternal damnation. No mobile alternative — the constraint defines their moral universe.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, feud_participants, payer,
    moderate, biographical, identity_locked, local).

% Collective liability for feud actions; entire kindred bears penitential burden and spiritual stigma. Kindred solidarity makes individual exit impossible — leaving the feud betrays the group. The constraint targets the corporate kindred as unit, suppressing collective honor practices. No exit without kindred dissolution.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, feud_kindred_groups, payer,
    organized, generational, trapped, regional).

% Lose autonomous private justice and feud arbitration rights to royal/ecclesiastical courts (payer). Gain delegated royal judicial offices and enforcement backing (beneficiary). Caught between: resisting Church prohibition risks spiritual sanction and royal opposition; complying surrenders traditional lordship powers. Exit options limited by need for both ecclesiastical legitimacy and royal protection.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, secular_lords, payer,
    powerful, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(feud_obligation_kernel__christianized_pacification_reading, secular_lords, beneficiary).

% Subject to both feud violence (as kin or bystanders) and penitential discipline (as sinners). No voice in canonical councils, royal courts, or kindred councils. Bear costs of both systems — feud raids and Church tithes/penances — with no structural representation. Would object to both extractive systems if consulted.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, peasantry_commoners, excluded,
    powerless, immediate, trapped, local).

% Canonists, theologians, and later historians who analyze the prohibition's coherence, enforcement, and effects. Neither collect nor pay; they trace the constraint's evolution from patristic texts through Gratian to Trent. Their analysis feeds back into ecclesiastical self-understanding but does not drive enforcement.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, theological_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Replaces private vengeance with divinely sanctioned ecclesiastical/royal justice, coordinating social order around a single legitimate violence authority that claims to protect souls and public peace.
% TRANSFER_FUNCTION: Moves spiritual authority, jurisdictional reach, penitential revenue, and violence monopoly from feud participants and kindred groups to ecclesiastical institutions, and delegated coercive authority to royal institutions.
% ABSENT_VOICES: Feuding kindreds and local lords who lose autonomous justice capacity; peasantry caught between feud violence and penitential discipline; neither present in the theological-legal discourse that authorizes the constraint.
% DISAPPEARANCE_RATIONALE: The constraint structures the legitimate violence monopoly in medieval Christendom; its removal returns violence authority to private hands (feud revival) or requires new legitimation for royal courts (secularization of justice). The ecclesiastical revenue stream from penitential discipline collapses.
% FOUNDING_PROBLEM: Private vengeance and blood-feud create endemic violence that threatens Christian social order and souls; divine law prohibits vengeance, requiring a single legitimate authority for justice to replace kin-based retribution.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by contemporary chronicles (e.g., Orderic Vitalis, Galbert of Bruges) describing feud violence as social scourge requiring spiritual remedy; contested by legal historians (e.g., Paul Hyams, Stephen White) who argue feud was functional coordination in stateless regions and Church prohibition served jurisdictional expansion more than peace.
narrative_ontology:disappearance_verdict(feud_obligation_kernel__christianized_pacification_reading, world_rearranges).
narrative_ontology:founding_problem_status(feud_obligation_kernel__christianized_pacification_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feud_obligation_kernel__christianized_pacification_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(feud_obligation_kernel__christianized_pacification_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feud_obligation_kernel__christianized_pacification_reading, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feud_obligation_kernel__christianized_pacification_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(feud_obligation_kernel__christianized_pacification_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(feud_obligation_kernel__christianized_pacification_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.82: penitential system extracts wealth, labor, and spiritual compliance from feud participants; ecclesiastical courts monetize absolution; royal courts monetize justice. Suppression 0.88: feud is not merely discouraged but canonically prohibited with spiritual sanctions (excommunication, interdict) and royal military enforcement. Theater 0.73: divine law framing masks jurisdictional extraction; penitential performance exceeds pastoral need. Accessibility collapse 0.84: alternative justice (feud, customary law) is theologically delegitimized and legally suppressed. Resistance 0.42: feudal lords resist but spiritual sanctions raise exit costs; kindred identity-lock prevents collective exit.
 *
 * PERSPECTIVAL GAP:
 *   From ecclesiastical seat: constraint is genuine coordination (divine law implementing God's peace). From feud participant seat: constraint is spiritual extortion (pay penance or burn). From royal seat: constraint is useful legitimation (sacralizes monopoly). From peasant seat: constraint is double extraction (feud violence + Church penance). The engine computes this divergence from structural data — the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Ecclesiastical institutions are structural beneficiaries (d ~ 0.1): they set rules, collect revenue, face no spiritual peril. Royal institutions are beneficiaries (d ~ 0.2): delegated authority, constrained by need for Church legitimation. Feud participants are full targets (d ~ 0.95): identity-locked, spiritual peril, no exit. Kindred groups are trapped targets (d ~ 0.9): corporate liability, no individual exit. Secular lords are constrained payers with secondary benefit (d ~ 0.55): lose feud rights, gain royal office. Peasantry are excluded (no seat in legitimation discourse).
 *
 * MANDATROPHY ANALYSIS:
 *   The reading presents itself as eternal divine law (mandate never resolves), but historically the penitential system's extraction intensified (Gregorian reform, Lateran IV) while feud persisted in practice. The mandate (prohibit vengeance) outlived its coordination function (royal courts established) — the constraint persists as extraction vehicle. Classification as snare prevents mislabeling this jurisdictional capture as divine coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_naturalness_vs_institutional_construction,
    'Is the divine-law prohibition on vengeance a genuine natural law (mountain) or an institutional construction that benefits ecclesiastical and royal power?',
    'Comparative analysis of pre-Christian Germanic law codes vs. canonical collections: if prohibition appears only with Church institutionalization, it is constructed. Patristic texts (Ambrose, Augustine) on vengeance as sin vs. later canonical systematization (Gratian, Decretals).',
    'If constructed, the constraint is a false summit mountain (FSM trigger) reclassifying to snare/tangled_rope. If genuine natural law, the beneficiary structure is incidental to divine command.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_naturalness_vs_institutional_construction, conceptual, 'Whether the mountain claim is genuine or a jurisdictional cover story.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (ecclesiastical courts, royal armies) or internalized (sin consciousness, fear of damnation)?',
    'Post-Reformation suppression trajectory: in Protestant regions where penitential system collapsed, did feud suppression persist via royal courts alone? If suppression drops with penitential system, internalized component was significant.',
    'If internalized, effective suppression is higher than structural measure suggests — the target carries suppression internally after formal enforcement relaxes. Affects χ computation for identity_locked agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in spiritual coercion.').

omega_variable(
    coordination_extraction_boundary,
    'Does the ecclesiastical/royal justice system provide genuine coordination (peace, order) that justifies its extraction, or is coordination purely cover?',
    'Compare violence rates and dispute resolution outcomes in regions with strong vs. weak ecclesiastical/royal penetration (e.g., Iceland vs. France). If royal justice reduces violence below feud levels, coordination is genuine; if violence persists but extraction continues, cover story.',
    'If coordination genuine, constraint may be tangled_rope not snare. If cover, snare classification holds. Affects Boltzmann floor for enforcement_mechanism type.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, empirical, 'Whether the constraint''s coordination function is real or pretextual.').

omega_variable(
    committer_frame_ambiguity,
    'How does this reading''s structural relationship to the feud_obligation_kernel differ from its siblings, and where is the disagreement located?',
    'Map each reading''s ε referent: stateless_coordination evaluates feud itself (low ε); extraction_cycle evaluates feud itself (high ε); christianized_pacification evaluates Church prohibition of feud (high ε). The disagreement is located in WHICH ARRANGEMENT is the referent — the feud or its suppression.',
    'Clarifies that ε-invariance holds per reading, not per kernel. Each reading authors ε for its own referent. Prevents category error of averaging across readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_ambiguity, conceptual, 'Commitment-frame location of the kernel contest: feud vs. its prohibition as referent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feud_obligation_kernel__christianized_pacification_reading, 1000, 1300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t1000, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 1000, 0.45).
narrative_ontology:measurement(feud_tr_t1050, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 1050, 0.52).
narrative_ontology:measurement(feud_tr_t1100, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 1100, 0.6).
narrative_ontology:measurement(feud_tr_t1150, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 1150, 0.65).
narrative_ontology:measurement(feud_tr_t1200, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 1200, 0.69).
narrative_ontology:measurement(feud_tr_t1250, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 1250, 0.71).
narrative_ontology:measurement(feud_tr_t1300, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 1300, 0.73).

% Extraction over time
narrative_ontology:measurement(feud_be_t1000, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 1000, 0.55).
narrative_ontology:measurement(feud_be_t1050, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 1050, 0.62).
narrative_ontology:measurement(feud_be_t1100, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 1100, 0.7).
narrative_ontology:measurement(feud_be_t1150, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 1150, 0.75).
narrative_ontology:measurement(feud_be_t1200, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 1200, 0.79).
narrative_ontology:measurement(feud_be_t1250, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 1250, 0.81).
narrative_ontology:measurement(feud_be_t1300, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 1300, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t1000, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 1000, 0.65).
narrative_ontology:measurement(feud_su_t1050, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 1050, 0.72).
narrative_ontology:measurement(feud_su_t1100, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 1100, 0.78).
narrative_ontology:measurement(feud_su_t1150, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 1150, 0.82).
narrative_ontology:measurement(feud_su_t1200, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 1200, 0.85).
narrative_ontology:measurement(feud_su_t1250, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 1250, 0.87).
narrative_ontology:measurement(feud_su_t1300, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 1300, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feud_obligation_kernel__christianized_pacification_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(feud_obligation_kernel__christianized_pacification_reading, 0.1).
narrative_ontology:affects_constraint(feud_obligation_kernel__christianized_pacification_reading, feud_obligation_kernel__stateless_coordination_reading).
narrative_ontology:affects_constraint(feud_obligation_kernel__christianized_pacification_reading, feud_obligation_kernel__extraction_cycle_reading).
narrative_ontology:affects_constraint(feud_obligation_kernel__christianized_pacification_reading, royal_justice_institutionalization).
narrative_ontology:affects_constraint(feud_obligation_kernel__christianized_pacification_reading, canonical_penitential_system).

% DUAL FORMULATION NOTE:
% Feud_obligation_kernel decomposes into three constraint stories: (1) stateless_coordination_reading — feud as coordination (rope/tangled_rope), low ε; (2) extraction_cycle_reading — feud as extraction (snare), high ε; (3) christianized_pacification_reading — Church prohibition as snare, high ε. The prohibition constraint (this story) structurally depends on the feud's existence as its referent; the feud constraints exist independently. Network edges reflect institutional coupling: Church prohibition suppresses feud (affects stateless_coordination), royal delegation absorbs feud's justice function (affects extraction_cycle).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
