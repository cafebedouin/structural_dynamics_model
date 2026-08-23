% ============================================================================
% CONSTRAINT STORY: quran_ontological_status__state_enforced_creation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_ontological_status__state_enforced_creation_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: quran_ontological_status__state_enforced_creation_reading
 *   human_readable: Miḥna Inquisition: State Enforcement of Created-Qurʾān Doctrine
 *   domain: islamic_theology/political_authority
 *
 * SUMMARY:
 *   The miḥna (833–848 CE) was an inquisition instituted by the Abbasid
 *   caliph al-Maʾmūn requiring scholars to affirm the createdness of the
 *   Qurʾān (the Muʿtazilite position). It transformed a metaphysical dispute
 *   into a tool of political control: refusal meant imprisonment, flogging,
 *   and professional ruin. The constraint is the state-enforced doctrine, not
 *   the theological claim alone. The calibration reflects the miḥna period
 *   only; the pure theological claim (created_reading) is a separate
 *   constraint with different metrics.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_ontological_status__state_enforced_creation_reading, 0.82).
domain_priors:suppression_score(quran_ontological_status__state_enforced_creation_reading, 0.91).
domain_priors:theater_ratio(quran_ontological_status__state_enforced_creation_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_ontological_status__state_enforced_creation_reading, snare).
narrative_ontology:human_readable(quran_ontological_status__state_enforced_creation_reading, "Miḥna Inquisition: State Enforcement of Created-Qurʾān Doctrine").
narrative_ontology:topic_domain(quran_ontological_status__state_enforced_creation_reading, "islamic_theology/political_authority").

domain_priors:requires_active_enforcement(quran_ontological_status__state_enforced_creation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_ontological_status__state_enforced_creation_reading, '115a384f-9aa7-41b2-a9b6-3f967228e560').
narrative_ontology:cs_kernel_codification('115a384f-9aa7-41b2-a9b6-3f967228e560', fixed_text).
narrative_ontology:cs_authority_grounding('115a384f-9aa7-41b2-a9b6-3f967228e560', extraction).
narrative_ontology:cs_interpretation_layer_present('115a384f-9aa7-41b2-a9b6-3f967228e560').
narrative_ontology:cs_reading_relation('115a384f-9aa7-41b2-a9b6-3f967228e560', quran_ontological_status__created_reading, influences).
narrative_ontology:cs_reading_relation('115a384f-9aa7-41b2-a9b6-3f967228e560', quran_ontological_status__uncreated_reading, forecloses).
narrative_ontology:cs_axiom('115a384f-9aa7-41b2-a9b6-3f967228e560', foundational, caliphate_adjudicates_quran_ontology).
narrative_ontology:cs_axiom_status(caliphate_adjudicates_quran_ontology, overridden).
narrative_ontology:cs_axiom_grounding('115a384f-9aa7-41b2-a9b6-3f967228e560', caliphate_adjudicates_quran_ontology, instrumental).
narrative_ontology:cs_reference_frame('115a384f-9aa7-41b2-a9b6-3f967228e560', mu_tazilite_rationalist_framework).
narrative_ontology:cs_drift_state('115a384f-9aa7-41b2-a9b6-3f967228e560', post_mihna_repudiation, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('115a384f-9aa7-41b2-a9b6-3f967228e560', '').
narrative_ontology:cs_kernel_id(quran_ontological_status__state_enforced_creation_reading, quran_ontological_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_ontological_status__state_enforced_creation_reading, caliphal_authority).
narrative_ontology:constraint_beneficiary(quran_ontological_status__state_enforced_creation_reading, mutazilite_school).
narrative_ontology:constraint_victim(quran_ontological_status__state_enforced_creation_reading, traditionalist_scholars).
narrative_ontology:constraint_victim(quran_ontological_status__state_enforced_creation_reading, literalist_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Abbasid caliphs (al-Maʾmūn, al-Muʿtaṣim, al-Wāthiq) initiate and sustain the miḥna, using state apparatus to impose a theological doctrine. They frame it as protecting rational theology but gain doctrinal control over the ʿulamāʾ class, making religious legitimacy contingent on caliphal favor. They can end the policy at will (al-Mutawakkil does so in 848 CE).
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, caliphal_authority, agenda_setter,
    institutional, generational, arbitrage, universal).

% Muʿtazilite theologians (e.g., Aḥmad ibn Abī Duʾād) gain state patronage, judicial appointments, and enforcement power for their doctrine. Their position becomes official orthodoxy. Their exit is constrained: they depend on caliphal favor; when the miḥna ends, they lose institutional protection and face scholarly marginalization.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, mutazilite_school, beneficiary,
    organized, biographical, constrained, regional).

% Scholars like Aḥmad ibn Ḥanbal refuse to affirm the createdness of the Qurʾān. They face imprisonment, flogging, exclusion from teaching, and social ostracism. Their professional and spiritual identity is fused with the uncreated-Qurʾān doctrine; recantation would dissolve their authority and self-concept. Exit is not merely costly — it is identity-negating.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, traditionalist_scholars, payer,
    powerful, biographical, identity_locked, regional).

% Wider communities of hadith transmitters, jurists, and lay believers who hold the Qurʾān as uncreated eternal speech. They lack the scholarly capital to resist publicly but bear the cost of coerced doctrinal conformity: their religious practice is policed, their teachers persecuted, their epistemic framework delegitimized by state power.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, literalist_communities, payer,
    moderate, generational, trapped, regional).

% The pre-miḥna ecology where multiple theological positions (Muʿtazilite, traditionalist, Shīʿa, Murjiʾite) coexisted and debated without state enforcement. The miḥna collapses this into a binary: affirm the official doctrine or suffer. Pluralism itself is not an agent but a structural good destroyed by the constraint.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, scholarly_pluralism, excluded,
    moderate, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(quran_ontological_status__state_enforced_creation_reading, scholarly_pluralism).

% Post-miḥna Sunnī orthodoxy (crystallized under al-Mutawakkil and later) that repudiates the inquisition, affirms the Qurʾān as uncreated, and enshrines the principle that the caliph does not define doctrine. This seat observes the constraint retrospectively and judges it a deviation.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, later_sunni_consensus, observer,
    institutional, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None genuine. The stated function — unifying the umma under a rational theology — is cover. The actual function is concentrating doctrinal authority in the caliphate, replacing scholarly consensus with state decree.
% TRANSFER_FUNCTION: Moves doctrinal authority and religious legitimacy from the dispersed ʿulamāʾ class to the caliphal office; extracts public affirmation of a contested metaphysical claim from scholars who hold the opposite conviction; transfers institutional resources (judgeships, stipends, teaching posts) to Muʿtazilite loyalists.
% ABSENT_VOICES: Shīʿa theologians (who held distinct views on Qurʾān ontology), Murjiʾite scholars, and the broader Muslim populace who had no access to the theological debate but lived under its enforcement. They are excluded because the miḥna operates as an elite inquisition, not a communal consultation.
% DISAPPEARANCE_RATIONALE: If the miḥna vanished overnight (as it effectively did under al-Mutawakkil), the ʿulamāʾ class reclaims doctrinal authority, the uncreated-Qurʾān position is restored as orthodoxy, Muʿtazilite scholars lose state patronage, and the principle of caliphal non-interference in theology becomes entrenched for centuries.
% FOUNDING_PROBLEM: The early Abbasid caliphate faced competing claims to religious legitimacy: ʿAlid partisans, Khārijite rebels, and a fragmented ʿulamāʾ class. Al-Maʾmūn sought a unifying rational theology that would legitimize caliphal authority as the guardian of true doctrine, superseding sectarian and scholarly fragmentation.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (caliphal legitimization amid fragmentation) is attested by non-beneficiary sources: Shīʿa historiography (e.g., al-Ṭabarī, al-Masʿūdī) documents the political crisis; traditionalist biographies (e.g., Ibn al-Jawzī's Manāqib Aḥmad ibn Ḥanbal) record the scholars' view that the miḥna was political usurpation; modern historians (van Ess, Crone, Hallaq) concur the theological doctrine was instrumentalized for state-building.
narrative_ontology:disappearance_verdict(quran_ontological_status__state_enforced_creation_reading, world_rearranges).
narrative_ontology:founding_problem_status(quran_ontological_status__state_enforced_creation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_ontological_status__state_enforced_creation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quran_ontological_status__state_enforced_creation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quran_ontological_status__state_enforced_creation_reading, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_ontological_status__state_enforced_creation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quran_ontological_status__state_enforced_creation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quran_ontological_status__state_enforced_creation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.82) is very high: the state extracts doctrinal conformity from scholars whose identity is bound to the opposite conviction. Suppression (0.91) is near-maximal: the mechanism is physical coercion and professional exclusion, not persuasion. Theater ratio (0.28) is moderate — the rationalist theology is genuinely held by the Muʿtazilites, but the enforcement theater (public interrogations, forced affirmations) increasingly dominates. Accessibility collapse (0.78) is high: once the inquisition begins, the alternative (open scholarly debate) is structurally closed. Resistance (0.73) is substantial: traditionalist scholars endure torture rather than comply, and popular sympathy shifts against the regime.
 *
 * PERSPECTIVAL GAP:
 *   From the caliphal seat, the constraint appears as rational coordination (imposing theological unity). From the traditionalist seat, it is pure extraction (coerced betrayal of conviction). The engine computes this divergence from the structural data: same constraint, opposite χ values.
 *
 * DIRECTIONALITY LOGIC:
 *   Caliphal authority is the primary beneficiary (d ≈ 0.1) — it gains doctrinal control. Muʿtazilite school is a secondary beneficiary (d ≈ 0.25) — it gains state backing but becomes dependent. Traditionalist scholars are full targets (d ≈ 0.95) — identity-locked, extraction is total. Literalist communities are trapped targets (d ≈ 0.85) — they bear diffuse costs with no exit. Scholarly pluralism is an excluded non-agent. Later Sunni consensus is analytical observer (d = 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (caliphal legitimization) was real but the solution (state enforcement of theology) outlived its function. The miḥna became a snare: the caliphate extracted scholarly submission, the Muʿtazilites extracted institutional privilege, and the victims paid with bodies and careers. When al-Mutawakkil ended it, the mandate was exposed as extractive — the coordination story collapsed, leaving only the extraction record.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mihna_as_pure_extraction_vs_genuine_coordination,
    'Was the miḥna''s stated rationalist theology a genuine coordination attempt (unifying the umma under demonstrable truth) or a cover for caliphal doctrinal capture from the start?',
    'Comparative analysis of al-Maʾmūn''s correspondence with Byzantine emperor (where he debates theology as intellectual equal) vs. his domestic inquisition decrees; also the rapid abandonment of the policy by al-Mutawakkil without theological rebuttal.',
    'If genuine coordination, the constraint is tangled_rope (coordination + extraction); if pure cover, it is snare. The classification hinges on the caliph''s epistemic sincerity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mihna_as_pure_extraction_vs_genuine_coordination, conceptual, 'Whether the coordination function is real or manufactured cover.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (state coercion: prisons, lashes, removal from office) or internalized (scholars'' identity-fusion with the uncreated-Qurʾān doctrine makes the demand itself suppressive)?',
    'Post-miḥna trajectory: traditionalist scholars who resisted did not internalize the createdness doctrine; the suppression ended when the state stopped applying it. This suggests structural suppression dominates.',
    'If internalized suppression is significant, the constraint''s effective suppression persists beyond state enforcement — the target carries it after exit. The data suggests structural dominance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the miḥna.').

omega_variable(
    kernel_reading_boundary,
    'Is the state_enforced_creation_reading a distinct constraint from the created_reading, or merely the created_reading under enforcement conditions?',
    'ε-invariance test: does the created_reading (pure theology) have a different ε when measured without state enforcement? Historical evidence: the Muʿtazilite position existed for decades before the miḥna and persisted after without state backing, with negligible extraction.',
    'Confirms the kernel decomposition: two structurally distinct constraints (created_reading: mountain/rope; state_enforced_creation_reading: snare) linked by network.affects_constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether state enforcement creates a new constraint or merely activates an existing one.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_ontological_status__state_enforced_creation_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(quran_ontological_status__state_enforced_creation_reading_tr_t0, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(quran_ontological_status__state_enforced_creation_reading_tr_t3, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 3, 0.3).
narrative_ontology:measurement(quran_ontological_status__state_enforced_creation_reading_tr_t6, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 6, 0.28).
narrative_ontology:measurement(quran_ontological_status__state_enforced_creation_reading_tr_t9, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 9, 0.28).
narrative_ontology:measurement(quran_ontological_status__state_enforced_creation_reading_tr_t12, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 12, 0.28).
narrative_ontology:measurement(quran_ontological_status__state_enforced_creation_reading_tr_t15, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 15, 0.28).

% Extraction over time
narrative_ontology:measurement(quran_ontological_status__state_enforced_creation_reading_be_t0, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(quran_ontological_status__state_enforced_creation_reading_be_t3, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 3, 0.72).
narrative_ontology:measurement(quran_ontological_status__state_enforced_creation_reading_be_t6, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 6, 0.78).
narrative_ontology:measurement(quran_ontological_status__state_enforced_creation_reading_be_t9, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 9, 0.81).
narrative_ontology:measurement(quran_ontological_status__state_enforced_creation_reading_be_t12, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 12, 0.82).
narrative_ontology:measurement(quran_ontological_status__state_enforced_creation_reading_be_t15, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 15, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(quran_ontological_status__state_enforced_creation_reading_su_t0, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(quran_ontological_status__state_enforced_creation_reading_su_t3, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 3, 0.82).
narrative_ontology:measurement(quran_ontological_status__state_enforced_creation_reading_su_t6, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 6, 0.88).
narrative_ontology:measurement(quran_ontological_status__state_enforced_creation_reading_su_t9, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 9, 0.91).
narrative_ontology:measurement(quran_ontological_status__state_enforced_creation_reading_su_t12, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 12, 0.91).
narrative_ontology:measurement(quran_ontological_status__state_enforced_creation_reading_su_t15, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 15, 0.91).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_ontological_status__state_enforced_creation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(quran_ontological_status__state_enforced_creation_reading, 0.12).
narrative_ontology:affects_constraint(quran_ontological_status__state_enforced_creation_reading, quran_ontological_status__created_reading).
narrative_ontology:affects_constraint(quran_ontological_status__state_enforced_creation_reading, quran_ontological_status__uncreated_reading).

% DUAL FORMULATION NOTE:
% The kernel quran_ontological_status decomposes into three constraints: (1) created_reading — the Muʿtazilite theological claim (Qurʾān is created), which operated as a rope/mountain with negligible extraction before state adoption; (2) state_enforced_creation_reading — this constraint, the same claim weaponized by state power (snare); (3) uncreated_reading — the traditionalist position (Qurʾān is uncreated), which functions as a mountain for its holders (high accessibility_collapse, near-zero resistance) but was the target of extraction during the miḥna. The state_enforced_creation_reading affects both siblings: it forecloses the uncreated_reading during its active interval and influences the created_reading by transforming it from a scholarly position into a political instrument.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quran_ontological_status__state_enforced_creation_reading, powerful, 0.95).
constraint_indexing:directionality_override(quran_ontological_status__state_enforced_creation_reading, moderate, 0.85).
constraint_indexing:directionality_override(quran_ontological_status__state_enforced_creation_reading, institutional, 0.1).
constraint_indexing:directionality_override(quran_ontological_status__state_enforced_creation_reading, organized, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
