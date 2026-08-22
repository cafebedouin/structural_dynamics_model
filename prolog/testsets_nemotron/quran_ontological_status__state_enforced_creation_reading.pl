% ============================================================================
% CONSTRAINT STORY: quran_ontological_status__state_enforced_creation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: State-Enforced Qur'an Createdness (Miḥna)
 *   domain: theological/political
 *
 * SUMMARY:
 *   The miḥna (833–848 CE) was an Abbasid inquisition instituted by Caliph
 *   al-Ma'mun requiring scholars and officials to publicly affirm that the
 *   Qur'an is created (makhlūq) — a Mu'tazilite theological doctrine. What
 *   began as a theological dispute over divine speech became a state
 *   enforcement apparatus: tribunals interrogated scholars, Ahmad ibn Hanbal
 *   was imprisoned and flogged for refusal, and the caliphate claimed the
 *   authority to define orthodoxy. The constraint is the fusion of a
 *   metaphysical claim (createdness) with state coercion (the inquisition).
 *   The Mu'tazilite school gained temporary state patronage; the caliphate
 *   gained a tool for political control over the scholarly class;
 *   traditionalist scholars and pluralism bore the costs. The miḥna ended
 *   under al-Mutawakkil, but its precedent — state power defining theology —
 *   persisted as a structural possibility in Islamic governance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_ontological_status__state_enforced_creation_reading, 0.78).
domain_priors:suppression_score(quran_ontological_status__state_enforced_creation_reading, 0.92).
domain_priors:theater_ratio(quran_ontological_status__state_enforced_creation_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_ontological_status__state_enforced_creation_reading, snare).
narrative_ontology:human_readable(quran_ontological_status__state_enforced_creation_reading, "State-Enforced Qur'an Createdness (Miḥna)").
narrative_ontology:topic_domain(quran_ontological_status__state_enforced_creation_reading, "theological/political").

domain_priors:requires_active_enforcement(quran_ontological_status__state_enforced_creation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_ontological_status__state_enforced_creation_reading, '5a3d97ce-5deb-461c-8d3e-e3c3f4b7eb56').
narrative_ontology:cs_kernel_codification('5a3d97ce-5deb-461c-8d3e-e3c3f4b7eb56', fixed_text).
narrative_ontology:cs_authority_grounding('5a3d97ce-5deb-461c-8d3e-e3c3f4b7eb56', extraction).
narrative_ontology:cs_interpretation_layer_present('5a3d97ce-5deb-461c-8d3e-e3c3f4b7eb56').
narrative_ontology:cs_reading_relation('5a3d97ce-5deb-461c-8d3e-e3c3f4b7eb56', quran_ontological_status__created_reading, influences).
narrative_ontology:cs_reading_relation('5a3d97ce-5deb-461c-8d3e-e3c3f4b7eb56', quran_ontological_status__uncreated_reading, forecloses).
narrative_ontology:cs_axiom('5a3d97ce-5deb-461c-8d3e-e3c3f4b7eb56', foundational, caliph_defines_orthodoxy).
narrative_ontology:cs_axiom_status(caliph_defines_orthodoxy, overridden).
narrative_ontology:cs_axiom_grounding('5a3d97ce-5deb-461c-8d3e-e3c3f4b7eb56', caliph_defines_orthodoxy, conventional).
narrative_ontology:cs_axiom('5a3d97ce-5deb-461c-8d3e-e3c3f4b7eb56', foundational, quran_is_created_makhluq).
narrative_ontology:cs_axiom_status(quran_is_created_makhluq, overridden).
narrative_ontology:cs_axiom_grounding('5a3d97ce-5deb-461c-8d3e-e3c3f4b7eb56', quran_is_created_makhluq, empirically_contingent).
narrative_ontology:cs_reference_frame('5a3d97ce-5deb-461c-8d3e-e3c3f4b7eb56', caliphal_theological_arbitration).
narrative_ontology:cs_drift_state('5a3d97ce-5deb-461c-8d3e-e3c3f4b7eb56', post_mihna_abolition, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('5a3d97ce-5deb-461c-8d3e-e3c3f4b7eb56', '').
narrative_ontology:cs_kernel_id(quran_ontological_status__state_enforced_creation_reading, quran_ontological_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_ontological_status__state_enforced_creation_reading, abbasid_caliphate_authority).
narrative_ontology:constraint_beneficiary(quran_ontological_status__state_enforced_creation_reading, mutazilite_school).
narrative_ontology:constraint_victim(quran_ontological_status__state_enforced_creation_reading, traditionalist_scholars).
narrative_ontology:constraint_victim(quran_ontological_status__state_enforced_creation_reading, literalist_communities).
narrative_ontology:constraint_victim(quran_ontological_status__state_enforced_creation_reading, scholarly_pluralism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Caliphs al-Ma'mun, al-Mu'tasim, and al-Wathiq instituted the miḥna as an inquisition requiring public affirmation of Qur'an createdness. They used doctrinal enforcement to consolidate caliphal authority over religious legitimacy, marginalize traditionalist opposition, and align the scholarly class with state policy. The caliphate collects political capital and ideological control; exit is arbitrage-grade — the state can modify or abandon the policy at will.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, abbasid_caliphate_authority, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(quran_ontological_status__state_enforced_creation_reading, abbasid_caliphate_authority, beneficiary).

% Mu'tazilite theologians (notably Ahmad ibn Abi Du'ad) gained state patronage, judicial appointments, and institutional dominance during the miḥna. Their rationalist theology became official doctrine. Their exit is constrained — they are intellectually committed to createdness but politically dependent on state enforcement; abandoning the doctrine would cost them their patronage and coherence.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, mutazilite_school, beneficiary,
    organized, biographical, constrained, continental).

% Scholars like Ahmad ibn Hanbal refused to affirm createdness, facing imprisonment, flogging, and exclusion from public office. Their resistance was grounded in hadith-based theology and the conviction that the Qur'an is God's uncreated speech. Exit is identity-locked: their scholarly identity, communal authority, and soteriological framework are fused with the uncreatedness doctrine; recantation would dissolve their self-concept and communal standing.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, traditionalist_scholars, payer,
    organized, biographical, identity_locked, continental).

% Wider communities of hadith transmitters, jurisprudence students, and lay believers who upheld the uncreated Qur'an faced state suspicion, surveillance, and social marginalization. They lacked the institutional platform of leading scholars but bore the diffuse costs of doctrinal policing. Exit is trapped: geographic mobility within the caliphate did not escape the inquisition's reach, and emigration meant abandoning home, lineage, and communal networks.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, literalist_communities, payer,
    moderate, generational, trapped, continental).

% The miḥna suppressed the pre-existing pluralism of kalām discourse, replacing competitive theological debate with a single state-mandated orthodoxy. The cost is the loss of interpretive diversity, the chilling effect on speculative theology, and the precedent of doctrinal coercion. As a non-agent entity, it bears costs structurally but has no subjective exit; its 'constrained' exit reflects the historical difficulty of restoring pluralism after state enforcement ends.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, scholarly_pluralism, payer,
    organized, civilizational, constrained, continental).
narrative_ontology:stakeholder_non_agent(quran_ontological_status__state_enforced_creation_reading, scholarly_pluralism).

% The post-miḥna synthesis (ash'arism/maturidism affirming uncreatedness while rejecting Hanbali literalism) emerged as the dominant Sunni position. This analytical seat observes the constraint's long-term trajectory: the state's enforcement failed, the createdness doctrine was abandoned, and the traditionalist resistance became foundational to Sunni orthodoxy. It neither collects nor pays; it reads the structural outcome.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, post_mihna_sunni_consensus, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(quran_ontological_status__state_enforced_creation_reading, post_mihna_sunni_consensus).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The miḥna presented itself as coordinating theological consensus (ijma') on a disputed metaphysical question — the ontological status of the Qur'an — to unify the ummah under a single rationalist doctrine endorsed by the caliph as God's deputy.
% TRANSFER_FUNCTION: Moves political legitimacy and doctrinal authority from the traditionalist scholarly class (rooted in hadith transmission and communal consensus) to the caliphal state and its allied rationalist theologians. The transfer is enforced through judicial tribunals that compel public affirmation under threat of corporal punishment and professional exclusion.
% ABSENT_VOICES: The broader Muslim populace (awām) who were not consulted on the doctrinal imposition; early mutakallimūn who held intermediate positions (e.g., Jahmite or Kullabi views) and were squeezed out by the binary enforcement; non-Sunni communities (Shi'a, Kharijites) whose own Qur'an doctrines were irrelevant to the Sunni inquisition but who experienced the precedent of state doctrinal policing.
% DISAPPEARANCE_RATIONALE: If the miḥna vanished overnight (as it effectively did under al-Mutawakkil), the caliphal claim to define orthodoxy collapses, the Mu'tazilite school loses state patronage and fractures, traditionalist scholars regain public authority, and the trajectory of Sunni theology shifts decisively toward the uncreated-Qur'an consensus that defined the next millennium. The arrangement's disappearance rearranges the theological-political landscape.
% FOUNDING_PROBLEM: The early Abbasid caliphate faced a legitimacy crisis: the Alid challenge (claiming prophetic lineage), the fragmentation of religious authority among competing scholarly networks, and the need to integrate Hellenistic philosophy into Islamic theology without fracturing the community. The miḥna was built to solve this by making the caliph the arbiter of orthodoxy through a rationally demonstrable doctrine (createdness).
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (caliphal legitimization through doctrinal arbitration) is attested as dead by the historical outcome: al-Mutawakkil's abolition of the miḥna (848 CE) and the subsequent centuries of Sunni practice where caliphs never again claimed the power to define theology. Corroboration comes from the traditionalist scholars who resisted (Ibn Hanbal's circle), the post-miḥna Ash'arite theologians who systematized the uncreated-Qur'an position without state enforcement, and modern historians (e.g., van Ess, Crone, Hallaq) who document the miḥna as a failed experiment in state theology — all sources outside the Mu'tazilite beneficiaries.
narrative_ontology:disappearance_verdict(quran_ontological_status__state_enforced_creation_reading, world_rearranges).
narrative_ontology:founding_problem_status(quran_ontological_status__state_enforced_creation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_ontological_status__state_enforced_creation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(quran_ontological_status__state_enforced_creation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quran_ontological_status__state_enforced_creation_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.78) is high because the constraint transfers doctrinal authority and material resources (appointments, safety, communal standing) from traditionalists to the state/Mu'tazilite alliance. Suppression (0.92) is near-maximum: the inquisition used corporal punishment, imprisonment, professional exclusion, and social stigma to compel affirmation — alternatives were not merely discouraged but actively crushed. Theater ratio (0.25) is low-moderate: the theological debate was genuine, but the enforcement machinery far exceeded any coordinative need; the performative component is the caliph's self-presentation as 'protector of true doctrine.' Accessibility collapse (0.35) is moderate: the createdness doctrine did not collapse all alternatives (traditionalist networks persisted underground), but the public sphere was effectively closed to dissent. Resistance (0.85) is very high: the traditionalist refusal, centered on Ibn Hanbal, became a defining episode of scholarly steadfastness.
 *
 * PERSPECTIVAL GAP:
 *   From the caliphal seat, the miḥna appears as a rational coordination mechanism: the caliph resolves a theological dispute that threatens unity, using the best available philosophy. From the traditionalist seat, it is a snare: state violence imposing a false doctrine to break independent scholarly authority. From the Mu'tazilite seat, it is a tangled rope: genuine theological conviction coordinated with state power — they believe createdness is true AND benefit from its enforcement. The engine computes these divergences from the structural data; the claimed type (snare) reflects the dominant structural reality (extraction + suppression).
 *
 * DIRECTIONALITY LOGIC:
 *   The Abbasid caliphate is the primary beneficiary (agenda_setter + beneficiary): it sets the doctrine, enforces it, and extracts political legitimacy — directionality d ≈ 0.1 (near beneficiary). The Mu'tazilite school is a secondary beneficiary: it gains state backing but is intellectually committed to the doctrine independently — d ≈ 0.25. Traditionalist scholars are primary payers: they bear corporal punishment, imprisonment, and exclusion; their identity-locked exit (d ≈ 0.95) makes them near-full targets. Literalist communities are diffuse payers with trapped exit (d ≈ 0.9). Scholarly pluralism is a structural payer (non-agent) bearing the cost of lost diversity — d ≈ 0.8. The analytical observer seat has d = 0.5 by definition.
 *
 * MANDATROPHY ANALYSIS:
 *   The miḥna's founding problem (caliphal legitimization via doctrinal arbitration) died with the miḥna itself — the Abbasids never again claimed theological arbitration power. The constraint persisted only during the 15-year enforcement window; it did not become a piton (no inertial persistence). The mandatrophy is resolved: the arrangement's function (legitimizing the caliph through theology) was recognized as failed and abandoned. The residue is the precedent — later rulers occasionally invoked doctrinal enforcement, but never as systematically. The founding_problem_status = dead + disappearance_verdict = world_rearranges mismatch flags this as a captured constraint that was successfully dismantled.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_frame_fork,
    'Is the miḥna a single constraint (theological doctrine + state enforcement fused) or two distinct constraints (the createdness doctrine as one, the inquisition apparatus as another)?',
    'Apply the ε-invariance test: if measuring the createdness doctrine alone yields low extraction (it is a theological position held voluntarily by Mu''tazilites) but measuring the miḥna yields high extraction (state coercion), they are distinct constraints. The kernel context declares this reading as the FUSED constraint — the enforcement is constitutive of this reading''s structure.',
    'If fused: this story is a snare with ε=0.78. If split: ''created_reading'' is a rope/tangled_rope (coordination among rationalists) and ''mihna_enforcement'' is a snare (pure extraction). The committer frame mandates the fused reading here; the sibling ''created_reading'' handles the doctrine-only constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_fork, conceptual, 'Whether the kernel reading''s structural identity includes the enforcement apparatus or only the doctrine.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Was the miḥna''s suppression primarily structural (state tribunals, corporal punishment, professional exclusion) or did it internalize into the scholarly class (self-censorship, doctrinal conformity persisting after the miḥna ended)?',
    'Post-miḥna trajectory: if scholarly discourse remained chilled on createdness after 848 CE without active tribunals, internalization occurred. Compare Ash''arite adoption of uncreatedness (voluntary) vs. the silencing of speculative kalām in some traditionalist circles (internalized).',
    'If internalized, effective suppression exceeds the structural measure — the constraint''s shadow persists after the enforcement apparatus is dismantled. This would elevate the miḥna''s legacy from a time-bounded snare to a structural deformation of the scholarly field.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in the miḥna''s aftermath.').

omega_variable(
    mutazilite_beneficiary_contingency,
    'Did the Mu''tazilite school genuinely benefit from state enforcement, or did the alliance ultimately discredit them and accelerate their decline?',
    'Counterfactual: trace the Mu''tazilite school''s trajectory with vs. without state patronage. Historically, the miḥna''s failure became a primary argument against rationalist theology (''the Mu''tazilites needed the sword''), and Ash''arism rose by synthesizing rationalism with traditionalism. The beneficiary declaration may be true ex ante but false ex post.',
    'If the alliance was net-harmful to Mu''tazilites, the beneficiary structure is more complex: the state extracted from traditionalists AND from the Mu''tazilites'' long-term credibility. This would shift the constraint toward a pure snare where even the apparent beneficiaries are ultimately extracted from.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mutazilite_beneficiary_contingency, empirical, 'Whether the Mu''tazilite school''s state alliance was net-beneficial or net-extractive over the long term.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_ontological_status__state_enforced_creation_reading, 833, 848).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t833, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 833, 0.15).
narrative_ontology:measurement(qura_tr_t837, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 837, 0.2).
narrative_ontology:measurement(qura_tr_t841, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 841, 0.25).
narrative_ontology:measurement(qura_tr_t845, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 845, 0.3).
narrative_ontology:measurement(qura_tr_t848, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 848, 0.25).

% Extraction over time
narrative_ontology:measurement(qura_be_t833, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 833, 0.65).
narrative_ontology:measurement(qura_be_t837, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 837, 0.72).
narrative_ontology:measurement(qura_be_t841, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 841, 0.78).
narrative_ontology:measurement(qura_be_t845, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 845, 0.81).
narrative_ontology:measurement(qura_be_t848, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 848, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t833, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 833, 0.85).
narrative_ontology:measurement(qura_su_t837, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 837, 0.88).
narrative_ontology:measurement(qura_su_t841, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 841, 0.92).
narrative_ontology:measurement(qura_su_t845, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 845, 0.92).
narrative_ontology:measurement(qura_su_t848, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 848, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_ontological_status__state_enforced_creation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(quran_ontological_status__state_enforced_creation_reading, 0.12).
narrative_ontology:affects_constraint(quran_ontological_status__state_enforced_creation_reading, quran_ontological_status__created_reading).
narrative_ontology:affects_constraint(quran_ontological_status__state_enforced_creation_reading, quran_ontological_status__uncreated_reading).
narrative_ontology:affects_constraint(quran_ontological_status__state_enforced_creation_reading, asharite_synthesis).
narrative_ontology:affects_constraint(quran_ontological_status__state_enforced_creation_reading, sunni_orthodoxy_formation).

% DUAL FORMULATION NOTE:
% This constraint is the state-enforced reading of the quran_ontological_status kernel. The created_reading isolates the Mu'tazilite doctrine without state power; the uncreated_reading is the traditionalist/Ash'arite position. This reading's enforcement apparatus structurally pressured both siblings: it suppressed the uncreated_reading's public expression and, by failing, discredited the created_reading's political viability. The network edges capture this structural influence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quran_ontological_status__state_enforced_creation_reading, organized, 0.25).
constraint_indexing:directionality_override(quran_ontological_status__state_enforced_creation_reading, moderate, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
