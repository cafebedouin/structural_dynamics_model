% ============================================================================
% CONSTRAINT STORY: quranic_gender_verses__progressive_abrogation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quranic_gender_verses__progressive_abrogation, []).

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
 *   constraint_id: quranic_gender_verses__progressive_abrogation
 *   human_readable: Progressive-Abrogation Reading of Qur'anic Gender Verses (naskh via 49:13)
 *   domain: religious/legal/gender
 *
 * SUMMARY:
 *   This story instantiates ONE reading within the contested
 *   quranic_gender_verses kernel: the progressive-abrogation reading, which
 *   holds that gender-specific verses (4:11, 2:282, 4:34) are superseded by
 *   later-revealed universal dignity principles (49:13) via the doctrinal
 *   mechanism of naskh. This is not a claim about the Qur'an as a whole or
 *   about Islamic jurisprudence generically — it is the specific structural
 *   claim that abrogation logic applies across chronological revelation to
 *   override gender-differentiated legal verses. The sibling readings
 *   (literal_hierarchical, contextual_egalitarian) are separate constraints
 *   with their own epsilon and stakeholder structures; they are referenced
 *   here only via network links and cs_structure.reading_relations, never
 *   absorbed into this constraint's own classification.
 *
 * KEY AGENTS:
 *   - reformist_women_scholars: agenda-setting beneficiaries advancing the abrogation argument, constrained exit from traditional institutions
 *   - muslim_women_seeking_legal_parity: primary intended beneficiary class, powerless, exit contingent on jurisdiction
 *   - traditionalist_ulama: institutional payers whose authority is comprehensively delegitimized by this reading, trapped exit
 *   - literalist_seminary_institutions: institutional payers facing curricular and financial threat, trapped exit
 *   - communities_identity_bound_to_literal_reading: payers experiencing the reading as epistemic violence against a fused religious identity, identity-locked exit
 *   - state_personal_status_courts: excluded administrative seat that would bear implementation cost without a voice in the theological contest
 *   - comparative_islamicists: analytical observer seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quranic_gender_verses__progressive_abrogation, 0.81).
domain_priors:suppression_score(quranic_gender_verses__progressive_abrogation, 0.68).
domain_priors:theater_ratio(quranic_gender_verses__progressive_abrogation, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, extractiveness, 0.81).
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(quranic_gender_verses__progressive_abrogation, resistance, 0.87).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quranic_gender_verses__progressive_abrogation, tangled_rope).
narrative_ontology:human_readable(quranic_gender_verses__progressive_abrogation, "Progressive-Abrogation Reading of Qur'anic Gender Verses (naskh via 49:13)").
narrative_ontology:topic_domain(quranic_gender_verses__progressive_abrogation, "religious/legal/gender").

domain_priors:requires_active_enforcement(quranic_gender_verses__progressive_abrogation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quranic_gender_verses__progressive_abrogation, '6d461e0e-15d0-46ad-a986-76cbd04c296b').
narrative_ontology:cs_kernel_codification('6d461e0e-15d0-46ad-a986-76cbd04c296b', fixed_text).
narrative_ontology:cs_authority_grounding('6d461e0e-15d0-46ad-a986-76cbd04c296b', lineage).
narrative_ontology:cs_interpretation_layer_present('6d461e0e-15d0-46ad-a986-76cbd04c296b').
narrative_ontology:cs_reading_relation('6d461e0e-15d0-46ad-a986-76cbd04c296b', quranic_gender_verses__literal_hierarchical, forecloses).
narrative_ontology:cs_reading_relation('6d461e0e-15d0-46ad-a986-76cbd04c296b', quranic_gender_verses__contextual_egalitarian, coexists_with).
narrative_ontology:cs_axiom('6d461e0e-15d0-46ad-a986-76cbd04c296b', foundational, later_revelation_can_void_earlier_ruling).
narrative_ontology:cs_axiom_status(later_revelation_can_void_earlier_ruling, holdable).
narrative_ontology:cs_axiom_grounding('6d461e0e-15d0-46ad-a986-76cbd04c296b', later_revelation_can_void_earlier_ruling, conventional).
narrative_ontology:cs_axiom('6d461e0e-15d0-46ad-a986-76cbd04c296b', foundational, universal_dignity_principle_has_overriding_legal_force).
narrative_ontology:cs_axiom_status(universal_dignity_principle_has_overriding_legal_force, holdable).
narrative_ontology:cs_axiom_grounding('6d461e0e-15d0-46ad-a986-76cbd04c296b', universal_dignity_principle_has_overriding_legal_force, deontological).
narrative_ontology:cs_reference_frame('6d461e0e-15d0-46ad-a986-76cbd04c296b', classical_naskh_doctrine_of_legal_abrogation).
narrative_ontology:cs_drift_state('6d461e0e-15d0-46ad-a986-76cbd04c296b', contemporary_reform_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6d461e0e-15d0-46ad-a986-76cbd04c296b', '').
narrative_ontology:cs_kernel_id(quranic_gender_verses__progressive_abrogation, quranic_gender_verses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quranic_gender_verses__progressive_abrogation, reformist_women_scholars).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__progressive_abrogation, muslim_women_seeking_legal_parity).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__progressive_abrogation, modernist_islamic_jurists).
narrative_ontology:constraint_victim(quranic_gender_verses__progressive_abrogation, traditionalist_ulama).
narrative_ontology:constraint_victim(quranic_gender_verses__progressive_abrogation, literalist_seminary_institutions).
narrative_ontology:constraint_victim(quranic_gender_verses__progressive_abrogation, communities_identity_bound_to_literal_reading).
narrative_ontology:constraint_vindicates(quranic_gender_verses__progressive_abrogation, quranic_moral_arc_toward_universal_dignity).
narrative_ontology:constraint_vindicates(quranic_gender_verses__progressive_abrogation, naskh_as_doctrinally_legitimate_mechanism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advance the abrogation argument in academic and activist venues, arguing 49:13's universal dignity principle chronologically and normatively supersedes earlier gender-differentiated verses. They gain doctrinal ground for full legal parity claims but face professional marginalization, accusations of heterodoxy, and exclusion from most traditional certifying bodies (ijaza chains) if they press the argument publicly.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, reformist_women_scholars, agenda_setter,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(quranic_gender_verses__progressive_abrogation, reformist_women_scholars, beneficiary).

% Stand to gain full parity in inheritance, testimony, and guardianship if the abrogation reading were adopted by courts or communities. Currently must navigate personal-status law regimes built on the literal reading; their exit options depend entirely on which jurisdictional or communal authority they are subject to, which they mostly cannot choose.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, muslim_women_seeking_legal_parity, beneficiary,
    powerless, biographical, constrained, national).

% Institutionally positioned in reform-oriented seminaries, NGOs, and some state fatwa councils. They deploy the abrogation framework to authorize new fiqh rulings and gain influence, funding, and international legitimacy from governments and donors favoring reform; they can relocate between institutions that will host this reading.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, modernist_islamic_jurists, beneficiary,
    organized, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(quranic_gender_verses__progressive_abrogation, modernist_islamic_jurists, agenda_setter).

% Hold their entire scholarly authority, curricular legitimacy, and communal standing on the premise that 4:11, 2:282, and 4:34 are direct, non-abrogated legal ordinances. The abrogation reading, if it gained traction, would delegitimize centuries of accumulated tafsir and usul al-fiqh reasoning they embody; they cannot exit this position without abandoning the basis of their institutional authority.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, traditionalist_ulama, payer,
    institutional, civilizational, trapped, global).

% Their curricula, certification pipelines, and funding from traditionalist patrons all depend on the literal-hierarchical reading remaining authoritative. Adoption of the abrogation reading elsewhere in the ummah threatens their graduates' recognized authority and their institutional revenue base; they cannot pivot without dissolving their founding mandate.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, literalist_seminary_institutions, payer,
    institutional, civilizational, trapped, regional).

% Have organized family structure, inheritance expectations, and communal self-understanding around the literal reading as unmediated divine command. Being told this reading is superseded is experienced as an attack on the coherence of their entire moral world, not merely a legal reinterpretation; their identity is fused with the practice such that abandoning it is not a simple cost-benefit exit.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, communities_identity_bound_to_literal_reading, payer,
    moderate, generational, identity_locked, regional).

% Administer inheritance, marriage, and testimony law according to codified fiqh, largely built on the literal reading. They are rarely present in the theological debate over abrogation itself even though any doctrinal shift would require them to rewrite statutory family law; their administrative burden is not represented in the scholarly contest.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, state_personal_status_courts, excluded,
    institutional, generational, constrained, national).

% Study the abrogation, contextualist, and literalist readings as competing hermeneutic traditions without adjudicating between them theologically. They document how each reading's adoption correlates with institutional power shifts, funding flows, and legal reform outcomes.
narrative_ontology:constraint_stakeholder(quranic_gender_verses__progressive_abrogation, comparative_islamicists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quranic_gender_verses__progressive_abrogation, diffuse).
narrative_ontology:fixing_cost_class(quranic_gender_verses__progressive_abrogation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a doctrinally internal mechanism (naskh) for reconciling apparently conflicting revelation, allowing reformers to argue for legal change from within recognized usul al-fiqh methodology rather than from outside the tradition — this genuinely solves the problem of how change can occur without appealing to secular or external authority.
% TRANSFER_FUNCTION: Moves interpretive and institutional authority from traditionalist ulama and literalist seminaries toward reformist scholars and modernist jurisprudential bodies; moves legal entitlement (inheritance shares, testimony weight, guardianship rights) toward women as a class, contingent on adoption by courts or communities.
% ABSENT_VOICES: State personal-status courts, who would have to operationalize any doctrinal shift into statute and case law, are largely absent from the theological contest over abrogation itself. Ordinary practicing Muslims who hold the literal reading as devotional certainty (not merely legal doctrine) are rarely surveyed directly in scholarly abrogation debates conducted in academic and elite clerical registers.
% DISAPPEARANCE_RATIONALE: If the progressive-abrogation reading vanished as a live doctrinal option, reform movements grounding legal-parity claims in internal Qur'anic hermeneutics would lose their primary textual argument, shifting reform advocacy toward external human-rights framing or maqasid-based contextualism instead; conversely, traditionalist institutions would face one fewer internal legitimacy challenge and could consolidate curricular authority further.
% FOUNDING_PROBLEM: How can Muslim communities reconcile gender-differentiated verses with an increasingly felt tension against universal human dignity claims (from within the Qur'an itself, e.g. 49:13) without abandoning revelation as the source of authority?
% FOUNDING_PROBLEM_CORROBORATION: Reformist scholars and some modernist jurists attest the founding problem is live and unresolved by traditional fiqh. Traditionalist ulama dispute that any such tension exists, holding the verses as complementary rather than contradictory — a corroboration entirely internal to the reformist camp; comparative Islamicists (an analytical, non-beneficiary seat) corroborate only that the tension is sociologically real as experienced by many practicing Muslims, without adjudicating whether naskh is the correct doctrinal resolution.
narrative_ontology:disappearance_verdict(quranic_gender_verses__progressive_abrogation, world_rearranges).
narrative_ontology:founding_problem_status(quranic_gender_verses__progressive_abrogation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quranic_gender_verses__progressive_abrogation, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(quranic_gender_verses__progressive_abrogation, 'none', 1).
narrative_ontology:epsilon_provenance(quranic_gender_verses__progressive_abrogation, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quranic_gender_verses__progressive_abrogation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quranic_gender_verses__progressive_abrogation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quranic_gender_verses__progressive_abrogation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored very high (0.81 at interval end) because adoption of this reading produces a complete normative reversal: centuries of accumulated legal authority built on the literal reading is rendered doctrinally obsolete, and that authority (curricular, financial, communal) is transferred rather than merely contested. Suppression is authored substantial but lower than extraction (0.68) because this reading does not (yet) hold state-backed enforcement power in most jurisdictions — its suppressive force operates mainly through delegitimization pressure on traditionalist institutions and reputational costs for scholars who reject it in reform-aligned spaces, not through comprehensive coercive apparatus. Theater ratio is modest (0.28) and rising, reflecting that the doctrinal argument is substantively engaged in most venues but is increasingly also performed for donor and international-legitimacy audiences rather than argued to persuade traditionalist interlocutors. Accessibility collapse is only moderate (0.42) because literal and contextualist readings remain fully available and actively practiced alternatives — this reading has not achieved anything like natural-law-style closure of alternatives. Resistance is authored very high (0.87): this is among the most actively contested claims in contemporary Islamic legal thought, met by sustained, organized, well-resourced traditionalist counter-argument.
 *
 * PERSPECTIVAL GAP:
 *   From the reformist and modernist seat, this reading is coordination: a doctrinally faithful mechanism resolving real scriptural tension while extending justice. From the traditionalist ulama and literalist seminary seat, the identical structure computes as comprehensive extraction of their accumulated authority and legitimacy, imposed through argument rather than force but experienced as existentially threatening. The engine's per-seat computation should reflect this asymmetry from the structural data (power atoms, exit options, beneficiary/victim declarations) rather than from any narrative adjudication of which side is theologically correct — this constraint story deliberately does not adjudicate the underlying theological question.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformist scholars and modernist jurists are structural beneficiaries: they gain interpretive authority, institutional positioning, and (for modernist jurists) donor/state legitimacy from advancing this reading (d near beneficiary end). Muslim women seeking legal parity are the intended beneficiary class but sit closer to symmetric in practice because their actual legal entitlements depend on downstream adoption by courts they do not control (d moderate, contingent). Traditionalist ulama and literalist seminary institutions are full targets — their entire institutional and epistemic capital is what this reading extracts value from by delegitimizing it (d near target end), and their exit is trapped because abandoning the literal reading dissolves the basis of their authority. Communities identity-bound to the literal reading are targets whose cost is not primarily material but ontological — the reading, if adopted, does not merely change law but destabilizes a coherent religious self-understanding; this warrants identity_locked exit options rather than merely constrained.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling gender-differentiated verses with felt tension against universal dignity claims — remains genuinely live and contested rather than resolved or obsolete; this is not a mandatrophy case where a solved problem persists as inertial extraction. The tangled_rope classification (rather than snare) is deliberate: this reading does perform a real coordination function (offering an internally legitimate, non-external mechanism for doctrinal change, avoiding a rupture with revelation as authority) even as it imposes severe, asymmetric costs on traditionalist institutions and identity-bound communities. Calling it a pure snare would erase the genuine hermeneutic work naskh does within usul al-fiqh; calling it a pure rope would erase the comprehensive delegitimization and institutional stakes involved. The active enforcement requirement is met through the sustained argumentative and institutional pressure this reading exerts on traditionalist legitimacy — not through state coercion, but through a real and organized campaign to displace incumbent authority structures.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naskh_applicability_to_this_verse_set,
    'Does the classical doctrine of naskh (originally developed for legal rulings with clear textual contradiction and known chronological order) validly apply to gender verses whose chronology and reconciliation classical scholars did not treat as abrogation candidates?',
    'Close comparison with classical usul al-fiqh criteria for identifying valid abrogation cases (clear contradiction, established chronology, absence of reconciliation possibility) against how classical exegetes actually treated 4:11, 2:282, 4:34, and 49:13.',
    'If the classical criteria for naskh do not fit this verse set, the reading is a novel doctrinal extension dressed in traditional vocabulary — closer to reinterpretation-as-abrogation than genuine abrogation, which would reframe (though not necessarily delegitimize) the reform argument.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naskh_applicability_to_this_verse_set, conceptual, 'Whether naskh doctrine, as classically constrained, actually covers this verse set.').

omega_variable(
    reading_as_kernel_committer_choice,
    'Given that literal_hierarchical, contextual_egalitarian, and progressive_abrogation all read the same kernel text differently, is the choice among them determined by prior commitments about revelation''s nature (timeless vs. developmental) rather than by textual evidence alone?',
    'Trace whether adherents of each reading converge or diverge based on independently held views about progressive revelation versus timeless divine command, tested against cases where the same scholar''s broader theology predicts their gender-verse reading.',
    'If reading choice is substantially predicted by prior theological commitment rather than textual argument alone, the contest between readings is partly a proxy for a deeper unresolved dispute about the nature of revelation itself, which no amount of verse-level argument within this reading can resolve.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_as_kernel_committer_choice, conceptual, 'Whether the reading choice reduces to prior commitments about the nature of revelation.').

omega_variable(
    institutional_versus_doctrinal_delegitimization,
    'Is the extraction this reading imposes on traditionalist ulama primarily doctrinal (their theological reasoning is shown to be incorrect) or institutional (their social and financial authority is displaced regardless of the argument''s merit)?',
    'Examine cases where the abrogation reading gained doctrinal acceptance without institutional displacement (or vice versa) to separate the two effects empirically.',
    'If institutional displacement occurs independent of doctrinal persuasion, the extraction is better modeled as a power transfer riding on a doctrinal argument rather than a consequence of the argument''s soundness — sharpening the tangled_rope classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_versus_doctrinal_delegitimization, empirical, 'Whether extraction from traditionalist institutions tracks doctrinal persuasion or institutional power shift.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quranic_gender_verses__progressive_abrogation, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quranic_gender_verses__progressive_abrogation, theater_ratio, 0, 0.1).
narrative_ontology:measurement(qura_tr_t8, quranic_gender_verses__progressive_abrogation, theater_ratio, 8, 0.14).
narrative_ontology:measurement(qura_tr_t16, quranic_gender_verses__progressive_abrogation, theater_ratio, 16, 0.18).
narrative_ontology:measurement(qura_tr_t24, quranic_gender_verses__progressive_abrogation, theater_ratio, 24, 0.22).
narrative_ontology:measurement(qura_tr_t32, quranic_gender_verses__progressive_abrogation, theater_ratio, 32, 0.26).
narrative_ontology:measurement(qura_tr_t40, quranic_gender_verses__progressive_abrogation, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quranic_gender_verses__progressive_abrogation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(qura_be_t8, quranic_gender_verses__progressive_abrogation, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(qura_be_t16, quranic_gender_verses__progressive_abrogation, base_extractiveness, 16, 0.6).
narrative_ontology:measurement(qura_be_t24, quranic_gender_verses__progressive_abrogation, base_extractiveness, 24, 0.7).
narrative_ontology:measurement(qura_be_t32, quranic_gender_verses__progressive_abrogation, base_extractiveness, 32, 0.77).
narrative_ontology:measurement(qura_be_t40, quranic_gender_verses__progressive_abrogation, base_extractiveness, 40, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quranic_gender_verses__progressive_abrogation, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(qura_su_t8, quranic_gender_verses__progressive_abrogation, suppression_requirement, 8, 0.48).
narrative_ontology:measurement(qura_su_t16, quranic_gender_verses__progressive_abrogation, suppression_requirement, 16, 0.55).
narrative_ontology:measurement(qura_su_t24, quranic_gender_verses__progressive_abrogation, suppression_requirement, 24, 0.6).
narrative_ontology:measurement(qura_su_t32, quranic_gender_verses__progressive_abrogation, suppression_requirement, 32, 0.65).
narrative_ontology:measurement(qura_su_t40, quranic_gender_verses__progressive_abrogation, suppression_requirement, 40, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quranic_gender_verses__progressive_abrogation, identity_coordination).
narrative_ontology:affects_constraint(quranic_gender_verses__progressive_abrogation, quranic_gender_verses__literal_hierarchical).
narrative_ontology:affects_constraint(quranic_gender_verses__progressive_abrogation, quranic_gender_verses__contextual_egalitarian).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the quranic_gender_verses kernel, decomposed per the ε-invariance principle because the three readings produce structurally distinct extractiveness, victim sets, and enforcement profiles from the same underlying text. literal_hierarchical treats the verses as direct timeless ordinance (low extractiveness from its own internal frame, high extractiveness as experienced by reform-seeking women). contextual_egalitarian reaches similar outcomes to this reading via maqasid rather than abrogation, producing lower delegitimization of traditionalist authority since it does not claim the earlier verses are void. progressive_abrogation (this story) claims the highest extractiveness of the three reform-adjacent readings because naskh asserts complete supersession, not merely contextual reapplication — traditionalist authority is not reinterpreted but rendered doctrinally obsolete. All three stories link to each other via affects_constraints; none absorbs another's classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
