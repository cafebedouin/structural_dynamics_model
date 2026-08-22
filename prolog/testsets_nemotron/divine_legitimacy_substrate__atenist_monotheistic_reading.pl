% ============================================================================
% CONSTRAINT STORY: divine_legitimacy_substrate__atenist_monotheistic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_legitimacy_substrate__atenist_monotheistic_reading, []).

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
 *   constraint_id: divine_legitimacy_substrate__atenist_monotheistic_reading
 *   human_readable: Atenist Monotheistic Revelation as Exclusive Legitimacy Substrate
 *   domain: ancient_history/religious_studies/political_economy_of_belief_systems
 *
 * SUMMARY:
 *   Akhenaten's Atenist revolution (c. 1353–1336 BCE) imposed a monotheistic
 *   revelation claiming Aten as the sole deity, with the pharaoh as exclusive
 *   intermediary. The constraint dismantled the Amun priesthood's temple
 *   economy, closed traditional temples, erased divine names, and redirected
 *   resources to the new Aten institutions. The claim that this arrangement
 *   was a genuine cosmological revelation (mountain) is the operator's
 *   framing; the authored metrics describe an extraction apparatus that
 *   concentrated temple wealth, eliminated rival priestly power, and required
 *   escalating enforcement to maintain. The engine will compute per-seat
 *   types from the structural data; the claimed_type (snare) reflects the
 *   author's structural assessment.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.78).
domain_priors:suppression_score(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.85).
domain_priors:theater_ratio(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_legitimacy_substrate__atenist_monotheistic_reading, snare).
narrative_ontology:human_readable(divine_legitimacy_substrate__atenist_monotheistic_reading, "Atenist Monotheistic Revelation as Exclusive Legitimacy Substrate").
narrative_ontology:topic_domain(divine_legitimacy_substrate__atenist_monotheistic_reading, "ancient_history/religious_studies/political_economy_of_belief_systems").

domain_priors:requires_active_enforcement(divine_legitimacy_substrate__atenist_monotheistic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_legitimacy_substrate__atenist_monotheistic_reading, '8067791f-e94d-4fb2-b206-6a2fae9d17f1').
narrative_ontology:cs_kernel_codification('8067791f-e94d-4fb2-b206-6a2fae9d17f1', fixed_text).
narrative_ontology:cs_authority_grounding('8067791f-e94d-4fb2-b206-6a2fae9d17f1', extraction).
narrative_ontology:cs_reading_relation('8067791f-e94d-4fb2-b206-6a2fae9d17f1', divine_legitimacy_substrate__amun_polytheistic_reading, forecloses).
narrative_ontology:cs_reading_relation('8067791f-e94d-4fb2-b206-6a2fae9d17f1', divine_legitimacy_substrate__folk_syncretistic_reading, influences).
narrative_ontology:cs_axiom('8067791f-e94d-4fb2-b206-6a2fae9d17f1', foundational, aten_exclusive_deity_pharaoh_sole_interpreter).
narrative_ontology:cs_axiom_status(aten_exclusive_deity_pharaoh_sole_interpreter, overridden).
narrative_ontology:cs_axiom_grounding('8067791f-e94d-4fb2-b206-6a2fae9d17f1', aten_exclusive_deity_pharaoh_sole_interpreter, theological).
narrative_ontology:cs_axiom('8067791f-e94d-4fb2-b206-6a2fae9d17f1', secondary, traditional_priesthood_illegitimate_usurpers).
narrative_ontology:cs_axiom_status(traditional_priesthood_illegitimate_usurpers, overridden).
narrative_ontology:cs_axiom_grounding('8067791f-e94d-4fb2-b206-6a2fae9d17f1', traditional_priesthood_illegitimate_usurpers, theological).
narrative_ontology:cs_reference_frame('8067791f-e94d-4fb2-b206-6a2fae9d17f1', pharaonic_sole_revelator_of_aten).
narrative_ontology:cs_drift_state('8067791f-e94d-4fb2-b206-6a2fae9d17f1', post_amarna_restoration, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('8067791f-e94d-4fb2-b206-6a2fae9d17f1', '').
narrative_ontology:cs_kernel_id(divine_legitimacy_substrate__atenist_monotheistic_reading, divine_legitimacy_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__atenist_monotheistic_reading, pharaoh_akhenaten_court).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__atenist_monotheistic_reading, aten_priesthood_new).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__atenist_monotheistic_reading, amun_priesthood).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__atenist_monotheistic_reading, traditional_temple_economies).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__atenist_monotheistic_reading, folk_practitioners).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__atenist_monotheistic_reading, regional_nobility_temple_ties).
narrative_ontology:constraint_vindicates(divine_legitimacy_substrate__atenist_monotheistic_reading, exclusive_divine_revelation_through_pharaoh).
narrative_ontology:constraint_vindicates(divine_legitimacy_substrate__atenist_monotheistic_reading, single_deity_cosmology).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Akhenaten and his immediate court are the sole authors and enforcers of the Atenist revelation. They control the interpretation of divine will, direct the dismantling of Amun temples, and capture the redirected resources (land, labor, tribute, gold). Their exit is arbitrage-grade: they could revert to traditional polytheism at any point (as Tutankhamun did), but doing so would cost them the concentrated power and wealth the monopoly provides.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, pharaoh_akhenaten_court, agenda_setter,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(divine_legitimacy_substrate__atenist_monotheistic_reading, pharaoh_akhenaten_court, beneficiary).

% The newly created Aten priesthood derives status, livelihood, and institutional position entirely from the pharaoh's revelation. They have no independent power base — their authority is delegated and revocable. They benefit from the resource flows but are trapped by their identity-fusion with the new cult; exit means loss of priestly identity and livelihood with no fallback in the dismantled traditional system.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, aten_priesthood_new, beneficiary,
    organized, biographical, constrained, national).

% The Amun priesthood of Thebes was the wealthiest and most powerful institutional actor in Egypt before the Atenist revolution. They controlled vast temple estates, labor forces, and regional tribute networks. The Atenist constraint seized their assets, erased their god's name, closed their temples, and persecuted their personnel. Their exit is trapped: their entire institutional identity, generational knowledge, and resource base are bound to Amun; they cannot 'become' Aten priests without total self-negation.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, amun_priesthood, payer,
    organized, generational, trapped, national).

% The network of temples across Egypt (Ptah at Memphis, Ra at Heliopolis, Osiris at Abydos, etc.) formed a distributed economic and ritual infrastructure. The Atenist constraint redirected their revenues, labor, and land to the new Aten institutions. These temple economies are not agents in the sense of unified actors but are instantiated through their priesthoods and dependents; their exit is trapped because the constraint dismantles the very infrastructure that would enable adaptation.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, traditional_temple_economies, payer,
    organized, generational, trapped, national).

% Village and household practitioners of traditional multi-deity worship (Bes, Taweret, Hathor, ancestor cults, local spirits) experienced the Atenist constraint as disruption of the ritual technologies that structured daily life, health, birth, and death. They were excluded from the official cult (which centered on the pharaoh's unique relationship to Aten) but continued practice covertly. Their exit is identity-locked: their self-concept, family continuity, and cosmological orientation are fused with the traditional deities; open abandonment is unthinkable, but covert persistence is the only available mode.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, folk_practitioners, payer,
    powerless, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(divine_legitimacy_substrate__atenist_monotheistic_reading, folk_practitioners, excluded).

% Provincial governors and local elites derived legitimacy, patronage, and economic standing from their roles in traditional temple administrations. The Atenist centralization stripped these positions, redirecting patronage to the center. They are constrained rather than trapped: they retain landholdings and military/administrative roles, but their ritual legitimacy and local power bases are eroded. Some collaborated with the new order; others waited for restoration.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, regional_nobility_temple_ties, payer,
    moderate, biographical, constrained, regional).

% Vassal rulers in Canaan and Syria (documented in the Amarna letters) observed the Atenist revolution from outside. They had to navigate diplomatic protocols with a pharaoh who claimed exclusive divine authority while they maintained their own traditional pantheons. They neither benefit nor pay directly but their diplomatic positioning is shaped by the constraint's ideological projection.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, foreign_vassals_diplomatic_correspondents, observer,
    organized, biographical, analytical, regional).

% Tutankhamun, Ay, and Horemheb — the post-Amarna pharaohs — inherited the Atenist constraint's ruins. They had the power to dismantle it (arbitrage-grade exit from the Atenist frame) and strong incentive to restore the distributed priestly legitimacy that balanced royal power against the Amun priesthood. Their analytical seat sees the full structural trajectory: the Atenist monopoly was a pharaonic power grab that overreached and collapsed.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, successor_pharaohs_restorationists, observer,
    institutional, generational, arbitrage, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The Atenist constraint claimed to solve the coordination problem of maintaining cosmic order (ma'at) through a single, unambiguous divine channel — eliminating the ambiguity and competition of the multi-deity system where priestly interpretations could diverge.
% TRANSFER_FUNCTION: Moves temple wealth (land, labor, gold, tribute, agricultural surplus) from the distributed Amun-dominated temple economy to the centralized Aten institutions and the pharaoh's treasury; moves interpretive authority from the priesthood to the pharaoh alone; moves ritual legitimacy from local/regional cults to the royal court.
% ABSENT_VOICES: The vast majority of the Egyptian population — peasants, laborers, women, foreign residents — had no voice in the Atenist imposition. The folk_practitioners stakeholder captures their ritual situation but not their political silence. The Amarna letters show vassal rulers' perspectives but not the Egyptian populace's. The constraint's enforcement relied on this exclusion.
% DISAPPEARANCE_RATIONALE: When the Atenist constraint vanished (pharaoh's death, Tutankhamun's restoration), the world rearranged completely: Amun priesthood restored, temples reopened, names re-carved, resources redirected back, traditional priesthood re-empowered. The rearrangement was rapid and near-total, confirming that the constraint's persistence depended entirely on active enforcement by a single pharaoh — it had no self-sustaining coordination foundation.
% FOUNDING_PROBLEM: The founding problem was the perceived fragility of cosmic order under a fragmented priestly system where Amun's priesthood had grown to rival pharaonic authority — Akhenaten claimed Aten's exclusive revelation solved this by restoring the pharaoh as sole guarantor of ma'at.
% FOUNDING_PROBLEM_CORROBORATION: The Amun priesthood's own records (restoration stelae of Tutankhamun) attest that the 'problem' of priestly overreach was real but that the Atenist solution was a pharaonic power grab, not a genuine solution. Modern Egyptology (Kemp, Dodson, Allen) corroborates that the Amun priesthood's power was a genuine structural challenge to royal authority, but the Atenist response concentrated rather than balanced power. The folk_syncretistic reading's persistence demonstrates the founding problem (cosmic order maintenance) was never actually solved by the monopoly — it was displaced.
narrative_ontology:disappearance_verdict(divine_legitimacy_substrate__atenist_monotheistic_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_legitimacy_substrate__atenist_monotheistic_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_legitimacy_substrate__atenist_monotheistic_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(divine_legitimacy_substrate__atenist_monotheistic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_legitimacy_substrate__atenist_monotheistic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_legitimacy_substrate__atenist_monotheistic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(divine_legitimacy_substrate__atenist_monotheistic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.78) reflects the massive resource transfer from traditional temple economies to the pharaoh's new institutions — the Amun priesthood alone controlled estates, labor, and tribute across Egypt. Suppression (0.85) captures the active erasure of alternative worship: name erasures, temple closures, persecution of traditional priesthood. Theater ratio (0.62) is high because the 'revelation' narrative increasingly served to mask the extraction; by year 12, the Aten cult's ritual complexity had become performative maintenance of the pharaoh's monopoly rather than functional coordination. Accessibility_collapse (0.72) is substantial but not total — folk practice persisted covertly. Resistance (0.75) reflects elite non-compliance, regional passive resistance, and the immediate post-Amarna restoration.
 *
 * PERSPECTIVAL GAP:
 *   From the pharaoh's seat, the constraint is a genuine revelation (mountain-like) that he uniquely accesses — the extraction is the cost of maintaining cosmic order. From the Amun priesthood's seat, it is a snare that destroys their institutional existence. From the folk practitioner's seat, it is a disruptive imposition that they navigate through concealment — the constraint's type diverges radically across seats. The engine computes this divergence from the structural data authored here.
 *
 * DIRECTIONALITY LOGIC:
 *   Pharaoh and the new Aten priesthood are structural beneficiaries (d ≈ 0.1–0.2): they collect the redirected temple revenues, control the sole legitimate interpretive channel, and face arbitrage-grade exit (they could revert). Amun priesthood and traditional temple economies are full targets (d ≈ 0.9–1.0): total asset seizure, identity destruction, trapped exit. Folk practitioners are constrained payers (d ≈ 0.6–0.7): they bear the cost of disrupted ritual life but retain identity-locked exit through covert practice. Regional nobility with temple ties are moderate payers (d ≈ 0.5–0.6): they lose patronage networks but retain land bases.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (cosmic order/maintenance of ma'at through correct divine relationship) was live, but the Atenist reading claimed it was SOLVED exclusively through the pharaoh's revelation — a false resolution that concentrated legitimacy in one actor. The mandate did not atrophy; it was weaponized. The constraint's collapse came not from mandatrophy but from the pharaoh's death and the successor's incentive to restore the distributed priestly legitimacy that balanced power. The snare classification captures that the coordination function (cosmic order maintenance) was real but entirely subordinated to extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    atenist_natural_law_vs_constructed_monopoly,
    'Is the claim of Aten''s exclusive divinity a genuine revelation of natural law, or a constructed monopoly legitimating pharaonic resource extraction?',
    'Compare pre-Atenist solar theology trajectories with the abruptness of institutional dismantling; assess whether theological innovation or resource capture better predicts the enforcement pattern.',
    'If constructed monopoly, the constraint is a snare with high extraction masked as cosmology; if natural law, the suppression and extraction metrics would misrepresent a mountain-like revelation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(atenist_natural_law_vs_constructed_monopoly, conceptual, 'Whether Atenist monotheism is a discovered cosmological truth or an engineered legitimacy substrate').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Was suppression of traditional practice primarily structural (state coercion) or internalized (ideological conversion of elites and populace)?',
    'Track post-Amarna restoration: if traditional practice resumes rapidly with minimal resistance, suppression was primarily structural; if folk memory persists in altered forms, internalized components existed.',
    'If internalized suppression was significant, the constraint''s effective suppression exceeds the structural measure — the population carried the suppression after exit became possible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Structural vs. internalized suppression mechanisms in Atenist enforcement').

omega_variable(
    kernel_reading_framing_atenist_vs_polytheistic,
    'Does the divine_legitimacy_substrate kernel admit the atenist_monotheistic_reading as a legitimate reading, or is the kernel itself a retrospective projection onto a contest that had no shared framework?',
    'Analyze whether the three declared readings (atenist, amun_polytheistic, folk_syncretistic) operate within a single commitment framework or represent incommensurable legitimacy orders.',
    'If incommensurable, the kernel is a false unity — each reading instantiates a different constraint with no shared substrate; the CS analysis should treat them as separate kernels.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_framing_atenist_vs_polytheistic, conceptual, 'Whether the kernel framing accurately captures the contest or imposes retrospective coherence').

omega_variable(
    folk_syncretistic_survival_as_resistance,
    'Did folk_syncretistic practice persist through the Atenist period as passive inertia or as active resistance to the monotheistic constraint?',
    'Compare archaeological evidence of household ritual continuity vs. elite compliance; assess whether folk practice shows adaptive concealment patterns.',
    'If active resistance, the resistance metric understates the constraint''s contestedness; if passive inertia, the constraint''s accessibility_collapse was higher than the folk survival suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(folk_syncretistic_survival_as_resistance, empirical, 'Nature of folk practice persistence under Atenist suppression').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_legitimacy_substrate__atenist_monotheistic_reading, 0, 17).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divine_legitimacy_substrate__atenist_monotheistic_reading_tr_t0, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(divine_legitimacy_substrate__atenist_monotheistic_reading_tr_t3, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 3, 0.35).
narrative_ontology:measurement(divine_legitimacy_substrate__atenist_monotheistic_reading_tr_t6, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 6, 0.52).
narrative_ontology:measurement(divine_legitimacy_substrate__atenist_monotheistic_reading_tr_t9, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 9, 0.62).
narrative_ontology:measurement(divine_legitimacy_substrate__atenist_monotheistic_reading_tr_t12, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 12, 0.62).
narrative_ontology:measurement(divine_legitimacy_substrate__atenist_monotheistic_reading_tr_t15, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 15, 0.62).

% Extraction over time
narrative_ontology:measurement(divine_legitimacy_substrate__atenist_monotheistic_reading_be_t0, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(divine_legitimacy_substrate__atenist_monotheistic_reading_be_t3, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 3, 0.55).
narrative_ontology:measurement(divine_legitimacy_substrate__atenist_monotheistic_reading_be_t6, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 6, 0.72).
narrative_ontology:measurement(divine_legitimacy_substrate__atenist_monotheistic_reading_be_t9, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 9, 0.78).
narrative_ontology:measurement(divine_legitimacy_substrate__atenist_monotheistic_reading_be_t12, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 12, 0.78).
narrative_ontology:measurement(divine_legitimacy_substrate__atenist_monotheistic_reading_be_t15, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 15, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(divine_legitimacy_substrate__atenist_monotheistic_reading_su_t0, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(divine_legitimacy_substrate__atenist_monotheistic_reading_su_t3, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 3, 0.6).
narrative_ontology:measurement(divine_legitimacy_substrate__atenist_monotheistic_reading_su_t6, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 6, 0.78).
narrative_ontology:measurement(divine_legitimacy_substrate__atenist_monotheistic_reading_su_t9, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 9, 0.85).
narrative_ontology:measurement(divine_legitimacy_substrate__atenist_monotheistic_reading_su_t12, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 12, 0.85).
narrative_ontology:measurement(divine_legitimacy_substrate__atenist_monotheistic_reading_su_t15, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 15, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_legitimacy_substrate__atenist_monotheistic_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.12).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__atenist_monotheistic_reading, divine_legitimacy_substrate__amun_polytheistic_reading).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__atenist_monotheistic_reading, divine_legitimacy_substrate__folk_syncretistic_reading).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__atenist_monotheistic_reading, temple_economy_resource_allocation).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__atenist_monotheistic_reading, pharaonic_succession_legitimacy).

% DUAL FORMULATION NOTE:
% The divine_legitimacy_substrate kernel decomposes into three constraint stories. This atenist_monotheistic_reading is the most extractive (ε=0.78) because it centralizes all interpretive authority and resource flows in the pharaoh. The amun_polytheistic_reading distributes authority across priesthood and pharaoh (lower ε). The folk_syncretistic_reading has minimal extractiveness but no institutional coordination function. They are linked as a family through shared referent (divine legitimacy in New Kingdom Egypt) and mutual structural influence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(divine_legitimacy_substrate__atenist_monotheistic_reading, institutional, 0.15).
constraint_indexing:directionality_override(divine_legitimacy_substrate__atenist_monotheistic_reading, organized, 0.92).
constraint_indexing:directionality_override(divine_legitimacy_substrate__atenist_monotheistic_reading, moderate, 0.65).
constraint_indexing:directionality_override(divine_legitimacy_substrate__atenist_monotheistic_reading, powerless, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
