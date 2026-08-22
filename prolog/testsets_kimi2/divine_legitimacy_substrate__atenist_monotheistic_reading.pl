% ============================================================================
% CONSTRAINT STORY: divine_legitimacy_substrate__atenist_monotheistic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: divine_legitimacy_substrate__atenist_monotheistic_reading
 *   human_readable: Atenist Monotheistic Divine Legitimacy
 *   domain: ancient_history/religious_studies/political_economy
 *
 * SUMMARY:
 *   During the Amarna period (circa 1353â1336 BCE), Pharaoh Akhenaten
 *   imposed a radical theological constraint: the solar disk Aten was the
 *   sole deity, and Akhenaten himself was the exclusive revelatory
 *   intermediary. This reading of the divine legitimacy substrate dismantled
 *   the polycentric cult economy of New Kingdom Egypt, confiscating Amun
 *   temple estates, suppressing household ritual, and redirecting all
 *   devotional performance through the pharaoh. The constraint is authored as
 *   one reading of a contested kernel; its siblingsâAmun-priestly
 *   polytheism and folk syncretismâare structurally foreclosed by its core
 *   axioms.
 *
 * KEY AGENTS:
 *   - pharaoh_akhenaten: Agenda-setter (institutional/national, mobile exit) â claims sole revelatory monopoly and enforces cultic exclusivity via decrees and temple closures
 *   - atenist_court_elite: Primary beneficiary (organized/national, constrained exit) â accrues redistributed temple wealth and new ritual-administrative roles
 *   - amun_priesthood: Primary payer (institutional/national, identity-locked exit) â bears expropriation of estates and erasure of cosmological function
 *   - folk_religious_practitioners: Secondary payer (moderate/local, constrained exit) â household and village shrine-keepers forced into public Atenist performance
 *   - egyptian_subjects: Diffuse payer (powerless/national, constrained exit) â populace redirected from traditional festivals to pharaonic monotheism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.82).
domain_priors:suppression_score(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.91).
domain_priors:theater_ratio(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__atenist_monotheistic_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_legitimacy_substrate__atenist_monotheistic_reading, tangled_rope).
narrative_ontology:human_readable(divine_legitimacy_substrate__atenist_monotheistic_reading, "Atenist Monotheistic Divine Legitimacy").
narrative_ontology:topic_domain(divine_legitimacy_substrate__atenist_monotheistic_reading, "ancient_history/religious_studies/political_economy").

domain_priors:requires_active_enforcement(divine_legitimacy_substrate__atenist_monotheistic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_legitimacy_substrate__atenist_monotheistic_reading, '585af3e0-3ae3-46f5-b4f7-b8b6decd83db').
narrative_ontology:cs_kernel_codification('585af3e0-3ae3-46f5-b4f7-b8b6decd83db', fixed_text).
narrative_ontology:cs_authority_grounding('585af3e0-3ae3-46f5-b4f7-b8b6decd83db', lineage).
narrative_ontology:cs_interpretation_layer_present('585af3e0-3ae3-46f5-b4f7-b8b6decd83db').
narrative_ontology:cs_reading_relation('585af3e0-3ae3-46f5-b4f7-b8b6decd83db', divine_legitimacy_substrate__amun_polytheistic_reading, forecloses).
narrative_ontology:cs_reading_relation('585af3e0-3ae3-46f5-b4f7-b8b6decd83db', divine_legitimacy_substrate__folk_syncretistic_reading, forecloses).
narrative_ontology:cs_axiom('585af3e0-3ae3-46f5-b4f7-b8b6decd83db', foundational, aten_exclusive_sole_deity).
narrative_ontology:cs_axiom_status(aten_exclusive_sole_deity, holdable).
narrative_ontology:cs_axiom_grounding('585af3e0-3ae3-46f5-b4f7-b8b6decd83db', aten_exclusive_sole_deity, theological).
narrative_ontology:cs_axiom('585af3e0-3ae3-46f5-b4f7-b8b6decd83db', foundational, pharaoh_sole_intermediary).
narrative_ontology:cs_axiom_status(pharaoh_sole_intermediary, holdable).
narrative_ontology:cs_axiom_grounding('585af3e0-3ae3-46f5-b4f7-b8b6decd83db', pharaoh_sole_intermediary, theological).
narrative_ontology:cs_reference_frame('585af3e0-3ae3-46f5-b4f7-b8b6decd83db', atenist_revelatory_monopoly).
narrative_ontology:cs_drift_state('585af3e0-3ae3-46f5-b4f7-b8b6decd83db', post_amarna_restoration, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('585af3e0-3ae3-46f5-b4f7-b8b6decd83db', '2026-06-19T12:00:00Z').
narrative_ontology:cs_kernel_id(divine_legitimacy_substrate__atenist_monotheistic_reading, divine_legitimacy_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__atenist_monotheistic_reading, atenist_court_elite).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__atenist_monotheistic_reading, pharaoh_akhenaten).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__atenist_monotheistic_reading, amun_priesthood).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__atenist_monotheistic_reading, folk_religious_practitioners).
narrative_ontology:constraint_victim(divine_legitimacy_substrate__atenist_monotheistic_reading, egyptian_subjects).
narrative_ontology:constraint_vindicates(divine_legitimacy_substrate__atenist_monotheistic_reading, divine_kingship_doctrine).
narrative_ontology:constraint_vindicates(divine_legitimacy_substrate__atenist_monotheistic_reading, aten_theism_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claims unique and exclusive revelatory access to the solar disk Aten, issues decrees closing rival temples, and redirects all cultic revenue and labor obligations to the crown. His political legitimacy is fused with this theological monopoly.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, pharaoh_akhenaten, agenda_setter,
    institutional, generational, mobile, national).

% Administrators and favored officials of the new Aten cult who receive redistributed temple estates, titles, and ritual roles that were previously held by the Amun priesthood. Their status is entirely contingent on the pharaoh's revelatory monopoly.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, atenist_court_elite, beneficiary,
    organized, biographical, constrained, national).

% Former holders of vast temple lands and interpretive authority over Amun-Ra. Their shrines are closed, estates confiscated, and ritual performance criminalized. Their professional and cosmological identity is bound to the suppressed cult, making exit existentially costly.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, amun_priesthood, payer,
    institutional, generational, identity_locked, national).

% Household and village shrine-keepers who maintained local deities and ancestor cults. They are compelled to adopt Atenist public formulae while hiding traditional icons, facing surveillance and punishment for noncompliance.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, folk_religious_practitioners, payer,
    moderate, biographical, constrained, local).

% The general populace required to redirect all devotional activity through the pharaoh and the Aten. Traditional festivals are abolished, local cults erased from official life, and the psychological and social costs of forced religious reorientation are borne diffusely.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__atenist_monotheistic_reading, egyptian_subjects, payer,
    powerless, immediate, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(divine_legitimacy_substrate__atenist_monotheistic_reading, pharaoh_akhenaten).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unifies political and cosmological authority under a single interpretive center, eliminating rival priestly networks and consolidating ritual practice under direct pharaonic oversight.
% TRANSFER_FUNCTION: Moves temple lands, offerings, labor obligations, and symbolic legitimacy from the plural cult economy (Amun priesthood, local shrines) to the pharaoh and the centralized Aten cult; moves compliance and public devotional performance from subjects to the pharaoh as sole intermediary.
% ABSENT_VOICES: The suppressed priesthoods of Amun, Ptah, Mut, and other gods; household shrine-keepers in Upper and Lower Egypt; and regional governors with syncretist leanings are structurally excluded. They would argue for the continuity of ancestral practice and the distributed legitimacy of multiple cults, but are silenced by temple closure and erasure of theological texts.
% DISAPPEARANCE_RATIONALE: If the Atenist exclusivity vanished overnight, temple lands would revert to their former holders, priesthoods would reconstitute, the pharaoh's unique claim to revelation would collapse, and the political economy of legitimacy would fragment back into polycentric cult networks.
% FOUNDING_PROBLEM: Theocratic pluralism had produced powerful, rival priesthoodsâespecially the Amun cult at Thebesâwhose wealth, tax-exempt estates, and independent interpretive authority challenged pharaonic supremacy and diverted state revenue.
% FOUNDING_PROBLEM_CORROBORATION: The pharaoh and atenist court attest the problem as ongoing priestly overreach threatening cosmic and political order. The Amun priesthood and subsequent restoration narratives (e.g., Tutankhamun's Restoration Stela) attest the problem was manufactured to justify confiscation; no independent corroborating party exists outside the beneficiary-victim binary.
narrative_ontology:disappearance_verdict(divine_legitimacy_substrate__atenist_monotheistic_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_legitimacy_substrate__atenist_monotheistic_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_legitimacy_substrate__atenist_monotheistic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(divine_legitimacy_substrate__atenist_monotheistic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_legitimacy_substrate__atenist_monotheistic_reading, 0.82, 'kimi-k2.6', 'none', direct).

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
 *   Extraction (0.82) is high because the constraint transferred massive temple estates and labor obligations to the crown while eliminating competing cultic tax shelters. Suppression (0.91) is near-maximum because the constraint required active demolition of alternative temples, erasure of deity names, and policing of private practice. Theater ratio (0.45) reflects that while the theological innovation may have been sincere, a substantial share of enforcement activity was performative maintenance of an exclusivity that lacked deep popular uptake. Accessibility collapse (0.88) is high because once the exclusivity was instituted, alternative cosmologies became literally unspeakable in official contexts. Resistance (0.72) reflects covert persistence of traditional practice and the rapid restoration of polytheism after Akhenaten's death.
 *
 * PERSPECTIVAL GAP:
 *   From the pharaoh's seat the constraint is genuine cosmic re-coordination that restores proper order; from the Amun priesthood's seat it is expropriation dressed as theology; from the folk practitioner's seat it is the criminalization of ancestral household ritual. The engine computes these divergences from the structural dataâpower, exit, and role declarationsârather than from the claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Pharaoh and court elite sit near the beneficiary pole (low d): they collect wealth and legitimacy from the exclusivity. Amun priesthood sits near the full-target pole (high d): they are structurally expropriated and their identity is fused with the suppressed cult, leaving no exit. Folk practitioners and subjects also sit near the target end but with less identity-lock and more constrained/concealed-practice exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling the Atenist reform as pure coordination (Rope) by requiring victim declarations and active enforcement for Tangled Rope certification. The presence of declared payers (amun_priesthood, folk_religious_practitioners, egyptian_subjects) alongside beneficiaries (atenist_court_elite) forces the hybrid classification. If the reform were merely a new theological consensus without expropriation, the victims array would be empty and the type would compute toward Rope; if it were pure extraction without any coordinative unification of the state cult, the metrics would trend toward Snare. The authored claim of Tangled Rope reflects the dual-use structure: genuine state-cult coordination layered with massive asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    atenist_sincerity_vs_instrumentality,
    'Was the Atenist monotheistic constraint driven by sincere theological conviction or by instrumental political-economic centralization?',
    'Archaeological correlation of temple closure decrees with land seizure records; textual analysis of the Boundary Stelae for internal consistency versus ad-hoc justification.',
    'If instrumental, the coordination story is cover and the engine''s Boltzmann excess-extraction flag fires more strongly; if sincere, the constraint retains a stronger coordination classification despite its extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(atenist_sincerity_vs_instrumentality, conceptual, 'Theological sincerity versus political instrumentality of Atenism').

omega_variable(
    suppression_scope_local_vs_national,
    'Did the suppression of alternative cults achieve national penetration or remain concentrated around Akhetaten and major cities?',
    'Survey of provincial site destruction layers and Theban ostraca referencing continued private practice.',
    'If suppression was shallow, effective extraction was lower outside the capital and the constraint''s scope was more regional than national.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_scope_local_vs_national, empirical, 'Geographic depth of cultic suppression under Atenism').

omega_variable(
    kernel_reading_contest_resolution,
    'Does the Atenist reading''s foreclosure of its siblings represent a logical contradiction within Egyptian religious thought or merely a political suppression of mutually tolerable practices?',
    'Comparative analysis of whether Akhenaten''s agents conceptually integrated or simply silenced Amun theology; detection of syncretic Aten-Amun iconography would suggest co-existence was thinkable.',
    'If the practices were logically co-tenable, the forecloses relation should be downgraded to influences or coexists_with, altering the kernel family topology.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest_resolution, conceptual, 'Whether Atenist foreclosure of sibling readings is logical or political').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_legitimacy_substrate__atenist_monotheistic_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t0, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(divi_tr_t3, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 3, 0.25).
narrative_ontology:measurement(divi_tr_t6, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 6, 0.32).
narrative_ontology:measurement(divi_tr_t9, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 9, 0.38).
narrative_ontology:measurement(divi_tr_t12, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 12, 0.42).
narrative_ontology:measurement(divi_tr_t15, divine_legitimacy_substrate__atenist_monotheistic_reading, theater_ratio, 15, 0.45).

% Extraction over time
narrative_ontology:measurement(divi_be_t0, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(divi_be_t3, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 3, 0.58).
narrative_ontology:measurement(divi_be_t6, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 6, 0.68).
narrative_ontology:measurement(divi_be_t9, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 9, 0.75).
narrative_ontology:measurement(divi_be_t12, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 12, 0.8).
narrative_ontology:measurement(divi_be_t15, divine_legitimacy_substrate__atenist_monotheistic_reading, base_extractiveness, 15, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t0, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(divi_su_t3, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 3, 0.78).
narrative_ontology:measurement(divi_su_t6, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 6, 0.85).
narrative_ontology:measurement(divi_su_t9, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 9, 0.88).
narrative_ontology:measurement(divi_su_t12, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 12, 0.9).
narrative_ontology:measurement(divi_su_t15, divine_legitimacy_substrate__atenist_monotheistic_reading, suppression_requirement, 15, 0.91).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_legitimacy_substrate__atenist_monotheistic_reading, identity_coordination).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__atenist_monotheistic_reading, amun_polytheistic_reading).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__atenist_monotheistic_reading, folk_syncretistic_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the divine_legitimacy_substrate kernel, decomposed per the Îµ-invariance principle because the natural-language label 'divine legitimacy in New Kingdom Egypt' conflates structurally distinct claims: Amun-priestly polytheistic legitimacy, Atenist monotheistic revelatory legitimacy, and folk syncretistic practice-based legitimacy. Each reading carries a different Îµ, beneficiary structure, and enforcement mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
