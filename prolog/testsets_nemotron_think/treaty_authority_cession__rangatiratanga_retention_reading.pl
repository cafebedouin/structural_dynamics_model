% ============================================================================
% CONSTRAINT STORY: treaty_authority_cession__rangatiratanga_retention_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_treaty_authority_cession__rangatiratanga_retention_reading, []).

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
 *   constraint_id: treaty_authority_cession__rangatiratanga_retention_reading
 *   human_readable: Te Tiriti Partnership: Māori Text Control, Kāwanatanga Limited to Governance, Tino Rangatiratanga Retained
 *   domain: constitutional/indigenous_rights/colonial_history
 *
 * SUMMARY:
 *   This constraint story instantiates the rangatiratanga_retention_reading
 *   of the treaty_authority_cession kernel. Under this reading, the Māori
 *   text of Te Tiriti o Waitangi controls via contra proferentem; kāwanatanga
 *   (governorship) is limited to governance functions over settlers and does
 *   not extend to authority over hapū/iwi; tino rangatiratanga (chiefly
 *   authority/full self-determination) is retained in full; and the Treaty
 *   establishes a partnership requiring ongoing hapū consent for Crown
 *   actions affecting Māori authority. The constraint is a Rope: a
 *   coordination mechanism solving the problem of how British governance and
 *   Māori authority coexist. Its operation is low-extraction when honored —
 *   both parties benefit from legitimate, stable governance. The historical
 *   trajectory shows severe degradation (1860-1900) when the Crown abandoned
 *   the partnership frame, then partial recovery (1975-present) through
 *   Tribunal process and settlements. This reading does not deny the
 *   historical extraction; it locates that extraction in the Crown's breach
 *   of the partnership constraint, not in the constraint itself. The
 *   retrospective_snare_exposure reading is a separate constraint story
 *   assessing the historical land alienation sequence.
 *
 * KEY AGENTS:
 *   - hapu_iwi: Primary beneficiaries and partners — retain tino rangatiratanga, exercise consent authority (organized/biographical/identity_locked)
 *   - crown_governance: Agenda setter and beneficiary — exercises kāwanatanga legitimately only with hapū consent (institutional/generational/arbitrage)
 *   - pakeha_settlers: Beneficiaries — gain stable governance framework through legitimate Crown authority (organized/biographical/mobile)
 *   - waitangi_tribunal_courts: Observers and agenda setters — adjudicate partnership disputes, interpret Treaty principles (institutional/generational/analytical)
 *   - anti_parties: Excluded — reject partnership framework entirely, assert Crown sovereignty as absolute (powerful/biographical/trapped)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(treaty_authority_cession__rangatiratanga_retention_reading, 0.15).
domain_priors:suppression_score(treaty_authority_cession__rangatiratanga_retention_reading, 0.1).
domain_priors:theater_ratio(treaty_authority_cession__rangatiratanga_retention_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(treaty_authority_cession__rangatiratanga_retention_reading, rope).
narrative_ontology:human_readable(treaty_authority_cession__rangatiratanga_retention_reading, "Te Tiriti Partnership: Māori Text Control, Kāwanatanga Limited to Governance, Tino Rangatiratanga Retained").
narrative_ontology:topic_domain(treaty_authority_cession__rangatiratanga_retention_reading, "constitutional/indigenous_rights/colonial_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(treaty_authority_cession__rangatiratanga_retention_reading, 'e8dae64e-851c-4874-97fc-79a14594e238').
narrative_ontology:cs_kernel_codification('e8dae64e-851c-4874-97fc-79a14594e238', fixed_text).
narrative_ontology:cs_authority_grounding('e8dae64e-851c-4874-97fc-79a14594e238', lineage).
narrative_ontology:cs_interpretation_layer_present('e8dae64e-851c-4874-97fc-79a14594e238').
narrative_ontology:cs_reading_relation('e8dae64e-851c-4874-97fc-79a14594e238', treaty_authority_cession__crown_cession_reading, forecloses).
narrative_ontology:cs_reading_relation('e8dae64e-851c-4874-97fc-79a14594e238', treaty_authority_cession__biculturalism_reading, influences).
narrative_ontology:cs_reading_relation('e8dae64e-851c-4874-97fc-79a14594e238', treaty_authority_cession__retrospective_snare_exposure, coexists_with).
narrative_ontology:cs_axiom('e8dae64e-851c-4874-97fc-79a14594e238', foundational, tino_rangatiratanga_retained).
narrative_ontology:cs_axiom_status(tino_rangatiratanga_retained, holdable).
narrative_ontology:cs_axiom_grounding('e8dae64e-851c-4874-97fc-79a14594e238', tino_rangatiratanga_retained, deontological).
narrative_ontology:cs_axiom('e8dae64e-851c-4874-97fc-79a14594e238', foundational, partnership_requires_ongoing_consent).
narrative_ontology:cs_axiom_status(partnership_requires_ongoing_consent, holdable).
narrative_ontology:cs_axiom_grounding('e8dae64e-851c-4874-97fc-79a14594e238', partnership_requires_ongoing_consent, deontological).
narrative_ontology:cs_axiom('e8dae64e-851c-4874-97fc-79a14594e238', foundational, kawanatanga_limited_to_governance).
narrative_ontology:cs_axiom_status(kawanatanga_limited_to_governance, holdable).
narrative_ontology:cs_axiom_grounding('e8dae64e-851c-4874-97fc-79a14594e238', kawanatanga_limited_to_governance, empirically_contingent).
narrative_ontology:cs_axiom('e8dae64e-851c-4874-97fc-79a14594e238', foundational, maori_text_controls_via_contra_proferentem).
narrative_ontology:cs_axiom_status(maori_text_controls_via_contra_proferentem, holdable).
narrative_ontology:cs_axiom_grounding('e8dae64e-851c-4874-97fc-79a14594e238', maori_text_controls_via_contra_proferentem, conventional).
narrative_ontology:cs_reference_frame('e8dae64e-851c-4874-97fc-79a14594e238', id_1840_partnership_agreement).
narrative_ontology:cs_drift_state('e8dae64e-851c-4874-97fc-79a14594e238', contemporary_post_settlement_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e8dae64e-851c-4874-97fc-79a14594e238', '').
narrative_ontology:cs_kernel_id(treaty_authority_cession__rangatiratanga_retention_reading, treaty_authority_cession).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(treaty_authority_cession__rangatiratanga_retention_reading, hapu_iwi).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__rangatiratanga_retention_reading, crown_governance).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__rangatiratanga_retention_reading, pakeha_settlers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hapū and iwi (Māori tribes) retain tino rangatiratanga — full chiefly authority over their territories, resources, and people. They are partners to the Treaty, not subjects. Their consent is required for Crown actions affecting their authority. Exit from the partnership would mean surrendering the constitutional recognition of their authority, which is constitutive of their political identity. They participate in co-governance arrangements, Waitangi Tribunal claims, and Treaty settlements as the partnership's Māori parties.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, hapu_iwi, beneficiary,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(treaty_authority_cession__rangatiratanga_retention_reading, hapu_iwi, agenda_setter).

% The New Zealand Crown (executive, legislature, judiciary) exercises kāwanatanga — governance authority over settlers, immigration, foreign affairs, and general law — but only legitimately with hapū consent where Māori interests are affected. The Crown benefits from the partnership: it gains legitimate authority to govern rather than relying on force or unilateral assertion. Its exit option is arbitrage: it could abandon the partnership frame (as it did historically) but loses constitutional legitimacy and faces domestic/international pressure. In practice, the Crown has often acted unilaterally, treating consultation as sufficient rather than requiring consent.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, crown_governance, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(treaty_authority_cession__rangatiratanga_retention_reading, crown_governance, beneficiary).

% Non-Māori New Zealanders benefit from the stable, legitimate governance framework the partnership provides. They are not parties to the Treaty but are the primary subjects of Crown kāwanatanga. Their exit is mobile: they could emigrate, but the partnership framework makes their presence legitimate rather than colonial occupation. They have no direct role in consent decisions but are affected by partnership outcomes (co-governance, resource management, Treaty settlements).
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, pakeha_settlers, beneficiary,
    organized, biographical, mobile, national).

% The Waitangi Tribunal (established 1975) and courts interpret Treaty principles, hear claims, and recommend remedies. They are the partnership's interpretive layer — absorbing drift between Crown practice and Treaty obligations without requiring constitutional amendment. They do not collect extraction or pay costs; they adjudicate. Their analytical exit means they assess the constraint from outside its operational pressures. Their findings have shifted Crown practice toward partnership but lack binding enforcement power.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, waitangi_tribunal_courts, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(treaty_authority_cession__rangatiratanga_retention_reading, waitangi_tribunal_courts, agenda_setter).

% Political parties, lobby groups, and commentators who reject the partnership framework entirely — asserting Crown sovereignty as absolute, Treaty as historical document only, and Māori authority as extinguished or subordinate. They are excluded from the partnership conversation because their position denies its premise. They are trapped: they cannot exit the constraint's effects (Treaty settlements, co-governance, Tribunal jurisprudence) but refuse to engage its legitimacy. Their opposition fuels political resistance to partnership implementation.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, anti_partnership_actors, excluded,
    powerful, biographical, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes legitimate dual governance: Crown exercises kāwanatanga (governance) over settlers and general affairs; hapū/iwi retain tino rangatiratanga (full authority) over their territories and people. The partnership solves the coordination problem of two peoples sharing one state without either subordinating the other — authority is negotiated, not imposed.
% TRANSFER_FUNCTION: Transfers governance authority (kāwanatanga) from hapū to Crown for specified purposes (settler governance, external relations) while retaining all other authority (tino rangatiratanga) with hapū. The transfer is conditional: Crown authority is legitimate only when exercised consistently with the partnership and with ongoing hapū consent. No transfer of underlying authority or sovereignty occurs.
% ABSENT_VOICES: Māori who reject the Treaty entirely (e.g., some independence movements) — they would argue the partnership framework legitimizes Crown occupation. Also absent: future generations of both parties whose consent cannot be given retrospectively. Anti-partnership actors are structurally excluded (they deny the framework's premise) but politically powerful.
% DISAPPEARANCE_RATIONALE: If the partnership constraint vanished overnight, New Zealand's constitutional foundation would collapse. Crown legitimacy would rest solely on conquest/unilateral assertion. Māori would lose the legal basis for Treaty claims, co-governance, and resource rights. The state would face immediate legitimacy crisis domestically and internationally. A new constitutional arrangement would be required — likely either Crown absolute sovereignty (recolonization) or Māori independence movements gaining traction.
% FOUNDING_PROBLEM: How to establish British governance over British subjects in New Zealand while protecting Māori authority (tino rangatiratanga) over their lands, villages, and treasures — given that Māori chiefs would not sign a document ceding sovereignty.
% FOUNDING_PROBLEM_CORROBORATION: Waitangi Tribunal (Te Paparahi o Te Raki inquiry, 2014) found chiefs did not cede sovereignty; UN Declaration on the Rights of Indigenous Peoples (2007, endorsed by NZ 2010) affirms indigenous self-determination; constitutional law scholars (e.g., Matthew Palmer, Carwyn Jones) attest the partnership problem remains live. Crown ministers have occasionally acknowledged the problem is live (e.g., 2019 'Māori-Crown relations' portfolio creation) but legislative practice often treats it as settled.
narrative_ontology:disappearance_verdict(treaty_authority_cession__rangatiratanga_retention_reading, world_rearranges).
narrative_ontology:founding_problem_status(treaty_authority_cession__rangatiratanga_retention_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(treaty_authority_cession__rangatiratanga_retention_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-25',
    'no_scope_rebuild_nemotron_think+seed_rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(treaty_authority_cession__rangatiratanga_retention_reading, 'none', 1).
narrative_ontology:epsilon_provenance(treaty_authority_cession__rangatiratanga_retention_reading, 0.15, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(treaty_authority_cession__rangatiratanga_retention_reading_tests).
:- end_tests(treaty_authority_cession__rangatiratanga_retention_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) for the partnership constraint itself: when operating as intended, it coordinates governance authority between two sovereign parties with mutual benefit. The historical peak (0.65 in 1900) reflects Crown breach, not the constraint's design. Suppression is low (0.10) because the partnership depends on consent, not coercion — historical suppression peaks reflect Crown's departure from the constraint. Theater ratio is low (0.08) because the coordination function (mutual governance legitimacy) is genuine, though historical theater was high during breach periods. Accessibility collapse is moderate (0.35): alternatives exist (Crown unilateralism, Māori independence movements) but the partnership framework has deep constitutional embedding. Resistance is moderate (0.45): Crown has resisted full implementation, but the constraint itself generates its own compliance pressure through legitimacy claims.
 *
 * PERSPECTIVAL GAP:
 *   From the hapū/iwi seat, the constraint is a Mountain of retained authority — tino rangatiratanga is non-negotiable. From the Crown seat, it is a Rope requiring negotiated exercise of kāwanatanga. From the anti-partnership seat, it is a Snare (imposing obligations on Crown) or a Piton (vestigial). The engine computes these per-seat types from the structural data: hapū are identity_locked beneficiaries (exit means losing constitutive authority); Crown is institutional agenda_setter with arbitrage exit (could abandon partnership but loses legitimacy); excluded parties are trapped by the framework's constitutional entrenchment.
 *
 * DIRECTIONALITY LOGIC:
 *   hapu_iwi are structural beneficiaries (d ≈ 0.15): they retain tino rangatiratanga and hold consent authority. Crown is symmetric-to-beneficiary (d ≈ 0.35): gains legitimate governance authority but bears obligation to seek consent. Pākehā settlers are beneficiaries (d ≈ 0.20): gain stable governance. Waitangi Tribunal is analytical (d = 0.5). Anti-partnership actors are excluded — their directionality is undefined within the constraint because they reject its legitimacy. The derivation follows from beneficiary declarations: hapū and Crown both declared as beneficiaries because both gain from the partnership when honored. No victims declared under this reading — extraction appears in the retrospective_snare_exposure reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (establishing British governance while protecting Māori authority) remains contested. Crown often acts as if the problem is solved (cession complete), while hapū assert it is live (partnership ongoing). The constraint has not suffered mandatrophy — its coordination function (legitimate dual governance) is still needed. What appears as mandatrophy from Crown perspective (treating Treaty as historical document only) is actually mandate denial. The partnership constraint persists because the structural problem it solves persists: two peoples, one state, requiring ongoing negotiation of authority.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint one reading of the contested treaty_authority_cession kernel, and what structural elements distinguish it from sibling readings?',
    'Committee frame analysis: this reading instantiates a Rope (partnership requiring negotiated authority); crown_cession_reading instantiates a Mountain (completed legal cession); retrospective_snare_exposure instantiates a Snare (extraction via mistranslation). The disagreement is located in the referent of kāwanatanga and the status of tino rangatiratanga.',
    'If this reading is structurally correct, the constraint is a coordination mechanism with low ε; if crown_cession_reading is correct, the constraint is a fixed transfer of sovereignty (Mountain); if retrospective_snare_exposure is correct, the historical operation was extractive (Snare). Classification diverges across readings because ε''s referent differs: this reading assesses the partnership arrangement; retrospective_snare assesses the historical land alienation sequence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Kernel identity and reading-level ε-invariance: this reading''s ε refers to the partnership arrangement, not the historical extraction sequence.').

omega_variable(
    translation_asymmetry_mechanism,
    'Does the textual divergence between Māori and English texts constitute an extraction mechanism (retrospective_snare_exposure reading) or a genuine interpretive ambiguity resolvable within the partnership (this reading)?',
    'Historical linguistics of 1840 Māori usage of kāwanatanga vs. English sovereignty; missionary translation records; chiefs'' recorded understandings at signing. If chiefs could not have assented to English sovereignty claim given Māori text, the divergence operated as extraction.',
    'If divergence is extraction mechanism, historical Crown actions (land alienation, legislative override) are properly classified as Snare operating under mistranslation. If resolvable ambiguity, they are partnership breaches within a Rope framework. This reading treats it as resolvable within partnership; retrospective_snare_exposure treats it as the extraction mechanism itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(translation_asymmetry_mechanism, empirical, 'Whether textual divergence is the extraction mechanism or an ambiguity within the coordination function.').

omega_variable(
    partnership_consent_threshold,
    'Does ''ongoing consent'' require hapū veto over Crown actions affecting tino rangatiratanga, or a duty to consult and accommodate short of veto?',
    'Waitangi Tribunal jurisprudence evolution; UN Declaration on the Rights of Indigenous Peoples (FPIC standard); constitutional practice of co-governance arrangements (e.g., Waikato River, Te Urewera).',
    'Veto threshold makes partnership a stronger Rope (higher coordination fidelity, lower effective extraction for hapū). Consultation-only threshold admits more Crown unilateral action, raising effective extraction toward Tangled Rope. This reading authors veto as the structural requirement; Crown practice has operated at consultation level.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partnership_consent_threshold, conceptual, 'Consent threshold within the partnership coordination function.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(treaty_authority_cession__rangatiratanga_retention_reading, 1840, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(treaty_auth_rangatiratanga_tr_t1840, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 1840, 0.02).
narrative_ontology:measurement(treaty_auth_rangatiratanga_tr_t1860, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 1860, 0.4).
narrative_ontology:measurement(treaty_auth_rangatiratanga_tr_t1900, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 1900, 0.7).
narrative_ontology:measurement(treaty_auth_rangatiratanga_tr_t1975, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 1975, 0.3).
narrative_ontology:measurement(treaty_auth_rangatiratanga_tr_t2000, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(treaty_auth_rangatiratanga_tr_t2024, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 2024, 0.08).

% Extraction over time
narrative_ontology:measurement(treaty_auth_rangatiratanga_be_t1840, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 1840, 0.05).
narrative_ontology:measurement(treaty_auth_rangatiratanga_be_t1860, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 1860, 0.35).
narrative_ontology:measurement(treaty_auth_rangatiratanga_be_t1900, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 1900, 0.65).
narrative_ontology:measurement(treaty_auth_rangatiratanga_be_t1975, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 1975, 0.45).
narrative_ontology:measurement(treaty_auth_rangatiratanga_be_t2000, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 2000, 0.25).
narrative_ontology:measurement(treaty_auth_rangatiratanga_be_t2024, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(treaty_auth_rangatiratanga_su_t1840, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 1840, 0.05).
narrative_ontology:measurement(treaty_auth_rangatiratanga_su_t1860, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 1860, 0.55).
narrative_ontology:measurement(treaty_auth_rangatiratanga_su_t1900, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 1900, 0.85).
narrative_ontology:measurement(treaty_auth_rangatiratanga_su_t1975, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 1975, 0.4).
narrative_ontology:measurement(treaty_auth_rangatiratanga_su_t2000, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 2000, 0.2).
narrative_ontology:measurement(treaty_auth_rangatiratanga_su_t2024, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 2024, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(treaty_authority_cession__rangatiratanga_retention_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(treaty_authority_cession__rangatiratanga_retention_reading, 0.08).
narrative_ontology:affects_constraint(treaty_authority_cession__rangatiratanga_retention_reading, treaty_authority_cession__crown_cession_reading).
narrative_ontology:affects_constraint(treaty_authority_cession__rangatiratanga_retention_reading, treaty_authority_cession__biculturalism_reading).
narrative_ontology:affects_constraint(treaty_authority_cession__rangatiratanga_retention_reading, treaty_authority_cession__retrospective_snare_exposure).

% DUAL FORMULATION NOTE:
% This reading and crown_cession_reading are mutually exclusive framings of the same 1840 agreement (forecloses relation). This reading and retrospective_snare_exposure address different temporal referents: this reading assesses the partnership constraint; retrospective_snare assesses the historical extraction sequence operating under mistranslation. Both can be structurally true of their respective referents. Biculturalism_reading is a downstream institutionalization of this reading's partnership principle within Crown sovereignty framework (influences relation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(treaty_authority_cession__rangatiratanga_retention_reading, institutional, 0.35).
constraint_indexing:directionality_override(treaty_authority_cession__rangatiratanga_retention_reading, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
