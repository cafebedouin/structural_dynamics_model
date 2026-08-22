% ============================================================================
% CONSTRAINT STORY: treaty_authority_cession__rangatiratanga_retention_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-14
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
 *   constraint_id: treaty_authority_cession__rangatiratanga_retention_reading
 *   human_readable: Te Tiriti o Waitangi — Rangatiratanga Retention Reading (Partnership Governance)
 *   domain: constitutional/indigenous/colonial
 *
 * SUMMARY:
 *   This constraint story instantiates the rangatiratanga_retention_reading
 *   of the treaty_authority_cession kernel. It reads te Tiriti o Waitangi
 *   through the Māori text (contra proferentem): kāwanatanga = limited
 *   governance authority delegated to the Crown; tino rangatiratanga = full
 *   hapū authority retained; the treaty establishes an ongoing partnership
 *   requiring Crown to seek hapū consent. The constraint is the partnership
 *   framework itself — the structural requirement that Crown governance
 *   legitimacy depends on negotiated authority exercise with hapū. This
 *   reading claims the constraint is a Rope: genuine coordination between two
 *   peoples, mutual benefit, minimal coercion. The metrics reflect the
 *   partnership's operation WHEN HONORED. The retrospective extraction
 *   visible in land alienation and legislative override (the snare) is a
 *   DIFFERENT constraint — the retrospective_snare_exposure reading — linked
 *   via network.affects_constraints. This story's ε (0.28) measures the
 *   partnership framework's current operation, not the historical extraction
 *   that occurred when the Crown breached it.
 *
 * KEY AGENTS:
 *   - hapu_iwi: Primary beneficiary (organized/identity_locked) — retains tino rangatiratanga, partnership consent gate
 *   - crown_as_partner: Beneficiary + agenda_setter (institutional/constrained) — gains legitimate kāwanatanga conditional on consent
 *   - settler_population: Beneficiary (organized/mobile) — lawful presence via Crown's partnered governance
 *   - waitangi_tribunal: Observer (institutional/analytical) — interprets partnership, no enforcement
 *   - courts: Agenda_setter (institutional/constrained) — adjudicates partnership principles within Crown sovereignty
 *   - parliament: Agenda_setter (institutional/constrained) — exercises kāwanatanga, politically constrained by partnership
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(treaty_authority_cession__rangatiratanga_retention_reading, 0.28).
domain_priors:suppression_score(treaty_authority_cession__rangatiratanga_retention_reading, 0.12).
domain_priors:theater_ratio(treaty_authority_cession__rangatiratanga_retention_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, accessibility_collapse, 0.18).
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(treaty_authority_cession__rangatiratanga_retention_reading, rope).
narrative_ontology:human_readable(treaty_authority_cession__rangatiratanga_retention_reading, "Te Tiriti o Waitangi — Rangatiratanga Retention Reading (Partnership Governance)").
narrative_ontology:topic_domain(treaty_authority_cession__rangatiratanga_retention_reading, "constitutional/indigenous/colonial").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(treaty_authority_cession__rangatiratanga_retention_reading, '8ffde080-fd56-481c-ae74-5de9fefc63a9').
narrative_ontology:cs_kernel_codification('8ffde080-fd56-481c-ae74-5de9fefc63a9', fixed_text).
narrative_ontology:cs_authority_grounding('8ffde080-fd56-481c-ae74-5de9fefc63a9', lineage).
narrative_ontology:cs_interpretation_layer_present('8ffde080-fd56-481c-ae74-5de9fefc63a9').
narrative_ontology:cs_reading_relation('8ffde080-fd56-481c-ae74-5de9fefc63a9', treaty_authority_cession__crown_cession_reading, forecloses).
narrative_ontology:cs_reading_relation('8ffde080-fd56-481c-ae74-5de9fefc63a9', treaty_authority_cession__retrospective_snare_exposure, coexists_with).
narrative_ontology:cs_axiom('8ffde080-fd56-481c-ae74-5de9fefc63a9', foundational, maori_text_contra_proferentem).
narrative_ontology:cs_axiom_status(maori_text_contra_proferentem, holdable).
narrative_ontology:cs_axiom_grounding('8ffde080-fd56-481c-ae74-5de9fefc63a9', maori_text_contra_proferentem, conventional).
narrative_ontology:cs_axiom('8ffde080-fd56-481c-ae74-5de9fefc63a9', foundational, tino_rangatiratanga_unextinguished).
narrative_ontology:cs_axiom_status(tino_rangatiratanga_unextinguished, holdable).
narrative_ontology:cs_axiom_grounding('8ffde080-fd56-481c-ae74-5de9fefc63a9', tino_rangatiratanga_unextinguished, deontological).
narrative_ontology:cs_axiom('8ffde080-fd56-481c-ae74-5de9fefc63a9', foundational, partnership_requires_ongoing_consent).
narrative_ontology:cs_axiom_status(partnership_requires_ongoing_consent, holdable).
narrative_ontology:cs_axiom_grounding('8ffde080-fd56-481c-ae74-5de9fefc63a9', partnership_requires_ongoing_consent, conventional).
narrative_ontology:cs_reference_frame('8ffde080-fd56-481c-ae74-5de9fefc63a9', te_tiriti_partnership_1840).
narrative_ontology:cs_drift_state('8ffde080-fd56-481c-ae74-5de9fefc63a9', contemporary_settlement_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('8ffde080-fd56-481c-ae74-5de9fefc63a9', '').
narrative_ontology:cs_kernel_id(treaty_authority_cession__rangatiratanga_retention_reading, treaty_authority_cession).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(treaty_authority_cession__rangatiratanga_retention_reading, hapu_iwi).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__rangatiratanga_retention_reading, crown_as_partner).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__rangatiratanga_retention_reading, settler_population).
narrative_ontology:constraint_vindicates(treaty_authority_cession__rangatiratanga_retention_reading, contra_proferentem_interpretation).
narrative_ontology:constraint_vindicates(treaty_authority_cession__rangatiratanga_retention_reading, tino_rangatiratanga_retention).
narrative_ontology:constraint_vindicates(treaty_authority_cession__rangatiratanga_retention_reading, kawanatanga_limited_governance).
narrative_ontology:constraint_vindicates(treaty_authority_cession__rangatiratanga_retention_reading, partnership_ongoing_consent).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Māori tribal collectives who signed te reo Māori text. Retain tino rangatiratanga (full authority over their affairs, lands, taonga). Their consent is required for Crown action affecting their interests. Exit from the partnership framework would mean surrendering the constitutional recognition of their authority — identity-locked because rangatiratanga is constitutive of collective identity, not a negotiable concession.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, hapu_iwi, beneficiary,
    organized, generational, identity_locked, national).

% The Crown gains legitimate governance authority (kāwanatanga) over settlers and shared affairs ONLY through the partnership established by te Tiriti. Its authority is conditional on ongoing hapū consent. Benefits from stable governance framework but constrained: unilateral action breaches the partnership. Cannot exit without losing the legitimacy te Tiriti confers — exit would mean governing without consent, reverting to naked force.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, crown_as_partner, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(treaty_authority_cession__rangatiratanga_retention_reading, crown_as_partner, agenda_setter).

% Non-Māori New Zealanders who gain lawful presence and governance through Crown's kāwanatanga. Benefit from stable institutions, property rights, and public services the partnership enables. Mobile exit: could emigrate, but the partnership framework makes their presence lawful rather than occupation — they are not the constraint's target.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, settler_population, beneficiary,
    organized, biographical, mobile, national).

% Permanent commission of inquiry established 1975. Interprets te Tiriti, hears claims, issues recommendations. Sits outside the partnership as analytical observer — its findings carry moral and political weight but no binding enforcement. Sees the full structure: the partnership's operation, Crown breaches, and the gap between kāwanatanga exercise and hapū consent.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, waitangi_tribunal, observer,
    institutional, generational, analytical, national).

% Judicial branch that adjudicates te Tiriti's legal effect. Has progressively recognized partnership principles (1987 Lands case, 1994 Fisheries case, 2014 Tūrangawaewae). Constrained by parliamentary sovereignty doctrine — can declare inconsistency but cannot strike down legislation. Sets agenda for how Crown must engage with hapū, but operates within Crown's constitutional framework.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, courts, agenda_setter,
    institutional, generational, constrained, national).

% Legislature that exercises kāwanatanga. Can pass laws affecting hapū interests without consent (parliamentary sovereignty). Constrained politically: te Tiriti settlements, Tribunal findings, and international scrutiny create costs for unilateral action. The partnership framework makes parliamentary sovereignty contingent in practice, though not in formal law.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, parliament, agenda_setter,
    institutional, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a constitutional partnership between two sovereign peoples: hapū/iwi retain tino rangatiratanga (full authority over their affairs); Crown exercises kāwanatanga (governance) over settlers and shared matters ONLY with ongoing hapū consent. Solves the coordination problem of how two distinct political orders share one territory without either subsuming the other.
% TRANSFER_FUNCTION: Moves legitimacy and lawful governance authority from hapū to Crown — NOT sovereignty, NOT land, NOT resources. The transfer is conditional: Crown's kāwanatanga is legitimate only while exercised in partnership with hapū consent. No extraction of resources or authority from hapū to Crown; the partnership framework itself is the coordination gain for both.
% ABSENT_VOICES: Hapū who refused to sign te Tiriti (e.g., Tūhoe, Waikato factions) — their absence from the original signing is structural, not incidental. Their descendants contest whether the partnership framework binds them. Also absent: the Crown's 1840 understanding of what it was acquiring — the English text's sovereignty claim was never put to Māori signatories.
% DISAPPEARANCE_RATIONALE: If the partnership constraint vanished, Crown's governance would lose its te Tiriti-derived legitimacy over Māori. Hapū would revert to exercising full tino rangatiratanga without Crown partnership — the constitutional basis for Crown authority in Aotearoa would collapse. Settler property rights and state institutions would face existential legitimacy crisis. The physical world would not change, but the constitutional world would rearrange entirely.
% FOUNDING_PROBLEM: 1840: How to enable British settlement and governance in Aotearoa while protecting Māori authority, land, and culture — without war. The British Crown sought lawful acquisition of sovereignty; rangatira sought protection of their rangatiratanga and regulation of Pākehā. Te Tiriti was the negotiated instrument attempting both.
% FOUNDING_PROBLEM_CORROBORATION: Waitangi Tribunal findings (1985–present) corroborate from institutional seat: the founding problem was mutual recognition and regulated settlement, not cession. Historical scholarship outside beneficiary sets (Orange 1987, Moon 2002, Stirling 2020) confirms Māori signatories understood governance partnership, not sovereignty transfer. Crown's own 1840 instructions to Hobson (protect Māori interests, acquire sovereignty by consent) corroborate the partnership reading from the Crown's own archival record.
narrative_ontology:disappearance_verdict(treaty_authority_cession__rangatiratanga_retention_reading, world_rearranges).
narrative_ontology:founding_problem_status(treaty_authority_cession__rangatiratanga_retention_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(treaty_authority_cession__rangatiratanga_retention_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(treaty_authority_cession__rangatiratanga_retention_reading, 'none', 1).
narrative_ontology:epsilon_provenance(treaty_authority_cession__rangatiratanga_retention_reading, 0.28, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is low (0.28) because the partnership framework itself extracts nothing — it coordinates. The measured extraction reflects residual Crown unilateralism (legislation without consent, settlement terms dictated) and the cost hapū bear to enforce partnership (Tribunal claims, litigation, political mobilization). Suppression is very low (0.12) — the constraint does not suppress alternatives; it IS the alternative to unilateral Crown rule. Theater is minimal (0.08) — partnership rhetoric largely matches practice in contemporary settlements and resource co-governance (Waikato River, Te Urewera, Whanganui River). Accessibility collapse is low (0.18) — the Crown could legally abandon partnership tomorrow (parliamentary sovereignty), but politically cannot. Resistance is moderate (0.42) — hapū continuously assert rangatiratanga; Crown resists full partnership implementation.
 *
 * PERSPECTIVAL GAP:
 *   From hapū seat: the constraint is a Rope — genuine coordination protecting rangatiratanga. From Crown institutional seat (parliament): it can appear as a Snare — constraints on sovereignty, costly settlements, Tribunal findings limiting legislative freedom. From settler seat: it is invisible coordination — lawful presence without conscious negotiation. The engine computes these divergences from the structural data; the claimed_type (rope) reflects the partnership's DESIGN, not every seat's experience of its IMPLEMENTATION.
 *
 * DIRECTIONALITY LOGIC:
 *   hapū_iwi are structural beneficiaries (d ~ 0.15): the constraint protects their authority, requires their consent. Identity-locked exit: leaving the partnership means abandoning constitutional recognition of rangatiratanga — constitutive of identity. crown_as_partner is dual-positioned: beneficiary of legitimate governance (d ~ 0.25) but also agenda_setter bearing coordination costs (negotiation, consent processes, settlements). Constrained exit: unilateral withdrawal loses legitimacy. settler_population are pure beneficiaries (d ~ 0.20): lawful presence via partnership, mobile exit. waitangi_tribunal and courts are analytical/institutional observers (d ~ 0.50): they mediate but do not collect or pay. parliament sits near symmetric (d ~ 0.45): exercises authority but bears political costs of partnership compliance.
 *
 * MANDATROPHY ANALYSIS:
 *   The partnership's founding problem (regulated settlement + Māori protection) is contested — Crown claims it is live (ongoing settlement regulation needed); hapū claim the problem was solved by te Tiriti itself and the arrangement persists as partnership, not mandatrophy. No party treats the constraint as obsolete; all treat it as live constitutional architecture. Mandatrophy_resolved = false.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    translation_asymmetry_extraction,
    'Does the textual divergence between Māori and English texts constitute an extraction mechanism operating at signing (chiefs could not assent to what they did not understand), or is it a subsequent interpretive dispute?',
    'Linguistic analysis of 1840 Māori comprehension of ''kāwanatanga'' vs ''mana''/''rangatiratanga''; historical record of oral explanations at signings; comparative analysis of other British treaties with indigenous peoples using translation intermediaries.',
    'If extraction at signing: the partnership constraint''s legitimacy is foundationally compromised — the Crown gained kāwanatanga through a mechanism the constraint itself would classify as extractive. If interpretive dispute: the partnership constraint stands, and extraction occurred only when Crown breached it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(translation_asymmetry_extraction, empirical, 'Whether translation asymmetry was an extraction mechanism at formation or a later interpretive fracture.').

omega_variable(
    partnership_vs_sovereignty_incommensurability,
    'Are ''partnership requiring ongoing consent'' and ''Crown sovereignty'' structurally incommensurable, or can they be reconciled in a single constitutional framework?',
    'Constitutional theory: analyze whether a framework can simultaneously hold (a) Crown exercises governance only with hapū consent AND (b) Crown holds ultimate legislative authority (parliamentary sovereignty). New Zealand''s current constitution attempts this; the tension is unresolved.',
    'If incommensurable: this reading''s Rope classification is structurally unstable — the constraint contains its own negation. If reconcilable: the partnership is a genuine Rope with institutional friction, not a disguised Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(partnership_vs_sovereignty_incommensurability, conceptual, 'Whether partnership governance and parliamentary sovereignty can coexist in one framework.').

omega_variable(
    kernel_reading_foreclosure_structure,
    'Does this reading''s core premise (Māori text controls, tino rangatiratanga retained, partnership requiring consent) logically foreclose the crown_cession_reading within a single constitutional framework?',
    'Formal analysis of the two readings'' core propositions: (1) te Tiriti transferred sovereignty to Crown vs (2) te Tiriti established partnership with hapū retaining rangatiratanga. Test whether a single legal framework can hold both without contradiction.',
    'If forecloses: the kernel has a genuine forecloses pair — constitutional frameworks must choose. If coexists_with: both readings can be held by different parties simultaneously (current NZ reality). The reading_relation declared here (forecloses) is a structural claim about logical compatibility, not political coexistence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_structure, conceptual, 'Logical foreclosure between rangatiratanga_retention and crown_cession readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(treaty_authority_cession__rangatiratanga_retention_reading, 1840, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trea_tr_t1840, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 1840, 0.02).
narrative_ontology:measurement(trea_tr_t1860, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 1860, 0.15).
narrative_ontology:measurement(trea_tr_t1890, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 1890, 0.28).
narrative_ontology:measurement(trea_tr_t1920, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 1920, 0.35).
narrative_ontology:measurement(trea_tr_t1975, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 1975, 0.22).
narrative_ontology:measurement(trea_tr_t1990, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(trea_tr_t2010, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(trea_tr_t2024, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 2024, 0.08).

% Extraction over time
narrative_ontology:measurement(trea_be_t1840, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 1840, 0.05).
narrative_ontology:measurement(trea_be_t1860, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 1860, 0.35).
narrative_ontology:measurement(trea_be_t1890, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 1890, 0.55).
narrative_ontology:measurement(trea_be_t1920, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 1920, 0.62).
narrative_ontology:measurement(trea_be_t1975, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 1975, 0.48).
narrative_ontology:measurement(trea_be_t1990, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 1990, 0.38).
narrative_ontology:measurement(trea_be_t2010, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 2010, 0.32).
narrative_ontology:measurement(trea_be_t2024, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 2024, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(trea_su_t1840, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 1840, 0.02).
narrative_ontology:measurement(trea_su_t1860, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 1860, 0.55).
narrative_ontology:measurement(trea_su_t1890, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 1890, 0.72).
narrative_ontology:measurement(trea_su_t1920, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 1920, 0.68).
narrative_ontology:measurement(trea_su_t1975, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 1975, 0.35).
narrative_ontology:measurement(trea_su_t1990, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 1990, 0.22).
narrative_ontology:measurement(trea_su_t2010, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 2010, 0.15).
narrative_ontology:measurement(trea_su_t2024, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 2024, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(treaty_authority_cession__rangatiratanga_retention_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(treaty_authority_cession__rangatiratanga_retention_reading, 0.1).
narrative_ontology:affects_constraint(treaty_authority_cession__rangatiratanga_retention_reading, treaty_authority_cession__retrospective_snare_exposure).
narrative_ontology:affects_constraint(treaty_authority_cession__rangatiratanga_retention_reading, treaty_authority_cession__crown_cession_reading).

% DUAL FORMULATION NOTE:
% This constraint (rangatiratanga_retention_reading) and the retrospective_snare_exposure reading form a constraint family decomposing the treaty_authority_cession kernel. The partnership constraint (this story) has low ε (0.28) — genuine coordination. The retrospective snare constraint has high ε — extraction via translation asymmetry and legislative override. They are linked: the snare reading describes what happened when the partnership was breached; this reading describes the partnership that was breached. The crown_cession_reading is the third family member — the Crown's operational reading that enabled the breach.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(treaty_authority_cession__rangatiratanga_retention_reading, institutional, 0.25).
constraint_indexing:directionality_override(treaty_authority_cession__rangatiratanga_retention_reading, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
