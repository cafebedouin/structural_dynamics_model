% ============================================================================
% CONSTRAINT STORY: treaty_authority_cession__crown_cession_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_treaty_authority_cession__crown_cession_reading, []).

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
 *   constraint_id: treaty_authority_cession__crown_cession_reading
 *   human_readable: Treaty of Waitangi — Crown Cession Reading (English Text Sovereignty)
 *   domain: constitutional_law/indigenous_rights/colonial_history
 *
 * SUMMARY:
 *   This story instantiates the CROWN CESSION READING of the Treaty of
 *   Waitangi kernel: the English-language text is treated as controlling,
 *   'kawanatanga' is read as equivalent to full sovereignty, and the treaty
 *   is understood as completing a legal cession of governance and,
 *   functionally, of land authority to the Crown. This is one of three
 *   structurally distinct readings of the same founding instrument — the
 *   rangatiratanga retention reading (Maori text controls, kawanatanga is
 *   limited governance, tino rangatiratanga retained, treaty establishes an
 *   ongoing partnership) and the retrospective snare exposure reading (the
 *   textual divergence itself is the extraction mechanism, since chiefs
 *   signing the Maori text could not have assented to what the English text
 *   claims) are separate constraints, not alternative measurements of this
 *   one. Under the ε-invariance principle, each reading gets its own file
 *   because each has a different beneficiary/victim structure and a different
 *   persistence mechanism. This reading is authored as it operated
 *   historically and juridically: as the Crown's own account of what happened
 *   at Waitangi, later entrenched through legislation, land courts, and
 *   constitutional doctrine.
 *
 * KEY AGENTS:
 *   - crown_government: agenda_setter (institutional/arbitrage) — sets and enforces the cession reading through law and administration
 *   - settler_land_purchasers: beneficiary (organized/mobile) — acquire land whose title validity depends on the cession reading
 *   - colonial_administrative_apparatus: beneficiary/agenda_setter (institutional/arbitrage) — operationalizes the reading through courts and land commissions
 *   - signatory_hapu: payer (powerless/trapped) — bound by an English text they did not sign or, in most cases, read
 *   - iwi_land_holders: payer (moderate/trapped) — lose customary land interests through processes premised on this reading
 *   - subsequent_maori_generations: payer (powerless/trapped) — inherit compounding consequences
 *   - waitangi_tribunal: observer (institutional/analytical) — investigates but historically lacked binding authority
 *   - constitutional_courts: observer (institutional/analytical) — adjudicate from within the sovereignty premise being contested
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(treaty_authority_cession__crown_cession_reading, 0.81).
domain_priors:suppression_score(treaty_authority_cession__crown_cession_reading, 0.87).
domain_priors:theater_ratio(treaty_authority_cession__crown_cession_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, suppression_requirement, 0.87).
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(treaty_authority_cession__crown_cession_reading, tangled_rope).
narrative_ontology:human_readable(treaty_authority_cession__crown_cession_reading, "Treaty of Waitangi — Crown Cession Reading (English Text Sovereignty)").
narrative_ontology:topic_domain(treaty_authority_cession__crown_cession_reading, "constitutional_law/indigenous_rights/colonial_history").

domain_priors:requires_active_enforcement(treaty_authority_cession__crown_cession_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(treaty_authority_cession__crown_cession_reading, '9ae7b879-01f1-49f1-9e44-7c3401f1772f').
narrative_ontology:cs_kernel_codification('9ae7b879-01f1-49f1-9e44-7c3401f1772f', fixed_text).
narrative_ontology:cs_authority_grounding('9ae7b879-01f1-49f1-9e44-7c3401f1772f', extraction).
narrative_ontology:cs_interpretation_layer_present('9ae7b879-01f1-49f1-9e44-7c3401f1772f').
narrative_ontology:cs_reading_relation('9ae7b879-01f1-49f1-9e44-7c3401f1772f', treaty_authority_cession__rangatiratanga_retention_reading, forecloses).
narrative_ontology:cs_reading_relation('9ae7b879-01f1-49f1-9e44-7c3401f1772f', treaty_authority_cession__retrospective_snare_exposure, influences).
narrative_ontology:cs_axiom('9ae7b879-01f1-49f1-9e44-7c3401f1772f', foundational, english_text_is_controlling_instrument).
narrative_ontology:cs_axiom_status(english_text_is_controlling_instrument, holdable).
narrative_ontology:cs_axiom_grounding('9ae7b879-01f1-49f1-9e44-7c3401f1772f', english_text_is_controlling_instrument, conventional).
narrative_ontology:cs_axiom('9ae7b879-01f1-49f1-9e44-7c3401f1772f', foundational, kawanatanga_equals_full_sovereignty_transfer).
narrative_ontology:cs_axiom_status(kawanatanga_equals_full_sovereignty_transfer, holdable).
narrative_ontology:cs_axiom_grounding('9ae7b879-01f1-49f1-9e44-7c3401f1772f', kawanatanga_equals_full_sovereignty_transfer, conventional).
narrative_ontology:cs_axiom('9ae7b879-01f1-49f1-9e44-7c3401f1772f', secondary, crown_radical_title_extends_to_all_ceded_territory).
narrative_ontology:cs_axiom_status(crown_radical_title_extends_to_all_ceded_territory, holdable).
narrative_ontology:cs_axiom_grounding('9ae7b879-01f1-49f1-9e44-7c3401f1772f', crown_radical_title_extends_to_all_ceded_territory, instrumental).
narrative_ontology:cs_reference_frame('9ae7b879-01f1-49f1-9e44-7c3401f1772f', crown_antecedent_sovereignty_framework).
narrative_ontology:cs_drift_state('9ae7b879-01f1-49f1-9e44-7c3401f1772f', post_waitangi_tribunal_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9ae7b879-01f1-49f1-9e44-7c3401f1772f', '').
narrative_ontology:cs_kernel_id(treaty_authority_cession__crown_cession_reading, treaty_authority_cession).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(treaty_authority_cession__crown_cession_reading, crown_government).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__crown_cession_reading, settler_land_purchasers).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__crown_cession_reading, colonial_administrative_apparatus).
narrative_ontology:constraint_victim(treaty_authority_cession__crown_cession_reading, signatory_hapu).
narrative_ontology:constraint_victim(treaty_authority_cession__crown_cession_reading, iwi_land_holders).
narrative_ontology:constraint_victim(treaty_authority_cession__crown_cession_reading, subsequent_maori_generations).
narrative_ontology:constraint_vindicates(treaty_authority_cession__crown_cession_reading, parliamentary_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(treaty_authority_cession__crown_cession_reading, crown_radical_title_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafts, ratifies, and enforces the English-text reading through courts, legislature, and land administration. Declares kawanatanga equivalent to sovereignty in the English version signed by only a minority of chiefs, and treats the treaty as completing legal cession of governance and, functionally, of land access. Collects legislative supremacy and land title as the direct product of this reading.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, crown_government, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Acquire land through Crown pre-emption and subsequent sale, relying on the cession reading to validate that the Crown holds radical title over all territory covered by the treaty. Their claims to land are legally secure only if the English-text sovereignty reading holds.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, settler_land_purchasers, beneficiary,
    organized, generational, mobile, national).

% Courts, land commissions, and native land legislation operate on the presumption that the Crown's sovereignty is total and antecedent, using this presumption to adjudicate title, impose statute over customary law, and process land confiscation and sale without requiring ongoing Maori consent.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, colonial_administrative_apparatus, beneficiary,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(treaty_authority_cession__crown_cession_reading, colonial_administrative_apparatus, agenda_setter).

% Signed the Maori-language text at Waitangi and elsewhere, in which kawanatanga was understood as a limited grant of governance for order and trade, not the surrender of tino rangatiratanga. Under the crown cession reading, their understanding is treated as legally irrelevant; the English text controls regardless of what was read aloud or agreed to at signing.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, signatory_hapu, payer,
    powerless, generational, trapped, regional).

% Hold customary land interests that are progressively extinguished, confiscated, or converted through Native Land Court processes premised on Crown sovereignty and radical title. Legal recourse depends entirely on institutions that operate from the reading they are contesting.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, iwi_land_holders, payer,
    moderate, generational, trapped, regional).

% Inherit the compounding legal and economic consequences of land alienation legitimated by the cession reading — reduced landholding, disrupted governance structures, and a legal system that treats the loss as settled history rather than an ongoing, remediable extraction.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, subsequent_maori_generations, payer,
    powerless, civilizational, trapped, national).

% Investigates historical Crown conduct and treaty breaches, taking evidence on both textual readings. Can recommend remedy but historically lacked binding authority over legislative sovereignty claims already exercised under the cession reading.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, waitangi_tribunal, observer,
    institutional, generational, analytical, national).

% Adjudicate disputes arising from the treaty's legal status, historically deferring to parliamentary sovereignty and treating the treaty as not directly enforceable domestic law absent statutory incorporation — a stance itself dependent on accepting the cession reading's premise of prior, complete Crown sovereignty.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(treaty_authority_cession__crown_cession_reading, crown_government).
narrative_ontology:fixing_cost_class(treaty_authority_cession__crown_cession_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, administrable legal foundation for governance, law enforcement, and land title across the colony — settling (from the Crown's side) which authority's law applies and enabling orderly land transfer and infrastructure development under one sovereign framework.
% TRANSFER_FUNCTION: Moves land, legislative authority, and customary governance capacity from signatory hapu and iwi to the Crown and, through Crown land sales, to settler purchasers — legitimated as the completion of a sovereignty cession rather than as an ongoing transfer requiring continued consent.
% ABSENT_VOICES: The chiefs who signed the Maori-language text, and their descendants, were never party to the English-text drafting or its later judicial and legislative elaboration; their understanding of kawanatanga as limited governance is treated as extrinsic to the controlling instrument. Contemporary Maori legal scholars and the Waitangi Tribunal raise this but operate downstream of institutions already committed to the cession premise.
% DISAPPEARANCE_RATIONALE: If the crown cession reading lost its controlling status, the entire chain of land title, legislative supremacy, and administrative authority built on presumed antecedent Crown sovereignty would require re-grounding — land titles derived from Crown pre-emption, statutes overriding customary law, and the constitutional architecture treating Parliament as unconstrained would all become contestable at their root.
% FOUNDING_PROBLEM: The stated problem was establishing lawful order and protecting Maori from unregulated settler encroachment and inter-hapu conflict by introducing a single governing authority (kawanatanga) recognized by, and negotiated with, Maori chiefs.
% FOUNDING_PROBLEM_CORROBORATION: The Crown's own subsequent legal and legislative tradition attests the founding problem as solved via total sovereignty transfer. Outside the benefiting institutions, the Waitangi Tribunal's historical reports, independent linguistic and historical scholarship on the 1840 negotiations, and comparative treaty-interpretation analysis attest that the signed Maori text does not support a full-sovereignty-cession reading — corroboration against the cession reading comes substantially from outside the parties who benefit from it.
narrative_ontology:disappearance_verdict(treaty_authority_cession__crown_cession_reading, world_rearranges).
narrative_ontology:founding_problem_status(treaty_authority_cession__crown_cession_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(treaty_authority_cession__crown_cession_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(treaty_authority_cession__crown_cession_reading, 'none', 1).
narrative_ontology:epsilon_provenance(treaty_authority_cession__crown_cession_reading, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(treaty_authority_cession__crown_cession_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(treaty_authority_cession__crown_cession_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(treaty_authority_cession__crown_cession_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.81) because the reading legitimates land alienation and legislative override without requiring ongoing Maori consent — a direct, structural transfer of authority and resource from signatory communities to the Crown and settler purchasers. Suppression is authored even higher (0.87) because the reading's persistence has depended on active enforcement: military campaigns following the 1860s land wars, the Native Land Court's individualization of communal title, and statutory declarations that the treaty was not part of domestic law, all of which foreclosed the possibility of the alternative reading gaining legal traction. Theater ratio is moderate (0.42) — genuine administrative and legal machinery operates on this reading (it is not pure performance), but an increasing share of its late-20th-century maintenance (Waitangi Tribunal hearings, treaty settlement processes) is now devoted to managing the reputational and political costs of a reading whose historical accuracy is increasingly conceded even by Crown-aligned institutions. Accessibility collapse (0.62) reflects that alternatives were not fully foreclosed — the Maori text always existed and was never suppressed as a document — but access to a court or legislature that would act on it was closed for over a century. Resistance is authored high (0.78): this reading met continuous, organized Maori resistance from 1840 onward, including armed resistance, land leagues, petitions to the Crown and Privy Council, and sustained legal and political challenge culminating in the Waitangi Tribunal.
 *
 * PERSPECTIVAL GAP:
 *   From the Crown's seat, this reading is the settled, legitimate constitutional foundation — a genuine coordination achievement establishing lawful government over a previously ungoverned colonial space. From the signatory hapu seat, the identical structure is an imposed extraction dressed in the language of consent: a text they did not sign, read as displacing an agreement they did sign. The engine should compute divergent seat types precisely because the structural positions (agenda_setter vs. trapped payer) are genuinely asymmetric, not because of rhetorical framing.
 *
 * DIRECTIONALITY LOGIC:
 *   The Crown and its administrative apparatus sit at the beneficiary end: they authored the controlling text, enforce its reading, and collect legislative and territorial authority as its direct product — d near zero. Settler land purchasers are secondary beneficiaries with mobile exit (their claims are portable once secured) but their title security is entirely parasitic on the cession reading holding. Signatory hapu and iwi land holders sit at the target end: trapped exit (customary authority and land cannot be relocated), generational time horizon, and no meaningful institutional lever to contest the reading from within the very system built on it — d near one. Subsequent generations inherit this position at civilizational time horizon, which the engine should treat as amplifying rather than diluting the extraction, since the harm compounds rather than resolves.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — establishing lawful order recognized by Maori chiefs — is contested as to whether it was ever actually solved by THIS reading, since the reading itself is the disputed instrument. Authoring tangled_rope rather than pure snare reflects that a genuine coordination function existed (the Maori signatories did seek a form of governance to manage settler conflict) alongside asymmetric extraction (the cession reading extends that limited grant into total sovereignty and land alienation). Classifying this as snare would erase the genuine, if bounded, consent the Maori text represents; classifying it as rope would launder the century of enforced land loss. Tangled rope holds both facts in view without collapsing them.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_authority_selection,
    'Which language version of the treaty is the legally and morally authoritative instrument when the two versions materially diverge on the scope of ceded authority?',
    'Comparative treaty law analysis of contra proferentem principles applied to indigenous treaties, combined with historical linguistic reconstruction of how ''kawanatanga'' and ''tino rangatiratanga'' were understood by signatories in 1840, and testimony recorded at the signing itself (missionary accounts, oral tradition).',
    'If the Maori text is held authoritative, the entire cession reading''s legal foundation dissolves and the constraint reclassifies toward the rangatiratanga_retention_reading structure — beneficiary and victim positions would substantially invert or dissolve.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_authority_selection, conceptual, 'Which treaty text controls when the two diverge on the scope of sovereignty ceded.').

omega_variable(
    cession_naturalization_vs_construction,
    'Is the crown cession reading''s dominance a natural consequence of the Crown''s superior military and administrative capacity following 1840, or a constructed legal artifact maintained by identifiable institutional beneficiaries who could revise it?',
    'Trace whether Crown legal and legislative institutions have, when confronted with the linguistic evidence, revised the reading (partial evidence: Waitangi Tribunal findings, some judicial acknowledgment) versus continuing to enforce it where politically costly to revise (land title, resource allocation).',
    'If constructed and revisable, the persistence of the cession reading past the point of acknowledged historical inaccuracy indicates piton-like inertia layered onto the original extraction, strengthening the case for tangled_rope over any naturalized framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cession_naturalization_vs_construction, empirical, 'Whether the reading''s persistence reflects genuine historical inevitability or maintained institutional benefit despite acknowledged inaccuracy.').

omega_variable(
    remedy_sufficiency_ambiguity,
    'Do modern Treaty settlement processes (Waitangi Tribunal recommendations, negotiated settlements) constitute genuine correction of the cession reading''s extraction, or a managed, capped remedy that preserves the underlying sovereignty and title structure while addressing only a fraction of its consequences?',
    'Quantitative comparison of settlement quantum against independently assessed value of land and resources alienated, and analysis of whether settlements require claimants to waive further historical claims (finality clauses) regardless of remaining harm.',
    'If settlements are capped and finality-driven rather than proportionate, the declining extractiveness measurements in later years partly reflect theater (managed reconciliation) rather than genuine reduction in extraction — this would revise the theater_ratio trajectory upward for the 1990-2020 period.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(remedy_sufficiency_ambiguity, empirical, 'Whether declining measured extraction in recent decades reflects real remedy or managed containment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(treaty_authority_cession__crown_cession_reading, 1840, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trea_tr_t1840, treaty_authority_cession__crown_cession_reading, theater_ratio, 1840, 0.2).
narrative_ontology:measurement(trea_tr_t1863, treaty_authority_cession__crown_cession_reading, theater_ratio, 1863, 0.25).
narrative_ontology:measurement(trea_tr_t1900, treaty_authority_cession__crown_cession_reading, theater_ratio, 1900, 0.3).
narrative_ontology:measurement(trea_tr_t1940, treaty_authority_cession__crown_cession_reading, theater_ratio, 1940, 0.35).
narrative_ontology:measurement(trea_tr_t1975, treaty_authority_cession__crown_cession_reading, theater_ratio, 1975, 0.4).
narrative_ontology:measurement(trea_tr_t2000, treaty_authority_cession__crown_cession_reading, theater_ratio, 2000, 0.42).
narrative_ontology:measurement(trea_tr_t2020, treaty_authority_cession__crown_cession_reading, theater_ratio, 2020, 0.42).

% Extraction over time
narrative_ontology:measurement(trea_be_t1840, treaty_authority_cession__crown_cession_reading, base_extractiveness, 1840, 0.55).
narrative_ontology:measurement(trea_be_t1863, treaty_authority_cession__crown_cession_reading, base_extractiveness, 1863, 0.7).
narrative_ontology:measurement(trea_be_t1900, treaty_authority_cession__crown_cession_reading, base_extractiveness, 1900, 0.82).
narrative_ontology:measurement(trea_be_t1940, treaty_authority_cession__crown_cession_reading, base_extractiveness, 1940, 0.79).
narrative_ontology:measurement(trea_be_t1975, treaty_authority_cession__crown_cession_reading, base_extractiveness, 1975, 0.68).
narrative_ontology:measurement(trea_be_t2000, treaty_authority_cession__crown_cession_reading, base_extractiveness, 2000, 0.62).
narrative_ontology:measurement(trea_be_t2020, treaty_authority_cession__crown_cession_reading, base_extractiveness, 2020, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(trea_su_t1840, treaty_authority_cession__crown_cession_reading, suppression_requirement, 1840, 0.5).
narrative_ontology:measurement(trea_su_t1863, treaty_authority_cession__crown_cession_reading, suppression_requirement, 1863, 0.88).
narrative_ontology:measurement(trea_su_t1900, treaty_authority_cession__crown_cession_reading, suppression_requirement, 1900, 0.9).
narrative_ontology:measurement(trea_su_t1940, treaty_authority_cession__crown_cession_reading, suppression_requirement, 1940, 0.85).
narrative_ontology:measurement(trea_su_t1975, treaty_authority_cession__crown_cession_reading, suppression_requirement, 1975, 0.75).
narrative_ontology:measurement(trea_su_t2000, treaty_authority_cession__crown_cession_reading, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(trea_su_t2020, treaty_authority_cession__crown_cession_reading, suppression_requirement, 2020, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(treaty_authority_cession__crown_cession_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(treaty_authority_cession__crown_cession_reading, treaty_authority_cession__rangatiratanga_retention_reading).
narrative_ontology:affects_constraint(treaty_authority_cession__crown_cession_reading, treaty_authority_cession__retrospective_snare_exposure).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the treaty_authority_cession kernel. The crown_cession_reading (this file) authors the English-text-controls, full-sovereignty interpretation as historically and juridically dominant, with high extraction and suppression reflecting enforced land alienation and legislative override. The rangatiratanga_retention_reading authors the Maori-text-controls interpretation with a substantially different beneficiary/victim structure — potentially inverting who is coordinated versus who pays, since a limited-governance kawanatanga reading would frame Maori as retaining sovereign authority the Crown's subsequent conduct breached. The retrospective_snare_exposure reading treats the textual divergence itself as the extraction mechanism, authored as a pure snare rather than tangled_rope, since under that reading no genuine bilateral coordination occurred at all — only a mistranslation exploited retrospectively. All three are linked here; each carries its own epsilon, its own claimed_type, and its own stakeholder set, per the epsilon-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
