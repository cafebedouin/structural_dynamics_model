% ============================================================================
% CONSTRAINT STORY: treaty_authority_cession__retrospective_snare_exposure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_treaty_authority_cession__retrospective_snare_exposure, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: treaty_authority_cession__retrospective_snare_exposure
 *   human_readable: Treaty Textual Divergence as Covert Extraction Mechanism
 *   domain: constitutional/indigenous/colonial
 *
 * SUMMARY:
 *   This constraint story instantiates the retrospective_snare_exposure
 *   reading of the treaty_authority_cession kernel. It treats the textual
 *   divergence between Te Tiriti o Waitangi (Māori text) and the Treaty of
 *   Waitangi (English text) not as a translation error but as the extraction
 *   mechanism itself: chiefs signed a document guaranteeing their tino
 *   rangatiratanga; the Crown enforced a different document claiming cession
 *   of sovereignty. The extraction — 95% of Māori land transferred to
 *   Crown/settler control by 1900, legislative override of Māori law,
 *   constitutional subordination — operated under the cover of this
 *   divergence. The snare classification reflects that the coordination story
 *   (mutual protection and governance) was cover; persistence depended on
 *   coercion (Native Land Court, Suppression of Rebellion Act, confiscation)
 *   and suppressing the Māori text's authority.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(treaty_authority_cession__retrospective_snare_exposure, 0.85).
domain_priors:suppression_score(treaty_authority_cession__retrospective_snare_exposure, 0.9).
domain_priors:theater_ratio(treaty_authority_cession__retrospective_snare_exposure, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, extractiveness, 0.85).
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(treaty_authority_cession__retrospective_snare_exposure, snare).
narrative_ontology:human_readable(treaty_authority_cession__retrospective_snare_exposure, "Treaty Textual Divergence as Covert Extraction Mechanism").
narrative_ontology:topic_domain(treaty_authority_cession__retrospective_snare_exposure, "constitutional/indigenous/colonial").

domain_priors:requires_active_enforcement(treaty_authority_cession__retrospective_snare_exposure).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(treaty_authority_cession__retrospective_snare_exposure, '7e73ca2e-897f-4ff7-8e71-ba707d9a3291').
narrative_ontology:cs_kernel_codification('7e73ca2e-897f-4ff7-8e71-ba707d9a3291', formalized).
narrative_ontology:cs_authority_grounding('7e73ca2e-897f-4ff7-8e71-ba707d9a3291', extraction).
narrative_ontology:cs_interpretation_layer_present('7e73ca2e-897f-4ff7-8e71-ba707d9a3291').
narrative_ontology:cs_reading_relation('7e73ca2e-897f-4ff7-8e71-ba707d9a3291', treaty_authority_cession__crown_cession_reading, forecloses).
narrative_ontology:cs_reading_relation('7e73ca2e-897f-4ff7-8e71-ba707d9a3291', treaty_authority_cession__rangatiratanga_retention_reading, coexists_with).
narrative_ontology:cs_axiom('7e73ca2e-897f-4ff7-8e71-ba707d9a3291', foundational, textual_divergence_as_extraction_mechanism).
narrative_ontology:cs_axiom_status(textual_divergence_as_extraction_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('7e73ca2e-897f-4ff7-8e71-ba707d9a3291', textual_divergence_as_extraction_mechanism, empirically_contingent).
narrative_ontology:cs_axiom('7e73ca2e-897f-4ff7-8e71-ba707d9a3291', foundational, chiefs_could_not_assent_to_english_sovereignty).
narrative_ontology:cs_axiom_status(chiefs_could_not_assent_to_english_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('7e73ca2e-897f-4ff7-8e71-ba707d9a3291', chiefs_could_not_assent_to_english_sovereignty, empirically_contingent).
narrative_ontology:cs_reference_frame('7e73ca2e-897f-4ff7-8e71-ba707d9a3291', treaty_as_cession_instrument).
narrative_ontology:cs_drift_state('7e73ca2e-897f-4ff7-8e71-ba707d9a3291', contemporary_waitangi_tribunal_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('7e73ca2e-897f-4ff7-8e71-ba707d9a3291', '').
narrative_ontology:cs_kernel_id(treaty_authority_cession__retrospective_snare_exposure, treaty_authority_cession).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(treaty_authority_cession__retrospective_snare_exposure, crown_land_purchasing_apparatus).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__retrospective_snare_exposure, colonial_government).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__retrospective_snare_exposure, settler_land_speculators).
narrative_ontology:constraint_victim(treaty_authority_cession__retrospective_snare_exposure, maori_signatories).
narrative_ontology:constraint_victim(treaty_authority_cession__retrospective_snare_exposure, maori_descendants).
narrative_ontology:constraint_victim(treaty_authority_cession__retrospective_snare_exposure, hapu_iwi_collective).
narrative_ontology:constraint_vindicates(treaty_authority_cession__retrospective_snare_exposure, terra_nullius_doctrine).
narrative_ontology:constraint_vindicates(treaty_authority_cession__retrospective_snare_exposure, crown_sovereignty_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Comprises the Colonial Office, Governor, Native Department, and Native Land Court. Sets the rules for land purchase, defines 'valid' title, and administers the conversion of customary title to Crown grants. Collects the surplus between Māori reservation prices and on-sale prices to settlers. The apparatus writes the legislation that overrides the treaty's Māori text protections.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, crown_land_purchasing_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Gains sovereign authority over the territory and the revenue base from land sales. Uses the English text to legitimize legislative supremacy and the Māori text to secure Māori acquiescence. The divergence allows it to claim both the moral authority of consent and the legal authority of cession.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, colonial_government, beneficiary,
    institutional, generational, arbitrage, national).

% Acquire Māori land at fractions of market value through the Crown's monopsony purchasing, then on-sell at substantial profit. Their capital and political influence shape the Native Land Court's operation. They are the immediate financial beneficiaries of the extraction mechanism.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, settler_land_speculators, beneficiary,
    organized, biographical, mobile, national).

% Rangatira who signed the Māori text believing they retained tino rangatiratanga (full authority) over their lands and people while granting kāwanatanga (governorship) to the Crown over settlers. They could not read the English text and were not told it claimed 'cession of sovereignty.' Their authority and land base were progressively stripped through legislation and court rulings relying on the English text.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, maori_signatories, payer,
    organized, biographical, trapped, national).

% Inherit the dispossession: loss of land base, erosion of hapū/iwi authority, and the ongoing struggle to have the Māori text recognized as the controlling instrument. Their identity is constituted through the relationship to whenua (land) and the treaty; exit from the constraint would mean abandoning the core of their collective self-understanding.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, maori_descendants, payer,
    powerless, generational, identity_locked, national).

% The collective polities that were the actual parties to Te Tiriti. Their political authority (tino rangatiratanga) was the target of the extraction. They experience the constraint as the replacement of their law with Crown law, enforced through the Native Land Court's individualization of title and the Crown's legislative override. Some hapū refused to sign and were subjected to the same regime.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, hapu_iwi_collective, payer,
    organized, generational, constrained, national).

% Henry Williams and his son Edward translated the English draft into Māori. Williams' diary records explaining kāwanatanga as 'governorship' not 'sovereignty,' and assuring rangatira their authority would be protected. Their contemporary warnings about the divergence were excluded from the official record and later Crown narratives.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, missionaries_translators, excluded,
    moderate, biographical, constrained, national).

% Established 1975 as a permanent commission of inquiry. Produces retrospective structural analysis of the treaty's operation, documenting the divergence and its extraction effects. Its findings are recommendatory only; the Crown accepts or rejects them. The Tribunal's existence is itself evidence the constraint's extraction mechanism required institutional management.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, waitangi_tribunal, observer,
    institutional, generational, analytical, national).

% From Wi Parata (1877) declaring the treaty a 'simple nullity' to the 1987 Lands case establishing 'partnership' principles, the courts have been the primary site where the textual divergence is litigated. They interpret the constraint but lack power to strike down legislation inconsistent with the treaty (no entrenched constitutional status).
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, new_zealand_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(treaty_authority_cession__retrospective_snare_exposure, crown_land_purchasing_apparatus).
narrative_ontology:fixing_cost_class(treaty_authority_cession__retrospective_snare_exposure, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The treaty solved the Crown's coordination problem: how to acquire legitimate title to Māori land and establish governance over British settlers without expensive warfare. It provided a single legal instrument that could be presented to Māori as protecting their authority and to the British government as ceding sovereignty.
% TRANSFER_FUNCTION: Moves land and sovereign decision-making authority from Māori polities (hapū/iwi) to the Crown and its settler beneficiaries. The transfer operates through the gap between 'kāwanatanga' (governorship, Māori text) and 'cession of sovereignty' (English text) — chiefs assent to the former, the Crown enforces the latter. The mechanism is the Crown's monopoly on translation and interpretation.
% ABSENT_VOICES: Māori signatories themselves (deceased); hapū who refused to sign (e.g., Tāwhiao's Waikato, Te Kooti's followers) and were subjected to the regime anyway; Māori women excluded from the signing process despite their authority in many hapū; the 1835 He Whakaputanga signatories whose declared independence the treaty was meant to respect.
% DISAPPEARANCE_RATIONALE: If the treaty's interpreted meaning (English text as cession) vanished overnight, the entire land tenure system — every Crown grant derived from pre-1865 purchases, every piece of legislation premised on Crown sovereignty — would lose its foundational legitimacy. The constitutional order would require reconstruction from the Māori text up: tino rangatiratanga as the starting point, with kāwanatanga as a delegated authority requiring ongoing hapū consent.
% FOUNDING_PROBLEM: The Crown needed a legitimate mechanism to acquire Māori land and govern British settlers in New Zealand without the cost and risk of conquest. The Colonial Office required a treaty to satisfy humanitarian networks and international law; the New Zealand Company required one to validate its speculative land claims.
% FOUNDING_PROBLEM_CORROBORATION: Waitangi Tribunal Stage 1 Report (2014) finds the Crown's land acquisition problem was substantially solved by 1865; the arrangement persists as extraction. Historical scholarship (Orange 1987, Belich 1996, Moon 2002) documents the shift from treaty-as-consent to treaty-as-cession-instrument. The Crown's own 1987 Lands case admission that the treaty created 'partnership' obligations acknowledges the founding problem (legitimate acquisition) has transformed into an ongoing extraction relationship.
narrative_ontology:disappearance_verdict(treaty_authority_cession__retrospective_snare_exposure, world_rearranges).
narrative_ontology:founding_problem_status(treaty_authority_cession__retrospective_snare_exposure, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(treaty_authority_cession__retrospective_snare_exposure, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(treaty_authority_cession__retrospective_snare_exposure, 'none', 1).
narrative_ontology:epsilon_provenance(treaty_authority_cession__retrospective_snare_exposure, 0.85, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(treaty_authority_cession__retrospective_snare_exposure_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(treaty_authority_cession__retrospective_snare_exposure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(treaty_authority_cession__retrospective_snare_exposure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.85 because the transfer of land and authority was massive, one-directional, and sustained over 184 years. Suppression is 0.9 because the constraint's operation required active enforcement: legislation overriding the treaty, courts denying its legal force, police/military suppressing resistance (Parihaka, Waikato, Tūhoe). Theater ratio is low (0.15) because the extraction was real and material, not performative — though 'partnership' rhetoric increased after 1975, the material transfers continued. Accessibility collapse is 0.9 because once the English text was institutionalized as 'the Treaty,' alternatives (Māori text as controlling, contra proferentem, international law) were structurally excluded. Resistance is 0.6: substantial (Kīngitanga, Parihaka, 1975 land march, Bastion Point, modern claims) but contained by the constraint's enforcement machinery.
 *
 * PERSPECTIVAL GAP:
 *   From the Crown seat (agenda_setter/beneficiary), the constraint appears as a legitimate cession instrument that solved a coordination problem — the divergence is a minor translation issue. From the Māori payer seats, the same structure is experienced as a snare: the document they signed was not the document enforced. The engine computes this divergence from the structural data; the authored claim (snare) reflects the payer seats' structural reality, not the agenda-setter's framing.
 *
 * DIRECTIONALITY LOGIC:
 *   The Crown land-purchasing apparatus (agenda_setter) sits at d ≈ 0.05 — full beneficiary, writes the rules, collects the surplus. Colonial government and settler speculators (beneficiaries) sit at d ≈ 0.15 — collect land/authority, mobile exit. Māori signatories (payer) at d ≈ 0.95 — trapped, identity-locked, could not exit the relationship without abandoning their whenua and mana. Descendants at d ≈ 0.9 — identity-locked across generations. Hapū/iwi collective at d ≈ 0.85 — constrained exit (Waitangi Tribunal process exists but is recommendatory). Missionaries/translators at d ≈ 0.7 — excluded from the interpretive monopoly they helped create. Tribunal and courts at d ≈ 0.0 (analytical) — observer seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (legitimate land acquisition without conquest) was solved by 1865 — the Crown had acquired sufficient land and established governance. The arrangement persisted not because the problem remained live but because the extraction mechanism (textual divergence + interpretive monopoly) had become self-sustaining: the Crown's land-purchasing apparatus, settler political economy, and constitutional order all depended on the English text's authority. The mandatrophy is resolved: the constraint is a zombie arrangement maintained by the beneficiaries of its extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_divergence_intent,
    'Was the divergence between the Māori and English texts of Te Tiriti o Waitangi a deliberate drafting strategy to secure Māori assent to a document whose English version ceded sovereignty, or an accidental consequence of translation difficulties?',
    'Archival research into the drafting process: Hobson''s instructions, Busby''s drafts, Williams'' translation notes, and the 5 February 1840 meeting records. Comparative analysis of other British treaty-making in the same period.',
    'If deliberate, the constraint is a designed snare from inception. If accidental, it became a snare through subsequent Crown insistence on the English text''s authority despite known divergence — the extraction mechanism is the refusal to acknowledge the Māori text as controlling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_divergence_intent, empirical, 'Whether the extraction mechanism was designed or emergent.').

omega_variable(
    retrospective_visibility,
    'Can the extraction mechanism be identified as such without presentist moral framing, or does the snare classification depend on contemporary values that the historical actors did not share?',
    'Contemporaneous evidence: Māori protests at Kohimarama (1860), Waitangi (1870s), and petitions to Parliament; Crown officials'' private correspondence acknowledging Māori understanding differed; the Native Land Court''s own records showing purchasers exploiting the divergence.',
    'If the extraction was visible to participants at the time (even if powerless to stop it), the snare classification rests on structural facts, not retrospective judgment. If only visible now, the classification carries a conceptual omega about historicist evaluation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(retrospective_visibility, conceptual, 'Whether the snare''s extractive character was structurally visible to historical agents.').

omega_variable(
    contra_proferentem_application,
    'Does the contra proferentem principle (ambiguity construed against the drafter) apply to Te Tiriti given the Crown drafted the English text and commissioned the Māori translation, or does the treaty''s constitutional status place it outside ordinary interpretive rules?',
    'Legal analysis: the 1987 Lands case, the 2014 Waitangi Tribunal Stage 1 Report (Te Paparahi o Te Raki), and international law on treaty interpretation (Vienna Convention Article 31-33). The Crown''s own 1989 ''Principles for Crown Action on the Treaty of Waitangi'' as subsequent practice.',
    'If contra proferentem applies, the Māori text controls and the English sovereignty claim fails — the extraction mechanism is legally unfounded. If it does not apply, the Crown''s interpretive monopoly is structurally entrenched.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contra_proferentem_application, conceptual, 'Whether standard interpretive rules constrain the Crown''s reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(treaty_authority_cession__retrospective_snare_exposure, 0, 184).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trea_tr_t0, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(trea_tr_t0, observed).
narrative_ontology:measurement(trea_tr_t20, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 20, 0.1).
narrative_ontology:measurement_basis(trea_tr_t20, observed).
narrative_ontology:measurement(trea_tr_t40, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 40, 0.15).
narrative_ontology:measurement_basis(trea_tr_t40, observed).
narrative_ontology:measurement(trea_tr_t60, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 60, 0.2).
narrative_ontology:measurement_basis(trea_tr_t60, observed).
narrative_ontology:measurement(trea_tr_t80, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 80, 0.25).
narrative_ontology:measurement_basis(trea_tr_t80, observed).
narrative_ontology:measurement(trea_tr_t100, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 100, 0.2).
narrative_ontology:measurement_basis(trea_tr_t100, observed).
narrative_ontology:measurement(trea_tr_t120, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 120, 0.15).
narrative_ontology:measurement_basis(trea_tr_t120, observed).
narrative_ontology:measurement(trea_tr_t140, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 140, 0.15).
narrative_ontology:measurement_basis(trea_tr_t140, observed).
narrative_ontology:measurement(trea_tr_t160, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 160, 0.15).
narrative_ontology:measurement_basis(trea_tr_t160, observed).
narrative_ontology:measurement(trea_tr_t184, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 184, 0.15).
narrative_ontology:measurement_basis(trea_tr_t184, observed).

% Extraction over time
narrative_ontology:measurement(trea_be_t0, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 0, 0.1).
narrative_ontology:measurement_basis(trea_be_t0, observed).
narrative_ontology:measurement(trea_be_t20, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 20, 0.35).
narrative_ontology:measurement_basis(trea_be_t20, observed).
narrative_ontology:measurement(trea_be_t40, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 40, 0.65).
narrative_ontology:measurement_basis(trea_be_t40, observed).
narrative_ontology:measurement(trea_be_t60, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 60, 0.8).
narrative_ontology:measurement_basis(trea_be_t60, observed).
narrative_ontology:measurement(trea_be_t80, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 80, 0.85).
narrative_ontology:measurement_basis(trea_be_t80, observed).
narrative_ontology:measurement(trea_be_t100, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 100, 0.85).
narrative_ontology:measurement_basis(trea_be_t100, observed).
narrative_ontology:measurement(trea_be_t120, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 120, 0.8).
narrative_ontology:measurement_basis(trea_be_t120, observed).
narrative_ontology:measurement(trea_be_t140, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 140, 0.75).
narrative_ontology:measurement_basis(trea_be_t140, observed).
narrative_ontology:measurement(trea_be_t160, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 160, 0.7).
narrative_ontology:measurement_basis(trea_be_t160, observed).
narrative_ontology:measurement(trea_be_t184, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 184, 0.85).
narrative_ontology:measurement_basis(trea_be_t184, observed).

% Suppression requirement over time
narrative_ontology:measurement(trea_su_t0, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 0, 0.7).
narrative_ontology:measurement_basis(trea_su_t0, observed).
narrative_ontology:measurement(trea_su_t20, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 20, 0.8).
narrative_ontology:measurement_basis(trea_su_t20, observed).
narrative_ontology:measurement(trea_su_t40, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 40, 0.9).
narrative_ontology:measurement_basis(trea_su_t40, observed).
narrative_ontology:measurement(trea_su_t60, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 60, 0.95).
narrative_ontology:measurement_basis(trea_su_t60, observed).
narrative_ontology:measurement(trea_su_t80, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 80, 0.95).
narrative_ontology:measurement_basis(trea_su_t80, observed).
narrative_ontology:measurement(trea_su_t100, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 100, 0.9).
narrative_ontology:measurement_basis(trea_su_t100, observed).
narrative_ontology:measurement(trea_su_t120, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 120, 0.8).
narrative_ontology:measurement_basis(trea_su_t120, observed).
narrative_ontology:measurement(trea_su_t140, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 140, 0.7).
narrative_ontology:measurement_basis(trea_su_t140, observed).
narrative_ontology:measurement(trea_su_t160, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 160, 0.6).
narrative_ontology:measurement_basis(trea_su_t160, observed).
narrative_ontology:measurement(trea_su_t184, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 184, 0.9).
narrative_ontology:measurement_basis(trea_su_t184, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(treaty_authority_cession__retrospective_snare_exposure, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(treaty_authority_cession__retrospective_snare_exposure, 0.1).
narrative_ontology:affects_constraint(treaty_authority_cession__retrospective_snare_exposure, treaty_authority_cession__crown_cession_reading).
narrative_ontology:affects_constraint(treaty_authority_cession__retrospective_snare_exposure, treaty_authority_cession__rangatiratanga_retention_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the single label 'Treaty of Waitangi' into three structurally distinct constraints with different ε values, beneficiary/victim structures, and types. The crown_cession_reading claims Mountain (natural law of cession); rangatiratanga_retention_reading claims Tangled Rope (coordination with asymmetric extraction); this reading claims Snare (pure extraction). Their ε values differ by >0.6. They are linked via affects_constraints because the Crown's interpretive monopoly (crown_cession_reading) is the enforcement mechanism for this reading's extraction, and the rangatiratanga_retention_reading's partnership framework is the contested alternative that this reading shows is structurally suppressed.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(treaty_authority_cession__retrospective_snare_exposure, moderate, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
