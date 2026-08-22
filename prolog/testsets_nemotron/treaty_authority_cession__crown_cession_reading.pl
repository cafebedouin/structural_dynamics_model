% ============================================================================
% CONSTRAINT STORY: treaty_authority_cession__crown_cession_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-27
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
 *   constraint_id: treaty_authority_cession__crown_cession_reading
 *   human_readable: Treaty of Waitangi — Crown Cession Reading (English text sovereignty)
 *   domain: constitutional/indigenous/colonial
 *
 * SUMMARY:
 *   This constraint story instantiates the Crown cession reading of the
 *   Treaty of Waitangi: the English text controls, 'kāwanatanga' in Article 1
 *   equals full sovereignty cession, and the treaty completes the legal
 *   transfer of authority to the Crown. Under this reading, Māori customary
 *   authority (tino rangatiratanga) is extinguished or made subordinate to
 *   Crown law; land alienation through Crown pre-emption and later free
 *   market is legitimate. The constraint operates as a snare: high extraction
 *   (land, authority, sovereignty) from Māori, actively enforced through
 *   legislation, courts, and military force, with the coordination story
 *   (single sovereign for orderly settlement) functioning as cover. The
 *   extraction accumulated over 1840-1900 as Māori land dropped from ~66M
 *   acres to ~3M acres; theater ratio rose as the 'protection' narrative
 *   thinned; suppression peaked during the New Zealand Wars and Native Land
 *   Court era. The Waitangi Tribunal (1975) introduced analytical scrutiny
 *   but the structural constraint persists — Crown sovereignty remains the
 *   operating constitutional premise.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(treaty_authority_cession__crown_cession_reading, 0.82).
domain_priors:suppression_score(treaty_authority_cession__crown_cession_reading, 0.78).
domain_priors:theater_ratio(treaty_authority_cession__crown_cession_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(treaty_authority_cession__crown_cession_reading, snare).
narrative_ontology:human_readable(treaty_authority_cession__crown_cession_reading, "Treaty of Waitangi — Crown Cession Reading (English text sovereignty)").
narrative_ontology:topic_domain(treaty_authority_cession__crown_cession_reading, "constitutional/indigenous/colonial").

domain_priors:requires_active_enforcement(treaty_authority_cession__crown_cession_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(treaty_authority_cession__crown_cession_reading, '9904861c-6f3f-49fc-8266-d69fe8de58cc').
narrative_ontology:cs_kernel_codification('9904861c-6f3f-49fc-8266-d69fe8de58cc', fixed_text).
narrative_ontology:cs_authority_grounding('9904861c-6f3f-49fc-8266-d69fe8de58cc', extraction).
narrative_ontology:cs_interpretation_layer_present('9904861c-6f3f-49fc-8266-d69fe8de58cc').
narrative_ontology:cs_reading_relation('9904861c-6f3f-49fc-8266-d69fe8de58cc', treaty_authority_cession__rangatiratanga_retention_reading, forecloses).
narrative_ontology:cs_reading_relation('9904861c-6f3f-49fc-8266-d69fe8de58cc', treaty_authority_cession__retrospective_snare_exposure, influences).
narrative_ontology:cs_axiom('9904861c-6f3f-49fc-8266-d69fe8de58cc', foundational, english_text_sovereignty_cession).
narrative_ontology:cs_axiom_status(english_text_sovereignty_cession, holdable).
narrative_ontology:cs_axiom_grounding('9904861c-6f3f-49fc-8266-d69fe8de58cc', english_text_sovereignty_cession, conventional).
narrative_ontology:cs_axiom('9904861c-6f3f-49fc-8266-d69fe8de58cc', foundational, maori_customary_authority_extinguished).
narrative_ontology:cs_axiom_status(maori_customary_authority_extinguished, holdable).
narrative_ontology:cs_axiom_grounding('9904861c-6f3f-49fc-8266-d69fe8de58cc', maori_customary_authority_extinguished, conventional).
narrative_ontology:cs_reference_frame('9904861c-6f3f-49fc-8266-d69fe8de58cc', imperial_cession_framework_1840).
narrative_ontology:cs_drift_state('9904861c-6f3f-49fc-8266-d69fe8de58cc', contemporary_treaty_settlement_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9904861c-6f3f-49fc-8266-d69fe8de58cc', '2026-08-27T14:30:00Z').
narrative_ontology:cs_kernel_id(treaty_authority_cession__crown_cession_reading, treaty_authority_cession).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(treaty_authority_cession__crown_cession_reading, crown_government).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__crown_cession_reading, settler_land_companies).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__crown_cession_reading, colonial_parliament).
narrative_ontology:constraint_victim(treaty_authority_cession__crown_cession_reading, maori_hapu_iwi).
narrative_ontology:constraint_victim(treaty_authority_cession__crown_cession_reading, maori_landholders).
narrative_ontology:constraint_vindicates(treaty_authority_cession__crown_cession_reading, crown_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(treaty_authority_cession__crown_cession_reading, legal_cession_completeness).
narrative_ontology:constraint_vindicates(treaty_authority_cession__crown_cession_reading, english_text_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Asserts full legislative and territorial sovereignty over New Zealand based on the English treaty text. Administers land purchase, legislation, and governance structures that treat Māori authority as extinguished or subordinate. Collects the political and economic gains of uncontrolled land alienation and legislative supremacy.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, crown_government, agenda_setter,
    institutional, generational, arbitrage, national).

% Enacts legislation (Native Land Acts, Suppression of Rebellion Act, etc.) that operationalizes Crown sovereignty, converts customary title to Crown grants, and enables settler acquisition. Its legislative programme depends on the cession reading being settled law.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, colonial_parliament, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(treaty_authority_cession__crown_cession_reading, colonial_parliament, agenda_setter).

% Purchase Māori land at Crown-determined prices under Crown pre-emption (and later outside it), on-sell to settlers at profit. Their business model requires the legal certainty that Crown title is absolute and Māori customary title is extinguished — the cession reading provides that certainty.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, settler_land_companies, beneficiary,
    organized, biographical, mobile, national).

% Lose effective control of land, resources, and governance through Crown legislation, court rulings, and military enforcement. Their chiefs signed the Māori text understanding kāwanatanga as limited governance; the English-text cession reading is imposed retrospectively. Exit from the constraint means abandoning ancestral lands and political identity — identity_locked because authority, territory, and collective self-concept are fused.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, maori_hapu_iwi, payer,
    organized, generational, identity_locked, national).

% Individual or whānau landholders facing Native Land Court conversion, partition, and alienation. Can sometimes retain fragments through legal maneuver or non-compliance, but the structural direction of the constraint is toward loss. Exit is constrained: selling out means losing turangawaewae; resisting means cost and risk with low probability of structural change.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, maori_landholders, payer,
    moderate, biographical, constrained, local).

% Established 1975 to inquire into Crown breaches. Produces reports that document the divergence between texts and the extraction history, but lacks binding remedial power. Its analytical seat sees the full structure: the cession reading as an enforced interpretation that enabled land transfer.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, waitangi_tribunal, observer,
    institutional, generational, analytical, national).

% Apply contra proferentem, Vienna Convention principles, and indigenous rights norms to the treaty. Their analyses consistently find the English-text cession reading legally indefensible as the sole interpretation, but their conclusions carry no domestic enforcement weight.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, international_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a single recognized sovereign authority for British settlement, land acquisition, and governance — replacing the unstable pre-treaty situation of competing Māori authorities and unregulated European presence.
% TRANSFER_FUNCTION: Moves land, legislative authority, and political sovereignty from Māori collectives (hapū/iwi) to the Crown and its settler beneficiaries. The transfer is effected through legislation, courts, and force, justified by the cession reading.
% ABSENT_VOICES: The rangatira who signed the Māori text in 1840 — they are dead, but their understanding (kāwanatanga as limited governance, tino rangatiratanga retained) is the excluded counter-reading. Their descendants' voices were excluded from constitutional interpretation until the Waitangi Tribunal era (1975+).
% DISAPPEARANCE_RATIONALE: If the cession reading vanished overnight, the legal basis for Crown sovereignty over Māori land and authority would collapse. The Native Land Acts, the pre-emption system, and 150 years of legislative override would lose their founding justification. Land titles derived from Crown grants would face fundamental challenge. The constitutional order would reorganize around tino rangatiratanga and partnership.
% FOUNDING_PROBLEM: British authorities needed a legal instrument to legitimize settlement and governance in New Zealand without acknowledging Māori sovereignty as a barrier. The pre-treaty situation (1830s) had no recognized British authority, rampant land speculation, and inter-hapū conflict involving Europeans — the Colonial Office wanted a cession treaty to establish Crown title and control.
% FOUNDING_PROBLEM_CORROBORATION: The Colonial Office's own instructions to Hobson (1839-1840) and the subsequent treaty negotiation record, attested by historians outside the Crown's beneficiary line (e.g., Claudia Orange, Anne Salmond, Mason Durie). The founding problem — establishing a British colony — is achieved; the arrangement persists as extraction.
narrative_ontology:disappearance_verdict(treaty_authority_cession__crown_cession_reading, world_rearranges).
narrative_ontology:founding_problem_status(treaty_authority_cession__crown_cession_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(treaty_authority_cession__crown_cession_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(treaty_authority_cession__crown_cession_reading, 'none', 1).
narrative_ontology:epsilon_provenance(treaty_authority_cession__crown_cession_reading, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness 0.82 at interval end: the constraint transferred ~95% of land and full legislative authority to Crown/settlers. Suppression 0.78: enforcement required wars, legislation, courts, and police — not voluntary compliance. Theater 0.45: the 'protection' and 'benefits of British subjecthood' narrative is real but shrinking relative to the extraction machinery. Accessibility collapse 0.72: once the cession reading is accepted as settled law, alternatives (partnership, retained sovereignty) are legally foreclosed. Resistance 0.68: sustained Māori resistance (petitions, Kotahitanga, Land March, Bastion Point, contemporary claims) meets the constraint but has not displaced it.
 *
 * PERSPECTIVAL GAP:
 *   From the Crown seat, the constraint appears as legitimate coordination (rope-like): a treaty was signed, law followed, order established. From Māori seats, the same structure is experienced as enforced extraction (snare): the signed text said something else, and the English reading was imposed by power. The engine computes this divergence from the structural data — the declared roles, exit_options, and power atoms encode the asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Crown government and colonial parliament are agenda_setters (d ~0.05-0.15): they write and enforce the rules, collect the sovereignty gains. Settler land companies are beneficiaries (d ~0.2): they gain land wealth without administering the constraint. Māori hapū/iwi are payers (d ~0.9): identity_locked exit means they bear extraction with no structural escape — authority, land, and identity are fused. Māori landholders are payers (d ~0.75): constrained exit via legal system but structural direction is loss. Waitangi Tribunal and international scholars are observers (d ~0.5): analytical seats with no structural stake.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (establishing a British colony) is dead — achieved by 1860s. The arrangement persists as extraction: land transfer continued long after governance was settled, and the cession reading now functions to legitimate ongoing Crown legislative supremacy over Māori affairs. The coordination function was transient; the extraction function is structural. This is a false summit candidate if anyone claims the cesis reading is 'just what the treaty says' (natural law) — it is a constructed interpretation that benefits identifiable agents.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_authority_contestation,
    'Which treaty text (English or Māori) carries legal authority, and by what interpretive principle?',
    'Constitutional convention, judicial ruling, or Treaty settlement legislation adopting a single authoritative text or a harmonized reading. Contra proferentem favors the Māori text; Crown practice favors English.',
    'If Māori text authority is accepted, the cession reading collapses — kāwanatanga is limited governance, tino rangatiratanga is retained. The constraint reclassifies from snare to tangled_rope (coordination with ongoing negotiation) or scaffold (transitional).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_authority_contestation, conceptual, 'The core interpretive contest: English text sovereignty vs. Māori text partnership.').

omega_variable(
    cession_vs_protection_structure,
    'Does the English text Article 1 actually cede sovereignty, or does Article 2''s ''guarantee'' of tino rangatiratanga qualify Article 1?',
    'Integrated treaty interpretation (Vienna Convention Art 31): read the treaty as a whole, in context, with purpose. The Chiefs'' understanding (via Māori text) is relevant context.',
    'If Article 2 qualifies Article 1, the cession is partial — Crown gets kāwanatanga (governance), Māori retain tino rangatiratanga (authority). The constraint''s extraction drops; classification shifts toward tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cession_vs_protection_structure, conceptual, 'Whether the English text itself contains the limitation the Māori text makes explicit.').

omega_variable(
    identity_lock_mechanism_maori,
    'Is Māori identity_locked exit driven by professional/relational/ideological/institutional identity fusion, and what would break it?',
    'Trace the specific fusion: turangawaewae (ancestral land) as constitutive of hapū/iwi identity; whakapapa linking people to land; the Crown''s own recognition of Māori as Treaty partners. Breakage would require constitutional recognition that decouples identity from Crown-defined status.',
    'If identity_locked is structural (not chosen), the constraint''s effective extraction on Māori is amplified — they cannot exit without self-loss. If exit becomes mobile (e.g., through constitutional transformation), χ drops for the payer seat.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(identity_lock_mechanism_maori, empirical, 'The mechanism binding Māori to the constraint despite its extractiveness.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(treaty_authority_cession__crown_cession_reading, 1840, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(treaty_cession_crown_tr_t1840, treaty_authority_cession__crown_cession_reading, theater_ratio, 1840, 0.15).
narrative_ontology:measurement(treaty_cession_crown_tr_t1860, treaty_authority_cession__crown_cession_reading, theater_ratio, 1860, 0.25).
narrative_ontology:measurement(treaty_cession_crown_tr_t1880, treaty_authority_cession__crown_cession_reading, theater_ratio, 1880, 0.35).
narrative_ontology:measurement(treaty_cession_crown_tr_t1900, treaty_authority_cession__crown_cession_reading, theater_ratio, 1900, 0.4).
narrative_ontology:measurement(treaty_cession_crown_tr_t1975, treaty_authority_cession__crown_cession_reading, theater_ratio, 1975, 0.42).
narrative_ontology:measurement(treaty_cession_crown_tr_t2025, treaty_authority_cession__crown_cession_reading, theater_ratio, 2025, 0.45).

% Extraction over time
narrative_ontology:measurement(treaty_cession_crown_be_t1840, treaty_authority_cession__crown_cession_reading, base_extractiveness, 1840, 0.35).
narrative_ontology:measurement(treaty_cession_crown_be_t1860, treaty_authority_cession__crown_cession_reading, base_extractiveness, 1860, 0.55).
narrative_ontology:measurement(treaty_cession_crown_be_t1880, treaty_authority_cession__crown_cession_reading, base_extractiveness, 1880, 0.72).
narrative_ontology:measurement(treaty_cession_crown_be_t1900, treaty_authority_cession__crown_cession_reading, base_extractiveness, 1900, 0.8).
narrative_ontology:measurement(treaty_cession_crown_be_t1975, treaty_authority_cession__crown_cession_reading, base_extractiveness, 1975, 0.75).
narrative_ontology:measurement(treaty_cession_crown_be_t2025, treaty_authority_cession__crown_cession_reading, base_extractiveness, 2025, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(treaty_cession_crown_su_t1840, treaty_authority_cession__crown_cession_reading, suppression_requirement, 1840, 0.4).
narrative_ontology:measurement(treaty_cession_crown_su_t1860, treaty_authority_cession__crown_cession_reading, suppression_requirement, 1860, 0.65).
narrative_ontology:measurement(treaty_cession_crown_su_t1880, treaty_authority_cession__crown_cession_reading, suppression_requirement, 1880, 0.75).
narrative_ontology:measurement(treaty_cession_crown_su_t1900, treaty_authority_cession__crown_cession_reading, suppression_requirement, 1900, 0.78).
narrative_ontology:measurement(treaty_cession_crown_su_t1975, treaty_authority_cession__crown_cession_reading, suppression_requirement, 1975, 0.6).
narrative_ontology:measurement(treaty_cession_crown_su_t2025, treaty_authority_cession__crown_cession_reading, suppression_requirement, 2025, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(treaty_authority_cession__crown_cession_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(treaty_authority_cession__crown_cession_reading, 0.1).
narrative_ontology:affects_constraint(treaty_authority_cession__crown_cession_reading, treaty_authority_cession__rangatiratanga_retention_reading).
narrative_ontology:affects_constraint(treaty_authority_cession__crown_cession_reading, treaty_authority_cession__retrospective_snare_exposure).
narrative_ontology:affects_constraint(treaty_authority_cession__crown_cession_reading, native_land_acts).
narrative_ontology:affects_constraint(treaty_authority_cession__crown_cession_reading, waitangi_tribunal_jurisdiction).

% DUAL FORMULATION NOTE:
% Part of the treaty_authority_cession constraint family (3 readings). This reading (crown_cession) asserts English-text sovereignty cession. The rangatiratanga_retention_reading asserts Māori-text partnership. The retrospective_snare_exposure reading treats the textual divergence itself as the extraction mechanism. ε differs widely: crown_cession ε=0.82 (high extraction), rangatiratanga_retention ε~0.3 (coordination with residual extraction), snare_exposure ε~0.9 (the divergence IS the extraction). Network edges: crown_cession → rangatiratanga_retention (forecloses); crown_cession → snare_exposure (influences — provides the enforced interpretation the snare exposes).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(treaty_authority_cession__crown_cession_reading, organized, 0.85).
constraint_indexing:directionality_override(treaty_authority_cession__crown_cession_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
