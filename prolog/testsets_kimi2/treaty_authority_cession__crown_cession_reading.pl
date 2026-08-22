% ============================================================================
% CONSTRAINT STORY: treaty_authority_cession__crown_cession_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-14
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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: treaty_authority_cession__crown_cession_reading
 *   human_readable: Crown Cession Reading of Treaty Authority
 *   domain: constitutional law / indigenous rights / colonial history
 *
 * SUMMARY:
 *   This constraint instantiates the Crown cession reading of the treaty
 *   authority cession kernel: the English text of the Treaty of Waitangi
 *   controls, kÄwanatanga is read as full sovereignty, and the treaty
 *   completes a legal cession of authority to the Crown. Structurally, this
 *   reading constructs a wall enclosing land and legislative authority under
 *   the Crown, extinguishing or subordinating MÄori customary authority and
 *   legitimizing land alienation. It is authored as one reading among
 *   siblings (rangatiratanga_retention_reading, biculturalism_reading,
 *   retrospective_snare_exposure) and carries the kernel's committer
 *   structure in cs_structure.
 *
 * KEY AGENTS:
 *   - crown_government: Primary agenda-setter (institutional/arbitrage) â claims sovereignty and enforces the reading
 *   - settler_population: Primary beneficiary (organized/constrained) â gains land titles and legal certainty
 *   - maori_rangatira: Primary payer (organized/trapped) â customary authority subordinated
 *   - maori_communities: Secondary payer (powerless/trapped) â land and resources alienated
 *   - maori_text_drafters: Excluded voice (moderate/trapped) â translation overridden
 *   - waitangi_tribunal: Analytical observer (institutional/analytical) â assesses historical validity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(treaty_authority_cession__crown_cession_reading, 0.82).
domain_priors:suppression_score(treaty_authority_cession__crown_cession_reading, 0.88).
domain_priors:theater_ratio(treaty_authority_cession__crown_cession_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(treaty_authority_cession__crown_cession_reading, tangled_rope).
narrative_ontology:human_readable(treaty_authority_cession__crown_cession_reading, "Crown Cession Reading of Treaty Authority").
narrative_ontology:topic_domain(treaty_authority_cession__crown_cession_reading, "constitutional law / indigenous rights / colonial history").

domain_priors:requires_active_enforcement(treaty_authority_cession__crown_cession_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(treaty_authority_cession__crown_cession_reading, '6a81f348-a164-439b-832c-f03f5ebcde8f').
narrative_ontology:cs_kernel_codification('6a81f348-a164-439b-832c-f03f5ebcde8f', fixed_text).
narrative_ontology:cs_authority_grounding('6a81f348-a164-439b-832c-f03f5ebcde8f', lineage).
narrative_ontology:cs_interpretation_layer_present('6a81f348-a164-439b-832c-f03f5ebcde8f').
narrative_ontology:cs_reading_relation('6a81f348-a164-439b-832c-f03f5ebcde8f', treaty_authority_cession__rangatiratanga_retention_reading, forecloses).
narrative_ontology:cs_reading_relation('6a81f348-a164-439b-832c-f03f5ebcde8f', treaty_authority_cession__biculturalism_reading, influences).
narrative_ontology:cs_reading_relation('6a81f348-a164-439b-832c-f03f5ebcde8f', treaty_authority_cession__retrospective_snare_exposure, coexists_with).
narrative_ontology:cs_axiom('6a81f348-a164-439b-832c-f03f5ebcde8f', foundational, english_text_supremacy).
narrative_ontology:cs_axiom_status(english_text_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('6a81f348-a164-439b-832c-f03f5ebcde8f', english_text_supremacy, conventional).
narrative_ontology:cs_axiom('6a81f348-a164-439b-832c-f03f5ebcde8f', foundational, kawanatanga_full_sovereignty_equivalence).
narrative_ontology:cs_axiom_status(kawanatanga_full_sovereignty_equivalence, holdable).
narrative_ontology:cs_axiom_grounding('6a81f348-a164-439b-832c-f03f5ebcde8f', kawanatanga_full_sovereignty_equivalence, empirically_contingent).
narrative_ontology:cs_reference_frame('6a81f348-a164-439b-832c-f03f5ebcde8f', crown_sovereignty_supremacy).
narrative_ontology:cs_drift_state('6a81f348-a164-439b-832c-f03f5ebcde8f', contemporary_constitutional_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6a81f348-a164-439b-832c-f03f5ebcde8f', '').
narrative_ontology:cs_kernel_id(treaty_authority_cession__crown_cession_reading, treaty_authority_cession).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(treaty_authority_cession__crown_cession_reading, crown_government).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__crown_cession_reading, settler_population).
narrative_ontology:constraint_victim(treaty_authority_cession__crown_cession_reading, maori_rangatira).
narrative_ontology:constraint_victim(treaty_authority_cession__crown_cession_reading, maori_communities).
narrative_ontology:constraint_vindicates(treaty_authority_cession__crown_cession_reading, crown_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(treaty_authority_cession__crown_cession_reading, english_legal_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claims exclusive sovereignty and legislative supremacy derived from the English treaty text; administers land alienation, courts, and enforcement apparatus.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, crown_government, agenda_setter,
    institutional, generational, arbitrage, national).

% Receives secure land titles and representative institutions under the Crown's legal framework; depends on the Crown's reading to validate their property claims against customary tenure.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, settler_population, beneficiary,
    organized, biographical, constrained, national).

% Held customary authority over land and people; under the Crown cession reading their signatory understanding is overridden by the English text, and their political authority is rendered subordinate or legally void.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, maori_rangatira, payer,
    organized, generational, trapped, national).

% Lose land and customary resource access as the Crown's reading legitimizes large-scale alienation; excluded from English-law property rights unless individually granted.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, maori_communities, payer,
    powerless, generational, trapped, regional).

% Henry Williams and missionaries who drafted the MÄori text; their translation choices and assurances to chiefs are overridden by the English-text supremacy claim, and their voices are absent from the authoritative interpretive framework.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, maori_text_drafters, excluded,
    moderate, biographical, trapped, local).

% Investigates treaty breaches and produces findings on MÄori text meaning; lacks power to override Crown sovereignty but provides external analytical assessment of the cession reading's historical validity.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, waitangi_tribunal, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(treaty_authority_cession__crown_cession_reading, diffuse).
narrative_ontology:fixing_cost_class(treaty_authority_cession__crown_cession_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single unified legal authority and land-title system over the territory, eliminating competing customary claims and providing governance certainty for the colonial project.
% TRANSFER_FUNCTION: Transfers sovereignty and legislative authority from MÄori customary structures to the Crown; transfers land from MÄori communal tenure to Crown-granted or alienated individual title, validated by the English text.
% ABSENT_VOICES: MÄori signatories who understood kÄwanatanga as limited governance; MÄori text drafters whose translation was overridden; contemporary MÄori political movements asserting tino rangatiratanga as unextinguished.
% DISAPPEARANCE_RATIONALE: If the Crown cession reading vanished, the legal foundation for Crown sovereignty, parliamentary supremacy, and the bulk of derived land titles would be destabilized; the constitutional order would require renegotiation or replacement.
% FOUNDING_PROBLEM: Colonial administration required legal certainty over sovereignty and land tenure to enable mass settlement and govern territory acquired through treaty.
% FOUNDING_PROBLEM_CORROBORATION: British Colonial Office correspondence corroborates the 19th-century settlement imperative. Waitangi Tribunal historians and MÄori scholars corroborate that the problem was addressed through unilateral imposition rather than genuine cession; no impartial contemporary party attests the founding problem as uncontestedly live.
narrative_ontology:disappearance_verdict(treaty_authority_cession__crown_cession_reading, world_rearranges).
narrative_ontology:founding_problem_status(treaty_authority_cession__crown_cession_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(treaty_authority_cession__crown_cession_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(treaty_authority_cession__crown_cession_reading, 'none', 1).
narrative_ontology:epsilon_provenance(treaty_authority_cession__crown_cession_reading, 0.82, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.82) is high because the reading enabled massive transfer of land and sovereignty with minimal reciprocity; suppression (0.88) is higher because persistence required military confiscation, the Native Land Court, and statutory bars on customary title. Accessibility_collapse (0.85) reflects near-total judicial and statutory extinguishment of MÄori legal alternatives by the mid-20th century. Resistance (0.75) captures the Waikato wars, Kingitanga, and modern treaty claims. Theater_ratio (0.30) is moderate-low: the legal machinery was functionally effective at extraction, though modern partnership rhetoric adds performative overlay.
 *
 * PERSPECTIVAL GAP:
 *   From the Crown seat the reading is legitimate constitutional lineage and necessary order; from MÄori payer seats it is unilateral imposition using a mistranslated text. The engine computes this divergence from structural data: Crown has institutional power, arbitrage exit, and beneficiary role (d near 0.0); MÄori communities have powerless status, trapped exit, and payer role (d near 1.0). Settlers sit between, constrained by their dependence on Crown title but benefiting from the transfer.
 *
 * DIRECTIONALITY LOGIC:
 *   The Crown and settler population are structural beneficiaries: the Crown gains sovereignty and legislative supremacy, settlers gain secure property. Directionality is near the beneficiary end for the Crown (institutional/arbitrage) and moderate-low for settlers (organized/constrained). MÄori rangatira and communities are structural targets: they bear the loss of authority and land, with trapped exit options amplifying effective extraction. The Waitangi Tribunal observer seat is analytical with no directional extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â colonial legal order for settlement â is contested as to whether it persists or has been superseded by a different constitutional order. The Crown cession reading persists despite substantial authority erosion because dismantling it would rearrange the land-title system and sovereignty framework; the cost of genuine restoration exceeds what the Crown has been willing to bear, suggesting mandatrophy risk, though the reading is actively maintained rather than purely inertial.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    maori_text_consent_gap,
    'Does the Crown cession reading validly derive sovereignty from a treaty text that MÄori signatories did not read or assent to in English?',
    'Historical-linguistic analysis of the Williams translation and testimony from 1840; legal recognition of contra proferentem in treaty interpretation.',
    'If the MÄori text did not convey sovereignty cession, the Crown cession reading rests on a consent gap that transforms the constraint from a cession mechanism into a unilateral imposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(maori_text_consent_gap, empirical, 'Whether MÄori signatories consented to English-text sovereignty.').

omega_variable(
    coordination_vs_extraction_ratio,
    'Can the colonial legal order''s coordination function for settlers be separated from its extraction function directed at MÄori?',
    'Comparative analysis of jurisdictions where parallel MÄori customary title recognition was maintained alongside Crown sovereignty.',
    'If separable, the constraint is a tangled rope with distinct coordination and extraction strands; if inseparable, the coordination is the extraction mechanism itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_ratio, conceptual, 'Separability of coordination and extraction in the Crown cession reading.').

omega_variable(
    kernel_reading_operational_dominance,
    'Which reading of the treaty authority cession kernel does the current legal system structurally enforce?',
    'Tracking which reading controls land-title determination, legislative supremacy claims, and constitutional recognition.',
    'If the Crown cession reading remains operationally dominant despite rhetorical partnership, extraction is ongoing; if superseded by partnership readings, the constraint becomes historical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_operational_dominance, preference, 'Operational dominance of the Crown cession reading versus sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(treaty_authority_cession__crown_cession_reading, 0, 184).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(treaty_auth_crown_tr_t0, treaty_authority_cession__crown_cession_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(treaty_auth_crown_tr_t20, treaty_authority_cession__crown_cession_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(treaty_auth_crown_tr_t50, treaty_authority_cession__crown_cession_reading, theater_ratio, 50, 0.2).
narrative_ontology:measurement(treaty_auth_crown_tr_t90, treaty_authority_cession__crown_cession_reading, theater_ratio, 90, 0.25).
narrative_ontology:measurement(treaty_auth_crown_tr_t130, treaty_authority_cession__crown_cession_reading, theater_ratio, 130, 0.35).
narrative_ontology:measurement(treaty_auth_crown_tr_t184, treaty_authority_cession__crown_cession_reading, theater_ratio, 184, 0.4).

% Extraction over time
narrative_ontology:measurement(treaty_auth_crown_be_t0, treaty_authority_cession__crown_cession_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(treaty_auth_crown_be_t20, treaty_authority_cession__crown_cession_reading, base_extractiveness, 20, 0.82).
narrative_ontology:measurement(treaty_auth_crown_be_t50, treaty_authority_cession__crown_cession_reading, base_extractiveness, 50, 0.9).
narrative_ontology:measurement(treaty_auth_crown_be_t90, treaty_authority_cession__crown_cession_reading, base_extractiveness, 90, 0.85).
narrative_ontology:measurement(treaty_auth_crown_be_t130, treaty_authority_cession__crown_cession_reading, base_extractiveness, 130, 0.78).
narrative_ontology:measurement(treaty_auth_crown_be_t184, treaty_authority_cession__crown_cession_reading, base_extractiveness, 184, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(treaty_auth_crown_su_t0, treaty_authority_cession__crown_cession_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(treaty_auth_crown_su_t20, treaty_authority_cession__crown_cession_reading, suppression_requirement, 20, 0.95).
narrative_ontology:measurement(treaty_auth_crown_su_t50, treaty_authority_cession__crown_cession_reading, suppression_requirement, 50, 0.88).
narrative_ontology:measurement(treaty_auth_crown_su_t90, treaty_authority_cession__crown_cession_reading, suppression_requirement, 90, 0.8).
narrative_ontology:measurement(treaty_auth_crown_su_t130, treaty_authority_cession__crown_cession_reading, suppression_requirement, 130, 0.65).
narrative_ontology:measurement(treaty_auth_crown_su_t184, treaty_authority_cession__crown_cession_reading, suppression_requirement, 184, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(treaty_authority_cession__crown_cession_reading, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
