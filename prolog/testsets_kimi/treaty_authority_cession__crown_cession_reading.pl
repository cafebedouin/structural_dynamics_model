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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
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
 *   human_readable: Crown Cession Reading of the Treaty of Waitangi
 *   domain: constitutional/colonial/indigenous_rights
 *
 * SUMMARY:
 *   The Crown cession reading of the Treaty of Waitangi asserts that the
 *   English text is authoritative, that MÄori signatories ceded full
 *   sovereignty (kÄwanatanga understood as total governing authority), and
 *   that the Treaty therefore completed a legal transfer of absolute
 *   legislative and land authority to the British Crown. This reading
 *   constructed a constitutional wall enclosing New Zealand territory under
 *   Crown supremacy, rendering MÄori customary authority extinguished or
 *   subordinate and legitimizing subsequent land alienation. It is one
 *   reading of the treaty_authority_cession kernel, structurally
 *   contradicting the rangatiratanga retention reading and coexisting with
 *   retrospective snare exposure analyses.
 *
 * KEY AGENTS:
 *   - crown_executive: agenda_setter (institutional/arbitrage) â asserts and maintains the cession doctrine
 *   - settler_citizens: beneficiary (organized/mobile) â receive secure tenure and representative government under Crown law
 *   - maori_iwi_hapu: payer (powerless/trapped) â sovereignty and land authority transferred away by the legal interpretation
 *   - crown_judiciary: agenda_setter (institutional/constrained) â interprets and enforces the cession reading in land and sovereignty cases
 *   - waitangi_tribunal: observer (institutional/analytical) â documents textual divergence and Treaty breaches from an analytical seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(treaty_authority_cession__crown_cession_reading, 0.62).
domain_priors:suppression_score(treaty_authority_cession__crown_cession_reading, 0.75).
domain_priors:theater_ratio(treaty_authority_cession__crown_cession_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(treaty_authority_cession__crown_cession_reading, tangled_rope).
narrative_ontology:human_readable(treaty_authority_cession__crown_cession_reading, "Crown Cession Reading of the Treaty of Waitangi").
narrative_ontology:topic_domain(treaty_authority_cession__crown_cession_reading, "constitutional/colonial/indigenous_rights").

domain_priors:requires_active_enforcement(treaty_authority_cession__crown_cession_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(treaty_authority_cession__crown_cession_reading, '36af078d-02ce-4724-994c-10cefaddee81').
narrative_ontology:cs_kernel_codification('36af078d-02ce-4724-994c-10cefaddee81', fixed_text).
narrative_ontology:cs_authority_grounding('36af078d-02ce-4724-994c-10cefaddee81', lineage).
narrative_ontology:cs_interpretation_layer_present('36af078d-02ce-4724-994c-10cefaddee81').
narrative_ontology:cs_reading_relation('36af078d-02ce-4724-994c-10cefaddee81', treaty_authority_cession__rangatiratanga_retention_reading, forecloses).
narrative_ontology:cs_reading_relation('36af078d-02ce-4724-994c-10cefaddee81', treaty_authority_cession__retrospective_snare_exposure, coexists_with).
narrative_ontology:cs_axiom('36af078d-02ce-4724-994c-10cefaddee81', foundational, english_text_controls_interpretation).
narrative_ontology:cs_axiom_status(english_text_controls_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('36af078d-02ce-4724-994c-10cefaddee81', english_text_controls_interpretation, conventional).
narrative_ontology:cs_axiom('36af078d-02ce-4724-994c-10cefaddee81', foundational, sovereignty_completely_ceded).
narrative_ontology:cs_axiom_status(sovereignty_completely_ceded, holdable).
narrative_ontology:cs_axiom_grounding('36af078d-02ce-4724-994c-10cefaddee81', sovereignty_completely_ceded, empirically_contingent).
narrative_ontology:cs_reference_frame('36af078d-02ce-4724-994c-10cefaddee81', crown_legal_supremacy_framework).
narrative_ontology:cs_drift_state('36af078d-02ce-4724-994c-10cefaddee81', post_waitangi_tribunal_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('36af078d-02ce-4724-994c-10cefaddee81', '').
narrative_ontology:cs_kernel_id(treaty_authority_cession__crown_cession_reading, treaty_authority_cession).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(treaty_authority_cession__crown_cession_reading, crown_executive).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__crown_cession_reading, settler_citizens).
narrative_ontology:constraint_victim(treaty_authority_cession__crown_cession_reading, maori_iwi_hapu).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Asserts and maintains that the Treaty of Waitangi effected a complete cession of MÄori sovereignty to the British Crown; governs New Zealand through Westminster-style legislative and executive institutions and defends the legal supremacy of Parliament over customary MÄori authority.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, crown_executive, agenda_setter,
    institutional, generational, arbitrage, national).

% Receive secure land tenure, access to representative governance, and property rights under Crown law; their political and economic standing depends on the Crown sovereignty framework that declared MÄori customary authority subordinate.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, settler_citizens, beneficiary,
    organized, biographical, mobile, national).

% Held customary authority over land and people prior to 1840; the Crown cession reading subjected them to Crown law, enabled Native Land Court individualization of title, and left their sovereignty claims without legal standing in colonial and later national courts. Exit from the constraint is structurally impossible because the reading declares their sovereignty already ceded.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, maori_iwi_hapu, payer,
    powerless, generational, trapped, national).

% Interprets the Treaty of Waitangi in cases testing Crown authority; from the 1840s onward, the judiciary treated the Treaty as effecting complete cession in matters of land, fisheries, and criminal jurisdiction over MÄori, enforcing the legal supremacy of Crown statute over customary law.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, crown_judiciary, agenda_setter,
    institutional, biographical, constrained, national).

% Established in 1975 to investigate Crown breaches of the Treaty; it documents the textual divergence between the English and MÄori texts and records MÄori understandings of kÄwanatanga, but its findings are recommendations only and do not bind the Crown's constitutional reading.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, waitangi_tribunal, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(treaty_authority_cession__crown_cession_reading, crown_executive).
narrative_ontology:fixing_cost_class(treaty_authority_cession__crown_cession_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single sovereign legal authority over New Zealand territory, coordinating settlers, trade, and land tenure under Crown law and Westminster parliamentary institutions.
% TRANSFER_FUNCTION: Moves sovereignty, land authority, and legislative supremacy from MÄori iwi and hapÅ« to the British Crown and settler institutions; transfers land from MÄori collective tenure to Crown and individual settler ownership.
% ABSENT_VOICES: MÄori signatories to the MÄori-language text, who understood kÄwanatanga as limited governance while retaining tino rangatiratanga, were excluded from the English-text interpretive framework; their descendants' sovereignty claims were systematically excluded from Crown courts until the late twentieth century.
% DISAPPEARANCE_RATIONALE: If the Crown cession reading vanished overnight, Crown sovereignty would lack its primary constitutional foundation in New Zealand; land titles derived from Crown pre-emption would be destabilized, and the legal basis for parliamentary sovereignty over MÄori customary authority would fracture â the constitutional order would rearrange around either the rangatiratanga reading or a new settlement.
% FOUNDING_PROBLEM: British colonial expansion required a recognized legal basis for asserting sovereignty over New Zealand territory and managing settler land acquisition without continual violent conflict.
% FOUNDING_PROBLEM_CORROBORATION: Waitangi Tribunal historians and MÄori legal scholars attest the founding problem of colonial acquisition is solved and the constraint persists as constitutional inertia; Crown legal advisers assert ongoing necessity, but independent constitutional historians outside the Crown beneficiary set document the functional obsolescence of the pure cession reading.
narrative_ontology:disappearance_verdict(treaty_authority_cession__crown_cession_reading, world_rearranges).
narrative_ontology:founding_problem_status(treaty_authority_cession__crown_cession_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(treaty_authority_cession__crown_cession_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(treaty_authority_cession__crown_cession_reading, 'none', 1).
narrative_ontology:epsilon_provenance(treaty_authority_cession__crown_cession_reading, 0.62, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.62) reflects the substantial transfer of sovereignty and land authority from MÄori to the Crown; it is moderated from a higher peak by late-century Treaty settlement processes but remains structurally high because the cession reading still underpins Crown land title and parliamentary sovereignty. Suppression (0.75) is high because the reading's persistence required excluding the MÄori text, suppressing rangatiratanga claims in Crown courts, and enforcing Crown law over customary authority. Theater ratio (0.68) is high and rising: the cession reading is increasingly performative as historical and linguistic evidence against full cession accumulates, yet it is maintained in legal and political ritual. Accessibility collapse (0.80) is high because once the cession reading was installed in colonial courts and statute, MÄori alternatives for asserting sovereignty collapsed within the legal system. Resistance (0.55) reflects persistent MÄori political and legal resistance from the nineteenth century to the present, including wars, protests, and Tribunal claims.
 *
 * PERSPECTIVAL GAP:
 *   The Crown executive and judiciary experience this constraint as constitutional foundation and legal order â a necessary framework for governance and property rights. MÄori iwi and hapÅ« experience the identical constraint as the legal mechanism by which their sovereignty was voided and their land alienated. The engine computes this divergence from the structural data: beneficiaries with arbitrage/mobile exits versus victims with trapped exits.
 *
 * DIRECTIONALITY LOGIC:
 *   crown_executive and settler_citizens are declared beneficiaries with generational/biographical horizons and mobile/arbitrage exit options, placing them near the full-beneficiary end (low d). maori_iwi_hapu are declared victims with generational time horizon and trapped exit, placing them near the full-target end (high d). crown_judiciary sits between: agenda-setter enforcing the constraint but constrained by precedent and institutional identity, producing a moderate d. waitangi_tribunal holds an analytical exit and observer role, insulating it from extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   Without the tangled_rope classification, the Crown cession reading could be misread as rope (pure coordination of colonial governance) by ignoring the extinguishment of MÄori authority, or as snare (pure extraction) by ignoring that it did coordinate a functioning legal order for settlers. The tangled_rope gate forces the dual structure: genuine coordination function for the settler state alongside asymmetric extraction from indigenous authority. The high theater ratio signals that the coordination story is thinning as the reading becomes more performative, but it has not yet decayed to piton because Crown sovereignty still materially benefits concentrated parties.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_sovereignty_ambiguity,
    'Did MÄori signatories in 1840 comprehend kÄwanatanga as full sovereignty, or as limited governance subject to retained tino rangatiratanga?',
    'Historical-linguistic analysis of 1840 MÄori political vocabulary, missionary usage records, and contemporary testimony from signatories and witnesses.',
    'If kÄwanatanga was understood as limited governance, the Crown cession reading rests on a mistranslation and extraction increases; if full sovereignty was comprehended, the coordination story gains historical support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_sovereignty_ambiguity, empirical, 'Whether the semantic content of the MÄori text supports full sovereignty cession.').

omega_variable(
    cession_reading_kernel_status,
    'This constraint is the Crown cession reading of the treaty_authority_cession kernel. Would classification change if the MÄori text (rangatiratanga retention) were taken as the authoritative kernel?',
    'Comparative classification of the sibling reading under identical structural conditions, swapping beneficiary and victim sets.',
    'If the rangatiratanga reading is taken as kernel, the Crown cession reading may reclassify as snare (pure extraction via textual imposition) rather than tangled_rope, because the coordination function would be exposed as operating only for one party.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cession_reading_kernel_status, conceptual, 'Kernel-level ambiguity: which reading''s axioms govern the classification of this constraint.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the persistence of the cession reading maintained by structural force (legal and bureaucratic enforcement) or by internalized acceptance within the settler constitutional culture?',
    'Measure resistance and enforcement costs across periods of crisis: if suppression drops and the reading persists, internalization is dominant; if it requires active reinforcement during challenges, structural suppression is dominant.',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest and removal would require cultural rather than legal intervention; if structural, legal reform may suffice to shift the reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized suppression mechanism for the cession reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(treaty_authority_cession__crown_cession_reading, 0, 180).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trea_tr_t0, treaty_authority_cession__crown_cession_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(trea_tr_t30, treaty_authority_cession__crown_cession_reading, theater_ratio, 30, 0.25).
narrative_ontology:measurement(trea_tr_t60, treaty_authority_cession__crown_cession_reading, theater_ratio, 60, 0.35).
narrative_ontology:measurement(trea_tr_t100, treaty_authority_cession__crown_cession_reading, theater_ratio, 100, 0.45).
narrative_ontology:measurement(trea_tr_t135, treaty_authority_cession__crown_cession_reading, theater_ratio, 135, 0.55).
narrative_ontology:measurement(trea_tr_t180, treaty_authority_cession__crown_cession_reading, theater_ratio, 180, 0.68).

% Extraction over time
narrative_ontology:measurement(trea_be_t0, treaty_authority_cession__crown_cession_reading, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(trea_be_t30, treaty_authority_cession__crown_cession_reading, base_extractiveness, 30, 0.85).
narrative_ontology:measurement(trea_be_t60, treaty_authority_cession__crown_cession_reading, base_extractiveness, 60, 0.9).
narrative_ontology:measurement(trea_be_t100, treaty_authority_cession__crown_cession_reading, base_extractiveness, 100, 0.82).
narrative_ontology:measurement(trea_be_t135, treaty_authority_cession__crown_cession_reading, base_extractiveness, 135, 0.7).
narrative_ontology:measurement(trea_be_t180, treaty_authority_cession__crown_cession_reading, base_extractiveness, 180, 0.62).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(treaty_authority_cession__crown_cession_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(treaty_authority_cession__crown_cession_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(treaty_authority_cession__crown_cession_reading, rangatiratanga_retention_reading).
narrative_ontology:affects_constraint(treaty_authority_cession__crown_cession_reading, retrospective_snare_exposure).

% DUAL FORMULATION NOTE:
% The treaty_authority_cession kernel decomposes into three structurally distinct constraints: the Crown cession reading (English text authoritative, full sovereignty transferred), the rangatiratanga retention reading (MÄori text authoritative, sovereignty retained), and the retrospective snare exposure (textual divergence as extraction mechanism). Each has distinct epsilon, beneficiary/victim structure, and classification. This story instantiates the Crown cession reading only.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
