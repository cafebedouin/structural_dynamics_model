% ============================================================================
% CONSTRAINT STORY: treaty_authority_cession__retrospective_snare_exposure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   constraint_id: treaty_authority_cession__retrospective_snare_exposure
 *   human_readable: Treaty Authority Cession: Retrospective Snare Exposure (Textual Divergence as Extraction)
 *   domain: constitutional/political/colonial
 *
 * SUMMARY:
 *   This constraint is the retrospective_snare_exposure reading of the
 *   treaty_authority_cession kernel. The kernel is the Treaty of Waitangi /
 *   Te Tiriti o Waitangi as a contested authority structure. Sibling readings
 *   include: crown_cession_reading (English text controls, valid sovereignty
 *   transfer) and rangatiratanga_retention_reading (MÄori text controls,
 *   partnership with retained authority). This reading does not treat the
 *   divergence as a translation error to be reconciled, but as the extraction
 *   mechanism itself: the chiefs signed a MÄori text that could not
 *   semanticize English sovereignty, and the Crown operated an English text
 *   that enabled legislative supremacy and land purchasing the chiefs never
 *   assented to. The extraction was covert at the time of operation and
 *   becomes visible only retrospectively through historical-linguistic
 *   analysis.
 *
 * KEY AGENTS:
 *   - Crown land apparatus (agenda_setter/beneficiary): constructs and enforces the English-text sovereignty claim
 *   - MÄori signatory chiefs (payer): signed the MÄori text and were subsequently subjected to confiscation and override
 *   - MÄori descendant communities (payer): bear intergenerational loss of land and authority through the persisting sovereignty doctrine
 *   - Non-signatory MÄori iwi (excluded): subjected to the sovereignty claim despite not being party to the signing
 *   - Postcolonial treaty analyst (observer): maps the extraction mechanism retrospectively
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(treaty_authority_cession__retrospective_snare_exposure, 0.85).
domain_priors:suppression_score(treaty_authority_cession__retrospective_snare_exposure, 0.78).
domain_priors:theater_ratio(treaty_authority_cession__retrospective_snare_exposure, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, extractiveness, 0.85).
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(treaty_authority_cession__retrospective_snare_exposure, snare).
narrative_ontology:human_readable(treaty_authority_cession__retrospective_snare_exposure, "Treaty Authority Cession: Retrospective Snare Exposure (Textual Divergence as Extraction)").
narrative_ontology:topic_domain(treaty_authority_cession__retrospective_snare_exposure, "constitutional/political/colonial").

domain_priors:requires_active_enforcement(treaty_authority_cession__retrospective_snare_exposure).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(treaty_authority_cession__retrospective_snare_exposure, '09e29073-9382-4ee6-abf4-1987b76907fa').
narrative_ontology:cs_kernel_codification('09e29073-9382-4ee6-abf4-1987b76907fa', fixed_text).
narrative_ontology:cs_authority_grounding('09e29073-9382-4ee6-abf4-1987b76907fa', extraction).
narrative_ontology:cs_interpretation_layer_present('09e29073-9382-4ee6-abf4-1987b76907fa').
narrative_ontology:cs_reading_relation('09e29073-9382-4ee6-abf4-1987b76907fa', treaty_authority_cession__crown_cession_reading, forecloses).
narrative_ontology:cs_reading_relation('09e29073-9382-4ee6-abf4-1987b76907fa', treaty_authority_cession__rangatiratanga_retention_reading, coexists_with).
narrative_ontology:cs_axiom('09e29073-9382-4ee6-abf4-1987b76907fa', foundational, textual_divergence_precludes_valid_cession).
narrative_ontology:cs_axiom_status(textual_divergence_precludes_valid_cession, holdable).
narrative_ontology:cs_axiom_grounding('09e29073-9382-4ee6-abf4-1987b76907fa', textual_divergence_precludes_valid_cession, empirically_contingent).
narrative_ontology:cs_axiom('09e29073-9382-4ee6-abf4-1987b76907fa', foundational, mistranslation_as_covert_extraction).
narrative_ontology:cs_axiom_status(mistranslation_as_covert_extraction, holdable).
narrative_ontology:cs_axiom_grounding('09e29073-9382-4ee6-abf4-1987b76907fa', mistranslation_as_covert_extraction, conventional).
narrative_ontology:cs_reference_frame('09e29073-9382-4ee6-abf4-1987b76907fa', english_sovereignty_cession_framework).
narrative_ontology:cs_drift_state('09e29073-9382-4ee6-abf4-1987b76907fa', post_waitangi_tribunal_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('09e29073-9382-4ee6-abf4-1987b76907fa', '').
narrative_ontology:cs_kernel_id(treaty_authority_cession__retrospective_snare_exposure, treaty_authority_cession).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(treaty_authority_cession__retrospective_snare_exposure, crown_land_apparatus).
narrative_ontology:constraint_victim(treaty_authority_cession__retrospective_snare_exposure, maori_signatory_chiefs).
narrative_ontology:constraint_victim(treaty_authority_cession__retrospective_snare_exposure, maori_descendant_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administered land purchases and legislative supremacy under the English Treaty text's sovereignty clause. Constructed and maintained the legal fiction that MÄori chiefs had ceded sovereignty, enabling systematic land transfer to the Crown and settler government while asserting unilateral authority to interpret and enforce the text.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, crown_land_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(treaty_authority_cession__retrospective_snare_exposure, crown_land_apparatus, beneficiary).

% Signed the MÄori text believing they retained tino rangatiratanga while conceding limited kÄwanatanga. Subsequently subjected to land confiscation and legislative override they had not agreed to in the text they signed. Exit was blocked by the Crown's unilateral assertion of English-text sovereignty and military enforcement.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, maori_signatory_chiefs, payer,
    organized, biographical, trapped, regional).

% Bear the intergenerational loss of land and authority transmitted through the Crown's English-text sovereignty claim. Their identity remains tied to whenua and rangatiratanga that the constraint structurally denies, making exit equivalent to abandoning collective selfhood.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, maori_descendant_communities, payer,
    powerless, generational, identity_locked, regional).

% Were not party to the Treaty signing but were subsequently subjected to the sovereignty claim and land purchasing anyway. Their exclusion from the text did not protect them from its extractive application across the territory.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, non_signatory_maori_iwi, excluded,
    moderate, generational, trapped, regional).

% Analytical seat that retrospectively examines the textual divergence and exposes the extraction mechanism. Does not bear or benefit from the constraint's operation but maps its structure across the historical interval.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, postcolonial_treaty_analyst, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(treaty_authority_cession__retrospective_snare_exposure, crown_land_apparatus).
narrative_ontology:fixing_cost_class(treaty_authority_cession__retrospective_snare_exposure, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Presented as coordinating peaceful inter-societal governance and land transaction protocols between British settlers and MÄori chiefs under a single agreed text.
% TRANSFER_FUNCTION: Moves land, territorial authority, and sovereign jurisdiction from MÄori signatories and their descendants to the Crown land-purchasing apparatus and settler legislative bodies, under the authority of an English sovereignty claim that the MÄori text did not convey.
% ABSENT_VOICES: MÄori who could read the English text and would have objected to the sovereignty cession were structurally absent from the negotiation table; non-signatory iwi and later MÄori legal scholars are excluded from the original frame but contest it retrospectively.
% DISAPPEARANCE_RATIONALE: If the textual divergence and the English sovereignty claim it enabled had disappeared, the Crown would lack the instrumental basis for large-scale land purchasing and legislative supremacy; MÄori authority structures would remain territorially intact, and the colonial legal order would require genuinely consensual foundation or outright conquest rather than contractual cover.
% FOUNDING_PROBLEM: British colonial expansion in Aotearoa required a legal instrument to secure large-scale land access and settler governance while avoiding the costs and instability of purely military conquest.
% FOUNDING_PROBLEM_CORROBORATION: Colonial Office memoranda and Hobson's instructions from 1839 corroborate the need for a 'voluntary' cession to preempt French claims and secure land. MÄori historians and Waitangi Tribunal reports from outside the Crown beneficiary seat corroborate that the problem was British colonial need, not a mutual MÄori-British coordination failure, and that the treaty was the instrumental solution to that colonial problem.
narrative_ontology:disappearance_verdict(treaty_authority_cession__retrospective_snare_exposure, world_rearranges).
narrative_ontology:founding_problem_status(treaty_authority_cession__retrospective_snare_exposure, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(treaty_authority_cession__retrospective_snare_exposure, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(treaty_authority_cession__retrospective_snare_exposure, 'none', 1).
narrative_ontology:epsilon_provenance(treaty_authority_cession__retrospective_snare_exposure, 0.85, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is very high (0.85) because the mechanism transferred the majority of MÄori land and all sovereign authority under a textual cover the signatories could not have assented to. Suppression is high (0.78) because the mechanism required active enforcement: military action, the Native Land Court, legislative override of MÄori authority, and the suppression of MÄori language and institutions. Theater ratio is moderately high (0.60) because the constraint depended heavily on performative partnership â ceremonial signing, flag-raising, gift exchange â to mask the extraction. Accessibility collapse is high (0.82) because once the Crown asserted sovereignty and established the Native Land Court, MÄori alternatives for territorial authority collapsed rapidly. Resistance is substantial (0.72) because the constraint met continuous resistance from the New Zealand Wars through to modern treaty claims. The temporal measurements show extraction rising sharply after 1840 as the land-purchasing machinery accelerated, theater dropping during open confiscation, then rising again with modern partnership rhetoric, while suppression shifted from military to legal-bureaucratic forms.
 *
 * PERSPECTIVAL GAP:
 *   From the Crown seat, the arrangement was a lawful treaty establishing sovereignty and land-purchase protocols. From the MÄori signatory seat, the arrangement was a ceremonial exchange where rangatiratanga was retained, followed by unilateral confiscation and override. The engine computes this divergence from the structural data: beneficiary declarations on the Crown side, payer declarations on the MÄori side, and the trapped/identity-locked exit options that amplify effective extraction for the target seats.
 *
 * DIRECTIONALITY LOGIC:
 *   The Crown land apparatus is the full beneficiary (d near 0.0): the English text's sovereignty claim subsidizes its authority to acquire land and legislate. MÄori signatories and descendants are full targets (d near 1.0): they bear the extraction of land and authority through a text they could not have understood as ceding sovereignty. Non-signatory iwi are also targets (d near 1.0) despite not signing, because the sovereignty claim was applied universally. The analyst seat is neutral (d = 0.5, analytical exit).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â securing colonial land and governance under color of consent â is dead. The colonial land-purchasing imperative that motivated the treaty is no longer a live coordination need. Yet the constraint persists in the form of Crown sovereignty doctrine, parliamentary supremacy, and the land tenure system. This mismatch between a dead founding problem and a persisting extractive structure is exactly the mandatrophy pattern the R5 fields are designed to catch: founding_problem_status=dead paired with disappearance_verdict=world_rearranges flags that the arrangement persists not because it solves a current coordination problem, but because it embeds accumulated extraction in constitutional architecture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    translation_intent_ambiguity,
    'Was the divergence between the English and MÄori texts deliberate extraction or translation incompetence?',
    'Archival discovery of Colonial Office instructions to translators and comparative analysis of the translation process.',
    'Deliberate divergence strengthens snare classification; incompetence might suggest tangled_rope (coordination attempted but failed asymmetrically).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(translation_intent_ambiguity, empirical, 'Whether textual divergence was deliberate extraction or incompetence').

omega_variable(
    maori_conceptual_sovereignty_possibility,
    'Could MÄori political thought in 1840 accommodate the concept of absolute sovereignty transfer as understood in English law?',
    'Comparative historical anthropology of MÄori and English political concepts in the 1830s-1840s.',
    'If impossible, the English cession claim is structurally fraudulent; if possible, the divergence is less determinative.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(maori_conceptual_sovereignty_possibility, conceptual, 'Whether MÄori conceptual framework could contain English sovereignty').

omega_variable(
    modern_continuity_of_extraction,
    'Does contemporary Crown sovereignty and land tenure continue the same extraction mechanism, or has the constraint transformed into a distinct type?',
    'Analysis of post-settlement governance, treaty settlement legislation, and contemporary constitutional practice.',
    'If continuous, the snare persists; if transformed, a new constraint story is needed for the modern period.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(modern_continuity_of_extraction, empirical, 'Whether modern Crown practice continues the historical snare').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(treaty_authority_cession__retrospective_snare_exposure, 1840, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trea_tr_t1840, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1840, 0.7).
narrative_ontology:measurement(trea_tr_t1860, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1860, 0.25).
narrative_ontology:measurement(trea_tr_t1900, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1900, 0.35).
narrative_ontology:measurement(trea_tr_t1960, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1960, 0.3).
narrative_ontology:measurement(trea_tr_t1985, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1985, 0.55).
narrative_ontology:measurement(trea_tr_t2026, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 2026, 0.6).

% Extraction over time
narrative_ontology:measurement(trea_be_t1840, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1840, 0.6).
narrative_ontology:measurement(trea_be_t1860, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1860, 0.78).
narrative_ontology:measurement(trea_be_t1900, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1900, 0.84).
narrative_ontology:measurement(trea_be_t1960, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1960, 0.8).
narrative_ontology:measurement(trea_be_t1985, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1985, 0.83).
narrative_ontology:measurement(trea_be_t2026, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 2026, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(trea_su_t1840, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1840, 0.5).
narrative_ontology:measurement(trea_su_t1860, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1860, 0.9).
narrative_ontology:measurement(trea_su_t1900, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1900, 0.8).
narrative_ontology:measurement(trea_su_t1960, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1960, 0.65).
narrative_ontology:measurement(trea_su_t1985, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1985, 0.55).
narrative_ontology:measurement(trea_su_t2026, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 2026, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


% DUAL FORMULATION NOTE:
% This constraint is one member of the treaty_authority_cession kernel family. It decomposes the colloquial label 'Treaty of Waitangi' into structurally distinct readings: crown_cession_reading (English-text supremacy), rangatiratanga_retention_reading (MÄori-text partnership), and retrospective_snare_exposure (textual divergence as extraction mechanism). Each has its own epsilon, beneficiary/victim structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
