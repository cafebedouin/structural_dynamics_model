% ============================================================================
% CONSTRAINT STORY: treaty_authority_cession__retrospective_snare_exposure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    narrative_ontology:measurement_basis/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: treaty_authority_cession__retrospective_snare_exposure
 *   human_readable: Treaty Authority Cession (Retrospective Snare Exposure Reading)
 *   domain: constitutional_law/indigenous_rights/colonial_history
 *
 * SUMMARY:
 *   This constraint describes the Treaty of Waitangi as a 'retrospective
 *   snare' from the perspective of Māori signatories and their descendants.
 *   The core mechanism of extraction is the textual divergence between the
 *   Māori and English versions of the Treaty, where Māori chiefs assented to
 *   a document (Māori text) that preserved their authority (tino
 *   rangatiratanga), while the Crown subsequently enforced a different
 *   document (English text) claiming full sovereignty (kāwanatanga). This
 *   divergence, initially covert, became the structural basis for land
 *   alienation and legislative override, making the Treaty an extractive
 *   mechanism that operated under the guise of agreement. The snare is
 *   'retrospective' because its full extractive nature only became apparent
 *   as the Crown's interpretation was enforced over time, revealing the
 *   impossibility of Māori assent to the English claim.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(treaty_authority_cession__retrospective_snare_exposure, 0.95).
domain_priors:suppression_score(treaty_authority_cession__retrospective_snare_exposure, 0.88).
domain_priors:theater_ratio(treaty_authority_cession__retrospective_snare_exposure, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, extractiveness, 0.95).
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(treaty_authority_cession__retrospective_snare_exposure, snare).
narrative_ontology:human_readable(treaty_authority_cession__retrospective_snare_exposure, "Treaty Authority Cession (Retrospective Snare Exposure Reading)").
narrative_ontology:topic_domain(treaty_authority_cession__retrospective_snare_exposure, "constitutional_law/indigenous_rights/colonial_history").

domain_priors:requires_active_enforcement(treaty_authority_cession__retrospective_snare_exposure).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(treaty_authority_cession__retrospective_snare_exposure, '1d6657ad-8c40-4189-8409-d97ebbf53ccc').
narrative_ontology:cs_kernel_codification('1d6657ad-8c40-4189-8409-d97ebbf53ccc', fixed_text).
narrative_ontology:cs_authority_grounding('1d6657ad-8c40-4189-8409-d97ebbf53ccc', lineage).
narrative_ontology:cs_interpretation_layer_present('1d6657ad-8c40-4189-8409-d97ebbf53ccc').
narrative_ontology:cs_reading_relation('1d6657ad-8c40-4189-8409-d97ebbf53ccc', treaty_authority_cession__crown_cession_reading, forecloses).
narrative_ontology:cs_reading_relation('1d6657ad-8c40-4189-8409-d97ebbf53ccc', treaty_authority_cession__rangatiratanga_retention_reading, coexists_with).
narrative_ontology:cs_axiom('1d6657ad-8c40-4189-8409-d97ebbf53ccc', foundational, textual_divergence_precludes_assent).
narrative_ontology:cs_axiom_status(textual_divergence_precludes_assent, holdable).
narrative_ontology:cs_axiom_grounding('1d6657ad-8c40-4189-8409-d97ebbf53ccc', textual_divergence_precludes_assent, deontological).
narrative_ontology:cs_axiom('1d6657ad-8c40-4189-8409-d97ebbf53ccc', foundational, retrospective_extraction_is_illegitimate).
narrative_ontology:cs_axiom_status(retrospective_extraction_is_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('1d6657ad-8c40-4189-8409-d97ebbf53ccc', retrospective_extraction_is_illegitimate, deontological).
narrative_ontology:cs_reference_frame('1d6657ad-8c40-4189-8409-d97ebbf53ccc', maori_text_as_primary_authority).
narrative_ontology:cs_drift_state('1d6657ad-8c40-4189-8409-d97ebbf53ccc', post_colonial_legislative_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('1d6657ad-8c40-4189-8409-d97ebbf53ccc', '').
narrative_ontology:cs_kernel_id(treaty_authority_cession__retrospective_snare_exposure, treaty_authority_cession).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(treaty_authority_cession__retrospective_snare_exposure, crown_land_purchasing_apparatus).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__retrospective_snare_exposure, settler_government).
narrative_ontology:constraint_victim(treaty_authority_cession__retrospective_snare_exposure, maori_signatories).
narrative_ontology:constraint_victim(treaty_authority_cession__retrospective_snare_exposure, maori_descendants).
narrative_ontology:constraint_victim(treaty_authority_cession__retrospective_snare_exposure, iwi_hapu).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Chiefs who signed the Māori text of the Treaty of Waitangi, believing they were assenting to a form of governance that preserved their authority (tino rangatiratanga), not ceding sovereignty. Their 'assent' was structurally impossible given the textual divergence.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, maori_signatories, payer,
    powerless, generational, identity_locked, national).

% Generations of Māori whose land and authority were progressively alienated under the English interpretation of the Treaty, experiencing the ongoing effects of the original textual snare. Their identity is deeply tied to the land and ancestral authority.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, maori_descendants, payer,
    powerless, generational, identity_locked, national).

% Māori tribal groups and sub-tribes who collectively lost land and self-governance, now engaged in ongoing claims and litigation to reclaim rights and resources. Their collective action is a form of resistance against the snare's ongoing effects.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, iwi_hapu, payer,
    organized, generational, constrained, regional).

% The colonial and later settler government mechanisms that acquired vast tracts of Māori land, legitimizing these acquisitions through the English text's interpretation of sovereignty cession. This apparatus directly benefited from the textual snare.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, crown_land_purchasing_apparatus, beneficiary,
    institutional, generational, arbitrage, national).

% The successive governments that inherited and perpetuated the English interpretation of the Treaty, enacting legislation and policies that overrode Māori authority and facilitated land alienation. They administer the legal framework that maintains the snare.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, settler_government, agenda_setter,
    institutional, generational, mobile, national).

% The courts, which have historically upheld the Crown's interpretation but have more recently acknowledged the Treaty's principles and the Māori text, leading to some redress. They are a site of ongoing contestation and potential re-interpretation.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, new_zealand_judiciary, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ostensibly, to establish a framework for governance and settlement in New Zealand, coordinating relations between Māori and the British Crown.
% TRANSFER_FUNCTION: Transferred sovereignty (in the English text) and vast tracts of land from Māori to the British Crown and subsequent settler governments, while Māori believed they were retaining their authority (tino rangatiratanga).
% ABSENT_VOICES: The full, uncompromised voice of Māori signatories, whose understanding of the Māori text was fundamentally different from the English interpretation, was absent from the subsequent legislative and judicial processes that enforced the Crown's claim. Their true assent was never present.
% DISAPPEARANCE_RATIONALE: If the retrospective snare of textual divergence vanished, the entire constitutional and land ownership framework of New Zealand would be fundamentally challenged. Land titles, legislative authority, and the relationship between Māori and the Crown would require complete renegotiation, leading to a profound rearrangement of the nation's structure.
% FOUNDING_PROBLEM: The British Crown sought to establish sovereignty over New Zealand to facilitate settlement and regulate relations with Māori, while Māori sought to protect their lands and authority amidst increasing European presence.
% FOUNDING_PROBLEM_CORROBORATION: The settler government maintains the problem of establishing a unified nation was solved by the Treaty. Māori and independent historians corroborate that the problem of protecting Māori authority was not solved, but rather exacerbated by the textual divergence, leading to ongoing dispossession. The Waitangi Tribunal's findings provide extensive corroboration from outside the benefiting parties.
narrative_ontology:disappearance_verdict(treaty_authority_cession__retrospective_snare_exposure, world_rearranges).
narrative_ontology:founding_problem_status(treaty_authority_cession__retrospective_snare_exposure, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(treaty_authority_cession__retrospective_snare_exposure, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(treaty_authority_cession__retrospective_snare_exposure, 'none', 1).

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
 *   Extractiveness is very high (0.95) because the fundamental transfer of sovereignty and land was achieved through a mechanism that denied genuine consent, leading to profound and ongoing dispossession. Suppression is also very high (0.88) as the Crown's interpretation was enforced through legislative power, military force, and judicial rulings, actively suppressing Māori resistance and alternative interpretations. The theater ratio is high (0.65) because the 'agreement' itself was performative, masking a fundamental misunderstanding that enabled subsequent extraction. The initial coordination function (establishing a framework for relations) was quickly overshadowed by the extractive function, which became dominant and self-perpetuating.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the settler government, the Treaty was a legitimate act of cession, establishing a unified nation. From the Māori perspective, and this reading, it was a fundamental betrayal, a snare that leveraged linguistic and conceptual differences to dispossess them. The engine's classification will highlight this divergence by computing a snare from the victim's seat, despite the historical 'rope' framing by the beneficiaries.
 *
 * DIRECTIONALITY LOGIC:
 *   The Crown land-purchasing apparatus and the settler government are clear beneficiaries (d near 0.0) as they directly gained land and authority. Māori signatories, their descendants, and iwi/hapū are the primary victims (d near 1.0), experiencing the loss of land, sovereignty, and cultural integrity. The New Zealand judiciary, while an observer, has historically leaned towards the Crown's interpretation, though recent shifts indicate a more nuanced analytical position.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's original 'mandate' (to establish a legitimate basis for Crown authority and settlement) was fundamentally flawed by the textual divergence. The 'mandate' for Māori was to protect their authority. The persistence of the Crown's interpretation, despite growing evidence of the snare, indicates that the original 'coordination' function has atrophied into pure extraction, maintained by institutional inertia and power. Resolving this mandatrophy would require acknowledging the snare and renegotiating the foundational relationship based on the Māori text's intent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_assent_impossibility,
    'Was genuine Māori assent to the English concept of sovereignty (as understood by the Crown) structurally possible given the Māori text and cultural context?',
    'Deep historical and linguistic analysis of 19th-century Māori political concepts and the specific language used in the Māori text of the Treaty, corroborated by contemporary Māori oral histories.',
    'If genuine assent was structurally impossible, it strengthens the snare classification by demonstrating the foundational illegitimacy of the Crown''s claim. If some form of assent (even if limited) could be argued, it might slightly reduce the perceived extractiveness, though not eliminate it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(genuine_assent_impossibility, empirical, 'The structural possibility of Māori assent to the English sovereignty claim.').

omega_variable(
    textual_divergence_as_extraction_mechanism,
    'Is the textual divergence itself the primary extraction mechanism, or merely a symptom of a broader colonial power imbalance?',
    'Comparative analysis with other colonial treaties where textual divergence was present but did not lead to the same degree of dispossession, or where dispossession occurred without such divergence. This would clarify the causal role of the textual snare.',
    'If the divergence is the primary mechanism, it highlights the specific, almost ''linguistic'' nature of the snare. If it''s a symptom, the snare is still present, but the underlying power dynamics become the more fundamental constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_divergence_as_extraction_mechanism, conceptual, 'The causal role of textual divergence in the extraction process.').

omega_variable(
    mandatrophy_of_coordination_function,
    'To what extent has the Treaty''s original coordination function (establishing a framework for relations) atrophied, leaving only the extractive function?',
    'Analysis of legislative and judicial history: tracking the proportion of actions that genuinely coordinate vs. those that enforce the Crown''s sovereignty claim and facilitate land acquisition. Also, the degree of Māori resistance and calls for renegotiation.',
    'If the coordination function is fully atrophied, the snare classification is robust. If residual coordination elements are identified, it might suggest a ''tangled rope'' element, though the overwhelming asymmetry points to snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandatrophy_of_coordination_function, empirical, 'The degree of atrophy of the Treaty''s coordination function.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(treaty_authority_cession__retrospective_snare_exposure, 1840, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trea_tr_t1840, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1840, 0.3).
narrative_ontology:measurement_basis(trea_tr_t1840, observed).
narrative_ontology:measurement(trea_tr_t1870, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1870, 0.45).
narrative_ontology:measurement_basis(trea_tr_t1870, observed).
narrative_ontology:measurement(trea_tr_t1900, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1900, 0.55).
narrative_ontology:measurement_basis(trea_tr_t1900, observed).
narrative_ontology:measurement(trea_tr_t1950, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1950, 0.6).
narrative_ontology:measurement_basis(trea_tr_t1950, observed).
narrative_ontology:measurement(trea_tr_t1980, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1980, 0.65).
narrative_ontology:measurement_basis(trea_tr_t1980, observed).
narrative_ontology:measurement(trea_tr_t2024, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 2024, 0.65).
narrative_ontology:measurement_basis(trea_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(trea_be_t1840, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1840, 0.7).
narrative_ontology:measurement_basis(trea_be_t1840, observed).
narrative_ontology:measurement(trea_be_t1870, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1870, 0.85).
narrative_ontology:measurement_basis(trea_be_t1870, observed).
narrative_ontology:measurement(trea_be_t1900, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1900, 0.9).
narrative_ontology:measurement_basis(trea_be_t1900, observed).
narrative_ontology:measurement(trea_be_t1950, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1950, 0.92).
narrative_ontology:measurement_basis(trea_be_t1950, observed).
narrative_ontology:measurement(trea_be_t1980, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1980, 0.95).
narrative_ontology:measurement_basis(trea_be_t1980, observed).
narrative_ontology:measurement(trea_be_t2024, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 2024, 0.95).
narrative_ontology:measurement_basis(trea_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(trea_su_t1840, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1840, 0.6).
narrative_ontology:measurement_basis(trea_su_t1840, observed).
narrative_ontology:measurement(trea_su_t1870, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1870, 0.75).
narrative_ontology:measurement_basis(trea_su_t1870, observed).
narrative_ontology:measurement(trea_su_t1900, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1900, 0.8).
narrative_ontology:measurement_basis(trea_su_t1900, observed).
narrative_ontology:measurement(trea_su_t1950, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1950, 0.85).
narrative_ontology:measurement_basis(trea_su_t1950, observed).
narrative_ontology:measurement(trea_su_t1980, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1980, 0.88).
narrative_ontology:measurement_basis(trea_su_t1980, observed).
narrative_ontology:measurement(trea_su_t2024, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 2024, 0.88).
narrative_ontology:measurement_basis(trea_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
