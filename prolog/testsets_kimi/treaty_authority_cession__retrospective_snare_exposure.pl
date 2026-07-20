% ============================================================================
% CONSTRAINT STORY: treaty_authority_cession__retrospective_snare_exposure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   human_readable: Treaty Authority Cession via Mistranslation (Retrospective Snare Exposure)
 *   domain: constitutional_law/indigenous_rights/colonial_history
 *
 * SUMMARY:
 *   This constraint is the retrospective_snare_exposure reading of the
 *   treaty_authority_cession kernel. It treats the textual divergence between
 *   the English and MÄori treaty texts not as a translation error but as the
 *   operating mechanism of a snare: chiefs signing the MÄori text could not
 *   have assented to the English sovereignty claim because the semantic
 *   content differed materially. The Crown land-purchasing apparatus is the
 *   primary beneficiary of the extraction; MÄori signatories, their
 *   descendants, and iwi/hapu are the victim set. The constraint persists
 *   through legislative override and sovereign assertion that rely on the
 *   English text's authority. This is one of three structurally distinct
 *   readings of the kernel; the others (crown_cession_reading,
 *   rangatiratanga_retention_reading) instantiate different constraints with
 *   different Îµ values and must be authored separately.
 *
 * KEY AGENTS:
 *   - Crown land-purchasing apparatus (agenda_setter/beneficiary, institutional/arbitrage): designs, administers, and profits from the land transfer and sovereignty framework justified by the English text.
 *   - MÄori chiefs (payer, organized/trapped): signed the MÄori text and were subsequently subjected to the English legal framework's override.
 *   - MÄori descendants (payer, powerless/identity_locked): inherit the diffuse but cumulative costs of authority and land loss; exit is blocked by identity fusion with whenua.
 *   - Iwi/hapÅ« collectives (payer, organized/identity_locked): continuing governance structures whose territorial authority was displaced by Crown statute and local government.
 *   - Waitangi Tribunal (observer, institutional/analytical): produces retrospective findings on textual divergence but cannot alter the sovereignty framework.
 *   - MÄori text interpreters (excluded, moderate/constrained): understood the MÄori text's limitations on cession but were denied standing in the English legal interpretive framework.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(treaty_authority_cession__retrospective_snare_exposure, 0.9).
domain_priors:suppression_score(treaty_authority_cession__retrospective_snare_exposure, 0.85).
domain_priors:theater_ratio(treaty_authority_cession__retrospective_snare_exposure, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, extractiveness, 0.9).
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(treaty_authority_cession__retrospective_snare_exposure, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(treaty_authority_cession__retrospective_snare_exposure, snare).
narrative_ontology:human_readable(treaty_authority_cession__retrospective_snare_exposure, "Treaty Authority Cession via Mistranslation (Retrospective Snare Exposure)").
narrative_ontology:topic_domain(treaty_authority_cession__retrospective_snare_exposure, "constitutional_law/indigenous_rights/colonial_history").

domain_priors:requires_active_enforcement(treaty_authority_cession__retrospective_snare_exposure).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(treaty_authority_cession__retrospective_snare_exposure, 'a9acce12-bc10-4cd5-9406-1130a4bc4403').
narrative_ontology:cs_kernel_codification('a9acce12-bc10-4cd5-9406-1130a4bc4403', fixed_text).
narrative_ontology:cs_authority_grounding('a9acce12-bc10-4cd5-9406-1130a4bc4403', extraction).
narrative_ontology:cs_interpretation_layer_present('a9acce12-bc10-4cd5-9406-1130a4bc4403').
narrative_ontology:cs_reading_relation('a9acce12-bc10-4cd5-9406-1130a4bc4403', treaty_authority_cession__crown_cession_reading, forecloses).
narrative_ontology:cs_reading_relation('a9acce12-bc10-4cd5-9406-1130a4bc4403', treaty_authority_cession__rangatiratanga_retention_reading, coexists_with).
narrative_ontology:cs_axiom('a9acce12-bc10-4cd5-9406-1130a4bc4403', foundational, textual_divergence_voids_sovereignty_assent).
narrative_ontology:cs_axiom_status(textual_divergence_voids_sovereignty_assent, holdable).
narrative_ontology:cs_axiom_grounding('a9acce12-bc10-4cd5-9406-1130a4bc4403', textual_divergence_voids_sovereignty_assent, empirically_contingent).
narrative_ontology:cs_axiom('a9acce12-bc10-4cd5-9406-1130a4bc4403', foundational, crown_sovereignty_derived_from_mistranslated_text_is_extractive).
narrative_ontology:cs_axiom_status(crown_sovereignty_derived_from_mistranslated_text_is_extractive, holdable).
narrative_ontology:cs_axiom_grounding('a9acce12-bc10-4cd5-9406-1130a4bc4403', crown_sovereignty_derived_from_mistranslated_text_is_extractive, deontological).
narrative_ontology:cs_reference_frame('a9acce12-bc10-4cd5-9406-1130a4bc4403', bilateral_limited_governance_agreement).
narrative_ontology:cs_drift_state('a9acce12-bc10-4cd5-9406-1130a4bc4403', contemporary_retrospective_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('a9acce12-bc10-4cd5-9406-1130a4bc4403', '').
narrative_ontology:cs_kernel_id(treaty_authority_cession__retrospective_snare_exposure, treaty_authority_cession).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(treaty_authority_cession__retrospective_snare_exposure, crown_land_purchasing_apparatus).
narrative_ontology:constraint_victim(treaty_authority_cession__retrospective_snare_exposure, maori_chiefs).
narrative_ontology:constraint_victim(treaty_authority_cession__retrospective_snare_exposure, maori_descendants).
narrative_ontology:constraint_victim(treaty_authority_cession__retrospective_snare_exposure, iwi_hapu).
narrative_ontology:constraint_vindicates(treaty_authority_cession__retrospective_snare_exposure, crown_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(treaty_authority_cession__retrospective_snare_exposure, english_text_jurisdictional_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operated the Native Land Court, land purchase offices, and legislative override mechanisms to convert MÄori customary tenure into Crown-granted titles. Justified acquisitions through the English treaty text's sovereignty clause. Could alter the legal framework but gains substantial land and authority from its persistence.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, crown_land_purchasing_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(treaty_authority_cession__retrospective_snare_exposure, crown_land_purchasing_apparatus, beneficiary).

% Signed the MÄori text believing they retained tino rangatiratanga and ceded only limited kÄwanatanga. Subsequently found their authority overridden by Crown institutions citing the English text. Militarily and legally unable to reverse the imposition once the English legal framework was enforced.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, maori_chiefs, payer,
    organized, biographical, trapped, national).

% Inherit the cumulative loss of land and authority justified by the English treaty text. Subject to legislative override and Crown sovereignty assertions. Exit options are constrained by identity fusion with whenua and whakapapa; rejecting the constraint framework implies existential rupture rather than simple relocation.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, maori_descendants, payer,
    powerless, generational, identity_locked, national).

% Collective governance structures whose territorial authority was systematically displaced by Crown local government and statute law. Continue to bear the constraint's costs through extinguishment of customary title in settlement processes that rely on the Crown's sovereignty narrative.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, iwi_hapu, payer,
    organized, generational, identity_locked, regional).

% Investigates treaty breaches and textual divergence, producing retrospective findings that the MÄori text did not cede sovereignty. Lacks authority to alter the underlying Crown sovereignty or land tenure framework, functioning as an analytical observer rather than an agenda-setter.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, waitangi_tribunal, observer,
    institutional, generational, analytical, national).

% MÄori linguistic experts and rangatira who understood kÄwanatanga as limited governance were structurally excluded from the English legal framework's interpretive authority. Their readings had no standing in Crown law and were suppressed as the English text was enforced.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__retrospective_snare_exposure, maori_text_interpreters, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(treaty_authority_cession__retrospective_snare_exposure, crown_land_purchasing_apparatus).
narrative_ontology:fixing_cost_class(treaty_authority_cession__retrospective_snare_exposure, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Purported to coordinate peaceful settlement and a stable authority framework between the British Crown and MÄori collectives by establishing mutual understandings of governance and land tenure.
% TRANSFER_FUNCTION: Transferred land and ultimate authority from MÄori collectives to the Crown under the legal cover of a treaty of cession; moved interpretive authority over the text exclusively to English-language colonial institutions, concentrating land title and legislative power with the Crown apparatus.
% ABSENT_VOICES: MÄori linguistic experts and rangatira who understood kÄwanatanga as limited governance were structurally excluded from the English legal interpretive framework; dissenting chiefs who refused to sign or who contested the English text were marginalized or militarily suppressed.
% DISAPPEARANCE_RATIONALE: If the textual divergence were recognized as voiding MÄori assent to Crown sovereignty, the entire colonial and post-colonial land tenure system, legislative supremacy, and asserted Crown sovereignty over New Zealand would lack foundational legitimacy. The legal, property, and constitutional order would be forced into fundamental rearrangement.
% FOUNDING_PROBLEM: Colonial administration required a legal instrument to acquire MÄori land and assert sovereignty over New Zealand in the face of an organized indigenous population with established authority structures.
% FOUNDING_PROBLEM_CORROBORATION: Historical records from colonial officials attest the imperial need for sovereignty instruments. However, the Waitangi Tribunal and subsequent historians outside the benefiting Crown apparatus corroborate that the colonial acquisition imperative was the true founding problem, while contesting whether the treaty represented a consensual solution.
narrative_ontology:disappearance_verdict(treaty_authority_cession__retrospective_snare_exposure, world_rearranges).
narrative_ontology:founding_problem_status(treaty_authority_cession__retrospective_snare_exposure, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(treaty_authority_cession__retrospective_snare_exposure, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(treaty_authority_cession__retrospective_snare_exposure, 'none', 1).
narrative_ontology:epsilon_provenance(treaty_authority_cession__retrospective_snare_exposure, 0.9, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is very high (0.90) because the constraint transferred the bulk of MÄori land and asserted unilateral sovereignty over all New Zealand, operating through a text that the signatories could not have understood as conveying what the English version claimed. Suppression is high (0.85) because the constraint's persistence required systematically overriding MÄori authority, militarily suppressing resistance, and excluding MÄori linguistic interpretations from legal standing. Theater ratio (0.60) captures the ongoing performance of treaty partnership and biculturalism that masks the underlying extraction of authority and the continued legislative supremacy. Accessibility collapse (0.88) is high because once the English legal framework was imposed, alternatives such as independent MÄori jurisdiction or unfettered rangatiratanga collapsed almost completely. Resistance (0.55) reflects persistent MÄori resistance movements (Kingitanga, Kotahitanga, modern treaty protests) that were structurally suppressed but never fully extinguished. The temporal measurements show extraction intensifying through the nineteenth century, plateauing under twentieth-century assimilation, and persisting into the contemporary era through settlement processes that still rely on Crown sovereignty.
 *
 * PERSPECTIVAL GAP:
 *   From the Crown apparatus seat, the constraint appears as a legitimate legal foundation for sovereignty and land tenure â a necessary coordination mechanism for colonial and post-colonial governance. From the MÄori signatory and descendant seats, the same structure is experienced as covert extraction: a promise of protection and limited governance that operated as a mechanism for land confiscation and authority suppression. The analytical observer seat (Waitangi Tribunal) computes a third view: the divergence is objectively recoverable, making the extraction visible retrospectively. The engine computes these divergent per-seat types from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The Crown land-purchasing apparatus is declared as beneficiary and agenda-setter, deriving directionality near the full-beneficiary end (low d). MÄori chiefs, descendants, and iwi/hapÅ« are declared victims and payers, deriving directionality near the full-target end (high d). The apparatus had arbitrage-grade exit (it could have altered the framework) but chose not to because it captured the extraction. MÄori descendants and iwi/hapÅ« carry identity_locked exit: leaving the constraint framework implies cultural and existential rupture, amplifying effective extraction. The Waitangi Tribunal sits at analytical exit with symmetric directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is classified as a snare rather than a rope or tangled rope because the coordination story (a treaty of peaceful settlement and shared governance) is cover for pure extraction. There is no genuine collective-action problem solved by Crown sovereignty and land purchase; the 'problem' from the benefiting side was how to acquire territory and authority from an indigenous population. The constraint prevents mislabeling as coordination by its lack of a symmetric beneficiary set: the Crown apparatus collects land and authority while MÄori collectives pay. A tangled_rope reading would require a genuine coordination function alongside extraction; here, the coordination story was the vehicle of extraction, not an independent function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    treaty_kernel_reading_contest,
    'Does the classification of this constraint as a snare depend on adopting the retrospective_snare_exposure reading, or would the crown_cession_reading reclassify the same historical structure as a rope or mountain?',
    'Cross-constraint family comparison: compile all three readings and examine whether the same historical events receive different engine-computed types based solely on reading choice.',
    'If type is reading-dependent, the kernel is irreducibly contested and the decomposition into separate constraint stories is validated; if not, one reading may be structurally dominant.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(treaty_kernel_reading_contest, conceptual, 'Whether kernel classification is reading-dependent').

omega_variable(
    intentionality_of_translation_divergence,
    'Was the textual divergence between the English and MÄori treaty texts a deliberately designed extraction mechanism, or the product of colonial translational incompetence?',
    'Archival analysis of drafting instructions (Busby, Hobson, Williams) and comparison of translation choices against contemporary MÄori linguistic evidence and translation capability.',
    'Deliberate design would solidify snare classification; incompetence might suggest tangled_rope (unintended extraction layered on a coordination attempt).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intentionality_of_translation_divergence, empirical, 'Whether divergence was deliberate extraction or translational failure').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of MÄori authority structural (maintained by Crown legal and police apparatus) or internalized (accepted by some MÄori descendants as legitimate due to colonial education and identity fusion)?',
    'Post-structural-reform trajectory analysis: measure whether authority claims revive rapidly when structural barriers ease, or whether change lags due to internalized legitimacy.',
    'If internalized, effective suppression exceeds the structural measure and the constraint operates as a deeper snare with identity-locked exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism').

omega_variable(
    retrospective_visibility_bias,
    'Does retrospective visibility of the extraction mechanism depend on present-day power shifts, or was the snare structure objectively recoverable from contemporary evidence?',
    'Historiographic analysis of contemporary MÄori dissent (1860s Kingitanga, 1880s Kotahitanga) to determine whether the snare structure was articulated at the time or is anachronistic.',
    'If recoverable contemporaneously, the constraint was always a snare; if only visible retrospectively, classification may shift toward piton for the post-founding period.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(retrospective_visibility_bias, conceptual, 'Whether snare classification is time-indexed').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(treaty_authority_cession__retrospective_snare_exposure, 1840, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(treaty_snare_tr_t1840, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1840, 0.25).
narrative_ontology:measurement(treaty_snare_tr_t1860, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1860, 0.45).
narrative_ontology:measurement(treaty_snare_tr_t1900, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1900, 0.7).
narrative_ontology:measurement(treaty_snare_tr_t1940, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1940, 0.75).
narrative_ontology:measurement(treaty_snare_tr_t1980, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 1980, 0.65).
narrative_ontology:measurement(treaty_snare_tr_t2024, treaty_authority_cession__retrospective_snare_exposure, theater_ratio, 2024, 0.6).

% Extraction over time
narrative_ontology:measurement(treaty_snare_be_t1840, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1840, 0.35).
narrative_ontology:measurement(treaty_snare_be_t1860, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1860, 0.75).
narrative_ontology:measurement(treaty_snare_be_t1900, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1900, 0.88).
narrative_ontology:measurement(treaty_snare_be_t1940, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1940, 0.92).
narrative_ontology:measurement(treaty_snare_be_t1980, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 1980, 0.85).
narrative_ontology:measurement(treaty_snare_be_t2024, treaty_authority_cession__retrospective_snare_exposure, base_extractiveness, 2024, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(treaty_snare_su_t1840, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1840, 0.6).
narrative_ontology:measurement(treaty_snare_su_t1860, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1860, 0.9).
narrative_ontology:measurement(treaty_snare_su_t1900, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1900, 0.95).
narrative_ontology:measurement(treaty_snare_su_t1940, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1940, 0.8).
narrative_ontology:measurement(treaty_snare_su_t1980, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 1980, 0.65).
narrative_ontology:measurement(treaty_snare_su_t2024, treaty_authority_cession__retrospective_snare_exposure, suppression_requirement, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(treaty_authority_cession__retrospective_snare_exposure, crown_cession_reading).
narrative_ontology:affects_constraint(treaty_authority_cession__retrospective_snare_exposure, rangatiratanga_retention_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'Treaty of Waitangi' conflates three structurally distinct constraints: crown_cession_reading (English text supremacy, negligible extraction from Crown seat), rangatiratanga_retention_reading (MÄori text partnership, coordination function), and retrospective_snare_exposure (textual divergence as extraction mechanism). Each has a different Îµ, beneficiary/victim structure, and classification. They are linked as a constraint family because they share the same kernel but instantiate different structural claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
