% ============================================================================
% CONSTRAINT STORY: treaty_authority_cession__rangatiratanga_retention_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: treaty_authority_cession__rangatiratanga_retention_reading
 *   human_readable: Treaty of Waitangi Authority: Rangatiratanga Retention Reading
 *   domain: constitutional law / indigenous rights / colonial history
 *
 * SUMMARY:
 *   This constraint story instantiates the rangatiratanga retention reading
 *   of the Treaty of Waitangi authority kernel. Under this reading, the
 *   MÄori text controls via contra proferentem, 'kÄwanatanga' is limited to
 *   governance, and 'tino rangatiratanga' denotes retained sovereignty. The
 *   treaty establishes a partnership in which Crown authority over MÄori
 *   land and resources is legitimate only with ongoing hapÅ« consent. The
 *   reading exposes the retrospective snare visible in land alienation:
 *   chiefs signing the MÄori text could not have assented to the English
 *   sovereignty claim, making subsequent legislative override and land
 *   transfers structurally extractive.
 *
 * KEY AGENTS:
 *   - maori_hapu_iwi: Primary beneficiary (organized/constrained) â retains sovereignty through partnership and consent rights.
 *   - alienated_maori_landholders: Primary target (powerless/trapped) â bore the costs of extraction via land alienation under translation asymmetry.
 *   - crown_in_parliament_and_executive: Agenda setter (institutional/constrained) â administers treaty policy and asserts sovereignty while constrained by partnership obligations.
 *   - waitangi_tribunal: Analytical observer (institutional/analytical) â interprets the treaty but lacks enforcement.
 *   - maori_independence_advocates: Excluded voice (moderate/identity_locked) â rejects treaty framework entirely, absent from negotiation tables.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(treaty_authority_cession__rangatiratanga_retention_reading, 0.62).
domain_priors:suppression_score(treaty_authority_cession__rangatiratanga_retention_reading, 0.58).
domain_priors:theater_ratio(treaty_authority_cession__rangatiratanga_retention_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(treaty_authority_cession__rangatiratanga_retention_reading, tangled_rope).
narrative_ontology:human_readable(treaty_authority_cession__rangatiratanga_retention_reading, "Treaty of Waitangi Authority: Rangatiratanga Retention Reading").
narrative_ontology:topic_domain(treaty_authority_cession__rangatiratanga_retention_reading, "constitutional law / indigenous rights / colonial history").

domain_priors:requires_active_enforcement(treaty_authority_cession__rangatiratanga_retention_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(treaty_authority_cession__rangatiratanga_retention_reading, '0a757bc3-71b1-4286-a59f-dcf2ab44bbc6').
narrative_ontology:cs_kernel_codification('0a757bc3-71b1-4286-a59f-dcf2ab44bbc6', fixed_text).
narrative_ontology:cs_authority_grounding('0a757bc3-71b1-4286-a59f-dcf2ab44bbc6', lineage).
narrative_ontology:cs_interpretation_layer_present('0a757bc3-71b1-4286-a59f-dcf2ab44bbc6').
narrative_ontology:cs_reading_relation('0a757bc3-71b1-4286-a59f-dcf2ab44bbc6', treaty_authority_cession__crown_cession_reading, forecloses).
narrative_ontology:cs_reading_relation('0a757bc3-71b1-4286-a59f-dcf2ab44bbc6', treaty_authority_cession__biculturalism_reading, coexists_with).
narrative_ontology:cs_axiom('0a757bc3-71b1-4286-a59f-dcf2ab44bbc6', foundational, tino_rangatiratanga_retained_as_sovereignty).
narrative_ontology:cs_axiom_status(tino_rangatiratanga_retained_as_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('0a757bc3-71b1-4286-a59f-dcf2ab44bbc6', tino_rangatiratanga_retained_as_sovereignty, conventional).
narrative_ontology:cs_axiom('0a757bc3-71b1-4286-a59f-dcf2ab44bbc6', foundational, crown_authority_conditional_on_ongoing_consent).
narrative_ontology:cs_axiom_status(crown_authority_conditional_on_ongoing_consent, holdable).
narrative_ontology:cs_axiom_grounding('0a757bc3-71b1-4286-a59f-dcf2ab44bbc6', crown_authority_conditional_on_ongoing_consent, conventional).
narrative_ontology:cs_reference_frame('0a757bc3-71b1-4286-a59f-dcf2ab44bbc6', rangatiratanga_intact_partnership).
narrative_ontology:cs_drift_state('0a757bc3-71b1-4286-a59f-dcf2ab44bbc6', contemporary_jurisprudence, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0a757bc3-71b1-4286-a59f-dcf2ab44bbc6', '').
narrative_ontology:cs_kernel_id(treaty_authority_cession__rangatiratanga_retention_reading, treaty_authority_cession).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(treaty_authority_cession__rangatiratanga_retention_reading, maori_hapu_iwi).
narrative_ontology:constraint_victim(treaty_authority_cession__rangatiratanga_retention_reading, alienated_maori_landholders).
narrative_ontology:constraint_vindicates(treaty_authority_cession__rangatiratanga_retention_reading, contra_proferentem_indigenous_texts).
narrative_ontology:constraint_vindicates(treaty_authority_cession__rangatiratanga_retention_reading, tino_rangatiratanga_as_sovereignty).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold tino rangatiratanga as retained sovereignty under the MÄori text; exercise ongoing consent rights in treaty settlements and resource management; constrained by the Crown's institutional dominance but recognized as partnership parties with collective decision-making authority.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, maori_hapu_iwi, beneficiary,
    organized, generational, constrained, national).

% Administers the treaty settlement process and legislates for MÄori land and resources; under this reading its authority is constrained by the requirement of ongoing hapÅ« consent, though it frequently asserts parliamentary sovereignty unilaterally.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, crown_in_parliament_and_executive, agenda_setter,
    institutional, generational, constrained, national).

% MÄori individuals and communities whose land was alienated through Crown purchase, confiscation, or Native Land Court individualization, operating under the English-text assumption of ceded sovereignty rather than the MÄori-text retention of rangatiratanga.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, alienated_maori_landholders, payer,
    powerless, generational, trapped, national).

% Investigates treaty breaches and produces findings supporting the rangatiratanga retention reading; lacks enforcement power but shapes settlement negotiations and public understanding.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, waitangi_tribunal, observer,
    institutional, generational, analytical, national).

% Reject the treaty framework entirely as a colonial imposition; would argue for full independence rather than partnership within the Crown's constitutional structure; excluded from mainstream treaty negotiation tables.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, maori_independence_advocates, excluded,
    moderate, generational, identity_locked, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Distributes authority between Crown and MÄori by reserving tino rangatiratanga to hapÅ« while permitting Crown kÄwanatanga only with ongoing consent, creating a bilateral governance partnership.
% TRANSFER_FUNCTION: Transfers land and resource decision-making authority from unilateral Crown control to a consent-based partnership; historically transferred MÄori land to Crown ownership through statutes justified by the English-text reading.
% ABSENT_VOICES: MÄori independence advocates who reject any Crown legitimacy are excluded from treaty negotiation architecture; Crown officials committed to the cession reading are present but their framework is contested by this reading.
% DISAPPEARANCE_RATIONALE: If the partnership-with-consent framework disappeared, Crown assertions of sovereignty over MÄori land and resources would lose their only legitimacy under this reading, and the historical justification for land alienation would unravel.
% FOUNDING_PROBLEM: How to establish Crown presence in Aotearoa without annihilating MÄori self-governance, and how to secure MÄori agreement to British settlement while protecting against foreign encroachment.
% FOUNDING_PROBLEM_CORROBORATION: MÄori oral testimony and the MÄori treaty text corroborate the protection-of-rangatiratanga framing; British Colonial Office archives and legislative history corroborate the sovereignty-cession framing. Independent historians and the Waitangi Tribunal provide cross-cutting assessment outside both beneficiary structures.
narrative_ontology:disappearance_verdict(treaty_authority_cession__rangatiratanga_retention_reading, world_rearranges).
narrative_ontology:founding_problem_status(treaty_authority_cession__rangatiratanga_retention_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(treaty_authority_cession__rangatiratanga_retention_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(treaty_authority_cession__rangatiratanga_retention_reading, 'none', 1).
narrative_ontology:epsilon_provenance(treaty_authority_cession__rangatiratanga_retention_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(treaty_authority_cession__rangatiratanga_retention_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(treaty_authority_cession__rangatiratanga_retention_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(treaty_authority_cession__rangatiratanga_retention_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is set at the contemporary endpoint to reflect that Crown unilateralism in land and resource governance persists despite partnership rhetoric, though it has declined from colonial peaks. Suppression (0.58) reflects ongoing legislative and judicial marginalization of the MÄori text in favor of Crown sovereignty doctrines, albeit reduced since the Tribunal era. Theater ratio (0.48) is elevated because modern treaty settlements and partnership consultations often perform consent without substantive veto power. Accessibility collapse (0.72) captures that for over a century the MÄori-text alternative was legally inaccessible, though partial revival has occurred. Resistance (0.75) is high due to persistent MÄori political mobilization, litigation, and direct action defending rangatiratanga. The measurement series trace a colonial peak in extraction and suppression followed by gradual decay and theatricalization.
 *
 * PERSPECTIVAL GAP:
 *   The Crown seat experiences the constraint as a necessary limitation on its sovereignty that nonetheless preserves order and legitimacy; the MÄori beneficiary seat experiences it as a hard-won recognition of retained authority; the alienated landholder seat experiences it as the architecture of dispossession. The engine should compute high directionality for the alienated landholders and low directionality for the hapÅ« beneficiaries, with the Crown seat near symmetric or slightly target-side because its authority is structurally constrained.
 *
 * DIRECTIONALITY LOGIC:
 *   maori_hapu_iwi are beneficiaries (retained rangatiratanga, ongoing consent rights) â low d, damped extraction. alienated_maori_landholders are victims (dispossessed via Crown statutory machinery) â high d, amplified extraction. crown_in_parliament_and_executive is agenda setter: it benefits from coordination legitimacy but pays through constrained sovereignty; structural derivation places it near symmetric. No override needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â legitimate authority and protection from foreign encroachment â is contested as either live (partnership still needed) or dead (colonial consolidation complete). The constraint persists in a tangled state because genuine coordination (settlement redress, co-management) operates through the same institutional machinery that historically executed extraction (Native Land Court, Crown purchasing). Classification as tangled_rope prevents misreading the modern partnership as pure coordination (ignoring historical extraction) or pure extraction (ignoring the genuine consent function).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary,
    'Does the rangatiratanga retention reading instantiate a structurally distinct constraint from the Crown cession reading, or do they operate as rhetorical poles within a single continuum of Crown-MÄori relations?',
    'Analyze whether the two readings produce divergent structural predictions for Crown legislative authority over MÄori resources.',
    'If distinct, the kernel is genuinely split and the rangatiratanga reading''s partnership rope is separable from the cession snare; if rhetorical, the constraint is a single arrangement with high theater_ratio masking persistent extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether the sibling readings are structurally distinct or rhetorical variations.').

omega_variable(
    translation_asymmetry_intent,
    'Was the MÄori-English textual divergence a deliberate extraction mechanism or a good-faith communication failure?',
    'Historical archival research into the drafting intentions of Hobson''s translators and Busby''s notes.',
    'If deliberate, the Crown reading is a snare by design; if good-faith, extraction emerged from structural power asymmetry and institutional drift.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(translation_asymmetry_intent, empirical, 'Deliberate vs emergent extraction via translation asymmetry.').

omega_variable(
    ongoing_consent_operationalization,
    'Can ongoing hapÅ« consent be operationalized in a way that is neither purely performative nor veto-power paralysis?',
    'Comparative analysis of partnership models in treaty jurisdictions and resource management practice.',
    'If inoperable without Crown unilateralism, the coordination function is theatrical and the constraint trends toward piton or snare; if operable, the rope aspect strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ongoing_consent_operationalization, conceptual, 'Whether consent requirement is substantive or theatrical.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(treaty_authority_cession__rangatiratanga_retention_reading, 0, 180).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(treaty_auth_rangat_tr_t0, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(treaty_auth_rangat_tr_t30, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 30, 0.15).
narrative_ontology:measurement(treaty_auth_rangat_tr_t60, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 60, 0.25).
narrative_ontology:measurement(treaty_auth_rangat_tr_t90, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 90, 0.3).
narrative_ontology:measurement(treaty_auth_rangat_tr_t120, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 120, 0.35).
narrative_ontology:measurement(treaty_auth_rangat_tr_t150, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 150, 0.42).
narrative_ontology:measurement(treaty_auth_rangat_tr_t180, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 180, 0.48).

% Extraction over time
narrative_ontology:measurement(treaty_auth_rangat_be_t0, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(treaty_auth_rangat_be_t30, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 30, 0.75).
narrative_ontology:measurement(treaty_auth_rangat_be_t60, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 60, 0.88).
narrative_ontology:measurement(treaty_auth_rangat_be_t90, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 90, 0.85).
narrative_ontology:measurement(treaty_auth_rangat_be_t120, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 120, 0.78).
narrative_ontology:measurement(treaty_auth_rangat_be_t150, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 150, 0.7).
narrative_ontology:measurement(treaty_auth_rangat_be_t180, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 180, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(treaty_auth_rangat_su_t0, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(treaty_auth_rangat_su_t30, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 30, 0.85).
narrative_ontology:measurement(treaty_auth_rangat_su_t60, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 60, 0.95).
narrative_ontology:measurement(treaty_auth_rangat_su_t90, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 90, 0.92).
narrative_ontology:measurement(treaty_auth_rangat_su_t120, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 120, 0.82).
narrative_ontology:measurement(treaty_auth_rangat_su_t150, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 150, 0.68).
narrative_ontology:measurement(treaty_auth_rangat_su_t180, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 180, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(treaty_authority_cession__rangatiratanga_retention_reading, resource_allocation).
narrative_ontology:affects_constraint(treaty_authority_cession__rangatiratanga_retention_reading, crown_cession_reading).
narrative_ontology:affects_constraint(treaty_authority_cession__rangatiratanga_retention_reading, biculturalism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the treaty_authority_cession kernel. It is structurally paired with crown_cession_reading (the English-text sovereignty cession reading) and biculturalism_reading (the managerial partnership reading). The Îµ values differ because the referent â the treaty arrangement â is assessed by each reading's own lights: this reading sees partnership with retained sovereignty; the Crown reading sees completed cession.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
