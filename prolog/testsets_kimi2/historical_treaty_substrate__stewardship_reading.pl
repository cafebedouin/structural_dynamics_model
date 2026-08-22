% ============================================================================
% CONSTRAINT STORY: historical_treaty_substrate__stewardship_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_historical_treaty_substrate__stewardship_reading, []).

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
 *   constraint_id: historical_treaty_substrate__stewardship_reading
 *   human_readable: Historical Treaty Substrate â Stewardship Reading
 *   domain: legal_anthropology/indigenous_law/comparative_constitutional_theory
 *
 * SUMMARY:
 *   This is the stewardship reading of the contested
 *   historical_treaty_substrate kernel. It frames historical treaties as
 *   relational pacts creating mutual obligations of territorial stewardship,
 *   explicitly rejecting sovereign cession. Indigenous nations remain in the
 *   beneficiary set for territorial jurisdiction; the settler state enters
 *   the obligation set for consent and shared governance. The constraint
 *   coordinates coexistence but structurally extracts from the settler
 *   state's unilateral sovereignty. Claimed as tangled_rope because the
 *   coordination function (shared stewardship, peace) is genuine and
 *   necessary, while the standing arrangement also imposes ongoing
 *   sovereignty costs on the settler state that it persistently resists. The
 *   authored metrics and claimed type are independent: the metrics capture
 *   the descriptive reality of evasion, theater, and resistance, while the
 *   structural claim asserts the coordination/extraction hybrid.
 *
 * KEY AGENTS:
 *   - Indigenous nations: Primary beneficiary (organized/identity_locked) â collect recognized jurisdiction and consent authority
 *   - Settler state: Primary target (institutional/constrained) â bears obligations of shared governance and consent-seeking
 *   - Indigenous governance councils: Agenda setter (organized/identity_locked) â administers stewardship protocols and treaty enforcement
 *   - Resource extraction sector: Excluded actor (powerful/constrained) â blocked from unilateral access, lobbies against obligations
 *   - Comparative legal scholars: Analytical observer (analytical) â assesses historical intent and structural fit of readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(historical_treaty_substrate__stewardship_reading, 0.58).
domain_priors:suppression_score(historical_treaty_substrate__stewardship_reading, 0.68).
domain_priors:theater_ratio(historical_treaty_substrate__stewardship_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(historical_treaty_substrate__stewardship_reading, tangled_rope).
narrative_ontology:human_readable(historical_treaty_substrate__stewardship_reading, "Historical Treaty Substrate â Stewardship Reading").
narrative_ontology:topic_domain(historical_treaty_substrate__stewardship_reading, "legal_anthropology/indigenous_law/comparative_constitutional_theory").

domain_priors:requires_active_enforcement(historical_treaty_substrate__stewardship_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(historical_treaty_substrate__stewardship_reading, 'd51ebaed-2814-4e81-90c4-0429543f2aa2').
narrative_ontology:cs_kernel_codification('d51ebaed-2814-4e81-90c4-0429543f2aa2', fixed_text).
narrative_ontology:cs_authority_grounding('d51ebaed-2814-4e81-90c4-0429543f2aa2', lineage).
narrative_ontology:cs_interpretation_layer_present('d51ebaed-2814-4e81-90c4-0429543f2aa2').
narrative_ontology:cs_reading_relation('d51ebaed-2814-4e81-90c4-0429543f2aa2', historical_treaty_substrate__extinguishment_reading, forecloses).
narrative_ontology:cs_reading_relation('d51ebaed-2814-4e81-90c4-0429543f2aa2', historical_treaty_substrate__nation_to_nation_reading, coexists_with).
narrative_ontology:cs_axiom('d51ebaed-2814-4e81-90c4-0429543f2aa2', foundational, no_cession_of_indigenous_sovereignty).
narrative_ontology:cs_axiom_status(no_cession_of_indigenous_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('d51ebaed-2814-4e81-90c4-0429543f2aa2', no_cession_of_indigenous_sovereignty, deontological).
narrative_ontology:cs_axiom('d51ebaed-2814-4e81-90c4-0429543f2aa2', foundational, mutual_obligation_of_shared_stewardship).
narrative_ontology:cs_axiom_status(mutual_obligation_of_shared_stewardship, holdable).
narrative_ontology:cs_axiom_grounding('d51ebaed-2814-4e81-90c4-0429543f2aa2', mutual_obligation_of_shared_stewardship, conventional).
narrative_ontology:cs_reference_frame('d51ebaed-2814-4e81-90c4-0429543f2aa2', original_coexistence_compact).
narrative_ontology:cs_drift_state('d51ebaed-2814-4e81-90c4-0429543f2aa2', contemporary_settler_legal_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('d51ebaed-2814-4e81-90c4-0429543f2aa2', '').
narrative_ontology:cs_kernel_id(historical_treaty_substrate__stewardship_reading, historical_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__stewardship_reading, indigenous_nations).
narrative_ontology:constraint_victim(historical_treaty_substrate__stewardship_reading, settler_state).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold treaty rights to territorial jurisdiction and authority over resource use under a stewardship framework. Must engage in ongoing legal and political enforcement to hold the settler state to consent and coexistence obligations. Exit from the treaty relationship is identity-locked because the treaties constitute their ongoing nationhood and relationship to the territory.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, indigenous_nations, beneficiary,
    organized, generational, identity_locked, regional).

% Bears obligations under the treaty to obtain consent for resource use and share territorial governance. Historically has resisted these obligations and defaulted to unilateral extraction and extinguishment doctrines. Constrained from outright exit because treaty legitimacy underpins its own territorial authority, but seeks to reinterpret the constraint to minimize sovereignty costs.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, settler_state, payer,
    institutional, generational, constrained, national).

% Administer Indigenous treaty protocols, stewardship laws, and territorial governance from the Indigenous side. Set the agenda for treaty implementation, resource monitoring, and consent processes. Their authority derives from Indigenous legal traditions and the treaty relationship itself.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, indigenous_governance_councils, agenda_setter,
    organized, generational, identity_locked, regional).

% Would prefer unilateral access to territorial resources through extinguishment or state licensing. Are structurally excluded from the stewardship framework because their business model depends on bypassing Indigenous consent. Actively lobby the settler state to weaken treaty obligations.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, resource_extraction_sector, excluded,
    powerful, biographical, constrained, national).

% Analyze treaty texts, oral histories, and legal doctrines to assess whether the stewardship reading or extinguishment reading better matches historical intent and current practice. Provide analytical framing without direct stake in the resource outcomes.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, comparative_legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes protocols for peaceful coexistence and shared territorial stewardship between Indigenous nations and settler populations, replacing violent conflict and unilateral displacement with ongoing mutual obligations of care, consent, and resource management.
% TRANSFER_FUNCTION: Moves governance authority over territory and resources from unilateral settler control to joint Indigenous-settler stewardship; transfers obligations of ongoing consent-seeking and ecological care onto the settler state and its successors.
% ABSENT_VOICES: Resource extraction industries and colonial administrators advocating for extinguishment are structurally excluded from the stewardship framework; future generations of both parties are not directly represented in the original pact but are affected by its implementation.
% DISAPPEARANCE_RATIONALE: If the stewardship pact vanished, territorial governance would revert to either unilateral settler extraction under extinguishment logic or protracted conflict over exclusive jurisdiction; the entire legal and political architecture of coexistence would collapse.
% FOUNDING_PROBLEM: How to establish peaceful, ongoing coexistence and resource-sharing between incoming settler populations and Indigenous nations who maintain prior and continuing jurisdiction over the territory, without resorting to war or displacement.
% FOUNDING_PROBLEM_CORROBORATION: Indigenous oral histories, wampum belts, and treaty elders attest to the mutual intent from within the beneficiary set. Independent legal historians and some settler jurists corroborate the stewardship framing from outside the directly benefiting Indigenous parties, though the settler state itself generally denies this genealogy in favor of extinguishment.
narrative_ontology:disappearance_verdict(historical_treaty_substrate__stewardship_reading, world_rearranges).
narrative_ontology:founding_problem_status(historical_treaty_substrate__stewardship_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(historical_treaty_substrate__stewardship_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(historical_treaty_substrate__stewardship_reading, 'none', 1).
narrative_ontology:epsilon_provenance(historical_treaty_substrate__stewardship_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(historical_treaty_substrate__stewardship_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(historical_treaty_substrate__stewardship_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(historical_treaty_substrate__stewardship_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.58) because the constraint reallocates substantial sovereignty and resource decision-making from the settler state to joint stewardship. Suppression is high (0.68) because the arrangement must actively suppress the extinguishment alternative and unilateral extraction to persist, even as the settler state deploys its own legal apparatus to resist. Theater ratio (0.42) reflects widespread performative consultation and reconciliation rhetoric without corresponding transfer of decision-making power. Accessibility collapse (0.55) captures how the treaty's existence as the governing legal framework partially collapses alternatives such as full Indigenous territorial sovereignty or outright settler extinguishment. Resistance (0.62) is high from both Indigenous nations defending the stewardship framework and the settler state seeking to minimize its obligations. The temporal series show extraction rising as the settler state defaulted on obligations during expansion, dipping slightly during the modern rights-recognition era, and rising again under contemporary resource-extraction pressure.
 *
 * PERSPECTIVAL GAP:
 *   The Indigenous beneficiary seat and the settler-state payer seat compute differently: from the Indigenous position the treaty is a living source of jurisdiction and a bulwark against extinguishment, while from the settler-state position the same text operates as an ongoing sovereignty cost that its institutions seek to minimize through legal evasion and theater. The engine computes this divergence from the structural data rather than from any authored type reconciliation.
 *
 * DIRECTIONALITY LOGIC:
 *   Indigenous nations are declared beneficiaries with identity-locked exit, yielding low directionality (d near 0.0) and damped effective extraction; the constraint subsidizes their jurisdictional position. The settler state is declared victim/payer with constrained exit, yielding high directionality (d near 1.0) and amplified effective extraction; the constraint extracts from its unilateral sovereignty. Resource extraction industries are excluded and do not feed the beneficiary/victim derivation. The asymmetry is intentional: the stewardship reading inverts the extinguishment reading's directionality structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The stewardship reading prevents mislabeling the treaty substrate as pure extraction (snare) by preserving the genuine coordination function: without the treaty, the historical record suggests violent displacement rather than coexistence. It also prevents mislabeling it as pure coordination (rope) by acknowledging that the settler state experiences the arrangement as a sovereignty cost and actively resists it. The temporal measurements show theater rising over time, indicating that later phases of the arrangement drifted toward performative maintenance of a coordination function that was no longer fully operative, but the core structural claim remains tangled_rope because the coordination and extraction are inseparable in the same legal framework.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    descriptive_bearer_ambiguity,
    'Does the cost of the stewardship arrangement fall descriptively on the settler state (sovereignty constraints) or on Indigenous nations (enforcement burdens and continued territorial loss due to settler non-compliance)?',
    'Comparative analysis of legal expenditure, land-use data, and health or economic outcomes across treaty territories: if Indigenous nations bear disproportionate costs relative to the sovereignty constraints on the settler state, the descriptive victim seat differs from the normative structure.',
    'If Indigenous nations are the descriptive bearers, the directionality vector inverts for that seat and the constraint computes closer to snare for Indigenous nations despite the stewardship reading''s beneficiary declaration; if the settler state bears the costs, the authored structure holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(descriptive_bearer_ambiguity, empirical, 'Whether extraction costs fall on the obligated party or the rights-holding party').

omega_variable(
    kernel_decomposition_risk,
    'Does the stewardship reading conflate two structurally distinct sub-constraints â one governing territorial jurisdiction and another governing resource consent â that should be modeled separately under epsilon-invariance?',
    'Separate measurement of jurisdictional-recognition claims versus resource-consent claims within the same treaty corpus: if the two show different epsilon profiles, split into stewardship_jurisdiction and stewardship_resource stories.',
    'If the kernel decomposes, this story''s single epsilon conflates two constraints and should be split; if unified, the current single-constraint model stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_decomposition_risk, conceptual, 'Epsilon-invariance check for sub-constraint decomposition within the stewardship reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(historical_treaty_substrate__stewardship_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hist_tr_t0, historical_treaty_substrate__stewardship_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(hist_tr_t30, historical_treaty_substrate__stewardship_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement(hist_tr_t60, historical_treaty_substrate__stewardship_reading, theater_ratio, 60, 0.35).
narrative_ontology:measurement(hist_tr_t90, historical_treaty_substrate__stewardship_reading, theater_ratio, 90, 0.4).
narrative_ontology:measurement(hist_tr_t120, historical_treaty_substrate__stewardship_reading, theater_ratio, 120, 0.46).
narrative_ontology:measurement(hist_tr_t150, historical_treaty_substrate__stewardship_reading, theater_ratio, 150, 0.52).

% Extraction over time
narrative_ontology:measurement(hist_be_t0, historical_treaty_substrate__stewardship_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(hist_be_t30, historical_treaty_substrate__stewardship_reading, base_extractiveness, 30, 0.48).
narrative_ontology:measurement(hist_be_t60, historical_treaty_substrate__stewardship_reading, base_extractiveness, 60, 0.6).
narrative_ontology:measurement(hist_be_t90, historical_treaty_substrate__stewardship_reading, base_extractiveness, 90, 0.55).
narrative_ontology:measurement(hist_be_t120, historical_treaty_substrate__stewardship_reading, base_extractiveness, 120, 0.62).
narrative_ontology:measurement(hist_be_t150, historical_treaty_substrate__stewardship_reading, base_extractiveness, 150, 0.65).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(historical_treaty_substrate__stewardship_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(historical_treaty_substrate__stewardship_reading, historical_treaty_substrate__extinguishment_reading).
narrative_ontology:affects_constraint(historical_treaty_substrate__stewardship_reading, historical_treaty_substrate__nation_to_nation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the historical_treaty_substrate kernel, which decomposes into three structurally distinct claims: extinguishment (transactional cession), nation_to_nation (sovereign equality), and stewardship (relational coexistence). Each reading carries a distinct epsilon and beneficiary/victim structure. The stewardship reading shares the referent (treaty texts and practices) but authors a low-directionality seat for Indigenous nations and a high-directionality seat for the settler state, inverting the extinguishment reading's structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
