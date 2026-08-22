% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_commitment__syncretic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_commitment__syncretic_reading, []).

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
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: shinbutsu_ontological_commitment__syncretic_reading
 *   human_readable: Honji-Suijaku Syncretic Ontological Commitment
 *   domain: religious_studies/japanese_history/ontology
 *
 * SUMMARY:
 *   This constraint story models the syncretic reading of the shinbutsu
 *   ontological commitment kernel: the medieval Japanese doctrinal framework
 *   of honji-suijaku, which interpreted kami as local manifestations
 *   (suijaku) of universal Buddhist original natures (honji). From this
 *   reading, the arrangement is a genuine cosmological discovery integrating
 *   two ritual systems into one sacred order. Structurally, however, it
 *   operated as an asymmetric hierarchy: Buddhist temples administered the
 *   mappings, received combined patronage, and held doctrinal supremacy,
 *   while Shinto shrine priests were subordinated to temple administration
 *   and stripped of independent ontological authority. Sibling readings
 *   include the partition_reading (Shinto and Buddhism as separate functional
 *   domains) and the incoherence_reading (no stable ontological commitment,
 *   only institutionally tolerated incoherence).
 *
 * KEY AGENTS:
 *   - buddhist_institutional_hierarchy: agenda_setter and beneficiary (institutional power, mobile exit) â authors and administers the doctrine, captures patronage and land
 *   - shinto_priesthood: primary payer (moderate power, identity_locked exit) â bears ontological subordination and loss of institutional autonomy
 *   - syncretic_worship_communities: coordinated beneficiary (moderate power, constrained exit) â receives unified ritual access at cost of subordinating local kami to distant Buddhist authority
 *   - pure_shinto_advocates: excluded (powerless, trapped) â would assert independent kami sovereignty but are marginalized
 *   - religious_studies_analysts: observer (analytical seat) â modern scholarly analysis outside historical power relations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_commitment__syncretic_reading, 0.65).
domain_priors:suppression_score(shinbutsu_ontological_commitment__syncretic_reading, 0.58).
domain_priors:theater_ratio(shinbutsu_ontological_commitment__syncretic_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_commitment__syncretic_reading, tangled_rope).
narrative_ontology:human_readable(shinbutsu_ontological_commitment__syncretic_reading, "Honji-Suijaku Syncretic Ontological Commitment").
narrative_ontology:topic_domain(shinbutsu_ontological_commitment__syncretic_reading, "religious_studies/japanese_history/ontology").

domain_priors:requires_active_enforcement(shinbutsu_ontological_commitment__syncretic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_commitment__syncretic_reading, 'a0e8dfde-fa9f-4628-b958-638dd20f0e78').
narrative_ontology:cs_kernel_codification('a0e8dfde-fa9f-4628-b958-638dd20f0e78', fixed_text).
narrative_ontology:cs_authority_grounding('a0e8dfde-fa9f-4628-b958-638dd20f0e78', lineage).
narrative_ontology:cs_interpretation_layer_present('a0e8dfde-fa9f-4628-b958-638dd20f0e78').
narrative_ontology:cs_reading_relation('a0e8dfde-fa9f-4628-b958-638dd20f0e78', shinbutsu_ontological_commitment__partition_reading, coexists_with).
narrative_ontology:cs_reading_relation('a0e8dfde-fa9f-4628-b958-638dd20f0e78', shinbutsu_ontological_commitment__incoherence_reading, coexists_with).
narrative_ontology:cs_axiom('a0e8dfde-fa9f-4628-b958-638dd20f0e78', foundational, kami_are_suijaku_of_honji).
narrative_ontology:cs_axiom_status(kami_are_suijaku_of_honji, holdable).
narrative_ontology:cs_axiom_grounding('a0e8dfde-fa9f-4628-b958-638dd20f0e78', kami_are_suijaku_of_honji, theological).
narrative_ontology:cs_axiom('a0e8dfde-fa9f-4628-b958-638dd20f0e78', foundational, buddhist_dharma_supersedes_local_cult).
narrative_ontology:cs_axiom_status(buddhist_dharma_supersedes_local_cult, holdable).
narrative_ontology:cs_axiom_grounding('a0e8dfde-fa9f-4628-b958-638dd20f0e78', buddhist_dharma_supersedes_local_cult, theological).
narrative_ontology:cs_reference_frame('a0e8dfde-fa9f-4628-b958-638dd20f0e78', honji_suijaku_orthodoxy).
narrative_ontology:cs_drift_state('a0e8dfde-fa9f-4628-b958-638dd20f0e78', shinbutsu_bunri_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('a0e8dfde-fa9f-4628-b958-638dd20f0e78', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_commitment__syncretic_reading, shinbutsu_ontological_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__syncretic_reading, buddhist_institutional_hierarchy).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__syncretic_reading, shinto_priesthood).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__syncretic_reading, syncretic_worship_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the honji-suijaku doctrinal framework, assigns specific buddhas as original natures to specific kami, manages combined shrine-temple complexes, and receives land, patronage, and ritual authority from the integrated system.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, buddhist_institutional_hierarchy, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_commitment__syncretic_reading, buddhist_institutional_hierarchy, beneficiary).

% Performs kami rituals within a religious hierarchy that explicitly subordinates kami to buddhas. Their shrines are often administratively controlled by Buddhist temples, and asserting an independent ontological status for kami risks loss of institutional standing and patronage.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, shinto_priesthood, payer,
    moderate, generational, identity_locked, national).

% Engage in ritual practices that move between kami shrines and buddha halls, benefiting from a unified sacred geography and shared cosmological narrative. They do not typically question which cosmology is supreme and have few alternative frameworks available within their local ritual economy.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, syncretic_worship_communities, beneficiary,
    moderate, biographical, constrained, national).

% Theologians and practitioners who would assert that kami are independent sovereign beings with no need of Buddhist origination. Their views are marginalized in institutional theology and lack court or state recognition during the period of syncretic dominance.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, pure_shinto_advocates, excluded,
    powerless, generational, trapped, national).

% Modern scholars who analyze the honji-suijaku system as a historical structure of doctrinal integration and institutional hierarchy, operating outside the constraint's historical power relations.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, religious_studies_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_ontological_commitment__syncretic_reading, buddhist_institutional_hierarchy).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Integrates Buddhist and Shinto ritual and cosmological systems into a single hierarchical framework, enabling shared sacred geography, combined worship, and reducing doctrinal conflict across the Japanese archipelago.
% TRANSFER_FUNCTION: Moves ontological priority, ritual authority, land, and patronage from independent kami cults to Buddhist temple institutions; transfers doctrinal subordination to Shinto shrine priests.
% ABSENT_VOICES: Shinto theologians asserting fully independent kami ontology; local cult practitioners whose rituals fall outside the orthodox honji-suijaku mappings; women and marginalized ritualists excluded from the Buddhist doctrinal interpretive layer.
% DISAPPEARANCE_RATIONALE: If the ontological commitment vanished, the administrative and ritual integration of shrines and temples would collapse, patronage and land arrangements would separate along Buddhist and Shinto lines, and worship communities would face a fragmented sacred landscape without the coordinating cosmological narrative.
% FOUNDING_PROBLEM: How to reconcile the introduction of Buddhist universalist soteriology with pre-existing local kami worship, preventing religious conflict and constructing a unified sacred order in the Japanese islands.
% FOUNDING_PROBLEM_CORROBORATION: Imperial court chronicles and early Shinto shrine records from outside the Buddhist beneficiary group attest that the syncretic framework reduced inter-religious violence and enabled combined ritual administration, corroborating the coordination function, though these same sources also document the subordination of shrines to temples.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_commitment__syncretic_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_commitment__syncretic_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_commitment__syncretic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(shinbutsu_ontological_commitment__syncretic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_commitment__syncretic_reading, 0.65, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_commitment__syncretic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(shinbutsu_ontological_commitment__syncretic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(shinbutsu_ontological_commitment__syncretic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) is substantial because the honji-suijaku framework transferred ontological priority, ritual authority, and economic patronage to Buddhist institutions. Suppression (0.58) reflects the structural subordination of shrines to temples and the exclusion of non-syncretic kami theology. Theater_ratio (0.55) is moderate-to-high because the later phase involved elaborate doctrinal distinctions and mapping exercises that served institutional hierarchy as much as soteriological need. Accessibility_collapse (0.65) captures how independent Shinto ontology became nearly unthinkable within the syncretic frame. Resistance (0.52) reflects periodic shrine-line assertions of autonomy and the eventual Meiji-state repudiation. The metrics are authored independently of the claimed type; the syncretic reading's theological conviction does not alter the structural extraction profile.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter/beneficiary seat experiences the constraint as doctrinal truth and necessary cosmological order; the payer seat experiences it as institutional subordination dressed in metaphysical language. The engine computes this divergence from the structural asymmetry in exit options and power: the Buddhist hierarchy could modify specific mappings without self-harm, whereas Shinto priests could not reject the framework without fracturing their professional identity.
 *
 * DIRECTIONALITY LOGIC:
 *   The Buddhist institutional hierarchy sits near the beneficiary end (low d): it authored and administered the doctrine, controlled shrine-temple complexes, and captured patronage flows. Its exit is mobile because it could adjust specific honji-suijaku pairings without threatening its own supremacy. The Shinto priesthood sits near the target end (high d): its identity is fused with kami ritual within a Buddhist superordinate frame, making exit identity_locked and amplifying effective extraction. Worship communities sit near symmetric: they receive genuine coordination benefits (unified ritual economy) while paying diffuse costs (subordination of local kami).
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying as tangled_rope rather than snare preserves the genuine coordination function: honji-suijaku reduced inter-religious conflict and enabled a shared sacred geography that neither tradition could produce alone in the Japanese context. A snare classification would erase this and treat the theology as pure extraction cover. The asymmetric beneficiary structure (Buddhist hierarchy benefits, Shinto autonomy suppressed) prevents classification as pure rope. Temporal measurements show extraction accumulating and theater rising over the interval, consistent with a coordination mechanism gradually captured by its dominant party.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    shinto_subordination_internalization,
    'Was the suppression of Shinto autonomy in the honji-suijaku system primarily structural (temple control of shrine administration and land) or internalized (Shinto priests adopting Buddhist soteriology as their own theological commitment)?',
    'Archival analysis of shrine-priest writings and ritual manuals: if priests independently reproduced Buddhist ontological frames without temple coercion, suppression is substantially internalized.',
    'If internalized, effective suppression exceeds the structural measure and the constraint operated partly through identity fusion rather than external enforcement alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(shinto_subordination_internalization, empirical, 'Structural vs internalized suppression of Shinto autonomy').

omega_variable(
    syncretic_kernel_foreclosure,
    'Does the syncretic reading''s assertion of a stable ontological unity foreclose the incoherence reading, or can both readings coexist as descriptions of different historical moments or institutional scales?',
    'Microhistorical study of regional shrine-temple relations: if some regions show stable syncretic doctrine while others show ad hoc incoherence, the readings describe different scales rather than mutually exclusive histories.',
    'If the readings are scale-dependent rather than contradictory, the kernel decomposes into multiple constraints by region or period rather than by competing historiographical commitment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(syncretic_kernel_foreclosure, conceptual, 'Whether syncretic and incoherence readings are mutually exclusive or scale-dependent').

omega_variable(
    honji_suijaku_coordination_genuine,
    'Is the cosmological coordination produced by honji-suijaku separable from the hierarchical extraction it enabled, or was the integration itself constitutionally extractive?',
    'Comparative analysis with other religious syncretisms: if asymmetric hierarchy is a necessary feature of such integrations, the coordination and extraction are structurally inseparable.',
    'If inseparable, the constraint''s coordination function cannot be redeemed without its extraction component, supporting a more pessimistic classification boundary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(honji_suijaku_coordination_genuine, conceptual, 'Whether coordination and extraction are separable in this syncretic system').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_commitment__syncretic_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t0, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(shin_tr_t5, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 5, 0.25).
narrative_ontology:measurement(shin_tr_t10, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(shin_tr_t15, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement(shin_tr_t20, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 20, 0.45).
narrative_ontology:measurement(shin_tr_t25, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 25, 0.5).
narrative_ontology:measurement(shin_tr_t30, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 30, 0.55).

% Extraction over time
narrative_ontology:measurement(shin_be_t0, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(shin_be_t5, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(shin_be_t10, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(shin_be_t15, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 15, 0.55).
narrative_ontology:measurement(shin_be_t20, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(shin_be_t25, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 25, 0.62).
narrative_ontology:measurement(shin_be_t30, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 30, 0.65).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(shinbutsu_ontological_commitment__syncretic_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


% DUAL FORMULATION NOTE:
% This constraint is the syncretic reading of the shinbutsu ontological commitment kernel; sibling readings (partition_reading, incoherence_reading) model the same historical phenomenon under different structural framings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
