% ============================================================================
% CONSTRAINT STORY: shinbutsu_coexistence_commitment__incoherent_bundle_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_coexistence_commitment__incoherent_bundle_reading, []).

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
 *   constraint_id: shinbutsu_coexistence_commitment__incoherent_bundle_reading
 *   human_readable: Shinbutsu-shugo as Incoherent Bundle
 *   domain: religious studies/historical
 *
 * SUMMARY:
 *   The constraint under examination is the historical arrangement of
 *   shinbutsu-shugo in pre-modern Japan, interpreted here not as a coherent
 *   religious synthesis or a clean domain partition, but as an incoherent
 *   bundle of practices, doctrines, and property relations held together by
 *   deliberate ambiguity and Buddhist institutional power. Under this
 *   reading, the 'coexistence' of kami and buddhas was not a stable
 *   ontological achievement but a suppressed contradiction: Buddhist temples
 *   controlled shrine lands and appointments, shrine priests performed
 *   Buddhist rites under temple supervision, and doctrinal questions that
 *   would expose the incoherence were systematically avoided. The arrangement
 *   collapsed rapidly under Meiji-period state pressure (shinbutsu bunri),
 *   revealing that its persistence had depended on active suppression of
 *   categorical clarity rather than on mutual coordination or shared belief.
 *   This is the incoherent-bundle reading of the
 *   shinbutsu_coexistence_commitment kernel.
 *
 * KEY AGENTS:
 *   - buddhist_temple_elite: Primary beneficiary/agenda-setter (institutional/arbitrage) â extracts land, status, and ritual subordination from shrines.
 *   - shrine_priests: Primary target (moderate/constrained) â bears theological and administrative subordination.
 *   - lay_worship_communities: Secondary target (powerless/identity_locked) â bears diffuse costs of doctrinal obscurity.
 *   - syncretic_theologians: Secondary beneficiary (moderate/mobile) â provides intellectual cover, collects patronage.
 *   - kami_cult_practitioners: Excluded voice (powerless/trapped) â pure kami worshippers excluded from institutional discourse.
 *   - meiji_state_actors: External observer/analytical seat (institutional/analytical) â ultimately collapses the bundle.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0.82).
domain_priors:suppression_score(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0.78).
domain_priors:theater_ratio(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__incoherent_bundle_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_coexistence_commitment__incoherent_bundle_reading, snare).
narrative_ontology:human_readable(shinbutsu_coexistence_commitment__incoherent_bundle_reading, "Shinbutsu-shugo as Incoherent Bundle").
narrative_ontology:topic_domain(shinbutsu_coexistence_commitment__incoherent_bundle_reading, "religious studies/historical").

domain_priors:requires_active_enforcement(shinbutsu_coexistence_commitment__incoherent_bundle_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_coexistence_commitment__incoherent_bundle_reading, '1a15a673-da2f-4bcc-b030-1eb78873ca1d').
narrative_ontology:cs_kernel_codification('1a15a673-da2f-4bcc-b030-1eb78873ca1d', implicit).
narrative_ontology:cs_authority_grounding('1a15a673-da2f-4bcc-b030-1eb78873ca1d', extraction).
narrative_ontology:cs_interpretation_layer_present('1a15a673-da2f-4bcc-b030-1eb78873ca1d').
narrative_ontology:cs_reading_relation('1a15a673-da2f-4bcc-b030-1eb78873ca1d', shinbutsu_coexistence_commitment__syncretic_fusion_reading, forecloses).
narrative_ontology:cs_reading_relation('1a15a673-da2f-4bcc-b030-1eb78873ca1d', shinbutsu_coexistence_commitment__domain_partition_reading, forecloses).
narrative_ontology:cs_axiom('1a15a673-da2f-4bcc-b030-1eb78873ca1d', foundational, incoherence_not_syncretism).
narrative_ontology:cs_axiom_status(incoherence_not_syncretism, holdable).
narrative_ontology:cs_axiom_grounding('1a15a673-da2f-4bcc-b030-1eb78873ca1d', incoherence_not_syncretism, empirically_contingent).
narrative_ontology:cs_axiom('1a15a673-da2f-4bcc-b030-1eb78873ca1d', foundational, power_sustains_bundle).
narrative_ontology:cs_axiom_status(power_sustains_bundle, holdable).
narrative_ontology:cs_axiom_grounding('1a15a673-da2f-4bcc-b030-1eb78873ca1d', power_sustains_bundle, empirically_contingent).
narrative_ontology:cs_reference_frame('1a15a673-da2f-4bcc-b030-1eb78873ca1d', institutional_ambiguity_equilibrium).
narrative_ontology:cs_drift_state('1a15a673-da2f-4bcc-b030-1eb78873ca1d', meiji_reform_period, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('1a15a673-da2f-4bcc-b030-1eb78873ca1d', '').
narrative_ontology:cs_kernel_id(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shinbutsu_coexistence_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__incoherent_bundle_reading, buddhist_temple_elite).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__incoherent_bundle_reading, syncretic_theologians).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shrine_priests).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__incoherent_bundle_reading, lay_worship_communities).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__incoherent_bundle_reading, kami_cult_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administered shrine-temple complexes, controlled shrine land and priest appointments, and propagated doctrines that subordinated kami to Buddhist cosmology. Collected material and symbolic rents from shrines while preventing categorical clarification of the relationship between kami and buddhas.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, buddhist_temple_elite, agenda_setter,
    institutional, generational, arbitrage, national).

% Performed rites under Buddhist supervision, saw their kami reinterpreted as Buddhist manifestations or protectors, and depended on temple patronage for income and status. Independent shrine administration was institutionally blocked, and open assertion of kami autonomy risked loss of livelihood and legitimacy.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, shrine_priests, payer,
    moderate, biographical, constrained, regional).

% Participated in mixed shrine-temple festivals and burial rites where doctrinal distinctions were deliberately obscured. Their local and familial identity was fused with the syncretic ritual calendar, making independent adherence to either pure Buddhism or pure kami worship socially costly and practically unavailable.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, lay_worship_communities, payer,
    powerless, biographical, identity_locked, local).

% Produced honji suijaku literature and related doctrinal work that provided an intellectual veneer for the bundle. Benefited from temple patronage, scholarly status, and the institutional demand for interpretive frameworks that avoided categorical resolution.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, syncretic_theologians, beneficiary,
    moderate, generational, mobile, national).

% Maintained kami-centric practices in remote areas or covertly. Their voices were excluded from the institutional Buddhist-Shinto discourse; they would have objected to the ontological subordination of kami but lacked platforms or protection to contest the arrangement openly.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, kami_cult_practitioners, excluded,
    powerless, biographical, trapped, local).

% Viewed the shinbutsu bundle as an obstruction to imperial state formation and a source of Buddhist institutional power independent of the state. Used centralized authority to forcibly separate shrines and temples, revealing that the arrangement's persistence had depended on suppressing categorical clarity rather than on shared conviction.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__incoherent_bundle_reading, meiji_state_actors, observer,
    institutional, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_coexistence_commitment__incoherent_bundle_reading, buddhist_temple_elite).
narrative_ontology:fixing_cost_class(shinbutsu_coexistence_commitment__incoherent_bundle_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Nominally coordinated Buddhist and Shinto practice into a unified ritual landscape with shared sites and overlapping clergy, allowing communities to address birth, harvest, and death through a single local religious economy.
% TRANSFER_FUNCTION: Transferred land, ritual authority, and theological status from independent shrines and shrine priests to Buddhist temple institutions; moved material and symbolic resources from lay communities to a fused shrine-temple economy that lacked doctrinal accountability.
% ABSENT_VOICES: Pure kami worshippers and shrine priests seeking independence were structurally excluded from doctrinal discourse; their objections were absorbed into the Buddhist interpretive frame or suppressed through administrative subordination.
% DISAPPEARANCE_RATIONALE: Buddhist temples would lose shrine land and patronage networks; shrine priests would regain independent liturgical and administrative status; lay communities would face forced choice between separated traditions; the entire pre-modern religious economy would reorganize around explicit boundaries.
% FOUNDING_PROBLEM: Managing religious pluralism in a society where both kami worship and Buddhism were deeply embedded, and providing a single ritual economy for communities with mixed devotional needs.
% FOUNDING_PROBLEM_CORROBORATION: Meiji reform officials and post-Meiji shrine priests attested that independent shrine administration was viable once Buddhist patronage was removed; modern historians outside the Buddhist institutional tradition corroborate that the pluralism problem was superseded by state-managed separation.
narrative_ontology:disappearance_verdict(shinbutsu_coexistence_commitment__incoherent_bundle_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_coexistence_commitment__incoherent_bundle_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_coexistence_commitment__incoherent_bundle_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__incoherent_bundle_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(shinbutsu_coexistence_commitment__incoherent_bundle_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.82) is high because the arrangement transferred substantial material and symbolic resources from shrines to temples while preventing shrine independence. Suppression (0.78) is high because the bundle's persistence required active avoidance of ontological clarification, institutional control of shrine appointments, and suppression of independent kami cults. Theater ratio (0.65) reflects the growing performative maintenance of 'coexistence' rituals that masked underlying incoherence, especially in the Edo period. Accessibility collapse (0.70) indicates that genuine alternatives (clean separation or coherent fusion) were structurally suppressed, though intellectually imaginable. Resistance (0.45) is moderate: sporadic shrine independence movements and local kami-centrism existed but were largely contained until the Meiji rupture. The temporal series show extraction and theater rising as the Tokugawa peace allowed institutional hardening, peaking just before the Meiji collapse.
 *
 * PERSPECTIVAL GAP:
 *   The temple elite experienced the constraint as natural institutional hierarchy and benevolent patronage; the shrine priesthood experienced it as subordination with constrained exit; lay communities experienced it as an identity-locked tradition where questioning the fusion was socially costly. The Meiji state, operating from an analytical seat with different legitimacy needs, saw only incoherence to be excised. The engine will compute divergent per-seat types from this structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations map directly to the flow of extraction: Buddhist temple elites and their theologians captured shrine resources and theological autonomy; shrine priests and local worship communities paid through subordinated status and obscured religious identity. The directionality derivation will place temple elites near the beneficiary pole (low d) and shrine priests near the target pole (high d); lay communities, though diffuse, sit nearer the target end due to identity-locked exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling the arrangement as a rope or tangled rope by insisting that the coordination story (syncretism as pluralist management) was cover for extraction. The founding problemâmanaging religious pluralismâwas invoked by the benefiting parties but was not genuinely solved by the bundle; the Meiji separation showed that clearer categorical boundaries were always possible. The R5 genealogy (founding_problem_status: dead) corroborates that the constraint persisted beyond its functional justification, but unlike a piton, it was actively maintained by concentrated beneficiaries rather than by mere inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ambiguity_intentionality,
    'Was the incoherence of shinbutsu-shugo a deliberate institutional strategy of Buddhist temple elites, or an emergent byproduct of uncoordinated local religious adaptation?',
    'Archival study of temple administrative records and shrine-temple contracts to determine whether doctrinal ambiguity was enforced top-down or emerged bottom-up.',
    'If deliberate, the constraint is structurally a snare with directed extraction; if emergent, it may reclassify as a tangled rope where coordination and extraction are inseparable byproducts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ambiguity_intentionality, empirical, 'Whether ambiguity was strategically enforced or emergent.').

omega_variable(
    shrine_priest_experience,
    'Did subordinated shrine priests experience shinbutsu-shugo as extractive domination or as legitimate participation in a shared religious economy?',
    'Analysis of Edo-period shrine priest petitions, literary sources, and post-Meiji memoirs for evidence of resistance versus willing compliance.',
    'If compliance was willing and mutually beneficial for most actors, the constraint shifts toward tangled rope; if dominated, it stays snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(shrine_priest_experience, empirical, 'Subjective experience of shrine priests under Buddhist patronage.').

omega_variable(
    kernel_reading_underdetermination,
    'Does the historical evidence underdetermine whether shinbutsu-shugo was a domain partition, a syncretic fusion, or an incoherent bundle, such that the constraint family is irreducibly polysemic?',
    'Comprehensive doctrinal and archival synthesis; if evidence equally supports all three readings, the kernel is structurally underdetermined.',
    'Would validate the decomposition into three constraint stories rather than a single variable constraint; confirms the epsilon-invariance decomposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Historical underdetermination of the kernel''s structural reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_coexistence_commitment__incoherent_bundle_reading, 0, 670).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shinbutsu_incoherent_tr_t0, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(shinbutsu_incoherent_tr_t150, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 150, 0.35).
narrative_ontology:measurement(shinbutsu_incoherent_tr_t300, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 300, 0.4).
narrative_ontology:measurement(shinbutsu_incoherent_tr_t450, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 450, 0.5).
narrative_ontology:measurement(shinbutsu_incoherent_tr_t600, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 600, 0.6).
narrative_ontology:measurement(shinbutsu_incoherent_tr_t670, shinbutsu_coexistence_commitment__incoherent_bundle_reading, theater_ratio, 670, 0.65).

% Extraction over time
narrative_ontology:measurement(shinbutsu_incoherent_be_t0, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(shinbutsu_incoherent_be_t150, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 150, 0.65).
narrative_ontology:measurement(shinbutsu_incoherent_be_t300, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 300, 0.68).
narrative_ontology:measurement(shinbutsu_incoherent_be_t450, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 450, 0.72).
narrative_ontology:measurement(shinbutsu_incoherent_be_t600, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 600, 0.78).
narrative_ontology:measurement(shinbutsu_incoherent_be_t670, shinbutsu_coexistence_commitment__incoherent_bundle_reading, base_extractiveness, 670, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(shinbutsu_incoherent_su_t0, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(shinbutsu_incoherent_su_t150, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 150, 0.55).
narrative_ontology:measurement(shinbutsu_incoherent_su_t300, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 300, 0.6).
narrative_ontology:measurement(shinbutsu_incoherent_su_t450, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 450, 0.7).
narrative_ontology:measurement(shinbutsu_incoherent_su_t600, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 600, 0.75).
narrative_ontology:measurement(shinbutsu_incoherent_su_t670, shinbutsu_coexistence_commitment__incoherent_bundle_reading, suppression_requirement, 670, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
