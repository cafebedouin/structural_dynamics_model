% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_commitment__syncretic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
 *   domain: religious_studies/japanese_history/ontology_of_practice
 *
 * SUMMARY:
 *   This constraint instantiates the syncretic reading of the shinbutsu
 *   ontological commitment kernel: under honji-suijaku metaphysics, kami are
 *   local manifestations (suijaku) of Buddhist buddhas (honji), subsuming
 *   native Japanese cults within a Buddhist cosmological hierarchy.
 *   Historically, this framework produced high institutional integration and
 *   genuine coordination of ritual life, while asymmetrically extracting
 *   doctrinal authority and institutional autonomy from the Shinto priesthood
 *   for the benefit of the Buddhist hierarchy. The claim/metric independence
 *   is maintained: the constraint is claimed as tangled_rope (genuine
 *   coordination plus asymmetric extraction) while metrics are authored
 *   descriptively to reflect the active enforcement and accumulated theater
 *   of centuries of performative maintenance.
 *
 * KEY AGENTS:
 *   - Buddhist institutional hierarchy: agenda-setter and beneficiary (institutional/constrained) â defends and profits from the doctrinal framework
 *   - Shinto priesthood: payer (moderate/identity_locked) â bears subordination and loss of independent doctrinal authority
 *   - Syncretic lay communities: incidental beneficiary (moderate/constrained) â receives coordination benefits but loses access to purely Shinto alternatives
 *   - Shinto revivalists: excluded (moderate/trapped) â voices asserting kami independence are kept out of institutional discourse
 *   - Religious studies scholar: observer (analytical/analytical) â external analytical seat evaluating the structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_commitment__syncretic_reading, 0.72).
domain_priors:suppression_score(shinbutsu_ontological_commitment__syncretic_reading, 0.78).
domain_priors:theater_ratio(shinbutsu_ontological_commitment__syncretic_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__syncretic_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_commitment__syncretic_reading, tangled_rope).
narrative_ontology:human_readable(shinbutsu_ontological_commitment__syncretic_reading, "Honji-Suijaku Syncretic Ontological Commitment").
narrative_ontology:topic_domain(shinbutsu_ontological_commitment__syncretic_reading, "religious_studies/japanese_history/ontology_of_practice").

domain_priors:requires_active_enforcement(shinbutsu_ontological_commitment__syncretic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_commitment__syncretic_reading, '558450b2-1dcb-475a-b3d7-286c2ea41460').
narrative_ontology:cs_kernel_codification('558450b2-1dcb-475a-b3d7-286c2ea41460', formalized).
narrative_ontology:cs_authority_grounding('558450b2-1dcb-475a-b3d7-286c2ea41460', lineage).
narrative_ontology:cs_interpretation_layer_present('558450b2-1dcb-475a-b3d7-286c2ea41460').
narrative_ontology:cs_reading_relation('558450b2-1dcb-475a-b3d7-286c2ea41460', shinbutsu_ontological_commitment__partition_reading, forecloses).
narrative_ontology:cs_reading_relation('558450b2-1dcb-475a-b3d7-286c2ea41460', shinbutsu_ontological_commitment__incoherence_reading, coexists_with).
narrative_ontology:cs_axiom('558450b2-1dcb-475a-b3d7-286c2ea41460', foundational, kami_as_buddhist_trace_manifestations).
narrative_ontology:cs_axiom_status(kami_as_buddhist_trace_manifestations, holdable).
narrative_ontology:cs_axiom_grounding('558450b2-1dcb-475a-b3d7-286c2ea41460', kami_as_buddhist_trace_manifestations, theological).
narrative_ontology:cs_axiom('558450b2-1dcb-475a-b3d7-286c2ea41460', foundational, honji_suijaku_cosmological_exhaustiveness).
narrative_ontology:cs_axiom_status(honji_suijaku_cosmological_exhaustiveness, holdable).
narrative_ontology:cs_axiom_grounding('558450b2-1dcb-475a-b3d7-286c2ea41460', honji_suijaku_cosmological_exhaustiveness, theological).
narrative_ontology:cs_reference_frame('558450b2-1dcb-475a-b3d7-286c2ea41460', integrated_honji_suijaku_cosmology).
narrative_ontology:cs_drift_state('558450b2-1dcb-475a-b3d7-286c2ea41460', meiji_shinbutsu_bunri_era, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('558450b2-1dcb-475a-b3d7-286c2ea41460', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_commitment__syncretic_reading, shinbutsu_ontological_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__syncretic_reading, buddhist_institutional_hierarchy).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__syncretic_reading, syncretic_lay_communities).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__syncretic_reading, shinto_priesthood).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the honji-suijaku doctrinal framework through temple networks and scholastic traditions; claims interpretive authority over all kami worship; receives institutional subordination, land patronage, and ritual precedence from integrated shrines.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, buddhist_institutional_hierarchy, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_commitment__syncretic_reading, buddhist_institutional_hierarchy, beneficiary).

% Performs shrine rituals under Buddhist supervision; their kami are catalogued as local manifestations of buddhas; independent doctrinal authority is denied; leaving the integrated structure means loss of institutional role, patronage, and social standing as kannushi.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, shinto_priesthood, payer,
    moderate, biographical, identity_locked, national).

% Access a unified ritual calendar and shared shrine-temple complexes; receive Buddhist soteriological framework alongside kami propitiation; their religious vocabulary is restructured by Buddhist interpretive authority, limiting purely Shinto alternatives.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, syncretic_lay_communities, beneficiary,
    moderate, biographical, constrained, national).

% Advocate for independent ontological status of kami outside Buddhist cosmology; excluded from institutional discourse and patronage networks during the dominance of honji-suijaku; their position survives only in marginal texts or suppressed local movements.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, shinto_revivalists, excluded,
    moderate, generational, trapped, national).

% Analyzes the historical construction of honji-suijaku as an institutional strategy; evaluates the divergence between doctrinal claims and ritual practice; neither benefits from nor pays into the historical constraint.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__syncretic_reading, religious_studies_scholar, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_ontological_commitment__syncretic_reading, buddhist_institutional_hierarchy).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Integrates two distinct religious traditions into a single cosmological and institutional framework, preventing sectarian conflict and enabling shared ritual, patronage, and shrine-temple complexes.
% TRANSFER_FUNCTION: Moves doctrinal authority and institutional subordination from independent Shinto priesthoods to Buddhist institutional hierarchy; reinterprets native kami as local manifestations of Buddhist buddhas.
% ABSENT_VOICES: Shinto theologians asserting fully independent ontological status for kami, and Buddhist purists rejecting kami veneration as heterodox, are excluded from the institutional conversation; their positions are absent from sanctioned doctrinal discourse.
% DISAPPEARANCE_RATIONALE: If the syncretic ontological commitment vanished, integrated shrine-temple complexes would split into competing authorities, patronage flows would reorganize around separate institutions, and the Buddhist hierarchy would lose its claimed cosmological supremacy over native kami cults.
% FOUNDING_PROBLEM: The coexistence of immigrant Buddhism and native Shinto created ritual conflict, competing patronage claims, and unresolved cosmological contradictions in the Japanese religious landscape.
% FOUNDING_PROBLEM_CORROBORATION: Buddhist institutional chroniclers attest the problem as ongoing heresy and disorder. Modern religious studies scholars outside the beneficiary tradition corroborate that syncretism emerged from institutional competition rather than pure philosophical necessity. Edo-period Shinto revivalist movements attest the problem was suppressed rather than solved.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_commitment__syncretic_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_commitment__syncretic_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_commitment__syncretic_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(shinbutsu_ontological_commitment__syncretic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_commitment__syncretic_reading, 0.72, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.72) is high because the framework systematically transfers cosmological and institutional authority from Shinto to Buddhist institutions. Suppression (0.78) is higher still: the constraint persists only through active doctrinal enforcement, institutional hierarchy, and the exclusion of alternative ontologies. Theater ratio (0.50) reflects that by the end of the interval, much shrine-temple activity was habitual performance of a subordination whose theological vitality had eroded. Accessibility collapse (0.75) captures the difficulty of articulating independent Shinto theology within integrated institutions. Resistance (0.60) acknowledges persistent Shinto counter-movements and later Kokugaku critiques. The measurement series share one time grid to prevent misaligned drift dating.
 *
 * PERSPECTIVAL GAP:
 *   The Buddhist hierarchy experiences the constraint as harmonious integration and legitimate ordering of the religious field; the Shinto priesthood experiences it as ontological capture and institutional subordination. The engine computes this divergence from beneficiary/victim declarations and exit options â the Buddhist seat is subsidized by the constraint, the Shinto seat is structurally targeted.
 *
 * DIRECTIONALITY LOGIC:
 *   The Buddhist institutional hierarchy is declared as beneficiary (low directionality, subsidized by the constraint). The Shinto priesthood is declared as victim (high directionality, targeted extraction). Syncretic lay communities sit nearer symmetric: they collect genuine coordination benefits but bear the hidden cost of lost alternative religious vocabularies. Shinto revivalists are excluded rather than coordinated; their exclusion is the enforcement perimeter.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling the constraint as pure rope (which would ignore the asymmetric extraction of Shinto autonomy) or pure snare (which would deny the genuine coordination function of shared ritual space and reduced sectarian conflict). The R5 genealogy indicates the founding problem was contested, and the constraint persisted beyond the era when its coordination benefits were most vital, accumulating theater and extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    syncretic_kernel_vs_incoherence,
    'Is the honji-suijaku framework a sincerely held systematic metaphysics, or a veneer of coherence over institutional bricolage?',
    'Archaeological and textual analysis comparing doctrinal treatises against regional shrine-temple practice records to assess consistency.',
    'If incoherence is validated, this constraint shifts toward snare (coordination story as cover) or piton (inertial performance of hollowed doctrine).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(syncretic_kernel_vs_incoherence, conceptual, 'Whether the syncretic doctrine was genuine metaphysics or institutional cover.').

omega_variable(
    shinto_autonomy_suppression_nature,
    'Is the suppression of Shinto autonomy achieved through institutional subordination alone, or through internalized identity fusion among Shinto priests?',
    'Examine Edo-period shrine records and priestly correspondence for evidence of resistance versus compliance framed as sincere acceptance of honji-suijaku.',
    'If internalized, effective suppression exceeds structural measures and the Shinto priesthood carries the constraint beyond institutional reach.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(shinto_autonomy_suppression_nature, empirical, 'Structural versus internalized suppression of Shinto autonomy.').

omega_variable(
    buddhist_hierarchy_benefit_concentration,
    'Does the benefit to Buddhist hierarchy reflect necessary coordination overhead, or extractive surplus decoupled from coordination cost?',
    'Comparative analysis of resource flows between temples and shrines before and after syncretic integration, measuring concentration of land and patronage.',
    'High surplus would push classification toward the snare boundary; low surplus would support a purer coordination reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(buddhist_hierarchy_benefit_concentration, empirical, 'Coordination cost versus extractive surplus in Buddhist hierarchy gains.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_commitment__syncretic_reading, 0, 800).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shinbutsu_syncretic_tr_t0, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(shinbutsu_syncretic_tr_t200, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 200, 0.32).
narrative_ontology:measurement(shinbutsu_syncretic_tr_t400, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 400, 0.4).
narrative_ontology:measurement(shinbutsu_syncretic_tr_t600, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 600, 0.46).
narrative_ontology:measurement(shinbutsu_syncretic_tr_t800, shinbutsu_ontological_commitment__syncretic_reading, theater_ratio, 800, 0.5).

% Extraction over time
narrative_ontology:measurement(shinbutsu_syncretic_be_t0, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(shinbutsu_syncretic_be_t200, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 200, 0.62).
narrative_ontology:measurement(shinbutsu_syncretic_be_t400, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 400, 0.68).
narrative_ontology:measurement(shinbutsu_syncretic_be_t600, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 600, 0.71).
narrative_ontology:measurement(shinbutsu_syncretic_be_t800, shinbutsu_ontological_commitment__syncretic_reading, base_extractiveness, 800, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(shinbutsu_syncretic_su_t0, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(shinbutsu_syncretic_su_t200, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 200, 0.65).
narrative_ontology:measurement(shinbutsu_syncretic_su_t400, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 400, 0.71).
narrative_ontology:measurement(shinbutsu_syncretic_su_t600, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 600, 0.76).
narrative_ontology:measurement(shinbutsu_syncretic_su_t800, shinbutsu_ontological_commitment__syncretic_reading, suppression_requirement, 800, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_commitment__syncretic_reading, identity_coordination).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__syncretic_reading, partition_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__syncretic_reading, incoherence_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the shinbutsu_ontological_commitment kernel family. It instantiates the syncretic reading, wherein kami and buddhas form a unified cosmological order. Sibling readings (partition, incoherence) decompose the same historical phenomenon into structurally distinct constraints with different epsilon profiles and directionalities.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
