% ============================================================================
% CONSTRAINT STORY: kami_buddha_ontology__domain_partition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kami_buddha_ontology__domain_partition, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: kami_buddha_ontology__domain_partition
 *   human_readable: Kami-Buddha Ontological Domain Partition
 *   domain: religious_studies/japanese_history
 *
 * SUMMARY:
 *   This constraint story models the domain_partition reading of the
 *   kami_buddha_ontology kernel: the claim that kami and buddhas are
 *   ontologically distinct entities governing functionally separate
 *   domainsâShinto over life, purity, and the living; Buddhism over death,
 *   impurity, and the deceased. The reading emerged as a coordinated
 *   alternative to medieval honji suijaku monism, achieving institutional
 *   dominance in the Edo and Meiji periods through kokugaku scholarship and
 *   state enforcement (shinbutsu bunri). It presents itself as a natural
 *   complementary order, but functions as an actively enforced jurisdictional
 *   partition allocating ritual markets to separate institutional centers
 *   while imposing navigational and financial costs on the laity and
 *   suppressing syncretic hybridity.
 *
 * KEY AGENTS:
 *   - shinto_institutions (beneficiary): shrine organizations and priesthoods capturing life-cycle ritual jurisdiction
 *   - buddhist_institutions (beneficiary): temple networks capturing mortuary and ancestral economies
 *   - lay_community (payer): populace compelled to maintain dual ritual affiliations and payments
 *   - syncretic_practitioners (excluded/victim): hybrid shrine-temple communities and practitioners whose fused cosmology is institutionally suppressed
 *   - nativist_scholars (agenda_setter): kokugaku intellectual architects of the ontological partition
 *   - modern_academics (observer): historians of religion analyzing the partition as modern construction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kami_buddha_ontology__domain_partition, 0.48).
domain_priors:suppression_score(kami_buddha_ontology__domain_partition, 0.58).
domain_priors:theater_ratio(kami_buddha_ontology__domain_partition, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, extractiveness, 0.48).
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kami_buddha_ontology__domain_partition, tangled_rope).
narrative_ontology:human_readable(kami_buddha_ontology__domain_partition, "Kami-Buddha Ontological Domain Partition").
narrative_ontology:topic_domain(kami_buddha_ontology__domain_partition, "religious_studies/japanese_history").

domain_priors:requires_active_enforcement(kami_buddha_ontology__domain_partition).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kami_buddha_ontology__domain_partition, 'e0b96525-3549-40c0-8328-83767afd65a3').
narrative_ontology:cs_kernel_codification('e0b96525-3549-40c0-8328-83767afd65a3', fixed_text).
narrative_ontology:cs_authority_grounding('e0b96525-3549-40c0-8328-83767afd65a3', lineage).
narrative_ontology:cs_interpretation_layer_present('e0b96525-3549-40c0-8328-83767afd65a3').
narrative_ontology:cs_reading_relation('e0b96525-3549-40c0-8328-83767afd65a3', kami_buddha_ontology__honji_suijaku_monism, forecloses).
narrative_ontology:cs_reading_relation('e0b96525-3549-40c0-8328-83767afd65a3', kami_buddha_ontology__incoherent_bundle, influences).
narrative_ontology:cs_axiom('e0b96525-3549-40c0-8328-83767afd65a3', foundational, kami_buddha_ontological_incommensurability).
narrative_ontology:cs_axiom_status(kami_buddha_ontological_incommensurability, holdable).
narrative_ontology:cs_axiom_grounding('e0b96525-3549-40c0-8328-83767afd65a3', kami_buddha_ontological_incommensurability, theological).
narrative_ontology:cs_axiom('e0b96525-3549-40c0-8328-83767afd65a3', foundational, functional_domain_exclusivity).
narrative_ontology:cs_axiom_status(functional_domain_exclusivity, holdable).
narrative_ontology:cs_axiom_grounding('e0b96525-3549-40c0-8328-83767afd65a3', functional_domain_exclusivity, conventional).
narrative_ontology:cs_reference_frame('e0b96525-3549-40c0-8328-83767afd65a3', classical_purity_separation).
narrative_ontology:cs_drift_state('e0b96525-3549-40c0-8328-83767afd65a3', medieval_shinbutsu_shugo, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('e0b96525-3549-40c0-8328-83767afd65a3', '').
narrative_ontology:cs_kernel_id(kami_buddha_ontology__domain_partition, kami_buddha_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__domain_partition, shinto_institutions).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__domain_partition, buddhist_institutions).
narrative_ontology:constraint_victim(kami_buddha_ontology__domain_partition, lay_community).
narrative_ontology:constraint_victim(kami_buddha_ontology__domain_partition, syncretic_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer life-cycle, purity, and state rites; hold exclusive jurisdiction over kami veneration. Benefit from doctrinal separation that prevents Buddhist cosmological absorption and guarantees a stable ritual clientele.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, shinto_institutions, beneficiary,
    organized, generational, constrained, national).

% Administer funerary, ancestral, and death-pollution rites; hold exclusive jurisdiction over mortuary economy. Benefit from stable allocation of the lucrative afterlife market and protection from Shinto territorial expansion.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, buddhist_institutions, beneficiary,
    organized, generational, constrained, national).

% Must engage both ritual systems separatelyâshrines for birth, marriage, and prosperity; temples for death, mourning, and ancestors. Bears the cost of dual donations, dual affiliations, and navigational complexity; syncretic one-stop religious care is institutionally unavailable.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, lay_community, payer,
    moderate, biographical, constrained, national).

% Hold that kami and buddhas interpenetrate or are identical; maintain hybrid practices such as kami as avatars of bodhisattvas. Their shrines and temples were forcibly separated during state enforcement; their cosmology is rendered illegitimate by the partition framework.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, syncretic_practitioners, excluded,
    powerless, biographical, trapped, local).

% Kokugaku intellectuals who formulated the ontological distinction and lobbied for institutional separation. Do not directly collect ritual revenue but derive scholarly authority and political influence from maintaining the partition's ideological legitimacy.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, nativist_scholars, agenda_setter,
    moderate, generational, mobile, national).

% Historians and religion scholars who analyze the domain partition as an early modern ideological construction rather than an ancient indigenous structure. They document the medieval syncretic reality that the partition reading suppresses.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, modern_academics, observer,
    institutional, generational, analytical, global).

narrative_ontology:fixing_cost_class(kami_buddha_ontology__domain_partition, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Organizes coexistence between two universalist religious traditions within one society by allocating non-overlapping ritual jurisdiction: birth, growth, purity, and state rites to kami/Shinto; death, pollution, ancestral memorial, and afterlife to buddhas/Buddhism.
% TRANSFER_FUNCTION: Moves ritual revenue and institutional turf from a shared syncretic economy into separate shrine and temple channels; moves the cognitive and financial burden of dual affiliation onto the lay community.
% ABSENT_VOICES: Syncretic practitioners who worship kami as bodhisattvas or buddhas as local deities; Buddhist institutions during periods when the partition is enforced as Shinto supremacy rather than parallelism; rural communities for whom the life/death separation is alien to lived practice.
% DISAPPEARANCE_RATIONALE: If the ontological partition vanished, the Japanese ritual economy would reintegrate: Buddhist temples would reclaim shrine affiliations, mortuary revenue would redistribute, Shinto institutions would lose exclusive state ritual standing, and lay practice would revert to the hybrid shinbutsu-shugo patterns documented in the medieval period.
% FOUNDING_PROBLEM: How to preserve indigenous kami worship from absorption into Buddhist cosmological hegemony while stabilizing the ritual economy of a society with two universalist religious systems.
% FOUNDING_PROBLEM_CORROBORATION: Academic historians of Japanese religion (e.g., Kuroda Toshio, Allan Grapard, Fabio Rambelli) attest from outside the beneficiary institutions that the strict partition is a retroactive early modern construction; Shinto institutional sources and kokugaku-derived historiography attest the problem remains live.
narrative_ontology:disappearance_verdict(kami_buddha_ontology__domain_partition, world_rearranges).
narrative_ontology:founding_problem_status(kami_buddha_ontology__domain_partition, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kami_buddha_ontology__domain_partition, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kami_buddha_ontology__domain_partition, 'none', 1).
narrative_ontology:epsilon_provenance(kami_buddha_ontology__domain_partition, 0.48, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kami_buddha_ontology__domain_partition_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kami_buddha_ontology__domain_partition, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kami_buddha_ontology__domain_partition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48) is moderate-to-high because the partition creates ritual monopolies and institutional rents, though it also solves a genuine coordination problem (preventing absorptive competition). Suppression (0.58) reflects the active enforcement required to maintain separation against syncretic pressure and the violent separation campaigns (haibutsu kishaku) of the Meiji period. Theater_ratio (0.35) captures the performative dimension: the partition is presented as an ancient, natural order but is largely an early modern ideological construction. Accessibility_collapse (0.45) indicates that while syncretic alternatives persist subculturally, they are institutionally marginalized. Resistance (0.52) reflects Buddhist institutional pushback during Meiji and ongoing lay indifference to strict separation. The metrics peak during the Meiji enforcement window (T80) and moderate thereafter, showing lifecycle drift from coordination-plus-extraction to normalized institutional inertia.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary institutional seats (shrines and temples), the partition is a legitimate jurisdictional settlement that preserves theological identity and stable revenue. From the lay and syncretic seats, the same structure appears as an imposed artificial duality requiring double payment and suppressing integrated religious experience. The engine will compute divergent per-seat classifications from this structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Shinto institutions and Buddhist institutions are both structural beneficiaries of the turf allocation (low d for their respective domains). However, the lay community bears the cost of dual maintenance (high d). Syncretic practitioners, whose cosmology is directly contradicted by the partition, sit at the highest extraction end (trapped exit, identity_locked in hybrid communities). Nativist scholars are agenda-setters with mobile exit (they can shift scholarly fashions) but their intellectual authority depends on maintaining the partition's legitimacy.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling the partition as pure rope (which would ignore the institutional rents and suppression of syncretism) or pure snare (which would ignore the genuine coordination function: without some partition, Buddhist universalism historically threatened to absorb Shinto into a derivative status, and unregulated competition destabilized ritual markets). The founding problemâpreventing absorptive collapseâwas live in the Nara-Heian transition, but by the Edo period the arrangement had accumulated extractive functions that outlived the original threat.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_retrofit,
    'Is the strict domain partition a genuine premodern theological reading or a modern (Edo-Meiji) retrofit onto earlier syncretic practice?',
    'Archaeological and textual analysis of pre-Edo ritual records; comparison of medieval shrine-temple multiplexes against Edo-period ideological historiography.',
    'If the partition is proven a modern retrofit, its claimed_type shifts toward snare (ideological extraction) and its coordination function is downgraded from ancient necessity to modern construction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_retrofit, empirical, 'Whether the partition is historically authentic or retroactively imposed.').

omega_variable(
    meiji_hierarchy_contradiction,
    'Does the hierarchical enforcement of Shinto over Buddhism during Meiji contradict the ''no hierarchy'' core premise of the domain partition reading?',
    'Comparative analysis of Meiji-era legal codes and shrine administration against the parallel-complementarity doctrinal claim; assess whether state Shinto was a separate constraint or an enforcement mode of this one.',
    'If Meiji hierarchy is inseparable from the partition reading, the ''no hierarchy'' axiom is undermined and the constraint reveals hidden asymmetric extraction favoring Shinto institutions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meiji_hierarchy_contradiction, conceptual, 'Whether Meiji state Shoto supremacy contradicts the parallel ontology claim.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the persistence of the partition due to ongoing structural enforcement or to internalized common sense among the laity?',
    'Post-1945 trajectory analysis: after SCAP disestablished state Shinto and legalized religious freedom, did lay practice rapidly re-syncretize or did the life/shrine-death/temple habit persist?',
    'If internalized, effective suppression is higher than structural measures suggestâthe constraint operates through cognitive habit even after legal barriers fall. If purely structural, removal of enforcement should predict faster collapse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kami_buddha_ontology__domain_partition, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kami_buddha_partition_tr_t0, kami_buddha_ontology__domain_partition, theater_ratio, 0, 0.18).
narrative_ontology:measurement(kami_buddha_partition_tr_t40, kami_buddha_ontology__domain_partition, theater_ratio, 40, 0.25).
narrative_ontology:measurement(kami_buddha_partition_tr_t80, kami_buddha_ontology__domain_partition, theater_ratio, 80, 0.62).
narrative_ontology:measurement(kami_buddha_partition_tr_t120, kami_buddha_ontology__domain_partition, theater_ratio, 120, 0.48).
narrative_ontology:measurement(kami_buddha_partition_tr_t160, kami_buddha_ontology__domain_partition, theater_ratio, 160, 0.28).
narrative_ontology:measurement(kami_buddha_partition_tr_t200, kami_buddha_ontology__domain_partition, theater_ratio, 200, 0.35).

% Extraction over time
narrative_ontology:measurement(kami_buddha_partition_be_t0, kami_buddha_ontology__domain_partition, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(kami_buddha_partition_be_t40, kami_buddha_ontology__domain_partition, base_extractiveness, 40, 0.35).
narrative_ontology:measurement(kami_buddha_partition_be_t80, kami_buddha_ontology__domain_partition, base_extractiveness, 80, 0.68).
narrative_ontology:measurement(kami_buddha_partition_be_t120, kami_buddha_ontology__domain_partition, base_extractiveness, 120, 0.58).
narrative_ontology:measurement(kami_buddha_partition_be_t160, kami_buddha_ontology__domain_partition, base_extractiveness, 160, 0.42).
narrative_ontology:measurement(kami_buddha_partition_be_t200, kami_buddha_ontology__domain_partition, base_extractiveness, 200, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(kami_buddha_partition_su_t0, kami_buddha_ontology__domain_partition, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(kami_buddha_partition_su_t40, kami_buddha_ontology__domain_partition, suppression_requirement, 40, 0.42).
narrative_ontology:measurement(kami_buddha_partition_su_t80, kami_buddha_ontology__domain_partition, suppression_requirement, 80, 0.9).
narrative_ontology:measurement(kami_buddha_partition_su_t120, kami_buddha_ontology__domain_partition, suppression_requirement, 120, 0.55).
narrative_ontology:measurement(kami_buddha_partition_su_t160, kami_buddha_ontology__domain_partition, suppression_requirement, 160, 0.3).
narrative_ontology:measurement(kami_buddha_partition_su_t200, kami_buddha_ontology__domain_partition, suppression_requirement, 200, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kami_buddha_ontology__domain_partition, identity_coordination).
narrative_ontology:affects_constraint(kami_buddha_ontology__domain_partition, honji_suijaku_monism).
narrative_ontology:affects_constraint(kami_buddha_ontology__domain_partition, incoherent_bundle).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kami_buddha_ontology kernel. The kernel decomposes into at least three structurally distinct constraints because the natural-language label 'shinbutsu-shugo' conflates ontological monism (honji suijaku), ontological dualism (domain partition), and skeptical disintegration (incoherent bundle). Each reading has a different epsilon, beneficiary structure, and institutional history.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
