% ============================================================================
% CONSTRAINT STORY: kami_buddha_ontology__domain_partition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: kami_buddha_ontology__domain_partition
 *   human_readable: Kami-Buddha Domain Partition (Ontologically Distinct Functional Domains)
 *   domain: religious/philosophical/cultural
 *
 * SUMMARY:
 *   This constraint instantiates the domain_partition reading of the
 *   kami_buddha_ontology kernel: the claim that kami and buddhas are
 *   ontologically distinct entities governing separate functional domains
 *   (life/purity vs. death/impurity). Historically institutionalized through
 *   the Meiji state's shinbutsu bunri policies, the arrangement coordinates
 *   ritual jurisdiction between Shinto and Buddhist institutions while also
 *   extracting institutional privilege and state ideological support for the
 *   Shinto side. It is claimed as a natural functional differentiation but is
 *   contested by the honji_suijaku_monism reading (ontological identity) and
 *   the incoherent_bundle reading (no coherent kernel). This JSON authors
 *   only the domain_partition reading as a clean epsilon-invariant
 *   constraint; sibling readings are separate files.
 *
 * KEY AGENTS:
 *   - shinto_priesthood (beneficiary/identity_locked): derives distinct institutional identity and ritual monopoly from the partition
 *   - buddhist_clergy (payer/constrained): historically stripped of living-domain functions and confined to funerals
 *   - syncretic_communities (payer/trapped): folk practitioners forcibly separated and delegitimized
 *   - national_state_apparatus (agenda_setter/arbitrage): imposed and legally maintains the partition
 *   - religious_studies_scholars (observer/analytical): debate the historicity and function of the partition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kami_buddha_ontology__domain_partition, 0.64).
domain_priors:suppression_score(kami_buddha_ontology__domain_partition, 0.61).
domain_priors:theater_ratio(kami_buddha_ontology__domain_partition, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, extractiveness, 0.64).
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kami_buddha_ontology__domain_partition, tangled_rope).
narrative_ontology:human_readable(kami_buddha_ontology__domain_partition, "Kami-Buddha Domain Partition (Ontologically Distinct Functional Domains)").
narrative_ontology:topic_domain(kami_buddha_ontology__domain_partition, "religious/philosophical/cultural").

domain_priors:requires_active_enforcement(kami_buddha_ontology__domain_partition).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kami_buddha_ontology__domain_partition, 'c8de98f4-bfd1-4253-bc50-81bdc1d76bc5').
narrative_ontology:cs_kernel_codification('c8de98f4-bfd1-4253-bc50-81bdc1d76bc5', formalized).
narrative_ontology:cs_authority_grounding('c8de98f4-bfd1-4253-bc50-81bdc1d76bc5', lineage).
narrative_ontology:cs_interpretation_layer_present('c8de98f4-bfd1-4253-bc50-81bdc1d76bc5').
narrative_ontology:cs_reading_relation('c8de98f4-bfd1-4253-bc50-81bdc1d76bc5', kami_buddha_ontology__honji_suijaku_monism, forecloses).
narrative_ontology:cs_reading_relation('c8de98f4-bfd1-4253-bc50-81bdc1d76bc5', kami_buddha_ontology__incoherent_bundle, coexists_with).
narrative_ontology:cs_axiom('c8de98f4-bfd1-4253-bc50-81bdc1d76bc5', foundational, kami_buddha_ontological_distinction).
narrative_ontology:cs_axiom_status(kami_buddha_ontological_distinction, holdable).
narrative_ontology:cs_axiom_grounding('c8de98f4-bfd1-4253-bc50-81bdc1d76bc5', kami_buddha_ontological_distinction, theological).
narrative_ontology:cs_axiom('c8de98f4-bfd1-4253-bc50-81bdc1d76bc5', foundational, ritual_domain_purity_separation).
narrative_ontology:cs_axiom_status(ritual_domain_purity_separation, holdable).
narrative_ontology:cs_axiom_grounding('c8de98f4-bfd1-4253-bc50-81bdc1d76bc5', ritual_domain_purity_separation, conventional).
narrative_ontology:cs_reference_frame('c8de98f4-bfd1-4253-bc50-81bdc1d76bc5', classical_ritual_functionalism).
narrative_ontology:cs_drift_state('c8de98f4-bfd1-4253-bc50-81bdc1d76bc5', modern_nation_state, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c8de98f4-bfd1-4253-bc50-81bdc1d76bc5', '').
narrative_ontology:cs_kernel_id(kami_buddha_ontology__domain_partition, kami_buddha_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__domain_partition, shinto_priesthood).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__domain_partition, national_state_apparatus).
narrative_ontology:constraint_victim(kami_buddha_ontology__domain_partition, buddhist_clergy).
narrative_ontology:constraint_victim(kami_buddha_ontology__domain_partition, syncretic_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Derives institutional identity, professional licensure, and exclusive ritual jurisdiction over birth, marriage, and purity rites from the claim that kami govern a domain wholly separate from buddhas. Their self-concept and livelihood are fused with the partition; abandoning it would dissolve the theological basis for an independent Shinto priesthood.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, shinto_priesthood, beneficiary,
    organized, generational, identity_locked, national).

% Imposed the partition through the Meiji shinbutsu bunri policies and subsequent legal frameworks that separate shrine and temple corporations. Benefits from a non-Buddhist national cult centered on the emperor and shrines, which stabilizes a distinct source of civic symbolism. Can alter the constraint by legislative or constitutional action but retains the institutional framework because it simplifies religious governance.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, national_state_apparatus, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(kami_buddha_ontology__domain_partition, national_state_apparatus, beneficiary).

% Historically stripped of shrine affiliations, protective kami, and life-cycle rituals during state-enforced separation; redefined around death, funerary, and impurity functions. Modern clergy remain structurally confined to the memorial economy, with limited capacity to reclaim living-domain rituals without violating the partition's legal and cultural boundaries.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, buddhist_clergy, payer,
    organized, generational, constrained, national).

% Local communities and folk practitioners whose shrines and temples were forcibly separated, destroying integrated kami-buddha worship. Their ancestral practices were delegitimized as corrupt superstition. Modern descendants lack institutional voice or resources to restore integrated forms; the partition makes their unified practice unthinkable within official religious categories.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, syncretic_communities, payer,
    powerless, biographical, trapped, local).

% Analyze the historical and theological claims of the partition. Some defend the functional differentiation model; others, following Kuroda Toshio and subsequent critical scholarship, deconstruct it as a modern ideological construct imposed by the nation-state. Their conclusions either naturalize or destabilize the constraint in textbooks and public discourse.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, religious_studies_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates ritual jurisdiction between Shinto and Buddhist institutions in a shared society, assigning life-cycle domains (birth, marriage, purity to Shinto; death, funerals, impurity to Buddhism) to prevent destructive conflict over ceremonies and theological authority.
% TRANSFER_FUNCTION: Moves ritual authority over life and purity to Shinto institutions; moves death and funerary authority to Buddhist institutions; transfers state patronage, legal recognition, and cultural prestige toward Shinto and away from Buddhist national-cult status.
% ABSENT_VOICES: Syncretic practitioners and critical scholars who view Japanese religion as an integrated shinbutsu-shugo field are marginalized in official shrine-temple categorization and standard religious education. Folk communities whose practice fused kami and buddha have no seat at the institutional table.
% DISAPPEARANCE_RATIONALE: If the partition vanished, shrines and temples would compete directly for life-cycle rituals, the funeral economy would destabilize, scholarly taxonomies of Japanese religion would collapse into a single analytical field, and the legal distinction between shrine and temple corporations would require complete renegotiation.
% FOUNDING_PROBLEM: How to prevent jurisdictional and theological conflict between Shinto and Buddhist institutions sharing the same population and ritual economy, and how to construct a unified national religious identity around the emperor and shrines.
% FOUNDING_PROBLEM_CORROBORATION: Shinto institutions and kokugaku scholars attest the partition restores ancient Japanese practice. Buddhist historians and critical scholars (e.g., Kuroda Toshio, Mark Teeuwen) attest the problem was manufactured by the modern state to suppress Buddhism; their analysis is corroborated by extensive documentary evidence of medieval shinbutsu-shugo integration.
narrative_ontology:disappearance_verdict(kami_buddha_ontology__domain_partition, world_rearranges).
narrative_ontology:founding_problem_status(kami_buddha_ontology__domain_partition, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kami_buddha_ontology__domain_partition, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kami_buddha_ontology__domain_partition, 'none', 1).
narrative_ontology:epsilon_provenance(kami_buddha_ontology__domain_partition, 0.64, 'kimi-k2.6', 'none', direct).

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
 *   The partition solves a genuine coordination problemâwho performs which ritual in a multi-religious societyâbut its persistence has required active state enforcement and institutional suppression of syncretic alternatives. Base extractiveness (0.64) reflects the ongoing asymmetry: Shinto institutions retain preferential access to life-cycle rituals and cultural legitimacy, while Buddhist clergy remain structurally confined. Theater ratio (0.44) captures the performative maintenance of 'pure Shinto' free of Buddhist influence, a narrative that obscures the historical reality of shinbutsu-shugo. Resistance (0.40) is moderate: scholarly deconstruction and Buddhist historiography challenge the partition, but institutional and legal structures remain stable. The temporal series shows extraction and suppression peaking during the high State Shinto period and moderating but persisting through institutional inertia post-1945.
 *
 * PERSPECTIVAL GAP:
 *   The Shinto priesthood experiences the constraint as protective of their distinct identity and necessary for ritual clarity; the Buddhist clergy and syncretic communities experience it as an enforced categorization that stripped them of functions and legitimacy. The national state apparatus experiences it as a stabilizing governance tool, while scholars experience it as an analytically contested construct. The engine will compute divergent per-seat types from these structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Shinto priesthood and national state apparatus are structural beneficiaries (low d): the partition subsidizes their institutional identity and governance capacity. Buddhist clergy and syncretic communities are structural targets (high d): the partition extracts ritual jurisdiction and historical legitimacy from them. Scholars are analytical (d neutral). The state has arbitrage-grade exit (could change the law), while syncretic communities are trapped (no resources or standing to restore integrated practice).
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling the arrangement as pure coordination (rope) by requiring the declaration of victims and active enforcement. It also prevents mislabeling it as pure extraction (snare) by acknowledging the genuine coordination function: without some jurisdictional clarity, Shinto and Buddhist institutions would compete destructively for the same ritual economy. The founding problem is contestedâwhether pre-modern Japan actually needed this coordinationâbut the institutional reality is that the arrangement coordinates and extracts simultaneously.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    partition_historicity,
    'Does the domain partition reflect an ancient functional differentiation in Japanese religion, or is it a modern construct imposed during the Meiji period?',
    'Archaeological and textual analysis of pre-Meiji shrine-temple relations; comparison with medieval shinbutsu-shugo documentary evidence.',
    'If a modern construct, base_extractiveness and theater_ratio should be revised upward and the constraint reclassified toward snare; if ancient, the coordination function is more genuine and the type may stabilize as rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_historicity, empirical, 'Historical authenticity of the partition').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the persistence of the partition maintained by structural legal-institutional barriers or by internalized scholarly and theological categories?',
    'Comparative analysis of jurisdictions where legal separation has been relaxed versus where it remains strict; survey of religious studies pedagogy and shrine priest training.',
    'If primarily internalized, effective suppression is higher than structural measures suggest, and the constraint behaves more like cognitive capture; if structural, reform is a matter of legal intervention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, conceptual, 'Structural vs internalized suppression mechanism').

omega_variable(
    state_beneficiary_ambiguity,
    'Does the modern Japanese state still benefit from the partition, or has the benefit shifted entirely to Shinto institutions alone?',
    'Analysis of constitutional cases, state shrine visits, public education curricula, and administrative treatment of religious corporations regarding religious neutrality.',
    'If the state is no longer a beneficiary, the extraction asymmetry narrows and directionality for the state seat shifts toward neutral; if the state still covertly benefits, the constraint retains its tangled_rope character.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_beneficiary_ambiguity, empirical, 'State role in modern partition maintenance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kami_buddha_ontology__domain_partition, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kami_tr_t0, kami_buddha_ontology__domain_partition, theater_ratio, 0, 0.1).
narrative_ontology:measurement(kami_tr_t25, kami_buddha_ontology__domain_partition, theater_ratio, 25, 0.3).
narrative_ontology:measurement(kami_tr_t50, kami_buddha_ontology__domain_partition, theater_ratio, 50, 0.55).
narrative_ontology:measurement(kami_tr_t75, kami_buddha_ontology__domain_partition, theater_ratio, 75, 0.48).
narrative_ontology:measurement(kami_tr_t100, kami_buddha_ontology__domain_partition, theater_ratio, 100, 0.38).
narrative_ontology:measurement(kami_tr_t125, kami_buddha_ontology__domain_partition, theater_ratio, 125, 0.4).
narrative_ontology:measurement(kami_tr_t150, kami_buddha_ontology__domain_partition, theater_ratio, 150, 0.44).

% Extraction over time
narrative_ontology:measurement(kami_be_t0, kami_buddha_ontology__domain_partition, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(kami_be_t25, kami_buddha_ontology__domain_partition, base_extractiveness, 25, 0.58).
narrative_ontology:measurement(kami_be_t50, kami_buddha_ontology__domain_partition, base_extractiveness, 50, 0.72).
narrative_ontology:measurement(kami_be_t75, kami_buddha_ontology__domain_partition, base_extractiveness, 75, 0.68).
narrative_ontology:measurement(kami_be_t100, kami_buddha_ontology__domain_partition, base_extractiveness, 100, 0.6).
narrative_ontology:measurement(kami_be_t125, kami_buddha_ontology__domain_partition, base_extractiveness, 125, 0.62).
narrative_ontology:measurement(kami_be_t150, kami_buddha_ontology__domain_partition, base_extractiveness, 150, 0.64).

% Suppression requirement over time
narrative_ontology:measurement(kami_su_t0, kami_buddha_ontology__domain_partition, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(kami_su_t25, kami_buddha_ontology__domain_partition, suppression_requirement, 25, 0.75).
narrative_ontology:measurement(kami_su_t50, kami_buddha_ontology__domain_partition, suppression_requirement, 50, 0.92).
narrative_ontology:measurement(kami_su_t75, kami_buddha_ontology__domain_partition, suppression_requirement, 75, 0.8).
narrative_ontology:measurement(kami_su_t100, kami_buddha_ontology__domain_partition, suppression_requirement, 100, 0.45).
narrative_ontology:measurement(kami_su_t125, kami_buddha_ontology__domain_partition, suppression_requirement, 125, 0.52).
narrative_ontology:measurement(kami_su_t150, kami_buddha_ontology__domain_partition, suppression_requirement, 150, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kami_buddha_ontology__domain_partition, identity_coordination).
narrative_ontology:affects_constraint(kami_buddha_ontology__domain_partition, honji_suijaku_monism).
narrative_ontology:affects_constraint(kami_buddha_ontology__domain_partition, incoherent_bundle).

% DUAL FORMULATION NOTE:
% This constraint is one member of the kami_buddha_ontology kernel family. The domain_partition reading (this file) is linked to its sibling readings, which instantiate competing ontological claims from the same historical material.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
