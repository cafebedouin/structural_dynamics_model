% ============================================================================
% CONSTRAINT STORY: simultaneous_veneration__ontological_fusion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_simultaneous_veneration__ontological_fusion_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: simultaneous_veneration__ontological_fusion_reading
 *   human_readable: Honji-Suijaku Ontological Fusion: Buddhist Interpretive Monopoly Over Kami Identity
 *   domain: religious/metaphysical/institutional
 *
 * SUMMARY:
 *   The honji-suijaku (original essence / manifest traces) theory, formalized
 *   in medieval Japanese Buddhism, claims that kami are ontologically
 *   identical to buddhas: kami are the localized manifestations (suijaku)
 *   through which universal Buddhist deities (honji) interact with the
 *   material world. This reading treats the theory as capturing metaphysical
 *   truth—kami really are buddha-manifestations—and as an institutional
 *   arrangement where Buddhist interpretive authority claims monopoly over
 *   the meaning and status of kami. The constraint extraction operates on
 *   indigenous kami practitioners (who lose kami autonomy) and benefits the
 *   Buddhist institutional hierarchy (who gain interpretive monopoly and
 *   territorial religious control). The theater_ratio increase over the
 *   interval reflects growing performative maintenance of the fusion claim:
 *   as institutional pressure mounts and resistance to Meiji separation
 *   develops, more energy goes into reaffirming the theory as doctrine rather
 *   than allowing it to function pragmatically.
 *
 * KEY AGENTS:
 *   - Buddhist institutional hierarchy: Sets the honji-suijaku framework; extracts interpretive authority and institutional legitimacy from kami subordination
 *   - Indigenous kami practitioners: Bear the cost of kami autonomy loss; trapped by Buddhist institutional power over ritual authority
 *   - Syncretism practitioners: Gain cognitive permission to hold both kami and Buddhist veneration coherently; benefit from the framework
 *   - Shoguns and aristocratic patrons: Enforce the framework for political stability while retaining mobile power; can shift patronage
 *   - Meiji authorities (observer): Later reject the fusion reading and mandate kami-Buddha separation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(simultaneous_veneration__ontological_fusion_reading, 0.79).
domain_priors:suppression_score(simultaneous_veneration__ontological_fusion_reading, 0.62).
domain_priors:theater_ratio(simultaneous_veneration__ontological_fusion_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, extractiveness, 0.79).
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(simultaneous_veneration__ontological_fusion_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(simultaneous_veneration__ontological_fusion_reading, tangled_rope).
narrative_ontology:human_readable(simultaneous_veneration__ontological_fusion_reading, "Honji-Suijaku Ontological Fusion: Buddhist Interpretive Monopoly Over Kami Identity").
narrative_ontology:topic_domain(simultaneous_veneration__ontological_fusion_reading, "religious/metaphysical/institutional").

domain_priors:requires_active_enforcement(simultaneous_veneration__ontological_fusion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(simultaneous_veneration__ontological_fusion_reading, '7e5f4b66-dfe7-404d-a338-e1cf91155dce').
narrative_ontology:cs_kernel_codification('7e5f4b66-dfe7-404d-a338-e1cf91155dce', formalized).
narrative_ontology:cs_authority_grounding('7e5f4b66-dfe7-404d-a338-e1cf91155dce', extraction).
narrative_ontology:cs_interpretation_layer_present('7e5f4b66-dfe7-404d-a338-e1cf91155dce').
narrative_ontology:cs_reading_relation('7e5f4b66-dfe7-404d-a338-e1cf91155dce', simultaneous_veneration__domain_partition_reading, coexists_with).
narrative_ontology:cs_reading_relation('7e5f4b66-dfe7-404d-a338-e1cf91155dce', simultaneous_veneration__pragmatic_incoherence_reading, coexists_with).
narrative_ontology:cs_axiom('7e5f4b66-dfe7-404d-a338-e1cf91155dce', foundational, kami_are_buddha_manifestations).
narrative_ontology:cs_axiom_status(kami_are_buddha_manifestations, holdable).
narrative_ontology:cs_axiom_grounding('7e5f4b66-dfe7-404d-a338-e1cf91155dce', kami_are_buddha_manifestations, deontological).
narrative_ontology:cs_axiom('7e5f4b66-dfe7-404d-a338-e1cf91155dce', foundational, buddhist_interpretive_hierarchy_legitimate).
narrative_ontology:cs_axiom_status(buddhist_interpretive_hierarchy_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('7e5f4b66-dfe7-404d-a338-e1cf91155dce', buddhist_interpretive_hierarchy_legitimate, conventional).
narrative_ontology:cs_reference_frame('7e5f4b66-dfe7-404d-a338-e1cf91155dce', unified_buddha_nature_cosmology).
narrative_ontology:cs_drift_state('7e5f4b66-dfe7-404d-a338-e1cf91155dce', meiji_restoration_separation_era, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('7e5f4b66-dfe7-404d-a338-e1cf91155dce', '').
narrative_ontology:cs_kernel_id(simultaneous_veneration__ontological_fusion_reading, simultaneous_veneration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(simultaneous_veneration__ontological_fusion_reading, buddhist_institutional_hierarchy).
narrative_ontology:constraint_victim(simultaneous_veneration__ontological_fusion_reading, indigenous_kami_autonomy).
narrative_ontology:constraint_victim(simultaneous_veneration__ontological_fusion_reading, pre_buddhist_kami_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__ontological_fusion_reading, syncretism_practitioners).
narrative_ontology:constraint_victim(simultaneous_veneration__ontological_fusion_reading, indigenous_kami_practitioners).
narrative_ontology:constraint_vindicates(simultaneous_veneration__ontological_fusion_reading, ontological_monism_universal_buddha_nature).
narrative_ontology:constraint_vindicates(simultaneous_veneration__ontological_fusion_reading, interpretive_hierarchy_validates_syncretism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develops and enforces honji-suijaku theory to claim that kami are manifestations of buddhas. Sets the interpretive framework that allows simultaneous veneration while subordinating kami to Buddhist metaphysical authority. Collects institutional legitimacy, territorial control, and ritual authority by absorbing kami worship into Buddhist structure. Can arbitrage between competing patrons (aristocrats, shoguns) by offering interpretive flexibility.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, buddhist_institutional_hierarchy, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(simultaneous_veneration__ontological_fusion_reading, buddhist_institutional_hierarchy, beneficiary).

% Lose autonomous ontological status for kami through the fusion reading: their kami become secondary manifestations of Buddhist entities rather than independent beings. Trapped by the constraint because Buddhist institutional power controls ritual authority, textual interpretation, and access to the organized religious infrastructure. Cannot exit without loss of religious legitimacy and community standing.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, indigenous_kami_practitioners, payer,
    powerless, generational, trapped, local).

% Gain permission to practice simultaneous veneration under the honji-suijaku framework: they can honor both kami and buddhas within a single coherent metaphysical system rather than holding contradictory beliefs. The framework resolves cognitive dissonance at the cost of accepting Buddhist interpretive authority over kami identity.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, syncretism_practitioners, beneficiary,
    moderate, biographical, constrained, national).

% Use the honji-suijaku framework to legitimize simultaneous veneration across their domains, preventing religious conflict while maintaining their own local kami traditions as secondary to Buddhist authority. They enforce the framework through patronage and legal mandate, though they retain the option to shift patronage or suppress Buddhism if it becomes politically disadvantageous.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, shoguns_and_aristocratic_patrons, agenda_setter,
    powerful, biographical, mobile, national).

% The indigenous Japanese understanding of kami as autonomous spiritual beings governing specific places and phenomena. This cosmological framework would assert the independence of kami from any universal Buddha-nature, but it is structurally excluded from the honji-suijaku interpretation: its core premises contradict the fusion reading's axioms.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, pre_buddhist_kami_cosmology, excluded,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(simultaneous_veneration__ontological_fusion_reading, pre_buddhist_kami_cosmology).

% Later reject the honji-suijaku framework entirely and mandate separation of kami and Buddha (Shinto vs Buddhism). They observe the constraint's operation during the Edo period and deliberately dissolve it via state power, treating the fusion reading as a constructed distortion of authentic kami worship.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__ontological_fusion_reading, meiji_restoration_authorities, observer,
    institutional, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(simultaneous_veneration__ontological_fusion_reading, buddhist_institutional_hierarchy).
narrative_ontology:fixing_cost_class(simultaneous_veneration__ontological_fusion_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single metaphysical framework that permits simultaneous veneration of kami and buddhas without logical contradiction: adherents can honor both within a unified ontology where kami are understood as localized manifestations of universal Buddha-nature.
% TRANSFER_FUNCTION: Transfers interpretive authority over kami identity from indigenous practitioners to the Buddhist institutional hierarchy. Indigenous kami lose autonomous ontological standing and become secondary manifestations defined by Buddhist metaphysics. Buddhist institutions gain religious legitimacy, institutional power, and control over ritual practice across Japan's territory.
% ABSENT_VOICES: Pre-Buddhist kami practitioners and indigenous cosmologists are structurally excluded: their assertion that kami are autonomous beings independent of Buddha-nature is treated as heresy or folk superstition rather than a rival metaphysical position. The Meiji-era authorities who would later mandate kami-Buddha separation are also absent: they would testify that the fusion reading is an institutional imposition, not metaphysical truth.
% DISAPPEARANCE_RATIONALE: If the honji-suijaku fusion framework vanished, simultaneous veneration would collapse into either explicit metaphysical contradiction (as in the pragmatic_incoherence_reading) or institutional fragmentation into separate kami and Buddhist domains. The Meiji restoration's actual historical move demonstrates this: when the fusion reading was officially rejected, kami worship was reorganized as Shinto and severed from Buddhist institutional control. The constraint's disappearance would require either re-partition of kami/Buddha domains or re-legalization of pragmatic incoherence.
% FOUNDING_PROBLEM: Early Japanese Buddhism faced institutional incorporation into a society with deep-rooted kami veneration. Rather than suppress kami worship (risking resistance) or admit kami as equal supernatural agents (competing with Buddhist authority), the honji-suijaku theory offered a third path: kami are real and worthy of veneration, but they are manifestations of buddhas whose ultimate nature is the universal Buddha-principle. This resolved the institutional problem of simultaneous veneration without requiring explicit choice between religions.
% FOUNDING_PROBLEM_CORROBORATION: Buddhist institutional sources testify the founding problem was institutional integration (need to absorb kami without yielding authority). Kami practitioners' records and later Meiji historians testify instead that the founding problem was kami autonomy threatened by Buddhist expansion—the honji-suijaku theory solved the institutional problem by constructing a metaphysical domination, not by solving a genuine logical incoherence. Modern comparative religionists (outside both benefiting parties) attest the framework was contingent institutional strategy, not metaphysical truth.
narrative_ontology:disappearance_verdict(simultaneous_veneration__ontological_fusion_reading, world_rearranges).
narrative_ontology:founding_problem_status(simultaneous_veneration__ontological_fusion_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(simultaneous_veneration__ontological_fusion_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(simultaneous_veneration__ontological_fusion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(simultaneous_veneration__ontological_fusion_reading, 0.79, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(simultaneous_veneration__ontological_fusion_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(simultaneous_veneration__ontological_fusion_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(simultaneous_veneration__ontological_fusion_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high and rising (0.58 to 0.79) because the constraint systematically subordinates kami to Buddhist metaphysics: what was an independent ontological status becomes a derivative role in a Buddhist-centered cosmology. This subordination is not reversible by the kami practitioners—they cannot claim kami are independent without directly contradicting the honji-suijaku axioms. Suppression is moderate (0.62) because the framework operates partly through persuasion and cognitive coherence (syncretism practitioners genuinely benefit from the unified worldview) and partly through institutional control (ritual authority, textual interpretation, patronage systems). The theater_ratio increase (0.22 to 0.48) reflects the constraint's lifecycle: early adoption involved genuine interpretive work (reconciling cosmologies); later maintenance increasingly involved ritual performance and doctrinal reaffirmation as resistance mounted (Meiji challenges to the framework, indigenous cosmology reassertions). The three metrics share one time grid so the measurement series is consistent: every metric is authored at every examined point, enabling proper temporal analysis of constraint drift.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seat (Buddhist institutional hierarchy) experiences the constraint as genuine metaphysical truth that resolves incoherence and permits unified practice. The victim seat (indigenous kami practitioners) experiences it as interpretive domination that erases kami autonomy. The payer seat and the agenda-setter seat diverge sharply: the institution experiences itself as providing philosophical coherence and religious legitimacy; the practitioners experience institutional extraction of their cosmological authority. The shoguns occupy a third position—they benefit from political stability without ideological commitment to the honji-suijaku truth claim, giving them arbitrage mobility the other seats lack.
 *
 * DIRECTIONALITY LOGIC:
 *   The Buddhist institutional hierarchy is the full beneficiary (d ≈ 0.15): they set the framework, collect interpretive authority, and gain institutional power through kami subordination. They have arbitrage mobility—they can shift interpretive claims or accommodate challenges if patronage shifts. Indigenous kami practitioners are the full targets (d ≈ 0.85): they are trapped by institutional control of ritual, textual authority, and religious legitimacy; their kami lose autonomous status and become Buddhist derivatives. They cannot exit without loss of religious standing and community identity. Syncretism practitioners sit near beneficiary (d ≈ 0.30): they gain cognitive permission and participation in organized religion but accept Buddhist interpretive authority. Shoguns and patrons sit near symmetric (d ≈ 0.45–0.55): they benefit from religious stability and institutional support but retain mobile power to shift patronage or suppress Buddhism; the constraint does not fully capture them because they control enforcement and can redraw the framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is classified as tangled_rope because it possesses BOTH genuine coordination function (it does coherently permit simultaneous veneration without logical contradiction) AND asymmetric extraction (Buddhist institutional hierarchy gains interpretive monopoly and kami autonomy is erased). The coordination function is real: practitioners who adopt the honji-suijaku framework do resolve cognitive dissonance and participate in a unified religious system. But the coordination is achieved through and subordinated to institutional extraction: the unity is only coherent because Buddhist metaphysics dominates; kami cannot be reframed as independent without the system collapsing. This is why it is tangled rather than pure rope or pure snare: remove the extraction (grant kami full autonomous status) and the coordination dissolves; remove the coordination (allow explicit kami-Buddha separation) and the institutional power dissolves. The two elements are structurally entangled, not separable. Mandatrophy surfaces as the constraint's later rejection: once Meiji authorities treat the fusion reading as false doctrine rather than metaphysical truth, the coordination function loses its legitimacy foundation and the extraction becomes naked. The 'truth claim' was doing work holding the tangled rope together.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    metaphysical_truth_vs_institutional_strategy,
    'Is honji-suijaku theory a genuine metaphysical claim about the nature of kami and buddhas, or a strategic institutional framework constructed to solve the religious-integration problem?',
    'Analysis of textual genealogy and institutional motivation: if the theory predates Buddhist expansion into kami-veneration societies, it is more likely metaphysical; if it was formalized after institutional pressure, it is more likely strategic. Testimony from practitioners about whether the theory is held as truth or pragmatic accommodation.',
    'If institutional strategy: the constraint''s extractiveness is fully acknowledged and the victim classification stands. If metaphysical truth: the constraint''s extractiveness is reframed as the cost of truth-alignment, and the victim classification becomes contestable (is kami-subordination extraction or correct ontological ordering?).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metaphysical_truth_vs_institutional_strategy, conceptual, 'Whether honji-suijaku is a truth claim or institutional imposition').

omega_variable(
    kami_autonomy_loss_vs_integration,
    'Did the fusion reading erase indigenous kami as autonomous beings (ontological loss) or integrate them into a larger metaphysical system where their power is preserved but recontextualized (transformation)?',
    'Ethnographic study of how practitioners actually understood kami status before and after honji-suijaku adoption. Analysis of whether kami received continued independent offerings and maintained localized authority despite the fusion framework.',
    'If ontological loss: victimhood classification is appropriate; if recontextualization with preserved function: the extraction may be smaller than high ε suggests, and the constraint may be more rope-like (coordination with moderate asymmetry) than tangled rope. Direct historical testimony from non-Buddhist sources about their understanding of kami-Buddha relationship.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kami_autonomy_loss_vs_integration, empirical, 'Whether fusion reading constituted kami erasure or adaptive integration').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.62) driven by structural factors (Buddhist institutional control of ritual, textual authority, patronage dependence) or internalized factors (practitioners accepting the fusion framework as true, making kami autonomy unthinkable)?',
    'Post-separation observation: after Meiji mandated kami-Buddha separation, did practitioners quickly re-assert kami autonomy (indicating suppression was structural) or continue fusion practice despite new legal permission (indicating suppression was internalized, identity-fused)?',
    'If structural suppression: the constraint''s effectiveness rests on institutional control and would collapse with institutional pressure (as Meiji separation demonstrates). If internalized: the suppression persists in practitioners'' self-understanding even after institutional framework is removed, indicating deeper identity fusion and higher effective suppression than structural alone.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Suppression source: institutional control vs. internalized belief').

omega_variable(
    kernel_reading_committer_frame,
    'This constraint is one reading of the simultaneous_veneration kernel. What canonical text or authority grounds the honji-suijaku framework''s claim to be THE correct reading?',
    'Textual genealogy of honji-suijaku formalization: which Buddhist texts or authorities first stated the framework? When? With what explicit claims about metaphysical truth vs. pragmatic accommodation? Which alternative readings (domain-partition, pragmatic-incoherence) claim canonical support?',
    'If grounded in foundational Buddhist texts interpreted as metaphysical truth: the fusion reading has high authority-grounding legitimacy within Buddhist tradition and stronger foreclosure claim over rival readings. If formalized later as institutional adaptation: the authority-grounding is more extraction-based (Buddhist institutional hierarchy uses canonical authority to justify a new institutional solution), strengthening the tangled-rope classification and supporting the institutional-strategy resolution of omega_1.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_committer_frame, empirical, 'Canonical grounding and authority trajectory of the fusion reading within Buddhist tradition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(simultaneous_veneration__ontological_fusion_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(simu_tr_t0, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(simu_tr_t0, observed).
narrative_ontology:measurement(simu_tr_t3, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 3, 0.28).
narrative_ontology:measurement_basis(simu_tr_t3, observed).
narrative_ontology:measurement(simu_tr_t6, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 6, 0.34).
narrative_ontology:measurement_basis(simu_tr_t6, observed).
narrative_ontology:measurement(simu_tr_t12, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 12, 0.42).
narrative_ontology:measurement_basis(simu_tr_t12, observed).
narrative_ontology:measurement(simu_tr_t18, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 18, 0.46).
narrative_ontology:measurement_basis(simu_tr_t18, observed).
narrative_ontology:measurement(simu_tr_t25, simultaneous_veneration__ontological_fusion_reading, theater_ratio, 25, 0.48).
narrative_ontology:measurement_basis(simu_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(simu_be_t0, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(simu_be_t0, observed).
narrative_ontology:measurement(simu_be_t3, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 3, 0.64).
narrative_ontology:measurement_basis(simu_be_t3, observed).
narrative_ontology:measurement(simu_be_t6, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 6, 0.7).
narrative_ontology:measurement_basis(simu_be_t6, observed).
narrative_ontology:measurement(simu_be_t12, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 12, 0.74).
narrative_ontology:measurement_basis(simu_be_t12, observed).
narrative_ontology:measurement(simu_be_t18, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 18, 0.77).
narrative_ontology:measurement_basis(simu_be_t18, observed).
narrative_ontology:measurement(simu_be_t25, simultaneous_veneration__ontological_fusion_reading, base_extractiveness, 25, 0.79).
narrative_ontology:measurement_basis(simu_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(simu_su_t0, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(simu_su_t0, observed).
narrative_ontology:measurement(simu_su_t3, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 3, 0.44).
narrative_ontology:measurement_basis(simu_su_t3, observed).
narrative_ontology:measurement(simu_su_t6, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 6, 0.5).
narrative_ontology:measurement_basis(simu_su_t6, observed).
narrative_ontology:measurement(simu_su_t12, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 12, 0.57).
narrative_ontology:measurement_basis(simu_su_t12, observed).
narrative_ontology:measurement(simu_su_t18, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 18, 0.6).
narrative_ontology:measurement_basis(simu_su_t18, observed).
narrative_ontology:measurement(simu_su_t25, simultaneous_veneration__ontological_fusion_reading, suppression_requirement, 25, 0.62).
narrative_ontology:measurement_basis(simu_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(simultaneous_veneration__ontological_fusion_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(simultaneous_veneration__ontological_fusion_reading, 0.14).
narrative_ontology:affects_constraint(simultaneous_veneration__ontological_fusion_reading, simultaneous_veneration__domain_partition_reading).
narrative_ontology:affects_constraint(simultaneous_veneration__ontological_fusion_reading, simultaneous_veneration__pragmatic_incoherence_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of the simultaneous_veneration kernel family. The kernel is the historical practice of venerating both kami and buddhas together in Japan. Three distinct readings instantiate three distinct constraints with different ε values, beneficiary/victim structures, and classifications: (1) ontological_fusion_reading (this file) claims kami are Buddha-manifestations—high ε, Buddhist hierarchy beneficiary, kami-autonomy victim, tangled-rope; (2) domain_partition_reading claims kami and buddhas govern separate domains—moderate ε, no clear victim (specialization is functional), rope-like coordination; (3) pragmatic_incoherence_reading claims the practice sustained explicit contradiction without resolution—moderate ε, institutional hierarchy beneficiary (sustains ambiguity), practitioners victim (held contradictory beliefs), snare-like. The three readings coexist in historical sources and among different actors; they do not cancel each other. The fusion reading forecloses pre-Buddhist kami-autonomy cosmology but coexists with the other two readings because different historical parties held them simultaneously. All three link to the same kernel; the fusion reading influences (downstream pressure) the other readings because it establishes the authoritative Buddhist framing against which the alternatives are defined.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
