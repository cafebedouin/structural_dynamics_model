% ============================================================================
% CONSTRAINT STORY: correct_latin_kernel__discontinuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_correct_latin_kernel__discontinuity_reading, []).

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
 *   constraint_id: correct_latin_kernel__discontinuity_reading
 *   human_readable: Correct Latin Kernel â Discontinuity Reading
 *   domain: historical_linguistics/philology/intellectual_history
 *
 * SUMMARY:
 *   This constraint instantiates the discontinuity reading of the
 *   correct_latin_kernel: the claim that Classical and Medieval Latin are
 *   structurally distinct linguistic systems, such that medieval forms are
 *   corruptions of a classical norm recoverable only through symbolic
 *   reoccupation from surviving texts. From the Renaissance through the
 *   twentieth century, this reading coordinated European philology by
 *   supplying a single editorial standard, while asymmetrically extracting
 *   epistemic authority, labor, and institutional prestige from medievalist
 *   scholars and textual editors. The classical philologist seat captured the
 *   gains; the medievalist seats bore the costs of perpetual justification
 *   and reconstructive labor. The authored metrics and claimed type are
 *   independently authored: the metrics describe substantially extractive,
 *   actively enforced operation, while the type asserts a tangled-rope
 *   structure in which a real coordination function (standardized textual
 *   criticism) is inseparable from asymmetric extraction.
 *
 * KEY AGENTS:
 *   - classical_philologists: Primary beneficiary and agenda-setter (institutional/arbitrage) â defines the norm, controls standards, collects prestige and curriculum centrality.
 *   - medievalist_scholars: Primary target (moderate/identity_locked) â studies medieval texts but must justify through classical recovery; identity fused to delegitimized objects.
 *   - textual_critics: Secondary target (moderate/constrained) â edits medieval manuscripts under the reconstructive imperative; labor directed toward classical archetypes.
 *   - sociolinguists: Excluded voice (moderate/mobile) â would treat medieval variation as systemic but is outside the philological conversation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(correct_latin_kernel__discontinuity_reading, 0.72).
domain_priors:suppression_score(correct_latin_kernel__discontinuity_reading, 0.68).
domain_priors:theater_ratio(correct_latin_kernel__discontinuity_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(correct_latin_kernel__discontinuity_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(correct_latin_kernel__discontinuity_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(correct_latin_kernel__discontinuity_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(correct_latin_kernel__discontinuity_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(correct_latin_kernel__discontinuity_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(correct_latin_kernel__discontinuity_reading, tangled_rope).
narrative_ontology:human_readable(correct_latin_kernel__discontinuity_reading, "Correct Latin Kernel â Discontinuity Reading").
narrative_ontology:topic_domain(correct_latin_kernel__discontinuity_reading, "historical_linguistics/philology/intellectual_history").

domain_priors:requires_active_enforcement(correct_latin_kernel__discontinuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(correct_latin_kernel__discontinuity_reading, '4dfad81a-7839-42f6-94fe-a3fc96b8fa99').
narrative_ontology:cs_kernel_codification('4dfad81a-7839-42f6-94fe-a3fc96b8fa99', fixed_text).
narrative_ontology:cs_authority_grounding('4dfad81a-7839-42f6-94fe-a3fc96b8fa99', lineage).
narrative_ontology:cs_interpretation_layer_present('4dfad81a-7839-42f6-94fe-a3fc96b8fa99').
narrative_ontology:cs_reading_relation('4dfad81a-7839-42f6-94fe-a3fc96b8fa99', correct_latin_kernel__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('4dfad81a-7839-42f6-94fe-a3fc96b8fa99', correct_latin_kernel__hybrid_reading, forecloses).
narrative_ontology:cs_axiom('4dfad81a-7839-42f6-94fe-a3fc96b8fa99', foundational, medieval_forms_are_corruptions).
narrative_ontology:cs_axiom_status(medieval_forms_are_corruptions, holdable).
narrative_ontology:cs_axiom_grounding('4dfad81a-7839-42f6-94fe-a3fc96b8fa99', medieval_forms_are_corruptions, empirically_contingent).
narrative_ontology:cs_axiom('4dfad81a-7839-42f6-94fe-a3fc96b8fa99', foundational, classical_norm_authoritative).
narrative_ontology:cs_axiom_status(classical_norm_authoritative, holdable).
narrative_ontology:cs_axiom_grounding('4dfad81a-7839-42f6-94fe-a3fc96b8fa99', classical_norm_authoritative, deontological).
narrative_ontology:cs_reference_frame('4dfad81a-7839-42f6-94fe-a3fc96b8fa99', classical_latin_norm).
narrative_ontology:cs_drift_state('4dfad81a-7839-42f6-94fe-a3fc96b8fa99', post_empirical_challenge, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4dfad81a-7839-42f6-94fe-a3fc96b8fa99', '').
narrative_ontology:cs_kernel_id(correct_latin_kernel__discontinuity_reading, correct_latin_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(correct_latin_kernel__discontinuity_reading, classical_philologists).
narrative_ontology:constraint_victim(correct_latin_kernel__discontinuity_reading, medievalist_scholars).
narrative_ontology:constraint_victim(correct_latin_kernel__discontinuity_reading, textual_critics).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the disciplinary boundary that treats Classical Latin as the sole correct norm and Medieval Latin as a corrupted derivative. Control curricula, critical edition standards, and hiring committees in European and North American philology departments. Their institutional prestige, professional identity, and resource flows depend on maintaining the classical norm as the recoverable kernel against which all later Latin is measured.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, classical_philologists, agenda_setter,
    institutional, generational, arbitrage, continental).

% Study medieval Latin texts, institutions, and culture. Must frame research as either recovering classical antecedents or apologizing for medieval deviation from the classical norm. Career advancement, publication acceptance, and departmental standing depend on evaluation standards set by classical philologists. Their scholarly identity is fused with texts that the constraint delegitimizes as corrupt.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, medievalist_scholars, payer,
    moderate, biographical, identity_locked, continental).

% Edit medieval Latin manuscripts for scholarly publication. Must produce stemmata and reconstruct hypothetical classical archetypes rather than presenting the medieval text as a witness to its own linguistic moment. Their editorial labor is structurally directed toward a classical target that the discontinuity reading posits as the true object of philology.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, textual_critics, payer,
    moderate, biographical, constrained, continental).

% Would treat medieval Latin variation as evidence of functional, rule-governed linguistic systems rather than corruption of a classical ideal. Their methodological premises are systematically excluded from philological editorial protocol and curriculum design, which treat sociolinguistic description as ancillary to reconstructive textual criticism.
narrative_ontology:constraint_stakeholder(correct_latin_kernel__discontinuity_reading, sociolinguists, excluded,
    moderate, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(correct_latin_kernel__discontinuity_reading, classical_philologists).
narrative_ontology:fixing_cost_class(correct_latin_kernel__discontinuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates textual criticism and language instruction across diverse manuscript traditions by positing a single, stable classical norm against which all variants can be measured, corrected, and pedagogically ordered.
% TRANSFER_FUNCTION: Moves scholarly labor, epistemic authority, and institutional prestige from medieval textual traditions and their interpreters to classical reconstruction projects and the philologists who administer the classical standard.
% ABSENT_VOICES: Medieval scribes and authors who experienced their Latin as functionally adequate; vernacular scholars whose languages were marginalized by the prestige economy of Latin reconstruction; sociolinguists who would treat variation as systemic rather than corrupt.
% DISAPPEARANCE_RATIONALE: If the discontinuity premise vanished overnight, medieval texts would be edited as witnesses to their own linguistic moment rather than reconstructed toward classical archetypes; philology departments would redistribute authority, curriculum, and funding toward medieval studies; the editorial labor now spent on stemmatic reconstruction would shift to historical sociolinguistic description.
% FOUNDING_PROBLEM: The proliferation of variant manuscript readings in the medieval period and the Renaissance need for a stable, authoritative language norm for education, theology, and statecraft across fragmented textual traditions.
% FOUNDING_PROBLEM_CORROBORATION: Classical philologists attest that manuscript variation was chaotic and required a classical anchor. Historians of medieval education and modern sociolinguists attest that medieval Latin functioned communicatively within its own institutional contexts; corroboration from outside the beneficiary set supports the view that the founding problem was partly manufactured by the prestige economy of Renaissance humanism.
narrative_ontology:disappearance_verdict(correct_latin_kernel__discontinuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(correct_latin_kernel__discontinuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(correct_latin_kernel__discontinuity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(correct_latin_kernel__discontinuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(correct_latin_kernel__discontinuity_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(correct_latin_kernel__discontinuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(correct_latin_kernel__discontinuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(correct_latin_kernel__discontinuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the discontinuity reading forces medievalists to perform a constant tax of justification: their objects of study are treated as derivative or corrupted, and their labor is directed toward classical archetypes rather than medieval realities. Suppression (0.68) is high because the constraint persists through active curricular gatekeeping, peer-review standards, and editorial protocol â not through spontaneous consensus. Theater_ratio (0.45) reflects moderate performative maintenance: much reconstruction work is genuine scholarly labor, but a substantial share involves ritually asserting the classical norm even where recovery is speculative. Accessibility_collapse (0.70) captures the near-total closure of alternative editorial methods (editing the medieval text as a witness to its own moment) within classical philology departments. Resistance (0.60) reflects ongoing medievalist and sociolinguistic pushback. The measurement series runs on one shared grid (0â600) with base_extractiveness rising as the constraint consolidated from Renaissance humanism through nineteenth-century scientific philology, theater_ratio peaking around 300 as the reconstructive method became ritualized, and suppression_requirement tracking the maturation and slight modern erosion of enforcement capacity.
 *
 * PERSPECTIVAL GAP:
 *   From the classical philologist seat, the constraint is experienced as necessary coordination around a standard that prevents editorial chaos and enables cross-manuscript comparison; from the medievalist or textual-critic seat, the same structure is experienced as enforced extraction that redirects their labor toward another era's language. The engine computes this divergence from the same structural data: identical metrics, opposite directionality.
 *
 * DIRECTIONALITY LOGIC:
 *   Classical philologists are structural beneficiaries: they collect prestige, curriculum centrality, and the authority to adjudicate correct Latin (d near the beneficiary end). Medievalist scholars and textual critics are structural targets: their labor and objects of study are subordinated to the classical norm (d near the target end). Sociolinguists, though structurally excluded, would experience high directionality if included because the constraint delegitimizes their core methodological premise. No override is needed: the structural derivation from beneficiary/victim declarations plus exit options (classical philologists are arbitrage; medievalists are identity_locked/constrained) already produces the correct asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â manuscript variation and the need for a stable language norm â was genuine in the Renaissance. However, the discontinuity reading outlived the problem's severity: by the nineteenth century, the classical norm functioned less as a solution to textual chaos than as a disciplinary hierarchy. The coordination function (a shared editorial standard) remains real, preventing a pure snare classification, while the extraction has accumulated beyond the original mandate, preventing a pure rope classification. The constraint is therefore best modeled as tangled rope: genuine coordination hybridized with asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    discontinuity_empirical_status,
    'Is the discontinuity between Classical and Medieval Latin a genuine structural linguistic break or a graded continuum obscured by disciplinary boundary-work?',
    'Comparative sociolinguistic analysis of medieval textual communities alongside quantitative stylometric measurement of morphosyntactic continuity across the late-antique and early-medieval corpus.',
    'If the break is graded rather than total, the discontinuity reading''s extraction is higher than its coordination, pushing classification toward snare; if total, the coordination function (reconstruction as genuine recovery) is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discontinuity_empirical_status, empirical, 'Empirical status of the Classical-Medieval Latin discontinuity').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the persistence of the discontinuity reading maintained by structural institutional gatekeeping or by internalized scholarly identity fusion?',
    'Track career trajectories of scholars who exit medievalist specializations for classical ones, measuring whether the discontinuity narrative persists absent institutional enforcement.',
    'If primarily internalized, effective suppression exceeds the structural measure and the constraint functions more as identity coordination; if structural, it is maintained by extractive institutional design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, conceptual, 'Structural vs internalized suppression mechanism').

omega_variable(
    kernel_reading_underdetermination,
    'Does the ''correct Latin'' kernel admit only the three recognized readings, or does a fourth reading (e.g., sociolinguistic pluralism) dissolve the kernel entirely?',
    'Historical sociology of philology tracking whether sociolinguistic approaches that reject the norm/recovery framework altogether are institutionally assimilable or remain permanently excluded.',
    'If the kernel dissolves under pluralist pressure, all three readings are revealed as commitment-system artifacts rather than empirical claims, collapsing the authority structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Whether the kernel is stable against pluralist dissolution').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(correct_latin_kernel__discontinuity_reading, 0, 600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(corr_tr_t0, correct_latin_kernel__discontinuity_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(corr_tr_t150, correct_latin_kernel__discontinuity_reading, theater_ratio, 150, 0.4).
narrative_ontology:measurement(corr_tr_t300, correct_latin_kernel__discontinuity_reading, theater_ratio, 300, 0.55).
narrative_ontology:measurement(corr_tr_t450, correct_latin_kernel__discontinuity_reading, theater_ratio, 450, 0.5).
narrative_ontology:measurement(corr_tr_t600, correct_latin_kernel__discontinuity_reading, theater_ratio, 600, 0.45).

% Extraction over time
narrative_ontology:measurement(corr_be_t0, correct_latin_kernel__discontinuity_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(corr_be_t150, correct_latin_kernel__discontinuity_reading, base_extractiveness, 150, 0.58).
narrative_ontology:measurement(corr_be_t300, correct_latin_kernel__discontinuity_reading, base_extractiveness, 300, 0.65).
narrative_ontology:measurement(corr_be_t450, correct_latin_kernel__discontinuity_reading, base_extractiveness, 450, 0.7).
narrative_ontology:measurement(corr_be_t600, correct_latin_kernel__discontinuity_reading, base_extractiveness, 600, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(corr_su_t0, correct_latin_kernel__discontinuity_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(corr_su_t150, correct_latin_kernel__discontinuity_reading, suppression_requirement, 150, 0.6).
narrative_ontology:measurement(corr_su_t300, correct_latin_kernel__discontinuity_reading, suppression_requirement, 300, 0.7).
narrative_ontology:measurement(corr_su_t450, correct_latin_kernel__discontinuity_reading, suppression_requirement, 450, 0.72).
narrative_ontology:measurement(corr_su_t600, correct_latin_kernel__discontinuity_reading, suppression_requirement, 600, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(correct_latin_kernel__discontinuity_reading, correct_latin_kernel__continuity_reading).
narrative_ontology:affects_constraint(correct_latin_kernel__discontinuity_reading, correct_latin_kernel__hybrid_reading).

% DUAL FORMULATION NOTE:
% The correct_latin_kernel decomposes into three structurally distinct constraints because the label 'correct Latin' conflates incompatible empirical claims about linguistic continuity. The discontinuity reading posits total distinctness; the continuity reading posits natural evolution; the hybrid reading posits partial continuity. Each reading carries a different epsilon, beneficiary structure, and axiomatic foundation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
