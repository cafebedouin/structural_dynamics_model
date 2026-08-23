% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_inevitability__strategic_lock_in_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence_inevitability__strategic_lock_in_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: qwerty_persistence_inevitability__strategic_lock_in_reading
 *   human_readable: QWERTY Persistence as Manufacturer-Engineered Strategic Lock-In
 *   domain: technology_history/political_economy/institutional_analysis
 *
 * SUMMARY:
 *   This constraint story instantiates the strategic_lock_in_reading of the
 *   qwerty_persistence_inevitability kernel. It models the QWERTY keyboard
 *   layout not as an efficient or accidental equilibrium, but as a
 *   deliberately engineered mechanism of manufacturer lock-in. The 1893
 *   typewriter cartel organized standardization, partnered with training
 *   institutions, and extracted rents through switching-cost barriers borne
 *   by typists and alternative-layout innovators. The sibling
 *   path_dependency_reading treats the same persistence as beneficiary-free
 *   accident-driven path dependency. The two readings are linked as a
 *   constraint family.
 *
 * KEY AGENTS:
 *   - typewriter_cartel_members: Primary agenda-setters (institutional/arbitrage) â engineered the standard and extracted rents
 *   - standardization_incumbents: Secondary beneficiaries (institutional/constrained) â inherited the installed base
 *   - professional_typists: Primary targets (powerless/trapped) â bear ergonomic costs and retraining barriers
 *   - alternative_layout_innovators: Excluded payers (moderate/trapped) â developed superior alternatives but locked out
 *   - typing_training_institutions: Enforcement partners (institutional/constrained) â perpetuated the standard through curriculum
 *   - economic_historians: Analytical observers (analytical/analytical) â adjudicate the competing readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.72).
domain_priors:suppression_score(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.68).
domain_priors:theater_ratio(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_inevitability__strategic_lock_in_reading, tangled_rope).
narrative_ontology:human_readable(qwerty_persistence_inevitability__strategic_lock_in_reading, "QWERTY Persistence as Manufacturer-Engineered Strategic Lock-In").
narrative_ontology:topic_domain(qwerty_persistence_inevitability__strategic_lock_in_reading, "technology_history/political_economy/institutional_analysis").

domain_priors:requires_active_enforcement(qwerty_persistence_inevitability__strategic_lock_in_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_inevitability__strategic_lock_in_reading, 'a9d168c6-319c-4f3e-8c7a-95c665a03cc2').
narrative_ontology:cs_kernel_codification('a9d168c6-319c-4f3e-8c7a-95c665a03cc2', implicit).
narrative_ontology:cs_authority_grounding('a9d168c6-319c-4f3e-8c7a-95c665a03cc2', extraction).
narrative_ontology:cs_reading_relation('a9d168c6-319c-4f3e-8c7a-95c665a03cc2', qwerty_persistence_inevitability__path_dependency_reading, coexists_with).
narrative_ontology:cs_axiom('a9d168c6-319c-4f3e-8c7a-95c665a03cc2', foundational, manufactured_standardization_extracts_rents).
narrative_ontology:cs_axiom_status(manufactured_standardization_extracts_rents, holdable).
narrative_ontology:cs_axiom_grounding('a9d168c6-319c-4f3e-8c7a-95c665a03cc2', manufactured_standardization_extracts_rents, empirically_contingent).
narrative_ontology:cs_axiom('a9d168c6-319c-4f3e-8c7a-95c665a03cc2', secondary, switching_cost_burden_falls_on_non_owners).
narrative_ontology:cs_axiom_status(switching_cost_burden_falls_on_non_owners, holdable).
narrative_ontology:cs_axiom_grounding('a9d168c6-319c-4f3e-8c7a-95c665a03cc2', switching_cost_burden_falls_on_non_owners, deontological).
narrative_ontology:cs_reference_frame('a9d168c6-319c-4f3e-8c7a-95c665a03cc2', cartel_enforced_standardization).
narrative_ontology:cs_drift_state('a9d168c6-319c-4f3e-8c7a-95c665a03cc2', digital_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a9d168c6-319c-4f3e-8c7a-95c665a03cc2', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_inevitability__strategic_lock_in_reading, qwerty_persistence_inevitability).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence_inevitability__strategic_lock_in_reading, typewriter_cartel_members).
narrative_ontology:constraint_beneficiary(qwerty_persistence_inevitability__strategic_lock_in_reading, standardization_incumbents).
narrative_ontology:constraint_victim(qwerty_persistence_inevitability__strategic_lock_in_reading, professional_typists).
narrative_ontology:constraint_victim(qwerty_persistence_inevitability__strategic_lock_in_reading, alternative_layout_innovators).
narrative_ontology:constraint_vindicates(qwerty_persistence_inevitability__strategic_lock_in_reading, manufactured_inevitability_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controlled typewriter manufacturing and distribution networks from the 1890s onward. Organized the 1893 standardization agreement, funded typing schools to teach exclusively QWERTY, and captured rents through equipment sales and market control. Could have adopted alternative layouts but enforced their own standard to maintain pricing power and foreclose competition.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, typewriter_cartel_members, agenda_setter,
    institutional, generational, arbitrage, global).

% Later manufacturers and technology firms that inherited the QWERTY installed base. Benefit from the existing standard without having borne the initial cartelization costs. Their equipment remains compatible with the global typing labor pool. Supporting alternative layouts would fragment the market and erode their incumbent advantage.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, standardization_incumbents, beneficiary,
    institutional, generational, constrained, global).

% Performed typing labor in offices, publishing houses, and government agencies. Required to learn and operate the QWERTY layout regardless of its ergonomic inefficiency. Bore repetitive strain injuries and suboptimal typing speeds relative to alternative layouts designed for human factors. Faced high retraining costs and loss of employability if switching to non-standard layouts. Had no collective voice in the standard-setting process.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, professional_typists, payer,
    powerless, biographical, trapped, global).

% Developed and patented alternative keyboard layouts that demonstrated superior ergonomic and speed metrics in experimental settings. Unable to achieve commercial adoption because typewriter manufacturers refused to produce compatible equipment, training institutions would not certify typists in new layouts, and employers would not hire non-QWERTY graduates. Their innovations were structurally frozen out by the incumbent standard.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, alternative_layout_innovators, payer,
    moderate, biographical, trapped, global).

% Business colleges, vocational schools, and employer training programs that taught typing as a professional skill. Taught QWERTY exclusively as the industry standard, certifying typists for the dominant layout. Curricula were shaped by manufacturer donations of equipment and by employer demand for interchangeable labor. Switching to an alternative layout would have required new machines, new teaching materials, and risked graduate unemployability.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, typing_training_institutions, agenda_setter,
    institutional, generational, constrained, global).

% Scholars analyzing the historical evolution of technical standards and market structure. Examine primary sources such as cartel agreements, patent filings, trade publications, and training curricula to assess whether QWERTY persistence was strategically engineered or emergent from decentralized user choice. Do not bear the constraint's costs or collect its benefits.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, economic_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qwerty_persistence_inevitability__strategic_lock_in_reading, typewriter_cartel_members).
narrative_ontology:fixing_cost_class(qwerty_persistence_inevitability__strategic_lock_in_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single interoperable keyboard layout standard across all typewriters and typing education, ensuring that any trained typist can operate any machine and any employer can hire from a universal labor pool without retraining costs.
% TRANSFER_FUNCTION: Moves rents from typists and alternative-layout innovators to the typewriter cartel and subsequent incumbents via enforced standardization; transfers ergonomic costs, retraining barriers, and suppressed innovation to typists and layout innovators.
% ABSENT_VOICES: Alternative layout innovators and ergonomic reform advocates were structurally excluded from the 1893 standardization process and from subsequent industry governance; professional typists had no representation in the cartel agreements that determined their working conditions and equipment.
% DISAPPEARANCE_RATIONALE: If the QWERTY lock-in vanished, keyboard layouts would compete on ergonomic and speed merits, training institutions would diversify curricula, the installed-base advantage would dissolve, alternative innovations that were suppressed would re-enter the market, and the rent stream to incumbents from switching-cost lock-in would collapse.
% FOUNDING_PROBLEM: Late 19th-century typewriter markets were fragmented across multiple incompatible keyboard layouts, preventing manufacturers from achieving scale economies and creating friction for employers hiring trained typists.
% FOUNDING_PROBLEM_CORROBORATION: Business historians corroborate the fragmentation problem from trade publications of the 1890s. However, ergonomics researchers and efficiency studies from the 1930s onward attest that the founding coordination problem was solved long before the mid-20th century, and the arrangement persisted well beyond its functional necessity. The persistence is contested: the strategic reading interprets it as extraction, while the path dependency reading interprets it as efficient equilibrium.
narrative_ontology:disappearance_verdict(qwerty_persistence_inevitability__strategic_lock_in_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence_inevitability__strategic_lock_in_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_inevitability__strategic_lock_in_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(qwerty_persistence_inevitability__strategic_lock_in_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence_inevitability__strategic_lock_in_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qwerty_persistence_inevitability__strategic_lock_in_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qwerty_persistence_inevitability__strategic_lock_in_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the constraint deliberately transfers ergonomic costs and switching barriers to typists while concentrating standardization rents with manufacturers. Suppression (0.68) reflects both the active cartel enforcement (training partnerships, equipment bundling) and the resulting collapse of alternative layout viability. Theater ratio (0.45) captures the manufactured 'inevitability' narrative that naturalizes what was strategically engineered. Accessibility collapse (0.70) is high because once the QWERTY standard was entrenched through training and equipment networks, alternatives became practically inaccessible. Resistance (0.40) is moderate: ergonomic reform movements and Dvorak advocacy mounted challenges but were systematically outmatched by incumbent coordination. The measurement series shows extraction and theater rising together as the cartel matured and the inevitability narrative naturalized.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats experience the constraint as genuine coordination (a universal standard solving fragmentation) and legitimate rent to its architects. The payer seats experience it as manufactured inevitability that imposes unnecessary costs and forecloses alternatives. The analytical seat sees both: the coordination function was real at founding, but its persistence beyond functional necessity and its asymmetric cost distribution reveal extraction layered onto coordination. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Cartel members and incumbents are declared beneficiaries with arbitrage-grade or constrained exit (they profit from the status quo and could theoretically adopt alternatives but gain from not doing so), placing them at the beneficiary end of the directionality spectrum. Professional typists are declared victims with trapped exit (high retraining costs and employability lock-in), placing them at the target end. Alternative layout innovators are also victims with trapped exit (their innovations cannot reach market due to installed-base lock-in). Training institutions are agenda-setters with constrained exit (their curricula are locked to employer demand and equipment availability). The effective extraction is thus amplified for typists and innovators and damped for manufacturers.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents the false binary between rope (pure coordination) and snare (pure extraction). The QWERTY arrangement DID solve a genuine coordination problem (fragmented layouts), but the same structure was weaponized for extraction: the coordination story is not merely cover (which would make it a snare), nor is it neutral coordination (which would make it a rope). The constraint requires active enforcement (training partnerships, cartel agreements, equipment bundling) to hold, and the enforcement machinery protects both the coordination function and the extraction simultaneously. The founding problem (fragmentation) is dead, but the arrangement persists because it now serves extraction â the mandatrophy is resolved by identifying the dual function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_strategic_vs_path,
    'Is QWERTY persistence the result of manufacturer-engineered strategic lock-in, or accident-driven path dependency without strategic beneficiaries?',
    'Archival discovery of manufacturer cartel meeting minutes, training partnership contracts, and internal correspondence documenting intent to create switching costs; or econometric analysis showing whether QWERTY''s persistence exceeded what network effects alone would predict.',
    'If strategic intent is documented, the path_dependency_reading loses its beneficiary-empty structure and the kernel collapses toward the strategic_lock_in_reading classification (tangled_rope or snare). If no strategic intent is found, this reading''s victim/beneficiary structure dissolves and the constraint reverts toward a lower-extraction type.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_strategic_vs_path, empirical, 'Kernel contest between strategic lock-in and path dependency readings').

omega_variable(
    suppression_mechanism_training_vs_inertia,
    'Does the measured suppression reflect active training-partnership enforcement by the cartel, or passive institutional inertia after the cartel dissolved?',
    'Temporal analysis of training curriculum contracts and manufacturer subsidies to typing schools; if suppression persists after documented cartel dissolution, reclassify toward passive path dependency; if tied to active cartel enforcement, confirm the strategic reading.',
    'Active enforcement supports the tangled_rope classification; passive inertia without active beneficiaries would shift the classification toward piton or mountain (if no beneficiaries remain).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_training_vs_inertia, empirical, 'Whether suppression is active cartel enforcement or passive inertia').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_inevitability__strategic_lock_in_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t0, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(qwer_tr_t20, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(qwer_tr_t40, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement(qwer_tr_t60, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 60, 0.38).
narrative_ontology:measurement(qwer_tr_t80, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 80, 0.42).
narrative_ontology:measurement(qwer_tr_t100, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 100, 0.45).

% Extraction over time
narrative_ontology:measurement(qwer_be_t0, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(qwer_be_t20, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(qwer_be_t40, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(qwer_be_t60, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 60, 0.65).
narrative_ontology:measurement(qwer_be_t80, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 80, 0.7).
narrative_ontology:measurement(qwer_be_t100, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 100, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(qwer_su_t0, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(qwer_su_t20, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(qwer_su_t40, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 40, 0.62).
narrative_ontology:measurement(qwer_su_t60, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 60, 0.65).
narrative_ontology:measurement(qwer_su_t80, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 80, 0.66).
narrative_ontology:measurement(qwer_su_t100, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 100, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_inevitability__strategic_lock_in_reading, information_standard).
narrative_ontology:affects_constraint(qwerty_persistence_inevitability__strategic_lock_in_reading, path_dependency_reading).

% DUAL FORMULATION NOTE:
% This constraint and path_dependency_reading are sibling readings of the qwerty_persistence_inevitability kernel. They share the referent (QWERTY keyboard layout persistence) but decompose into structurally distinct claims with different epsilon values, beneficiary/victim structures, and classifications per the epsilon-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
