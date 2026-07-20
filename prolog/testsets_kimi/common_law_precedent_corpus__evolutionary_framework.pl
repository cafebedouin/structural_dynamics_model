% ============================================================================
% CONSTRAINT STORY: common_law_precedent_corpus__evolutionary_framework
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_law_precedent_corpus__evolutionary_framework, []).

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
 *   constraint_id: common_law_precedent_corpus__evolutionary_framework
 *   human_readable: Common Law Precedent as Evolutionary Framework
 *   domain: legal/jurisprudential
 *
 * SUMMARY:
 *   This constraint instantiates the evolutionary_framework reading of the
 *   common_law_precedent_corpus kernel. Under this reading, precedent
 *   functions not as a rigid backward-looking constraint but as an adaptive
 *   framework that permits contemporary normative evolution and judicial
 *   reinterpretation. The judiciary is empowered as a normative updater, and
 *   precedent overruling is normalized as corrective development rather than
 *   extraordinary departure. This creates genuine coordination across the
 *   legal hierarchy (courts share a common corpus and methodology) but
 *   asymmetrically concentrates interpretive power in appellate courts while
 *   imposing uncertainty costs on litigants seeking stability and lower-court
 *   judges bound by shifting doctrine.
 *
 * KEY AGENTS:
 *   - Appellate judiciary: agenda_setter (institutional/constrained) â sets interpretive methodology and captures expanded normative authority
 *   - Trial judiciary: payer (moderate/constrained) â bears application costs and reversal risk under shifting precedent
 *   - Normative reform litigants: beneficiary (moderate/constrained) â gain pathways to challenge old precedent
 *   - Stability-seeking litigants: payer (moderate/constrained) â bear uncertainty and reliance costs
 *   - Textualist legal scholars: excluded (organized/analytical) â methodological alternative systematically marginalized
 *   - Comparative law observers: observer (analytical/analytical) â analytical seat outside the system
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_law_precedent_corpus__evolutionary_framework, 0.6).
domain_priors:suppression_score(common_law_precedent_corpus__evolutionary_framework, 0.48).
domain_priors:theater_ratio(common_law_precedent_corpus__evolutionary_framework, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, extractiveness, 0.6).
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_law_precedent_corpus__evolutionary_framework, tangled_rope).
narrative_ontology:human_readable(common_law_precedent_corpus__evolutionary_framework, "Common Law Precedent as Evolutionary Framework").
narrative_ontology:topic_domain(common_law_precedent_corpus__evolutionary_framework, "legal/jurisprudential").

domain_priors:requires_active_enforcement(common_law_precedent_corpus__evolutionary_framework).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_law_precedent_corpus__evolutionary_framework, '38fd766d-a6cf-4e75-b4a7-477288d4f771').
narrative_ontology:cs_kernel_codification('38fd766d-a6cf-4e75-b4a7-477288d4f771', distributed).
narrative_ontology:cs_authority_grounding('38fd766d-a6cf-4e75-b4a7-477288d4f771', lineage).
narrative_ontology:cs_interpretation_layer_present('38fd766d-a6cf-4e75-b4a7-477288d4f771').
narrative_ontology:cs_reading_relation('38fd766d-a6cf-4e75-b4a7-477288d4f771', common_law_precedent_corpus__strict_stare_decisis, forecloses).
narrative_ontology:cs_reading_relation('38fd766d-a6cf-4e75-b4a7-477288d4f771', common_law_precedent_corpus__pluralist_balancing, coexists_with).
narrative_ontology:cs_axiom('38fd766d-a6cf-4e75-b4a7-477288d4f771', foundational, contemporary_norms_license_precedent_revision).
narrative_ontology:cs_axiom_status(contemporary_norms_license_precedent_revision, holdable).
narrative_ontology:cs_axiom_grounding('38fd766d-a6cf-4e75-b4a7-477288d4f771', contemporary_norms_license_precedent_revision, conventional).
narrative_ontology:cs_axiom('38fd766d-a6cf-4e75-b4a7-477288d4f771', foundational, judiciary_empowered_as_normative_updater).
narrative_ontology:cs_axiom_status(judiciary_empowered_as_normative_updater, holdable).
narrative_ontology:cs_axiom_grounding('38fd766d-a6cf-4e75-b4a7-477288d4f771', judiciary_empowered_as_normative_updater, conventional).
narrative_ontology:cs_reference_frame('38fd766d-a6cf-4e75-b4a7-477288d4f771', adaptive_precedent_tradition).
narrative_ontology:cs_drift_state('38fd766d-a6cf-4e75-b4a7-477288d4f771', contemporary_legal_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('38fd766d-a6cf-4e75-b4a7-477288d4f771', '').
narrative_ontology:cs_kernel_id(common_law_precedent_corpus__evolutionary_framework, common_law_precedent_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__evolutionary_framework, appellate_judiciary).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__evolutionary_framework, normative_reform_litigants).
narrative_ontology:constraint_victim(common_law_precedent_corpus__evolutionary_framework, trial_judiciary).
narrative_ontology:constraint_victim(common_law_precedent_corpus__evolutionary_framework, stability_seeking_litigants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possesses broad authority to reinterpret precedent in light of contemporary norms. Frames overruling as corrective development rather than extraordinary departure. Benefits from institutional legitimacy while exercising normative updating power over the entire jurisdiction.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, appellate_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Must apply appellate precedent that may be reinterpreted or overruled without warning. Bears professional cost of having decisions reversed and the cognitive load of tracking evolving doctrinal standards across a shifting corpus.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, trial_judiciary, payer,
    moderate, biographical, constrained, national).

% Bring challenges to existing precedent seeking progressive or rights-expanding outcomes. Benefit from lowered barriers to overturning unfavorable precedent when contemporary norms have shifted since the original decision.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, normative_reform_litigants, beneficiary,
    moderate, biographical, constrained, national).

% Rely on settled precedent for commercial planning, property rights, or long-term legal arrangements. Face uncertainty and increased litigation costs when precedent is reinterpreted, reducing confidence in legal stability.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, stability_seeking_litigants, payer,
    moderate, biographical, constrained, national).

% Argue that precedent should bind according to fixed textual meaning or original understanding. Their methodological arguments are systematically marginalized within the evolutionary framework, treated as formalist relics rather than live interpretive options.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, textualist_legal_scholars, excluded,
    organized, generational, analytical, national).

% Study how different precedent regimes balance stability and change. Observe the divergence between strict and evolutionary approaches without being bound by either jurisdiction's interpretive commitments.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, comparative_law_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(common_law_precedent_corpus__evolutionary_framework, appellate_judiciary).
narrative_ontology:fixing_cost_class(common_law_precedent_corpus__evolutionary_framework, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates legal interpretation across time and hierarchy by providing a shared corpus of prior decisions that constrains arbitrary judicial discretion while permitting adaptive development of doctrine.
% TRANSFER_FUNCTION: Transfers interpretive authority from past decision-makers and bound litigants to the contemporary appellate judiciary, enabling normative updating at the cost of predictability and reliance.
% ABSENT_VOICES: Textualist jurists and strict stare decisis adherents are methodologically excluded; their arguments for fixed precedent meaning are treated as formally valid but substantively defeated by the evolutionary premise.
% DISAPPEARANCE_RATIONALE: If the evolutionary precedent framework vanished, appellate courts would lose the legitimating vocabulary for overruling prior decisions; lower courts would face radical uncertainty about binding authority; reform litigants would lose primary pathways for challenging settled doctrine; the legal system would lurch toward either rigid formalism or raw judicial discretion.
% FOUNDING_PROBLEM: How to maintain a functioning legal system that respects past decisions while adapting to changing social conditions and normative understandings, avoiding both arbitrary judicial power and sclerotic legal immobility.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and comparative jurists attest the problem is genuine and persistent. However, its current resolution via evolutionary framework is contested: textualist scholars and strict constructionists argue the problem is better solved through democratic legislation rather than judicial reinterpretation; these sources sit outside the beneficiary set of empowered appellate judges and reform litigants.
narrative_ontology:disappearance_verdict(common_law_precedent_corpus__evolutionary_framework, world_rearranges).
narrative_ontology:founding_problem_status(common_law_precedent_corpus__evolutionary_framework, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_law_precedent_corpus__evolutionary_framework, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(common_law_precedent_corpus__evolutionary_framework, 'none', 1).
narrative_ontology:epsilon_provenance(common_law_precedent_corpus__evolutionary_framework, 0.6, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_law_precedent_corpus__evolutionary_framework_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(common_law_precedent_corpus__evolutionary_framework, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(common_law_precedent_corpus__evolutionary_framework_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.30 to 0.60 over the interval because the evolutionary framework increasingly normalizes overruling and reinterpretation, concentrating judicial power while extracting predictability from stability-seeking parties. Suppression is moderate (0.48) because textualist alternatives are marginalized within mainstream legal discourse but not eliminated. Theater_ratio is low-moderate (0.32) because the performative aspect of claiming to follow precedent while actually updating it is present but not dominant; most judicial opinions sincerely attempt doctrinal continuity. Accessibility_collapse is moderate-to-high (0.60) because once the evolutionary framework is accepted, strict stare decisis arguments lose significant purchase in appellate advocacy. Resistance is moderate (0.55) due to persistent methodological opposition from textualists and originalists. The claim/metric gap is deliberate: claimed as tangled_rope (hybrid coordination/extraction) while metrics describe a trajectory toward higher extraction over time as normative updating becomes routine.
 *
 * PERSPECTIVAL GAP:
 *   The appellate judiciary experiences this constraint as a source of legitimate institutional power and coordinating authority. The trial judiciary and stability-seeking litigants experience it as a source of uncertainty and reversed expectations. Textualist scholars experience it as a silenced methodological alternative. The engine computes this divergence from the structural data; the authored claim does not adjudicate which seat is correct.
 *
 * DIRECTIONALITY LOGIC:
 *   Appellate judiciary and reform litigants occupy the beneficiary side (low directionality, subsidized by the constraint's flexibility). Trial judiciary and stability-seeking litigants occupy the target side (high directionality, bear the uncertainty costs). Textualist scholars are excluded from the directionality computation; their exclusion is part of the suppression mechanism that keeps the evolutionary framework stable.
 *
 * MANDATROPHY ANALYSIS:
 *   The framework prevents mandatrophy mislabeling by requiring both coordination function (shared precedent corpus enables hierarchical legal order) and asymmetric extraction (concentrated appellate power, diffuse uncertainty costs). Without the coordination component, this would be pure judicial discretion (snare). Without the extraction component, it would be rigid stare decisis (rope). The tangled_rope classification captures the hybrid.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    evolutionary_reinterpretation_boundary,
    'At what threshold does normative evolution through precedent reinterpretation become indistinguishable from judicial substitution of new rules for old?',
    'Comparative doctrinal analysis tracking the ratio decidendi of overruling decisions against the original holdings to measure interpretive continuity.',
    'If reinterpretation routinely departs from any plausible reading of the original holding, the constraint''s coordination function collapses and it approaches raw judicial discretion (snare-like).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(evolutionary_reinterpretation_boundary, conceptual, 'Boundary between adaptive interpretation and judicial legislation').

omega_variable(
    reading_family_ambiguity,
    'Does the evolutionary_framework reading foreclose strict_stare_decisis within a unified legal system, or can both readings coexist as methodological options for different judges?',
    'Empirical analysis of judicial opinions to determine whether judges self-identify as holding one or both frameworks simultaneously.',
    'If both are holdable by the same judge, the forecloses relation is overstated and should be coexists_with, altering the kernel''s structural classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_family_ambiguity, empirical, 'Whether strict and evolutionary readings are mutually exclusive or methodologically coextensive').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_law_precedent_corpus__evolutionary_framework, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clpe_tr_t0, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 0, 0.2).
narrative_ontology:measurement(clpe_tr_t10, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 10, 0.22).
narrative_ontology:measurement(clpe_tr_t20, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 20, 0.24).
narrative_ontology:measurement(clpe_tr_t30, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 30, 0.26).
narrative_ontology:measurement(clpe_tr_t40, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 40, 0.28).
narrative_ontology:measurement(clpe_tr_t50, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 50, 0.3).
narrative_ontology:measurement(clpe_tr_t60, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 60, 0.32).

% Extraction over time
narrative_ontology:measurement(clpe_be_t0, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(clpe_be_t10, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(clpe_be_t20, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(clpe_be_t30, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 30, 0.45).
narrative_ontology:measurement(clpe_be_t40, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 40, 0.5).
narrative_ontology:measurement(clpe_be_t50, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 50, 0.55).
narrative_ontology:measurement(clpe_be_t60, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 60, 0.6).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(common_law_precedent_corpus__evolutionary_framework, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_law_precedent_corpus__evolutionary_framework, enforcement_mechanism).
narrative_ontology:affects_constraint(common_law_precedent_corpus__evolutionary_framework, strict_stare_decisis).
narrative_ontology:affects_constraint(common_law_precedent_corpus__evolutionary_framework, pluralist_balancing).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the common_law_precedent_corpus kernel, which decomposes into structurally distinct claims: strict_stare_decisis (high rigidity, low judicial discretion), evolutionary_framework (adaptive, empowered judiciary), and pluralist_balancing (domain-varying). Each reading instantiates a different constraint with different beneficiary/victim structures and extraction profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
