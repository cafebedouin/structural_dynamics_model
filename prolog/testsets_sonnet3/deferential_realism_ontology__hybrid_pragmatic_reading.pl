% ============================================================================
% CONSTRAINT STORY: deferential_realism_ontology__hybrid_pragmatic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_deferential_realism_ontology__hybrid_pragmatic_reading, []).

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
 *   constraint_id: deferential_realism_ontology__hybrid_pragmatic_reading
 *   human_readable: Deferential Realism Typology — Hybrid Pragmatic Reading (Fixed Core / Contested Periphery)
 *   domain: epistemology/normative_theory/institutional_design
 *
 * SUMMARY:
 *   This story is one reading of a contested kernel about what the
 *   deferential-realism constraint typology itself IS — an observational
 *   instrument, a persuasive vocabulary, or (this reading) a hybrid:
 *   observationally grounded at the core, normatively adjudicated at the
 *   periphery. The hybrid_pragmatic_reading holds that mountains and ropes
 *   classify consistently across interpretive communities because they are
 *   grounded in physical and minimal-coordination-overhead facts, while
 *   tangled_ropes and snares require a judgment about which beneficiaries are
 *   legitimate — a judgment the framework's maintainers make, not a fact the
 *   framework merely detects. This reading is itself a constraint: it
 *   structures who gets to adjudicate hard cases (the maintainers) and who
 *   bears the cost of that adjudication remaining unresolved (peripheral
 *   challengers). Two sibling constraints exist for the other readings of the
 *   same kernel: immutable_diagnostic_reading (periphery disagreement is
 *   measurement error, resolvable by better observation, not normative
 *   judgment) and rhetorical_scaffold_reading (the whole typology, core
 *   included, is a persuasive vocabulary whose value is rhetorical rather
 *   than descriptive). All three readings share the kernel_id
 *   deferential_realism_ontology but instantiate structurally different
 *   constraints with different beneficiary/victim sets and different epsilon:
 *   this reading's epsilon is moderate because the maintainers' adjudicatory
 *   authority is a real but partial extraction, riding on a genuinely stable
 *   core.
 *
 * KEY AGENTS:
 *   - framework_maintainers: primary agenda_setter — adjudicate contested periphery classifications, benefit from retained interpretive authority
 *   - core_classification_users: primary beneficiary — rely on the stable mountain/rope core, largely untouched by periphery disputes
 *   - peripheral_classification_challengers: primary target — bear the cost of unresolved periphery adjudication, no appeal outside the framework's own maintainers
 *   - rival_reading_communities: excluded — hold sibling readings of the same kernel, addressed in separate constraint stories
 *   - epistemic_observers: analytical seat — trace whether periphery normativity contaminates confidence in the core
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deferential_realism_ontology__hybrid_pragmatic_reading, 0.42).
domain_priors:suppression_score(deferential_realism_ontology__hybrid_pragmatic_reading, 0.48).
domain_priors:theater_ratio(deferential_realism_ontology__hybrid_pragmatic_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deferential_realism_ontology__hybrid_pragmatic_reading, tangled_rope).
narrative_ontology:human_readable(deferential_realism_ontology__hybrid_pragmatic_reading, "Deferential Realism Typology — Hybrid Pragmatic Reading (Fixed Core / Contested Periphery)").
narrative_ontology:topic_domain(deferential_realism_ontology__hybrid_pragmatic_reading, "epistemology/normative_theory/institutional_design").

domain_priors:requires_active_enforcement(deferential_realism_ontology__hybrid_pragmatic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(deferential_realism_ontology__hybrid_pragmatic_reading, 'bcd53436-7750-42ab-8652-8d6f4aa3b0b1').
narrative_ontology:cs_kernel_codification('bcd53436-7750-42ab-8652-8d6f4aa3b0b1', distributed).
narrative_ontology:cs_authority_grounding('bcd53436-7750-42ab-8652-8d6f4aa3b0b1', expertise).
narrative_ontology:cs_interpretation_layer_present('bcd53436-7750-42ab-8652-8d6f4aa3b0b1').
narrative_ontology:cs_reading_relation('bcd53436-7750-42ab-8652-8d6f4aa3b0b1', deferential_realism_ontology__immutable_diagnostic_reading, coexists_with).
narrative_ontology:cs_reading_relation('bcd53436-7750-42ab-8652-8d6f4aa3b0b1', deferential_realism_ontology__rhetorical_scaffold_reading, influences).
narrative_ontology:cs_axiom('bcd53436-7750-42ab-8652-8d6f4aa3b0b1', foundational, core_periphery_epistemic_asymmetry).
narrative_ontology:cs_axiom_status(core_periphery_epistemic_asymmetry, holdable).
narrative_ontology:cs_axiom_grounding('bcd53436-7750-42ab-8652-8d6f4aa3b0b1', core_periphery_epistemic_asymmetry, empirically_contingent).
narrative_ontology:cs_axiom('bcd53436-7750-42ab-8652-8d6f4aa3b0b1', foundational, periphery_classification_requires_legitimacy_judgment).
narrative_ontology:cs_axiom_status(periphery_classification_requires_legitimacy_judgment, holdable).
narrative_ontology:cs_axiom_grounding('bcd53436-7750-42ab-8652-8d6f4aa3b0b1', periphery_classification_requires_legitimacy_judgment, conventional).
narrative_ontology:cs_reference_frame('bcd53436-7750-42ab-8652-8d6f4aa3b0b1', core_periphery_epistemic_asymmetry).
narrative_ontology:cs_drift_state('bcd53436-7750-42ab-8652-8d6f4aa3b0b1', contemporary_classification_corpus_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('bcd53436-7750-42ab-8652-8d6f4aa3b0b1', '').
narrative_ontology:cs_kernel_id(deferential_realism_ontology__hybrid_pragmatic_reading, deferential_realism_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__hybrid_pragmatic_reading, framework_maintainers).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__hybrid_pragmatic_reading, core_classification_users).
narrative_ontology:constraint_victim(deferential_realism_ontology__hybrid_pragmatic_reading, peripheral_classification_challengers).
narrative_ontology:constraint_vindicates(deferential_realism_ontology__hybrid_pragmatic_reading, mountains_and_ropes_are_observationally_stable).
narrative_ontology:constraint_vindicates(deferential_realism_ontology__hybrid_pragmatic_reading, periphery_classification_requires_normative_adjudication).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain and revise the typology's classification rules, decide which signature-detection thresholds apply to contested cases (tangled_rope vs snare vs piton), and adjudicate disputes about beneficiary legitimacy at the periphery. They benefit from the framework's continued authority and from being the recognized arbiters of ambiguous cases; they can revise the rules but bear little cost when the periphery stays contested indefinitely.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, framework_maintainers, agenda_setter,
    institutional, generational, arbitrage, global).

% Rely on the stable mountain/rope core to ground everyday classification work — treaty analysis, market-structure review, protocol design. They benefit from the fixed core's reliability and rarely engage the contested periphery; their exit from the framework would mean losing a working vocabulary with no cheaper substitute.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, core_classification_users, beneficiary,
    organized, biographical, constrained, global).

% Analysts and advocates who apply the framework to genuinely contested cases (is this arrangement a tangled_rope or a snare?) and find their classification challenged, overruled, or absorbed by the maintainers' normative priors about legitimate beneficiaries. They pay the cost of the periphery's unresolved status: their analyses are treated as provisional or partisan even when structurally well-grounded, and they have no forum outside the framework's own maintainers to appeal to.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, peripheral_classification_challengers, payer,
    moderate, biographical, constrained, global).

% Communities committed to the immutable_diagnostic_reading (periphery is measurement error, not normative contest) or the rhetorical_scaffold_reading (the whole vocabulary is persuasive rather than descriptive) are not party to this reading's adjudication process. They would object that this reading either overclaims observational grounding at the core or underclaims rhetorical function at the periphery, but their objections are addressed in separate constraint stories, not inside this one.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, rival_reading_communities, excluded,
    organized, generational, mobile, global).

% Philosophers of science and institutional-design theorists who study how the typology handles the fixed-core/contested-periphery split without taking a side in any specific classification dispute. They can trace whether the periphery's normative dependence contaminates confidence in the core.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, epistemic_observers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(deferential_realism_ontology__hybrid_pragmatic_reading, framework_maintainers).
narrative_ontology:fixing_cost_class(deferential_realism_ontology__hybrid_pragmatic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared vocabulary that lets analysts distinguish constraints with negligible extraction and near-natural-law status (mountains, ropes) from those requiring active enforcement and asymmetric benefit (tangled_ropes, snares), enabling comparable classification across otherwise incommensurable domains.
% TRANSFER_FUNCTION: Moves interpretive authority from whoever classifies a contested arrangement to the framework's maintainers whenever a case falls in the periphery; the core's stability is not transferred anywhere because it requires no adjudication.
% ABSENT_VOICES: Immutable_diagnostic_reading holders would object that calling periphery classification 'normative judgment' concedes too much — they hold that better observation, not adjudication, resolves hard cases. Rhetorical_scaffold_reading holders would object that pretending the core is observationally fixed while the periphery is normatively contested draws an arbitrary and self-serving line that protects the framework's authority exactly where it is most exposed. Neither is present in this reading's own six-questions interview; they are addressed as sibling constraints.
% DISAPPEARANCE_RATIONALE: If the hybrid reading disappeared, users of the fixed core (mountain/rope classification of physical and coordination constraints) would likely notice little change — that work does not depend on this reading's specific account of the periphery. But the periphery-adjudication function — the practice of treating tangled_rope/snare classification as requiring beneficiary-legitimacy judgment rather than either pure measurement or pure rhetoric — would need to be replaced by one of the sibling readings, and which one replaces it changes who gets to decide contested cases. Framework maintainers and peripheral challengers disagree sharply about whether this matters.
% FOUNDING_PROBLEM: Early users of the typology found that some constraints (physical laws, minimal-overhead coordination mechanisms) classified cleanly and consistently across observers, while others (arrangements combining real coordination with asymmetric extraction) produced persistent disagreement that no amount of additional metric precision resolved — because the disagreement was about which beneficiaries count as legitimate, not about the facts of extraction.
% FOUNDING_PROBLEM_CORROBORATION: Independent replication attempts across different analyst teams (documented in the corpus's own cross-classification audits) show core-type agreement rates far exceeding periphery-type agreement rates, corroborating the split from outside the framework's own maintainers. No corroboration exists, however, for the specific claim that normative judgment (rather than measurement error or rhetorical framing) is the correct account of WHY the periphery disagrees — that remains attested only from within this reading.
narrative_ontology:disappearance_verdict(deferential_realism_ontology__hybrid_pragmatic_reading, contested).
narrative_ontology:founding_problem_status(deferential_realism_ontology__hybrid_pragmatic_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(deferential_realism_ontology__hybrid_pragmatic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(deferential_realism_ontology__hybrid_pragmatic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(deferential_realism_ontology__hybrid_pragmatic_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(deferential_realism_ontology__hybrid_pragmatic_reading_tests).
:- end_tests(deferential_realism_ontology__hybrid_pragmatic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) and suppression (0.48) are moderate, reflecting the story's claim that the framework is hybrid rather than uniformly extractive or uniformly natural: the core carries near-zero extraction (its stability is measured, not adjudicated) while the periphery carries real but partial extraction (adjudicatory authority is a genuine but bounded rent). Accessibility_collapse (0.4) is low-moderate because alternative framings of contested cases remain visible and contestable — that is precisely the periphery's defining feature under this reading. Resistance (0.55) is higher than accessibility_collapse because peripheral challengers actively contest specific classifications even though they cannot exit the framework's vocabulary altogether. Theater_ratio (0.28) is modest and rising slowly — some performative signature-detection activity accompanies genuine adjudication, but the framework is not primarily theatrical.
 *
 * DIRECTIONALITY LOGIC:
 *   Framework_maintainers sit near the beneficiary end: they collect interpretive authority and are rarely the ones whose classifications get overruled. Core_classification_users are near-symmetric beneficiaries: they get a working, stable vocabulary at low cost because they rarely touch the contested periphery. Peripheral_classification_challengers sit near the target end: their work is the site where the framework's own admitted normativity becomes a cost they bear repeatedly, with no external appeal. Rival_reading_communities are excluded rather than coordinated — under this reading their objections are structurally out of scope, not wrong, just addressed elsewhere.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (some constraints classify consistently, others do not, and the disagreement tracks beneficiary-legitimacy judgments rather than measurement precision) remains live: the corpus's own cross-classification audits keep reproducing the core/periphery agreement-rate split. This blocks mislabeling the whole typology as either purely extractive (which the rhetorical_scaffold_reading would claim) or purely observational (which the immutable_diagnostic_reading would claim) — the hybrid reading's classification as tangled_rope (not mountain, not snare) is the structurally honest middle: real coordination function (a stable core), real asymmetric benefit (maintainers' retained adjudicatory authority over the periphery), and active enforcement (adjudication is not self-executing; maintainers must actively rule on contested cases).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    core_periphery_split_is_itself_contested,
    'Is the fixed-core/contested-periphery split a genuine epistemic feature of the typology, or is it itself a normative choice that protects the framework''s authority by locating all contestation at a safe distance from the core claims maintainers care most about defending?',
    'Cross-community replication: if independent analyst communities untrained in this framework converge on the same core/periphery boundary when classifying novel constraints blind, the split is more likely structural; if the boundary shifts with which cases the analyst community has a stake in, the split is more likely self-protective.',
    'If the split is self-protective, this reading is itself better classified as a tangled_rope with a larger extraction share than authored here — the ''fixed core'' framing would be doing rhetorical work rather than describing a real epistemic asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(core_periphery_split_is_itself_contested, conceptual, 'Whether the core/periphery distinction is a discovered epistemic fact or a protective normative choice.').

omega_variable(
    beneficiary_legitimacy_adjudication_procedure,
    'What specific procedure do framework_maintainers use to decide which beneficiaries count as ''legitimate'' when classifying a periphery case as tangled_rope vs snare vs piton, and is that procedure itself contestable by peripheral_classification_challengers on equal footing?',
    'Document and audit actual adjudication decisions across a sample of contested classifications; check whether challengers'' counter-classifications receive comparable weight or are structurally subordinate to maintainer rulings.',
    'If the procedure gives challengers no comparable standing, the extraction this story authors (0.42) may be understated — periphery adjudication would function closer to a snare (maintainer-captured) than a tangled_rope (genuinely shared coordination with asymmetric cost).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_legitimacy_adjudication_procedure, empirical, 'Whether periphery adjudication is procedurally symmetric or maintainer-captured.').

omega_variable(
    hybrid_epsilon_measurement_validity,
    'Is it coherent to measure epsilon observationally for the core and constructively (via normative adjudication) for the periphery within a single typology, or does mixing measurement regimes within one framework undermine the claim that the core''s stability is independent of the periphery''s contestation?',
    'Formal audit of whether core classifications ever depend, even indirectly, on periphery adjudication outcomes (e.g., does a contested tangled_rope/snare ruling ever retroactively affect confidence in a mountain classification in the same domain).',
    'If core classifications are shown to depend on periphery rulings, the hybrid reading''s central claim — that the core is insulated from normative contest — fails, and this reading collapses toward the rhetorical_scaffold_reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(hybrid_epsilon_measurement_validity, conceptual, 'Whether hybrid epsilon measurement (observational core, constructed periphery) is internally coherent or contaminates the core.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deferential_realism_ontology__hybrid_pragmatic_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(defe_tr_t0, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(defe_tr_t4, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 4, 0.18).
narrative_ontology:measurement(defe_tr_t8, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 8, 0.21).
narrative_ontology:measurement(defe_tr_t12, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 12, 0.23).
narrative_ontology:measurement(defe_tr_t16, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 16, 0.25).
narrative_ontology:measurement(defe_tr_t20, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement(defe_tr_t24, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(defe_be_t0, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(defe_be_t4, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 4, 0.33).
narrative_ontology:measurement(defe_be_t8, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 8, 0.36).
narrative_ontology:measurement(defe_be_t12, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 12, 0.38).
narrative_ontology:measurement(defe_be_t16, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 16, 0.4).
narrative_ontology:measurement(defe_be_t20, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 20, 0.41).
narrative_ontology:measurement(defe_be_t24, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 24, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(defe_su_t0, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(defe_su_t4, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 4, 0.38).
narrative_ontology:measurement(defe_su_t8, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 8, 0.41).
narrative_ontology:measurement(defe_su_t12, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 12, 0.43).
narrative_ontology:measurement(defe_su_t16, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 16, 0.45).
narrative_ontology:measurement(defe_su_t20, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 20, 0.47).
narrative_ontology:measurement(defe_su_t24, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 24, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(deferential_realism_ontology__hybrid_pragmatic_reading, immutable_diagnostic_reading).
narrative_ontology:affects_constraint(deferential_realism_ontology__hybrid_pragmatic_reading, rhetorical_scaffold_reading).

% DUAL FORMULATION NOTE:
% This constraint, immutable_diagnostic_reading, and rhetorical_scaffold_reading form a three-member kernel family under deferential_realism_ontology. Each reading assigns a different epsilon regime to the same underlying practice of constraint classification: this reading (hybrid_pragmatic_reading) splits epsilon measurement by region (observational core, constructed periphery); immutable_diagnostic_reading treats epsilon as uniformly observational with disagreement as correctable error; rhetorical_scaffold_reading treats epsilon as uniformly constructed/rhetorical throughout, including at the 'core.' The three are linked bidirectionally via affects_constraints; each carries its own beneficiary/victim structure and its own stable epsilon, per the epsilon-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
