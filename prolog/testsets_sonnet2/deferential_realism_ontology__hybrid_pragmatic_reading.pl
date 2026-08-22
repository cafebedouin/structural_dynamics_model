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
 *   constraint_id: deferential_realism_ontology__hybrid_pragmatic_reading
 *   human_readable: Deferential Realism Typology — Hybrid Pragmatic Reading (Fixed Core / Contested Periphery)
 *   domain: epistemology/normative_theory/institutional_design
 *
 * SUMMARY:
 *   This story instantiates one reading of a contested kernel about what the
 *   six-category constraint typology IS. The hybrid pragmatic reading holds
 *   that the typology has a genuinely fixed, observation-grounded core
 *   (mountain and rope classifications track physical invariants and
 *   low-coercion coordination structures that any competent observer
 *   converges on) but an openly contested periphery (tangled_rope and snare
 *   classifications require a normative judgment about which beneficiaries
 *   are legitimate, and reasonable observers using the same structural data
 *   can and do disagree). This is distinct from the
 *   immutable_diagnostic_reading (which claims ALL six categories, including
 *   snare, are equally observation-grounded and that disagreement is
 *   measurement error) and the rhetorical_scaffold_reading (which claims NONE
 *   of the categories are discovered — the whole typology, including
 *   mountain, is a persuasive vocabulary deployed for policy critique). The
 *   extraction this constraint's OWN operation performs is the classificatory
 *   authority it exercises over peripheral cases: when the hybrid reading's
 *   users classify an arrangement as tangled_rope or snare, that verdict
 *   carries real consequences for the classified parties, and the verdict
 *   rests on a normative judgment the reading itself declares to be a
 *   judgment rather than a discovery — yet the classification is still
 *   delivered with the typology's institutional authority attached.
 *
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
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deferential_realism_ontology__hybrid_pragmatic_reading, tangled_rope).
narrative_ontology:human_readable(deferential_realism_ontology__hybrid_pragmatic_reading, "Deferential Realism Typology — Hybrid Pragmatic Reading (Fixed Core / Contested Periphery)").
narrative_ontology:topic_domain(deferential_realism_ontology__hybrid_pragmatic_reading, "epistemology/normative_theory/institutional_design").

domain_priors:requires_active_enforcement(deferential_realism_ontology__hybrid_pragmatic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(deferential_realism_ontology__hybrid_pragmatic_reading, 'e241fed7-0a33-43d1-8933-8158c9f44e56').
narrative_ontology:cs_kernel_codification('e241fed7-0a33-43d1-8933-8158c9f44e56', distributed).
narrative_ontology:cs_authority_grounding('e241fed7-0a33-43d1-8933-8158c9f44e56', expertise).
narrative_ontology:cs_interpretation_layer_present('e241fed7-0a33-43d1-8933-8158c9f44e56').
narrative_ontology:cs_reading_relation('e241fed7-0a33-43d1-8933-8158c9f44e56', deferential_realism_ontology__immutable_diagnostic_reading, coexists_with).
narrative_ontology:cs_reading_relation('e241fed7-0a33-43d1-8933-8158c9f44e56', deferential_realism_ontology__rhetorical_scaffold_reading, coexists_with).
narrative_ontology:cs_axiom('e241fed7-0a33-43d1-8933-8158c9f44e56', foundational, core_periphery_split_is_real).
narrative_ontology:cs_axiom_status(core_periphery_split_is_real, holdable).
narrative_ontology:cs_axiom_grounding('e241fed7-0a33-43d1-8933-8158c9f44e56', core_periphery_split_is_real, empirically_contingent).
narrative_ontology:cs_axiom('e241fed7-0a33-43d1-8933-8158c9f44e56', foundational, peripheral_beneficiary_legitimacy_is_irreducibly_normative).
narrative_ontology:cs_axiom_status(peripheral_beneficiary_legitimacy_is_irreducibly_normative, holdable).
narrative_ontology:cs_axiom_grounding('e241fed7-0a33-43d1-8933-8158c9f44e56', peripheral_beneficiary_legitimacy_is_irreducibly_normative, conventional).
narrative_ontology:cs_reference_frame('e241fed7-0a33-43d1-8933-8158c9f44e56', split_ontology_core_periphery_framework).
narrative_ontology:cs_drift_state('e241fed7-0a33-43d1-8933-8158c9f44e56', contemporary_corpus_application_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('e241fed7-0a33-43d1-8933-8158c9f44e56', '').
narrative_ontology:cs_kernel_id(deferential_realism_ontology__hybrid_pragmatic_reading, deferential_realism_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__hybrid_pragmatic_reading, framework_developers).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__hybrid_pragmatic_reading, institutional_analysts_using_core_categories).
narrative_ontology:constraint_victim(deferential_realism_ontology__hybrid_pragmatic_reading, actors_misclassified_at_the_periphery).
narrative_ontology:constraint_victim(deferential_realism_ontology__hybrid_pragmatic_reading, communities_whose_beneficiary_legitimacy_is_adjudicated_by_outsiders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain the typology's engine, choose the classifier thresholds and gating rules for tangled_rope/snare, and decide which structural facts (beneficiary declarations, enforcement flags) feed the computation. They benefit from the framework's adoption and citation while bearing little cost if peripheral calls are contested, since the core (mountain/rope) classifications remain defensible regardless.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, framework_developers, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(deferential_realism_ontology__hybrid_pragmatic_reading, framework_developers, beneficiary).

% Apply the typology to real institutions and constraints. They get genuine analytical leverage from the fixed core (nobody seriously disputes that gravity is a mountain or that a shared measurement standard is a rope), and can walk away from or bracket the periphery when it is too contested to be useful for their purposes.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, institutional_analysts_using_core_categories, beneficiary,
    organized, biographical, mobile, global).

% Institutions or practices that get labeled tangled_rope or snare based on a normative judgment about which beneficiaries count as legitimate. They cannot exit the classification process itself — the typology's periphery is adjudicated by the framework's users, not by them — and a snare verdict carries reputational and policy consequences even when the underlying beneficiary judgment is genuinely contestable.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, actors_misclassified_at_the_periphery, payer,
    moderate, biographical, constrained, national).

% Groups whose local arrangements (informal lending, kinship obligation networks, traditional resource allocation) get classified by external analysts using the hybrid reading's periphery logic. Whether their arrangement reads as coordination (rope) or extraction (snare) turns on which beneficiaries the classifying analyst treats as legitimate — a judgment made without their participation.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, communities_whose_beneficiary_legitimacy_is_adjudicated_by_outsiders, payer,
    powerless, generational, trapped, regional).

% The immutable_diagnostic and rhetorical_scaffold readings of the same kernel are not represented in this constraint's own classification process — they compete for adoption in the broader discourse but do not get a vote inside this reading's periphery adjudications.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, rival_typology_readings, excluded,
    organized, generational, mobile, global).

% Philosophers of science and institutional theorists studying the typology itself, including whether its fixed-core/contested-periphery structure is a coherent epistemic design or a way of laundering normative judgment as classification.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, meta_theoretic_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared vocabulary that lets analysts agree quickly on obvious cases (natural laws are mountains, voluntary standards are ropes) while reserving explicit, visible contest for genuinely hard cases (is this arrangement coordination with a rider of extraction, or pure extraction wearing a coordination story) — avoiding both false certainty and paralysis.
% TRANSFER_FUNCTION: Moves classificatory authority from the classified arrangement's own participants to the framework's users and developers at the periphery; moves interpretive stability (and the credibility that comes with it) to the framework's maintainers at the core.
% ABSENT_VOICES: The communities and institutions being classified at the periphery are rarely party to the classification decision itself; likewise, proponents of the rival readings (immutable_diagnostic, rhetorical_scaffold) do not participate in this reading's own adjudication process even though their accounts of what 'snare' means would change the verdict.
% DISAPPEARANCE_RATIONALE: If the hybrid reading vanished, the fixed-core classifications (mountain, rope) would likely persist under any successor framework since they track physical/coordination invariants independent of interpretive stance. But the periphery's contested classifications would either collapse into the immutable_diagnostic reading's false confidence or the rhetorical_scaffold reading's explicit advocacy framing — parties disagree about which outcome is worse, hence contested rather than a clean verdict.
% FOUNDING_PROBLEM: Early uses of the six-category typology treated all six types as equally observation-grounded, which invited both overconfident snare-labeling (treating a normative call as a discovered fact) and paralysis (refusing to classify anything contested at all). The hybrid reading was built to solve this by explicitly splitting the typology into an observationally-grounded core and an openly-adjudicated periphery.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by meta_theoretic_observers analyzing classification disputes across the corpus, who independently note that mountain/rope calls converge across interpretive communities while tangled_rope/snare calls do not — this is not merely asserted by framework_developers, who have an interest in the framework appearing well-designed.
narrative_ontology:disappearance_verdict(deferential_realism_ontology__hybrid_pragmatic_reading, contested).
narrative_ontology:founding_problem_status(deferential_realism_ontology__hybrid_pragmatic_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(deferential_realism_ontology__hybrid_pragmatic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extraction is moderate (0.42) because the reading's own core is low-extraction (fixed categories genuinely coordinate analytical convergence) while its periphery carries real extraction risk (adjudicated snare/tangled_rope verdicts affect classified parties without their participation). Suppression is medium (0.48) matching the expected structural delta — the reading does not suppress alternative periphery classifications outright (it acknowledges contest openly) but it does suppress the possibility that the classified party itself adjudicates its own case. Theater ratio is modest (0.28, rising slightly) reflecting some performative precision in periphery calls that are presented with the same apparent rigor as core calls despite resting on normative judgment. The measurement grid captures a slow drift toward more classificatory activity and more visible enforcement of the framework's own internal gates (tangled_rope/snare gating logic) as the framework matures and gets applied to more real cases.
 *
 * PERSPECTIVAL GAP:
 *   From the framework_developers' seat, the fixed-core/contested-periphery split is a considered epistemic design choice that improves on both naive alternatives. From the seat of communities being classified at the periphery, the same split looks like: 'the parts of this framework that are convenient to apply confidently are called discovered, and the parts that require judging us are called contested but applied anyway.' The engine's per-seat computation should reflect this asymmetry even though both seats are describing the same structural facts.
 *
 * DIRECTIONALITY LOGIC:
 *   Framework developers sit near the beneficiary end: they set the gating rules, control what counts as a valid beneficiary declaration, and benefit from the framework's continued use regardless of how any single peripheral case resolves. Institutional analysts applying the core categories are moderate beneficiaries — real coordination gain, low cost. The two payer groups are structurally different: actors_misclassified_at_the_periphery have moderate power and constrained exit (they can contest a classification but cannot exit the classification process), while communities_whose_beneficiary_legitimacy_is_adjudicated_by_outsiders are powerless and trapped — their own arrangements are read through a framework whose periphery-adjudication logic was built by, and is used by, people outside the community.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid reading's structure is itself a mandatrophy-prevention device: by declaring only the periphery contested, it avoids collapsing all classification into either naive discovery (immutable_diagnostic's failure mode — treating normative calls as measurement) or pure declaration (rhetorical_scaffold's failure mode — treating even physical invariants as rhetorical choices). But this only prevents mandatrophy IF the fixed/contested boundary itself is periodically re-examined; if the boundary calcifies (if today's genuinely contested periphery calls become tomorrow's unquestioned defaults), the hybrid reading would itself become a snare wearing the hybrid reading's own coordination story.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    core_periphery_boundary_stability,
    'Is the boundary between the typology''s ''fixed core'' (mountain, rope) and ''contested periphery'' (tangled_rope, snare) itself stable across interpretive communities, or does it shift depending on who is doing the classifying?',
    'Cross-community classification study: present the same set of ambiguous cases to multiple interpretive communities (framework developers, classified-party representatives, third-party auditors) and measure whether the core/periphery line is drawn in the same place.',
    'If the boundary is stable across communities, the hybrid reading''s central claim is empirically supported. If the boundary itself shifts (some communities treat cases the hybrid reading calls ''core'' as actually contested), the hybrid reading''s fixed core is smaller and more contested than it claims, moving this constraint''s structure toward the rhetorical_scaffold_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(core_periphery_boundary_stability, empirical, 'Whether the fixed-core/contested-periphery boundary is itself a stable observational fact or a further contested judgment.').

omega_variable(
    which_reading_is_itself_a_mountain_rope_or_snare,
    'Applying the typology reflexively to the three kernel readings themselves — is the hybrid_pragmatic_reading''s own dominance in the discourse a rope (genuine coordination value that earns its adoption) or a tangled_rope (coordination cover for the classificatory authority its developers exercise)?',
    'Track adoption patterns and dissent: does the hybrid reading persist because it demonstrably resolves more classification disputes correctly than its rivals, or because its developers and users have institutional positions that make dissent costly?',
    'If genuine coordination value dominates, this story''s tangled_rope claim is a conservative self-assessment (the reading is closer to a rope than it claims). If institutional capture dominates, the extraction figure (0.42) may understate the true extraction concentrated in the periphery-adjudication function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(which_reading_is_itself_a_mountain_rope_or_snare, conceptual, 'Whether the hybrid reading''s own persistence in the discourse is best explained by coordination value or by the classificatory authority it grants its developers.').

omega_variable(
    legitimate_beneficiary_judgment_universality,
    'Is there a culture-independent standard for what counts as a ''legitimate beneficiary'' that could ground the periphery classifications non-normatively, or is the normative judgment genuinely irreducible?',
    'Comparative institutional analysis across normative traditions: if independent traditions converge on the same legitimate-beneficiary judgments for a wide range of test cases, a non-normative floor may exist; persistent divergence would support irreducibility.',
    'A discovered floor would let some currently-contested periphery cases migrate into the fixed core, shrinking this reading''s periphery and raising its accessibility_collapse; confirmed irreducibility would validate treating the periphery as permanently and openly contested rather than as a temporary gap awaiting better observation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimate_beneficiary_judgment_universality, conceptual, 'Whether beneficiary legitimacy judgments are reducible to observation given enough analysis, or are irreducibly normative.').


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
narrative_ontology:measurement(defe_tr_t8, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement(defe_tr_t12, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 12, 0.22).
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
narrative_ontology:measurement(defe_su_t0, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(defe_su_t4, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 4, 0.4).
narrative_ontology:measurement(defe_su_t8, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 8, 0.42).
narrative_ontology:measurement(defe_su_t12, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 12, 0.44).
narrative_ontology:measurement(defe_su_t16, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 16, 0.46).
narrative_ontology:measurement(defe_su_t20, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 20, 0.47).
narrative_ontology:measurement(defe_su_t24, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 24, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(deferential_realism_ontology__hybrid_pragmatic_reading, identity_coordination).
narrative_ontology:affects_constraint(deferential_realism_ontology__hybrid_pragmatic_reading, deferential_realism_ontology__immutable_diagnostic_reading).
narrative_ontology:affects_constraint(deferential_realism_ontology__hybrid_pragmatic_reading, deferential_realism_ontology__rhetorical_scaffold_reading).

% DUAL FORMULATION NOTE:
% Three constraints form the deferential_realism_ontology kernel family, each a distinct reading of what the six-category typology fundamentally is. This story (hybrid_pragmatic_reading) claims a split ontology: core categories are discovered, periphery categories are normatively adjudicated. immutable_diagnostic_reading claims all six categories are equally discovered (denies the periphery is normative at all — lower suppression, since it treats disagreement as pure measurement error rather than legitimate contest). rhetorical_scaffold_reading claims none of the six categories are discovered (treats even mountain/rope as persuasive vocabulary — this would produce a very different epsilon profile, likely lower observed extraction from the typology's OWN operation since on that reading the typology makes no discovery claims to defend, but higher extraction risk in its rhetorical deployment against classified parties). The three stories are not measuring the same epsilon from different angles; they are three structurally distinct claims about the typology's nature, linked here per the ε-invariance decomposition rule.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
