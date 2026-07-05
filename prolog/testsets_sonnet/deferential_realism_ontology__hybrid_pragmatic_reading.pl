% ============================================================================
% CONSTRAINT STORY: deferential_realism_ontology__hybrid_pragmatic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: Deferential Realism Typology — Hybrid Pragmatic Reading (Fixed Core, Contested Periphery)
 *   domain: epistemology/normative_theory/institutional_design
 *
 * SUMMARY:
 *   The Deferential Realism apparatus classifies constraints into six types.
 *   This story examines the apparatus's own self-description under one
 *   contested reading of what kind of thing that apparatus IS. Under the
 *   hybrid pragmatic reading, mountains and ropes are diagnosed largely by
 *   observation (physical necessity, low coercion, net benefit to
 *   participants) and classification is stable across interpretive
 *   communities that otherwise disagree about everything else. Tangled ropes
 *   and snares, by contrast, require someone to judge whether a beneficiary
 *   is legitimate — and that judgment is not extractable from observation
 *   alone. Two economists can agree on every measured fact about a labor
 *   arrangement and still disagree about whether its beneficiaries are
 *   legitimate, because that disagreement is normative, not factual. The
 *   constraint here is the practice of using ONE typology, with ONE set of
 *   category labels, to cover both kinds of claim — treating 'mountain' and
 *   'snare' as members of a single natural kind when only one of them is
 *   diagnosed the way natural kinds are diagnosed.
 *
 * KEY AGENTS:
 *   - framework_maintainers: institutional agenda-setters who control the classification engine
 *   - policy_analysts_using_snare_diagnosis: organized beneficiaries who borrow the framework's core-level objectivity to ground periphery-level normative claims
 *   - institutions_contesting_snare_or_tangled_rope_labels: powerful payers who bear the cost of periphery contestedness with no observational tiebreaker available
 *   - communities_relying_on_stable_classification_for_legal_or_policy_action: powerless, trapped payers who need periphery resolution and get honest uncertainty instead
 *   - rival_interpretive_communities: excluded holders of the sibling readings
 *   - meta_theoretical_observers: analytical seat, including this story itself
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
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deferential_realism_ontology__hybrid_pragmatic_reading, tangled_rope).
narrative_ontology:human_readable(deferential_realism_ontology__hybrid_pragmatic_reading, "Deferential Realism Typology — Hybrid Pragmatic Reading (Fixed Core, Contested Periphery)").
narrative_ontology:topic_domain(deferential_realism_ontology__hybrid_pragmatic_reading, "epistemology/normative_theory/institutional_design").

domain_priors:requires_active_enforcement(deferential_realism_ontology__hybrid_pragmatic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(deferential_realism_ontology__hybrid_pragmatic_reading, '06efa299-b290-4b7e-bd61-c943bcc5a61e').
narrative_ontology:cs_kernel_codification('06efa299-b290-4b7e-bd61-c943bcc5a61e', distributed).
narrative_ontology:cs_authority_grounding('06efa299-b290-4b7e-bd61-c943bcc5a61e', practice).
narrative_ontology:cs_interpretation_layer_present('06efa299-b290-4b7e-bd61-c943bcc5a61e').
narrative_ontology:cs_reading_relation('06efa299-b290-4b7e-bd61-c943bcc5a61e', deferential_realism_ontology__immutable_diagnostic_reading, coexists_with).
narrative_ontology:cs_reading_relation('06efa299-b290-4b7e-bd61-c943bcc5a61e', deferential_realism_ontology__rhetorical_scaffold_reading, influences).
narrative_ontology:cs_axiom('06efa299-b290-4b7e-bd61-c943bcc5a61e', foundational, core_periphery_epistemic_asymmetry).
narrative_ontology:cs_axiom_status(core_periphery_epistemic_asymmetry, holdable).
narrative_ontology:cs_axiom_grounding('06efa299-b290-4b7e-bd61-c943bcc5a61e', core_periphery_epistemic_asymmetry, conventional).
narrative_ontology:cs_axiom('06efa299-b290-4b7e-bd61-c943bcc5a61e', foundational, periphery_classification_requires_ineliminable_normative_judgment).
narrative_ontology:cs_axiom_status(periphery_classification_requires_ineliminable_normative_judgment, holdable).
narrative_ontology:cs_axiom_grounding('06efa299-b290-4b7e-bd61-c943bcc5a61e', periphery_classification_requires_ineliminable_normative_judgment, deontological).
narrative_ontology:cs_axiom('06efa299-b290-4b7e-bd61-c943bcc5a61e', secondary, core_classification_is_observationally_convergent).
narrative_ontology:cs_axiom_status(core_classification_is_observationally_convergent, holdable).
narrative_ontology:cs_axiom_grounding('06efa299-b290-4b7e-bd61-c943bcc5a61e', core_classification_is_observationally_convergent, empirically_contingent).
narrative_ontology:cs_reference_frame('06efa299-b290-4b7e-bd61-c943bcc5a61e', dual_regime_typology_baseline).
narrative_ontology:cs_drift_state('06efa299-b290-4b7e-bd61-c943bcc5a61e', contemporary_corpus_expansion_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('06efa299-b290-4b7e-bd61-c943bcc5a61e', '').
narrative_ontology:cs_kernel_id(deferential_realism_ontology__hybrid_pragmatic_reading, deferential_realism_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__hybrid_pragmatic_reading, framework_maintainers).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__hybrid_pragmatic_reading, policy_analysts_using_snare_diagnosis).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__hybrid_pragmatic_reading, institutions_certified_as_mountain_or_rope).
narrative_ontology:constraint_victim(deferential_realism_ontology__hybrid_pragmatic_reading, institutions_contesting_snare_or_tangled_rope_labels).
narrative_ontology:constraint_victim(deferential_realism_ontology__hybrid_pragmatic_reading, analysts_whose_periphery_calls_are_overturned).
narrative_ontology:constraint_victim(deferential_realism_ontology__hybrid_pragmatic_reading, communities_relying_on_stable_classification_for_legal_or_policy_action).
narrative_ontology:constraint_vindicates(deferential_realism_ontology__hybrid_pragmatic_reading, core_periphery_distinction_is_epistemically_sound).
narrative_ontology:constraint_vindicates(deferential_realism_ontology__hybrid_pragmatic_reading, normative_judgment_is_ineliminable_from_extraction_diagnosis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain and revise the classification engine's signatures, thresholds, and gating rules. They decide what counts as sufficient structural evidence for tangled_rope or snare classification at the periphery, and they can adjust the apparatus itself when contested cases accumulate. They benefit from the framework's continued authority and use — their analytical labor is valued because the typology is treated as a working instrument, not because any single classification favors them personally.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, framework_maintainers, agenda_setter,
    institutional, generational, arbitrage, global).

% Use the typology's mountain/rope core to ground uncontroversial diagnostic claims (natural monopolies, physical scarcity) and reach for the tangled_rope/snare periphery to make extraction claims stick in policy debates. They gain rhetorical and analytical leverage from the framework's apparent objectivity, especially where the core classifications are genuinely stable.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, policy_analysts_using_snare_diagnosis, beneficiary,
    organized, biographical, mobile, national).

% Institutions whose arrangements the typology classifies as mountain or rope receive a kind of exoneration — their structure is treated as physically or coordinatively necessary rather than extractive. Where this core classification is genuinely stable across interpretive communities, this is not obviously unearned; where an institution's arrangement sits nearer the periphery but gets pulled toward the stable core by favorable framing, the exoneration is a live risk the reading itself flags.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, institutions_certified_as_mountain_or_rope, beneficiary,
    powerful, generational, mobile, global).

% Institutions classified as tangled_rope or snare bear reputational and material costs from the label — legal exposure, loss of legitimacy, regulatory attention. Because periphery classification depends on normative judgments about legitimate beneficiaries, contesting the label means contesting the judgment, not just the facts; the framework offers no purely observational appeal, only re-argument of the normative premise.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, institutions_contesting_snare_or_tangled_rope_labels, payer,
    powerful, biographical, constrained, national).

% Researchers and practitioners who classify a contested case as snare or tangled_rope, then see the call overturned or disputed by a rival interpretive community applying different normative priors about legitimate beneficiaries. Their labor and credibility absorb the cost of the periphery's genuine contestedness — there is no higher observational court to appeal to, only competing readings of the kernel.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, analysts_whose_periphery_calls_are_overturned, payer,
    moderate, biographical, constrained, national).

% Communities that need a stable snare/tangled_rope finding to ground legal remedy or policy relief discover that the periphery classification they depend on is exactly the part of the typology that is normatively contested and least settled. They cannot compel a resolution; the apparatus's own honesty about periphery contestedness (a virtue at the analytical level) becomes, for them, a source of delay and uncertainty they did not choose and cannot appeal past.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, communities_relying_on_stable_classification_for_legal_or_policy_action, payer,
    powerless, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(deferential_realism_ontology__hybrid_pragmatic_reading, communities_relying_on_stable_classification_for_legal_or_policy_action, excluded).

% Hold the immutable_diagnostic_reading or the rhetorical_scaffold_reading of the same kernel. They are not part of this reading's classification process — their alternative accounts of what the typology IS (a fixed observational instrument, or a persuasive vocabulary) are not adjudicated within this constraint but exist as competing claims on the same underlying commitment.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, rival_interpretive_communities, excluded,
    organized, generational, constrained, global).

% Study the typology's own operation as a constraint — asking whether the fixed-core/contested-periphery structure is itself best described as mountain (an unavoidable epistemic limit on any extraction-diagnosis apparatus), tangled_rope (coordination function for policy discourse mixed with rent extraction by whichever community controls the periphery calls), or something else. This story is one such observation.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, meta_theoretical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(deferential_realism_ontology__hybrid_pragmatic_reading, diffuse).
narrative_ontology:fixing_cost_class(deferential_realism_ontology__hybrid_pragmatic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared vocabulary and gating logic so that disparate observers can converge on classification for the easy cases (genuine natural limits, genuine low-coercion coordination) without re-litigating first principles each time, freeing normative debate to concentrate where it is actually needed — the periphery.
% TRANSFER_FUNCTION: Moves interpretive authority and reputational consequence: institutions classified at the stable core receive legitimacy transfers (exoneration from extraction scrutiny); institutions and analysts contesting periphery calls absorb the cost of unresolved normative disagreement, since no observational tiebreaker exists at that layer.
% ABSENT_VOICES: Communities awaiting a periphery classification to ground legal or policy remedy have no seat in the interpretive process that produces or contests that classification; they experience the honesty about contestedness as institutional non-delivery, not as epistemic virtue.
% DISAPPEARANCE_RATIONALE: If the hybrid pragmatic reading vanished, the fixed-core classifications (mountain, rope) would likely persist under whichever reading replaced it, since they are grounded in structural facts most readings agree on. But the specific practice of treating the periphery as OPENLY and STRUCTURALLY contested — rather than either resolvable by better observation (immutable_diagnostic_reading) or purely rhetorical (rhetorical_scaffold_reading) — would disappear, and with it the current distribution of who bears the cost of periphery disagreement. Framework maintainers and analysts using the typology dispute how much would actually change; communities awaiting remedy would likely see no difference either way.
% FOUNDING_PROBLEM: Extraction-diagnosis frameworks need to distinguish constraints that are unavoidable (physical, coordinative) from constraints that are chosen and defended for someone's benefit, without either falsely naturalizing extraction as necessity or falsely treating every coordination cost as illegitimate extraction.
% FOUNDING_PROBLEM_CORROBORATION: Independent institutional-design scholars outside the framework's own maintainer community corroborate that some form of mountain/snare distinction is needed to prevent both over- and under-diagnosis of extraction; however, whether THIS specific hybrid formulation — fixed core, openly contested periphery — is the correct resolution, versus the immutable_diagnostic or rhetorical_scaffold alternatives, is exactly what the sibling readings dispute. No party outside the three interpretive communities has adjudicated which reading is correct.
narrative_ontology:disappearance_verdict(deferential_realism_ontology__hybrid_pragmatic_reading, contested).
narrative_ontology:founding_problem_status(deferential_realism_ontology__hybrid_pragmatic_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(deferential_realism_ontology__hybrid_pragmatic_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness is moderate (0.42) and suppression medium (0.48) — the specified structural delta for this reading. Extraction rises modestly over the interval because the periphery classifications are increasingly relied upon in policy and legal contexts (analysts_using_snare_diagnosis, institutions_certified) even as their normative-judgment dependency remains undiminished, which is itself a mild rent: the apparatus's core-level credibility is partially borrowed to underwrite periphery-level claims. Theater ratio is low-to-moderate and rising slowly (0.15 to 0.28) reflecting increasing procedural elaboration (more gating signatures, more omega documentation) without a proportional increase in resolving power over the genuinely contested cases — some of that elaboration is functional refinement, some is performative rigor. Suppression is medium and rising gently, tracking the degree to which institutions certified at the stable core become harder to challenge over time (0.40 to 0.48) as the classification hardens into precedent.
 *
 * PERSPECTIVAL GAP:
 *   From the framework-maintainer seat, the hybrid structure looks like intellectual honesty — refusing to pretend the periphery is more settled than it is. From the seat of a community awaiting a snare finding to ground a remedy, the identical honesty looks like institutional failure to deliver a verdict. Both perceptions are structurally correct from their respective positions; the engine's per-seat computation is expected to diverge here rather than converge, and that divergence is itself evidence for the hybrid reading's core claim about where epistemic and normative components separate.
 *
 * DIRECTIONALITY LOGIC:
 *   Framework maintainers sit near full beneficiary: they control the apparatus and are not structurally exposed to a wrong periphery call in the way a classified institution is. Institutions certified mountain/rope are near-beneficiary by construction — the reading exonerates them, correctly when the core classification is sound, over-broadly if a periphery-adjacent case gets pulled into the stable core by favorable argument. Institutions contesting snare/tangled_rope labels and analysts whose periphery calls are overturned sit near the target end — they pay in reputational, legal, and professional currency for a disagreement that has no observational resolution mechanism. Communities relying on classification for remedy are the most target-like of all: powerless, trapped, and structurally unable to compel resolution of the very periphery contestedness the reading is honest about.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — distinguishing unavoidable constraint from defended extraction — remains live (status: live). The risk of mandatrophy here is specific: if the periphery's genuine contestedness hardens into settled precedent (institutions repeatedly certified core, classifications rarely revisited) without a corresponding hardening of the normative consensus that would justify that settling, the apparatus would be treating an unresolved normative question as resolved. The classification here (tangled_rope, not mountain and not snare) is deliberately chosen to prevent exactly that: it registers that the apparatus does coordinate (shared vocabulary, tractable core) genuine collective-action value while also asymmetrically extracting (legitimacy costs falling on periphery-classified institutions and on remedy-seeking communities) through the SAME structure, and that active maintenance (framework revision, precedent-setting, contested re-argument) is required to keep it from tipping toward pure normative theater in either direction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indeterminacy,
    'Is the deferential_realism_ontology kernel correctly read as hybrid (fixed core / contested periphery), as purely diagnostic (all misclassification is observational error), or as purely rhetorical (the whole typology is a persuasive vocabulary with no discovery function)?',
    'No single observation resolves this — it is a second-order question about what kind of epistemic activity classification itself is. Partial evidence: track whether periphery disagreements between interpretive communities converge over time with more data (favors immutable_diagnostic), remain stably divided along normative lines regardless of data (favors hybrid_pragmatic), or track policy fashion independent of both data and stable normative commitment (favors rhetorical_scaffold).',
    'If the immutable_diagnostic_reading is correct, this story''s claimed tangled_rope classification is wrong — the apparatus is better read as a mountain-like observational instrument with a temporarily under-resolved periphery, not a hybrid with structurally different epistemic status at core vs. periphery. If the rhetorical_scaffold_reading is correct, even the ''fixed core'' claimed here is overstated and the whole apparatus is closer to snare or tangled_rope at every level.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Which reading of the kernel is structurally correct — this story assumes hybrid_pragmatic but cannot settle the contest from within.').

omega_variable(
    core_periphery_boundary_stability,
    'Is the boundary between ''core'' (mountain/rope, observationally stable) and ''periphery'' (tangled_rope/snare, normatively contested) itself fixed, or does it drift as interpretive communities gain or lose the power to relocate contested cases into the stable core?',
    'Longitudinal tracking of specific contested cases (e.g., particular labor arrangements, platform fee structures) to see whether classification outcomes correlate more with new structural evidence or with shifts in which normative community holds institutional power over the classification apparatus.',
    'If the boundary drifts with power rather than with evidence, the ''fixed core'' claim is itself partially constructed rather than purely observational, which would push this reading toward the rhetorical_scaffold_reading and undermine the epsilon-invariance claimed for mountain/rope classifications specifically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(core_periphery_boundary_stability, empirical, 'Whether the core/periphery boundary is itself stable or subject to normative capture over time.').

omega_variable(
    legitimate_beneficiary_judgment_source,
    'When periphery classification depends on judging whether a beneficiary is ''legitimate,'' whose normative framework supplies that judgment — and is there any framework-external standard by which competing judgments could be adjudicated?',
    'Comparative institutional analysis: examine whether appeals to sources outside the immediate interpretive dispute (broad cross-cultural ethical consensus, revealed preference under genuine informed consent, absence of measurable coercion) narrow the range of reasonable disagreement in practice, even if they cannot fully eliminate it.',
    'If a framework-external standard exists and narrows disagreement substantially, the periphery is less irreducibly contested than this reading claims, pushing toward immutable_diagnostic. If no such standard exists even in principle, the periphery classification is closer to pure normative declaration, pushing toward rhetorical_scaffold.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimate_beneficiary_judgment_source, preference, 'Whether legitimate-beneficiary judgments have any framework-external adjudicating standard.').


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
narrative_ontology:measurement(defe_tr_t16, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 16, 0.24).
narrative_ontology:measurement(defe_tr_t20, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 20, 0.26).
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
narrative_ontology:measurement(defe_su_t0, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(defe_su_t4, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 4, 0.42).
narrative_ontology:measurement(defe_su_t8, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 8, 0.44).
narrative_ontology:measurement(defe_su_t12, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 12, 0.45).
narrative_ontology:measurement(defe_su_t16, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 16, 0.46).
narrative_ontology:measurement(defe_su_t20, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 20, 0.47).
narrative_ontology:measurement(defe_su_t24, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 24, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(deferential_realism_ontology__hybrid_pragmatic_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(deferential_realism_ontology__hybrid_pragmatic_reading, 0.1).
narrative_ontology:affects_constraint(deferential_realism_ontology__hybrid_pragmatic_reading, deferential_realism_ontology__immutable_diagnostic_reading).
narrative_ontology:affects_constraint(deferential_realism_ontology__hybrid_pragmatic_reading, deferential_realism_ontology__rhetorical_scaffold_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints instantiating readings of the deferential_realism_ontology kernel. immutable_diagnostic_reading treats the whole typology (including periphery cases) as an observational instrument with fixed referents, correctable by better measurement — no genuine normative contestedness, only current measurement error. rhetorical_scaffold_reading treats the whole typology, core included, as a persuasive vocabulary whose value is argumentative rather than discovery-oriented — 'mountain' and 'snare' are declared, not found, at every level. This hybrid_pragmatic_reading occupies the structural middle: it accepts the diagnostic reading's claim for mountain/rope and the scaffold reading's claim for tangled_rope/snare, producing a single typology with two different epistemic regimes inside it. Each of the three stories carries its own epsilon, its own beneficiary/victim structure (this one's beneficiaries and victims are drawn specifically from parties affected by the CORE/PERIPHERY split, which the sibling readings do not recognize as structurally real), and its own classification. They are linked here rather than merged because merging them would violate the epsilon-invariance principle: the three readings do not merely evaluate the same constraint via different observables, they assert different facts about what the constraint IS.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
