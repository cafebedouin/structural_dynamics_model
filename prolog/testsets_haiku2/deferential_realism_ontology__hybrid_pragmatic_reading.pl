% ============================================================================
% CONSTRAINT STORY: deferential_realism_ontology__hybrid_pragmatic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: deferential_realism_ontology__hybrid_pragmatic_reading
 *   human_readable: Deferential Realism Typology: Hybrid Pragmatic Reading
 *   domain: epistemology/normative_theory/institutional_design
 *
 * SUMMARY:
 *   The constraint typology (mountains, ropes, tangled_ropes, snares,
 *   scaffolds, pitons) purports to be an observational instrument for
 *   classifying institutional arrangements. The hybrid pragmatic reading
 *   claims that the core categories (mountains and ropes) are observationally
 *   stable—their reference frames hold across epistemic communities—but the
 *   contested periphery (tangled_ropes and snares) depends on normative
 *   judgments about which beneficiaries count as legitimate. This reading
 *   coordinates between institutional designers who need a usable typology
 *   and empiricists who demand observational grounding. It does so by
 *   explicitly naming normativity in the periphery while claiming the core
 *   remains fact-based. The reading's beneficiaries are pragmatist
 *   interpreters and institutional designers; its victims are diagnosticists
 *   seeking fixed referents and critical scholars who demand the entire
 *   framework be transparently normative. The tension between these positions
 *   is suppressed—administratively routed to footnotes—by treating pragmatism
 *   as neutral rather than as itself a normative choice.
 *
 * KEY AGENTS:
 *   - pragmatist_interpreters: institutional beneficiaries of the framework's hybridity; they gain legitimacy by appearing systematic without claiming false precision
 *   - institutional_designers_using_hybrid_model: organizational actors who deploy the reading to justify governance choices while appearing objective
 *   - diagnosticists_seeking_fixed_referents: philosophers and scientists who view the reading's normativity acknowledgment as undermining the typology's standing as observational
 *   - critical_scholars_demanding_explicit_normativity: researchers who reject the reading's residual claim to observational core and demand the entire framework be treated as normatively constructed
 *   - constraint_typology_governance_authority: administrative seat that enforces which readings are canonical and which are peripheral
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deferential_realism_ontology__hybrid_pragmatic_reading, 0.62).
domain_priors:suppression_score(deferential_realism_ontology__hybrid_pragmatic_reading, 0.58).
domain_priors:theater_ratio(deferential_realism_ontology__hybrid_pragmatic_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deferential_realism_ontology__hybrid_pragmatic_reading, tangled_rope).
narrative_ontology:human_readable(deferential_realism_ontology__hybrid_pragmatic_reading, "Deferential Realism Typology: Hybrid Pragmatic Reading").
narrative_ontology:topic_domain(deferential_realism_ontology__hybrid_pragmatic_reading, "epistemology/normative_theory/institutional_design").

domain_priors:requires_active_enforcement(deferential_realism_ontology__hybrid_pragmatic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(deferential_realism_ontology__hybrid_pragmatic_reading, 'c7802638-c13d-4490-8695-f804b1614132').
narrative_ontology:cs_kernel_codification('c7802638-c13d-4490-8695-f804b1614132', distributed).
narrative_ontology:cs_authority_grounding('c7802638-c13d-4490-8695-f804b1614132', distributed).
narrative_ontology:cs_reading_relation('c7802638-c13d-4490-8695-f804b1614132', deferential_realism_ontology__immutable_diagnostic_reading, coexists_with).
narrative_ontology:cs_reading_relation('c7802638-c13d-4490-8695-f804b1614132', deferential_realism_ontology__rhetorical_scaffold_reading, coexists_with).
narrative_ontology:cs_axiom('c7802638-c13d-4490-8695-f804b1614132', foundational, constraint_typology_has_observational_core).
narrative_ontology:cs_axiom_status(constraint_typology_has_observational_core, holdable).
narrative_ontology:cs_axiom_grounding('c7802638-c13d-4490-8695-f804b1614132', constraint_typology_has_observational_core, empirically_contingent).
narrative_ontology:cs_axiom('c7802638-c13d-4490-8695-f804b1614132', foundational, peripheral_classification_depends_on_legitimate_beneficiary_judgment).
narrative_ontology:cs_axiom_status(peripheral_classification_depends_on_legitimate_beneficiary_judgment, holdable).
narrative_ontology:cs_axiom_grounding('c7802638-c13d-4490-8695-f804b1614132', peripheral_classification_depends_on_legitimate_beneficiary_judgment, deontological).
narrative_ontology:cs_axiom('c7802638-c13d-4490-8695-f804b1614132', secondary, pragmatic_hybridity_enables_cross_community_governance).
narrative_ontology:cs_axiom_status(pragmatic_hybridity_enables_cross_community_governance, holdable).
narrative_ontology:cs_axiom_grounding('c7802638-c13d-4490-8695-f804b1614132', pragmatic_hybridity_enables_cross_community_governance, instrumental).
narrative_ontology:cs_reference_frame('c7802638-c13d-4490-8695-f804b1614132', constraint_typology_as_unified_observational_and_normative_instrument).
narrative_ontology:cs_drift_state('c7802638-c13d-4490-8695-f804b1614132', contemporary_institutional_deployment, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c7802638-c13d-4490-8695-f804b1614132', '').
narrative_ontology:cs_kernel_id(deferential_realism_ontology__hybrid_pragmatic_reading, deferential_realism_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__hybrid_pragmatic_reading, pragmatist_interpreters).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__hybrid_pragmatic_reading, institutional_designers_using_hybrid_model).
narrative_ontology:constraint_victim(deferential_realism_ontology__hybrid_pragmatic_reading, diagnosticists_seeking_fixed_referents).
narrative_ontology:constraint_victim(deferential_realism_ontology__hybrid_pragmatic_reading, critical_scholars_demanding_explicit_normativity).
narrative_ontology:constraint_vindicates(deferential_realism_ontology__hybrid_pragmatic_reading, constraint_typology_has_observational_core).
narrative_ontology:constraint_vindicates(deferential_realism_ontology__hybrid_pragmatic_reading, classification_periphery_depends_on_value_judgments).
narrative_ontology:constraint_vindicates(deferential_realism_ontology__hybrid_pragmatic_reading, pragmatic_hybridity_enables_cross_community_work).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Communities of scholars and practitioners who adopt the hybrid pragmatic reading as their primary interpretive frame. They benefit from appearing systematic (the observational core claim) while avoiding the burden of proving pure objectivity (acknowledged by the normative periphery claim). Their professional standing depends on sustained publication and citation within the typology's institutional structure. Exit is academically available—they could shift to alternative frameworks—but their sunk investment in constraint-typology work makes exit costly.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, pragmatist_interpreters, beneficiary,
    institutional, generational, mobile, global).

% Policymakers, governance architects, and organizational leaders who use the hybrid reading to justify institutional design decisions. The reading permits them to claim systematic grounding (the observational core) while sidesteping accusations of arbitrary normativity (the periphery acknowledgment). They deploy the typology to classify policy arrangements as mountains/ropes (unchallengeable), contested tangles, or snares. Their exit is organizationally embedded—changing frameworks requires coalition-building across stakeholders with different epistemic commitments.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, institutional_designers_using_hybrid_model, beneficiary,
    organized, biographical, constrained, national).

% Philosophers, logicians, and empirical scientists committed to the immutable_diagnostic_reading—the claim that constraint classification is observational with fixed referents across all categories. They view the hybrid reading's embrace of normativity in the periphery as epistemically indefensible; they argue the typology should either be purely observational or abandoned. Their cost is being routed to a secondary interpretive position; the hybrid reading's institutional dominance makes their work harder to publish and cite. Their high exit mobility (alternative frameworks exist) is offset by substantial sunk investment in constraint-typology diagnostics.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, diagnosticists_seeking_fixed_referents, payer,
    powerful, generational, arbitrage, global).

% Researchers in critical theory, postcolonial studies, political economy, and value-laden institutional critique who hold the rhetorical_scaffold_reading—the claim that the entire typology is normatively constructed and should be transparently designed as a vocabulary for policy analysis. They reject the hybrid reading's residual claim to observational core as obscuring whose norms are being preserved. Their cost is institutional exclusion: the hybrid reading dominates governance conversations, making it harder for their work to influence policy. Academic exit is available but their public-facing work would lose institutional amplification.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, critical_scholars_demanding_explicit_normativity, payer,
    moderate, generational, mobile, global).

% Communities working with constraint models in physics, biology, systems engineering, and computer science. They treat mountains and certain ropes as observational categories without engaging the normativity debates that dominate institutional design contexts. Their position is closest to the immutable_diagnostic_reading but they remain largely outside the policy implications where the hybrid reading creates extraction. They have analytical standing independent of which reading prevails, though their work is sometimes cited to support the hybrid reading's claim that the observational core exists.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, empirical_scientists_and_engineers, observer,
    institutional, generational, analytical, global).

% The collective institutional structures maintaining the constraint typology as a living framework—academic journals, conference organizing bodies, textbook publishers, the Deferential Realism engine development community, the corpus-management authorities. They enforce which readings are treated as canonical in new constraint stories, which observables count as valid measurements, and which boundary disputes are resolvable vs. definitionally unsettled. Their situation is entirely bound to the framework's persistence; exit means framework dissolution.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, constraint_typology_governance_authority, agenda_setter,
    institutional, generational, trapped, global).

% Competing ontologies for classifying institutional arrangements—formal game theory, Marxist institutional analysis, rational choice economics, complexity theory, organizational sociology. They would compete for explanatory territory and institutional resources if the constraint typology's hegemonic status were open to challenge. Their exclusion is maintained by the typology's institutional embedding in governance discourse and the switching cost required to translate between frameworks. They have no seat at the table where the hybrid reading's canon is enforced.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, alternative_institutional_frameworks, excluded,
    powerful, generational, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(deferential_realism_ontology__hybrid_pragmatic_reading, pragmatist_interpreters).
narrative_ontology:fixing_cost_class(deferential_realism_ontology__hybrid_pragmatic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified language for institutional constraint classification that cut across epistemic communities (empiricists, philosophers, policymakers) and policy domains. Solves the coordination problem: how can a single framework be simultaneously grounded in observation (for mountains/ropes) and acknowledge normative construction (for tangled_ropes/snares)? Enables conversation across the diagnosticist-pragmatist-critical divide by explicitly naming where each side's concerns are valid.
% TRANSFER_FUNCTION: Transfers interpretive authority from pure diagnosticists (who demand fixed observational referents) and critical scholars (who demand explicit normativity) to pragmatist institutional designers. The transfer is accomplished by presenting the hybrid position as epistemically neutral (pragmatism as compromise) while administratively suppressing both pure-objectivity and pure-normativity alternative framings. Pragmatists gain standing to classify peripheral cases without bearing the defensive burden of either pure position; diagnosticists and critical scholars are routed to secondary interpretive status.
% ABSENT_VOICES: Representatives of the immutable_diagnostic_reading would argue the typology should be purely observational or abandoned; they are excluded from the primary classification debates because the hybrid reading is dominant. Representatives of the rhetorical_scaffold_reading would argue the entire typology is normative and should be transparently designed as such; they are excluded because the hybrid reading retains the language of observational core, making their critique appear to deny empirical facts rather than making an epistemological point.
% DISAPPEARANCE_RATIONALE: If the hybrid pragmatic reading vanished, institutional designers would face naked the choice the reading conceals: either adopt pure diagnosticism (deny peripheral normativity, risking false objectivity claims) or adopt pure rhetorical scaffold framing (accept all normativity, risking loss of epistemic standing). The unified language for cross-domain constraint classification would fragment. The governance authority's administrative work would intensify—without the hybrid reading's frame, every peripheral classification dispute would become a foundational epistemological battle. Alternative frameworks (game theory, Marxist analysis) would gain relative standing. The observational core would retain explanatory power but would lose its role in legitimating peripheral classification decisions.
% FOUNDING_PROBLEM: Early constraint analysis discovered an irreducible tension: mountains and ropes partition stably across interpretive communities and empirical domains (supporting the immutable_diagnostic reading), but snares and tangled_ropes' boundaries shift with changes in what counts as legitimate beneficiaries (supporting the rhetorical_scaffold reading). Institutional designers needed a typology that could appear unified while accommodating both discoveries. The pragmatic compromise: frame the core as observational and the periphery as normative, permitting both empiricists and policymakers to draw classification lines using the same vocabulary without resolving their underlying epistemic disagreement.
% FOUNDING_PROBLEM_CORROBORATION: Empirical scientists studying phase-space dynamics, coordination problems, and evolutionary constraints (independent of policy questions) attest that mountains and ropes continue to classify correctly across unrelated domains—the observational core persists. Institutional designers and comparative policy analysts attest that snare/tangled_rope boundary disputes recur in every new policy domain they encounter (labor, platforms, public health, trade)—the normativity question remains live. Critical scholars from the rhetorical_scaffold reading attest the founding problem has not been solved but administratively suppressed—the hybrid reading's institutional dominance forces their position to the margins. Diagnosticists attest the hybrid reading compromises the typology's epistemic standing by admitting normativity, but they lack institutional power to change the canon.
narrative_ontology:disappearance_verdict(deferential_realism_ontology__hybrid_pragmatic_reading, world_rearranges).
narrative_ontology:founding_problem_status(deferential_realism_ontology__hybrid_pragmatic_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(deferential_realism_ontology__hybrid_pragmatic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(deferential_realism_ontology__hybrid_pragmatic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(deferential_realism_ontology__hybrid_pragmatic_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(deferential_realism_ontology__hybrid_pragmatic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(deferential_realism_ontology__hybrid_pragmatic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(deferential_realism_ontology__hybrid_pragmatic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness measurement (0.62) captures the reading's asymmetric transfer of interpretive authority: pragmatists gain institutional standing by appearing to compromise between observation and normativity, but the compromise itself is hidden—it is not negotiated as a value choice but presented as epistemic inevitability. The suppression value (0.58) reflects medium institutional and epistemic friction: the governance authority actively polices which stories are admitted under the reading, and the framework structure itself makes it harder to articulate pure diagnosticist or pure-critical positions. The theater ratio (0.41) is elevated because a substantial portion of the reading's function is performative—maintaining the appearance of objectivity about the core while concealing the normativity work in the periphery. Accessibility collapse (0.65) is moderate: diagnosticists and critical scholars retain academic exit paths, but their standing in institutional design discourse is compressed. Resistance (0.72) is high because the reading faces sustained pushback from both pure-objectivity camps (who see it as caving to normativity) and radical-normativity camps (who see it as hiding normativity under a pragmatism label). The measurement series show extraction and theater rising over time—as the reading becomes more institutionalized, more work goes into maintaining its appearance of hybrid neutrality while suppressing the normative choices it embodies.
 *
 * PERSPECTIVAL GAP:
 *   From the pragmatist institutional designer's seat, the reading appears as a genuine compromise: it preserves observational rigor for mountains/ropes, acknowledges value judgments for snares/tangled_ropes, and enables governance without false precision. From the diagnosticist's seat, the same reading appears as illegitimate capitulation: once normativity is admitted into the typology at all, the boundary between 'observational core' and 'constructed periphery' becomes questionable. From the critical scholar's seat, the reading appears as sophisticated suppression: it pretends to acknowledge normativity while actually hiding it—the pragmatic framing makes normative choices appear inevitable and unquestionable. The engine should compute these as separate per-seat types: from the pragmatist seat, tangled_rope (coordinating observation with normativity); from the diagnosticist seat, snare (extraction of interpretive authority through false hybridity); from the critical seat, rope-masquerading-as-mountain (performative neutrality).
 *
 * DIRECTIONALITY LOGIC:
 *   Pragmatists hold d near the beneficiary end (0.15-0.25): they gain institutional standing and publication access without bearing the cost of defending pure objectivity or pure normativity claims. Institutional designers hold d similarly beneficiary-oriented (0.20-0.30): they can justify policy choices while appearing systematic. Diagnosticists hold d near the target end (0.75-0.85): they are routed away from peripheral classification work and their core claim (objectivity about referents) is administratively suppressed. Critical scholars hold d moderately target-oriented (0.60-0.70): they are excluded from mainstream policy discourse and their position on the normativity question is treated as an alternative reading rather than the primary framework. The governance authority holds d at 0.50 (symmetric): it has interest in maintaining the typology's institutional status but bears costs when the reading's fragility becomes visible.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to honor both the observational core and the normative periphery) remains live: institutional designers continue to face the tension between needing systematic classification and acknowledging irreducible value disagreement. The hybrid reading does not solve this problem—it manages it through administrative suppression. The constraint avoids misclassification as a pure rope (which would falsely claim all four stakeholder seats benefit equally) by explicitly naming the transfer from diagnosticists and critical scholars to pragmatists. It avoids misclassification as a pure snare by retaining the language of observational rigor for the core, which creates genuine coordination value alongside the extraction. The tangled_rope classification holds: there is real coordination value (unified language across domains, observational core that holds), but the coordination is asymmetric—pragmatists coordinate the framework for their benefit while diagnosticists and critical scholars bear the cost of suppressed alternative framings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    observational_core_stability_across_domains,
    'Is the constraint typology''s core (mountains and ropes) genuinely observational—stable classifications across unrelated empirical domains and epistemic communities—or does institutional embedding suppress the discovery that normativity penetrates earlier in the classification chain than the hybrid reading admits?',
    'Cross-domain empirical survey: if mountain and rope classifications hold identically across physics, biology, computer science, and economics with NO institutional incentive structure aligning them, the core is genuinely observational. If systematic divergence appears only when institutions apply the framework to policy questions, suppression may be occurring.',
    'If the core is contaminated by unacknowledged normativity, the entire typology collapses to the rhetorical_scaffold_reading and the hybrid reading is a sophisticated suppression mechanism. If the core holds, the boundary between observational and constructed can be maintained. This determines whether the hybrid reading is pragmatic-temporary or pragmatic-permanent, and whether the measurement of extractiveness (0.62) understates or correctly captures the reading''s asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(observational_core_stability_across_domains, empirical, 'Whether the claimed observational core is genuinely stable or suppressed by institutional embedding.').

omega_variable(
    normativity_location_in_classification_chain,
    'At what point in the constraint classification process does normativity enter? The hybrid reading claims it enters in boundary judgment (snare vs. tangled_rope decisions). But does normativity actually enter earlier—in the choice of which observables to measure, in the definition of ''extraction,'' in the demarcation of who counts as a stakeholder?',
    'Trace a canonical classification dispute (e.g., platform commission rates: rope or snare?) through the measurement chain. At each step, ask: were the choices about what to measure, how to measure, and what counts as evidence made by invoking normativity? If normativity choices appear at the observable selection or stakeholder identification stage, the hybrid reading''s location-of-normativity claim is wrong.',
    'If normativity enters earlier than the hybrid reading claims, the ''observational core'' label is misleading—it conceals normative work that appears to be observation. This radically shifts the reading''s epistemic standing and its extractiveness rating (should be higher if core normativity is hidden). If normativity enters only at boundary judgment, the hybrid reading''s structure is defensible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(normativity_location_in_classification_chain, conceptual, 'Whether normativity penetrates the classification chain earlier than the hybrid reading locates it.').

omega_variable(
    pragmatism_as_neutral_vs_hidden_normative_choice,
    'Does the hybrid reading''s presentation of itself as pragmatically neutral—accommodating both observation and normativity—conceal the fact that ''pragmatism'' is itself a normative choice? That preferring pragmatic compromise over pure-diagnosticism or pure-criticism is a normative stance, not an epistemic inevitability?',
    'Compare publication friction: measure the time and resources required to publish a constraint story under each reading. If pragmatist stories face lower friction than diagnosticist or critical stories with identical empirical content, pragmatism is being institutionally privileged as if it were epistemically neutral. The differential friction IS the hidden normativity.',
    'If pragmatism is genuinely neutral, the hybrid reading is what it claims. If pragmatism masks normativity, the hybrid reading embodies a normative choice (favor coordination over precision, or over critical clarity) while claiming epistemic neutrality. This would mean the entire reading is better classified as rhetorical_scaffold with false objectivity claims—making it a snare, not a tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(pragmatism_as_neutral_vs_hidden_normative_choice, conceptual, 'Whether the pragmatic frame is a genuine epistemological position or a rhetorical move masking normativity.').

omega_variable(
    beneficiary_legitimacy_frame_contestation,
    'The hybrid reading claims the snare/tangled_rope boundary depends on normative judgments about legitimate beneficiaries. But what makes a beneficiary ''legitimate'' in the reading''s own terms? Is legitimacy defined by procedures (any beneficiary who was part of a deliberative process), by outcomes (beneficiaries who don''t extract inefficient rents), by foundational rights (beneficiaries whose benefit violates no antecedent rights)? And do all three definitions converge on the same boundaries, or do they diverge?',
    'Take a single policy arrangement (e.g., a platform commission) and classify it under procedural, outcome-based, and rights-based definitions of beneficiary legitimacy. If all three converge on the same snare/tangled_rope boundary, normativity-of-beneficiaries is stable. If they diverge, the hybrid reading has hidden multiple competing framings under a single ''normativity'' label—and the reading''s pragmatic appeal depends on not articulating which definition it actually uses.',
    'If legitimacy framings diverge, the hybrid reading''s peripheral classification is unstable—it appears to accommodate normativity while actually privileging one normative frame (likely the one most convenient for institutional designers, the reading''s primary beneficiaries). This raises the extractiveness rating and supports the snare diagnosis from the critical-scholar seat.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(beneficiary_legitimacy_frame_contestation, conceptual, 'Whether normativity-of-beneficiaries has a stable meaning or the reading conceals competing legitimacy definitions.').

omega_variable(
    extraction_measurement_reading_dependence,
    'The hybrid reading measures extractiveness at 0.62 for itself, while the immutable_diagnostic reading would measure the same kernel-constraint at ~0.15 (it''s pure observational instrument, no extraction), and the rhetorical_scaffold_reading would measure it at ~0.85 (it''s pure normative rhetoric). These three readings generate three different epsilon values for the same kernel. Which epsilon is the ''true'' value, and what does it mean that the reading chosen determines the measurement?',
    'This is not a data problem; it is a structural feature of kernel readings (OQ-26: epsilon is reading-indexed, the ε-referent is fixed, but epsilon values vary across readings). The mechanism is: commit to what epsilon refers to (the standing arrangement under contest, assessed by the reading''s own lights) and accept that different readings produce different epsilon values for the same kernel because they assess differently what the standing arrangement is.',
    'If readings are treated as commensurable (all measuring the same constraint differently), the corpus can use measurement divergence as diagnostic (high ε-variance across readings flags a contested kernel). If readings are incommensurable, each reading''s epsilon is locally valid but the corpus cannot aggregate across them. The hybrid reading must choose: either defend its epsilon as the correct reading of the same kernel, or acknowledge incommensurability and stop claiming to measure ''the'' constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_measurement_reading_dependence, conceptual, 'Whether reading-indexed epsilon values are commensurable measurements or evidence of incommensurable framings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deferential_realism_ontology__hybrid_pragmatic_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(defe_tr_t0, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(defe_tr_t0, observed).
narrative_ontology:measurement(defe_tr_t4, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 4, 0.27).
narrative_ontology:measurement_basis(defe_tr_t4, observed).
narrative_ontology:measurement(defe_tr_t8, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 8, 0.33).
narrative_ontology:measurement_basis(defe_tr_t8, observed).
narrative_ontology:measurement(defe_tr_t12, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 12, 0.38).
narrative_ontology:measurement_basis(defe_tr_t12, observed).
narrative_ontology:measurement(defe_tr_t16, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 16, 0.4).
narrative_ontology:measurement_basis(defe_tr_t16, projected).
narrative_ontology:measurement(defe_tr_t20, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(defe_tr_t20, projected).

% Extraction over time
narrative_ontology:measurement(defe_be_t0, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(defe_be_t0, observed).
narrative_ontology:measurement(defe_be_t4, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 4, 0.52).
narrative_ontology:measurement_basis(defe_be_t4, observed).
narrative_ontology:measurement(defe_be_t8, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 8, 0.56).
narrative_ontology:measurement_basis(defe_be_t8, observed).
narrative_ontology:measurement(defe_be_t12, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 12, 0.59).
narrative_ontology:measurement_basis(defe_be_t12, observed).
narrative_ontology:measurement(defe_be_t16, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 16, 0.61).
narrative_ontology:measurement_basis(defe_be_t16, projected).
narrative_ontology:measurement(defe_be_t20, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement_basis(defe_be_t20, projected).

% Suppression requirement over time
narrative_ontology:measurement(defe_su_t0, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(defe_su_t0, observed).
narrative_ontology:measurement(defe_su_t4, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 4, 0.48).
narrative_ontology:measurement_basis(defe_su_t4, observed).
narrative_ontology:measurement(defe_su_t8, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 8, 0.52).
narrative_ontology:measurement_basis(defe_su_t8, observed).
narrative_ontology:measurement(defe_su_t12, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 12, 0.56).
narrative_ontology:measurement_basis(defe_su_t12, observed).
narrative_ontology:measurement(defe_su_t16, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 16, 0.57).
narrative_ontology:measurement_basis(defe_su_t16, projected).
narrative_ontology:measurement(defe_su_t20, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement_basis(defe_su_t20, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(deferential_realism_ontology__hybrid_pragmatic_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(deferential_realism_ontology__hybrid_pragmatic_reading, 0.18).
narrative_ontology:affects_constraint(deferential_realism_ontology__hybrid_pragmatic_reading, deferential_realism_ontology__immutable_diagnostic_reading).
narrative_ontology:affects_constraint(deferential_realism_ontology__hybrid_pragmatic_reading, deferential_realism_ontology__rhetorical_scaffold_reading).

% DUAL FORMULATION NOTE:
% The deferential_realism_ontology kernel supports three structurally distinct readings. Each reading produces a different constraint story with different epsilon values, beneficiary structures, and classifications. The hybrid_pragmatic_reading (this story) claims the typology has observational core + normative periphery, positioning itself as a compromise between pure diagnosticism (immutable_diagnostic_reading: all observational, ε~0.15) and pure normativity (rhetorical_scaffold_reading: all constructed, ε~0.85). The three readings coexist in the scholarly community; the hybrid reading's institutional dominance suppresses alternatives. All three readings reference the same kernel (the constraint typology itself), but each reading instantiates a different constraint by authoring a different ε, beneficiary set, and classification. This is the designed use case for kernel readings: a single contested kernel produces multiple constraint stories, one per reading. Epsilon variance across readings is NOT a measurement error—it is diagnostic evidence of kernel contestation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(deferential_realism_ontology__hybrid_pragmatic_reading, institutional, 0.25).
constraint_indexing:directionality_override(deferential_realism_ontology__hybrid_pragmatic_reading, powerful, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
