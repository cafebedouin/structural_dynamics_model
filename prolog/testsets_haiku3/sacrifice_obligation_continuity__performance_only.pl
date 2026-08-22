% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_continuity__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_continuity__performance_only, []).

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
 *   constraint_id: sacrifice_obligation_continuity__performance_only
 *   human_readable: Sacrifice Obligation Continuity (Performance-Only Reading)
 *   domain: religious_law/ritual_studies/textual_tradition
 *
 * SUMMARY:
 *   This constraint instantiates the 'performance-only' reading of a
 *   contested kernel: the binding status of Jewish sacrifice law after the
 *   destruction of the Second Temple (70 CE) made performance impossible.
 *   Under the performance-only reading, the obligation persists as binding
 *   forever—study of sacrifice law and performance of substitute rituals
 *   (prayer, fasting, seasonal observances) are PREPARATION for future
 *   restoration (literal rebuilding or messianic redemption), NOT
 *   fulfillment. The reading generates high extractiveness and guilt without
 *   remedy: current and future generations inherit an unfulfillable
 *   obligation they cannot refuse (identity-locked), cannot satisfy
 *   (structurally impossible), and are told cannot be satisfied through study
 *   (reading-specific claim). Three sibling readings offer alternatives:
 *   study-as-performance (study itself is fulfillment), messianic-suspension
 *   (obligation is held in abeyance), and archival-preservation (obligation
 *   is no longer binding). The performance-only reading forecloses the first,
 *   coexists with the second and third, and bears the structural weight of
 *   maintaining the obligation's binding force across centuries without
 *   providing a path to satisfaction.
 *
 * KEY AGENTS:
 *   - textual_authority_rabbinate: Institutional agenda-setter. Maintains and enforces the performance-only interpretation. Collects authority from the claim that the obligation persists binding. Benefit: institutional power, interpretive control, coherence of the legal corpus.
 *   - current_generation_jews: Organized payer. Inherit unfulfillable obligation. Identity-locked (cannot exit through renunciation). Trapped between obligation (binding) and performance (impossible). Guilt is structural.
 *   - lineage_inheritors: Powerless payer. Born into covenant obligation they did not choose and cannot satisfy. Carry intergenerational debt without intergenerational remedy. Trapped across time.
 *   - ritual_practitioners: Moderate payer/beneficiary. Perform substitute practices (prayer, fasting) but under the teaching that these do NOT fulfill the obligation. Derive community and meaning but operate under acknowledged insufficiency.
 *   - study_as_performance_advocates: Excluded powerful alternative reading. Would argue study itself is fulfillment. Structurally blocked by the performance-only claim.
 *   - theological_observer_seat: Analytical position. Measures structural consequences of the reading: guilt distribution, suppression mechanisms, how the claim that 'study is preparation' works.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_continuity__performance_only, 0.82).
domain_priors:suppression_score(sacrifice_obligation_continuity__performance_only, 0.78).
domain_priors:theater_ratio(sacrifice_obligation_continuity__performance_only, 0.67).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, extractiveness, 0.82).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, theater_ratio, 0.67).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_continuity__performance_only, snare).
narrative_ontology:human_readable(sacrifice_obligation_continuity__performance_only, "Sacrifice Obligation Continuity (Performance-Only Reading)").
narrative_ontology:topic_domain(sacrifice_obligation_continuity__performance_only, "religious_law/ritual_studies/textual_tradition").

domain_priors:requires_active_enforcement(sacrifice_obligation_continuity__performance_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_continuity__performance_only, '61dc279f-fd3a-4e47-b175-86bc00ad6663').
narrative_ontology:cs_kernel_codification('61dc279f-fd3a-4e47-b175-86bc00ad6663', fixed_text).
narrative_ontology:cs_authority_grounding('61dc279f-fd3a-4e47-b175-86bc00ad6663', extraction).
narrative_ontology:cs_interpretation_layer_present('61dc279f-fd3a-4e47-b175-86bc00ad6663').
narrative_ontology:cs_reading_relation('61dc279f-fd3a-4e47-b175-86bc00ad6663', sacrifice_obligation_continuity__study_as_performance, forecloses).
narrative_ontology:cs_reading_relation('61dc279f-fd3a-4e47-b175-86bc00ad6663', sacrifice_obligation_continuity__messianic_suspension, coexists_with).
narrative_ontology:cs_reading_relation('61dc279f-fd3a-4e47-b175-86bc00ad6663', sacrifice_obligation_continuity__archival_preservation, coexists_with).
narrative_ontology:cs_axiom('61dc279f-fd3a-4e47-b175-86bc00ad6663', foundational, obligation_persists_unfulfilled).
narrative_ontology:cs_axiom_status(obligation_persists_unfulfilled, holdable).
narrative_ontology:cs_axiom_grounding('61dc279f-fd3a-4e47-b175-86bc00ad6663', obligation_persists_unfulfilled, deontological).
narrative_ontology:cs_axiom('61dc279f-fd3a-4e47-b175-86bc00ad6663', foundational, study_inadequate_substitute).
narrative_ontology:cs_axiom_status(study_inadequate_substitute, holdable).
narrative_ontology:cs_axiom_grounding('61dc279f-fd3a-4e47-b175-86bc00ad6663', study_inadequate_substitute, deontological).
narrative_ontology:cs_reference_frame('61dc279f-fd3a-4e47-b175-86bc00ad6663', torah_binding_perpetuity_obligation).
narrative_ontology:cs_drift_state('61dc279f-fd3a-4e47-b175-86bc00ad6663', contemporary_post_modernity_skepticism, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('61dc279f-fd3a-4e47-b175-86bc00ad6663', '2026-06-19T14:32:00Z').
narrative_ontology:cs_kernel_id(sacrifice_obligation_continuity__performance_only, sacrifice_obligation_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_victim(sacrifice_obligation_continuity__performance_only, current_generation_jews).
narrative_ontology:constraint_victim(sacrifice_obligation_continuity__performance_only, lineage_inheritors).
narrative_ontology:constraint_victim(sacrifice_obligation_continuity__performance_only, ritual_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__performance_only, ritual_practitioners).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__performance_only, temple_worship_irreplaceability).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__performance_only, performance_obligation_binding).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains and interprets the binding status of sacrifice law. Under this reading, declares that sacrifice is non-performable in the current era but remains a binding obligation, not suspended or fulfilled through study. Administers the ritual-preparation regime (holidays, theological study, seasonal observances) as placeholder activity. Collects authority and interpretive legitimacy from the assertion that the obligation persists unfulfilled.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, textual_authority_rabbinate, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Inherit an unfulfillable obligation: sacrifice law remains binding but performance is impossible (no temple, no priesthood, no altar). They cannot exit the obligation through renunciation (identity-bound), cannot satisfy it through performance (structurally unavailable), and are told that study is preparation, not fulfillment. They bear the weight of an obligation they cannot meet and the teaching that attempting satisfaction through textual engagement is inadequate. Guilt is structural and without remedy.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, current_generation_jews, payer,
    organized, biographical, identity_locked, global).

% Born into a covenant whose core performative requirement is presented as eternally binding but practically unfulfillable. Cannot inherit the ritual knowledge needed to execute sacrifice (the priesthood chain is broken). Carry intergenerational obligation debt without intergenerational remedy. The constraint operates across generations as a transmitted wound.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, lineage_inheritors, payer,
    powerless, generational, trapped, global).

% Perform substitute rituals (prayer, fasting, giving) with the understanding that these are NOT fulfillment of the sacrifice obligation but only preparation for a future restoration. Derive meaning and community from the practice, but operate under the teaching that their practice does not and cannot satisfy the original obligation. They are both practicing a tradition and being told their practice is inadequate.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, ritual_practitioners, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_continuity__performance_only, ritual_practitioners, beneficiary).

% Argue that textual study and engagement with sacrifice law itself constitutes fulfillment of the commandment—that the obligation persists through interpretation and learning, not through future restoration. They would reframe the obligation as satisfied in the present through intellectual and spiritual work. This reading is structurally excluded by the performance-only frame, which declares study to be inadequate preparation, not fulfillment.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, study_as_performance_advocates, excluded,
    powerful, biographical, mobile, global).

% Hold that sacrifice obligation is suspended (not violated, not fulfilled) pending messianic restoration. This reading offers a middle path: the obligation is not abandoned but temporarily held in abeyance. They would argue that current generation is neither obligated nor guilty, but in a waiting state. This reading is excluded from the performance-only frame.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, messianic_theology_adherents, excluded,
    powerful, civilizational, mobile, global).

% Argue that sacrifice law is no longer binding; its study preserves cultural memory and textual tradition without normative force. They would dissolve the obligation entirely and reframe the practice as historical recovery, not present duty. This reading is excluded from the performance-only frame, which insists the obligation persists as binding even if unfulfillable.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, archival_preservationists, excluded,
    moderate, civilizational, mobile, global).

% Analyzes the structural consequences of the performance-only reading: how it distributes guilt, obligation, and interpretive authority across the community and across generations; what work the claim that 'study is preparation' does in sustaining the constraint; and how the competing readings reshape the obligation's existential weight.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, theological_observer_seat, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: No genuine coordination function. The constraint does not solve a collective-action problem or enable mutual benefit. Instead, it maintains a coherence claim: that the Jewish people remain bound by a commandment whose original form (temple sacrifice) is no longer performable, and that the obligation persists as binding even in the absence of performance.
% TRANSFER_FUNCTION: Moves the burden of an unfulfillable obligation from the rabbinical authority (which administers the interpretation) to the living community (which inherits the guilt). The authority collects the legitimacy of upholding law; the community carries the weight of an obligation without remedy. Textual study becomes a substitute activity—a way of performing obedience to the obligation while explicitly not fulfilling it.
% ABSENT_VOICES: Study-as-performance advocates and messianic theology adherents are structurally excluded by the performance-only frame's core premise. Archival preservationists, who would dissolve the obligation entirely, are also excluded. These parties would argue that the obligation's binding status should be reconsidered given its unfulfillability, but the performance-only reading closes off that argument by declaring the obligation binding regardless of performance possibility.
% DISAPPEARANCE_RATIONALE: If the performance-only reading disappeared—if the community accepted either study-as-performance, messianic suspension, or archival preservation—the guilt structure would collapse and be replaced by either satisfaction, waiting, or dissolution. The world would reorganize around a different reading of the obligation's binding status and current force. Some would say the obligation genuinely disappears; others would say only the reading of it disappears while the underlying obligation persists under a different interpretation.
% FOUNDING_PROBLEM: After the destruction of the Second Temple (70 CE), sacrifice became impossible but was understood as commanded by Torah in perpetuity. The founding problem is: what is the binding status of a law whose performance is now impossible? The performance-only reading answers: the obligation persists as binding; study is preparation for future restoration (either literal rebuilding or messianic redemption).
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by historical texts (Talmud, Maimonides' Mishneh Torah, medieval responsa) and by competing rabbinical readings across 20 centuries. The performance-only reading is explicitly endorsed by major strands of Orthodox Jewish law and theology. The corroboration is internal to the tradition itself; secular academic observers (historians of religion, anthropologists) attest to the problem's persistence and the reading's prevalence, but they do not endorse the obligation as binding—they describe it as a theological position. This is a constraint whose founding problem is both live and contested within the community that holds it.
narrative_ontology:disappearance_verdict(sacrifice_obligation_continuity__performance_only, contested).
narrative_ontology:founding_problem_status(sacrifice_obligation_continuity__performance_only, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_continuity__performance_only, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sacrifice_obligation_continuity__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_continuity__performance_only, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_continuity__performance_only_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sacrifice_obligation_continuity__performance_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sacrifice_obligation_continuity__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because the obligation is unfulfillable yet binding—no beneficiary exists to justify the extraction; it purely burdens the payer. Suppression is high (0.78) because the reading must actively prevent alternative interpretations (study-as-performance, messianic suspension) from being adopted; the suppression is the institutional insistence that the obligation persists despite unfulfillability. Theater ratio is high (0.67) because substitute practices (prayer, study, fasting, seasonal rituals) are explicitly not fulfillment but preparation—they are performed as if they matter while being taught they do not satisfy the obligation. This creates a cycle of performance + acknowledged insufficiency, characteristic of theater. Accessibility_collapse is moderate-high (0.72): individuals cannot renounce the obligation (identity-locked), but they can adopt alternative readings (study-as-performance, messianic suspension) if they accept the cost of breaking with institutional authority. Resistance is moderate (0.58): the community has upheld the reading for 20 centuries, suggesting either deep buy-in or effective suppression; but competing readings persist, and contemporary Jews increasingly adopt study-as-performance or dissolution, indicating real resistance. The measurement series span 2000 years: extractiveness and theater ratio rise slowly but plateau by t=1500, suggesting the configuration stabilized after the medieval period and has remained stable into modernity. Suppression requirement rises more steeply and stays highest, indicating institutional cost to maintaining the reading against alternatives has been real and increasing.
 *
 * PERSPECTIVAL GAP:
 *   Agenda-setter (rabbinate) and payer (current generation) should compute radically differently. From the agenda-setter's seat, the reading is a coherent and faithful interpretation: the obligation persists because the tradition says so; study is preparation because restoration remains (theologically) possible; the reading preserves the law's binding force without claiming false performance. From the payer's seat, the same structure is extractive: an obligation is imposed that cannot be met; alternatives that would dissolve or reframe the obligation are suppressed; study is taught as insufficient while being the only available practice; guilt persists without remedy. The engine computes these divergences from the structural data: the rabbinate is agenda_setter (power: institutional, exit: arbitrage—can change the interpretation), while payers are identity-locked (power: organized/powerless, exit: identity_locked—cannot leave the obligation). The d-values diverge sharply: the rabbinate benefits from the obligation's persistence (d near 0.2, beneficiary end); the payers are targets (d near 0.85, target end). A beneficiary stakeholder (the rabbinate) collects interpretive authority; a victim stakeholder (payers) carries obligation debt. This divergence is not an error in the authored metrics—it is the structural fact the reading instantiates.
 *
 * DIRECTIONALITY LOGIC:
 *   The rabbinical authority is the beneficiary: it maintains control over the interpretation of Jewish law by preserving the obligation's binding force and preventing alternative readings (study-as-performance) from being legitimized. The obligation's persistence justifies its institutional role as interpreter and guardian of the tradition. d-value: near 0.15 (beneficiary end). Current generation and lineage inheritors are the victims: they inherit an unfulfillable obligation they cannot refuse (identity-locked), cannot satisfy (performance is impossible), and are taught cannot be satisfied through their actual practice (study, ritual substitutes). They bear pure extraction: obligation debt without remedy. d-value: near 0.88 (target end). Ritual practitioners are dual-positioned: they derive genuine community and spiritual meaning from practice, so they have some beneficiary characteristics; but they practice under the teaching that their practice is insufficient, creating a built-in inadequacy. d-value: near 0.58 (symmetric, leaning toward target). The authority has arbitrage-level exit (can change the interpretation) and institutional power to maintain the reading. The payers have identity-locked exit (cannot renounce without renouncing self-identity) and low power to force alternatives. This structural asymmetry is the foundation of the high effective extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits classic mandatrophy: the founding problem (what commandment should be binding after Temple destruction?) has been formally answered by the tradition (obligation persists), but the answer has outlived its practical coherence. Current and future generations cannot fulfill the obligation; they are told study is preparation (not fulfillment) and that alternatives (study-as-performance, messianic suspension) are inadequate. The mandate (preserve the binding force of Jewish law) persists even though the machinery for executing that mandate (performance of sacrifice) is gone and explicitly cannot be reconstructed without external intervention (Temple rebuilding, messianic restoration—both outside the community's control). Mandatrophy is visible in the gap between the obligation's stated binding force and the community's actual practice: they perform substitutes (prayer, study, fasting) while being taught these do NOT satisfy the obligation. This is performative maintenance: the practices are maintained to avoid admitting that the obligation cannot be met. The theater ratio (0.67) captures this: ~67% of the energy in the system goes to performing compliance with an obligation that the reading itself declares cannot be satisfied in the present era. The mandate would be resolved (mandatrophy dissolved) if the community adopted an alternative reading: study-as-performance (obligation satisfied through engagement with the text), messianic suspension (obligation held in abeyance, not violated), or archival preservation (obligation no longer binding). The performance-only reading prevents this resolution by insisting the obligation persists binding despite unfulfillability. This is institutional preference for an unresolved mandate over coherent alternatives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    obligation_persistence_mechanism,
    'What grounds the claim that the obligation persists as binding after performance becomes impossible? Is it rooted in the literal command (divine imperative language), the covenant relationship itself (Jews remain bound regardless of conditions), or the interpretive authority''s will?',
    'Genealogical analysis of rabbinic texts: trace which justifications are offered for the binding status through successive generations. Interviews with contemporary believers: ask what makes the obligation binding for them now, absent performance possibility. The grounding mechanism determines whether the reading is a logical consequence of the textual tradition or a choice the authority structure makes.',
    'If grounded in literal command language, the obligation is presented as natural/structural (mountain-adjacent, though practically enforced). If grounded in covenant relationship or authority will, the obligation is explicitly constructed (constraint, engineered) and the high extractiveness is less defensible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(obligation_persistence_mechanism, conceptual, 'What kind of claim grounds the obligation''s persistence after performance becomes impossible.').

omega_variable(
    study_as_placeholder_function,
    'Does the assertion that ''study is preparation for future restoration'' serve a genuine theological function (keeping the obligation intelligible and actionable across generations without performance) or does it primarily suppress competing readings (study-as-performance, messianic suspension, archival preservation)?',
    'Analysis of how ''study as preparation'' language functions in different historical periods and communities. Does it enable genuine practice and community continuity, or does it primarily serve to deny the adequacy of lived alternatives? Comparison with what ''preparation'' actually involves: is it a coherent mode of practice, or a placeholder that fails upon examination?',
    'If study genuinely functions as preparation, the theater ratio is lower (0.45-0.55) and extractiveness is partly justified as the cost of maintaining continuity. If it primarily suppresses alternatives, the theater ratio is correctly high (0.65-0.75) and extractiveness is mostly pure extraction of guilt and obligation debt.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(study_as_placeholder_function, empirical, 'Whether ''study as preparation'' language serves practice continuity or alternative suppression.').

omega_variable(
    identity_lock_mechanism,
    'For current_generation_jews, is exit from the obligation prevented primarily by structural legal/institutional barriers (exit_options = constrained) or by identity fusion—a sense of self so deeply constituted through covenant relationship that renouncing the obligation feels like renouncing one''s own existence (exit_options = identity_locked)?',
    'Post-exit ethnography: study individuals who have left the community—do they retain the sense of obligation guilt (internalized, identity-fused) or do they experience relief (structural suppression removed)? Comparative analysis of those born into vs. those who convert in: does conversion entail identity fusion, or can obligation be taken up as a choice?',
    'If mostly structural, the constraint is a snare (high suppression, easily escaped if alternatives are offered). If mostly identity-fused, the constraint approaches tangled_rope for those who stay—they are coordinating around a shared identity even as they are being extracted from. Degree of internalization determines post-exit trajectory and affects reclassification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether exit-prevention is structural or internalized through identity fusion.').

omega_variable(
    kernel_reading_ambiguity_performance_only,
    'This constraint instantiates the ''performance_only'' reading of the sacrifice obligation kernel. That reading''s core claim is that the obligation persists as binding and that study is preparation, not fulfillment. But is this reading one LOGICAL CONSEQUENCE of the textual tradition, or is it one CHOICE among equally defensible alternatives? That is: does the tradition foreclose the study-as-performance reading, or merely dis-prefer it?',
    'Textual analysis: can study-as-performance be grounded in the same sources (Torah, Talmud, medieval commentaries) using equivalent hermeneutical moves? If yes, then the performance-only reading is a choice, not a logical necessity, and the choice to maintain it despite alternatives is more explicitly extractive. If no, then the performance-only reading has stronger claim to be the tradition''s natural reading.',
    'If study-as-performance is textually defensible, the obligation''s persistence is being chosen, not logically required, and the high extractiveness reflects institutional power protecting one reading against alternatives. If it is not defensible, the extractiveness is more justifiable as the cost of coherent fidelity to the tradition. Determines whether the reading is a natural consequence or an enforced interpretation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity_performance_only, conceptual, 'Whether the performance-only reading is a logical consequence of the tradition or an institutional choice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_continuity__performance_only, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_continuity__performance_only, theater_ratio, 0, 0.55).
narrative_ontology:measurement_basis(sacr_tr_t0, projected).
narrative_ontology:measurement(sacr_tr_t250, sacrifice_obligation_continuity__performance_only, theater_ratio, 250, 0.6).
narrative_ontology:measurement_basis(sacr_tr_t250, observed).
narrative_ontology:measurement(sacr_tr_t500, sacrifice_obligation_continuity__performance_only, theater_ratio, 500, 0.63).
narrative_ontology:measurement_basis(sacr_tr_t500, observed).
narrative_ontology:measurement(sacr_tr_t1000, sacrifice_obligation_continuity__performance_only, theater_ratio, 1000, 0.65).
narrative_ontology:measurement_basis(sacr_tr_t1000, observed).
narrative_ontology:measurement(sacr_tr_t1500, sacrifice_obligation_continuity__performance_only, theater_ratio, 1500, 0.67).
narrative_ontology:measurement_basis(sacr_tr_t1500, observed).
narrative_ontology:measurement(sacr_tr_t2000, sacrifice_obligation_continuity__performance_only, theater_ratio, 2000, 0.67).
narrative_ontology:measurement_basis(sacr_tr_t2000, observed).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_continuity__performance_only, base_extractiveness, 0, 0.75).
narrative_ontology:measurement_basis(sacr_be_t0, projected).
narrative_ontology:measurement(sacr_be_t250, sacrifice_obligation_continuity__performance_only, base_extractiveness, 250, 0.78).
narrative_ontology:measurement_basis(sacr_be_t250, observed).
narrative_ontology:measurement(sacr_be_t500, sacrifice_obligation_continuity__performance_only, base_extractiveness, 500, 0.79).
narrative_ontology:measurement_basis(sacr_be_t500, observed).
narrative_ontology:measurement(sacr_be_t1000, sacrifice_obligation_continuity__performance_only, base_extractiveness, 1000, 0.81).
narrative_ontology:measurement_basis(sacr_be_t1000, observed).
narrative_ontology:measurement(sacr_be_t1500, sacrifice_obligation_continuity__performance_only, base_extractiveness, 1500, 0.82).
narrative_ontology:measurement_basis(sacr_be_t1500, observed).
narrative_ontology:measurement(sacr_be_t2000, sacrifice_obligation_continuity__performance_only, base_extractiveness, 2000, 0.82).
narrative_ontology:measurement_basis(sacr_be_t2000, observed).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_obligation_continuity__performance_only, suppression_requirement, 0, 0.7).
narrative_ontology:measurement_basis(sacr_su_t0, projected).
narrative_ontology:measurement(sacr_su_t250, sacrifice_obligation_continuity__performance_only, suppression_requirement, 250, 0.72).
narrative_ontology:measurement_basis(sacr_su_t250, observed).
narrative_ontology:measurement(sacr_su_t500, sacrifice_obligation_continuity__performance_only, suppression_requirement, 500, 0.74).
narrative_ontology:measurement_basis(sacr_su_t500, observed).
narrative_ontology:measurement(sacr_su_t1000, sacrifice_obligation_continuity__performance_only, suppression_requirement, 1000, 0.77).
narrative_ontology:measurement_basis(sacr_su_t1000, observed).
narrative_ontology:measurement(sacr_su_t1500, sacrifice_obligation_continuity__performance_only, suppression_requirement, 1500, 0.78).
narrative_ontology:measurement_basis(sacr_su_t1500, observed).
narrative_ontology:measurement(sacr_su_t2000, sacrifice_obligation_continuity__performance_only, suppression_requirement, 2000, 0.78).
narrative_ontology:measurement_basis(sacr_su_t2000, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=2000
narrative_ontology:measurement(sacr_grid_01, sacrifice_obligation_continuity__performance_only, accessibility_collapse(class), 0, 0.7).
narrative_ontology:measurement(sacr_grid_02, sacrifice_obligation_continuity__performance_only, accessibility_collapse(class), 2000, 0.73).
narrative_ontology:measurement(sacr_grid_03, sacrifice_obligation_continuity__performance_only, accessibility_collapse(individual), 0, 0.68).
narrative_ontology:measurement(sacr_grid_04, sacrifice_obligation_continuity__performance_only, accessibility_collapse(individual), 2000, 0.72).
narrative_ontology:measurement(sacr_grid_05, sacrifice_obligation_continuity__performance_only, accessibility_collapse(organizational), 0, 0.75).
narrative_ontology:measurement(sacr_grid_06, sacrifice_obligation_continuity__performance_only, accessibility_collapse(organizational), 2000, 0.78).
narrative_ontology:measurement(sacr_grid_07, sacrifice_obligation_continuity__performance_only, accessibility_collapse(structural), 0, 0.8).
narrative_ontology:measurement(sacr_grid_08, sacrifice_obligation_continuity__performance_only, accessibility_collapse(structural), 2000, 0.82).
narrative_ontology:measurement(sacr_grid_09, sacrifice_obligation_continuity__performance_only, resistance(class), 0, 0.58).
narrative_ontology:measurement(sacr_grid_10, sacrifice_obligation_continuity__performance_only, resistance(class), 2000, 0.59).
narrative_ontology:measurement(sacr_grid_11, sacrifice_obligation_continuity__performance_only, resistance(individual), 0, 0.55).
narrative_ontology:measurement(sacr_grid_12, sacrifice_obligation_continuity__performance_only, resistance(individual), 2000, 0.6).
narrative_ontology:measurement(sacr_grid_13, sacrifice_obligation_continuity__performance_only, resistance(organizational), 0, 0.62).
narrative_ontology:measurement(sacr_grid_14, sacrifice_obligation_continuity__performance_only, resistance(organizational), 2000, 0.65).
narrative_ontology:measurement(sacr_grid_15, sacrifice_obligation_continuity__performance_only, resistance(structural), 0, 0.48).
narrative_ontology:measurement(sacr_grid_16, sacrifice_obligation_continuity__performance_only, resistance(structural), 2000, 0.5).
narrative_ontology:measurement(sacr_grid_17, sacrifice_obligation_continuity__performance_only, stakes_inflation(class), 0, 0.7).
narrative_ontology:measurement(sacr_grid_18, sacrifice_obligation_continuity__performance_only, stakes_inflation(class), 2000, 0.74).
narrative_ontology:measurement(sacr_grid_19, sacrifice_obligation_continuity__performance_only, stakes_inflation(individual), 0, 0.72).
narrative_ontology:measurement(sacr_grid_20, sacrifice_obligation_continuity__performance_only, stakes_inflation(individual), 2000, 0.76).
narrative_ontology:measurement(sacr_grid_21, sacrifice_obligation_continuity__performance_only, stakes_inflation(organizational), 0, 0.65).
narrative_ontology:measurement(sacr_grid_22, sacrifice_obligation_continuity__performance_only, stakes_inflation(organizational), 2000, 0.68).
narrative_ontology:measurement(sacr_grid_23, sacrifice_obligation_continuity__performance_only, stakes_inflation(structural), 0, 0.78).
narrative_ontology:measurement(sacr_grid_24, sacrifice_obligation_continuity__performance_only, stakes_inflation(structural), 2000, 0.8).
narrative_ontology:measurement(sacr_grid_25, sacrifice_obligation_continuity__performance_only, suppression(class), 0, 0.7).
narrative_ontology:measurement(sacr_grid_26, sacrifice_obligation_continuity__performance_only, suppression(class), 2000, 0.72).
narrative_ontology:measurement(sacr_grid_27, sacrifice_obligation_continuity__performance_only, suppression(individual), 0, 0.68).
narrative_ontology:measurement(sacr_grid_28, sacrifice_obligation_continuity__performance_only, suppression(individual), 2000, 0.71).
narrative_ontology:measurement(sacr_grid_29, sacrifice_obligation_continuity__performance_only, suppression(organizational), 0, 0.73).
narrative_ontology:measurement(sacr_grid_30, sacrifice_obligation_continuity__performance_only, suppression(organizational), 2000, 0.76).
narrative_ontology:measurement(sacr_grid_31, sacrifice_obligation_continuity__performance_only, suppression(structural), 0, 0.82).
narrative_ontology:measurement(sacr_grid_32, sacrifice_obligation_continuity__performance_only, suppression(structural), 2000, 0.84).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_continuity__performance_only, identity_coordination).
narrative_ontology:boltzmann_floor_override(sacrifice_obligation_continuity__performance_only, 0.25).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__performance_only, sacrifice_obligation_continuity__study_as_performance).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__performance_only, sacrifice_obligation_continuity__messianic_suspension).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__performance_only, sacrifice_obligation_continuity__archival_preservation).

% DUAL FORMULATION NOTE:
% This constraint is part of a four-story constraint family around the sacrifice obligation kernel. Each story instantiates one reading of the binding status and contemporary force of Jewish sacrifice law after Temple destruction (70 CE). The performance_only reading (this constraint) differs from study_as_performance in the claim that study does NOT fulfill the obligation; differs from messianic_suspension in maintaining the obligation as binding (not suspended); differs from archival_preservation in maintaining normative force (not dissolved). The network links show how each reading structures the obligation differently: performance_only makes the obligation unfulfillable and generates guilt; study_as_performance makes the obligation satisfiable through practice; messianic_suspension makes the obligation temporally suspended; archival_preservation makes the obligation historically interesting but normatively inert. These are not competing interpretations of one constraint—they are different constraints instantiated by different readings of the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
