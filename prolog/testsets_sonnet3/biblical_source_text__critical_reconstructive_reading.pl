% ============================================================================
% CONSTRAINT STORY: biblical_source_text__critical_reconstructive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_source_text__critical_reconstructive_reading, []).

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
 *   constraint_id: biblical_source_text__critical_reconstructive_reading
 *   human_readable: Critical-Reconstructive Priority of the Hypothetical Original Text
 *   domain: religious/academic/textual
 *
 * SUMMARY:
 *   This constraint is one reading of the biblical_source_text kernel: the
 *   claim that historical recovery of the hypothetical original text must be
 *   established before either structural (formal-equivalence) or
 *   meaning-based (dynamic-equivalence) claims can be privileged. Since the
 *   19th-century rise of textual criticism (Westcott-Hort and successors),
 *   this reading has organized biblical scholarship's methodological order of
 *   operations: manuscript comparison and stemmatic reconstruction come
 *   first, and everything else waits. From academic scholarship's own
 *   vantage, this is careful epistemic hygiene against building
 *   interpretation on an uncertain textual foundation. From confessional
 *   communities holding a received text as doctrinally and liturgically
 *   load-bearing, the same ordering rule perpetually defers textual
 *   certainty, subordinates their tradition's transmitted text to an academic
 *   reconstruction project with no terminal point, and transfers interpretive
 *   authority to a discipline whose institutional incentives favor perpetual
 *   textual contestation over settlement.
 *
 * KEY AGENTS:
 *   - academic_biblical_scholarship: agenda_setter/beneficiary (institutional/arbitrage) — sets and profits from the methodological ordering
 *   - textual_critics: beneficiary (organized/mobile) — career and prestige tied to reconstruction's priority
 *   - critical_edition_publishers: beneficiary (powerful/arbitrage) — commercial stake in the apparatus's indispensability
 *   - confessional_communities: payer (moderate/constrained) — doctrinal and liturgical stability destabilized
 *   - lay_readers_of_received_text: payer (powerless/trapped) — bear uncertainty with no capacity to adjudicate it
 *   - translation_committees_bound_to_ecclesial_tradition: payer/beneficiary (organized/constrained) — caught between rigor and confessional continuity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_source_text__critical_reconstructive_reading, 0.58).
domain_priors:suppression_score(biblical_source_text__critical_reconstructive_reading, 0.42).
domain_priors:theater_ratio(biblical_source_text__critical_reconstructive_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_source_text__critical_reconstructive_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(biblical_source_text__critical_reconstructive_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(biblical_source_text__critical_reconstructive_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_source_text__critical_reconstructive_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(biblical_source_text__critical_reconstructive_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_source_text__critical_reconstructive_reading, tangled_rope).
narrative_ontology:human_readable(biblical_source_text__critical_reconstructive_reading, "Critical-Reconstructive Priority of the Hypothetical Original Text").
narrative_ontology:topic_domain(biblical_source_text__critical_reconstructive_reading, "religious/academic/textual").

domain_priors:requires_active_enforcement(biblical_source_text__critical_reconstructive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_source_text__critical_reconstructive_reading, '120b3d92-c5f9-4b8b-8bf2-f512ffedd110').
narrative_ontology:cs_kernel_codification('120b3d92-c5f9-4b8b-8bf2-f512ffedd110', distributed).
narrative_ontology:cs_authority_grounding('120b3d92-c5f9-4b8b-8bf2-f512ffedd110', expertise).
narrative_ontology:cs_interpretation_layer_present('120b3d92-c5f9-4b8b-8bf2-f512ffedd110').
narrative_ontology:cs_reading_relation('120b3d92-c5f9-4b8b-8bf2-f512ffedd110', biblical_source_text__formal_equivalence_reading, influences).
narrative_ontology:cs_reading_relation('120b3d92-c5f9-4b8b-8bf2-f512ffedd110', biblical_source_text__dynamic_equivalence_reading, influences).
narrative_ontology:cs_axiom('120b3d92-c5f9-4b8b-8bf2-f512ffedd110', foundational, textual_basis_precedes_structural_and_semantic_claims).
narrative_ontology:cs_axiom_status(textual_basis_precedes_structural_and_semantic_claims, holdable).
narrative_ontology:cs_axiom_grounding('120b3d92-c5f9-4b8b-8bf2-f512ffedd110', textual_basis_precedes_structural_and_semantic_claims, empirically_contingent).
narrative_ontology:cs_axiom('120b3d92-c5f9-4b8b-8bf2-f512ffedd110', secondary, manuscript_divergence_defeats_received_text_authority).
narrative_ontology:cs_axiom_status(manuscript_divergence_defeats_received_text_authority, holdable).
narrative_ontology:cs_axiom_grounding('120b3d92-c5f9-4b8b-8bf2-f512ffedd110', manuscript_divergence_defeats_received_text_authority, empirically_contingent).
narrative_ontology:cs_reference_frame('120b3d92-c5f9-4b8b-8bf2-f512ffedd110', pre_critical_received_text_authority).
narrative_ontology:cs_drift_state('120b3d92-c5f9-4b8b-8bf2-f512ffedd110', post_westcott_hort_critical_consensus_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('120b3d92-c5f9-4b8b-8bf2-f512ffedd110', '').
narrative_ontology:cs_kernel_id(biblical_source_text__critical_reconstructive_reading, biblical_source_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_source_text__critical_reconstructive_reading, academic_biblical_scholarship).
narrative_ontology:constraint_beneficiary(biblical_source_text__critical_reconstructive_reading, textual_critics).
narrative_ontology:constraint_beneficiary(biblical_source_text__critical_reconstructive_reading, critical_edition_publishers).
narrative_ontology:constraint_victim(biblical_source_text__critical_reconstructive_reading, confessional_communities).
narrative_ontology:constraint_victim(biblical_source_text__critical_reconstructive_reading, lay_readers_of_received_text).
narrative_ontology:constraint_victim(biblical_source_text__critical_reconstructive_reading, translation_committees_bound_to_ecclesial_tradition).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(biblical_source_text__critical_reconstructive_reading, translation_committees_bound_to_ecclesial_tradition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the methodological order of operations for biblical studies: establish the earliest recoverable text through manuscript comparison and stemmatics before any claim about literary structure or theological meaning is treated as authoritative. Controls peer review, critical edition production, and doctoral training pipelines that transmit the method. Gains professional standing, publication output, and disciplinary authority from privileging text-critical reconstruction as the necessary first step.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, academic_biblical_scholarship, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(biblical_source_text__critical_reconstructive_reading, academic_biblical_scholarship, beneficiary).

% Their specialized labor of manuscript collation, apparatus construction, and stemma-building is the load-bearing skill this reading elevates above literary and theological interpretation. Career advancement, grant funding, and citation depend on the field continuing to treat reconstructed originals as the necessary foundation for downstream work.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, textual_critics, beneficiary,
    organized, biographical, mobile, global).

% Produce and sell critical editions (e.g. Nestle-Aland style apparatuses) whose commercial and institutional value depends on the premise that the reconstructed original text is the indispensable scholarly starting point that translation committees, seminaries, and scholars must license and cite.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, critical_edition_publishers, beneficiary,
    powerful, generational, arbitrage, global).

% Have historically received a stable text (whether Masoretic, Textus Receptus, or a received critical consensus) as the basis of doctrine, liturgy, and communal identity. This reading treats every received text as provisional pending further manuscript discovery and stemmatic revision, which destabilizes the textual ground under settled doctrinal and liturgical practice. They cannot simply exit the discipline's authority because seminary training, translation committees, and denominational scholarship increasingly defer to the critical apparatus.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, confessional_communities, payer,
    moderate, civilizational, constrained, global).

% Read whatever translation their tradition hands them without access to the text-critical apparatus or the languages needed to evaluate it. When translation footnotes report textual variants and scholarly uncertainty about 'what the original said,' their sense of the text's stability erodes without any corresponding capacity to adjudicate the dispute themselves.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, lay_readers_of_received_text, payer,
    powerless, biographical, trapped, local).

% Must reconcile denominational commitments to a received textual tradition with the discipline's insistence that translation decisions wait on prior text-critical resolution. They benefit from the scholarly rigor the method supplies but pay in prolonged committee disputes, footnote-heavy translations that unsettle congregational confidence, and periodic revision cycles driven by new manuscript findings.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, translation_committees_bound_to_ecclesial_tradition, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(biblical_source_text__critical_reconstructive_reading, translation_committees_bound_to_ecclesial_tradition, beneficiary).

% A sibling reading of the same kernel holding that fidelity to the source language's actual structure is primary and intelligibility is a secondary responsibility delegated to teaching. Not a party to this constraint's operation but structurally sidelined by it: this reading treats structure as unprivileged until textual basis is settled, which subordinates formal-equivalence commitments to prior text-critical resolution.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, formal_equivalence_reading, excluded,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(biblical_source_text__critical_reconstructive_reading, formal_equivalence_reading).

% A sibling reading holding that communicative effectiveness in the target language is primary and structural fidelity is subordinate. Also sidelined: this reading's insistence that meaning cannot be privileged until textual basis is established slows or complicates dynamic-equivalence translation projects that want to move directly to communicative effect.
narrative_ontology:constraint_stakeholder(biblical_source_text__critical_reconstructive_reading, dynamic_equivalence_reading, excluded,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(biblical_source_text__critical_reconstructive_reading, dynamic_equivalence_reading).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(biblical_source_text__critical_reconstructive_reading, academic_biblical_scholarship).
narrative_ontology:fixing_cost_class(biblical_source_text__critical_reconstructive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a discipline-wide sequence of operations: manuscript comparison and stemmatic reconstruction must occur, and reach provisional consensus, before literary-structural or theological-meaning claims are treated as scholarly authoritative. This prevents translation and interpretation from building on textual foundations later manuscript discoveries overturn.
% TRANSFER_FUNCTION: Moves interpretive and translational authority from confessional tradition and lay reception toward the academic text-critical apparatus; moves attention, funding, and institutional prestige toward manuscript science and away from structural and theological scholarship until the textual question is treated as settled.
% ABSENT_VOICES: Confessional communities whose liturgical and doctrinal life depends on textual stability are rarely seated at the table where methodological priority is set; the discipline's peer-review and doctoral-training institutions are controlled by scholars committed to the critical-reconstructive premise. Formal-equivalence and dynamic-equivalence readings are also structurally absent from this reading's own operation, since it does not require their categories to function.
% DISAPPEARANCE_RATIONALE: Academic text-critics would say the discipline collapses into unprincipled eclecticism without the priority rule, since structural and theological claims would be argued without settled textual grounding. Confessional communities would say very little changes for their practice, since they have always operated on a received text and largely ignore or absorb the apparatus through footnotes; some would say the destabilization stops and doctrinal confidence stabilizes. The two sides genuinely disagree about whether the world rearranges.
% FOUNDING_PROBLEM: Multiple divergent manuscript traditions exist for every biblical text with no autograph surviving; early modern and Reformation-era printed editions had frozen particular manuscript families (e.g. the Byzantine-derived Textus Receptus) as if they were unproblematic originals, and 18th-19th century discovery of older and divergent manuscripts (Codex Sinaiticus, Vaticanus, Dead Sea Scrolls) made clear that any received text was one witness among many, not a settled original.
% FOUNDING_PROBLEM_CORROBORATION: Paleographers and manuscript specialists working outside confessional translation committees corroborate that manuscript divergence is real and textually significant, not a scholarly invention. However, whether that divergence entails the ORDERING claim -- that reconstruction must precede and control structural/theological interpretation -- is attested mainly by scholars within the discipline that the ordering claim itself empowers; comparative religion scholars and some hermeneuticians outside biblical text criticism argue the sequencing is a disciplinary convention rather than an epistemic necessity.
narrative_ontology:disappearance_verdict(biblical_source_text__critical_reconstructive_reading, contested).
narrative_ontology:founding_problem_status(biblical_source_text__critical_reconstructive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_source_text__critical_reconstructive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(biblical_source_text__critical_reconstructive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_source_text__critical_reconstructive_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_source_text__critical_reconstructive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(biblical_source_text__critical_reconstructive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(biblical_source_text__critical_reconstructive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate-high (0.58 at interval end) because the reading's ordering claim extracts real interpretive authority and institutional legitimacy from confessional communities and lay readers without their consent or meaningful participation in setting the methodological priority, while returning to them chiefly footnoted uncertainty. It is not maximal because the underlying manuscript-comparison work is genuinely truth-tracking and not fabricated; the extraction is in the SEQUENCING claim (reconstruction must precede structure/meaning), not in the existence of textual criticism itself. Suppression is moderate (0.42): there is no coercive apparatus forcing confessional communities to accept the ordering, but doctoral training, seminary curricula, and translation-committee composition increasingly funnel authority through scholars trained in the critical-reconstructive premise, which functions as soft suppression of alternative orderings. Theater ratio is low-moderate and rising (0.12 to 0.28) reflecting the accumulation of apparatus-maintenance activity (perpetual re-collation, minor variant cataloguing) that increasingly serves disciplinary continuity as much as genuinely open textual questions.
 *
 * DIRECTIONALITY LOGIC:
 *   Academic biblical scholarship, textual critics, and critical edition publishers are declared beneficiaries: the ordering claim is the foundation of their professional and commercial standing, so directionality sits near the beneficiary end (low d). Confessional communities, lay readers, and (partially) translation committees are declared victims: they bear the destabilization of a text they rely on for doctrinal continuity, with limited capacity to contest the ordering from outside the discipline's own institutions, so directionality sits nearer the target end (higher d). Translation committees carry a dual role because they draw real benefit from rigorous method while absorbing the cost of unresolved sequencing in every translation cycle.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem -- genuine, extensive manuscript divergence with no surviving autograph -- remains partly live: new manuscript discoveries and stemmatic refinements continue. But the ORDERING claim (reconstruction must precede structure/meaning) has arguably outlived strict necessity for many practical translation and pastoral purposes, where working critical consensus has existed for generations on the vast majority of textual questions; treating the residual small percentage of genuinely contested readings as license to universally subordinate structure and meaning to text-critical priority is where the mandate risks becoming self-perpetuating rather than problem-tracking. The tangled_rope classification captures this: there is a real coordination function (preventing premature interpretive commitments on textually unstable ground) that requires active enforcement (disciplinary gatekeeping through peer review and training) and rides alongside genuine asymmetric extraction from confessional communities who never consented to the ordering and cannot easily exit its authority.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sequencing_necessity_vs_disciplinary_convention,
    'Is the claim that textual reconstruction must precede structural and theological interpretation an epistemic necessity, or a disciplinary convention that protects text criticism''s institutional priority?',
    'Compare interpretive outcomes in scholarly traditions (e.g. certain strands of literary-canonical criticism) that proceed with structural/theological analysis on a received text without waiting for text-critical resolution, against text-critical-first traditions, controlling for the actual rate of manuscript-driven interpretive overturns.',
    'If sequencing is epistemically necessary, the coordination function dominates and the classification should lean toward rope; if it is largely a disciplinary convention protecting textual critics'' institutional position, the extraction component dominates and the classification should lean toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sequencing_necessity_vs_disciplinary_convention, conceptual, 'Whether the ordering claim is epistemically required or institutionally self-serving.').

omega_variable(
    reading_framing_underdetermination,
    'Could this reading instead be framed as a claim about scholarly method (a Mountain-like epistemic discipline with no beneficiary) rather than as a claim about interpretive authority (a Tangled Rope with a beneficiary set)?',
    'Examine whether text-critical priority is defended primarily on evidentiary grounds (manuscripts genuinely diverge, caution is warranted) versus institutional grounds (curricula, journals, and hiring committees enforce the ordering regardless of the state of manuscript consensus for a given passage).',
    'The methodological framing would support treating this as a low-extraction epistemic norm; the institutional framing (adopted in this story) supports the tangled_rope classification with academic scholarship as a concentrated beneficiary. This story adopts the institutional framing because doctoral training and peer-review gatekeeping are load-bearing for the ordering''s persistence, not merely evidentiary caution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_framing_underdetermination, conceptual, 'Alternative framing of this reading as epistemic norm versus institutional authority claim, and which was adopted and why.').

omega_variable(
    confessional_exit_capacity,
    'Do confessional communities actually lack meaningful exit from the critical-reconstructive apparatus''s influence, or do traditions with strong independent textual authority (e.g. communities holding the Masoretic Text or Textus Receptus as theologically settled) constitute a genuine arbitrage exit?',
    'Survey denominational translation policy across traditions that explicitly reject text-critical priority (e.g. some King James Only movements, some Orthodox reliance on the Byzantine text) versus those that defer to critical editions, and assess whether the rejecting traditions bear lower effective extraction.',
    'If genuine exit traditions exist and are viable, effective extraction on confessional communities as a class is lower than modeled here and directionality should be revised toward mobile/constrained rather than constrained/trapped.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(confessional_exit_capacity, empirical, 'Whether confessional exit from text-critical authority is genuinely available to some communities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_source_text__critical_reconstructive_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t0, biblical_source_text__critical_reconstructive_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(bibl_tr_t40, biblical_source_text__critical_reconstructive_reading, theater_ratio, 40, 0.15).
narrative_ontology:measurement(bibl_tr_t80, biblical_source_text__critical_reconstructive_reading, theater_ratio, 80, 0.18).
narrative_ontology:measurement(bibl_tr_t120, biblical_source_text__critical_reconstructive_reading, theater_ratio, 120, 0.22).
narrative_ontology:measurement(bibl_tr_t160, biblical_source_text__critical_reconstructive_reading, theater_ratio, 160, 0.25).
narrative_ontology:measurement(bibl_tr_t200, biblical_source_text__critical_reconstructive_reading, theater_ratio, 200, 0.28).

% Extraction over time
narrative_ontology:measurement(bibl_be_t0, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(bibl_be_t40, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 40, 0.4).
narrative_ontology:measurement(bibl_be_t80, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 80, 0.47).
narrative_ontology:measurement(bibl_be_t120, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 120, 0.52).
narrative_ontology:measurement(bibl_be_t160, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 160, 0.55).
narrative_ontology:measurement(bibl_be_t200, biblical_source_text__critical_reconstructive_reading, base_extractiveness, 200, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t0, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(bibl_su_t40, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 40, 0.25).
narrative_ontology:measurement(bibl_su_t80, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 80, 0.3).
narrative_ontology:measurement(bibl_su_t120, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 120, 0.34).
narrative_ontology:measurement(bibl_su_t160, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 160, 0.38).
narrative_ontology:measurement(bibl_su_t200, biblical_source_text__critical_reconstructive_reading, suppression_requirement, 200, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_source_text__critical_reconstructive_reading, identity_coordination).
narrative_ontology:affects_constraint(biblical_source_text__critical_reconstructive_reading, biblical_source_text__formal_equivalence_reading).
narrative_ontology:affects_constraint(biblical_source_text__critical_reconstructive_reading, biblical_source_text__dynamic_equivalence_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the biblical_source_text kernel. formal_equivalence_reading holds source-language structure primary with intelligibility delegated to teaching; dynamic_equivalence_reading holds communicative effect in the target language primary with structural fidelity subordinated. All three readings share the same underlying manuscript evidence but disagree about what comes first in the interpretive stack, producing different beneficiary sets and different extraction profiles. Per the epsilon-invariance principle these are authored as three separate constraint files linked here, not as one constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
