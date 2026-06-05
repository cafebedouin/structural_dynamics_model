% ============================================================================
% CONSTRAINT STORY: substantive_due_process__history_tradition_test_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substantive_due_process__history_tradition_test_reading, []).

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
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: substantive_due_process__history_tradition_test_reading
 *   human_readable: Substantive Due Process: History-and-Tradition Test (Glucksberg Reading)
 *   domain: constitutional_law/doctrinal
 *
 * SUMMARY:
 *   Washington v. Glucksberg (1997) established a gatekeeping test for
 *   recognizing new fundamental rights under the Fourteenth Amendment's Due
 *   Process Clause: a liberty interest must be deeply rooted in the nation's
 *   history and tradition and described with careful precision. This
 *   constraint embodies one reading of the substantive due process doctrine —
 *   the reading that uses historical anchorage as a disciplinary mechanism to
 *   restrain judicial recognition of unenumerated rights. The doctrine serves
 *   a coordination function: it provides clear guidance to lower courts
 *   (filter novel claims) and legitimizes judicial restraint against
 *   accusations of judicial activism. Simultaneously, it extracts by
 *   suppressing novel rights claims at the threshold. This reading coexists
 *   with two sibling readings: the Lochner-era economic liberty reading
 *   (where the doctrine was used to strike down labor regulations) and the
 *   privacy-line reading (where the doctrine was used to protect intimate
 *   decisions). Each reading of the same kernel produces a different
 *   constraint with different ε values because each reading assumes different
 *   beneficiaries, different victims, and different mechanisms of
 *   suppression. The history-and-tradition test constrains novel claims but
 *   permits judicial flexibility through careful historical narration — the
 *   mechanism that allows both Glucksberg (rejecting assisted suicide) and
 *   Lawrence (recognizing intimate liberty) to invoke the same test with
 *   opposite outcomes. This flexibility renders the test partly performative
 *   (theater_ratio rising from 0.32 to 0.48 over the interval as courts'
 *   historical narratives become more freighted with policy preference). The
 *   constraint exhibits a false-summit signature: from the
 *   analytical/civilizational perspective, the history-and-tradition
 *   requirement appears as a natural law of constitutional interpretation,
 *   but the structural data reveals identifiable beneficiaries (judicial
 *   restraint doctrine, established liberty interests) and victims (novel
 *   rights claimants), indicating this is a contingent doctrinal choice that
 *   benefits certain political coalitions and could be replaced by
 *   alternative interpretive methods.
 *
 * KEY AGENTS:
 *   - Novel Rights Claimants: Primary victim (powerless/trapped/biographical) — bear the epistemic burden of proving historical tradition; cannot exit the framework without abandoning the claim's novelty
 *   - Emerging Liberty Movement: Secondary victim (moderate/constrained/generational) — can challenge the test itself, argue about tradition scope, or wait for generational consensus but faces institutional barriers
 *   - Judicial Restraint Coalition: Primary beneficiary (institutional/arbitrage/immediate) — gains clear interpretive guidance and judicial legitimacy; stabilizes judicial authority against accusations of activism
 *   - Expansionist Judicial Reading: Institutional actor (institutional/constrained/generational) — coordinates around rule-of-law constraint but experiences extraction through limited scope of permissible reasoning; can innovate through careful description of existing traditions
 *   - Historical Legitimacy Ritual: Performative institutional structure (institutional/arbitrage/civilizational) — demonstrates that the test is substantially theater; judges author historical narratives fitting preferred outcomes
 *   - Analytical Observer: Civilizational view (analytical/analytical/civilizational) — risks naturalizing a contingent doctrinal choice as an immutable feature of constitutional interpretation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substantive_due_process__history_tradition_test_reading, 0.58).
domain_priors:suppression_score(substantive_due_process__history_tradition_test_reading, 0.65).
domain_priors:theater_ratio(substantive_due_process__history_tradition_test_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substantive_due_process__history_tradition_test_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(substantive_due_process__history_tradition_test_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(substantive_due_process__history_tradition_test_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substantive_due_process__history_tradition_test_reading, tangled_rope).
narrative_ontology:human_readable(substantive_due_process__history_tradition_test_reading, "Substantive Due Process: History-and-Tradition Test (Glucksberg Reading)").
narrative_ontology:topic_domain(substantive_due_process__history_tradition_test_reading, "constitutional_law/doctrinal").

domain_priors:requires_active_enforcement(substantive_due_process__history_tradition_test_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substantive_due_process__history_tradition_test_reading, '145a6566-be7a-4151-9300-23c70e1bf4b1').
narrative_ontology:cs_kernel_codification('145a6566-be7a-4151-9300-23c70e1bf4b1', fixed_text).
narrative_ontology:cs_authority_grounding('145a6566-be7a-4151-9300-23c70e1bf4b1', lineage).
narrative_ontology:cs_interpretation_layer_present('145a6566-be7a-4151-9300-23c70e1bf4b1').
narrative_ontology:cs_reading_relation('145a6566-be7a-4151-9300-23c70e1bf4b1', substantive_due_process__lochner_economic_liberty_reading, coexists_with).
narrative_ontology:cs_reading_relation('145a6566-be7a-4151-9300-23c70e1bf4b1', substantive_due_process__privacy_line_reading, influences).
narrative_ontology:cs_axiom('145a6566-be7a-4151-9300-23c70e1bf4b1', foundational, legitimate_liberty_requires_historical_anchorage).
narrative_ontology:cs_axiom_status(legitimate_liberty_requires_historical_anchorage, holdable).
narrative_ontology:cs_axiom_grounding('145a6566-be7a-4151-9300-23c70e1bf4b1', legitimate_liberty_requires_historical_anchorage, deontological).
narrative_ontology:cs_axiom('145a6566-be7a-4151-9300-23c70e1bf4b1', secondary, careful_description_constrains_judicial_choice).
narrative_ontology:cs_axiom_status(careful_description_constrains_judicial_choice, holdable).
narrative_ontology:cs_axiom_grounding('145a6566-be7a-4151-9300-23c70e1bf4b1', careful_description_constrains_judicial_choice, instrumental).
narrative_ontology:cs_reference_frame('145a6566-be7a-4151-9300-23c70e1bf4b1', traditional_constitutional_restraint).
narrative_ontology:cs_drift_state('145a6566-be7a-4151-9300-23c70e1bf4b1', contemporary_post_lawrence, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('145a6566-be7a-4151-9300-23c70e1bf4b1', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(substantive_due_process__history_tradition_test_reading, substantive_due_process).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substantive_due_process__history_tradition_test_reading, judicial_restraint_doctrine).
narrative_ontology:constraint_beneficiary(substantive_due_process__history_tradition_test_reading, established_liberty_interests).
narrative_ontology:constraint_victim(substantive_due_process__history_tradition_test_reading, novel_rights_claimants).
narrative_ontology:constraint_victim(substantive_due_process__history_tradition_test_reading, emerging_liberty_claims).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NOVEL RIGHTS CLAIMANT (SNARE) — Trapped by the evidentiary burden: no historical pedigree exists for the claimed right (by definition of novelty). The test forecloses the claim at the threshold. No appeal mechanism within the framework itself; the only exit is to reframe the claim as derivative of an established tradition, which requires either rewriting history or abandoning the claim's novelty. Maximum suppression of exit options.
constraint_indexing:constraint_classification(substantive_due_process__history_tradition_test_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PROGRESSIVE LEGAL MOVEMENT (TANGLED ROPE) — Constrained by the doctrinal gate but not entirely trapped. Can challenge the test itself (questioning its interpretive legitimacy), can argue about what counts as 'tradition' (broadening the historical record), can wait for generational consensus to solidify the claim as traditional. Experiences both extraction (rights claims suppressed at threshold) and coordination benefit (the framework provides clear rules for adjudication). Constrained exit because reframing or temporal generational shifts require sustained organizing effort.
constraint_indexing:constraint_classification(substantive_due_process__history_tradition_test_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: JUDICIAL RESTRAINT COALITION (ROPE) — Primary beneficiary. The history-and-tradition test provides a coordination mechanism: judges have clear interpretive guidance (filter novel claims, defer to legislatures) and judicial authority is stabilized against accusations of judicial supremacy or ideology. The test reduces doctrinal controversy by relocating rights-creation authority to the legislature and historical record. Net beneficiary experiencing this as pure coordination of judicial role.
constraint_indexing:constraint_classification(substantive_due_process__history_tradition_test_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: EXPANSIONIST JUDICIAL READING (TANGLED ROPE) — Institutional actor with constrained exit. The test limits judicial innovation but does not foreclose it: judges can expand existing traditions by careful description of what past practices implicitly protected. This is what Glucksberg itself demonstrates — even applying the test strictly, judges found a liberty interest in refusing unwanted medical treatment by referencing the informed-consent tradition. Institutional actors are coordinating (around the rule of law constraint), but the test extracts by limiting the scope of permissible judicial reasoning.
constraint_indexing:constraint_classification(substantive_due_process__history_tradition_test_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: HISTORICAL LEGITIMACY RITUAL (PITON) — From a civilizational view, the history-and-tradition requirement is substantially performative. Judges author historical narratives that fit the contemporary policy conclusion they prefer. Washington v. Glucksberg (applying the test to reject assisted suicide), followed by Lawrence v. Texas (applying the test to recognize intimate liberty), demonstrates that the same doctrinal framework produces opposite outcomes depending on how 'tradition' is described. The ritual provides legitimacy cover for judicial choice rather than constraining it. Theater ratio reflects that historical narrative construction is the primary engine, not the constraint itself.
constraint_indexing:constraint_classification(substantive_due_process__history_tradition_test_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal analytical perspective, some historical grounding requirement is inherent to constitutional interpretation itself: a constitution is a document from the past being applied to the present; any interpretation must establish continuity with prior law. The history-and-tradition requirement is a necessary feature of constitutional fidelity. However, the structural data (identifiable beneficiaries, suppression of novel claims, variable application depending on narrative framing) reveals this as a false-summit candidate: what appears as an immutable constraint of constitutional method is actually a contingent doctrinal choice that could be replaced by competing frameworks (living constitutionalism, original public meaning, purposivism).
constraint_indexing:constraint_classification(substantive_due_process__history_tradition_test_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substantive_due_process__history_tradition_test_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(substantive_due_process__history_tradition_test_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(substantive_due_process__history_tradition_test_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(substantive_due_process__history_tradition_test_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(substantive_due_process__history_tradition_test_reading, TR),
    TR >= 0.70.

:- end_tests(substantive_due_process__history_tradition_test_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-to-high. The test suppresses novel rights claims at the threshold by requiring historical pedigree. For claimants seeking recognition of genuinely new liberty interests (not rooted in past practice), the barrier is nearly absolute — they cannot clear the gate. However, the test is not purely extractive because it provides a coordination function: clear rules for judicial role, reduced controversy over activism accusations, guidance for lower courts. The extractiveness has increased over the interval (0.35 → 0.58) as courts have become more sophisticated at using historical narrative framing to reach predetermined outcomes (Lawrence narrates intimate liberty broadly; Glucksberg narrates physician-assisted death narrowly). Suppression (0.65): High. Multiple suppression mechanisms: the evidentiary burden (prove historical tradition), the narrative control (judges author the history), the threshold gate (fail to establish tradition = claim fails), and the reframing requirement (must claim right as derivative of established tradition, not novel). But suppression is not total because established traditions can be reinterpreted (Lawrence broadened the intimacy tradition beyond contraception), legislative action can create new traditions over time, and some judges apply the test more expansively. Theater ratio (0.48): Moderate. The test is not pure performance because it does constrain some claims (Glucksberg itself rejected the assisted-suicide claim despite sympathetic framing). However, courts' selective emphasis on different historical precedents, the flexibility in how 'careful description' is applied, and the post-hoc rationalization of outcomes via historical narrative all indicate substantial performative content. Theater has risen over the interval (0.32 → 0.48) as courts have become more adept at narrative construction.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a sharp perspectival divergence. From the novel claimant's view (snare), the test is nearly absolute suppression with no exit. From the judicial restraint coalition's view (rope), it is pure coordination providing clear guidance and legitimacy. From the expansionist judge's view (tangled rope), it is coordination mixed with extraction — the rule constrains while providing innovation pathways. From the civilizational analytical view (mountain), it appears as an immutable constitutional principle. The perspectival gap reveals the constraint's distributional character: what appears as natural law or neutral doctrine is actually an institutional arrangement that benefits certain actors (those defending established liberty interests and judicial restraint) at the expense of others (those seeking novel rights recognition). The piton perspective shows that even the constraint's 'disciplinary' function (the history-and-tradition gate) is partly performed — the outcome often tracks judicial preference rather than historical accuracy.
 *
 * DIRECTIONALITY LOGIC:
 *   This reading's directionality values derive from the structural relationship between agent and constraint. Novel rights claimants face trapped exit: they cannot achieve recognition without satisfying the historical test, which by definition (novel claim) they cannot. Judicial restraint beneficiaries experience arbitrage exit: they can choose to apply the test stringently or flexibly to reach preferred outcomes; the test provides cover for either choice. Expansionist judges face constrained exit: they can innovate through careful description of tradition but cannot openly create new rights without historical grounding; this constraint costs them authority. Moderate legal movements face constrained exit: they must either reframe claims as traditional, challenge the test's legitimacy, or wait for time to establish new traditions. The engine derives d from these positions: trapped and institutional-arbitrage positions produce the highest perspectival gap in experienced extractiveness.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves the mandatrophy by showing that the Glucksberg test serves a genuine coordination function (clear guidance for judicial role, legitimate restraint on judicial activism claims) while simultaneously extracting (suppressing novel rights claims at the threshold). The tension between coordination and extraction is not resolvable within the reading itself — it is the reading's defining feature. The test disciplines the doctrine by requiring historical grounding, but the 'discipline' is selective: it constrains novel claims more than established traditions. The mandatrophy resolves by noting that the reading is not claiming to be pure coordination (Rope) — it explicitly acknowledges the need for restraint and gatekeeping. It is a hybrid that coordinates around the rule-of-law principle (judges follow precedent and tradition) while extracting from novel claimants (whose claims are suppressed at the threshold regardless of substantive merit). The false-summit concern (that historians may contest the test's legitimacy and application) is genuine but does not undermine the tangled-rope classification — tangled ropes often have contested boundaries.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tradition_definition_under_determination,
    'What counts as a sufficiently established ''tradition'' to ground a fundamental right? How narrow or capacious is the historical record required to be?',
    'Comparative analysis of how the test has been applied across decisions; measurement of how broadly courts have framed historical traditions (family law tradition narrowly vs. liberty of bodily integrity broadly) and whether the framing determines the outcome',
    'Narrow definition of tradition: test functions as a true constraint on novel rights (snare from novel claimants'' view). Broad definition: test becomes flexible enough to accommodate preferred outcomes (piton from civilizational view — ritual rather than constraint).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(tradition_definition_under_determination, conceptual, 'How tradition is defined determines whether the test constrains judicial choice').

omega_variable(
    judicial_narrative_authority,
    'When courts narrate the historical tradition underlying a claimed right, who adjudicates whether that narrative is accurate or merely convenient? Is the judge''s historical account subject to external verification?',
    'Examine whether courts cite competing historical interpretations, acknowledge alternative narratives, or treat their own historical descriptions as conclusive. Compare historical claims in doctrinal opinions with scholarship from professional historians.',
    'If histories are contestable and courts acknowledge alternatives: test retains some disciplinary force (tangled_rope). If judges'' narratives are treated as conclusive: test becomes performative theater (piton), enabling judicial preference-laundering through historical framing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(judicial_narrative_authority, empirical, 'Whether judges'' historical narratives are subject to external verification').

omega_variable(
    double_reading_of_tradition,
    'Is this reading (history-and-tradition as a disciplinary test) genuinely distinct from the lochner_economic_liberty_reading and privacy_line_reading, or does it merely describe the doctrinal mechanism both prior readings used?',
    'Examine whether the history-and-tradition test is the kernel''s stable feature across all three readings (i.e., a method all three use), or whether each reading instantiates a different operative test. If the test is the same structure in all three, the distinctiveness lies in WHICH traditions are recognized as legitimate, not in the test mechanism itself.',
    'If the test is the stable kernel method: the three readings should be formulated as different historical narratives within one constraint, not as three separate constraints. If the readings genuinely use different methods: the history-and-tradition framing is unique to this reading. Current framing assumes the latter; resolution may require reconceptualization of the kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(double_reading_of_tradition, conceptual, 'Whether this reading''s test mechanism is distinct or merely the method shared across all three readings').

omega_variable(
    false_summit_natural_law_status,
    'Is the history-and-tradition requirement genuinely a natural law of constitutional interpretation (immutable), or is it a contingent doctrinal choice that could be replaced by competing interpretive methodologies (living constitutionalism, originalism, purposivism)?',
    'Historical evidence: courts have applied substantive due process using different methodologies in different eras (Lochner-era unenumerated economic liberty vs. privacy-era liberty interest vs. contemporary dignity-based approaches). Each methodology claimed constitutional legitimacy. The test is not invariant across constitutional interpretation schools.',
    'If natural law: the mountain classification is correct; no reformable institutional structure. If contingent: the test is a false summit (beneficiary-serving constraint naturalized as immutable method). Resolves whether FSM signature override applies.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law_status, empirical, 'Whether the history-and-tradition requirement is immutable or contingent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substantive_due_process__history_tradition_test_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sdpt_tr_t0, substantive_due_process__history_tradition_test_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(sdpt_tr_t10, substantive_due_process__history_tradition_test_reading, theater_ratio, 10, 0.4).
narrative_ontology:measurement(sdpt_tr_t20, substantive_due_process__history_tradition_test_reading, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(sdpt_be_t0, substantive_due_process__history_tradition_test_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sdpt_be_t10, substantive_due_process__history_tradition_test_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(sdpt_be_t20, substantive_due_process__history_tradition_test_reading, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(sdpt_su_t0, substantive_due_process__history_tradition_test_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(sdpt_su_t10, substantive_due_process__history_tradition_test_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(sdpt_su_t20, substantive_due_process__history_tradition_test_reading, suppression_requirement, 20, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substantive_due_process__history_tradition_test_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(substantive_due_process__history_tradition_test_reading, substantive_due_process__lochner_economic_liberty_reading).
narrative_ontology:affects_constraint(substantive_due_process__history_tradition_test_reading, substantive_due_process__privacy_line_reading).

% DUAL FORMULATION NOTE:
% The substantive due process kernel has three structurally distinct readings, each with different ε values, beneficiary/victim sets, and application mechanisms. The history_tradition_test_reading (this constraint) has ε=0.58 (tangled rope). The lochner_economic_liberty_reading has higher extractiveness (ε≈0.65+) because it involves striking down protective legislation in favor of abstract liberty. The privacy_line_reading has lower theater (different beneficiary set protects intimate autonomy rather than judicial restraint doctrine). All three readings share the kernel (the constitutional text and clause) but diverge in how they identify legitimate traditions and apply the gatekeeping mechanism. Network links show that this reading's application influences both sibling readings: the history-and-tradition test adopted in Glucksberg created the framework within which privacy-line claims must be narrated as traditional, and within which lochner-era economic liberty claims could be distinguished as illegitimate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(substantive_due_process__history_tradition_test_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
