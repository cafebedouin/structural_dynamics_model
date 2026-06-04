% ============================================================================
% CONSTRAINT STORY: habeas_corpus_act_1679__modern_detention_tests_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_habeas_corpus_act_1679__modern_detention_tests_reading, []).

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
 *   constraint_id: habeas_corpus_act_1679__modern_detention_tests_reading
 *   human_readable: Habeas Corpus Act 1679: Modern Detention Tests Reading
 *   domain: constitutional_law/executive_detention
 *
 * SUMMARY:
 *   This constraint instantiates the modern_detention_tests_reading of the
 *   habeas_corpus_act_1679 kernel. The reading focuses on how the
 *   seventeenth-century machinery of the writ still functions as the live
 *   instrument for testing detention legality, but operates at the margins
 *   where modern security doctrine has substantially reshaped its practical
 *   force. The classic writ (habeas corpus ad subjiciendum) requires return
 *   and tests the lawfulness of detention, yet modern frameworks —
 *   security-sensitive procedures, Belmarsh declarations, in camera evidence,
 *   judicial deference to executive security judgment — have created
 *   procedural and doctrinal attenuations that reduce the writ's protective
 *   scope while maintaining its rhetorical centrality. The constraint
 *   exhibits high suppression (0.72) because indefinite detention pending
 *   judicial review, security exceptions, and procedural delays create
 *   genuine barriers to release, yet it remains a tangled rope rather than a
 *   pure snare because the writ still coordinates legitimate detention review
 *   and detainees with counsel can invoke real (if attenuated) judicial
 *   oversight. The measurements track the rise in suppression and
 *   extractiveness from the 1679 baseline through post-1998 incorporation of
 *   human rights law and post-2001 security framework expansion, showing how
 *   the same writ has functioned under progressively more constraining
 *   security doctrine.
 *
 * KEY AGENTS:
 *   - Detained suspects without counsel (powerless/trapped) — cannot access the writ's machinery; face indefinite detention with no functional exit
 *   - Detained suspects with counsel (moderate/constrained) — can invoke the writ; face constrained access to justice through security procedures, in camera evidence, and doctrinal deference
 *   - The judiciary (institutional/arbitrage) — coordinates detention review through habeas; benefits from the writ's legitimacy as symbol of judicial constraint on executive power
 *   - The security executive (institutional/arbitrage) — operates within the writ's framework while extracting through security-sensitive procedures, deference doctrine, and procedural delays that permit indefinite detention pending review
 *   - Civil rights advocates (moderate/constrained) — benefit from the writ's continued existence as a doctrinal tool; constrained by the writ's modern margins and security exceptions
 *   - The 1679 text as doctrinal continuity (institutional/arbitrage) — maintains legitimating force while its actual operative scope has been substantially reconstructed through modern doctrine
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(habeas_corpus_act_1679__modern_detention_tests_reading, 0.58).
domain_priors:suppression_score(habeas_corpus_act_1679__modern_detention_tests_reading, 0.72).
domain_priors:theater_ratio(habeas_corpus_act_1679__modern_detention_tests_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(habeas_corpus_act_1679__modern_detention_tests_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(habeas_corpus_act_1679__modern_detention_tests_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(habeas_corpus_act_1679__modern_detention_tests_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(habeas_corpus_act_1679__modern_detention_tests_reading, tangled_rope).
narrative_ontology:human_readable(habeas_corpus_act_1679__modern_detention_tests_reading, "Habeas Corpus Act 1679: Modern Detention Tests Reading").
narrative_ontology:topic_domain(habeas_corpus_act_1679__modern_detention_tests_reading, "constitutional_law/executive_detention").

domain_priors:requires_active_enforcement(habeas_corpus_act_1679__modern_detention_tests_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(habeas_corpus_act_1679__modern_detention_tests_reading, '5b84b1c9-ab80-4145-82d1-224180ce30a2').
narrative_ontology:cs_kernel_codification('5b84b1c9-ab80-4145-82d1-224180ce30a2', fixed_text).
narrative_ontology:cs_authority_grounding('5b84b1c9-ab80-4145-82d1-224180ce30a2', lineage).
narrative_ontology:cs_interpretation_layer_present('5b84b1c9-ab80-4145-82d1-224180ce30a2').
narrative_ontology:cs_reading_relation('5b84b1c9-ab80-4145-82d1-224180ce30a2', habeas_corpus_act_1679__procedural_teeth_reading, coexists_with).
narrative_ontology:cs_reading_relation('5b84b1c9-ab80-4145-82d1-224180ce30a2', habeas_corpus_act_1679__suspension_history_reading, coexists_with).
narrative_ontology:cs_axiom('5b84b1c9-ab80-4145-82d1-224180ce30a2', foundational, seventeenth_century_machinery_still_live).
narrative_ontology:cs_axiom_status(seventeenth_century_machinery_still_live, holdable).
narrative_ontology:cs_axiom_grounding('5b84b1c9-ab80-4145-82d1-224180ce30a2', seventeenth_century_machinery_still_live, conventional).
narrative_ontology:cs_axiom('5b84b1c9-ab80-4145-82d1-224180ce30a2', foundational, security_exceptions_and_deference_are_modern_margins).
narrative_ontology:cs_axiom_status(security_exceptions_and_deference_are_modern_margins, holdable).
narrative_ontology:cs_axiom_grounding('5b84b1c9-ab80-4145-82d1-224180ce30a2', security_exceptions_and_deference_are_modern_margins, empirically_contingent).
narrative_ontology:cs_reference_frame('5b84b1c9-ab80-4145-82d1-224180ce30a2', common_law_habeas_undisturbed).
narrative_ontology:cs_drift_state('5b84b1c9-ab80-4145-82d1-224180ce30a2', post_2001_security_framework, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('5b84b1c9-ab80-4145-82d1-224180ce30a2', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(habeas_corpus_act_1679__modern_detention_tests_reading, habeas_corpus_act_1679).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(habeas_corpus_act_1679__modern_detention_tests_reading, detainees_with_counsel).
narrative_ontology:constraint_beneficiary(habeas_corpus_act_1679__modern_detention_tests_reading, civil_rights_advocates).
narrative_ontology:constraint_victim(habeas_corpus_act_1679__modern_detention_tests_reading, internment_schemes).
narrative_ontology:constraint_victim(habeas_corpus_act_1679__modern_detention_tests_reading, indefinite_executive_detention).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DETAINED SUSPECT WITHOUT COUNSEL (SNARE) — Faces indefinite detention without effective access to the writ. The seventeenth-century machinery is silent to those who cannot afford or locate counsel to invoke it. Maximum suppression: trapped in the executive's holding pattern with no exit. The writ exists but is functionally inaccessible.
constraint_indexing:constraint_classification(habeas_corpus_act_1679__modern_detention_tests_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DETAINED SUSPECT WITH COUNSEL (TANGLED ROPE) — Can invoke the writ through legal representation; the seventeenth-century machinery still functions as live instrument. However, success is constrained: Belmarsh declarations, security-sensitive procedures, in camera evidence, and deference to executive on national security grounds limit the writ's actual force. Mixed outcome: the constraint coordinates the legal process (beneficiary effect) while suppressing the detainee's liberty (victim effect). The writ provides some exit but not full release.
constraint_indexing:constraint_classification(habeas_corpus_act_1679__modern_detention_tests_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE JUDICIARY / HIGH COURT (ROPE) — Experiences the writ as pure coordination mechanism. The court's duty to hear habeas petitions and test detention legality structures orderly judicial review. The 1679 Act's procedural framework (deadlines, mandatory hearings, written returns) enables the judiciary to coordinate the rule of law. The court benefits from the writ's legitimacy (arbitrage position) — it frames judicial review as enforcing ancient rights rather than second-guessing executive security judgments.
constraint_indexing:constraint_classification(habeas_corpus_act_1679__modern_detention_tests_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: THE SECURITY EXECUTIVE (TANGLED ROPE) — The writ is both constraint and coordination mechanism. It coordinates legitimate detention authority (the executive can detain for lawful cause, and the writ tests that cause) while extracting by allowing indefinite detention pending judicial review, security-sensitive procedures that obscure evidence, and deference doctrine that permits detention where normal criminal procedure would not. The executive benefits from the writ's legitimacy (claims to uphold the rule of law while maintaining detention) while minimizing actual constraints through procedural attenuation.
constraint_indexing:constraint_classification(habeas_corpus_act_1679__modern_detention_tests_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: THE SEVENTEENTH-CENTURY TEXT / DOCTRINAL CONTINUITY (PITON) — The 1679 Act persists as the live instrument through rhetorical invocation and formal legal standing, but its functional meaning has become primarily performative. Modern detention operates on security-sensitive procedures, in camera evidence, Belmarsh declarations, and executive deference doctrine — mechanisms the 1679 text does not contain. The writ is maintained as the legitimating symbol of judicial review, but the actual constraint on executive detention is substantially lower than the seventeenth-century text implies. Theater ratio reflects this: the formal writ is invoked continuously, but its practical effect on detention authority is dampened by modern doctrinal layers.
constraint_indexing:constraint_classification(habeas_corpus_act_1679__modern_detention_tests_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / IMMUTABLE SEPARATION OF POWERS VIEW (MOUNTAIN) — From a universal/civilizational perspective, the tension between executive detention authority and judicial review is structurally inherent to separation of powers: any system must permit temporary detention pending judicial determination, and that determination itself requires time. From this view, the modern tests are natural limits on habeas — not suppression but the logical boundaries of a coordinate system. However, the structural data (beneficiaries capturing security-detention advantage, victims bearing indefinite delay, measured suppression at 0.72) suggests this is a false summit: what appears as immutable constitutional logic may be a naturalization of contingent doctrinal choices about deference, security exceptions, and procedural delays.
constraint_indexing:constraint_classification(habeas_corpus_act_1679__modern_detention_tests_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(habeas_corpus_act_1679__modern_detention_tests_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(habeas_corpus_act_1679__modern_detention_tests_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(habeas_corpus_act_1679__modern_detention_tests_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(habeas_corpus_act_1679__modern_detention_tests_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(habeas_corpus_act_1679__modern_detention_tests_reading, TR),
    TR >= 0.70.

:- end_tests(habeas_corpus_act_1679__modern_detention_tests_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts through indefinite detention pending judicial review, security-sensitive procedures that obscure evidence, and doctrinal deference to executive judgment on national security. However, it is not maximal extraction (snare, ε ≥ 0.66) because the writ does coordinate genuine judicial review and detainees with counsel can succeed in obtaining release through habeas petitions. The measurement trajectory shows extraction rising from a 1679 baseline (0.35, when the writ faced few modern security exceptions) through post-1998 human rights incorporation (0.48, when security detention became framed as a human rights balancing question) to the current post-2001 framework (0.58, when security exceptions, Belmarsh declarations, and deference doctrine matured). Suppression (0.72): High. Significant barriers exist to immediate release: indefinite detention pending review, security-sensitive procedures, Belmarsh exceptions, in camera evidence, judicial deference, and the sheer resource requirements of mounting a successful habeas petition. These barriers are structural, not merely circumstantial. Suppression has risen over time as security frameworks have accumulated additional procedural layers. Theater ratio (0.48): Moderate. The writ's invocation is frequent and formal (court hearings, written returns, judicial pronouncements about ancient rights), but the functional outcome (actual release vs. detention orders upheld) is increasingly determined by security doctrine rather than by the writ's seventeenth-century mechanics. The theater has increased over time as the gap between the writ's rhetorical centrality and its actual operative scope has widened.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits a profound perspectival gap between powerless detainees (snare) and institutional actors (rope, tangled rope, piton). The detainee without counsel sees pure extraction with no exit. The detainee with counsel sees mixed coordination and extraction — the writ functions but is constrained. The judiciary sees coordination — the writ structures orderly review. The executive sees coordination and arbitrage — the writ legitimates detention authority while security doctrine permits indefinite detention. The text-as-doctrine sees itself as performing a core constraint (piton perspective) — still invoked, but functionally bypassed by modern frameworks. The analytical observer risks seeing an immutable separation-of-powers requirement (mountain) when the structural data reveals that the suppression and extractiveness are historically contingent on doctrinal choices about deference, security exceptions, and procedural delay.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) derives from the agent's structural position relative to this constraint. The detainee without counsel (powerless, trapped) experiences maximum extraction — no access to the writ's machinery, no exit, no benefit. The detainee with counsel (moderate, constrained) experiences moderate extraction — some access, constrained success, some benefit from judicial oversight. The judiciary (institutional, arbitrage) experiences negative extraction — the writ coordinates their authority and legitimates judicial review as boundary on executive power. The security executive (institutional, arbitrage) also experiences negative extraction in formal terms, but the structural data reveals asymmetry: security-sensitive procedures, deference doctrine, and indefinite detention pending review shift effective extraction toward the executive, making the 'arbitrage' position more beneficial than the formal classification suggests. The piton perspective (theater ratio) reflects that the 1679 text persists as legitimating symbol while modern doctrine governs actual outcomes.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exemplifies how the same formal writ can instantiate different types across different observer positions, and how doctrinal evolution shifts the classification over time. The writ is simultaneously a coordination mechanism (rope for institutional actors), a partial protection with substantial constraints (tangled rope for detainees with counsel), a pure extraction mechanism (snare for detainees without counsel), a degraded symbol (piton for the 1679 text itself as formal doctrine), and an apparently immutable separation-of-powers requirement (false-summit mountain for the analytical observer). The measurements show the trajectory: as security doctrine has accumulated (post-1998 HRA incorporation, post-2001 security frameworks), suppression and extractiveness have risen while theater ratio has increased — the gap between the writ's rhetorical centrality and its actual protective scope has widened. The mandatrophy is resolved by recognizing that all six types are legitimate readings from their respective observer positions, and the historical trend is toward snare classification for powerless detainees (those without counsel) as security exceptions accumulate, while institutional actors experience continued coordination and arbitrage benefits.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    indefinite_detention_vs_coordinate_review,
    'Does the seventeenth-century writ''s machinery actually constrain indefinite executive detention, or does modern security doctrine permit detention pending review to expand beyond the writ''s original protective intent?',
    'Historical comparison: detention duration distributions pre-1998 (prior to HRA incorporation) vs post-2001 (post-9/11 security framework) vs post-2011 (post-judicial review of security detention). Measure: median time from arrest to habeas hearing to final release/conviction.',
    'If constraint is effective: extraction ε lowers, snare classification drops. If modern security doctrine permits extended indefinite detention: extraction ε rises, classification shifts toward snare for powerless detainees.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(indefinite_detention_vs_coordinate_review, empirical, 'Whether modern security doctrine permits indefinite detention despite the writ').

omega_variable(
    security_sensitive_evidence_collapse,
    'Do in camera proceedings and security-sensitive evidence frameworks functionally vitiate the writ''s power to test detention legality, since the detainee''s counsel cannot see or challenge the evidence?',
    'Empirical: Rate of habeas success when security-sensitive evidence is deployed vs. standard evidence. Procedural audit: how many detention orders rely on wholly closed evidence vs. partially disclosed. Outcome tracking: detainees released after full evidence disclosure vs. ordered released after in camera review where evidence remained concealed.',
    'If security procedures gut the writ: suppression rises toward 0.85+, classification shifts snare for detainees with counsel. If writ remains functional despite procedures: suppression stabilizes, tangled_rope holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(security_sensitive_evidence_collapse, empirical, 'Whether in camera procedures vitiate the writ''s protective function').

omega_variable(
    belmarsh_declaration_binding_scope,
    'Does the Belmarsh declaration (indefinite detention of foreign nationals under anti-terrorism powers) represent a genuine exception to habeas constraint or a routinized doctrinal expansion that now governs security detention generally?',
    'Legal analysis: track post-Belmarsh detention frameworks in UK law (Prevention of Terrorism Acts, immigration detention, national security grounds for denying bail). Measure: proportion of security detentions now authorized under Belmarsh doctrine vs. traditional custody law.',
    'If Belmarsh is exception: constraint ε remains moderate. If Belmarsh logic has become default security detention framework: extraction ε rises, suppression rises, classification shifts toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(belmarsh_declaration_binding_scope, empirical, 'Scope of Belmarsh exception as expanded framework for security detention').

omega_variable(
    judicial_deference_as_suppression,
    'Is judicial deference to executive security judgment (the court''s reluctance to second-guess national security determinations) a legitimate limit on habeas power or a doctrinal suppression mechanism that neutralizes the writ''s constraint?',
    'Doctrinal analysis: when courts invoke deference to security judgment, do they still conduct substantive habeas review or do they defer entirely? Outcome measurement: rate of detention orders upheld on deference vs. rates at which deference is overridden by clear evidence of illegality.',
    'If deference permits genuinely protective review: suppression moderate. If deference doctrine permits detention despite absence of credible evidence: suppression rises, classification shifts snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_deference_as_suppression, conceptual, 'Whether judicial deference to security is legitimate limit or suppression mechanism').

omega_variable(
    ancient_writ_vs_modern_framework,
    'Does the 1679 Act''s text and procedural machinery remain the live instrument of detention review, or has modern security doctrine (HRA, deference, security-sensitive procedures, immigration detention) so substantially reconstructed the framework that the seventeenth-century writ is primarily symbolic?',
    'Textual and doctrinal analysis: trace which aspects of modern habeas practice derive from the 1679 Act''s express terms (deadlines, habeas corpus ad subjiciendum, return requirements) vs. modern judicial constructions or statutory overlays (HRA incorporation, Belmarsh doctrine, security procedure rules). How many modern detention cases cite the 1679 Act as controlling vs. modern security frameworks?',
    'If 1679 Act remains live: constraint classification and ε estimates hold. If 1679 Act is primarily symbolic and modern doctrine is controlling: piton classification confirmed, theater ratio rises toward 0.60+.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ancient_writ_vs_modern_framework, conceptual, 'Whether 1679 Act remains live instrument or is primarily symbolic').

omega_variable(
    kernel_reading_foreclosure,
    'Does the modern_detention_tests_reading foreclose, coexist with, or influence the procedural_teeth_reading (the 1679 Act''s genius was enforcement machinery) and the suspension_history_reading (liberty defined by when suspension is permitted)?',
    'Structural analysis of competing premises: modern_detention_tests emphasizes that the seventeenth-century machinery is still live and its margins (security exceptions, deference) are where the real constraint operates. procedural_teeth emphasizes that deadlines, penalties, and no-re-commitment rules are the genius. suspension_history emphasizes that the definition of liberty depends on when Parliament may suspend. These are compatible readings (coexist) or do they entail different constitutional architectures?',
    'If readings coexist: all three are live perspectives on the same kernel. If modern_detention_tests forecloses suspension_history: contemporary doctrine treats suspension as superseded. If procedural_teeth influences modern_detention_tests: modern margins are themselves constrained by the 1679 procedures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Structural relationship between modern detention, procedural, and suspension readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(habeas_corpus_act_1679__modern_detention_tests_reading, 0, 350).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(habeas_modern_theater_1679, habeas_corpus_act_1679__modern_detention_tests_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(habeas_modern_theater_post_1998_hra, habeas_corpus_act_1679__modern_detention_tests_reading, theater_ratio, 80, 0.4).
narrative_ontology:measurement(habeas_modern_theater_post_2001_security, habeas_corpus_act_1679__modern_detention_tests_reading, theater_ratio, 100, 0.48).

% Extraction over time
narrative_ontology:measurement(habeas_modern_extract_1679, habeas_corpus_act_1679__modern_detention_tests_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(habeas_modern_extract_post_1998_hra, habeas_corpus_act_1679__modern_detention_tests_reading, base_extractiveness, 80, 0.48).
narrative_ontology:measurement(habeas_modern_extract_post_2001_security, habeas_corpus_act_1679__modern_detention_tests_reading, base_extractiveness, 100, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(habeas_modern_suppression_1679, habeas_corpus_act_1679__modern_detention_tests_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(habeas_modern_suppression_post_1998_hra, habeas_corpus_act_1679__modern_detention_tests_reading, suppression_requirement, 80, 0.65).
narrative_ontology:measurement(habeas_modern_suppression_post_2001_security, habeas_corpus_act_1679__modern_detention_tests_reading, suppression_requirement, 100, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(habeas_corpus_act_1679__modern_detention_tests_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(habeas_corpus_act_1679__modern_detention_tests_reading, habeas_corpus_act_1679__procedural_teeth_reading).
narrative_ontology:affects_constraint(habeas_corpus_act_1679__modern_detention_tests_reading, habeas_corpus_act_1679__suspension_history_reading).
narrative_ontology:affects_constraint(habeas_corpus_act_1679__modern_detention_tests_reading, belmarsh_declaration_security_exception).
narrative_ontology:affects_constraint(habeas_corpus_act_1679__modern_detention_tests_reading, in_camera_evidence_security_procedure).

% DUAL FORMULATION NOTE:
% Three readings of the habeas_corpus_act_1679 kernel form a family: modern_detention_tests (this story, focusing on seventeenth-century machinery operating under modern security margins), procedural_teeth (focusing on the 1679 Act's enforcement mechanisms as the real constraint), and suspension_history (focusing on the conditional, suspendable nature of the right). Each reading has its own constraint story with distinct ε values and beneficiary/victim structures. Network links connect all three as mutual influences within the kernel's interpretive space.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(habeas_corpus_act_1679__modern_detention_tests_reading, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
