% ============================================================================
% CONSTRAINT STORY: habeas_corpus_act_1679__suspension_history_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_habeas_corpus_act_1679__suspension_history_reading, []).

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
 *   constraint_id: habeas_corpus_act_1679__suspension_history_reading
 *   human_readable: Habeas Corpus Act 1679: Suspension History Reading
 *   domain: legal/doctrinal/constitutional
 *
 * SUMMARY:
 *   The suspension_history_reading instantiates one foundational
 *   interpretation of the Habeas Corpus Act 1679: that the liberty it
 *   purports to protect is defined not by an unqualified right but by the
 *   conditional grant of Parliament to permit or forbid the writ. This
 *   reading traces the historical pattern of suspension during wartime and
 *   rebellion — the Suspension Acts of 1793-1801 (Napoleonic Wars), 1914-1918
 *   (World War I), 1939-1945 (World War II), and the Northern Ireland
 *   detention regime (1972-present, with interruptions) — to argue that
 *   habeas corpus as a practical institution is the
 *   writ-plus-the-suspension-mechanism, not the writ alone. The reading does
 *   not claim suspension is unjust or that parliaments have acted unlawfully;
 *   rather, it claims that the structural reality of habeas is that its
 *   operation is contingent on parliamentary forbearance, and this
 *   contingency is built into the doctrine itself. The constraint exhibits
 *   characteristics of tangled_rope: it coordinates legitimate emergency
 *   response (Parliament and executive jointly managing security crises)
 *   while simultaneously extracting liberty from those detained during
 *   suspension windows. The victim set is not 'all subjects of the Crown' but
 *   specifically 'the detained during suspension periods,' who experience the
 *   writ as a suspended threat rather than a protection. The beneficiary is
 *   the emergency executive detention power — the capacity to hold persons
 *   without trial during declared crises. The suppression (0.72) reflects
 *   that during suspension windows, no procedural safeguard protects the
 *   detained; the suppression is not total (0.82 at peak crisis) because
 *   suspension Acts are theoretically temporary and Parliament retains formal
 *   authority to revoke them.
 *
 * KEY AGENTS:
 *   - The Detained in Suspension Windows: Primary victim (powerless/trapped) — bear full extraction of liberty during wartime/rebellion suspensions; zero exit, zero protection, zero alternatives
 *   - The Emergency Executive: Primary beneficiary (institutional/arbitrage) — gains detention authority without procedural constraint during suspension; chooses statutory route over prerogative for legitimacy
 *   - The Parliamentary Majority: Secondary beneficiary (institutional/arbitrage) — gains control lever to authorize emergency detention collectively; preserves parliamentary supremacy fiction
 *   - The Parliamentary Opposition: Secondary victim (moderate/constrained) — face majoritarian foreclosure during suspension; can organize opposition in normal times but locked out during crises
 *   - The Judiciary: Mixed (powerful/mobile) — coordination function in normal times (habeas review), extraction in suspension (loss of jurisdiction, standby role)
 *   - The Abolitionist/Rights Coalition: Organized (organized/constrained) — see ratcheting pattern (suspension windows expand, thresholds lower); agency through advocacy but facing adverse institutional drift
 *   - The Analytical Observer: Neutral (analytical/analytical) — risks naturalizing the suspension mechanism as inherent to parliamentary sovereignty rather than as a constituted institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(habeas_corpus_act_1679__suspension_history_reading, 0.58).
domain_priors:suppression_score(habeas_corpus_act_1679__suspension_history_reading, 0.72).
domain_priors:theater_ratio(habeas_corpus_act_1679__suspension_history_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(habeas_corpus_act_1679__suspension_history_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(habeas_corpus_act_1679__suspension_history_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(habeas_corpus_act_1679__suspension_history_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(habeas_corpus_act_1679__suspension_history_reading, tangled_rope).
narrative_ontology:human_readable(habeas_corpus_act_1679__suspension_history_reading, "Habeas Corpus Act 1679: Suspension History Reading").
narrative_ontology:topic_domain(habeas_corpus_act_1679__suspension_history_reading, "legal/doctrinal/constitutional").

domain_priors:requires_active_enforcement(habeas_corpus_act_1679__suspension_history_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(habeas_corpus_act_1679__suspension_history_reading, '133aaed3-4e77-431b-a41c-42a49016f1fd').
narrative_ontology:cs_kernel_codification('133aaed3-4e77-431b-a41c-42a49016f1fd', formalized).
narrative_ontology:cs_authority_grounding('133aaed3-4e77-431b-a41c-42a49016f1fd', extraction).
narrative_ontology:cs_interpretation_layer_present('133aaed3-4e77-431b-a41c-42a49016f1fd').
narrative_ontology:cs_reading_relation('133aaed3-4e77-431b-a41c-42a49016f1fd', habeas_corpus_act_1679__procedural_teeth_reading, influences).
narrative_ontology:cs_reading_relation('133aaed3-4e77-431b-a41c-42a49016f1fd', habeas_corpus_act_1679__modern_detention_tests_reading, coexists_with).
narrative_ontology:cs_axiom('133aaed3-4e77-431b-a41c-42a49016f1fd', foundational, liberty_contingent_on_parliamentary_forbearance).
narrative_ontology:cs_axiom_status(liberty_contingent_on_parliamentary_forbearance, holdable).
narrative_ontology:cs_axiom_grounding('133aaed3-4e77-431b-a41c-42a49016f1fd', liberty_contingent_on_parliamentary_forbearance, deontological).
narrative_ontology:cs_axiom('133aaed3-4e77-431b-a41c-42a49016f1fd', foundational, suspension_mechanism_integral_to_habeas_structure).
narrative_ontology:cs_axiom_status(suspension_mechanism_integral_to_habeas_structure, holdable).
narrative_ontology:cs_axiom_grounding('133aaed3-4e77-431b-a41c-42a49016f1fd', suspension_mechanism_integral_to_habeas_structure, empirically_contingent).
narrative_ontology:cs_reference_frame('133aaed3-4e77-431b-a41c-42a49016f1fd', habeas_as_conditional_parliamentary_grant).
narrative_ontology:cs_drift_state('133aaed3-4e77-431b-a41c-42a49016f1fd', contemporary_permanent_emergency, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('133aaed3-4e77-431b-a41c-42a49016f1fd', '').
narrative_ontology:cs_kernel_id(habeas_corpus_act_1679__suspension_history_reading, habeas_corpus_act_1679).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(habeas_corpus_act_1679__suspension_history_reading, emergency_executive_detention_power).
narrative_ontology:constraint_victim(habeas_corpus_act_1679__suspension_history_reading, detained_persons_suspension_windows).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE DETAINED IN SUSPENSION WINDOWS (SNARE) — During wartime or rebellion, when Parliament suspends habeas, the detained have zero exit options and zero protection. They experience the constraint as pure extraction: the writ they otherwise depend on is statutorily neutralized, leaving them at executive discretion. Maximum suppression, no alternatives, no agency. The liberty itself vanishes by act of Parliament.
constraint_indexing:constraint_classification(habeas_corpus_act_1679__suspension_history_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PARLIAMENTARY OPPOSITION (TANGLED ROPE) — The minority in Parliament experiences the suspension Acts as both coordination mechanism (Parliament legitimately acts in emergency) and extraction mechanism (their political voice is overridden by majority and executive when suspension passes). They benefit from the principle of parliamentary supremacy in normal times but are locked out during crises. Constrained exit — they can organize opposition but face majoritarian foreclosure during emergencies.
constraint_indexing:constraint_classification(habeas_corpus_act_1679__suspension_history_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE EMERGENCY EXECUTIVE (ROPE) — Experiences the constraint as pure coordination: the Suspension Acts enable Parliament and executive to jointly manage security crises without the procedural delays the writ would impose. The executive has real exit options (common law prerogative theories, martial law) but chooses the statutory suspension pathway because it is cleaner and has parliamentary endorsement. Net beneficiary — the extraction is structured as legitimate authorization.
constraint_indexing:constraint_classification(habeas_corpus_act_1679__suspension_history_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: THE PARLIAMENTARY MAJORITY (ROPE) — The voting coalition that passes suspension Acts experiences the constraint as coordination: Parliament collectively solves the security problem by temporarily lifting an individual safeguard. The majority has arbitrage options (prerogative powers, executive initiatives) but prefers the parliamentary route because it preserves the fiction of parliamentary control over emergency. Coordination mechanism — the constraint structures how Parliament and executive jointly allocate emergency authority.
constraint_indexing:constraint_classification(habeas_corpus_act_1679__suspension_history_reading, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: THE JUDICIARY (TANGLED ROPE) — The judiciary experiences the constraint as both coordination and extraction. In normal times, habeas is a coordination mechanism through which the courts protect liberty by checking executive detention. During suspension, the courts are coordinating with Parliament and executive to temporarily stand aside. But the judiciary is also extracted from — they lose their function, their authority to review detention, and their role in the liberty safeguard. Mobile exit (other judicial powers, common law doctrines) but constrained by legitimacy: courts cannot simply ignore a suspension Act.
constraint_indexing:constraint_classification(habeas_corpus_act_1679__suspension_history_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: THE ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a purely logical standpoint, if a right can be suspended by statute, it is not a right — it is a license granted by the sovereign. The 'liberty of habeas corpus' is therefore not an unchangeable natural law but a conditional grant that Parliament may revoke. This perspective naturalizes the suspension mechanism as an inherent feature of parliamentary sovereignty — there is no fixed liberty, only the liberty Parliament permits. However, the structural data reveals this as a false summit: the mountain classification naturalizes what is actually a constituted institutional choice to make liberty contingent on parliamentary forbearance.
constraint_indexing:constraint_classification(habeas_corpus_act_1679__suspension_history_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: THE ABOLITIONIST/RIGHTS COALITION (TANGLED ROPE) — Organized agents (civil liberties groups, progressive legal scholars, human rights advocates) experience the constraint as both coordination and extraction. The historical trajectory of habeas — from suspension during wars of religion through Napoleonic wars to 20th-century emergency detention — shows a coordination function: the writ + suspension Acts allow Parliament to respond to genuine emergencies without permanent constitutional collapse. But the pattern also shows extraction: each suspension ratchets executive power upward, suspension windows expand, and the threshold for crisis invocation lowers. The coalition has agency and political voice (constrained exit) but faces an adverse ratchet where each coordination round gives executives more discretion.
constraint_indexing:constraint_classification(habeas_corpus_act_1679__suspension_history_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(habeas_corpus_act_1679__suspension_history_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(habeas_corpus_act_1679__suspension_history_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(habeas_corpus_act_1679__suspension_history_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(habeas_corpus_act_1679__suspension_history_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(habeas_corpus_act_1679__suspension_history_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The suspension mechanism is not pure extraction because it is (1) statutorily authorized and (2) theoretically temporary. However, the historical pattern shows extractiveness well above what pure coordination would predict. The measure reflects the constraint as experienced during the sustained suspension state (t=2-3), not the normal state (t=0). During suspension, the detained face arbitrary detention without habeas review; that is high extractiveness. The measurement interval starts at normal operations (ε=0.15, pure coordination) and demonstrates the transition through early suspension (ε=0.42) to crisis peak (ε=0.68, approaching snare) to sustained suspension (ε=0.58, stabilized tangled_rope). Suppression (0.72): High. During suspension, the detained have no procedural safeguard, no right to petition, no judicial review, no exit options. The suppression is not total (0.82 at peak) because suspension Acts are theoretically time-limited and Parliament retains formal power to revoke them. In practice, this formal power is weak: the Northern Ireland suspension lasted 28 years (1972-2000, with interruptions and renewals), suggesting the nominal temporality is decoupled from actual duration. Theater ratio (0.35): Low. Unlike the verification bottleneck or many institutional constraints, the Suspension Acts are not performative. The writ is simply suspended — the procedural machinery of the 1679 Act (deadlines, penalties, habeas non-obstructed) is not invoked. There is no ritual, no review theater, no appearance of safeguard. The low theater reflects that suspension is structurally straightforward: Parliament passes an Act, habeas ceases to issue, detention becomes executive discretion. This is coordination without theater, not extraction disguised as coordination.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is maximal. The detained in suspension see snare (pure extraction, no exit, no alternatives, no procedure). The parliamentary majority see rope (coordination of emergency response). The emergency executive sees rope (authorization to manage security). The opposition sees tangled_rope (coordination that extracts from them politically). The judiciary see tangled_rope (coordination + extraction of function). The rights coalition see tangled_rope with ratcheting (each suspension window lowers the threshold for the next). The natural law observer sees mountain (parliamentary sovereignty entails the power to suspend any right). The suspension_history reading synthesizes these gaps: the constraint is a machine for coordinating emergency response while simultaneously extracting liberty from the detained, and that extraction is not accidental — it is the point of the constraint. The Suspension Acts exist to detain people without habeas review during crises. That is the coordination mechanism: how do we make it legally possible for the executive to act fast without judicial delay? Answer: Parliament suspends the writ.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality is structured through the suspension mechanism. In normal times (t=0), the constraint is nearly pure coordination: habeas protects liberty, courts review detention, executive respects the writ. From the executive's perspective, it is a rope (coordination). From the detained's perspective in normal times, it is also rope (protection). The pivotal moment is suspension. When Parliament suspends the writ, the directionality reverses. The detained become victims (d→1.0, maximum extraction), the executive becomes beneficiary (d→0.0, full benefit), and the courts are extracted from (lose function). The beneficiary/victim declarations reflect the suspension state, not the normal state, because the reading is specifically about suspension history. The analytical context risks computing d as if habeas is always like the normal state (d=0.5, symmetric), missing that the constraint's structure is precisely the capacity to flip into asymmetry on statutory authority. The engine's directionality computation must recognize that the constraint's defining feature is the transition between d states, not stability at one d value.
 *
 * MANDATROPHY ANALYSIS:
 *   The suspension_history_reading avoids mandatrophy by being clear about what it classifies: not habeas corpus in general, but habeas corpus during suspension windows, measured at the sustained crisis state (t=2-3). The constraint is tangled_rope during suspension specifically, not rope (which would be false — there is real extraction) and not snare (which is also partially true but misses the coordination function). The key to mandatrophy resolution is recognizing that the constraint changes classification across the interval: at t=0 (normal operation), habeas is rope. At t=1-2 (early-to-mid suspension), it transitions to tangled_rope. The suspension Acts do coordinate emergency response AND extract from the detained. Both are true. The mandatrophy dissolves when the perspectival gap is mapped explicitly — different agents legitimately see different types because they occupy different structural positions, and the constraint's defining feature is precisely this structural differentiation by crisis state.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suspension_counterfactual,
    'What is habeas corpus without suspension? Does the absence of a suspension mechanism fundamentally change the constraint''s classification?',
    'Constitutional comparison: jurisdictions without suspension authority (e.g., modern Canada, EU ECHR with no derogation clause for detention procedure). Examine whether those jurisdictions'' detention safeguards are structurally comparable or fundamentally different.',
    'If comparable: the suspension mechanism is contingent institutional choice (this reading is correct). If incomparable or incoherent: the suspension mechanism is inherent to the concept of habeas corpus under parliamentary sovereignty (the natural law perspective has merit).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suspension_counterfactual, empirical, 'Whether habeas corpus without suspension authority is structurally coherent').

omega_variable(
    suspension_window_asymmetry,
    'Are suspension windows (wartime, rebellion, emergency) genuinely temporary, or do they ratchet in duration and trigger threshold over time?',
    'Historical analysis of suspension Acts: duration, frequency, stated versus actual duration, and the evolution of the emergency threshold. Compare 1793-1801 (Napoleonic Wars, 8 years), 1914-1918 (WWI, 4 years), 1939-1945 (WWII, 6 years), 1972-present (Northern Ireland, 28+ years).',
    'If ratcheting: the constraint is functionally a gradual extraction mechanism disguised as temporary emergency (snare classification becomes more accurate). If genuinely cyclic: the constraint is a legitimate coordination mechanism with abuse risk (tangled_rope correct).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suspension_window_asymmetry, empirical, 'Whether suspension windows ratchet in duration and emergency threshold').

omega_variable(
    parliamentary_forbearance_mechanism,
    'What structural mechanism actually enforces parliamentary forbearance? Is Parliament''s refusal to suspend habeas a constitutional duty, a political choice, or merely a convention?',
    'Doctrinal analysis of parliamentary supremacy: can Parliament constitutionally suspend habeas indefinitely? What would prevent perpetual suspension? Is there a constitutional court or higher law that could invalidate a suspension Act? Historical moments when Parliament declined to suspend despite pressure (if any).',
    'If duty: the constraint is genuinely conditional (victim liberation possible if Parliament honors duty). If political choice: the constraint depends on contingent political will (victim liberation vulnerable to majoritarian erosion). If convention only: the constraint is permanently contingent and no safeguard truly exists (snare dominates).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(parliamentary_forbearance_mechanism, conceptual, 'Whether parliamentary forbearance is duty, political choice, or convention').

omega_variable(
    reading_foreclosure,
    'Does the suspension_history_reading logically foreclose the procedural_teeth_reading, or can both coexist?',
    'Doctrinal reconstruction: if the Suspension Act negates the writ entirely, can the procedural machinery (deadlines, penalties on gaolers) still function? If suspension preserves the procedural framework while merely preventing the writ''s issuance, do both readings describe the same constraint?',
    'If foreclosed: only the suspension reading captures the structural reality during crisis (one reading dominates). If coexistent: both readings are partial truths reflecting different moments in the constraint''s lifecycle (presheaf analysis required).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure, conceptual, 'Whether suspension_history_reading forecloses the procedural_teeth_reading').

omega_variable(
    extractiveness_measurement_ambiguity,
    'Is the extractiveness (0.58) measuring the constraint during suspension, during normal operation, or averaged across the full cycle?',
    'Temporal decomposition: separate measurements for habeas during non-suspension (ε ≈ 0.15, pure coordination) versus during suspension (ε ≈ 0.85, pure extraction). If the constraint changes classification between states, the suspension_history reading is specifically about the extraction window, not the full lifecycle.',
    'If during suspension: this reading focuses on the crisis state (accurate to reading intent). If averaged: the reading conflates two different constraints (violates ε-invariance). If normal operation: the reading misses the point (suspension history is irrelevant to non-suspension extractiveness).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extractiveness_measurement_ambiguity, empirical, 'Temporal measurement target for extractiveness in suspension_history_reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(habeas_corpus_act_1679__suspension_history_reading, 0, 3).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(habeas_susp_theater_normal, habeas_corpus_act_1679__suspension_history_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(habeas_susp_theater_suspension, habeas_corpus_act_1679__suspension_history_reading, theater_ratio, 3, 0.35).

% Extraction over time
narrative_ontology:measurement(habeas_susp_extract_normal_baseline, habeas_corpus_act_1679__suspension_history_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(habeas_susp_extract_early_suspension, habeas_corpus_act_1679__suspension_history_reading, base_extractiveness, 1, 0.42).
narrative_ontology:measurement(habeas_susp_extract_crisis_peak, habeas_corpus_act_1679__suspension_history_reading, base_extractiveness, 2, 0.68).
narrative_ontology:measurement(habeas_susp_extract_sustained_suspension, habeas_corpus_act_1679__suspension_history_reading, base_extractiveness, 3, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(habeas_susp_supp_normal, habeas_corpus_act_1679__suspension_history_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(habeas_susp_supp_early, habeas_corpus_act_1679__suspension_history_reading, suppression_requirement, 1, 0.45).
narrative_ontology:measurement(habeas_susp_supp_peak, habeas_corpus_act_1679__suspension_history_reading, suppression_requirement, 2, 0.82).
narrative_ontology:measurement(habeas_susp_supp_sustained, habeas_corpus_act_1679__suspension_history_reading, suppression_requirement, 3, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(habeas_corpus_act_1679__suspension_history_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(habeas_corpus_act_1679__suspension_history_reading, habeas_corpus_act_1679__modern_detention_tests_reading).
narrative_ontology:affects_constraint(habeas_corpus_act_1679__suspension_history_reading, habeas_corpus_act_1679__procedural_teeth_reading).

% DUAL FORMULATION NOTE:
% The habeas_corpus_act_1679 kernel has three readings with different ε values and structural focuses. The suspension_history_reading (ε=0.58, tangled_rope) emphasizes the crisis-state extraction mechanism and parliamentary authorization. The procedural_teeth_reading (estimated ε=0.12, rope) emphasizes the 1679 Act's procedural innovations in normal operation. The modern_detention_tests_reading (estimated ε=0.08, rope) emphasizes the writ's contemporary application in normal-state detention law. All three readings are live interpretations of the same kernel text; they are not alternatives but complementary perspectives on different aspects of the Act's structural role.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(habeas_corpus_act_1679__suspension_history_reading, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
