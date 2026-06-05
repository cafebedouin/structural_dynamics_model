% ============================================================================
% CONSTRAINT STORY: legal_codification_twelve_tables__foundation_myth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legal_codification_twelve_tables__foundation_myth_reading, []).

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
 *   constraint_id: legal_codification_twelve_tables__foundation_myth_reading
 *   human_readable: The Twelve Tables as Foundation Myth (Canonical Ancestor Cult Reading)
 *   domain: legal/doctrinal/jurisprudence
 *
 * SUMMARY:
 *   The Twelve Tables (mid-5th century BCE) were Rome's first written legal
 *   code and remained its mythic foundation for centuries after their
 *   concrete rules became obsolete. This constraint analyzes ONE reading of
 *   the contested kernel 'the Twelve Tables as legal founding': the
 *   foundation_myth_reading treats the Tables as ancestor cult — a
 *   half-remembered text maintained through schoolboy recitation and
 *   canonical deference long after its substantive content ceased to govern
 *   actual law. The sibling readings (harsh_content_reading and
 *   publication_victory_reading) instantiate different structural functions:
 *   codification of class oppression vs. the victory of published law over
 *   pontiff monopoly. This reading focuses on the mythic function: how Rome
 *   sustained legal authority by treating the Tables as immutable foundation
 *   even as interpretation absorbed all practical changes. The constraint
 *   exhibits the full lifecycle of a degrading myth: initially (t0) the
 *   Tables functioned as working law with moderate extractiveness (0.15); by
 *   the classical period (t50) they had become partially mythologized with
 *   rising theater (0.55); by late empire (t100) the Tables were almost
 *   purely ceremonial, maintained through educational inertia and ancestor
 *   worship, with theater ratio approaching pure performance (0.81) and
 *   suppression of doubt about their foundational status reaching
 *   institutional enforcement levels (0.62).
 *
 * KEY AGENTS:
 *   - Historical Accuracy About Early Law: Primary victim (powerless/trapped) — cannot revise founding narrative without appearing to attack legal legitimacy itself
 *   - Critical Scholars and Jurists: Secondary victim (moderate/constrained) — face career cost for questioning the Tables' founding status; can publish critique but at substantial institutional risk
 *   - The Legal Tradition's Institutional Continuity: Primary beneficiary (institutional/arbitrage) — the Tables-as-ancestor-cult solves the succession problem: how does law maintain legitimacy across generations when specific rules become obsolete?
 *   - Patrician Legal Authority and Pontifical Monopoly: Secondary beneficiary (institutional/arbitrage) — control over interpretation of the Tables preserved the class monopoly on legal knowledge
 *   - Later Jurists and Legislators: Mixed position (powerful/mobile) — benefit from having canonical founding text to cite and reinterpret (coordination function) while constrained by its mythic status (cannot fundamentally revise)
 *   - Schoolboys and Legal Education System: Performative vector (institutional/arbitrage) — maintain the recitation ritual centuries after rules are dead; the Tables persist through educational theater, not functional law
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the constraint as inherent to how legal codes establish authority, missing that Rome made a specific choice to treat the Tables as immutable rather than revisable
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legal_codification_twelve_tables__foundation_myth_reading, 0.38).
domain_priors:suppression_score(legal_codification_twelve_tables__foundation_myth_reading, 0.62).
domain_priors:theater_ratio(legal_codification_twelve_tables__foundation_myth_reading, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legal_codification_twelve_tables__foundation_myth_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(legal_codification_twelve_tables__foundation_myth_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(legal_codification_twelve_tables__foundation_myth_reading, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legal_codification_twelve_tables__foundation_myth_reading, tangled_rope).
narrative_ontology:human_readable(legal_codification_twelve_tables__foundation_myth_reading, "The Twelve Tables as Foundation Myth (Canonical Ancestor Cult Reading)").
narrative_ontology:topic_domain(legal_codification_twelve_tables__foundation_myth_reading, "legal/doctrinal/jurisprudence").

domain_priors:requires_active_enforcement(legal_codification_twelve_tables__foundation_myth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legal_codification_twelve_tables__foundation_myth_reading, '84d7f556-f32c-4518-a1fb-b7802edc8231').
narrative_ontology:cs_kernel_codification('84d7f556-f32c-4518-a1fb-b7802edc8231', fixed_text).
narrative_ontology:cs_authority_grounding('84d7f556-f32c-4518-a1fb-b7802edc8231', lineage).
narrative_ontology:cs_interpretation_layer_present('84d7f556-f32c-4518-a1fb-b7802edc8231').
narrative_ontology:cs_reading_relation('84d7f556-f32c-4518-a1fb-b7802edc8231', legal_codification_twelve_tables__harsh_content_reading, coexists_with).
narrative_ontology:cs_reading_relation('84d7f556-f32c-4518-a1fb-b7802edc8231', legal_codification_twelve_tables__publication_victory_reading, coexists_with).
narrative_ontology:cs_axiom('84d7f556-f32c-4518-a1fb-b7802edc8231', foundational, founding_code_immutable_by_definition).
narrative_ontology:cs_axiom_status(founding_code_immutable_by_definition, holdable).
narrative_ontology:cs_axiom_grounding('84d7f556-f32c-4518-a1fb-b7802edc8231', founding_code_immutable_by_definition, instrumental).
narrative_ontology:cs_axiom('84d7f556-f32c-4518-a1fb-b7802edc8231', foundational, canonical_status_suppresses_substantive_doubt).
narrative_ontology:cs_axiom_status(canonical_status_suppresses_substantive_doubt, holdable).
narrative_ontology:cs_axiom_grounding('84d7f556-f32c-4518-a1fb-b7802edc8231', canonical_status_suppresses_substantive_doubt, empirically_contingent).
narrative_ontology:cs_reference_frame('84d7f556-f32c-4518-a1fb-b7802edc8231', immutable_founding_code).
narrative_ontology:cs_drift_state('84d7f556-f32c-4518-a1fb-b7802edc8231', late_empire_period, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('84d7f556-f32c-4518-a1fb-b7802edc8231', '').
narrative_ontology:cs_kernel_id(legal_codification_twelve_tables__foundation_myth_reading, legal_codification_twelve_tables).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legal_codification_twelve_tables__foundation_myth_reading, legal_tradition_continuity).
narrative_ontology:constraint_victim(legal_codification_twelve_tables__foundation_myth_reading, historical_accuracy_about_archaic_law).
narrative_ontology:constraint_victim(legal_codification_twelve_tables__foundation_myth_reading, critical_inquiry_into_founding_claims).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HISTORICAL ACCURACY (SNARE) — Bears the cost of canonical mystification. Later historians cannot revise the founding narrative without appearing to attack Rome's legal legitimacy. The victim here is the epistemic commons: doubt about what the Tables actually contained and enforced is suppressed by their canonical status. No exit from this entrapment — the constraint exists to prevent exactly this kind of revision.
constraint_indexing:constraint_classification(legal_codification_twelve_tables__foundation_myth_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: CRITICAL SCHOLARS (SNARE) — Face career and intellectual cost for questioning the Tables' founding status. A Roman jurist who suggested the Tables were half-remembered performance pieces risked appearing to undermine law's authority. The suppression is enforced through social sanction and institutional pressure — scholars must treat the Tables as foundational or face delegitimation. Exit is possible (publish the critique anyway) but carries substantial cost.
constraint_indexing:constraint_classification(legal_codification_twelve_tables__foundation_myth_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: LEGAL TRADITION (ROPE) — Primary beneficiary. The Tables function as ancestor cult precisely to solve a coordination problem: How does law maintain legitimacy across generations when specific rules become obsolete? Answer: by treating the founding code as sacred text whose interpretation can evolve while the Tables themselves remain immutable. This is pure coordination — the tradition benefits from having a stable mythic foundation that can absorb centuries of reinterpretation. The Tables-as-ancestor-cult solves the succession problem for legal authority.
constraint_indexing:constraint_classification(legal_codification_twelve_tables__foundation_myth_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: PATRICIAN LEGAL MONOPOLY (ROPE) — Secondary beneficiary. In the pre-publication era, control over knowledge of law was control. The Tables, even as half-remembered ancestor worship, remained in the hands of the pontiffs and patrician jurists who 'knew' what they meant. Treating the Tables as sacred text that requires expert interpretation preserved the monopoly on legal knowledge. The constraint appears as coordination (maintaining law's authority) but functions to maintain class control.
constraint_indexing:constraint_classification(legal_codification_twelve_tables__foundation_myth_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 5: LATER JURISTS (TANGLED ROPE) — Benefit from having a canonical founding text to cite and reinterpret (coordination function) while bearing the cost of being constrained by its mythic status. A later jurist cannot simply discard the Tables and start over; they must work within the founding narrative. This is genuine mixed coordination-extraction: the constraint enables legal continuity while constraining legal innovation. The jurist experiences this as both tool (the Tables provide authority) and cage (they cannot be substantially revised).
constraint_indexing:constraint_classification(legal_codification_twelve_tables__foundation_myth_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 6: SCHOOLBOY RECITATION (PITON) — The Tables persist as pedagogical theater: centuries after their rules were dead, schoolboys still recited them as part of legal education. This is pure performance — the content has become decoupled from function. The recitation maintains the myth of foundational continuity without requiring that anyone actually believes the Tables' archaic rules work. Theater ratio is extremely high (>0.85): the content is performed but not enforced. The Tables have become a degraded institution maintained through educational inertia and ceremonial importance.
constraint_indexing:constraint_classification(legal_codification_twelve_tables__foundation_myth_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, this appears to be a natural property of how written law establishes legitimacy: once codified, a founding text becomes immutable by definition. Revision would undermine the entire authority structure. From this perspective, the Tables' mythic status is not a specific Roman choice but an inherent feature of how legal codes function — they must be treated as foundational to ground current rules. This perspective risks being a false summit, naturalizing a contingent institutional choice as an inevitable feature of law itself.
constraint_indexing:constraint_classification(legal_codification_twelve_tables__foundation_myth_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legal_codification_twelve_tables__foundation_myth_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(legal_codification_twelve_tables__foundation_myth_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(legal_codification_twelve_tables__foundation_myth_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(legal_codification_twelve_tables__foundation_myth_reading, TR),
    TR >= 0.70.

:- end_tests(legal_codification_twelve_tables__foundation_myth_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): The core extraction mechanism is the suppression of doubt about the founding narrative. The Tables benefit the legal tradition by providing a stable mythic anchor that can absorb centuries of reinterpretation without admitting revision. But extractiveness is moderate (not high) because the coordination function is genuine — Rome does solve the succession problem through ancestor cult. The constraint is tangled rope: it coordinates legal continuity while extracting the cost of historical accuracy and critical inquiry. Suppression (0.62): Institutional enforcement of canonical status. By the classical period, questioning the Tables' foundational role carried professional risk. Jurists who suggested the Tables were half-remembered performance pieces risked delegitimization. The suppression is enforced through social sanction (loss of authority), not legal punishment, but it is real and structural. Theater ratio (0.81): Rising dramatically over the interval. At t0 (early period), the Tables were working law with moderate performance content. By t100 (late empire), they were almost purely ceremonial — recited in schools, cited in legal argument, but not actually enforced as binding rules. The trajectory shows the constraint degrading from functional law to pure myth.
 *
 * PERSPECTIVAL GAP:
 *   The foundation_myth reading produces a wide perspectival gap. For historical accuracy (powerless/trapped), the constraint is a snare — absolute suppression with no exit. For critical scholars (moderate/constrained), it is also a snare, but with a narrower band of suppression (high cost to exit, but exit is possible). For the legal tradition (institutional/arbitrage), it is rope — pure coordination benefit with no experienced extraction. For the schoolboy recitation system, it is piton — the content is almost entirely performative. For later jurists, it is tangled rope — both benefit from the canonical anchor and suffer from being unable to revise it. The analytical observer risks seeing mountain (natural law of legal authority) but the structural data reveals this as a false summit: Rome could have treated the Tables as superseded rather than foundational, as later constitutionalism would do. The Tables' immutability is a choice, not a necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from their structural position relative to the constraint. Historical accuracy has no choice but to bear the constraint — it is trapped (d ≈ 0.95, f(d) ≈ 1.42). Critical scholars can exit by publishing the critique, but at career cost — they are constrained (d ≈ 0.75, f(d) ≈ 1.15). The legal tradition benefits from the constraint and has multiple exit options (could revise the founding narrative, could treat the Tables as dead law) but chooses not to — they are institutional beneficiaries with arbitrage options (d ≈ 0.05, f(d) ≈ -0.12, yielding negative effective extraction). Later jurists experience both benefit and constraint — they cite the Tables as authority but cannot fundamentally revise them — yielding mixed directionality (d ≈ 0.50, f(d) ≈ 0.65). The piton perspective sees the constraint as mostly performative — theater dominates, but the institutional actor still benefits from maintaining the myth, so directionality remains favorable (d ≈ 0.10). The analytical observer at civilizational scale risks seeing the constraint as a natural law of legal systems, missing that Rome's choice to treat the Tables as immutable (rather than revisable, as later constitutionalism would do) is contingent.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves the mandatrophy by showing that the Tables simultaneously coordinate (they solve the succession problem for legal authority) and extract (they suppress doubt about the founding narrative and constrain legal innovation). The classification as tangled_rope is stable because both functions are real and structural. The false summit risk comes from the mountain perspective — the analytical observer who naturalizes the constraint as inherent to how legal codes work. The foundation_myth reading deliberately foregrounds this risk: by treating the Tables as ancestor cult rather than working law, it shows how mythology can suppress alternative possibilities (the Tables could be revised; law could be treated as continuously evolving rather than rooted in an immutable foundation). The mandatrophy is resolved at the level of reading strategy: the foundation_myth reading acknowledges that treating the Tables as sacred foundation is a contingent institutional choice that benefits the legal tradition while suppressing historical accuracy and critical inquiry.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    myth_versus_working_law_boundary,
    'At what point does a legal code transition from working law to canonical ancestor cult? What distinguishes codification (living instrument) from mythologization (half-remembered performance)?',
    'Historical analysis of enforcement patterns: are the Tables'' rules actually applied in later litigation, or are they cited ceremonially? Textual analysis: do later jurists claim the Tables are obsolete or claim they require interpretation to remain relevant?',
    'If Tables are treated as living law: reclassify as rope (pure coordination mechanism for legal succession). If treated as mythic ancestor: confirm tangled_rope (coordination benefit + extraction via suppression of doubt). If Tables are purely performative: reclassify as piton.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(myth_versus_working_law_boundary, empirical, 'Boundary between functional codification and mythic ancestor cult status').

omega_variable(
    canonical_immutability_necessity,
    'Is the Tables'' immutability a necessary feature of how founding texts establish legal legitimacy, or a specific choice Rome made? Could a legal tradition maintain authority while openly revising its founding code?',
    'Comparative jurisprudence: examine legal traditions that treat founding codes as revisable (constitutionalism, common law) and assess whether they experience legitimacy deficits. Philosophical analysis: what distinguishes legitimate interpretation from illegitimate revision?',
    'If immutability is necessary: mountain classification is correct. If revisability is possible: the mountain is a false summit — the constraint naturalizes a contingent choice. The Tables could have been treated as outdated rather than foundational.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(canonical_immutability_necessity, conceptual, 'Whether canonical immutability is necessary or contingent in legal systems').

omega_variable(
    suppression_mechanism_internalization,
    'Is the suppression of doubt about the Tables'' founding status enforced externally (institutional pressure, career risk) or internalized (jurists genuinely believe the Tables are sacred and dare not question them)?',
    'Textual analysis of private correspondence and unpublished work by Roman jurists. Institutional history: do jurists who question the Tables face demonstrable sanctions, or do they self-censor? Post-publication evidence: when the Tables were finally written down and widely distributed, does textual criticism increase?',
    'If externally enforced: suppression value (0.62) is correct and based on structural barriers. If internalized: the constraint functions more like identity_locked than trapped — jurists cannot exit because their professional identity is fused with the Tables'' sacredness, not because of external punishment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is externally enforced or internalized').

omega_variable(
    foundation_myth_versus_sibling_readings,
    'Which reading — foundation myth, harsh codification of existing order, or publication victory — best explains why the Tables mattered to Rome? Or do all three readings describe different structural functions of the same institution?',
    'This is the kernel contest itself. Historical evidence can support multiple readings: the harsh content supports harsh_content_reading, the publication event supports publication_victory_reading, the mythic treatment supports foundation_myth_reading. The question is which framing best predicts how Romans actually used the Tables.',
    'If foundation_myth reading dominates: extractiveness derives from suppression of doubt about founding narrative. If harsh_content reading dominates: extractiveness derives from codification of class oppression. If publication_victory reading dominates: extractiveness is minimal because the Tables solved an epistemic access problem for the plebs. The readings may coexist, but their relative salience changes the classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(foundation_myth_versus_sibling_readings, conceptual, 'Kernel contest: which reading best explains the Tables'' historical function').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legal_codification_twelve_tables__foundation_myth_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tables_myth_theater_t0, legal_codification_twelve_tables__foundation_myth_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(tables_myth_theater_t50, legal_codification_twelve_tables__foundation_myth_reading, theater_ratio, 50, 0.55).
narrative_ontology:measurement(tables_myth_theater_t100, legal_codification_twelve_tables__foundation_myth_reading, theater_ratio, 100, 0.81).

% Extraction over time
narrative_ontology:measurement(tables_myth_extract_t0, legal_codification_twelve_tables__foundation_myth_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(tables_myth_extract_t50, legal_codification_twelve_tables__foundation_myth_reading, base_extractiveness, 50, 0.28).
narrative_ontology:measurement(tables_myth_extract_t100, legal_codification_twelve_tables__foundation_myth_reading, base_extractiveness, 100, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(tables_myth_suppress_t0, legal_codification_twelve_tables__foundation_myth_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(tables_myth_suppress_t50, legal_codification_twelve_tables__foundation_myth_reading, suppression_requirement, 50, 0.52).
narrative_ontology:measurement(tables_myth_suppress_t100, legal_codification_twelve_tables__foundation_myth_reading, suppression_requirement, 100, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legal_codification_twelve_tables__foundation_myth_reading, identity_coordination).
narrative_ontology:affects_constraint(legal_codification_twelve_tables__foundation_myth_reading, legal_codification_twelve_tables__harsh_content_reading).
narrative_ontology:affects_constraint(legal_codification_twelve_tables__foundation_myth_reading, legal_codification_twelve_tables__publication_victory_reading).

% DUAL FORMULATION NOTE:
% The kernel 'legal_codification_twelve_tables' decomposes into three structurally distinct constraints, each a reading of the same historical event but with different ε values and different structural mechanisms. The foundation_myth_reading (this constraint) focuses on mythologization and suppression of doubt about the founding narrative. The harsh_content_reading focuses on codification of class oppression. The publication_victory_reading focuses on epistemic democratization. Each reading is a complete constraint story with its own classification, perspectives, and measurements. They are linked as a constraint family because they share a kernel but instantiate different structural functions. The foundation_myth_reading is downstream of the historical event (the Tables were created) but upstream of the institutional persistence mechanisms (schoolboy recitation, canonical deference) that maintain the myth centuries after the content becomes obsolete.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
