% ============================================================================
% CONSTRAINT STORY: magistracies_and_collegiality__term_limit_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magistracies_and_collegiality__term_limit_reading, []).

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
 *   constraint_id: magistracies_and_collegiality__term_limit_reading
 *   human_readable: Magistracies and Collegiality — Term Limit Reading (One Year and Out)
 *   domain: legal/doctrinal/constitutional
 *
 * SUMMARY:
 *   This constraint models the term-limit reading of the magistracies and
 *   collegiality kernel: one year and out, with iteration restricted, so that
 *   power expired before it could consolidate. The reading instantiates the
 *   claim that the Roman magistracy system's core mechanism for preventing
 *   tyranny is the calendar itself — the consular year is a hard stop on
 *   authority, and rotation is enforced through term limits rather than
 *   through colleague veto or the ladder structure of the cursus honorum.
 *   This constraint is ONE READING of a contested kernel
 *   (magistracies_and_collegiality). Other readings invoke collegial veto as
 *   the primary check on power, or the cursus honorum as the primary
 *   organizing principle. This term-limit reading claims the calendar is the
 *   prime mechanism. The structural delta shows extractiveness bounded by the
 *   consular year (magistrates cannot consolidate authority beyond their
 *   term), suppression of incumbency accumulation (re-election is
 *   restricted), and the beneficiary class as the elite rotation cohort
 *   (those awaiting their turn to hold office). The victim is policy
 *   continuity: annual turnover disrupts institutional memory and the
 *   execution of long-term projects. The constraint exhibits genuine
 *   coordination (ensuring rotation, preventing any one magistrate from
 *   perpetuating power) alongside asymmetric costs (to policy continuity and
 *   subordinate officials who must retrain annually). The theater ratio
 *   reflects increasing doctrinal invocation of the term limit as a natural
 *   principle (piton perspective) even as the actual mechanism weakens in the
 *   face of other power asymmetries (wealth, client networks, the ability to
 *   seek re-election after interval).
 *
 * KEY AGENTS:
 *   - Elite Rotation Cohort (organized/mobile): Primary beneficiary — assured that their turn will come to hold office; prevents any single magistrate from perpetuating power indefinitely
 *   - Policy Continuity / Institutional Memory (powerless/trapped): Primary victim — suffers from annual magistrate turnover; institutional projects interrupted; knowledge loss as magistrates exit
 *   - Sitting Magistrate (moderate/constrained): Secondary beneficiary and victim — holds real power during the year but cannot consolidate or continue authority; experiences both the benefit of having had a turn and the extraction of being forced to yield
 *   - The Senate (institutional/constrained): Institutional actor managing succession — benefits from orderly rotation and tyranny prevention; bears cost of interrupted policy and retraining burden
 *   - Subordinate Officials (powerless/trapped): Severe victim — must re-establish working relationships with new magistrate annually; institutional memory is destroyed by turnover
 *   - Historical-Legal Doctrine (institutional/arbitrage): Maintains naturalization of term limits as a foundational principle; invokes the calendar as a check on tyranny through performative elaboration
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magistracies_and_collegiality__term_limit_reading, 0.32).
domain_priors:suppression_score(magistracies_and_collegiality__term_limit_reading, 0.48).
domain_priors:theater_ratio(magistracies_and_collegiality__term_limit_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magistracies_and_collegiality__term_limit_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(magistracies_and_collegiality__term_limit_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(magistracies_and_collegiality__term_limit_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magistracies_and_collegiality__term_limit_reading, tangled_rope).
narrative_ontology:human_readable(magistracies_and_collegiality__term_limit_reading, "Magistracies and Collegiality — Term Limit Reading (One Year and Out)").
narrative_ontology:topic_domain(magistracies_and_collegiality__term_limit_reading, "legal/doctrinal/constitutional").

domain_priors:requires_active_enforcement(magistracies_and_collegiality__term_limit_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magistracies_and_collegiality__term_limit_reading, '991d24ef-a25b-49ed-b8af-c811cbe6dd6e').
narrative_ontology:cs_kernel_codification('991d24ef-a25b-49ed-b8af-c811cbe6dd6e', formalized).
narrative_ontology:cs_authority_grounding('991d24ef-a25b-49ed-b8af-c811cbe6dd6e', lineage).
narrative_ontology:cs_interpretation_layer_present('991d24ef-a25b-49ed-b8af-c811cbe6dd6e').
narrative_ontology:cs_reading_relation('991d24ef-a25b-49ed-b8af-c811cbe6dd6e', magistracies_and_collegiality__collegial_veto_reading, coexists_with).
narrative_ontology:cs_reading_relation('991d24ef-a25b-49ed-b8af-c811cbe6dd6e', magistracies_and_collegiality__cursus_honorum_reading, coexists_with).
narrative_ontology:cs_axiom('991d24ef-a25b-49ed-b8af-c811cbe6dd6e', foundational, calendar_mechanical_tyranny_prevention).
narrative_ontology:cs_axiom_status(calendar_mechanical_tyranny_prevention, holdable).
narrative_ontology:cs_axiom_grounding('991d24ef-a25b-49ed-b8af-c811cbe6dd6e', calendar_mechanical_tyranny_prevention, instrumental).
narrative_ontology:cs_axiom('991d24ef-a25b-49ed-b8af-c811cbe6dd6e', foundational, power_expires_by_temporal_necessity).
narrative_ontology:cs_axiom_status(power_expires_by_temporal_necessity, overridden).
narrative_ontology:cs_axiom_grounding('991d24ef-a25b-49ed-b8af-c811cbe6dd6e', power_expires_by_temporal_necessity, empirically_contingent).
narrative_ontology:cs_reference_frame('991d24ef-a25b-49ed-b8af-c811cbe6dd6e', annual_calendar_supremacy).
narrative_ontology:cs_drift_state('991d24ef-a25b-49ed-b8af-c811cbe6dd6e', late_republic_crisis, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('991d24ef-a25b-49ed-b8af-c811cbe6dd6e', '').
narrative_ontology:cs_kernel_id(magistracies_and_collegiality__term_limit_reading, magistracies_and_collegiality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magistracies_and_collegiality__term_limit_reading, elite_rotation_cohort).
narrative_ontology:constraint_victim(magistracies_and_collegiality__term_limit_reading, policy_continuity).
narrative_ontology:constraint_victim(magistracies_and_collegiality__term_limit_reading, executive_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBORDINATE MAGISTRATE / LICTORS (SNARE) — Faces the full extractive force of term limits without the compensatory rotation benefit. Annual turnover destroys institutional memory and continuity of administration. No exit option: enforced by the calendar itself. Maximum experienced extraction.
constraint_indexing:constraint_classification(magistracies_and_collegiality__term_limit_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: SITTING MAGISTRATE (TANGLED ROPE) — Holds real power during the consular year but cannot consolidate or renew it; must yield to successor. Constrained by the calendar and by the need to prepare for the next magistrate's assumption. Experiences genuine coordination function (orderly succession, rotation of opportunities) alongside extraction (inability to continue projects, consolidate authority, or accumulate precedent). Mixed beneficiary-victim: benefits from having had a turn, loses the ability to extend it.
constraint_indexing:constraint_classification(magistracies_and_collegiality__term_limit_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ELITE ROTATION COHORT (ROPE) — The patrician or propertied class eligible for rotation. Experiences the term limit primarily as coordination: it guarantees their turn will come, prevents any single magistrate from perpetuating power, and ensures the rotation cycle continues. Mobile exit option reflects their agency in navigating the ladder (cursus honorum). Low effective extraction from the elite's perspective because the term limit distributes opportunity rather than concentrating it.
constraint_indexing:constraint_classification(magistracies_and_collegiality__term_limit_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 4: THE SENATE (TANGLED ROPE) — Institutional perspective on term limits. The Senate benefits from the orderly succession mechanism and the prevention of personal tyranny; it also bears the cost of interrupted policy and the need to retrain new magistrates annually. Constrained exit reflects institutional dependency on the magistracies themselves. Requires active enforcement: the calendar must be observed, succession must be enforced, the sitting magistrate must yield. Genuine coordination (succession order) alongside asymmetric cost (policy disruption).
constraint_indexing:constraint_classification(magistracies_and_collegiality__term_limit_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: HISTORICAL-LEGAL DOCTRINE (PITON) — The civilizational-level perspective on term limits as an immutable principle of republican governance. The doctrine invokes the term limit as a foundational check on tyranny, maintaining it through interpretive elaboration and doctrinal repetition. From this perspective, the calendar is seen as performing a natural-law function (preventing power concentration inherent to human nature), but the performance is increasingly divorced from actual governance capacity. Theater ratio reflects that the doctrinal invocation of the term limit persists even as its functional role (preventing tyranny) has eroded in the face of other power asymmetries (wealth, client networks, the possibility of repeated election after interval). Arbitrage exit reflects that the doctrine itself is maintained by those who benefit from its performative force rather than its actual constraint.
constraint_indexing:constraint_classification(magistracies_and_collegiality__term_limit_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: REFORM MOVEMENT (SCAFFOLD) — Temporary coalitions demanding stricter enforcement of term limits and rotation (or conversely, demanding relaxation for specific magistrates in crisis). These moments of intensified term-limit rhetoric represent a scaffold: the coalition sees the constraint as needing temporary reinforcement or temporary removal, with an expectation that normal rotation will resume. Low chi reflects that the reform is not primarily extractive; it is addressing a coordination failure. Sunset clause is implicit: once the crisis passes, normal rotation resumes.
constraint_indexing:constraint_classification(magistracies_and_collegiality__term_limit_reading, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — Civilizational and universal scope. The term limit appears as an immutable structural feature of republican governance: the calendar is fixed, power expires by logical necessity, consolidation is impossible. From this view, the constraint is a mountain of political mathematics. However, the structural data reveals this as a false summit: the term limit is maintained by the elite's commitment to rotation and the enforcement mechanisms (Senate votes, re-eligibility restrictions), both of which are doctrinal, not necessary. The constraint is contingent on the reading that term limits bind all magistrates equally and that the rotation cohort enforces compliance.
constraint_indexing:constraint_classification(magistracies_and_collegiality__term_limit_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magistracies_and_collegiality__term_limit_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(magistracies_and_collegiality__term_limit_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(magistracies_and_collegiality__term_limit_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(magistracies_and_collegiality__term_limit_reading, TR),
    TR >= 0.70.

:- end_tests(magistracies_and_collegiality__term_limit_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.32): Moderate. The term limit extracts the benefit of authority consolidation from sitting magistrates and subordinate officials (who cannot build continuous policy or career progression). However, the constraint also distributes opportunity to the rotation cohort, which reduces the overall extractiveness compared to a regime with hereditary or perpetual tenure. The measurement trajectory (0.18→0.25→0.32) reflects increasing extractiveness as the elite begins to realize that iteration restrictions prevent them from building family dynasties or personal power bases — the extraction is the forced yield of authority. Suppression (0.48): Moderate. The term limit creates real barriers to incumbency accumulation and re-election; however, suppression is not total because the rotation ensures that every magistrate will have their turn. The trajectory (0.35→0.42→0.48) reflects increasing suppression as the magistracy system matures and enforcement becomes stricter. Theater ratio (0.35): Low-to-moderate. The term limit itself is not primarily performative — the calendar is functionally enforced, and magistrates do actually yield power after one year. However, the doctrinal layer (the invocation of the term limit as a natural principle preventing tyranny) introduces performative content, especially as the mechanism weakens in the face of other power asymmetries. The trajectory shows theater increasing slightly (0.22→0.28→0.35) as doctrinal elaboration accumulates while functional enforcement remains intact.
 *
 * PERSPECTIVAL GAP:
 *   The term-limit reading produces a significant perspectival gap between the elite beneficiary (rope: coordination ensures my turn) and the powerless victim (snare: I suffer the disruption of annual turnover with no compensation). The sitting magistrate occupies the middle ground (tangled_rope: I benefit from having held power, but I bear the extraction of being forced to yield). The Senate sees a coordination mechanism (tangled_rope: orderly succession is essential, though costly). The historical doctrine naturalizes the term limit as an immutable principle (piton and mountain: the calendar prevents tyranny by necessity), but the structural data reveals this as performance that masks the contingency of enforcement. The strongest gap emerges between the rotation cohort's rope perspective (low extraction, high coordination benefit) and the subordinate officials' snare perspective (high extraction, no coordination benefit). This gap reveals the unequal distribution of the constraint's burden: the rotation cohort is protected by the term limit, while those without eligibility for rotation (or with limited frequency of rotation) bear disproportionate costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective derives its directionality from the agent's structural position relative to the calendar constraint. The elite rotation cohort (organized/mobile/arbitrage) benefits from the constraint because it ensures their turn and prevents perpetual incumbency — d is low (~0.20), producing negative chi. The sitting magistrate (moderate/constrained) experiences moderate extraction because they must yield authority — d is elevated (~0.55). Policy continuity (powerless/trapped) bears maximum extraction from the disruption — d is high (~0.90). The Senate as institution (institutional/constrained) experiences mixed directionality because it both benefits (orderly succession) and bears costs (policy disruption) — d is moderate (~0.50). The derivation accounts for the fact that the constraint distributes opportunity asymmetrically: some agents (eligible for rotation) have low d, while others (subordinate officials, policy continuity) have high d. The false summit accusation targets the analytical observer's mountain perspective: from a universal/civilizational scope, the calendar appears to be an immutable law of republican governance, but the structural data reveals this as naturalization of a contingent doctrinal choice.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves mandatrophy by showing that term limits function primarily as a coordination mechanism (ensuring rotation and preventing perpetual incumbency) alongside asymmetric extraction (policy disruption, forced yield of authority). The constraint is neither pure coordination (rope) nor pure extraction (snare), but a hybrid (tangled_rope) with different experiences depending on the observer's position. The sitting magistrate's perspective is diagnostic: they simultaneously benefit from the rotation mechanism (they got their turn) and suffer from the extraction (they cannot continue). The false summit accusation (mountain perspective) reveals that the naturalization of the calendar as an immutable law serves the interests of those who benefit from rotation — the doctrine performs a legitimation function that masks the contingency of enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    re_eligibility_scope_ambiguity,
    'Does iteration restriction mean absolute prohibition on re-election, or merely prohibition on immediate re-election? Do intermediate terms allow return to office?',
    'Textual analysis of constitutional language (if formalized) and historical practice of magistrate succession. Determine whether magistrates reappear after intervals (suggesting mobile exit) or never after initial year (suggesting stronger suppression).',
    'If absolute prohibition: suppression rises to 0.65, and the victim class (policy continuity) is severely constrained. Classification may shift to Snare. If intermediate re-eligibility allowed: suppression falls to 0.35, elite benefits more clearly, and classification stabilizes as Tangled Rope. The beneficiary group changes from ''rotation cohort'' to ''stratified rotation cohort with re-eligible veterans.''',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(re_eligibility_scope_ambiguity, empirical, 'Re-eligibility scope: absolute ban vs interval-based ban').

omega_variable(
    enforcement_mechanism_authority,
    'Who enforces the calendar? Is enforcement automatic (magistrate''s term ends by law), or does the Senate/Assembly actively enforce it against magistrates seeking extended tenure?',
    'Historical records of attempts to extend terms; analysis of constitutional mechanisms that mandate succession; examination of whether term limits have ever been violated and with what consequences.',
    'If automatic/legal: suppression is structural and immutable; extractiveness may be lower (inevitable decay of power). If Senate-enforced: suppression depends on Senate resolve; extractiveness may be higher (requires active suppression of re-election bids). This affects whether the constraint is read as a mountain (inevitable) or as a doctrinal choice (contingent on enforcement coalition).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_mechanism_authority, empirical, 'Enforcement mechanism: automatic legal constraint vs active Senate enforcement').

omega_variable(
    policy_continuity_alternative_mechanisms,
    'Do alternative mechanisms (advisory councils, law codes, magistrate collegiality veto) compensate for the policy disruption created by annual turnover, reducing the actual cost borne by the victim class?',
    'Comparative analysis of policy implementation outcomes before and after term-limit institution; study of coordination mechanisms (councils, written law, veto structure) that allow policy continuation despite magistrate turnover.',
    'If alternative mechanisms are effective: the victim class ''policy continuity'' is less severely harmed; extractiveness may fall to 0.20 (turning this into a Rope reading). If mechanisms are weak: extractiveness remains high (0.32+) and the snare perspective''s assessment of policy disruption is validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_continuity_alternative_mechanisms, empirical, 'Effectiveness of policy-continuity mechanisms to offset term-limit disruption').

omega_variable(
    kernel_reading_contest_natural_law_claim,
    'Is the term limit''s naturalization (piton and mountain perspectives) a genuine doctrinal claim or a cover story for the elite''s rotation interests? Does the reading foreclose the collegial_veto_reading, or coexist with it?',
    'Textual and historical analysis: do doctrinal sources justify term limits as preventing tyranny through calendar mechanics, or do they justify them through rotation-of-veto and colleague restraint? Can both justifications coexist in the same tradition?',
    'If term limits are naturalized as a primary principle: this reading forecloses the collegial_veto_reading (calendar is the core check, collegiality is secondary). If both justifications coexist: the readings coexist_with each other, and the kernel contest is unresolved. This affects the strength of the false summit accusation against the mountain perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_natural_law_claim, conceptual, 'Whether term-limit naturalization forecloses or coexists with collegiality-based readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magistracies_and_collegiality__term_limit_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magi_tr_t0, magistracies_and_collegiality__term_limit_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(magi_tr_t3, magistracies_and_collegiality__term_limit_reading, theater_ratio, 3, 0.28).
narrative_ontology:measurement(magi_tr_t6, magistracies_and_collegiality__term_limit_reading, theater_ratio, 6, 0.35).

% Extraction over time
narrative_ontology:measurement(magi_be_t0, magistracies_and_collegiality__term_limit_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(magi_be_t3, magistracies_and_collegiality__term_limit_reading, base_extractiveness, 3, 0.25).
narrative_ontology:measurement(magi_be_t6, magistracies_and_collegiality__term_limit_reading, base_extractiveness, 6, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(magi_su_t0, magistracies_and_collegiality__term_limit_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(magi_su_t3, magistracies_and_collegiality__term_limit_reading, suppression_requirement, 3, 0.42).
narrative_ontology:measurement(magi_su_t6, magistracies_and_collegiality__term_limit_reading, suppression_requirement, 6, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magistracies_and_collegiality__term_limit_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(magistracies_and_collegiality__term_limit_reading, magistracies_and_collegiality__collegial_veto_reading).
narrative_ontology:affects_constraint(magistracies_and_collegiality__term_limit_reading, magistracies_and_collegiality__cursus_honorum_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a three-part kernel. The other readings (collegial_veto and cursus_honorum) are separate constraint stories with potentially different epsilon values, reflecting different structural mechanisms for preventing power consolidation. The term-limit reading claims the calendar is primary; the collegial reading claims the veto is primary; the ladder reading claims the sequence is primary. All three stories link to each other through network.affects_constraints to represent the kernel contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
