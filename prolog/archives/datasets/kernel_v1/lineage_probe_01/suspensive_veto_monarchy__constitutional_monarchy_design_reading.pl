% ============================================================================
% CONSTRAINT STORY: suspensive_veto_monarchy__constitutional_monarchy_design_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_suspensive_veto_monarchy__constitutional_monarchy_design_reading, []).

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
 *   constraint_id: suspensive_veto_monarchy__constitutional_monarchy_design_reading
 *   human_readable: Suspensive Veto in Constitutional Monarchy (Design Reading)
 *   domain: constitutional_law/separation_of_powers
 *
 * SUMMARY:
 *   The suspensive veto in the French constitutional monarchy (1791–1792)
 *   instantiated a serious design: an executive with the power to delay
 *   legislation but not to deny it permanently, forcing both the assembly and
 *   the monarch to reconsider contested measures without permitting either
 *   side to exercise absolutism. Montesquieu's separation-of-powers doctrine
 *   operationalized in this mechanism required the assembly to pass a bill a
 *   second time after the executive returned it, converting a veto from an
 *   absolute block into a supermajority gate. This constraint exhibits the
 *   classic tangled-rope structure: genuine coordination function (forcing
 *   deliberation on consequential measures) coupled with asymmetric
 *   extraction (time and political capital extracted from legislative
 *   majorities). The design's beneficiary is bicameral-style deliberation in
 *   time — measures cannot pass on whim, and both executive and assembly must
 *   justify their positions across readings. The victim set is
 *   winner-take-all sovereignty: both absolute monarchy and unchecked
 *   assembly omnipotence are suppressed, making the veto unpopular with
 *   radical democratizers (who saw assembly supremacy as legitimate) and
 *   reactionaries (who wanted executive absolutism). The extractiveness score
 *   (0.35) reflects that the veto imposes real coordination costs
 *   (supermajority discipline, reconsideration infrastructure) but does not
 *   constitute pure extraction — the assembly retains genuine agency through
 *   the second-reading override mechanism, and deliberation benefits both
 *   sides in legitimate cases.
 *
 * KEY AGENTS:
 *   - The Assembly (institutional/constrained): Experiences the veto as both coordination mechanism (forced reconsideration improves legislation) and extraction (time, political capital, supermajority discipline required). Primary victim/beneficiary status depends on legislative outcome.
 *   - The Executive/Monarch (institutional/arbitrage): Benefits from veto as coordination tool without extracting rent — cannot rule unilaterally, cannot deny permanently. Net beneficiary but without expropriation.
 *   - The Rash Legislator (powerless/trapped): A faction of the assembly majority attempting to pass legislation without deliberation faces the veto as a snare — trapped in reconsideration with no exit except withdrawal or supermajority organization.
 *   - The Aristocracy and Church (powerful/mobile): Status quo defenders benefit from the veto's delay mechanism, gaining time to organize opposition. But also experience extraction if supermajority passage overrides their objections — mixed tangled_rope status.
 *   - Constitutional Designer / Montesquieu Operationalized (powerful/mobile): Sees the veto as temporary structural support — a scaffold preventing both absolutism and assembly tyranny until procedural norms mature.
 *   - The Analytical Observer (analytical/analytical): Risks seeing the design as a natural law of balanced government rather than a contingent institutional arrangement that depends on trust, perceived legitimacy, and stable constitutional consensus.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(suspensive_veto_monarchy__constitutional_monarchy_design_reading, 0.35).
domain_priors:suppression_score(suspensive_veto_monarchy__constitutional_monarchy_design_reading, 0.48).
domain_priors:theater_ratio(suspensive_veto_monarchy__constitutional_monarchy_design_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(suspensive_veto_monarchy__constitutional_monarchy_design_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(suspensive_veto_monarchy__constitutional_monarchy_design_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(suspensive_veto_monarchy__constitutional_monarchy_design_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(suspensive_veto_monarchy__constitutional_monarchy_design_reading, tangled_rope).
narrative_ontology:human_readable(suspensive_veto_monarchy__constitutional_monarchy_design_reading, "Suspensive Veto in Constitutional Monarchy (Design Reading)").
narrative_ontology:topic_domain(suspensive_veto_monarchy__constitutional_monarchy_design_reading, "constitutional_law/separation_of_powers").

domain_priors:requires_active_enforcement(suspensive_veto_monarchy__constitutional_monarchy_design_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(suspensive_veto_monarchy__constitutional_monarchy_design_reading, '3cef46a5-726d-43c8-91ba-584226155457').
narrative_ontology:cs_kernel_codification('3cef46a5-726d-43c8-91ba-584226155457', formalized).
narrative_ontology:cs_authority_grounding('3cef46a5-726d-43c8-91ba-584226155457', lineage).
narrative_ontology:cs_interpretation_layer_present('3cef46a5-726d-43c8-91ba-584226155457').
narrative_ontology:cs_reading_relation('3cef46a5-726d-43c8-91ba-584226155457', suspensive_veto_monarchy__paralysis_mechanism_reading, influences).
narrative_ontology:cs_reading_relation('3cef46a5-726d-43c8-91ba-584226155457', suspensive_veto_monarchy__varennes_broken_trust_reading, coexists_with).
narrative_ontology:cs_axiom('3cef46a5-726d-43c8-91ba-584226155457', foundational, executive_deliberation_incentive_alignment).
narrative_ontology:cs_axiom_status(executive_deliberation_incentive_alignment, holdable).
narrative_ontology:cs_axiom_grounding('3cef46a5-726d-43c8-91ba-584226155457', executive_deliberation_incentive_alignment, instrumental).
narrative_ontology:cs_axiom('3cef46a5-726d-43c8-91ba-584226155457', foundational, assembly_supermajority_override_availability).
narrative_ontology:cs_axiom_status(assembly_supermajority_override_availability, holdable).
narrative_ontology:cs_axiom_grounding('3cef46a5-726d-43c8-91ba-584226155457', assembly_supermajority_override_availability, deontological).
narrative_ontology:cs_reference_frame('3cef46a5-726d-43c8-91ba-584226155457', balanced_separation_of_powers).
narrative_ontology:cs_drift_state('3cef46a5-726d-43c8-91ba-584226155457', varennes_flight_june_1791, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('3cef46a5-726d-43c8-91ba-584226155457', '').
narrative_ontology:cs_kernel_id(suspensive_veto_monarchy__constitutional_monarchy_design_reading, suspensive_veto_monarchy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(suspensive_veto_monarchy__constitutional_monarchy_design_reading, bicameral_deliberation).
narrative_ontology:constraint_beneficiary(suspensive_veto_monarchy__constitutional_monarchy_design_reading, executive_authority).
narrative_ontology:constraint_victim(suspensive_veto_monarchy__constitutional_monarchy_design_reading, absolute_monarchy).
narrative_ontology:constraint_victim(suspensive_veto_monarchy__constitutional_monarchy_design_reading, assembly_omnipotence).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RASH LEGISLATOR (SNARE) — The assembly majority facing a suspensive veto has only one exit: accept delay or withdraw the bill. Cannot override (by design); cannot proceed; trapped in reconsideration cycle. The veto's asymmetry extracts time and political capital from the majority, forcing them to litigate every measure twice. No genuine coordination benefit to the trapped majority — only constraint.
constraint_indexing:constraint_classification(suspensive_veto_monarchy__constitutional_monarchy_design_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE ASSEMBLY (TANGLED ROPE) — Constrained by the veto's delay but also benefits from being forced to reconsider rash legislation. The constraint coordinates deliberation (genuine coordination function) but also extracts time and requires supermajority discipline (active enforcement). The assembly has agency — it can pass measures on second reading — but at a cost. Mixed coordination and extraction at institutional scale.
constraint_indexing:constraint_classification(suspensive_veto_monarchy__constitutional_monarchy_design_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE EXECUTIVE (ROPE) — The monarch with suspensive veto experiences pure coordination: the veto is a mechanism for forcing reconsideration, not a tool of extraction. The monarch cannot extract rent (cannot deny permanently, cannot rule unilaterally). The veto's value lies in the deliberation it produces — the monarch benefits from better legislation emerging from reconsideration. Net beneficiary but without exploitative extraction.
constraint_indexing:constraint_classification(suspensive_veto_monarchy__constitutional_monarchy_design_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CONSTITUTIONAL DESIGNER (SCAFFOLD) — The designer sees the veto as temporary structural support: a mechanism to prevent both absolutism and assembly omnipotence until constitutional norms mature and procedural discipline replaces veto enforcement. Low effective extraction (theater ≤ 0.70) because the design has a built-in sunset — as deliberative norms strengthen, the veto's suppressive force diminishes. Sunset implicit in the mechanism: as assembly discipline increases, veto invocations decline naturally.
constraint_indexing:constraint_classification(suspensive_veto_monarchy__constitutional_monarchy_design_reading, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: THE ARISTOCRACY (TANGLED ROPE) — Powerful actors (nobility, church) who oppose democratic legislation benefit from the veto's delay mechanism — they gain time to organize opposition and extract concessions. But they also experience extraction: if the assembly passes measures a second time, the veto failed to protect their interests. Mixed beneficiary-victim status depending on legislative outcome. Coordination function (forcing time for interest aggregation) coupled with asymmetric extraction favoring status quo defenders.
constraint_indexing:constraint_classification(suspensive_veto_monarchy__constitutional_monarchy_design_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational perspective, the suspensive veto embodies an immutable truth about separated powers: preventing both absolutism AND assembly tyranny simultaneously requires a symmetric suppression mechanism. No side can overcome the other absolutely; deadlock is a structural feature, not a bug. This perspective sees the design as a natural law of balanced government. However, the structural data (identifiable beneficiaries and victims, active enforcement, extractiveness > 0) contradicts the mountain classification — the engine will reclassify as false summit.
constraint_indexing:constraint_classification(suspensive_veto_monarchy__constitutional_monarchy_design_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(suspensive_veto_monarchy__constitutional_monarchy_design_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(suspensive_veto_monarchy__constitutional_monarchy_design_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(suspensive_veto_monarchy__constitutional_monarchy_design_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(suspensive_veto_monarchy__constitutional_monarchy_design_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   EXTRACTIVENESS (0.35): Moderate, reflecting genuine mixed coordination-extraction structure. The veto imposes real costs on legislative majorities — time for reconsideration, supermajority discipline, political capital expenditure — but these costs are partly legitimate coordination overheads (ensuring deliberation on consequential measures) rather than pure expropriation. The value sits at the lower end of the tangled-rope range because the assembly retains genuine agency through the second-reading override mechanism. If the supermajority threshold were insurmountable or if the veto were frequently deployed to prevent measures the assembly endorsed on second reading, extractiveness would be higher. SUPPRESSION (0.48): Moderate-high. Suppression operates symmetrically on both absolutism (executive cannot rule unilaterally) and assembly omnipotence (assembly cannot impose measures without deliberation). But suppression is not total — the assembly can override on second reading, and the executive can be overridden. Suppression reflects the mechanism's design to prevent both sides from achieving total power. THEATER RATIO (0.55): Moderate. The veto ceremony has performative elements (formal return of legislation, formal second reading) but also genuine functional content (reconsideration occurs, amendments are sometimes introduced, positions are defended). The theater ratio is not high because deliberation is not purely theatrical — some measures are actually changed through reconsideration. The ratio increases slightly over time (0.42 → 0.55) as the veto becomes ritualized and the probability of override success decreases, turning the ceremony into more theater and less consequential reconsideration.
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces a perspectival gap between design intent and institutional reception. The constitutional designer (powerful/mobile) sees the veto as temporary scaffolding producing bicameral deliberation — a coordination mechanism with an implicit sunset as norms mature. The executive sees pure coordination (rope). The assembly sees mixed coordination and extraction (tangled_rope). A rash legislative faction sees a snare. Status quo defenders see a strategic delay mechanism (tangled_rope, with opposite directionality from the assembly). The analytical observer risks naturalizing the design as an immutable law of balanced government (mountain, false summit). The perspectival gap widens when trust collapses — if the monarch uses the veto to obstruct emergency measures or if the assembly treats the veto as illegitimate, the shared understanding that drives the design breaks down. The Varennes flight (June 1791) occurs within the measurement interval and should appear as a discontinuity in the extractiveness trajectory — post-Varennes, the veto becomes an exercise of authority by an agent the people no longer trust, shifting all perspectives toward higher extractiveness and lower suppression effectiveness.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value is determined by structural position. The assembly (institutional/constrained beneficiary) has moderate d reflecting constrained exit and mixed benefits. The executive (institutional/arbitrage beneficiary) has low d reflecting arbitrage exit and net beneficiary status. The rash legislator (powerless/trapped victim) has high d reflecting trapped exit and victim status. The analytical observer (analytical/analytical) has canonical d ≈ 0.73 reflecting the analytical position's asymmetry. The directionality derivation captures that the veto extracts more from legislative majorities (who must organize supermajorities) than from the executive (who simply invokes the mechanism). The suppression metric is NOT scaled by directionality — it is a raw structural property of the mechanism itself (symmetrically suppressing both absolutism and omnipotence). The effective extractiveness χ is scaled by f(d), so agents with higher d (higher structural extraction target status) experience higher χ even though base extractiveness ε is constant.
 *
 * MANDATROPHY ANALYSIS:
 *   READING-SPECIFIC MANDATROPHY: This constraint resolves the mandatrophy at the reading level. The design reading posits that the suspensive veto IS a coordination mechanism (tangled_rope: genuine coordination function + asymmetric extraction). The paralysis reading would posit that the veto became pure extraction during the crisis (snare classification). The Varennes reading would posit that trust collapse emptied the design (piton or snare classification). The mandatrophy is not 'which type is the veto really?' but 'which reading's structural analysis is operative at which historical moment?' Pre-Varennes, the design reading captures the actual constraint: extractiveness ~0.35, suppression ~0.48, genuine coordination function present. Post-Varennes, the design reading's assumptions (legitimate executive authority, good-faith deliberation) collapse. The constraint's identity itself shifts — the formal mechanism remains, but the extractive mechanism changes. This is not a single constraint viewed from different angles; it is a kernel with multiple readings, each of which identifies a different constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deliberation_quality_threshold,
    'Does forced reconsideration actually improve legislative quality, or does it merely slow passage without improving substance?',
    'Comparative analysis of measures withdrawn on first veto vs. passed on second reading; examination of amendments introduced in reconsideration; tracking of legislative stability and reversal rates',
    'If reconsideration improves quality: genuine coordination function confirmed, classification remains tangled_rope/rope. If quality unchanged: veto is pure delay mechanism, shifts toward snare/piton classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deliberation_quality_threshold, empirical, 'Whether forced reconsideration improves legislative quality or merely delays passage').

omega_variable(
    assembly_supermajority_availability,
    'Under what conditions can the assembly actually achieve supermajority on second reading? Is the threshold realistic or designed to be insurmountable?',
    'Historical data on second-reading passage rates; analysis of veto override success; comparison to absolutist veto systems where override is impossible',
    'If supermajority achievable: assembly retains genuine agency, coordinate design intent validated. If insurmountable: veto becomes absolute in practice despite formally suspensive design, classification shifts to snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(assembly_supermajority_availability, empirical, 'Achievability of assembly supermajority override on second reading').

omega_variable(
    absolutism_vs_democracy_false_dichotomy,
    'Does the design truly suppress both absolutism AND assembly omnipotence, or does it ultimately privilege one side over the other?',
    'Historical analysis of legislative outcomes under suspensive veto: which side (executive, assembly, status quo) wins contested measures on average? Comparison to systems with different veto architectures.',
    'If truly symmetric: the design''s claim to balance is validated. If asymmetric: identifies which side the mechanism actually privileges and reframes the ''victim set'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(absolutism_vs_democracy_false_dichotomy, conceptual, 'Whether suspensive veto truly suppresses both absolutism and assembly omnipotence or privileges one side').

omega_variable(
    constitutional_reading_ambiguity,
    'Which constitutional reading is this constraint instantiating: the design reading (veto as deliberation mechanism) or the paralysis reading (veto as obstruction mechanism) or the broken-trust reading (Varennes invalidates design)?',
    'Historical periodization: pre-Varennes period (design operative), Varennes moment (trust collapse), post-Varennes period (design void). Compare extractiveness and suppression metrics across periods.',
    'If design reading holds pre-Varennes: extractiveness drops post-Varennes as trust collapses. If paralysis reading accurate from outset: extractiveness should be high and stable throughout. Determines which sibling reading captures the actual constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutional_reading_ambiguity, conceptual, 'Which reading of the suspensive veto''s constitutional role is accurate: design, paralysis, or broken-trust').

omega_variable(
    montesquieu_intent_vs_jacobin_reception,
    'Does the veto''s extractiveness derive from Montesquieu''s design intent (balance through symmetric suppression) or from Jacobin reception of the veto as obstruction?',
    'Textual analysis of Montesquieu''s mechanism vs. Revolutionary assembly''s experience; comparative analysis of design rhetoric vs. lived institutional behavior.',
    'If design intent operative: extractiveness reflects the coordination cost of deliberation (~0.35 claimed). If Jacobin reception dominant: extractiveness should be higher, reflecting experienced obstruction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(montesquieu_intent_vs_jacobin_reception, conceptual, 'Whether extractiveness derives from design intent or institutional reception').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(suspensive_veto_monarchy__constitutional_monarchy_design_reading, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sveto_tr_t0, suspensive_veto_monarchy__constitutional_monarchy_design_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(sveto_tr_t2, suspensive_veto_monarchy__constitutional_monarchy_design_reading, theater_ratio, 2, 0.5).
narrative_ontology:measurement(sveto_tr_t4, suspensive_veto_monarchy__constitutional_monarchy_design_reading, theater_ratio, 4, 0.55).

% Extraction over time
narrative_ontology:measurement(sveto_be_t0, suspensive_veto_monarchy__constitutional_monarchy_design_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(sveto_be_t2, suspensive_veto_monarchy__constitutional_monarchy_design_reading, base_extractiveness, 2, 0.32).
narrative_ontology:measurement(sveto_be_t4, suspensive_veto_monarchy__constitutional_monarchy_design_reading, base_extractiveness, 4, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(sveto_su_t0, suspensive_veto_monarchy__constitutional_monarchy_design_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(sveto_su_t2, suspensive_veto_monarchy__constitutional_monarchy_design_reading, suppression_requirement, 2, 0.46).
narrative_ontology:measurement(sveto_su_t4, suspensive_veto_monarchy__constitutional_monarchy_design_reading, suppression_requirement, 4, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(suspensive_veto_monarchy__constitutional_monarchy_design_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(suspensive_veto_monarchy__constitutional_monarchy_design_reading, suspensive_veto_monarchy__paralysis_mechanism_reading).
narrative_ontology:affects_constraint(suspensive_veto_monarchy__constitutional_monarchy_design_reading, suspensive_veto_monarchy__varennes_broken_trust_reading).

% DUAL FORMULATION NOTE:
% The suspensive veto kernel has three structurally distinct constraint readings. This file instantiates the design reading (veto as coordination mechanism, ε=0.35). The paralysis reading (veto as obstruction during crisis, higher ε) and Varennes reading (trust collapse empties the mechanism, different suppression and victim-set analysis) are separate constraint stories linked via network.affects_constraints. Each reading has its own extracted metrics, its own perspectives, and its own temporal dynamics. The three readings are NOT perspectives on a single constraint — they are different constraints instantiated by different readings of the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(suspensive_veto_monarchy__constitutional_monarchy_design_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
