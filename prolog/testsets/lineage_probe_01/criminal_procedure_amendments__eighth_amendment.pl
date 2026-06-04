% ============================================================================
% CONSTRAINT STORY: criminal_procedure_amendments__eighth_amendment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_criminal_procedure_amendments__eighth_amendment, []).

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
 *   constraint_id: criminal_procedure_amendments__eighth_amendment
 *   human_readable: Eighth Amendment Proportionality Constraint on Penal Power
 *   domain: political/legal/criminal_procedure
 *
 * SUMMARY:
 *   The Eighth Amendment forbids excessive bail, excessive fines, and cruel
 *   and unusual punishments, instantiating a proportionality principle into
 *   American penal power. This constraint operates as a reading of the
 *   broader criminal_procedure_amendments kernel — a contested commitment
 *   spanning the Fourth, Fifth, Sixth, Seventh, and Eighth Amendments — each
 *   of which structures different aspects of criminal procedure. The Eighth
 *   Amendment's specific contribution is the proportionality boundary:
 *   punishment must be bounded not only by procedure (Fourth, Fifth, Sixth,
 *   Seventh) but also by substance — the severity of the punishment must bear
 *   rational relationship to the offense. This reading distinguishes itself
 *   from its siblings by importing a substantive constraint on *outcomes*
 *   rather than only on *processes*. The constraint exhibits the classic
 *   structure of a commitment system: a fixed text ('cruel and unusual
 *   punishments') that grounds authority claims, an interpretive tradition
 *   that has substantially shifted from originalist to evolving-standards
 *   doctrine, and active enforcement through appellate review. The Eighth
 *   Amendment is simultaneously protective (defending imprisoned and detained
 *   persons from excess), coordinative (establishing baseline legitimacy for
 *   sentencing systems across jurisdictions), and extractive (permitting
 *   incarceration as long as it remains within proportionality bounds, thus
 *   enabling massive punishment volumes that would be foreclosed by
 *   alternative proportionality readings).
 *
 * KEY AGENTS:
 *   - Imprisoned and detained persons: Primary beneficiary (powerless/trapped) — protected from excess bail, fines, and cruel punishment; but protection is bounded and proportionality itself may permit vast incarceration
 *   - Bail-eligible detainees: Secondary beneficiary (moderate/constrained) — protected from excessive bail but still subject to bail systems that function as wealth filters and pre-trial extraction
 *   - Maximal-deterrence policy advocates: Primary victim (institutional/mobile) — Eighth Amendment proportionality bounds limit deterrence-maximizing strategies (extremely long sentences, novel punishments)
 *   - Courts and judiciary: Primary coordinator (institutional/arbitrage) — enforce proportionality, gain legitimacy from constraint, maintain sentencing discretion within bounds
 *   - Criminal justice reform coalition: Secondary coordinator (organized/constrained) — use Eighth Amendment proportionality principle to argue against mass incarceration, three-strikes laws, mandatory minimums
 *   - Originalist jurisprudence: Institutional interpreter (institutional/mobile) — invokes Eighth Amendment while treating 'unusual' as historically bounded, reducing its contemporary force
 *   - Evolving-standards doctrine: Institutional interpreter (institutional/mobile) — invokes Eighth Amendment while treating proportionality as living principle, expanding its contemporary force
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(criminal_procedure_amendments__eighth_amendment, 0.38).
domain_priors:suppression_score(criminal_procedure_amendments__eighth_amendment, 0.52).
domain_priors:theater_ratio(criminal_procedure_amendments__eighth_amendment, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(criminal_procedure_amendments__eighth_amendment, extractiveness, 0.38).
narrative_ontology:constraint_metric(criminal_procedure_amendments__eighth_amendment, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(criminal_procedure_amendments__eighth_amendment, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(criminal_procedure_amendments__eighth_amendment, tangled_rope).
narrative_ontology:human_readable(criminal_procedure_amendments__eighth_amendment, "Eighth Amendment Proportionality Constraint on Penal Power").
narrative_ontology:topic_domain(criminal_procedure_amendments__eighth_amendment, "political/legal/criminal_procedure").

domain_priors:requires_active_enforcement(criminal_procedure_amendments__eighth_amendment).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(criminal_procedure_amendments__eighth_amendment, '1cc76013-49fa-4b44-9aab-0cdd9747c218').
narrative_ontology:cs_kernel_codification('1cc76013-49fa-4b44-9aab-0cdd9747c218', fixed_text).
narrative_ontology:cs_authority_grounding('1cc76013-49fa-4b44-9aab-0cdd9747c218', lineage).
narrative_ontology:cs_interpretation_layer_present('1cc76013-49fa-4b44-9aab-0cdd9747c218').
narrative_ontology:cs_reading_relation('1cc76013-49fa-4b44-9aab-0cdd9747c218', criminal_procedure_amendments__fifth_amendment, influences).
narrative_ontology:cs_reading_relation('1cc76013-49fa-4b44-9aab-0cdd9747c218', criminal_procedure_amendments__fourth_amendment, coexists_with).
narrative_ontology:cs_reading_relation('1cc76013-49fa-4b44-9aab-0cdd9747c218', criminal_procedure_amendments__sixth_amendment, influences).
narrative_ontology:cs_reading_relation('1cc76013-49fa-4b44-9aab-0cdd9747c218', criminal_procedure_amendments__seventh_amendment, coexists_with).
narrative_ontology:cs_axiom('1cc76013-49fa-4b44-9aab-0cdd9747c218', foundational, punishment_must_be_proportional_to_offense).
narrative_ontology:cs_axiom_status(punishment_must_be_proportional_to_offense, holdable).
narrative_ontology:cs_axiom_grounding('1cc76013-49fa-4b44-9aab-0cdd9747c218', punishment_must_be_proportional_to_offense, deontological).
narrative_ontology:cs_axiom('1cc76013-49fa-4b44-9aab-0cdd9747c218', secondary, cruel_and_unusual_means_historically_unusual_at_ratification).
narrative_ontology:cs_axiom_status(cruel_and_unusual_means_historically_unusual_at_ratification, overridden).
narrative_ontology:cs_axiom_grounding('1cc76013-49fa-4b44-9aab-0cdd9747c218', cruel_and_unusual_means_historically_unusual_at_ratification, empirically_contingent).
narrative_ontology:cs_reference_frame('1cc76013-49fa-4b44-9aab-0cdd9747c218', historical_unusual_and_cruel_baseline).
narrative_ontology:cs_drift_state('1cc76013-49fa-4b44-9aab-0cdd9747c218', contemporary_mass_incarceration, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('1cc76013-49fa-4b44-9aab-0cdd9747c218', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(criminal_procedure_amendments__eighth_amendment, criminal_procedure_amendments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(criminal_procedure_amendments__eighth_amendment, convicted_and_detained_persons).
narrative_ontology:constraint_victim(criminal_procedure_amendments__eighth_amendment, maximal_deterrence_policy).
narrative_ontology:constraint_victim(criminal_procedure_amendments__eighth_amendment, retributive_excess_prevention).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: IMPRISONED PERSON (SNARE) — Faces maximum extraction through excessive bail, fines calibrated beyond capacity to pay, and punishments designed without proportionality constraint. No exit from the penal system except through sentence completion or clemency. The Eighth Amendment's proportionality requirement is their only structural protection against extraction of suffering as a primary goal rather than incidental effect. Suppression is total: confinement itself prevents alternative arrangements.
constraint_indexing:constraint_classification(criminal_procedure_amendments__eighth_amendment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: BAIL-ELIGIBLE DETAINEE (TANGLED ROPE) — Subject to bail systems that provide genuine coordination function (securing court appearance) but also extract through excessive amounts. Constrained but not trapped: bail-eligible means structural exit exists (posting bail, release conditions) at a price. The Eighth Amendment's excessive-bail clause provides partial protection but coordination and extraction coexist — the system genuinely needs security but also accumulates wealth from those who cannot meet it.
constraint_indexing:constraint_classification(criminal_procedure_amendments__eighth_amendment, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: COURTS / JUDICIAL SYSTEM (ROPE) — Experience the Eighth Amendment as a coordination mechanism: establishing proportionality floors prevents races-to-the-bottom in punitiveness between jurisdictions and maintains legitimacy of sentencing. Institutional arbitrage: courts gain legitimacy and reduced appellate burden by using proportionality review. The constraint structures judicial authority without severely limiting it — judges still control sentencing within bounds.
constraint_indexing:constraint_classification(criminal_procedure_amendments__eighth_amendment, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REFORM COALITION (SCAFFOLD) — Organized actors (public defenders, civil rights groups, some prosecutors) see the Eighth Amendment as a temporary scaffolding enabling criminal justice reform. The amendment provides legitimacy for arguments against mass incarceration, three-strikes laws, and mandatory minimums. Sunset dynamic: as sentencing norms shift and reformation becomes default framing, the Eighth Amendment's proportionality constraint becomes normalized rather than exceptional. Low extraction from this view because the constraint enables exit from punitive excess.
constraint_indexing:constraint_classification(criminal_procedure_amendments__eighth_amendment, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ORIGINALIST JURISPRUDENCE (PITON) — The Eighth Amendment, read as a fixed 18th-century prohibition on 'cruel and unusual punishments,' has become substantially theater. The constraint persists through institutional inertia and invocation of original meaning, but the contemporary force of 'cruel and unusual' is mostly performative — courts invoke it sparingly, allow massive mandatory sentences to stand, and struggle with application. The original constitutional reference to 'unusual' (meaning unusual relative to established practice at ratification) no longer functions; 'cruel' has become the only operative term, and courts treat it as requiring extreme circumstances. Theater ratio rises because the constraint is repeatedly invoked without producing systematic constraints on actual sentencing practices.
constraint_indexing:constraint_classification(criminal_procedure_amendments__eighth_amendment, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, proportionality between offense and punishment is an irreducible requirement of legitimate penal power. Any system claiming legitimacy must bound punishment by proportionality; without this, punishment becomes indistinguishable from torture. This perspective treats proportionality as a natural law of justice itself, not a contingent constitutional choice. However, the structural data contradicts this classification: identifiable beneficiaries (those protected from excessive punishment), victims (deterrence-maximalist policies), and active enforcement requirements all signal a contingent institutional arrangement, not a law of nature. The engine will identify this as a false summit.
constraint_indexing:constraint_classification(criminal_procedure_amendments__eighth_amendment, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(criminal_procedure_amendments__eighth_amendment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(criminal_procedure_amendments__eighth_amendment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(criminal_procedure_amendments__eighth_amendment, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(criminal_procedure_amendments__eighth_amendment, TR),
    TR >= 0.70.

:- end_tests(criminal_procedure_amendments__eighth_amendment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The Eighth Amendment structures the constraint as protective (beneficiaries clearly defined) but also permits substantial incarceration within proportionality bounds. The key insight is that proportionality does not eliminate extraction — it bounds it. A jurisdiction can incarcerate massive populations as long as individual sentences remain proportional to offenses. This differs from constraints that would eliminate incarceration entirely (abolitionist reading) or cap total incarceration volume (resource-allocation constraint). The extractiveness value reflects that the Eighth Amendment permits extraction of liberty as long as it remains bounded. Suppression (0.52): Moderate-high. Significant barriers to challenging excessive punishments: appellate review is narrow (courts defer to trial judges), proportionality claims require expensive appellate litigation, standard-of-review doctrine places burden on defendant to show 'grossly disproportionate' punishment (high bar), and clemency alternatives are rare. However, suppression is not total — some successful challenges exist, proportionality review is conducted (even if rarely overturned), and reform movements can mobilize proportionality rhetoric. Theater ratio (0.48): Moderate. The Eighth Amendment is genuinely enforced through appellate review (not purely performative) but enforcement is selective and limited: courts rarely overturn sentences as disproportionate, originalist interpretation has narrowed 'unusual' to mean 'unusual by historical practice' rather than 'unusual by contemporary standards,' and the constraint coexists with mandatory minimums, three-strikes laws, and life-without-parole sentences that seem to violate proportionality by any reasonable metric. The theater has increased over time as the constraint is invoked more often in appellate briefs but overturned less frequently.
 *
 * PERSPECTIVAL GAP:
 *   The range of classifications (snare through mountain) reveals deep structural contestation. The imprisoned person perceives the constraint as inadequate (snare — proportionality is a floor that permits vast punishment volumes). The reform coalition perceives it as a temporary scaffold toward abolition (sunset as norms shift). The originalist interpreter perceives it as degraded theater (piton — historical 'unusual' no longer constrains contemporary practice). The court perceives it as coordination (rope — establishes legitimacy and discretionary room). The analytical observer risks perceiving it as natural law (mountain — proportionality as intrinsic to justice) but the structural data reveals false-summit pathology: identifiable beneficiaries, active enforcement, and contingent institutional grounding contradict the natural-law reading. The deepest gap is between the originalist (piton — theater) and evolving-standards (rope/scaffold — functioning protection) interpretations: they disagree on what 'cruel and unusual' means, producing radically different classifications from the same text.
 *
 * DIRECTIONALITY LOGIC:
 *   The Eighth Amendment's directionality varies sharply across perspectives based on structural position. Imprisoned persons occupy maximum victim status (d ≈ 0.95) — they are maximally extracted-from, completely trapped, with no exit except sentence completion. Bail-eligible detainees occupy moderate victim status (d ≈ 0.65) — extracted from through bail systems but structurally able to exit via posting bail. Courts occupy moderate beneficiary status (d ≈ 0.45) — they benefit from the constraint's legitimacy-providing function and maintain discretionary authority within bounds. Maximal-deterrence policy advocates occupy moderate victim status (d ≈ 0.70) — their preferred policies are constrained, though not foreclosed. The Eighth Amendment's proportionality principle generates beneficial treatment for those protected from excess and constraining treatment for those implementing deterrence-maximum strategies. This asymmetry is the core tangled-rope signature: genuine coordination function (establishing baseline legitimacy for sentencing) alongside asymmetric extraction (benefiting confined persons at the cost of deterrence-maximalist policies).
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification resolves the mandatrophy by showing that proportionality is genuinely coordinative (establishing baseline legitimacy for sentencing systems) but also extractive (permitting massive punishment volumes within proportionality bounds). The constraint does not pit coordination against extraction — it combines them. The Eighth Amendment enables both the coordination function (courts gain legitimacy from proportionality review) and the extraction function (incarceration proceeds unchecked as long as individual sentences remain proportional). The false-summit perspective (mountain) reveals the mandatrophy most clearly: if proportionality were a natural law of justice, it would foreclose extraction entirely. But proportionality is a bounded principle — it permits extraction as long as bounds are respected. This is the signature of tangled_rope, not mountain. The measurement trajectory showing rising theater_ratio reflects that as jurisprudence has shifted toward originalism, the Eighth Amendment has become more theatrical (invoked but less frequently overturned), while extractiveness has remained stable (the constraint bounds punishment but does not eliminate it). The stable suppression trajectory reflects that barriers to challenging excessive punishment have remained consistent — appellate deference, high burden of proof, rare clemency.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_metric_indeterminacy,
    'What constitutes ''proportionality'' between offense and punishment? Is proportionality an objective scalar (years-of-imprisonment-to-offense-severity ratio) or a constitutional delegated judgment?',
    'Comparative jurisprudence: analyze whether courts across jurisdictions converge on proportionality ratios or consistently diverge; track appellate reversal rates for disproportionality claims',
    'If objective metric exists and courts apply it systematically: extractiveness drops (clear rule, predictable enforcement). If delegated judgment: extractiveness rises (discretion enables extraction, theater increases as proportionality claims lack anchor).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_metric_indeterminacy, conceptual, 'Whether proportionality is objective scalar or delegated constitutional judgment').

omega_variable(
    cruel_and_unusual_temporal_shift,
    'Does ''cruel and unusual'' mean ''unusual relative to 1791 practice'' (originalist anchor) or ''cruel by contemporary standards'' (evolving standards doctrine)? How does the reading chosen affect which punishments are prohibited?',
    'Historical analysis of Supreme Court precedent (Trop v. Dulles, Weems v. United States, Estelle v. Gamble); comparison of outcomes under originalist vs. evolving-standards interpretations applied to specific punishments (solitary confinement duration, execution methods, mandatory minimums)',
    'Originalist reading: extractiveness rises (most contemporary punishments are not ''unusual'' by 1791 standards, constraint becomes theatrical). Evolving-standards reading: extractiveness drops (constraint adapts to changing justice norms, enforcement capacity increases).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cruel_and_unusual_temporal_shift, conceptual, 'Originalist vs. evolving-standards interpretation of cruel and unusual').

omega_variable(
    bail_excessiveness_interaction_with_poverty,
    'Does the excessive-bail clause protect against quantitatively excessive amounts (e.g., bail set above the defendant''s net worth) or against the structural effect of bail as a wealth filter that detains poor people pre-trial?',
    'Empirical data on detention rates by bail amount relative to socioeconomic status; pre-trial detention outcomes by bail level; correlation between bail excessiveness (defined by multiple standards: absolute amount, percentage of income, payment capacity) and pre-trial jail time',
    'If quantitative standard: courts can calibrate bail amounts mathematically, extractiveness is bounded. If structural effect: bail systems inherently extract detention-as-punishment from poor defendants, extractiveness remains high regardless of calibration (extraction is structural, not incidental).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(bail_excessiveness_interaction_with_poverty, empirical, 'Whether excessive bail is quantitative or structural-impact problem').

omega_variable(
    eighth_amendment_kernel_reading_ambiguity,
    'Is the Eighth Amendment''s core commitment a fixed prohibition on specific cruelties (18th-century meaning of ''unusual'') or a constitutional requirement that all penal power be bounded by proportionality (living principle)? Which sibling reading of the criminal_procedure_amendments kernel is the Eighth Amendment actually aligned with?',
    'Examine whether the Eighth Amendment''s primary function is defensive (protecting against specific harms: torture, disproportionate punishment) or structural (constraining how all five amendments relate to each other in penal contexts). Analyze whether proportionality claims require integration with Sixth Amendment fair-trial guarantees and Fifth Amendment due-process protections, or whether the Eighth stands alone.',
    'If defensive-only: the Eighth Amendment is structurally independent from its siblings (coexists_with or influences weakly). If structural-integrating: the Eighth Amendment forecloses or influences readings of the Fifth and Sixth that would allow penal processes without proportionality bounds. Classification of sibling reading_relations may need revision.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(eighth_amendment_kernel_reading_ambiguity, conceptual, 'Eighth Amendment''s core commitment: fixed prohibition vs. living proportionality principle').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(criminal_procedure_amendments__eighth_amendment, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eighth_tr_t0, criminal_procedure_amendments__eighth_amendment, theater_ratio, 0, 0.35).
narrative_ontology:measurement(eighth_tr_t50, criminal_procedure_amendments__eighth_amendment, theater_ratio, 50, 0.42).
narrative_ontology:measurement(eighth_tr_t100, criminal_procedure_amendments__eighth_amendment, theater_ratio, 100, 0.48).

% Extraction over time
narrative_ontology:measurement(eighth_be_t0, criminal_procedure_amendments__eighth_amendment, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(eighth_be_t50, criminal_procedure_amendments__eighth_amendment, base_extractiveness, 50, 0.36).
narrative_ontology:measurement(eighth_be_t100, criminal_procedure_amendments__eighth_amendment, base_extractiveness, 100, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(eighth_su_t0, criminal_procedure_amendments__eighth_amendment, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(eighth_su_t50, criminal_procedure_amendments__eighth_amendment, suppression_requirement, 50, 0.5).
narrative_ontology:measurement(eighth_su_t100, criminal_procedure_amendments__eighth_amendment, suppression_requirement, 100, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(criminal_procedure_amendments__eighth_amendment, enforcement_mechanism).
narrative_ontology:affects_constraint(criminal_procedure_amendments__eighth_amendment, fifth_amendment).
narrative_ontology:affects_constraint(criminal_procedure_amendments__eighth_amendment, sixth_amendment).
narrative_ontology:affects_constraint(criminal_procedure_amendments__eighth_amendment, sentencing_discretion_structure).
narrative_ontology:affects_constraint(criminal_procedure_amendments__eighth_amendment, bail_as_pretrial_detention).

% DUAL FORMULATION NOTE:
% The Eighth Amendment reading is part of a constraint family covering criminal procedure amendments. The Fifth Amendment reading covers double jeopardy, self-incrimination, and due process protections; the Fourth Amendment covers search and seizure; the Sixth Amendment covers fair trial machinery; the Seventh Amendment covers civil jury preservation. Each reading has distinct extractiveness and type, reflecting different structural arrangements. The Eighth Amendment is downstream of the Sixth Amendment's fair trial guarantee (proportionality review presupposes a trial occurred) and interacts with the Fifth Amendment's due-process protections (proportionality is a component of due process). Network links enable contamination analysis: if the Sixth Amendment's fair trial guarantee is degraded (faster trials, reduced counsel quality), the Eighth Amendment's proportionality review becomes less meaningful because proportionality is bounded by the trial record available for appeal.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
