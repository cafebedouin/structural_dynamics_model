% ============================================================================
% CONSTRAINT STORY: electronic_money_emergence__m4_m5_collapse_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_electronic_money_emergence__m4_m5_collapse_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: electronic_money_emergence__m4_m5_collapse_reading
 *   human_readable: M4/M5 Statistical Boundary as Retroactive Category-Constructor (Collapse Reading)
 *   domain: economic history/monetary theory/technology studies
 *
 * SUMMARY:
 *   Official monetary statistics maintain a boundary between two broad-money
 *   aggregate classes that was drawn when the underlying liability classes
 *   were institutionally segregated. Over the interval, dematerialization and
 *   institutional convergence moved instruments across the line faster than
 *   the line was redrawn, until the boundary tracked a distinction the
 *   monetary system no longer makes. The category of electronic money exists
 *   in the statistical record because the boundary drew it: series for the
 *   new instrument class begin at definition, and histories written from
 *   those series date the category's emergence to the measurement act. This
 *   story authors that arrangement — the maintained distinction itself — as a
 *   classificatory structure whose original function has atrophied, whose
 *   persistence rests on series-continuity inertia, and whose ongoing
 *   maintenance is increasingly performative. The claim and the metrics are
 *   authored independently: the claimed type states what this reading takes
 *   the structure to be; the metrics state what its operation descriptively
 *   looks like.
 *
 * KEY AGENTS:
 *   - - central_bank_statistics_division: Agenda-setter (institutional/constrained) — administers the aggregate definitions, publishes the series, footnotes the breaks; collects no rents from the boundary
 *   - - monetary_economists: Primary payer (organized/constrained) — inherits artifact categories into estimation and replication
 *   - - reporting_financial_institutions: Payer (powerful/constrained) — bears the statutory classification-labor burden
 *   - - economic_historians_of_money: Payer (moderate/constrained) — inherits retroactively fixed category boundaries in the documentary record
 *   - - longitudinal_monetary_analysts: Residual beneficiary (moderate/mobile) — consumes unbroken comparable series, bears no maintenance
 *   - - aggregate_consuming_policy_makers: Dual beneficiary/payer (institutional/constrained) — receives standard numbers and absorbs their distortion
 *   - - divisia_alternative_advocates: Excluded (organized/mobile) — builds parallel aggregates outside official governance
 *   - - measurement_studies_scholars: Analytical observer (analytical/analytical) — sees the full structure, collects and pays nothing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(electronic_money_emergence__m4_m5_collapse_reading, 0.38).
domain_priors:suppression_score(electronic_money_emergence__m4_m5_collapse_reading, 0.26).
domain_priors:theater_ratio(electronic_money_emergence__m4_m5_collapse_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, suppression_requirement, 0.26).
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, accessibility_collapse, 0.34).
narrative_ontology:constraint_metric(electronic_money_emergence__m4_m5_collapse_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(electronic_money_emergence__m4_m5_collapse_reading, piton).
narrative_ontology:human_readable(electronic_money_emergence__m4_m5_collapse_reading, "M4/M5 Statistical Boundary as Retroactive Category-Constructor (Collapse Reading)").
narrative_ontology:topic_domain(electronic_money_emergence__m4_m5_collapse_reading, "economic history/monetary theory/technology studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(electronic_money_emergence__m4_m5_collapse_reading, '824f7a7a-de58-4d17-a81c-dc7024098f52').
narrative_ontology:cs_kernel_codification('824f7a7a-de58-4d17-a81c-dc7024098f52', formalized).
narrative_ontology:cs_authority_grounding('824f7a7a-de58-4d17-a81c-dc7024098f52', expertise).
narrative_ontology:cs_interpretation_layer_present('824f7a7a-de58-4d17-a81c-dc7024098f52').
narrative_ontology:cs_reading_relation('824f7a7a-de58-4d17-a81c-dc7024098f52', electronic_money_emergence__became_thinkable_reading, forecloses).
narrative_ontology:cs_reading_relation('824f7a7a-de58-4d17-a81c-dc7024098f52', electronic_money_emergence__first_held_reading, forecloses).
narrative_ontology:cs_axiom('824f7a7a-de58-4d17-a81c-dc7024098f52', foundational, measurement_act_constitutes_category).
narrative_ontology:cs_axiom_status(measurement_act_constitutes_category, holdable).
narrative_ontology:cs_axiom_grounding('824f7a7a-de58-4d17-a81c-dc7024098f52', measurement_act_constitutes_category, empirically_contingent).
narrative_ontology:cs_axiom('824f7a7a-de58-4d17-a81c-dc7024098f52', foundational, no_genuine_digital_emergence_event).
narrative_ontology:cs_axiom_status(no_genuine_digital_emergence_event, holdable).
narrative_ontology:cs_axiom_grounding('824f7a7a-de58-4d17-a81c-dc7024098f52', no_genuine_digital_emergence_event, empirically_contingent).
narrative_ontology:cs_axiom('824f7a7a-de58-4d17-a81c-dc7024098f52', secondary, boundary_persistence_is_inertial).
narrative_ontology:cs_axiom_status(boundary_persistence_is_inertial, holdable).
narrative_ontology:cs_axiom_grounding('824f7a7a-de58-4d17-a81c-dc7024098f52', boundary_persistence_is_inertial, empirically_contingent).
narrative_ontology:cs_reference_frame('824f7a7a-de58-4d17-a81c-dc7024098f52', measurement_constituted_category).
narrative_ontology:cs_drift_state('824f7a7a-de58-4d17-a81c-dc7024098f52', contemporary_post_dematerialization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('824f7a7a-de58-4d17-a81c-dc7024098f52', '').
narrative_ontology:cs_kernel_id(electronic_money_emergence__m4_m5_collapse_reading, electronic_money_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(electronic_money_emergence__m4_m5_collapse_reading, longitudinal_monetary_analysts).
narrative_ontology:constraint_victim(electronic_money_emergence__m4_m5_collapse_reading, monetary_economists).
narrative_ontology:constraint_victim(electronic_money_emergence__m4_m5_collapse_reading, reporting_financial_institutions).
narrative_ontology:constraint_victim(electronic_money_emergence__m4_m5_collapse_reading, economic_historians_of_money).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(electronic_money_emergence__m4_m5_collapse_reading, aggregate_consuming_policy_makers).
narrative_ontology:constraint_victim(electronic_money_emergence__m4_m5_collapse_reading, aggregate_consuming_policy_makers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines and publishes the broad-money aggregates, maintains the boundary between the M4 and M5 instrument classes, and footnotes every series break when instruments migrate across the line. Methodology committees keep the definitions stable because discontinuing or redrawing them would break decades of published comparability. Nobody inside the division draws budget or career advantage from the boundary itself; redrawing it would cost the division more than keeping it costs anyone.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, central_bank_statistics_division, agenda_setter,
    institutional, generational, constrained, national).

% Use the official aggregates for empirical work on money demand, policy transmission, and the growth of dematerialized balances. Where the boundary misclassifies instruments, their estimates absorb the error, and correcting it means rebuilding series privately at significant cost. Most continue on official definitions because journals, referees, and replication standards expect them.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, monetary_economists, payer,
    organized, biographical, constrained, global).

% File regular returns classifying their liabilities into the prescribed aggregate categories. The classification exercise consumes compliance labor, and instruments that straddle the old lines require judgment calls that can be second-guessed in supervisory review. Declining to file is not an available option.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, reporting_financial_institutions, payer,
    powerful, biographical, constrained, national).

% Date the arrival of dematerialized money from the statistical record, which fixes the category's beginning at the point the boundary was drawn or revised. Episodes in which electronic balances circulated before the category existed are hard to see through series that begin only at definition, and their primary archives are the official publications themselves.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, economic_historians_of_money, payer,
    moderate, generational, constrained, global).

% Rely on the unbroken run of comparable aggregate figures for decade-spanning studies. They receive continuity as a service and bear none of the maintenance; if the series were discontinued they would lose their panels, so they prefer the boundary stay exactly where it is, but they are free to leave for other data at any time.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, longitudinal_monetary_analysts, beneficiary,
    moderate, biographical, mobile, global).

% Read the aggregates as inputs to liquidity and credit assessments on short policy cycles. They get ready-made standard numbers, and they also absorb whatever distortion the boundary introduces into those numbers; their decisions are only as good as the categories fed to them, and they have no practical route to commissioning a rival official series.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, aggregate_consuming_policy_makers, beneficiary,
    institutional, immediate, constrained, national).
narrative_ontology:stakeholder_secondary_role(electronic_money_emergence__m4_m5_collapse_reading, aggregate_consuming_policy_makers, payer).

% Hold that the official boundary cuts monetary reality at the wrong joints and have built weighted, user-cost-based aggregates outside the official system. They sit outside the statistical governance process that fixes the official definitions; their products circulate in research but carry no official standing.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, divisia_alternative_advocates, excluded,
    organized, biographical, mobile, global).

% Study how statistical conventions constitute the phenomena they report. They compare pre-definition and post-definition documents, take the other seats' testimony, and publish on the gap between the category's statistical birthday and any behavioral change in the underlying instruments. They collect nothing and pay nothing under the arrangement.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__m4_m5_collapse_reading, measurement_studies_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(electronic_money_emergence__m4_m5_collapse_reading, diffuse).
narrative_ontology:fixing_cost_class(electronic_money_emergence__m4_m5_collapse_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The M4/M5 boundary once coordinated a real problem: institutionally segregated liability classes (bank deposits versus building-society-type deposits, later dematerialized balances) needed distinct readable aggregates so credit conditions could be assessed by instrument class, and a fixed boundary kept the resulting time series comparable across decades.
% TRANSFER_FUNCTION: Moves definitional authority over what counts as money from market participants and independent researchers to the central statistical authority; moves classification labor from the statistical system onto financial institutions' returns; and moves research attention toward reconciling categories rather than measuring underlying monetary flows.
% ABSENT_VOICES: The institutions whose liabilities migrate across the boundary had no seat when the line was drawn or redrawn; the pre-definition users of electronic balances — the people whose practice allegedly constituted the category — left no testimony in the statistical record; and alternative-aggregate builders sit outside the governance process that fixes official definitions.
% DISAPPEARANCE_RATIONALE: If the boundary and its published series vanished overnight, the statistical-analytical world would rearrange: longitudinal research programs would lose their panels, replication chains would break, and any contractual or supervisory references to the defined aggregates would need re-basing. The underlying monetary arrangements — balances held, payments made, instruments traded — would continue unchanged, which is precisely what the artifact reading predicts for a category constituted by its measurement.
% FOUNDING_PROBLEM: The boundary was built to give policymakers a readable handle on distinct classes of liquid liabilities while those classes were still institutionally segregated, so that monetary assessment could target credit conditions by instrument type rather than through one undifferentiated total.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the academic literature documenting the breakdown of stable relationships among the aggregates (the Goodhart-style critiques), the statistical authorities' own series-break and redefinition records acknowledging instrument convergence, and measurement-studies scholarship noting that the category's documented birthday coincides with the definition rather than with any behavioral discontinuity. No party that profits from the distinction attests the founding problem is live.
narrative_ontology:disappearance_verdict(electronic_money_emergence__m4_m5_collapse_reading, world_rearranges).
narrative_ontology:founding_problem_status(electronic_money_emergence__m4_m5_collapse_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(electronic_money_emergence__m4_m5_collapse_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(electronic_money_emergence__m4_m5_collapse_reading, 'none', 1).
narrative_ontology:epsilon_provenance(electronic_money_emergence__m4_m5_collapse_reading, 0.38, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(electronic_money_emergence__m4_m5_collapse_reading_tests).
:- end_tests(electronic_money_emergence__m4_m5_collapse_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate and diffuse (0.38): the boundary taxes analytical accuracy (estimates absorb misclassification), compliance labor (returns must classify straddling instruments), and historical fidelity (the record's category birthday is the definition). Nothing concentrates: no seat's livelihood depends on the boundary, so extraction per seat is small and widely spread. Suppression is low (0.26) and structural rather than coercive — alternatives are legal, partially built (weighted user-cost aggregates circulate in research), and the holding force is data-infrastructure path dependence plus journal/replication convention, not prohibition. Theater is the signature reading (0.58 and rising): a growing majority of ongoing activity is publishing series whose distinguishing line no longer binds, footnoting breaks, and ritually revising definitions — the performance of measurement rather than measurement that tracks anything. Accessibility collapse is low-moderate (0.34): understanding the convention does not close alternatives, since parallel aggregates remain constructible; it only raises their cost. Resistance is muted (0.30): professional critique exists, review proposals recur, but the costs are too diffuse to sustain a campaign, and the natural coalition (professional associations commissioning a rival series) is blunted by coordination costs and the absence of any acute harm to coordinate against. The two tracked metrics share one time grid ({0, 8, 16, 24, 32, 40}); suppression_requirement is deliberately not tracked because the enforcement picture is static — the arrangement is held by inertia, not by machinery whose buildup or decay this story traces.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the agenda-setter's position the boundary is neutral craft: a definition maintained competently, its breaks documented, its revisions defensible — the administrative seat should compute near-symmetric, low-stakes. From the payer seats the same structure operates as inherited distortion: economists absorb misclassification into estimates, institutions burn compliance labor on judgments the line forces, historians date a category to its measurement. The beneficiary seat experiences service, not burden. Among same-power actors, monetary_economists and divisia_alternative_advocates hold the same nominal power atom but diverge on exit: the former are constrained by replication convention and official-data dependence, the latter have already built parallel infrastructure and are mobile — constraint-specific factors, not global standing, differentiate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. longitudinal_monetary_analysts sit near the beneficiary end (d low, effective extraction damped or inverted into subsidy) — they receive continuity and can leave freely. The three victim groups sit toward the target end (d high, extraction amplified), with reporting_financial_institutions least trapped in effect (the burden is real but shallow) and economic_historians_of_money most exposed relative to their power (they inherit the artifact with the least capacity to rebuild the record). The agenda-setter derives near-symmetric: it administers, collects no rents, and bears reputational exposure to the artifact critique — no override is authored because there is no capture for the derivation to miss. aggregate_consuming_policy_makers are genuinely dual-positioned and land near symmetric. divisia_alternative_advocates, though excluded and critical, derive beneficiary-side by exit: their mobile position means the boundary costs them little they cannot route around.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reading segregated instrument classes for policy targeting — died when dematerialization dissolved the segregation it presupposed; the arrangement persists on the series-continuity imperative alone. The classification guards against two opposite errors. Calling this a snare would require a capturer: the receipt surface is affirmatively diffuse, and the audit omega checks for quiet capture precisely because a found capturer would flip the verdict. Calling it a rope would require a live coordination function: what remains (series continuity) is real but residual, consumed passively by agents who bear none of the maintenance. The cost-asymmetry test carries the verdict: the administrator could redraw or retire the boundary tomorrow, but the fix (rebasing decades of series, breaking international comparability, invalidating longitudinal panels) costs it far more than the boundary costs it — so it persists by inertia, maintained theatrically. The R5 mismatch consumer should find coherence here: founding_problem_status dead combined with disappearance_verdict world_rearranges flags the zombie pattern, which cross-checks against the rising theater trajectory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_truth_maker_disagreement,
    'This constraint is one reading of the electronic_money_emergence kernel: it denies any genuine emergence event and locates the category''s origin in the M4/M5 measurement act, while sibling readings locate genuine pre-measurement events (technical/social thinkability; first institutional bearer). Which truth-maker does the historical record support?',
    'Comparative archival analysis: pre-definition contracts, accounting practice, and market commentary examined for whether dematerialized balances were treated as a distinct kind of money before the statistical definition existed, and whether any behavioral discontinuity coincides with a date other than the statistical one.',
    'If a genuine pre-measurement event is established, this reading reduces to a dating quarrel, its epsilon loses its artifact grounding, and the inertial-persistence account gives way to whichever sibling reading survives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_truth_maker_disagreement, conceptual, 'Committer-frame omega: which reading''s truth-maker for ''emergence'' the record supports.').

omega_variable(
    retroactive_creation_timing,
    'Did the M4/M5 boundary create the electronic-money category retroactively, or did it merely attach an official label to a category already forming in market practice?',
    'Document-trail analysis of the period before definition: whether contemporaries distinguished the instrument class in practice, or whether the first systematic distinction appears only in the statistical publications themselves.',
    'Genuine retroactive creation supports the artifact reading and the measured extraction profile; mere labeling lowers effective extraction and shifts weight toward residual-coordination accounts of the boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retroactive_creation_timing, empirical, 'Whether category formation preceded or followed the statistical definition.').

omega_variable(
    residual_continuity_value,
    'How much of the distinction''s remaining value is genuine coordination (an unbroken comparable series serving longitudinal analysis) versus pure inertia?',
    'Counterfactual usage study: estimate the analytical loss if the series were discontinued or rebased — citation dependence, replication breakage, contractual and supervisory references to the defined aggregates.',
    'Substantial continuity value pulls the computed classification toward coordination-with-overhead; negligible value confirms persistence by inertia and validates the theater-dominated profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(residual_continuity_value, empirical, 'Coordination residue versus inertia in the maintained distinction.').

omega_variable(
    quiet_capture_audit,
    'Does any seat quietly capture gains from maintaining the distinction — statistical-division budget justification, data-vendor resale margins, consultancy built around series breaks and reconciliations?',
    'Budget and vendor-revenue audit tied specifically to the boundary''s maintenance rather than to statistical production generally; disclosure of who funds and who profits from break-documentation activity.',
    'A demonstrated capturer would move the arrangement from diffuse-gain inertia toward captured extraction, changing the receipt surface from diffuse to a named seat and shifting the computed type accordingly.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(quiet_capture_audit, empirical, 'Whether maintenance produces a hidden concentrated beneficiary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(electronic_money_emergence__m4_m5_collapse_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(emm_collapse_tr_t0, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(emm_collapse_tr_t8, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 8, 0.26).
narrative_ontology:measurement(emm_collapse_tr_t16, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 16, 0.36).
narrative_ontology:measurement(emm_collapse_tr_t24, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 24, 0.45).
narrative_ontology:measurement(emm_collapse_tr_t32, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 32, 0.52).
narrative_ontology:measurement(emm_collapse_tr_t40, electronic_money_emergence__m4_m5_collapse_reading, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(emm_collapse_be_t0, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(emm_collapse_be_t8, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 8, 0.27).
narrative_ontology:measurement(emm_collapse_be_t16, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 16, 0.31).
narrative_ontology:measurement(emm_collapse_be_t24, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 24, 0.35).
narrative_ontology:measurement(emm_collapse_be_t32, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 32, 0.37).
narrative_ontology:measurement(emm_collapse_be_t40, electronic_money_emergence__m4_m5_collapse_reading, base_extractiveness, 40, 0.38).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(electronic_money_emergence__m4_m5_collapse_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(electronic_money_emergence__m4_m5_collapse_reading, information_standard).
narrative_ontology:affects_constraint(electronic_money_emergence__m4_m5_collapse_reading, electronic_money_emergence__became_thinkable_reading).
narrative_ontology:affects_constraint(electronic_money_emergence__m4_m5_collapse_reading, electronic_money_emergence__first_held_reading).
narrative_ontology:affects_constraint(electronic_money_emergence__m4_m5_collapse_reading, uk_broad_money_operational_targeting).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial question 'when did electronic money emerge?' decomposes into three structurally distinct claims sharing the kernel electronic_money_emergence. This story is the collapse reading (category constructed retroactively by measurement; no genuine event). The became_thinkable reading (emergence at thinkability, upstream of measurement) and the first_held reading (emergence at first institutional bearer) are separate files with their own epsilon and victim sets. Family linkage runs through network.affects_constraints in all three; the upstream thinkability claim is typically cited as evidence by the event-dating claims, while this reading undercut both by relocating the category's origin into the measurement apparatus. uk_broad_money_operational_targeting is the downstream policy regime that consumed the aggregates while the boundary still bound.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
