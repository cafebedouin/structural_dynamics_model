% ============================================================================
% CONSTRAINT STORY: reformation_event_boundary__theological_climb_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_event_boundary__theological_climb_reading, []).

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
 *   constraint_id: reformation_event_boundary__theological_climb_reading
 *   human_readable: Reformation as Theological Climb: Justification-by-Faith Breakthrough Requiring Institutional Separation
 *   domain: historical_epistemology/religious_history/commitment_system_analysis
 *
 * SUMMARY:
 *   This story instantiates the theological_climb reading of the
 *   reformation_event_boundary kernel: the Reformation as primarily a genuine
 *   doctrinal breakthrough (justification by faith alone) that necessitated
 *   institutional separation from Rome. Under this reading, the Catholic
 *   Church is the party corrected by theology it could not absorb, believers
 *   who accept the doctrine are the beneficiaries of a truer soteriology, and
 *   the periodization runs tight, 1517 (Ninety-Five Theses) to 1555 (Peace of
 *   Augsburg, territorial confessional settlement). This is a deliberately
 *   partial reading: the sibling political_swap_reading holds that theology
 *   was post-hoc rationalization for princely asset seizure, and the sibling
 *   composite_overdetermination_reading holds that no single driver —
 *   theological, institutional, political, or proliferative — captures the
 *   event. Each sibling is a separate constraint story with its own epsilon;
 *   this file does not average across them or hedge its own epsilon to
 *   accommodate them, per the epsilon-invariance principle.
 *
 * KEY AGENTS:
 *   - reformed_believers: primary beneficiary group, freed from merit-based penitential anxiety but bound into new confessional enforcement
 *   - evangelical_clergy: agenda-setting seat administering the doctrinal break and its institutional consequences
 *   - territorial_protestant_princes: beneficiary/agenda-setter seat whose adoption this reading treats as doctrinally driven rather than merely opportunistic
 *   - roman_catholic_church: primary payer, the institution theologically corrected and materially dispossessed
 *   - religious_minorities_under_new_confessional_states: payer seat showing that doctrinal correction did not by itself produce toleration
 *   - dissenting_radical_reformers: payer seat showing the doctrine's own logic exceeded what the magisterial settlement would tolerate
 *   - historians_of_doctrine: analytical observer seat assessing whether the theological claim carried independent causal force
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_event_boundary__theological_climb_reading, 0.62).
domain_priors:suppression_score(reformation_event_boundary__theological_climb_reading, 0.71).
domain_priors:theater_ratio(reformation_event_boundary__theological_climb_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_event_boundary__theological_climb_reading, tangled_rope).
narrative_ontology:human_readable(reformation_event_boundary__theological_climb_reading, "Reformation as Theological Climb: Justification-by-Faith Breakthrough Requiring Institutional Separation").
narrative_ontology:topic_domain(reformation_event_boundary__theological_climb_reading, "historical_epistemology/religious_history/commitment_system_analysis").

domain_priors:requires_active_enforcement(reformation_event_boundary__theological_climb_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_event_boundary__theological_climb_reading, '04706f88-b92e-4cc0-a3d9-7b48a8d1e5c3').
narrative_ontology:cs_kernel_codification('04706f88-b92e-4cc0-a3d9-7b48a8d1e5c3', fixed_text).
narrative_ontology:cs_authority_grounding('04706f88-b92e-4cc0-a3d9-7b48a8d1e5c3', lineage).
narrative_ontology:cs_interpretation_layer_present('04706f88-b92e-4cc0-a3d9-7b48a8d1e5c3').
narrative_ontology:cs_reading_relation('04706f88-b92e-4cc0-a3d9-7b48a8d1e5c3', reformation_event_boundary__political_swap_reading, coexists_with).
narrative_ontology:cs_reading_relation('04706f88-b92e-4cc0-a3d9-7b48a8d1e5c3', reformation_event_boundary__composite_overdetermination_reading, influences).
narrative_ontology:cs_axiom('04706f88-b92e-4cc0-a3d9-7b48a8d1e5c3', foundational, sola_fide_constitutes_genuine_doctrinal_discovery).
narrative_ontology:cs_axiom_status(sola_fide_constitutes_genuine_doctrinal_discovery, holdable).
narrative_ontology:cs_axiom_grounding('04706f88-b92e-4cc0-a3d9-7b48a8d1e5c3', sola_fide_constitutes_genuine_doctrinal_discovery, deontological).
narrative_ontology:cs_axiom('04706f88-b92e-4cc0-a3d9-7b48a8d1e5c3', foundational, doctrinal_incompatibility_necessitated_institutional_separation).
narrative_ontology:cs_axiom_status(doctrinal_incompatibility_necessitated_institutional_separation, holdable).
narrative_ontology:cs_axiom_grounding('04706f88-b92e-4cc0-a3d9-7b48a8d1e5c3', doctrinal_incompatibility_necessitated_institutional_separation, conventional).
narrative_ontology:cs_reference_frame('04706f88-b92e-4cc0-a3d9-7b48a8d1e5c3', medieval_sacramental_merit_economy).
narrative_ontology:cs_drift_state('04706f88-b92e-4cc0-a3d9-7b48a8d1e5c3', reformation_confessional_settlement_1555, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('04706f88-b92e-4cc0-a3d9-7b48a8d1e5c3', '').
narrative_ontology:cs_kernel_id(reformation_event_boundary__theological_climb_reading, reformation_event_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_event_boundary__theological_climb_reading, reformed_believers).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__theological_climb_reading, evangelical_clergy).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__theological_climb_reading, territorial_protestant_princes).
narrative_ontology:constraint_victim(reformation_event_boundary__theological_climb_reading, roman_catholic_church).
narrative_ontology:constraint_victim(reformation_event_boundary__theological_climb_reading, religious_minorities_under_new_confessional_states).
narrative_ontology:constraint_victim(reformation_event_boundary__theological_climb_reading, dissenting_radical_reformers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Laypeople who accept that justification comes by faith alone, not through indulgences, penance regimes, or clerical mediation. They are freed from a purchased-grace economy but bound into new confessional identities enforced by territorial churches and civil law; leaving the new church carries social and sometimes legal cost.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, reformed_believers, beneficiary,
    powerless, generational, identity_locked, regional).

% Former priests and new pastors who adopt and teach the doctrine, write catechisms, and administer the break from Rome. They gain doctrinal authority and, in many territories, state salary and protection, in exchange for enforcing confessional conformity on their congregations.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, evangelical_clergy, agenda_setter,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(reformation_event_boundary__theological_climb_reading, evangelical_clergy, beneficiary).

% Rulers who adopt the new theology as the territorial confession, gaining control over church property, appointments, and moral authority previously held by Rome. Whether their embrace of the doctrine is sincere or opportunistic is exactly what the sibling political-swap reading disputes; this reading holds that the doctrine's genuine force is what made their adoption possible, not merely convenient.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, territorial_protestant_princes, beneficiary,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(reformation_event_boundary__theological_climb_reading, territorial_protestant_princes, agenda_setter).

% Loses papal jurisdiction, tithe income, monastic property, and doctrinal monopoly across entire territories. From this reading's lights, the Church is the party theologically corrected — its sacramental economy of merit and indulgence is displaced by a doctrine it cannot absorb without ceasing to be itself, which is why separation rather than internal reform was the outcome.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, roman_catholic_church, payer,
    institutional, civilizational, trapped, continental).

% Catholics remaining in newly Protestant territories, and vice versa, who face confiscation, exile, or worse under the era's confessionalization. The new doctrinal settlement enforces conformity through the same coercive machinery it displaced, just under new management.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, religious_minorities_under_new_confessional_states, payer,
    powerless, biographical, trapped, regional).

% Anabaptists, spiritualists, and others who took sola fide further than the magisterial reformers intended, rejecting infant baptism or civil church authority. They are suppressed by both Catholic and mainstream Protestant authorities alike, showing that the doctrinal breakthrough this reading centers did not by itself determine institutional tolerance.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, dissenting_radical_reformers, payer,
    powerless, biographical, trapped, regional).

% Scholars who assess whether Luther's doctrinal claim was a genuine theological discovery with independent causal force, or whether it functioned primarily as legitimating vocabulary for pre-existing political and economic pressures — the question this reading answers one way and the sibling readings answer differently.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, historians_of_doctrine, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reformation_event_boundary__theological_climb_reading, diffuse).
narrative_ontology:fixing_cost_class(reformation_event_boundary__theological_climb_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves a genuine soteriological crisis within Western Christendom: the late-medieval penitential and indulgence system had generated widespread anxiety about the certainty of salvation. Sola fide offers a coherent doctrinal answer — assurance grounded in faith and grace rather than accumulated merit — that a large population of believers and clergy found more theologically defensible than the system it replaced.
% TRANSFER_FUNCTION: Moves doctrinal authority, ecclesiastical property, and jurisdictional control from the Roman hierarchy to territorial churches and their evangelical clergy; moves psychological and social certainty from a merit-based sacramental economy to a faith-based one for believers who accept the new doctrine, while transferring the cost of nonconformity onto those who do not.
% ABSENT_VOICES: Catholic theologians who held that the doctrinal dispute was resolvable within a reformed Catholic framework (the position eventually taken at Trent on some points) are largely written out of this reading's tight periodization, which treats separation as doctrinally necessitated rather than as one contested outcome among possible reconciliations. Radical reformers who pushed sola fide toward anti-magisterial conclusions are also marginal to this reading's account, which centers the magisterial Wittenberg-to-territorial-church line.
% DISAPPEARANCE_RATIONALE: If the claim that sola fide was a genuine, independently-forced doctrinal breakthrough disappeared as an organizing frame for this history, the entire moral architecture of separation-as-necessity would need re-founding: the sixteenth century's confessional boundaries, the theological self-justification of Protestant state churches, and the historiographical periodization anchored to 1517-1555 would all require re-derivation from other grounds (political, economic, or composite).
% FOUNDING_PROBLEM: The felt inadequacy of the medieval penitential system to deliver assurance of salvation, intensified by the indulgence controversy, which Luther's doctrine of justification by faith alone was developed to resolve.
% FOUNDING_PROBLEM_CORROBORATION: Confessional Protestant theologians and denominational historians attest the problem was genuinely theological and remains foundational to Reformation-descended church identity. Historians outside any confessional commitment (including social and economic historians of the period) attest that whatever the theological problem's reality, its resolution into institutional separation tracked princely property interests and existing political fault lines at least as closely as it tracked doctrinal logic — corroboration for the theological-necessity claim specifically comes primarily from within the Protestant theological tradition itself, which is exactly the caution this reading's own axioms must carry.
narrative_ontology:disappearance_verdict(reformation_event_boundary__theological_climb_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_event_boundary__theological_climb_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_event_boundary__theological_climb_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(reformation_event_boundary__theological_climb_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_event_boundary__theological_climb_reading, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_event_boundary__theological_climb_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reformation_event_boundary__theological_climb_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reformation_event_boundary__theological_climb_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.62 by interval end: substantial, because even under this reading's own charitable framing of the doctrinal breakthrough, the institutional separation it necessitated transferred real property, jurisdiction, and coercive authority from Rome to territorial churches and princes, and imposed real costs on minorities and radicals who did not fit the new settlement. It is not maximal because the coordination function (resolving a genuine, widely-felt soteriological crisis) is authored as real under this reading, not merely cover. Suppression rises sharply across the interval (0.30 to 0.71) tracking the actual historical trajectory from theological dispute (1517-1521) through armed conflict and princely enforcement (1530-1548) to confessional territorialization backed by state coercion (Peace of Augsburg, 1555) — cuius regio, eius religio formalizes suppression as the settlement mechanism. Theater ratio stays low (0.08 to 0.20) because this reading holds the doctrinal content to be substantively operative throughout, not predominantly performative cover for other aims — that judgment is itself the reading's central commitment and is exactly what the political_swap sibling denies.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter seats (evangelical_clergy, territorial_protestant_princes) and the payer seat (roman_catholic_church) diverge sharply under this reading's own structural data: from the clergy and princely seats, the arrangement is a genuine doctrinal correction that required and justified separation; from the Church's seat, the same arrangement is confiscation and schism dressed in theological necessity it does not accept as necessary. The engine computes this divergence from power, exit options, and the declared beneficiary/victim structure — this reading does not adjudicate which seat is right, only that the divergence is structurally real given the declared parties.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformed believers and evangelical clergy sit toward the beneficiary end: the doctrine's coordination function (assurance, coherent soteriology) accrues to them directly. Territorial princes sit toward the beneficiary end with institutional power and arbitrage exit — they can and did play confessional politics across territories. The Roman Catholic Church sits at the full-target end: trapped by continental scope and civilizational time horizon, it cannot exit a controversy that strips its own jurisdiction. Religious minorities and radical dissenters sit at the full-target end for a different reason: they are powerless and geographically trapped within the new confessional states, bearing suppression from an arrangement that claims to have liberated believers generally but did not liberate them specifically.
 *
 * MANDATROPHY ANALYSIS:
 *   The tight periodization (1517-1555) and founding_problem_status of 'contested' work together to prevent this reading from either canonizing the doctrinal-necessity narrative uncritically or dismissing it as pure rationalization. The founding problem (assurance under a penitential system in crisis) is authored as having been genuinely live in 1517; whether the institutional separation that followed was the necessary consequence of that problem, or one contingent resolution among several, is left open and is precisely the fault line between this reading and its two siblings. Declaring the Church a 'victim of theological correction' under this reading's own lights does not require endorsing that framing as historically exhaustive — it requires only that this reading's internal logic be stated cleanly and left to compete with the siblings rather than blended with them.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    doctrinal_independence_from_political_interest,
    'Did Luther''s doctrine of justification by faith alone possess independent causal force sufficient to produce institutional separation, or did its adoption track princely material interest so closely that the doctrine functioned as legitimating vocabulary rather than a driver?',
    'Comparative analysis of territories where doctrinal adoption ran contrary to or absent clear princely material benefit, versus territories where adoption tracked property and jurisdictional seizure tightly; correspondence and internal deliberative records of princes and reformers regarding motive.',
    'If doctrinal adoption is shown to track material interest near-perfectly across territories, this reading''s central claim weakens substantially in favor of the political_swap sibling; if adoption is shown in multiple cases to run against or independent of princely material interest, this reading''s claim strengthens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(doctrinal_independence_from_political_interest, conceptual, 'Whether the theological claim had independent causal force or was legitimating vocabulary for political-economic drivers.').

omega_variable(
    reading_boundary_periodization,
    'Does the tight 1517-1555 periodization this reading adopts correctly bound the theological-innovation event, or does confining the frame to those dates artificially exclude the longer institutional and political processes (pre-1517 anticlerical sentiment, post-1555 confessionalization and denominational proliferation) that the composite_overdetermination_reading holds are inseparable from the doctrinal story?',
    'Compare causal-chain analyses that extend the window backward (to 15th-century conciliarism and anticlerical reform movements) and forward (to the Thirty Years'' War and beyond) against analyses bounded to 1517-1555; assess whether extending the window changes attribution of causal weight to doctrine versus institutional/political factors.',
    'A periodization that survives window-extension without changing causal attribution supports this reading''s tight framing; a periodization that only supports the theological-climb narrative within an artificially narrow window suggests the composite reading''s broader frame is more descriptively adequate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_boundary_periodization, conceptual, 'Whether the tight periodization this reading adopts is a genuine event boundary or an artifact of the reading''s own framing choice.').

omega_variable(
    victim_status_of_institutional_church,
    'Is it structurally coherent to characterize the Roman Catholic Church, a powerful continental institution, as a ''victim'' of theological correction in the same sense that powerless religious minorities and radical dissenters are victims of confessional suppression?',
    'Distinguish institutional loss of jurisdiction/property/doctrinal monopoly (which the Church experienced as a powerful institutional actor with its own continuing exit options and counter-reformation capacity) from the trapped, powerless suppression experienced by minorities and radicals; assess whether collapsing both into ''payer'' role obscures a real difference in kind.',
    'If the difference in kind is significant, the stakeholder model may need to differentiate institutional-level payer status (Church) from individual-level payer status (minorities, radicals) more sharply than a shared ''payer'' role captures, though the schema''s role vocabulary does not currently provide that granularity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_status_of_institutional_church, conceptual, 'Whether ''payer'' role adequately distinguishes institutional-level loss from individual-level suppression within the same reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_event_boundary__theological_climb_reading, 1517, 1555).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t1517, reformation_event_boundary__theological_climb_reading, theater_ratio, 1517, 0.08).
narrative_ontology:measurement(refo_tr_t1521, reformation_event_boundary__theological_climb_reading, theater_ratio, 1521, 0.1).
narrative_ontology:measurement(refo_tr_t1530, reformation_event_boundary__theological_climb_reading, theater_ratio, 1530, 0.14).
narrative_ontology:measurement(refo_tr_t1540, reformation_event_boundary__theological_climb_reading, theater_ratio, 1540, 0.17).
narrative_ontology:measurement(refo_tr_t1548, reformation_event_boundary__theological_climb_reading, theater_ratio, 1548, 0.19).
narrative_ontology:measurement(refo_tr_t1555, reformation_event_boundary__theological_climb_reading, theater_ratio, 1555, 0.2).

% Extraction over time
narrative_ontology:measurement(refo_be_t1517, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1517, 0.35).
narrative_ontology:measurement(refo_be_t1521, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1521, 0.44).
narrative_ontology:measurement(refo_be_t1530, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1530, 0.53).
narrative_ontology:measurement(refo_be_t1540, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1540, 0.58).
narrative_ontology:measurement(refo_be_t1548, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1548, 0.6).
narrative_ontology:measurement(refo_be_t1555, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1555, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t1517, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1517, 0.3).
narrative_ontology:measurement(refo_su_t1521, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1521, 0.42).
narrative_ontology:measurement(refo_su_t1530, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1530, 0.55).
narrative_ontology:measurement(refo_su_t1540, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1540, 0.64).
narrative_ontology:measurement(refo_su_t1548, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1548, 0.68).
narrative_ontology:measurement(refo_su_t1555, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1555, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(reformation_event_boundary__theological_climb_reading, political_swap_reading).
narrative_ontology:affects_constraint(reformation_event_boundary__theological_climb_reading, composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the reformation_event_boundary kernel. theological_climb_reading (this file) authors the Reformation as primarily a genuine doctrinal breakthrough necessitating separation, with the Catholic Church as theologically-corrected payer and believers as beneficiaries, on a tight 1517-1555 periodization. political_swap_reading authors the same historical episode as primarily a political-economic realignment in which theology functioned as post-hoc legitimation for princely seizure of ecclesiastical assets and jurisdiction. composite_overdetermination_reading declines to assign primacy to any single driver, treating theological, institutional, political, and proliferative dimensions as irreducibly simultaneous. Each carries its own epsilon, its own beneficiary/victim structure, and its own claimed_type; they are linked here rather than merged, per the epsilon-invariance principle — measuring 'the Reformation' by its doctrinal content versus its property/jurisdiction transfers versus its full composite yields structurally different constraints, not one constraint under different observables.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
