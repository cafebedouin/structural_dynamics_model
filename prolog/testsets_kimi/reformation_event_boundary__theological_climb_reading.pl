% ============================================================================
% CONSTRAINT STORY: reformation_event_boundary__theological_climb_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   human_readable: Theological Climb Reading of the Reformation
 *   domain: historical/religious/commitment_system
 *
 * SUMMARY:
 *   Theological climb reading of the Reformation kernel: Luther's recovery of
 *   justification by faith alone is held as a rediscovery of eternal truth
 *   embedded in scripture, requiring institutional separation from Rome. This
 *   reading treats the event as a Mountainâdivine truth emerging naturally
 *   from a fixed textâbut historical analysis reveals identifiable
 *   beneficiaries (Protestant believers and reformers gaining identity and
 *   authority) and victims (Catholic hierarchy losing legitimacy and
 *   resources). The constraint operates as a commitment system with scripture
 *   as fixed text and lineage authority through reformist tradition,
 *   generating strong seat divergence between the faithful, the displaced
 *   hierarchy, and the analytical observer.
 *
 * KEY AGENTS:
 *   - protestant_reformers: agenda_setter (organized/civilizational/identity_locked) â set the theological agenda and gain institutional authority from the separation
 *   - protestant_believers: beneficiary (moderate/civilizational/identity_locked) â receive spiritual relief and new religious identity
 *   - catholic_hierarchy: payer (institutional/civilizational/constrained) â bear the cost of lost authority, territory, and legitimacy
 *   - secular_princes: excluded (powerful/biographical/mobile) â political and economic motives are erased from the theological narrative
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_event_boundary__theological_climb_reading, 0.6).
domain_priors:suppression_score(reformation_event_boundary__theological_climb_reading, 0.7).
domain_priors:theater_ratio(reformation_event_boundary__theological_climb_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_event_boundary__theological_climb_reading, mountain).
narrative_ontology:human_readable(reformation_event_boundary__theological_climb_reading, "Theological Climb Reading of the Reformation").
narrative_ontology:topic_domain(reformation_event_boundary__theological_climb_reading, "historical/religious/commitment_system").

domain_priors:emerges_naturally(reformation_event_boundary__theological_climb_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_event_boundary__theological_climb_reading, '66cd0e0a-842e-4b03-a81c-6fdf2006809b').
narrative_ontology:cs_kernel_codification('66cd0e0a-842e-4b03-a81c-6fdf2006809b', fixed_text).
narrative_ontology:cs_authority_grounding('66cd0e0a-842e-4b03-a81c-6fdf2006809b', lineage).
narrative_ontology:cs_interpretation_layer_present('66cd0e0a-842e-4b03-a81c-6fdf2006809b').
narrative_ontology:cs_reading_relation('66cd0e0a-842e-4b03-a81c-6fdf2006809b', reformation_event_boundary__political_swap_reading, coexists_with).
narrative_ontology:cs_reading_relation('66cd0e0a-842e-4b03-a81c-6fdf2006809b', reformation_event_boundary__composite_overdetermination_reading, influences).
narrative_ontology:cs_axiom('66cd0e0a-842e-4b03-a81c-6fdf2006809b', foundational, justification_by_faith_alone).
narrative_ontology:cs_axiom_status(justification_by_faith_alone, holdable).
narrative_ontology:cs_axiom_grounding('66cd0e0a-842e-4b03-a81c-6fdf2006809b', justification_by_faith_alone, theological).
narrative_ontology:cs_axiom('66cd0e0a-842e-4b03-a81c-6fdf2006809b', foundational, scripture_self_interpreting_authority).
narrative_ontology:cs_axiom_status(scripture_self_interpreting_authority, holdable).
narrative_ontology:cs_axiom_grounding('66cd0e0a-842e-4b03-a81c-6fdf2006809b', scripture_self_interpreting_authority, theological).
narrative_ontology:cs_reference_frame('66cd0e0a-842e-4b03-a81c-6fdf2006809b', sola_fide_authority).
narrative_ontology:cs_drift_state('66cd0e0a-842e-4b03-a81c-6fdf2006809b', peace_of_augsburg_1555, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('66cd0e0a-842e-4b03-a81c-6fdf2006809b', '').
narrative_ontology:cs_kernel_id(reformation_event_boundary__theological_climb_reading, reformation_event_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_event_boundary__theological_climb_reading, protestant_believers).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__theological_climb_reading, protestant_reformers).
narrative_ontology:constraint_victim(reformation_event_boundary__theological_climb_reading, catholic_hierarchy).
narrative_ontology:constraint_vindicates(reformation_event_boundary__theological_climb_reading, sola_fide).
narrative_ontology:constraint_vindicates(reformation_event_boundary__theological_climb_reading, sola_scriptura).
narrative_ontology:constraint_vindicates(reformation_event_boundary__theological_climb_reading, priesthood_of_all_believers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Monks, professors, and pastors who articulated the doctrine of justification by faith alone, broke with papal authority, and built new ecclesial structures. They set the theological agenda, translated scripture into vernacular languages, and gained institutional authority from the separation. Their personal and vocational identity is fused with the truth-claim of the reform; recantation would mean self-annihilation.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, protestant_reformers, agenda_setter,
    organized, civilizational, identity_locked, continental).

% Laypeople and lower clergy who adopted the reformed soteriology, receiving spiritual relief from sacramental anxiety and the promise of unmediated divine grace. Their religious identity reorganizes around the new theological frame; exit means returning to Catholic practice or apostasy, which carries social, political, and perceived spiritual costs.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, protestant_believers, beneficiary,
    moderate, civilizational, identity_locked, regional).

% Papal curia, bishops, and religious orders who lost moral authority, territorial control, and economic resources to Protestant rulers and congregations. They bear the cost of the theological correction narrative, which labels their sacramental system as corrupt. Their resistance took the form of excommunication, the Index, and the Counter-Reformation, but full exit from the constraint would require abandoning the institutional identity of the Roman Church.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, catholic_hierarchy, payer,
    institutional, civilizational, constrained, global).

% Territorial rulers whose political and economic motives for breaking with Romeâasset seizures, dynastic autonomy from emperor and popeâare backgrounded by the theological reading. They would argue their interests were primary drivers of the separation, but the theological frame renders their voice absent from the narrative of pure doctrinal breakthrough. They retained the mobility to choose sides based on interest.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, secular_princes, excluded,
    powerful, biographical, mobile, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reformation_event_boundary__theological_climb_reading, protestant_reformers).
narrative_ontology:fixing_cost_class(reformation_event_boundary__theological_climb_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates Protestant believers around a shared soteriologyâsalvation by grace through faith alone, with scripture as the sole infallible authorityâsolving the problem of doctrinal uncertainty, clerical mediation, and sacramental anxiety.
% TRANSFER_FUNCTION: Moves religious authority, legitimacy, and material resources from the papal hierarchy and sacramental priesthood to individual believers, local congregations, and reformist clergy through the medium of vernacular scripture and faith-alone soteriology.
% ABSENT_VOICES: Secular rulers seeking political autonomy and church assets, Catholic theologians defending sacramental theology and papal supremacy, and peasants seeking social revolution are backgrounded or excluded by the theological framing; their objections appear as distractions from the pure doctrinal narrative.
% DISAPPEARANCE_RATIONALE: If the theological reading vanished overnight, Protestant institutions would lose their claim to represent a rediscovery of eternal truth; the schism would read as purely political or social, and the distinctive identity of Lutheran and Reformed churches would rearrange around alternative legitimations or reunification pressures.
% FOUNDING_PROBLEM: The medieval Western church had allegedly corrupted the gospel by adding human works and institutional mediation to salvation, creating spiritual anxiety and doctrinal error among the faithful.
% FOUNDING_PROBLEM_CORROBORATION: No corroboration from outside the benefiting parties exists; Catholic hierarchy and modern secular historiography both treat the 'corruption' narrative as polemical construction rather than historical fact.
narrative_ontology:disappearance_verdict(reformation_event_boundary__theological_climb_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_event_boundary__theological_climb_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_event_boundary__theological_climb_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(reformation_event_boundary__theological_climb_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_event_boundary__theological_climb_reading, 0.6, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_event_boundary__theological_climb_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reformation_event_boundary__theological_climb_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, ExtMetricName, E),
    domain_priors:suppression_score(reformation_event_boundary__theological_climb_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(reformation_event_boundary__theological_climb_reading),
    narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(reformation_event_boundary__theological_climb_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.60) reflects the substantial transfer of authority, legitimacy, and resources from Catholic to Protestant institutions over the interval. Suppression (0.70) captures the active enforcementâimperial bans, excommunications, warsârequired to maintain the theological reading against Catholic and political alternatives. Theater_ratio (0.30) is moderate: the theological commitment is genuinely held, but a growing share of polemical output serves institutional boundary maintenance rather than spiritual inquiry. Accessibility_collapse (0.88) is high because within the Protestant frame, sacramental theology collapses as a live alternative once sola fide is accepted. Resistance (0.72) reflects sustained Counter-Reformation and imperial opposition. The metrics describe an arrangement that reads as Mountain from the believer's seat but shows the extraction signature of a contested construct when viewed analytically.
 *
 * PERSPECTIVAL GAP:
 *   From the reformer's seat, the constraint is a Mountainâeternal truth recovered. From the Catholic seat, it is extraction and schism. From the analytical seat, the divergence itself is the measurement: a claimed Mountain with concentrated beneficiaries and active suppression is a false-summit candidate. The engine computes different per-seat types from these structural positions; the authored claim does not adjudicate the disagreement.
 *
 * DIRECTIONALITY LOGIC:
 *   Protestant_reformers are structural beneficiaries and agenda-setters (d near 0.0): the constraint subsidizes their authority and defines their mission. Protestant_believers are beneficiaries (d near 0.2): they receive identity and soteriology, though they also supply loyalty and resources. Catholic_hierarchy is the target (d near 1.0): the constraint extracts legitimacy, territory, and institutional coherence from them. Secular_princes sit outside the beneficiary/victim structure of the theological reading; their exclusion is structural to the reading's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate was to recover the gospel from papal corruption. By 1555, the Peace of Augsburg had territorialized the split, suggesting the theological mandate had drifted into political institutionalization. However, the reading itself does not acknowledge this mandatrophy; it treats the political settlement as secondary to the theological achievement. The classification catches this by flagging the gap between founding_problem_status (contested) and disappearance_verdict (world_rearranges): the arrangement persists and rearranges the world even if its founding problem is historically contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_truth_vs_constructed_narrative,
    'Is the Reformation''s theological reading a genuine rediscovery of eternal divine truth embedded in scripture, or a constructed narrative that legitimates institutional separation and benefits Protestant identity?',
    'Comparative historical analysis of late-medieval theological sources to determine whether Luther''s reading was recovery, innovation, or strategic construction; sociological analysis of the beneficiary structure to test for concentrated institutional gains.',
    'If the reading is constructed, the constraint is a false summit (likely tangled_rope or snare) rather than a mountain, and the extraction metrics indicate rent-bearing narrative rather than necessary cost of truth. If genuine, the high accessibility_collapse and suppression are reclassified as the friction of truth encountering error.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_truth_vs_constructed_narrative, empirical, 'Whether the theological climb reading represents divine natural law or a constructed historiographical constraint.').

omega_variable(
    causal_primacy_ambiguity,
    'Does the theological reading''s exclusion of secular political motives constitute legitimate foregrounding of primary causes, or structural suppression of an alternative causal account that would redistribute beneficiary and victim designations?',
    'Archival analysis of princely correspondence and diets alongside theological publications to weight the relative timing and causal force of political versus theological commitments in the decision to break with Rome.',
    'If political motives were primary and theology secondary, the beneficiary structure inverts: secular princes become agenda-setters and beneficiaries, while reformers become ideological instruments. This would reclassify the constraint toward political_swap or tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_primacy_ambiguity, conceptual, 'Whether theology or politics was the primary driver, and how that ambiguity shapes the reading''s structural classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_event_boundary__theological_climb_reading, 0, 38).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ref_theo_climb_tr_t0, reformation_event_boundary__theological_climb_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(ref_theo_climb_tr_t5, reformation_event_boundary__theological_climb_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement(ref_theo_climb_tr_t10, reformation_event_boundary__theological_climb_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(ref_theo_climb_tr_t15, reformation_event_boundary__theological_climb_reading, theater_ratio, 15, 0.22).
narrative_ontology:measurement(ref_theo_climb_tr_t20, reformation_event_boundary__theological_climb_reading, theater_ratio, 20, 0.24).
narrative_ontology:measurement(ref_theo_climb_tr_t25, reformation_event_boundary__theological_climb_reading, theater_ratio, 25, 0.26).
narrative_ontology:measurement(ref_theo_climb_tr_t30, reformation_event_boundary__theological_climb_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement(ref_theo_climb_tr_t38, reformation_event_boundary__theological_climb_reading, theater_ratio, 38, 0.3).

% Extraction over time
narrative_ontology:measurement(ref_theo_climb_be_t0, reformation_event_boundary__theological_climb_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(ref_theo_climb_be_t5, reformation_event_boundary__theological_climb_reading, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(ref_theo_climb_be_t10, reformation_event_boundary__theological_climb_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(ref_theo_climb_be_t15, reformation_event_boundary__theological_climb_reading, base_extractiveness, 15, 0.5).
narrative_ontology:measurement(ref_theo_climb_be_t20, reformation_event_boundary__theological_climb_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(ref_theo_climb_be_t25, reformation_event_boundary__theological_climb_reading, base_extractiveness, 25, 0.55).
narrative_ontology:measurement(ref_theo_climb_be_t30, reformation_event_boundary__theological_climb_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(ref_theo_climb_be_t38, reformation_event_boundary__theological_climb_reading, base_extractiveness, 38, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(ref_theo_climb_su_t0, reformation_event_boundary__theological_climb_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(ref_theo_climb_su_t5, reformation_event_boundary__theological_climb_reading, suppression_requirement, 5, 0.5).
narrative_ontology:measurement(ref_theo_climb_su_t10, reformation_event_boundary__theological_climb_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(ref_theo_climb_su_t15, reformation_event_boundary__theological_climb_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(ref_theo_climb_su_t20, reformation_event_boundary__theological_climb_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement(ref_theo_climb_su_t25, reformation_event_boundary__theological_climb_reading, suppression_requirement, 25, 0.75).
narrative_ontology:measurement(ref_theo_climb_su_t30, reformation_event_boundary__theological_climb_reading, suppression_requirement, 30, 0.73).
narrative_ontology:measurement(ref_theo_climb_su_t38, reformation_event_boundary__theological_climb_reading, suppression_requirement, 38, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(reformation_event_boundary__theological_climb_reading, political_swap_reading).
narrative_ontology:affects_constraint(reformation_event_boundary__theological_climb_reading, composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% The reformation_event_boundary kernel decomposes into three structurally distinct readings because the natural-language label 'the Reformation' conflates competing causal and normative claims. Each reading carries a different epsilon, beneficiary/victim structure, and directionality profile. The theological reading and political reading are mutually irreducible as claims of primacy; the composite reading incorporates both as overdetermined factors. All three stories are required to map the full constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
