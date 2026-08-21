% ============================================================================
% CONSTRAINT STORY: vatican_ii_authority__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_authority__composite_overdetermination_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: vatican_ii_authority__composite_overdetermination_reading
 *   human_readable: Vatican II as Composite Overdetermination of Doctrinal Shifts
 *   domain: theology/ecclesiology/religious_authority
 *
 * SUMMARY:
 *   This constraint models the 'composite overdetermination' reading of
 *   Vatican II, which posits that the council's documents are not a single
 *   coherent event but an amalgamation of distinct doctrinal shifts with
 *   incompatible theological rationales. This produces structural ambiguity
 *   that cannot be resolved into either a 'continuity' or 'rupture'
 *   framework. This reading benefits scholars who thrive on complexity and
 *   critical analysis, while challenging the institutional Magisterium's
 *   claim to univocal interpretation and creating confusion for the faithful.
 *   Post-conciliar conflicts are seen as structural, not accidental, outcomes
 *   of this inherent overdetermination.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_authority__composite_overdetermination_reading, 0.65).
domain_priors:suppression_score(vatican_ii_authority__composite_overdetermination_reading, 0.7).
domain_priors:theater_ratio(vatican_ii_authority__composite_overdetermination_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(vatican_ii_authority__composite_overdetermination_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_authority__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_authority__composite_overdetermination_reading, "Vatican II as Composite Overdetermination of Doctrinal Shifts").
narrative_ontology:topic_domain(vatican_ii_authority__composite_overdetermination_reading, "theology/ecclesiology/religious_authority").

domain_priors:requires_active_enforcement(vatican_ii_authority__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_authority__composite_overdetermination_reading, '587cc5d0-e158-4416-8438-387c5a4272c4').
narrative_ontology:cs_kernel_codification('587cc5d0-e158-4416-8438-387c5a4272c4', fixed_text).
narrative_ontology:cs_authority_grounding('587cc5d0-e158-4416-8438-387c5a4272c4', lineage).
narrative_ontology:cs_interpretation_layer_present('587cc5d0-e158-4416-8438-387c5a4272c4').
narrative_ontology:cs_reading_relation('587cc5d0-e158-4416-8438-387c5a4272c4', vatican_ii_authority__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('587cc5d0-e158-4416-8438-387c5a4272c4', vatican_ii_authority__rupture_reading, coexists_with).
narrative_ontology:cs_axiom('587cc5d0-e158-4416-8438-387c5a4272c4', foundational, doctrinal_overdetermination_is_structural).
narrative_ontology:cs_axiom_status(doctrinal_overdetermination_is_structural, holdable).
narrative_ontology:cs_axiom_grounding('587cc5d0-e158-4416-8438-387c5a4272c4', doctrinal_overdetermination_is_structural, empirically_contingent).
narrative_ontology:cs_axiom('587cc5d0-e158-4416-8438-387c5a4272c4', foundational, compromise_produces_incompatible_rationales).
narrative_ontology:cs_axiom_status(compromise_produces_incompatible_rationales, holdable).
narrative_ontology:cs_axiom_grounding('587cc5d0-e158-4416-8438-387c5a4272c4', compromise_produces_incompatible_rationales, empirically_contingent).
narrative_ontology:cs_reference_frame('587cc5d0-e158-4416-8438-387c5a4272c4', post_conciliar_interpretive_struggle).
narrative_ontology:cs_drift_state('587cc5d0-e158-4416-8438-387c5a4272c4', contemporary_theological_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('587cc5d0-e158-4416-8438-387c5a4272c4', '').
narrative_ontology:cs_kernel_id(vatican_ii_authority__composite_overdetermination_reading, vatican_ii_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_authority__composite_overdetermination_reading, theological_scholars_of_complexity).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__composite_overdetermination_reading, critical_historians).
narrative_ontology:constraint_victim(vatican_ii_authority__composite_overdetermination_reading, institutional_magisterium).
narrative_ontology:constraint_victim(vatican_ii_authority__composite_overdetermination_reading, faithful_seeking_univocal_guidance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These scholars benefit from the recognition of Vatican II's inherent ambiguities and contradictions, as it provides fertile ground for ongoing research, critical analysis, and the development of new theological frameworks. Their careers are advanced by demonstrating the limits of simplistic interpretations.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, theological_scholars_of_complexity, beneficiary,
    analytical, generational, analytical, global).

% Historians who analyze the council's proceedings, compromises, and the diverse theological currents at play find their work validated by this reading. It allows them to highlight the political and intellectual struggles that shaped the documents, rather than presenting a monolithic narrative.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, critical_historians, beneficiary,
    analytical, generational, analytical, global).

% The teaching authority of the Church is challenged by this reading, as it undermines claims of a singular, coherent interpretation of Vatican II. It forces the Magisterium to navigate inherent contradictions, leading to internal conflicts and a perceived erosion of authority. Their identity is tied to presenting a unified doctrinal front.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, institutional_magisterium, payer,
    institutional, civilizational, identity_locked, universal).

% Many adherents seek clear, unambiguous doctrinal guidance from the Church. This reading, by emphasizing irresolvable ambiguities, creates confusion, anxiety, and a sense of instability, making it difficult for them to reconcile conflicting interpretations and maintain a coherent theological worldview. Their identity is often fused with the Church's teaching.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, faithful_seeking_univocal_guidance, payer,
    powerless, biographical, identity_locked, global).

% These groups, often advocating for a 'rupture' reading from a conservative perspective, are excluded from the mainstream discourse that attempts to reconcile or explain away the council's complexities. Their critiques, while acknowledging contradictions, are often dismissed as disloyal or schismatic, preventing their full participation in the interpretive debate.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__composite_overdetermination_reading, traditionalist_factions, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This reading coordinates the understanding of Vatican II as a complex historical and theological event, acknowledging the multiple, sometimes conflicting, intentions and outcomes, thereby allowing for diverse scholarly approaches and preventing forced, simplistic narratives.
% TRANSFER_FUNCTION: It transfers interpretive authority from a singular institutional voice to a more distributed, critical, and historically informed scholarly community, while transferring a sense of doctrinal instability to those seeking clear, unified guidance.
% ABSENT_VOICES: Traditionalist factions, who often articulate a 'rupture' reading from a position of dissent, are frequently marginalized in discussions that seek to understand the council's internal contradictions. Their perspective, while acknowledging ambiguity, is often framed as disloyal rather than a valid interpretive lens.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the interpretive landscape of Vatican II would revert to a binary struggle between 'continuity' and 'rupture' narratives, losing the nuance of overdetermination and internal contradiction. Scholarly discourse would be impoverished, and institutional attempts at reconciliation would face less critical scrutiny, potentially leading to a more rigid, less honest engagement with the council's legacy.
% FOUNDING_PROBLEM: The problem this reading addresses is the intellectual dishonesty and historical inaccuracy of attempting to force Vatican II into a singular, coherent interpretive framework (either pure continuity or pure rupture), despite clear evidence of internal theological tensions and factional compromises during its drafting.
% FOUNDING_PROBLEM_CORROBORATION: Independent theological historians and critical scholars, outside of the institutional Magisterium, corroborate that the problem of oversimplification and forced coherence remains live, as evidenced by ongoing debates and the persistent difficulty in reconciling various conciliar texts with each other and with prior tradition. This is supported by archival research and textual analysis.
narrative_ontology:disappearance_verdict(vatican_ii_authority__composite_overdetermination_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_authority__composite_overdetermination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_authority__composite_overdetermination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(vatican_ii_authority__composite_overdetermination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_authority__composite_overdetermination_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_authority__composite_overdetermination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_authority__composite_overdetermination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_authority__composite_overdetermination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) reflects the cost borne by the institutional Magisterium and the faithful in navigating irresolvable contradictions, which this reading highlights. Suppression (0.70) is high because the institutional Church actively attempts to enforce a coherent, unified interpretation, often suppressing dissenting or overly critical analyses. The theater ratio (0.40) indicates that a significant portion of institutional interpretive effort is performative, aimed at maintaining an appearance of coherence despite underlying contradictions. Resistance (0.75) is high due to ongoing theological debates and the persistent challenges to a singular interpretation. Accessibility collapse (0.45) is moderate, as alternative interpretations (continuity, rupture) are readily available, but this reading argues they are ultimately insufficient.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the institutional Magisterium, this reading is highly extractive, as it undermines their authority to provide clear, unified teaching. For scholars, it is a beneficial framework that enables deeper, more honest engagement with the historical and theological realities of Vatican II. The engine's classification will reflect this divergence based on the declared roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Theological scholars and critical historians are beneficiaries (low d) as this reading validates their analytical approach and provides rich material for their work. The institutional Magisterium and the faithful seeking univocal guidance are victims (high d) because this reading directly challenges their preferred mode of authority and understanding, creating internal strain and confusion. Traditionalist factions are excluded, as their 'rupture' reading, while acknowledging contradictions, is often dismissed by the mainstream interpretive efforts that this reading critiques.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    resolvability_of_ambiguity,
    'Are the ambiguities and contradictions within Vatican II truly irresolvable, or could a more sophisticated hermeneutic eventually reconcile them?',
    'Future theological developments or a new interpretive paradigm that successfully demonstrates a coherent synthesis without suppressing historical evidence of conflict.',
    'If resolvable, the extractiveness on the Magisterium and faithful would decrease, and the constraint might shift towards a more ''rope-like'' coordination of complex truth. If truly irresolvable, the current ''tangled_rope'' classification is reinforced, highlighting the ongoing cost of managing inherent contradictions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resolvability_of_ambiguity, conceptual, 'Whether Vatican II''s internal tensions are fundamentally irresolvable or merely await a better interpretive framework.').

omega_variable(
    institutional_response_to_complexity,
    'To what extent does the institutional Magisterium genuinely attempt to engage with the council''s complexities versus enforcing a simplified narrative for pastoral reasons?',
    'Analysis of official documents, theological commissions, and papal statements over time, assessing their engagement with critical scholarship and their willingness to acknowledge internal tensions.',
    'If genuine engagement increases, the ''suppression'' and ''theater_ratio'' metrics would decrease, potentially shifting the constraint towards a more ''rope-like'' coordination of theological discourse. If simplification persists, the ''tangled_rope'' classification is reinforced, indicating ongoing extraction through narrative control.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_response_to_complexity, empirical, 'The degree of institutional honesty in confronting Vatican II''s complexities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_authority__composite_overdetermination_reading, 1965, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1965, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 1965, 0.2).
narrative_ontology:measurement(vati_tr_t1975, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 1975, 0.25).
narrative_ontology:measurement(vati_tr_t1985, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 1985, 0.3).
narrative_ontology:measurement(vati_tr_t1995, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 1995, 0.35).
narrative_ontology:measurement(vati_tr_t2005, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 2005, 0.4).
narrative_ontology:measurement(vati_tr_t2015, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 2015, 0.42).
narrative_ontology:measurement(vati_tr_t2024, vatican_ii_authority__composite_overdetermination_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(vati_be_t1965, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 1965, 0.5).
narrative_ontology:measurement(vati_be_t1975, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 1975, 0.55).
narrative_ontology:measurement(vati_be_t1985, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 1985, 0.6).
narrative_ontology:measurement(vati_be_t1995, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 1995, 0.63).
narrative_ontology:measurement(vati_be_t2005, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 2005, 0.65).
narrative_ontology:measurement(vati_be_t2015, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 2015, 0.66).
narrative_ontology:measurement(vati_be_t2024, vatican_ii_authority__composite_overdetermination_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1965, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 1965, 0.6).
narrative_ontology:measurement(vati_su_t1975, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 1975, 0.65).
narrative_ontology:measurement(vati_su_t1985, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 1985, 0.68).
narrative_ontology:measurement(vati_su_t1995, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 1995, 0.7).
narrative_ontology:measurement(vati_su_t2005, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 2005, 0.72).
narrative_ontology:measurement(vati_su_t2015, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 2015, 0.7).
narrative_ontology:measurement(vati_su_t2024, vatican_ii_authority__composite_overdetermination_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_authority__composite_overdetermination_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
