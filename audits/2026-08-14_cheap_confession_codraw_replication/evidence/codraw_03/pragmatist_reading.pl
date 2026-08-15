% ============================================================================
% CONSTRAINT STORY: pragmatist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_pragmatist_reading, []).

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
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: pragmatist_reading
 *   human_readable: Pragmatist Reading of Positional Disagreement (Corrigible Inquiry Norm)
 *   domain: epistemology/philosophy_of_technology/institutional_analysis
 *
 * SUMMARY:
 *   This story instantiates the pragmatist reading of the kernel
 *   positional_disagreement_as_evidence: disagreement is provisional data
 *   within an ongoing corrigible inquiry; no position carries standing
 *   epistemic advantage merely by being declared, and truth is what
 *   indefinite inquiry converges on. The practical determinant of which
 *   disagreements actually get resolved is not epistemic merit but the
 *   mundane bottleneck triad of self-audit cost, propagation incentives, and
 *   institutional acknowledgment capacity. Unlike the standpoint reading
 *   (which treats structural position as evidentially privileged) or the
 *   proceduralist reading (which treats procedural compliance as the
 *   resolution criterion), this reading refuses to grant either standpoint or
 *   procedure a priori epistemic standing — resolution is earned through
 *   sustained, resourced inquiry, and the reading's honest cost is that
 *   resourcing is unevenly distributed.
 *
 * KEY AGENTS:
 *   - research_communities_with_low_self_audit_cost: primary beneficiary (organized/mobile) — the norm mostly describes their existing low-cost revision practice
 *   - institutions_with_acknowledgment_capacity: agenda_setter/beneficiary (institutional/mobile) — administers which disagreements get formally taken up
 *   - well_resourced_disputants: beneficiary/payer (powerful/mobile) — can sustain indefinite contestation, benefiting from the framework but bearing its real costs
 *   - under_resourced_disputants: payer (moderate/constrained) — formally unprivileged either way but structurally unable to clear the practical bottlenecks
 *   - urgent_decision_stakeholders: payer (powerless/trapped) — need practical closure now and bear the cost of indefinite inquiry without a voice in it
 *   - philosophers_of_inquiry: analytical observer — traces the bottleneck structure itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(pragmatist_reading, 0.28).
domain_priors:suppression_score(pragmatist_reading, 0.22).
domain_priors:theater_ratio(pragmatist_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(pragmatist_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(pragmatist_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(pragmatist_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(pragmatist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(pragmatist_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(pragmatist_reading, rope).
narrative_ontology:human_readable(pragmatist_reading, "Pragmatist Reading of Positional Disagreement (Corrigible Inquiry Norm)").
narrative_ontology:topic_domain(pragmatist_reading, "epistemology/philosophy_of_technology/institutional_analysis").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(pragmatist_reading, 'c52e2320-3eb1-4c06-bcbe-62dbf486d400').
narrative_ontology:cs_kernel_codification('c52e2320-3eb1-4c06-bcbe-62dbf486d400', distributed).
narrative_ontology:cs_authority_grounding('c52e2320-3eb1-4c06-bcbe-62dbf486d400', practice).
narrative_ontology:cs_interpretation_layer_present('c52e2320-3eb1-4c06-bcbe-62dbf486d400').
narrative_ontology:cs_reading_relation('c52e2320-3eb1-4c06-bcbe-62dbf486d400', pragmatist_reading__standpoint_reading, coexists_with).
narrative_ontology:cs_reading_relation('c52e2320-3eb1-4c06-bcbe-62dbf486d400', pragmatist_reading__proceduralist_reading, coexists_with).
narrative_ontology:cs_reading_relation('c52e2320-3eb1-4c06-bcbe-62dbf486d400', pragmatist_reading__instrumentalist_reading, influences).
narrative_ontology:cs_axiom('c52e2320-3eb1-4c06-bcbe-62dbf486d400', foundational, no_position_has_a_priori_standing_advantage).
narrative_ontology:cs_axiom_status(no_position_has_a_priori_standing_advantage, holdable).
narrative_ontology:cs_axiom_grounding('c52e2320-3eb1-4c06-bcbe-62dbf486d400', no_position_has_a_priori_standing_advantage, conventional).
narrative_ontology:cs_axiom('c52e2320-3eb1-4c06-bcbe-62dbf486d400', foundational, declaration_is_procedural_stopgap_not_epistemic_privilege).
narrative_ontology:cs_axiom_status(declaration_is_procedural_stopgap_not_epistemic_privilege, holdable).
narrative_ontology:cs_axiom_grounding('c52e2320-3eb1-4c06-bcbe-62dbf486d400', declaration_is_procedural_stopgap_not_epistemic_privilege, instrumental).
narrative_ontology:cs_axiom('c52e2320-3eb1-4c06-bcbe-62dbf486d400', secondary, truth_is_limit_of_indefinite_convergent_inquiry).
narrative_ontology:cs_axiom_status(truth_is_limit_of_indefinite_convergent_inquiry, holdable).
narrative_ontology:cs_axiom_grounding('c52e2320-3eb1-4c06-bcbe-62dbf486d400', truth_is_limit_of_indefinite_convergent_inquiry, conventional).
narrative_ontology:cs_reference_frame('c52e2320-3eb1-4c06-bcbe-62dbf486d400', corrigible_inquiry_without_terminal_authority).
narrative_ontology:cs_drift_state('c52e2320-3eb1-4c06-bcbe-62dbf486d400', contemporary_institutional_science, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c52e2320-3eb1-4c06-bcbe-62dbf486d400', '').
narrative_ontology:cs_kernel_id(pragmatist_reading, positional_disagreement_as_evidence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(pragmatist_reading, research_communities_with_low_self_audit_cost).
narrative_ontology:constraint_beneficiary(pragmatist_reading, institutions_with_acknowledgment_capacity).
narrative_ontology:constraint_beneficiary(pragmatist_reading, well_resourced_disputants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(pragmatist_reading, well_resourced_disputants).
narrative_ontology:constraint_victim(pragmatist_reading, under_resourced_disputants).
narrative_ontology:constraint_victim(pragmatist_reading, urgent_decision_stakeholders).
narrative_ontology:constraint_vindicates(pragmatist_reading, convergent_truth_as_regulative_ideal).
narrative_ontology:constraint_vindicates(pragmatist_reading, declaration_as_procedural_stopgap).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate in fields where re-checking one's own prior claims is cheap (public data, replicable methods, low reputational stakes for reversal). They can treat disagreement as provisional data and revise quickly, so the norm mostly describes how they already behave and costs them little to affirm.
narrative_ontology:constraint_stakeholder(pragmatist_reading, research_communities_with_low_self_audit_cost, beneficiary,
    organized, generational, mobile, national).

% Journals, funding bodies, and professional societies with standing procedures for revising past determinations (errata, retraction processes, updated guidelines). They administer which disagreements get formally taken up, and their existing capacity determines whose provisional claims actually reach resolution.
narrative_ontology:constraint_stakeholder(pragmatist_reading, institutions_with_acknowledgment_capacity, beneficiary,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(pragmatist_reading, institutions_with_acknowledgment_capacity, agenda_setter).

% Individuals or labs with time, institutional standing, and career security to keep re-litigating a contested claim across years of inquiry. Because the norm treats disagreement as ongoing and unprivileged, they can afford to keep contesting; this benefits them, but it also costs them real effort sustaining the inquiry rather than closing it early.
narrative_ontology:constraint_stakeholder(pragmatist_reading, well_resourced_disputants, beneficiary,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(pragmatist_reading, well_resourced_disputants, payer).

% Researchers, practitioners, or claimants without institutional backing to sustain multi-year self-audit and propagation efforts. Under the pragmatist norm their disagreement counts as data with no a priori privilege, but they lack the practical bottleneck-clearing resources (time, venues, institutional standing) to get their position actually taken up, so their disagreement can persist unresolved indefinitely through no fault of the framework's stated logic.
narrative_ontology:constraint_stakeholder(pragmatist_reading, under_resourced_disputants, payer,
    moderate, biographical, constrained, national).

% People who need a practical answer now (a patient awaiting a diagnostic protocol decision, a community awaiting a regulatory determination) and cannot wait for indefinite convergence. The norm's insistence that declaration is only a procedural stopgap, not epistemic privilege, means no one can point to institutional closure as settled; they bear the cost of ongoing inquiry in real time even though they cannot participate in resolving it.
narrative_ontology:constraint_stakeholder(pragmatist_reading, urgent_decision_stakeholders, payer,
    powerless, immediate, trapped, local).

% Study the structure of the disagreement-as-evidence norm itself, tracing how self-audit cost, propagation incentives, and institutional acknowledgment capacity function as the actual bottlenecks on resolution, independent of which position is correct.
narrative_ontology:constraint_stakeholder(pragmatist_reading, philosophers_of_inquiry, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared procedural norm for treating unresolved disagreement as provisional evidence rather than as a contest to be won by whoever declares first, allowing distributed inquiry to continue without premature lock-in on any single position.
% TRANSFER_FUNCTION: Moves the burden of resolution from a single declared authority onto the ongoing process of inquiry itself; practically, it shifts effort and standing toward whichever parties can sustain self-audit and propagation costs, and away from parties who cannot wait out the process.
% ABSENT_VOICES: Under-resourced disputants and urgent decision stakeholders are formally unprivileged either way under this reading but structurally unable to move the practical bottlenecks (self-audit cost, propagation incentives, institutional capacity) that actually determine resolution; they are named in the framework's own account of what gates resolution but have no seat in clearing those gates.
% DISAPPEARANCE_RATIONALE: If this reading of the kernel vanished, institutions administering acknowledgment capacity would still exist and continue resolving some disputes on other grounds (proceduralist or instrumentalist criteria); researchers with low audit cost would likely continue behaving the same way regardless of which reading is named. What would change is the explicit refusal to grant declaration epistemic privilege — without that norm, institutional declarations could more easily be treated as settled truth rather than provisional stopgaps, which matters most to those currently waiting on unresolved disagreements.
% FOUNDING_PROBLEM: Scientific and institutional communities repeatedly conflated 'this has been officially declared/published/ratified' with 'this is true,' foreclosing revision of positions that were procedurally settled but empirically or normatively wrong, and blocking legitimate dissent that lacked standing to be heard.
% FOUNDING_PROBLEM_CORROBORATION: Philosophers of science and institutional historians outside any specific benefiting research community attest that premature closure (Kuhn's normal-science lock-in, replication-crisis retractions arriving years after the fact) remains a documented recurring failure mode, corroborating that the founding problem persists independent of any single field's self-report.
narrative_ontology:disappearance_verdict(pragmatist_reading, contested).
narrative_ontology:founding_problem_status(pragmatist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(pragmatist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-14',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(pragmatist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(pragmatist_reading, 0.28, 'claude-sonnet-5', 'cheap_confession_2026_20260814_151329', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(pragmatist_reading_tests).
:- end_tests(pragmatist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low-moderate (0.28) because this reading, taken on its own terms, is a genuine coordination norm against premature epistemic closure — it does not by itself transfer resources from a victim class to a beneficiary class. What it does transfer, however, is practical standing: the disagreements that get resolved are those whose disputants can pay the self-audit and propagation costs, which quietly favors already well-resourced parties without the reading declaring this as its function. Suppression is low (0.22) — nothing coercively forecloses alternative readings; the mechanism is neglect of bottleneck-clearing capacity, not active suppression. Theater ratio rises modestly over the interval (0.18 to 0.30) as institutions increasingly cite 'ongoing inquiry' and 'no standing privilege' as a stated commitment while acknowledgment capacity does not scale to match — a mild but real drift toward using the pragmatist framing performatively to defer resolution rather than pursue it.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (low-audit-cost communities, capacious institutions, well-resourced disputants), the reading looks like straightforward, low-cost epistemic hygiene — of course disagreement is provisional, of course no one gets to declare and stop inquiry. From the payer seats (under-resourced disputants, urgent decision stakeholders), the identical norm looks like an indefinite deferral mechanism: the refusal to grant declaration privilege means no one is ever institutionally answerable for resolving their case, and the practical bottlenecks that would resolve it are exactly the resources they lack.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are those for whom the bottleneck-clearing costs (self-audit, propagation, institutional uptake) are already low or absorbable — the reading's coordination function costs them little and its refusal of premature closure protects positions they can afford to keep contesting. Victims are not named as a fixed class (per the expected structural delta for this reading) but the payer roles fall on those for whom the same bottleneck structure is a genuine constraint: under-resourced disputants and urgent decision stakeholders bear the cost of indefinite inquiry without possessing the means to accelerate resolution.
 *
 * MANDATROPHY ANALYSIS:
 *   The pragmatist reading resists mandatrophy in the classic sense — it explicitly refuses to let a founding declaration ossify into unrevisable authority, keeping the founding problem (premature closure) perpetually live by design. Its distinct failure mode, as flagged in the kernel decomposition, is not corrective silence but premature closure BEFORE genuine convergence — institutions citing 'ongoing inquiry' to defer answerable resolution indefinitely, which the rising theater_ratio series is intended to capture as an early warning rather than a verdict.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bottleneck_neutrality_vs_capture,
    'Are self-audit cost, propagation incentives, and institutional acknowledgment capacity neutral practical facts about inquiry, or are they themselves shaped by the same power asymmetries the pragmatist reading declines to privilege epistemically?',
    'Track whether bottleneck-clearing capacity correlates systematically with prior institutional power across a sample of resolved vs. unresolved disagreements; if resolution outcomes track power rather than evidential merit, the reading''s claimed neutrality is compromised in practice even if not in its stated logic.',
    'If bottlenecks are power-shaped rather than neutral, the pragmatist reading''s refusal to grant a priori privilege does not prevent de facto privilege from flowing through resource asymmetry — pushing this constraint''s computed type toward tangled_rope despite its coordination framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bottleneck_neutrality_vs_capture, empirical, 'Whether the practical resolution bottlenecks are power-neutral or power-correlated.').

omega_variable(
    convergence_horizon_indeterminacy,
    'Is ''what indefinite inquiry converges on'' a well-defined regulative ideal, or does the absence of any terminal condition make the norm unfalsifiable in practice — always deferring judgment to a future that never arrives?',
    'Examine historical cases claimed as convergence (e.g. long-settled scientific disputes) for whether an identifiable closure event occurred, versus disputes still labeled ''ongoing'' after comparable or longer timeframes with no closure in sight.',
    'If genuine convergence events are rare or ambiguous, the reading''s central regulative ideal may function mainly as justification for indefinite deferral, supporting the premature-closure-avoidance failure mode over the productive-inquiry function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(convergence_horizon_indeterminacy, conceptual, 'Whether indefinite convergence is an operative test or an unfalsifiable deferral.').

omega_variable(
    reading_selection_under_determination,
    'Given the same underlying disagreement, would a different observer with the same facts but a proceduralist or standpoint commitment classify the same institutional behavior differently — and is there a fact of the matter about which reading better fits the constraint''s actual operation?',
    'Compare classifications this reading, standpoint_reading, and proceduralist_reading produce for the same case studies once all three sibling stories exist; look for cases where the readings diverge sharply on victim/beneficiary structure.',
    'Divergent classifications across siblings for identical underlying facts would confirm the kernel genuinely under-determines a single structural answer, supporting the framework''s decision to author these as separate linked constraints rather than one averaged story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_selection_under_determination, conceptual, 'Whether kernel readings genuinely diverge in structural classification for shared cases.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(pragmatist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prag_tr_t0, pragmatist_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(prag_tr_t8, pragmatist_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement(prag_tr_t16, pragmatist_reading, theater_ratio, 16, 0.23).
narrative_ontology:measurement(prag_tr_t24, pragmatist_reading, theater_ratio, 24, 0.25).
narrative_ontology:measurement(prag_tr_t32, pragmatist_reading, theater_ratio, 32, 0.28).
narrative_ontology:measurement(prag_tr_t40, pragmatist_reading, theater_ratio, 40, 0.3).

% Extraction over time
narrative_ontology:measurement(prag_be_t0, pragmatist_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(prag_be_t8, pragmatist_reading, base_extractiveness, 8, 0.22).
narrative_ontology:measurement(prag_be_t16, pragmatist_reading, base_extractiveness, 16, 0.24).
narrative_ontology:measurement(prag_be_t24, pragmatist_reading, base_extractiveness, 24, 0.26).
narrative_ontology:measurement(prag_be_t32, pragmatist_reading, base_extractiveness, 32, 0.27).
narrative_ontology:measurement(prag_be_t40, pragmatist_reading, base_extractiveness, 40, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(pragmatist_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
