% ============================================================================
% CONSTRAINT STORY: press_reformation_causation__technological_determinism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causation__technological_determinism, []).

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
 *   constraint_id: press_reformation_causation__technological_determinism
 *   human_readable: Printing Press as Deterministic Cause of Reformation
 *   domain: history/technology/religion
 *
 * SUMMARY:
 *   This constraint instantiates the technological determinism reading of the
 *   press-Reformation kernel: the printing press is treated as an upstream
 *   mountain whose inherent properties—mechanical reproducibility, vernacular
 *   typographic capacity, and cost-scaling—made ecclesiastical censorship
 *   impossible and vernacular scripture inevitable. Reformers and lay readers
 *   appear as downstream beneficiaries of an exogenous technological force,
 *   while the Catholic hierarchy and scribal monopolies bear the cost of a
 *   collapsed information monopoly without effective recourse. The constraint
 *   is claimed as mountain because the reading structurally asserts natural
 *   inevitability; the authored metrics remain low-extraction and
 *   low-suppression to reflect that internal framing, while declared
 *   beneficiaries trigger False Summit Mountain evaluation.
 *
 * KEY AGENTS:
 *   - vernacular_reformers: Primary beneficiaries (moderate/mobile) — receive irreversible diffusion capacity
 *   - lay_readers: Secondary beneficiaries (powerless/constrained) — gain access without agency to shape the constraint
 *   - catholic_church_hierarchy: Primary targets/payers (institutional/trapped) — lose monopoly, resistance futile
 *   - scribal_monopolies: Secondary targets/payers (organized/trapped) — craft obsolescence
 *   - academic_observer: Analytical observer — tracks the gap between determinist narrative and archival evidence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causation__technological_determinism, 0.12).
domain_priors:suppression_score(press_reformation_causation__technological_determinism, 0.1).
domain_priors:theater_ratio(press_reformation_causation__technological_determinism, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, extractiveness, 0.12).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causation__technological_determinism, mountain).
narrative_ontology:human_readable(press_reformation_causation__technological_determinism, "Printing Press as Deterministic Cause of Reformation").
narrative_ontology:topic_domain(press_reformation_causation__technological_determinism, "history/technology/religion").

domain_priors:emerges_naturally(press_reformation_causation__technological_determinism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causation__technological_determinism, 'fe741040-ad24-4eaa-b5b7-c540b9a2e7b5').
narrative_ontology:cs_kernel_codification('fe741040-ad24-4eaa-b5b7-c540b9a2e7b5', distributed).
narrative_ontology:cs_authority_grounding('fe741040-ad24-4eaa-b5b7-c540b9a2e7b5', expertise).
narrative_ontology:cs_interpretation_layer_present('fe741040-ad24-4eaa-b5b7-c540b9a2e7b5').
narrative_ontology:cs_reading_relation('fe741040-ad24-4eaa-b5b7-c540b9a2e7b5', press_reformation_causation__strategic_deployment, coexists_with).
narrative_ontology:cs_reading_relation('fe741040-ad24-4eaa-b5b7-c540b9a2e7b5', press_reformation_causation__mutual_shaping, coexists_with).
narrative_ontology:cs_axiom('fe741040-ad24-4eaa-b5b7-c540b9a2e7b5', foundational, technology_autonomously_determines_socio_religious_outcomes).
narrative_ontology:cs_axiom_status(technology_autonomously_determines_socio_religious_outcomes, holdable).
narrative_ontology:cs_axiom_grounding('fe741040-ad24-4eaa-b5b7-c540b9a2e7b5', technology_autonomously_determines_socio_religious_outcomes, empirically_contingent).
narrative_ontology:cs_axiom('fe741040-ad24-4eaa-b5b7-c540b9a2e7b5', foundational, censorship_is_technologically_impossible_post_press).
narrative_ontology:cs_axiom_status(censorship_is_technologically_impossible_post_press, holdable).
narrative_ontology:cs_axiom_grounding('fe741040-ad24-4eaa-b5b7-c540b9a2e7b5', censorship_is_technologically_impossible_post_press, empirically_contingent).
narrative_ontology:cs_reference_frame('fe741040-ad24-4eaa-b5b7-c540b9a2e7b5', printing_press_as_autonomous_force).
narrative_ontology:cs_drift_state('fe741040-ad24-4eaa-b5b7-c540b9a2e7b5', contemporary_historiography, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('fe741040-ad24-4eaa-b5b7-c540b9a2e7b5', '').
narrative_ontology:cs_kernel_id(press_reformation_causation__technological_determinism, press_reformation_causation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causation__technological_determinism, vernacular_reformers).
narrative_ontology:constraint_beneficiary(press_reformation_causation__technological_determinism, lay_readers).
narrative_ontology:constraint_victim(press_reformation_causation__technological_determinism, catholic_church_hierarchy).
narrative_ontology:constraint_victim(press_reformation_causation__technological_determinism, scribal_monopolies).
narrative_ontology:constraint_vindicates(press_reformation_causation__technological_determinism, technological_determinism_thesis).
narrative_ontology:constraint_vindicates(press_reformation_causation__technological_determinism, vernacular_scripture_inevitability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive an exogenous, irreversible diffusion channel for vernacular theological texts that bypasses ecclesiastical gatekeeping. Their message spreads not through institutional negotiation but through the mechanical logic of the press, which renders the old monopoly on sacred interpretation structurally obsolete.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, vernacular_reformers, beneficiary,
    moderate, biographical, mobile, continental).

% Gain unmediated access to vernacular scripture and pamphlets that were previously inaccessible due to Latinity and scarcity. Their consumption is enabled by the press's cost structure rather than by ecclesiastical permission or personal clerical contacts.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, lay_readers, beneficiary,
    powerless, biographical, constrained, continental).

% Bears the loss of the information monopoly that underwrote doctrinal unity and political authority. Bulls, indexes, and territorial censorship cannot arrest the mechanical reproduction of texts; the hierarchy's resistance is structurally futile against the press's diffusion logic.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, catholic_church_hierarchy, payer,
    institutional, civilizational, trapped, global).

% See their economically viable monopoly on manuscript production and sacred textual accuracy collapse as the press decouples text reproduction from scribal skill and monastic infrastructure, making their craft redundant.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, scribal_monopolies, payer,
    organized, biographical, trapped, regional).

% Evaluates the determinism thesis against archival evidence of reformer-printer negotiations and Church adaptation strategies. Notes the asymmetry between the mountain claim and the documented agency of historical actors, treating the thesis as a historiographical constraint rather than a natural law.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, academic_observer, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Elimination of information scarcity through mechanical reproduction, rendering coordinated ecclesiastical gatekeeping structurally redundant and replacing institutional control with technologically driven diffusion.
% TRANSFER_FUNCTION: Moves textual reproduction capacity and doctrinal control from the manuscript-scribal-Catholic hierarchy to the vernacular reformer and lay reader, driven by the press's cost and scale logic rather than negotiated exchange.
% ABSENT_VOICES: Scribal producers, local bishops attempting vernacular control outside reform channels, and the Catholic laity attached to non-textual sacramental practice are absent from the determinist narrative; the story is told from the press's output and the reformers' reception, erasing counterfactual adaptations.
% DISAPPEARANCE_RATIONALE: If the deterministic constraint vanished—if the press had not made censorship impossible—the Reformation might not have achieved mass diffusion; yet the determinist and strategic readings dispute whether the world rearranges around technology or around human agency.
% FOUNDING_PROBLEM: Latin liturgy and manuscript scarcity restricted scripture access to a clerical elite, creating an information bottleneck in Latin Christendom that the press is said to have obliterated.
% FOUNDING_PROBLEM_CORROBORATION: Medieval historians corroborate the pre-1450 information bottleneck, but they are divided on whether the press deterministically solved it; social historians of religion note ongoing lay religious practice outside textual channels, corroborating a more complex founding picture than pure scarcity.
narrative_ontology:disappearance_verdict(press_reformation_causation__technological_determinism, contested).
narrative_ontology:founding_problem_status(press_reformation_causation__technological_determinism, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causation__technological_determinism, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'f1436bd4937f864097dabaad92b27bd9b6eec212', '2026-08-04',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(press_reformation_causation__technological_determinism, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causation__technological_determinism, 0.12, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causation__technological_determinism_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, ExtMetricName, E),
    domain_priors:suppression_score(press_reformation_causation__technological_determinism, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(press_reformation_causation__technological_determinism),
    narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(press_reformation_causation__technological_determinism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored low (0.12) because the determinism reading frames the press as a non-agentive force of nature rather than an extractive arrangement; suppression is similarly low (0.10) because censorship collapse is attributed to technological obsolescence rather than active coercion. Accessibility collapse is authored very high (0.92) because once the press exists, manuscript alternatives become structurally non-viable. Resistance is near-zero (0.08) because the reading renders Church resistance futile by definition. Theater ratio is low but rising (0.12) to capture the performative maintenance of the inevitability narrative in later historiography. The flat, low metric profile is consistent with a mountain claim; the presence of beneficiaries triggers the FSM signature.
 *
 * PERSPECTIVAL GAP:
 *   From the reformer and lay-reader seats the press appears as liberating mountain; from the Church and scribal seats it appears as a catastrophic, irresistible force. The academic observer seat sees a historiographical construct that has dissolved agency into mechanical causation. The engine will compute divergent per-seat classifications from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Vernacular reformers and lay readers are declared beneficiaries, mapping to low directionality and damped effective extraction. The Catholic Church and scribal monopolies are declared victims/payers, mapping to high directionality and amplified effective extraction despite the low base epsilon. The asymmetry is structural: the reading distributes costs to incumbent gatekeepers and benefits to vernacular challengers.
 *
 * MANDATROPHY ANALYSIS:
 *   The determinism narrative persists as a template for later technological disruption discourse long after the specific Reformation information scarcity problem was resolved. The founding problem (manuscript bottleneck) is dead, but the arrangement (determinist explanatory frame) persists, suggesting mandatrophy risk if the narrative continues to foreclose attention to human agency in technological change.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fsm_natural_law_ambiguity,
    'Is the printing press''s causal power a genuine technological mountain, or a constructed narrative that benefits Protestant historiography and later tech-determinist ideology?',
    'Comparative archival analysis of pre-press lay religious networks and post-press reformer-printer contracts; if reformers actively shaped press deployment, the mountain claim is a false summit.',
    'If constructed, the constraint reclassifies from mountain to tangled_rope or snare under FSM, revealing the determinism narrative as an extraction mechanism that suppresses agency.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fsm_natural_law_ambiguity, empirical, 'Natural-law versus constructed narrative ambiguity for FSM evaluation').

omega_variable(
    agency_suppression,
    'Does the determinism reading structurally suppress the empirical evidence of strategic reformer and printer agency, and if so, is that suppression internalized or enforced?',
    'Quantitative analysis of archival printer-reformer correspondence and contracts versus determinist historiography citations; divergence indicates narrative suppression of agency.',
    'If agency evidence is systematically elided, the constraint''s effective suppression is higher than the structural measure suggests, raising extraction through cognitive capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(agency_suppression, empirical, 'Agency suppression mechanism in technological determinism').

omega_variable(
    scope_of_collapse,
    'Does the press make censorship impossible universally, or does it merely shift the locus of textual control to territorial princes and reformer networks?',
    'Map of successful Catholic censorship post-1517 (Index, Bavaria, Spain) versus failed censorship zones; persistent successful control falsifies the impossibility claim.',
    'If censorship remained possible in some jurisdictions, the accessibility_collapse metric is overstated and the inevitability axiom is empirically contingent rather than mountain-grade.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_collapse, empirical, 'Geographic and jurisdictional limits of press-driven censorship collapse').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causation__technological_determinism, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t0, press_reformation_causation__technological_determinism, theater_ratio, 0, 0.02).
narrative_ontology:measurement(pres_tr_t20, press_reformation_causation__technological_determinism, theater_ratio, 20, 0.04).
narrative_ontology:measurement(pres_tr_t40, press_reformation_causation__technological_determinism, theater_ratio, 40, 0.06).
narrative_ontology:measurement(pres_tr_t60, press_reformation_causation__technological_determinism, theater_ratio, 60, 0.08).
narrative_ontology:measurement(pres_tr_t80, press_reformation_causation__technological_determinism, theater_ratio, 80, 0.1).
narrative_ontology:measurement(pres_tr_t100, press_reformation_causation__technological_determinism, theater_ratio, 100, 0.12).

% Extraction over time
narrative_ontology:measurement(pres_be_t0, press_reformation_causation__technological_determinism, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(pres_be_t20, press_reformation_causation__technological_determinism, base_extractiveness, 20, 0.06).
narrative_ontology:measurement(pres_be_t40, press_reformation_causation__technological_determinism, base_extractiveness, 40, 0.07).
narrative_ontology:measurement(pres_be_t60, press_reformation_causation__technological_determinism, base_extractiveness, 60, 0.08).
narrative_ontology:measurement(pres_be_t80, press_reformation_causation__technological_determinism, base_extractiveness, 80, 0.1).
narrative_ontology:measurement(pres_be_t100, press_reformation_causation__technological_determinism, base_extractiveness, 100, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(pres_su_t0, press_reformation_causation__technological_determinism, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(pres_su_t20, press_reformation_causation__technological_determinism, suppression_requirement, 20, 0.05).
narrative_ontology:measurement(pres_su_t40, press_reformation_causation__technological_determinism, suppression_requirement, 40, 0.06).
narrative_ontology:measurement(pres_su_t60, press_reformation_causation__technological_determinism, suppression_requirement, 60, 0.07).
narrative_ontology:measurement(pres_su_t80, press_reformation_causation__technological_determinism, suppression_requirement, 80, 0.08).
narrative_ontology:measurement(pres_su_t100, press_reformation_causation__technological_determinism, suppression_requirement, 100, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(press_reformation_causation__technological_determinism, press_reformation_causation__strategic_deployment).
narrative_ontology:affects_constraint(press_reformation_causation__technological_determinism, press_reformation_causation__mutual_shaping).

% DUAL FORMULATION NOTE:
% This story is one of three decomposition readings of the press_reformation_causation kernel. The technological determinism reading posits an autonomous mountain; the strategic deployment reading posits instrumental agency; the mutual shaping reading posits co-evolution. Each reading carries a distinct epsilon and beneficiary structure, linked via the network to enable cross-reading contamination analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
