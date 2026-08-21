% ============================================================================
% CONSTRAINT STORY: vatican_ii_authority__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_authority__continuity_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: vatican_ii_authority__continuity_reading
 *   human_readable: Vatican II as Organic Doctrinal Continuity
 *   domain: theology/ecclesiology/religious_authority
 *
 * SUMMARY:
 *   This constraint story instantiates the 'continuity reading' of Vatican
 *   II, which asserts that the Council's reforms represent an organic
 *   doctrinal development in full continuity with the Catholic Church's
 *   unchanging deposit of faith. From this perspective, the reforms are
 *   legitimate expressions of tradition, and any perceived contradictions are
 *   due to misinterpretation. This reading is officially promulgated by the
 *   Magisterium and supported by a significant body of theological
 *   scholarship.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_authority__continuity_reading, 0.15).
domain_priors:suppression_score(vatican_ii_authority__continuity_reading, 0.2).
domain_priors:theater_ratio(vatican_ii_authority__continuity_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_authority__continuity_reading, mountain).
narrative_ontology:human_readable(vatican_ii_authority__continuity_reading, "Vatican II as Organic Doctrinal Continuity").
narrative_ontology:topic_domain(vatican_ii_authority__continuity_reading, "theology/ecclesiology/religious_authority").

domain_priors:emerges_naturally(vatican_ii_authority__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_authority__continuity_reading, 'd5ec0f94-c705-4775-b512-b62ab422f550').
narrative_ontology:cs_kernel_codification('d5ec0f94-c705-4775-b512-b62ab422f550', fixed_text).
narrative_ontology:cs_authority_grounding('d5ec0f94-c705-4775-b512-b62ab422f550', lineage).
narrative_ontology:cs_interpretation_layer_present('d5ec0f94-c705-4775-b512-b62ab422f550').
narrative_ontology:cs_reading_relation('d5ec0f94-c705-4775-b512-b62ab422f550', vatican_ii_authority__rupture_reading, forecloses).
narrative_ontology:cs_reading_relation('d5ec0f94-c705-4775-b512-b62ab422f550', vatican_ii_authority__composite_overdetermination_reading, forecloses).
narrative_ontology:cs_axiom('d5ec0f94-c705-4775-b512-b62ab422f550', foundational, vatican_ii_is_doctrinal_continuity).
narrative_ontology:cs_axiom_status(vatican_ii_is_doctrinal_continuity, holdable).
narrative_ontology:cs_axiom_grounding('d5ec0f94-c705-4775-b512-b62ab422f550', vatican_ii_is_doctrinal_continuity, deontological).
narrative_ontology:cs_axiom('d5ec0f94-c705-4775-b512-b62ab422f550', foundational, magisterial_interpretation_is_authoritative).
narrative_ontology:cs_axiom_status(magisterial_interpretation_is_authoritative, holdable).
narrative_ontology:cs_axiom_grounding('d5ec0f94-c705-4775-b512-b62ab422f550', magisterial_interpretation_is_authoritative, conventional).
narrative_ontology:cs_reference_frame('d5ec0f94-c705-4775-b512-b62ab422f550', post_conciliar_magisterial_hermeneutic).
narrative_ontology:cs_drift_state('d5ec0f94-c705-4775-b512-b62ab422f550', contemporary_theological_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('d5ec0f94-c705-4775-b512-b62ab422f550', '').
narrative_ontology:cs_kernel_id(vatican_ii_authority__continuity_reading, vatican_ii_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_authority__continuity_reading, progressive_reformers_claiming_continuity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__continuity_reading, faithful_laity).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__continuity_reading, theologians_supporting_continuity).
narrative_ontology:constraint_victim(vatican_ii_authority__continuity_reading, traditionalist_critics).
narrative_ontology:constraint_victim(vatican_ii_authority__continuity_reading, faithful_laity).
narrative_ontology:constraint_vindicates(vatican_ii_authority__continuity_reading, doctrinal_development_theory).
narrative_ontology:constraint_vindicates(vatican_ii_authority__continuity_reading, hermeneutic_of_continuity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The official teaching authority of the Catholic Church, which promulgates and defends the continuity reading as the authentic interpretation of Vatican II, ensuring doctrinal coherence and institutional stability.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, magisterium, agenda_setter,
    institutional, civilizational, constrained, universal).

% Theologians, clergy, and lay movements who advocate for and implement post-conciliar reforms, finding their legitimacy and authority affirmed by the continuity reading, which frames their work as faithful development.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, progressive_reformers_claiming_continuity, beneficiary,
    organized, generational, constrained, global).

% Groups who perceive Vatican II as a rupture with tradition and reject many post-conciliar reforms. From the continuity reading's perspective, their dissent is a misunderstanding or lack of faith, and their arguments are marginalized within official discourse.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, traditionalist_critics, payer,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_authority__continuity_reading, traditionalist_critics, excluded).

% The general body of believers who receive and are expected to adhere to the official interpretation. They benefit from a clear, unified doctrinal framework but bear the cost of accepting reforms that may challenge prior understandings or practices.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, faithful_laity, beneficiary,
    powerless, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_authority__continuity_reading, faithful_laity, payer).

% Scholars whose academic work supports and elaborates the hermeneutic of continuity, finding their research validated and promoted within the institutional framework.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__continuity_reading, theologians_supporting_continuity, beneficiary,
    organized, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_authority__continuity_reading, magisterium).
narrative_ontology:fixing_cost_class(vatican_ii_authority__continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified theological framework for understanding the Second Vatican Council and subsequent reforms, aiming to prevent schism and maintain doctrinal coherence within the Catholic Church.
% TRANSFER_FUNCTION: Transfers theological legitimacy and institutional authority to post-conciliar reforms and their proponents, while conceptually marginalizing dissenting interpretations as misreadings or errors.
% ABSENT_VOICES: Proponents of the rupture reading and the composite overdetermination reading are present in theological discourse but are structurally excluded from authoritative interpretation; they would argue for fundamental breaks or irresolvable ambiguities.
% DISAPPEARANCE_RATIONALE: If the continuity reading vanished overnight, the legitimacy of Vatican II and subsequent reforms would be fundamentally undermined, leading to widespread doctrinal confusion, potential schism, and a profound crisis of authority within the Catholic Church, forcing a complete reorganization of its theological and institutional self-understanding.
% FOUNDING_PROBLEM: To reconcile the perceived innovations and pastoral shifts of Vatican II with the Catholic Church's claim to an unchanging, divinely revealed deposit of faith, providing a stable and authoritative basis for doctrinal development.
% FOUNDING_PROBLEM_CORROBORATION: The Magisterium consistently affirms this problem as live and the continuity reading as its solution. Theologians and historians aligned with this view provide extensive scholarly corroboration. Even traditionalist critics, while disagreeing with the continuity conclusion, acknowledge the *problem* of reconciling Vatican II with prior tradition.
narrative_ontology:disappearance_verdict(vatican_ii_authority__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_authority__continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_authority__continuity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(vatican_ii_authority__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_authority__continuity_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_authority__continuity_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, ExtMetricName, E),
    domain_priors:suppression_score(vatican_ii_authority__continuity_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(vatican_ii_authority__continuity_reading),
    narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(vatican_ii_authority__continuity_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(vatican_ii_authority__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The continuity reading, by its own lights, presents Vatican II as a natural and necessary development, not an extractive or coercive imposition. Thus, base extractiveness is low (0.15), reflecting the claim of 'cost-free development.' Suppression (0.20) is also low, as the truth of continuity is presented as self-evident or discernible through proper hermeneutics, rather than requiring active coercion. Theater ratio is low (0.10) because the claim is about genuine doctrinal truth, not performance. Accessibility collapse is high (0.80) because alternative readings (rupture, overdetermination) are conceptually foreclosed by the core premise of organic continuity. Resistance (0.25) is present from traditionalist critics but is framed as a misunderstanding rather than a challenge to the constraint's fundamental validity.
 *
 * PERSPECTIVAL GAP:
 *   From the Magisterium's and supporting theologians' seats, this reading provides essential clarity and stability, making it a beneficial coordination mechanism. For traditionalist critics, however, the same reading functions to marginalize their concerns and interpretations, effectively extracting their voice and influence from official discourse. The engine will compute these divergent classifications from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The Magisterium and progressive reformers are beneficiaries and agenda-setters, as this reading legitimizes their authority and reforms (low directionality). Traditionalist critics are payers and excluded, as their interpretations are dismissed, and they bear the cost of marginalization (higher directionality). The faithful laity are both beneficiaries (receiving clear teaching) and payers (expected to accept reforms that may challenge prior understandings).
 *
 * MANDATROPHY ANALYSIS:
 *   The continuity reading actively prevents the 'founding problem' of reconciling Vatican II with tradition from becoming a 'dead' problem that leaves behind a 'piton' of unresolved theological conflict. By asserting a live solution, it maintains the constraint's perceived function and prevents its atrophy into mere theatrical maintenance. The ongoing contestation, however, means its status as a 'live' solution is not universally accepted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    continuity_definition_ambiguity,
    'What constitutes ''continuity'' in doctrinal development, and is the official interpretation sufficiently robust to encompass all aspects of Vatican II without internal tension?',
    'Further theological and historical scholarship, potentially leading to a more nuanced or revised understanding of ''organic development'' that acknowledges greater tension or redefines the boundaries of continuity.',
    'If the definition of continuity proves too elastic or insufficient to resolve perceived tensions, the extractiveness and suppression of alternative readings might be re-evaluated upward, potentially shifting the classification towards a Tangled Rope or Snare for dissenting seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(continuity_definition_ambiguity, conceptual, 'Ambiguity in the definition and application of ''doctrinal continuity''.').

omega_variable(
    suppression_of_dissent_mechanism,
    'Is the marginalization of traditionalist critics a natural consequence of their theological errors (as claimed by this reading), or is it a structural suppression mechanism enforced by institutional power?',
    'Analysis of institutional responses to dissent, including disciplinary actions, censorship, and resource allocation, compared to the theological merits of the dissenting arguments. If institutional power is disproportionately used to silence dissent regardless of its theological substance, it points to structural suppression.',
    'If found to be primarily structural suppression, the constraint''s suppression metric would be re-evaluated upward, and the classification for traditionalist seats would shift towards a Snare, as their exclusion is actively enforced rather than merely a consequence of theological disagreement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_of_dissent_mechanism, empirical, 'Structural vs. theological basis for the marginalization of dissenting voices.').

omega_variable(
    empirical_evidence_of_rupture,
    'Does historical and theological evidence, when viewed without the hermeneutic of continuity, suggest actual doctrinal or practical ruptures between Vatican II and prior tradition?',
    'Independent historical and theological analysis that explicitly sets aside the continuity hermeneutic, focusing on direct comparison of pre- and post-conciliar texts and practices, and assessing the degree of change.',
    'If significant ruptures are empirically established, the ''emerges_naturally'' claim of this reading would be challenged, and its classification as a Mountain (even a false summit) would be undermined, potentially reclassifying it as a Snare or Tangled Rope that actively suppresses evidence of rupture.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(empirical_evidence_of_rupture, empirical, 'Whether objective evidence supports claims of rupture despite the continuity reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_authority__continuity_reading, 1965, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1965, vatican_ii_authority__continuity_reading, theater_ratio, 1965, 0.08).
narrative_ontology:measurement(vati_tr_t1975, vatican_ii_authority__continuity_reading, theater_ratio, 1975, 0.09).
narrative_ontology:measurement(vati_tr_t1985, vatican_ii_authority__continuity_reading, theater_ratio, 1985, 0.1).
narrative_ontology:measurement(vati_tr_t1995, vatican_ii_authority__continuity_reading, theater_ratio, 1995, 0.1).
narrative_ontology:measurement(vati_tr_t2005, vatican_ii_authority__continuity_reading, theater_ratio, 2005, 0.1).
narrative_ontology:measurement(vati_tr_t2015, vatican_ii_authority__continuity_reading, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(vati_tr_t2025, vatican_ii_authority__continuity_reading, theater_ratio, 2025, 0.1).

% Extraction over time
narrative_ontology:measurement(vati_be_t1965, vatican_ii_authority__continuity_reading, base_extractiveness, 1965, 0.12).
narrative_ontology:measurement(vati_be_t1975, vatican_ii_authority__continuity_reading, base_extractiveness, 1975, 0.13).
narrative_ontology:measurement(vati_be_t1985, vatican_ii_authority__continuity_reading, base_extractiveness, 1985, 0.14).
narrative_ontology:measurement(vati_be_t1995, vatican_ii_authority__continuity_reading, base_extractiveness, 1995, 0.14).
narrative_ontology:measurement(vati_be_t2005, vatican_ii_authority__continuity_reading, base_extractiveness, 2005, 0.15).
narrative_ontology:measurement(vati_be_t2015, vatican_ii_authority__continuity_reading, base_extractiveness, 2015, 0.15).
narrative_ontology:measurement(vati_be_t2025, vatican_ii_authority__continuity_reading, base_extractiveness, 2025, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1965, vatican_ii_authority__continuity_reading, suppression_requirement, 1965, 0.18).
narrative_ontology:measurement(vati_su_t1975, vatican_ii_authority__continuity_reading, suppression_requirement, 1975, 0.19).
narrative_ontology:measurement(vati_su_t1985, vatican_ii_authority__continuity_reading, suppression_requirement, 1985, 0.2).
narrative_ontology:measurement(vati_su_t1995, vatican_ii_authority__continuity_reading, suppression_requirement, 1995, 0.2).
narrative_ontology:measurement(vati_su_t2005, vatican_ii_authority__continuity_reading, suppression_requirement, 2005, 0.2).
narrative_ontology:measurement(vati_su_t2015, vatican_ii_authority__continuity_reading, suppression_requirement, 2015, 0.2).
narrative_ontology:measurement(vati_su_t2025, vatican_ii_authority__continuity_reading, suppression_requirement, 2025, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_authority__continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_authority__continuity_reading, vatican_ii_authority__rupture_reading).
narrative_ontology:affects_constraint(vatican_ii_authority__continuity_reading, vatican_ii_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'vatican_ii_authority' kernel. It asserts organic doctrinal development and continuity, contrasting with the 'rupture_reading' (which claims a break) and the 'composite_overdetermination_reading' (which claims irresolvable ambiguity). Each reading instantiates a distinct constraint with its own ε value and structural properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
