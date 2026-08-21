% ============================================================================
% CONSTRAINT STORY: vatican_ii_doctrinal_authority__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_doctrinal_authority__continuity_reading, []).

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
 *   constraint_id: vatican_ii_doctrinal_authority__continuity_reading
 *   human_readable: Vatican II Doctrinal Authority (Continuity Reading)
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   This constraint represents the 'continuity reading' of Vatican II, which
 *   asserts that the Council's teachings are an organic development of prior
 *   Catholic tradition, with apparent novelties being explications of
 *   implicit prior teaching. This reading is the official hermeneutic of the
 *   Magisterium. While it aims to preserve doctrinal unity (low
 *   extractiveness on core doctrine), it exerts significant pressure on those
 *   who perceive rupture (traditionalists) and requires active enforcement to
 *   maintain its interpretive dominance. The 'continuity' claim itself is a
 *   coordination mechanism for the Church's authority.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_doctrinal_authority__continuity_reading, 0.25).
domain_priors:suppression_score(vatican_ii_doctrinal_authority__continuity_reading, 0.4).
domain_priors:theater_ratio(vatican_ii_doctrinal_authority__continuity_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_doctrinal_authority__continuity_reading, rope).
narrative_ontology:human_readable(vatican_ii_doctrinal_authority__continuity_reading, "Vatican II Doctrinal Authority (Continuity Reading)").
narrative_ontology:topic_domain(vatican_ii_doctrinal_authority__continuity_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_doctrinal_authority__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_doctrinal_authority__continuity_reading, '8be7aae4-747a-4b24-9aa6-afeb2d70e1a5').
narrative_ontology:cs_kernel_codification('8be7aae4-747a-4b24-9aa6-afeb2d70e1a5', fixed_text).
narrative_ontology:cs_authority_grounding('8be7aae4-747a-4b24-9aa6-afeb2d70e1a5', lineage).
narrative_ontology:cs_interpretation_layer_present('8be7aae4-747a-4b24-9aa6-afeb2d70e1a5').
narrative_ontology:cs_reading_relation('8be7aae4-747a-4b24-9aa6-afeb2d70e1a5', vatican_ii_doctrinal_authority__rupture_progressive_reading, coexists_with).
narrative_ontology:cs_reading_relation('8be7aae4-747a-4b24-9aa6-afeb2d70e1a5', vatican_ii_doctrinal_authority__rupture_traditionalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('8be7aae4-747a-4b24-9aa6-afeb2d70e1a5', vatican_ii_doctrinal_authority__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('8be7aae4-747a-4b24-9aa6-afeb2d70e1a5', foundational, doctrine_develops_organically).
narrative_ontology:cs_axiom_status(doctrine_develops_organically, holdable).
narrative_ontology:cs_axiom_grounding('8be7aae4-747a-4b24-9aa6-afeb2d70e1a5', doctrine_develops_organically, deontological).
narrative_ontology:cs_axiom('8be7aae4-747a-4b24-9aa6-afeb2d70e1a5', foundational, magisterium_sole_authentic_interpreter).
narrative_ontology:cs_axiom_status(magisterium_sole_authentic_interpreter, holdable).
narrative_ontology:cs_axiom_grounding('8be7aae4-747a-4b24-9aa6-afeb2d70e1a5', magisterium_sole_authentic_interpreter, conventional).
narrative_ontology:cs_reference_frame('8be7aae4-747a-4b24-9aa6-afeb2d70e1a5', pre_conciliar_doctrinal_unity).
narrative_ontology:cs_drift_state('8be7aae4-747a-4b24-9aa6-afeb2d70e1a5', post_conciliar_implementation_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('8be7aae4-747a-4b24-9aa6-afeb2d70e1a5', '').
narrative_ontology:cs_kernel_id(vatican_ii_doctrinal_authority__continuity_reading, vatican_ii_doctrinal_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__continuity_reading, magisterium).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__continuity_reading, theologians_of_nouvelle_theologie).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__continuity_reading, traditionalist_catholics).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__continuity_reading, ecumenical_partners).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__continuity_reading, development_of_doctrine_theory).
narrative_ontology:constraint_vindicates(vatican_ii_doctrinal_authority__continuity_reading, organic_growth_of_tradition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The teaching authority of the Catholic Church, which promulgates and defends the continuity reading as the official interpretation of Vatican II. It benefits from maintaining a coherent narrative of doctrinal development.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, magisterium, agenda_setter,
    institutional, generational, identity_locked, universal).

% Theologians whose work influenced Vatican II and who subsequently interpret the Council's teachings as a legitimate, organic development of prior tradition. Their careers and intellectual legacy are affirmed by this reading.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, theologians_of_nouvelle_theologie, beneficiary,
    powerful, biographical, constrained, global).

% Catholics who perceive Vatican II as a rupture with tradition and find the continuity reading unconvincing or disingenuous. They bear the cost of being marginalized or disciplined for rejecting the official interpretation, often feeling alienated from the mainstream Church.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, traditionalist_catholics, payer,
    powerless, biographical, identity_locked, global).

% Catholics who embrace the 'spirit of Vatican II' and often push for further reforms. While they may not fully align with the strict continuity reading, they generally accept the Council's legitimacy and its call for aggiornamento, often viewing the continuity reading as a necessary but incomplete step.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, progressive_catholics, observer,
    moderate, biographical, mobile, global).

% Other Christian denominations and religious traditions who engage in dialogue with the Catholic Church. The continuity reading, by emphasizing development rather than radical change, provides a more stable and predictable partner for ecumenical efforts.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, ecumenical_partners, beneficiary,
    organized, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent hermeneutic framework for understanding the Second Vatican Council's teachings, ensuring doctrinal stability and unity within the Catholic Church by integrating apparent novelties into existing tradition.
% TRANSFER_FUNCTION: Transfers interpretive authority and legitimacy to the Magisterium and theologians who uphold the continuity reading, while demanding intellectual assent and practical conformity from all Catholics, particularly those who perceive rupture.
% ABSENT_VOICES: Those who advocate for a 'rupture' reading (both progressive and traditionalist) are present but often marginalized or disciplined within official discourse. Their perspectives are actively suppressed in favor of the continuity narrative.
% DISAPPEARANCE_RATIONALE: If the continuity reading vanished, the Catholic Church would face a profound crisis of identity and authority. The Council's teachings would be open to radically divergent interpretations, leading to potential schism and a breakdown of doctrinal unity. The Magisterium's legitimacy would be severely challenged.
% FOUNDING_PROBLEM: The Second Vatican Council introduced pastoral and theological developments that appeared to deviate from prior Catholic teaching, creating a need to reconcile these with the Church's claim to unchanging truth.
% FOUNDING_PROBLEM_CORROBORATION: The Magisterium and its supporting theologians attest that the problem of reconciling tradition and development remains live. Historians of theology and some ecumenical partners also acknowledge the hermeneutical challenge, though they may not endorse the specific continuity solution.
narrative_ontology:disappearance_verdict(vatican_ii_doctrinal_authority__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_doctrinal_authority__continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_doctrinal_authority__continuity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(vatican_ii_doctrinal_authority__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_doctrinal_authority__continuity_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_doctrinal_authority__continuity_reading_tests).
:- end_tests(vatican_ii_doctrinal_authority__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is relatively low (0.25) because the core doctrinal claims are presented as consistent, minimizing direct 'extraction' from belief itself. However, there is extraction in terms of intellectual conformity and suppression of dissenting interpretations. Suppression (0.4) is moderate, reflecting the active effort to marginalize or discipline those who reject the continuity reading. Theater ratio (0.3) is present as some efforts to demonstrate continuity require elaborate theological arguments that may obscure genuine shifts in emphasis or practice. Accessibility collapse is high (0.7) because for many Catholics, rejecting the official hermeneutic means rejecting the Magisterium itself, which is a high-cost exit. Resistance (0.3) is moderate, primarily from traditionalist groups.
 *
 * PERSPECTIVAL GAP:
 *   From the Magisterium's perspective, this is a necessary 'rope' for doctrinal unity. From traditionalist Catholics, it can feel like a 'snare' of enforced conformity that denies perceived historical rupture. The engine's classification will reflect the structural position of each seat.
 *
 * DIRECTIONALITY LOGIC:
 *   The Magisterium and theologians who championed the 'nouvelle théologie' are beneficiaries, as the continuity reading legitimizes their authority and intellectual contributions. Traditionalist Catholics are payers, as they bear the cost of intellectual and spiritual alienation for rejecting the official interpretation. Progressive Catholics are observers, as they often seek to move beyond the strict continuity reading but do not directly pay into its enforcement. Ecumenical partners benefit from the perceived stability and coherence of Catholic doctrine.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    doctrinal_vs_pastoral_ambiguity,
    'To what extent are the ''novelties'' of Vatican II genuinely doctrinal developments versus prudential pastoral adaptations, and how does the continuity reading distinguish them?',
    'Detailed historical-theological analysis of specific conciliar texts and their reception, focusing on whether changes were presented as binding dogma or as disciplinary/pastoral shifts.',
    'If many ''novelties'' are demonstrably pastoral, the continuity reading''s claim of doctrinal stability is strengthened. If they are doctrinal shifts, the extractiveness of enforcing ''continuity'' increases for those who perceive rupture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_vs_pastoral_ambiguity, empirical, 'Ambiguity in distinguishing doctrinal development from pastoral adaptation in Vatican II.').

omega_variable(
    interpretive_authority_legitimacy,
    'Is the Magisterium''s claim to be the sole authentic interpreter of Vatican II universally accepted, or is it contested by significant factions within the Church?',
    'Sociological studies of Catholic belief and practice, analysis of dissenting theological movements, and examination of episcopal conferences'' reception of official hermeneutics.',
    'If interpretive authority is widely contested, the suppression metric for the continuity reading is higher than currently measured, reflecting the ongoing effort to enforce a non-consensual interpretation. If widely accepted, the constraint operates more as a genuine rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_authority_legitimacy, empirical, 'Contestation over the Magisterium''s sole interpretive authority for Vatican II.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_doctrinal_authority__continuity_reading, 1965, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1965, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 1965, 0.1).
narrative_ontology:measurement(vati_tr_t1980, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 1980, 0.2).
narrative_ontology:measurement(vati_tr_t1995, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 1995, 0.25).
narrative_ontology:measurement(vati_tr_t2010, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 2010, 0.28).
narrative_ontology:measurement(vati_tr_t2024, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 2024, 0.3).

% Extraction over time
narrative_ontology:measurement(vati_be_t1965, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 1965, 0.2).
narrative_ontology:measurement(vati_be_t1980, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 1980, 0.22).
narrative_ontology:measurement(vati_be_t1995, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 1995, 0.23).
narrative_ontology:measurement(vati_be_t2010, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 2010, 0.24).
narrative_ontology:measurement(vati_be_t2024, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 2024, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1965, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 1965, 0.3).
narrative_ontology:measurement(vati_su_t1980, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 1980, 0.35).
narrative_ontology:measurement(vati_su_t1995, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 1995, 0.38).
narrative_ontology:measurement(vati_su_t2010, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 2010, 0.39).
narrative_ontology:measurement(vati_su_t2024, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 2024, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_doctrinal_authority__continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__continuity_reading, vatican_ii_doctrinal_authority__rupture_progressive_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__continuity_reading, vatican_ii_doctrinal_authority__rupture_traditionalist_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__continuity_reading, vatican_ii_doctrinal_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Vatican II doctrinal authority kernel. It asserts continuity with tradition, contrasting with rupture-focused readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
