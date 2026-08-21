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
 *   This constraint represents the 'hermeneutic of continuity' reading of
 *   Vatican II, which asserts that the Council's teachings are an organic
 *   development within the unchanging tradition of the Catholic Church.
 *   Apparent novelties are interpreted as explications of implicit prior
 *   teaching, or as prudential adaptations to modern circumstances, rather
 *   than doctrinal shifts. This reading aims to preserve doctrinal stability
 *   and institutional unity. The low extractiveness reflects that, from this
 *   reading's perspective, the constraint primarily coordinates understanding
 *   rather than extracting resources, though it does impose costs on those
 *   who dissent.
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
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_doctrinal_authority__continuity_reading, rope).
narrative_ontology:human_readable(vatican_ii_doctrinal_authority__continuity_reading, "Vatican II Doctrinal Authority (Continuity Reading)").
narrative_ontology:topic_domain(vatican_ii_doctrinal_authority__continuity_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_doctrinal_authority__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_doctrinal_authority__continuity_reading, 'ae8483b1-1903-472d-ad49-ce295482bbdf').
narrative_ontology:cs_kernel_codification('ae8483b1-1903-472d-ad49-ce295482bbdf', fixed_text).
narrative_ontology:cs_authority_grounding('ae8483b1-1903-472d-ad49-ce295482bbdf', lineage).
narrative_ontology:cs_interpretation_layer_present('ae8483b1-1903-472d-ad49-ce295482bbdf').
narrative_ontology:cs_reading_relation('ae8483b1-1903-472d-ad49-ce295482bbdf', vatican_ii_doctrinal_authority__rupture_progressive_reading, coexists_with).
narrative_ontology:cs_reading_relation('ae8483b1-1903-472d-ad49-ce295482bbdf', vatican_ii_doctrinal_authority__rupture_traditionalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('ae8483b1-1903-472d-ad49-ce295482bbdf', vatican_ii_doctrinal_authority__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('ae8483b1-1903-472d-ad49-ce295482bbdf', foundational, organic_doctrinal_development).
narrative_ontology:cs_axiom_status(organic_doctrinal_development, holdable).
narrative_ontology:cs_axiom_grounding('ae8483b1-1903-472d-ad49-ce295482bbdf', organic_doctrinal_development, deontological).
narrative_ontology:cs_axiom('ae8483b1-1903-472d-ad49-ce295482bbdf', foundational, magisterial_interpretive_primacy).
narrative_ontology:cs_axiom_status(magisterial_interpretive_primacy, holdable).
narrative_ontology:cs_axiom_grounding('ae8483b1-1903-472d-ad49-ce295482bbdf', magisterial_interpretive_primacy, conventional).
narrative_ontology:cs_reference_frame('ae8483b1-1903-472d-ad49-ce295482bbdf', pre_conciliar_doctrinal_unity).
narrative_ontology:cs_drift_state('ae8483b1-1903-472d-ad49-ce295482bbdf', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('ae8483b1-1903-472d-ad49-ce295482bbdf', '').
narrative_ontology:cs_kernel_id(vatican_ii_doctrinal_authority__continuity_reading, vatican_ii_doctrinal_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__continuity_reading, magisterium).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__continuity_reading, theologians_of_continuity).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__continuity_reading, dissenting_theologians).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__continuity_reading, traditionalist_groups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__continuity_reading, faithful_laity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The teaching authority of the Catholic Church, which promulgates and interprets the documents of Vatican II. This reading is largely articulated and defended by the Magisterium to maintain doctrinal coherence and authority.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, magisterium, agenda_setter,
    institutional, civilizational, identity_locked, universal).

% Academics and scholars who develop and propagate the hermeneutic of continuity, finding career and intellectual validation within this framework. They benefit from the stability and coherence this reading provides to their theological work.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, theologians_of_continuity, beneficiary,
    organized, generational, constrained, global).

% Theologians who interpret Vatican II as a more significant break or who find ambiguities that challenge pre-conciliar teaching. They face pressure to conform to the continuity reading, risking marginalization or censure if they do not.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, dissenting_theologians, payer,
    moderate, biographical, constrained, global).

% Groups who reject Vatican II as a rupture with tradition, viewing its 'novelties' as errors. They are often disciplined or excommunicated for non-compliance with the Council's teachings as interpreted by the Magisterium, bearing the cost of non-conformity.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, traditionalist_groups, payer,
    powerless, generational, identity_locked, local).

% The general body of believers who seek clear and consistent teaching. This reading provides a sense of stability and reassurance that the Church's doctrine remains unchanged, fostering trust and adherence.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, faithful_laity, beneficiary,
    moderate, biographical, constrained, local).

% Other Christian denominations and religious bodies who observe the internal debates within Catholicism regarding Vatican II. Their engagement with the Catholic Church is influenced by how the Council's teachings are interpreted and applied.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, ecumenical_partners, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains doctrinal coherence and institutional unity within the Catholic Church by providing a framework for interpreting a significant historical event (Vatican II) in continuity with prior teaching.
% TRANSFER_FUNCTION: Transfers interpretive authority and legitimacy from potentially ambiguous conciliar texts to the Magisterium and its approved theological schools, while transferring the burden of reconciling apparent contradictions to dissenting parties.
% ABSENT_VOICES: The 'spirit of the Council' progressives, who would argue for a more radical interpretation of Vatican II, and extreme traditionalists, who reject the Council entirely, are both marginalized by this reading. Their voices are present in the broader discourse but excluded from the official interpretive framework.
% DISAPPEARANCE_RATIONALE: If the continuity reading vanished, the Catholic Church would face a severe crisis of doctrinal authority and internal schism. Different factions would claim competing interpretations of Vatican II, leading to fragmentation and a fundamental reordering of its institutional structure and theological discourse.
% FOUNDING_PROBLEM: The problem of reconciling the apparent innovations of Vatican II with the Catholic Church's claim to unchanging doctrine, and maintaining institutional unity amidst diverse interpretations.
% FOUNDING_PROBLEM_CORROBORATION: The Magisterium and theologians of continuity attest the problem is live, citing ongoing challenges from both progressive and traditionalist interpretations. Independent historians and sociologists of religion corroborate the existence of this interpretive tension as a persistent feature of post-conciliar Catholicism, though they may dispute the Magisterium's specific resolution.
narrative_ontology:disappearance_verdict(vatican_ii_doctrinal_authority__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_doctrinal_authority__continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_doctrinal_authority__continuity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is low (0.25) because the primary function is to maintain doctrinal coherence, which is a coordination benefit for the institution. However, it is not zero, as it extracts conformity from dissenting voices. Suppression (0.4) is moderate, reflecting the institutional pressure on theologians and groups to adhere to this interpretation, with consequences for non-compliance. Theater ratio (0.3) is present because some efforts to reconcile apparent contradictions may involve rhetorical rather than substantive theological work, but a core function of maintaining unity remains.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Magisterium and continuity theologians, this is a necessary and beneficial interpretive framework (a Rope). From the perspective of dissenting theologians and traditionalist groups, it can feel more extractive, as it suppresses alternative interpretations and imposes a specific understanding of history and doctrine.
 *
 * DIRECTIONALITY LOGIC:
 *   The Magisterium and theologians of continuity are beneficiaries, as this reading solidifies their authority and intellectual framework. Dissenting theologians and traditionalist groups are payers, bearing the cost of intellectual and institutional conformity. Faithful laity are beneficiaries, receiving clear and stable teaching. Ecumenical partners are observers, their engagement shaped by this internal Catholic interpretive framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    doctrinal_vs_pastoral_ambiguity,
    'Are the ''novelties'' of Vatican II genuinely doctrinal developments in continuity, or are some primarily pastoral/disciplinary shifts that implicitly contradict prior doctrinal teaching?',
    'Long-term historical-theological analysis of the reception of Vatican II, coupled with formal Magisterial clarifications that explicitly address specific points of contention and their relationship to prior infallible teaching.',
    'If some ''novelties'' are found to be implicitly contradictory on doctrinal points, the extractiveness and suppression of this reading would increase significantly, as it would be seen as enforcing a false continuity. If all are confirmed as continuous doctrinal development or purely pastoral, the reading''s legitimacy as a Rope would be strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_vs_pastoral_ambiguity, conceptual, 'Ambiguity regarding the nature of change introduced by Vatican II.').

omega_variable(
    enforcement_of_continuity_vs_organic_reception,
    'To what extent is the ''hermeneutic of continuity'' enforced through institutional power (e.g., appointments, censorship) versus being organically received and adopted by the faithful and theologians?',
    'Sociological studies of theological discourse and Magisterial interventions, analysis of appointment patterns in theological faculties, and surveys of lay belief regarding Vatican II''s interpretation.',
    'If enforcement is found to be the primary driver, the suppression metric would be higher, and the constraint would lean more towards a Tangled Rope or Snare. If organic reception is dominant, it would reinforce the Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_of_continuity_vs_organic_reception, empirical, 'The balance between institutional enforcement and organic acceptance of the continuity reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_doctrinal_authority__continuity_reading, 1965, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1965, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 1965, 0.1).
narrative_ontology:measurement(vati_tr_t1975, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 1975, 0.2).
narrative_ontology:measurement(vati_tr_t1985, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 1985, 0.25).
narrative_ontology:measurement(vati_tr_t1995, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 1995, 0.28).
narrative_ontology:measurement(vati_tr_t2005, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 2005, 0.3).
narrative_ontology:measurement(vati_tr_t2015, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 2015, 0.3).
narrative_ontology:measurement(vati_tr_t2024, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 2024, 0.3).

% Extraction over time
narrative_ontology:measurement(vati_be_t1965, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 1965, 0.15).
narrative_ontology:measurement(vati_be_t1975, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 1975, 0.2).
narrative_ontology:measurement(vati_be_t1985, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 1985, 0.22).
narrative_ontology:measurement(vati_be_t1995, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 1995, 0.23).
narrative_ontology:measurement(vati_be_t2005, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 2005, 0.24).
narrative_ontology:measurement(vati_be_t2015, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 2015, 0.25).
narrative_ontology:measurement(vati_be_t2024, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 2024, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1965, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 1965, 0.3).
narrative_ontology:measurement(vati_su_t1975, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 1975, 0.35).
narrative_ontology:measurement(vati_su_t1985, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 1985, 0.38).
narrative_ontology:measurement(vati_su_t1995, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 1995, 0.4).
narrative_ontology:measurement(vati_su_t2005, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 2005, 0.4).
narrative_ontology:measurement(vati_su_t2015, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 2015, 0.4).
narrative_ontology:measurement(vati_su_t2024, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 2024, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_doctrinal_authority__continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__continuity_reading, vatican_ii_doctrinal_authority__rupture_progressive_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__continuity_reading, vatican_ii_doctrinal_authority__rupture_traditionalist_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__continuity_reading, vatican_ii_doctrinal_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Vatican II doctrinal authority kernel. Its interpretation of continuity directly influences the perceived legitimacy and structural dynamics of the rupture readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
