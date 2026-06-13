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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: vatican_ii_doctrinal_authority__continuity_reading
 *   human_readable: Vatican II Doctrinal Authority: Continuity Reading
 *   domain: ecclesiology/institutional_history/hermeneutics
 *
 * SUMMARY:
 *   This constraint represents the 'continuity reading' of the Second Vatican
 *   Council (1962-1965), which asserts that the Council's teachings are an
 *   organic development of prior Catholic tradition, with apparent novelties
 *   being explications of implicit prior teaching. This reading is primarily
 *   advanced by the Magisterium and theologians aligned with it, aiming to
 *   maintain doctrinal stability and institutional unity. It faces challenges
 *   from both progressive and traditionalist interpretations that argue for
 *   rupture.
 *
 * KEY AGENTS:
 *   - magisterium: Agenda setter (institutional/arbitrage) — defines and enforces the continuity reading.
 *   - theologians_of_continuity: Beneficiary (organized/constrained) — their careers and intellectual projects are validated by this reading.
 *   - dissenting_theologians: Payer (moderate/constrained) — face professional and intellectual costs for challenging the continuity reading.
 *   - traditionalist_factions: Payer (organized/identity_locked) — reject the Council as a rupture, incurring costs for non-compliance.
 *   - progressive_factions: Payer (organized/constrained) — interpret the Council as a rupture, seeking further reforms, incurring costs for non-compliance.
 *   - faithful_laity: Beneficiary/Payer (powerless/identity_locked) — receive a coherent doctrinal narrative, but may bear costs of internal conflict or perceived inconsistencies.
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
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(vatican_ii_doctrinal_authority__continuity_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_doctrinal_authority__continuity_reading, rope).
narrative_ontology:human_readable(vatican_ii_doctrinal_authority__continuity_reading, "Vatican II Doctrinal Authority: Continuity Reading").
narrative_ontology:topic_domain(vatican_ii_doctrinal_authority__continuity_reading, "ecclesiology/institutional_history/hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_doctrinal_authority__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_doctrinal_authority__continuity_reading, '39eb8a15-f877-4199-b4f4-69024c5ad5d8').
narrative_ontology:cs_kernel_codification('39eb8a15-f877-4199-b4f4-69024c5ad5d8', fixed_text).
narrative_ontology:cs_authority_grounding('39eb8a15-f877-4199-b4f4-69024c5ad5d8', lineage).
narrative_ontology:cs_interpretation_layer_present('39eb8a15-f877-4199-b4f4-69024c5ad5d8').
narrative_ontology:cs_reading_relation('39eb8a15-f877-4199-b4f4-69024c5ad5d8', vatican_ii_doctrinal_authority__rupture_progressive_reading, coexists_with).
narrative_ontology:cs_reading_relation('39eb8a15-f877-4199-b4f4-69024c5ad5d8', vatican_ii_doctrinal_authority__rupture_traditionalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('39eb8a15-f877-4199-b4f4-69024c5ad5d8', vatican_ii_doctrinal_authority__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('39eb8a15-f877-4199-b4f4-69024c5ad5d8', foundational, doctrinal_infallibility_of_magisterium).
narrative_ontology:cs_axiom_status(doctrinal_infallibility_of_magisterium, holdable).
narrative_ontology:cs_axiom_grounding('39eb8a15-f877-4199-b4f4-69024c5ad5d8', doctrinal_infallibility_of_magisterium, deontological).
narrative_ontology:cs_axiom('39eb8a15-f877-4199-b4f4-69024c5ad5d8', foundational, organic_development_of_doctrine).
narrative_ontology:cs_axiom_status(organic_development_of_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('39eb8a15-f877-4199-b4f4-69024c5ad5d8', organic_development_of_doctrine, conventional).
narrative_ontology:cs_reference_frame('39eb8a15-f877-4199-b4f4-69024c5ad5d8', pre_conciliar_doctrinal_unity).
narrative_ontology:cs_drift_state('39eb8a15-f877-4199-b4f4-69024c5ad5d8', post_conciliar_implementation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('39eb8a15-f877-4199-b4f4-69024c5ad5d8', '').
narrative_ontology:cs_kernel_id(vatican_ii_doctrinal_authority__continuity_reading, vatican_ii_doctrinal_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__continuity_reading, magisterium).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__continuity_reading, theologians_of_continuity).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__continuity_reading, dissenting_theologians).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__continuity_reading, traditionalist_factions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vatican_ii_doctrinal_authority__continuity_reading, faithful_laity).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__continuity_reading, progressive_factions).
narrative_ontology:constraint_victim(vatican_ii_doctrinal_authority__continuity_reading, faithful_laity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The teaching authority of the Catholic Church (Pope and bishops in communion with him). They officially promulgate and enforce the continuity reading, viewing it as essential for doctrinal integrity and unity. They benefit from the stability and authority this reading provides.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, magisterium, agenda_setter,
    institutional, generational, arbitrage, global).

% Academic theologians and scholars whose work aligns with and supports the continuity reading. Their intellectual projects and professional standing are affirmed by this interpretation. They contribute to the hermeneutical effort to reconcile conciliar texts with tradition.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, theologians_of_continuity, beneficiary,
    organized, biographical, constrained, global).

% Theologians who find the continuity reading unconvincing or intellectually dishonest, arguing for more significant shifts or ambiguities. They face professional marginalization, censorship, or loss of academic positions for challenging the official interpretation.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, dissenting_theologians, payer,
    moderate, biographical, constrained, global).

% Groups within the Church who reject Vatican II as a rupture with tradition, often refusing to accept its liturgical or doctrinal changes. They incur costs of excommunication, marginalization, or operating in schism for their non-compliance.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, traditionalist_factions, payer,
    organized, generational, identity_locked, global).

% Groups who interpret Vatican II as a call for radical reform and see the continuity reading as an attempt to stifle the 'spirit of the Council'. They face institutional resistance and suppression of their reform efforts.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, progressive_factions, payer,
    organized, generational, constrained, global).

% The general body of Catholic believers. They benefit from a stable and unified doctrinal framework. However, they may bear the cost of intellectual dissonance when trying to reconcile perceived changes with the official continuity narrative, or experience internal conflict due to ongoing debates.
narrative_ontology:constraint_stakeholder(vatican_ii_doctrinal_authority__continuity_reading, faithful_laity, beneficiary,
    powerless, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_doctrinal_authority__continuity_reading, faithful_laity, payer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To provide a coherent and unified interpretation of the Second Vatican Council's teachings, integrating them into the existing Catholic tradition and preventing fragmentation of doctrine and practice.
% TRANSFER_FUNCTION: Transfers intellectual and spiritual authority from individual interpretations to the Magisterium's official reading, ensuring a consistent narrative of doctrinal development. It also transfers the burden of reconciling perceived novelties onto theologians and the faithful.
% ABSENT_VOICES: The voices of those who left the Church due to perceived rupture (both progressive and traditionalist) are absent from the internal debate, as are those who simply disengaged due to confusion or disillusionment. They would argue that the continuity reading fails to acknowledge genuine shifts or the need for more radical reform/restoration.
% DISAPPEARANCE_RATIONALE: If the continuity reading vanished, the Catholic Church would face immediate and severe doctrinal fragmentation. The authority of the Magisterium would be undermined, leading to multiple competing interpretations of Vatican II, potential schisms, and a loss of a unified identity. The global institution would struggle to maintain cohesion.
% FOUNDING_PROBLEM: The problem was how to reconcile the perceived novelties and reforms introduced by Vatican II with the Church's claim to unchanging doctrinal truth, avoiding both radical rupture and ossification.
% FOUNDING_PROBLEM_CORROBORATION: The Magisterium and theologians of continuity attest that the problem is live, as new challenges and interpretations continue to emerge. Dissenting theologians and traditionalist/progressive factions attest that the problem is live, but argue that the continuity reading is an inadequate or even false solution, leading to ongoing internal conflict. The persistence of these debates, documented by independent historians and sociologists of religion, corroborates the 'live' status of the problem from outside the benefiting parties.
narrative_ontology:disappearance_verdict(vatican_ii_doctrinal_authority__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(vatican_ii_doctrinal_authority__continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_doctrinal_authority__continuity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(vatican_ii_doctrinal_authority__continuity_reading, 'none', 1).

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
 *   The continuity reading functions as a Rope because it genuinely attempts to coordinate doctrinal understanding and maintain unity within a large institution. However, it exhibits moderate extractiveness (0.25) and suppression (0.4) because maintaining this reading requires active enforcement against alternative interpretations, particularly those advocating for rupture. The 'apparent novelties' often require significant hermeneutical effort to reconcile with prior teaching, and those who find this reconciliation unconvincing bear the cost of intellectual dissonance or institutional marginalization. Theater ratio (0.3) reflects the performative aspect of constantly re-asserting continuity in the face of perceived changes, but there is still substantial functional coordination.
 *
 * PERSPECTIVAL GAP:
 *   From the Magisterium's perspective, the continuity reading is a necessary and natural coordination mechanism for doctrinal integrity. From the perspective of dissenting theologians or traditionalist factions, it can appear as an imposed interpretation that suppresses legitimate questions or critiques, leading to a higher perceived extractiveness and suppression.
 *
 * DIRECTIONALITY LOGIC:
 *   The Magisterium and theologians of continuity are beneficiaries, as their authority and intellectual framework are affirmed. Dissenting theologians, traditionalist factions, and progressive factions are payers, as they face pressure to conform or are marginalized for their alternative readings. The faithful laity are both beneficiaries (receiving a stable doctrine) and payers (bearing the costs of internal conflict or intellectual gymnastics to accept the official reading).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (maintaining doctrinal continuity) is still live, but its status is contested. The continuity reading prevents the mislabeling of genuine doctrinal development as rupture, but also risks mislabeling genuine rupture as development if the hermeneutical effort becomes too strained. The ongoing debate suggests the coordination function is active, but the costs of maintaining it are borne by those whose interpretations are suppressed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine ''continuity'' reading of Vatican II, or is it a selective interpretation designed to maintain institutional authority?',
    'Historical-theological analysis of pre-conciliar sources and conciliar documents, focusing on explicit vs. implicit doctrinal statements and the evolution of theological language. Comparison with the ''rupture_progressive_reading'' and ''rupture_traditionalist_reading'' to identify points of irreconcilable difference.',
    'If a genuine continuity, the constraint functions as a Rope, coordinating doctrinal understanding. If a selective interpretation, it functions as a Tangled Rope or Snare, extracting compliance by suppressing alternative readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Ambiguity of Vatican II as organic development vs. selective interpretation.').

omega_variable(
    doctrinal_vs_pastoral_ambiguity,
    'To what extent do ''apparent novelties'' represent genuine doctrinal shifts versus merely pastoral or liturgical adaptations?',
    'Detailed textual analysis of specific conciliar documents and subsequent magisterial interpretations, distinguishing between dogmatic pronouncements and prudential directives. Examination of the ''spirit of the Council'' vs. the ''letter of the Council'' debate.',
    'If novelties are purely pastoral, extractiveness on doctrinal change is low. If they imply unacknowledged doctrinal shifts, extractiveness is higher, as compliance is extracted under a false premise of continuity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_vs_pastoral_ambiguity, empirical, 'Distinguishing doctrinal change from pastoral adaptation in Vatican II.').

omega_variable(
    sibling_reading_impact_rupture_progressive,
    'How would the ''rupture_progressive_reading'' alter the structural properties of this ''continuity_reading''?',
    'Analysis of the ''spirit of the Council'' vs. ''letter of the Council'' debate and its implications for ongoing reform. Examination of how a progressive reading would challenge the authority of the Magisterium to define continuity.',
    'A rupture_progressive_reading would increase resistance to the continuity reading, potentially raising suppression requirements for the Magisterium to maintain its interpretation. It would also challenge the perceived naturalness of the continuity claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_impact_rupture_progressive, conceptual, 'Impact of rupture_progressive_reading on continuity_reading.').

omega_variable(
    sibling_reading_impact_rupture_traditionalist,
    'How would the ''rupture_traditionalist_reading'' alter the structural properties of this ''continuity_reading''?',
    'Analysis of traditionalist critiques of Vatican II, focusing on specific doctrinal ambiguities or perceived errors. Examination of how a traditionalist reading would challenge the legitimacy of the post-conciliar Magisterium.',
    'A rupture_traditionalist_reading would increase resistance to the continuity reading, potentially raising suppression requirements for the Magisterium to maintain its interpretation. It would also challenge the perceived naturalness of the continuity claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_impact_rupture_traditionalist, conceptual, 'Impact of rupture_traditionalist_reading on continuity_reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_doctrinal_authority__continuity_reading, 1965, 2015).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t0, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(vati_tr_t10, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(vati_tr_t20, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(vati_tr_t30, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 30, 0.3).
narrative_ontology:measurement(vati_tr_t40, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 40, 0.29).
narrative_ontology:measurement(vati_tr_t50, vatican_ii_doctrinal_authority__continuity_reading, theater_ratio, 50, 0.3).

% Extraction over time
narrative_ontology:measurement(vati_be_t0, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(vati_be_t10, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 10, 0.22).
narrative_ontology:measurement(vati_be_t20, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 20, 0.23).
narrative_ontology:measurement(vati_be_t30, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 30, 0.25).
narrative_ontology:measurement(vati_be_t40, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 40, 0.24).
narrative_ontology:measurement(vati_be_t50, vatican_ii_doctrinal_authority__continuity_reading, base_extractiveness, 50, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t0, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(vati_su_t10, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 10, 0.35).
narrative_ontology:measurement(vati_su_t20, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 20, 0.38).
narrative_ontology:measurement(vati_su_t30, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 30, 0.4).
narrative_ontology:measurement(vati_su_t40, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 40, 0.39).
narrative_ontology:measurement(vati_su_t50, vatican_ii_doctrinal_authority__continuity_reading, suppression_requirement, 50, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_doctrinal_authority__continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__continuity_reading, vatican_ii_doctrinal_authority__rupture_progressive_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__continuity_reading, vatican_ii_doctrinal_authority__rupture_traditionalist_reading).
narrative_ontology:affects_constraint(vatican_ii_doctrinal_authority__continuity_reading, vatican_ii_doctrinal_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Vatican II doctrinal authority kernel. Its structural properties are distinct from the 'rupture_progressive_reading' and 'rupture_traditionalist_reading', which emphasize discontinuity and have different beneficiary/victim structures and extractiveness profiles. All three are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
