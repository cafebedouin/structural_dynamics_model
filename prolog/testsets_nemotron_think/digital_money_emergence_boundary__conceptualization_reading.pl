% ============================================================================
% CONSTRAINT STORY: digital_money_emergence_boundary__conceptualization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_emergence_boundary__conceptualization_reading, []).

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
 *   constraint_id: digital_money_emergence_boundary__conceptualization_reading
 *   human_readable: Digital Money Emergence at Theoretical Conceptualization (1960s telecom, 1985 Chaum)
 *   domain: monetary_economics/financial_history/technology_governance
 *
 * SUMMARY:
 *   This constraint story instantiates the conceptualization_reading of the
 *   digital_money_emergence_boundary kernel: the claim that digital money
 *   emerged when it became theoretically thinkable, anchored in 1960s
 *   telecommunications advances and David Chaum's 1985 formalization of blind
 *   signatures for digital cash. The reading draws the earliest possible
 *   boundary — at the moment of theoretical possibility — and asserts this as
 *   the natural origin point for the category 'digital money.' The academic
 *   cryptography community (Chaum, his students, the early e-cash literature)
 *   benefits from this boundary by establishing priority for their formal
 *   contributions over later infrastructure deployments (ATMs, ACH, SWIFT)
 *   and consumer-facing instruments (e-purses, stored-value cards). The
 *   constraint presents itself as a mountain — a natural law of conceptual
 *   history — but declares beneficiaries, triggering FSM evaluation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_emergence_boundary__conceptualization_reading, 0.15).
domain_priors:suppression_score(digital_money_emergence_boundary__conceptualization_reading, 0.1).
domain_priors:theater_ratio(digital_money_emergence_boundary__conceptualization_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_emergence_boundary__conceptualization_reading, mountain).
narrative_ontology:human_readable(digital_money_emergence_boundary__conceptualization_reading, "Digital Money Emergence at Theoretical Conceptualization (1960s telecom, 1985 Chaum)").
narrative_ontology:topic_domain(digital_money_emergence_boundary__conceptualization_reading, "monetary_economics/financial_history/technology_governance").

domain_priors:emerges_naturally(digital_money_emergence_boundary__conceptualization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_emergence_boundary__conceptualization_reading, 'c3116a02-cccb-432c-8f11-4171fda9f914').
narrative_ontology:cs_kernel_codification('c3116a02-cccb-432c-8f11-4171fda9f914', formalized).
narrative_ontology:cs_authority_grounding('c3116a02-cccb-432c-8f11-4171fda9f914', expertise).
narrative_ontology:cs_interpretation_layer_present('c3116a02-cccb-432c-8f11-4171fda9f914').
narrative_ontology:cs_reading_relation('c3116a02-cccb-432c-8f11-4171fda9f914', digital_money_emergence_boundary__infrastructure_reading, coexists_with).
narrative_ontology:cs_reading_relation('c3116a02-cccb-432c-8f11-4171fda9f914', digital_money_emergence_boundary__consumer_holdings_reading, coexists_with).
narrative_ontology:cs_axiom('c3116a02-cccb-432c-8f11-4171fda9f914', foundational, theoretical_formalization_defines_emergence).
narrative_ontology:cs_axiom_status(theoretical_formalization_defines_emergence, holdable).
narrative_ontology:cs_axiom_grounding('c3116a02-cccb-432c-8f11-4171fda9f914', theoretical_formalization_defines_emergence, conventional).
narrative_ontology:cs_axiom('c3116a02-cccb-432c-8f11-4171fda9f914', secondary, priority_belongs_to_first_formalization).
narrative_ontology:cs_axiom_status(priority_belongs_to_first_formalization, holdable).
narrative_ontology:cs_axiom_grounding('c3116a02-cccb-432c-8f11-4171fda9f914', priority_belongs_to_first_formalization, conventional).
narrative_ontology:cs_reference_frame('c3116a02-cccb-432c-8f11-4171fda9f914', chaum_1985_formalization_as_origin).
narrative_ontology:cs_drift_state('c3116a02-cccb-432c-8f11-4171fda9f914', post_cryptocurrency_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c3116a02-cccb-432c-8f11-4171fda9f914', '').
narrative_ontology:cs_kernel_id(digital_money_emergence_boundary__conceptualization_reading, digital_money_emergence_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__conceptualization_reading, academic_cryptography_community).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__conceptualization_reading, digital_money_priority_claimants).
narrative_ontology:constraint_vindicates(digital_money_emergence_boundary__conceptualization_reading, chaum_1985_formalizes_digital_cash).
narrative_ontology:constraint_vindicates(digital_money_emergence_boundary__conceptualization_reading, theoretical_conceptualization_precedes_infrastructure).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Establishes priority for Chaum's 1985 formalization and the 1960s theoretical foundations as the true origin of digital money. Collects citation authority, field-definition power, and genealogical primacy for the cryptography -> digital cash lineage. Can exit by adopting a pluralistic boundary framework but loses the priority claim.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, academic_cryptography_community, beneficiary,
    organized, generational, mobile, global).

% Individual researchers and research groups who anchor their contribution narratives in being 'first' at the theoretical layer. The boundary functions as a career and funding credential. Exit means reframing contributions without the 'origin' credential — possible but costly in academic capital.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, digital_money_priority_claimants, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(digital_money_emergence_boundary__conceptualization_reading, digital_money_priority_claimants, agenda_setter).

% Built and operated the electronic funds transfer systems (ATM networks, ACH, SWIFT) that moved actual value electronically decades before Chaum. Their preferred boundary (operational deployment) is not represented in this reading. They would object that theory without deployment is not money, but they are not in the conversation when the conceptualization boundary is asserted as 'the' origin.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, infrastructure_engineers, excluded,
    organized, biographical, mobile, global).

% Developed and deployed consumer-facing digital instruments (stored-value cards, e-purses, early online banking). Their preferred boundary (retail accessibility) is excluded. They would argue money is defined by what people can hold and spend, not by what cryptographers can prove. Not structurally targeted — merely excluded from this reading's frame.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, consumer_finance_practitioners, excluded,
    organized, biographical, mobile, global).

% Study the contested boundary as a historiographical problem. They see all three readings as live positions reflecting different methodological commitments (intellectual history vs. institutional history vs. social history of money). They do not collect from or pay into any single boundary.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, monetary_historians, observer,
    analytical, generational, analytical, global).

% Produce official money classifications (M0-M4, CBDC taxonomies). They must choose or combine boundaries for operational definitions. They are influenced by all three readings but are not bound to any single one — their choice has regulatory consequences.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, central_bank_taxonomy_authorities, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, clean origin date for the category 'digital money' that anchors intellectual history, citation practices, and field identity for the academic cryptography community. Solves the coordination problem of 'where does our field begin?' by fixing the boundary at theoretical formalization.
% TRANSFER_FUNCTION: Moves definitional authority and priority credit from infrastructure deployers (banks, telecom operators, standards bodies) and consumer-facing innovators (card networks, fintechs) to the academic cryptography community that produced the theoretical formalization. The transfer is in epistemic capital — citations, genealogy, field-definition power — not monetary value.
% ABSENT_VOICES: Infrastructure engineers who built operational electronic funds transfer systems (ATM networks, ACH, SWIFT) in the 1960s-1970s, and consumer finance practitioners who deployed retail digital instruments (e-purses, stored-value cards) in the 1990s. Both groups would argue their deployment milestones are the genuine emergence of digital money as a social technology, not the theoretical formalization. They are excluded because this reading defines 'emergence' as 'thinkability' rather than 'deployment' or 'accessibility.'
% DISAPPEARANCE_RATIONALE: If the conceptualization boundary vanished overnight, the actual history of digital money — the infrastructure built, the instruments deployed, the value transferred — would not change. Only the academic framing of 'where it began' would become contested. Central banks would still classify money by liability structure and accessibility; practitioners would still build on operational milestones. The constraint's disappearance rearranges intellectual genealogy, not monetary reality.
% FOUNDING_PROBLEM: The academic cryptography community needed a clean origin story for digital money as a distinct field of study, separable from banking technology, telecommunications, and computer science. Chaum's 1985 blind signature paper provided a formal foundation that could be claimed as the field's birth certificate.
% FOUNDING_PROBLEM_CORROBORATION: Chaum's 1985 paper and the 1960s telecommunications literature attest the theoretical possibility. However, monetary historians (e.g., James, Schumpeter scholars), central bank archivists, and infrastructure engineers corroborate that the founding problem (a clean origin story for the field) is not the same as the actual emergence of digital money as a social technology — the latter has multiple valid boundaries. No single community outside the academic cryptography lineage treats the conceptualization boundary as the exclusive origin.
narrative_ontology:disappearance_verdict(digital_money_emergence_boundary__conceptualization_reading, world_unchanged).
narrative_ontology:founding_problem_status(digital_money_emergence_boundary__conceptualization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_emergence_boundary__conceptualization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(digital_money_emergence_boundary__conceptualization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_emergence_boundary__conceptualization_reading, 0.15, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_emergence_boundary__conceptualization_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, ExtMetricName, E),
    domain_priors:suppression_score(digital_money_emergence_boundary__conceptualization_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(digital_money_emergence_boundary__conceptualization_reading),
    narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(digital_money_emergence_boundary__conceptualization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the constraint is primarily a definitional boundary claim, not an operational mechanism that transfers resources. However, the academic community does extract priority/credit from the boundary's acceptance. Suppression is very low (0.10) — no one is coerced into accepting this boundary; alternative readings (infrastructure, consumer holdings) are openly debated. Theater is minimal (0.05) — the boundary claim performs little ritual maintenance. Accessibility collapse is high (0.88) because once the theoretical formalization is understood as the origin, earlier 'proto-digital' arrangements (telegraphic money orders, book-entry systems) become conceptually inaccessible as 'digital money.' Resistance is low (0.12) — the main resistance comes from competing readings, not from agents harmed by the constraint. The measurement series shows modest increases over the interval, reflecting growing academic contestation over the boundary as digital money becomes politically salient.
 *
 * PERSPECTIVAL GAP:
 *   From the academic seat, this is a genuine mountain — the theoretical boundary is where the concept 'digital money' becomes well-defined, and any earlier date would be anachronistic. From the infrastructure practitioner seat, the boundary is arbitrary — electronic funds transfer existed operationally decades before Chaum. From the consumer finance seat, the boundary is irrelevant — what matters is when people could actually hold and spend digital value. The engine computes these divergences from the structural data; the claimed mountain type reflects the academic seat's self-perception.
 *
 * DIRECTIONALITY LOGIC:
 *   The academic_cryptography_community and digital_money_priority_claimants are beneficiaries (d near 0.0) — they collect priority, citation, and field-definition authority from the boundary's acceptance. No agent is a clear victim/payer — the constraint does not extract from infrastructure engineers or consumers; it merely excludes their preferred boundary. The infrastructure_reading and consumer_holdings_reading communities are excluded voices (would object if present) but are not structurally targeted by this constraint. The analytical observer sees the full field of contestation.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (providing a clean origin story for digital money as a field) may have outlived its function. The field now has multiple mature subfields (cryptocurrencies, CBDCs, fintech) that don't depend on a single origin story. The arrangement persists as a priority claim rather than a coordination necessity — classic mandatrophy pattern. However, because the constraint is low-extraction and low-suppression, it doesn't actively harm; it merely occupies conceptual space that could be held by a more pluralistic boundary framework.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_boundary_vs_priority_claim,
    'Is the conceptualization boundary a genuine natural law of monetary history (the moment digital money became thinkable), or a constructed boundary that serves academic priority claims?',
    'Comparative analysis of whether other fields (e.g., digital communication, digital computing) draw their emergence boundaries at theoretical formalization or at infrastructure deployment. If monetary history uniquely privileges theory, the boundary is constructed.',
    'If constructed, the constraint is a false summit mountain — the engine would reclassify via FSM to tangled_rope, revealing the academic community as beneficiary of a priority claim masquerading as natural law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_boundary_vs_priority_claim, conceptual, 'Whether the conceptualization boundary is natural or constructed for priority').

omega_variable(
    kernel_reading_foreclosure_structure,
    'Does the conceptualization reading foreclose the infrastructure reading, or do they coexist as different valid boundaries for different analytical purposes?',
    'Test whether a single analytical framework (e.g., a monetary history textbook, a central bank policy document) can consistently adopt both boundaries simultaneously without contradiction. If frameworks must choose one, foreclosure holds; if frameworks use both for different questions, coexistence holds.',
    'Foreclosure would mean this reading''s core premise (theory-first) logically excludes infrastructure-first within any single commitment framework. Coexistence means both are live positions held by different communities for different purposes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_structure, conceptual, 'Structural relationship between conceptualization and infrastructure readings').

omega_variable(
    potential_money_measurement_implication,
    'If M4/M5 money measures must account for ''potential money'' (theoretical concepts, research prototypes not in circulation), does this create measurable distortion in monetary aggregates or policy transmission?',
    'Empirical check: do any monetary authorities or statistical agencies currently impute value to theoretical prototypes? If none do, the ''potential money'' requirement is a theoretical artifact with no operational consequence.',
    'If potential money has no operational uptake, the conceptualization boundary is analytically inert — a mountain claim with no downstream coordination function. If some authorities do impute, the boundary has live coordination consequences.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(potential_money_measurement_implication, empirical, 'Whether the ''potential money'' implication of this boundary has operational consequences').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_emergence_boundary__conceptualization_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dm_eb_cr_tr_t0, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 0, 0.03).
narrative_ontology:measurement(dm_eb_cr_tr_t10, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 10, 0.04).
narrative_ontology:measurement(dm_eb_cr_tr_t20, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 20, 0.05).
narrative_ontology:measurement(dm_eb_cr_tr_t30, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 30, 0.05).

% Extraction over time
narrative_ontology:measurement(dm_eb_cr_be_t0, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(dm_eb_cr_be_t10, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 10, 0.1).
narrative_ontology:measurement(dm_eb_cr_be_t20, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 20, 0.12).
narrative_ontology:measurement(dm_eb_cr_be_t30, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 30, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(dm_eb_cr_su_t0, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(dm_eb_cr_su_t10, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 10, 0.07).
narrative_ontology:measurement(dm_eb_cr_su_t20, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 20, 0.09).
narrative_ontology:measurement(dm_eb_cr_su_t30, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 30, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_emergence_boundary__conceptualization_reading, information_standard).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__conceptualization_reading, digital_money_emergence_boundary__infrastructure_reading).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__conceptualization_reading, digital_money_emergence_boundary__consumer_holdings_reading).

% DUAL FORMULATION NOTE:
% The digital_money_emergence_boundary kernel decomposes into three constraint stories, each drawing the emergence boundary at a different structural layer: theory (this story), infrastructure, and consumer access. The readings compete for definitional authority in monetary history, central bank taxonomy, and regulatory frameworks. This story (conceptualization) is upstream in the sense that theoretical priority claims are often cited as evidence for the field's origin; the other readings treat infrastructure/consumer deployment as the genuine coordination milestone.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
