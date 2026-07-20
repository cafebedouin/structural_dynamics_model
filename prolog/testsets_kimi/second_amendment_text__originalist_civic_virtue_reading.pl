% ============================================================================
% CONSTRAINT STORY: second_amendment_text__originalist_civic_virtue_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_text__originalist_civic_virtue_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: second_amendment_text__originalist_civic_virtue_reading
 *   human_readable: Second Amendment Originalist Civic Virtue Reading
 *   domain: constitutional law / political theory
 *
 * SUMMARY:
 *   This constraint story captures the originalist civic-virtue reading of
 *   the Second Amendment: the right to keep and bear arms protects the
 *   universal militia capacity of the citizenry as a political community.
 *   Under this reading, the militia comprises all able-bodied citizens, and
 *   the Amendment guards against both federal overreach and the reduction of
 *   the people to a disarmed subject populace. The kernel is the text of the
 *   Second Amendment; this reading is distinguished from the individual-right
 *   reading (Heller-style self-defense) and the collective-security reading
 *   (state-regulated organized militia) by its insistence that the right is
 *   tied to citizen-soldier capacity and republican virtue rather than
 *   personal self-defense or state regulatory prerogative.
 *
 * KEY AGENTS:
 *   - Citizenry (organized/beneficiary): the universal militia whose armed capacity is protected.
 *   - Originalist jurists (institutional/agenda_setter): enforce the reading through original-public-meaning jurisprudence.
 *   - General government (institutional/payer): bears the cost of foregone regulatory authority over arms.
 *   - Disarmament advocates (organized/excluded): structurally outside the civic-virtue interpretive framework.
 *   - Legal historians (analytical/observer): provide external historical corroboration and challenge.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_text__originalist_civic_virtue_reading, 0.15).
domain_priors:suppression_score(second_amendment_text__originalist_civic_virtue_reading, 0.18).
domain_priors:theater_ratio(second_amendment_text__originalist_civic_virtue_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_text__originalist_civic_virtue_reading, rope).
narrative_ontology:human_readable(second_amendment_text__originalist_civic_virtue_reading, "Second Amendment Originalist Civic Virtue Reading").
narrative_ontology:topic_domain(second_amendment_text__originalist_civic_virtue_reading, "constitutional law / political theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_text__originalist_civic_virtue_reading, 'af732e50-3897-42ac-b054-1f782bf16cf3').
narrative_ontology:cs_kernel_codification('af732e50-3897-42ac-b054-1f782bf16cf3', fixed_text).
narrative_ontology:cs_authority_grounding('af732e50-3897-42ac-b054-1f782bf16cf3', lineage).
narrative_ontology:cs_interpretation_layer_present('af732e50-3897-42ac-b054-1f782bf16cf3').
narrative_ontology:cs_reading_relation('af732e50-3897-42ac-b054-1f782bf16cf3', second_amendment_text__collective_security_reading, coexists_with).
narrative_ontology:cs_reading_relation('af732e50-3897-42ac-b054-1f782bf16cf3', second_amendment_text__individual_right_reading, coexists_with).
narrative_ontology:cs_axiom('af732e50-3897-42ac-b054-1f782bf16cf3', foundational, citizen_soldier_capacity_protected_purpose).
narrative_ontology:cs_axiom_status(citizen_soldier_capacity_protected_purpose, holdable).
narrative_ontology:cs_axiom_grounding('af732e50-3897-42ac-b054-1f782bf16cf3', citizen_soldier_capacity_protected_purpose, conventional).
narrative_ontology:cs_axiom('af732e50-3897-42ac-b054-1f782bf16cf3', foundational, militia_comprehends_body_of_people).
narrative_ontology:cs_axiom_status(militia_comprehends_body_of_people, holdable).
narrative_ontology:cs_axiom_grounding('af732e50-3897-42ac-b054-1f782bf16cf3', militia_comprehends_body_of_people, empirically_contingent).
narrative_ontology:cs_reference_frame('af732e50-3897-42ac-b054-1f782bf16cf3', founding_era_universal_militia).
narrative_ontology:cs_drift_state('af732e50-3897-42ac-b054-1f782bf16cf3', contemporary_professional_military_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('af732e50-3897-42ac-b054-1f782bf16cf3', '').
narrative_ontology:cs_kernel_id(second_amendment_text__originalist_civic_virtue_reading, second_amendment_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_text__originalist_civic_virtue_reading, citizenry).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(second_amendment_text__originalist_civic_virtue_reading, general_government).
narrative_ontology:constraint_vindicates(second_amendment_text__originalist_civic_virtue_reading, civic_republicanism).
narrative_ontology:constraint_vindicates(second_amendment_text__originalist_civic_virtue_reading, original_public_meaning_doctrine).
narrative_ontology:constraint_vindicates(second_amendment_text__originalist_civic_virtue_reading, universal_militia_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Comprises the universal militiaâable-bodied citizens holding arms as a matter of civic duty and republican citizenship. The right protects their capacity to muster for community defense and against tyranny. Exit would mean accepting a state monopoly on armed force.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, citizenry, beneficiary,
    organized, generational, constrained, national).

% Interpret and enforce the Second Amendment according to its original public meaning, framing the right around founding-era militia practice and civic republican duty. Their rulings bind lower courts and legislatures. Exit would require abandoning originalism as an interpretive method.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, originalist_jurists, agenda_setter,
    institutional, civilizational, analytical, national).

% Federal and state legislatures possess broad police powers but are constitutionally barred from disarming the general populace or reducing the militia to a select corps under this reading. They bear the cost of foregone regulatory authority over arms.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, general_government, payer,
    institutional, biographical, constrained, national).

% Argue for state monopoly on armed force and view professional standing armies as sufficient for national defense. They are present in broader policy discourse but structurally excluded from the civic-virtue interpretive framework.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, disarmament_advocates, excluded,
    organized, biographical, constrained, national).

% Study founding-era militia practice, republican political thought, and the original public meaning of the Second Amendment. They provide external corroboration or challenge to the historical premises of the civic-virtue reading without being bound by its normative conclusions.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, legal_historians, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves a republican political community in which the whole body of the people possesses arms and can muster for collective defense, preventing both state monopoly on force and reliance on a select military corps detached from the citizenry.
% TRANSFER_FUNCTION: Moves authority over small-arms possession from general government to individual citizens as members of the political community, and concentrates interpretive authority over the text in the originalist juridical tradition.
% ABSENT_VOICES: Advocates of total state monopoly on armed force and proponents of exclusive reliance on professional standing armies are present in broader policy discourse but structurally excluded from the civic-virtue interpretive framework.
% DISAPPEARANCE_RATIONALE: If this reading vanished overnight, legislatures would regain constitutional authority to disarm the general populace and reconstitute the militia as a select corps; the barrier to state monopoly on force would dissolve and the armed citizenry concept would lose constitutional protection.
% FOUNDING_PROBLEM: Founding-era fear of standing armies and mercenary forces; the need for republican defense through a universal militia of citizen-soldiers rather than a select corps or professional army detached from the people.
% FOUNDING_PROBLEM_CORROBORATION: Anti-federalist writings and Founding-era state ratifying convention records corroborate the historical fear of standing armies. Contemporary military historians and defense policy analysts attest the problem is structurally transformed by modern professional warfare; this corroboration comes from outside the beneficiary set.
narrative_ontology:disappearance_verdict(second_amendment_text__originalist_civic_virtue_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_text__originalist_civic_virtue_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_text__originalist_civic_virtue_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(second_amendment_text__originalist_civic_virtue_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_text__originalist_civic_virtue_reading, 0.15, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_text__originalist_civic_virtue_reading_tests).
:- end_tests(second_amendment_text__originalist_civic_virtue_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the constraint transfers authority to the citizenry rather than extracting from them; suppression is low (0.18) because the constraint expands citizen options and limits state alternatives rather than coercing citizens. Theater ratio is low-moderate (0.20) because while the civic virtue function is substantive, the mismatch between founding-era militia practice and modern professional warfare introduces performative elements. Accessibility collapse is moderate (0.35): within the originalist framework alternatives to an armed citizenry collapse, but the framework itself is contested. Resistance is moderate (0.40) because competing readings (individual right, collective security) and gun-control advocates actively contest this framing in courts and scholarship. Measurements track slight drift upward in theater_ratio as the militia concept becomes anachronistic.
 *
 * PERSPECTIVAL GAP:
 *   The citizenry seat experiences the constraint as a protective liberty; the general government seat experiences it as a constitutional limitation on regulatory capacity. Originalist jurists experience it as an enforceable historical mandate. The engine will compute low directionality for the citizenry (declared beneficiary, organized, constrained exit) and moderate directionality for the government (non-beneficiary, institutional, constrained exit), producing seat divergence despite low base extractiveness.
 *
 * DIRECTIONALITY LOGIC:
 *   Citizenry is declared as the sole beneficiary, yielding directionality near the beneficiary end (low d). General government is not a beneficiary and bears the coordination cost of foregone regulation, yielding moderate d near the symmetric-to-target range. Originalist jurists administer the reading but do not personally collect extraction; their d remains low because their role is agenda-setting enforcement of a coordination function. No victims are declared, so no seat sits near full-target.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâfear of standing armies and the need for a universal militiaâis contested as to whether it persists today. However, the constraint is not a piton because the coordination function (preventing state monopoly on force and preserving citizen-soldier capacity) remains structurally meaningful to adherents and is actively enforced by originalist jurists. The theater ratio is not high enough to suggest proxy goals have replaced real function. It is also not a snare because no identifiable victim group bears concentrated costs; the government's regulatory cost is diffuse and symmetrical with the citizenry's benefit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    militia_viability_in_modern_warfare,
    'Is a universal citizen militia operationally viable in an era of professional standing armies and advanced military technology?',
    'Comparative military operational analysis and historical case studies of citizen militia effectiveness in twentieth- and twenty-first-century conflicts.',
    'If non-viable, the coordination function of this reading becomes largely performative, raising theater_ratio and shifting classification toward piton; if still viable, the rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(militia_viability_in_modern_warfare, empirical, 'Operational viability of universal militia in contemporary warfare').

omega_variable(
    kernel_reading_exclusivity,
    'Does the originalist civic-virtue reading foreclose the individual-right reading, or do they coexist as live interpretive options within a single legal framework?',
    'Jurisprudential analysis of whether a single legal framework can simultaneously hold that the right is tied to militia service (civic virtue) and independent of militia service (individual self-defense).',
    'If foreclosed, the kernel is more deeply fractured than a simple contest of readings; if coexisting, the kernel sustains multiple stable constraints without logical contradiction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_exclusivity, conceptual, 'Logical relationship between civic-virtue and individual-right readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_text__originalist_civic_virtue_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sa_ocvr_tr_t0, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(sa_ocvr_tr_t20, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 20, 0.08).
narrative_ontology:measurement(sa_ocvr_tr_t40, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 40, 0.12).
narrative_ontology:measurement(sa_ocvr_tr_t60, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 60, 0.15).
narrative_ontology:measurement(sa_ocvr_tr_t80, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 80, 0.18).
narrative_ontology:measurement(sa_ocvr_tr_t100, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 100, 0.2).

% Extraction over time
narrative_ontology:measurement(sa_ocvr_be_t0, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(sa_ocvr_be_t20, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 20, 0.11).
narrative_ontology:measurement(sa_ocvr_be_t40, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 40, 0.12).
narrative_ontology:measurement(sa_ocvr_be_t60, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 60, 0.13).
narrative_ontology:measurement(sa_ocvr_be_t80, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 80, 0.14).
narrative_ontology:measurement(sa_ocvr_be_t100, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 100, 0.15).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(second_amendment_text__originalist_civic_virtue_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(second_amendment_text__originalist_civic_virtue_reading, collective_security_reading).
narrative_ontology:affects_constraint(second_amendment_text__originalist_civic_virtue_reading, individual_right_reading).

% DUAL FORMULATION NOTE:
% The Second Amendment text is a contested kernel whose label conflates three structurally distinct readings. This story isolates the originalist civic-virtue reading with its own epsilon, beneficiary structure, and classification, linked to sibling readings per the epsilon-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
