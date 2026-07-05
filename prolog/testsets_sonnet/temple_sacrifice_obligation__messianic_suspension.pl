% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_obligation__messianic_suspension
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_obligation__messianic_suspension, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: temple_sacrifice_obligation__messianic_suspension
 *   human_readable: Temple Sacrifice Obligation — Suspended Pending Messianic Restoration
 *   domain: religious_studies/halakhic_authority
 *
 * SUMMARY:
 *   Following the destruction of the Second Temple, a legal system whose
 *   central positive commandments (daily offerings, festival sacrifices,
 *   purification rites) required a physical, currently nonexistent
 *   institution faced a structural crisis: how can law that claims perpetual,
 *   unconditional bindingness coexist with the physical impossibility of
 *   compliance? The messianic_suspension reading resolves this by placing the
 *   obligation into a distinct legal category — neither performed nor
 *   transgressed, but held pending an eschatological event (messianic
 *   restoration and Temple rebuilding) that lies outside any present party's
 *   control or timeline. No one currently owes performance; no one is
 *   currently in violation; and no current human action (including study) is
 *   claimed to move the needle on the obligation's status.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_obligation__messianic_suspension, 0.06).
domain_priors:suppression_score(temple_sacrifice_obligation__messianic_suspension, 0.08).
domain_priors:theater_ratio(temple_sacrifice_obligation__messianic_suspension, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, extractiveness, 0.06).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_obligation__messianic_suspension, rope).
narrative_ontology:human_readable(temple_sacrifice_obligation__messianic_suspension, "Temple Sacrifice Obligation — Suspended Pending Messianic Restoration").
narrative_ontology:topic_domain(temple_sacrifice_obligation__messianic_suspension, "religious_studies/halakhic_authority").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_obligation__messianic_suspension, '3b9b37fb-d077-4697-a253-18d220b6a245').
narrative_ontology:cs_kernel_codification('3b9b37fb-d077-4697-a253-18d220b6a245', fixed_text).
narrative_ontology:cs_authority_grounding('3b9b37fb-d077-4697-a253-18d220b6a245', lineage).
narrative_ontology:cs_interpretation_layer_present('3b9b37fb-d077-4697-a253-18d220b6a245').
narrative_ontology:cs_reading_relation('3b9b37fb-d077-4697-a253-18d220b6a245', temple_sacrifice_obligation__study_as_archiving, coexists_with).
narrative_ontology:cs_reading_relation('3b9b37fb-d077-4697-a253-18d220b6a245', temple_sacrifice_obligation__study_as_occupation, influences).
narrative_ontology:cs_axiom('3b9b37fb-d077-4697-a253-18d220b6a245', foundational, obligation_status_inert_pending_external_event).
narrative_ontology:cs_axiom_status(obligation_status_inert_pending_external_event, holdable).
narrative_ontology:cs_axiom_grounding('3b9b37fb-d077-4697-a253-18d220b6a245', obligation_status_inert_pending_external_event, deontological).
narrative_ontology:cs_axiom('3b9b37fb-d077-4697-a253-18d220b6a245', secondary, present_human_action_cannot_discharge_suspended_obligation).
narrative_ontology:cs_axiom_status(present_human_action_cannot_discharge_suspended_obligation, holdable).
narrative_ontology:cs_axiom_grounding('3b9b37fb-d077-4697-a253-18d220b6a245', present_human_action_cannot_discharge_suspended_obligation, deontological).
narrative_ontology:cs_reference_frame('3b9b37fb-d077-4697-a253-18d220b6a245', temple_era_continuous_obligation).
narrative_ontology:cs_drift_state('3b9b37fb-d077-4697-a253-18d220b6a245', post_destruction_rabbinic_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('3b9b37fb-d077-4697-a253-18d220b6a245', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_obligation__messianic_suspension, temple_sacrifice_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__messianic_suspension, religious_community_continuity).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__messianic_suspension, halakhic_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__messianic_suspension, observant_community_members).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__messianic_suspension, obligation_persists_across_historical_rupture).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__messianic_suspension, divine_command_not_extinguished_by_circumstance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and transmit the doctrine that sacrificial obligation is suspended rather than abrogated. They administer the category itself — deciding what counts as suspension versus fulfillment versus violation — and their institutional standing as authoritative interpreters of law depends on the coherence of a legal system that persists even when its central rite cannot be performed. They neither pay a cost nor collect a material extraction; their gain is the continued relevance of their interpretive vocation.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, halakhic_scholars, agenda_setter,
    institutional, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_obligation__messianic_suspension, halakhic_scholars, beneficiary).

% Live under a legal system that would otherwise declare them in permanent violation of a core commandment. The suspension category lets them remain in good standing without performing an impossible act. They experience no compulsion and no penalty; the doctrine resolves what would otherwise be an unbearable status contradiction (obligated yet perpetually unable to comply).
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, observant_community_members, beneficiary,
    moderate, generational, constrained, global).

% A projected future adjudicator to whom the entire question is deferred. Not a present actor; the constraint's design routes all resolution to this non-existent-yet authority, which is why no present party is asked to render final judgment on whether the obligation is being met.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, future_messianic_authority, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(temple_sacrifice_obligation__messianic_suspension, future_messianic_authority).

% Small movements that argue the suspension should end now — that Temple service should be actively prepared for or attempted. They are structurally outside the mainstream conversation; their position would require reopening a question the suspension doctrine treats as closed pending an event outside human control. They have exit (they can form their own communities) but no voice within the dominant reading.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, dissenting_reconstructionist_movements, excluded,
    powerless, biographical, mobile, regional).

% Study the doctrine as a case of legal systems managing the gap between an unchangeable textual command and an impossible present circumstance. They have no stake in the doctrine's truth, only in describing its structural function within Jewish law.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, comparative_religion_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves a genuine legal-status contradiction: a community formally obligated to perform sacrifices it structurally cannot perform (no Temple exists) would otherwise be in permanent, unresolvable violation. The suspension category coordinates continued legal and communal coherence without requiring either fabricated compliance or an admission of abrogation.
% TRANSFER_FUNCTION: Moves almost nothing materially. What it transfers is a status: it converts a would-be permanent violation into a deferred, non-culpable state. The 'cost' it removes is reputational/theological (the stigma or crisis of being a law-breaking community by definition) rather than economic.
% ABSENT_VOICES: Movements advocating present-day Temple reconstruction or preparatory sacrifice are not part of the mainstream halakhic conversation; their objection — that suspension has calcified into permanent deferral — is heard mainly within their own small circles, not by the interpretive authorities who administer the category.
% DISAPPEARANCE_RATIONALE: If the suspension doctrine vanished, mainstream halakhic communities would face a stark binary: either the obligation is being violated continuously (a crisis-generating status) or it was fulfilled/abrogated by some other mechanism (a theologically radical claim). Some scholars argue the community's practice (prayer substituting for sacrifice, established for two millennia) would absorb the shock with little visible change; others argue the entire self-understanding of law-observant Judaism as an unbroken chain of obligation would be destabilized. The parties genuinely disagree on how much rearranges.
% FOUNDING_PROBLEM: After the Temple's destruction (70 CE), a legal system built on the assumption of ongoing sacrificial service needed a way to hold the sacrificial commandments as still binding — because the law itself claims not to expire — without declaring an entire community in continuous violation of divine command.
% FOUNDING_PROBLEM_CORROBORATION: Historians of religion outside the halakhic tradition (working from Josephus, rabbinic-era communal records, and comparative studies of post-destruction Second Temple Judaism) corroborate that the doctrinal move to 'suspension' rather than 'abrogation' tracks the actual historical rupture of 70 CE and served a documented stabilizing function for a community facing institutional collapse — this is not solely a claim made by those who benefit from the doctrine's continuation.
narrative_ontology:disappearance_verdict(temple_sacrifice_obligation__messianic_suspension, contested).
narrative_ontology:founding_problem_status(temple_sacrifice_obligation__messianic_suspension, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_obligation__messianic_suspension, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(temple_sacrifice_obligation__messianic_suspension, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_obligation__messianic_suspension, 0.06, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_obligation__messianic_suspension_tests).
:- end_tests(temple_sacrifice_obligation__messianic_suspension_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored very low (0.06) because the reading structurally removes any live transfer: no one is compelled to act, no one is penalized for non-performance, and no resource flows from a victim class to a beneficiary class through this specific doctrinal mechanism. Suppression is similarly low (0.08) — there is no active coercive apparatus enforcing the suspension category against dissent, though mainstream institutional weight discourages present-day reconstruction attempts. Theater ratio is modest and rises slightly across the interval (0.08 to 0.12) reflecting increasing liturgical and commemorative elaboration (fast days, prayers referencing restoration) around an obligation that performs no material function — but this is closer to genuine devotional practice than to extraction theater. Accessibility collapse is moderate (0.35), not high: the category structurally forecloses the 'obligation is currently violated' framing and the 'obligation was abrogated' framing, but it does NOT foreclose reconstructionist alternatives (they exist, are practiced by small movements) — hence a mid-range rather than mountain-level collapse. Resistance is very low (0.05): almost no one within the tradition contests the suspension framing itself; the marginal dissent comes from reconstructionist minorities who accept the underlying legal logic but dispute its current applicability.
 *
 * DIRECTIONALITY LOGIC:
 *   Halakhic scholars and observant communities sit near the beneficiary end: the doctrine removes a status crisis for both — scholars retain interpretive authority over a coherent, unbroken legal system, and community members retain good standing without impossible performance demands. No victim class exists because the doctrine's entire function is to prevent harm (the harm of unresolved permanent violation) rather than to extract anything from anyone. The engine should find this reading close to the Mountain/Rope boundary from most seats — the interesting classification question is whether it clears the Rope threshold given how thin its coordination-benefit flow actually is, or whether its minimal-extraction, minimal-suppression profile is closer to a mountain-adjacent artifact of deferral itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The doctrine explicitly resists mandatrophy by design: rather than declaring the obligation fulfilled by a substitute (which would risk claiming victory prematurely) or declaring it abrogated (which would risk claiming the underlying command has lapsed), it holds the status open. This is the opposite of the usual failure mode where an institution declares its founding problem solved to justify continued extraction — here the founding problem (Temple destruction) is declared unambiguously NOT solved, and the institutional response is deferral rather than false completion. The founding_problem_status is authored as 'live' rather than 'dead' precisely because the doctrine's own logic requires the problem to remain acknowledged as unsolved; a 'dead' framing would collapse the suspension into either fulfillment or abrogation, which is what the sibling readings partially explore instead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suspension_versus_permanent_deferral,
    'Is ''suspension pending restoration'' a genuinely open legal status, or has nearly two millennia of non-fulfillment functionally converted it into permanent deferral indistinguishable from quiet abrogation?',
    'Track whether authoritative bodies within the tradition treat any concrete preparatory step (e.g., red heifer identification, priestly genealogy verification) as changing the obligation''s practical status, versus treating all such developments as symbolically interesting but legally inert. Absence of any such live preparatory machinery over centuries would support the ''functionally permanent deferral'' reading.',
    'If the suspension is functionally permanent deferral, the classification would shift toward piton (a category maintained by institutional inertia and theatrical commemoration long after its original open-endedness became structurally moot) rather than rope. This is exactly the kind of divergence the claim/metric independence rule is designed to surface.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suspension_versus_permanent_deferral, conceptual, 'Whether suspension has calcified into de facto permanent non-fulfillment.').

omega_variable(
    kernel_reading_disagreement_location,
    'This story is one reading (messianic_suspension) of the temple_sacrifice_obligation kernel; the sibling readings (study_as_archiving, study_as_occupation) disagree specifically about whether present-day Torah study of sacrificial law has any legal bearing on the obligation''s status. Where exactly does the disagreement sit?',
    'The disagreement is located precisely at the question: does an act performed today (study) count as ANY kind of legal engagement with the obligation, or does the obligation remain wholly inert regardless of what anyone does until the external triggering event (restoration) occurs? messianic_suspension answers ''wholly inert''; study_as_archiving answers ''preserves but does not discharge''; study_as_occupation answers ''partially constitutes fulfillment.''',
    'Adopting study_as_occupation instead of this reading would introduce a live beneficiary class (scholars whose study IS claimed to discharge obligation) and could raise extractiveness/accessibility_collapse metrics, since a stronger claim is being made about what current practice accomplishes. Adopting messianic_suspension keeps the profile minimal, as authored here.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Locates the structural disagreement among sibling kernel readings regarding the legal status of present-day study.').

omega_variable(
    natural_versus_constructed_obligation_category,
    'Is the category of ''suspended obligation'' a natural feature of the legal logic (an obligation genuinely cannot be discharged or violated when performance is physically impossible, so suspension is simply what any coherent legal system would do) or a constructed doctrinal innovation that serves the interpretive authority of the scholars who administer it?',
    'Comparative legal history: examine whether other legal systems facing structurally analogous impossibility-of-performance situations independently converge on a suspension category, versus inventing abrogation, fictional compliance, or crisis declarations instead. Convergence would support naturalness; divergence would support the constructed-authority reading.',
    'If constructed, halakhic_scholars'' beneficiary status becomes more significant — the doctrine''s persistence would serve their continued interpretive relevance more than it serves any natural legal necessity. This bears on the FSM-adjacent concern the schema requires an omega for, given beneficiaries are declared on a low-extraction, rope-adjacent claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_versus_constructed_obligation_category, conceptual, 'Whether the suspension category is a natural legal necessity or a constructed doctrine benefiting its administrators.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_obligation__messianic_suspension, 0, 1955).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(temp_tr_t0, observed).
narrative_ontology:measurement(temp_tr_t400, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 400, 0.09).
narrative_ontology:measurement_basis(temp_tr_t400, observed).
narrative_ontology:measurement(temp_tr_t800, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 800, 0.1).
narrative_ontology:measurement_basis(temp_tr_t800, observed).
narrative_ontology:measurement(temp_tr_t1200, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 1200, 0.11).
narrative_ontology:measurement_basis(temp_tr_t1200, observed).
narrative_ontology:measurement(temp_tr_t1600, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 1600, 0.12).
narrative_ontology:measurement_basis(temp_tr_t1600, observed).
narrative_ontology:measurement(temp_tr_t1955, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 1955, 0.12).
narrative_ontology:measurement_basis(temp_tr_t1955, observed).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 0, 0.04).
narrative_ontology:measurement_basis(temp_be_t0, observed).
narrative_ontology:measurement(temp_be_t400, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 400, 0.05).
narrative_ontology:measurement_basis(temp_be_t400, observed).
narrative_ontology:measurement(temp_be_t800, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 800, 0.05).
narrative_ontology:measurement_basis(temp_be_t800, observed).
narrative_ontology:measurement(temp_be_t1200, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 1200, 0.06).
narrative_ontology:measurement_basis(temp_be_t1200, observed).
narrative_ontology:measurement(temp_be_t1600, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 1600, 0.06).
narrative_ontology:measurement_basis(temp_be_t1600, observed).
narrative_ontology:measurement(temp_be_t1955, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 1955, 0.06).
narrative_ontology:measurement_basis(temp_be_t1955, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(temple_sacrifice_obligation__messianic_suspension, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_obligation__messianic_suspension, identity_coordination).
narrative_ontology:boltzmann_floor_override(temple_sacrifice_obligation__messianic_suspension, 0.05).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__messianic_suspension, study_as_archiving).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__messianic_suspension, study_as_occupation).

% DUAL FORMULATION NOTE:
% This constraint (messianic_suspension) is the deferential-minimalist member of a three-reading family sharing the temple_sacrifice_obligation kernel. study_as_archiving claims present study preserves but does not discharge the obligation (slightly higher engagement claim, still low extraction). study_as_occupation claims present study actively constitutes partial fulfillment (the strongest claim, likely to show higher accessibility_collapse and a more defined beneficiary class in scholarly institutions whose study IS the claimed discharge mechanism). All three readings share the same underlying kernel text and historical rupture event but diverge on whether present human action (study) has any legal bearing on the suspended obligation's status — the exact locus of disagreement is documented in the kernel_reading_disagreement_location omega above.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
