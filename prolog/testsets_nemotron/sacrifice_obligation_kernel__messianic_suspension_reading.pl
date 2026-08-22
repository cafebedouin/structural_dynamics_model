% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_kernel__messianic_suspension_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_kernel__messianic_suspension_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: sacrifice_obligation_kernel__messianic_suspension_reading
 *   human_readable: Sacrifice Obligation — Messianic Suspension Reading
 *   domain: religious_law/halakhic_authority/commitment_system_dynamics
 *
 * SUMMARY:
 *   This constraint story instantiates the 'messianic_suspension_reading' of
 *   the sacrifice_obligation_kernel. The reading holds that the biblical
 *   obligation to offer sacrifices was divinely suspended — not transformed,
 *   not substituted — with the destruction of the Second Temple, and remains
 *   in abeyance until messianic restoration. During the suspension period,
 *   study of sacrifice law (kodashim) serves an instrumental coordination
 *   function: preserving operational knowledge so that the priesthood and
 *   community can immediately resume performance when the Temple is rebuilt.
 *   The constraint is structured as a scaffold: it carries a sunset clause
 *   (messianic restoration), its coordination function is explicitly
 *   transitional (maintaining readiness, not ongoing performance), and it
 *   declares beneficiaries (future generations who inherit intact knowledge;
 *   halakhic authorities who maintain the transmission chain) but no victims
 *   during the suspension period. Extractiveness is low because the
 *   obligation extracts nothing from current practitioners — study is
 *   voluntary, intellectually engaged, and not enforced by penalty.
 *   Suppression is minimal: no one is coerced into studying, and alternative
 *   frameworks (the sibling readings) coexist in the interpretive field.
 *
 * KEY AGENTS:
 *   - future_generations: Primary beneficiary (civilizational/analytical) — inherits preserved operational capacity for restoration
 *   - halakhic_authorities: Agenda setter / secondary beneficiary (institutional/generational) — maintains transmission chain, adjudicates study parameters, derives authority from custodial role
 *   - priestly_lineages: Beneficiary (organized/generational) — preserves genealogical and procedural knowledge for future service
 *   - study_practitioners: Beneficiary / payer (moderate/biographical) — voluntarily engages in study; bears opportunity cost but gains intellectual/religious capital
 *   - rival_reading_adherents: Excluded (various/various) — hold competing readings but are not structurally suppressed by this reading's operation
 *   - analytical_observer: Observer (analytical/civilizational) — sees full kernel structure and reading relations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_kernel__messianic_suspension_reading, 0.12).
domain_priors:suppression_score(sacrifice_obligation_kernel__messianic_suspension_reading, 0.08).
domain_priors:theater_ratio(sacrifice_obligation_kernel__messianic_suspension_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, resistance, 0.18).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_kernel__messianic_suspension_reading, scaffold).
narrative_ontology:human_readable(sacrifice_obligation_kernel__messianic_suspension_reading, "Sacrifice Obligation — Messianic Suspension Reading").
narrative_ontology:topic_domain(sacrifice_obligation_kernel__messianic_suspension_reading, "religious_law/halakhic_authority/commitment_system_dynamics").

domain_priors:requires_active_enforcement(sacrifice_obligation_kernel__messianic_suspension_reading).
narrative_ontology:has_sunset_clause(sacrifice_obligation_kernel__messianic_suspension_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_kernel__messianic_suspension_reading, '2d06f29e-97a4-4ca1-a5a2-5688c3d5b33b').
narrative_ontology:cs_kernel_codification('2d06f29e-97a4-4ca1-a5a2-5688c3d5b33b', formalized).
narrative_ontology:cs_authority_grounding('2d06f29e-97a4-4ca1-a5a2-5688c3d5b33b', lineage).
narrative_ontology:cs_interpretation_layer_present('2d06f29e-97a4-4ca1-a5a2-5688c3d5b33b').
narrative_ontology:cs_reading_relation('2d06f29e-97a4-4ca1-a5a2-5688c3d5b33b', sacrifice_obligation_kernel__study_as_exercise_reading, coexists_with).
narrative_ontology:cs_reading_relation('2d06f29e-97a4-4ca1-a5a2-5688c3d5b33b', sacrifice_obligation_kernel__performance_only_reading, coexists_with).
narrative_ontology:cs_reading_relation('2d06f29e-97a4-4ca1-a5a2-5688c3d5b33b', sacrifice_obligation_kernel__symbolic_archive_reading, coexists_with).
narrative_ontology:cs_axiom('2d06f29e-97a4-4ca1-a5a2-5688c3d5b33b', foundational, obligation_suspended_not_transformed).
narrative_ontology:cs_axiom_status(obligation_suspended_not_transformed, holdable).
narrative_ontology:cs_axiom_grounding('2d06f29e-97a4-4ca1-a5a2-5688c3d5b33b', obligation_suspended_not_transformed, deontological).
narrative_ontology:cs_axiom('2d06f29e-97a4-4ca1-a5a2-5688c3d5b33b', foundational, study_preserves_operational_readiness).
narrative_ontology:cs_axiom_status(study_preserves_operational_readiness, holdable).
narrative_ontology:cs_axiom_grounding('2d06f29e-97a4-4ca1-a5a2-5688c3d5b33b', study_preserves_operational_readiness, instrumental).
narrative_ontology:cs_reference_frame('2d06f29e-97a4-4ca1-a5a2-5688c3d5b33b', intact_temple_sacrificial_system).
narrative_ontology:cs_drift_state('2d06f29e-97a4-4ca1-a5a2-5688c3d5b33b', post_second_temple_destruction, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2d06f29e-97a4-4ca1-a5a2-5688c3d5b33b', '2026-06-11T00:00:00Z').
narrative_ontology:cs_kernel_id(sacrifice_obligation_kernel__messianic_suspension_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__messianic_suspension_reading, future_generations).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__messianic_suspension_reading, halakhic_authorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__messianic_suspension_reading, priestly_lineages).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__messianic_suspension_reading, study_practitioners).
narrative_ontology:constraint_victim(sacrifice_obligation_kernel__messianic_suspension_reading, study_practitioners).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__messianic_suspension_reading, divine_suspension_doctrine).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__messianic_suspension_reading, operational_readiness_principle).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__messianic_suspension_reading, messianic_restoration_teleology).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Inherit the preserved operational knowledge of sacrifice law at the moment of messianic restoration. They bear no cost during the suspension period and receive the full benefit of an unbroken transmission chain. Their 'exit' is not applicable — they are the terminus of the scaffold.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, future_generations, beneficiary,
    analytical, civilizational, analytical, universal).

% Maintain the transmission chain of sacrifice law (kodashim), adjudicate parameters of study, authorize curricula, and derive custodial authority from the role. They bear the burden of accurate transmission but gain institutional legitimacy and authority from the custodial function. Exit is constrained — abandoning the transmission chain would undermine their authority structure, but they could theoretically shift to other halakhic domains.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, halakhic_authorities, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_kernel__messianic_suspension_reading, halakhic_authorities, beneficiary).

% Preserve genealogical records and procedural knowledge for future Temple service. Their identity is fused with the restoration narrative — kohanim status, duchaning, and other present practices depend on the continuity claim. Exit would mean relinquishing a core identity marker; they are identity_locked to the suspension/restoration frame.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, priestly_lineages, beneficiary,
    organized, generational, identity_locked, global).

% Voluntarily engage in study of sacrifice law (kodashim tractates, commentaries, practical guides). They bear opportunity cost (time, cognitive effort) but gain intellectual mastery, religious merit, communal standing, and professional credentials (rabbinic ordination often requires kodashim mastery). Exit is mobile — they can reduce or cease study without structural penalty, though communal expectation creates mild pressure.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, study_practitioners, beneficiary,
    moderate, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_kernel__messianic_suspension_reading, study_practitioners, payer).

% Hold competing readings of the sacrifice obligation (study_as_exercise, performance_only, symbolic_archive). They are not structurally suppressed by this reading — the readings coexist in the interpretive field, debated in yeshivot, journals, and responsa literature. They would object to this reading's claim that the obligation is merely suspended, but their exclusion is from this reading's framework, not from the discourse.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, rival_reading_adherents, excluded,
    moderate, biographical, mobile, global).

% Sees the full kernel structure and all four reading-instantiations. Bears no cost, collects no benefit from the constraint's operation. Provides the classification frame.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_obligation_kernel__messianic_suspension_reading, diffuse).
narrative_ontology:fixing_cost_class(sacrifice_obligation_kernel__messianic_suspension_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves the operational knowledge and procedural capacity for Temple sacrifice across the exile period so that the system can be immediately reactivated at messianic restoration. Solves the coordination problem of intergenerational knowledge transmission for a practice that cannot be performed but must not be lost.
% TRANSFER_FUNCTION: Moves cognitive effort and study-time from current practitioners into the preservation of a knowledge corpus that future generations will inherit. No material transfer (money, goods) occurs during suspension; the transfer is epistemic — effort now for capacity later.
% ABSENT_VOICES: Those who would argue that the suspension narrative itself is a historical construction masking the simple fact that the Temple is gone and sacrifices cannot be performed — a voice largely absent from the internal halakhic discourse but present in historical-critical scholarship. Also absent: potential victims of a future restoration that reinstates animal sacrifice (animal welfare advocates, universalist ethical frameworks) — their objection is projected onto a future that this reading treats as certain but they may reject.
% DISAPPEARANCE_RATIONALE: If the messianic_suspension_reading vanished overnight, the halakhic framework would lose its account of why sacrifice study continues without performance — the 'why' of kodashim study would collapse. Halakhic authorities would need a new justification for maintaining the corpus; priestly lineages would lose the restoration narrative that anchors their identity; study practitioners would lose the teleological frame that gives their effort meaning. The world of halakhic practice would rearrange around a different account of the obligation's status.
% FOUNDING_PROBLEM: How to preserve the divine command to offer sacrifices through the historical catastrophe of Temple destruction and exile, without either violating the command (by inventing substitutes) or abandoning it (by forgetting how).
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested as live by the halakhic authorities who maintain the transmission chain (internal corroboration). External corroboration: historians of halakha note that the kodashim corpus was preserved with unique intensity compared to other impractical areas of law (e.g., agricultural laws in exile), suggesting a genuine coordination effort toward future restoration rather than mere archival instinct. No corroboration from outside the tradition exists for the restoration teleology itself — that remains a faith commitment.
narrative_ontology:disappearance_verdict(sacrifice_obligation_kernel__messianic_suspension_reading, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_kernel__messianic_suspension_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_kernel__messianic_suspension_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(sacrifice_obligation_kernel__messianic_suspension_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_kernel__messianic_suspension_reading, 0.12, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_kernel__messianic_suspension_reading_tests).
:- end_tests(sacrifice_obligation_kernel__messianic_suspension_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The claimed_type is scaffold because: (1) the constraint explicitly carries a sunset (messianic restoration ends the suspension); (2) the coordination function is transitional — maintaining readiness for a future state, not coordinating ongoing activity; (3) beneficiaries are declared (future generations, authorities) and no victims exist during suspension. Metrics reflect this: extractiveness 0.12 (voluntary study, no enforced transfer); suppression 0.08 (social expectation to study exists but exit is mobile); theater_ratio 0.15 (some performative study occurs but most is genuine knowledge preservation). The temporal measurements show a slight drift over 2000 years: extractiveness creeps up as study becomes more institutionalized and less directly tied to imminent restoration; theater_ratio rises as the gap between study and performance widens; suppression_requirement increases modestly as communal norms around kodashim study solidify. The drift is slow and shallow — consistent with a genuine scaffold that has not degraded into piton.
 *
 * PERSPECTIVAL GAP:
 *   From the halakhic_authorities seat: the constraint is genuine coordination — they maintain a living transmission chain for a real future need. From the future_generations seat (analytical projection): the constraint is a gift — intact knowledge waiting at restoration. From the study_practitioners seat: symmetric — voluntary engagement with intellectual reward. From rival_reading_adherents seats: the constraint is irrelevant to them (they operate under different readings). The engine will compute per-seat types from these structural positions; the divergence between agenda_setter (scaffold) and beneficiary (rope-like coordination) seats is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries declared: future_generations (d ≈ 0.05 — pure beneficiary, receives preserved knowledge with no cost), halakhic_authorities (d ≈ 0.15 — benefits from custodial authority but bears transmission burden), priestly_lineages (d ≈ 0.1 — preserves status/knowledge for future role), study_practitioners (d ≈ 0.45 — near symmetric: voluntary cost of study ≈ intellectual/spiritual benefit). No victims declared during suspension period — the obligation is in abeyance, not violated. The directionality derivation from beneficiary declarations + mobile exit options for practitioners produces low d values across seats, yielding low effective extraction χ. Rival readings are excluded, not victimized — they operate in parallel interpretive space.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate (divine command to offer sacrifices) has not atrophied — it is explicitly suspended with a defined restoration condition. The arrangement (study-as-readiness) serves the mandate's future fulfillment, not a degraded present function. This is not mandatrophy: the founding problem (how to preserve sacrificial capacity through exile) remains live in this reading's framework, and the sunset clause (restoration) is the mandated resolution. The scaffold classification correctly captures this: coordination meant to be transitional, justified by the transition, not the steady state.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suspension_mechanism_ambiguity,
    'Is the suspension of the sacrifice obligation a genuine divine decree (temporal abeyance) or a post-hoc interpretive construction that masks the loss of practical capacity after the Temple''s destruction?',
    'Comparative analysis of early tannaitic sources vs. later amoraic/codification layers tracking the shift from ''cannot perform'' to ''divinely suspended''; historical reconstruction of when the suspension narrative crystallized relative to the loss of Temple infrastructure.',
    'If the suspension narrative is a later construction, the reading''s claimed low extractiveness during the suspension period masks a historical transition from enforced performance to interpretive maintenance — the constraint would show extraction accumulation over centuries as study replaced performance while claiming continuity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suspension_mechanism_ambiguity, conceptual, 'Whether the suspension mechanism is intrinsic divine decree or historical interpretive construction').

omega_variable(
    kernel_reading_framing,
    'Does the ''messianic_suspension_reading'' frame the same kernel as the sibling readings, or does it constitute a different kernel by redefining the obligation''s referent from performance to readiness?',
    'Structural comparison of beneficiary/victim sets, extractiveness profiles, and sunset conditions across all four declared readings. If this reading''s referent (operational readiness for future restoration) produces a fundamentally different ε trajectory than the siblings'' referents, it is a distinct constraint under the ε-invariance principle.',
    'If distinct kernel, this reading should be authored as a standalone constraint without reading_relations to the declared siblings; if same kernel, the reading_relations and axioms must accurately capture the structural delta.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_framing, conceptual, 'Commitment-system framing: same kernel or distinct constraint?').

omega_variable(
    study_as_coordination_function,
    'Does the study of sacrifice law genuinely coordinate a community around future restoration capacity, or does it function as identity_coordination that maintains group boundaries and scholarly authority in the present?',
    'Empirical analysis of how sacrifice-study communities allocate resources, transmit knowledge, and respond to challenges to the restoration narrative; comparison with other identity_coordination constraints (professional licensing, national myth maintenance).',
    'If primarily identity_coordination, the low extractiveness claim is maintained by reclassifying the constraint''s coordination type — the Boltzmann floor would shift from scaffold-appropriate to identity_coordination (0.08), changing the excess-extraction calculation and potentially revealing hidden extractive overhead.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(study_as_coordination_function, empirical, 'Whether study coordinates future capacity or present identity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_kernel__messianic_suspension_reading, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacrifice_obligation_kernel__messianic_suspension_reading_tr_t0, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(sacrifice_obligation_kernel__messianic_suspension_reading_tr_t500, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 500, 0.08).
narrative_ontology:measurement(sacrifice_obligation_kernel__messianic_suspension_reading_tr_t1000, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 1000, 0.12).
narrative_ontology:measurement(sacrifice_obligation_kernel__messianic_suspension_reading_tr_t1500, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 1500, 0.14).
narrative_ontology:measurement(sacrifice_obligation_kernel__messianic_suspension_reading_tr_t2000, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 2000, 0.15).

% Extraction over time
narrative_ontology:measurement(sacrifice_obligation_kernel__messianic_suspension_reading_be_t0, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(sacrifice_obligation_kernel__messianic_suspension_reading_be_t500, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 500, 0.1).
narrative_ontology:measurement(sacrifice_obligation_kernel__messianic_suspension_reading_be_t1000, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 1000, 0.11).
narrative_ontology:measurement(sacrifice_obligation_kernel__messianic_suspension_reading_be_t1500, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 1500, 0.12).
narrative_ontology:measurement(sacrifice_obligation_kernel__messianic_suspension_reading_be_t2000, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 2000, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(sacrifice_obligation_kernel__messianic_suspension_reading_su_t0, sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 0, 0.03).
narrative_ontology:measurement(sacrifice_obligation_kernel__messianic_suspension_reading_su_t500, sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 500, 0.05).
narrative_ontology:measurement(sacrifice_obligation_kernel__messianic_suspension_reading_su_t1000, sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 1000, 0.07).
narrative_ontology:measurement(sacrifice_obligation_kernel__messianic_suspension_reading_su_t1500, sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 1500, 0.08).
narrative_ontology:measurement(sacrifice_obligation_kernel__messianic_suspension_reading_su_t2000, sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 2000, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_kernel__messianic_suspension_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(sacrifice_obligation_kernel__messianic_suspension_reading, 0.1).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__messianic_suspension_reading, sacrifice_obligation_kernel__study_as_exercise_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__messianic_suspension_reading, sacrifice_obligation_kernel__performance_only_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__messianic_suspension_reading, sacrifice_obligation_kernel__symbolic_archive_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the 'sacrifice obligation' label into four structurally distinct claims with different ε values, beneficiary/victim sets, and sunset conditions. The messianic_suspension_reading has the lowest extractiveness (obligation suspended, study voluntary) and a genuine sunset (restoration). The study_as_exercise_reading has higher extractiveness (study as obligatory substitute creates enforcement pressure). The performance_only_reading has high suppression (physical performance impossible, creating violation-state). The symbolic_archive_reading has near-zero extractiveness but no halakhic coordination function. All four link via affects_constraints to enable contamination analysis across the kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sacrifice_obligation_kernel__messianic_suspension_reading, institutional, 0.15).
constraint_indexing:directionality_override(sacrifice_obligation_kernel__messianic_suspension_reading, moderate, 0.45).
constraint_indexing:directionality_override(sacrifice_obligation_kernel__messianic_suspension_reading, organized, 0.1).
constraint_indexing:directionality_override(sacrifice_obligation_kernel__messianic_suspension_reading, analytical, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
