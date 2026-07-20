% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_kernel__messianic_suspension_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: sacrifice_obligation_kernel__messianic_suspension_reading
 *   human_readable: Messianic Suspension of Sacrifice Obligation with Instrumental Study
 *   domain: religious_law/halakhic_authority
 *
 * SUMMARY:
 *   This constraint instantiates the messianic_suspension_reading of the
 *   sacrifice_obligation_kernel. In this reading, the biblical commandment of
 *   sacrifice remains valid but is divinely suspended until messianic
 *   restoration; the obligation is in abeyance, not nullified, transformed,
 *   or symbolically archived. Study of sacrificial law is instrumentalâit
 *   maintains operational readiness rather than substituting for performance.
 *   The arrangement coordinates the community across a long temporal gap at
 *   low extraction cost, with future generations as the primary beneficiaries
 *   and no victim set during the suspension period. Alternative readings
 *   contest this framing: study_as_exercise_reading treats study as
 *   constitutive fulfillment, performance_only_reading insists on physical
 *   performance, and symbolic_archive_reading denies the normative claim
 *   entirely.
 *
 * KEY AGENTS:
 *   - halakhic_authority: Agenda setter (institutional/generational/constrained) â administers the suspension doctrine and study curriculum
 *   - future_generations: Primary beneficiary (moderate/generational/constrained) â receives preserved sacrificial competence at restoration
 *   - studying_community: Coordinated participant (organized/biographical/constrained) â performs the labor of transmission
 *   - temple_mount_activists: Excluded voice (moderate/mobile) â demands immediate performance and is kept outside normative deliberation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_kernel__messianic_suspension_reading, 0.15).
domain_priors:suppression_score(sacrifice_obligation_kernel__messianic_suspension_reading, 0.22).
domain_priors:theater_ratio(sacrifice_obligation_kernel__messianic_suspension_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, resistance, 0.18).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_kernel__messianic_suspension_reading, rope).
narrative_ontology:human_readable(sacrifice_obligation_kernel__messianic_suspension_reading, "Messianic Suspension of Sacrifice Obligation with Instrumental Study").
narrative_ontology:topic_domain(sacrifice_obligation_kernel__messianic_suspension_reading, "religious_law/halakhic_authority").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_kernel__messianic_suspension_reading, '1f866a9b-dc9c-4226-8498-59ba073b1e9f').
narrative_ontology:cs_kernel_codification('1f866a9b-dc9c-4226-8498-59ba073b1e9f', fixed_text).
narrative_ontology:cs_authority_grounding('1f866a9b-dc9c-4226-8498-59ba073b1e9f', lineage).
narrative_ontology:cs_interpretation_layer_present('1f866a9b-dc9c-4226-8498-59ba073b1e9f').
narrative_ontology:cs_reading_relation('1f866a9b-dc9c-4226-8498-59ba073b1e9f', sacrifice_obligation_kernel__study_as_exercise_reading, forecloses).
narrative_ontology:cs_reading_relation('1f866a9b-dc9c-4226-8498-59ba073b1e9f', sacrifice_obligation_kernel__performance_only_reading, coexists_with).
narrative_ontology:cs_reading_relation('1f866a9b-dc9c-4226-8498-59ba073b1e9f', sacrifice_obligation_kernel__symbolic_archive_reading, forecloses).
narrative_ontology:cs_axiom('1f866a9b-dc9c-4226-8498-59ba073b1e9f', foundational, divine_abeyance_not_extinguishment).
narrative_ontology:cs_axiom_status(divine_abeyance_not_extinguishment, holdable).
narrative_ontology:cs_axiom_grounding('1f866a9b-dc9c-4226-8498-59ba073b1e9f', divine_abeyance_not_extinguishment, theological).
narrative_ontology:cs_axiom('1f866a9b-dc9c-4226-8498-59ba073b1e9f', foundational, instrumental_study_preserves_capacity).
narrative_ontology:cs_axiom_status(instrumental_study_preserves_capacity, holdable).
narrative_ontology:cs_axiom_grounding('1f866a9b-dc9c-4226-8498-59ba073b1e9f', instrumental_study_preserves_capacity, conventional).
narrative_ontology:cs_reference_frame('1f866a9b-dc9c-4226-8498-59ba073b1e9f', abeyant_pending_restoration).
narrative_ontology:cs_drift_state('1f866a9b-dc9c-4226-8498-59ba073b1e9f', contemporary_diaspora, gap(stable, minor, true)).
narrative_ontology:cs_created_at('1f866a9b-dc9c-4226-8498-59ba073b1e9f', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_kernel__messianic_suspension_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__messianic_suspension_reading, future_generations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__messianic_suspension_reading, studying_community).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__messianic_suspension_reading, rabbinic_continuity_doctrine).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__messianic_suspension_reading, messianic_restoration_eschatology).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the normative framework that holds sacrifice in divinely mandated abeyance and directs communal study toward preservation of operational detail. Sets curricula, publishes responsa, and adjudicates the boundaries of permissible discussion regarding Temple practice, ensuring transmission across the diaspora.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, halakhic_authority, agenda_setter,
    institutional, generational, constrained, global).

% The future Jewish community at the time of messianic restoration, who will inherit the preserved operational knowledge of sacrificial law and regain the capacity to perform Temple sacrifices without halakhic discontinuity as a result of current transmission practices.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, future_generations, beneficiary,
    moderate, generational, constrained, global).

% Engages in the study of sacrificial law as a halakhic obligation, investing time and cognitive labor to preserve detailed operational knowledge across the generations of dispersion, expecting no present fulfillment but anticipating future restoration.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, studying_community, beneficiary,
    organized, biographical, constrained, global).

% Advocate for immediate physical performance of sacrifices on the Temple Mount and reject the rabbinic suspension doctrine. They are treated as outside normative halakhic discourse and are excluded from legitimate deliberation about the obligation's current status.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, temple_mount_activists, excluded,
    moderate, biographical, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserve detailed operational knowledge of sacrificial law across generations during the period of Temple absence, so that the community can reactivate correct practice upon messianic restoration without loss of halakhic continuity or procedural competence.
% TRANSFER_FUNCTION: Moves scholarly attention, interpretive labor, and pedagogical resources from the current generation into a transmissible archive of sacrificial competence, to be inherited and activated by a future restoration community.
% ABSENT_VOICES: Temple Mount activists who demand immediate physical performance are marginalized in rabbinic discourse; secular and academic scholars who treat the sacrificial code as purely cultural history rather than normative command are also excluded from halakhic deliberation.
% DISAPPEARANCE_RATIONALE: If the norm of suspension and instrumental study vanished, the community would face an unmediated choice between abandoning the sacrificial commandments entirely, risking schismatic performance attempts, or generating an alternative continuity mechanism; the current arrangement of rabbinic study and deferred expectation would collapse.
% FOUNDING_PROBLEM: The destruction of the Second Temple eliminated the legitimate physical and cultic framework for biblical sacrifices, creating a crisis of halakhic continuity: how to remain obedient to an unfulfillable commandment without nullifying it or violating its procedural boundaries.
% FOUNDING_PROBLEM_CORROBORATION: Extra-halakhic historical sources (Josephus, Roman administrative records) corroborate the Temple destruction. The continuity crisis is attested by Second Temple-era literature and modern historiography of religion. The specific rabbinic resolution is primarily self-attested; rival resolutions (Christian supersession, Samaritan temple persistence) are advanced by outside parties.
narrative_ontology:disappearance_verdict(sacrifice_obligation_kernel__messianic_suspension_reading, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_kernel__messianic_suspension_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_kernel__messianic_suspension_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sacrifice_obligation_kernel__messianic_suspension_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_kernel__messianic_suspension_reading, 0.15, 'kimi-k2.6', 'none', direct).

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
 *   Extraction is low (0.15) because the arrangement is framed as divine suspension rather than human rent-seeking; suppression is mild (0.22) because the constraint operates primarily through normative consensus and curricular inheritance rather than coercion, though activists face discursive exclusion; theater is minimal (0.08) because study has a genuine preservation function and is not performative compliance; accessibility_collapse is moderate (0.35) because alternatives (assimilation, activist performance, secular archival framing) remain conceptually available but are socially marginal within the halakhic community; resistance is low (0.18) because the reading enjoys broad rabbinic consensus, with only fringe activist opposition.
 *
 * PERSPECTIVAL GAP:
 *   The halakhic authority and studying community experience the constraint as continuity-preserving coordination across catastrophe; the future_generations seat receives a preserved capability it did not pay for. The temple_mount_activist seat experiences the same structure as illegitimate suppression of valid performance. The divergence is structural: the activist is excluded from the normative framework and bears the cost of that exclusion, while the coordinated community is net benefited. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Future_generations and studying_community sit at low directionality (beneficiaries of preserved knowledge and continuity). Halakhic_authority sits near the beneficiary end but bears administrative and interpretive labor (d approximately 0.25). Temple_mount_activists sit at high directionality (d approximately 0.85) as excluded targets of the normative boundary; they are structurally pushed outside the arrangement, though no extractive transfer is directed at them.
 *
 * MANDATROPHY ANALYSIS:
 *   The low extraction, absence of victims, and genuine coordination function (preserving knowledge across a known institutional gap) prevent misclassification as a snare or tangled rope. The reading is not a piton because it retains a live coordination function and a clear beneficiary set. It is not a scaffold because its terminus is eschatological rather than a human-legible policy sunset, and it operates as steady-state coordination within the suspension rather than as transitional support.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_reading_location,
    'This constraint is the messianic_suspension_reading of kernel sacrifice_obligation_kernel. Would reclassifying study as constitutive fulfillment (study_as_exercise_reading) or as mere cultural memory (symbolic_archive_reading) alter the beneficiary structure and directionality of the current arrangement?',
    'Comparative analysis of the four sibling readings as separate constraint stories; each reading must be authored with its own epsilon, beneficiary/victim sets, and directionality profile per the epsilon-invariance principle.',
    'A sibling reading could introduce victims (e.g., studying community as extracted-from if study is non-fulfilling busywork) or eliminate beneficiaries (if the law is merely historical), shifting classification from rope toward tangled rope or piton.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_reading_location, conceptual, 'Committer-frame location within the contested kernel and structural deltas across readings').

omega_variable(
    suspension_ontology,
    'Is the divine suspension a metaphysical fact independent of rabbinic discourse, or a juridical construction maintained by the interpretive layer?',
    'Historical-critical tracing of the suspension doctrine through tannaitic, amoraic, and geonic literature to identify whether it is framed as received mesorah or as rabbinic legislative accommodation.',
    'If the suspension is shown to be a rabbinic construction rather than a received divine status, the constraint''s directionality shifts toward institutional authority maintenance and the effective extraction for the agenda_setter seat rises, potentially moving the computed type toward tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suspension_ontology, empirical, 'Ontological status of the suspension mechanism and its implications for extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_kernel__messianic_suspension_reading, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(sacrifice_obligation_kernel__messianic_suspension_reading, study_as_exercise_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__messianic_suspension_reading, performance_only_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__messianic_suspension_reading, symbolic_archive_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the sacrifice_obligation_kernel, decomposed from the colloquial label 'sacrifice obligation' into four structurally distinct claims per the epsilon-invariance principle. Each reading carries a different epsilon, beneficiary/victim structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
