% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_continuity__study_as_performance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_continuity__study_as_performance, []).

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
 *   constraint_id: sacrifice_obligation_continuity__study_as_performance
 *   human_readable: Study of Sacrifice Law as Fulfillment of the Commandment
 *   domain: religious_law/ritual_studies/textual_tradition
 *
 * SUMMARY:
 *   In the rabbinic tradition following the destruction of the Second Temple,
 *   the obligation to offer sacrifices (korbanot) was transformed: study of
 *   the sacrificial laws (seder kodashim) was declared equivalent to the
 *   physical performance of the sacrifices themselves. This reading — 'study
 *   as performance' — holds that the commandment persists through textual
 *   engagement; the obligation is fulfilled when the practitioner learns the
 *   laws of the olah, minchat, chatat, and asham offerings. The constraint is
 *   the standing interpretive arrangement: the sacrificial obligation
 *   continues, but its mode of fulfillment has shifted from physical act to
 *   intellectual-ritual engagement. No party is victimized — the obligation
 *   is satisfied through study. Beneficiaries are the communities of
 *   scholars, students, and interpretive traditions that maintain continuity
 *   through this equivalence. Extractiveness is low: texts are accessible,
 *   study is voluntary in form (though communally expected), and no
 *   extraction apparatus enforces participation. The theater ratio captures
 *   the performative dimension: study sessions may emphasize the *enactment*
 *   of learning over comprehension, but the functional core (mastery of the
 *   material) remains dominant.
 *
 * KEY AGENTS:
 *   - textual_scholars: Primary beneficiary (organized/biographical) — maintain interpretive authority through mastery of sacrificial law
 *   - study_practitioners: Primary beneficiary (moderate/biographical) — fulfill obligation through accessible textual engagement
 *   - interpretive_communities: Secondary beneficiary (organized/generational) — preserve continuity of obligation without physical temple
 *   - analytical_observer: Observer (analytical/civilizational/universal) — sees full structural transformation from physical to textual fulfillment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_continuity__study_as_performance, 0.08).
domain_priors:suppression_score(sacrifice_obligation_continuity__study_as_performance, 0.12).
domain_priors:theater_ratio(sacrifice_obligation_continuity__study_as_performance, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, extractiveness, 0.08).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_continuity__study_as_performance, rope).
narrative_ontology:human_readable(sacrifice_obligation_continuity__study_as_performance, "Study of Sacrifice Law as Fulfillment of the Commandment").
narrative_ontology:topic_domain(sacrifice_obligation_continuity__study_as_performance, "religious_law/ritual_studies/textual_tradition").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_continuity__study_as_performance, '9bfc132e-554e-4f2a-8541-9b23bfd077c6').
narrative_ontology:cs_kernel_codification('9bfc132e-554e-4f2a-8541-9b23bfd077c6', fixed_text).
narrative_ontology:cs_authority_grounding('9bfc132e-554e-4f2a-8541-9b23bfd077c6', lineage).
narrative_ontology:cs_interpretation_layer_present('9bfc132e-554e-4f2a-8541-9b23bfd077c6').
narrative_ontology:cs_reading_relation('9bfc132e-554e-4f2a-8541-9b23bfd077c6', sacrifice_obligation_continuity__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('9bfc132e-554e-4f2a-8541-9b23bfd077c6', sacrifice_obligation_continuity__messianic_suspension, coexists_with).
narrative_ontology:cs_reading_relation('9bfc132e-554e-4f2a-8541-9b23bfd077c6', sacrifice_obligation_continuity__archival_preservation, coexists_with).
narrative_ontology:cs_axiom('9bfc132e-554e-4f2a-8541-9b23bfd077c6', foundational, study_equals_sacrifice_fulfillment).
narrative_ontology:cs_axiom_status(study_equals_sacrifice_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('9bfc132e-554e-4f2a-8541-9b23bfd077c6', study_equals_sacrifice_fulfillment, deontological).
narrative_ontology:cs_axiom('9bfc132e-554e-4f2a-8541-9b23bfd077c6', foundational, obligation_persists_through_textual_engagement).
narrative_ontology:cs_axiom_status(obligation_persists_through_textual_engagement, holdable).
narrative_ontology:cs_axiom_grounding('9bfc132e-554e-4f2a-8541-9b23bfd077c6', obligation_persists_through_textual_engagement, deontological).
narrative_ontology:cs_reference_frame('9bfc132e-554e-4f2a-8541-9b23bfd077c6', rabbinic_equivalence_declaration).
narrative_ontology:cs_drift_state('9bfc132e-554e-4f2a-8541-9b23bfd077c6', contemporary_digital_study_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('9bfc132e-554e-4f2a-8541-9b23bfd077c6', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_continuity__study_as_performance, sacrifice_obligation_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__study_as_performance, textual_scholars).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__study_as_performance, study_practitioners).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__study_as_performance, interpretive_communities).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__study_as_performance, torah_study_equivalence_doctrine).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__study_as_performance, commandment_persistence_through_text).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__study_as_performance, interpretive_fulfillment_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Master and teach the sacrificial law corpus (seder kodashim, relevant talmudic tractates, codes). Their authority derives from certified mastery; they define what counts as adequate study. They benefit from the arrangement's persistence — it secures their interpretive role and institutional position. Exit is constrained: leaving the interpretive community means losing the authority that mastery confers, but they could shift to other areas of Torah study.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, textual_scholars, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_continuity__study_as_performance, textual_scholars, agenda_setter).

% Engage in regular study of sacrificial laws — through daily daf yomi, shiurim, chevruta, or digital platforms. They fulfill the obligation through this engagement. The texts are accessible (translated, commented, digitized); no certification gatekeeps participation. Exit is mobile: they can study more or less, switch topics, or stop without institutional penalty — the obligation is communal, not personally enforced.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, study_practitioners, beneficiary,
    moderate, biographical, mobile, global).

% Communities (yeshivas, synagogue study groups, online learning platforms) that organize and transmit the study practice. They benefit from the continuity the arrangement provides — a living link to Temple ritual without the Temple. Their institutional identity is bound to this continuity. Exit is constrained: abandoning the study-as-fulfillment framework would require adopting a different reading of the kernel, which disrupts communal self-understanding.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, interpretive_communities, beneficiary,
    organized, generational, constrained, global).

% Sees the full structural transformation: a high-extraction physical sacrificial system replaced by a low-extraction textual system. No personal stake in the obligation's fulfillment; analyzes the coordination function, the identity maintenance, and the historical contingency of the reading.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_obligation_continuity__study_as_performance, diffuse).
narrative_ontology:fixing_cost_class(sacrifice_obligation_continuity__study_as_performance, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains continuity of the sacrificial obligation across the rupture of Temple destruction by transmuting physical performance into textual engagement — solving the collective-action problem of how a dispersed people sustain a Temple-centered commandment without a Temple.
% TRANSFER_FUNCTION: Moves the fulfillment-locus from physical act (animal, grain, priestly mediation, pilgrimage) to intellectual-ritual act (study, comprehension, recitation). No material transfer; the 'payment' is attention and cognitive effort, and the 'receipt' is obligation-discharge and communal continuity.
% ABSENT_VOICES: Those who hold the performance_only reading (physical sacrifice still required, study is mere preparation) — they would object that study-as-fulfillment empties the commandment of its substance. They are located in factions awaiting literal Temple restoration (some messianic groups, Temple Institute adherents). Also absent: the archival_preservation reading proponents (secular/academic scholars) who would say the obligation is void and study is cultural preservation — they are in university departments and non-observant intellectual circles.
% DISAPPEARANCE_RATIONALE: If the study-as-fulfillment equivalence vanished overnight, the sacrificial obligation would either (a) become unfulfillable (no Temple, no physical performance possible), creating a crisis of unmet obligation for observant communities, or (b) revert to the performance_only reading, requiring physical restoration as the only path — either way, the communal-arrangement landscape would rearrange. The interpretive communities organized around this equivalence would lose their structuring logic.
% FOUNDING_PROBLEM: After the destruction of the Second Temple (70 CE), the sacrificial system — central to Israelite worship, atonement, and communal identity — became physically impossible. The founding problem: how to maintain the binding force of the sacrificial commandments when their physical performance is structurally precluded?
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested as live by the continuing centrality of seder kodashim in yeshiva curricula, the proliferation of new commentaries on sacrificial law (e.g., Steinsaltz, ArtScroll, digital platforms), and the Temple Institute's ongoing preparation of physical vessels — the latter corroborates from OUTSIDE the study-as-fulfillment reading (performance_only proponents agree the problem is live but differ on the solution). No single tradition monopolizes the problem's persistence.
narrative_ontology:disappearance_verdict(sacrifice_obligation_continuity__study_as_performance, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_continuity__study_as_performance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_continuity__study_as_performance, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(sacrifice_obligation_continuity__study_as_performance, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_continuity__study_as_performance, 0.08, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_continuity__study_as_performance_tests).
:- end_tests(sacrifice_obligation_continuity__study_as_performance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08) reflects the low cost of textual engagement relative to the physical sacrifice it replaces — animals, pilgrimage, priestly mediation all eliminated. Suppression (0.12) is minimal: no enforcement mechanism compels study; communal expectation and identity provide soft pressure only. Theater ratio (0.15) acknowledges that some study is performative (recitation without comprehension, status signaling through textual mastery), but the functional core dominates. Accessibility collapse (0.2) is low because alternatives exist: one can study alone, in chevruta, in formal institutions, or through digital media; the constraint does not close exits. Resistance (0.05) is near zero — the reading was adopted voluntarily by communities seeking continuity, not imposed. The claimed type 'rope' reflects genuine coordination: the arrangement solves the collective-action problem of maintaining sacrificial obligation without a Temple, with minimal coercive overhead and net benefit to participants.
 *
 * PERSPECTIVAL GAP:
 *   From the study_practitioner seat, the constraint is pure coordination (rope) — a accessible path to fulfill a binding obligation. From the textual_scholar seat, there is a secondary benefit: interpretive authority and institutional position are maintained through mastery of this specific corpus. The analytical observer sees the full transformation: a high-extraction physical system (animal sacrifice, priestly monopoly, pilgrimage cost) replaced by a low-extraction textual system. The engine will compute per-seat types from these structural positions; the analytical seat should compute mountain-like (near-zero extraction, no suppression), while scholar seats may show slightly higher effective extraction if gatekeeping exists (captured in omega_study_accessibility_vs_mastery_barrier).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries declared: textual_scholars, study_practitioners, interpretive_communities — all collect the primary benefit (obligation fulfillment, continuity, identity) without bearing extraction costs. No victims declared: the obligation is satisfied, not extracted from anyone. The directionality derivation from beneficiary declarations yields d ≈ 0.15 for beneficiaries (subsidy-like), and the analytical observer sits at d = 0.0. The low extractiveness is not scaled up by directionality because no seat is a target.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to maintain sacrificial obligation without a Temple) remains live in the sense that the Temple is not rebuilt and the obligation persists. The arrangement has not atrophied — study practice is vigorous and expanding (digital platforms, new commentaries, renewed interest in kodashim). Mandatrophy is not resolved; the coordination function is active. The constraint would be a piton only if study practice became purely performative with no one actually learning the material — the rising theater_ratio trend (0.10→0.15) warrants monitoring but has not crossed the threshold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_kernel_reading_structure,
    'This constraint instantiates the study_as_performance reading of the sacrifice_obligation_continuity kernel. What structural elements would change under sibling readings, and where is the disagreement located?',
    'Comparative constraint analysis across the kernel family: each sibling reading (performance_only, messianic_suspension, archival_preservation) instantiates a distinct constraint with its own beneficiary/victim structure, extractiveness profile, and CS axioms. The disagreement is located on whether study constitutes fulfillment (this reading), preparation (performance_only), readiness-maintenance (messianic_suspension), or cultural preservation without normative force (archival_preservation).',
    'If study is mere preparation (performance_only), extraction rises (study effort required without satisfaction). If suspended (messianic_suspension), obligation persists but fulfillment is deferred — study bears opportunity cost without discharge. If archival (archival_preservation), obligation is void — study becomes optional cultural practice, extracting only voluntary attention. The reading choice structurally determines whether a victim set exists and what extractiveness the obligation carries.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_reading_structure, conceptual, 'Commitment-system framing: which reading of the sacrifice_obligation_continuity kernel this constraint instantiates, and the structural delta across sibling readings.').

omega_variable(
    study_accessibility_vs_mastery_barrier,
    'Is the study practice genuinely accessible (low extractiveness) or does mastery of sacrificial law require gatekept expertise, creating an implicit beneficiary class of certified interpreters?',
    'Empirical survey of study-practice communities: distribution of textual authority, barriers to recognized fulfillment, whether lay engagement counts as fulfillment or whether institutional certification is required.',
    'If mastery barriers exist, extractiveness rises and a beneficiary class of gatekeeping authorities emerges (certified rabbis, institutional academies). The current low extractiveness assumes open textual engagement; gatekeeping would shift the constraint toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_accessibility_vs_mastery_barrier, empirical, 'Whether the accessibility of sacrificial study is structurally open or mediated by certified interpreters.').

omega_variable(
    cs_framing_under_determination,
    'Does the study_as_performance reading ground its authority in the interpretive tradition (lineage) or in the practice of study itself (practice)? The CS structure claims both: authority_grounding=lineage and interpretation_layer_present=true. But the practice-grounded framing (study IS the authority) would set authority_grounding=practice and interpretation_layer_present=false. Which framing is the actual structural commitments of this reading?',
    'Internal discourse analysis: when challenged on a novel interpretive move, does this reading appeal to chain-of-transmission (lineage) or to the internal coherence of the study practice itself (practice)? The former makes interpretation_layer_present coherent; the latter would make the kernel directly govern practice.',
    'If authority_grounding=practice, the reading has no interpretive buffer — drift in study practice directly alters the kernel''s content. This reading would be structurally brittle but more responsive. The current lineage+practice hybrid may be an unstable equilibrium.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_under_determination, conceptual, 'Whether the study_as_performance reading''s authority derives from transmission lineage or from the study practice itself — a framing ambiguity that changes CS structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_continuity__study_as_performance, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 0, 0.1).
narrative_ontology:measurement(sacr_tr_t25, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 25, 0.12).
narrative_ontology:measurement(sacr_tr_t50, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 50, 0.13).
narrative_ontology:measurement(sacr_tr_t75, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 75, 0.14).
narrative_ontology:measurement(sacr_tr_t100, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 0, 0.06).
narrative_ontology:measurement(sacr_be_t25, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 25, 0.07).
narrative_ontology:measurement(sacr_be_t50, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 50, 0.07).
narrative_ontology:measurement(sacr_be_t75, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 75, 0.075).
narrative_ontology:measurement(sacr_be_t100, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 100, 0.08).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_obligation_continuity__study_as_performance, suppression_requirement, 0, 0.08).
narrative_ontology:measurement(sacr_su_t25, sacrifice_obligation_continuity__study_as_performance, suppression_requirement, 25, 0.1).
narrative_ontology:measurement(sacr_su_t50, sacrifice_obligation_continuity__study_as_performance, suppression_requirement, 50, 0.11).
narrative_ontology:measurement(sacr_su_t75, sacrifice_obligation_continuity__study_as_performance, suppression_requirement, 75, 0.115).
narrative_ontology:measurement(sacr_su_t100, sacrifice_obligation_continuity__study_as_performance, suppression_requirement, 100, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_continuity__study_as_performance, identity_coordination).
narrative_ontology:boltzmann_floor_override(sacrifice_obligation_continuity__study_as_performance, 0.08).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__study_as_performance, sacrifice_obligation_continuity__performance_only).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__study_as_performance, sacrifice_obligation_continuity__messianic_suspension).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__study_as_performance, sacrifice_obligation_continuity__archival_preservation).

% DUAL FORMULATION NOTE:
% This constraint is one member of the sacrifice_obligation_continuity kernel family. The kernel decomposes into four structurally distinct constraints with different ε values and beneficiary/victim structures: study_as_performance (ε=0.08, rope, no victims), performance_only (ε≈0.35, tangled_rope — study prepares but physical performance still required, creating opportunity cost), messianic_suspension (ε≈0.15, scaffold — obligation deferred, study maintains readiness with sunset at restoration), archival_preservation (ε≈0.02, mountain — obligation void, study is optional cultural preservation). All four linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
