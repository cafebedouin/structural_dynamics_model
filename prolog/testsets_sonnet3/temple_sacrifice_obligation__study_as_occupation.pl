% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_obligation__study_as_occupation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_obligation__study_as_occupation, []).

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
 *   constraint_id: temple_sacrifice_obligation__study_as_occupation
 *   human_readable: Study of Sacrifice Law as Fulfillment-Equivalent Occupation of the Korbanot Obligation
 *   domain: religious/halakhic_authority
 *
 * SUMMARY:
 *   This story instantiates the 'study_as_occupation' reading of the
 *   temple_sacrifice_obligation kernel: the position, rooted in Talmudic
 *   statements and codified across later halakhic tradition, that dedicated
 *   study of the laws of sacrifice (Seder Kodashim) constitutes legitimate
 *   present-tense fulfillment — not mere preservation or suspension — of the
 *   underlying biblical obligation while the Temple does not exist. Under
 *   this reading, the obligation is not void, not merely archived, and not
 *   suspended in limbo; it has migrated its performative locus from altar to
 *   text, and that migration is itself treated as adequate. The theater_ratio
 *   trajectory declines over the interval, reflecting the doctrine's
 *   transition from an emergency accommodation (immediately post-70 CE, when
 *   the practice functioned partly as psychological/communal theater covering
 *   a raw institutional wound) toward a fully naturalized, non-theatrical
 *   default within rabbinic Judaism by the modern era.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_obligation__study_as_occupation, 0.12).
domain_priors:suppression_score(temple_sacrifice_obligation__study_as_occupation, 0.2).
domain_priors:theater_ratio(temple_sacrifice_obligation__study_as_occupation, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, extractiveness, 0.12).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_obligation__study_as_occupation, rope).
narrative_ontology:human_readable(temple_sacrifice_obligation__study_as_occupation, "Study of Sacrifice Law as Fulfillment-Equivalent Occupation of the Korbanot Obligation").
narrative_ontology:topic_domain(temple_sacrifice_obligation__study_as_occupation, "religious/halakhic_authority").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_obligation__study_as_occupation, 'f0d79997-887c-4f1c-b65f-66ec4f36e8a8').
narrative_ontology:cs_kernel_codification('f0d79997-887c-4f1c-b65f-66ec4f36e8a8', fixed_text).
narrative_ontology:cs_authority_grounding('f0d79997-887c-4f1c-b65f-66ec4f36e8a8', lineage).
narrative_ontology:cs_interpretation_layer_present('f0d79997-887c-4f1c-b65f-66ec4f36e8a8').
narrative_ontology:cs_reading_relation('f0d79997-887c-4f1c-b65f-66ec4f36e8a8', temple_sacrifice_obligation__study_as_archiving, coexists_with).
narrative_ontology:cs_reading_relation('f0d79997-887c-4f1c-b65f-66ec4f36e8a8', temple_sacrifice_obligation__messianic_suspension, coexists_with).
narrative_ontology:cs_axiom('f0d79997-887c-4f1c-b65f-66ec4f36e8a8', foundational, study_constitutes_legal_equivalence_to_performance).
narrative_ontology:cs_axiom_status(study_constitutes_legal_equivalence_to_performance, holdable).
narrative_ontology:cs_axiom_grounding('f0d79997-887c-4f1c-b65f-66ec4f36e8a8', study_constitutes_legal_equivalence_to_performance, conventional).
narrative_ontology:cs_axiom('f0d79997-887c-4f1c-b65f-66ec4f36e8a8', foundational, obligation_remains_actively_dischargeable_absent_temple).
narrative_ontology:cs_axiom_status(obligation_remains_actively_dischargeable_absent_temple, holdable).
narrative_ontology:cs_axiom_grounding('f0d79997-887c-4f1c-b65f-66ec4f36e8a8', obligation_remains_actively_dischargeable_absent_temple, conventional).
narrative_ontology:cs_reference_frame('f0d79997-887c-4f1c-b65f-66ec4f36e8a8', temple_era_direct_performance).
narrative_ontology:cs_drift_state('f0d79997-887c-4f1c-b65f-66ec4f36e8a8', post_destruction_rabbinic_consolidation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('f0d79997-887c-4f1c-b65f-66ec4f36e8a8', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_obligation__study_as_occupation, temple_sacrifice_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_occupation, rabbinic_academies).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_occupation, observant_laity).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_occupation, halakhic_authorities).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__study_as_occupation, torah_study_equivalence_doctrine).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__study_as_occupation, continuity_of_divine_service_without_temple).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Rule on whether and how study of the sacrificial order discharges the underlying obligation. They administer the interpretive tradition (via Talmudic sugyot, later codes, and yeshiva curricula) that treats sustained textual engagement with korbanot law as the obligation's live form. Their authority is not threatened by Temple absence; if anything, the reading gives them an ongoing adjudicative role they would otherwise lose.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, halakhic_authorities, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Institutions (yeshivot, kollelim) whose curricula center on Seder Kodashim, the sacrificial order, as core study material. The occupation reading gives their study activity independent religious weight rather than merely preparatory or archival status, sustaining enrollment, funding, and institutional prestige tied to mastery of this material.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, rabbinic_academies, beneficiary,
    organized, civilizational, arbitrage, global).

% Individuals under the obligation who cannot physically perform sacrifice (no Temple exists). Under this reading, dedicating time to studying the relevant laws satisfies the commandment's demand on them, resolving what would otherwise be a standing, unfulfillable duty. This removes a source of religious anxiety and gives them a concrete, achievable form of compliance.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, observant_laity, beneficiary,
    moderate, biographical, constrained, global).

% Groups holding that the obligation is suspended, not fulfilled, pending Temple restoration; treating study as full occupation is seen by them as prematurely closing a gap that should remain visibly open to maintain restorationist urgency. Their view is present in the tradition but is not the operative halakhic default in most communities today, so their objection rarely surfaces in mainstream practice.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, messianic_restorationist_communities, excluded,
    moderate, civilizational, constrained, global).

% Academic and traditional scholars who trace how the occupation doctrine developed (e.g., from statements attributed to the sages that study of the sacrificial laws is 'as if' one offered the sacrifice) and assess whether it functions as genuine legal equivalence or as a coping mechanism for institutional discontinuity.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, textual_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(temple_sacrifice_obligation__study_as_occupation, diffuse).
narrative_ontology:fixing_cost_class(temple_sacrifice_obligation__study_as_occupation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a way for an entire legal-religious system to continue functioning coherently after its central performative institution (the Temple) was destroyed, without declaring the underlying law void, suspended, or irretrievably broken.
% TRANSFER_FUNCTION: Moves the locus of religious value from physical performance (animal sacrifice at a specific site) to intellectual/textual engagement (study of the laws governing that performance), redistributing prestige and religious capital toward scholarly institutions and away from a priestly/Temple-based class that no longer exists.
% ABSENT_VOICES: Restorationist and messianic-suspension communities who hold the obligation should remain visibly unfulfilled to preserve eschatological urgency are present in classical sources but structurally marginal in the operative consensus that treats study as sufficient; their objection is rarely adjudicated because the study reading is dominant practice, not merely one option among equals.
% DISAPPEARANCE_RATIONALE: If the study-as-occupation doctrine disappeared, the immediate practical world would not visibly rearrange (no one is currently offering sacrifices either way), but the felt religious status of millions of hours of Seder Kodashim study would change from 'fulfillment of a live commandment' to 'preparatory or archival study only' — a significant shift in institutional self-understanding and possibly enrollment incentive, even without an external behavioral change. Whether that counts as 'the world rearranges' or 'unchanged' is itself disputed between the sibling readings.
% FOUNDING_PROBLEM: After the Temple's destruction in 70 CE, an entire category of biblically mandated obligations (animal sacrifice) became physically impossible to perform, threatening either to render large sections of Torah law permanently void or to leave the community in permanent, anxiety-inducing non-compliance.
% FOUNDING_PROBLEM_CORROBORATION: Talmudic sources themselves (attributed to sages including Rav) assert the study-equivalence principle, which is an internal, tradition-authoritative source rather than an outside check. Independent historians of religion note the doctrine's functional role in institutional continuity, but this is a structural-functionalist reading, not the tradition's own self-understanding; within the restorationist minority, the founding problem is explicitly held to still be live and NOT solved by study, which is the clearest outside-the-consensus corroboration that the 'resolved' status is itself a majority reading, not a settled fact.
narrative_ontology:disappearance_verdict(temple_sacrifice_obligation__study_as_occupation, contested).
narrative_ontology:founding_problem_status(temple_sacrifice_obligation__study_as_occupation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_obligation__study_as_occupation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(temple_sacrifice_obligation__study_as_occupation, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_obligation__study_as_occupation, 0.12, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_obligation__study_as_occupation_tests).
:- end_tests(temple_sacrifice_obligation__study_as_occupation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.12) because no identifiable party is made worse off by this doctrine's operation — no rents are extracted, no coercive apparatus enforces it, and its 'beneficiaries' (academies, laity, authorities) gain religious/institutional continuity rather than extracting from a victim class. Suppression is modest (0.2): the doctrine does not forcibly suppress alternative readings so much as become the operative default that other readings must argue against. Accessibility_collapse is moderate (0.4) rather than mountain-high, because the sibling readings (archiving, suspension) remain live, articulable positions within the tradition — the alternatives have not collapsed, they have been out-competed institutionally. Resistance is low (0.15): minority restorationist objections exist but do not mount organized challenge to mainstream practice.
 *
 * DIRECTIONALITY LOGIC:
 *   Halakhic authorities and rabbinic academies sit near the beneficiary end: they administer and are institutionally strengthened by treating study as fulfillment. Observant laity are near-symmetric-to-beneficiary: they receive genuine relief from an otherwise permanently unfulfillable obligation, which is a real coordination benefit rather than an imposed cost. No stakeholder is authored as a payer/victim because the reading's structural claim (per the expected delta) is precisely that no one bears an extraction cost — the obligation is discharged, not deferred or defaulted upon. This is why no victims array is authored; declaring one would misrepresent the reading's own structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification risk here is treating a genuine, functioning coordination doctrine as either a dead-letter fiction (which would wrongly read it as pure theater/piton) or as extraction dressed as piety (which would wrongly read it as tangled_rope or snare). The rope classification with low theater_ratio and low extractiveness reflects that the doctrine solves a real problem (systemic obligation-continuity after institutional collapse) with genuine, voluntary uptake rather than coercive maintenance. The founding_problem_status is authored as 'contested' specifically to prevent the corpus from silently endorsing the majority reading as uncontested fact — the restorationist minority's live disagreement is real data, not noise.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    occupation_vs_archiving_boundary,
    'Does sustained study of sacrificial law genuinely discharge the underlying obligation (occupation reading), or does it merely preserve the knowledge needed for eventual restoration without touching the obligation''s live status (archiving reading)?',
    'Comparative analysis of the specific Talmudic and later halakhic sources cited for each position, and whether the legal category invoked (e.g., ''as if he offered it'') is treated as full legal equivalence or as a rhetorical/spiritual consolation in the earliest strata of the tradition versus later codification.',
    'If the occupation reading is the correct account of the earliest sources, this constraint''s classification as a functioning, non-extractive rope is well-grounded; if the archiving reading is closer to the original intent and ''occupation'' is a later theological upgrade, this story''s beneficiary structure (particularly halakhic_authorities gaining ongoing adjudicative relevance) would look more like an institutional reinterpretation serving continuity needs of the rabbinic class specifically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(occupation_vs_archiving_boundary, conceptual, 'Whether study constitutes actual fulfillment or mere preservation of the sacrificial obligation — the core disagreement between this reading and its study_as_archiving sibling.').

omega_variable(
    occupation_vs_suspension_priority,
    'Is the framing of the obligation as something that CAN be ''occupied'' by an alternative activity even coherent, or does the messianic_suspension reading correctly hold that an obligation requiring a nonexistent physical site (the Temple) is simply suspended, with ''occupation'' being a category error smuggling closure into an intentionally open wound?',
    'Examine whether normative practice (halakhic ruling and communal behavior) treats the obligation as actively, presently satisfied (supporting occupation) or as formally pending indefinitely (supporting suspension) — e.g., whether any residual liturgical or legal markers of non-fulfillment persist alongside the study practice.',
    'If suspension is the more accurate structural description, this story''s disappearance_verdict and founding_problem_status framing overstate resolution; the doctrine would function more as a psychologically comforting overlay on a still-open obligation than as genuine occupation, which would push the classification toward piton (a maintained fiction of closure) rather than rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(occupation_vs_suspension_priority, conceptual, 'Whether the very concept of ''occupying'' an obligation via study is coherent against the rival suspension framing.').

omega_variable(
    institutional_beneficiary_capture_ambiguity,
    'Do rabbinic academies and halakhic authorities promote the occupation reading because it is the theologically correct reading of the sources, or partly because it sustains their own institutional relevance and curricular centrality (a form of beneficiary capture of the interpretive tradition)?',
    'Historical tracing of when and where the occupation doctrine gained prominence relative to periods of institutional stress for rabbinic authority (e.g., competition from Karaite or other movements that rejected rabbinic mediation), to see if doctrinal emphasis correlates with institutional pressure.',
    'If correlation is strong, this constraint''s low extractiveness score may understate a subtle self-serving function within the beneficiary set even absent an identifiable victim class — supporting a more skeptical read without requiring reclassification, since no external party is harmed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_beneficiary_capture_ambiguity, empirical, 'Whether institutional self-interest partly explains the occupation doctrine''s dominance among its administering authorities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_obligation__study_as_occupation, 0, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 0, 0.55).
narrative_ontology:measurement_basis(temp_tr_t0, projected).
narrative_ontology:measurement(temp_tr_t300, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 300, 0.45).
narrative_ontology:measurement_basis(temp_tr_t300, projected).
narrative_ontology:measurement(temp_tr_t700, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 700, 0.4).
narrative_ontology:measurement_basis(temp_tr_t700, projected).
narrative_ontology:measurement(temp_tr_t1100, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 1100, 0.35).
narrative_ontology:measurement_basis(temp_tr_t1100, projected).
narrative_ontology:measurement(temp_tr_t1500, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 1500, 0.32).
narrative_ontology:measurement_basis(temp_tr_t1500, projected).
narrative_ontology:measurement(temp_tr_t1900, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 1900, 0.3).
narrative_ontology:measurement_basis(temp_tr_t1900, observed).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 0, 0.2).
narrative_ontology:measurement_basis(temp_be_t0, projected).
narrative_ontology:measurement(temp_be_t300, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 300, 0.17).
narrative_ontology:measurement_basis(temp_be_t300, projected).
narrative_ontology:measurement(temp_be_t700, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 700, 0.16).
narrative_ontology:measurement_basis(temp_be_t700, projected).
narrative_ontology:measurement(temp_be_t1100, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 1100, 0.14).
narrative_ontology:measurement_basis(temp_be_t1100, projected).
narrative_ontology:measurement(temp_be_t1500, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 1500, 0.13).
narrative_ontology:measurement_basis(temp_be_t1500, projected).
narrative_ontology:measurement(temp_be_t1900, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 1900, 0.12).
narrative_ontology:measurement_basis(temp_be_t1900, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(temple_sacrifice_obligation__study_as_occupation, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_obligation__study_as_occupation, identity_coordination).
narrative_ontology:boltzmann_floor_override(temple_sacrifice_obligation__study_as_occupation, 0.08).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__study_as_occupation, temple_sacrifice_obligation__study_as_archiving).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__study_as_occupation, temple_sacrifice_obligation__messianic_suspension).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the temple_sacrifice_obligation kernel, decomposed per the epsilon-invariance principle: study_as_occupation (this file, claimed rope, low ε ~0.12, no victim set), study_as_archiving (expected: also low-extraction but with a live, unsatisfied obligation rather than fulfillment — a different disappearance_verdict), and messianic_suspension (expected: the obligation held in indefinite non-fulfilled/non-violated limbo, likely with a distinct beneficiary structure centered on restorationist urgency rather than institutional continuity). Each sibling must author its own ε and stakeholder set; none should be treated as measuring 'the same constraint' at different observables.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
