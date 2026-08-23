% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_kernel__symbolic_archive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_kernel__symbolic_archive_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: sacrifice_obligation_kernel__symbolic_archive_reading
 *   human_readable: Sacrificial Law as Cultural-Historical Archive (Symbolic Reading)
 *   domain: religious law/commitment systems/cultural continuity
 *
 * SUMMARY:
 *   This story instantiates ONE reading of a contested kernel: that the
 *   Torah's sacrificial legislation is received today as a
 *   cultural-historical archive, whose study preserves identity and
 *   continuity while making no halakhic claim. The standing arrangement
 *   described is the voluntary study economy around the sacrificial
 *   tractates: educators transmit, communities schedule and fund,
 *   participants attend freely, and nothing is enforced. ASSUMPTIONS: the
 *   interval 0-60 maps to roughly 1965-2025 (postwar reconstruction of
 *   diaspora education through the digital-access era); the claimed type and
 *   the metrics were authored independently per the claim/metric independence
 *   rule; sibling readings of the same kernel are separate constraint files
 *   linked through network.affects_constraints, and no sibling's content
 *   conditions this file's epsilon, parties, or classification. KEY AGENTS
 *   (by structural relationship): - rabbinic_kodashim_educators:
 *   Agenda-setter/beneficiary (institutional/identity_locked) - administer
 *   curricula and draw vocation from transmission -
 *   heritage_study_participants: Beneficiary (moderate/mobile) - voluntary
 *   learners bearing only their own elected time cost -
 *   jewish_diaspora_communities: Beneficiary (organized/mobile) - fund and
 *   celebrate the practice as continuity work -
 *   hebrew_illiterate_unaffiliated_jews: Excluded (moderate/constrained) -
 *   inherit the history but face language and institutional gates -
 *   academic_scholars_of_judaism: Observer (institutional/analytical) -
 *   external documentation and genealogical corroboration
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_kernel__symbolic_archive_reading, 0.06).
domain_priors:suppression_score(sacrifice_obligation_kernel__symbolic_archive_reading, 0.08).
domain_priors:theater_ratio(sacrifice_obligation_kernel__symbolic_archive_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, extractiveness, 0.06).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, resistance, 0.06).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_kernel__symbolic_archive_reading, rope).
narrative_ontology:human_readable(sacrifice_obligation_kernel__symbolic_archive_reading, "Sacrificial Law as Cultural-Historical Archive (Symbolic Reading)").
narrative_ontology:topic_domain(sacrifice_obligation_kernel__symbolic_archive_reading, "religious law/commitment systems/cultural continuity").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_kernel__symbolic_archive_reading, 'e5121951-b0aa-4220-a32d-fc5ced76a3cc').
narrative_ontology:cs_kernel_codification('e5121951-b0aa-4220-a32d-fc5ced76a3cc', fixed_text).
narrative_ontology:cs_authority_grounding('e5121951-b0aa-4220-a32d-fc5ced76a3cc', lineage).
narrative_ontology:cs_interpretation_layer_present('e5121951-b0aa-4220-a32d-fc5ced76a3cc').
narrative_ontology:cs_reading_relation('e5121951-b0aa-4220-a32d-fc5ced76a3cc', sacrifice_obligation_kernel__performance_only_reading, coexists_with).
narrative_ontology:cs_reading_relation('e5121951-b0aa-4220-a32d-fc5ced76a3cc', sacrifice_obligation_kernel__study_as_exercise_reading, coexists_with).
narrative_ontology:cs_reading_relation('e5121951-b0aa-4220-a32d-fc5ced76a3cc', sacrifice_obligation_kernel__messianic_suspension_reading, coexists_with).
narrative_ontology:cs_axiom('e5121951-b0aa-4220-a32d-fc5ced76a3cc', foundational, sacrificial_corpus_carries_no_operative_obligation).
narrative_ontology:cs_axiom_status(sacrificial_corpus_carries_no_operative_obligation, holdable).
narrative_ontology:cs_axiom_grounding('e5121951-b0aa-4220-a32d-fc5ced76a3cc', sacrificial_corpus_carries_no_operative_obligation, conventional).
narrative_ontology:cs_axiom('e5121951-b0aa-4220-a32d-fc5ced76a3cc', secondary, voluntary_corpus_engagement_sustains_identity_continuity).
narrative_ontology:cs_axiom_status(voluntary_corpus_engagement_sustains_identity_continuity, holdable).
narrative_ontology:cs_axiom_grounding('e5121951-b0aa-4220-a32d-fc5ced76a3cc', voluntary_corpus_engagement_sustains_identity_continuity, instrumental).
narrative_ontology:cs_reference_frame('e5121951-b0aa-4220-a32d-fc5ced76a3cc', cultural_archive_inheritance).
narrative_ontology:cs_drift_state('e5121951-b0aa-4220-a32d-fc5ced76a3cc', contemporary_heritage_revival, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('e5121951-b0aa-4220-a32d-fc5ced76a3cc', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_kernel__symbolic_archive_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__symbolic_archive_reading, heritage_study_participants).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__symbolic_archive_reading, jewish_diaspora_communities).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__symbolic_archive_reading, rabbinic_kodashim_educators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Teaches the sacrificial tractates (Zevachim, Menahot, Kereisos and kin) inside academy and adult-education curricula, decides which parts of the corpus enter the syllabus and how it is framed, and draws vocation, standing, and institutional purpose from the transmission role. Stepping away would mean leaving the profession and a self understood through handing the corpus on.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, rabbinic_kodashim_educators, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_kernel__symbolic_archive_reading, rabbinic_kodashim_educators, beneficiary).

% Studies the corpus voluntarily through evening classes, structured cycles, and online platforms, trading freely chosen time for textual literacy, connection, and meaning. Beginning and stopping carry no penalty; the only cost borne is the time the participant elects to spend.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, heritage_study_participants, beneficiary,
    moderate, biographical, mobile, global).

% Funds study tracks, schedules them into communal calendars, and celebrates completion milestones as acts of continuity. Each community chooses its level of engagement year to year and can expand or retire a track at low organizational cost.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, jewish_diaspora_communities, beneficiary,
    organized, generational, mobile, global).

% Inherits the history the archive preserves but sits outside the curricular conversation: the corpus's gates are linguistic and institutional, and translation, cost, and affiliation gatekeeping stand between them and full participation. They would engage with the material if the barriers dropped; their objection is currently voiced nowhere.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, hebrew_illiterate_unaffiliated_jews, excluded,
    moderate, biographical, constrained, global).

% Documents, edits, and contextualizes the corpus from outside the practicing communities, tracing transmission lineages and the post-destruction preservation project. Neither collects from the practice nor bears its costs; their attestations are the main external witness to the arrangement's genealogy.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, academic_scholars_of_judaism, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_obligation_kernel__symbolic_archive_reading, rabbinic_kodashim_educators).
narrative_ontology:fixing_cost_class(sacrifice_obligation_kernel__symbolic_archive_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Keeps a large, technically demanding corpus in shared possession across dispersed and discontinuous communities: common texts, recurring curriculum cycles, a trained transmitter class, and a vocabulary that links members across geography and generation. Solves the collective problem of preserving complex historical knowledge at low cost without requiring performance of the practices the texts describe.
% TRANSFER_FUNCTION: Moves discretionary time and attention from voluntary participants into corpus engagement, and moves status, honor, and institutional support toward the educators who transmit. Nothing is compelled; part of the flow reverses as communities fund the study infrastructure they value.
% ABSENT_VOICES: Unaffiliated and Hebrew-nonliterate Jews are absent from curricular decision-making; the archive's benefits concentrate among the already-literate and affiliated. Voices holding binding-obligation framings of the same corpus also sit outside this reading's conversation; their structural objection is recorded as a kernel-level omega rather than folded into this constraint.
% DISAPPEARANCE_RATIONALE: If the practice vanished overnight, the arrangements built on it would lose their object: academy curricula would shed a track, study cycles and completion celebrations would lapse, scholarly lineages would thin, and participants would lose a channel of identity work they currently use. The wider world would not notice; the rearrangement is confined to the communities organized around the practice, and it would unfold over a generation or two rather than immediately.
% FOUNDING_PROBLEM: After the destruction of the Jerusalem Temple made the sacrificial system unperformable, rabbinic Judaism faced the loss of its central ritual corpus and of the identity threaded through it; the arrangement preserves the corpus by converting performance into study, and under this reading, into heritage.
% FOUNDING_PROBLEM_CORROBORATION: Academic historians of rabbinic Judaism, writing from outside the practicing beneficiary communities, attest the post-70 CE preservation project and the continuity of the transmission lineages; source-critical scholarship corroborates the antiquity of the Yavneh-era turn to sustaining ritual-law study independently of the community's own testimony.
narrative_ontology:disappearance_verdict(sacrifice_obligation_kernel__symbolic_archive_reading, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_kernel__symbolic_archive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_kernel__symbolic_archive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sacrifice_obligation_kernel__symbolic_archive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_kernel__symbolic_archive_reading, 0.06, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_kernel__symbolic_archive_reading_tests).
:- end_tests(sacrifice_obligation_kernel__symbolic_archive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Metrics are descriptive, the claim is structural, and the two were authored without reconciling them. Extractiveness sits at 0.06 because nothing is compelled: no fee, no penalty for lapse, no conscripted labor; the residue is an expectation gradient that omega tracking treats as open. Suppression 0.08 reflects that the mild pull toward participation runs through internalized esteem rather than structural barrier; suppression is authored as a raw structural property and is deliberately unscaled, since the engine scales only extraction. Theater 0.18: milestone celebrations and standardized cycles are genuine ceremony but subordinate to the transmission function they mark. Accessibility collapse 0.30: heritage channels are plentiful (history, language study, museums, genealogy) and choosing the archive path forecloses none. Resistance 0.06: indifference and occasional anti-clerical critique, never organization. The temporal series share one grid (points 0, 12, 24, 36, 48, 60) with both tracked metrics authored at every point; suppression_requirement is intentionally untracked because the enforcement picture is static - nothing is enforced, and the scalar carries that fact. The gentle upward creep in both series reflects institutional professionalization (formalized milestones, funded programs), not ratcheting coercion; it is a watch item sized far below any threshold that would move classification. Coordination type identity_coordination fits the dominant function (boundary and membership continuity); no floor override is authored, and measured epsilon sits well beneath even the conservative default floor for that type.
 *
 * PERSPECTIVAL GAP:
 *   Seat divergence here lives in exit options rather than extraction sign. The educator seat fuses professional and relational identity with transmission - the role has become the self - so the same practice reads as vocation and duty from that chair, while mobile participants read it as chosen enrichment they may suspend without cost. If that identity frame broke (academy contraction, or full absorption of the corpus into academic Jewish studies), the educator's exit options shift identity_locked toward mobile and the divergence between seats collapses. The engine computes divergent per-seat classifications from the authored power/exit/role data; nothing in this commentary adjudicates them by hand.
 *
 * DIRECTIONALITY LOGIC:
 *   Every declared party is a beneficiary, so the derivation seats all of them near the subsidy end of the directionality axis: participants trade freely elected time for meaning; communities convert discretionary funds into continuity; educators invest labor but recover vocation and standing, and their identity-lock raises the investment side of their ledger, edging them toward symmetric without crossing it - they still net-gain. No victim declarations exist, so no seat approaches the target pole and effective extraction stays floor-adjacent everywhere; the arrangement's global scope amplifies verification difficulty, but amplifying a negligible base yields a negligible result. The excluded seat stands off the axis entirely: barred entry is not extraction, and its grievance is routed through the access-barrier omega rather than through the extraction arithmetic.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - continuity for a corpus whose ritual referent was destroyed - is live, so no mandatrophy resolution is declared. The rope placement blocks two opposite misreads: calling this a snare would manufacture victims out of voluntary participants and mistake communal esteem for coercion; calling it a mountain would assert naturalness where a community's continuing choice operates, which is why emerges_naturally stays false. The measurement series guard the third failure mode, drift: should the expectation gradient harden into de facto obligation, base_extractiveness and suppression would climb past their thresholds and the engine would move the classification without anyone re-authoring the claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint instantiates one reading (symbolic_archive) of the sacrifice_obligation_kernel; what would each sibling reading change structurally if adopted as the operative frame?',
    'Comparative read across the four sibling story files: performance_only creates a binding obligation with prospective violators (materially positive epsilon, a payer set appears); messianic_suspension preserves a latent obligation plus a readiness-maintenance duty on the studious; study_as_exercise converts study hours into discharge, attaching normative weight to engagement itself.',
    'Adopting any sibling flips epsilon from near-zero to materially positive and introduces victim/payer sets, moving classification off rope; this file''s metrics and parties are valid only for the archive reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: one of four readings of a shared kernel; siblings are separate constraints, not hedges inside this one.').

omega_variable(
    framework_dependent_foreclosure,
    'Do this reading''s premises logically foreclose the object-level sibling readings, or do all four merely coexist? Within a framework committed to Torah''s binding normativity, receiving the sacrificial corpus as inert archive looks internally unstable; within a secular-cultural framework, the object-level readings are idle rather than refuted.',
    'Survey halakhic authorities and communal practice for any tradition that holds binding-Torah commitment together with archival treatment of korbanot; if none exists, foreclosure holds inside the traditional framework and fails outside it.',
    'If foreclosure obtains within the binding framework, reading_relations shift from coexists_with to forecloses for that jurisdiction and the engine''s axiomatic-contradiction computation changes accordingly; if not, the four-reading contest is never resolved by logic alone and persists as factional coexistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framework_dependent_foreclosure, conceptual, 'Whether archive framing annihilates or merely sidesteps the obligation readings is relative to the host framework.').

omega_variable(
    voluntary_vs_expected_participation,
    'Is participation in archive-study purely voluntary, or does communal esteem for learners create an expectation gradient that functions as a soft obligation?',
    'Lapse interviews and longitudinal participation data: whether stopping carries social sanction or diminished standing, comparing retention curves of publicly celebrated learners against uncelebrated ones.',
    'A real expectation gradient adds a payer-side residue and lifts suppression above the noise floor, pushing classification from rope toward tangled_rope with the educator seat as partial collector; a null result leaves the current classification intact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_vs_expected_participation, empirical, 'Soft-expectation ambiguity inside a nominally voluntary practice.').

omega_variable(
    access_barrier_composition,
    'The archive''s benefits concentrate among the Hebrew-literate and institutionally affiliated: are the barriers facing unaffiliated Jews structural (language, cost, gatekeeping) or preferential (they simply choose other heritage channels)?',
    'Uptake studies offering unaffiliated Jews low-barrier translations and formats; high uptake indicates blocked demand, low uptake indicates absent demand.',
    'Blocked demand implies the coordination arrangement shuts out part of its natural constituency, a mild suppression and equity charge against an otherwise-clean rope; absent demand leaves the classification untouched and retires the excluded-seat grievance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(access_barrier_composition, empirical, 'Whether the excluded seat is blocked from the archive or merely engaged elsewhere.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_kernel__symbolic_archive_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(sacr_tr_t12, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 12, 0.11).
narrative_ontology:measurement(sacr_tr_t24, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 24, 0.13).
narrative_ontology:measurement(sacr_tr_t36, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 36, 0.15).
narrative_ontology:measurement(sacr_tr_t48, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 48, 0.17).
narrative_ontology:measurement(sacr_tr_t60, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 60, 0.18).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 0, 0.03).
narrative_ontology:measurement(sacr_be_t12, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 12, 0.04).
narrative_ontology:measurement(sacr_be_t24, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 24, 0.04).
narrative_ontology:measurement(sacr_be_t36, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 36, 0.05).
narrative_ontology:measurement(sacr_be_t48, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 48, 0.06).
narrative_ontology:measurement(sacr_be_t60, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 60, 0.06).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(sacrifice_obligation_kernel__symbolic_archive_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_kernel__symbolic_archive_reading, identity_coordination).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__symbolic_archive_reading, sacrifice_obligation_kernel__performance_only_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__symbolic_archive_reading, sacrifice_obligation_kernel__study_as_exercise_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__symbolic_archive_reading, sacrifice_obligation_kernel__messianic_suspension_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: one colloquial label - 'the obligation of sacrifices after the Temple' - covers four structurally distinct constraints with different epsilon and party structures. performance_only binds and awaits performers; messianic_suspension binds latently and charges the studious with readiness; study_as_exercise lets study discharge the obligation; this file's symbolic_archive reading strips operative force entirely and leaves a voluntary heritage coordination with epsilon near zero. The stories share one kernel text and are linked family-wide so that drift in any reading's fortunes registers against its siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
