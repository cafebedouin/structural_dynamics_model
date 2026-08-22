% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_commitment__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_commitment__performance_only, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: temple_sacrifice_commitment__performance_only
 *   human_readable: Sacrificial Commitment as Dormant Husk (Performance-Only Reading: Study Is Archival Preservation, Not Occupation)
 *   domain: religious law / halakhic tradition / commitment-system theory
 *
 * SUMMARY:
 *   This file instantiates ONE reading — performance_only — of the contested
 *   kernel temple_sacrifice_commitment: the halakhic commitment to the Temple
 *   sacrificial order, rendered materially unperformable by the destruction
 *   that ended the cult. Under this reading the commitment requires material
 *   instantiation; study of the sacrificial laws, however extensive, is
 *   archival preservation of a defunct practice, not occupation of the
 *   commitment. The standing arrangement under contest — the referent of
 *   every metric here — is the present arrangement assessed by this reading's
 *   own lights: a dormant commitment surrounded by a large preservation
 *   apparatus (academy study of the sacrificial orders, daily liturgical
 *   recitation, restorationist preparation), with no current victim set.
 *   Claim and metrics are independent authored facts: the claimed type states
 *   what this reading's structure appears to be, and the metrics describe its
 *   observed operation; the engine computes each seat's type from the
 *   structural data, and divergence between claim and computed type is the
 *   measurement the corpus exists to take. This story is one member of a
 *   four-reading constraint family; the siblings (study_as_exercise,
 *   hybrid_preparatory, symbolic_transformation) instantiate different
 *   constraints from the same kernel — different occupation predicates,
 *   different epsilon values, and under some readings different victim sets.
 *   They are linked, not averaged: no sibling's claims enter this file's
 *   classification.
 *
 * KEY AGENTS:
 *   - performance_only_authorities: agenda-setting seat (organized / identity_locked) — articulates and maintains the dormancy thesis
 *   - sacrificial_law_students: primary beneficiary seat (moderate / constrained) — study communities whose engagement the reading frames as preservation
 *   - temple_restoration_movement: secondary beneficiary seat (organized / identity_locked) — inherits the preserved archive as operational asset
 *   - study_as_exercise_majority: excluded seat (institutional / identity_locked) — the Talmudic-liturgical mainstream whose occupation claim this reading forecloses
 *   - restoration_ethics_dissenters: excluded seat (moderate / mobile) — would object to restoration without ethical evolution; absent while the commitment is dormant
 *   - commitment_system_analysts: observer seat (analytical / analytical) — sees the full four-reading contest from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_commitment__performance_only, 0.1).
domain_priors:suppression_score(temple_sacrifice_commitment__performance_only, 0.15).
domain_priors:theater_ratio(temple_sacrifice_commitment__performance_only, 0.13).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, extractiveness, 0.1).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, theater_ratio, 0.13).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_commitment__performance_only, rope).
narrative_ontology:human_readable(temple_sacrifice_commitment__performance_only, "Sacrificial Commitment as Dormant Husk (Performance-Only Reading: Study Is Archival Preservation, Not Occupation)").
narrative_ontology:topic_domain(temple_sacrifice_commitment__performance_only, "religious law / halakhic tradition / commitment-system theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_commitment__performance_only, '896c6062-a96d-413d-8ea9-c1ca0028db0a').
narrative_ontology:cs_kernel_codification('896c6062-a96d-413d-8ea9-c1ca0028db0a', formalized).
narrative_ontology:cs_authority_grounding('896c6062-a96d-413d-8ea9-c1ca0028db0a', lineage).
narrative_ontology:cs_interpretation_layer_present('896c6062-a96d-413d-8ea9-c1ca0028db0a').
narrative_ontology:cs_reading_relation('896c6062-a96d-413d-8ea9-c1ca0028db0a', temple_sacrifice_commitment__study_as_exercise, forecloses).
narrative_ontology:cs_reading_relation('896c6062-a96d-413d-8ea9-c1ca0028db0a', temple_sacrifice_commitment__hybrid_preparatory, forecloses).
narrative_ontology:cs_reading_relation('896c6062-a96d-413d-8ea9-c1ca0028db0a', temple_sacrifice_commitment__symbolic_transformation, forecloses).
narrative_ontology:cs_axiom('896c6062-a96d-413d-8ea9-c1ca0028db0a', foundational, occupation_requires_material_instantiation).
narrative_ontology:cs_axiom_status(occupation_requires_material_instantiation, holdable).
narrative_ontology:cs_axiom_grounding('896c6062-a96d-413d-8ea9-c1ca0028db0a', occupation_requires_material_instantiation, deontological).
narrative_ontology:cs_axiom('896c6062-a96d-413d-8ea9-c1ca0028db0a', secondary, study_is_preservation_not_performance).
narrative_ontology:cs_axiom_status(study_is_preservation_not_performance, holdable).
narrative_ontology:cs_axiom_grounding('896c6062-a96d-413d-8ea9-c1ca0028db0a', study_is_preservation_not_performance, instrumental).
narrative_ontology:cs_reference_frame('896c6062-a96d-413d-8ea9-c1ca0028db0a', temple_standing_material_performance).
narrative_ontology:cs_drift_state('896c6062-a96d-413d-8ea9-c1ca0028db0a', contemporary_post_destruction_era, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('896c6062-a96d-413d-8ea9-c1ca0028db0a', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_commitment__performance_only, temple_sacrifice_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__performance_only, sacrificial_law_students).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__performance_only, temple_restoration_movement).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__performance_only, commandment_deed_primacy).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__performance_only, commitment_material_instantiation_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Rabbinic scholars and religious philosophers who argue that the sacrificial commandments remain binding but are presently unperformable, and that studying their laws preserves a defunct practice rather than fulfilling it. They write, teach, and publish within academies and journals; their standing rests on the argument's distinctiveness inside a tradition whose mainstream reads study as fulfillment. Abandoning the position would mean conceding the framework their scholarly identity is built on; leaving the halakhic world entirely is not a live option for them.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, performance_only_authorities, agenda_setter,
    organized, generational, identity_locked, global).

% Students in yeshivot and study cycles who work through the orders of sacrifices, Temple service, and purity as standard curriculum, and who recite the sacrificial order in daily liturgy. This reading gives their engagement an honest description — preservation of knowledge awaiting conditions — rather than a fulfillment claim they do not experience as performance. Their participation is curricular and liturgical; individually they neither chose the syllabus nor can redirect it.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, sacrificial_law_students, beneficiary,
    moderate, biographical, constrained, global).

% Organizations and activists preparing for renewed Temple service: reconstructing vessels, cataloguing priestly lineages, rehearsing procedures, drafting liturgy for a restored cult. The preserved archive of sacrificial knowledge is their operating inventory, and its value to them grows as they judge restoration nearer. Their organizations exist for the restoration project itself; redirecting their efforts would dissolve the institutions' purpose. They are centered in Jerusalem and draw support from diaspora communities.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, temple_restoration_movement, beneficiary,
    organized, generational, identity_locked, regional).

% The Talmudic and liturgical mainstream, from the sugya accounting study of the sacrifices as though they were offered to the daily recitation of the sacrificial order in prayer. They hold that intellectual engagement occupies the commandment while the Temple stands unbuilt, and they would reject this reading's characterization of their practice as archival. They are not party to this reading's framework; their practice continues unimpeded outside it, backed by the tradition's institutional weight.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, study_as_exercise_majority, excluded,
    institutional, generational, identity_locked, global).

% Ethicists, animal-welfare-minded traditionalists, and thinkers in the lineage expecting the restored service to be transformed — grain offerings, revised liturgy, welfare safeguards — who would object if material restoration proceeded without the ethical evolution they hold prerequisite. They are absent from the present arrangement because the commitment is dormant and no restoration decision is on the table; their objection becomes live only when performance conditions return.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, restoration_ethics_dissenters, excluded,
    moderate, biographical, mobile, global).

% Scholars of religious law and commitment-system theory who observe the four-way contest over the sacrificial commitment's status. They hold no position inside the framework; they map how each reading assigns a different status to the same codified corpus and what each assignment would imply for the tradition's self-understanding.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, commitment_system_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(temple_sacrifice_commitment__performance_only, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains the operational knowledge of the sacrificial order — procedures, vessel specifications, priestly genealogies, calendar and purity calculations — as a coordinated archive across study communities, so the commitment could be performed if material conditions returned, while fixing the commitment's present status as unoccupied and blocking false claims that study already fulfills it.
% TRANSFER_FUNCTION: Moves attention and scholarly labor from the study community into the preserved archive of sacrificial knowledge, and moves the doctrine of dormancy from the reading's authorities into the community's self-understanding. No material goods move in either direction; the commitment's costs — animals, altar, priesthood — are suspended with the practice itself.
% ABSENT_VOICES: The study-as-fulfillment majority is excluded from this framework as mistaken, though it is the tradition's operative position and would object loudest. Restoration-ethics dissenters are absent because dormancy keeps their question off the table. The animals a restored cult would require have no seat at all, in this framework or any sibling's.
% DISAPPEARANCE_RATIONALE: The material world barely moves — no goods flow under this arrangement either way — but the status map of the commitment would reorganize: study would be understood as performing the command under the majority's claim, the restoration movement would lose the preserved-for-future-use framing of its inventory, and the dissenters' prospective objection would lose its sharpest anchor, the claim that nothing is currently being fulfilled. The reading's holders' scholarly identities, built on the dormancy thesis, would dissolve into the majority position.
% FOUNDING_PROBLEM: After 70 CE destroyed the Temple, a community bound to commandments it could no longer perform had to decide what remained of the commitment — lapsed, suspended, transferred to substitutes, or binding but unoccupied — and what, if anything, studying the unperformable laws accomplishes.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside this reading's beneficiary set: the sibling readings' own authorities agree the problem is live — the Talmudic discussion treating study of the sacrifices as offering presupposes the performance gap, and the liturgy's daily sacrificial recitation and Ninth of Av observances institutionalize the absence; historians of halakha document the post-destruction restructuring of the sacrificial material into study and memory. What the corroborating sources dispute is the answer — occupation, suspension, transformation, or dormancy — not the problem's liveness. No corroborating source outside the restoration movement claims performance conditions currently obtain.
narrative_ontology:disappearance_verdict(temple_sacrifice_commitment__performance_only, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_commitment__performance_only, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_commitment__performance_only, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(temple_sacrifice_commitment__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_commitment__performance_only, 0.1, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_commitment__performance_only_tests).
:- end_tests(temple_sacrifice_commitment__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is low (0.10, near the identity-coordination floor of 0.08): the arrangement moves no material goods, levies no costs on any seat, and its near-floor residue is the ordinary cost of maintaining a knowledge archive and a status doctrine. Suppression is low (0.15): the reading forecloses rival occupation claims within its own framework but holds no enforcement machinery — the majority reading flourishes outside it and no seat is coerced. Theater is low (0.13): under this reading's lights the study apparatus is honestly labeled preservation rather than performance; the modest rise across the interval tracks restorationist performativity (vessel reconstructions, priestly rehearsals) that is preservation-adjacent but carries a growing demonstrative share. Accessibility collapse is moderate-low (0.30): the reading collapses the sibling occupation claims only inside its own framework; in the world they remain live, institutionally embedded, and accessible. Resistance is substantial (0.55): the Talmudic-liturgical mainstream actively holds the contrary occupation claim, so this reading operates as a minority position against the tradition's operative assumption. All three tracked series run on one shared time grid (T0 roughly 1975, T50 roughly 2025, units roughly years). Suppression_requirement is tracked because this story's dynamic is a maintenance-burden change, not a static enforcement picture: as restorationist activity intensified across the interval, holding the dormancy claim required more discursive work against both the majority occupation claim and accelerationist preparation.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently. From the authorities' seat the arrangement is honest self-understanding: naming the commitment dormant is fidelity, not loss. From the excluded majority's seat the same arrangement is a denial of the tradition's operative claim that study performs the command — they experience this reading as an error about their practice, not as a structure they sit inside. From the restoration movement's seat the archive is an operational asset whose value grows with proximity to restoration. From the dissenters' seat the arrangement is a prospective risk: a preserved, rehearsed, instrumented cult lowers the cost of a restoration they hold ethically unready. The engine computes these divergences from the power, exit, and role data; this file does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are declared and victims are deliberately not. The students and the restoration movement sit near the beneficiary end — the reading subsidizes their engagement with honest meaning and preserved capability — and no current seat bears extraction, so no high-directionality target exists: this is the structural content of the expected delta's no-current-victim-set. The excluded seats take the canonical fallback near symmetry: the majority loses only a discursive claim inside a framework it already rejects; the dissenters bear a prospective rather than present cost. Potential future victims — the animals and dissenters a restoration without ethical evolution would create — are NOT declared as current victims, because declaring them would fabricate a present extraction structure that does not exist; the conditional is carried in the omega restoration_without_ethical_evolution instead. This keeps epsilon invariant: the referent is the standing dormant arrangement, and under this reading's lights it extracts almost nothing. Receipt: the near-floor residue of extraction is coordination cost borne diffusely by the discourse; no named seat captures gains. The students and restoration movement accrue the coordination output (honest meaning, preserved capability) — that is benefit, not extraction-receipt — so gain_flow is authored as diffuse, an affirmative finding from the structural data, not a default.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — what a community owes a command it cannot perform — is live, not dead: the Temple stands unbuilt and the sibling contest is active, so no mandatrophy resolution is declared and none is due. The rope classification does double duty here. It blocks the majority reading's move (reclassifying study as performance would dissolve the dormancy fact into a fulfillment claim and hide the husk), and it blocks the opposite error (reading the preservation apparatus as extraction, which the near-floor extraction and low theater ratio contradict). It also marks the drift risk honestly: the arrangement is stable only while the commitment stays dormant; a restoration without ethical evolution would convert the archive into operating infrastructure, the no-victim status would flip, and the classification would degrade toward extraction — that contingency lives in the omega, not in the base classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is the performance_only reading of the temple_sacrifice_commitment kernel; what would adopting a sibling reading change structurally?',
    'Framework choice, not data: each sibling is a separate constraint file, and family comparison shows the deltas — study_as_exercise reclassifies the study apparatus as the performance itself (near-zero extraction on the commitment, different beneficiary structure), hybrid_preparatory introduces a suspended-live category carrying a standing maintenance burden, symbolic_transformation dissolves the performance referent entirely.',
    'The rope classification, near-floor extraction, and empty victim set hold only under this reading; sibling files carry their own classifications, and cross-reading verdicts belong to the family comparison, not to this file.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer position: one reading of the temple_sacrifice_commitment kernel, named and delimited.').

omega_variable(
    occupation_predicate_disagreement_location,
    'Where exactly do the four readings disagree?',
    'Structural analysis across the sibling files: all affirm the command''s bindingness; they divide on the occupation predicate — whether study occupies (study_as_exercise), maintains-in-suspension (hybrid_preparatory), instantiates a transformed form (symbolic_transformation), or leaves the commitment unoccupied pending material performance (this reading).',
    'Classification divergence across the family is located in one predicate; disputes that present as disputes about extraction, victims, or theater are downstream of it and should not be re-modeled as separate structural facts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(occupation_predicate_disagreement_location, conceptual, 'The disagreement''s location: the occupation predicate, not the command''s bindingness.').

omega_variable(
    restoration_without_ethical_evolution,
    'If material conditions for performance returned, would restoration proceed with the ethical evolution the dissenters hold prerequisite (transformed offerings, welfare safeguards), or without it — creating a victim set that does not currently exist?',
    'Observable only at restoration; pre-restoration signals include the restoration movement''s drafted liturgy (animal versus grain offerings), stated welfare safeguards, and whether dissenting voices gain seats in restoration planning bodies.',
    'Without ethical evolution, the arrangement''s no-victim status flips retroactively for the preparation phase — the preserved archive becomes operating infrastructure for a cult with identifiable victims, and the classification degrades toward extraction; with evolution, the dormancy framing is vindicated as the honesty that made an ethical transition possible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(restoration_without_ethical_evolution, empirical, 'Conditional future victim set: restoration attempted without ethical evolution.').

omega_variable(
    study_function_understates_identity_work,
    'Does this reading''s characterization of study as mere archival preservation understate what study does for its practitioners — does the study apparatus functionally occupy the commitment even for holders of this reading?',
    'Comparative practice study: do communities holding this reading study, recite, and teach the sacrificial order differently from majority-reading communities, or does practice converge while theory diverges?',
    'If practice converges, the reading is unstable — its holders'' lived engagement contradicts their theory, and the dormancy claim functions partly as self-description rather than description; if practice diverges, the preservation framing is descriptively accurate and the low theater ratio is corroborated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_function_understates_identity_work, empirical, 'Whether mere-preservation accurately describes the study apparatus''s lived function.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_commitment__performance_only, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_commitment__performance_only, theater_ratio, 0, 0.08).
narrative_ontology:measurement(temp_tr_t10, temple_sacrifice_commitment__performance_only, theater_ratio, 10, 0.09).
narrative_ontology:measurement(temp_tr_t20, temple_sacrifice_commitment__performance_only, theater_ratio, 20, 0.1).
narrative_ontology:measurement(temp_tr_t30, temple_sacrifice_commitment__performance_only, theater_ratio, 30, 0.11).
narrative_ontology:measurement(temp_tr_t40, temple_sacrifice_commitment__performance_only, theater_ratio, 40, 0.12).
narrative_ontology:measurement(temp_tr_t50, temple_sacrifice_commitment__performance_only, theater_ratio, 50, 0.13).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_commitment__performance_only, base_extractiveness, 0, 0.06).
narrative_ontology:measurement(temp_be_t10, temple_sacrifice_commitment__performance_only, base_extractiveness, 10, 0.07).
narrative_ontology:measurement(temp_be_t20, temple_sacrifice_commitment__performance_only, base_extractiveness, 20, 0.08).
narrative_ontology:measurement(temp_be_t30, temple_sacrifice_commitment__performance_only, base_extractiveness, 30, 0.08).
narrative_ontology:measurement(temp_be_t40, temple_sacrifice_commitment__performance_only, base_extractiveness, 40, 0.09).
narrative_ontology:measurement(temp_be_t50, temple_sacrifice_commitment__performance_only, base_extractiveness, 50, 0.1).

% Suppression requirement over time
narrative_ontology:measurement(temp_su_t0, temple_sacrifice_commitment__performance_only, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(temp_su_t10, temple_sacrifice_commitment__performance_only, suppression_requirement, 10, 0.11).
narrative_ontology:measurement(temp_su_t20, temple_sacrifice_commitment__performance_only, suppression_requirement, 20, 0.12).
narrative_ontology:measurement(temp_su_t30, temple_sacrifice_commitment__performance_only, suppression_requirement, 30, 0.13).
narrative_ontology:measurement(temp_su_t40, temple_sacrifice_commitment__performance_only, suppression_requirement, 40, 0.14).
narrative_ontology:measurement(temp_su_t50, temple_sacrifice_commitment__performance_only, suppression_requirement, 50, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_commitment__performance_only, identity_coordination).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__performance_only, temple_sacrifice_commitment__study_as_exercise).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__performance_only, temple_sacrifice_commitment__hybrid_preparatory).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__performance_only, temple_sacrifice_commitment__symbolic_transformation).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'the sacrificial commitment' covers four structurally distinct claims that differ on the occupation predicate. This file authors the performance_only reading (dormant husk; study is archival; no current victims; epsilon ~0.10). The sibling files author study_as_exercise (study is the performance; the apparatus is occupied, not archival), hybrid_preparatory (suspended-live commitment; study carries a standing maintenance burden), and symbolic_transformation (authorized transformation; the performance referent itself has moved). The upstream Talmudic study-as-offering tradition is cited by the other readings as evidence; this reading's distinctiveness is its refusal of that citation. Each file carries its own epsilon, beneficiaries, and claimed type; they are linked here and in each sibling's network block, never merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
