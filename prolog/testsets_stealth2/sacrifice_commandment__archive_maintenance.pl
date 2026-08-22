% ============================================================================
% CONSTRAINT STORY: sacrifice_commandment__archive_maintenance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_commandment__archive_maintenance, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: sacrifice_commandment__archive_maintenance
 *   human_readable: Sacrifice-Law Study as Restoration Archive (Archive-Maintenance Reading)
 *   domain: religious/halakhic/commitment_system
 *
 * SUMMARY:
 *   This story instantiates the archive_maintenance reading of the
 *   sacrifice_commandment kernel: since the Temple's destruction made
 *   sacrificial practice impossible, the commandment persists as a standing
 *   duty to study sacrificial law, justified — on this reading — by
 *   preserving the technical knowledge a future restoration would require.
 *   The arrangement is maintained by academies that set curricula, certify
 *   mastery, and collect the funding and deference the duty channels to them;
 *   its costs fall on present students (years of study with no practice
 *   outlet) and on the communities that fund and schedule around it; its
 *   declared beneficiary is a generation that does not yet exist and cannot
 *   consent. The ε referent is the standing study arrangement as this reading
 *   assesses it — moderate (≈0.55): real present opportunity costs and
 *   institutional claims, weighed against a benefit stream entirely
 *   contingent on a restoration event no present actor controls. Sibling
 *   readings of the same kernel are separate constraints with different ε,
 *   decomposed per the ε-invariance principle: study_as_performance authors
 *   the same study practice as intrinsically fulfilling (low ε — the practice
 *   is its own point); performance_only authors the commandment itself as
 *   suspended (the study duty loses commandment force and the cost claim
 *   dissolves toward voluntary scholarship). This file authors ε only for its
 *   own reading. The upstream member of the family in empirical confidence is
 *   performance_only's factual core (practice is impossible without a Temple
 *   — uncontested); archive_maintenance builds its justification on top of
 *   it, and study_as_performance contests the deferral itself.
 *
 * KEY AGENTS:
 *   - - future_temple_generation: Declared beneficiary (powerless/trapped) — inherits the archive's state but cannot act or consent in the present
 *   - - rabbinic_archive_institutions: Agenda-setting administrator and operative collector (institutional/arbitrage) — sets the curriculum and collects the funding, authority, and continuity
 *   - - present_torah_students: Primary present bearer of costs (moderate/identity_locked) — years of study with no practice outlet
 *   - - observant_lay_communities: Cost-bearing supporters with incidental cohesion gains (organized/constrained)
 *   - - temple_preparation_movements: Secondary beneficiary drawing legitimacy and donations from the preparation frame (organized/constrained)
 *   - - halakhic_reformers: Excluded critics (moderate/mobile) — objections circulate outside the curriculum-setting conversation
 *   - - religious_studies_scholars: Analytical observer (analytical/analytical) — sees the full structure from no committed seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_commandment__archive_maintenance, 0.55).
domain_priors:suppression_score(sacrifice_commandment__archive_maintenance, 0.48).
domain_priors:theater_ratio(sacrifice_commandment__archive_maintenance, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, extractiveness, 0.55).
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_commandment__archive_maintenance, tangled_rope).
narrative_ontology:human_readable(sacrifice_commandment__archive_maintenance, "Sacrifice-Law Study as Restoration Archive (Archive-Maintenance Reading)").
narrative_ontology:topic_domain(sacrifice_commandment__archive_maintenance, "religious/halakhic/commitment_system").

domain_priors:requires_active_enforcement(sacrifice_commandment__archive_maintenance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_commandment__archive_maintenance, 'e8464dd7-790b-45c0-b04f-e0f9da971482').
narrative_ontology:cs_kernel_codification('e8464dd7-790b-45c0-b04f-e0f9da971482', fixed_text).
narrative_ontology:cs_authority_grounding('e8464dd7-790b-45c0-b04f-e0f9da971482', lineage).
narrative_ontology:cs_interpretation_layer_present('e8464dd7-790b-45c0-b04f-e0f9da971482').
narrative_ontology:cs_reading_relation('e8464dd7-790b-45c0-b04f-e0f9da971482', sacrifice_commandment__study_as_performance, coexists_with).
narrative_ontology:cs_reading_relation('e8464dd7-790b-45c0-b04f-e0f9da971482', sacrifice_commandment__performance_only, coexists_with).
narrative_ontology:cs_axiom('e8464dd7-790b-45c0-b04f-e0f9da971482', foundational, deferred_utility_of_study).
narrative_ontology:cs_axiom_status(deferred_utility_of_study, holdable).
narrative_ontology:cs_axiom_grounding('e8464dd7-790b-45c0-b04f-e0f9da971482', deferred_utility_of_study, instrumental).
narrative_ontology:cs_axiom('e8464dd7-790b-45c0-b04f-e0f9da971482', foundational, living_transmission_necessity).
narrative_ontology:cs_axiom_status(living_transmission_necessity, holdable).
narrative_ontology:cs_axiom_grounding('e8464dd7-790b-45c0-b04f-e0f9da971482', living_transmission_necessity, empirically_contingent).
narrative_ontology:cs_axiom('e8464dd7-790b-45c0-b04f-e0f9da971482', secondary, restoration_certainty_premise).
narrative_ontology:cs_axiom_status(restoration_certainty_premise, holdable).
narrative_ontology:cs_axiom_grounding('e8464dd7-790b-45c0-b04f-e0f9da971482', restoration_certainty_premise, theological).
narrative_ontology:cs_reference_frame('e8464dd7-790b-45c0-b04f-e0f9da971482', post_destruction_preservation_duty).
narrative_ontology:cs_drift_state('e8464dd7-790b-45c0-b04f-e0f9da971482', contemporary_written_codification_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e8464dd7-790b-45c0-b04f-e0f9da971482', '').
narrative_ontology:cs_kernel_id(sacrifice_commandment__archive_maintenance, sacrifice_commandment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_commandment__archive_maintenance, future_temple_generation).
narrative_ontology:constraint_beneficiary(sacrifice_commandment__archive_maintenance, rabbinic_archive_institutions).
narrative_ontology:constraint_beneficiary(sacrifice_commandment__archive_maintenance, temple_preparation_movements).
narrative_ontology:constraint_victim(sacrifice_commandment__archive_maintenance, present_torah_students).
narrative_ontology:constraint_victim(sacrifice_commandment__archive_maintenance, observant_lay_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sacrifice_commandment__archive_maintenance, observant_lay_communities).
narrative_ontology:constraint_vindicates(sacrifice_commandment__archive_maintenance, mesorah_necessity_doctrine).
narrative_ontology:constraint_vindicates(sacrifice_commandment__archive_maintenance, restoration_certainty_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Will inherit whatever state the sacrificial knowledge-base is in when a restored Temple becomes possible. Cannot act, consent, or decline in the present; everything about their position — whether they receive a usable procedural archive or a gap-ridden one — is determined by choices made by people not yet born at their birth. Their stake is entirely prospective and they have no mechanism to refuse it.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, future_temple_generation, beneficiary,
    powerless, generational, trapped, global).

% Set the curriculum that assigns sacrifice law its place in the study program, certify mastery, and maintain the academies through which the material is transmitted generation to generation. Collect tuition, communal funding, and the institutional continuity that comes from administering a study duty spanning centuries. Could redirect the curriculum toward other subjects at any time; doing so would dissolve the academies' distinctive role and require admitting that the written codes suffice without them.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, rabbinic_archive_institutions, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_commandment__archive_maintenance, rabbinic_archive_institutions, beneficiary).

% Reconstruct vessels, train would-be priests, and campaign on restoration timelines, drawing donations and volunteers on the premise that preparation is both possible and urgent. Depend on the study tradition for the technical content they claim to operationalize; their fundraising appeals presuppose that the knowledge exists and is being kept alive to be applied.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, temple_preparation_movements, beneficiary,
    organized, generational, constrained, regional).

% Devote years of study to sacrificial procedure they will never perform — species qualification, measurements, disqualification rules, priestly and altar law, service order. The mastery carries scholarly standing inside their communities but has no practice outlet. Leaving the study track means forgoing the communal standing bound up with advanced learning and, often, rethinking a life plan and self-concept built around the academy.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, present_torah_students, payer,
    moderate, biographical, identity_locked, global).

% Fund the academies through tithing and donations, schedule communal and family life around study cycles that include the sacrificial orders, and recite sacrifice-related liturgy daily. Receive communal cohesion and a felt continuity with the Temple service; bear the cost of the resources and deference the arrangement requires. Reducing support draws communal censure; full withdrawal means leaving observant life altogether.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, observant_lay_communities, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_commandment__archive_maintenance, observant_lay_communities, beneficiary).

% Question the priority given to sacrifice study — from the medieval rationalist discomfort with the sacrificial system itself to modern movements that reallocate study time to other subjects. Their arguments circulate in print and in adjacent institutions but hold no seat in the academies that set the curriculum; their practical influence is confined to communities that have already stepped outside the traditional study framework.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, halakhic_reformers, excluded,
    moderate, generational, mobile, global).

% Document what the study practice has and has not preserved, compare written codifications against living transmission, and trace how the stated rationale for the study duty has shifted across centuries. Neither collect from the arrangement nor bear its costs; they can describe its mechanics from outside any of the committed seats.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, religious_studies_scholars, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_commandment__archive_maintenance, rabbinic_archive_institutions).
narrative_ontology:fixing_cost_class(sacrifice_commandment__archive_maintenance, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Keeps a body of procedural knowledge — animal qualification, measurements, disqualification rules, priestly and altar law, service order — alive across generations in which it cannot be practiced, addressing the problem that no single generation has a use for the knowledge it is asked to maintain.
% TRANSFER_FUNCTION: Moves present resources — years of student labor and study time, communal funding and deference, curricular priority — from the current generation of students and supporting households to the administering academies, with the declared ultimate destination of a future generation that would use the preserved procedure in a restored Temple.
% ABSENT_VOICES: The future generation — the declared beneficiary — has no seat and cannot consent to or decline the inheritance it is said to receive. Halakhic rationalists and reformers argue from print and adjacent institutions but hold no seat in the curriculum-setting academies. Advocates of written-only preservation are answered by mesorah doctrine rather than seated as a live alternative. The unanimity of the study duty inside the academies is produced partly by who was never admitted to the conversation.
% DISAPPEARANCE_RATIONALE: If the study duty vanished overnight, the academies would lose their distinctive curriculum and much of their claim on communal funding, students would redirect years of study toward other subjects, and the daily liturgical and educational rhythms built around sacrificial material would reorganize. The restoration movements would lose the knowledge base their preparation claims to draw on. Nothing physical rearranges — no Temple exists to use the knowledge — but the educational, financial, and liturgical arrangements of observant life would visibly reorganize within a generation.
% FOUNDING_PROBLEM: The Temple's destruction created a gap between a commandment system that presupposed sacrificial practice and a world in which that practice was impossible; the arrangement was built to keep the practical knowledge alive until practice becomes possible again.
% FOUNDING_PROBLEM_CORROBORATION: The academies attest the problem is live (restoration certain, preparation obligatory) but they are the arrangement's benefiting and administering parties, so their attestation is not independent. Outside the beneficiary set: historical-philological scholarship corroborates the factual core — substantial procedural content was preserved through the study practice and would plausibly have been lost without living transmission — while documenting that other content (red heifer preparation, exact Temple measurements, priestly genealogies) was lost or remains contested despite the practice. No source outside the benefiting parties attests that the founding problem remains live in its original form; the liveness claim rests on the restoration-certainty premise, which only the tradition itself holds.
narrative_ontology:disappearance_verdict(sacrifice_commandment__archive_maintenance, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_commandment__archive_maintenance, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_commandment__archive_maintenance, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sacrifice_commandment__archive_maintenance, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_commandment__archive_maintenance, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_commandment__archive_maintenance_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sacrifice_commandment__archive_maintenance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sacrifice_commandment__archive_maintenance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.55: the present generation supplies real labor and funds while the declared collector is contingent and voiceless, but the costs are assumed within a committed framework the participants largely endorse and the archive's preservation value is historically documented. Suppression 0.48: institutional and doctrinal rather than physical — curriculum mandates, communal stigma, and the mesorah doctrine declaring written codification insufficient foreclose the cheap alternative, while exit via leaving observant life remains open at identity cost. Suppression is authored as a raw structural property of the arrangement; the engine, not the story, scales extractiveness by directionality and scope, and no such scaling applies to suppression. Theater 0.35: the technical archive is genuinely transmitted (the Talmudic sacrificial orders are studied with procedural precision), but a growing share of engagement is liturgical recitation and piety display that preserves little operational knowledge. The three series share one time grid — points 0–60 in normalized units, 0 marking the post-destruction consolidation of the oral archive (roughly the Tannaitic era) and 60 the contemporary era, each unit a stylized equal step rather than a calendar year. The enforcement ratchet (suppression_requirement 0.35 → 0.48) tracks the arrangement's history: when living memory of the Temple service made study's necessity self-evident, little enforcement was needed; as written codification accumulated and the restoration receded, curriculum mandates and mesorah doctrine had to carry what self-evidence no longer did. coordination_type is declared as information_standard because this reading's own claim is that the arrangement maintains a shared procedural corpus; the type's low default floor is kept (no override) so living-maintenance costs are not pre-absorbed — whether those costs are coordination or extraction is exactly what the written_sufficiency_uncertainty omega tests.
 *
 * PERSPECTIVAL GAP:
 *   The student seat and the administrator seat compute different types from the same structure. From the academies, the arrangement is a duty faithfully administered across centuries — the archive is real, the transmission works, the justification coherent. From the identity-locked student seat, the same arrangement is years of masterful study of the inapplicable, with communal standing as the only present yield. The future-generation seat cannot compute at all — it holds the declared benefit without present agency. The engine derives these divergences from the declared roles, power, and exit options; the divergence is the finding, not an artifact.
 *
 * DIRECTIONALITY LOGIC:
 *   Students and supporting communities sit near the target end: they supply the labor and funds and cannot collect the declared benefit; students additionally carry identity lock, which the derivation treats as amplifying their target position relative to mobile payers. The academies derive a low directionality from their declared beneficiary role, but an override to 0.25 is authored because they are also the administering seat — the gain_flow capturer — collecting funding, authority, and continuity in the present; an administrator-who-collects sits higher on the target axis than a passive beneficiary, though far below the paying seats. The future generation is a beneficiary with no present agency: its low directionality is real but its benefit is contingent on a restoration event no present actor controls. The restoration movements collect legitimacy and donations from the preparation premise — beneficiaries of the frame rather than of the archive's eventual use.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim keeps two facts in view that single-type labels would lose. Reading the arrangement as pure extraction would erase the genuine archive: technical content demonstrably survived two millennia of practice-impossibility, a collective outcome no market or individual incentive produces. Reading it as pure coordination would erase the asymmetry: the participants who pay are not the participants who collect, the collector is contingent and voiceless, and the administering seat captures the present gains. The contested founding-problem status (live for the tradition, moot outside it) blocks a mandatrophy-resolved declaration: the mandate has not so much outlived its function as its function has become undecidable from inside. The enforcement ratchet in the suppression series is the diagnostic: a coordination arrangement still doing its work would need less enforcement over time, not more — the rising requirement is the signature of a structure increasingly held by its administrators rather than by its self-evidence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    written_sufficiency_uncertainty,
    'Does continuous living study preserve technical content that written codification cannot — or do the comprehensive written codes and modern digital archives already carry everything the living transmission adds?',
    'Comparative reconstruction trials: teams relying on written codes alone versus academies with living transmission reconstruct sacrificial procedure under test conditions; philological audit of what the written corpus demonstrably lacks (tacit procedure, error-correction practice, argumentative context).',
    'If written transmission suffices, the archive rationale for the living-study obligation collapses and the arrangement drifts toward performance (rising theater) or dissolves into the study_as_performance sibling; if not, the intergenerational preservation function is load-bearing and the present costs purchase something real.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(written_sufficiency_uncertainty, empirical, 'Whether living transmission adds preservation value over written codification.').

omega_variable(
    restoration_conditionality,
    'Will the restoration event that redeems the archive''s value occur — and does the arrangement''s justification survive two millennia of non-occurrence so far?',
    'Not resolvable by data: within the tradition''s frame the restoration is certain (theological premise); outside it the event is indeterminate. Resolution would require the tradition''s frame to shift or the event to occur.',
    'If the restoration never occurs, the present generation''s costs purchased nothing and the arrangement trends toward extraction maintained by institutional inertia; if it occurs, the archive redeems the accumulated costs and the deferred-utility structure is vindicated retroactively.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(restoration_conditionality, conceptual, 'The contingency of the entire benefit stream on an indeterminate future event.').

omega_variable(
    operative_beneficiary_capture,
    'Is the operative beneficiary the future restoration generation (as declared) or the administering academies (who demonstrably collect funding, authority, and continuity in the present)?',
    'Follow the flows rather than the doctrine: budget allocation, career structures, curriculum control, and who would lose standing if the study obligation were redirected tomorrow.',
    'If the academies are the operative capturers, the declared future-benefit functions as justification for present institutional gain and the arrangement sits closer to the extraction end; if the future generation is the genuine residual claimant, the deferred asymmetric structure holds as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operative_beneficiary_capture, empirical, 'Declared versus operative beneficiary of the study arrangement.').

omega_variable(
    identity_lock_contribution,
    'How much of the present generation''s persistence in sacrifice study is sustained by scholarly-identity fusion rather than endorsement of the archive rationale — and is the measured hold therefore structural (institutional, doctrinal) or internalized (self-concept)?',
    'Post-exit trajectory: students who leave the academy track — do they reassess the obligation''s force or carry it with them? Compare against students who remain for explicitly archive-rationale reasons.',
    'If identity fusion carries much of the persistence, the arrangement is more robust to institutional weakening than enforcement metrics suggest, and part of the hold travels with the student after exit — the structural measure understates it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_contribution, empirical, 'Structural versus internalized share of the arrangement''s grip on present students.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_commandment__archive_maintenance, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_commandment__archive_maintenance, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(sacr_tr_t0, observed).
narrative_ontology:measurement(sacr_tr_t10, sacrifice_commandment__archive_maintenance, theater_ratio, 10, 0.18).
narrative_ontology:measurement_basis(sacr_tr_t10, observed).
narrative_ontology:measurement(sacr_tr_t20, sacrifice_commandment__archive_maintenance, theater_ratio, 20, 0.21).
narrative_ontology:measurement_basis(sacr_tr_t20, observed).
narrative_ontology:measurement(sacr_tr_t30, sacrifice_commandment__archive_maintenance, theater_ratio, 30, 0.25).
narrative_ontology:measurement_basis(sacr_tr_t30, observed).
narrative_ontology:measurement(sacr_tr_t40, sacrifice_commandment__archive_maintenance, theater_ratio, 40, 0.29).
narrative_ontology:measurement_basis(sacr_tr_t40, observed).
narrative_ontology:measurement(sacr_tr_t50, sacrifice_commandment__archive_maintenance, theater_ratio, 50, 0.32).
narrative_ontology:measurement_basis(sacr_tr_t50, observed).
narrative_ontology:measurement(sacr_tr_t60, sacrifice_commandment__archive_maintenance, theater_ratio, 60, 0.35).
narrative_ontology:measurement_basis(sacr_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_commandment__archive_maintenance, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(sacr_be_t0, observed).
narrative_ontology:measurement(sacr_be_t10, sacrifice_commandment__archive_maintenance, base_extractiveness, 10, 0.47).
narrative_ontology:measurement_basis(sacr_be_t10, observed).
narrative_ontology:measurement(sacr_be_t20, sacrifice_commandment__archive_maintenance, base_extractiveness, 20, 0.49).
narrative_ontology:measurement_basis(sacr_be_t20, observed).
narrative_ontology:measurement(sacr_be_t30, sacrifice_commandment__archive_maintenance, base_extractiveness, 30, 0.51).
narrative_ontology:measurement_basis(sacr_be_t30, observed).
narrative_ontology:measurement(sacr_be_t40, sacrifice_commandment__archive_maintenance, base_extractiveness, 40, 0.53).
narrative_ontology:measurement_basis(sacr_be_t40, observed).
narrative_ontology:measurement(sacr_be_t50, sacrifice_commandment__archive_maintenance, base_extractiveness, 50, 0.54).
narrative_ontology:measurement_basis(sacr_be_t50, observed).
narrative_ontology:measurement(sacr_be_t60, sacrifice_commandment__archive_maintenance, base_extractiveness, 60, 0.55).
narrative_ontology:measurement_basis(sacr_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_commandment__archive_maintenance, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(sacr_su_t0, observed).
narrative_ontology:measurement(sacr_su_t10, sacrifice_commandment__archive_maintenance, suppression_requirement, 10, 0.37).
narrative_ontology:measurement_basis(sacr_su_t10, observed).
narrative_ontology:measurement(sacr_su_t20, sacrifice_commandment__archive_maintenance, suppression_requirement, 20, 0.39).
narrative_ontology:measurement_basis(sacr_su_t20, observed).
narrative_ontology:measurement(sacr_su_t30, sacrifice_commandment__archive_maintenance, suppression_requirement, 30, 0.41).
narrative_ontology:measurement_basis(sacr_su_t30, observed).
narrative_ontology:measurement(sacr_su_t40, sacrifice_commandment__archive_maintenance, suppression_requirement, 40, 0.43).
narrative_ontology:measurement_basis(sacr_su_t40, observed).
narrative_ontology:measurement(sacr_su_t50, sacrifice_commandment__archive_maintenance, suppression_requirement, 50, 0.46).
narrative_ontology:measurement_basis(sacr_su_t50, observed).
narrative_ontology:measurement(sacr_su_t60, sacrifice_commandment__archive_maintenance, suppression_requirement, 60, 0.48).
narrative_ontology:measurement_basis(sacr_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_commandment__archive_maintenance, information_standard).
narrative_ontology:affects_constraint(sacrifice_commandment__archive_maintenance, sacrifice_commandment__study_as_performance).
narrative_ontology:affects_constraint(sacrifice_commandment__archive_maintenance, sacrifice_commandment__performance_only).

% DUAL FORMULATION NOTE:
% The colloquial label 'the sacrifice commandment after the Temple's destruction' covers three structurally distinct claims about what the obligation is now, decomposed per the ε-invariance principle: sacrifice_commandment__performance_only (the commandment is suspended, not fulfilled — the study duty loses commandment force and the cost claim dissolves toward voluntary scholarship), sacrifice_commandment__study_as_performance (present study IS the commandment's exercise — low ε, near-pure devotional coordination), and this story, sacrifice_commandment__archive_maintenance (a preservation duty with deferred, contingent utility — moderate ε). The upstream member in empirical confidence is performance_only's factual core; archive_maintenance builds its justification on top of it, and study_as_performance contests the deferral itself. Each file links the others via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sacrifice_commandment__archive_maintenance, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
