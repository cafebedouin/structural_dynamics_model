% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_continuity__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_continuity__performance_only, []).

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
 *   constraint_id: sacrifice_obligation_continuity__performance_only
 *   human_readable: Sacrifice Obligation as Physical-Performance-Only (Unfulfillable Pending Temple Restoration)
 *   domain: religious_law/ritual_studies/textual_tradition
 *
 * SUMMARY:
 *   This is one reading among four of a single contested kernel: whether the
 *   sacrificial commandments, unperformable since the Temple's destruction,
 *   remain binding, and if so, how (or whether) study of their laws relates
 *   to fulfillment. Under the performance_only reading generated here,
 *   physical enactment is the sole mode of fulfillment; study — however
 *   encouraged, however liturgically embedded — is explicitly preparatory,
 *   not satisfying. This produces a structural feature the sibling readings
 *   do not share: every adherent alive since the Temple's destruction sits in
 *   permanent, unresolvable non-fulfillment of a live commandment. The
 *   sibling readings (study_as_performance, messianic_suspension,
 *   archival_preservation) resolve this tension differently and are NOT
 *   represented in this file — they are separate constraints in the same
 *   family, linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - current_generation_adherents: primary target (powerless/trapped) — bears the psychological and devotional cost of permanent non-fulfillment
 *   - rabbinic_interpretive_authorities: agenda-setter (institutional/arbitrage) — adjudicates and administers the reading
 *   - messianic_restoration_institutions: beneficiary (organized/arbitrage) — derives institutional purpose from unfulfillable-but-live obligation
 *   - study_as_performance_communities: excluded voice (organized/constrained) — holds the resolving alternative but lacks authority within this reading's institutions
 *   - textual_tradition_scholars: analytical observer — traces the kernel's readings across history
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_continuity__performance_only, 0.72).
domain_priors:suppression_score(sacrifice_obligation_continuity__performance_only, 0.58).
domain_priors:theater_ratio(sacrifice_obligation_continuity__performance_only, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, extractiveness, 0.72).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__performance_only, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_continuity__performance_only, tangled_rope).
narrative_ontology:human_readable(sacrifice_obligation_continuity__performance_only, "Sacrifice Obligation as Physical-Performance-Only (Unfulfillable Pending Temple Restoration)").
narrative_ontology:topic_domain(sacrifice_obligation_continuity__performance_only, "religious_law/ritual_studies/textual_tradition").

domain_priors:requires_active_enforcement(sacrifice_obligation_continuity__performance_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_continuity__performance_only, '79354983-8795-454e-9ebf-9e43b1af38e5').
narrative_ontology:cs_kernel_codification('79354983-8795-454e-9ebf-9e43b1af38e5', fixed_text).
narrative_ontology:cs_authority_grounding('79354983-8795-454e-9ebf-9e43b1af38e5', lineage).
narrative_ontology:cs_interpretation_layer_present('79354983-8795-454e-9ebf-9e43b1af38e5').
narrative_ontology:cs_reading_relation('79354983-8795-454e-9ebf-9e43b1af38e5', sacrifice_obligation_continuity__study_as_performance, forecloses).
narrative_ontology:cs_reading_relation('79354983-8795-454e-9ebf-9e43b1af38e5', sacrifice_obligation_continuity__messianic_suspension, coexists_with).
narrative_ontology:cs_reading_relation('79354983-8795-454e-9ebf-9e43b1af38e5', sacrifice_obligation_continuity__archival_preservation, forecloses).
narrative_ontology:cs_axiom('79354983-8795-454e-9ebf-9e43b1af38e5', foundational, physical_enactment_is_sole_fulfillment_mode).
narrative_ontology:cs_axiom_status(physical_enactment_is_sole_fulfillment_mode, holdable).
narrative_ontology:cs_axiom_grounding('79354983-8795-454e-9ebf-9e43b1af38e5', physical_enactment_is_sole_fulfillment_mode, deontological).
narrative_ontology:cs_axiom('79354983-8795-454e-9ebf-9e43b1af38e5', foundational, study_constitutes_preparation_not_satisfaction).
narrative_ontology:cs_axiom_status(study_constitutes_preparation_not_satisfaction, holdable).
narrative_ontology:cs_axiom_grounding('79354983-8795-454e-9ebf-9e43b1af38e5', study_constitutes_preparation_not_satisfaction, conventional).
narrative_ontology:cs_reference_frame('79354983-8795-454e-9ebf-9e43b1af38e5', temple_era_sacrificial_praxis).
narrative_ontology:cs_drift_state('79354983-8795-454e-9ebf-9e43b1af38e5', two_millennia_post_destruction, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('79354983-8795-454e-9ebf-9e43b1af38e5', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_continuity__performance_only, sacrifice_obligation_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__performance_only, rabbinic_interpretive_authorities).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__performance_only, messianic_restoration_institutions).
narrative_ontology:constraint_victim(sacrifice_obligation_continuity__performance_only, current_generation_adherents).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__performance_only, temple_centrality_doctrine).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__performance_only, commandment_immutability_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Are bound by a commandment counted among the tradition's core obligations, yet have no physical Temple, altar, priesthood in functioning order, or sacrificial animals recognized as valid — so the obligation cannot be discharged by any act available to them. Study of the sacrificial laws is affirmed as virtuous and even as partially substitutive in liturgy, but under this reading it is explicitly NOT fulfillment. They carry the weight of an unfulfilled commandment for the duration of their lives with no exit: leaving the framework means abandoning the broader commitment structure their identity is built on; remaining means permanent non-fulfillment of a named obligation.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, current_generation_adherents, payer,
    powerless, biographical, trapped, global).

% Adjudicate which reading of the sacrifice obligation is authoritative, teach that physical performance is required and study is preparatory only, and administer the liturgical and educational structures built around 'readiness' for restoration. Their institutional role, curriculum authority, and communal standing are constituted by being the interpreters who hold the line on this reading; they do not personally bear the unfulfillable obligation's psychological cost in the way lay adherents do, and they retain interpretive latitude that ordinary adherents lack.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, rabbinic_interpretive_authorities, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Organizations, seminaries, and movements oriented toward active preparation for Temple restoration (priestly genealogy registries, ritual-object reconstruction, red heifer breeding programs) derive their institutional purpose, funding, and communal urgency directly from the performance-only reading: if study or suspension satisfied the obligation, their preparatory mission would lose its distinguishing rationale. They benefit from the obligation remaining unfulfillable-but-live.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, messianic_restoration_institutions, beneficiary,
    organized, civilizational, arbitrage, global).

% Communities and scholars who hold that textual engagement with the sacrificial laws itself constitutes fulfillment are structurally at odds with this reading's core claim and are treated within performance-only communities as offering comfort rather than correct doctrine. Their view would relieve the psychological burden this reading imposes, but they are not the deciding authority within performance-only institutions and their position is marginalized in performance-only liturgical and educational settings.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, study_as_performance_communities, excluded,
    organized, generational, constrained, global).

% Historians and comparative-religion scholars examine how the four readings of the sacrifice kernel emerged, diverged, and compete across communities and eras, without being bound by any of them personally.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__performance_only, textual_tradition_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_obligation_continuity__performance_only, messianic_restoration_institutions).
narrative_ontology:fixing_cost_class(sacrifice_obligation_continuity__performance_only, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a unified, temple-centered ritual system as the tradition's normative center of gravity, coordinating communal memory, priestly lineage claims, and eschatological expectation around a single restorable institution rather than allowing the obligation to dissolve or be redefined into something else entirely.
% TRANSFER_FUNCTION: Moves psychological and devotional labor from current-generation adherents (who carry permanent non-fulfillment and its associated guilt/anxiety) to the interpretive and preparatory institutions that administer the reading and derive purpose, funding, and authority from its persistence.
% ABSENT_VOICES: Adherents who find the permanent-unfulfillment framing psychologically corrosive rarely have standing to reclassify the obligation themselves — that authority sits with rabbinic interpretive bodies. Study-as-performance communities who would resolve the guilt through a different reading are present in the broader tradition but excluded from authority within performance-only institutions specifically.
% DISAPPEARANCE_RATIONALE: If the performance-only reading vanished, current-generation adherents would either (a) adopt study-as-performance and experience the obligation as actively fulfilled, ending an entire category of ritual anxiety, or (b) adopt messianic_suspension and experience the obligation as paused rather than perpetually failed. Restoration-preparation institutions would lose a distinguishing rationale for urgency. Rabbinic authorities dispute whether this would represent doctrinal loss (abandoning a real, if painful, requirement) or relief (correcting an unnecessarily severe reading) — hence contested rather than a clean verdict either way.
% FOUNDING_PROBLEM: After the Temple's destruction, the tradition needed to explain how a body of commandments centered on animal sacrifice could remain binding and central to religious identity when the physical infrastructure for performing them no longer existed, without simply declaring the commandments abrogated.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic authorities within the performance-only tradition attest the problem remains live — restoration is a genuine future prospect and the obligation's physical-performance character preserves proper reverence for it. Scholars of comparative religious law and adherents in study_as_performance and messianic_suspension communities (outside the performance-only benefiting institutions) attest that the founding problem has effectively been resolved through alternative doctrinal moves for two millennia, and that performance-only persistence serves institutional and psychological functions distinct from the original problem of ritual continuity.
narrative_ontology:disappearance_verdict(sacrifice_obligation_continuity__performance_only, contested).
narrative_ontology:founding_problem_status(sacrifice_obligation_continuity__performance_only, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_continuity__performance_only, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sacrifice_obligation_continuity__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_continuity__performance_only, 0.72, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_continuity__performance_only_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sacrifice_obligation_continuity__performance_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sacrifice_obligation_continuity__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.72) because the reading imposes a real, felt cost — the guilt or spiritual deficit of unfulfilled commandment — on every adherent for the duration of exile, with no remedy available to them personally; the cost is structural, not incidental. Suppression is moderate-high (0.58) because the reading is actively taught and defended against the study_as_performance alternative that would relieve the very burden it imposes — suppression here is enforcement of ONE reading against a live doctrinal competitor, not enforcement of a physical act. Theater ratio is moderate (0.45) and rising: as centuries pass without restoration, an increasing share of 'preparation' activity (registries, liturgical rehearsal, restoration advocacy) functions as institutional self-perpetuation rather than genuine operational readiness. Accessibility collapse is moderate (0.40), not high, because the sibling readings remain visibly available within the broader tradition — an adherent CAN move to study_as_performance or messianic_suspension communities, which is precisely why suppression (defending against those alternatives) is a needed and active mechanism rather than redundant.
 *
 * PERSPECTIVAL GAP:
 *   From the rabbinic authority seat, this reading is a faithful preservation of the commandment's true character — a correct, if demanding, reading that protects the seriousness of the obligation against dilution. From the adherent seat, the identical structure operates as a permanent, unrelievable debt with no path to discharge, a cost imposed for as long as restoration is deferred. The engine computes these as structurally different experiences of the same constraint from the declared power/exit data; the claim (tangled_rope) does not resolve which seat is 'right' — it names that both a coordination function (preserving temple-centered communal identity) and asymmetric extraction (guilt without remedy, concentrated on the powerless) are simultaneously present.
 *
 * DIRECTIONALITY LOGIC:
 *   Current-generation adherents are declared victims: they bear an obligation they structurally cannot discharge, with no exit that does not require leaving the broader identity-constituting framework — this pushes their derived directionality toward the target end. Rabbinic interpretive authorities and restoration-preparation institutions are declared beneficiaries: their institutional role, funding, and communal standing are constituted by the reading's persistence, and they hold arbitrage-grade exit (they can and do adjust doctrine, unlike lay adherents who must live under whatever is taught). No override was needed — the beneficiary/victim declarations map directly onto the actual institutional asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how to keep temple-centered law binding after the Temple's fall — is contested as live or dead precisely along the lines of this kernel's readings. Classifying this specific reading as tangled_rope (not snare) preserves the genuine coordination function it performs (maintaining a coherent, temple-oriented communal identity across two millennia) while still registering the asymmetric cost it imposes on adherents who have no institutional power to revise the reading themselves. Calling it a pure snare would miss the real coordination achieved; calling it a pure rope would erase the documented psychological cost carried disproportionately by the powerless seat.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_authority,
    'Which of the four readings of the sacrifice_obligation_continuity kernel (performance_only, study_as_performance, messianic_suspension, archival_preservation) is authoritative for a given adherent, and who has standing to decide?',
    'No empirical resolution mechanism exists — this is a live doctrinal dispute internal to the tradition, adjudicated by denominational and communal authority structures that themselves differ across the tradition''s branches.',
    'If study_as_performance is adopted instead, current_generation_adherents exit the victim set entirely — the obligation becomes actively fulfilled through textual engagement, and extractiveness collapses toward the rope end. If messianic_suspension is adopted, adherents move from ''permanently failing'' to ''validly waiting,'' which is a different and much lower-cost structural position. This story generates ONLY the performance_only reading; the siblings are separate constraint files.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_authority, conceptual, 'This story instantiates one reading of a four-way contested kernel; the reading choice is the primary structural fork.').

omega_variable(
    guilt_measurement_validity,
    'Is the psychological cost attributed to ''permanent non-fulfillment'' actually experienced as guilt/deficit by most adherents, or is it a scholarly inference about doctrinal structure that overstates lived experience?',
    'Ethnographic or survey research among communities that explicitly hold the performance_only reading, comparing self-reported religious distress to communities holding study_as_performance or messianic_suspension.',
    'If lived distress is low despite doctrinal severity (e.g., because liturgical practice provides adequate psychological compensation even while denying it constitutes fulfillment), the authored extractiveness score may overstate the constraint''s actual cost to adherents.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(guilt_measurement_validity, empirical, 'Whether authored extractiveness reflects measured or inferred adherent experience.').

omega_variable(
    restoration_institution_sincerity,
    'Do messianic_restoration_institutions genuinely believe restoration is imminent/achievable, or does the institutional benefit from perpetual ''readiness'' create an incentive to indefinitely defer actual restoration efforts?',
    'Track record analysis: do restoration-preparation institutions show measurable progress toward stated restoration prerequisites over time, or does ''readiness'' persist as a stable institutional state across generations without approaching completion?',
    'If institutions show no progress and no urgency to complete stated prerequisites, the beneficiary relationship is more extractive (indefinite fundraising/purpose-generation from an undischargeable future promise) than a good-faith preparatory reading would suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restoration_institution_sincerity, empirical, 'Whether restoration-preparation institutions have incentive misalignment favoring perpetual non-completion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_continuity__performance_only, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_continuity__performance_only, theater_ratio, 0, 0.3).
narrative_ontology:measurement(sacr_tr_t20, sacrifice_obligation_continuity__performance_only, theater_ratio, 20, 0.34).
narrative_ontology:measurement(sacr_tr_t40, sacrifice_obligation_continuity__performance_only, theater_ratio, 40, 0.37).
narrative_ontology:measurement(sacr_tr_t60, sacrifice_obligation_continuity__performance_only, theater_ratio, 60, 0.4).
narrative_ontology:measurement(sacr_tr_t80, sacrifice_obligation_continuity__performance_only, theater_ratio, 80, 0.43).
narrative_ontology:measurement(sacr_tr_t100, sacrifice_obligation_continuity__performance_only, theater_ratio, 100, 0.45).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_continuity__performance_only, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(sacr_be_t20, sacrifice_obligation_continuity__performance_only, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(sacr_be_t40, sacrifice_obligation_continuity__performance_only, base_extractiveness, 40, 0.64).
narrative_ontology:measurement(sacr_be_t60, sacrifice_obligation_continuity__performance_only, base_extractiveness, 60, 0.67).
narrative_ontology:measurement(sacr_be_t80, sacrifice_obligation_continuity__performance_only, base_extractiveness, 80, 0.7).
narrative_ontology:measurement(sacr_be_t100, sacrifice_obligation_continuity__performance_only, base_extractiveness, 100, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_obligation_continuity__performance_only, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(sacr_su_t20, sacrifice_obligation_continuity__performance_only, suppression_requirement, 20, 0.49).
narrative_ontology:measurement(sacr_su_t40, sacrifice_obligation_continuity__performance_only, suppression_requirement, 40, 0.52).
narrative_ontology:measurement(sacr_su_t60, sacrifice_obligation_continuity__performance_only, suppression_requirement, 60, 0.55).
narrative_ontology:measurement(sacr_su_t80, sacrifice_obligation_continuity__performance_only, suppression_requirement, 80, 0.57).
narrative_ontology:measurement(sacr_su_t100, sacrifice_obligation_continuity__performance_only, suppression_requirement, 100, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_continuity__performance_only, identity_coordination).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__performance_only, sacrifice_obligation_continuity__study_as_performance).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__performance_only, sacrifice_obligation_continuity__messianic_suspension).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__performance_only, sacrifice_obligation_continuity__archival_preservation).

% DUAL FORMULATION NOTE:
% This file is one of four constraints decomposing the natural-language label 'the sacrifice obligation continuity question.' Each of the four readings (performance_only, study_as_performance, messianic_suspension, archival_preservation) is authored as a separate constraint with its own epsilon, beneficiary/victim structure, and claimed type, per the ε-invariance principle: the readings produce structurally different victim sets and extraction profiles and cannot be averaged into one constraint. This file (performance_only) is distinguished by placing current-generation adherents in the victim set with high extractiveness (guilt without remedy) — the sharpest of the four readings in terms of adherent cost. All four should be linked bidirectionally via affects_constraints once generated.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
