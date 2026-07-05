% ============================================================================
% CONSTRAINT STORY: simultaneous_veneration__domain_partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_simultaneous_veneration__domain_partition_reading, []).

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
 *   constraint_id: simultaneous_veneration__domain_partition_reading
 *   human_readable: Domain-Partition Reading of Kami-Buddha Simultaneous Veneration (shinbutsu-shūgō)
 *   domain: religious_studies/comparative_religion/japanese_history
 *
 * SUMMARY:
 *   This story instantiates the domain-partition reading of shinbutsu-shūgō
 *   (kami-buddha amalgamation): the claim that kami and buddhas are
 *   functionally distinct entities governing separate domains — kami for
 *   this-worldly prosperity and protection, buddhas for afterlife salvation —
 *   such that simultaneous veneration at combined shrine-temple complexes
 *   reflects domain-appropriate specialization rather than confusion or
 *   contradiction. This is one of three contested readings of the same
 *   historical practice; the other two (ontological fusion via honji-suijaku,
 *   and pragmatic incoherence sustained by lack of enforcement) are separate
 *   constraints with their own ε values, per the ε-invariance principle. This
 *   story is confined to the domain-partition reading only.
 *
 * KEY AGENTS:
 *   - lay_householders: primary beneficiaries of a working functional division that lets them draw on two ritual technologies without doctrinal cost
 *   - shrine_temple_complexes: institutional coordinators who administer the joint ritual calendar (jingū-ji)
 *   - local_ritual_specialists: priests and monks whose distinct domains of authority are preserved by the partition
 *   - doctrinal_purists: excluded minority voices who reject functional coexistence, rarely visible in ordinary practice
 *   - comparative_religion_scholars: analytical observers weighing this reading against its siblings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(simultaneous_veneration__domain_partition_reading, 0.12).
domain_priors:suppression_score(simultaneous_veneration__domain_partition_reading, 0.08).
domain_priors:theater_ratio(simultaneous_veneration__domain_partition_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(simultaneous_veneration__domain_partition_reading, rope).
narrative_ontology:human_readable(simultaneous_veneration__domain_partition_reading, "Domain-Partition Reading of Kami-Buddha Simultaneous Veneration (shinbutsu-shūgō)").
narrative_ontology:topic_domain(simultaneous_veneration__domain_partition_reading, "religious_studies/comparative_religion/japanese_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(simultaneous_veneration__domain_partition_reading, '1abd94da-0906-4170-b637-be3522cf00e3').
narrative_ontology:cs_kernel_codification('1abd94da-0906-4170-b637-be3522cf00e3', distributed).
narrative_ontology:cs_authority_grounding('1abd94da-0906-4170-b637-be3522cf00e3', practice).
narrative_ontology:cs_interpretation_layer_present('1abd94da-0906-4170-b637-be3522cf00e3').
narrative_ontology:cs_reading_relation('1abd94da-0906-4170-b637-be3522cf00e3', simultaneous_veneration__ontological_fusion_reading, coexists_with).
narrative_ontology:cs_reading_relation('1abd94da-0906-4170-b637-be3522cf00e3', simultaneous_veneration__pragmatic_incoherence_reading, influences).
narrative_ontology:cs_axiom('1abd94da-0906-4170-b637-be3522cf00e3', foundational, kami_and_buddhas_functionally_distinct).
narrative_ontology:cs_axiom_status(kami_and_buddhas_functionally_distinct, holdable).
narrative_ontology:cs_axiom_grounding('1abd94da-0906-4170-b637-be3522cf00e3', kami_and_buddhas_functionally_distinct, conventional).
narrative_ontology:cs_axiom('1abd94da-0906-4170-b637-be3522cf00e3', secondary, domain_specialization_resolves_practice_without_metaphysical_commitment).
narrative_ontology:cs_axiom_status(domain_specialization_resolves_practice_without_metaphysical_commitment, holdable).
narrative_ontology:cs_axiom_grounding('1abd94da-0906-4170-b637-be3522cf00e3', domain_specialization_resolves_practice_without_metaphysical_commitment, instrumental).
narrative_ontology:cs_reference_frame('1abd94da-0906-4170-b637-be3522cf00e3', premodern_shrine_temple_complex_practice).
narrative_ontology:cs_drift_state('1abd94da-0906-4170-b637-be3522cf00e3', post_meiji_shinbutsu_bunri, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('1abd94da-0906-4170-b637-be3522cf00e3', '').
narrative_ontology:cs_kernel_id(simultaneous_veneration__domain_partition_reading, simultaneous_veneration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(simultaneous_veneration__domain_partition_reading, lay_householders).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__domain_partition_reading, shrine_temple_complexes).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__domain_partition_reading, local_ritual_specialists).
narrative_ontology:constraint_vindicates(simultaneous_veneration__domain_partition_reading, functional_domain_specialization_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Petition kami at shrines for harvest, health, and fortune in this life while relying on buddhas and temple rites for funerary care and afterlife salvation. They move freely between shrine and temple as the occasion calls for it, treating the two as complementary specialists rather than competitors, with no felt contradiction and no cost extracted by holding both practices.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, lay_householders, beneficiary,
    moderate, biographical, mobile, regional).

% Jointly administer combined shrine-temple precincts (jingū-ji), coordinating ritual calendars so kami rites and buddhist rites occupy distinct functional slots. They benefit from a stable division of ritual labor that lets both institutions draw patronage without doctrinal conflict; their exit from the arrangement would mean forfeiting a working coordination structure that costs little to maintain.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, shrine_temple_complexes, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(simultaneous_veneration__domain_partition_reading, shrine_temple_complexes, agenda_setter).

% Shrine priests and temple monks each retain distinct ritual authority over their respective domain (fertility/protection versus death/salvation), so neither specialist's role is displaced by the other's presence. They can and do practice in parallel without professional rivalry over the same functional territory.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, local_ritual_specialists, beneficiary,
    moderate, biographical, mobile, local).

% Minority voices within both Shinto and Buddhist lineages who hold that the traditions should not be practiced as a functional division of labor at all, but their objection is rarely represented in the ordinary practice of shrine-temple complexes, which simply proceeds on the working assumption of domain specialization.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, doctrinal_purists, excluded,
    powerless, generational, constrained, regional).

% Analyze shinbutsu-shūgō as a case study in religious syncretism, examining whether the domain-partition framing, the ontological-identity framing (honji-suijaku), or the incoherence framing best accounts for premodern practice, without a stake in which reading prevails.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, comparative_religion_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Divides religious labor along a functional seam: kami are addressed for this-worldly, immediate concerns (harvest, health, protection, fortune) while buddhas are addressed for afterlife salvation and death ritual. This lets a single household or community draw on two distinct ritual technologies without either specialist claiming the other's territory.
% TRANSFER_FUNCTION: No systematic transfer of resources from one party to another; patronage (offerings, labor, land grants) flows to whichever institution is functionally appropriate to the occasion — shrine for seasonal/this-worldly rites, temple for funerary/salvation rites — and both institutions receive support for genuinely distinct services rendered.
% ABSENT_VOICES: Doctrinal purists within both traditions who reject functional coexistence are marginal to ordinary practice and rarely surface in the historical record of household or village-level veneration; their objection is preserved mainly in sectarian polemical texts rather than in the practice itself.
% DISAPPEARANCE_RATIONALE: If the domain-partition understanding vanished, households would lose a coherent way to justify visiting both shrine and temple; either one institution would need to claim both domains (unlikely, given genuine functional specialization built up over centuries) or practitioners would face real doctrinal tension requiring choice between traditions — a rearrangement, not a null change, given how thoroughly shrine-temple complexes and ritual calendars were organized around the division.
% FOUNDING_PROBLEM: Early Japanese religious life needed both immediate, this-worldly efficacy (protection, fertility, harvest) and a framework for death and what follows it; neither the indigenous kami cults nor imported Buddhism alone addressed both needs as thoroughly as the combination did, so institutions and practice converged on complementary specialization rather than competition.
% FOUNDING_PROBLEM_CORROBORATION: Practitioners and shrine-temple administrators attest the functional division remained operative through most of the premodern period. Historians of the Meiji shinbutsu bunri (forced separation) policy, writing from outside either tradition's own self-understanding, corroborate that the division was practiced and administratively real enough that its forcible dismantling in 1868 required substantial state intervention — evidence the partition was a working arrangement, not merely a retrospective rationalization.
narrative_ontology:disappearance_verdict(simultaneous_veneration__domain_partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(simultaneous_veneration__domain_partition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(simultaneous_veneration__domain_partition_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(simultaneous_veneration__domain_partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(simultaneous_veneration__domain_partition_reading, 0.12, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(simultaneous_veneration__domain_partition_reading_tests).
:- end_tests(simultaneous_veneration__domain_partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.12) and stays low across the interval because the domain-partition reading describes a genuine complementary specialization: two institutions serving two distinct needs, neither capturing rents from the other's function. Suppression is low (0.08) because nothing coerces a household into visiting both shrine and temple — the practice is adopted because it addresses two real, distinct needs. Theater ratio is low-to-moderate (0.15 at interval end) reflecting some ritual elaboration over time without indicating that the coordination function was hollowed out. Accessibility collapse is moderate (0.35) — households could in principle have chosen a single-tradition path, but the two-domain solution became broadly normative once shrine-temple complexes institutionalized it, so alternatives narrowed without fully vanishing. Resistance is low (0.15), consistent with a rope: some doctrinal objection exists but does not rise to sustained active resistance against the arrangement itself.
 *
 * PERSPECTIVAL GAP:
 *   Lay householders and shrine-temple administrators should compute similarly under this reading — both experience the arrangement as functional complementarity rather than as extraction or coercion, unlike constraints where agenda-setters and payers diverge sharply. The engine's computation of low extraction and low suppression across all named seats is itself the signature that distinguishes this reading structurally from the pragmatic-incoherence sibling, where the same historical practice would show elevated suppression from unresolved contradiction rather than the ease shown here.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (lay householders, shrine-temple complexes, ritual specialists) are declared without corresponding victims because, under this reading, no party is structurally extracted from — the two institutions serve genuinely distinct functions and patronage flows to whichever is functionally appropriate. This is what distinguishes the domain-partition reading from a tangled-rope or snare reading of the same practice: there is no asymmetric extraction to declare, only complementary coordination. Doctrinal purists are excluded rather than victimized — they object on principle but are not structurally paying a cost through the arrangement.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (need for both this-worldly efficacy and afterlife salvation, met by no single tradition alone) is marked contested rather than dead: the practice was forcibly dismantled by Meiji-era shinbutsu bunri policy in 1868, not abandoned because it stopped functioning. That the state needed to intervene to separate the traditions is itself evidence the coordination function remained live up to that point — this reading resists a mandatrophy verdict because there is no diffuse-cost, no-longer-serving-anyone structure to detect; the arrangement was ended by external fiat, not internal exhaustion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    domain_partition_vs_ontological_fusion_locus,
    'Did premodern practitioners themselves hold the domain-partition view (functionally distinct entities), the ontological-fusion view (honji-suijaku: kami as local manifestations of buddhas), or did the two framings coexist without practitioners needing to choose between them?',
    'Close reading of premodern doctrinal texts, ritual manuals, and votive inscriptions to determine whether functional-specialization language or identity/manifestation language dominates at different sites and periods, and whether ritual specialists themselves distinguished the two framings.',
    'If the domain-partition framing was the dominant lived understanding, this story''s rope classification is well-grounded as the primary reading of the historical practice. If the ontological-fusion framing (honji-suijaku) was in fact dominant, this story documents a secondary or scholarly reconstruction rather than the practitioners'' own understanding, and the sibling constraint would carry more historical weight.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_partition_vs_ontological_fusion_locus, conceptual, 'Whether domain-partition or ontological-fusion was the practitioners'' actual operative framework.').

omega_variable(
    coherence_vs_convenient_non_resolution,
    'Was the domain-partition reading a genuinely coherent doctrinal position, or a convenient non-resolution that let practitioners avoid confronting tension between traditions, as the pragmatic-incoherence sibling reading claims?',
    'Examine whether historical actors ever needed to resolve edge cases (e.g., can a kami intervene in afterlife matters, can a buddha grant this-worldly fortune) and how such cases were handled — clean domain boundaries support this reading, ad hoc improvisation supports the incoherence reading.',
    'If edge cases were handled with genuine doctrinal reasoning preserving the domain boundary, this reading''s low-suppression, low-extraction profile is well-supported. If edge cases reveal unprincipled improvisation, the pragmatic-incoherence sibling would better characterize the historical practice, and this story''s ease-of-coordination framing may understate real, unresolved tension practitioners lived with.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coherence_vs_convenient_non_resolution, empirical, 'Whether the domain boundary held up under doctrinal pressure or was maintained only by avoiding hard cases.').

omega_variable(
    meiji_separation_as_evidence,
    'Does the forcible Meiji-era separation of kami and buddha worship (shinbutsu bunri, 1868) count as evidence that the pre-Meiji arrangement was a stable, functioning coordination (this reading), or as evidence that the arrangement required a strong external hand to prevent conflict from ever surfacing (favoring the incoherence reading)?',
    'Historical analysis of whether shinbutsu bunri was driven primarily by state ideological consolidation (State Shinto formation) independent of internal religious tension, versus evidence that local tensions or doctrinal disputes were already building before state intervention.',
    'If separation was purely a top-down ideological project unrelated to internal strain, this supports reading the pre-1868 arrangement as a genuinely stable rope. If local evidence shows unresolved tension pre-dating Meiji policy, the incoherence sibling gains support and this story''s low-resistance metric may be understated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meiji_separation_as_evidence, empirical, 'Whether Meiji separation reflects external imposition on a stable system or termination of a long-brewing internal tension.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(simultaneous_veneration__domain_partition_reading, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(simu_tr_t0, simultaneous_veneration__domain_partition_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(simu_tr_t0, observed).
narrative_ontology:measurement(simu_tr_t200, simultaneous_veneration__domain_partition_reading, theater_ratio, 200, 0.1).
narrative_ontology:measurement_basis(simu_tr_t200, observed).
narrative_ontology:measurement(simu_tr_t400, simultaneous_veneration__domain_partition_reading, theater_ratio, 400, 0.11).
narrative_ontology:measurement_basis(simu_tr_t400, observed).
narrative_ontology:measurement(simu_tr_t600, simultaneous_veneration__domain_partition_reading, theater_ratio, 600, 0.12).
narrative_ontology:measurement_basis(simu_tr_t600, observed).
narrative_ontology:measurement(simu_tr_t800, simultaneous_veneration__domain_partition_reading, theater_ratio, 800, 0.13).
narrative_ontology:measurement_basis(simu_tr_t800, observed).
narrative_ontology:measurement(simu_tr_t1000, simultaneous_veneration__domain_partition_reading, theater_ratio, 1000, 0.14).
narrative_ontology:measurement_basis(simu_tr_t1000, observed).
narrative_ontology:measurement(simu_tr_t1200, simultaneous_veneration__domain_partition_reading, theater_ratio, 1200, 0.15).
narrative_ontology:measurement_basis(simu_tr_t1200, observed).

% Extraction over time
narrative_ontology:measurement(simu_be_t0, simultaneous_veneration__domain_partition_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement_basis(simu_be_t0, observed).
narrative_ontology:measurement(simu_be_t200, simultaneous_veneration__domain_partition_reading, base_extractiveness, 200, 0.09).
narrative_ontology:measurement_basis(simu_be_t200, observed).
narrative_ontology:measurement(simu_be_t400, simultaneous_veneration__domain_partition_reading, base_extractiveness, 400, 0.1).
narrative_ontology:measurement_basis(simu_be_t400, observed).
narrative_ontology:measurement(simu_be_t600, simultaneous_veneration__domain_partition_reading, base_extractiveness, 600, 0.1).
narrative_ontology:measurement_basis(simu_be_t600, observed).
narrative_ontology:measurement(simu_be_t800, simultaneous_veneration__domain_partition_reading, base_extractiveness, 800, 0.11).
narrative_ontology:measurement_basis(simu_be_t800, observed).
narrative_ontology:measurement(simu_be_t1000, simultaneous_veneration__domain_partition_reading, base_extractiveness, 1000, 0.12).
narrative_ontology:measurement_basis(simu_be_t1000, observed).
narrative_ontology:measurement(simu_be_t1200, simultaneous_veneration__domain_partition_reading, base_extractiveness, 1200, 0.12).
narrative_ontology:measurement_basis(simu_be_t1200, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(simultaneous_veneration__domain_partition_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(simultaneous_veneration__domain_partition_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(simultaneous_veneration__domain_partition_reading, 0.08).
narrative_ontology:affects_constraint(simultaneous_veneration__domain_partition_reading, ontological_fusion_reading).
narrative_ontology:affects_constraint(simultaneous_veneration__domain_partition_reading, pragmatic_incoherence_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints sharing the simultaneous_veneration kernel. domain_partition_reading (this story) models the practice as functionally specialized coordination (rope, low ε). ontological_fusion_reading models it as a metaphysical-identity claim (honji-suijaku) with distinct structural implications for religious authority. pragmatic_incoherence_reading models it as an unresolved contradiction sustained by absent enforcement, which would carry a different suppression/resistance profile once enforcement (Meiji separation) arrived. Each carries its own ε and classification per the ε-invariance principle; they are linked here, not merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
