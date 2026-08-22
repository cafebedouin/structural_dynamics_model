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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: simultaneous_veneration__domain_partition_reading
 *   human_readable: Domain-Partition Simultaneous Veneration of Kami and Buddhas (Japanese Religious Pluralism)
 *   domain: religious/cultural/institutional
 *
 * SUMMARY:
 *   From approximately the 6th century CE onward, Japanese religious
 *   practitioners simultaneously venerated kami (indigenous deities with
 *   material/this-worldly portfolios) and buddhas (transcendent beings
 *   governing salvation and rebirth). This was not understood as theological
 *   incoherence by most practitioners; rather, it was treated as a functional
 *   domain partition: kami govern this-worldly prosperity (crops, health,
 *   children, business), buddhas govern soteriological outcomes (salvation,
 *   rebirth in better realms). The constraint is the social and institutional
 *   arrangement that makes this simultaneous veneration work without
 *   requiring practitioners to resolve the metaphysical status of kami
 *   relative to buddhas. This reading treats the domain partition as a
 *   genuine coherence solution — kami and buddhas are functionally distinct,
 *   each specialized for its domain, and simultaneous veneration is
 *   domain-appropriate specialization rather than theological confusion. This
 *   is one of three competing readings of the contested kernel 'simultaneous
 *   veneration'; the sibling readings offer alternative framings (ontological
 *   identity via honji-suijaku theory, or pragmatic incoherence without
 *   resolution).
 *
 * KEY AGENTS:
 *   - Practitioners seeking worldly prosperity: approach kami shrines for material benefits; benefit from the partition by not needing kami to be soteriologically competent.
 *   - Practitioners seeking soteriological assurance: approach buddhist temples for salvation; benefit from the partition by not needing to derive kami from buddha doctrine.
 *   - Shrine priests (kami realm maintenance): administer kami veneration and teach domain specialization; benefit from recognized role in material-welfare coordination.
 *   - Buddhist clergy (buddha realm maintenance): teach doctrine and conduct merit-transfer rituals; benefit from recognized role in soteriological coordination.
 *   - Meiji-era state authorities and later competing frameworks: excluded from the partition's coordination because they refuse its fundamental premise (treating kami and buddhas as legitimately distinct); their exclusion is external to the constraint's operation.
 *   - Theological synthesists (honji-suijaku theorists): provide alternative ontological accounts but do not enforce them; their work creates a sibling reading (ontological_fusion_reading) rather than displacing the working partition.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(simultaneous_veneration__domain_partition_reading, 0.15).
domain_priors:suppression_score(simultaneous_veneration__domain_partition_reading, 0.08).
domain_priors:theater_ratio(simultaneous_veneration__domain_partition_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(simultaneous_veneration__domain_partition_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(simultaneous_veneration__domain_partition_reading, rope).
narrative_ontology:human_readable(simultaneous_veneration__domain_partition_reading, "Domain-Partition Simultaneous Veneration of Kami and Buddhas (Japanese Religious Pluralism)").
narrative_ontology:topic_domain(simultaneous_veneration__domain_partition_reading, "religious/cultural/institutional").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(simultaneous_veneration__domain_partition_reading, 'ce644e4b-b05b-42e8-8d6c-f50d62458374').
narrative_ontology:cs_kernel_codification('ce644e4b-b05b-42e8-8d6c-f50d62458374', distributed).
narrative_ontology:cs_authority_grounding('ce644e4b-b05b-42e8-8d6c-f50d62458374', practice).
narrative_ontology:cs_interpretation_layer_present('ce644e4b-b05b-42e8-8d6c-f50d62458374').
narrative_ontology:cs_reading_relation('ce644e4b-b05b-42e8-8d6c-f50d62458374', simultaneous_veneration__ontological_fusion_reading, coexists_with).
narrative_ontology:cs_reading_relation('ce644e4b-b05b-42e8-8d6c-f50d62458374', simultaneous_veneration__pragmatic_incoherence_reading, coexists_with).
narrative_ontology:cs_axiom('ce644e4b-b05b-42e8-8d6c-f50d62458374', foundational, kami_buddha_functional_distinctness).
narrative_ontology:cs_axiom_status(kami_buddha_functional_distinctness, holdable).
narrative_ontology:cs_axiom_grounding('ce644e4b-b05b-42e8-8d6c-f50d62458374', kami_buddha_functional_distinctness, conventional).
narrative_ontology:cs_axiom('ce644e4b-b05b-42e8-8d6c-f50d62458374', foundational, domain_partition_coherence).
narrative_ontology:cs_axiom_status(domain_partition_coherence, holdable).
narrative_ontology:cs_axiom_grounding('ce644e4b-b05b-42e8-8d6c-f50d62458374', domain_partition_coherence, instrumental).
narrative_ontology:cs_reference_frame('ce644e4b-b05b-42e8-8d6c-f50d62458374', simultaneous_kami_buddha_veneration_as_coherent_specialization).
narrative_ontology:cs_drift_state('ce644e4b-b05b-42e8-8d6c-f50d62458374', meiji_forced_separation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ce644e4b-b05b-42e8-8d6c-f50d62458374', '2026-06-19T14:32:00Z').
narrative_ontology:cs_kernel_id(simultaneous_veneration__domain_partition_reading, simultaneous_veneration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(simultaneous_veneration__domain_partition_reading, practitioners__seeking_worldly_prosperity).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__domain_partition_reading, practitioners__seeking_soteriological_assurance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Approach kami shrines for this-worldly benefits: bumper crops, healthy children, business success, safe travel. The domain partition enables them to address immediate material concerns to a specialized entity (kami) while maintaining soteriological insurance elsewhere. They participate voluntarily and can adjust their devotional practice based on perceived efficacy.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, practitioners__seeking_worldly_prosperity, beneficiary,
    moderate, biographical, mobile, local).

% Approach buddhist temples for salvation and rebirth in a better realm, merit accumulation, and guidance on ethical conduct. The domain partition allows them to maintain this soteriological commitment without requiring kami to be soteriologically effective (which would demand they address kami within a salvation framework). They can choose among buddhist schools and temples based on their preferred path.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, practitioners__seeking_soteriological_assurance, beneficiary,
    moderate, generational, mobile, local).

% Shrine priests and local authority figures maintain kami shrines, conduct seasonal rituals, and adjudicate disputes about proper kami veneration practice. They administer the domain partition by teaching that kami govern this-worldly matters and directing practitioners to buddhas for salvation. Their authority rests on recognized competence in kami propitiation, not on enforcement coercion.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, kami_realm_maintenance_agents, agenda_setter,
    organized, generational, constrained, regional).

% Buddhist clergy (monks, temple administrators) maintain doctrine, conduct ordinations and merit-transfer rituals, and teach the path to enlightenment. They administer the domain partition by accepting that kami operate in the this-worldly realm while buddhas govern the salvific path. Their authority rests on transmitted doctrine and recognized spiritual expertise, not on coercion.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, buddha_realm_maintenance_agents, agenda_setter,
    organized, generational, constrained, regional).

% Meiji-era state authorities and later monotheistic frameworks (Christianity, later nationalism) that treated simultaneous veneration as incoherent and sought to enforce exclusive allegiance. They are excluded from the domain partition's adjudication because the partition was never built to satisfy them — it was built to satisfy practitioners' simultaneous needs within Japanese religious ecology. The state later imposed separation (1868 onwards) but that was external coercion, not internal to the partition constraint.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, competing_religious_monopolists, excluded,
    institutional, generational, trapped, national).

% Scholars and religious thinkers who developed honji-suijaku theory (original essence and manifest traces) to provide an ontological account of kami-buddha identity. They are excluded from enforcing the domain partition because their synthesis was always minority position, never the working assumption of most practitioners. Their intellectual work provided an alternative reading (ontological_fusion_reading) but did not displace the domain-partition practice.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, theological_synthesists, excluded,
    moderate, biographical, mobile, regional).

% External scholarly and analytical position assessing the constraint's structure from outside the practicing communities. Evaluates whether the domain partition is genuinely coherent as a coordination mechanism or whether it masks unresolved theological contradictions.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__domain_partition_reading, analytical_observer, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the problem of simultaneous soteriological need (how to ensure salvation/rebirth in better realm) and material-welfare need (how to secure crops, health, prosperity in this life) by partitioning the religious domain: kami handle immediate this-worldly concerns, buddhas handle ultimate salvation. This allows practitioners to address both needs without requiring a single entity to be competent across incommensurable domains.
% TRANSFER_FUNCTION: Moves no goods between agents. The constraint is coordinative, not extractive. Practitioners devote time and resources (ritual participation, vows, offerings) to both kami and buddhas, but the partition itself does not extract wealth, labor, or status asymmetrically — both shrine priests and temple clergy maintain their communities through voluntary participation and traditional support mechanisms, not through the partition's operation. The partition enables voluntary dual participation.
% ABSENT_VOICES: Practitioners who experienced the partition as religiously incoherent (those who could not resolve the kami-buddha distinction philosophically) would object, but they are largely silent in the historical record because the pragmatic success of the partition (enabling simultaneous veneration without forcing a choice) meant most practitioners did not need coherence at the philosophical level. Meiji-era state authorities and later Christian missionaries objected to what they saw as incoherence, but they were external to the system and their objections drove coercive change (Kami-Buddha Separation edict, 1868) rather than resolving internal contradiction.
% DISAPPEARANCE_RATIONALE: If the domain partition vanished overnight — if practitioners were forced to choose between kami and buddha veneration, or required to provide a unified metaphysical account of their dual practice — the Japanese religious ecology would reorganize. Some practitioners would drop one domain; some would adopt the ontological-fusion reading (honji-suijaku); some might shift to pure secularism or import religions (Christianity, rationalism) that offer no kami domain. The coordinated solution would dissolve and new equilibria would emerge.
% FOUNDING_PROBLEM: Early-to-mid period Japan had a genuine problem: practitioners needed both material welfare assurance (crops, health, children, prosperity) and soteriological assurance (salvation, rebirth in better realm). Individual kami were known to operate in localized, material registers (water kami for irrigation, war kami for victory, etc.). Buddhism offered sophisticated accounts of salvation and the realms of rebirth. Requiring practitioners to choose between them forced a sacrifice that neither the practice nor the theological resources demanded. The domain partition solved this by treating kami and buddhas as specialized for different ontological registers rather than as competitors.
% FOUNDING_PROBLEM_CORROBORATION: The domain partition solving this dual-need problem is corroborated by (1) ethnographic and historical accounts of Japanese religious practice showing continuous simultaneous participation from at least the Heian period onward; (2) temple and shrine records showing coordinated rather than competitive relationships (shared calendars, joint observances, priests trained in both traditions); (3) textual evidence of early-period Buddhist clergy (e.g., Saichō, Kūkai) explicitly accommodating kami veneration within Buddhist frameworks without requiring doctrinal resolution; (4) practitioners' own accounts (diaries, pilgrimage records) showing comfort with dual veneration without expressing philosophical anxiety about it. Outside corroboration: contemporary anthropologists (Gombrich, Religion and Society in Modern Japan) and historians (Kuroda Toshio's work on Buddhist-Shinto symbiosis, Hardacre) confirm the live nature of the coordination problem and its practical solution through domain partition.
narrative_ontology:disappearance_verdict(simultaneous_veneration__domain_partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(simultaneous_veneration__domain_partition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(simultaneous_veneration__domain_partition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(simultaneous_veneration__domain_partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(simultaneous_veneration__domain_partition_reading, 0.15, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is very low (0.15 at interval end) because the partition operates as pure coordination — no agent extracts rents from the arrangement; both shrine priests and temple clergy maintain their communities through traditional voluntary support, not through monopoly control enabled by the partition. The separation of domains is genuinely beneficial to practitioners: it allows them to address incommensurable needs (material welfare, ultimate salvation) without forcing a false choice. Suppression is minimal (0.08) because the partition requires no coercive enforcement — practitioners adopt it voluntarily because it solves their real problem. Theater ratio is very low (0.12) because the actual coordination work (directing practitioners to specialized entities for specialized needs) is the same as the functional work; there is minimal gap between the explicit claim (kami handle prosperity, buddhas handle salvation) and what actually happens. Accessibility_collapse is moderately high (0.72) because once the domain partition is understood and accepted, alternatives collapse — a practitioner seeking material prosperity will naturally approach kami rather than buddhas, not because they are barred from approaching buddhas but because that would be functionally incoherent within the partition framework. Resistance is very low (0.15) because the arrangement does not meet opposition from those it serves; opposition comes only from external frameworks (state authorities, monotheistic religions) that reject the partition's premises but are not practitioners within it. The measurement series is stable over 800 time units because the core coordination function persists with only minor drift — slight increase in extractiveness reflects the accumulation of institutional ritual overhead and priestly mediation, but the fundamental coordinative structure does not change until external coercion (Meiji separation) is imposed.
 *
 * PERSPECTIVAL GAP:
 *   The payer and beneficiary seats should compute identically: from every seat inside the partition (both practitioner poles, both maintenance communities), the constraint looks like rope — a coordinative solution to a genuine collective-action problem. There is no perspectival divergence because no seat is systematically harmed. The analytical observer seat (external scholars) may compute differently by questioning whether the partition is genuinely coherent or pragmatically incoherent, but that is a judgment about the partition's coherence, not a difference in its extraction profile — even if pragmatically incoherent, it is still coordinative and non-extractive from the practitioner perspective.
 *
 * DIRECTIONALITY LOGIC:
 *   All stakeholders within the partition have low directionality toward extraction (high subsidy/coordination benefit). Practitioners are the primary beneficiaries (d near 0.0 for beneficiary end). Maintenance agents (shrine priests, clergy) benefit from stable institutional roles and do not extract from practitioners via the partition (their support comes from traditional alms/offerings, not from partition-enforced monopoly). The excluded seats (state authorities, competing religions) have no directionality within the constraint because they are external to it — the state's later imposition of separation is a different constraint (forced separation) entirely. No agent sits at the target end (d near 1.0) unless practitioners are forced to articulate a choice between kami and buddhas, at which point the partition dissolves and a new constraint (enforced choice) replaces it.
 *
 * MANDATROPHY ANALYSIS:
 *   The domain partition is not subject to mandatrophy because its founding problem (practitioners need both material welfare assurance and soteriological assurance, and neither kami nor buddhas alone satisfy both needs) remains live. Practitioners continue to approach kami for this-worldly benefits and buddhas for salvation; the functional partition persists. The Meiji separation (1868 onwards) was an external imposition, not a sign of internal mandate obsolescence. However, the separation did trigger a secondary mandatrophy: the post-1868 forced-separation constraint persists even after the state's original rationale (nation-building through religious purification) degraded; contemporary Japanese practitioners often re-synthesize kami and buddha veneration despite the formal separation, suggesting the forced-separation mandate has become inert or theatrical while the original coordinative partition reasserts itself informally.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    partition_metaphysical_coherence,
    'Is the domain partition a coherent metaphysical arrangement (kami and buddhas are genuinely different kinds of entities), or is it an intellectually unstable pragmatic arrangement concealing an incoherent belief system?',
    'Examination of practitioners'' own theological accounts and philosophical treatises; comparison with honji-suijaku frameworks (ontological fusion) to assess whether the partition was ever defended as coherent OR merely assumed as functional. Historical evidence of philosophical anxiety or lack thereof among the educated elite.',
    'If practitioners articulated the partition as genuinely coherent (two different ontological domains), it supports the domain-partition reading as a real constraint. If no coherence argument is made and practitioners simply acted as if the partition worked without defending it philosophically, it suggests the pragmatic-incoherence reading may better explain the actual operation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_metaphysical_coherence, conceptual, 'Whether the domain partition rests on genuine metaphysical distinctness or pragmatic assumption without coherence.').

omega_variable(
    honji_suijaku_foreclosure_status,
    'Did the development of honji-suijaku theory (ontological fusion) foreclose the domain-partition reading by providing a superior intellectual account that practitioners eventually adopted, or did honji-suijaku remain a scholarly minority position while the partition persisted as the working assumption?',
    'Textual analysis of the circulation and adoption of honji-suijaku among different social classes; temple curriculum records; clergy training texts; evidence of whether honji-suijaku was taught to lay practitioners or remained elite/scholarly. Measurement of its influence on actual practice vs. on intellectual justification.',
    'If honji-suijaku became the dominant framework and the partition was abandoned as incoherent, the ontological-fusion reading would retroactively foreclose this reading. If honji-suijaku remained minority-elite while practitioners continued domain-partition practice, this reading persists as the descriptively accurate constraint even if ontologically unstable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(honji_suijaku_foreclosure_status, empirical, 'Whether honji-suijaku theory displaced the domain partition or coexisted with it.').

omega_variable(
    meiji_separation_external_coercion,
    'Was the Meiji-era forced separation of kami and buddha veneration (1868 onwards) an externally imposed policy that violated an internally stable constraint, or was it a response to pre-existing internal instability that the state exploited?',
    'Examination of pre-1868 debate and tension within Japanese religious communities; evidence of calls for separation before the state mandate; comparison of how rapidly the separation was adopted vs. resisted across regions and communities.',
    'If the partition was internally stable and the separation was purely state coercion, the constraint should be modeled as persisting by inertia despite external pressure (piton characteristics might emerge post-1868, but the partition itself was rope). If significant pre-1868 internal pressure existed, the constraint''s stability was already degrading and the state merely formalized an emerging instability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meiji_separation_external_coercion, empirical, 'Whether Meiji separation was external coercion or response to pre-existing internal contradiction.').

omega_variable(
    reading_committer_sibling_ontology,
    'If the ontological-fusion reading (honji-suijaku) is the metaphysically true account of kami-buddha identity, does the domain-partition reading become false or merely pragmatically incomplete?',
    'Clarification of what ''true'' means in the context: metaphysical truth about being (what kami and buddhas actually are) vs. institutional/practical truth (how practitioners coordinate and make sense of practice). If honji-suijaku is metaphysically true but the partition is pragmatically useful, both framings can remain live. If honji-suijaku forecloses the partition''s core premise (kami and buddhas are genuinely different), the readings are not merely alternative but contradictory.',
    'This omega directly addresses the relationship between this reading and the ontological_fusion_reading sibling. Resolution determines whether the two readings coexist_with (hold simultaneously by different parties) or the fusion reading forecloses the partition reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_committer_sibling_ontology, conceptual, 'Committer-axis question: relationship between domain partition (functional distinctness) and ontological fusion (identity underneath).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(simultaneous_veneration__domain_partition_reading, 0, 800).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(simu_tr_t0, simultaneous_veneration__domain_partition_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(simu_tr_t0, observed).
narrative_ontology:measurement(simu_tr_t200, simultaneous_veneration__domain_partition_reading, theater_ratio, 200, 0.09).
narrative_ontology:measurement_basis(simu_tr_t200, observed).
narrative_ontology:measurement(simu_tr_t400, simultaneous_veneration__domain_partition_reading, theater_ratio, 400, 0.1).
narrative_ontology:measurement_basis(simu_tr_t400, observed).
narrative_ontology:measurement(simu_tr_t600, simultaneous_veneration__domain_partition_reading, theater_ratio, 600, 0.11).
narrative_ontology:measurement_basis(simu_tr_t600, observed).
narrative_ontology:measurement(simu_tr_t800, simultaneous_veneration__domain_partition_reading, theater_ratio, 800, 0.12).
narrative_ontology:measurement_basis(simu_tr_t800, observed).

% Extraction over time
narrative_ontology:measurement(simu_be_t0, simultaneous_veneration__domain_partition_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement_basis(simu_be_t0, observed).
narrative_ontology:measurement(simu_be_t200, simultaneous_veneration__domain_partition_reading, base_extractiveness, 200, 0.12).
narrative_ontology:measurement_basis(simu_be_t200, observed).
narrative_ontology:measurement(simu_be_t400, simultaneous_veneration__domain_partition_reading, base_extractiveness, 400, 0.14).
narrative_ontology:measurement_basis(simu_be_t400, observed).
narrative_ontology:measurement(simu_be_t600, simultaneous_veneration__domain_partition_reading, base_extractiveness, 600, 0.15).
narrative_ontology:measurement_basis(simu_be_t600, observed).
narrative_ontology:measurement(simu_be_t800, simultaneous_veneration__domain_partition_reading, base_extractiveness, 800, 0.15).
narrative_ontology:measurement_basis(simu_be_t800, observed).

% Suppression requirement over time
narrative_ontology:measurement(simu_su_t0, simultaneous_veneration__domain_partition_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement_basis(simu_su_t0, observed).
narrative_ontology:measurement(simu_su_t200, simultaneous_veneration__domain_partition_reading, suppression_requirement, 200, 0.06).
narrative_ontology:measurement_basis(simu_su_t200, observed).
narrative_ontology:measurement(simu_su_t400, simultaneous_veneration__domain_partition_reading, suppression_requirement, 400, 0.07).
narrative_ontology:measurement_basis(simu_su_t400, observed).
narrative_ontology:measurement(simu_su_t600, simultaneous_veneration__domain_partition_reading, suppression_requirement, 600, 0.075).
narrative_ontology:measurement_basis(simu_su_t600, observed).
narrative_ontology:measurement(simu_su_t800, simultaneous_veneration__domain_partition_reading, suppression_requirement, 800, 0.08).
narrative_ontology:measurement_basis(simu_su_t800, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(simultaneous_veneration__domain_partition_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(simultaneous_veneration__domain_partition_reading, 0.12).
narrative_ontology:affects_constraint(simultaneous_veneration__domain_partition_reading, simultaneous_veneration__ontological_fusion_reading).
narrative_ontology:affects_constraint(simultaneous_veneration__domain_partition_reading, simultaneous_veneration__pragmatic_incoherence_reading).
narrative_ontology:affects_constraint(simultaneous_veneration__domain_partition_reading, meiji_kami_buddha_forced_separation).

% DUAL FORMULATION NOTE:
% The 'simultaneous veneration' kernel decomposes into three structurally distinct constraints, each a different reading with different ε values and stakeholder structures. The domain_partition_reading (this constraint) treats kami-buddha veneration as coherent domain-appropriate specialization (low extractiveness, pure rope). The ontological_fusion_reading treats it as metaphysically identical beings differentiated by cultural tradition (minimal extractiveness, rope with symbolic overhead). The pragmatic_incoherence_reading treats it as unresolved theological contradiction sustained by pragmatic tolerance (moderate extractiveness from forced cognitive coherence-making, tangled rope or piton). Each reading is a complete constraint story; they are linked here by affects_constraints because they share the same kernel and compete as alternative framings of the same practice. The Meiji separation constraint (meiji_kami_buddha_forced_separation) is downstream of all three readings, as it externally enforces a choice between them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
