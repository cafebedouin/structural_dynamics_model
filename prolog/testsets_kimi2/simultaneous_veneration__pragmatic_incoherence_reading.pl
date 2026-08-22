% ============================================================================
% CONSTRAINT STORY: simultaneous_veneration__pragmatic_incoherence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_simultaneous_veneration__pragmatic_incoherence_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: simultaneous_veneration__pragmatic_incoherence_reading
 *   human_readable: Simultaneous Veneration (Pragmatic Incoherence Reading)
 *   domain: religious studies/history
 *
 * SUMMARY:
 *   This constraint is the pragmatic incoherence reading of the
 *   simultaneous_veneration kernel, which frames shinbutsu-shÅ«gÅ
 *   (Shinto-Buddhist syncretism) as an operationally incoherent arrangement
 *   that extracted material and cognitive compliance from practitioners
 *   without delivering theological coherence. Sibling readings include
 *   ontological_fusion_reading (honji-suijaku as metaphysical truth) and
 *   domain_partition_reading (functional specialization by domain). This
 *   reading instantiates the constraint as a snare: the coordination
 *   narratives are retrospective cover for an arrangement whose actual
 *   function was institutional rent extraction sustained by suppressed
 *   contradiction.
 *
 * KEY AGENTS:
 *   - common_practitioners: Primary target (powerless/constrained) â bears extraction through cognitive and material costs
 *   - temple_shrine_institutions: Primary beneficiary (institutional/mobile) â collects resources and maintains power through ambiguous theology
 *   - syncretic_ritual_specialists: Secondary beneficiary/target (moderate/identity_locked) â dual-positioned agent who benefits materially but pays cognitively
 *   - meiji_reform_observers: Analytical observer (institutional/analytical) â sees full structure and imposes separation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(simultaneous_veneration__pragmatic_incoherence_reading, 0.82).
domain_priors:suppression_score(simultaneous_veneration__pragmatic_incoherence_reading, 0.65).
domain_priors:theater_ratio(simultaneous_veneration__pragmatic_incoherence_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(simultaneous_veneration__pragmatic_incoherence_reading, snare).
narrative_ontology:human_readable(simultaneous_veneration__pragmatic_incoherence_reading, "Simultaneous Veneration (Pragmatic Incoherence Reading)").
narrative_ontology:topic_domain(simultaneous_veneration__pragmatic_incoherence_reading, "religious studies/history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(simultaneous_veneration__pragmatic_incoherence_reading, '4c17c307-5d90-4737-9cdc-1cd71bac210d').
narrative_ontology:cs_kernel_codification('4c17c307-5d90-4737-9cdc-1cd71bac210d', distributed).
narrative_ontology:cs_authority_grounding('4c17c307-5d90-4737-9cdc-1cd71bac210d', practice).
narrative_ontology:cs_interpretation_layer_present('4c17c307-5d90-4737-9cdc-1cd71bac210d').
narrative_ontology:cs_reading_relation('4c17c307-5d90-4737-9cdc-1cd71bac210d', simultaneous_veneration__ontological_fusion_reading, forecloses).
narrative_ontology:cs_reading_relation('4c17c307-5d90-4737-9cdc-1cd71bac210d', simultaneous_veneration__domain_partition_reading, forecloses).
narrative_ontology:cs_axiom('4c17c307-5d90-4737-9cdc-1cd71bac210d', foundational, simultaneous_veneration_incoherent).
narrative_ontology:cs_axiom_status(simultaneous_veneration_incoherent, holdable).
narrative_ontology:cs_axiom_grounding('4c17c307-5d90-4737-9cdc-1cd71bac210d', simultaneous_veneration_incoherent, empirically_contingent).
narrative_ontology:cs_axiom('4c17c307-5d90-4737-9cdc-1cd71bac210d', foundational, meiji_revelation_not_imposition).
narrative_ontology:cs_axiom_status(meiji_revelation_not_imposition, holdable).
narrative_ontology:cs_axiom_grounding('4c17c307-5d90-4737-9cdc-1cd71bac210d', meiji_revelation_not_imposition, empirically_contingent).
narrative_ontology:cs_reference_frame('4c17c307-5d90-4737-9cdc-1cd71bac210d', syncretic_equilibrium).
narrative_ontology:cs_drift_state('4c17c307-5d90-4737-9cdc-1cd71bac210d', meiji_separation_era, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('4c17c307-5d90-4737-9cdc-1cd71bac210d', '2026-06-11T00:00:00Z').
narrative_ontology:cs_kernel_id(simultaneous_veneration__pragmatic_incoherence_reading, simultaneous_veneration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(simultaneous_veneration__pragmatic_incoherence_reading, temple_shrine_institutions).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__pragmatic_incoherence_reading, syncretic_ritual_specialists).
narrative_ontology:constraint_victim(simultaneous_veneration__pragmatic_incoherence_reading, common_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(simultaneous_veneration__pragmatic_incoherence_reading, syncretic_ritual_specialists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Required to participate in both Buddhist and Shinto rituals under the danka and ujiko systems, paying fees and offerings to temple and shrine institutions. Lacked any theological framework resolving the relationship between kami and buddhas, and had no institutional channel to demand coherence. Exit meant social ostracism, loss of community standing, and legal penalties under temple registration requirements.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, common_practitioners, payer,
    powerless, biographical, constrained, national).

% Administered syncretic doctrines and rituals, collected parishioner fees, land rents, and state patronage. Maintained institutional power by preserving the ambiguous theological middle ground between pure Buddhism and pure Shinto. Resisted theological clarification because it threatened their dual-income base and social role. Could adapt to pure sectarian identities when externally forced.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, temple_shrine_institutions, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(simultaneous_veneration__pragmatic_incoherence_reading, temple_shrine_institutions, beneficiary).

% Performed rituals drawing on both Buddhist and Shinto registers. Benefited materially from broad ritual demand but bore the cognitive cost of maintaining contradictory theological claims in professional practice. Training and professional identity were fused with the syncretic system, making exit synonymous with career dissolution.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, syncretic_ritual_specialists, beneficiary,
    moderate, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(simultaneous_veneration__pragmatic_incoherence_reading, syncretic_ritual_specialists, payer).

% External to the Edo-period religious economy. Observed accumulated theological and institutional incoherence and imposed shinbutsu-bunri as a modernizing clarification. Their intervention exposed the latent contradiction that the arrangement had suppressed for centuries.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, meiji_reform_observers, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(simultaneous_veneration__pragmatic_incoherence_reading, temple_shrine_institutions).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None coherent; the arrangement claimed to integrate kami and buddha worship into a unified spiritual economy, but this integration was never achieved. Practitioners were instead bound to contradictory ritual obligations without theological resolution.
% TRANSFER_FUNCTION: Moves material resources and cognitive compliance from common practitioners to temple and shrine institutions, under the cover of providing unified spiritual services that were never theologically coherent.
% ABSENT_VOICES: Theologians advocating strict domain separation or exclusive Buddhist or Shinto identity were marginalized. Common practitioners who might have demanded theological coherence had no institutional voice. Anti-Buddhist polemicists occasionally noted the contradiction but were suppressed by the institutional mainstream.
% DISAPPEARANCE_RATIONALE: The Meiji separation demonstrates that the arrangement was structuring religious practice, not merely describing it. Its disappearance forced a reorganization of ritual life, parishioner obligations, institutional funding, and professional identity.
% FOUNDING_PROBLEM: The need to integrate immigrant Buddhist institutions with indigenous kami worship in a society where both were socially necessary but metaphysically unresolved.
% FOUNDING_PROBLEM_CORROBORATION: Meiji-era bureaucrats and modern religious studies scholars outside the benefiting institutions attest that the pre-Meiji arrangement concealed incoherence. Edo-period anti-Buddhist polemicists, also outside the benefiting core, occasionally noted the contradiction, though they were suppressed.
narrative_ontology:disappearance_verdict(simultaneous_veneration__pragmatic_incoherence_reading, world_rearranges).
narrative_ontology:founding_problem_status(simultaneous_veneration__pragmatic_incoherence_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(simultaneous_veneration__pragmatic_incoherence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(simultaneous_veneration__pragmatic_incoherence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(simultaneous_veneration__pragmatic_incoherence_reading, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(simultaneous_veneration__pragmatic_incoherence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(simultaneous_veneration__pragmatic_incoherence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(simultaneous_veneration__pragmatic_incoherence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because the arrangement extracted material resources and cognitive compliance while delivering no coherent spiritual good. Suppression is moderate-high (0.65): while the reading notes a lack of active state enforcement, alternatives were suppressed through social obligation, temple registration, and identity fusion. Theater ratio is elevated (0.6) because the performative maintenance of theological coherence (honji-suijaku discourse, syncretic ritual) occupied an increasing share of institutional activity relative to genuine coordination. Resistance is low (0.3) until the Meiji rupture; the incoherence was latent rather than contested by the constrained practitioner population.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat (common practitioners) experienced the constraint as an unresolvable tangle of contradictory obligations bearing real material and cognitive cost. The beneficiary seats (temple/shrine institutions and ritual specialists) experienced it as a sustainable source of revenue and status. The engine will compute this divergence from the structural data: identical scope, radically different exit options and power levels.
 *
 * DIRECTIONALITY LOGIC:
 *   Temple and shrine institutions are structural beneficiaries (collect patronage, control ritual access â d near the beneficiary end). Ritual specialists sit closer to symmetric: they benefit materially but their identity-locked exit makes them partly captured. Common practitioners are the targets (pay fees, bear cognitive dissonance, constrained exit â d near the target end). Meiji observers are analytical with no directionality stake.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â integrating Buddhist and indigenous institutions â was never solved by the syncretic arrangement under this reading. The constraint persisted not because its mandate was live but because its incoherence was institutionally profitable and socially embedded. Meiji shinbutsu-bunri is read not as an imposed rupture but as a revelation that the mandate had been dead for centuries, exposing the arrangement as extraction without coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_validity,
    'Does the pragmatic incoherence reading accurately describe the historical operation of simultaneous veneration, or does it project modern secular rationality onto pre-modern practice?',
    'Archaeological and textual evidence of practitioner-level theological understanding; comparative analysis of other religious syncretisms.',
    'If the incoherence reading is correct, the constraint is properly classified as snare. If domain partition or ontological fusion held at the practitioner level, the constraint could reclassify as rope or tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_validity, conceptual, 'Whether pragmatic incoherence is historically accurate or anachronistic').

omega_variable(
    suppression_mechanism_ambiguity,
    'Was the persistence of simultaneous veneration sustained primarily by structural enforcement (temple registration, social obligation) or by internalized cognitive habits that prevented practitioners from perceiving the contradiction?',
    'Ethnographic and textual study of practitioner discourse; analysis of exit costs under the danka system.',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest, and the snare classification is reinforced through identity-locked exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression in religious syncretism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(simultaneous_veneration__pragmatic_incoherence_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(simven_pragmatic_tr_t0, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 0, 0.5).
narrative_ontology:measurement(simven_pragmatic_tr_t25, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 25, 0.53).
narrative_ontology:measurement(simven_pragmatic_tr_t50, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 50, 0.56).
narrative_ontology:measurement(simven_pragmatic_tr_t75, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 75, 0.58).
narrative_ontology:measurement(simven_pragmatic_tr_t100, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 100, 0.6).

% Extraction over time
narrative_ontology:measurement(simven_pragmatic_be_t0, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(simven_pragmatic_be_t25, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 25, 0.75).
narrative_ontology:measurement(simven_pragmatic_be_t50, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 50, 0.78).
narrative_ontology:measurement(simven_pragmatic_be_t75, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 75, 0.8).
narrative_ontology:measurement(simven_pragmatic_be_t100, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 100, 0.82).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(simultaneous_veneration__pragmatic_incoherence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(simultaneous_veneration__pragmatic_incoherence_reading, ontological_fusion_reading).
narrative_ontology:affects_constraint(simultaneous_veneration__pragmatic_incoherence_reading, domain_partition_reading).

% DUAL FORMULATION NOTE:
% The kernel of simultaneous veneration decomposes into three structurally distinct readings: ontological fusion (metaphysical coherence), domain partition (functional coherence), and pragmatic incoherence (no stable constraint). Each reading instantiates a different constraint with different epsilon, beneficiary/victim structures, and classification. This reading asserts the kernel was always operationally incoherent, with the other two readings serving as retrospective rationalizations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
