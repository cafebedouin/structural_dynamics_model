% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_survival__competence_transmission_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_survival__competence_transmission_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: catastrophe_memory_survival__competence_transmission_reading
 *   human_readable: Ritual Competence Transmission via Catastrophe Memory
 *   domain: religious_studies/collective_memory
 *
 * SUMMARY:
 *   This constraint instantiates the competence_transmission_reading of the
 *   catastrophe_memory_survival kernel. The kernel concerns whether ritual
 *   practice under catastrophic discontinuity functions as a survival
 *   mechanism. In this reading, ritual is a durable encoding vessel for
 *   practical knowledgeâagricultural timing, resource management, family
 *   protocolsâtransmitted to diaspora communities who retain adaptive
 *   capacity. Heritage communities in the original locus bear the cost of
 *   maintaining ritual form after the practical content has eroded, creating
 *   asymmetric extraction within the same structural arrangement. Sibling
 *   readings (symbol_survival_reading, hybrid_encoding_reading) produce
 *   different beneficiary/victim structures and classification profiles.
 *
 * KEY AGENTS:
 *   - diaspora_communities: Primary beneficiary (moderate/global/constrained) â receives encoded adaptive capacity.
 *   - heritage_communities: Primary target (powerless/local/identity_locked) â maintains costly ritual form without functional return.
 *   - ritual_specialists: Agenda-setter (organized/regional/constrained) â administers performance and enforces correct form.
 *   - ethnographic_observers: Analytical observer (analytical/global/analytical) â documents divergence between encoded content and current performance.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_survival__competence_transmission_reading, 0.58).
domain_priors:suppression_score(catastrophe_memory_survival__competence_transmission_reading, 0.48).
domain_priors:theater_ratio(catastrophe_memory_survival__competence_transmission_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_survival__competence_transmission_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_survival__competence_transmission_reading, "Ritual Competence Transmission via Catastrophe Memory").
narrative_ontology:topic_domain(catastrophe_memory_survival__competence_transmission_reading, "religious_studies/collective_memory").

domain_priors:requires_active_enforcement(catastrophe_memory_survival__competence_transmission_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_survival__competence_transmission_reading, 'b2794f65-55f9-4fef-b009-dca78c73b371').
narrative_ontology:cs_kernel_codification('b2794f65-55f9-4fef-b009-dca78c73b371', distributed).
narrative_ontology:cs_authority_grounding('b2794f65-55f9-4fef-b009-dca78c73b371', lineage).
narrative_ontology:cs_interpretation_layer_present('b2794f65-55f9-4fef-b009-dca78c73b371').
narrative_ontology:cs_reading_relation('b2794f65-55f9-4fef-b009-dca78c73b371', catastrophe_memory_survival__symbol_survival_reading, coexists_with).
narrative_ontology:cs_reading_relation('b2794f65-55f9-4fef-b009-dca78c73b371', catastrophe_memory_survival__hybrid_encoding_reading, coexists_with).
narrative_ontology:cs_axiom('b2794f65-55f9-4fef-b009-dca78c73b371', foundational, practical_competence_constitutes_cultural_survival).
narrative_ontology:cs_axiom_status(practical_competence_constitutes_cultural_survival, holdable).
narrative_ontology:cs_axiom_grounding('b2794f65-55f9-4fef-b009-dca78c73b371', practical_competence_constitutes_cultural_survival, empirically_contingent).
narrative_ontology:cs_axiom('b2794f65-55f9-4fef-b009-dca78c73b371', foundational, ritual_form_encodes_operational_content).
narrative_ontology:cs_axiom_status(ritual_form_encodes_operational_content, holdable).
narrative_ontology:cs_axiom_grounding('b2794f65-55f9-4fef-b009-dca78c73b371', ritual_form_encodes_operational_content, instrumental).
narrative_ontology:cs_reference_frame('b2794f65-55f9-4fef-b009-dca78c73b371', practical_competence_reservoir).
narrative_ontology:cs_drift_state('b2794f65-55f9-4fef-b009-dca78c73b371', contemporary_diaspora_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b2794f65-55f9-4fef-b009-dca78c73b371', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_survival__competence_transmission_reading, catastrophe_memory_survival).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__competence_transmission_reading, diaspora_communities).
narrative_ontology:constraint_victim(catastrophe_memory_survival__competence_transmission_reading, heritage_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Displaced communities that draw on ritual structures as repositories of practical adaptation knowledgeâagricultural calendars, resource-preservation protocols, kinship obligationsâtranslating encoded competence into survival strategies in new environments. They benefit from the functional content without bearing the full institutional cost of ritual maintenance.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, diaspora_communities, beneficiary,
    moderate, generational, constrained, global).

% Communities in the original locale who continue performing ritual cycles but whose practical content has eroded due to environmental change or economic displacement. They bear the social and material costs of maintaining ritual infrastructureâtime, resources, status obligationsâwhile the functional knowledge that once justified the expenditure no longer operates in their context. Exit is experienced as cultural death.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, heritage_communities, payer,
    powerless, generational, identity_locked, local).

% Elders, priests, or knowledge-keepers who officiate ritual performance, adjudicate correct form, and enforce participation through social sanction and lineage authority. Their status and social role depend on the ritual's continued performance regardless of whether the encoded practical content remains legible or functional.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, ritual_specialists, agenda_setter,
    organized, generational, constrained, regional).

% Scholars studying ritual transmission who document the divergence between encoded practical instructions and current performance, mapping which communities retain operational competence and which maintain hollowed form.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, ethnographic_observers, observer,
    analytical, civilizational, analytical, global).

narrative_ontology:fixing_cost_class(catastrophe_memory_survival__competence_transmission_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves and transmits practical survival knowledge across generational and geographic discontinuity, encoding adaptation strategies into repeatable ritual form so that displaced or threatened communities retain operational competence.
% TRANSFER_FUNCTION: Moves encoded practical knowledge and adaptive capacity from heritage communities toward diaspora communities, while the ritual form persists in communities where the practical content has eroded.
% ABSENT_VOICES: Secular modernizers within heritage communities who would abandon ritual form entirely if they recognized the practical content was already lost; also diaspora members who experience the ritual as purely symbolic and are unaware of its encoded competence function.
% DISAPPEARANCE_RATIONALE: If the ritual constraint vanished, diaspora communities would lose a structured repository of adaptation knowledge they rely on for resource timing and family protocols; heritage communities would be released from the social obligation to maintain hollowed-out performance, but intergenerational memory of practical strategies would fragment.
% FOUNDING_PROBLEM: Catastrophic disruption (displacement, environmental collapse, persecution) breaks linear knowledge transmission; communities need durable, low-fidelity-cost encoding methods to preserve actionable survival competence across discontinuities.
% FOUNDING_PROBLEM_CORROBORATION: Anthropologists and oral historians outside the benefiting diaspora communities document the encoding function; some heritage-community elders corroborate the practical origin, while others assert the ritual has always been primarily symbolic.
narrative_ontology:disappearance_verdict(catastrophe_memory_survival__competence_transmission_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_survival__competence_transmission_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_survival__competence_transmission_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_survival__competence_transmission_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_survival__competence_transmission_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_survival__competence_transmission_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_survival__competence_transmission_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_survival__competence_transmission_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58) because the constraint genuinely transmits practical knowledge to diaspora communities while simultaneously extracting maintenance labor and identity-locked compliance from heritage communities who no longer receive functional benefit. Suppression (0.48) reflects social and lineage enforcement rather than physical coercion, intensifying as practical content hollows out and performance must be defended against doubt. Theater ratio (0.42) captures the growing share of performative maintenance relative to operational content in heritage contexts. Accessibility collapse (0.65) is high because once a community is identity-locked into ritual performance, alternatives collapse socially; resistance (0.20) is low because the arrangement is normalized as tradition.
 *
 * PERSPECTIVAL GAP:
 *   The diaspora seat experiences the constraint as genuine coordinationâa rope transmitting survival knowledge across displacement. The heritage-community seat experiences the same constraint as extractionâa snare that locks them into costly form without functional return. The ritual-specialist seat experiences it as institutional maintenance. The engine computes this divergence from the same structural data; the authored claim (tangled_rope) marks the hybrid reality without reconciling the seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Diaspora communities are declared beneficiaries (low d, subsidized by the constraint's knowledge transmission). Heritage communities are declared victims (high d, extraction amplified by identity-locked exit and local scope). Ritual specialists sit near the agenda-setter middle with constrained exit; their directionality is structurally closer to the beneficiary end because the constraint sustains their authority, though they do not capture the primary extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâpreserving practical knowledge across catastrophic discontinuityâis live for diaspora communities but substantially dead for heritage communities, whose environment no longer matches the encoded competence. This mismatch is a mandatrophy signal. However, because the constraint still performs genuine coordination for one seat, classification as tangled_rope prevents mislabeling it as a piton (pure inertia) or snare (pure extraction). The asymmetry is structural: one seat's coordination is another seat's extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'How does the competence_transmission_reading structurally relate to its sibling readings within the catastrophe_memory_survival kernel?',
    'Comparative ethnographic analysis of whether communities holding one reading can logically hold another within the same ritual framework; also tracking how sibling readings redistribute victim and beneficiary sets.',
    'If symbol_survival is adopted, the heritage-community victim set dissolves (hollow form becomes legitimate continuity), dropping Îµ and potentially reclassifying toward rope. If hybrid_encoding is adopted, extraction is split across registers, altering directionality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Structural location of this kernel reading relative to siblings.').

omega_variable(
    encoded_competence_verifiability,
    'Can the practical survival knowledge attributed to ritual be independently verified as encoded in the ritual structure, or is it an ex post functionalist interpretation projected by diaspora communities?',
    'Archaeological and ethnographic reconstruction comparing ritual instructions to independently attested historical survival outcomes; controlled comparison of diaspora communities with and without ritual access.',
    'If the encoding is not verifiable, the coordination function dissolves and the constraint reclassifies toward snare or piton. If verifiable, the tangled_rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(encoded_competence_verifiability, empirical, 'Whether ritual actually encodes actionable practical knowledge.').

omega_variable(
    heritage_victim_status,
    'Are heritage communities that maintain ritual form without practical content genuinely victimized by the constraint, or are they willing participants in identity preservation?',
    'Comparative study of exit costs and internalized versus structural barriers to abandoning ritual form; interview data on subjective experience of obligation.',
    'If participation is willing identity choice with low structural barriers, victim status is weakened, directionality for heritage_communities shifts toward symmetric, and Îµ may drop. If barriers are structural and internalized, the victim declaration holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(heritage_victim_status, conceptual, 'Whether heritage community costs are extractive or voluntary identity maintenance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_survival__competence_transmission_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 30, 0.32).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 40, 0.37).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 50, 0.42).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 60, 0.46).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 20, 0.44).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 30, 0.5).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 40, 0.54).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 50, 0.58).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 60, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(cata_su_t10, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 10, 0.34).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 20, 0.38).
narrative_ontology:measurement(cata_su_t30, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 30, 0.42).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 40, 0.46).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 50, 0.5).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 60, 0.54).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(catastrophe_memory_survival__competence_transmission_reading, symbol_survival_reading).
narrative_ontology:affects_constraint(catastrophe_memory_survival__competence_transmission_reading, hybrid_encoding_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the catastrophe_memory_survival kernel. The kernel decomposes into three structurally distinct claims: competence_transmission (practical knowledge focus, moderate Îµ), symbol_survival (identity focus, lower Îµ), and hybrid_encoding (dual-register, split Îµ). Each reading has distinct beneficiary/victim structures and classification profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
