% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_transmission__symbol_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_transmission__symbol_continuity_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: catastrophe_memory_transmission__symbol_continuity_reading
 *   human_readable: Catastrophe Memory Transmission: Symbol Continuity Reading
 *   domain: religious_studies/collective_memory
 *
 * SUMMARY:
 *   In this reading of the catastrophe memory transmission kernel, ritual
 *   survives and transmits community identity through strict adherence to
 *   inherited symbolic form. The constraint operates within communities
 *   marked by collective trauma (genocide, displacement, historical
 *   persecution) where ritual practice—prescribed actions, symbolic objects,
 *   mourning sequences—functions as both external memory storage and
 *   psychological container. This reading claims that the FORM itself carries
 *   the irreducible essence of identity and that deviation from inherited
 *   practice amounts to betrayal of the dead and loss of community coherence.
 *   The constraint enforces form-fidelity through identity-lock (belonging
 *   requires obedience to ritual rules) and social sanction (innovation or
 *   abbreviation triggers exclusion or shaming). This is ONE reading of a
 *   contested kernel; two sibling readings—the operational_competence_reading
 *   and the hybrid_embedded_reading—hold that survival knowledge and adaptive
 *   capacity can coexist with, or are inseparable from, ritualized practice.
 *   This story models ONLY the symbol_continuity reading: high extraction
 *   from adaptive capacity, low tolerance for innovation, beneficiary is
 *   institutional identity continuity, victims are those who would adapt.
 *
 * KEY AGENTS:
 *   - Communal identity guardians (organized, identity-locked): enforce ritual fidelity, control narrative of authenticity, exclude innovators
 *   - Younger participants (moderate power, identity-locked): bear the cost of rigid adherence, cannot exit without losing identity
 *   - Adaptive practitioners (powerful, constrained): propose innovations that preserve mnemonic power while accommodating conditions, face institutional suppression
 *   - Generational innovators (moderate, constrained): propose hybrid approaches, face social pressure and exclusion
 *   - External analysts (analytical seat): observe from outside, document whether form-fidelity actually transmits survival knowledge or primarily maintains institutional control
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_transmission__symbol_continuity_reading, 0.68).
domain_priors:suppression_score(catastrophe_memory_transmission__symbol_continuity_reading, 0.72).
domain_priors:theater_ratio(catastrophe_memory_transmission__symbol_continuity_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, resistance, 0.43).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_transmission__symbol_continuity_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_transmission__symbol_continuity_reading, "Catastrophe Memory Transmission: Symbol Continuity Reading").
narrative_ontology:topic_domain(catastrophe_memory_transmission__symbol_continuity_reading, "religious_studies/collective_memory").

domain_priors:requires_active_enforcement(catastrophe_memory_transmission__symbol_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_transmission__symbol_continuity_reading, 'b2496d6b-c292-46fe-9eb2-8558d178b43a').
narrative_ontology:cs_kernel_codification('b2496d6b-c292-46fe-9eb2-8558d178b43a', implicit).
narrative_ontology:cs_authority_grounding('b2496d6b-c292-46fe-9eb2-8558d178b43a', lineage).
narrative_ontology:cs_interpretation_layer_present('b2496d6b-c292-46fe-9eb2-8558d178b43a').
narrative_ontology:cs_reading_relation('b2496d6b-c292-46fe-9eb2-8558d178b43a', catastrophe_memory_transmission__operational_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('b2496d6b-c292-46fe-9eb2-8558d178b43a', catastrophe_memory_transmission__hybrid_embedded_reading, influences).
narrative_ontology:cs_axiom('b2496d6b-c292-46fe-9eb2-8558d178b43a', foundational, identity_constituted_by_form_fidelity).
narrative_ontology:cs_axiom_status(identity_constituted_by_form_fidelity, holdable).
narrative_ontology:cs_axiom_grounding('b2496d6b-c292-46fe-9eb2-8558d178b43a', identity_constituted_by_form_fidelity, deontological).
narrative_ontology:cs_axiom('b2496d6b-c292-46fe-9eb2-8558d178b43a', foundational, form_deviation_equals_identity_loss).
narrative_ontology:cs_axiom_status(form_deviation_equals_identity_loss, holdable).
narrative_ontology:cs_axiom_grounding('b2496d6b-c292-46fe-9eb2-8558d178b43a', form_deviation_equals_identity_loss, deontological).
narrative_ontology:cs_reference_frame('b2496d6b-c292-46fe-9eb2-8558d178b43a', unbroken_symbolic_transmission).
narrative_ontology:cs_drift_state('b2496d6b-c292-46fe-9eb2-8558d178b43a', contemporary_diaspora_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b2496d6b-c292-46fe-9eb2-8558d178b43a', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_transmission__symbol_continuity_reading, catastrophe_memory_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__symbol_continuity_reading, communal_identity_guardians).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__symbol_continuity_reading, adaptive_practitioners).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__symbol_continuity_reading, generational_innovators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__symbol_continuity_reading, adaptive_practitioners).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__symbol_continuity_reading, younger_participants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Transmit and enforce ritual form as the essential carrier of communal identity and the encoded memory of past catastrophes. They argue that fidelity to ancestral practice is non-negotiable because identity IS continuity with the dead; departing from symbolic form means forgetting, betrayal, and the dissolution of the community. They control which rituals are performed, how strictly, and who is authorized to lead them. They enforce compliance through social shaming, exclusion from full community participation, and interpretive authority over what 'authentic' practice means.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, communal_identity_guardians, agenda_setter,
    organized, generational, identity_locked, local).

% Bear the cost of rigid ritual adherence: time away from work, economic opportunity, adaptation to changing environmental conditions, and psychological separation from peers outside the community. They are not opposed to remembrance but experience the form-fidelity requirement as an escalating burden whose connection to genuine survival or memory is increasingly unclear. Exit requires abandoning their inherited identity and communal belonging.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, younger_participants, payer,
    moderate, biographical, identity_locked, local).

% Develop ritual innovations (new materials, abbreviated forms, contextual variations) that preserve the ritual's emotional and mnemonic power while adapting to contemporary conditions. They argue this preserves both identity AND functionality. They are constrained because proposing adaptation meets fierce resistance from guardians—being labeled a 'traitor to memory' or 'eraser of ancestors' can result in institutional exclusion. Their innovations are suppressed or reframed as corruptions.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, adaptive_practitioners, payer,
    powerful, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_transmission__symbol_continuity_reading, adaptive_practitioners, beneficiary).

% Young community members who want to participate in the ritual's core function—honoring the dead, maintaining communal bonds—but cannot afford the time, resource, or psychological cost of strict adherence to inherited form. They propose hybrid approaches: some traditional elements, some modernized ones. They face social pressure, exclusion from leadership roles, and accusations of forgetting. Leaving the community entirely is their primary unconstrained exit, but it severs their identity.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, generational_innovators, payer,
    moderate, biographical, constrained, local).

% Document the constraint from outside the community: historians, anthropologists, religious scholars who examine whether the rigid form-preservation actually serves memory transmission or whether it primarily maintains institutional hierarchy within the community. They collect testimony from all seats and assess what survival capacity or knowledge the ritual actually encodes and transmits.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, external_analysts, observer,
    analytical, generational, analytical, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_transmission__symbol_continuity_reading, communal_identity_guardians).
narrative_ontology:fixing_cost_class(catastrophe_memory_transmission__symbol_continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ritual practice maintains emotional bonds among community members across generations; the shared performance of symbolic action creates synchronization and mutual recognition. Mourning-practice channels grief into socially contained, collectively meaningful activity rather than isolated suffering. The coordination problem solved: 'How does a community maintain identity and remember shared catastrophe without dissolving into individual trauma or institutional chaos?'
% TRANSFER_FUNCTION: Transfers time, labor, economic opportunity, and adaptive capacity FROM younger and adaptive practitioners TO the identity-guardian institutions and the abstract claim that 'form fidelity = survival.' The extraction is enforced through identity-lock (belonging requires obedience) and social exclusion (deviation = betrayal).
% ABSENT_VOICES: Descendants of catastrophe survivors who have left the community or been excluded for ritual innovation are structurally absent from the decision space. They would testify that memory survives adaptation and that rigid form can become traumatic re-enactment rather than healing. Their testimony would reframe the constraint as possible identity-capture rather than identity-preservation.
% DISAPPEARANCE_RATIONALE: Guardians would say ritual form is the ONLY mechanism that preserves memory and prevents dissolution into forgetting and identity-loss—without strict fidelity, the dead are truly lost. Adaptive practitioners would say the core mourning function, emotional bonding, and mnemonic power survive and possibly strengthen with adaptation; rigid form may actually damage psychological health and community cohesion over time. External analysts note that catastrophe memory does persist in communities with more adaptive rituals, but also that some rigidly-transmitted rituals do embed survival knowledge that casual observers miss.
% FOUNDING_PROBLEM: After a collective catastrophe (war, genocide, famine, displacement), the community faces two simultaneous threats: (1) the dead will be forgotten and the catastrophe's meaning erased from collective memory; (2) survivors face psychological trauma and fractured identity. Ritual form—prescribed actions, symbolic objects, repeated sequences—serves as an external storage system for memory (written in practice, not in words) and as a therapeutic container for grief. The founding problem: 'How do we ensure the next generation knows what happened, honors the dead, and maintains identity as a community of survivors?'
% FOUNDING_PROBLEM_CORROBORATION: Identity guardians attest the founding problem is perpetually live—each new generation risks forgetting if ritual form is not enforced. Adaptive practitioners and some survivors' descendants attest the founding problem (genuine catastrophe memory loss, severe intergenerational trauma) is substantially addressed by any form of consistent, emotionally authentic remembrance practice; rigid form may address only the institutional need to control narrative, not the survivors' psychological need to heal. Historical and anthropological literature from outside the benefiting parties documents cases of ritual adaptation that preserved both memory fidelity and psychological resilience, and cases of rigid transmission that calcified into trauma re-enactment.
narrative_ontology:disappearance_verdict(catastrophe_memory_transmission__symbol_continuity_reading, contested).
narrative_ontology:founding_problem_status(catastrophe_memory_transmission__symbol_continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_transmission__symbol_continuity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_transmission__symbol_continuity_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_transmission__symbol_continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_transmission__symbol_continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_transmission__symbol_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.68) because the constraint systematically transfers adaptive capacity and generational freedom to the abstract goal of form-fidelity, enforced through identity-lock rather than external coercion alone. Suppression is high (0.72) because the primary enforcement mechanism is the threat of identity-loss and community exclusion—'deviate and you are no longer one of us.' Theater ratio climbs from 0.35 to 0.58 over the 40-unit interval, indicating that the proportion of ritual activity devoted to maintaining form-fidelity (rather than genuine mourning or psychological processing) increases over time as adaptive pressure mounts. The measurements show extractiveness and suppression plateauing in the later interval (25–40), suggesting the constraint has stabilized at a quasi-equilibrium: enough younger members conform to keep the institution intact, but enough quietly leave or minimize participation that the community gradually shrinks or bifurcates. Accessibility_collapse (0.79) is high because once a younger member internalizes 'identity = form fidelity,' alternatives feel like identity-death; the collapse is partly structural (institutional exclusion) and partly internalized (identity fusion). Resistance (0.43) is moderate because adaptive practitioners and younger members resist through quiet non-compliance, selective participation, and innovation proposals, but they lack the institutional authority or external legal standing to force change.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (identity guardians) and payer (adaptive practitioners, younger participants) should compute to different types from the same constraint data. From the guardian seat, the arrangement is genuine identity-coordination—a real collective need solved through enforced form-fidelity. From the payer seats, the same structure operates as identity-capture: the institution leverages identity-lock to extract conformity that benefits the institution more than it serves the stated goal of memory transmission. The engine derives directionality separately for each seat from the structural data (who benefits, who bears costs, what exits are available); the authored claim does NOT adjudicate which seat's perception is correct—that divergence IS the measurement the corpus exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Identity guardians (organized power, identity-locked) are structural beneficiaries: they control the definition of authentic practice, hold institutional authority, and benefit from every enforcement action. Younger participants and generational innovators (moderate power, identity-locked) are structural targets: they cannot exit without identity-loss, bear the time/economic/opportunity cost, and are sanctioned for dissent. Adaptive practitioners (powerful, constrained) sit closer to the target end despite their power: their innovations are suppressed regardless of merit, they face institutional exclusion if they push too hard, and their adaptive capacity is the primary thing the constraint extracts. Directionality overrides are not needed here; the structural derivation captures the asymmetry: beneficiary → low d (subsidy), payer/excluded → high d (extraction).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates the possibility of mandatrophy: the founding problem (catastrophe memory loss, intergenerational trauma) may be substantially addressed by consistent, emotionally authentic remembrance practice in any form; yet the institutional enforcement of rigid form persists and intensifies (theater_ratio climbing from 0.35 to 0.58) even as the founding problem's acute phase passes. The constraint's mandate—prevent catastrophe memory loss—may have been genuinely necessary for 1–2 generations after the catastrophe but becomes increasingly ceremonial and identity-maintenance-driven rather than survival-driven as time passes and new generations face non-catastrophe conditions. The theater ratio climb is the diagnostic signal: an increasing share of ritual activity is devoted to maintaining institutional control of form (and punishing deviation) rather than to psychological mourning or transmission of survival knowledge. This is a candidate for mandatrophy resolution: if evidence shows that memory transmission and intergenerational bonding occur equally well in adapted ritual forms (as the sibling hybrid_embedded_reading claims), then the current form-fidelity requirement is orphaned purpose maintained by institutional inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    structural_vs_internalized_suppression,
    'Is the measured suppression (0.72) primarily structural (institutional exclusion, material sanctions) or primarily internalized (identity fusion such that deviation feels like self-erasure)?',
    'Longitudinal interview with members who leave the community: do they report continued suppression post-exit, or does pressure ease once structural enforcement is removed? If suppression persists post-exit (psychological, identity-based), it is partly internalized.',
    'If largely internalized, the constraint''s effective suppression is higher than the structural measure; if structural, external remedies (rule changes, alternative institutions) could weaken it. Identity-internalized suppression makes exit more costly psychologically even after institutional pressure ceases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_vs_internalized_suppression, empirical, 'Whether suppression is structural exclusion or internalized identity-fusion').

omega_variable(
    form_fidelity_vs_memory_transmission_decoupling,
    'Is strict form-fidelity actually necessary for catastrophe memory transmission and psychological healing, or is the connection between form and outcome primarily a post-hoc narrative justification for institutional control?',
    'Comparative study of communities with rigid ritual forms versus those with adaptive forms, tracking memory accuracy, intergenerational trauma recovery, and community cohesion metrics. If memory and healing outcomes are equivalent or better in adaptive communities, the form-fidelity requirement is revealed as institution-serving rather than survivor-serving.',
    'If decoupled, the constraint shifts from tangled_rope (genuine coordination + extraction) to snare (pure extraction using coordination justification). Mandatrophy resolution would follow: founding problem is solved, but enforcement persists because it benefits institutional power-holders.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(form_fidelity_vs_memory_transmission_decoupling, empirical, 'Whether form-fidelity is a necessary condition for memory transmission or a cover story for institutional control').

omega_variable(
    identity_lock_mechanism,
    'What specific identity-fusion mechanism binds younger participants and adaptive practitioners? Is it professional identity (ritual keeper as career), relational identity (self-concept constituted through community belonging), ideological identity (worldview where form-fidelity is non-negotiable), or institutional identity (the organization has ''become'' the person''s identity)?',
    'Ethnographic interview probing the subjective experience of identity-loss if ritual form changes: what exactly would be lost? Which loss is most painful? This identifies the fusion mechanism.',
    'Different fusion mechanisms suggest different intervention points. Institutional identity loss is most easily addressed by creating alternative institutions; relational identity loss requires community reform; ideological fusion requires ideological reframing (the hardest).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'The specific mechanism binding participants to form-fidelity via identity').

omega_variable(
    kernel_reading_contest,
    'This reading treats identity-preservation as the primary function and form-fidelity as its vehicle. Are the sibling readings (operational_competence and hybrid_embedded) genuinely alternative readings of the same kernel, or are they fundamentally different constraints that happen to share a domain (catastrophe ritual)?',
    'Examine whether all three readings can be held within the same community''s framework (the coexists_with relation) or whether holding one reading forecloses the other (the forecloses relation). If communities can coherently hold all three simultaneously, they are coexisting readings. If one precludes the others, the readings carve at different joints and may represent different constraints.',
    'If the readings genuinely coexist, this story and the sibling stories are three readings of one kernel, linked by network.affects_constraints. If they foreclose each other, the ''catastrophe memory transmission'' label is ambiguous and should be split into three separate constraints. This affects how the corpus models constraint families.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether the three kernel readings are alternative framings of one constraint or fundamentally different constraints').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_transmission__symbol_continuity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(cata_tr_t0, observed).
narrative_ontology:measurement(cata_tr_t5, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 5, 0.41).
narrative_ontology:measurement_basis(cata_tr_t5, observed).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 10, 0.48).
narrative_ontology:measurement_basis(cata_tr_t10, observed).
narrative_ontology:measurement(cata_tr_t15, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 15, 0.52).
narrative_ontology:measurement_basis(cata_tr_t15, observed).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 20, 0.56).
narrative_ontology:measurement_basis(cata_tr_t20, observed).
narrative_ontology:measurement(cata_tr_t25, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 25, 0.57).
narrative_ontology:measurement_basis(cata_tr_t25, observed).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 30, 0.58).
narrative_ontology:measurement_basis(cata_tr_t30, observed).
narrative_ontology:measurement(cata_tr_t35, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 35, 0.58).
narrative_ontology:measurement_basis(cata_tr_t35, observed).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 40, 0.58).
narrative_ontology:measurement_basis(cata_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(cata_be_t0, observed).
narrative_ontology:measurement(cata_be_t5, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement_basis(cata_be_t5, observed).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement_basis(cata_be_t10, observed).
narrative_ontology:measurement(cata_be_t15, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 15, 0.64).
narrative_ontology:measurement_basis(cata_be_t15, observed).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(cata_be_t20, observed).
narrative_ontology:measurement(cata_be_t25, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement_basis(cata_be_t25, observed).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(cata_be_t30, observed).
narrative_ontology:measurement(cata_be_t35, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement_basis(cata_be_t35, observed).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(cata_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(cata_su_t0, observed).
narrative_ontology:measurement(cata_su_t5, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 5, 0.61).
narrative_ontology:measurement_basis(cata_su_t5, observed).
narrative_ontology:measurement(cata_su_t10, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement_basis(cata_su_t10, observed).
narrative_ontology:measurement(cata_su_t15, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement_basis(cata_su_t15, observed).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(cata_su_t20, observed).
narrative_ontology:measurement(cata_su_t25, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(cata_su_t25, observed).
narrative_ontology:measurement(cata_su_t30, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(cata_su_t30, observed).
narrative_ontology:measurement(cata_su_t35, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 35, 0.72).
narrative_ontology:measurement_basis(cata_su_t35, observed).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(cata_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_transmission__symbol_continuity_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_transmission__symbol_continuity_reading, 0.12).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__symbol_continuity_reading, catastrophe_memory_transmission__operational_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__symbol_continuity_reading, catastrophe_memory_transmission__hybrid_embedded_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a contested kernel (catastrophe_memory_transmission). The kernel—ritual transmits catastrophe memory and identity—is instantiated by three structurally distinct constraint stories: (1) symbol_continuity_reading (this file): identity is preserved through strict form-fidelity; adaptive capacity is the victim. (2) operational_competence_reading: survival knowledge is encoded in ritual form; the constraint transmits competence, not just identity. (3) hybrid_embedded_reading: competence and form are inseparable; the reading claims they cannot be decomposed. The three readings have different ε values, different beneficiary/victim structures, and different classification outcomes. They are linked via network.affects_constraints to enable cross-reading analysis of how different committer assumptions reshape the constraint's structural character.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_transmission__symbol_continuity_reading, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
