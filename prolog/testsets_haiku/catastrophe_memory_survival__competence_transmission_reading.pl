% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_survival__competence_transmission_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: catastrophe_memory_survival__competence_transmission_reading
 *   human_readable: Ritual as Practical Survival Knowledge Transmission
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   Ritual practice in diaspora and post-displacement communities encodes
 *   practical survival knowledge: timing of seasonal resources, hierarchy of
 *   family obligations during scarcity, adaptation strategies to ecological
 *   and economic change. This reading claims ritual FUNCTIONS primarily as a
 *   competence-transmission system, not as pure identity maintenance or
 *   symbolic boundary-marking. The constraint is CLAIMED as tangled_rope
 *   (genuine coordination problem + asymmetric extraction) because it solves
 *   a real knowledge-loss problem while concentrating authority over
 *   interpretation in ritual specialists and cultural custodians, who become
 *   indispensable mediators. The beneficiaries (diaspora communities who gain
 *   adaptive knowledge, knowledge holders who gain authority) are
 *   structurally distinct from the payers (communities losing practical
 *   content, younger members cut off from knowledge without permission,
 *   communities whose ecological contexts vanished). The claim/metric
 *   divergence is intentional: the constraint is defended in coordination
 *   language ('we preserve survival knowledge') while extracting through
 *   gatekeeping and authority concentration. This is not rope (pure
 *   coordination) because the knowledge could in principle be transmitted
 *   through technical documentation or explicit instruction, and the ritual
 *   form's persistence depends on maintaining the authority structure, not
 *   solely on its coordination function.
 *
 * KEY AGENTS:
 *   - Diaspora communities: beneficiary, moderate power, identity-locked (cannot abandon ritual without cultural dissolution)
 *   - Communities losing practical content: payer, powerless, trapped (maintain ritual form as identity but lose function as ecological context changes)
 *   - Adaptive knowledge holders / ritual specialists: beneficiary + agenda_setter, powerful, arbitrage exit (hold authority over interpretation and transmission)
 *   - Younger generation: payer, powerless, constrained exit (participate due to cultural obligation but lack automatic access to practical knowledge)
 *   - Colonizing authorities (excluded): institutional power, trapped exit (structurally implicated in making ecological contexts inaccessible but excluded from discussion of what rituals carry)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_survival__competence_transmission_reading, 0.58).
domain_priors:suppression_score(catastrophe_memory_survival__competence_transmission_reading, 0.41).
domain_priors:theater_ratio(catastrophe_memory_survival__competence_transmission_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_survival__competence_transmission_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_survival__competence_transmission_reading, "Ritual as Practical Survival Knowledge Transmission").
narrative_ontology:topic_domain(catastrophe_memory_survival__competence_transmission_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_survival__competence_transmission_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_survival__competence_transmission_reading, 'ff64aed9-76a9-4adb-9a0a-6b00d8f62306').
narrative_ontology:cs_kernel_codification('ff64aed9-76a9-4adb-9a0a-6b00d8f62306', distributed).
narrative_ontology:cs_authority_grounding('ff64aed9-76a9-4adb-9a0a-6b00d8f62306', practice).
narrative_ontology:cs_interpretation_layer_present('ff64aed9-76a9-4adb-9a0a-6b00d8f62306').
narrative_ontology:cs_reading_relation('ff64aed9-76a9-4adb-9a0a-6b00d8f62306', catastrophe_memory_survival__symbol_survival_reading, coexists_with).
narrative_ontology:cs_reading_relation('ff64aed9-76a9-4adb-9a0a-6b00d8f62306', catastrophe_memory_survival__hybrid_encoding_reading, influences).
narrative_ontology:cs_axiom('ff64aed9-76a9-4adb-9a0a-6b00d8f62306', foundational, practical_knowledge_functionally_separable_from_symbol).
narrative_ontology:cs_axiom_status(practical_knowledge_functionally_separable_from_symbol, holdable).
narrative_ontology:cs_axiom_grounding('ff64aed9-76a9-4adb-9a0a-6b00d8f62306', practical_knowledge_functionally_separable_from_symbol, empirically_contingent).
narrative_ontology:cs_axiom('ff64aed9-76a9-4adb-9a0a-6b00d8f62306', foundational, survival_defined_as_adaptive_competence_access).
narrative_ontology:cs_axiom_status(survival_defined_as_adaptive_competence_access, holdable).
narrative_ontology:cs_axiom_grounding('ff64aed9-76a9-4adb-9a0a-6b00d8f62306', survival_defined_as_adaptive_competence_access, instrumental).
narrative_ontology:cs_reference_frame('ff64aed9-76a9-4adb-9a0a-6b00d8f62306', ritual_knowledge_transmission_system).
narrative_ontology:cs_drift_state('ff64aed9-76a9-4adb-9a0a-6b00d8f62306', post_displacement_diaspora_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ff64aed9-76a9-4adb-9a0a-6b00d8f62306', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_survival__competence_transmission_reading, catastrophe_memory_survival).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__competence_transmission_reading, diaspora_communities).
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__competence_transmission_reading, adaptive_knowledge_holders).
narrative_ontology:constraint_victim(catastrophe_memory_survival__competence_transmission_reading, communities_losing_practical_content).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(catastrophe_memory_survival__competence_transmission_reading, younger_generation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Communities displaced from ancestral territories or original ecological contexts who maintain ritual practice in diaspora. Derive adaptive survival knowledge from rituals originally encoded for specific environmental and resource conditions: timing of agricultural cycles, seasonal migration patterns, resource management hierarchies, kinship obligations during scarcity. The ritual form survives intact; the practical content it carries is now their primary inheritance of how to navigate displacement and resource scarcity.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, diaspora_communities, beneficiary,
    moderate, generational, identity_locked, global).

% Communities remaining in or returning to ancestral territories where the original ecological and resource conditions that generated the ritual's practical content have changed or vanished (environmental collapse, economic restructuring, technological replacement of traditional practices). They maintain the ritual form as identity and boundary marker but lose the practical knowledge embedded in it because the conditions it addresses no longer exist or are no longer visible as relevant. Their exit from ritual practice would mean cultural dissolution; their participation yields form without function.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, communities_losing_practical_content, payer,
    powerless, biographical, trapped, local).

% Scholars, ritual specialists, diaspora leaders, and cultural custodians who recognize and actively teach the practical knowledge embedded in ritual. They hold authority over interpretation and transmission, curating which practical lessons are highlighted versus which are permitted to fade as 'symbolic only.' They benefit from the constraint's operation by establishing themselves as indispensable interpreters: the community cannot access the survival knowledge without their mediation and authority.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, adaptive_knowledge_holders, beneficiary,
    powerful, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_survival__competence_transmission_reading, adaptive_knowledge_holders, agenda_setter).

% Historical and contemporary power structures that suppressed or displaced ritual practice as superstition, inefficiency, or cultural primitiveness. They are excluded from the conversation about ritual's value but remain structurally implicated: many communities lost the ecological contexts in which rituals' practical content made sense because of colonial resource extraction, environmental destruction, or imposed economic systems. Their exclusion means the constraint operates without addressing how external power made practical knowledge inaccessible in the first place.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, colonizing_authorities, excluded,
    institutional, generational, trapped, global).

% Diaspora and heritage community members socialized primarily in displacement contexts, not in the ecological conditions the rituals originally addressed. They participate in rituals because of cultural obligation and identity, but the practical survival knowledge is increasingly opaque to them without explicit teaching. They bear the cost of participation (time, labor, conformity) without automatic access to the knowledge unless more powerful knowledge holders choose to transmit it explicitly rather than treat the content as 'understood' or 'secret.'
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, younger_generation, payer,
    powerless, biographical, constrained, local).

% External analytical position: tracks whether ritual is functioning as knowledge transmission system or as identity boundary maintenance system, and whether loss of practical content represents loss of adaptive capacity or successful symbolic preservation of cultural continuity.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, anthropological_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_survival__competence_transmission_reading, adaptive_knowledge_holders).
narrative_ontology:fixing_cost_class(catastrophe_memory_survival__competence_transmission_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Encodes and transmits survival knowledge across generations and displacement: timing of resource availability, management of scarcity, family protocols for mutual aid, adaptation strategies for environmental or economic change. Solves the problem of preserving actionable knowledge that would be lost if transmitted only through direct ecological experience or technical instruction.
% TRANSFER_FUNCTION: Moves practical survival competence from elder knowledge holders (or from historical contexts) to younger and displaced community members, and distributes authority over that knowledge to ritual specialists and cultural custodians who become indispensable interpreters. The transfer is conditional: younger members who do not participate in ritual, or who question the knowledge holders' authority, are cut off from the practical content.
% ABSENT_VOICES: Communities whose practical knowledge was destroyed by colonialism, environmental destruction, or economic displacement cannot testify to what their rituals once carried because the knowledge is gone. Scholars of indigenous resilience, historians of ecological collapse, and communities attempting to recover lost practices would argue that understanding ritual requires examining the material conditions that made the knowledge legible in the first place.
% DISAPPEARANCE_RATIONALE: If ritual as knowledge-transmission system disappeared, diaspora communities would lose a structured channel for intergenerational transfer of survival strategies adapted to displacement and scarcity. Younger members would rely on explicit technical instruction or rediscovery from fragments, with higher losses of tacit knowledge. Communities in ancestral territories would face choice between abandoning ritual entirely (cultural dissolution) or reinventing it without the practical content. The social arrangement around who holds authority over knowledge interpretation would break.
% FOUNDING_PROBLEM: Survival knowledge embedded in ecological and social practices risks total loss across displacement, ecological change, or generational discontinuity. Direct instruction and formal documentation fail for knowledge that is tacit, context-dependent, or embedded in experience. Ritual encodes that knowledge in a form that is both structurally memorable (repeated action, narrative, sensory pattern) and portable (can be performed anywhere).
% FOUNDING_PROBLEM_CORROBORATION: Diaspora communities testify to survival knowledge retrieved from ritual practice. Historians of colonialism and environmental destruction document the loss of practical knowledge when ecological contexts vanished. Linguists and cultural practitioners outside the benefiting communities note that ritual knowledge transmission occurs, though some question whether the content survives intact or is increasingly ceremonial. No external verification from those who abandoned ritual practice exists; communities that lost the knowledge cannot attest to what they once had.
narrative_ontology:disappearance_verdict(catastrophe_memory_survival__competence_transmission_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_survival__competence_transmission_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_survival__competence_transmission_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_survival__competence_transmission_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_survival__competence_transmission_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness rises over the 50-unit interval (0.35 → 0.58) as diaspora context deepens: in early displacement, practical knowledge is still recoverable from elders with living memory of ancestral conditions; over generations, the knowledge becomes more opaque and the gatekeeping by knowledge holders becomes more necessary and more extractive. Theater rises alongside (0.22 → 0.38) because explanation and interpretation increasingly replace direct transmission: knowledge holders must now teach 'what this ritual means for resource management' rather than having younger members absorb it through participation in original contexts. Suppression is moderate (0.41 at endpoint) because the constraint relies partly on identity-lock (younger members cannot leave ritual without leaving community) and partly on active gatekeeping (knowledge holders restrict who learns the practical content, when, and under what conditions). Accessibility collapse is moderate (0.62) because while alternatives exist (anthropological documentation, technical instruction, ecological experimentation), the identity-locked exit makes ritual participation mandatory even if alternatives are available. Resistance is substantial (0.52) because younger generation and some community scholars question whether ritual is still the best knowledge-transmission method in diaspora, and because some communities reject the knowledge-holder gatekeeping model. The tangled_rope claim is grounded in this structure: genuine coordination (survival knowledge IS preserved through ritual when other systems fail), genuine enforcement (younger members must participate; knowledge holders control access), genuine extraction (authority concentration, gatekeeping, symbolic performance increasingly substituting for explicit teaching).
 *
 * PERSPECTIVAL GAP:
 *   From the knowledge-holder seat, the arrangement is genuine coordination: 'we preserve knowledge that would otherwise vanish.' From the younger-generation seat, the arrangement is gatekeeping: 'I must participate in ritual to stay in community, but I don't understand what the practical content is unless the knowledge holder chooses to teach me explicitly, and their interpretation may differ from what the knowledge actually carried.' From the communities-losing-content seat, the constraint is partly vestigial theater: 'we maintain the ritual form because it is identity, but the practical content no longer applies to our ecological context, and the knowledge holders treat it as still relevant even though our survival no longer depends on those strategies.' The engine computes these as per-seat classifications from power, exit_options, and beneficiary/victim declarations; the divergence is the analytical payload.
 *
 * DIRECTIONALITY LOGIC:
 *   Diaspora communities and knowledge holders sit near the beneficiary end (d ≈ 0.15–0.25): they gain survival competence and authority respectively. Communities losing practical content and younger generation sit near the target end (d ≈ 0.75–0.85): they bear the cost of participation without guaranteed access to the knowledge, and must navigate the authority structure. The younger generation faces a particularly extractive position because they are trapped by identity and have constrained alternatives — even if the knowledge is theoretically available, accessing it requires deference to knowledge holders. Colonial authorities are structurally implicated but excluded, so their directionality is not computed in the main seat analysis; they sit outside the constraint's operational boundary.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem ('survival knowledge risks loss across displacement') is live but increasingly contested. Early post-displacement, the problem is urgent and the solution is clear: ritual IS the knowledge-transmission system that works. After 2–3 generations in diaspora, the problem becomes ambiguous: is the practical knowledge about resource scarcity still relevant when diaspora members have access to formal markets? Is ritual the best transmission method when documentation exists? The constraint exhibits early signs of mandatrophy (function atrophy, form persistence): theater_ratio rises while extractiveness also rises, suggesting that knowledge transmission is increasingly ceremonial explanation rather than embedded practice, while the gatekeeping authority structure persists. The constraint avoids full piton status because the founding problem is still genuinely live for some seats (communities in extreme precarity still need the knowledge) and because alternatives are not yet fully available (formal documentation of indigenous survival knowledge remains sparse). The tension between 'ritual preserves knowledge communities need' and 'ritual gatekeeping extracts authority without clear functional payoff' is where mandatrophy candidates emerge.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    practical_vs_symbolic_decomposition,
    'Is the practical knowledge embedded in ritual genuinely separable from its symbolic and identity functions, or do the symbol and the knowledge form a single functional unit?',
    'Ethnographic documentation of communities that have retained the ritual form while losing access to the original ecological context (forced displacement, environmental collapse). If practical knowledge decays while symbol persists, the functions are separable; if knowledge and symbol degrade together, they are fused.',
    'If separable, the competence_transmission_reading stands: ritual''s primary function is knowledge preservation and the symbol is the vehicle. If fused, the hybrid_encoding_reading is more structurally accurate and this reading over-represents the knowledge component. This shifts the ε valuation: a separable system with gatekeeping is high-extraction tangled_rope; a fused system whose symbolic and practical dimensions are inseparable might be lower-extractiveness rope (coordination with side-effect asymmetry rather than designed extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(practical_vs_symbolic_decomposition, empirical, 'Whether practical knowledge and symbolic identity are functionally separable in ritual transmission.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (identity-lock exit, gatekeeping by knowledge holders) primarily structural (external barriers to leaving ritual or to accessing knowledge) or internalized (younger members believe they should not know, or that the knowledge is forbidden for them)?',
    'Post-exit trajectory: if younger members who leave ritual practice report that suppression decreases over time after exit, the suppression is internalized; if suppression persists because community sanctions continue, it is structural.',
    'If internalized, the effective suppression is higher than the authored structural measure — the target carries the suppression with them after exit and may not fully reconstruct alternative knowledge access. If structural, remedies are available (open access policies, democratized documentation) that would break the constraint. This affects the terminal classification: structural suppression is still extractive but removable; internalized suppression indicates deeper identity-fusion and suggests the constraint is harder to break.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression mechanisms are external (structural barriers) or internal (internalized belief and identity-lock).').

omega_variable(
    mandatrophy_trajectory_uncertainty,
    'Is the rising theater_ratio an indicator of gradual knowledge loss (mandatrophy: function atrophying while form persists) or of transition to explicit pedagogical methods (theater is explanation, not replacement)?',
    'Cross-generational competence assessment: measure whether younger generation members who participate in ritual can articulate and apply the practical knowledge at rates comparable to prior generations. If rates drop despite equal participation, mandatrophy is underway; if rates hold despite changing transmission methods, the constraint is adapting.',
    'If mandatrophy is underway, the constraint will eventually become a piton (form with atrophied function, persisting by identity obligation). If the constraint is adapting, the theater_ratio rise reflects pedagogical innovation rather than decay. This affects terminal prognosis: an adapting constraint may remain tangled_rope indefinitely; a mandatrophy candidate is on trajectory to piton status.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mandatrophy_trajectory_uncertainty, empirical, 'Whether rising theater ratio indicates knowledge loss or pedagogical adaptation.').

omega_variable(
    colonialism_exclusion_structural_role,
    'Is the exclusion of colonizing authorities merely descriptive (they are not in the conversation), or is it structurally constitutive (the constraint depends on not examining how colonialism created the very material conditions — resource scarcity, displacement, ecological collapse — that make the knowledge transmission problem urgent)?',
    'Counterfactual: if colonizing authorities were included in the conversation and forced to account for their role in creating the knowledge-loss problem, would the constraint''s function change? If yes, the exclusion is structurally constitutive.',
    'If constitutive, the constraint is not purely a coordination solution but a partial cover for a larger structural injustice. The extraction is not just gatekeeping by knowledge holders but complicity with (or silence about) the forces that made survival knowledge precarious in the first place. This would shift the reading toward snare (pure extraction with justice claim) rather than tangled_rope (genuine coordination + asymmetric extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(colonialism_exclusion_structural_role, conceptual, 'Whether the exclusion of colonizing authorities is structural or merely descriptive.').

omega_variable(
    kernel_contest_over_competence_primacy,
    'In the contested kernel ''catastrophe_memory_survival'', which reading is structurally more accurate: does ritual primarily encode practical competence (this reading), identity-symbolism (symbol_survival_reading), or both inseparably (hybrid_encoding_reading)?',
    'Ethnographic and historical documentation of ritual systems before and after displacement, showing which components of ritual knowledge communities actively reconstruct when they have choice. If practical knowledge is reconstructed first, competence_transmission is primary; if identity and boundary-norms are reconstructed first, symbol_survival is primary; if both are reconstructed simultaneously and dependently, hybrid_encoding is primary.',
    'Different readings instantiate different ε values, beneficiary sets, and victim sets. This omega documents that the constraint''s classification is reading-indexed and that sibling readings would produce different classifications from the same kernel. The engine computes per-reading classifications; this omega marks the irreducible contestation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_contest_over_competence_primacy, conceptual, 'Which reading of the catastrophe_memory_survival kernel is structurally primary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_survival__competence_transmission_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 10, 0.26).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 20, 0.31).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 40, 0.37).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 50, 0.38).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 30, 0.56).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 40, 0.57).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 50, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(cata_su_t10, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 10, 0.32).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 20, 0.37).
narrative_ontology:measurement(cata_su_t30, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 30, 0.4).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 40, 0.41).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 50, 0.41).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_survival__competence_transmission_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_survival__competence_transmission_reading, 0.18).
narrative_ontology:affects_constraint(catastrophe_memory_survival__competence_transmission_reading, catastrophe_memory_survival__symbol_survival_reading).
narrative_ontology:affects_constraint(catastrophe_memory_survival__competence_transmission_reading, catastrophe_memory_survival__hybrid_encoding_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel 'catastrophe_memory_survival': ritual as practical knowledge transmission. The hybrid_encoding_reading refuses the separation and claims both symbolic and practical dimensions are necessary and inseparable. The symbol_survival_reading claims the primary function is identity preservation and boundary-maintenance, with practical knowledge as a secondary payload. All three readings share the same kernel (ritual practice across catastrophic disruption) and the same base stakeholders, but author different ε values, different beneficiary/victim relationships, and derive different classifications. The network links show that acceptance of the hybrid or symbol readings would reframe the competence-transmission reading as incomplete or ideologically partial. The competence_transmission_reading influences the others by establishing the empirical question of whether practical knowledge is functionally separable from symbol; resolution of that question would shift the efficacy of all three readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
