% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_function__hybrid_transformation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_function__hybrid_transformation_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: catastrophe_memory_function__hybrid_transformation_reading
 *   human_readable: Catastrophe Memory Function: Hybrid Transformation Reading
 *   domain: religious_studies/ritual_theory/collective_memory
 *
 * SUMMARY:
 *   This constraint is one reading of the contested kernel
 *   catastrophe_memory_function: the hybrid_transformation_reading. Passover
 *   (and analogous catastrophe-commemorative rituals across cultures) encodes
 *   both mourning-practice and survival-competence in a single ritual
 *   structure. The bitter herbs commemorate loss and encode
 *   boundary-maintenance (mourning-practice reading, D1/D4); the seder
 *   performance rehearses emergency governance, resource rationing, and rapid
 *   institutional decentralization (survival-competence reading, D5). This
 *   reading holds that the ritual is functionally hybrid — neither reading
 *   captures the full structure alone. Extractiveness is moderate-low because
 *   the ritual solves genuine coordination problems (how to preserve
 *   loss-memory without psychological collapse; how to transmit survival
 *   protocols) with minimal coercive overhead. The community largely chooses
 *   to participate; identity-lock is the binding mechanism, not external
 *   suppression. The measurement series shows slight rise in extractiveness
 *   (assimilationist pressure increases over the 50-year interval, making
 *   ritual participation harder for second-generation immigrants) and slight
 *   rise in theater ratio (some communities increase commemorative emphasis
 *   while de-emphasizing survival-competence teaching, risking
 *   instrumentalization drift), but both remain low — the ritual's core
 *   function persists.
 *
 * KEY AGENTS:
 *   - ritual_community_participants: Those who enact Passover annually, bearing identity-lock to the ritual; they experience both mourning obligation and inherited survival competence as inseparable.
 *   - future_generations_inheriting_adaptive_capacity: Born into the ritual without choosing it; they are the target of transmission but have no voice in designing it.
 *   - scholarly_interpreters: Analytical seat that identifies both the mourning-practice layer (D1/D4) and survival-competence layer (D5) and their functional interdependence.
 *   - institutional_authorities_maintaining_ritual: Rabbis, teachers, ceremony-leaders who preserve the ritual's formal structure; their enforcement is against degradation, not against the adaptive layer.
 *   - assimilationist_pressure: Institutional voices (government education, secular culture, integration ideology) excluded from the ritual that argue its persistence is atavism; they erode participation across generations.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_function__hybrid_transformation_reading, 0.38).
domain_priors:suppression_score(catastrophe_memory_function__hybrid_transformation_reading, 0.22).
domain_priors:theater_ratio(catastrophe_memory_function__hybrid_transformation_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_function__hybrid_transformation_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_function__hybrid_transformation_reading, "Catastrophe Memory Function: Hybrid Transformation Reading").
narrative_ontology:topic_domain(catastrophe_memory_function__hybrid_transformation_reading, "religious_studies/ritual_theory/collective_memory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_function__hybrid_transformation_reading, '8688508c-0107-426c-8fb3-4952cd0b5dbb').
narrative_ontology:cs_kernel_codification('8688508c-0107-426c-8fb3-4952cd0b5dbb', distributed).
narrative_ontology:cs_authority_grounding('8688508c-0107-426c-8fb3-4952cd0b5dbb', lineage).
narrative_ontology:cs_interpretation_layer_present('8688508c-0107-426c-8fb3-4952cd0b5dbb').
narrative_ontology:cs_reading_relation('8688508c-0107-426c-8fb3-4952cd0b5dbb', catastrophe_memory_function__mourning_practice_reading, coexists_with).
narrative_ontology:cs_reading_relation('8688508c-0107-426c-8fb3-4952cd0b5dbb', catastrophe_memory_function__survival_competence_reading, coexists_with).
narrative_ontology:cs_axiom('8688508c-0107-426c-8fb3-4952cd0b5dbb', foundational, ritual_dual_layer_inseparability).
narrative_ontology:cs_axiom_status(ritual_dual_layer_inseparability, holdable).
narrative_ontology:cs_axiom_grounding('8688508c-0107-426c-8fb3-4952cd0b5dbb', ritual_dual_layer_inseparability, instrumental).
narrative_ontology:cs_axiom('8688508c-0107-426c-8fb3-4952cd0b5dbb', foundational, loss_memory_preservation_through_adaptive_practice).
narrative_ontology:cs_axiom_status(loss_memory_preservation_through_adaptive_practice, holdable).
narrative_ontology:cs_axiom_grounding('8688508c-0107-426c-8fb3-4952cd0b5dbb', loss_memory_preservation_through_adaptive_practice, deontological).
narrative_ontology:cs_reference_frame('8688508c-0107-426c-8fb3-4952cd0b5dbb', catastrophe_responsive_ritual_transmission).
narrative_ontology:cs_drift_state('8688508c-0107-426c-8fb3-4952cd0b5dbb', contemporary_assimilationist_pressure, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8688508c-0107-426c-8fb3-4952cd0b5dbb', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_function__hybrid_transformation_reading, catastrophe_memory_function).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__hybrid_transformation_reading, community_transmitting_ritual).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__hybrid_transformation_reading, future_generations_inheriting_adaptive_capacity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__hybrid_transformation_reading, ritual_community_participants).
narrative_ontology:constraint_vindicates(catastrophe_memory_function__hybrid_transformation_reading, ritual_multivalence_thesis).
narrative_ontology:constraint_vindicates(catastrophe_memory_function__hybrid_transformation_reading, commemorative_transmission_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enact the ritual annually, preserving both the commemorative obligation to ancestors (mourning-practice encoding) and the practical capacity for survival under repeated institutional stress (seder as rehearsal for emergency governance, food security under constraint, identity persistence through adversity). Participation is deeply identity-constituted; exiting the ritual would mean abandoning the framework through which their community understands itself and transmits its capacity to survive.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, ritual_community_participants, beneficiary,
    moderate, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_function__hybrid_transformation_reading, ritual_community_participants, agenda_setter).

% Analyze ritual structure and transmission logic. They see Passover as encoding survival protocols (family-based coordination, resource rationing, institutional improvisation, rapid decentralization) embedded inside commemorative forms. Their analytical seat allows them to identify both layers and their functional interdependence.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, scholarly_interpreters, observer,
    moderate, biographical, mobile, global).

% Receive the ritual as an inherited mechanism that carries survival protocols in commemorative form. They cannot choose whether the ritual was designed as dual-function; they encounter it as a fact of their inheritance. If the ritual attenuates, the adaptive capacity it encodes is lost.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, future_generations_inheriting_adaptive_capacity, beneficiary,
    powerless, civilizational, trapped, regional).

% Preserve the ritual's formal structure and teach its commemorative narrative. They may or may not explicitly recognize the survival-competence layer (some teachings foreground it; others treat it as secondary or emergent). Their enforcement is primarily against ritual degradation and commemoration-practice abandonment, not against the adaptive layer.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, institutional_authorities_maintaining_ritual, agenda_setter,
    organized, generational, constrained, regional).

% Would argue that ritual persistence is atavism or separatism; that survival capacity should be transmitted through mainstream institutional channels, not encoded in commemorative forms. This voice is structurally excluded from the ritual itself — the ritual's persistence does not depend on their agreement, but pressure from this direction can erode participation (especially in second-generation communities).
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, assimilationist_pressure, excluded,
    institutional, generational, trapped, national).

% The loss the ritual commemorates (enslavement, exile, persecution, catastrophe) is not an agent but a historical fact that structures the ritual's commemoration obligation. It cannot object to being commemorated, but its memory is preserved only through ritual transmission.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__hybrid_transformation_reading, historical_loss_itself, excluded,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_function__hybrid_transformation_reading, historical_loss_itself).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_function__hybrid_transformation_reading, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_memory_function__hybrid_transformation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The ritual simultaneously solves two coordination problems: (1) how to preserve the memory of catastrophic loss across generations without psychologically breaking the community (mourning-practice + boundary-norms, D1/D4 layer), and (2) how to transmit survival protocols and institutional-resilience mechanisms embedded in that same commemorative structure (seder as rehearsal for emergency governance, resource rationing under constraint, rapid decentralization, D5 layer). Both layers are solved by encoding survival competence as an obligation to remember.
% TRANSFER_FUNCTION: The ritual moves cognitive and procedural capacity forward across time: the memory of loss (non-fungible, cannot be replaced by historical text alone), the embodied practice of mourning-community boundary-maintenance, and the implicit competence for survival under institutional stress. It transfers these from those who lived through the catastrophe or its direct aftermath to those born into its aftermath, binding future generations into both the obligation to remember and the inherited capacity to survive.
% ABSENT_VOICES: Assimilationist voices are excluded from the ritual frame itself — they would argue that survival capacity should be transmitted through secular institutional channels and that commemorative encoding is redundant or culturally regressive. Voices within the community that emphasize pure mourning (rejecting survival-competence transmission as instrumentalizing loss) or pure survival technique (rejecting the mourning obligation as emotional burden) would dispute the hybrid reading but remain inside the community and the ritual.
% DISAPPEARANCE_RATIONALE: If this ritual disappeared, the community would lose not merely a commemorative observance but a dual-encoded transmission mechanism. The mourning-practice and boundary-norm layer would shift to other forms (narrative, silence, alternative ceremonies), but the survival-competence layer — which is encoded as implicit, embodied procedure within the ritual structure — would become tacit knowledge dependent on explicit textual or institutional transmission. Communities that have lost the ritual report degraded capacity to coordinate under institutional stress and diluted boundary-identity across generations. The adaptive function does not disappear, but its transmission becomes less reliable and less intergenerational.
% FOUNDING_PROBLEM: Two problems crystallized together: (1) how to preserve the memory of catastrophic loss in a form that allows communities to continue living and transmitting their identity (purely narrative commemoration risks psychological paralysis; purely forward-looking survival focus risks erasing loss), and (2) how to encode survival protocols developed in catastrophe (rapid decentralization, family-based coordination, resource rationing, identity preservation under persecution) so that they are available if similar stress recurs. The ritual embeds both solutions in a single structure: bitter herbs commemorate the loss (D1/D4 mourning layer) while the seder performance rehearses survival competence (D5 adaptive layer).
% FOUNDING_PROBLEM_CORROBORATION: Scholarly analysis of ritual structure and transmission (Frazer, Turner, Bloch on ritual's adaptive function; Connerton on bodily practices and memory) corroborates the dual-layer hypothesis from outside the community. Historical analysis of communities that maintained the ritual through persecution and institutional stress (Ottoman, Eastern European, North African diasporas) attests to the survival-competence transmission layer. Communities that abandoned or attenuated the ritual report demographic integration and loss of institutional resilience in subsequent generations. Voices within the ritual-maintaining community themselves debate whether the survival layer is original design or post-hoc interpretation — that contestation is part of the reading structure itself (see omega variables). Assimilationist voices dispute that the ritual is necessary for either mourning or survival, but they do not deny that the ritual encodes both functions.
narrative_ontology:disappearance_verdict(catastrophe_memory_function__hybrid_transformation_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_function__hybrid_transformation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_function__hybrid_transformation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_function__hybrid_transformation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_function__hybrid_transformation_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_function__hybrid_transformation_reading_tests).
:- end_tests(catastrophe_memory_function__hybrid_transformation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.38 at interval end, moderate but not high, because the ritual solves genuine dual coordination problems and the community largely accepts both layers. The mourning-practice reading solves the psychological problem of preserving catastrophic loss (which would be lost if treated purely as historical narrative). The survival-competence reading solves the transmission problem of embodying protocols for institutional resilience (which attenuates if transmitted only as explicit text). Neither problem has a cheaper solution that preserves both functions. Suppression is low (0.22) because the primary binding mechanism is identity-lock, not external coercion — participants treat the ritual as constitutive of their group identity, not as a burden imposed on them. Accessibility collapse is high (0.68) because exiting the ritual means exiting the community itself, in the perception of identity-locked participants. Resistance is moderate (0.42) because assimilationist pressure does erode participation across generations, but the core community continues to renew commitment. Theater ratio is low (0.15) because the ritual's functional content (mourning, survival rehearsal) is genuine; the rise over the interval reflects some drift toward purely commemorative emphasis in communities under higher assimilationist pressure, but the survival-competence teaching remains embedded in the seder structure itself. The measurement grid shows stabilization after t=40: extractiveness plateaus, theater ratio stabilizes at low level, suppression requirement stabilizes — the ritual finds a steady-state operation in the communities that maintain it.
 *
 * PERSPECTIVAL GAP:
 *   The ritual_community_participants and the institutional_authorities_maintaining_ritual both perceive the constraint as coordination (solving the dual problem of loss-memory and survival capacity), though they may weight the two layers differently. Future_generations_inheriting_adaptive_capacity experience it as an inherited mechanism they did not choose (higher d-value toward target, since they bear the obligation to remember without having voted for it). Assimilationist_pressure perceives it as redundant or culturally regressive (excluded voice, no seat at the constraint table). Scholarly_interpreters perceive it as a functional hybrid that neither purely mourning nor purely survival readings capture. The engine should compute these divergent d-values from the structural data: high d for those identity-locked into participation (near-zero exit), lower d for those whose participation is chosen and mobile, zero or inverted d for beneficiaries (future generations inherit adaptive capacity without bearing the cost directly, though they inherit obligation).
 *
 * DIRECTIONALITY LOGIC:
 *   ritual_community_participants carry high identity_locked exit (cannot leave without leaving the community), giving them high d toward the target end despite their role as beneficiary (they choose the ritual, but exit is psychologically catastrophic). future_generations_inheriting_adaptive_capacity carry trapped exit (born into the obligation; they receive the adaptive capacity but had no choice in the arrangement), giving them the highest d-value toward the target end (they are beneficiaries of transmission but targets of obligation). institutional_authorities_maintaining_ritual carry moderate d (powerful role in enforcing ritual structure but constrained exit — they cannot abandon the tradition without losing authority; they are partly beneficiary, partly bound). scholarly_interpreters carry analytical exit (mobile, external, no identity-lock), giving them low/inverted d-values (beneficiaries of analytical access without bearing the ritual's burden). assimilationist_pressure is excluded, so no d-value applies, but their pressure works by increasing the cost of identity-locked participation over time (captured in the suppression_requirement measurements).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is contested (status=contested): the ritual's core function is to preserve loss-memory AND transmit survival competence. Some communities emphasize the mourning-practice reading and de-emphasize survival teaching; others emphasize survival competence and risk instrumentalizing loss. This reading holds that the founding problem persists (status should be live) because: (1) loss-memory remains under threat of psychological erasure or historical abstraction in each generation, and (2) institutional resilience capacity remains under threat of attenuation if transmitted only as explicit text rather than embodied practice. The ritual persists because it solves both problems simultaneously in a way that separate mechanisms cannot. Mandatrophy would arise if the ritual's core function (dual-layer transmission) were replaced by pure memorialization (losing survival competence) or pure competence training (losing loss-memory). The measurement series shows slight mandatrophy pressure (theater_ratio rising, suggesting increasing emphasis on pure commemoration), but the constraint has not crossed the threshold where one layer dominates and the other becomes vestigial.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dual_layer_original_design_vs_emergent,
    'Is the ritual''s dual-layer structure (mourning + survival competence) an original design feature of the founding catastrophe response, or an emergent property that accumulated through interpretation and transmission?',
    'Textual-historical analysis of the earliest ritual prescriptions and their rationales; ethnographic comparison with other catastrophe-commemorative rituals across cultures; interview analysis of communities that explicitly teach both layers vs. those that teach primarily one.',
    'If original design, the reading gains authority as faithful transmission of founding intent. If emergent, the reading gains authority as recognition of a functional discovery made through practice. Either resolution supports the hybrid reading; what differs is the genealogy (design intent vs. evolved functionality).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dual_layer_original_design_vs_emergent, empirical, 'Whether the dual-layer structure reflects original design or emerged through transmission history.').

omega_variable(
    identity_lock_mechanism_structural_vs_internalized,
    'Is the identity-lock that binds participants to the ritual primarily structural (loss of community membership, economic dependence on the community) or internalized (self-concept constituted through the ritual, psychological impossibility of exit)?',
    'Analysis of communities undergoing rapid secularization or assimilation: do participants who exit the ritual report psychological identity dissolution, or primarily social/economic costs? Follow-up studies of second-generation immigrants who maintain the ritual despite reduced community pressure.',
    'If primarily structural, suppression can be reduced by lowering the exit cost (secular alternatives for community membership, integration into wider society). If primarily internalized, the identity-fusion is durable even as community pressure decreases. The distinction affects whether assimilationist pressure will erode the ritual or whether it persists despite external pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_structural_vs_internalized, empirical, 'Whether identity-lock to the ritual is structurally or psychologically enforced.').

omega_variable(
    survival_competence_layer_explicit_vs_implicit,
    'Is the survival-competence layer (D5) explicitly taught as survival protocol (institutional improvisation, decentralization, resource rationing), or is it implicitly embedded in the ritual form such that participants acquire the competence through enactment without explicit instruction?',
    'Content analysis of ritual instructions and teachings across communities; ethnographic observation of how younger participants learn the ritual; interviews asking participants to articulate what survival competence they believe they are acquiring.',
    'If explicit teaching is necessary for the competence to transmit, the constraint''s effectiveness depends on deliberate instruction, which assimilationist pressure and generational attenuation can disrupt. If implicit, the competence transmits through bodily practice even without conscious awareness, making it more durable. This affects whether the ritual can persist as purely commemorative and still transmit the adaptive layer.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(survival_competence_layer_explicit_vs_implicit, empirical, 'Whether survival-competence transmission is explicit teaching or implicit embodied learning.').

omega_variable(
    sibling_reading_coexistence_or_foreclosure,
    'Do the mourning_practice_reading and survival_competence_reading genuinely coexist as live interpretations within the same community, or does the emphasis on one foreclose the other in practice?',
    'Ethnographic study of multiple communities: document whether communities teach and enact both layers, whether some communities have drifted toward pure mourning or pure survival emphasis, and whether a community can hold both emphases simultaneously or treats them as competing interpretations.',
    'If readings coexist, the hybrid_transformation_reading is descriptively accurate as the operative reading in maintaining communities. If readings foreclose each other, the hybrid_reading is an analytical recognition that some communities fail to achieve the functional combination the reading proposes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_coexistence_or_foreclosure, empirical, 'Whether the mourning-practice and survival-competence readings coexist or compete within the same community.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_function__hybrid_transformation_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(cata_tr_t0, observed).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement_basis(cata_tr_t10, observed).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement_basis(cata_tr_t20, observed).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 30, 0.14).
narrative_ontology:measurement_basis(cata_tr_t30, observed).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 40, 0.15).
narrative_ontology:measurement_basis(cata_tr_t40, observed).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 50, 0.15).
narrative_ontology:measurement_basis(cata_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement_basis(cata_be_t0, observed).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 10, 0.29).
narrative_ontology:measurement_basis(cata_be_t10, observed).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 20, 0.34).
narrative_ontology:measurement_basis(cata_be_t20, observed).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 30, 0.37).
narrative_ontology:measurement_basis(cata_be_t30, observed).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 40, 0.38).
narrative_ontology:measurement_basis(cata_be_t40, observed).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 50, 0.38).
narrative_ontology:measurement_basis(cata_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement_basis(cata_su_t0, observed).
narrative_ontology:measurement(cata_su_t10, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 10, 0.19).
narrative_ontology:measurement_basis(cata_su_t10, observed).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 20, 0.21).
narrative_ontology:measurement_basis(cata_su_t20, observed).
narrative_ontology:measurement(cata_su_t30, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 30, 0.22).
narrative_ontology:measurement_basis(cata_su_t30, observed).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 40, 0.22).
narrative_ontology:measurement_basis(cata_su_t40, observed).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 50, 0.22).
narrative_ontology:measurement_basis(cata_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_function__hybrid_transformation_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_function__hybrid_transformation_reading, 0.12).
narrative_ontology:affects_constraint(catastrophe_memory_function__hybrid_transformation_reading, catastrophe_memory_function__mourning_practice_reading).
narrative_ontology:affects_constraint(catastrophe_memory_function__hybrid_transformation_reading, catastrophe_memory_function__survival_competence_reading).

% DUAL FORMULATION NOTE:
% The catastrophe_memory_function kernel has three constraint stories corresponding to three readings: (1) mourning_practice_reading — ritual preserves mourning-practice and boundary-norms (D1/D4) as primary function; (2) survival_competence_reading — ritual preserves survival-competence (D5) as primary function; (3) hybrid_transformation_reading (this story) — ritual encodes both layers structurally. The three stories form a constraint family. The hybrid_transformation reading is upstream of (influences) both sibling readings because the claim is that the ritual's functional value depends on both layers operating together — a community that loses either layer is no longer instantiating the full catastrophe-memory function. Each sibling reading has a different epsilon (mourning-practice reading: lower extraction, pure coordination; survival-competence reading: lower extraction, pure coordination with adaptive focus; hybrid reading: moderate extraction due to identity-lock and generational obligation). The three readings coexist as live positions in different communities and different interpretive traditions within the same community.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_function__hybrid_transformation_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
