% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_kernel__survival_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_kernel__survival_competence_reading, []).

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
 *   constraint_id: catastrophe_memory_kernel__survival_competence_reading
 *   human_readable: Catastrophe Memory Ritual as Survival-Competence Encoding
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   Catastrophe-memory rituals in persecuted communities encode and transmit
 *   adaptive survival competencies across generations. This constraint is the
 *   survival_competence_reading: the ritual's primary function is to preserve
 *   operational knowledge of how the community survived prior
 *   persecution—behavioral patterns, threat recognition, covert
 *   communication, psychological resilience—in a form that survives
 *   suppression and governmental prohibition. The ritual is not merely
 *   symbolic continuity or trauma encoding; it is a practical archive of
 *   survival technique encoded in ceremonial form so that explicit
 *   survival-training (which would be criminalized or targeted) can be
 *   disguised as cultural/spiritual practice. The constraint shows moderate
 *   extractiveness (0.48) because the transmission of survival competence
 *   genuinely serves the community while also requiring identity-locked
 *   participation and emotional labor extraction from younger generations.
 *   The beneficiary is the persecuted community's collective resilience under
 *   threat; the victim is assimilation pressure—the structural force that
 *   would dissolve the ritual and its encoded competencies.
 *
 * KEY AGENTS:
 *   - persecuted_community: benefits from survival-competence transmission; identity-fused with ritual participation
 *   - younger_generation: pays psychological/temporal cost; constrained exit; inherit burden of intergenerational trauma rehearsal
 *   - diaspora_members: benefit from geographically distributed cohesion and shared threat-recognition; identity-locked participation
 *   - assimilationist_pressure: the excluded structural force the ritual resists
 *   - hostile_authorities: who the ritual prepares survival against; excluded from its meaning
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel__survival_competence_reading, 0.48).
domain_priors:suppression_score(catastrophe_memory_kernel__survival_competence_reading, 0.62).
domain_priors:theater_ratio(catastrophe_memory_kernel__survival_competence_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__survival_competence_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_kernel__survival_competence_reading, "Catastrophe Memory Ritual as Survival-Competence Encoding").
narrative_ontology:topic_domain(catastrophe_memory_kernel__survival_competence_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_kernel__survival_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__survival_competence_reading, '422e820d-5af9-4dd3-bea6-e034961bc79e').
narrative_ontology:cs_kernel_codification('422e820d-5af9-4dd3-bea6-e034961bc79e', distributed).
narrative_ontology:cs_authority_grounding('422e820d-5af9-4dd3-bea6-e034961bc79e', practice).
narrative_ontology:cs_interpretation_layer_present('422e820d-5af9-4dd3-bea6-e034961bc79e').
narrative_ontology:cs_reading_relation('422e820d-5af9-4dd3-bea6-e034961bc79e', catastrophe_memory_kernel__boundary_maintenance_reading, coexists_with).
narrative_ontology:cs_reading_relation('422e820d-5af9-4dd3-bea6-e034961bc79e', catastrophe_memory_kernel__trauma_encoding_reading, coexists_with).
narrative_ontology:cs_reading_relation('422e820d-5af9-4dd3-bea6-e034961bc79e', catastrophe_memory_kernel__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_axiom('422e820d-5af9-4dd3-bea6-e034961bc79e', foundational, survival_competence_operationally_transmissible).
narrative_ontology:cs_axiom_status(survival_competence_operationally_transmissible, holdable).
narrative_ontology:cs_axiom_grounding('422e820d-5af9-4dd3-bea6-e034961bc79e', survival_competence_operationally_transmissible, empirically_contingent).
narrative_ontology:cs_axiom('422e820d-5af9-4dd3-bea6-e034961bc79e', secondary, ritual_disguises_functional_training).
narrative_ontology:cs_axiom_status(ritual_disguises_functional_training, holdable).
narrative_ontology:cs_axiom_grounding('422e820d-5af9-4dd3-bea6-e034961bc79e', ritual_disguises_functional_training, instrumental).
narrative_ontology:cs_reference_frame('422e820d-5af9-4dd3-bea6-e034961bc79e', persecution_survival_through_encoded_competence).
narrative_ontology:cs_drift_state('422e820d-5af9-4dd3-bea6-e034961bc79e', diaspora_low_persecution_context, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('422e820d-5af9-4dd3-bea6-e034961bc79e', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__survival_competence_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__survival_competence_reading, persecuted_community).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__survival_competence_reading, assimilation_pressure).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__survival_competence_reading, diaspora_members).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__survival_competence_reading, younger_generation).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__survival_competence_reading, younger_generation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains and transmits catastrophe-memory rituals that encode survival competencies: how to hide, communicate covertly, recognize threat signals, preserve identity under pressure, resist psychological dissolution. The rituals rehearse responses to persecution, encode escape routes and safe-house networks in symbolic form, and transmit them across generations. The community enforces participation through social obligation and identity fusion—leaving the ritual means risking severance from the group and loss of the knowledge it carries. They both direct the practice and depend on it for intergenerational transmission of adaptive capacity.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, persecuted_community, beneficiary,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_kernel__survival_competence_reading, persecuted_community, agenda_setter).

% Participate in catastrophe-memory rituals across dispersed locations. The rituals maintain psychological readiness and group cohesion despite geographic separation, encode shared threat-recognition patterns, and preserve the knowledge of how the community survived prior persecutions. Non-participation risks identity dissolution and severance from the diaspora's protective network.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, diaspora_members, beneficiary,
    moderate, generational, identity_locked, global).

% Inherit and learn the catastrophe-memory rituals, but experience them as emotionally costly, time-consuming, and increasingly disconnected from contemporary threats they perceive as more salient (economic precarity, climate, digital surveillance). They bear the costs of participation (emotional labor, time, psychological weight of intergenerational trauma) while questioning whether the encoded competencies remain adaptive to current persecution forms. Exit is constrained by family obligation and identity fusion.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, younger_generation, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_kernel__survival_competence_reading, younger_generation, beneficiary).

% The structural force that the ritual resists: state integration policies, economic incentives for cultural abandonment, social pressure to adopt host-culture norms. Not a seated actor but the vector the ritual's enforcement addresses. Maintaining the ritual requires active suppression of assimilationist appeals and institutional barriers to cultural transmission.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, assimilationist_pressure, excluded,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_kernel__survival_competence_reading, assimilationist_pressure).

% Scholars, historians, documentary makers, and anthropological witnesses who document and study the catastrophe-memory rituals. They see adaptive function (survival competence encoding) and also see the psychological and social costs of participation, the identity-locked exit options, and the generational friction. They occupy an outside analytical position.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, external_observers, observer,
    analytical, biographical, analytical, global).

% State or majority-group actors whose persecution the ritual prepares the community to survive. They may ban, suppress, or criminalize the ritual practice, treat it as seditious, or subject it to surveillance. Their exclusion from the ritual's meaning is structural—the ritual encodes resistance to their power.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, hostile_authorities, excluded,
    institutional, generational, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_kernel__survival_competence_reading, persecuted_community).
narrative_ontology:fixing_cost_class(catastrophe_memory_kernel__survival_competence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Encodes and transmits adaptive survival competencies—how to recognize persecution signals, communicate covertly, maintain group cohesion under threat, resist psychological dissolution, access safe networks—across generations through ritualized rehearsal. Solves the problem that explicit survival-training is dangerous (it flags the community as a threat to authorities) and vulnerable to interruption, while ritualization disguises the function as spiritual or cultural practice.
% TRANSFER_FUNCTION: Moves emotional and temporal labor from elders (who design, lead, and transmit the ritual) to younger generations (who learn, embody, and pay the psychological cost of rehearsing catastrophe). The transfer preserves community survival capacity but at the cost of generational psychological burden and identity-lock—younger members cannot exit without losing access to the knowledge and risking severance from the protecting network.
% ABSENT_VOICES: Assimilating members who have chosen cultural abandonment are structurally excluded; they would argue the ritual's costs exceed its protective value in contemporary contexts where persecution risk is lower and economic integration is possible. Younger members experiencing the ritual as identity-imposed rather than freely chosen are partially excluded—their objections are heard within the community but the decision to maintain the practice is made by elder leadership.
% DISAPPEARANCE_RATIONALE: If the catastrophe-memory ritual disappeared overnight, the community would lose the encoded survival competencies within one generation. A subsequent persecution event would find the community unprepared with behavioral and psychological patterns for resistance, covert communication, and psychological resilience. The group's survival probability in future catastrophe would decline measurably. The ritual is not merely symbolic—it is a distributed archive of adaptive response patterns that, once lost, cannot be rapidly reconstructed.
% FOUNDING_PROBLEM: Prior catastrophe (historical persecution, genocide, forced diaspora) revealed that communities face extinction not only through direct violence but through psychological dissolution, identity loss, and failure to transmit survival knowledge. The ritual was developed to encode that knowledge in a form that survives interruption, ban, and generational distance—written documents could be burned or seized; oral teaching could be suppressed; only ritual embedded in spiritual/cultural practice could survive official prohibition while transmitting operational competence.
% FOUNDING_PROBLEM_CORROBORATION: Holocaust survivors and diaspora historians attest the founding problem is live and the ritual function remains adaptive. Younger community members and assimilationist scholars attest the founding problem is substantially historical and the ritual persists as identity-enforcement or trauma rehearsal divorced from current survival need. External observers (security studies scholars, anthropologists studying persecution-survival mechanisms) corroborate that the foundational problem—intergenerational transmission of survival competence under persecution—remains live in contexts where persecution risk is high or historically volatile, but is contested in low-risk diaspora contexts where assimilation is economically advantageous.
narrative_ontology:disappearance_verdict(catastrophe_memory_kernel__survival_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_kernel__survival_competence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__survival_competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_kernel__survival_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_kernel__survival_competence_reading, 0.48, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_kernel__survival_competence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_kernel__survival_competence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_kernel__survival_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.48) and rising slowly over the interval. Early in the interval (t=0), the ritual's adaptive function is high-salience and participatory costs are normalized—communities face active persecution and the survival-training function is clearly necessary. Over time (t=75–100), as persecution risk decreases or becomes historically distant in diaspora contexts, the extractiveness rises because the same ritual now functions more as identity-enforcement and psychological burden than as acute survival-training. Younger generations experience it as imposed rather than protective, yet cannot exit without identity severance. Theater ratio shows complementary rise (0.25→0.42): in high-persecution contexts, the ritual is mostly functional (low theater); in low-persecution diaspora contexts, more of the activity becomes performative (maintaining identity and group boundary) rather than operational (rehearsing actual survival patterns). Suppression requirement falls over time (0.72→0.62) because in diaspora contexts, authorities are less hostile to the ritual; in high-persecution contexts, suppression is externally imposed (by hostile authorities), while in diaspora, self-suppression (internal identity-enforcement) becomes more salient. The theater ratio rise is the key signal: as persecution risk diminishes, the ritual's function shifts from survival-encoding to identity-performance, and extractiveness from younger generations increases because the emotional labor is no longer justified by immediate survival value.
 *
 * PERSPECTIVAL GAP:
 *   Elder community leadership sees the ritual as protective and adaptive—it preserves knowledge that sustained the community through catastrophe and remains necessary insurance against future persecution. Younger members see it as identity-imposed burden whose survival-value is historical, not contemporary. The persecuted_community seat (leadership) computes as moderate beneficiary; the younger_generation seat computes as constrained payer. This divergence should register in the engine's per-seat classification: the agenda-setter (elder leadership) computes the constraint as coordination-with-extraction; the constrained payer computes it as extraction with coordination overlay. The divergence is structural, not evaluative—it flows from different threat-perception and different power to exit.
 *
 * DIRECTIONALITY LOGIC:
 *   Persecuted community (organized power, identity-locked exit): benefits from survival-competence transmission; collects the coordination gain; directionality near beneficiary end (~d=0.2). Younger generation (moderate power, constrained exit): inherits the ritual through obligation, pays psychological cost of catastrophe-rehearsal, identity-fused to the group, cannot exit without severance; directionality near target end (~d=0.75). Diaspora members (moderate power, identity-locked exit): benefit from cohesion and shared threat-pattern recognition; pay participation cost; directionality near symmetric (d=0.55). Assimilationist pressure (non-agent structural force): structurally opposed to the ritual; no directionality (analytical). The ritual's persistence depends on active enforcement of identity-lock (suppression of assimilationist exit) and community enforcement of participation (excluding those who refuse the ritual). This enforcement is why requires_active_enforcement=true.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint claims tangled_rope: genuine coordination function (survival-competence encoding + transmission) plus asymmetric extraction (younger generation pays psychological/temporal labor; elders collect and enforce). The founding_problem_status=contested indicates possible mandatrophy: the founding problem (need to transmit survival competence under persecution) is live in high-persecution contexts but substantially dead in low-risk diaspora contexts. The constraint persists in diaspora despite foundational problem decay because identity-lock and community enforcement maintain it. This is a classic mandatrophy signature: coordination function that persists by inertia and identity-fusion long after the problem it solved has receded. The theater ratio rise (0.25→0.42) supports the mandatrophy reading—as the survival-training function becomes less salient, more of the ritual's energy goes to performative identity-maintenance. The classification should remain tangled_rope (coordination + extraction remain structurally present), but the mandatrophy flag should register that the coordination justification is aging and identity-lock is the primary persistence mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression primarily structural (external authorities ban the ritual) or internalized (community self-enforces participation through identity-lock and shame)?',
    'Compare suppression trajectories across contexts: high-persecution zones where external suppression dominates vs. diaspora zones where internal community enforcement dominates. Post-legalization trajectory in formerly-persecuted contexts (e.g., post-Soviet diaspora): if suppression drops dramatically when legal barriers are removed, the mechanism was primarily structural; if it remains high, the mechanism is primarily internalized.',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest—the target (younger generation) carries the suppression internally and cannot fully exit even if external barriers are removed. This feeds into identity_locked exit classification and may increase per-seat extractiveness for the payer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression is structurally external or internalised through identity-fusion').

omega_variable(
    survival_competence_functional_obsolescence,
    'To what extent do contemporary persecution threats require the specific behavioral/psychological patterns encoded in the historical ritual? Is the encoded competence adaptive to modern persecution forms (surveillance, algorithmic discrimination, digital suppression) or only to historical persecution forms (physical hiding, covert communication, psychological resilience to direct violence)?',
    'Security studies analysis of persecution mechanisms across historical and contemporary contexts. Testimony from communities facing active persecution (can the encoded patterns be applied?) vs. diaspora communities (are the encoded patterns taught or ritually rehearsed despite obsolescence?). Case studies of ritual adaptation or abandonment following persecution-mode shift.',
    'If the encoded competence is functionally obsolete in contemporary persecution forms, the constraint is aging mandatrophy—the coordination function decays while identity-lock maintains it. Extractiveness from younger generations increases because they pay the rehearsal cost for competence that does not transfer to current threat. If the encoded competence remains functional, extractiveness is justified and the constraint remains adaptive tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(survival_competence_functional_obsolescence, empirical, 'Whether historically-encoded survival patterns transfer to contemporary persecution forms').

omega_variable(
    kernel_reading_boundary_maintenance_coexistence,
    'Does the survival_competence_reading structurally coexist with the boundary_maintenance_reading within a single community framework, or does prioritizing survival-competence transmission imply boundary-maintenance is secondary?',
    'Ethnographic analysis: communities that emphasize survival-competence encoding typically also use the ritual for boundary maintenance; communities that emphasize boundary maintenance may encode survival competence as a byproduct. Examine whether the same ritual can be understood as primarily survival-training by elders and primarily identity-boundary-maintenance by youth.',
    'If the readings genuinely coexist (both functions present, both emphasized by different seats), the constraints are distinct but structurally linked via network.affects_constraints. If they compete (emphasizing survival-competence implies de-emphasizing boundary-maintenance function), the readings may foreclose each other contextually. The boundary_maintenance_reading would show higher mandatrophy risk if survival-competence is the primary justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary_maintenance_coexistence, conceptual, 'Whether survival-competence and boundary-maintenance functions coexist or compete within the same ritual').

omega_variable(
    identity_lock_reversibility,
    'Can a younger-generation member exit the catastrophe-memory ritual and retain community membership, or does ritual abandonment entail community severance?',
    'Case studies of individuals who have abandoned ritual participation: do they maintain community standing, family connection, and social support? Are there intermediate positions (ceremonial participation without psychological internalization) that allow partial exit? Compare across communities with different assimilation pressures.',
    'If exit does not entail community severance, exit_options for younger_generation should be reclassified from identity_locked to constrained; directionality would shift downward, reducing per-seat extractiveness. If ritual abandonment entails hard severance, identity_locked holds and extractiveness remains high. This is the primary empirical test for distinguishing extractive vs. coordinating components.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_reversibility, empirical, 'Whether ritual exit entails community severance (identity-lock) or allows constrained partial participation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__survival_competence_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(cata_tr_t15, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 15, 0.28).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 30, 0.32).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 50, 0.39).
narrative_ontology:measurement(cata_tr_t75, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 75, 0.41).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 100, 0.42).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(cata_be_t15, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 15, 0.41).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 30, 0.44).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 50, 0.47).
narrative_ontology:measurement(cata_be_t75, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 75, 0.48).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 100, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement(cata_su_t15, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(cata_su_t30, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 50, 0.64).
narrative_ontology:measurement(cata_su_t75, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 75, 0.62).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 100, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel__survival_competence_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_kernel__survival_competence_reading, 0.12).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__survival_competence_reading, catastrophe_memory_kernel__boundary_maintenance_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__survival_competence_reading, catastrophe_memory_kernel__trauma_encoding_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__survival_competence_reading, catastrophe_memory_kernel__symbol_continuity_reading).

% DUAL FORMULATION NOTE:
% This constraint (survival_competence_reading) is one of four readings of the catastrophe_memory_kernel. All four readings share the same kernel—catastrophe-memory rituals in persecuted communities—but instantiate different constraints by emphasizing different functional aspects. The survival_competence_reading uniquely treats the ritual as operational survival-training; it affects (upstream influences) the boundary_maintenance_reading because survival-competence encoding is a constraint on which boundary-maintenance choices are available. The trauma_encoding_reading and symbol_continuity_reading coexist with this reading: some rituals emphasize trauma-encoding, others emphasize symbolic continuity, and many do all simultaneously. Each reading has its own ε value, beneficiary structure, and mandatrophy profile. Decomposition is required by ε-invariance (DP-001): a survival-competence reading and a boundary-maintenance reading produce different ε values depending on which function is measured as the primary extracted resource.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_kernel__survival_competence_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
