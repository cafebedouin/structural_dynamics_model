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
    narrative_ontology:measurement_basis/2,
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
 *   human_readable: Ritual as Practical Survival Knowledge Transmission (Competence Reading)
 *   domain: religious_studies/collective_memory/adaptation
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the contested kernel
 *   'catastrophe_memory_survival': the competence-transmission reading. Under
 *   this reading, ritual is understood primarily as a technology for encoding
 *   practical survival knowledge — timing, resource management, family
 *   protocols, adaptation strategies — in forms that survive displacement,
 *   cultural change, and forgetting. The constraint operates when
 *   ritual-preserving communities maintain the practice out of tradition
 *   while communities needing the embedded knowledge (diaspora,
 *   crisis-affected populations) extract and apply it. The reading is one
 *   voice in a kernel dispute: the symbol-survival reading emphasizes
 *   identity and boundary-maintenance; the hybrid-encoding reading holds that
 *   ritual operates on dual registers. This story generates the
 *   competence-transmission version only, with its own ε, beneficiary/victim
 *   structure, and temporal profile. The measurement series shows extraction
 *   accumulating from t=0 to t=75 (as the practical content becomes
 *   increasingly invisible in stable contexts while diaspora communities
 *   develop dependency on the ritual form) then declining at t=100 (a
 *   hypothetical moment when crisis-driven adaptation surfaces the knowledge
 *   and the asymmetry becomes visible and contested).
 *
 * KEY AGENTS:
 *   - Diaspora communities with adaptive capacity: beneficiary, moderate power, constrained exit — benefit from ritual-encoded knowledge when crisis or displacement forces activation
 *   - Communities in stable contexts maintaining ritual form: payer, powerless to moderate, trapped exit — bear the cost of ritual performance without recognizing its practical content
 *   - Ritual knowledge keepers: agenda setter, organized power, identity-locked exit — maintain transmission authority, may or may not be aware the knowledge is practical rather than purely symbolic
 *   - Younger generation in context-shifted communities: payer/beneficiary dual role, powerless, constrained exit — learn form without function; would benefit if crisis forces knowledge application
 *   - Crisis events: analytical observer seat — reveal what the ritual constraint carried
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_survival__competence_transmission_reading, 0.58).
domain_priors:suppression_score(catastrophe_memory_survival__competence_transmission_reading, 0.47).
domain_priors:theater_ratio(catastrophe_memory_survival__competence_transmission_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 0.47).
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, resistance, 0.53).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_survival__competence_transmission_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_survival__competence_transmission_reading, "Ritual as Practical Survival Knowledge Transmission (Competence Reading)").
narrative_ontology:topic_domain(catastrophe_memory_survival__competence_transmission_reading, "religious_studies/collective_memory/adaptation").

domain_priors:requires_active_enforcement(catastrophe_memory_survival__competence_transmission_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_survival__competence_transmission_reading, 'e90deaa9-98b8-4b59-b072-014c203833f1').
narrative_ontology:cs_kernel_codification('e90deaa9-98b8-4b59-b072-014c203833f1', distributed).
narrative_ontology:cs_authority_grounding('e90deaa9-98b8-4b59-b072-014c203833f1', practice).
narrative_ontology:cs_interpretation_layer_present('e90deaa9-98b8-4b59-b072-014c203833f1').
narrative_ontology:cs_reading_relation('e90deaa9-98b8-4b59-b072-014c203833f1', catastrophe_memory_survival__symbol_survival_reading, coexists_with).
narrative_ontology:cs_reading_relation('e90deaa9-98b8-4b59-b072-014c203833f1', catastrophe_memory_survival__hybrid_encoding_reading, influences).
narrative_ontology:cs_axiom('e90deaa9-98b8-4b59-b072-014c203833f1', foundational, practical_knowledge_is_survival_essential).
narrative_ontology:cs_axiom_status(practical_knowledge_is_survival_essential, holdable).
narrative_ontology:cs_axiom_grounding('e90deaa9-98b8-4b59-b072-014c203833f1', practical_knowledge_is_survival_essential, empirically_contingent).
narrative_ontology:cs_axiom('e90deaa9-98b8-4b59-b072-014c203833f1', foundational, ritual_form_encodes_competence_content).
narrative_ontology:cs_axiom_status(ritual_form_encodes_competence_content, holdable).
narrative_ontology:cs_axiom_grounding('e90deaa9-98b8-4b59-b072-014c203833f1', ritual_form_encodes_competence_content, empirically_contingent).
narrative_ontology:cs_reference_frame('e90deaa9-98b8-4b59-b072-014c203833f1', ritual_as_mnemonic_technology_for_survival_knowledge).
narrative_ontology:cs_drift_state('e90deaa9-98b8-4b59-b072-014c203833f1', modernization_and_resource_abundance_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e90deaa9-98b8-4b59-b072-014c203833f1', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_survival__competence_transmission_reading, catastrophe_memory_survival).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__competence_transmission_reading, diaspora_communities_adaptive_capacity).
narrative_ontology:constraint_victim(catastrophe_memory_survival__competence_transmission_reading, communities_losing_practical_content).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__competence_transmission_reading, younger_generation_context_shifted).
narrative_ontology:constraint_victim(catastrophe_memory_survival__competence_transmission_reading, younger_generation_context_shifted).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Groups displaced from ancestral contexts extract practical knowledge from ritual — timing of agricultural cycles, water identification, kinship-based mutual aid protocols, resource allocation during scarcity. They benefit when crisis or displacement makes the embedded knowledge urgent. Their exit options are constrained because relocation or resource collapse forces them to depend on whatever adaptive knowledge they retain; they cannot simply stay in stable contexts where they do not need the knowledge.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, diaspora_communities_adaptive_capacity, beneficiary,
    moderate, generational, constrained, global).

% Communities in industrialized, resource-abundant, or modernized contexts where the original survival rationale is invisible. Younger generations learn ritual because it is transmitted as binding tradition, but the practical knowledge is not legible in their lived environment (commercial agriculture, municipal water systems, state welfare). They cannot exit because the practice is embedded in family identity and community belonging; not participating marks one as culturally disloyal or assimilated. The cost is time, cognitive load, and forgone alternatives, with no recognized return.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, communities_losing_practical_content, payer,
    powerless, biographical, trapped, local).

% Elders, ceremonial leaders, oral historians, religious authorities who maintain transmission authority. They decide what counts as proper practice, what may be omitted, how knowledge is explained. Their professional and personal identity is constituted through the keeper role; they cannot exit without losing status and self-definition. They have incentive to enforce transmission fidelity because it centralizes their expertise and authority. They may or may not consciously recognize the practical knowledge embedded in the forms they transmit (omega 1 addresses this uncertainty).
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, ritual_knowledge_keepers, agenda_setter,
    organized, generational, identity_locked, regional).

% Born into communities where the survival rationale is socially invisible — modern housing, commercial agriculture, state systems. They must learn and perform ritual but receive the practical knowledge lessons only implicitly, if at all. They pay the cost of learning form disconnected from function (time, attention, cultural obligation). They would benefit if displacement or crisis later requires the knowledge, but in stable contexts the knowledge appears irrelevant. They cannot exit because ritual participation is a condition of family membership and community recognition.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, younger_generation_context_shifted, payer,
    powerless, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_survival__competence_transmission_reading, younger_generation_context_shifted, beneficiary).

% Catastrophes, displacement, resource collapse, pandemic, or climate disruption that suddenly make the embedded practical knowledge urgent and visible. These events reveal the constraint's actual function and expose the asymmetry between stable-context payers (who maintained form) and diaspora beneficiaries (who can activate the knowledge). The observer seat documents what the constraint carried and how visibility changes under crisis conditions.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, crisis_events_forced_adaptation, observer,
    analytical, immediate, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_survival__competence_transmission_reading, diaspora_communities_adaptive_capacity).
narrative_ontology:fixing_cost_class(catastrophe_memory_survival__competence_transmission_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Encodes and preserves practical survival knowledge — seasonal agricultural timing, water-source identification, kinship mutual-aid protocols, resource allocation under scarcity, family-based succession and knowledge-transfer practices — in ritual form so the knowledge survives displacement, cultural change, and the forgetting that accompanies stable abundance. The coordination problem solved: how to transmit competences across generations and geographic displacement when the original context (famine, drought, resource scarcity) becomes invisible.
% TRANSFER_FUNCTION: Moves cognitive, temporal, and social resources from stable-context communities and younger generations (who maintain ritual form without recognized practical return) toward diaspora and crisis-affected communities (who extract and apply the embedded adaptive knowledge). The transfer is latent: those paying do not know they are subsidizing others; those receiving may not consciously recognize they are drawing on encoded knowledge rather than receiving symbolic inheritance.
% ABSENT_VOICES: Communities that lost the ritual corpus entirely (forced assimilation, ritual suppression under colonialism, diaspora fracture) cannot testify to what they no longer have. Descendants of survivors who escaped catastrophe by accessing ritual-encoded knowledge (but are now in stable contexts where the knowledge appears obsolete) rarely articulate the competence layer explicitly — they emphasize cultural continuity rather than practical survival. The practical knowledge is usually transmitted tacitly (through demonstration, not explanation), so even communities that have it may not recognize it as distinct from symbolic practice.
% DISAPPEARANCE_RATIONALE: If the ritual-as-competence constraint disappeared (rituals stripped of practical knowledge encoding, transmitted only as symbolic form), diaspora and crisis-affected communities would lose access to survival strategies their ancestors encoded. Communities currently in stable contexts would face no immediate change because they do not consciously depend on the knowledge. But the disappearance would reveal itself most sharply when the next crisis occurs: communities without access to the ritual-preserved competences would lack adaptive strategies their ancestors' ritual carried. The world would rearrange around increased vulnerability in post-crisis survival.
% FOUNDING_PROBLEM: Societies facing recurring catastrophe — seasonal scarcity, migration, conflict, plague, resource volatility — needed to preserve practical survival knowledge in forms that could endure displacement, cultural suppression, modernization, and the natural forgetting of stable abundance. Ritual provided both mnemonic structure (embodied repetition encodes knowledge in body and community memory, making it harder to forget) and social legitimacy (ritual framing protected knowledge from being dismissed as 'superstition' during modernization or conquest).
% FOUNDING_PROBLEM_CORROBORATION: Anthropological evidence (Malinowski's kula magic, Turner's Ndembu ritual studies, Bloch's ceremonial time) documents ritual's role in encoding practical knowledge. Archaeological and ethnographic studies of diaspora communities show activation of ritual-preserved competences under crisis (drought-timing knowledge in water-scarce regions, post-Partition communities using ritual-encoded kinship mutual aid, pandemic-era communities recovering food-preservation practices from ritual). Crisis narratives (refugee camps, post-disaster communities, pandemic mutual aid networks) include testimony from survivors and aid workers that ritual-transmitted knowledge enabled adaptation and survival. This corroboration comes from anthropologists, crisis survivors, diaspora historians, and NGO workers — seats outside the ritual keepers' authority — rather than from the keepers themselves, who often frame the constraint in continuity and identity terms rather than competence terms.
narrative_ontology:disappearance_verdict(catastrophe_memory_survival__competence_transmission_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_survival__competence_transmission_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_survival__competence_transmission_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is moderate (0.58 at interval end, starting at 0.35) because the constraint operates through latent dependency: communities paying the cost of maintaining ritual do not recognize they are subsidizing others' survival capacity. The knowledge is embedded (non-transparent), so the transfer is not explicitly negotiated. Suppression is moderate (0.47) because the constraint persists through cultural authority and inheritance rather than external coercion — the knowledge keepers enforce transmission fidelity, but the suppression is internalized (younger generations accept the obligation as normal tradition). Theater rises from 0.18 to 0.48 then dips to 0.42 because as contexts stabilize (industrialization, modernization), the practical rationale becomes invisible and ritual performance becomes increasingly theatrical — repetition without understood function. The measurement decline at t=100 reflects a hypothetical crisis moment when the competence layer becomes visible again and the theatrical quality drops as the knowledge becomes urgent and legible again. The series is authored on a single time grid (all three metrics at all six points) and reflects cyclical dynamics: stability → form-only maintenance → crisis → knowledge activation → temporary clarity → relaxation back toward form.
 *
 * PERSPECTIVAL GAP:
 *   From the ritual keeper's seat, the constraint preserves identity, continuity, and community practice. From the stable-context payer's seat, it is a costly tradition whose rationale is lost. From the diaspora beneficiary's seat, it is a lifeline whose knowledge must be extracted from forms that keepers themselves may not consciously understand as practical. The engine computes these divergent seat classifications from the authored structural data: beneficiaries (diaspora) get low directionality; payers (stable communities) get high directionality; agenda-setters (keepers) get moderate directionality reflecting their mixed position (they set the rules but are often identity-locked to the practice's framing).
 *
 * DIRECTIONALITY LOGIC:
 *   Diaspora communities carry beneficiary status and constrained exit, placing them near d=0.2 (subsidy side): the constraint benefits them by preserving knowledge they would otherwise lose. Communities in stable contexts carry victim status and trapped exit, placing them near d=0.85 (target side): they pay the cost of maintaining form, unable to exit because the practice is embedded in family and community identity. Ritual keepers carry agenda-setter status with identity-locked exit and organized power, placing them near d=0.5 (symmetric): they set and enforce the rules, but their professional identity is fused with the practice, so they cannot freely modify it without risking their authority. Younger generations in shifted contexts carry dual payer/beneficiary roles with powerless status and constrained exit — they currently pay the cost of learning form without function, but would benefit if crisis requires application. No directionality overrides are needed; the derived directionalities are structurally accurate.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resists mislabeling as pure rope (coordination only) because the transfer is asymmetric and the cost to stable-context communities is active: they must maintain ritual fidelity without receiving equivalent benefit or even understanding the arrangement. It also resists snare classification because the constraint is not primarily coercive — it operates through cultural inheritance, and the knowledge keepers themselves may not intend extraction. The tangled-rope classification fits: genuine coordination function (knowledge preservation) coupled with asymmetric extraction (stable communities subsidize diaspora survival capacity). The knowledge keepers enforce transmission fidelity (active enforcement), creating the tangled structure. The constraint persists because the practical rationale is hidden in form, making the asymmetry invisible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    knowledge_visibility_and_extraction,
    'Do ritual keepers and community members in stable contexts consciously recognize the practical knowledge embedded in ritual, or is the knowledge tacit and invisible even to those maintaining it?',
    'Ethnographic interviews with knowledge keepers asking them to explain ritual components in practical terms; documentation of crisis moments where knowledge suddenly becomes explicit and applied; comparison of knowledge-conscious vs. knowledge-opaque communities.',
    'If knowledge is invisible, the extraction is latent and suppression is internalized (communities maintain form because it is tradition, unaware they subsidize others). If knowledge is conscious, the constraint becomes more akin to snare (keepers knowingly withhold or control access). The visibility axis determines whether the constraint is enforced through cultural authority (invisible case) or through explicit gatekeeping (conscious case).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(knowledge_visibility_and_extraction, empirical, 'Whether the practical knowledge embedded in ritual is tacit or explicit to those maintaining it.').

omega_variable(
    dual_register_vs_pure_competence,
    'Does ritual encode ONLY practical knowledge (this reading''s claim), or does it operate on dual registers where symbolic boundary-maintenance is equally essential to survival (the hybrid reading''s claim)?',
    'Test whether diaspora communities can extract and apply the practical knowledge without performing the symbolic components; examine whether communities that lose ritual form but retain practical knowledge (through written documentation, scientific transmission) survive equally well; study crisis response where one register is preserved and the other lost.',
    'If pure competence transmission, the constraint is correctly classified here. If dual registers are equally essential, the constraint should be decomposed into two stories or reclassified as hybrid encoding. The resolution determines whether this reading''s ε is accurate or whether extraction is understated because symbolic function is unaccounted.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dual_register_vs_pure_competence, conceptual, 'Whether ritual transmits competence alone or operates on dual symbolic and practical registers.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is the measured suppression (0.47) primarily structural (external enforcement by keepers preventing knowledge-sharing) or internalized (younger generations and community members believe ritual knowledge is sacred or untranslatable, enforcing the suppression themselves)?',
    'Post-prohibition trajectory: remove the ritual keeper''s authority to enforce transmission standards and observe whether suppression persists through internalized norms; compare communities where keepers are present vs. absent but ritual is maintained; track knowledge transfer in diaspora where keepers are unavailable but younger generations continue to learn.',
    'If internalized, the constraint''s suppression persists even after structural barriers are removed and targets carry the suppression with them. If structural, removing keeper authority and enforcement would collapse suppression. Internalized suppression indicates stronger identity-lock for younger generations than the current exit_options score suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Whether suppression of knowledge-sharing is externally enforced or internalized in community identity.').

omega_variable(
    committer_frame_competence_vs_symbol_boundary,
    'This reading privileges practical competence as the organizing principle of ritual''s survival function. The symbol-survival reading privileges identity continuity and boundary-maintenance. Is the dispute about what ritual IS, or about what aspects of ritual are ESSENTIAL for survival?',
    'Examine whether the readings dispute the same referent (ritual itself) or different aspects of it. If they disagree about what makes ritual effective for survival (competence vs. symbol), they are one kernel with multiple readings. If they dispute whether the referent is ''ritual''s practical knowledge'' (competence reading''s ε) vs. ''ritual''s identity function'' (symbol reading''s ε), they may be two constraints (ε-invariance principle).',
    'If one kernel, this story and the symbol reading share the same artifact but read it differently. The competence reading''s ε (0.58) applies to ritual-as-knowledge-carrier; the symbol reading''s ε applies to ritual-as-identity-carrier. If two constraints (distinct referents), they are structurally independent and should be decomposed with network.affects_constraints links. The classification of the kernel contest determines whether the sibling readings are readings or decomposed constraints.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_frame_competence_vs_symbol_boundary, conceptual, 'Whether competence and symbol readings contest the same constraint or refer to different constraints.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_survival__competence_transmission_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(cata_tr_t0, observed).
narrative_ontology:measurement(cata_tr_t15, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 15, 0.24).
narrative_ontology:measurement_basis(cata_tr_t15, observed).
narrative_ontology:measurement(cata_tr_t35, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 35, 0.35).
narrative_ontology:measurement_basis(cata_tr_t35, observed).
narrative_ontology:measurement(cata_tr_t55, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 55, 0.44).
narrative_ontology:measurement_basis(cata_tr_t55, observed).
narrative_ontology:measurement(cata_tr_t75, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 75, 0.48).
narrative_ontology:measurement_basis(cata_tr_t75, observed).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 100, 0.42).
narrative_ontology:measurement_basis(cata_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(cata_be_t0, observed).
narrative_ontology:measurement(cata_be_t15, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 15, 0.42).
narrative_ontology:measurement_basis(cata_be_t15, observed).
narrative_ontology:measurement(cata_be_t35, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 35, 0.53).
narrative_ontology:measurement_basis(cata_be_t35, observed).
narrative_ontology:measurement(cata_be_t55, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 55, 0.61).
narrative_ontology:measurement_basis(cata_be_t55, observed).
narrative_ontology:measurement(cata_be_t75, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 75, 0.64).
narrative_ontology:measurement_basis(cata_be_t75, observed).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 100, 0.58).
narrative_ontology:measurement_basis(cata_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 0, 0.31).
narrative_ontology:measurement_basis(cata_su_t0, observed).
narrative_ontology:measurement(cata_su_t15, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 15, 0.38).
narrative_ontology:measurement_basis(cata_su_t15, observed).
narrative_ontology:measurement(cata_su_t35, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 35, 0.44).
narrative_ontology:measurement_basis(cata_su_t35, observed).
narrative_ontology:measurement(cata_su_t55, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 55, 0.49).
narrative_ontology:measurement_basis(cata_su_t55, observed).
narrative_ontology:measurement(cata_su_t75, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 75, 0.51).
narrative_ontology:measurement_basis(cata_su_t75, observed).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 100, 0.47).
narrative_ontology:measurement_basis(cata_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_survival__competence_transmission_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_survival__competence_transmission_reading, 0.18).
narrative_ontology:affects_constraint(catastrophe_memory_survival__competence_transmission_reading, catastrophe_memory_survival__symbol_survival_reading).
narrative_ontology:affects_constraint(catastrophe_memory_survival__competence_transmission_reading, catastrophe_memory_survival__hybrid_encoding_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the catastrophe_memory_survival kernel. The competence-transmission reading emphasizes ritual as technology for encoding practical knowledge (timing, resource management, adaptation strategies). Sibling readings: symbol-survival_reading (ritual preserves identity and boundary-norms through symbolic experience; survival = continuity of practice itself) and hybrid-encoding_reading (ritual operates on dual registers: symbolic boundary AND embedded practical knowledge, with survival depending on both). All three share the kernel artifact (ritual practice) but differ in what aspect is essential for survival. The competence reading's ε (0.58) applies to ritual-as-knowledge-carrier; the symbol reading's ε applies to ritual-as-identity-carrier; the hybrid reading's ε applies to ritual-as-dual-function. Network links enable contamination analysis: if the competence reading's knowledge-visibility assumption is wrong (omega 1), the hybrid reading becomes more plausible and affects the classification of both; if pure competence extraction is confirmed (omega 2 resolution favoring this reading), the symbol and hybrid readings' ε values should be adjusted downward.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
