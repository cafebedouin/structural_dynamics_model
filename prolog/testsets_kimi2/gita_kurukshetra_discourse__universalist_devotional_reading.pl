% ============================================================================
% CONSTRAINT STORY: gita_kurukshetra_discourse__universalist_devotional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gita_kurukshetra_discourse__universalist_devotional_reading, []).

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
 *   constraint_id: gita_kurukshetra_discourse__universalist_devotional_reading
 *   human_readable: Gita Universalist Devotional Reading: Bhakti as Caste-Transcendent Surrender
 *   domain: religious/textual/hermeneutic
 *
 * SUMMARY:
 *   The Bhagavad Gita's Kurukshetra discourse, read through the universalist
 *   devotional lens, teaches that path-independent devotion (bhakti)
 *   transcends caste and social position, and that true dharma consists in
 *   surrender to divine will rather than performance of hereditary social
 *   role. This reading functions as a religious constraint that coordinates
 *   spiritual access for a universal devotee class while structurally
 *   undermining traditional Brahminical gatekeeping authority. As a kernel
 *   reading, it is instantiated against sibling readings: the orthodox
 *   literal reading (caste-based duty and prescribed violence) and the
 *   Gandhian allegorical reading (internal psychological battlefield).
 *
 * KEY AGENTS:
 *   - universal_devotee_class: Primary beneficiary (moderate power, identity-locked exit) â gains spiritual access without ritual mediation
 *   - brahminical_gatekeepers: Primary target (powerful, constrained exit) â loses exclusive authority over dharma interpretation
 *   - bhakti_lineage_teachers: Agenda-setter (organized, mobile exit) â interprets and propagates the devotional reading
 *   - orthodox_commentators: Structurally excluded (institutional, constrained) â defends caste-based reading from outside this framework
 *   - critical_textual_scholars: Analytical observer (analytical, analytical exit) â studies the reception and sociological function
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gita_kurukshetra_discourse__universalist_devotional_reading, 0.42).
domain_priors:suppression_score(gita_kurukshetra_discourse__universalist_devotional_reading, 0.58).
domain_priors:theater_ratio(gita_kurukshetra_discourse__universalist_devotional_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gita_kurukshetra_discourse__universalist_devotional_reading, tangled_rope).
narrative_ontology:human_readable(gita_kurukshetra_discourse__universalist_devotional_reading, "Gita Universalist Devotional Reading: Bhakti as Caste-Transcendent Surrender").
narrative_ontology:topic_domain(gita_kurukshetra_discourse__universalist_devotional_reading, "religious/textual/hermeneutic").

domain_priors:requires_active_enforcement(gita_kurukshetra_discourse__universalist_devotional_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gita_kurukshetra_discourse__universalist_devotional_reading, '33de6cdc-6893-4eb0-93da-69694a70ed95').
narrative_ontology:cs_kernel_codification('33de6cdc-6893-4eb0-93da-69694a70ed95', fixed_text).
narrative_ontology:cs_authority_grounding('33de6cdc-6893-4eb0-93da-69694a70ed95', lineage).
narrative_ontology:cs_interpretation_layer_present('33de6cdc-6893-4eb0-93da-69694a70ed95').
narrative_ontology:cs_reading_relation('33de6cdc-6893-4eb0-93da-69694a70ed95', gita_kurukshetra_discourse__orthodox_literal_reading, influences).
narrative_ontology:cs_reading_relation('33de6cdc-6893-4eb0-93da-69694a70ed95', gita_kurukshetra_discourse__gandhian_allegorical_reading, coexists_with).
narrative_ontology:cs_axiom('33de6cdc-6893-4eb0-93da-69694a70ed95', foundational, bhakti_transcends_caste_identity).
narrative_ontology:cs_axiom_status(bhakti_transcends_caste_identity, holdable).
narrative_ontology:cs_axiom_grounding('33de6cdc-6893-4eb0-93da-69694a70ed95', bhakti_transcends_caste_identity, deontological).
narrative_ontology:cs_axiom('33de6cdc-6893-4eb0-93da-69694a70ed95', foundational, divine_surrender_supersedes_social_dharma).
narrative_ontology:cs_axiom_status(divine_surrender_supersedes_social_dharma, holdable).
narrative_ontology:cs_axiom_grounding('33de6cdc-6893-4eb0-93da-69694a70ed95', divine_surrender_supersedes_social_dharma, deontological).
narrative_ontology:cs_reference_frame('33de6cdc-6893-4eb0-93da-69694a70ed95', egalitarian_bhakti_communion).
narrative_ontology:cs_drift_state('33de6cdc-6893-4eb0-93da-69694a70ed95', post_colonial_academic_critique, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('33de6cdc-6893-4eb0-93da-69694a70ed95', '').
narrative_ontology:cs_kernel_id(gita_kurukshetra_discourse__universalist_devotional_reading, gita_kurukshetra_discourse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__universalist_devotional_reading, universal_devotee_class).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__universalist_devotional_reading, brahminical_gatekeepers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Anyone regardless of birth who adopts devotional surrender as their spiritual path. They gain access to salvation and divine relationship without ritual gatekeeping, but their identity becomes fused with the devotional community and its textual framework.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, universal_devotee_class, beneficiary,
    moderate, generational, identity_locked, global).

% Hereditary ritual specialists and caste authorities whose exclusive claim to mediate dharma and scripture is delegitimized by the universalist reading. They bear the cost of diminished social and religious authority.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, brahminical_gatekeepers, payer,
    powerful, generational, constrained, national).

% Interpreters and teachers within bhakti traditions who propagate the universalist devotional reading, set interpretive norms, and organize devotional communities around this textual understanding.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, bhakti_lineage_teachers, agenda_setter,
    organized, generational, mobile, continental).

% Traditional scholars and institutions defending the caste-based, social-role dharma reading. They are structurally excluded from the universalist devotional framework, which treats their interpretation as missing the text's essential teaching.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, orthodox_commentators, excluded,
    institutional, generational, constrained, national).

% Academic scholars analyzing the Gita's historical redaction, sociological function, and competing receptions. They observe how the universalist reading operates as a constraint on social and religious identity without participating in its devotional economy.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, critical_textual_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gita_kurukshetra_discourse__universalist_devotional_reading, diffuse).
narrative_ontology:fixing_cost_class(gita_kurukshetra_discourse__universalist_devotional_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified devotional path (bhakti) accessible across caste and social position, coordinating spiritual practice without requiring ritual expertise or social role performance.
% TRANSFER_FUNCTION: Transfers spiritual authority and salvific legitimacy from caste-based ritual specialists and social-role performance to direct personal devotion and surrender to divine will.
% ABSENT_VOICES: Orthodox literalist interpreters who hold that dharma is caste-specific duty and that the battlefield violence is prescriptive; they are structurally excluded from this reading's hermeneutic framework which treats such readings as missing the text's devotional core.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the universal-devotional framework would collapse; devotees would revert to caste-segregated ritual access or competing paths, and the social-reform leverage derived from the text would dissipate. The text would be re-absorbed into orthodox social-role dharma.
% FOUNDING_PROBLEM: How can salvation or divine union be made accessible to all persons regardless of birth, literacy, or ritual competence, in a context where religious authority is monopolized by hereditary priestly elites?
% FOUNDING_PROBLEM_CORROBORATION: Bhakti movement hagiographies and devotional poets (e.g., Kabir, Ravidas, Mirabai) attest the problem from within the non-Brahminical, devotional seat. Orthodox commentators contest that the problem was ever the text's intent. Academic scholars of religion observe the sociological shift but do not uniformly corroborate the theological framing.
narrative_ontology:disappearance_verdict(gita_kurukshetra_discourse__universalist_devotional_reading, world_rearranges).
narrative_ontology:founding_problem_status(gita_kurukshetra_discourse__universalist_devotional_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gita_kurukshetra_discourse__universalist_devotional_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gita_kurukshetra_discourse__universalist_devotional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gita_kurukshetra_discourse__universalist_devotional_reading, 0.42, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gita_kurukshetra_discourse__universalist_devotional_reading_tests).
:- end_tests(gita_kurukshetra_discourse__universalist_devotional_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) is moderate: the reading genuinely coordinates cross-caste spiritual community but also extracts authority from hereditary gatekeepers. Suppression (0.58) reflects active hermeneutic enforcement required to maintain the universalist claim against the text's own caste- and violence-accepting surface. Theater ratio (0.28) captures moderate performative maintenance in institutionalized devotional display. Accessibility collapse (0.72) is high: once the devotional framework is accepted, caste-based spiritual alternatives lose legitimacy within the frame. Resistance (0.48) is moderate, reflecting persistent orthodox counter-interpretation. The temporal series tracks the reading's evolution from insurgent challenge (T=0, high extraction and suppression) to normalized institution (T=100), with theater rising as the constraint's maintenance becomes increasingly performative.
 *
 * PERSPECTIVAL GAP:
 *   The universal devotee seat experiences the constraint as liberating coordination (low effective extraction, high accessibility), while the Brahminical gatekeeper seat experiences it as targeted displacement of authority (high effective extraction). The agenda-setting bhakti teachers sit between: they benefit from the reading's authority while bearing the labor of its enforcement. The engine computes this divergence from the same structural data through directionality and scope modulation.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (universal devotees) derive low directionality: the constraint subsidizes their spiritual access. Victims (Brahminical gatekeepers) derive high directionality: the constraint extracts their hereditary authority. The agenda-setters (bhakti teachers) are near-symmetric or mild beneficiaries â they accrue prestige and following but must actively reproduce the interpretive labor. No directionality override is needed because the structural derivation captures the relationships: universal devotees have identity-locked exit (fused to devotional community), amplifying their beneficiary status; gatekeepers have constrained exit (bound to the textual tradition they are losing), amplifying their target status.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â exclusion from spiritual access based on birth â remains contestedly live in social practice even if the text's devotional frame claims to solve it. The constraint avoids piton misclassification because its coordination function (cross-caste devotional access) is not merely theatrical: it has produced centuries of genuine religious community. It avoids snare misclassification because the beneficiary class is broad and diffuse rather than a narrow capturing group. The classification as tangled_rope reflects the dual reality: a real coordination mechanism layered with asymmetric authority transfer from traditional gatekeepers to the devotional community.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_authority_vs_hegemonic_instrument,
    'Does this reading derive its force from genuine divine authority or from its utility as a counter-hegemonic instrument against Brahminical power?',
    'Historical reception study: does the reading retain adherence when separated from its social-leveling function (e.g., in diaspora contexts where caste pressure is absent)?',
    'If adherence persists without caste opposition, the reading has autonomous spiritual force (mountain-leaning); if it attenuates, it was primarily instrumental (tangled_rope confirmed).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(divine_authority_vs_hegemonic_instrument, conceptual, 'Whether the reading''s authority is intrinsic or instrumental').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of caste-based dharma structural (textual interpretation marginalizes it) or internalized (devotees cognitively discard caste identity as spiritually irrelevant)?',
    'Comparative study of devotees who exit the tradition: if caste identity reasserts after exit, suppression was internalized; if caste remains spiritually irrelevant after exit, suppression was structural.',
    'If internalized, effective suppression is higher than structural measure â the devotee carries the constraint''s framing even after leaving the community.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression of caste identity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gita_kurukshetra_discourse__universalist_devotional_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gita_tr_t0, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(gita_tr_t20, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(gita_tr_t40, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 40, 0.25).
narrative_ontology:measurement(gita_tr_t60, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 60, 0.3).
narrative_ontology:measurement(gita_tr_t80, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 80, 0.32).
narrative_ontology:measurement(gita_tr_t100, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 100, 0.28).

% Extraction over time
narrative_ontology:measurement(gita_be_t0, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(gita_be_t20, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(gita_be_t40, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 40, 0.4).
narrative_ontology:measurement(gita_be_t60, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 60, 0.35).
narrative_ontology:measurement(gita_be_t80, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 80, 0.38).
narrative_ontology:measurement(gita_be_t100, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 100, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(gita_su_t0, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement(gita_su_t20, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(gita_su_t40, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 40, 0.62).
narrative_ontology:measurement(gita_su_t60, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 60, 0.55).
narrative_ontology:measurement(gita_su_t80, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 80, 0.56).
narrative_ontology:measurement(gita_su_t100, gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 100, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gita_kurukshetra_discourse__universalist_devotional_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% One of three structurally distinct readings of the Bhagavad Gita Kurukshetra discourse kernel. The universalist devotional reading is decomposed from the orthodox literal and Gandhian allegorical readings because each instantiates a different epsilon, beneficiary structure, and normative axiom set on the same textual kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
