% ============================================================================
% CONSTRAINT STORY: gita_kurukshetra_discourse__gandhian_allegorical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gita_kurukshetra_discourse__gandhian_allegorical_reading, []).

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
 *   constraint_id: gita_kurukshetra_discourse__gandhian_allegorical_reading
 *   human_readable: Gandhian Allegorical Reading of the Gita
 *   domain: religious_studies/hermeneutics/ethical_philosophy
 *
 * SUMMARY:
 *   The Bhagavad Gita's Kurukshetra discourse, read through Gandhi's
 *   allegorical hermeneutic, constrains interpretation by mandating that the
 *   battlefield symbolizes internal psychic conflict and that ahimsa is the
 *   supreme dharma. This reading dissolves the divine mandate for caste
 *   hierarchy and repudiates physical violence, shifting interpretive
 *   authority from Brahminical scholars to individual moral conscience. It
 *   functions as a commitment system with the Gita as fixed kernel and
 *   Gandhian practice as interpretive authority.
 *
 * KEY AGENTS:
 *   - Dalit communities (powerless/constrained) â liberated from scriptural caste mandate
 *   - Individual conscience adherents (moderate/identity_locked) â gain hermeneutic authority, pay through strict ahimsa discipline
 *   - Gandhian institutions (organized/constrained) â agenda-setters propagating the reading
 *   - Brahminical interpreters (institutional/constrained) â lose interpretive monopoly
 *   - Militant literalists (organized/mobile) â lose textual justification for warfare
 *   - Western academic observers (analytical/analytical) â analytical seat tracking contestation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gita_kurukshetra_discourse__gandhian_allegorical_reading, 0.48).
domain_priors:suppression_score(gita_kurukshetra_discourse__gandhian_allegorical_reading, 0.42).
domain_priors:theater_ratio(gita_kurukshetra_discourse__gandhian_allegorical_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gita_kurukshetra_discourse__gandhian_allegorical_reading, tangled_rope).
narrative_ontology:human_readable(gita_kurukshetra_discourse__gandhian_allegorical_reading, "Gandhian Allegorical Reading of the Gita").
narrative_ontology:topic_domain(gita_kurukshetra_discourse__gandhian_allegorical_reading, "religious_studies/hermeneutics/ethical_philosophy").

domain_priors:requires_active_enforcement(gita_kurukshetra_discourse__gandhian_allegorical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gita_kurukshetra_discourse__gandhian_allegorical_reading, '1d17e775-8c92-4897-b819-ad707f6c4179').
narrative_ontology:cs_kernel_codification('1d17e775-8c92-4897-b819-ad707f6c4179', fixed_text).
narrative_ontology:cs_authority_grounding('1d17e775-8c92-4897-b819-ad707f6c4179', practice).
narrative_ontology:cs_interpretation_layer_present('1d17e775-8c92-4897-b819-ad707f6c4179').
narrative_ontology:cs_reading_relation('1d17e775-8c92-4897-b819-ad707f6c4179', gita_kurukshetra_discourse__orthodox_literal_reading, forecloses).
narrative_ontology:cs_reading_relation('1d17e775-8c92-4897-b819-ad707f6c4179', gita_kurukshetra_discourse__universalist_devotional_reading, coexists_with).
narrative_ontology:cs_axiom('1d17e775-8c92-4897-b819-ad707f6c4179', foundational, kurukshetra_as_internal_struggle).
narrative_ontology:cs_axiom_status(kurukshetra_as_internal_struggle, holdable).
narrative_ontology:cs_axiom_grounding('1d17e775-8c92-4897-b819-ad707f6c4179', kurukshetra_as_internal_struggle, deontological).
narrative_ontology:cs_axiom('1d17e775-8c92-4897-b819-ad707f6c4179', foundational, ahimsa_as_supreme_dharma).
narrative_ontology:cs_axiom_status(ahimsa_as_supreme_dharma, holdable).
narrative_ontology:cs_axiom_grounding('1d17e775-8c92-4897-b819-ad707f6c4179', ahimsa_as_supreme_dharma, deontological).
narrative_ontology:cs_reference_frame('1d17e775-8c92-4897-b819-ad707f6c4179', gandhian_nonviolent_conscience).
narrative_ontology:cs_drift_state('1d17e775-8c92-4897-b819-ad707f6c4179', contemporary_hindutva_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('1d17e775-8c92-4897-b819-ad707f6c4179', '').
narrative_ontology:cs_kernel_id(gita_kurukshetra_discourse__gandhian_allegorical_reading, gita_kurukshetra_discourse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__gandhian_allegorical_reading, dalit_communities).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__gandhian_allegorical_reading, individual_conscience_adherents).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__gandhian_allegorical_reading, gandhian_institutions).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__gandhian_allegorical_reading, brahminical_interpreters).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__gandhian_allegorical_reading, militant_literalists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__gandhian_allegorical_reading, individual_conscience_adherents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the removal of scriptural divine mandate for caste hierarchy; the allegorical reading denies that the Gita sanctions birth-based social subordination, opening spiritual dignity independent of caste status.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, dalit_communities, beneficiary,
    powerless, generational, constrained, national).

% Gain authority to interpret sacred text through personal moral conscience rather than Brahminical mediation; simultaneously bound to strict ahimsa and perpetual inner struggle as the path of dharma.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, individual_conscience_adherents, beneficiary,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(gita_kurukshetra_discourse__gandhian_allegorical_reading, individual_conscience_adherents, payer).

% Propagate the allegorical hermeneutic through ashrams, publishing houses, and educational curricula; maintain the boundary between authentic Gandhian interpretation and literalist reclamation.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, gandhian_institutions, agenda_setter,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(gita_kurukshetra_discourse__gandhian_allegorical_reading, gandhian_institutions, beneficiary).

% Lose exclusive hermeneutic monopoly over the Gita as interpretive authority shifts to individual conscience; their caste-based textual authority is delegitimized within the Gandhian framework.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, brahminical_interpreters, payer,
    institutional, generational, constrained, national).

% Lose scriptural justification for righteous physical warfare; their political theology is delegitimized as the text is allegorized into a manual of internal nonviolent discipline.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, militant_literalists, payer,
    organized, biographical, mobile, national).

% Analyze the Gandhian reading as a postcolonial hermeneutic phenomenon, tracking its rise, institutionalization, and contemporary contestation without being bound to its normative claims.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, western_academic_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gita_kurukshetra_discourse__gandhian_allegorical_reading, diffuse).
narrative_ontology:fixing_cost_class(gita_kurukshetra_discourse__gandhian_allegorical_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates mass nonviolent resistance and ethical community by providing a shared scriptural basis for ahimsa and inner discipline, uniting diverse participants under a common hermeneutic that repudiates physical violence and caste hierarchy.
% TRANSFER_FUNCTION: Moves interpretive authority from Brahminical scholarly elites and scriptural literalists to individual moral conscience; moves social legitimacy from caste hierarchy to egalitarian ethical practice; demands strict nonviolent compliance from adherents.
% ABSENT_VOICES: Orthodox temple authorities and traditional warrior-caste communities who hold the literal reading are absent from Gandhian interpretive councils; their objections to allegorization are heard only as antagonists, never as interlocutors with standing.
% DISAPPEARANCE_RATIONALE: The nonviolent mass movements that relied on this reading for scriptural legitimacy would lose a key unifying narrative; the interpretive space would likely revert to caste-based or militarized readings, and Gandhian ethical pedagogy would require a new textual anchor.
% FOUNDING_PROBLEM: Colonial-era legitimation of violence and caste hierarchy through scriptural literalism; need for an indigenous ethical framework that could mobilize mass resistance without recourse to physical warfare or reinforcing Brahminical social dominance.
% FOUNDING_PROBLEM_CORROBORATION: Colonial administrative records document caste violence; Dalit testimonies and Ambedkarite critiques corroborate the problem of scriptural sanction for hierarchy from outside the Gandhian beneficiary set. The specific allegorical solution is primarily corroborated by Gandhian practitioners; external corroboration of the reading as the uniquely necessary response is limited.
narrative_ontology:disappearance_verdict(gita_kurukshetra_discourse__gandhian_allegorical_reading, world_rearranges).
narrative_ontology:founding_problem_status(gita_kurukshetra_discourse__gandhian_allegorical_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gita_kurukshetra_discourse__gandhian_allegorical_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gita_kurukshetra_discourse__gandhian_allegorical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gita_kurukshetra_discourse__gandhian_allegorical_reading, 0.48, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gita_kurukshetra_discourse__gandhian_allegorical_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gita_kurukshetra_discourse__gandhian_allegorical_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gita_kurukshetra_discourse__gandhian_allegorical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.48) because the reading genuinely coordinates mass nonviolent resistance while asymmetrically extracting from Brahminical interpreters and militant nationalists who lose textual legitimacy. Suppression is moderate (0.42) because the reading actively marginalizes literal interpretations through pedagogical and rhetorical enforcement rather than physical coercion. Theater ratio is low-moderate (0.28): the allegory is functionally central to Gandhian ethics, though state ritual introduces performative elements. Accessibility collapse is moderate (0.38) because literal readings remain available outside Gandhian institutions. Resistance is moderate-to-high (0.52) due to persistent orthodox and Hindutva opposition. Measurements share a single time grid to prevent misaligned substitution.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats (Gandhian institutions, Dalit communities, individual adherents) experience the constraint as liberatory coordination; the payer seats (Brahminical interpreters, militant literalists) experience it as hermeneutic dispossession and political delegitimization. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries gain hermeneutic authority and ethical coherence, placing them near the subsidy end of directionality. Victims lose status monopoly and political theology, placing them near the target end. Individual adherents sit near symmetric: they gain authority but pay through strict ethical discipline and identity-locked exit.
 *
 * MANDATROPHY ANALYSIS:
 *   Tangled rope classification prevents mislabeling: a pure rope reading would ignore the delegitimization of orthodox authority; a pure snare reading would ignore the genuine ethical coordination of mass nonviolent resistance. The reading's hybrid natureâcoordinating liberation while extracting from prior elitesâis structurally captured by the tangled rope gate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    gandhian_caste_ambiguity,
    'Does the Gandhian reading fully abolish caste hierarchy or merely reinterpret varna duty as spiritual service without eliminating birth-ascription?',
    'Historical analysis of Gandhi''s writings on varna versus untouchability, cross-referenced with Ambedkarite critiques and post-Gandhian Dalit theological responses.',
    'If varna-ascription persists, the constraint extracts from Dalit communities through continued subordination disguised as spiritual equality, raising extractiveness and shifting victim-beneficiary boundaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gandhian_caste_ambiguity, conceptual, 'Ambiguity in Gandhian caste abolition versus varna reinterpretation').

omega_variable(
    suppression_of_literal_readings,
    'Is the delegitimization of literal readings structural suppression through institutional control of education and media, or merely argumentative disagreement without barrier?',
    'Study of educational curricula, textbook commissions, and institutional access for orthodox interpreters in postcolonial Indian universities and religious establishments.',
    'If structural suppression, effective suppression exceeds the scalar metric and the constraint trends toward snare; if open disagreement, the reading operates closer to rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_of_literal_readings, empirical, 'Structural versus discursive suppression of orthodox hermeneutics').

omega_variable(
    committer_foreclosure_validity,
    'Does the Gandhian reading''s allegorical premise logically foreclose the orthodox literal reading, or can syncretic frameworks hold both simultaneously?',
    'Logical analysis of core premises: nonviolence as supreme dharma versus divine command to physical warfare; examination of syncretic religious movements in India.',
    'If syncretism is possible, the reading_relations entry should shift from forecloses to coexists_with, altering the kernel''s contamination propagation topology.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_foreclosure_validity, conceptual, 'Whether Gandhian and orthodox readings are mutually exclusive or syncretically compatible').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gita_kurukshetra_discourse__gandhian_allegorical_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gita_gandhi_tr_t0, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(gita_gandhi_tr_t18, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 18, 0.2).
narrative_ontology:measurement(gita_gandhi_tr_t36, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 36, 0.25).
narrative_ontology:measurement(gita_gandhi_tr_t54, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 54, 0.3).
narrative_ontology:measurement(gita_gandhi_tr_t72, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 72, 0.35).
narrative_ontology:measurement(gita_gandhi_tr_t90, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 90, 0.3).

% Extraction over time
narrative_ontology:measurement(gita_gandhi_be_t0, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(gita_gandhi_be_t18, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 18, 0.4).
narrative_ontology:measurement(gita_gandhi_be_t36, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 36, 0.48).
narrative_ontology:measurement(gita_gandhi_be_t54, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 54, 0.46).
narrative_ontology:measurement(gita_gandhi_be_t72, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 72, 0.5).
narrative_ontology:measurement(gita_gandhi_be_t90, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 90, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(gita_gandhi_su_t0, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(gita_gandhi_su_t18, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 18, 0.3).
narrative_ontology:measurement(gita_gandhi_su_t36, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 36, 0.4).
narrative_ontology:measurement(gita_gandhi_su_t54, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 54, 0.45).
narrative_ontology:measurement(gita_gandhi_su_t72, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 72, 0.5).
narrative_ontology:measurement(gita_gandhi_su_t90, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 90, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gita_kurukshetra_discourse__gandhian_allegorical_reading, identity_coordination).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__gandhian_allegorical_reading, orthodox_literal_reading).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__gandhian_allegorical_reading, universalist_devotional_reading).

% DUAL FORMULATION NOTE:
% The gita_kurukshetra_discourse kernel decomposes into structurally distinct constraints. The orthodox literal reading extracts through caste and war legitimation; the universalist devotional reading coordinates through path-independent devotion; this Gandhian reading coordinates nonviolent resistance while extracting from Brahminical and militant authority. Each has distinct epsilon, beneficiaries, and directionality.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
