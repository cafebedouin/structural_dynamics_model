% ============================================================================
% CONSTRAINT STORY: gita_kurukshetra_discourse__gandhian_allegorical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: gita_kurukshetra_discourse__gandhian_allegorical_reading
 *   human_readable: Gandhian Allegorical Reading of the Gita's Kurukshetra Discourse
 *   domain: religious_studies/textual_hermeneutics/ethical_philosophy
 *
 * SUMMARY:
 *   The Gandhian allegorical reading of the Bhagavad Gita treats the
 *   Kurukshetra battlefield as a metaphor for the internal psychic and moral
 *   struggle of every individual. It repudiates the divine mandate for caste
 *   hierarchy and physical violence, elevating ahimsa (non-violence) as the
 *   supreme principle and shifting interpretive authority from Brahminical
 *   scholars to individual moral conscience. As a commitment system
 *   constraint, this reading organizes a massive social movement, but also
 *   redistributes authority and demands severe moral discipline.
 *
 * KEY AGENTS:
 *   - gandhian_hermeneutic_community: Agenda-setter (organized/identity_locked) â propagates the allegorical reading, enforces non-violent discipline, administers ashram-based interpretive authority.
 *   - subaltern_caste_communities: Primary beneficiary (powerless/constrained) â liberated from divine caste mandate, granted spiritual equality through reinterpreted scripture.
 *   - brahminical_scholarly_establishment: Primary payer (institutional/constrained) â bears loss of interpretive monopoly and ritual gatekeeping authority.
 *   - militant_literalist_interpreters: Excluded voice (organized/constrained) â advocates physical warfare reading and caste duty, structurally delegitimized by the allegorical frame.
 *   - lay_spiritual_seekers: Secondary beneficiary (moderate/mobile) â gains direct access to textual meaning without priestly mediation.
 *   - critical_religious_studies_scholars: Analytical observer (analytical/analytical) â evaluates the reading's historical effects and structural commitments.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gita_kurukshetra_discourse__gandhian_allegorical_reading, 0.48).
domain_priors:suppression_score(gita_kurukshetra_discourse__gandhian_allegorical_reading, 0.42).
domain_priors:theater_ratio(gita_kurukshetra_discourse__gandhian_allegorical_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gita_kurukshetra_discourse__gandhian_allegorical_reading, tangled_rope).
narrative_ontology:human_readable(gita_kurukshetra_discourse__gandhian_allegorical_reading, "Gandhian Allegorical Reading of the Gita's Kurukshetra Discourse").
narrative_ontology:topic_domain(gita_kurukshetra_discourse__gandhian_allegorical_reading, "religious_studies/textual_hermeneutics/ethical_philosophy").

domain_priors:requires_active_enforcement(gita_kurukshetra_discourse__gandhian_allegorical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gita_kurukshetra_discourse__gandhian_allegorical_reading, '249e0c16-59fa-4df3-94b3-aaf4c3939a47').
narrative_ontology:cs_kernel_codification('249e0c16-59fa-4df3-94b3-aaf4c3939a47', fixed_text).
narrative_ontology:cs_authority_grounding('249e0c16-59fa-4df3-94b3-aaf4c3939a47', practice).
narrative_ontology:cs_interpretation_layer_present('249e0c16-59fa-4df3-94b3-aaf4c3939a47').
narrative_ontology:cs_reading_relation('249e0c16-59fa-4df3-94b3-aaf4c3939a47', gita_kurukshetra_discourse__orthodox_literal_reading, forecloses).
narrative_ontology:cs_reading_relation('249e0c16-59fa-4df3-94b3-aaf4c3939a47', gita_kurukshetra_discourse__universalist_devotional_reading, coexists_with).
narrative_ontology:cs_axiom('249e0c16-59fa-4df3-94b3-aaf4c3939a47', foundational, ahimsa_as_supreme_dharma).
narrative_ontology:cs_axiom_status(ahimsa_as_supreme_dharma, holdable).
narrative_ontology:cs_axiom_grounding('249e0c16-59fa-4df3-94b3-aaf4c3939a47', ahimsa_as_supreme_dharma, deontological).
narrative_ontology:cs_axiom('249e0c16-59fa-4df3-94b3-aaf4c3939a47', foundational, kurukshetra_as_internal_psychic_battlefield).
narrative_ontology:cs_axiom_status(kurukshetra_as_internal_psychic_battlefield, holdable).
narrative_ontology:cs_axiom_grounding('249e0c16-59fa-4df3-94b3-aaf4c3939a47', kurukshetra_as_internal_psychic_battlefield, conventional).
narrative_ontology:cs_reference_frame('249e0c16-59fa-4df3-94b3-aaf4c3939a47', allegorical_nonviolent_dharma).
narrative_ontology:cs_drift_state('249e0c16-59fa-4df3-94b3-aaf4c3939a47', contemporary_hindutva_ascendancy, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('249e0c16-59fa-4df3-94b3-aaf4c3939a47', '').
narrative_ontology:cs_kernel_id(gita_kurukshetra_discourse__gandhian_allegorical_reading, gita_kurukshetra_discourse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__gandhian_allegorical_reading, subaltern_caste_communities).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__gandhian_allegorical_reading, lay_spiritual_seekers).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__gandhian_allegorical_reading, brahminical_scholarly_establishment).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__gandhian_allegorical_reading, ahimsa_as_supreme_dharma).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__gandhian_allegorical_reading, individual_moral_conscience_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Propagates the allegorical reading through ashram education, satyagraha training, and public discourse. Enforces non-violent discipline by expelling militants from the movement and delegitimizing literal martial interpretations. Draws moral authority from Gandhi's practice and the claim that the Gita's true message is internal struggle and caste equality.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, gandhian_hermeneutic_community, agenda_setter,
    organized, generational, identity_locked, national).

% Are taught that their caste status carries no divine sanction and that spiritual self-realization is open to all regardless of birth. Gain scriptural warrant for dignity and equality, though they may still bear the material and moral burden of practicing soul-force under oppression.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, subaltern_caste_communities, beneficiary,
    powerless, generational, constrained, national).

% Gain direct access to the text's meaning without Brahminical mediation, authorized to interpret duty through personal moral conscience rather than caste obligation or priestly instruction.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, lay_spiritual_seekers, beneficiary,
    moderate, biographical, mobile, national).

% Traditionally controlled access to Sanskrit knowledge, textual interpretation, and ritual legitimacy. This reading bypasses their lineage-based authority, distributing interpretive power to individual conscience and political practice, eroding their gatekeeping rents.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, brahminical_scholarly_establishment, payer,
    institutional, generational, constrained, national).

% Read the Gita as mandating righteous physical warfare and caste-based duty. Their interpretation is delegitimized within the Gandhian frame as a failure of spiritual understanding; they are excluded from the anti-colonial coalition when non-violent discipline is enforced.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, militant_literalist_interpreters, excluded,
    organized, generational, constrained, national).

% Analyze the Gandhian reading as a historical intervention in Hindu political theology, tracking how the allegorical method redistributed authority and how its institutionalization has shifted since independence.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__gandhian_allegorical_reading, critical_religious_studies_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gita_kurukshetra_discourse__gandhian_allegorical_reading, gandhian_hermeneutic_community).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified non-violent ethical framework for anti-colonial resistance and anti-caste reform by reinterpreting the central Hindu scripture as a manual of internal spiritual discipline rather than social hierarchy and physical warfare.
% TRANSFER_FUNCTION: Moves interpretive authority from Brahminical scholarly lineages to individual moral conscience; moves the locus of legitimate struggle from external physical violence to internal self-discipline; moves caste-oppressed communities from a position of divinely mandated subordination to spiritual equality.
% ABSENT_VOICES: Militant anti-colonial revolutionaries who read the Gita as sanctioning armed resistance, and Brahminical orthodoxy that insists on caste-based duty and textual literalism, are structurally excluded from the Gandhian interpretive community; they would argue for the legitimacy of physical force and hierarchical social order.
% DISAPPEARANCE_RATIONALE: If the allegorical reading vanished overnight, the anti-caste reform and non-violent discipline frameworks that depend on it would lose their primary scriptural anchor; Brahminical literal readings would regain dominance in Hindu political theology; the distinction between internal and external struggle would collapse, rearranging the ethical architecture of millions of practitioners and the historiography of the independence movement.
% FOUNDING_PROBLEM: Colonial rule and caste oppression were legitimated by religious orthodoxy; the Indian masses needed an indigenous ethical framework that authorized non-violent resistance and social equality without requiring rejection of Hindu scriptural tradition.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by anti-caste intellectuals outside the Gandhian movement who acknowledge the problem but dispute the Gandhian solution; corroborated by colonial historians documenting the need for mass mobilization frameworks; Hindu nationalist historians deny the founding problem was genuine, asserting alternative militant frameworks were available.
narrative_ontology:disappearance_verdict(gita_kurukshetra_discourse__gandhian_allegorical_reading, world_rearranges).
narrative_ontology:founding_problem_status(gita_kurukshetra_discourse__gandhian_allegorical_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gita_kurukshetra_discourse__gandhian_allegorical_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is moderate (0.48) because the reading imposes significant costs on traditional authority holders and demands sustained moral discipline from practitioners, while delivering substantial coordination benefits in anti-caste and anti-colonial solidarity. Suppression (0.42) reflects the active delegitimization of literal martial and casteist interpretations within the Gandhian sphere, though these alternatives persist outside it. Accessibility collapse (0.65) is high for adherents but bounded by India's pluralistic religious landscape. Resistance (0.58) is substantial from orthodox institutions and militant nationalist currents. The measurement series show slowly accumulating extraction and rising theater as the reading transitions from movement praxis to institutionalized national ideology, with performative maintenance (commemorative spinning, ritualized non-violence) growing relative to original transformative function.
 *
 * PERSPECTIVAL GAP:
 *   From within the Gandhian seat, the constraint appears as necessary coordination to liberate spirituality from priestcraft and violence; from the Brahminical seat, it appears as an extractive displacement of legitimate traditional authority; from the subaltern seat, it is experienced as simultaneously liberatory and potentially burdensome, as the demand to practice soul-force can function as a demand to absorb suffering without physical resistance.
 *
 * DIRECTIONALITY LOGIC:
 *   The Gandhian hermeneutic community sits near the beneficiary end through accumulated moral authority and leadership of a mass movement; subaltern castes and lay seekers are structural beneficiaries (low d) through liberation from caste mandate and democratized access; the Brahminical scholarly establishment is the primary payer (high d) through loss of interpretive monopoly and ritual authority.
 *
 * MANDATROPHY ANALYSIS:
 *   The reading prevents mislabeling by maintaining a genuine coordination functionâmass anti-colonial mobilization and anti-caste solidarityâalongside asymmetric extractionâauthority displacement from Brahminical gatekeepers and moral discipline costs borne by practitioners. Without the coordination component, it would be a snare of charismatic extraction; without the extraction component, it would be a pure rope of theological innovation. The mandatrophy question arises post-independence, where the reading persisted as state ideology and educational curriculum without the original liberation function, increasing theater ratio and suggesting partial piton drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    subaltern_cost_of_ahimsa,
    'Does the Gandhian non-violent discipline extract disproportionate moral and material suffering from subaltern communities who face ongoing structural violence, by demanding soul-force instead of physical resistance?',
    'Comparative historical analysis of Dalit self-assertion movements: where subaltern communities rejected Gandhian ahimsa in favor of direct political confrontation, did material and dignitary outcomes improve relative to Gandhian-managed reform?',
    'If the cost is borne disproportionately by subaltern communities, their directionality shifts toward the target end and the constraint''s coordination framing becomes a more extractive cover story; if the cost is symmetric, the reading remains a genuine coordination mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subaltern_cost_of_ahimsa, empirical, 'Whether ahimsa discipline imposes asymmetric costs on the already oppressed.').

omega_variable(
    allegory_as_recovery_or_construction,
    'Is the allegorical reading a genuine recovery of the text''s intended meaning, or a strategic political construction that uses the text''s prestige to authorize a modern ethical program?',
    'Philological and historical-critical analysis of pre-Gandhian commentarial traditions and the Gita''s composition context, weighed against the political conditions of early twentieth-century anti-colonial mobilization.',
    'If purely strategic, the constraint''s natural-law pretense collapses and it reclassifies toward snare; if a recoverable valid interpretation, the coordination function retains legitimacy and it stays tangled_rope or rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(allegory_as_recovery_or_construction, conceptual, 'Whether the allegorical method is hermeneutically legitimate or instrumental appropriation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gita_kurukshetra_discourse__gandhian_allegorical_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gita_tr_t0, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(gita_tr_t20, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(gita_tr_t40, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 40, 0.25).
narrative_ontology:measurement(gita_tr_t60, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 60, 0.35).
narrative_ontology:measurement(gita_tr_t80, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 80, 0.42).
narrative_ontology:measurement(gita_tr_t100, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 100, 0.5).

% Extraction over time
narrative_ontology:measurement(gita_be_t0, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(gita_be_t20, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(gita_be_t40, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 40, 0.48).
narrative_ontology:measurement(gita_be_t60, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 60, 0.5).
narrative_ontology:measurement(gita_be_t80, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 80, 0.52).
narrative_ontology:measurement(gita_be_t100, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 100, 0.55).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(gita_kurukshetra_discourse__gandhian_allegorical_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gita_kurukshetra_discourse__gandhian_allegorical_reading, identity_coordination).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__gandhian_allegorical_reading, orthodox_literal_reading).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__gandhian_allegorical_reading, universalist_devotional_reading).

% DUAL FORMULATION NOTE:
% The gita_kurukshetra_discourse kernel decomposes into at least three structurally distinct constraints: the orthodox_literal_reading treats the text as mandating caste duty and righteous war; the gandhian_allegorical_reading treats it as interiorized non-violent discipline; the universalist_devotional_reading treats it as path-independent devotion. Their epsilon values, beneficiary structures, and authority groundings differ widely, necessitating separate constraint stories linked as a family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
