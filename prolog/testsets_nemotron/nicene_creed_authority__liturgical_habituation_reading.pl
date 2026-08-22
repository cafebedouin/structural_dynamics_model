% ============================================================================
% CONSTRAINT STORY: nicene_creed_authority__liturgical_habituation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nicene_creed_authority__liturgical_habituation_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: nicene_creed_authority__liturgical_habituation_reading
 *   human_readable: Nicene Creed as Liturgical Habituation Boundary
 *   domain: systematic_theology/ecclesiology
 *
 * SUMMARY:
 *   This constraint models the Nicene Creed as it functions in actual
 *   liturgical practice: a weekly recited formula that marks the boundary of
 *   the worshipping community through habitual performance. The reading draws
 *   on the ancient principle lex orandi, lex credendi (the law of praying is
 *   the law of believing) but inverts the usual causal arrow — belief is
 *   formed through prayer, not prayer expressing prior belief. The Creed's
 *   words are largely unchanged since 381, but their function as identity
 *   boundary operates independently of whether reciters hold the specific
 *   metaphysical commitments the strict_orthodox_reading demands or the
 *   symbolic_confessional_reading historicizes. This is a coordination
 *   mechanism (rope) with very low extractiveness: the cost of recitation is
 *   minimal, the coordination return (recognizable Christian identity across
 *   1700 years) is massive, and no party collects rents from the operation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nicene_creed_authority__liturgical_habituation_reading, 0.07).
domain_priors:suppression_score(nicene_creed_authority__liturgical_habituation_reading, 0.12).
domain_priors:theater_ratio(nicene_creed_authority__liturgical_habituation_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, extractiveness, 0.07).
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nicene_creed_authority__liturgical_habituation_reading, rope).
narrative_ontology:human_readable(nicene_creed_authority__liturgical_habituation_reading, "Nicene Creed as Liturgical Habituation Boundary").
narrative_ontology:topic_domain(nicene_creed_authority__liturgical_habituation_reading, "systematic_theology/ecclesiology").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nicene_creed_authority__liturgical_habituation_reading, '49d1c483-a590-4a23-9d79-ca06d0a0e9a2').
narrative_ontology:cs_kernel_codification('49d1c483-a590-4a23-9d79-ca06d0a0e9a2', fixed_text).
narrative_ontology:cs_authority_grounding('49d1c483-a590-4a23-9d79-ca06d0a0e9a2', lineage).
narrative_ontology:cs_interpretation_layer_present('49d1c483-a590-4a23-9d79-ca06d0a0e9a2').
narrative_ontology:cs_reading_relation('49d1c483-a590-4a23-9d79-ca06d0a0e9a2', nicene_creed_authority__strict_orthodox_reading, coexists_with).
narrative_ontology:cs_reading_relation('49d1c483-a590-4a23-9d79-ca06d0a0e9a2', nicene_creed_authority__symbolic_confessional_reading, coexists_with).
narrative_ontology:cs_axiom('49d1c483-a590-4a23-9d79-ca06d0a0e9a2', foundational, performance_constitutes_identity_prior_to_assent).
narrative_ontology:cs_axiom_status(performance_constitutes_identity_prior_to_assent, holdable).
narrative_ontology:cs_axiom_grounding('49d1c483-a590-4a23-9d79-ca06d0a0e9a2', performance_constitutes_identity_prior_to_assent, conventional).
narrative_ontology:cs_axiom('49d1c483-a590-4a23-9d79-ca06d0a0e9a2', foundational, lex_orandi_lex_credendi_operative_in_liturgy).
narrative_ontology:cs_axiom_status(lex_orandi_lex_credendi_operative_in_liturgy, holdable).
narrative_ontology:cs_axiom_grounding('49d1c483-a590-4a23-9d79-ca06d0a0e9a2', lex_orandi_lex_credendi_operative_in_liturgy, conventional).
narrative_ontology:cs_reference_frame('49d1c483-a590-4a23-9d79-ca06d0a0e9a2', patristic_liturgical_formation).
narrative_ontology:cs_drift_state('49d1c483-a590-4a23-9d79-ca06d0a0e9a2', contemporary_ecumenical_era, gap(authority_erosion, minor, true)).
narrative_ontology:cs_created_at('49d1c483-a590-4a23-9d79-ca06d0a0e9a2', '').
narrative_ontology:cs_kernel_id(nicene_creed_authority__liturgical_habituation_reading, nicene_creed_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nicene_creed_authority__liturgical_habituation_reading, liturgical_communities).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__liturgical_habituation_reading, ecumenical_dialogue_participants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(nicene_creed_authority__liturgical_habituation_reading, catechumens_and_new_members).
narrative_ontology:constraint_vindicates(nicene_creed_authority__liturgical_habituation_reading, lex_orandi_lex_credendi).
narrative_ontology:constraint_vindicates(nicene_creed_authority__liturgical_habituation_reading, sacramental_identity_formation).
narrative_ontology:constraint_vindicates(nicene_creed_authority__liturgical_habituation_reading, liturgical_coordination_independent_of_assent).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participate in weekly Eucharistic liturgy where the Creed is recited as habitual performance. The recitation structures communal identity across linguistic, cultural, and theological differences without requiring uniform metaphysical commitments. Members experience the Creed as 'what we do' rather than primarily 'what we think.' Exit requires leaving the liturgical tradition itself.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, liturgical_communities, beneficiary,
    organized, generational, constrained, global).

% Use shared liturgical recitation as common ground for theological dialogue across denominational lines. The performance creates a coordination surface that enables conversation about differences without requiring prior agreement on metaphysical referents. They can enter and exit dialogue initiatives without identity loss.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, ecumenical_dialogue_participants, beneficiary,
    moderate, biographical, mobile, global).

% Learn the Creed by rote before full cognitive assent develops. The habituation cost is real (memorization, public performance, social pressure to conform) but the extraction is low — the performance integrates them into the community's identity boundary. Exit is possible but socially costly during formation.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, catechumens_and_new_members, payer,
    powerless, biographical, constrained, local).

% Would require the Creed to function as a cognitive-metaphysical boundary test with sanctions for deviation. They are excluded from this reading's framing because the reading treats metaphysical assent as downstream of performance, not upstream. Their objection is structural: they cannot accept a Creed that does not bind conscience to specific ontological claims.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, strict_orthodox_enforcers, excluded,
    institutional, civilizational, identity_locked, global).

% Studies the Creed's reception history and notes that liturgical habituation precedes and outlasts specific metaphysical interpretations. Sees the constraint as a coordination mechanism that has persisted across 1700 years of doctrinal conflict precisely because it does not require resolved metaphysics.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, historical_theologian_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of maintaining recognizable Christian identity across time, geography, language, and doctrinal divergence by providing a shared performative act that structures communal boundaries without requiring cognitive uniformity.
% TRANSFER_FUNCTION: Moves minimal cognitive-linguistic effort (memorization and recitation) from participants to the community, returning a durable identity marker that survives metaphysical disagreement. The transfer is near-symmetric: low cost, high coordination return.
% ABSENT_VOICES: Strict metaphysical realists who require propositional assent as the condition of authentic Christian identity. They are absent because this reading's frame — performance precedes and constitutes belief — makes their demand for prior cognitive assent structurally invisible. They exist in the strict_orthodox_reading constraint.
% DISAPPEARANCE_RATIONALE: If liturgical recitation of the Creed disappeared overnight, ecumenical dialogue would lose its most universal shared practice, catechetical formation would lose its primary performative anchor, and the visible boundary between 'Christian liturgical communities' and 'non-Christian religious gatherings' would blur. The coordination substrate for both stricter and looser readings would degrade.
% FOUNDING_PROBLEM: How to maintain a unified Christian identity across the Roman Empire's linguistic, cultural, and philosophical diversity without requiring every believer to master Greek metaphysics or submit to centralized doctrinal enforcement.
% FOUNDING_PROBLEM_CORROBORATION: Patristic scholars (e.g., Augustine, Cyril of Jerusalem) document that the Creed entered liturgy as a baptismal interrogatory before becoming a Eucharistic acclamation — the performative use preceded the conciliar definition. Modern liturgical historians (Bradshaw, Johnson) confirm the Creed's habitual recitation has functioned as identity boundary across doctrinal schisms. No single beneficiary group controls this attestation.
narrative_ontology:disappearance_verdict(nicene_creed_authority__liturgical_habituation_reading, world_rearranges).
narrative_ontology:founding_problem_status(nicene_creed_authority__liturgical_habituation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nicene_creed_authority__liturgical_habituation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(nicene_creed_authority__liturgical_habituation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nicene_creed_authority__liturgical_habituation_reading, 0.07, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nicene_creed_authority__liturgical_habituation_reading_tests).
:- end_tests(nicene_creed_authority__liturgical_habituation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.07) because the constraint demands only performative participation, not cognitive surrender or material tribute. Suppression is low (0.12) because alternatives exist (non-creedal worship, silent dissent, denominational switching) and the constraint does not actively suppress them — it simply provides a coordination surface others may use or ignore. Theater ratio is moderate (0.25) because the performance has accumulated ceremonial weight over centuries that exceeds its minimal functional core, but this is aesthetic accretion, not extraction. Accessibility collapse is moderate (0.35) because one can participate in Christian community without reciting the Creed (e.g., Quakers, some Baptists), but the Creed remains the dominant boundary marker. Resistance is low (0.2) because the constraint is voluntarily adopted by communities that find it useful.
 *
 * PERSPECTIVAL GAP:
 *   The strict_orthodox_reading experiences the same liturgical act as high-extraction coercion (it demands metaphysical assent under penalty of heresy). The symbolic_confessional_reading experiences it as voluntary symbolic participation. This reading experiences it as low-cost coordination. The divergence comes from what each reading treats as the constraint's referent: cognitive content vs. communal discernment vs. performative habit. The engine computes this from the different beneficiary/victim structures each reading authors.
 *
 * DIRECTIONALITY LOGIC:
 *   Liturgical communities and ecumenical participants are beneficiaries (d ~ 0.1-0.2): they gain a coordination mechanism at minimal cost. Catechumens are mild payers (d ~ 0.4-0.5): they bear the habituation cost but receive identity integration in return. Strict orthodox enforcers are excluded (not in the coordination game of this reading) — their identity_locked exit reflects that their framework cannot accommodate this reading. The analytical observer sits at d=0.5 (symmetric). The engine will compute per-seat effective extraction from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (unified identity across diversity without centralized enforcement) remains live — the Creed still coordinates Christian identity globally without a pope for Protestants or a patriarch for all Orthodox. No mandatrophy: the coordination function has not atrophied. The theater accretion is real but does not indicate functional decay; ceremonial weight can serve coordination by making the boundary more salient.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    performance_assent_causal_direction,
    'Does liturgical performance genuinely form metaphysical belief (lex orandi -> lex credendi), or does it merely express pre-existing belief in a way that creates the illusion of formation?',
    'Longitudinal cognitive-developmental studies of catechumens measuring belief change before/during/after liturgical habituation; comparative studies of communities with/without creedal recitation.',
    'If performance forms belief, this reading''s low extractiveness is structural (the constraint builds what it coordinates). If performance merely expresses belief, the constraint is parasitic on a formation process located elsewhere — extractiveness may be understated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performance_assent_causal_direction, empirical, 'Causal direction between liturgical habituation and cognitive metaphysical assent.').

omega_variable(
    kernel_reading_framing_underdetermination,
    'Is the liturgical_habituation_reading a genuine third frame, or a reductive description that collapses the strict_orthodox and symbolic_confessional readings into a behavioral substrate they both already presuppose?',
    'Test whether each sibling reading can operate without the performative substrate. If strict_orthodox enforcement requires liturgical recitation as its vehicle, and symbolic_confessional discernment requires shared liturgical memory as its archive, the substrate reading is structural, not reductive.',
    'If reductive, this constraint is not a separate reading but a meta-description of the kernel''s material conditions — ε-invariance would require merging it with the kernel level. If structural, it is a genuine coordination constraint with its own ε.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_framing_underdetermination, conceptual, 'Whether the performative frame is a distinct constraint or a reductive meta-description of the kernel''s material substrate.').

omega_variable(
    strict_orthodox_reading_commitment_frame,
    'Does the strict_orthodox_reading foreclose this reading within a single commitment framework, or do they coexist as different parties'' live positions?',
    'Examine whether any historical community has simultaneously maintained: (a) the Creed as liturgical habit forming identity, and (b) the Creed as cognitive boundary test with sanctions. If yes, they coexist. If the strict_orthodox position logically requires rejecting the habituation frame as ''empty ritual,'' they foreclose.',
    'Foreclosure would mean the kernel cannot hold both readings in one framework — the strict_orthodox_reading would treat this reading''s axioms as heretical. Coexistence means the kernel supports both as live positions across different communities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strict_orthodox_reading_commitment_frame, conceptual, 'Structural relationship between this reading and the strict_orthodox_reading: forecloses vs. coexists_with.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nicene_creed_authority__liturgical_habituation_reading, 325, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nice_tr_t325, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 325, 0.1).
narrative_ontology:measurement(nice_tr_t451, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 451, 0.15).
narrative_ontology:measurement(nice_tr_t1054, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 1054, 0.2).
narrative_ontology:measurement(nice_tr_t1517, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 1517, 0.25).
narrative_ontology:measurement(nice_tr_t1962, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 1962, 0.22).
narrative_ontology:measurement(nice_tr_t2025, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 2025, 0.25).

% Extraction over time
narrative_ontology:measurement(nice_be_t325, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 325, 0.05).
narrative_ontology:measurement(nice_be_t451, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 451, 0.06).
narrative_ontology:measurement(nice_be_t1054, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 1054, 0.07).
narrative_ontology:measurement(nice_be_t1517, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 1517, 0.06).
narrative_ontology:measurement(nice_be_t1962, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 1962, 0.07).
narrative_ontology:measurement(nice_be_t2025, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 2025, 0.07).

% Suppression requirement over time
narrative_ontology:measurement(nice_su_t325, nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 325, 0.1).
narrative_ontology:measurement(nice_su_t451, nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 451, 0.12).
narrative_ontology:measurement(nice_su_t1054, nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 1054, 0.15).
narrative_ontology:measurement(nice_su_t1517, nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 1517, 0.1).
narrative_ontology:measurement(nice_su_t1962, nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 1962, 0.08).
narrative_ontology:measurement(nice_su_t2025, nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 2025, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nicene_creed_authority__liturgical_habituation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(nicene_creed_authority__liturgical_habituation_reading, 0.08).
narrative_ontology:affects_constraint(nicene_creed_authority__liturgical_habituation_reading, nicene_creed_authority__strict_orthodox_reading).
narrative_ontology:affects_constraint(nicene_creed_authority__liturgical_habituation_reading, nicene_creed_authority__symbolic_confessional_reading).

% DUAL FORMULATION NOTE:
% This constraint is the liturgical_habituation_reading of the nicene_creed_authority kernel. The strict_orthodox_reading treats the Creed as a cognitive-metaphysical boundary with sanctions (high ε, snare/tangled_rope). The symbolic_confessional_reading treats it as a historically contingent witness grounded in community discernment (moderate ε, scaffold/rope). This reading provides the performative substrate both siblings interpret: the strict reading overlays metaphysical enforcement on the habit; the symbolic reading historicizes the habit. All three share the same liturgical act but author different beneficiary/victim structures and different ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nicene_creed_authority__liturgical_habituation_reading, organized, 0.15).
constraint_indexing:directionality_override(nicene_creed_authority__liturgical_habituation_reading, powerless, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
