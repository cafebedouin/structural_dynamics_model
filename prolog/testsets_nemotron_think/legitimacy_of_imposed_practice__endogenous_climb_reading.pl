% ============================================================================
% CONSTRAINT STORY: legitimacy_of_imposed_practice__endogenous_climb_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_imposed_practice__endogenous_climb_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: legitimacy_of_imposed_practice__endogenous_climb_reading
 *   human_readable: State Imposition of Calendar and Dress Codes (Endogenous Climb Reading)
 *   domain: political/historical/cultural
 *
 * SUMMARY:
 *   A republican state imposes solar calendar and Western dress codes by
 *   decree, framing them as modernization necessities. The
 *   endogenous_climb_reading evaluates this imposition as a constraint that
 *   extracts compliance without achieving internalization. Lunar observance
 *   persists privately for decades; urban adoption is instrumental, not
 *   internalized. The constraint operates as a snare: the coordination story
 *   (administrative efficiency) is cover for extraction of cultural autonomy;
 *   persistence depends on active suppression of alternatives; traditional
 *   communities are identifiable victims. The claimed type (snare) and
 *   metrics are authored independently — the engine will compute per-seat
 *   classifications from the structural data.
 *
 * KEY AGENTS:
 *   - state_modernizers: Primary agenda_setter (institutional/arbitrage) — designs and enforces the imposition, collects administrative legibility
 *   - traditional_communities: Primary payer (organized/identity_locked) — bears cultural disruption, religious dislocation, identity costs
 *   - urban_adopters: Dual-positioned beneficiary/payer (moderate/constrained) — gains market access, loses cultural coherence
 *   - state_bureaucracy: Beneficiary (institutional/constrained) — career advancement through enforcement machinery
 *   - historical_observers: Observer (analytical/analytical) — sees full structure, assesses whether constraint achieved its stated function
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.75).
domain_priors:suppression_score(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.8).
domain_priors:theater_ratio(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_imposed_practice__endogenous_climb_reading, snare).
narrative_ontology:human_readable(legitimacy_of_imposed_practice__endogenous_climb_reading, "State Imposition of Calendar and Dress Codes (Endogenous Climb Reading)").
narrative_ontology:topic_domain(legitimacy_of_imposed_practice__endogenous_climb_reading, "political/historical/cultural").

domain_priors:requires_active_enforcement(legitimacy_of_imposed_practice__endogenous_climb_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_imposed_practice__endogenous_climb_reading, '23a4f10a-3b85-4408-b5d6-f5025fb5b68a').
narrative_ontology:cs_kernel_codification('23a4f10a-3b85-4408-b5d6-f5025fb5b68a', formalized).
narrative_ontology:cs_authority_grounding('23a4f10a-3b85-4408-b5d6-f5025fb5b68a', extraction).
narrative_ontology:cs_interpretation_layer_present('23a4f10a-3b85-4408-b5d6-f5025fb5b68a').
narrative_ontology:cs_reading_relation('23a4f10a-3b85-4408-b5d6-f5025fb5b68a', legitimacy_of_imposed_practice__exogenous_override_reading, forecloses).
narrative_ontology:cs_reading_relation('23a4f10a-3b85-4408-b5d6-f5025fb5b68a', legitimacy_of_imposed_practice__hybrid_scaffolding_reading, influences).
narrative_ontology:cs_axiom('23a4f10a-3b85-4408-b5d6-f5025fb5b68a', foundational, internalization_necessary_for_practice_displacement).
narrative_ontology:cs_axiom_status(internalization_necessary_for_practice_displacement, holdable).
narrative_ontology:cs_axiom_grounding('23a4f10a-3b85-4408-b5d6-f5025fb5b68a', internalization_necessary_for_practice_displacement, empirically_contingent).
narrative_ontology:cs_axiom('23a4f10a-3b85-4408-b5d6-f5025fb5b68a', foundational, state_decree_insufficient_for_legitimate_practice_change).
narrative_ontology:cs_axiom_status(state_decree_insufficient_for_legitimate_practice_change, holdable).
narrative_ontology:cs_axiom_grounding('23a4f10a-3b85-4408-b5d6-f5025fb5b68a', state_decree_insufficient_for_legitimate_practice_change, deontological).
narrative_ontology:cs_reference_frame('23a4f10a-3b85-4408-b5d6-f5025fb5b68a', republican_modernization_decree).
narrative_ontology:cs_drift_state('23a4f10a-3b85-4408-b5d6-f5025fb5b68a', post_imposition_decades, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('23a4f10a-3b85-4408-b5d6-f5025fb5b68a', '').
narrative_ontology:cs_kernel_id(legitimacy_of_imposed_practice__endogenous_climb_reading, legitimacy_of_imposed_practice).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__endogenous_climb_reading, state_modernizers).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__endogenous_climb_reading, traditional_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__endogenous_climb_reading, urban_adopters).
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__endogenous_climb_reading, state_bureaucracy).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__endogenous_climb_reading, urban_adopters).
narrative_ontology:constraint_vindicates(legitimacy_of_imposed_practice__endogenous_climb_reading, administrative_legibility_thesis).
narrative_ontology:constraint_vindicates(legitimacy_of_imposed_practice__endogenous_climb_reading, internalization_necessity_for_cultural_change).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and enforce calendar reform (solar replacing lunar) and dress codes (Western replacing traditional) as pillars of modernization. They control the legislative and coercive apparatus, benefit from administrative legibility and national unification narrative, and can shift strategy when enforcement costs exceed political capital.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, state_modernizers, agenda_setter,
    institutional, generational, arbitrage, global).

% Bear the costs of disrupted religious observance (lunar calendar governs festivals, fasting, pilgrimage), social cohesion, and identity. Public compliance is enforced; private retention of lunar reckoning and traditional dress persists for decades. Exit means abandoning communal identity — the practices are constitutive, not optional.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, traditional_communities, payer,
    organized, biographical, identity_locked, regional).

% Adopt solar calendar and Western dress for market access, education, and bureaucratic navigation. Gain coordination benefits in urban commerce and state interaction. Privately retain lunar calendar for family and religious life. Caught between two systems — neither fully internalized nor fully rejected.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, urban_adopters, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_imposed_practice__endogenous_climb_reading, urban_adopters, payer).

% Career advancement depends on enforcement metrics (compliance rates, violation reports). The imposition creates positions, budgets, and promotion pathways. They are not the authors of the policy but its operational beneficiaries — their interest is in the enforcement machinery, not the cultural outcome.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, state_bureaucracy, beneficiary,
    institutional, biographical, constrained, national).

% Analyze the imposition as a case study in state formation. They see the full structure: the decree, the resistance, the partial adoption, the private retention. Their seat is not in the conflict but in the retrospective assessment of whether the constraint achieved its stated coordination function.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, historical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The arrangement purports to coordinate society around modern temporal and bodily standards for administrative efficiency, national unity, and integration into global systems — a single calendar for commerce, bureaucracy, and diplomacy; a single dress code for civic equality and modern identity.
% TRANSFER_FUNCTION: Moves the burden of cultural transformation from the state to communities: extracts cultural autonomy, religious authority, and habitual practice; imposes administrative legibility and standardization. The state gains readable populations; communities lose the internal coherence of their lifeworld.
% ABSENT_VOICES: Rural and religious communities whose practices are displaced — village elders, mosque timekeepers, women who maintain domestic ritual calendars. They are in the villages, mosques, and homes, not in the capital's ministries or the urban press. Their objection is recorded only in the persistence of the old practices.
% DISAPPEARANCE_RATIONALE: If the imposition vanished overnight, communities would revert to lunar calendar and traditional dress within weeks — the practices never died, they went private. The state would lose the administrative legibility it built the imposition to achieve. The world rearranges because the constraint was holding a displaced equilibrium in place by force.
% FOUNDING_PROBLEM: The early republican state needed legible, uniform populations for taxation, conscription, public education, and legal administration in the modern era. The lunar calendar and diverse dress codes were illegible to central bureaucracy — they varied by region, sect, and season.
% FOUNDING_PROBLEM_CORROBORATION: Independent historians of state formation (Scott, Anderson, Bayly) corroborate the administrative legibility thesis from outside the state's own narrative. State archives corroborate the modernization imperative. Traditional communities' oral histories corroborate that the 'problem' was state-made — their practices were legible *to them* until the state required a different legibility.
narrative_ontology:disappearance_verdict(legitimacy_of_imposed_practice__endogenous_climb_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_imposed_practice__endogenous_climb_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_imposed_practice__endogenous_climb_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legitimacy_of_imposed_practice__endogenous_climb_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.75, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_imposed_practice__endogenous_climb_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_of_imposed_practice__endogenous_climb_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimacy_of_imposed_practice__endogenous_climb_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.75) because the state extracts compliance and cultural surrender without reciprocal benefit to communities. Suppression is high (0.8) because the constraint's persistence depends on continuous enforcement — penalties for lunar calendar use, dress code policing, bureaucratic refusal to recognize non-compliant documents. Theater ratio is moderate-high (0.6) because enforcement increasingly performs 'modernization' while the actual coordination function (internalized practice change) fails — the bureaucracy enforces the form while the substance persists privately. Accessibility collapse is low (0.3) because alternatives (lunar calendar, traditional dress) never collapsed — they went underground and persisted. Resistance is high (0.7) because communities actively maintain private practice, creating a dual-structure that the state cannot fully penetrate.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter seat (state_modernizers) experiences the constraint as a necessary coordination mechanism — they see administrative efficiency gains. The payer seat (traditional_communities) experiences it as extraction of identity — they see cultural destruction. The dual-positioned urban_adopters experience it as both: coordination benefit in public life, extraction in private life. The engine computes this divergence from power/exit/role structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   State_modernizers are structural beneficiaries (d near 0.0) — they control the constraint, collect its gains (legibility, unification narrative), and have arbitrage-grade exit (can modify or abandon the policy). Traditional_communities are structural targets (d near 1.0) — they bear the costs, have identity-locked exit (practices are constitutive of self), and face active suppression of alternatives. Urban_adopters sit near symmetric (d ~ 0.5) — genuine coordination benefit in public sphere, real extraction in private sphere. State_bureaucracy sits near beneficiary (d ~ 0.2) — they collect career capital from enforcement but cannot change the policy. Historical_observers are analytical (d = 0.5 by definition).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (administrative illegibility of diverse practices) was real from the state's perspective. The endogenous_climb_reading argues the mandate has outlived its function because the coordination mechanism (decree without internalization) never worked — the constraint persists as a snare because the state bureaucracy benefits from enforcement machinery, not because it achieves modernization. Mandatrophy is resolved in the reading's analysis: the arrangement persists despite its founding problem being either solved by other means (urban diffusion created partial legibility) or revealed as state-created. The constraint is now maintained by the enforcement apparatus's self-interest.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_structure_endogenous_climb,
    'This constraint is one reading (endogenous_climb_reading) of the contested kernel ''legitimacy_of_imposed_practice''. What structural elements do the sibling readings (exogenous_override_reading, hybrid_scaffolding_reading) change?',
    'Compare the three readings'' beneficiary/victim structures, claimed types, and metric profiles. The exogenous_override_reading would claim lower extractiveness (decree as coordination), higher accessibility_collapse (alternatives eliminated by law). The hybrid_scaffolding_reading would claim moderate extractiveness with time-declining theater_ratio (scaffolding works then withdraws).',
    'If exogenous_override_reading''s metric profile is empirically supported, this reading''s snare classification is contested — the constraint might be a rope or tangled_rope from a different structural framing. The kernel''s classification is reading-dependent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_endogenous_climb, conceptual, 'Commitment to kernel_id:legitimacy_of_imposed_practice, reading_id:endogenous_climb_reading; sibling readings change beneficiary structure and metric profiles').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.8) primarily structural (legal penalties, bureaucratic refusal) or internalized (communities policing themselves, believing the new practices are superior)?',
    'Post-reform suppression trajectory: if private retention persists after enforcement relaxes, suppression was structural not internalized. If private retention collapses when enforcement relaxes, internalization occurred despite appearances.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them. This would shift classification toward mountain (internalized coordination) or tangled_rope (coordination + extraction) rather than snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in cultural imposition').

omega_variable(
    snare_vs_failed_scaffold,
    'Is this constraint a snare (pure extraction with coordination as cover) or a scaffold that failed to sunset (transitional coordination that became permanent extraction)?',
    'Examine founding documents: did the modernizers declare a sunset condition (e.g., ''until literacy reaches X%'')? If yes, it was authored as scaffold. If no, it was authored as permanent — snare from inception.',
    'If scaffold-authored, mandatrophy_resolved = true and the constraint''s current form is degradation. If snare-authored, mandatrophy_resolved = false — extraction was the point. Changes the omega-resolution path for the kernel''s other readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(snare_vs_failed_scaffold, conceptual, 'Whether the constraint was authored as transitional (scaffold) or permanent (snare)').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_imposed_practice__endogenous_climb_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(legi_tr_t6, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 6, 0.5).
narrative_ontology:measurement(legi_tr_t12, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 12, 0.55).
narrative_ontology:measurement(legi_tr_t18, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 18, 0.6).
narrative_ontology:measurement(legi_tr_t24, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 24, 0.62).
narrative_ontology:measurement(legi_tr_t30, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 30, 0.6).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(legi_be_t6, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 6, 0.7).
narrative_ontology:measurement(legi_be_t12, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 12, 0.73).
narrative_ontology:measurement(legi_be_t18, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 18, 0.75).
narrative_ontology:measurement(legi_be_t24, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 24, 0.74).
narrative_ontology:measurement(legi_be_t30, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 30, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(legi_su_t6, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 6, 0.8).
narrative_ontology:measurement(legi_su_t12, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 12, 0.82).
narrative_ontology:measurement(legi_su_t18, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 18, 0.8).
narrative_ontology:measurement(legi_su_t24, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 24, 0.78).
narrative_ontology:measurement(legi_su_t30, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 30, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_imposed_practice__endogenous_climb_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.1).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__endogenous_climb_reading, legitimacy_of_imposed_practice__exogenous_override_reading).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__endogenous_climb_reading, legitimacy_of_imposed_practice__hybrid_scaffolding_reading).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__endogenous_climb_reading, state_legibility_project).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the kernel 'legitimacy_of_imposed_practice' into three readings with distinct ε values and structural profiles. The endogenous_climb_reading (this story) has high ε (0.75) and classifies as snare. The exogenous_override_reading claims low ε (decree as genuine coordination) and would classify as rope or mountain. The hybrid_scaffolding_reading claims moderate ε with time-declining theater and would classify as scaffold or tangled_rope. The ε-invariance principle requires separate stories because the observables (decree compliance vs. private practice vs. ideological internalization) yield different ε values for the same nominal constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legitimacy_of_imposed_practice__endogenous_climb_reading, institutional, 0.15).
constraint_indexing:directionality_override(legitimacy_of_imposed_practice__endogenous_climb_reading, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
