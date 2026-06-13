% ============================================================================
% CONSTRAINT STORY: unsc_242_withdrawal_clause__partial_withdrawal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unsc_242_withdrawal_clause__partial_withdrawal_reading, []).

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
 *   constraint_id: unsc_242_withdrawal_clause__partial_withdrawal_reading
 *   human_readable: UNSC 242 Withdrawal Clause — Partial Withdrawal Reading
 *   domain: international_law/diplomatic_history
 *
 * SUMMARY:
 *   United Nations Security Council Resolution 242 (1967) instructs
 *   'withdrawal of Israeli armed forces from territories occupied in the
 *   recent conflict.' The English text uses the indefinite article ('from
 *   territories') while the French working text uses the definite article
 *   ('des territoires' — all territories). This linguistic difference has
 *   generated three structurally distinct constraint stories: (1)
 *   interpretive_authority_structure — who may authoritatively resolve the
 *   ambiguity; (2) maximal_withdrawal_reading — withdrawal must be
 *   unconditional and complete per Charter Article 2(4); (3) this story —
 *   partial_withdrawal_reading, instantiating the occupying power's position
 *   that the indefinite article permits discretionary, phased withdrawal
 *   conditioned on 'secure boundaries' negotiations. The partial reading
 *   benefits the occupying power and mediating permanent members by
 *   converting textual ambiguity into sustained leverage. It extracts from
 *   dispossessed claimants by denying them a fixed withdrawal timeline. ε is
 *   moderate because the constraint is conditional (phased) and depends on
 *   active suppression of the maximal reading.
 *
 * KEY AGENTS:
 *   - occupying_military_power: Institutional power with arbitrage exit — holds the territory and interprets the clause in its favor; benefits from indefiniteness
 *   - dispossessed_claimants: Moderate power with constrained exit — seek unconditional return; victims of the reading's indefiniteness
 *   - mediating_permanent_members: Institutional power, analytical exit — drafted the ambiguity deliberately; benefit from control over interpretation
 *   - icj_as_potential_interpreter: Institutional analytical seat — possesses authority but lacks enforcement power to override Security Council
 *   - non_aligned_third_states: Organized power, constrained exit — excluded from binding interpretation; advocate for maximal reading but carry no enforcement weight
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.62).
domain_priors:suppression_score(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.58).
domain_priors:theater_ratio(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unsc_242_withdrawal_clause__partial_withdrawal_reading, tangled_rope).
narrative_ontology:human_readable(unsc_242_withdrawal_clause__partial_withdrawal_reading, "UNSC 242 Withdrawal Clause — Partial Withdrawal Reading").
narrative_ontology:topic_domain(unsc_242_withdrawal_clause__partial_withdrawal_reading, "international_law/diplomatic_history").

domain_priors:requires_active_enforcement(unsc_242_withdrawal_clause__partial_withdrawal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unsc_242_withdrawal_clause__partial_withdrawal_reading, 'cb5e2002-255a-45c2-ba4a-e1ec4dab4c87').
narrative_ontology:cs_kernel_codification('cb5e2002-255a-45c2-ba4a-e1ec4dab4c87', fixed_text).
narrative_ontology:cs_authority_grounding('cb5e2002-255a-45c2-ba4a-e1ec4dab4c87', extraction).
narrative_ontology:cs_interpretation_layer_present('cb5e2002-255a-45c2-ba4a-e1ec4dab4c87').
narrative_ontology:cs_reading_relation('cb5e2002-255a-45c2-ba4a-e1ec4dab4c87', unsc_242_withdrawal_clause__maximal_withdrawal_reading, coexists_with).
narrative_ontology:cs_reading_relation('cb5e2002-255a-45c2-ba4a-e1ec4dab4c87', unsc_242_withdrawal_clause__interpretive_authority_structure, influences).
narrative_ontology:cs_axiom('cb5e2002-255a-45c2-ba4a-e1ec4dab4c87', foundational, indefinite_article_as_intentional_discretion).
narrative_ontology:cs_axiom_status(indefinite_article_as_intentional_discretion, holdable).
narrative_ontology:cs_axiom_grounding('cb5e2002-255a-45c2-ba4a-e1ec4dab4c87', indefinite_article_as_intentional_discretion, empirically_contingent).
narrative_ontology:cs_axiom('cb5e2002-255a-45c2-ba4a-e1ec4dab4c87', foundational, secure_boundaries_permits_strategic_retention).
narrative_ontology:cs_axiom_status(secure_boundaries_permits_strategic_retention, holdable).
narrative_ontology:cs_axiom_grounding('cb5e2002-255a-45c2-ba4a-e1ec4dab4c87', secure_boundaries_permits_strategic_retention, deontological).
narrative_ontology:cs_reference_frame('cb5e2002-255a-45c2-ba4a-e1ec4dab4c87', multilateral_consensus_ambiguity_preservation).
narrative_ontology:cs_drift_state('cb5e2002-255a-45c2-ba4a-e1ec4dab4c87', contemporary_settlement_expansion_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('cb5e2002-255a-45c2-ba4a-e1ec4dab4c87', '').
narrative_ontology:cs_kernel_id(unsc_242_withdrawal_clause__partial_withdrawal_reading, unsc_242_withdrawal_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__partial_withdrawal_reading, occupying_military_power).
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__partial_withdrawal_reading, mediating_permanent_members).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__partial_withdrawal_reading, dispossessed_claimants).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__partial_withdrawal_reading, regional_alignment_pressured_states).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unsc_242_withdrawal_clause__partial_withdrawal_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(unsc_242_withdrawal_clause__partial_withdrawal_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unsc_242_withdrawal_clause__partial_withdrawal_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unsc_242_withdrawal_clause__partial_withdrawal_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unsc_242_withdrawal_clause__partial_withdrawal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness reaches 0.62 because the indefinite scope is the distributed resource: claimants cannot claim breach unless scope is fixed, so the constraint's persistence depends on keeping scope ambiguous. The occupying power collects negotiating leverage from that ambiguity. Suppression is moderate (0.58) because the reading requires active diplomatic suppression of the maximal-reading interpretation — this is not a constraint that persists passively. Theater ratio rises from 0.25 to 0.41 because over the 56-year interval, the 'secure boundaries' justification has become increasingly performative: security rationale cited initially for buffer zones has evolved into permanent settlement-building, suggesting the functional purpose (security) has been subordinated to the extraction purpose (territorial retention). The measurement series tracks one shared temporal grid: as extractiveness plateaus at 0.62 (the asymptote of what indefiniteness can deliver), theater rises (performance substituting for function) and suppression stabilizes (the enforcement effort needed to sustain the reading remains constant). The plateau in extractiveness after year 35 indicates the constraint has reached its maximum leverage — further gains would require either moving to a snare (declaring the clause unilaterally interpreted) or moving to a rope (converting temporary occupation into formal annexation), both of which would trigger explicit breach allegations.
 *
 * PERSPECTIVAL GAP:
 *   From the occupying power's seat, the reading is genuine coordination: the indefinite article permits flexibility, 'secure boundaries' is a real doctrine in international law (Waldock, ICJ opinions), and phased withdrawal with security guarantees is a reasonable resolution path. From the claimants' seat, the same structure operates as enforced extraction: the indefinite article was drafting accident or deliberate trap, 'secure boundaries' is cover for territorial appropriation, and phased withdrawal is indefinite delay of return. From the mediating permanent members' seat, the reading is pure leverage — they drafted the ambiguity, benefit from controlling interpretation, and have an interest in neither full enforcement (which would remove occupation) nor explicit violation (which would trigger crisis). The engine computes these divergences from the structural data: occupying power gets low d (benefits without running the full constraint); claimants get high d (targeted by the indefiniteness); mediators get moderate-to-low d (benefit from leverage without bearing extraction cost). The committer frame (Rules 3–4) routes the reading contest itself into the CS structure and omega variables rather than into the claim/metric relationship.
 *
 * DIRECTIONALITY LOGIC:
 *   Occupying power: Declared beneficiary (collects indefinite scope as negotiating leverage), institutional power, arbitrage exit (can reinterpret or move to annexation claim). Derives d near 0.1–0.2 (full beneficiary, though not passive collector — maintains the reading through diplomatic suppression). Dispossessed claimants: Declared victims (denied fixed withdrawal timeline), moderate power, constrained exit (cannot unilaterally terminate occupation, must negotiate). Derives d near 0.75–0.85 (targeted by the indefiniteness, constrained exit, power insufficient to force maximal reading). Mediating permanent members: Declared beneficiaries (control the interpretation authority, benefit from leverage preservation), institutional power, analytical exit (no direct costs, can shift positions). Derive d near 0.15–0.25. Non-aligned third states: Excluded from binding interpretation, organized power, constrained exit. Would have high d if targeted directly, but exclusion means the constraint operates without reference to them — they experience it as suppression of their preferred reading but not as direct extraction. No directionality override needed; the structural data produce the seat divergence directly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (1967 stalemate: occupying power's security needs vs. claimants' territorial integrity claims) remains contested in status. The occupying power attests it is still live; claimants and third states attest it is dead. The partial-withdrawal reading depends on this contestation: if the founding problem is declared dead, the reading becomes a snare (pure extraction with no coordination function). If declared live, it remains tangled_rope (security coordination + extraction). The divergence between founding_problem_status=contested and disappearance_verdict=world_rearranges is the mandatrophy signal: the constraint's disappearance WOULD rearrange the world (occupying power would face breach allegations, claimants could invoke Article 2(4) directly, mediators would lose leverage), but the founding problem that justified the constraint is no longer credibly live — the security threat has mutated into settlement expansion. This is a classic mandatrophy candidate: a coordinate structure whose mandate has shifted from solving a real coordination problem to protecting extraction. The reading's persistence depends on the permanent members and occupying power continuously suppressing the maximal reading and the ICJ's authority to rule — active suppression is the extraction mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deliberate_vs_accidental_indefiniteness,
    'Was the indefinite article in the English text a deliberate drafting choice to preserve both readings, or an accidental mismatch with the French definite article?',
    'Historical analysis of UNSC drafting records (UN archives, declassified diplomatic cables); testimony from living drafters or their documented intent statements; comparison of drafting timelines and who controlled English vs. French versions.',
    'If deliberate, the partial reading is the authorial intent and the constraint''s persistence is the intended outcome. If accidental, the partial reading is a false-summit beneficiary trap — a misreading of the drafters'' intent that extracts from claimants. If contested (evidence points both ways), the reading contest itself becomes the constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(deliberate_vs_accidental_indefiniteness, empirical, 'Whether textual indefiniteness was strategic or accidental in 242''s drafting.').

omega_variable(
    secure_boundaries_doctrine_scope,
    'Does ''secure boundaries'' under international law permit indefinite retention of occupied territory, or only temporary occupation pending agreement on boundary terms?',
    'ICJ jurisprudence on occupation and security (e.g., Legality of Threat or Use of Force, Wall opinion); customary international law analysis; state practice in comparable occupations (Kuwait, Crimea, post-WWII cases).',
    'If indefinite retention is permitted, the partial reading is doctrinally sound and the constraint is a genuine coordination mechanism (occupying power''s security vs. claimants'' return), making it tangled_rope rather than snare. If only temporary pending agreement, the reading becomes snare (extraction under cover of security doctrine).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secure_boundaries_doctrine_scope, empirical, 'Whether ''secure boundaries'' doctrine constrains or permits indefinite retention.').

omega_variable(
    reading_contest_vs_textual_ambiguity,
    'Is the constraint the textual ambiguity itself (indefinite article permits discretion) or the contest over which reading controls interpretation?',
    'If ICJ rules definitively on scope, textual ambiguity becomes resolved and the partial reading either wins or loses — the constraint shifts from ''indefiniteness as leverage'' to ''reading as fixed legal claim.'' If no ruling, the contest persists and the constraint remains the ambiguity.',
    'If the contest itself is the constraint (reading_relations: coexists_with between maximal and partial readings), then all three sibling stories are necessary to capture the full constraint family. If textual scope is the constraint, the partial reading stands alone as binding interpretation once certified. The committer structure (Rule 2) routes this into omega rather than modifying the claim or metrics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_vs_textual_ambiguity, conceptual, 'Whether the constraint is the ambiguous text or the reading contest over its interpretation.').

omega_variable(
    mediating_states_authority_vs_occupying_state_practice,
    'Do mediating permanent members (who drafted 242 with deliberate ambiguity) retain binding interpretive authority, or does occupying power''s actual practice over decades constitute customary interpretation?',
    'ICJ analysis of subsequent state practice (VCLT Article 31(3)(b)) — if occupying power has retained territory for 50+ years and other states have acquiesced or treated it as customary, practice may bind the interpretation. Alternatively, Security Council explicit statement reaffirming the interpretation.',
    'If practice binds, the partial reading becomes customary law and no longer depends on active suppression — it shifts from tangled_rope (requires enforcement) toward rope (coordination settled by accepted practice). If mediating authority remains primary, the reading depends on their continued suppression of maximal interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mediating_states_authority_vs_occupying_state_practice, empirical, 'Whether customary practice has settled the reading or mediating authority still controls.').

omega_variable(
    identity_locked_suppression_in_occupying_power,
    'Is the occupying power''s continued interpretation as indefinite scope locked into its institutional identity (security-state logic, permanent occupation as normalized state identity) such that exit from the partial reading would require identity transformation?',
    'Post-reading-shift analysis: if a court ruling or diplomatic settlement forced maximal interpretation, would the occupying power accept it as legitimate or would its institutions resist as identity-threatening? Does security doctrine reject return-to-pre-1967 borders as existentially incompatible with state identity?',
    'If identity-locked, the occupying power''s exit options are lower than ''arbitrage'' suggests — true exit would require institutional reconstitution. This would raise d and suppress alternatives more deeply than structural analysis captures. If merely institutional preference (arbitrage-grade exit preserved), the original d remains accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_suppression_in_occupying_power, empirical, 'Whether occupying power''s institutional identity is locked into the partial reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0, 56).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unsc_tr_t0, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(unsc_tr_t7, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 7, 0.28).
narrative_ontology:measurement(unsc_tr_t14, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 14, 0.32).
narrative_ontology:measurement(unsc_tr_t21, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 21, 0.36).
narrative_ontology:measurement(unsc_tr_t28, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 28, 0.39).
narrative_ontology:measurement(unsc_tr_t35, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 35, 0.4).
narrative_ontology:measurement(unsc_tr_t42, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 42, 0.41).
narrative_ontology:measurement(unsc_tr_t56, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 56, 0.41).

% Extraction over time
narrative_ontology:measurement(unsc_be_t0, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(unsc_be_t7, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 7, 0.52).
narrative_ontology:measurement(unsc_be_t14, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 14, 0.57).
narrative_ontology:measurement(unsc_be_t21, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 21, 0.6).
narrative_ontology:measurement(unsc_be_t28, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 28, 0.61).
narrative_ontology:measurement(unsc_be_t35, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 35, 0.62).
narrative_ontology:measurement(unsc_be_t42, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 42, 0.62).
narrative_ontology:measurement(unsc_be_t56, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 56, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(unsc_su_t0, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 0, 0.44).
narrative_ontology:measurement(unsc_su_t7, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 7, 0.47).
narrative_ontology:measurement(unsc_su_t14, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 14, 0.51).
narrative_ontology:measurement(unsc_su_t21, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 21, 0.54).
narrative_ontology:measurement(unsc_su_t28, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 28, 0.57).
narrative_ontology:measurement(unsc_su_t35, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 35, 0.58).
narrative_ontology:measurement(unsc_su_t42, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 42, 0.58).
narrative_ontology:measurement(unsc_su_t56, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 56, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unsc_242_withdrawal_clause__partial_withdrawal_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.12).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__partial_withdrawal_reading, unsc_242_withdrawal_clause__maximal_withdrawal_reading).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__partial_withdrawal_reading, unsc_242_withdrawal_clause__interpretive_authority_structure).

% DUAL FORMULATION NOTE:
% The UNSC 242 withdrawal clause decomposes into three structurally distinct constraints per the ε-invariance principle (DP-001). All three share a kernel (the text of 242, specifically the indefinite vs. definite article tension) but instantiate different ε values and beneficiary structures: (1) partial_withdrawal_reading (this story, ε=0.62) — indefiniteness permits discretionary retention; (2) maximal_withdrawal_reading (sibling, ε lower due to default to Article 2(4)) — indefiniteness is overridden by mandatory-return default; (3) interpretive_authority_structure (sibling, ε varies by seat) — who may authoritatively resolve the scope. The three readings are causally linked: the partial reading influences (creates structural pressure on) the maximal reading by occupying the narrative space, and both influence the authority-structure reading by creating demand for interpretation. The network captures the ε-invariance decomposition: each reading is a separate, stable constraint; together they form a family modeling the contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unsc_242_withdrawal_clause__partial_withdrawal_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
