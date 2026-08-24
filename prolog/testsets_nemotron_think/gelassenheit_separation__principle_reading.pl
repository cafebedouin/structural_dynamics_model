% ============================================================================
% CONSTRAINT STORY: gelassenheit_separation__principle_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gelassenheit_separation__principle_reading, []).

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
 *   constraint_id: gelassenheit_separation__principle_reading
 *   human_readable: Gelassenheit Separation — Principle Reading (Functional Isolation Standard)
 *   domain: religious/commitment_system/technology_governance
 *
 * SUMMARY:
 *   The principle_reading of Gelassenheit separation holds that the core
 *   requirement is avoiding structural entanglement in worldly systems —
 *   state apparatus, market dependency, insurance regimes, digital networks.
 *   Technologies that can be operated fully off-grid and without ongoing
 *   external connection (solar panels, pneumatic tools, hydraulic systems)
 *   are permitted because they do not create structural ties. Technologies
 *   that inherently require connection to worldly infrastructure (internet,
 *   commercial insurance, grid electricity) are forbidden regardless of
 *   whether a particular household could isolate them. This reading is
 *   instantiated in several Amish districts (e.g., certain Swiss Amish, Andy
 *   Weaver) and contrasts with the artifact_reading (which forbids
 *   technologies that visually resemble worldly artifacts) and the
 *   consequence_reading (which evaluates by effect on visiting, mutual aid,
 *   and geographic rootedness). The constraint coordinates community boundary
 *   maintenance while extracting technological autonomy from members — a
 *   genuine coordination function with asymmetric extraction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gelassenheit_separation__principle_reading, 0.42).
domain_priors:suppression_score(gelassenheit_separation__principle_reading, 0.48).
domain_priors:theater_ratio(gelassenheit_separation__principle_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gelassenheit_separation__principle_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(gelassenheit_separation__principle_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(gelassenheit_separation__principle_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gelassenheit_separation__principle_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(gelassenheit_separation__principle_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gelassenheit_separation__principle_reading, tangled_rope).
narrative_ontology:human_readable(gelassenheit_separation__principle_reading, "Gelassenheit Separation — Principle Reading (Functional Isolation Standard)").
narrative_ontology:topic_domain(gelassenheit_separation__principle_reading, "religious/commitment_system/technology_governance").

domain_priors:requires_active_enforcement(gelassenheit_separation__principle_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gelassenheit_separation__principle_reading, 'baa0641e-308c-4e33-a694-ce6278e8a39b').
narrative_ontology:cs_kernel_codification('baa0641e-308c-4e33-a694-ce6278e8a39b', formalized).
narrative_ontology:cs_authority_grounding('baa0641e-308c-4e33-a694-ce6278e8a39b', lineage).
narrative_ontology:cs_interpretation_layer_present('baa0641e-308c-4e33-a694-ce6278e8a39b').
narrative_ontology:cs_reading_relation('baa0641e-308c-4e33-a694-ce6278e8a39b', gelassenheit_separation__artifact_reading, coexists_with).
narrative_ontology:cs_reading_relation('baa0641e-308c-4e33-a694-ce6278e8a39b', gelassenheit_separation__consequence_reading, coexists_with).
narrative_ontology:cs_axiom('baa0641e-308c-4e33-a694-ce6278e8a39b', foundational, structural_entanglement_forbidden).
narrative_ontology:cs_axiom_status(structural_entanglement_forbidden, holdable).
narrative_ontology:cs_axiom_grounding('baa0641e-308c-4e33-a694-ce6278e8a39b', structural_entanglement_forbidden, deontological).
narrative_ontology:cs_axiom('baa0641e-308c-4e33-a694-ce6278e8a39b', foundational, functional_isolation_sufficient).
narrative_ontology:cs_axiom_status(functional_isolation_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('baa0641e-308c-4e33-a694-ce6278e8a39b', functional_isolation_sufficient, deontological).
narrative_ontology:cs_reference_frame('baa0641e-308c-4e33-a694-ce6278e8a39b', gelassenheit_as_structural_separation).
narrative_ontology:cs_drift_state('baa0641e-308c-4e33-a694-ce6278e8a39b', contemporary_technology_pressure, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('baa0641e-308c-4e33-a694-ce6278e8a39b', '').
narrative_ontology:cs_kernel_id(gelassenheit_separation__principle_reading, gelassenheit_separation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gelassenheit_separation__principle_reading, church_elders).
narrative_ontology:constraint_beneficiary(gelassenheit_separation__principle_reading, bishop_ministry).
narrative_ontology:constraint_victim(gelassenheit_separation__principle_reading, ordinary_members).
narrative_ontology:constraint_victim(gelassenheit_separation__principle_reading, youth_seeking_modern_tools).
narrative_ontology:constraint_vindicates(gelassenheit_separation__principle_reading, gelassenheit_as_structural_yieldedness).
narrative_ontology:constraint_vindicates(gelassenheit_separation__principle_reading, functional_isolation_preserves_separation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bishops, ministers, and deacons who adjudicate technology requests in the twice-yearly Ordnung reviews. They interpret 'functional isolation' vs. 'structural entanglement' for each new technology. They bear no personal cost from prohibitions — their livelihoods are within the community economy. Their authority derives from ordination lineage and community consent. Exit is analytically available but vocationally and identity-wise infeasible.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, church_elders, agenda_setter,
    institutional, generational, analytical, regional).

% The ordained ministry collectively — they set the agenda for what technologies come before the congregation, frame the functional-isolation test, and benefit from the authority that boundary-maintenance confers. They do not personally pay the costs of forbidden technologies (medical bankruptcy risk without insurance, business competitiveness without internet).
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, bishop_ministry, agenda_setter,
    institutional, generational, analytical, regional).
narrative_ontology:stakeholder_secondary_role(gelassenheit_separation__principle_reading, bishop_ministry, beneficiary).

% Baptized adult members who live under the Ordnung. They bear the costs: no internet for business/education, no commercial insurance (relying on community mutual aid for catastrophic medical costs), no grid electricity. They participate in the twice-yearly communion where the Ordnung is reaffirmed. Exit means shunning (Meidung) — severing all social, economic, and familial ties. Their identity is constituted through the community; leaving is not a consumer choice but an existential rupture.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, ordinary_members, payer,
    organized, biographical, identity_locked, local).

% Young adults (typically 16-25, during Rumspringa or after baptism) who want to use internet for education, business, or communication, or who see insurance as prudent. They experience the extraction most acutely — the functional-isolation standard forbids tools their non-Amish peers use routinely. Their exit is constrained: they can leave, but face Meidung, loss of inheritance rights, and total social reconstruction. Some stay and comply resentfully; some leave and return; some leave permanently.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, youth_seeking_modern_tools, payer,
    moderate, biographical, constrained, local).

% Academics studying Amish technology adjudication, religious studies scholars, legal scholars examining religious freedom boundaries. They observe the constraint from outside, document the reading's coherence and drift, but have no stake in its enforcement or violation. Their exit is analytical — they can change research focus without personal cost.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, external_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains the community's structural separation from worldly systems (state, market, digital networks) while permitting technologies that can be fully operated off-grid without ongoing external dependency. Solves the problem: how to adopt useful tools without creating structural ties that undermine Gelassenheit (yieldedness to God's will over self-will).
% TRANSFER_FUNCTION: Moves technological autonomy from individual members to the collective adjudication authority. Forbids internet and commercial insurance even when a household could technically isolate them (e.g., air-gapped computer, self-insurance fund), transferring the option value of those technologies from members to the community's boundary-maintenance project. Permits solar, pneumatic, hydraulic tools — transferring the benefit of those technologies to members while keeping the coordination function intact.
% ABSENT_VOICES: Members who would adopt internet for telemedicine, online education, or small business if permitted; members who would purchase catastrophic health insurance if allowed; reform-minded members who argue the functional-isolation standard has become a moving target that always excludes the next useful technology. These voices are not in the adjudication room — the Ordnung review is elder-led, and dissent is channeled through private counsel, not public debate.
% DISAPPEARANCE_RATIONALE: If the principle_reading vanished overnight, the community would lose its primary structural boundary against state/market/digital entanglement. Districts holding this reading would either adopt the artifact_reading or consequence_reading (both stricter in different ways), or fracture — some members adopting unrestricted technology, others tightening to artifact-standard. The mobile phone / internet adoption curve in non-principle-reading districts shows rapid structural entanglement within a generation.
% FOUNDING_PROBLEM: How to engage with necessary technologies (for farming, craft, medical care) without creating structural entanglement in worldly systems — state regulation, market dependency, insurance regimes, digital networks — that would undermine Gelassenheit (submission to God's order over self-will) and the community's separation-from-the-world witness.
% FOUNDING_PROBLEM_CORROBORATION: Historical Anabaptist scholars (e.g., Donald Kraybill, Steven Nolt) attest the founding problem is live: the tension between necessary technology and structural separation is the permanent Amish condition, not a solved problem. The principle_reading's specific adjudication line (functional isolation) is a 20th-century formulation; the underlying problem persists. Current church elders attest it is live; youth who leave attest it is live (they experience the tension acutely). No external corroboration declares it dead.
narrative_ontology:disappearance_verdict(gelassenheit_separation__principle_reading, world_rearranges).
narrative_ontology:founding_problem_status(gelassenheit_separation__principle_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gelassenheit_separation__principle_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gelassenheit_separation__principle_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gelassenheit_separation__principle_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gelassenheit_separation__principle_reading_tests).
:- end_tests(gelassenheit_separation__principle_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) because the constraint forbids specific high-value technologies (internet, insurance) even when they could be functionally isolated, extracting option value from members. Suppression is moderate (0.48) — enforcement is real (confession, shunning) but calibrated; the community does not pursue total surveillance. Theater ratio is low (0.18) — the functional-isolation criterion is genuinely used in adjudication, not a veneer. Accessibility collapse is moderate (0.52) — members can leave (and some do), but identity-locked exit makes alternatives psychologically costly. Resistance is moderate (0.38) — youth retention pressures exist but open dissent is rare; the constraint's legitimacy is widely internalized.
 *
 * PERSPECTIVAL GAP:
 *   From the elders' seat, the constraint is a Rope — a genuine coordination mechanism preserving Gelassenheit with minimal coercion. From the ordinary member seat, it computes as Tangled Rope — coordination function real, but asymmetric extraction on technologies that could be isolated. From the youth seat, it approaches Snare — the coordination story feels like cover for control over life options. The engine computes this divergence from the structural data; the authored claim (tangled_rope) reflects the analyst's seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Church elders and bishop ministry are structural beneficiaries (agenda_setters): they control the adjudication boundary, gain authority from interpreting the principle, and face no personal cost from the prohibitions. Ordinary members are payers: they bear the cost of forbidden technologies (no internet for business, no insurance for medical/hospital costs) with identity_locked exit — leaving means severing family, community, and spiritual framework. Youth seeking modern tools are acute payers with constrained exit: they experience the extraction most sharply but lack the social capital to challenge the adjudication. External observers (scholars, neighboring districts) are analytical seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to engage necessary technology without structural entanglement) remains live — new technologies constantly press the boundary. The constraint has not atrophied into a Piton; active adjudication continues. However, the rising extractiveness trend (0.25→0.42 over 50 years) suggests mandate creep: the functional-isolation standard is being applied to forbid technologies that earlier would have been permitted, extracting more option value over time without a corresponding change in the founding problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'This constraint is the principle_reading of the gelassenheit_separation kernel — how does its ε and beneficiary/victim structure differ from the artifact_reading and consequence_reading?',
    'Compare the three readings'' ε values, suppression profiles, and victim sets side-by-side; the kernel''s contest is exactly the divergence in these structural properties across readings.',
    'If principle_reading shows lower ε than artifact_reading, the functional-isolation standard is structurally less extractive than the visual-distinction standard; if consequence_reading shows different victim sets, the community-practice test extracts from different members.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Committee-frame identity: this JSON is one reading of a contested kernel; sibling readings are separate constraint files.').

omega_variable(
    functional_isolation_boundary,
    'Where exactly is the line between ''functionally isolated'' (permitted: solar, pneumatic off-grid) and ''structurally entangling'' (forbidden: internet, insurance even if isolated)?',
    'Trace specific technology adjudications in district minutes: why solar panels are isolated but a standalone offline computer is not; why insurance is forbidden even when no payout connects to state systems.',
    'If the boundary is coherent (e.g., ''any technology requiring ongoing external infrastructure''), the reading has internal consistency; if ad hoc, the extraction is disguised as principle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(functional_isolation_boundary, conceptual, 'Whether the functional-isolation criterion has a stable structural definition or operates as a moving target.').

omega_variable(
    suppression_mechanism_identity_lock,
    'Is the suppression of forbidden technologies structural (church discipline, shunning) or internalized (members'' identity fused with Gelassenheit such that exit is unthinkable)?',
    'Post-exit trajectory study: do former members who adopt forbidden technologies report persistent internal suppression, or does it dissolve with structural removal?',
    'If internalized, effective suppression is higher than the structural measure — the constraint travels with the agent after exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_identity_lock, empirical, 'Structural vs. internalized suppression in an identity-locked community.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gelassenheit_separation__principle_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gelassenheit_principle_tr_t0, gelassenheit_separation__principle_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(gelassenheit_principle_tr_t10, gelassenheit_separation__principle_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(gelassenheit_principle_tr_t20, gelassenheit_separation__principle_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement(gelassenheit_principle_tr_t30, gelassenheit_separation__principle_reading, theater_ratio, 30, 0.15).
narrative_ontology:measurement(gelassenheit_principle_tr_t40, gelassenheit_separation__principle_reading, theater_ratio, 40, 0.17).
narrative_ontology:measurement(gelassenheit_principle_tr_t50, gelassenheit_separation__principle_reading, theater_ratio, 50, 0.18).

% Extraction over time
narrative_ontology:measurement(gelassenheit_principle_be_t0, gelassenheit_separation__principle_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(gelassenheit_principle_be_t10, gelassenheit_separation__principle_reading, base_extractiveness, 10, 0.3).
narrative_ontology:measurement(gelassenheit_principle_be_t20, gelassenheit_separation__principle_reading, base_extractiveness, 20, 0.35).
narrative_ontology:measurement(gelassenheit_principle_be_t30, gelassenheit_separation__principle_reading, base_extractiveness, 30, 0.38).
narrative_ontology:measurement(gelassenheit_principle_be_t40, gelassenheit_separation__principle_reading, base_extractiveness, 40, 0.4).
narrative_ontology:measurement(gelassenheit_principle_be_t50, gelassenheit_separation__principle_reading, base_extractiveness, 50, 0.42).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(gelassenheit_separation__principle_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gelassenheit_separation__principle_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gelassenheit_separation__principle_reading, 0.08).
narrative_ontology:affects_constraint(gelassenheit_separation__principle_reading, gelassenheit_separation__artifact_reading).
narrative_ontology:affects_constraint(gelassenheit_separation__principle_reading, gelassenheit_separation__consequence_reading).

% DUAL FORMULATION NOTE:
% Kernel gelassenheit_separation decomposes into three constraint stories by ε-invariance: principle_reading (this file), artifact_reading, consequence_reading. Each has distinct ε, distinct victim sets, distinct adjudication criteria. They are linked via affects_constraints because districts sometimes switch readings and the readings compete for legitimacy in the broader Amish conversation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gelassenheit_separation__principle_reading, organized, 0.7).
constraint_indexing:directionality_override(gelassenheit_separation__principle_reading, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
