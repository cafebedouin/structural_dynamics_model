% ============================================================================
% CONSTRAINT STORY: doomsday_clock_metric__hybrid_legitimacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_doomsday_clock_metric__hybrid_legitimacy_reading, []).

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
 *   constraint_id: doomsday_clock_metric__hybrid_legitimacy_reading
 *   human_readable: Doomsday Clock Metric — Hybrid Legitimacy Reading
 *   domain: science_communication/normative_epistemology/risk_governance
 *
 * SUMMARY:
 *   The Doomsday Clock, maintained by the Bulletin of the Atomic Scientists
 *   since 1947, is read here as embodying an irreducible entanglement of
 *   scientific judgment and normative stakes in the existential risk domain.
 *   This hybrid legitimacy reading holds that the clock's authority derives
 *   precisely from its refusal to separate empirical risk assessment from
 *   value-laden urgency signaling — the ambiguity is not a flaw but the
 *   source of its coordination power. The reading claims pure coordination
 *   (rope) with no clear victims, yet the accountability void creates diffuse
 *   extraction from policymakers and publics who rely on an unaccountable
 *   metric. The claimed type (rope) diverges from the authored metrics
 *   (moderate extractiveness, rising theater), which the engine will measure
 *   as seat-dependent divergence.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(doomsday_clock_metric__hybrid_legitimacy_reading, 0.38).
domain_priors:suppression_score(doomsday_clock_metric__hybrid_legitimacy_reading, 0.22).
domain_priors:theater_ratio(doomsday_clock_metric__hybrid_legitimacy_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(doomsday_clock_metric__hybrid_legitimacy_reading, rope).
narrative_ontology:human_readable(doomsday_clock_metric__hybrid_legitimacy_reading, "Doomsday Clock Metric — Hybrid Legitimacy Reading").
narrative_ontology:topic_domain(doomsday_clock_metric__hybrid_legitimacy_reading, "science_communication/normative_epistemology/risk_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(doomsday_clock_metric__hybrid_legitimacy_reading, '812929b3-3486-4a36-97ab-ad7541a2be70').
narrative_ontology:cs_kernel_codification('812929b3-3486-4a36-97ab-ad7541a2be70', distributed).
narrative_ontology:cs_authority_grounding('812929b3-3486-4a36-97ab-ad7541a2be70', expertise).
narrative_ontology:cs_interpretation_layer_present('812929b3-3486-4a36-97ab-ad7541a2be70').
narrative_ontology:cs_reading_relation('812929b3-3486-4a36-97ab-ad7541a2be70', doomsday_clock_metric__objective_index_reading, forecloses).
narrative_ontology:cs_reading_relation('812929b3-3486-4a36-97ab-ad7541a2be70', doomsday_clock_metric__performative_tool_reading, coexists_with).
narrative_ontology:cs_axiom('812929b3-3486-4a36-97ab-ad7541a2be70', foundational, irreducible_entanglement_grounds_legitimacy).
narrative_ontology:cs_axiom_status(irreducible_entanglement_grounds_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('812929b3-3486-4a36-97ab-ad7541a2be70', irreducible_entanglement_grounds_legitimacy, conventional).
narrative_ontology:cs_axiom('812929b3-3486-4a36-97ab-ad7541a2be70', secondary, deliberate_ambiguity_enables_cross_domain_coordination).
narrative_ontology:cs_axiom_status(deliberate_ambiguity_enables_cross_domain_coordination, holdable).
narrative_ontology:cs_axiom_grounding('812929b3-3486-4a36-97ab-ad7541a2be70', deliberate_ambiguity_enables_cross_domain_coordination, instrumental).
narrative_ontology:cs_reference_frame('812929b3-3486-4a36-97ab-ad7541a2be70', epistemic_normative_entanglement_practice).
narrative_ontology:cs_drift_state('812929b3-3486-4a36-97ab-ad7541a2be70', contemporary_polycrisis_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('812929b3-3486-4a36-97ab-ad7541a2be70', '').
narrative_ontology:cs_kernel_id(doomsday_clock_metric__hybrid_legitimacy_reading, doomsday_clock_metric).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__hybrid_legitimacy_reading, bulletin_atomic_scientists).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__hybrid_legitimacy_reading, existential_risk_governance_field).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(doomsday_clock_metric__hybrid_legitimacy_reading, policymakers_public_attention).
narrative_ontology:constraint_vindicates(doomsday_clock_metric__hybrid_legitimacy_reading, irreducible_entanglement_of_science_and_normativity_in_existential_risk).
narrative_ontology:constraint_vindicates(doomsday_clock_metric__hybrid_legitimacy_reading, deliberate_ambiguity_as_legitimate_coordination_mechanism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the Doomsday Clock through its Science and Security Board. Sets the clock time annually based on expert deliberation. Gains institutional legitimacy, media attention, and funding relevance from the clock's symbolic authority. Can exit by discontinuing the clock but would lose its signature public-facing asset.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, bulletin_atomic_scientists, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(doomsday_clock_metric__hybrid_legitimacy_reading, bulletin_atomic_scientists, beneficiary).

% Researchers, policy advocates, and institutions working on existential risk gain a shared focal point and media hook from the clock. The clock coordinates attention across nuclear, climate, and emerging technology domains. Field members can pursue alternative risk metrics but lose the clock's unique cross-domain recognition.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, existential_risk_governance_field, beneficiary,
    organized, biographical, mobile, global).

% Policymakers and the engaged public rely on the clock as a heuristic for existential risk severity. They bear the cost of the clock's accountability void: no formal review process, no error correction mechanism, and no way to audit the judgment-normativity entanglement. Exit means developing independent risk assessment capacity — costly and fragmented.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, policymakers_public_attention, payer,
    moderate, immediate, constrained, global).

% Alternative risk metrics (e.g., Global Catastrophic Risk Index, planetary boundaries frameworks) lack the clock's cultural penetration. They are structurally excluded from the clock's legitimacy niche — the clock's ambiguity-based authority occupies the symbolic space they might contest. Their exclusion is maintained by the clock's first-mover advantage and cultural entrenchment.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, competing_risk_assessment_bodies, excluded,
    organized, biographical, trapped, global).

% Analyze the clock as a case study in science communication, boundary work, and the politics of expertise. They do not collect rents from the clock nor bear its operational costs. Their exit is costless — they can shift analytical focus to other cases.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, critical_science_studies_scholars, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, globally recognized symbol that coordinates attention, discourse, and policy urgency across diverse existential risk domains (nuclear, climate, AI, bio) without requiring consensus on a common metric or methodology.
% TRANSFER_FUNCTION: Moves epistemic authority and public attention from distributed scientific communities to a single institutional signifier (the clock), concentrating symbolic capital in the Bulletin while diffusing accountability for the normative judgments embedded in each setting.
% ABSENT_VOICES: Affected populations in the Global South who bear disproportionate existential risk but have no representation in the clock-setting process; future generations whose interests are invoked but not structurally represented; scientists who dissent from the Board's consensus but have no formal channel for challenge.
% DISAPPEARANCE_RATIONALE: If the clock vanished, the existential risk governance field would lose its primary cross-domain coordination symbol. Media coverage would fragment across issue-specific metrics. Policymakers would lose a shared heuristic. The Bulletin would lose its central public platform. Alternative metrics would compete to fill the void, but none currently possess the clock's cultural penetration.
% FOUNDING_PROBLEM: Post-WWII scientists needed a way to communicate nuclear danger to the public and policymakers without requiring technical literacy, while maintaining scientific credibility — a symbol that could translate expert judgment into political urgency.
% FOUNDING_PROBLEM_CORROBORATION: The Bulletin attests the founding problem persists (nuclear danger remains, now joined by climate and AI). Critical scholars (e.g., Jasanoff, Wynne) and competing risk assessors attest the original problem is substantially solved — nuclear communication has matured — and the clock now serves expanded institutional interests. No independent corroboration exists that the current clock-setting process uniquely solves the founding problem versus alternative coordination mechanisms.
narrative_ontology:disappearance_verdict(doomsday_clock_metric__hybrid_legitimacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(doomsday_clock_metric__hybrid_legitimacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(doomsday_clock_metric__hybrid_legitimacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(doomsday_clock_metric__hybrid_legitimacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(doomsday_clock_metric__hybrid_legitimacy_reading, 0.38, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(doomsday_clock_metric__hybrid_legitimacy_reading_tests).
:- end_tests(doomsday_clock_metric__hybrid_legitimacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38) reflects the accountability void: the clock concentrates symbolic authority without formal review, error correction, or representational legitimacy. The Bulletin and risk governance field benefit (coordination, attention, funding); policymakers/public bear diffuse costs of an unaccountable heuristic. Suppression (0.22) is low — no active enforcement excludes alternatives, but first-mover advantage and cultural entrenchment create soft barriers. Theater ratio (0.45) is moderate-high and rising: the annual setting ceremony performs deliberative rigor while the judgment-normativity entanglement insulates the result from challenge. Accessibility collapse (0.35) is moderate — alternatives exist but lack the clock's symbolic penetration. Resistance (0.48) is moderate — critical scholars and competing metrics contest the clock's authority but have not displaced it.
 *
 * PERSPECTIVAL GAP:
 *   From the Bulletin's seat, the clock is genuine coordination (rope) — it solves the problem of translating expert judgment into public urgency. From the policymaker/public seat, the same structure operates as unaccountable authority extraction — they cannot audit the normative judgments embedded in each setting. From the excluded competitor seat, the clock is a niche-monopolizing symbol. The engine computes these divergences from the declared roles, power, and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   The Bulletin (agenda_setter/beneficiary) sits at d ≈ 0.15 — full beneficiary of the clock's authority. The existential risk field (beneficiary) sits at d ≈ 0.25 — net coordination gain with minor reputational risk. Policymakers/public (payer) sit at d ≈ 0.75 — constrained exit, bear accountability costs. Competing bodies (excluded) sit at d ≈ 0.85 — trapped by the clock's niche dominance. Critical scholars (observer) sit at d ≈ 0.5 — analytical symmetry. The engine computes per-seat χ from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (communicating nuclear danger without technical literacy) is contested: the Bulletin says it persists; critics say it is solved and the clock now serves institutional self-preservation. The clock lacks a sunset clause or formal review mechanism. Mandatrophy is unresolved — the arrangement persists beyond its verified founding function, maintained by the very ambiguity that constitutes its legitimacy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_structure_kernel_reading,
    'How does the hybrid legitimacy reading''s claim of irreducible entanglement structurally relate to the sibling readings of the doomsday_clock_metric kernel?',
    'Comparative analysis of the three readings'' beneficiary/victim structures, coordination functions, and accountability mechanisms across the kernel''s constraint family.',
    'If the hybrid reading forecloses the objective index reading, the kernel contains a logical fracture. If all three coexist, the kernel hosts a stable interpretive pluralism. If the hybrid reading influences the performative reading, the legitimacy ambiguity creates downstream pressure on strategic framings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, conceptual, 'Committer-frame structural relationships among kernel readings').

omega_variable(
    accountability_void_as_extraction,
    'Does the clock''s accountability void (no formal review, no error correction, no representational legitimacy) constitute extraction from policymakers/public, or is it the necessary cost of a coordination symbol that must remain epistemically opaque to function?',
    'Counterfactual analysis: if a formal accountability mechanism were added (e.g., external review board, methodology transparency, dissent channel), would the clock lose its coordination power? Historical comparison with other risk symbols that gained/lost authority through accountability reforms.',
    'If accountability void is extractive, the constraint is tangled_rope (coordination + asymmetric extraction). If it is necessary coordination cost, the constraint remains rope. The claimed_type (rope) vs. metrics (extractiveness 0.38) divergence is the measurement signal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accountability_void_as_extraction, conceptual, 'Whether the accountability void is feature (coordination cost) or bug (extraction)').

omega_variable(
    beneficiary_structure_ambiguity,
    'Are the Bulletin and existential risk field genuine beneficiaries (net coordination gain) or do they extract rents from the clock''s cultural monopoly (institutional relevance, funding, attention)?',
    'Compare the clock''s coordination value (measured by policy citations, media uptake, cross-domain coordination events) against the Bulletin''s resource dependence on the clock (funding, staff, institutional identity). Survey risk governance actors on whether alternative coordination mechanisms would serve them better.',
    'If net extraction, the constraint is tangled_rope or snare. If net coordination gain with diffuse costs, rope. The beneficiary declarations in this story reflect the reading''s claim; the engine computes effective extraction from structural positions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_structure_ambiguity, empirical, 'Whether declared beneficiaries capture rents or receive coordination value').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(doomsday_clock_metric__hybrid_legitimacy_reading, 1947, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dcm_hlr_tr_t1947, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 1947, 0.1).
narrative_ontology:measurement(dcm_hlr_tr_t1960, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 1960, 0.18).
narrative_ontology:measurement(dcm_hlr_tr_t1980, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 1980, 0.28).
narrative_ontology:measurement(dcm_hlr_tr_t1991, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 1991, 0.35).
narrative_ontology:measurement(dcm_hlr_tr_t2007, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 2007, 0.41).
narrative_ontology:measurement(dcm_hlr_tr_t2020, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 2020, 0.45).

% Extraction over time
narrative_ontology:measurement(dcm_hlr_be_t1947, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 1947, 0.15).
narrative_ontology:measurement(dcm_hlr_be_t1960, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 1960, 0.22).
narrative_ontology:measurement(dcm_hlr_be_t1980, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 1980, 0.28).
narrative_ontology:measurement(dcm_hlr_be_t1991, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 1991, 0.31).
narrative_ontology:measurement(dcm_hlr_be_t2007, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 2007, 0.35).
narrative_ontology:measurement(dcm_hlr_be_t2020, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 2020, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(dcm_hlr_su_t1947, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 1947, 0.05).
narrative_ontology:measurement(dcm_hlr_su_t1960, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 1960, 0.08).
narrative_ontology:measurement(dcm_hlr_su_t1980, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 1980, 0.12).
narrative_ontology:measurement(dcm_hlr_su_t1991, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 1991, 0.18).
narrative_ontology:measurement(dcm_hlr_su_t2007, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 2007, 0.2).
narrative_ontology:measurement(dcm_hlr_su_t2020, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 2020, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(doomsday_clock_metric__hybrid_legitimacy_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(doomsday_clock_metric__hybrid_legitimacy_reading, 0.08).
narrative_ontology:affects_constraint(doomsday_clock_metric__hybrid_legitimacy_reading, doomsday_clock_metric__objective_index_reading).
narrative_ontology:affects_constraint(doomsday_clock_metric__hybrid_legitimacy_reading, doomsday_clock_metric__performative_tool_reading).

% DUAL FORMULATION NOTE:
% This constraint is the hybrid_legitimacy_reading of the doomsday_clock_metric kernel. The objective_index_reading claims the clock is a measurable risk index (ε ≈ 0, mountain). The performative_tool_reading claims the clock is a strategic mobilization instrument (ε high, snare/tangled_rope). This reading claims the clock's legitimacy derives from irreducible entanglement (ε moderate, rope claimed). The three readings form a constraint family linked by affects_constraints. ε values differ structurally: the kernel label 'Doomsday Clock' conflates three distinct claims with different extraction profiles, stakeholder structures, and persistence mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
