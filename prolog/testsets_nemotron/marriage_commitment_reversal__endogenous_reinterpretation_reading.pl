% ============================================================================
% CONSTRAINT STORY: marriage_commitment_reversal__endogenous_reinterpretation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_reversal__endogenous_reinterpretation_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: marriage_commitment_reversal__endogenous_reinterpretation_reading
 *   human_readable: Plural Marriage Suspension by Prophetic Revelation (1890 Manifesto)
 *   domain: religious_institutional/political_theology
 *
 * SUMMARY:
 *   The 1890 Manifesto (Official Declaration 1) issued by Wilford Woodruff,
 *   President of the Church of Jesus Christ of Latter-day Saints, publicly
 *   suspended the practice of plural marriage following his reported vision
 *   of September 23, 1890. The constraint is the institutional arrangement
 *   that interprets this suspension as a divinely authorized reinterpretation
 *   of God's will — not a concession to federal coercion. The kernel is the
 *   marriage commitment reversal itself; this reading instantiates the
 *   constraint where prophetic revelation legitimates the reversal,
 *   preserving institutional authority while creating a victim class of
 *   theological consistency (why did God's eternal principle change?) and
 *   faithful practitioners who bore the costs of compliance. The
 *   exogenous_override_reading and practice_doctrine_gap are sibling
 *   constraints from the same kernel, not alternative framings within this
 *   story.
 *
 * KEY AGENTS:
 *   - church_presidency: Primary agenda_setter (prophet/president defines revelation) — maintains interpretive authority
 *   - prophet_woodruff: Primary beneficiary (revelation recipient) — legitimacy preserved via divine authorization
 *   - faithful_practitioners: Primary payer (bore social/legal costs of plural marriage, then costs of abandonment) — identity-locked exit
 *   - theological_consistency: Abstract victim (eternal principle apparently reversed) — no exit
 *   - dissenting_apostles: Secondary payer (some resisted, faced discipline) — constrained exit
 *   - federal_authorities: External coercer (not a stakeholder in this reading's structure; their pressure is the exogenous reading's causal claim)
 *   - scholarly_observers: Analytical seat — sees full structural ambiguity across readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.58).
domain_priors:suppression_score(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.32).
domain_priors:theater_ratio(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_reversal__endogenous_reinterpretation_reading, tangled_rope).
narrative_ontology:human_readable(marriage_commitment_reversal__endogenous_reinterpretation_reading, "Plural Marriage Suspension by Prophetic Revelation (1890 Manifesto)").
narrative_ontology:topic_domain(marriage_commitment_reversal__endogenous_reinterpretation_reading, "religious_institutional/political_theology").

domain_priors:requires_active_enforcement(marriage_commitment_reversal__endogenous_reinterpretation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_reversal__endogenous_reinterpretation_reading, 'ed2fd5f3-eda9-4e5b-bf9a-24413e9e931d').
narrative_ontology:cs_kernel_codification('ed2fd5f3-eda9-4e5b-bf9a-24413e9e931d', formalized).
narrative_ontology:cs_authority_grounding('ed2fd5f3-eda9-4e5b-bf9a-24413e9e931d', lineage).
narrative_ontology:cs_interpretation_layer_present('ed2fd5f3-eda9-4e5b-bf9a-24413e9e931d').
narrative_ontology:cs_reading_relation('ed2fd5f3-eda9-4e5b-bf9a-24413e9e931d', marriage_commitment_reversal__exogenous_override_reading, influences).
narrative_ontology:cs_reading_relation('ed2fd5f3-eda9-4e5b-bf9a-24413e9e931d', marriage_commitment_reversal__practice_doctrine_gap, coexists_with).
narrative_ontology:cs_axiom('ed2fd5f3-eda9-4e5b-bf9a-24413e9e931d', foundational, continuing_revelation_modifies_eternal_principles).
narrative_ontology:cs_axiom_status(continuing_revelation_modifies_eternal_principles, holdable).
narrative_ontology:cs_axiom_grounding('ed2fd5f3-eda9-4e5b-bf9a-24413e9e931d', continuing_revelation_modifies_eternal_principles, deontological).
narrative_ontology:cs_axiom('ed2fd5f3-eda9-4e5b-bf9a-24413e9e931d', foundational, prophetic_authority_is_epistemically_privileged).
narrative_ontology:cs_axiom_status(prophetic_authority_is_epistemically_privileged, holdable).
narrative_ontology:cs_axiom_grounding('ed2fd5f3-eda9-4e5b-bf9a-24413e9e931d', prophetic_authority_is_epistemically_privileged, deontological).
narrative_ontology:cs_reference_frame('ed2fd5f3-eda9-4e5b-bf9a-24413e9e931d', section_132_eternal_plural_marriage).
narrative_ontology:cs_drift_state('ed2fd5f3-eda9-4e5b-bf9a-24413e9e931d', post_manifesto_institutionalization, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ed2fd5f3-eda9-4e5b-bf9a-24413e9e931d', '2026-08-03T14:22:17Z').
narrative_ontology:cs_kernel_id(marriage_commitment_reversal__endogenous_reinterpretation_reading, marriage_commitment_reversal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__endogenous_reinterpretation_reading, church_presidency).
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__endogenous_reinterpretation_reading, prophet_woodruff).
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__endogenous_reinterpretation_reading, institutional_legitimacy).
narrative_ontology:constraint_victim(marriage_commitment_reversal__endogenous_reinterpretation_reading, theological_consistency).
narrative_ontology:constraint_victim(marriage_commitment_reversal__endogenous_reinterpretation_reading, faithful_practitioners).
narrative_ontology:constraint_victim(marriage_commitment_reversal__endogenous_reinterpretation_reading, dissenting_apostles).
narrative_ontology:constraint_vindicates(marriage_commitment_reversal__endogenous_reinterpretation_reading, continuing_revelation_doctrine).
narrative_ontology:constraint_vindicates(marriage_commitment_reversal__endogenous_reinterpretation_reading, prophetic_authority_doctrine).
narrative_ontology:constraint_vindicates(marriage_commitment_reversal__endogenous_reinterpretation_reading, institutional_survival_imperative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The prophet-president receives revelation, defines its meaning, and enforces compliance. Collects institutional survival, statehood, and continued interpretive authority. Can reframe any doctrinal tension via continuing revelation. Exit is arbitrage-grade: the institution itself is the constraint's author.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, church_presidency, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_reversal__endogenous_reinterpretation_reading, church_presidency, beneficiary).

% Received the September 23 vision; his prophetic legitimacy depends on the vision's authenticity. If the vision was strategic rather than divine, his authority collapses. Identity-locked: the prophetic office fuses personal identity with institutional authority — exit means renunciation of vocation and self.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, prophet_woodruff, beneficiary,
    powerful, biographical, identity_locked, global).

% Entered plural marriage at prophetic direction, bore legal persecution, social ostracism, and economic ruin. Then bore the cost of abandonment: dissolving families, surrendering theological identity, submitting to a reversal they could not verify. Identity-locked exit: religious self-concept constituted through obedience to prophetic authority; leaving means losing the framework that gave suffering meaning.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, faithful_practitioners, payer,
    powerless, biographical, identity_locked, global).

% The doctrinal principle that plural marriage is an eternal, unchangeable requirement for exaltation (Section 132). The reversal via revelation creates a logical contradiction: God's eternal will apparently changed. No exit — the principle either holds or it does not; the constraint's operation makes the contradiction structural.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, theological_consistency, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(marriage_commitment_reversal__endogenous_reinterpretation_reading, theological_consistency).

% Several apostles (e.g., Matthias Cowley, John W. Taylor) continued performing post-Manifesto plural marriages, resisted the suspension, and were eventually disciplined (removed from quorum, excommunicated). Constrained exit: they could resist within the institution (costly) or leave via schism (founding fundamentalist groups), but both paths carry extreme identity and community costs.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, dissenting_apostles, payer,
    organized, biographical, constrained, global).

% Historians, theologians, and scholars of religion who analyze the Manifesto's causation, the revelation claim's authenticity, and the structural dynamics across all three readings. They do not bear the constraint's costs or collect its benefits; their exit is analytical (they can change frameworks without personal cost).
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, scholarly_observers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves an existential institutional crisis: federal disincorporation, asset seizure, and leadership imprisonment threatened the church's corporate existence. The revelation narrative coordinates a unified, authoritative suspension that the federal government could accept as genuine, enabling statehood and institutional survival.
% TRANSFER_FUNCTION: Moves the costs of plural marriage (legal persecution, social stigma, economic burden) from the institution onto the faithful practitioners who must abandon the practice, while moving institutional survival, political legitimacy, and prophetic authority to the presidency. The revelation narrative transfers the causal burden from 'we surrendered to coercion' to 'God changed His mind.'
% ABSENT_VOICES: The women in plural marriages — their consent, experience, and fate in the abandonment are largely absent from the official narrative. Post-Manifesto children of plural marriages (born into a practice now suspended). Federal officials who saw the Manifesto as tactical rather than genuine. Fundamentalist schismatics who rejected the reversal entirely. These voices would object to the revelation framing but were structurally excluded from the authoritative interpretation.
% DISAPPEARANCE_RATIONALE: If the revelation-based suspension vanished overnight, the church would face immediate existential crisis: either resume plural marriage (triggering federal destruction) or admit the revelation was not divine (collapsing prophetic authority). The institutional order, doctrinal framework, and identity of millions would reorganize fundamentally.
% FOUNDING_PROBLEM: Federal extermination threat: the Edmunds-Tucker Act (1887) disincorporated the church, seized its assets, and threatened imprisonment of its entire leadership. The institution faced corporate death unless plural marriage — the flashpoint of federal hostility — was credibly suspended.
% FOUNDING_PROBLEM_CORROBORATION: Federal court records, congressional testimony, and the 1896 Utah statehood enabling act corroborate that the existential threat was real and was resolved by the Manifesto. The church's own 1890-1910 leadership discourse attests the threat was the occasion for the revelation. However, the founding problem is dead (federal threat resolved) while the constraint (revelation-based suspension) persists and has been generalized as a permanent doctrinal feature — the mandate has outlived its function.
narrative_ontology:disappearance_verdict(marriage_commitment_reversal__endogenous_reinterpretation_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_reversal__endogenous_reinterpretation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_reversal__endogenous_reinterpretation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(marriage_commitment_reversal__endogenous_reinterpretation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.58, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_reversal__endogenous_reinterpretation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_reversal__endogenous_reinterpretation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_reversal__endogenous_reinterpretation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Moderate extractiveness (0.58 at interval end) reflects the hybrid structure: genuine coordination function (institutional survival, federal conflict resolution, statehood pathway) combined with asymmetric extraction (leadership legitimacy preserved at cost of theological coherence and practitioner welfare). Suppression starts high (0.65) during active federal prosecution and post-Manifesto enforcement against dissenters, then decays as the new regime normalizes (0.32 by 1910). Theater ratio rises from 0.25 to 0.41 as the revelation narrative increasingly performs legitimacy maintenance while the coordination function (survival) is achieved. Accessibility collapse is moderate (0.48) — alternatives existed (exile, resistance, schism) but were structurally costly. Resistance is significant (0.54) — post-Manifesto plural marriages continued, fundamentalist schisms emerged, internal dissent persisted.
 *
 * PERSPECTIVAL GAP:
 *   From the presidency/prophet seat: the constraint is a rope (divine coordination solving an existential threat). From the faithful practitioner seat: the constraint is a snare (identity-locked extraction — they paid the costs of a practice leadership then reversed via revelation they cannot verify). From the theological consistency seat: the constraint is a mountain that turned into a piton (eternal principle became performative). The engine computes these per-seat divergences from the structural data; this commentary only documents the analytical expectation.
 *
 * DIRECTIONALITY LOGIC:
 *   The church presidency and prophet are structural beneficiaries (d near 0.0-0.2): they collect institutional survival and interpretive authority. Faithful practitioners are targets (d near 0.7-0.8): they bore the costs of both the original practice and its abandonment, with identity-locked exit (religious identity fused to obedience). Theological consistency is an abstract victim with no exit (d = 1.0). Dissenting apostles are constrained targets (d ~0.6): some exit via discipline or schism, but institutional identity binds most. Federal authorities are not stakeholders in THIS reading — their coercion is the exogenous reading's causal variable.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (federal extermination threat to the institution) was live in 1890. By 1910 the threat had substantially receded (statehood achieved 1896, Reed Smoot hearings 1904-1907 resolved political status), yet the revelation-based suspension persists as permanent doctrine. The constraint has not sunset — it became the new steady state. This is mandatrophy: the coordination function (survival) was achieved, but the extraction structure (prophetic authority to reinterpret eternal principles) was retained and generalized. The revelation narrative obscures the doctrine-practice gap, preventing the mandate from being recognized as resolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint one reading of the contested kernel ''marriage_commitment_reversal'', or does it represent the kernel itself?',
    'Compare structural outputs across the three declared readings (endogenous_reinterpretation, exogenous_override, practice_doctrine_gap). If ε values and victim/beneficiary sets diverge significantly, the kernel has genuinely instantiated multiple constraints.',
    'If the kernel has genuinely instantiated multiple constraints, each reading must be authored as a separate JSON file with its own ε and classification, linked via network.affects_constraints. This file instantiates only the endogenous_reinterpretation_reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment-system frame: this constraint is one reading of the marriage_commitment_reversal kernel; the kernel is the stabilized commitment to plural marriage as eternal principle (Section 132). The endogenous reading reinterprets God''s will via Woodruff''s vision rather than conceding to federal coercion.').

omega_variable(
    revelation_authenticity,
    'Was Woodruff''s September 23, 1890 vision a genuine divine revelation, a strategic reinterpretation, or a constructed narrative?',
    'No empirical resolution possible; contested across all three readings. The exogenous reading treats it as cover; the practice_doctrine_gap reading treats it as ambiguous; this reading treats it as authentic. Omega records the irreducible ambiguity.',
    'If genuine: constraint is a mountain (divine will cannot be extractive). If strategic: constraint is a snare (revelation as extraction cover). If ambiguous: tangled_rope (coordination + extraction hybrid). Current authoring: tangled_rope with moderate extractiveness.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(revelation_authenticity, preference, 'Theological authenticity of the revelatory claim — the core factual dispute across the kernel''s readings').

omega_variable(
    doctrine_practice_gap,
    'Does the Manifesto suspend practice while preserving doctrine (Section 132), or does it revoke the doctrine itself?',
    'Examine post-1890 doctrinal discourse: is Section 132 still taught as binding principle, or has it been formally rescinded? The practice_doctrine_gap reading asserts suspension-only; this reading claims doctrinal reinterpretation.',
    'If doctrine preserved: constraint is a scaffold (temporary suspension with doctrinal continuity). If doctrine reinterpreted: tangled_rope (coordination + extraction). If doctrine revoked: rope (pure coordination under new revelation). Current authoring: tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_practice_gap, empirical, 'Whether the constraint preserves, reinterprets, or revokes the founding doctrinal commitment').

omega_variable(
    coercion_vs_revelation_causality,
    'Was the 1890 reversal causally driven by federal suppression (Edmunds-Tucker, disincorporation threat) or by genuine revelatory experience?',
    'Historical analysis of temporal sequence: federal pressure peaks 1887-1890; Woodruff''s vision September 23, 1890; Manifesto issued October 6, 1890. The exogenous reading asserts coercion caused the reversal and revelation was post-hoc; this reading asserts revelation caused the reversal and coincidence with federal pressure is providential.',
    'If coercion-driven: extractiveness higher, suppression higher, constraint tends toward snare. If revelation-driven: extractiveness lower, suppression lower, constraint tends toward rope or mountain. Current authoring: moderate extraction (0.58) reflects ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_vs_revelation_causality, empirical, 'Causal attribution of the reversal — the central historical-theological dispute').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_reversal__endogenous_reinterpretation_reading, 1890, 1910).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marriage_reversal_endo_tr_t1890, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 1890, 0.25).
narrative_ontology:measurement(marriage_reversal_endo_tr_t1895, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 1895, 0.33).
narrative_ontology:measurement(marriage_reversal_endo_tr_t1900, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 1900, 0.38).
narrative_ontology:measurement(marriage_reversal_endo_tr_t1904, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 1904, 0.41).
narrative_ontology:measurement(marriage_reversal_endo_tr_t1910, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 1910, 0.41).

% Extraction over time
narrative_ontology:measurement(marriage_reversal_endo_be_t1890, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 1890, 0.45).
narrative_ontology:measurement(marriage_reversal_endo_be_t1895, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 1895, 0.52).
narrative_ontology:measurement(marriage_reversal_endo_be_t1900, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 1900, 0.56).
narrative_ontology:measurement(marriage_reversal_endo_be_t1904, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 1904, 0.58).
narrative_ontology:measurement(marriage_reversal_endo_be_t1910, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 1910, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(marriage_reversal_endo_su_t1890, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 1890, 0.65).
narrative_ontology:measurement(marriage_reversal_endo_su_t1895, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 1895, 0.48).
narrative_ontology:measurement(marriage_reversal_endo_su_t1900, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 1900, 0.35).
narrative_ontology:measurement(marriage_reversal_endo_su_t1904, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 1904, 0.32).
narrative_ontology:measurement(marriage_reversal_endo_su_t1910, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 1910, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_reversal__endogenous_reinterpretation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.08).
narrative_ontology:affects_constraint(marriage_commitment_reversal__endogenous_reinterpretation_reading, marriage_commitment_reversal__exogenous_override_reading).
narrative_ontology:affects_constraint(marriage_commitment_reversal__endogenous_reinterpretation_reading, marriage_commitment_reversal__practice_doctrine_gap).
narrative_ontology:affects_constraint(marriage_commitment_reversal__endogenous_reinterpretation_reading, post_manifesto_polygamy_enforcement).
narrative_ontology:affects_constraint(marriage_commitment_reversal__endogenous_reinterpretation_reading, reed_smoots_seat_contest).

% DUAL FORMULATION NOTE:
% The marriage_commitment_reversal kernel decomposes into three constraint stories: (1) this endogenous_reinterpretation_reading — revelation as authentic cause, moderate extraction, tangled_rope; (2) exogenous_override_reading — coercion as cause, higher extraction, snare; (3) practice_doctrine_gap — structural ambiguity, scaffold-like suspension. They share the same referent (the 1890 Manifesto and its doctrinal aftermath) but instantiate different ε, different victim/beneficiary structures, and different classifications. The endogenous reading influences the exogenous reading (revelation narrative constrains the coercion narrative's plausibility) and coexists with the practice_doctrine_gap (both can be held by different parties simultaneously).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_commitment_reversal__endogenous_reinterpretation_reading, institutional, 0.15).
constraint_indexing:directionality_override(marriage_commitment_reversal__endogenous_reinterpretation_reading, powerless, 0.85).
constraint_indexing:directionality_override(marriage_commitment_reversal__endogenous_reinterpretation_reading, organized, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
