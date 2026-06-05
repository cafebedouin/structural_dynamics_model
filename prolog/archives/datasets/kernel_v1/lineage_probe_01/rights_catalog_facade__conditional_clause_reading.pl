% ============================================================================
% CONSTRAINT STORY: rights_catalog_facade__conditional_clause_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rights_catalog_facade__conditional_clause_reading, []).

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
 *   constraint_id: rights_catalog_facade__conditional_clause_reading
 *   human_readable: Rights Catalog Facade: Conditional Clause Reading
 *   domain: legal/doctrinal/constitutional
 *
 * SUMMARY:
 *   This constraint instantiates the conditional-clause reading of the rights
 *   catalog kernel: every freedom guaranteed in the text came pre-conditioned
 *   — 'in conformity with the interests of the working people' — with the
 *   limitation built into the grant's own grammar, not added afterward. The
 *   reading interprets this conditional as a genuine structural feature of
 *   the right itself, not as external constraint layered on top of an
 *   otherwise-unconditional freedom. Under this reading, the right does not
 *   exist as an independent sphere; it exists only insofar as the holder
 *   exercises it in conformity with an externally-determined standard. The
 *   beneficiary is the entity that determines what 'conformity' means. The
 *   victims are citizens whose exercise of the right is deemed non-conforming
 *   and therefore suppressible. This reading differs fundamentally from the
 *   external-showcase reading (the catalog was propaganda for export) and the
 *   social-rights reading (the social guarantees had measurable substance).
 *   The conditional-clause reading locates the constraint's extractiveness in
 *   the internal grammar of the right itself: the right is granted in a form
 *   that makes its exercise contingent on approval by the state authority
 *   empowered to interpret 'the interests of the working people.'
 *
 * KEY AGENTS:
 *   - State Authority: Primary beneficiary (institutional/arbitrage) — owns the power to interpret 'conformity' and therefore owns the discretion to grant or deny the right in each instance
 *   - Citizens Deemed Non-Conforming: Primary victim (powerless/trapped) — granted a right in text but denied it in practice; no appeal mechanism internal to the right itself
 *   - Loyal Citizens (Identity-Locked): Secondary victim (powerless/identity_locked) — structurally mobile but identity-fused with the state's interpretation; see conformity as legitimate duty
 *   - Regime-Aligned Citizens: Moderate beneficiary/victim (moderate/constrained) — benefit when views align with state interpretation; face extraction when alignment breaks
 *   - Constitutional Text: Institutional artifact (institutional/arbitrage) — persists as formal constraint but functions performatively; cited for legitimacy without operative force
 *   - Analytical Observer: Universal perspective (analytical/analytical) — risks naturalizing the contingent choice to make conformity determination centralized and unappealable
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rights_catalog_facade__conditional_clause_reading, 0.68).
domain_priors:suppression_score(rights_catalog_facade__conditional_clause_reading, 0.75).
domain_priors:theater_ratio(rights_catalog_facade__conditional_clause_reading, 0.82).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rights_catalog_facade__conditional_clause_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(rights_catalog_facade__conditional_clause_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(rights_catalog_facade__conditional_clause_reading, theater_ratio, 0.82).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rights_catalog_facade__conditional_clause_reading, snare).
narrative_ontology:human_readable(rights_catalog_facade__conditional_clause_reading, "Rights Catalog Facade: Conditional Clause Reading").
narrative_ontology:topic_domain(rights_catalog_facade__conditional_clause_reading, "legal/doctrinal/constitutional").

domain_priors:requires_active_enforcement(rights_catalog_facade__conditional_clause_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rights_catalog_facade__conditional_clause_reading, '67a25a22-f198-459f-ac98-de0b185eb7d6').
narrative_ontology:cs_kernel_codification('67a25a22-f198-459f-ac98-de0b185eb7d6', fixed_text).
narrative_ontology:cs_authority_grounding('67a25a22-f198-459f-ac98-de0b185eb7d6', extraction).
narrative_ontology:cs_interpretation_layer_present('67a25a22-f198-459f-ac98-de0b185eb7d6').
narrative_ontology:cs_reading_relation('67a25a22-f198-459f-ac98-de0b185eb7d6', rights_catalog_facade__external_showcase_reading, coexists_with).
narrative_ontology:cs_reading_relation('67a25a22-f198-459f-ac98-de0b185eb7d6', rights_catalog_facade__social_rights_substance_reading, influences).
narrative_ontology:cs_axiom('67a25a22-f198-459f-ac98-de0b185eb7d6', foundational, conformity_clause_limits_right_internally).
narrative_ontology:cs_axiom_status(conformity_clause_limits_right_internally, holdable).
narrative_ontology:cs_axiom_grounding('67a25a22-f198-459f-ac98-de0b185eb7d6', conformity_clause_limits_right_internally, deontological).
narrative_ontology:cs_axiom('67a25a22-f198-459f-ac98-de0b185eb7d6', foundational, state_authority_interprets_interests).
narrative_ontology:cs_axiom_status(state_authority_interprets_interests, holdable).
narrative_ontology:cs_axiom_grounding('67a25a22-f198-459f-ac98-de0b185eb7d6', state_authority_interprets_interests, conventional).
narrative_ontology:cs_reference_frame('67a25a22-f198-459f-ac98-de0b185eb7d6', sovereign_state_authority_over_rights_interpretation).
narrative_ontology:cs_drift_state('67a25a22-f198-459f-ac98-de0b185eb7d6', post_regime_collapse_or_reform, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('67a25a22-f198-459f-ac98-de0b185eb7d6', '').
narrative_ontology:cs_kernel_id(rights_catalog_facade__conditional_clause_reading, rights_catalog_facade).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rights_catalog_facade__conditional_clause_reading, state_authority).
narrative_ontology:constraint_victim(rights_catalog_facade__conditional_clause_reading, citizens_deemed_non_conforming).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE NON-CONFORMING CITIZEN (SNARE) — Granted a right in text but denied it in practice whenever their exercise conflicts with 'the interests of the working people' as interpreted by state authority. No appeal mechanism internal to the right itself; exit via flight is material but identity-shattering. Maximum extraction: the right exists theatrically but not functionally.
constraint_indexing:constraint_classification(rights_catalog_facade__conditional_clause_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE LOYAL CITIZEN / IDENTITY-LOCKED (SNARE) — Internalized the doctrine that the conditional phrasing is legitimate: 'my freedoms should conform to the working people's interests.' Their identity is constituted through obedience to the state's interpretation of these interests. Structurally mobile (could exit or resist) but identity-fused with the regime's framing. High extraction masked as duty.
constraint_indexing:constraint_classification(rights_catalog_facade__conditional_clause_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 3: THE STATE AUTHORITY / CONSTITUTIONAL INTERPRETER (ROPE) — Experiences the conditional clause as a coordination mechanism: it establishes norms (conformity to working people's interests) that coordinate mass behavior without requiring explicit prohibition. The beneficiary sees the constraint as enabling rather than extractive. The state has maximal exit optionality — it defines what constitutes 'conforming.' Net beneficiary.
constraint_indexing:constraint_classification(rights_catalog_facade__conditional_clause_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: THE REGIME-ALIGNED CITIZEN (TANGLED ROPE) — Benefits from the rights when their views align with the state's interpretation of 'working people's interests.' Genuine coordination function when alignment holds (information about acceptable speech, assembly norms). Extraction when alignment breaks. Constrained exit: leaving means abandoning social position, but theoretically possible at career/social cost.
constraint_indexing:constraint_classification(rights_catalog_facade__conditional_clause_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: THE CONSTITUTIONAL TEXT / FORMAL READING (PITON) — As a legal artifact, the document persists; the conditional clause continues to be cited as evidence of legal constraint. In practice, the clause is almost entirely performative — cited for legitimacy, enforced inconsistently, reinterpreted constantly. The written constraint functions as cover for discretionary authority rather than as an operative legal limit. Theater ratio is high because the formal clause substitutes for actual limitation.
constraint_indexing:constraint_classification(rights_catalog_facade__conditional_clause_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: THE ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, any rights catalog must employ some limiting language to establish the scope of legitimate exercise. The phrase 'in conformity with...' is an irreducible feature of how rights are defined — it is inherent to the logical structure of bounded freedoms. However, this perspective risks naturalizing what is actually a contingent choice in how 'conformity' is determined and enforced. The engine will detect this as a false summit.
constraint_indexing:constraint_classification(rights_catalog_facade__conditional_clause_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rights_catalog_facade__conditional_clause_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rights_catalog_facade__conditional_clause_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rights_catalog_facade__conditional_clause_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(rights_catalog_facade__conditional_clause_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(rights_catalog_facade__conditional_clause_reading, TR),
    TR >= 0.70.

:- end_tests(rights_catalog_facade__conditional_clause_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The state authority retains the power to determine what constitutes 'conformity' and can therefore suppress any exercise of the right deemed non-conforming. The extraction is substantial because the right appears to be granted in the text, but its exercise is controlled entirely by the external interpretation. Suppression (0.75): High. The suppression is internal to the right's definition — the right is defined as existing only in conformity with an external standard. Citizens have no mechanism to know in advance whether a proposed exercise will be deemed conforming, and enforcement is retrospective (determination made after the act). Theater ratio (0.82): High. The conditional clause creates an appearance of legal constraint while preserving unlimited discretion for the interpreter. The text of the right (the formal grant) functions theatrically: it provides legitimacy cover for discretionary authority. The actual limiting force comes from unstated norms and the interpreter's preferences, not from the written clause. Rising theater_ratio over the interval indicates increasing reliance on the theatrical function (more consistent invocation of the clause without meaningful limitation).
 *
 * PERSPECTIVAL GAP:
 *   The state authority sees the conditional clause as a coordination mechanism — it establishes norms for acceptable speech and assembly without requiring explicit prohibition for every case. The non-conforming citizen sees pure extraction: a right granted in text but withheld in practice, with no appeal. The identity-locked citizen internalizes the conformity standard and sees it as legitimate. The regime-aligned citizen experiences a mixed constraint: genuine coordination when their views align, extraction when they diverge. The constitutional text appears as a formal limit (piton perspective) — cited for legitimacy but not operatively constraining. The analytical observer risks seeing conditional rights as a natural feature of bounded freedoms (mountain perspective), missing the contingent choice to centralize the conformity determination. The perspectival spread reveals the gap between the formal right and its actual operation: the same grammatical structure produces snare for the powerless, rope for the beneficiary, and mountain for the analytical observer.
 *
 * DIRECTIONALITY LOGIC:
 *   The state authority as beneficiary with arbitrage exit derives d ≈ 0.10 (low): the state benefits from the conditional clause by retaining discretionary power to suppress non-conforming speech/assembly without having to forbid them explicitly. The authority can interpret the clause differently in each case and cannot be bound by prior determinations. This is maximal arbitrage: the state can exit any constraint the clause might seem to impose by simply declaring the citizen's exercise non-conforming. Citizens deemed non-conforming as victims with trapped exit derive d ≈ 0.95 (high): they cannot exit the jurisdiction without abandoning identity and property; they cannot exit the constraint without complying with an unknown and changing standard. They bear full extraction cost. Identity-locked citizens as victims with identity-locked exit derive d ≈ 0.89: they could structurally exit (leave, resist, seek clarification) but their identity is fused with loyalty to the regime and its interpretation of 'working people's interests.' The identity lock sustains the extraction through internal framing rather than external barrier.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conformity_determination_authority,
    'Who determines what ''conformity with the interests of the working people'' means, and is that determination subject to appeal or revision?',
    'Historical analysis of explicit determinations made under this clause; documentation of mechanisms for challenging interpretations; comparison with stated legislative intent (if available)',
    'If determination is centralized and unappealable: extraction is high and suppression is structural (snare confirmed). If determination is distributed or revisable: some exit optionality emerges (reclassify toward tangled_rope). If determination is visible and predictable: theater_ratio decreases (reclassify toward rope or scaffold).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(conformity_determination_authority, empirical, 'Authority structure and appealability of conformity determinations').

omega_variable(
    conformity_prior_vs_posterior,
    'Is ''conformity with working people''s interests'' determined prospectively (before speech/assembly act, providing notice) or retrospectively (after the act, as a basis for punishment)?',
    'Chronological analysis of enforcement actions: when were determinations made relative to the speech/assembly act? Were citizens given notice before the act?',
    'If prospective: extraction is lower (more coordinating, less suppressing — rope territory). If retrospective: extraction is higher (suppression is hidden until enforcement — snare confirmed). If mixed: cyclical theater_ratio pattern in measurements (sometimes coordinating, sometimes extracting).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(conformity_prior_vs_posterior, empirical, 'Prospective vs retrospective determination of conformity').

omega_variable(
    reading_foreclosure_claim,
    'Does this conditional-clause reading logically foreclose the external-showcase reading? Can the regime simultaneously maintain that the clause is genuinely limiting (conditional-clause reading) AND that the catalog is primarily an export propaganda document (external-showcase reading)?',
    'Documentary analysis of regime statements and actions: internal security documents and policy memos vs international public relations messaging. If both narratives appear in the same regime, the readings coexist rather than foreclose.',
    'If they foreclose: reading_relations should declare ''forecloses'' toward external-showcase reading. If they coexist: reading_relations should declare ''coexists_with'' (the regime holds both contradictory frames simultaneously, using them in different contexts).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_claim, conceptual, 'Whether conditional-clause reading forecloses external-showcase reading').

omega_variable(
    social_rights_substance_independence,
    'Are the measurable social rights (employment, housing, education) genuinely independent from the conditional clause''s suppression logic, or do they depend on conformity determinations?',
    'Historical analysis of social rights delivery: were benefits withheld from non-conforming citizens? Did regime use conditional clause to deny housing, employment, or education to disfavored groups?',
    'If independent: social-rights reading is a genuinely distinct reading with different ε (lower suppression, more genuine benefit). If dependent: the conditional clause extends its suppression into the social rights domain (ε increases, suppression rises). If partially dependent: reading influences rather than forecloses social-rights reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(social_rights_substance_independence, empirical, 'Independence of social rights from conditional-clause suppression').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rights_catalog_facade__conditional_clause_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(righ_tr_t0, rights_catalog_facade__conditional_clause_reading, theater_ratio, 0, 0.65).
narrative_ontology:measurement(righ_tr_t3, rights_catalog_facade__conditional_clause_reading, theater_ratio, 3, 0.75).
narrative_ontology:measurement(righ_tr_t6, rights_catalog_facade__conditional_clause_reading, theater_ratio, 6, 0.82).

% Extraction over time
narrative_ontology:measurement(righ_be_t0, rights_catalog_facade__conditional_clause_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(righ_be_t3, rights_catalog_facade__conditional_clause_reading, base_extractiveness, 3, 0.62).
narrative_ontology:measurement(righ_be_t6, rights_catalog_facade__conditional_clause_reading, base_extractiveness, 6, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(righ_su_t0, rights_catalog_facade__conditional_clause_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(righ_su_t3, rights_catalog_facade__conditional_clause_reading, suppression_requirement, 3, 0.72).
narrative_ontology:measurement(righ_su_t6, rights_catalog_facade__conditional_clause_reading, suppression_requirement, 6, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rights_catalog_facade__conditional_clause_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(rights_catalog_facade__conditional_clause_reading, 0.12).
narrative_ontology:affects_constraint(rights_catalog_facade__conditional_clause_reading, rights_catalog_facade__external_showcase_reading).
narrative_ontology:affects_constraint(rights_catalog_facade__conditional_clause_reading, rights_catalog_facade__social_rights_substance_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the rights_catalog_facade kernel. The three constraint stories (conditional-clause, external-showcase, social-rights) are sibling readings of the same contested text. They share a kernel (the constitution's rights guarantees) but interpret that kernel via different readings of what the grammar constrains and what the regime intended. Each reading has its own ε value, suppression mechanism, and beneficiary/victim structure. They coexist or influence each other — no single reading forecloses the others within the regime's actual practice (the regime uses all three framings in different contexts). Link via network.affects_constraints to enable contamination analysis across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
