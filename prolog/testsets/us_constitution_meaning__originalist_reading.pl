% ============================================================================
% CONSTRAINT STORY: us_constitution_meaning__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_meaning__originalist_reading, []).

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
 *   constraint_id: us_constitution_meaning__originalist_reading
 *   human_readable: Originalist Constitutional Constraint: Meaning Fixed at Ratification
 *   domain: constitutional_law/legal_theory/political_philosophy
 *
 * SUMMARY:
 *   The originalist reading of constitutional meaning frames the U.S.
 *   Constitution as a written document whose meaning is fixed at the moment
 *   of ratification (1788 for the original document, amendment-date for
 *   subsequent amendments). Under this reading, judges are bound to interpret
 *   the Constitution according to the original public meaning of its text —
 *   what the words would have been understood to mean by informed readers at
 *   the time of enactment. Contemporary circumstances, social attitudes,
 *   technological changes, and moral evolution are irrelevant to
 *   constitutional meaning (though relevant to application). This constraint
 *   embodies a substantive institutional choice about interpretive
 *   methodology that produces asymmetric effects: it benefits conservative
 *   coalitions and counter-majoritarian doctrine advocates while suppressing
 *   the interpretive claims of groups seeking recognition for unenumerated
 *   rights not present in 1788 framing. The constraint is a tangled rope
 *   because it simultaneously coordinates judicial behavior (solving the
 *   legitimacy problem of unelected judges by binding them to historical
 *   authority) and extracts from those whose rights cannot be grounded in
 *   historical text.
 *
 * KEY AGENTS:
 *   - Unenumerated Rights Claimants: Primary victims (powerless/trapped) — cannot access constitutional protection for rights not enumerated or contemplated in 1788 (privacy, contemporary equal protection, reproductive autonomy)
 *   - Counter-Majoritarian Constraint Advocates: Primary beneficiaries (institutional/arbitrage) — use originalism to constrain democratic majorities and protect counter-majoritarian institutional roles
 *   - Living Constitutionalist Scholars and Judges: Secondary victims (moderate/constrained) — institutional actors whose interpretive framework is suppressed despite social support
 *   - Progressive Judicial Coalitions: Secondary actors (institutional/constrained) — benefit from constitutional review but suffer from originalist suppression of doctrinal development
 *   - Conservative Judicial Majority: Institutional beneficiary (institutional/arbitrage) — coordinates through originalist framing; maintains institutional dominance via appointment strategy
 *   - Originalist Judicial Orthodoxy: Institutional performance (institutional/arbitrage) — maintains legitimacy narrative increasingly separated from actual interpretive consistency
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_meaning__originalist_reading, 0.58).
domain_priors:suppression_score(us_constitution_meaning__originalist_reading, 0.72).
domain_priors:theater_ratio(us_constitution_meaning__originalist_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(us_constitution_meaning__originalist_reading, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_meaning__originalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_meaning__originalist_reading, "Originalist Constitutional Constraint: Meaning Fixed at Ratification").
narrative_ontology:topic_domain(us_constitution_meaning__originalist_reading, "constitutional_law/legal_theory/political_philosophy").

domain_priors:requires_active_enforcement(us_constitution_meaning__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_meaning__originalist_reading, '7e2f89d7-e744-41d1-a9c2-ee5c38df07ff').
narrative_ontology:cs_kernel_codification('7e2f89d7-e744-41d1-a9c2-ee5c38df07ff', fixed_text).
narrative_ontology:cs_authority_grounding('7e2f89d7-e744-41d1-a9c2-ee5c38df07ff', lineage).
narrative_ontology:cs_interpretation_layer_present('7e2f89d7-e744-41d1-a9c2-ee5c38df07ff').
narrative_ontology:cs_reading_relation('7e2f89d7-e744-41d1-a9c2-ee5c38df07ff', us_constitution_meaning__living_constitutionalist_reading, forecloses).
narrative_ontology:cs_reading_relation('7e2f89d7-e744-41d1-a9c2-ee5c38df07ff', us_constitution_meaning__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('7e2f89d7-e744-41d1-a9c2-ee5c38df07ff', foundational, meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('7e2f89d7-e744-41d1-a9c2-ee5c38df07ff', meaning_fixed_at_ratification, deontological).
narrative_ontology:cs_axiom('7e2f89d7-e744-41d1-a9c2-ee5c38df07ff', foundational, historical_public_meaning_determinacy).
narrative_ontology:cs_axiom_status(historical_public_meaning_determinacy, holdable).
narrative_ontology:cs_axiom_grounding('7e2f89d7-e744-41d1-a9c2-ee5c38df07ff', historical_public_meaning_determinacy, empirically_contingent).
narrative_ontology:cs_reference_frame('7e2f89d7-e744-41d1-a9c2-ee5c38df07ff', historical_public_meaning_framework).
narrative_ontology:cs_drift_state('7e2f89d7-e744-41d1-a9c2-ee5c38df07ff', contemporary_progressive_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('7e2f89d7-e744-41d1-a9c2-ee5c38df07ff', '').
narrative_ontology:cs_kernel_id(us_constitution_meaning__originalist_reading, us_constitution_meaning).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_meaning__originalist_reading, counter_majoritarian_constraint_advocates).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__originalist_reading, institutional_judiciary).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__originalist_reading, conservative_coalitions).
narrative_ontology:constraint_victim(us_constitution_meaning__originalist_reading, unenumerated_rights_claimants).
narrative_ontology:constraint_victim(us_constitution_meaning__originalist_reading, contemporary_rights_movements).
narrative_ontology:constraint_victim(us_constitution_meaning__originalist_reading, evolutionary_interpretation_scholars).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNENUMERATED RIGHTS CLAIMANTS (SNARE) — Groups seeking constitutional protection for rights not explicitly enumerated or not textually present in 1788 (e.g., privacy, equal protection in contemporary forms, reproductive autonomy) face a constraint that forecloses their interpretive pathway. They are trapped by the historical evidence gate: if their right was not recognized or contemplated in 1788, originalism suppresses their claim regardless of contemporary consensus or moral argument. No exit available — cannot appeal to social change or evolving values. Maximum experienced extraction.
constraint_indexing:constraint_classification(us_constitution_meaning__originalist_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LIVING CONSTITUTIONALIST SCHOLARS AND JUDGES (TANGLED ROPE) — Face constrained exit: they can advocate for evolutionary interpretation, but originalist doctrine has institutional dominance in the judiciary (especially after conservative appointment strategies). They benefit from constitutional review as an institution (they participate in it, shape doctrine through scholarship) but suffer extraction through suppression of their interpretive framework. Mixed benefit and cost — constrained mobility.
constraint_indexing:constraint_classification(us_constitution_meaning__originalist_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: COUNTER-MAJORITARIAN CONSTRAINT ADVOCATES (ROPE) — Conservative constitutional scholars, institutional defenders of judicial restraint, and federalist frameworks benefit from originalism as a coordination mechanism. It solves the legitimacy problem: 'How can unelected judges overturn democratic decisions?' Originalism coordinates this by binding judges to historical meaning, preventing creative reinterpretation. Net beneficiary — arbitrage exit (can shift among interpretive methodologies while remaining institutionally invested). Experiences the constraint primarily as coordination, not extraction.
constraint_indexing:constraint_classification(us_constitution_meaning__originalist_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PROGRESSIVE JUDICIAL COALITIONS (TANGLED ROPE) — Institutional actors who benefit from robust constitutional review (as a mechanism for protecting minorities, enforcing fundamental rights) but suffer from originalist suppression of their preferred doctrines. Constrained exit: they can advocate for living constitutionalism, but originalist institutional dominance (Supreme Court majority) limits their practical influence. Both coordinate through constitutional review AND suffer extraction through meaning-fixing constraints.
constraint_indexing:constraint_classification(us_constitution_meaning__originalist_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ORIGINALIST JUDICIAL ORTHODOXY (PITON) — From a long-term institutional perspective, originalism is increasingly performative. Judges claiming to apply historical public meaning routinely disagree about what that meaning was (textualist vs historical-context originalism; individual vs original-public meaning). The ritual of consulting founding-era evidence persists but produces inconsistent results. Theater ratio is moderate (not as theatrical as legacy Lochner jurisprudence, but significant interpretive discretion persists beneath 'original meaning' framing). Maintained through institutional inertia and legitimacy narrative rather than functional success.
constraint_indexing:constraint_classification(us_constitution_meaning__originalist_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, originalism appears as a natural law of constitutional interpretation: written documents have fixed meanings at their moment of creation; any attempt to change meaning without amendment is illegitimate. This reflects a deep principle about how language, law, and legitimacy function. However, the structural data contradicts the mountain classification — the false summit detector will identify this as naturalization of a contingent institutional choice about interpretive methodology.
constraint_indexing:constraint_classification(us_constitution_meaning__originalist_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_meaning__originalist_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_constitution_meaning__originalist_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_constitution_meaning__originalist_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_meaning__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_constitution_meaning__originalist_reading, TR),
    TR >= 0.70.

:- end_tests(us_constitution_meaning__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts from unenumerated rights claimants by permanently foreclosing their interpretive pathway unless they obtain a constitutional amendment (structurally difficult). The extraction is not total suppression because the constraint does solve a legitimate coordination problem (judges need some interpretive methodology). The intermediate value reflects the genuine benefit of originalism as a constraint on judicial discretion, balanced against the extraction cost to rights claimants. Suppression (0.72): High. The constraint suppresses non-originalist outcomes through enforcement of historical evidence requirements and institutional dominance of originalist jurisprudence in the Supreme Court. However, suppression is not total (0.05 level) because alternative interpretive methodologies are still advocated and constrained outcomes still occur. Theater ratio (0.38): Moderate-low. Originalism requires genuine engagement with historical evidence, textual analysis, and founding-era documentation — this is less theatrical than some doctrinal areas. However, originalists frequently disagree about what historical evidence means, and individual interpretive discretion persists beneath the 'original meaning' framing. The moderate theater reflects that originalism constrains but does not eliminate judicial discretion. The rising trajectory (0.28 → 0.38) reflects that as originalism has become institutionalized, the performative dimension has increased — the ritual of consulting founding-era sources has become more standardized and less contested, even as substantive disagreement persists about what those sources mean.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap lies between the beneficiary view (rope: originalism is coordination) and the victim view (snare: originalism is extraction). Counter-majoritarian advocates see a constraint that solves a legitimacy problem: unelected judges need principled limits, and originalism provides them. Unenumerated rights claimants see a mechanism that permanently forecloses their constitutional claims. Progressive judges see a constraint that benefits some institutional roles (counter-majoritarian, conservative coalitions) while suppressing others (evolutionary protection of rights). The analytical observer risks seeing originalism as a natural law (linguistic meaning is fixed at creation) when the structural data reveals it as a contingent institutional choice that benefits particular stakeholders. The false summit detector will identify this perspectival gap as evidence of constructed constraint presented as natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is computed from agent power, exit options, and beneficiary/victim status. Unenumerated rights claimants are powerless and trapped: they bear full extraction cost with no exit options. Living constitutionalists are moderate power with constrained exit: they can advocate alternative interpretations but face institutional suppression. Counter-majoritarian advocates are institutional power with arbitrage exit: they can shift among constitutional methodologies while maintaining institutional positions. Progressive coalitions are institutional power with constrained exit: they benefit from constitutional review as an institution but suffer from originalist dominance. The false summit analytical perspective uses canonical d for analytical observers (0.73), which produces a mountain classification that the FSM gate will evaluate. The beneficiaries declared trigger FSM evaluation; the omega documents the natural-law versus contingent-choice ambiguity.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that originalism is a tangled rope, not a pure coordination mechanism or pure extraction. It genuinely coordinates judicial behavior and solves a legitimacy problem (unelected judges need principled limits), which is why counter-majoritarian advocates see it as rope. But it simultaneously extracts from unenumerated rights claimants by foreclosing their interpretive pathway, which is why victims see it as snare. The constraint is extractive (suppression ≥ 0.40, extractiveness ≥ 0.30) AND coordinative (it solves a real institutional problem). The tangled_rope classification captures both dimensions: genuine coordination function paired with asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_meaning_determinacy,
    'Is historical public meaning determinate enough to constrain judicial interpretation, or does the historical record underspecify meaning in ways that permit interpretive discretion indistinguishable from living constitutionalism?',
    'Empirical analysis of originalist Supreme Court opinions: do disagreements among originalists about historical meaning correlate with disagreements among living constitutionalists? If correlations are high (>0.7), historical evidence underdetermines meaning and originalism functions as a rationalization rather than constraint.',
    'If highly indeterminate: extractiveness rises substantially (0.58 → 0.72); constraint becomes rationalization for preferred outcomes. If determinate: classification holds; originalism functions as meaningful constraint on judicial discretion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_meaning_determinacy, empirical, 'Whether historical public meaning is sufficiently determinate to constrain interpretation').

omega_variable(
    amendment_as_escape_valve,
    'If the Constitution can be amended through Article V processes, does originalism''s suppression of unenumerated rights constitute a legitimate institutional choice (channeling change through democratic amendment) or extraction (making amendment so difficult that unenumerated rights become permanently foreclosed)?',
    'Comparison of amendment success rates across eras; analysis of whether amendment difficulty has increased in ways that make Fifteenth, Nineteenth, and Twenty-Sixth Amendment analogues structurally impossible in contemporary politics',
    'If amendment remains viable democratic channel: constraint is coordination device (high suppression but with procedural escape). If amendment is functionally impossible: constraint is extraction mechanism (suppression forecloses exit, permitting permanent subordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_as_escape_valve, conceptual, 'Whether Article V amendment functions as a legitimate escape valve for originalist constraints').

omega_variable(
    originalism_versus_living_constitutionalism_normative_grounding,
    'Is the conflict between originalism and living constitutionalism resolvable as a dispute within a single normative framework (e.g., both committed to rule of law, differing on methods), or does the conflict reflect incompatible foundational commitments (e.g., one privileging historical legitimacy, the other contemporary consent)?',
    'Conceptual analysis of whether either reading can adopt the other''s axiom without contradiction. Can an originalist accept that constitutional meaning evolves with social values while remaining originalist? Can a living constitutionalist accept that meaning was fixed at ratification while remaining living constitutionalist?',
    'If resolvable within one framework: readings coexist_with or influence each other. If incompatible: readings foreclose each other; only one can be held within a single consistent jurisprudential system.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalism_versus_living_constitutionalism_normative_grounding, conceptual, 'Whether originalism and living constitutionalism are compatible normative frameworks or mutually exclusive').

omega_variable(
    false_summit_natural_law_candidacy,
    'Is originalism''s claim to be binding judges by fixed meaning a natural law (inherent to written constitutions and language), or is it a contingent institutional choice that benefits particular stakeholders (conservative coalitions, counter-majoritarian doctrine advocates)?',
    'Cross-national constitutional history: do all democracies with written constitutions adopt originalist interpretation, or is originalism one choice among viable alternatives? If alternatives are viable and competitive, originalism is not a natural law.',
    'If natural law: mountain classification should hold. If contingent institutional choice: false summit signature fires; reclassifies to tangled_rope. Beneficiaries declared; omega omega documents the ambiguity required by FSM schema gate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_law_candidacy, empirical, 'Whether originalist constraint is a natural law or contingent institutional choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_meaning__originalist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uscon_orig_tr_t0, us_constitution_meaning__originalist_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(uscon_orig_tr_t20, us_constitution_meaning__originalist_reading, theater_ratio, 20, 0.33).
narrative_ontology:measurement(uscon_orig_tr_t40, us_constitution_meaning__originalist_reading, theater_ratio, 40, 0.38).

% Extraction over time
narrative_ontology:measurement(uscon_orig_be_t0, us_constitution_meaning__originalist_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(uscon_orig_be_t20, us_constitution_meaning__originalist_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(uscon_orig_be_t40, us_constitution_meaning__originalist_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(uscon_orig_su_t0, us_constitution_meaning__originalist_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(uscon_orig_su_t20, us_constitution_meaning__originalist_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(uscon_orig_su_t40, us_constitution_meaning__originalist_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_meaning__originalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_meaning__originalist_reading, us_constitution_meaning__living_constitutionalist_reading).
narrative_ontology:affects_constraint(us_constitution_meaning__originalist_reading, us_constitution_meaning__positivist_reading).

% DUAL FORMULATION NOTE:
% The U.S. constitutional meaning kernel produces three structurally distinct constraints, one for each viable interpretive reading. Each reading has its own ε, beneficiaries, victims, and suppression profile. Originalism (this story) fixes meaning at ratification; living constitutionalism allows evolutionary application; positivism grounds authority in procedure rather than meaning. These are not observational variants of one constraint — they are genuinely different constraints arising from different readings of the same kernel. Linked via affects_constraints for contamination propagation analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_constitution_meaning__originalist_reading, institutional, 0.32).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
