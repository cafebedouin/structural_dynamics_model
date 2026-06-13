% ============================================================================
% CONSTRAINT STORY: ost_article_ii_non_appropriation__extraction_permissive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ost_article_ii_non_appropriation__extraction_permissive, []).

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
 *   constraint_id: ost_article_ii_non_appropriation__extraction_permissive
 *   human_readable: Article II Non-Appropriation: Extraction-Permissive Reading
 *   domain: international_law/space_governance/commons
 *
 * SUMMARY:
 *   The Outer Space Treaty (1967) declares celestial bodies the 'province of
 *   all mankind' and bars 'appropriation.' Yet Article II's language—barring
 *   sovereignty claims but silent on resource extraction—permits a reading
 *   that separates sovereign appropriation (forbidden) from extractive
 *   appropriation (permitted). Under the extraction-permissive reading, a
 *   non-spacefaring state cannot claim the Moon as sovereign territory, but a
 *   spacefaring state CAN authorize its private operators to extract, own,
 *   and sell lunar resources. This reading has become de facto operational:
 *   the USA, China, Russia, and Luxembourg have licensed or authorized
 *   extraction operations. The constraint is claimed as tangled-rope (genuine
 *   coordination problem: how to permit space development while preventing
 *   wasteful open-access races) while the authored metrics describe
 *   substantially extractive operation with active enforcement needed to
 *   exclude non-spacefaring states from resource access. The measurement
 *   series captures the constraint's evolution from 1967 (initial ambiguity,
 *   minimal enforcement) through 2025 (extraction operationalized,
 *   suppression of conservation alternatives intensifying) to 2050
 *   (projections assume extraction-permissive reading remains unchallenged).
 *
 * KEY AGENTS:
 *   - Technologically advanced spacefaring states (USA, China, Russia, ESA members, Japan): agenda-setters, possess enforcement power via flag-state authority over national operators and spacecraft registration.
 *   - Private extraction corporations (Axiom Space, Planetary Resources, China National Space Administration-licensed operators, Luxembourg-chartered enterprises): beneficiaries, operate under flag-state protection and appropriate resources as private property.
 *   - Non-spacefaring states (majority of UN membership, particularly Global South nations): payers and victims, excluded from direct resource access, cannot block licensed extraction, bound by the treaty they signed in commons-governance faith.
 *   - Global South populations and indigenous groups: structurally powerless, represented only abstractly in 'common heritage' language, have no seat in enforcement apparatus, trapped in the constraint indefinitely.
 *   - Conservation and common-heritage advocates (NGOs, certain UN delegations, scholars): excluded from enforcement, their alternative reading (commons-conservation) lacks binding power, constrained exit via non-binding advocacy.
 *   - International law observers and treaty scholars: analytical seats, document the contest and its effects but hold no enforcement or veto power.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ost_article_ii_non_appropriation__extraction_permissive, 0.78).
domain_priors:suppression_score(ost_article_ii_non_appropriation__extraction_permissive, 0.71).
domain_priors:theater_ratio(ost_article_ii_non_appropriation__extraction_permissive, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, extractiveness, 0.78).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ost_article_ii_non_appropriation__extraction_permissive, tangled_rope).
narrative_ontology:human_readable(ost_article_ii_non_appropriation__extraction_permissive, "Article II Non-Appropriation: Extraction-Permissive Reading").
narrative_ontology:topic_domain(ost_article_ii_non_appropriation__extraction_permissive, "international_law/space_governance/commons").

domain_priors:requires_active_enforcement(ost_article_ii_non_appropriation__extraction_permissive).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ost_article_ii_non_appropriation__extraction_permissive, '21289d61-a0d6-49c9-a895-1983e114648b').
narrative_ontology:cs_kernel_codification('21289d61-a0d6-49c9-a895-1983e114648b', formalized).
narrative_ontology:cs_authority_grounding('21289d61-a0d6-49c9-a895-1983e114648b', extraction).
narrative_ontology:cs_interpretation_layer_present('21289d61-a0d6-49c9-a895-1983e114648b').
narrative_ontology:cs_reading_relation('21289d61-a0d6-49c9-a895-1983e114648b', ost_article_ii_non_appropriation__commons_conservation, coexists_with).
narrative_ontology:cs_reading_relation('21289d61-a0d6-49c9-a895-1983e114648b', ost_article_ii_non_appropriation__international_regime, influences).
narrative_ontology:cs_axiom('21289d61-a0d6-49c9-a895-1983e114648b', foundational, private_property_trumps_common_heritage).
narrative_ontology:cs_axiom_status(private_property_trumps_common_heritage, holdable).
narrative_ontology:cs_axiom_grounding('21289d61-a0d6-49c9-a895-1983e114648b', private_property_trumps_common_heritage, conventional).
narrative_ontology:cs_axiom('21289d61-a0d6-49c9-a895-1983e114648b', secondary, flag_state_licensing_is_valid_appropriation_bar).
narrative_ontology:cs_axiom_status(flag_state_licensing_is_valid_appropriation_bar, holdable).
narrative_ontology:cs_axiom_grounding('21289d61-a0d6-49c9-a895-1983e114648b', flag_state_licensing_is_valid_appropriation_bar, deontological).
narrative_ontology:cs_reference_frame('21289d61-a0d6-49c9-a895-1983e114648b', commons_heritage_governance_framework).
narrative_ontology:cs_drift_state('21289d61-a0d6-49c9-a895-1983e114648b', post_commercial_extraction_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('21289d61-a0d6-49c9-a895-1983e114648b', '').
narrative_ontology:cs_kernel_id(ost_article_ii_non_appropriation__extraction_permissive, ost_article_ii_non_appropriation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__extraction_permissive, technologically_advanced_spacefaring_states).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__extraction_permissive, private_extraction_corporations).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__extraction_permissive, non_spacefaring_states).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__extraction_permissive, indigenous_and_global_south_populations).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ost_article_ii_non_appropriation__extraction_permissive, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(ost_article_ii_non_appropriation__extraction_permissive, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ost_article_ii_non_appropriation__extraction_permissive_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ost_article_ii_non_appropriation__extraction_permissive, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ost_article_ii_non_appropriation__extraction_permissive_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78 at interval end) because the reading gates resource access entirely by technological capability and flag-state recognition—non-spacefaring states get zero share of resources extracted from bodies they putatively co-own. The measurement series shows acceleration: extractiveness was 0.15 in 1967 (mere interpretation, no actual extraction) and 0.32 by 1985 (Soviet and US operations beginning), rising to 0.68 by 2025 (commercial lunar mining, asteroid prospecting now operationalized). Suppression is high (0.71) because the reading's persistence requires active enforcement: blocking alternative interpretations, defending flag-state licensing monopolies against conservation challenges, preventing non-spacefaring-state coalitions from triggering treaty amendment or international-regime creation. Theater rises from 0.08 to 0.42 over the interval: early justifications emphasized scientific exploration and preventing wasteful races (genuine coordination problems); by 2025, the same machinery defends pure profit extraction, and justifications grow increasingly focused on 'space development' rhetoric divorced from resource-sharing or conservation concerns. Accessibility collapse is moderate (0.62): alternatives to the extraction-permissive reading exist (conservation reading, international-regime reading) and are actively advocated, but non-spacefaring states cannot mount them effectively because they lack technological leverage. Resistance is substantial (0.68): non-spacefaring states and conservation advocates actively contest the reading in UN forums, propose treaty amendments (repeatedly blocked), and marshal scientific arguments against irreversible depletion—yet their resistance does not prevent licensed extraction or change the rule.
 *
 * PERSPECTIVAL GAP:
 *   From spacefaring states' seat, the extraction-permissive reading is a natural interpretation that permits beneficial space development while preventing wasteful open-access competition—a genuine rope-coordination solution. From non-spacefaring states' seat, the same structure operates as enforced enclosure: a reading that was not adopted through multilateral consent, that contradicts the treaty's stated commons-governance purpose, and that is sustained by technological monopoly rather than legitimacy. The engine's per-seat computation should reflect this divergence: spacefaring-state seats may compute as rope (genuine coordination, low asymmetric cost), while non-spacefaring-state and victim seats compute as snare or tangled_rope (asymmetric extraction with enforcement needed to suppress alternatives and maintain the reading's dominance).
 *
 * DIRECTIONALITY LOGIC:
 *   The core asymmetry is technological monopoly on interpretation authority. Only spacefaring states can mount countervailing extraction or enforcement; non-spacefaring states can only interpret, advocate, and propose amendments. The constraint's persistence depends on spacefaring states' continued technological dominance and non-spacefaring states' inability to mount effective coalitions. If non-spacefaring states ever developed indigenous space capability, they could adopt the conservation reading or international-regime reading unilaterally—but that technological development is itself blocked by the resource-extraction economics the constraint enables (extracted resources flow to advanced states, capital for competing space programs stays concentrated). The lock is self-reinforcing.
 *
 * MANDATROPHY ANALYSIS:
 *   The extraction-permissive reading avoids the mandatrophy label (where function is gone but structure persists) only if one accepts the 'space development' justification as the founding problem that remains live. The spacefaring-state narrative: 'We need extraction-permissive reading to incentivize investment in space mining and lunar resource utilization.' The non-spacefaring-state and conservation narrative: 'That founding problem (uncertainty inhibiting investment) was solved fifty years ago; the constraint now persists purely as rent capture, divorced from its original rationale.' The measurement series supports the second reading: theater_ratio rises from 0.08 (genuine coordination problem, real uncertainty) to 0.42 (routine resource extraction justified by abstract 'development' rhetoric, persistent emphasis on private property rights increasingly divorced from commons governance). The constraint sits at the boundary between tangled_rope (coordination function present but extraction dominant) and piton (coordination function atrophied, extraction persists theatrically). It is not yet piton because spacefaring states genuinely benefit from the reading and could maintain it costlessly; they are not theater-maintaining it against their interests. But the measurement trajectory suggests drift toward piton if the founding coordination problem (space-development investment uncertainty) becomes permanently resolved—at which point the constraint would persist as pure rent extraction and theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_foreclosure_status,
    'Does the extraction-permissive reading logically foreclose the commons-conservation reading, or do they merely coexist as competing interpretations of an ambiguous text?',
    'Linguistic and historical analysis of appropriation doctrine in property law and treaty interpretation; expert testimony from treaty scholars representing both readings.',
    'If foreclosure is confirmed, extraction-permissive is the only defensible reading and conservation reading is logically impossible. If coexistence is confirmed, both readings remain live and the constraint classification hinges on which reading is operative de facto (presently extraction-permissive, but subject to future interpretation shifts). This omega gates whether the constraint is structurally singular or fundamentally contested.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_foreclosure_status, conceptual, 'Whether extraction-permissive and commons-conservation readings are textually incompatible (foreclosure) or merely different interpretations of ambiguous language (coexistence).').

omega_variable(
    spacefaring_state_enforcement_divergence,
    'Do spacefaring states genuinely enforce the extraction-permissive reading through active suppression of alternatives, or is their enforcement limited to protecting private extraction claims while remaining neutral on competing interpretations?',
    'Document spacefaring-state behavior in UN forums, treaty negotiations, and domestic law: do they actively block conservation amendments, sponsor counter-resolutions asserting extraction-permissive authority, or license extraction explicitly to demonstrate their reading? Or do they permit conservation advocates equal voice and legal standing, treating the reading contest as open? Analysis of UNOOSA records, state positions in General Assembly debates, and domestic legislation (e.g., Luxembourg''s 2017 space resources law, US commercial space legislation) over the past 15 years.',
    'Active enforcement (blocking amendments, counter-resolutions, licensing to operationalize the reading) indicates suppression is structural and intentional—the constraint persists because spacefaring states maintain it against alternatives. Passive enforcement (neutral framework permitting private extraction but not actively blocking alternatives) would suggest the extraction-permissive reading is simply the default interpretation absent competing consensus—less extractive as a constraint, more like a Rope. High suppression indicates Tangled Rope or Snare; low suppression indicates Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(spacefaring_state_enforcement_divergence, empirical, 'Whether spacefaring states actively suppress conservation and international-regime alternatives or remain neutral on interpretation contests.').

omega_variable(
    non_spacefaring_state_coalition_capacity,
    'Could a coalition of non-spacefaring states muster sufficient diplomatic or economic leverage to force treaty amendment toward conservation or international-regime reading, if they chose to invest in such a campaign?',
    'Analysis of UN coalition dynamics: what fraction of UN membership would support conservation/international-regime amendment? Do any spacefaring states (e.g., developing spacefaring nations) have incentives to defect from the extraction-permissive cartel? Are there economic leverage points (e.g., Earth-based resources non-spacefaring states control) that could be deployed? Historical precedent from other treaty amendments (Montreal Protocol, Paris Agreement) showing coalition paths to change entrenched interpretation.',
    'If coalition capacity is high (2/3 or more of UN membership could support amendment, some spacefaring-state defection possible), then non-spacefaring states are constrained but not trapped—their exit is difficult but possible. If coalition capacity is low (amendment impossible absent spacefaring-state consent, no defection incentives, no leverage points), then non-spacefaring states are truly trapped and the constraint is highly extractive. Coalition capacity affects directionality: low capacity (d near 1.0 for non-spacefaring states) vs. high capacity (d might lower toward 0.7).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_spacefaring_state_coalition_capacity, empirical, 'Whether non-spacefaring states possess sufficient coalition power to force treaty amendment absent spacefaring-state consent.').

omega_variable(
    private_property_vs_usufruct_distinction,
    'Is the distinction between private property ownership (extraction-permissive reading) and usufruct/use rights (commons-conservation reading) semantically clear in treaty language, or is it a constructed distinction that conflates property regimes?',
    'Comparative law analysis of usufruct traditions (Roman law, civil law systems, indigenous commons regimes) vs. absolute private property. Examine treaty-drafting history: did the original drafters intend to permit or prohibit private property in extracted resources? Do any contemporaneous statements from USSR, USA, or other drafters address this question? Analyze how other treaty provisions (Article XI''s international-regime language, Article V''s jurisdictional provisions) treat the property question.',
    'If the property/usufruct distinction is clear and intentional, the extraction-permissive reading''s claim to private ownership is well-grounded. If the distinction is constructed or ambiguous, extraction-permissive is reading a property right into the text that was never explicitly granted—it becomes a more aggressive interpretation, possibly an instance of false natural law (reading a constructed distinction as natural/obvious). This affects legitimacy of the reading and feeds into mandatrophy analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(private_property_vs_usufruct_distinction, conceptual, 'Whether the property-ownership foundation of the extraction-permissive reading is semantically explicit or constructed.').

omega_variable(
    conservation_reading_viability,
    'Is the commons-conservation reading genuinely viable as an alternative governance path, or does it face insurmountable practical obstacles (e.g., monitoring celestial-body resource use, enforcing conservation limits at orbital distances, coordinating global conservation enforcement)?',
    'Technical and governance analysis: what would enforcement of a conservation regime require? What monitoring and verification capacity would be necessary? Are there precedents from terrestrial commons (Antarctica Treaty, Law of the Sea regimes) that succeeded or failed at large-scale commons governance? Model the transaction costs of maintaining a conservation regime vs. the transaction costs of maintaining extraction-permissive enforcement.',
    'If conservation reading faces severe practical obstacles that extraction-permissive avoids (e.g., monitoring is impossible, enforcement is infeasible), then the extraction-permissive reading may be justified as the pragmatic default despite commons-governance ideals—it solves a genuine coordination problem (how to permit space activity without tragedy of the commons) that conservation reading cannot solve. If practical obstacles are surmountable (monitoring is feasible with modern technology, enforcement models exist from other regimes), then extraction-permissive is a choice, not an inevitability—making it more clearly extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conservation_reading_viability, empirical, 'Whether conservation-reading governance is practically feasible or faces insurmountable implementation obstacles.').

omega_variable(
    identity_lock_mechanism_interpersonal_analog,
    'For non-spacefaring states coded as identity_locked, what is the mechanism binding them? Is it treaty signatory identity (institutional lock to the Outer Space Treaty framework), national-development identity (self-conception as a developing space-capable nation in the future), or both?',
    'Survey non-spacefaring-state delegations and diplomats: would they remain bound to the Outer Space Treaty if it permitted them to exit cleanly via a separate framework (e.g., a Global South space-governance treaty)? Or do they regard the OST as non-negotiable even though they disagree with the extraction-permissive reading? Analyze statements from non-spacefaring-state representatives in UN forums: do they frame their objection as a contractual obligation they must honor despite disagreement (treaty identity lock), or as a developmental aspiration they hope to achieve (future space-capability identity lock)?',
    'If the lock is purely institutional (treaty signatory identity, obligation to honor the text regardless of interpretation), then non-spacefaring states could theoretically exit by withdrawing from the treaty—making their lock weaker than identity_locked coding suggests. If the lock includes developmental identity (self-concept as a future spacefaring nation, stake in the integrity of space governance for when they develop capability), then their lock is deeper and more entrenched. Identity-lock classification and the strength of the lock directly affect directionality: tighter lock = higher d (more target-like).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_interpersonal_analog, empirical, 'What mechanism binds non-spacefaring states to the Outer Space Treaty despite their disagreement with the extraction-permissive reading: institutional obligation, developmental aspiration, or both.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ost_article_ii_non_appropriation__extraction_permissive, 1967, 2050).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ost__tr_t1967, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 1967, 0.08).
narrative_ontology:measurement(ost__tr_t1985, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 1985, 0.12).
narrative_ontology:measurement(ost__tr_t2010, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 2010, 0.28).
narrative_ontology:measurement(ost__tr_t2025, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 2025, 0.38).
narrative_ontology:measurement(ost__tr_t2040, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 2040, 0.41).
narrative_ontology:measurement(ost__tr_t2050, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 2050, 0.42).

% Extraction over time
narrative_ontology:measurement(ost__be_t1967, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 1967, 0.15).
narrative_ontology:measurement(ost__be_t1985, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 1985, 0.32).
narrative_ontology:measurement(ost__be_t2010, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 2010, 0.54).
narrative_ontology:measurement(ost__be_t2025, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 2025, 0.68).
narrative_ontology:measurement(ost__be_t2040, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 2040, 0.75).
narrative_ontology:measurement(ost__be_t2050, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 2050, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(ost__su_t1967, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 1967, 0.35).
narrative_ontology:measurement(ost__su_t1985, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 1985, 0.48).
narrative_ontology:measurement(ost__su_t2010, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 2010, 0.61).
narrative_ontology:measurement(ost__su_t2025, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 2025, 0.68).
narrative_ontology:measurement(ost__su_t2040, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 2040, 0.7).
narrative_ontology:measurement(ost__su_t2050, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 2050, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ost_article_ii_non_appropriation__extraction_permissive, resource_allocation).
narrative_ontology:boltzmann_floor_override(ost_article_ii_non_appropriation__extraction_permissive, 0.18).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__extraction_permissive, ost_article_ii_non_appropriation__commons_conservation).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__extraction_permissive, ost_article_ii_non_appropriation__international_regime).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__extraction_permissive, moon_agreement_benefit_sharing_regime).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__extraction_permissive, deep_seabed_mining_isp_precedent).

% DUAL FORMULATION NOTE:
% The Outer Space Treaty's Article II non-appropriation clause is a contested kernel permitting three structurally distinct constraint readings: commons_conservation (extraction itself constitutes appropriation), extraction_permissive (this story—appropriation barred but extraction permitted), and international_regime (appropriation deferred to future regime). Each reading is a separate constraint story with its own epsilon value, beneficiary/victim structure, and enforcement mechanism. The extraction-permissive reading (this file) instantiates high-extractiveness enclosure via technological gatekeeping. The commons-conservation reading instantiates a mountain or rope (resource preservation as natural law or genuine coordination). The international_regime reading instantiates a scaffold with a sunset clause (deferral to future regime creation). These are NOT different measurements of one constraint—they are different constraints, each with distinct ε-invariant structural data, linked via network.affects_constraints to document the kernel family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ost_article_ii_non_appropriation__extraction_permissive, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
