% ============================================================================
% CONSTRAINT STORY: fair_use_statutory_exception__market_licensing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fair_use_statutory_exception__market_licensing_reading, []).

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
 *   constraint_id: fair_use_statutory_exception__market_licensing_reading
 *   human_readable: Fair Use Statutory Exception: Market Licensing Reading
 *   domain: intellectual_property_law/legal_interpretation/information_economics
 *
 * SUMMARY:
 *   This constraint represents the 'market licensing' reading of the fair use
 *   statutory exception in copyright law. Under this interpretation, any use
 *   of copyrighted material that could potentially be licensed is deemed to
 *   harm the market for licensed uses, thereby precluding a fair use defense.
 *   Fair use is thus restricted to only those uses for which no existing or
 *   potential market for licensing can be identified. This reading
 *   effectively transforms fair use from a balancing right into a narrow
 *   defense that primarily serves to expand the market for copyright holders,
 *   leading to high extractiveness and suppression for secondary creators and
 *   the public.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_statutory_exception__market_licensing_reading, 0.92).
domain_priors:suppression_score(fair_use_statutory_exception__market_licensing_reading, 0.88).
domain_priors:theater_ratio(fair_use_statutory_exception__market_licensing_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, extractiveness, 0.92).
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_statutory_exception__market_licensing_reading, snare).
narrative_ontology:human_readable(fair_use_statutory_exception__market_licensing_reading, "Fair Use Statutory Exception: Market Licensing Reading").
narrative_ontology:topic_domain(fair_use_statutory_exception__market_licensing_reading, "intellectual_property_law/legal_interpretation/information_economics").

domain_priors:requires_active_enforcement(fair_use_statutory_exception__market_licensing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_statutory_exception__market_licensing_reading, 'fbf083df-e68b-4979-8c7a-5a83794ec601').
narrative_ontology:cs_kernel_codification('fbf083df-e68b-4979-8c7a-5a83794ec601', fixed_text).
narrative_ontology:cs_authority_grounding('fbf083df-e68b-4979-8c7a-5a83794ec601', lineage).
narrative_ontology:cs_interpretation_layer_present('fbf083df-e68b-4979-8c7a-5a83794ec601').
narrative_ontology:cs_reading_relation('fbf083df-e68b-4979-8c7a-5a83794ec601', fair_use_statutory_exception__transformative_right_reading, forecloses).
narrative_ontology:cs_reading_relation('fbf083df-e68b-4979-8c7a-5a83794ec601', fair_use_statutory_exception__narrow_defense_reading, influences).
narrative_ontology:cs_axiom('fbf083df-e68b-4979-8c7a-5a83794ec601', foundational, any_licensable_use_harms_market).
narrative_ontology:cs_axiom_status(any_licensable_use_harms_market, holdable).
narrative_ontology:cs_axiom_grounding('fbf083df-e68b-4979-8c7a-5a83794ec601', any_licensable_use_harms_market, conventional).
narrative_ontology:cs_axiom('fbf083df-e68b-4979-8c7a-5a83794ec601', foundational, fair_use_is_market_exception_only).
narrative_ontology:cs_axiom_status(fair_use_is_market_exception_only, holdable).
narrative_ontology:cs_axiom_grounding('fbf083df-e68b-4979-8c7a-5a83794ec601', fair_use_is_market_exception_only, conventional).
narrative_ontology:cs_reference_frame('fbf083df-e68b-4979-8c7a-5a83794ec601', copyright_as_absolute_property).
narrative_ontology:cs_drift_state('fbf083df-e68b-4979-8c7a-5a83794ec601', contemporary_digital_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fbf083df-e68b-4979-8c7a-5a83794ec601', '').
narrative_ontology:cs_kernel_id(fair_use_statutory_exception__market_licensing_reading, fair_use_statutory_exception).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__market_licensing_reading, copyright_holders).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__market_licensing_reading, licensing_agencies).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, content_creators).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, educators).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, researchers).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, public_domain_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocate for and benefit from the broadest possible interpretation of market harm, ensuring that any potential licensed use is protected from fair use claims. They collect licensing fees and assert control over derivative works.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, copyright_holders, agenda_setter,
    institutional, generational, arbitrage, global).

% Profit from brokering licenses for copyrighted works. This reading expands their potential market by reducing the scope of fair use, making more uses subject to licensing.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, licensing_agencies, beneficiary,
    organized, biographical, mobile, global).

% Bear the cost of this interpretation by facing increased legal risk for uses that might otherwise be considered fair. They must either pay for licenses or abandon potentially transformative projects, stifling creative output.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, content_creators, payer,
    moderate, biographical, constrained, global).

% Are severely restricted in using copyrighted materials for teaching and scholarship without explicit licenses. This increases costs and administrative burden, potentially limiting access to educational resources.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, educators, payer,
    powerless, biographical, constrained, national).

% Face significant barriers to text and data mining, archival research, and other scholarly activities that rely on using copyrighted works. Licensing requirements can make large-scale research impractical or impossible.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, researchers, payer,
    moderate, biographical, constrained, global).

% Argue for a robust public domain and broad fair use to foster creativity and access to knowledge. This reading fundamentally undermines their goals by privatizing nearly all potential uses of copyrighted material.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, public_domain_advocates, excluded,
    organized, generational, trapped, global).

% Are tasked with interpreting and applying fair use law. Under this reading, their role shifts towards identifying potential markets for licensing rather than balancing competing interests in free expression and innovation.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This reading coordinates the market for copyrighted works by clearly delineating what uses require a license, thereby reducing ambiguity for rights holders and potential licensees.
% TRANSFER_FUNCTION: Transfers economic value from users and secondary creators to copyright holders and licensing agencies by expanding the scope of licensable uses and restricting non-licensed fair uses.
% ABSENT_VOICES: Public domain advocates, open access proponents, and digital rights activists are largely excluded from the legal and policy discourse that entrenches this reading. They would argue for a more balanced approach that prioritizes public benefit and transformative use.
% DISAPPEARANCE_RATIONALE: If this reading of fair use vanished, the market for copyrighted works would undergo significant restructuring. Many uses currently requiring licenses would become free, reducing revenue for copyright holders but potentially spurring innovation and cultural production. Licensing agencies would see their business models challenged, and courts would need to re-evaluate fair use claims under a different framework.
% FOUNDING_PROBLEM: The original fair use doctrine aimed to balance copyright holders' rights with the public interest in promoting science and the useful arts, preventing copyright from stifling creativity.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, content creators, and public interest groups widely attest that this reading has effectively nullified the original balancing function of fair use, transforming it into a mechanism for market expansion rather than a public right. Copyright holders, however, maintain that it is essential for protecting their investments and incentivizing creation.
narrative_ontology:disappearance_verdict(fair_use_statutory_exception__market_licensing_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_statutory_exception__market_licensing_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_statutory_exception__market_licensing_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(fair_use_statutory_exception__market_licensing_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_statutory_exception__market_licensing_reading, 0.92, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_statutory_exception__market_licensing_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fair_use_statutory_exception__market_licensing_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fair_use_statutory_exception__market_licensing_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is extremely high (0.92) because this reading maximizes the scope of licensable uses, converting nearly all potential uses into revenue streams for copyright holders. Suppression is also very high (0.88) as it actively discourages and legally penalizes non-licensed uses, effectively collapsing alternatives for creators and educators. The theater ratio is low (0.15) because the constraint is highly functional in achieving its goal of market expansion; there is little performative maintenance for a doctrine that is actively enforced to generate revenue. Accessibility collapse is near total (0.95) as the legal interpretation leaves almost no viable non-licensed alternatives. Resistance is high (0.70) due to ongoing legal challenges and advocacy from affected parties.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of copyright holders, this reading is a necessary protection of their property rights and an incentive for creation. From the perspective of creators and the public, it is an extractive mechanism that stifles innovation and access to knowledge. The engine's classification will highlight this divergence by computing a Snare for victims and a Rope/Tangled Rope for beneficiaries, despite the claimed type of Snare.
 *
 * DIRECTIONALITY LOGIC:
 *   Copyright holders and licensing agencies are clear beneficiaries, as this reading directly expands their revenue and control. Content creators, educators, and researchers are victims, facing increased costs, legal risks, and restrictions on their work. Public domain advocates are excluded, as their core principles are undermined. Courts act as observers, applying this interpretation, which shifts their function from balancing interests to identifying market harm.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading exemplifies mandatrophy where the original mandate of fair use (balancing rights with public interest) has atrophied, replaced by a new, unstated mandate of market expansion. The classification as a Snare prevents mislabeling this as a legitimate coordination mechanism, highlighting its purely extractive nature under this interpretation. The 'dead' status of the founding problem further underscores this shift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    market_harm_definition_ambiguity,
    'How broadly should ''harm to the market for licensed uses'' be interpreted? Does it include hypothetical future markets or only existing ones?',
    'Legislative clarification or Supreme Court ruling establishing a precise, limited definition of market harm, potentially distinguishing between existing and merely speculative markets.',
    'A narrower definition would reduce extractiveness and suppression, potentially shifting the constraint towards a Tangled Rope or even a Rope. A broader definition would further entrench its Snare-like characteristics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(market_harm_definition_ambiguity, conceptual, 'Ambiguity in the scope of market harm for fair use analysis.').

omega_variable(
    transformative_use_interaction,
    'To what extent does a transformative use (e.g., parody, commentary) mitigate or negate market harm under this reading?',
    'Judicial precedent that explicitly weighs transformative purpose against market harm, potentially creating a carve-out for highly transformative works even if a market for licensing exists.',
    'If transformative use is given significant weight, it could reduce the constraint''s extractiveness for creators, moving it closer to a Tangled Rope. If market harm always trumps transformation, the Snare classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transformative_use_interaction, empirical, 'The interplay between transformative use and market harm in fair use analysis.').

omega_variable(
    reading_legitimacy_source,
    'Is this market-centric reading a legitimate evolution of copyright jurisprudence, or a capture of the doctrine by commercial interests?',
    'Historical legal analysis tracing the evolution of fair use case law, combined with political economy analysis of lobbying efforts and judicial appointments related to copyright.',
    'If found to be a capture, it would strengthen the Snare classification and highlight the role of power dynamics in shaping legal interpretation. If a legitimate evolution, it would still be extractive but perhaps less ''illegitimate'' in its origin.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_legitimacy_source, preference, 'The normative legitimacy of the market-licensing reading of fair use.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_statutory_exception__market_licensing_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t1980, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 1980, 0.25).
narrative_ontology:measurement(fair_tr_t1990, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(fair_tr_t2000, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(fair_tr_t2010, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 2010, 0.16).
narrative_ontology:measurement(fair_tr_t2024, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(fair_be_t1980, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 1980, 0.6).
narrative_ontology:measurement(fair_be_t1990, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 1990, 0.75).
narrative_ontology:measurement(fair_be_t2000, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 2000, 0.85).
narrative_ontology:measurement(fair_be_t2010, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 2010, 0.9).
narrative_ontology:measurement(fair_be_t2024, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 2024, 0.92).

% Suppression requirement over time
narrative_ontology:measurement(fair_su_t1980, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 1980, 0.55).
narrative_ontology:measurement(fair_su_t1990, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 1990, 0.7).
narrative_ontology:measurement(fair_su_t2000, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 2000, 0.8).
narrative_ontology:measurement(fair_su_t2010, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 2010, 0.85).
narrative_ontology:measurement(fair_su_t2024, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 2024, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_statutory_exception__market_licensing_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fair_use_statutory_exception__market_licensing_reading, copyright_term_extension).
narrative_ontology:affects_constraint(fair_use_statutory_exception__market_licensing_reading, digital_millennium_copyright_act).
narrative_ontology:affects_constraint(fair_use_statutory_exception__market_licensing_reading, transformative_right_reading).
narrative_ontology:affects_constraint(fair_use_statutory_exception__market_licensing_reading, narrow_defense_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'fair_use_statutory_exception' kernel. This 'market_licensing_reading' emphasizes market harm, while the 'transformative_right_reading' prioritizes cultural production and the 'narrow_defense_reading' treats fair use as a strict affirmative defense. All three are distinct constraints linked by their common kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
