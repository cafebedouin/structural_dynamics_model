% ============================================================================
% CONSTRAINT STORY: commerce_clause_text__expansive_federal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_text__expansive_federal_reading, []).

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
 *   constraint_id: commerce_clause_text__expansive_federal_reading
 *   human_readable: Commerce Clause Expansive Federal Reading
 *   domain: constitutional_law/federalism
 *
 * SUMMARY:
 *   The expansive federal reading of the Commerce Clause holds that Congress
 *   may regulate any economic activity that, in the aggregate, substantially
 *   affects interstate commerce. This reading, crystallized in Wickard v.
 *   Filburn (1942) and reaffirmed in Gonzales v. Raich (2005), treats the
 *   distinction between interstate and intrastate commerce as functionally
 *   irrelevant when local activity has cumulative national market effects.
 *   The constraint coordinates national economic regulation but extracts
 *   regulatory sovereignty from states. The claimed_type is tangled_rope:
 *   genuine coordination of national markets coexists with asymmetric
 *   extraction of state autonomy. The metrics reflect the post-New Deal
 *   trajectory: extraction rises as the aggregation principle expands;
 *   suppression rises as preemption doctrine hardens; theater rises as
 *   federalism rhetoric persists while doctrinal limits erode.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_text__expansive_federal_reading, 0.72).
domain_priors:suppression_score(commerce_clause_text__expansive_federal_reading, 0.65).
domain_priors:theater_ratio(commerce_clause_text__expansive_federal_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(commerce_clause_text__expansive_federal_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_text__expansive_federal_reading, tangled_rope).
narrative_ontology:human_readable(commerce_clause_text__expansive_federal_reading, "Commerce Clause Expansive Federal Reading").
narrative_ontology:topic_domain(commerce_clause_text__expansive_federal_reading, "constitutional_law/federalism").

domain_priors:requires_active_enforcement(commerce_clause_text__expansive_federal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_text__expansive_federal_reading, 'ed1f6a65-16cc-4632-b907-c909601bfb4a').
narrative_ontology:cs_kernel_codification('ed1f6a65-16cc-4632-b907-c909601bfb4a', fixed_text).
narrative_ontology:cs_authority_grounding('ed1f6a65-16cc-4632-b907-c909601bfb4a', lineage).
narrative_ontology:cs_interpretation_layer_present('ed1f6a65-16cc-4632-b907-c909601bfb4a').
narrative_ontology:cs_reading_relation('ed1f6a65-16cc-4632-b907-c909601bfb4a', commerce_clause_text__originalist_narrow_reading, coexists_with).
narrative_ontology:cs_reading_relation('ed1f6a65-16cc-4632-b907-c909601bfb4a', commerce_clause_text__substantial_effects_limited_reading, influences).
narrative_ontology:cs_axiom('ed1f6a65-16cc-4632-b907-c909601bfb4a', foundational, aggregate_effects_suffice_for_federal_power).
narrative_ontology:cs_axiom_status(aggregate_effects_suffice_for_federal_power, holdable).
narrative_ontology:cs_axiom_grounding('ed1f6a65-16cc-4632-b907-c909601bfb4a', aggregate_effects_suffice_for_federal_power, conventional).
narrative_ontology:cs_axiom('ed1f6a65-16cc-4632-b907-c909601bfb4a', foundational, local_activity_aggregation_principle).
narrative_ontology:cs_axiom_status(local_activity_aggregation_principle, holdable).
narrative_ontology:cs_axiom_grounding('ed1f6a65-16cc-4632-b907-c909601bfb4a', local_activity_aggregation_principle, conventional).
narrative_ontology:cs_reference_frame('ed1f6a65-16cc-4632-b907-c909601bfb4a', new_deal_settlement).
narrative_ontology:cs_drift_state('ed1f6a65-16cc-4632-b907-c909601bfb4a', contemporary_federalism_revival, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ed1f6a65-16cc-4632-b907-c909601bfb4a', '').
narrative_ontology:cs_kernel_id(commerce_clause_text__expansive_federal_reading, commerce_clause_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_text__expansive_federal_reading, federal_administrative_state).
narrative_ontology:constraint_beneficiary(commerce_clause_text__expansive_federal_reading, national_policy_coherence_advocates).
narrative_ontology:constraint_victim(commerce_clause_text__expansive_federal_reading, state_autonomy).
narrative_ontology:constraint_victim(commerce_clause_text__expansive_federal_reading, local_variation).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(commerce_clause_text__expansive_federal_reading, state_governments).
narrative_ontology:constraint_victim(commerce_clause_text__expansive_federal_reading, local_communities).
narrative_ontology:constraint_vindicates(commerce_clause_text__expansive_federal_reading, federal_supremacy_doctrine).
narrative_ontology:constraint_vindicates(commerce_clause_text__expansive_federal_reading, national_market_coherence).
narrative_ontology:constraint_vindicates(commerce_clause_text__expansive_federal_reading, aggregation_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercises regulatory authority over all economic activity with substantial aggregate effects on national markets through Commerce Clause jurisprudence. Sets the scope of federal power via legislation and defends it in courts. Collects regulatory authority as primary beneficiary; can shift regulatory burdens to states or private actors. Exit means abandoning national regulatory coherence — not a live option for the institutional imperative.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, federal_government, agenda_setter,
    institutional, generational, arbitrage, national).

% Lose regulatory autonomy over intrastate economic activity that the expansive reading sweeps into federal power. Must comply with federal standards that preempt state law; can resist through litigation, interstate compacts, or political pressure but cannot exit the federal system. Bear the cost of displaced regulatory authority and compliance with uniform national rules that may not fit local conditions.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, state_governments, payer,
    organized, generational, constrained, regional).

% Gain a unified national regulatory floor preventing race-to-bottom competition among states. Includes civil rights organizations, consumer protection groups, environmental advocates, and labor unions who rely on federal power to set baseline standards. Benefit from the constraint without administering it; exit means advocating at state level — possible but fragmented.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, national_policy_coherence_advocates, beneficiary,
    organized, biographical, mobile, national).

% The agencies (EPA, SEC, NLRB, FDA, etc.) whose enabling statutes rest on the expansive Commerce Clause reading. They both benefit from the regulatory jurisdiction and help define its boundaries through rulemaking and adjudication. Institutional survival depends on the reading's persistence; exit is institutional dissolution.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, federal_administrative_state, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_text__expansive_federal_reading, federal_administrative_state, agenda_setter).

% Subject to federal regulations that may not reflect local economic conditions, cultural values, or political preferences. Can petition for exemptions or waivers but lack structural power to resist preemption. Exit means relocation or political mobilization — both costly and uncertain.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, local_communities, payer,
    moderate, biographical, constrained, local).

% The Supreme Court and lower federal courts that articulate and enforce the doctrinal boundaries (substantial effects test, aggregation principle, Necessary and Proper Clause). They administer the constraint by deciding which regulations fall within federal power. Their authority derives from the reading's legitimacy; they are neither pure beneficiaries nor payers but the enforcement mechanism itself.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, federal_courts, agenda_setter,
    institutional, generational, analytical, national).

% Advocate for the originalist_narrow_reading that would restrict Commerce Clause to trade crossing state borders. Hold influential positions (federal judgeships, academic posts) but are structurally excluded from the prevailing doctrinal framework — their arguments are heard but rarely adopted as controlling law. Would object to the expansive reading's displacement of state police power but cannot exit the institutional obligation to apply binding precedent.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, originalist_scholars_and_judges, excluded,
    powerful, generational, trapped, national).

% Analyze the constraint's doctrinal evolution, empirical effects, and theoretical coherence from outside the enforcement structure. Provide the intellectual raw material for all three readings but do not collect regulatory authority or bear compliance costs. Exit is irrelevant — the analytical seat is voluntary.
narrative_ontology:constraint_stakeholder(commerce_clause_text__expansive_federal_reading, constitutional_law_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates a unified national regulatory framework that prevents regulatory fragmentation and race-to-bottom competition among states, enabling coherent national solutions to economic externalities that cross state lines (pollution, labor standards, financial stability, civil rights enforcement).
% TRANSFER_FUNCTION: Moves regulatory authority over intrastate economic activity from state governments to the federal administrative state, justified by the aggregate effects of that activity on national markets. The transfer includes both legislative power and the enforcement apparatus of federal agencies.
% ABSENT_VOICES: Local communities whose regulatory preferences are preempted without representation in the federal process; small states with distinct economic conditions that cannot influence national majorities; future generations who inherit a constitutional structure where the Commerce Clause has become a general police power clause.
% DISAPPEARANCE_RATIONALE: If the expansive reading vanished overnight, the constitutional basis for 70%+ of federal regulatory statutes (environmental, labor, consumer protection, civil rights, healthcare, financial regulation) would collapse. States would immediately reclaim regulatory authority, producing a fragmented patchwork of standards. The federal administrative state would lose its primary constitutional foundation. The national economy would reorganize around state-level regulation within months.
% FOUNDING_PROBLEM: The Articles of Confederation failed because states could not coordinate on interstate commerce, leading to trade wars, currency instability, and inability to respond to national economic crises. The Commerce Clause was drafted to give Congress power to regulate commerce 'among the several states' to solve this coordination failure.
% FOUNDING_PROBLEM_CORROBORATION: The Federalist Papers (Nos. 11, 22, 42) and Constitutional Convention records confirm the founding problem was interstate trade barriers and coordination failure. However, originalist scholars (e.g., Barnett, Epstein) corroborate that the founding generation did not envision federal power over all economic activity with aggregate effects — they understood 'commerce' as trade/exchange, not all economic activity. The expansive reading's genealogy relies on the New Deal Court's reinterpretation, not the founding understanding. No non-beneficiary source corroborates that the founding problem required the aggregation principle or substantial effects doctrine as originally understood.
narrative_ontology:disappearance_verdict(commerce_clause_text__expansive_federal_reading, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_text__expansive_federal_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_text__expansive_federal_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(commerce_clause_text__expansive_federal_reading, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_text__expansive_federal_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_text__expansive_federal_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(commerce_clause_text__expansive_federal_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(commerce_clause_text__expansive_federal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the reading transfers vast regulatory authority from states to federal agencies without compensation or consent. Suppression (0.65) is substantial because preemption doctrine actively displaces state law and the substantial effects test is nearly impossible to fail — the constraint suppresses exit by making the federal power boundary recede as the regulatory state advances. Theater (0.38) is moderate: the Court maintains federalism rhetoric (Lopez, Morrison, NFIB v. Sebelius) while the doctrinal core remains expansive. Accessibility collapse (0.55) reflects that states retain theoretical sovereignty but practical alternatives are constrained by the supremacy of federal law. Resistance (0.45) is moderate: states litigate and resist politically but cannot structurally exit.
 *
 * PERSPECTIVAL GAP:
 *   From the federal seat, the constraint is genuine coordination solving interstate externalities. From the state seat, it is enforced extraction of sovereign authority. From the local community seat, it is distant rule with no voice. The engine will compute these divergences from the structural data — the expansive reading's coherence as coordination depends entirely on which seat you occupy.
 *
 * DIRECTIONALITY LOGIC:
 *   Federal government and administrative state are structural beneficiaries (d ~ 0.15) — they collect regulatory authority and institutional survival. State governments are primary payers (d ~ 0.85) — they lose regulatory autonomy with constrained exit. National policy advocates are beneficiaries (d ~ 0.25) — gain coordination without bearing enforcement costs. Local communities are payers (d ~ 0.80) — subject to distant regulation with minimal influence. Federal courts are agenda_setters (d ~ 0.40) — they administer the boundary but their institutional role depends on the reading's legitimacy. Originalist scholars are excluded (d ~ 0.90) — they bear the intellectual cost of a framework they reject but cannot exit the obligation to apply it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (interstate trade coordination) is live but has been solved for the domain originally contemplated (trade crossing borders). The expansive reading applies the solution to a domain (all aggregated economic activity) the founders did not contemplate. This is classic mandatrophy: the mandate (regulate interstate commerce) has outlived its original function and now serves a different function (general federal police power) while retaining the original label. The constraint persists because the federal administrative state and its beneficiaries are too powerful to displace, not because the coordination problem requires this scope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct reading of the commerce_clause_text kernel, or does it describe the same constraint as the sibling readings evaluated from a different angle?',
    'Apply the ε-invariance test: if changing the observable (e.g., measuring federal regulatory reach vs. state autonomy displacement) changes ε, they are different constraints. The expansive reading''s ε (0.72) differs structurally from the originalist reading''s ε (near 0) and the limited reading''s ε (~0.45).',
    'Confirms this is a distinct constraint story in the kernel family. Requires separate JSON files for each reading linked via network.affects_constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Validates the kernel-reading decomposition per DP-001 ε-invariance principle.').

omega_variable(
    coordination_extraction_boundary,
    'Is the national market coordination function structurally separable from the federal power expansion, or does the coordination require this scope of federal authority?',
    'Counterfactual analysis: if Congress retained power only over interstate trade and instrumentalities (originalist reading), would national coordination problems (pollution, labor standards, financial stability) remain unsolvable? Empirical evidence from state-level coordination (interstate compacts, uniform laws) vs. federal preemption.',
    'If separable, the expansive reading is a Snare riding on a Rope''s cover story. If inseparable, the extraction is the price of coordination — genuine Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, empirical, 'Whether the coordination and extraction components are structurally distinct.').

omega_variable(
    pretextual_regulation_boundary,
    'Where does the substantial effects doctrine become a pretext for general federal police power, and is that boundary enforceable?',
    'Track the Court''s jurisprudence: Lopez (1995) and Morrison (2000) attempted to draw a line at non-economic activity; Raich (2005) collapsed it for economic activity; NFIB v. Sebelius (2012) refused to extend it to inactivity. The boundary moves with the Court''s composition.',
    'If no stable boundary exists, the constraint is a Snare (coordination story is pretext). If a stable boundary emerges, it remains Tangled Rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(pretextual_regulation_boundary, conceptual, 'Whether the constraint''s own doctrinal limits are real or performative.').

omega_variable(
    state_coalition_power,
    'Can state governments form effective coalitions to resist federal preemption, or does the constraint''s structure prevent collective action among victims?',
    'Analyze interstate compacts, state attorney general litigation coalitions, and Article V convention movements. Measure whether state resistance achieves doctrinal change or merely delays.',
    'If coalition power is real, state governments'' effective power atom may be ''powerful'' not ''organized'', reducing their d and the constraint''s effective extraction. If illusory, the payer seat is more trapped than authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_coalition_power, empirical, 'Whether victim coalition power modifies the structural extraction calculus.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_text__expansive_federal_reading, 0, 87).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(commerce_clause_expansive_tr_t0, commerce_clause_text__expansive_federal_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(commerce_clause_expansive_tr_t10, commerce_clause_text__expansive_federal_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(commerce_clause_expansive_tr_t20, commerce_clause_text__expansive_federal_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(commerce_clause_expansive_tr_t30, commerce_clause_text__expansive_federal_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement(commerce_clause_expansive_tr_t40, commerce_clause_text__expansive_federal_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement(commerce_clause_expansive_tr_t50, commerce_clause_text__expansive_federal_reading, theater_ratio, 50, 0.27).
narrative_ontology:measurement(commerce_clause_expansive_tr_t60, commerce_clause_text__expansive_federal_reading, theater_ratio, 60, 0.31).
narrative_ontology:measurement(commerce_clause_expansive_tr_t70, commerce_clause_text__expansive_federal_reading, theater_ratio, 70, 0.34).
narrative_ontology:measurement(commerce_clause_expansive_tr_t80, commerce_clause_text__expansive_federal_reading, theater_ratio, 80, 0.36).
narrative_ontology:measurement(commerce_clause_expansive_tr_t87, commerce_clause_text__expansive_federal_reading, theater_ratio, 87, 0.38).

% Extraction over time
narrative_ontology:measurement(commerce_clause_expansive_be_t0, commerce_clause_text__expansive_federal_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(commerce_clause_expansive_be_t10, commerce_clause_text__expansive_federal_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(commerce_clause_expansive_be_t20, commerce_clause_text__expansive_federal_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(commerce_clause_expansive_be_t30, commerce_clause_text__expansive_federal_reading, base_extractiveness, 30, 0.48).
narrative_ontology:measurement(commerce_clause_expansive_be_t40, commerce_clause_text__expansive_federal_reading, base_extractiveness, 40, 0.55).
narrative_ontology:measurement(commerce_clause_expansive_be_t50, commerce_clause_text__expansive_federal_reading, base_extractiveness, 50, 0.62).
narrative_ontology:measurement(commerce_clause_expansive_be_t60, commerce_clause_text__expansive_federal_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement(commerce_clause_expansive_be_t70, commerce_clause_text__expansive_federal_reading, base_extractiveness, 70, 0.7).
narrative_ontology:measurement(commerce_clause_expansive_be_t80, commerce_clause_text__expansive_federal_reading, base_extractiveness, 80, 0.71).
narrative_ontology:measurement(commerce_clause_expansive_be_t87, commerce_clause_text__expansive_federal_reading, base_extractiveness, 87, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(commerce_clause_expansive_su_t0, commerce_clause_text__expansive_federal_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(commerce_clause_expansive_su_t10, commerce_clause_text__expansive_federal_reading, suppression_requirement, 10, 0.42).
narrative_ontology:measurement(commerce_clause_expansive_su_t20, commerce_clause_text__expansive_federal_reading, suppression_requirement, 20, 0.45).
narrative_ontology:measurement(commerce_clause_expansive_su_t30, commerce_clause_text__expansive_federal_reading, suppression_requirement, 30, 0.48).
narrative_ontology:measurement(commerce_clause_expansive_su_t40, commerce_clause_text__expansive_federal_reading, suppression_requirement, 40, 0.52).
narrative_ontology:measurement(commerce_clause_expansive_su_t50, commerce_clause_text__expansive_federal_reading, suppression_requirement, 50, 0.56).
narrative_ontology:measurement(commerce_clause_expansive_su_t60, commerce_clause_text__expansive_federal_reading, suppression_requirement, 60, 0.6).
narrative_ontology:measurement(commerce_clause_expansive_su_t70, commerce_clause_text__expansive_federal_reading, suppression_requirement, 70, 0.62).
narrative_ontology:measurement(commerce_clause_expansive_su_t80, commerce_clause_text__expansive_federal_reading, suppression_requirement, 80, 0.64).
narrative_ontology:measurement(commerce_clause_expansive_su_t87, commerce_clause_text__expansive_federal_reading, suppression_requirement, 87, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_text__expansive_federal_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(commerce_clause_text__expansive_federal_reading, 0.12).
narrative_ontology:affects_constraint(commerce_clause_text__expansive_federal_reading, commerce_clause_text__originalist_narrow_reading).
narrative_ontology:affects_constraint(commerce_clause_text__expansive_federal_reading, commerce_clause_text__substantial_effects_limited_reading).

% DUAL FORMULATION NOTE:
% This constraint is the expansive_federal_reading of the commerce_clause_text kernel. It differs from the originalist_narrow_reading by treating aggregation as sufficient for federal power over all economic activity, and from the substantial_effects_limited_reading by eliminating jurisdictional nexus and non-pretextual requirements. The three readings form a constraint family linked by network.affects_constraints. The expansive reading structurally influences the limited reading by setting the doctrinal ceiling that the limited reading attempts to cabin.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(commerce_clause_text__expansive_federal_reading, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
