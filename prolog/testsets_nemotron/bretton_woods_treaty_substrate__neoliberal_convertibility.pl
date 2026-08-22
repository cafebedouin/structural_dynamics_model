% ============================================================================
% CONSTRAINT STORY: bretton_woods_treaty_substrate__neoliberal_convertibility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bretton_woods_treaty_substrate__neoliberal_convertibility, []).

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
 *   constraint_id: bretton_woods_treaty_substrate__neoliberal_convertibility
 *   human_readable: Neoliberal Convertibility Reading of Bretton Woods Treaty Substrate
 *   domain: international_political_economy/monetary_history/institutional_design
 *
 * SUMMARY:
 *   This constraint story instantiates the 'neoliberal convertibility'
 *   reading of the Bretton Woods treaty substrate. The kernel is the 1944
 *   Articles of Agreement; this reading interprets them as creating a
 *   progressive, legally binding obligation toward full capital account
 *   convertibility — treating capital controls as violations rather than
 *   permitted tools. The reading emerged through IMF institutional practice
 *   (1970s-1990s), not the original text. It structurally benefits
 *   international finance and multinational capital by locking in open
 *   capital accounts, while extracting policy autonomy from national
 *   governments, especially in the Global South. The claimed type is
 *   tangled_rope because the reading does perform a genuine coordination
 *   function (predictable cross-border payments) while simultaneously
 *   enabling asymmetric extraction (financial rents vs. national adjustment
 *   costs). The engine will compute per-seat types from the structural data;
 *   this authoring declares the beneficiary/victim structure and metrics
 *   independently of the claim.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.78).
domain_priors:suppression_score(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.65).
domain_priors:theater_ratio(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, extractiveness, 0.78).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bretton_woods_treaty_substrate__neoliberal_convertibility, tangled_rope).
narrative_ontology:human_readable(bretton_woods_treaty_substrate__neoliberal_convertibility, "Neoliberal Convertibility Reading of Bretton Woods Treaty Substrate").
narrative_ontology:topic_domain(bretton_woods_treaty_substrate__neoliberal_convertibility, "international_political_economy/monetary_history/institutional_design").

domain_priors:requires_active_enforcement(bretton_woods_treaty_substrate__neoliberal_convertibility).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bretton_woods_treaty_substrate__neoliberal_convertibility, 'd7d22025-9c47-4a30-b42e-5dba088b162e').
narrative_ontology:cs_kernel_codification('d7d22025-9c47-4a30-b42e-5dba088b162e', formalized).
narrative_ontology:cs_authority_grounding('d7d22025-9c47-4a30-b42e-5dba088b162e', extraction).
narrative_ontology:cs_interpretation_layer_present('d7d22025-9c47-4a30-b42e-5dba088b162e').
narrative_ontology:cs_reading_relation('d7d22025-9c47-4a30-b42e-5dba088b162e', bretton_woods_treaty_substrate__keynesian_embedded_liberalism, forecloses).
narrative_ontology:cs_reading_relation('d7d22025-9c47-4a30-b42e-5dba088b162e', bretton_woods_treaty_substrate__sovereignty_defense, influences).
narrative_ontology:cs_axiom('d7d22025-9c47-4a30-b42e-5dba088b162e', foundational, capital_account_liberalization_obligation).
narrative_ontology:cs_axiom_status(capital_account_liberalization_obligation, holdable).
narrative_ontology:cs_axiom_grounding('d7d22025-9c47-4a30-b42e-5dba088b162e', capital_account_liberalization_obligation, conventional).
narrative_ontology:cs_axiom('d7d22025-9c47-4a30-b42e-5dba088b162e', foundational, capital_controls_as_treaty_violation).
narrative_ontology:cs_axiom_status(capital_controls_as_treaty_violation, holdable).
narrative_ontology:cs_axiom_grounding('d7d22025-9c47-4a30-b42e-5dba088b162e', capital_controls_as_treaty_violation, conventional).
narrative_ontology:cs_reference_frame('d7d22025-9c47-4a30-b42e-5dba088b162e', original_articles_of_agreement_1944).
narrative_ontology:cs_drift_state('d7d22025-9c47-4a30-b42e-5dba088b162e', post_washington_consensus_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d7d22025-9c47-4a30-b42e-5dba088b162e', '').
narrative_ontology:cs_kernel_id(bretton_woods_treaty_substrate__neoliberal_convertibility, bretton_woods_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__neoliberal_convertibility, international_finance_capital).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__neoliberal_convertibility, multinational_corporations).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__neoliberal_convertibility, global_banks).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__neoliberal_convertibility, national_policy_autonomy).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__neoliberal_convertibility, developing_country_governments).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__neoliberal_convertibility, domestic_labor_markets).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receives unrestricted cross-border capital flows, currency convertibility guarantees, and the ability to allocate capital globally without national restrictions. The reading frames national capital controls as violations of the Bretton Woods treaty substrate rather than legitimate policy tools.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, international_finance_capital, beneficiary,
    institutional, generational, arbitrage, global).

% Gains freedom to structure global operations, tax planning, and supply chains without national capital controls. Lobbies through trade agreements and investment treaties to lock in convertibility commitments that exceed the original IMF Articles.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, multinational_corporations, beneficiary,
    powerful, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(bretton_woods_treaty_substrate__neoliberal_convertibility, multinational_corporations, agenda_setter).

% Administers the cross-border payment and lending infrastructure that convertibility requires. Shapes IMF conditionality and bilateral investment treaties to expand the scope of 'current account convertibility' into full capital account liberalization. Collects intermediation rents from the volume of unrestricted flows.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, global_banks, agenda_setter,
    institutional, generational, mobile, global).

% Loses the ability to manage capital flows, set independent monetary policy, or pursue industrial policy without triggering capital flight and market discipline. The reading treats any capital control as a treaty violation rather than an embedded liberalism safeguard.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, national_policy_autonomy, payer,
    powerful, biographical, constrained, national).

% Faces asymmetric pressure: capital account liberalization is demanded as a condition for market access and crisis lending, while the benefits of volatile short-term flows accrue to external financiers. Exit from the convertibility regime means exclusion from global capital markets.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, developing_country_governments, payer,
    moderate, generational, trapped, national).

% Absorbs the adjustment costs of capital flight and austerity when convertibility commitments are tested. Wage suppression and labor market flexibility become the primary adjustment mechanism because exchange rate and capital control options are ruled out by the reading.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, domestic_labor_markets, payer,
    powerless, biographical, trapped, national).

% Interprets the Articles of Agreement to progressively expand the obligation of convertibility from current account (Article VIII) to capital account (proposed amendment). The reading uses the Fund's surveillance and lending power to enforce an expanding convertibility norm.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, imf_surveillance_architecture, agenda_setter,
    institutional, generational, analytical, global).

% Argues that Bretton Woods was designed to protect policy space for full employment and capital controls were an intended feature, not a bug. Their reading is marginalized in IMF governance and mainstream economics after the 1970s.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, keynesian_policy_advocates, excluded,
    organized, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates a stable, predictable framework for cross-border payments and investment by committing states to currency convertibility and limiting arbitrary exchange restrictions — solving the coordination problem of international monetary disorder that plagued the interwar period.
% TRANSFER_FUNCTION: Transfers policy autonomy and crisis absorption capacity from national governments (especially developing countries) to international financial markets and creditors. The reading converts the Bretton Woods compromise (fixed but adjustable rates with capital controls permitted) into a one-way ratchet toward full capital mobility.
% ABSENT_VOICES: Post-colonial states that negotiated the original Articles expecting capital controls to be permanent features; labor movements and developmental states that used capital controls for industrial policy; the original Keynes/White drafting history which shows capital controls were deliberate, not transitional.
% DISAPPEARANCE_RATIONALE: If the neoliberal convertibility reading vanished overnight, capital controls would return to the policy toolkit as legitimate instruments. Developing countries would regain macroeconomic policy space. The architecture of bilateral investment treaties and IMF conditionality would lose its primary legal-ideological justification. Global finance would face a fragmented regulatory landscape.
% FOUNDING_PROBLEM: The interwar period's competitive devaluations, capital flight, and monetary nationalism destroyed international trade and deepened the Great Depression. Bretton Woods was built to prevent a return to that chaos by creating a rules-based monetary order.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (preventing 1930s-style monetary chaos) is attested by the IMF's own official history and the conference records. The neoliberal reading's claim that this requires full capital account liberalization is contested by the drafting history (Keynes and White both defended capital controls) and by the embedded liberalism reading. No independent corroboration exists for the claim that the founders intended the capital account to be fully liberalized — that is a retrospective construction.
narrative_ontology:disappearance_verdict(bretton_woods_treaty_substrate__neoliberal_convertibility, world_rearranges).
narrative_ontology:founding_problem_status(bretton_woods_treaty_substrate__neoliberal_convertibility, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bretton_woods_treaty_substrate__neoliberal_convertibility, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(bretton_woods_treaty_substrate__neoliberal_convertibility, 'none', 1).
narrative_ontology:epsilon_provenance(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bretton_woods_treaty_substrate__neoliberal_convertibility_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(bretton_woods_treaty_substrate__neoliberal_convertibility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bretton_woods_treaty_substrate__neoliberal_convertibility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.15 (1944, capital controls explicitly permitted) to 0.78 (2024, capital account liberalization as de facto norm) because the reading progressively reinterprets the treaty substrate to expand the scope of convertibility. Suppression rises from 0.10 to 0.65 because enforcement shifts from peer pressure to IMF conditionality, bilateral investment treaties, and market discipline. Theater ratio rises from 0.05 to 0.42 because the coordination rationale (stable payments) becomes increasingly decorative relative to the extraction function (unrestricted capital mobility for rent extraction). The measurement grid is aligned across all three metrics at seven shared time points spanning the interval.
 *
 * PERSPECTIVAL GAP:
 *   From the IMF/international finance seat, this reading is a rope (genuine coordination, minimal coercion). From the developing country government seat, it is a snare (extraction via conditionality, no exit). From the domestic labor seat, it is a snare with identity_locked dynamics (labor cannot exit the nation-state but bears the costs). The engine computes this divergence from the declared power/exit/role structure; the authoring does not reconcile the perspectives.
 *
 * DIRECTIONALITY LOGIC:
 *   International finance capital, MNCs, and global banks are structural beneficiaries (d near 0.0) — they collect the rents of unrestricted mobility and face arbitrage-grade exit. National policy autonomy, developing country governments, and domestic labor markets are targets (d near 1.0) — they bear the adjustment costs with constrained or trapped exit. The IMF surveillance architecture sits as agenda_setter with analytical exit (it administers the interpretation). Keynesian advocates are excluded — their reading was the original framework but is structurally frozen out of the governance that applies this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The original mandate (prevent 1930s monetary chaos via adjustable pegs with capital controls) is dead — the adjustable peg system ended in 1971, capital controls were the intended tool. The neoliberal reading keeps the treaty substrate alive by reinterpreting it as a mandate for full convertibility, which the founders did not intend. This is mandatrophy: the constraint's original function has atrophied but the treaty text is repurposed to legitimate a new function (capital account liberalization) that benefits different parties. The founding_problem_status = contested captures this: the problem the arrangement was built for is gone, but the arrangement persists under a new reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founder_intent_vs_institutional_drift,
    'Did the Bretton Woods founders (Keynes, White, conference delegates) intend Article VIII to create a trajectory toward full capital account liberalization, or was the current account convertibility obligation meant to coexist permanently with capital controls?',
    'Archival research on drafting history, negotiating records, and contemporaneous interpretations. The Keynes plan explicitly included capital controls; the White plan permitted them. The 1944 Articles distinguish current account (Article VIII) from capital account (Article VI).',
    'If founders intended capital controls as permanent, the neoliberal reading is a pure institutional drift with no textual anchor — its extraction is entirely constructed. If founders left the capital account open-ended, the reading has a plausible textual foothold but the trajectory is still an institutional choice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founder_intent_vs_institutional_drift, empirical, 'Whether the neoliberal reading has textual-historical legitimacy or is pure institutional construction').

omega_variable(
    coordination_extraction_separability,
    'Is the coordination function (stable cross-border payments) structurally separable from the extraction function (unrestricted capital mobility for financial rents), or does the former require the latter?',
    'Counterfactual analysis: did the 1946-1971 Bretton Woods system (fixed rates with capital controls) achieve payment stability? Yes — trade and long-term investment grew. The coordination function was served without full capital mobility. The extraction function emerged only after the peg system collapsed and the reading redefined convertibility.',
    'If separable, the neoliberal reading is a Tangled Rope where the coordination cover is real but the extraction is additive. If inseparable, the reading''s claimed coordination function is a myth — it would be a Snare with a false coordination story.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether the constraint''s coordination and extraction components are structurally distinct or fused').

omega_variable(
    imf_interpretive_authority_legitimacy,
    'Does the IMF''s interpretive authority to expand ''convertibility'' from current to capital account derive from the Articles'' text, or is it an institutional self-authorization that exploits textual ambiguity?',
    'Legal analysis of Article VIII Sections 2-4 vs. Article VI Section 3. The Fund''s 1990s push for a capital account amendment (which failed) suggests the Articles did not already require it. The interpretive expansion is institutional practice, not textual mandate.',
    'If the expansion is textual, the reading has legal legitimacy; if institutional, the reading''s enforcement machinery (surveillance, conditionality) is an extraction mechanism masquerading as legal obligation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(imf_interpretive_authority_legitimacy, empirical, 'Whether the IMF''s progressive convertibility interpretation has legal basis or is institutional overreach').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bretton_woods_treaty_substrate__neoliberal_convertibility, 1944, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bretton_woods_neolib_conv_tr_t1944, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 1944, 0.05).
narrative_ontology:measurement(bretton_woods_neolib_conv_tr_t1958, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 1958, 0.08).
narrative_ontology:measurement(bretton_woods_neolib_conv_tr_t1971, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 1971, 0.18).
narrative_ontology:measurement(bretton_woods_neolib_conv_tr_t1982, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 1982, 0.28).
narrative_ontology:measurement(bretton_woods_neolib_conv_tr_t1997, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 1997, 0.38).
narrative_ontology:measurement(bretton_woods_neolib_conv_tr_t2008, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 2008, 0.4).
narrative_ontology:measurement(bretton_woods_neolib_conv_tr_t2024, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(bretton_woods_neolib_conv_be_t1944, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 1944, 0.15).
narrative_ontology:measurement(bretton_woods_neolib_conv_be_t1958, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 1958, 0.22).
narrative_ontology:measurement(bretton_woods_neolib_conv_be_t1971, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 1971, 0.35).
narrative_ontology:measurement(bretton_woods_neolib_conv_be_t1982, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 1982, 0.55).
narrative_ontology:measurement(bretton_woods_neolib_conv_be_t1997, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 1997, 0.68).
narrative_ontology:measurement(bretton_woods_neolib_conv_be_t2008, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 2008, 0.72).
narrative_ontology:measurement(bretton_woods_neolib_conv_be_t2024, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(bretton_woods_neolib_conv_su_t1944, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 1944, 0.1).
narrative_ontology:measurement(bretton_woods_neolib_conv_su_t1958, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 1958, 0.15).
narrative_ontology:measurement(bretton_woods_neolib_conv_su_t1971, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 1971, 0.3).
narrative_ontology:measurement(bretton_woods_neolib_conv_su_t1982, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 1982, 0.5).
narrative_ontology:measurement(bretton_woods_neolib_conv_su_t1997, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 1997, 0.6).
narrative_ontology:measurement(bretton_woods_neolib_conv_su_t2008, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 2008, 0.62).
narrative_ontology:measurement(bretton_woods_neolib_conv_su_t2024, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 2024, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bretton_woods_treaty_substrate__neoliberal_convertibility, resource_allocation).
narrative_ontology:boltzmann_floor_override(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.12).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__neoliberal_convertibility, imf_conditionality_architecture).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__neoliberal_convertibility, bilateral_investment_treaty_regime).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__neoliberal_convertibility, capital_account_liberalization_pressure).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__neoliberal_convertibility, bretton_woods_treaty_substrate__keynesian_embedded_liberalism).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__neoliberal_convertibility, bretton_woods_treaty_substrate__sovereignty_defense).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the bretton_woods_treaty_substrate kernel. The keynesian_embedded_liberalism reading (ε ≈ 0.25, claimed_type: rope) treats capital controls as legitimate coordination tools. The sovereignty_defense reading (ε ≈ 0.35, claimed_type: tangled_rope) treats the treaty as protecting national monetary autonomy. This neoliberal_convertibility reading (ε = 0.78, claimed_type: tangled_rope) treats the treaty as mandating capital account liberalization. The ε values differ by >0.5 — these are structurally distinct constraints sharing a textual kernel, not one constraint viewed differently. The neoliberal reading influences the others by embedding its interpretation in IMF practice and investment treaties, creating structural pressure on states that would prefer the keynesian or sovereignty readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bretton_woods_treaty_substrate__neoliberal_convertibility, institutional, 0.15).
constraint_indexing:directionality_override(bretton_woods_treaty_substrate__neoliberal_convertibility, powerful, 0.85).
constraint_indexing:directionality_override(bretton_woods_treaty_substrate__neoliberal_convertibility, moderate, 0.9).
constraint_indexing:directionality_override(bretton_woods_treaty_substrate__neoliberal_convertibility, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
