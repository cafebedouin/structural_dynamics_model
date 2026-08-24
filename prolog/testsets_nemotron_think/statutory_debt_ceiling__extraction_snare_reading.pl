% ============================================================================
% CONSTRAINT STORY: statutory_debt_ceiling__extraction_snare_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_statutory_debt_ceiling__extraction_snare_reading, []).

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
 *   constraint_id: statutory_debt_ceiling__extraction_snare_reading
 *   human_readable: Statutory Debt Ceiling as Weaponized Extraction Snare
 *   domain: constitutional_law/political_economy/fiscal_governance
 *
 * SUMMARY:
 *   The statutory debt ceiling, originally a WWI-era coordination scaffold
 *   granting Treasury borrowing flexibility, has mutated into a weaponized
 *   boundary. A cohesive legislative minority faction repeatedly threatens
 *   sovereign default — an existential economic event — to extract policy
 *   concessions it cannot win through normal legislating. The constraint's
 *   persistence depends entirely on the minority's willingness to inflict
 *   catastrophic costs on taxpayers, federal employees, bondholders, and
 *   benefit recipients. The coordination story ('forcing fiscal
 *   responsibility') is cover; the real function is asymmetric extraction.
 *   Credit rating downgrades (S&P 2011, Fitch 2023) confirm the extraction is
 *   real and costly. The minority faction benefits politically; the polity
 *   pays.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statutory_debt_ceiling__extraction_snare_reading, 0.85).
domain_priors:suppression_score(statutory_debt_ceiling__extraction_snare_reading, 0.82).
domain_priors:theater_ratio(statutory_debt_ceiling__extraction_snare_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, resistance, 0.63).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statutory_debt_ceiling__extraction_snare_reading, snare).
narrative_ontology:human_readable(statutory_debt_ceiling__extraction_snare_reading, "Statutory Debt Ceiling as Weaponized Extraction Snare").
narrative_ontology:topic_domain(statutory_debt_ceiling__extraction_snare_reading, "constitutional_law/political_economy/fiscal_governance").

domain_priors:requires_active_enforcement(statutory_debt_ceiling__extraction_snare_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statutory_debt_ceiling__extraction_snare_reading, 'db5c96d0-1772-40c3-8566-8f26e894fa5f').
narrative_ontology:cs_kernel_codification('db5c96d0-1772-40c3-8566-8f26e894fa5f', formalized).
narrative_ontology:cs_authority_grounding('db5c96d0-1772-40c3-8566-8f26e894fa5f', extraction).
narrative_ontology:cs_interpretation_layer_present('db5c96d0-1772-40c3-8566-8f26e894fa5f').
narrative_ontology:cs_reading_relation('db5c96d0-1772-40c3-8566-8f26e894fa5f', statutory_debt_ceiling__coordination_scaffold_reading, coexists_with).
narrative_ontology:cs_reading_relation('db5c96d0-1772-40c3-8566-8f26e894fa5f', statutory_debt_ceiling__constitutional_nullity_reading, coexists_with).
narrative_ontology:cs_axiom('db5c96d0-1772-40c3-8566-8f26e894fa5f', foundational, debt_ceiling_enables_minority_extraction).
narrative_ontology:cs_axiom_status(debt_ceiling_enables_minority_extraction, holdable).
narrative_ontology:cs_axiom_grounding('db5c96d0-1772-40c3-8566-8f26e894fa5f', debt_ceiling_enables_minority_extraction, empirically_contingent).
narrative_ontology:cs_axiom('db5c96d0-1772-40c3-8566-8f26e894fa5f', secondary, default_threat_as_legislative_leverage).
narrative_ontology:cs_axiom_status(default_threat_as_legislative_leverage, holdable).
narrative_ontology:cs_axiom_grounding('db5c96d0-1772-40c3-8566-8f26e894fa5f', default_threat_as_legislative_leverage, instrumental).
narrative_ontology:cs_reference_frame('db5c96d0-1772-40c3-8566-8f26e894fa5f', statutory_debt_limit_as_legislative_tool).
narrative_ontology:cs_drift_state('db5c96d0-1772-40c3-8566-8f26e894fa5f', contemporary_partisan_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('db5c96d0-1772-40c3-8566-8f26e894fa5f', '').
narrative_ontology:cs_kernel_id(statutory_debt_ceiling__extraction_snare_reading, statutory_debt_ceiling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__extraction_snare_reading, legislative_minority_faction).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, taxpayers).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, federal_employees).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, bondholders).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, social_security_recipients).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, treasury_department).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, majority_party).
narrative_ontology:constraint_vindicates(statutory_debt_ceiling__extraction_snare_reading, legislative_power_of_the_purse).
narrative_ontology:constraint_vindicates(statutory_debt_ceiling__extraction_snare_reading, fiscal_responsibility_rhetoric).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A cohesive minority bloc in the legislature that refuses to raise the debt ceiling unless policy concessions are extracted. They control enough votes to block must-pass legislation, using the threat of sovereign default as leverage. They benefit politically and ideologically from the concessions won and from the performance of fiscal brinkmanship.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, legislative_minority_faction, agenda_setter,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(statutory_debt_ceiling__extraction_snare_reading, legislative_minority_faction, beneficiary).

% The executive agency tasked with managing federal finances. When the ceiling binds, Treasury must deploy extraordinary measures (suspending investments, shifting funds) to avoid default, bearing operational costs, reputational risk, and legal uncertainty. It cannot exit the constraint; it must administer it.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, treasury_department, payer,
    institutional, immediate, trapped, national).

% Bear the macroeconomic costs of brinkmanship: higher borrowing costs, credit rating downgrades, market volatility, and potential recession. They have no direct exit from the sovereign's fiscal commitments.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, taxpayers, payer,
    powerless, biographical, trapped, national).

% Face furloughs, delayed paychecks, and operational chaos during impasses. Their livelihoods are hostage to the standoff; exit requires leaving public service.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, federal_employees, payer,
    moderate, immediate, constrained, national).

% Hold U.S. Treasuries as the world's premier safe asset. Default risk forces them to demand higher yields or shift portfolios, imposing a risk premium on the entire debt stock. They can exit to other sovereign debt but at systemic cost.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, bondholders, payer,
    powerful, biographical, mobile, global).

% Depend on timely benefit payments. Debt ceiling standoffs create direct threat of missed checks. No exit; they are the most vulnerable hostages.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, social_security_recipients, payer,
    powerless, biographical, trapped, national).

% Holds legislative majority but cannot raise the ceiling without minority cooperation due to procedural rules (filibuster, discharge petition difficulty). Bears political blame for dysfunction and concedes policy ground to avoid default.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, majority_party, payer,
    organized, biographical, constrained, national).

% Assess sovereign creditworthiness. Downgrade or threaten downgrade based on governance dysfunction, not fiscal capacity. Their judgments amplify the extraction by raising borrowing costs.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, credit_rating_agencies, observer,
    analytical, generational, analytical, global).

% Monetary authority that may intervene to stabilize markets during a default crisis (e.g., buying Treasuries, providing liquidity). Its independence is strained by being drawn into fiscal politics.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, federal_reserve, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The debt ceiling nominally coordinates congressional oversight of executive borrowing, forcing periodic legislative attention to fiscal trajectory.
% TRANSFER_FUNCTION: Moves policy concessions and legislative agenda control from the majority to the minority faction, under the threat of imposing sovereign default costs on the entire polity (taxpayers, employees, beneficiaries, bondholders).
% ABSENT_VOICES: Future generations who inherit the debt and the degraded fiscal credibility; state and local governments whose borrowing costs rise with Treasury yields; foreign central banks and sovereign wealth funds that hold Treasuries as reserves — they have no voice in U.S. legislative procedure but bear the spillover.
% DISAPPEARANCE_RATIONALE: If the debt ceiling vanished overnight, Congress would lose its recurring hostage-taking mechanism. The minority faction would lose its most potent leverage. Treasury would manage debt without artificial cliffs. The majority would govern without constant brinkmanship. The fiscal policymaking process would rearrange toward continuous authorization (as in most democracies).
% FOUNDING_PROBLEM: The debt ceiling was created in 1917 (Second Liberty Bond Act) to give Treasury flexibility to issue debt up to a limit during WWI without seeking congressional approval for each issuance. It was a coordination scaffold: streamlining war finance while retaining legislative control.
% FOUNDING_PROBLEM_CORROBORATION: Historians of fiscal policy (e.g., Anita Krishnakumar, 'The Debt Ceiling: A Historical Analysis') and former Treasury officials (e.g., Timothy Geithner, Jacob Lew) attest the original coordination purpose has been obsolete for decades; the limit now binds after spending decisions are already made, not before. The minority faction itself does not claim the ceiling solves a coordination problem — it explicitly uses it as a leverage tool.
narrative_ontology:disappearance_verdict(statutory_debt_ceiling__extraction_snare_reading, world_rearranges).
narrative_ontology:founding_problem_status(statutory_debt_ceiling__extraction_snare_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statutory_debt_ceiling__extraction_snare_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(statutory_debt_ceiling__extraction_snare_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statutory_debt_ceiling__extraction_snare_reading, 0.85, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statutory_debt_ceiling__extraction_snare_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(statutory_debt_ceiling__extraction_snare_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(statutory_debt_ceiling__extraction_snare_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.85) because the concession transfer is large relative to any coordination benefit, and the threat (default) is existential. Suppression is high (0.82) because the constraint actively blocks the majority's ability to govern and suppresses the alternative of clean debt authorization; extraordinary measures are a narrowing exit corridor. Theater ratio is moderate (0.45): the brinkmanship rituals (press conferences, floor speeches, countdown clocks) are performative, but the underlying threat and occasional real concessions are not theater. Accessibility collapse (0.72) reflects that once the ceiling binds, alternatives (platinum coin, 14th Amendment invocation, discharge petition) are legally contested or politically impractical. Resistance (0.63) is substantial — majorities, presidents, markets, and the public resist — but the constraint persists because the minority's veto point is structural.
 *
 * PERSPECTIVAL GAP:
 *   From the minority faction's seat, the ceiling is a legitimate check on runaway spending (a rope-like coordination). From the payer seats, it is a snare: a hostage mechanism that extracts rents. The engine computes this divergence from the structural data — the authored claim (snare) reflects the payer seats' reality, while the minority's self-justification is a false summit.
 *
 * DIRECTIONALITY LOGIC:
 *   The legislative minority faction is the structural beneficiary (agenda_setter + beneficiary): it sets the terms of the standoff and collects the concessions. Its directionality d is near 0.0 (full beneficiary). Treasury, taxpayers, employees, beneficiaries, and bondholders are payers with d near 1.0 (full targets) — they bear the costs and have no meaningful exit. The majority party is a payer with constrained exit (d ~0.7): it could theoretically change rules or discharge, but the procedural friction is high. Credit rating agencies and the Fed are observers (d=0.5, analytical).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (WWI borrowing flexibility) is dead. The arrangement persists because the minority faction captures the agenda-setting power the ceiling provides. The constraint is not a piton (theatrical inertia) — it is actively maintained because it delivers concentrated benefits to the minority. The theater ratio rise reflects increasing performative brinkmanship, but the extraction is real and growing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_extraction_boundary,
    'Is the debt ceiling''s coordination function (forcing periodic fiscal debate) genuine but captured, or was it always a pretext for minority veto power?',
    'Counterfactual analysis: if the ceiling were replaced by automatic authorization tied to budget resolutions, would fiscal discipline improve or deteriorate? Compare states with and without similar constraints.',
    'If coordination is genuine but captured, the constraint is a tangled_rope (coordination + extraction). If coordination is entirely pretextual, it is a pure snare. Classification shifts at the boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary, conceptual, 'Whether the constraint has a separable coordination function that is being exploited, or whether extraction is its sole function.').

omega_variable(
    constitutional_validity_impact,
    'If the 14th Amendment Section 4 (''validity of the public debt... shall not be questioned'') renders the debt ceiling unconstitutional, does the extraction snare dissolve or shift form?',
    'Supreme Court ruling on a test case (e.g., Treasury ignores ceiling, issues debt, bondholders sue). Until then, the constitutional cloud hangs over every standoff.',
    'If nullity reading prevails, the snare vanishes (world_unchanged for the constraint itself, but world_rearranges for the political equilibrium). If rejected, the snare is legitimized. The constitutional_nullity_reading coexists as a structural pressure on the snare''s legitimacy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(constitutional_validity_impact, conceptual, 'Whether the constitutional challenge fatally undermines the snare''s enforcement mechanism.').

omega_variable(
    minority_faction_cohesion,
    'Does the minority faction''s cohesion depend on the debt ceiling as its primary leverage point, or would it find other hostages (appropriations, nominations) if the ceiling disappeared?',
    'Observe minority tactics in periods when the ceiling is suspended (e.g., 2019-2021). Do they shift to government shutdown threats?',
    'If the faction substitutes other hostages, the extraction is portable — the snare is one instance of a deeper structural asymmetry. If the ceiling is unique, its removal rearranges the world more completely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_faction_cohesion, empirical, 'Whether the extraction snare is specific to the debt ceiling or a general feature of minority veto power.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statutory_debt_ceiling__extraction_snare_reading, 2011, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(debt_ceiling_snare_tr_t2011, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 2011, 0.28).
narrative_ontology:measurement(debt_ceiling_snare_tr_t2013, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 2013, 0.33).
narrative_ontology:measurement(debt_ceiling_snare_tr_t2015, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 2015, 0.38).
narrative_ontology:measurement(debt_ceiling_snare_tr_t2017, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 2017, 0.41).
narrative_ontology:measurement(debt_ceiling_snare_tr_t2019, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 2019, 0.43).
narrative_ontology:measurement(debt_ceiling_snare_tr_t2021, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 2021, 0.44).
narrative_ontology:measurement(debt_ceiling_snare_tr_t2023, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 2023, 0.45).

% Extraction over time
narrative_ontology:measurement(debt_ceiling_snare_be_t2011, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 2011, 0.62).
narrative_ontology:measurement(debt_ceiling_snare_be_t2013, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 2013, 0.71).
narrative_ontology:measurement(debt_ceiling_snare_be_t2015, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 2015, 0.75).
narrative_ontology:measurement(debt_ceiling_snare_be_t2017, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 2017, 0.78).
narrative_ontology:measurement(debt_ceiling_snare_be_t2019, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 2019, 0.81).
narrative_ontology:measurement(debt_ceiling_snare_be_t2021, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 2021, 0.83).
narrative_ontology:measurement(debt_ceiling_snare_be_t2023, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 2023, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(debt_ceiling_snare_su_t2011, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 2011, 0.72).
narrative_ontology:measurement(debt_ceiling_snare_su_t2013, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 2013, 0.75).
narrative_ontology:measurement(debt_ceiling_snare_su_t2015, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 2015, 0.77).
narrative_ontology:measurement(debt_ceiling_snare_su_t2017, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 2017, 0.79).
narrative_ontology:measurement(debt_ceiling_snare_su_t2019, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 2019, 0.8).
narrative_ontology:measurement(debt_ceiling_snare_su_t2021, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 2021, 0.81).
narrative_ontology:measurement(debt_ceiling_snare_su_t2023, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 2023, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statutory_debt_ceiling__extraction_snare_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(statutory_debt_ceiling__extraction_snare_reading, 0.12).
narrative_ontology:affects_constraint(statutory_debt_ceiling__extraction_snare_reading, statutory_debt_ceiling__coordination_scaffold_reading).
narrative_ontology:affects_constraint(statutory_debt_ceiling__extraction_snare_reading, statutory_debt_ceiling__constitutional_nullity_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the 'debt ceiling' label into three structurally distinct claims: (1) coordination_scaffold_reading — a procedural coordination mechanism (low extraction, sunset-warranted); (2) extraction_snare_reading — a weaponized hostage mechanism (high extraction, pure snare); (3) constitutional_nullity_reading — a legally void constraint (mountain-like natural law of constitutional hierarchy). Their ε values differ by >0.5. They are linked because the extraction reading's practice of weaponization fuels the nullity reading's legal argument, and the scaffold reading's historical origin is invoked by both as cover or context.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(statutory_debt_ceiling__extraction_snare_reading, organized, 0.15).
constraint_indexing:directionality_override(statutory_debt_ceiling__extraction_snare_reading, powerless, 0.95).
constraint_indexing:directionality_override(statutory_debt_ceiling__extraction_snare_reading, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
