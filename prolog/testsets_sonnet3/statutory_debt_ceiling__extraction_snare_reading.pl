% ============================================================================
% CONSTRAINT STORY: statutory_debt_ceiling__extraction_snare_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   human_readable: Statutory Debt Ceiling as Hostage-Threat Extraction Mechanism
 *   domain: constitutional_law/political_economy/fiscal_governance
 *
 * SUMMARY:
 *   This story instantiates the extraction_snare reading of the statutory
 *   debt ceiling kernel: since appropriations already fix federal borrowing
 *   needs, the recurring ceiling vote adds no independent fiscal-restraint
 *   function but supplies a periodic veto point that a cohesive legislative
 *   minority can use to threaten default and extract policy concessions it
 *   could not win through ordinary majoritarian bargaining. The costs of the
 *   resulting brinkmanship — benefit-payment disruption, credit downgrades,
 *   workforce pay uncertainty, higher long-run borrowing costs — fall on
 *   parties absent from the negotiation. This is one of three linked readings
 *   of the same kernel (see kernel_context); the coordination_scaffold
 *   reading and the constitutional_nullity reading are separate constraint
 *   files with their own ε and stakeholder structures, not alternative
 *   measurements of this one.
 *
 * KEY AGENTS:
 *   - minority_faction_leadership: Primary beneficiary (organized/arbitrage) — extracts concessions under threat of default without bearing the costs of default
 *   - treasury_department: Institutional executor (institutional/trapped) — absorbs the operational and reputational cost of extraordinary measures with no independent authority to resolve the standoff
 *   - federal_beneficiary_populations, federal_civilian_workforce, state_and_local_governments: Primary targets (powerless-moderate/trapped-constrained) — bear disruption risk with no seat at the table
 *   - treasury_bondholders: Diffuse target (organized/constrained) — bears credit and yield risk from recurring brinkmanship
 *   - constitutional_scholars_and_treasury_counsel: Excluded analytical voice — the Fourteenth Amendment argument that would dissolve the leverage is never tested
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statutory_debt_ceiling__extraction_snare_reading, 0.81).
domain_priors:suppression_score(statutory_debt_ceiling__extraction_snare_reading, 0.72).
domain_priors:theater_ratio(statutory_debt_ceiling__extraction_snare_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statutory_debt_ceiling__extraction_snare_reading, snare).
narrative_ontology:human_readable(statutory_debt_ceiling__extraction_snare_reading, "Statutory Debt Ceiling as Hostage-Threat Extraction Mechanism").
narrative_ontology:topic_domain(statutory_debt_ceiling__extraction_snare_reading, "constitutional_law/political_economy/fiscal_governance").

domain_priors:requires_active_enforcement(statutory_debt_ceiling__extraction_snare_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statutory_debt_ceiling__extraction_snare_reading, '202d4e64-79dc-48ba-9de4-ab083079f763').
narrative_ontology:cs_kernel_codification('202d4e64-79dc-48ba-9de4-ab083079f763', formalized).
narrative_ontology:cs_authority_grounding('202d4e64-79dc-48ba-9de4-ab083079f763', extraction).
narrative_ontology:cs_interpretation_layer_present('202d4e64-79dc-48ba-9de4-ab083079f763').
narrative_ontology:cs_reading_relation('202d4e64-79dc-48ba-9de4-ab083079f763', statutory_debt_ceiling__coordination_scaffold_reading, coexists_with).
narrative_ontology:cs_reading_relation('202d4e64-79dc-48ba-9de4-ab083079f763', statutory_debt_ceiling__constitutional_nullity_reading, influences).
narrative_ontology:cs_axiom('202d4e64-79dc-48ba-9de4-ab083079f763', foundational, leverage_extraction_supersedes_coordination_function).
narrative_ontology:cs_axiom_status(leverage_extraction_supersedes_coordination_function, holdable).
narrative_ontology:cs_axiom_grounding('202d4e64-79dc-48ba-9de4-ab083079f763', leverage_extraction_supersedes_coordination_function, empirically_contingent).
narrative_ontology:cs_axiom('202d4e64-79dc-48ba-9de4-ab083079f763', secondary, default_threat_credibility_is_the_operative_mechanism).
narrative_ontology:cs_axiom_status(default_threat_credibility_is_the_operative_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('202d4e64-79dc-48ba-9de4-ab083079f763', default_threat_credibility_is_the_operative_mechanism, empirically_contingent).
narrative_ontology:cs_reference_frame('202d4e64-79dc-48ba-9de4-ab083079f763', wartime_borrowing_authorization_convenience).
narrative_ontology:cs_drift_state('202d4e64-79dc-48ba-9de4-ab083079f763', post_2011_brinkmanship_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('202d4e64-79dc-48ba-9de4-ab083079f763', '').
narrative_ontology:cs_kernel_id(statutory_debt_ceiling__extraction_snare_reading, statutory_debt_ceiling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__extraction_snare_reading, minority_faction_leadership).
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__extraction_snare_reading, brinkmanship_aligned_incumbents).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, federal_beneficiary_populations).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, treasury_bondholders).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, federal_civilian_workforce).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, state_and_local_governments).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, treasury_department).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, congressional_majority_coalition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A cohesive voting bloc, often not commanding a legislative majority on substantive policy but sufficient in a closely divided chamber to withhold the votes needed to raise or suspend the statutory ceiling. Uses the approaching default date as leverage to extract policy concessions (spending caps, program cuts, unrelated riders) that could not be won through ordinary majoritarian bargaining. Faces no personal exposure to default consequences and can walk away from talks without bearing the downstream costs.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, minority_faction_leadership, agenda_setter,
    organized, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(statutory_debt_ceiling__extraction_snare_reading, minority_faction_leadership, beneficiary).

% Must administer extraordinary measures, delay disbursements, and publicly warn of the X-date while having no independent statutory authority to exceed the ceiling. Bears the operational and reputational cost of the standoff, absorbs market volatility, and cannot exit the constraint since it is bound to execute congressional appropriations without congressional authorization to borrow to fund them.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, treasury_department, payer,
    institutional, immediate, trapped, global).
narrative_ontology:stakeholder_secondary_role(statutory_debt_ceiling__extraction_snare_reading, treasury_department, agenda_setter).

% Recipients of Social Security, veterans' benefits, Medicare, and other mandatory payments who face delayed or suspended disbursements if the ceiling binds and extraordinary measures are exhausted. Have no direct voice in the negotiation and no ability to hedge against a payment interruption they did not create.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, federal_beneficiary_populations, payer,
    powerless, immediate, trapped, national).

% Domestic and foreign holders of U.S. Treasury securities who bear technical default risk, credit-rating downgrade risk (as in 2011 and near-misses since), and yield volatility whenever the ceiling standoff approaches the X-date. Large institutional holders can partially hedge or diversify; most cannot fully exit exposure to the world's reserve-currency debt instrument without accepting other risks.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, treasury_bondholders, payer,
    organized, biographical, constrained, global).

% Federal employees and contractors whose pay processing and continuity of operations become contingent on resolution of the standoff. Can seek other employment over time but face immediate income disruption risk with each cycle and limited ability to plan around a recurring, deliberately-timed crisis.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, federal_civilian_workforce, payer,
    moderate, biographical, constrained, national).

% Recipients of federal transfers and grants-in-aid whose budgets are exposed to disruption if federal disbursements are delayed. Have no seat at the negotiating table despite bearing pass-through fiscal risk.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, state_and_local_governments, payer,
    moderate, biographical, constrained, national).

% The broader governing coalition that must ultimately supply votes to raise the ceiling and is politically blamed for the standoff regardless of which faction manufactured it. Negotiates under time pressure it did not choose and often accepts concessions to avoid a default it would be blamed for.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, congressional_majority_coalition, payer,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(statutory_debt_ceiling__extraction_snare_reading, congressional_majority_coalition, agenda_setter).

% Monitor the standoff and have downgraded U.S. sovereign credit ratings or issued negative outlooks when brinkmanship escalates, translating legislative dysfunction into a measurable market signal that itself raises borrowing costs for years afterward.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, credit_rating_agencies, observer,
    institutional, biographical, analytical, global).

% Argue, from outside the negotiating room, that the Fourteenth Amendment's public-debt clause and the structural logic of appropriations already made undermine the ceiling's legal force, but their view has never been tested in court or acted on by an administration, leaving the hostage dynamic intact.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, constitutional_scholars_and_treasury_counsel, excluded,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(statutory_debt_ceiling__extraction_snare_reading, minority_faction_leadership).
narrative_ontology:fixing_cost_class(statutory_debt_ceiling__extraction_snare_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In its declared form, the ceiling is supposed to let Congress set an aggregate borrowing limit rather than approve each individual bond issuance — a coordination shortcut for public borrowing. This reading holds that the coordination function is now vestigial: appropriations and revenue decisions already fix the borrowing need, so the ceiling vote adds no real fiscal control, only a second veto point.
% TRANSFER_FUNCTION: Moves negotiating leverage from the ordinary legislative process (where policy is decided by majority vote weighed against political cost) to whichever faction is willing to accept blame for approaching default. Concessions won under this leverage move policy content — spending caps, program cuts, unrelated riders — toward the threatening faction's preferences, while the tangible costs of the standoff (delayed benefits, downgrade-driven higher borrowing costs, workforce disruption) are distributed onto populations who have no part in the negotiation.
% ABSENT_VOICES: Federal beneficiaries, federal employees, and state/local governments bear the disruption risk but are not parties to the negotiation. Constitutional scholars and Treasury counsel who argue the ceiling has no binding force post-Fourteenth-Amendment are excluded from the practical negotiation entirely — no administration has tested their argument, so their objection never enters the room where the hostage dynamic is priced in.
% DISAPPEARANCE_RATIONALE: If the ceiling vanished overnight, the recurring standoff cycle would end: Treasury would issue debt to meet appropriated spending without a second congressional gate, minority factions would lose their principal leverage point for extracting concessions outside ordinary appropriations bargaining, and credit markets would likely re-price sovereign risk downward absent the periodic brinkmanship premium. Federal beneficiaries, employees, and bondholders would no longer face recurring disruption risk tied to the statute itself.
% FOUNDING_PROBLEM: The 1917 Second Liberty Bond Act ceiling was created to let Treasury issue war-financing debt without needing individual congressional authorization for each bond issuance — an administrative convenience during wartime borrowing.
% FOUNDING_PROBLEM_CORROBORATION: Independent economists, the Government Accountability Office, and Treasury officials across administrations of both parties have repeatedly stated in testimony that the ceiling performs no independent fiscal-restraint function once appropriations are enacted, since spending and revenue decisions already determine the borrowing need; this assessment comes from career civil servants and outside analysts rather than from the minority factions that currently benefit from the leverage the ceiling provides.
narrative_ontology:disappearance_verdict(statutory_debt_ceiling__extraction_snare_reading, world_rearranges).
narrative_ontology:founding_problem_status(statutory_debt_ceiling__extraction_snare_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statutory_debt_ceiling__extraction_snare_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(statutory_debt_ceiling__extraction_snare_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statutory_debt_ceiling__extraction_snare_reading, 0.81, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored high (0.81 at interval end) because the standoff mechanism transfers real policy concessions to a faction that bears none of the costs it threatens to impose — this is the defining snare signature: coordination story as cover, coercion as the actual persistence mechanism. Suppression (0.72) reflects that exits from the standoff are structurally blocked: Treasury cannot unilaterally resolve it, beneficiaries cannot opt out of dependence on federal disbursements, and the statute itself forecloses the ordinary alternative (issuing debt to meet appropriated spending) until the minority's terms are met. Theater ratio is moderate and rising (0.15 to 0.38) because a genuine administrative function (aggregate borrowing authorization) persists in form even as its substantive fiscal-restraint content has been supplanted by leverage extraction — the ceiling still gets voted on, but the vote's function has drifted from coordination to hostage-taking.
 *
 * DIRECTIONALITY LOGIC:
 *   Minority faction leadership sits at the full-beneficiary end: it manufactures the leverage, extracts concessions, and exits the episode without absorbing default consequences (arbitrage exit). Federal beneficiary populations, civilian workforce, and state/local governments sit at the full-target end: trapped or constrained exit, no negotiating voice, and direct exposure to payment disruption. Treasury sits as an institutional payer with no exit at all — it must execute the mechanism regardless of the political cost, which is a different structural position from a target with even constrained mobility. Treasury bondholders are organized but only constrained, not mobile, because sovereign debt exposure cannot be fully hedged away without accepting other risk.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (wartime administrative convenience for aggregate bond authorization) is dead by wide corroboration outside the beneficiary set (GAO, career Treasury officials, cross-partisan economists), yet the world clearly rearranges if the mechanism disappears (disappearance_verdict: world_rearranges) — this is not a contradiction. The mismatch the R5 interview is built to surface is exactly this one: founding_problem_status=dead paired with disappearance_verdict=world_rearranges signals that the arrangement has been repurposed rather than genuinely still needed. It persists because it was captured as a leverage mechanism after its original coordination rationale expired, which is the diagnostic signature of a snare wearing a coordination-scaffold's clothing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    which_debt_ceiling_reading_is_operative,
    'Is the statutory debt ceiling better read as a coordination scaffold that has degraded, a constitutionally void constraint whose apparent force is a legal illusion, or a fully weaponized extraction mechanism as authored here?',
    'A Supreme Court ruling on the Fourteenth Amendment public-debt clause argument would resolve the nullity question; a period of ceiling suspension without brinkmanship-driven concession extraction would support the scaffold reading; continued extraction of unrelated policy concessions under default threat, as observed in 2011, 2013, and 2023, supports this reading.',
    'If the nullity reading is judicially confirmed, this snare reading''s entire extraction mechanism would be legally dissolved rather than merely reformed. If the scaffold reading is correct, the extraction observed here would be better modeled as occasional abuse of a genuinely functional mechanism rather than as the mechanism''s dominant mode.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(which_debt_ceiling_reading_is_operative, conceptual, 'Which of the three kernel readings (scaffold, nullity, snare) best describes the ceiling''s actual operative status is unresolved and contested across constitutional and political-economy literature.').

omega_variable(
    minority_faction_identity_persistence,
    'Is the beneficiary of the extraction a persistent structural position (whichever faction holds the pivotal votes in a closely divided chamber) or a specific partisan faction whose current dominance of the tactic could shift?',
    'Track which party/faction initiates brinkmanship-linked ceiling standoffs across multiple congressional sessions and chamber compositions; if the tactic is used symmetrically by whichever faction holds pivotal leverage regardless of party, that supports the structural-position reading over a partisan-identity reading.',
    'If the extraction is tied to the structural position rather than a specific faction, then any electoral realignment gets a new group access to the same leverage, meaning the constraint''s beneficiary set is functionally defined by chamber arithmetic, not by ideology.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_faction_identity_persistence, empirical, 'Whether the beneficiary group is a fixed political identity or a rotating structural position occupied by whoever holds pivotal votes.').

omega_variable(
    credit_rating_damage_permanence,
    'Do credit rating downgrades and elevated risk premiums from ceiling brinkmanship persist structurally (raising the baseline cost of U.S. borrowing permanently) or are they transient, fully reversing once each standoff resolves?',
    'Time-series analysis of Treasury yield spreads and sovereign CDS pricing in the months and years following each major standoff (2011, 2013, 2023), controlling for macroeconomic conditions.',
    'If damage is structurally persistent, the diffuse cost borne by taxpayers generally (through permanently higher federal borrowing costs) is a significant and previously under-counted victim class beyond the named stakeholders here.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(credit_rating_damage_permanence, empirical, 'Whether credit-market damage from repeated brinkmanship is transient or cumulative and structural.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statutory_debt_ceiling__extraction_snare_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(stat_tr_t8, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement(stat_tr_t16, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 16, 0.27).
narrative_ontology:measurement(stat_tr_t24, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 24, 0.32).
narrative_ontology:measurement(stat_tr_t32, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 32, 0.35).
narrative_ontology:measurement(stat_tr_t40, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 40, 0.38).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(stat_be_t8, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(stat_be_t16, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 16, 0.6).
narrative_ontology:measurement(stat_be_t24, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 24, 0.7).
narrative_ontology:measurement(stat_be_t32, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 32, 0.77).
narrative_ontology:measurement(stat_be_t40, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 40, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(stat_su_t8, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 8, 0.5).
narrative_ontology:measurement(stat_su_t16, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 16, 0.58).
narrative_ontology:measurement(stat_su_t24, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 24, 0.65).
narrative_ontology:measurement(stat_su_t32, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 32, 0.7).
narrative_ontology:measurement(stat_su_t40, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statutory_debt_ceiling__extraction_snare_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(statutory_debt_ceiling__extraction_snare_reading, 0.05).
narrative_ontology:affects_constraint(statutory_debt_ceiling__extraction_snare_reading, statutory_debt_ceiling__coordination_scaffold_reading).
narrative_ontology:affects_constraint(statutory_debt_ceiling__extraction_snare_reading, statutory_debt_ceiling__constitutional_nullity_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the statutory_debt_ceiling kernel. The coordination_scaffold_reading authors low-to-moderate extraction and treats the periodic vote as a legitimate, if inefficient, aggregate-authorization mechanism. The constitutional_nullity_reading authors the ceiling as void or voidable under the Fourteenth Amendment's public-debt clause, in which case its apparent binding force — and thus all extraction modeled downstream of it, including in this file — is an artifact of unlitigated legal uncertainty rather than genuine constraint. This file (extraction_snare_reading) authors high extraction and treats the mechanism as captured leverage. All three share the same statutory text but diverge on kernel_codification treatment, authority_grounding, and the beneficiary/victim structure that follows from each reading's premises.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
