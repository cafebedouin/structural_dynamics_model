% ============================================================================
% CONSTRAINT STORY: issuance_as_deliberative_judgment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_issuance_as_deliberative_judgment, []).

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
 *   constraint_id: issuance_as_deliberative_judgment
 *   human_readable: Monetary Organ Deliberative Issuance Authority
 *   domain: constitutional_political_economy/monetary_theory/corporate_property_law
 *
 * SUMMARY:
 *   This story instantiates one reading of the kernel 'future claims present
 *   resources' — the question of what legitimately entitles a proposed future
 *   economic state to command real purchasing power today. In this reading,
 *   legitimacy is procedural and institutional: a recursively-composed
 *   political body (the Monetary Organ) must deliberately vote to authorize
 *   new money, disclosing members' interests (§15) as the load-bearing
 *   safeguard. Money quantity is treated as a judgment call constitutionally
 *   governed, not an automatic formula, a physical anchor, or an emergent
 *   market/credit process. The sibling readings — endogenous credit
 *   multiplication, physical backing, and market-discovered confidence — are
 *   separate constraints with their own ε values; they are not blended into
 *   this one. This reading's central observable is 'who voted and what was
 *   their interest,' which is precisely the axis the other readings do not
 *   treat as primary.
 *
 * KEY AGENTS:
 *   - monetary_organ_delegates: agenda_setter — votes to authorize new money quantity, discloses interests under §15
 *   - government_fiscal_authority: beneficiary — depends on organ votes to finance deficits
 *   - politically_connected_credit_recipients: beneficiary — captures first-mover advantage of new issuance
 *   - savers_holding_currency: payer — bears diffuse dilution cost, no organ seat
 *   - unrepresented_future_generations: payer — inherits compounding structural bias with no retroactive correction
 *   - excluded_regions_without_organ_seats: excluded — affected but unrepresented in the deliberative body
 *   - constitutional_law_scholars: observer — assesses whether disclosure legitimacy actually resolves the distributional question
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(issuance_as_deliberative_judgment, 0.42).
domain_priors:suppression_score(issuance_as_deliberative_judgment, 0.55).
domain_priors:theater_ratio(issuance_as_deliberative_judgment, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(issuance_as_deliberative_judgment, extractiveness, 0.42).
narrative_ontology:constraint_metric(issuance_as_deliberative_judgment, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(issuance_as_deliberative_judgment, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(issuance_as_deliberative_judgment, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(issuance_as_deliberative_judgment, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(issuance_as_deliberative_judgment, tangled_rope).
narrative_ontology:human_readable(issuance_as_deliberative_judgment, "Monetary Organ Deliberative Issuance Authority").
narrative_ontology:topic_domain(issuance_as_deliberative_judgment, "constitutional_political_economy/monetary_theory/corporate_property_law").

domain_priors:requires_active_enforcement(issuance_as_deliberative_judgment).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(issuance_as_deliberative_judgment, '27f8766d-81a7-4df0-9910-c840d1ef207e').
narrative_ontology:cs_kernel_codification('27f8766d-81a7-4df0-9910-c840d1ef207e', formalized).
narrative_ontology:cs_authority_grounding('27f8766d-81a7-4df0-9910-c840d1ef207e', lineage).
narrative_ontology:cs_interpretation_layer_present('27f8766d-81a7-4df0-9910-c840d1ef207e').
narrative_ontology:cs_reading_relation('27f8766d-81a7-4df0-9910-c840d1ef207e', issuance_as_deliberative_judgment__issuance_as_endogenous_credit_multiplication, influences).
narrative_ontology:cs_reading_relation('27f8766d-81a7-4df0-9910-c840d1ef207e', issuance_as_deliberative_judgment__issuance_as_physical_backing, forecloses).
narrative_ontology:cs_reading_relation('27f8766d-81a7-4df0-9910-c840d1ef207e', issuance_as_deliberative_judgment__issuance_as_market_discovered_confidence, coexists_with).
narrative_ontology:cs_axiom('27f8766d-81a7-4df0-9910-c840d1ef207e', foundational, legitimacy_from_deciding_body_constitution).
narrative_ontology:cs_axiom_status(legitimacy_from_deciding_body_constitution, holdable).
narrative_ontology:cs_axiom_grounding('27f8766d-81a7-4df0-9910-c840d1ef207e', legitimacy_from_deciding_body_constitution, conventional).
narrative_ontology:cs_axiom('27f8766d-81a7-4df0-9910-c840d1ef207e', foundational, quantity_as_governed_judgment_not_formula).
narrative_ontology:cs_axiom_status(quantity_as_governed_judgment_not_formula, holdable).
narrative_ontology:cs_axiom_grounding('27f8766d-81a7-4df0-9910-c840d1ef207e', quantity_as_governed_judgment_not_formula, conventional).
narrative_ontology:cs_axiom('27f8766d-81a7-4df0-9910-c840d1ef207e', secondary, interest_disclosure_sufficient_safeguard).
narrative_ontology:cs_axiom_status(interest_disclosure_sufficient_safeguard, holdable).
narrative_ontology:cs_axiom_grounding('27f8766d-81a7-4df0-9910-c840d1ef207e', interest_disclosure_sufficient_safeguard, instrumental).
narrative_ontology:cs_reference_frame('27f8766d-81a7-4df0-9910-c840d1ef207e', constituted_deliberative_supremacy).
narrative_ontology:cs_drift_state('27f8766d-81a7-4df0-9910-c840d1ef207e', contemporary_disclosure_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('27f8766d-81a7-4df0-9910-c840d1ef207e', '').
narrative_ontology:cs_kernel_id(issuance_as_deliberative_judgment, future_claims_present_resources).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(issuance_as_deliberative_judgment, monetary_organ_delegates).
narrative_ontology:constraint_beneficiary(issuance_as_deliberative_judgment, government_fiscal_authority).
narrative_ontology:constraint_beneficiary(issuance_as_deliberative_judgment, politically_connected_credit_recipients).
narrative_ontology:constraint_victim(issuance_as_deliberative_judgment, savers_holding_currency).
narrative_ontology:constraint_victim(issuance_as_deliberative_judgment, unrepresented_future_generations).
narrative_ontology:constraint_victim(issuance_as_deliberative_judgment, excluded_regions_without_organ_seats).
narrative_ontology:constraint_vindicates(issuance_as_deliberative_judgment, monetary_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(issuance_as_deliberative_judgment, constitutional_supervision_of_money).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sit on the recursively-composed body that votes to authorize new money quantity. Their vote is the sole legitimating act — no external anchor (gold, formula, market rate) governs the decision. They set the agenda for what counts as a legitimate monetary quantity and disclose (per §15) their interests in any given vote, but the disclosure regime is the only check on their own judgment.
narrative_ontology:constraint_stakeholder(issuance_as_deliberative_judgment, monetary_organ_delegates, agenda_setter,
    institutional, generational, analytical, national).

% Depends on organ votes to authorize the money that finances deficits or targeted credit programs. Benefits when the organ's composition tilts toward its fiscal priorities; has strong incentive to influence organ appointments without appearing to compromise the organ's constitutional independence.
narrative_ontology:constraint_stakeholder(issuance_as_deliberative_judgment, government_fiscal_authority, beneficiary,
    powerful, biographical, constrained, national).

% Sectors and firms whose credit access moves in step with organ decisions they can lobby toward. They receive newly authorized purchasing power ahead of the general public repricing it, capturing the first-mover advantage of new money entering the economy through their channel.
narrative_ontology:constraint_stakeholder(issuance_as_deliberative_judgment, politically_connected_credit_recipients, beneficiary,
    organized, biographical, mobile, national).

% Hold currency and fixed claims whose purchasing power is diluted whenever the organ votes to expand the money quantity. Have no seat in the deliberative body and no exit from the national currency without cross-border capital mobility they typically lack. Bear the diffuse, delayed cost of every issuance vote.
narrative_ontology:constraint_stakeholder(issuance_as_deliberative_judgment, savers_holding_currency, payer,
    powerless, biographical, trapped, national).

% Inherit whatever monetary quantity and interest-disclosure norms the organ established, without having voted or been consulted. Bear compounding effects of any structural bias in the organ's historical composition — no mechanism retroactively corrects for votes made before they existed.
narrative_ontology:constraint_stakeholder(issuance_as_deliberative_judgment, unrepresented_future_generations, payer,
    powerless, civilizational, trapped, national).

% Regions or constituencies whose economic conditions are affected by national issuance decisions but who hold no seat on the recursively-composed organ. Would object that the organ's 'visibility and constitution' legitimacy standard describes only the organ's internal composition, not whether all affected parties are represented within it.
narrative_ontology:constraint_stakeholder(issuance_as_deliberative_judgment, excluded_regions_without_organ_seats, excluded,
    moderate, generational, constrained, regional).

% Study whether the organ's recursive composition and §15 disclosure regime actually deliver the legitimacy the framework claims, or merely relocate the extraction question from 'is the money quantity correct' to 'was the vote properly disclosed' without resolving who bears the cost of the judgment.
narrative_ontology:constraint_stakeholder(issuance_as_deliberative_judgment, constitutional_law_scholars, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism for society to govern monetary quantity as a collective judgment rather than leaving it to either an arbitrary rule (which cannot adapt to circumstance) or unaccountable private credit creation (which has no visible decision point at all). The organ makes 'who decided and why' a knowable, contestable fact.
% TRANSFER_FUNCTION: Moves purchasing power from anyone holding fixed nominal claims (currency, savings, unindexed wages, fixed-income contracts) to whoever receives newly authorized money first or whose debts are denominated in the diluting currency — typically the fiscal authority and politically connected credit recipients who can secure organ attention.
% ABSENT_VOICES: Savers, future generations, and regions without organ seats would object that 'the organ voted and disclosed its interests' does not answer whether the vote's distributional consequences were justified — they are not part of the deliberation whose visibility is supposed to confer legitimacy.
% DISAPPEARANCE_RATIONALE: If the deliberative-organ framework disappeared, money quantity would have to be governed by some other mechanism (rule-bound formula, market-discovered rate, physical backing, or unconstrained bank credit expansion) — the fiscal authority would lose its current channel for financing deficits through organ votes, and politically connected recipients would lose their preferential first-access position. The organ's disappearance would force an immediate re-founding of monetary governance, not a return to any baseline.
% FOUNDING_PROBLEM: Historical experience with both rigid metallic/formula standards (which produced deflationary crises the political system could not correct) and unaccountable private credit expansion (which produced booms and busts with no visible decision point) created demand for a governed, visible, judgment-based alternative — quantity as an act of constituted political will rather than a mechanical output.
% FOUNDING_PROBLEM_CORROBORATION: The organ and fiscal authority attest the founding problem remains live — adaptive monetary governance is still needed and only a deliberative body can supply it. Independent monetary historians and constitutional scholars outside the organ's own membership corroborate that rigid rules and unaccountable credit expansion were genuine historical failure modes, but dispute whether the current organ's composition actually solves the representation problem or has become a mechanism that reliably favors whoever can secure a seat or lobby one.
narrative_ontology:disappearance_verdict(issuance_as_deliberative_judgment, world_rearranges).
narrative_ontology:founding_problem_status(issuance_as_deliberative_judgment, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(issuance_as_deliberative_judgment, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-08',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(issuance_as_deliberative_judgment, 'none', 1).
narrative_ontology:epsilon_provenance(issuance_as_deliberative_judgment, 0.42, 'claude-sonnet-5', 'c2_monetary_architecture_2026_20260808_170220', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(issuance_as_deliberative_judgment_tests).
:- end_tests(issuance_as_deliberative_judgment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 at interval end) because the coordination function is genuine — someone must decide monetary quantity, and a visible, disclosed vote is a real improvement over either rigid rules that cannot adapt or invisible private credit creation. But extraction rises over the interval as the organ's disclosure regime becomes a compliance ritual (theater_ratio climbing from 0.15 to 0.28) rather than a substantive check on distributional consequence — disclosing an interest is not the same as neutralizing it. Suppression (0.55) reflects that the organ's composition and voting procedure are constitutionally insulated from direct challenge by those who bear the diluting cost; savers and future generations cannot contest a vote after the fact.
 *
 * PERSPECTIVAL GAP:
 *   From the organ's own seat, each vote is a legitimate exercise of constituted judgment, properly disclosed, properly deliberated — the coordination story is sincere and structurally real. From the seat of savers holding currency or excluded regions, the same vote is an act that moves purchasing power toward whoever has organ access, dressed in the legitimating language of visibility and disclosure. The engine should register this seat divergence: the agenda_setter seat computes near coordination, the payer seats compute nearer extraction, because directionality differs even though the constitutional procedure is identical for both.
 *
 * DIRECTIONALITY LOGIC:
 *   Monetary_organ_delegates and government_fiscal_authority sit near the beneficiary end: they set or directly draw on the issuance decision. Politically_connected_credit_recipients also skew toward beneficiary via preferential first access to new money. Savers_holding_currency and unrepresented_future_generations sit near the full-target end: trapped exit, no organ voice, and they absorb the dilution cost through no channel they can contest. Excluded_regions_without_organ_seats sit closer to target than beneficiary despite moderate power, because their exclusion from the deliberative body is the structural fact the constraint's legitimacy claim rests on without addressing.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — governing money quantity through visible judgment rather than rigid rule or invisible credit expansion — remains partially live: some form of monetary governance is always needed. But the specific claim that visibility-and-disclosure (§15) fully discharges legitimacy is contested: the disclosure regime answers 'who decided and did they admit their interest' without answering 'was the decision's distributional burden justified.' This is not pure mandatrophy (the coordination function has not fully died) but the classification as tangled_rope rather than rope reflects that the enforcement of organ authority persists regardless of whether the legitimacy story it tells about itself remains adequate to the distributional facts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    disclosure_versus_neutralization,
    'Does §15 interest disclosure by organ delegates actually neutralize the distributional bias of issuance votes, or does it merely make the bias visible and legally defensible without correcting it?',
    'Track organ votes where a delegate disclosed a conflicting interest against votes without disclosed conflicts; compare the distributional outcomes (who received newly authorized money first, at what cost to savers) across the two groups over multiple issuance cycles.',
    'If disclosure correlates with no reduction in distributional bias, the tangled_rope classification is strongly supported — the safeguard is theatrical rather than substantive. If disclosure correlates with materially reduced bias (delegates recuse or abstain), the constraint moves closer to a genuine rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disclosure_versus_neutralization, empirical, 'Whether §15 disclosure functions as substantive check or procedural theater.').

omega_variable(
    recursive_composition_representativeness,
    'Does the organ''s ''recursive composition'' (however delegates are selected/re-selected) produce a body that is representative of all parties affected by monetary quantity decisions, or does the recursion systematically favor incumbents and politically connected sectors over time?',
    'Longitudinal study of organ membership turnover and selection mechanisms; compare composition drift against demographic and sectoral distribution of the population bearing dilution costs.',
    'If recursion entrenches a narrow selecting class, the legitimacy claim (''legitimacy derives from the visibility and constitution of the deciding body'') is undermined by the composition itself being extractive, which would push the classification toward snare over time even without further changes to voting procedure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(recursive_composition_representativeness, conceptual, 'Whether the organ''s self-perpetuating composition mechanism is itself a source of bias.').

omega_variable(
    kernel_reading_boundary_stability,
    'Is ''issuance as deliberative judgment'' a stable, self-contained reading, or does it inevitably shade into ''issuance as endogenous credit multiplication'' once commercial banks are permitted to lend against organ-authorized reserves — i.e., does the organ''s vote actually control final money quantity, or only a base layer that private credit then multiplies?',
    'Trace the empirical relationship between organ-authorized base money changes and total money supply (including bank-created deposits) across the interval; a tight relationship supports this reading''s premise that the organ controls quantity; a loose one suggests the endogenous-credit sibling reading is doing more real work than this reading''s framing admits.',
    'If the relationship is loose, this story''s central claim — that the organ''s vote is the legitimating act for quantity — describes only part of the actual money-creation process, and the sibling reading (issuance_as_endogenous_credit_multiplication) would need to be understood as governing the larger share of actual monetary expansion, with correspondingly higher real-world ε than this reading alone suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary_stability, conceptual, 'Whether this reading''s premise (organ vote determines quantity) holds once bank credit multiplication is considered, or whether the sibling reading dominates in practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(issuance_as_deliberative_judgment, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(issu_tr_t0, issuance_as_deliberative_judgment, theater_ratio, 0, 0.15).
narrative_ontology:measurement(issu_tr_t8, issuance_as_deliberative_judgment, theater_ratio, 8, 0.18).
narrative_ontology:measurement(issu_tr_t16, issuance_as_deliberative_judgment, theater_ratio, 16, 0.21).
narrative_ontology:measurement(issu_tr_t24, issuance_as_deliberative_judgment, theater_ratio, 24, 0.24).
narrative_ontology:measurement(issu_tr_t32, issuance_as_deliberative_judgment, theater_ratio, 32, 0.26).
narrative_ontology:measurement(issu_tr_t40, issuance_as_deliberative_judgment, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(issu_be_t0, issuance_as_deliberative_judgment, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(issu_be_t8, issuance_as_deliberative_judgment, base_extractiveness, 8, 0.33).
narrative_ontology:measurement(issu_be_t16, issuance_as_deliberative_judgment, base_extractiveness, 16, 0.36).
narrative_ontology:measurement(issu_be_t24, issuance_as_deliberative_judgment, base_extractiveness, 24, 0.39).
narrative_ontology:measurement(issu_be_t32, issuance_as_deliberative_judgment, base_extractiveness, 32, 0.4).
narrative_ontology:measurement(issu_be_t40, issuance_as_deliberative_judgment, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(issu_su_t0, issuance_as_deliberative_judgment, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(issu_su_t8, issuance_as_deliberative_judgment, suppression_requirement, 8, 0.45).
narrative_ontology:measurement(issu_su_t16, issuance_as_deliberative_judgment, suppression_requirement, 16, 0.48).
narrative_ontology:measurement(issu_su_t24, issuance_as_deliberative_judgment, suppression_requirement, 24, 0.51).
narrative_ontology:measurement(issu_su_t32, issuance_as_deliberative_judgment, suppression_requirement, 32, 0.53).
narrative_ontology:measurement(issu_su_t40, issuance_as_deliberative_judgment, suppression_requirement, 40, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(issuance_as_deliberative_judgment, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(issuance_as_deliberative_judgment, 0.12).
narrative_ontology:affects_constraint(issuance_as_deliberative_judgment, issuance_as_endogenous_credit_multiplication).
narrative_ontology:affects_constraint(issuance_as_deliberative_judgment, issuance_as_physical_backing).
narrative_ontology:affects_constraint(issuance_as_deliberative_judgment, issuance_as_market_discovered_confidence).

% DUAL FORMULATION NOTE:
% This story is one of four constraints in the future_claims_present_resources kernel family, each instantiating a structurally distinct claim about what legitimates a proposed future's claim on present purchasing power: deliberative judgment (this story), endogenous credit multiplication (commercial bank lending), physical backing (redeemability against a real anchor), and market-discovered confidence (price-discovered interest rates). Each has its own ε, beneficiary/victim structure, and classification. This story's ε (0.42, moderate, rising) differs sharply from what an endogenous-credit reading would author (likely higher, given the diffuseness of bank-level extraction) and from a physical-backing reading (likely lower ε but higher suppression, given the rigidity cost). Do not average across readings; each is a separate file linked here.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
