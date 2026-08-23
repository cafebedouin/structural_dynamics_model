% ============================================================================
% CONSTRAINT STORY: gold_fiat_transition_mechanism__automatic_constraint_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gold_fiat_transition_mechanism__automatic_constraint_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: gold_fiat_transition_mechanism__automatic_constraint_reading
 *   human_readable: Post-Gold Discretionary Monetary Authority (Automatic-Constraint Reading)
 *   domain: economic/political/history_of_economic_thought
 *
 * SUMMARY:
 *   Between 1944 and 1971 the rule tying money issue to metal redemption was
 *   dismantled in stages — wartime suspension made semi-permanent by the
 *   Bretton Woods official-only window, then closed outright in August 1971 —
 *   leaving money creation governed by institutional discretion instead of a
 *   mechanical reserve test. This story assesses the standing post-transition
 *   arrangement as this reading sees it: a constraint that changed type from
 *   automatic-material to discretionary-institutional, weaker than its
 *   predecessor, kept real by mandate law, market expectations, and the
 *   issuing institutions' own self-conception. Under the automatic-constraint
 *   reading, discretion itself is the prize: the issuing authority gained the
 *   option to create money without external limit, and the class holding
 *   fixed nominal claims lost the automatic cap on how far their claims'
 *   purchasing power could be stretched. DECOMPOSITION NOTE: 'the end of the
 *   gold standard' is a colloquial label covering structurally distinct
 *   claims. This file instantiates only the automatic_constraint_reading of
 *   the gold_fiat_transition_mechanism kernel; the sibling files
 *   (creditor_discipline_reading, composite_overdetermination_reading) author
 *   the creditor-veto and multi-causal-convergence claims respectively. Their
 *   epsilon values differ because their referent arrangements differ; this
 *   file links them via network.affects_constraints and does not average over
 *   them.
 *
 * KEY AGENTS:
 *   - monetary_authorities: primary beneficiary and agenda setter (institutional/arbitrage) — holds the discretion the transition created, collects seigniorage, sets the terms of its own review
 *   - commercial_banking_sector: secondary beneficiary (institutional/constrained) — rides an emergency backstop that judgment-based authority makes unlimited in principle
 *   - creditor_class: primary target (organized/constrained) — bears debasement risk on fixed nominal claims with no redemption right left
 *   - private_note_issuers: excluded competitor (moderate/trapped) — shut out by the very statutes the arrangement rests on
 *   - monetary_economists: analytical observer (analytical/analytical) — evaluates the record from outside the vote
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.72).
domain_priors:suppression_score(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.48).
domain_priors:theater_ratio(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gold_fiat_transition_mechanism__automatic_constraint_reading, tangled_rope).
narrative_ontology:human_readable(gold_fiat_transition_mechanism__automatic_constraint_reading, "Post-Gold Discretionary Monetary Authority (Automatic-Constraint Reading)").
narrative_ontology:topic_domain(gold_fiat_transition_mechanism__automatic_constraint_reading, "economic/political/history_of_economic_thought").

domain_priors:requires_active_enforcement(gold_fiat_transition_mechanism__automatic_constraint_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gold_fiat_transition_mechanism__automatic_constraint_reading, 'de2eea34-df3d-4ed0-b102-4cc66d0b485b').
narrative_ontology:cs_kernel_codification('de2eea34-df3d-4ed0-b102-4cc66d0b485b', formalized).
narrative_ontology:cs_authority_grounding('de2eea34-df3d-4ed0-b102-4cc66d0b485b', expertise).
narrative_ontology:cs_interpretation_layer_present('de2eea34-df3d-4ed0-b102-4cc66d0b485b').
narrative_ontology:cs_reading_relation('de2eea34-df3d-4ed0-b102-4cc66d0b485b', gold_fiat_transition_mechanism__creditor_discipline_reading, coexists_with).
narrative_ontology:cs_reading_relation('de2eea34-df3d-4ed0-b102-4cc66d0b485b', gold_fiat_transition_mechanism__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('de2eea34-df3d-4ed0-b102-4cc66d0b485b', foundational, money_creation_requires_automatic_material_limit).
narrative_ontology:cs_axiom_status(money_creation_requires_automatic_material_limit, holdable).
narrative_ontology:cs_axiom_grounding('de2eea34-df3d-4ed0-b102-4cc66d0b485b', money_creation_requires_automatic_material_limit, empirically_contingent).
narrative_ontology:cs_axiom('de2eea34-df3d-4ed0-b102-4cc66d0b485b', secondary, constraint_relaxation_transfers_margin_to_issuer).
narrative_ontology:cs_axiom_status(constraint_relaxation_transfers_margin_to_issuer, holdable).
narrative_ontology:cs_axiom_grounding('de2eea34-df3d-4ed0-b102-4cc66d0b485b', constraint_relaxation_transfers_margin_to_issuer, empirically_contingent).
narrative_ontology:cs_reference_frame('de2eea34-df3d-4ed0-b102-4cc66d0b485b', automatic_gold_conversion_discipline).
narrative_ontology:cs_drift_state('de2eea34-df3d-4ed0-b102-4cc66d0b485b', contemporary_fiat_era, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('de2eea34-df3d-4ed0-b102-4cc66d0b485b', '').
narrative_ontology:cs_kernel_id(gold_fiat_transition_mechanism__automatic_constraint_reading, gold_fiat_transition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__automatic_constraint_reading, monetary_authorities).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__automatic_constraint_reading, commercial_banking_sector).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__automatic_constraint_reading, creditor_class).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operates the sole legal issuer of base money under a statutory mandate; sets short-term rates and balance-sheet composition, lends in emergencies, and remits operating surplus to the treasury. Since the redemption window closed, nothing outside its own judgment and mandate caps the quantity it can create; it answers to oversight bodies it effectively educates, and its staff circulate between academia, treasury, and its own research arm.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, monetary_authorities, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(gold_fiat_transition_mechanism__automatic_constraint_reading, monetary_authorities, beneficiary).

% Creates deposit money on top of central bank reserves and relies on the issuer's emergency lending to survive runs. Judgment-based authority is the feature that makes the backstop unlimited in principle; the sector pays for it through reserve requirements, supervision, and resolution regimes, and its largest members sit close enough to the issuer that policy consultations routinely run through them.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, commercial_banking_sector, beneficiary,
    institutional, biographical, constrained, national).

% Holds long-dated nominal government and corporate bonds, pension annuities, and mortgage books whose payouts are fixed in currency units someone else decides. Before 1971 a gold clause or redemption right capped how far the payout's purchasing power could be stretched; since then the only defenses are repricing at rollover, inflation-indexed instruments, or shifting into real assets — all slower than the decisions that erode them. Exit abroad is possible but taxed, monitored, and still settles through the same banking rails.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, creditor_class, payer,
    organized, generational, constrained, global).

% Historically supplied competing redeemable notes and private mint services until monopoly statutes, legal tender acts, and tax treatment closed every circulating niche. Would re-enter with hard-currency or bearer-instrument products if admission were granted; currently confined to novelty commemorative issues and offshore niches.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, private_note_issuers, excluded,
    moderate, biographical, trapped, national).

% Study the regime from universities and think tanks, publish on its record, and staff a revolving door with the issuing institutions. Their assessments feed framework reviews but carry no vote; several hold tenured positions funded in whole or part by the institutions they evaluate.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, monetary_economists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gold_fiat_transition_mechanism__automatic_constraint_reading, monetary_authorities).
narrative_ontology:fixing_cost_class(gold_fiat_transition_mechanism__automatic_constraint_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the liquidity and stabilization problem that a fixed-metal rule could not: seasonal and crisis demands for money are met by expanding the issue on judgment, a standing emergency lender backstops solvent-but-illiquid banks, and the price level is steered toward a target rather than inherited from mine output.
% TRANSFER_FUNCTION: Moves purchasing power from everyone holding fixed nominal claims — bondholders, annuitants, depositors between adjustments — toward the issuing complex and the first receivers of new credit; in crises it moves private losses onto the public balance sheet financed by issue.
% ABSENT_VOICES: Competing note issuers barred by monopoly statutes, hard-money constituencies with no seat in framework reviews, and future cohorts who inherit the debased unit but cannot vote in today's decisions. They object from op-eds, occasional courtrooms, and the ballot box on long delay.
% DISAPPEARANCE_RATIONALE: If discretionary issuance authority vanished overnight with no successor rule, clearing would seize within days, states that roll debt through the banking system would face immediate funding failure, and the scramble would end only when a new anchor — revived metal parity, a currency board, or a rewritten mandate — was installed. Trade invoicing, pensions, and sovereign finance are all built on the current setup continuing.
% FOUNDING_PROBLEM: Recurring liquidity panics and deflationary contractions under metal-bound issue: the 1907 panic found no elastic lender, and the interwar metal regime forced deflation onto economies that needed the opposite.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: economic historians (Friedman and Schwartz's contraction account; Eichengreen and Bordo on the metal regime's deflationary mechanics) document the founding problem independently of the issuer, and depositors and firms demanding emergency lending in 2008 and 2020 attest the demand side is real rather than issuer-manufactured.
narrative_ontology:disappearance_verdict(gold_fiat_transition_mechanism__automatic_constraint_reading, world_rearranges).
narrative_ontology:founding_problem_status(gold_fiat_transition_mechanism__automatic_constraint_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gold_fiat_transition_mechanism__automatic_constraint_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gold_fiat_transition_mechanism__automatic_constraint_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gold_fiat_transition_mechanism__automatic_constraint_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gold_fiat_transition_mechanism__automatic_constraint_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gold_fiat_transition_mechanism__automatic_constraint_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored high (0.72) because the arrangement's margin — the gap between what the issuer can create and what any external rule would permit — accrued entirely to the issuing side once the redemption test was removed, and the incidence lands on claim-holders who priced their loans and annuities under the previous regime's assurances. Suppression is authored moderate (0.48): legal-tender status, tax denomination, and chartering law hold the domestic niche closed, while capital mobility leaves real exits open — suppression here is structural (statutes and rails), not internalized. Theater (0.38) tracks the growing share of institutional effort spent on communication and expectation management rather than operations, dipping in the early 1980s when policy briefly became substantive again; scalars are taken at the interval's calm-phase endpoint. Resistance (0.45) reflects persistent hard-money politics and the migration of savings behavior rather than open confrontation; accessibility_collapse (0.60) reflects a locked unit of account with costly-but-open real-asset exits. The three measurement series share one eight-point grid (1944-2024) so no metric borrows another's timeline; the suppression_requirement series traces the enforcement arc the story actually tracks (heavy Bretton Woods-era controls, the 1971 spike, liberalization, mild re-hardening). Receipt surface: gains demonstrably accrue at the issuing seat (seigniorage and the discretion premium), hence gain_flow names monetary_authorities; reinstating any external rule would carry prohibitive transition costs relative to the diffuse benefit of doing so, hence fixing_cost prohibitive. Claim and metrics are authored independently: the tangled_rope claim follows from the structure — one discretion serving both an elastic-stabilization job and a debasement option — not from tuning to predicted engine outputs.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently because the same discretion reads differently from each side. From the issuing chair the arrangement is a policy instrument and a crisis tool — the thing that stood between 2008 and a 1930s repeat; from the creditor seat it is a standing repudiation option attached to every nominal contract, priced only imperfectly at origination. The two institutional seats split further: the issuing authority holds arbitrage-grade freedom inside its own franchise, while banks hold the backstop but not the switch — same nominal power level, differentiated by who operates the instrument. The excluded issuer seat experiences pure exclusion — it stands on the enforcement surface itself. Identity-lock note: the issuing institutions' self-conception (guardian of the currency, technocratic steward) fuses their legitimacy to never plainly admitting the debasement option they hold; if that professional frame broke, acknowledgment of the drift would stop being selective and oversight pressure would land differently.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. monetary_authorities sits at the beneficiary pole (declared beneficiary, arbitrage-grade exit inside its own franchise, so effective extraction damps toward subsidy); commercial_banking_sector is a declared beneficiary with constrained exit, giving low positive d; creditor_class is the declared victim with organized power but constrained exit, pushing d toward the full-target end, amplified by the global scope of the claims it holds. Suppression is authored as a raw structural property and is deliberately NOT scaled — only extractiveness scales with directionality and scope. One directionality override is declared: the story's only moderate-power seat, private_note_issuers, would receive a canonical mid-scale d as an undeclared bystander, but structurally the arrangement's enforcement exists to keep them out — their relationship to the constraint is that of a target of its boundary maintenance, so their d is overridden to 0.85.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — panics and deflationary contractions that a fixed-metal rule could not meet — is still live and is attested from outside the beneficiary set, so no mandatrophy is declared and the piton signature is not in play. The classification work this story performs is boundary-keeping in both directions: a pure-coordination reading would erase the real transfer running from nominal claim-holders to the issuing complex, while a pure-extraction reading would erase the demonstrated stabilization the same discretion delivered in 2008 and 2020. The tangled_rope claim keeps both facts on the table and lets the engine's per-seat computation decide whether the payer seat's experience outranks the coordinator's.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'Which reading of the gold_fiat_transition_mechanism kernel correctly locates the transition''s structural center: the constraint-type swap (this file), the creditor-veto removal, or multi-causal convergence?',
    'Cross-reading corpus comparison: compile all three sibling stories, compare computed per-seat classifications and epsilon profiles against counterfactual historical tests (would the 1971 decision have mattered absent the telecom change? would creditor veto have bound without redemption rights?).',
    'If the creditor-discipline reading dominates, the victim seat shifts from fixed-claim holders toward sovereign debtors and the transfer reframes as geopolitical; if the overdetermination reading dominates, this single-node constraint decomposes into a linked family with no privileged causal seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Committer structure: which sibling reading owns the causal center of the transition.').

omega_variable(
    automaticity_of_prior_constraint,
    'Was the pre-1971 metal constraint genuinely automatic and material, or already an institutional convention actively maintained by policy choices (interwar sterilization, the 1933 gold-clause abrogation, the Bretton Woods official-only window)?',
    'Historiographic audit of the predecessor regime''s operation: catalog episodes where the automatic test was suspended, managed, or overridden by decision, and weigh how much binding force ever came from mechanism versus enforcement.',
    'If the prior constraint was substantially institutional, the material-to-institutional type-change premise of this reading weakens, the transition reads as continuity rather than swap, and epsilon attribution shifts toward the whole managed-money century rather than the post-1971 arrangement alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(automaticity_of_prior_constraint, conceptual, 'Load-bearing premise of this reading: the automaticity of the predecessor constraint.').

omega_variable(
    authority_grounding_framing,
    'Is the issuing authority''s legitimacy grounded in demonstrated competence (technocratic expertise adjudicating the mandate) or in the benefit it draws from preventing any return to a hard external rule?',
    'Test the two framings against the same record: if credibility survives episodes where competence failed but the franchise persisted, the extraction-grounding framing fits better; if legitimacy tracks forecast accuracy, the expertise framing holds.',
    'Under the expertise framing the arrangement certifies as a functioning commitment system with an interpretive buffer; under the extraction framing the same structure reads as authority maintained by drift denial, and the coupling profile flags accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_grounding_framing, conceptual, 'CS-framing under-determination: two coherent grounds for the same authority structure yield different commitment-system classifications.').

omega_variable(
    inflation_tax_incidence_seat,
    'Does the debasement margin concentrate on long-duration nominal claim-holders (the declared creditor seat) or diffuse across all money-holders including wage earners and small depositors?',
    'Distributional incidence studies of the inflation tax by balance-sheet composition: claim duration, indexing status, and refinancing speed across wealth deciles.',
    'If incidence is diffuse, the victim seat widens beyond creditor_class and per-seat extraction flattens toward symmetric; if concentrated as declared, the payer seat''s computed extraction stays high.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inflation_tax_incidence_seat, empirical, 'Who actually bears the debasement margin.').

omega_variable(
    rule_vs_discretion_net_welfare,
    'Does judgment-based issue deliver more than it takes — is the stabilization value of discretion greater than the debasement margin it opens?',
    'Welfare accounting across regime episodes: crisis outcomes under discretion (2008, 2020) versus deflationary episodes under binding metal rules (1870s, 1930s), weighted by who bore each cost.',
    'A strongly favorable answer supports treating the coordination function as dominant; an unfavorable answer pushes the payer seat toward pure-snare experience. The reading itself stays agnostic — this is the corpus question, not a settled fact.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(rule_vs_discretion_net_welfare, preference, 'Net-welfare contest underlying the reading''s high-epsilon attribution.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gold_fiat_transition_mechanism__automatic_constraint_reading, 1944, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gold_tr_t1944, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 1944, 0.14).
narrative_ontology:measurement_basis(gold_tr_t1944, observed).
narrative_ontology:measurement(gold_tr_t1958, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 1958, 0.17).
narrative_ontology:measurement_basis(gold_tr_t1958, observed).
narrative_ontology:measurement(gold_tr_t1971, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 1971, 0.26).
narrative_ontology:measurement_basis(gold_tr_t1971, observed).
narrative_ontology:measurement(gold_tr_t1982, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 1982, 0.21).
narrative_ontology:measurement_basis(gold_tr_t1982, observed).
narrative_ontology:measurement(gold_tr_t1997, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 1997, 0.29).
narrative_ontology:measurement_basis(gold_tr_t1997, observed).
narrative_ontology:measurement(gold_tr_t2008, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 2008, 0.33).
narrative_ontology:measurement_basis(gold_tr_t2008, observed).
narrative_ontology:measurement(gold_tr_t2020, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 2020, 0.36).
narrative_ontology:measurement_basis(gold_tr_t2020, observed).
narrative_ontology:measurement(gold_tr_t2024, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 2024, 0.38).
narrative_ontology:measurement_basis(gold_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(gold_be_t1944, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 1944, 0.34).
narrative_ontology:measurement_basis(gold_be_t1944, observed).
narrative_ontology:measurement(gold_be_t1958, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 1958, 0.38).
narrative_ontology:measurement_basis(gold_be_t1958, observed).
narrative_ontology:measurement(gold_be_t1971, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 1971, 0.52).
narrative_ontology:measurement_basis(gold_be_t1971, observed).
narrative_ontology:measurement(gold_be_t1982, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 1982, 0.55).
narrative_ontology:measurement_basis(gold_be_t1982, observed).
narrative_ontology:measurement(gold_be_t1997, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 1997, 0.58).
narrative_ontology:measurement_basis(gold_be_t1997, observed).
narrative_ontology:measurement(gold_be_t2008, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 2008, 0.66).
narrative_ontology:measurement_basis(gold_be_t2008, observed).
narrative_ontology:measurement(gold_be_t2020, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 2020, 0.7).
narrative_ontology:measurement_basis(gold_be_t2020, observed).
narrative_ontology:measurement(gold_be_t2024, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 2024, 0.72).
narrative_ontology:measurement_basis(gold_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(gold_su_t1944, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 1944, 0.55).
narrative_ontology:measurement_basis(gold_su_t1944, observed).
narrative_ontology:measurement(gold_su_t1958, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 1958, 0.5).
narrative_ontology:measurement_basis(gold_su_t1958, observed).
narrative_ontology:measurement(gold_su_t1971, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 1971, 0.61).
narrative_ontology:measurement_basis(gold_su_t1971, observed).
narrative_ontology:measurement(gold_su_t1982, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 1982, 0.47).
narrative_ontology:measurement_basis(gold_su_t1982, observed).
narrative_ontology:measurement(gold_su_t1997, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 1997, 0.35).
narrative_ontology:measurement_basis(gold_su_t1997, observed).
narrative_ontology:measurement(gold_su_t2008, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 2008, 0.41).
narrative_ontology:measurement_basis(gold_su_t2008, observed).
narrative_ontology:measurement(gold_su_t2020, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 2020, 0.44).
narrative_ontology:measurement_basis(gold_su_t2020, observed).
narrative_ontology:measurement(gold_su_t2024, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 2024, 0.48).
narrative_ontology:measurement_basis(gold_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gold_fiat_transition_mechanism__automatic_constraint_reading, resource_allocation).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__automatic_constraint_reading, gold_fiat_transition_mechanism__creditor_discipline_reading).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__automatic_constraint_reading, gold_fiat_transition_mechanism__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% Constraint family: 'the end of the gold standard' decomposes into three structurally distinct constraint stories under the epsilon-invariance principle. This file authors the automatic_constraint_reading (type-swap: automatic-material limit replaced by discretionary-institutional authority; issuer gains the margin, fixed-claim holders lose the automatic cap). The creditor_discipline_reading authors the removal of creditor veto and balance-of-payments discipline as a geopolitical transfer; the composite_overdetermination_reading authors the transition as convergence of independent structural changes with no single causal node. Upstream/downstream: the automatic-constraint claim is the most established of the three (the repeal is a matter of record) and is cited as evidence by the sibling claims; each file links the others via affects_constraints. No averaging: each file's epsilon refers to its own referent arrangement only.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gold_fiat_transition_mechanism__automatic_constraint_reading, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
