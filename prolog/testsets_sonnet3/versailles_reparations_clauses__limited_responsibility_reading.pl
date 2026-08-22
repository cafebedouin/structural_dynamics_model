% ============================================================================
% CONSTRAINT STORY: versailles_reparations_clauses__limited_responsibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_versailles_reparations_clauses__limited_responsibility_reading, []).

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
 *   constraint_id: versailles_reparations_clauses__limited_responsibility_reading
 *   human_readable: Versailles Reparations Regime — Capacity-Bounded (Limited Responsibility) Reading
 *   domain: international_relations/legal_history/political_economy
 *
 * SUMMARY:
 *   This story instantiates the capacity-bounded ('limited responsibility')
 *   reading of the Versailles reparations kernel: German liability under
 *   Article 231 is a legal-formal predicate for reparations, not a moral
 *   condemnation, and the actual payment schedule must be continuously
 *   revised to match demonstrated German economic capacity. Under this
 *   reading the 1921 London Schedule, the 1924 Dawes Plan, and the 1929 Young
 *   Plan are successive corrections toward economic reality, and German
 *   negotiators' resistance to the original figures is read as prudent fiscal
 *   management rather than evasion. This is NOT the same constraint as the
 *   punitive_liability_reading (which reads Article 231 as grounding
 *   near-unlimited moral-financial liability) or the repudiation_reading
 *   (which denies any binding obligation at all) — those are separate
 *   constraint files with their own epsilon values, sharing only the treaty
 *   text and the historical apparatus that adjudicated between the readings.
 *
 * KEY AGENTS:
 *   - german_treasury_negotiators: agenda_setter/beneficiary (institutional/constrained) — administers payment mechanism, shapes revision negotiations
 *   - german_industrial_elites: beneficiary (powerful/mobile) — insulated from tax burden, benefits from reduced schedules
 *   - french_reconstruction_claimants: payer (powerful/trapped) — promised reconstruction financing repeatedly reduced
 *   - belgian_reconstruction_claimants: payer (moderate/trapped) — smaller leverage, absorbs revisions
 *   - allied_war_bondholders: payer (moderate/constrained) — reduced reparations flow shifts burden to their own taxpayers
 *   - reparations_commission: observer (institutional/analytical) — adjudicates capacity claims
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(versailles_reparations_clauses__limited_responsibility_reading, 0.52).
domain_priors:suppression_score(versailles_reparations_clauses__limited_responsibility_reading, 0.61).
domain_priors:theater_ratio(versailles_reparations_clauses__limited_responsibility_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(versailles_reparations_clauses__limited_responsibility_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(versailles_reparations_clauses__limited_responsibility_reading, tangled_rope).
narrative_ontology:human_readable(versailles_reparations_clauses__limited_responsibility_reading, "Versailles Reparations Regime — Capacity-Bounded (Limited Responsibility) Reading").
narrative_ontology:topic_domain(versailles_reparations_clauses__limited_responsibility_reading, "international_relations/legal_history/political_economy").

domain_priors:requires_active_enforcement(versailles_reparations_clauses__limited_responsibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(versailles_reparations_clauses__limited_responsibility_reading, 'b04f9c4e-9a74-4167-8449-5efb8e24ecaf').
narrative_ontology:cs_kernel_codification('b04f9c4e-9a74-4167-8449-5efb8e24ecaf', fixed_text).
narrative_ontology:cs_authority_grounding('b04f9c4e-9a74-4167-8449-5efb8e24ecaf', lineage).
narrative_ontology:cs_interpretation_layer_present('b04f9c4e-9a74-4167-8449-5efb8e24ecaf').
narrative_ontology:cs_reading_relation('b04f9c4e-9a74-4167-8449-5efb8e24ecaf', versailles_reparations_clauses__punitive_liability_reading, coexists_with).
narrative_ontology:cs_reading_relation('b04f9c4e-9a74-4167-8449-5efb8e24ecaf', versailles_reparations_clauses__repudiation_reading, influences).
narrative_ontology:cs_axiom('b04f9c4e-9a74-4167-8449-5efb8e24ecaf', foundational, capacity_to_pay_bounds_obligation).
narrative_ontology:cs_axiom_status(capacity_to_pay_bounds_obligation, holdable).
narrative_ontology:cs_axiom_grounding('b04f9c4e-9a74-4167-8449-5efb8e24ecaf', capacity_to_pay_bounds_obligation, empirically_contingent).
narrative_ontology:cs_axiom('b04f9c4e-9a74-4167-8449-5efb8e24ecaf', foundational, article_231_is_legal_predicate_not_moral_verdict).
narrative_ontology:cs_axiom_status(article_231_is_legal_predicate_not_moral_verdict, holdable).
narrative_ontology:cs_axiom_grounding('b04f9c4e-9a74-4167-8449-5efb8e24ecaf', article_231_is_legal_predicate_not_moral_verdict, conventional).
narrative_ontology:cs_reference_frame('b04f9c4e-9a74-4167-8449-5efb8e24ecaf', treaty_text_as_negotiated_settlement).
narrative_ontology:cs_drift_state('b04f9c4e-9a74-4167-8449-5efb8e24ecaf', post_dawes_stabilization_1924, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('b04f9c4e-9a74-4167-8449-5efb8e24ecaf', '').
narrative_ontology:cs_kernel_id(versailles_reparations_clauses__limited_responsibility_reading, versailles_reparations_clauses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__limited_responsibility_reading, german_industrial_elites).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__limited_responsibility_reading, german_treasury_negotiators).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__limited_responsibility_reading, weimar_fiscal_administration).
narrative_ontology:constraint_victim(versailles_reparations_clauses__limited_responsibility_reading, french_reconstruction_claimants).
narrative_ontology:constraint_victim(versailles_reparations_clauses__limited_responsibility_reading, belgian_reconstruction_claimants).
narrative_ontology:constraint_victim(versailles_reparations_clauses__limited_responsibility_reading, allied_war_bondholders).
narrative_ontology:constraint_vindicates(versailles_reparations_clauses__limited_responsibility_reading, capacity_to_pay_doctrine).
narrative_ontology:constraint_vindicates(versailles_reparations_clauses__limited_responsibility_reading, article_231_as_legal_technicality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Argue before the Reparations Commission and at successive conferences (London 1921, Dawes 1924, Young 1929) that the schedule must be revised downward to match German fiscal capacity, using currency collapse and budget deficits as evidence. They administer the actual transfer mechanism domestically and shape which taxes and exports are directed toward payment, giving them leverage to slow-walk enforcement while presenting each renegotiation as forced by economic reality rather than chosen.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, german_treasury_negotiators, agenda_setter,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(versailles_reparations_clauses__limited_responsibility_reading, german_treasury_negotiators, beneficiary).

% Benefit directly from downward-revised schedules and from the framing that ties payments to macroeconomic viability rather than moral liability — this keeps industrial capacity, credit access, and export markets from being encumbered by reparations-in-kind. Some hold assets and capital that can move across borders, insulating them from the domestic tax burden the treasury uses as evidence of incapacity.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, german_industrial_elites, beneficiary,
    powerful, generational, mobile, continental).

% Uses the capacity-to-pay framing to justify hyperinflation-era non-payment and to obtain successive moratoria; the reading gives this administration a legitimate vocabulary (viability, not guilt) for resisting Allied collection demands, though it remains bound by occupation threats (Ruhr 1923) if it pushes too far.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, weimar_fiscal_administration, beneficiary,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(versailles_reparations_clauses__limited_responsibility_reading, weimar_fiscal_administration, agenda_setter).

% Northern France's devastated departments were promised reconstruction financed by German payments. Under the capacity-bounded reading, the schedule they were promised is repeatedly revised downward; they cannot compel higher payment without risking German fiscal collapse or renewed occupation crisis, and cannot exit the arrangement since the damage and the claim both remain physically theirs.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, french_reconstruction_claimants, payer,
    powerful, biographical, trapped, regional).

% Smaller and less diplomatically powerful than France, Belgian claimants absorb a similar downward revision with even less capacity to contest it in the negotiating rounds that produce Dawes and Young; their reconstruction financing shortfall becomes a domestic fiscal problem instead of a German one.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, belgian_reconstruction_claimants, payer,
    moderate, biographical, trapped, regional).

% Britain and France had financed war costs partly through debt to each other and to the United States, expecting German reparations to help cover repayment; the capacity-bounded reading reduces the reparations flow they depend on, pushing the shortfall onto their own taxpayers and war-loan holders without those holders having any seat in the reparations negotiations.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, allied_war_bondholders, payer,
    moderate, biographical, constrained, continental).

% The inter-Allied body charged with assessing German capacity and revising schedules; it adjudicates between German claims of incapacity and Allied claims of deliberate evasion, producing successive expert reports (Dawes, Young) that operationalize the capacity-to-pay doctrine as technical economics rather than politics.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, reparations_commission, observer,
    institutional, generational, analytical, continental).

% American loans (Dawes Plan bonds) effectively recycle capital through Germany back to Allied treasuries; the syndicates that underwrite this profit from the arrangement's persistence but are not formal parties to the treaty negotiation and have no declared voice in the capacity assessments, despite structuring the mechanism by which capacity claims get financially operationalized.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__limited_responsibility_reading, us_banking_syndicates, excluded,
    organized, biographical, arbitrage, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(versailles_reparations_clauses__limited_responsibility_reading, german_treasury_negotiators).
narrative_ontology:fixing_cost_class(versailles_reparations_clauses__limited_responsibility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ties the reparations schedule to periodically reassessed German fiscal and economic capacity, in principle preventing a payment burden that would collapse the paying economy and destabilize the continent — a genuine coordination problem given that an insolvent Germany serves no one's interest, including the creditors'.
% TRANSFER_FUNCTION: Bounds and reduces the flow of compensation from Germany to French and Belgian reconstruction claimants and to Allied treasuries relative to the schedule those claimants were promised, redirecting fiscal slack toward German domestic recovery and industrial elites' balance sheets.
% ABSENT_VOICES: Individual French and Belgian war-damage claimants (farmers, small manufacturers, municipalities) have no seat at London, Dawes, or Young; their claims are aggregated into state-level totals that are then the object of downward revision without their participation. US banking syndicates that structure the actual capital flows are also outside the formal treaty apparatus despite shaping outcomes.
% DISAPPEARANCE_RATIONALE: German negotiators and industrial elites would say the world barely rearranges — capacity constraints are real and would reassert themselves through default or currency collapse regardless of the doctrine's existence. French and Belgian claimants would say the world rearranges significantly: without the capacity-bounded framing as diplomatic cover, either enforcement would have been harsher (Ruhr-style occupation sustained) or the shortfall would have been named as unrecovered damage rather than economic necessity.
% FOUNDING_PROBLEM: The Treaty needed a way to reconcile the size of Allied wartime losses and the political demand for accountability with the practical reality that an economically prostrate Germany could destabilize the continent if crushed by an unpayable schedule.
% FOUNDING_PROBLEM_CORROBORATION: British economists (Keynes, contemporaneously, from outside the German negotiating party) corroborated the capacity constraint as economically real and warned against an unpayable schedule; French domestic audiences and reconstruction claimants, also outside the German party, dispute that capacity assessments were conducted in good faith rather than as a negotiating tactic exploited by German elites to shrink obligations beyond what capacity actually required.
narrative_ontology:disappearance_verdict(versailles_reparations_clauses__limited_responsibility_reading, contested).
narrative_ontology:founding_problem_status(versailles_reparations_clauses__limited_responsibility_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(versailles_reparations_clauses__limited_responsibility_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(versailles_reparations_clauses__limited_responsibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(versailles_reparations_clauses__limited_responsibility_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(versailles_reparations_clauses__limited_responsibility_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(versailles_reparations_clauses__limited_responsibility_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(versailles_reparations_clauses__limited_responsibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts moderately high (0.62) reflecting the original London Schedule's demands, falls through the Dawes/Young revisions as the capacity-bounded framing succeeds in shrinking the effective transfer (trough ~0.46 around the mid-1920s currency stabilization), then ticks back up slightly as post-stabilization German growth reopens capacity-based arguments for partial restoration of pressure before the Great Depression truncates the whole arrangement. Suppression (enforcement requirement) peaks early around the Ruhr occupation crisis (1923) when France used physical occupation to compel payment against German claims of incapacity, then declines as the Dawes Plan substitutes American-financed recycling for direct coercion, before rising again as depression-era German default resistance forces renewed enforcement pressure. Theater ratio rises through the period as expert commissions (Dawes, Young) increasingly perform technical neutrality around what is, from the payer seats' view, a political negotiation over how much of the loss gets absorbed by whom.
 *
 * PERSPECTIVAL GAP:
 *   From the German treasury/elite seat, this reads as a rope or scaffold — a sensible, temporary, economically necessary correction preventing mutual catastrophe, oriented toward eventual normalization (hence the schedule's genuine sunset ambitions in Dawes and Young). From the French and Belgian claimant seats, the same structure reads as tangled rope at best: a genuine coordination rationale (preventing German collapse) riding alongside an asymmetric extraction (their promised compensation shrinks) that requires continuous diplomatic and occasional military enforcement (the Ruhr occupation) to sustain any payment at all.
 *
 * DIRECTIONALITY LOGIC:
 *   German treasury negotiators and industrial elites sit near the beneficiary end: they receive the schedule reduction directly and control the negotiating apparatus that produces it. French and Belgian reconstruction claimants sit near the target end: trapped by geography (the damage is theirs, physically, and cannot be relocated or exited), they absorb the downward revision as an unrecoverable shortfall. Allied bondholders sit closer to symmetric-but-diffuse: constrained rather than trapped, since the shortfall is distributed across taxpayers rather than concentrated, but with no direct negotiating seat of their own.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling accountability with the risk of destabilizing an insolvent Germany — was genuinely live through the hyperinflation crisis of 1923 and arguably resolved once currency stabilization (1924) removed the acute collapse risk. The capacity-bounded reading's persistence past that point, continuing to justify reductions through the Dawes and Young revisions even as German industrial output recovered, is where the mandatrophy question sharpens: was the doctrine still tracking genuine capacity constraints, or had it become a negotiating template that German elites deployed regardless of actual capacity? The contested founding_problem_status field reflects this: German-seat corroboration treats the problem as persistently live; claimant-seat corroboration treats it as substantially resolved by the mid-1920s, with the doctrine continuing as leverage rather than diagnosis.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capacity_assessment_good_faith,
    'Were the capacity assessments underlying the London, Dawes, and Young schedules genuine technical measurements of German economic capacity, or negotiating instruments that German elites systematically biased downward?',
    'Comparative analysis of independent contemporary economic estimates (British Treasury, US financial advisors, League of Nations economists) against the figures German negotiators submitted, checked against Germany''s actual post-1924 industrial recovery trajectory.',
    'If assessments were substantially good-faith, this reading''s coordination function (preventing destabilizing over-extraction) dominates and the tangled_rope classification tilts toward rope. If assessments were systematically biased by the negotiating party controlling the domestic data, the coordination story is largely cover and the classification tilts toward snare from the claimant seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capacity_assessment_good_faith, empirical, 'Whether capacity assessments were technically honest or strategically biased.').

omega_variable(
    article_231_formal_vs_moral_reading,
    'Is Article 231''s ''war guilt clause'' correctly read as a narrow legal predicate for the reparations claim (this reading''s premise), or does its drafting and diplomatic reception establish it as a moral condemnation with independent force (the punitive_liability_reading''s premise)?',
    'Textual and drafting-history analysis of the treaty negotiations, contemporaneous diplomatic correspondence, and how German versus Allied publics understood the clause at signing versus in later legal argument.',
    'If the narrow legal-predicate reading is correct, capacity-bounding is the natural implementation and this reading''s beneficiary/victim structure holds. If the moral-condemnation reading is correct, capacity-bounding functions as evasion of an acknowledged moral debt, strengthening the case for classifying this reading''s operation as more extractive-of-legitimacy than the metrics here suggest.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(article_231_formal_vs_moral_reading, conceptual, 'Whether Article 231 is a legal technicality or a moral verdict with independent force.').

omega_variable(
    counterfactual_enforcement_absent_doctrine,
    'Absent the capacity-bounded doctrine as diplomatic cover, would Allied powers have sustained harsher enforcement (extended occupation, asset seizure) or would the schedule have collapsed into de facto non-payment regardless?',
    'Comparative case study against reparations regimes without a comparable capacity-bounding doctrine, and closer analysis of French/Belgian domestic political appetite for sustained occupation after 1923-24.',
    'If harsher enforcement was politically sustainable, the doctrine''s function was primarily to reduce claimant recovery relative to an enforceable maximum, sharpening the extraction reading. If enforcement was never sustainable regardless of doctrine, the doctrine''s marginal effect on outcomes is smaller than the metrics imply.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_enforcement_absent_doctrine, conceptual, 'Counterfactual enforcement capacity absent the capacity-bounding doctrine.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(versailles_reparations_clauses__limited_responsibility_reading, 0, 132).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vers_tr_t0, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(vers_tr_t22, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 22, 0.2).
narrative_ontology:measurement(vers_tr_t44, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 44, 0.26).
narrative_ontology:measurement(vers_tr_t66, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 66, 0.31).
narrative_ontology:measurement(vers_tr_t88, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 88, 0.29).
narrative_ontology:measurement(vers_tr_t110, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 110, 0.27).
narrative_ontology:measurement(vers_tr_t132, versailles_reparations_clauses__limited_responsibility_reading, theater_ratio, 132, 0.28).

% Extraction over time
narrative_ontology:measurement(vers_be_t0, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(vers_be_t22, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 22, 0.58).
narrative_ontology:measurement(vers_be_t44, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 44, 0.5).
narrative_ontology:measurement(vers_be_t66, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 66, 0.46).
narrative_ontology:measurement(vers_be_t88, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 88, 0.5).
narrative_ontology:measurement(vers_be_t110, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 110, 0.53).
narrative_ontology:measurement(vers_be_t132, versailles_reparations_clauses__limited_responsibility_reading, base_extractiveness, 132, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(vers_su_t0, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(vers_su_t22, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 22, 0.75).
narrative_ontology:measurement(vers_su_t44, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 44, 0.68).
narrative_ontology:measurement(vers_su_t66, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 66, 0.6).
narrative_ontology:measurement(vers_su_t88, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 88, 0.55).
narrative_ontology:measurement(vers_su_t110, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 110, 0.58).
narrative_ontology:measurement(vers_su_t132, versailles_reparations_clauses__limited_responsibility_reading, suppression_requirement, 132, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(versailles_reparations_clauses__limited_responsibility_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(versailles_reparations_clauses__limited_responsibility_reading, 0.1).
narrative_ontology:affects_constraint(versailles_reparations_clauses__limited_responsibility_reading, versailles_reparations_clauses__punitive_liability_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__limited_responsibility_reading, versailles_reparations_clauses__repudiation_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__limited_responsibility_reading, dawes_plan_bond_recycling).
narrative_ontology:affects_constraint(versailles_reparations_clauses__limited_responsibility_reading, weimar_hyperinflation_fiscal_crisis).

% DUAL FORMULATION NOTE:
% This is one of three sibling constraint files decomposing the natural-language label 'the Versailles reparations regime' per the epsilon-invariance principle: the punitive_liability_reading (Article 231 grounds near-unlimited moral-financial liability), the repudiation_reading (the treaty is illegitimate and non-binding), and this limited_responsibility_reading (Article 231 is legal formality; payments bounded by capacity). Each carries its own epsilon, beneficiary/victim structure, and classification because each reading of the same treaty text produces a structurally distinct claim about who owes what to whom and why.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(versailles_reparations_clauses__limited_responsibility_reading, powerful, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
