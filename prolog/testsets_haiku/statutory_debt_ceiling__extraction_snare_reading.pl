% ============================================================================
% CONSTRAINT STORY: statutory_debt_ceiling__extraction_snare_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: Debt Ceiling Hostage Mechanism (Extraction Snare Reading)
 *   domain: constitutional/political/economic
 *
 * SUMMARY:
 *   The U.S. statutory debt ceiling is a fixed aggregate limit on Treasury
 *   borrowing, nominally set by Congress to constrain fiscal excess. Under
 *   this extraction-snare reading, it functions as an asymmetric hostage
 *   mechanism: a legislative minority holding veto power over raising the
 *   ceiling uses that veto to extract policy concessions from the majority,
 *   the executive, and the general public. The majority party and executive
 *   bear the political and economic cost of a potential default; the minority
 *   captures policy wins it could not achieve through normal legislative
 *   process. This reading interprets the ceiling as pure extraction
 *   maintained by the structural imbalance between the majority's fear of
 *   default (reputational, economic) and the minority's willingness to
 *   threaten it. The measurement series tracks rising extractiveness and
 *   rising theater ratio (policy concessions increasing as proportion of the
 *   ceiling's functional role relative to actual fiscal constraint), with
 *   suppression requirement rising and stabilizing at the plateau where
 *   threat-capacity becomes effective.
 *
 * KEY AGENTS:
 *   - Legislative minority faction: holds veto over ceiling increase; extracts policy concessions using default threat as leverage
 *   - Majority party: forced to negotiate policy losses in exchange for ceiling raise; bears responsibility for default if negotiations fail
 *   - Executive branch: constrained by ceiling, cannot prevent default unilaterally, identity-locked to fiscal stewardship role
 *   - Federal employees, welfare recipients, public: bear diffuse costs of standoff and extracted policy changes without negotiating leverage
 *   - Creditors and financial markets: price in political risk, face downgrades and repricing costs during standoff
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statutory_debt_ceiling__extraction_snare_reading, 0.82).
domain_priors:suppression_score(statutory_debt_ceiling__extraction_snare_reading, 0.71).
domain_priors:theater_ratio(statutory_debt_ceiling__extraction_snare_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(statutory_debt_ceiling__extraction_snare_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statutory_debt_ceiling__extraction_snare_reading, snare).
narrative_ontology:human_readable(statutory_debt_ceiling__extraction_snare_reading, "Debt Ceiling Hostage Mechanism (Extraction Snare Reading)").
narrative_ontology:topic_domain(statutory_debt_ceiling__extraction_snare_reading, "constitutional/political/economic").

domain_priors:requires_active_enforcement(statutory_debt_ceiling__extraction_snare_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statutory_debt_ceiling__extraction_snare_reading, '5ca09c7d-d090-4312-bdee-1d944e832bab').
narrative_ontology:cs_kernel_codification('5ca09c7d-d090-4312-bdee-1d944e832bab', fixed_text).
narrative_ontology:cs_authority_grounding('5ca09c7d-d090-4312-bdee-1d944e832bab', extraction).
narrative_ontology:cs_interpretation_layer_present('5ca09c7d-d090-4312-bdee-1d944e832bab').
narrative_ontology:cs_reading_relation('5ca09c7d-d090-4312-bdee-1d944e832bab', statutory_debt_ceiling__coordination_scaffold_reading, influences).
narrative_ontology:cs_reading_relation('5ca09c7d-d090-4312-bdee-1d944e832bab', statutory_debt_ceiling__constitutional_nullity_reading, coexists_with).
narrative_ontology:cs_axiom('5ca09c7d-d090-4312-bdee-1d944e832bab', foundational, ceiling_hostage_mechanism_doctrine).
narrative_ontology:cs_axiom_status(ceiling_hostage_mechanism_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('5ca09c7d-d090-4312-bdee-1d944e832bab', ceiling_hostage_mechanism_doctrine, empirically_contingent).
narrative_ontology:cs_axiom('5ca09c7d-d090-4312-bdee-1d944e832bab', secondary, minority_veto_extraction_legitimacy).
narrative_ontology:cs_axiom_status(minority_veto_extraction_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('5ca09c7d-d090-4312-bdee-1d944e832bab', minority_veto_extraction_legitimacy, conventional).
narrative_ontology:cs_reference_frame('5ca09c7d-d090-4312-bdee-1d944e832bab', minority_veto_structural_capacity).
narrative_ontology:cs_drift_state('5ca09c7d-d090-4312-bdee-1d944e832bab', contemporary_post_2011_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5ca09c7d-d090-4312-bdee-1d944e832bab', '2026-06-12T14:32:15Z').
narrative_ontology:cs_kernel_id(statutory_debt_ceiling__extraction_snare_reading, statutory_debt_ceiling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__extraction_snare_reading, legislative_minority_faction).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, majority_party).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, executive_branch).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, creditors).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, federal_employees).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, welfare_recipients).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, general_public).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(statutory_debt_ceiling__extraction_snare_reading, creditors_financial_markets).
narrative_ontology:constraint_vindicates(statutory_debt_ceiling__extraction_snare_reading, legislative_hostage_capacity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls enough votes to block debt ceiling increase, using that veto power to extract policy concessions unrelated to fiscal responsibility. Sets the frame: 'we will default unless you adopt our preferred tax, spending, or regulatory agenda.' Directly captures the extraction in the form of policy wins that would not have occurred through ordinary legislative process.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, legislative_minority_faction, agenda_setter,
    organized, biographical, constrained, national).

% Must negotiate with the minority to raise the ceiling or face a debt default it does not want. Pays through policy concessions: spending restraints, regulatory rollbacks, or legislative defeats on items the majority prioritized. Cannot exit by overriding the minority (filibuster rules, coalition arithmetic) without fracturing their own coalition.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, majority_party, payer,
    organized, biographical, constrained, national).

% Cannot unilaterally raise the debt ceiling or prevent default; has only emergency authority at the margins (extraordinary measures). Bears reputational and operational cost of default threat, and constraint limits fiscal flexibility. Identity-locked to constitutional oath and fiscal stewardship — cannot refuse to govern.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, executive_branch, payer,
    powerful, generational, identity_locked, national).

% Face delayed pay or furloughs during debt ceiling standoffs. No leverage over the constraint. Bears the cost of political negotiation without seat at the table. Exit is resignation, not renegotiation.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, federal_employees, payer,
    powerless, biographical, trapped, national).

% Lose benefits or face administrative delays during default threat periods. Carry the constraint's costs without negotiating power. Most vulnerable to disruption from both the threat and the concessions extracted by the minority.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, welfare_recipients, payer,
    powerless, immediate, trapped, national).

% Face credit rating downgrades, higher borrowing costs, and default risk premiums when the ceiling threatens. Price in political risk from each standoff. Constrained by global demand for dollar-denominated assets, but can shift allocations. Extracted via lost returns and forced repricing.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, creditors_financial_markets, payer,
    powerful, biographical, constrained, global).

% Bears diffuse costs: market volatility, higher borrowing costs for mortgages and student loans, economic uncertainty, potential recession from default. No direct negotiating role. Exit options are migration or political pressure (neither immediate).
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, general_public, payer,
    powerless, biographical, trapped, national).

% The commitment structure grounding legislative authority in appropriations power and the treasury authority in debt management are both implicated by the ceiling's constraint on their interaction. Observers of the constitutional tension, not parties to the extraction.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__extraction_snare_reading, constitutional_tradition, observer,
    analytical, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(statutory_debt_ceiling__extraction_snare_reading, constitutional_tradition).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(statutory_debt_ceiling__extraction_snare_reading, legislative_minority_faction).
narrative_ontology:fixing_cost_class(statutory_debt_ceiling__extraction_snare_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None from this reading's perspective: the debt ceiling performs no active coordination function. The majority and minority could allocate resources, approve appropriations, and authorize borrowing through ordinary legislative process without a rigid ceiling that creates hostage leverage.
% TRANSFER_FUNCTION: Transfers policy concessions from the majority party to the minority faction: tax codes, spending allocations, regulatory priorities, judicial nominees, or procedural rules shift to align with minority preferences in exchange for the minority voting to raise the ceiling.
% ABSENT_VOICES: Future generations and creditors (global markets) would argue the ceiling's use as hostage creates long-term fiscal instability and sovereign risk premiums they bear indefinitely. They have no seat at debt ceiling standoffs. Supranational actors (IMF, rating agencies) observe but do not control the outcome.
% DISAPPEARANCE_RATIONALE: If the debt ceiling disappeared, the executive and majority party would exercise fiscal authority without minority hostage leverage. Spending and borrowing decisions would proceed through ordinary appropriations and budget process. Minority factions would lose the only mechanism that forces the majority to negotiate around-the-table on issues they have already lost on the floor.
% FOUNDING_PROBLEM: The debt ceiling was established in 1917 as a procedural gate to give Congress collective oversight of Treasury borrowing, replacing item-by-item bond authorization with a single aggregate limit.
% FOUNDING_PROBLEM_CORROBORATION: Congressional historians and fiscal scholars (outside the minority faction benefiting from its hostage use) document that Treasury operations and fiscal forecasting have evolved since 1917 — Congress now sets appropriations (which determine spending) and tax policy separately; the ceiling adds no substantive fiscal constraint, only procedural friction. The founding problem of fragmented authorization oversight no longer exists; modern appropriations committees perform that function. The minority faction contests this characterization but cannot cite structural evidence that removal would harm fiscal discipline.
narrative_ontology:disappearance_verdict(statutory_debt_ceiling__extraction_snare_reading, world_rearranges).
narrative_ontology:founding_problem_status(statutory_debt_ceiling__extraction_snare_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statutory_debt_ceiling__extraction_snare_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(statutory_debt_ceiling__extraction_snare_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statutory_debt_ceiling__extraction_snare_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.82 endpoint) because the mechanism systematically transfers policy outcomes from the majority to the minority against the majority's stated preferences. Suppression is substantial (0.71) because the extraction persists through institutional veto power and default threat, not through voluntary coordination — the majority cannot exit without catastrophic reputational cost (triggering default). Theater ratio rises from 0.28 to 0.44 because the ratio of performative negotiation (political theater, public statements) to actual fiscal constraint (the ceiling's nominal role as spending limit) increases over time — what began as a nominal fiscal gate becomes increasingly a political negotiation stage. Accessibility of alternatives collapses only partially (0.38) because the majority and executive retain theoretical alternatives (constitutional amendment, executive authority tests, market confidence repair) but these are suppressed by practical cost and risk, not eliminated. Resistance is substantial (0.72) because both the majority party and the public actively resist the minority's use of default threat — this is not a consensually accepted arrangement. The measurement series shows rising extractiveness as the minority faction learns the threat is credible and raises demands; the plateau at point 25 reflects institutional learning: the threat-capacity has reached maximum credibility, and further extractiveness gains require ratcheting the threat (requiring actual default, which would be mutually destructive).
 *
 * PERSPECTIVAL GAP:
 *   From the minority faction's seat, the constraint enables legitimate leverage: they hold actual veto power and use it to force renegotiation of a majority position they believe is wrong. From the majority seat, it is hostage-taking: the same veto power is experienced as extractive coercion because the majority has already won the policy fight through normal process and should not have to re-fight it. From the executive seat, it is a vulnerability: the ceiling constrains fiscal authority and creates default risk outside the executive's control. From the powerless seats (federal employees, welfare recipients), it is arbitrary harm: policy concessions made in the ceiling negotiation may reverse their protections, and they bear costs from both the threat and the concessions. The engine computes these divergences from the structural data — the minority's veto power is real; its experience as legitimate leverage is a coherent reading from that seat; the majority's experience as coercion is equally structural, as is the powerless seats' experience as arbitrary.
 *
 * DIRECTIONALITY LOGIC:
 *   The legislative minority is the agenda-setter and beneficiary (d near 0.0, beneficiary end): they control the hostage mechanism and extract policy concessions. The majority party is a target (d near 1.0, victim end): they lose policy outcomes they won and bear the political cost of negotiating under threat. The executive is close to the target end (d ~0.85): constrained by the ceiling, cannot prevent default unilaterally, identity-locked to stewardship role preventing exit. Federal employees and welfare recipients are full targets (d~1.0, trapped): bear costs with no negotiating leverage. Creditors are targets (d~0.75): face pricing costs and rating downgrades from political risk they did not create and cannot control. The general public is diffuse target (d~0.80): bears costs across multiple dimensions (market volatility, recession risk, fiscal drag from policy concessions) without leverage. No override needed: the derivation chain from beneficiary (minority) and victims (all others) + trapped/constrained exit options produces accurate d-placement.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem of the debt ceiling (1917: fragmented Treasury authorization) is dead. Congress now exercises fiscal authority through consolidated appropriations and tax committees; the Treasury operates under unified direction. The ceiling adds no procedural value and performs no active fiscal constraint (borrowing is mechanically determined by spending minus revenue, so the ceiling binds only by repeated legislative action). Yet the constraint persists because it has acquired a new function: enabling hostage leverage. The minority faction benefits from keeping the constraint alive; the majority would prefer to abolish it but lacks the supermajority to do so without minority cooperation. This is the mandatrophy signature: a founding problem that has atrophied but the constraint persists because a new extractive function has grafted onto the old structure. The theater ratio rising from 0.28 to 0.44 captures this transition — the functional activity shifts from nominal fiscal oversight to political negotiation, an increasing proportion of enforcement effort goes to extracting concessions rather than constraining spending.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    veto_legitimacy_vs_hostage_framing,
    'Does the minority faction''s use of ceiling veto constitute legitimate legislative leverage or illegitimate hostage-taking — or is that distinction reading-dependent?',
    'Comparative analysis of other legislative veto structures (filibuster, motion to recommit, committee veto) and their normative evaluation across constitutional traditions. Distinguishing moments: (a) Does the minority propose alternative fiscal solutions (legitimate counteroffer) or demand unrelated policy concessions (hostage extortion)? (b) Do they set a deadline for negotiation (legitimate bargaining) or use the threat indefinitely to suppress majority decisions (hostage persistence)? (c) Is the veto tied to their fiscal philosophy or disconnected from any budget principle (coherent position vs. arbitrary extraction).',
    'If framed as hostage-taking, the constraint reclassifies robustly as snare; if framed as legitimate leverage, it coexists with the scaffold reading as two institutional readings of the same mechanism. This reading''s ε assumes hostage framing; the alternate reading uses coexists_with relation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(veto_legitimacy_vs_hostage_framing, conceptual, 'Whether veto-hostage distinction is factual or frame-dependent.').

omega_variable(
    default_threat_credibility,
    'How credible is the minority''s threat to permit default? If the threat is not credible, the extraction mechanism collapses.',
    'Empirical record: (a) Historical instances where the minority threatened default and the majority capitulated (credibility signal). (b) Instances where the minority backed down or raised the ceiling without concessions (non-credibility signal). (c) Market pricing of default risk during standoff periods (credibility revealed through financial instrument pricing). (d) Statements from minority actors about willingness to default (weak evidence, often contradicted by action).',
    'If the threat is not credible (majority knows the minority will back down at the last second), the extraction disappears and the constraint reverts to nominal fiscal gate (rope or scaffold). If the threat is credible, the extraction persists and ε remains high (snare). The measurement series assumes rising credibility to t18, then plateau.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(default_threat_credibility, empirical, 'Whether the default threat is structurally credible or illusory.').

omega_variable(
    fiscal_constraint_residual,
    'Does the ceiling perform ANY actual fiscal constraint, or is borrowing entirely mechanically determined by spending minus revenue?',
    'Fiscal analysis: (a) Does the ceiling ever bind between congressional appropriations votes? (b) Do appropriations committees consider the ceiling when budgeting, or do they set appropriations first and then raise the ceiling as needed? (c) Are there instances where the ceiling actually prevented spending that had appropriation, or does the ceiling only halt execution after appropriation is already set? (d) Do other democracies that lack a debt ceiling show different fiscal discipline outcomes?',
    'If the ceiling performs no actual constraint, the ''fiscal oversight'' rationale is pure theater and supports snare classification. If the ceiling does constrain, it supports scaffold or rope reading (genuine coordination function). This reading assumes the constraint performs minimal fiscal coordination and high extraction; the coordinate reading assumes residual coordination value.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fiscal_constraint_residual, empirical, 'Whether the ceiling constrains spending or merely halts execution after spending is committed.').

omega_variable(
    reading_foreclosure_constitutional,
    'Does this extraction-snare reading foreclose the constitutional-nullity reading, or can both coexist as institutional and constitutional framings of the same text?',
    'Jurisprudential analysis: (a) The nullity reading rests on 14th Amendment Section 4 preemption of the ceiling. (b) This extraction reading assumes the ceiling is valid law that the minority uses to extract concessions. (c) These can coexist if courts have never definitively resolved the 14th Amendment question (which is true); or they foreclose if the 14th Amendment reading is adopted (the ceiling becomes legally void, ending the extraction mechanism). The coexistence is jurisdictionally contingent: courts permit ceiling use (extraction reading live), or courts void the ceiling (nullity reading instantiated, extraction reading becomes moot).',
    'If the extraction reading forecloses nullity, this would require establishing the ceiling is structurally unchallengeable — a strong claim not yet judicially confirmed. If they coexist (high probability), they are institutional readings held by different factions, and both constraints exist in different reference frames (political vs. constitutional). This reading treats them as coexisting.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_constitutional, conceptual, 'Whether constitutional nullity and extraction readings are foreclosing or coexisting.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.71) structural (the ceiling''s legal enforcement and veto mechanism) or internalized (the majority''s acceptance of majority-rule norms and default-fear)?',
    'Natural experiment: if a new legislative coalition tried to remove the ceiling without minority consent, would they face (a) legal barriers (structural suppression) or (b) norm-based pressure and fear of retaliation (internalized suppression)? The U.S. has no legal barrier to abolishing the ceiling via majority vote (only filibuster); the suppression is institutional and norm-based. The majority could override via supermajority coalition-building or reform, but the default-fear and norm internalization prevent it.',
    'If suppression is structural (legal), it is harder to reverse than if it is internalized (norm-based). If internalized, post-reform suppression might persist (the majority has learned that ceiling threats are credible) — making the extraction zombie-like. This reading estimates suppression as ~60% structural veto power, ~40% internalized fear, putting it at 0.71 blended.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether measured suppression is veto mechanism or internalized fear.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statutory_debt_ceiling__extraction_snare_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(stat_tr_t0, observed).
narrative_ontology:measurement(stat_tr_t3, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 3, 0.32).
narrative_ontology:measurement_basis(stat_tr_t3, observed).
narrative_ontology:measurement(stat_tr_t6, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 6, 0.36).
narrative_ontology:measurement_basis(stat_tr_t6, observed).
narrative_ontology:measurement(stat_tr_t12, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 12, 0.41).
narrative_ontology:measurement_basis(stat_tr_t12, observed).
narrative_ontology:measurement(stat_tr_t18, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 18, 0.43).
narrative_ontology:measurement_basis(stat_tr_t18, observed).
narrative_ontology:measurement(stat_tr_t25, statutory_debt_ceiling__extraction_snare_reading, theater_ratio, 25, 0.44).
narrative_ontology:measurement_basis(stat_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(stat_be_t0, observed).
narrative_ontology:measurement(stat_be_t3, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 3, 0.61).
narrative_ontology:measurement_basis(stat_be_t3, observed).
narrative_ontology:measurement(stat_be_t6, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 6, 0.68).
narrative_ontology:measurement_basis(stat_be_t6, observed).
narrative_ontology:measurement(stat_be_t12, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 12, 0.76).
narrative_ontology:measurement_basis(stat_be_t12, observed).
narrative_ontology:measurement(stat_be_t18, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 18, 0.8).
narrative_ontology:measurement_basis(stat_be_t18, observed).
narrative_ontology:measurement(stat_be_t25, statutory_debt_ceiling__extraction_snare_reading, base_extractiveness, 25, 0.82).
narrative_ontology:measurement_basis(stat_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 0, 0.54).
narrative_ontology:measurement_basis(stat_su_t0, observed).
narrative_ontology:measurement(stat_su_t3, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 3, 0.59).
narrative_ontology:measurement_basis(stat_su_t3, observed).
narrative_ontology:measurement(stat_su_t6, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 6, 0.64).
narrative_ontology:measurement_basis(stat_su_t6, observed).
narrative_ontology:measurement(stat_su_t12, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 12, 0.69).
narrative_ontology:measurement_basis(stat_su_t12, observed).
narrative_ontology:measurement(stat_su_t18, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 18, 0.71).
narrative_ontology:measurement_basis(stat_su_t18, observed).
narrative_ontology:measurement(stat_su_t25, statutory_debt_ceiling__extraction_snare_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(stat_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statutory_debt_ceiling__extraction_snare_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(statutory_debt_ceiling__extraction_snare_reading, 0.12).
narrative_ontology:affects_constraint(statutory_debt_ceiling__extraction_snare_reading, statutory_debt_ceiling__coordination_scaffold_reading).
narrative_ontology:affects_constraint(statutory_debt_ceiling__extraction_snare_reading, statutory_debt_ceiling__constitutional_nullity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the statutory debt ceiling kernel. The coordination_scaffold_reading instantiates the same text as a transitional procedural mechanism; the constitutional_nullity_reading instantiates it as legally void under the 14th Amendment. The three readings coexist as institutional and constitutional framings; they affect each other's resource availability and legitimacy conditions. The extraction-snare reading (this file) increases political capital required to maintain the ceiling's legitimacy, which influences the constitutional challenge's viability and the scaffold reading's credibility. Sibling readings linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(statutory_debt_ceiling__extraction_snare_reading, organized, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
