% ============================================================================
% CONSTRAINT STORY: commerce_clause_scope__broad_effects_test
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_scope__broad_effects_test, []).

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
 *   constraint_id: commerce_clause_scope__broad_effects_test
 *   human_readable: Commerce Clause Broad Effects Test Reading
 *   domain: constitutional_law/federalism
 *
 * SUMMARY:
 *   This constraint instantiates the broad effects test reading of the
 *   Commerce Clause kernel: the interpretation that federal power extends to
 *   any intrastate economic activity whose aggregate effects substantially
 *   impact interstate commerce, with 'regulate' understood to include
 *   prohibition and comprehensive control. Under this reading, virtually all
 *   economic activity falls within federal reach, as nearly all commerce has
 *   some cumulative economic impact. The federal government and
 *   national-interest coalitions benefit from the authority and uniformity
 *   this generates; state governments and local economic autonomy bear the
 *   cost. This reading coexists with narrow originalist and
 *   intermediate-limiting readings of the same constitutional text—they are
 *   different constraints with different beneficiary structures and different
 *   ε values. This story addresses only the broad effects test reading as a
 *   single, ε-invariant constraint. The kernel contest is documented in omega
 *   variables and cs_structure fields, not embedded in the constraint claim
 *   itself.
 *
 * KEY AGENTS:
 *   - Federal regulatory agencies (institutional beneficiary; set doctrine through enforcement and rulemaking)
 *   - State legislatures (institutional victim; lose autonomy over intrastate economic regulation)
 *   - Congress (institutional agenda-setter; legislates under delegated commerce power)
 *   - Supreme Court majority (institutional agenda-setter; interprets the clause to endorse broad effects test)
 *   - National policy coalitions (organized beneficiary; shift between federal and state advocacy)
 *   - Local businesses (moderate-power payer; subject to aggregation doctrine)
 *   - Citizens valuing local autonomy (powerless victim; trapped in federal reach)
 *   - Strict federalism advocates (excluded; their core premise is incompatible with this reading)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_scope__broad_effects_test, 0.78).
domain_priors:suppression_score(commerce_clause_scope__broad_effects_test, 0.71).
domain_priors:theater_ratio(commerce_clause_scope__broad_effects_test, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, extractiveness, 0.78).
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_scope__broad_effects_test, tangled_rope).
narrative_ontology:human_readable(commerce_clause_scope__broad_effects_test, "Commerce Clause Broad Effects Test Reading").
narrative_ontology:topic_domain(commerce_clause_scope__broad_effects_test, "constitutional_law/federalism").

domain_priors:requires_active_enforcement(commerce_clause_scope__broad_effects_test).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_scope__broad_effects_test, 'd695e066-6047-449b-9098-40ede33ac04d').
narrative_ontology:cs_kernel_codification('d695e066-6047-449b-9098-40ede33ac04d', fixed_text).
narrative_ontology:cs_authority_grounding('d695e066-6047-449b-9098-40ede33ac04d', lineage).
narrative_ontology:cs_interpretation_layer_present('d695e066-6047-449b-9098-40ede33ac04d').
narrative_ontology:cs_reading_relation('d695e066-6047-449b-9098-40ede33ac04d', commerce_clause_scope__narrow_originalist, forecloses).
narrative_ontology:cs_reading_relation('d695e066-6047-449b-9098-40ede33ac04d', commerce_clause_scope__intermediate_channels, influences).
narrative_ontology:cs_axiom('d695e066-6047-449b-9098-40ede33ac04d', foundational, aggregation_doctrine_legitimate).
narrative_ontology:cs_axiom_status(aggregation_doctrine_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('d695e066-6047-449b-9098-40ede33ac04d', aggregation_doctrine_legitimate, empirically_contingent).
narrative_ontology:cs_axiom('d695e066-6047-449b-9098-40ede33ac04d', foundational, regulate_includes_comprehensive_control).
narrative_ontology:cs_axiom_status(regulate_includes_comprehensive_control, holdable).
narrative_ontology:cs_axiom_grounding('d695e066-6047-449b-9098-40ede33ac04d', regulate_includes_comprehensive_control, deontological).
narrative_ontology:cs_axiom('d695e066-6047-449b-9098-40ede33ac04d', secondary, federal_police_power_coextensive_with_commerce).
narrative_ontology:cs_axiom_status(federal_police_power_coextensive_with_commerce, holdable).
narrative_ontology:cs_axiom_grounding('d695e066-6047-449b-9098-40ede33ac04d', federal_police_power_coextensive_with_commerce, instrumental).
narrative_ontology:cs_reference_frame('d695e066-6047-449b-9098-40ede33ac04d', national_economic_integration_framework).
narrative_ontology:cs_drift_state('d695e066-6047-449b-9098-40ede33ac04d', contemporary_regulatory_state, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d695e066-6047-449b-9098-40ede33ac04d', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(commerce_clause_scope__broad_effects_test, commerce_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_scope__broad_effects_test, federal_regulatory_agencies).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__broad_effects_test, national_policy_coalitions).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__broad_effects_test, civil_rights_enforcement_institutions).
narrative_ontology:constraint_victim(commerce_clause_scope__broad_effects_test, state_legislatures).
narrative_ontology:constraint_victim(commerce_clause_scope__broad_effects_test, local_economic_experimentation).
narrative_ontology:constraint_victim(commerce_clause_scope__broad_effects_test, federalism_constraint_defenders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__broad_effects_test, state_legislatures).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__broad_effects_test, state_courts).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__broad_effects_test, congress).
narrative_ontology:constraint_victim(commerce_clause_scope__broad_effects_test, local_businesses).
narrative_ontology:constraint_victim(commerce_clause_scope__broad_effects_test, state_courts).
narrative_ontology:constraint_victim(commerce_clause_scope__broad_effects_test, citizens_valuing_local_autonomy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and enforce the broad effects test to regulate intrastate economic activities claimed to have cumulative interstate impact. Set jurisdictional boundaries through rulemaking and enforcement discretion. Justify expansions via economic aggregation doctrine. Collect regulatory authority and budgetary justification from the breadth of jurisdiction.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, federal_regulatory_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Lose traditional police power autonomy when intrastate economic regulation is preempted via aggregation doctrine. Must conform state laws to federal standards or face commerce-clause invalidation. Retain some coordination benefit from uniform national rules but bear the cost of surrendered experimentation space. Cannot easily exit federal jurisdiction without dismantling their own economies.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, state_legislatures, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_scope__broad_effects_test, state_legislatures, beneficiary).

% Subject to federal regulation based on aggregation doctrine: their individual intrastate activity, combined with millions of similar activities, is claimed to substantially affect interstate commerce. Face compliance costs calibrated to national standards rather than local conditions. Cannot exit by staying local—the aggregation principle reaches them anyway.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, local_businesses, payer,
    moderate, biographical, constrained, local).

% Apply the broad effects test in their own jurisdictions; gain prestige and uniform national doctrine from alignment with federal interpretation, but lose authority to develop independent state constitutional law on commerce and federalism. Identity-locked to the federal constitutional hierarchy through oath and professional credentials.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, state_courts, beneficiary,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_scope__broad_effects_test, state_courts, payer).

% Seek uniform national policy (civil rights enforcement, environmental standards, labor protections) and benefit from federal power to preempt local variation. Can shift between federal and state advocacy depending on which level favors their agenda at any moment. High exit optionality—they choose where to contest.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, national_policy_coalitions, beneficiary,
    organized, generational, arbitrage, national).

% Use federal commerce power to enforce anti-discrimination law into markets that would otherwise claim local autonomy. The broad effects test allows commerce-clause validation of statutes (Civil Rights Act of 1964 upheld via effects on interstate commerce) that would lack independent enumerated power. Can deploy federal authority strategically where state politics block remedy.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, civil_rights_enforcement_institutions, beneficiary,
    organized, generational, arbitrage, national).

% Would argue for narrowing commerce power to its original channels-and-instrumentalities reading, restoring state sovereignty and limiting federal reach to trade crossing state lines. Excluded from the baseline constitutional interpretation, though they participate in litigation and scholarly discourse. Their core claim—that aggregation doctrine conflates correlation with causal nexus—is structurally incompatible with the broad effects test's core premise.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, strict_federalism_advocates, excluded,
    powerful, civilizational, trapped, national).

% Interprets the Commerce Clause text and doctrine to endorse the broad effects test. Sets the constitutional boundary via precedent (Wickard v. Filburn, Gonzales v. Raich). Sustains the doctrine through selective application and limiting dicta. Maintains authority to revisit the reading if political composition changes.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, supreme_court_majority, agenda_setter,
    institutional, generational, analytical, universal).

% Legislates under delegated commerce power; uses aggregation doctrine to justify sweeping regulatory statutes (Environmental Protection Act, Affordable Care Act). Gains authority to regulate intrastate activities; faces political pressure from federalism critics. Could withdraw from using commerce power but gains regulatory reach and political credit for national solutions.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, congress, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_scope__broad_effects_test, congress, beneficiary).

% Bear the cost of lost state experimentation and local policy divergence. Cannot exit by moving to a different state—federal law follows. Cannot organize politically to change the constitutional reading without a supermajority coalition. Diffuse and unorganized relative to concentrated federal and national-coalition beneficiaries.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, citizens_valuing_local_autonomy, payer,
    powerless, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(commerce_clause_scope__broad_effects_test, federal_regulatory_agencies).
narrative_ontology:fixing_cost_class(commerce_clause_scope__broad_effects_test, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single national economic order by authorizing federal power to regulate intrastate economic activities whose aggregate effects substantially impact interstate commerce. Solves the collective-action problem of fragmentary state-level economic rules that would otherwise create a patchwork of standards. Enables uniform enforcement of civil rights, environmental, and consumer protections across state boundaries.
% TRANSFER_FUNCTION: Transfers regulatory authority from state police powers to federal agencies. Moves autonomy over local economic governance from states to federal institutions. Redirects the beneficiary stream: civil rights enforcement, national safety standards, and federal regulatory prestige accrue to federal institutions; state experimentation rights and regulatory autonomy flow away from states.
% ABSENT_VOICES: Strict federalism advocates and small-scale local businesses that would profit from state-level autonomy are structurally excluded from setting the default constitutional reading. Their arguments appear in dissents and scholarly literature but do not shape the baseline interpretation courts apply. States themselves appear as parties in litigation but operate under the constraint they contest.
% DISAPPEARANCE_RATIONALE: If the broad effects test disappeared and the narrow originalist reading replaced it, federal power would collapse to channels and instrumentalities only. States would immediately reclaim regulatory autonomy over local economic activity. Intrastate commerce would fragment into fifty different regimes. Federal civil rights enforcement would lose its primary statutory basis (commerce power is why the Civil Rights Act of 1964 was upheld). Environmental and labor law would revert to state control. The entire architecture of post-1937 federal regulatory authority would reorganize.
% FOUNDING_PROBLEM: Widespread state protectionism and economic fragmentation in the 19th and early 20th centuries, exacerbated by the Lochner Court's invalidation of federal economic regulation. The broad effects test was developed to allow federal response to national economic crises (Great Depression, post-WWII integration) and to prevent local regulatory variation from blocking national markets.
% FOUNDING_PROBLEM_CORROBORATION: Federal agencies and national policy coalitions attest the founding problem is still live, citing regulatory complexity in a national economy. Federalism advocates and state officials attest the problem is substantially solved and the reading persists as federal overreach. Economic historians and constitutional scholars outside the federal beneficiary set document that state protectionism is no longer the primary problem; the constraint now addresses coordination that could be achieved through other means. Legislative history and law review commentary from independent sources support the shifted-function reading.
narrative_ontology:disappearance_verdict(commerce_clause_scope__broad_effects_test, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_scope__broad_effects_test, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_scope__broad_effects_test, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(commerce_clause_scope__broad_effects_test, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_scope__broad_effects_test, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_scope__broad_effects_test_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(commerce_clause_scope__broad_effects_test, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(commerce_clause_scope__broad_effects_test_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the constraint's operation transfers state regulatory authority to federal institutions and subordinates local economic autonomy to aggregation doctrine. The transfer is not freely chosen—states cannot opt out of federal jurisdiction without dismantling their economies. Suppression is substantial (0.71) because federal enforcement machinery (agency rulemaking, judicial precedent, preemption doctrine) maintains the broad reading against state resistance and federalism critique. The suppression metric reflects active enforcement: states must conform or litigate, courts apply the doctrine expansively, and alternative interpretations are structurally disfavored. Theater ratio (0.42) is moderate: genuine regulatory functions exist (civil rights enforcement, environmental protection, consumer safety), but a growing share of enforcement activity targets economic regulation that may not require federal reach—regulatory expansion rides legitimate functions. Accessibility collapse (0.68) reflects that states cannot realistically escape federal jurisdiction; resistance (0.55) reflects ongoing federalism litigation and scholarly contestation. The measurement series tracks metric drift over 80 years: extractiveness rises as the doctrine expands from its post-1937 foundation to encompass activities with increasingly attenuated interstate effects. Theater rises through the same interval as regulatory scope expands beyond core functions (channels, instrumentalities) into peripheral economic activity. Suppression plateaus after mid-century as the doctrine becomes institutionalized and resistance is absorbed or litigated to exhaustion. All metrics share the same time grid per alignment requirements.
 *
 * PERSPECTIVAL GAP:
 *   Federal agencies and the Supreme Court compute the constraint as coordinating a national market and protecting interstate commerce from state-imposed fragmentation—genuine coordination. State legislatures and local businesses compute it as federal overreach extracting autonomy through an expansive doctrine of aggregation. This perspectival asymmetry is structural: the beneficiary seats (federal agencies, Congress, national coalitions) control interpretation and enforcement; the victim seats (states, local business) must litigate within a doctrine shaped by their opponents. The engine computes this divergence from the declared beneficiary/victim structure and directionality atoms—the authored claim (tangled_rope) does not preset the per-seat result.
 *
 * DIRECTIONALITY LOGIC:
 *   Federal regulatory agencies (institutional power, analytical exit) sit at d near 0.05—they are the clear beneficiaries, setting agenda, collecting authority, with unlimited exit optionality (they choose how to apply the doctrine). State legislatures (institutional power, constrained exit) sit at d near 0.85—they are the victims, losing autonomy, unable to exit without economic catastrophe. National policy coalitions (organized power, arbitrage exit) sit at d near 0.30—beneficiaries with high exit optionality, they can shift to state advocacy if federal action fails. Local businesses (moderate power, constrained exit) sit at d near 0.75—they are victims, subject to aggregation doctrine, trapped by the logic of intrastate activity. State courts (institutional power, identity-locked exit) sit at d near 0.40—they benefit from prestige and uniform doctrine but lose authority and are identity-locked to federal hierarchy. Civil rights institutions (organized power, arbitrage exit) sit at d near 0.10—clear beneficiaries with high strategic exit optionality. Citizens valuing autonomy (powerless, trapped) sit at d near 0.95—maximal targets with no exit. Strict federalism advocates (excluded) occupy no seat in the baseline constraint; they would occupy d near 0.90 if the reading changed to their alternative.
 *
 * MANDATROPHY ANALYSIS:
 *   The broad effects test was founded to solve state protectionism and economic fragmentation during the Great Depression and post-WWII integration crisis. That founding problem is substantially dead: modern economies are already integrated, state protectionism is rare and preempted by other doctrines (dormant commerce clause), and interstate trade flows with minimal friction. The constraint persists because federal agencies, Congress, and national coalitions benefit from the authority it confers—the founding problem's resolution created a new use case (uniform national regulation of intrastate activity) that benefits different parties than the original founding coalition. This is a live mandatrophy case: the constraint solves a dead problem (state protectionism) but persists to solve a different problem (national policy uniformity). The Tangled Rope classification captures this: the original coordination function is solved; what remains is asymmetric extraction (federal authority over intrastate activity) riding on that outdated coordination logic. Civil rights enforcement via commerce power is a genuine new coordination function (preventing state-level discrimination), but it is minority-party and could be achieved through enumerated powers (14th Amendment, Enforcement Clause) if the Supreme Court permitted. The broad effects test is the primary mechanism for federal economic regulation generally, making it more snare-like than rope-like at the aggregate level.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    aggregation_doctrine_empirical_boundary,
    'At what level of cumulative economic impact does the aggregation doctrine legitimately reach intrastate activity? Where is the principled threshold between ''substantial effect'' and ''negligible effect''?',
    'Empirical economic analysis of causal chains from specific intrastate activities to measurable interstate commerce effects. Regression analysis, natural experiments, econometric studies testing whether claimed effects are distinguishable from noise.',
    'A clear empirical threshold would allow courts to reject commerce-clause jurisdictional claims below the threshold, effectively narrowing the constraint''s reach. Absent a threshold, the constraint expands to cover all economic activity (the current state).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aggregation_doctrine_empirical_boundary, empirical, 'Whether aggregation doctrine has or could have a principled empirical stopping point.').

omega_variable(
    commerce_clause_originalism_kernel_contest,
    'Is the broad effects test the correct reading of the Commerce Clause kernel, or is the narrow originalist reading (commerce = trade crossing state lines; regulate = make regular) structurally truer to the text and founding intent?',
    'Textual analysis of the Clause itself (what ''commerce among the several states'' meant circa 1787); historical record of the Framers'' intent; parallel textual comparisons (does the same language elsewhere in the Constitution carry narrower meanings). A full originalist reconstruction could support either reading depending on methodology.',
    'If the narrow originalist reading is correct, the broad effects test is not a legitimate constitutional interpretation but a judicial rewriting that expanded federal power beyond the Framers'' grant. This would support immediate narrowing (as in Lopez, United States v. Morrison). If the broad effects test is correct, federalism constraints on commerce power are an anachronism and should be abandoned.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(commerce_clause_originalism_kernel_contest, conceptual, 'The core kernel contest: which reading of the Commerce Clause is structurally true. This omega documents that the constraint is one reading of a contested kernel, not a settled constitutional fact.').

omega_variable(
    federalism_as_constitutional_value,
    'Is federalism as a constraint on centralized power a constitutional value in its own right (structurally limiting federal reach), or is federalism merely an institutional arrangement serving other values (efficiency, representation, liberty) that can be traded off if centralized regulation serves those values better?',
    'Constitutional jurisprudence and scholarly consensus on the status of federalism: is it a structural limit on power (Tenth Amendment, Reserved Powers doctrine) or an institutional presumption rebuttable by national-interest claims? Current doctrine treats federalism as rebuttable; originalist revival would treat it as structural.',
    'If federalism is structural, the broad effects test violates the Constitution by allowing federal reach into intrastate activity reserved to the states. If federalism is rebuttable, the broad effects test is legitimate because national economic regulation serves the greater good. This is a preference-class omega: it depends on normative constitutional theory, not on facts.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(federalism_as_constitutional_value, preference, 'Whether federalism constrains federal power as a constitutional value or yields to national-interest claims.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression (0.71) primarily structural (federal institutional machinery blocking state action) or internalized (states have accepted the doctrine as legitimate and no longer resist)?',
    'Litigation frequency and resource commitment: states that accept the doctrine litigate rarely and minimally; states that resist litigate often and aggressively. Post-Lopez era should show increased resistance if internalization has faded. Survey data on state official attitudes toward federalism limits.',
    'If suppression is primarily structural, the constraint is maintained by institutional force and would collapse if courts changed the doctrine. If suppression is internalized, states have absorbed the constraint''s legitimacy and would not easily reassert authority even if courts weakened the doctrine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether suppression of state resistance is backed by institutional force or has become internalized as legitimate doctrine.').

omega_variable(
    commerce_power_as_fifth_enumerated_power,
    'Does the broad effects test effectively collapse the distinction between the Commerce Clause as one enumerated power and the other enumerated powers (Necessary and Proper, Taxing, Spending, Treaty)? If commerce power reaches all intrastate economic activity via aggregation, does enumeration mean anything?',
    'Doctrinal analysis: do courts treat commerce power differently from other enumerated powers, or has it become functionally a general police power? Contrast with strict limits on Spending power or Necessary and Proper power to show whether commerce is uniquely unconstrained.',
    'If commerce power has become unbounded, the constitutional structure of enumeration is meaningless and the Tenth Amendment is a dead letter (all authority is federal). This would support constitutional amendment or Supreme Court reversal to restore enumeration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commerce_power_as_fifth_enumerated_power, conceptual, 'Whether the broad effects test has effectively eliminated the enumeration structure of federal power.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_scope__broad_effects_test, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, commerce_clause_scope__broad_effects_test, theater_ratio, 0, 0.25).
narrative_ontology:measurement(comm_tr_t10, commerce_clause_scope__broad_effects_test, theater_ratio, 10, 0.28).
narrative_ontology:measurement(comm_tr_t20, commerce_clause_scope__broad_effects_test, theater_ratio, 20, 0.32).
narrative_ontology:measurement(comm_tr_t30, commerce_clause_scope__broad_effects_test, theater_ratio, 30, 0.36).
narrative_ontology:measurement(comm_tr_t40, commerce_clause_scope__broad_effects_test, theater_ratio, 40, 0.39).
narrative_ontology:measurement(comm_tr_t50, commerce_clause_scope__broad_effects_test, theater_ratio, 50, 0.41).
narrative_ontology:measurement(comm_tr_t60, commerce_clause_scope__broad_effects_test, theater_ratio, 60, 0.41).
narrative_ontology:measurement(comm_tr_t80, commerce_clause_scope__broad_effects_test, theater_ratio, 80, 0.42).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, commerce_clause_scope__broad_effects_test, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(comm_be_t10, commerce_clause_scope__broad_effects_test, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(comm_be_t20, commerce_clause_scope__broad_effects_test, base_extractiveness, 20, 0.64).
narrative_ontology:measurement(comm_be_t30, commerce_clause_scope__broad_effects_test, base_extractiveness, 30, 0.7).
narrative_ontology:measurement(comm_be_t40, commerce_clause_scope__broad_effects_test, base_extractiveness, 40, 0.74).
narrative_ontology:measurement(comm_be_t50, commerce_clause_scope__broad_effects_test, base_extractiveness, 50, 0.76).
narrative_ontology:measurement(comm_be_t60, commerce_clause_scope__broad_effects_test, base_extractiveness, 60, 0.77).
narrative_ontology:measurement(comm_be_t80, commerce_clause_scope__broad_effects_test, base_extractiveness, 80, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t0, commerce_clause_scope__broad_effects_test, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(comm_su_t10, commerce_clause_scope__broad_effects_test, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(comm_su_t20, commerce_clause_scope__broad_effects_test, suppression_requirement, 20, 0.66).
narrative_ontology:measurement(comm_su_t30, commerce_clause_scope__broad_effects_test, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(comm_su_t40, commerce_clause_scope__broad_effects_test, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(comm_su_t50, commerce_clause_scope__broad_effects_test, suppression_requirement, 50, 0.71).
narrative_ontology:measurement(comm_su_t60, commerce_clause_scope__broad_effects_test, suppression_requirement, 60, 0.71).
narrative_ontology:measurement(comm_su_t80, commerce_clause_scope__broad_effects_test, suppression_requirement, 80, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_scope__broad_effects_test, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(commerce_clause_scope__broad_effects_test, 0.18).
narrative_ontology:affects_constraint(commerce_clause_scope__broad_effects_test, commerce_clause_scope__narrow_originalist).
narrative_ontology:affects_constraint(commerce_clause_scope__broad_effects_test, commerce_clause_scope__intermediate_channels).
narrative_ontology:affects_constraint(commerce_clause_scope__broad_effects_test, dormant_commerce_clause_protectionism).
narrative_ontology:affects_constraint(commerce_clause_scope__broad_effects_test, state_police_power_preemption).

% DUAL FORMULATION NOTE:
% The commerce_clause_scope kernel decomposes into three structurally distinct constraints, one per reading: broad_effects_test (this story), narrow_originalist (commerce = trade crossing state lines), and intermediate_channels (allows effects doctrine with limiting principles). Each reading has a different ε (beneficiary/victim structure, scope of federal reach, state autonomy). All three are deployed simultaneously in current constitutional law—different courts and parties emphasize different readings depending on litigation strategy. They form a constraint family linked by the shared kernel; network edges track that dependency. Each story models only one reading's constraint and ε value; the kernel contest is documented via omega variables and cs_structure fields in each story, not collapsed into a single averaged constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(commerce_clause_scope__broad_effects_test, institutional, 0.05).
constraint_indexing:directionality_override(commerce_clause_scope__broad_effects_test, organized, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
