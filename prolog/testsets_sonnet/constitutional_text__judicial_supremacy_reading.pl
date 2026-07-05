% ============================================================================
% CONSTRAINT STORY: constitutional_text__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text__judicial_supremacy_reading, []).

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
 *   constraint_id: constitutional_text__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy Reading of Constitutional Interpretive Authority
 *   domain: constitutional_theory/political_philosophy/comparative_law
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the constitutional_text kernel:
 *   the judicial supremacy reading, under which courts possess final
 *   interpretive authority and judicial invalidation of legislation is the
 *   conclusive, non-overridable determination of constitutional meaning.
 *   Under this reading, legislative majorities cannot reverse a
 *   constitutional ruling through ordinary statute; only the (deliberately
 *   supermajoritarian, often practically unavailable) amendment process can.
 *   This reading coordinates a genuine function — protecting minority rights
 *   and structural limits against majoritarian overreach — while
 *   simultaneously extracting democratic responsiveness from legislative
 *   majorities and the electorate that empowers them. The sibling readings
 *   (legislative_sovereignty_reading, popular_sovereignty_reading) are NOT
 *   part of this constraint; they are separate constraints with their own ε
 *   values, generated separately and linked via network.affects_constraints.
 *   Under legislative sovereignty, courts advise but parliament retains
 *   override; under popular sovereignty, neither institution is supreme and
 *   the demos retains ultimate authority through amendment or constituent
 *   action. This story's ε (0.42) reflects the judicial supremacy reading
 *   specifically — its extraction is the gap between what legislatures enact
 *   and what survives judicial review, compounded over time as precedent
 *   accretes and doctrinal tests entrench.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text__judicial_supremacy_reading, 0.42).
domain_priors:suppression_score(constitutional_text__judicial_supremacy_reading, 0.55).
domain_priors:theater_ratio(constitutional_text__judicial_supremacy_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text__judicial_supremacy_reading, "Judicial Supremacy Reading of Constitutional Interpretive Authority").
narrative_ontology:topic_domain(constitutional_text__judicial_supremacy_reading, "constitutional_theory/political_philosophy/comparative_law").

domain_priors:requires_active_enforcement(constitutional_text__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text__judicial_supremacy_reading, 'd8e718f7-8867-4285-ba98-f7ea1c5bf1e5').
narrative_ontology:cs_kernel_codification('d8e718f7-8867-4285-ba98-f7ea1c5bf1e5', fixed_text).
narrative_ontology:cs_authority_grounding('d8e718f7-8867-4285-ba98-f7ea1c5bf1e5', lineage).
narrative_ontology:cs_interpretation_layer_present('d8e718f7-8867-4285-ba98-f7ea1c5bf1e5').
narrative_ontology:cs_reading_relation('d8e718f7-8867-4285-ba98-f7ea1c5bf1e5', constitutional_text__legislative_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('d8e718f7-8867-4285-ba98-f7ea1c5bf1e5', constitutional_text__popular_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('d8e718f7-8867-4285-ba98-f7ea1c5bf1e5', foundational, judicial_finality_over_constitutional_meaning).
narrative_ontology:cs_axiom_status(judicial_finality_over_constitutional_meaning, holdable).
narrative_ontology:cs_axiom_grounding('d8e718f7-8867-4285-ba98-f7ea1c5bf1e5', judicial_finality_over_constitutional_meaning, conventional).
narrative_ontology:cs_axiom('d8e718f7-8867-4285-ba98-f7ea1c5bf1e5', secondary, countermajoritarian_protection_justifies_override_immunity).
narrative_ontology:cs_axiom_status(countermajoritarian_protection_justifies_override_immunity, holdable).
narrative_ontology:cs_axiom_grounding('d8e718f7-8867-4285-ba98-f7ea1c5bf1e5', countermajoritarian_protection_justifies_override_immunity, instrumental).
narrative_ontology:cs_reference_frame('d8e718f7-8867-4285-ba98-f7ea1c5bf1e5', founding_era_countermajoritarian_check).
narrative_ontology:cs_drift_state('d8e718f7-8867-4285-ba98-f7ea1c5bf1e5', contemporary_judicial_review_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d8e718f7-8867-4285-ba98-f7ea1c5bf1e5', '').
narrative_ontology:cs_kernel_id(constitutional_text__judicial_supremacy_reading, constitutional_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text__judicial_supremacy_reading, rights_claimant_minorities).
narrative_ontology:constraint_beneficiary(constitutional_text__judicial_supremacy_reading, constitutional_court_judiciary).
narrative_ontology:constraint_beneficiary(constitutional_text__judicial_supremacy_reading, entrenched_property_and_contract_interests).
narrative_ontology:constraint_victim(constitutional_text__judicial_supremacy_reading, legislative_majorities).
narrative_ontology:constraint_victim(constitutional_text__judicial_supremacy_reading, democratic_electorate).
narrative_ontology:constraint_victim(constitutional_text__judicial_supremacy_reading, policy_reform_coalitions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds final say on whether legislation is constitutionally valid; invalidation cannot be overridden by ordinary legislative majority. Sets the doctrinal tests (strict scrutiny, proportionality, etc.) that determine outcomes in future cases. Faces no direct electoral accountability and controls the pace and scope of its own docket.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, constitutional_court_judiciary, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text__judicial_supremacy_reading, constitutional_court_judiciary, beneficiary).

% Groups without durable legislative majorities (racial minorities, dissenters, unpopular speech actors) can obtain protection by litigating rather than by winning votes. Judicial supremacy is their primary defense against majoritarian repeal of hard-won protections; without it they would depend entirely on shifting legislative coalitions.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, rights_claimant_minorities, beneficiary,
    powerless, biographical, constrained, national).

% Well-resourced economic actors use constitutional litigation (due process, takings, contract clauses) to entrench favorable arrangements against subsequent legislative revision. They can afford sustained, multi-year litigation strategies that outlast the political coalitions that produced adverse legislation.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, entrenched_property_and_contract_interests, beneficiary,
    organized, generational, mobile, national).

% Enacts statutes reflecting current electoral mandates, only to have them invalidated by unelected judges applying contestable doctrinal tests. Has no ordinary mechanism to override an adverse constitutional ruling short of the supermajority amendment process, which is designed to fail in ordinary politics. Bears the cost of policy paralysis when courts strike down enacted programs.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, legislative_majorities, payer,
    powerful, biographical, trapped, national).

% Votes for representatives and platforms that, once enacted, can be nullified by a court whose members were not chosen for this specific policy question and often serve without direct accountability. The electorate's expressed preference is subordinated to prior judicial doctrine or to the current bench's interpretation of text drafted generations earlier.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, democratic_electorate, payer,
    powerless, generational, trapped, national).

% Builds cross-party coalitions to pass structural reforms (labor law, redistribution, regulatory regimes) only to face constitutional challenge in courts that can strike the reform down using doctrine the coalition had no part in shaping. Must anticipate judicial review at the drafting stage, distorting policy design toward litigation-survivability rather than substantive effectiveness.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, policy_reform_coalitions, payer,
    organized, biographical, constrained, national).

% Argue that elected legislatures, not appointed judges, should have final constitutional say, and would prefer a notwithstanding-clause or override mechanism. Under this reading their position has no institutional foothold: the text itself is construed to foreclose legislative override, so their objection can be voiced in academic and political commentary but has no formal channel within the constitutional order as read here.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, legislative_sovereignty_advocates, excluded,
    organized, generational, trapped, national).

% Study comparative constitutional design, tracking how judicial supremacy regimes perform against legislative sovereignty and popular sovereignty alternatives across jurisdictions and historical periods, without a personal stake in any particular ruling.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, constitutional_theorists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_text__judicial_supremacy_reading, diffuse).
narrative_ontology:fixing_cost_class(constitutional_text__judicial_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, depoliticized forum for adjudicating whether legislative acts exceed constitutional limits, preventing transient majorities from stripping entrenched rights or restructuring fundamental governmental arrangements through ordinary lawmaking.
% TRANSFER_FUNCTION: Moves final say over the boundary of permissible legislation from elected legislatures (and the electorates behind them) to appointed or life-tenured judges; moves durability of protection from majority-vote-dependent to litigation-dependent for rights claimants and entrenched interests capable of sustaining constitutional litigation.
% ABSENT_VOICES: Legislative sovereignty advocates and popular sovereignty theorists would object that no textual necessity compels judicial finality — the text is also compatible with legislative override or constituent-power supremacy — but under this reading's own doctrine their objection is treated as a policy preference external to constitutional meaning, not as a live interpretive option.
% DISAPPEARANCE_RATIONALE: If judicial supremacy were abandoned overnight in favor of legislative override, currently entrenched rights protections would become subject to ordinary legislative repeal, litigation strategies by economic and rights-claimant actors would lose their trump-card function, and legislative majorities would regain the capacity to enact and sustain policy without anticipating judicial invalidation — the entire architecture of constitutional litigation, precedent-building, and rights advocacy would reorganize around legislative politics instead.
% FOUNDING_PROBLEM: Founding-era and postwar constitutional designers sought a check against majoritarian tyranny and legislative self-dealing — a mechanism to prevent transient majorities from entrenching their own power or stripping rights from disfavored minorities through ordinary statute.
% FOUNDING_PROBLEM_CORROBORATION: Sitting judges and constitutional-court scholars attest the countermajoritarian problem remains live, citing contemporary instances of majoritarian overreach against minorities. Comparative constitutional theorists and legislative-sovereignty jurisdictions (which report functioning rights protection via override-constrained parliamentary systems) attest that judicial finality is one contingent institutional solution among several, not a logical entailment of the founding problem — this corroboration comes from outside the benefiting judiciary and rights-claimant coalition.
narrative_ontology:disappearance_verdict(constitutional_text__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text__judicial_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text__judicial_supremacy_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(constitutional_text__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text__judicial_supremacy_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text__judicial_supremacy_reading_tests).
:- end_tests(constitutional_text__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) is moderate-to-substantial: judicial supremacy transfers real decisional power away from legislative majorities to courts, but the transfer is bounded by the coordination function it also performs (protecting minorities, providing interpretive stability). Suppression (0.55) reflects the structural closure of the override channel — legislative majorities have no ordinary-politics remedy for an adverse ruling, only the near-unusable amendment process, which is a genuine coercive asymmetry, not mere friction. Theater ratio is low (0.22) because judicial review under this reading performs substantial genuine work (real doctrinal tests are actually applied and actually bind outcomes) rather than being predominantly performative. Accessibility collapse (0.68) is high because once a matter is constitutionalized and adjudicated, alternative political remedies genuinely closed off for ordinary legislative actors. Resistance (0.58) reflects sustained political and academic contestation of judicial supremacy itself (court-packing debates, jurisdiction-stripping proposals, calls for legislative override) — this reading is actively defended, not passively accepted.
 *
 * PERSPECTIVAL GAP:
 *   From the judiciary's own seat, judicial supremacy is simply what the constitutional text requires — an analytical, almost mountain-like reading of institutional design. From a legislative majority's seat, the identical structure operates as an externally imposed veto on democratically mandated policy, with no path to contest the veto within ordinary politics. The engine should compute these as structurally different experiences of the same authored data, not reconcile them into a single verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   Rights-claimant minorities and entrenched economic interests are structural beneficiaries: judicial supremacy converts a legal victory into a durable, majority-proof entitlement, which is precisely why both disfavored minorities and well-resourced repeat litigants prize it. The constitutional judiciary itself benefits secondarily through institutional power accretion, even though it does not extract in a pecuniary sense. Legislative majorities and the electorate they represent are the structural targets: their expressed collective will can be nullified without recourse to ordinary politics. Policy reform coalitions are also targets — this reading forces them to design policy for litigation-survivability, a real cost even for coalitions that never lose a case, because it distorts drafting incentives at the front end.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (checking majoritarian tyranny) remains genuinely live in some domains (protecting electoral or expressive minorities from targeted majoritarian legislation) but is contested in others (routine economic and regulatory policy, where judicial invalidation increasingly serves entrenched interests rather than vulnerable minorities). Classifying this as tangled_rope rather than snare or mountain prevents two mislabeling errors: treating judicial supremacy as pure extraction would erase the genuine minority-protection function it performs; treating it as natural/inevitable (a mountain) would launder a contingent institutional design choice — one of at least three defensible readings of the same text — into an unchallengeable structural fact. The tangled_rope classification holds both truths: real coordination function, real and asymmetric extraction from legislative majorities, sustained by active judicial and academic enforcement of the finality doctrine.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_necessity_of_judicial_finality,
    'Does the constitutional text itself compel judicial finality, or is judicial supremacy a contingent institutional construction layered onto text that is equally compatible with legislative or popular sovereignty readings?',
    'Comparative textual analysis across constitutions with similar structural language but different institutional outcomes (e.g., jurisdictions with notwithstanding clauses built on textually similar rights guarantees); founding-era drafting history and ratification debates.',
    'If judicial finality is textually contingent rather than compelled, this reading''s claim to being the ''conclusive'' interpretation is itself an interpretive choice that could have gone otherwise — strengthening the case that this is one reading among several rather than the natural reading of the kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_necessity_of_judicial_finality, conceptual, 'Whether judicial supremacy is textually necessitated or one contingent institutional choice among defensible alternatives.').

omega_variable(
    countermajoritarian_function_drift,
    'Has the countermajoritarian protective function that originally justified judicial supremacy drifted toward protecting entrenched economic interests rather than vulnerable minorities, as the rising extractiveness trend in the measurement series suggests?',
    'Empirical tracking of case outcomes over time: proportion of invalidations protecting politically powerless groups versus protecting incumbent economic or institutional interests against redistributive or regulatory legislation.',
    'If the function has substantially drifted, the tangled_rope classification''s coordination component weakens over time relative to its extraction component, which would support reclassification toward snare in a future measurement window even though the founding coordination function was genuine at inception.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(countermajoritarian_function_drift, empirical, 'Whether judicial supremacy''s protective function has drifted from minority protection toward incumbent-interest entrenchment.').

omega_variable(
    cross_reading_legitimacy_contest,
    'Is the fact that reasonable constitutional democracies adopt each of the three sibling readings (judicial supremacy, legislative sovereignty, popular sovereignty) evidence that no reading is uniquely correct, or is it evidence only of path-dependent historical accident with one reading being genuinely superior?',
    'Longitudinal comparative study of rights outcomes, democratic responsiveness, and institutional stability across matched jurisdictions differing primarily in this dimension.',
    'If comparative outcomes show no systematic superiority for judicial supremacy jurisdictions, that undercuts any claim that this reading''s extraction from democratic responsiveness is justified by superior rights protection, sharpening the tangled_rope''s extraction component relative to its coordination claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cross_reading_legitimacy_contest, empirical, 'Whether comparative outcomes across kernel readings support or undercut judicial supremacy''s superior-protection justification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text__judicial_supremacy_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_text__judicial_supremacy_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cons_tr_t10, constitutional_text__judicial_supremacy_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(cons_tr_t20, constitutional_text__judicial_supremacy_reading, theater_ratio, 20, 0.14).
narrative_ontology:measurement(cons_tr_t30, constitutional_text__judicial_supremacy_reading, theater_ratio, 30, 0.16).
narrative_ontology:measurement(cons_tr_t40, constitutional_text__judicial_supremacy_reading, theater_ratio, 40, 0.18).
narrative_ontology:measurement(cons_tr_t50, constitutional_text__judicial_supremacy_reading, theater_ratio, 50, 0.2).
narrative_ontology:measurement(cons_tr_t60, constitutional_text__judicial_supremacy_reading, theater_ratio, 60, 0.22).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_text__judicial_supremacy_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(cons_be_t10, constitutional_text__judicial_supremacy_reading, base_extractiveness, 10, 0.31).
narrative_ontology:measurement(cons_be_t20, constitutional_text__judicial_supremacy_reading, base_extractiveness, 20, 0.34).
narrative_ontology:measurement(cons_be_t30, constitutional_text__judicial_supremacy_reading, base_extractiveness, 30, 0.36).
narrative_ontology:measurement(cons_be_t40, constitutional_text__judicial_supremacy_reading, base_extractiveness, 40, 0.39).
narrative_ontology:measurement(cons_be_t50, constitutional_text__judicial_supremacy_reading, base_extractiveness, 50, 0.4).
narrative_ontology:measurement(cons_be_t60, constitutional_text__judicial_supremacy_reading, base_extractiveness, 60, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_text__judicial_supremacy_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(cons_su_t10, constitutional_text__judicial_supremacy_reading, suppression_requirement, 10, 0.42).
narrative_ontology:measurement(cons_su_t20, constitutional_text__judicial_supremacy_reading, suppression_requirement, 20, 0.46).
narrative_ontology:measurement(cons_su_t30, constitutional_text__judicial_supremacy_reading, suppression_requirement, 30, 0.49).
narrative_ontology:measurement(cons_su_t40, constitutional_text__judicial_supremacy_reading, suppression_requirement, 40, 0.51).
narrative_ontology:measurement(cons_su_t50, constitutional_text__judicial_supremacy_reading, suppression_requirement, 50, 0.53).
narrative_ontology:measurement(cons_su_t60, constitutional_text__judicial_supremacy_reading, suppression_requirement, 60, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_text__judicial_supremacy_reading, legislative_sovereignty_reading).
narrative_ontology:affects_constraint(constitutional_text__judicial_supremacy_reading, popular_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language concept 'who has final say on constitutional meaning' (the constitutional_text kernel). judicial_supremacy_reading (this story), legislative_sovereignty_reading, and popular_sovereignty_reading each instantiate a structurally distinct constraint with different beneficiary/victim sets, different ε trajectories, and different classifications. They are linked as a constraint family via affects_constraints rather than merged into one story with an observable-selection parameter, per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
