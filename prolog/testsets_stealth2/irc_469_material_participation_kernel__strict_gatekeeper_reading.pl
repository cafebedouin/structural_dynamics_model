% ============================================================================
% CONSTRAINT STORY: irc_469_material_participation_kernel__strict_gatekeeper_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_irc_469_material_participation_kernel__strict_gatekeeper_reading, []).

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
 *   constraint_id: irc_469_material_participation_kernel__strict_gatekeeper_reading
 *   human_readable: Strict Gatekeeper Reading of IRC Section 469 Material Participation
 *   domain: economic/legal-regulatory
 *
 * SUMMARY:
 *   Section 469 of the Internal Revenue Code bars losses from passive
 *   activities from offsetting salaries and portfolio income unless the owner
 *   materially participates. This story authors the strict gatekeeper reading
 *   of that rule as it actually operates: participation is accepted only when
 *   proven by verifiable, substantial personal labor - contemporaneous logs,
 *   appointment books, calendars - and reconstructed estimates carry
 *   essentially no weight on examination. The reading narrows the qualifying
 *   population, imposes heavy documentation friction, and leaves most passive
 *   losses suspended against ordinary income. It retains a genuine
 *   coordination function (a single administrable evidentiary standard for an
 *   anti-abuse statute) while placing asymmetric costs on small operators
 *   whose labor is real but thinly documented. This is one of two readings of
 *   the material participation kernel; the sibling strategic_shelter_reading
 *   is authored separately and linked through the network block. KEY AGENTS
 *   (by structural relationship): - treasury_irs_enforcement: Primary
 *   beneficiary and agenda-setter (institutional/arbitrage) - writes the
 *   evidentiary expectations, examines against them, and receives disallowed
 *   deductions as revenue - small_residential_landlords: Primary target
 *   (moderate/constrained) - genuine personal labor, thin documentation,
 *   suspended losses - high_income_passive_investors: Secondary target
 *   (powerful/arbitrage) - the screen's intended object, able to restructure
 *   around it - documented_real_estate_professionals: Secondary beneficiary
 *   (organized/mobile) - clears the bar professionally and gains from its
 *   scarcity effect - tax_advisory_compliance_industry: Secondary beneficiary
 *   (organized/mobile) - sells the documentation the bar demands -
 *   tax_court_judiciary: Analytical observer (institutional/analytical) -
 *   case law fixes what verifiable means -
 *   prospective_small_scale_housing_providers: Excluded voice
 *   (powerless/mobile) - deterred before entry, unrepresented in guidance
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0.58).
domain_priors:suppression_score(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0.45).
domain_priors:theater_ratio(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(irc_469_material_participation_kernel__strict_gatekeeper_reading, tangled_rope).
narrative_ontology:human_readable(irc_469_material_participation_kernel__strict_gatekeeper_reading, "Strict Gatekeeper Reading of IRC Section 469 Material Participation").
narrative_ontology:topic_domain(irc_469_material_participation_kernel__strict_gatekeeper_reading, "economic/legal-regulatory").

domain_priors:requires_active_enforcement(irc_469_material_participation_kernel__strict_gatekeeper_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(irc_469_material_participation_kernel__strict_gatekeeper_reading, '9168e6a5-7a0c-4dd9-aba5-540f146dc185').
narrative_ontology:cs_kernel_codification('9168e6a5-7a0c-4dd9-aba5-540f146dc185', fixed_text).
narrative_ontology:cs_authority_grounding('9168e6a5-7a0c-4dd9-aba5-540f146dc185', expertise).
narrative_ontology:cs_interpretation_layer_present('9168e6a5-7a0c-4dd9-aba5-540f146dc185').
narrative_ontology:cs_reading_relation('9168e6a5-7a0c-4dd9-aba5-540f146dc185', irc_469_material_participation_kernel__strategic_shelter_reading, coexists_with).
narrative_ontology:cs_axiom('9168e6a5-7a0c-4dd9-aba5-540f146dc185', foundational, tax_benefits_track_verified_conduct).
narrative_ontology:cs_axiom_status(tax_benefits_track_verified_conduct, holdable).
narrative_ontology:cs_axiom_grounding('9168e6a5-7a0c-4dd9-aba5-540f146dc185', tax_benefits_track_verified_conduct, conventional).
narrative_ontology:cs_axiom('9168e6a5-7a0c-4dd9-aba5-540f146dc185', secondary, hour_estimation_insufficient_for_gatekeeping).
narrative_ontology:cs_axiom_status(hour_estimation_insufficient_for_gatekeeping, holdable).
narrative_ontology:cs_axiom_grounding('9168e6a5-7a0c-4dd9-aba5-540f146dc185', hour_estimation_insufficient_for_gatekeeping, conventional).
narrative_ontology:cs_reference_frame('9168e6a5-7a0c-4dd9-aba5-540f146dc185', verified_personal_labor_prerequisite).
narrative_ontology:cs_drift_state('9168e6a5-7a0c-4dd9-aba5-540f146dc185', contemporary_short_term_rental_exception_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('9168e6a5-7a0c-4dd9-aba5-540f146dc185', '').
narrative_ontology:cs_kernel_id(irc_469_material_participation_kernel__strict_gatekeeper_reading, irc_469_material_participation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strict_gatekeeper_reading, treasury_irs_enforcement).
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strict_gatekeeper_reading, documented_real_estate_professionals).
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strict_gatekeeper_reading, tax_advisory_compliance_industry).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strict_gatekeeper_reading, small_residential_landlords).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strict_gatekeeper_reading, high_income_passive_investors).
narrative_ontology:constraint_vindicates(irc_469_material_participation_kernel__strict_gatekeeper_reading, anti_shelter_purpose_of_tax_reform_act_1986).
narrative_ontology:constraint_vindicates(irc_469_material_participation_kernel__strict_gatekeeper_reading, strict_substantiation_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Publishes the regulations, rulings, and audit guidance that define what counts as material participation and what records suffice to prove it. Selects returns for examination based on passive-loss claims, discounts reconstructed hour estimates, and assesses the revenue difference when logs fail inspection, sometimes with accuracy-related penalties. It can revise sub-regulatory guidance or audit emphasis at will and bears none of the documentation burden itself.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, treasury_irs_enforcement, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(irc_469_material_participation_kernel__strict_gatekeeper_reading, treasury_irs_enforcement, beneficiary).

% Own one to a handful of units and personally handle repairs, tenant turnover, and bookkeeping, usually without contemporaneous time logs. On examination, their reconstructed estimates of hours carry little weight, so losses they economically earned are suspended year after year. Many respond by stopping loss claims altogether or by selling; selling means transaction costs, possible depreciation recapture, and giving up an income stream they built.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, small_residential_landlords, payer,
    moderate, biographical, constrained, national).

% Hold interests in rental ventures and limited partnerships alongside large salaries and portfolio income, and would apply suspended losses against that ordinary income if the gate opened. They can restructure around the gate: shifting capital to portfolio assets, using grouping elections, or buying into short-term-rental structures that sidestep the participation tests entirely. The gate redirects their planning rather than trapping them.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, high_income_passive_investors, payer,
    powerful, biographical, arbitrage, global).

% Work in real estate as agents, brokers, developers, or full-time operators and maintain professional-grade time records as a matter of course. They clear the participation thresholds comfortably, and the difficulty others face in doing the same protects the scarcity value of their active status and the deductions attached to it.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, documented_real_estate_professionals, beneficiary,
    organized, biographical, mobile, national).

% Accountants, tax attorneys, and software vendors sell time-log systems, grouping-election strategy, real-property-professional status studies, and examination defense. Revenue scales with the height of the documentation bar: every tightening of evidentiary expectations converts directly into billable preparation and defense work.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, tax_advisory_compliance_industry, beneficiary,
    organized, biographical, mobile, national).

% Adjudicates disputes over whether particular records prove participation, and its published opinions - repeatedly declining to estimate hours from incomplete evidence - fix the practical meaning of verifiable. It decides cases rather than setting policy and takes no side in the underlying dispute.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, tax_court_judiciary, observer,
    institutional, generational, analytical, national).

% Would buy and personally manage small rental properties but have not yet entered the market; the anticipated record-keeping burden and examination risk figure into their decision to stay out. They appear nowhere in the guidance process, which runs through comment channels dominated by institutional filers and advisory firms.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, prospective_small_scale_housing_providers, excluded,
    powerless, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(irc_469_material_participation_kernel__strict_gatekeeper_reading, treasury_irs_enforcement).
narrative_ontology:fixing_cost_class(irc_469_material_participation_kernel__strict_gatekeeper_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Supplies a common evidentiary threshold that separates genuinely active participants from passive investors, letting millions of self-reported returns be prepared and checked against one administrable standard instead of case-by-case inquiries into who really works.
% TRANSFER_FUNCTION: Moves the cash value of suspended and disallowed losses - together with penalty exposure and the price of documentation itself - from taxpayers claiming active status to the federal fisc, and moves fee income from examined taxpayers to the accounting and legal professions.
% ABSENT_VOICES: Prospective small-scale housing providers deterred before entry, and first-time owners facing their first examination, have no seat in the interpretive process; comment letters and hearings on section 469 guidance draw almost entirely from institutional filers and the advisory industry. Tenants, affected second-order through rental supply and maintenance decisions, are absent entirely.
% DISAPPEARANCE_RATIONALE: If the strict gate vanished overnight, hour-counting would become effectively self-certified, previously suspended losses would flood against salaries and portfolio income, Treasury revenue would fall, the advisory industry's documentation practice would shrink, and the population of taxpayers treated as active participants would widen sharply - the surrounding planning economy would reorganize within a few filing seasons.
% FOUNDING_PROBLEM: Before 1986, mass-marketed tax shelters sold paper losses from limited partnerships and rental ventures that professionals used to offset salaries and investment income; Congress enacted section 469 in the Tax Reform Act of 1986 to stop losses from reaching people who do not work for them.
% FOUNDING_PROBLEM_CORROBORATION: The shelter-specific form of the founding problem is corroborated closed by the collapse of the mass-marketed partnership shelter industry after 1986, per Joint Committee on Taxation histories and contemporaneous Treasury testimony. That the line-drawing problem remains live is attested from outside the benefiting parties by continuing Tax Court dockets over participation proof, Government Accountability Office reviews of the passive-loss rules, and academic tax scholarship treating the active/passive boundary as unresolved - none of whom collect from the gate.
narrative_ontology:disappearance_verdict(irc_469_material_participation_kernel__strict_gatekeeper_reading, world_rearranges).
narrative_ontology:founding_problem_status(irc_469_material_participation_kernel__strict_gatekeeper_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(irc_469_material_participation_kernel__strict_gatekeeper_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(irc_469_material_participation_kernel__strict_gatekeeper_reading, 'none', 1).
narrative_ontology:epsilon_provenance(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(irc_469_material_participation_kernel__strict_gatekeeper_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(irc_469_material_participation_kernel__strict_gatekeeper_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(irc_469_material_participation_kernel__strict_gatekeeper_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate-high (0.58 at interval end) because the gate converts economically real losses into permanently suspended carryforwards for a large class of owners, and because the documentation bar prices out precisely the operators least equipped to meet it. Suppression is moderate (0.45) and is authored as a raw structural property - the engine scales only extractiveness by directionality and scope; suppression here consists of audit-risk defaults and evidentiary rules that make estimation impractical, not of barred physical alternatives, since other qualification paths (additional tests, grouping elections, the short-term-rental exception) remain open at a price. Theater is moderate (0.40): a growing share of documentation activity is performed for the file rather than for the property - logs kept to survive examination rather than to run buildings. Accessibility collapse is moderate (0.50) because alternatives persist but each carries its own friction; resistance is moderate (0.50), expressed through litigation, practitioner criticism, and recurring legislative proposals rather than open defiance. The temporal series run on one shared grid (points roughly 1986, 1992, 1998, 2004, 2010, 2016, 2022) so every tracked metric is authored at every examined time point; the rising base_extractiveness trajectory documents a ratchet in which each tightening of substantiation expectations layers compliance cost onto the original screen, and the rising suppression_requirement series tracks the deliberate maturation of enforcement capacity (temporary regulations, final regulations, the no-estimation case-law line, targeted audit campaigns) rather than a static enforcement picture.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the agenda-setter seat, the gate is a product the Treasury built and maintains, and its costs are invisible from inside. From the trapped small-operator seat, the same structure operates as an evidentiary trap: real work, unrewarded for want of paper, with exit priced at recapture and sale. From the arbitrage-class investor seat, it is a routing cost - annoying, plannable-around, and partly self-inflicted by the desire to shelter salary income. The beneficiary seats experience protection (professionals) and revenue (advisors) where the payer seats experience denial. Coalition potential among small landlords exists in principle but is blunted by dispersed holdings and heterogeneous facts, which is why their resistance registers as attrition (stopped claims, sales) rather than organized pressure.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: the Treasury seat both administers and collects (d near the beneficiary end, amplified by arbitrage-grade control over its own rules); professionals and advisors collect without running the gate (d near zero, mobile exits damping further). Victim declarations drive high directionality: small landlords sit near the full-target end, pushed there by constrained exit - their losses are stuck and their capital is illiquid. High-income passive investors are declared victims but their arbitrage-grade exit pulls their effective directionality back toward the middle: the gate redirects their capital rather than confining it, so they bear real but elastic costs. The observer and excluded seats feed no directional arithmetic - the judiciary decides cases without collecting, and the deterred entrant stands outside the arrangement the story measures.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem in its original form - the mass-marketed shelter industry - is dead, corroborated from outside the beneficiary set; the gate persists because the boundary problem it was built to police recurs every filing season in new clothing. Classifying this as tangled_rope rather than snare keeps both truths visible: the screen is real (an anti-abuse statute needs some evidentiary standard, and without one the passive-loss limitation is unadministerable), and the friction is real and unevenly placed (it falls hardest on those with the thinnest record infrastructure). Calling it pure coordination would erase the concentrated payers; calling it pure extraction would erase the coordination function that makes the statute workable. Mandatrophy is not resolved - the mandate is live - but the measurement series marks the drift vector: if theater and extractiveness continue climbing while the qualifying population keeps narrowing, the arrangement is migrating toward maintenance of the barrier itself rather than the screen it justifies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_delta_strategic_shelter,
    'This constraint is the strict_gatekeeper_reading of kernel irc_469_material_participation_kernel; what changes structurally if the sibling strategic_shelter_reading is instantiated instead?',
    'Compile the sibling story and compare qualifying-population width, compliance-friction load, and the volume of passive losses reaching ordinary income across the two files.',
    'Under the sibling reading the qualifying population widens, documentation friction drops, and passive losses reach ordinary income far more often; the extraction locus shifts from compliance friction on operators toward shelter-enabled revenue loss at the Treasury, moving the computed classification toward lighter coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_delta_strategic_shelter, conceptual, 'Committer structure: this story is one of two readings of the section 469 material participation kernel; the delta names what the sibling reading would change.').

omega_variable(
    documentation_bar_separability,
    'Is the high documentation bar necessary to the gate''s screening function, or could statistical and sampling-based verification achieve comparable screening at far lower taxpayer friction?',
    'Compare improper-claim rates and examination outcomes across eras and administrative regimes with differing substantiation intensity; treat any future administrative softening as a natural experiment.',
    'If the functions are separable, a large share of measured extraction is avoidable overhead riding on a cheaply replicable screen, supporting movement toward the extractive end; if inseparable, the friction is the price of the coordination itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(documentation_bar_separability, empirical, 'Whether the evidentiary height of the gate is load-bearing for its screening function or separable overhead.').

omega_variable(
    incidence_by_operator_scale,
    'Does the documentation bar bind uniformly across operator sizes, or does it concentrate disallowance and suspension on small operators lacking record infrastructure?',
    'Examination and disallowance statistics segmented by return profile and entity size, plus practitioner surveys of documentation capacity by client scale.',
    'Concentration on small operators implies regressive placement of costs within the payer class and sharpens the coordination/extraction asymmetry; uniform incidence would support a purer coordination reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incidence_by_operator_scale, empirical, 'Whether the gate''s costs are evenly distributed or scale-regressive across the governed population.').

omega_variable(
    internalized_overdocumentation,
    'How much of observed documentation behavior reflects the formal evidentiary requirement versus internalized practitioner caution that exceeds it?',
    'Survey preparers and counsel on what they believe is required versus what controlling case law actually demands; compare litigation outcomes for estimated records against prevailing practice.',
    'If heavily internalized, the constraint''s effective coercive force exceeds the formal bar and would persist even after formal loosening - the suppression lives in practice norms rather than text, and the omega feeds the structural-versus-internalized suppression ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_overdocumentation, empirical, 'Split of the documentation burden between formal requirement and internalized professional caution.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(irc__tr_t0, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(irc__tr_t0, observed).
narrative_ontology:measurement(irc__tr_t6, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 6, 0.26).
narrative_ontology:measurement_basis(irc__tr_t6, observed).
narrative_ontology:measurement(irc__tr_t12, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 12, 0.29).
narrative_ontology:measurement_basis(irc__tr_t12, observed).
narrative_ontology:measurement(irc__tr_t18, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 18, 0.32).
narrative_ontology:measurement_basis(irc__tr_t18, observed).
narrative_ontology:measurement(irc__tr_t24, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 24, 0.35).
narrative_ontology:measurement_basis(irc__tr_t24, observed).
narrative_ontology:measurement(irc__tr_t30, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement_basis(irc__tr_t30, observed).
narrative_ontology:measurement(irc__tr_t36, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 36, 0.4).
narrative_ontology:measurement_basis(irc__tr_t36, observed).

% Extraction over time
narrative_ontology:measurement(irc__be_t0, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement_basis(irc__be_t0, observed).
narrative_ontology:measurement(irc__be_t6, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 6, 0.44).
narrative_ontology:measurement_basis(irc__be_t6, observed).
narrative_ontology:measurement(irc__be_t12, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 12, 0.48).
narrative_ontology:measurement_basis(irc__be_t12, observed).
narrative_ontology:measurement(irc__be_t18, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 18, 0.51).
narrative_ontology:measurement_basis(irc__be_t18, observed).
narrative_ontology:measurement(irc__be_t24, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 24, 0.54).
narrative_ontology:measurement_basis(irc__be_t24, observed).
narrative_ontology:measurement(irc__be_t30, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 30, 0.56).
narrative_ontology:measurement_basis(irc__be_t30, observed).
narrative_ontology:measurement(irc__be_t36, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 36, 0.58).
narrative_ontology:measurement_basis(irc__be_t36, observed).

% Suppression requirement over time
narrative_ontology:measurement(irc__su_t0, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(irc__su_t0, observed).
narrative_ontology:measurement(irc__su_t6, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 6, 0.33).
narrative_ontology:measurement_basis(irc__su_t6, observed).
narrative_ontology:measurement(irc__su_t12, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 12, 0.36).
narrative_ontology:measurement_basis(irc__su_t12, observed).
narrative_ontology:measurement(irc__su_t18, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 18, 0.39).
narrative_ontology:measurement_basis(irc__su_t18, observed).
narrative_ontology:measurement(irc__su_t24, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 24, 0.41).
narrative_ontology:measurement_basis(irc__su_t24, observed).
narrative_ontology:measurement(irc__su_t30, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 30, 0.43).
narrative_ontology:measurement_basis(irc__su_t30, observed).
narrative_ontology:measurement(irc__su_t36, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 36, 0.45).
narrative_ontology:measurement_basis(irc__su_t36, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(irc_469_material_participation_kernel__strict_gatekeeper_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(irc_469_material_participation_kernel__strict_gatekeeper_reading, irc_469_material_participation_kernel__strategic_shelter_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the material participation test' decomposes into two structurally distinct constraints sharing one statutory text: this strict-evidentiary gate (high friction, narrow qualifying population, passive losses rarely reaching ordinary income) and the sibling permissive-threshold reading (low friction, wide qualifying population, losses routinely usable against ordinary income). Their epsilon values differ because the operative arrangements differ in who qualifies and what proof costs - changing the observable changes the constraint, so the label is split into two stories linked as a constraint family rather than merged into one observable-dependent classification. The upstream reading cited as settled administrative practice lends legitimacy to the downstream planning literature, and vice versa.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
