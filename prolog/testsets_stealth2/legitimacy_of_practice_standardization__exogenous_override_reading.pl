% ============================================================================
% CONSTRAINT STORY: legitimacy_of_practice_standardization__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_practice_standardization__exogenous_override_reading, []).

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
 *   constraint_id: legitimacy_of_practice_standardization__exogenous_override_reading
 *   human_readable: State-Decree Practice Standardization Legitimacy (Exogenous Override Reading)
 *   domain: political_history/institutional_change
 *
 * SUMMARY:
 *   A modernizing state promulgates binding decrees replacing plural
 *   calendrical, sartorial, and ceremonial practice with a single official
 *   register, justified publicly by collective benefit: fiscal
 *   synchronization, administrative legibility, international alignment.
 *   Inspectors and gendarmerie back the decree with fines and license
 *   sanctions; official statistics report steadily rising compliance;
 *   meanwhile household accounts, parish registers, and festival rolls show
 *   rural communities running the traditional calendar in parallel for
 *   decades — a stable double life rather than a transitional phase. This
 *   file instantiates the exogenous_override_reading of the
 *   legitimacy_of_practice_standardization kernel and fixes epsilon's
 *   referent to the standing decree-and-enforce arrangement, assessed by that
 *   reading's own lights — never to any endorsed alternative. The sibling
 *   readings are separate constraint files linked through
 *   network.affects_constraints, with their structural deltas routed to omega
 *   variables. Claim and metrics are authored independently: the claim is
 *   tangled_rope because genuine fiscal and commercial coordination and an
 *   asymmetric rural-and-clerical burden coexist inside one enforced
 *   structure; the metrics describe observed operation, including a
 *   theater_ratio that crosses 0.5 late in the interval.
 *
 * KEY AGENTS:
 *   - central_state_bureaucracy: Primary agenda-setter and receipt seat (institutional/arbitrage) — decrees, enforces, converts its own ledgers first, captures fiscal gains
 *   - urban_merchants_and_exporters: Concentrated beneficiary (powerful/mobile) — petitioned for unification, harvests synchronized contracting
 *   - international_trade_partners: External beneficiary (institutional/arbitrage) — presses alignment, bears no domestic adjustment cost
 *   - enforcement_officials: Administering seat (organized/constrained) — runs inspections, inflates returns, did not design the policy
 *   - rural_peasant_households: Primary target (powerless/trapped) — pays fines and re-dating costs, sustains the hidden register
 *   - religious_clergy: Identity-locked target (organized/identity_locked) — liturgical office fused to the overridden calendar
 *   - village_ritual_specialists: Target and excluded voice (moderate/trapped) — criminalized in public, consulted in private
 *   - modernization_historians: Analytical observer (analytical/analytical) — reconstructs both registers from the record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_practice_standardization__exogenous_override_reading, 0.62).
domain_priors:suppression_score(legitimacy_of_practice_standardization__exogenous_override_reading, 0.58).
domain_priors:theater_ratio(legitimacy_of_practice_standardization__exogenous_override_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_practice_standardization__exogenous_override_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_practice_standardization__exogenous_override_reading, "State-Decree Practice Standardization Legitimacy (Exogenous Override Reading)").
narrative_ontology:topic_domain(legitimacy_of_practice_standardization__exogenous_override_reading, "political_history/institutional_change").

domain_priors:requires_active_enforcement(legitimacy_of_practice_standardization__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_practice_standardization__exogenous_override_reading, '20d96d8e-2853-42cf-8408-186940d5c4ed').
narrative_ontology:cs_kernel_codification('20d96d8e-2853-42cf-8408-186940d5c4ed', formalized).
narrative_ontology:cs_authority_grounding('20d96d8e-2853-42cf-8408-186940d5c4ed', lineage).
narrative_ontology:cs_interpretation_layer_present('20d96d8e-2853-42cf-8408-186940d5c4ed').
narrative_ontology:cs_reading_relation('20d96d8e-2853-42cf-8408-186940d5c4ed', legitimacy_of_practice_standardization__endogenous_displacement_reading, coexists_with).
narrative_ontology:cs_reading_relation('20d96d8e-2853-42cf-8408-186940d5c4ed', legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, influences).
narrative_ontology:cs_axiom('20d96d8e-2853-42cf-8408-186940d5c4ed', foundational, collective_benefit_decree_legitimizes_practice_change).
narrative_ontology:cs_axiom_status(collective_benefit_decree_legitimizes_practice_change, holdable).
narrative_ontology:cs_axiom_grounding('20d96d8e-2853-42cf-8408-186940d5c4ed', collective_benefit_decree_legitimizes_practice_change, instrumental).
narrative_ontology:cs_axiom('20d96d8e-2853-42cf-8408-186940d5c4ed', secondary, administrative_uniformity_precedes_spontaneous_consensus).
narrative_ontology:cs_axiom_status(administrative_uniformity_precedes_spontaneous_consensus, holdable).
narrative_ontology:cs_axiom_grounding('20d96d8e-2853-42cf-8408-186940d5c4ed', administrative_uniformity_precedes_spontaneous_consensus, empirically_contingent).
narrative_ontology:cs_reference_frame('20d96d8e-2853-42cf-8408-186940d5c4ed', state_decree_modernization_frame).
narrative_ontology:cs_drift_state('20d96d8e-2853-42cf-8408-186940d5c4ed', post_compliance_historiography, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('20d96d8e-2853-42cf-8408-186940d5c4ed', '').
narrative_ontology:cs_kernel_id(legitimacy_of_practice_standardization__exogenous_override_reading, legitimacy_of_practice_standardization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__exogenous_override_reading, central_state_bureaucracy).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__exogenous_override_reading, urban_merchants_and_exporters).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__exogenous_override_reading, international_trade_partners).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__exogenous_override_reading, rural_peasant_households).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__exogenous_override_reading, religious_clergy).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__exogenous_override_reading, village_ritual_specialists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafts and promulgates the decree, sets conversion deadlines, funds the inspectorate, and receives fine revenue plus the fiscal gains of a single dated year. Its own offices, payrolls, and ledgers convert immediately, so the ruling cadre bears almost none of the adjustment its rules impose on others; amendment or suspension of enforcement lies wholly in its hands.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, central_state_bureaucracy, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_practice_standardization__exogenous_override_reading, central_state_bureaucracy, beneficiary).

% Petitioned for unification. Gains contract dating, credit maturities, and shipping rotations synchronized with foreign correspondents; arbitrage across ports disciplines its exposure, and it can reroute trade through other jurisdictions if domestic terms sour.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, urban_merchants_and_exporters, beneficiary,
    powerful, biographical, mobile, national).

% Treaty counterparts and creditor states press alignment as a condition of loans, tariff terms, and recognition. They receive the coordination dividend of compatible dates and documents while bearing none of the domestic adjustment cost.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, international_trade_partners, beneficiary,
    institutional, generational, arbitrage, global).

% Local inspectors and gendarme detachments run market-day garment checks and shop-calendar audits, levy fines, and file compliance returns. Careers advance on reported compliance, so returns inflate. They administer a policy they did not design, apply it selectively against neighbors, and shoulder parts of it personally.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, enforcement_officials, agenda_setter,
    organized, biographical, constrained, regional).

% Must acquire prescribed garments for markets, courts, and census visits; re-date rents, debts, and feast obligations onto the official reckoning; and pay fines when inspectors encounter old-calendar use in public. Land tenure ties them to the jurisdiction. Households keep the traditional calendar alive indoors — planting dates, weddings, funerals, saints' days — sustaining two registered lives, official and domestic, across generations.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, rural_peasant_households, payer,
    powerless, biographical, trapped, local).

% Liturgical sequence, fasting periods, and feast days run on the traditional reckoning that the decree re-dates. Accommodation splits congregations; defiance invites license suspension and building closures. Office and calendar are fused: abandoning the old reckoning dissolves the vocation itself, so stepping out from under the rule means leaving the priesthood.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, religious_clergy, payer,
    organized, generational, identity_locked, national).

% Elders, almanac-keepers, and ritual schedulers whose communal timing expertise predates the decree. Public exercise of their function draws penalties, yet households still consult them privately. They held no seat in the drafting assembly; their objections survive only in petitions and constabulary reports.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, village_ritual_specialists, payer,
    moderate, generational, trapped, local).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_practice_standardization__exogenous_override_reading, village_ritual_specialists, excluded).

% Reconstruct the episode from enforcement dockets, household account books, parish registers, and festival attendance rolls to separate surface compliance from internalized change; their findings feed later policy debates. They hold no stake in either register.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, modernization_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimacy_of_practice_standardization__exogenous_override_reading, central_state_bureaucracy).
narrative_ontology:fixing_cost_class(legitimacy_of_practice_standardization__exogenous_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A single authoritative civil-fiscal calendar and public dress register solves real synchronization problems: tax-year boundaries, contract dating, court sessions, military logistics, and treaty and trade compatibility across a religiously and regionally heterogeneous population.
% TRANSFER_FUNCTION: Moves compliance cost and identity capital from rural households, clergy, and ritual specialists toward the center: prescribed-garment purchases, re-dating of obligations, fine revenue to the treasury, and the administrative-efficiency and international-credibility dividends accruing to the state and its export sectors.
% ABSENT_VOICES: Village ritual specialists, elderly calendar-keepers, and the women who schedule domestic and ritual life were absent from the drafting assembly; minority communities subject to the garment edicts had no representation. Their objection — that the private and ritual domain was never consulted — survives only in petitions and constabulary files. Commentary-grade: this recorded absence informs the consensus-provenance check, not any classification correction.
% DISAPPEARANCE_RATIONALE: Fiscal administration, debt instruments, court calendars, and treaty scheduling now presuppose the imposed standard; reverting would scramble records, contracts, and pension arithmetic built on it. The double-life equilibrium also depends on the legal boundary it straddles — remove the boundary and both registers lose their defining contrast.
% FOUNDING_PROBLEM: Plural calendrical and sartorial practice made the population administratively illegible: multiple new-year dates corrupted tax accounting, saints'-day reckonings clashed with fiscal quarters, and date mismatches impeded treaty negotiation and trade settlement. The arrangement was built to synchronize a heterogeneous population to state and international rhythms by fiat.
% FOUNDING_PROBLEM_CORROBORATION: Foreign consular correspondence and creditor-state memoranda independently document the pre-reform date-mismatch problem — attestation from parties outside the benefiting set. Fiscal-archive studies corroborate the accounting errors. By contrast, no source outside the promoting elites corroborates the dress-code rationale: its 'collective benefit' claim rests almost solely on the beneficiaries' own assertions, and that asymmetry is itself signal.
narrative_ontology:disappearance_verdict(legitimacy_of_practice_standardization__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_practice_standardization__exogenous_override_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_practice_standardization__exogenous_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legitimacy_of_practice_standardization__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_practice_standardization__exogenous_override_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_practice_standardization__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_of_practice_standardization__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimacy_of_practice_standardization__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness settles near 0.62: the opening decade carries shock costs (forced purchases, fines, re-dating of obligations), then the burden floors at the recurring cost of maintaining two registers rather than decaying to zero, because the hidden register never closes. Suppression (0.58) is authored as the raw structural property it is — unscaled by power or scope; only extractiveness gets context scaling in the engine's computation. The suppression_requirement series traces the enforcement arc the story turns on: machinery ramps steeply through the revolt-suppression decade (0.45 to 0.75), then decays toward 0.58 as enforcement shifts from compelled conversion to routine inspection — a genuine build-up-then-erosion of enforcement capacity, hence its inclusion alongside the scalar. Theater_ratio climbs monotonically from 0.22 to 0.55, crossing 0.5 around t=30: early enforcement pursued real conversion; late enforcement increasingly stages compliance — inflated returns, ceremonial inspections, anniversary statistics — while behaviorally decisive practice migrated underground. That crossing is the Goodhart signature of proxy substitution in the enforcement layer. Accessibility_collapse is low (0.40): the traditional alternative never vanished, it went indoors — alternatives were displaced, not destroyed. Resistance is substantial (0.60): armed local revolts in the garment-edict districts, clerical petitions, and decades of quiet non-observance. Seasonal enforcement spikes around festival periods exist but sit below the decadal measurement resolution; the series is deliberately on one shared time grid, every tracked metric authored at every point {0,10,20,30,40,50}.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the ministry seat the arrangement is nation-building coordination it designed, funded, and converted itself onto first — costs borne elsewhere, benefits banked centrally. From the peasant seat the same structure is a penalized parallelism: nominal membership in the official register purchased with fines and garment outlays, lived reality elsewhere. The clergy seat adds identity fusion — office and calendar are one thing, so the rule reaches the person, not just the practice. Merchants sit near indifference-to-benefit with mobile exit; foreign partners collect the dividend at zero domestic cost. Enforcement officials occupy an intermediate seat: administering a policy whose reported success their careers require, which biases the very statistics any observer would use. The engine derives this divergence from power, exit, and role data; nothing in the authored claim adjudicates it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations push the bureaucracy (self-exempted, arbitrage-grade control of the rules), merchants (mobile), and foreign partners (zero-cost arbitrage) toward the subsidy end of directionality — the bureaucracy furthest, since it wrote the deadline everyone else must meet. Victim declarations push rural households (trapped, land-bound) near the full-target end; identity_lock amplifies the clergy seat toward the target pole despite its organized power — fusion of vocation and calendar removes the exit the derivation would otherwise credit; ritual specialists are trapped and additionally excluded from the conversation that re-dated their function. Enforcement officials carry no beneficiary or victim declaration and take the canonical fallback, which suits their intermediate position; no directionality overrides are authored because the structural derivation already separates the seats correctly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding synchronization problem was substantially solved within roughly a decade of conversion — the fiscal and treaty-alignment gains were banked early — while enforcement machinery operated for decades beyond that point, its activity increasingly theatrical. The classification discipline matters in both directions here. A pure-coordination reading would hide the asymmetric, identity-reaching burden on rural and clerical seats; a pure-predation reading would erase the real coordination dividend that merchants, creditors, and the treasury demonstrably collect and that removal would forfeit. The honest structure is layered: the standard itself remains load-bearing (the world rearranges without it), while the enforcement superstructure shows classic residue symptoms — rising theater, decaying necessity, careers fed on reported compliance. Hence mandatrophy_resolved is authored false for the constraint as a whole, with the mandate question concentrated in the enforcement layer rather than the standard.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contingency,
    'This constraint is one reading of the practice-standardization legitimacy kernel (exogenous_override_reading) — how would instantiating a sibling reading change the structural classification?',
    'Compile the sibling files (endogenous_displacement_reading, dual_practice_equilibrium_reading) and compare computed per-seat types. The disagreement is located in the SOURCE of practice-legitimacy: decree versus voluntary utility-driven adoption versus domain partition between state and traditional authority.',
    'Under the endogenous reading the target set collapses to outright-coerced minorities and epsilon drops toward the coordination floor; under the dual-practice reading the private-domain burden vanishes from the ledger entirely and the arrangement approaches a plain coordination mechanism. This file''s values are valid only for the decree-legitimacy reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contingency, conceptual, 'Reading-indexed classification of a contested legitimacy kernel; sibling readings instantiate different constraints.').

omega_variable(
    double_life_permanence,
    'Is the rural double life a transitional phase that cohort replacement will close, or a stable equilibrium persisting indefinitely?',
    'Cohort-replacement analysis of rural calendar use: if younger households internalize the official reckoning for domestic purposes as elders die off, the dual register closes within a generation or two; if each cohort re-learns the traditional calendar for planting and rites, the equilibrium is self-reproducing.',
    'If transitional, the arrangement resembles temporary support approaching a sunset and late-interval enforcement is pure residue; if stable, the dual-register burden is a permanent feature and late-period enforcement theater persists indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(double_life_permanence, empirical, 'Whether the observed surface-compliance equilibrium converges or reproduces across cohorts.').

omega_variable(
    compliance_measurement_gap,
    'How far do official compliance statistics diverge from actual household practice?',
    'Cross-validate inspectorate returns against household account books, parish and mosque registers, festival attendance rolls, and marriage/burial dating practices.',
    'A wide gap means the measured theater ratio understates performance depth, reported extraction is biased downward by the enforcement apparatus''s own incentive to inflate success, and any certification resting on official statistics inherits the bias.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_measurement_gap, empirical, 'Divergence between enforced-reporting statistics and behavioral evidence of continued traditional practice.').

omega_variable(
    component_decomposition_ambiguity,
    'Are the calendar mandate and the dress mandate one constraint or two, given they plausibly carry different intrinsic burdens?',
    'Natural experiments from jurisdictions that imposed calendar unification without garment edicts (and vice versa): estimate each component''s coordination floor and target overlap separately.',
    'If separated, the calendar component computes closer to a coordination mechanism with modest excess burden, while the dress component — thin coordination justification, heavy identity cost — computes markedly more extractive; the single-story epsilon blends these and the blend is the price of the merged label.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(component_decomposition_ambiguity, conceptual, 'Possible epsilon-invariance decomposition inside the standardization package.').

omega_variable(
    cs_authority_grounding_framing,
    'Is the state''s authority over the standardization kernel grounded in constitutional-continuity lineage, or in extraction — switching costs that lock fiscal advantage behind the imposed standard?',
    'Compare framings against the decree chain and interpretive practice: courts and ministries absorb drift through administrative reinterpretation (supports lineage with an interpretation layer); treasury resistance to any reversal proposal regardless of fiscal-neutral redesigns (supports extraction framing).',
    'Under the extraction framing the authority structure reads as profiting from kernel stability and drift denial, shifting the commitment-system diagnosis; under the lineage framing it reads as a transmission chain interpreting a formalized kernel. Both framings are coherent; the choice changes the cs_pattern outcome.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_authority_grounding_framing, conceptual, 'Commitment-system framing under-determination for the decree regime''s authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_practice_standardization__exogenous_override_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(legi_tr_t10, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 10, 0.34).
narrative_ontology:measurement(legi_tr_t20, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 20, 0.44).
narrative_ontology:measurement(legi_tr_t30, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 30, 0.5).
narrative_ontology:measurement(legi_tr_t40, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 40, 0.53).
narrative_ontology:measurement(legi_tr_t50, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 50, 0.55).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(legi_be_t10, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 10, 0.71).
narrative_ontology:measurement(legi_be_t20, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(legi_be_t30, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(legi_be_t40, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement(legi_be_t50, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 50, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(legi_su_t10, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 10, 0.72).
narrative_ontology:measurement(legi_su_t20, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 20, 0.75).
narrative_ontology:measurement(legi_su_t30, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(legi_su_t40, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 40, 0.62).
narrative_ontology:measurement(legi_su_t50, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_practice_standardization__exogenous_override_reading, information_standard).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__exogenous_override_reading, legitimacy_of_practice_standardization__endogenous_displacement_reading).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__exogenous_override_reading, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'standardization reform' covers three structurally distinct legitimacy claims, written as three files sharing the kernel id. This (exogenous_override) reading is the upstream member historically — decree regimes acted on it first, and the compliance record they produced is the evidentiary environment the downstream readings (endogenous displacement, dual-practice equilibrium) interpret. Its epsilon is fixed to the decree-enforced arrangement; the siblings carry their own epsilon, victim sets, and classifications, and the differences are documented in the omega variables of each file.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
