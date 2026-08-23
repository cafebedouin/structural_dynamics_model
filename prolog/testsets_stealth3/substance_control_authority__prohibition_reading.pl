% ============================================================================
% CONSTRAINT STORY: substance_control_authority__prohibition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_authority__prohibition_reading, []).

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
 *   constraint_id: substance_control_authority__prohibition_reading
 *   human_readable: Criminal Prohibition of Drug Use and Possession (Third-Party Protection Reading)
 *   domain: public health policy / criminal justice / political economy
 *
 * SUMMARY:
 *   Statutes in most jurisdictions criminalize the possession and use of
 *   defined psychoactive substances, and enforcement of those statutes —
 *   patrol, arrest, prosecution, incarceration — is justified as the state's
 *   protection of third parties from drug-related crime and social disorder.
 *   The arrangement's operation concentrates on users and on the
 *   neighborhoods where drug markets are most visible: simple possession
 *   accounts for a large share of drug arrests, incarceration is the primary
 *   mechanism, and application falls disproportionately on minority
 *   communities relative to measured use. A real public-order problem stands
 *   behind the arrangement — visible markets, property crime, and violence
 *   that residents cannot suppress alone — and residents' demand for order is
 *   independently corroborated; the arrangement supplies that order through
 *   the criminal law while transferring its heaviest costs onto the conduct
 *   it defines as criminal and the communities where enforcement lands. Claim
 *   and metrics are authored independently: the claimed type is tangled_rope
 *   from the authoring seat (a genuine coordination demand met through a
 *   structure that also concentrates heavy asymmetric costs), and the metrics
 *   describe the arrangement's observed operation without being tuned to any
 *   predicted engine verdict.
 *
 * KEY AGENTS:
 *   - state_legislative_and_executive_branch: agenda setter (institutional/arbitrage) — enacts and maintains the criminal framework, claims the protective warrant
 *   - police_and_prosecutorial_agencies: primary collector (institutional/constrained) — administers enforcement, captures budgets, overtime, and forfeiture
 *   - correctional_systems: secondary collector (institutional/constrained) — houses the incarcerated inflow, capacity scales with it
 *   - residents_of_disorder_affected_neighborhoods: dual-positioned third party (moderate/constrained) — receives order benefits, bears over-policing costs
 *   - drug_users: primary target (powerless/trapped) — bears criminalization directly, exit foreclosed by dependence and record
 *   - minority_communities_subject_to_disparate_enforcement: concentrated target (moderate/constrained) — bears enforcement at rates above their share of use
 *   - families_of_incarcerated_people: collateral target (powerless/trapped) — absorb the household costs of removal
 *   - harm_reduction_practitioners: excluded voice (moderate/constrained) — operate under legal constraint of the same statutes
 *   - policy_researchers: analytical observer (moderate/analytical) — measures the arrangement's effects without a vote
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_authority__prohibition_reading, 0.72).
domain_priors:suppression_score(substance_control_authority__prohibition_reading, 0.85).
domain_priors:theater_ratio(substance_control_authority__prohibition_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_authority__prohibition_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_authority__prohibition_reading, "Criminal Prohibition of Drug Use and Possession (Third-Party Protection Reading)").
narrative_ontology:topic_domain(substance_control_authority__prohibition_reading, "public health policy / criminal justice / political economy").

domain_priors:requires_active_enforcement(substance_control_authority__prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_authority__prohibition_reading, '85d70685-dfb7-41d9-a6f7-455c1cf74d44').
narrative_ontology:cs_kernel_codification('85d70685-dfb7-41d9-a6f7-455c1cf74d44', formalized).
narrative_ontology:cs_authority_grounding('85d70685-dfb7-41d9-a6f7-455c1cf74d44', lineage).
narrative_ontology:cs_interpretation_layer_present('85d70685-dfb7-41d9-a6f7-455c1cf74d44').
narrative_ontology:cs_reading_relation('85d70685-dfb7-41d9-a6f7-455c1cf74d44', substance_control_authority__legalization_reading, forecloses).
narrative_ontology:cs_reading_relation('85d70685-dfb7-41d9-a6f7-455c1cf74d44', substance_control_authority__harm_reduction_reading, coexists_with).
narrative_ontology:cs_axiom('85d70685-dfb7-41d9-a6f7-455c1cf74d44', foundational, criminalization_deters_third_party_harm).
narrative_ontology:cs_axiom_status(criminalization_deters_third_party_harm, holdable).
narrative_ontology:cs_axiom_grounding('85d70685-dfb7-41d9-a6f7-455c1cf74d44', criminalization_deters_third_party_harm, empirically_contingent).
narrative_ontology:cs_axiom('85d70685-dfb7-41d9-a6f7-455c1cf74d44', foundational, protective_duty_overrides_user_autonomy).
narrative_ontology:cs_axiom_status(protective_duty_overrides_user_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('85d70685-dfb7-41d9-a6f7-455c1cf74d44', protective_duty_overrides_user_autonomy, deontological).
narrative_ontology:cs_axiom('85d70685-dfb7-41d9-a6f7-455c1cf74d44', secondary, incarceration_primary_protective_mechanism).
narrative_ontology:cs_axiom_status(incarceration_primary_protective_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('85d70685-dfb7-41d9-a6f7-455c1cf74d44', incarceration_primary_protective_mechanism, instrumental).
narrative_ontology:cs_reference_frame('85d70685-dfb7-41d9-a6f7-455c1cf74d44', criminal_deterrence_protective_order).
narrative_ontology:cs_drift_state('85d70685-dfb7-41d9-a6f7-455c1cf74d44', contemporary, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('85d70685-dfb7-41d9-a6f7-455c1cf74d44', '').
narrative_ontology:cs_kernel_id(substance_control_authority__prohibition_reading, substance_control_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_authority__prohibition_reading, residents_of_disorder_affected_neighborhoods).
narrative_ontology:constraint_beneficiary(substance_control_authority__prohibition_reading, police_and_prosecutorial_agencies).
narrative_ontology:constraint_beneficiary(substance_control_authority__prohibition_reading, correctional_systems).
narrative_ontology:constraint_victim(substance_control_authority__prohibition_reading, drug_users).
narrative_ontology:constraint_victim(substance_control_authority__prohibition_reading, minority_communities_subject_to_disparate_enforcement).
narrative_ontology:constraint_victim(substance_control_authority__prohibition_reading, families_of_incarcerated_people).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(substance_control_authority__prohibition_reading, residents_of_disorder_affected_neighborhoods).
narrative_ontology:constraint_vindicates(substance_control_authority__prohibition_reading, police_power_third_party_protection_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacts and maintains the statutes criminalizing possession and use, sets scheduling and sentencing policy, and appropriates enforcement budgets. Justifies the criminal approach as the state's protective duty to third parties. Faces electoral cycles that reward visible order enforcement and punish perceived leniency, but retains full structural authority to amend or repeal the framework at any session.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, state_legislative_and_executive_branch, agenda_setter,
    institutional, generational, arbitrage, national).

% Make the arrests, bring the prosecutions, and administer the enforcement machinery day to day. Possession offenses supply a large share of caseload, overtime, and grant funding; asset forfeiture returns revenue directly to some agencies. Their staffing, promotion pathways, and budget baselines are built around drug enforcement volume, which makes unilateral retrenchment organizationally costly even where leadership favors it.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, police_and_prosecutorial_agencies, beneficiary,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(substance_control_authority__prohibition_reading, police_and_prosecutorial_agencies, agenda_setter).

% House the incarcerated population that drug offenses feed, directly and through probation and parole revocations. Facility staffing, service contracts, and construction budgets scale with that population. They do not set drug policy, but their capacity needs feed back into sentencing politics and their employment base depends on continued inflow.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, correctional_systems, beneficiary,
    institutional, biographical, constrained, national).

% Live with the disorder, property crime, and open market activity the arrangement claims to suppress, and many of them demand visible police response. They get quieter corners where enforcement concentrates. They also bear the over-policing side of the same intensity: stops of the non-involved, arrests of family members, and the household destabilization that follows a removed earner or caregiver.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, residents_of_disorder_affected_neighborhoods, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(substance_control_authority__prohibition_reading, residents_of_disorder_affected_neighborhoods, payer).

% Bear arrest, prosecution, incarceration, fines, and a lifelong criminal record for conduct the statute defines. Dependence limits their ability to simply stop, and the record forecloses employment, housing, and licensing long after any sentence ends. They hold no seat in the policy process that defines their conduct as criminal.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, drug_users, payer,
    powerless, biographical, trapped, national).

% Experience arrest and incarceration rates for drug offenses that run far above their share of measured use, because enforcement concentrates where markets are visible and patrol density is highest. The disparity accumulates across generations through records, removals, and family instability. Advocacy organizations exist and persist, but the enforcement pattern has proven durable against them.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, minority_communities_subject_to_disparate_enforcement, payer,
    moderate, biographical, constrained, regional).

% Lose income, caregiving, and housing stability when a member is incarcerated for a drug offense, and carry the burdens that follow the sentence home: probation conditions, visitation costs, supervision stigma, and debt from fines and fees. Their exposure is direct and they are not consulted in policy design.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, families_of_incarcerated_people, payer,
    powerless, biographical, trapped, local).

% Run syringe services, naloxone distribution, and treatment linkage — interventions that operate in the legal shadow of the same statutes that criminalize possession. Their sites and tools require exemptions and waivers, and their clients risk arrest at the point of service. They would argue the criminal framework suppresses the interventions with the strongest health evidence, but they hold no formal seat in scheduling or sentencing policy.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, harm_reduction_practitioners, excluded,
    moderate, biographical, constrained, national).

% Measure deterrence effects, disparity patterns, enforcement costs, and outcomes in jurisdictions that changed course. Their findings circulate into the policy debate but carry no vote; several reform efforts cite their work while the core statutes remain unchanged.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, policy_researchers, observer,
    moderate, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_authority__prohibition_reading, police_and_prosecutorial_agencies).
narrative_ontology:fixing_cost_class(substance_control_authority__prohibition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Addresses the public-order collective-action problem: drug markets and heavy use generate visible disorder, property crime, and violence that individual residents cannot suppress alone, and the criminal law supplies a state-run deterrence mechanism aimed at protecting third parties from those spillovers.
% TRANSFER_FUNCTION: Moves liberty, years of life (incarceration), income (fines, fees, asset forfeiture, and employment and housing foreclosure via criminal records), and civic standing from drug users and from the neighborhoods where enforcement concentrates, to the enforcement apparatus (budgets, staffing, forfeiture revenue, institutional scale) and to third parties as suppressed visible disorder.
% ABSENT_VOICES: Drug users themselves — the seat with the most direct knowledge of how the arrangement operates on its targets — are absent: partly disenfranchised through felony records, stigmatized, and only marginally organized. Harm reduction practitioners are legally constrained from full participation because their tools sit adjacent to the criminalized conduct. Both would object that the protective framing prices their liberty and health as acceptable inputs.
% DISAPPEARANCE_RATIONALE: Overnight disappearance would empty a large share of prison and jail beds, eliminate forfeiture revenue and a substantial share of arrest and prosecution caseload, force reorganization of policing priorities, and change the visible order of heavily policed neighborhoods; drug markets would restructure, and third-party exposure to disorder would shift rather than simply vanish. Enforcement employment, correctional populations, and market structure all rearrange simultaneously.
% FOUNDING_PROBLEM: Early-twentieth-century narcotics waves and, in the modern form, the 1970s-1980s crisis of visible open-air drug markets, property crime funding use, and violence around unregulated supply — a public-order breakdown residents demanded the state suppress and that existing civil and health institutions appeared unable to address.
% FOUNDING_PROBLEM_CORROBORATION: Public health agencies and academic criminology — outside the beneficiary set — corroborate that drug-related disorder and property crime are real and persistent, while the same sources document that enforcement-heavy application shows weak net deterrence and substantial collateral harm; residents of affected neighborhoods corroborate the disorder problem from direct experience. No source outside the benefiting parties attests that criminalization specifically (as opposed to some intervention) is required to address it.
narrative_ontology:disappearance_verdict(substance_control_authority__prohibition_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_authority__prohibition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_authority__prohibition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(substance_control_authority__prohibition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_authority__prohibition_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_authority__prohibition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_authority__prohibition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substance_control_authority__prohibition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72 at interval end) because the transfer is large and concentrated — users' liberty and lifetime records, communities' concentrated enforcement burden — against a protective benefit that is real but diffuse, uneven, and partly displaced to other areas. Suppression is higher still (0.85) because the arrangement's persistence rests on the criminal law itself: it is authored as a raw structural property of the arrangement (unscaled by power or scope — only extractiveness is scaled in the engine's computation), and incarceration is the primary mechanism by the arrangement's own design, with alternatives such as possession deprioritization or supervised services requiring legal exemption. Theater is moderate (0.45): order enforcement is real activity producing real if displaced reductions in visible disorder, but a growing share of operation — simple-possession arrests, disparity-producing patrol allocation — does not serve the protective justification it cites. Accessibility collapse is low-moderate (0.40) because alternatives remain live and demonstrated in other jurisdictions, though individually foreclosed for the trapped seats by records and dependence. Resistance is moderate-high (0.60): sustained reform movements, ballot initiatives, sentencing reform, and public-health advocacy, durable but not framework-breaking. The measurement series run on one shared eight-point grid (1971-2025) so every tracked metric is authored at every examined time point. The trajectory is an enforcement ratchet with reform-era give rather than a clean cycle: crisis politics (the crack-era panic, then the fentanyl-era panic) re-intensifies enforcement after each relaxation, and the re-intensification phases are when extraction ratchets up — intermittent crisis reinforcement, not noise. Coalition note: the powerless seats (users, families) hold latent coalition potential through reform movements and affected-family advocacy, which is the mechanism behind the 2010-2018 retrenchment visible in the series.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter and enforcement seats the arrangement is a functioning protective service they staff, fund, and watch work: arrests clear corners, constituents report relief, budgets sustain institutions. From the user and community seats the same operation is a machine that converts their conduct and neighborhoods into caseload, records, and removal. Residents of affected neighborhoods sit genuinely astride the gap — the same streets generate both their demand for order and their exposure to over-policing — which is why their seat is authored dual-positioned. The engine computes per-seat classifications from the structural data; the authored claim does not adjudicate the divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to real collectors: the enforcement apparatus (budgets, overtime, forfeiture — near the beneficiary end of d), correctional systems (capacity funding scales with inflow), and residents (genuine order receipt, pulled toward mid-range by the secondary payer position from over-policing). Victim declarations map to the targets: users sit near the full-target end (trapped exit — dependence plus record forecloses exit), minority communities carry high d from concentrated application, and families of incarcerated people carry high d from direct household burden. Spatial scope is national statute with intensely local application; verification of the protective claim is hardest at national scope, which amplifies effective extraction for the target seats. No directionality overrides are authored: the beneficiary/victim declarations plus exit options produce the correct d for every seat, and the one dual-positioned agent is captured by its secondary_role rather than by an override.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim guards against two mislabelings. Reading the arrangement as pure coordination would erase the users' and communities' burden and hand its defenders the protective story unexamined. Reading it as pure extraction would erase the genuine, independently corroborated public-order demand that gives the arrangement its constituency — a demand that would not vanish if the arrangement did, and that any replacement must address. Mandatrophy: the founding problem is contested-live; the arrangement persists while its fit to the problem is disputed. If the disorder problem were resolved, or shown to be substantially endogenous to illegality itself, the coordination half would atrophy and the structure would drift toward theatrical maintenance of the protective story — the theater_ratio series is the early-warning line for that drift, and the piton test (administrator could change it, but fixing costs more than it bears the administrator) is checked against the agenda-setter seat.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint instantiates the prohibition_reading of the substance_control_authority kernel; what would the sibling readings change structurally, and where exactly is the disagreement located?',
    'Not resolvable by data within this file — it is a framing commitment resolved at kernel level. A jurisdiction adopting harm_reduction_reading removes users from the victim set and dissolves the enforcement apparatus''s collection base; adoption of legalization_reading eliminates possession crimes entirely and converts the market to regulated commerce. The disagreement is located in (a) the normative status of user conduct and (b) the causal premise that criminalization protects third parties.',
    'If the kernel were re-read under either sibling, this constraint''s beneficiary/victim structure inverts or dissolves: the enforcement apparatus loses its collection base and users exit the victim set entirely, recomputing every seat''s classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: this file is one reading of the substance_control_authority kernel; the sibling readings would restructure the victim and beneficiary sets.').

omega_variable(
    net_deterrence_efficacy,
    'Does criminalizing use and possession reduce third parties'' net exposure to drug-related crime and disorder, once black-market violence, displacement of markets to other areas, and theft to pay prohibition-inflated prices are counted?',
    'Natural experiments: jurisdictions adopting the sibling arrangements (decriminalization, regulated markets) with third-party-harm endpoints measured before and after, difference-in-differences against matched prohibition jurisdictions.',
    'If net deterrence fails, the coordination function is cover and the arrangement recomputes toward pure extraction with a protective story; if it holds, the tangled_rope structure stands and part of the measured burden is the price of a working protective service.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(net_deterrence_efficacy, empirical, 'Whether the protective coordination claim survives counting the arrangement''s own crime externalities.').

omega_variable(
    disparate_application_source,
    'Is the racial disparity in enforcement application driven by market visibility and patrol allocation (structure-neutral mechanisms) or by enforcement bias (structure-producing mechanisms)?',
    'Arrest-to-usage-ratio comparisons across substances and jurisdictions, audit and sentinel-event studies, patrol-allocation records.',
    'If bias-driven, the arrangement''s costs concentrate on a protected-class population, its legitimacy erodes faster, the enforcement seats'' classification hardens toward extraction, and reform pressure shifts from marginal to structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disparate_application_source, empirical, 'Source of the documented disparity in who bears the arrangement''s costs.').

omega_variable(
    protective_punitive_separability,
    'Is third-party protection separable from possession criminalization — can order maintenance (addressing markets, violence, and visible disorder) be delivered while possession itself is deprioritized or decriminalized?',
    'Jurisdictions that deprioritized possession enforcement while maintaining order policing, compared against matched jurisdictions on disorder and third-party-harm trajectories.',
    'If separable, the possession-criminalization component is a rider on a genuine order-maintenance function and the extractive share of the arrangement is larger than the protective share; if inseparable, criminalization is load-bearing for the protective function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(protective_punitive_separability, conceptual, 'Whether the protective function requires criminalizing the user''s conduct specifically.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_authority__prohibition_reading, 1971, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t1971, substance_control_authority__prohibition_reading, theater_ratio, 1971, 0.3).
narrative_ontology:measurement_basis(subs_tr_t1971, observed).
narrative_ontology:measurement(subs_tr_t1980, substance_control_authority__prohibition_reading, theater_ratio, 1980, 0.33).
narrative_ontology:measurement_basis(subs_tr_t1980, observed).
narrative_ontology:measurement(subs_tr_t1986, substance_control_authority__prohibition_reading, theater_ratio, 1986, 0.38).
narrative_ontology:measurement_basis(subs_tr_t1986, observed).
narrative_ontology:measurement(subs_tr_t1994, substance_control_authority__prohibition_reading, theater_ratio, 1994, 0.44).
narrative_ontology:measurement_basis(subs_tr_t1994, observed).
narrative_ontology:measurement(subs_tr_t2001, substance_control_authority__prohibition_reading, theater_ratio, 2001, 0.47).
narrative_ontology:measurement_basis(subs_tr_t2001, observed).
narrative_ontology:measurement(subs_tr_t2010, substance_control_authority__prohibition_reading, theater_ratio, 2010, 0.46).
narrative_ontology:measurement_basis(subs_tr_t2010, observed).
narrative_ontology:measurement(subs_tr_t2018, substance_control_authority__prohibition_reading, theater_ratio, 2018, 0.43).
narrative_ontology:measurement_basis(subs_tr_t2018, observed).
narrative_ontology:measurement(subs_tr_t2025, substance_control_authority__prohibition_reading, theater_ratio, 2025, 0.45).
narrative_ontology:measurement_basis(subs_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(subs_be_t1971, substance_control_authority__prohibition_reading, base_extractiveness, 1971, 0.52).
narrative_ontology:measurement_basis(subs_be_t1971, observed).
narrative_ontology:measurement(subs_be_t1980, substance_control_authority__prohibition_reading, base_extractiveness, 1980, 0.58).
narrative_ontology:measurement_basis(subs_be_t1980, observed).
narrative_ontology:measurement(subs_be_t1986, substance_control_authority__prohibition_reading, base_extractiveness, 1986, 0.67).
narrative_ontology:measurement_basis(subs_be_t1986, observed).
narrative_ontology:measurement(subs_be_t1994, substance_control_authority__prohibition_reading, base_extractiveness, 1994, 0.76).
narrative_ontology:measurement_basis(subs_be_t1994, observed).
narrative_ontology:measurement(subs_be_t2001, substance_control_authority__prohibition_reading, base_extractiveness, 2001, 0.79).
narrative_ontology:measurement_basis(subs_be_t2001, observed).
narrative_ontology:measurement(subs_be_t2010, substance_control_authority__prohibition_reading, base_extractiveness, 2010, 0.74).
narrative_ontology:measurement_basis(subs_be_t2010, observed).
narrative_ontology:measurement(subs_be_t2018, substance_control_authority__prohibition_reading, base_extractiveness, 2018, 0.7).
narrative_ontology:measurement_basis(subs_be_t2018, observed).
narrative_ontology:measurement(subs_be_t2025, substance_control_authority__prohibition_reading, base_extractiveness, 2025, 0.72).
narrative_ontology:measurement_basis(subs_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t1971, substance_control_authority__prohibition_reading, suppression_requirement, 1971, 0.55).
narrative_ontology:measurement_basis(subs_su_t1971, observed).
narrative_ontology:measurement(subs_su_t1980, substance_control_authority__prohibition_reading, suppression_requirement, 1980, 0.62).
narrative_ontology:measurement_basis(subs_su_t1980, observed).
narrative_ontology:measurement(subs_su_t1986, substance_control_authority__prohibition_reading, suppression_requirement, 1986, 0.74).
narrative_ontology:measurement_basis(subs_su_t1986, observed).
narrative_ontology:measurement(subs_su_t1994, substance_control_authority__prohibition_reading, suppression_requirement, 1994, 0.85).
narrative_ontology:measurement_basis(subs_su_t1994, observed).
narrative_ontology:measurement(subs_su_t2001, substance_control_authority__prohibition_reading, suppression_requirement, 2001, 0.87).
narrative_ontology:measurement_basis(subs_su_t2001, observed).
narrative_ontology:measurement(subs_su_t2010, substance_control_authority__prohibition_reading, suppression_requirement, 2010, 0.82).
narrative_ontology:measurement_basis(subs_su_t2010, observed).
narrative_ontology:measurement(subs_su_t2018, substance_control_authority__prohibition_reading, suppression_requirement, 2018, 0.78).
narrative_ontology:measurement_basis(subs_su_t2018, observed).
narrative_ontology:measurement(subs_su_t2025, substance_control_authority__prohibition_reading, suppression_requirement, 2025, 0.85).
narrative_ontology:measurement_basis(subs_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_authority__prohibition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(substance_control_authority__prohibition_reading, substance_control_authority__harm_reduction_reading).
narrative_ontology:affects_constraint(substance_control_authority__prohibition_reading, substance_control_authority__legalization_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'drug policy' covers one kernel — state authority over psychoactive substance use — instantiated by three structurally distinct constraints: this prohibition reading (criminalize use and possession; users in the victim set; the enforcement apparatus as collector), the harm_reduction_reading (accept use, intervene on harms; users exit the victim set), and the legalization_reading (regulate as commerce; possession crimes eliminated). Each file authors its own epsilon, beneficiaries, and victims over the same kernel, per the epsilon-invariance principle; they are linked here as a constraint family. This reading is upstream of the siblings in enforcement resources: the apparatus built under prohibition is what the sibling readings would inherit or dismantle if adopted.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
